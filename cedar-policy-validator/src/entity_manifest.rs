//! Entity Manifest definition and static analysis.

/*
 * Copyright Cedar Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

use std::collections::HashMap;
use std::fmt::{Display, Formatter};

use cedar_policy_core::ast::{
    BinaryOp, EntityUID, Expr, ExprKind, Literal, PolicyID, PolicySet, RequestType, UnaryOp, Var,
};
use cedar_policy_core::entities::err::EntitiesError;
use cedar_policy_core::impl_diagnostic_from_source_loc_field;
use cedar_policy_core::parser::Loc;
use miette::Diagnostic;
use serde::{Deserialize, Serialize};
use serde_with::serde_as;
use smol_str::SmolStr;
use thiserror::Error;

use crate::entity_manifest_analysis::{EntityManifestAnalysisResult, WrappedAccessPaths};
use crate::ValidationError;
use crate::{
    typecheck::{PolicyCheck, Typechecker},
    types::Type,
    ValidationMode, ValidatorSchema,
};

/// Data structure storing what data is needed
/// based on the the [`RequestType`].
/// For each request type, the [`EntityManifest`] stores
/// a [`RootAccessTrie`] of data to retrieve.
///
/// `T` represents an optional type annotation on each
/// node in the [`AccessTrie`].
#[doc = include_str!("../../cedar-policy/experimental_warning.md")]
#[serde_as]
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct EntityManifest<T = ()>
where
    T: Clone,
{
    /// A map from request types to [`RootAccessTrie`]s.
    #[serde_as(as = "Vec<(_, _)>")]
    #[serde(bound(deserialize = "T: Default"))]
    pub per_action: HashMap<RequestType, RootAccessTrie<T>>,
}

/// A map of data fields to [`AccessTrie`]s.
/// The keys to this map form the edges in the access trie,
/// pointing to sub-tries.
#[doc = include_str!("../../cedar-policy/experimental_warning.md")]
pub type Fields<T> = HashMap<SmolStr, Box<AccessTrie<T>>>;

/// The root of a data path or [`RootAccessTrie`].
#[doc = include_str!("../../cedar-policy/experimental_warning.md")]
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum EntityRoot {
    /// Literal entity ids
    Literal(EntityUID),
    /// A Cedar variable
    Var(Var),
}

impl Display for EntityRoot {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            EntityRoot::Literal(l) => write!(f, "{l}"),
            EntityRoot::Var(v) => write!(f, "{v}"),
        }
    }
}

/// A [`RootAccessTrie`] is a trie describing a set of
/// data paths to retrieve. Each edge in the trie
/// is either a record or entity dereference.
///
/// If an entity or record field does not exist in the backing store,
/// it is safe to stop loading data at that point.
///
/// `T` represents an optional type annotation on each
/// node in the [`AccessTrie`].
#[doc = include_str!("../../cedar-policy/experimental_warning.md")]
#[serde_as]
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RootAccessTrie<T = ()>
where
    T: Clone,
{
    /// The data that needs to be loaded, organized by root.
    #[serde_as(as = "Vec<(_, _)>")]
    #[serde(bound(deserialize = "T: Default"))]
    pub trie: HashMap<EntityRoot, AccessTrie<T>>,
}

/// A Trie representing a set of data paths to load,
/// starting implicitly from a Cedar value.
///
/// `T` represents an optional type annotation on each
/// node in the [`AccessTrie`].
#[doc = include_str!("../../cedar-policy/experimental_warning.md")]
#[serde_as]
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct AccessTrie<T = ()> {
    /// Child data of this entity slice.
    /// The keys are edges in the trie pointing to sub-trie values.
    #[serde_as(as = "Vec<(_, _)>")]
    pub children: Fields<T>,
    /// For entity types, this boolean may be `true`
    /// to signal that all the ancestors in the entity hierarchy
    /// are required (transitively).
    pub ancestors_required: bool,
    /// Optional data annotation, usually used for type information.
    #[serde(skip_serializing, skip_deserializing)]
    #[serde(bound(deserialize = "T: Default"))]
    pub data: T,
}

/// A data path that may end with requesting the parents of
/// an entity.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct AccessPath {
    /// The root variable that begins the data path
    pub root: EntityRoot,
    /// The path of fields of entities or structs
    pub path: Vec<SmolStr>,
    /// Request all the parents in the entity hierarchy of this entity.
    pub ancestors_required: bool,
}

/// Entity manifest computation does not handle the full
/// cedar language. In particular, the policies must follow the
/// following grammar:
/// ```text
/// <expr> = <datapath-expr>
///          <datapath-expr> in <expr>
///          <expr> + <expr>
///          if <expr> { <expr> } { <expr> }
///          ... all other cedar operators not mentioned by datapath-expr

/// <datapath-expr> = <datapath-expr>.<field>
///                   <datapath-expr> has <field>
///                   <variable>
///                   <entity literal>
/// ```
/// The `get_expr_path` function handles `datapath-expr` expressions.
/// This error message tells the user not to use certain operators
/// before accessing record or entity attributes, breaking this grammar.
#[derive(Debug, Clone, Error, Hash, Eq, PartialEq)]
#[error("For policy `{policy_id}`, failed to analyze expression while computing entity manifest.`")]
pub struct FailedAnalysisError {
    /// Source location
    pub source_loc: Option<Loc>,
    /// Policy ID where the error occurred
    pub policy_id: PolicyID,
    /// The kind of the expression that was unexpected
    pub expr_kind: ExprKind<Option<Type>>,
}

impl Diagnostic for FailedAnalysisError {
    impl_diagnostic_from_source_loc_field!();

    fn help<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        Some(Box::new(format!(
            "failed to compute entity manifest: {} operators are not allowed before accessing record or entity attributes",
            self.expr_kind.operator_description()
        )))
    }
}

/// Error when expressions are partial during entity
/// manifest computation
#[derive(Debug, Clone, Error, Hash, Eq, PartialEq)]
#[error(
    "Entity manifest computation requires fully concrete policies. Got a policy with an unknown expression."
)]
pub struct PartialExpressionError {}

impl Diagnostic for PartialExpressionError {}

/// Error when the request is partial during entity
/// manifest computation
#[derive(Debug, Clone, Error, Hash, Eq, PartialEq)]
#[error("Entity manifest computation and entity slicing require a fully concrete request. Got a partial request.")]
pub struct PartialRequestError {}
impl Diagnostic for PartialRequestError {}

/// An error generated by entity manifest computation.
/// See [`FailedAnalysisError`] for details on the fragment
/// of Cedar handled by entity slicing.
#[derive(Debug, Error, Diagnostic)]
#[non_exhaustive]
pub enum EntityManifestError {
    /// A validation error was encountered
    #[error(transparent)]
    #[diagnostic(transparent)]
    ValidationError(#[from] ValidationError),
    /// A entities error was encountered
    #[error(transparent)]
    #[diagnostic(transparent)]
    EntitiesError(#[from] EntitiesError),

    /// The request was partial
    #[error(transparent)]
    #[diagnostic(transparent)]
    PartialRequestError(#[from] PartialRequestError),
    /// A policy was partial
    #[error(transparent)]
    #[diagnostic(transparent)]
    PartialExpressionError(#[from] PartialExpressionError),

    /// A policy was not analyzable because it used unsupported operators
    /// before a [`ExprKind::GetAttr`]
    /// See [`FailedAnalysisError`] for more details.
    #[error(transparent)]
    #[diagnostic(transparent)]
    FailedAnalysis(#[from] FailedAnalysisError),
}

/// Union two tries by combining the fields.
fn union_fields<T: Clone>(first: &Fields<T>, second: &Fields<T>) -> Fields<T> {
    let mut res = first.clone();
    for (key, value) in second {
        res.entry(key.clone())
            .and_modify(|existing| *existing = Box::new((*existing).union(value)))
            .or_insert(value.clone());
    }
    res
}

impl AccessPath {
    /// Convert a [`AccessPath`] into corresponding [`RootAccessTrie`].
    pub fn to_root_access_trie(&self) -> RootAccessTrie {
        self.to_root_access_trie_with_leaf(AccessTrie::default())
    }

    /// Convert an [`AccessPath`] to a [`RootAccessTrie`], and also
    /// add a full trie as the leaf at the end.
    pub(crate) fn to_root_access_trie_with_leaf(&self, leaf_trie: AccessTrie) -> RootAccessTrie {
        let mut current = leaf_trie;
        // reverse the path, visiting the last access first
        for (index, field) in self.path.iter().rev().enumerate() {
            let mut fields = HashMap::new();
            fields.insert(field.clone(), Box::new(current));

            // the first time we build an access trie is the leaf
            // of the path, so set the `ancestors_required` flag
            let ancestors_required = if index == 0 {
                self.ancestors_required
            } else {
                false
            };

            current = AccessTrie {
                ancestors_required,
                children: fields,
                data: (),
            };
        }

        let mut primary_map = HashMap::new();

        // special case: if the path is empty and ancestors not required,
        // no need to insert anything
        if current != AccessTrie::new() {
            primary_map.insert(self.root.clone(), current);
        }
        RootAccessTrie { trie: primary_map }
    }
}

impl RootAccessTrie {
    /// Create an empty [`RootAccessTrie`] that requests nothing.
    pub fn new() -> Self {
        Self {
            trie: Default::default(),
        }
    }
}

impl<T: Clone> RootAccessTrie<T> {
    /// Union two [`RootAccessTrie`]s together.
    /// The new trie requests the data from both of the original.
    pub fn union(&self, other: &Self) -> Self {
        let mut res = self.clone();
        for (key, value) in &other.trie {
            res.trie
                .entry(key.clone())
                .and_modify(|existing| *existing = (*existing).union(value))
                .or_insert(value.clone());
        }
        res
    }
}

impl Default for RootAccessTrie {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Clone> AccessTrie<T> {
    /// Union two [`AccessTrie`]s together.
    /// The new trie requests the data from both of the original.
    pub fn union(&self, other: &Self) -> Self {
        Self {
            children: union_fields(&self.children, &other.children),
            ancestors_required: self.ancestors_required || other.ancestors_required,
            data: self.data.clone(),
        }
    }
}

impl AccessTrie {
    /// A new trie that requests no data.
    pub fn new() -> Self {
        Self {
            children: Default::default(),
            ancestors_required: false,
            data: (),
        }
    }
}

impl Default for AccessTrie {
    fn default() -> Self {
        Self::new()
    }
}

/// Computes an [`EntityManifest`] from the schema and policies.
/// The policies must validate against the schema in strict mode,
/// otherwise an error is returned.
pub fn compute_entity_manifest(
    schema: &ValidatorSchema,
    policies: &PolicySet,
) -> Result<EntityManifest, EntityManifestError> {
    let mut manifest: HashMap<RequestType, RootAccessTrie> = HashMap::new();

    // now, for each policy we add the data it requires to the manifest
    for policy in policies.policies() {
        // typecheck the policy and get all the request environments
        let typechecker = Typechecker::new(schema, ValidationMode::Strict, policy.id().clone());
        let request_envs = typechecker.typecheck_by_request_env(policy.template());
        for (request_env, policy_check) in request_envs {
            let new_primary_slice = match policy_check {
                PolicyCheck::Success(typechecked_expr) => {
                    // compute the trie from the typechecked expr
                    // using static analysis
                    entity_manifest_from_expr(&typechecked_expr, policy.id())
                        .map(|val| val.global_trie)
                }
                PolicyCheck::Irrelevant(_) => {
                    // this policy is irrelevant, so we need no data
                    Ok(RootAccessTrie::new())
                }

                // TODO is returning the first error correct?
                // Also, should we run full validation instead of just
                // typechecking? Validation does a little more right?
                PolicyCheck::Fail(errors) => {
                    // PANIC SAFETY: policy check fail should be a non-empty vector.
                    #[allow(clippy::expect_used)]
                    Err(errors
                        .first()
                        .expect("Policy check failed without an error")
                        .clone()
                        .into())
                }
            }?;

            let request_type = request_env
                .to_request_type()
                .ok_or(PartialRequestError {})?;
            // Add to the manifest based on the request type.
            manifest
                .entry(request_type)
                .and_modify(|existing| {
                    *existing = existing.union(&new_primary_slice);
                })
                .or_insert(new_primary_slice);
        }
    }

    Ok(EntityManifest {
        per_action: manifest,
    })
}

/// A static analysis on type-annotated cedar expressions.
/// Computes the [`RootAccessTrie`] representing all the data required
/// to evaluate the expression.
fn entity_manifest_from_expr(
    expr: &Expr<Option<Type>>,
    policy_id: &PolicyID,
) -> Result<EntityManifestAnalysisResult, EntityManifestError> {
    match expr.expr_kind() {
        ExprKind::Slot(slot_id) => {
            if slot_id.is_principal() {
                Ok(EntityManifestAnalysisResult::from_root(EntityRoot::Var(
                    Var::Principal,
                )))
            } else {
                assert!(slot_id.is_resource());
                Ok(EntityManifestAnalysisResult::from_root(EntityRoot::Var(
                    Var::Resource,
                )))
            }
        }
        ExprKind::Var(var) => Ok(EntityManifestAnalysisResult::from_root(EntityRoot::Var(
            *var,
        ))),
        ExprKind::GetAttr { expr, attr } => {
            Ok(entity_manifest_from_expr(expr, policy_id)?.get_attr(attr))
        }
        ExprKind::Lit(Literal::EntityUID(literal)) => Ok(EntityManifestAnalysisResult::from_root(
            EntityRoot::Literal((**literal).clone()),
        )),
        ExprKind::Unknown(_) => Err(PartialExpressionError {})?,

        // Non-entity literals need no fields to be loaded.
        ExprKind::Lit(_) => Ok(EntityManifestAnalysisResult::default()),
        ExprKind::Unknown(_) => Err(PartialExpressionError {})?,
        ExprKind::If {
            test_expr,
            then_expr,
            else_expr,
        } => Ok(entity_manifest_from_expr(test_expr, policy_id)?
            .empty_paths()
            .union(&entity_manifest_from_expr(then_expr, policy_id)?)
            .union(&entity_manifest_from_expr(else_expr, policy_id)?)),
        ExprKind::And { left, right }
        | ExprKind::Or { left, right }
        | ExprKind::BinaryApp {
            op: BinaryOp::Less | BinaryOp::LessEq | BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul,
            arg1: left,
            arg2: right,
        } => Ok(entity_manifest_from_expr(left, policy_id)?
            .empty_paths()
            .union(&entity_manifest_from_expr(right, policy_id)?)),
        ExprKind::UnaryApp { op, arg } => {
            match op {
                // both unary ops are on booleans, so they are simple
                UnaryOp::Not | UnaryOp::Neg => {
                    Ok(entity_manifest_from_expr(arg, policy_id)?.empty_paths())
                }
            }
        }
        ExprKind::BinaryApp {
            op:
                BinaryOp::Eq
                | BinaryOp::In
                | BinaryOp::Contains
                | BinaryOp::ContainsAll
                | BinaryOp::ContainsAny,
            arg1,
            arg2,
        } => {
            // TODO more elegant way to bind op?
            let ExprKind::BinaryApp { op, .. } = expr.expr_kind() else {
                panic!("Matched above");
            };

            // First, find the data paths for each argument
            let mut arg1_res = entity_manifest_from_expr(arg1, policy_id)?;
            let mut arg2_res = entity_manifest_from_expr(arg2, policy_id)?;

            let ty1 = arg1
                .data()
                .as_ref()
                .expect("Expected annotated types after typechecking");
            let ty2 = arg2
                .data()
                .as_ref()
                .expect("Expected annotated types after typechecking");

            // For containment operations, we need the ancestors
            // of entities, since the semantics check them.
            match op {
                BinaryOp::ContainsAll | BinaryOp::ContainsAny | BinaryOp::Contains => {
                    arg2_res = arg2_res.ancestors_required(ty2);
                }
                BinaryOp::In => {
                    arg1_res = arg1_res.ancestors_required(ty1);
                }
                _ => (),
            }

            // Load all fields using `full_type_required`, since
            // these operations do equality checks.
            Ok(arg1_res
                .full_type_required(ty1)
                .union(&arg2_res.full_type_required(ty2)))
        }
        ExprKind::ExtensionFunctionApp { fn_name: _, args } => {
            // WARNING: this code assumes that extension functions
            // don't take full structs as inputs.
            // If they did, we would need to use logic similar to the Eq binary operator.

            let mut res = EntityManifestAnalysisResult::default();

            for arg in args.iter() {
                res = res.union(&entity_manifest_from_expr(arg, policy_id)?);
            }
            Ok(res)
        }
        ExprKind::Like { expr, pattern: _ }
        | ExprKind::Is {
            expr,
            entity_type: _,
        } => {
            // drop paths since boolean returned
            Ok(entity_manifest_from_expr(expr, policy_id)?.empty_paths())
        }
        ExprKind::Set(contents) => {
            let mut res = EntityManifestAnalysisResult::default();

            // take union of all of the contents
            for expr in &**contents {
                let mut content = entity_manifest_from_expr(expr, policy_id)?;

                res = res.union(&content);
            }

            // now, wrap result in a set
            res.resulting_paths = WrappedAccessPaths::SetLiteral(Box::new(res.resulting_paths));

            Ok(res)
        }
        ExprKind::Record(content) => {
            let mut record_contents = HashMap::new();
            let mut global_trie = RootAccessTrie::default();

            for (key, child_expr) in content.iter() {
                let res = entity_manifest_from_expr(child_expr, policy_id)?;
                record_contents.insert(key.clone(), Box::new(res.resulting_paths));

                global_trie = global_trie.union(&res.global_trie);
            }

            Ok(EntityManifestAnalysisResult {
                resulting_paths: WrappedAccessPaths::RecordLiteral(record_contents),
                global_trie,
            })
        }
        ExprKind::GetAttr { expr, attr } | ExprKind::HasAttr { expr, attr } => {
            Ok(entity_manifest_from_expr(expr, policy_id)?.get_attr(attr))
        }
    }
}

/// Given an expression, get the corresponding data path
/// starting with a variable.
/// If the expression is not a `<datapath-expr>`, return an error.
/// See [`FailedAnalysisError`] for more information.
fn get_expr_path(
    expr: &Expr<Option<Type>>,
    policy_id: &PolicyID,
) -> Result<AccessPath, EntityManifestError> {
    Ok(match expr.expr_kind() {
        ExprKind::Slot(slot_id) => {
            if slot_id.is_principal() {
                AccessPath {
                    root: EntityRoot::Var(Var::Principal),
                    path: vec![],
                    ancestors_required: false,
                }
            } else {
                assert!(slot_id.is_resource());
                AccessPath {
                    root: EntityRoot::Var(Var::Resource),
                    path: vec![],
                    ancestors_required: false,
                }
            }
        }
        ExprKind::Var(var) => AccessPath {
            root: EntityRoot::Var(*var),
            path: vec![],
            ancestors_required: false,
        },
        ExprKind::GetAttr { expr, attr } => {
            let mut slice = get_expr_path(expr, policy_id)?;
            slice.path.push(attr.clone());
            slice
        }
        ExprKind::Lit(Literal::EntityUID(literal)) => AccessPath {
            root: EntityRoot::Literal((**literal).clone()),
            path: vec![],
            ancestors_required: false,
        },
        ExprKind::Unknown(_) => Err(PartialExpressionError {})?,
        // all other variants of expressions result in failure to analyze.
        _ => Err(EntityManifestError::FailedAnalysis(FailedAnalysisError {
            source_loc: expr.source_loc().cloned(),
            policy_id: policy_id.clone(),
            expr_kind: expr.expr_kind().clone(),
        }))?,
    })
}

#[cfg(test)]
mod entity_slice_tests {
    use cedar_policy_core::{
        ast::{Context, PolicyID, Request},
        entities::{EntityJsonParser, TCComputation},
        extensions::Extensions,
        parser::parse_policy,
    };

    use crate::CoreSchema;

    use super::*;

    // Schema for testing in this module
    fn schema() -> ValidatorSchema {
        ValidatorSchema::from_str_natural(
            "
entity User = {
  name: String,
};

entity Document;

action Read appliesTo {
  principal: [User],
  resource: [Document]
};
    ",
            Extensions::all_available(),
        )
        .unwrap()
        .0
    }

    fn expect_entity_slice_to(
        original: serde_json::Value,
        expected: serde_json::Value,
        schema: &ValidatorSchema,
        manifest: &EntityManifest,
    ) {
        let request = Request::new(
            (
                EntityUID::with_eid_and_type("User", "oliver").unwrap(),
                None,
            ),
            (
                EntityUID::with_eid_and_type("Action", "Read").unwrap(),
                None,
            ),
            (
                EntityUID::with_eid_and_type("Document", "dummy").unwrap(),
                None,
            ),
            Context::empty(),
            Some(schema),
            Extensions::all_available(),
        )
        .unwrap();

        let schema = CoreSchema::new(schema);
        let parser: EntityJsonParser<'_, '_, CoreSchema<'_>> = EntityJsonParser::new(
            Some(&schema),
            Extensions::all_available(),
            TCComputation::AssumeAlreadyComputed,
        );
        let original_entities = parser.from_json_value(original).unwrap();

        // Entity slicing results in invalid entity stores
        // since attributes may be missing.
        let parser_without_validation: EntityJsonParser<'_, '_> = EntityJsonParser::new(
            None,
            Extensions::all_available(),
            TCComputation::AssumeAlreadyComputed,
        );
        let expected_entities = parser_without_validation.from_json_value(expected).unwrap();

        let sliced_entities = manifest.slice(&original_entities, &request).unwrap();

        // PANIC SAFETY: Test panics when result is incorrect.
        #[allow(clippy::panic)]
        if !sliced_entities.deep_eq(&expected_entities) {
            panic!(
                "Sliced entities differed from expected. Expected:\n{}\nGot:\n{}",
                expected_entities.to_json_value().unwrap(),
                sliced_entities.to_json_value().unwrap()
            );
        }
    }

    #[test]
    fn test_simple_entity_manifest() {
        let mut pset = PolicySet::new();
        let policy = parse_policy(
            None,
            "permit(principal, action, resource)
when {
    principal.name == \"John\"
};",
        )
        .expect("should succeed");
        pset.add(policy.into()).expect("should succeed");

        let schema = schema();

        let entity_manifest = compute_entity_manifest(&schema, &pset).expect("Should succeed");
        let expected = r#"
{
  "per_action": [
    [
      {
        "principal": "User",
        "action": {
          "ty": "Action",
          "eid": "Read"
        },
        "resource": "Document"
      },
      {
        "trie": [
          [
            {
              "Var": "principal"
            },
            {
              "children": [
                [
                  "name",
                  {
                    "children": [],
                    "ancestors_required": false
                  }
                ]
              ],
              "ancestors_required": false
            }
          ]
        ]
      }
    ]
  ]
}"#;
        let expected_manifest = serde_json::from_str(expected).unwrap();
        assert_eq!(entity_manifest, expected_manifest);

        let entities_json = serde_json::json!(
            [
                {
                    "uid" : { "type" : "User", "id" : "oliver"},
                    "attrs" : {
                        "name" : "Oliver"
                    },
                    "parents" : []
                },
                {
                    "uid" : { "type" : "User", "id" : "oliver2"},
                    "attrs" : {
                        "name" : "Oliver2"
                    },
                    "parents" : []
                },
            ]
        );

        let expected_entities_json = serde_json::json!(
            [
                {
                    "uid" : { "type" : "User", "id" : "oliver"},
                    "attrs" : {
                        "name" : "Oliver"
                    },
                    "parents" : []
                },
            ]
        );

        expect_entity_slice_to(
            entities_json,
            expected_entities_json,
            &schema,
            &expected_manifest,
        );
    }

    #[test]
    #[should_panic(expected = "Sliced entities differed")]
    fn sanity_test_empty_entity_manifest() {
        let mut pset = PolicySet::new();
        let policy =
            parse_policy(None, "permit(principal, action, resource);").expect("should succeed");
        pset.add(policy.into()).expect("should succeed");

        let schema = ValidatorSchema::from_str_natural(
            "
entity User = {
  name: String,
};

entity Document;

action Read appliesTo {
  principal: [User],
  resource: [Document]
};
        ",
            Extensions::all_available(),
        )
        .unwrap()
        .0;

        let entity_manifest = compute_entity_manifest(&schema, &pset).expect("Should succeed");
        let expected = r#"
{
  "per_action": [
    [
      {
        "principal": "User",
        "action": {
          "ty": "Action",
          "eid": "Read"
        },
        "resource": "Document"
      },
      {
        "trie": [
        ]
      }
    ]
  ]
}"#;
        let expected_manifest = serde_json::from_str(expected).unwrap();
        assert_eq!(entity_manifest, expected_manifest);

        let entities_json = serde_json::json!(
            [
                {
                    "uid" : { "type" : "User", "id" : "oliver"},
                    "attrs" : {
                        "name" : "Oliver"
                    },
                    "parents" : []
                },
                {
                    "uid" : { "type" : "User", "id" : "oliver2"},
                    "attrs" : {
                        "name" : "Oliver2"
                    },
                    "parents" : []
                },
            ]
        );

        let expected_entities_json = serde_json::json!([
            {
                "uid" : { "type" : "User", "id" : "oliver"},
                "attrs" : {
                    "name" : "Oliver"
                },
                "parents" : []
            },
            {
                "uid" : { "type" : "User", "id" : "oliver2"},
                "attrs" : {
                    "name" : "Oliver2"
                },
                "parents" : []
            },
        ]);

        expect_entity_slice_to(
            entities_json,
            expected_entities_json,
            &schema,
            &expected_manifest,
        );
    }

    #[test]
    fn test_empty_entity_manifest() {
        let mut pset = PolicySet::new();
        let policy =
            parse_policy(None, "permit(principal, action, resource);").expect("should succeed");
        pset.add(policy.into()).expect("should succeed");

        let schema = schema();

        let entity_manifest = compute_entity_manifest(&schema, &pset).expect("Should succeed");
        let expected = r#"
{
  "per_action": [
    [
      {
        "principal": "User",
        "action": {
          "ty": "Action",
          "eid": "Read"
        },
        "resource": "Document"
      },
      {
        "trie": [
        ]
      }
    ]
  ]
}"#;
        let expected_manifest = serde_json::from_str(expected).unwrap();
        assert_eq!(entity_manifest, expected_manifest);

        let entities_json = serde_json::json!(
            [
                {
                    "uid" : { "type" : "User", "id" : "oliver"},
                    "attrs" : {
                        "name" : "Oliver"
                    },
                    "parents" : []
                },
                {
                    "uid" : { "type" : "User", "id" : "oliver2"},
                    "attrs" : {
                        "name" : "Oliver2"
                    },
                    "parents" : []
                },
            ]
        );

        let expected_entities_json = serde_json::json!([]);

        expect_entity_slice_to(
            entities_json,
            expected_entities_json,
            &schema,
            &expected_manifest,
        );
    }

    #[test]
    fn test_entity_manifest_ancestors_required() {
        let mut pset = PolicySet::new();
        let policy = parse_policy(
            None,
            "permit(principal, action, resource)
when {
    principal in resource || principal.manager in resource
};",
        )
        .expect("should succeed");
        pset.add(policy.into()).expect("should succeed");

        let schema = ValidatorSchema::from_str_natural(
            "
entity User in [Document] = {
  name: String,
  manager: User
};
entity Document;
action Read appliesTo {
  principal: [User],
  resource: [Document]
};
  ",
            Extensions::all_available(),
        )
        .unwrap()
        .0;

        let entity_manifest = compute_entity_manifest(&schema, &pset).expect("Should succeed");
        let expected = r#"
{
  "per_action": [
    [
      {
        "principal": "User",
        "action": {
          "ty": "Action",
          "eid": "Read"
        },
        "resource": "Document"
      },
      {
        "trie": [
          [
            {
              "Var": "principal"
            },
            {
              "children": [
                [
                  "manager",
                  {
                    "children": [],
                    "ancestors_required": true
                  }
                ]
              ],
              "ancestors_required": true
            }
          ]
        ]
      }
    ]
  ]
}"#;
        let expected_manifest = serde_json::from_str(expected).unwrap();
        eprintln!("Got: {}", serde_json::to_string(&entity_manifest).unwrap());
        assert_eq!(entity_manifest, expected_manifest);

        let entities_json = serde_json::json!(
            [
                {
                    "uid" : { "type" : "User", "id" : "oliver"},
                    "attrs" : {
                        "name" : "Oliver",
                        "manager": { "type" : "User", "id" : "george"}
                    },
                    "parents" : [
                        { "type" : "Document", "id" : "oliverdocument"}
                    ]
                },
                {
                    "uid" : { "type" : "User", "id" : "george"},
                    "attrs" : {
                        "name" : "George",
                        "manager": { "type" : "User", "id" : "george"}
                    },
                    "parents" : [
                        { "type" : "Document", "id" : "georgedocument"}
                    ]
                },
            ]
        );

        let expected_entities_json = serde_json::json!(
            [
                {
                    "uid" : { "type" : "User", "id" : "oliver"},
                    "attrs" : {
                        "manager": { "__entity": { "type" : "User", "id" : "george"} }
                    },
                    "parents" : [
                        { "type" : "Document", "id" : "oliverdocument"}
                    ]
                },
                {
                    "uid" : { "type" : "User", "id" : "george"},
                    "attrs" : {
                    },
                    "parents" : [
                        { "type" : "Document", "id" : "georgedocument"}
                    ]
                },
            ]
        );

        expect_entity_slice_to(
            entities_json,
            expected_entities_json,
            &schema,
            &expected_manifest,
        );
    }

    #[test]
    fn test_entity_manifest_multiple_types() {
        let mut pset = PolicySet::new();
        let policy = parse_policy(
            None,
            "permit(principal, action, resource)
when {
    principal.name == \"John\"
};",
        )
        .expect("should succeed");
        pset.add(policy.into()).expect("should succeed");

        let schema = ValidatorSchema::from_str_natural(
            "
entity User = {
  name: String,
};

entity OtherUserType = {
  name: String,
  irrelevant: String,
};

entity Document;

action Read appliesTo {
  principal: [User, OtherUserType],
  resource: [Document]
};
        ",
            Extensions::all_available(),
        )
        .unwrap()
        .0;

        let entity_manifest = compute_entity_manifest(&schema, &pset).expect("Should succeed");
        let expected = r#"
{
  "per_action": [
    [
      {
        "principal": "User",
        "action": {
          "ty": "Action",
          "eid": "Read"
        },
        "resource": "Document"
      },
      {
        "trie": [
          [
            {
              "Var": "principal"
            },
            {
              "children": [
                [
                  "name",
                  {
                    "children": [],
                    "ancestors_required": false
                  }
                ]
              ],
              "ancestors_required": false
            }
          ]
        ]
      }
    ],
    [
      {
        "principal": "OtherUserType",
        "action": {
          "ty": "Action",
          "eid": "Read"
        },
        "resource": "Document"
      },
      {
        "trie": [
          [
            {
              "Var": "principal"
            },
            {
              "children": [
                [
                  "name",
                  {
                    "children": [],
                    "ancestors_required": false
                  }
                ]
              ],
              "ancestors_required": false
            }
          ]
        ]
      }
    ]
  ]
}"#;
        let expected_manifest = serde_json::from_str(expected).unwrap();
        assert_eq!(entity_manifest, expected_manifest);
    }

    #[test]
    fn test_entity_manifest_multiple_branches() {
        let mut pset = PolicySet::new();
        let policy1 = parse_policy(
            None,
            r#"
permit(
  principal,
  action == Action::"Read",
  resource
)
when
{
  resource.readers.contains(principal)
};"#,
        )
        .unwrap();
        let policy2 = parse_policy(
            Some(PolicyID::from_string("Policy2")),
            r#"permit(
  principal,
  action == Action::"Read",
  resource
)
when
{
  resource.metadata.owner == principal
};"#,
        )
        .unwrap();
        pset.add(policy1.into()).expect("should succeed");
        pset.add(policy2.into()).expect("should succeed");

        let schema = ValidatorSchema::from_str_natural(
            "
entity User;

entity Metadata = {
   owner: User,
   time: String,
};

entity Document = {
  metadata: Metadata,
  readers: Set<User>,
};

action Read appliesTo {
  principal: [User],
  resource: [Document]
};
        ",
            Extensions::all_available(),
        )
        .unwrap()
        .0;

        let entity_manifest = compute_entity_manifest(&schema, &pset).expect("Should succeed");
        let expected = r#"
{
  "per_action": [
    [
      {
        "principal": "User",
        "action": {
          "ty": "Action",
          "eid": "Read"
        },
        "resource": "Document"
      },
      {
        "trie": [
          [
            {
              "Var": "resource"
            },
            {
              "children": [
                [
                  "metadata",
                  {
                    "children": [
                      [
                        "owner",
                        {
                          "children": [],
                          "ancestors_required": false
                        }
                      ]
                    ],
                    "ancestors_required": false
                  }
                ],
                [
                  "readers",
                  {
                    "children": [],
                    "ancestors_required": false
                  }
                ]
              ],
              "ancestors_required": false
            }
          ]
        ]
      }
    ]
  ]
}"#;
        let expected_manifest = serde_json::from_str(expected).unwrap();
        assert_eq!(entity_manifest, expected_manifest);

        let entities_json = serde_json::json!(
            [
                {
                    "uid" : { "type" : "User", "id" : "oliver"},
                    "attrs" : {
                    },
                    "parents" : [
                    ]
                },
                {
                    "uid": { "type": "Document", "id": "dummy"},
                    "attrs": {
                        "metadata": { "type": "Metadata", "id": "olivermetadata"},
                        "readers": [{"type": "User", "id": "oliver"}]
                    },
                    "parents": [],
                },
                {
                    "uid": { "type": "Metadata", "id": "olivermetadata"},
                    "attrs": {
                        "owner": { "type": "User", "id": "oliver"},
                        "time": "now"
                    },
                    "parents": [],
                },
            ]
        );

        let expected_entities_json = serde_json::json!(
            [
                {
                    "uid": { "type": "Document", "id": "dummy"},
                    "attrs": {
                        "metadata": {"__entity": { "type": "Metadata", "id": "olivermetadata"}},
                        "readers": [{ "__entity": {"type": "User", "id": "oliver"}}]
                    },
                    "parents": [],
                },
                {
                    "uid": { "type": "Metadata", "id": "olivermetadata"},
                    "attrs": {
                        "owner": {"__entity": { "type": "User", "id": "oliver"}},
                    },
                    "parents": [],
                },
                {
                    "uid" : { "type" : "User", "id" : "oliver"},
                    "attrs" : {
                    },
                    "parents" : [
                    ]
                },
            ]
        );

        expect_entity_slice_to(
            entities_json,
            expected_entities_json,
            &schema,
            &expected_manifest,
        );
    }

    #[test]
    fn test_entity_manifest_struct_equality() {
        let mut pset = PolicySet::new();
        // we need to load all of the metadata, not just nickname
        // no need to load actual name
        let policy = parse_policy(
            None,
            r#"permit(principal, action, resource)
when {
    principal.metadata.nickname == "timmy" && principal.metadata == {
        "friends": [ "oliver" ],
        "nickname": "timmy"
    }
};"#,
        )
        .expect("should succeed");
        pset.add(policy.into()).expect("should succeed");

        let schema = ValidatorSchema::from_str_natural(
            "
entity User = {
  name: String,
  metadata: {
    friends: Set<String>,
    nickname: String,
  },
};

entity Document;

action BeSad appliesTo {
  principal: [User],
  resource: [Document]
};
        ",
            Extensions::all_available(),
        )
        .unwrap()
        .0;

        let entity_manifest = compute_entity_manifest(&schema, &pset).expect("Should succeed");
        let expected = r#"
{
  "per_action": [
    [
      {
        "principal": "User",
        "action": {
          "ty": "Action",
          "eid": "BeSad"
        },
        "resource": "Document"
      },
      {
        "trie": [
          [
            {
              "Var": "principal"
            },
            {
              "children": [
                [
                  "metadata",
                  {
                    "children": [
                      [
                        "nickname",
                        {
                          "children": [],
                          "ancestors_required": false
                        }
                      ],
                      [
                        "friends",
                        {
                          "children": [],
                          "ancestors_required": false
                        }
                      ]
                    ],
                    "ancestors_required": false
                  }
                ]
              ],
              "ancestors_required": false
            }
          ]
        ]
      }
    ]
  ]
}"#;
        let expected_manifest = serde_json::from_str(expected).unwrap();
        assert_eq!(entity_manifest, expected_manifest);
    }

    #[test]
    fn test_entity_manifest_struct_equality_left_right_different() {
        let mut pset = PolicySet::new();
        // we need to load all of the metadata, not just nickname
        // no need to load actual name
        let policy = parse_policy(
            None,
            r#"permit(principal, action, resource)
when {
    principal.metadata == resource.metadata
};"#,
        )
        .expect("should succeed");
        pset.add(policy.into()).expect("should succeed");

        let schema = ValidatorSchema::from_str_natural(
            "
entity User = {
  name: String,
  metadata: {
    friends: Set<String>,
    nickname: String,
  },
};

entity Document;

action Hello appliesTo {
  principal: [User],
  resource: [User]
};
        ",
            Extensions::all_available(),
        )
        .unwrap()
        .0;

        let entity_manifest = compute_entity_manifest(&schema, &pset).expect("Should succeed");
        let expected = r#"
{
  "per_action": [
    [
      {
        "principal": "User",
        "action": {
          "ty": "Action",
          "eid": "Hello"
        },
        "resource": "User"
      },
      {
        "trie": [
          [
            {
              "Var": "resource"
            },
            {
              "children": [
                [
                  "metadata",
                  {
                    "children": [
                      [
                        "friends",
                        {
                          "children": [],
                          "ancestors_required": false
                        }
                      ],
                      [
                        "nickname",
                        {
                          "children": [],
                          "ancestors_required": false
                        }
                      ]
                    ],
                    "ancestors_required": false
                  }
                ]
              ],
              "ancestors_required": false
            }
          ],
          [
            {
              "Var": "principal"
            },
            {
              "children": [
                [
                  "metadata",
                  {
                    "children": [
                      [
                        "nickname",
                        {
                          "children": [],
                          "ancestors_required": false
                        }
                      ],
                      [
                        "friends",
                        {
                          "children": [],
                          "ancestors_required": false
                        }
                      ]
                    ],
                    "ancestors_required": false
                  }
                ]
              ],
              "ancestors_required": false
            }
          ]
        ]
      }
    ]
  ]
}"#;
        let expected_manifest = serde_json::from_str(expected).unwrap();
        assert_eq!(entity_manifest, expected_manifest);
    }
}