//! Entity Slicing

use std::collections::{HashMap, HashSet};

use cedar_policy_core::ast::{
        EntityUID, Expr, ExprKind, Policy, PolicySet, Var,
    };
use serde::{Deserialize, Serialize};
use serde_with::serde_as;
use smol_str::SmolStr;

use crate::ValidatorSchema;

type PerAction = HashMap<EntityUID, PrimarySlice>;

/// Data structure that tells the user what data is needed
/// based on the action's ID
#[serde_as]
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
pub struct EntitySliceManifest {
    /// A map from actions to primary slice
    #[serde_as(as = "Vec<(_, _)>")]
    pub per_action: PerAction,
}

/// A data field- either an entities field,
/// a struct field, or cedar's parent relation
#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub enum DataField {
    /// The parents in the parent hierarchy
    Parents,
    /// A field of a struct or entity
    Field(SmolStr),
}

/// A map of data fields to entity slices
pub type Fields = HashMap<DataField, Box<EntitySlice>>;

/// a [`PrimarySlice`] is a tree that tells you what data to load
#[serde_as]
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
pub struct PrimarySlice {
    /// the fields of the principal entity needed
    #[serde_as(as = "Vec<(_, _)>")]
    principal_slice: Fields,
    /// the fields of the resource entity needed
    #[serde_as(as = "Vec<(_, _)>")]
    resource_slice: Fields,
    /// the fields of the action entity needed
    #[serde_as(as = "Vec<(_, _)>")]
    action_slice: Fields,
    /// the fields of the context entity needed
    #[serde_as(as = "Vec<(_, _)>")]
    context_slice: Fields,
}

/// Constraints on entity slices
#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub enum Constraint {}

/// An entity slice- tells users a tree of data to load
#[serde_as]
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
pub struct EntitySlice {
    /// Constraints allow us to load part of the data.
    /// e.g. for a set, we could load only some elements
    constraints: HashSet<Constraint>,
    /// Child data of this entity slice.
    #[serde_as(as = "Vec<(_, _)>")]
    children: Fields,
}

fn union_fields(first: &Fields, second: &Fields) -> Fields {
    let mut res = first.clone();
    for (key, value) in second {
        if let Some(existing) = res.get(key) {
            res.insert(key.clone(), Box::new((*existing).union(value)));
        } else {
            res.insert(key.clone(), value.clone());
        }
    }
    res
}

impl PrimarySlice {
    /// Create an empty [`PrimarySlice`] that requires no data
    pub fn new() -> Self {
        Self {
            principal_slice: Default::default(),
            resource_slice: Default::default(),
            context_slice: Default::default(),
            action_slice: Default::default(),
        }
    }

    /// Union two [`PrimarySlice`]s together, requiring
    /// the data that both of them require
    fn union(&self, other: &Self) -> Self {
        PrimarySlice {
            principal_slice: union_fields(&self.principal_slice.clone(), &other.principal_slice),
            resource_slice: union_fields(&self.resource_slice.clone(), &other.resource_slice),
            context_slice: union_fields(&self.context_slice.clone(), &other.context_slice),
            action_slice: union_fields(&self.action_slice.clone(), &other.action_slice),
        }
    }
}

impl Default for PrimarySlice {
    fn default() -> Self {
        Self::new()
    }
}

impl EntitySlice {
    fn union(&self, other: &Self) -> Self {
        EntitySlice {
            constraints: self
                .constraints
                .union(&other.constraints)
                .cloned()
                .collect(),
            children: union_fields(&self.children, &other.children),
        }
    }
}

/// Computes an [`EntitySliceManifest`] from the schema and policies
pub fn compute_entity_slice_manifest(
    schema: &ValidatorSchema,
    policies: &PolicySet,
) -> EntitySliceManifest {
    let mut entity_slice_manifest = HashMap::new();

    // first, make an entry for every action in the schema
    for action_entity in schema.action_entities_iter() {
        entity_slice_manifest.insert(action_entity.uid().clone(), PrimarySlice::new());
    }

    // now, for each policy we add the data it requires to the manifest
    for policy in policies.policies() {
        add_policy_to_manifest(&mut entity_slice_manifest, policy);
    }

    EntitySliceManifest {
        per_action: entity_slice_manifest,
    }
}

fn add_policy_to_manifest(manifest: &mut PerAction, policy: &Policy) {
    // first, lets generate the primary slice
    let primary_slice = compute_primary_slice(policy);

    // we check what action the policy is for
    // for each of the actions, we add the primary slice
    match policy.action_constraint() {
        cedar_policy_core::ast::ActionConstraint::Any => {
            for (_action, slice) in manifest.iter_mut() {
                *slice = slice.union(&primary_slice);
            }
        }
        cedar_policy_core::ast::ActionConstraint::In(actions) => {
            for action in actions {
                if let Some(existing) = manifest.get(action) {
                    manifest.insert((**action).clone(), existing.union(&primary_slice));
                } else {
                    manifest.insert((**action).clone(), primary_slice.clone());
                }
            }
        }
        cedar_policy_core::ast::ActionConstraint::Eq(action) => {
            if let Some(existing) = manifest.get(action) {
                manifest.insert((**action).clone(), existing.union(&primary_slice));
            } else {
                manifest.insert((**action).clone(), primary_slice.clone());
            }
        }
    }
}

fn compute_primary_slice(policy: &Policy) -> PrimarySlice {
    let mut primary_slice = PrimarySlice::new();
    add_to_primary_slice(&mut primary_slice, &policy.condition());
    primary_slice
}

fn add_to_primary_slice(primary_slice: &mut PrimarySlice, expr: &Expr) {
    match expr.expr_kind() {
        ExprKind::Lit(_) => (),
        ExprKind::Var(_) => (),
        ExprKind::Slot(_) => panic!("Templates not allowed when computing entity manifest"),
        ExprKind::Unknown(_) => {
            panic!("Partial expressions not allowed when computing entity manifest")
        }
        ExprKind::If {
            test_expr,
            then_expr,
            else_expr,
        } => {
            add_to_primary_slice(primary_slice, test_expr);
            add_to_primary_slice(primary_slice, then_expr);
            add_to_primary_slice(primary_slice, else_expr);
        }
        ExprKind::And { left, right } => {
            add_to_primary_slice(primary_slice, left);
            add_to_primary_slice(primary_slice, right);
        }
        ExprKind::Or { left, right } => {
            add_to_primary_slice(primary_slice, left);
            add_to_primary_slice(primary_slice, right);
        }
        ExprKind::UnaryApp { op, arg } => {
            add_to_primary_slice(primary_slice, arg);
        }
        // TODO this is not sound for equality checks between structs
        // we need to fix to load in everything for these
        ExprKind::BinaryApp { op, arg1, arg2 } => {
            add_to_primary_slice(primary_slice, arg1);
            add_to_primary_slice(primary_slice, arg2);
        }
        ExprKind::ExtensionFunctionApp { fn_name, args } => {
            panic!("Extension functions not supported by entity manifest")
        }
        ExprKind::Like { expr, pattern } => {
            add_to_primary_slice(primary_slice, expr);
        }
        ExprKind::Is { expr, entity_type } => {
            add_to_primary_slice(primary_slice, expr);
        }
        ExprKind::Set(contents) => {
            for expr in &**contents {
                add_to_primary_slice(primary_slice, &expr);
            }
        }
        ExprKind::Record(content) => {
            for expr in content.values() {
                add_to_primary_slice(primary_slice, &expr);
            }
        }
        ExprKind::GetAttr { .. } | ExprKind::HasAttr { .. } => {
            let (base, path) = get_expr_path(expr);
            let slice = path_to_field(path);

            match base {
                Var::Principal => {
                    primary_slice.principal_slice =
                        union_fields(&primary_slice.principal_slice, &slice)
                }
                Var::Action => {
                    primary_slice.action_slice = union_fields(&primary_slice.action_slice, &slice)
                }
                Var::Resource => {
                    primary_slice.resource_slice =
                        union_fields(&primary_slice.resource_slice, &slice)
                }
                Var::Context => {
                    primary_slice.context_slice = union_fields(&primary_slice.context_slice, &slice)
                }
            }
        }
    }
}

fn path_to_field(path: Vec<SmolStr>) -> Fields {
    let mut current = HashMap::new();
    // reverse the path, visiting the last access first
    for field in path.iter().rev() {
        let slice = EntitySlice {
            children: current,
            constraints: HashSet::new(),
        };
        current = HashMap::new();
        current.insert(DataField::Field(field.clone()), Box::new(slice));
    }
    current
}

fn get_expr_path(expr: &Expr) -> (Var, Vec<SmolStr>) {
    match expr.expr_kind() {
        ExprKind::Var(var) => (*var, vec![]),
        ExprKind::GetAttr { expr, attr } => {
            let (base, mut path) = get_expr_path(expr);
            path.push(attr.clone());
            (base, path)
        }
        ExprKind::HasAttr { expr, attr } => {
            let (base, mut path) = get_expr_path(expr);
            path.push(attr.clone());
            (base, path)
        }
        _ => panic!(
            "Tried to use GetAttr or HasAttr on an expression with operation {:?}",
            expr.expr_kind()
        ),
    }
}
