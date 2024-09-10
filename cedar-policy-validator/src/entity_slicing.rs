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

//! Entity Slicing

use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt::Display;

use cedar_policy_core::entities::err::EntitiesError;
use cedar_policy_core::entities::{Dereference, NoEntitiesSchema, TCComputation};
use cedar_policy_core::extensions::Extensions;
use cedar_policy_core::{
    ast::{Entity, EntityUID, Literal, PartialValue, Request, Value, ValueKind, Var},
    entities::Entities,
};
use miette::Diagnostic;
use smol_str::SmolStr;
use thiserror::Error;

use crate::entity_manifest::{
    AccessTrie, EntityManifest, EntityRoot, PartialRequestError, RootAccessTrie,
};

/// Error when expressions are partial during entity
/// slicing.
// CAUTION: this type is publicly exported in `cedar-policy`.
// Don't make fields `pub`, don't make breaking changes, and use caution
// when adding public methods.
#[derive(Debug, Clone, Error, Eq, PartialEq)]
#[error(
    "Entity slicing requires fully concrete policies. Got a policy with an unknown expression."
)]
pub struct PartialExpressionError {}

impl Diagnostic for PartialExpressionError {}

/// Error when expressions are partial during entity
/// slicing.
// CAUTION: this type is publicly exported in `cedar-policy`.
// Don't make fields `pub`, don't make breaking changes, and use caution
// when adding public methods.
#[derive(Debug, Clone, Error, Eq, PartialEq)]
#[error(
    "Entity slicing requires fully concrete policies. Got a policy with an unknown expression."
)]
pub struct IncompatibleEntityManifestError {
    non_record_entity_value: Value,
}

impl Diagnostic for IncompatibleEntityManifestError {
    fn help<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        Some(Box::new(format!(
            "Expected entity or record during entity loading. Got value: {}",
            self.non_record_entity_value
        )))
    }
}

/// Error when entities are partial during entity manifest computation.
// CAUTION: this type is publicly exported in `cedar-policy`.
// Don't make fields `pub`, don't make breaking changes, and use caution
// when adding public methods.
#[derive(Debug, Clone, Error, Eq, PartialEq)]
#[error("Entity slicing requires fully concrete entities. Got a partial entity.")]
pub struct PartialEntityError {}

impl Diagnostic for PartialEntityError {}

/// An error generated by entity slicing.
/// TODO make public API wrapper
#[derive(Debug, Error, Diagnostic)]
pub enum EntitySliceError {
    /// An entities error was encountered
    #[error(transparent)]
    #[diagnostic(transparent)]
    Entities(#[from] EntitiesError),

    /// The request was partial
    #[error(transparent)]
    PartialRequest(#[from] PartialRequestError),
    /// A policy was partial
    #[error(transparent)]
    PartialExpression(#[from] PartialExpressionError),

    /// During entity loading, attempted to load from
    /// a type without fields.
    #[error(transparent)]
    IncompatibleEntityManifest(#[from] IncompatibleEntityManifestError),

    /// Found a partial entity during entity loading.
    #[error(transparent)]
    PartialEntity(#[from] PartialEntityError),
}

impl EntityManifest {
    /// Use this entity manifest to
    /// find an entity slice using an existing [`Entities`] store.
    pub fn slice_entities(
        &self,
        entities: &Entities,
        request: &Request,
    ) -> Result<Entities, EntitySliceError> {
        let request_type = request.to_request_type().ok_or(PartialRequestError {})?;
        self.per_action
            .get(&request_type)
            .map(|primary| primary.slice_entities(entities, request))
            .unwrap_or(Ok(Entities::default()))
    }
}

impl RootAccessTrie {
    /// Given entities and a request, return a new entitity store
    /// which is a slice of the old one.
    fn slice_entities(
        &self,
        entities: &Entities,
        request: &Request,
    ) -> Result<Entities, EntitySliceError> {
        let mut res = HashMap::<EntityUID, Entity>::new();
        for (root, slice) in &self.trie {
            match root {
                EntityRoot::Literal(lit) => {
                    slice.slice_entity(entities, lit, &mut res)?;
                }
                EntityRoot::Var(Var::Action) => {
                    let entity_id = request.action().uid().ok_or(PartialRequestError {})?;
                    slice.slice_entity(entities, entity_id, &mut res)?;
                }
                EntityRoot::Var(Var::Principal) => {
                    let entity_id = request.principal().uid().ok_or(PartialRequestError {})?;
                    slice.slice_entity(entities, entity_id, &mut res)?;
                }
                EntityRoot::Var(Var::Resource) => {
                    let resource_id = request.resource().uid().ok_or(PartialRequestError {})?;
                    slice.slice_entity(entities, resource_id, &mut res)?;
                }
                EntityRoot::Var(Var::Context) => {
                    if slice.children.is_empty() {
                        // no data loading needed
                    } else {
                        let partial_val: PartialValue = PartialValue::from(
                            request.context().ok_or(PartialRequestError {})?.clone(),
                        );
                        let PartialValue::Value(val) = partial_val else {
                            return Err(PartialRequestError {}.into());
                        };
                        slice.slice_val(entities, &val, &mut res)?;
                    }
                }
            }
        }
        Ok(Entities::from_entities(
            res.into_values(),
            None::<&NoEntitiesSchema>,
            TCComputation::AssumeAlreadyComputed,
            Extensions::all_available(),
        )?)
    }
}

impl AccessTrie {
    /// Given an entities store, an entity id, and a resulting store
    /// Slice the entities and put them in the resulting store.
    fn slice_entity(
        &self,
        entities: &Entities,
        lit: &EntityUID,
        res: &mut HashMap<EntityUID, Entity>,
    ) -> Result<(), EntitySliceError> {
        // If the entity is not present, no need to slice
        let Dereference::Data(entity) = entities.entity(lit) else {
            return Ok(());
        };
        let mut new_entity = HashMap::<SmolStr, PartialValue>::new();
        for (field, slice) in &self.children {
            // only slice when field is available
            if let Some(pval) = entity.get(field).cloned() {
                let PartialValue::Value(val) = pval else {
                    return Err(PartialEntityError {}.into());
                };
                let sliced = slice.slice_val(entities, &val, res)?;

                new_entity.insert(field.clone(), PartialValue::Value(sliced));
            }
        }

        let new_ancestors = if self.ancestors_required {
            entity.ancestors().cloned().collect()
        } else {
            HashSet::new()
        };

        let new_entity =
            Entity::new_with_attr_partial_value(lit.clone(), new_entity, new_ancestors);

        // PANIC SAFETY: Entities in the entity store with the same ID should be compatible to union together.
        #[allow(clippy::expect_used)]
        if let Some(existing) = res.get_mut(lit) {
            // Here we union the new entity with any existing one
            *existing = existing
                .union(&new_entity)
                .expect("Incompatible values found in entity store");
        } else {
            res.insert(lit.clone(), new_entity);
        }
        Ok(())
    }

    fn slice_val(
        &self,
        entities: &Entities,
        val: &Value,
        res: &mut HashMap<EntityUID, Entity>,
    ) -> Result<Value, EntitySliceError> {
        // unless this is an entity id, parents should not be required
        assert!(
            !self.ancestors_required
                || matches!(val.value_kind(), ValueKind::Lit(Literal::EntityUID(_)))
        );

        Ok(match val.value_kind() {
            ValueKind::Lit(Literal::EntityUID(id)) => {
                self.slice_entity(entities, id, res)?;
                val.clone()
            }
            ValueKind::Set(_) | ValueKind::ExtensionValue(_) | ValueKind::Lit(_) => {
                if !self.children.is_empty() {
                    return Err(IncompatibleEntityManifestError {
                        non_record_entity_value: val.clone(),
                    }
                    .into());
                }

                val.clone()
            }
            ValueKind::Record(record) => {
                let mut new_map = BTreeMap::<SmolStr, Value>::new();
                for (field, slice) in &self.children {
                    // only slice when field is available
                    if let Some(v) = record.get(field) {
                        new_map.insert(field.clone(), slice.slice_val(entities, v, res)?);
                    }
                }

                Value::new(ValueKind::record(new_map), None)
            }
        })
    }
}

#[cfg(test)]
mod entity_slice_tests {
    use cedar_policy_core::{
        ast::{Context, PolicyID, PolicySet},
        entities::EntityJsonParser,
        parser::parse_policy,
    };

    use crate::{entity_manifest::compute_entity_manifest, CoreSchema, ValidatorSchema};

    use super::*;

    // Schema for testing in this module
    fn schema() -> ValidatorSchema {
        ValidatorSchema::from_cedarschema_str(
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

        let sliced_entities = manifest
            .slice_entities(&original_entities, &request)
            .unwrap();

        // PANIC SAFETY: panic in testing when test fails
        #[allow(clippy::panic)]
        if !sliced_entities.deep_equal(&expected_entities) {
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
            &entity_manifest,
        );
    }

    #[test]
    #[should_panic(expected = "Sliced entities differed")]
    fn sanity_test_empty_entity_manifest() {
        let mut pset = PolicySet::new();
        let policy =
            parse_policy(None, "permit(principal, action, resource);").expect("should succeed");
        pset.add(policy.into()).expect("should succeed");

        let schema = schema();

        let entity_manifest = compute_entity_manifest(&schema, &pset).expect("Should succeed");

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
            &entity_manifest,
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
            &entity_manifest,
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

        let schema = ValidatorSchema::from_cedarschema_str(
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
            &entity_manifest,
        );
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

        let schema = ValidatorSchema::from_cedarschema_str(
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
            &entity_manifest,
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

        let schema = ValidatorSchema::from_cedarschema_str(
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
        assert_eq!(entity_manifest, entity_manifest);
    }
}
