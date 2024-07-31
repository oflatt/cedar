use std::collections::HashMap;

use cedar_policy_core::{
    ast::{Entity, EntityUID, PartialValue, Request, Var},
    entities::{Dereference, Entities, NoEntitiesSchema, TCComputation},
    extensions::Extensions,
};
use smol_str::SmolStr;

use crate::entity_manifest::{EntityManifest, EntityRoot, EntitySliceError, RootAccessTrie};

impl<T: Clone> EntityManifest<T> {
    /// Use this entity manifest to
    /// find an entity slice using an existing [`Entities`] store.
    pub fn slice(
        &self,
        entities: &Entities,
        request: &Request,
    ) -> Result<Entities, EntitySliceError> {
        let request_type = request
            .to_request_type()
            .ok_or(EntitySliceError::PartialRequestError)?;
        self.per_action
            .get(&request_type)
            .map(|primary| primary.slice(entities, request))
            .unwrap_or(Ok(Entities::default()))
    }
}

impl<T: Clone> RootAccessTrie<T> {
    /// Given entities and a request, return a new entitity store
    /// which is a slice of the old one.
    fn slice(
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
                    let entity_id = request
                        .action()
                        .uid()
                        .ok_or(EntitySliceError::PartialRequestError)?;
                    slice.slice_entity(entities, entity_id, &mut res)?;
                }
                EntityRoot::Var(Var::Principal) => {
                    let entity_id = request
                        .principal()
                        .uid()
                        .ok_or(EntitySliceError::PartialRequestError)?;
                    slice.slice_entity(entities, entity_id, &mut res)?;
                }
                EntityRoot::Var(Var::Resource) => {
                    let resource_id = request
                        .resource()
                        .uid()
                        .ok_or(EntitySliceError::PartialRequestError)?;
                    slice.slice_entity(entities, resource_id, &mut res)?;
                }
                EntityRoot::Var(Var::Context) => {
                    if slice.children.is_empty() {
                        // no data loading needed
                    } else {
                        let partial_val: PartialValue = PartialValue::from(
                            request
                                .context()
                                .ok_or(EntitySliceError::PartialRequestError)?
                                .clone(),
                        );
                        let PartialValue::Value(val) = partial_val else {
                            return Err(EntitySliceError::PartialRequestError);
                        };
                        slice.slice_val(entities, &val, &mut res);
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

impl<T: Clone> RootAccessTrie<T> {
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
                    return Err(EntitySliceError::PartialEntity);
                };
                let sliced = slice.slice_val(entities, &val, res)?;

                new_entity.insert(field.clone(), PartialValue::Value(sliced));
            }
        }

        let new_ancestors = if self.parents_required {
            entity.ancestors().cloned().collect()
        } else {
            HashSet::new()
        };

        let new_entity =
            Entity::new_with_attr_partial_value(lit.clone(), new_entity, new_ancestors);

        #[allow(clippy::expect_used)]
        if let Some(existing) = res.get_mut(lit) {
            // Here we union the new entity with any existing one
            // PANIC SAFETY: Entities in the entity store with the same ID should be compatible to union together.
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
            !self.parents_required
                || matches!(val.value_kind(), ValueKind::Lit(Literal::EntityUID(_)))
        );

        Ok(match val.value_kind() {
            ValueKind::Lit(Literal::EntityUID(id)) => {
                self.slice_entity(entities, id, res)?;
                val.clone()
            }
            ValueKind::Set(_) | ValueKind::ExtensionValue(_) | ValueKind::Lit(_) => {
                if !self.children.is_empty() {
                    return Err(EntitySliceError::IncompatibleEntityManifest(val.clone()));
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
