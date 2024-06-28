//! Entity Slicing

use std::collections::{HashMap, HashSet};

use cedar_policy_core::{
    ast::{EntityType, EntityUID, Expr, ExprKind, Policy, PolicyID, PolicySet, ACTION_ENTITY_TYPE},
    authorizer::PartialResponse,
};

use crate::{
    typecheck::{TypecheckAnswer, Typechecker},
    types::Type,
    SchemaFragment, ValidationMode, ValidatorActionId, ValidatorSchema,
};

/// Data structure that tells the user what data is needed
/// based on the action's ID
pub type EntitySliceManifest = HashMap<EntityUID, PrimarySlice>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DataField {
    /// The parents in the parent hierarchy
    Parents,
    /// A field of a struct or entity
    Field(String),
}

/// A map of data fields to entity slices
pub type Fields = HashMap<DataField, Box<EntitySlice>>;


/// a [`PrimarySlice`] is a tree that tells you what data to load
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PrimarySlice {
    /// the fields of the principal entity needed
    principal_slice: Fields,
    /// the fields of the resource entity needed
    resource_slice: Fields,
    /// the fields of the context entity needed
    context_slice: Fields,
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
        }
    }

    /// Union two [`PrimarySlice`]s together, requiring
    /// the data that both of them require
    fn union(&self, other: &Self) -> Self {
        PrimarySlice {
            principal_slice: union_fields(&self.principal_slice.clone(), &other.principal_slice),
            resource_slice: union_fields(&self.resource_slice.clone(), &other.resource_slice),
            context_slice: union_fields(&self.context_slice.clone(), &other.context_slice),
        }
    }
}

/// Constraints on entity slices
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Constraint {}

/// An entity slice- tells users a tree of data to load
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EntitySlice {
    /// Constraints allow us to load part of the data.
    /// e.g. for a set, we could load only some elements
    constraints: HashSet<Constraint>,
    /// Child data of this entity slice.
    children: HashMap<DataField, Box<EntitySlice>>,
}

impl EntitySlice {
    fn union(&self, other: &Self) -> Self {
        EntitySlice {
            constraints: self.constraints.union(&other.constraints).cloned().collect(), 
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

    entity_slice_manifest
}


fn add_policy_to_manifest(manifest: &mut EntitySliceManifest, policy: &Policy) {
    // first, lets generate the primary slice
    let primary_slice = compute_primary_slice(policy);

    // we check what action the policy is for
    // for each of the actions, we add the primary slice
    match policy.action_constraint() {
        cedar_policy_core::ast::ActionConstraint::Any => {

        },
        cedar_policy_core::ast::ActionConstraint::In(_) => todo!(),
        cedar_policy_core::ast::ActionConstraint::Eq(_) => todo!(),
    }
}

fn compute_primary_slice(policy: &Policy) -> PrimarySlice {
    todo!()
}