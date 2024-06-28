//! Entity Slicing

use std::collections::{HashMap, HashSet};

use cedar_policy_core::{
    ast::{EntityType, EntityUID, Expr, ExprKind, PolicyID, PolicySet, ACTION_ENTITY_TYPE},
    authorizer::PartialResponse,
};

use crate::{
    typecheck::{TypecheckAnswer, Typechecker},
    types::Type,
    SchemaFragment, ValidationMode, ValidatorActionId, ValidatorSchema,
};

/// Data structure that tells the user what data is needed
/// based on the action's ID
pub type EntitySliceManifest = HashMap<EntityUID, Vec<PrimarySlice>>;

/// a [`PrimarySlice`]` is a tree that tells you what data to load
#[derive(Debug)]
pub enum PrimarySlice {
    /// todo
    Principal(Vec<EntitySlice>),
    /// todo
    Resource(Vec<EntitySlice>),
    /// todo
    Context(Vec<EntitySlice>),
}

/// Constraints on entity slices
#[derive(Debug)]
pub enum Constraint {}

/// An entity slice- tells users a tree of data to load
#[derive(Debug)]
pub enum EntitySlice {
    /// read this field of the entity or struct
    Field {
        /// todo
        name: String,
        /// todo
        constraints: Vec<Constraint>,
        /// todo
        children: Vec<EntitySlice>,
    },
    /// pull in the parents of this entity, transitively
    Parents {
        /// todo
        constraints: Vec<Constraint>,
    },
}

/// Computes an [`EntitySliceManifest`] from the schema and policies
pub fn compute_entity_slice_manifest(
    schema: &ValidatorSchema,
    policies: &PolicySet,
) -> EntitySliceManifest {
    let mut entity_slice_manifest = HashMap::new();

    // first, make an entry for every action in the schema
    for action_entity in schema.action_entities_iter() {
        entity_slice_manifest.insert(action_entity.uid().clone(), Vec::new());
    }

    eprintln!("{:?}", entity_slice_manifest);

    entity_slice_manifest
}
