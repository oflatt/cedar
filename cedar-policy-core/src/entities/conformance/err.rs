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
//! This module cotnains errors around entities not conforming to schemas
use super::TypeMismatchError;
use crate::ast::{Eid, EntityType, EntityUID};
use crate::extensions::ExtensionFunctionLookupError;
use crate::impl_diagnostic_from_method_on_field;
use itertools::Itertools;
use miette::Diagnostic;
use smol_str::SmolStr;
use thiserror::Error;

/// Errors raised when entities do not conform to the schema
//
// CAUTION: this type is publicly exported in `cedar-policy`.
// Don't make fields `pub`, don't make breaking changes, and use caution
// when adding public methods.
#[derive(Debug, Diagnostic, Error)]
#[non_exhaustive]
pub enum EntitySchemaConformanceError {
    /// Encountered attribute that shouldn't exist on entities of this type
    #[error(transparent)]
    #[diagnostic(transparent)]
    UnexpectedEntityAttr(UnexpectedEntityAttr),
    /// Encountered tag, but no tags should exist on entities of this type
    #[error(transparent)]
    #[diagnostic(transparent)]
    UnexpectedEntityTag(UnexpectedEntityTag),
    /// Didn't encounter attribute that should exist
    #[error(transparent)]
    #[diagnostic(transparent)]
    MissingRequiredEntityAttr(MissingRequiredEntityAttr),
    /// The given attribute on the given entity had a different type than the
    /// schema indicated
    #[error(transparent)]
    #[diagnostic(transparent)]
    TypeMismatch(TypeMismatch),
    /// Found an ancestor of a type that's not allowed for that entity
    #[error(transparent)]
    #[diagnostic(transparent)]
    InvalidAncestorType(InvalidAncestorType),
    /// Encountered an entity of a type which is not declared in the schema.
    /// Note that this error is only used for non-Action entity types.
    #[error(transparent)]
    #[diagnostic(transparent)]
    UnexpectedEntityType(#[from] UnexpectedEntityTypeError),
    /// Encountered an action which was not declared in the schema
    #[error(transparent)]
    #[diagnostic(transparent)]
    UndeclaredAction(#[from] UndeclaredAction),
    /// Encountered an action whose definition doesn't precisely match the
    /// schema's declaration of that action
    #[error(transparent)]
    #[diagnostic(transparent)]
    ActionDeclarationMismatch(ActionDeclarationMismatch),
    /// Error looking up an extension function. This error can occur when
    /// checking entity conformance because that may require getting information
    /// about any extension functions referenced in entity attribute values.
    #[error(transparent)]
    #[diagnostic(transparent)]
    ExtensionFunctionLookup(ExtensionFunctionLookup),
    /// Returned when an entity is of an enumerated entity type but has invalid EID
    #[error(transparent)]
    #[diagnostic(transparent)]
    InvalidEnumEntity(#[from] InvalidEnumEntity),
}

impl EntitySchemaConformanceError {
    pub(crate) fn unexpected_entity_attr(uid: EntityUID, attr: impl Into<SmolStr>) -> Self {
        Self::UnexpectedEntityAttr(UnexpectedEntityAttr {
            uid,
            attr: attr.into(),
        })
    }

    pub(crate) fn unexpected_entity_tag(uid: EntityUID, tag: impl Into<SmolStr>) -> Self {
        Self::UnexpectedEntityTag(UnexpectedEntityTag {
            uid,
            tag: tag.into(),
        })
    }

    pub(crate) fn missing_entity_attr(uid: EntityUID, attr: impl Into<SmolStr>) -> Self {
        Self::MissingRequiredEntityAttr(MissingRequiredEntityAttr {
            uid,
            attr: attr.into(),
        })
    }

    pub(crate) fn type_mismatch(
        uid: EntityUID,
        attr_or_tag: impl Into<SmolStr>,
        context: AttrOrTag,
        err: TypeMismatchError,
    ) -> Self {
        Self::TypeMismatch(TypeMismatch {
            uid,
            attr_or_tag: attr_or_tag.into(),
            context,
            err,
        })
    }

    pub(crate) fn invalid_ancestor_type(uid: EntityUID, ancestor_type: EntityType) -> Self {
        Self::InvalidAncestorType(InvalidAncestorType {
            uid,
            ancestor_ty: Box::new(ancestor_type),
        })
    }

    pub(crate) fn undeclared_action(uid: EntityUID) -> Self {
        Self::UndeclaredAction(UndeclaredAction { uid })
    }

    pub(crate) fn action_declaration_mismatch(uid: EntityUID) -> Self {
        Self::ActionDeclarationMismatch(ActionDeclarationMismatch { uid })
    }

    pub(crate) fn extension_function_lookup(
        uid: EntityUID,
        attr_or_tag: impl Into<SmolStr>,
        context: AttrOrTag,
        err: ExtensionFunctionLookupError,
    ) -> Self {
        Self::ExtensionFunctionLookup(ExtensionFunctionLookup {
            uid,
            attr_or_tag: attr_or_tag.into(),
            context,
            err,
        })
    }
}

/// Error looking up an extension function. This error can occur when
/// checking entity conformance because that may require getting information
/// about any extension functions referenced in entity attribute values.
//
// CAUTION: this type is publicly exported in `cedar-policy`.
// Don't make fields `pub`, don't make breaking changes, and use caution
// when adding public methods.
#[derive(Debug, Error, Diagnostic)]
#[error("in `{context}` `{attr_or_tag}` on `{uid}`, {err}")]
pub struct ExtensionFunctionLookup {
    /// Entity where the error occurred
    uid: EntityUID,
    /// Name of the attribute where the error occurred
    attr_or_tag: SmolStr,
    /// If it shows up as an attribute or a tag value
    context: AttrOrTag,
    /// Underlying error
    #[diagnostic(transparent)]
    err: ExtensionFunctionLookupError,
}

/// Encountered an action whose definition doesn't precisely match the
/// schema's declaration of that action
//
// CAUTION: this type is publicly exported in `cedar-policy`.
// Don't make fields `pub`, don't make breaking changes, and use caution
// when adding public methods.
#[derive(Debug, Error, Diagnostic)]
#[error("definition of action `{uid}` does not match its schema declaration")]
#[diagnostic(help(
    "to use the schema's definition of `{uid}`, simply omit it from the entities input data"
))]
pub struct ActionDeclarationMismatch {
    /// Action whose definition mismatched between entity data and schema
    uid: EntityUID,
}

/// Encountered an action which was not declared in the schema
//
// CAUTION: this type is publicly exported in `cedar-policy`.
// Don't make fields `pub`, don't make breaking changes, and use caution
// when adding public methods.
#[derive(Debug, Error, Diagnostic)]
#[error("found action entity `{uid}`, but it was not declared as an action in the schema")]
pub struct UndeclaredAction {
    /// Action which was not declared in the schema
    pub(crate) uid: EntityUID,
}

/// Found an ancestor of a type that's not allowed for that entity
//
// CAUTION: this type is publicly exported in `cedar-policy`.
// Don't make fields `pub`, don't make breaking changes, and use caution
// when adding public methods.
#[derive(Debug, Error, Diagnostic)]
#[error(
    "`{uid}` is not allowed to have an ancestor of type `{ancestor_ty}` according to the schema"
)]
pub struct InvalidAncestorType {
    /// Entity that has an invalid ancestor type
    uid: EntityUID,
    /// Ancestor type which was invalid
    ancestor_ty: Box<EntityType>, // boxed to avoid this variant being very large (and thus all EntitySchemaConformanceErrors being large)
}

/// Encountered attribute that shouldn't exist on entities of this type
//
// CAUTION: this type is publicly exported in `cedar-policy`.
// Don't make fields `pub`, don't make breaking changes, and use caution
// when adding public methods.
#[derive(Debug, Error, Diagnostic)]
#[error("attribute `{attr}` on `{uid}` should not exist according to the schema")]
pub struct UnexpectedEntityAttr {
    uid: EntityUID,
    attr: SmolStr,
}

/// Encountered tag, but no tags should exist on entities of this type
//
// CAUTION: this type is publicly exported in `cedar-policy`.
// Don't make fields `pub`, don't make breaking changes, and use caution
// when adding public methods.
#[derive(Debug, Error, Diagnostic)]
#[error(
    "found a tag `{tag}` on `{uid}`, but no tags should exist on `{uid}` according to the schema"
)]
pub struct UnexpectedEntityTag {
    uid: EntityUID,
    tag: SmolStr,
}

/// Didn't encounter attribute that should exist
//
// CAUTION: this type is publicly exported in `cedar-policy`.
// Don't make fields `pub`, don't make breaking changes, and use caution
// when adding public methods.
#[derive(Debug, Error, Diagnostic)]
#[error("expected entity `{uid}` to have attribute `{attr}`, but it does not")]
pub struct MissingRequiredEntityAttr {
    uid: EntityUID,
    attr: SmolStr,
}

/// If the context is an attribute or a tag
#[derive(Debug, Clone, Copy)]
pub enum AttrOrTag {
    /// When it's an attribute
    Attr,
    /// When it's a tag
    Tag,
}

impl std::fmt::Display for AttrOrTag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AttrOrTag::Attr => write!(f, "attribute"),
            AttrOrTag::Tag => write!(f, "tag"),
        }
    }
}

/// The given attribute on the given entity had a different type than the
/// schema indicated
//
// CAUTION: this type is publicly exported in `cedar-policy`.
// Don't make fields `pub`, don't make breaking changes, and use caution
// when adding public methods.
#[derive(Debug, Error, Diagnostic)]
#[error("in {context} `{attr_or_tag}` on `{uid}`, {err}")]
pub struct TypeMismatch {
    uid: EntityUID,
    attr_or_tag: SmolStr,
    context: AttrOrTag,
    #[diagnostic(transparent)]
    err: TypeMismatchError,
}

/// Encountered an entity of a type which is not declared in the schema.
/// Note that this error is only used for non-Action entity types.
//
// CAUTION: this type is publicly exported in `cedar-policy`.
// Don't make fields `pub`, don't make breaking changes, and use caution
// when adding public methods.
#[derive(Debug, Error)]
#[error("entity `{uid}` has type `{}` which is not declared in the schema", .uid.entity_type())]
pub struct UnexpectedEntityTypeError {
    /// Entity that had the unexpected type
    pub uid: EntityUID,
    /// Suggested similar entity types that actually are declared in the schema (if any)
    pub suggested_types: Vec<EntityType>,
}

impl Diagnostic for UnexpectedEntityTypeError {
    fn help<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        match self.suggested_types.as_slice() {
            [] => None,
            [ty] => Some(Box::new(format!("did you mean `{ty}`?"))),
            tys => Some(Box::new(format!(
                "did you mean one of {:?}?",
                tys.iter().map(ToString::to_string).collect::<Vec<String>>()
            ))),
        }
    }
}

/// Returned when an entity is of an enumerated entity type but has invalid EID
//
// CAUTION: this type is publicly exported in `cedar-policy`.
// Don't make fields `pub`, don't make breaking changes, and use caution
// when adding public methods.
#[derive(Debug, Error, Diagnostic)]
#[error(transparent)]
#[diagnostic(transparent)]
pub struct InvalidEnumEntity {
    err: InvalidEnumEntityError,
}

impl From<InvalidEnumEntityError> for InvalidEnumEntity {
    fn from(value: InvalidEnumEntityError) -> Self {
        Self { err: value }
    }
}

/// Returned when an entity is of an enumerated entity type but has invalid EID
#[derive(Debug, Error, Clone, PartialEq, Eq, Hash)]
#[error("entity `{uid}` is of an enumerated entity type, but `\"{}\"` is not declared as a valid eid", uid.eid().escaped())]
pub struct InvalidEnumEntityError {
    /// Entity where the error occurred
    pub uid: EntityUID,
    /// Name of the attribute where the error occurred
    pub choices: Vec<Eid>,
}

impl Diagnostic for InvalidEnumEntityError {
    impl_diagnostic_from_method_on_field!(uid, loc);

    fn help<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        Some(Box::new(format!(
            "valid entity eids: {}",
            self.choices
                .iter()
                .map(|e| format!("\"{}\"", e.escaped()))
                .join(", ")
        )))
    }
}
