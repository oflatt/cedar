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

//! This module defines the publicly exported error types.

use crate::EntityUid;
use crate::PolicyId;
use cedar_policy_core::ast;
pub use cedar_policy_core::ast::RestrictedExpressionParseError;
use cedar_policy_core::authorizer;
use cedar_policy_core::est;
pub use cedar_policy_core::evaluator::{evaluation_errors, EvaluationError};
pub use cedar_policy_core::extensions::{
    extension_function_lookup_errors, ExtensionFunctionLookupError,
};
pub use cedar_policy_core::parser::err::{ParseError, ParseErrors};
pub use cedar_policy_validator::human_schema::SchemaWarning;
pub use cedar_policy_validator::schema_error;
use miette::Diagnostic;
use ref_cast::RefCast;
use smol_str::SmolStr;
use thiserror::Error;

/// Errors that can occur during authorization
#[derive(Debug, Diagnostic, PartialEq, Eq, Error, Clone)]
pub enum AuthorizationError {
    /// An error occurred when evaluating a policy.
    #[error(transparent)]
    #[diagnostic(transparent)]
    PolicyEvaluationError(#[from] PolicyEvaluationError),
}

impl AuthorizationError {
    /// Get the id of the erroring policy
    pub fn id(&self) -> &PolicyId {
        match self {
            Self::PolicyEvaluationError(e) => e.id(),
        }
    }
}

/// An error occurred when evaluating a policy
#[derive(Debug, Diagnostic, PartialEq, Eq, Error, Clone)]
#[error("while evaluating policy `{id}`: {error}")]
pub struct PolicyEvaluationError {
    /// Id of the policy with an error
    id: ast::PolicyID,
    /// Underlying evaluation error
    #[diagnostic(transparent)]
    error: EvaluationError,
}

impl PolicyEvaluationError {
    /// Get the [`PolicyId`] of the erroring policy
    pub fn id(&self) -> &PolicyId {
        PolicyId::ref_cast(&self.id)
    }

    /// Get the underlying [`EvaluationError`]
    pub fn inner(&self) -> &EvaluationError {
        &self.error
    }

    /// Consume this error, producing the underlying [`EvaluationError`]
    pub fn into_inner(self) -> EvaluationError {
        self.error
    }
}

#[doc(hidden)]
impl From<authorizer::AuthorizationError> for AuthorizationError {
    fn from(value: authorizer::AuthorizationError) -> Self {
        match value {
            authorizer::AuthorizationError::PolicyEvaluationError { id, error } => {
                Self::PolicyEvaluationError(PolicyEvaluationError { id, error })
            }
        }
    }
}

/// Errors that can be encountered when re-evaluating a partial response
#[derive(Debug, Error)]
pub enum ReAuthorizeError {
    /// An evaluation error was encountered
    #[error("{err}")]
    Evaluation {
        /// The evaluation error
        #[from]
        err: EvaluationError,
    },
    /// A policy id conflict was found
    #[error("{err}")]
    PolicySet {
        /// The conflicting ids
        #[from]
        err: cedar_policy_core::ast::PolicySetError,
    },
}

/// Errors serializing Schemas to the natural syntax
#[derive(Debug, Error, Diagnostic)]
pub enum ToHumanSyntaxError {
    /// Duplicate names were found in the schema
    #[error(transparent)]
    #[diagnostic(transparent)]
    NameCollisions(#[from] to_human_syntax_errors::NameCollisionsError),
}

/// Error subtypes for [`ToHumanSyntaxError`]
pub mod to_human_syntax_errors {
    use itertools::Itertools;
    use miette::Diagnostic;
    use nonempty::NonEmpty;
    use smol_str::SmolStr;
    use thiserror::Error;

    /// Duplicate names were found in the schema
    #[derive(Debug, Error, Diagnostic)]
    #[error("There are name collisions: [{}]", .names.iter().join(", "))]
    pub struct NameCollisionsError {
        /// Names that had collisions
        names: NonEmpty<SmolStr>,
    }

    impl NameCollisionsError {
        /// Construct a new [`NameCollisionsError`]
        pub(crate) fn new(names: NonEmpty<SmolStr>) -> Self {
            Self { names }
        }

        /// Get the names that had collisions
        pub fn names(&self) -> impl Iterator<Item = &str> {
            self.names.iter().map(|n| n.as_str())
        }
    }
}

#[doc(hidden)]
impl From<cedar_policy_validator::human_schema::ToHumanSchemaStrError> for ToHumanSyntaxError {
    fn from(value: cedar_policy_validator::human_schema::ToHumanSchemaStrError) -> Self {
        match value {
            cedar_policy_validator::human_schema::ToHumanSchemaStrError::NameCollisions(
                collisions,
            ) => Self::NameCollisions(to_human_syntax_errors::NameCollisionsError::new(collisions)),
        }
    }
}

mod human_schema_error {
    use crate::schema_error::SchemaError;
    use miette::Diagnostic;
    use thiserror::Error;

    /// Parsing errors for human-readable schemas
    #[derive(Debug, Error, Diagnostic)]
    #[error(transparent)]
    #[diagnostic(transparent)]
    pub struct ParseError(#[from] pub(super) cedar_policy_validator::HumanSyntaxParseError);

    /// Errors when converting parsed human-readable schemas into full schemas
    #[derive(Debug, Error, Diagnostic)]
    #[error(transparent)]
    #[diagnostic(transparent)]
    pub struct CoreError(#[from] pub(super) SchemaError);

    /// IO errors when parsing human-readable schemas
    #[derive(Debug, Error, Diagnostic)]
    #[error(transparent)]
    pub struct IoError(#[from] pub(super) std::io::Error);
}

/// Errors when parsing schemas
#[derive(Debug, Diagnostic, Error)]
pub enum HumanSchemaError {
    /// Error parsing a schema in natural syntax
    #[error(transparent)]
    #[diagnostic(transparent)]
    Parse(#[from] human_schema_error::ParseError),
    /// Errors combining fragments into full schemas
    #[error(transparent)]
    #[diagnostic(transparent)]
    Core(#[from] human_schema_error::CoreError),
    /// IO errors while parsing
    #[error(transparent)]
    #[diagnostic(transparent)]
    Io(#[from] human_schema_error::IoError),
}

#[doc(hidden)]
impl From<cedar_policy_validator::HumanSchemaError> for HumanSchemaError {
    fn from(value: cedar_policy_validator::HumanSchemaError) -> Self {
        match value {
            cedar_policy_validator::HumanSchemaError::Core(core) => {
                human_schema_error::CoreError(core).into()
            }
            cedar_policy_validator::HumanSchemaError::IO(io_err) => {
                human_schema_error::IoError(io_err).into()
            }
            cedar_policy_validator::HumanSchemaError::Parsing(e) => {
                human_schema_error::ParseError(e).into()
            }
        }
    }
}

#[doc(hidden)]
impl From<crate::schema_error::SchemaError> for HumanSchemaError {
    fn from(value: crate::schema_error::SchemaError) -> Self {
        human_schema_error::CoreError(value).into()
    }
}

/// Error when evaluating an entity attribute
#[derive(Debug, Diagnostic, Error)]
#[error("in attribute `{attr}` of `{uid}`: {err}")]
pub struct EntityAttrEvaluationError {
    /// Action that had the attribute with the error
    uid: EntityUid,
    /// Attribute that had the error
    attr: SmolStr,
    /// Underlying evaluation error
    #[diagnostic(transparent)]
    err: EvaluationError,
}

impl EntityAttrEvaluationError {
    /// Get the [`EntityUid`] of the action that had the attribute with the error
    pub fn action(&self) -> &EntityUid {
        &self.uid
    }

    /// Get the name of the attribute that had the error
    pub fn attr(&self) -> &SmolStr {
        &self.attr
    }

    /// Get the underlying evaluation error
    pub fn inner(&self) -> &EvaluationError {
        &self.err
    }
}

#[doc(hidden)]
impl From<ast::EntityAttrEvaluationError> for EntityAttrEvaluationError {
    fn from(err: ast::EntityAttrEvaluationError) -> Self {
        Self {
            uid: EntityUid::new(err.uid),
            attr: err.attr,
            err: err.err,
        }
    }
}

/// This module contains definitions for structs containing detailed information
/// about each validation error. Errors are primarily documented on their
/// variants in [`ValidationError`].
pub mod validation_errors;

/// An error generated by the validator when it finds a potential problem in a
/// policy. The error contains a enumeration that specifies the kind of problem,
/// and provides details specific to that kind of problem. The error also records
/// where the problem was encountered.
#[derive(Debug, Clone, Error, Diagnostic)]
#[non_exhaustive]
pub enum ValidationError {
    /// A policy contains an entity type that is not declared in the schema.
    #[error(transparent)]
    #[diagnostic(transparent)]
    UnrecognizedEntityType(#[from] validation_errors::UnrecognizedEntityType),
    /// A policy contains an action that is not declared in the schema.
    #[error(transparent)]
    #[diagnostic(transparent)]
    UnrecognizedActionId(#[from] validation_errors::UnrecognizedActionId),
    /// There is no action satisfying the action scope constraint that can be
    /// applied to a principal and resources that both satisfy their respective
    /// scope conditions.
    #[error(transparent)]
    #[diagnostic(transparent)]
    InvalidActionApplication(#[from] validation_errors::InvalidActionApplication),
    /// An unspecified entity was used in a policy. This should be impossible,
    /// assuming that the policy was constructed by the parser.
    #[error(transparent)]
    #[diagnostic(transparent)]
    UnspecifiedEntity(#[from] validation_errors::UnspecifiedEntity),
    /// The typechecker expected to see a subtype of one of the types in
    /// `expected`, but saw `actual`.
    #[error(transparent)]
    #[diagnostic(transparent)]
    UnexpectedType(#[from] validation_errors::UnexpectedType),
    /// The typechecker could not compute a least upper bound for `types`.
    #[error(transparent)]
    #[diagnostic(transparent)]
    IncompatibleTypes(#[from] validation_errors::IncompatibleTypes),
    /// The typechecker detected an access to a record or entity attribute
    /// that it could not statically guarantee would be present.
    #[error(transparent)]
    #[diagnostic(transparent)]
    UnsafeAttributeAccess(#[from] validation_errors::UnsafeAttributeAccess),
    /// The typechecker could not conclude that an access to an optional
    /// attribute was safe.
    #[error(transparent)]
    #[diagnostic(transparent)]
    UnsafeOptionalAttributeAccess(#[from] validation_errors::UnsafeOptionalAttributeAccess),
    /// Undefined extension function.
    #[error(transparent)]
    #[diagnostic(transparent)]
    UndefinedFunction(#[from] validation_errors::UndefinedFunction),
    /// Multiply defined extension function.
    #[error(transparent)]
    #[diagnostic(transparent)]
    MultiplyDefinedFunction(#[from] validation_errors::MultiplyDefinedFunction),
    /// Incorrect number of arguments in an extension function application.
    #[error(transparent)]
    #[diagnostic(transparent)]
    WrongNumberArguments(#[from] validation_errors::WrongNumberArguments),
    /// Incorrect call style in an extension function application.
    #[error(transparent)]
    #[diagnostic(transparent)]
    WrongCallStyle(#[from] validation_errors::WrongCallStyle),
    /// Error returned by custom extension function argument validation
    #[diagnostic(transparent)]
    #[error(transparent)]
    FunctionArgumentValidation(#[from] validation_errors::FunctionArgumentValidation),
    /// Error returned when an empty set literal is found in a policy.
    #[diagnostic(transparent)]
    #[error(transparent)]
    EmptySetForbidden(#[from] validation_errors::EmptySetForbidden),
    /// Error returned when an extension constructor is applied to an non-literal expression.
    #[diagnostic(transparent)]
    #[error(transparent)]
    NonLitExtConstructor(#[from] validation_errors::NonLitExtConstructor),
    /// To pass strict validation a policy cannot contain an `in` expression
    /// where the entity type on the left might not be able to be a member of
    /// the entity type on the right.
    #[error(transparent)]
    #[diagnostic(transparent)]
    HierarchyNotRespected(#[from] validation_errors::HierarchyNotRespected),
}

impl ValidationError {
    /// Extract the policy id of the policy where the validator found the issue.
    pub fn policy_id(&self) -> &crate::PolicyId {
        match self {
            ValidationError::UnrecognizedEntityType(e) => e.policy_id(),
            ValidationError::UnrecognizedActionId(e) => e.policy_id(),
            ValidationError::InvalidActionApplication(e) => e.policy_id(),
            ValidationError::UnspecifiedEntity(e) => e.policy_id(),
            ValidationError::UnexpectedType(e) => e.policy_id(),
            ValidationError::IncompatibleTypes(e) => e.policy_id(),
            ValidationError::UnsafeAttributeAccess(e) => e.policy_id(),
            ValidationError::UnsafeOptionalAttributeAccess(e) => e.policy_id(),
            ValidationError::UndefinedFunction(e) => e.policy_id(),
            ValidationError::MultiplyDefinedFunction(e) => e.policy_id(),
            ValidationError::WrongNumberArguments(e) => e.policy_id(),
            ValidationError::WrongCallStyle(e) => e.policy_id(),
            ValidationError::FunctionArgumentValidation(e) => e.policy_id(),
            ValidationError::EmptySetForbidden(e) => e.policy_id(),
            ValidationError::NonLitExtConstructor(e) => e.policy_id(),
            ValidationError::HierarchyNotRespected(e) => e.policy_id(),
        }
    }
}

#[doc(hidden)]
impl From<cedar_policy_validator::ValidationError> for ValidationError {
    fn from(error: cedar_policy_validator::ValidationError) -> Self {
        match error {
            cedar_policy_validator::ValidationError::UnrecognizedEntityType(e) => {
                Self::UnrecognizedEntityType(e.into())
            }
            cedar_policy_validator::ValidationError::UnrecognizedActionId(e) => {
                Self::UnrecognizedActionId(e.into())
            }
            cedar_policy_validator::ValidationError::InvalidActionApplication(e) => {
                Self::InvalidActionApplication(e.into())
            }
            cedar_policy_validator::ValidationError::UnspecifiedEntity(e) => {
                Self::UnspecifiedEntity(e.into())
            }
            cedar_policy_validator::ValidationError::UnexpectedType(e) => {
                Self::UnexpectedType(e.into())
            }
            cedar_policy_validator::ValidationError::IncompatibleTypes(e) => {
                Self::IncompatibleTypes(e.into())
            }
            cedar_policy_validator::ValidationError::UnsafeAttributeAccess(e) => {
                Self::UnsafeAttributeAccess(e.into())
            }
            cedar_policy_validator::ValidationError::UnsafeOptionalAttributeAccess(e) => {
                Self::UnsafeOptionalAttributeAccess(e.into())
            }
            cedar_policy_validator::ValidationError::UndefinedFunction(e) => {
                Self::UndefinedFunction(e.into())
            }
            cedar_policy_validator::ValidationError::MultiplyDefinedFunction(e) => {
                Self::MultiplyDefinedFunction(e.into())
            }
            cedar_policy_validator::ValidationError::WrongNumberArguments(e) => {
                Self::WrongNumberArguments(e.into())
            }
            cedar_policy_validator::ValidationError::WrongCallStyle(e) => {
                Self::WrongCallStyle(e.into())
            }
            cedar_policy_validator::ValidationError::FunctionArgumentValidation(e) => {
                Self::FunctionArgumentValidation(e.into())
            }
            cedar_policy_validator::ValidationError::EmptySetForbidden(e) => {
                Self::EmptySetForbidden(e.into())
            }
            cedar_policy_validator::ValidationError::NonLitExtConstructor(e) => {
                Self::NonLitExtConstructor(e.into())
            }
            cedar_policy_validator::ValidationError::HierarchyNotRespected(e) => {
                Self::HierarchyNotRespected(e.into())
            }
        }
    }
}

/// This module contains definitions for structs containing detailed information
/// about each validation warning. Warnings are primarily documented on their
/// variants in [`ValidationWarning`].
pub mod validation_warnings;

/// Represents the different kinds of validation warnings and information
/// specific to that warning. Marked as `non_exhaustive` to allow adding
/// additional warnings in the future as a non-breaking change.
#[derive(Debug, Clone, Error, Diagnostic)]
#[non_exhaustive]
pub enum ValidationWarning {
    /// A string contains mixed scripts. Different scripts can contain visually similar characters which may be confused for each other.
    #[diagnostic(transparent)]
    #[error(transparent)]
    MixedScriptString(#[from] validation_warnings::MixedScriptString),
    /// A string contains BIDI control characters. These can be used to create crafted pieces of code that obfuscate true control flow.
    #[diagnostic(transparent)]
    #[error(transparent)]
    BidiCharsInString(#[from] validation_warnings::BidiCharsInString),
    /// An id contains BIDI control characters. These can be used to create crafted pieces of code that obfuscate true control flow.
    #[diagnostic(transparent)]
    #[error(transparent)]
    BidiCharsInIdentifier(#[from] validation_warnings::BidiCharsInIdentifier),
    /// An id contains mixed scripts. This can cause characters to be confused for each other.
    #[diagnostic(transparent)]
    #[error(transparent)]
    MixedScriptIdentifier(#[from] validation_warnings::MixedScriptIdentifier),
    /// An id contains characters that fall outside of the General Security Profile for Identifiers. We recommend adhering to this if possible. See Unicode® Technical Standard #39 for more info.
    #[diagnostic(transparent)]
    #[error(transparent)]
    ConfusableIdentifier(#[from] validation_warnings::ConfusableIdentifier),
    /// The typechecker found that a policy condition will always evaluate to false.
    #[diagnostic(transparent)]
    #[error(transparent)]
    ImpossiblePolicy(#[from] validation_warnings::ImpossiblePolicy),
}

impl ValidationWarning {
    /// Extract the policy id of the policy where the validator found the issue.
    pub fn policy_id(&self) -> &PolicyId {
        match self {
            ValidationWarning::MixedScriptString(w) => w.policy_id(),
            ValidationWarning::BidiCharsInString(w) => w.policy_id(),
            ValidationWarning::BidiCharsInIdentifier(w) => w.policy_id(),
            ValidationWarning::MixedScriptIdentifier(w) => w.policy_id(),
            ValidationWarning::ConfusableIdentifier(w) => w.policy_id(),
            ValidationWarning::ImpossiblePolicy(w) => w.policy_id(),
        }
    }
}

#[doc(hidden)]
impl From<cedar_policy_validator::ValidationWarning> for ValidationWarning {
    fn from(warning: cedar_policy_validator::ValidationWarning) -> Self {
        match warning {
            cedar_policy_validator::ValidationWarning::MixedScriptString(w) => {
                Self::MixedScriptString(w.into())
            }
            cedar_policy_validator::ValidationWarning::BidiCharsInString(w) => {
                Self::BidiCharsInString(w.into())
            }
            cedar_policy_validator::ValidationWarning::BidiCharsInIdentifier(w) => {
                Self::BidiCharsInIdentifier(w.into())
            }
            cedar_policy_validator::ValidationWarning::MixedScriptIdentifier(w) => {
                Self::MixedScriptIdentifier(w.into())
            }
            cedar_policy_validator::ValidationWarning::ConfusableIdentifier(w) => {
                Self::ConfusableIdentifier(w.into())
            }
            cedar_policy_validator::ValidationWarning::ImpossiblePolicy(w) => {
                Self::ImpossiblePolicy(w.into())
            }
        }
    }
}

/// Error structs for the variants of `PolicySetError`
pub mod policy_set_error_structs {
    use super::Error;
    use crate::PolicyId;
    use miette::Diagnostic;

    /// There was a duplicate [`PolicyId`] encountered in either the set of
    /// templates or the set of policies.
    #[derive(Debug, Diagnostic, Error)]
    #[error("duplicate template or policy id `{id}`")]
    pub struct AlreadyDefined {
        pub(crate) id: PolicyId,
    }

    /// Expected a static policy, but a template-linked policy was provided
    #[derive(Debug, Diagnostic, Error)]
    #[error("expected a static policy, but a template-linked policy was provided")]
    pub struct ExpectedStatic {}

    /// Expected a template, but a static policy was provided.
    #[derive(Debug, Diagnostic, Error)]
    #[error("expected a template, but a static policy was provided")]
    pub struct ExpectedTemplate {}

    /// Error when removing a static policy that doesn't exist
    #[derive(Debug, Diagnostic, Error)]
    #[error("unable to remove static policy `{policy_id}` because it does not exist")]
    pub struct PolicyNonexistentError {
        pub(crate) policy_id: PolicyId,
    }

    /// Error when removing a static policy that doesn't exist
    #[derive(Debug, Diagnostic, Error)]
    #[error("unable to remove template `{template_id}` because it does not exist")]
    pub struct TemplateNonexistentError {
        pub(crate) template_id: PolicyId,
    }

    /// Error when removing a template with active links
    #[derive(Debug, Diagnostic, Error)]
    #[error("unable to remove policy template `{template_id}` because it has active links")]
    pub struct RemoveTemplateWithActiveLinksError {
        pub(crate) template_id: PolicyId,
    }

    /// Error when removing a template that is not a template
    #[derive(Debug, Diagnostic, Error)]
    #[error("unable to remove policy template `{template_id}` because it is not a template")]
    pub struct RemoveTemplateNotTemplateError {
        pub(crate) template_id: PolicyId,
    }

    /// Error when unlinking a template
    #[derive(Debug, Diagnostic, Error)]
    #[error("unable to unlink policy `{policy_id}` because it does not exist")]
    pub struct LinkNonexistentError {
        pub(crate) policy_id: PolicyId,
    }

    /// Error when removing a link that is not a link
    #[derive(Debug, Diagnostic, Error)]
    #[error("unable to unlink `{policy_id}` because it is not a link")]
    pub struct UnlinkLinkNotLinkError {
        pub(crate) policy_id: PolicyId,
    }
}

/// Potential errors when adding to a `PolicySet`.
#[derive(Debug, Diagnostic, Error)]
#[non_exhaustive]
pub enum PolicySetError {
    /// There was a duplicate [`PolicyId`] encountered in either the set of
    /// templates or the set of policies.
    #[error(transparent)]
    #[diagnostic(transparent)]
    AlreadyDefined(#[from] policy_set_error_structs::AlreadyDefined),
    /// Error when linking a template
    #[error("unable to link template: {0}")]
    #[diagnostic(transparent)]
    LinkingError(#[from] ast::LinkingError),
    /// Expected a static policy, but a template-linked policy was provided
    #[error(transparent)]
    #[diagnostic(transparent)]
    ExpectedStatic(#[from] policy_set_error_structs::ExpectedStatic),
    /// Expected a template, but a static policy was provided.
    #[error(transparent)]
    #[diagnostic(transparent)]
    ExpectedTemplate(#[from] policy_set_error_structs::ExpectedTemplate),
    /// Error when removing a static policy that doesn't exist
    #[error(transparent)]
    #[diagnostic(transparent)]
    PolicyNonexistentError(#[from] policy_set_error_structs::PolicyNonexistentError),
    /// Error when removing a template that doesn't exist
    #[error(transparent)]
    #[diagnostic(transparent)]
    TemplateNonexistentError(#[from] policy_set_error_structs::TemplateNonexistentError),
    /// Error when removing a template with active links
    #[error(transparent)]
    #[diagnostic(transparent)]
    RemoveTemplateWithActiveLinksError(
        #[from] policy_set_error_structs::RemoveTemplateWithActiveLinksError,
    ),
    /// Error when removing a template that is not a template
    #[error(transparent)]
    #[diagnostic(transparent)]
    RemoveTemplateNotTemplateError(
        #[from] policy_set_error_structs::RemoveTemplateNotTemplateError,
    ),
    /// Error when unlinking a linked policy
    #[error(transparent)]
    #[diagnostic(transparent)]
    LinkNonexistentError(#[from] policy_set_error_structs::LinkNonexistentError),
    /// Error when removing a link that is not a link
    #[error(transparent)]
    #[diagnostic(transparent)]
    UnlinkLinkNotLinkError(#[from] policy_set_error_structs::UnlinkLinkNotLinkError),
    /// Error when converting from EST
    #[error("Error deserializing a policy/template from JSON: {0}")]
    #[diagnostic(transparent)]
    FromJson(#[from] cedar_policy_core::est::FromJsonError),
    /// Error when converting to EST
    #[error("Error serializing a policy to JSON: {0}")]
    #[diagnostic(transparent)]
    ToJson(#[from] PolicyToJsonError),
    /// Errors encountered in JSON ser/de of the policy set (as opposed to individual policies)
    #[error("Error serializing / deserializing PolicySet to / from JSON: {0})")]
    Json(#[from] serde_json::Error),
}

#[doc(hidden)]
impl From<ast::PolicySetError> for PolicySetError {
    fn from(e: ast::PolicySetError) -> Self {
        match e {
            ast::PolicySetError::Occupied { id } => {
                Self::AlreadyDefined(policy_set_error_structs::AlreadyDefined {
                    id: PolicyId::new(id),
                })
            }
        }
    }
}

#[doc(hidden)]
impl From<ast::UnexpectedSlotError> for PolicySetError {
    fn from(_: ast::UnexpectedSlotError) -> Self {
        Self::ExpectedStatic(policy_set_error_structs::ExpectedStatic {})
    }
}

/// Errors that can happen when getting the JSON representation of a policy
#[derive(Debug, Diagnostic, Error)]
pub enum PolicyToJsonError {
    /// Parse error in the policy text
    #[error(transparent)]
    #[diagnostic(transparent)]
    Parse(#[from] ParseErrors),
    /// For linked policies, error linking the JSON representation
    #[error(transparent)]
    #[diagnostic(transparent)]
    Link(#[from] json_errors::JsonLinkError),
    /// Error in the JSON serialization
    #[error(transparent)]
    JsonSerialization(#[from] json_errors::PolicyJsonSerializationError),
}

#[doc(hidden)]
impl From<est::LinkingError> for PolicyToJsonError {
    fn from(e: est::LinkingError) -> Self {
        json_errors::JsonLinkError::from(e).into()
    }
}

impl From<serde_json::Error> for PolicyToJsonError {
    fn from(e: serde_json::Error) -> Self {
        json_errors::PolicyJsonSerializationError::from(e).into()
    }
}

/// Error types related to JSON processing
pub mod json_errors {
    use cedar_policy_core::est;
    use miette::Diagnostic;
    use thiserror::Error;

    /// Error linking the JSON representation of a linked policy
    #[derive(Debug, Diagnostic, Error)]
    #[error(transparent)]
    #[diagnostic(transparent)]
    pub struct JsonLinkError {
        /// Underlying error
        #[from]
        err: est::LinkingError,
    }

    /// Error serializing a policy as JSON
    #[derive(Debug, Diagnostic, Error)]
    #[error(transparent)]
    pub struct PolicyJsonSerializationError {
        /// Underlying error
        #[from]
        err: serde_json::Error,
    }
}

/// Error type for parsing `Context` from JSON
#[derive(Debug, Diagnostic, Error)]
pub enum ContextJsonError {
    /// Error deserializing the JSON into a Context
    #[error(transparent)]
    #[diagnostic(transparent)]
    JsonDeserialization(#[from] context_json_errors::ContextJsonDeserializationError),
    /// The supplied action doesn't exist in the supplied schema
    #[error(transparent)]
    #[diagnostic(transparent)]
    MissingAction(#[from] context_json_errors::MissingActionError),
}

impl ContextJsonError {
    /// Construct a `ContextJsonError::MissingAction`
    pub(crate) fn missing_action(action: EntityUid) -> Self {
        Self::MissingAction(context_json_errors::MissingActionError { action })
    }
}

#[doc(hidden)]
impl From<cedar_policy_core::entities::json::ContextJsonDeserializationError> for ContextJsonError {
    fn from(error: cedar_policy_core::entities::json::ContextJsonDeserializationError) -> Self {
        context_json_errors::ContextJsonDeserializationError::from(error).into()
    }
}

/// Error subtypes for [`ContextJsonError`]
pub mod context_json_errors {
    use super::EntityUid;
    use miette::Diagnostic;
    use thiserror::Error;

    /// Error deserializing the JSON into a Context
    #[derive(Debug, Diagnostic, Error)]
    #[error(transparent)]
    pub struct ContextJsonDeserializationError {
        #[diagnostic(transparent)]
        #[from]
        error: cedar_policy_core::entities::json::ContextJsonDeserializationError,
    }

    /// The supplied action doesn't exist in the supplied schema
    #[derive(Debug, Diagnostic, Error)]
    #[error("action `{action}` does not exist in the supplied schema")]
    pub struct MissingActionError {
        /// UID of the action which doesn't exist
        pub(super) action: EntityUid,
    }

    impl MissingActionError {
        /// Get the [`EntityUid`] of the action which doesn't exist
        pub fn action(&self) -> &EntityUid {
            &self.action
        }
    }
}
