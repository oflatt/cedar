use std::collections::{HashMap, HashSet};

use cedar_policy_core::{
    ast::{Expr, ExprKind, PolicyID},
    authorizer::PartialResponse,
};

use crate::{
    typecheck::{TypecheckAnswer, Typechecker}, types::Type, SchemaFragment, ValidationMode
};

enum EntitiesNeeded {
    All,
    Particular(HashSet<String>),
}

#[derive(Default)]
struct EntitiesNeededPerType {
    per_type: HashMap<String, EntitiesNeeded>,
}

impl EntitiesNeededPerType {
    fn union(&self, other: &EntitiesNeededPerType) -> EntitiesNeededPerType {
        todo!()
    }
}

fn residual_entities_needed(
    schema: SchemaFragment,
    residual: &PartialResponse,
) -> EntitiesNeededPerType {
    let into_schema = schema.try_into().expect("failed to construct schema.");
    let typechecker = Typechecker::new(
        &into_schema,
        ValidationMode::Strict,
        // TODO is this correct?
        PolicyID::from_string("0"),
    );
    let residuals = residual.all_residuals();
    let mut res = EntitiesNeededPerType::default();

    for residual in residuals {
        let mut errors = HashSet::new();
        let TypecheckAnswer::TypecheckSuccess { expr_type, .. } =
            typechecker.typecheck_expr(&residual.condition(), &mut errors)
        else {
            panic!("Failed to type check {}", residual.condition());
        };
        res = res.union(&expr_entities_needed(&expr_type));
    }
    res
}

// given an expression, attempt to break it
// into a conjunct of multiple expressions
// by finding `And` operations
fn get_conjunct(expr: &Expr<Option<Type>>) -> Vec<Expr<Option<Type>>> {
    match expr.expr_kind() {
        ExprKind::And { left, right } => {
            let mut res = get_conjunct(left);
            res.extend(get_conjunct(right));
            res
        }
        _ => {
            vec![expr.clone()]
        }
    }
}

fn expr_entities_needed(expr: &Expr<Option<Type>>) -> EntitiesNeededPerType {
    // TODO use this to look for equalities
    let _conjunct = get_conjunct(expr);
    match expr.expr_kind() {
        ExprKind::Lit(_) => EntitiesNeededPerType::default(),
        ExprKind::Var(v) => {
            expr.data
        }
        ExprKind::Slot(_) => todo!(),
        ExprKind::Unknown(_) => todo!(),
        ExprKind::If {
            test_expr,
            then_expr,
            else_expr,
        } => todo!(),
        ExprKind::And { left, right } => todo!(),
        ExprKind::Or { left, right } => todo!(),
        ExprKind::UnaryApp { op, arg } => todo!(),
        ExprKind::BinaryApp { op, arg1, arg2 } => todo!(),
        ExprKind::ExtensionFunctionApp { fn_name, args } => todo!(),
        ExprKind::GetAttr { expr, attr } => todo!(),
        ExprKind::HasAttr { expr, attr } => todo!(),
        ExprKind::Like { expr, pattern } => todo!(),
        ExprKind::Is { expr, entity_type } => todo!(),
        ExprKind::Set(_) => todo!(),
        ExprKind::Record(_) => todo!(),
    }
}
