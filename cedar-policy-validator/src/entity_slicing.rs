use std::collections::{HashMap, HashSet};

use cedar_policy_core::{ast::PolicyID, authorizer::PartialResponse};

use crate::{typecheck::Typechecker, SchemaFragment, ValidationMode};

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
    let typechecker = Typechecker::new(
        &schema.try_into().expect("failed to construct schema."),
        ValidationMode::Strict,
        // TODO is this correct?
        PolicyID::from_string("0"),
    );
    let residuals = residual.all_residuals();
    let mut res = EntitiesNeededPerType::default();
    for residual in residuals {
        res = res.union(&expr_entities_needed(&residual.condition()));
    }
    res
}

// given an expression, attempt to break it
// into a conjunct of multiple expressions
// by finding `And` operations
fn get_conjunct(expr: &Expr) -> Vec<Expr> {
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

fn expr_entities_needed(expr: &Expr) -> EntitiesNeededPerType {
    // TODO use this to look for equalities
    let _conjunct = get_conjunct(expr);

    match expr.expr_kind() {
        ExprKind::Lit(_) => EntitiesNeededPerType::default(),
        ExprKind::Var(_) => {}
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
