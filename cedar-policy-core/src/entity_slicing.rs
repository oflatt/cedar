use std::collections::{HashMap, HashSet};

use crate::{
    ast::{Expr, ExprKind},
    authorizer::PartialResponse,
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

fn residual_entities_needed(residual: &PartialResponse) -> EntitiesNeededPerType {
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
    let conjunct = get_conjunct(expr);
    let entities_needed = EntitiesNeededPerType::default();


    entities_needed
}
