use indexmap::{IndexMap, IndexSet};

use rumoca_core::{Expression, OpBinary, OpUnary, VarName};
use rumoca_ir_dae as dae;

use super::runtime_protection::assignment_var_ref_name;

pub(super) struct DirectDefinitionIndex {
    per_equation: Vec<Vec<VarName>>,
    counts: IndexMap<VarName, usize>,
}

impl DirectDefinitionIndex {
    pub(super) fn build(dae: &dae::Dae) -> Self {
        let mut per_equation = Vec::with_capacity(dae.continuous.equations.len());
        let mut counts = IndexMap::<VarName, usize>::new();
        for equation in &dae.continuous.equations {
            let targets = direct_assignment_targets(&equation.rhs);
            for target in &targets {
                *counts.entry(target.clone()).or_default() += 1;
            }
            per_equation.push(targets.into_iter().collect());
        }
        Self {
            per_equation,
            counts,
        }
    }

    pub(super) fn len(&self) -> usize {
        self.counts.len()
    }

    pub(super) fn has_other_direct_definition(
        &self,
        current_eq_idx: usize,
        candidate: &VarName,
    ) -> bool {
        let count = self.counts.get(candidate).copied().unwrap_or_default();
        if count == 0 {
            return false;
        }
        let current_has_definition = self
            .per_equation
            .get(current_eq_idx)
            .is_some_and(|targets| targets.iter().any(|target| target == candidate));
        count > usize::from(current_has_definition)
    }

    pub(super) fn has_other_non_connection_direct_definition(
        &self,
        dae: &dae::Dae,
        current_eq_idx: usize,
        candidate: &VarName,
    ) -> bool {
        self.per_equation
            .iter()
            .enumerate()
            .any(|(eq_idx, targets)| {
                eq_idx != current_eq_idx
                    && targets.iter().any(|target| target == candidate)
                    && !dae
                        .continuous
                        .equations
                        .get(eq_idx)
                        .is_some_and(|eq| eq.origin.contains("connection"))
            })
    }
}

fn direct_assignment_targets(expr: &Expression) -> IndexSet<VarName> {
    let mut targets = IndexSet::new();
    collect_direct_assignment_targets(expr, &mut targets);
    targets
}

fn collect_direct_assignment_targets(expr: &Expression, targets: &mut IndexSet<VarName>) {
    match expr {
        Expression::Binary {
            op: OpBinary::Sub,
            lhs,
            rhs,
            ..
        } => {
            if let Some(target) = direct_assignment_side_target(lhs) {
                targets.insert(target);
            }
            if let Some(target) = direct_assignment_side_target(rhs) {
                targets.insert(target);
            }
        }
        Expression::Unary {
            op: OpUnary::Minus,
            rhs,
            ..
        } => collect_direct_assignment_targets(rhs, targets),
        _ => {}
    }
}

fn direct_assignment_side_target(expr: &Expression) -> Option<VarName> {
    let Expression::VarRef {
        name, subscripts, ..
    } = expr
    else {
        return None;
    };
    assignment_var_ref_name(name.var_name(), subscripts)
}
