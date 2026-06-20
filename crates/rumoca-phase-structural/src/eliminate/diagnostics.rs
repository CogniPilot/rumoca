use rumoca_ir_dae::expr_contains_var;

use super::{Dae, StructuralError, VarName};

pub(super) fn trace_singular_reduced_rows(trace: bool, dae: &Dae, err: &StructuralError) {
    if !trace {
        return;
    }
    let StructuralError::Singular {
        unmatched_equations,
        unmatched_unknowns,
        ..
    } = err
    else {
        return;
    };
    for name in unmatched_equations {
        let Some(index) = name
            .strip_prefix("f_x[")
            .and_then(|tail| tail.strip_suffix(']'))
            .and_then(|value| value.parse::<usize>().ok())
        else {
            continue;
        };
        let Some(eq) = dae.continuous.equations.get(index) else {
            continue;
        };
        crate::structural_trace!(
            "[sim-trace] eliminate_trivial singular reduced {name} origin='{}' rhs={:?}",
            eq.origin,
            eq.rhs
        );
    }
    for unknown in unmatched_unknowns {
        let unknown_name = VarName::new(unknown.clone());
        for (index, eq) in dae.continuous.equations.iter().enumerate() {
            if !expr_contains_var(&eq.rhs, &unknown_name) {
                continue;
            }
            crate::structural_trace!(
                "[sim-trace] eliminate_trivial reduced ref {unknown} in f_x[{index}] origin='{}' rhs={:?}",
                eq.origin,
                eq.rhs
            );
        }
    }
}
