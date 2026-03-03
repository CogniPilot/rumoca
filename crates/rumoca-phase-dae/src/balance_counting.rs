//! Balance reporting helpers for ToDAE diagnostics.

use rumoca_ir_dae as dae;

/// Compute (equation, unknown) scalar counts used for unbalanced diagnostics.
///
/// This mirrors DAE balance semantics: interface and overconstrained corrections
/// only close deficits, while break-edge correction only reduces overdetermined
/// systems.
pub(crate) fn compute_balance_counts(dae: &dae::Dae) -> (usize, usize) {
    let detail = dae.balance_detail();
    compute_balance_counts_from_detail(&detail)
}

fn compute_balance_counts_from_detail(detail: &dae::BalanceDetail) -> (usize, usize) {
    let unknowns = detail.state_unknowns + detail.alg_unknowns + detail.output_unknowns;
    let brk = detail.oc_break_edge_scalar_count as i64;
    let available_oc_interface = detail.overconstrained_interface_count.max(0);
    let base_equations = (detail.f_x_scalar
        + detail.algorithm_outputs
        + detail.when_eq_scalar
        + detail.interface_flow_count) as i64;
    let oc_needed = (unknowns as i64 - base_equations).max(0);
    let effective_oc_interface = available_oc_interface.min(oc_needed);
    let raw_equations = base_equations + effective_oc_interface;
    let raw_balance = raw_equations - unknowns as i64;
    let effective_brk = brk.min(raw_balance.max(0));
    let equations = (raw_equations - effective_brk) as usize;
    (equations, unknowns)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn detail(
        unknowns: usize,
        f_x_scalar: usize,
        interface_flow_count: usize,
        overconstrained_interface_count: i64,
        oc_break_edge_scalar_count: usize,
    ) -> dae::BalanceDetail {
        dae::BalanceDetail {
            state_unknowns: unknowns,
            alg_unknowns: 0,
            output_unknowns: 0,
            f_x_scalar,
            algorithm_outputs: 0,
            when_eq_scalar: 0,
            interface_flow_count,
            overconstrained_interface_count,
            oc_break_edge_scalar_count,
        }
    }

    #[test]
    fn test_compute_balance_counts_caps_overconstrained_correction_to_deficit() {
        let (equations, unknowns) = compute_balance_counts_from_detail(&detail(8, 6, 0, 10, 0));
        assert_eq!(unknowns, 8);
        assert_eq!(equations, 8);
    }

    #[test]
    fn test_compute_balance_counts_applies_break_edge_only_to_positive_surplus() {
        let (equations, unknowns) = compute_balance_counts_from_detail(&detail(6, 8, 0, 0, 5));
        assert_eq!(unknowns, 6);
        assert_eq!(equations, 6);
    }
}
