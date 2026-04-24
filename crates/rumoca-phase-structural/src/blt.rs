//! BLT (Block Lower Triangular) block construction from SCCs.

use crate::incidence::Incidence;
use crate::tarjan::tarjan_scc;
use crate::types::{BltBlock, EquationRef, UnknownId};

/// Build BLT blocks from the incidence data, matching, and dependency graph.
///
/// Tarjan emits SCCs in reverse topological order of the condensation DAG.
/// Since dependency edges point from dependent → dependency, this output order
/// is already the correct BLT evaluation order (dependencies first).
pub(crate) fn build_blt_blocks(
    incidence: &Incidence,
    match_eq: &[Option<usize>],
    adj: &[Vec<usize>],
) -> Vec<BltBlock> {
    let sccs = tarjan_scc(incidence.n_eq, adj);
    sccs.into_iter()
        .map(|scc| scc_to_block(&scc, incidence, match_eq))
        .collect()
}

/// Convert a single SCC into a BLT block.
fn scc_to_block(scc: &[usize], incidence: &Incidence, match_eq: &[Option<usize>]) -> BltBlock {
    if scc.len() == 1 {
        let eq_idx = scc[0];
        let eq_ref = incidence.equation_refs[eq_idx].clone();
        let unknown = match match_eq[eq_idx] {
            Some(var_idx) => incidence.unknown_names[var_idx].clone(),
            None => UnknownId::Variable(rumoca_ir_dae::VarName::from("???")),
        };
        BltBlock::Scalar {
            equation: eq_ref,
            unknown,
        }
    } else {
        let equations: Vec<EquationRef> = scc
            .iter()
            .map(|&i| incidence.equation_refs[i].clone())
            .collect();
        let unknowns: Vec<UnknownId> = scc
            .iter()
            .filter_map(|&i| match_eq[i].map(|v| incidence.unknown_names[v].clone()))
            .collect();
        BltBlock::AlgebraicLoop {
            equations,
            unknowns,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    fn make_incidence(
        n_eq: usize,
        n_var: usize,
        eq_unknowns: Vec<HashSet<usize>>,
        eq_refs: Vec<EquationRef>,
        unknown_names: Vec<UnknownId>,
    ) -> Incidence {
        Incidence {
            n_eq,
            n_var,
            eq_unknowns,
            unknown_names,
            equation_refs: eq_refs,
        }
    }

    #[test]
    fn test_blt_linear_chain() {
        // 3 equations in a linear chain: eq0→eq1→eq2 (no cycles)
        // eq0 solves v0, eq1 solves v1 (depends on v0), eq2 solves v2 (depends on v1)
        let eq_unknowns = vec![
            HashSet::from([0]),
            HashSet::from([0, 1]),
            HashSet::from([1, 2]),
        ];
        let eq_refs = vec![
            EquationRef::Continuous(0),
            EquationRef::Continuous(1),
            EquationRef::Continuous(2),
        ];
        let unknown_names = vec![
            UnknownId::Variable(rumoca_ir_dae::VarName::from("a")),
            UnknownId::Variable(rumoca_ir_dae::VarName::from("b")),
            UnknownId::Variable(rumoca_ir_dae::VarName::from("c")),
        ];
        let incidence = make_incidence(3, 3, eq_unknowns, eq_refs, unknown_names);

        // Perfect matching: eq0↔v0, eq1↔v1, eq2↔v2
        let match_eq = vec![Some(0), Some(1), Some(2)];

        // Dependency graph: eq1 depends on eq0 (uses v0), eq2 depends on eq1 (uses v1)
        let adj = vec![vec![], vec![0], vec![1]];

        let blocks = build_blt_blocks(&incidence, &match_eq, &adj);
        assert_eq!(blocks.len(), 3, "3 scalar blocks");
        assert!(
            blocks.iter().all(|b| matches!(b, BltBlock::Scalar { .. })),
            "all blocks should be scalar"
        );
    }

    #[test]
    fn test_blt_single_algebraic_loop() {
        // 2 equations forming a cycle: eq0↔eq1
        let eq_unknowns = vec![HashSet::from([0, 1]), HashSet::from([0, 1])];
        let eq_refs = vec![EquationRef::Continuous(0), EquationRef::Continuous(1)];
        let unknown_names = vec![
            UnknownId::Variable(rumoca_ir_dae::VarName::from("y")),
            UnknownId::Variable(rumoca_ir_dae::VarName::from("z")),
        ];
        let incidence = make_incidence(2, 2, eq_unknowns, eq_refs, unknown_names);

        let match_eq = vec![Some(0), Some(1)];
        // Cycle: eq0→eq1 (uses v1, matched to eq1), eq1→eq0 (uses v0, matched to eq0)
        let adj = vec![vec![1], vec![0]];

        let blocks = build_blt_blocks(&incidence, &match_eq, &adj);
        assert_eq!(blocks.len(), 1, "one algebraic loop block");
        match &blocks[0] {
            BltBlock::AlgebraicLoop {
                equations,
                unknowns,
            } => {
                assert_eq!(equations.len(), 2);
                assert_eq!(unknowns.len(), 2);
            }
            BltBlock::Scalar { .. } => panic!("expected algebraic loop"),
        }
    }

    #[test]
    fn test_blt_mixed() {
        // eq0: scalar (solves v0), eq1+eq2: algebraic loop (solve v1,v2)
        // eq1,eq2 depend on eq0
        let eq_unknowns = vec![
            HashSet::from([0]),
            HashSet::from([0, 1, 2]),
            HashSet::from([1, 2]),
        ];
        let eq_refs = vec![
            EquationRef::Continuous(0),
            EquationRef::Continuous(1),
            EquationRef::Continuous(2),
        ];
        let unknown_names = vec![
            UnknownId::Variable(rumoca_ir_dae::VarName::from("a")),
            UnknownId::Variable(rumoca_ir_dae::VarName::from("b")),
            UnknownId::Variable(rumoca_ir_dae::VarName::from("c")),
        ];
        let incidence = make_incidence(3, 3, eq_unknowns, eq_refs, unknown_names);

        let match_eq = vec![Some(0), Some(1), Some(2)];
        // eq1→eq0 (uses v0), eq1↔eq2 cycle (eq1 uses v2, eq2 uses v1)
        let adj = vec![vec![], vec![0, 2], vec![1]];

        let blocks = build_blt_blocks(&incidence, &match_eq, &adj);
        assert_eq!(blocks.len(), 2, "one scalar + one loop");

        // First block should be scalar (eq0 must come first)
        assert!(matches!(&blocks[0], BltBlock::Scalar { .. }));
        // Second block should be the algebraic loop
        assert!(matches!(&blocks[1], BltBlock::AlgebraicLoop { .. }));
    }
}
