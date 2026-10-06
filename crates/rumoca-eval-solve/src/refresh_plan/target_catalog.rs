use std::{cell::RefCell, collections::BTreeMap};

use super::{OutputRowPosition, output_row_positions};
use crate::EvalSolveError;
use indexmap::IndexMap;
use rumoca_ir_solve::ScalarProgramBlock;

pub(crate) trait ExactAssignmentAccess {
    fn block(&self) -> &ScalarProgramBlock;
    fn row_output_position(&self, row: usize) -> Option<(usize, usize)>;
    fn is_exact_assignment(&self, row: usize, target: usize) -> bool;

    fn coefficient_proof(&self, row: usize, target: usize) -> rumoca_ir_solve::CausalCoefficient {
        self.row_output_position(row)
            .and_then(|(program, output)| Some((self.block().program(program)?, output)))
            .map_or(
                rumoca_ir_solve::CausalCoefficient::Unproven,
                |(ops, output)| rumoca_ir_solve::isolator_coefficient_proof(ops, output, target),
            )
    }
}

/// Exact assignment queries bound to one checked scalar projection owner.
pub(super) struct TargetAssignmentCatalog<'a> {
    block: &'a ScalarProgramBlock,
    positions: IndexMap<usize, OutputRowPosition>,
    exact: RefCell<BTreeMap<(usize, usize), bool>>,
}

impl<'a> TargetAssignmentCatalog<'a> {
    pub(super) fn new(block: &'a ScalarProgramBlock) -> Result<Self, EvalSolveError> {
        Ok(Self {
            block,
            positions: output_row_positions(block)?,
            exact: RefCell::new(BTreeMap::new()),
        })
    }
}

impl ExactAssignmentAccess for TargetAssignmentCatalog<'_> {
    fn block(&self) -> &ScalarProgramBlock {
        self.block
    }

    fn row_output_position(&self, row: usize) -> Option<(usize, usize)> {
        self.positions
            .get(&row)
            .map(|position| (position.program_index, position.output_offset))
    }

    fn is_exact_assignment(&self, row: usize, target: usize) -> bool {
        if let Some(exact) = self.exact.borrow().get(&(row, target)) {
            return *exact;
        }
        let exact = self.positions.get(&row).is_some_and(|position| {
            self.block
                .programs()
                .get(position.program_index)
                .is_some_and(|program| {
                    crate::prepared::program_certifies_exact_target(
                        program,
                        position.output_offset,
                        target,
                    )
                    .unwrap_or(false)
                })
        });
        self.exact.borrow_mut().insert((row, target), exact);
        exact
    }
}

impl ExactAssignmentAccess for crate::PreparedScalarProgramBlock {
    fn block(&self) -> &ScalarProgramBlock {
        crate::PreparedScalarProgramBlock::block(self)
    }

    fn row_output_position(&self, row: usize) -> Option<(usize, usize)> {
        crate::PreparedScalarProgramBlock::row_output_position(self, row)
    }

    fn is_exact_assignment(&self, row: usize, target: usize) -> bool {
        self.row_output_position(row)
            .is_some_and(|(program, output)| {
                self.certifies_exact_target_assignment_output(program, output, target)
            })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_ir_solve::{BinaryOp, LinearOp};

    #[test]
    fn targeted_catalog_preserves_logical_output_identity_and_prefix_versions() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("target_catalog.mo"),
            0,
            1,
        );
        let source = ScalarProgramBlock::with_output_indices(
            vec![vec![
                LinearOp::LoadY { dst: 0, index: 0 },
                LinearOp::LoadY { dst: 1, index: 1 },
                LinearOp::Binary {
                    dst: 2,
                    op: BinaryOp::Sub,
                    lhs: 0,
                    rhs: 1,
                },
                LinearOp::StoreOutput { src: 2 },
                LinearOp::Const {
                    dst: 0,
                    value: -0.0,
                },
                LinearOp::StoreOutput { src: 2 },
            ]],
            vec![span],
            vec![7, 11],
        )
        .unwrap();
        let catalog = TargetAssignmentCatalog::new(&source).unwrap();
        let exhaustive = crate::PreparedScalarProgramBlock::new(source.clone()).unwrap();
        for row in [7, 11, 0] {
            for target in [0, 1, 2] {
                assert_eq!(
                    catalog.is_exact_assignment(row, target),
                    exhaustive.is_exact_assignment(row, target)
                );
            }
        }
        assert!(catalog.is_exact_assignment(7, 0));
        assert!(!catalog.is_exact_assignment(11, 0));
        assert_eq!(catalog.exact.borrow().len(), 9);
    }

    #[test]
    fn targeted_catalog_keeps_upstream_coefficient_proofs_and_zero_refusal() {
        use rumoca_ir_solve::{BlockTearing, CausalCoefficient, CausalStep};
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("target_catalog_coefficient.mo"),
            0,
            1,
        );
        for coefficient in [0.0, -0.0, 2.0, -2.0] {
            let source = ScalarProgramBlock::with_output_indices(
                vec![vec![
                    LinearOp::Const {
                        dst: 0,
                        value: coefficient,
                    },
                    LinearOp::LoadY { dst: 1, index: 1 },
                    LinearOp::LoadY { dst: 2, index: 2 },
                    LinearOp::Binary {
                        dst: 3,
                        op: BinaryOp::Mul,
                        lhs: 0,
                        rhs: 1,
                    },
                    LinearOp::Binary {
                        dst: 4,
                        op: BinaryOp::Sub,
                        lhs: 3,
                        rhs: 2,
                    },
                    LinearOp::StoreOutput { src: 4 },
                ]],
                vec![span],
                vec![17],
            )
            .unwrap();
            let catalog = TargetAssignmentCatalog::new(&source).unwrap();
            assert_eq!(catalog.row_output_position(17), Some((0, 0)));
            assert_eq!(catalog.row_output_position(0), None);
            let mut tearing = BlockTearing {
                tear_y_indices: vec![2],
                residual_rows: vec![],
                causal_steps: vec![CausalStep {
                    row: 17,
                    y_index: 1,
                    ..Default::default()
                }],
            };
            let result = super::super::promote_inexact_causal_steps(&mut tearing, &catalog);
            if coefficient == 0.0 {
                assert_eq!(
                    catalog.coefficient_proof(17, 1),
                    CausalCoefficient::Unproven
                );
                let error = result.expect_err("zero coefficient cannot isolate a causal unknown");
                assert!(error.to_string().contains("solver slot 1"));
                assert!(error.to_string().contains("proven zero"));
            } else {
                result.unwrap();
                assert_eq!(tearing.causal_steps.len(), 1);
                assert_eq!(
                    tearing.causal_steps[0].coefficient,
                    CausalCoefficient::Literal
                );
                assert_eq!(tearing.tear_y_indices, [2]);
            }
        }
    }
}
