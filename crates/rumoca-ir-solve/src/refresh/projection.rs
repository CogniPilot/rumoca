use std::collections::{BTreeMap, BTreeSet};

use super::{
    AlgebraicProjectionPlan, ContinuousRefreshConstructionError, ContinuousRefreshOwners,
    RefreshPlan, RefreshRowSelection, RefreshStage, refresh_error,
};

impl ContinuousRefreshOwners {
    pub(super) fn omit_affine_projection_seeds(&mut self) {
        for plan in [
            &mut self.algebraic,
            &mut self.derivative,
            &mut self.root,
            &mut self.event,
        ]
        .into_iter()
        .chain(self.clock_events.iter_mut())
        .chain(
            self.root_after_derivative
                .iter_mut()
                .map(|relation| &mut relation.remainder),
        )
        .chain(
            self.algebraic_after_derivative
                .iter_mut()
                .map(|relation| &mut relation.remainder),
        )
        .chain(
            self.clock_events_after_event
                .iter_mut()
                .map(|relation| &mut relation.remainder),
        ) {
            omit_affine_seeds(plan, &self.projection_affinities);
        }
    }

    pub(crate) fn validate_projection_ownership(
        &self,
        canonical: &AlgebraicProjectionPlan,
        source: &super::ComputeBlock,
    ) -> Result<(), ContinuousRefreshConstructionError> {
        if !self.is_issued() {
            return Ok(());
        }
        validate_tearing_sources(canonical, source)?;
        [&self.algebraic, &self.derivative, &self.root, &self.event]
            .into_iter()
            .chain(self.clock_events.iter())
            .try_for_each(|plan| validate_projection_plan(plan, canonical))
    }

    /// Degree proof issued from the canonical algebraic block and its residuals.
    #[must_use]
    pub fn algebraic_projection_block_is_affine(&self, block_index: usize) -> bool {
        self.projection_affinities
            .get(&block_index)
            .copied()
            .unwrap_or(false)
    }
}

fn omit_affine_seeds(plan: &mut RefreshPlan, affinities: &BTreeMap<usize, bool>) {
    for stage in &mut plan.value_stages {
        if let RefreshStage::ProjectionBlock {
            block_index,
            seed_rows,
            ..
        } = stage
            && affinities.get(block_index) == Some(&true)
        {
            // The affine solve evaluates at the arithmetic origin. A scalar
            // seed cannot affect its solution and can divide by zero even
            // when the complete coupled matrix is nonsingular.
            *seed_rows = RefreshRowSelection::default();
        }
    }
}

fn validate_projection_plan(
    plan: &RefreshPlan,
    canonical: &AlgebraicProjectionPlan,
) -> Result<(), ContinuousRefreshConstructionError> {
    let mut seen = BTreeSet::new();
    for (&index, block) in plan
        .simultaneous_block_indices
        .iter()
        .zip(&plan.simultaneous_plan.blocks)
    {
        if !seen.insert(index) || canonical.blocks.get(index) != Some(block) {
            return refresh_error(
                "continuous refresh owner does not replay its canonical projection block"
                    .to_string(),
            );
        }
    }
    Ok(())
}

/// Replay pair eligibility with the same shape and dependency owners as exact assignments.
fn validate_tearing_sources(
    plan: &AlgebraicProjectionPlan,
    source: &super::ComputeBlock,
) -> Result<(), ContinuousRefreshConstructionError> {
    if !plan
        .blocks
        .iter()
        .any(|block| block.guarded_tearing.is_some())
    {
        return Ok(());
    }
    let outputs = super::source_outputs::SourceOutputs::new(source)?;
    let mut assignments = TearingSourceAssignments::new(outputs.scalar_rows(source)?);
    for block in plan
        .blocks
        .iter()
        .filter(|block| block.guarded_tearing.is_some())
    {
        if !block.has_valid_tearing_partitions() {
            return refresh_error("invalid tearing pair partition".into());
        }
        let owned = block.y_indices.iter().copied().collect::<BTreeSet<_>>();
        for (_, tearing) in block.tearing_candidates() {
            validate_candidate_steps(tearing, &mut assignments, &owned)?;
        }
    }
    Ok(())
}

fn validate_candidate_steps(
    tearing: &crate::BlockTearing,
    assignments: &mut TearingSourceAssignments<'_>,
    owned: &BTreeSet<usize>,
) -> Result<(), ContinuousRefreshConstructionError> {
    let mut known = tearing
        .tear_y_indices
        .iter()
        .copied()
        .collect::<BTreeSet<_>>();
    for step in &tearing.causal_steps {
        if assignments
            .dependencies(step)?
            .iter()
            .any(|index| owned.contains(index) && !known.contains(index))
        {
            return refresh_error("tearing step reads an unrecovered block coordinate".into());
        }
        known.insert(step.y_index);
    }
    Ok(())
}

/// Analysis lives only as long as the immutable scalar sources being checked.
struct TearingSourceAssignments<'a> {
    rows: super::source_outputs::ScalarSourceRows<'a>,
    dependencies: BTreeMap<(usize, usize), Box<[usize]>>,
    #[cfg(test)]
    derivations: usize,
}

impl<'a> TearingSourceAssignments<'a> {
    fn new(rows: super::source_outputs::ScalarSourceRows<'a>) -> Self {
        Self {
            rows,
            dependencies: BTreeMap::new(),
            #[cfg(test)]
            derivations: 0,
        }
    }

    fn dependencies(
        &mut self,
        step: &crate::CausalStep,
    ) -> Result<&[usize], ContinuousRefreshConstructionError> {
        use std::collections::btree_map::Entry;
        match self.dependencies.entry((step.row, step.y_index)) {
            Entry::Occupied(entry) => Ok(entry.into_mut()),
            Entry::Vacant(entry) => {
                #[cfg(test)]
                {
                    self.derivations += 1;
                }
                Ok(entry.insert(Self::derive_dependencies(&self.rows, step)?))
            }
        }
    }

    fn derive_dependencies(
        rows: &super::source_outputs::ScalarSourceRows<'_>,
        step: &crate::CausalStep,
    ) -> Result<Box<[usize]>, ContinuousRefreshConstructionError> {
        let Some(&(program, output)) = rows.get(&step.row) else {
            return refresh_error("tearing step has no issued scalar source output".into());
        };
        if program.iter().any(super::non_causal_assignment_operation) {
            return refresh_error("tearing step contains an effectful assignment".into());
        }
        let Some(shape) = super::assignment_shape::canonical_assignment_shape_for_output(
            program,
            output,
            step.y_index,
        ) else {
            return refresh_error("tearing step is not an exact target assignment".into());
        };
        Ok(super::dependency::assignment_y_dependencies_for_shape(
            program, &shape,
        ))
    }
}

#[cfg(test)]
mod tearing_tests {
    use super::*;
    use crate::{
        AlgebraicProjectionBlock, BinaryOp, BlockTearing, CausalStep, ComputeBlock, LinearOp,
        ScalarProgramBlock,
    };
    fn source(nonlinear: bool) -> ComputeBlock {
        let mut row0 = vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::LoadY { dst: 1, index: 2 },
        ];
        if nonlinear {
            row0.push(LinearOp::Binary {
                dst: 2,
                op: BinaryOp::Mul,
                lhs: 0,
                rhs: 0,
            });
        }
        row0.push(LinearOp::Binary {
            dst: 3,
            op: BinaryOp::Sub,
            lhs: if nonlinear { 2 } else { 0 },
            rhs: 1,
        });
        row0.push(LinearOp::StoreOutput { src: 3 });
        let row1 = vec![
            LinearOp::LoadP { dst: 0, index: 0 },
            LinearOp::LoadY { dst: 1, index: 1 },
            LinearOp::Binary {
                dst: 2,
                op: BinaryOp::Mul,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::LoadY { dst: 3, index: 2 },
            LinearOp::Binary {
                dst: 4,
                op: BinaryOp::Add,
                lhs: 2,
                rhs: 3,
            },
            LinearOp::StoreOutput { src: 4 },
        ];
        let row2 = vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::LoadY { dst: 1, index: 1 },
            LinearOp::Binary {
                dst: 2,
                op: BinaryOp::Add,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::StoreOutput { src: 2 },
        ];
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("pair.mo"),
            0,
            1,
        );
        ComputeBlock::from_scalar_program_block(
            ScalarProgramBlock::with_source_span(
                vec![row0, row1, row2],
                span.require_provenance("pair").unwrap(),
            )
            .unwrap(),
        )
    }
    fn plan() -> AlgebraicProjectionPlan {
        AlgebraicProjectionPlan {
            blocks: vec![AlgebraicProjectionBlock {
                rows: vec![0, 1, 2],
                y_indices: vec![0, 1, 2],
                alternate_charts: vec![],
                guarded_tearing: Some(BlockTearing {
                    tear_y_indices: vec![2],
                    residual_rows: vec![2],
                    causal_steps: vec![
                        CausalStep { row: 0, y_index: 0 },
                        CausalStep { row: 1, y_index: 1 },
                    ],
                }),
                tearing: Some(BlockTearing {
                    tear_y_indices: vec![1],
                    residual_rows: vec![1],
                    causal_steps: vec![
                        CausalStep { row: 2, y_index: 0 },
                        CausalStep { row: 0, y_index: 2 },
                    ],
                }),
            }],
        }
    }
    #[test]
    fn pair_source_checker_rejects_bad_order_and_ineligible_inverse() {
        let mut plan = plan();
        validate_tearing_sources(&plan, &source(false)).unwrap();
        assert!(
            validate_tearing_sources(&plan, &source(true)).is_err(),
            "same incidence cannot retain an ineligible inverse"
        );
        plan.blocks[0]
            .tearing
            .as_mut()
            .unwrap()
            .causal_steps
            .reverse();
        assert!(
            validate_tearing_sources(&plan, &source(false)).is_err(),
            "complete partition alone does not prove execution order"
        );
    }

    #[test]
    fn pair_source_analysis_is_shared_but_order_is_rechecked() {
        let source = source(false);
        let outputs = super::super::source_outputs::SourceOutputs::new(&source).unwrap();
        let mut assignments = TearingSourceAssignments::new(outputs.scalar_rows(&source).unwrap());
        let mut plan = plan();
        let block = &mut plan.blocks[0];
        let owned = block.y_indices.iter().copied().collect();
        let guarded = block.guarded_tearing.as_ref().unwrap();
        for _ in 0..2 {
            validate_candidate_steps(guarded, &mut assignments, &owned).unwrap();
        }
        assert_eq!(assignments.derivations, 2);
        let primary = block.tearing.as_mut().unwrap();
        validate_candidate_steps(primary, &mut assignments, &owned).unwrap();
        // Row zero's two different isolated targets must remain distinct.
        assert_eq!(assignments.derivations, 4);
        primary.causal_steps.reverse();
        assert!(validate_candidate_steps(primary, &mut assignments, &owned).is_err());
        assert_eq!(assignments.derivations, 4);
    }

    #[test]
    fn overlapping_pair_derives_each_shared_assignment_once() {
        let source = source(false);
        let outputs = super::super::source_outputs::SourceOutputs::new(&source).unwrap();
        let mut assignments = TearingSourceAssignments::new(outputs.scalar_rows(&source).unwrap());
        let mut plan = plan();
        let block = &mut plan.blocks[0];
        block.tearing = Some(BlockTearing {
            tear_y_indices: vec![0, 2],
            residual_rows: vec![0, 2],
            causal_steps: vec![CausalStep { row: 1, y_index: 1 }],
        });
        assert!(block.has_valid_tearing_partitions());
        let owned = block.y_indices.iter().copied().collect();
        for (_, candidate) in block.tearing_candidates() {
            validate_candidate_steps(candidate, &mut assignments, &owned).unwrap();
        }
        // Three causal visits across distinct complete plans require only two analyses.
        assert_eq!(assignments.derivations, 2);
        validate_tearing_sources(&plan, &source).unwrap();
    }
}

#[cfg(test)]
mod dependency_tests;
