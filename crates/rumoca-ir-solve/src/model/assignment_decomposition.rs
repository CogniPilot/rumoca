//! Structural authority for exact stateless assignment execution.

use super::*;

#[derive(Clone, Debug)]
pub struct CheckedAssignmentDecomposition {
    projection: crate::ProjectionBlocks,
    source_programs: Box<[ScalarProgramBlock]>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ScalarArtifactRefusal {
    CheckedAssignmentOnly,
    ForeignAssignmentOwner,
}

impl std::fmt::Display for ScalarArtifactRefusal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::CheckedAssignmentOnly => {
                "checked assignment artifacts have no scalar Newton or JVP preparation"
            }
            Self::ForeignAssignmentOwner => {
                "checked assignment artifacts belong to another source or projection owner"
            }
        })
    }
}

impl std::error::Error for ScalarArtifactRefusal {}

impl CheckedAssignmentDecomposition {
    pub(crate) fn issue(problem: &SolveProblem) -> Option<Self> {
        let source_programs = problem
            .continuous
            .implicit_rhs
            .nodes
            .iter()
            .map(|node| {
                let ComputeNode::ScalarPrograms(programs) = node else {
                    return None;
                };
                Some(programs.clone())
            })
            .collect::<Option<Box<[_]>>>()?;
        Some(Self {
            projection: problem.continuous.algebraic_projection_plan.blocks.clone(),
            source_programs,
        })
    }

    /// The projection blocks this decomposition isolates.
    pub fn projection(&self) -> &crate::ProjectionBlocks {
        &self.projection
    }

    pub fn check_source(&self, problem: &SolveProblem) -> Result<(), ScalarArtifactRefusal> {
        if problem.solve_layout.state_scalar_count() != 0
            || !self.projection.shares_stage_owner(&problem.continuous.algebraic_projection_plan.blocks)
            || self.source_programs.len() != problem.continuous.implicit_rhs.nodes.len()
            || !self.source_programs.iter().zip(&problem.continuous.implicit_rhs.nodes).all(|(owner, node)| {
                matches!(node, ComputeNode::ScalarPrograms(source) if owner.shares_program_owner(source))
            })
        {
            return Err(ScalarArtifactRefusal::ForeignAssignmentOwner);
        }
        Ok(())
    }
}

impl ContinuousStructuralArtifacts {
    pub fn checked_assignment_for_problem(
        problem: &SolveProblem,
    ) -> Result<Option<Self>, &'static str> {
        let capability = problem
            .continuous
            .refresh_owners
            .checked_assignment_artifact(problem)?;
        Ok(capability.map(Self::from_assignment))
    }

    /// Reconstruct assignment artifacts with the actual finalized call table.
    pub fn checked_assignment_with_calls(
        problem: &SolveProblem,
        calls: &crate::SolvePureCallTable,
    ) -> Result<Option<Self>, &'static str> {
        let capability = problem
            .continuous
            .refresh_owners
            .checked_assignment_artifact_with_calls(problem, calls)?;
        Ok(capability.map(Self::from_assignment))
    }

    fn from_assignment(capability: CheckedAssignmentDecomposition) -> Self {
        Self {
            checked_assignment: Some(capability),
            ..Self::default()
        }
    }

    pub fn checked_assignment(&self) -> Option<&CheckedAssignmentDecomposition> {
        self.checked_assignment.as_ref()
    }

    pub fn require_scalar_derivatives(&self) -> Result<(), ScalarArtifactRefusal> {
        if self.checked_assignment.is_some() {
            return Err(ScalarArtifactRefusal::CheckedAssignmentOnly);
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::AlgebraicProjectionBlock;

    fn rows() -> ScalarProgramBlock {
        ScalarProgramBlock::with_source_span(
            vec![vec![
                LinearOp::Const { dst: 0, value: 1.0 },
                LinearOp::StoreOutput { src: 0 },
            ]],
            rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name("assignment_decomposition.mo"),
                0,
                1,
            )
            .require_provenance("assignment decomposition fixture")
            .unwrap(),
        )
        .unwrap()
    }

    fn problem() -> SolveProblem {
        let mut problem = SolveProblem::default();
        problem.continuous.implicit_rhs = ComputeBlock {
            nodes: vec![ComputeNode::ScalarPrograms(rows())],
        };
        problem.continuous.algebraic_projection_plan.blocks =
            crate::ProjectionBlocks::from(vec![AlgebraicProjectionBlock {
                rows: vec![0],
                y_indices: vec![0],
                ..Default::default()
            }]);
        problem
    }

    #[test]
    fn the_decomposition_exposes_the_projection_stages_it_isolates() {
        let problem = problem();
        let decomposition = CheckedAssignmentDecomposition::issue(&problem).unwrap();
        decomposition.check_source(&problem).unwrap();
        let blocks = &problem.continuous.algebraic_projection_plan.blocks;
        assert!(decomposition.projection().shares_stage_owner(blocks));
        assert_eq!(decomposition.projection().len(), 1);
        assert_eq!(decomposition.projection().stages().len(), 1);
    }

    #[test]
    fn a_decomposition_refuses_a_problem_with_another_projection_owner() {
        let decomposition = CheckedAssignmentDecomposition::issue(&problem()).unwrap();
        assert_eq!(
            decomposition.check_source(&problem()),
            Err(ScalarArtifactRefusal::ForeignAssignmentOwner)
        );
    }
}
