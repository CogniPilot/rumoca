//! Construction-owned logical output projections of residual and JVP programs.

mod program_effects;
mod projection_application;
mod seed_domain;

#[cfg(test)]
mod tests;

use std::collections::BTreeMap;

use super::*;
use projection_application::ProjectionJacobianSource;

pub use projection_application::{ProjectionJacobianApplication, ProjectionJacobianColor};
pub use seed_domain::ProjectionJacobianSeedDomain;

/// One invocation of an existing tensor/scalar program and its selected outputs.
#[derive(Clone, Debug)]
pub struct ProjectionProgramOutputs {
    program: usize,
    output_count: usize,
    placements: Vec<(usize, usize)>,
}

impl ProjectionProgramOutputs {
    pub const fn program(&self) -> usize {
        self.program
    }

    pub const fn output_count(&self) -> usize {
        self.output_count
    }

    /// Pairs of program-local output offset and destination buffer offset.
    pub fn placements(&self) -> &[(usize, usize)] {
        &self.placements
    }
}

/// Exact ordered source-program invocations for one projection evaluation.
#[derive(Clone, Debug)]
pub struct ProjectionOutputSelection {
    output_len: usize,
    programs: Vec<ProjectionProgramOutputs>,
}

impl ProjectionOutputSelection {
    pub const fn output_len(&self) -> usize {
        self.output_len
    }

    pub fn programs(&self) -> &[ProjectionProgramOutputs] {
        &self.programs
    }
}

/// The same logical row projection in both canonical AD seed spaces.
#[derive(Clone, Debug)]
pub struct ProjectionJacobianOutputs {
    solver_y: Option<ProjectionOutputSelection>,
    solver_y_and_parameters: Option<ProjectionOutputSelection>,
}

impl ProjectionJacobianOutputs {
    pub const fn solver_y(&self) -> Option<&ProjectionOutputSelection> {
        self.solver_y.as_ref()
    }

    pub const fn solver_y_and_parameters(&self) -> Option<&ProjectionOutputSelection> {
        self.solver_y_and_parameters.as_ref()
    }
}

impl ContinuousStructuralArtifacts {
    /// Install only a specialization derived from this exact issued application.
    pub fn bind_algebraic_jacobian_application(
        &mut self,
        application: ProjectionJacobianApplication,
    ) -> Result<(), &'static str> {
        let structure = self
            .algebraic_projection
            .get_mut(application.block_index())
            .ok_or("projection specialization block is outside the issued plan")?;
        let original = structure
            .structure
            .jacobian_application
            .as_ref()
            .ok_or("projection specialization has no canonical application")?;
        if !original
            .canonical_source()
            .shares_program_owner(application.canonical_source())
            || original.rows() != application.rows()
            || original.y_indices() != application.y_indices()
        {
            return Err("projection specialization belongs to a different source or block");
        }
        structure.structure.jacobian_application = Some(Box::new(application));
        Ok(())
    }

    /// Bind the retained state constraints to their exact directional outputs.
    pub fn with_manifold_output_evaluations(
        mut self,
        plan: &AlgebraicProjectionPlan,
        directional: &ScalarProgramBlock,
    ) -> Self {
        let outputs = ProgramOutputCatalog::new(directional);
        for (structure, block) in self
            .manifold_projection
            .iter_mut()
            .zip(plan.blocks.scalar_blocks())
        {
            structure.structure.output_evaluations = color_rows(structure, block)
                .into_iter()
                .map(|rows| ProjectionJacobianOutputs {
                    solver_y: outputs.selection(&rows, block.rows.len()),
                    solver_y_and_parameters: None,
                })
                .collect();
        }
        self
    }

    /// Bind output projections before evaluator preparation. No programs are
    /// cloned or rewritten; source output identities determine every placement.
    pub fn with_algebraic_output_evaluations(
        mut self,
        plan: &AlgebraicProjectionPlan,
        primal: &ScalarProgramBlock,
        solver_y: &ScalarProgramBlock,
        full: &ScalarProgramBlock,
    ) -> Self {
        let primal_outputs = ProgramOutputCatalog::new(primal);
        let y_outputs = ProgramOutputCatalog::new(solver_y);
        let full_outputs = ProgramOutputCatalog::new(full);
        self.algebraic_jacobian_source = Some(solver_y.clone());
        let application_source = self
            .algebraic_projection
            .first()
            .and_then(|_| ProjectionJacobianSource::derive(solver_y));
        for (index, (structure, block)) in self
            .algebraic_projection
            .iter_mut()
            .zip(plan.blocks.scalar_blocks())
            .enumerate()
        {
            structure.structure.linearization_repeatable = structure.pattern().rows() as usize
                == block.rows.len()
                && structure.pattern().columns() as usize == block.y_indices.len()
                && [&primal_outputs, &y_outputs, &full_outputs]
                    .iter()
                    .all(|catalog| catalog.covers_repeatable_rows(&block.rows));
            let residual_rows = block
                .rows
                .iter()
                .copied()
                .enumerate()
                .map(|(local, source)| (source, local))
                .collect::<Vec<_>>();
            structure.structure.residual_output_evaluation =
                primal_outputs.shared_selection(&residual_rows, block.rows.len());
            structure.structure.output_evaluations =
                color_output_evaluations(structure, block, &y_outputs, &full_outputs);
            structure.structure.jacobian_application = application_source
                .as_ref()
                .and_then(|source| {
                    ProjectionJacobianApplication::derive(
                        index, structure, block, source, &y_outputs,
                    )
                })
                .map(Box::new);
            structure.structure.affine_elimination =
                AffineEliminationLayout::derive(block, structure.pattern()).map(Box::new);
        }
        self
    }
}

fn color_output_evaluations(
    structure: &ColoredJacobianStructure,
    block: &AlgebraicProjectionBlock,
    y_outputs: &ProgramOutputCatalog,
    full_outputs: &ProgramOutputCatalog,
) -> Box<[ProjectionJacobianOutputs]> {
    color_rows(structure, block)
        .into_iter()
        .map(|rows| ProjectionJacobianOutputs {
            solver_y: y_outputs.shared_selection(&rows, block.rows.len()),
            solver_y_and_parameters: full_outputs.shared_selection(&rows, block.rows.len()),
        })
        .collect()
}

fn color_rows(
    structure: &ColoredJacobianStructure,
    block: &AlgebraicProjectionBlock,
) -> Vec<Vec<(usize, usize)>> {
    if structure.pattern().rows() as usize != block.rows.len()
        || structure.pattern().columns() as usize != block.y_indices.len()
    {
        return Vec::new();
    }
    let column_rows = structure.column_rows();
    structure
        .coloring()
        .groups()
        .iter()
        .map(|group| {
            group
                .iter()
                .flat_map(|&column| column_rows[column as usize].iter().copied())
                .map(|row| (block.rows[row], row))
                .collect()
        })
        .collect()
}

type ProgramOutput = (usize, usize, usize);

/// Who produces one logical output row.
#[derive(Clone, Copy)]
enum RowOwner {
    /// No program output is bound to the row.
    Unbound,
    /// The row is bound twice, or by a program whose repetition has an
    /// effect, so no selection may reuse it.
    Unusable,
    /// Exactly one repeatable program output is bound to the row.
    Output(ProgramOutput),
}

/// The repeatable output of each logical row, indexed by the row.
struct ProgramOutputCatalog(Vec<RowOwner>);

impl ProgramOutputCatalog {
    fn output(&self, row: usize) -> Option<ProgramOutput> {
        match self.0.get(row) {
            Some(RowOwner::Output(output)) => Some(*output),
            Some(RowOwner::Unbound | RowOwner::Unusable) | None => None,
        }
    }

    fn covers_repeatable_rows(&self, rows: &[usize]) -> bool {
        rows.iter().all(|&row| self.output(row).is_some())
    }

    fn new(block: &ScalarProgramBlock) -> Self {
        let repeatable = block
            .programs()
            .iter()
            .map(|ops| program_effects::program_is_repeatable(ops))
            .collect::<Vec<_>>();
        let rows = block
            .output_bindings()
            .map(|binding| binding.logical_index + 1)
            .max()
            .unwrap_or(0);
        let mut owners = vec![RowOwner::Unbound; rows];
        for binding in block.output_bindings() {
            let owner = &mut owners[binding.logical_index];
            *owner = match owner {
                RowOwner::Unbound if repeatable[binding.program] => {
                    RowOwner::Output((binding.program, binding.offset, binding.output_count))
                }
                RowOwner::Unbound | RowOwner::Unusable | RowOwner::Output(_) => RowOwner::Unusable,
            };
        }
        Self(owners)
    }

    fn selection(
        &self,
        rows: &[(usize, usize)],
        output_len: usize,
    ) -> Option<ProjectionOutputSelection> {
        let mut positions = BTreeMap::new();
        let mut programs = Vec::<ProjectionProgramOutputs>::new();
        // Rows of one program arrive together, so the previous program's
        // position answers most lookups.
        let mut last: Option<(usize, usize)> = None;
        for &(source, target) in rows {
            let (program, offset, output_count) = self.output(source)?;
            let position = match last {
                Some((previous, position)) if previous == program => position,
                _ => *positions.entry(program).or_insert_with(|| {
                    programs.push(ProjectionProgramOutputs {
                        program,
                        output_count,
                        placements: Vec::new(),
                    });
                    programs.len() - 1
                }),
            };
            last = Some((program, position));
            programs[position].placements.push((offset, target));
        }
        Some(ProjectionOutputSelection {
            output_len,
            programs,
        })
    }

    fn shared_selection(
        &self,
        rows: &[(usize, usize)],
        output_len: usize,
    ) -> Option<ProjectionOutputSelection> {
        self.selection(rows, output_len)
            .filter(|selection| selection.programs.iter().any(|p| p.placements.len() > 1))
    }
}
