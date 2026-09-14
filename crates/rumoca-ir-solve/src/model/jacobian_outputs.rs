//! Construction-owned logical output projections of residual and JVP programs.

mod program_effects;

#[cfg(test)]
mod tests;

use std::collections::BTreeMap;

use super::*;

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

    /// Pairs of program-local output offset and projection-local matrix row.
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
    /// Bind the retained state constraints to their exact directional outputs.
    pub fn with_manifold_output_evaluations(
        mut self,
        plan: &AlgebraicProjectionPlan,
        directional: &ScalarProgramBlock,
    ) -> Self {
        let outputs = ProgramOutputCatalog::new(directional);
        for (structure, block) in self.manifold_projection.iter_mut().zip(&plan.blocks) {
            structure.output_evaluations = color_rows(structure, block)
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
        for (structure, block) in self.algebraic_projection.iter_mut().zip(&plan.blocks) {
            structure.linearization_repeatable = structure.pattern.rows() as usize
                == block.rows.len()
                && structure.pattern.columns() as usize == block.y_indices.len()
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
            structure.residual_output_evaluation =
                primal_outputs.shared_selection(&residual_rows, block.rows.len());
            structure.output_evaluations =
                color_output_evaluations(structure, block, &y_outputs, &full_outputs);
        }
        self
    }
}

fn color_output_evaluations(
    structure: &JacobianStructure,
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
    structure: &JacobianStructure,
    block: &AlgebraicProjectionBlock,
) -> Vec<Vec<(usize, usize)>> {
    if structure.pattern.rows() as usize != block.rows.len()
        || structure.pattern.columns() as usize != block.y_indices.len()
    {
        return Vec::new();
    }
    let column_rows = structure.pattern.column_rows();
    structure
        .coloring
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

struct ProgramOutputCatalog(BTreeMap<usize, Option<ProgramOutput>>);

impl ProgramOutputCatalog {
    fn covers_repeatable_rows(&self, rows: &[usize]) -> bool {
        rows.iter()
            .all(|row| self.0.get(row).is_some_and(Option::is_some))
    }

    fn new(block: &ScalarProgramBlock) -> Self {
        let mut outputs = BTreeMap::new();
        let repeatable = block
            .programs()
            .iter()
            .map(|ops| program_effects::program_is_repeatable(ops))
            .collect::<Vec<_>>();
        for binding in block.output_bindings() {
            outputs
                .entry(binding.logical_index)
                .and_modify(|owner| *owner = None)
                .or_insert(repeatable[binding.program].then_some((
                    binding.program,
                    binding.offset,
                    binding.output_count,
                )));
        }
        Self(outputs)
    }

    fn selection(
        &self,
        rows: &[(usize, usize)],
        output_len: usize,
    ) -> Option<ProjectionOutputSelection> {
        let mut positions = BTreeMap::new();
        let mut programs = Vec::<ProjectionProgramOutputs>::new();
        for &(source, target) in rows {
            let &(program, offset, output_count) = self.0.get(&source)?.as_ref()?;
            let position = *positions.entry(program).or_insert_with(|| {
                programs.push(ProjectionProgramOutputs {
                    program,
                    output_count,
                    placements: Vec::new(),
                });
                programs.len() - 1
            });
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
