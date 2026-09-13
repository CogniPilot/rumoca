//! Construction-owned logical output projections of shared JVP programs.

use std::collections::BTreeMap;

use super::*;
mod program_effects;

#[cfg(test)]
mod tests;

/// One invocation of an existing tensor/scalar program and its selected outputs.
#[derive(Clone, Debug)]
pub struct JacobianProgramOutputs {
    program: usize,
    output_count: usize,
    placements: Vec<(usize, usize)>,
}

impl JacobianProgramOutputs {
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

/// Exact ordered source-program invocations for one projection color.
#[derive(Clone, Debug)]
pub struct JacobianOutputSelection {
    output_len: usize,
    programs: Vec<JacobianProgramOutputs>,
}

impl JacobianOutputSelection {
    pub const fn output_len(&self) -> usize {
        self.output_len
    }

    pub fn programs(&self) -> &[JacobianProgramOutputs] {
        &self.programs
    }
}

/// The same logical row projection in both canonical AD seed spaces.
#[derive(Clone, Debug)]
pub struct ProjectionJacobianOutputs {
    solver_y: Option<JacobianOutputSelection>,
    solver_y_and_parameters: Option<JacobianOutputSelection>,
}

impl ProjectionJacobianOutputs {
    pub const fn solver_y(&self) -> Option<&JacobianOutputSelection> {
        self.solver_y.as_ref()
    }

    pub const fn solver_y_and_parameters(&self) -> Option<&JacobianOutputSelection> {
        self.solver_y_and_parameters.as_ref()
    }
}

impl ContinuousStructuralArtifacts {
    /// Bind output projections before evaluator preparation. No programs are
    /// cloned or rewritten; source output identities determine every placement.
    pub fn with_algebraic_output_evaluations(
        mut self,
        plan: &AlgebraicProjectionPlan,
        solver_y: &ScalarProgramBlock,
        full: &ScalarProgramBlock,
    ) -> Self {
        let y_outputs = ProgramOutputCatalog::new(solver_y);
        let full_outputs = ProgramOutputCatalog::new(full);
        for (structure, block) in self.algebraic_projection.iter_mut().zip(&plan.blocks) {
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
    if structure.pattern.rows() as usize != block.rows.len()
        || structure.pattern.columns() as usize != block.y_indices.len()
    {
        return Box::default();
    }
    let column_rows = structure.pattern.column_rows();
    structure
        .coloring
        .groups()
        .iter()
        .map(|group| {
            let rows = group
                .iter()
                .flat_map(|&column| column_rows[column as usize].iter().copied())
                .map(|row| (block.rows[row], row))
                .collect::<Vec<_>>();
            ProjectionJacobianOutputs {
                solver_y: y_outputs.selection(&rows, block.rows.len()),
                solver_y_and_parameters: full_outputs.selection(&rows, block.rows.len()),
            }
        })
        .collect()
}

type ProgramOutput = (usize, usize, usize);

struct ProgramOutputCatalog(BTreeMap<usize, Option<ProgramOutput>>);

impl ProgramOutputCatalog {
    fn new(block: &ScalarProgramBlock) -> Self {
        let mut outputs = BTreeMap::new();
        let mut indices = block.output_indices().iter();
        for (program, ops) in block.programs().iter().enumerate() {
            let count = ScalarProgramBlock::program_output_count(ops);
            let repeatable = program_effects::program_is_repeatable(ops);
            for offset in 0..count {
                let &index = indices
                    .next()
                    .expect("checked program output count matches its output catalog");
                outputs
                    .entry(index)
                    .and_modify(|owner| *owner = None)
                    .or_insert(repeatable.then_some((program, offset, count)));
            }
        }
        Self(outputs)
    }

    fn selection(
        &self,
        rows: &[(usize, usize)],
        output_len: usize,
    ) -> Option<JacobianOutputSelection> {
        let mut positions = BTreeMap::new();
        let mut programs = Vec::<JacobianProgramOutputs>::new();
        for &(source, target) in rows {
            let &(program, offset, output_count) = self.0.get(&source)?.as_ref()?;
            let position = *positions.entry(program).or_insert_with(|| {
                programs.push(JacobianProgramOutputs {
                    program,
                    output_count,
                    placements: Vec::new(),
                });
                programs.len() - 1
            });
            programs[position].placements.push((offset, target));
        }
        // A singleton output already has the direct selected-row entry point.
        programs
            .iter()
            .any(|p| p.placements.len() > 1)
            .then_some(JacobianOutputSelection {
                output_len,
                programs,
            })
    }
}
