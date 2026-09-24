use super::*;
use std::sync::Arc;

#[cfg(test)]
thread_local! {
    pub(super) static INVARIANCE_PROOF_OPERATIONS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

pub(super) struct ProjectionJacobianSource<'source> {
    source: &'source ScalarProgramBlock,
    invariant_operations: Arc<[Box<[bool]>]>,
}

impl<'source> ProjectionJacobianSource<'source> {
    pub(super) fn derive(source: &'source ScalarProgramBlock) -> Option<Self> {
        Some(Self {
            source,
            invariant_operations: derive_invariant_operations(source)?.into(),
        })
    }
}

/// Complete colored forward application bound to one immutable source owner.
#[derive(Clone, Debug)]
pub struct ProjectionJacobianApplication {
    identity: Arc<()>,
    block_index: usize,
    rows: Box<[usize]>,
    y_indices: Box<[usize]>,
    value_layout: Arc<JacobianValueLayout>,
    source: ScalarProgramBlock,
    canonical_source: ScalarProgramBlock,
    primal_source: Option<ScalarProgramBlock>,
    colors: Box<[ProjectionJacobianColor]>,
    invariant_operations: Arc<[Box<[bool]>]>,
}

#[derive(Clone, Debug)]
pub struct ProjectionJacobianColor {
    seed_indices: Box<[usize]>,
    outputs: ProjectionOutputSelection,
}

impl ProjectionJacobianColor {
    pub fn seed_indices(&self) -> &[usize] {
        &self.seed_indices
    }

    pub const fn outputs(&self) -> &ProjectionOutputSelection {
        &self.outputs
    }
}

impl ProjectionJacobianApplication {
    pub(super) fn derive(
        block_index: usize,
        structure: &JacobianStructure,
        block: &AlgebraicProjectionBlock,
        source: &ProjectionJacobianSource<'_>,
        outputs: &ProgramOutputCatalog,
    ) -> Option<Self> {
        if structure.pattern.rows() as usize != block.rows.len()
            || structure.pattern.columns() as usize != block.y_indices.len()
        {
            return None;
        }
        let value_layout = Arc::clone(structure.value_layout()?);
        let output_len = value_layout.len();
        let column_rows = structure.pattern.column_rows();
        let colors = structure
            .coloring
            .groups()
            .iter()
            .map(|group| {
                let placements = color_placements(group, &column_rows, block, &value_layout)?;
                Some(ProjectionJacobianColor {
                    seed_indices: group
                        .iter()
                        .map(|&column| block.y_indices[column as usize])
                        .collect(),
                    outputs: outputs.selection(&placements, output_len)?,
                })
            })
            .collect::<Option<Box<[_]>>>()?;
        complete_writers(&colors, output_len)?;
        Some(Self {
            identity: Arc::new(()),
            block_index,
            rows: block.rows.clone().into_boxed_slice(),
            y_indices: block.y_indices.clone().into_boxed_slice(),
            value_layout,
            source: source.source.clone(),
            canonical_source: source.source.clone(),
            primal_source: None,
            colors,
            invariant_operations: Arc::clone(&source.invariant_operations),
        })
    }

    pub const fn canonical_source(&self) -> &ScalarProgramBlock {
        &self.canonical_source
    }
    pub const fn primal_source(&self) -> Option<&ScalarProgramBlock> {
        self.primal_source.as_ref()
    }

    pub(super) fn with_specialized_source(
        mut self,
        primal: &ScalarProgramBlock,
        source: ScalarProgramBlock,
    ) -> Option<Self> {
        let catalog = ProgramOutputCatalog::new(&source);
        let colors = self
            .colors
            .iter()
            .map(|color| {
                let placements = color
                    .outputs()
                    .programs()
                    .iter()
                    .flat_map(|program| program.placements().iter())
                    .map(|&(_, target)| {
                        let (row, _) = self.value_layout.coordinate(target)?;
                        Some((*self.rows.get(row)?, target))
                    })
                    .collect::<Option<Vec<_>>>()?;
                Some(ProjectionJacobianColor {
                    seed_indices: color.seed_indices.clone(),
                    outputs: catalog.selection(&placements, self.output_len())?,
                })
            })
            .collect::<Option<Box<[_]>>>()?;
        complete_writers(&colors, self.output_len())?;
        self.colors = colors;
        self.identity = Arc::new(());
        self.invariant_operations = derive_invariant_operations(&source)?.into();
        self.source = source;
        self.primal_source = Some(primal.clone());
        Some(self)
    }

    pub const fn block_index(&self) -> usize {
        self.block_index
    }
    pub fn rows(&self) -> &[usize] {
        &self.rows
    }
    pub fn y_indices(&self) -> &[usize] {
        &self.y_indices
    }
    pub fn output_len(&self) -> usize {
        self.value_layout.len()
    }
    pub fn value_layout(&self) -> &Arc<JacobianValueLayout> {
        &self.value_layout
    }
    pub fn identity(&self) -> &Arc<()> {
        &self.identity
    }
    pub const fn source(&self) -> &ScalarProgramBlock {
        &self.source
    }
    pub fn colors(&self) -> &[ProjectionJacobianColor] {
        &self.colors
    }

    /// Complete source operations whose inputs and effects are invariant across
    /// this application's colors within one call at fixed coordinates.
    pub fn invariant_operations(&self, program: usize) -> &[bool] {
        &self.invariant_operations[program]
    }
}

fn complete_writers(colors: &[ProjectionJacobianColor], output_len: usize) -> Option<()> {
    let mut writers = vec![false; output_len];
    let slots = colors
        .iter()
        .flat_map(|color| color.outputs.programs())
        .flat_map(|program| program.placements().iter().map(|&(_, slot)| slot));
    for slot in slots {
        if std::mem::replace(writers.get_mut(slot)?, true) {
            return None;
        }
    }
    writers.iter().all(|written| *written).then_some(())
}

fn derive_invariant_operations(source: &ScalarProgramBlock) -> Option<Box<[Box<[bool]>]>> {
    #[cfg(test)]
    INVARIANCE_PROOF_OPERATIONS.with(|count| {
        count.set(count.get() + source.programs().iter().map(Vec::len).sum::<usize>());
    });
    source
        .programs()
        .iter()
        .map(|program| {
            if program_effects::program_is_repeatable(program) {
                crate::ScalarProgramRegisterFlow::seed_invariant_operations(program)
            } else {
                Some(vec![false; program.len()].into_boxed_slice())
            }
        })
        .collect()
}

fn color_placements(
    group: &[u32],
    column_rows: &[Vec<usize>],
    block: &AlgebraicProjectionBlock,
    layout: &JacobianValueLayout,
) -> Option<Vec<(usize, usize)>> {
    group
        .iter()
        .flat_map(|&column| {
            column_rows[column as usize]
                .iter()
                .map(move |&row| (row, column as usize))
        })
        .map(|(row, column)| Some((block.rows[row], layout.locate(row, column)??)))
        .collect()
}
