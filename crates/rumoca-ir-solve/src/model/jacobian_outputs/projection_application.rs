use super::*;

/// Complete colored forward application bound to one immutable source owner.
#[derive(Clone, Debug)]
pub struct ProjectionJacobianApplication {
    block_index: usize,
    rows: Box<[usize]>,
    y_indices: Box<[usize]>,
    output_len: usize,
    source: ScalarProgramBlock,
    colors: Box<[ProjectionJacobianColor]>,
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
        source: &ScalarProgramBlock,
        outputs: &ProgramOutputCatalog,
    ) -> Option<Self> {
        if structure.pattern.rows() as usize != block.rows.len()
            || structure.pattern.columns() as usize != block.y_indices.len()
        {
            return None;
        }
        let output_len = block.rows.len().checked_mul(block.y_indices.len())?;
        let column_rows = structure.pattern.column_rows();
        let colors = structure
            .coloring
            .groups()
            .iter()
            .map(|group| {
                let placements = color_placements(group, &column_rows, block);
                Some(ProjectionJacobianColor {
                    seed_indices: group
                        .iter()
                        .map(|&column| block.y_indices[column as usize])
                        .collect(),
                    outputs: outputs.selection(&placements, output_len)?,
                })
            })
            .collect::<Option<Box<[_]>>>()?;
        Some(Self {
            block_index,
            rows: block.rows.clone().into_boxed_slice(),
            y_indices: block.y_indices.clone().into_boxed_slice(),
            output_len,
            source: source.clone(),
            colors,
        })
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
    pub const fn output_len(&self) -> usize {
        self.output_len
    }
    pub const fn source(&self) -> &ScalarProgramBlock {
        &self.source
    }
    pub fn colors(&self) -> &[ProjectionJacobianColor] {
        &self.colors
    }
}

fn color_placements(
    group: &[u32],
    column_rows: &[Vec<usize>],
    block: &AlgebraicProjectionBlock,
) -> Vec<(usize, usize)> {
    group
        .iter()
        .flat_map(|&column| {
            column_rows[column as usize]
                .iter()
                .map(move |&row| (block.rows[row], column as usize * block.rows.len() + row))
        })
        .collect()
}
