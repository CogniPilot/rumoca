use crate::ScalarProgramBlock;

pub(crate) struct ProgramOutputBinding {
    pub program: usize,
    pub offset: usize,
    pub output_count: usize,
    pub logical_index: usize,
}

impl ScalarProgramBlock {
    /// Borrow the complete output pairing proved by this block's constructor.
    pub(crate) fn output_bindings(&self) -> impl Iterator<Item = ProgramOutputBinding> + '_ {
        self.programs()
            .iter()
            .enumerate()
            .flat_map(|(program, ops)| {
                let count = Self::program_output_count(ops);
                (0..count).map(move |offset| (program, offset, count))
            })
            .zip(self.output_indices())
            .map(
                |((program, offset, output_count), &logical_index)| ProgramOutputBinding {
                    program,
                    offset,
                    output_count,
                    logical_index,
                },
            )
    }
}
