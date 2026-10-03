//! `for` statements of an initial algorithm, unrolled over their ranges.
//!
//! An initial algorithm runs once, so a `for` whose range is fixed at
//! translation (see `fixed_loops`) is exactly its unrolled sequence, replayed
//! in order through the section's ordinary grammar, so an indexed target such
//! as `y[i]` becomes the whole coordinate `y[2]`.

use super::*;
use rumoca_core::StatementRewriter;

impl Replay<'_> {
    pub(super) fn unrolled(
        &mut self,
        indices: &[rumoca_core::ForIndex],
        body: &[rumoca_core::Statement],
        span: Span,
        guard: Option<&Expression>,
        values: &mut ReplayValues,
    ) -> Result<(), ToDaeError> {
        let Some((index, rest)) = indices.split_first() else {
            return self.statements(body, guard, values);
        };
        let range = fixed_range(self.flat, self.shapes, &index.range, "initial", span)?;
        for value in range_values(range) {
            let mut binding = IndexBinding {
                name: &index.ident,
                value,
                span,
            };
            let rest = binding.rewrite_for_indices(rest);
            let body = binding.rewrite_statements(body);
            self.unrolled(&rest, &body, span, guard, values)?;
        }
        Ok(())
    }
}
