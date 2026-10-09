//! Reads of a loop-carried value on the first pass of a loop.
//!
//! MLS §12.4.4 gives a function local no implicit start value. At the head of
//! a loop the value is the join of the value before the loop and the value the
//! previous pass left, so a value no statement defined before the loop has a
//! definition at a read of the first pass only when an earlier pass of the
//! loop defined it on every path and the path of the read excludes the first
//! pass.

use super::*;

impl FunctionDefinitions {
    /// Reject a read of a declared value that nothing defines on the first
    /// pass of an enclosing loop: no definition reaches the read in this
    /// state, and no earlier pass certainly defined it at every binder value
    /// the path of the read reaches.
    pub(super) fn require_defined_at_loop_entry(
        &self,
        name: &VarName,
        folds: &FoldScopes,
        context: FunctionValidationContext<'_>,
        span: Span,
    ) -> Result<(), ToDaeError> {
        let undefined = folds.is_active()
            && declared_value(name, context).is_some()
            && context
                .shapes
                .get(name)
                .is_some_and(|dimensions| dimensions.is_empty())
            && !self.values.contains_key(name)
            && !self.branch_only.contains_key(name)
            && !folds.defined_by_earlier_iteration_whole(name);
        if !undefined {
            return Ok(());
        }
        Err(ToDaeError::unsupported_flat(
            "function loop definition",
            format!(
                "`{}` reads `{name}` on the first pass of a loop, before any statement defines it",
                context.function.name
            ),
            span,
        ))
    }
}
