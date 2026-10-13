//! Source-bound reuse of the canonical dependency walk across exact prefixes.

use std::cell::OnceCell;

use super::ScalarProgramYDependency;
use crate::refresh::assignment_shape::producers::UniqueProgram;
use crate::{IndexIntervals, LinearOp, Reg};

#[cfg(test)]
mod tests;

/// Owns one complete register inventory when unique writers prove prefix
/// stability, otherwise one exact prefix inventory. Nothing outlives `source`.
pub struct ScalarProgramYDependencyQueries<'source> {
    source: &'source [LinearOp],
    complete: OnceCell<Option<CompleteAnalysis<'source>>>,
    prefix: Option<(usize, ScalarProgramYDependency<'source>)>,
    #[cfg(test)]
    complete_builds: usize,
    #[cfg(test)]
    prefix_builds: usize,
}

struct CompleteAnalysis<'source> {
    producers: UniqueProgram<'source>,
    dependencies: ScalarProgramYDependency<'source>,
}

impl<'source> ScalarProgramYDependencyQueries<'source> {
    pub fn new(source: &'source [LinearOp]) -> Self {
        Self {
            source,
            complete: OnceCell::new(),
            prefix: None,
            #[cfg(test)]
            complete_builds: 0,
            #[cfg(test)]
            prefix_builds: 0,
        }
    }

    /// Internal inventory for assignment queries whose register definitions
    /// are checked by their exact producer-prefix view.
    pub(in crate::refresh) fn prefix(
        &mut self,
        len: usize,
    ) -> Option<&ScalarProgramYDependency<'source>> {
        let source = self.source.get(..len)?;
        #[cfg(test)]
        if self.complete.get().is_none() {
            self.complete_builds += 1;
        }
        self.complete.get_or_init(|| {
            Some(CompleteAnalysis {
                producers: UniqueProgram::new(self.source)?,
                dependencies: ScalarProgramYDependency::complete(self.source)?,
            })
        });
        if self.complete.get()?.is_some() {
            return self
                .complete
                .get()?
                .as_ref()
                .map(|analysis| &analysis.dependencies);
        }
        if self.prefix.as_ref().map(|(position, _)| *position) != Some(len) {
            self.prefix = None;
            self.prefix = Some((len, ScalarProgramYDependency::new(source)));
            #[cfg(test)]
            {
                self.prefix_builds += 1;
            }
        }
        self.prefix.as_ref().map(|(_, dependencies)| dependencies)
    }

    /// The exact prefix footprint, refusing missing or not-yet-written cells.
    pub fn footprint(
        &mut self,
        len: usize,
        registers: impl IntoIterator<Item = Reg>,
    ) -> Option<IndexIntervals> {
        self.prefix(len)?;
        if let Some(analysis) = self.complete.get()?.as_ref() {
            let prefix = analysis.producers.view().before(len)?;
            return analysis
                .dependencies
                .footprint_checked(registers, |register| {
                    prefix.producer_position(register).is_some()
                });
        }
        self.prefix.as_ref()?.1.footprint(registers)
    }

    /// Exact range view; prefix membership comes from the same producer proof.
    pub fn footprint_ranges(
        &mut self,
        prefix_len: usize,
        ranges: impl IntoIterator<Item = (Reg, usize)>,
    ) -> Option<IndexIntervals> {
        let _ = self.prefix(prefix_len)?;
        if let Some(complete) = self.complete.get().and_then(Option::as_ref) {
            let prefix = complete.producers.view().before(prefix_len)?;
            return complete
                .dependencies
                .footprint_ranges_checked(ranges, |start, count| {
                    prefix.contains_range(start, count)
                });
        }
        self.prefix.as_ref()?.1.footprint_ranges(ranges)
    }
}
