//! Canonical output-read proofs retained for one immutable source program.
#[cfg(test)]
mod tests;

use std::cell::OnceCell;
use std::sync::Arc;

use rumoca_ir_solve as solve;

pub(super) struct OutputDependencies<'source> {
    source: &'source [solve::LinearOp],
    complete: OnceCell<Option<Vec<Arc<solve::IndexIntervals>>>>,
    stores: Option<solve::ScalarProgramOutputStores>,
    prefix: Option<(usize, solve::ScalarProgramYDependency<'source>)>,
    #[cfg(test)]
    complete_builds: usize,
    #[cfg(test)]
    prefix_builds: usize,
}

impl<'source> OutputDependencies<'source> {
    pub(super) fn new(source: &'source [solve::LinearOp]) -> Self {
        Self {
            source,
            complete: OnceCell::new(),
            stores: solve::ScalarProgramOutputStores::new(source),
            prefix: None,
            #[cfg(test)]
            complete_builds: 0,
            #[cfg(test)]
            prefix_builds: 0,
        }
    }

    pub(super) fn depends_on(&mut self, output: usize, target: usize) -> bool {
        #[cfg(test)]
        if self.complete.get().is_none() {
            self.complete_builds += 1;
        }
        let complete = self.complete.get_or_init(|| {
            solve::StructuralPattern::derive_output_y_dependency_ranges(self.source, None).ok()
        });
        if let Some(reads) = complete {
            return reads.get(output).is_none_or(|reads| reads.contains(target));
        }
        self.prefix_depends_on(output, target)
    }

    // A refused suffix cannot invalidate an earlier exact store-prefix proof.
    // Keep only one fallback prefix so scalar stores cannot retain quadratic
    // inventories. The complete canonical proof answers all normal outputs.
    fn prefix_depends_on(&mut self, output: usize, target: usize) -> bool {
        let Some(stores) = &self.stores else {
            return match solve::output_y_reads(self.source, output) {
                solve::OutputYReads::Bounded(reads) => reads.contains(target),
                solve::OutputYReads::Absent | solve::OutputYReads::Unbounded => true,
            };
        };
        let Some((register, position)) = stores.output(output) else {
            return true;
        };
        if self.prefix.as_ref().map(|(position, _)| *position) != Some(position) {
            self.prefix = None;
            let Some(prefix) = self.source.get(..position) else {
                return true;
            };
            self.prefix = Some((position, solve::ScalarProgramYDependency::new(prefix)));
            #[cfg(test)]
            {
                self.prefix_builds += 1;
            }
        }
        self.prefix
            .as_ref()
            .is_none_or(|(_, proof)| proof.depends_on(register, target))
    }
}
