//! Exact canonical prefix analysis shared only by adjacent queries on one source.
#[cfg(test)]
mod tests;

use super::{canonical_assignment_shape, producers::UniqueProgram};
use crate::refresh::dependency::ScalarProgramYDependency;
use crate::{LinearOp, Reg, TargetAssignmentShape};

/// Holds at most one analyzed prefix. Changing its exact store position drops
/// the previous analysis, so scalar stores cannot accumulate quadratic caches.
pub(in crate::refresh) struct CanonicalAssignmentQueries<'source> {
    source: &'source [LinearOp],
    stores: Option<Vec<OutputStore>>,
    prefix: Option<(usize, Option<PrefixAnalysis<'source>>)>,
    causal: bool,
    #[cfg(test)]
    prefix_builds: usize,
}

struct OutputStore {
    first: usize,
    end: usize,
    position: usize,
    start: Reg,
    stride: usize,
}

struct PrefixAnalysis<'source> {
    producers: UniqueProgram<'source>,
    dependencies: ScalarProgramYDependency<'source>,
}

impl<'source> CanonicalAssignmentQueries<'source> {
    pub(in crate::refresh) fn new(source: &'source [LinearOp]) -> Self {
        Self {
            source,
            stores: output_stores(source),
            prefix: None,
            causal: !source.iter().any(super::non_causal_assignment_operation),
            #[cfg(test)]
            prefix_builds: 0,
        }
    }

    pub(in crate::refresh) fn is_causal(&self) -> bool {
        self.causal
    }

    pub(in crate::refresh) fn derive(
        &mut self,
        output_offset: usize,
        target: usize,
    ) -> Option<TargetAssignmentShape> {
        let Some(stores) = self.stores.as_ref() else {
            // Optional index construction overflowed; the unchanged owner still
            // answers this exact query and all source validation stays in force.
            return super::canonical_assignment_shape_for_output(
                self.source,
                output_offset,
                target,
            );
        };
        let index = stores.partition_point(|store| store.end <= output_offset);
        let store = stores.get(index)?;
        let offset = output_offset.checked_sub(store.first)?;
        let register = Reg::try_from(offset.checked_mul(store.stride)?)
            .ok()
            .and_then(|offset| store.start.checked_add(offset))?;
        let position = store.position;
        if self.prefix.as_ref().map(|(position, _)| *position) != Some(position) {
            self.prefix = None;
            let source = self.source.get(..position)?;
            let analysis = UniqueProgram::new(source).map(|producers| PrefixAnalysis {
                producers,
                dependencies: ScalarProgramYDependency::new(source),
            });
            self.prefix = Some((position, analysis));
            #[cfg(test)]
            {
                self.prefix_builds += 1;
            }
        }
        let (_, analysis) = self.prefix.as_ref()?;
        let analysis = analysis.as_ref()?;
        canonical_assignment_shape(
            analysis.producers.view(),
            register,
            target,
            &analysis.dependencies,
        )
    }
}

/// One record per store instruction, including ranged stores. The original
/// output iterator skips offsets whose register arithmetic overflows; valid
/// offsets form a prefix, whose exact length is computed without enumeration.
fn output_stores(source: &[LinearOp]) -> Option<Vec<OutputStore>> {
    let mut stores = Vec::new();
    let mut first = 0usize;
    for (position, operation) in source.iter().enumerate() {
        let (start, count, stride) = match *operation {
            LinearOp::StoreOutput { src } => (src, 1, 0),
            LinearOp::StoreOutputRange {
                start,
                count,
                stride,
            } => (start, count, stride),
            _ => continue,
        };
        let valid_count = ((Reg::MAX - start) as usize)
            .checked_div(stride)
            .map_or(count, |last| count.min(last.saturating_add(1)));
        let end = first.checked_add(valid_count)?;
        if end != first {
            stores.push(OutputStore {
                first,
                end,
                position,
                start,
                stride,
            });
        }
        first = end;
    }
    Some(stores)
}

/// Construction-local source identity. At most one original program and one
/// prefix stay resident; nothing survives a validation pass or source change.
pub(in crate::refresh) struct SourceCertificateQueries<'source> {
    source: &'source crate::ComputeBlock,
    active: Option<(
        crate::RefreshScalarProgramSource,
        CanonicalAssignmentQueries<'source>,
    )>,
}

impl<'source> SourceCertificateQueries<'source> {
    pub(in crate::refresh) fn new(source: &'source crate::ComputeBlock) -> Self {
        Self {
            source,
            active: None,
        }
    }

    pub(in crate::refresh) fn derive(
        &mut self,
        row: &crate::AlgebraicRefreshRow,
        label: &str,
    ) -> Result<(Option<TargetAssignmentShape>, bool), crate::ContinuousRefreshConstructionError>
    {
        let queries = match &mut self.active {
            Some((source, queries)) if *source == row.source => queries,
            active => {
                let (program, _) = super::super::scalar_source_program(self.source, row.source)?
                    .ok_or_else(|| crate::ContinuousRefreshConstructionError {
                        reason: format!(
                            "{label} refresh row refers to a missing canonical source program"
                        ),
                    })?;
                &mut active
                    .insert((row.source, CanonicalAssignmentQueries::new(program)))
                    .1
            }
        };
        Ok((
            queries.derive(row.output_offset, row.target_index),
            queries.is_causal(),
        ))
    }
}
