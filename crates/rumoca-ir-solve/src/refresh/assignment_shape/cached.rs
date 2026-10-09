//! Exact canonical prefix analysis shared only by adjacent queries on one source.
#[cfg(test)]
mod tests;

use super::{ProgramPrefix, canonical_assignment_shape, producers::UniqueProgram};
use crate::refresh::dependency::{ScalarProgramYDependency, ScalarProgramYDependencyQueries};
use crate::{LinearOp, ScalarProgramOutputStores, TargetAssignmentShape};

/// Holds at most one analyzed prefix. Changing its exact store position drops
/// the previous analysis, so scalar stores cannot accumulate quadratic caches.
pub struct CanonicalAssignmentQueries<'source> {
    source: &'source [LinearOp],
    stores: Option<ScalarProgramOutputStores>,
    prefix: Option<(usize, Option<PrefixAnalysis<'source>>)>,
    dependencies: ScalarProgramYDependencyQueries<'source>,
    causal: bool,
    any_shape: Option<(usize, bool)>,
    #[cfg(test)]
    prefix_builds: usize,
}

struct PrefixAnalysis<'source> {
    producers: UniqueProgram<'source>,
}

impl<'source> CanonicalAssignmentQueries<'source> {
    pub fn new(source: &'source [LinearOp]) -> Self {
        Self {
            source,
            stores: ScalarProgramOutputStores::new(source),
            prefix: None,
            dependencies: ScalarProgramYDependencyQueries::new(source),
            causal: !source.iter().any(super::non_causal_assignment_operation),
            any_shape: None,
            #[cfg(test)]
            prefix_builds: 0,
        }
    }

    pub(in crate::refresh) fn is_causal(&self) -> bool {
        self.causal
    }

    pub fn derive(&mut self, output_offset: usize, target: usize) -> Option<TargetAssignmentShape> {
        let Some(stores) = self.stores.as_ref() else {
            // Optional index construction overflowed; the unchanged owner still
            // answers this exact query and all source validation stays in force.
            return super::canonical_assignment_shape_for_output(
                self.source,
                output_offset,
                target,
            );
        };
        let (register, position) = stores.output(output_offset)?;
        let (prefix, dependencies) = self.prepare_prefix(position)?;
        canonical_assignment_shape(prefix, register, target, dependencies)
    }

    /// Whether any eligible target has an assignment shape for this output.
    /// Only one output's Boolean and one exact prefix stay resident.
    pub fn has_any(&mut self, output_offset: usize) -> bool {
        if let Some((output, result)) = self.any_shape
            && output == output_offset
        {
            return result;
        }
        let output = match &self.stores {
            Some(stores) => stores.output(output_offset),
            None => super::store_output_registers(self.source).nth(output_offset),
        };
        let result = output.is_some_and(|(register, position)| {
            self.prepare_prefix(position)
                .is_some_and(|(prefix, dependencies)| {
                    super::has_assignment_shape(prefix, register, dependencies)
                })
        });
        self.any_shape = Some((output_offset, result));
        result
    }

    fn prepare_prefix(
        &mut self,
        position: usize,
    ) -> Option<(ProgramPrefix<'_>, &ScalarProgramYDependency<'source>)> {
        if self.prefix.as_ref().map(|(position, _)| *position) != Some(position) {
            self.prefix = None;
            let source = self.source.get(..position)?;
            let analysis = UniqueProgram::new(source).map(|producers| PrefixAnalysis { producers });
            self.prefix = Some((position, analysis));
            #[cfg(test)]
            {
                self.prefix_builds += 1;
            }
        }
        let (_, analysis) = self.prefix.as_ref()?;
        let analysis = analysis.as_ref()?;
        Some((
            analysis.producers.view(),
            self.dependencies.prefix(position)?,
        ))
    }
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
