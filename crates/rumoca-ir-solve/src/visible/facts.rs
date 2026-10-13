use super::*;

impl VisibleProjection {
    pub(super) fn validate_source_facts(
        &self,
        problem: &SolveProblem,
    ) -> Result<(), VisibleProjectionError> {
        let mut coordinates = std::collections::BTreeSet::new();
        let mut prefix = 0;
        for member in &self.members {
            let (extent, facts) = match member {
                VisibleProjectionInput::Declaration(source) => {
                    let count =
                        problem.solve_layout.variable_storage_runs[source.declaration].scalar_count;
                    let facts = source
                        .state_coordinates
                        .iter()
                        .map(|(ordinal, _)| *ordinal)
                        .chain(source.phasors.iter().map(|(ordinal, _)| *ordinal))
                        .collect::<std::collections::BTreeSet<_>>();
                    (count, facts)
                }
                VisibleProjectionInput::Expression {
                    names, metadata, ..
                } => (
                    names.len(),
                    metadata
                        .as_deref()
                        .unwrap_or_default()
                        .iter()
                        .enumerate()
                        .filter(|(_, meta)| {
                            meta.state_coordinate.is_some() || meta.phasor.is_some()
                        })
                        .map(|(ordinal, _)| ordinal)
                        .collect(),
                ),
            };
            for ordinal in facts {
                let meta = self
                    .metadata(problem, prefix + ordinal)
                    .ok_or_else(|| error("missing source fact point", None))?;
                self.validate_point_facts(problem, &meta, &mut coordinates)?;
            }
            prefix += extent;
        }
        Ok(())
    }

    fn validate_point_facts(
        &self,
        problem: &SolveProblem,
        meta: &SolveVariableMeta,
        coordinates: &mut std::collections::BTreeSet<(String, u32)>,
    ) -> Result<(), VisibleProjectionError> {
        if let Some(source) = &meta.state_coordinate
            && (!meta.is_state
                || self.index(problem, &source.variable).is_none()
                || !coordinates.insert((source.variable.clone(), source.derivative_order)))
        {
            return Err(error(
                "invalid state coordinate source equation",
                Some(meta.source_span),
            ));
        }
        if let Some(phasor) = &meta.phasor
            && (phasor.re == phasor.im
                || [&phasor.re, &phasor.im].iter().any(|name| {
                    **name == meta.name
                        || self
                            .index(problem, name)
                            .and_then(|point| self.metadata(problem, point))
                            .is_none_or(|source| source.value_type.as_deref() != Some("Real"))
                }))
        {
            return Err(error(
                "invalid phasor source equation",
                Some(meta.source_span),
            ));
        }
        Ok(())
    }
}
