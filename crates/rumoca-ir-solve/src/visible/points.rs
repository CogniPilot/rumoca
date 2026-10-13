use super::*;

impl VisibleProjection {
    pub(super) fn member_at<'a>(
        &'a self,
        problem: &SolveProblem,
        mut point: usize,
    ) -> Option<(&'a VisibleProjectionInput, usize)> {
        for member in &self.members {
            let extent = match member {
                VisibleProjectionInput::Declaration(source) => {
                    problem
                        .solve_layout
                        .variable_storage_runs
                        .get(source.declaration)?
                        .scalar_count
                }
                VisibleProjectionInput::Expression { names, .. } => names.len(),
            };
            if point < extent {
                return Some((member, point));
            }
            point -= extent;
        }
        None
    }

    pub(super) fn index(&self, problem: &SolveProblem, name: &str) -> Option<usize> {
        let mut prefix = 0usize;
        for member in &self.members {
            let (ordinal, extent) = match member {
                VisibleProjectionInput::Declaration(source) => {
                    let run = problem
                        .solve_layout
                        .variable_storage_runs
                        .get(source.declaration)?;
                    (
                        declaration_name_index(problem, source, run, name),
                        run.scalar_count,
                    )
                }
                VisibleProjectionInput::Expression { names, .. } => (
                    names.iter().position(|candidate| candidate == name),
                    names.len(),
                ),
            };
            if let Some(ordinal) = ordinal {
                return prefix.checked_add(ordinal);
            }
            prefix = prefix.checked_add(extent)?;
        }
        None
    }

    pub(super) fn name(&self, problem: &SolveProblem, point: usize) -> Option<String> {
        match self.member_at(problem, point)? {
            (VisibleProjectionInput::Declaration(source), ordinal) => problem
                .layout
                .scalar_declaration_name(&source.binding, ordinal),
            (VisibleProjectionInput::Expression { names, .. }, ordinal) => {
                names.get(ordinal).cloned()
            }
        }
    }

    pub(super) fn slot(&self, problem: &SolveProblem, point: usize) -> Option<ScalarSlot> {
        let (VisibleProjectionInput::Declaration(source), ordinal) =
            self.member_at(problem, point)?
        else {
            return None;
        };
        storage_point(
            problem
                .solve_layout
                .variable_storage_runs
                .get(source.declaration)?
                .base,
            ordinal,
        )
    }

    pub(super) fn metadata(
        &self,
        problem: &SolveProblem,
        point: usize,
    ) -> Option<SolveVariableMeta> {
        match self.member_at(problem, point)? {
            (VisibleProjectionInput::Expression { metadata, .. }, ordinal) => {
                metadata.as_ref()?.get(ordinal).cloned()
            }
            (VisibleProjectionInput::Declaration(source), ordinal) => {
                let declaration = problem
                    .solve_layout
                    .variable_declarations
                    .get(source.declaration)?;
                let attrs = &source.attributes;
                Some(SolveVariableMeta {
                    name: self.name(problem, point)?,
                    source_span: attrs.source_span,
                    role: role_name(declaration.role()).into(),
                    is_state: declaration.role() == crate::SolveVariableStorageRole::State,
                    value_type: Some(format!("{:?}", declaration.value_kind())),
                    variability: attrs.variability.clone(),
                    time_domain: Some(declaration.time_domain().as_str().into()),
                    unit: attrs.unit.clone(),
                    start: attrs.start.clone(),
                    min: attrs.min.clone(),
                    max: attrs.max.clone(),
                    nominal: attrs.nominal.clone(),
                    fixed: attrs.fixed,
                    description: attrs.description.clone(),
                    state_coordinate: source
                        .state_coordinates
                        .binary_search_by_key(&ordinal, |fact| fact.0)
                        .ok()
                        .map(|index| source.state_coordinates[index].1.clone()),
                    phasor: source
                        .phasors
                        .binary_search_by_key(&ordinal, |fact| fact.0)
                        .ok()
                        .map(|index| source.phasors[index].1.clone()),
                })
            }
        }
    }
}

fn declaration_name_index(
    problem: &SolveProblem,
    source: &VisibleDeclarationInput,
    run: &crate::SolveVariableStorageRun,
    name: &str,
) -> Option<usize> {
    let slot = problem.layout.binding(name)?;
    let ordinal = match (run.base, slot) {
        (ScalarSlot::Y { index: base, .. }, ScalarSlot::Y { index, .. })
        | (ScalarSlot::P { index: base, .. }, ScalarSlot::P { index, .. }) => {
            index.checked_sub(base)?
        }
        _ => return None,
    };
    (ordinal < run.scalar_count
        && problem
            .layout
            .scalar_declaration_name(&source.binding, ordinal)
            .as_deref()
            == Some(name))
    .then_some(ordinal)
}
