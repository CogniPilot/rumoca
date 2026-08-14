//! Checked, target-neutral FMI component data.
//!
//! An [`FmiComponent`] binds tensor-native source declarations to the exact
//! Solve storage runs that execute them. FMI 2 scalar variables and FMI 3
//! aggregate value references are derived views of this one checked object.

use rumoca_core::Span;
use rumoca_ir_solve::{
    DiscreteRowRole, LinearOp, ScalarProgramYDependency, ScalarSlot, SolveEventActionKind,
    SolveEventMessagePart, SolveProblem, SolvePureCallTable, SolveVariableStorageRole,
    SolveVariableValueKind,
};
use serde::Serialize;
use std::collections::BTreeSet;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum FmiStorageColumn {
    Y,
    P,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct FmiStorageRun {
    column: FmiStorageColumn,
    base: usize,
    scalar_count: usize,
}

impl FmiStorageRun {
    #[must_use]
    pub const fn column(self) -> FmiStorageColumn {
        self.column
    }

    #[must_use]
    pub const fn base(self) -> usize {
        self.base
    }

    #[must_use]
    pub const fn scalar_count(self) -> usize {
        self.scalar_count
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct FmiVariable {
    name: String,
    scalar_names: Vec<String>,
    role: SolveVariableStorageRole,
    value_kind: SolveVariableValueKind,
    dimensions: Vec<u32>,
    storage: FmiStorageRun,
    start: Vec<f64>,
    minimum: Option<Vec<f64>>,
    maximum: Option<Vec<f64>>,
    nominal: Option<Vec<f64>>,
    unit: Option<String>,
    description: Option<String>,
    causality: FmiCausality,
    variability: FmiVariability,
    tunable: bool,
    declaration: Span,
    value_reference_fmi3: u32,
}

impl FmiVariable {
    #[must_use]
    pub fn name(&self) -> &str {
        &self.name
    }

    #[must_use]
    pub fn scalar_names(&self) -> &[String] {
        &self.scalar_names
    }

    #[must_use]
    pub const fn role(&self) -> SolveVariableStorageRole {
        self.role
    }

    #[must_use]
    pub const fn value_kind(&self) -> SolveVariableValueKind {
        self.value_kind
    }

    #[must_use]
    pub fn dimensions(&self) -> &[u32] {
        &self.dimensions
    }

    #[must_use]
    pub const fn storage(&self) -> FmiStorageRun {
        self.storage
    }

    #[must_use]
    pub fn start(&self) -> &[f64] {
        &self.start
    }

    #[must_use]
    pub const fn value_reference_fmi3(&self) -> u32 {
        self.value_reference_fmi3
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum FmiCausality {
    Input,
    Output,
    Parameter,
    CalculatedParameter,
    Independent,
    Local,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum FmiVariability {
    Constant,
    Fixed,
    Tunable,
    Discrete,
    Continuous,
}

/// Unchecked lowering input consumed only by [`FmiComponent::construct`].
#[derive(Debug, Clone)]
pub struct FmiVariableInput {
    pub name: String,
    pub scalar_names: Vec<String>,
    pub role: SolveVariableStorageRole,
    pub value_kind: SolveVariableValueKind,
    pub dimensions: Vec<u32>,
    pub start: Vec<f64>,
    pub minimum: Option<Vec<f64>>,
    pub maximum: Option<Vec<f64>>,
    pub nominal: Option<Vec<f64>>,
    pub unit: Option<String>,
    pub description: Option<String>,
    pub causality: FmiCausality,
    pub variability: FmiVariability,
    pub tunable: bool,
    pub declaration: Span,
}

#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum FmiComponentError {
    #[error("Solve kernel is invalid: {0}")]
    InvalidSolve(String),
    #[error("FMI declaration count {variables} does not match Solve storage count {storage}")]
    VariableCount { variables: usize, storage: usize },
    #[error("FMI variable `{name}` has a zero-extent tensor shape")]
    ZeroExtentShape { name: String, span: Span },
    #[error("FMI variable `{name}` has {actual} scalars but its shape requires {expected}")]
    ScalarCount {
        name: String,
        actual: usize,
        expected: usize,
        span: Span,
    },
    #[error("FMI variable `{name}` has duplicate source identity")]
    DuplicateName { name: String, span: Span },
    #[error("FMI variable `{name}` does not match its Solve declaration or storage role")]
    StorageTypeMismatch { name: String, span: Span },
    #[error("FMI variable `{name}` is stored in a non-addressable Solve slot")]
    NonAddressableStorage { name: String, span: Span },
    #[error("FMI 3 value-reference space exceeds u32")]
    ValueReferenceOverflow,
    #[error("FMI state scalar count {actual} does not match Solve state count {expected}")]
    StateCount { actual: usize, expected: usize },
    #[error("FMI assertion-only event profile is invalid: {0}")]
    UnsupportedAssertionProfile(&'static str),
}

#[derive(Debug, Clone, Serialize)]
pub struct FmiAssertion {
    pub message_bytes: Vec<u8>,
    pub span: Span,
}

#[derive(Debug, Serialize)]
pub struct FmiComponent {
    variables: Vec<FmiVariable>,
    state_variable_indices: Vec<usize>,
    derivative_value_reference_base_fmi3: u32,
    assertions: Vec<FmiAssertion>,
    #[serde(skip)]
    solve: SolveProblem,
    #[serde(skip)]
    pure_calls: SolvePureCallTable,
}

impl FmiComponent {
    pub fn construct(
        solve: SolveProblem,
        pure_calls: SolvePureCallTable,
        inputs: Vec<FmiVariableInput>,
    ) -> Result<Self, FmiComponentError> {
        rumoca_ir_solve::validate_problem_pure_call_sites(&solve, &pure_calls)
            .map_err(|error| FmiComponentError::InvalidSolve(error.to_string()))?;
        let assertions = checked_assertion_profile(&solve, &pure_calls)?;
        let runs = &solve.solve_layout.variable_storage_runs;
        let declarations = &solve.solve_layout.variable_declarations;
        if inputs.len() != runs.len() || inputs.len() != declarations.len() {
            return Err(FmiComponentError::VariableCount {
                variables: inputs.len(),
                storage: runs.len(),
            });
        }

        let mut names = BTreeSet::new();
        let mut state_scalar_count = 0usize;
        let mut state_variable_indices = Vec::new();
        let mut variables = Vec::with_capacity(inputs.len());
        for (index, (input, run)) in inputs.into_iter().zip(runs).enumerate() {
            let declaration = declarations[index];
            if !names.insert(input.name.clone()) {
                return Err(FmiComponentError::DuplicateName {
                    name: input.name,
                    span: input.declaration,
                });
            }
            let scalar_count = checked_scalar_count(&input)?;
            if scalar_count != run.scalar_count
                || input.role != run.role
                || input.value_kind != run.value_kind
                || input.role != declaration.role()
                || input.value_kind != declaration.value_kind()
            {
                return Err(FmiComponentError::StorageTypeMismatch {
                    name: input.name,
                    span: input.declaration,
                });
            }
            let (column, base) = match run.base {
                ScalarSlot::Y { index, .. } => (FmiStorageColumn::Y, index),
                ScalarSlot::P { index, .. } => (FmiStorageColumn::P, index),
                ScalarSlot::Time | ScalarSlot::Constant(_) => {
                    return Err(FmiComponentError::NonAddressableStorage {
                        name: input.name,
                        span: input.declaration,
                    });
                }
            };
            if input.role == SolveVariableStorageRole::State {
                state_variable_indices.push(index);
                state_scalar_count = state_scalar_count
                    .checked_add(scalar_count)
                    .ok_or(FmiComponentError::ValueReferenceOverflow)?;
            }
            let value_reference_fmi3 = u32::try_from(index)
                .ok()
                .and_then(|value| value.checked_add(1))
                .ok_or(FmiComponentError::ValueReferenceOverflow)?;
            variables.push(FmiVariable {
                name: input.name,
                scalar_names: input.scalar_names,
                role: input.role,
                value_kind: input.value_kind,
                dimensions: input.dimensions,
                storage: FmiStorageRun {
                    column,
                    base,
                    scalar_count,
                },
                start: input.start,
                minimum: input.minimum,
                maximum: input.maximum,
                nominal: input.nominal,
                unit: input.unit,
                description: input.description,
                causality: input.causality,
                variability: input.variability,
                tunable: input.tunable,
                declaration: input.declaration,
                value_reference_fmi3,
            });
        }
        if state_scalar_count != solve.solve_layout.state_scalar_count {
            return Err(FmiComponentError::StateCount {
                actual: state_scalar_count,
                expected: solve.solve_layout.state_scalar_count,
            });
        }
        let derivative_value_reference_base_fmi3 = u32::try_from(variables.len())
            .ok()
            .and_then(|value| value.checked_add(1))
            .ok_or(FmiComponentError::ValueReferenceOverflow)?;
        Ok(Self {
            variables,
            state_variable_indices,
            derivative_value_reference_base_fmi3,
            assertions,
            solve,
            pure_calls,
        })
    }

    #[must_use]
    pub fn variables(&self) -> &[FmiVariable] {
        &self.variables
    }

    #[must_use]
    pub fn state_variable_indices(&self) -> &[usize] {
        &self.state_variable_indices
    }

    #[must_use]
    pub const fn derivative_value_reference_base_fmi3(&self) -> u32 {
        self.derivative_value_reference_base_fmi3
    }

    #[must_use]
    pub fn into_executable(self) -> (SolveProblem, SolvePureCallTable) {
        (self.solve, self.pure_calls)
    }
}

fn checked_assertion_profile(
    solve: &SolveProblem,
    pure_calls: &SolvePureCallTable,
) -> Result<Vec<FmiAssertion>, FmiComponentError> {
    let events = &solve.events;
    let discrete = &solve.discrete;
    if !events.scheduled_root_conditions.is_empty()
        || !events.scheduled_time_events.is_empty()
        || !events.dynamic_time_event_names.is_empty()
        || !events.dynamic_time_event_rhs.programs().is_empty()
        || events.has_terminal_event
        || !events.delays.source_rhs.programs().is_empty()
        || !events.delays.delay_time_rhs.programs().is_empty()
        || !events.delays.delay_max_rhs.programs().is_empty()
        || !events.delays.value_parameter_indices.is_empty()
        || !discrete.runtime_assignment_rhs.programs().is_empty()
        || !discrete.post_commit_assignment_rhs.programs().is_empty()
        || !discrete.guarded_assignments.is_empty()
        || !discrete.event_transactions.is_empty()
        || !discrete.structured_updates.is_empty()
        || !discrete.clock_partition_order.is_empty()
        || !solve.clocks.periodic_event_schedules.is_empty()
    {
        return Err(FmiComponentError::UnsupportedAssertionProfile(
            "runtime, scheduled, clocked, or updating events remain",
        ));
    }
    if events
        .root_relation_memory_targets
        .iter()
        .any(Option::is_some)
        || events.root_conditions.len() > events.actions.len()
    {
        return Err(FmiComponentError::UnsupportedAssertionProfile(
            "a root is not covered by the checked assertion action profile",
        ));
    }
    if events.action_conditions.len() != events.actions.len() {
        return Err(FmiComponentError::UnsupportedAssertionProfile(
            "assertion conditions are not action-aligned",
        ));
    }
    validate_discrete_assertion_rows(solve)?;
    validate_parameter_assertion_programs(solve, pure_calls)?;
    events
        .actions
        .iter()
        .map(|action| {
            if action.kind != SolveEventActionKind::Assert || action.clock_owner.is_some() {
                return Err(FmiComponentError::UnsupportedAssertionProfile(
                    "a non-assertion or clock-owned action remains",
                ));
            }
            let mut message = String::new();
            for part in &action.message.parts {
                let SolveEventMessagePart::Text(text) = part else {
                    return Err(FmiComponentError::UnsupportedAssertionProfile(
                        "a dynamic assertion message remains",
                    ));
                };
                message.push_str(text);
            }
            Ok(FmiAssertion {
                message_bytes: message.into_bytes(),
                span: action.span,
            })
        })
        .collect()
}

fn validate_parameter_assertion_programs(
    solve: &SolveProblem,
    pure_calls: &SolvePureCallTable,
) -> Result<(), FmiComponentError> {
    let static_y = solve
        .continuous
        .refresh_owners
        .algebraic()
        .static_causal_rows()
        .iter()
        .map(|row| row.target_index())
        .collect::<BTreeSet<_>>();
    let y_count = solve.solve_layout.solver_scalar_count();
    for program in solve.events.action_conditions.programs() {
        let outputs = program
            .iter()
            .filter_map(|operation| match operation {
                LinearOp::StoreOutput { src } => Some(*src),
                _ => None,
            })
            .collect::<Vec<_>>();
        if outputs.is_empty()
            || program
                .iter()
                .any(|operation| matches!(operation, LinearOp::StoreOutputRange { .. }))
        {
            return Err(FmiComponentError::UnsupportedAssertionProfile(
                "an assertion condition has no scalar output owner",
            ));
        }
        let dependencies = ScalarProgramYDependency::new_with_pure_calls(program, pure_calls);
        if outputs.iter().any(|output| {
            (0..y_count)
                .any(|index| !static_y.contains(&index) && dependencies.depends_on(*output, index))
        }) {
            return Err(FmiComponentError::UnsupportedAssertionProfile(
                "an assertion depends on runtime solver storage",
            ));
        }
    }
    Ok(())
}

fn validate_discrete_assertion_rows(solve: &SolveProblem) -> Result<(), FmiComponentError> {
    let discrete = &solve.discrete;
    for ((program, role), target) in discrete
        .rhs
        .programs()
        .iter()
        .zip(&discrete.row_roles)
        .zip(&discrete.update_targets)
    {
        if !matches!(target, ScalarSlot::P { .. }) {
            return Err(FmiComponentError::UnsupportedAssertionProfile(
                "a discrete row writes non-parameter storage",
            ));
        }
        match role {
            DiscreteRowRole::ConditionMemory if !solve.events.actions.is_empty() => {}
            DiscreteRowRole::Equation if is_constant_discrete_program(program) => {}
            DiscreteRowRole::Equation
            | DiscreteRowRole::EventAction
            | DiscreteRowRole::ConditionMemory => {
                return Err(FmiComponentError::UnsupportedAssertionProfile(
                    "a non-constant discrete equation or event update remains",
                ));
            }
        }
    }
    Ok(())
}

fn is_constant_discrete_program(program: &[LinearOp]) -> bool {
    matches!(
        program,
        [
            LinearOp::Const { dst: 0, .. },
            LinearOp::StoreOutput { src: 0 }
        ]
    )
}

fn checked_scalar_count(input: &FmiVariableInput) -> Result<usize, FmiComponentError> {
    if input.dimensions.contains(&0) {
        return Err(FmiComponentError::ZeroExtentShape {
            name: input.name.clone(),
            span: input.declaration,
        });
    }
    let expected = input
        .dimensions
        .iter()
        .try_fold(1usize, |count, extent| count.checked_mul(*extent as usize))
        .ok_or(FmiComponentError::ValueReferenceOverflow)?;
    let counts = [
        input.scalar_names.len(),
        input.start.len(),
        input.minimum.as_ref().map_or(expected, Vec::len),
        input.maximum.as_ref().map_or(expected, Vec::len),
        input.nominal.as_ref().map_or(expected, Vec::len),
    ];
    if let Some(actual) = counts.into_iter().find(|actual| *actual != expected) {
        return Err(FmiComponentError::ScalarCount {
            name: input.name.clone(),
            actual,
            expected,
            span: input.declaration,
        });
    }
    Ok(expected)
}
