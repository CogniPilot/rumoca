//! Checked, target-neutral FMI component data.
//!
//! An [`FmiComponent`] binds tensor-native source declarations to the exact
//! Solve storage runs that execute them. FMI 2 scalar variables and FMI 3
//! aggregate value references are derived views of this one checked object.

use rumoca_core::Span;
use rumoca_ir_solve::{ScalarSlot, SolveProblem, SolveVariableStorageRole, SolveVariableValueKind};
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
}

#[derive(Debug, Serialize)]
pub struct FmiComponent {
    variables: Vec<FmiVariable>,
    state_variable_indices: Vec<usize>,
    derivative_value_reference_base_fmi3: u32,
    #[serde(skip)]
    solve: SolveProblem,
}

impl FmiComponent {
    pub fn construct(
        solve: SolveProblem,
        inputs: Vec<FmiVariableInput>,
    ) -> Result<Self, FmiComponentError> {
        solve
            .validate()
            .map_err(|error| FmiComponentError::InvalidSolve(error.to_string()))?;
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
            solve,
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
    pub fn into_solve(self) -> SolveProblem {
        self.solve
    }
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
