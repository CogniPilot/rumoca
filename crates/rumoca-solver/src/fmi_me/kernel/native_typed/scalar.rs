//! Float64 values of a scalar (non-native) instance through the retained
//! component's original FMI inventory. There is no second VR table: ordinary
//! variables are checked by the same request owner the native path uses, and
//! the scalar Co-Simulation description's two further published owners are
//! resolved from the same checked metadata: the declared maximum step
//! duration, and each state's derivative at the derivative value-reference
//! base plus the state's declaration ordinal (the shared FMI 3 template's
//! numbering). Values come from one branded observation, or before
//! initialization from the host-written cells only. Integer, Boolean and
//! enumeration variables are read and written through the same request owner,
//! each cell holding its value as the generated C kernel holds it
//! ([`ScalarCell`]); an enumeration is served through the Int64 accessors and
//! holds one of its literals' ordinals. String variables are checked by the
//! same owner; their values are never evaluated (see the instance cell).

use rumoca_core::EnumerationDeclaration;
use rumoca_ir_solve::SolveVariableValueKind;
use rumoca_ir_solve::fmi::{FmiMetadata, FmiStorageColumn, FmiStorageRun, FmiVariable};

use super::super::{MeError, MeFloat64Backing, MeObservation, MeValueRef, SolveMeKernel, contract};
use super::request::{self, Access, ReadMember, Served};
use crate::fmi_me::lifecycle::MeState;

/// An Integer or Boolean FMI value held in a scalar instance's numeric cell,
/// encoded as the generated C kernel encodes it (the fmi3 template's
/// `RMC_GET_NUMERIC`/`RMC_SET_NUMERIC`): a Boolean cell holds exactly 0 or 1;
/// an Int64 cell reads as a finite integral value in [-2^63, 2^63) and is
/// written only within the exact range ±2^53. An enumeration cell or value
/// is also one of its literals' ordinals (`rmc_integer_public_value`,
/// `rmc_ordinal_valid`). A cell or value outside its type is refused, never
/// rounded or converted.
pub(crate) trait ScalarCell: Copy + PartialEq + std::fmt::Display {
    const KIND: SolveVariableValueKind;
    fn decode(cell: f64) -> Result<Self, MeError>;
    fn encode(self) -> Result<f64, MeError>;
    /// The enumeration ordinal this value denotes, if its type has ordinals.
    fn ordinal(self) -> Option<i64>;
}

/// Refuse `value` unless it is a literal of `enumeration`, when there is one.
fn require_literal<T: ScalarCell>(
    value: T,
    enumeration: Option<&EnumerationDeclaration>,
) -> Result<T, MeError> {
    match enumeration {
        Some(enumeration)
            if !value
                .ordinal()
                .is_some_and(|ordinal| enumeration.contains_ordinal(ordinal)) =>
        {
            Err(contract(format!(
                "{value} is not an ordinal of enumeration {}",
                enumeration.name
            )))
        }
        _ => Ok(value),
    }
}

/// The largest magnitude every Int64 write keeps exact in its cell.
const EXACT_INTEGER: i64 = 1 << 53;
/// 2^63, exact as a double: the open upper bound of an Int64 cell.
const INT64_END: f64 = 9_223_372_036_854_775_808.0;

impl ScalarCell for i64 {
    const KIND: SolveVariableValueKind = SolveVariableValueKind::Integer;

    fn decode(cell: f64) -> Result<Self, MeError> {
        if cell.is_finite() && cell.trunc() == cell && (-INT64_END..INT64_END).contains(&cell) {
            Ok(cell as i64)
        } else {
            Err(contract(format!("an Integer cell holds {cell}")))
        }
    }

    fn encode(self) -> Result<f64, MeError> {
        if (-EXACT_INTEGER..=EXACT_INTEGER).contains(&self) {
            Ok(self as f64)
        } else {
            Err(contract(format!(
                "Integer {self} is outside the exact range of its cell"
            )))
        }
    }

    fn ordinal(self) -> Option<i64> {
        Some(self)
    }
}

impl ScalarCell for bool {
    const KIND: SolveVariableValueKind = SolveVariableValueKind::Boolean;

    fn decode(cell: f64) -> Result<Self, MeError> {
        if cell == 0.0 {
            Ok(false)
        } else if cell == 1.0 {
            Ok(true)
        } else {
            Err(contract(format!("a Boolean cell holds {cell}")))
        }
    }

    fn encode(self) -> Result<f64, MeError> {
        Ok(if self { 1.0 } else { 0.0 })
    }

    fn ordinal(self) -> Option<i64> {
        None
    }
}

/// One checked typed member: its storage run and, for an enumeration, the
/// declaration whose literals its cells hold.
type TypedRun<'inventory> = (FmiStorageRun, Option<&'inventory EnumerationDeclaration>);

/// Decode every cell of `runs`, in request order, from the column each names.
fn decode_runs<'cells, T: ScalarCell>(
    runs: &[TypedRun<'_>],
    mut cells: impl FnMut(FmiStorageRun) -> Result<&'cells [f64], MeError>,
) -> Result<Vec<T>, MeError> {
    let mut values = Vec::new();
    for &(run, enumeration) in runs {
        for &cell in cells(run)? {
            values.push(require_literal(T::decode(cell)?, enumeration)?);
        }
    }
    Ok(values)
}

/// One checked member of a scalar-route Float64 read, in request order.
#[derive(Clone, Copy)]
enum ScalarMember {
    Time,
    Run(FmiStorageRun),
    MaxStepDuration,
    /// The derivative of the state stored at `base..base + count` of Y.
    Derivative {
        base: usize,
        count: usize,
    },
}

impl ScalarMember {
    fn scalar_count(self) -> usize {
        match self {
            Self::Time | Self::MaxStepDuration => 1,
            Self::Run(run) => run.scalar_count(),
            Self::Derivative { count, .. } => count,
        }
    }
}

impl SolveMeKernel {
    fn scalar_inventory(&self) -> Result<&FmiMetadata, MeError> {
        if self.native_transport.is_some() {
            return Err(contract(
                "a native instance reads Float64 values through its native transport",
            ));
        }
        self.runtime
            .model()
            .retained_assertion_root()
            .map(|root| root.metadata())
            .ok_or_else(|| contract("scalar FMI values require the correlated retained component"))
    }

    /// Check a whole Float64 read request before anything is evaluated, and
    /// return its members and scalar count. Before initialization only the
    /// host-written cells are readable (`pending`).
    fn scalar_read_request(
        &self,
        references: &[u32],
        pending: bool,
    ) -> Result<(Vec<ScalarMember>, usize), MeError> {
        let metadata = self.scalar_inventory()?;
        let states = metadata.state_variable_indices();
        let derivative_base = metadata.derivative_value_reference_base_fmi3();
        derivative_base
            .checked_add(u32::try_from(states.len()).map_err(|_| contract("too many states"))?)
            .ok_or_else(|| contract("derivative value references overflow"))?;
        let max_step_duration = metadata
            .max_step_duration()
            .map(rumoca_ir_solve::fmi::FmiVariable::value_reference_fmi3);
        let mut members = Vec::with_capacity(references.len());
        let mut count = 0usize;
        for &reference in references {
            let ordinal = reference
                .checked_sub(derivative_base)
                .map(|ordinal| ordinal as usize)
                .filter(|&ordinal| ordinal < states.len());
            let member = if let Some(ordinal) = ordinal {
                self.derivative_member(metadata, states[ordinal], reference, pending)?
            } else if Some(reference) == max_step_duration {
                if pending {
                    return Err(contract(
                        "the maximum step duration is not a host-written cell",
                    ));
                }
                ScalarMember::MaxStepDuration
            } else {
                let (read, _) = request::read_request(
                    metadata,
                    std::slice::from_ref(&reference),
                    SolveVariableValueKind::Real,
                    pending,
                )?;
                match read.first() {
                    Some(ReadMember::Time) => ScalarMember::Time,
                    Some(ReadMember::Variable(variable)) => ScalarMember::Run(
                        variable
                            .storage()
                            .expect("request checked original storage"),
                    ),
                    None => return Err(contract("empty checked read member")),
                }
            };
            count = count.checked_add(member.scalar_count()).ok_or_else(|| {
                contract("typed FMI batch scalar cardinality exceeds addressable storage")
            })?;
            members.push(member);
        }
        Ok((members, count))
    }

    /// The derivative of the state variable at metadata index `state`, the
    /// one the shared template publishes at `reference`.
    fn derivative_member(
        &self,
        metadata: &FmiMetadata,
        state: usize,
        reference: u32,
        pending: bool,
    ) -> Result<ScalarMember, MeError> {
        if pending {
            return Err(contract(
                "state derivatives are readable only after initialization",
            ));
        }
        if reference
            .checked_sub(1)
            .and_then(|index| metadata.variables().get(index as usize))
            .is_some_and(|variable| variable.value_reference_fmi3() == reference)
        {
            return Err(contract(format!(
                "derivative value reference {reference} collides with a declared variable"
            )));
        }
        let run = metadata
            .variables()
            .get(state)
            .and_then(rumoca_ir_solve::fmi::FmiVariable::storage)
            .filter(|run| run.column() == FmiStorageColumn::Y)
            .ok_or_else(|| contract("a published state has no Y storage run"))?;
        let end = run
            .base()
            .checked_add(run.scalar_count())
            .filter(|&end| end <= self.state_count)
            .ok_or_else(|| contract("a published state lies outside the continuous states"))?;
        Ok(ScalarMember::Derivative {
            base: run.base(),
            count: end - run.base(),
        })
    }

    /// Project a whole checked Float64 batch, in request order, from one
    /// observation of this instance taken at the kernel's current coordinate;
    /// derivatives are evaluated at that same coordinate. The result is staged
    /// and returned only when every member projected.
    pub(crate) fn project_scalar_float64(
        &self,
        observation: &MeObservation,
        references: &[u32],
    ) -> Result<Vec<f64>, MeError> {
        self.require_active_lifecycle("project_scalar_float64")?;
        self.require_observation_brand(observation)?;
        let (members, count) = self.scalar_read_request(references, false)?;
        let derivatives = if members
            .iter()
            .any(|member| matches!(member, ScalarMember::Derivative { .. }))
        {
            self.observation_derivatives(observation)?
        } else {
            Vec::new()
        };
        let step_bound = if members
            .iter()
            .any(|member| matches!(member, ScalarMember::MaxStepDuration))
        {
            self.require_observation_coordinate(observation)?;
            self.observation_step_bound()?
        } else {
            None
        };
        let mut values = Vec::with_capacity(count);
        for member in members {
            match member {
                ScalarMember::Time => values.push(observation.time),
                ScalarMember::MaxStepDuration => values.push(
                    step_bound.unwrap_or(rumoca_ir_solve::fmi::MAX_STEP_DURATION_UNCONSTRAINED),
                ),
                ScalarMember::Derivative { base, count } => {
                    values.extend_from_slice(&derivatives[base..base + count]);
                }
                ScalarMember::Run(run) => {
                    let column = match run.column() {
                        FmiStorageColumn::Y => &observation.solver_y,
                        FmiStorageColumn::P => &observation.parameters,
                    };
                    let slice = column
                        .get(run.base()..run.base() + run.scalar_count())
                        .ok_or_else(|| {
                            contract("checked storage run lies outside the observation")
                        })?;
                    values.extend_from_slice(slice);
                }
            }
        }
        Ok(values)
    }

    /// The observation was taken at this kernel's current coordinate (same
    /// time and continuous-state prefix, bit for bit); anything else refuses.
    fn require_observation_coordinate(&self, observation: &MeObservation) -> Result<(), MeError> {
        let same_states = observation
            .solver_y
            .get(..self.states.len())
            .is_some_and(|prefix| {
                prefix
                    .iter()
                    .zip(&self.states)
                    .all(|(observed, state)| observed.to_bits() == state.to_bits())
            });
        if observation.time.to_bits() != self.time.to_bits() || !same_states {
            return Err(contract(
                "the observation is not at this instance's current coordinate",
            ));
        }
        Ok(())
    }

    /// State derivatives bound to `observation`: evaluated at its public time
    /// with its refreshed parameters (delay values, public clock activation,
    /// dependent discrete rows) and its refreshed solver vector as the guess,
    /// through the production derivative evaluator. Nothing is cached or
    /// committed. Where the integrator's derivative coordinate differs from
    /// the public one (a reached, unprocessed time-event boundary uses the
    /// left limit), a published derivative is ambiguous and is refused.
    fn observation_derivatives(&self, observation: &MeObservation) -> Result<Vec<f64>, MeError> {
        self.require_observation_coordinate(observation)?;
        let time = self.public_time_eval_time(observation.time);
        if self.continuous_eval_time().to_bits() != time.to_bits() {
            return Err(contract(
                "a state derivative at an unprocessed event boundary is ambiguous",
            ));
        }
        let settle = self.numerics_settle();
        let mut guess = observation.solver_y.clone();
        let mut derivatives = vec![0.0; self.state_count];
        self.runtime
            .eval_state_derivatives_with_guess_into(
                time,
                &self.states,
                &observation.parameters,
                &mut guess,
                settle.tol,
                settle.max_iters,
                &mut derivatives,
            )
            .map_err(MeError::from)?;
        Ok(derivatives)
    }

    /// Before initialization completes, read only host-written cells (inputs,
    /// parameters, constants) from this instance's own parameter vector.
    pub(crate) fn read_scalar_host_cells(&self, references: &[u32]) -> Result<Vec<f64>, MeError> {
        self.require_active_lifecycle("read_scalar_host_cells")?;
        let (members, count) = self.scalar_read_request(references, true)?;
        let mut values = Vec::with_capacity(count);
        for member in members {
            let ScalarMember::Run(run) = member else {
                return Err(contract("only host-written cells are readable here"));
            };
            let slice = self
                .params
                .get(run.base()..run.base() + run.scalar_count())
                .ok_or_else(|| contract("checked host cell lies outside the parameters"))?;
            values.extend_from_slice(slice);
        }
        Ok(values)
    }

    /// The branded input-parameter references of a checked Float64 write, one
    /// per scalar in request order. Causality and the lifecycle window are
    /// checked first, by the request owner. The window is this instance's
    /// actual lifecycle state, not who holds it: in Instantiated and
    /// Initialization Mode inputs and settable parameters are writable,
    /// otherwise (including after a failed ExitInitializationMode that left
    /// a later state) inputs only. Derivatives, the maximum step duration and
    /// time are never writable here.
    pub(crate) fn scalar_float64_targets(
        &self,
        references: &[u32],
        value_count: usize,
    ) -> Result<Vec<MeValueRef>, MeError> {
        self.require_active_lifecycle("scalar_float64_targets")?;
        self.scalar_targets(references, value_count, SolveVariableValueKind::Real)
    }

    /// The window is this instance's actual lifecycle state (see
    /// [`Self::scalar_float64_targets`]).
    fn scalar_write_access(&self) -> Access {
        match self.initialization_state() {
            MeState::Instantiated | MeState::InitializationMode => Access::WriteInputsAndParameters,
            _ => Access::WriteInputs,
        }
    }

    fn scalar_write_variables(
        &self,
        references: &[u32],
        value_count: usize,
        kind: SolveVariableValueKind,
    ) -> Result<Vec<&FmiVariable>, MeError> {
        request::checked_request_served(
            self.scalar_inventory()?,
            references,
            value_count,
            kind,
            self.scalar_write_access(),
            Served::Fmi3,
        )
    }

    fn scalar_targets(
        &self,
        references: &[u32],
        value_count: usize,
        kind: SolveVariableValueKind,
    ) -> Result<Vec<MeValueRef>, MeError> {
        let variables = self.scalar_write_variables(references, value_count, kind)?;
        Ok(self.input_parameter_targets(&variables, value_count))
    }

    fn input_parameter_targets(
        &self,
        variables: &[&FmiVariable],
        value_count: usize,
    ) -> Vec<MeValueRef> {
        let mut targets = Vec::with_capacity(value_count);
        for variable in variables {
            let storage = variable
                .storage()
                .expect("request checked original storage");
            targets.extend(
                (storage.base()..storage.base() + storage.scalar_count()).map(|index| MeValueRef {
                    backing: MeFloat64Backing::InputParameter(index),
                    instance_brand: std::rc::Rc::clone(&self.instance_brand),
                }),
            );
        }
        targets
    }

    /// The storage runs of a checked Integer or Boolean request, in request
    /// order, with each enumeration's declaration. Only declared variables of
    /// `kind` answer, and enumerations to an Int64 request; before
    /// initialization only the host-written ones (`pending`).
    fn scalar_typed_runs(
        &self,
        references: &[u32],
        kind: SolveVariableValueKind,
        pending: bool,
    ) -> Result<Vec<TypedRun<'_>>, MeError> {
        let (members, _) = request::read_request_served(
            self.scalar_inventory()?,
            references,
            kind,
            pending,
            Served::Fmi3,
        )?;
        members
            .iter()
            .map(|member| {
                let variable = member
                    .variable()
                    .ok_or_else(|| contract("a typed request member is not a variable"))?;
                let run = variable
                    .storage()
                    .ok_or_else(|| contract("a typed request member has no storage run"))?;
                Ok((run, variable.enumeration()))
            })
            .collect()
    }

    /// Project a whole checked Integer or Boolean batch, in request order,
    /// from one observation of this instance at its current coordinate. The
    /// result is staged and returned only when every cell decoded.
    pub(crate) fn project_scalar_typed<T: ScalarCell>(
        &self,
        observation: &MeObservation,
        references: &[u32],
    ) -> Result<Vec<T>, MeError> {
        self.require_active_lifecycle("project_scalar_typed")?;
        self.require_observation_brand(observation)?;
        let runs = self.scalar_typed_runs(references, T::KIND, false)?;
        decode_runs(&runs, |run| {
            let column = match run.column() {
                FmiStorageColumn::Y => &observation.solver_y,
                FmiStorageColumn::P => &observation.parameters,
            };
            column
                .get(run.base()..run.base() + run.scalar_count())
                .ok_or_else(|| contract("checked storage run lies outside the observation"))
        })
    }

    /// Before initialization completes, read only host-written Integer or
    /// Boolean cells from this instance's own parameter vector.
    pub(crate) fn read_scalar_typed_host_cells<T: ScalarCell>(
        &self,
        references: &[u32],
    ) -> Result<Vec<T>, MeError> {
        self.require_active_lifecycle("read_scalar_typed_host_cells")?;
        let runs = self.scalar_typed_runs(references, T::KIND, true)?;
        decode_runs(&runs, |run| {
            self.params
                .get(run.base()..run.base() + run.scalar_count())
                .ok_or_else(|| contract("checked host cell lies outside the parameters"))
        })
    }

    /// The branded input-parameter references and encoded cells of a checked
    /// Integer, Boolean or enumeration write, one per scalar in request order,
    /// by the same causality and lifecycle window as a Float64 write. Every
    /// value is checked against its variable's type and encoded before
    /// anything is written.
    pub(crate) fn scalar_typed_targets<T: ScalarCell>(
        &self,
        references: &[u32],
        values: &[T],
    ) -> Result<(Vec<MeValueRef>, Vec<f64>), MeError> {
        self.require_active_lifecycle("scalar_typed_targets")?;
        let variables = self.scalar_write_variables(references, values.len(), T::KIND)?;
        let mut cells = Vec::with_capacity(values.len());
        let mut pending = values.iter();
        for variable in &variables {
            let count = variable
                .storage()
                .expect("request checked original storage")
                .scalar_count();
            for &value in pending.by_ref().take(count) {
                cells.push(require_literal(value, variable.enumeration())?.encode()?);
            }
        }
        Ok((
            self.input_parameter_targets(&variables, values.len()),
            cells,
        ))
    }

    /// The checked String variables of a read, in request order. A String
    /// value is never evaluated (the scalar deployment admits only parameters
    /// and constants with a literal start), so every active state reads it.
    pub(crate) fn scalar_text_variables(
        &self,
        references: &[u32],
    ) -> Result<Vec<&FmiVariable>, MeError> {
        self.require_active_lifecycle("scalar_text_variables")?;
        let (members, _) = request::read_request(
            self.scalar_inventory()?,
            references,
            SolveVariableValueKind::String,
            false,
        )?;
        members
            .iter()
            .map(|member| {
                member
                    .variable()
                    .ok_or_else(|| contract("a String request member is not a variable"))
            })
            .collect()
    }

    /// The checked String variables of a write of `value_count` values, by the
    /// same causality and lifecycle window as every other typed write.
    pub(crate) fn scalar_text_targets(
        &self,
        references: &[u32],
        value_count: usize,
    ) -> Result<Vec<&FmiVariable>, MeError> {
        self.require_active_lifecycle("scalar_text_targets")?;
        self.scalar_write_variables(references, value_count, SolveVariableValueKind::String)
    }

    /// Scalar count of a checked batch, by the same owner as its reads.
    pub(crate) fn scalar_value_count(
        &self,
        references: &[u32],
        kind: SolveVariableValueKind,
        pending: bool,
    ) -> Result<usize, MeError> {
        if kind == SolveVariableValueKind::String {
            return Ok(self
                .scalar_text_variables(references)?
                .iter()
                .map(|variable| {
                    variable
                        .storage()
                        .expect("request checked original storage")
                        .scalar_count()
                })
                .sum());
        }
        if kind != SolveVariableValueKind::Real {
            let (_, count) = request::read_request_served(
                self.scalar_inventory()?,
                references,
                kind,
                pending,
                Served::Fmi3,
            )?;
            return Ok(count);
        }
        let (_, count) = self.scalar_read_request(references, pending)?;
        Ok(count)
    }
}
