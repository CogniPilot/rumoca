//! Pre-projection admissibility over the untouched canonical DAE (GAL-004).
//!
//! Every check inspects canonical DAE fields directly — nothing is prepared,
//! rewritten, or erased before checking (destructive preparation would make
//! the gates vacuous). All failures carry stable `ET0xx` codes; scope
//! rejections use the GAL-025 wording ("not yet supported by the Rumoca
//! GALEC projection").
//!
//! Clocked relations are admissible: with no continuous states, the
//! relations in `conditions.relations`/`f_c` (e.g. limiter comparisons)
//! evaluate only at clock ticks and lower to if-expression conditions — they
//! are *not* runtime zero-crossing events and must not be rejected as such.

use rumoca_ir_dae::{Dae, DaeVariablePartition};

use crate::diagnostic::GalecTargetError;
use crate::input::GalecInput;

/// The single fixed-period clock a projected block runs on, validated by
/// [`check_admissibility`].
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct AdmittedClock {
    /// Finite, strictly positive tick period in seconds.
    pub period_seconds: f64,
    /// Tick phase offset in seconds.
    pub phase_seconds: f64,
}

/// Run all admissibility checks over the untouched DAE, collecting every
/// failure (collect-all, so one compile reports the full rejection surface).
///
/// On success returns the validated block clock.
pub fn check_admissibility(input: &GalecInput<'_>) -> Result<AdmittedClock, Vec<GalecTargetError>> {
    let dae = input.dae;
    let mut errors = Vec::new();
    check_metadata(dae, &mut errors);
    check_continuous(dae, &mut errors);
    check_initialization(dae, &mut errors);
    check_external_functions(dae, &mut errors);
    check_runtime_events(dae, &mut errors);
    check_dimensions(dae, &mut errors);
    let clock = check_clock(dae, &mut errors);
    match (clock, errors.is_empty()) {
        (Some(clock), true) => Ok(clock),
        _ => Err(errors),
    }
}

/// (e) Partial models and non-simulation class types are rejected.
fn check_metadata(dae: &Dae, errors: &mut Vec<GalecTargetError>) {
    if dae.metadata.is_partial {
        errors.push(GalecTargetError::PartialModel);
    }
    use rumoca_core::ClassType;
    match &dae.metadata.class_type {
        ClassType::Model | ClassType::Block | ClassType::Class => {}
        other => errors.push(GalecTargetError::UnsupportedClassType {
            class_type: other.as_str(),
        }),
    }
}

/// (a) No continuous dynamics: no state variables and no continuous
/// equations (GAL-025 wording).
fn check_continuous(dae: &Dae, errors: &mut Vec<GalecTargetError>) {
    let states = dae.variables.states.len();
    let equations = dae.continuous.equations.len();
    if states > 0 || equations > 0 {
        errors.push(GalecTargetError::ContinuousDynamics { states, equations });
    }
}

/// (g) No initial equations (GAL-025 wording): `Startup` is built from
/// manifest `start` values only, so a non-empty initialization partition
/// would be silently ignored rather than lowered — reject it up front.
fn check_initialization(dae: &Dae, errors: &mut Vec<GalecTargetError>) {
    let equations = dae.initialization.equations.len();
    let structured_families = dae.initialization.structured_equations.len();
    if equations > 0 || structured_families > 0 {
        errors.push(GalecTargetError::InitialEquations {
            equations,
            structured_families,
        });
    }
}

/// (b) No external functions (GAL-025 wording), one error per function so
/// the report names every offender.
fn check_external_functions(dae: &Dae, errors: &mut Vec<GalecTargetError>) {
    for function in dae.symbols.functions.values() {
        if let Some(external) = &function.external {
            errors.push(GalecTargetError::ExternalFunction {
                function: function.name.as_str().to_owned(),
                language: external.language.clone(),
                span: function.span,
            });
        }
    }
}

/// (c) No runtime events: no scheduled time events, no event actions
/// (assert/terminate at event instants), and no runtime-triggered (dynamic)
/// clock conditions. Clocked relations remain admissible (module docs).
fn check_runtime_events(dae: &Dae, errors: &mut Vec<GalecTargetError>) {
    let scheduled_time_events = dae.events.scheduled_time_events.len();
    let event_actions = dae.events.event_actions.len();
    if scheduled_time_events > 0 || event_actions > 0 {
        errors.push(GalecTargetError::RuntimeEvents {
            scheduled_time_events,
            event_actions,
        });
    }
    let triggered = dae.clocks.triggered_conditions.len();
    if triggered > 0 {
        errors.push(GalecTargetError::DynamicClock { count: triggered });
    }
}

/// (d) Exactly one fixed-period clock with a finite, strictly positive
/// period.
fn check_clock(dae: &Dae, errors: &mut Vec<GalecTargetError>) -> Option<AdmittedClock> {
    let schedules = &dae.clocks.schedules;
    if schedules.len() != 1 {
        errors.push(GalecTargetError::ClockCountNotOne {
            count: schedules.len(),
        });
        return None;
    }
    let schedule = &schedules[0];
    if !(schedule.period_seconds.is_finite() && schedule.period_seconds > 0.0) {
        errors.push(GalecTargetError::InvalidClockPeriod {
            period_seconds: schedule.period_seconds,
            span: schedule.source_span,
        });
        return None;
    }
    Some(AdmittedClock {
        period_seconds: schedule.period_seconds,
        phase_seconds: schedule.phase_seconds,
    })
}

/// (f) Every declared dimension is a literal integer >= 1 (GAL-020);
/// structurally-parametric or empty array sizes are rejected up front so
/// later slices can rely on positive literal dims.
fn check_dimensions(dae: &Dae, errors: &mut Vec<GalecTargetError>) {
    for_each_variable(dae, |_, variable| {
        for (index, size) in variable.dims.iter().enumerate() {
            if *size < 1 {
                errors.push(GalecTargetError::NonPositiveDimension {
                    variable: variable.name.as_str().to_owned(),
                    dimension: index + 1,
                    size: *size,
                    span: variable.source_span,
                });
            }
        }
    });
}

/// Visit every variable of all eight DAE partitions in canonical order
/// (inputs, outputs, parameters, constants, z, m, algebraics, states).
/// Shared with classification so both walk the same surface in the same
/// order.
pub(crate) fn for_each_variable<'a>(
    dae: &'a Dae,
    mut visit: impl FnMut(DaeVariablePartition, &'a rumoca_ir_dae::Variable),
) {
    use DaeVariablePartition as P;
    let partitions = [
        (P::Input, &dae.variables.inputs),
        (P::Output, &dae.variables.outputs),
        (P::Parameter, &dae.variables.parameters),
        (P::Constant, &dae.variables.constants),
        (P::DiscreteReal, &dae.variables.discrete_reals),
        (P::DiscreteValued, &dae.variables.discrete_valued),
        (P::Algebraic, &dae.variables.algebraics),
        (P::State, &dae.variables.states),
    ];
    for (partition, variables) in partitions {
        for variable in variables.values() {
            visit(partition, variable);
        }
    }
}
