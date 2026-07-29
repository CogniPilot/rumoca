//! GALEC admissibility checks over an immutable checked DAE.

use rumoca_ir_dae as dae;

use crate::diagnostic::GalecTargetError;
use crate::input::GalecInput;

/// The single fixed-period clock a projected block runs on.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct AdmittedClock {
    pub period_seconds: f64,
    pub phase_seconds: f64,
}

/// Inspect checked DAE ownership directly and collect every projection-scope
/// rejection. No preparation pass may erase semantics before this check.
pub fn check_admissibility(input: &GalecInput<'_>) -> Result<AdmittedClock, Vec<GalecTargetError>> {
    input.dae.inspect(check_view)
}

fn check_view(view: dae::DaeView<'_>) -> Result<AdmittedClock, Vec<GalecTargetError>> {
    let mut errors = Vec::new();
    let states = view
        .variables()
        .filter(|(_, variable)| variable.role() == dae::VariableRole::State)
        .map(|(_, variable)| variable.scalar_count())
        .sum::<usize>();
    let equations = continuous_scalar_rows(view);
    if states != 0 || equations != 0 {
        errors.push(GalecTargetError::ContinuousDynamics { states, equations });
    }
    let initial_equations = initialization_scalar_rows(view);
    if initial_equations != 0 {
        errors.push(GalecTargetError::InitialEquations {
            equations: initial_equations,
            structured_families: view.initialization_family_count(),
        });
    }
    if view.time_event_count() != 0 {
        errors.push(GalecTargetError::RuntimeEvents {
            scheduled_time_events: view.time_event_count(),
            event_actions: 0,
        });
    }
    let triggered = (0..view.clock_count())
        .filter(|index| {
            let id = view.clock_id(*index).expect("dense checked clock identity");
            matches!(
                view.clock(id).expect("checked clock resolves").operation(),
                dae::ClockOperation::Triggered(_)
            )
        })
        .count();
    if triggered != 0 {
        errors.push(GalecTargetError::DynamicClock { count: triggered });
    }
    let periodic = periodic_clocks(view);
    if periodic.len() != 1 {
        errors.push(GalecTargetError::ClockCountNotOne {
            count: periodic.len(),
        });
    }
    if !errors.is_empty() {
        return Err(errors);
    }
    let (period, phase) = periodic[0];
    Ok(AdmittedClock {
        period_seconds: period,
        phase_seconds: phase,
    })
}

fn continuous_scalar_rows(view: dae::DaeView<'_>) -> usize {
    let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
    view.continuous_owners()
        .map(|owner| match owner {
            dae::ContinuousOwnerView::Residual { id, .. } if definitions.consumes(id) => 0,
            dae::ContinuousOwnerView::Residual { equation, .. } => view
                .expression(equation.residual())
                .expect("checked residual resolves")
                .value_type()
                .scalar_count()
                .expect("checked scalar capacity"),
            dae::ContinuousOwnerView::Structured { family, .. } => family.scalar_rows() as usize,
        })
        .sum()
}

fn initialization_scalar_rows(view: dae::DaeView<'_>) -> usize {
    view.initialization_owners()
        .map(|owner| match owner {
            dae::InitializationOwnerView::Residual { equation, .. } => view
                .expression(equation.residual())
                .expect("checked residual resolves")
                .value_type()
                .scalar_count()
                .expect("checked scalar capacity"),
            dae::InitializationOwnerView::Structured { family, .. } => {
                family.scalar_rows() as usize
            }
        })
        .sum()
}

fn periodic_clocks(view: dae::DaeView<'_>) -> Vec<(f64, f64)> {
    (0..view.clock_count())
        .filter_map(|index| {
            let id = view.clock_id(index).expect("dense checked clock identity");
            match view.clock(id).expect("checked clock resolves").operation() {
                dae::ClockOperation::Periodic(lattice) => {
                    Some((lattice.period_seconds(), lattice.phase_seconds()))
                }
                dae::ClockOperation::Triggered(_) => None,
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use rumoca_core::{SourceMap, Span, TypeId, VarName};

    use super::*;

    #[test]
    fn checked_dae_without_required_clock_fails_early() {
        let dae = dae::Dae::construct(SourceMap::new(), |_| Ok(())).unwrap();
        let input = GalecInput::new(&dae, "Empty");
        let errors = check_admissibility(&input).unwrap_err();
        assert!(matches!(
            errors.as_slice(),
            [GalecTargetError::ClockCountNotOne { count: 0 }]
        ));
    }

    #[test]
    fn checked_continuous_owner_is_never_ignored() {
        let mut sources = SourceMap::new();
        let source = sources.add("galec.mo", "Real x; x = 0;");
        let declaration = dae::DaeProvenance::source(Span::from_offsets(source, 0, 6)).unwrap();
        let equation = dae::DaeProvenance::source(Span::from_offsets(source, 8, 13)).unwrap();
        let model = dae::Dae::construct(sources, |model| {
            let real = model.types(|types| {
                types.intern(
                    TypeId::new(0),
                    dae::ValueType::scalar(dae::ScalarType::Real),
                    declaration,
                )
            })?;
            let x = model.variables(|variables| {
                variables.algebraic(
                    VarName::new("x"),
                    real,
                    declaration,
                    dae::VariableAttributes::default(),
                )
            })?;
            let residual = model.expressions(|expressions| {
                expressions
                    .at(equation)
                    .coordinate(dae::CoordinateInput::Algebraic(x))
            })?;
            model.continuous(|continuous| continuous.value_equation(equation, residual))
        })
        .unwrap();
        let errors = check_admissibility(&GalecInput::new(&model, "Continuous")).unwrap_err();
        assert!(errors.iter().any(|error| matches!(
            error,
            GalecTargetError::ContinuousDynamics { equations: 1, .. }
        )));
    }
}
