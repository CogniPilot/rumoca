//! GALEC admissibility checks over an immutable checked DAE.

use rumoca_ir_dae as dae;

use crate::diagnostic::GalecTargetError;
use crate::input::GalecInput;

/// One source clock scheduled on the projected block's fixed base period.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AdmittedClockDomain {
    pub clock_index: u32,
    pub divisor: u32,
}

/// The fixed base period and exactly commensurate source-clock schedule.
#[derive(Debug, Clone, PartialEq)]
pub struct AdmittedClock {
    pub period_seconds: f64,
    pub phase_seconds: f64,
    pub domains: Vec<AdmittedClockDomain>,
}

/// Inspect checked DAE ownership directly and collect every projection-scope
/// rejection. No preparation pass may erase semantics before this check.
pub fn check_admissibility(input: &GalecInput<'_>) -> Result<AdmittedClock, Vec<GalecTargetError>> {
    input.dae.inspect(|view| {
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        check_view(view, &definitions)
    })
}

pub(crate) fn check_view<'dae>(
    view: dae::DaeView<'dae>,
    definitions: &rumoca_phase_structural::CausalDefinitions<'dae>,
) -> Result<AdmittedClock, Vec<GalecTargetError>> {
    let mut errors = projection_errors(view, definitions);
    let periodic = periodic_clocks(view);
    if periodic.is_empty() {
        errors.push(GalecTargetError::NoPeriodicClock);
    }
    if !errors.is_empty() {
        return Err(errors);
    }
    admit_clock_lattice(view, &periodic).map_err(|error| vec![error])
}

fn projection_errors<'dae>(
    view: dae::DaeView<'dae>,
    definitions: &rumoca_phase_structural::CausalDefinitions<'dae>,
) -> Vec<GalecTargetError> {
    let mut errors = Vec::new();
    let (states, first_state_span) = view.variables().fold(
        (0usize, None),
        |(scalar_count, first_span), (_, variable)| {
            if variable.role() == dae::VariableRole::State {
                (
                    scalar_count + variable.scalar_count(),
                    first_span.or_else(|| Some(variable.declaration().span())),
                )
            } else {
                (scalar_count, first_span)
            }
        },
    );
    let equations = continuous_scalar_rows(view, definitions);
    if states != 0 || equations != 0 {
        let span = first_state_span.or_else(|| {
            view.continuous_equation(0)
                .map(|equation| equation.provenance().span())
                .or_else(|| {
                    view.continuous_family(0)
                        .map(|family| family.provenance().span())
                })
        });
        errors.push(GalecTargetError::ContinuousDynamics {
            states,
            equations,
            span,
        });
    }
    let initial_equations = initialization_scalar_rows(view);
    if initial_equations != 0 {
        errors.push(GalecTargetError::InitialEquations {
            equations: initial_equations,
            structured_families: view.initialization_family_count(),
        });
    }
    if view.initial_discrete_value_count() != 0 {
        errors.push(GalecTargetError::InitialDiscreteValues {
            definitions: view.initial_discrete_value_count(),
        });
    }
    if view.time_event_count() != 0 {
        errors.push(GalecTargetError::RuntimeEvents {
            scheduled_time_events: view.time_event_count(),
            event_actions: 0,
        });
    }
    for (_, transaction) in view.model_event_transactions() {
        errors.push(GalecTargetError::UnsupportedFeature {
            feature: "model-event-transaction".to_owned(),
            detail: "GALEC transaction-owner lowering is not implemented; the derived B.1b and B.1c views cannot execute independently"
                .to_owned(),
            span: Some(transaction.provenance().span()),
        });
    }
    let dynamic = view
        .clocks()
        .filter(|(_, clock)| {
            matches!(clock.operation(), dae::ClockOperation::Triggered(_))
                || matches!(
                    clock.operation(),
                    dae::ClockOperation::Periodic(schedule)
                        if schedule.anchor() == rumoca_core::ClockPhaseAnchor::SimulationStart
                )
        })
        .count();
    if dynamic != 0 {
        errors.push(GalecTargetError::DynamicClock { count: dynamic });
    }
    errors
}

fn admit_clock_lattice(
    view: dae::DaeView<'_>,
    periodic: &[(u32, &rumoca_core::PeriodicClockSchedule)],
) -> Result<AdmittedClock, GalecTargetError> {
    let Some(base) = periodic
        .iter()
        .min_by_key(|(_, schedule)| schedule.period())
    else {
        unreachable!("an admitted projection has at least one periodic clock")
    };
    let domains = periodic
        .iter()
        .map(|(clock_index, schedule)| {
            let ratio = schedule
                .period()
                .checked_div(base.1.period())
                .map_err(|error| GalecTargetError::UnsupportedFeature {
                    feature: "clock-lattice".to_owned(),
                    detail: error.to_string(),
                    span: view
                        .clock(
                            view.clock_id(*clock_index as usize)
                                .expect("clock index resolves"),
                        )
                        .map(|clock| clock.provenance().span()),
                })?;
            let divisor = u32::try_from(ratio.numerator())
                .ok()
                .filter(|_| ratio.denominator() == 1)
                .ok_or_else(|| GalecTargetError::UnsupportedFeature {
                    feature: "incommensurate-clock".to_owned(),
                    detail: format!(
                        "period {} s is not an integer multiple of base period {} s",
                        schedule.period_seconds(),
                        base.1.period_seconds()
                    ),
                    span: view
                        .clock(
                            view.clock_id(*clock_index as usize)
                                .expect("clock index resolves"),
                        )
                        .map(|clock| clock.provenance().span()),
                })?;
            Ok(AdmittedClockDomain {
                clock_index: *clock_index,
                divisor,
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok(AdmittedClock {
        period_seconds: base.1.period_seconds(),
        phase_seconds: base.1.phase_seconds(),
        domains,
    })
}

fn continuous_scalar_rows<'dae>(
    view: dae::DaeView<'dae>,
    definitions: &rumoca_phase_structural::CausalDefinitions<'dae>,
) -> usize {
    view.continuous_owners()
        .map(|owner| match owner {
            dae::ContinuousOwnerView::Residual { id, .. } if definitions.consumes(id) => 0,
            dae::ContinuousOwnerView::Residual { equation, .. } => view
                .expression(equation.residual())
                .expect("checked residual resolves")
                .value_type()
                .scalar_count()
                .expect("checked scalar capacity"),
            dae::ContinuousOwnerView::Structured { id, .. } if definitions.consumes_family(id) => 0,
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

fn periodic_clocks(view: dae::DaeView<'_>) -> Vec<(u32, &rumoca_core::PeriodicClockSchedule)> {
    view.clocks()
        .filter_map(|(id, clock)| match clock.operation() {
            dae::ClockOperation::Periodic(schedule)
                if schedule.anchor() == rumoca_core::ClockPhaseAnchor::Absolute =>
            {
                Some((id.index(), schedule))
            }
            dae::ClockOperation::Periodic(_) | dae::ClockOperation::Triggered(_) => None,
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use rumoca_core::{ClockLattice, ClockRational, SourceMap, Span, TypeId, VarName};

    use super::*;

    #[test]
    fn checked_dae_without_required_clock_fails_early() {
        let dae = dae::Dae::construct(SourceMap::new(), |_| Ok(())).unwrap();
        let input = GalecInput::new(&dae, "Empty");
        let errors = check_admissibility(&input).unwrap_err();
        assert!(matches!(
            errors.as_slice(),
            [GalecTargetError::NoPeriodicClock]
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
                    rumoca_core::InstanceId::new(1),
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

    /// MLS §8.6: an algorithm-determined discrete initial value is a checked
    /// DAE owner GALEC Startup has no lowering for. It initializes from `start`
    /// attributes only, so admitting the model would run the block from the
    /// declared `start` instead of the determined value.
    #[test]
    fn checked_discrete_initial_value_is_never_ignored() {
        let mut sources = SourceMap::new();
        let source = sources.add("galec.mo", "discrete Real m; initial algorithm m := 1.0;");
        let declaration = dae::DaeProvenance::source(Span::from_offsets(source, 0, 15)).unwrap();
        let owner = dae::DaeProvenance::source(Span::from_offsets(source, 35, 43)).unwrap();
        let model = dae::Dae::construct(sources, |model| {
            let real = model.types(|types| {
                types.intern(
                    TypeId::new(0),
                    dae::ValueType::scalar(dae::ScalarType::Real),
                    declaration,
                )
            })?;
            let m = model.variables(|variables| {
                variables.discrete_real(
                    VarName::new("m"),
                    rumoca_core::InstanceId::new(2),
                    real,
                    declaration,
                    dae::VariableAttributes::default(),
                )
            })?;
            let value = model.expressions(|expressions| {
                expressions.at(owner).literal(dae::DaeLiteral::Real(1.0))
            })?;
            model.initialization(|initialization| {
                initialization
                    .discrete_real_initial_value(m, value, owner)
                    .map(|_| ())
            })
        })
        .unwrap();
        let errors = check_admissibility(&GalecInput::new(&model, "DiscreteInitial")).unwrap_err();
        assert!(
            errors.iter().any(|error| matches!(
                error,
                GalecTargetError::InitialDiscreteValues { definitions: 1 }
            )),
            "an algorithm-determined discrete initial value must be reported: {errors:?}"
        );
    }

    #[derive(Clone, Copy)]
    struct TransactionFixtureSpans {
        z: dae::DaeProvenance,
        valid: dae::DaeProvenance,
        sample: dae::DaeProvenance,
        owner: dae::DaeProvenance,
        z_definition: dae::DaeProvenance,
        valid_definition: dae::DaeProvenance,
    }

    struct TransactionFixtureValues<'dae> {
        z: dae::DiscreteRealId<'dae>,
        valid: dae::DiscreteValueId<'dae>,
        clock: dae::ClockId<'dae>,
        guard: dae::ConditionId<'dae>,
        z_value: dae::ExprId<'dae>,
        valid_value: dae::ExprId<'dae>,
    }

    fn add_transaction_views<'dae>(
        model: &mut dae::DaeConstruction<'dae>,
        spans: TransactionFixtureSpans,
        values: TransactionFixtureValues<'dae>,
    ) -> Result<(), dae::DaeConstructionError> {
        model.discrete(|discrete| {
            discrete.when_real_equation(
                values.guard,
                values.guard,
                spans.z_definition,
                |equation| equation.residual(values.z_value),
            )
        })?;
        model.b1c([values.valid], |topology| {
            topology.owner(spans.owner, [values.valid], |owner| {
                owner.when(
                    values.guard,
                    values.guard,
                    spans.sample,
                    [(values.valid_value, spans.valid_definition)],
                )
            })?;
            Ok(())
        })?;
        model.model_events(|events| {
            events.transaction(
                [
                    dae::ModelEventTarget::DiscreteReal(values.z),
                    dae::ModelEventTarget::DiscreteValue(values.valid),
                ],
                [dae::ModelEventStep::new(
                    values.guard,
                    values.guard,
                    Some(values.clock),
                    [
                        dae::ModelEventDefinition::new(
                            dae::ModelEventTarget::DiscreteReal(values.z),
                            values.z_value,
                            spans.z_definition,
                        ),
                        dae::ModelEventDefinition::new(
                            dae::ModelEventTarget::DiscreteValue(values.valid),
                            values.valid_value,
                            spans.valid_definition,
                        ),
                    ],
                    spans.owner,
                )],
                spans.owner,
            )
        })?;
        Ok(())
    }

    fn construct_transaction_fixture<'dae>(
        model: &mut dae::DaeConstruction<'dae>,
        spans: TransactionFixtureSpans,
    ) -> Result<(), dae::DaeConstructionError> {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                spans.z,
            )
        })?;
        let boolean = model.types(|types| {
            types.intern(
                TypeId::new(1),
                dae::ValueType::scalar(dae::ScalarType::Boolean),
                spans.valid,
            )
        })?;
        let (z, valid) = model.variables(|variables| {
            Ok((
                variables.discrete_real(
                    VarName::new("z"),
                    rumoca_core::InstanceId::new(3),
                    real,
                    spans.z,
                    dae::VariableAttributes::default(),
                )?,
                variables.discrete_value(
                    VarName::new("valid"),
                    rumoca_core::InstanceId::new(4),
                    boolean,
                    spans.valid,
                    dae::VariableAttributes::default(),
                )?,
            ))
        })?;
        let clock = model.clocks(|clocks| {
            let clock = clocks.periodic(
                ClockLattice::new(ClockRational::ONE, ClockRational::ZERO)
                    .expect("fixture lattice is exact"),
                spans.sample,
            )?;
            clocks.own_discrete_real(clock.into(), z, spans.z_definition)?;
            clocks.own_discrete_value(clock.into(), valid, spans.valid_definition)?;
            Ok(clock)
        })?;
        let guard = model.conditions(|conditions| conditions.reserve(spans.sample))?;
        model.conditions(|conditions| {
            conditions.define(
                guard,
                dae::ConditionInput::Clock(clock.into()),
                spans.sample,
            )
        })?;
        let (z_value, valid_value) = model.expressions(|expressions| {
            Ok((
                expressions
                    .at(spans.z_definition)
                    .literal(dae::DaeLiteral::Real(1.0))?,
                expressions
                    .at(spans.valid_definition)
                    .literal(dae::DaeLiteral::Boolean(true))?,
            ))
        })?;
        add_transaction_views(
            model,
            spans,
            TransactionFixtureValues {
                z,
                valid,
                clock: clock.into(),
                guard,
                z_value,
                valid_value,
            },
        )
    }

    #[test]
    fn checked_model_event_transaction_is_refused_at_its_owner() {
        let text = "discrete Real z; discrete Boolean valid; when sample(0, 1) then z := 1; valid := true; end when;";
        let mut sources = SourceMap::new();
        let source = sources.add("galec-model-event-transaction.mo", text);
        let provenance = |needle: &str| {
            let start = text.find(needle).expect("fixture snippet exists");
            dae::DaeProvenance::source(Span::from_offsets(source, start, start + needle.len()))
                .expect("fixture provenance is source-backed")
        };
        let spans = TransactionFixtureSpans {
            z: provenance("discrete Real z"),
            valid: provenance("discrete Boolean valid"),
            sample: provenance("sample(0, 1)"),
            owner: provenance("when sample(0, 1) then z := 1; valid := true; end when"),
            z_definition: provenance("z := 1"),
            valid_definition: provenance("valid := true"),
        };
        let model =
            dae::Dae::construct(sources, |model| construct_transaction_fixture(model, spans))
                .expect("checked mixed-role model-event transaction constructs");

        let errors = check_admissibility(&GalecInput::new(&model, "ModelEvent")).unwrap_err();
        assert!(errors.iter().any(|error| matches!(
            error,
            GalecTargetError::UnsupportedFeature {
                feature,
                span: Some(span),
                ..
            } if feature == "model-event-transaction" && *span == spans.owner.span()
        )));
    }
}
