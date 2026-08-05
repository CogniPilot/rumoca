use super::*;

pub(super) fn lower_call_expression<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    expression: &Expression,
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let Expression::FunctionCall {
        name,
        args,
        is_constructor,
        span,
    } = expression
    else {
        unreachable!("call lowering is selected from a function call")
    };
    if !*is_constructor
        && let Some(conversion) = enumeration_conversion(symbols.functions.flat, name, args, *span)
            .expect("analysis accepts every enumeration conversion it lowers")
    {
        return construction.expressions(|expressions| {
            expressions
                .at(provenance)
                .enumeration_literal(conversion.ordinal)
        });
    }
    match classify_function_call(*is_constructor) {
        FunctionCallLowering::Constructor | FunctionCallLowering::Registry => lower_function_call(
            construction,
            symbols,
            binders,
            name,
            args,
            *is_constructor,
            provenance,
        ),
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(in crate::construction) enum FunctionCallLowering {
    Constructor,
    Registry,
}

pub(in crate::construction) fn classify_function_call(
    is_constructor: bool,
) -> FunctionCallLowering {
    if is_constructor {
        FunctionCallLowering::Constructor
    } else {
        FunctionCallLowering::Registry
    }
}

pub(super) struct RangeInput<'expression> {
    expression: &'expression Expression,
    provenance: dae::DaeProvenance,
    generated_root: Option<dae::DaeGeneration>,
}

impl<'expression> RangeInput<'expression> {
    pub(super) const fn new(
        expression: &'expression Expression,
        provenance: dae::DaeProvenance,
        generated_root: Option<dae::DaeGeneration>,
    ) -> Self {
        Self {
            expression,
            provenance,
            generated_root,
        }
    }
}

pub(super) fn lower_range<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    input: RangeInput<'_>,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let Expression::Range {
        start, step, end, ..
    } = input.expression
    else {
        unreachable!("range lowering is selected from a range expression")
    };
    if enumeration_range_type(start, step.as_deref(), end, &|name| {
        is_flat_enumeration_literal(symbols.functions.flat, name)
    })
    .is_some()
    {
        return lower_enumeration_range(construction, symbols, start, end, input.provenance);
    }
    let mut bound =
        |bound: &Expression| lower_range_bound(construction, symbols, binders, bound, &input);
    let start = bound(start)?;
    let explicit_step = step.as_deref().map(&mut bound).transpose()?;
    let end = bound(end)?;
    construction.expressions(|expressions| {
        expressions
            .at(input.provenance)
            .range(start, explicit_step, end)
    })
}

/// Lower one compact-range bound, folding it when the scope proves its value.
///
/// A value-proven function specialization settles its inputs (MLS §12.2), so a
/// bound like `integer(m/2)` denotes one Integer here. Emitting that Integer —
/// rather than a read of the input coordinate — is what keeps the range the
/// statically sized owner the analysis admitted it as. The admitting analysis
/// reads `proven_extent` through the *same* scoped environment this lowering
/// does, including the loop binders that shadow it, so the two cannot disagree
/// about which bounds are static.
///
/// The folded literal keeps the bound's own span: the value replaces what the
/// source wrote at that occurrence, and a diagnostic about the extent must point
/// there rather than at the whole range.
fn lower_range_bound<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    bound: &Expression,
    input: &RangeInput<'_>,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    if !matches!(bound, Expression::Literal { .. })
        && let Some(value) = symbols.shapes.proven_extent(bound)
    {
        let provenance = bound
            .span()
            .filter(|span| !span.is_dummy())
            .map_or(Ok(input.provenance), dae::DaeProvenance::source)?;
        return construction.expressions(|expressions| {
            expressions
                .at(provenance)
                .literal(dae::DaeLiteral::Integer(value))
        });
    }
    lower_expression_scoped(construction, symbols, binders, bound, input.generated_root)
}

/// Lower `E.first : E.last` to the array of enumeration values it denotes.
///
/// MLS §10.4.1 gives an enumeration range the values from the first bound to
/// the second, so the canonical DAE owner is the enumeration-valued array whose
/// elements are exactly those ordinals. The compact `Range` node stays Integer:
/// its bounds are Integer literals by construction, and an enumeration range
/// carries no step to lower into one.
fn lower_enumeration_range<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    start: &Expression,
    end: &Expression,
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let (first, last) = enumeration_range_ordinals(symbols.functions.flat, start, end).ok_or(
        dae::DaeConstructionError::InvalidRangeBound {
            span: provenance.span(),
        },
    )?;
    let mut elements = Vec::new();
    for ordinal in first..=last {
        elements.push(
            construction.expressions(|expressions| {
                expressions.at(provenance).enumeration_literal(ordinal)
            })?,
        );
    }
    construction.expressions(|expressions| expressions.at(provenance).array(elements))
}

pub(super) fn lower_delay<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    arguments: &[Expression],
    provenance: dae::DaeProvenance,
    span: Span,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let plan = symbols
        .functions
        .delay_plans
        .get(&span)
        .copied()
        .ok_or(dae::DaeConstructionError::InvalidPositiveParameter { span })?;
    let source = lower_expression_scoped(construction, symbols, binders, &arguments[0], None)?;
    let delay_time = lower_expression_scoped(construction, symbols, binders, &arguments[1], None)?;
    match plan {
        DelayPlan::Fixed(timing) => {
            let timing_provenance = expression_provenance(timing.provenance(), None)?;
            let positive = construction.temporal(|temporal| {
                temporal.positive_parameter(delay_time, timing.value(), timing_provenance)
            })?;
            construction.expressions(|expressions| {
                expressions
                    .at(provenance)
                    .delay(source, positive, provenance)
            })
        }
        DelayPlan::Bounded(maximum) => {
            let delay_max =
                lower_expression_scoped(construction, symbols, binders, &arguments[2], None)?;
            let maximum_provenance = expression_provenance(maximum.provenance(), None)?;
            let maximum = construction.temporal(|temporal| {
                temporal.positive_parameter(delay_max, maximum.value(), maximum_provenance)
            })?;
            construction.expressions(|expressions| {
                expressions
                    .at(provenance)
                    .bounded_delay(source, delay_time, maximum, provenance)
            })
        }
    }
    .map(|delay| delay.expression())
}

pub(super) fn lower_hold<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    arguments: &[Expression],
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let [value] = arguments else {
        return Err(dae::DaeConstructionError::InvalidArity {
            expected: 1,
            found: arguments.len(),
            span: provenance.span(),
        });
    };
    lower_temporal_identity(construction, symbols, binders, value, provenance)
}

/// The MLS §3.7.4.5 operator `semiLinear(x, positiveSlope, negativeSlope)`.
///
/// MLS defines the operator to return
/// `smooth(0, if x >= 0 then positiveSlope*x else negativeSlope*x)`, so the
/// checked DAE owns exactly that conditional. Both linear segments multiply the
/// *same* lowered operand node, so the branch the conditional selects and the
/// value it scales are one expression identity, and the result is continuous
/// across `x = 0`.
///
/// The `smooth(0, ...)` of the definition is why the `x >= 0` relation stays a
/// pure branch selector instead of owning a state event: MLS §3.7.5 leaves a
/// tool free not to generate events under `smooth` (it grants a freedom, it
/// does not mandate suppression — only `noEvent` says "shall"), rumoca elects
/// to take that freedom here, the operator's residual is C0-continuous so
/// no crossing has to be located to keep it continuous, and OMC likewise emits
/// no zero crossing for the operator. Event-owner analysis therefore keys no
/// plan on this call, exactly as it does for a source-written `smooth`.
///
/// The MLS §3.7.4.5 "Rule 1"/"Rule 2" transformations for a set of `semiLinear`
/// equations that becomes underdetermined at `x = 0` rewrite a *set* of
/// equations, not this expression, so they are owned one level up by
/// [`analysis::SemiLinearRules`]: analysis proves the rule shape over the model
/// equation rows and equation lowering builds the replacement residual. A row
/// that reaches this function is therefore either untouched by the rules or is
/// the rules' own surviving `y = semiLinear(x, sa, sb)`.
pub(super) fn lower_semi_linear<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    arguments: &[Expression],
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let [x, positive_slope, negative_slope] = arguments else {
        return Err(dae::DaeConstructionError::InvalidArity {
            expected: 3,
            found: arguments.len(),
            span: provenance.span(),
        });
    };
    let generated =
        dae::DaeProvenance::generated(dae::DaeGeneration::SemiLinearLowering, provenance.span())?;
    let x = lower_expression_scoped(construction, symbols, binders, x, None)?;
    let positive_slope =
        lower_expression_scoped(construction, symbols, binders, positive_slope, None)?;
    let negative_slope =
        lower_expression_scoped(construction, symbols, binders, negative_slope, None)?;
    let zero = construction.expressions(|expressions| {
        expressions
            .at(generated)
            .literal(dae::DaeLiteral::Real(0.0))
    })?;
    let nonnegative = construction.expressions(|expressions| {
        expressions
            .at(generated)
            .binary(dae::BinaryOperator::GreaterEqual, x, zero)
    })?;
    let positive = construction.expressions(|expressions| {
        expressions
            .at(generated)
            .binary(dae::BinaryOperator::Multiply, positive_slope, x)
    })?;
    let negative = construction.expressions(|expressions| {
        expressions
            .at(generated)
            .binary(dae::BinaryOperator::Multiply, negative_slope, x)
    })?;
    construction.expressions(|expressions| {
        expressions
            .at(provenance)
            .conditional([(nonnegative, positive)], negative)
    })
}

pub(super) fn lower_previous<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    arguments: &[Expression],
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let [value] = arguments else {
        return Err(dae::DaeConstructionError::InvalidArity {
            expected: 1,
            found: arguments.len(),
            span: provenance.span(),
        });
    };
    let (name, subscripts) =
        derivative_reference(value).ok_or(dae::DaeConstructionError::InvalidClockedOperand {
            operator: "previous",
            span: provenance.span(),
        })?;
    let clock =
        symbols
            .owner_clock
            .ok_or(dae::DaeConstructionError::MissingPreviousClockOwner {
                span: provenance.span(),
            })?;
    let coordinate = symbols
        .coordinates
        .get(name.var_name())
        .copied()
        .ok_or_else(|| dae::DaeConstructionError::InvalidVariableRole {
            name: name.var_name().clone(),
            span: provenance.span(),
        })?;
    let previous = construction.temporal(|temporal| match coordinate {
        Coordinate::DiscreteReal(variable) => {
            temporal.previous_discrete_real(clock.into(), variable, provenance)
        }
        Coordinate::DiscreteValue(variable) => {
            temporal.previous_discrete_value(clock.into(), variable, provenance)
        }
        _ => Err(dae::DaeConstructionError::InvalidVariableRole {
            name: name.var_name().clone(),
            span: provenance.span(),
        }),
    })?;
    lower_coordinate_reference(
        construction,
        symbols,
        binders,
        dae::CoordinateInput::Previous(previous),
        subscripts,
        provenance,
    )
}

/// The sampled value of a clocked value sample, MLS §16.3.
///
/// `sample(u)` infers its clock from the partition; `sample(u, c)` names it.
/// Clock analysis proves that a named clock is the partition owner, so both
/// forms lower to the temporal identity of `u`. The two-argument Boolean event
/// operator `sample(start, interval)` never names a `Clock` coordinate and so
/// is not a value sample.
pub(super) fn clocked_value_sample<'expression>(
    flat: &flat::Model,
    arguments: &'expression [Expression],
) -> Option<&'expression Expression> {
    match arguments {
        [value] => Some(value),
        [value, clock] if is_whole_clock_coordinate(flat, clock) => Some(value),
        _ => None,
    }
}

/// The MLS §3.7.5 Boolean event operator `sample(start, interval)`.
///
/// Its ticks are a periodic schedule, not a zero crossing, so the checked DAE
/// owns them as a periodic clock and the expression reads the activation of
/// that clock's condition. Analysis proved the exact lattice, so a plan is
/// missing only when the call is neither a clocked value sample nor a
/// two-argument event operator.
pub(super) fn lower_sample_event_operator<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    arguments: &[Expression],
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let span = provenance.span();
    let operands: Vec<&Expression> = arguments.iter().collect();
    let Some(ExpressionEventPlan::SampleClock(schedule)) =
        symbols.functions.expression_events.plan(span, &operands)
    else {
        return Err(dae::DaeConstructionError::InvalidExpressionForm { span });
    };
    let clock = symbols.functions.clocks.sample_id(schedule, span)?;
    let condition = construction.conditions(|conditions| conditions.reserve(provenance))?;
    construction.conditions(|conditions| {
        conditions.define(
            condition,
            dae::ConditionInput::Clock(clock.into()),
            provenance,
        )
    })?;
    construction.expressions(|expressions| {
        expressions
            .at(provenance)
            .coordinate(dae::CoordinateInput::Condition(condition))
    })
}

pub(super) fn lower_temporal_identity<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    value: &Expression,
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    if let Some((name, subscripts)) = derivative_reference(value) {
        return lower_variable_reference(
            construction,
            symbols,
            binders,
            name,
            subscripts,
            provenance,
        );
    }
    lower_expression_scoped(construction, symbols, binders, value, None)
}

pub(super) fn lower_record_array_field_access<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    expression: &Expression,
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let fields = &symbols.functions.record_array_fields;
    if let Some(plan) = fields.structural(expression) {
        let Expression::FieldAccess { base, .. } = expression else {
            unreachable!("structural field plans are keyed only by field access")
        };
        let base = lower_expression_scoped(construction, symbols, binders, base, None)?;
        let ordinal = construction.expressions(|expressions| {
            expressions.record_field_ordinal(base, &plan.name, provenance)
        })?;
        if ordinal != Some(plan.ordinal) {
            return Err(dae::DaeConstructionError::InvalidExpressionForm {
                span: provenance.span(),
            });
        }
        return construction
            .expressions(|expressions| expressions.at(provenance).field(base, plan.ordinal));
    }
    let plan = fields
        .get(expression)
        .expect("analysis certifies every lowered record-array field projection");
    let (coordinates, subscripts) = match plan {
        RecordArrayFieldPlan::MaterializedCoordinate { coordinate, .. } => {
            let coordinate = exact_model_coordinate(symbols, *coordinate, provenance.span())?;
            return construction.expressions(|expressions| {
                expressions.at(provenance).coordinate(coordinate.current())
            });
        }
        RecordArrayFieldPlan::Projection {
            coordinates,
            subscripts,
            ..
        } => (coordinates, subscripts),
    };
    let generated = dae::DaeProvenance::generated(
        dae::DaeGeneration::RecordEquationProjection,
        provenance.span(),
    )?;
    let elements = coordinates
        .iter()
        .map(|coordinate| {
            let coordinate = exact_model_coordinate(symbols, *coordinate, generated.span())?;
            construction.expressions(|expressions| {
                expressions.at(generated).coordinate(coordinate.current())
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    let base = construction.expressions(|expressions| expressions.at(generated).array(elements))?;
    lower_index(construction, symbols, binders, base, subscripts, provenance)
}
