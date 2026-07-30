use super::*;

pub(super) fn lower_expression<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    expression: &Expression,
    generated_root: Option<dae::DaeGeneration>,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let symbols = LoweringSymbols {
        coordinates,
        functions,
        shapes: functions.shapes.model_values(),
        function_body: None,
        values: None,
        owner_clock: None,
    };
    lower_expression_scoped(
        construction,
        symbols,
        &HashMap::new(),
        expression,
        generated_root,
    )
}

#[derive(Clone, Copy)]
pub(super) struct LoweringSymbols<'symbols, 'dae> {
    pub(super) coordinates: &'symbols HashMap<VarName, Coordinate<'dae>>,
    pub(super) functions: &'symbols FunctionRegistry<'symbols, 'dae>,
    pub(super) shapes: &'symbols ShapeEnvironment,
    pub(super) function_body: Option<&'symbols dae::FunctionBody<'dae>>,
    pub(super) values: Option<&'symbols HashMap<VarName, dae::ExprId<'dae>>>,
    pub(super) owner_clock: Option<dae::PeriodicClockId<'dae>>,
}

pub(super) fn lower_clocked_expression<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    owner_clock: dae::PeriodicClockId<'dae>,
    expression: &Expression,
    generated_root: Option<dae::DaeGeneration>,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    lower_expression_scoped(
        construction,
        LoweringSymbols {
            coordinates,
            functions,
            shapes: functions.shapes.model_values(),
            function_body: None,
            values: None,
            owner_clock: Some(owner_clock),
        },
        &HashMap::new(),
        expression,
        generated_root,
    )
}

pub(super) fn lower_model_algorithm_expression<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    values: &HashMap<VarName, dae::ExprId<'dae>>,
    expression: &Expression,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    lower_expression_scoped(
        construction,
        LoweringSymbols {
            coordinates,
            functions,
            shapes: functions.shapes.model_values(),
            function_body: None,
            values: Some(values),
            owner_clock: None,
        },
        &HashMap::new(),
        expression,
        None,
    )
}

pub(super) fn lower_function_expression<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    shapes: &ShapeEnvironment,
    body: &dae::FunctionBody<'dae>,
    expression: &Expression,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    lower_function_expression_scoped(
        construction,
        coordinates,
        functions,
        shapes,
        body,
        &HashMap::new(),
        expression,
    )
}

pub(super) fn lower_function_expression_scoped<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    shapes: &ShapeEnvironment,
    body: &dae::FunctionBody<'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    expression: &Expression,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    lower_expression_scoped(
        construction,
        LoweringSymbols {
            coordinates,
            functions,
            shapes,
            function_body: Some(body),
            values: None,
            owner_clock: None,
        },
        binders,
        expression,
        None,
    )
}

pub(super) struct FunctionArrayUpdate<'symbols, 'dae> {
    pub(super) symbols: LoweringSymbols<'symbols, 'dae>,
    pub(super) binders: &'symbols HashMap<VarName, dae::DomainBinderId<'dae>>,
    pub(super) target: dae::FunctionValueId<'dae>,
    pub(super) subscripts: &'symbols [Subscript],
    pub(super) value: dae::ExprId<'dae>,
    pub(super) provenance: dae::DaeProvenance,
}

pub(super) fn lower_function_array_update<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    input: FunctionArrayUpdate<'_, 'dae>,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let FunctionArrayUpdate {
        symbols,
        binders,
        target,
        subscripts,
        value,
        provenance,
    } = input;
    let body = symbols
        .function_body
        .expect("function array update has a semantic function owner");
    let base = construction.functions(|functions| functions.read(body, target, provenance))?;
    let subscripts = subscripts
        .iter()
        .map(|subscript| lower_subscript(construction, symbols, binders, subscript))
        .collect::<Result<Vec<_>, _>>()?;
    construction.expressions(|expressions| {
        expressions
            .at(provenance)
            .array_update(base, value, subscripts)
    })
}

pub(super) fn lower_expression_scoped<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    expression: &Expression,
    generated_root: Option<dae::DaeGeneration>,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let span = expression
        .span()
        .expect("analysis proves expression provenance");
    let lowered =
        lower_expression_node(construction, symbols, binders, expression, generated_root)?;
    lower_expression_event(construction, symbols, binders, expression, span, lowered)?;
    Ok(lowered)
}

/// Build the MLS §8.5 state-event owner of a relation that analysis proved
/// event-generating.
///
/// The relation keeps its own expression identity in `f(x)`; the checked DAE
/// additionally owns it as a `relation` with a root activation, which is the
/// Appendix B surface the solver locates crossings on. Function bodies are
/// pure and array comprehensions carry domain binders, so neither can own a
/// model event: analysis never keys a plan on their spans, and the binder
/// guard keeps a comprehension body from closing over one by accident.
fn lower_expression_event<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    expression: &Expression,
    span: Span,
    lowered: dae::ExprId<'dae>,
) -> Result<(), dae::DaeConstructionError> {
    if symbols.function_body.is_some() || !binders.is_empty() {
        return Ok(());
    }
    // Expansions such as MLS §15.3 `actualStream` build several nodes from one
    // source span, so the span alone does not name the planned owner. Only the
    // relation node itself may claim the plan.
    if !matches!(expression, Expression::Binary { op, .. } if op.is_relational()) {
        return Ok(());
    }
    if !matches!(
        symbols.functions.expression_events.plan(span),
        Some(ExpressionEventPlan::StateRelation)
    ) {
        return Ok(());
    }
    let provenance = dae::DaeProvenance::source(span)?;
    let relation =
        construction.conditions(|conditions| conditions.relation(lowered, provenance))?;
    let condition = construction.conditions(|conditions| conditions.reserve(provenance))?;
    construction.conditions(|conditions| {
        conditions.define(
            condition,
            dae::ConditionInput::Relation(relation),
            provenance,
        )
    })?;
    construction.conditions(|conditions| conditions.root(relation, condition, provenance))?;
    Ok(())
}

fn lower_expression_node<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    expression: &Expression,
    generated_root: Option<dae::DaeGeneration>,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let span = expression
        .span()
        .expect("analysis proves expression provenance");
    let provenance = expression_provenance(span, generated_root)?;
    match expression {
        Expression::Binary { op, lhs, rhs, .. } => {
            lower_binary_expression(construction, symbols, binders, op, lhs, rhs, provenance)
        }
        Expression::Unary { op, rhs, .. } => {
            lower_unary_expression(construction, symbols, binders, op, rhs, provenance)
        }
        Expression::VarRef {
            name, subscripts, ..
        } => lower_variable_reference(construction, symbols, binders, name, subscripts, provenance),
        Expression::BuiltinCall { function, args, .. } => {
            lower_builtin_expression(construction, symbols, binders, *function, args, provenance)
        }
        Expression::Literal { value, .. } => construction
            .expressions(|expressions| expressions.at(provenance).literal(lower_literal(value))),
        Expression::If {
            branches,
            else_branch,
            ..
        } => lower_conditional_expression(
            construction,
            symbols,
            binders,
            branches,
            else_branch,
            provenance,
        ),
        Expression::Array { elements, .. } => {
            lower_array_expression(construction, symbols, binders, elements, provenance)
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter: _,
            ..
        } => lower_array_comprehension(construction, symbols, binders, expr, indices, provenance),
        Expression::Range { .. } => lower_range(
            construction,
            symbols,
            binders,
            RangeInput::new(expression, provenance, generated_root),
        ),
        Expression::Index {
            base, subscripts, ..
        } => lower_index_expression(construction, symbols, binders, base, subscripts, provenance),
        Expression::FunctionCall { .. } => {
            lower_call_expression(construction, symbols, binders, expression, provenance)
        }
        Expression::StringConversion {
            declaration,
            value,
            format,
            ..
        } => lower_string_conversion(
            construction,
            symbols,
            binders,
            *declaration,
            value,
            format,
            provenance,
        ),
        Expression::FieldAccess { .. } => {
            lower_record_array_field_access(construction, symbols, binders, expression, provenance)
        }
        Expression::Tuple { .. } | Expression::Empty { .. } => {
            Err(dae::DaeConstructionError::InvalidExpressionForm { span })
        }
    }
}

fn lower_builtin_expression<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    function: BuiltinFunction,
    arguments: &[Expression],
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let span = provenance.span();
    match function {
        BuiltinFunction::Der => {
            lower_derivative(construction, symbols, binders, arguments, provenance, span)
        }
        BuiltinFunction::Pre => {
            lower_pre(construction, symbols, binders, arguments, provenance, span)
        }
        BuiltinFunction::Sample => {
            let Some(value) = clocked_value_sample(symbols.functions.flat, arguments) else {
                return lower_sample_event_operator(construction, symbols, provenance);
            };
            lower_temporal_identity(construction, symbols, binders, value, provenance)
        }
        BuiltinFunction::Hold => lower_hold(construction, symbols, binders, arguments, provenance),
        BuiltinFunction::Previous => {
            lower_previous(construction, symbols, binders, arguments, provenance)
        }
        BuiltinFunction::Interval => {
            if arguments.len() > 1 {
                return Err(dae::DaeConstructionError::InvalidArity {
                    expected: 1,
                    found: arguments.len(),
                    span,
                });
            }
            let owner_clock = symbols
                .owner_clock
                .ok_or(dae::DaeConstructionError::MissingClockDomainOwner { span })?;
            construction.expressions(|expressions| {
                expressions
                    .at(provenance)
                    .coordinate(dae::CoordinateInput::ClockInterval(owner_clock))
            })
        }
        BuiltinFunction::Clock
        | BuiltinFunction::SubSample
        | BuiltinFunction::SuperSample
        | BuiltinFunction::ShiftSample
        | BuiltinFunction::BackSample
        | BuiltinFunction::NoClock => {
            Err(dae::DaeConstructionError::InvalidExpressionForm { span })
        }
        BuiltinFunction::Delay => {
            lower_delay(construction, symbols, binders, arguments, provenance, span)
        }
        _ => lower_builtin_call(
            construction,
            symbols,
            binders,
            function,
            arguments,
            provenance,
        ),
    }
}

fn lower_index_expression<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    base: &Expression,
    subscripts: &[rumoca_core::Subscript],
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let base = lower_expression_scoped(construction, symbols, binders, base, None)?;
    lower_index(construction, symbols, binders, base, subscripts, provenance)
}

fn lower_string_conversion<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    declaration: rumoca_core::DefId,
    value: &Expression,
    format: &rumoca_core::StringConversionFormat,
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let value = lower_expression_scoped(construction, symbols, binders, value, None)?;
    let format = lower_string_conversion_format(construction, symbols, binders, format)?;
    construction.expressions(|expressions| {
        expressions
            .at(provenance)
            .string_conversion(declaration, value, format)
    })
}

fn lower_string_conversion_format<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    format: &rumoca_core::StringConversionFormat,
) -> Result<dae::StringConversionFormatInput<'dae>, dae::DaeConstructionError> {
    Ok(match format {
        rumoca_core::StringConversionFormat::Options {
            minimum_length,
            left_justified,
            significant_digits,
        } => dae::StringConversionFormatInput::Options {
            minimum_length: lower_optional_expression(
                construction,
                symbols,
                binders,
                minimum_length.as_deref(),
            )?,
            left_justified: lower_optional_expression(
                construction,
                symbols,
                binders,
                left_justified.as_deref(),
            )?,
            significant_digits: lower_optional_expression(
                construction,
                symbols,
                binders,
                significant_digits.as_deref(),
            )?,
        },
        rumoca_core::StringConversionFormat::Format { value } => {
            dae::StringConversionFormatInput::Format {
                value: lower_expression_scoped(construction, symbols, binders, value, None)?,
            }
        }
    })
}

fn lower_optional_expression<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    expression: Option<&Expression>,
) -> Result<Option<dae::ExprId<'dae>>, dae::DaeConstructionError> {
    expression
        .map(|expression| lower_expression_scoped(construction, symbols, binders, expression, None))
        .transpose()
}

fn lower_call_expression<'dae>(
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
pub(super) enum FunctionCallLowering {
    Constructor,
    Registry,
}

pub(super) fn classify_function_call(is_constructor: bool) -> FunctionCallLowering {
    if is_constructor {
        FunctionCallLowering::Constructor
    } else {
        FunctionCallLowering::Registry
    }
}

struct RangeInput<'expression> {
    expression: &'expression Expression,
    provenance: dae::DaeProvenance,
    generated_root: Option<dae::DaeGeneration>,
}

impl<'expression> RangeInput<'expression> {
    const fn new(
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

fn lower_range<'dae>(
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
    let start =
        lower_expression_scoped(construction, symbols, binders, start, input.generated_root)?;
    let explicit_step = step
        .as_deref()
        .map(|step| {
            lower_expression_scoped(construction, symbols, binders, step, input.generated_root)
        })
        .transpose()?;
    let end = lower_expression_scoped(construction, symbols, binders, end, input.generated_root)?;
    construction.expressions(|expressions| {
        expressions
            .at(input.provenance)
            .range(start, explicit_step, end)
    })
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

fn lower_delay<'dae>(
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

fn lower_hold<'dae>(
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

fn lower_previous<'dae>(
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
fn clocked_value_sample<'expression>(
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
fn lower_sample_event_operator<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let span = provenance.span();
    let Some(ExpressionEventPlan::SampleClock(lattice)) =
        symbols.functions.expression_events.plan(span)
    else {
        return Err(dae::DaeConstructionError::InvalidExpressionForm { span });
    };
    let clock = construction.clocks(|clocks| clocks.periodic(lattice, provenance))?;
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

fn lower_temporal_identity<'dae>(
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

fn lower_record_array_field_access<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    expression: &Expression,
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let plan = symbols
        .functions
        .record_array_fields
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

fn exact_model_coordinate<'dae>(
    symbols: LoweringSymbols<'_, 'dae>,
    instance: rumoca_core::InstanceId,
    span: Span,
) -> Result<Coordinate<'dae>, dae::DaeConstructionError> {
    symbols
        .functions
        .coordinate_instances
        .get(&instance)
        .copied()
        .ok_or(dae::DaeConstructionError::UnknownId {
            kind: "Flat runtime coordinate instance",
            index: instance.index(),
            span,
        })
}

fn lower_variable_reference<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    name: &rumoca_core::Reference,
    subscripts: &[Subscript],
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    if name.as_str() == "time" && subscripts.is_empty() {
        return construction.expressions(|expressions| {
            expressions
                .at(provenance)
                .coordinate(dae::CoordinateInput::Time)
        });
    }
    if let Some(binder) = binders.get(name.var_name()).copied()
        && subscripts.is_empty()
    {
        return construction.expressions(|expressions| expressions.at(provenance).binder(binder));
    }
    if let Some(value) = symbols
        .values
        .and_then(|values| values.get(name.var_name()))
        .copied()
    {
        return lower_index(
            construction,
            symbols,
            binders,
            value,
            subscripts,
            provenance,
        );
    }
    if subscripts.is_empty()
        && let Some(ordinal) = symbols
            .functions
            .flat
            .enum_literal_ordinals
            .get(name.as_str())
    {
        return construction
            .expressions(|expressions| expressions.at(provenance).enumeration_literal(*ordinal));
    }
    if symbols.function_body.is_some()
        && let Some(projected) =
            lower_function_record_projection(construction, symbols, name, provenance)?
    {
        return lower_index(
            construction,
            symbols,
            binders,
            projected,
            subscripts,
            provenance,
        );
    }
    let coordinate = symbols
        .coordinates
        .get(name.var_name())
        .copied()
        .ok_or_else(|| dae::DaeConstructionError::InvalidVariableRole {
            name: name.var_name().clone(),
            span: provenance.span(),
        })?;
    match coordinate {
        Coordinate::FunctionValue(value) => {
            let body = symbols
                .function_body
                .expect("function value analysis supplies its semantic owner");
            let base =
                construction.functions(|functions| functions.read(body, value, provenance))?;
            lower_index(construction, symbols, binders, base, subscripts, provenance)
        }
        coordinate => lower_coordinate_reference(
            construction,
            symbols,
            binders,
            coordinate.current(),
            subscripts,
            provenance,
        ),
    }
}

/// Read `value.field...` inside a function body as a checked record projection.
///
/// Flat renders a record field read as one reference whose joined `VarName` is
/// not a declared function value, but whose component reference keeps the exact
/// structure: a root declaration followed by declared field parts. The DAE
/// interns the record layout in the record constructor's declared field order
/// (`function_value_type`), which is also the order analysis proves the
/// projection names from, so the field name locates its ordinal exactly.
///
/// Returns `None` when the reference is not a projection of a record-typed
/// function value, leaving the caller's own unresolved-reference surface intact.
fn lower_function_record_projection<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    name: &rumoca_core::Reference,
    provenance: dae::DaeProvenance,
) -> Result<Option<dae::ExprId<'dae>>, dae::DaeConstructionError> {
    let Some(reference) = name.component_ref() else {
        return Ok(None);
    };
    let parts = reference.parts();
    let [root, fields @ ..] = parts else {
        return Ok(None);
    };
    if fields.is_empty() || parts.iter().any(|part| !part.subs.is_empty()) {
        return Ok(None);
    }
    let Some(coordinate) = symbols.coordinates.get(&VarName::new(&root.ident)).copied() else {
        return Ok(None);
    };
    let mut base = match coordinate {
        Coordinate::FunctionValue(value) => {
            let body = symbols
                .function_body
                .expect("record projection lowering runs inside a function body");
            construction.functions(|functions| functions.read(body, value, provenance))?
        }
        Coordinate::FunctionParameter(_) => construction.expressions(|expressions| {
            expressions.at(provenance).coordinate(coordinate.current())
        })?,
        // Model coordinates are scalar: Flat expands a model record container
        // into its field variables, so no model reference roots a projection.
        _ => return Ok(None),
    };
    for field in fields {
        let ordinal = construction.expressions(|expressions| {
            expressions.record_field_ordinal(base, &VarName::new(&field.ident), provenance)
        })?;
        let Some(ordinal) = ordinal else {
            return Err(dae::DaeConstructionError::InvalidVariableRole {
                name: name.var_name().clone(),
                span: provenance.span(),
            });
        };
        base = construction
            .expressions(|expressions| expressions.at(provenance).field(base, ordinal))?;
    }
    Ok(Some(base))
}

fn expression_provenance(
    span: Span,
    generated_root: Option<dae::DaeGeneration>,
) -> Result<dae::DaeProvenance, dae::DaeConstructionError> {
    match generated_root {
        Some(generation) => dae::DaeProvenance::generated(generation, span),
        None => dae::DaeProvenance::source(span),
    }
}

fn lower_binary_expression<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    operator: &OpBinary,
    lhs: &Expression,
    rhs: &Expression,
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let lhs = lower_expression_scoped(construction, symbols, binders, lhs, None)?;
    let rhs = lower_expression_scoped(construction, symbols, binders, rhs, None)?;
    construction.expressions(|expressions| {
        expressions
            .at(provenance)
            .binary(binary_operator(operator), lhs, rhs)
    })
}

fn lower_unary_expression<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    operator: &OpUnary,
    rhs: &Expression,
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let rhs = lower_expression_scoped(construction, symbols, binders, rhs, None)?;
    construction.expressions(|expressions| {
        expressions
            .at(provenance)
            .unary(unary_operator(operator), rhs)
    })
}

fn lower_derivative<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    arguments: &[Expression],
    provenance: dae::DaeProvenance,
    span: Span,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let (name, subscripts) =
        derivative_reference(&arguments[0]).expect("analysis proves the derivative target shape");
    let coordinate = symbols.coordinates[name.var_name()]
        .derivative(span)
        .expect("analysis proves derivative role");
    lower_coordinate_reference(
        construction,
        symbols,
        binders,
        coordinate,
        subscripts,
        provenance,
    )
}

fn lower_pre<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    arguments: &[Expression],
    provenance: dae::DaeProvenance,
    span: Span,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let (name, subscripts) =
        derivative_reference(&arguments[0]).expect("analysis proves the pre-value target shape");
    if symbols.functions.reinit_state_pre.contains(&span) {
        let Coordinate::State(state) = symbols.coordinates[name.var_name()] else {
            unreachable!("reinit pre analysis certifies a state coordinate")
        };
        let provenance = dae::DaeProvenance::generated(dae::DaeGeneration::PreValueLowering, span)?;
        return lower_coordinate_reference(
            construction,
            symbols,
            binders,
            dae::CoordinateInput::State(state),
            subscripts,
            provenance,
        );
    }
    let coordinate = symbols.coordinates[name.var_name()]
        .previous(span)
        .expect("analysis proves the pre-value role");
    lower_coordinate_reference(
        construction,
        symbols,
        binders,
        coordinate,
        subscripts,
        provenance,
    )
}

pub(super) fn derivative_reference(
    expression: &Expression,
) -> Option<(&rumoca_core::Reference, &[Subscript])> {
    match expression {
        Expression::VarRef {
            name, subscripts, ..
        } => Some((name, subscripts)),
        Expression::Index {
            base, subscripts, ..
        } => match base.as_ref() {
            Expression::VarRef {
                name,
                subscripts: base_subscripts,
                ..
            } if base_subscripts.is_empty() => Some((name, subscripts)),
            _ => None,
        },
        _ => None,
    }
}

fn lower_builtin_call<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    function: BuiltinFunction,
    arguments: &[Expression],
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let arguments = arguments
        .iter()
        .map(|argument| lower_expression_scoped(construction, symbols, binders, argument, None))
        .collect::<Result<Vec<_>, _>>()?;
    construction.expressions(|expressions| {
        expressions
            .at(provenance)
            .builtin(pure_builtin(function), arguments)
    })
}

fn lower_function_call<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    name: &rumoca_core::Reference,
    arguments: &[Expression],
    is_constructor: bool,
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    if is_constructor && name.as_str().starts_with("__rumoca_named_arg__.") {
        let [value] = arguments else {
            return Err(dae::DaeConstructionError::InvalidArity {
                expected: 1,
                found: arguments.len(),
                span: provenance.span(),
            });
        };
        return lower_expression_scoped(construction, symbols, binders, value, None);
    }
    if is_constructor {
        return lower_record_constructor(
            construction,
            symbols,
            binders,
            name,
            arguments,
            provenance,
        );
    }
    let (key, function) =
        symbols
            .functions
            .select_with_key(name, arguments, symbols.shapes, provenance.span());
    let arguments = arguments
        .iter()
        .enumerate()
        .map(|(ordinal, argument)| {
            if matches!(
                argument,
                Expression::Array {
                    elements,
                    ..
                } if elements.is_empty()
            ) {
                return lower_empty_function_argument(
                    construction,
                    symbols,
                    &key,
                    ordinal,
                    argument,
                );
            }
            lower_expression_scoped(construction, symbols, binders, argument, None)
        })
        .collect::<Result<Vec<_>, _>>()?;
    construction.expressions(|expressions| expressions.at(provenance).call(function, 0, arguments))
}

fn lower_record_constructor<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    name: &rumoca_core::Reference,
    arguments: &[Expression],
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let constructor = &symbols.functions.flat.functions[name.var_name()];
    let shapes = symbols
        .functions
        .shapes
        .constructor_field_shapes(name, arguments, symbols.shapes)
        .expect("analysis certifies every accepted record-constructor occurrence");
    let mut active_records = HashSet::new();
    let mut fields = Vec::with_capacity(arguments.len());
    let mut values = Vec::with_capacity(arguments.len());
    for ((parameter, shape), argument) in constructor.inputs.iter().zip(shapes).zip(arguments) {
        fields.push((
            VarName::new(&parameter.name),
            function_value_type(
                construction,
                symbols.functions.flat,
                parameter,
                shape,
                &mut active_records,
            )?,
        ));
        values.push(lower_expression_scoped(
            construction,
            symbols,
            binders,
            argument,
            None,
        )?);
    }
    let value_type =
        construction.types(|types| types.record(constructor.name.clone(), fields, provenance))?;
    construction.expressions(|expressions| expressions.at(provenance).record(value_type, values))
}

fn lower_empty_function_argument<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    key: &FunctionSpecializationKey,
    ordinal: usize,
    argument: &Expression,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let provenance = dae::DaeProvenance::source(
        argument
            .span()
            .expect("analysis proves empty argument provenance"),
    )?;
    let scalar = symbols.functions.primitive_parameter_scalar(key, ordinal);
    let value_type = construction.types(|types| {
        types.derived(
            dae::ValueType::array(scalar, key.inputs[ordinal].clone()),
            provenance,
        )
    })?;
    construction.expressions(|expressions| expressions.at(provenance).empty_array(value_type))
}

fn lower_conditional_expression<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    branches: &[(Expression, Expression)],
    else_branch: &Expression,
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let mut lowered = Vec::with_capacity(branches.len());
    for (condition, value) in branches {
        lowered.push((
            lower_expression_scoped(construction, symbols, binders, condition, None)?,
            lower_expression_scoped(construction, symbols, binders, value, None)?,
        ));
    }
    let fallback = lower_expression_scoped(construction, symbols, binders, else_branch, None)?;
    construction
        .expressions(|expressions| expressions.at(provenance).conditional(lowered, fallback))
}

fn lower_array_expression<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    elements: &[Expression],
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let elements = elements
        .iter()
        .map(|element| lower_expression_scoped(construction, symbols, binders, element, None))
        .collect::<Result<Vec<_>, _>>()?;
    construction.expressions(|expressions| expressions.at(provenance).array(elements))
}

fn lower_array_comprehension<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    enclosing_binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    body: &Expression,
    indices: &[rumoca_core::ComprehensionIndex],
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let key = ComprehensionKey::new(provenance.span(), indices)
        .expect("analysis proves comprehension-owner provenance");
    let plan = &symbols.functions.comprehension_plans[&key];
    let domain = construction.domains(|domains| {
        domains.nested_in_scope(
            enclosing_binders.values().copied(),
            plan.domain.clone(),
            provenance,
        )
    })?;
    let mut binders = enclosing_binders.clone();
    for (ordinal, (index, span)) in indices.iter().zip(&plan.binder_spans).enumerate() {
        let binder_provenance = dae::DaeProvenance::source(*span)?;
        let binder =
            construction.domains(|domains| domains.binder(domain, ordinal, binder_provenance))?;
        binders.insert(VarName::new(&index.name), binder);
    }
    let body = lower_expression_scoped(construction, symbols, &binders, body, None)?;
    construction.expressions(|expressions| expressions.at(provenance).comprehension(domain, body))
}

pub(super) fn lower_coordinate_reference<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    coordinate: dae::CoordinateInput<'dae>,
    subscripts: &[Subscript],
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let base = construction
        .expressions(|expressions| expressions.at(provenance).coordinate(coordinate))?;
    lower_index(construction, symbols, binders, base, subscripts, provenance)
}

fn lower_index<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    base: dae::ExprId<'dae>,
    subscripts: &[Subscript],
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    if subscripts.is_empty() {
        return Ok(base);
    }
    let subscripts = subscripts
        .iter()
        .map(|subscript| lower_subscript(construction, symbols, binders, subscript))
        .collect::<Result<Vec<_>, _>>()?;
    construction.expressions(|expressions| expressions.at(provenance).index(base, subscripts))
}

fn lower_subscript<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    subscript: &Subscript,
) -> Result<dae::Subscript<'dae>, dae::DaeConstructionError> {
    let provenance = dae::DaeProvenance::source(subscript.span())?;
    Ok(match subscript {
        Subscript::Index { value, .. } => {
            let expression = construction.expressions(|expressions| {
                expressions
                    .at(provenance)
                    .literal(dae::DaeLiteral::Integer(*value))
            })?;
            dae::Subscript::Index {
                expression,
                provenance,
            }
        }
        Subscript::Colon { .. } => dae::Subscript::Whole { provenance },
        Subscript::Expr { expr, .. } => {
            let expression = lower_expression_scoped(construction, symbols, binders, expr, None)?;
            dae::Subscript::Value {
                expression,
                provenance,
            }
        }
    })
}

pub(super) fn planned_input_variability(variable: &flat::Variable) -> dae::InputVariability {
    if matches!(variable.variability, Variability::Discrete(_)) || variable.is_discrete_type {
        dae::InputVariability::Discrete
    } else {
        dae::InputVariability::Continuous
    }
}

pub(super) fn expression_span(expression: &Expression) -> Result<Span, ToDaeError> {
    expression
        .span()
        .ok_or_else(|| ToDaeError::MissingProvenance {
            owner: format!("{expression:?}"),
        })
}

pub(super) fn require_span(span: Span, owner: impl Into<String>) -> Result<(), ToDaeError> {
    if span.is_dummy() {
        return Err(ToDaeError::MissingProvenance {
            owner: owner.into(),
        });
    }
    Ok(())
}

pub(super) fn variable_attribute_expressions(
    variable: &flat::Variable,
) -> impl Iterator<Item = &Expression> {
    [
        variable.start.as_ref(),
        variable.min.as_ref(),
        variable.max.as_ref(),
        variable.nominal.as_ref(),
        variable.binding.as_ref(),
    ]
    .into_iter()
    .flatten()
}

pub(super) fn all_model_expressions(flat: &flat::Model) -> impl Iterator<Item = &Expression> {
    flat.variables
        .values()
        .flat_map(variable_attribute_expressions)
        .chain(flat.equations.iter().map(|equation| &equation.residual))
        .chain(
            flat.initial_equations
                .iter()
                .map(|equation| &equation.residual),
        )
}

pub(super) fn expression_children(expression: &Expression) -> Vec<&Expression> {
    match expression {
        Expression::Binary { lhs, rhs, .. } => vec![lhs, rhs],
        Expression::Unary { rhs, .. } => vec![rhs],
        Expression::BuiltinCall { args, .. } | Expression::FunctionCall { args, .. } => {
            args.iter().collect()
        }
        Expression::StringConversion { value, format, .. } => std::iter::once(value.as_ref())
            .chain(format.operands())
            .collect(),
        Expression::If {
            branches,
            else_branch,
            ..
        } => branches
            .iter()
            .flat_map(|(condition, value)| [condition, value])
            .chain(std::iter::once(else_branch.as_ref()))
            .collect(),
        Expression::Array { elements, .. } | Expression::Tuple { elements, .. } => {
            elements.iter().collect()
        }
        Expression::Range {
            start, step, end, ..
        } => std::iter::once(start.as_ref())
            .chain(step.as_deref())
            .chain(std::iter::once(end.as_ref()))
            .collect(),
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => std::iter::once(expr.as_ref())
            .chain(indices.iter().map(|index| &index.range))
            .chain(filter.as_deref())
            .collect(),
        Expression::Index {
            base, subscripts, ..
        } => std::iter::once(base.as_ref())
            .chain(subscripts.iter().filter_map(subscript_expression))
            .collect(),
        Expression::VarRef { subscripts, .. } => {
            subscripts.iter().filter_map(subscript_expression).collect()
        }
        Expression::FieldAccess { base, .. } => vec![base],
        Expression::Literal { .. } | Expression::Empty { .. } => Vec::new(),
    }
}

fn subscript_expression(subscript: &Subscript) -> Option<&Expression> {
    match subscript {
        Subscript::Expr { expr, .. } => Some(expr),
        Subscript::Index { .. } | Subscript::Colon { .. } => None,
    }
}

fn binary_operator(operator: &OpBinary) -> dae::BinaryOperator {
    match operator {
        OpBinary::Add => dae::BinaryOperator::Add,
        OpBinary::Sub => dae::BinaryOperator::Subtract,
        OpBinary::Mul => dae::BinaryOperator::Multiply,
        OpBinary::Div => dae::BinaryOperator::Divide,
        OpBinary::Exp => dae::BinaryOperator::Power,
        OpBinary::AddElem => dae::BinaryOperator::ElementwiseAdd,
        OpBinary::SubElem => dae::BinaryOperator::ElementwiseSubtract,
        OpBinary::MulElem => dae::BinaryOperator::ElementwiseMultiply,
        OpBinary::DivElem => dae::BinaryOperator::ElementwiseDivide,
        OpBinary::ExpElem => dae::BinaryOperator::ElementwisePower,
        OpBinary::Eq => dae::BinaryOperator::Equal,
        OpBinary::Neq => dae::BinaryOperator::NotEqual,
        OpBinary::Lt => dae::BinaryOperator::Less,
        OpBinary::Le => dae::BinaryOperator::LessEqual,
        OpBinary::Gt => dae::BinaryOperator::Greater,
        OpBinary::Ge => dae::BinaryOperator::GreaterEqual,
        OpBinary::And => dae::BinaryOperator::And,
        OpBinary::Or => dae::BinaryOperator::Or,
        OpBinary::Empty | OpBinary::Assign => unreachable!("analysis restricts binary operators"),
    }
}

fn unary_operator(operator: &OpUnary) -> dae::UnaryOperator {
    match operator {
        OpUnary::Minus | OpUnary::DotMinus => dae::UnaryOperator::Negate,
        OpUnary::Plus | OpUnary::DotPlus => dae::UnaryOperator::Plus,
        OpUnary::Not => dae::UnaryOperator::Not,
        OpUnary::Empty => {
            unreachable!("analysis restricts unary operators")
        }
    }
}

fn pure_builtin(function: BuiltinFunction) -> dae::PureBuiltin {
    match function {
        BuiltinFunction::Abs => dae::PureBuiltin::Abs,
        BuiltinFunction::Sign => dae::PureBuiltin::Sign,
        BuiltinFunction::Sqrt => dae::PureBuiltin::Sqrt,
        BuiltinFunction::Div => dae::PureBuiltin::Div,
        BuiltinFunction::Mod => dae::PureBuiltin::Mod,
        BuiltinFunction::Rem => dae::PureBuiltin::Rem,
        BuiltinFunction::Floor => dae::PureBuiltin::Floor,
        BuiltinFunction::Ceil => dae::PureBuiltin::Ceil,
        BuiltinFunction::Integer => dae::PureBuiltin::Integer,
        BuiltinFunction::Sin => dae::PureBuiltin::Sin,
        BuiltinFunction::Cos => dae::PureBuiltin::Cos,
        BuiltinFunction::Tan => dae::PureBuiltin::Tan,
        BuiltinFunction::Asin => dae::PureBuiltin::Asin,
        BuiltinFunction::Acos => dae::PureBuiltin::Acos,
        BuiltinFunction::Atan => dae::PureBuiltin::Atan,
        BuiltinFunction::Atan2 => dae::PureBuiltin::Atan2,
        BuiltinFunction::Sinh => dae::PureBuiltin::Sinh,
        BuiltinFunction::Cosh => dae::PureBuiltin::Cosh,
        BuiltinFunction::Tanh => dae::PureBuiltin::Tanh,
        BuiltinFunction::Exp => dae::PureBuiltin::Exp,
        BuiltinFunction::Log => dae::PureBuiltin::Log,
        BuiltinFunction::Log10 => dae::PureBuiltin::Log10,
        BuiltinFunction::Smooth => dae::PureBuiltin::Smooth,
        BuiltinFunction::NoEvent => dae::PureBuiltin::NoEvent,
        BuiltinFunction::Homotopy => dae::PureBuiltin::Homotopy,
        BuiltinFunction::Min => dae::PureBuiltin::Min,
        BuiltinFunction::Max => dae::PureBuiltin::Max,
        BuiltinFunction::Sum => dae::PureBuiltin::Sum,
        BuiltinFunction::Product => dae::PureBuiltin::Product,
        BuiltinFunction::Size => dae::PureBuiltin::Size,
        BuiltinFunction::Zeros => dae::PureBuiltin::Zeros,
        BuiltinFunction::Ones => dae::PureBuiltin::Ones,
        BuiltinFunction::Fill => dae::PureBuiltin::Fill,
        BuiltinFunction::Linspace => dae::PureBuiltin::Linspace,
        BuiltinFunction::Cross => dae::PureBuiltin::Cross,
        _ => unreachable!("analysis restricts pure builtins"),
    }
}

fn lower_literal(literal: &Literal) -> dae::DaeLiteral {
    match literal {
        Literal::Real(value) => dae::DaeLiteral::Real(*value),
        Literal::Integer(value) => dae::DaeLiteral::Integer(*value),
        Literal::Boolean(value) => dae::DaeLiteral::Boolean(*value),
        Literal::String(value) => dae::DaeLiteral::String(value.clone()),
    }
}
