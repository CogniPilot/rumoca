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
    pub(super) owner_clock: Option<dae::ClockId<'dae>>,
}

pub(super) fn lower_clocked_expression<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    owner_clock: dae::ClockId<'dae>,
    expression: &Expression,
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
        None,
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
        Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args,
            ..
        } => lower_derivative(construction, symbols, binders, args, provenance, span),
        Expression::BuiltinCall {
            function: BuiltinFunction::Pre,
            args,
            ..
        } => lower_pre(construction, symbols, binders, args, provenance, span),
        Expression::BuiltinCall {
            function: BuiltinFunction::Sample,
            args,
            ..
        } => {
            let Some(value) = args.first() else {
                unreachable!("clock analysis rejects value sampling without an operand")
            };
            lower_temporal_identity(construction, symbols, binders, value, provenance)
        }
        Expression::BuiltinCall { function, args, .. } => {
            lower_builtin_call(construction, symbols, binders, *function, args, provenance)
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
        Expression::Range {
            start, step, end, ..
        } => {
            let start = integer_literal(start);
            let step = step.as_deref().map(integer_literal).unwrap_or(1);
            let end = integer_literal(end);
            construction
                .expressions(|expressions| expressions.at(provenance).range(start, step, end))
        }
        Expression::Index {
            base, subscripts, ..
        } => {
            let base = lower_expression_scoped(construction, symbols, binders, base, None)?;
            lower_index(construction, symbols, binders, base, subscripts, provenance)
        }
        Expression::FunctionCall {
            name,
            args,
            is_constructor,
            ..
        } => match name.as_str() {
            "previous" => lower_previous(construction, symbols, binders, args, provenance),
            "hold" => lower_hold(construction, symbols, binders, args, provenance),
            _ => lower_function_call(
                construction,
                symbols,
                binders,
                name,
                args,
                *is_constructor,
                provenance,
            ),
        },
        Expression::FieldAccess { .. } => {
            lower_record_array_field_projection(construction, symbols, binders, provenance)
        }
        Expression::Tuple { .. } | Expression::Empty { .. } => {
            unreachable!("analysis rejects expressions outside the checked lowering grammar")
        }
    }
}

fn lower_hold<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    arguments: &[Expression],
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let [value] = arguments else {
        unreachable!("intrinsic analysis proves hold arity")
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
        unreachable!("intrinsic analysis proves previous arity")
    };
    let (name, subscripts) =
        derivative_reference(value).expect("clock analysis proves a coordinate previous operand");
    let clock = symbols
        .owner_clock
        .expect("clock analysis supplies the owning previous clock");
    let previous =
        construction.temporal(|temporal| match symbols.coordinates[name.var_name()] {
            Coordinate::DiscreteReal(variable) => {
                temporal.previous_discrete_real(clock, variable, provenance)
            }
            Coordinate::DiscreteValue(variable) => {
                temporal.previous_discrete_value(clock, variable, provenance)
            }
            _ => unreachable!("clock analysis classifies previous operands as clocked discrete"),
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

fn lower_temporal_identity<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    value: &Expression,
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    if let Some((name, subscripts)) = derivative_reference(value) {
        return lower_coordinate_reference(
            construction,
            symbols,
            binders,
            symbols.coordinates[name.var_name()].current(),
            subscripts,
            provenance,
        );
    }
    lower_expression_scoped(construction, symbols, binders, value, None)
}

fn lower_record_array_field_projection<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let plan = symbols
        .functions
        .record_array_fields
        .get(&provenance.span())
        .expect("analysis certifies every lowered record-array field projection");
    let generated = dae::DaeProvenance::generated(
        dae::DaeGeneration::RecordEquationProjection,
        provenance.span(),
    )?;
    let elements = plan
        .coordinates
        .iter()
        .map(|coordinate| {
            construction.expressions(|expressions| {
                expressions
                    .at(generated)
                    .coordinate(symbols.coordinates[coordinate].current())
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    let base = construction.expressions(|expressions| expressions.at(generated).array(elements))?;
    lower_index(
        construction,
        symbols,
        binders,
        base,
        &plan.subscripts,
        provenance,
    )
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

fn integer_literal(expression: &Expression) -> i64 {
    let Expression::Literal {
        value: Literal::Integer(value),
        ..
    } = expression
    else {
        unreachable!("analysis restricts compact range bounds")
    };
    *value
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
        BuiltinFunction::Mod => dae::PureBuiltin::Mod,
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
