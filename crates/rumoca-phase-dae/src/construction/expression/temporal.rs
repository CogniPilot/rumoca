use super::*;

pub(super) fn lower_derivative<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    expression: &Expression,
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let span = provenance.span();
    let missing = || dae::DaeConstructionError::MissingDerivativeCertificate { span };
    let plan = symbols
        .functions
        .derivatives
        .certificate(expression)
        .ok_or_else(missing)?;
    let state = symbols
        .functions
        .state_occurrences
        .get(&plan.target())
        .copied()
        .ok_or_else(missing)?;
    lower_coordinate_reference(
        construction,
        symbols,
        binders,
        dae::CoordinateInput::Derivative(state),
        plan.subscripts(),
        provenance,
    )
}

pub(super) fn lower_pre<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    arguments: &[Expression],
    provenance: dae::DaeProvenance,
    span: Span,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let (name, subscripts) =
        derivative_reference(&arguments[0]).expect("analysis proves the pre-value target shape");
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

/// Consume the exact analysis certificate for MLS §3.7.5 `edge`/`change`.
///
/// Both ordinary expressions and condition lowering enter here through
/// [`lower_expression_scoped`], so DAE-C06 has one implementation and one
/// generated provenance owner.
pub(super) fn lower_history_operator<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    function: BuiltinFunction,
    arguments: &[Expression],
    span: Span,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let missing = || dae::DaeConstructionError::MissingHistoryOperatorCertificate {
        operator: function.name(),
        span,
    };
    let [argument] = arguments else {
        return Err(missing());
    };
    let Some((name, subscripts)) = derivative_reference(argument) else {
        return Err(missing());
    };
    let variable = symbols
        .functions
        .flat
        .variables
        .get(name.var_name())
        .ok_or_else(missing)?;
    let plan = symbols
        .functions
        .history_operators
        .certificate(function, span, variable.instance_id)
        .ok_or_else(missing)?;
    if plan.subscripts() != subscripts {
        return Err(missing());
    }
    let coordinate = symbols
        .functions
        .coordinate_instances
        .get(&plan.instance())
        .copied()
        .ok_or_else(missing)?;
    let generated = dae::DaeProvenance::generated(dae::DaeGeneration::PreValueLowering, span)?;
    let current_provenance = dae::DaeProvenance::source(plan.operand_span())?;
    let current = lower_coordinate_reference(
        construction,
        symbols,
        binders,
        coordinate.current(),
        plan.subscripts(),
        current_provenance,
    )?;
    let previous_coordinate = coordinate.previous(span).map_err(|_| missing())?;
    let previous = lower_coordinate_reference(
        construction,
        symbols,
        binders,
        previous_coordinate,
        plan.subscripts(),
        generated,
    )?;
    match function {
        BuiltinFunction::Edge => {
            let not_previous = construction.expressions(|expressions| {
                expressions
                    .at(generated)
                    .unary(dae::UnaryOperator::Not, previous)
            })?;
            construction.expressions(|expressions| {
                expressions
                    .at(generated)
                    .binary(dae::BinaryOperator::And, current, not_previous)
            })
        }
        BuiltinFunction::Change => construction.expressions(|expressions| {
            expressions
                .at(generated)
                .binary(dae::BinaryOperator::NotEqual, current, previous)
        }),
        _ => Err(missing()),
    }
}
