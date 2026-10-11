use super::*;

/// MLS §10.3.1: `size(A, i)` is the extent of `A`, never its value. A
/// function body proves that extent from the declaration (MLS §12.2), so the
/// query folds to that Integer and reads nothing, which lets an output state
/// its own extent before the algorithm defines it, as in
/// `state := f(size(state, 1))`.
pub(super) fn lower_builtin_node<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    (expression, function, arguments): (&Expression, BuiltinFunction, &[Expression]),
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let proven = (function == BuiltinFunction::Size
        && arguments.len() == 2
        && symbols.function_body.is_some())
    .then(|| symbols.shapes.proven_extent(expression))
    .flatten();
    match proven {
        Some(extent) => construction.expressions(|expressions| {
            expressions
                .at(provenance)
                .literal(dae::DaeLiteral::Integer(extent))
        }),
        None => lower_builtin_expression(
            construction,
            symbols,
            binders,
            function,
            arguments,
            provenance,
        ),
    }
}

/// The extent `size(a, k)` names when `k` is a translation-time Integer.
///
/// MLS §10.3.1: `size` reads only the shape of `a`, which the shape proof of
/// the scope fixes (through the callee specialization for a call result), so
/// the call is that Integer and `a` is not lowered: a call result `a` is never
/// evaluated (nor its assertions checked) for a value no one uses.
pub(super) fn static_size(
    symbols: LoweringSymbols<'_, '_>,
    arguments: &[Expression],
) -> Option<i64> {
    let [array, axis] = arguments else {
        return None;
    };
    let axis = match axis {
        Expression::Literal {
            value: Literal::Integer(axis),
            ..
        } => Some(*axis),
        _ => symbols.shapes.proven_extent(axis),
    };
    let axis = usize::try_from(axis?).ok()?.checked_sub(1)?;
    let shape = symbols
        .functions
        .shapes
        .expression_shape(array, symbols.shapes)
        .ok()?;
    shape.get(axis).map(|extent| i64::from(*extent))
}
