use super::*;

#[derive(Clone, Copy)]
pub(in crate::projection::tests::guard_replay) enum SizeMode {
    Literal,
    Selected,
    Nested,
}

pub(super) fn input_types<'dae>(
    vector: dae::ValueTypeId<'dae>,
    size: Option<(dae::ValueTypeId<'dae>, SizeMode)>,
) -> Vec<dae::ValueTypeId<'dae>> {
    std::iter::once(vector)
        .chain(size.map(|(axes, _)| axes))
        .collect()
}

pub(super) fn axis_parameter<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    reservation: &dae::FunctionReservation<'_, 'dae>,
    size: Option<(dae::ValueTypeId<'dae>, SizeMode)>,
    at: dae::DaeProvenance,
) -> Result<Option<dae::ExprId<'dae>>, dae::DaeConstructionError> {
    if size.is_none() {
        return Ok(None);
    }
    let parameter = model.functions(|f| f.parameter(reservation, VarName::new("axes"), 1, at))?;
    model
        .expressions(|e| e.at(at).function_parameter(parameter))
        .map(Some)
}

pub(super) fn operands<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    array: dae::ExprId<'dae>,
    axis: Option<dae::ExprId<'dae>>,
    index: dae::ExprId<'dae>,
    size: Option<(dae::ValueTypeId<'dae>, SizeMode)>,
    at: dae::DaeProvenance,
) -> Result<Option<(dae::ExprId<'dae>, dae::ExprId<'dae>)>, dae::DaeConstructionError> {
    let Some((_, mode)) = size else {
        return Ok(None);
    };
    model.expressions(|e| {
        let one = e.at(at).literal(dae::DaeLiteral::Integer(1))?;
        let dimension = match mode {
            SizeMode::Literal => one,
            SizeMode::Selected | SizeMode::Nested => e.at(at).index(
                axis.unwrap(),
                [dae::Subscript::Index {
                    expression: index,
                    provenance: at,
                }],
            )?,
        };
        let dimension = if matches!(mode, SizeMode::Nested) {
            let single = e.at(at).array([one])?;
            let inner_size = e.at(at).builtin(dae::PureBuiltin::Size, [single, one])?;
            let sum = e
                .at(at)
                .binary(dae::BinaryOperator::Add, dimension, inner_size)?;
            e.at(at).binary(dae::BinaryOperator::Subtract, sum, one)?
        } else {
            dimension
        };
        Ok(Some((array, dimension)))
    })
}

pub(super) fn guard<'dae>(
    expressions: &mut dae::Expressions<'_, 'dae>,
    condition: dae::ExprId<'dae>,
    size: Option<(dae::ExprId<'dae>, dae::ExprId<'dae>)>,
    at: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let Some((array, dimension)) = size else {
        return Ok(condition);
    };
    let extent = expressions
        .at(at)
        .builtin(dae::PureBuiltin::Size, [array, dimension])?;
    let zero = expressions.at(at).literal(dae::DaeLiteral::Integer(0))?;
    let nonempty = expressions
        .at(at)
        .binary(dae::BinaryOperator::Greater, extent, zero)?;
    expressions
        .at(at)
        .binary(dae::BinaryOperator::And, condition, nonempty)
}

pub(super) fn arguments<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    values: dae::AlgebraicId<'dae>,
    name: &str,
    size: Option<(dae::ValueTypeId<'dae>, SizeMode)>,
    at: dae::DaeProvenance,
) -> Result<Vec<dae::ExprId<'dae>>, dae::DaeConstructionError> {
    let mut args =
        vec![model.expressions(|e| e.at(at).coordinate(dae::CoordinateInput::Algebraic(values)))?];
    if let Some((axes, _)) = size {
        let variable = model.variables(|v| {
            v.input(
                VarName::new(format!("axes_{name}")),
                axes,
                dae::InputVariability::Discrete,
                at,
                Default::default(),
            )
        })?;
        args.push(
            model.expressions(|e| e.at(at).coordinate(dae::CoordinateInput::Input(variable)))?,
        );
    }
    Ok(args)
}
