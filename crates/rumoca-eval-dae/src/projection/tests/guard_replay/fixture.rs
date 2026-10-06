use super::*;
mod size;
pub(super) use size::SizeMode;

#[derive(Clone, Copy)]
struct GuardInputs<'dae> {
    size: Option<(dae::ValueTypeId<'dae>, SizeMode)>,
    builtin: Option<dae::PureBuiltin>,
}

struct GuardOperands<'dae> {
    helper: Option<dae::FunctionId<'dae>>,
    size: Option<(dae::ExprId<'dae>, dae::ExprId<'dae>)>,
    builtin: Option<dae::PureBuiltin>,
}

fn range(name: &str) -> StructuredIndexDomain {
    StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            id: 0,
            display_name: name.to_string(),
            lower: 1,
            upper: 2,
            step: 1,
        }],
    }
}

fn nested_index<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    outer: dae::DomainId<'dae>,
    inner: dae::DomainId<'dae>,
    offset: i64,
    only_outer: bool,
    at: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let p = model.domains(|domains| domains.binder(outer, 0, at))?;
    let q = model.domains(|domains| domains.binder(inner, 0, at))?;
    model.expressions(|expressions| {
        let p = expressions.at(at).binder(p)?;
        let q = expressions.at(at).binder(q)?;
        if only_outer {
            return Ok(p);
        }
        let one = expressions.at(at).literal(dae::DaeLiteral::Integer(1))?;
        let two = expressions.at(at).literal(dae::DaeLiteral::Integer(2))?;
        let last = expressions
            .at(at)
            .literal(dae::DaeLiteral::Integer(offset))?;
        let offset = expressions
            .at(at)
            .binary(dae::BinaryOperator::Subtract, p, one)?;
        let offset = expressions
            .at(at)
            .binary(dae::BinaryOperator::Multiply, offset, two)?;
        let index = expressions
            .at(at)
            .binary(dae::BinaryOperator::Add, offset, q)?;
        expressions
            .at(at)
            .binary(dae::BinaryOperator::Add, index, last)
    })
}

fn update_nested<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    inner: &mut dae::FunctionLoop<'dae>,
    targets: [dae::FunctionValueId<'dae>; 2],
    selected: dae::ExprId<'dae>,
    guard: GuardOperands<'dae>,
    at: dae::DaeProvenance,
) -> Result<(), dae::DaeConstructionError> {
    let [y, z] = targets;
    let (old_y, old_z) = model.functions(|functions| {
        Ok((
            functions.read(inner.body(), y, at)?,
            functions.read(inner.body(), z, at)?,
        ))
    })?;
    let condition = model.expressions(|e| {
        let selected = if let Some(helper) = guard.helper {
            let arguments = e.at(at).array([old_z, selected])?;
            e.at(at).call(helper, 0, [arguments])?
        } else {
            selected
        };
        let selected = if let Some(builtin) = guard.builtin {
            e.at(at).builtin(builtin, [selected])?
        } else {
            selected
        };
        let sum = e.at(at).binary(dae::BinaryOperator::Add, old_z, selected)?;
        let condition = e.at(at).binary(dae::BinaryOperator::Greater, sum, old_z)?;
        size::guard(e, condition, guard.size, at)
    })?;
    let (next_y, next_z) = model.expressions(|expressions| {
        Ok((
            expressions
                .at(at)
                .binary(dae::BinaryOperator::Add, old_y, old_z)?,
            expressions
                .at(at)
                .binary(dae::BinaryOperator::Add, old_z, selected)?,
        ))
    })?;
    let (next_y, next_z) = model.expressions(|e| {
        Ok((
            e.at(at).conditional([(condition, next_y)], old_y)?,
            e.at(at).conditional([(condition, next_z)], old_z)?,
        ))
    })?;
    model.functions(|functions| {
        functions.assign_loop(inner, y, next_y, at)?;
        functions.assign_loop(inner, z, next_z, at)
    })
}

fn nested_function<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    types: [dae::ValueTypeId<'dae>; 2],
    offset: i64,
    only_outer: bool,
    helper: Option<dae::FunctionId<'dae>>,
    guard: GuardInputs<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    let [vector, scalar] = types;
    model
        .function(
            dae::FunctionSignature::new(
                VarName::new("nested"),
                size::input_types(vector, guard.size),
                [scalar, scalar, scalar],
                at,
            ),
            |model, reservation| {
                let u = model.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("u"), 0, at)
                })?;
                let axis = size::axis_parameter(model, &reservation, guard.size, at)?;
                let (y, z, unrelated) = model.functions(|functions| {
                    Ok((
                        functions.output(&reservation, VarName::new("y"), 0, at)?,
                        functions.output(&reservation, VarName::new("z"), 1, at)?,
                        functions.output(&reservation, VarName::new("unrelated"), 2, at)?,
                    ))
                })?;
                let u =
                    model.expressions(|expressions| expressions.at(at).function_parameter(u))?;
                let mut body = model.functions(|functions| functions.begin(reservation, at))?;
                for (target, index) in [(y, 1), (z, 2), (unrelated, 7)] {
                    let selected = model.expressions(|expressions| {
                        let index = expressions
                            .at(at)
                            .literal(dae::DaeLiteral::Integer(index))?;
                        expressions.at(at).index(
                            u,
                            [dae::Subscript::Index {
                                expression: index,
                                provenance: at,
                            }],
                        )
                    })?;
                    model
                        .functions(|functions| functions.assign(&mut body, target, selected, at))?;
                }
                let outer_domain = model.domains(|domains| domains.structured(range("i"), at))?;
                let inner_domain =
                    model.domains(|domains| domains.nested(outer_domain, range("i"), at))?;
                let outer = model.functions(|functions| {
                    functions.begin_loop(body, outer_domain, [y, z, unrelated], at)
                })?;
                let mut inner = model.functions(|functions| {
                    functions.begin_nested_loop(outer, inner_domain, [y, z], at)
                })?;
                let index =
                    nested_index(model, outer_domain, inner_domain, offset, only_outer, at)?;
                let selected = model.expressions(|expressions| {
                    expressions.at(at).index(
                        u,
                        [dae::Subscript::Index {
                            expression: index,
                            provenance: at,
                        }],
                    )
                })?;
                let size_operands = size::operands(model, u, axis, index, guard.size, at)?;
                update_nested(
                    model,
                    &mut inner,
                    [y, z],
                    selected,
                    GuardOperands {
                        helper,
                        size: size_operands,
                        builtin: guard.builtin,
                    },
                    at,
                )?;
                let outer = model.functions(|functions| functions.finish_nested_loop(inner, at))?;
                body = model.functions(|functions| functions.finish_loop(outer, at))?;
                let (last_y, last_z) = model.functions(|functions| {
                    Ok((functions.read(&body, y, at)?, functions.read(&body, z, at)?))
                })?;
                let result = model.expressions(|expressions| {
                    expressions
                        .at(at)
                        .binary(dae::BinaryOperator::Add, last_y, last_z)
                })?;
                model.functions(|functions| {
                    functions.assign(&mut body, y, result, at)?;
                    functions.define(body, at)
                })
            },
        )
        .map(|(function, ())| function)
}

pub(super) fn model(offset: i64, only_outer: bool, call: bool) -> dae::Dae {
    build(offset, only_outer, call, None, None)
}

pub(super) fn size_model(offset: i64, mode: SizeMode) -> dae::Dae {
    build(offset, false, false, Some(mode), None)
}

pub(super) fn floor_model(offset: i64, call: bool) -> dae::Dae {
    build(offset, false, call, None, Some(dae::PureBuiltin::Floor))
}

pub(super) fn abs_model(offset: i64, call: bool) -> dae::Dae {
    build(offset, false, call, None, Some(dae::PureBuiltin::Abs))
}

fn build(
    offset: i64,
    only_outer: bool,
    call: bool,
    size_mode: Option<SizeMode>,
    builtin: Option<dae::PureBuiltin>,
) -> dae::Dae {
    let text = "nested accumulation with shadowed i, sequential y/z reads and an unrelated carry";
    let mut sources = SourceMap::new();
    let source = sources.add("nested_projection.mo", text);
    let at = provenance(source, 0, text.len());
    dae::Dae::construct(sources, |model| {
        let vector = model
            .types(|types| types.derived(dae::ValueType::array(dae::ScalarType::Real, [7]), at))?;
        let scalar = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let helper = if call {
            let pair = model
                .types(|t| t.derived(dae::ValueType::array(dae::ScalarType::Real, [2]), at))?;
            Some(construct_second_function(model, pair, scalar, at)?)
        } else {
            None
        };
        let size_inputs = if let Some(mode) = size_mode {
            let axes = model
                .types(|t| t.derived(dae::ValueType::array(dae::ScalarType::Integer, [4]), at))?;
            Some((axes, mode))
        } else {
            None
        };
        let function = nested_function(
            model,
            [vector, scalar],
            offset,
            only_outer,
            helper,
            GuardInputs {
                size: size_inputs,
                builtin,
            },
            at,
        )?;
        for name in ["a", "b"] {
            let variable = model.variables(|variables| {
                variables.algebraic(VarName::new(name), vector, at, Default::default())
            })?;
            let arguments = size::arguments(model, variable, name, size_inputs, at)?;
            model.expressions(|e| {
                e.at(at).call(function, 0, arguments)?;
                Ok(())
            })?;
        }
        Ok(())
    })
    .unwrap()
}
