use super::*;

#[derive(Clone, Copy, Default)]
pub(super) enum Guard {
    #[default]
    Pure,
    Call,
    ModelCoordinate,
}
#[derive(Clone, Copy, Default)]
pub(super) enum Update {
    #[default]
    Literal,
    NonLiteral,
    OtherCarry,
    AliasIndex,
    WithoutPassthrough,
}
pub(super) struct Case {
    pub(super) width: u32,
    pub(super) lower: i64,
    pub(super) upper: i64,
    pub(super) step: i64,
    pub(super) guard_offset: i64,
    pub(super) guard: Guard,
    pub(super) update: Update,
}
impl Default for Case {
    fn default() -> Self {
        Self {
            width: 4,
            lower: 1,
            upper: 4,
            step: 1,
            guard_offset: 0,
            guard: Guard::Pure,
            update: Update::Literal,
        }
    }
}

struct Inputs<'dae> {
    u: dae::ExprId<'dae>,
    old: dae::ExprId<'dae>,
    other: dae::ExprId<'dae>,
    index: dae::ExprId<'dae>,
    model: dae::ExprId<'dae>,
    helper: dae::FunctionId<'dae>,
    at: dae::DaeProvenance,
}

fn helper<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    real: dae::ValueTypeId<'dae>,
    boolean: dae::ValueTypeId<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    model
        .function(
            dae::FunctionSignature::new(VarName::new("positive"), [real], [boolean], at),
            |model, reservation| {
                let input =
                    model.functions(|f| f.parameter(&reservation, VarName::new("value"), 0, at))?;
                let output =
                    model.functions(|f| f.output(&reservation, VarName::new("result"), 0, at))?;
                let result = model.expressions(|e| {
                    let value = e.at(at).function_parameter(input)?;
                    let zero = e.at(at).literal(dae::DaeLiteral::Real(0.0))?;
                    e.at(at)
                        .binary(dae::BinaryOperator::GreaterEqual, value, zero)
                })?;
                let mut body = model.functions(|f| f.begin(reservation, at))?;
                model.functions(|f| {
                    f.assign(&mut body, output, result, at)?;
                    f.define(body, at)
                })
            },
        )
        .map(|(function, ())| function)
}

fn guard<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    inputs: &Inputs<'dae>,
    case: &Case,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let selected = model.expressions(|e| {
        let offset = e
            .at(inputs.at)
            .literal(dae::DaeLiteral::Integer(case.guard_offset))?;
        let index = e
            .at(inputs.at)
            .binary(dae::BinaryOperator::Add, inputs.index, offset)?;
        e.at(inputs.at).index(
            inputs.u,
            [dae::Subscript::Index {
                expression: index,
                provenance: inputs.at,
            }],
        )
    })?;
    model.expressions(|e| match case.guard {
        Guard::Call => e.at(inputs.at).call(inputs.helper, 0, [selected]),
        Guard::Pure | Guard::ModelCoordinate => {
            let zero = e.at(inputs.at).literal(dae::DaeLiteral::Real(0.0))?;
            let value = if matches!(case.guard, Guard::ModelCoordinate) {
                inputs.model
            } else {
                selected
            };
            e.at(inputs.at)
                .binary(dae::BinaryOperator::GreaterEqual, value, zero)
        }
    })
}

fn body<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    inputs: &Inputs<'dae>,
    case: &Case,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let condition = guard(model, inputs, case)?;
    model.expressions(|e| {
        let value = match case.update {
            Update::NonLiteral => condition,
            Update::OtherCarry => e.at(inputs.at).index(
                inputs.other,
                [dae::Subscript::Index {
                    expression: inputs.index,
                    provenance: inputs.at,
                }],
            )?,
            _ => e.at(inputs.at).literal(dae::DaeLiteral::Boolean(true))?,
        };
        let index = if matches!(case.update, Update::AliasIndex) {
            let zero = e.at(inputs.at).literal(dae::DaeLiteral::Integer(0))?;
            e.at(inputs.at)
                .binary(dae::BinaryOperator::Add, inputs.index, zero)?
        } else {
            inputs.index
        };
        let updated = e.at(inputs.at).array_update(
            inputs.old,
            value,
            [dae::Subscript::Index {
                expression: index,
                provenance: inputs.at,
            }],
        )?;
        if matches!(case.update, Update::WithoutPassthrough) {
            Ok(updated)
        } else {
            e.at(inputs.at)
                .conditional([(condition, updated)], inputs.old)
        }
    })
}

fn function<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    case: &Case,
    vector: dae::ValueTypeId<'dae>,
    mask: dae::ValueTypeId<'dae>,
    helper: dae::FunctionId<'dae>,
    global: dae::ExprId<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    let real = model.types(|t| t.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
    model
        .function(
            dae::FunctionSignature::new(VarName::new("mark"), [vector], [mask, mask, real], at),
            |model, reservation| {
                let input =
                    model.functions(|f| f.parameter(&reservation, VarName::new("u"), 0, at))?;
                let y = model.functions(|f| f.output(&reservation, VarName::new("mask"), 0, at))?;
                let z =
                    model.functions(|f| f.output(&reservation, VarName::new("other"), 1, at))?;
                let hits =
                    model.functions(|f| f.output(&reservation, VarName::new("hits"), 2, at))?;
                let mut initial = model.functions(|f| f.begin(reservation, at))?;
                let zeros = model.expressions(|e| {
                    let zero = e.at(at).literal(dae::DaeLiteral::Boolean(false))?;
                    e.at(at).array(vec![zero; case.width as usize])
                })?;
                model.functions(|f| {
                    f.assign(&mut initial, y, zeros, at)?;
                    f.assign(&mut initial, z, zeros, at)
                })?;
                let domain = model.domains(|d| {
                    d.structured(
                        StructuredIndexDomain {
                            binders: vec![StructuredIndexBinder {
                                id: 0,
                                display_name: "i".into(),
                                lower: case.lower,
                                upper: case.upper,
                                step: case.step,
                            }],
                        },
                        at,
                    )
                })?;
                let binder = model.domains(|d| d.binder(domain, 0, at))?;
                let mut iteration =
                    model.functions(|f| f.begin_loop(initial, domain, [y, z], at))?;
                let old = model.functions(|f| f.read(iteration.body(), y, at))?;
                let other = model.functions(|f| f.read(iteration.body(), z, at))?;
                let (u, index) = model.expressions(|e| {
                    Ok((
                        e.at(at).function_parameter(input)?,
                        e.at(at).binder(binder)?,
                    ))
                })?;
                let update = body(
                    model,
                    &Inputs {
                        u,
                        old,
                        other,
                        index,
                        model: global,
                        helper,
                        at,
                    },
                    case,
                )?;
                model.functions(|f| f.assign_loop(&mut iteration, y, update, at))?;
                let mut completed = model.functions(|f| f.finish_loop(iteration, at))?;
                let mask = model.functions(|f| f.read(&completed, y, at))?;
                let total = summarize_mask(model, mask, case.width, at)?;
                model.functions(|f| {
                    f.assign(&mut completed, hits, total, at)?;
                    f.define(completed, at)
                })
            },
        )
        .map(|(function, ())| function)
}

fn summarize_mask<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    mask: dae::ExprId<'dae>,
    width: u32,
    at: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let domain = model.domains(|d| {
        d.structured(
            StructuredIndexDomain {
                binders: vec![StructuredIndexBinder {
                    id: 0,
                    display_name: "j".into(),
                    lower: 1,
                    upper: i64::from(width),
                    step: 1,
                }],
            },
            at,
        )
    })?;
    let binder = model.domains(|d| d.binder(domain, 0, at))?;
    model.expressions(|e| {
        let index = e.at(at).binder(binder)?;
        let selected = e.at(at).index(
            mask,
            [dae::Subscript::Index {
                expression: index,
                provenance: at,
            }],
        )?;
        let one = e.at(at).literal(dae::DaeLiteral::Real(1.0))?;
        let zero = e.at(at).literal(dae::DaeLiteral::Real(0.0))?;
        let value = e.at(at).conditional([(selected, one)], zero)?;
        let values = e.at(at).comprehension(domain, value)?;
        e.at(at).builtin(dae::PureBuiltin::Sum, [values])
    })
}

pub(super) fn model(case: &Case) -> Result<dae::Dae, dae::DaeConstructionError> {
    let text = "function mark input Real u[:]; output Boolean mask[:]; algorithm for i in 1:size(u,1) loop if u[i]>=0 then mask[i]:=true; end if; end for; end mark;";
    let mut sources = SourceMap::new();
    let source = sources.add("literal_update_sweep.mo", text);
    let at = provenance(source, 0, text.len());
    dae::Dae::construct(sources, |model| {
        let real = model.types(|t| t.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let boolean =
            model.types(|t| t.derived(dae::ValueType::scalar(dae::ScalarType::Boolean), at))?;
        let vector = model.types(|t| {
            t.derived(
                dae::ValueType::array(dae::ScalarType::Real, [case.width]),
                at,
            )
        })?;
        let mask = model.types(|t| {
            t.derived(
                dae::ValueType::array(dae::ScalarType::Boolean, [case.width]),
                at,
            )
        })?;
        let x = model
            .variables(|v| v.algebraic(VarName::new("global"), real, at, Default::default()))?;
        let global =
            model.expressions(|e| e.at(at).coordinate(dae::CoordinateInput::Algebraic(x)))?;
        let helper = helper(model, real, boolean, at)?;
        let mark = function(model, case, vector, mask, helper, global, at)?;
        let u =
            model.variables(|v| v.algebraic(VarName::new("u"), vector, at, Default::default()))?;
        model.expressions(|e| {
            let input = e.at(at).coordinate(dae::CoordinateInput::Algebraic(u))?;
            e.at(at).call(mark, 2, [input])?;
            Ok(())
        })
    })
}
