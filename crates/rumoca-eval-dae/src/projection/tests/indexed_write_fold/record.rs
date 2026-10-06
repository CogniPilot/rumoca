use super::*;

fn function<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    array: dae::ValueTypeId<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    model
        .function(
            dae::FunctionSignature::new(VarName::new("copyPairs"), [array], [array], at),
            |model, reservation| {
                let input =
                    model.functions(|f| f.parameter(&reservation, VarName::new("u"), 0, at))?;
                let output =
                    model.functions(|f| f.output(&reservation, VarName::new("y"), 0, at))?;
                let input = model.expressions(|e| e.at(at).function_parameter(input))?;
                let mut body = model.functions(|f| f.begin(reservation, at))?;
                model.functions(|f| f.assign(&mut body, output, input, at))?;
                let domain = model.domains(|d| {
                    d.structured(
                        StructuredIndexDomain {
                            binders: vec![StructuredIndexBinder {
                                id: 0,
                                display_name: "i".into(),
                                lower: 1,
                                upper: 2,
                                step: 1,
                            }],
                        },
                        at,
                    )
                })?;
                let binder = model.domains(|d| d.binder(domain, 0, at))?;
                let mut iteration =
                    model.functions(|f| f.begin_loop(body, domain, [output], at))?;
                let old = model.functions(|f| f.read(iteration.body(), output, at))?;
                let updated = model.expressions(|e| {
                    let index = e.at(at).binder(binder)?;
                    let selected = e.at(at).index(
                        input,
                        [dae::Subscript::Index {
                            expression: index,
                            provenance: at,
                        }],
                    )?;
                    e.at(at).array_update(
                        old,
                        selected,
                        [dae::Subscript::Index {
                            expression: index,
                            provenance: at,
                        }],
                    )
                })?;
                model.functions(|f| f.assign_loop(&mut iteration, output, updated, at))?;
                let body = model.functions(|f| f.finish_loop(iteration, at))?;
                model.functions(|f| f.define(body, at))
            },
        )
        .map(|(function, ())| function)
}

pub(super) fn model() -> dae::Dae {
    let text = "record Pair Real first; Real second; end Pair; function copyPairs input Pair u[2]; output Pair y[2]; algorithm y:=u; for i in 1:2 loop y[i]:=u[i]; end for; end copyPairs;";
    let mut sources = SourceMap::new();
    let source = sources.add("record_indexed_fold.mo", text);
    let at = provenance(source, 0, text.len());
    dae::Dae::construct(sources, |model| {
        let real = model.types(|t| t.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let fields = [
            (VarName::new("first"), real),
            (VarName::new("second"), real),
        ];
        let pair = model.types(|t| t.record(VarName::new("Pair"), fields.clone(), at))?;
        let array = model.types(|t| t.record_array(VarName::new("Pair"), fields, [2], at))?;
        let vector =
            model.types(|t| t.derived(dae::ValueType::array(dae::ScalarType::Real, [4]), at))?;
        let actual = model
            .variables(|v| v.algebraic(VarName::new("actual"), vector, at, Default::default()))?;
        let function = function(model, array, at)?;
        model.expressions(|e| {
            let actual = e
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(actual))?;
            let mut records = Vec::new();
            for start in [1, 3] {
                records.push(pair_from_flat(e, actual, pair, start, at)?);
            }
            let argument = e.at(at).array(records)?;
            let result = e.at(at).call(function, 0, [argument])?;
            e.at(at).field(result, 0)?;
            Ok(())
        })
    })
    .unwrap()
}

fn pair_from_flat<'dae>(
    e: &mut dae::Expressions<'_, 'dae>,
    actual: dae::ExprId<'dae>,
    pair: dae::ValueTypeId<'dae>,
    start: i64,
    at: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let mut values = Vec::new();
    for index in [start, start + 1] {
        let index = e.at(at).literal(dae::DaeLiteral::Integer(index))?;
        values.push(e.at(at).index(
            actual,
            [dae::Subscript::Index {
                expression: index,
                provenance: at,
            }],
        )?);
    }
    e.at(at).record(pair, values)
}
