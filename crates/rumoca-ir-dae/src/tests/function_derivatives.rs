use super::*;
use rumoca_core::FunctionDerivativeInput::{self, Differentiate, NoDerivative, ZeroDerivative};

fn identity_function<'dae>(
    dae: &mut DaeConstruction<'dae>,
    name: &str,
    inputs: &[ValueTypeId<'dae>],
    results: &[usize],
    at: DaeProvenance,
) -> Result<FunctionId<'dae>, DaeConstructionError> {
    let signature = FunctionSignature::new(
        VarName::new(name),
        inputs.iter().copied(),
        results.iter().map(|index| inputs[*index]),
        at,
    );
    dae.function(signature, |dae, reservation| {
        let parameters = (0..inputs.len())
            .map(|ordinal| {
                dae.functions(|functions| {
                    functions.parameter(
                        &reservation,
                        VarName::new(format!("arg{ordinal}")),
                        ordinal,
                        at,
                    )
                })
            })
            .collect::<Result<Vec<_>, _>>()?;
        let outputs = (0..results.len())
            .map(|ordinal| {
                dae.functions(|functions| {
                    functions.output(
                        &reservation,
                        VarName::new(format!("out{ordinal}")),
                        ordinal,
                        at,
                    )
                })
            })
            .collect::<Result<Vec<_>, _>>()?;
        let mut body = dae.functions(|functions| functions.begin(reservation, at))?;
        for (output, input) in outputs.into_iter().zip(results) {
            let value = dae.expressions(|expressions| {
                expressions.at(at).function_parameter(parameters[*input])
            })?;
            dae.functions(|functions| functions.assign(&mut body, output, value, at))?;
        }
        dae.functions(|functions| functions.define(body, at))
    })
    .map(|(function, ())| function)
}

fn derivative_call_fixture(extent: u32) -> (Dae, [usize; 5]) {
    let source = TestSource::new("f(x); df(x, dx); ddf(x, dx, ddx);");
    let at = source.source("f(x)", 0);
    let mut calls = [0; 5];
    let model = Dae::construct(source.map, |model| {
        let ty = model.types(|t| t.derived(ValueType::array(ScalarType::Real, [extent]), at))?;
        let f = identity_function(model, "f", &[ty], &[0, 0], at)?;
        let df = identity_function(model, "df", &[ty, ty], &[1, 1], at)?;
        let ddf = identity_function(model, "ddf", &[ty, ty, ty], &[2, 2], at)?;
        model.functions(|functions| {
            let link = functions.first_derivative(f, df, [Differentiate], 0, at)?;
            functions.next_derivative(df, link, ddf, [Differentiate; 2], 0, at)
        })?;
        let vars = model.variables(|v| {
            ["x", "dx", "ddx"]
                .into_iter()
                .map(|name| v.parameter(VarName::new(name), ty, at, Default::default()))
                .collect::<Result<Vec<_>, _>>()
        })?;
        model.expressions(|e| {
            let args = vars
                .into_iter()
                .map(|v| e.at(at).coordinate(CoordinateInput::Parameter(v)))
                .collect::<Result<Vec<_>, _>>()?;
            let primal = e.at(at).call(f, 0, [args[0]])?;
            let ordinary = e.at(at).call(df, 0, args[..2].iter().copied())?;
            assert!(
                e.at(at)
                    .differentiated_call(ordinary, 0, args.iter().copied())
                    .is_err()
            );
            assert!(
                e.at(at)
                    .differentiated_call(primal, 0, [args[1], args[0]])
                    .is_err()
            );
            assert!(e.at(at).differentiated_call(primal, 0, [args[0]]).is_err());
            assert!(
                e.at(at)
                    .differentiated_call(primal, 99, args[..2].iter().copied())
                    .is_err()
            );
            let first = e
                .at(at)
                .differentiated_call(primal, 0, args[..2].iter().copied())?;
            let second = e
                .at(at)
                .differentiated_call(first, 0, args.iter().copied())?;
            let projection = e.at(at).call_projection(second, ddf, 1, args)?;
            calls = [primal, ordinary, first, second, projection].map(|id| id.index() as usize);
            Ok(())
        })
    })
    .unwrap();
    (model, calls)
}

#[test]
fn differentiated_calls_in_function_bodies_roundtrip() {
    let source = TestSource::new("function wrapper input Real x, dx; output Real y; end wrapper;");
    let at = source.source("wrapper", 0);
    let model = Dae::construct(source.map, |model| {
        let ty = model.types(|t| t.derived(ValueType::array(ScalarType::Real, [3]), at))?;
        let f = identity_function(model, "f", &[ty], &[0], at)?;
        let df = identity_function(model, "df", &[ty, ty], &[1], at)?;
        model.functions(|functions| functions.first_derivative(f, df, [Differentiate], 0, at))?;
        let signature = FunctionSignature::new(VarName::new("wrapper"), [ty; 2], [ty], at);
        model.function(signature, |model, reservation| {
            let args = wrapper_arguments(model, &reservation, at)?;
            let out = model.functions(|f| f.output(&reservation, VarName::new("y"), 0, at))?;
            let value = model.expressions(|e| {
                let primal = e.at(at).call(f, 0, [args[0]])?;
                e.at(at).differentiated_call(primal, 0, args)
            })?;
            let mut body = model.functions(|f| f.begin(reservation, at))?;
            model.functions(|f| f.assign(&mut body, out, value, at))?;
            model.functions(|f| f.define(body, at))
        })?;
        Ok(())
    })
    .unwrap();
    let json = serde_json::to_string(&model).unwrap();
    let decoded: Dae = serde_json::from_str(&json).unwrap();
    assert_eq!(serde_json::to_string(&decoded).unwrap(), json);
    let binary = bincode::serialize(&model).unwrap();
    let decoded: Dae = bincode::deserialize(&binary).unwrap();
    assert_eq!(bincode::serialize(&decoded).unwrap(), binary);
}

fn wrapper_arguments<'dae>(
    model: &mut DaeConstruction<'dae>,
    reservation: &FunctionReservation<'_, 'dae>,
    at: DaeProvenance,
) -> Result<Vec<ExprId<'dae>>, DaeConstructionError> {
    ["x", "dx"]
        .into_iter()
        .enumerate()
        .map(|(ordinal, name)| {
            let p =
                model.functions(|f| f.parameter(reservation, VarName::new(name), ordinal, at))?;
            model.expressions(|e| e.at(at).function_parameter(p))
        })
        .collect()
}

#[test]
fn differentiated_calls_preserve_chain_prefix_and_compact_wire() {
    let counts = [1, 3, 4096].map(|extent| {
        let (model, calls) = derivative_call_fixture(extent);
        let json = serde_json::to_string(&model).unwrap();
        let decoded: Dae = serde_json::from_str(&json).unwrap();
        assert_eq!(serde_json::to_string(&decoded).unwrap(), json);
        let binary = bincode::serialize(&model).unwrap();
        let decoded: Dae = bincode::deserialize(&binary).unwrap();
        assert_eq!(bincode::serialize(&decoded).unwrap(), binary);
        decoded.inspect(|view| {
            let histories = calls.map(|i| {
                view.expression(view.expression_id(i).unwrap())
                    .unwrap()
                    .call_derivative()
                    .map(call_history)
            });
            assert_eq!(
                histories,
                [
                    None,
                    None,
                    Some((calls[0], 0, 0)),
                    Some((calls[2], 1, 0)),
                    Some((calls[2], 1, 0))
                ]
            );
            view.expression_count()
        })
    });
    assert!(counts.windows(2).all(|pair| pair[0] == pair[1]));
}

fn call_history((source, link): (ExprId<'_>, FunctionDerivativeId<'_>)) -> (usize, u32, u32) {
    (
        source.index() as usize,
        link.function().index(),
        link.ordinal(),
    )
}

#[test]
fn differentiated_call_wire_rejects_forged_origin_and_prefix() {
    let (model, calls) = derivative_call_fixture(3);
    let wire = serde_json::to_value(model).unwrap();
    for source in [calls[1], calls[3], 9999] {
        let mut bad = wire.clone();
        bad["storage"]["expressions"]["nodes"][calls[3]]["call"]["derivative"][0] = source.into();
        assert!(serde_json::from_value::<Dae>(bad).is_err());
    }
    let mut bad = wire.clone();
    bad["storage"]["expressions"]["nodes"][calls[2]]["call"]["function"] = 0.into();
    assert!(serde_json::from_value::<Dae>(bad).is_err());
    let mut bad = wire.clone();
    bad["storage"]["expressions"]["nodes"][calls[4]]["call"]["derivative"] =
        serde_json::Value::Null;
    assert!(serde_json::from_value::<Dae>(bad).is_err());
    let mut bad = wire.clone();
    bad["storage"]["expressions"]["nodes"][calls[2]]["call"]
        .as_object_mut()
        .unwrap()
        .remove("derivative");
    assert!(serde_json::from_value::<Dae>(bad).is_err());
    let mut bad = wire;
    let prefix: usize = bad["storage"]["expressions"]["nodes"]
        .as_array()
        .unwrap()
        .iter()
        .take(calls[2])
        .filter_map(|node| node.get("call"))
        .map(|call| call["operand_count"].as_u64().unwrap() as usize)
        .sum();
    bad["storage"]["expressions"]["operands"][prefix] = 1.into();
    assert!(serde_json::from_value::<Dae>(bad).is_err());
}

fn derivative_fixture() -> Dae {
    let source = TestSource::new("function f derivative(noDerivative=arg2)=df;");
    let at = source.source("derivative(noDerivative=arg2)=df", 0);
    Dae::construct(source.map, |dae| {
        let vector =
            dae.types(|types| types.derived(ValueType::array(ScalarType::Real, [3]), at))?;
        let integer =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Integer), at))?;
        let real = dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), at))?;
        let primal = identity_function(dae, "f", &[vector, integer, real], &[0, 1, 2], at)?;
        let first = identity_function(dae, "df", &[vector, integer, real, vector], &[3, 2], at)?;
        let second = identity_function(
            dae,
            "df_general",
            &[vector, integer, real, vector, real],
            &[3, 4],
            at,
        )?;
        dae.functions(|functions| {
            functions.first_derivative(
                primal,
                first,
                [Differentiate, Differentiate, NoDerivative],
                0,
                at,
            )
        })?;
        dae.functions(|functions| {
            functions
                .first_derivative(primal, second, [Differentiate; 3], 1, at)
                .map(|_| ())
        })
    })
    .unwrap()
}

fn assert_derivative_views(dae: &Dae) {
    dae.inspect(|view| {
        let source = view.function_id(0).unwrap();
        let primal = view.function(source).unwrap();
        let links = primal.derivatives().collect::<Vec<_>>();
        assert_eq!(links.len(), 2);
        assert_eq!(links[0].source(), source);
        assert_eq!(links[0].target(), view.function_id(1).unwrap());
        assert_eq!(links[1].target(), view.function_id(2).unwrap());
        assert_eq!(
            links[0].inputs(),
            [Differentiate, Differentiate, NoDerivative]
        );
        assert_eq!(links[0].tangent_inputs().collect::<Vec<_>>(), [0]);
        assert_eq!(links[1].tangent_inputs().collect::<Vec<_>>(), [0, 2]);
        assert_eq!(links[0].result(0), Some(0));
        assert_eq!(links[0].result(1), None);
        assert_eq!(links[0].result(2), Some(1));
        assert_eq!(links[0].result(3), None);
    });
}

#[test]
fn first_derivative_preserves_priority_shapes_roles_and_checked_wire() {
    let dae = derivative_fixture();
    assert_derivative_views(&dae);
    let wire = serde_json::to_string(&dae).unwrap();
    let decoded = serde_json::from_str::<Dae>(&wire).unwrap();
    assert_derivative_views(&decoded);
    assert_eq!(wire, serde_json::to_string(&decoded).unwrap());
    let binary = bincode::serialize(&dae).unwrap();
    let decoded = bincode::deserialize::<Dae>(&binary).unwrap();
    assert_derivative_views(&decoded);
    assert_eq!(binary, bincode::serialize(&decoded).unwrap());
}

#[test]
fn first_derivative_wire_rejects_foreign_target_changed_roles_and_missing_contract() {
    let wire = serde_json::to_value(derivative_fixture()).unwrap();
    let mut foreign = wire.clone();
    foreign["storage"]["functions"][0]["derivatives"][0]["target"] = 999.into();
    assert!(serde_json::from_value::<Dae>(foreign).is_err());
    let mut changed = wire.clone();
    changed["storage"]["functions"][0]["derivatives"][0]["inputs"] =
        serde_json::to_value([Differentiate; 3]).unwrap();
    assert!(serde_json::from_value::<Dae>(changed).is_err());
    let mut missing = wire.clone();
    missing["storage"]["functions"][0]
        .as_object_mut()
        .unwrap()
        .remove("derivatives");
    assert!(serde_json::from_value::<Dae>(missing).is_err());
    let mut old = wire;
    old["schema_version"] = 33.into();
    assert!(serde_json::from_value::<Dae>(old).is_err());
}

fn scalar_derivative(
    inputs: &[FunctionDerivativeInput],
    target_shapes: &[&[u32]],
) -> Result<Dae, DaeConstructionError> {
    let source = TestSource::new("derivative = df");
    let at = source.source("derivative = df", 0);
    Dae::construct(source.map, |dae| {
        let real = dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), at))?;
        let target_types = target_shapes
            .iter()
            .map(|shape| {
                dae.types(|types| {
                    types.derived(ValueType::array(ScalarType::Real, shape.to_vec()), at)
                })
            })
            .collect::<Result<Vec<_>, _>>()?;
        let primal = identity_function(dae, "f", &[real, real], &[0], at)?;
        let target = identity_function(dae, "df", &target_types, &[0], at)?;
        dae.functions(|functions| {
            functions
                .first_derivative(primal, target, inputs.iter().copied(), 0, at)
                .map(|_| ())
        })
    })
}

#[test]
fn first_derivative_rejects_incomplete_roles_and_wrong_tangent_abi() {
    for (roles, shapes) in [
        (vec![Differentiate], vec![&[][..]; 3]),
        (vec![Differentiate; 2], vec![&[][..]; 3]),
        (vec![Differentiate, NoDerivative], vec![&[][..]; 4]),
        (
            vec![Differentiate, NoDerivative],
            vec![&[][..], &[][..], &[1][..]],
        ),
        (
            vec![Differentiate, NoDerivative],
            vec![&[2][..], &[][..], &[][..]],
        ),
    ] {
        assert!(matches!(
            scalar_derivative(&roles, &shapes),
            Err(DaeConstructionError::InvalidFunctionDerivative { .. })
        ));
    }
    let dae = scalar_derivative(&[ZeroDerivative, Differentiate], &[&[], &[], &[]]).unwrap();
    dae.inspect(|view| {
        let link = view
            .function(view.function_id(0).unwrap())
            .unwrap()
            .derivatives()
            .next()
            .unwrap();
        assert_eq!(link.inputs(), [ZeroDerivative, Differentiate]);
        assert_eq!(link.tangent_inputs().collect::<Vec<_>>(), [1]);
    });
}

#[test]
fn first_derivative_record_tangents_omit_non_real_fields_recursively() {
    let source = TestSource::new("derivative = df");
    let at = source.source("derivative = df", 0);
    let dae = Dae::construct(source.map, |dae| {
        let real = dae.types(|types| types.derived(ValueType::array(ScalarType::Real, [3]), at))?;
        let boolean =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Boolean), at))?;
        let record = dae.types(|types| {
            types.record(
                VarName::new("R"),
                [(VarName::new("v"), real), (VarName::new("valid"), boolean)],
                at,
            )
        })?;
        let tangent =
            dae.types(|types| types.record(VarName::new("dR"), [(VarName::new("v"), real)], at))?;
        let outer = dae.types(|types| {
            types.record_array(
                VarName::new("Outer"),
                [(VarName::new("r"), record), (VarName::new("flag"), boolean)],
                [2],
                at,
            )
        })?;
        let outer_tangent = dae.types(|types| {
            types.record_array(
                VarName::new("dOuter"),
                [(VarName::new("r"), tangent)],
                [2],
                at,
            )
        })?;
        let primal = identity_function(dae, "f", &[outer], &[0], at)?;
        let target = identity_function(dae, "df", &[outer, outer_tangent], &[1], at)?;
        let invalid = identity_function(dae, "df_bad", &[outer, outer], &[1], at)?;
        let rejected = dae.functions(|functions| {
            functions.first_derivative(primal, invalid, [Differentiate], 0, at)
        });
        assert!(matches!(
            rejected,
            Err(DaeConstructionError::InvalidFunctionDerivative { .. })
        ));
        dae.functions(|functions| {
            functions
                .first_derivative(primal, target, [Differentiate], 0, at)
                .map(|_| ())
        })
    })
    .unwrap();
    let decoded: Dae = serde_json::from_value(serde_json::to_value(&dae).unwrap()).unwrap();
    decoded.inspect(|view| {
        let link = view
            .function(view.function_id(0).unwrap())
            .unwrap()
            .derivatives()
            .next()
            .unwrap();
        assert_eq!(link.tangent_inputs().collect::<Vec<_>>(), [0]);
        assert_eq!(link.result(0), Some(0));
    });
}

fn external_function<'dae>(
    dae: &mut DaeConstruction<'dae>,
    inputs: usize,
    real: ValueTypeId<'dae>,
    purity: FunctionPurity,
    at: DaeProvenance,
) -> Result<FunctionId<'dae>, DaeConstructionError> {
    let signature =
        FunctionSignature::new(VarName::new("external_f"), vec![real; inputs], [real], at);
    dae.function(signature, |dae, reservation| {
        let mut arguments = Vec::new();
        for ordinal in 0..inputs {
            let parameter = dae.functions(|functions| {
                functions.parameter(
                    &reservation,
                    VarName::new(format!("arg{ordinal}")),
                    ordinal,
                    at,
                )
            })?;
            let expression =
                dae.expressions(|expressions| expressions.at(at).function_parameter(parameter))?;
            arguments.push(ExternalArgument::Input(expression));
        }
        let output =
            dae.functions(|functions| functions.output(&reservation, VarName::new("out"), 0, at))?;
        let body = ExternalFunctionBody::new(
            purity,
            ExternalLanguage::C,
            VarName::new("f"),
            arguments,
            Some(output),
            ExternalLinkage::new([], None, None, None),
        );
        dae.functions(|functions| functions.define_external(reservation, body, at))
    })
    .map(|(function, ())| function)
}

#[test]
fn first_derivative_requires_pure_original_and_derivative_functions() {
    for (original_purity, derivative_purity) in [
        (FunctionPurity::Pure, FunctionPurity::Pure),
        (FunctionPurity::Pure, FunctionPurity::Impure),
        (FunctionPurity::Impure, FunctionPurity::Pure),
    ] {
        let source = TestSource::new("derivative = df");
        let at = source.source("derivative = df", 0);
        let result = Dae::construct(source.map, |dae| {
            let real = dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), at))?;
            let original = external_function(dae, 1, real, original_purity, at)?;
            let derivative = external_function(dae, 2, real, derivative_purity, at)?;
            dae.functions(|functions| {
                functions
                    .first_derivative(original, derivative, [Differentiate], 0, at)
                    .map(|_| ())
            })
        });
        if original_purity.is_pure() && derivative_purity.is_pure() {
            assert!(result.is_ok());
        } else {
            assert!(matches!(
                result,
                Err(DaeConstructionError::InvalidFunctionDerivative { .. })
            ));
        }
    }
}

#[test]
fn higher_derivatives_require_the_exact_chain_and_append_only_latest_tangents() {
    let source = TestSource::new("derivative(order=2)=ddf");
    let at = source.source("derivative(order=2)=ddf", 0);
    let dae = Dae::construct(source.map, |dae| {
        let vector =
            dae.types(|types| types.derived(ValueType::array(ScalarType::Real, [3]), at))?;
        let real = dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), at))?;
        let f = identity_function(dae, "f", &[vector, real], &[1], at)?;
        let df = identity_function(dae, "df", &[vector, real, real], &[2], at)?;
        let ddf = identity_function(dae, "ddf", &[vector, real, real, real], &[3], at)?;
        let first = dae.functions(|functions| {
            functions.first_derivative(f, df, [ZeroDerivative, Differentiate], 5, at)
        })?;
        assert!(
            dae.functions(|functions| functions.first_derivative(
                df,
                ddf,
                [ZeroDerivative, Differentiate, Differentiate],
                0,
                at
            ))
            .is_err(),
            "an ordinary first derivative needs both der(arg1) and der(arg2)"
        );
        assert!(
            dae.functions(|functions| functions.next_derivative(
                f,
                first,
                ddf,
                [ZeroDerivative, Differentiate],
                0,
                at
            ))
            .is_err(),
            "the predecessor must derive the declaring function"
        );
        assert!(
            dae.functions(|functions| functions.next_derivative(
                df,
                first,
                ddf,
                [Differentiate; 3],
                0,
                at
            ))
            .is_err(),
            "zeroDerivative must remain consistent in common inputs"
        );
        dae.functions(|functions| {
            functions.next_derivative(
                df,
                first,
                ddf,
                [ZeroDerivative, Differentiate, Differentiate],
                0,
                at,
            )
        })
        .map(|_| ())
    })
    .unwrap();
    let wire = serde_json::to_value(&dae).unwrap();
    let decoded = serde_json::from_value::<Dae>(wire.clone()).unwrap();
    decoded.inspect(|view| {
        let first = view
            .function(view.function_id(0).unwrap())
            .unwrap()
            .derivatives()
            .next()
            .unwrap();
        let second = view
            .function(view.function_id(1).unwrap())
            .unwrap()
            .derivatives()
            .next()
            .unwrap();
        assert_eq!(first.previous(), None);
        assert_eq!(first.order(), 1);
        assert_eq!(first.priority(), 5);
        assert_eq!(second.previous(), Some(first.id()));
        assert_eq!(second.order(), 2);
        assert_eq!(second.priority(), 0);
        assert_eq!(second.tangent_inputs().collect::<Vec<_>>(), [2]);
    });
    let binary = bincode::serialize(&dae).unwrap();
    let decoded: Dae = bincode::deserialize(&binary).unwrap();
    assert_eq!(bincode::serialize(&decoded).unwrap(), binary);
    let mut cycle = wire;
    cycle["storage"]["functions"][1]["derivatives"][0]["previous"] = serde_json::json!([1, 0]);
    assert!(serde_json::from_value::<Dae>(cycle).is_err());
}
