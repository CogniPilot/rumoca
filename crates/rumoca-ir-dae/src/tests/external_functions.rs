use super::*;

const EXTERNAL_SOURCE: &str = "pure function f input Real u; output Real y; output Real s; \
     external \"C\" y = my_func(u, s); end f;";

fn linkage() -> ExternalLinkage {
    ExternalLinkage::new(
        ["ModelicaExternalC".to_string()],
        Some("#include \"my_func.h\"".to_string()),
        Some("modelica://Test/Resources".to_string()),
        None,
    )
}

/// Build the reference fixture: one pure external function whose declaration
/// binds `y` through the return form and `s` through an output argument.
fn external_fixture() -> Dae {
    external_fixture_with_purity(FunctionPurity::Pure)
}

fn external_fixture_with_purity(purity: FunctionPurity) -> Dae {
    let source = TestSource::new(EXTERNAL_SOURCE);
    let function_at = source.source("pure function f", 0);
    let input_at = source.source("input Real u", 0);
    let first_output_at = source.source("output Real y", 0);
    let second_output_at = source.source("output Real s", 0);
    let external_at = source.source("external \"C\" y = my_func(u, s)", 0);
    let argument_at = source.source("u, s", 0);
    Dae::construct(source.map, |dae| {
        let real =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), function_at))?;
        dae.function(
            FunctionSignature::new(VarName::new("f"), [real], [real, real], function_at),
            |dae, reservation| {
                let parameter = dae.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("u"), 0, input_at)
                })?;
                let first = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, first_output_at)
                })?;
                let second = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("s"), 1, second_output_at)
                })?;
                let argument = dae.expressions(|expressions| {
                    expressions
                        .at(argument_at)
                        .coordinate(CoordinateInput::FunctionParameter(parameter))
                })?;
                let body = ExternalFunctionBody::new(
                    purity,
                    ExternalLanguage::C,
                    VarName::new("my_func"),
                    [
                        ExternalArgument::Input(argument),
                        ExternalArgument::Output(second),
                    ],
                    Some(first),
                    linkage(),
                );
                dae.functions(|functions| functions.define_external(reservation, body, external_at))
            },
        )
        .map(|_| ())
    })
    .expect("a checked external interface defines its reserved function")
}

#[test]
fn pure_external_interface_is_a_checked_purity_bearing_callable() {
    let dae = external_fixture();
    dae.inspect(|view| {
        let function = view
            .function(view.function_id(0).expect("the fixture owns one function"))
            .expect("the branded function resolves");
        assert!(function.is_external());
        assert_eq!(function.statements().count(), 0);
        assert!(function.result_values().is_empty());
        let external = function.external().expect("the body is external");
        assert_eq!(external.purity(), FunctionPurity::Pure);
        assert_eq!(external.language(), ExternalLanguage::C);
        assert_eq!(external.symbol().as_str(), "my_func");
        assert_eq!(external.linkage().libraries(), ["ModelicaExternalC"]);
        assert_eq!(external.linkage().include(), Some("#include \"my_func.h\""));
        assert_eq!(
            external.linkage().include_directory(),
            Some("modelica://Test/Resources")
        );
        assert_eq!(external.linkage().library_directory(), None);
        assert_eq!(external.argument_count(), 2);
        let arguments = external.arguments().collect::<Vec<_>>();
        assert!(matches!(arguments[0], ExternalArgumentView::Input(_)));
        assert!(matches!(arguments[1], ExternalArgumentView::Output(_)));
        assert!(external.result().is_some());
    });
}

#[test]
fn external_interface_round_trips_through_the_checked_wire() {
    let dae = external_fixture();
    let encoded = serde_json::to_string(&dae).expect("checked DAE serializes");
    let decoded: Dae = serde_json::from_str(&encoded).expect("external replay reconstructs");
    assert_eq!(
        serde_json::to_string(&decoded).expect("replayed DAE serializes"),
        encoded,
        "external interfaces have one canonical wire representation"
    );

    let binary = bincode::serialize(&dae).expect("external interface serializes");
    let replayed: Dae = bincode::deserialize(&binary).expect("external interface reconstructs");
    assert_eq!(
        bincode::serialize(&replayed).unwrap(),
        binary,
        "binary external interfaces have one canonical representation"
    );

    let canonical: serde_json::Value = serde_json::from_str(&encoded).unwrap();
    let function = canonical["storage"]["functions"][0]
        .as_object()
        .expect("functions serialize as records");
    assert!(
        function["statements"].as_array().unwrap().is_empty(),
        "an external body owns no Modelica statement"
    );
    let external = function["external"]
        .as_object()
        .expect("an external body serializes its interface");
    assert_eq!(external["symbol"], serde_json::json!("my_func"));
    assert_eq!(external["purity"], serde_json::json!("pure"));
    assert_eq!(external["language"], serde_json::json!("c"));
}

/// Replay reconstructs the declared purity, it does not re-derive one.
///
/// MLS §12.3 purity is a body fact a replayed DAE cannot recompute: an impure
/// body that came back pure would license exactly the optimizations MLS
/// forbids ("A tool is not allowed to perform any optimizations on function
/// calls to an impure function"). The impure direction is the one omission
/// silently produces, so it is proven on the wire in both encodings.
#[test]
fn an_impure_external_interface_replays_as_impure() {
    let dae = external_fixture_with_purity(FunctionPurity::Impure);
    dae.inspect(|view| {
        let function = view.function(view.function_id(0).unwrap()).unwrap();
        assert_eq!(
            function.external().unwrap().purity(),
            FunctionPurity::Impure
        );
    });

    let encoded = serde_json::to_string(&dae).expect("checked DAE serializes");
    let canonical: serde_json::Value = serde_json::from_str(&encoded).unwrap();
    assert_eq!(
        canonical["storage"]["functions"][0]["external"]["purity"],
        serde_json::json!("impure")
    );

    let decoded: Dae = serde_json::from_str(&encoded).expect("external replay reconstructs");
    decoded.inspect(|view| {
        let function = view.function(view.function_id(0).unwrap()).unwrap();
        assert_eq!(
            function.external().unwrap().purity(),
            FunctionPurity::Impure,
            "replay carries the declared purity instead of defaulting it"
        );
    });

    let binary = bincode::serialize(&dae).expect("external interface serializes");
    let replayed: Dae = bincode::deserialize(&binary).expect("external interface reconstructs");
    replayed.inspect(|view| {
        let function = view.function(view.function_id(0).unwrap()).unwrap();
        assert_eq!(
            function.external().unwrap().purity(),
            FunctionPurity::Impure
        );
    });
}

#[test]
fn wire_rejects_an_external_body_that_also_claims_statements() {
    let dae = external_fixture();
    let mut wire = serde_json::to_value(&dae).expect("checked DAE serializes");
    wire["storage"]["functions"][0]["statements"] = serde_json::json!([{
        "assignment": { "target": 0, "rhs": 0, "provenance": { "source": { "span": null } } }
    }]);
    assert!(
        serde_json::from_value::<Dae>(wire).is_err(),
        "a function cannot own both a Modelica body and an external interface"
    );
}

#[test]
fn wire_rejects_a_forged_external_output_identity() {
    let dae = external_fixture();
    let mut wire = serde_json::to_value(&dae).expect("checked DAE serializes");
    wire["storage"]["functions"][0]["external"]["arguments"] = serde_json::json!([{ "output": 7 }]);
    assert!(
        serde_json::from_value::<Dae>(wire).is_err(),
        "an external argument cannot name a value the function never declared"
    );
}

#[test]
fn wire_rejects_an_external_interface_that_leaves_an_output_unproduced() {
    let dae = external_fixture();
    let mut wire = serde_json::to_value(&dae).expect("checked DAE serializes");
    wire["storage"]["functions"][0]["external"]
        .as_object_mut()
        .expect("the external interface is a record")
        .remove("result");
    assert!(
        serde_json::from_value::<Dae>(wire).is_err(),
        "every declared output needs exactly one external producer"
    );
}

#[test]
fn external_construction_rejects_an_unproduced_output() {
    let source = TestSource::new(EXTERNAL_SOURCE);
    let function_at = source.source("pure function f", 0);
    let first_output_at = source.source("output Real y", 0);
    let second_output_at = source.source("output Real s", 0);
    let external_at = source.source("external \"C\" y = my_func(u, s)", 0);
    let error = Dae::construct(source.map, |dae| {
        let real =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), function_at))?;
        dae.function(
            FunctionSignature::new(VarName::new("f"), [], [real, real], function_at),
            |dae, reservation| {
                let first = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, first_output_at)
                })?;
                dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("s"), 1, second_output_at)
                })?;
                let body = ExternalFunctionBody::new(
                    FunctionPurity::Impure,
                    ExternalLanguage::C,
                    VarName::new("my_func"),
                    [],
                    Some(first),
                    ExternalLinkage::default(),
                );
                dae.functions(|functions| functions.define_external(reservation, body, external_at))
            },
        )
        .map(|_| ())
    })
    .expect_err("an unproduced output cannot become a plausible default");
    assert!(matches!(
        error,
        DaeConstructionError::IncompleteDefinition {
            kind: "external function output",
            ..
        }
    ));
}

#[test]
fn external_construction_rejects_a_duplicate_output_producer() {
    let source = TestSource::new(EXTERNAL_SOURCE);
    let function_at = source.source("pure function f", 0);
    let first_output_at = source.source("output Real y", 0);
    let second_output_at = source.source("output Real s", 0);
    let external_at = source.source("external \"C\" y = my_func(u, s)", 0);
    let error = Dae::construct(source.map, |dae| {
        let real =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), function_at))?;
        dae.function(
            FunctionSignature::new(VarName::new("f"), [], [real, real], function_at),
            |dae, reservation| {
                let first = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, first_output_at)
                })?;
                let second = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("s"), 1, second_output_at)
                })?;
                let body = ExternalFunctionBody::new(
                    FunctionPurity::Pure,
                    ExternalLanguage::C,
                    VarName::new("my_func"),
                    [
                        ExternalArgument::Output(first),
                        ExternalArgument::Output(second),
                    ],
                    Some(first),
                    ExternalLinkage::default(),
                );
                dae.functions(|functions| functions.define_external(reservation, body, external_at))
            },
        )
        .map(|_| ())
    })
    .expect_err("one output cannot be produced at two ABI positions");
    assert!(matches!(
        error,
        DaeConstructionError::DuplicateDefinition {
            kind: "external function output",
            ..
        }
    ));
}

#[test]
fn external_construction_rejects_a_rendered_entry_point() {
    let source = TestSource::new(EXTERNAL_SOURCE);
    let function_at = source.source("pure function f", 0);
    let output_at = source.source("output Real y", 0);
    let external_at = source.source("external \"C\" y = my_func(u, s)", 0);
    let error = Dae::construct(source.map, |dae| {
        let real =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), function_at))?;
        dae.function(
            FunctionSignature::new(VarName::new("f"), [], [real], function_at),
            |dae, reservation| {
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, output_at)
                })?;
                let body = ExternalFunctionBody::new(
                    FunctionPurity::Pure,
                    ExternalLanguage::C,
                    VarName::new("Modelica.Math.my func"),
                    [],
                    Some(output),
                    ExternalLinkage::default(),
                );
                dae.functions(|functions| functions.define_external(reservation, body, external_at))
            },
        )
        .map(|_| ())
    })
    .expect_err("a rendered display name is not a foreign entry point");
    assert!(matches!(
        error,
        DaeConstructionError::InvalidExternalSymbol { .. }
    ));
}

#[test]
fn external_construction_rejects_a_model_coordinate_argument() {
    let source = TestSource::new(
        "Real m; pure function f output Real y; external \"C\" y = my_func(m); end f;",
    );
    let variable_at = source.source("Real m", 0);
    let function_at = source.source("pure function f", 0);
    let output_at = source.source("output Real y", 0);
    let argument_at = source.source("my_func(m)", 0);
    let external_at = source.source("external \"C\" y = my_func(m)", 0);
    let error = Dae::construct(source.map, |dae| {
        let real =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), function_at))?;
        let parameter = dae.variables(|variables| {
            variables.parameter(
                VarName::new("m"),
                real,
                variable_at,
                VariableAttributes::default(),
            )
        })?;
        dae.function(
            FunctionSignature::new(VarName::new("f"), [], [real], function_at),
            |dae, reservation| {
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, output_at)
                })?;
                let argument = dae.expressions(|expressions| {
                    expressions
                        .at(argument_at)
                        .coordinate(CoordinateInput::Parameter(parameter))
                })?;
                let body = ExternalFunctionBody::new(
                    FunctionPurity::Pure,
                    ExternalLanguage::C,
                    VarName::new("my_func"),
                    [ExternalArgument::Input(argument)],
                    Some(output),
                    ExternalLinkage::default(),
                );
                dae.functions(|functions| functions.define_external(reservation, body, external_at))
            },
        )
        .map(|_| ())
    })
    .expect_err("a model coordinate cannot escape into an external argument");
    assert!(matches!(
        error,
        DaeConstructionError::InvalidFunctionCoordinate { .. }
    ));
}

const NATIVE_SOURCE: &str = "pure function random input Integer stateIn[2]; output Real result; \
     output Integer stateOut[2]; external \"C\" ModelicaRandom_xorshift64star(stateIn, stateOut, \
     result); end random;";

/// A `ModelicaRandom_xorshift64star` declaration with the given purity, entry
/// point, and state extent, in the MSL argument order.
fn native_fixture(purity: FunctionPurity, symbol: &str, extent: u32) -> Dae {
    let source = TestSource::new(NATIVE_SOURCE);
    let function_at = source.source("pure function random", 0);
    let input_at = source.source("input Integer stateIn[2]", 0);
    let result_at = source.source("output Real result", 0);
    let state_at = source.source("output Integer stateOut[2]", 0);
    let external_at = source.source("external \"C\"", 0);
    let argument_at = source.source("stateIn, stateOut", 0);
    Dae::construct(source.map, |dae| {
        let real =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), function_at))?;
        let state = dae.types(|types| {
            types.derived(ValueType::array(ScalarType::Integer, [extent]), function_at)
        })?;
        dae.function(
            FunctionSignature::new(VarName::new("random"), [state], [real, state], function_at),
            |dae, reservation| {
                let parameter = dae.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("stateIn"), 0, input_at)
                })?;
                let result = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("result"), 0, result_at)
                })?;
                let state_out = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("stateOut"), 1, state_at)
                })?;
                let argument = dae.expressions(|expressions| {
                    expressions
                        .at(argument_at)
                        .coordinate(CoordinateInput::FunctionParameter(parameter))
                })?;
                let body = ExternalFunctionBody::new(
                    purity,
                    ExternalLanguage::C,
                    VarName::new(symbol),
                    [
                        ExternalArgument::Input(argument),
                        ExternalArgument::Output(state_out),
                        ExternalArgument::Output(result),
                    ],
                    None,
                    ExternalLinkage::default(),
                );
                dae.functions(|functions| functions.define_external(reservation, body, external_at))
            },
        )
        .map(|_| ())
    })
    .expect("a checked external interface defines its reserved function")
}

type NativeBinding = (rumoca_core::native_body::NativeBody, Vec<bool>, Vec<u32>);

/// The bound row, whether each input reads a function parameter, and the
/// result each output writes.
fn native_binding(dae: &Dae) -> Option<NativeBinding> {
    dae.inspect(|view| {
        let binding = view
            .function(view.function_id(0).unwrap())
            .unwrap()
            .external()
            .unwrap()
            .native_body()?;
        let inputs = binding
            .inputs()
            .map(|input| {
                matches!(
                    view.expression(input).unwrap().operation(),
                    ExpressionOperation::Coordinate(CoordinateView::FunctionParameter(_))
                )
            })
            .collect();
        Some((binding.body(), inputs, binding.results().to_vec()))
    })
}

/// SPEC_0040 DAE-C30: the proven binding maps each catalog input to the
/// argument it reads and each catalog output to the result it writes, and
/// wire replay derives it again rather than reading it.
#[test]
fn a_cataloged_interface_is_bound_to_its_native_body() {
    let dae = native_fixture(FunctionPurity::Pure, "ModelicaRandom_xorshift64star", 2);
    let binding = native_binding(&dae).expect("the interface matches its catalog row");
    assert_eq!(
        binding,
        (
            rumoca_core::native_body::NativeBody::Xorshift64Star,
            vec![true],
            vec![1, 0]
        )
    );
    let encoded = serde_json::to_string(&dae).unwrap();
    assert!(
        !encoded.contains("native"),
        "the binding is never a wire input"
    );
    let replayed: Dae = serde_json::from_str(&encoded).unwrap();
    assert_eq!(native_binding(&replayed), Some(binding));
}

#[test]
fn an_interface_outside_its_catalog_row_has_no_native_body() {
    for (purity, symbol, extent) in [
        (FunctionPurity::Impure, "ModelicaRandom_xorshift64star", 2),
        (FunctionPurity::Pure, "ModelicaRandom_xorshift64star", 4),
        (FunctionPurity::Pure, "ModelicaRandom_xorshift128plus", 2),
        (FunctionPurity::Pure, "my_xorshift64star", 2),
    ] {
        let dae = native_fixture(purity, symbol, extent);
        assert_eq!(native_binding(&dae), None, "{purity:?} {symbol} {extent}");
    }
}
