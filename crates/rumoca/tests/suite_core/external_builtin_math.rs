//! Compile-only proof for builtin external mathematical bodies (MLS §12.9).
use rumoca::Compiler;
use rumoca_ir_dae as dae;

fn source(prefix: &str, external: &str) -> String {
    format!(
        r#"model ExternalMath
  {prefix} function shifted
    input Real u;
    output Real y;
    {external}
  end shifted;
  Real x;
  Real y;
equation
  x = time;
  y = shifted(x);
end ExternalMath;"#
    )
}

fn compile(source: &str) -> std::sync::Arc<dae::Dae> {
    Compiler::new()
        .model("ExternalMath")
        .compile_str(source, "ExternalMath.mo")
        .expect("external interface compiles")
        .dae
}

fn assert_purity(model: &dae::Dae, expected: dae::FunctionPurity, symbol: &str) {
    model.inspect(|view| {
        let external = (0..view.function_count())
            .filter_map(|index| view.function(view.function_id(index)?))
            .find_map(|function| function.external())
            .expect("the external declaration is preserved");
        assert_eq!(external.purity(), expected);
        assert_eq!(external.language(), dae::ExternalLanguage::Builtin);
        assert_eq!(external.symbol().as_str(), symbol);
    });
}

#[test]
fn external_builtin_exp_lowers_without_explicit_purity() {
    let model = compile(&source("", r#"external "builtin" y = exp(2*u);"#));
    assert_purity(&model, dae::FunctionPurity::Impure, "exp");
    rumoca_phase_solve::lower_solve_problem(&model)
        .expect("checked builtin math lowers without running a simulation");
}

#[test]
fn external_builtin_exp_preserves_explicit_purity() {
    let model = compile(&source("pure", r#"external "builtin" y = exp(u);"#));
    assert_purity(&model, dae::FunctionPurity::Pure, "exp");
    rumoca_phase_solve::lower_solve_problem(&model).expect("pure builtin exp lowers");
}

#[test]
fn external_builtin_exp_rebuilds_its_proof_on_wire_replay() {
    let model = compile(&source("", r#"external "builtin" y = exp(2*u);"#));
    let encoded = serde_json::to_vec(&model).expect("encode checked DAE");
    let replayed: dae::Dae = serde_json::from_slice(&encoded).expect("replay checked DAE");
    assert_purity(&replayed, dae::FunctionPurity::Impure, "exp");
    rumoca_phase_solve::lower_solve_problem(&replayed)
        .expect("replayed builtin body lowers without simulation");
}

#[test]
fn external_builtin_exp_does_not_claim_other_foreign_bodies() {
    for external in [
        r#"external "C" y = exp(u);"#,
        r#"external "builtin" y = unknown(u);"#,
        r#"external "builtin" y = exp(u, u);"#,
        r#"external "builtin" y = exp({u, u});"#,
        r#"external "builtin" y = exp(true);"#,
    ] {
        let model = compile(&source("pure", external));
        assert!(
            rumoca_phase_solve::lower_solve_problem(&model).is_err(),
            "unsupported interface must remain refused: {external}"
        );
    }
}

fn assert_actual_argument(model: &dae::Dae, expected: rumoca_core::native_body::NativeBody) {
    model.inspect(|view| {
        let function = (0..view.function_count())
            .filter_map(|index| view.function(view.function_id(index)?))
            .find(|function| function.external().is_some())
            .expect("external function remains present");
        let body = function.external().unwrap().native_body()
            .expect("the interface has a construction-issued mathematical body");
        assert_eq!(body.body(), expected);
        assert_eq!(body.results(), &[0]);
        let mut inputs = body.inputs();
        let argument = inputs.next().expect("one mathematical argument");
        assert!(inputs.next().is_none());
        let dae::ExpressionOperation::Binary { operator, lhs, rhs } =
            view.expression(argument).unwrap().operation()
        else {
            panic!("exp must read the declared product, not the bare formal");
        };
        assert_eq!(operator, dae::BinaryOperator::Multiply);
        match view.expression(lhs).unwrap().operation() {
            dae::ExpressionOperation::Literal(dae::DaeLiteral::Integer(value)) => assert_eq!(*value, 2),
            dae::ExpressionOperation::Literal(dae::DaeLiteral::Real(value)) => assert_eq!(*value, 2.0),
            _ => panic!("the product retains its multiplier"),
        }
        let dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(parameter)) =
            view.expression(rhs).unwrap().operation()
        else {
            panic!("the product must read the declared parameter");
        };
        assert_eq!(parameter, function.parameters().next().unwrap().id());
    });
}

#[test]
fn external_builtin_exp_keeps_the_actual_argument_expression() {
    let model = compile(&source("", r#"external "builtin" y = exp(2*u);"#));
    assert_actual_argument(&model, rumoca_core::native_body::NativeBody::BuiltinExp);
    let encoded = serde_json::to_vec(&model).unwrap();
    let replayed: dae::Dae = serde_json::from_slice(&encoded).unwrap();
    assert_actual_argument(&replayed, rumoca_core::native_body::NativeBody::BuiltinExp);
}

#[test]
fn external_builtin_exp_rejects_non_scalar_real_results() {
    for (output, variable) in [
        ("output Integer y;", "Integer y;"),
        ("output Real y[2];", "Real y[2];"),
    ] {
        let text = source("pure", r#"external "builtin" y = exp(u);"#)
            .replace("output Real y;", output)
            .replace("\n  Real y;", &format!("\n  {variable}"));
        let model = compile(&text);
        model.inspect(|view| {
            assert!(
                (0..view.function_count())
                    .filter_map(|index| view.function(view.function_id(index)?))
                    .filter_map(|function| function.external())
                    .all(|external| external.native_body().is_none())
            );
        });
        assert!(rumoca_phase_solve::lower_solve_problem(&model).is_err());
    }
}

#[test]
fn external_builtin_exp_keeps_a_typed_owner_through_a_modelica_wrapper() {
    let text = source("", r#"external "builtin" y = exp(2*u);"#)
        .replace(
            "  Real x;",
            "  function wrapper\n    input Real u;\n    output Real y;\n  algorithm\n    y := shifted(3*u);\n  end wrapper;\n  Real x;",
        )
        .replace("y = shifted(x);", "y = wrapper(x);");
    let model = compile(&text);
    assert_actual_argument(&model, rumoca_core::native_body::NativeBody::BuiltinExp);
    rumoca_phase_solve::lower_solve_problem(&model)
        .expect("a Modelica wrapper must not inline an external body");
}

#[test]
fn external_builtin_math_preserves_argument_mapping_and_wire_replay() {
    use rumoca_core::native_body::NativeBody;
    for (symbol, body) in [
        ("exp", NativeBody::BuiltinExp),
        ("log10", NativeBody::BuiltinLog10),
        ("log", NativeBody::BuiltinLog),
        ("acos", NativeBody::BuiltinAcos),
    ] {
        let text = source("", &format!("external \"builtin\" y = {symbol}(2*u);"));
        let model = compile(&text);
        assert_purity(&model, dae::FunctionPurity::Impure, symbol);
        assert_actual_argument(&model, body);
        rumoca_phase_solve::lower_solve_problem(&model).expect("builtin math lowers");
        let replayed: dae::Dae =
            serde_json::from_slice(&serde_json::to_vec(&model).unwrap()).unwrap();
        assert_purity(&replayed, dae::FunctionPurity::Impure, symbol);
        assert_actual_argument(&replayed, body);
        rumoca_phase_solve::lower_solve_problem(&replayed).expect("replayed math lowers");
    }
}
