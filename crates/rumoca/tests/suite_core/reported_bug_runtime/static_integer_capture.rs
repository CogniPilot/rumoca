//! Source Integer captures cannot silently acquire a rounded scalar ABI value.
use super::*;

fn source(kind: &str, value: &str) -> String {
    format!(
        "function capture input Real u; input {kind} k; output Real y; algorithm y:=u+mod(k,7); end capture; model IntegerCapture Real x(start=1,fixed=true); equation der(x)=capture(x,{value}); end IntegerCapture;"
    )
}

#[test]
fn integer_literal_call_capture_keeps_exact_values_or_refuses_the_scalar_abi() {
    for (literal, expected) in [("3", 4.0), ("9007199254740992", 5.0)] {
        let compiled = compile(&source("Integer", literal), "IntegerCapture");
        let native =
            rumoca_sim::lower_for_simulation_with_overrides(&compiled.dae, &SimOptions::default())
                .expect("exactly representable Integer input");
        let runtime = rumoca_solver::SolveRuntime::new(&native).unwrap();
        let report = runtime.eval_at(0.0, &[1.0], &native.parameters, 1e-10, 32);
        assert!(report.error.is_none(), "{literal}: {:?}", report.error);
        assert_eq!(report.derivatives.len(), 1);
        assert_eq!(report.derivatives[0].value, expected);
    }
    let compiled = compile(&source("Integer", "9007199254740993"), "IntegerCapture");
    let error = rumoca_sim::lower_solve_problem(&compiled.dae)
        .expect_err("scalar ABI cannot preserve this original Integer");
    assert!(
        error
            .to_string()
            .contains("not exactly representable in the scalar ABI"),
        "{error}"
    );
    assert!(
        error.source_span().is_some(),
        "the original capture retains its source fault"
    );
}

#[test]
fn explicit_integer_to_real_call_input_keeps_real_coercion_semantics() {
    let source = "function capture input Real u; input Real k; output Real y; algorithm y:=u+k/9007199254740992.0; end capture; model RealCapture Real x(start=1,fixed=true); equation der(x)=capture(x,9007199254740993); end RealCapture;";
    let compiled = compile(source, "RealCapture");
    let native =
        rumoca_sim::lower_for_simulation_with_overrides(&compiled.dae, &SimOptions::default())
            .expect("the checked Real conversion may round");
    let runtime = rumoca_solver::SolveRuntime::new(&native).unwrap();
    let report = runtime.eval_at(0.0, &[1.0], &native.parameters, 1e-10, 32);
    assert!(report.error.is_none(), "{:?}", report.error);
    assert_eq!(report.derivatives.len(), 1);
    assert_eq!(report.derivatives[0].value, 2.0);
}
