//! Issue489: unsupported foreign calls are source features, not broken Solve IR.
//! Acceptance stays with ordinary Modelica bodies and compiler-owned native
//! bodies (SPEC_0040 DAE-C30); an unused foreign declaration is not executed.
use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

fn check_foreign_call(qualifier: &str, external: &str, equations: &str, call: &str) {
    let source = format!(
        "model E\n  {qualifier} function f\n    input Real x;\n    output Real y;\n    {external}\n  end f;\n  {equations}\nend E;\n"
    );
    let compiled = Compiler::new()
        .model("E")
        .compile_str(&source, "issue489.mo")
        .expect("the external declaration is valid Modelica");
    let error = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 0.01,
            ..Default::default()
        },
    )
    .expect_err("this foreign implementation has no checked native body");
    assert_eq!(error.diagnostic_code(), "EL001", "{error}");
    let message = error.to_string();
    for word in ["external C function", "f", "sin"] {
        assert!(message.contains(word), "missing {word:?}: {message}");
    }
    assert!(!message.contains("invalid Solve IR contract"), "{message}");
    let span = error
        .source_span()
        .expect("the refusal retains source identity");
    assert!(!span.is_dummy());
    let call_start = source.rfind(call).expect("fixture call exists");
    let line_start = source[..call_start].rfind('\n').map_or(0, |p| p + 1);
    let line_end = source[call_start..]
        .find('\n')
        .map_or(source.len(), |p| call_start + p);
    assert!(
        line_start <= span.start.0
            && span.end.0 <= line_end
            && span.start.0 <= call_start
            && span.end.0 >= call_start + call.len(),
        "call-site span {:?} does not cover {call:?} on its own line",
        span
    );
}

#[test]
fn external_c_algebraic_call_names_the_function_at_its_source() {
    check_foreign_call(
        "pure",
        "external \"C\" y = sin(x);",
        "Real z;\nequation\n  z = f(time);",
        "f(time)",
    );
}

#[test]
fn external_c_state_derivative_call_keeps_its_source_diagnostic() {
    check_foreign_call(
        "pure",
        "external \"C\" y = sin(x);",
        "Real z(start=0,fixed=true);\nequation\n  der(z) = f(time);",
        "f(time)",
    );
}

#[test]
fn bare_external_c_call_is_not_an_internal_contract_error() {
    check_foreign_call(
        "",
        "external \"C\" y = sin(x);",
        "Real z;\nequation\n  z = f(time);",
        "f(time)",
    );
}

#[test]
fn external_c_include_does_not_change_the_diagnostic_category() {
    check_foreign_call(
        "pure",
        "external \"C\" y = sin(x) annotation(Include=\"/* diagnostic fixture */\");",
        "Real z;\nequation\n  z = f(time);",
        "f(time)",
    );
}

#[test]
fn nested_external_c_call_names_the_foreign_leaf_not_the_wrapper() {
    check_foreign_call(
        "pure",
        "external \"C\" y = sin(x);",
        "function wrapper\n  input Real t;\n  output Real value;\nalgorithm\n  value := f(t);\nend wrapper;\nReal z;\nequation\n  z = wrapper(time);",
        "f(t)",
    );
}

#[test]
fn unused_external_declaration_does_not_reject_a_modelica_body() {
    let source = r#"model UnusedForeign
  pure function foreign
    input Real x;
    output Real y;
    external "C" y = sin(x);
  end foreign;
  function twice
    input Real x;
    output Real y;
  algorithm
    y := 2*x;
  end twice;
  Real x(start=1,fixed=true);
  Real y;
equation
  der(x) = 1;
  y = twice(x);
end UnusedForeign;"#;
    let compiled = Compiler::new()
        .model("UnusedForeign")
        .compile_str(source, "UnusedForeign.mo")
        .expect("ordinary Modelica body and unused declaration compile");
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 0.01,
            ..Default::default()
        },
    )
    .expect("an unused foreign declaration does not require execution");
    let y = result
        .names
        .iter()
        .position(|n| n == "y")
        .expect("y is recorded");
    for (time, value) in result.times.iter().zip(&result.data[y]) {
        assert!((value - 2.0 * (1.0 + time)).abs() < 1e-8);
    }
}

#[test]
fn nested_foreign_diagnostic_keeps_the_leaf_identity_and_symbol() {
    let source = r#"model NamedLeaf
  pure function leaf
    input Real x;
    output Real y;
    external "C" y = sin(x);
  end leaf;
  function wrapper
    input Real x;
    output Real y;
  algorithm
    y := leaf(x);
  end wrapper;
  Real z;
equation
  z = wrapper(time);
end NamedLeaf;"#;
    let compiled = Compiler::new()
        .model("NamedLeaf")
        .compile_str(source, "NamedLeaf.mo")
        .expect("the external declaration and wrapper are valid Modelica");
    let error = simulate_dae_with_diagnostics(&compiled.dae, &SimOptions::default())
        .expect_err("the nested foreign implementation has no checked native body");
    assert_eq!(error.diagnostic_code(), "EL001", "{error}");
    let message = error.to_string();
    assert!(
        message.contains("function `NamedLeaf.leaf`") || message.contains("function `leaf`"),
        "the diagnostic must identify the foreign leaf: {message}"
    );
    assert!(message.contains("calls `sin`"), "{message}");
}
