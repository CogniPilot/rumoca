//! Original modelica_models a41f7c0c clamp body, MLS11.2.2/11.2.6.
use rumoca::Compiler;

const SOURCE: &str = r#"
package Clamp
function saturate3 "Clamp 3-vector element-wise"
  input Real x[3];
  input Real x_min[3];
  input Real x_max[3];
  output Real y[3];
algorithm
  for i in 1:3 loop
    y[i] := if x[i] > x_max[i] then x_max[i]
            elseif x[i] < x_min[i] then x_min[i]
            else x[i];
  end for;
end saturate3;
model Run
  input Real x[3];
  output Real y[3] = saturate3(x, {-1,-1,-1}, {1,1,1});
end Run;
end Clamp;
"#;

#[test]
fn original_saturate3_exhaustive_element_clamp_defines_full_output() {
    Compiler::new()
        .model("Clamp.Run")
        .compile_str(SOURCE, "Clamp.mo")
        .unwrap();
}

#[test]
fn clamp_incomplete_domain_preserves_original_output_refusal() {
    let source = SOURCE.replace("for i in 1:3 loop", "for i in 1:2 loop");
    let error = Compiler::new()
        .model("Clamp.Run")
        .compile_str(&source, "Clamp.mo")
        .unwrap_err();
    assert!(
        error
            .to_string()
            .contains("without defining every declared element"),
        "{error}"
    );
}

#[test]
fn runtime_selected_clamp_without_fallback_stays_partial() {
    let source = SOURCE.replace("y[i] := if x[i] > x_max[i] then x_max[i]\n            elseif x[i] < x_min[i] then x_min[i]\n            else x[i];", "if x[i] > x_max[i] then y[i] := x_max[i]; elseif x[i] < x_min[i] then y[i] := x_min[i]; end if;");
    let error = Compiler::new()
        .model("Clamp.Run")
        .compile_str(&source, "Clamp.mo")
        .unwrap_err();
    let rumoca::CompilerError::CompileDiagnosticsError { failures, .. } = &error else {
        panic!("expected typed construction diagnostics: {error}");
    };
    let failure = failures
        .iter()
        .find(|failure| failure.model_name == "Clamp.Run")
        .unwrap();
    assert_eq!(failure.error_code.as_deref(), Some("ED019"));
    assert_eq!(
        failure.error,
        "unsupported Flat semantic owner `function conditional`: `saturate3` leaves output `y` without a definition on some branch"
    );
}

#[test]
fn exhaustive_clamp_with_different_symbolic_axes_stays_partial() {
    let source = SOURCE.replace(
        "y[i] := if x[i] > x_max[i] then x_max[i]\n            elseif x[i] < x_min[i] then x_min[i]\n            else x[i];",
        "if x[i] > x_max[i] then y[i] := x_max[i]; else if x[i] < x_min[i] then y[1] := x_min[i]; else y[i] := x[i]; end if; end if;",
    );
    let error = Compiler::new()
        .model("Clamp.Run")
        .compile_str(&source, "Clamp.mo")
        .unwrap_err();
    assert!(
        error
            .to_string()
            .contains("without defining every declared element"),
        "{error}"
    );
}
