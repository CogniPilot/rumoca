//! Element-wise operators over Integer arrays inside a function body
//! (MLS 3.7 §10.6). Scalar and element-wise power return Real even for
//! Integer operands; element-wise multiplication keeps Integer elements.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
model IntegerElementwise
  function f
    input Integer k;
    input Real t;
    output Real y;
  protected
    Real s[2] = {2, k} .^ 2;
    Real q = k ^ 3;
    Real r[2] = {2, 3} .^ 2;
    Real m[2] = {2, 3} .* 2;
  algorithm
    y := t*(s[1] + s[2] + q + r[1] + r[2] + m[1] + m[2]);
  end f;
  parameter Integer k = 3;
  Real y = f(k, time);
end IntegerElementwise;
"#;

#[test]
fn integer_element_wise_operators_lower_in_function_bodies() {
    let compiled = Compiler::new()
        .model("IntegerElementwise")
        .compile_str(SOURCE, "IntegerElementwise.mo")
        .unwrap_or_else(|error| panic!("IntegerElementwise compiles: {error:?}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("IntegerElementwise simulates: {error}"));
    let index = result.names.iter().position(|n| n == "y").expect("y");
    // 4 + 9 + 27 + 4 + 9 + 4 + 6 = 63.
    for (row, &time) in result.times.iter().enumerate() {
        assert!((result.data[index][row] - 63.0 * time).abs() < 1e-9);
    }
}

#[test]
fn integer_sinks_refuse_real_scalar_and_elementwise_power_during_typecheck() {
    for (real, integer) in [
        ("Real s[2] =", "Integer s[2] ="),
        ("Real q =", "Integer q ="),
    ] {
        let source = SOURCE.replace(real, integer);
        let parsed = rumoca_phase_parse::parse_to_ast(&source, "integer_power_sink.mo")
            .expect("the source parses before the type mismatch");
        let mut tree = rumoca_ir_ast::ClassTree::from_parsed(parsed);
        tree.source_map.add("integer_power_sink.mo", &source);
        let tree = rumoca_phase_resolve::resolve(rumoca_ir_ast::ParsedTree::new(tree))
            .expect("the original Integer operands resolve")
            .into_inner();
        let mut overlay = rumoca_phase_instantiate::instantiate_model(&tree, "IntegerElementwise")
            .expect("the original function declarations instantiate");
        rumoca_phase_typecheck::typecheck_instanced(&tree, &mut overlay, "IntegerElementwise")
            .expect_err("power returns Real and cannot initialize an Integer sink");
    }
}
