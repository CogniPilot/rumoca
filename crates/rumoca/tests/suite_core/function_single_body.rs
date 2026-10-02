//! MLS 3.7 §12.2: "A function can have at most one algorithm section or one
//! external function interface (not both), which, if present, is the body of
//! the function." A `redeclare function extends` that adds an algorithm
//! section to a base that already has one ends up with two and is refused
//! (EF035). A function that extends one with a body and only modifies it, as
//! MSL media functions do, keeps the inherited section and is accepted.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
package MB
  partial package PartialMed
    replaceable function prop
      input Real T;
      output Real y;
    algorithm
      y := T;
    end prop;
  end PartialMed;
  package A
    extends PartialMed;
    redeclare function extends prop
    algorithm
      y := 2*T + 1;
    end prop;
  end A;
  package B
    extends PartialMed;
  end B;
  function scaled
    extends B.prop(T(min = 0));
  end scaled;
  model Top
    Real y = A.prop(time);
  end Top;
  model Inherited
    Real y = scaled(time) + B.prop(time);
  end Inherited;
end MB;
"#;

#[test]
fn a_function_with_two_algorithm_sections_is_refused() {
    let error = Compiler::new()
        .model("MB.Top")
        .compile_str(SOURCE, "MB.mo")
        .expect_err("an inherited and an added algorithm section are two bodies");
    let message = error.to_string();
    assert!(
        message.contains("algorithm sections") && message.contains("prop"),
        "unexpected diagnostic: {message}"
    );
}

#[test]
fn a_function_inheriting_one_algorithm_section_is_accepted() {
    let compiled = Compiler::new()
        .model("MB.Inherited")
        .compile_str(SOURCE, "MB.mo")
        .unwrap_or_else(|error| panic!("MB.Inherited compiles: {error:?}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("MB.Inherited simulates: {error}"));
    let index = result.names.iter().position(|n| n == "y").expect("y");
    for (row, &time) in result.times.iter().enumerate() {
        assert!((result.data[index][row] - 2.0 * time).abs() < 1e-12);
    }
}
