//! MLS 3.7 §12.4.4 definedness of a value a search loop writes together with
//! its exit flag.
//!
//! `Modelica.Math.Nonlinear.solveOneNonlinearEquation` runs
//! `while not found loop ... end while` and assigns its result only in the
//! pass that sets `found := true`. No single pass defines the result on every
//! path, yet it is defined once the loop has ended: the flag is false before
//! the loop, and every statement that sets it true sits beside an assignment
//! of the result. A flag that some other statement can set proves nothing,
//! and the function keeps its refusal.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimResult, simulate_dae_with_diagnostics};

/// A bisection for the cube root: `found` is false before the loop and set
/// only beside `u := ...`.
const SEARCH: &str = r#"
model Search
  function cubeRoot
    input Real c;
    output Real u;
  protected
    Real a = 0;
    Real b = 5;
    Real m;
    Boolean found = false;
  algorithm
    while not found loop
      m := (a + b)/2;
      if b - a < 1e-12 then
        found := true;
        u := m;
      elseif m^3 < c then
        a := m;
      else
        b := m;
      end if;
    end while;
  end cubeRoot;
  Real r = cubeRoot(2 + time);
end Search;
"#;

fn compile_and_simulate(source: &str) -> Result<SimResult, String> {
    let compiled = Compiler::new()
        .model("Search")
        .compile_str(source, "Search.mo")
        .map_err(|error| format!("{error:?}"))?;
    simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            dt: Some(0.5),
            ..SimOptions::default()
        },
    )
    .map_err(|error| format!("{error:?}"))
}

fn assert_cube_roots(result: &SimResult) {
    let column = result
        .names
        .iter()
        .position(|name| name == "r")
        .expect("the result records r");
    for (time, value) in result.times.iter().zip(&result.data[column]) {
        assert!(
            (value - (2.0 + time).cbrt()).abs() < 1e-9,
            "r({time}) = {value}"
        );
    }
}

#[test]
fn a_value_written_beside_the_exit_flag_is_defined_after_the_loop() {
    let result = compile_and_simulate(SEARCH).expect("the search compiles and simulates");
    assert_cube_roots(&result);
}

#[test]
fn a_flag_assigned_false_before_the_loop_proves_the_same() {
    let source = SEARCH
        .replace("Boolean found = false;", "Boolean found;")
        .replace(
            "  algorithm\n    while",
            "  algorithm\n    found := false;\n    while",
        );
    assert_ne!(source, SEARCH);
    let result = compile_and_simulate(&source).expect("the search compiles and simulates");
    assert_cube_roots(&result);
}

fn assert_undefined_result(source: &str) {
    assert_ne!(source, SEARCH);
    let error = compile_and_simulate(source).expect_err("the result has no definition");
    assert!(
        error.contains("returns `u` without defining every declared element"),
        "{error}"
    );
}

#[test]
fn a_flag_set_away_from_the_value_keeps_the_refusal() {
    // The loop can now end through a pass that never wrote `u`.
    assert_undefined_result(&SEARCH.replace(
        "      elseif m^3 < c then\n        a := m;",
        "      elseif m^3 < c then\n        a := m;\n        found := a > 4;",
    ));
    assert_undefined_result(&SEARCH.replace(
        "      elseif m^3 < c then\n        a := m;",
        "      elseif m^3 < c then\n        a := m;\n        found := true;",
    ));
}

#[test]
fn a_flag_that_may_be_true_before_the_loop_keeps_the_refusal() {
    // With `found` true on entry the loop runs no pass and `u` is never
    // written.
    assert_undefined_result(&SEARCH.replace("Boolean found = false;", "Boolean found = c > 3;"));
}

#[test]
fn a_value_written_in_another_arm_than_the_flag_keeps_the_refusal() {
    assert_undefined_result(&SEARCH.replace(
        "        found := true;\n        u := m;\n      elseif m^3 < c then\n        a := m;",
        "        found := true;\n      elseif m^3 < c then\n        a := m;\n        u := m;",
    ));
}

#[test]
fn a_value_every_pass_also_writes_keeps_its_own_definition() {
    // `u` is written at the head of each pass as well as beside the flag.
    let source = SEARCH.replace(
        "      m := (a + b)/2;\n",
        "      m := (a + b)/2;\n      u := m;\n",
    );
    assert_ne!(source, SEARCH);
    let result = compile_and_simulate(&source).expect("the search compiles and simulates");
    assert_cube_roots(&result);
}

#[test]
fn a_flag_set_after_the_loop_keeps_the_refusal() {
    // The loop can end through its interval test with `found` still false and
    // `u` unwritten; the assignment after it makes the flag true all the same.
    assert_undefined_result(
        &SEARCH
            .replace(
                "while not found loop",
                "while not found and b - a > 1e-6 loop",
            )
            .replace("    end while;\n", "    end while;\n    found := true;\n"),
    );
}

#[test]
fn an_element_written_beside_the_flag_keeps_the_refusal() {
    // Only `u[1]` is written in the pass that sets the flag.
    let source = SEARCH
        .replace("output Real u;", "output Real u[2];")
        .replace("        u := m;", "        u[1] := m;")
        .replace("Real r = cubeRoot", "Real r[2] = cubeRoot");
    assert_ne!(source, SEARCH);
    let error = compile_and_simulate(&source).expect_err("u[2] has no definition");
    assert!(error.contains("`u`"), "{error}");
}
