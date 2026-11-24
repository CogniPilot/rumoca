//! MLS 3.7 §10.3.3: `fill(s, n1, n2, ...)` repeats the whole value `s`. An
//! array `s` keeps its own axes after the new ones, so `fill(s, n)` is
//! `{s, s, ..., s}`.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae};

fn simulated(name: &str, source: &str) -> rumoca_sim::SimResult {
    let compiled = Compiler::new()
        .model(name)
        .compile_str(source, &format!("{name}.mo"))
        .unwrap_or_else(|error| panic!("compile {name}: {error:#}"));
    simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("simulate {name}: {error:#}"))
}

fn final_value(result: &rumoca_sim::SimResult, name: &str) -> f64 {
    let index = result
        .names
        .iter()
        .position(|column| column == name)
        .unwrap_or_else(|| panic!("no column {name} in {:?}", result.names));
    *result.data[index].last().expect("a final sample")
}

/// Every row of `y` is `{c1, c2}`, where `c1` and `c2` are the final values
/// of the repeated value.
fn assert_rows(result: &rumoca_sim::SimResult, rows: &[&str], expected: [f64; 2]) {
    for row in rows {
        for (column, expected) in expected.iter().enumerate() {
            let name = format!("y[{row},{}]", column + 1);
            let found = final_value(result, &name);
            assert!((found - expected).abs() < 1.0e-9, "{name} = {found}");
        }
    }
}

#[test]
fn a_fill_of_an_array_repeats_the_whole_value() {
    for (name, declarations, equation, expected) in [
        (
            "FillOfVariable",
            "Real c[2] = {1 + time, 3};\n  Real y[3, 2];",
            "y = fill(c, 3);",
            [2.0, 3.0],
        ),
        (
            "FillOfConstructor",
            "Real y[3, 2];",
            "y = fill({1 + time, 3}, 3);",
            [2.0, 3.0],
        ),
        (
            "FillOfSlice",
            "parameter Real X[3] = {4, 5, 6};\n  Real y[3, 2];",
            "y = fill(X[2:3], 3);",
            [5.0, 6.0],
        ),
    ] {
        let source =
            format!("model {name}\n  {declarations}\nequation\n  {equation}\nend {name};\n");
        assert_rows(&simulated(name, &source), &["1", "2", "3"], expected);
    }
}

#[test]
fn further_extents_nest_outside_the_value() {
    let result = simulated(
        "FillTwoExtents",
        "model FillTwoExtents\n  Real c[2] = {1 + time, 3};\n  Real y[2, 3, 2];\n  Real total;\n\
         equation\n  y = fill(c, 2, 3);\n  total = sum(fill(c, 4));\nend FillTwoExtents;\n",
    );
    assert_rows(
        &result,
        &["1,1", "1,2", "1,3", "2,1", "2,2", "2,3"],
        [2.0, 3.0],
    );
    assert!((final_value(&result, "total") - 20.0).abs() < 1.0e-9);
}

#[test]
fn a_zero_extent_keeps_the_axes_of_the_value() {
    let result = simulated(
        "FillNoRows",
        "model FillNoRows\n  Real c[2] = {1 + time, 3};\n  Real z[0, 2];\n  Real y[1, 2];\n\
         equation\n  z = fill(c, 0);\n  y = fill(c, 1);\nend FillNoRows;\n",
    );
    assert_rows(&result, &["1"], [2.0, 3.0]);
}

#[test]
fn a_declaration_takes_its_dimensions_from_a_fill_of_an_array() {
    let result = simulated(
        "FillSizesDeclaration",
        "model FillSizesDeclaration\n  Real c[2] = {1 + time, 3};\n  Real y[:, :] = fill(c, 3);\n\
         end FillSizesDeclaration;\n",
    );
    assert_rows(&result, &["1", "2", "3"], [2.0, 3.0]);
}

#[test]
fn a_function_body_fills_with_an_array() {
    let result = simulated(
        "FillInFunction",
        "model FillInFunction\n  function rows\n    input Real c[2];\n    output Real y[3, 2];\n  \
         algorithm\n    y := fill(c, 3);\n  end rows;\n  Real c[2] = {1 + time, 3};\n  \
         Real y[3, 2] = rows(c);\nend FillInFunction;\n",
    );
    assert_rows(&result, &["1", "2", "3"], [2.0, 3.0]);
}

/// The initial equations of `Modelica.Fluid.Interfaces.PartialDistributedVolume`
/// have this shape: one start vector for every segment.
#[test]
fn an_initial_equation_starts_every_row_from_one_vector() {
    let result = simulated(
        "FillStartsStates",
        "model FillStartsStates\n  parameter Real x0[2] = {1, 2};\n  Real y[3, 2];\n\
         initial equation\n  y = fill(x0, 3);\nequation\n  der(y) = -y;\nend FillStartsStates;\n",
    );
    let decay = (-1.0_f64).exp();
    for row in ["1", "2", "3"] {
        for (column, start) in [1.0, 2.0].into_iter().enumerate() {
            let name = format!("y[{row},{}]", column + 1);
            let found = final_value(&result, &name);
            assert!((found - start * decay).abs() < 1.0e-4, "{name} = {found}");
        }
    }
}
