//! MLS §10.5: constant function evaluation preserves retained slice axes.

use rumoca::Compiler;

const ROWS: &str = r#"
model ParameterRows
  function takeRows
    input Integer n;
    output Real y[n,2];
  protected
    Real cm[3,2] = [11,12;21,22;31,32];
  algorithm
    y := cm[1:n, :];
  end takeRows;
  parameter Real table[2,2] = takeRows(2);
  Real y[2,2] = table;
end ParameterRows;
"#;

const COLUMN: &str = r#"
model ParameterColumn
  function column
    output Real y[3];
  protected
    Real cm[3,3] = [11,12,13;21,22,23;31,32,33];
  algorithm
    y := cm[:, 2];
  end column;
  parameter Real values[3] = column();
  Real y[3] = values;
end ParameterColumn;
"#;

fn check(source: &str, model: &str, expected: &[(&str, f64)]) {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(source, "function_parameter_slicing.mo")
        .expect("a constant function slice must compile");
    let result = rumoca_sim::simulate_dae(
        &compiled.dae,
        &rumoca_sim::SimOptions {
            t_end: 0.2,
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .expect("a constant slice must simulate");
    for (name, expected) in expected {
        let index = result.names.iter().position(|n| n == name).unwrap();
        for actual in &result.data[index] {
            assert_eq!(actual, expected, "incorrect constant slice channel {name}");
        }
    }
}

#[test]
fn explicit_matrix_control_evaluates() {
    let source = ROWS.replace("cm[1:n, :]", "[11,12;21,22]");
    check(
        &source,
        "ParameterRows",
        &[("y[1,1]", 11.0), ("y[2,2]", 22.0)],
    );
}

#[test]
fn integer_range_retains_the_matrix_row_axis() {
    check(
        ROWS,
        "ParameterRows",
        &[
            ("y[1,1]", 11.0),
            ("y[1,2]", 12.0),
            ("y[2,1]", 21.0),
            ("y[2,2]", 22.0),
        ],
    );
}

#[test]
fn scalar_subscript_after_colon_selects_a_column() {
    check(
        COLUMN,
        "ParameterColumn",
        &[("y[1]", 12.0), ("y[2]", 22.0), ("y[3]", 32.0)],
    );
}
