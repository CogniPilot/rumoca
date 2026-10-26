use rumoca_compile::compile::{Session, SessionConfig};
use rumoca_sim::{SimOptions, simulate_dae};

#[test]
fn scaled_cross_product_preserves_all_three_torque_equations() {
    // MLS §§10.3.3/10.3.5: the prismatic-joint torque balance is a vector
    // equation even when the other operand contains a time-varying scale.
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document(
            "torque_balance.mo",
            r#"
model TorqueBalance
  parameter Real e[3] = {0, 0, 1};
  Real s = 2 + time;
  Real f[3] = {3, 4, 5};
  Real torque[3];
equation
  zeros(3) = torque + cross(e*s, f);
end TorqueBalance;
"#,
        )
        .expect("torque balance parses");
    let compiled = session
        .compile_model("TorqueBalance")
        .expect("three torque equations balance all three unknown components");
    assert_eq!(compiled.balance_detail.equations_unknowns(), (7, 7));
    let result = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 0.25,
            ..SimOptions::default()
        },
    )
    .expect("the complete torque balance simulates");
    assert!(result.times.len() > 1);
    for (name, factor) in [
        ("s", 1.0),
        ("torque[1]", 4.0),
        ("torque[2]", -3.0),
        ("torque[3]", 0.0),
    ] {
        let column = result
            .names
            .iter()
            .position(|candidate| candidate == name)
            .expect("every physical component remains observable");
        assert_eq!(result.data[column].len(), result.times.len());
        for (actual, time) in result.data[column].iter().zip(&result.times) {
            let expected = factor * (2.0 + time);
            assert!(
                (actual - expected).abs() < 1.0e-10,
                "{name} = {actual}, expected {expected}"
            );
        }
    }
}

/// MLS §10.4: the array constructor function `array(A, B, ...)` is `{A, B, ...}`
/// and `array(e for i in r)` is `{e for i in r}`, in a function's protected
/// constant (as `IF97_Utilities` writes its viscosity coefficients) and in an
/// equation.
#[test]
fn the_array_function_constructs_the_array_of_its_arguments() {
    let compiled = rumoca::Compiler::new()
        .model("ArrayFunction.Top")
        .compile_str(
            r#"
package ArrayFunction
  function f
    input Real x;
    output Real y;
  protected
    constant Real[3] nn = array(1.0, 2.0, 3.0);
  algorithm
    y := x*nn[2] + nn[3];
  end f;
  model Top
    Real y = f(time);
    Real z[2] = array(time, 2*time);
    Real w[3] = array(i*time for i in 1:3);
  end Top;
end ArrayFunction;
"#,
            "ArrayFunction.mo",
        )
        .unwrap_or_else(|error| panic!("Top compiles: {error:?}"));
    let result = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..Default::default()
        },
    )
    .expect("Top simulates");
    let last = result.times.len() - 1;
    let time = result.times[last];
    let value = |name: &str| {
        let index = result.names.iter().position(|n| n == name);
        result.data[index.unwrap_or_else(|| panic!("{name} in {:?}", result.names))][last]
    };
    let expected = [
        ("y", 2.0 * time + 3.0),
        ("z[1]", time),
        ("z[2]", 2.0 * time),
        ("w[1]", time),
        ("w[3]", 3.0 * time),
    ];
    for (name, expected) in expected {
        let actual = value(name);
        assert!((actual - expected).abs() < 1e-9, "{name} = {actual}");
    }
}

#[test]
fn component_array_vector_members_keep_every_axis_and_value() {
    // MLS §§10.6.1/10.6.9: four three-axis members form a [4,3] equation,
    // not four scalar equations. A singleton parent still retains the field axis.
    for rotors in [1, 4] {
        let source = format!(
            r#"
model Source
  parameter Real bias;
  output Real moment[3] = {{bias + time, bias + 2*time, bias + 3*time}};
end Source;
model Sink
  input Real moment[3];
end Sink;
model Projection
  Source sources[{rotors}](bias={{10*i for i in 1:{rotors}}});
  Sink sinks[{rotors}];
equation
  sinks.moment = sources.moment;
end Projection;
"#
        );
        let compiler = rumoca::Compiler::new().model("Projection");
        let flat = compiler.compile_str_flat(&source, "Projection.mo").unwrap();
        let row = flat.equations.len() - 1;
        assert_eq!(flat.equations[row].scalar_count, rotors * 3);
        let family = flat
            .structured_equations
            .iter()
            .find(|family| family.first_equation_index == row)
            .unwrap();
        assert_eq!(family.domain.extents().unwrap(), vec![rotors, 3]);
        let compiled = compiler.compile_str(&source, "Projection.mo").unwrap();
        assert_eq!(
            compiled.balance_detail.equations_unknowns(),
            (rotors * 6, rotors * 6)
        );
        let result = simulate_dae(
            &compiled.dae,
            &SimOptions {
                t_end: 0.1,
                ..Default::default()
            },
        )
        .expect("every projected axis simulates");
        for rotor in 1..=rotors {
            for axis in 1..=3 {
                let name = format!("sinks[{rotor}].moment[{axis}]");
                let column = result
                    .names
                    .iter()
                    .position(|candidate| candidate == &name)
                    .unwrap();
                assert_linear_trace(
                    &name,
                    &result.data[column],
                    &result.times,
                    (10 * rotor) as f64,
                    axis as f64,
                );
            }
        }
    }
}

#[test]
fn component_array_matrix_members_keep_initial_equation_domains() {
    let source = r#"
model Source
  output Real value[2,3] = {{1,2,3},{4,5,6}};
end Source;
model Sink
  Real value[2,3](each fixed=false);
equation
  der(value) = zeros(2,3);
end Sink;
model Projection
  Source sources[4];
  Sink sinks[4];
initial equation
  sinks.value = sources.value;
end Projection;
"#;
    let compiler = rumoca::Compiler::new().model("Projection");
    let flat = compiler.compile_str_flat(source, "Projection.mo").unwrap();
    assert_eq!(flat.initial_equations[0].scalar_count, 24);
    assert_eq!(
        flat.initial_structured_equations[0]
            .domain
            .extents()
            .unwrap(),
        vec![4, 2, 3]
    );
    let compiled = compiler.compile_str(source, "Projection.mo").unwrap();
    let result = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 0.01,
            ..Default::default()
        },
    )
    .expect("all matrix elements initialize from their projected source");
    for rotor in 1..=4 {
        for row in 1..=2 {
            for col in 1..=3 {
                let name = format!("sinks[{rotor}].value[{row},{col}]");
                let column = result
                    .names
                    .iter()
                    .position(|candidate| candidate == &name)
                    .unwrap();
                assert_linear_trace(
                    &name,
                    &result.data[column],
                    &result.times,
                    ((row - 1) * 3 + col) as f64,
                    0.0,
                );
            }
        }
    }
}

fn assert_linear_trace(name: &str, values: &[f64], times: &[f64], offset: f64, slope: f64) {
    for (value, time) in values.iter().zip(times) {
        let expected = offset + slope * time;
        assert!(
            (value - expected).abs() < 1e-9,
            "{name}: {value} != {expected}"
        );
    }
}
