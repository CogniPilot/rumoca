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
