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
