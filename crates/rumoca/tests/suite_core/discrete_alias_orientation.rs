//! Discrete alias equations oriented by their producers (MLS 3.7 Appendix B).
//!
//! `Modelica.Media.Water` defines `phase` with an `if` equation and then states
//! `phase = state.phase`. Read left to right that alias would define `phase`
//! a second time and leave the record field without a definition; Appendix B
//! admits flipping the sides, which gives every coordinate one owner.

use rumoca::Compiler;

const SOURCE: &str = r#"
record State
  Integer phase;
  Real h;
end State;

model Medium
  Real x(start = 0, fixed = true);
  Integer phase;
  State state;
equation
  der(x) = 1;
  if x > 0.5 then
    phase = 2;
  else
    phase = 1;
  end if;
  state.h = 2*x;
  phase = state.phase;
end Medium;
"#;

#[test]
fn an_alias_defines_the_side_without_another_owner() {
    let compiled = Compiler::new()
        .model("Medium")
        .compile_str(SOURCE, "DiscreteAliasOrientation.mo")
        .unwrap_or_else(|error| panic!("Medium compiles: {error:?}"));
    let result = rumoca_sim::simulate_dae_with_diagnostics(
        &compiled.dae,
        &rumoca_sim::SimOptions {
            t_end: 1.0,
            ..Default::default()
        },
    )
    .expect("Medium simulates");
    let column = |name: &str| {
        let index = result.names.iter().position(|n| n == name);
        &result.data[index.unwrap_or_else(|| panic!("{name} in {:?}", result.names))]
    };
    for (index, _) in result.times.iter().enumerate() {
        let x = column("x")[index];
        let expected = if x > 0.5 { 2.0 } else { 1.0 };
        if (x - 0.5).abs() < 1e-6 {
            continue;
        }
        assert_eq!(column("phase")[index], expected, "phase at x = {x}");
        assert_eq!(
            column("state.phase")[index],
            expected,
            "state.phase at x = {x}"
        );
    }
}
