//! Tensor-native event ownership for relations written inside equation loops.
//!
//! This is the minimal counterexample that exposed a false compile: the
//! structured residual retained its binder, but event lowering discarded the
//! relation because it was not a closed scalar expression. The resulting
//! Solve model had no roots and an adaptive integrator could cross either
//! branch discontinuity without locating the Modelica event.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, lower_dae_for_simulation, simulate_dae};

const STRUCTURED_CONTACT: &str = r#"
model StructuredContact
  Real height[2](start = {0.25, 0.75}, each fixed = true);
  Real force[2];
equation
  der(height) = -ones(2);
  for contact in 1:2 loop
    force[contact] = if height[contact] > 0.0 then 0.0 else -height[contact];
  end for;
end StructuredContact;
"#;

#[test]
fn structured_relation_owns_one_compact_root_family_and_two_solve_views() {
    let compiled = Compiler::new()
        .model("StructuredContact")
        .compile_str(STRUCTURED_CONTACT, "StructuredContact.mo")
        .expect("structured state relations must compile with an event owner");

    compiled.dae.inspect(|view| {
        assert_eq!(view.structured_root_count(), 1);
        assert_eq!(view.root_count(), 0);
        let root = view
            .structured_root(view.structured_root_id(0).expect("root identity"))
            .expect("root family");
        let domain = view.domain(root.domain()).expect("root domain");
        assert_eq!(
            domain.structured().scalar_count().expect("finite domain"),
            2
        );
        assert_eq!(
            view.expression(root.expression())
                .expect("root expression")
                .binder_domain(),
            Some(root.domain())
        );
    });

    let solve = lower_dae_for_simulation(&compiled.dae, &SimOptions::default())
        .expect("structured root family must have a Solve view");
    solve.problem.validate().expect("Solve contract");
    assert_eq!(solve.problem.events.root_conditions.len(), 2);
    assert_eq!(solve.problem.events.root_zero_domains.len(), 2);
    assert_eq!(solve.problem.events.root_relation_memory_targets.len(), 2);

    let json = serde_json::to_string(&compiled.dae).expect("serialize compact DAE");
    let decoded: rumoca_ir_dae::Dae =
        serde_json::from_str(&json).expect("replay compact root owner");
    decoded.inspect(|view| assert_eq!(view.structured_root_count(), 1));
}

/// MLS §8.5: a relation over `time` and parameters in a `for` family is a
/// scheduled time event per point. Each point switches at its own instant,
/// owned by the family's scalar views, without a root of the family's own.
const STAGED_SWITCHES: &str = r#"
model StagedSwitches
  parameter Real t0[3] = {0.25, 0.5, 0.75};
  Real x(start = 0, fixed = true);
  Real y[3];
equation
  der(x) = 1;
  for i in 1:3 loop
    y[i] = if time < t0[i] then 0 else 1;
  end for;
end StagedSwitches;
"#;

#[test]
fn every_point_of_a_time_event_family_switches_at_its_own_instant() {
    let compiled = Compiler::new()
        .model("StagedSwitches")
        .compile_str(STAGED_SWITCHES, "StagedSwitches.mo")
        .unwrap_or_else(|error| panic!("StagedSwitches compiles: {error:?}"));
    let result = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("StagedSwitches simulates: {error:?}"));
    let value_at = |name: &str, time: f64| {
        let column = result
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap_or_else(|| panic!("StagedSwitches exposes {name}"));
        let row = result
            .times
            .iter()
            .rposition(|sample| *sample <= time)
            .expect("a sample at or before the requested time");
        result.data[column][row]
    };
    let mismatches: Vec<(&str, f64, f64, f64)> = [0.1, 0.4, 0.6, 0.9]
        .into_iter()
        .flat_map(|time| {
            [("y[1]", 0.25), ("y[2]", 0.5), ("y[3]", 0.75)]
                .into_iter()
                .map(move |(name, switch)| (name, time, f64::from(time >= switch)))
        })
        .map(|(name, time, expected)| (name, time, expected, value_at(name, time)))
        .filter(|(_, _, expected, actual)| expected != actual)
        .collect();
    assert!(
        mismatches.is_empty(),
        "a point missed its scheduled instant (name, time, expected, actual): {mismatches:?}"
    );
}
