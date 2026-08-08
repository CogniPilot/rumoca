//! Tensor-native event ownership for relations written inside equation loops.
//!
//! This is the minimal counterexample that exposed a false compile: the
//! structured residual retained its binder, but event lowering discarded the
//! relation because it was not a closed scalar expression. The resulting
//! Solve model had no roots and an adaptive integrator could cross either
//! branch discontinuity without locating the Modelica event.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, lower_dae_for_simulation};

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
