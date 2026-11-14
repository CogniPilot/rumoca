//! A continuous write that meets an indicator outside the domain the
//! component froze is the input event, with nothing changed before it.

use super::*;
use me::kernel::UnguardedContinuousWrite;
use me::session::input_write_audit::{
    InputWriteAudit, force_input_event, input_write_counts, unguarded_input_write_count,
};

/// The harmonic oscillator with the indicator `x` and one input `u` that no
/// event owner reads.
fn oscillator_with_indicator_and_input() -> solve::SolveModel {
    let mut model = super::super::harmonic_oscillator();
    model.problem.solve_layout.compiled_parameter_len = 1;
    model.problem.solve_layout.input_scalar_names = vec!["u".to_string()].into();
    model.parameters = vec![0.0].into();
    model.problem.events.root_conditions = block(
        vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        "fmi_me_oscillator_indicator.mo",
    );
    model.problem.events.root_zero_domains = vec![solve::RootZeroDomain::Positive];
    refresh_owned(model)
}

/// The session after a changed write of `u` under inverted frozen domains,
/// its changed batches as `[continuous, input event]`, and how many it wrote
/// without a restoration point.
fn write_under_inverted_domains(forced: bool) -> (InputWriteAudit, [usize; 2], usize) {
    let model = oscillator_with_indicator_and_input();
    let mut retained = retained_branch_component(&model);
    let mut session = plugin_session(&mut retained, FaultyPlugin::new(PluginFault::Healthy));
    assert_eq!(session.input_event_reach().unwrap(), [false]);
    session.verification_invert_frozen_indicator_domains();
    force_input_event(forced);
    let before = (input_write_counts(), unguarded_input_write_count());
    session.set_input("u", 2.0).unwrap();
    let after = (input_write_counts(), unguarded_input_write_count());
    force_input_event(false);
    (
        session.input_write_audit().unwrap(),
        [after.0[0] - before.0[0], after.0[1] - before.0[1]],
        after.1 - before.1,
    )
}

#[test]
fn an_indicator_outside_its_frozen_domain_is_found_without_changing_the_component() {
    let model = oscillator_with_indicator_and_input();
    let mut retained = retained_branch_component(&model);
    let session = plugin_session(&mut retained, FaultyPlugin::new(PluginFault::Healthy));
    assert_eq!(
        session.verification_unguarded_continuous_write("u", 2.0),
        UnguardedContinuousWrite::Written,
        "the fixture admits the rollback-free write"
    );
    let model = oscillator_with_indicator_and_input();
    let mut retained = retained_branch_component(&model);
    let session = plugin_session(&mut retained, FaultyPlugin::new(PluginFault::Healthy));
    session.verification_invert_frozen_indicator_domains();
    let before = session.verification_checkpoint();
    assert_eq!(
        session.verification_unguarded_continuous_write("u", 2.0),
        UnguardedContinuousWrite::NeedsInputEvent
    );
    assert!(session.verification_matches_checkpoint(&before));
}

#[test]
fn a_write_under_a_stale_frozen_domain_leaves_the_input_event_bits() {
    let (plain, plain_writes, plain_unguarded) = write_under_inverted_domains(false);
    let (forced, forced_writes, forced_unguarded) = write_under_inverted_domains(true);
    assert_eq!(plain_writes, [0, 1]);
    assert_eq!(forced_writes, [0, 1]);
    assert_eq!((plain_unguarded, forced_unguarded), (0, 0));
    assert_eq!(plain, forced);
}
