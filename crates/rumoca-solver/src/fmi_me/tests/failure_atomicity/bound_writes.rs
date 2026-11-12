//! Bound input writes are the named transaction, and a refused one changes
//! nothing.

use super::*;
use me::session::{MeBindingRefusal, MeSessionError};

fn healthy_session(
    retained: &mut me::session::MeRetainedComponent,
) -> (
    me::session::MeSimulationSession<'_, 'static>,
    Rc<Cell<usize>>,
) {
    let plugin = FaultyPlugin::new(PluginFault::Healthy);
    let initializations = Rc::clone(&plugin.initializations);
    (plugin_session(retained, plugin), initializations)
}

#[test]
fn a_refused_bound_write_leaves_the_checkpoint_bit_equal() {
    let model = branch_component_with_input();
    let mut retained = retained_branch_component(&model);
    let mut other = retained_branch_component(&model);
    let (mut session, initializations) = healthy_session(&mut retained);
    let (mut other_session, _) = healthy_session(&mut other);
    let inputs = session.bind_inputs(&["u"]).unwrap();
    let outputs = session.bind_outputs(&["u"]).unwrap();
    let foreign = other_session.bind_inputs(&["u"]).unwrap();
    session.write_bound(inputs, &[2.0]).unwrap();
    assert_eq!(initializations.get(), 2);
    let refusal = |binding, refusal| MeSessionError::Binding { binding, refusal };
    for (binding, values, expected) in [
        (inputs, &[f64::NAN][..], None),
        (inputs, &[f64::INFINITY][..], None),
        (inputs, &[f64::NEG_INFINITY][..], None),
        (
            inputs,
            &[][..],
            Some(refusal(
                inputs,
                MeBindingRefusal::Arity { bound: 1, given: 0 },
            )),
        ),
        (
            inputs,
            &[3.0, 4.0][..],
            Some(refusal(
                inputs,
                MeBindingRefusal::Arity { bound: 1, given: 2 },
            )),
        ),
        (
            outputs,
            &[3.0][..],
            Some(refusal(outputs, MeBindingRefusal::NotInputs)),
        ),
        (
            foreign,
            &[3.0][..],
            Some(refusal(foreign, MeBindingRefusal::Foreign)),
        ),
    ] {
        let before = session.verification_checkpoint();
        let error = session
            .write_bound(binding, values)
            .expect_err("the write is refused");
        match expected {
            Some(expected) => assert_eq!(error.to_string(), expected.to_string()),
            None => assert!(
                matches!(error, MeSessionError::Component(_)),
                "the component refuses {values:?}: {error}"
            ),
        }
        assert!(
            session.verification_matches_checkpoint(&before),
            "{values:?} through binding {binding} moved the session"
        );
        assert_eq!(initializations.get(), 2);
        assert_eq!(session.visible_values().unwrap()["u"], 2.0);
    }
    session.reset(0.0).unwrap();
    let before = session.verification_checkpoint();
    let error = session.write_bound(inputs, &[3.0]).unwrap_err();
    assert_eq!(
        error.to_string(),
        refusal(inputs, MeBindingRefusal::Stale).to_string()
    );
    assert!(session.verification_matches_checkpoint(&before));
    // A refused read leaves the caller's buffer as it was.
    let mut read = vec![7.0, -0.0];
    assert!(session.read_bound(outputs, &mut read).is_err());
    assert_eq!(
        read.iter().map(|value| value.to_bits()).collect::<Vec<_>>(),
        [7.0_f64.to_bits(), (-0.0_f64).to_bits()]
    );
    assert_eq!(
        session.verification_component_point(),
        session.verification_session_point()
    );
}

/// Repeated names, an unchanged frame and a signed zero behave as named.
#[test]
fn a_bound_write_matches_the_named_write_of_the_same_names() {
    let model = branch_component_with_input();
    let mut named_component = retained_branch_component(&model);
    let mut bound_component = retained_branch_component(&model);
    let (mut named, named_starts) = healthy_session(&mut named_component);
    let (mut bound, bound_starts) = healthy_session(&mut bound_component);
    let inputs = bound.bind_inputs(&["u", "u"]).unwrap();
    let outputs = bound.bind_outputs(&["u", "u"]).unwrap();
    for (first, last) in [(2.0, 3.0), (3.0, 3.0), (0.5, -0.0)] {
        named.set_inputs(&[("u", first), ("u", last)]).unwrap();
        bound.write_bound(inputs, &[first, last]).unwrap();
        assert_eq!(named_starts.get(), bound_starts.get());
        assert_eq!(
            named.verification_component_point(),
            bound.verification_component_point()
        );
        let mut read = Vec::new();
        bound.read_bound(outputs, &mut read).unwrap();
        let expected: Vec<u64> = named
            .selected_values(&["u", "u"])
            .unwrap()
            .into_iter()
            .map(|value| value.unwrap().to_bits())
            .collect();
        assert_eq!(
            read.iter().map(|value| value.to_bits()).collect::<Vec<_>>(),
            expected
        );
    }
    assert_eq!(
        bound_starts.get(),
        3,
        "the unchanged frame restarts nothing"
    );
}
