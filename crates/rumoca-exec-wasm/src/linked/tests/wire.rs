//! The linked artifact travels from the build to the guest beside the saved
//! product. The receiver issues its own product from its checked sources and
//! admits the artifact only if the two are the same, position by position.
use super::fixtures;
use super::product::{self, Sources};
use crate::linked::{LinkedArtifact, LinkedProduct, LinkedProductAssembler};
use serde_json::{Value, json};

const SAVED: &[u8] = b"saved product bytes";

fn decode(value: Value) -> LinkedArtifact {
    serde_json::from_value(value).unwrap()
}

fn position(entries: &Value, kind: &str, index: u64) -> usize {
    entries
        .as_array()
        .unwrap()
        .iter()
        .position(|entry| entry["id"]["kind"] == kind && entry["id"]["index"] == index)
        .unwrap()
}

#[test]
fn an_artifact_certifies_against_the_product_its_sources_issue_again() {
    let sources = Sources::new();
    let issued = product::build(&sources).linked;
    let wire = serde_json::to_value(issued.artifact(SAVED)).unwrap();
    let reissued: LinkedProduct = product::build(&Sources::new()).linked;
    let artifact = decode(wire.clone());
    artifact.admit(SAVED).unwrap().finish(&reissued).unwrap();
    assert_eq!(artifact.dispatch_module(), reissued.plan().dispatch());
    assert!(
        decode(wire).admit(b"another saved product").is_err(),
        "the artifact is bound to the saved product it was captured with"
    );
}

/// Positions are compared one by one. Private 0 and 2 hold the same source,
/// so their keys and bindings are equal; only their position tells them
/// apart, and a reordered artifact is refused all the same.
#[test]
fn a_reordered_or_altered_artifact_is_refused() {
    let sources = Sources::new();
    let issued = product::build(&sources).linked;
    let wire = serde_json::to_value(issued.artifact(SAVED)).unwrap();
    let entries = &wire["entries"];
    let (first, second) = (
        position(entries, "Private", 0),
        position(entries, "Private", 1),
    );
    let again = position(entries, "Private", 2);
    assert_eq!(entries[first]["key"], entries[again]["key"]);
    assert_ne!(entries[first]["key"], entries[second]["key"]);
    let mut swapped = wire.clone();
    swapped["entries"][first] = entries[second].clone();
    swapped["entries"][second] = entries[first].clone();
    let private = wire["modules"]
        .as_array()
        .unwrap()
        .iter()
        .position(|module| module["exports"].as_array().unwrap().len() == 3)
        .unwrap();
    let mut duplicate = wire.clone();
    let exports = &mut duplicate["modules"][private]["exports"];
    let (zero, two) = (exports[0].clone(), exports[2].clone());
    exports[0] = two;
    exports[2] = zero;
    let mut rekeyed = wire.clone();
    let word = entries[again]["key"][0].as_u64().unwrap();
    rekeyed["entries"][again]["key"][0] = json!(word ^ 1);
    let mut dispatch = wire.clone();
    let byte = wire["dispatch"][8].as_u64().unwrap();
    dispatch["dispatch"][8] = json!(byte ^ 1);
    let reissued = product::build(&Sources::new()).linked;
    for (name, value) in [
        ("two positions of different sources swapped", swapped),
        ("two equal-key positions swapped in their module", duplicate),
        ("one key word changed", rekeyed),
        ("one dispatch module byte changed", dispatch),
    ] {
        let artifact = decode(value);
        let refused = artifact
            .admit(SAVED)
            .and_then(|admission| admission.finish(&reissued));
        assert!(refused.is_err(), "{name}");
    }
}

/// The product's issues one request at a time, in the order the owner made
/// them, each through the receiver's own assembler.
type Step<'a> = Box<dyn FnOnce(&mut LinkedProductAssembler) + 'a>;

fn steps(sources: &Sources) -> Vec<Step<'_>> {
    let mut steps: Vec<Step<'_>> = Vec::new();
    for component in &sources.derivatives {
        steps.push(Box::new(move |assembler: &mut LinkedProductAssembler| {
            product::with_operation(component, |operation| {
                assembler.derivative(operation).unwrap();
            });
        }));
    }
    steps.push(Box::new(move |assembler: &mut LinkedProductAssembler| {
        let model = &sources.exact;
        assembler
            .exact(
                &model.problem.continuous.implicit_rhs,
                &model.problem.continuous.refresh_owners,
                fixtures::exact_schedule(model),
                &model.problem.layout,
                &model.pure_calls,
            )
            .unwrap();
    }));
    steps.push(Box::new(move |assembler: &mut LinkedProductAssembler| {
        let (layout, calls) = (fixtures::issued_layout(), fixtures::empty_calls());
        assembler.issued(&sources.rows, &layout, &calls).unwrap();
    }));
    steps.push(Box::new(move |assembler: &mut LinkedProductAssembler| {
        let (admission, admitted) = product::private_admission(sources);
        let programs = admitted.iter().collect::<Vec<_>>();
        assembler.private(&admission, &programs).unwrap();
    }));
    steps.push(Box::new(move |assembler: &mut LinkedProductAssembler| {
        assembler.event(&sources.event).unwrap();
    }));
    steps
}

/// The index of the first step after which `certify_prefix` refuses, if any;
/// every earlier step passes.
fn first_refusal(artifact: &LinkedArtifact, sources: &Sources) -> Option<usize> {
    let admission = artifact.admit(SAVED).unwrap();
    let mut assembler = LinkedProductAssembler::new();
    for (index, step) in steps(sources).into_iter().enumerate() {
        step(&mut assembler);
        if admission.certify_prefix(&assembler).is_err() {
            return Some(index);
        }
    }
    None
}

/// The step whose issue appends module `k`: the first step after which the
/// issued plan holds the entry that module exports, or the derivative
/// whose helper it is.
fn issuing_step(wire: &Value, k: usize, sources: &Sources) -> usize {
    let export = wire["modules"][k]["exports"][0]
        .as_str()
        .unwrap()
        .to_owned();
    let mut assembler = LinkedProductAssembler::new();
    for (index, step) in steps(sources).into_iter().enumerate() {
        step(&mut assembler);
        let holds = assembler.plan().entries().any(|(id, _)| {
            let symbol = id.symbol();
            export == symbol || export.starts_with(&format!("{symbol}_"))
        });
        if holds {
            return index;
        }
    }
    panic!("module {k} is issued by no step")
}

#[test]
fn every_prefix_of_the_owner_order_certifies_and_the_whole_product_does_too() {
    let sources = Sources::new();
    let issued = product::build(&sources).linked;
    let artifact = issued.artifact(SAVED);
    assert!(artifact.is_bound_to(SAVED));
    assert!(!artifact.is_bound_to(b"another saved product"));
    assert_eq!(first_refusal(&artifact, &sources), None);
    let mut assembler = LinkedProductAssembler::new();
    for step in steps(&sources) {
        step(&mut assembler);
    }
    let whole = assembler.finish(&[]).unwrap();
    artifact.admit(SAVED).unwrap().finish(&whole).unwrap();
}

/// A one-byte change in module k passes every call before the one that
/// issues k, and is refused at that call, before anything it issued is bound.
/// Every module is changed in turn, so this covers each entry's whole
/// closure: a derivative's helper modules (refresh, values, kinematic values)
/// as well as its control module, and the private batch's shared module.
#[test]
fn a_changed_module_is_refused_at_the_call_that_issues_it() {
    let sources = Sources::new();
    let issued = product::build(&sources).linked;
    let wire = serde_json::to_value(issued.artifact(SAVED)).unwrap();
    let modules = wire["modules"].as_array().unwrap();
    let exported = |suffix: &str| {
        modules.iter().any(|module| {
            module["exports"]
                .as_array()
                .unwrap()
                .iter()
                .any(|export| export.as_str().unwrap().ends_with(suffix))
        })
    };
    for helper in [
        "_static_refresh",
        "_dynamic_refresh",
        "_values",
        "_kinematic_values",
    ] {
        assert!(
            exported(helper),
            "the product holds a {helper} helper module"
        );
    }
    assert!(
        modules
            .iter()
            .any(|module| module["exports"].as_array().unwrap().len() == 3),
        "the product holds the shared private module"
    );
    let count = modules.len();
    for k in 0..count {
        let mut changed = wire.clone();
        let bytes = &mut changed["modules"][k]["bytes"];
        let middle = bytes.as_array().unwrap().len() / 2;
        let byte = bytes[middle].as_u64().unwrap();
        bytes[middle] = json!(byte ^ 1);
        assert_eq!(
            first_refusal(&decode(changed), &sources),
            Some(issuing_step(&wire, k, &sources)),
            "module {k}"
        );
    }
}

#[test]
fn equal_key_positions_swapped_in_their_module_are_refused_at_their_issue() {
    let sources = Sources::new();
    let issued = product::build(&sources).linked;
    let wire = serde_json::to_value(issued.artifact(SAVED)).unwrap();
    let private = wire["modules"]
        .as_array()
        .unwrap()
        .iter()
        .position(|module| module["exports"].as_array().unwrap().len() == 3)
        .unwrap();
    let mut swapped = wire.clone();
    let exports = &mut swapped["modules"][private]["exports"];
    let (zero, two) = (exports[0].clone(), exports[2].clone());
    exports[0] = two;
    exports[2] = zero;
    assert_eq!(
        first_refusal(&decode(swapped), &sources),
        Some(issuing_step(&wire, private, &sources))
    );
}

#[test]
fn a_changed_dispatch_byte_or_another_saved_product_is_refused_before_any_issue() {
    let sources = Sources::new();
    let issued = product::build(&sources).linked;
    let wire = serde_json::to_value(issued.artifact(SAVED)).unwrap();
    let mut changed = wire.clone();
    let byte = wire["dispatch"][8].as_u64().unwrap();
    changed["dispatch"][8] = json!(byte ^ 1);
    assert!(decode(changed).admit(SAVED).is_err());
    assert!(decode(wire.clone()).admit(b"another").is_err());
    let mut unordered = wire.clone();
    let entries = unordered["entries"].as_array_mut().unwrap();
    entries.swap(0, 1);
    assert!(decode(unordered).admit(SAVED).is_err());
    assert!(decode(wire).admit(SAVED).is_ok());
}

/// Issuing past the artifact's records is refused, and so is a whole
/// product with a record no request claimed.
#[test]
fn an_extra_issue_or_an_unclaimed_record_is_refused() {
    let sources = Sources::new();
    let artifact = product::build(&sources).linked.artifact(SAVED);
    let admission = artifact.admit(SAVED).unwrap();
    let mut assembler = LinkedProductAssembler::new();
    let all = steps(&sources);
    let count = all.len();
    for (index, step) in all.into_iter().enumerate() {
        if index + 1 < count {
            step(&mut assembler);
        }
    }
    admission.certify_prefix(&assembler).unwrap();
    let partial = assembler.finish(&[]).unwrap();
    assert!(
        artifact.admit(SAVED).unwrap().finish(&partial).is_err(),
        "event unclaimed"
    );
    let mut extra = LinkedProductAssembler::new();
    for step in steps(&sources) {
        step(&mut extra);
    }
    let (admission, admitted) = product::private_admission(&sources);
    let programs = admitted.iter().collect::<Vec<_>>();
    extra.private(&admission, &programs).unwrap();
    assert!(
        admission.certify_prefix(&extra).is_err(),
        "private 3.. issued"
    );
}
