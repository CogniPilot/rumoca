//! The linked artifact travels from the build to the guest beside the saved
//! product. The receiver issues its own product from its checked sources and
//! admits the artifact only if the two are the same, position by position.
use super::product::{self, Sources};
use crate::linked::{LinkedArtifact, LinkedProduct};
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
    decode(wire.clone()).certify(SAVED, &reissued).unwrap();
    assert!(
        decode(wire)
            .certify(b"another saved product", &reissued)
            .is_err(),
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
    dispatch["dispatch"] = json!("0".repeat(64));
    let reissued = product::build(&Sources::new()).linked;
    for (name, value) in [
        ("two positions of different sources swapped", swapped),
        ("two equal-key positions swapped in their module", duplicate),
        ("one key word changed", rekeyed),
        ("another dispatch module", dispatch),
    ] {
        assert!(decode(value).certify(SAVED, &reissued).is_err(), "{name}");
    }
}
