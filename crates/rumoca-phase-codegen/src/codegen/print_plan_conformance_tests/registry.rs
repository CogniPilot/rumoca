//! The decoded-region registry, read from the same file the template check
//! reads. Registration lets the template check leave a region's literal text
//! to these decoders; here each registered region must actually be rendered
//! by production `definition()` over the conformance programs, every
//! declared dispatch variant must be reached, and that output must decode.

/// One registered region: its macro and its dispatch tags with variants.
#[derive(Debug, Default)]
pub(super) struct Region {
    pub(super) name: String,
    pub(super) tags: Vec<(String, Vec<String>)>,
    pub(super) refused: Vec<(String, Vec<String>)>,
}

/// The registry's regions. Every line must be a comment, blank, a table
/// header or a known `key = "value"` / `variants = [...]` entry; anything
/// else fails, so a row this reader does not understand cannot pass unseen.
pub(super) fn regions(text: &str) -> Vec<Region> {
    let mut regions: Vec<Region> = Vec::new();
    let mut section = "";
    let mut path = String::new();
    for raw in text.lines() {
        let line = raw.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        if line.starts_with("[[") {
            section = match line {
                "[[region]]" => {
                    regions.push(Region::default());
                    "region"
                }
                "[[region.argument]]" => "argument",
                "[[region.tag]]" => "tag",
                other => panic!("unknown registry table {other}"),
            };
            continue;
        }
        let (key, value) = line.split_once(" = ").expect("registry entry");
        let region = regions.last_mut().expect("entry inside a region");
        match (section, key) {
            ("region", "macro") => region.name = unquote(value),
            ("region", "file" | "consumer" | "decoder")
            | ("argument", "name" | "schema")
            | ("tag", "schema") => {}
            ("tag", "refused") => {
                let list = value.trim_start_matches('[').trim_end_matches(']');
                let refused = list.split(", ").map(unquote).collect();
                region.refused.push((path.clone(), refused));
            }
            ("tag", "path") => path = unquote(value),
            ("tag", "variants") => {
                let list = value.trim_start_matches('[').trim_end_matches(']');
                let variants = list.split(", ").map(unquote).collect();
                region.tags.push((path.clone(), variants));
            }
            other => panic!("unknown registry entry {other:?}"),
        }
    }
    regions
}

fn unquote(value: &str) -> String {
    value
        .strip_prefix('"')
        .and_then(|v| v.strip_suffix('"'))
        .expect("quoted registry value")
        .to_owned()
}

/// The template with a marker after the header of each registered macro
/// and after the opening tag of each declared dispatch branch.
pub(super) fn instrumented(template: &str, regions: &[Region]) -> String {
    let mut text = template.to_owned();
    for region in regions {
        let header = format!("{{%- macro {}(", region.name);
        let start = text.find(&header).expect("registered macro exists");
        let open = start + text[start..].find("-%}").expect("macro header") + 3;
        text.insert_str(open, &marker(&format!("R:{}", region.name), true));
        let end = open + text[open..].find("endmacro").expect("macro end");
        let mut body = text[open..end].to_owned();
        for (path, variants) in &region.tags {
            for variant in variants {
                let test = format!("{path} == \"{variant}\"");
                let at = body.find(&test).expect("declared variant branch");
                let close = at + body[at..].find("%}").expect("branch tag") + 2;
                let trims = body[..close].ends_with("-%}");
                let key = format!("V:{}:{path}:{variant}", region.name);
                body.insert_str(close, &marker(&key, trims));
            }
        }
        text.replace_range(open..end, &body);
    }
    text
}

/// A marker printed as an expression that keeps the whitespace control of
/// the tag it follows: after `-%}` it trims what that tag would have trimmed.
fn marker(key: &str, trims: bool) -> String {
    if trims {
        format!("{{{{- \"@@{key}@@\" -}}}}")
    } else {
        format!("{{{{ \"@@{key}@@\" }}}}")
    }
}

/// A marker occurrence admitted at its exact byte position in a trusted
/// instrumentation rendering. Equal keys at different positions are distinct
/// events (a rendered helper argument can legitimately be used several times).
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct Occurrence {
    key: String,
    offset: usize,
    ordinal: usize,
}

/// Only consumed, position-checked occurrences may provide coverage evidence.
#[derive(Debug)]
pub(super) struct Consumed {
    output: String,
    occurrences: Vec<Occurrence>,
}

impl Consumed {
    pub(super) fn output(&self) -> &str {
        &self.output
    }
    pub(super) fn occurrences(&self) -> &[Occurrence] {
        &self.occurrences
    }
}

/// Every marker key `instrumented` issues for `regions`.
fn marker_keys(regions: &[Region]) -> std::collections::BTreeSet<String> {
    let mut keys = std::collections::BTreeSet::new();
    for region in regions {
        keys.insert(format!("R:{}", region.name));
        for (path, variants) in &region.tags {
            for variant in variants {
                keys.insert(format!("V:{}:{path}:{variant}", region.name));
            }
        }
    }
    keys
}

/// The caller builds `reference` afresh from the production printer's source,
/// with this module's instrumentation and the same program/name. Neither a
/// template nor a registration can supply its own reference or approval flag.
/// Consume every expected marker once, at its exact expected output position.
/// Unknown, extra, moved, repeated, missing or malformed events refuse, even
/// when the forged key is otherwise valid. No raw-output substring is coverage.
pub(super) fn consume_trusted(
    actual: &str,
    reference: &str,
    regions: &[Region],
) -> Result<Consumed, String> {
    let keys = marker_keys(regions);
    let mut output = String::with_capacity(reference.len());
    let mut occurrences = Vec::new();
    let mut cursor = 0;
    while let Some(relative) = reference[cursor..].find("@@") {
        let start = cursor + relative;
        let payload = start + 2;
        let end = reference[payload..]
            .find("@@")
            .map(|end| payload + end)
            .ok_or("unterminated trusted marker")?;
        let key = &reference[payload..end];
        if !keys.contains(key) {
            return Err(format!("unissued trusted marker {key}"));
        }
        let after = end + 2;
        if actual.get(cursor..after) != Some(&reference[cursor..after]) {
            return Err(format!(
                "marker occurrence {} differs at byte {start}",
                occurrences.len()
            ));
        }
        output.push_str(&reference[cursor..start]);
        occurrences.push(Occurrence {
            key: key.into(),
            offset: start,
            ordinal: occurrences.len(),
        });
        cursor = after;
    }
    if actual.get(cursor..) != Some(&reference[cursor..]) {
        return Err(format!("unconsumed or changed output at byte {cursor}"));
    }
    output.push_str(&reference[cursor..]);
    Ok(Consumed {
        output,
        occurrences,
    })
}

/// Registered regions no checked event reached, and variants no checked event
/// selected. These events can only be issued by `consume_trusted`, never by
/// searching arbitrary strings or treating a valid marker key as authority.
pub(super) fn unreached(regions: &[Region], events: &[Occurrence]) -> (Vec<String>, Vec<String>) {
    let observed: std::collections::BTreeSet<_> =
        events.iter().map(|event| event.key.as_str()).collect();
    let unrun = regions
        .iter()
        .filter(|region| !observed.contains(format!("R:{}", region.name).as_str()))
        .map(|region| region.name.clone())
        .collect();
    let mut unselected = Vec::new();
    for region in regions {
        for (path, variants) in &region.tags {
            for variant in variants {
                let key = format!("{}:{path}:{variant}", region.name);
                if !observed.contains(format!("V:{key}").as_str()) {
                    unselected.push(key);
                }
            }
        }
    }
    (unrun, unselected)
}

#[cfg(test)]
mod occurrence_tests {
    use super::*;
    fn inventory() -> Vec<Region> {
        vec![Region {
            name: "item".into(),
            tags: vec![("item.kind".into(), vec!["copy".into(), "constant".into()])],
            refused: vec![],
        }]
    }
    #[test]
    fn exact_positions_allow_real_repeated_helper_output() {
        let regions = inventory();
        let reference = "α@@R:item@@x@@V:item:item.kind:copy@@y@@R:item@@z";
        let consumed = consume_trusted(reference, reference, &regions).unwrap();
        assert_eq!(consumed.output(), "αxyz");
        assert_eq!(consumed.occurrences().len(), 3);
        assert_eq!(consumed.occurrences()[0].offset, 2);
        assert_eq!(consumed.occurrences()[2].ordinal, 2);
        let (unrun, unselected) = unreached(&regions, consumed.occurrences());
        assert!(unrun.is_empty());
        assert_eq!(unselected, ["item:item.kind:constant"]);
    }
    #[test]
    fn known_keys_do_not_authorize_extra_moved_copied_or_missing_occurrences() {
        let regions = inventory();
        let reference = "a@@R:item@@b@@V:item:item.kind:copy@@c";
        for actual in [
            "a@@R:item@@@@R:item@@b@@V:item:item.kind:copy@@c",
            "a@@R:item@@@@V:item:item.kind:constant@@b@@V:item:item.kind:copy@@c",
            "a@@R:item@@@@V:unknown@@b@@V:item:item.kind:copy@@c",
            "@@R:item@@ab@@V:item:item.kind:copy@@c",
            "ab@@V:item:item.kind:copy@@c",
            "a@@R:item@@b@@V:item:item.kind:copy@@c@@R:item@@",
            "a@@R:item@@b@@V:item:item.kind:copy@@changed",
            "a@@R:item@b@@V:item:item.kind:copy@@c",
        ] {
            assert!(
                consume_trusted(actual, reference, &regions).is_err(),
                "accepted {actual}"
            );
        }
    }
    #[test]
    fn unknown_or_malformed_reference_is_never_authority() {
        for reference in ["@@R:unissued@@", "@@R:item", "@@R:item@@@@V:unissued@@"] {
            assert!(consume_trusted(reference, reference, &inventory()).is_err());
        }
        let empty = consume_trusted("", "", &inventory()).unwrap();
        assert!(!unreached(&inventory(), empty.occurrences()).0.is_empty());
    }
}
