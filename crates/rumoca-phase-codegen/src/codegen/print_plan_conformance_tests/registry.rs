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

/// Registered regions no rendering reached, and declared variants no
/// rendering selected, as `region` and `region:path:variant`.
pub(super) fn unreached(regions: &[Region], rendered: &str) -> (Vec<String>, Vec<String>) {
    let unrun = regions
        .iter()
        .filter(|r| !rendered.contains(&format!("@@R:{}@@", r.name)))
        .map(|r| r.name.clone())
        .collect();
    let mut unselected = Vec::new();
    for region in regions {
        for (path, variants) in &region.tags {
            for variant in variants {
                let key = format!("{}:{path}:{variant}", region.name);
                if !rendered.contains(&format!("@@V:{key}@@")) {
                    unselected.push(key);
                }
            }
        }
    }
    (unrun, unselected)
}

/// `text` without the markers `instrumented` inserts.
pub(super) fn stripped(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    let mut rest = text;
    while let Some(at) = rest.find("@@") {
        out.push_str(&rest[..at]);
        let tail = &rest[at + 2..];
        let marker = (tail.starts_with("R:") || tail.starts_with("V:"))
            .then(|| tail.find("@@"))
            .flatten();
        match marker {
            Some(end) => rest = &tail[end + 2..],
            None => {
                out.push_str("@@");
                rest = tail;
            }
        }
    }
    out.push_str(rest);
    out
}
