//! Shared eFMU-container XML inspection helpers for the packaging suites
//! (`cli_target_galec.rs`, `cli_target_galec_production.rs`).
//!
//! Included per suite via `#[path = "galec_cli_support/container_xml.rs"]`
//! — see `galec_cli_support/cli.rs` for the include-pattern rationale.
//! All XML readers work over the exact on-disk bytes (quick-xml), because
//! the eFMI checksum web is defined over written bytes, never re-serialized
//! documents.

use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

/// The vendored eFMI Beta-1 schema tree (GAL-023). Lives beside the generic
/// container build step in the `rumoca` crate now that the eFMI packaging crate is
/// dissolved (contract §6).
pub(super) fn vendored_schemas_dir() -> PathBuf {
    let dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("assets/efmi-schemas");
    assert!(
        dir.is_dir(),
        "vendored schema tree missing at {}",
        dir.display()
    );
    dir
}

/// Validate an XML file against an XSD by shelling out to `xmllint`
/// (`xmllint --noout --schema <xsd> <xml>`). The xmllint gate lives at the
/// build-step / template-CI home now (contract §6).
///
/// A missing `xmllint` is a hard, actionable error — never a silent skip
/// (GAL-012/GAL-021): the caller `.expect()`s success, so an absent xmllint
/// fails the suite instead of passing it.
pub(super) fn validate_against_xsd(xml_path: &Path, xsd_path: &Path) -> Result<(), String> {
    let output = Command::new("xmllint")
        .arg("--noout")
        .arg("--schema")
        .arg(xsd_path)
        .arg(xml_path)
        .output()
        .map_err(|error| match error.kind() {
            std::io::ErrorKind::NotFound => "xmllint not found on PATH: XSD validation requires \
                 libxml2's xmllint (Debian/Ubuntu: `apt-get install libxml2-utils`)"
                .to_owned(),
            _ => format!("failed to run xmllint: {error}"),
        })?;
    if output.status.success() {
        return Ok(());
    }
    let mut combined = String::from_utf8_lossy(&output.stderr).into_owned();
    combined.push_str(&String::from_utf8_lossy(&output.stdout));
    Err(format!(
        "`{}` failed XSD validation against `{}`:\n{combined}",
        xml_path.display(),
        xsd_path.display()
    ))
}

/// Relative `/`-separated paths of every file under `root`.
pub(super) fn relative_file_paths(root: &Path) -> BTreeSet<String> {
    walkdir::WalkDir::new(root)
        .into_iter()
        .map(|entry| entry.expect("walk directory tree"))
        .filter(|entry| entry.file_type().is_file())
        .map(|entry| {
            entry
                .path()
                .strip_prefix(root)
                .expect("walked path is under root")
                .components()
                .map(|c| c.as_os_str().to_str().expect("UTF-8 path").to_owned())
                .collect::<Vec<_>>()
                .join("/")
        })
        .collect()
}

// ---------------------------------------------------------------------------
// Negative-schema corruption helpers (contract §7): prove the vendored XSDs
// are strict enough to REJECT a manifest regression, not only to accept the
// current output. A rendered (valid) manifest is corrupted one way, then
// `xmllint` must reject it. Ported from the dissolved rumoca-efmi crate's
// `negative_*_schema_cases_are_rejected` tests, now driving TEMPLATE output.
// ---------------------------------------------------------------------------

/// Replace the first occurrence of `needle` with `replacement`; panic if the
/// needle is absent, so a corruption can never silently no-op into a pass.
pub(super) fn surgically(xml: &str, needle: &str, replacement: &str) -> String {
    assert!(
        xml.contains(needle),
        "corruption needle `{needle}` not found in rendered manifest"
    );
    xml.replacen(needle, replacement, 1)
}

/// Drop the single line containing `needle` (the manifests print one element
/// per line); panic if absent.
pub(super) fn without_line(xml: &str, needle: &str) -> String {
    assert!(xml.contains(needle), "line needle `{needle}` not found");
    let mut out: String = xml
        .lines()
        .filter(|line| !line.contains(needle))
        .collect::<Vec<_>>()
        .join("\n");
    out.push('\n');
    out
}

/// Drop the block from the line containing `open` through the line containing
/// `close`, inclusive; panic if either is absent.
pub(super) fn without_block(xml: &str, open: &str, close: &str) -> String {
    let lines: Vec<&str> = xml.lines().collect();
    let start = lines
        .iter()
        .position(|line| line.contains(open))
        .unwrap_or_else(|| panic!("block open `{open}` not found"));
    let end = lines[start..]
        .iter()
        .position(|line| line.contains(close))
        .map(|offset| start + offset)
        .unwrap_or_else(|| panic!("block close `{close}` not found"));
    let mut out: String = lines
        .iter()
        .enumerate()
        .filter(|(i, _)| *i < start || *i > end)
        .map(|(_, line)| *line)
        .collect::<Vec<_>>()
        .join("\n");
    out.push('\n');
    out
}

/// Move the single line containing `line_needle` to just after the line
/// containing `after_needle` — a well-formed but out-of-`xs:sequence`-order
/// document; panic if either is absent.
pub(super) fn move_line_after(xml: &str, line_needle: &str, after_needle: &str) -> String {
    let moved = xml
        .lines()
        .find(|line| line.contains(line_needle))
        .unwrap_or_else(|| panic!("line `{line_needle}` not found"))
        .to_owned();
    let mut out = String::new();
    for line in without_line(xml, line_needle).lines() {
        out.push_str(line);
        out.push('\n');
        if line.contains(after_needle) {
            out.push_str(&moved);
            out.push('\n');
        }
    }
    out
}

/// Assert `xmllint` REJECTS `xml` against `xsd` (never a skip; a missing
/// xmllint fails via [`validate_against_xsd`]).
pub(super) fn assert_xsd_rejects(label: &str, xml: &str, xsd: &Path) {
    let dir = tempfile::tempdir().expect("tempdir");
    let path = dir.path().join("corrupted.xml");
    fs::write(&path, xml).expect("write corrupted manifest");
    assert!(
        validate_against_xsd(&path, xsd).is_err(),
        "case `{label}`: xmllint must REJECT this corrupted manifest, but it validated"
    );
}

/// All values of attribute `name` anywhere in an XML document, in document
/// order (quick-xml over the exact on-disk bytes).
pub(super) fn attribute_values(xml_path: &Path, name: &str) -> Vec<String> {
    let bytes = fs::read(xml_path).expect("read XML file");
    let mut reader = quick_xml::Reader::from_reader(bytes.as_slice());
    let mut values = Vec::new();
    let mut buf = Vec::new();
    loop {
        use quick_xml::events::Event;
        match reader.read_event_into(&mut buf).expect("well-formed XML") {
            Event::Eof => break,
            Event::Start(element) | Event::Empty(element) => {
                values.extend(named_attribute_values(&element, name));
            }
            _ => {}
        }
        buf.clear();
    }
    values
}

/// Values of every attribute called `name` on one element.
fn named_attribute_values(element: &quick_xml::events::BytesStart<'_>, name: &str) -> Vec<String> {
    element
        .attributes()
        .map(|attribute| attribute.expect("well-formed attribute"))
        .filter(|attribute| attribute.key.as_ref() == name.as_bytes())
        .map(|attribute| {
            attribute
                .unescape_value()
                .expect("unescapable attribute")
                .into_owned()
        })
        .collect()
}

/// The single value of attribute `name` in the document.
pub(super) fn sole_attribute_value(xml_path: &Path, name: &str) -> String {
    let values = attribute_values(xml_path, name);
    assert_eq!(
        values.len(),
        1,
        "expected exactly one `{name}` attribute in {}, found {values:?}",
        xml_path.display()
    );
    values.into_iter().next().expect("length checked")
}

/// Replace every brace-wrapped UUID with `{UUID}` — the documented
/// per-build nondeterminism of the container metadata.
pub(super) fn mask_uuids(text: &str) -> String {
    /// Length of the hyphenated hex form between the braces.
    const INNER: usize = 36;
    let mut out = String::with_capacity(text.len());
    let mut rest = text;
    while let Some(at) = rest.find('{') {
        let candidate = &rest.as_bytes()[at..];
        let is_uuid = candidate.len() > INNER + 1
            && candidate[INNER + 1] == b'}'
            && candidate[1..=INNER]
                .iter()
                .all(|b| b.is_ascii_hexdigit() || *b == b'-');
        out.push_str(&rest[..at]);
        if is_uuid {
            out.push_str("{UUID}");
            rest = &rest[at + INNER + 2..];
        } else {
            out.push('{');
            rest = &rest[at + 1..];
        }
    }
    out.push_str(rest);
    out
}

/// Replace the value of `attribute="…"` occurrences with a fixed marker.
pub(super) fn mask_attribute(text: &str, attribute: &str) -> String {
    let needle = format!("{attribute}=\"");
    let mut out = String::with_capacity(text.len());
    let mut rest = text;
    while let Some(at) = rest.find(&needle) {
        let value_start = at + needle.len();
        let value_len = rest[value_start..]
            .find('"')
            .expect("attribute value must be quote-terminated");
        out.push_str(&rest[..value_start]);
        out.push_str("MASKED");
        rest = &rest[value_start + value_len..];
    }
    out.push_str(rest);
    out
}
