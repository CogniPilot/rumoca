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

/// The vendored eFMI Beta-1 schema tree (GAL-023).
pub(super) fn vendored_schemas_dir() -> PathBuf {
    let dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("../rumoca-efmi/assets/efmi-schemas");
    assert!(
        dir.is_dir(),
        "vendored schema tree missing at {}",
        dir.display()
    );
    dir
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
