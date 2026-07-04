//! Identity and time primitives for eFMI manifests.
//!
//! Ground truth: `assets/efmi-schemas/efmiIdentifierType.xsd` and the
//! `generationDateAndTime` pattern of `efmiManifestAttributes.xsd`.
//!
//! Serialization paths in this crate take ids and timestamps as **values**
//! (deterministic, testable); the generation helpers ([`ManifestId::generate`],
//! [`UtcTimestamp::now_utc`]) are for callers that mint fresh identities and
//! are never invoked implicitly during serialization.

use std::collections::BTreeSet;
use std::fmt;

use time::macros::format_description;
use time::{OffsetDateTime, PrimitiveDateTime};
use uuid::Uuid;

use crate::manifest_context::diagnostic::EfmiError;

/// Brace-wrapped manifest UUID (`efmiManifestIdentifierType`).
///
/// Parsing accepts upper- and lowercase hex (as the XSD does); display is
/// normalized to `{8-4-4-4-12}` lowercase hex.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ManifestId(Uuid);

impl ManifestId {
    /// Parse and validate a brace-wrapped UUID string.
    pub fn parse(value: &str) -> Result<Self, EfmiError> {
        let invalid = |reason: &str| EfmiError::InvalidManifestId {
            value: value.to_owned(),
            reason: reason.to_owned(),
        };
        let inner = value
            .strip_prefix('{')
            .and_then(|rest| rest.strip_suffix('}'))
            .ok_or_else(|| invalid("must be wrapped in `{` and `}`"))?;
        let segments: Vec<&str> = inner.split('-').collect();
        let lengths: Vec<usize> = segments.iter().map(|s| s.len()).collect();
        if lengths != [8, 4, 4, 4, 12] {
            return Err(invalid("must have hex segments of lengths 8-4-4-4-12"));
        }
        if !segments
            .iter()
            .all(|s| s.chars().all(|c| c.is_ascii_hexdigit()))
        {
            return Err(invalid("segments must contain only hex digits"));
        }
        let uuid = Uuid::parse_str(inner).map_err(|e| invalid(&format!("unparsable UUID: {e}")))?;
        Ok(Self(uuid))
    }

    /// Mint a fresh random (v4) manifest id. Generation helper for callers;
    /// never called by serialization paths.
    pub fn generate() -> Self {
        Self(Uuid::new_v4())
    }

    /// Wrap an existing UUID value.
    pub fn from_uuid(uuid: Uuid) -> Self {
        Self(uuid)
    }

    /// The nil (all-zero) UUID `{00000000-0000-0000-0000-000000000000}`. A
    /// deterministic placeholder for validation-only assembly that must not
    /// mint randomness — no `getrandom`, so it is safe on `wasm32`.
    pub fn nil() -> Self {
        Self(Uuid::nil())
    }

    /// The underlying UUID.
    pub fn uuid(&self) -> Uuid {
        self.0
    }
}

impl fmt::Display for ManifestId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{{}}}", self.0.hyphenated())
    }
}

/// Strict UTC timestamp matching exactly `YYYY-MM-DDTHH:MM:SSZ`
/// (the `generationDateAndTime` XSD pattern: no offset, no fractions).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UtcTimestamp(PrimitiveDateTime);

impl UtcTimestamp {
    /// Parse and validate a strict UTC timestamp string.
    pub fn parse(value: &str) -> Result<Self, EfmiError> {
        let format = format_description!("[year]-[month]-[day]T[hour]:[minute]:[second]Z");
        let parsed =
            PrimitiveDateTime::parse(value, format).map_err(|e| EfmiError::InvalidTimestamp {
                value: value.to_owned(),
                reason: e.to_string(),
            })?;
        Ok(Self(parsed))
    }

    /// A fixed placeholder timestamp (`2000-01-01T00:00:00Z`) for
    /// validation-only assembly that must not read the wall clock — no
    /// `SystemTime::now`, so it is safe on `wasm32`.
    pub fn placeholder() -> Self {
        Self::parse("2000-01-01T00:00:00Z")
            .expect("the hardcoded placeholder timestamp is a valid strict-UTC literal")
    }

    /// Current wall-clock time, truncated to whole seconds. Generation helper
    /// for callers; never called by serialization paths.
    pub fn now_utc() -> Self {
        let now = OffsetDateTime::now_utc();
        let time = now
            .time()
            .replace_nanosecond(0)
            .expect("0 nanoseconds is always a valid time component");
        Self(PrimitiveDateTime::new(now.date(), time))
    }
}

impl fmt::Display for UtcTimestamp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let format = format_description!("[year]-[month]-[day]T[hour]:[minute]:[second]Z");
        let rendered = self
            .0
            .format(format)
            .expect("formatting a validated timestamp cannot fail");
        f.write_str(&rendered)
    }
}

/// Non-manifest id (`efmiIdentifierType`): one or more characters from the
/// XSD character set (ASCII alphanumerics plus `# ' ( ) , - . / : [ ] _ { }`).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identifier(String);

impl Identifier {
    /// Validate and wrap an id value.
    pub fn new(value: impl Into<String>) -> Result<Self, EfmiError> {
        let value = value.into();
        if value.is_empty() {
            return Err(EfmiError::InvalidIdentifier {
                value,
                reason: "must not be empty".to_owned(),
            });
        }
        if let Some(bad) = value.chars().find(|c| !is_identifier_char(*c)) {
            return Err(EfmiError::InvalidIdentifier {
                reason: format!("character `{bad}` is outside the efmiIdentifierType charset"),
                value,
            });
        }
        Ok(Self(value))
    }

    /// The validated id text.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

const fn is_identifier_char(c: char) -> bool {
    c.is_ascii_alphanumeric()
        || matches!(
            c,
            '#' | '\'' | '(' | ')' | ',' | '-' | '.' | '/' | ':' | '[' | ']' | '_' | '{' | '}'
        )
}

/// Reject characters that can never survive attribute serialization.
///
/// XML 1.0 forbids C0 control characters other than tab/LF/CR (and
/// U+FFFE/U+FFFF) anywhere in a document; quick-xml escapes only `<>&'"` and
/// would emit them raw, producing non-well-formed bytes that `xmllint`
/// rejects. Tab/LF/CR themselves are rejected too: XML attribute-value
/// normalization silently replaces them with spaces on re-parse, so a value
/// containing them cannot round-trip byte-exactly (and `xs:normalizedString`
/// forbids them outright). Returns the rejection reason, if any.
fn invalid_attribute_char_reason(value: &str) -> Option<String> {
    let bad = value
        .chars()
        .find(|&c| (c as u32) < 0x20 || c == '\u{FFFE}' || c == '\u{FFFF}')?;
    Some(if matches!(bad, '\t' | '\n' | '\r') {
        "must not contain tab, line feed, or carriage return".to_owned()
    } else {
        format!(
            "must not contain XML-illegal character U+{:04X}",
            bad as u32
        )
    })
}

/// Non-empty `xs:normalizedString` value: no tab, line feed, carriage
/// return, or XML-illegal character. Stricter than the XSD (which permits
/// the empty string) because an empty name/version/tool attribute is always
/// a generator bug; use `None` for absent optional attributes instead.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NormalizedText(String);

impl NormalizedText {
    /// Validate and wrap a normalized-string value.
    pub fn new(value: impl Into<String>) -> Result<Self, EfmiError> {
        let value = value.into();
        if value.is_empty() {
            return Err(EfmiError::InvalidText {
                value,
                reason: "must not be empty".to_owned(),
            });
        }
        if let Some(reason) = invalid_attribute_char_reason(&value) {
            return Err(EfmiError::InvalidText { value, reason });
        }
        Ok(Self(value))
    }

    /// The validated text.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for NormalizedText {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

/// Container, manifest, or in-container file name
/// (`efmiNameWithoutSlashesType`): non-empty, normalized, and free of `/`.
/// Backslashes are also rejected — stricter than the XSD pattern — because a
/// `\` in a file name is ambiguous on Windows consumers (path separator vs.
/// name character).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NameWithoutSlashes(String);

impl NameWithoutSlashes {
    /// Validate and wrap a slash-free name.
    pub fn new(value: impl Into<String>) -> Result<Self, EfmiError> {
        let value = value.into();
        let invalid =
            |value: String, reason: String| EfmiError::InvalidContainerName { value, reason };
        if value.is_empty() {
            return Err(invalid(value, "must not be empty".to_owned()));
        }
        if value.contains('/') {
            return Err(invalid(value, "must not contain `/`".to_owned()));
        }
        if value.contains('\\') {
            return Err(invalid(
                value,
                "must not contain `\\` (ambiguous path separator on Windows)".to_owned(),
            ));
        }
        if let Some(reason) = invalid_attribute_char_reason(&value) {
            return Err(invalid(value, reason));
        }
        Ok(Self(value))
    }

    /// The validated name.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for NameWithoutSlashes {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

/// Directory part of an in-container file path (`efmiFilePathType`): starts
/// with `./`, ends with `/`. Stricter than the XSD pattern in rejecting empty,
/// `.` and `..` segments (which the pattern technically admits but which are
/// always generator bugs) and backslashes (ambiguous path separators on
/// Windows consumers).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FilePath(String);

impl FilePath {
    /// Validate and wrap a container-relative directory path.
    pub fn new(value: impl Into<String>) -> Result<Self, EfmiError> {
        let value = value.into();
        let invalid = |value: String, reason: &str| EfmiError::InvalidFilePath {
            value,
            reason: reason.to_owned(),
        };
        let Some(rest) = value.strip_prefix("./") else {
            return Err(invalid(value, "must start with `./`"));
        };
        if !value.ends_with('/') {
            return Err(invalid(value, "must end with `/`"));
        }
        if value.contains('\\') {
            return Err(invalid(
                value,
                "must not contain `\\` (ambiguous path separator on Windows)",
            ));
        }
        if let Some(reason) = invalid_attribute_char_reason(&value) {
            return Err(EfmiError::InvalidFilePath { value, reason });
        }
        let segments_ok = rest
            .split_terminator('/')
            .all(|segment| !segment.is_empty() && segment != "." && segment != "..");
        if !segments_ok {
            return Err(invalid(
                value,
                "segments must be non-empty and not `.`/`..`",
            ));
        }
        Ok(Self(value))
    }

    /// The container root directory, `./`.
    pub fn root() -> Self {
        Self("./".to_owned())
    }

    /// The validated path text.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for FilePath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

/// Per-manifest id-uniqueness checker: all `id` values in one manifest file
/// must be unique across the whole file, not per element type (eFMI §2.3
/// manifest principle 6).
#[derive(Debug, Default)]
pub struct IdRegistry {
    seen: BTreeSet<String>,
}

impl IdRegistry {
    /// Fresh registry for one manifest file.
    pub fn new() -> Self {
        Self::default()
    }

    /// Register an id value, failing on the second occurrence.
    pub fn register(&mut self, id: &str) -> Result<(), EfmiError> {
        if !self.seen.insert(id.to_owned()) {
            return Err(EfmiError::DuplicateId { id: id.to_owned() });
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn manifest_id_accepts_valid_forms() {
        let accepted = [
            "{2f1a03de-9f14-4a3d-8b1e-73c60d0a1c11}",
            "{ABCDEF01-2345-6789-ABCD-EF0123456789}",
            "{00000000-0000-0000-0000-000000000000}",
        ];
        for value in accepted {
            assert!(ManifestId::parse(value).is_ok(), "should accept {value}");
        }
    }

    #[test]
    fn manifest_id_rejects_invalid_forms() {
        let rejected = [
            "",
            "2f1a03de-9f14-4a3d-8b1e-73c60d0a1c11",
            "{2f1a03de-9f14-4a3d-8b1e-73c60d0a1c11",
            "2f1a03de-9f14-4a3d-8b1e-73c60d0a1c11}",
            "{2f1a03de9f144a3d8b1e73c60d0a1c11}",
            "{2f1a03de-9f14-4a3d-8b1e-73c60d0a1c1}",
            "{2f1a03de-9f14-4a3d-8b1e-73c60d0a1c111}",
            "{2g1a03de-9f14-4a3d-8b1e-73c60d0a1c11}",
            "{urn:uuid:2f1a03de-9f14-4a3d-8b1e-73c60d0a1c11}",
            "{{2f1a03de-9f14-4a3d-8b1e-73c60d0a1c11}}",
        ];
        for value in rejected {
            let err = ManifestId::parse(value).expect_err(value);
            assert_eq!(err.code(), "EFM001", "wrong code for {value}");
        }
    }

    #[test]
    fn manifest_id_displays_lowercase_braced() {
        let id = ManifestId::parse("{ABCDEF01-2345-6789-ABCD-EF0123456789}").unwrap();
        assert_eq!(id.to_string(), "{abcdef01-2345-6789-abcd-ef0123456789}");
    }

    #[test]
    fn timestamp_accepts_strict_utc() {
        let accepted = [
            "2026-07-02T12:00:00Z",
            "0001-01-01T00:00:00Z",
            "2026-12-31T23:59:59Z",
        ];
        for value in accepted {
            let ts = UtcTimestamp::parse(value).expect(value);
            assert_eq!(ts.to_string(), value, "round-trip for {value}");
        }
    }

    #[test]
    fn timestamp_rejects_lenient_forms() {
        let rejected = [
            "",
            "2026-07-02T12:00:00",
            "2026-07-02T12:00:00+00:00",
            "2026-07-02T12:00:00.5Z",
            "2026-07-02 12:00:00Z",
            "2026-7-2T12:00:00Z",
            "2026-07-02T12:00:00Zjunk",
            "2026-13-01T12:00:00Z",
            "2026-02-30T12:00:00Z",
            "2026-07-02T25:00:00Z",
            "2026-07-02T12:00:00z",
        ];
        for value in rejected {
            let err = UtcTimestamp::parse(value).expect_err(value);
            assert_eq!(err.code(), "EFM002", "wrong code for {value}");
        }
    }

    #[test]
    fn identifier_charset_enforced() {
        for value in [
            "a",
            "BM_1",
            "a.b[2]",
            "previous(x)",
            "'quoted'",
            "a-b:c/d,e#f",
        ] {
            assert!(Identifier::new(value).is_ok(), "should accept {value}");
        }
        for value in ["", "a b", "a\tb", "α", "a;b", "a\"b"] {
            let err = Identifier::new(value).expect_err(value);
            assert_eq!(err.code(), "EFM003", "wrong code for {value:?}");
        }
    }

    #[test]
    fn normalized_text_rejects_control_whitespace() {
        assert!(NormalizedText::new("Modelica.Blocks.PI").is_ok());
        for value in ["", "a\nb", "a\tb", "a\rb"] {
            let err = NormalizedText::new(value).expect_err(value);
            assert_eq!(err.code(), "EFM004");
        }
    }

    /// XML 1.0 forbids C0 controls other than tab/LF/CR (and U+FFFE/U+FFFF);
    /// quick-xml would emit them raw, so validated models must reject them
    /// (they otherwise serialize to bytes xmllint refuses to parse).
    #[test]
    fn normalized_text_rejects_xml_illegal_characters() {
        for value in ["a\u{1}b", "bell\u{7}", "\u{0}", "a\u{FFFE}", "a\u{FFFF}b"] {
            let err = NormalizedText::new(value).expect_err(value);
            assert_eq!(err.code(), "EFM004", "wrong code for {value:?}");
            assert!(
                err.to_string().contains("XML-illegal"),
                "reason must name the XML-illegal character, got: {err}"
            );
        }
        // XML-legal beyond ASCII controls stays accepted.
        assert!(NormalizedText::new("Grüße λ \u{7F}").is_ok());
    }

    #[test]
    fn name_without_slashes_enforced() {
        assert!(NameWithoutSlashes::new("AlgorithmCode").is_ok());
        assert!(NameWithoutSlashes::new("manifest.xml").is_ok());
        for value in ["", "a/b", "a\nb", "a\\b", "a\u{1}b"] {
            let err = NameWithoutSlashes::new(value).expect_err(value);
            assert_eq!(err.code(), "EFM005", "wrong code for {value:?}");
        }
    }

    #[test]
    fn file_path_shape_enforced() {
        for value in ["./", "./src/", "./a/b/"] {
            assert!(FilePath::new(value).is_ok(), "should accept {value}");
        }
        for value in [
            "",
            ".",
            "src/",
            "./src",
            "/src/",
            ".//",
            "././",
            "./../",
            "./a\\b/",
            "./a\u{1}b/",
        ] {
            let err = FilePath::new(value).expect_err(value);
            assert_eq!(err.code(), "EFM006", "wrong code for {value:?}");
        }
    }

    #[test]
    fn id_registry_flags_duplicates() {
        let mut registry = IdRegistry::new();
        registry.register("A").unwrap();
        registry.register("B").unwrap();
        let err = registry.register("A").unwrap_err();
        assert_eq!(err, EfmiError::DuplicateId { id: "A".into() });
    }
}
