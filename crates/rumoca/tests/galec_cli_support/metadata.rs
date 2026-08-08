//! Independent checks for target-owned eFMI metadata.

use std::path::Path;

use time::OffsetDateTime;
use time::format_description::well_known::Rfc3339;
use uuid::Uuid;

pub(crate) fn assert_manifest_id(value: &str, path: &Path) {
    let inner = value
        .strip_prefix('{')
        .and_then(|rest| rest.strip_suffix('}'))
        .unwrap_or_else(|| {
            panic!(
                "manifest id `{value}` in {} must be wrapped in braces",
                path.display()
            )
        });
    let segment_lengths = inner.split('-').map(str::len).collect::<Vec<_>>();
    assert_eq!(
        segment_lengths,
        [8, 4, 4, 4, 12],
        "manifest id `{value}` in {} must use the 8-4-4-4-12 UUID form",
        path.display()
    );
    assert!(
        inner
            .chars()
            .all(|character| { character == '-' || character.is_ascii_hexdigit() }),
        "manifest id `{value}` in {} contains a non-hex UUID character",
        path.display()
    );
    Uuid::parse_str(inner).unwrap_or_else(|error| {
        panic!(
            "manifest id `{value}` in {} must contain a valid UUID: {error}",
            path.display()
        )
    });
}

pub(crate) fn assert_strict_utc_timestamp(value: &str, path: &Path) {
    assert_eq!(
        value.len(),
        20,
        "generationDateAndTime `{value}` in {} must have no fractional seconds",
        path.display()
    );
    assert!(
        value.as_bytes().get(4) == Some(&b'-')
            && value.as_bytes().get(7) == Some(&b'-')
            && value.as_bytes().get(10) == Some(&b'T')
            && value.as_bytes().get(13) == Some(&b':')
            && value.as_bytes().get(16) == Some(&b':')
            && value.ends_with('Z'),
        "generationDateAndTime `{value}` in {} must match YYYY-MM-DDTHH:MM:SSZ",
        path.display()
    );
    let parsed = OffsetDateTime::parse(value, &Rfc3339).unwrap_or_else(|error| {
        panic!(
            "generationDateAndTime `{value}` in {} must be RFC 3339: {error}",
            path.display()
        )
    });
    assert_eq!(
        parsed.offset(),
        time::UtcOffset::UTC,
        "generationDateAndTime `{value}` in {} must use the UTC offset",
        path.display()
    );
}
