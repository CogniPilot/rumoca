use serde::{Deserialize, Serialize};
use std::ops::Range;

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct Wire {
    start: usize,
    end: usize,
}

pub(super) fn serialize<S: serde::Serializer>(
    range: &Range<usize>,
    serializer: S,
) -> Result<S::Ok, S::Error> {
    Wire {
        start: range.start,
        end: range.end,
    }
    .serialize(serializer)
}

pub(super) fn deserialize<'de, D: serde::Deserializer<'de>>(
    deserializer: D,
) -> Result<Range<usize>, D::Error> {
    let wire = Wire::deserialize(deserializer)?;
    Ok(wire.start..wire.end)
}
