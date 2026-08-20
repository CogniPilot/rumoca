use rumoca_core::{Location, SourceMap, Span};

use crate::FlattenError;

pub(crate) fn required_location_span(
    source_map: &SourceMap,
    location: &Location,
    context: &str,
) -> Result<Span, FlattenError> {
    if !location.has_source() {
        return Err(FlattenError::missing_source_context(format!(
            "{context} is missing a non-empty source location"
        )));
    }
    source_map
        .try_span(
            location.source,
            location.start as usize,
            location.end as usize,
        )
        .ok_or_else(|| {
            let file_name = source_map
                .name(location.source)
                .unwrap_or(UNKNOWN_SOURCE_DISPLAY_NAME);
            FlattenError::missing_source_context(format!(
                "source file `{file_name}` for {context} was not found"
            ))
        })
}

/// Placeholder used when a `SourceId` has no registered name in the source map.
pub(crate) const UNKNOWN_SOURCE_DISPLAY_NAME: &str = "<unknown source>";
