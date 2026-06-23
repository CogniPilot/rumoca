use rumoca_core::{Location, SourceMap, Span};

use crate::FlattenError;

pub(crate) fn required_location_span(
    source_map: &SourceMap,
    location: &Location,
    context: &str,
) -> Result<Span, FlattenError> {
    if location.file_name.is_empty() || location.start >= location.end {
        return Err(FlattenError::missing_source_context(format!(
            "{context} is missing a non-empty source location"
        )));
    }
    source_map
        .try_location_to_span(
            &location.file_name,
            location.start as usize,
            location.end as usize,
        )
        .ok_or_else(|| {
            FlattenError::missing_source_context(format!(
                "source file `{}` for {context} was not found",
                location.file_name
            ))
        })
}
