//! Helper functions for grammar conversion.

use crate::generated::modelica_grammar_trait;
use rumoca_core::{SourceId, Span};
use std::sync::Arc;

/// Helper to format location info from a token for error messages
pub(crate) fn loc_info(token: &rumoca_core::Token) -> String {
    let loc = &token.location;
    format!(
        " at {}:{}:{}",
        loc.file_name, loc.start_line, loc.start_column
    )
}

/// Create a location spanning from the start of one token to the end of another
pub(crate) fn span_location(
    start: &rumoca_core::Token,
    end: &rumoca_core::Token,
) -> rumoca_core::Location {
    rumoca_core::Location {
        start_line: start.location.start_line,
        start_column: start.location.start_column,
        end_line: end.location.end_line,
        end_column: end.location.end_column,
        start: start.location.start,
        end: end.location.end,
        file_name: start.location.file_name.clone(),
    }
}

pub(crate) fn location_span(location: &rumoca_core::Location) -> anyhow::Result<Span> {
    let start = location.start as usize;
    let end = location.end as usize;
    if end > start && !location.file_name.is_empty() {
        Ok(Span::from_offsets(
            SourceId::from_source_name(&location.file_name),
            start,
            end,
        ))
    } else {
        anyhow::bail!("missing parser source location at {location}")
    }
}

pub(crate) fn token_span(token: &rumoca_core::Token) -> anyhow::Result<Span> {
    location_span(&token.location)
        .map_err(|err| anyhow::anyhow!("token '{}' has no source span: {err}", token.text))
}

pub(crate) fn merge_spans(start: Span, end: Span) -> Span {
    if start.is_dummy() {
        return end;
    }
    if end.is_dummy() {
        return start;
    }
    Span::from_offsets(start.source, start.start.0, end.end.0.max(start.start.0))
}

pub(crate) fn expression_list_span(elements: &[rumoca_ir_ast::Expression]) -> Option<Span> {
    match (elements.first(), elements.last()) {
        (Some(first), Some(last)) => Some(merge_spans(first.span(), last.span())),
        _ => None,
    }
}

pub(crate) fn required_expression_list_span(
    elements: &[rumoca_ir_ast::Expression],
    context: &'static str,
) -> anyhow::Result<Span> {
    let Some(span) = expression_list_span(elements) else {
        anyhow::bail!("{context} has no source expression span");
    };
    Ok(span)
}

/// Helper to collect elements from array_arguments into an Expression
/// Handles both simple arrays like {1, 2, 3} and array comprehensions like {i for i in 1:10}
pub(crate) fn collect_array_elements(
    args: &modelica_grammar_trait::ArrayArguments,
) -> anyhow::Result<rumoca_ir_ast::Expression> {
    // Check if this is an array comprehension or a simple array
    if let Some(opt) = &args.array_arguments_opt {
        match &opt.array_arguments_opt_group {
            modelica_grammar_trait::ArrayArgumentsOptGroup::CommaArrayArgumentsNonFirst(
                comma_args,
            ) => {
                // Simple array: collect all elements
                let mut elements = vec![args.expression.clone()];
                collect_array_non_first(&comma_args.array_arguments_non_first, &mut elements);
                let span = required_expression_list_span(&elements, "array arguments")?;
                Ok(rumoca_ir_ast::Expression::Array {
                    span,
                    elements,
                    is_matrix: false,
                })
            }
            modelica_grammar_trait::ArrayArgumentsOptGroup::ForForIndicesArrayArgumentsOpt0(
                for_group,
            ) => {
                // Array comprehension: {expr for i in range, j in range2, ... [if filter]}
                let indices = convert_for_indices(&for_group.for_indices)?;
                let filter = for_group
                    .array_arguments_opt0
                    .as_ref()
                    .map(|opt| Arc::new(opt.expression.clone()));
                Ok(rumoca_ir_ast::Expression::ArrayComprehension {
                    span: args.expression.span(),
                    expr: Arc::new(args.expression.clone()),
                    indices,
                    filter,
                })
            }
        }
    } else {
        // Single element array
        Ok(rumoca_ir_ast::Expression::Array {
            span: args.expression.span(),
            elements: vec![args.expression.clone()],
            is_matrix: false,
        })
    }
}

/// Convert grammar ForIndices to AST ForIndex vec
pub(crate) fn convert_for_indices(
    indices: &modelica_grammar_trait::ForIndices,
) -> anyhow::Result<Vec<rumoca_ir_ast::ForIndex>> {
    let mut result = Vec::new();

    // First index
    result.push(convert_for_index(&indices.for_index)?);

    // Additional indices
    for item in &indices.for_indices_list {
        result.push(convert_for_index(&item.for_index)?);
    }

    Ok(result)
}

/// Convert a single grammar ForIndex to AST ForIndex
fn convert_for_index(
    index: &modelica_grammar_trait::ForIndex,
) -> anyhow::Result<rumoca_ir_ast::ForIndex> {
    let range = match &index.for_index_opt {
        Some(opt) => opt.expression.clone(),
        None => rumoca_ir_ast::Expression::Empty {
            span: token_span(&index.ident)?,
        },
    };

    Ok(rumoca_ir_ast::ForIndex {
        ident: index.ident.clone(),
        range,
    })
}

/// Helper to recursively collect elements from array_arguments_non_first chain
pub(crate) fn collect_array_non_first(
    args: &modelica_grammar_trait::ArrayArgumentsNonFirst,
    elements: &mut Vec<rumoca_ir_ast::Expression>,
) {
    // Add current element
    elements.push(args.expression.clone());

    // Recursively collect remaining elements
    if let Some(opt) = &args.array_arguments_non_first_opt {
        collect_array_non_first(&opt.array_arguments_non_first, elements);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn location_span_rejects_missing_source_location() {
        let err = location_span(&rumoca_core::Location::default()).unwrap_err();
        assert!(
            err.to_string().contains("missing parser source location"),
            "unexpected error: {err}"
        );
    }
}
