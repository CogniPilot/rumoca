//! Helper functions for grammar conversion.

use crate::generated::modelica_grammar_trait;
use rumoca_core::Span;
use std::sync::Arc;

/// Helper to format location info from a token for error messages.
///
/// SPEC_0008 requires source-backed diagnostics to carry their origin. These
/// conversion failures are bare `anyhow` strings whose fallback spans are
/// source-free, so the message is the only carrier and must name the file.
/// A `Location` cannot resolve its own name, so the file the parser is
/// currently tokenizing is used when its identity matches, and the stable
/// `SourceId` placeholder name otherwise; both identify the same file.
pub(crate) fn loc_info(token: &rumoca_core::Token) -> String {
    let loc = &token.location;
    format!(
        " at line {}:{} in `{}`",
        loc.start_line,
        loc.start_column,
        source_display_name(loc.source)
    )
}

fn source_display_name(source: rumoca_core::SourceId) -> String {
    current_parse_file_name(source).unwrap_or_else(|| rumoca_core::placeholder_source_name(source))
}

/// Path of the file being tokenized, when it is the file `source` came from.
fn current_parse_file_name(source: rumoca_core::SourceId) -> Option<String> {
    crate::PARSE_SOURCE_ID.with(|cell| {
        let memo = cell.borrow();
        let (path, id) = memo.as_ref()?;
        (*id == source).then(|| path.to_string_lossy().into_owned())
    })
}

/// Create a location spanning from the start of one token to the end of another
pub(crate) fn span_location(
    start: &rumoca_core::Token,
    end: &rumoca_core::Token,
) -> rumoca_core::Location {
    start.location.merged_with(&end.location)
}

pub(crate) fn location_span(location: &rumoca_core::Location) -> anyhow::Result<Span> {
    if location.has_source() {
        Ok(location.span())
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

    #[test]
    fn loc_info_keeps_the_file_identity_outside_a_parse() {
        let source = rumoca_core::SourceId::from_source_name("pkg/A.mo");
        let token = rumoca_core::Token {
            text: Arc::from("x"),
            location: rumoca_core::Location {
                start_line: 3,
                start_column: 7,
                end_line: 3,
                end_column: 8,
                start: 10,
                end: 11,
                source,
            },
            token_number: 0,
            token_type: 0,
        };

        let info = loc_info(&token);
        assert!(
            info.contains("line 3:7"),
            "unexpected location info: {info}"
        );
        let placeholder = rumoca_core::placeholder_source_name(source);
        assert!(
            info.contains(&placeholder),
            "location info must identify the file: {info}"
        );
        assert_eq!(
            rumoca_core::source_id_for_name(&placeholder),
            source,
            "the printed identity must resolve back to the originating file"
        );
    }

    #[test]
    fn conversion_errors_name_the_file_being_parsed() {
        // MLS §7.3.1: attributes of basic types may not be redeclared. The
        // conversion rejects it with a bare `anyhow` message, which is the only
        // place the offending file can still be named.
        let source = "model M\n  A a(redeclare Real start = 1.0);\nend M;\n";
        let err = crate::parse_to_ast(source, "pkg/Redeclare.mo")
            .expect_err("redeclaring `start` must be rejected");
        let rendered = format!("{err:?}");
        assert!(
            rendered.contains("pkg/Redeclare.mo"),
            "conversion error lost its file: {rendered}"
        );
    }
}
