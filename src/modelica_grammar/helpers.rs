//! Helper functions for grammar conversion.

use crate::ir;
use crate::modelica_grammar_trait;

/// Helper to format location info from a token for error messages
pub fn loc_info(token: &ir::ast::Token) -> String {
    let loc = &token.location;
    format!(
        " at {}:{}:{}",
        loc.file_name, loc.start_line, loc.start_column
    )
}

/// Create a location spanning from the start of one token to the end of another
pub fn span_location(start: &ir::ast::Token, end: &ir::ast::Token) -> ir::ast::Location {
    ir::ast::Location {
        start_line: start.location.start_line,
        start_column: start.location.start_column,
        end_line: end.location.end_line,
        end_column: end.location.end_column,
        start: start.location.start,
        end: end.location.end,
        file_name: start.location.file_name.clone(),
    }
}

/// Helper to format location info from an expression
pub fn expr_loc_info(expr: &ir::ast::Expression) -> String {
    expr.get_location()
        .map(|loc| {
            format!(
                " at {}:{}:{}",
                loc.file_name, loc.start_line, loc.start_column
            )
        })
        .unwrap_or_default()
}

/// Helper to collect elements from array_arguments into a Vec<Expression>
/// Handles both simple arrays like {1, 2, 3} and nested arrays like {{1, 2}, {3, 4}}
pub fn collect_array_elements(
    args: &modelica_grammar_trait::ArrayArguments,
) -> anyhow::Result<Vec<ir::ast::Expression>> {
    let mut elements = Vec::new();

    // First element
    elements.push((*args.expression).clone());

    // Collect remaining elements from the optional chain
    if let Some(opt) = &args.array_arguments_opt {
        match &opt.array_arguments_opt_group {
            modelica_grammar_trait::ArrayArgumentsOptGroup::CommaArrayArgumentsNonFirst(
                comma_args,
            ) => {
                collect_array_non_first(&comma_args.array_arguments_non_first, &mut elements);
            }
            modelica_grammar_trait::ArrayArgumentsOptGroup::ForForIndices(_for_indices) => {
                // Array comprehension like {i for i in 1:10} - not yet supported
                anyhow::bail!(
                    "Array comprehension with 'for' is not yet supported{}",
                    expr_loc_info(&args.expression)
                );
            }
        }
    }

    Ok(elements)
}

/// Helper to recursively collect elements from array_arguments_non_first chain
pub fn collect_array_non_first(
    args: &modelica_grammar_trait::ArrayArgumentsNonFirst,
    elements: &mut Vec<ir::ast::Expression>,
) {
    // Add current element
    elements.push(args.expression.clone());

    // Recursively collect remaining elements
    if let Some(opt) = &args.array_arguments_non_first_opt {
        collect_array_non_first(&opt.array_arguments_non_first, elements);
    }
}
