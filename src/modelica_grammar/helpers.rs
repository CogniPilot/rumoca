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

/// Helper to collect elements from array_arguments into an Expression
/// Handles both simple arrays like {1, 2, 3} and array comprehensions like {i for i in 1:10}
pub fn collect_array_elements(
    args: &modelica_grammar_trait::ArrayArguments,
) -> anyhow::Result<ir::ast::Expression> {
    // Check if this is an array comprehension or a simple array
    if let Some(opt) = &args.array_arguments_opt {
        match &opt.array_arguments_opt_group {
            modelica_grammar_trait::ArrayArgumentsOptGroup::CommaArrayArgumentsNonFirst(
                comma_args,
            ) => {
                // Simple array: collect all elements
                let mut elements = vec![args.expression.clone()];
                collect_array_non_first(&comma_args.array_arguments_non_first, &mut elements);
                Ok(ir::ast::Expression::Array {
                    elements,
                    is_matrix: false,
                })
            }
            modelica_grammar_trait::ArrayArgumentsOptGroup::ForForIndices(for_indices) => {
                // Array comprehension: {expr for i in range, j in range2, ...}
                let indices = convert_for_indices(&for_indices.for_indices);
                Ok(ir::ast::Expression::ArrayComprehension {
                    expr: Box::new(args.expression.clone()),
                    indices,
                })
            }
        }
    } else {
        // Single element array
        Ok(ir::ast::Expression::Array {
            elements: vec![args.expression.clone()],
            is_matrix: false,
        })
    }
}

/// Convert grammar ForIndices to AST ForIndex vec
pub fn convert_for_indices(indices: &modelica_grammar_trait::ForIndices) -> Vec<ir::ast::ForIndex> {
    let mut result = Vec::new();

    // First index
    result.push(convert_for_index(&indices.for_index));

    // Additional indices
    for item in &indices.for_indices_list {
        result.push(convert_for_index(&item.for_index));
    }

    result
}

/// Convert a single grammar ForIndex to AST ForIndex
fn convert_for_index(index: &modelica_grammar_trait::ForIndex) -> ir::ast::ForIndex {
    let range = index
        .for_index_opt
        .as_ref()
        .map(|opt| opt.expression.clone())
        .unwrap_or(ir::ast::Expression::Empty);

    ir::ast::ForIndex {
        ident: index.ident.clone(),
        range,
    }
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
