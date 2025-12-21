//! Inlay Hints handler for Modelica files.
//!
//! Provides inline hints for:
//! - Parameter names in function calls

use std::collections::HashMap;

use lsp_types::{InlayHint, InlayHintKind, InlayHintLabel, InlayHintParams, Position, Uri};

use crate::ir::ast::Expression;
use crate::ir::transform::constants::{BuiltinFunction, get_builtin_functions};
use crate::ir::visitor::{Visitable, Visitor};

use crate::lsp::utils::parse_document;

// =============================================================================
// Inlay Hint Collector Visitor
// =============================================================================

/// Visitor that collects inlay hints from expressions.
struct InlayHintCollector<'a> {
    range: &'a lsp_types::Range,
    builtins: &'a HashMap<&'a str, &'a BuiltinFunction>,
    hints: Vec<InlayHint>,
}

impl<'a> InlayHintCollector<'a> {
    fn new(
        range: &'a lsp_types::Range,
        builtins: &'a HashMap<&'a str, &'a BuiltinFunction>,
    ) -> Self {
        Self {
            range,
            builtins,
            hints: Vec::new(),
        }
    }

    fn into_hints(self) -> Vec<InlayHint> {
        self.hints
    }
}

impl Visitor for InlayHintCollector<'_> {
    fn enter_expression(&mut self, node: &Expression) {
        if let Expression::FunctionCall { comp, args } = node {
            // Get the function name
            let func_name = comp
                .parts
                .first()
                .map(|p| p.ident.text.as_str())
                .unwrap_or("");

            // Skip functions where parameter hints aren't useful
            if SKIP_HINT_FUNCTIONS.contains(&func_name) {
                return;
            }

            // Look up the function in builtins
            let Some(builtin) = self.builtins.get(func_name) else {
                return;
            };

            // Only show hints for functions with multiple parameters
            if builtin.parameters.len() <= 1 {
                return;
            }

            // Add parameter name hints for each argument
            for (i, arg) in args.iter().enumerate() {
                let Some(loc) = arg.get_location() else {
                    continue;
                };
                let line = loc.start_line.saturating_sub(1);

                // Check if in range
                if line < self.range.start.line || line > self.range.end.line {
                    continue;
                }

                // Get parameter name from signature
                if let Some(param_name) = get_param_name_from_signature(builtin.signature, i) {
                    self.hints.push(InlayHint {
                        position: Position {
                            line,
                            character: loc.start_column.saturating_sub(1),
                        },
                        label: InlayHintLabel::String(format!("{}:", param_name)),
                        kind: Some(InlayHintKind::PARAMETER),
                        text_edits: None,
                        tooltip: None,
                        padding_left: Some(false),
                        padding_right: Some(true),
                        data: None,
                    });
                }
            }
        }
    }
}

/// Handle inlay hints request
pub fn handle_inlay_hints(
    documents: &HashMap<Uri, String>,
    params: InlayHintParams,
) -> Option<Vec<InlayHint>> {
    let uri = &params.text_document.uri;
    let text = documents.get(uri)?;
    let path = uri.path().as_str();
    let range = params.range;

    let builtins: HashMap<&str, &BuiltinFunction> = get_builtin_functions()
        .iter()
        .map(|f| (f.name, f))
        .collect();

    let ast = parse_document(text, path)?;

    let mut collector = InlayHintCollector::new(&range, &builtins);
    for class in ast.class_list.values() {
        class.accept(&mut collector);
    }

    Some(collector.into_hints())
}

/// Functions where parameter hints are not useful (single obvious parameter)
const SKIP_HINT_FUNCTIONS: &[&str] = &[
    "der",
    "pre",
    "noEvent",
    "edge",
    "change",
    "initial",
    "terminal",
    "sin",
    "cos",
    "tan",
    "asin",
    "acos",
    "atan",
    "sinh",
    "cosh",
    "tanh",
    "exp",
    "log",
    "log10",
    "sqrt",
    "abs",
    "sign",
    "floor",
    "ceil",
    "sum",
    "product",
    "transpose",
    "ndims",
    "integer",
];

/// Extract parameter name from a function signature string
fn get_param_name_from_signature(signature: &str, index: usize) -> Option<String> {
    // Parse signature like "sin(x)" or "atan2(y, x)" or "smooth(order, expr)"
    // Handle signatures with return types like "pre(x) -> typeof(x)"
    let start = signature.find('(')?;

    // Find the matching closing parenthesis by counting parens
    let after_open = &signature[start + 1..];
    let mut paren_count = 1;
    let mut end_offset = 0;
    for (i, c) in after_open.char_indices() {
        match c {
            '(' => paren_count += 1,
            ')' => {
                paren_count -= 1;
                if paren_count == 0 {
                    end_offset = i;
                    break;
                }
            }
            _ => {}
        }
    }

    if paren_count != 0 {
        return None; // Unbalanced parentheses
    }

    let params_str = &after_open[..end_offset];
    let params: Vec<&str> = params_str.split(',').map(|s| s.trim()).collect();

    params.get(index).map(|p| {
        // Extract just the parameter name (remove type info if present)
        // Handle formats like "x: Real" or "Real x" or just "x"
        if let Some(colon_pos) = p.find(':') {
            // Format: "x: Real" - take the part before the colon
            p[..colon_pos].trim().to_string()
        } else {
            // Format: "Real x" or just "x" - take the last word
            p.split_whitespace()
                .last()
                .unwrap_or(p)
                .trim_end_matches("...")
                .to_string()
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_param_name_simple() {
        assert_eq!(
            get_param_name_from_signature("sin(x)", 0),
            Some("x".to_string())
        );
    }

    #[test]
    fn test_get_param_name_multiple() {
        assert_eq!(
            get_param_name_from_signature("atan2(y, x)", 0),
            Some("y".to_string())
        );
        assert_eq!(
            get_param_name_from_signature("atan2(y, x)", 1),
            Some("x".to_string())
        );
    }

    #[test]
    fn test_get_param_name_with_type() {
        assert_eq!(
            get_param_name_from_signature("smooth(Integer order, Real expr)", 0),
            Some("order".to_string())
        );
        assert_eq!(
            get_param_name_from_signature("smooth(Integer order, Real expr)", 1),
            Some("expr".to_string())
        );
    }

    #[test]
    fn test_get_param_name_with_typeof_return() {
        // pre(x) -> typeof(x) should correctly parse the parameter as "x"
        assert_eq!(
            get_param_name_from_signature("pre(x) -> typeof(x)", 0),
            Some("x".to_string())
        );
        assert_eq!(
            get_param_name_from_signature("noEvent(expr) -> typeof(expr)", 0),
            Some("expr".to_string())
        );
    }

    #[test]
    fn test_get_param_name_colon_format() {
        // Handle "x: Real" format
        assert_eq!(
            get_param_name_from_signature("der(x: Real) -> Real", 0),
            Some("x".to_string())
        );
    }
}
