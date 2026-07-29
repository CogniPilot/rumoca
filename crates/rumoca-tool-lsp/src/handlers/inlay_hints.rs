//! Inlay hints handler for Modelica files.

use crate::text_position::{byte_offset_to_position, char_column_to_utf16_column, line_text};
use lsp_types::{InlayHint, InlayHintKind, InlayHintLabel, InlayHintTooltip, Position, Range};
use rumoca_compile::parsing::ast;
use std::ops::ControlFlow;

use crate::traversal_adapter;

/// Resolve a location's LSP position from its byte offset, falling back to the
/// lexer's 1-based character column when the location carries no byte span.
/// Both branches yield UTF-16 columns, never raw character or byte counts.
fn position_at(source: &str, line: u32, byte_offset: u32, char_column_1based: u32) -> Position {
    if byte_offset > 0 && (byte_offset as usize) <= source.len() {
        return byte_offset_to_position(source, byte_offset as usize);
    }
    let character = match line_text(source, line) {
        Some(text) => char_column_to_utf16_column(text, char_column_1based),
        None => char_column_1based.saturating_sub(1),
    };
    Position { line, character }
}

/// Handle inlay hints request.
///
/// Provides:
/// - Array dimension hints for component declarations.
/// - Parameter name hints for common builtin function calls.
pub fn handle_inlay_hints(
    ast: &ast::StoredDefinition,
    source: &str,
    range: &Range,
) -> Vec<InlayHint> {
    let mut collector = InlayHintCollector::new(range, source);
    // Also scan raw source lines for direct builtin calls not represented in AST sections.
    // This keeps hints useful even for partially parsed files during editing.
    collect_loose_builtin_call_hints(source, range, &mut collector.hints);
    let _ = traversal_adapter::walk_stored_definition(&mut collector, ast);
    collector.hints
}

fn component_dimension_hint(
    comp: &ast::Component,
    range: &Range,
    source: &str,
) -> Option<InlayHint> {
    let line = comp.name_token.location.end_line.saturating_sub(1);
    if line < range.start.line || line > range.end.line {
        return None;
    }

    let dims = if !comp.shape.is_empty() {
        comp.shape
            .iter()
            .map(|d| d.to_string())
            .collect::<Vec<_>>()
            .join("x")
    } else if !comp.shape_expr.is_empty() {
        comp.shape_expr
            .iter()
            .map(|s| match s {
                ast::Subscript::Expression(expr) => expr.to_string(),
                ast::Subscript::Range { .. } => ":".to_string(),
                ast::Subscript::Empty => "?".to_string(),
            })
            .collect::<Vec<_>>()
            .join("x")
    } else {
        return None;
    };

    Some(InlayHint {
        position: position_at(
            source,
            line,
            comp.name_token.location.end,
            comp.name_token.location.end_column,
        ),
        label: InlayHintLabel::String(format!(" [{}]", dims)),
        kind: Some(InlayHintKind::TYPE),
        text_edits: None,
        tooltip: Some(InlayHintTooltip::String("Array dimensions".to_string())),
        padding_left: Some(true),
        padding_right: Some(false),
        data: None,
    })
}

struct InlayHintCollector<'a> {
    range: &'a Range,
    source: &'a str,
    hints: Vec<InlayHint>,
}

impl<'a> InlayHintCollector<'a> {
    fn new(range: &'a Range, source: &'a str) -> Self {
        Self {
            range,
            source,
            hints: Vec::new(),
        }
    }
}

impl ast::visitor::Visitor for InlayHintCollector<'_> {
    fn visit_class_def(&mut self, class: &ast::ClassDef) -> ControlFlow<()> {
        traversal_adapter::walk_class_sections(self, class, false)
    }

    fn visit_component(&mut self, component: &ast::Component) -> ControlFlow<()> {
        if let Some(hint) = component_dimension_hint(component, self.range, self.source) {
            self.hints.push(hint);
        }
        traversal_adapter::walk_component_fields(self, component)
    }

    fn visit_expr_function_call_ctx(
        &mut self,
        comp: &ast::ComponentReference,
        args: &[ast::Expression],
        ctx: ast::visitor::FunctionCallContext,
    ) -> ControlFlow<()> {
        collect_function_call_hints(comp, args, self.range, self.source, &mut self.hints);
        ast::visitor::walk_expr_function_call_ctx_default(self, comp, args, ctx)
    }

    fn visit_expression(&mut self, expression: &ast::Expression) -> ControlFlow<()> {
        traversal_adapter::walk_expression_default(self, expression)
    }
}

fn collect_function_call_hints(
    comp: &ast::ComponentReference,
    args: &[ast::Expression],
    range: &Range,
    source: &str,
    hints: &mut Vec<InlayHint>,
) {
    let Some(function_name) = comp.parts.last().map(|p| p.ident.text.as_ref()) else {
        return;
    };
    let param_names = builtin_param_names(function_name);
    if param_names.is_empty() {
        return;
    }

    for (idx, arg) in args.iter().enumerate() {
        if matches!(arg, ast::Expression::NamedArgument { .. }) {
            continue;
        }
        let Some(param_name) = param_names.get(idx) else {
            break;
        };
        let Some(loc) = arg.get_location() else {
            continue;
        };
        let line = loc.start_line.saturating_sub(1);
        if line < range.start.line || line > range.end.line {
            continue;
        }
        hints.push(InlayHint {
            position: position_at(source, line, loc.start, loc.start_column),
            label: InlayHintLabel::String(format!("{param_name}:")),
            kind: Some(InlayHintKind::PARAMETER),
            text_edits: None,
            tooltip: Some(InlayHintTooltip::String(format!(
                "Parameter `{param_name}` of `{function_name}`"
            ))),
            padding_left: Some(false),
            padding_right: Some(true),
            data: None,
        });
    }
}

fn builtin_param_names(name: &str) -> &'static [&'static str] {
    match name {
        "der" => &["x"],
        "abs" => &["v"],
        "sin" | "cos" | "tan" | "asin" | "acos" | "atan" | "exp" | "log" | "log10" => &["u"],
        "atan2" => &["y", "x"],
        "min" | "max" | "mod" | "rem" | "div" => &["x", "y"],
        "size" => &["A", "i"],
        "sample" => &["start", "interval"],
        "delay" => &["expr", "delayTime", "delayMax"],
        "reinit" => &["x", "expr"],
        "assert" => &["condition", "message", "level"],
        "connect" => &["a", "b"],
        "fill" => &["s", "n1"],
        _ => &[],
    }
}

fn collect_loose_builtin_call_hints(source: &str, range: &Range, hints: &mut Vec<InlayHint>) {
    for (line_idx, line) in source.lines().enumerate() {
        let line_u32 = line_idx as u32;
        if line_u32 < range.start.line || line_u32 > range.end.line {
            continue;
        }
        for func in ["der(", "reinit(", "assert(", "connect("] {
            let mut search_from = 0usize;
            while let Some(pos) = line[search_from..].find(func) {
                let abs = search_from + pos + func.len();
                hints.push(InlayHint {
                    position: Position {
                        line: line_u32,
                        // `abs` is a byte column within the line; LSP needs the
                        // UTF-16 column, which differs on any non-ASCII line.
                        character: byte_offset_to_position(line, abs).character,
                    },
                    label: InlayHintLabel::String("...".to_string()),
                    kind: Some(InlayHintKind::PARAMETER),
                    text_edits: None,
                    tooltip: Some(InlayHintTooltip::String(
                        "Builtin call parameters".to_string(),
                    )),
                    padding_left: Some(false),
                    padding_right: Some(false),
                    data: None,
                });
                search_from = abs;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_compile::parsing::parse_source_to_ast;

    #[test]
    fn provides_array_dimension_inlay_hint() {
        let source = r#"
model M
  Real x[2,3];
equation
  der(x[1,1]) = 0;
end M;
"#;
        let ast = parse_source_to_ast(source, "input.mo").expect("parse");
        let range = Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 20,
                character: 0,
            },
        };
        let hints = handle_inlay_hints(&ast, source, &range);
        assert!(
            hints.iter().any(|h| match &h.label {
                InlayHintLabel::String(s) => s.contains("[2x3]"),
                _ => false,
            }),
            "expected dimension inlay hint, got: {:?}",
            hints
        );
    }
}
