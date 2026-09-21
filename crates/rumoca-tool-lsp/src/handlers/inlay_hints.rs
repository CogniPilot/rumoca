//! Inlay hints handler for Modelica files.

use crate::text_position::{byte_offset_to_position, char_column_to_utf16_column, line_text};
use lsp_types::{InlayHint, InlayHintKind, InlayHintLabel, InlayHintTooltip, Position, Range};
use rumoca_compile::parsing::ast;
use std::collections::HashMap;
use std::ops::ControlFlow;

use crate::traversal_adapter;

/// Controls whether parameter-name inlay hints are emitted, and for which
/// callees. Array-dimension type hints are independent of this setting and are
/// always produced.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ParameterNameHintMode {
    /// No parameter-name hints are emitted.
    #[default]
    None,
    /// Hints are emitted only for calls to user-defined functions, using the
    /// callee's declared input parameter names.
    UserFunctions,
    /// Hints are emitted for user-defined functions and for known builtins.
    All,
}

impl ParameterNameHintMode {
    /// Parse the `rumoca.inlayHints.parameterNames` setting value. Any
    /// unrecognized value (including an absent setting) resolves to `None`.
    pub fn from_setting(value: &str) -> Self {
        match value {
            "all" => Self::All,
            "userFunctions" => Self::UserFunctions,
            _ => Self::None,
        }
    }
}

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
/// - Array dimension hints for component declarations (always on).
/// - Parameter name hints for function calls, gated by `mode`.
pub fn handle_inlay_hints(
    ast: &ast::StoredDefinition,
    source: &str,
    range: &Range,
    mode: ParameterNameHintMode,
) -> Vec<InlayHint> {
    let user_functions = if mode == ParameterNameHintMode::None {
        HashMap::new()
    } else {
        let mut map = HashMap::new();
        collect_user_function_params(&ast.classes, &mut map);
        map
    };
    let mut collector = InlayHintCollector::new(range, source, mode, user_functions);
    let _ = traversal_adapter::walk_stored_definition(&mut collector, ast);
    collector.hints
}

/// Index user-defined functions by name in pre-order, mapping each to its
/// declared input parameter names in source order.
fn collect_user_function_params(
    classes: &ast::AstIndexMap<String, ast::ClassDef>,
    out: &mut HashMap<String, Vec<String>>,
) {
    let mut stack: Vec<&ast::ClassDef> = Vec::new();
    for class in classes.values().rev() {
        stack.push(class);
    }
    while let Some(class) = stack.pop() {
        if class.class_type == rumoca_core::ClassType::Function {
            let params: Vec<String> = class
                .components
                .values()
                .filter(|comp| matches!(comp.causality, rumoca_core::Causality::Input(_)))
                .map(|comp| comp.name.clone())
                .collect();
            out.entry(class.name.text.to_string()).or_insert(params);
        }
        for child in class.classes.values().rev() {
            stack.push(child);
        }
    }
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
    mode: ParameterNameHintMode,
    user_functions: HashMap<String, Vec<String>>,
    hints: Vec<InlayHint>,
}

impl<'a> InlayHintCollector<'a> {
    fn new(
        range: &'a Range,
        source: &'a str,
        mode: ParameterNameHintMode,
        user_functions: HashMap<String, Vec<String>>,
    ) -> Self {
        Self {
            range,
            source,
            mode,
            user_functions,
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
        collect_function_call_hints(
            comp,
            args,
            self.range,
            self.source,
            self.mode,
            &self.user_functions,
            &mut self.hints,
        );
        ast::visitor::walk_expr_function_call_ctx_default(self, comp, args, ctx)
    }

    fn visit_expression(&mut self, expression: &ast::Expression) -> ControlFlow<()> {
        traversal_adapter::walk_expression_default(self, expression)
    }
}

/// Resolve the LSP position for a parameter-name hint anchored at the start of
/// the full argument expression, including any leading unary operator.
///
/// `Expression::span` covers the whole expression (the leading `-` of
/// `-0.9*pre(v)` included), unlike `Expression::get_location`, which for a unary
/// expression reports the operand location. Returns `None` when the position
/// falls outside the requested range.
fn arg_hint_position(source: &str, arg: &ast::Expression, range: &Range) -> Option<Position> {
    let start = arg.span().start.0;
    let position = if start > 0 && start <= source.len() {
        byte_offset_to_position(source, start)
    } else {
        let loc = arg.get_location()?;
        let line = loc.start_line.saturating_sub(1);
        position_at(source, line, loc.start, loc.start_column)
    };
    if position.line < range.start.line || position.line > range.end.line {
        return None;
    }
    Some(position)
}

fn collect_function_call_hints(
    comp: &ast::ComponentReference,
    args: &[ast::Expression],
    range: &Range,
    source: &str,
    mode: ParameterNameHintMode,
    user_functions: &HashMap<String, Vec<String>>,
    hints: &mut Vec<InlayHint>,
) {
    if mode == ParameterNameHintMode::None {
        return;
    }
    let Some(function_name) = comp.parts.last().map(|p| p.ident.text.as_ref()) else {
        return;
    };

    // Prefer a user-defined function's declared parameters. Builtins only
    // contribute names in `All` mode.
    let param_names: Vec<&str> = if let Some(user_params) = user_functions.get(function_name) {
        user_params.iter().map(String::as_str).collect()
    } else if mode == ParameterNameHintMode::All {
        builtin_param_names(function_name).to_vec()
    } else {
        return;
    };
    if param_names.is_empty() {
        return;
    }

    // Single-argument calls gain nothing from a parameter-name hint (der(x)
    // adds no information), so suppress them.
    let positional_count = args
        .iter()
        .filter(|arg| !matches!(arg, ast::Expression::NamedArgument { .. }))
        .count();
    if positional_count <= 1 {
        return;
    }

    let mut positional_index = 0usize;
    for arg in args {
        if matches!(arg, ast::Expression::NamedArgument { .. }) {
            continue;
        }
        let Some(param_name) = param_names.get(positional_index) else {
            break;
        };
        positional_index += 1;
        let Some(position) = arg_hint_position(source, arg, range) else {
            continue;
        };
        hints.push(InlayHint {
            position,
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

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_compile::parsing::parse_source_to_ast;

    fn full_range() -> Range {
        Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 100,
                character: 0,
            },
        }
    }

    fn parameter_hints(hints: &[InlayHint]) -> Vec<(&InlayHint, &str)> {
        hints
            .iter()
            .filter(|hint| hint.kind == Some(InlayHintKind::PARAMETER))
            .filter_map(|hint| match &hint.label {
                InlayHintLabel::String(label) => Some((hint, label.as_str())),
                _ => None,
            })
            .collect()
    }

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
        let hints = handle_inlay_hints(&ast, source, &full_range(), ParameterNameHintMode::None);
        assert!(
            hints.iter().any(|h| match &h.label {
                InlayHintLabel::String(s) => s.contains("[2x3]"),
                _ => false,
            }),
            "expected dimension inlay hint, got: {:?}",
            hints
        );
    }

    #[test]
    fn parameter_hints_off_by_default() {
        let source = r#"
model M
  Real y;
  Real v;
equation
  y = atan2(v, 2 * v);
end M;
"#;
        let ast = parse_source_to_ast(source, "input.mo").expect("parse");
        let hints = handle_inlay_hints(&ast, source, &full_range(), ParameterNameHintMode::None);
        assert!(
            parameter_hints(&hints).is_empty(),
            "no parameter-name hints should be emitted in None mode, got: {:?}",
            hints
        );
    }

    #[test]
    fn builtin_parameter_hints_emitted_in_all_mode() {
        let source = r#"
model M
  Real y;
  Real v;
equation
  y = atan2(v, 2 * v);
end M;
"#;
        let ast = parse_source_to_ast(source, "input.mo").expect("parse");
        let hints = handle_inlay_hints(&ast, source, &full_range(), ParameterNameHintMode::All);
        let labels: Vec<&str> = parameter_hints(&hints).iter().map(|(_, l)| *l).collect();
        assert!(
            labels.contains(&"y:") && labels.contains(&"x:"),
            "atan2 should emit its two builtin parameter names in All mode, got: {:?}",
            hints
        );
    }

    #[test]
    fn single_argument_calls_are_suppressed() {
        let source = r#"
model M
  Real v;
equation
  der(v) = sin(v);
end M;
"#;
        let ast = parse_source_to_ast(source, "input.mo").expect("parse");
        let hints = handle_inlay_hints(&ast, source, &full_range(), ParameterNameHintMode::All);
        assert!(
            parameter_hints(&hints).is_empty(),
            "single-argument builtin calls must not emit parameter hints, got: {:?}",
            hints
        );
    }

    #[test]
    fn leading_unary_minus_anchors_hint_before_operator() {
        let source = "model M\n  Real v;\nequation\n  when v > 0 then\n    reinit(v, -0.9*pre(v));\n  end when;\nend M;\n";
        let ast = parse_source_to_ast(source, "input.mo").expect("parse");
        let hints = handle_inlay_hints(&ast, source, &full_range(), ParameterNameHintMode::All);
        let expr_hint = parameter_hints(&hints)
            .into_iter()
            .find(|(_, label)| *label == "expr:")
            .map(|(hint, _)| hint)
            .expect("reinit should emit an `expr:` parameter hint in All mode");

        // Line index 4 (0-based) is `    reinit(v, -0.9*pre(v));`.
        let minus_line = source.lines().nth(4).expect("reinit line");
        let minus_column = minus_line
            .find('-')
            .expect("the reinit line contains a leading minus") as u32;
        assert_eq!(
            expr_hint.position.line, 4,
            "expr hint should land on the reinit line, got: {:?}",
            expr_hint
        );
        assert_eq!(
            expr_hint.position.character, minus_column,
            "expr hint must anchor before the leading `-`, not on the `0`; got char {} (minus at {})",
            expr_hint.position.character, minus_column
        );
    }

    #[test]
    fn user_function_hints_use_declared_parameter_names() {
        let source = r#"
package MyPkg
  function scale
    input Real value;
    input Real factor;
    output Real result;
  algorithm
    result := value * factor;
  end scale;
end MyPkg;

model M
  Real y;
  Real v;
equation
  y = MyPkg.scale(v, 2.0);
end M;
"#;
        let ast = parse_source_to_ast(source, "input.mo").expect("parse");
        let hints = handle_inlay_hints(
            &ast,
            source,
            &full_range(),
            ParameterNameHintMode::UserFunctions,
        );
        let labels: Vec<&str> = parameter_hints(&hints).iter().map(|(_, l)| *l).collect();
        assert!(
            labels.contains(&"value:") && labels.contains(&"factor:"),
            "nested user function call should use declared parameter names, got: {:?}",
            hints
        );
    }

    #[test]
    fn user_functions_mode_skips_builtins() {
        let source = r#"
model M
  Real y;
  Real v;
equation
  y = atan2(v, 2 * v);
end M;
"#;
        let ast = parse_source_to_ast(source, "input.mo").expect("parse");
        let hints = handle_inlay_hints(
            &ast,
            source,
            &full_range(),
            ParameterNameHintMode::UserFunctions,
        );
        assert!(
            parameter_hints(&hints).is_empty(),
            "builtins must not emit hints in UserFunctions mode, got: {:?}",
            hints
        );
    }
}
