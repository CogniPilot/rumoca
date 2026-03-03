//! Inlay hints handler for Modelica files.

use lsp_types::{InlayHint, InlayHintKind, InlayHintLabel, InlayHintTooltip, Position, Range};
use rumoca_ir_ast as ast;

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
    let mut hints = Vec::new();
    for class in ast.classes.values() {
        collect_class_hints(class, source, range, &mut hints);
    }
    hints
}

fn collect_class_hints(
    class: &ast::ClassDef,
    source: &str,
    range: &Range,
    hints: &mut Vec<InlayHint>,
) {
    for comp in class.components.values() {
        if let Some(hint) = component_dimension_hint(comp, range) {
            hints.push(hint);
        }
    }

    for eq in &class.equations {
        collect_equation_hints(eq, range, hints);
    }
    for eq in &class.initial_equations {
        collect_equation_hints(eq, range, hints);
    }
    for section in &class.algorithms {
        for stmt in section {
            collect_statement_hints(stmt, range, hints);
        }
    }
    for section in &class.initial_algorithms {
        for stmt in section {
            collect_statement_hints(stmt, range, hints);
        }
    }

    // Also scan raw source line for direct builtin calls not represented in AST sections.
    // This keeps hints useful even for partially parsed files during editing.
    collect_loose_builtin_call_hints(source, range, hints);

    for nested in class.classes.values() {
        collect_class_hints(nested, source, range, hints);
    }
}

fn component_dimension_hint(comp: &ast::Component, range: &Range) -> Option<InlayHint> {
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
        position: Position {
            line,
            character: comp.name_token.location.end_column.saturating_sub(1),
        },
        label: InlayHintLabel::String(format!(" [{}]", dims)),
        kind: Some(InlayHintKind::TYPE),
        text_edits: None,
        tooltip: Some(InlayHintTooltip::String("Array dimensions".to_string())),
        padding_left: Some(true),
        padding_right: Some(false),
        data: None,
    })
}

fn collect_equation_hints(eq: &ast::Equation, range: &Range, hints: &mut Vec<InlayHint>) {
    match eq {
        ast::Equation::Simple { lhs, rhs } => {
            collect_expression_hints(lhs, range, hints);
            collect_expression_hints(rhs, range, hints);
        }
        ast::Equation::Connect { .. } => {}
        ast::Equation::For {
            indices: _,
            equations,
        } => {
            for inner in equations {
                collect_equation_hints(inner, range, hints);
            }
        }
        ast::Equation::When(blocks) => {
            for block in blocks {
                collect_expression_hints(&block.cond, range, hints);
                for inner in &block.eqs {
                    collect_equation_hints(inner, range, hints);
                }
            }
        }
        ast::Equation::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                collect_expression_hints(&block.cond, range, hints);
                for inner in &block.eqs {
                    collect_equation_hints(inner, range, hints);
                }
            }
            if let Some(else_eqs) = else_block {
                for inner in else_eqs {
                    collect_equation_hints(inner, range, hints);
                }
            }
        }
        ast::Equation::FunctionCall { comp, args } => {
            collect_function_call_hints(comp, args, range, hints);
            for arg in args {
                collect_expression_hints(arg, range, hints);
            }
        }
        ast::Equation::Assert {
            condition,
            message,
            level,
        } => {
            collect_expression_hints(condition, range, hints);
            collect_expression_hints(message, range, hints);
            if let Some(level) = level {
                collect_expression_hints(level, range, hints);
            }
        }
        ast::Equation::Empty => {}
    }
}

fn collect_statement_hints(stmt: &ast::Statement, range: &Range, hints: &mut Vec<InlayHint>) {
    match stmt {
        ast::Statement::Assignment { comp: _, value } => {
            collect_expression_hints(value, range, hints);
        }
        ast::Statement::For {
            indices: _,
            equations,
        } => {
            for inner in equations {
                collect_statement_hints(inner, range, hints);
            }
        }
        ast::Statement::While(block) => {
            collect_expression_hints(&block.cond, range, hints);
            for inner in &block.stmts {
                collect_statement_hints(inner, range, hints);
            }
        }
        ast::Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                collect_expression_hints(&block.cond, range, hints);
                for inner in &block.stmts {
                    collect_statement_hints(inner, range, hints);
                }
            }
            if let Some(else_stmts) = else_block {
                for inner in else_stmts {
                    collect_statement_hints(inner, range, hints);
                }
            }
        }
        ast::Statement::When(blocks) => {
            for block in blocks {
                collect_expression_hints(&block.cond, range, hints);
                for inner in &block.stmts {
                    collect_statement_hints(inner, range, hints);
                }
            }
        }
        ast::Statement::FunctionCall {
            comp,
            args,
            outputs,
        } => {
            collect_function_call_hints(comp, args, range, hints);
            for arg in args {
                collect_expression_hints(arg, range, hints);
            }
            for out in outputs {
                collect_expression_hints(out, range, hints);
            }
        }
        ast::Statement::Reinit { variable: _, value } => {
            collect_expression_hints(value, range, hints);
        }
        ast::Statement::Assert {
            condition,
            message,
            level,
        } => {
            collect_expression_hints(condition, range, hints);
            collect_expression_hints(message, range, hints);
            if let Some(level) = level {
                collect_expression_hints(level, range, hints);
            }
        }
        ast::Statement::Return { .. } | ast::Statement::Break { .. } | ast::Statement::Empty => {}
    }
}

fn collect_expression_hints(expr: &ast::Expression, range: &Range, hints: &mut Vec<InlayHint>) {
    match expr {
        ast::Expression::Unary { rhs, .. } => collect_expression_hints(rhs, range, hints),
        ast::Expression::Binary { lhs, rhs, .. } => {
            collect_expression_hints(lhs, range, hints);
            collect_expression_hints(rhs, range, hints);
        }
        ast::Expression::FunctionCall { comp, args } => {
            collect_function_call_hints(comp, args, range, hints);
            for arg in args {
                collect_expression_hints(arg, range, hints);
            }
        }
        ast::Expression::ClassModification {
            target: _,
            modifications,
        } => {
            for m in modifications {
                collect_expression_hints(m, range, hints);
            }
        }
        ast::Expression::NamedArgument { value, .. } => {
            collect_expression_hints(value, range, hints)
        }
        ast::Expression::Modification { value, .. } => {
            collect_expression_hints(value, range, hints)
        }
        ast::Expression::Array { elements, .. } => {
            for el in elements {
                collect_expression_hints(el, range, hints);
            }
        }
        ast::Expression::Tuple { elements } => {
            for el in elements {
                collect_expression_hints(el, range, hints);
            }
        }
        ast::Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, value) in branches {
                collect_expression_hints(cond, range, hints);
                collect_expression_hints(value, range, hints);
            }
            collect_expression_hints(else_branch, range, hints);
        }
        ast::Expression::Parenthesized { inner } => collect_expression_hints(inner, range, hints),
        ast::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            collect_expression_hints(expr, range, hints);
            for idx in indices {
                collect_expression_hints(&idx.range, range, hints);
            }
            if let Some(filter) = filter {
                collect_expression_hints(filter, range, hints);
            }
        }
        ast::Expression::ArrayIndex { base, subscripts } => {
            collect_expression_hints(base, range, hints);
            for s in subscripts {
                if let ast::Subscript::Expression(e) = s {
                    collect_expression_hints(e, range, hints);
                }
            }
        }
        ast::Expression::FieldAccess { base, field: _ } => {
            collect_expression_hints(base, range, hints);
        }
        ast::Expression::Range { start, step, end } => {
            collect_expression_hints(start, range, hints);
            if let Some(step) = step {
                collect_expression_hints(step, range, hints);
            }
            collect_expression_hints(end, range, hints);
        }
        ast::Expression::Terminal { .. }
        | ast::Expression::ComponentReference(_)
        | ast::Expression::Empty => {}
    }
}

fn collect_function_call_hints(
    comp: &ast::ComponentReference,
    args: &[ast::Expression],
    range: &Range,
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
            position: Position {
                line,
                character: loc.start_column.saturating_sub(1),
            },
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
                        character: abs as u32,
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

    #[test]
    fn provides_array_dimension_inlay_hint() {
        let source = r#"
model M
  Real x[2,3];
equation
  der(x[1,1]) = 0;
end M;
"#;
        let ast = rumoca_phase_parse::parse_to_ast(source, "input.mo").expect("parse");
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
