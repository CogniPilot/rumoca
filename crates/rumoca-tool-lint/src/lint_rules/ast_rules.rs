//! AST-driven implementations of the built-in lint rules.
//!
//! Every rule walks the parsed tree with [`ast::Visitor`], so comments and
//! string literals are structurally invisible to them.

use std::ops::ControlFlow::{self, Continue};

use rumoca_compile::parsing::{ClassType, TerminalType, Variability, ast};

use crate::lint_context::LintContext;
use crate::lint_rules::{
    LintLevel, LintMessage, LintRule, create_naming_message, is_magic_number, starts_with_lowercase,
};

/// Class kinds whose names must be PascalCase.
fn requires_pascal_case(class_type: &ClassType) -> bool {
    matches!(
        class_type,
        ClassType::Model
            | ClassType::Class
            | ClassType::Block
            | ClassType::Connector
            | ClassType::Record
            | ClassType::Package
    )
}

/// Class kinds that must carry a description string (MLS §4.5).
fn requires_description(class_type: &ClassType) -> bool {
    matches!(class_type, ClassType::Model | ClassType::Function)
}

// ---------------------------------------------------------------------------
// naming-convention
// ---------------------------------------------------------------------------

/// Rule: Check naming conventions.
pub(crate) struct NamingConventionRule;

struct NamingVisitor<'a> {
    file_name: &'a str,
    messages: Vec<LintMessage>,
}

impl ast::Visitor for NamingVisitor<'_> {
    fn enter_class_def(&mut self, class: &ast::ClassDef) -> ControlFlow<()> {
        if requires_pascal_case(&class.class_type) && starts_with_lowercase(&class.name.text) {
            self.messages.push(create_naming_message(
                class.class_type.as_str(),
                &class.name.text,
                self.file_name,
                class.name.location.start_line,
                class.name.location.start_column,
            ));
        }
        Continue(())
    }
}

impl LintRule for NamingConventionRule {
    fn name(&self) -> &'static str {
        "naming-convention"
    }

    fn description(&self) -> &'static str {
        "Check that model and class names follow conventions (PascalCase)"
    }

    fn check(&self, ctx: &LintContext<'_>) -> Vec<LintMessage> {
        let mut visitor = NamingVisitor {
            file_name: ctx.file_name,
            messages: Vec::new(),
        };
        let _ = ast::Visitor::visit_stored_definition(&mut visitor, ctx.ast);
        visitor.messages
    }
}

// ---------------------------------------------------------------------------
// missing-documentation
// ---------------------------------------------------------------------------

/// Rule: Check for missing documentation.
pub(crate) struct MissingDocumentationRule;

struct DocumentationVisitor<'a> {
    file_name: &'a str,
    messages: Vec<LintMessage>,
}

impl ast::Visitor for DocumentationVisitor<'_> {
    fn enter_class_def(&mut self, class: &ast::ClassDef) -> ControlFlow<()> {
        if requires_description(&class.class_type) && class.description.is_empty() {
            self.messages.push(LintMessage::new(
                "missing-documentation",
                LintLevel::Note,
                format!(
                    "{} '{}' is missing a description string",
                    class.class_type.as_str(),
                    class.name.text
                ),
                self.file_name,
                class.name.location.start_line,
                class.name.location.start_column,
            ));
        }
        Continue(())
    }
}

impl LintRule for MissingDocumentationRule {
    fn name(&self) -> &'static str {
        "missing-documentation"
    }

    fn description(&self) -> &'static str {
        "Check that models and functions have documentation strings"
    }

    fn check(&self, ctx: &LintContext<'_>) -> Vec<LintMessage> {
        let mut visitor = DocumentationVisitor {
            file_name: ctx.file_name,
            messages: Vec::new(),
        };
        let _ = ast::Visitor::visit_stored_definition(&mut visitor, ctx.ast);
        visitor.messages
    }
}

// ---------------------------------------------------------------------------
// magic-number
// ---------------------------------------------------------------------------

/// Rule: Check for magic numbers.
pub(crate) struct MagicNumberRule;

/// Expression positions where numeric literals are structural, not magic:
/// annotation graphics coordinates, `start`/attribute modifiers and array
/// dimensions all legitimately spell out raw numbers.
fn is_exempt_expression_context(ctx: ast::ExpressionContext) -> bool {
    matches!(
        ctx,
        ast::ExpressionContext::ClassAnnotation
            | ast::ExpressionContext::ComponentAnnotation
            | ast::ExpressionContext::ExtendAnnotation
            | ast::ExpressionContext::ComponentStart
    )
}

/// Built-in variable attributes (MLS §4.8): a number bound to one of these is a
/// declaration detail, not a magic constant buried in behavior.
const BUILTIN_ATTRIBUTES: &[&str] = &[
    "start",
    "fixed",
    "min",
    "max",
    "nominal",
    "unit",
    "displayUnit",
    "quantity",
    "stateSelect",
];

fn modifies_builtin_attribute(target: &ast::ComponentReference) -> bool {
    target
        .parts
        .last()
        .is_some_and(|part| BUILTIN_ATTRIBUTES.contains(&part.ident.text.as_ref()))
}

struct MagicNumberVisitor<'a> {
    file_name: &'a str,
    messages: Vec<LintMessage>,
    /// True while inside a `parameter`/`constant` declaration, whose whole
    /// purpose is to name a literal.
    inside_named_constant: bool,
}

impl ast::Visitor for MagicNumberVisitor<'_> {
    fn visit_component(&mut self, comp: &ast::Component) -> ControlFlow<()> {
        let outer = self.inside_named_constant;
        self.inside_named_constant = matches!(
            comp.variability,
            Variability::Constant(_) | Variability::Parameter(_)
        );
        let result = ast::walk_component_default(self, comp);
        self.inside_named_constant = outer;
        result
    }

    fn visit_expression_ctx(
        &mut self,
        expr: &ast::Expression,
        ctx: ast::ExpressionContext,
    ) -> ControlFlow<()> {
        if is_exempt_expression_context(ctx) {
            return Continue(());
        }
        self.visit_expression(expr)
    }

    fn visit_subscript_ctx(
        &mut self,
        _subscript: &ast::Subscript,
        _ctx: ast::SubscriptContext,
    ) -> ControlFlow<()> {
        // Array dimensions and indices are structure, not magic numbers.
        Continue(())
    }

    fn visit_for_index(&mut self, idx: &ast::ForIndex) -> ControlFlow<()> {
        // A `for` range (MLS §11.2.2 / §8.3.2) spells out the same structure as
        // an array dimension: the `3` in `for i in 1:3` is the `3` in `x[3]`, so
        // it is exempt for the same reason. Only the range is skipped; literals
        // in the loop body are still reported.
        self.enter_for_index(idx)
    }

    fn visit_expression(&mut self, expr: &ast::Expression) -> ControlFlow<()> {
        if let ast::Expression::Modification { target, .. } = expr
            && modifies_builtin_attribute(target)
        {
            return Continue(());
        }
        ast::walk_expression_default(self, expr)
    }

    fn enter_expression(&mut self, expr: &ast::Expression) -> ControlFlow<()> {
        if self.inside_named_constant {
            return Continue(());
        }
        let ast::Expression::Terminal {
            terminal_type,
            token,
            ..
        } = expr
        else {
            return Continue(());
        };
        if !matches!(
            terminal_type,
            TerminalType::UnsignedReal | TerminalType::UnsignedInteger
        ) {
            return Continue(());
        }
        let Ok(value) = token.text.parse::<f64>() else {
            return Continue(());
        };
        if !is_magic_number(value) {
            return Continue(());
        }
        self.messages.push(LintMessage::new(
            "magic-number",
            LintLevel::Help,
            format!("Consider extracting '{}' as a named constant", token.text),
            self.file_name,
            token.location.start_line,
            token.location.start_column,
        ));
        Continue(())
    }
}

impl LintRule for MagicNumberRule {
    fn name(&self) -> &'static str {
        "magic-number"
    }

    fn description(&self) -> &'static str {
        "Check for literal numbers that should be named constants"
    }

    fn check(&self, ctx: &LintContext<'_>) -> Vec<LintMessage> {
        let mut visitor = MagicNumberVisitor {
            file_name: ctx.file_name,
            messages: Vec::new(),
            inside_named_constant: false,
        };
        let _ = ast::Visitor::visit_stored_definition(&mut visitor, ctx.ast);
        visitor.messages
    }
}

#[cfg(test)]
mod tests;
