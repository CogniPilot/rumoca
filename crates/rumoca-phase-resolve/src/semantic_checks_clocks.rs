use super::*;
use rumoca_ir_ast::ExpressionContext;
use std::ops::ControlFlow::Continue;

pub(super) const ER063_CLOCK_INVALID_PREFIX: &str = "ER063";
pub(super) const ER069_DER_ON_CLOCK_OPERATOR: &str = "ER069";

pub(super) fn check_clock_restrictions(
    class: &ClassDef,
    def: &StoredDefinition,
    diags: &mut Vec<Diagnostic>,
) {
    for (name, comp) in &class.components {
        let Some(ResolvedTypeRoot::Builtin("Clock")) = resolve_component_type_root(comp, def)
        else {
            continue;
        };

        match &comp.connection {
            Connection::Flow(token) => diags.push(clock_prefix_error(
                name,
                "flow",
                token,
                "check_clock_restrictions/flow_prefix",
            )),
            Connection::Stream(token) => diags.push(clock_prefix_error(
                name,
                "stream",
                token,
                "check_clock_restrictions/stream_prefix",
            )),
            Connection::Empty => {}
        }

        match &comp.variability {
            Variability::Discrete(token) => diags.push(clock_prefix_error(
                name,
                "discrete",
                token,
                "check_clock_restrictions/discrete_prefix",
            )),
            Variability::Parameter(token) => diags.push(clock_prefix_error(
                name,
                "parameter",
                token,
                "check_clock_restrictions/parameter_prefix",
            )),
            Variability::Constant(token) => diags.push(clock_prefix_error(
                name,
                "constant",
                token,
                "check_clock_restrictions/constant_prefix",
            )),
            Variability::Empty => {}
        }
    }
}

fn clock_prefix_error(
    component_name: &str,
    prefix: &str,
    token: &Token,
    context: &str,
) -> Diagnostic {
    semantic_error(
        ER063_CLOCK_INVALID_PREFIX,
        format!(
            "Clock variable '{}' cannot have '{}' prefix (MLS §16.3)",
            component_name, prefix
        ),
        label_from_token(
            token,
            context,
            format!("invalid '{}' prefix on Clock variable", prefix),
        ),
    )
}

pub(super) fn run_clock_expression_semantic_checks(def: &StoredDefinition) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    let mut visitor = ClockExpressionVisitor { diags: &mut diags };
    let _ = visitor.visit_stored_definition(def);
    diags
}

struct ClockExpressionVisitor<'a> {
    diags: &'a mut Vec<Diagnostic>,
}

impl ast::Visitor for ClockExpressionVisitor<'_> {
    fn visit_expression_ctx(
        &mut self,
        expr: &Expression,
        ctx: ExpressionContext,
    ) -> std::ops::ControlFlow<()> {
        if matches!(ctx, ExpressionContext::ComponentAnnotation) {
            return Continue(());
        }
        self.visit_expression(expr)
    }

    fn visit_expr_function_call_ctx(
        &mut self,
        comp: &ComponentReference,
        args: &[Expression],
        ctx: ast::FunctionCallContext,
    ) -> std::ops::ControlFlow<()> {
        if matches!(ctx, ast::FunctionCallContext::Expression)
            && let Some("der") = builtin_name(comp)
            && let Some(Expression::FunctionCall {
                comp: inner_comp, ..
            }) = args.first()
            && let Some(inner_name) = builtin_name(inner_comp)
            && matches!(
                inner_name,
                "sample" | "subSample" | "superSample" | "shiftSample" | "backSample" | "noClock"
            )
            && let Some(inner_token) = inner_comp.parts.first().map(|part| &part.ident)
        {
            self.diags.push(semantic_error(
                ER069_DER_ON_CLOCK_OPERATOR,
                format!(
                    "der() cannot be applied to {}() expressions (MLS §16.5.2)",
                    inner_name
                ),
                label_from_token(
                    inner_token,
                    "run_clock_expression_semantic_checks/der_on_clock_operator",
                    format!("{inner_name}() cannot appear under der()"),
                ),
            ));
        }

        ast::visitor::walk_expr_function_call_ctx_default(self, comp, args, ctx)
    }
}
