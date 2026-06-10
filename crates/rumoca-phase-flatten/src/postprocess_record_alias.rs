use super::*;
use rumoca_core::{ExpressionRewriter, StatementRewriter};

pub(super) fn canonicalize_record_alias_expr(
    expr: &mut rumoca_core::Expression,
    ctx: &Context,
    known_variables: &HashSet<String>,
) {
    let mut rewriter = RecordAliasCanonicalizer {
        ctx,
        known_variables,
    };
    *expr = rewriter.rewrite_expression(expr);
}

pub(super) fn canonicalize_record_alias_statements(
    statements: &mut [rumoca_core::Statement],
    ctx: &Context,
    known_variables: &HashSet<String>,
) {
    let mut rewriter = RecordAliasCanonicalizer {
        ctx,
        known_variables,
    };
    for statement in statements {
        *statement = rewriter.rewrite_statement(statement);
    }
}

pub(super) fn canonicalize_record_alias_when_equations(
    equations: &mut [flat::WhenEquation],
    ctx: &Context,
    known_variables: &HashSet<String>,
) {
    for equation in equations {
        match equation {
            flat::WhenEquation::Assign { value, .. } | flat::WhenEquation::Reinit { value, .. } => {
                canonicalize_record_alias_expr(value, ctx, known_variables);
            }
            flat::WhenEquation::Assert {
                condition, message, ..
            } => {
                canonicalize_record_alias_expr(condition, ctx, known_variables);
                canonicalize_record_alias_expr(message, ctx, known_variables);
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                for (condition, branch_equations) in branches {
                    canonicalize_record_alias_expr(condition, ctx, known_variables);
                    canonicalize_record_alias_when_equations(
                        branch_equations,
                        ctx,
                        known_variables,
                    );
                }
                canonicalize_record_alias_when_equations(else_branch, ctx, known_variables);
            }
            flat::WhenEquation::FunctionCallOutputs { function, .. } => {
                canonicalize_record_alias_expr(function, ctx, known_variables);
            }
            flat::WhenEquation::Terminate { message, .. } => {
                canonicalize_record_alias_expr(message, ctx, known_variables);
            }
        }
    }
}

struct RecordAliasCanonicalizer<'a> {
    ctx: &'a Context,
    known_variables: &'a HashSet<String>,
}

impl ExpressionRewriter for RecordAliasCanonicalizer<'_> {
    fn rewrite_var_ref_expression(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        let rewritten_name = if subscripts.is_empty() {
            record_alias_rewrite_name(name.as_str(), self.ctx, self.known_variables)
                .map(rumoca_core::Reference::new)
                .unwrap_or_else(|| name.clone())
        } else {
            name.clone()
        };
        rumoca_core::Expression::VarRef {
            name: rewritten_name,
            subscripts: self.rewrite_subscripts(subscripts),
            span,
        }
    }
}

impl StatementRewriter for RecordAliasCanonicalizer<'_> {}
