use super::*;
use rumoca_core::{ExpressionRewriter, StatementRewriter};

pub(super) fn canonicalize_record_alias_expr(
    expr: &mut rumoca_core::Expression,
    targets: &RecordAliasTargets<'_>,
) {
    let mut rewriter = RecordAliasCanonicalizer { targets };
    *expr = rewriter.rewrite_expression(expr);
}

pub(super) fn canonicalize_record_alias_statements(
    statements: &mut [rumoca_core::Statement],
    targets: &RecordAliasTargets<'_>,
) {
    let mut rewriter = RecordAliasCanonicalizer { targets };
    for statement in statements {
        *statement = rewriter.rewrite_statement(statement);
    }
}

pub(super) fn canonicalize_record_alias_when_equations(
    equations: &mut [flat::WhenEquation],
    targets: &RecordAliasTargets<'_>,
) {
    for equation in equations {
        match equation {
            flat::WhenEquation::Assign { value, .. } | flat::WhenEquation::Reinit { value, .. } => {
                canonicalize_record_alias_expr(value, targets);
            }
            flat::WhenEquation::Assert {
                condition,
                message,
                level,
                ..
            } => {
                canonicalize_record_alias_expr(condition, targets);
                canonicalize_record_alias_expr(message, targets);
                if let Some(level) = level {
                    canonicalize_record_alias_expr(level, targets);
                }
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                for (condition, branch_equations) in branches {
                    canonicalize_record_alias_expr(condition, targets);
                    canonicalize_record_alias_when_equations(branch_equations, targets);
                }
                if let Some(else_branch) = else_branch {
                    canonicalize_record_alias_when_equations(else_branch, targets);
                }
            }
            flat::WhenEquation::FunctionCallOutputs { function, .. } => {
                canonicalize_record_alias_expr(function, targets);
            }
            flat::WhenEquation::Terminate { message, .. } => {
                canonicalize_record_alias_expr(message, targets);
            }
        }
    }
}

struct RecordAliasCanonicalizer<'a> {
    targets: &'a RecordAliasTargets<'a>,
}

impl ExpressionRewriter for RecordAliasCanonicalizer<'_> {
    /// A read through a record alias becomes a read of the aliased Flat
    /// variable itself, carrying that variable's exact identity: the alias
    /// source and its target are distinct occurrences, so the original read's
    /// identity cannot be kept, and a bare spelling would carry none.
    fn rewrite_var_ref_expression(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        let rewritten_name = if subscripts.is_empty() {
            self.targets
                .canonical_reference(name.as_str())
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
