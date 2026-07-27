use indexmap::{IndexMap, IndexSet};
use rumoca_core::{Expression, ExpressionRewriter, FallibleExpressionRewriter, Reference, VarName};
use rumoca_ir_dae::DaeExpressionRewriter;

use super::{
    Dae, StructuralError, Substitution, apply_substitutions_to_expressions_in_order,
    is_trivial_alias,
};

/// A substitution set factored into exact named causal expressions.
#[derive(Debug, Clone)]
pub struct CausalSubstitutionPlan {
    /// Substitutions with shared expression trees replaced by retained targets.
    pub substitutions: Vec<Substitution>,
    /// Canonical computational targets restored as causal algebraic assignments.
    pub retained_targets: IndexSet<VarName>,
}

struct Candidate<'a> {
    substitution_index: usize,
    substitution: &'a Substitution,
    fingerprint: u64,
    normalized_expr: Expression,
    normalized_fingerprint: u64,
}

/// Recover a causal DAG from the exact expression copies introduced by
/// symbolic elimination.
///
/// Every distinct nontrivial eliminated definition is retained. Equal
/// definitions share the first target in substitution order, while aliases and
/// literals remain collapsed. Edges between retained definitions replace
/// strict subexpressions, so the resulting graph is acyclic without a heuristic
/// expression-size threshold.
pub fn factor_causal_substitutions(
    dae: &Dae,
    substitutions: &[Substitution],
) -> Result<CausalSubstitutionPlan, StructuralError> {
    let candidates = unique_computational_candidates(substitutions);
    let retained_candidates = candidates.iter().collect::<Vec<_>>();
    let retained_targets = retained_candidates
        .iter()
        .map(|candidate| candidate.substitution.var_name.clone())
        .collect::<IndexSet<_>>();
    let collapsed_substitutions = substitutions
        .iter()
        .filter(|substitution| !retained_targets.contains(&substitution.var_name))
        .cloned()
        .collect::<Vec<_>>();
    let mut factored = substitutions
        .iter()
        .enumerate()
        .map(|(index, substitution)| factor_substitution(index, substitution, &retained_candidates))
        .collect::<Result<Vec<_>, _>>()?;
    let mut expressions = factored
        .iter()
        .map(|substitution| substitution.expr.clone())
        .collect::<Vec<_>>();
    apply_substitutions_to_expressions_in_order(dae, &mut expressions, &collapsed_substitutions)?;
    for (substitution, expression) in factored.iter_mut().zip(expressions) {
        substitution.expr = expression;
    }
    Ok(CausalSubstitutionPlan {
        substitutions: factored,
        retained_targets,
    })
}

/// Replace expanded copies of retained computations throughout an already
/// eliminated DAE with references to their restored causal owners.
///
/// Elimination mutates the surviving residuals before returning its
/// substitution inventory. Refactoring only the substitution expressions
/// would therefore restore named variables without making the surviving
/// equations consume them, retaining both expression blow-up and its numeric
/// cancellation. This pass reconnects every DAE partition to the causal DAG
/// before the owning assignments are restored.
pub fn factor_retained_computations_in_dae(
    dae: &mut Dae,
    substitutions: &[Substitution],
    retained_targets: &IndexSet<VarName>,
) -> Result<(), StructuralError> {
    let candidates = unique_computational_candidates(substitutions)
        .into_iter()
        .filter(|candidate| retained_targets.contains(&candidate.substitution.var_name))
        .collect::<Vec<_>>();
    let mut rewriter = RetainedComputationRewriter {
        candidates: &candidates,
        error: None,
    };
    rewriter.rewrite_dae(dae);
    match rewriter.error {
        Some(error) => Err(error),
        None => Ok(()),
    }
}

struct RetainedComputationRewriter<'a> {
    candidates: &'a [Candidate<'a>],
    error: Option<StructuralError>,
}

impl ExpressionRewriter for RetainedComputationRewriter<'_> {
    fn rewrite_expression(&mut self, expr: &Expression) -> Expression {
        if self.error.is_some() {
            return expr.clone();
        }
        let fingerprint = rumoca_core::expression_semantic_fingerprint(expr);
        if let Some(candidate) = self
            .candidates
            .iter()
            .find(|candidate| candidate_matches(candidate, expr, fingerprint))
        {
            return match target_reference_expression(candidate.substitution, expr) {
                Ok(reference) => reference,
                Err(error) => {
                    self.error = Some(error);
                    expr.clone()
                }
            };
        }
        self.walk_expression(expr)
    }
}

impl DaeExpressionRewriter for RetainedComputationRewriter<'_> {}

fn unique_computational_candidates(substitutions: &[Substitution]) -> Vec<Candidate<'_>> {
    let mut candidates: Vec<Candidate<'_>> = Vec::new();
    let mut buckets = IndexMap::<u64, Vec<usize>>::new();
    for (substitution_index, substitution) in substitutions.iter().enumerate() {
        if !expression_owns_computation(&substitution.expr) {
            continue;
        }
        let fingerprint = rumoca_core::expression_semantic_fingerprint(&substitution.expr);
        let duplicate = buckets.get(&fingerprint).is_some_and(|indices| {
            indices.iter().any(|index| {
                rumoca_core::expressions_semantically_equal(
                    &candidates[*index].substitution.expr,
                    &substitution.expr,
                )
            })
        });
        if duplicate {
            continue;
        }
        let candidate_index = candidates.len();
        let normalized_expr = super::simplify_arithmetic_identities(substitution.expr.clone());
        let normalized_fingerprint = rumoca_core::expression_semantic_fingerprint(&normalized_expr);
        candidates.push(Candidate {
            substitution_index,
            substitution,
            fingerprint,
            normalized_expr,
            normalized_fingerprint,
        });
        buckets
            .entry(fingerprint)
            .or_default()
            .push(candidate_index);
    }
    candidates
}

fn candidate_matches(candidate: &Candidate<'_>, expr: &Expression, fingerprint: u64) -> bool {
    (candidate.fingerprint == fingerprint
        && rumoca_core::expressions_semantically_equal(expr, &candidate.substitution.expr))
        || (candidate.normalized_fingerprint == fingerprint
            && rumoca_core::expressions_semantically_equal(expr, &candidate.normalized_expr))
}

fn expression_owns_computation(expr: &Expression) -> bool {
    !is_trivial_alias(expr)
        && !matches!(expr, Expression::Literal { .. } | Expression::Empty { .. })
}

fn factor_substitution(
    substitution_index: usize,
    substitution: &Substitution,
    retained_candidates: &[&Candidate<'_>],
) -> Result<Substitution, StructuralError> {
    let mut factored = substitution.clone();
    factored.expr = CausalFactorRewriter {
        substitution_index,
        retained_candidates,
    }
    .rewrite_expression(&substitution.expr)?;
    Ok(factored)
}

struct CausalFactorRewriter<'a> {
    substitution_index: usize,
    retained_candidates: &'a [&'a Candidate<'a>],
}

impl FallibleExpressionRewriter for CausalFactorRewriter<'_> {
    type Error = StructuralError;

    fn rewrite_expression(&mut self, expr: &Expression) -> Result<Expression, Self::Error> {
        let fingerprint = rumoca_core::expression_semantic_fingerprint(expr);
        if let Some(candidate) = self.retained_candidates.iter().find(|candidate| {
            candidate.substitution_index != self.substitution_index
                && candidate_matches(candidate, expr, fingerprint)
        }) {
            return target_reference_expression(candidate.substitution, expr);
        }
        self.walk_expression(expr)
    }
}

fn target_reference_expression(
    substitution: &Substitution,
    matched: &Expression,
) -> Result<Expression, StructuralError> {
    let span = matched
        .span()
        .or_else(|| substitution.expr.span())
        .ok_or_else(|| StructuralError::UnspannedContractViolation {
            reason: format!(
                "causal factor target `{}` has no source provenance",
                substitution.var_name
            ),
        })?;
    Ok(Expression::VarRef {
        name: substitution
            .var_ref
            .clone()
            .unwrap_or_else(|| Reference::new(substitution.var_name.as_str())),
        subscripts: Vec::new(),
        span,
    })
}

#[cfg(test)]
mod tests {
    use rumoca_core::{BuiltinFunction, Literal, OpBinary, Span};

    use super::*;

    fn span() -> Span {
        Span::from_offsets(
            rumoca_core::SourceId::from_source_name("causal_factor_test.mo"),
            1,
            2,
        )
    }

    fn var(name: &str) -> Expression {
        Expression::VarRef {
            name: Reference::generated(name),
            subscripts: Vec::new(),
            span: span(),
        }
    }

    fn sin(expr: Expression) -> Expression {
        Expression::BuiltinCall {
            function: BuiltinFunction::Sin,
            args: vec![expr],
            span: span(),
        }
    }

    fn real(value: f64) -> Expression {
        Expression::Literal {
            value: Literal::Real(value),
            span: span(),
        }
    }

    fn add(lhs: Expression, rhs: Expression) -> Expression {
        Expression::Binary {
            op: OpBinary::Add,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: span(),
        }
    }

    fn substitution(name: &str, expr: Expression) -> Substitution {
        Substitution {
            var_name: VarName::new(name),
            var_ref: Some(Reference::generated(name)),
            expr,
            var_dims: Vec::new(),
            replacement_dims: Vec::new(),
            env_keys: vec![name.to_string()],
        }
    }

    #[test]
    fn factors_exact_copied_definition_through_named_target() {
        let shared = sin(var("u"));
        let substitutions = vec![
            substitution("x", shared.clone()),
            substitution("y", add(shared.clone(), shared)),
        ];

        let plan = factor_causal_substitutions(&Dae::new(), &substitutions)
            .expect("causal factoring should succeed");

        assert_eq!(
            plan.retained_targets,
            IndexSet::from([VarName::new("x"), VarName::new("y")])
        );
        assert_eq!(plan.substitutions[0].expr, substitutions[0].expr);
        assert_eq!(plan.substitutions[1].expr, add(var("x"), var("x")));
    }

    /// Duplicate removal here is fingerprint-bucketed: a candidate is dropped
    /// when it lands in an existing bucket *and* compares semantically equal.
    /// Its correctness rests on equal expressions hashing equal, so this pins
    /// the collapse now that the fingerprint hashes interned ids rather than
    /// rendered names. The interner is a bijection between text and id, so the
    /// bucket partition is the same one the byte hash produced.
    #[test]
    fn duplicate_computations_collapse_to_one_retained_target() {
        let shared = sin(var("u"));
        let substitutions = vec![
            substitution("first", shared.clone()),
            substitution("second", shared.clone()),
            substitution("consumer", add(shared.clone(), shared)),
        ];

        let plan = factor_causal_substitutions(&Dae::new(), &substitutions)
            .expect("causal factoring should succeed");

        assert!(
            plan.retained_targets.contains(&VarName::new("first")),
            "the first candidate in the bucket owns the shared computation"
        );
        assert!(
            !plan.retained_targets.contains(&VarName::new("second")),
            "an equal computation must not be retained a second time"
        );
        assert_eq!(
            plan.substitutions[2].expr,
            add(var("first"), var("first")),
            "both uses must route through the one retained owner"
        );
    }

    #[test]
    fn does_not_retain_aliases_or_literal_definitions() {
        let substitutions = vec![
            substitution("alias", var("u")),
            substitution(
                "constant",
                Expression::Literal {
                    value: Literal::Real(2.0),
                    span: span(),
                },
            ),
        ];

        let plan = factor_causal_substitutions(&Dae::new(), &substitutions)
            .expect("causal factoring should succeed");

        assert!(plan.retained_targets.is_empty());
        assert_eq!(
            plan.substitutions
                .iter()
                .map(|substitution| &substitution.expr)
                .collect::<Vec<_>>(),
            substitutions
                .iter()
                .map(|substitution| &substitution.expr)
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn closes_forward_aliases_inside_retained_computations() {
        let substitutions = vec![
            substitution("frame", var("shape")),
            substitution("consumer", add(var("frame"), real(1.0))),
            substitution("shape", var("world")),
        ];

        let plan = factor_causal_substitutions(&Dae::new(), &substitutions)
            .expect("forward aliases should close during causal factoring");
        let consumer = plan
            .substitutions
            .iter()
            .find(|substitution| substitution.var_name.as_str() == "consumer")
            .expect("consumer substitution should remain present");

        assert_eq!(consumer.expr, add(var("world"), real(1.0)));
    }

    #[test]
    fn reconnects_expanded_residuals_to_retained_owner() {
        let shared = sin(var("u"));
        let substitutions = vec![substitution("shared", shared.clone())];
        let mut dae = Dae::new();
        dae.continuous
            .equations
            .push(rumoca_ir_dae::Equation::residual(
                add(shared, real(1.0)),
                span(),
                "expanded residual",
            ));

        factor_retained_computations_in_dae(
            &mut dae,
            &substitutions,
            &IndexSet::from([VarName::new("shared")]),
        )
        .expect("expanded computation should reconnect");

        let Expression::Binary { lhs, .. } = &dae.continuous.equations[0].rhs else {
            panic!("expected binary residual");
        };
        assert_eq!(lhs.as_ref(), &var("shared"));
    }
}
