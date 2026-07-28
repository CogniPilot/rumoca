//! Equivalence guards for the target-keyed substitution candidate index.
//!
//! The index only ever *filters* which substitutions are tested. These tests
//! pin the two properties that make the filter safe: the candidate set is a
//! superset of the sequential loop's matches, and applying the filtered walk
//! produces exactly what the sequential loop produces.

use super::*;
use crate::eliminate::substitution_index::{SubstitutionCandidates, SubstitutionIndex};

fn span() -> Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("substitution_index_tests.mo"),
        1,
        2,
    )
}

fn var(name: &str) -> Expression {
    Expression::VarRef {
        name: Reference::new(name),
        subscripts: Vec::new(),
        span: span(),
    }
}

fn indexed(name: &str, index: i64) -> Expression {
    Expression::VarRef {
        name: Reference::new(name),
        subscripts: vec![rumoca_core::Subscript::generated_index(index, span())],
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

fn der(name: &str) -> Expression {
    Expression::BuiltinCall {
        function: BuiltinFunction::Der,
        args: vec![var(name)],
        span: span(),
    }
}

fn substitution(name: &str, expr: Expression, var_dims: Vec<i64>) -> Substitution {
    Substitution {
        var_name: VarName::new(name),
        var_ref: Some(Reference::new(name)),
        expr,
        var_dims,
        replacement_dims: Vec::new(),
        env_keys: vec![name.to_string()],
    }
}

/// The naive walk the index must stay observationally identical to.
fn sequential_apply(
    expr: &Expression,
    substitutions: &[Substitution],
    plan: &SubstitutionApplicationPlan,
) -> Expression {
    let mut out = structural_ok(apply_substitutions_to_expr_with_plan(
        expr,
        &[],
        plan,
        |_| Ok(None),
    ));
    for sub in substitutions {
        if !expr_contains_substitution_target(&out, sub) {
            continue;
        }
        out = structural_ok(
            SubstituteVarRewriter {
                substitution: sub,
                replacement: &sub.expr,
                replacement_dims: &sub.replacement_dims,
                derivative_replacement: None,
            }
            .rewrite_expression(&out),
        );
    }
    out
}

fn matcher_family_substitutions() -> Vec<Substitution> {
    vec![
        substitution("a", var("b"), Vec::new()),
        substitution("b", real(2.0), Vec::new()),
        substitution("x[3]", var("q"), Vec::new()),
        substitution("y", var("w"), vec![3]),
        substitution("z.re", real(1.0), Vec::new()),
        substitution("z.im", real(-1.0), Vec::new()),
        substitution("p[2].q", var("r"), Vec::new()),
        substitution("sup[1].phi", var("phi0"), Vec::new()),
        substitution("c", add(var("a"), var("x")), Vec::new()),
        substitution("d", der("b"), Vec::new()),
        substitution("e", indexed("y", 2), Vec::new()),
        substitution("f", var("time"), Vec::new()),
    ]
}

fn matcher_family_expressions() -> Vec<Expression> {
    vec![
        var("a"),
        var("b"),
        var("x[3]"),
        indexed("x", 3),
        indexed("y", 2),
        var("z"),
        var("z.re"),
        var("p.q"),
        var("p[2].q"),
        add(var("a"), var("c")),
        der("b"),
        Expression::Index {
            base: Box::new(var("y")),
            subscripts: vec![rumoca_core::Subscript::generated_expr(
                Box::new(var("a")),
                span(),
            )],
            span: span(),
        },
        var("sup[1].phi"),
        real(4.0),
    ]
}

#[test]
fn index_candidates_are_a_superset_of_sequential_matches() {
    let substitutions = matcher_family_substitutions();
    let index = SubstitutionIndex::new(&substitutions);
    let mut observed_matches = 0usize;
    for expr in matcher_family_expressions() {
        let candidates = index.candidates_for_expression(&expr);
        for (position, sub) in substitutions.iter().enumerate() {
            if !expr_contains_substitution_target(&expr, sub) {
                continue;
            }
            observed_matches += 1;
            assert!(
                candidates.contains(position as u32),
                "candidate index dropped substitution `{}` for expression {expr:?}; \
                 candidates={:?}",
                sub.var_name,
                candidates.positions()
            );
        }
    }
    assert!(
        observed_matches > 0,
        "the table must exercise at least one real match"
    );
}

#[test]
fn indexed_application_equals_sequential_application() {
    let substitutions = matcher_family_substitutions();
    let plan = SubstitutionApplicationPlan::new(&substitutions, None);
    for expr in matcher_family_expressions() {
        let indexed_result = structural_ok(apply_substitutions_to_expr_with_plan(
            &expr,
            &substitutions,
            &plan,
            |_| Ok(None),
        ));
        let sequential_result = sequential_apply(&expr, &substitutions, &plan);
        assert_eq!(
            indexed_result, sequential_result,
            "indexed application diverged from the sequential loop for {expr:?}"
        );
    }
}

#[test]
fn spliced_replacement_exposes_later_substitution() {
    let substitutions = vec![
        substitution("a", var("b"), Vec::new()),
        substitution("b", real(2.0), Vec::new()),
    ];
    let plan = SubstitutionApplicationPlan::new(&substitutions, None);
    let result = structural_ok(apply_substitutions_to_expr_with_plan(
        &var("a"),
        &substitutions,
        &plan,
        |_| Ok(None),
    ));
    assert!(
        matches!(
            result,
            Expression::Literal {
                value: Literal::Real(2.0),
                ..
            }
        ),
        "alias chain a -> b -> 2.0 must close in one walk, got {result:?}"
    );
}

#[test]
fn earlier_substitution_is_not_reapplied_after_a_later_splice() {
    // The sequential loop never revisits position 0 after position 1 fires, so
    // neither may the filtered walk.
    let substitutions = vec![
        substitution("b", real(2.0), Vec::new()),
        substitution("a", var("b"), Vec::new()),
    ];
    let plan = SubstitutionApplicationPlan::new(&substitutions, None);
    let result = structural_ok(apply_substitutions_to_expr_with_plan(
        &var("a"),
        &substitutions,
        &plan,
        |_| Ok(None),
    ));
    assert_eq!(result, sequential_apply(&var("a"), &substitutions, &plan));
    assert!(
        matches!(result, Expression::VarRef { ref name, .. } if name.as_str() == "b"),
        "position 0 was already passed when `a -> b` fired, got {result:?}"
    );
}

#[test]
fn plan_push_matches_full_rebuild() {
    let substitutions = matcher_family_substitutions();
    let constructor = Reference::new("Complex");
    for split in 0..=substitutions.len() {
        let mut incremental =
            SubstitutionApplicationPlan::new(&substitutions[..split], Some(&constructor));
        for sub in &substitutions[split..] {
            incremental.push(sub);
        }
        let rebuilt = SubstitutionApplicationPlan::new(&substitutions, Some(&constructor));
        for expr in matcher_family_expressions() {
            let from_incremental = structural_ok(apply_substitutions_to_expr_with_plan(
                &expr,
                &substitutions,
                &incremental,
                |_| Ok(None),
            ));
            let from_rebuild = structural_ok(apply_substitutions_to_expr_with_plan(
                &expr,
                &substitutions,
                &rebuilt,
                |_| Ok(None),
            ));
            assert_eq!(
                from_incremental, from_rebuild,
                "incremental plan (split={split}) diverged from a full rebuild for {expr:?}"
            );
            assert_eq!(
                incremental
                    .index()
                    .candidates_for_expression(&expr)
                    .positions(),
                rebuilt.index().candidates_for_expression(&expr).positions(),
                "incremental candidate set (split={split}) diverged for {expr:?}"
            );
        }
    }
}

fn scalar_substitution_list(count: u32) -> Vec<Substitution> {
    (0..count)
        .map(|i| substitution(&format!("v{i}"), real(f64::from(i)), Vec::new()))
        .collect()
}

/// The index exists so that rewriting one expression costs a single walk plus
/// the candidates that walk finds. A membership side-table sized by the
/// substitution list would re-allocate and re-zero `n_subs` slots per
/// expression, reinstating the O(n_eq * n_subs) term the index removes.
///
/// The size check is the structural half of that guarantee: the candidate set
/// owns exactly one buffer, the ascending position vector, so there is nowhere
/// for a per-call `vec![false; n_subs]` to live. It fails the moment a side
/// table is reintroduced as a second field.
#[test]
fn candidate_bookkeeping_is_independent_of_substitution_list_length() {
    let candidates_for_list_length = |count: u32| {
        let substitutions = scalar_substitution_list(count);
        SubstitutionIndex::new(&substitutions).candidates_for_expression(&var("v7"))
    };
    let short = candidates_for_list_length(64);
    let long = candidates_for_list_length(4096);
    assert_eq!(short.positions(), [7]);
    assert_eq!(long.positions(), [7]);
    assert_eq!(short.tracked_slot_count(), 1);
    assert_eq!(
        short.tracked_slot_count(),
        long.tracked_slot_count(),
        "candidate bookkeeping must scale with the candidate set, not with the \
         cumulative substitution list"
    );
    assert_eq!(
        std::mem::size_of::<SubstitutionCandidates>(),
        std::mem::size_of::<Vec<u32>>(),
        "the candidate set must own only its ascending position vector; a second \
         buffer sized by the substitution list reinstates the per-expression \
         O(n_subs) allocate-and-memset the index exists to remove"
    );
}

/// The ascending vector doubles as the membership set and is reused in place
/// across the `extend_candidates_after` calls a splice triggers. Reuse must be
/// idempotent and must yield exactly the set a single collection pass yields.
#[test]
fn reused_candidate_buffer_matches_freshly_collected_sets() {
    let substitutions = scalar_substitution_list(4096);
    let index = SubstitutionIndex::new(&substitutions);
    let spliced = add(var("v11"), var("v7"));

    let mut reused = index.candidates_for_expression(&var("v7"));
    assert_eq!(reused.positions(), [7]);
    for _ in 0..3 {
        // `v7` is at or below the cursor, so only `v11` may be added, and only
        // once however often the same replacement is re-scanned.
        index.extend_candidates_after(&spliced, 7, &mut reused);
        assert_eq!(
            reused.positions(),
            [7, 11],
            "reusing the candidate buffer must stay ascending and de-duplicated"
        );
    }

    let fresh = index.candidates_for_expression(&spliced);
    assert_eq!(
        fresh.positions(),
        reused.positions(),
        "the reused buffer must agree with a freshly collected candidate set"
    );
    assert_eq!(reused.tracked_slot_count(), 2);
}

/// The index is addressed by list position, so a plan that indexes fewer
/// positions than the list simply never offers the tail substitutions to the
/// exact predicate: they vanish silently. SPEC_0008 forbids that recovery.
#[test]
fn plan_shorter_than_substitution_list_is_a_hard_error() {
    let substitutions = vec![
        substitution("a", var("b"), Vec::new()),
        substitution("b", real(2.0), Vec::new()),
    ];
    let short_plan = SubstitutionApplicationPlan::new(&substitutions[..1], None);
    let err =
        apply_substitutions_to_expr_with_plan(&var("b"), &substitutions, &short_plan, |_| Ok(None))
            .expect_err("a plan that indexes fewer positions than the list must not be applied");
    assert_eq!(
        err.code(),
        crate::diagnostic_codes::ES014_CONTRACT_VIOLATION,
        "unexpected diagnostic for a plan/list mismatch: {err}"
    );
    assert!(
        format!("{err}").contains("silently skip substitutions"),
        "unexpected error for a plan/list mismatch: {err}"
    );
}

#[test]
fn candidate_index_skips_unrelated_substitutions() {
    // Without this the "index" would be a no-op filter and the pass would keep
    // its quadratic behaviour.
    let substitutions = (0..64)
        .map(|i| substitution(&format!("v{i}"), real(f64::from(i)), Vec::new()))
        .collect::<Vec<_>>();
    let index = SubstitutionIndex::new(&substitutions);
    let candidates = index.candidates_for_expression(&var("v7"));
    assert_eq!(candidates.positions(), [7]);
    assert!(
        index
            .candidates_for_expression(&var("absent"))
            .positions()
            .is_empty()
    );
}
