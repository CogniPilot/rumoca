use super::*;
use crate::projection::tests::fold_context::fold_model;

pub(super) fn with_memo(test: impl for<'dae> FnOnce(dae::DaeView<'dae>, GuardMemo<'dae>)) {
    fold_model(0).inspect(|view| {
        let (function, actuals) = (0..view.expression_count())
            .filter_map(|i| view.expression_id(i))
            .find_map(|expr| match view.expression(expr).unwrap().operation() {
                dae::ExpressionOperation::Call {
                    function,
                    arguments,
                    ..
                } => Some((function, arguments.iter().collect())),
                _ => None,
            })
            .unwrap();
        test(view, GuardMemo::new(function, actuals));
    });
}

fn key(expression: u32, context: usize) -> Key {
    Key {
        activation: Activation::Guaranteed,
        expression,
        context: DomainContextId::test_identity(context),
    }
}

#[test]
fn guard_memo_success_only_publication_preserves_cacheability_and_bounded_keys() {
    with_memo(|_, mut memo| {
        memo.key_limit = 1;
        let first = key(u32::MAX, usize::MAX);
        assert!(matches!(memo.begin(first), Start::Checking));
        memo.finish(false, true);
        assert!(memo.checked.is_empty());
        assert!(matches!(memo.begin(first), Start::Checking));
        memo.finish(true, false);
        let Start::Hit(checked) = memo.begin(first) else {
            panic!("successful guard reuses")
        };
        assert!(!checked.cacheable);
        assert!(matches!(memo.begin(key(0, 0)), Start::None));
        assert_eq!(memo.checked.len(), 1);
    });
}

#[test]
fn guard_memo_effect_overflow_and_cross_frame_invalidation_retain_original_walk() {
    with_memo(|view, mut memo| {
        memo.payload_limit = 1;
        assert!(matches!(memo.begin(key(1, 0)), Start::Checking));
        let span = view
            .expression(view.expression_id(0).unwrap())
            .unwrap()
            .provenance()
            .span();
        let effect = Effect::Parameter {
            function: memo.root,
            dependency: FunctionParameterDependency::Scalar {
                activation: crate::projection::Activation::Guaranteed,
                parameter: 0,
                scalar: usize::MAX,
            },
            span,
        };
        memo.record(effect.clone());
        assert!(
            !memo.independent_walk(),
            "incomplete fragments use ordinary visitation"
        );
        memo.finish(true, true);
        assert!(memo.checked.is_empty());
        assert_eq!(memo.bytes, 0);
        assert!(memo.admission_saturated);
        assert!(matches!(memo.begin(key(1, 0)), Start::None));
        // A distinct fresh invocation can still record; saturation never
        // leaks through the reusable function-summary cache.
        let mut memo = GuardMemo::new(memo.root, memo.actuals.clone());
        assert!(matches!(memo.begin(key(1, 0)), Start::Checking));
        assert!(matches!(memo.begin(key(2, 0)), Start::Checking));
        memo.record(effect);
        memo.invalidate();
        memo.finish(true, true);
        memo.finish(true, true);
        assert!(memo.checked.is_empty());
        assert_eq!(memo.bytes, 0);
    });
}

#[test]
fn guard_memo_effect_order_and_exact_context_are_not_collapsed() {
    with_memo(|view, mut memo| {
        let span = view
            .expression(view.expression_id(0).unwrap())
            .unwrap()
            .provenance()
            .span();
        let first = key(1, 10);
        assert!(matches!(memo.begin(first), Start::Checking));
        for scalar in [9, 2, 9] {
            memo.record(Effect::Parameter {
                function: memo.root,
                dependency: FunctionParameterDependency::Scalar {
                    activation: crate::projection::Activation::Guaranteed,
                    parameter: 0,
                    scalar,
                },
                span,
            });
        }
        memo.finish(true, true);
        let Start::Hit(checked) = memo.begin(first) else {
            panic!("guard check succeeded")
        };
        let scalars = checked
            .effects
            .iter()
            .map(|effect| match effect {
                Effect::Parameter {
                    dependency: FunctionParameterDependency::Scalar { scalar, .. },
                    ..
                } => *scalar,
                _ => panic!("expected recorded scalar"),
            })
            .collect::<Vec<_>>();
        assert_eq!(scalars, [9, 2, 9]);
        assert!(matches!(memo.begin(key(1, 11)), Start::Checking));
        memo.finish(false, true);
        assert!(
            GuardMemo::new(memo.root, memo.actuals.clone())
                .checked
                .is_empty()
        );
    });
}

#[test]
fn guard_memo_payload_saturation_preserves_existing_hits_and_disables_new_forced_checks() {
    with_memo(|view, mut memo| {
        let span = view
            .expression(view.expression_id(0).unwrap())
            .unwrap()
            .provenance()
            .span();
        let effect = Effect::Parameter {
            function: memo.root,
            dependency: FunctionParameterDependency::Scalar {
                activation: crate::projection::Activation::Guaranteed,
                parameter: 0,
                scalar: 2,
            },
            span,
        };
        memo.payload_limit = effect.charge();
        let retained = key(1, 0);
        assert!(matches!(memo.begin(retained), Start::Checking));
        memo.record(effect.clone());
        memo.finish(true, true);
        assert_eq!(memo.bytes, memo.payload_limit);
        assert!(matches!(memo.begin(key(2, 0)), Start::Checking));
        memo.record(effect);
        assert!(!memo.independent_walk());
        memo.finish(true, true);
        assert!(memo.admission_saturated);
        assert!(matches!(memo.begin(retained), Start::Hit(_)));
        for context in [0, 1, usize::MAX] {
            assert!(matches!(memo.begin(key(2, context)), Start::None));
            assert!(
                memo.recording.is_empty(),
                "fallback cannot force independent visitation"
            );
        }
        assert_eq!(memo.checked.len(), 1);
        assert_eq!(memo.bytes, memo.payload_limit);
    });
}

#[test]
fn guard_completion_is_activation_specific_and_failed_strict_work_is_not_published() {
    with_memo(|_, mut memo| {
        let strict = key(3, 0);
        let conditional = Key {
            activation: Activation::Conditional,
            ..strict
        };
        assert!(matches!(memo.begin(conditional), Start::Checking));
        memo.finish(true, true);
        assert!(matches!(memo.begin(strict), Start::Checking));
        memo.finish(false, true);
        assert!(matches!(memo.begin(strict), Start::Checking));
        memo.finish(true, true);
        assert!(matches!(memo.begin(conditional), Start::Hit(_)));
        assert!(matches!(memo.begin(strict), Start::Hit(_)));
        assert_eq!(memo.checked.len(), 2);
    });
}
