use super::*;
use crate::projection::tests::fold_context::fold_model;

#[test]
fn binder_support_uses_exact_axes_and_last_lexical_binding_only() {
    fold_model(0).inspect(|view| {
        let outer = view.domain_id(0).unwrap();
        let inner = view.domain_id(1).unwrap();
        let binder = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .find_map(|expr| match view.expression(expr).unwrap().operation() {
                dae::ExpressionOperation::Coordinate(dae::CoordinateView::Binder(binder))
                    if binder.domain() == outer =>
                {
                    Some(binder)
                }
                _ => None,
            })
            .unwrap();
        let support = [binder];
        for unused in [i64::MIN, -7, 1, 2, i64::MAX] {
            let points = [(outer, vec![2]), (inner, vec![unused])];
            assert_eq!(binder_values(&support, &points), Some([2, 0, 0, 0]));
        }
        assert_eq!(
            binder_values(&support, &[(outer, vec![1]), (outer, vec![2])]),
            Some([2, 0, 0, 0])
        );
        assert_eq!(binder_values(&support, &[(inner, vec![1])]), None);
        assert_eq!(binder_values(&support, &[(outer, vec![])]), None);
    });
}

#[test]
fn successful_checks_are_bounded_and_failed_checks_are_not_published() {
    // The same checked DAE supplies branded IDs; no sparse numeric identity
    // is converted to an arena-sized allocation.
    fold_model(0).inspect(|view| {
        let function = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .find_map(|expr| match view.expression(expr).unwrap().operation() {
                dae::ExpressionOperation::Call { function, .. } => Some(function),
                _ => None,
            })
            .unwrap();
        let mut memo = ValidationMemo::new(function, vec![]);
        let key = Key {
            expression: u32::MAX,
            scalar: usize::MAX,
            binders: [i64::MIN; 4],
        };
        memo.recording = 1;
        memo.finish(key, false);
        assert!(memo.success.is_empty());
        memo.recording = 1;
        memo.finish(key, true);
        assert!(memo.success.contains(&key));
        for scalar in 0..LIMIT {
            memo.recording = 1;
            memo.finish(Key { scalar, ..key }, true);
        }
        assert_eq!(memo.success.len(), LIMIT);
        assert_eq!(memo.recording, 0);
        assert!(ValidationMemo::new(function, vec![]).success.is_empty());
    });
}

#[test]
fn pure_index_ignores_unused_inner_axis_and_keeps_used_outer_axis() {
    crate::projection::tests::fold_context::fold_model_used_axes(0, true).inspect(|view| {
        let (function, actuals) = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .find_map(|expr| match view.expression(expr).unwrap().operation() {
                dae::ExpressionOperation::Call {
                    function,
                    arguments,
                    ..
                } => Some((function, arguments.iter().collect())),
                _ => None,
            })
            .unwrap();
        let mut memo = ValidationMemo::new(function, actuals);
        let outer = view.domain_id(0).unwrap();
        let inner = view.domain_id(1).unwrap();
        let selected = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .find(|expr| {
                memo.support(view, *expr)
                    .is_some_and(|axes| axes.len() == 1)
            })
            .unwrap();
        let Start::Checking(key) =
            memo.begin(view, selected, 0, &[(outer, vec![1]), (inner, vec![1])])
        else {
            panic!("first check is a miss")
        };
        memo.finish(key, true);
        assert!(matches!(
            memo.begin(
                view,
                selected,
                0,
                &[(outer, vec![1]), (inner, vec![i64::MAX])]
            ),
            Start::Hit
        ));
        let Start::Checking(other) =
            memo.begin(view, selected, 0, &[(outer, vec![2]), (inner, vec![1])])
        else {
            panic!("used binder changes the key")
        };
        memo.finish(other, false);
        assert!(!memo.success.contains(&other));
        assert!(matches!(
            memo.begin(view, selected, 0, &[(inner, vec![1])]),
            Start::None
        ));
        let mut other_invocation = ValidationMemo::new(function, vec![]);
        assert!(matches!(
            other_invocation.begin(view, selected, 0, &[(outer, vec![1])]),
            Start::None
        ));
    });
}
