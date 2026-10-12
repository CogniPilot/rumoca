//! Frozen model arms retain their data and activation dependency authority.
use super::*;

fn assert_arm_dependency_roots(model: &Dae) {
    model.inspect(|view| {
        let owner = view.model_conditional_at(0).unwrap();
        let ExpressionOperation::Conditional(operands) =
            view.expression(owner.expression()).unwrap().operation()
        else {
            panic!("the checked model owner must close a conditional");
        };
        let conditional = operands.conditional().unwrap();
        let (condition, first) = conditional.branches().next().unwrap();
        let second = conditional.fallback();
        assert_eq!(condition, owner.condition());
        assert_eq!(conditional.branch_count(), 1);
        for (selected, expression, expected) in [(true, first, 1.0), (false, second, 2.0)] {
            assert!(matches!(
                view.expression(expression).unwrap().operation(),
                ExpressionOperation::Literal(DaeLiteral::Real(value)) if *value == expected
            ));
            assert_eq!(
                view.model_arm_of(expression).unwrap().index(),
                owner.arm(selected).index()
            );
            let guard = owner.guards()[usize::from(!selected)];
            assert_eq!(
                view.model_expression_roots(expression).collect::<Vec<_>>(),
                [expression, guard]
            );
        }
        assert!(matches!(
            view.expression(owner.guards()[1]).unwrap().operation(),
            ExpressionOperation::Unary { operator: UnaryOperator::Not, operand }
                if operand == condition
        ));
        assert_eq!(
            view.model_expression_roots(owner.expression())
                .collect::<Vec<_>>(),
            [owner.expression()],
            "closing discharges arm activation from the resulting value"
        );
    });
}

#[test]
fn model_arm_dependency_views_preserve_selection_and_replayed_authority() {
    let model = conditional_fixture();
    assert_arm_dependency_roots(&model);
    let json: Dae = serde_json::from_slice(&serde_json::to_vec(&model).unwrap()).unwrap();
    assert_arm_dependency_roots(&json);
    let binary: Dae = bincode::deserialize(&bincode::serialize(&model).unwrap()).unwrap();
    assert_arm_dependency_roots(&binary);
}
