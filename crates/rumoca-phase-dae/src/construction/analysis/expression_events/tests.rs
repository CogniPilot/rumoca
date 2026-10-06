use super::*;
use rumoca_core::SourceId;

fn span(start: usize) -> Span {
    Span::from_offsets(
        SourceId::from_source_name("event_occurrence_lookup.mo"),
        start,
        start + 1,
    )
}

fn literal(value: Literal, source_span: Span) -> Expression {
    Expression::Literal {
        value,
        span: source_span,
    }
}

#[test]
fn fingerprint_collision_preserves_distinct_owners_and_source_order() {
    let occurrence_span = span(0);
    let first = literal(Literal::String("aa".into()), span(1));
    let second = literal(Literal::String("bb".into()), span(1));
    assert_eq!(
        operand_fingerprints(&[&first]),
        operand_fingerprints(&[&second])
    );
    let mut plans = ExpressionEventPlans::default();
    plans
        .insert(
            occurrence_span,
            &[&first],
            ExpressionEventPlan::StateRelation,
        )
        .unwrap();
    plans
        .insert(
            occurrence_span,
            &[&second],
            ExpressionEventPlan::DynamicTimeEvent(DynamicTimeEventOperand::Rhs),
        )
        .unwrap();
    assert!(matches!(
        plans.plan(occurrence_span, &[&first]),
        Some(ExpressionEventPlan::StateRelation)
    ));
    assert!(matches!(
        plans.plan(occurrence_span, &[&second]),
        Some(ExpressionEventPlan::DynamicTimeEvent(
            DynamicTimeEventOperand::Rhs
        ))
    ));
    assert!(!plans.is_structured_state_relation(occurrence_span));
    assert_eq!(plans.ordered().count(), 2);
    assert!(matches!(
        plans.ordered().next().unwrap().1,
        ExpressionEventPlan::StateRelation
    ));
    assert!(matches!(
        plans.ordered().nth(1).unwrap().1,
        ExpressionEventPlan::DynamicTimeEvent(DynamicTimeEventOperand::Rhs)
    ));
}

#[test]
fn equal_signed_zeros_deduplicate_but_conflicting_plan_is_refused() {
    let occurrence_span = span(0);
    let positive = literal(Literal::Real(0.0), span(1));
    let negative = literal(Literal::Real(-0.0), span(1));
    let mut plans = ExpressionEventPlans::default();
    plans
        .insert(
            occurrence_span,
            &[&positive],
            ExpressionEventPlan::StateRelation,
        )
        .unwrap();
    plans
        .insert(
            occurrence_span,
            &[&negative],
            ExpressionEventPlan::StateRelation,
        )
        .unwrap();
    assert_eq!(plans.ordered().count(), 1);
    assert!(
        plans
            .insert(
                occurrence_span,
                &[&negative],
                ExpressionEventPlan::TimeEvent(ClockRational::ONE)
            )
            .is_err()
    );
    assert!(matches!(
        plans.plan(occurrence_span, &[&negative]),
        Some(ExpressionEventPlan::StateRelation)
    ));
    assert!(plans.is_structured_state_relation(occurrence_span));
}

#[test]
fn same_semantic_fingerprint_keeps_exact_operand_spans_distinct() {
    let occurrence_span = span(0);
    let first = literal(Literal::Real(1.0), span(1));
    let second = literal(Literal::Real(1.0), span(2));
    assert_eq!(
        operand_fingerprints(&[&first]),
        operand_fingerprints(&[&second])
    );
    let mut plans = ExpressionEventPlans::default();
    plans
        .insert(
            occurrence_span,
            &[&first],
            ExpressionEventPlan::StateRelation,
        )
        .unwrap();
    plans
        .insert(
            occurrence_span,
            &[&second],
            ExpressionEventPlan::TimeEvent(ClockRational::ONE),
        )
        .unwrap();
    assert_eq!(plans.ordered().count(), 2);
    assert!(matches!(
        plans.plan(occurrence_span, &[&second]),
        Some(ExpressionEventPlan::TimeEvent(ClockRational::ONE))
    ));
    assert!(plans.contains_span(occurrence_span));
    assert!(!plans.contains_span(span(9)));
    assert!(
        plans
            .plan(occurrence_span, &[&literal(Literal::Real(2.0), span(1))])
            .is_none()
    );
}

#[test]
fn large_replicated_span_preserves_every_instance_in_insertion_order() {
    let occurrence_span = span(0);
    let mut plans = ExpressionEventPlans::default();
    for index in 0..14400 {
        let operand = literal(Literal::Integer(index), span(1));
        plans
            .insert(
                occurrence_span,
                &[&operand],
                ExpressionEventPlan::TimeEvent(ClockRational::integer(index.into())),
            )
            .unwrap();
    }
    assert_eq!(plans.ordered().count(), 14400);
    for (index, (_, plan)) in plans.ordered().enumerate() {
        assert!(
            matches!(plan, ExpressionEventPlan::TimeEvent(instant) if instant == ClockRational::integer(index as i128))
        );
        let operand = literal(Literal::Integer(index as i64), span(1));
        assert!(
            matches!(plans.plan(occurrence_span, &[&operand]), Some(ExpressionEventPlan::TimeEvent(instant)) if instant == ClockRational::integer(index as i128))
        );
    }
    assert!(!plans.is_structured_state_relation(occurrence_span));
}
