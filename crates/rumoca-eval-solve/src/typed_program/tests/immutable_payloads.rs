use super::*;

#[test]
fn cloned_full_tensor_payload_is_shared_immutable_and_survives_original_drop() {
    let p = profile(SolveRealFormat::Binary64);
    let value_type = SolveValueType::tensor(SolveScalarType::real(p), vec![14400]).unwrap();
    let bits = [
        0_u64,
        1_u64 << 63,
        0x7ff8_1234_5678_9abc,
        f64::INFINITY.to_bits(),
    ];
    let expected = (0..14400)
        .map(|i| SolveValueKind::Real64(bits[i % 4]))
        .collect::<Vec<_>>();
    let original = TypedValue::construct(value_type, expected.clone()).unwrap();
    let cloned = original.clone();
    assert!(original.elements.is_same_allocation(&cloned.elements));
    assert_eq!(original.elements.holders(), 2);
    assert_eq!(original, cloned);
    drop(original);
    assert_eq!(cloned.elements(), expected);
    assert_eq!(cloned.elements.holders(), 1);
}

#[test]
fn aggregate_update_retains_old_ssa_alias_and_original_input_bits() {
    let p = profile(SolveRealFormat::Binary64);
    let tensor = SolveValueType::tensor(SolveScalarType::real(p), vec![4]).unwrap();
    let mut table = SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(120),
            vec![tensor.clone()],
            vec![SolvePureCallOutput::result(tensor.clone()); 2],
            span(670),
            |b, inputs, outputs| {
                let first = b.load(inputs[0], span(671))?;
                let alias = b.load(inputs[0], span(672))?;
                let replacement = b.constant(SolveValue::real(p, 9.0), span(673))?;
                let index = b.constant(SolveValue::integer(p, 2).unwrap(), span(674))?;
                let updated = b.update_element(first, replacement, &[index], span(675))?;
                b.store(outputs[0], alias, span(676))?;
                b.store(outputs[1], updated, span(677))
            },
        )
        .unwrap();
    let table = table.finish();
    let original = [
        real_kind(SolveRealFormat::Binary64, -0.0),
        SolveValueKind::Real64(0x7ff8_1234_5678_9abc),
        real_kind(SolveRealFormat::Binary64, 3.0),
        real_kind(SolveRealFormat::Binary64, f64::INFINITY),
    ];
    let input = TypedValue::construct(tensor, original.to_vec()).unwrap();
    let result = eval_pure_call(&table, owner, std::slice::from_ref(&input)).unwrap();
    let mut expected = original;
    expected[1] = real_kind(SolveRealFormat::Binary64, 9.0);
    assert_eq!(input.elements(), original);
    assert_eq!(result[0].elements(), original);
    assert_eq!(result[1].elements(), expected);
    assert!(!result[0].elements.is_same_allocation(&result[1].elements));
}

/// A fold that rewrites one element of its carried aggregate per iteration
/// copies the payload once, when it is first shared with the caller, however
/// many iterations follow: the update consumes the carried value it reads last.
#[test]
fn carried_aggregate_updates_reuse_the_dying_payload() {
    let p = profile(SolveRealFormat::Binary64);
    let copies_for = |extent: u32| {
        let tensor = SolveValueType::tensor(SolveScalarType::real(p), vec![extent]).unwrap();
        let domain = StructuredIndexDomain {
            binders: vec![StructuredIndexBinder {
                id: 3,
                display_name: "i".into(),
                lower: 1,
                upper: i64::from(extent),
                step: 1,
            }],
        };
        let mut table = SolvePureCallTable::builder(p);
        let owner = table
            .add_owner(
                identity(130),
                vec![tensor.clone()],
                vec![SolvePureCallOutput::result(tensor.clone())],
                span(700),
                |b, inputs, outputs| {
                    let initial = b.load(inputs[0], span(701))?;
                    let result = b.fold(
                        domain.clone(),
                        &[initial],
                        &[],
                        span(702),
                        |transition, carried, _captures, binders, outputs| {
                            let aggregate = transition.load(carried[0], span(703))?;
                            let index = transition.load(binders[0], span(704))?;
                            let value = transition.constant(SolveValue::real(p, 1.5), span(705))?;
                            let updated =
                                transition.update_element(aggregate, value, &[index], span(706))?;
                            transition.store(outputs[0], updated, span(707))
                        },
                    )?;
                    b.store(outputs[0], result[0], span(708))
                },
            )
            .unwrap();
        let table = table.finish();
        let input = TypedValue::construct(
            tensor,
            vec![real_kind(SolveRealFormat::Binary64, 0.0); extent as usize],
        )
        .unwrap();
        super::super::PAYLOAD_COPIES.with(|copies| copies.set(0));
        let result = eval_pure_call(&table, owner, std::slice::from_ref(&input)).unwrap();
        assert!(
            result[0]
                .elements()
                .iter()
                .all(|element| *element == real_kind(SolveRealFormat::Binary64, 1.5))
        );
        assert!(
            input
                .elements()
                .iter()
                .all(|element| *element == real_kind(SolveRealFormat::Binary64, 0.0))
        );
        super::super::PAYLOAD_COPIES.with(std::cell::Cell::get)
    };
    assert_eq!(copies_for(8), 1);
    assert_eq!(copies_for(512), 1);
}
