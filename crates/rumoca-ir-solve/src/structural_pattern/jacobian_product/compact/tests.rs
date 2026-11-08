use super::super::tests::{model, span};
use super::*;
use crate::*;
use rumoca_core::{IndexBinderProvenance, StructuredIndexBinder};

fn domain(n: u32) -> StructuredIndexDomain {
    StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            provenance: IndexBinderProvenance::Generated,
            id: 7,
            display_name: "i".into(),
            lower: 1,
            upper: i64::from(n),
            step: 1,
        }],
    }
}

#[test]
fn actual_output_presence_retains_cancelled_zero_and_omits_unread_seed() {
    let model = model();
    let p =
        CompactJacobianPlacement::new(vec![2], (0, vec![1]), vec![(0, vec![0]), (1, vec![0])], 2)
            .unwrap();
    let product = p
        .with_dependency_receipt(span(), |receipt| {
            StateJacobianProduct::construct(
                model.continuous_numerical_source().unwrap(),
                StoredStateJacobianOutputOrder::canonical(&model).unwrap(),
                &SolveTargetProfile::current_dense_host(),
                span(),
                |c| {
                    c.store_compact(receipt, domain(2), 20, span(), |body| {
                        let x = body.read(StateJacobianCompactInput::Seed(1), (span(), 0))?;
                        body.binary(SolveBinaryOperator::Subtract, [&x, &x], (span(), 1))
                    })
                },
            )
        })
        .unwrap()
        .unwrap();
    for row in 0..2 {
        assert!(product.dependencies().contains(row, 1));
        assert!(!product.dependencies().contains(row, 0));
    }
    assert!(matches!(
        product.canonical_dependencies().view(),
        StructuralPatternView::Stacked { segments: 1 }
    ));
}

#[test]
fn wrong_domain_or_state_owner_refuses_before_body() {
    assert_wrong_domain(1, 2);
    assert_wrong_domain(2, 3);
}
fn assert_wrong_domain(count: u32, states: usize) {
    let model = model();
    let called = std::cell::Cell::new(false);
    let p = CompactJacobianPlacement::new(vec![2], (0, vec![1]), vec![], states).unwrap();
    let result = p
        .with_dependency_receipt(span(), |receipt| {
            StateJacobianProduct::construct(
                model.continuous_numerical_source().unwrap(),
                StoredStateJacobianOutputOrder::canonical(&model).unwrap(),
                &SolveTargetProfile::current_dense_host(),
                span(),
                |c| {
                    c.store_compact(receipt, domain(count), 0, span(), |body| {
                        called.set(true);
                        body.constant(0.0, (span(), 0))
                    })
                },
            )
        })
        .unwrap();
    assert!(result.is_err());
    assert!(!called.get());
}

#[test]
fn row_ownership_is_complete_even_without_state_directions() {
    assert_incomplete(false);
    assert_incomplete(true);
}
fn assert_incomplete(duplicate: bool) {
    let model = model();
    let p = CompactJacobianPlacement::new(vec![1], (0, vec![1]), vec![], 2).unwrap();
    let result = p
        .with_dependency_receipt(span(), |receipt| {
            StateJacobianProduct::construct(
                model.continuous_numerical_source().unwrap(),
                StoredStateJacobianOutputOrder::canonical(&model).unwrap(),
                &SolveTargetProfile::current_dense_host(),
                span(),
                |c| {
                    c.store_compact(receipt, domain(1), 0, span(), |b| {
                        b.constant(0.0, (span(), 0))
                    })?;
                    if duplicate {
                        let z = c.constant(0.0, (span(), SolveFiniteCheck::Derivative))?;
                        c.store_row(0, &z, span())?;
                    }
                    Ok(())
                },
            )
        })
        .unwrap();
    assert!(result.is_err());
}

fn entries(value: &serde_json::Value) -> usize {
    match value {
        serde_json::Value::Array(a) => a.len() + a.iter().map(entries).sum::<usize>(),
        serde_json::Value::Object(o) => o.len() + o.values().map(entries).sum::<usize>(),
        _ => 1,
    }
}

#[test]
fn actual_joined_metadata_stays_compact_at_4096_points() {
    let counts = [8, 4096].map(|n| {
        let profile = SolveArithmeticProfile::construct(
            SolveRealFormat::Binary64,
            SolveIntegerDomain::construct(i32::MIN.into(), i32::MAX.into()).unwrap(),
        );
        let p =
            CompactJacobianPlacement::new(vec![n], (0, vec![1]), vec![(0, vec![1])], n as usize)
                .unwrap();
        let mut serialized = None;
        p.with_dependency_receipt(span(), |receipt| {
            TypedProgram::construct(profile, |builder| {
                let zero = builder.constant(SolveValue::real(profile, 0.0), span())?;
                let inputs = vec![zero; n as usize + 1];
                let body = construct_body(
                    builder,
                    &inputs,
                    receipt,
                    domain(n),
                    (n as usize, 0, profile, 0, span()),
                    |b| b.read(StateJacobianCompactInput::Seed(0), (span(), 0)),
                )?;
                serialized = Some(serde_json::to_value(&body.relation).unwrap());
                assert!(body.relation.contains(n - 1, n - 1));
                Ok(())
            })
        })
        .unwrap()
        .unwrap();
        let serialized = serialized.unwrap();
        let count = entries(&serialized);
        println!("COMPACT_METADATA_POINTS={n} ENTRIES={count} JSON={serialized}");
        assert!(count < 128);
        count
    });
    assert_eq!(counts[0], counts[1]);
}

#[test]
fn parameter_and_time_reads_are_guarded_but_never_seed_a_state_direction() {
    let m = model();
    let profile = m
        .continuous_numerical_source()
        .unwrap()
        .calls()
        .arithmetic();
    let p = CompactJacobianPlacement::new(vec![1], (0, vec![0]), vec![], 1).unwrap();
    p.with_dependency_receipt(span(), |receipt| {
        TypedProgram::construct(profile, |builder| {
            let inputs = [0.0, 3.0, 2.0]
                .into_iter()
                .map(|x| builder.constant(SolveValue::real(profile, x), span()))
                .collect::<Result<Vec<_>, _>>()?;
            let body = construct_body(
                builder,
                &inputs,
                receipt,
                domain(1),
                (1, 1, profile, 0, span()),
                |b| {
                    let p = b.read(StateJacobianCompactInput::Parameter(0), (span(), 0))?;
                    let t = b.read(StateJacobianCompactInput::Time, (span(), 1))?;
                    let sum = b.binary(SolveBinaryOperator::Add, [&p, &t], (span(), 2))?;
                    let copied = b.copy(&sum, (span(), 3))?;
                    b.unary(SolveUnaryOperator::Abs, &copied, (span(), 4))
                },
            )?;
            assert_eq!(body.relation.nonzero_upper_bound(), Some(0));
            Ok(())
        })
    })
    .unwrap()
    .unwrap();
}
