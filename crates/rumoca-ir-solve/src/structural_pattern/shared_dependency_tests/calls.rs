use super::*;

fn site() -> crate::SolvePureCallSite {
    site_for(14400)
}

fn site_for(count: u32) -> crate::SolvePureCallSite {
    let arithmetic = crate::SolveArithmeticProfile::construct(
        crate::SolveRealFormat::Binary64,
        crate::SolveIntegerDomain::construct(i64::MIN, i64::MAX).unwrap(),
    );
    let scalar = crate::SolveScalarType::real(arithmetic);
    let vector = crate::SolveValueType::tensor(scalar, vec![count]).unwrap();
    let real = crate::SolveValueType::scalar(scalar);
    let table = crate::SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            crate::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(1).unwrap()),
            vec![vector.clone(), vector.clone()],
            vec![
                crate::SolvePureCallOutput::result(vector),
                crate::SolvePureCallOutput::result(real),
            ],
            span(),
            |builder, inputs, outputs| {
                let tensor = builder.load(inputs[0], span())?;
                builder.store(outputs[0], tensor, span())?;
                let sum = builder.reduce(crate::SolveReductionOperator::Sum, tensor, span())?;
                builder.store(outputs[1], sum, span())
            },
        )?;
        Ok(())
    })
    .unwrap();
    table.owners()[0].call_site()
}

fn load(input: crate::TensorInputKind, start: Reg, input_start: usize) -> LinearOp {
    LinearOp::TensorLoad {
        dst_start: start,
        input,
        input_start,
        count: 14400,
        seed_start: Some(input_start),
        lanes: 1,
    }
}

#[test]
fn issued_per_element_and_whole_input_summaries_preserve_full_capacity() {
    let site = site();
    assert!(!site.output_dependencies()[0][0].is_whole_input());
    assert!(site.output_dependencies()[1][0].is_whole_input());
    compare_program(&[
        load(crate::TensorInputKind::Y, 0, 0),
        load(crate::TensorInputKind::P, 14400, 14400),
        LinearOp::PureCall {
            dst_start: 28800,
            input_starts: vec![0, 14400].into_boxed_slice(),
            site,
        },
        LinearOp::StoreOutputRange {
            start: 28800,
            count: 14401,
            stride: 1,
        },
    ]);
}

#[test]
fn unused_input_and_interface_errors_are_still_traversed() {
    let site = site();
    // Input1 is unused by both issued outputs; the original eager range walk
    // still validates it and must refuse its uninitialized first register.
    compare_program(&[
        load(crate::TensorInputKind::Y, 0, 0),
        LinearOp::PureCall {
            dst_start: 28800,
            input_starts: vec![0, 14400].into_boxed_slice(),
            site: site.clone(),
        },
    ]);
    compare_program(&[LinearOp::PureCall {
        dst_start: 0,
        input_starts: vec![0].into_boxed_slice(),
        site,
    }]);
}

#[test]
fn wide_whole_input_outputs_retain_one_shared_dependency_range() {
    let arithmetic = crate::SolveArithmeticProfile::construct(
        crate::SolveRealFormat::Binary64,
        crate::SolveIntegerDomain::construct(i64::MIN, i64::MAX).unwrap(),
    );
    let vector =
        crate::SolveValueType::tensor(crate::SolveScalarType::real(arithmetic), vec![14400])
            .unwrap();
    let table = crate::SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            crate::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(2).unwrap()),
            vec![vector.clone()],
            vec![crate::SolvePureCallOutput::result(vector)],
            span(),
            |builder, inputs, outputs| {
                let tensor = builder.load(inputs[0], span())?;
                let sum = builder.reduce(crate::SolveReductionOperator::Sum, tensor, span())?;
                let filled = builder.fill(sum, vec![14400], span())?;
                builder.store(outputs[0], filled, span())
            },
        )?;
        Ok(())
    })
    .unwrap();
    let program = [
        load(crate::TensorInputKind::P, 0, 14400),
        LinearOp::PureCall {
            dst_start: 14400,
            input_starts: vec![0].into_boxed_slice(),
            site: table.owners()[0].call_site(),
        },
        LinearOp::StoreOutputRange {
            start: 14400,
            count: 14400,
            stride: 1,
        },
    ];
    let dependencies =
        StructuralPattern::derive_output_p_dependency_ranges(&program, Some(span())).unwrap();
    assert_eq!(dependencies.len(), 14400);
    assert_eq!(
        dependencies[0].intervals().collect::<Vec<_>>(),
        [14400..=28799]
    );
    assert!(
        dependencies
            .iter()
            .all(|set| Arc::ptr_eq(set, &dependencies[0]))
    );
    let y = StructuralPattern::derive_output_y_dependency_ranges(&program, Some(span())).unwrap();
    assert!(
        y.iter()
            .all(|set| set.is_empty() && Arc::ptr_eq(set, &y[0]))
    );
}

#[test]
fn compact_output_views_match_scalar_sets_and_eager_refusals() {
    let site = site();
    let program = [
        load(crate::TensorInputKind::Y, 0, 0),
        load(crate::TensorInputKind::P, 14400, 14400),
        LinearOp::PureCall {
            dst_start: 28800,
            input_starts: vec![0, 14400].into_boxed_slice(),
            site: site.clone(),
        },
        LinearOp::StoreOutputRange {
            start: 28800,
            count: 14401,
            stride: 1,
        },
    ];
    for (ranges, sets) in [
        (
            StructuralPattern::derive_output_y_dependency_ranges(&program, Some(span())).unwrap(),
            StructuralPattern::derive_output_y_dependencies(&program, Some(span())).unwrap(),
        ),
        (
            StructuralPattern::derive_output_p_dependency_ranges(&program, Some(span())).unwrap(),
            StructuralPattern::derive_output_p_dependencies(&program, Some(span())).unwrap(),
        ),
    ] {
        assert_eq!(ranges.len(), sets.len());
        assert!(
            ranges
                .iter()
                .zip(&sets)
                .all(|(ranges, set)| ranges.to_set() == *set)
        );
    }
    let invalid = [
        load(crate::TensorInputKind::Y, 0, 0),
        LinearOp::PureCall {
            dst_start: 28800,
            input_starts: vec![0, 14400].into_boxed_slice(),
            site,
        },
    ];
    assert!(StructuralPattern::derive_output_y_dependency_ranges(&invalid, Some(span())).is_err());
    assert!(StructuralPattern::derive_output_p_dependency_ranges(&invalid, Some(span())).is_err());
}

#[test]
fn four_million_coordinate_call_cells_use_checked_source_families() {
    let count = 4_194_304;
    let site = site_for(count);
    for input in [crate::TensorInputKind::Y, crate::TensorInputKind::P] {
        let program = [
            LinearOp::TensorLoad {
                dst_start: 0,
                input,
                input_start: 0,
                count: count as usize,
                seed_start: None,
                lanes: 1,
            },
            LinearOp::TensorLoad {
                dst_start: count,
                input: crate::TensorInputKind::P,
                input_start: 0,
                count: count as usize,
                seed_start: None,
                lanes: 1,
            },
            LinearOp::Const {
                dst: 2 * count,
                value: 1.0,
            },
            LinearOp::TensorBinary {
                dst_start: 2 * count + 1,
                op: BinaryOp::Add,
                lhs_start: 0,
                rhs_start: 2 * count,
                count: count as usize,
                lhs_stride: 1,
                rhs_stride: 0,
                lanes: 1,
            },
            LinearOp::PureCall {
                dst_start: 3 * count + 1,
                input_starts: vec![2 * count + 1, count].into_boxed_slice(),
                site: site.clone(),
            },
        ];
        let registers = program_register_y_dependencies(&program).unwrap();
        assert_eq!(registers.family_count(), 6);
        let expected = if input == crate::TensorInputKind::Y {
            DependencyState::from_range(0, count as usize)
        } else {
            DependencyState::Empty
        };
        assert_eq!(
            registers.range(3 * count + 1, count as usize),
            Some(expected)
        );
        for offset in [0, count / 2, count - 1] {
            let expected = if input == crate::TensorInputKind::Y {
                DependencyState::singleton(offset as usize)
            } else {
                DependencyState::Empty
            };
            assert_eq!(*registers.state(3 * count + 1 + offset).unwrap(), expected);
        }
    }
}
