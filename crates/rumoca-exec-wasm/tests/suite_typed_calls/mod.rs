mod aggregate_folds;
mod aggregate_ops;
mod assertion_outputs;
mod broadcast;
mod call_region_laziness;
mod calls;
mod capture_borrows;
mod carried_lifetimes;
mod cross_backend_aliasing;
mod extrema_tensor;
mod finite_domains;
mod fold_initial_aliasing;
mod fold_scratch;
mod in_place_aliasing;
mod integer_quotients;
mod integer_unaries;
pub(crate) mod maps;
pub(crate) mod math;
mod matrix_products;
mod real_stores;
mod repeated_calls;
mod returned_storage;
mod scalar_cells;
mod slice_updates;
mod source_tables;
pub(crate) mod square;
mod static_slices;
mod tensor_source;
mod tensor_views;
mod view_updates;

use super::*;
use rumoca_core::{StructuredIndexBinder, StructuredIndexDomain};

#[test]
fn fold_binder_boundaries_never_increment_past_the_final_value() {
    let p = profile();
    let integer = solve::SolveValueType::scalar(solve::SolveScalarType::integer(p));
    for domain in [
        domain(i64::MAX - 1, i64::MAX, 1),
        domain(i64::MIN, -1, i64::MAX),
        domain(i64::MIN + 1, i64::MIN, -1),
    ] {
        let mut table = solve::SolvePureCallTable::builder(p);
        let owner = table
            .add_owner(
                identity(25),
                vec![],
                vec![solve::SolvePureCallOutput::result(integer.clone())],
                span(270),
                |b, _, outputs| {
                    let initial =
                        b.constant(solve::SolveValue::integer(p, 0).unwrap(), span(271))?;
                    let result = b.fold(
                        domain,
                        &[initial],
                        &[],
                        span(272),
                        |r, _, _, binders, outputs| {
                            let index = r.load(binders[0], span(273))?;
                            r.store(outputs[0], index, span(274))
                        },
                    )?;
                    b.store(outputs[0], result[0], span(275))
                },
            )
            .unwrap();
        let site = table.call_site(owner).unwrap();
        let table = table.finish();
        let compiled = compile_pure_call_wasm(&table, &site).unwrap();
        let (status, actual) = Runner::new(&compiled).run(&[]);
        assert_eq!(status, 0);
        assert_eq!(actual, oracle(&table, &site, &[]).unwrap());
    }
}

#[test]
fn nested_conditional_inside_fold_retains_activation_and_full_fault_path() {
    let p = profile();
    let real_type = solve::SolveValueType::scalar(solve::SolveScalarType::real(p));
    let integer = solve::SolveValueType::scalar(solve::SolveScalarType::integer(p));
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(26),
            vec![real_type],
            vec![solve::SolvePureCallOutput::result(integer.clone())],
            span(280),
            |b, inputs, outputs| {
                let input = b.load(inputs[0], span(281))?;
                let initial = b.constant(solve::SolveValue::integer(p, 0).unwrap(), span(282))?;
                let result = b.fold(
                    domain(1, 3, 1),
                    &[initial],
                    &[input],
                    span(283),
                    |r, carried, captures, binders, outputs| {
                        let old = r.load(carried[0], span(284))?;
                        let input = r.load(captures[0], span(285))?;
                        let index = r.load(binders[0], span(286))?;
                        let three =
                            r.constant(solve::SolveValue::integer(p, 3).unwrap(), span(287))?;
                        let condition =
                            r.compare(solve::SolveCompareOperator::Equal, index, three, span(288))?;
                        let next = r.conditional(
                            condition,
                            &[old, input],
                            vec![integer.clone()],
                            span(289),
                            |r, inputs, outputs| {
                                let input = r.load(inputs[1], span(290))?;
                                let converted = r.convert(
                                    solve::SolveConversionOperator::RealToIntegerTowardZero,
                                    input,
                                    span(291),
                                )?;
                                r.store(outputs[0], converted, span(292))
                            },
                            |r, inputs, outputs| {
                                let old = r.load(inputs[0], span(293))?;
                                let one = r.constant(
                                    solve::SolveValue::integer(p, 1).unwrap(),
                                    span(294),
                                )?;
                                let next =
                                    r.binary(solve::SolveBinaryOperator::Add, old, one, span(295))?;
                                r.store(outputs[0], next, span(296))
                            },
                        )?;
                        r.store(outputs[0], next[0], span(297))
                    },
                )?;
                b.store(outputs[0], result[0], span(298))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    let table = table.finish();
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let mut runner = Runner::new(&compiled);
    for value in [f64::INFINITY, 4.5] {
        let inputs = vec![vec![real(value)]];
        let (status, actual) = runner.run(&cells(inputs.iter().flatten().copied()));
        match oracle(&table, &site, &inputs) {
            Ok(expected) => {
                assert_eq!(status, 0);
                assert_eq!(actual, expected);
            }
            Err(_) => {
                assert_eq!(actual, vec![0xa5; 8]);
                let fault = compiled
                    .faults()
                    .iter()
                    .find(|f| f.status == status as u32)
                    .unwrap();
                assert_eq!(fault.provenance, span(291));
                assert_eq!(fault.region_path, [(2, 0), (5, 0)]);
            }
        }
    }
}

fn domain(lower: i64, upper: i64, step: i64) -> StructuredIndexDomain {
    StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            id: 9,
            display_name: "coordinate".into(),
            lower,
            upper,
            step,
        }],
    }
}

#[test]
fn nested_folds_keep_distinct_counters_and_outer_binder_captures() {
    let p = profile();
    let integer = solve::SolveValueType::scalar(solve::SolveScalarType::integer(p));
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(27),
            vec![],
            vec![solve::SolvePureCallOutput::result(integer)],
            span(300),
            |b, _, outputs| {
                let zero = b.constant(solve::SolveValue::integer(p, 0).unwrap(), span(301))?;
                let sum = b.fold(
                    domain(1, 3, 1),
                    &[zero],
                    &[],
                    span(302),
                    |r, carried, _, binders, outputs| {
                        let old = r.load(carried[0], span(303))?;
                        let outer = r.load(binders[0], span(304))?;
                        let inner = r.fold(
                            domain(1, 2, 1),
                            &[old],
                            &[outer],
                            span(305),
                            |r, carried, captures, binders, outputs| {
                                let old = r.load(carried[0], span(306))?;
                                let outer = r.load(captures[0], span(307))?;
                                let inner = r.load(binders[0], span(308))?;
                                let ten = r.constant(
                                    solve::SolveValue::integer(p, 10).unwrap(),
                                    span(309),
                                )?;
                                let outer = r.binary(
                                    solve::SolveBinaryOperator::Multiply,
                                    outer,
                                    ten,
                                    span(310),
                                )?;
                                let term = r.binary(
                                    solve::SolveBinaryOperator::Add,
                                    outer,
                                    inner,
                                    span(311),
                                )?;
                                let next = r.binary(
                                    solve::SolveBinaryOperator::Add,
                                    old,
                                    term,
                                    span(312),
                                )?;
                                r.store(outputs[0], next, span(313))
                            },
                        )?;
                        r.store(outputs[0], inner[0], span(314))
                    },
                )?;
                b.store(outputs[0], sum[0], span(315))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    let table = table.finish();
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let (status, actual) = Runner::new(&compiled).run(&[]);
    assert_eq!(status, 0);
    assert_eq!(actual, oracle(&table, &site, &[]).unwrap());
    assert_eq!(actual, cells([solve::SolveValueKind::Integer(129)]));
}

#[test]
fn empty_fold_never_evaluates_faulting_transition_and_returns_initial_tuple() {
    let p = profile();
    let integer = solve::SolveValueType::scalar(solve::SolveScalarType::integer(p));
    let real_type = solve::SolveValueType::scalar(solve::SolveScalarType::real(p));
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(28),
            vec![real_type],
            vec![solve::SolvePureCallOutput::result(integer)],
            span(320),
            |b, inputs, outputs| {
                let capture = b.load(inputs[0], span(321))?;
                let initial = b.constant(solve::SolveValue::integer(p, -7).unwrap(), span(322))?;
                let result = b.fold(
                    domain(4, 3, 1),
                    &[initial],
                    &[capture],
                    span(323),
                    |r, _, captures, _, outputs| {
                        let input = r.load(captures[0], span(324))?;
                        let value = r.convert(
                            solve::SolveConversionOperator::RealToIntegerTowardZero,
                            input,
                            span(325),
                        )?;
                        r.store(outputs[0], value, span(326))
                    },
                )?;
                b.store(outputs[0], result[0], span(327))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    let table = table.finish();
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let inputs = vec![vec![real(f64::INFINITY)]];
    let (status, actual) = Runner::new(&compiled).run(&cells(inputs.iter().flatten().copied()));
    assert_eq!(status, 0);
    assert_eq!(actual, oracle(&table, &site, &inputs).unwrap());
    assert_eq!(actual, cells([solve::SolveValueKind::Integer(-7)]));
}

#[test]
fn multi_binder_domain_executes_the_complete_checked_cartesian_domain() {
    let mut d = domain(1, 3, 1);
    let mut second = d.binders[0].clone();
    second.id = 10;
    d.binders.push(second);
    let (table, site) = integer_fold(d);
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let inputs = vec![vec![solve::SolveValueKind::Integer(0)]];
    let (status, output) = Runner::new(&compiled).run(&cells(inputs.iter().flatten().copied()));
    assert_eq!(status, 0);
    assert_eq!(
        output,
        cells([
            solve::SolveValueKind::Integer(0),
            solve::SolveValueKind::Integer(18)
        ])
    );
    assert_eq!(output, oracle(&table, &site, &inputs).unwrap());
}

fn integer_fold(
    domain: StructuredIndexDomain,
) -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let p = profile();
    let integer = solve::SolveValueType::scalar(solve::SolveScalarType::integer(p));
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(20),
            vec![integer.clone()],
            vec![solve::SolvePureCallOutput::result(integer); 2],
            span(200),
            |b, inputs, outputs| {
                let initial = b.load(inputs[0], span(201))?;
                b.store(outputs[0], initial, span(202))?;
                let result = b.fold(
                    domain,
                    &[initial],
                    &[],
                    span(203),
                    |r, carried, _, binders, outputs| {
                        let old = r.load(carried[0], span(204))?;
                        let index = r.load(binders[0], span(205))?;
                        let next =
                            r.binary(solve::SolveBinaryOperator::Add, old, index, span(206))?;
                        r.store(outputs[0], next, span(207))
                    },
                )?;
                b.store(outputs[1], result[0], span(208))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    (table.finish(), site)
}

/// `s := s + i` over `i in 1:14400` while `s < limit`, from the input `s`.
fn continued_fold() -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let p = profile();
    let integer = solve::SolveValueType::scalar(solve::SolveScalarType::integer(p));
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(21),
            vec![integer.clone(), integer.clone()],
            vec![solve::SolvePureCallOutput::result(integer)],
            span(210),
            |b, inputs, outputs| {
                let initial = b.load(inputs[0], span(211))?;
                let limit = b.load(inputs[1], span(211))?;
                let result = b.fold_while(
                    domain(1, 14400, 1),
                    &[initial],
                    &[limit],
                    span(212),
                    |r, carried, captures, outputs| {
                        let sum = r.load(carried[0], span(213))?;
                        let limit = r.load(captures[0], span(213))?;
                        let below =
                            r.compare(solve::SolveCompareOperator::Less, sum, limit, span(213))?;
                        r.store(outputs[0], below, span(213))
                    },
                    |r, carried, _, binders, outputs| {
                        let old = r.load(carried[0], span(214))?;
                        let index = r.load(binders[0], span(214))?;
                        let next =
                            r.binary(solve::SolveBinaryOperator::Add, old, index, span(214))?;
                        r.store(outputs[0], next, span(214))
                    },
                )?;
                b.store(outputs[0], result[0], span(215))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    (table.finish(), site)
}

#[test]
fn a_continued_fold_ends_at_its_first_false_predicate() {
    let (table, site) = continued_fold();
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let mut runner = Runner::new(&compiled);
    // 0, 1, 3, 6, 10 stops below 10; a start at the limit runs no pass.
    for (start, limit, expected) in [(0, 10, 10), (5, 5, 5), (0, 1_000_000_000, 103_687_200)] {
        let inputs = vec![
            vec![solve::SolveValueKind::Integer(start)],
            vec![solve::SolveValueKind::Integer(limit)],
        ];
        let (status, actual) = runner.run(&cells(inputs.iter().flatten().copied()));
        assert_eq!(status, 0);
        let oracle = oracle(&table, &site, &inputs).unwrap();
        assert_eq!(actual, oracle);
        assert_eq!(oracle, cells([solve::SolveValueKind::Integer(expected)]));
    }
}

#[test]
fn finite_fold_domains_are_compact_ordered_and_empty_is_identity() {
    let mut sizes = Vec::new();
    for d in [
        domain(1, 3, 1),
        domain(1, 14400, 1),
        domain(9, -4, -3),
        domain(1, 0, 1),
    ] {
        let (table, site) = integer_fold(d);
        let compiled = compile_pure_call_wasm(&table, &site).unwrap();
        sizes.push((
            compiled.module_bytes().len(),
            compiled.layout().scratch_bytes,
        ));
        let mut runner = Runner::new(&compiled);
        for input in [0, 9, -7] {
            let inputs = vec![vec![solve::SolveValueKind::Integer(input)]];
            let (status, actual) = runner.run(&cells(inputs.iter().flatten().copied()));
            assert_eq!(status, 0);
            assert_eq!(actual, oracle(&table, &site, &inputs).unwrap());
        }
    }
    assert!(
        sizes[0].0.abs_diff(sizes[1].0) < 40,
        "domain extent cannot expand code"
    );
    assert_eq!(
        sizes[0].1, sizes[1].1,
        "scratch is independent of iteration count"
    );
}

#[test]
fn late_fold_failure_retains_region_span_atomic_outputs_and_recovery() {
    let (table, site) = integer_fold(domain(1, 3, 1));
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let mut runner = Runner::new(&compiled);
    let inputs = vec![vec![solve::SolveValueKind::Integer(i64::MAX - 3)]];
    assert!(oracle(&table, &site, &inputs).is_err());
    let (status, actual) = runner.run(&cells(inputs.iter().flatten().copied()));
    assert_eq!(actual, vec![0xa5; compiled.layout().output_bytes as usize]);
    let fault = compiled
        .faults()
        .iter()
        .find(|f| f.status == status as u32)
        .unwrap();
    assert_eq!(fault.kind, TypedCallFaultKind::IntegerArithmetic);
    assert_eq!(fault.provenance, span(206));
    assert_eq!(fault.region_path, [(2, 0)]);
    assert_eq!(fault.operation, Some(2));
    let inputs = vec![vec![solve::SolveValueKind::Integer(2)]];
    let (status, actual) = runner.run(&cells(inputs.iter().flatten().copied()));
    assert_eq!(status, 0);
    assert_eq!(actual, oracle(&table, &site, &inputs).unwrap());
}

#[test]
fn lazy_conditional_does_not_execute_inactive_faulting_region() {
    let p = profile();
    let r = solve::SolveValueType::scalar(solve::SolveScalarType::real(p));
    let integer = solve::SolveValueType::scalar(solve::SolveScalarType::integer(p));
    let boolean = solve::SolveValueType::scalar(solve::SolveScalarType::Boolean);
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(21),
            vec![boolean, r],
            vec![solve::SolvePureCallOutput::result(integer.clone())],
            span(210),
            |b, inputs, outputs| {
                let condition = b.load(inputs[0], span(211))?;
                let capture = b.load(inputs[1], span(212))?;
                let result = b.conditional(
                    condition,
                    &[capture],
                    vec![integer],
                    span(213),
                    |r, inputs, outputs| {
                        let input = r.load(inputs[0], span(214))?;
                        let converted = r.convert(
                            solve::SolveConversionOperator::RealToIntegerTowardZero,
                            input,
                            span(215),
                        )?;
                        r.store(outputs[0], converted, span(216))
                    },
                    |r, _, outputs| {
                        let seven =
                            r.constant(solve::SolveValue::integer(p, 7).unwrap(), span(217))?;
                        r.store(outputs[0], seven, span(218))
                    },
                )?;
                b.store(outputs[0], result[0], span(219))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    let table = table.finish();
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let mut runner = Runner::new(&compiled);
    for (condition, input) in [
        (false, f64::NAN),
        (true, f64::INFINITY),
        (false, f64::INFINITY),
        (true, -8.3),
    ] {
        let inputs = vec![
            vec![solve::SolveValueKind::Boolean(condition)],
            vec![real(input)],
        ];
        let (status, actual) = runner.run(&cells(inputs.iter().flatten().copied()));
        match oracle(&table, &site, &inputs) {
            Ok(expected) => {
                assert_eq!(status, 0);
                assert_eq!(actual, expected);
            }
            Err(_) => {
                assert_eq!(actual, vec![0xa5; 8]);
                let fault = compiled
                    .faults()
                    .iter()
                    .find(|f| f.status == status as u32)
                    .unwrap();
                assert_eq!(fault.provenance, span(215));
                assert_eq!(fault.region_path, [(2, 0)]);
            }
        }
    }
}

#[test]
fn ordered_real_fold_keeps_scalar_snapshots_and_full_tensor_capture() {
    let p = profile();
    let r = solve::SolveValueType::scalar(solve::SolveScalarType::real(p));
    let tensor = solve::SolveValueType::tensor(r.element_type(), vec![14400]).unwrap();
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(22),
            vec![tensor],
            vec![solve::SolvePureCallOutput::result(r.clone()); 2],
            span(220),
            |b, inputs, outputs| {
                let values = b.load(inputs[0], span(221))?;
                let zero = b.constant(solve::SolveValue::real(p, -0.0), span(222))?;
                let one = b.constant(solve::SolveValue::real(p, 1.0), span(223))?;
                let result = b.fold(
                    domain(1, 14400, 1),
                    &[zero, one],
                    &[values],
                    span(224),
                    |r, carried, captures, binders, outputs| {
                        let sum = r.load(carried[0], span(225))?;
                        let old = r.load(carried[1], span(226))?;
                        let values = r.load(captures[0], span(227))?;
                        let index = r.load(binders[0], span(228))?;
                        let value = r.project_element_dynamic(values, &[index], span(229))?;
                        let next =
                            r.binary(solve::SolveBinaryOperator::Add, sum, value, span(230))?;
                        r.store(outputs[0], next, span(231))?;
                        // The old first carried value remains a snapshot after output zero.
                        let next_old =
                            r.binary(solve::SolveBinaryOperator::Subtract, old, sum, span(232))?;
                        r.store(outputs[1], next_old, span(233))
                    },
                )?;
                b.store(outputs[0], result[0], span(234))?;
                b.store(outputs[1], result[1], span(235))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    let table = table.finish();
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    assert!(compiled.module_bytes().len() < 6000);
    let mut runner = Runner::new(&compiled);
    for pattern in [
        [1e16, 1.0, -1e16, 3.0],
        [-0.0, 0.0, -0.0, 0.0],
        [0.125, -0.25, 0.5, -1.0],
    ] {
        let inputs = vec![(0..14400).map(|n| real(pattern[n % 4])).collect::<Vec<_>>()];
        let (status, actual) = runner.run(&cells(inputs.iter().flatten().copied()));
        assert_eq!(status, 0);
        assert_eq!(actual, oracle(&table, &site, &inputs).unwrap());
    }
}

#[test]
fn conditional_aggregate_input_loads_remain_immutable_across_consumed_updates() {
    let p = profile();
    let tensor = solve::SolveValueType::tensor(solve::SolveScalarType::real(p), vec![4]).unwrap();
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(23),
            vec![tensor.clone()],
            vec![solve::SolvePureCallOutput::result(tensor.clone()); 2],
            span(240),
            |b, inputs, outputs| {
                let input = b.load(inputs[0], span(241))?;
                let condition = b.constant(solve::SolveValue::boolean(true), span(242))?;
                let result = b.conditional(
                    condition,
                    &[input],
                    vec![tensor.clone(), tensor],
                    span(243),
                    |r, inputs, outputs| {
                        let original = r.load(inputs[0], span(244))?;
                        let index =
                            r.constant(solve::SolveValue::integer(p, 1).unwrap(), span(245))?;
                        let value = r.constant(solve::SolveValue::real(p, 99.0), span(246))?;
                        let changed = r.update_element(original, value, &[index], span(247))?;
                        r.store(outputs[0], changed, span(248))?;
                        let original_again = r.load(inputs[0], span(249))?;
                        r.store(outputs[1], original_again, span(250))
                    },
                    |r, inputs, outputs| {
                        let original = r.load(inputs[0], span(251))?;
                        r.store(outputs[0], original, span(252))?;
                        r.store(outputs[1], original, span(253))
                    },
                )?;
                b.store(outputs[0], result[0], span(254))?;
                b.store(outputs[1], result[1], span(255))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    let table = table.finish();
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let inputs = vec![vec![real(1.0), real(2.0), real(3.0), real(4.0)]];
    let (status, actual) = Runner::new(&compiled).run(&cells(inputs.iter().flatten().copied()));
    assert_eq!(status, 0);
    assert_eq!(actual, oracle(&table, &site, &inputs).unwrap());
}

#[test]
fn aggregate_carried_fold_identity_preserves_empty_and_nonempty_tuples() {
    let p = profile();
    let tensor = solve::SolveValueType::tensor(solve::SolveScalarType::real(p), vec![4]).unwrap();
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(24),
            vec![tensor.clone()],
            vec![solve::SolvePureCallOutput::result(tensor)],
            span(260),
            |b, inputs, outputs| {
                let input = b.load(inputs[0], span(261))?;
                let result = b.fold(
                    domain(1, 2, 1),
                    &[input],
                    &[],
                    span(262),
                    |r, carried, _, _, outputs| {
                        let old = r.load(carried[0], span(263))?;
                        r.store(outputs[0], old, span(264))
                    },
                )?;
                b.store(outputs[0], result[0], span(265))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    let table = table.finish();
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let inputs = vec![vec![real(-0.0), real(2.0), real(3.0), real(4.0)]];
    let (status, actual) = Runner::new(&compiled).run(&cells(inputs.iter().flatten().copied()));
    assert_eq!(status, 0);
    assert_eq!(actual, oracle(&table, &site, &inputs).unwrap());
}
