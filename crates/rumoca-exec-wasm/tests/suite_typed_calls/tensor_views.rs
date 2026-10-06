//! Actual WASM bit permutations versus independent indices and canonical values.
use super::*;

fn transpose_table(
    scalar: solve::SolveScalarType,
    shape: Vec<u32>,
) -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let input = solve::SolveValueType::tensor(scalar, shape.clone()).unwrap();
    let mut result_shape = shape;
    result_shape.swap(0, 1);
    let output = solve::SolveValueType::tensor(scalar, result_shape).unwrap();
    let mut table = solve::SolvePureCallTable::builder(profile());
    let owner = table
        .add_owner(
            identity(120),
            vec![input.clone()],
            vec![
                solve::SolvePureCallOutput::result(output),
                solve::SolvePureCallOutput::result(input),
            ],
            span(12000),
            |b, inputs, outputs| {
                let original = b.load(inputs[0], span(12001))?;
                let transposed = b.transpose(original, span(12002))?;
                b.store(outputs[0], transposed, span(12003))?;
                b.store(outputs[1], original, span(12004))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    (table.finish(), site)
}

fn literal_transpose(
    source: &[solve::SolveValueKind],
    shape: &[u32],
) -> Vec<solve::SolveValueKind> {
    let width = shape[2..].iter().map(|n| *n as usize).product::<usize>();
    let mut result = Vec::new();
    for column in 0..shape[1] as usize {
        for row in 0..shape[0] as usize {
            let first = (row * shape[1] as usize + column) * width;
            result.extend_from_slice(&source[first..first + width]);
        }
    }
    result
}

#[test]
fn transpose_full14400_and_trailing_axes_preserves_all_real_bits() {
    for shape in [vec![120, 120], vec![2, 3], vec![2, 3, 2], vec![2, 3, 2, 2]] {
        let count = shape.iter().map(|n| *n as usize).product::<usize>();
        let pattern = [
            0.,
            -0.,
            f64::from_bits(0x7ff8_1234_5678_abcd),
            f64::INFINITY,
            f64::NEG_INFINITY,
            1.25,
        ];
        let input = (0..count)
            .map(|i| real(pattern[i % pattern.len()]))
            .collect::<Vec<_>>();
        let (table, site) = transpose_table(solve::SolveScalarType::real(profile()), shape.clone());
        let compiled = compile_pure_call_wasm(&table, &site).unwrap();
        let (status, actual) = Runner::new(&compiled).run(&cells(input.iter().copied()));
        assert_eq!(status, 0);
        let mut expected = literal_transpose(&input, &shape);
        expected.extend_from_slice(&input);
        assert_eq!(actual, cells(expected));
        assert_eq!(actual, oracle(&table, &site, &[input]).unwrap());
        assert!(
            compiled.module_bytes().len() < 4096,
            "permutation body stays compact"
        );
    }
}

#[test]
fn transpose_integer_and_boolean_cells_preserves_typed_payloads() {
    for (scalar, input) in [
        (
            solve::SolveScalarType::integer(profile()),
            vec![i64::MIN, -1, 0, 1, i64::MAX, 42]
                .into_iter()
                .map(solve::SolveValueKind::Integer)
                .collect::<Vec<_>>(),
        ),
        (
            solve::SolveScalarType::Boolean,
            vec![true, false, false, true, true, false]
                .into_iter()
                .map(solve::SolveValueKind::Boolean)
                .collect(),
        ),
    ] {
        let (table, site) = transpose_table(scalar, vec![2, 3]);
        let compiled = compile_pure_call_wasm(&table, &site).unwrap();
        let actual = Runner::new(&compiled).run(&cells(input.iter().copied()));
        let mut expected = literal_transpose(&input, &[2, 3]);
        expected.extend_from_slice(&input);
        assert_eq!(actual, (0, cells(expected)));
        assert_eq!(actual.1, oracle(&table, &site, &[input]).unwrap());
    }
}

#[derive(Clone, Copy)]
enum Axis {
    Index(usize),
    Span(u32, u32),
}

fn view_table(
    shape: Vec<u32>,
    axes: &[Axis],
) -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let real = solve::SolveScalarType::real(profile());
    let source = solve::SolveValueType::tensor(real, shape).unwrap();
    let dimensions = axes
        .iter()
        .filter_map(|a| match a {
            Axis::Span(_, n) => Some(*n),
            _ => None,
        })
        .collect();
    let output = solve::SolveValueType::tensor(real, dimensions).unwrap();
    let index_count = axes
        .iter()
        .filter_map(|a| match a {
            Axis::Index(i) => Some(i + 1),
            _ => None,
        })
        .max()
        .unwrap_or(0);
    let mut inputs = vec![source.clone()];
    inputs.extend(vec![
        solve::SolveValueType::scalar(
            solve::SolveScalarType::integer(profile())
        );
        index_count
    ]);
    let mut table = solve::SolvePureCallTable::builder(profile());
    let owner = table
        .add_owner(
            identity(121),
            inputs,
            vec![
                solve::SolvePureCallOutput::result(output),
                solve::SolvePureCallOutput::result(source),
            ],
            span(12100),
            |b, inputs, outputs| {
                let original = b.load(inputs[0], span(12101))?;
                let indices = inputs[1..]
                    .iter()
                    .map(|slot| b.load(*slot, span(12102)))
                    .collect::<Result<Vec<_>, _>>()?;
                let axes = axes
                    .iter()
                    .map(|a| match *a {
                        Axis::Index(i) => solve::ProgramTensorViewAxis::Index(indices[i]),
                        Axis::Span(origin, extent) => {
                            solve::ProgramTensorViewAxis::Span { origin, extent }
                        }
                    })
                    .collect::<Vec<_>>();
                let view = b.project_view(original, &axes, span(12103))?;
                b.store(outputs[0], view, span(12104))?;
                b.store(outputs[1], original, span(12105))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    (table.finish(), site)
}

#[test]
fn project_view_rank_drop_nonzero_spans_and_old_source_snapshot() {
    let source = (0..120)
        .map(|i| real(if i == 0 { -0. } else { i as f64 }))
        .collect::<Vec<_>>();
    let axes = [
        Axis::Index(0),
        Axis::Span(1, 2),
        Axis::Index(1),
        Axis::Span(1, 3),
    ];
    let (table, site) = view_table(vec![2, 3, 4, 5], &axes);
    let inputs = vec![
        source.clone(),
        vec![solve::SolveValueKind::Integer(2)],
        vec![solve::SolveValueKind::Integer(3)],
    ];
    let mut expected = Vec::new();
    for j in 1..3 {
        for l in 1..4 {
            expected.push(source[((3 + j) * 4 + 2) * 5 + l]);
        }
    }
    expected.extend_from_slice(&source);
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let actual = Runner::new(&compiled).run(&cells(inputs.iter().flatten().copied()));
    assert_eq!(actual, (0, cells(expected)));
    assert_eq!(actual.1, oracle(&table, &site, &inputs).unwrap());
    // A static view still copies bits, including an exact NaN payload.
    let (table, site) = view_table(vec![2, 3], &[Axis::Span(0, 2), Axis::Span(1, 2)]);
    let input = vec![
        real(-0.),
        real(f64::from_bits(0x7ff8_abcd_0123_4567)),
        real(2.),
        real(3.),
        real(4.),
        real(5.),
    ];
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let mut expected = vec![input[1], input[2], input[4], input[5]];
    expected.extend_from_slice(&input);
    assert_eq!(
        Runner::new(&compiled).run(&cells(input.iter().copied())),
        (0, cells(expected))
    );
}

#[test]
fn project_view_bounds_fault_is_atomic_source_bound_and_recovers() {
    let (table, site) = view_table(
        vec![2, 3, 4],
        &[Axis::Index(0), Axis::Span(0, 3), Axis::Index(1)],
    );
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let mut runner = Runner::new(&compiled);
    let source = (0..24).map(|i| real(i as f64)).collect::<Vec<_>>();
    for (first, last) in [
        (2, 4),
        (0, 4),
        (-1, 4),
        (3, 4),
        (i64::MAX, 4),
        (2, 0),
        (2, 5),
        (2, i64::MIN),
        (2, 4),
    ] {
        let inputs = vec![
            source.clone(),
            vec![solve::SolveValueKind::Integer(first)],
            vec![solve::SolveValueKind::Integer(last)],
        ];
        let (status, actual) = runner.run(&cells(inputs.iter().flatten().copied()));
        match oracle(&table, &site, &inputs) {
            Ok(expected) => assert_eq!((status, actual), (0, expected)),
            Err(error) => {
                assert_eq!(actual, vec![0xa5; compiled.layout().output_bytes as usize]);
                let fault = compiled
                    .faults()
                    .iter()
                    .find(|f| f.status == status as u32)
                    .unwrap();
                assert_eq!(fault.kind, TypedCallFaultKind::IndexBounds);
                assert_eq!(fault.opcode, "project_view");
                assert_eq!(fault.provenance, span(12103));
                assert_eq!(error.source_span(), Some(fault.provenance));
            }
        }
    }
}

#[test]
fn project_view_in_unselected_region_does_not_fault_or_read_result() {
    let scalar = solve::SolveScalarType::real(profile());
    let matrix = solve::SolveValueType::tensor(scalar, vec![2, 3]).unwrap();
    let row = solve::SolveValueType::tensor(scalar, vec![3]).unwrap();
    let integer = solve::SolveValueType::scalar(solve::SolveScalarType::integer(profile()));
    let boolean = solve::SolveValueType::scalar(solve::SolveScalarType::Boolean);
    let mut table = solve::SolvePureCallTable::builder(profile());
    let owner = table
        .add_owner(
            identity(122),
            vec![boolean, matrix, integer, row.clone()],
            vec![solve::SolvePureCallOutput::result(row.clone())],
            span(12200),
            |b, inputs, outputs| {
                let condition = b.load(inputs[0], span(12201))?;
                let captures = inputs[1..]
                    .iter()
                    .map(|s| b.load(*s, span(12202)))
                    .collect::<Result<Vec<_>, _>>()?;
                let result = b.conditional(
                    condition,
                    &captures,
                    vec![row],
                    span(12203),
                    |r, inputs, outputs| {
                        let matrix = r.load(inputs[0], span(12204))?;
                        let index = r.load(inputs[1], span(12205))?;
                        let view = r.project_view(
                            matrix,
                            &[
                                solve::ProgramTensorViewAxis::Index(index),
                                solve::ProgramTensorViewAxis::Span {
                                    origin: 0,
                                    extent: 3,
                                },
                            ],
                            span(12206),
                        )?;
                        r.store(outputs[0], view, span(12207))
                    },
                    |r, inputs, outputs| {
                        let fallback = r.load(inputs[2], span(12208))?;
                        r.store(outputs[0], fallback, span(12209))
                    },
                )?;
                b.store(outputs[0], result[0], span(12210))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    let table = table.finish();
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let mut runner = Runner::new(&compiled);
    for condition in [false, true, false] {
        let inputs = vec![
            vec![solve::SolveValueKind::Boolean(condition)],
            (0..6).map(|i| real(i as f64)).collect(),
            vec![solve::SolveValueKind::Integer(3)],
            vec![real(-0.); 3],
        ];
        let (status, actual) = runner.run(&cells(inputs.iter().flatten().copied()));
        if condition {
            assert!(oracle(&table, &site, &inputs).is_err());
            assert_eq!(actual, vec![0xa5; 24]);
            let fault = compiled
                .faults()
                .iter()
                .find(|f| f.status == status as u32)
                .unwrap();
            assert_eq!(fault.provenance, span(12206));
            assert_eq!(fault.region_path, [(4, 0)]);
        } else {
            assert_eq!(
                (status, actual),
                (0, oracle(&table, &site, &inputs).unwrap())
            );
        }
    }
}

#[test]
fn transpose_oversized_frame_refuses_without_allocating_tensor_payload() {
    let (table, site) = transpose_table(
        solve::SolveScalarType::real(profile()),
        vec![500_000_000, 1],
    );
    assert!(matches!(
        compile_pure_call_wasm(&table, &site),
        Err(TypedCallCompileError::SizeLimit)
    ));
}
