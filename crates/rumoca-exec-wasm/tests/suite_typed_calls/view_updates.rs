//! Checked view writes retain aggregate/value bits and source-bound faults.
use super::*;

#[derive(Clone, Copy)]
enum Axis {
    Index(usize),
    Span(u32, u32),
}

fn table(
    scalar: solve::SolveScalarType,
    shape: Vec<u32>,
    axes: &[Axis],
) -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let aggregate = solve::SolveValueType::tensor(scalar, shape).unwrap();
    let extents = axes
        .iter()
        .filter_map(|axis| match axis {
            Axis::Span(_, n) => Some(*n),
            _ => None,
        })
        .collect::<Vec<_>>();
    let value = solve::SolveValueType::tensor(scalar, extents).unwrap();
    let indices = axes
        .iter()
        .filter_map(|axis| match axis {
            Axis::Index(n) => Some(n + 1),
            _ => None,
        })
        .max()
        .unwrap_or(0);
    let mut inputs = vec![aggregate.clone(), value.clone()];
    inputs.extend(vec![
        solve::SolveValueType::scalar(
            solve::SolveScalarType::integer(profile())
        );
        indices
    ]);
    let mut builder = solve::SolvePureCallTable::builder(profile());
    let owner = builder
        .add_owner(
            identity(170),
            inputs,
            vec![
                solve::SolvePureCallOutput::result(aggregate.clone()),
                solve::SolvePureCallOutput::result(aggregate),
                solve::SolvePureCallOutput::result(value),
            ],
            span(17000),
            |b, inputs, outputs| {
                let original = b.load(inputs[0], span(17001))?;
                let value = b.load(inputs[1], span(17002))?;
                let mut indexes = Vec::new();
                for slot in &inputs[2..] {
                    indexes.push(b.load(*slot, span(17003))?);
                }
                let axes = axes
                    .iter()
                    .map(|axis| match *axis {
                        Axis::Index(n) => solve::ProgramTensorViewAxis::Index(indexes[n]),
                        Axis::Span(origin, extent) => {
                            solve::ProgramTensorViewAxis::Span { origin, extent }
                        }
                    })
                    .collect::<Vec<_>>();
                let updated = b.update_view(original, value, &axes, span(17004))?;
                b.store(outputs[0], updated, span(17005))?;
                b.store(outputs[1], original, span(17006))?;
                b.store(outputs[2], value, span(17007))
            },
        )
        .unwrap();
    let site = builder.call_site(owner).unwrap();
    (builder.finish(), site)
}

fn literal(
    original: &[solve::SolveValueKind],
    value: &[solve::SolveValueKind],
    shape: &[u32],
    axes: &[Axis],
    indexes: &[i64],
) -> Vec<solve::SolveValueKind> {
    let mut output = original.to_vec();
    for (ordinal, element) in value.iter().enumerate() {
        let mut remainder = ordinal;
        let mut offset = 0;
        let mut stride = 1;
        for axis in (0..shape.len()).rev() {
            let coordinate = match axes[axis] {
                Axis::Index(n) => indexes[n] as usize - 1,
                Axis::Span(origin, extent) => {
                    let coordinate = origin as usize + remainder % extent as usize;
                    remainder /= extent as usize;
                    coordinate
                }
            };
            offset += coordinate * stride;
            stride *= shape[axis] as usize;
        }
        output[offset] = *element;
    }
    output
}

#[test]
fn update_view_static_and_dropped_axes_match_independent_bits_and_canonical() {
    for (shape, axes, indexes) in [
        (vec![4, 5], vec![Axis::Span(1, 2), Axis::Span(2, 3)], vec![]),
        (
            vec![3, 4, 5],
            vec![Axis::Index(0), Axis::Span(1, 2), Axis::Span(2, 3)],
            vec![2],
        ),
        (
            vec![2, 3, 4, 5],
            vec![
                Axis::Index(0),
                Axis::Span(1, 2),
                Axis::Index(1),
                Axis::Span(1, 3),
            ],
            vec![2, 3],
        ),
        (
            vec![120, 120],
            vec![Axis::Span(0, 120), Axis::Span(0, 120)],
            vec![],
        ),
    ] {
        let bits = [
            0.,
            -0.,
            f64::from_bits(0x7ff8_1234_5678_abcd),
            f64::INFINITY,
            f64::NEG_INFINITY,
            1.25,
        ];
        let aggregate = (0..shape.iter().map(|n| *n as usize).product())
            .map(|i| real(bits[i % 6]))
            .collect::<Vec<_>>();
        let count = axes
            .iter()
            .filter_map(|a| match a {
                Axis::Span(_, n) => Some(*n as usize),
                _ => None,
            })
            .product::<usize>();
        let value = (0..count)
            .map(|i| real(bits[(i + 3) % 6]))
            .collect::<Vec<_>>();
        let mut expected = literal(&aggregate, &value, &shape, &axes, &indexes);
        expected.extend_from_slice(&aggregate);
        expected.extend_from_slice(&value);
        let (table, site) = table(solve::SolveScalarType::real(profile()), shape, &axes);
        let compiled = compile_pure_call_wasm(&table, &site).unwrap();
        let mut input = vec![aggregate, value];
        input.extend(
            indexes
                .into_iter()
                .map(|index| vec![solve::SolveValueKind::Integer(index)]),
        );
        let bytes = cells(input.iter().flatten().copied());
        let mut runner = Runner::new(&compiled);
        let (status, actual) = runner.run(&bytes);
        assert_eq!(status, 0);
        assert_eq!(actual, cells(expected));
        assert_eq!(actual, oracle(&table, &site, &input).unwrap());
        let mut retained = vec![0; bytes.len()];
        runner.memory.read(&runner.store, 0, &mut retained).unwrap();
        assert_eq!(retained, bytes);
        assert!(compiled.module_bytes().len() < 4096);
    }
}

#[test]
fn update_view_integer_boolean_cells_are_exact() {
    for (scalar, aggregate, value) in [
        (
            solve::SolveScalarType::integer(profile()),
            vec![i64::MIN, -1, 0, 1, i64::MAX, 42]
                .into_iter()
                .map(solve::SolveValueKind::Integer)
                .collect::<Vec<_>>(),
            vec![i64::MAX, i64::MIN]
                .into_iter()
                .map(solve::SolveValueKind::Integer)
                .collect::<Vec<_>>(),
        ),
        (
            solve::SolveScalarType::Boolean,
            vec![false, true, false, true, false, true]
                .into_iter()
                .map(solve::SolveValueKind::Boolean)
                .collect(),
            vec![true, false]
                .into_iter()
                .map(solve::SolveValueKind::Boolean)
                .collect(),
        ),
    ] {
        let (table, site) = table(scalar, vec![2, 3], &[Axis::Index(0), Axis::Span(1, 2)]);
        let compiled = compile_pure_call_wasm(&table, &site).unwrap();
        let inputs = [aggregate, value, vec![solve::SolveValueKind::Integer(2)]];
        let (status, actual) = Runner::new(&compiled).run(&cells(inputs.iter().flatten().copied()));
        assert_eq!(status, 0);
        assert_eq!(actual, oracle(&table, &site, &inputs).unwrap());
    }
}

#[test]
fn update_view_index_fault_is_atomic_source_bound_and_recovers() {
    let (table, site) = table(
        solve::SolveScalarType::real(profile()),
        vec![2, 3, 4],
        &[Axis::Index(0), Axis::Span(1, 2), Axis::Index(1)],
    );
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let mut runner = Runner::new(&compiled);
    for indexes in [
        [0, 1],
        [-1, 1],
        [3, 1],
        [i64::MIN, 1],
        [1, 0],
        [1, -1],
        [1, 5],
        [1, i64::MAX],
    ] {
        let inputs = [
            (0..24).map(|i| real(i as f64)).collect::<Vec<_>>(),
            vec![real(-0.), real(42.)],
            vec![solve::SolveValueKind::Integer(indexes[0])],
            vec![solve::SolveValueKind::Integer(indexes[1])],
        ];
        let bytes = cells(inputs.iter().flatten().copied());
        let (status, actual) = runner.run(&bytes);
        assert!(status > 0);
        assert_eq!(actual, vec![0xa5; actual.len()]);
        let fault = compiled
            .faults()
            .iter()
            .find(|f| f.status == status as u32)
            .unwrap();
        assert_eq!(fault.kind, TypedCallFaultKind::IndexBounds);
        assert_eq!(fault.opcode, "update_view");
        assert_eq!(fault.provenance, span(17004));
        assert!(oracle(&table, &site, &inputs).is_err());
        let mut retained = vec![0; bytes.len()];
        runner.memory.read(&runner.store, 0, &mut retained).unwrap();
        assert_eq!(retained, bytes);
    }
    let valid = [
        (0..24).map(|i| real(i as f64)).collect::<Vec<_>>(),
        vec![real(-0.), real(42.)],
        vec![solve::SolveValueKind::Integer(2)],
        vec![solve::SolveValueKind::Integer(4)],
    ];
    let (status, actual) = runner.run(&cells(valid.iter().flatten().copied()));
    assert_eq!(status, 0);
    assert_eq!(actual, oracle(&table, &site, &valid).unwrap());
}

#[test]
fn update_view_constructor_preserves_rank_bounds_value_shape_and_index_type_refusals() {
    let scalar = solve::SolveScalarType::real(profile());
    for mode in 0..4 {
        let aggregate = solve::SolveValueType::tensor(scalar, vec![2, 3]).unwrap();
        let value = solve::SolveValueType::tensor(scalar, vec![2]).unwrap();
        let mut builder = solve::SolvePureCallTable::builder(profile());
        let result = builder.add_owner(
            identity(171),
            vec![
                aggregate.clone(),
                value,
                solve::SolveValueType::scalar(scalar),
            ],
            vec![solve::SolvePureCallOutput::result(aggregate)],
            span(17100),
            |b, inputs, outputs| {
                let original = b.load(inputs[0], span(17101))?;
                let value = b.load(inputs[1], span(17102))?;
                let index = b.load(inputs[2], span(17103))?;
                use solve::ProgramTensorViewAxis as A;
                let axes = match mode {
                    0 => vec![A::Span {
                        origin: 0,
                        extent: 2,
                    }],
                    1 => vec![
                        A::Span {
                            origin: 1,
                            extent: 2,
                        },
                        A::Span {
                            origin: 0,
                            extent: 3,
                        },
                    ],
                    2 => vec![
                        A::Span {
                            origin: 0,
                            extent: 2,
                        },
                        A::Span {
                            origin: 0,
                            extent: 3,
                        },
                    ],
                    _ => vec![
                        A::Index(index),
                        A::Span {
                            origin: 0,
                            extent: 2,
                        },
                    ],
                };
                let result = b.update_view(original, value, &axes, span(17104))?;
                b.store(outputs[0], result, span(17105))
            },
        );
        assert_eq!(
            result.unwrap_err(),
            solve::SolveProgramConstructionError::InvalidProjection {
                provenance: span(17104)
            }
        );
    }
}
