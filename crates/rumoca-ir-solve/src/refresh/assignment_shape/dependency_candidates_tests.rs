use super::*;

// Independent small-program oracle: enumerate every prefix Y load, including
// loads unrelated to the selected output. Production prunes using its checked
// output-reachable complete operation ownership instead.
fn exhaustive_shapes(program: &[LinearOp]) -> Vec<(usize, TargetAssignmentShape)> {
    let mut shapes = Vec::new();
    for (offset, (output, position)) in store_output_registers(program).enumerate() {
        let prefix = &program[..position];
        let Some(producers) = UniqueProgram::new(prefix) else {
            continue;
        };
        let dependencies = ScalarProgramYDependency::new(prefix);
        for target in y_load_indices(prefix) {
            if let Some(shape) =
                canonical_assignment_shape(producers.view(), output, target, &dependencies)
            {
                shapes.push((offset, shape));
            }
        }
    }
    let mut queries = crate::CanonicalAssignmentQueries::new(program);
    for (offset, (_, position)) in store_output_registers(program).enumerate() {
        assert_eq!(
            queries.has_any(offset),
            shapes.iter().any(|(output, _)| *output == offset)
        );
        for target in y_load_indices(&program[..position])
            .into_iter()
            .chain([9999])
        {
            let expected = shapes
                .iter()
                .find(|(output, shape)| *output == offset && shape.target_y_index() == target)
                .map(|(_, shape)| shape.clone());
            assert_eq!(
                queries.derive(offset, target),
                expected,
                "output{offset}/target{target}"
            );
        }
    }
    shapes
}

fn cross_program() -> Vec<LinearOp> {
    vec![
        LinearOp::TensorLoad {
            dst_start: 0,
            input: crate::TensorInputKind::P,
            input_start: 0,
            seed_start: None,
            count: 3,
            lanes: 1,
        },
        LinearOp::TensorLoad {
            dst_start: 3,
            input: crate::TensorInputKind::Y,
            input_start: 0,
            seed_start: None,
            count: 3,
            lanes: 1,
        },
        LinearOp::TensorLoad {
            dst_start: 6,
            input: crate::TensorInputKind::Y,
            input_start: 3,
            seed_start: None,
            count: 3,
            lanes: 1,
        },
        LinearOp::TensorCross {
            dst_start: 9,
            lhs_start: 0,
            rhs_start: 3,
            lanes: 1,
        },
        LinearOp::TensorBinary {
            dst_start: 12,
            op: BinaryOp::Sub,
            lhs_start: 6,
            rhs_start: 9,
            count: 3,
            lhs_stride: 1,
            rhs_stride: 1,
            lanes: 1,
        },
        LinearOp::StoreOutputRange {
            start: 12,
            count: 3,
            stride: 1,
        },
    ]
}

#[test]
fn output_dependency_candidates_match_exhaustive_tensor_isolation() {
    let program = cross_program();
    let expected = exhaustive_shapes(&program);
    assert!(!expected.is_empty());
    assert_eq!(derive_target_assignment_shapes(&program), expected);
    for (output, shape) in &expected {
        assert_eq!(
            derive_target_assignment_shape_for_output(&program, *output, shape.target_y_index()),
            Some(shape.clone())
        );
    }
}

#[test]
fn repeated_stores_keep_exact_prefix_and_signed_zero_arithmetic() {
    for scale in [0.0, -0.0, 1.0, -1.0] {
        let mut program = cross_program();
        program.push(LinearOp::Const {
            dst: 15,
            value: scale,
        });
        program.push(LinearOp::Binary {
            dst: 16,
            op: BinaryOp::Mul,
            lhs: 12,
            rhs: 15,
        });
        program.push(LinearOp::StoreOutput { src: 16 });
        program.push(LinearOp::Const { dst: 3, value: 8.0 });
        program.push(LinearOp::StoreOutput { src: 12 });
        let actual = derive_target_assignment_shapes(&program);
        assert_eq!(actual, exhaustive_shapes(&program));
        assert!(actual.iter().all(|(output, _)| *output < 4));
    }
}

#[test]
fn unrelated_tensor_loads_are_not_output_candidates() {
    let mut program = cross_program();
    program.insert(
        0,
        LinearOp::TensorLoad {
            dst_start: 100,
            input: crate::TensorInputKind::Y,
            input_start: 100,
            seed_start: None,
            count: 512,
            lanes: 1,
        },
    );
    let prefix = &program[..program.len() - 1];
    let producers = UniqueProgram::new(prefix).unwrap();
    assert_eq!(y_load_indices(prefix).len(), 518);
    assert_eq!(
        dependency_candidates::derive(producers.view(), 12)
            .unwrap()
            .targets
            .len(),
        6
    );
    assert_eq!(
        derive_target_assignment_shapes(&program),
        exhaustive_shapes(&program)
    );
}

#[test]
fn unsupported_projection_operations_are_opaque_to_candidates() {
    let program = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadY { dst: 1, index: 1 },
        LinearOp::LoadY { dst: 2, index: 100 },
        LinearOp::Unary {
            dst: 3,
            op: UnaryOp::Sqrt,
            arg: 0,
        },
        LinearOp::Binary {
            dst: 4,
            op: BinaryOp::Sub,
            lhs: 1,
            rhs: 3,
        },
        LinearOp::StoreOutput { src: 4 },
    ];
    let producers = UniqueProgram::new(&program[..5]).unwrap();
    let candidates = dependency_candidates::derive(producers.view(), 4).unwrap();
    assert_eq!(candidates.targets, BTreeSet::from([1]));
    assert!(candidates.linear);
    assert_eq!(
        derive_target_assignment_shapes(&program),
        exhaustive_shapes(&program)
    );
}

#[test]
fn aggregate_matrix_candidates_keep_whole_operation_certificates() {
    for size in 1..=3 {
        let count = size * size;
        let register_count = count as u32;
        let program = vec![
            LinearOp::TensorLoad {
                dst_start: 0,
                input: crate::TensorInputKind::Y,
                input_start: 0,
                seed_start: None,
                count,
                lanes: 1,
            },
            LinearOp::TensorLoad {
                dst_start: register_count,
                input: crate::TensorInputKind::P,
                input_start: 0,
                seed_start: None,
                count,
                lanes: 1,
            },
            LinearOp::MatrixMultiply {
                dst_start: register_count * 2,
                lhs_start: 0,
                rhs_start: register_count,
                rows: size,
                inner: size,
                columns: size,
                lanes: 1,
            },
            LinearOp::StoreOutputRange {
                start: register_count * 2,
                count,
                stride: 1,
            },
        ];
        let actual = derive_target_assignment_shapes(&program);
        assert!(!actual.is_empty());
        assert_eq!(actual, exhaustive_shapes(&program));
    }
}

/// The lanes of one wide residual each isolate their own target: the walk is
/// shared by the lanes and the candidates follow each lane's dependencies, so
/// the inventory still matches the exhaustive oracle.
#[test]
fn wide_lane_residuals_keep_the_exhaustive_inventory() {
    let count = 300;
    let lanes = |dst_start, input_start| LinearOp::TensorLoad {
        dst_start,
        input: crate::TensorInputKind::Y,
        input_start,
        seed_start: None,
        count,
        lanes: 1,
    };
    let registers = u32::try_from(count).unwrap();
    let program = vec![
        lanes(0, 0),
        lanes(registers, count),
        LinearOp::TensorBinary {
            dst_start: registers * 2,
            op: BinaryOp::Sub,
            lhs_start: 0,
            rhs_start: registers,
            count,
            lhs_stride: 1,
            rhs_stride: 1,
            lanes: 1,
        },
        LinearOp::StoreOutputRange {
            start: registers * 2,
            count,
            stride: 1,
        },
    ];
    let actual = derive_target_assignment_shapes(&program);
    assert_eq!(actual.len(), count * 2);
    assert_eq!(actual, exhaustive_shapes(&program));
}

/// The rule that isolated a shape, so the oracle compares the winning rule
/// as well as the certificate.
fn rule(shape: &TargetAssignmentShape) -> &'static str {
    match shape {
        TargetAssignmentShape::Zero { .. } => "zero",
        TargetAssignmentShape::Direct { .. } => "direct",
        TargetAssignmentShape::Affine { .. } => "affine",
        TargetAssignmentShape::Additive { .. } => "additive",
        TargetAssignmentShape::Reciprocal { .. } => "reciprocal",
        TargetAssignmentShape::TensorAffine { .. } => "tensor-affine",
    }
}

fn rules(shapes: &[(usize, TargetAssignmentShape)]) -> Vec<(usize, usize, &'static str)> {
    shapes
        .iter()
        .map(|(output, shape)| (*output, shape.target_y_index(), rule(shape)))
        .collect()
}

/// Deterministic xorshift stream, so failures replay.
struct Stream(u64);

impl Stream {
    fn next(&mut self, bound: usize) -> usize {
        self.0 ^= self.0 << 13;
        self.0 ^= self.0 >> 7;
        self.0 ^= self.0 << 17;
        (self.0 % bound as u64) as usize
    }
}

/// A random SSA program over scalar and tensor operations of a few extents,
/// ending in range and scalar stores: the vocabulary every rule reads.
fn random_program(stream: &mut Stream, powers: bool) -> Vec<LinearOp> {
    let mut program = Vec::new();
    // (start, count) of every defined value, scalars with count 1.
    let mut values: Vec<(u32, usize)> = Vec::new();
    let push =
        |program: &mut Vec<LinearOp>, values: &mut Vec<(u32, usize)>, op: LinearOp, count| {
            values.push((op.dst_register().unwrap(), count));
            program.push(op);
        };
    for _ in 0..2 + stream.next(3) {
        let next = next_register(&values);
        let count = 1 + stream.next(4);
        let input = if stream.next(3) == 0 {
            crate::TensorInputKind::P
        } else {
            crate::TensorInputKind::Y
        };
        let op = LinearOp::TensorLoad {
            dst_start: next,
            input,
            input_start: stream.next(6),
            seed_start: None,
            count,
            lanes: 1,
        };
        push(&mut program, &mut values, op, count);
    }
    for _ in 0..3 + stream.next(8) {
        if let Some((op, count)) = random_operation(stream, &values, powers) {
            push(&mut program, &mut values, op, count);
        }
    }
    for _ in 0..1 + stream.next(2) {
        let (start, count) = values[values.len() - 1 - stream.next(values.len().min(3))];
        program.push(if count == 1 {
            LinearOp::StoreOutput { src: start }
        } else {
            LinearOp::StoreOutputRange {
                start,
                count,
                stride: 1,
            }
        });
    }
    program
}

/// One random scalar or elementwise operation over the defined `values`, or
/// `None` for an elementwise pair of mismatched extents.
fn random_operation(
    stream: &mut Stream,
    values: &[(u32, usize)],
    powers: bool,
) -> Option<(LinearOp, usize)> {
    let next = next_register(values);
    let op_kind = [BinaryOp::Add, BinaryOp::Sub, BinaryOp::Mul, BinaryOp::Div][stream.next(4)];
    let (lhs, lhs_count) = values[stream.next(values.len())];
    let (rhs, rhs_count) = values[stream.next(values.len())];
    let scalar = |start: u32, count: usize, stream: &mut Stream| {
        start + u32::try_from(stream.next(count)).unwrap()
    };
    // Without powers the draws are those of the original corpus.
    match stream.next(if powers { 7 } else { 6 }) {
        0 => {
            let value = [0.0, 2.0, -1.0, 0.5][stream.next(4)];
            Some((LinearOp::Const { dst: next, value }, 1))
        }
        1 => {
            let index = stream.next(8);
            Some((LinearOp::LoadY { dst: next, index }, 1))
        }
        2 => {
            let arg = scalar(lhs, lhs_count, stream);
            let op = LinearOp::Unary {
                dst: next,
                op: UnaryOp::Neg,
                arg,
            };
            Some((op, 1))
        }
        3 => {
            let op = LinearOp::Binary {
                dst: next,
                op: op_kind,
                lhs: scalar(lhs, lhs_count, stream),
                rhs: scalar(rhs, rhs_count, stream),
            };
            Some((op, 1))
        }
        4 if powers => {
            // An elementwise literal power of one value, positive or negative.
            let exponent = [2, 3, 7, -2, -3][stream.next(5)];
            let op = LinearOp::TensorIntegerPower {
                dst_start: next,
                src_start: lhs,
                count: lhs_count,
                power: crate::IntegerPower::new(exponent).expect("exponent has a product"),
                lanes: 1,
            };
            Some((op, lhs_count))
        }
        _ => {
            // An elementwise operation: equal extents, or a broadcast scalar.
            let count = lhs_count.max(rhs_count);
            let stride = |operand_count: usize| usize::from(operand_count == count);
            if (lhs_count != count && lhs_count != 1) || (rhs_count != count && rhs_count != 1) {
                return None;
            }
            let op = LinearOp::TensorBinary {
                dst_start: next,
                op: op_kind,
                lhs_start: lhs,
                rhs_start: rhs,
                count,
                lhs_stride: stride(lhs_count),
                rhs_stride: stride(rhs_count),
                lanes: 1,
            };
            Some((op, count))
        }
    }
}

/// Whether a later operation or store of `program` reads a register some
/// `TensorIntegerPower` defines.
fn reads_a_power(program: &[LinearOp]) -> bool {
    let ranges: Vec<_> = program
        .iter()
        .filter_map(|op| match op {
            LinearOp::TensorIntegerPower {
                dst_start, count, ..
            } => Some(*dst_start..*dst_start + u32::try_from(*count).unwrap()),
            _ => None,
        })
        .collect();
    let read = |register: u32, count: usize| {
        let end = register + u32::try_from(count).unwrap();
        ranges
            .iter()
            .any(|range| register < range.end && range.start < end)
    };
    program.iter().any(|op| match *op {
        LinearOp::StoreOutput { src } | LinearOp::Unary { arg: src, .. } => read(src, 1),
        LinearOp::StoreOutputRange { start, count, .. } => read(start, count),
        LinearOp::Binary { lhs, rhs, .. } => read(lhs, 1) || read(rhs, 1),
        LinearOp::TensorBinary {
            lhs_start,
            rhs_start,
            count,
            lhs_stride,
            rhs_stride,
            ..
        } => {
            read(lhs_start, if lhs_stride == 0 { 1 } else { count })
                || read(rhs_start, if rhs_stride == 0 { 1 } else { count })
        }
        LinearOp::TensorIntegerPower {
            src_start, count, ..
        } => read(src_start, count),
        _ => false,
    })
}

fn next_register(values: &[(u32, usize)]) -> u32 {
    values
        .last()
        .map_or(0, |(start, count)| start + u32::try_from(*count).unwrap())
}

/// 400 programs of the original vocabulary, then 400 that also hold
/// elementwise literal powers.
fn random_corpus() -> Vec<Vec<LinearOp>> {
    let mut stream = Stream(0x9e37_79b9_7f4a_7c15);
    let mut corpus: Vec<_> = (0..400)
        .map(|_| random_program(&mut stream, false))
        .collect();
    let mut stream = Stream(0x2545_f491_4f6c_dd1d);
    corpus.extend((0..400).map(|_| random_program(&mut stream, true)));
    corpus
}

/// The inventory asks only the targets some rule can isolate, and still
/// answers every pair as the exhaustive oracle does, with the same winning
/// rule, on random programs.
#[test]
fn random_programs_keep_the_exhaustive_inventory_and_its_rules() {
    let mut certified = BTreeMap::new();
    let mut powers_read = 0usize;
    for program in random_corpus() {
        let expected = exhaustive_shapes(&program);
        let actual = derive_target_assignment_shapes(&program);
        assert_eq!(rules(&actual), rules(&expected), "{program:?}");
        assert_eq!(actual, expected, "{program:?}");
        for (_, shape) in &actual {
            *certified.entry(rule(shape)).or_insert(0usize) += 1;
        }
        powers_read += usize::from(reads_a_power(&program));
    }
    // Powers sit on the path of some stores, so the walk through them (the
    // non-linear `Kind::Power` projection) is compared with the oracle.
    assert!(powers_read >= 20, "{powers_read} programs read a power");
    // The corpus reaches every rule the lanes share, so the equivalence is
    // not vacuous.
    for kind in [
        "direct",
        "affine",
        "additive",
        "reciprocal",
        "tensor-affine",
    ] {
        assert!(
            certified.get(kind).is_some_and(|count| *count > 0),
            "{kind}: {certified:?}"
        );
    }
}

/// Each fault injected into the projections the lanes share changes the
/// inventory of some program, so the oracle above would catch it.
#[test]
fn a_fault_in_the_shared_projections_is_caught_by_the_oracle() {
    use tensor_affine::hooks::{Fault, with};
    let mut corpus = random_corpus();
    corpus.push(cross_program());
    for fault in [Fault::NoRebind, Fault::KeyByOperation, Fault::NoCandidates] {
        let caught = corpus.iter().any(|program| {
            let expected = exhaustive_shapes(program);
            with(fault, || derive_target_assignment_shapes(program)) != expected
        });
        assert!(caught, "{fault:?} escapes the exhaustive oracle");
    }
}

/// `x - s .* s .* s` over `count` lanes: each lane isolates its own `x`,
/// from among all the targets of the operation.
fn cubic_program(count: usize) -> Vec<LinearOp> {
    let registers = u32::try_from(count).unwrap();
    let load = |dst_start, input_start| LinearOp::TensorLoad {
        dst_start,
        input: crate::TensorInputKind::Y,
        input_start,
        seed_start: None,
        count,
        lanes: 1,
    };
    let binary = |dst_start, op, lhs_start, rhs_start| LinearOp::TensorBinary {
        dst_start,
        op,
        lhs_start,
        rhs_start,
        count,
        lhs_stride: 1,
        rhs_stride: 1,
        lanes: 1,
    };
    vec![
        load(0, 0),
        load(registers, count),
        binary(registers * 2, BinaryOp::Mul, 0, 0),
        binary(registers * 3, BinaryOp::Mul, registers * 2, 0),
        binary(registers * 4, BinaryOp::Sub, registers, registers * 3),
        LinearOp::StoreOutputRange {
            start: registers * 4,
            count,
            stride: 1,
        },
    ]
}

/// The work grows with the lanes, not with lanes times targets: from 20 to
/// 2000 lanes, the pairs asked, the projections derived, and the targets
/// scanned for an operation's candidates each grow by at most the lane ratio,
/// and the inventory still matches the oracle at the small extent. Without the
/// per-operation memo of candidates, every lane scans the operation's targets
/// again and the scan grows quadratically, which the same bound catches.
#[test]
fn the_inventory_work_grows_linearly_with_the_lanes() {
    use tensor_affine::hooks::{Counts, Fault, counted, with};
    let work = |count| {
        let (shapes, counts) = counted(|| derive_target_assignment_shapes(&cubic_program(count)));
        assert_eq!(shapes.len(), count);
        counts
    };
    let linear = |small: Counts, large: Counts| {
        large.asked <= small.asked * 100
            && large.derived <= small.derived * 100
            && large.scanned <= small.scanned * 100
    };
    assert_eq!(
        derive_target_assignment_shapes(&cubic_program(20)),
        exhaustive_shapes(&cubic_program(20))
    );
    let (small, large) = (work(20), work(2000));
    assert!(small.scanned > 0 && small.derived > 0, "{small:?}");
    assert!(
        linear(small, large),
        "{small:?} at 20 lanes, {large:?} at 2000"
    );
    let (small, large) = with(Fault::NoCertifiedMemo, || (work(20), work(2000)));
    assert!(
        !linear(small, large),
        "without the candidate memo: {small:?} at 20 lanes, {large:?} at 2000"
    );
}
