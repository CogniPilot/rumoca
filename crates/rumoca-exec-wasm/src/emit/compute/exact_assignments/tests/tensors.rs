//! Static tensor operations keep the element and term order of their counted
//! loops, whether their registers are issued memory cells or local homes.
use super::*;
use rumoca_ir_solve::TensorInputKind;

const Y: usize = 0;
const P: usize = 1024;
const OUT: usize = 4096;

fn load(dst_start: Reg, input: TensorInputKind, input_start: usize, count: usize) -> LinearOp {
    LinearOp::TensorLoad {
        dst_start,
        input,
        input_start,
        count,
        seed_start: None,
        lanes: 1,
    }
}

/// `out = ((A * v) x w + s * v) / (A * A)[row 0]`, with `A = y[0..9]`,
/// `v = p[0..3]`, `w = p[3..6]` and the broadcast scalar `s = p[6]`.
fn program() -> Vec<LinearOp> {
    vec![
        load(0, TensorInputKind::Y, 0, 9),
        load(9, TensorInputKind::P, 0, 3),
        load(12, TensorInputKind::P, 3, 3),
        LinearOp::LoadP { dst: 15, index: 6 },
        LinearOp::MatrixMultiply {
            dst_start: 16,
            lhs_start: 0,
            rhs_start: 9,
            rows: 3,
            inner: 3,
            columns: 1,
            lanes: 1,
        },
        LinearOp::TensorCross {
            dst_start: 19,
            lhs_start: 16,
            rhs_start: 12,
            lanes: 1,
        },
        LinearOp::TensorBinary {
            dst_start: 22,
            op: BinaryOp::Mul,
            lhs_start: 15,
            rhs_start: 9,
            count: 3,
            lhs_stride: 0,
            rhs_stride: 1,
            lanes: 1,
        },
        LinearOp::TensorBinary {
            dst_start: 25,
            op: BinaryOp::Add,
            lhs_start: 19,
            rhs_start: 22,
            count: 3,
            lhs_stride: 1,
            rhs_stride: 1,
            lanes: 1,
        },
        LinearOp::MatrixMultiply {
            dst_start: 28,
            lhs_start: 0,
            rhs_start: 0,
            rows: 3,
            inner: 3,
            columns: 3,
            lanes: 1,
        },
        LinearOp::TensorBinary {
            dst_start: 37,
            op: BinaryOp::Div,
            lhs_start: 25,
            rhs_start: 28,
            count: 3,
            lhs_stride: 1,
            rhs_stride: 1,
            lanes: 1,
        },
        LinearOp::StoreOutputRange {
            start: 37,
            count: 3,
            stride: 1,
        },
    ]
}

/// One input set: `A`, `v`, `w` and the broadcast scalar.
type Case = ([f64; 9], [f64; 3], [f64; 3], f64);

fn reference(a: [f64; 9], v: [f64; 3], w: [f64; 3], s: f64) -> [u64; 3] {
    let dot = |row: usize, column: [f64; 3]| {
        a[row * 3] * column[0] + a[row * 3 + 1] * column[1] + a[row * 3 + 2] * column[2]
    };
    let av = [dot(0, v), dot(1, v), dot(2, v)];
    let cross = [
        av[1] * w[2] - av[2] * w[1],
        av[2] * w[0] - av[0] * w[2],
        av[0] * w[1] - av[1] * w[0],
    ];
    std::array::from_fn(|i| {
        let sum = cross[i] + s * v[i];
        (sum / dot(0, [a[i], a[3 + i], a[6 + i]])).to_bits()
    })
}

fn artifact() -> CompiledNativeCallProgramWasm {
    artifact_of(program())
}

fn artifact_of(program: Vec<LinearOp>) -> CompiledNativeCallProgramWasm {
    artifact_with(program, 9, 7)
}

fn artifact_with(program: Vec<LinearOp>, y: usize, p: usize) -> CompiledNativeCallProgramWasm {
    let layout = VarLayout::from_parts(Default::default(), y, p);
    let span = solve::source_span_from_offsets(11, 20, 30);
    let block = solve::ScalarProgramBlock::with_program_spans(vec![program], vec![span]).unwrap();
    let arithmetic = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    );
    let calls = solve::SolvePureCallTable::builder(arithmetic).finish();
    emit_module(
        Ready::private(&block, &layout, &calls).unwrap(),
        &layout,
        true,
        &[],
    )
    .unwrap()
}

#[test]
fn static_tensor_arithmetic_keeps_loop_order_and_ieee_bits() {
    let artifact = artifact();
    let mut runner = Runner::new();
    let call = runner.instance(&artifact);
    let nan = f64::from_bits(0x7ff8_0123_4567_89ab);
    let cases: [Case; 4] = [
        (
            [1., 2., 3., 4., 5., 6., 7., 8., 10.],
            [0.5, -1.5, 2.25],
            [3., -2., 0.125],
            -0.75,
        ),
        // Signed zeros: a first term that is its own sum must stay -0.0.
        (
            [-0., 0., 0., 0., -0., 0., 0., 0., -0.],
            [1., 1., 1.],
            [-0., 0., -0.],
            -0.,
        ),
        // Cancellation and rounding depend on the term order.
        (
            [1e16, 1., -1e16, 3., 1e-16, 3., 1e16, -1e16, 1.],
            [1., 1., 1.],
            [1e-3, 1e3, 1.],
            f64::from_bits(1),
        ),
        (
            [1., f64::INFINITY, 0., nan, 1., 2., f64::MAX, f64::MAX, -1.],
            [0., 2., -3.],
            [1., 1., 1.],
            2.,
        ),
    ];
    for (a, v, w, s) in cases {
        runner.write(Y, &a);
        runner.write(P, &[v[0], v[1], v[2], w[0], w[1], w[2], s]);
        runner.write(OUT, &[77.; 3]);
        assert_eq!(runner.call_at(call, Y, P, OUT), 0);
        assert_eq!(runner.read(OUT, 3), reference(a, v, w, s));
        assert_eq!(runner.read(Y, 9), a.map(f64::to_bits));
    }
}

/// Loops and private-memory accesses in the emitted code.
fn private_traffic(artifact: &CompiledNativeCallProgramWasm) -> (usize, usize) {
    use wasmparser::Operator;
    let (mut loops, mut accesses) = (0, 0);
    for payload in wasmparser::Parser::new(0).parse_all(artifact.module_bytes()) {
        let wasmparser::Payload::CodeSectionEntry(body) = payload.unwrap() else {
            continue;
        };
        for operator in body.get_operators_reader().unwrap() {
            match operator.unwrap() {
                Operator::Loop { .. } => loops += 1,
                Operator::F64Load { memarg } | Operator::F64Store { memarg }
                    if memarg.memory == 1 =>
                {
                    accesses += 1
                }
                Operator::MemoryCopy { .. } => accesses += 1,
                _ => {}
            }
        }
    }
    (loops, accesses)
}

#[cfg(not(feature = "diagnostic-root-scalar-locals"))]
#[test]
fn default_placement_keeps_static_tensors_in_private_memory() {
    let (loops, accesses) = private_traffic(&artifact());
    assert!(
        loops >= 5 && accesses >= 20,
        "{loops} loops, {accesses} accesses"
    );
}

#[cfg(feature = "diagnostic-root-scalar-locals")]
#[test]
fn issued_local_homes_print_static_tensors_without_loops_or_private_memory() {
    assert_eq!(private_traffic(&artifact()), (0, 0));
}

/// `[transpose(A) * v, w]` then `[A | v]` along the second axis.
fn layout_program() -> Vec<LinearOp> {
    let source = |start: Reg, dimensions: &[u32]| solve::TensorConcatenateSource {
        start,
        dimensions: dimensions.into(),
    };
    vec![
        load(0, TensorInputKind::Y, 0, 9),
        load(9, TensorInputKind::P, 0, 3),
        load(12, TensorInputKind::P, 3, 3),
        LinearOp::TensorTranspose {
            dst_start: 15,
            src_start: 0,
            rows: 3,
            columns: 3,
            element_width: 1,
            lanes: 1,
        },
        LinearOp::MatrixMultiply {
            dst_start: 24,
            lhs_start: 15,
            rhs_start: 9,
            rows: 3,
            inner: 3,
            columns: 1,
            lanes: 1,
        },
        LinearOp::TensorConcatenate {
            dst_start: 27,
            sources: [source(24, &[3]), source(12, &[3])].into(),
            dimensions: [6].into(),
            axis: 0,
            lanes: 1,
        },
        LinearOp::TensorConcatenate {
            dst_start: 33,
            sources: [source(0, &[3, 3]), source(9, &[3, 1])].into(),
            dimensions: [3, 4].into(),
            axis: 1,
            lanes: 1,
        },
        LinearOp::StoreOutputRange {
            start: 27,
            count: 6,
            stride: 1,
        },
        LinearOp::StoreOutputRange {
            start: 33,
            count: 12,
            stride: 1,
        },
    ]
}

fn layout_reference(a: [f64; 9], v: [f64; 3], w: [f64; 3]) -> Vec<u64> {
    let mut out = Vec::new();
    for column in 0..3 {
        out.push(a[column] * v[0] + a[3 + column] * v[1] + a[6 + column] * v[2]);
    }
    out.extend(w);
    for row in 0..3 {
        out.extend_from_slice(&a[row * 3..row * 3 + 3]);
        out.push(v[row]);
    }
    out.into_iter().map(f64::to_bits).collect()
}

#[test]
fn static_tensor_layouts_keep_loop_order_and_ieee_bits() {
    let artifact = artifact_of(layout_program());
    let mut runner = Runner::new();
    let call = runner.instance(&artifact);
    let nan = f64::from_bits(0x7ff8_0123_4567_89ab);
    let cases: [([f64; 9], [f64; 3], [f64; 3]); 2] = [
        (
            [1., 2., 3., 4., 5., 6., 7., 8., 10.],
            [0.5, -1.5, 2.25],
            [3., -2., 0.125],
        ),
        (
            [-0., 1e16, nan, 0., 1., -1e16, -0., f64::INFINITY, 1.],
            [-0., 1., 1.],
            [-0., f64::from_bits(1), f64::MAX],
        ),
    ];
    for (a, v, w) in cases {
        runner.write(Y, &a);
        runner.write(P, &[v[0], v[1], v[2], w[0], w[1], w[2], 0.]);
        runner.write(OUT, &[77.; 18]);
        assert_eq!(runner.call_at(call, Y, P, OUT), 0);
        assert_eq!(runner.read(OUT, 18), layout_reference(a, v, w));
    }
    #[cfg(feature = "diagnostic-root-scalar-locals")]
    assert_eq!(private_traffic(&artifact), (0, 0));
}

/// A scalar broadcast over a run wider than the issued extent: the scalar
/// keeps its memory home, so the operation keeps its counted loop.
#[test]
fn an_operation_wider_than_the_issued_extent_keeps_its_counted_loop() {
    const WIDE: usize = 40;
    let program = vec![
        LinearOp::LoadP { dst: 0, index: 0 },
        load(1, TensorInputKind::P, 1, WIDE),
        LinearOp::TensorBinary {
            dst_start: 41,
            op: BinaryOp::Mul,
            lhs_start: 0,
            rhs_start: 1,
            count: WIDE,
            lhs_stride: 0,
            rhs_stride: 1,
            lanes: 1,
        },
        LinearOp::StoreOutputRange {
            start: 41,
            count: WIDE,
            stride: 1,
        },
    ];
    let artifact = artifact_with(program, 1, WIDE + 1);
    assert!(private_traffic(&artifact).0 >= 1);
    let mut runner = Runner::new();
    let call = runner.instance(&artifact);
    let mut p = vec![-0.75];
    p.extend((0..WIDE).map(|index| index as f64 - 3.5));
    runner.write(P, &p);
    runner.write(OUT, &[77.; WIDE]);
    assert_eq!(runner.call_at(call, Y, P, OUT), 0);
    let expected: Vec<u64> = p[1..]
        .iter()
        .map(|value| (p[0] * value).to_bits())
        .collect();
    assert_eq!(runner.read(OUT, WIDE), expected);
}
