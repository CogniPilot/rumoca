//! Actual CPU/WASM dense numerical products against the shared scalar oracle.
use rumoca_eval_solve::{PreparedScalarProgramBlock, RowEvalContext};
use rumoca_exec_wasm::{
    compile_expression_compute_block_wasm, compile_expression_compute_block_wasm_bytes,
};
use rumoca_ir_solve::*;
use wasmi::{Engine, Linker, Memory, MemoryType, Module, Store};
mod support;
use support::{assert_bits, execute, scalar_reference};

fn matrix_block(m: usize, k: usize, n: usize) -> (ComputeBlock, VarLayout, Vec<f64>, Vec<f64>) {
    let span = source_span_from_offsets(1, 0, 1);
    let provenance =
        PatternProvenance::derived(PatternDerivation::DependencyPropagation, span).unwrap();
    let lhs_count = m * k;
    let rhs_count = k * n;
    let node = ComputeNode::MatMul {
        lhs_ops: vec![LinearOp::TensorLoad {
            dst_start: 0,
            input: TensorInputKind::Y,
            input_start: 2,
            count: lhs_count,
            seed_start: None,
            lanes: 1,
        }],
        lhs_start: 0,
        rhs_ops: vec![LinearOp::TensorLoad {
            dst_start: lhs_count as Reg,
            input: TensorInputKind::P,
            input_start: 3,
            count: rhs_count,
            seed_start: None,
            lanes: 1,
        }],
        rhs_start: lhs_count as Reg,
        m,
        k,
        n,
        lhs_pattern: StructuralPattern::full(m, k, provenance).unwrap(),
        rhs_pattern: StructuralPattern::full(k, n, provenance).unwrap(),
        metadata: TensorNodeMetadata::default(),
        span,
    };
    let y = (0..lhs_count + 2)
        .map(|i| ((i * 13 % 37) as f64 - 18.) * 0.037)
        .collect();
    let p = (0..rhs_count + 3)
        .map(|i| ((i * 19 % 31) as f64 - 15.) * 0.019)
        .collect();
    (
        ComputeBlock { nodes: vec![node] },
        VarLayout::from_parts(Default::default(), lhs_count + 2, rhs_count + 3),
        y,
        p,
    )
}

fn check(block: &ComputeBlock, layout: &VarLayout, y: &[f64], p: &[f64]) -> usize {
    let kernel = compile_expression_compute_block_wasm(block, layout).unwrap();
    let expected = scalar_reference(block, y, p);
    assert_bits(
        &execute(kernel.module_bytes(), y, p, kernel.rows()),
        &expected,
    );
    kernel.module_bytes().len()
}

#[test]
fn covariance_shapes_use_compact_reductions_with_scalar_bit_parity() {
    let mut sizes = Vec::new();
    for (m, k, n) in [
        (15, 15, 15),
        (15, 15, 12),
        (15, 12, 15),
        (15, 6, 15),
        (6, 15, 6),
        (24, 24, 24),
    ] {
        let (block, layout, y, p) = matrix_block(m, k, n);
        let bytes = check(&block, &layout, &y, &p);
        eprintln!("compact product {m}x{k}x{n}: {bytes} bytes");
        assert!(
            bytes < 600,
            "matrix extent expanded into emitted operations"
        );
        sizes.push(bytes);
    }
    assert!(sizes.last().unwrap().abs_diff(sizes[0]) < 32);
}

#[test]
fn affine_input_views_and_multiple_nodes_preserve_outputs_and_signed_zero() {
    let (mut block, layout, mut y, p) = matrix_block(3, 4, 2);
    if let ComputeNode::MatMul { lhs_ops, .. } = &mut block.nodes[0] {
        *lhs_ops = (0..12)
            .map(|i| LinearOp::LoadY {
                dst: i as Reg,
                index: 13 - i,
            })
            .collect();
    }
    block.nodes.push(block.nodes[0].clone());
    check(&block, &layout, &y, &p);
    y.fill(-0.0);
    let p = vec![1.; p.len()];
    check(&block, &layout, &y, &p);
    let (empty, layout, y, p) = matrix_block(3, 0, 2);
    assert!(compile_expression_compute_block_wasm(&empty, &layout).is_err());
    let _ = (y, p);
    let (empty, layout, y, p) = matrix_block(0, 4, 2);
    assert!(compile_expression_compute_block_wasm(&empty, &layout).is_err());
    let _ = (y, p);
}

#[test]
fn invalid_input_views_bounds_and_coupled_operands_fail_explicitly() {
    let (block, layout, _, _) = matrix_block(15, 15, 12);
    let mut invalid = block.clone();
    if let ComputeNode::MatMul { lhs_ops, .. } = &mut invalid.nodes[0]
        && let LinearOp::TensorLoad { input_start, .. } = &mut lhs_ops[0]
    {
        *input_start = 3;
    }
    assert!(compile_expression_compute_block_wasm(&invalid, &layout).is_err());
    let mut invalid = block.clone();
    if let ComputeNode::MatMul {
        rhs_start, rhs_ops, ..
    } = &mut invalid.nodes[0]
    {
        *rhs_start = 0;
        if let LinearOp::TensorLoad { dst_start, .. } = &mut rhs_ops[0] {
            *dst_start = 0;
        }
    }
    assert!(compile_expression_compute_block_wasm(&invalid, &layout).is_err());
    let mut invalid = block;
    if let ComputeNode::MatMul { lhs_ops, .. } = &mut invalid.nodes[0] {
        lhs_ops.push(LinearOp::Const { dst: 0, value: 4. });
    }
    assert!(compile_expression_compute_block_wasm(&invalid, &layout).is_err());
}

#[test]
fn packed_tensor_reads_preserve_dual_zero_lanes_and_scalar_program_contract() {
    let ops = vec![
        LinearOp::TensorLoad {
            dst_start: 0,
            input: TensorInputKind::Y,
            input_start: 1,
            count: 12,
            seed_start: None,
            lanes: 2,
        },
        LinearOp::StoreOutputRange {
            start: 0,
            count: 24,
            stride: 1,
        },
    ];
    let source = ScalarProgramBlock::with_source_span(
        vec![ops],
        source_span_from_offsets(1, 0, 1)
            .require_provenance("tensor test")
            .unwrap(),
    )
    .unwrap();
    let block = ComputeBlock {
        nodes: vec![ComputeNode::ScalarPrograms(source)],
    };
    let y = (0..13).map(|i| i as f64 - 0.5).collect::<Vec<_>>();
    let layout = VarLayout::from_parts(Default::default(), 13, 0);
    check(&block, &layout, &y, &[]);
    let short = VarLayout::from_parts(Default::default(), 12, 0);
    assert!(compile_expression_compute_block_wasm(&block, &short).is_err());
    let mut seeded = block.clone();
    if let ComputeNode::ScalarPrograms(source) = &mut seeded.nodes[0] {
        let mut ops = source.programs()[0].clone();
        if let LinearOp::TensorLoad { seed_start, .. } = &mut ops[0] {
            *seed_start = Some(1);
        }
        *source = ScalarProgramBlock::with_source_span(
            vec![ops],
            source_span_from_offsets(1, 0, 1)
                .require_provenance("seed test")
                .unwrap(),
        )
        .unwrap();
        let prepared = PreparedScalarProgramBlock::new(source.clone()).unwrap();
        let mut expected = vec![0.; 24];
        prepared
            .eval_with_context(
                &y,
                &[],
                0.25,
                RowEvalContext {
                    seed: Some(&y),
                    ..Default::default()
                },
                &mut expected,
            )
            .unwrap();
        let kernel = compile_expression_compute_block_wasm(&seeded, &layout).unwrap();
        assert_bits(&execute(kernel.module_bytes(), &y, &[], 24), &expected);
    }
}

#[test]
fn matrix_runtime_rejects_input_output_aliasing_before_mutation() {
    let (block, layout, y, p) = matrix_block(3, 4, 2);
    let kernel = compile_expression_compute_block_wasm(&block, &layout).unwrap();
    let bytes = compile_expression_compute_block_wasm_bytes(&block, &layout).unwrap();
    assert_eq!(bytes, kernel.module_bytes());
    let engine = Engine::default();
    let module = Module::new(&engine, &bytes[..]).unwrap();
    let mut store = Store::new(&engine, ());
    let memory = Memory::new(&mut store, MemoryType::new(1, None)).unwrap();
    let input: Vec<u8> = y.iter().chain(&p).flat_map(|v| v.to_le_bytes()).collect();
    memory.write(&mut store, 0, &input).unwrap();
    let mut linker = Linker::new(&engine);
    linker.define("env", "memory", memory).unwrap();
    let instance = linker
        .instantiate(&mut store, &module)
        .unwrap()
        .start(&mut store)
        .unwrap();
    let function = instance
        .get_typed_func::<(i32, i32, f64, i32, i32), ()>(&store, "eval_residual")
        .unwrap();
    for output in [0, (y.len() * 8) as i32, -1] {
        assert!(
            function
                .call(&mut store, (0, (y.len() * 8) as i32, 0., 0, output))
                .is_err()
        );
        let mut actual = vec![0; input.len()];
        memory.read(&store, 0, &mut actual).unwrap();
        assert_eq!(
            actual, input,
            "invalid alias must not clear or mutate either input"
        );
    }
}

#[test]
fn byte_emission_and_instantiated_compile_share_exact_checks_and_execution() {
    let (block, layout, y, p) = matrix_block(3, 4, 2);
    let bytes = compile_expression_compute_block_wasm_bytes(&block, &layout).unwrap();
    let compiled = compile_expression_compute_block_wasm(&block, &layout).unwrap();
    assert_eq!(bytes, compiled.module_bytes());
    assert_bits(
        &execute(&bytes, &y, &p, compiled.rows()),
        &scalar_reference(&block, &y, &p),
    );
    let short = VarLayout::from_parts(Default::default(), y.len() - 1, p.len());
    assert!(compile_expression_compute_block_wasm_bytes(&block, &short).is_err());
    assert!(compile_expression_compute_block_wasm(&block, &short).is_err());
}
