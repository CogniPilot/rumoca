//! Seed marshalling must scale with the source reads, not the global layout.
use super::*;
use rumoca_ir_solve::{LinearOp, SolveArithmeticProfile, SolveIntegerDomain, SolveRealFormat};

fn compile(operations: Vec<LinearOp>, count: usize) -> CompiledPrivateProgramWasm {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("seed_copy.mo"),
        1,
        2,
    );
    let source = ScalarProgramBlock::with_program_spans(vec![operations], vec![span]).unwrap();
    let layout = VarLayout::from_parts(Default::default(), 1, 0);
    let calls = SolvePureCallTable::builder(SolveArithmeticProfile::construct(
        SolveRealFormat::Binary64,
        SolveIntegerDomain::FULL,
    ))
    .finish();
    compile_private_directional_program_wasm(&source, &layout, &calls, count).unwrap()
}

#[test]
fn sparse_seed_reads_do_not_copy_the_global_seed_prefix() {
    const N: usize = 10000;
    let code = compile(
        vec![
            LinearOp::LoadSeed { dst: 0, index: 2 },
            LinearOp::StoreOutput { src: 0 },
            LinearOp::LoadSeed {
                dst: 1,
                index: N - 2,
            },
            LinearOp::StoreOutput { src: 1 },
        ],
        N,
    );
    let offset = code.seed.as_ref().unwrap().offset;
    let mut scratch = vec![37.0; code.scratch_bytes() as usize / 8];
    let mut seed = vec![0.0; N];
    seed[2] = -0.0;
    seed[N - 2] = 4.0;
    code.call_directional(&[1.0], &[], 0.0, &seed, &mut scratch)
        .unwrap();
    assert_eq!(scratch[0].to_bits(), (-0.0f64).to_bits());
    assert_eq!(scratch[1], 4.0);
    // Unread seed storage is not an output or a write target of this entry.
    for index in (0..N).filter(|index| ![2, N - 2].contains(index)) {
        assert_eq!(scratch[offset + index], 37.0, "unread seed {index} copied");
    }
    seed[2] = 5.0;
    seed[N - 2] = -6.0;
    code.call_directional(&[1.0], &[], 0.0, &seed, &mut scratch)
        .unwrap();
    assert_eq!(&scratch[..2], &[5.0, -6.0]);
    // The declared ABI extent still applies even when the entry reads little.
    let before = scratch.clone();
    assert!(
        code.call_directional(&[1.0], &[], 0.0, &seed[..N - 1], &mut scratch)
            .is_err()
    );
    assert_eq!(scratch, before);
}

#[test]
fn indexed_seed_reads_copy_every_selectable_coordinate_each_call() {
    let code = compile(
        vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::LoadIndexedSeed {
                dst: 1,
                base: 7,
                count: 3,
                index: 0,
            },
            LinearOp::StoreOutput { src: 1 },
        ],
        16,
    );
    let mut scratch = vec![37.0; code.scratch_bytes() as usize / 8];
    let mut seed = vec![0.0; 16];
    for (index, expected) in [0.0, 1.0, 2.0].into_iter().zip([4.0f64, -0.0, 6.0]) {
        seed[7] = 4.0;
        seed[8] = -0.0;
        seed[9] = 6.0;
        code.call_directional(&[index], &[], 0.0, &seed, &mut scratch)
            .unwrap();
        assert_eq!(scratch[0].to_bits(), expected.to_bits());
    }
    seed[8] = 8.0;
    code.call_directional(&[1.0], &[], 0.0, &seed, &mut scratch)
        .unwrap();
    assert_eq!(scratch[0], 8.0);
}

#[test]
fn tensor_seed_reads_keep_the_packed_tangent_bits() {
    let code = compile(
        vec![
            LinearOp::TensorLoad {
                dst_start: 0,
                input: rumoca_ir_solve::TensorInputKind::Y,
                input_start: 0,
                count: 1,
                seed_start: Some(10),
                lanes: 2,
            },
            LinearOp::StoreOutput { src: 1 },
        ],
        32,
    );
    let mut scratch = vec![37.0; code.scratch_bytes() as usize / 8];
    let mut seed = vec![0.0; 32];
    seed[10] = -0.0;
    code.call_directional(&[1.0], &[], 0.0, &seed, &mut scratch)
        .unwrap();
    assert_eq!(scratch[0].to_bits(), (-0.0f64).to_bits());
    seed[10] = 9.0;
    code.call_directional(&[1.0], &[], 0.0, &seed, &mut scratch)
        .unwrap();
    assert_eq!(scratch[0], 9.0);
}

#[test]
fn nested_regions_retain_both_seed_read_footprints_and_lazy_execution() {
    use rumoca_ir_solve::{CompareOp, FunctionConditionalProgram};
    let branch = |index| {
        vec![
            LinearOp::LoadSeed { dst: 0, index },
            LinearOp::StoreOutput { src: 0 },
        ]
    };
    let conditional = FunctionConditionalProgram::checked(
        1,
        vec![1],
        [(
            vec![
                LinearOp::LoadFunctionConditionalCapture { dst: 0, index: 0 },
                LinearOp::Const { dst: 1, value: 0.0 },
                LinearOp::Compare {
                    dst: 2,
                    op: CompareOp::Gt,
                    lhs: 0,
                    rhs: 1,
                },
                LinearOp::StoreOutput { src: 2 },
            ],
            branch(7),
        )],
        branch(11),
    )
    .unwrap();
    let code = compile(
        vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::FunctionConditional {
                dst_start: 1,
                capture_start: 0,
                program: std::sync::Arc::new(conditional),
            },
            LinearOp::StoreOutput { src: 1 },
        ],
        32,
    );
    let mut scratch = vec![37.0; code.scratch_bytes() as usize / 8];
    let mut seed = vec![0.0; 32];
    seed[7] = 4.0;
    seed[11] = f64::NAN;
    code.call_directional(&[1.0], &[], 0.0, &seed, &mut scratch)
        .unwrap();
    assert_eq!(scratch[0], 4.0);
    seed[7] = f64::NAN;
    seed[11] = -6.0;
    code.call_directional(&[-1.0], &[], 0.0, &seed, &mut scratch)
        .unwrap();
    assert_eq!(scratch[0], -6.0);
}
