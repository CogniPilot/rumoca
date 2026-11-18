//! MLS §10.5: checked native input reads retain one-based bounds and current values.
use super::*;
use solve::TensorIndex;

fn source(count: usize, guarded: bool, extra_reader: bool) -> ScalarProgramBlock {
    let result = count as solve::Reg + 1;
    let mut ops = vec![
        LinearOp::LoadP { dst: 0, index: 0 },
        LinearOp::TensorLoad {
            dst_start: 1,
            input: TensorInputKind::P,
            input_start: 1,
            count,
            seed_start: None,
            lanes: 1,
        },
        LinearOp::LoadIndexedRegister {
            dst: result,
            base: 1,
            stride: 1,
            dimensions: vec![count as u32].into(),
            indices: vec![TensorIndex::Runtime(0)].into(),
        },
        LinearOp::StoreOutput { src: result },
    ];
    if extra_reader {
        ops.push(LinearOp::StoreOutput { src: 1 });
    }
    if guarded {
        let width = if extra_reader { 2 } else { 1 };
        let program = solve::FunctionConditionalProgram::checked(
            0,
            vec![width],
            [(
                vec![
                    LinearOp::Const { dst: 0, value: 1. },
                    LinearOp::StoreOutput { src: 0 },
                ],
                ops,
            )],
            (0..width)
                .flat_map(|index| {
                    [
                        LinearOp::Const {
                            dst: index as solve::Reg,
                            value: -1.,
                        },
                        LinearOp::StoreOutput {
                            src: index as solve::Reg,
                        },
                    ]
                })
                .collect(),
        )
        .unwrap();
        ops = vec![
            LinearOp::FunctionConditional {
                dst_start: 0,
                capture_start: 0,
                program: std::sync::Arc::new(program),
            },
            LinearOp::StoreOutputRange {
                start: 0,
                count: width,
                stride: 1,
            },
        ];
    }
    ScalarProgramBlock::with_program_spans(vec![ops], vec![span(950)]).unwrap()
}

#[test]
fn parameter_gather_private_emission_does_not_grow_with_the_array() {
    for guarded in [false, true] {
        let sizes = [128, 4096].map(|count| {
            let layout = VarLayout::from_parts(Default::default(), 0, count + 1);
            compile_private_program_wasm(&source(count, guarded, false), &layout, &table().0)
                .unwrap()
                .module_bytes()
                .len()
        });
        assert!(sizes[1] < sizes[0] + 256, "guarded={guarded}: {sizes:?}");
    }
}

#[test]
fn parameter_gather_private_preserves_bounds_other_readers_and_changed_inputs() {
    for (guarded, extra_reader) in [(false, false), (false, true), (true, false), (true, true)] {
        let count = 128;
        let layout = VarLayout::from_parts(Default::default(), 0, count + 1);
        let program = compile_private_program_wasm(
            &source(count, guarded, extra_reader),
            &layout,
            &table().0,
        )
        .unwrap();
        let mut scratch = vec![0.; (program.scratch_bytes() as usize).div_ceil(8)];
        let mut p = (0..=count)
            .map(|value| value as f64 * 2.)
            .collect::<Vec<_>>();
        for first in [42., -12.] {
            p[0] = 1.;
            p[1] = first;
            program.call(&[], &p, 0., &mut scratch).unwrap();
            assert_eq!(scratch[0], first);
            if extra_reader {
                assert_eq!(scratch[1], first);
            }
            p[0] = 128.;
            program.call(&[], &p, 0., &mut scratch).unwrap();
            assert_eq!(scratch[0], 256.);
        }
        for index in [0., -1., 129., 1.5, f64::NAN, f64::INFINITY] {
            p[0] = index;
            assert!(
                program.call(&[], &p, 0., &mut scratch).is_err(),
                "index={index}"
            );
        }
    }
}
