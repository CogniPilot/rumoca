use super::*;
use rumoca_ir_solve::{BinaryOp, SolvePureCallTable};

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name("shared_scratch.mo"), 0, 23)
}

/// Five outputs, so its tuple is wider than the two-slot Y vector and it sizes
/// the shared scratch.
fn wide(directional: bool) -> Vec<LinearOp> {
    let mut ops = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadY { dst: 1, index: 1 },
        LinearOp::LoadP { dst: 2, index: 0 },
        LinearOp::Binary {
            dst: 3,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 2,
        },
        LinearOp::Binary {
            dst: 4,
            op: BinaryOp::Sub,
            lhs: 1,
            rhs: 3,
        },
    ];
    if directional {
        ops.push(LinearOp::LoadSeed { dst: 5, index: 1 });
    } else {
        ops.push(LinearOp::LoadTime { dst: 5 });
    }
    ops.extend((0..5).map(|offset| LinearOp::StoreOutput { src: offset + 1 }));
    ops
}

/// One output, so it runs on cells the wide program wrote last.
fn narrow(directional: bool) -> Vec<LinearOp> {
    vec![
        if directional {
            LinearOp::LoadSeed { dst: 0, index: 0 }
        } else {
            LinearOp::LoadY { dst: 0, index: 1 }
        },
        LinearOp::LoadP { dst: 1, index: 0 },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Add,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 2 },
    ]
}

fn compile(programs: Vec<Vec<LinearOp>>, directional: bool) -> WasmExpression {
    let spans = vec![span(); programs.len()];
    let source = ScalarProgramBlock::with_program_spans(programs, spans).unwrap();
    WasmExpression::compile_with_seed(
        &source,
        &VarLayout::from_parts(Default::default(), 2, 1),
        &SolvePureCallTable::default(),
        directional.then_some(2),
    )
    .unwrap()
}

fn cells(program: &selected::Program) -> usize {
    (program.0.scratch_bytes() as usize).div_ceil(8)
}

fn bits(values: &[f64]) -> Vec<u64> {
    values.iter().map(|value| value.to_bits()).collect()
}

fn assert_interleaved_matches_alone(directional: bool) {
    let programs = vec![wide(directional), narrow(directional), wide(directional)];
    let shared = compile(programs.clone(), directional);
    let buffer = &shared.selected[0].1;
    assert!(
        shared
            .selected
            .iter()
            .all(|program| Rc::ptr_eq(&program.1, buffer))
    );
    let widest = shared
        .selected
        .iter()
        .map(|program| cells(program))
        .max()
        .unwrap();
    assert!(cells(&shared.selected[1]) < widest);
    assert_eq!(buffer.borrow().len(), widest);
    let alone = programs
        .into_iter()
        .map(|program| compile(vec![program], directional))
        .collect::<Vec<_>>();
    let (mut actual, mut expected) = (Vec::new(), Vec::new());
    for (y, p, seed) in [
        ([1.5, -2.0], [3.0], [0.25, -8.0]),
        ([-0.0, 7.25], [0.5], [f64::NAN, 0.0]),
        ([1e300, -1e-300], [-4.0], [-0.0, 1e-308]),
    ] {
        let inputs = rumoca_eval_solve::JacobianEvalInputs {
            y: &y,
            p: &p,
            t: 0.75,
            seed: if directional { &seed } else { &[] },
        };
        for program in [1, 0, 1, 2, 1, 0, 1] {
            shared
                .values_with_seed(program, inputs, &[], &mut actual)
                .unwrap();
            alone[program]
                .values_with_seed(0, inputs, &[], &mut expected)
                .unwrap();
            assert_eq!(
                bits(&actual),
                bits(&expected),
                "directional {directional}, program {program}"
            );
        }
    }
}

#[test]
fn interleaved_programs_on_one_shared_scratch_match_each_program_alone() {
    assert_interleaved_matches_alone(false);
}

#[test]
fn interleaved_directional_programs_on_one_shared_scratch_match_each_program_alone() {
    assert_interleaved_matches_alone(true);
}
