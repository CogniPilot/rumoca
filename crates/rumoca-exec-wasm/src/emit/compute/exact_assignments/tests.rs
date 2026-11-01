mod faults;
mod runner;

use super::*;
use runner::Runner;

fn artifact(sinusoid: bool) -> CompiledNativeCallProgramWasm {
    let mut ops = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadP { dst: 1, index: 0 },
    ];
    if sinusoid {
        ops.push(LinearOp::Unary {
            dst: 2,
            op: UnaryOp::Sin,
            arg: 0,
        });
    }
    ops.extend([
        LinearOp::Binary {
            dst: 3,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 3 },
    ]);
    let layout = VarLayout::from_parts(Default::default(), 1, 1);
    let span = solve::source_span_from_offsets(11, 20, 30);
    let block = solve::ScalarProgramBlock::with_program_spans(vec![ops], vec![span]).unwrap();
    let arithmetic = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    );
    let calls = solve::SolvePureCallTable::builder(arithmetic).finish();
    emit_module(
        Ready::private(&block, &layout, &calls).unwrap(),
        &layout,
        true,
    )
    .unwrap()
}

#[test]
fn defined_register_memory_keeps_ieee_values_without_input_changes() {
    let artifact = artifact(false);
    let mut runner = Runner::new();
    let call = runner.instance(&artifact);
    for value in [
        0.,
        -0.,
        f64::from_bits(1),
        -f64::from_bits(1),
        2.5,
        f64::MAX,
        f64::INFINITY,
        f64::from_bits(0x7ff8_0123_4567_89ab),
    ] {
        let original = [value, -2.0];
        assert_eq!(runner.run(call, original), (0, (value * -2.0).to_bits()));
        assert_eq!(runner.inputs(), original.map(f64::to_bits));
    }
}

#[test]
fn nested_math_import_keeps_distinct_kernel_memories_and_outer_old_register_values() {
    let artifact = artifact(true);
    let mut runner = Runner::new();
    let outer = runner.instance(&artifact);
    let child = runner.instance(&artifact);
    runner.nested(child);
    assert_eq!(runner.run(outer, [2., 3.]), (0, 6f64.to_bits()));
    assert_eq!(runner.nested_calls(), 1);
    assert_eq!(runner.child_output(), 35f64.to_bits());
    assert_eq!(runner.inputs(), [2f64.to_bits(), 3f64.to_bits()]);
}

#[test]
fn private_modules_define_their_register_memory_and_import_no_arena() {
    let artifact = artifact(true);
    let (mut imports, mut memories) = (0, 0);
    for payload in wasmparser::Parser::new(0).parse_all(artifact.module_bytes()) {
        match payload.unwrap() {
            wasmparser::Payload::ImportSection(section) => imports = section.count(),
            wasmparser::Payload::MemorySection(section) => memories = section.count(),
            _ => {}
        }
    }
    assert_eq!((imports, memories), (2, 1)); // public memory and sin; private registers
    let mut runner = Runner::new();
    let call = runner.instance(&artifact);
    assert_eq!(runner.run(call, [2., 3.]), (0, 6f64.to_bits()));
}
