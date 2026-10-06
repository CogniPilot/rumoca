mod faults;
mod runner;

use super::*;
use runner::Runner;

fn artifact(storage: arena::ArenaStorage, sinusoid: bool) -> CompiledNativeCallProgramWasm {
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
    emit_module_with_storage(
        Ready::private(&block, &layout, &calls).unwrap(),
        &layout,
        true,
        storage,
    )
    .unwrap()
}

#[test]
fn pooled_private_regions_match_defined_arenas_for_ieee_values_without_input_changes() {
    let pooled = artifact(arena::ArenaStorage::Pooled, false);
    let defined = artifact(arena::ArenaStorage::Defined, false);
    assert_eq!(pooled.pooled_arena_bytes(), Some(65536));
    assert_eq!(defined.pooled_arena_bytes(), None);
    let mut pool = Runner::new();
    let first = pool.instance(&pooled, Some(0));
    let second = pool.instance(&pooled, Some(65536));
    let plain = pool.instance(&defined, None);
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
        let expected = pool.run(plain, original);
        assert_eq!(pool.run(first, original), expected);
        assert_eq!(pool.run(second, original), expected);
        assert_eq!(pool.inputs(), original.map(f64::to_bits));
    }
}

#[test]
fn invalid_pooled_bases_fail_before_work_or_publication_and_valid_owner_recovers() {
    let artifact = artifact(arena::ArenaStorage::Pooled, false);
    let mut pool = Runner::new();
    let valid = pool.instance(&artifact, Some(65536));
    for base in [1u32, 8, 65536 + 8, 2 * 65536, u32::MAX - 7] {
        let invalid = pool.instance(&artifact, Some(base));
        let before = pool.private_bytes();
        assert_eq!(pool.run(invalid, [2., 3.]), (1, 77f64.to_bits()));
        assert_eq!(pool.private_bytes(), before);
        assert_eq!(pool.run(valid, [2., 3.]), (0, 6f64.to_bits()));
    }
}

#[test]
fn nested_math_import_keeps_distinct_kernel_regions_and_outer_old_register_values() {
    let artifact = artifact(arena::ArenaStorage::Pooled, true);
    let mut pool = Runner::new();
    let outer = pool.instance(&artifact, Some(0));
    let child = pool.instance(&artifact, Some(65536));
    pool.nested(child);
    assert_eq!(pool.run(outer, [2., 3.]), (0, 6f64.to_bits()));
    assert_eq!(pool.nested_calls(), 1);
    assert_eq!(pool.child_output(), 35f64.to_bits());
    assert_eq!(pool.inputs(), [2f64.to_bits(), 3f64.to_bits()]);
}

#[test]
fn pooled_instances_do_not_define_memories_or_shift_math_function_indices() {
    let artifact = artifact(arena::ArenaStorage::Pooled, true);
    let mut count = 0;
    for payload in wasmparser::Parser::new(0).parse_all(artifact.module_bytes()) {
        match payload.unwrap() {
            wasmparser::Payload::ImportSection(imports) => {
                count = imports.count();
            }
            wasmparser::Payload::MemorySection(_) => {
                panic!("pooled artifact defines private memory")
            }
            _ => {}
        }
    }
    assert_eq!(count, 4); // public memory, private memory, immutable base, Math.sin
    let mut pool = Runner::new();
    let call = pool.instance(&artifact, Some(65536));
    assert_eq!(pool.run(call, [2., 3.]), (0, 6f64.to_bits()));
}
