//! Exercise the actual emitter in Wasmi, including internal overlapping ranges.
use super::*;
use rumoca_core::{SourceId, Span};
use std::num::NonZeroU64;
use wasmi::{Engine, Linker, Memory, MemoryType as RuntimeMemoryType, Store};

fn harness(body: impl FnOnce(&mut Emitter<'_>)) -> Vec<u8> {
    let profile = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    );
    let mut builder = solve::SolvePureCallTable::builder(profile);
    let root = builder
        .add_owner(
            solve::SolvePureCallIdentity::issued(NonZeroU64::new(1).unwrap()),
            vec![],
            vec![solve::SolvePureCallOutput::result(
                solve::SolveValueType::scalar(solve::SolveScalarType::integer(profile)),
            )],
            Span::from_offsets(SourceId::from_source_name("cell-emitter.mo"), 0, 1),
            |b, _, outputs| {
                let value = b.constant(
                    solve::SolveValue::integer(profile, 0).unwrap(),
                    Span::from_offsets(SourceId::from_source_name("cell-emitter.mo"), 1, 2),
                )?;
                b.store(
                    outputs[0],
                    value,
                    Span::from_offsets(SourceId::from_source_name("cell-emitter.mo"), 2, 3),
                )
            },
        )
        .unwrap();
    let table = builder.finish();
    let linked = LinkedOwners::construct(&table, root).unwrap();
    let owner = table.owner(root).unwrap();
    let mut emitter = Emitter {
        function: Function::new([(1, ValType::I32), (4, ValType::I64), (2, ValType::F64)]),
        owner,
        plan: linked.plan(root).unwrap(),
        program: owner.body(),
        region_path: Vec::new(),
        linked: &linked,
        fault_offset: 0,
        faults: Vec::new(),
    };
    body(&mut emitter);
    emitter.push(I::End);
    module(vec![emitter.function], 0, &[])
}

fn execute(
    bytes: &[u8],
    initial: &[u8],
    pointers: (i32, i32, i32),
) -> (bool, Option<i32>, Vec<u8>) {
    let engine = Engine::default();
    let module = wasmi::Module::new(&engine, bytes).unwrap();
    let mut store = Store::new(&engine, ());
    let memory = Memory::new(&mut store, RuntimeMemoryType::new(1, None)).unwrap();
    memory.write(&mut store, 0, initial).unwrap();
    let mut linker = Linker::new(&engine);
    linker.define("env", "memory", memory).unwrap();
    let instance = linker
        .instantiate(&mut store, &module)
        .unwrap()
        .start(&mut store)
        .unwrap();
    let call = instance
        .get_typed_func::<(i32, i32, i32), i32>(&store, "eval_typed_call")
        .unwrap();
    let result = call.call(&mut store, pointers);
    (result.is_err(), result.ok(), memory.data(&store).to_vec())
}

fn op_counts(bytes: &[u8]) -> (usize, usize) {
    let mut counts = (0, 0);
    for payload in wasmparser::Parser::new(0).parse_all(bytes) {
        let wasmparser::Payload::CodeSectionEntry(body) = payload.unwrap() else {
            continue;
        };
        for op in body.get_operators_reader().unwrap() {
            match op.unwrap() {
                wasmparser::Operator::MemoryCopy { .. } => counts.0 += 1,
                wasmparser::Operator::Loop { .. } => counts.1 += 1,
                _ => {}
            }
        }
    }
    counts
}

fn copy_module(bytes: u32) -> Vec<u8> {
    harness(|e| {
        e.copy(
            CellRange {
                base: 0,
                offset: 0,
                bytes,
            },
            CellRange {
                base: 1,
                offset: 0,
                bytes,
            },
        );
        e.push(I::I32Const(0));
    })
}

#[test]
fn eight_byte_copy_matches_snapshot_memmove_for_every_overlap_and_preserves_bits() {
    let module = copy_module(8);
    assert_eq!(op_counts(&module).0, 0);
    check_overlap(&module, None);
}

fn copy_addresses_module(bytes: u32, original: bool) -> Vec<u8> {
    harness(|e| {
        e.push(I::LocalGet(0));
        e.push(I::LocalGet(1));
        if original {
            e.push(I::I32Const(bytes as i32));
            e.push(I::MemoryCopy {
                src_mem: 0,
                dst_mem: 0,
            });
        } else {
            e.copy_addresses(bytes);
        }
        e.push(I::I32Const(0));
    })
}

#[test]
fn stack_address_cell_copy_matches_original_bulk_copy_and_independent_memmove() {
    let module = copy_addresses_module(8, false);
    let original = copy_addresses_module(8, true);
    assert_eq!(op_counts(&module).0, 0);
    assert_eq!(op_counts(&original).0, 1);
    check_overlap(&module, Some(&original));
}

fn check_overlap(module: &[u8], original: Option<&[u8]>) {
    for bits in [
        0x8000_0000_0000_0000u64,
        0x7ff8_dead_beef_1234,
        0x7ff0_0000_0000_0001,
        i64::MIN as u64,
        i64::MAX as u64,
        0,
        1,
        u64::MAX,
    ] {
        for displacement in -7i32..=7 {
            let source = 32usize;
            let destination = (source as i32 + displacement) as usize;
            let mut initial = vec![0xa5; 65536];
            initial[source..source + 8].copy_from_slice(&bits.to_le_bytes());
            let mut expected = initial.clone();
            expected.copy_within(source..source + 8, destination);
            let (trap, status, actual) =
                execute(module, &initial, (destination as i32, source as i32, 0));
            assert!(!trap);
            assert_eq!(status, Some(0));
            assert_eq!(actual, expected);
            if let Some(original) = original {
                assert_eq!(
                    execute(original, &initial, (destination as i32, source as i32, 0)),
                    (trap, status, actual)
                );
            }
        }
    }
}

#[test]
fn eight_byte_copy_bounds_traps_never_partially_publish() {
    let module = copy_module(8);
    let initial = vec![0xa5; 65536];
    for pointers in [(0, 65532, 0), (65532, 0, 0), (-4, 0, 0), (0, -4, 0)] {
        let (trap, _, actual) = execute(&module, &initial, pointers);
        assert!(trap);
        assert_eq!(actual, initial);
    }
}

#[test]
fn stack_address_cell_copy_bounds_and_fallback_match_original_bulk_copy() {
    for bytes in [0, 8, 16] {
        let module = copy_addresses_module(bytes, false);
        let original = copy_addresses_module(bytes, true);
        assert_eq!(op_counts(&module).0, usize::from(bytes != 8));
        let initial: Vec<_> = (0..65536).map(|i| i as u8).collect();
        for pointers in [
            (33, 32, 0),
            (0, 65532, 0),
            (65532, 0, 0),
            (-4, 0, 0),
            (0, -4, 0),
            (65536, 65536, 0),
        ] {
            let actual = execute(&module, &initial, pointers);
            assert_eq!(actual, execute(&original, &initial, pointers));
            if actual.0 {
                assert_eq!(actual.2, initial);
            }
        }
    }
}

#[test]
fn larger_copy_keeps_bulk_memory_overlap_semantics() {
    let module = copy_module(16);
    assert_eq!(op_counts(&module).0, 1);
    let initial: Vec<_> = (0..65536).map(|i| i as u8).collect();
    let mut expected = initial.clone();
    expected.copy_within(32..48, 35);
    let (trap, status, actual) = execute(&module, &initial, (35, 32, 0));
    assert!(!trap);
    assert_eq!(status, Some(0));
    assert_eq!(actual, expected);
}

#[test]
fn single_cell_runs_once_at_zero_and_leaves_index_one_without_loop() {
    for count in [0, 1, 2] {
        let module = harness(|e| {
            e.cells(count, |e| {
                e.push(I::I32Const(64));
                e.push(I::I32Const(64));
                e.push(I::I64Load(CELL));
                e.push(I::I64Const(1));
                e.push(I::I64Add);
                e.push(I::I64Store(CELL));
                e.push(I::I32Const(80));
                e.push(I::LocalGet(3));
                e.push(I::I32Store(MemArg { align: 2, ..CELL }));
            });
            e.push(I::LocalGet(3));
        });
        assert_eq!(op_counts(&module).1, usize::from(count != 1));
        let (trap, status, actual) = execute(&module, &vec![0; 65536], (0, 0, 0));
        assert!(!trap);
        assert_eq!(status, Some(count as i32));
        assert_eq!(
            u64::from_le_bytes(actual[64..72].try_into().unwrap()),
            u64::from(count)
        );
        assert_eq!(
            u32::from_le_bytes(actual[80..84].try_into().unwrap()),
            count.saturating_sub(1)
        );
    }
}
