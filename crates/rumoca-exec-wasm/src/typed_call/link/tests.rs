//! Explicit target refusal consumes the canonical recursive-group owner.
use super::super::{compile_pure_call_wasm, program::ProgramHelpers};
use super::*;
use rumoca_core::{SourceId, Span};
use std::num::NonZeroU64;

fn span(offset: usize) -> Span {
    Span::from_offsets(
        SourceId::from_source_name("native_recursion_refusal.mo"),
        offset,
        offset + 1,
    )
}

fn arithmetic() -> solve::SolveArithmeticProfile {
    solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    )
}

fn identity(id: u64) -> solve::SolvePureCallIdentity {
    solve::SolvePureCallIdentity::issued(NonZeroU64::new(id).unwrap())
}

fn prefix(table: &mut solve::SolvePureCallTableBuilder) -> solve::SolvePureCallOwnerId {
    let scalar = solve::SolveValueType::scalar(solve::SolveScalarType::real(arithmetic()));
    table
        .add_owner(
            identity(1),
            vec![scalar.clone()],
            vec![solve::SolvePureCallOutput::result(scalar)],
            span(1),
            |b, input, output| {
                let value = b.load(input[0], span(2))?;
                b.store(output[0], value, span(3))
            },
        )
        .unwrap()
}

fn recursive_table(
    count: usize,
    conditional: bool,
) -> (
    solve::SolvePureCallTable,
    solve::SolvePureCallSite,
    solve::SolvePureCallSite,
    solve::SolvePureCallSite,
) {
    let scalar = solve::SolveValueType::scalar(solve::SolveScalarType::real(arithmetic()));
    let mut table = solve::SolvePureCallTable::builder(arithmetic());
    let ordinary = prefix(&mut table);
    let members = (0..count)
        .map(|i| {
            solve::SolveRecursiveMember::new(
                identity(10 + i as u64),
                vec![scalar.clone()],
                vec![solve::SolvePureCallOutput::result(scalar.clone())],
                span(10 + i),
            )
        })
        .collect();
    let group = table
        .add_recursive_group(members, |ordinal, ids, b, input, output| {
            let value = b.load(input[0], span(20))?;
            let result = b.call(ids[(ordinal + 1) % ids.len()], &[value], span(21))?;
            b.store(output[0], result[0], span(22))
        })
        .unwrap();
    let input_types = if conditional {
        vec![
            scalar.clone(),
            solve::SolveValueType::scalar(solve::SolveScalarType::Boolean),
        ]
    } else {
        vec![scalar.clone()]
    };
    let caller = table
        .add_owner(
            identity(30),
            input_types,
            vec![solve::SolvePureCallOutput::result(scalar.clone())],
            span(30),
            |b, input, output| {
                let value = b.load(input[0], span(31))?;
                let result = if conditional {
                    let condition = b.load(input[1], span(32))?;
                    b.conditional(
                        condition,
                        &[value],
                        vec![scalar],
                        span(33),
                        |region, input, output| {
                            let value = region.load(input[0], span(34))?;
                            let result = region.call(group[0], &[value], span(35))?;
                            region.store(output[0], result[0], span(36))
                        },
                        |region, input, output| {
                            let value = region.load(input[0], span(37))?;
                            region.store(output[0], value, span(38))
                        },
                    )?
                } else {
                    b.call(group[0], &[value], span(39))?
                };
                b.store(output[0], result[0], span(40))
            },
        )
        .unwrap();
    let ordinary = table.call_site(ordinary).unwrap();
    let direct = table.call_site(group[0]).unwrap();
    let caller = table.call_site(caller).unwrap();
    (table.finish(), ordinary, direct, caller)
}

fn assert_refused(table: &solve::SolvePureCallTable, site: &solve::SolvePureCallSite) {
    let error = compile_pure_call_wasm(table, site).unwrap_err();
    let TypedCallCompileError::UnsupportedRecursiveGroup { owner, provenance } = &error else {
        panic!("expected native recursion refusal, got {error:?}");
    };
    assert!(table.recursive_group(*owner).is_some());
    assert_eq!(*provenance, table.owner(*owner).unwrap().provenance());
    let program = ProgramHelpers::new(table, std::slice::from_ref(site))
        .err()
        .expect("native whole-program helpers must refuse recursion");
    assert_eq!(program, error);
}

#[test]
fn native_direct_and_ordinary_nested_recursive_groups_refuse_before_layout() {
    for count in [1, 2] {
        for conditional in [false, true] {
            let (table, _, direct, caller) = recursive_table(count, conditional);
            assert_refused(&table, &direct);
            assert_refused(&table, &caller);
        }
    }
}

#[test]
fn unreachable_recursive_group_preserves_ordinary_compilation() {
    let (table, ordinary, _, _) = recursive_table(2, true);
    let actual = compile_pure_call_wasm(&table, &ordinary).unwrap();
    ProgramHelpers::new(&table, std::slice::from_ref(&ordinary)).unwrap();
    let mut prefix_table = solve::SolvePureCallTable::builder(arithmetic());
    let owner = prefix(&mut prefix_table);
    let site = prefix_table.call_site(owner).unwrap();
    let expected = compile_pure_call_wasm(&prefix_table.finish(), &site).unwrap();
    assert_eq!(actual.module_bytes(), expected.module_bytes());
    assert_eq!(actual.layout(), expected.layout());
}

#[test]
fn recursive_refusal_survives_current_wire_replay() {
    let (table, _, _, caller) = recursive_table(2, true);
    let wire = serde_json::to_string(&table).unwrap();
    let replayed: solve::SolvePureCallTable = serde_json::from_str(&wire).unwrap();
    assert_refused(&replayed, &caller);
}
