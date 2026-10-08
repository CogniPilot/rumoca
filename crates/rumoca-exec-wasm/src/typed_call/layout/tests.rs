//! Operation-private scratch is shared; hand-computed byte counts.
use super::super::program::ProgramHelpers;
use crate::ScratchOwner;
use rumoca_core::{SourceId, Span};
use rumoca_ir_solve as solve;
use std::num::NonZeroU64;

fn span(offset: usize) -> Span {
    Span::from_offsets(
        SourceId::from_source_name("scratch_layout.mo"),
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

fn real() -> solve::SolveValueType {
    solve::SolveValueType::scalar(solve::SolveScalarType::real(arithmetic()))
}

fn array() -> solve::SolveValueType {
    solve::SolveValueType::tensor(solve::SolveScalarType::real(arithmetic()), vec![100]).unwrap()
}

/// `big(x)`: a 100 element local (800 bytes) and its first element.
fn add_big(table: &mut solve::SolvePureCallTableBuilder) -> solve::SolvePureCallOwnerId {
    table
        .add_owner(
            identity(1),
            vec![real()],
            vec![solve::SolvePureCallOutput::result(real())],
            span(1),
            |b, input, output| {
                let x = b.load(input[0], span(2))?;
                let local = b.fill(x, vec![100], span(3))?;
                let first = b.project_element(local, vec![0], span(4))?;
                b.store(output[0], first, span(5))
            },
        )
        .unwrap()
}

fn owner(owners: &[ScratchOwner], index: usize) -> &ScratchOwner {
    owners.iter().find(|o| o.owner == index).unwrap()
}

fn report(table: solve::SolvePureCallTable, site: &solve::SolvePureCallSite) -> Vec<ScratchOwner> {
    ProgramHelpers::new(&table, std::slice::from_ref(site))
        .unwrap()
        .owner_scratch(&table)
}

#[test]
fn sequential_calls_share_one_callee_frame() {
    let mut table = solve::SolvePureCallTable::builder(arithmetic());
    let big = add_big(&mut table);
    let twice = table
        .add_owner(
            identity(2),
            vec![real()],
            vec![solve::SolvePureCallOutput::result(real())],
            span(10),
            |b, input, output| {
                let a = b.load(input[0], span(11))?;
                let first = b.call(big, &[a], span(12))?;
                let second = b.call(big, &[first[0]], span(13))?;
                b.store(output[0], second[0], span(14))
            },
        )
        .unwrap();
    let site = table.call_site(twice).unwrap();
    let owners = report(table.finish(), &site);
    // big: output 8 + local 800 + element 8.
    let big = owner(&owners, 0);
    assert_eq!(big.frame.high_water_bytes, 816);
    assert_eq!(big.frame.unshared_bytes, 816);
    // twice: output 8, two call results 16, then one shared callee span of
    // input 8 + output 8 + scratch 816 above them.
    let twice = owner(&owners, 1);
    assert_eq!(twice.frame.high_water_bytes, 8 + 16 + 832);
    assert_eq!(twice.frame.unshared_bytes, 8 + 16 + 2 * 832);
    assert_eq!(twice.frame.calls.len(), 2);
    // The second call frame starts one result register above the first and
    // overlaps it.
    assert_eq!(twice.frame.calls[0].offset_bytes, 16);
    assert_eq!(twice.frame.calls[1].offset_bytes, 24);
    assert_eq!(twice.frame.calls[0].scratch_bytes, 816);
    assert_eq!(
        (twice.frame.register_count, twice.frame.register_bytes),
        (2, 16)
    );
}

fn conditional_owner(
    table: &mut solve::SolvePureCallTableBuilder,
    output: solve::SolveValueType,
    large: bool,
) -> solve::SolvePureCallOwnerId {
    let boolean = solve::SolveValueType::scalar(solve::SolveScalarType::Boolean);
    table
        .add_owner(
            identity(3),
            vec![boolean, real()],
            vec![solve::SolvePureCallOutput::result(output.clone())],
            span(20),
            move |b, input, out| {
                let condition = b.load(input[0], span(25))?;
                let x = b.load(input[1], span(26))?;
                let result = b.conditional(
                    condition,
                    &[x],
                    vec![output],
                    span(27),
                    |r, i, o| branch(r, i, o, large),
                    |r, i, o| branch(r, i, o, large),
                )?;
                b.store(out[0], result[0], span(28))
            },
        )
        .unwrap()
}

#[test]
fn conditional_arms_share_one_span_and_a_scalar_result_is_not_doubled() {
    let mut table = solve::SolvePureCallTable::builder(arithmetic());
    let id = conditional_owner(&mut table, real(), false);
    let site = table.call_site(id).unwrap();
    let owners = report(table.finish(), &site);
    let frame = &owner(&owners, 0).frame;
    // Parent: output 8 + result register 8. Each arm: output slot 8, local
    // 800, element 8, placed at the same base.
    assert_eq!(frame.high_water_bytes, 16 + 816);
    assert_eq!(frame.unshared_bytes, 16 + 2 * 816);
    let roles = frame
        .regions
        .iter()
        .map(|r| (r.role, r.frame.base_bytes))
        .collect::<Vec<_>>();
    assert_eq!(roles, [("then", 16), ("else", 16)]);
}

#[test]
fn conditional_publication_of_a_large_array_keeps_one_result_and_one_arm_span() {
    let mut table = solve::SolvePureCallTable::builder(arithmetic());
    let id = conditional_owner(&mut table, array(), true);
    let site = table.call_site(id).unwrap();
    let owners = report(table.finish(), &site);
    let frame = &owner(&owners, 0).frame;
    // Parent: output 800 + result 800. Each arm: output slot 800 + local 800.
    assert_eq!(frame.high_water_bytes, 1600 + 1600);
    assert_eq!(frame.unshared_bytes, 1600 + 2 * 1600);
}

fn branch<'r>(
    region: &mut solve::TypedProgramBuilder<'r>,
    input: &[solve::ProgramSlot<'r>],
    out: &[solve::ProgramSlot<'r>],
    large: bool,
) -> Result<(), solve::SolveProgramConstructionError> {
    let x = region.load(input[0], span(21))?;
    let local = region.fill(x, vec![100], span(22))?;
    let value = if large {
        local
    } else {
        region.project_element(local, vec![0], span(23))?
    };
    region.store(out[0], value, span(24))
}
