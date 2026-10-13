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

fn array_of(count: u32) -> solve::SolveValueType {
    solve::SolveValueType::tensor(solve::SolveScalarType::real(arithmetic()), vec![count]).unwrap()
}

/// `big(x)`: a 100 element local (800 bytes) and its first element.
fn add_big(table: &mut solve::SolvePureCallTableBuilder) -> solve::SolvePureCallOwnerId {
    add_sized_big(table, 100)
}

fn add_sized_big(
    table: &mut solve::SolvePureCallTableBuilder,
    count: u32,
) -> solve::SolvePureCallOwnerId {
    table
        .add_owner(
            identity(1),
            vec![real()],
            vec![solve::SolvePureCallOutput::result(real())],
            span(1),
            |b, input, output| {
                let x = b.load(input[0], span(2))?;
                let local = b.fill(x, vec![count], span(3))?;
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
    // twice: output 8, two call results 16. A callee leaves its outputs in the
    // head of its scratch, so each call's scratch starts at its result
    // register and its input tuple of 8 lies above that scratch.
    let twice = owner(&owners, 1);
    assert_eq!(twice.frame.high_water_bytes, 8 + 16 + 816);
    assert_eq!(twice.frame.unshared_bytes, 8 + 16 + 2 * (8 + 816));
    assert_eq!(twice.frame.calls.len(), 2);
    // The second call frame starts one result register above the first and
    // overlaps it.
    assert_eq!(twice.frame.calls[0].offset_bytes, 8 + 816);
    assert_eq!(twice.frame.calls[1].offset_bytes, 16 + 816);
    assert_eq!(twice.frame.calls[0].scratch_bytes, 816);
    assert_eq!(
        (twice.frame.register_count, twice.frame.register_bytes),
        (2, 16)
    );
}

#[test]
fn a_large_real_input_is_passed_by_address_to_the_entry_and_to_a_called_helper() {
    let large =
        solve::SolveValueType::tensor(solve::SolveScalarType::real(arithmetic()), vec![1000])
            .unwrap();
    let mut table = solve::SolvePureCallTable::builder(arithmetic());
    let first = table
        .add_owner(
            identity(1),
            vec![large.clone()],
            vec![solve::SolvePureCallOutput::result(real())],
            span(1),
            |b, input, output| {
                let values = b.load(input[0], span(2))?;
                let head = b.project_element(values, vec![0], span(3))?;
                b.store(output[0], head, span(4))
            },
        )
        .unwrap();
    let entry = table
        .add_owner(
            identity(2),
            vec![large],
            vec![solve::SolvePureCallOutput::result(real())],
            span(10),
            |b, input, output| {
                let values = b.load(input[0], span(11))?;
                let head = b.call(first, &[values], span(12))?;
                b.store(output[0], head[0], span(13))
            },
        )
        .unwrap();
    let site = table.call_site(entry).unwrap();
    let owners = report(table.finish(), &site);
    // The program's entry glue and the helper both receive one eight byte
    // address; the entry's call frame for the helper holds one as well.
    assert_eq!(owner(&owners, 1).frame.input_bytes, 8);
    assert_eq!(owner(&owners, 0).frame.input_bytes, 8);
    assert_eq!(owner(&owners, 1).frame.calls[0].input_bytes, 8);
}

fn entry_reading_the_head_of(input: solve::SolveValueType) -> u32 {
    let integers = input;
    let mut table = solve::SolvePureCallTable::builder(arithmetic());
    let entry = table
        .add_owner(
            identity(2),
            vec![integers],
            vec![solve::SolvePureCallOutput::result(real())],
            span(10),
            |b, input, output| {
                let values = b.load(input[0], span(11))?;
                let head = b.project_element(values, vec![0], span(12))?;
                let head = b.convert(
                    solve::SolveConversionOperator::IntegerToReal,
                    head,
                    span(13),
                )?;
                b.store(output[0], head, span(14))
            },
        )
        .unwrap();
    let site = table.call_site(entry).unwrap();
    let owners = report(table.finish(), &site);
    owner(&owners, 0).frame.input_bytes
}

#[test]
fn a_large_integer_input_of_the_entry_is_passed_by_address() {
    let integers = solve::SolveValueType::tensor(
        solve::SolveScalarType::Integer(solve::SolveIntegerDomain::FULL),
        vec![1000],
    )
    .unwrap();
    // The glue stages the cells or names the typed lane that is exactly them.
    assert_eq!(entry_reading_the_head_of(integers), 8);
}

#[test]
fn an_integer_array_no_bigger_than_an_address_is_packed() {
    let integers = solve::SolveValueType::tensor(
        solve::SolveScalarType::Integer(solve::SolveIntegerDomain::FULL),
        vec![2],
    )
    .unwrap();
    assert_eq!(entry_reading_the_head_of(integers), 16);
}

fn conditional_owner(
    table: &mut solve::SolvePureCallTableBuilder,
    output: solve::SolveValueType,
    array: Option<u32>,
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
                    |r, i, o| branch(r, i, o, array),
                    |r, i, o| branch(r, i, o, array),
                )?;
                b.store(out[0], result[0], span(28))
            },
        )
        .unwrap()
}

#[test]
fn conditional_arms_share_one_span_and_a_scalar_result_is_not_doubled() {
    let mut table = solve::SolvePureCallTable::builder(arithmetic());
    let id = conditional_owner(&mut table, real(), None);
    let site = table.call_site(id).unwrap();
    let owners = report(table.finish(), &site);
    let frame = &owner(&owners, 0).frame;
    // Parent: output 8 + result register 8. Each arm: local 800 and element 8;
    // its output slot is the element register, so it adds nothing. Both arms
    // start above the result register.
    assert_eq!(frame.high_water_bytes, 16 + 808);
    assert_eq!(frame.unshared_bytes, 16 + 2 * 808);
    let roles = frame
        .regions
        .iter()
        .map(|r| (r.role, r.frame.base_bytes))
        .collect::<Vec<_>>();
    assert_eq!(roles, [("then", 16), ("else", 16)]);
}

#[test]
fn conditional_publication_of_an_array_by_address_keeps_one_cell_and_one_arm_span() {
    let mut table = solve::SolvePureCallTable::builder(arithmetic());
    let id = conditional_owner(&mut table, array_of(100), Some(100));
    let site = table.call_site(id).unwrap();
    let owners = report(table.finish(), &site);
    let frame = &owner(&owners, 0).frame;
    // The result is returned by address, in the buffer its caller passes: the
    // parent holds the output cell (8), and each arm builds its local (800)
    // above it, which is copied to the buffer.
    assert_eq!(frame.output_bytes, 8);
    assert_eq!(frame.high_water_bytes, 8 + 800);
    assert_eq!(frame.unshared_bytes, 8 + 2 * 800);
    // An arm that returns its local as the result owns no output slot.
    for region in &frame.regions {
        assert_eq!(region.frame.slot_bytes, 0);
        assert_eq!(region.frame.base_bytes, 8);
    }
}

#[test]
fn conditional_publication_of_an_array_no_bigger_than_an_address_keeps_one_result_and_one_arm_span()
{
    let mut table = solve::SolvePureCallTable::builder(arithmetic());
    let id = conditional_owner(&mut table, array_of(2), Some(2));
    let site = table.call_site(id).unwrap();
    let owners = report(table.finish(), &site);
    let frame = &owner(&owners, 0).frame;
    // Two cells (16 bytes) cost no more than an address and one indirection, so
    // the value is stored: parent output 16 + result 16, and each arm's local
    // (16) is also its output slot.
    assert_eq!(frame.output_bytes, 16);
    assert_eq!(frame.high_water_bytes, 32 + 16);
    assert_eq!(frame.unshared_bytes, 32 + 2 * 16);
    for region in &frame.regions {
        assert_eq!(region.frame.slot_bytes, 0);
        assert_eq!(region.frame.base_bytes, 32);
    }
}

fn branch<'r>(
    region: &mut solve::TypedProgramBuilder<'r>,
    input: &[solve::ProgramSlot<'r>],
    out: &[solve::ProgramSlot<'r>],
    array: Option<u32>,
) -> Result<(), solve::SolveProgramConstructionError> {
    let x = region.load(input[0], span(21))?;
    let local = region.fill(x, vec![array.unwrap_or(100)], span(22))?;
    let value = if array.is_some() {
        local
    } else {
        region.project_element(local, vec![0], span(23))?
    };
    region.store(out[0], value, span(24))
}

#[test]
fn sequential_checked_call_frames_can_report_more_than_four_gib_without_addressing_it() {
    let mut table = solve::SolvePureCallTable::builder(arithmetic());
    let large = add_sized_big(&mut table, 100_000_000);
    let repeated = table
        .add_owner(
            identity(2),
            vec![real()],
            vec![solve::SolvePureCallOutput::result(real())],
            span(10),
            |b, input, output| {
                let mut current = b.load(input[0], span(11))?;
                for index in 0..6 {
                    current = b.call(large, &[current], span(12 + index))?[0];
                }
                b.store(output[0], current, span(20))
            },
        )
        .unwrap();
    let site = table.call_site(repeated).unwrap();
    let owners = report(table.finish(), &site);
    let frame = &owner(&owners, 1).frame;
    assert_eq!(frame.calls.len(), 6);
    assert_eq!(frame.unshared_bytes, 8 + 6 * 8 + 6 * 800_000_024u64);
    assert!(frame.unshared_bytes > u64::from(u32::MAX));
    assert!(frame.high_water_bytes < 1_000_000_000);
    for call in &frame.calls {
        assert!(
            call.offset_bytes
                .checked_add(call.input_bytes)
                .and_then(|end| end.checked_add(call.output_bytes))
                .and_then(|end| end.checked_add(call.scratch_bytes))
                .is_some()
        );
    }
}

#[test]
fn actual_frame_addresses_and_wide_accounting_still_refuse_overflow() {
    let mut cursor = u32::MAX - 7;
    assert!(matches!(
        super::alloc(2, &mut cursor, 8),
        Err(super::TypedCallCompileError::SizeLimit)
    ));
    assert_eq!(cursor, u32::MAX - 7);
    let mut frame = super::FramePlan::at(0);
    frame.unshared_bytes = u64::MAX;
    assert!(matches!(
        frame.adopt(1),
        Err(super::TypedCallCompileError::SizeLimit)
    ));
    assert_eq!(frame.unshared_bytes, u64::MAX);
    assert_eq!(frame.scratch_bytes, 0);
}

fn register_chain(keep_first: bool) -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let mut table = solve::SolvePureCallTable::builder(arithmetic());
    let id = table
        .add_owner(
            identity(91),
            vec![real()],
            vec![solve::SolvePureCallOutput::result(real())],
            span(900),
            |b, input, output| {
                let x = b.load(input[0], span(901))?;
                let first = b.fill(x, vec![100], span(902))?;
                let mut value = b.project_element(first, vec![0], span(903))?;
                for index in 0..16 {
                    let tensor = b.fill(value, vec![100], span(904 + index * 2))?;
                    value = b.project_element(tensor, vec![99], span(905 + index * 2))?;
                }
                if keep_first {
                    value = b.project_element(first, vec![99], span(950))?;
                }
                b.store(output[0], value, span(951))
            },
        )
        .unwrap();
    let site = table.call_site(id).unwrap();
    (table.finish(), site)
}

#[test]
fn completed_tensor_intermediates_reuse_whole_private_allocations() {
    let (table, site) = register_chain(false);
    let owners = report(table, &site);
    let frame = &owner(&owners, 0).frame;
    // One tensor plus two scalars (an operation reads its old scalar while
    // defining the next). The immutable input and private output stay distinct.
    assert!(frame.high_water_bytes <= 8 + 800 + 16);
    assert!(frame.unshared_bytes >= 17 * 800);
}

#[test]
fn a_late_tensor_read_pins_the_original_allocation_until_that_read() {
    let (table, site) = register_chain(true);
    let owners = report(table, &site);
    let frame = &owner(&owners, 0).frame;
    assert!(frame.high_water_bytes >= 8 + 2 * 800);
    assert!(frame.high_water_bytes <= 8 + 2 * 800 + 16);
}

fn tensor(count: u32) -> solve::SolveValueType {
    solve::SolveValueType::tensor(solve::SolveScalarType::real(arithmetic()), vec![count]).unwrap()
}

/// `select(c, a, b)` over two `count` element inputs: each arm returns one of
/// them (`passes`) or builds a fresh array from `a`'s head. A helper, so its
/// inputs are the caller's values and may not be rewritten.
fn selecting_helper(
    table: &mut solve::SolvePureCallTableBuilder,
    count: u32,
    passes: [bool; 2],
) -> solve::SolvePureCallOwnerId {
    let boolean = solve::SolveValueType::scalar(solve::SolveScalarType::Boolean);
    let large = tensor(count);
    let helper = table
        .add_owner(
            identity(40),
            vec![boolean.clone(), large.clone(), large.clone()],
            vec![solve::SolvePureCallOutput::result(large.clone())],
            span(40),
            move |b, input, out| {
                let condition = b.load(input[0], span(41))?;
                let first = b.load(input[1], span(42))?;
                let second = b.load(input[2], span(43))?;
                let result = b.conditional(
                    condition,
                    &[first, second],
                    vec![large],
                    span(49),
                    |r, i, o| selecting_arm(r, i, o, passes[0].then_some(0), count),
                    |r, i, o| selecting_arm(r, i, o, passes[1].then_some(1), count),
                )?;
                b.store(out[0], result[0], span(50))
            },
        )
        .unwrap();
    let entry = table
        .add_owner(
            identity(41),
            vec![boolean, tensor(count), tensor(count)],
            vec![solve::SolvePureCallOutput::result(tensor(count))],
            span(51),
            move |b, input, out| {
                let condition = b.load(input[0], span(52))?;
                let first = b.load(input[1], span(53))?;
                let second = b.load(input[2], span(54))?;
                let value = b.call(helper, &[condition, first, second], span(55))?;
                b.store(out[0], value[0], span(56))
            },
        )
        .unwrap();
    let _ = helper;
    entry
}

fn selecting_frame(count: u32, passes: [bool; 2]) -> crate::scratch_report::ScratchFrame {
    let mut table = solve::SolvePureCallTable::builder(arithmetic());
    let entry = selecting_helper(&mut table, count, passes);
    let site = table.call_site(entry).unwrap();
    let owners = report(table.finish(), &site);
    owner(&owners, 0).frame.clone()
}

#[test]
fn an_identity_conditional_arm_does_not_copy_its_carried_input() {
    // The helper returns its result by address in a buffer its caller passes.
    // Both arms return an input: the result is a cell holding the address of
    // the chosen input, so nothing is stored beside the output cell and the
    // result cell.
    let frame = selecting_frame(1000, [true, true]);
    assert_eq!(frame.register_bytes, 0);
    assert_eq!(frame.high_water_bytes, 8 + 8);
    // One arm passes its input through: the other builds its array (8000
    // bytes and its head element) in private storage above the cells and
    // copies it into the caller's buffer; the passing arm copies nothing.
    let frame = selecting_frame(1000, [true, false]);
    assert_eq!(frame.high_water_bytes, 8 + 8 + 8000 + 8);
    // Neither arm passes a value through: the result is the caller's buffer.
    let frame = selecting_frame(1000, [false, false]);
    assert_eq!(frame.high_water_bytes, 8 + 8000 + 8);
}

#[test]
fn a_small_conditional_result_is_stored_by_value() {
    // Two elements (16 bytes) cost no more than an address and an indirection.
    let frame = selecting_frame(2, [true, true]);
    assert_eq!(frame.register_bytes, 16);
}

/// An arm of `selecting_helper`: returns capture `passed`, or builds an array
/// filled with the head of the first capture.
fn selecting_arm<'r>(
    region: &mut solve::TypedProgramBuilder<'r>,
    input: &[solve::ProgramSlot<'r>],
    out: &[solve::ProgramSlot<'r>],
    passed: Option<usize>,
    count: u32,
) -> Result<(), solve::SolveProgramConstructionError> {
    let value = if let Some(which) = passed {
        region.load(input[which], span(44))?
    } else {
        let head = region.load(input[0], span(45))?;
        let head = region.project_element(head, vec![0], span(46))?;
        region.fill(head, vec![count], span(47))?
    };
    region.store(out[0], value, span(48))
}

#[test]
fn a_large_output_of_a_helper_is_returned_by_address_in_a_buffer_its_caller_passes() {
    // The helper's output span is one cell holding the address of its value, and
    // its input tuple gains one cell for the buffer to build the value in.
    let helper = selecting_frame(1000, [true, false]);
    assert_eq!(helper.output_bytes, 8);
    assert_eq!(helper.input_bytes, 8 + 8 + 8 + 8);
    // A small output is still stored in the span itself.
    let small = selecting_frame(2, [true, false]);
    assert_eq!(small.output_bytes, 16);
    assert_eq!(small.input_bytes, 8 + 16 + 16);
}

/// `if c then (if c then a else b) else build(a)` over two large inputs.
fn nested_selecting_frame(count: u32) -> crate::scratch_report::ScratchFrame {
    let boolean = solve::SolveValueType::scalar(solve::SolveScalarType::Boolean);
    let large = tensor(count);
    let mut table = solve::SolvePureCallTable::builder(arithmetic());
    let helper = table
        .add_owner(
            identity(60),
            vec![boolean.clone(), large.clone(), large.clone()],
            vec![solve::SolvePureCallOutput::result(large.clone())],
            span(60),
            move |b, input, out| {
                let condition = b.load(input[0], span(61))?;
                let first = b.load(input[1], span(62))?;
                let second = b.load(input[2], span(63))?;
                let result = b.conditional(
                    condition,
                    &[condition, first, second],
                    vec![large.clone()],
                    span(64),
                    |r, i, o| {
                        let c = r.load(i[0], span(65))?;
                        let a = r.load(i[1], span(66))?;
                        let s = r.load(i[2], span(67))?;
                        let inner = r.conditional(
                            c,
                            &[a, s],
                            vec![tensor(count)],
                            span(68),
                            |r, i, o| selecting_arm(r, i, o, Some(0), count),
                            |r, i, o| selecting_arm(r, i, o, Some(1), count),
                        )?;
                        r.store(o[0], inner[0], span(69))
                    },
                    |r, i, o| selecting_arm(r, &i[1..], o, None, count),
                )?;
                b.store(out[0], result[0], span(70))
            },
        )
        .unwrap();
    let entry = table
        .add_owner(
            identity(61),
            vec![boolean, tensor(count), tensor(count)],
            vec![solve::SolvePureCallOutput::result(tensor(count))],
            span(71),
            move |b, input, out| {
                let condition = b.load(input[0], span(72))?;
                let first = b.load(input[1], span(73))?;
                let second = b.load(input[2], span(74))?;
                let value = b.call(helper, &[condition, first, second], span(75))?;
                b.store(out[0], value[0], span(76))
            },
        )
        .unwrap();
    let site = table.call_site(entry).unwrap();
    let owners = report(table.finish(), &site);
    owner(&owners, 0).frame.clone()
}

#[test]
fn a_nested_conditional_returned_whole_names_its_inputs_without_a_buffer_of_its_own() {
    let frame = nested_selecting_frame(1000);
    // The output cell, the outer result cell and the building arm private
    // array and head element: the nested conditional passes both inputs and
    // adds no buffer, and the result is built in the caller's buffer.
    assert_eq!(frame.high_water_bytes, 8 + 8 + 8000 + 8);
    assert_eq!(frame.register_bytes, 0);
}

/// `pair(x)`: `(x, fill(x, count))`.
fn add_pair(
    table: &mut solve::SolvePureCallTableBuilder,
    count: u32,
) -> solve::SolvePureCallOwnerId {
    table
        .add_owner(
            identity(80),
            vec![real()],
            vec![
                solve::SolvePureCallOutput::result(real()),
                solve::SolvePureCallOutput::result(tensor(count)),
            ],
            span(80),
            move |b, input, out| {
                let x = b.load(input[0], span(81))?;
                let filled = b.fill(x, vec![count], span(82))?;
                b.store(out[0], x, span(83))?;
                b.store(out[1], filled, span(84))
            },
        )
        .unwrap()
}

/// The frames of `reader(x)`, which calls `pair` and stores its first result
/// and, when `reads_both`, its second as well, and of `pair` built for it.
fn pair_frames(
    count: u32,
    reads_both: bool,
) -> (
    crate::scratch_report::ScratchFrame,
    crate::scratch_report::ScratchFrame,
) {
    let mut table = solve::SolvePureCallTable::builder(arithmetic());
    let pair = add_pair(&mut table, count);
    let mut outputs = vec![solve::SolvePureCallOutput::result(real())];
    if reads_both {
        outputs.push(solve::SolvePureCallOutput::result(tensor(count)));
    }
    let reader = table
        .add_owner(
            identity(81),
            vec![real()],
            outputs,
            span(85),
            move |b, input, out| {
                let x = b.load(input[0], span(86))?;
                let both = b.call(pair, &[x], span(87))?;
                b.store(out[0], both[0], span(88))?;
                if reads_both {
                    b.store(out[1], both[1], span(89))?;
                }
                Ok(())
            },
        )
        .unwrap();
    let site = table.call_site(reader).unwrap();
    let owners = report(table.finish(), &site);
    (
        owner(&owners, 1).frame.clone(),
        owner(&owners, 0).frame.clone(),
    )
}

#[test]
fn a_large_output_no_call_site_reads_gets_no_buffer_in_its_caller() {
    // Both results are read: the caller reserves the 8000 byte buffer the
    // helper builds its second result in, and passes its address.
    let (caller, helper) = pair_frames(1000, true);
    assert!(caller.high_water_bytes >= 8000, "{caller:?}");
    assert_eq!(helper.output_bytes, 8 + 8);
    // Only the first is read: the helper is built for it alone, so its second
    // result has no buffer and the fill is not planned.
    let (caller, helper) = pair_frames(1000, false);
    assert!(caller.high_water_bytes < 8000, "{caller:?}");
    assert_eq!(helper.register_bytes, 0, "{helper:?}");
}

/// `twin(c, a, b)`: one conditional with two results, `(a, b)` or both filled
/// from the heads of `a` and `b`.
fn add_twin(
    table: &mut solve::SolvePureCallTableBuilder,
    count: u32,
) -> solve::SolvePureCallOwnerId {
    let boolean = solve::SolveValueType::scalar(solve::SolveScalarType::Boolean);
    let large = tensor(count);
    table
        .add_owner(
            identity(90),
            vec![boolean, large.clone(), large.clone()],
            vec![
                solve::SolvePureCallOutput::result(large.clone()),
                solve::SolvePureCallOutput::result(large.clone()),
            ],
            span(90),
            move |b, input, out| {
                let condition = b.load(input[0], span(91))?;
                let first = b.load(input[1], span(92))?;
                let second = b.load(input[2], span(93))?;
                let results = b.conditional(
                    condition,
                    &[first, second],
                    vec![tensor(count), tensor(count)],
                    span(94),
                    |r, i, o| {
                        let a = r.load(i[0], span(95))?;
                        let b = r.load(i[1], span(96))?;
                        r.store(o[0], a, span(97))?;
                        r.store(o[1], b, span(98))
                    },
                    |r, i, o| {
                        for k in 0..2 {
                            let whole = r.load(i[k], span(99))?;
                            let head = r.project_element(whole, vec![0], span(100))?;
                            let filled = r.fill(head, vec![count], span(101))?;
                            r.store(o[k], filled, span(102))?;
                        }
                        Ok(())
                    },
                )?;
                b.store(out[0], results[0], span(103))?;
                b.store(out[1], results[1], span(104))
            },
        )
        .unwrap()
}

/// The frame of `twin` built for a caller that reads its first result, and its
/// second as well when `reads_both`.
fn twin_frame(count: u32, reads_both: bool) -> crate::scratch_report::ScratchFrame {
    let mut table = solve::SolvePureCallTable::builder(arithmetic());
    let twin = add_twin(&mut table, count);
    let boolean = solve::SolveValueType::scalar(solve::SolveScalarType::Boolean);
    let mut outputs = vec![solve::SolvePureCallOutput::result(tensor(count))];
    if reads_both {
        outputs.push(solve::SolvePureCallOutput::result(tensor(count)));
    }
    let reader = table
        .add_owner(
            identity(91),
            vec![boolean, tensor(count), tensor(count)],
            outputs,
            span(110),
            move |b, input, out| {
                let condition = b.load(input[0], span(111))?;
                let first = b.load(input[1], span(112))?;
                let second = b.load(input[2], span(113))?;
                let both = b.call(twin, &[condition, first, second], span(114))?;
                b.store(out[0], both[0], span(115))?;
                if reads_both {
                    b.store(out[1], both[1], span(116))?;
                }
                Ok(())
            },
        )
        .unwrap();
    let site = table.call_site(reader).unwrap();
    let owners = report(table.finish(), &site);
    owner(&owners, 0).frame.clone()
}

#[test]
fn a_conditional_result_nothing_reads_is_not_built_and_its_captures_are_not_borrowed() {
    // The building arm needs storage for the second result only when it is read.
    let both = twin_frame(1000, true);
    let first = twin_frame(1000, false);
    assert!(
        both.unshared_bytes >= first.unshared_bytes + 8000,
        "{both:?} {first:?}"
    );
}

/// The arm of `choose` that returns `if c then a else b` over its inputs
/// `(c, a, b)`.
fn nested_arm<'r>(
    region: &mut solve::TypedProgramBuilder<'r>,
    input: &[solve::ProgramSlot<'r>],
    out: &[solve::ProgramSlot<'r>],
    count: u32,
) -> Result<(), solve::SolveProgramConstructionError> {
    let c = region.load(input[0], span(140))?;
    let a = region.load(input[1], span(141))?;
    let b = region.load(input[2], span(142))?;
    let inner = region.conditional(
        c,
        &[a, b],
        vec![tensor(count)],
        span(143),
        |r, i, o| selecting_arm(r, i, o, Some(0), count),
        |r, i, o| selecting_arm(r, i, o, Some(1), count),
    )?;
    region.store(out[0], inner[0], span(145))
}

/// `choose(c, x, y)`: `if c then pick(c, x, y) else y`, where `pick` returns
/// one of its inputs by reference. The call result is read by the conditional
/// alone. When `nested`, the first arm returns it through a conditional of its
/// own.
fn choosing_frame(count: u32, nested: bool) -> crate::scratch_report::ScratchFrame {
    let boolean = solve::SolveValueType::scalar(solve::SolveScalarType::Boolean);
    let large = tensor(count);
    let mut table = solve::SolvePureCallTable::builder(arithmetic());
    let pick = table
        .add_owner(
            identity(95),
            vec![boolean.clone(), large.clone(), large.clone()],
            vec![solve::SolvePureCallOutput::result(large.clone())],
            span(120),
            {
                let large = large.clone();
                move |b, input, out| {
                    let condition = b.load(input[0], span(121))?;
                    let x = b.load(input[1], span(122))?;
                    let y = b.load(input[2], span(123))?;
                    let picked = b.conditional(
                        condition,
                        &[x, y],
                        vec![large],
                        span(124),
                        |r, i, o| selecting_arm(r, i, o, Some(0), count),
                        |r, i, o| selecting_arm(r, i, o, Some(1), count),
                    )?;
                    b.store(out[0], picked[0], span(129))
                }
            },
        )
        .unwrap();
    let choose = table
        .add_owner(
            identity(96),
            vec![boolean, large.clone(), large.clone()],
            vec![solve::SolvePureCallOutput::result(large.clone())],
            span(130),
            move |b, input, out| {
                let condition = b.load(input[0], span(131))?;
                let x = b.load(input[1], span(132))?;
                let y = b.load(input[2], span(133))?;
                let picked = b.call(pick, &[condition, x, y], span(134))?;
                let chosen = b.conditional(
                    condition,
                    &[condition, picked[0], y],
                    vec![large.clone()],
                    span(135),
                    |r, i, o| {
                        if nested {
                            nested_arm(r, i, o, count)
                        } else {
                            selecting_arm(r, i, o, Some(1), count)
                        }
                    },
                    |r, i, o| selecting_arm(r, i, o, Some(2), count),
                )?;
                b.store(out[0], chosen[0], span(140))
            },
        )
        .unwrap();
    let site = table.call_site(choose).unwrap();
    let owners = report(table.finish(), &site);
    owner(&owners, 1).frame.clone()
}

#[test]
fn a_call_result_a_conditional_returns_is_built_in_the_output_buffer() {
    // The call builds in the buffer of the output the conditional returns,
    // which the caller passes, or names an input; the conditional names it, so
    // the body holds no array of its own and no arm copies one.
    for nested in [false, true] {
        let frame = choosing_frame(1000, nested);
        assert!(frame.high_water_bytes < 8000, "{nested} {frame:?}");
        assert_eq!(frame.register_bytes, 0, "{nested} {frame:?}");
    }
}
