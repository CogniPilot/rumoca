use super::*;
use rumoca_core::{IndexBinderProvenance, Span, StructuredIndexBinder, StructuredIndexDomain};
use rumoca_eval_solve::{PureCallInvocation, TypedValue, eval_pure_call};

fn at() -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("fold_update.mo"),
        0,
        1,
    )
}

fn profile() -> solve::SolveArithmeticProfile {
    solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    )
}

fn tensor(dimensions: Vec<u32>) -> solve::SolveValueType {
    solve::SolveValueType::tensor(solve::SolveScalarType::real(profile()), dimensions).unwrap()
}

fn axis(last: i64) -> StructuredIndexDomain {
    StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            provenance: IndexBinderProvenance::Generated,
            id: 0,
            display_name: "i".into(),
            lower: 1,
            upper: last,
            step: 1,
        }],
    }
}

#[derive(Clone, Copy)]
enum Case {
    Element,
    Conditional,
    OldRead,
    Reload,
    Slice,
    View,
    SamePayload,
}

fn table(case: Case, passes: i64) -> solve::SolvePureCallTable {
    let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(profile()));
    let shape = if matches!(case, Case::Slice | Case::View) {
        vec![2, 128]
    } else {
        vec![256]
    };
    solve::SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(901).unwrap()),
            vec![tensor(shape.clone())],
            vec![
                solve::SolvePureCallOutput::result(tensor(shape)),
                solve::SolvePureCallOutput::result(real),
            ],
            at(),
            |b, inputs, outputs| {
                let vector = b.load(inputs[0], at())?;
                let zero = b.constant(solve::SolveValue::real(profile(), 0.0), at())?;
                let result = b.fold(
                    axis(passes),
                    &[vector, zero],
                    &[],
                    at(),
                    |r, carried, _, binders, next| pass(r, carried, binders, next, case),
                )?;
                b.store(outputs[0], result[0], at())?;
                b.store(outputs[1], result[1], at())
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn pass<'p>(
    r: &mut solve::TypedProgramBuilder<'p>,
    carried: &[solve::ProgramSlot<'p>],
    binders: &[solve::ProgramSlot<'p>],
    next: &[solve::ProgramSlot<'p>],
    case: Case,
) -> Result<(), solve::SolveProgramConstructionError> {
    let old = r.load(carried[0], at())?;
    let index = r.load(binders[0], at())?;
    let one = r.constant(solve::SolveValue::real(profile(), 1.0), at())?;
    let updated = match case {
        Case::Conditional => {
            let bound = r.constant(solve::SolveValue::integer(profile(), 128).unwrap(), at())?;
            let condition =
                r.compare(solve::SolveCompareOperator::LessEqual, index, bound, at())?;
            r.conditional(
                condition,
                &[old, index, one],
                vec![tensor(vec![256])],
                at(),
                |b, captures, outputs| {
                    let old = b.load(captures[0], at())?;
                    let index = b.load(captures[1], at())?;
                    let one = b.load(captures[2], at())?;
                    let value = b.update_element(old, one, &[index], at())?;
                    b.store(outputs[0], value, at())
                },
                |b, captures, outputs| {
                    let old = b.load(captures[0], at())?;
                    b.store(outputs[0], old, at())
                },
            )?[0]
        }
        Case::View => {
            let row = r.fill(one, vec![128], at())?;
            let axes = [
                solve::ProgramTensorViewAxis::Index(index),
                solve::ProgramTensorViewAxis::Span {
                    origin: 0,
                    extent: 128,
                },
            ];
            r.update_view(old, row, &axes, at())?
        }
        Case::SamePayload => r.update_slice(old, old, vec![0], at())?,
        Case::Slice => {
            let row = r.fill(one, vec![1, 128], at())?;
            r.update_slice(old, row, vec![1, 0], at())?
        }
        _ => r.update_element(old, one, &[index], at())?,
    };
    let corner = if matches!(case, Case::Slice | Case::View) {
        vec![0, 0]
    } else {
        vec![0]
    };
    let observed = match case {
        Case::OldRead => r.project_element(old, vec![0], at())?,
        Case::Reload => {
            let reloaded = r.load(carried[0], at())?;
            r.project_element(reloaded, vec![0], at())?
        }
        _ => r.project_element(updated, corner, at())?,
    };
    let sum = r.load(carried[1], at())?;
    let sum = r.binary(solve::SolveBinaryOperator::Add, sum, observed, at())?;
    r.store(next[0], updated, at())?;
    r.store(next[1], sum, at())
}

fn check(table: &solve::SolvePureCallTable, input: &[f64]) -> Vec<f64> {
    let owner = &table.owners()[0];
    let argument = TypedValue::construct(
        owner.inputs()[0].clone(),
        input
            .iter()
            .map(|x| solve::SolveValueKind::Real64(x.to_bits()))
            .collect(),
    )
    .unwrap();
    let expected = eval_pure_call(table, owner.id(), &[argument]).unwrap();
    let expected = expected
        .iter()
        .flat_map(|x| x.elements().iter())
        .map(|x| match x {
            solve::SolveValueKind::Real64(bits) => *bits,
            _ => panic!("Real outputs"),
        })
        .collect::<Vec<_>>();
    let compiled = CompiledPureCallTable::compile(table).unwrap();
    let cells = input.iter().map(|x| x.to_bits()).collect::<Vec<_>>();
    let snapshot = cells.clone();
    let mut output = vec![0; expected.len()];
    for _ in 0..2 {
        compiled
            .call_cells(
                PureCallInvocation::Primal(&owner.call_site()),
                &cells,
                &mut output,
            )
            .unwrap();
        assert_eq!(output, expected);
        assert_eq!(cells, snapshot, "the caller's input remains immutable");
    }
    output.into_iter().map(f64::from_bits).collect()
}

#[test]
fn element_updates_preserve_mixed_scalar_values() {
    let table = table(Case::Element, 256);
    let output = check(&table, &vec![-0.0; 256]);
    assert_eq!(&output[..256], &[1.0; 256]);
    assert_eq!(output[256], 256.0);
}

#[test]
fn conditional_updates_preserve_the_unchanged_arm() {
    let table = table(Case::Conditional, 256);
    let output = check(&table, &vec![-0.0; 256]);
    assert_eq!(&output[..128], &[1.0; 128]);
    assert!(
        output[128..256]
            .iter()
            .all(|x| x.to_bits() == (-0.0_f64).to_bits())
    );
}

#[test]
fn slice_updates_keep_the_other_rows_and_empty_domains_keep_the_initial_tuple() {
    let input = vec![-0.0; 256];
    let slice = table(Case::Slice, 3);
    let output = check(&slice, &input);
    assert!(
        output[..128]
            .iter()
            .all(|x| x.to_bits() == (-0.0_f64).to_bits())
    );
    assert_eq!(&output[128..256], &[1.0; 128]);
    let output = check(&table(Case::Element, 0), &input);
    assert!(
        output[..256]
            .iter()
            .all(|x| x.to_bits() == (-0.0_f64).to_bits())
    );
    assert_eq!(output[256], 0.0);
}

#[test]
fn entry_reads_and_independent_slot_reloads_keep_old_values() {
    for case in [Case::OldRead, Case::Reload] {
        let table = table(case, 256);
        let output = check(&table, &vec![0.0; 256]);
        assert_eq!(output[256], 255.0);
    }
}

#[test]
fn dynamic_row_updates_preserve_unchanged_cells_and_self_payloads() {
    let view = table(Case::View, 2);
    let output = check(&view, &vec![-0.0; 256]);
    assert_eq!(&output[..256], &[1.0; 256]);
    let aliased = table(Case::SamePayload, 3);
    let input = (0..256).map(|i| i as f64).collect::<Vec<_>>();
    assert_eq!(&check(&aliased, &input)[..256], input.as_slice());
}

fn while_table() -> solve::SolvePureCallTable {
    solve::SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(902).unwrap()),
            vec![tensor(vec![256])],
            vec![solve::SolvePureCallOutput::result(tensor(vec![256]))],
            at(),
            |b, inputs, outputs| {
                let old = b.load(inputs[0], at())?;
                let result = b.fold_while(
                    axis(256),
                    &[old],
                    &[],
                    at(),
                    |r, carried, _, _, next| {
                        let current = r.load(carried[0], at())?;
                        let corner = r.project_element(current, vec![0], at())?;
                        let bound = r.constant(solve::SolveValue::real(profile(), 3.0), at())?;
                        let proceed =
                            r.compare(solve::SolveCompareOperator::Less, corner, bound, at())?;
                        r.store(next[0], proceed, at())
                    },
                    |r, carried, _, _, next| {
                        let current = r.load(carried[0], at())?;
                        let corner = r.project_element(current, vec![0], at())?;
                        let one = r.constant(solve::SolveValue::real(profile(), 1.0), at())?;
                        let value = r.binary(solve::SolveBinaryOperator::Add, corner, one, at())?;
                        let index =
                            r.constant(solve::SolveValue::integer(profile(), 1).unwrap(), at())?;
                        let updated = r.update_element(current, value, &[index], at())?;
                        r.store(next[0], updated, at())
                    },
                )?;
                b.store(outputs[0], result[0], at())
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn continuation_reads_current_carried_values_before_each_pass() {
    let table = while_table();
    for start in [0.0, 2.0, 4.0] {
        let mut input = vec![-0.0; 256];
        input[0] = start;
        let output = check(&table, &input);
        assert_eq!(output[0], start.max(3.0));
        assert!(
            output[1..]
                .iter()
                .all(|x| x.to_bits() == (-0.0_f64).to_bits())
        );
    }
}

#[test]
fn failed_pass_does_not_modify_the_callers_aggregate() {
    let table = table(Case::Element, 257);
    let compiled = CompiledPureCallTable::compile(&table).unwrap();
    let input = vec![(-0.0_f64).to_bits(); 256];
    let snapshot = input.clone();
    let mut output = vec![0; 257];
    let error = compiled
        .call_cells(
            PureCallInvocation::Primal(&table.owners()[0].call_site()),
            &input,
            &mut output,
        )
        .unwrap_err();
    assert!(error.to_string().contains("index"), "{error}");
    assert_eq!(input, snapshot);
}

#[test]
fn directional_fold_keeps_unwritten_seed_cells_and_zeroes_written_cells() {
    let table = table(Case::Element, 128);
    let compiled = CompiledPureCallTable::compile(&table).unwrap();
    let input = (0..256).map(|i| i as f64 / 16.0).collect::<Vec<_>>();
    let seeds = (0..256).map(|i| -(i as f64) / 32.0).collect::<Vec<_>>();
    let cells = input
        .iter()
        .chain(&seeds)
        .map(|x| x.to_bits())
        .collect::<Vec<_>>();
    let mut output = vec![0; 514];
    compiled
        .call_cells(
            PureCallInvocation::Directional(table.owners()[0].call_site().directional().unwrap()),
            &cells,
            &mut output,
        )
        .unwrap();
    for i in 0..256 {
        assert_eq!(
            output[i],
            if i < 128 {
                1.0_f64.to_bits()
            } else {
                input[i].to_bits()
            }
        );
        assert_eq!(
            output[256 + i],
            if i < 128 {
                0.0_f64.to_bits()
            } else {
                seeds[i].to_bits()
            }
        );
    }
    assert_eq!(output[512], 128.0_f64.to_bits());
    assert_eq!(output[513], 0.0_f64.to_bits());
}

fn duplicate_capture_update<'p>(
    b: &mut solve::TypedProgramBuilder<'p>,
    captures: &[solve::ProgramSlot<'p>],
    outputs: &[solve::ProgramSlot<'p>],
) -> Result<(), solve::SolveProgramConstructionError> {
    let observer = b.load(captures[0], at())?;
    let source = b.load(captures[1], at())?;
    let one = b.constant(solve::SolveValue::real(profile(), 1.0), at())?;
    let index = b.constant(solve::SolveValue::integer(profile(), 1).unwrap(), at())?;
    let updated = b.update_element(source, one, &[index], at())?;
    let previous = b.project_element(observer, vec![0], at())?;
    let observed = b.binary(solve::SolveBinaryOperator::Add, previous, one, at())?;
    let second = b.constant(solve::SolveValue::integer(profile(), 2).unwrap(), at())?;
    let result = b.update_element(updated, observed, &[second], at())?;
    b.store(outputs[0], result, at())
}

fn duplicate_capture_pass<'p>(
    r: &mut solve::TypedProgramBuilder<'p>,
    carried: &[solve::ProgramSlot<'p>],
    binders: &[solve::ProgramSlot<'p>],
    next: &[solve::ProgramSlot<'p>],
) -> Result<(), solve::SolveProgramConstructionError> {
    let old = r.load(carried[0], at())?;
    let ordinal = r.load(binders[0], at())?;
    let one = r.constant(solve::SolveValue::integer(profile(), 1).unwrap(), at())?;
    let condition = r.compare(solve::SolveCompareOperator::LessEqual, ordinal, one, at())?;
    let result = r.conditional(
        condition,
        &[old, old],
        vec![tensor(vec![4])],
        at(),
        duplicate_capture_update,
        |b, captures, outputs| {
            let old = b.load(captures[0], at())?;
            b.store(outputs[0], old, at())
        },
    )?;
    r.store(next[0], result[0], at())
}

fn duplicate_capture_table(materialized: bool) -> solve::SolvePureCallTable {
    solve::SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(950).unwrap()),
            vec![tensor(vec![4])],
            vec![solve::SolvePureCallOutput::result(tensor(vec![4]))],
            at(),
            |b, inputs, outputs| {
                let raw = b.load(inputs[0], at())?;
                let initial = if materialized {
                    let zero = b.constant(solve::SolveValue::real(profile(), 0.0), at())?;
                    let zeros = b.fill(zero, vec![4], at())?;
                    b.binary(solve::SolveBinaryOperator::Add, raw, zeros, at())?
                } else {
                    raw
                };
                let result = b.fold(
                    axis(1),
                    &[initial],
                    &[],
                    at(),
                    |r, carried, _, binders, next| {
                        duplicate_capture_pass(r, carried, binders, next)
                    },
                )?;
                b.store(outputs[0], result[0], at())
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn duplicate_captures_preserve_entry_reads_after_an_update() {
    for materialized in [false, true] {
        let table = duplicate_capture_table(materialized);
        assert_eq!(
            check(&table, &[10.0, 20.0, 30.0, 40.0]),
            [1.0, 11.0, 30.0, 40.0]
        );
    }
}

// Adversarial in-place transitions: shifted self-reads, reads of the entry value
// after an update, and fused producers feeding an in-place update.
type Transition =
    for<'p> fn(
        &mut solve::TypedProgramBuilder<'p>,
        solve::ProgramRegister<'p>,
        solve::ProgramRegister<'p>,
        solve::ProgramRegister<'p>,
    ) -> Result<solve::ProgramRegister<'p>, solve::SolveProgramConstructionError>;

fn adversarial_table(transition: Transition, passes: i64) -> solve::SolvePureCallTable {
    let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(profile()));
    solve::SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(911).unwrap()),
            vec![tensor(vec![256])],
            vec![
                solve::SolvePureCallOutput::result(tensor(vec![256])),
                solve::SolvePureCallOutput::result(real),
            ],
            at(),
            |b, inputs, outputs| {
                let vector = b.load(inputs[0], at())?;
                let zero = b.constant(solve::SolveValue::real(profile(), 0.0), at())?;
                let result = b.fold(
                    axis(passes),
                    &[vector, zero],
                    &[],
                    at(),
                    |r, carried, _, binders, next| {
                        let old = r.load(carried[0], at())?;
                        let index = r.load(binders[0], at())?;
                        let one = r.constant(solve::SolveValue::real(profile(), 1.0), at())?;
                        let updated = transition(r, old, index, one)?;
                        let observed = r.project_element(updated, vec![0], at())?;
                        let sum = r.load(carried[1], at())?;
                        let sum = r.binary(solve::SolveBinaryOperator::Add, sum, observed, at())?;
                        r.store(next[0], updated, at())?;
                        r.store(next[1], sum, at())
                    },
                )?;
                b.store(outputs[0], result[0], at())?;
                b.store(outputs[1], result[1], at())
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn shift_right<'p>(
    r: &mut solve::TypedProgramBuilder<'p>,
    old: solve::ProgramRegister<'p>,
    _: solve::ProgramRegister<'p>,
    _: solve::ProgramRegister<'p>,
) -> Result<solve::ProgramRegister<'p>, solve::SolveProgramConstructionError> {
    let head = r.project_slice(old, vec![0], vec![255], at())?;
    r.update_slice(old, head, vec![1], at())
}

fn shift_left<'p>(
    r: &mut solve::TypedProgramBuilder<'p>,
    old: solve::ProgramRegister<'p>,
    _: solve::ProgramRegister<'p>,
    _: solve::ProgramRegister<'p>,
) -> Result<solve::ProgramRegister<'p>, solve::SolveProgramConstructionError> {
    let tail = r.project_slice(old, vec![1], vec![255], at())?;
    r.update_slice(old, tail, vec![0], at())
}

fn entry_read_after_update<'p>(
    r: &mut solve::TypedProgramBuilder<'p>,
    old: solve::ProgramRegister<'p>,
    index: solve::ProgramRegister<'p>,
    one: solve::ProgramRegister<'p>,
) -> Result<solve::ProgramRegister<'p>, solve::SolveProgramConstructionError> {
    let updated = r.update_element(old, one, &[index], at())?;
    r.binary(solve::SolveBinaryOperator::Add, updated, old, at())
}

fn fused_square_then_update<'p>(
    r: &mut solve::TypedProgramBuilder<'p>,
    old: solve::ProgramRegister<'p>,
    index: solve::ProgramRegister<'p>,
    one: solve::ProgramRegister<'p>,
) -> Result<solve::ProgramRegister<'p>, solve::SolveProgramConstructionError> {
    let half = r.constant(solve::SolveValue::real(profile(), 0.5), at())?;
    let halves = r.fill(half, vec![256], at())?;
    let squared = r.binary(solve::SolveBinaryOperator::Multiply, old, old, at())?;
    let damped = r.binary(solve::SolveBinaryOperator::Multiply, squared, halves, at())?;
    r.update_element(damped, one, &[index], at())
}

fn conditional_shift<'p>(
    r: &mut solve::TypedProgramBuilder<'p>,
    old: solve::ProgramRegister<'p>,
    index: solve::ProgramRegister<'p>,
    _: solve::ProgramRegister<'p>,
) -> Result<solve::ProgramRegister<'p>, solve::SolveProgramConstructionError> {
    let bound = r.constant(solve::SolveValue::integer(profile(), 2).unwrap(), at())?;
    let condition = r.compare(solve::SolveCompareOperator::LessEqual, index, bound, at())?;
    Ok(r.conditional(
        condition,
        &[old],
        vec![tensor(vec![256])],
        at(),
        |b, captures, outputs| {
            let old = b.load(captures[0], at())?;
            let head = b.project_slice(old, vec![0], vec![255], at())?;
            let shifted = b.update_slice(old, head, vec![1], at())?;
            b.store(outputs[0], shifted, at())
        },
        |b, captures, outputs| {
            let old = b.load(captures[0], at())?;
            b.store(outputs[0], old, at())
        },
    )?[0])
}

fn mirrored_element<'p>(
    r: &mut solve::TypedProgramBuilder<'p>,
    old: solve::ProgramRegister<'p>,
    index: solve::ProgramRegister<'p>,
    _: solve::ProgramRegister<'p>,
) -> Result<solve::ProgramRegister<'p>, solve::SolveProgramConstructionError> {
    let total = r.constant(solve::SolveValue::integer(profile(), 257).unwrap(), at())?;
    let mirror = r.binary(solve::SolveBinaryOperator::Subtract, total, index, at())?;
    let value = r.project_element_dynamic(old, &[mirror], at())?;
    r.update_element(old, value, &[index], at())
}

#[test]
fn adversarial_in_place_transitions_match_the_reference_bitwise() {
    let input = (0..256)
        .map(|i| f64::from(i) * 0.75 - 3.0)
        .collect::<Vec<_>>();
    let cases: [(&str, Transition); 6] = [
        ("shift_right", shift_right),
        ("shift_left", shift_left),
        ("entry_read_after_update", entry_read_after_update),
        ("fused_square_then_update", fused_square_then_update),
        ("conditional_shift", conditional_shift),
        ("mirrored_element", mirrored_element),
    ];
    for (name, transition) in cases {
        for passes in [1, 2, 7] {
            eprintln!("adversarial case {name} passes {passes}");
            check(&adversarial_table(transition, passes), &input);
        }
    }
}
