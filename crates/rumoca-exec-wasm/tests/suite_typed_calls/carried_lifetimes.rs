//! Old aliases and slot reloads outlive a selected private carried update.
use super::*;

#[derive(Clone, Copy)]
enum OldRead {
    OtherCapture,
    LaterSlot,
    LaterRegister,
    BeforeUpdateOutput,
}

fn update_branch<'region>(
    mode: OldRead,
    r: &mut solve::TypedProgramBuilder<'region>,
    inputs: &[solve::ProgramSlot<'region>],
    outputs: &[solve::ProgramSlot<'region>],
) -> Result<(), solve::SolveProgramConstructionError> {
    let old = r.load(inputs[0], span(801))?;
    if matches!(mode, OldRead::BeforeUpdateOutput) {
        r.store(outputs[1], old, span(802))?;
    }
    let index = r.constant(solve::SolveValue::integer(profile(), 1).unwrap(), span(803))?;
    let value = r.constant(solve::SolveValue::real(profile(), 99.0), span(804))?;
    let changed = r.update_element(old, value, &[index], span(805))?;
    r.store(outputs[0], changed, span(806))?;
    if !matches!(mode, OldRead::BeforeUpdateOutput) {
        let snapshot = r.load(inputs[1], span(807))?;
        r.store(outputs[1], snapshot, span(808))?;
    }
    Ok(())
}

fn unchanged_branch<'region>(
    r: &mut solve::TypedProgramBuilder<'region>,
    inputs: &[solve::ProgramSlot<'region>],
    outputs: &[solve::ProgramSlot<'region>],
) -> Result<(), solve::SolveProgramConstructionError> {
    let old = r.load(inputs[0], span(809))?;
    r.store(outputs[0], old, span(810))?;
    r.store(outputs[1], old, span(811))
}

fn table(mode: OldRead) -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let p = profile();
    let tensor =
        solve::SolveValueType::tensor(solve::SolveScalarType::real(p), vec![14400]).unwrap();
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(80),
            vec![tensor.clone()],
            vec![solve::SolvePureCallOutput::result(tensor.clone()); 3],
            span(812),
            |b, inputs, outputs| {
                let original = b.load(inputs[0], span(813))?;
                let result = b.fold(
                    domain(1, 1, 1),
                    &[original, original],
                    &[],
                    span(814),
                    |r, carried, _, _, outputs| {
                        let old = r.load(carried[0], span(815))?;
                        let active = r.constant(solve::SolveValue::boolean(true), span(816))?;
                        let result = r.conditional(
                            active,
                            &[old, old],
                            vec![tensor.clone(); 2],
                            span(817),
                            |r, inputs, outputs| update_branch(mode, r, inputs, outputs),
                            unchanged_branch,
                        )?;
                        r.store(outputs[0], result[0], span(818))?;
                        let snapshot = match mode {
                            OldRead::OtherCapture | OldRead::BeforeUpdateOutput => result[1],
                            OldRead::LaterSlot => r.load(carried[0], span(819))?,
                            OldRead::LaterRegister => old,
                        };
                        r.store(outputs[1], snapshot, span(820))
                    },
                )?;
                b.store(outputs[0], result[0], span(821))?;
                b.store(outputs[1], result[1], span(822))?;
                b.store(outputs[2], original, span(823))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    (table.finish(), site)
}

#[test]
fn private_carried_updates_keep_duplicate_captures_later_slot_reads_and_old_ssa() {
    let mut input = (0..14400).map(|i| real(i as f64)).collect::<Vec<_>>();
    input[0] = real(-0.0);
    input[1] = solve::SolveValueKind::Real64(0x7ff8_0000_0000_1234);
    let mut changed = input.clone();
    changed[0] = real(99.0);
    let expected = cells(changed.iter().chain(&input).chain(&input).copied());
    for mode in [
        OldRead::OtherCapture,
        OldRead::LaterSlot,
        OldRead::LaterRegister,
        OldRead::BeforeUpdateOutput,
    ] {
        let (table, site) = table(mode);
        let compiled = compile_pure_call_wasm(&table, &site).unwrap();
        let (status, actual) = Runner::new(&compiled).run(&cells(input.iter().copied()));
        assert_eq!(status, 0);
        assert_eq!(actual, expected);
        assert_eq!(
            actual,
            oracle(&table, &site, std::slice::from_ref(&input)).unwrap()
        );
    }
}

fn invariant_update<'region>(
    r: &mut solve::TypedProgramBuilder<'region>,
    _: &[solve::ProgramSlot<'region>],
    captures: &[solve::ProgramSlot<'region>],
    binders: &[solve::ProgramSlot<'region>],
    outputs: &[solve::ProgramSlot<'region>],
) -> Result<(), solve::SolveProgramConstructionError> {
    let invariant = r.load(captures[0], span(825))?;
    let index = r.load(binders[0], span(826))?;
    let value = r.constant(solve::SolveValue::real(profile(), 99.0), span(827))?;
    let changed = r.update_element(invariant, value, &[index], span(828))?;
    r.store(outputs[0], changed, span(829))
}

#[test]
fn inner_fold_invariant_keeps_its_outer_private_snapshot_across_iterations() {
    let p = profile();
    let tensor =
        solve::SolveValueType::tensor(solve::SolveScalarType::real(p), vec![14400]).unwrap();
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(81),
            vec![tensor.clone()],
            vec![solve::SolvePureCallOutput::result(tensor); 2],
            span(830),
            |b, inputs, outputs| {
                let original = b.load(inputs[0], span(831))?;
                let result = b.fold(
                    domain(1, 1, 1),
                    &[original],
                    &[],
                    span(832),
                    |r, carried, _, _, outputs| {
                        let old = r.load(carried[0], span(833))?;
                        let inner =
                            r.fold(domain(1, 3, 1), &[old], &[old], span(834), invariant_update)?;
                        r.store(outputs[0], inner[0], span(835))
                    },
                )?;
                b.store(outputs[0], result[0], span(836))?;
                b.store(outputs[1], original, span(837))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    let table = table.finish();
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let input = vec![real(-0.0); 14400];
    let mut expected = input.clone();
    expected[2] = real(99.0);
    let (status, actual) = Runner::new(&compiled).run(&cells(input.iter().copied()));
    assert_eq!(status, 0);
    assert_eq!(actual, cells(expected.iter().chain(&input).copied()));
    assert_eq!(actual, oracle(&table, &site, &[input]).unwrap());
}
