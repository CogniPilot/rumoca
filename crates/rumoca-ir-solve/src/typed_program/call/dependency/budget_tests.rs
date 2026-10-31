//! Bounded may-depend derivation for long chains of static element updates.

use std::num::NonZeroU64;
use std::time::{Duration, Instant};

use rumoca_core::SourceId;

use super::*;
use crate::{
    ProgramRegister, ProgramSlot, SolveArithmeticProfile, SolveIntegerDomain,
    SolvePureCallIdentity, SolvePureCallOutput, SolvePureCallTable, SolveRealFormat,
    SolveScalarType, SolveValue, TypedProgramBuilder,
};

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name("budget.alg"), 0, 1)
}

/// The output dependencies of an owner that overwrites every element of an
/// `n`×`n` input matrix with its scalar input, one static update at a time.
fn element_fill_dependencies(n: i64) -> Vec<Box<[SolveCallDependency]>> {
    let profile =
        SolveArithmeticProfile::construct(SolveRealFormat::Binary64, SolveIntegerDomain::FULL);
    let real = SolveScalarType::real(profile);
    let extent = u32::try_from(n).unwrap();
    let matrix = SolveValueType::tensor(real, vec![extent, extent]).unwrap();
    let table = SolvePureCallTable::construct(profile, |table| {
        table.add_owner(
            SolvePureCallIdentity::issued(NonZeroU64::MIN),
            vec![matrix.clone(), SolveValueType::scalar(real)],
            vec![SolvePureCallOutput::result(matrix.clone())],
            span(),
            |body, inputs, outputs| {
                let mut filled = body.load(inputs[0], span())?;
                let value = body.load(inputs[1], span())?;
                let cells = (1..=n).flat_map(|row| (1..=n).map(move |column| (row, column)));
                for (row, column) in cells {
                    let row = body.constant(SolveValue::integer(profile, row).unwrap(), span())?;
                    let column =
                        body.constant(SolveValue::integer(profile, column).unwrap(), span())?;
                    filled = body.update_element(filled, value, &[row, column], span())?;
                }
                body.store(outputs[0], filled, span())
            },
        )?;
        Ok(())
    })
    .unwrap();
    let owner = table.owner(SolvePureCallOwnerId::from_index(0)).unwrap();
    owner.call_site().output_dependencies().to_vec()
}

/// The output dependencies of an owner that threads an `n`×`n` matrix
/// through `rounds` conditionals, each capturing it and updating one element
/// on its true arm, the shape of a correction step's branch-guarded updates.
fn conditional_update_dependencies(n: i64, rounds: i64) -> Vec<Box<[SolveCallDependency]>> {
    let profile =
        SolveArithmeticProfile::construct(SolveRealFormat::Binary64, SolveIntegerDomain::FULL);
    let real = SolveScalarType::real(profile);
    let extent = u32::try_from(n).unwrap();
    let matrix = SolveValueType::tensor(real, vec![extent, extent]).unwrap();
    let table = SolvePureCallTable::construct(profile, |table| {
        table.add_owner(
            SolvePureCallIdentity::issued(NonZeroU64::MIN),
            vec![
                matrix.clone(),
                SolveValueType::scalar(real),
                SolveValueType::scalar(SolveScalarType::Boolean),
            ],
            vec![SolvePureCallOutput::result(matrix.clone())],
            span(),
            |body, inputs, outputs| {
                let mut threaded = body.load(inputs[0], span())?;
                let value = body.load(inputs[1], span())?;
                let condition = body.load(inputs[2], span())?;
                for round in 0..rounds {
                    let cell = (round % n + 1, (round / n) % n + 1);
                    let round = GuardedRound {
                        condition,
                        value,
                        matrix: &matrix,
                        profile,
                        cell,
                    };
                    threaded = round.apply(body, threaded)?;
                }
                body.store(outputs[0], threaded, span())
            },
        )?;
        Ok(())
    })
    .unwrap();
    let owner = table.owner(SolvePureCallOwnerId::from_index(0)).unwrap();
    owner.call_site().output_dependencies().to_vec()
}

/// One round of the chain: a conditional over the threaded matrix whose true
/// arm updates `cell`.
struct GuardedRound<'round, 'program> {
    condition: ProgramRegister<'program>,
    value: ProgramRegister<'program>,
    matrix: &'round SolveValueType,
    profile: SolveArithmeticProfile,
    cell: (i64, i64),
}

impl<'program> GuardedRound<'_, 'program> {
    fn apply(
        &self,
        body: &mut TypedProgramBuilder<'program>,
        threaded: ProgramRegister<'program>,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        let (profile, cell) = (self.profile, self.cell);
        let outputs = body.conditional(
            self.condition,
            &[threaded, self.value],
            vec![self.matrix.clone()],
            span(),
            |arm, captures, outputs| update_one(arm, captures, outputs, profile, cell),
            pass_through,
        )?;
        Ok(outputs[0])
    }
}

/// The false arm of the chain: the captured matrix, unchanged.
fn pass_through<'arm>(
    arm: &mut TypedProgramBuilder<'arm>,
    captures: &[ProgramSlot<'arm>],
    outputs: &[ProgramSlot<'arm>],
) -> Result<(), SolveProgramConstructionError> {
    let held = arm.load(captures[0], span())?;
    arm.store(outputs[0], held, span())
}

/// One true arm of the chain: update element `(row, column)` of the
/// captured matrix with the captured scalar.
fn update_one<'arm>(
    arm: &mut TypedProgramBuilder<'arm>,
    captures: &[ProgramSlot<'arm>],
    outputs: &[ProgramSlot<'arm>],
    profile: SolveArithmeticProfile,
    (row, column): (i64, i64),
) -> Result<(), SolveProgramConstructionError> {
    let held = arm.load(captures[0], span())?;
    let value = arm.load(captures[1], span())?;
    let row = arm.constant(SolveValue::integer(profile, row).unwrap(), span())?;
    let column = arm.constant(SolveValue::integer(profile, column).unwrap(), span())?;
    let updated = arm.update_element(held, value, &[row, column], span())?;
    arm.store(outputs[0], updated, span())
}

#[test]
fn a_chain_of_guarded_updates_derives_within_bounded_work() {
    let started = Instant::now();
    let dependencies = conditional_update_dependencies(6, 36);
    assert!(
        started.elapsed() < Duration::from_secs(60),
        "derivation took {:?}",
        started.elapsed()
    );
    assert_eq!(dependencies.len(), 1);
    assert!(
        dependencies[0].iter().any(|run| run.inputs().contains(&0)),
        "an untaken arm keeps the matrix: {dependencies:?}"
    );
}

#[test]
fn a_short_element_fill_keeps_its_exact_summary() {
    // Every element is overwritten, so the exact summary proves the output
    // independent of the input matrix; the budget must not touch it.
    let dependencies = element_fill_dependencies(2);
    assert_eq!(dependencies.len(), 1);
    assert!(
        dependencies[0].iter().all(|run| run.first_input() == 1),
        "a 2x2 fill depends only on the scalar: {dependencies:?}"
    );
}

#[test]
fn a_long_element_fill_derives_within_bounded_work() {
    // 24x24 = 576 static updates, each composing a further restriction into
    // every surviving relation. This shape also derives within the limit
    // without the budget, so it guards bounded work and the summary's
    // content, not the original blowup; the budget's thresholds are pinned by
    // the boundary tests beside each guard.
    let started = Instant::now();
    let dependencies = element_fill_dependencies(24);
    assert!(
        started.elapsed() < Duration::from_secs(60),
        "derivation took {:?}",
        started.elapsed()
    );
    assert_eq!(dependencies.len(), 1);
    assert!(
        dependencies[0].iter().any(|run| run.inputs().contains(&1)),
        "the filled matrix depends on the scalar: {dependencies:?}"
    );
    assert!(
        dependencies[0]
            .iter()
            .filter(|run| run.inputs().contains(&0))
            .all(SolveCallDependency::is_whole_input),
        "any retained matrix dependency is the whole-input form: {dependencies:?}"
    );
}

#[test]
fn an_over_budget_composition_widens_to_exactly_the_run_it_covered() {
    let access = Coordinates::with_terms(0, 1, 1, 0, 0);
    let within = SolveCallDependency::exact_run(5, 12, Coordinates::with_terms(0, 1, 1, 0, 32))
        .remap_may_depend(&access, span())
        .unwrap();
    assert_eq!(within.inputs(), 5..12);
    assert!(!within.is_whole_input(), "32 terms compose exactly");
    let widened = SolveCallDependency::exact_run(5, 12, Coordinates::with_terms(0, 1, 1, 0, 33))
        .remap_may_depend(&access, span())
        .unwrap();
    assert_eq!(
        widened.inputs(),
        5..12,
        "every input and the count are kept"
    );
    assert!(widened.is_whole_input(), "33 terms widen to the whole run");
}
