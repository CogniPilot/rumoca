//! A static element update (the one-element aggregate of a value written by a
//! static slice update) replaces exactly the element the checked run-time
//! update of the same in-bounds index replaces: first and last element,
//! repeated writes of one cell in order, and the original aggregate intact,
//! bit for bit on the interpreter, the native adapter and the WASM adapter.
use super::cross_backend_aliasing::{
    BITS, UPDATE, aggregate_type, bits, decode, run_everywhere, table,
};
use super::*;

/// Writes the value at the first element, then 5 and then the value again at
/// the last element, by `update`; keeps the original aggregate as a result.
fn first_last_repeated<'p>(
    b: &mut solve::TypedProgramBuilder<'p>,
    inputs: &[solve::ProgramSlot<'p>],
    outputs: &[solve::ProgramSlot<'p>],
    update: impl Fn(
        &mut solve::TypedProgramBuilder<'p>,
        solve::ProgramRegister<'p>,
        solve::ProgramRegister<'p>,
        u32,
        usize,
    ) -> Result<solve::ProgramRegister<'p>, solve::SolveProgramConstructionError>,
) -> Result<(), solve::SolveProgramConstructionError> {
    let aggregate = b.load(inputs[0], span(18101))?;
    let value = b.load(inputs[1], span(18102))?;
    let five = b.constant(solve::SolveValue::real(profile(), 5.0), span(18103))?;
    let updated = update(b, aggregate, value, 0, 18110)?;
    let updated = update(b, updated, five, 2, 18120)?;
    let updated = update(b, updated, value, 2, 18130)?;
    b.store(outputs[0], updated, span(18140))?;
    b.store(outputs[1], aggregate, span(18141))
}

fn static_updates<'p>(
    b: &mut solve::TypedProgramBuilder<'p>,
    inputs: &[solve::ProgramSlot<'p>],
    outputs: &[solve::ProgramSlot<'p>],
) -> Result<(), solve::SolveProgramConstructionError> {
    first_last_repeated(b, inputs, outputs, |b, aggregate, value, index, at| {
        b.update_element_static(aggregate, value, vec![index], span(at))
    })
}

fn run_time_updates<'p>(
    b: &mut solve::TypedProgramBuilder<'p>,
    inputs: &[solve::ProgramSlot<'p>],
    outputs: &[solve::ProgramSlot<'p>],
) -> Result<(), solve::SolveProgramConstructionError> {
    first_last_repeated(b, inputs, outputs, |b, aggregate, value, index, at| {
        let index = b.constant(
            solve::SolveValue::integer(profile(), i64::from(index) + 1).unwrap(),
            span(at),
        )?;
        b.update_element(aggregate, value, &[index], span(at + 1))
    })
}

#[test]
fn static_and_run_time_updates_of_an_in_bounds_index_agree_on_every_executor() {
    let outputs = vec![aggregate_type(), aggregate_type()];
    let (table_static, site_static) = table(outputs.clone(), static_updates);
    let (table_dynamic, site_dynamic) = table(outputs, run_time_updates);
    let static_cells = run_everywhere(&table_static, &site_static);
    assert_eq!(static_cells, run_everywhere(&table_dynamic, &site_dynamic));
    let out = decode(&static_cells);
    assert_eq!(out[..3], bits(&[UPDATE, BITS[1], UPDATE]), "updated");
    assert_eq!(out[3..], bits(&BITS), "the original aggregate is intact");
}
