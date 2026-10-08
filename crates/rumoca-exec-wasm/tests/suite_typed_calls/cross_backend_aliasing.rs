//! In-place aggregate updates (SOLVE-C71) are value-preserving on every
//! executor: the interpreter, the native adapter and the WASM adapter agree
//! bit for bit on programs that read an aggregate around an update, carry it
//! through a fold that also captures it, and update it under a conditional.
use super::*;
use rumoca_eval_solve::PureCallInvocation;

pub(super) const BITS: [f64; 3] = [-0.0, f64::INFINITY, 1.25];

pub(super) fn aggregate_type() -> solve::SolveValueType {
    solve::SolveValueType::tensor(solve::SolveScalarType::real(profile()), vec![3]).unwrap()
}

pub(super) fn scalar_type() -> solve::SolveValueType {
    solve::SolveValueType::scalar(solve::SolveScalarType::real(profile()))
}

type Body = for<'p> fn(
    &mut solve::TypedProgramBuilder<'p>,
    &[solve::ProgramSlot<'p>],
    &[solve::ProgramSlot<'p>],
) -> Result<(), solve::SolveProgramConstructionError>;

/// A one-owner table over `(aggregate[3], value)` with `outputs` results.
fn table(
    outputs: Vec<solve::SolveValueType>,
    body: Body,
) -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let mut builder = solve::SolvePureCallTable::builder(profile());
    let owner = builder
        .add_owner(
            identity(180),
            vec![aggregate_type(), scalar_type()],
            outputs
                .into_iter()
                .map(solve::SolvePureCallOutput::result)
                .collect(),
            span(18000),
            body,
        )
        .unwrap();
    let site = builder.call_site(owner).unwrap();
    (builder.finish(), site)
}

const UPDATE: f64 = f64::from_bits(0x7ff8_1234_5678_abcd);

fn flat_inputs() -> Vec<f64> {
    BITS.iter().copied().chain([UPDATE]).collect()
}

fn inputs() -> Vec<Vec<solve::SolveValueKind>> {
    vec![BITS.iter().copied().map(real).collect(), vec![real(UPDATE)]]
}

/// The output cells of one program on all three executors, which must agree.
pub(super) fn run_everywhere(
    table: &solve::SolvePureCallTable,
    site: &solve::SolvePureCallSite,
) -> Vec<u8> {
    let input = inputs();
    let interpreted = oracle(table, site, &input).unwrap();

    let compiled = compile_pure_call_wasm(table, site).unwrap();
    let (status, wasm) = Runner::new(&compiled).run(&cells(input.iter().flatten().copied()));
    assert_eq!(status, 0);

    let native = rumoca_exec_cranelift::compile_pure_call_table(table).unwrap();
    let flat = flat_inputs();
    let mut out = vec![0.0; interpreted.len() / 8];
    native
        .call_scalar_payload(
            PureCallInvocation::Primal(site),
            &flat,
            &mut out,
            &mut Vec::new(),
            &mut Vec::new(),
        )
        .unwrap();
    let native_cells = out
        .iter()
        .flat_map(|value| value.to_bits().to_le_bytes())
        .collect::<Vec<_>>();

    assert_eq!(wasm, interpreted, "WASM differs from the interpreter");
    assert_eq!(
        native_cells, interpreted,
        "native differs from the interpreter"
    );
    interpreted
}

pub(super) fn decode(bytes: &[u8]) -> Vec<u64> {
    bytes
        .chunks_exact(8)
        .map(|cell| u64::from_le_bytes(cell.try_into().unwrap()))
        .collect()
}

pub(super) fn bits(values: &[f64]) -> Vec<u64> {
    values.iter().map(|value| value.to_bits()).collect()
}

/// Reads the aggregate before and after its functional update.
fn read_twice<'p>(
    b: &mut solve::TypedProgramBuilder<'p>,
    inputs: &[solve::ProgramSlot<'p>],
    outputs: &[solve::ProgramSlot<'p>],
) -> Result<(), solve::SolveProgramConstructionError> {
    let aggregate = b.load(inputs[0], span(18001))?;
    let value = b.load(inputs[1], span(18002))?;
    let before = b.project_element(aggregate, vec![1], span(18003))?;
    let index = b.constant(
        solve::SolveValue::integer(profile(), 2).unwrap(),
        span(18004),
    )?;
    let updated = b.update_element(aggregate, value, &[index], span(18005))?;
    let after = b.project_element(aggregate, vec![1], span(18006))?;
    b.store(outputs[0], updated, span(18007))?;
    b.store(outputs[1], aggregate, span(18008))?;
    b.store(outputs[2], before, span(18009))?;
    b.store(outputs[3], after, span(18010))
}

#[test]
fn an_aggregate_read_before_and_after_its_update_is_unchanged_on_every_executor() {
    let (table, site) = table(
        vec![
            aggregate_type(),
            aggregate_type(),
            scalar_type(),
            scalar_type(),
        ],
        read_twice,
    );
    let out = decode(&run_everywhere(&table, &site));
    assert_eq!(out[..3], bits(&[BITS[0], UPDATE, BITS[2]]), "updated");
    assert_eq!(out[3..6], bits(&BITS), "the original aggregate is intact");
    assert_eq!(
        out[6..],
        bits(&[BITS[1], BITS[1]]),
        "both reads see the original"
    );
}

/// The aggregate's last read is the update, so the destination may alias it.
fn consumed_update<'p>(
    b: &mut solve::TypedProgramBuilder<'p>,
    inputs: &[solve::ProgramSlot<'p>],
    outputs: &[solve::ProgramSlot<'p>],
) -> Result<(), solve::SolveProgramConstructionError> {
    let aggregate = b.load(inputs[0], span(18011))?;
    let value = b.load(inputs[1], span(18012))?;
    let index = b.constant(
        solve::SolveValue::integer(profile(), 3).unwrap(),
        span(18013),
    )?;
    let updated = b.update_element(aggregate, value, &[index], span(18014))?;
    b.store(outputs[0], updated, span(18015))
}

#[test]
fn a_consumed_update_writes_the_same_bits_on_every_executor() {
    let (table, site) = table(vec![aggregate_type()], consumed_update);
    let out = decode(&run_everywhere(&table, &site));
    assert_eq!(out, bits(&[BITS[0], BITS[1], UPDATE]));
}

fn copy_arm<'p>(
    b: &mut solve::TypedProgramBuilder<'p>,
    inputs: &[solve::ProgramSlot<'p>],
    outputs: &[solve::ProgramSlot<'p>],
) -> Result<(), solve::SolveProgramConstructionError> {
    let element = b.load(inputs[0], span(18021))?;
    b.store(outputs[0], element, span(18022))
}

fn value_arm<'p>(
    b: &mut solve::TypedProgramBuilder<'p>,
    inputs: &[solve::ProgramSlot<'p>],
    outputs: &[solve::ProgramSlot<'p>],
) -> Result<(), solve::SolveProgramConstructionError> {
    let value = b.load(inputs[1], span(18023))?;
    b.store(outputs[0], value, span(18024))
}

/// Carries a copy of the aggregate through a fold that also captures it, and
/// updates each element under a conditional; the original is read again
/// after the fold.
fn fold_with_capture_and_conditional<'p>(
    b: &mut solve::TypedProgramBuilder<'p>,
    inputs: &[solve::ProgramSlot<'p>],
    outputs: &[solve::ProgramSlot<'p>],
) -> Result<(), solve::SolveProgramConstructionError> {
    let aggregate = b.load(inputs[0], span(18031))?;
    let value = b.load(inputs[1], span(18032))?;
    let folded = b.fold(
        domain(1, 3, 1),
        &[aggregate],
        &[aggregate, value],
        span(18033),
        |r, carried, captures, binders, outputs| {
            let old = r.load(carried[0], span(18034))?;
            let source = r.load(captures[0], span(18035))?;
            let value = r.load(captures[1], span(18036))?;
            let index = r.load(binders[0], span(18037))?;
            let last = r.constant(
                solve::SolveValue::integer(profile(), 3).unwrap(),
                span(18038),
            )?;
            let is_last =
                r.compare(solve::SolveCompareOperator::Equal, index, last, span(18039))?;
            let element = r.project_element_dynamic(source, &[index], span(18040))?;
            let chosen = r.conditional(
                is_last,
                &[element, value],
                vec![scalar_type()],
                span(18041),
                value_arm,
                copy_arm,
            )?;
            let updated = r.update_element(old, chosen[0], &[index], span(18042))?;
            r.store(outputs[0], updated, span(18043))
        },
    )?;
    let reread = b.project_element(aggregate, vec![2], span(18044))?;
    b.store(outputs[0], folded[0], span(18045))?;
    b.store(outputs[1], aggregate, span(18046))?;
    b.store(outputs[2], reread, span(18047))
}

#[test]
fn a_fold_capturing_its_carried_aggregate_never_mutates_the_capture_on_any_executor() {
    let (table, site) = table(
        vec![aggregate_type(), aggregate_type(), scalar_type()],
        fold_with_capture_and_conditional,
    );
    let out = decode(&run_everywhere(&table, &site));
    assert_eq!(out[..3], bits(&[BITS[0], BITS[1], UPDATE]), "folded");
    assert_eq!(out[3..6], bits(&BITS), "the captured aggregate is intact");
    assert_eq!(
        out[6],
        BITS[2].to_bits(),
        "a read after the fold is the original"
    );
}

/// The aggregate is also the update's value, which construction admits only
/// as the identity view, so the consumed destination keeps every cell.
fn update_by_itself<'p>(
    b: &mut solve::TypedProgramBuilder<'p>,
    inputs: &[solve::ProgramSlot<'p>],
    outputs: &[solve::ProgramSlot<'p>],
) -> Result<(), solve::SolveProgramConstructionError> {
    let aggregate = b.load(inputs[0], span(18051))?;
    let axes = [solve::ProgramTensorViewAxis::Span {
        origin: 0,
        extent: 3,
    }];
    let viewed = b.update_view(aggregate, aggregate, &axes, span(18052))?;
    let sliced = b.update_slice(viewed, viewed, vec![0], span(18053))?;
    b.store(outputs[0], sliced, span(18054))
}

#[test]
fn an_aggregate_updated_by_itself_is_unchanged_on_every_executor() {
    let (table, site) = table(vec![aggregate_type()], update_by_itself);
    assert_eq!(decode(&run_everywhere(&table, &site)), bits(&BITS));
}

/// Carries the aggregate through a fold whose conditional rewrites one cell
/// in place on the selected iteration and passes the aggregate through on
/// every other, so the carried storage is never copied per iteration.
fn conditional_view_carry<'p>(
    b: &mut solve::TypedProgramBuilder<'p>,
    inputs: &[solve::ProgramSlot<'p>],
    outputs: &[solve::ProgramSlot<'p>],
) -> Result<(), solve::SolveProgramConstructionError> {
    let aggregate = b.load(inputs[0], span(18061))?;
    let folded = b.fold(
        domain(1, 3, 1),
        &[aggregate],
        &[],
        span(18063),
        |r, carried, _captures, binders, outputs| {
            let old = r.load(carried[0], span(18064))?;
            let index = r.load(binders[0], span(18066))?;
            let selected = r.constant(
                solve::SolveValue::integer(profile(), 2).unwrap(),
                span(18067),
            )?;
            let is_selected = r.compare(
                solve::SolveCompareOperator::Equal,
                index,
                selected,
                span(18068),
            )?;
            let next = r.conditional(
                is_selected,
                &[old],
                vec![aggregate_type()],
                span(18069),
                |arm, inputs, outputs| {
                    let old = arm.load(inputs[0], span(18070))?;
                    let two = arm.constant(solve::SolveValue::real(profile(), 2.0), span(18071))?;
                    let doubled = arm.scale(old, two, span(18072))?;
                    let axes = [solve::ProgramTensorViewAxis::Span {
                        origin: 0,
                        extent: 3,
                    }];
                    let updated = arm.update_view(old, doubled, &axes, span(18073))?;
                    arm.store(outputs[0], updated, span(18074))
                },
                |arm, inputs, outputs| {
                    let old = arm.load(inputs[0], span(18075))?;
                    arm.store(outputs[0], old, span(18076))
                },
            )?;
            r.store(outputs[0], next[0], span(18077))
        },
    )?;
    b.store(outputs[0], folded[0], span(18078))?;
    b.store(outputs[1], aggregate, span(18079))
}

#[test]
fn a_conditional_view_carry_matches_on_every_executor_and_copies_nothing_per_iteration() {
    let (table, site) = table(
        vec![aggregate_type(), aggregate_type()],
        conditional_view_carry,
    );
    let out = decode(&run_everywhere(&table, &site));
    assert_eq!(out[..3], bits(&[-0.0, f64::INFINITY, 2.5]), "carried");
    assert_eq!(out[3..], bits(&BITS), "the source aggregate is intact");
    // The fold's carried copy happens once before the loop; the transition
    // body, the conditional and its arms copy no aggregate (24 bytes).
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let aggregate_copies = wasmparser::Parser::new(0)
        .parse_all(compiled.module_bytes())
        .filter_map(Result::ok)
        .filter_map(|payload| match payload {
            wasmparser::Payload::CodeSectionEntry(body) => Some(body),
            _ => None,
        })
        .flat_map(|body| {
            body.get_operators_reader()
                .unwrap()
                .into_iter()
                .collect::<Result<Vec<_>, _>>()
                .unwrap()
        })
        .filter(|operator| matches!(operator, wasmparser::Operator::MemoryCopy { .. }))
        .count();
    assert_eq!(aggregate_copies, 4, "static aggregate copies");
}
