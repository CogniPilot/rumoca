//! Calls inside unselected `if`/`elseif`/`else` arms and inside a later
//! `elseif` condition stay lazy, and fault when selected, identically on the
//! interpreter, the native adapter and the WASM adapter (SOLVE-C73 demands a
//! call only where its region selects it).
use super::*;
use rumoca_eval_solve::PureCallInvocation;

const SAMPLES: [f64; 3] = [11.0, 22.0, 33.0];
/// Provenance of the callee's checked indexed read.
const PROBE_READ: usize = 19010;

fn real_type() -> solve::SolveValueType {
    solve::SolveValueType::scalar(solve::SolveScalarType::real(profile()))
}

fn samples_type() -> solve::SolveValueType {
    solve::SolveValueType::tensor(solve::SolveScalarType::real(profile()), vec![3]).unwrap()
}

/// `Probe(a, j) = a[j]`, whose out-of-range index faults inside the callee.
/// The index arrives as a Real and converts exactly, so every executor
/// receives one flat Real payload.
fn probe(table: &mut solve::SolvePureCallTableBuilder, id: u64) -> solve::SolvePureCallOwnerId {
    table
        .add_owner(
            identity(id),
            vec![samples_type(), real_type()],
            vec![solve::SolvePureCallOutput::result(real_type())],
            span(19000),
            |b, inputs, outputs| {
                let values = b.load(inputs[0], span(19002))?;
                let position = b.load(inputs[1], span(19003))?;
                let index = b.convert(
                    solve::SolveConversionOperator::RealToIntegerTowardZero,
                    position,
                    span(19004),
                )?;
                let value = b.project_element_dynamic(values, &[index], span(PROBE_READ))?;
                b.store(outputs[0], value, span(19011))
            },
        )
        .unwrap()
}

fn real_constant<'p>(
    b: &mut solve::TypedProgramBuilder<'p>,
    value: f64,
    at: usize,
) -> Result<solve::ProgramRegister<'p>, solve::SolveProgramConstructionError> {
    b.constant(solve::SolveValue::real(profile(), value), span(at))
}

/// The `elseif Probe(a, j) > 20 then 200 else Probe(a, m) + 0.5` tail; captures
/// are `[a, j, m]`.
fn third_arm<'r>(
    r: &mut solve::TypedProgramBuilder<'r>,
    caps: &[solve::ProgramSlot<'r>],
    outputs: &[solve::ProgramSlot<'r>],
    in_condition: solve::SolvePureCallOwnerId,
    in_else: solve::SolvePureCallOwnerId,
) -> Result<(), solve::SolveProgramConstructionError> {
    let a = r.load(caps[0], span(19041))?;
    let j = r.load(caps[1], span(19042))?;
    let m = r.load(caps[2], span(19043))?;
    let probed = r.call(in_condition, &[a, j], span(19044))?;
    let limit = real_constant(r, 20.0, 19045)?;
    let large = r.compare(
        solve::SolveCompareOperator::Greater,
        probed[0],
        limit,
        span(19046),
    )?;
    let last = r.conditional(
        large,
        &[a, m],
        vec![real_type()],
        span(19047),
        |r, _, outputs| {
            let two_hundred = real_constant(r, 200.0, 19048)?;
            r.store(outputs[0], two_hundred, span(19049))
        },
        move |r, caps, outputs| {
            let a = r.load(caps[0], span(19050))?;
            let m = r.load(caps[1], span(19051))?;
            let value = r.call(in_else, &[a, m], span(19052))?;
            let half = real_constant(r, 0.5, 19053)?;
            let sum = r.binary(solve::SolveBinaryOperator::Add, value[0], half, span(19054))?;
            r.store(outputs[0], sum, span(19055))
        },
    )?;
    r.store(outputs[0], last[0], span(19056))
}

/// ```modelica
/// if sel == 1 then 100
/// elseif sel == 2 then Probe(a, m)
/// elseif Probe(a, j) > 20 then 200
/// else Probe(a, m) + 0.5
/// ```
fn arms() -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let p = profile();
    let mut table = solve::SolvePureCallTable::builder(p);
    // One callee per authored call occurrence (SOLVE-C73).
    let (in_second, in_condition, in_else) = (
        probe(&mut table, 19001),
        probe(&mut table, 19002),
        probe(&mut table, 19003),
    );
    let root = table
        .add_owner(
            identity(19020),
            vec![samples_type(), real_type(), real_type(), real_type()],
            vec![solve::SolvePureCallOutput::result(real_type())],
            span(19020),
            move |b, inputs, outputs| {
                let a = b.load(inputs[0], span(19021))?;
                let sel = b.load(inputs[1], span(19022))?;
                let j = b.load(inputs[2], span(19023))?;
                let m = b.load(inputs[3], span(19024))?;
                let one = real_constant(b, 1.0, 19025)?;
                let first = b.compare(solve::SolveCompareOperator::Equal, sel, one, span(19026))?;
                let result = b.conditional(
                    first,
                    &[a, sel, j, m],
                    vec![real_type()],
                    span(19027),
                    |r, _, outputs| {
                        let hundred = real_constant(r, 100.0, 19028)?;
                        r.store(outputs[0], hundred, span(19029))
                    },
                    move |r, caps, outputs| {
                        let a = r.load(caps[0], span(19030))?;
                        let sel = r.load(caps[1], span(19031))?;
                        let j = r.load(caps[2], span(19032))?;
                        let m = r.load(caps[3], span(19033))?;
                        let two = real_constant(r, 2.0, 19034)?;
                        let second =
                            r.compare(solve::SolveCompareOperator::Equal, sel, two, span(19035))?;
                        let inner = r.conditional(
                            second,
                            &[a, j, m],
                            vec![real_type()],
                            span(19036),
                            move |r, caps, outputs| {
                                let a = r.load(caps[0], span(19037))?;
                                let m = r.load(caps[2], span(19038))?;
                                let value = r.call(in_second, &[a, m], span(19039))?;
                                r.store(outputs[0], value[0], span(19040))
                            },
                            move |r, caps, outputs| {
                                third_arm(r, caps, outputs, in_condition, in_else)
                            },
                        )?;
                        r.store(outputs[0], inner[0], span(19057))
                    },
                )?;
                b.store(outputs[0], result[0], span(19058))
            },
        )
        .unwrap();
    let site = table.call_site(root).unwrap();
    (table.finish(), site)
}

fn arguments(sel: f64, j: f64, m: f64) -> Vec<f64> {
    SAMPLES.iter().copied().chain([sel, j, m]).collect()
}

/// `Ok(output bits)` or the typed fault kind and provenance the WASM
/// executor reports; the interpreter and native adapter must agree on `Ok`
/// versus refusal.
fn run_all(
    table: &solve::SolvePureCallTable,
    site: &solve::SolvePureCallSite,
    flat: &[f64],
) -> Result<u64, (TypedCallFaultKind, Span)> {
    let kinds = flat.iter().copied().map(real).collect::<Vec<_>>();
    let inputs = vec![
        kinds[..3].to_vec(),
        kinds[3..4].to_vec(),
        kinds[4..5].to_vec(),
        kinds[5..].to_vec(),
    ];
    let interpreted = oracle(table, site, &inputs);

    let compiled = compile_pure_call_wasm(table, site).unwrap();
    let mut runner = Runner::new(&compiled);
    let (status, wasm) = runner.run(&cells(kinds.iter().copied()));

    let native = rumoca_exec_cranelift::compile_pure_call_table(table).unwrap();
    let mut out = vec![0.0; 1];
    let compiled_native = native.call_scalar_payload(
        PureCallInvocation::Primal(site),
        flat,
        &mut out,
        &mut Vec::new(),
        &mut Vec::new(),
    );

    if status == 0 {
        let expected = interpreted.expect("interpreter refused a call WASM accepted");
        assert_eq!(wasm, expected, "WASM differs from the interpreter");
        compiled_native.expect("native refused a call WASM accepted");
        assert_eq!(
            out[0].to_bits().to_le_bytes().to_vec(),
            expected,
            "native differs"
        );
        Ok(u64::from_le_bytes(expected.try_into().unwrap()))
    } else {
        assert!(interpreted.is_err(), "interpreter accepted a faulting call");
        assert!(compiled_native.is_err(), "native accepted a faulting call");
        assert_eq!(wasm, vec![0xa5; 8], "a faulting call published an output");
        let fault = compiled
            .faults()
            .iter()
            .find(|fault| fault.status == status as u32)
            .expect("status names a fault");
        Err((fault.kind, fault.provenance))
    }
}

#[test]
fn unselected_call_arms_and_later_elseif_conditions_stay_lazy_on_every_executor() {
    let (table, site) = arms();
    // (selector, condition index, arm index, expected value)
    for (sel, j, m, expected) in [
        // The first branch is selected, so no later call runs.
        (1.0, 0.0, 0.0, 100.0),
        (1.0, 9.0, -4.0, 100.0),
        // The second branch is selected; the later elseif condition's
        // invalid `j` is never read.
        (2.0, 0.0, 2.0, 22.0),
        (2.0, 7.0, 3.0, 33.0),
        // The elseif condition selects its arm; the else call's invalid `m`
        // is never read.
        (3.0, 2.0, 0.0, 200.0),
        (3.0, 3.0, 9.0, 200.0),
        // The elseif condition rejects its arm and the else call runs.
        (3.0, 1.0, 3.0, 33.5),
        (4.0, 1.0, 1.0, 11.5),
    ] {
        let actual = run_all(&table, &site, &arguments(sel, j, m));
        assert_eq!(actual, Ok(f64::to_bits(expected)), "sel={sel} j={j} m={m}");
    }
}

#[test]
fn selected_call_arms_and_elseif_conditions_fault_with_the_callee_read_provenance() {
    let (table, site) = arms();
    for (sel, j, m) in [
        // The selected second-branch call reads past the end or before the start.
        (2.0, 1.0, 0.0),
        (2.0, 1.0, 4.0),
        // The elseif condition's call is evaluated and faults.
        (3.0, 0.0, 1.0),
        (3.0, 4.0, 1.0),
        // The condition is false, so the else call is evaluated and faults.
        (3.0, 1.0, 0.0),
        (4.0, 1.0, 4.0),
    ] {
        let fault = run_all(&table, &site, &arguments(sel, j, m)).unwrap_err();
        assert_eq!(
            fault,
            (TypedCallFaultKind::IndexBounds, span(PROBE_READ)),
            "sel={sel} j={j} m={m}"
        );
    }
}

#[test]
fn a_faulting_arm_does_not_poison_the_next_valid_invocation_of_the_same_instance() {
    let (table, site) = arms();
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let mut runner = Runner::new(&compiled);
    let run = |runner: &mut Runner, sel, j, m| {
        runner.run(&cells(arguments(sel, j, m).into_iter().map(real)))
    };
    assert_eq!(run(&mut runner, 3.0, 1.0, 3.0), (0, cells([real(33.5)])));
    let (status, output) = run(&mut runner, 3.0, 1.0, 0.0);
    assert_ne!(status, 0);
    assert_eq!(output, vec![0xa5; 8]);
    assert_eq!(run(&mut runner, 1.0, 0.0, 0.0), (0, cells([real(100.0)])));
    assert_eq!(run(&mut runner, 3.0, 2.0, 0.0), (0, cells([real(200.0)])));
}
