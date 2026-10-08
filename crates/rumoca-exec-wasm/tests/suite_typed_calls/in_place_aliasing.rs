//! In-place aggregate updates are value-preserving on every executor.
//!
//! The interpreter, the native adapter and the WASM adapter agree bit for bit
//! on programs that exercise the rules which let an update reuse its source
//! storage:
//!
//! - An `UpdateSlice` or `UpdateView` may write its source in place only when
//!   the destination range and the value range cannot disturb each other, so
//!   overlapping slice and view updates still read the old aggregate.
//! - A call result that aliases its argument, or a call result used as the
//!   update value, never corrupts a source that is read afterwards.
//! - A conditional arm may pass a captured aggregate straight through as its
//!   result (the pass-through arm rule) only when no other result, capture or
//!   later read observes the written storage.
//! - Fold carries follow the last-read relation (SOLVE-C71): storage is reused
//!   only after the last read of the aggregate, and a capture is never
//!   mutated by the carried copy.
//! - A faulting update leaves the input memory unchanged and publishes no
//!   output, so a later run on the same instance recovers.
//!
//! Each program is run twice: once on a computed aggregate (the input scaled
//! by one, so it is temporary storage) and once on the raw input memory.
use super::cross_backend_aliasing::{BITS, aggregate_type, bits, decode, scalar_type};
use super::*;
use rumoca_eval_solve::PureCallInvocation;

type Builder<'p> = solve::TypedProgramBuilder<'p>;
type Register<'p> = solve::ProgramRegister<'p>;
type Slot<'p> = solve::ProgramSlot<'p>;
type Built = Result<(), solve::SolveProgramConstructionError>;
type BuiltRegister<'p> = Result<Register<'p>, solve::SolveProgramConstructionError>;

const NEG_ZERO: f64 = -0.0;

/// How a program obtains the aggregate it updates.
#[derive(Clone, Copy, Debug)]
enum AggregateSource {
    /// The input memory itself.
    RawInput,
    /// The input scaled by one: a computed aggregate in temporary storage.
    Computed,
}

impl AggregateSource {
    const ALL: [Self; 2] = [Self::Computed, Self::RawInput];
}

fn integer_type() -> solve::SolveValueType {
    solve::SolveValueType::scalar(solve::SolveScalarType::integer(profile()))
}

fn load_aggregate<'p>(
    b: &mut Builder<'p>,
    slot: Slot<'p>,
    source: AggregateSource,
) -> BuiltRegister<'p> {
    let loaded = b.load(slot, span(1))?;
    match source {
        AggregateSource::RawInput => Ok(loaded),
        AggregateSource::Computed => {
            let one = real_constant(b, 1.0)?;
            b.scale(loaded, one, span(2))
        }
    }
}

fn table_with_inputs<F>(
    ins: Vec<solve::SolveValueType>,
    outs: Vec<solve::SolveValueType>,
    body: F,
) -> (solve::SolvePureCallTable, solve::SolvePureCallSite)
where
    F: for<'p> FnOnce(&mut Builder<'p>, &[Slot<'p>], &[Slot<'p>]) -> Built,
{
    let mut builder = solve::SolvePureCallTable::builder(profile());
    let owner = builder
        .add_owner(
            identity(181),
            ins,
            outs.into_iter()
                .map(solve::SolvePureCallOutput::result)
                .collect(),
            span(19000),
            body,
        )
        .unwrap();
    let site = builder.call_site(owner).unwrap();
    (builder.finish(), site)
}

fn integer_constant<'p>(
    b: &mut Builder<'p>,
    v: i64,
) -> Result<Register<'p>, solve::SolveProgramConstructionError> {
    b.constant(
        solve::SolveValue::integer(profile(), v).unwrap(),
        span(19001),
    )
}
fn real_constant<'p>(
    b: &mut Builder<'p>,
    v: f64,
) -> Result<Register<'p>, solve::SolveProgramConstructionError> {
    b.constant(solve::SolveValue::real(profile(), v), span(19002))
}
fn selector_flag<'p>(
    b: &mut Builder<'p>,
    a: Register<'p>,
    t: bool,
) -> Result<Register<'p>, solve::SolveProgramConstructionError> {
    let e = b.project_element(a, vec![2], span(19003))?;
    let c = real_constant(b, if t { 1.25 } else { 9.0 })?;
    b.compare(solve::SolveCompareOperator::Equal, e, c, span(19004))
}
fn full_axes<'p>() -> [solve::ProgramTensorViewAxis<'p>; 1] {
    [solve::ProgramTensorViewAxis::Span {
        origin: 0,
        extent: 3,
    }]
}
fn span_axes<'p>(origin: u32, extent: u32) -> [solve::ProgramTensorViewAxis<'p>; 1] {
    [solve::ProgramTensorViewAxis::Span { origin, extent }]
}

fn run_on_every_executor(
    table: &solve::SolvePureCallTable,
    site: &solve::SolvePureCallSite,
    input: Vec<Vec<solve::SolveValueKind>>,
) -> Vec<u64> {
    let interpreted = oracle(table, site, &input).unwrap();
    let compiled = compile_pure_call_wasm(table, site).unwrap();
    let bytes = cells(input.iter().flatten().copied());
    let mut runner = Runner::new(&compiled);
    for round in 0..2 {
        let (status, wasm) = runner.run(&bytes);
        assert_eq!(status, 0);
        assert_eq!(
            wasm, interpreted,
            "WASM differs from the interpreter (round {round})"
        );
        let mut retained = vec![0; bytes.len()];
        runner.memory.read(&runner.store, 0, &mut retained).unwrap();
        assert_eq!(retained, bytes, "WASM clobbered its input region");
    }
    let native = rumoca_exec_cranelift::compile_pure_call_table(table).unwrap();
    let flat: Vec<f64> = input
        .iter()
        .flatten()
        .map(|v| match v {
            solve::SolveValueKind::Real64(r) => f64::from_bits(*r),
            solve::SolveValueKind::Integer(i) => *i as f64,
            _ => panic!(),
        })
        .collect();
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
    let nb = out
        .iter()
        .flat_map(|v| v.to_bits().to_le_bytes())
        .collect::<Vec<_>>();
    assert_eq!(nb, interpreted, "native differs from the interpreter");
    decode(&interpreted)
}

/// Runs `body` on every executor, for each aggregate source, and requires the
/// output cells to equal `expect` bit for bit.
fn assert_outputs<F>(outs: Vec<solve::SolveValueType>, body: F, expect: &[f64], name: &str)
where
    F: for<'p> Fn(&mut Builder<'p>, &[Slot<'p>], &[Slot<'p>], AggregateSource) -> Built + Copy,
{
    for source in AggregateSource::ALL {
        let (table, site) = table_with_inputs(
            vec![aggregate_type(), scalar_type()],
            outs.clone(),
            move |b, i, o| body(b, i, o, source),
        );
        let input = vec![BITS.iter().copied().map(real).collect(), vec![real(7.5)]];
        let got = run_on_every_executor(&table, &site, input);
        assert_eq!(
            got,
            bits(expect),
            "{name} ({source:?}): got {:?} want {expect:?}",
            got.iter().map(|b| f64::from_bits(*b)).collect::<Vec<_>>()
        );
    }
}

#[test]
fn an_overlapping_slice_or_view_update_keeps_the_old_aggregate_readable() {
    assert_outputs(
        vec![aggregate_type()],
        |b, i, o, source| {
            let a = load_aggregate(b, i[0], source)?;
            let v = b.project_slice(a, vec![1], vec![2], span(2))?;
            let u = b.update_slice(a, v, vec![0], span(3))?;
            b.store(o[0], u, span(4))
        },
        &[f64::INFINITY, 1.25, 1.25],
        "forward overlapping slice",
    );
    assert_outputs(
        vec![aggregate_type()],
        |b, i, o, source| {
            let a = load_aggregate(b, i[0], source)?;
            let v = b.project_slice(a, vec![0], vec![2], span(2))?;
            let u = b.update_slice(a, v, vec![1], span(3))?;
            b.store(o[0], u, span(4))
        },
        &[NEG_ZERO, NEG_ZERO, f64::INFINITY],
        "backward overlapping slice",
    );
    assert_outputs(
        vec![aggregate_type()],
        |b, i, o, source| {
            let a = load_aggregate(b, i[0], source)?;
            let v = b.project_view(a, &span_axes(0, 2), span(2))?;
            let u = b.update_view(a, v, &span_axes(1, 2), span(3))?;
            b.store(o[0], u, span(4))
        },
        &[NEG_ZERO, NEG_ZERO, f64::INFINITY],
        "backward overlapping view",
    );
    assert_outputs(
        vec![aggregate_type()],
        |b, i, o, source| {
            let a = load_aggregate(b, i[0], source)?;
            let v = b.project_view(a, &span_axes(1, 2), span(2))?;
            let u = b.update_view(a, v, &span_axes(0, 2), span(3))?;
            b.store(o[0], u, span(4))
        },
        &[f64::INFINITY, 1.25, 1.25],
        "forward overlapping view",
    );
}

#[test]
fn chained_overlapping_updates_keep_every_live_intermediate_and_the_source_readable() {
    assert_outputs(
        vec![aggregate_type(), aggregate_type(), scalar_type()],
        |b, i, o, source| {
            let a = load_aggregate(b, i[0], source)?;
            let v = b.project_slice(a, vec![0], vec![2], span(2))?;
            let u = b.update_slice(a, v, vec![1], span(3))?;
            let e = b.project_element(a, vec![1], span(5))?;
            b.store(o[0], u, span(4))?;
            b.store(o[1], a, span(6))?;
            b.store(o[2], e, span(7))
        },
        &[
            NEG_ZERO,
            NEG_ZERO,
            f64::INFINITY,
            NEG_ZERO,
            f64::INFINITY,
            1.25,
            f64::INFINITY,
        ],
        "read after an overlapping update",
    );
    assert_outputs(
        vec![aggregate_type()],
        |b, i, o, source| {
            let a = load_aggregate(b, i[0], source)?;
            let v = b.project_slice(a, vec![0], vec![2], span(2))?;
            let u1 = b.update_slice(a, v, vec![1], span(3))?;
            let v2 = b.project_slice(u1, vec![1], vec![2], span(2))?;
            let u2 = b.update_slice(u1, v2, vec![0], span(3))?;
            b.store(o[0], u2, span(4))
        },
        &[NEG_ZERO, f64::INFINITY, f64::INFINITY],
        "chained overlapping updates",
    );
    assert_outputs(
        vec![aggregate_type(), aggregate_type()],
        |b, i, o, source| {
            let a = load_aggregate(b, i[0], source)?;
            let v = b.project_slice(a, vec![0], vec![2], span(2))?;
            let u1 = b.update_slice(a, v, vec![1], span(3))?;
            let v2 = b.project_slice(u1, vec![1], vec![2], span(2))?;
            let u2 = b.update_slice(u1, v2, vec![0], span(3))?;
            b.store(o[0], u2, span(4))?;
            b.store(o[1], u1, span(5))
        },
        &[
            NEG_ZERO,
            f64::INFINITY,
            f64::INFINITY,
            NEG_ZERO,
            NEG_ZERO,
            f64::INFINITY,
        ],
        "chained updates with a live intermediate",
    );
    assert_outputs(
        vec![aggregate_type()],
        |b, i, o, source| {
            let a = load_aggregate(b, i[0], source)?;
            let two = real_constant(b, 2.0)?;
            let s = b.scale(a, two, span(2))?;
            let u = b.update_view(a, s, &full_axes(), span(3))?;
            let u2 = b.update_view(u, a, &full_axes(), span(3))?;
            b.store(o[0], u2, span(4))
        },
        &[NEG_ZERO, f64::INFINITY, 1.25],
        "scaled copy restored over the source",
    );
}

fn identity_owner(builder: &mut solve::SolvePureCallTableBuilder) -> solve::SolvePureCallOwnerId {
    builder
        .add_owner(
            identity(182),
            vec![aggregate_type()],
            vec![solve::SolvePureCallOutput::result(aggregate_type())],
            span(19100),
            |b, i, o| {
                let x = b.load(i[0], span(19101))?;
                b.store(o[0], x, span(19102))
            },
        )
        .unwrap()
}
fn element_setter_owner(
    builder: &mut solve::SolvePureCallTableBuilder,
) -> solve::SolvePureCallOwnerId {
    builder
        .add_owner(
            identity(183),
            vec![aggregate_type()],
            vec![solve::SolvePureCallOutput::result(aggregate_type())],
            span(19110),
            |b, i, o| {
                let x = b.load(i[0], span(19111))?;
                let c = real_constant(b, 7.0)?;
                let two = integer_constant(b, 2)?;
                let u = b.update_element(x, c, &[two], span(19112))?;
                b.store(o[0], u, span(19113))
            },
        )
        .unwrap()
}

fn call_alias_case(
    variant: u32,
    outs: Vec<solve::SolveValueType>,
    expect: &[f64],
    source: AggregateSource,
) {
    let mut builder = solve::SolvePureCallTable::builder(profile());
    let id = identity_owner(&mut builder);
    let set = element_setter_owner(&mut builder);
    let owner = builder
        .add_owner(
            identity(184),
            vec![aggregate_type(), scalar_type()],
            outs.into_iter()
                .map(solve::SolvePureCallOutput::result)
                .collect(),
            span(19120),
            move |b, i, o| {
                let a = load_aggregate(b, i[0], source)?;
                match variant {
                    0 => {
                        let r = b.call(id, &[a], span(2))?[0];
                        let u = b.update_view(a, r, &full_axes(), span(3))?;
                        b.store(o[0], u, span(4))
                    }
                    1 => {
                        let r = b.call(id, &[a], span(2))?[0];
                        let u = b.update_view(a, r, &full_axes(), span(3))?;
                        b.store(o[0], u, span(4))?;
                        b.store(o[1], a, span(5))
                    }
                    2 => {
                        let r = b.call(set, &[a], span(2))?[0];
                        let u = b.update_slice(a, r, vec![0], span(3))?;
                        b.store(o[0], u, span(4))
                    }
                    3 => {
                        let r = b.call(set, &[a], span(2))?[0];
                        let u = b.update_slice(a, r, vec![0], span(3))?;
                        b.store(o[0], u, span(4))?;
                        b.store(o[1], a, span(5))?;
                        b.store(o[2], r, span(6))
                    }
                    4 => {
                        let r = b.call(id, &[a], span(2))?[0];
                        let v = b.project_slice(a, vec![0], vec![2], span(2))?;
                        let u = b.update_slice(r, v, vec![1], span(3))?;
                        b.store(o[0], u, span(4))
                    }
                    5 => {
                        let r = b.call(id, &[a], span(2))?[0];
                        let v = b.project_slice(a, vec![0], vec![2], span(2))?;
                        let u = b.update_slice(r, v, vec![1], span(3))?;
                        b.store(o[0], u, span(4))?;
                        b.store(o[1], a, span(5))?;
                        b.store(o[2], r, span(6))
                    }
                    6 => {
                        // value is the call result of the aggregate itself, aggregate consumed
                        let r = b.call(id, &[a], span(2))?[0];
                        let u = b.update_slice(r, r, vec![0], span(3))?;
                        b.store(o[0], u, span(4))
                    }
                    _ => unreachable!(),
                }
            },
        )
        .unwrap();
    let site = builder.call_site(owner).unwrap();
    let t = builder.finish();
    let input = vec![BITS.iter().copied().map(real).collect(), vec![real(7.5)]];
    let got = run_on_every_executor(&t, &site, input);
    let want: Vec<u64> = expect.iter().map(|v| v.to_bits()).collect();
    let g: Vec<f64> = got.iter().map(|b| f64::from_bits(*b)).collect();
    assert_eq!(
        got, want,
        "call variant {variant}: got {g:?} want {expect:?}"
    );
}

#[test]
fn a_call_result_aliasing_its_argument_never_corrupts_the_argument_or_the_update() {
    for source in AggregateSource::ALL {
        call_alias_case(
            0,
            vec![aggregate_type()],
            &[NEG_ZERO, f64::INFINITY, 1.25],
            source,
        );
        call_alias_case(
            1,
            vec![aggregate_type(), aggregate_type()],
            &[NEG_ZERO, f64::INFINITY, 1.25, NEG_ZERO, f64::INFINITY, 1.25],
            source,
        );
        call_alias_case(2, vec![aggregate_type()], &[NEG_ZERO, 7.0, 1.25], source);
        call_alias_case(
            3,
            vec![aggregate_type(), aggregate_type(), aggregate_type()],
            &[
                NEG_ZERO,
                7.0,
                1.25,
                NEG_ZERO,
                f64::INFINITY,
                1.25,
                NEG_ZERO,
                7.0,
                1.25,
            ],
            source,
        );
        call_alias_case(
            4,
            vec![aggregate_type()],
            &[NEG_ZERO, NEG_ZERO, f64::INFINITY],
            source,
        );
        call_alias_case(
            5,
            vec![aggregate_type(), aggregate_type(), aggregate_type()],
            &[
                NEG_ZERO,
                NEG_ZERO,
                f64::INFINITY,
                NEG_ZERO,
                f64::INFINITY,
                1.25,
                NEG_ZERO,
                f64::INFINITY,
                1.25,
            ],
            source,
        );
        call_alias_case(
            6,
            vec![aggregate_type()],
            &[NEG_ZERO, f64::INFINITY, 1.25],
            source,
        );
    }
}

// builds: update element 2 := 7 ; or overlapping slice update
fn update_element_arm<'r>(b: &mut Builder<'r>, i: &[Slot<'r>], o: &[Slot<'r>]) -> Built {
    let x = b.load(i[0], span(10))?;
    let c = real_constant(b, 7.0)?;
    let two = integer_constant(b, 2)?;
    let u = b.update_element(x, c, &[two], span(11))?;
    b.store(o[0], u, span(12))
}
fn pass_through_arm<'r>(b: &mut Builder<'r>, i: &[Slot<'r>], o: &[Slot<'r>]) -> Built {
    let x = b.load(i[0], span(13))?;
    b.store(o[0], x, span(14))
}
fn overlapping_slice_arm<'r>(b: &mut Builder<'r>, i: &[Slot<'r>], o: &[Slot<'r>]) -> Built {
    let x = b.load(i[0], span(10))?;
    let v = b.project_slice(x, vec![0], vec![2], span(2))?;
    let u = b.update_slice(x, v, vec![1], span(3))?;
    b.store(o[0], u, span(12))
}

type Arm = for<'r> fn(&mut Builder<'r>, &[Slot<'r>], &[Slot<'r>]) -> Built;

fn conditional_with_flag<'p>(
    b: &mut Builder<'p>,
    t: bool,
    swap: bool,
    caps: &[Register<'p>],
    outs: Vec<solve::SolveValueType>,
    build: Arm,
    pass: Arm,
) -> Result<Vec<Register<'p>>, solve::SolveProgramConstructionError> {
    let a = caps[0];
    let c = selector_flag(b, a, t)?;
    let (x, y) = if swap { (pass, build) } else { (build, pass) };
    b.conditional(c, caps, outs, span(30), x, y)
}

fn update_second_capture_arm<'r>(b: &mut Builder<'r>, i: &[Slot<'r>], o: &[Slot<'r>]) -> Built {
    let x = b.load(i[1], span(10))?;
    let c = real_constant(b, 7.0)?;
    let two = integer_constant(b, 2)?;
    let u = b.update_element(x, c, &[two], span(11))?;
    b.store(o[0], u, span(12))
}
fn pass_first_capture_arm<'r>(b: &mut Builder<'r>, i: &[Slot<'r>], o: &[Slot<'r>]) -> Built {
    let x = b.load(i[0], span(13))?;
    b.store(o[0], x, span(14))
}
fn update_second_pass_second_arm<'r>(b: &mut Builder<'r>, i: &[Slot<'r>], o: &[Slot<'r>]) -> Built {
    update_second_capture_arm(b, i, &o[..1])?;
    let y = b.load(i[1], span(15))?;
    b.store(o[1], y, span(16))
}
fn pass_first_pass_second_arm<'r>(b: &mut Builder<'r>, i: &[Slot<'r>], o: &[Slot<'r>]) -> Built {
    pass_first_capture_arm(b, i, &o[..1])?;
    let y = b.load(i[1], span(15))?;
    b.store(o[1], y, span(16))
}
fn update_and_pass_same_arm<'r>(b: &mut Builder<'r>, i: &[Slot<'r>], o: &[Slot<'r>]) -> Built {
    update_element_arm(b, i, &o[..1])?;
    let y = b.load(i[0], span(15))?;
    b.store(o[1], y, span(16))
}
fn pass_same_twice_arm<'r>(b: &mut Builder<'r>, i: &[Slot<'r>], o: &[Slot<'r>]) -> Built {
    let x = b.load(i[0], span(13))?;
    b.store(o[0], x, span(14))?;
    b.store(o[1], x, span(16))
}
fn two_updates_arm<'r>(b: &mut Builder<'r>, i: &[Slot<'r>], o: &[Slot<'r>]) -> Built {
    let x = b.load(i[0], span(10))?;
    let c = real_constant(b, 7.0)?;
    let two = integer_constant(b, 2)?;
    let u = b.update_element(x, c, &[two], span(11))?;
    b.store(o[0], u, span(12))?;
    let c9 = real_constant(b, 9.0)?;
    let three = integer_constant(b, 3)?;
    let w = b.update_element(x, c9, &[three], span(11))?;
    b.store(o[1], w, span(16))
}
fn pass_original_and_updated_arm<'r>(b: &mut Builder<'r>, i: &[Slot<'r>], o: &[Slot<'r>]) -> Built {
    let x = b.load(i[0], span(13))?;
    let one = integer_constant(b, 1)?;
    let c = real_constant(b, 0.5)?;
    let u = b.update_element(x, c, &[one], span(11))?;
    b.store(o[0], x, span(14))?;
    b.store(o[1], u, span(16))
}

fn expected_selection(sel_build: bool, built: [f64; 3]) -> [f64; 3] {
    if sel_build { built } else { BITS }
}

/// Runs `case` once for every combination of selected branch and arm order.
fn for_each_selection(mut case: impl FnMut(bool, bool)) {
    for flag in [true, false] {
        for swap in [false, true] {
            case(flag, swap);
        }
    }
}

/// The updated arm wins when the flag and the arm order select it.
fn selects_updated_arm(flag: bool, swap: bool) -> bool {
    flag != swap
}

#[test]
fn a_conditional_updating_or_passing_one_capture_never_aliases_the_result_and_the_capture() {
    for_each_selection(|t, swap| {
        let sb = selects_updated_arm(t, swap);
        let nm = format!("t={t} swap={swap}");
        let r = expected_selection(sb, [NEG_ZERO, 7.0, 1.25]);
        assert_outputs(
            vec![aggregate_type()],
            move |b, i, o, source| {
                let a = load_aggregate(b, i[0], source)?;
                let r = conditional_with_flag(
                    b,
                    t,
                    swap,
                    &[a],
                    vec![aggregate_type()],
                    update_element_arm,
                    pass_through_arm,
                )?;
                b.store(o[0], r[0], span(2))
            },
            &r,
            &format!("update arm result only {nm}"),
        );
        let mut e = r.to_vec();
        e.extend(BITS);
        assert_outputs(
            vec![aggregate_type(), aggregate_type()],
            move |b, i, o, source| {
                let a = load_aggregate(b, i[0], source)?;
                let r = conditional_with_flag(
                    b,
                    t,
                    swap,
                    &[a],
                    vec![aggregate_type()],
                    update_element_arm,
                    pass_through_arm,
                )?;
                b.store(o[0], r[0], span(2))?;
                b.store(o[1], a, span(3))
            },
            &e,
            &format!("result and the passed capture read {nm}"),
        );
        let mut e = r.to_vec();
        e.extend([5.0, f64::INFINITY, 1.25]);
        assert_outputs(
            vec![aggregate_type(), aggregate_type()],
            move |b, i, o, source| {
                let a = load_aggregate(b, i[0], source)?;
                let r = conditional_with_flag(
                    b,
                    t,
                    swap,
                    &[a],
                    vec![aggregate_type()],
                    update_element_arm,
                    pass_through_arm,
                )?;
                let five = real_constant(b, 5.0)?;
                let one = integer_constant(b, 1)?;
                let a2 = b.update_element(a, five, &[one], span(4))?;
                b.store(o[0], r[0], span(2))?;
                b.store(o[1], a2, span(3))
            },
            &e,
            &format!("capture updated after the conditional {nm}"),
        );
        let mut e = r.to_vec();
        let mut r2 = r;
        r2[0] = 5.0;
        e.extend(r2);
        assert_outputs(
            vec![aggregate_type(), aggregate_type()],
            move |b, i, o, source| {
                let a = load_aggregate(b, i[0], source)?;
                let r = conditional_with_flag(
                    b,
                    t,
                    swap,
                    &[a],
                    vec![aggregate_type()],
                    update_element_arm,
                    pass_through_arm,
                )?;
                let five = real_constant(b, 5.0)?;
                let one = integer_constant(b, 1)?;
                let r2 = b.update_element(r[0], five, &[one], span(4))?;
                b.store(o[0], r[0], span(2))?;
                b.store(o[1], r2, span(3))
            },
            &e,
            &format!("result updated after the conditional {nm}"),
        );
    });
}

#[test]
fn a_conditional_arm_updating_a_slice_overlapping_its_source_keeps_the_passed_arm_intact() {
    for_each_selection(|t, swap| {
        let sb = selects_updated_arm(t, swap);
        let nm = format!("t={t} swap={swap}");
        let r5 = expected_selection(sb, [NEG_ZERO, NEG_ZERO, f64::INFINITY]);
        assert_outputs(
            vec![aggregate_type()],
            move |b, i, o, source| {
                let a = load_aggregate(b, i[0], source)?;
                let r = conditional_with_flag(
                    b,
                    t,
                    swap,
                    &[a],
                    vec![aggregate_type()],
                    overlapping_slice_arm,
                    pass_through_arm,
                )?;
                b.store(o[0], r[0], span(2))
            },
            &r5,
            &format!("overlapping slice arm {nm}"),
        );
    });
}

#[test]
fn a_conditional_updating_a_second_capture_never_aliases_either_capture() {
    for_each_selection(|t, swap| {
        let sb = selects_updated_arm(t, swap);
        let nm = format!("t={t} swap={swap}");
        let r6 = expected_selection(sb, [NEG_ZERO, 7.0, 3.0]);
        let b_ = [NEG_ZERO, f64::INFINITY, 3.0];
        assert_outputs(
            vec![aggregate_type()],
            move |b, i, o, source| {
                let a = load_aggregate(b, i[0], source)?;
                let three = real_constant(b, 3.0)?;
                let idx = integer_constant(b, 3)?;
                let bb = b.update_element(a, three, &[idx], span(4))?;
                let r = conditional_with_flag(
                    b,
                    t,
                    swap,
                    &[a, bb],
                    vec![aggregate_type()],
                    update_second_capture_arm,
                    pass_first_capture_arm,
                )?;
                b.store(o[0], r[0], span(2))
            },
            &r6,
            &format!("update of a second capture {nm}"),
        );
        let mut e = r6.to_vec();
        e.extend(b_);
        assert_outputs(
            vec![aggregate_type(), aggregate_type()],
            move |b, i, o, source| {
                let a = load_aggregate(b, i[0], source)?;
                let three = real_constant(b, 3.0)?;
                let idx = integer_constant(b, 3)?;
                let bb = b.update_element(a, three, &[idx], span(4))?;
                let r = conditional_with_flag(
                    b,
                    t,
                    swap,
                    &[a, bb],
                    vec![aggregate_type()],
                    update_second_capture_arm,
                    pass_first_capture_arm,
                )?;
                b.store(o[0], r[0], span(2))?;
                b.store(o[1], bb, span(3))
            },
            &e,
            &format!("second capture read after {nm}"),
        );
        let mut e = r6.to_vec();
        e.extend(BITS);
        assert_outputs(
            vec![aggregate_type(), aggregate_type()],
            move |b, i, o, source| {
                let a = load_aggregate(b, i[0], source)?;
                let three = real_constant(b, 3.0)?;
                let idx = integer_constant(b, 3)?;
                let bb = b.update_element(a, three, &[idx], span(4))?;
                let r = conditional_with_flag(
                    b,
                    t,
                    swap,
                    &[a, bb],
                    vec![aggregate_type()],
                    update_second_capture_arm,
                    pass_first_capture_arm,
                )?;
                b.store(o[0], r[0], span(2))?;
                b.store(o[1], a, span(3))
            },
            &e,
            &format!("first capture read after {nm}"),
        );
    });
}

#[test]
fn a_conditional_with_several_results_never_aliases_a_passed_result_and_an_updated_result() {
    for_each_selection(|t, swap| {
        let sb = selects_updated_arm(t, swap);
        let nm = format!("t={t} swap={swap}");
        let r = expected_selection(sb, [NEG_ZERO, 7.0, 1.25]);
        let r6 = expected_selection(sb, [NEG_ZERO, 7.0, 3.0]);
        let b_ = [NEG_ZERO, f64::INFINITY, 3.0];
        let mut e = r6.to_vec();
        e.extend(b_);
        assert_outputs(
            vec![aggregate_type(), aggregate_type()],
            move |b, i, o, source| {
                let a = load_aggregate(b, i[0], source)?;
                let three = real_constant(b, 3.0)?;
                let idx = integer_constant(b, 3)?;
                let bb = b.update_element(a, three, &[idx], span(4))?;
                let r = conditional_with_flag(
                    b,
                    t,
                    swap,
                    &[a, bb],
                    vec![aggregate_type(), aggregate_type()],
                    update_second_pass_second_arm,
                    pass_first_pass_second_arm,
                )?;
                b.store(o[0], r[0], span(2))?;
                b.store(o[1], r[1], span(3))
            },
            &e,
            &format!("two results, one updated one passed {nm}"),
        );
        let mut e = r.to_vec();
        e.extend(BITS);
        assert_outputs(
            vec![aggregate_type(), aggregate_type()],
            move |b, i, o, source| {
                let a = load_aggregate(b, i[0], source)?;
                let r = conditional_with_flag(
                    b,
                    t,
                    swap,
                    &[a],
                    vec![aggregate_type(), aggregate_type()],
                    update_and_pass_same_arm,
                    pass_same_twice_arm,
                )?;
                b.store(o[0], r[0], span(2))?;
                b.store(o[1], r[1], span(3))
            },
            &e,
            &format!("one result updated, one passed, same capture {nm}"),
        );
    });
}

#[test]
fn a_conditional_passing_a_capture_beside_two_updates_of_it_never_aliases_the_updates() {
    for_each_selection(|t, swap| {
        let sb = selects_updated_arm(t, swap);
        let nm = format!("t={t} swap={swap}");
        let r = expected_selection(sb, [NEG_ZERO, 7.0, 1.25]);
        let r10_1 = if sb {
            [NEG_ZERO, f64::INFINITY, 9.0]
        } else {
            BITS
        };
        let mut e = r.to_vec();
        e.extend(r10_1);
        assert_outputs(
            vec![aggregate_type(), aggregate_type()],
            move |b, i, o, source| {
                let a = load_aggregate(b, i[0], source)?;
                let r = conditional_with_flag(
                    b,
                    t,
                    swap,
                    &[a],
                    vec![aggregate_type(), aggregate_type()],
                    two_updates_arm,
                    pass_same_twice_arm,
                )?;
                b.store(o[0], r[0], span(2))?;
                b.store(o[1], r[1], span(3))
            },
            &e,
            &format!("two updates of one capture {nm}"),
        );
        let half = [0.5, f64::INFINITY, 1.25];
        let e0 = r;
        let e1 = if sb {
            [NEG_ZERO, f64::INFINITY, 9.0]
        } else {
            half
        };
        let mut e = e0.to_vec();
        e.extend(e1);
        assert_outputs(
            vec![aggregate_type(), aggregate_type()],
            move |b, i, o, source| {
                let a = load_aggregate(b, i[0], source)?;
                let r = conditional_with_flag(
                    b,
                    t,
                    swap,
                    &[a],
                    vec![aggregate_type(), aggregate_type()],
                    two_updates_arm,
                    pass_original_and_updated_arm,
                )?;
                b.store(o[0], r[0], span(2))?;
                b.store(o[1], r[1], span(3))
            },
            &e,
            &format!("two updates against passing original and updated {nm}"),
        );
    });
}

fn index_equals<'p>(
    b: &mut Builder<'p>,
    binder: Register<'p>,
    n: i64,
) -> Result<Register<'p>, solve::SolveProgramConstructionError> {
    let k = integer_constant(b, n)?;
    b.compare(solve::SolveCompareOperator::Equal, binder, k, span(40))
}

#[test]
fn a_fold_carry_updated_under_a_conditional_never_mutates_a_capture_or_a_later_read() {
    assert_outputs(
        vec![aggregate_type(), aggregate_type()],
        |b, i, o, source| {
            let a = load_aggregate(b, i[0], source)?;
            let f = b.fold(domain(1, 3, 1), &[a], &[], span(2), |r, c, _, bd, out| {
                let old = r.load(c[0], span(3))?;
                let ix = r.load(bd[0], span(4))?;
                let cnd = index_equals(r, ix, 2)?;
                let n = r.conditional(
                    cnd,
                    &[old],
                    vec![aggregate_type()],
                    span(5),
                    overlapping_slice_arm,
                    pass_through_arm,
                )?;
                r.store(out[0], n[0], span(6))
            })?;
            b.store(o[0], f[0], span(7))?;
            b.store(o[1], a, span(8))
        },
        &[
            NEG_ZERO,
            NEG_ZERO,
            f64::INFINITY,
            NEG_ZERO,
            f64::INFINITY,
            1.25,
        ],
        "fold with an overlapping slice arm",
    );
    assert_outputs(
        vec![aggregate_type(), aggregate_type()],
        |b, i, o, source| {
            let a = load_aggregate(b, i[0], source)?;
            let f = b.fold(
                domain(1, 3, 1),
                &[a, a],
                &[],
                span(2),
                |r, c, _, bd, out| {
                    let old0 = r.load(c[0], span(3))?;
                    let old1 = r.load(c[1], span(3))?;
                    let ix = r.load(bd[0], span(4))?;
                    let cnd = index_equals(r, ix, 2)?;
                    let n = r.conditional(
                        cnd,
                        &[old0],
                        vec![aggregate_type()],
                        span(5),
                        update_element_arm,
                        pass_through_arm,
                    )?;
                    let e = r.project_element(n[0], vec![1], span(5))?;
                    let three = integer_constant(r, 3)?;
                    let n1 = r.update_element(old1, e, &[three], span(5))?;
                    r.store(out[0], n[0], span(6))?;
                    r.store(out[1], n1, span(6))
                },
            )?;
            b.store(o[0], f[0], span(7))?;
            b.store(o[1], f[1], span(8))
        },
        &[NEG_ZERO, 7.0, 1.25, NEG_ZERO, f64::INFINITY, 7.0],
        "fold with two carries",
    );
}

#[test]
fn a_fold_swapping_or_rebuilding_carries_never_mutates_a_capture_or_the_other_carry() {
    for mode in 0..3 {
        let (e0, e1): ([f64; 3], [f64; 3]) = match mode {
            0 => ([NEG_ZERO, f64::INFINITY, 3.0], BITS),
            1 => ([NEG_ZERO, 7.0, 3.0], [NEG_ZERO, f64::INFINITY, 3.0]),
            _ => ([NEG_ZERO, 7.0, 3.0], BITS),
        };
        let mut e = e0.to_vec();
        e.extend(e1);
        assert_outputs(
            vec![aggregate_type(), aggregate_type()],
            move |b, i, o, source| {
                let a = load_aggregate(b, i[0], source)?;
                let three = real_constant(b, 3.0)?;
                let k3 = integer_constant(b, 3)?;
                let bb = b.update_element(a, three, &[k3], span(2))?;
                let f = b.fold(
                    domain(1, 3, 1),
                    &[a, bb],
                    &[],
                    span(2),
                    move |r, c, _, bd, out| {
                        let old0 = r.load(c[0], span(3))?;
                        let old1 = r.load(c[1], span(3))?;
                        let ix = r.load(bd[0], span(4))?;
                        let cnd = index_equals(r, ix, 2)?;
                        let (x, y): (Arm, Arm) = match mode {
                            0 => (swap_arm, keep_arm),
                            1 => (update_second_pass_second_arm, pass_first_pass_second_arm),
                            _ => (update_second_pass_first_arm, keep_arm),
                        };
                        let n = r.conditional(
                            cnd,
                            &[old0, old1],
                            vec![aggregate_type(), aggregate_type()],
                            span(5),
                            x,
                            y,
                        )?;
                        r.store(out[0], n[0], span(6))?;
                        r.store(out[1], n[1], span(6))
                    },
                )?;
                b.store(o[0], f[0], span(7))?;
                b.store(o[1], f[1], span(8))
            },
            &e,
            &format!("fold swapping carries, mode {mode}"),
        );
    }
    for at in [2, 3] {
        let c = [NEG_ZERO, 7.0, 1.25];
        let p = if at == 2 { c } else { BITS };
        let mut e = c.to_vec();
        e.extend(p);
        assert_outputs(
            vec![aggregate_type(), aggregate_type()],
            move |b, i, o, source| {
                let a = load_aggregate(b, i[0], source)?;
                let f = b.fold(
                    domain(1, 3, 1),
                    &[a, a],
                    &[],
                    span(2),
                    move |r, c, _, bd, out| {
                        let old = r.load(c[0], span(3))?;
                        let _ = r.load(c[1], span(3))?;
                        let ix = r.load(bd[0], span(4))?;
                        let cnd = index_equals(r, ix, at)?;
                        let n = r.conditional(
                            cnd,
                            &[old],
                            vec![aggregate_type()],
                            span(5),
                            update_element_arm,
                            pass_through_arm,
                        )?;
                        r.store(out[0], n[0], span(6))?;
                        r.store(out[1], old, span(6))
                    },
                )?;
                b.store(o[0], f[0], span(7))?;
                b.store(o[1], f[1], span(8))
            },
            &e,
            &format!("fold with a lagging carry, update at {at}"),
        );
    }
}

#[test]
fn a_fold_inside_a_conditional_leaves_the_passed_through_aggregate_intact() {
    for t in [true, false] {
        for swap in [false, true] {
            let sb = t != swap;
            let r_ = expected_selection(sb, [NEG_ZERO, 7.0, 1.25]);
            let mut e = r_.to_vec();
            e.extend(BITS);
            assert_outputs(
                vec![aggregate_type(), aggregate_type()],
                move |b, i, o, source| {
                    let a = load_aggregate(b, i[0], source)?;
                    let n = conditional_with_flag(
                        b,
                        t,
                        swap,
                        &[a],
                        vec![aggregate_type()],
                        conditional_fold_arm,
                        pass_through_arm,
                    )?;
                    b.store(o[0], n[0], span(2))?;
                    b.store(o[1], a, span(3))
                },
                &e,
                &format!("conditional around a fold, t={t} swap={swap}"),
            );
            assert_outputs(
                vec![aggregate_type()],
                move |b, i, o, source| {
                    let a = load_aggregate(b, i[0], source)?;
                    let n = conditional_with_flag(
                        b,
                        t,
                        swap,
                        &[a],
                        vec![aggregate_type()],
                        conditional_fold_arm,
                        pass_through_arm,
                    )?;
                    b.store(o[0], n[0], span(2))
                },
                &r_,
                &format!("conditional around a fold, result only, t={t} swap={swap}"),
            );
        }
    }
}

fn swap_arm<'r>(b: &mut Builder<'r>, i: &[Slot<'r>], o: &[Slot<'r>]) -> Built {
    let x = b.load(i[0], span(13))?;
    let y = b.load(i[1], span(13))?;
    b.store(o[0], y, span(14))?;
    b.store(o[1], x, span(14))
}
fn keep_arm<'r>(b: &mut Builder<'r>, i: &[Slot<'r>], o: &[Slot<'r>]) -> Built {
    let x = b.load(i[0], span(13))?;
    let y = b.load(i[1], span(13))?;
    b.store(o[0], x, span(14))?;
    b.store(o[1], y, span(14))
}
// out0 built from in1, out1 = in0 (cross)
fn update_second_pass_first_arm<'r>(b: &mut Builder<'r>, i: &[Slot<'r>], o: &[Slot<'r>]) -> Built {
    update_second_capture_arm(b, i, &o[..1])?;
    let y = b.load(i[0], span(15))?;
    b.store(o[1], y, span(16))
}
fn conditional_fold_arm<'r>(b: &mut Builder<'r>, i: &[Slot<'r>], o: &[Slot<'r>]) -> Built {
    let x = b.load(i[0], span(10))?;
    let f = b.fold(domain(1, 3, 1), &[x], &[], span(2), |r, c, _, bd, out| {
        let old = r.load(c[0], span(3))?;
        let ix = r.load(bd[0], span(4))?;
        let cnd = index_equals(r, ix, 2)?;
        let n = r.conditional(
            cnd,
            &[old],
            vec![aggregate_type()],
            span(5),
            update_element_arm,
            pass_through_arm,
        )?;
        r.store(out[0], n[0], span(6))
    })?;
    b.store(o[0], f[0], span(7))
}

fn fold_call_alias_case(source: AggregateSource) {
    let mut builder = solve::SolvePureCallTable::builder(profile());
    let id = identity_owner(&mut builder);
    let owner = builder
        .add_owner(
            identity(185),
            vec![aggregate_type(), scalar_type()],
            vec![
                solve::SolvePureCallOutput::result(aggregate_type()),
                solve::SolvePureCallOutput::result(aggregate_type()),
            ],
            span(19120),
            move |b, i, o| {
                let a = load_aggregate(b, i[0], source)?;
                let f = b.fold(
                    domain(1, 3, 1),
                    &[a, a],
                    &[],
                    span(2),
                    move |r, c, _, _bd, out| {
                        let old = r.load(c[0], span(3))?;
                        let _p = r.load(c[1], span(3))?;
                        let rr = r.call(id, &[old], span(4))?[0];
                        let v = r.project_slice(rr, vec![0], vec![2], span(5))?;
                        let n = r.update_slice(old, v, vec![1], span(6))?;
                        r.store(out[0], n, span(7))?;
                        r.store(out[1], rr, span(8))
                    },
                )?;
                b.store(o[0], f[0], span(9))?;
                b.store(o[1], f[1], span(10))
            },
        )
        .unwrap();
    let site = builder.call_site(owner).unwrap();
    let t = builder.finish();
    let input = vec![BITS.iter().copied().map(real).collect(), vec![real(7.5)]];
    let got = run_on_every_executor(&t, &site, input);
    let g: Vec<f64> = got.iter().map(|b| f64::from_bits(*b)).collect();
    let want = [NEG_ZERO, NEG_ZERO, NEG_ZERO, NEG_ZERO, NEG_ZERO, NEG_ZERO];
    assert_eq!(
        got,
        want.iter().map(|v: &f64| v.to_bits()).collect::<Vec<_>>(),
        "{source:?}: got {g:?}"
    );
}

#[test]
fn a_fold_carrying_a_call_result_alias_never_corrupts_the_carried_aggregate() {
    for source in AggregateSource::ALL {
        fold_call_alias_case(source);
    }
}

fn assert_fault_atomic(
    table: &solve::SolvePureCallTable,
    site: &solve::SolvePureCallSite,
    ok: Vec<Vec<solve::SolveValueKind>>,
    bad: Vec<Vec<Vec<solve::SolveValueKind>>>,
) {
    let compiled = compile_pure_call_wasm(table, site).unwrap();
    let mut runner = Runner::new(&compiled);
    for input in bad {
        let bytes = cells(input.iter().flatten().copied());
        let (status, out) = runner.run(&bytes);
        assert!(status > 0, "no fault for {input:?}");
        assert_eq!(out, vec![0xa5; out.len()], "published on fault");
        assert!(oracle(table, site, &input).is_err());
        let mut retained = vec![0; bytes.len()];
        runner.memory.read(&runner.store, 0, &mut retained).unwrap();
        assert_eq!(
            retained, bytes,
            "inputs clobbered by assert_fault_atomic run"
        );
        let okb = cells(ok.iter().flatten().copied());
        let (status, out) = runner.run(&okb);
        assert_eq!(status, 0);
        let want = oracle(table, site, &ok).unwrap();
        assert_eq!(out, want, "recover");
    }
}

fn matrix_type() -> solve::SolveValueType {
    solve::SolveValueType::tensor(solve::SolveScalarType::real(profile()), vec![2, 3]).unwrap()
}
fn row_axes<'p>(k: Register<'p>) -> [solve::ProgramTensorViewAxis<'p>; 2] {
    [
        solve::ProgramTensorViewAxis::Index(k),
        solve::ProgramTensorViewAxis::Span {
            origin: 0,
            extent: 3,
        },
    ]
}

fn integer_cell(value: i64) -> Vec<solve::SolveValueKind> {
    vec![solve::SolveValueKind::Integer(value)]
}

fn matrix_cells() -> Vec<solve::SolveValueKind> {
    [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
        .iter()
        .copied()
        .map(real)
        .collect()
}

fn row_cells() -> Vec<solve::SolveValueKind> {
    BITS.iter().copied().map(real).collect()
}

/// Row updates by a dynamic index fault on every index outside the matrix,
/// publish nothing, leave the input memory unchanged and let the next run
/// recover.
fn out_of_range_row_index_faults_atomically(source: AggregateSource) {
    let (a2, vv) = (matrix_cells(), row_cells());
    let ints = integer_cell;
    let (t, s) = table_with_inputs(
        vec![matrix_type(), aggregate_type(), integer_type()],
        vec![matrix_type()],
        |b, i, o| {
            let a = load_aggregate(b, i[0], source)?;
            let v = b.load(i[1], span(2))?;
            let ix = b.load(i[2], span(3))?;
            let u = b.update_view(a, v, &row_axes(ix), span(4))?;
            b.store(o[0], u, span(5))
        },
    );
    let ok = vec![a2.clone(), vv.clone(), ints(2)];
    let bad = [0i64, 3, -1, i64::MIN, i64::MAX]
        .iter()
        .map(|k| vec![a2.clone(), vv.clone(), ints(*k)])
        .collect();
    assert_fault_atomic(&t, &s, ok.clone(), bad);
    let got = run_on_every_executor(&t, &s, ok);
    assert_eq!(
        got,
        [1.0, 2.0, 3.0, NEG_ZERO, f64::INFINITY, 1.25]
            .iter()
            .map(|v: &f64| v.to_bits())
            .collect::<Vec<_>>()
    );
}

/// The row is the fold binder plus a limit; a limit of one faults on the second
/// iteration after the first wrote its row in place.
fn fold_fault_after_an_in_place_row_write_is_atomic(source: AggregateSource) {
    let (a2, vv) = (matrix_cells(), row_cells());
    let ints = integer_cell;
    let (t, s) = table_with_inputs(
        vec![matrix_type(), aggregate_type(), integer_type()],
        vec![matrix_type()],
        |b, i, o| {
            let a = load_aggregate(b, i[0], source)?;
            let v = b.load(i[1], span(2))?;
            let lim = b.load(i[2], span(3))?;
            let f = b.fold(
                domain(1, 2, 1),
                &[a],
                &[v, lim],
                span(4),
                |r, c, caps, bd, out| {
                    let old = r.load(c[0], span(5))?;
                    let v = r.load(caps[0], span(6))?;
                    let lim = r.load(caps[1], span(6))?;
                    let ix = r.load(bd[0], span(7))?;
                    let ix = r.binary(solve::SolveBinaryOperator::Add, ix, lim, span(7))?;
                    let u = r.update_view(old, v, &row_axes(ix), span(8))?;
                    r.store(out[0], u, span(9))
                },
            )?;
            b.store(o[0], f[0], span(10))
        },
    );
    let okf = vec![a2.clone(), vv.clone(), ints(0)];
    assert_fault_atomic(
        &t,
        &s,
        okf.clone(),
        vec![vec![a2.clone(), vv.clone(), ints(1)]],
    );
    let got = run_on_every_executor(&t, &s, okf);
    assert_eq!(
        got,
        [NEG_ZERO, f64::INFINITY, 1.25, NEG_ZERO, f64::INFINITY, 1.25]
            .iter()
            .map(|v: &f64| v.to_bits())
            .collect::<Vec<_>>()
    );
}

/// A conditional row update with an out-of-range index faults only on the
/// iteration where its arm is active.
fn conditional_row_update_fault_case(source: AggregateSource, active_at: i64, expect_fault: bool) {
    let (a2, vv) = (matrix_cells(), row_cells());
    let (t, s) = table_with_inputs(
        vec![matrix_type(), aggregate_type(), integer_type()],
        vec![matrix_type()],
        move |b, i, o| {
            let a = load_aggregate(b, i[0], source)?;
            let v = b.load(i[1], span(2))?;
            let ixr = b.load(i[2], span(3))?;
            let f = b.fold(
                domain(1, 3, 1),
                &[a],
                &[v, ixr],
                span(4),
                move |r, c, caps, bd, out| {
                    let old = r.load(c[0], span(5))?;
                    let v = r.load(caps[0], span(6))?;
                    let ixr = r.load(caps[1], span(6))?;
                    let ix = r.load(bd[0], span(7))?;
                    let cnd = index_equals(r, ix, active_at)?;
                    let n = r.conditional(
                        cnd,
                        &[old, v, ixr],
                        vec![matrix_type()],
                        span(8),
                        |x, i, o| {
                            let old = x.load(i[0], span(1))?;
                            let v = x.load(i[1], span(2))?;
                            let k = x.load(i[2], span(3))?;
                            let u = x.update_view(old, v, &row_axes(k), span(4))?;
                            x.store(o[0], u, span(5))
                        },
                        |x, i, o| {
                            let old = x.load(i[0], span(1))?;
                            x.store(o[0], old, span(5))
                        },
                    )?;
                    r.store(out[0], n[0], span(9))
                },
            )?;
            b.store(o[0], f[0], span(10))
        },
    );
    let input = [a2.clone(), vv.clone(), integer_cell(99)];
    let compiled = compile_pure_call_wasm(&t, &s).unwrap();
    let mut runner = Runner::new(&compiled);
    let bytes = cells(input.iter().flatten().copied());
    let (status, out) = runner.run(&bytes);
    if expect_fault {
        assert!(status > 0);
        assert_eq!(out, vec![0xa5; out.len()]);
    } else {
        assert_eq!(status, 0);
        assert_eq!(out, cells(a2.iter().copied()));
    }
    let mut retained = vec![0; bytes.len()];
    runner.memory.read(&runner.store, 0, &mut retained).unwrap();
    assert_eq!(retained, bytes);
}

#[test]
fn a_faulting_in_place_update_publishes_nothing_and_leaves_input_memory_unchanged() {
    for source in AggregateSource::ALL {
        out_of_range_row_index_faults_atomically(source);
        fold_fault_after_an_in_place_row_write_is_atomic(source);
        conditional_row_update_fault_case(source, 2, true);
        conditional_row_update_fault_case(source, 5, false);
    }
}
