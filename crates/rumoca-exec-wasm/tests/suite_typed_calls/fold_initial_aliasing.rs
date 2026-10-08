//! A fold rewrites its initial value in place only when nothing else reads
//! that storage: not a capture, not another carried value, not the predicate
//! of a `while` fold (SOLVE-C78). Every executor agrees with the interpreter.
use super::cross_backend_aliasing::{BITS, aggregate_type, bits, decode, scalar_type};
use super::*;
use rumoca_eval_solve::PureCallInvocation;

type B<'p> = solve::TypedProgramBuilder<'p>;
type S<'p> = solve::ProgramSlot<'p>;
type R<'p> = solve::ProgramRegister<'p>;
type Built = Result<(), solve::SolveProgramConstructionError>;

fn build(
    nout: usize,
    body: impl for<'p> FnOnce(&mut B<'p>, &[S<'p>], &[S<'p>]) -> Built,
) -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let mut builder = solve::SolvePureCallTable::builder(profile());
    let owner = builder
        .add_owner(
            identity(406),
            vec![aggregate_type(), scalar_type()],
            (0..nout)
                .map(|_| solve::SolvePureCallOutput::result(aggregate_type()))
                .collect(),
            span(40600),
            body,
        )
        .unwrap();
    let site = builder.call_site(owner).unwrap();
    (builder.finish(), site)
}
fn run(name: &str, t: &(solve::SolvePureCallTable, solve::SolvePureCallSite), expect: &[f64]) {
    let (table, site) = t;
    let input = vec![
        BITS.iter().copied().map(real).collect::<Vec<_>>(),
        vec![real(9.5)],
    ];
    let interp = oracle(table, site, &input).unwrap();
    let compiled = compile_pure_call_wasm(table, site).unwrap();
    let (st, wasm) = Runner::new(&compiled).run(&cells(input.iter().flatten().copied()));
    assert_eq!(st, 0);
    let native = rumoca_exec_cranelift::compile_pure_call_table(table).unwrap();
    let flat: Vec<f64> = BITS.iter().copied().chain([9.5]).collect();
    let mut out = vec![0.0; interp.len() / 8];
    native
        .call_scalar_payload(
            PureCallInvocation::Primal(site),
            &flat,
            &mut out,
            &mut Vec::new(),
            &mut Vec::new(),
        )
        .unwrap();
    let nat: Vec<u8> = out.iter().flat_map(|v| v.to_bits().to_le_bytes()).collect();
    let e = bits(expect);
    let (di, dw, dn) = (decode(&interp), decode(&wasm), decode(&nat));
    assert_eq!(di, e, "{name}: interpreter");
    assert_eq!(dw, di, "{name}: wasm");
    assert_eq!(dn, di, "{name}: native");
}
fn c<'p>(b: &mut B<'p>, v: f64) -> Result<R<'p>, solve::SolveProgramConstructionError> {
    b.constant(solve::SolveValue::real(profile(), v), span(40601))
}
fn ci<'p>(b: &mut B<'p>, v: i64) -> Result<R<'p>, solve::SolveProgramConstructionError> {
    b.constant(
        solve::SolveValue::integer(profile(), v).unwrap(),
        span(40602),
    )
}
fn temp<'p>(b: &mut B<'p>, s: S<'p>) -> Result<R<'p>, solve::SolveProgramConstructionError> {
    let l = b.load(s, span(40603))?;
    let one = c(b, 1.0)?;
    b.scale(l, one, span(40604))
}
fn pass1<'r>(r: &mut B<'r>, i: &[S<'r>], o: &[S<'r>]) -> Built {
    let a = r.load(i[0], span(40605))?;
    r.store(o[0], a, span(40606))
}
fn pass2<'r>(r: &mut B<'r>, i: &[S<'r>], o: &[S<'r>]) -> Built {
    let a = r.load(i[0], span(40605))?;
    r.store(o[0], a, span(40606))?;
    r.store(o[1], a, span(40607))
}
// fold body: set element idx to v, pass second carried
fn set_body<'r>(
    r: &mut B<'r>,
    car: &[S<'r>],
    _c: &[S<'r>],
    _b: &[S<'r>],
    o: &[S<'r>],
    v: f64,
    two: bool,
) -> Built {
    let a = r.load(car[0], span(40610))?;
    let i = ci(r, 1)?;
    let x = c(r, v)?;
    let u = r.update_element(a, x, &[i], span(40611))?;
    r.store(o[0], u, span(40612))?;
    if two {
        let bb = r.load(car[1], span(40613))?;
        r.store(o[1], bb, span(40614))?;
    }
    Ok(())
}

#[test]
fn carried_values_sharing_a_borrowed_range_are_not_rewritten_in_place() {
    let (z, inf, q) = (BITS[0], BITS[1], BITS[2]);
    let t = build(2, |b, i, o| {
        let x = temp(b, i[0])?;
        let t = b.constant(solve::SolveValue::boolean(true), span(40620))?;
        let d = b.conditional(
            t,
            &[x],
            vec![aggregate_type(); 2],
            span(40621),
            pass2,
            pass2,
        )?;
        let f = b.fold(
            domain(1, 1, 1),
            &[d[0], d[1]],
            &[],
            span(40622),
            |r, c1, c2, c3, o| set_body(r, c1, c2, c3, o, 7.0, true),
        )?;
        b.store(o[0], f[0], span(40623))?;
        b.store(o[1], f[1], span(40624))
    });
    run("P1 (x,x)->fold", &t, &[7.0, inf, q, z, inf, q]);
}

#[test]
fn a_capture_sharing_the_initial_range_sees_the_old_value() {
    let (z, inf, q) = (BITS[0], BITS[1], BITS[2]);
    let t = build(2, |b, i, o| {
        let x = temp(b, i[0])?;
        let t = b.constant(solve::SolveValue::boolean(true), span(40620))?;
        let d = b.conditional(
            t,
            &[x, x],
            vec![aggregate_type()],
            span(40621),
            pass1,
            pass1,
        )?;
        b.store(o[1], d[0], span(40624))?;
        let f = b.fold(
            domain(1, 2, 1),
            &[x],
            &[d[0]],
            span(40622),
            |r, car, cap, _b, o| {
                let a = r.load(car[0], span(1))?;
                let k = r.load(cap[0], span(2))?;
                let i1 = ci(r, 1)?;
                let nine = c(r, 9.0)?;
                let s1 = r.update_element(a, nine, &[i1], span(3))?;
                let v = r.project_element(k, vec![0], span(4))?;
                let i3 = ci(r, 3)?;
                let s2 = r.update_element(s1, v, &[i3], span(5))?;
                r.store(o[0], s2, span(6))
            },
        )?;
        b.store(o[0], f[0], span(40623))
    });
    // iter: s1=[9,inf,q]; v=k[0]=-0 ; s2=[9,inf,-0]; iter2 same -> [9,inf,-0]; capture d = [-0,inf,q]
    run("P6 capture alias", &t, &[9.0, inf, z, z, inf, q]);
}

#[test]
fn an_initial_read_after_the_fold_is_not_rewritten() {
    let (z, inf, q) = (BITS[0], BITS[1], BITS[2]);
    let t = build(2, |b, i, o| {
        let x = temp(b, i[0])?;
        let f = b.fold(
            domain(1, 1, 1),
            &[x],
            &[],
            span(40622),
            |r, c1, c2, c3, o| set_body(r, c1, c2, c3, o, 7.0, false),
        )?;
        b.store(o[0], f[0], span(40623))?;
        b.store(o[1], x, span(40624))
    });
    run("P3 init read after", &t, &[7.0, inf, q, z, inf, q]);
}

#[test]
fn two_folds_over_one_initial_each_start_from_it() {
    let (inf, q) = (BITS[1], BITS[2]);
    let t = build(2, |b, i, o| {
        let x = temp(b, i[0])?;
        let f = b.fold(
            domain(1, 1, 1),
            &[x],
            &[],
            span(40622),
            |r, c1, c2, c3, o| set_body(r, c1, c2, c3, o, 7.0, false),
        )?;
        let g = b.fold(
            domain(1, 1, 1),
            &[x],
            &[],
            span(40625),
            |r, c1, c2, c3, o| set_body(r, c1, c2, c3, o, 8.0, false),
        )?;
        b.store(o[0], f[0], span(40623))?;
        b.store(o[1], g[0], span(40624))
    });
    run("P4 two folds", &t, &[7.0, inf, q, 8.0, inf, q]);
}

#[test]
fn a_raw_input_initial_is_never_rewritten() {
    let (z, inf, q) = (BITS[0], BITS[1], BITS[2]);
    let t = build(2, |b, i, o| {
        let x = b.load(i[0], span(40630))?;
        let f = b.fold(
            domain(1, 1, 1),
            &[x],
            &[],
            span(40622),
            |r, c1, c2, c3, o| set_body(r, c1, c2, c3, o, 7.0, false),
        )?;
        let y = b.load(i[0], span(40631))?;
        b.store(o[0], f[0], span(40623))?;
        b.store(o[1], y, span(40624))
    });
    run("P5 raw input", &t, &[7.0, inf, q, z, inf, q]);
}

#[test]
fn a_while_predicate_sees_the_old_value_of_a_shared_capture() {
    let (z, inf, q) = (BITS[0], BITS[1], BITS[2]);
    let t = build(2, |b, i, o| {
        let x = temp(b, i[0])?;
        let t = b.constant(solve::SolveValue::boolean(true), span(40620))?;
        let d = b.conditional(
            t,
            &[x, x],
            vec![aggregate_type()],
            span(40621),
            pass1,
            pass1,
        )?;
        b.store(o[1], d[0], span(40624))?;
        let f = b.fold_while(
            domain(1, 3, 1),
            &[x],
            &[d[0]],
            span(40622),
            |r, car, cap, o| {
                let _a = r.load(car[0], span(1))?;
                let k = r.load(cap[0], span(2))?;
                let e = r.project_element(k, vec![0], span(3))?;
                let nine = c(r, 9.0)?;
                let p = r.compare(solve::SolveCompareOperator::NotEqual, e, nine, span(4))?;
                r.store(o[0], p, span(5))
            },
            |r, car, cap, _b, o| {
                let a = r.load(car[0], span(1))?;
                let _k = r.load(cap[0], span(2))?;
                let cnt = r.project_element(a, vec![2], span(3))?;
                let one = c(r, 1.0)?;
                let n = r.binary(solve::SolveBinaryOperator::Add, cnt, one, span(4))?;
                let i1 = ci(r, 1)?;
                let nine = c(r, 9.0)?;
                let i3 = ci(r, 3)?;
                let s1 = r.update_element(a, nine, &[i1], span(5))?;
                let s2 = r.update_element(s1, n, &[i3], span(6))?;
                r.store(o[0], s2, span(7))
            },
        )?;
        b.store(o[0], f[0], span(40623))
    });
    run("P7 while alias", &t, &[9.0, inf, q + 3.0, z, inf, q]);
}
