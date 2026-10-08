//! Two calls of one owner with different arguments inside one invocation are
//! two invocations on every executor: the interpreter keeps no result per
//! owner, so a call under a conditional region (or a later call in the same
//! frame) never observes the result of an earlier call with other arguments.
use super::cross_backend_aliasing::{
    BITS, aggregate_type, bits, decode, run_everywhere, scalar_type,
};
use super::*;

type Built = Result<(), solve::SolveProgramConstructionError>;

fn real_constant<'p>(
    b: &mut solve::TypedProgramBuilder<'p>,
    value: f64,
) -> Result<solve::ProgramRegister<'p>, solve::SolveProgramConstructionError> {
    b.constant(solve::SolveValue::real(profile(), value), span(19001))
}

/// `child(a) = (update(2a, [2] = 8), -a)`; the owner `(a, v)` calls it on its
/// borrowed input, then again on the first result from inside a conditional
/// region selected by `flag`, and returns the second result of the inner call.
fn nested_call_table(flag: bool) -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let mut builder = solve::SolvePureCallTable::builder(profile());
    let child = builder
        .add_owner(
            identity(190),
            vec![aggregate_type()],
            vec![
                solve::SolvePureCallOutput::result(aggregate_type()),
                solve::SolvePureCallOutput::result(aggregate_type()),
            ],
            span(19010),
            |b, i, o| -> Built {
                let a = b.load(i[0], span(19011))?;
                let two = real_constant(b, 2.0)?;
                let doubled = b.scale(a, two, span(19012))?;
                let index = b.constant(
                    solve::SolveValue::integer(profile(), 2).unwrap(),
                    span(19013),
                )?;
                let eight = real_constant(b, 8.0)?;
                let updated = b.update_element(doubled, eight, &[index], span(19014))?;
                b.store(o[0], updated, span(19015))?;
                let minus = real_constant(b, -1.0)?;
                let negated = b.scale(a, minus, span(19016))?;
                b.store(o[1], negated, span(19017))
            },
        )
        .unwrap();
    let owner = builder
        .add_owner(
            identity(191),
            vec![aggregate_type(), scalar_type()],
            vec![solve::SolvePureCallOutput::result(aggregate_type())],
            span(19020),
            move |b, i, o| -> Built {
                let input = b.load(i[0], span(19021))?;
                let one = real_constant(b, 1.0)?;
                let argument = b.scale(input, one, span(19022))?;
                let outer = b.call(child, &[argument], span(19023))?;
                let selector = b.constant(solve::SolveValue::boolean(flag), span(19024))?;
                let chosen = b.conditional(
                    selector,
                    &[outer[0], argument],
                    vec![aggregate_type()],
                    span(19025),
                    move |r, i, o| -> Built {
                        let x = r.load(i[0], span(19026))?;
                        let inner = r.call(child, &[x], span(19027))?;
                        r.store(o[0], inner[1], span(19028))
                    },
                    |r, i, o| -> Built {
                        let y = r.load(i[1], span(19029))?;
                        let seven = real_constant(r, 7.0)?;
                        let scaled = r.scale(y, seven, span(19030))?;
                        r.store(o[0], scaled, span(19031))
                    },
                )?;
                b.store(o[0], chosen[0], span(19032))
            },
        )
        .unwrap();
    let site = builder.call_site(owner).unwrap();
    (builder.finish(), site)
}

#[test]
fn a_call_under_a_conditional_is_not_served_by_an_earlier_call_with_other_arguments() {
    let (z, inf, q) = (BITS[0], BITS[1], BITS[2]);
    // child(x) = ([2x with 2nd = 8], -x). Outer: [-0, 8, 2.5]. Inner on it: -[-0, 8, 2.5].
    let (table, site) = nested_call_table(true);
    let out = decode(&run_everywhere(&table, &site));
    assert_eq!(
        out,
        bits(&[0.0, -8.0, -2.5]),
        "inner call sees the outer result"
    );
    // The inactive arm scales the borrowed input by 7.
    let (table, site) = nested_call_table(false);
    let out = decode(&run_everywhere(&table, &site));
    assert_eq!(out, bits(&[z * 7.0, inf * 7.0, q * 7.0]));
}
