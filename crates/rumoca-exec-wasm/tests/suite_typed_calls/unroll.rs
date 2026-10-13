//! Loops with a small constant trip count are emitted as straight-line passes
//! that agree with the canonical evaluator; larger ones keep their loop.
use super::pass_loops::{doubled_binder, loop_memory_accesses};
use super::*;

fn loop_count(module: &[u8]) -> usize {
    let mut loops = 0;
    for payload in wasmparser::Parser::new(0).parse_all(module) {
        let wasmparser::Payload::CodeSectionEntry(body) = payload.unwrap() else {
            continue;
        };
        for operator in body.get_operators_reader().unwrap() {
            loops += usize::from(matches!(
                operator.unwrap(),
                wasmparser::Operator::Loop { .. }
            ));
        }
    }
    loops
}

/// The carried integer of `integer_fold` after every pass over `domain`, from
/// the inputs 0, 9 and -7, agrees with the canonical evaluator.
fn fold_agrees(domain: StructuredIndexDomain) -> usize {
    let (table, site) = integer_fold(domain);
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let mut runner = Runner::new(&compiled);
    for input in [0, 9, -7] {
        let inputs = vec![vec![solve::SolveValueKind::Integer(input)]];
        let (status, actual) = runner.run(&cells(inputs.iter().flatten().copied()));
        assert_eq!(status, 0);
        assert_eq!(actual, oracle(&table, &site, &inputs).unwrap());
    }
    loop_count(compiled.module_bytes())
}

#[test]
fn a_fold_over_few_passes_is_straight_line_and_matches_the_oracle() {
    for d in [
        domain(1, 1, 1),
        domain(1, 8, 1),
        domain(9, -4, -3),
        domain(1, 32, 1),
    ] {
        assert_eq!(fold_agrees(d), 0);
    }
}

#[test]
fn a_fold_over_more_passes_than_the_unrolling_bound_keeps_its_loop() {
    assert_eq!(fold_agrees(domain(1, 33, 1)), 1);
    assert_eq!(fold_agrees(domain(1, 14400, 1)), 1);
}

#[test]
fn a_map_over_few_passes_is_straight_line_and_matches_the_oracle() {
    for (first, last, step, extent) in [(1, 8, 1, 8), (10, -2, -3, 5), (-3, 3, 2, 4), (5, 5, 1, 1)]
    {
        let (table, site) = doubled_binder(first, last, step, extent);
        let compiled = compile_pure_call_wasm(&table, &site).unwrap();
        let (status, actual) = Runner::new(&compiled).run(&[]);
        assert_eq!(status, 0);
        assert_eq!(actual, oracle(&table, &site, &[]).unwrap());
        assert_eq!(loop_count(compiled.module_bytes()), 0);
    }
}

/// `s := s + i` over `i in 1:passes` while the captured `enabled` holds, and,
/// when `conjunction` is set, while the captured `allowed` also holds.
fn guarded_fold(
    passes: i64,
    conjunction: bool,
) -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let p = profile();
    let integer = solve::SolveValueType::scalar(solve::SolveScalarType::integer(p));
    let boolean = solve::SolveValueType::scalar(solve::SolveScalarType::Boolean);
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(22),
            vec![integer.clone(), boolean.clone(), boolean],
            vec![solve::SolvePureCallOutput::result(integer)],
            span(220),
            |b, inputs, outputs| {
                let initial = b.load(inputs[0], span(221))?;
                let enabled = b.load(inputs[1], span(222))?;
                let allowed = b.load(inputs[2], span(222))?;
                let result = b.fold_while(
                    domain(1, passes, 1),
                    &[initial],
                    &[enabled, allowed],
                    span(223),
                    |r, _, captures, _, outputs| {
                        let enabled = r.load(captures[0], span(224))?;
                        let test = if conjunction {
                            let allowed = r.load(captures[1], span(224))?;
                            r.binary(solve::SolveBinaryOperator::And, enabled, allowed, span(224))?
                        } else {
                            enabled
                        };
                        r.store(outputs[0], test, span(225))
                    },
                    |r, carried, _, binders, outputs| {
                        let old = r.load(carried[0], span(226))?;
                        let index = r.load(binders[0], span(227))?;
                        let next =
                            r.binary(solve::SolveBinaryOperator::Add, old, index, span(228))?;
                        r.store(outputs[0], next, span(229))
                    },
                )?;
                b.store(outputs[0], result[0], span(230))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    (table.finish(), site)
}

/// The loop runs once when the test holds and not at all otherwise, for every
/// combination of its captured Booleans, with no counted loop in the code of
/// a short fold.
fn guarded_fold_agrees(conjunction: bool) {
    for (passes, loops) in [(8, 0), (14400, 1)] {
        let (table, site) = guarded_fold(passes, conjunction);
        let compiled = compile_pure_call_wasm(&table, &site).unwrap();
        assert_eq!(loop_count(compiled.module_bytes()), loops);
        let mut runner = Runner::new(&compiled);
        for (start, enabled, allowed) in [
            (0, true, true),
            (5, false, true),
            (-3, true, false),
            (2, false, false),
        ] {
            let inputs = vec![
                vec![solve::SolveValueKind::Integer(start)],
                vec![solve::SolveValueKind::Boolean(enabled)],
                vec![solve::SolveValueKind::Boolean(allowed)],
            ];
            let (status, actual) = runner.run(&cells(inputs.iter().flatten().copied()));
            assert_eq!(status, 0);
            let expected = oracle(&table, &site, &inputs).unwrap();
            assert_eq!(actual, expected);
            let runs = enabled && (allowed || !conjunction);
            let total = if runs {
                start + passes * (passes + 1) / 2
            } else {
                start
            };
            assert_eq!(expected, cells([solve::SolveValueKind::Integer(total)]));
        }
    }
}

#[test]
fn a_test_that_only_reads_a_captured_boolean_guards_the_loop_once() {
    guarded_fold_agrees(false);
}

#[test]
fn a_test_computed_from_several_captures_guards_the_loop_once() {
    guarded_fold_agrees(true);
}

#[test]
fn a_cartesian_domain_advances_its_binders_in_locals() {
    let mut d = domain(1, 40, 1);
    let mut second = d.binders[0].clone();
    second.id = 10;
    second.upper = 3;
    d.binders.push(second);
    let (table, site) = integer_fold(d);
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let inputs = vec![vec![solve::SolveValueKind::Integer(0)]];
    let (status, output) = Runner::new(&compiled).run(&cells(inputs.iter().flatten().copied()));
    assert_eq!(status, 0);
    assert_eq!(output, oracle(&table, &site, &inputs).unwrap());
    // The carried value is the only memory a pass touches.
    assert_eq!(loop_memory_accesses(compiled.module_bytes()), (1, 1));
}
