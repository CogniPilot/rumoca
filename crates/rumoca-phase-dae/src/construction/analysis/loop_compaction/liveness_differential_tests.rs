//! Measure where the store-deletion predicates and dataflow liveness disagree.
//!
//! Nothing here changes a compilation decision. Each test asks the same
//! question three ways over the enumerated corpus in [`super::liveness_corpus`]
//! and reports the differences:
//!
//! * by execution, with [`liveness_corpus::observed_incoming_reads`];
//! * by dataflow, with [`super::liveness`];
//! * by the syntactic predicates the pass actually uses.
//!
//! The execution answer pins the dataflow answer: any divergence there is a
//! defect in the liveness utility, so those comparisons assert. The syntactic
//! predicates are then compared against liveness, and the differences are
//! classified by direction. A predicate that reports a read where liveness
//! finds none only keeps a store that could have been deleted; a predicate that
//! reports no read where liveness finds one would delete a live store, so the
//! two directions are counted and reported separately.

use super::liveness::{self, LiveSet};
use super::liveness_corpus::{self, VALUE_NAMES, assign, branch, for_loop, render, while_loop};
use super::*;

const WIDE_READS: [&[&str]; 4] = [&[], &["a"], &["b"], &["a", "b"]];
const NARROW_READS: [&[&str]; 3] = [&[], &["a"], &["b"]];
const GUARD_READS: [&[&str]; 2] = [&[], &["a"]];

fn wide_leaves() -> Vec<rumoca_core::Statement> {
    VALUE_NAMES
        .iter()
        .flat_map(|target| WIDE_READS.iter().map(move |reads| assign(target, reads)))
        .collect()
}

fn narrow_leaves() -> Vec<rumoca_core::Statement> {
    VALUE_NAMES
        .iter()
        .flat_map(|target| NARROW_READS.iter().map(move |reads| assign(target, reads)))
        .collect()
}

fn filler_leaves() -> Vec<rumoca_core::Statement> {
    vec![assign("a", &["b"]), assign("c", &["a"])]
}

/// Every one- and two-statement straight-line body over the narrow leaves.
fn bodies() -> Vec<Vec<rumoca_core::Statement>> {
    let leaves = narrow_leaves();
    let mut bodies = leaves
        .iter()
        .map(|leaf| vec![leaf.clone()])
        .collect::<Vec<_>>();
    for first in &leaves {
        for second in &leaves {
            bodies.push(vec![first.clone(), second.clone()]);
        }
    }
    bodies
}

/// Every singly nested control-flow statement the grammar can build.
fn composites() -> Vec<rumoca_core::Statement> {
    let leaves = narrow_leaves();
    let mut composites = Vec::new();
    for body in bodies() {
        for guard in GUARD_READS {
            composites.push(for_loop("i", guard, body.clone()));
            composites.push(while_loop(guard, body.clone()));
        }
    }
    for taken in &leaves {
        let fallbacks = leaves
            .iter()
            .map(|leaf| Some(vec![leaf.clone()]))
            .chain(std::iter::once(None));
        for fallback in fallbacks {
            for guard in GUARD_READS {
                composites.push(branch(guard, vec![taken.clone()], fallback.clone()));
            }
        }
    }
    composites
}

/// Each composite wrapped one level deeper, with and without a sibling.
fn nested_composites(composites: &[rumoca_core::Statement]) -> Vec<rumoca_core::Statement> {
    let fillers = filler_leaves();
    let mut nested = Vec::new();
    for composite in composites {
        let mut inner_bodies = vec![vec![composite.clone()]];
        for filler in &fillers {
            inner_bodies.push(vec![filler.clone(), composite.clone()]);
            inner_bodies.push(vec![composite.clone(), filler.clone()]);
        }
        for body in inner_bodies {
            nested.push(for_loop("j", &[], body.clone()));
            nested.push(branch(&[], body, None));
        }
    }
    nested
}

/// Visit every program in the corpus exactly once.
fn for_each_program(visit: &mut dyn FnMut(&[rumoca_core::Statement])) {
    let composites = composites();
    let wide = wide_leaves();
    let fillers = filler_leaves();
    for composite in &composites {
        visit(std::slice::from_ref(composite));
        for leaf in &wide {
            visit(&[leaf.clone(), composite.clone()]);
            visit(&[composite.clone(), leaf.clone()]);
        }
        for before in &fillers {
            for after in &fillers {
                visit(&[before.clone(), composite.clone(), after.clone()]);
            }
        }
    }
    for nested in nested_composites(&composites) {
        visit(std::slice::from_ref(&nested));
        for filler in &fillers {
            visit(&[filler.clone(), nested.clone()]);
            visit(&[nested.clone(), filler.clone()]);
        }
    }
}

/// Disagreements between a syntactic predicate and liveness, by direction.
#[derive(Default)]
struct Divergence {
    /// The predicate reports a read that liveness does not: a kept store.
    conservative: usize,
    /// Liveness reports a read the predicate does not: a deletable live store.
    permissive: usize,
    kept_examples: Vec<String>,
    lost_examples: Vec<String>,
}

impl Divergence {
    fn record(&mut self, predicate: bool, live: bool, describe: impl FnOnce() -> String) {
        if predicate == live {
            return;
        }
        let (count, examples) = if predicate {
            (&mut self.conservative, &mut self.kept_examples)
        } else {
            (&mut self.permissive, &mut self.lost_examples)
        };
        *count += 1;
        if examples.len() < 4 {
            examples.push(describe());
        }
    }

    fn report(&self, label: &str) {
        println!(
            "{label}: {} conservative, {} permissive",
            self.conservative, self.permissive
        );
        for example in &self.kept_examples {
            println!("    kept a dead store: {example}");
        }
        for example in &self.lost_examples {
            println!("    LOST A LIVE READ: {example}");
        }
    }
}

fn value_names() -> Vec<VarName> {
    VALUE_NAMES.iter().map(|name| VarName::new(*name)).collect()
}

/// The dataflow answer must equal the executed answer on every corpus program.
#[test]
fn liveness_matches_execution_over_the_corpus() {
    let names = value_names();
    let mut programs = 0usize;
    for_each_program(&mut |program| {
        assert!(
            !liveness::contains_unstructured_jump(program),
            "the corpus must stay inside the modeled control flow"
        );
        let observed = liveness_corpus::observed_incoming_reads(program);
        let live = liveness::live_in(program, &LiveSet::new());
        for name in &names {
            assert_eq!(
                observed.contains(name),
                live.contains(name),
                "liveness disagrees with execution about `{}` in: {}",
                name.as_str(),
                render(program)
            );
        }
        programs += 1;
    });
    println!("checked {programs} programs against execution");
    assert!(
        programs > 40_000,
        "the corpus shrank to {programs} programs"
    );
}

/// Whether a program contains a `while`, which the predicates route through
/// their catch-all arm rather than through the incoming-value flow.
fn contains_while(statements: &[rumoca_core::Statement]) -> bool {
    statements.iter().any(|statement| match statement {
        rumoca_core::Statement::While { .. } => true,
        rumoca_core::Statement::For { equations, .. } => contains_while(equations),
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            cond_blocks.iter().any(|block| contains_while(&block.stmts))
                || else_block.as_deref().is_some_and(contains_while)
        }
        _ => false,
    })
}

/// `statements_read_incoming_name` against liveness on a single sequence.
#[test]
fn incoming_read_predicate_against_liveness() {
    let names = value_names();
    let mut divergence = Divergence::default();
    let mut without_while = Divergence::default();
    for_each_program(&mut |program| {
        let live = liveness::live_in(program, &LiveSet::new());
        let target = if contains_while(program) {
            &mut divergence
        } else {
            &mut without_while
        };
        for name in &names {
            let predicate = statements_read_incoming_name(program, name);
            target.record(predicate, live.contains(name), || {
                format!("`{}` in: {}", name.as_str(), render(program))
            });
        }
    });
    divergence.report("statements_read_incoming_name (programs containing `while`)");
    without_while.report("statements_read_incoming_name (`for`/`if` programs only)");
    assert_eq!(
        without_while.conservative + without_while.permissive,
        0,
        "the predicate and liveness must agree exactly outside `while`"
    );
    assert_eq!(
        divergence.permissive, 0,
        "a syntactic predicate reported no incoming read where one is live"
    );
}

/// `statements_read_name` is the coarsest of the predicates; it must still
/// never miss a live read, because a missed read deletes a live store.
#[test]
fn any_read_predicate_never_misses_a_live_read() {
    let names = value_names();
    let mut divergence = Divergence::default();
    for_each_program(&mut |program| {
        let live = liveness::live_in(program, &LiveSet::new());
        for name in &names {
            let predicate = statements_read_name(program, name);
            divergence.record(predicate, live.contains(name), || {
                format!("`{}` in: {}", name.as_str(), render(program))
            });
        }
    });
    divergence.report("statements_read_name");
    assert_eq!(
        divergence.permissive, 0,
        "a syntactic read scan missed a live read"
    );
}

/// Pairs of segments, composed once as a sequence and once as alternatives.
fn for_each_segment_pair(
    visit: &mut dyn FnMut(&[rumoca_core::Statement], &[rumoca_core::Statement]),
) {
    let composites = composites();
    let bodies = bodies();
    for composite in &composites {
        for body in &bodies {
            visit(std::slice::from_ref(composite), body);
        }
    }
}

/// Sequence composition: the follower segments of a straight-line definition
/// genuinely fall through into one another.
#[test]
fn segment_sequence_composition_against_liveness() {
    let names = value_names();
    let mut divergence = Divergence::default();
    for_each_segment_pair(&mut |first, second| {
        let joined = [first, second].concat();
        let observed = liveness_corpus::observed_incoming_reads(&joined);
        let live = liveness::live_in_concatenation(&[first, second], &LiveSet::new());
        for name in &names {
            assert_eq!(
                observed.contains(name),
                live.contains(name),
                "concatenated liveness disagrees with execution about `{}` in: {}",
                name.as_str(),
                render(&joined)
            );
            let predicate = statement_segments_read_incoming_name(&[first, second], name);
            divergence.record(predicate, live.contains(name), || {
                format!("`{}` in: {}", name.as_str(), render(&joined))
            });
        }
    });
    divergence.report("statement_segments_read_incoming_name (sequence)");
    assert_eq!(
        divergence.permissive, 0,
        "sequence composition reported no incoming read where one is live"
    );
}

/// Alternative composition: the several loop bodies a definition can be
/// re-entered by are competing successors, which is the composition
/// `definition_escapes_back_edge` performs by asking each segment separately.
///
/// Execution pins the join in both directions, and the join is asserted to
/// contain the sequence composition of the same segments, so treating
/// alternatives as a sequence can only lose a read and never invent one.
#[test]
fn segment_alternative_composition_against_liveness() {
    let names = value_names();
    let mut divergence = Divergence::default();
    let mut sequenced_predicate = Divergence::default();
    let mut sequence_loses_a_read = 0usize;
    for_each_segment_pair(&mut |first, second| {
        let live = liveness::live_in_alternatives(&[first, second], &LiveSet::new());
        let sequenced = liveness::live_in_concatenation(&[first, second], &LiveSet::new());
        assert!(
            sequenced.is_subset(&live),
            "sequencing reported a live name the join does not: {} | {}",
            render(first),
            render(second)
        );
        sequenced_predicate_divergence(&mut sequenced_predicate, first, second, &live, &names);
        for name in &names {
            let observed = liveness_corpus::observed_incoming_reads(first).contains(name)
                || liveness_corpus::observed_incoming_reads(second).contains(name);
            assert_eq!(
                observed,
                live.contains(name),
                "alternative liveness disagrees with execution about `{}`",
                name.as_str()
            );
            let predicate = [first, second]
                .iter()
                .any(|segment| statements_read_incoming_name(segment, name));
            divergence.record(predicate, live.contains(name), || {
                format!(
                    "`{}` in: {} | {}",
                    name.as_str(),
                    render(first),
                    render(second)
                )
            });
            if live.contains(name) && !sequenced.contains(name) {
                sequence_loses_a_read += 1;
            }
        }
    });
    divergence.report("definition_escapes_back_edge composition (alternatives)");
    sequenced_predicate
        .report("statement_segments_read_incoming_name over alternative re-entry segments");
    println!(
        "sequence composition loses a live read in {sequence_loses_a_read} segment-pair queries"
    );
    assert!(
        sequence_loses_a_read > 0,
        "the corpus must contain the shape that distinguishes the two compositions"
    );
    assert_eq!(
        divergence.permissive, 0,
        "alternative composition reported no incoming read where one is live"
    );
    assert!(
        sequenced_predicate.permissive > 0,
        "sequencing alternative re-entry segments must be observable as a lost read"
    );
}

/// `classify_iteration_local_targets` hands its list of enclosing loop bodies,
/// which are alternative re-entry paths, to the sequence operator. Measure how
/// often that composition reports no read where one is live.
fn sequenced_predicate_divergence(
    divergence: &mut Divergence,
    first: &[rumoca_core::Statement],
    second: &[rumoca_core::Statement],
    live: &LiveSet,
    names: &[VarName],
) {
    for name in names {
        let predicate = statement_segments_read_incoming_name(&[first, second], name);
        divergence.record(predicate, live.contains(name), || {
            format!(
                "`{}` in: {} | {}",
                name.as_str(),
                render(first),
                render(second)
            )
        });
    }
}

/// The enclosing loop bodies a definition can be re-entered by are alternative
/// paths, and `classify_iteration_local_targets` passes that list to the
/// sequence operator. With two enclosing loops the middle body's own write then
/// hides the outer body's read.
///
/// ```text
/// for k loop            // outer body: w := acc; then the middle loop
///   w := acc;           // reads what the previous outer iteration left
///   for m loop          // middle body: acc := 1.0; then the inner loop
///     acc := 1.0;
///     for n loop
///       acc := 2.0;     // the store under classification
///     end for;
///   end for;
/// end for;
/// ```
///
/// At the inner loop the re-entry segments are `[middle body, outer body]`. The
/// outer loop re-enters `w := acc` without re-running the middle body's prefix,
/// so `acc := 1.0` is not on that path and cannot settle the value the read
/// observes.
#[test]
fn sequencing_two_enclosing_back_edges_hides_the_outer_read() {
    let inner = for_loop("n", &[], vec![assign("acc", &[])]);
    let middle_body = vec![assign("acc", &[]), inner];
    let outer_body = vec![
        assign("w", &["acc"]),
        for_loop("m", &[], middle_body.clone()),
    ];
    let re_entry: [&[rumoca_core::Statement]; 2] = [&middle_body, &outer_body];
    let name = VarName::new("acc");

    assert!(
        !statement_segments_read_incoming_name(&re_entry, &name),
        "the sequence operator lets the middle body's write hide the outer read"
    );
    assert!(
        liveness::live_in_alternatives(&re_entry, &LiveSet::new()).contains(&name),
        "the outer body reads `acc` before writing it, on its own re-entry path"
    );
    assert!(
        re_entry
            .iter()
            .any(|segment| statements_read_incoming_name(segment, &name)),
        "asking each re-entry segment separately keeps the read visible"
    );
}

/// A `for` binder shadows an enclosing value of the same name inside the body,
/// so a body read of the binder is not a read of the enclosing value.
#[test]
fn binder_shadowing_separates_the_predicate_from_liveness() {
    let body = vec![assign("a", &["i"])];
    let program = vec![for_loop("i", &[], body)];
    let binder = VarName::new("i");
    assert!(
        statements_read_incoming_name(&program, &binder),
        "the syntactic predicate counts the binder read as a read of `i`"
    );
    assert!(
        !liveness::reads_incoming_value(&program, &binder),
        "the binder shadows the enclosing `i` inside the body"
    );
    assert!(
        !liveness_corpus::observed_incoming_reads(&program).contains(&binder),
        "no execution of the loop observes the enclosing `i`"
    );
}

/// A `while` body is scanned for any read at all, so a definite overwrite
/// inside it does not hide a later read of the overwritten value.
#[test]
fn while_bodies_are_scanned_without_kills() {
    let body = vec![assign("a", &[]), assign("c", &["a"])];
    let program = vec![while_loop(&[], body)];
    let name = VarName::new("a");
    assert!(
        statements_read_incoming_name(&program, &name),
        "the syntactic predicate scans a while body for any read"
    );
    assert!(
        !liveness::reads_incoming_value(&program, &name),
        "the body overwrites `a` before reading it on every path"
    );
    assert!(
        !liveness_corpus::observed_incoming_reads(&program).contains(&name),
        "no execution of the loop observes the incoming `a`"
    );
}

/// The shape fixed in `Keep a loop carry its last store`: splicing a loop body
/// in as a follower segment composes the back edge and the loop-exit edge in
/// sequence, and the body's own write then hides the read after the loop.
#[test]
fn a_body_write_must_not_hide_a_read_after_the_loop() {
    let body = vec![assign("a", &["b"])];
    let loop_statement = for_loop("i", &[], body.clone());
    let after = vec![assign("c", &["a"])];
    let name = VarName::new("a");

    let spliced = liveness::live_in_concatenation(&[&body, &after], &LiveSet::new());
    assert!(
        !spliced.contains(&name),
        "sequencing the raw body ahead of the suffix hides the read"
    );

    let entered_as_loop = liveness::live_in_concatenation(
        &[std::slice::from_ref(&loop_statement), &after],
        &LiveSet::new(),
    );
    assert!(
        entered_as_loop.contains(&name),
        "a compact domain may run zero times, so the read after the loop survives"
    );
    assert!(
        liveness_corpus::observed_incoming_reads(&[loop_statement, after[0].clone()])
            .contains(&name),
        "the zero-iteration execution reads the incoming `a`"
    );
}

fn span() -> Span {
    Span::from_offsets(rumoca_core::SourceId::DUMMY, 0, 1)
}

fn part(ident: &str) -> rumoca_core::ComponentRefPart {
    rumoca_core::ComponentRefPart {
        ident: ident.to_string(),
        span: span(),
        subs: Vec::new(),
        def_id: rumoca_core::DefId::new(1),
    }
}

fn nested_component(first: &str, second: &str) -> rumoca_core::ComponentReference {
    rumoca_core::ComponentReference::construct(false, span(), vec![part(first), part(second)])
        .expect("test component reference has exact identity")
}

fn nested_read(first: &str, second: &str) -> Expression {
    Expression::VarRef {
        name: Reference::new(format!("{first}.{second}")),
        subscripts: Vec::new(),
        span: span(),
    }
}

/// The substitution site recognizes a definite whole-value write by
/// `scalar_assignment_target`: one part, no subscripts. A write through a
/// longer path is not that rule, so it must not hide a later read here either.
/// Accepting it would let the oracle report a store dead that the pass itself
/// would never have called dead.
#[test]
fn a_multi_part_write_is_not_a_kill() {
    let program = vec![
        rumoca_core::Statement::Assignment {
            comp: nested_component("r", "f"),
            value: Expression::Literal {
                value: rumoca_core::Literal::Integer(1),
                span: span(),
            },
            span: span(),
        },
        rumoca_core::Statement::Assignment {
            comp: nested_component("c", "g"),
            value: nested_read("r", "f"),
            span: span(),
        },
    ];
    let live = liveness::live_in(&program, &LiveSet::new());
    assert!(
        live.contains(&VarName::new("r.f")),
        "a two-part write is not the site's kill rule, so it cannot hide the read"
    );
    assert!(
        live.contains(&VarName::new("r")),
        "reading `r.f` observes part of `r`"
    );
}

fn call_writing(target: &str) -> rumoca_core::Statement {
    rumoca_core::Statement::FunctionCall {
        comp: Reference::new("f"),
        args: Vec::new(),
        outputs: vec![Some(
            rumoca_core::ComponentReference::construct(false, span(), vec![part(target)])
                .expect("test component reference has exact identity"),
        )],
        span: span(),
    }
}

/// A call's receiving element writes its target (MLS §12.4.4), and neither
/// answer treats that write as a kill: the site's kill rule recognizes only an
/// assignment, so the incoming value stays live wherever a later statement
/// reads it. Both answers also agree that the element itself is not a read, so
/// the two stay aligned on this form in both directions.
#[test]
fn a_call_receiving_element_is_a_write_neither_answer_kills_on() {
    let name = VarName::new("t");
    let read_after = vec![call_writing("t"), assign("c", &["t"])];
    assert!(
        statements_read_incoming_name(&read_after, &name),
        "the syntactic scan sees the read behind the call"
    );
    assert!(
        liveness::live_in(&read_after, &LiveSet::new()).contains(&name),
        "the call is not a recognized kill, so the read behind it keeps `t` live"
    );
    let no_read_after = vec![call_writing("t"), assign("c", &[])];
    assert!(
        !statements_read_incoming_name(&no_read_after, &name),
        "a receiving element is a write, not a read"
    );
    assert!(
        !liveness::live_in(&no_read_after, &LiveSet::new()).contains(&name),
        "nothing in the segment reads `t`"
    );
}

/// `f[a]()`: the callable of a call statement is selected by a component
/// reference, and the subscripts on that reference are evaluated where the call
/// runs (MLS §12.4.4). A walk that inspects only the arguments and the receiving
/// elements reports the subscript's value dead while the call still observes it,
/// which is the direction a deletion proof may not be wrong in.
#[test]
fn a_subscripted_callable_reference_is_a_read() {
    let name = VarName::new("a");
    let call = vec![rumoca_core::Statement::FunctionCall {
        comp: Reference::from_component_reference(
            rumoca_core::ComponentReference::construct(
                false,
                span(),
                vec![rumoca_core::ComponentRefPart {
                    ident: "f".to_string(),
                    span: span(),
                    subs: vec![Subscript::Expr {
                        expr: Box::new(Expression::VarRef {
                            name: Reference::new("a"),
                            subscripts: Vec::new(),
                            span: span(),
                        }),
                        span: span(),
                    }],
                    def_id: rumoca_core::DefId::new(1),
                }],
            )
            .expect("test component reference has exact identity"),
        ),
        args: Vec::new(),
        outputs: Vec::new(),
        span: span(),
    }];
    assert!(
        statements_read_incoming_name(&call, &name),
        "the generic statement walk reaches the callable's subscript"
    );
    assert!(
        liveness::live_in(&call, &LiveSet::new()).contains(&name),
        "the callable's subscript keeps `a` live across the call"
    );
}

/// Evidence about one name, given the segments that run after an empty block.
fn evidence_for(
    name: &VarName,
    after: &[rumoca_core::Statement],
) -> Option<liveness::StoreUnobserved> {
    let mut proven =
        liveness::prove_unobserved_stores(&HashSet::from([name.clone()]), &[], &[after]);
    proven.remove(name)
}

/// `break` and `return` leave the region along an edge this dataflow does not
/// model, so no evidence is issued about a region containing either.
#[test]
fn the_witness_refuses_an_unstructured_jump() {
    let name = VarName::new("a");
    let quiet = vec![assign("c", &[])];
    assert!(
        evidence_for(&name, &quiet).is_some(),
        "nothing observes `a` in a segment that does not mention it"
    );
    for jump in [
        rumoca_core::Statement::Break { span: span() },
        rumoca_core::Statement::Return { span: span() },
    ] {
        let with_jump = vec![assign("c", &[]), jump];
        assert!(
            evidence_for(&name, &with_jump).is_none(),
            "a region that jumps out is outside the modeled control flow"
        );
    }
}

/// The evidence the substitution site requires, measured against both other
/// answers over the whole corpus.
///
/// Two properties matter and are asserted here. No evidence is ever issued for
/// a value some execution observes, which is the theorem's own second clause
/// checked by running the program. And the evidence is never withheld from a
/// name the retired predicate would have allowed the site to delete, so
/// replacing that predicate loses no deletion this grammar can express. The
/// reverse direction is a gain and is counted, not bounded: those are the
/// programs where the predicate scanned a loop body for any read at all.
#[test]
fn the_witness_is_sound_and_loses_nothing_over_the_corpus() {
    let names = value_names();
    let mut witnessed = 0usize;
    let mut newly_deletable = 0usize;
    for_each_program(&mut |program| {
        let observed = liveness_corpus::observed_incoming_reads(program);
        for name in &names {
            let query = witness_query(program, name, &observed);
            witnessed += usize::from(query.witnessed);
            newly_deletable += usize::from(query.beyond_the_predicate);
        }
    });
    println!(
        "{witnessed} witnessed store deletions, {newly_deletable} of them beyond the retired predicate"
    );
    assert!(witnessed > 0, "the corpus proved nothing deletable");
    assert!(
        newly_deletable > 0,
        "the corpus must contain a store only the dataflow answer proves dead"
    );
}

/// One corpus query, with both of the test's assertions discharged.
struct WitnessQuery {
    witnessed: bool,
    beyond_the_predicate: bool,
}

fn witness_query(
    program: &[rumoca_core::Statement],
    name: &VarName,
    observed: &LiveSet,
) -> WitnessQuery {
    let predicate_allowed = !statements_read_incoming_name(program, name);
    let Some(evidence) = evidence_for(name, program) else {
        assert!(
            !predicate_allowed,
            "no evidence for `{}`, which the retired predicate allowed in: {}",
            name.as_str(),
            render(program)
        );
        return WitnessQuery {
            witnessed: false,
            beyond_the_predicate: false,
        };
    };
    assert_eq!(
        evidence.name(),
        name,
        "the evidence must name the value it is about"
    );
    assert!(
        !observed.contains(name),
        "evidence was issued for `{}`, which is observed by: {}",
        name.as_str(),
        render(program)
    );
    WitnessQuery {
        witnessed: true,
        beyond_the_predicate: !predicate_allowed,
    }
}
