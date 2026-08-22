//! The preservation theorem for loop compaction, stated executably: for every
//! program `P` in the evaluable subset and every entry environment `E`,
//! `interpret(P, E)` and `interpret(compact_function_loops(P), E)` agree on the
//! final value of every function output, which is the statement intended for
//! mechanisation in Lean once the reference semantics exists.
//!
//! Outputs are the observable half of the final environment: a function's
//! locals are unreachable after it returns, and deleting their dead stores is
//! precisely the freedom this pass uses. The corpus therefore routes local
//! values into outputs through an epilogue, so a store the pass wrongly deletes
//! becomes an output disagreement rather than an invisible one.
//!
//! The theorem separates two versions of the pass only where they disagree on
//! some corpus program's outputs, so it cannot separate two that rewrite every
//! corpus program identically. The domain test in `inline_dominated_loop_locals`
//! is one such place. A well-formed function body assigns no bare name outside
//! its locals and its outputs, and those two declaration lists are disjoint, so
//! over every body the pass can be handed `!locals.contains(&target)` and
//! `outputs.contains(&target)` decide alike: their disjunction is load-bearing
//! and either term alone carries it. Losing both lets the pass delete stores to
//! outputs, which the corpus reports at once; losing one leaves the rewrite of
//! every corpus program byte for byte the same.
//!
//! Three guards keep the statement from being satisfied vacuously. Interpreter
//! refusals are counted by reason and capped, so a corpus that stops executing
//! goes red instead of quiet. Programs the pass actually rewrote are counted
//! separately, so agreement on untouched programs proves nothing. And
//! [`the_differential_reports_a_deleted_live_store`] feeds the comparison a
//! deliberately broken rewrite and requires it to be reported.
//!
//! [`OPEN_PASS_DEFECT_SEEDS`] names the corpus programs whose divergence is a
//! known defect of the pass rather than of the corpus. Each listed seed must
//! still diverge, so the list cannot outlive the defect and cannot absorb a new
//! one. Three open defects are stated directly, minimised, in
//! [`a_loop_local_prefix_substitution_moves_a_read_past_a_later_write`],
//! [`a_loop_local_prefix_substitution_ignores_a_rewrite_of_its_target`] and
//! [`outer_loop_distribution_reorders_writes_to_a_constant_coordinate`].

use super::preservation_corpus::*;
use super::preservation_interpreter::interpret;
use super::preservation_programs::{generated_program, render};
use super::preservation_values::{Environment, Refusal, Value, render_environment};
use super::*;
use rumoca_core::OpBinary;
use std::collections::BTreeMap;

/// Seeds drawn from the generator.
const GENERATED_PROGRAMS: u64 = 40_000;

/// Fewest program/entry pairs the corpus must actually execute on both sides.
const COMPARISON_FLOOR: usize = 120_000;

/// Fewest of those pairs whose program compaction genuinely rewrote.
const REWRITTEN_COMPARISON_FLOOR: usize = 40_000;

/// Fewest distinct programs compaction must have rewritten.
const REWRITTEN_PROGRAM_FLOOR: usize = 10_000;

/// Largest share of attempted pairs one refusal reason may account for.
const REFUSAL_SHARE_LIMIT: f64 = 0.05;

/// Largest share of attempted pairs the pass itself may refuse.
const PASS_REFUSAL_SHARE_LIMIT: f64 = 0.02;

/// Divergence examples kept for the failure report.
const REPORTED_EXAMPLES: usize = 6;

/// Corpus seeds whose divergence is an open defect of the pass.
///
/// Two defects account for the whole list, both of them in
/// `loop_local_substitutions`. Seeds 2980, 2988, 16285 and 23943 substitute a
/// value whose dependencies the loop body's trailing statement rewrites; seeds
/// 11994 and 12049 substitute a value whose own target that statement
/// rewrites. Each is minimised into its own test below. Every listed seed must
/// still diverge, so the list cannot outlive either defect, and any program not
/// listed must be preserved exactly.
///
/// The third open defect, outer-loop distribution over a constant coordinate,
/// carries no seed: no program the generator currently draws reaches it, so
/// [`outer_loop_distribution_reorders_writes_to_a_constant_coordinate`] is the
/// only thing holding it.
const OPEN_PASS_DEFECT_SEEDS: [u64; 6] = [2980, 2988, 11994, 12049, 16285, 23943];

/// What the theorem requires of one program.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Expectation {
    /// Compaction must preserve every output.
    Preserved,
    /// Compaction is known to change an output, and must still do so.
    OpenDefect,
}

/// The tally the preservation theorem is judged on.
#[derive(Default)]
struct Ledger {
    programs: usize,
    attempts: usize,
    compared: usize,
    rewritten_programs: usize,
    rewritten_compared: usize,
    pass_refusals: usize,
    refusals: BTreeMap<Refusal, usize>,
    /// The original ran and the rewrite did not: the rewrite left the subset.
    refused_after_only: usize,
    /// The rewrite ran and the original did not, which dead-store deletion can
    /// legitimately cause by removing the computation that left the subset.
    /// Legitimate but not unbounded: a corpus that stopped executing its source
    /// programs would show up here, so this shares the refusal ceiling.
    refused_before_only: usize,
    output_divergences: usize,
    local_divergences: usize,
    quarantined_programs: usize,
    quarantined_divergences: usize,
    examples: Vec<String>,
}

impl Ledger {
    fn count_refusal(&mut self, refusal: Refusal) {
        *self.refusals.entry(refusal).or_default() += 1;
    }

    fn note(&mut self, description: String) {
        if self.examples.len() < REPORTED_EXAMPLES {
            self.examples.push(description);
        }
    }

    fn report(&self, label: &str) {
        println!(
            "{label}: {} programs, {} rewritten, {} of {} attempted pairs compared ({} on rewritten programs)",
            self.programs,
            self.rewritten_programs,
            self.compared,
            self.attempts,
            self.rewritten_compared
        );
        println!(
            "    pass refusals {}, asymmetric refusals {} after-only / {} before-only",
            self.pass_refusals, self.refused_after_only, self.refused_before_only
        );
        for refusal in Refusal::ALL {
            let count = self.refusals.get(&refusal).copied().unwrap_or_default();
            if count > 0 {
                println!("    refused {}: {count}", refusal.label());
            }
        }
        println!(
            "    output divergences {}, local divergences {}",
            self.output_divergences, self.local_divergences
        );
        if self.quarantined_programs > 0 {
            println!(
                "    OPEN PASS DEFECT: {} programs still diverge on {} pairs",
                self.quarantined_programs, self.quarantined_divergences
            );
        }
        for example in &self.examples {
            println!("    {example}");
        }
    }

    /// Every floor and ceiling the corpus must satisfy to have proved anything.
    fn assert_sound(&self) {
        assert!(
            self.compared >= COMPARISON_FLOOR,
            "only {} program/entry pairs executed on both sides, below the floor of {COMPARISON_FLOOR}",
            self.compared
        );
        assert!(
            self.rewritten_compared >= REWRITTEN_COMPARISON_FLOOR,
            "only {} compared pairs came from a program compaction rewrote, below the floor of {REWRITTEN_COMPARISON_FLOOR}",
            self.rewritten_compared
        );
        assert!(
            self.rewritten_programs >= REWRITTEN_PROGRAM_FLOOR,
            "compaction rewrote only {} programs, below the floor of {REWRITTEN_PROGRAM_FLOOR}",
            self.rewritten_programs
        );
        self.assert_refusals_bounded();
        assert_eq!(
            self.refused_after_only, 0,
            "a compacted program left the evaluable subset its source was inside"
        );
        assert_eq!(
            self.output_divergences, 0,
            "compaction changed the final value of a function output"
        );
    }

    fn assert_refusals_bounded(&self) {
        let attempts = self.attempts as f64;
        for refusal in Refusal::ALL {
            let count = self.refusals.get(&refusal).copied().unwrap_or_default();
            assert!(
                count as f64 <= attempts * REFUSAL_SHARE_LIMIT,
                "refusal `{}` accounts for {count} of {} attempted pairs, above the {REFUSAL_SHARE_LIMIT} share limit",
                refusal.label(),
                self.attempts
            );
        }
        assert!(
            self.refused_before_only as f64 <= attempts * REFUSAL_SHARE_LIMIT,
            "the source refused and the rewrite ran on {} of {} attempted pairs, above the {REFUSAL_SHARE_LIMIT} share limit",
            self.refused_before_only,
            self.attempts
        );
        assert!(
            self.pass_refusals as f64 <= attempts * PASS_REFUSAL_SHARE_LIMIT,
            "the pass refused {} of {} attempted pairs, above the {PASS_REFUSAL_SHARE_LIMIT} share limit",
            self.pass_refusals,
            self.attempts
        );
    }
}

/// Execute one program against every entry environment and record the result.
fn compare(
    ledger: &mut Ledger,
    label: &str,
    body: &[rumoca_core::Statement],
    expectation: Expectation,
) {
    ledger.programs += 1;
    ledger.attempts += ENTRY_VALUES.len();
    let compacted = match compact(body) {
        Ok(compacted) => compacted,
        Err(error) => {
            ledger.pass_refusals += ENTRY_VALUES.len();
            ledger.note(format!("{label}: pass refused: {error}"));
            return;
        }
    };
    let rewritten = compacted != body;
    let compared_before = ledger.compared;
    let diverged_before = ledger.output_divergences;
    let examples_before = ledger.examples.len();
    for entry in &ENTRY_VALUES {
        let environment = entry.environment();
        compare_one(ledger, label, entry.label, body, &compacted, &environment);
    }
    if rewritten {
        ledger.rewritten_programs += 1;
        ledger.rewritten_compared += ledger.compared - compared_before;
    }
    if expectation == Expectation::OpenDefect {
        quarantine(ledger, label, diverged_before, examples_before);
    }
}

/// Move a listed program's divergences out of the theorem and require that it
/// still has some: a defect that stopped reproducing must be delisted, not
/// carried.
///
/// Every divergence a listed program produces is withdrawn, not just the one
/// the listed defect explains, so a second defect landing on the same program
/// would be withdrawn with it. The listed programs are named individually and
/// each must keep diverging, which bounds that to the handful of shapes named
/// in [`OPEN_PASS_DEFECT_SEEDS`].
fn quarantine(ledger: &mut Ledger, label: &str, diverged_before: usize, examples_before: usize) {
    let found = ledger.output_divergences - diverged_before;
    assert!(
        found > 0,
        "{label} is listed in OPEN_PASS_DEFECT_SEEDS but its outputs are now preserved; remove it"
    );
    ledger.output_divergences -= found;
    ledger.quarantined_programs += 1;
    ledger.quarantined_divergences += found;
    ledger.examples.truncate(examples_before);
}

fn compare_one(
    ledger: &mut Ledger,
    label: &str,
    entry_label: &str,
    body: &[rumoca_core::Statement],
    compacted: &[rumoca_core::Statement],
    environment: &Environment,
) {
    match (
        interpret(body, environment),
        interpret(compacted, environment),
    ) {
        (Ok(before), Ok(after)) => {
            ledger.compared += 1;
            record_agreement(ledger, label, entry_label, body, compacted, &before, &after);
        }
        (Err(before), Err(after)) => {
            ledger.count_refusal(before);
            if after != before {
                ledger.count_refusal(after);
            }
        }
        (Err(before), Ok(_)) => {
            ledger.count_refusal(before);
            ledger.refused_before_only += 1;
        }
        (Ok(_), Err(after)) => {
            ledger.count_refusal(after);
            ledger.refused_after_only += 1;
            ledger.note(format!(
                "{label}/{entry_label}: the rewrite refused with `{}`: {}",
                after.label(),
                render(compacted)
            ));
        }
    }
}

fn record_agreement(
    ledger: &mut Ledger,
    label: &str,
    entry_label: &str,
    body: &[rumoca_core::Statement],
    compacted: &[rumoca_core::Statement],
    before: &Environment,
    after: &Environment,
) {
    let observable = observable_names();
    if differing_names(&observable, before, after).is_empty() {
        if !differing_names(&local_names(), before, after).is_empty() {
            ledger.local_divergences += 1;
        }
        return;
    }
    ledger.output_divergences += 1;
    ledger.note(format!(
        "{label}/{entry_label}: outputs diverged\n        source:    {}\n        compacted: {}\n        before:    {}\n        after:     {}",
        render(body),
        render(compacted),
        render_environment(before, &observable),
        render_environment(after, &observable),
    ));
}

fn differing_names(names: &[VarName], before: &Environment, after: &Environment) -> Vec<VarName> {
    names
        .iter()
        .filter(|name| before.get(*name) != after.get(*name))
        .cloned()
        .collect()
}

/// The shapes every run must compare, whatever the generator draws.
///
/// Two families are stated here rather than left to the corpus. The first
/// re-expresses the loop-carry regressions this module is written about as
/// executable programs over the declaration table. The second pins the
/// whole-array shapes: an array local assigned unsubscripted is a substitution
/// target that only the partial-assignment and non-rewritable-read predicates
/// reject, so each of those predicates gets a shape whose meaning changes when
/// the predicate stops answering.
fn regression_programs() -> Vec<(&'static str, Vec<rumoca_core::Statement>)> {
    let read = |name: &str| element(name, var("i"));
    vec![
        (
            "unconditional overwrite carry read after the loop",
            vec![
                assign("a", real(-1.0)),
                for_loop("i", 3, vec![assign("a", read("r"))]),
                assign("y", var("a")),
            ],
        ),
        (
            "accumulate-style carry",
            vec![
                assign("a", real(0.0)),
                for_loop(
                    "i",
                    3,
                    vec![assign("a", binary(OpBinary::Add, var("a"), read("r")))],
                ),
                assign("y", var("a")),
            ],
        ),
        (
            "if-guarded write",
            vec![
                assign("a", real(-1.0)),
                for_loop(
                    "i",
                    3,
                    vec![branch(
                        vec![(
                            binary(OpBinary::Gt, read("r"), real(0.0)),
                            vec![assign("a", read("r"))],
                        )],
                        None,
                    )],
                ),
                assign("y", var("a")),
            ],
        ),
        (
            "same-iteration temporary",
            vec![
                for_loop(
                    "i",
                    3,
                    vec![
                        assign("a", binary(OpBinary::Mul, read("r"), real(2.0))),
                        assign_element("w", var("i"), var("a")),
                    ],
                ),
                assign("y", real(0.0)),
            ],
        ),
        (
            "whole-array local read at one coordinate",
            whole_array_local_read_at_one_coordinate(),
        ),
        (
            "whole-array local overwritten at one coordinate",
            whole_array_local_overwritten_at_one_coordinate(),
        ),
        (
            "whole-array local rewritten inside a nested loop",
            whole_array_local_rewritten_inside_a_nested_loop(),
        ),
        (
            "conditionally redefined temporary",
            conditionally_redefined_temporary(),
        ),
        ("nested outer back-edge carry", nested_outer_back_edge()),
        ("two enclosing back edges", two_enclosing_back_edges()),
    ]
}

/// ```text
/// for i in 1:2 loop t := r; b := t[i]; w[i] := b; end for;
/// ```
///
/// `t := r` is a live store: the next statement reads one coordinate of `t`,
/// and that coordinate reaches `w`. A subscripted read is invisible to
/// `statements_read_name`, so the non-rewritable-read predicate is the only
/// thing standing between this store and deletion.
fn whole_array_local_read_at_one_coordinate() -> Vec<rumoca_core::Statement> {
    vec![for_loop(
        "i",
        2,
        vec![
            assign("t", var("r")),
            assign("b", element("t", var("i"))),
            assign_element("w", var("i"), var("b")),
        ],
    )]
}

/// ```text
/// for i in 1:2 loop t := r; t[1] := u; w := t; end for;
/// ```
///
/// `t[1] := u` overwrites one coordinate of the value `t := r` established, so
/// substituting `r` into the later whole-array read of `t` drops that
/// overwrite. Only the partial-assignment predicate sees the subscripted
/// write.
fn whole_array_local_overwritten_at_one_coordinate() -> Vec<rumoca_core::Statement> {
    vec![for_loop(
        "i",
        2,
        vec![
            assign("t", var("r")),
            assign_element("t", integer(1), var("u")),
            assign("w", var("t")),
        ],
    )]
}

/// ```text
/// for i in 1:2 loop
///   t := r;
///   for j in 1:2 loop t[1] := u; w := t; end for;
/// end for;
/// y := r[1];
/// ```
///
/// The same overwrite, moved inside the loop body's trailing statement, which
/// is the segment `loop_local_substitutions` inspects. The trailing
/// `y := r[1]` keeps the outer loop from being the whole body's last
/// statement, so the prefix substitution path is the one that runs.
fn whole_array_local_rewritten_inside_a_nested_loop() -> Vec<rumoca_core::Statement> {
    vec![
        for_loop(
            "i",
            2,
            vec![
                assign("t", var("r")),
                for_loop(
                    "j",
                    2,
                    vec![
                        assign_element("t", integer(1), var("u")),
                        assign("w", var("t")),
                    ],
                ),
            ],
        ),
        assign("y", element("r", integer(1))),
    ]
}

/// ```text
/// for i in 1:2 loop a := u; if f then a := v; end if; w[i] := a; end for;
/// ```
///
/// The shortest body holding a definition, a conditional redefinition and a
/// use of the same local. Deleting `a := u` feeds the entry value of `a` to
/// `w[i]` on the iterations the branch is not taken, and ignoring the
/// redefinition lets `u` be substituted past the branch that replaces it.
fn conditionally_redefined_temporary() -> Vec<rumoca_core::Statement> {
    vec![for_loop(
        "i",
        2,
        vec![
            assign("a", var("u")),
            branch(vec![(var("f"), vec![assign("a", var("v"))])], None),
            assign_element("w", var("i"), var("a")),
        ],
    )]
}

/// ```text
/// a := 0; for i loop w[i] := a; for j loop a := 1; a := s[j]; end for; end for;
/// ```
///
/// The outer loop re-enters `w[i] := a` without re-running the inner body, so
/// the inner body's own write cannot settle the value that read observes.
fn nested_outer_back_edge() -> Vec<rumoca_core::Statement> {
    vec![
        assign("a", real(0.0)),
        for_loop(
            "i",
            3,
            vec![
                assign_element("w", var("i"), var("a")),
                for_loop(
                    "j",
                    2,
                    vec![assign("a", real(1.0)), assign("a", element("s", var("j")))],
                ),
            ],
        ),
        assign("y", var("a")),
    ]
}

/// ```text
/// for i loop y := a; for j loop a := 1; for l loop a := 2; end for; end for; end for;
/// ```
///
/// With two enclosing loops, sequencing the re-entry segments would let the
/// middle body's write hide the outer body's read of the same value.
fn two_enclosing_back_edges() -> Vec<rumoca_core::Statement> {
    vec![
        assign("a", real(7.0)),
        for_loop(
            "i",
            2,
            vec![
                assign("y", var("a")),
                for_loop(
                    "j",
                    2,
                    vec![
                        assign("a", real(1.0)),
                        for_loop("l", 2, vec![assign("a", real(2.0))]),
                    ],
                ),
            ],
        ),
    ]
}

/// The interpreter must actually execute; a no-op interpreter would make the
/// whole differential vacuously green.
#[test]
fn the_interpreter_executes_loops_and_branches() {
    let entry = ENTRY_VALUES[0].environment();
    let expected = [
        // `r` enters as {1, 2, 3}, so the overwriting carry ends at 3.
        ("unconditional overwrite carry read after the loop", 3.0),
        // 0 + 1 + 2 + 3.
        ("accumulate-style carry", 6.0),
        // Every element is positive, so the guard is taken every iteration.
        ("if-guarded write", 3.0),
        // The epilogue writes the literal zero.
        ("same-iteration temporary", 0.0),
        // `s` enters as {4, 5, 6} and the inner loop leaves `s[2]`.
        ("nested outer back-edge carry", 5.0),
        // The second outer iteration reads what the innermost loop left.
        ("two enclosing back edges", 2.0),
    ];
    for (label, value) in expected {
        let (_, body) = regression_programs()
            .into_iter()
            .find(|(name, _)| *name == label)
            .expect("the regression program is present");
        let final_environment =
            interpret(&body, &entry).expect("the regression program is evaluable");
        assert_eq!(
            final_environment.get(&VarName::new("y")),
            Some(&Value::Real(value)),
            "the interpreter computed a different `y` for {label}: {}",
            render(&body)
        );
    }
}

/// The array written inside the nested regression must record one value per
/// outer iteration, which is what the outer back edge carries.
#[test]
fn the_interpreter_records_the_outer_back_edge_carry() {
    let entry = ENTRY_VALUES[0].environment();
    let body = nested_outer_back_edge();
    let final_environment = interpret(&body, &entry).expect("the regression program is evaluable");
    let expected = Value::Array(vec![Value::Real(0.0), Value::Real(5.0), Value::Real(5.0)]);
    assert_eq!(
        final_environment.get(&VarName::new("w")),
        Some(&expected),
        "the outer iterations did not observe the carried value: {}",
        render(&body)
    );
}

/// The whole-array shapes must compute the values their guards protect, or the
/// preservation theorem would be agreeing about them for the wrong reason.
#[test]
fn the_interpreter_assigns_whole_array_locals() {
    let entry = ENTRY_VALUES[0].environment();
    // `r` enters as {1, 2, 3}, `u` as 2, `f` as false and `w` as {0, 0, 0}.
    let expected = [
        ("whole-array local read at one coordinate", [1.0, 2.0, 0.0]),
        (
            "whole-array local overwritten at one coordinate",
            [2.0, 2.0, 3.0],
        ),
        (
            "whole-array local rewritten inside a nested loop",
            [2.0, 2.0, 3.0],
        ),
        ("conditionally redefined temporary", [2.0, 2.0, 0.0]),
    ];
    for (label, values) in expected {
        let (_, body) = regression_programs()
            .into_iter()
            .find(|(name, _)| *name == label)
            .expect("the regression program is present");
        let final_environment =
            interpret(&body, &entry).expect("the regression program is evaluable");
        let expected = Value::Array(values.iter().copied().map(Value::Real).collect());
        assert_eq!(
            final_environment.get(&VarName::new("w")),
            Some(&expected),
            "the interpreter computed a different `w` for {label}: {}",
            render(&body)
        );
    }
}

/// Feed the comparison a rewrite that deletes a live store and require it to
/// be reported. Without this the floors alone could not distinguish a working
/// differential from one that compares nothing meaningful.
#[test]
fn the_differential_reports_a_deleted_live_store() {
    // A sentinel no entry environment can produce, so no entry hides the
    // deletion behind an arithmetic coincidence.
    let sentinel = real(-100.0);
    let body = vec![
        assign("a", sentinel.clone()),
        for_loop("i", 3, vec![assign("a", element("r", var("i")))]),
        assign("y", var("a")),
    ];
    let broken = vec![
        assign("a", sentinel),
        for_loop("i", 3, Vec::new()),
        assign("y", var("a")),
    ];
    let mut ledger = Ledger::default();
    for entry in &ENTRY_VALUES {
        let environment = entry.environment();
        compare_one(
            &mut ledger,
            "deleted live store",
            entry.label,
            &body,
            &broken,
            &environment,
        );
    }
    assert_eq!(
        ledger.compared,
        ENTRY_VALUES.len(),
        "the probe programs must execute on every entry environment"
    );
    assert_eq!(
        ledger.output_divergences,
        ENTRY_VALUES.len(),
        "deleting the loop body's live store went unreported"
    );
}

/// The regression shapes must be preserved individually, with no ledger floor
/// standing in for the check.
#[test]
fn compaction_preserves_the_known_regression_shapes() {
    let mut ledger = Ledger::default();
    for (label, body) in regression_programs() {
        compare(&mut ledger, label, &body, Expectation::Preserved);
    }
    ledger.report("regression shapes");
    assert_eq!(
        ledger.pass_refusals, 0,
        "the pass refused a regression shape it must compact"
    );
    assert_eq!(
        ledger.compared,
        ledger.programs * ENTRY_VALUES.len(),
        "a regression shape left the evaluable subset"
    );
    assert_eq!(
        ledger.output_divergences, 0,
        "compaction changed a regression shape's outputs"
    );
}

/// The one program the open defect is stated on.
///
/// ```text
/// for i in 1:2 loop
///   a := b;                 // the loop body's scalar prefix
///   for j in 1:2 loop       // the body's trailing statement
///     b := 1.0;             // writes a dependency of the prefix value
///     w[i] := a;            // reads the prefix value after that write
///   end for;
/// end for;
/// ```
fn loop_local_prefix_defect() -> Vec<rumoca_core::Statement> {
    vec![for_loop(
        "i",
        2,
        vec![
            assign("a", var("b")),
            for_loop(
                "j",
                2,
                vec![
                    assign("b", real(1.0)),
                    assign_element("w", var("i"), var("a")),
                ],
            ),
        ],
    )]
}

/// ```text
/// for i in 1:3 loop
///   for j in 1:2 loop w[2] := u; end for;   // writes one fixed coordinate
///   for j in 1:2 loop w[i] := v; end for;   // writes the binder's coordinate
/// end for;
/// ```
///
/// Both segments write `w`, and at `i = 2` they write the same coordinate, so
/// the outer iterations are not disjoint partitions of `w`.
fn outer_distribution_defect() -> Vec<rumoca_core::Statement> {
    vec![for_loop(
        "i",
        3,
        vec![
            for_loop("j", 2, vec![assign_element("w", integer(2), var("u"))]),
            for_loop("j", 2, vec![assign_element("w", var("i"), var("v"))]),
        ],
    )]
}

/// How many entry environments this rewrite changes an output on.
fn diverging_entries(
    body: &[rumoca_core::Statement],
    compacted: &[rumoca_core::Statement],
) -> usize {
    let observable = observable_names();
    ENTRY_VALUES
        .iter()
        .filter(|entry| {
            let environment = entry.environment();
            let before = interpret(body, &environment).expect("the shape is evaluable");
            let after = interpret(compacted, &environment).expect("the rewrite is evaluable");
            !differing_names(&observable, &before, &after).is_empty()
        })
        .count()
}

/// Compaction substitutes `a := b` into `w[i] := a`, which sits after a write
/// to `b`, so the substituted read observes the new `b` instead of the one the
/// definition read (MLS §11.1 orders the statements of an algorithm section).
///
/// `loop_local_substitutions` inlines a loop body's scalar prefix into the
/// body's trailing statement without asking whether the value's dependencies
/// are written again inside that trailing statement. Its two sibling
/// substitution paths, `inline_dominated_loop_locals` and
/// `inline_straight_line_scalar_definitions`, both ask exactly that through
/// `expression_dependencies_change`.
///
/// This test states the defect rather than the theorem, so fixing the pass
/// requires deleting it along with the matching entries in
/// [`OPEN_PASS_DEFECT_SEEDS`].
#[test]
fn a_loop_local_prefix_substitution_moves_a_read_past_a_later_write() {
    let body = loop_local_prefix_defect();
    let compacted = compact(&body).expect("the pass accepts the shape");
    assert_ne!(
        compacted,
        body,
        "the pass no longer rewrites the shape: {}",
        render(&body)
    );
    assert_eq!(
        diverging_entries(&body, &compacted),
        ENTRY_VALUES.len(),
        "the substitution no longer changes the output: {} became {}",
        render(&body),
        render(&compacted)
    );
}

/// The second program the open defects are stated on.
///
/// ```text
/// for i in 1:2 loop
///   a := 1.0;                 // the loop body's scalar prefix
///   for j in 1:2 loop         // the body's trailing statement
///     w[i] := a;              // reads the prefix value
///     a := 2.0;               // and then replaces it
///   end for;
/// end for;
/// ```
fn loop_local_target_rewrite_defect() -> Vec<rumoca_core::Statement> {
    vec![for_loop(
        "i",
        2,
        vec![
            assign("a", real(1.0)),
            for_loop(
                "j",
                2,
                vec![
                    assign_element("w", var("i"), var("a")),
                    assign("a", real(2.0)),
                ],
            ),
        ],
    )]
}

/// Compaction substitutes `a := 1.0` into `w[i] := a`, which sits inside a loop
/// that writes `a` itself. MLS §11.2.2 re-enters that loop's body from its
/// first statement, so from the second inner iteration onwards the read
/// observes what the inner body last wrote, not what the prefix established.
///
/// `loop_local_substitutions` asks `statements_partially_assign_name` whether
/// the trailing statement writes part of the target, and never asks whether it
/// writes the whole of it. Its sibling `inline_dominated_loop_locals` asks
/// exactly that through `statements_assign_name`, and refuses this very pair of
/// statements when it meets them as a straight-line suffix.
///
/// This test states the defect rather than the theorem, so fixing the pass
/// requires deleting it along with the matching entries in
/// [`OPEN_PASS_DEFECT_SEEDS`].
#[test]
fn a_loop_local_prefix_substitution_ignores_a_rewrite_of_its_target() {
    let body = loop_local_target_rewrite_defect();
    let compacted = compact(&body).expect("the pass accepts the shape");
    assert_ne!(
        compacted,
        body,
        "the pass no longer rewrites the shape: {}",
        render(&body)
    );
    assert_eq!(
        diverging_entries(&body, &compacted),
        ENTRY_VALUES.len(),
        "the substitution no longer changes the output: {} became {}",
        render(&body),
        render(&compacted)
    );
}

/// Outer-loop distribution splits `for i loop A(i); B(i); end for` into two
/// loops without proving that `A` and `B` touch disjoint coordinates.
///
/// `loop_values_partitioned_by` collects the mutated values under their bare
/// root identifier, then tests each reference under
/// `ComponentReference::to_var_name`, which renders a constant subscript into
/// the name: `w[2]` is looked up as `w[2]` and never matches the recorded `w`.
/// The one reference that breaks the partition is therefore the one reference
/// the check cannot see, and the proof succeeds vacuously.
///
/// This test states the defect rather than the theorem, so fixing the pass
/// requires deleting it along with the matching entries in
/// [`OPEN_PASS_DEFECT_SEEDS`].
#[test]
fn outer_loop_distribution_reorders_writes_to_a_constant_coordinate() {
    let body = outer_distribution_defect();
    let compacted = compact(&body).expect("the pass accepts the shape");
    assert_ne!(
        compacted,
        body,
        "the pass no longer distributes the shape: {}",
        render(&body)
    );
    assert!(
        diverging_entries(&body, &compacted) >= 3,
        "distribution no longer reorders the writes: {} became {}",
        render(&body),
        render(&compacted)
    );
}

/// The preservation theorem over the whole corpus.
#[test]
fn compaction_preserves_meaning_over_the_generated_corpus() {
    let mut ledger = Ledger::default();
    for (label, body) in regression_programs() {
        compare(&mut ledger, label, &body, Expectation::Preserved);
    }
    for seed in 0..GENERATED_PROGRAMS {
        let body = generated_program(seed);
        let expectation = if OPEN_PASS_DEFECT_SEEDS.contains(&seed) {
            Expectation::OpenDefect
        } else {
            Expectation::Preserved
        };
        compare(&mut ledger, &format!("seed {seed}"), &body, expectation);
    }
    ledger.report("generated corpus");
    assert_eq!(
        ledger.quarantined_programs,
        OPEN_PASS_DEFECT_SEEDS.len(),
        "an open-defect seed fell out of the corpus instead of being compared"
    );
    ledger.assert_sound();
}
