//! Ratchet on lint suppression attributes in hand-written production source.
//!
//! A suppression attribute answers a lint by silencing it rather than by
//! changing the design the lint objected to, so the set of them may only
//! shrink. The gate takes a census of every `allow` and `expect` attribute
//! under `crates/`, separates the three populations that carry different
//! obligations, and pins the hand-written production population file by file.
//!
//! The three populations are:
//!
//! * generated source, recognised by a `generated` path component. A grammar
//!   trait emitted by a parser generator carries whatever suppressions the
//!   generator writes; nobody edits it, so pinning it would pin the generator.
//!   The carve-out is asserted rather than assumed: the census must actually
//!   find suppressions there, so silently losing the classification shows up.
//! * test source: a file outside a crate's `src` directory, a file reachable
//!   only from a `#[cfg(test)] mod` declaration, and an attribute written
//!   inside a `#[cfg(test)]` item or behind `cfg_attr(test, ...)`. None of it
//!   reaches a release build.
//! * everything else, which is hand-written production source and is pinned.
//!
//! `allow` and `expect` are pinned in separate tables. `expect` is the better
//! of the two, because it fails when the lint it names stops firing, but it is
//! still a suppression: keeping the two counts apart is what stops a rewrite of
//! `allow` into `expect` from reading as a reduction.
//!
//! The gate is a suppression census, not a semantic proof. It reads syntax: it
//! knows that an attribute silencing a lint exists and where, and it knows
//! nothing about whether the code under the attribute would still trip the
//! lint, whether the underlying design problem was fixed or merely moved, or
//! whether a suppression that disappeared was replaced by a different evasion
//! such as a wider function boundary or a suppression raised to the manifest.
//! Falling counts are evidence that the population is shrinking, not proof
//! that the design improved.

use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Path, PathBuf};

use quote::ToTokens;
use syn::parse::Parser;
use syn::punctuated::Punctuated;
use syn::visit::{self, Visit};

use crate::architecture_hardening_support::{collect_rs_files, workspace_root};
use crate::totality_debt::gate::production_source_files_under;

/// Hand-written production `allow` counts, keyed by the path of the file
/// relative to `crates/`.
///
/// Exceeding an entry, or introducing a file that has none, fails the gate.
/// Falling below an entry passes and changes nothing: ratcheting down is done
/// by editing this table, which is the intended direction of travel.
const PRODUCTION_ALLOW_BASELINE: &[(&str, usize)] = &[
    ("rumoca-eval-solve/src/compute_block_scalarize/dense.rs", 1),
    ("rumoca-eval-solve/src/lib.rs", 2),
    ("rumoca-eval-solve/src/typed_program/mod.rs", 1),
    ("rumoca-exec-cranelift/src/emit.rs", 1),
    ("rumoca-exec-cranelift/src/emit/typed_program.rs", 1),
    ("rumoca-exec-mlir/src/lib.rs", 1),
    ("rumoca-ir-ast/src/instance/equality_constraint.rs", 1),
    (
        "rumoca-ir-ast/src/instance/equality_constraint/occurrence/errors.rs",
        1,
    ),
    ("rumoca-ir-ast/src/semantic_identity.rs", 1),
    ("rumoca-ir-ast/src/visitor/rewrite.rs", 1),
    ("rumoca-ir-solve/src/algorithm_block/call_transfer.rs", 2),
    ("rumoca-ir-solve/src/model.rs", 1),
    (
        "rumoca-ir-solve/src/typed_program/program/directional.rs",
        4,
    ),
    ("rumoca-ir-solve/src/typed_program/program/wire.rs", 1),
    ("rumoca-phase-codegen/src/codegen/render_solve.rs", 1),
    (
        "rumoca-phase-codegen/src/codegen/render_solve/template_partition.rs",
        1,
    ),
    ("rumoca-phase-codegen/src/codegen/scalar_program_plan.rs", 2),
    ("rumoca-phase-flatten/src/equations/mod.rs", 1),
    (
        "rumoca-phase-flatten/src/equations/zero_sized_reductions.rs",
        1,
    ),
    (
        "rumoca-phase-flatten/src/pipeline/context_and_tests/class_instance.rs",
        1,
    ),
    ("rumoca-phase-instantiate/src/lib.rs", 1),
    ("rumoca-phase-solve/src/ad.rs", 6),
    ("rumoca-phase-solve/src/lower.rs", 1),
    ("rumoca-phase-solve/src/lower/events.rs", 1),
    ("rumoca-phase-solve/src/lower/events/structured.rs", 1),
    ("rumoca-phase-solve/src/lower/scalar.rs", 2),
    (
        "rumoca-phase-solve/src/lower/scalar/call_scoped_actions.rs",
        1,
    ),
    ("rumoca-phase-solve/src/lower/scalar/coordinates.rs", 1),
    ("rumoca-phase-solve/src/lower/scalar/functions.rs", 17),
    ("rumoca-phase-solve/src/lower/typed_functions.rs", 7),
    (
        "rumoca-phase-solve/src/lower/typed_functions/model_events.rs",
        1,
    ),
    ("rumoca-phase-solve/src/lower/typed_functions/tensor.rs", 2),
    ("rumoca-sim/src/scheduled_sim/executor.rs", 3),
    ("rumoca-solver/src/runtime/solve_runtime.rs", 1),
];

/// Hand-written production `expect` counts, pinned in the same shape and for
/// the same reason. Rewriting an `allow` into an `expect` moves a row from the
/// table above into this one; because both are pinned, the move is visible
/// instead of reading as a reduction.
const PRODUCTION_EXPECT_BASELINE: &[(&str, usize)] = &[
    ("rumoca-bind-python/src/lib.rs", 1),
    ("rumoca-exec-cranelift/src/emit.rs", 6),
    ("rumoca-exec-cranelift/src/emit/interpreter.rs", 1),
    ("rumoca-phase-dae/src/construction/algorithm_lowering.rs", 1),
    ("rumoca-phase-solve/src/ad.rs", 1),
    (
        "rumoca-phase-structural/src/dae_transform/expressions.rs",
        1,
    ),
    ("rumoca-reference/src/value.rs", 1),
    ("rumoca-tool-lsp/src/handlers/document_symbols.rs", 1),
    ("rumoca-tool-lsp/src/handlers/workspace_symbols.rs", 1),
];

const GUIDANCE: &str = "\
A suppression attribute answers a lint by silencing it instead of by changing \
the design the lint objected to. Decompose the function, introduce the typed \
parameter or config struct, box the oversized error, or convert the cast into \
a checked conversion on an existing error path. If none of those apply because \
the surrounding code cannot represent the failure, that is a design question \
to settle before the suppression lands, not after.";

/// Where a source file sits relative to the release build.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum SourceKind {
    /// Emitted by a generator; a `generated` path component marks it.
    Generated,
    /// Absent from a release build: outside `src`, or `#[cfg(test)]`-only.
    Test,
    /// Hand-written and compiled into a release build.
    Production,
}

/// Which spelling of the suppression the author used.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum SuppressionForm {
    Allow,
    Expect,
}

impl SuppressionForm {
    fn label(self) -> &'static str {
        match self {
            SuppressionForm::Allow => "allow",
            SuppressionForm::Expect => "expect",
        }
    }
}

/// Which lint namespace a suppression names.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum LintTool {
    Clippy,
    Rustc,
}

/// One `allow` or `expect` attribute.
#[derive(Clone, Debug)]
struct Suppression {
    /// The attribute as written, whitespace removed.
    text: String,
    form: SuppressionForm,
    /// `Clippy` when any named lint is `clippy::`-qualified.
    tool: LintTool,
    /// Whether the attribute is written inside a `#[cfg(test)]` item or is
    /// itself applied only under `cfg_attr(test, ...)`.
    under_cfg_test: bool,
}

/// The suppressions one file carries, with the population it belongs to.
#[derive(Clone, Debug)]
struct FileCensus {
    kind: SourceKind,
    suppressions: Vec<Suppression>,
}

impl FileCensus {
    /// Suppressions of one form that reach a release build in hand-written
    /// source.
    fn production_suppressions(&self, form: SuppressionForm) -> Vec<&Suppression> {
        if self.kind != SourceKind::Production {
            return Vec::new();
        }
        self.suppressions
            .iter()
            .filter(|suppression| suppression.form == form && !suppression.under_cfg_test)
            .collect()
    }

    fn production_count(&self, form: SuppressionForm) -> usize {
        self.production_suppressions(form).len()
    }
}

#[test]
fn production_lint_allows_do_not_increase() {
    assert_ratchet_holds(SuppressionForm::Allow, PRODUCTION_ALLOW_BASELINE);
}

#[test]
fn production_lint_expectations_do_not_increase() {
    assert_ratchet_holds(SuppressionForm::Expect, PRODUCTION_EXPECT_BASELINE);
}

fn assert_ratchet_holds(form: SuppressionForm, baseline: &[(&str, usize)]) {
    let census = census_of(&workspace_root().join("crates"));
    let violations = ratchet_violations(&census, baseline, form);
    assert!(
        violations.is_empty(),
        "hand-written production `{}` suppressions rose above their pinned baseline:\n{}\n\n\
{GUIDANCE}\n\nMeasured production `{}` inventory:\n{}",
        form.label(),
        violations.join("\n"),
        form.label(),
        measured_inventory(&census, form).join("\n")
    );
}

/// A baseline entry that no measured file can reach is unreadable as a pin, so
/// both tables are kept sorted, unique, and non-zero.
#[test]
fn the_baseline_tables_are_readable_pins() {
    for baseline in [PRODUCTION_ALLOW_BASELINE, PRODUCTION_EXPECT_BASELINE] {
        let mut seen = BTreeSet::new();
        let mut previous: Option<&str> = None;
        for (path, count) in baseline {
            assert!(
                seen.insert(*path),
                "{path} is pinned twice; one file has one count"
            );
            if let Some(previous) = previous {
                assert!(
                    previous < *path,
                    "the baseline is read by hand and must stay sorted: {previous} precedes {path}"
                );
            }
            previous = Some(path);
            assert!(
                *count > 0,
                "{path} is pinned at zero; delete the row instead, so that a reintroduced \
suppression is reported as a new file"
            );
        }
    }
}

/// The generated carve-out is real only if generated source actually carries
/// suppressions the gate is declining to pin. If a generator stops emitting
/// them, or the classification stops recognising the path, this test says so
/// rather than letting the exemption quietly cover nothing or everything.
#[test]
fn the_generated_parser_carve_out_is_exercised() {
    let census = census_of(&workspace_root().join("crates"));
    let generated: Vec<&String> = census
        .iter()
        .filter(|(_, file)| file.kind == SourceKind::Generated && !file.suppressions.is_empty())
        .map(|(path, _)| path)
        .collect();
    assert!(
        !generated.is_empty(),
        "no generated source carries a suppression; the carve-out is either dead or the \
`generated` path component is no longer where generated source lives"
    );
    for path in &generated {
        assert!(
            !is_pinned(path),
            "{path} is generated and must not be pinned as hand-written production source"
        );
    }
}

/// Every pinned file must be classified as production and must exist. A pin
/// that names a file the census never reaches enforces nothing.
#[test]
fn every_pinned_file_is_measured_production_source() {
    let census = census_of(&workspace_root().join("crates"));
    let mut unreachable = Vec::new();
    for baseline in [PRODUCTION_ALLOW_BASELINE, PRODUCTION_EXPECT_BASELINE] {
        for (path, _) in baseline {
            match census.get(*path) {
                Some(file) if file.kind == SourceKind::Production => {}
                Some(file) => unreachable.push(format!("{path}: classified {:?}", file.kind)),
                None => unreachable.push(format!("{path}: no such file under crates/")),
            }
        }
    }
    assert!(
        unreachable.is_empty(),
        "these baseline rows pin nothing the census can measure; delete a row when its file \
goes away, and never pin generated or test source: {unreachable:#?}"
    );
}

/// The census must reach every population, or a classification bug that folds
/// one population into another passes unnoticed. Both lint namespaces must
/// also be represented, so that a scan which silently stopped recognising one
/// of them cannot look like a burn-down.
#[test]
fn the_census_separates_every_population_it_classifies() {
    let census = census_of(&workspace_root().join("crates"));
    for kind in [
        SourceKind::Generated,
        SourceKind::Test,
        SourceKind::Production,
    ] {
        assert!(
            census.values().any(|file| file.kind == kind),
            "the census classified no file as {kind:?}"
        );
    }
    for tool in [LintTool::Clippy, LintTool::Rustc] {
        assert!(
            census
                .values()
                .flat_map(|file| file.suppressions.iter())
                .any(|suppression| suppression.tool == tool),
            "the census found no {tool:?} suppression anywhere in the workspace"
        );
    }
    for (path, file) in &census {
        if file.kind == SourceKind::Production {
            assert!(
                !path.contains("/generated/"),
                "{path} has a `generated` path component but was classified production"
            );
        }
    }
}

/// A planted mutation, run against a fixture tree with known contents: the
/// gate must catch a suppression added to a pinned production file and a
/// suppression added to a production file the baseline does not name, must
/// stay silent for the generated and test populations and for a count that
/// falls, and must not accept an `expect` in place of a pinned `allow`.
#[test]
fn the_ratchet_catches_a_planted_suppression() {
    let fixture = Fixture::new("ratchet");
    // The attribute text is assembled rather than written, so this file does
    // not contain literal suppressions of its own for the census, the
    // SPEC_0021 exception gate, or the construction-lint gate to read.
    let hash = || String::from("#");
    let cast = format!("{}[allow(clippy::cast_precision_loss)]", hash());
    let dead = format!("{}[allow(dead_code)]", hash());
    let expected_cast = format!("{}[expect(clippy::cast_precision_loss)]", hash());
    let cfg_test = format!("{}[cfg(test)]", hash());

    let pinned_lib = |extra: &str| {
        format!(
            "mod model;\nmod emitted;\n{cfg_test}\nmod suite;\n{cast}\nfn pinned(v: f64) -> f64 {{ v }}\n{extra}"
        )
    };
    fixture.write("fixture-crate/src/lib.rs", &pinned_lib(""));
    fixture.write("fixture-crate/src/model.rs", "fn clean() {}\n");
    fixture.write("fixture-crate/src/emitted.rs", "pub mod generated;\n");
    fixture.write(
        "fixture-crate/src/emitted/generated/grammar.rs",
        &format!("{dead}\nfn emitted() {{}}\n"),
    );
    fixture.write(
        "fixture-crate/src/suite.rs",
        &format!("{cast}\nfn only_under_test(v: f64) -> f64 {{ v }}\n"),
    );
    fixture.write(
        "fixture-crate/tests/integration.rs",
        &format!("{cast}\nfn harness(v: f64) -> f64 {{ v }}\n"),
    );

    let allows: &[(&str, usize)] = &[("fixture-crate/src/lib.rs", 1)];
    let expects: &[(&str, usize)] = &[];

    let census = census_of(&fixture.root);
    assert_eq!(
        census["fixture-crate/src/lib.rs"].kind,
        SourceKind::Production
    );
    assert_eq!(census["fixture-crate/src/suite.rs"].kind, SourceKind::Test);
    assert_eq!(
        census["fixture-crate/tests/integration.rs"].kind,
        SourceKind::Test
    );
    assert_eq!(
        census["fixture-crate/src/emitted/generated/grammar.rs"].kind,
        SourceKind::Generated
    );
    assert!(
        ratchet_violations(&census, allows, SuppressionForm::Allow).is_empty()
            && ratchet_violations(&census, expects, SuppressionForm::Expect).is_empty(),
        "the unmutated fixture must sit exactly on its baseline"
    );

    // A suppression added to a pinned file.
    fixture.write(
        "fixture-crate/src/lib.rs",
        &pinned_lib(&format!("{dead}\nfn planted() {{}}\n")),
    );
    let violations = ratchet_violations(&census_of(&fixture.root), allows, SuppressionForm::Allow);
    assert_eq!(
        violations.len(),
        1,
        "one planted suppression must produce one violation: {violations:#?}"
    );
    assert!(
        violations[0].contains("fixture-crate/src/lib.rs") && violations[0].contains("baseline 1"),
        "the violation must name the file and its pin: {}",
        violations[0]
    );

    // A suppression added to a production file the baseline does not name.
    fixture.write(
        "fixture-crate/src/model.rs",
        &format!("{dead}\nfn now_suppressed() {{}}\n"),
    );
    let violations = ratchet_violations(&census_of(&fixture.root), allows, SuppressionForm::Allow);
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains("fixture-crate/src/model.rs")
                && violation.contains("not pinned")),
        "an unpinned production file that gains a suppression must be reported as new: \
{violations:#?}"
    );

    // Rewriting the pinned `allow` as an `expect` empties the allow count but
    // must be caught by the expect table rather than reading as a burn-down.
    fixture.write("fixture-crate/src/model.rs", "fn clean() {}\n");
    fixture.write(
        "fixture-crate/src/lib.rs",
        &format!(
            "mod model;\nmod emitted;\n{cfg_test}\nmod suite;\n{expected_cast}\nfn pinned(v: f64) -> f64 {{ v }}\n"
        ),
    );
    let census = census_of(&fixture.root);
    assert_eq!(
        census["fixture-crate/src/lib.rs"].production_count(SuppressionForm::Allow),
        0
    );
    assert!(
        ratchet_violations(&census, allows, SuppressionForm::Allow).is_empty(),
        "a falling allow count is silent on its own"
    );
    let violations = ratchet_violations(&census, expects, SuppressionForm::Expect);
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains("fixture-crate/src/lib.rs")),
        "an allow rewritten as an expect must surface in the expect table: {violations:#?}"
    );

    // Test and generated source may gain suppressions freely, and a
    // suppression inside a `#[cfg(test)]` item of a production file is test
    // code even though the file itself ships.
    fixture.write(
        "fixture-crate/src/suite.rs",
        &format!("{cast}\n{dead}\nfn only_under_test(v: f64) -> f64 {{ v }}\n"),
    );
    fixture.write(
        "fixture-crate/src/emitted/generated/grammar.rs",
        &format!("{dead}\n{cast}\nfn emitted(v: f64) -> f64 {{ v }}\n"),
    );
    fixture.write(
        "fixture-crate/src/lib.rs",
        &format!(
            "mod model;\nmod emitted;\n{cfg_test}\nmod inline {{ {cast}\nfn t(v: f64) -> f64 {{ v }} }}\n{cfg_test}\nmod suite;\n"
        ),
    );
    let census = census_of(&fixture.root);
    assert_eq!(
        census["fixture-crate/src/lib.rs"].production_count(SuppressionForm::Allow),
        0,
        "a suppression inside a `#[cfg(test)]` item is test code"
    );
    assert!(
        ratchet_violations(&census, allows, SuppressionForm::Allow).is_empty()
            && ratchet_violations(&census, expects, SuppressionForm::Expect).is_empty(),
        "a falling count, a test suppression and a generated suppression are all silent"
    );
}

/// Both spellings of the attribute are counted, `expect` is told apart from
/// `allow`, `cfg_attr` is followed, and the clippy/rustc split follows the lint
/// name rather than the attribute position.
#[test]
fn the_scan_reads_both_spellings_and_follows_cfg_attr() {
    let hash = || String::from("#");
    let source = format!(
        "{h}![allow(dead_code)]\n\
{h}[allow(clippy::cast_precision_loss)]\n\
fn a(v: f64) -> f64 {{ v }}\n\
{h}[expect(clippy::cast_precision_loss)]\n\
fn b(v: f64) -> f64 {{ v }}\n\
{h}[cfg_attr(test, allow(dead_code))]\n\
fn c() {{}}\n\
{h}[cfg_attr(feature = \"x\", allow(dead_code))]\n\
fn d() {{}}\n",
        h = hash()
    );
    let suppressions = suppressions_in(Path::new("scan.rs"), &source);
    assert_eq!(suppressions.len(), 5);
    assert_eq!(
        suppressions
            .iter()
            .filter(|suppression| suppression.tool == LintTool::Clippy)
            .count(),
        2
    );
    assert_eq!(
        suppressions
            .iter()
            .filter(|suppression| suppression.form == SuppressionForm::Expect)
            .count(),
        1
    );
    assert_eq!(
        suppressions
            .iter()
            .filter(|suppression| suppression.under_cfg_test)
            .count(),
        1,
        "only the `cfg_attr(test, ...)` suppression is test-conditional"
    );
    assert!(
        suppressions_in(Path::new("control.rs"), "fn nothing() {}\n").is_empty(),
        "a file with no attribute must contribute nothing"
    );
}

fn is_pinned(path: &str) -> bool {
    [PRODUCTION_ALLOW_BASELINE, PRODUCTION_EXPECT_BASELINE]
        .iter()
        .flat_map(|baseline| baseline.iter())
        .any(|(pinned, _)| *pinned == path)
}

/// Compares a census against a baseline table for one suppression form.
fn ratchet_violations(
    census: &BTreeMap<String, FileCensus>,
    baseline: &[(&str, usize)],
    form: SuppressionForm,
) -> Vec<String> {
    let pinned: BTreeMap<&str, usize> = baseline.iter().copied().collect();
    let mut violations = Vec::new();
    for (path, file) in census {
        let measured = file.production_count(form);
        if measured == 0 {
            continue;
        }
        let texts = file
            .production_suppressions(form)
            .into_iter()
            .map(|suppression| suppression.text.clone())
            .collect::<Vec<_>>()
            .join(", ");
        match pinned.get(path.as_str()) {
            Some(&allowed) if measured <= allowed => {}
            Some(&allowed) => violations.push(format!(
                "{path}: {measured} production `{}` suppressions, baseline {allowed} ({texts})",
                form.label()
            )),
            None => violations.push(format!(
                "{path}: {measured} production `{}` suppressions and the file is not pinned \
({texts})",
                form.label()
            )),
        }
    }
    violations
}

/// The measured production inventory in baseline-table shape, so a failure
/// report can be pasted back into the table once the change is reviewed.
fn measured_inventory(census: &BTreeMap<String, FileCensus>, form: SuppressionForm) -> Vec<String> {
    census
        .iter()
        .filter(|(_, file)| file.production_count(form) > 0)
        .map(|(path, file)| format!("    (\"{path}\", {}),", file.production_count(form)))
        .collect()
}

/// Takes the census of every `.rs` file under `crates_dir`, keyed by the path
/// relative to that directory with `/` separators.
fn census_of(crates_dir: &Path) -> BTreeMap<String, FileCensus> {
    let mut census = BTreeMap::new();
    let mut crate_dirs = fs::read_dir(crates_dir)
        .unwrap_or_else(|error| panic!("read {}: {error}", crates_dir.display()))
        .filter_map(Result::ok)
        .map(|entry| entry.path())
        .filter(|path| path.is_dir())
        .collect::<Vec<_>>();
    crate_dirs.sort();
    for crate_dir in crate_dirs {
        let shipped: BTreeSet<PathBuf> = production_source_files_under(&crate_dir.join("src"))
            .into_iter()
            .collect();
        let mut files = Vec::new();
        collect_rs_files(&crate_dir, &mut files);
        for path in files {
            let source = fs::read_to_string(&path)
                .unwrap_or_else(|error| panic!("read {}: {error}", path.display()));
            census.insert(
                relative_key(crates_dir, &path),
                FileCensus {
                    kind: classify(&path, &shipped),
                    suppressions: suppressions_in(&path, &source),
                },
            );
        }
    }
    census
}

fn relative_key(root: &Path, path: &Path) -> String {
    path.strip_prefix(root)
        .unwrap_or(path)
        .to_string_lossy()
        .replace('\\', "/")
}

/// Classifies one file. `shipped` is the set of files under the crate's `src`
/// directory that a release build compiles, computed by the same module-tree
/// walk the totality-debt gate uses, so "what does the shipped code say" keeps
/// one answer across the hardening gates.
fn classify(path: &Path, shipped: &BTreeSet<PathBuf>) -> SourceKind {
    if path
        .components()
        .any(|component| component.as_os_str() == "generated")
    {
        return SourceKind::Generated;
    }
    if shipped.contains(path) {
        return SourceKind::Production;
    }
    SourceKind::Test
}

/// Every `allow` and `expect` attribute in `source`, with its lint namespace
/// and whether it applies only under `cfg(test)`.
fn suppressions_in(path: &Path, source: &str) -> Vec<Suppression> {
    let syntax = syn::parse_file(source)
        .unwrap_or_else(|error| panic!("parse {} for suppressions: {error}", path.display()));
    let mut visitor = SuppressionVisitor {
        under_cfg_test: false,
        suppressions: Vec::new(),
    };
    visitor.visit_file(&syntax);
    visitor.suppressions
}

struct SuppressionVisitor {
    under_cfg_test: bool,
    suppressions: Vec<Suppression>,
}

impl SuppressionVisitor {
    fn enter<T>(&mut self, attrs: &[syn::Attribute], visit: impl FnOnce(&mut Self) -> T) -> T {
        let restore = self.under_cfg_test;
        if attrs.iter().any(attribute_is_cfg_test) {
            self.under_cfg_test = true;
        }
        let result = visit(self);
        self.under_cfg_test = restore;
        result
    }

    fn record(&mut self, meta: &syn::Meta, conditional_on_test: bool) {
        let Some(form) = suppression_form(meta) else {
            return;
        };
        let lints = suppressed_lints(meta);
        if lints.is_empty() {
            return;
        }
        let tool = if lints.iter().any(|lint| lint.starts_with("clippy::")) {
            LintTool::Clippy
        } else {
            LintTool::Rustc
        };
        self.suppressions.push(Suppression {
            text: meta.to_token_stream().to_string().replace(' ', ""),
            form,
            tool,
            under_cfg_test: self.under_cfg_test || conditional_on_test,
        });
    }
}

impl Visit<'_> for SuppressionVisitor {
    fn visit_item(&mut self, item: &syn::Item) {
        let attrs = item_attributes(item).to_vec();
        self.enter(&attrs, |visitor| visit::visit_item(visitor, item));
    }

    fn visit_impl_item(&mut self, item: &syn::ImplItem) {
        let attrs = impl_item_attributes(item).to_vec();
        self.enter(&attrs, |visitor| visit::visit_impl_item(visitor, item));
    }

    fn visit_trait_item(&mut self, item: &syn::TraitItem) {
        let attrs = trait_item_attributes(item).to_vec();
        self.enter(&attrs, |visitor| visit::visit_trait_item(visitor, item));
    }

    fn visit_attribute(&mut self, attribute: &syn::Attribute) {
        if suppression_form(&attribute.meta).is_some() {
            self.record(&attribute.meta, false);
        } else if let syn::Meta::List(list) = &attribute.meta
            && list.path.is_ident("cfg_attr")
        {
            let arguments = parsed_arguments(list);
            let conditional_on_test = arguments.first().is_some_and(meta_mentions_test);
            for nested in arguments.iter().skip(1) {
                if suppression_form(nested).is_some() {
                    self.record(nested, conditional_on_test);
                }
            }
        }
        visit::visit_attribute(self, attribute);
    }
}

/// The suppression form an attribute spells, if it is one.
fn suppression_form(meta: &syn::Meta) -> Option<SuppressionForm> {
    let syn::Meta::List(list) = meta else {
        return None;
    };
    if list.path.is_ident("allow") {
        return Some(SuppressionForm::Allow);
    }
    list.path
        .is_ident("expect")
        .then_some(SuppressionForm::Expect)
}

/// Lint names a suppression attribute silences. A `reason = "..."` argument is
/// not a lint name and does not count.
fn suppressed_lints(meta: &syn::Meta) -> Vec<String> {
    let syn::Meta::List(list) = meta else {
        return Vec::new();
    };
    parsed_arguments(list)
        .iter()
        .filter_map(|argument| match argument {
            syn::Meta::Path(path) => Some(path.to_token_stream().to_string().replace(' ', "")),
            _ => None,
        })
        .collect()
}

fn parsed_arguments(list: &syn::MetaList) -> Vec<syn::Meta> {
    Punctuated::<syn::Meta, syn::Token![,]>::parse_terminated
        .parse2(list.tokens.clone())
        .map(|parsed| parsed.into_iter().collect())
        .unwrap_or_default()
}

fn attribute_is_cfg_test(attribute: &syn::Attribute) -> bool {
    let syn::Meta::List(list) = &attribute.meta else {
        return false;
    };
    list.path.is_ident("cfg") && parsed_arguments(list).iter().any(meta_mentions_test)
}

/// Whether a `cfg` predicate names the `test` configuration, following the
/// `any`/`all`/`not` combinators.
fn meta_mentions_test(meta: &syn::Meta) -> bool {
    match meta {
        syn::Meta::Path(path) => path.is_ident("test"),
        syn::Meta::List(list) => parsed_arguments(list).iter().any(meta_mentions_test),
        syn::Meta::NameValue(_) => false,
    }
}

fn item_attributes(item: &syn::Item) -> &[syn::Attribute] {
    match item {
        syn::Item::Const(item) => &item.attrs,
        syn::Item::Enum(item) => &item.attrs,
        syn::Item::ExternCrate(item) => &item.attrs,
        syn::Item::Fn(item) => &item.attrs,
        syn::Item::ForeignMod(item) => &item.attrs,
        syn::Item::Impl(item) => &item.attrs,
        syn::Item::Macro(item) => &item.attrs,
        syn::Item::Mod(item) => &item.attrs,
        syn::Item::Static(item) => &item.attrs,
        syn::Item::Struct(item) => &item.attrs,
        syn::Item::Trait(item) => &item.attrs,
        syn::Item::TraitAlias(item) => &item.attrs,
        syn::Item::Type(item) => &item.attrs,
        syn::Item::Union(item) => &item.attrs,
        syn::Item::Use(item) => &item.attrs,
        _ => &[],
    }
}

fn impl_item_attributes(item: &syn::ImplItem) -> &[syn::Attribute] {
    match item {
        syn::ImplItem::Const(item) => &item.attrs,
        syn::ImplItem::Fn(item) => &item.attrs,
        syn::ImplItem::Type(item) => &item.attrs,
        syn::ImplItem::Macro(item) => &item.attrs,
        _ => &[],
    }
}

fn trait_item_attributes(item: &syn::TraitItem) -> &[syn::Attribute] {
    match item {
        syn::TraitItem::Const(item) => &item.attrs,
        syn::TraitItem::Fn(item) => &item.attrs,
        syn::TraitItem::Type(item) => &item.attrs,
        syn::TraitItem::Macro(item) => &item.attrs,
        _ => &[],
    }
}

/// A scratch source tree, removed when the test ends.
struct Fixture {
    root: PathBuf,
}

impl Fixture {
    fn new(label: &str) -> Self {
        let root =
            std::env::temp_dir().join(format!("rumoca-suppression-{label}-{}", std::process::id()));
        let _stale_fixture_removal_error = fs::remove_dir_all(&root);
        fs::create_dir_all(&root).expect("create fixture root");
        Self { root }
    }

    fn write(&self, relative: &str, content: &str) {
        let path = self.root.join(relative);
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).expect("create fixture directory");
        }
        fs::write(&path, content).expect("write fixture file");
    }
}

impl Drop for Fixture {
    fn drop(&mut self) {
        let _fixture_removal_error = fs::remove_dir_all(&self.root);
    }
}
