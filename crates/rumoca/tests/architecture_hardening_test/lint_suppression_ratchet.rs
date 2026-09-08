//! Ratchet on lint suppression attributes in hand-written production source.
//!
//! A suppression attribute answers a lint by silencing it rather than by
//! changing the design the lint objected to, so the set of them may only
//! shrink. The gate takes a census of every `allow` and `expect` attribute
//! under `crates/`, separates the populations that carry different
//! obligations, and pins the hand-written production population file by file.
//!
//! The source populations are:
//!
//! * generated source: a file a build script declares that its generator
//!   emits. Nothing else. Not the directory the file sits in, and not a banner
//!   the file carries, because a directory name and a comment are both
//!   spellings a person can set, and **a file cannot certify its own
//!   provenance**. SPEC_0021 scopes the carve-out to "explicit generator
//!   provenance" and says in terms that "a path containing `generated/` is not
//!   provenance". The build script is the thing that actually runs the
//!   generator, so its `parser_output_file` / `actions_output_file` calls are
//!   the declaration, read per crate from that crate's `build.rs`. The same
//!   declarations are pinned verbatim by the `parser_contract` gate, so a
//!   rename fails there and moves the file out of the carve-out here.
//!
//!   A consequence worth stating: the two `generated/mod.rs` wrappers are
//!   hand-written module declarations that nothing emits, so they are
//!   production source and their suppressions are pinned like any other. Only
//!   the four declared parser and grammar-trait files are carved out.
//! * test source: a file outside a crate's `src` directory, a file reachable
//!   only from a `#[cfg(test)] mod` declaration, and an attribute written
//!   inside a `#[cfg(test)]` item or behind `cfg_attr(test, ...)`. None of it
//!   reaches a release build.
//! * everything else, which is hand-written production source and is pinned.
//!
//! Production source is pinned in four tables, one per suppression form and
//! lint namespace, because the four carry different obligations and must not
//! be pooled:
//!
//! * clippy `allow` is design debt SPEC_0021 puts at a zero target baseline.
//! * rustc `allow` is where the soundness boundary is declared, plus the
//!   `dead_code` waivers on the hand-written parser wrappers. Pooling it with
//!   clippy debt would let a new unsafe boundary consume headroom meant for a
//!   complexity suppression, and would let the clippy number read lower than
//!   the debt actually is.
//! * clippy `expect` is SPEC_0021's one bounded exhaustive-dispatch exception.
//! * rustc `expect` is an upstream-API obligation: a deprecated wire field or
//!   a macro-expansion pattern that a dependency bump removes.
//!
//! `allow` and `expect` are pinned separately because `expect` is the better
//! of the two, since it fails when the lint it names stops firing, but it is
//! still a suppression: keeping the counts apart is what stops a rewrite of
//! `allow` into `expect` from reading as a reduction.
//!
//! Each pin states an exact count, not a ceiling. A ceiling is headroom, and
//! headroom is how a table goes stale: source work removes a suppression, the
//! pin keeps the old number, and the difference silently licenses re-adding
//! one. `every_production_pin_equals_its_measured_count` holds the tables to
//! equality against the same census the ratchet reads, so a real reduction
//! turns its own pin red and has to be recorded.
//!
//! This ratchet is not the gate SPEC_0021 ultimately mandates. SPEC_0021 sets
//! an absolute zero baseline for hand-written `allow(clippy::...)`, and these
//! tables record a non-zero debt that is being burned down in serial owner
//! lanes. The tables are the truthful current debt and an upper bound on it;
//! reaching zero, and replacing these tables with the absolute gate, is
//! outstanding work that this gate does not perform.
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

/// The generator builder calls that declare an emitted file.
///
/// These are the parol API, and the same call sites are pinned verbatim by the
/// `parser_contract` gate. This gate consumes the declaration; that gate pins
/// its text. One authority, two independent readings of it.
const GENERATOR_OUTPUT_DECLARATIONS: &[&str] = &["parser_output_file", "actions_output_file"];

/// The build script the census reads a crate's generator declarations from.
const BUILD_SCRIPT: &str = "build.rs";

/// How many generator output declarations the workspace carries. A count, not
/// a list: it makes a build script that stops declaring a file visible without
/// restating which files are generated, which is the declaration's job.
const WORKSPACE_GENERATOR_DECLARATIONS: usize = 4;

/// Hand-written production `allow(clippy::...)` counts, keyed by the path of
/// the file relative to `crates/`.
///
/// SPEC_0021 puts this population at a zero target baseline. Exceeding an
/// entry, introducing a file that has none, or falling below an entry without
/// editing the table all fail the gate: the pin is an equality, and ratcheting
/// down means editing this table in the same change that removes the
/// suppression.
const PRODUCTION_CLIPPY_ALLOW_BASELINE: &[(&str, usize)] = &[
    ("rumoca-eval-solve/src/compute_block_scalarize/dense.rs", 1),
    ("rumoca-eval-solve/src/lib.rs", 2),
    ("rumoca-eval-solve/src/typed_program/mod.rs", 1),
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
    ("rumoca-phase-parse-galec/src/parse/generated/mod.rs", 5),
    ("rumoca-phase-parse/src/generated/mod.rs", 5),
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
];

/// Hand-written production rustc `allow` counts.
///
/// Two obligations share this table because both are rustc lints, and both are
/// held apart from clippy design debt. The `unsafe_code` rows are deliberate
/// soundness boundaries: the MLIR C API surface and the three raw-pointer
/// entry points of the scheduled-simulation executor. The parser-wrapper rows
/// are `dead_code` waivers on hand-written module declarations. Neither is a
/// complexity suppression, and neither may share a budget with one.
const PRODUCTION_RUSTC_ALLOW_BASELINE: &[(&str, usize)] = &[
    ("rumoca-exec-mlir/src/lib.rs", 1),
    ("rumoca-phase-parse-galec/src/parse/generated/mod.rs", 2),
    ("rumoca-phase-parse/src/generated/mod.rs", 2),
    ("rumoca-sim/src/scheduled_sim/executor.rs", 3),
];

/// Hand-written production `expect(clippy::...)` counts, pinned in the same
/// shape and for the same reason. Rewriting an `allow` into an `expect` moves a
/// row from the clippy allow table into this one; because both are pinned by
/// equality, the move is visible instead of reading as a reduction.
const PRODUCTION_CLIPPY_EXPECT_BASELINE: &[(&str, usize)] = &[
    ("rumoca-exec-cranelift/src/emit.rs", 3),
    ("rumoca-exec-cranelift/src/emit/interpreter.rs", 1),
    ("rumoca-phase-solve/src/ad.rs", 1),
    (
        "rumoca-phase-structural/src/dae_transform/expressions.rs",
        1,
    ),
    ("rumoca-reference/src/value.rs", 1),
];

/// Hand-written production rustc `expect` counts. These name an obligation
/// owned by a dependency rather than by this workspace: a deprecated LSP wire
/// field and a macro expansion pattern. A dependency bump, not a redesign, is
/// what removes them, so they are tracked apart from the clippy exception.
const PRODUCTION_RUSTC_EXPECT_BASELINE: &[(&str, usize)] = &[
    ("rumoca-bind-python/src/lib.rs", 1),
    ("rumoca-tool-lsp/src/handlers/document_symbols.rs", 1),
    ("rumoca-tool-lsp/src/handlers/workspace_symbols.rs", 1),
];

const GUIDANCE: &str = "\
A suppression attribute answers a lint by silencing it instead of by changing \
the design the lint objected to. Decompose the function, introduce the typed \
parameter or config struct, box the oversized error, or convert the cast into \
a checked conversion on an existing error path. If none of those apply because \
the surrounding code cannot represent the failure, that is a design question \
to settle before the suppression lands, not after. A rustc suppression is a \
different obligation and lives in its own table: an `unsafe_code` allow \
declares a soundness boundary that needs an owner and a safety argument, not a \
refactor.";

/// Where a source file sits relative to the release build.
#[derive(Clone, PartialEq, Eq, Debug)]
enum SourceKind {
    /// Emitted by a generator. The variant carries the build-script
    /// declaration that classified the file, so "this file is generated"
    /// cannot be asserted without the evidence that made it so.
    Generated { declaration: String },
    /// Absent from a release build: outside `src`, or `#[cfg(test)]`-only.
    Test,
    /// Hand-written and compiled into a release build.
    Production,
}

/// Which spelling of the suppression the author used.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
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
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum LintTool {
    Clippy,
    Rustc,
}

impl LintTool {
    fn label(self) -> &'static str {
        match self {
            LintTool::Clippy => "clippy",
            LintTool::Rustc => "rustc",
        }
    }
}

/// One pinned population: a suppression form, a lint namespace, and the table
/// that pins it. The production tables and every fixture table use this one
/// shape, so a fixture and the workspace are compared by the same code.
#[derive(Clone, Copy)]
struct Population<'a> {
    form: SuppressionForm,
    tool: LintTool,
    baseline: &'a [(&'a str, usize)],
}

const PINNED_POPULATIONS: &[Population<'static>] = &[
    Population {
        form: SuppressionForm::Allow,
        tool: LintTool::Clippy,
        baseline: PRODUCTION_CLIPPY_ALLOW_BASELINE,
    },
    Population {
        form: SuppressionForm::Allow,
        tool: LintTool::Rustc,
        baseline: PRODUCTION_RUSTC_ALLOW_BASELINE,
    },
    Population {
        form: SuppressionForm::Expect,
        tool: LintTool::Clippy,
        baseline: PRODUCTION_CLIPPY_EXPECT_BASELINE,
    },
    Population {
        form: SuppressionForm::Expect,
        tool: LintTool::Rustc,
        baseline: PRODUCTION_RUSTC_EXPECT_BASELINE,
    },
];

/// How a population is named in a report, defined once so every message reads
/// the same way.
fn population_label(form: SuppressionForm, tool: LintTool) -> String {
    format!("`{}` `{}`", tool.label(), form.label())
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
    /// Suppressions of one form and one lint namespace that reach a release
    /// build in hand-written source.
    fn production_suppressions(&self, form: SuppressionForm, tool: LintTool) -> Vec<&Suppression> {
        if self.kind != SourceKind::Production {
            return Vec::new();
        }
        self.suppressions
            .iter()
            .filter(|suppression| {
                suppression.form == form && suppression.tool == tool && !suppression.under_cfg_test
            })
            .collect()
    }

    fn production_count(&self, form: SuppressionForm, tool: LintTool) -> usize {
        self.production_suppressions(form, tool).len()
    }
}

#[test]
fn production_lint_allows_do_not_increase() {
    assert_ratchet_holds(SuppressionForm::Allow);
}

#[test]
fn production_lint_expectations_do_not_increase() {
    assert_ratchet_holds(SuppressionForm::Expect);
}

fn assert_ratchet_holds(form: SuppressionForm) {
    let census = census_of(&workspace_root().join("crates"));
    let selected: Vec<Population<'_>> = PINNED_POPULATIONS
        .iter()
        .copied()
        .filter(|population| population.form == form)
        .collect();
    let mut inventory = Vec::new();
    for population in &selected {
        inventory.push(format!(
            "    // {}",
            population_label(population.form, population.tool)
        ));
        inventory.extend(measured_inventory(
            &census,
            population.form,
            population.tool,
        ));
    }
    let violations = violations_across(&selected, &census);
    assert!(
        violations.is_empty(),
        "hand-written production `{}` suppressions rose above their pinned baseline:\n{}\n\n\
{GUIDANCE}\n\nMeasured production `{}` inventory:\n{}",
        form.label(),
        violations.join("\n"),
        form.label(),
        inventory.join("\n")
    );
}

/// Every pin states the exact count the census measures, in both directions.
///
/// The ratchet above accepts a measurement below its pin, and must: it is also
/// driven by fixtures that deliberately fall, and tightening it there would
/// destroy the property that a burn-down is silent to the ratchet. But a pin
/// left above its measurement is headroom, and headroom regenerates debt: the
/// difference between the stale number and the real one silently licenses
/// re-adding a suppression that source work already removed. This gate closes
/// that by holding the production tables, and only the production tables, to
/// equality against the same census.
#[test]
fn every_production_pin_equals_its_measured_count() {
    let census = census_of(&workspace_root().join("crates"));
    let mut mismatches = Vec::new();
    for population in PINNED_POPULATIONS {
        mismatches.extend(census_mismatches(
            &census,
            population.baseline,
            population.form,
            population.tool,
        ));
    }
    assert!(
        mismatches.is_empty(),
        "a pinned count no longer equals the measured count. A pin is an exact statement of the \
current debt, not a ceiling: a pin above the measurement is headroom that licenses re-adding a \
suppression the source no longer carries, and a pin below it is debt the gate is not enforcing. \
Edit the table in the same change that moves the source:\n{}\n\n{GUIDANCE}",
        mismatches.join("\n")
    );
}

/// A baseline entry that no measured file can reach is unreadable as a pin, so
/// every table is kept sorted, unique, and non-zero.
#[test]
fn the_baseline_tables_are_readable_pins() {
    for population in PINNED_POPULATIONS {
        let mut seen = BTreeSet::new();
        let mut previous: Option<&str> = None;
        for (path, count) in population.baseline {
            assert!(
                seen.insert(*path),
                "{path} is pinned twice in the {} table; one file has one count",
                population_label(population.form, population.tool)
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

/// The generated carve-out is real only if declared generated source actually
/// carries suppressions the gate is declining to pin, and only if every
/// declaration resolves to exactly one file in its own crate. A declaration
/// that matches nothing is a dead carve-out; one that matches two files is an
/// ambiguous one; either way the gate says so rather than guessing.
#[test]
fn the_generated_parser_carve_out_is_exercised() {
    let crates_dir = workspace_root().join("crates");
    let census = census_of(&crates_dir);
    let generated: Vec<(&String, &str)> = census
        .iter()
        .filter_map(|(path, file)| match &file.kind {
            SourceKind::Generated { declaration } if !file.suppressions.is_empty() => {
                Some((path, declaration.as_str()))
            }
            _ => None,
        })
        .collect();
    assert!(
        !generated.is_empty(),
        "no declared generator output carries a suppression; the carve-out is either dead or the \
build scripts stopped declaring the files the generator emits"
    );
    for (path, declaration) in &generated {
        assert!(
            !is_pinned(path),
            "{path} is carved out by the `{declaration}` build-script declaration and must not \
also be pinned as hand-written production source"
        );
    }

    let mut declarations = 0usize;
    for crate_dir in crate_directories(&crates_dir) {
        let prefix = format!("{}/", relative_key(&crates_dir, &crate_dir));
        for declaration in declared_generator_outputs(&crate_dir) {
            declarations += 1;
            let name = declaration.as_str();
            let matched: Vec<&String> = census
                .keys()
                .filter(|path| path.starts_with(&prefix) && file_name_of(path) == Some(name))
                .collect();
            assert_eq!(
                matched.len(),
                1,
                "the `{declaration}` declaration in {}/{BUILD_SCRIPT} must resolve to exactly one \
file in that crate: {matched:#?}",
                relative_key(&crates_dir, &crate_dir)
            );
        }
    }
    assert_eq!(
        declarations, WORKSPACE_GENERATOR_DECLARATIONS,
        "the workspace build scripts declare a different number of generated files than the gate \
expects; a build script that stopped declaring a file must be recorded here, because that file \
becomes hand-written production source and its suppressions become pinned debt"
    );
}

/// Every pinned file must be classified as production and must exist. A pin
/// that names a file the census never reaches enforces nothing.
#[test]
fn every_pinned_file_is_measured_production_source() {
    let census = census_of(&workspace_root().join("crates"));
    let mut unreachable = Vec::new();
    for population in PINNED_POPULATIONS {
        for (path, _) in population.baseline {
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
goes away, and never pin declared generator output or test source: {unreachable:#?}"
    );
}

/// The census must reach every population, or a classification bug that folds
/// one population into another passes unnoticed. Both lint namespaces must
/// also be represented, so that a scan which silently stopped recognising one
/// of them cannot look like a burn-down.
#[test]
fn the_census_separates_every_population_it_classifies() {
    let census = census_of(&workspace_root().join("crates"));
    assert!(
        census
            .values()
            .any(|file| matches!(file.kind, SourceKind::Generated { .. })),
        "the census classified no file as declared generator output"
    );
    assert!(
        census.values().any(|file| file.kind == SourceKind::Test),
        "the census classified no file as test source"
    );
    assert!(
        census
            .values()
            .any(|file| file.kind == SourceKind::Production),
        "the census classified no file as production source"
    );
    for tool in [LintTool::Clippy, LintTool::Rustc] {
        assert!(
            census
                .values()
                .flat_map(|file| file.suppressions.iter())
                .any(|suppression| suppression.tool == tool),
            "the census found no {} suppression anywhere in the workspace",
            tool.label()
        );
    }
}

/// The tool split is operative rather than decorative: the pinned rustc allows
/// carry the soundness boundary, and none of it may be sitting in the clippy
/// table. Reading the workspace directly, rather than the tables, is what makes
/// this a check on the classifier instead of a check on the transcription.
#[test]
fn the_soundness_boundary_is_pinned_apart_from_clippy_debt() {
    let census = census_of(&workspace_root().join("crates"));
    let boundaries: Vec<(&String, &Suppression)> = census
        .iter()
        .filter(|(_, file)| file.kind == SourceKind::Production)
        .flat_map(|(path, file)| {
            file.suppressions
                .iter()
                .map(move |suppression| (path, suppression))
        })
        .filter(|(_, suppression)| {
            !suppression.under_cfg_test && suppression.text.contains("unsafe_code")
        })
        .collect();
    assert!(
        !boundaries.is_empty(),
        "no production `unsafe_code` suppression was found; either the soundness boundary moved \
and its table row is stale, or the census stopped reading rustc lint names"
    );
    for (path, suppression) in &boundaries {
        assert_eq!(
            suppression.tool,
            LintTool::Rustc,
            "{path} declares a soundness boundary that the census read as clippy debt: {}",
            suppression.text
        );
    }
    for (path, _) in PRODUCTION_CLIPPY_ALLOW_BASELINE {
        assert!(
            !boundaries
                .iter()
                .any(|(boundary, _)| boundary.as_str() == *path),
            "{path} carries an `unsafe_code` boundary and must be pinned in the rustc table, not \
the clippy one"
        );
    }
}

/// The pinned production tables themselves are mutation-tested against the
/// real census. The planted-fixture test below proves the comparison logic on a
/// tree with known contents; this one perturbs the tables that actually gate
/// the workspace, so a table that has stopped being connected to the source it
/// names cannot pass.
#[test]
fn the_production_tables_are_mutation_tested() {
    let census = census_of(&workspace_root().join("crates"));
    for population in PINNED_POPULATIONS {
        for (index, (path, count)) in population.baseline.iter().copied().enumerate() {
            let named = format!("{path}:");

            // Over-pinning by one: the ratchet accepts it, by design, and the
            // census invariant is the thing that must not.
            let mut over_pinned = population.baseline.to_vec();
            over_pinned[index] = (path, count + 1);
            let mismatches =
                census_mismatches(&census, &over_pinned, population.form, population.tool);
            assert!(
                mismatches
                    .iter()
                    .any(|mismatch| mismatch.starts_with(&named)),
                "pinning {path} one above its measurement must be reported as stale headroom: \
{mismatches:#?}"
            );
            assert!(
                ratchet_violations(&census, &over_pinned, population.form, population.tool)
                    .is_empty(),
                "over-pinning is silent to the ratchet, which is the whole reason the census \
invariant exists; if this assertion fails the ratchet was tightened globally and the fixtures \
that deliberately fall below their pins are now broken"
            );

            // Deleting the row: the measured suppressions become unpinned.
            let mut without_row = population.baseline.to_vec();
            without_row.remove(index);
            let violations =
                ratchet_violations(&census, &without_row, population.form, population.tool);
            assert!(
                violations
                    .iter()
                    .any(|violation| violation.starts_with(&named)
                        && violation.contains("not pinned")),
                "deleting the {path} row must leave its measured suppressions unpinned: \
{violations:#?}"
            );
            let mismatches =
                census_mismatches(&census, &without_row, population.form, population.tool);
            assert!(
                mismatches
                    .iter()
                    .any(|mismatch| mismatch.starts_with(&named)),
                "deleting the {path} row must also break the census invariant: {mismatches:#?}"
            );
        }
    }
}

struct PlantedSuppressionFixture {
    fixture: Fixture,
    cast: String,
    nested: String,
    dead: String,
    expected_cast: String,
    cfg_test: String,
}

impl PlantedSuppressionFixture {
    fn build() -> Self {
        let fixture = Fixture::new("ratchet");
        // Assemble the attribute text so this file contains no literal
        // suppressions for its own census or construction-lint gate to read.
        let hash = || String::from("#");
        let planted = Self {
            fixture,
            cast: format!("{}[allow(clippy::cast_precision_loss)]", hash()),
            nested: format!("{}[allow(clippy::excessive_nesting)]", hash()),
            dead: format!("{}[allow(dead_code)]", hash()),
            expected_cast: format!("{}[expect(clippy::cast_precision_loss)]", hash()),
            cfg_test: format!("{}[cfg(test)]", hash()),
        };
        planted.fixture.write(
            "fixture-crate/build.rs",
            &generator_build_script("src/emitted/generated", "grammar.rs"),
        );
        planted
            .fixture
            .write("fixture-crate/src/lib.rs", &planted.pinned_lib(""));
        planted
            .fixture
            .write("fixture-crate/src/model.rs", "fn clean() {}\n");
        planted
            .fixture
            .write("fixture-crate/src/emitted.rs", "pub mod generated;\n");
        planted.fixture.write(
            "fixture-crate/src/emitted/generated/grammar.rs",
            &planted.emitted_grammar(""),
        );
        planted.fixture.write(
            "fixture-crate/src/suite.rs",
            &format!(
                "{}\nfn only_under_test(v: f64) -> f64 {{ v }}\n",
                planted.cast
            ),
        );
        planted.fixture.write(
            "fixture-crate/tests/integration.rs",
            &format!("{}\nfn harness(v: f64) -> f64 {{ v }}\n", planted.cast),
        );
        planted
    }

    fn pinned_lib(&self, extra: &str) -> String {
        format!(
            "mod model;\nmod emitted;\n{}\nmod suite;\n{}\nfn pinned(v: f64) -> f64 {{ v }}\n{extra}",
            self.cfg_test, self.cast
        )
    }

    /// Declared generator output for the fixture crate: the build script above
    /// names `grammar.rs`, which is what carves this file out.
    fn emitted_grammar(&self, extra: &str) -> String {
        format!("{}\nfn emitted(v: f64) -> f64 {{ v }}\n{extra}", self.dead)
    }

    /// The tree as built sits exactly on its baseline, and every population is
    /// classified the way the rest of the mutations assume.
    fn assert_the_unmutated_tree_is_classified_and_clean(&self, populations: &[Population<'_>]) {
        let census = census_of(&self.fixture.root);
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
            SourceKind::Generated {
                declaration: String::from("grammar.rs")
            }
        );
        assert!(
            violations_across(populations, &census).is_empty(),
            "the unmutated fixture must sit exactly on its baseline"
        );
    }

    /// A clippy suppression added to a pinned file exceeds its pin.
    fn assert_a_planted_clippy_allow_is_caught(&self, populations: &[Population<'_>]) {
        self.fixture.write(
            "fixture-crate/src/lib.rs",
            &self.pinned_lib(&format!(
                "{}\nfn planted(v: f64) -> f64 {{ v }}\n",
                self.nested
            )),
        );
        let violations = violations_across(populations, &census_of(&self.fixture.root));
        assert_eq!(
            violations.len(),
            1,
            "one planted suppression must produce one violation: {violations:#?}"
        );
        assert!(
            violations[0].contains("fixture-crate/src/lib.rs")
                && violations[0].contains("baseline 1"),
            "the violation must name the file and its pin: {}",
            violations[0]
        );
        self.fixture
            .write("fixture-crate/src/lib.rs", &self.pinned_lib(""));
    }

    /// A rustc suppression added to a file pinned in the clippy table must not
    /// be absorbed by the clippy pin. The two populations carry different
    /// obligations, so a soundness boundary cannot be paid for out of a
    /// complexity budget, and vice versa.
    fn assert_a_planted_rustc_allow_keeps_its_own_table(&self, populations: &[Population<'_>]) {
        self.fixture.write(
            "fixture-crate/src/lib.rs",
            &self.pinned_lib(&format!("{}\nfn boundary() {{}}\n", self.dead)),
        );
        let census = census_of(&self.fixture.root);
        let clippy_allows = population_of(populations, SuppressionForm::Allow, LintTool::Clippy);
        let rustc_allows = population_of(populations, SuppressionForm::Allow, LintTool::Rustc);
        assert!(
            violations_across(&[clippy_allows], &census).is_empty(),
            "a rustc suppression must not consume the clippy pin"
        );
        let violations = violations_across(&[rustc_allows], &census);
        assert!(
            violations
                .iter()
                .any(|violation| violation.contains("fixture-crate/src/lib.rs")
                    && violation.contains("not pinned")),
            "a rustc suppression must surface in the rustc table: {violations:#?}"
        );
        self.fixture
            .write("fixture-crate/src/lib.rs", &self.pinned_lib(""));
    }

    /// A suppression added to a production file the baseline does not name is
    /// reported as a new file rather than silently accepted.
    fn assert_an_unpinned_production_file_is_reported(&self, populations: &[Population<'_>]) {
        self.fixture.write(
            "fixture-crate/src/model.rs",
            &format!("{}\nfn now_suppressed(v: f64) -> f64 {{ v }}\n", self.cast),
        );
        let violations = violations_across(populations, &census_of(&self.fixture.root));
        assert!(
            violations
                .iter()
                .any(|violation| violation.contains("fixture-crate/src/model.rs")
                    && violation.contains("not pinned")),
            "an unpinned production file that gains a suppression must be reported as new: \
{violations:#?}"
        );
        self.fixture
            .write("fixture-crate/src/model.rs", "fn clean() {}\n");
    }

    /// Rewriting the pinned `allow` as an `expect` empties the allow count but
    /// must be caught by the expect table rather than reading as a burn-down.
    fn assert_an_allow_rewritten_as_an_expect_is_caught(&self, populations: &[Population<'_>]) {
        self.fixture.write(
            "fixture-crate/src/lib.rs",
            &format!(
                "mod model;\nmod emitted;\n{}\nmod suite;\n{}\nfn pinned(v: f64) -> f64 {{ v }}\n",
                self.cfg_test, self.expected_cast
            ),
        );
        let census = census_of(&self.fixture.root);
        assert_eq!(
            census["fixture-crate/src/lib.rs"]
                .production_count(SuppressionForm::Allow, LintTool::Clippy),
            0
        );
        let clippy_allows = population_of(populations, SuppressionForm::Allow, LintTool::Clippy);
        assert!(
            violations_across(&[clippy_allows], &census).is_empty(),
            "a falling allow count is silent on its own"
        );
        let violations = violations_across(populations, &census);
        assert!(
            violations
                .iter()
                .any(|violation| violation.contains("fixture-crate/src/lib.rs")),
            "an allow rewritten as an expect must surface in the expect table: {violations:#?}"
        );
    }

    /// Test source, declared generator output, and a suppression inside a
    /// `#[cfg(test)]` item of a production file may all change without growing
    /// production debt.
    fn assert_test_and_generated_mutations_are_silent(&self, populations: &[Population<'_>]) {
        self.fixture.write(
            "fixture-crate/src/suite.rs",
            &format!(
                "{}\n{}\nfn only_under_test(v: f64) -> f64 {{ v }}\n",
                self.cast, self.dead
            ),
        );
        self.fixture.write(
            "fixture-crate/src/emitted/generated/grammar.rs",
            &self.emitted_grammar(&format!("{}\nfn more_emitted() {{}}\n", self.cast)),
        );
        self.fixture.write(
            "fixture-crate/src/lib.rs",
            &format!(
                "mod model;\nmod emitted;\n{}\nmod inline {{ {}\nfn t(v: f64) -> f64 {{ v }} }}\n{}\nmod suite;\n",
                self.cfg_test, self.cast, self.cfg_test
            ),
        );
        let census = census_of(&self.fixture.root);
        assert_eq!(
            census["fixture-crate/src/lib.rs"]
                .production_count(SuppressionForm::Allow, LintTool::Clippy),
            0,
            "a suppression inside a `#[cfg(test)]` item is test code"
        );
        assert!(
            violations_across(populations, &census).is_empty(),
            "a falling count, a test suppression and a declared generator output are all silent"
        );
    }
}

/// A planted mutation, run against a fixture tree with known contents: the
/// gate must catch a suppression added to a pinned production file and a
/// suppression added to a production file the baseline does not name, must
/// route a rustc suppression to the rustc table rather than into clippy
/// headroom, must stay silent for the generated and test populations and for a
/// count that falls, and must not accept an `expect` in place of a pinned
/// `allow`.
#[test]
fn the_ratchet_catches_a_planted_suppression() {
    let planted = PlantedSuppressionFixture::build();
    let pinned_lib: &[(&str, usize)] = &[("fixture-crate/src/lib.rs", 1)];
    let populations: &[Population<'_>] = &[
        Population {
            form: SuppressionForm::Allow,
            tool: LintTool::Clippy,
            baseline: pinned_lib,
        },
        Population {
            form: SuppressionForm::Allow,
            tool: LintTool::Rustc,
            baseline: &[],
        },
        Population {
            form: SuppressionForm::Expect,
            tool: LintTool::Clippy,
            baseline: &[],
        },
        Population {
            form: SuppressionForm::Expect,
            tool: LintTool::Rustc,
            baseline: &[],
        },
    ];

    planted.assert_the_unmutated_tree_is_classified_and_clean(populations);
    planted.assert_a_planted_clippy_allow_is_caught(populations);
    planted.assert_a_planted_rustc_allow_keeps_its_own_table(populations);
    planted.assert_an_unpinned_production_file_is_reported(populations);
    planted.assert_an_allow_rewritten_as_an_expect_is_caught(populations);
    planted.assert_test_and_generated_mutations_are_silent(populations);
}

/// Only a build-script declaration carves a file out.
///
/// Three mutants share one fixture tree, because they are three ways of
/// claiming provenance a file is not entitled to claim:
///
/// * a hand-written module wrapper sitting in a directory named `generated`;
/// * a hand-written file carrying a forged generator banner in its header;
/// * the declared output itself, after the build script stops declaring it.
///
/// The first two are spellings a person sets, and the gate must ignore both.
/// The third is the one that proves the authority is the live declaration and
/// not a cached name: the file's bytes do not change, only the build script
/// does, and the classification has to follow the build script.
#[test]
fn only_a_build_script_declaration_carves_a_file_out() {
    let fixture = Fixture::new("provenance");
    let hash = || String::from("#");
    let cast = format!("{}[allow(clippy::cast_precision_loss)]", hash());
    let forged_banner = "// This file was generated by parol.";
    let clippy_allows: &[Population<'_>] = &[Population {
        form: SuppressionForm::Allow,
        tool: LintTool::Clippy,
        baseline: &[],
    }];

    fixture.write(
        "fixture-crate/build.rs",
        &generator_build_script("src/generated", "fixture_parser.rs"),
    );
    fixture.write("fixture-crate/src/lib.rs", "mod generated;\n");
    fixture.write(
        "fixture-crate/src/generated/fixture_parser.rs",
        &format!("{cast}\nfn emitted(v: f64) -> f64 {{ v }}\n"),
    );
    fixture.write(
        "fixture-crate/src/generated/mod.rs",
        &format!("{cast}\npub mod fixture_parser;\n"),
    );
    fixture.write(
        "fixture-crate/src/forged.rs",
        &format!("{forged_banner}\n{cast}\nfn forged(v: f64) -> f64 {{ v }}\n"),
    );

    let census = census_of(&fixture.root);
    assert_eq!(
        census["fixture-crate/src/generated/fixture_parser.rs"].kind,
        SourceKind::Generated {
            declaration: String::from("fixture_parser.rs")
        },
        "the file the build script declares it emits is the carve-out"
    );
    for hand_written in [
        "fixture-crate/src/generated/mod.rs",
        "fixture-crate/src/forged.rs",
    ] {
        assert_eq!(
            census[hand_written].kind,
            SourceKind::Production,
            "{hand_written} is hand-written: neither a `generated` directory nor a forged \
generator banner is provenance, because a file cannot certify its own provenance"
        );
    }
    let violations = violations_across(clippy_allows, &census);
    for hand_written in [
        "fixture-crate/src/generated/mod.rs:",
        "fixture-crate/src/forged.rs:",
    ] {
        assert!(
            violations
                .iter()
                .any(|violation| violation.starts_with(hand_written)
                    && violation.contains("not pinned")),
            "{hand_written} must be gated as production source: {violations:#?}"
        );
    }
    assert!(
        !violations
            .iter()
            .any(|violation| violation.contains("fixture_parser.rs")),
        "the declared generator output must stay carved out: {violations:#?}"
    );

    // The declaration is the authority, not a cached name: the emitted file is
    // untouched and only the build script changes.
    fixture.write(
        "fixture-crate/build.rs",
        &generator_build_script("src/generated", "renamed_parser.rs"),
    );
    let census = census_of(&fixture.root);
    assert_eq!(
        census["fixture-crate/src/generated/fixture_parser.rs"].kind,
        SourceKind::Production,
        "a file the build script no longer declares is no longer generated source"
    );
    let violations = violations_across(clippy_allows, &census);
    assert!(
        violations.iter().any(|violation| violation
            .starts_with("fixture-crate/src/generated/fixture_parser.rs:")
            && violation.contains("not pinned")),
        "when a declaration is removed or renamed, the file it used to cover becomes pinned \
production debt: {violations:#?}"
    );
}

/// A generator declaration the census cannot read must stop the run rather
/// than read as an absent declaration, which would silently turn emitted files
/// into unpinned production debt or, worse, leave a carve-out unaccounted for.
#[test]
fn an_unreadable_generator_declaration_stops_the_census() {
    let declaration = fixture_output_declaration();
    let readable = format!("fn main() {{ builder.{declaration}(\"emitted.rs\"); }}\n");
    assert_eq!(
        declared_outputs_in(Path::new("readable.rs"), &readable),
        BTreeSet::from([String::from("emitted.rs")])
    );
    let unreadable = format!("fn main() {{ builder.{declaration}(&name); }}\n");
    let refused =
        std::panic::catch_unwind(|| declared_outputs_in(Path::new("unreadable.rs"), &unreadable));
    assert!(
        refused.is_err(),
        "a declaration whose argument is not a string literal must stop the census; reading it as \
`no declaration` would let the gate guess at provenance"
    );
}

/// Both spellings of the attribute are counted, `expect` is told apart from
/// `allow`, `cfg_attr` is followed, a `reason` argument does not remove the
/// attribute from the census, and the clippy/rustc split follows the lint name
/// rather than the attribute position.
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
fn d() {{}}\n\
{h}[allow(clippy::cast_precision_loss, reason = \"multi_argument_witness\")]\n\
fn e(v: f64) -> f64 {{ v }}\n",
        h = hash()
    );
    let suppressions = suppressions_in(Path::new("scan.rs"), &source);
    assert_eq!(suppressions.len(), 6);
    assert_eq!(
        suppressions
            .iter()
            .filter(|suppression| suppression.tool == LintTool::Clippy)
            .count(),
        3
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
        suppressions.iter().any(|suppression| suppression
            .text
            .contains("reason=\"multi_argument_witness\"")),
        "an attribute carrying a `reason` argument must still be counted; a scan that cannot read \
the argument list drops the suppression instead of reporting it"
    );
    assert!(
        suppressions_in(Path::new("control.rs"), "fn nothing() {}\n").is_empty(),
        "a file with no attribute must contribute nothing"
    );
}

/// An attribute whose argument list is not a comma-separated meta list must
/// stop the census, not read as zero lints.
///
/// Reading it as zero lints removes the suppression from the count, and a
/// ratchet that can undercount is not a ratchet. The witness observes the
/// refusal rather than the absence, because an assertion that the malformed
/// attribute is missing from the census would be satisfied by the defect
/// itself: that is exactly the vacuity this test has to avoid.
///
/// The malformed shape is constructed, not hypothesised: `syn` stores the
/// arguments of an attribute as an unparsed token stream, so `allow(1)` is a
/// file `syn::parse_file` accepts and the argument parser rejects. `rustc`
/// would reject the same text, so the shape cannot occur in first-party source
/// that compiles; the refusal is a defence of the scanner's own invariant
/// rather than the closing of a bypass observed in the workspace.
#[test]
fn a_malformed_suppression_attribute_stops_the_census() {
    let hash = || String::from("#");
    let source = format!(
        "{}[allow(1)]\nfn malformed(v: f64) -> f64 {{ v }}\n",
        hash()
    );
    let syntax = syn::parse_file(&source).expect("syn stores attribute arguments unparsed");
    let syn::Item::Fn(function) = &syntax.items[0] else {
        panic!("the witness source declares exactly one function");
    };
    let syn::Meta::List(list) = &function.attrs[0].meta else {
        panic!("the witness attribute is a meta list with unparsed arguments");
    };
    assert!(
        parsed_arguments(list).is_err(),
        "the shared argument predicate must report that it could not read the attribute"
    );
    let refused = std::panic::catch_unwind(|| suppressions_in(Path::new("malformed.rs"), &source));
    assert!(
        refused.is_err(),
        "the census must refuse an attribute it cannot read; answering `no lints` deletes the \
suppression from the count and moves the ratchet in the one direction it must never move"
    );
}

fn is_pinned(path: &str) -> bool {
    PINNED_POPULATIONS
        .iter()
        .flat_map(|population| population.baseline.iter())
        .any(|(pinned, _)| *pinned == path)
}

/// The one population in `populations` with this form and namespace.
fn population_of<'a>(
    populations: &[Population<'a>],
    form: SuppressionForm,
    tool: LintTool,
) -> Population<'a> {
    *populations
        .iter()
        .find(|population| population.form == form && population.tool == tool)
        .expect("the fixture must declare every population it asserts against")
}

/// Ratchet violations across a set of populations. Production and fixtures go
/// through this one comparison, so a fixture cannot drift from the gate.
fn violations_across(
    populations: &[Population<'_>],
    census: &BTreeMap<String, FileCensus>,
) -> Vec<String> {
    populations
        .iter()
        .flat_map(|population| {
            ratchet_violations(
                census,
                population.baseline,
                population.form,
                population.tool,
            )
        })
        .collect()
}

/// Compares a census against a baseline table for one form and lint namespace.
///
/// A measurement at or below its pin passes. That asymmetry is deliberate and
/// load-bearing: this function is also driven by fixtures that plant a falling
/// count and require silence, and the `measured == 0` skip below is what keeps
/// the clean unpinned production files out of the report. Exactness against
/// the production tables is a separate obligation, enforced by
/// `census_mismatches`.
fn ratchet_violations(
    census: &BTreeMap<String, FileCensus>,
    baseline: &[(&str, usize)],
    form: SuppressionForm,
    tool: LintTool,
) -> Vec<String> {
    let pinned: BTreeMap<&str, usize> = baseline.iter().copied().collect();
    let label = population_label(form, tool);
    let mut violations = Vec::new();
    for (path, file) in census {
        let measured = file.production_count(form, tool);
        if measured == 0 {
            continue;
        }
        let texts = file
            .production_suppressions(form, tool)
            .into_iter()
            .map(|suppression| suppression.text.clone())
            .collect::<Vec<_>>()
            .join(", ");
        match pinned.get(path.as_str()) {
            Some(&allowed) if measured <= allowed => {}
            Some(&allowed) => violations.push(format!(
                "{path}: {measured} production {label} suppressions, baseline {allowed} ({texts})"
            )),
            None => violations.push(format!(
                "{path}: {measured} production {label} suppressions and the file is not pinned \
({texts})"
            )),
        }
    }
    violations
}

/// Every difference between a baseline table and the census, in both
/// directions: a pin above its measurement, a pin below it, a pin whose file
/// carries nothing any more, and a measured file with no pin.
///
/// This is the exactness half of the gate. Unlike `ratchet_violations` it is
/// applied only to the production tables, so the fixtures that deliberately
/// fall below their pins keep their meaning.
fn census_mismatches(
    census: &BTreeMap<String, FileCensus>,
    baseline: &[(&str, usize)],
    form: SuppressionForm,
    tool: LintTool,
) -> Vec<String> {
    let pinned: BTreeMap<&str, usize> = baseline.iter().copied().collect();
    let measured: BTreeMap<&str, usize> = census
        .iter()
        .filter_map(|(path, file)| match file.production_count(form, tool) {
            0 => None,
            count => Some((path.as_str(), count)),
        })
        .collect();
    let label = population_label(form, tool);
    let mut mismatches = Vec::new();
    for (path, pin) in &pinned {
        match measured.get(path) {
            Some(count) if count == pin => {}
            Some(count) => mismatches.push(format!(
                "{path}: pinned {pin} production {label} suppressions, measured {count}"
            )),
            None => mismatches.push(format!(
                "{path}: pinned {pin} production {label} suppressions, measured none; delete the \
row so a reintroduced suppression is reported as a new file"
            )),
        }
    }
    for (path, count) in &measured {
        if !pinned.contains_key(path) {
            mismatches.push(format!(
                "{path}: {count} production {label} suppressions and the file is not pinned"
            ));
        }
    }
    mismatches
}

/// The measured production inventory in baseline-table shape, so a failure
/// report can be pasted back into the table once the change is reviewed.
fn measured_inventory(
    census: &BTreeMap<String, FileCensus>,
    form: SuppressionForm,
    tool: LintTool,
) -> Vec<String> {
    census
        .iter()
        .filter_map(|(path, file)| match file.production_count(form, tool) {
            0 => None,
            count => Some(format!("    (\"{path}\", {count}),")),
        })
        .collect()
}

/// Takes the census of every `.rs` file under `crates_dir`, keyed by the path
/// relative to that directory with `/` separators.
fn census_of(crates_dir: &Path) -> BTreeMap<String, FileCensus> {
    let mut census = BTreeMap::new();
    for crate_dir in crate_directories(crates_dir) {
        let declared = declared_generator_outputs(&crate_dir);
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
                    kind: classify(&path, &declared, &shipped),
                    suppressions: suppressions_in(&path, &source),
                },
            );
        }
    }
    census
}

/// The crate directories under `crates_dir`, in one order for every reader.
fn crate_directories(crates_dir: &Path) -> Vec<PathBuf> {
    let mut crate_dirs = fs::read_dir(crates_dir)
        .unwrap_or_else(|error| panic!("read {}: {error}", crates_dir.display()))
        .filter_map(Result::ok)
        .map(|entry| entry.path())
        .filter(|path| path.is_dir())
        .collect::<Vec<_>>();
    crate_dirs.sort();
    crate_dirs
}

fn relative_key(root: &Path, path: &Path) -> String {
    path.strip_prefix(root)
        .unwrap_or(path)
        .to_string_lossy()
        .replace('\\', "/")
}

fn file_name_of(path: &str) -> Option<&str> {
    Path::new(path).file_name().and_then(|name| name.to_str())
}

/// The file names a crate's build script declares that its generator emits.
///
/// This is the only authority for the generated carve-out. Not the directory a
/// file sits in, and not a banner a file carries: both are spellings a person
/// can set, and a file cannot certify its own provenance. The build script is
/// the thing that actually runs the generator.
fn declared_generator_outputs(crate_dir: &Path) -> BTreeSet<String> {
    let build_script = crate_dir.join(BUILD_SCRIPT);
    if !build_script.is_file() {
        return BTreeSet::new();
    }
    let source = fs::read_to_string(&build_script)
        .unwrap_or_else(|error| panic!("read {}: {error}", build_script.display()));
    declared_outputs_in(&build_script, &source)
}

/// Extracts every `<declaration>("<name>")` from a build script.
///
/// A declaration whose argument is not a string literal stops the run. Reading
/// it as an absent declaration would let the gate guess at provenance, and a
/// gate that guesses about which files are generated is the defect this whole
/// classification exists to remove.
fn declared_outputs_in(path: &Path, source: &str) -> BTreeSet<String> {
    let mut declared = BTreeSet::new();
    for declaration in GENERATOR_OUTPUT_DECLARATIONS {
        let names = literal_arguments_of(path, source, declaration);
        assert_eq!(
            source.matches(&format!("{declaration}(")).count(),
            names.len(),
            "{}: a `{declaration}` call does not name a string literal, so the census cannot read \
which file the generator emits and must not guess",
            path.display()
        );
        declared.extend(names);
    }
    declared
}

/// Every string-literal argument passed to `declaration` in `source`, in order.
fn literal_arguments_of(path: &Path, source: &str, declaration: &str) -> Vec<String> {
    let literal_call = format!("{declaration}(\"");
    let mut arguments = Vec::new();
    let mut rest = source;
    while let Some(start) = rest.find(&literal_call) {
        let after = &rest[start + literal_call.len()..];
        let Some(end) = after.find('"') else {
            panic!(
                "{}: unterminated `{declaration}` output declaration",
                path.display()
            );
        };
        arguments.push(after[..end].to_string());
        rest = &after[end..];
    }
    arguments
}

/// Classifies one file. `declared` is the set of file names the crate's build
/// script says its generator emits; `shipped` is the set of files under the
/// crate's `src` directory that a release build compiles, computed by the same
/// module-tree walk the totality-debt gate uses, so "what does the shipped code
/// say" keeps one answer across the hardening gates.
fn classify(path: &Path, declared: &BTreeSet<String>, shipped: &BTreeSet<PathBuf>) -> SourceKind {
    if let Some(declaration) = declared_output_for(path, declared) {
        return SourceKind::Generated { declaration };
    }
    if shipped.contains(path) {
        return SourceKind::Production;
    }
    SourceKind::Test
}

/// The build-script declaration that covers this file, if one does.
fn declared_output_for(path: &Path, declared: &BTreeSet<String>) -> Option<String> {
    let name = path.file_name()?.to_str()?;
    declared.get(name).cloned()
}

/// The declaration call the fixtures write, taken from the same list the
/// extractor reads, so a fixture cannot drift from the production rule.
fn fixture_output_declaration() -> &'static str {
    GENERATOR_OUTPUT_DECLARATIONS
        .first()
        .copied()
        .expect("the census declares at least one generator output call")
}

/// A build script in the shape the census reads: a generator declaring the
/// file it emits.
fn generator_build_script(output_dir: &str, declared: &str) -> String {
    format!(
        "fn main() {{\n    Builder::with_explicit_output_dir(\"{output_dir}\")\n        .{}(\"{declared}\")\n        .generate_parser();\n}}\n",
        fixture_output_declaration()
    )
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

    fn record_attribute(&mut self, attribute: &syn::Attribute) {
        if suppression_form(&attribute.meta).is_some() {
            self.record(&attribute.meta, false);
            return;
        }
        let syn::Meta::List(list) = &attribute.meta else {
            return;
        };
        if !list.path.is_ident("cfg_attr") {
            return;
        }
        let arguments = meta_arguments(list);
        let conditional_on_test = arguments.first().is_some_and(meta_mentions_test);
        for nested in arguments.iter().skip(1) {
            self.record(nested, conditional_on_test);
        }
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
        self.record_attribute(attribute);
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
    meta_arguments(list)
        .iter()
        .filter_map(|argument| match argument {
            syn::Meta::Path(path) => Some(path.to_token_stream().to_string().replace(' ', "")),
            _ => None,
        })
        .collect()
}

/// The comma-separated `Meta` arguments of an attribute list, or the parse
/// error that shows the attribute is not meta-shaped. Every caller, production
/// and witness alike, reads the arguments through this one predicate.
fn parsed_arguments(list: &syn::MetaList) -> syn::Result<Vec<syn::Meta>> {
    Punctuated::<syn::Meta, syn::Token![,]>::parse_terminated
        .parse2(list.tokens.clone())
        .map(|parsed| parsed.into_iter().collect())
}

/// The same predicate, refusing loudly.
///
/// Answering "no arguments" for an attribute the scan could not read deletes
/// the suppression from the census: `suppressed_lints` returns nothing, and a
/// suppression with no named lint is not recorded. That is an undercount, and
/// an undercount is the one direction a ratchet must never fail in, so an
/// unreadable attribute stops the run the way an unreadable file does.
fn meta_arguments(list: &syn::MetaList) -> Vec<syn::Meta> {
    parsed_arguments(list).unwrap_or_else(|error| {
        panic!(
            "read the arguments of `{}`: {error}",
            list.to_token_stream().to_string().replace(' ', "")
        )
    })
}

fn attribute_is_cfg_test(attribute: &syn::Attribute) -> bool {
    let syn::Meta::List(list) = &attribute.meta else {
        return false;
    };
    list.path.is_ident("cfg") && meta_arguments(list).iter().any(meta_mentions_test)
}

/// Whether a `cfg` predicate names the `test` configuration, following the
/// `any`/`all`/`not` combinators.
fn meta_mentions_test(meta: &syn::Meta) -> bool {
    match meta {
        syn::Meta::Path(path) => path.is_ident("test"),
        syn::Meta::List(list) => meta_arguments(list).iter().any(meta_mentions_test),
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
