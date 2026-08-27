//! Acceptance-surface gate: every path that renders an artifact names the
//! checked view standing between its IR and the emitted bytes.
//!
//! Two registries feed this gate: the built-in code-gen targets bundled by
//! `rumoca-phase-codegen`, and the `--emit <stage>` IR dumps declared by
//! `EmitTarget` in the CLI. Each surface gets exactly one row below stating
//! which checked view it consumes, or [`CheckedView::SelfDescribing`] when the
//! artifact is a complete serialization of the IR it names - flags included -
//! and therefore makes no claim a checked view could falsify. A surface added
//! to either registry without a row fails this gate, so "which view proves
//! this artifact" is a decision that cannot be skipped.
//!
//! The audit these rows summarize is `dev/2026-08-22-acceptance-surface-audit.md`.

use super::*;

use rumoca_compile::codegen::targets::{TargetTemplateIr, parse_target_manifest};
use rumoca_compile::codegen::templates::builtin_targets;

/// What a surface proves about an artifact before emitting its bytes.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum CheckedView {
    /// The checked DAE semantic projection (`dae_backend::project`), admitted
    /// by the manifest's `[capabilities]` table.
    CheckedDaeProjection,
    /// The lowered `SolveProblem`/`SolveArtifacts` render handle, admitted by
    /// `SolveProblem::validate` plus the Solve capability gate.
    CheckedSolveProblem,
    /// The event-free checked FMI component view.
    CheckedFmiComponent,
    /// The GALEC Algorithm Code projection.
    AlgorithmCodeProjection,
    /// The materialized Flat scalar-equation view, whose refusal is
    /// `rumoca::codegen::EC007`.
    MaterializedFlatEquations,
    /// The resolved class tree, serialized whole into the template context. It
    /// is source structure ahead of any lowering, so it makes no claim about
    /// simulation semantics for a checked view to guard - the `SelfDescribing`
    /// case in target form.
    ResolvedAstTree,
    /// A complete serialization of one IR, contract flags included. Nothing is
    /// asserted about the artifact beyond "this is that IR", so there is no
    /// contract for a checked view to guard.
    SelfDescribing,
}

/// Every classification, so the bijection gate below reads one list rather
/// than a second copy of the enum.
const ALL_CHECKED_VIEWS: &[CheckedView] = &[
    CheckedView::CheckedDaeProjection,
    CheckedView::CheckedSolveProblem,
    CheckedView::CheckedFmiComponent,
    CheckedView::AlgorithmCodeProjection,
    CheckedView::MaterializedFlatEquations,
    CheckedView::ResolvedAstTree,
    CheckedView::SelfDescribing,
];

impl CheckedView {
    /// The manifest `ir` a target consuming this view must declare, or `None`
    /// for a view no code-gen target can consume.
    fn target_ir(self) -> Option<TargetTemplateIr> {
        match self {
            Self::CheckedDaeProjection => Some(TargetTemplateIr::Dae),
            Self::CheckedSolveProblem => Some(TargetTemplateIr::Solve),
            Self::CheckedFmiComponent => Some(TargetTemplateIr::Fmi),
            Self::AlgorithmCodeProjection => Some(TargetTemplateIr::AlgorithmCode),
            Self::MaterializedFlatEquations => Some(TargetTemplateIr::Flat),
            Self::ResolvedAstTree => Some(TargetTemplateIr::Ast),
            Self::SelfDescribing => None,
        }
    }

    /// Whether a target consuming this view must declare `[capabilities]`.
    ///
    /// Mirrors `target_manifest::validate_target_requirements`: the DAE-derived
    /// views carry their admissibility proof in that table, while the Flat view
    /// proves its own contract inside the render context.
    fn requires_declared_capabilities(self) -> bool {
        matches!(
            self,
            Self::CheckedDaeProjection
                | Self::CheckedSolveProblem
                | Self::CheckedFmiComponent
                | Self::AlgorithmCodeProjection
        )
    }
}

/// One row per built-in code-gen target, keyed by registry name.
const BUILTIN_TARGET_SURFACES: &[(&str, CheckedView)] = &[
    ("base-modelica", CheckedView::MaterializedFlatEquations),
    ("casadi-ode", CheckedView::CheckedSolveProblem),
    ("c-ode", CheckedView::CheckedSolveProblem),
    ("cuda-ode", CheckedView::CheckedSolveProblem),
    ("dae-modelica", CheckedView::CheckedDaeProjection),
    ("embedded-c-galec", CheckedView::AlgorithmCodeProjection),
    ("flat-modelica", CheckedView::MaterializedFlatEquations),
    ("fmi2", CheckedView::CheckedFmiComponent),
    ("fmi3", CheckedView::CheckedFmiComponent),
    ("fmi-ls-wasm", CheckedView::CheckedFmiComponent),
    ("galec", CheckedView::AlgorithmCodeProjection),
    ("galec-production", CheckedView::AlgorithmCodeProjection),
    ("jax-ode", CheckedView::CheckedSolveProblem),
    ("mlir", CheckedView::CheckedSolveProblem),
    ("rust-fixed-ode", CheckedView::CheckedSolveProblem),
    ("rust-ode", CheckedView::CheckedSolveProblem),
    ("wgsl-ode", CheckedView::CheckedSolveProblem),
];

/// One row per `compile --emit` stage, keyed by the clap value name.
///
/// `flat-json` is deliberately `SelfDescribing`: it serializes cheapened
/// interior rows, and it serializes `interiors_materialized: false` alongside
/// them, so the artifact states its own contract instead of claiming a
/// materialized scalar view.
const EMIT_SURFACES: &[(&str, CheckedView)] = &[
    ("ast-json", CheckedView::SelfDescribing),
    ("flat-mo", CheckedView::MaterializedFlatEquations),
    ("flat-json", CheckedView::SelfDescribing),
    ("dae-mo", CheckedView::CheckedDaeProjection),
    ("dae-json", CheckedView::SelfDescribing),
    ("solve-json", CheckedView::SelfDescribing),
];

/// Registry names with no row, and rows naming nothing in the registry.
///
/// Both directions are reported: a renamed surface must fail as loudly as a
/// new one, because a stale row silently covers no surface at all.
fn surface_table_drift(registered: &[String], declared: &[&str]) -> (Vec<String>, Vec<String>) {
    let registered: BTreeSet<&str> = registered.iter().map(String::as_str).collect();
    let declared: BTreeSet<&str> = declared.iter().copied().collect();
    let undeclared = registered
        .difference(&declared)
        .map(|name| (*name).to_owned())
        .collect();
    let stale = declared
        .difference(&registered)
        .map(|name| (*name).to_owned())
        .collect();
    (undeclared, stale)
}

fn declared_target_names() -> Vec<&'static str> {
    BUILTIN_TARGET_SURFACES
        .iter()
        .map(|(name, _)| *name)
        .collect()
}

fn declared_view(target: &str) -> CheckedView {
    BUILTIN_TARGET_SURFACES
        .iter()
        .find(|(name, _)| *name == target)
        .map(|(_, view)| *view)
        .unwrap_or_else(|| panic!("built-in target `{target}` has no acceptance-surface row"))
}

/// The `#[value(name = "...")]` strings of the `EmitTarget` enum in the CLI.
fn registered_emit_stages() -> Vec<String> {
    let source = fs::read_to_string(workspace_root().join("crates/rumoca/src/cli.rs"))
        .expect("read the CLI argument definitions");
    let body = source
        .split_once("pub enum EmitTarget {")
        .expect("cli.rs declares `pub enum EmitTarget`")
        .1
        .split_once("\n}")
        .expect("the `EmitTarget` declaration is brace-terminated")
        .0;
    let stages: Vec<String> = body
        .lines()
        .filter_map(|line| line.trim().strip_prefix("#[value(name = \""))
        .filter_map(|rest| rest.split_once('"'))
        .map(|(name, _)| name.to_owned())
        .collect();
    assert!(
        !stages.is_empty(),
        "no `--emit` stages parsed out of `EmitTarget`; the scan has gone stale"
    );
    stages
}

#[test]
fn every_builtin_target_declares_the_checked_view_it_consumes() {
    let registered: Vec<String> = builtin_targets()
        .iter()
        .map(|target| target.name.to_owned())
        .collect();
    let (undeclared, stale) = surface_table_drift(&registered, &declared_target_names());
    assert!(
        undeclared.is_empty(),
        "built-in target(s) {undeclared:?} emit artifacts with no declared checked view; \
         add a row to BUILTIN_TARGET_SURFACES naming the view that proves the artifact"
    );
    assert!(
        stale.is_empty(),
        "acceptance-surface row(s) {stale:?} name no registered built-in target"
    );
}

#[test]
fn every_emit_stage_declares_the_checked_view_it_consumes() {
    let registered = registered_emit_stages();
    let declared: Vec<&str> = EMIT_SURFACES.iter().map(|(name, _)| *name).collect();
    let (undeclared, stale) = surface_table_drift(&registered, &declared);
    assert!(
        undeclared.is_empty(),
        "`--emit` stage(s) {undeclared:?} dump artifacts with no declared checked view; \
         add a row to EMIT_SURFACES naming the view, or SelfDescribing when the dump \
         serializes its own contract flags"
    );
    assert!(
        stale.is_empty(),
        "acceptance-surface row(s) {stale:?} name no `--emit` stage"
    );
}

#[test]
fn declared_checked_views_agree_with_each_target_manifest() {
    for target in builtin_targets() {
        let manifest = parse_target_manifest(target.manifest)
            .unwrap_or_else(|error| panic!("parse `{}` target manifest: {error}", target.name));
        let view = declared_view(target.name);
        assert_eq!(
            view.target_ir(),
            Some(manifest.ir),
            "target `{}` declares the {view:?} checked view but its manifest consumes {:?} IR",
            target.name,
            manifest.ir
        );
    }
}

#[test]
fn capability_gated_targets_declare_a_capabilities_table() {
    for target in builtin_targets() {
        let view = declared_view(target.name);
        if !view.requires_declared_capabilities() {
            continue;
        }
        let manifest = parse_target_manifest(target.manifest)
            .unwrap_or_else(|error| panic!("parse `{}` target manifest: {error}", target.name));
        assert!(
            manifest.capabilities.is_some(),
            "target `{}` consumes the {view:?} checked view, whose whole admissibility proof \
             runs through [capabilities]; an absent table is refused at render time, so a \
             built-in shipping without one can never emit",
            target.name
        );
    }
}

#[test]
fn surface_table_drift_reports_an_undeclared_surface() {
    let registered = vec!["dae-modelica".to_owned(), "brand-new-target".to_owned()];
    let (undeclared, stale) = surface_table_drift(&registered, &["dae-modelica"]);
    assert_eq!(undeclared, vec!["brand-new-target".to_owned()]);
    assert!(stale.is_empty());
}

#[test]
fn surface_table_drift_reports_a_stale_row() {
    let registered = vec!["dae-modelica".to_owned()];
    let (undeclared, stale) = surface_table_drift(&registered, &["dae-modelica", "renamed-away"]);
    assert!(undeclared.is_empty());
    assert_eq!(stale, vec!["renamed-away".to_owned()]);
}

#[test]
fn checked_views_and_target_ir_kinds_are_a_bijection() {
    // A new `TargetTemplateIr` variant is a new acceptance surface, so it must
    // arrive with the view that proves it rather than inheriting another's.
    let mapped: BTreeSet<String> = ALL_CHECKED_VIEWS
        .iter()
        .filter_map(|view| view.target_ir())
        .map(|ir| format!("{ir:?}"))
        .collect();
    let source =
        fs::read_to_string(workspace_root().join("crates/rumoca-compile/src/codegen_target.rs"))
            .expect("read the target manifest definitions");
    let body = source
        .split_once("pub enum TargetTemplateIr {")
        .expect("codegen_target.rs declares `pub enum TargetTemplateIr`")
        .1
        .split_once("\n}")
        .expect("the `TargetTemplateIr` declaration is brace-terminated")
        .0;
    let declared: BTreeSet<String> = body
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty() && !line.starts_with("//"))
        .map(|line| line.trim_end_matches(',').to_owned())
        .collect();
    assert_eq!(
        mapped, declared,
        "every TargetTemplateIr variant needs exactly one CheckedView that consumes it"
    );
}
