//! Acceptance-surface gate: every path that renders an artifact names the
//! checked view standing between its IR and the emitted bytes; a retained
//! unsupported spelling is classified as a named refusal instead.
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

use rumoca_compile::codegen::targets::{TargetRequiredProduct, builtin_target_descriptors};

/// What a surface proves before it can emit bytes.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum CheckedView {
    /// A retained CLI spelling that emits no artifact and returns one stable
    /// `unsupported-feature:*` refusal before semantic compilation.
    NamedRefusal,
    /// The checked DAE semantic projection (`dae_backend::project`), admitted
    /// by the manifest's `[capabilities]` table.
    CheckedDaeProjection,
    /// The complete checked `SolveModel`, admitted by its construction plus
    /// the Solve capability gate.
    CheckedSolveModel,
    /// The event-free checked `rumoca_ir_solve::fmi::FmiComponent` view.
    CheckedFmiComponent,
    /// The GALEC Algorithm Code projection.
    AlgorithmCodeProjection,
    /// One correlated Algorithm Code package plus its checked
    /// `SolveAlgorithmBlock` refinement.
    CheckedSolveAlgorithmProduct,
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
    CheckedView::NamedRefusal,
    CheckedView::CheckedDaeProjection,
    CheckedView::CheckedSolveModel,
    CheckedView::CheckedFmiComponent,
    CheckedView::AlgorithmCodeProjection,
    CheckedView::CheckedSolveAlgorithmProduct,
    CheckedView::MaterializedFlatEquations,
    CheckedView::ResolvedAstTree,
    CheckedView::SelfDescribing,
];

impl CheckedView {
    /// The single construction-issued product the manifest context set
    /// requires, or `None` for a view no code-generation target consumes.
    fn required_product(self) -> Option<TargetRequiredProduct> {
        match self {
            Self::NamedRefusal => None,
            Self::CheckedDaeProjection => Some(TargetRequiredProduct::Dae),
            Self::CheckedSolveModel => Some(TargetRequiredProduct::SolveModel),
            Self::CheckedFmiComponent => Some(TargetRequiredProduct::FmiComponent),
            Self::AlgorithmCodeProjection => Some(TargetRequiredProduct::AlgorithmCodePackage),
            Self::CheckedSolveAlgorithmProduct => {
                Some(TargetRequiredProduct::SolveAlgorithmProduct)
            }
            Self::MaterializedFlatEquations => Some(TargetRequiredProduct::Flat),
            Self::ResolvedAstTree => Some(TargetRequiredProduct::Ast),
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
                | Self::CheckedSolveModel
                | Self::CheckedFmiComponent
                | Self::AlgorithmCodeProjection
                | Self::CheckedSolveAlgorithmProduct
        )
    }
}

/// One row per built-in code-gen target, keyed by registry name.
const BUILTIN_TARGET_SURFACES: &[(&str, CheckedView)] = &[
    ("casadi-ode", CheckedView::CheckedSolveModel),
    ("cuda-ode", CheckedView::CheckedSolveModel),
    ("dae-modelica", CheckedView::CheckedDaeProjection),
    ("efmu", CheckedView::CheckedSolveAlgorithmProduct),
    ("fmi2", CheckedView::CheckedFmiComponent),
    ("fmi3", CheckedView::CheckedFmiComponent),
    ("fmi-ls-wasm", CheckedView::CheckedFmiComponent),
    ("galec", CheckedView::AlgorithmCodeProjection),
    ("jax-ode", CheckedView::CheckedSolveModel),
    ("mlir", CheckedView::CheckedSolveModel),
    ("rust-fixed-ode", CheckedView::CheckedSolveModel),
    ("rust-ode", CheckedView::CheckedSolveModel),
    ("wgsl-ode", CheckedView::CheckedSolveModel),
];

/// One row per `compile --emit` stage, keyed by the clap value name.
///
/// `flat-json` is deliberately `SelfDescribing`: it serializes cheapened
/// interior rows, and it serializes `interiors_materialized: false` alongside
/// them, so the artifact states its own contract instead of claiming a
/// materialized scalar view.
const EMIT_SURFACES: &[(&str, CheckedView)] = &[
    ("ast-json", CheckedView::SelfDescribing),
    ("flat-mo", CheckedView::NamedRefusal),
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
    let registered: Vec<String> = builtin_target_descriptors()
        .expect("check every built-in target bundle")
        .into_iter()
        .map(|target| target.id)
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
fn declared_checked_views_agree_with_each_checked_target_descriptor() {
    for target in builtin_target_descriptors().expect("check every built-in target bundle") {
        let view = declared_view(&target.id);
        assert_eq!(
            view.required_product(),
            Some(target.required_product),
            "target `{}` declares the {view:?} checked view but its context set requires {:?}",
            target.id,
            target.required_product
        );
    }
}

#[test]
fn capability_gated_targets_declare_a_capabilities_table() {
    for target in builtin_target_descriptors().expect("check every built-in target bundle") {
        let view = declared_view(&target.id);
        if !view.requires_declared_capabilities() {
            continue;
        }
        assert!(
            target.capabilities.is_some(),
            "target `{}` consumes the {view:?} checked view, whose whole admissibility proof \
             runs through [capabilities]; an absent table is refused at render time, so a \
             built-in shipping without one can never emit",
            target.id
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
fn checked_views_map_to_their_exact_required_product() {
    let mapped = ALL_CHECKED_VIEWS
        .iter()
        .filter_map(|view| view.required_product())
        .collect::<Vec<_>>();
    assert_eq!(
        CheckedView::CheckedFmiComponent.required_product(),
        Some(TargetRequiredProduct::FmiComponent),
        "FMI files must consume the real checked FmiComponent root"
    );
    assert!(
        mapped.contains(&TargetRequiredProduct::SolveModel)
            && mapped.contains(&TargetRequiredProduct::FmiComponent)
            && mapped.contains(&TargetRequiredProduct::AlgorithmCodePackage)
            && mapped.contains(&TargetRequiredProduct::SolveAlgorithmProduct),
        "registered acceptance views must retain each exact checked product"
    );
}
