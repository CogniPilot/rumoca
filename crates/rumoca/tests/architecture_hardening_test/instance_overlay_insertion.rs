//! Checked `InstanceOverlay` insertion boundary (SPEC_0036 / AS-052).

use super::architecture_hardening_support::{attributes_require_test, production_rust_sources};
use super::*;
use std::collections::HashSet;
use syn::visit::{self, Visit};

#[derive(Default)]
struct DirectOverlayInsertVisitor {
    count: usize,
    overlay_bindings: HashSet<String>,
    catalog_bindings: HashSet<String>,
}

impl<'ast> Visit<'ast> for DirectOverlayInsertVisitor {
    fn visit_item_mod(&mut self, module: &'ast syn::ItemMod) {
        if attributes_require_test(&module.attrs) {
            return;
        }
        visit::visit_item_mod(self, module);
    }

    fn visit_item_fn(&mut self, function: &'ast syn::ItemFn) {
        if attributes_require_test(&function.attrs) {
            return;
        }
        let saved_overlays = self.overlay_bindings.clone();
        let saved_catalogs = self.catalog_bindings.clone();
        self.record_overlay_parameters(&function.sig);
        visit::visit_item_fn(self, function);
        self.overlay_bindings = saved_overlays;
        self.catalog_bindings = saved_catalogs;
    }

    fn visit_impl_item_fn(&mut self, function: &'ast syn::ImplItemFn) {
        if attributes_require_test(&function.attrs) {
            return;
        }
        let saved_overlays = self.overlay_bindings.clone();
        let saved_catalogs = self.catalog_bindings.clone();
        self.record_overlay_parameters(&function.sig);
        visit::visit_impl_item_fn(self, function);
        self.overlay_bindings = saved_overlays;
        self.catalog_bindings = saved_catalogs;
    }

    fn visit_local(&mut self, local: &'ast syn::Local) {
        if let Some(init) = &local.init
            && let Some(binding) = pattern_ident(&local.pat)
        {
            if is_overlay_occurrence_catalog(
                &init.expr,
                &self.overlay_bindings,
                &self.catalog_bindings,
            ) {
                self.catalog_bindings.insert(binding.to_string());
            } else if expression_is_overlay_binding(&init.expr, &self.overlay_bindings) {
                self.overlay_bindings.insert(binding.to_string());
            }
        }
        visit::visit_local(self, local);
    }

    fn visit_expr_method_call(&mut self, call: &'ast syn::ExprMethodCall) {
        if call.method == "insert"
            && is_overlay_occurrence_catalog(
                &call.receiver,
                &self.overlay_bindings,
                &self.catalog_bindings,
            )
        {
            self.count += 1;
        }
        visit::visit_expr_method_call(self, call);
    }
}

impl DirectOverlayInsertVisitor {
    fn record_overlay_parameters(&mut self, signature: &syn::Signature) {
        for input in &signature.inputs {
            let syn::FnArg::Typed(argument) = input else {
                continue;
            };
            if type_mentions_instance_overlay(&argument.ty)
                && let Some(binding) = pattern_ident(&argument.pat)
            {
                self.overlay_bindings.insert(binding.to_string());
            }
        }
    }
}

fn is_overlay_occurrence_catalog(
    expression: &syn::Expr,
    overlay_bindings: &HashSet<String>,
    catalog_bindings: &HashSet<String>,
) -> bool {
    match peel_reference_and_parens(expression) {
        syn::Expr::Path(path) => path
            .path
            .get_ident()
            .is_some_and(|ident| catalog_bindings.contains(&ident.to_string())),
        syn::Expr::Field(field) => {
            let syn::Member::Named(catalog) = &field.member else {
                return false;
            };
            matches!(catalog.to_string().as_str(), "components" | "classes")
                && (expression_is_overlay_binding(&field.base, overlay_bindings)
                    || expression_mentions_overlay(&field.base))
        }
        _ => false,
    }
}

fn expression_is_overlay_binding(
    expression: &syn::Expr,
    overlay_bindings: &HashSet<String>,
) -> bool {
    match peel_reference_and_parens(expression) {
        syn::Expr::Path(path) => path
            .path
            .get_ident()
            .is_some_and(|ident| overlay_bindings.contains(&ident.to_string())),
        _ => false,
    }
}

fn peel_reference_and_parens(mut expression: &syn::Expr) -> &syn::Expr {
    loop {
        expression = match expression {
            syn::Expr::Reference(reference) => &reference.expr,
            syn::Expr::Paren(paren) => &paren.expr,
            _ => return expression,
        };
    }
}

fn pattern_ident(pattern: &syn::Pat) -> Option<&syn::Ident> {
    match pattern {
        syn::Pat::Ident(ident) => Some(&ident.ident),
        syn::Pat::Type(typed) => pattern_ident(&typed.pat),
        _ => None,
    }
}

fn type_mentions_instance_overlay(ty: &syn::Type) -> bool {
    match ty {
        syn::Type::Path(path) => path
            .path
            .segments
            .last()
            .is_some_and(|segment| segment.ident == "InstanceOverlay"),
        syn::Type::Reference(reference) => type_mentions_instance_overlay(&reference.elem),
        syn::Type::Paren(paren) => type_mentions_instance_overlay(&paren.elem),
        _ => false,
    }
}

fn expression_mentions_overlay(expression: &syn::Expr) -> bool {
    match expression {
        syn::Expr::Path(path) => path
            .path
            .segments
            .last()
            .is_some_and(|segment| segment.ident.to_string().contains("overlay")),
        syn::Expr::Field(field) => {
            matches!(&field.member, syn::Member::Named(name) if name.to_string().contains("overlay"))
                || expression_mentions_overlay(&field.base)
        }
        syn::Expr::Paren(paren) => expression_mentions_overlay(&paren.expr),
        syn::Expr::Reference(reference) => expression_mentions_overlay(&reference.expr),
        _ => false,
    }
}

#[test]
fn production_never_bypasses_checked_instance_overlay_insertion() {
    let root = workspace_root();
    let crates = root.join("crates");
    let mut offenders = Vec::new();
    for entry in fs::read_dir(&crates).expect("read workspace crates") {
        let entry = entry.expect("read crate entry");
        if !entry.path().join("Cargo.toml").is_file() {
            continue;
        }
        for (path, source) in production_rust_sources(&entry.path(), &root) {
            let syntax = syn::parse_file(&source)
                .unwrap_or_else(|error| panic!("parse {}: {error}", path.display()));
            let mut visitor = DirectOverlayInsertVisitor::default();
            visitor.visit_file(&syntax);
            if visitor.count != 0 {
                offenders.push((path, visitor.count));
            }
        }
    }
    assert!(
        offenders.is_empty(),
        "production code must use InstanceOverlay::add_component/add_class; raw overlay catalog insertion found: {offenders:#?}"
    );
}

#[test]
fn overlay_insertion_gate_detects_direct_catalog_mutation() {
    let syntax = syn::parse_file(
        "fn bad(overlay: &mut InstanceOverlay, data: InstanceData) { overlay.components.insert(data.instance_id, data); }",
    )
    .expect("parse mutation fixture");
    let mut visitor = DirectOverlayInsertVisitor::default();
    visitor.visit_file(&syntax);
    assert_eq!(visitor.count, 1);
}

#[test]
fn overlay_insertion_gate_detects_nonstandard_overlay_and_catalog_aliases() {
    for source in [
        "fn bad(instances: &mut InstanceOverlay, data: InstanceData) { instances.components.insert(data.instance_id, data); }",
        "fn bad(instances: &mut InstanceOverlay, data: InstanceData) { let catalog = &mut instances.components; catalog.insert(data.instance_id, data); }",
    ] {
        let syntax = syn::parse_file(source).expect("parse alias mutation fixture");
        let mut visitor = DirectOverlayInsertVisitor::default();
        visitor.visit_file(&syntax);
        assert_eq!(visitor.count, 1, "gate missed mutation fixture: {source}");
    }
}

#[test]
fn equality_constraint_pending_proofs_cannot_cross_overlay_roots() {
    let root = workspace_root();
    let occurrence = fs::read_to_string(
        root.join("crates/rumoca-ir-ast/src/instance/equality_constraint/occurrence.rs"),
    )
    .expect("read occurrence construction owner");
    let equality =
        fs::read_to_string(root.join("crates/rumoca-ir-ast/src/instance/equality_constraint.rs"))
            .expect("read equalityConstraint construction facade");
    let facade = fs::read_to_string(root.join("crates/rumoca-ir-ast/src/lib.rs"))
        .expect("read ir-ast facade");
    let instantiate =
        fs::read_to_string(root.join("crates/rumoca-phase-instantiate/src/equality_constraint.rs"))
            .expect("read equalityConstraint Instantiate owner");

    assert!(!occurrence.contains("pub struct EqualityConstraintRecordOccurrence"));
    assert!(!occurrence.contains("pub struct PendingEqualityConstraintOccurrenceExposure"));
    assert!(!occurrence.contains("pub fn register_overconstrained_record"));
    assert!(!equality.contains("pub fn complete_equality_constraint_occurrence"));
    assert!(!facade.contains("EqualityConstraintRecordOccurrence"));
    assert!(!facade.contains("PendingEqualityConstraintOccurrenceExposure"));
    assert!(occurrence.contains("pub fn construct_and_register_equality_constraint_occurrence"));
    assert!(!instantiate.contains("TypeOverrideMap"));
    assert!(!instantiate.contains("target_for_alias_def_id"));
    assert!(!instantiate.contains("effective type map"));
}

#[derive(Default)]
struct ProductionFixtureHelperUseVisitor {
    count: usize,
}

impl<'ast> Visit<'ast> for ProductionFixtureHelperUseVisitor {
    fn visit_item_mod(&mut self, module: &'ast syn::ItemMod) {
        if attributes_require_test(&module.attrs) {
            return;
        }
        visit::visit_item_mod(self, module);
    }

    fn visit_item_fn(&mut self, function: &'ast syn::ItemFn) {
        if attributes_require_test(&function.attrs) {
            return;
        }
        visit::visit_item_fn(self, function);
    }

    fn visit_expr_path(&mut self, path: &'ast syn::ExprPath) {
        if path
            .path
            .segments
            .iter()
            .any(|segment| segment.ident == "finalized_test_overlay")
        {
            self.count += 1;
        }
        visit::visit_expr_path(self, path);
    }

    fn visit_item_use(&mut self, item: &'ast syn::ItemUse) {
        if use_tree_mentions_finalized_test_overlay(&item.tree) {
            self.count += 1;
        }
        visit::visit_item_use(self, item);
    }
}

fn use_tree_mentions_finalized_test_overlay(tree: &syn::UseTree) -> bool {
    match tree {
        syn::UseTree::Path(path) => use_tree_mentions_finalized_test_overlay(&path.tree),
        syn::UseTree::Name(name) => name.ident == "finalized_test_overlay",
        syn::UseTree::Rename(rename) => rename.ident == "finalized_test_overlay",
        syn::UseTree::Group(group) => group
            .items
            .iter()
            .any(use_tree_mentions_finalized_test_overlay),
        syn::UseTree::Glob(_) => false,
    }
}

#[test]
fn retired_overconstrained_flat_projections_and_fixture_authority_cannot_return() {
    const RETIRED_PROJECTIONS: [&str; 3] = [
        "is_overconstrained",
        "oc_record_path",
        "oc_eq_constraint_size",
    ];
    let root = workspace_root();
    let crates = root.join("crates");
    let mut retired_projection_uses = Vec::new();
    let mut production_fixture_uses = Vec::new();

    for entry in fs::read_dir(&crates).expect("read workspace crates") {
        let entry = entry.expect("read crate entry");
        if !entry.path().join("Cargo.toml").is_file() {
            continue;
        }
        for (path, source) in production_rust_sources(&entry.path(), &root) {
            for retired in RETIRED_PROJECTIONS {
                if source.contains(retired) {
                    retired_projection_uses.push((path.clone(), retired));
                }
            }
            let syntax = syn::parse_file(&source)
                .unwrap_or_else(|error| panic!("parse {}: {error}", path.display()));
            let mut visitor = ProductionFixtureHelperUseVisitor::default();
            visitor.visit_file(&syntax);
            if visitor.count != 0 {
                production_fixture_uses.push((path.clone(), visitor.count));
            }
        }
    }

    assert!(
        retired_projection_uses.is_empty(),
        "Instance/Flat overconstrained compatibility projections are retired; use FinalizedOverconstrainedCatalog: {retired_projection_uses:#?}"
    );
    assert!(
        production_fixture_uses.is_empty(),
        "production code cannot call or import the test-only finalized overlay fixture constructor: {production_fixture_uses:#?}"
    );

    let fixture = fs::read_to_string(root.join("crates/rumoca-phase-flatten/src/test_support.rs"))
        .expect("read flatten test fixture support");
    let connections =
        fs::read_to_string(root.join("crates/rumoca-phase-flatten/src/connections/mod.rs"))
            .expect("read connection construction owner");
    assert!(
        !connections.contains("seed_connection_test_effective_types"),
        "the retired fixture repair/seeding authority cannot return"
    );
    let constructor = fixture
        .split_once("pub(crate) fn finalized_test_overlay")
        .and_then(|(_, tail)| tail.split_once("\n}\n"))
        .map(|(body, _)| body)
        .expect("locate the finalized test overlay constructor body");
    assert_eq!(
        constructor
            .matches("finalize_overconstrained_record_owners()")
            .count(),
        1,
        "the fixture constructor must invoke the production owner proof exactly once"
    );
    assert_eq!(
        constructor
            .matches("finalize_effective_type_publication(")
            .count(),
        1,
        "the fixture constructor must invoke the production effective-type proof exactly once"
    );
    assert_eq!(
        constructor.matches("finalized_overconstrained()").count(),
        1,
        "the fixture constructor must return the production catalog proof"
    );
    for forbidden in [
        "type_roots.entry",
        ".clone()",
        "unwrap_or",
        "Default::default",
    ] {
        assert!(
            !constructor.contains(forbidden),
            "the fixture constructor cannot repair, clone, or default proof inputs: {forbidden}"
        );
    }
}

#[test]
fn vcg_lookup_cannot_restore_fail_open_boolean_defaults() {
    let root = workspace_root();
    let boolean_eval =
        fs::read_to_string(root.join("crates/rumoca-phase-flatten/src/boolean_eval.rs"))
            .expect("read flatten boolean evaluator");

    for retired in [
        "Fallback for models without VCG data",
        "preserves previous behavior",
        "None => Some(true)",
        "None => Some(false)",
    ] {
        assert!(
            !boolean_eval.contains(retired),
            "VCG lookup restored fail-open compatibility behavior: {retired}"
        );
    }
    assert!(boolean_eval.contains("MissingVcgProof"));
    assert!(boolean_eval.contains("InvalidConnectionEvidence"));

    let occurrence = fs::read_to_string(
        root.join("crates/rumoca-ir-ast/src/instance/equality_constraint/occurrence.rs"),
    )
    .expect("read finalized occurrence catalog");
    assert!(occurrence.contains("Foreign,"));
    assert!(occurrence.contains("Ordinary,"));
    assert!(occurrence.contains("Record(FinalizedOverconstrainedRecord"));
    assert!(!occurrence.contains(") -> Option<FinalizedOverconstrainedComponent<'_>>"));
}

#[test]
fn instantiation_outcome_cannot_be_collapsed_back_into_a_result_facade() {
    let root = workspace_root();
    let entry = fs::read_to_string(root.join("crates/rumoca-phase-instantiate/src/entry.rs"))
        .expect("read instantiation entry points");
    let errors = fs::read_to_string(root.join("crates/rumoca-phase-instantiate/src/errors.rs"))
        .expect("read instantiation outcomes");
    let facade = fs::read_to_string(root.join("crates/rumoca-phase-instantiate/src/lib.rs"))
        .expect("read instantiation facade");

    for retired in [
        "pub fn instantiate(",
        "pub fn instantiate_with_options(",
        "pub fn instantiate_model(",
        "pub fn instantiate_model_with_options(",
        "pub fn into_result(",
        "pub fn is_error(&self)",
        "pub fn overlay(&self)",
        "old behavior of treating missing inners as errors",
    ] {
        assert!(
            !entry.contains(retired) && !errors.contains(retired),
            "retired Result-collapsing instantiation API returned: {retired}"
        );
    }
    assert!(entry.contains("pub fn instantiate_model_with_outcome("));
    assert!(entry.contains("pub fn instantiate_model_with_outcome_options("));
    assert!(
        errors.contains("#[must_use = \"instantiation outcomes must be handled exhaustively\"]")
    );
    assert!(errors.contains("pub(crate) type InstantiateResult"));
    assert!(!errors.contains("pub type InstantiateResult"));
    assert!(!facade.contains("pub use errors::{InstantiateError, InstantiateResult"));
}

#[test]
fn synthetic_inner_retry_cannot_discard_or_reclassify_phase_errors() {
    let root = workspace_root();
    let entry = fs::read_to_string(root.join("crates/rumoca-phase-instantiate/src/entry.rs"))
        .expect("read instantiation entry points");
    let retry = fs::read_to_string(root.join("crates/rumoca-phase-instantiate/src/inner_outer.rs"))
        .expect("read synthetic inner retry owner");
    let context = fs::read_to_string(root.join("crates/rumoca-phase-instantiate/src/lib.rs"))
        .expect("read instantiation context");

    for retired in [
        "InstantiationFailed",
        "SourceContext",
        ".is_err()",
        "fall back to original NeedsInner",
        "pub fn missing_inner_names",
        "public API compatibility",
    ] {
        assert!(
            !entry.contains(retired) && !retry.contains(retired) && !context.contains(retired),
            "synthetic-inner error discard or compatibility surface returned: {retired}"
        );
    }
    assert!(retry.contains(".map_err(SyntheticInnerError::Error)?"));
    assert!(
        entry.contains(
            "Err(SyntheticInnerError::Error(error)) => InstantiationOutcome::Error(error)"
        )
    );
    assert!(context.contains("fn unique_missing_inner_summary(&self)"));
}
