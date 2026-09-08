//! Locks mandatory source-occurrence identity to one independently checked DAE-to-Solve cut.

use std::collections::BTreeSet;
use std::fs;

use syn::visit::{self, Visit};

use super::architecture_hardening_support::{
    attributes_require_test, production_rust_sources, workspace_root,
};

const CORE_PATH: &str = "crates/rumoca-core/src/ir_primitives.rs";
const DAE_PATH: &str = "crates/rumoca-ir-dae/src/model.rs";
const DAE_VIEW_PATH: &str = "crates/rumoca-ir-dae/src/model/view.rs";
const SOLVE_CATALOG_PATH: &str = "crates/rumoca-ir-solve/src/variable_catalog.rs";
const CHECKER_PATH: &str = "crates/rumoca-phase-solve/src/variable_catalog_refinement.rs";
const LOWERING_PATH: &str = "crates/rumoca-phase-solve/src/model_values.rs";
const WIRE_PATH: &str = "crates/rumoca-phase-solve/src/model_wire.rs";
const RECEIPT: &str = "CheckedDaeSolveVariableCatalogRefinement";
const TRANSFER_MAP: &str = "VariableCatalogTransferMap";
const LEGACY_IDENTITY: &str = "SolveVariableSourceIdentity";
const CHECKER: &str = "check_variable_catalog_refinement";
const FACT_CHECKER: &str = "check_projected_variable_catalog_refinement";

#[test]
fn raw_fact_checker_has_only_the_closed_view_caller() {
    let root = workspace_root();
    let mut uses = Vec::new();
    for (path, source) in production_rust_sources(&root.join("crates/rumoca-phase-solve"), &root) {
        let syntax = parse(&source, &path.display().to_string());
        for owner in raw_fact_checker_uses(&syntax) {
            uses.push(format!("{}:{owner}", path.display()));
        }
    }
    assert_eq!(uses, [format!("{CHECKER_PATH}:{CHECKER}")]);
    let sources = production_sources();
    let syntax = parse(&sources.checker, CHECKER_PATH);
    assert!(matches!(
        find_function(&syntax, FACT_CHECKER)
            .expect("private fact checker exists")
            .vis,
        syn::Visibility::Inherited
    ));
}

#[test]
fn raw_fact_checker_census_detects_second_and_aliased_callers() {
    for body in [
        "check_projected_variable_catalog_refinement(dae, solve, mapping)",
        "{ let alias = check_projected_variable_catalog_refinement; alias(dae, solve, mapping) }",
    ] {
        let mut source = production_sources().checker;
        source.push_str(&format!("\nfn second_caller() {{ {body}; }}\n"));
        assert_eq!(
            raw_fact_checker_uses(&parse(&source, CHECKER_PATH)),
            [CHECKER, "second_caller"]
        );
    }
    let mut source = production_sources().checker;
    source.push_str(
        "\n#[cfg(all(test, not(kani)))] mod test_only { fn fault_injection() { check_projected_variable_catalog_refinement(a, b, c); } }\n",
    );
    assert_eq!(
        raw_fact_checker_uses(&parse(&source, CHECKER_PATH)),
        [CHECKER]
    );
}

fn raw_fact_checker_uses(syntax: &syn::File) -> Vec<String> {
    let mut visitor = RawFactCheckerUseVisitor {
        owner: None,
        uses: Vec::new(),
    };
    visitor.visit_file(syntax);
    visitor.uses
}

struct RawFactCheckerUseVisitor {
    owner: Option<String>,
    uses: Vec<String>,
}

impl Visit<'_> for RawFactCheckerUseVisitor {
    fn visit_item_mod(&mut self, item: &syn::ItemMod) {
        if !attributes_require_test(&item.attrs) {
            visit::visit_item_mod(self, item);
        }
    }

    fn visit_item_fn(&mut self, function: &syn::ItemFn) {
        if !attributes_require_test(&function.attrs) {
            let previous = self.owner.replace(function.sig.ident.to_string());
            self.visit_block(&function.block);
            self.owner = previous;
        }
    }

    fn visit_impl_item_fn(&mut self, function: &syn::ImplItemFn) {
        if !attributes_require_test(&function.attrs) {
            let previous = self.owner.replace(function.sig.ident.to_string());
            self.visit_block(&function.block);
            self.owner = previous;
        }
    }

    fn visit_ident(&mut self, ident: &syn::Ident) {
        if identifier_is(ident, FACT_CHECKER) {
            self.uses.push(
                self.owner
                    .as_deref()
                    .unwrap_or("outside-function")
                    .to_owned(),
            );
        }
    }

    fn visit_macro(&mut self, item: &syn::Macro) {
        if token_stream_mentions_identifier(&item.tokens, FACT_CHECKER) {
            self.uses.push("macro".to_owned());
        }
    }
}

#[derive(Clone)]
struct BoundarySources {
    core: String,
    dae: String,
    dae_view: String,
    solve_catalog: String,
    checker: String,
    lowering: String,
    wire: String,
}

#[test]
fn mandatory_occurrence_refinement_boundary_is_sealed() {
    let sources = production_sources();
    let mut violations = boundary_violations(&sources);
    let mut restricted_receipt_mints = Vec::new();
    let mut receipt_literals = Vec::new();
    let mut receipt_aliases = Vec::new();
    let mut lowered_solve_model_literals = Vec::new();

    let root = workspace_root();
    for crate_name in ["rumoca-ir-solve", "rumoca-phase-solve"] {
        for (path, source) in production_rust_sources(&root.join("crates").join(crate_name), &root)
        {
            let syntax = parse(&source, &path.display().to_string());
            if syntax_mentions_identifier_in_file(&syntax, LEGACY_IDENTITY) {
                violations.insert(format!(
                    "legacy-solve-variable-source-identity:{}",
                    path.display()
                ));
            }
            if crate_name == "rumoca-phase-solve" {
                check_cross_file_capability_escapes(&syntax, &path, &mut violations);
                collect_restricted_mints(
                    &syntax,
                    &path.display().to_string(),
                    RECEIPT,
                    &mut restricted_receipt_mints,
                );
                collect_owned_struct_literals(
                    &syntax,
                    &path.display().to_string(),
                    RECEIPT,
                    &mut receipt_literals,
                );
                collect_receipt_aliases(
                    &syntax,
                    &path.display().to_string(),
                    RECEIPT,
                    &mut receipt_aliases,
                );
                collect_struct_literals(
                    &syntax,
                    &path.display().to_string(),
                    "LoweredSolveModel",
                    &mut lowered_solve_model_literals,
                );
            }
        }
    }
    check_restricted_receipt_mint_catalog(&restricted_receipt_mints, &mut violations);
    check_receipt_literal_catalog(&receipt_literals, &mut violations);
    check_receipt_alias_catalog(&receipt_aliases, &mut violations);
    if lowered_solve_model_literals.len() != 1
        || !lowered_solve_model_literals[0].ends_with(LOWERING_PATH)
    {
        violations.insert(format!(
            "lowered-solve-model-literal-catalog:{lowered_solve_model_literals:?}"
        ));
    }
    assert!(
        violations.is_empty(),
        "mandatory occurrence/refinement boundary drifted: {violations:#?}"
    );
}

fn check_cross_file_capability_escapes(
    syntax: &syn::File,
    path: &std::path::Path,
    violations: &mut BTreeSet<String>,
) {
    for type_name in [RECEIPT, TRANSFER_MAP] {
        for trait_name in implemented_forbidden_traits(
            syntax,
            type_name,
            &["Clone", "Default", "Serialize", "Deserialize"],
        ) {
            violations.insert(format!(
                "{type_name}-forbidden-trait:{trait_name}:{}",
                path.display()
            ));
        }
        let mut public_mints = PublicMintVisitor {
            target: type_name,
            found: false,
        };
        public_mints.visit_file(syntax);
        if public_mints.found {
            violations.insert(format!("{type_name}-public-mint:{}", path.display()));
        }
    }
}

#[test]
fn mutations_detect_optional_or_fabricable_source_occurrence() {
    let sources = production_sources();

    let mut public_field = sources.clone();
    public_field.core = replace_once(
        &public_field.core,
        "pub struct SourceOccurrenceId(NonZeroU32);",
        "pub struct SourceOccurrenceId(pub NonZeroU32);",
    );
    assert_violation(&public_field, "source-occurrence-field-not-private");

    let mut default_identity = sources.clone();
    default_identity.core = replace_once(
        &default_identity.core,
        "#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]",
        "#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]",
    );
    assert_violation(
        &default_identity,
        "SourceOccurrenceId-forbidden-trait:Default",
    );

    let mut optional_dae_identity = sources.clone();
    optional_dae_identity.dae = replace_once(
        &optional_dae_identity.dae,
        "pub(crate) source_occurrence: SourceOccurrenceId,",
        "pub(crate) source_occurrence: Option<SourceOccurrenceId>,",
    );
    assert_violation(
        &optional_dae_identity,
        "dae-variable-source-occurrence-not-mandatory",
    );

    let mut legacy = sources.clone();
    legacy
        .solve_catalog
        .push_str("\ntype SolveVariableSourceIdentity = ();\n");
    assert_violation(&legacy, "legacy-solve-variable-source-identity");
}

/// Mutations of the DAE refinement entry's public surface and field set.
fn dae_refinement_entry_mutations_are_detected(sources: &BoundarySources) {
    let mut broad_dae_input = sources.clone();
    broad_dae_input.checker = replace_once(
        &broad_dae_input.checker,
        "dae: dae::DaeVariableRefinementView,",
        "dae: dae::DaeView<'_>,",
    );
    assert_violation(&broad_dae_input, "refinement-checker-inputs-not-restricted");

    let mut leaked_dae_name = sources.clone();
    leaked_dae_name.dae_view.push_str(
        "\nimpl DaeVariableRefinementEntry { pub fn name(&self) -> &'static str { \"x\" } }\n",
    );
    assert_violation(
        &leaked_dae_name,
        "DaeVariableRefinementEntry-public-method-surface",
    );

    let mut repurposed_dimensions = sources.clone();
    repurposed_dimensions.dae_view = replace_once(
        &repurposed_dimensions.dae_view,
        "pub fn dimensions(&self) -> &[u32] {",
        "pub fn dimensions(&self) -> &str {",
    );
    assert_violation(
        &repurposed_dimensions,
        "DaeVariableRefinementEntry-public-method-signature",
    );

    for (anchor, without_field, original_type) in [
        (
            "    fixed: rumoca_core::Fixity,\n    variability: ExpressionVariability,",
            "    variability: ExpressionVariability,",
            "rumoca_core::Fixity",
        ),
        (
            "    variability: ExpressionVariability,\n    is_tunable: bool,",
            "    is_tunable: bool,",
            "ExpressionVariability",
        ),
        (
            "    is_tunable: bool,\n    causality: VariableCausality,",
            "    causality: VariableCausality,",
            "bool",
        ),
    ] {
        let mut omitted = sources.clone();
        omitted.dae_view = replace_once(&omitted.dae_view, anchor, without_field);
        assert_violation(&omitted, "DaeVariableRefinementEntry-field-surface");

        let mut repurposed = sources.clone();
        repurposed.dae_view = replace_once(
            &repurposed.dae_view,
            anchor,
            &anchor.replacen(original_type, "String", 1),
        );
        assert_violation(&repurposed, "DaeVariableRefinementEntry-field-signature");
    }
}

#[test]
fn solve_refinement_slice_surface_rejects_widening_and_legacy_accessors() {
    assert_refinement_slice_surface(
        "SolveVariableRefinementView",
        "SolveVariableRefinementEntry",
        "SolveVariableCatalogEntry",
        "entry",
        |sources| &mut sources.solve_catalog,
    );
}

#[test]
fn dae_refinement_slice_surface_rejects_widening_and_legacy_accessors() {
    assert_refinement_slice_surface(
        "DaeVariableRefinementView",
        "DaeVariableRefinementEntry",
        "Variable",
        "variable",
        |sources| &mut sources.dae_view,
    );
}

fn assert_refinement_slice_surface(
    owner: &str,
    entry: &str,
    wider_entry: &str,
    legacy_index: &str,
    select: fn(&mut BoundarySources) -> &mut String,
) {
    let sources = production_sources();
    let mut missed = Vec::new();
    let valid_signature = format!("pub fn entries(&self) -> &[{entry}]");
    for signature in [
        format!("pub fn entries(&self) -> &[{wider_entry}]"),
        format!("pub fn entries(&mut self) -> &[{entry}]"),
        format!("pub fn entries(&self) -> &mut [{entry}]"),
        format!("pub fn entries(&mut self) -> &mut [{entry}]"),
        format!("pub fn entries(&self, foreign: &Self) -> &[{entry}]"),
    ] {
        let mut widened = sources.clone();
        let source = select(&mut widened);
        *source = replace_once(source, &valid_signature, &signature);
        if !boundary_violations(&widened).contains(&format!("{owner}-public-method-signature")) {
            missed.push(signature);
        }
    }
    assert!(
        missed.is_empty(),
        "unrejected slice signatures: {missed:#?}"
    );

    for method in [
        "pub fn variable_count(&self) -> usize { self.entries.len() }".to_owned(),
        format!(
            "pub fn {legacy_index}(&self, ordinal: usize) -> Option<&{entry}> {{ self.entries.get(ordinal) }}"
        ),
    ] {
        let mut legacy = sources.clone();
        select(&mut legacy).push_str(&format!("\nimpl {owner} {{ {method} }}\n"));
        assert_violation(&legacy, &format!("{owner}-public-method-surface"));
    }
}

#[test]
fn refinement_dimension_slices_reject_mutable_borrows() {
    let sources = production_sources();
    let mut dae = sources.clone();
    dae.dae_view = replace_once(
        &dae.dae_view,
        "pub fn dimensions(&self) -> &[u32]",
        "pub fn dimensions(&self) -> &mut [u32]",
    );
    let mut solve = sources;
    solve.solve_catalog = replace_once(
        &solve.solve_catalog,
        "pub fn dimensions(&self) -> &[u32] {\n        std::ops::Deref::deref(&self.dimensions)",
        "pub fn dimensions(&self) -> &mut [u32] {\n        std::ops::Deref::deref(&self.dimensions)",
    );
    let missed = [
        (dae, "DaeVariableRefinementEntry-public-method-signature"),
        (
            solve,
            "SolveVariableRefinementEntry-public-method-signature",
        ),
    ]
    .into_iter()
    .filter_map(|(mutated, violation)| {
        (!boundary_violations(&mutated).contains(violation)).then_some(violation)
    })
    .collect::<Vec<_>>();
    assert!(
        missed.is_empty(),
        "unrejected mutable dimensions: {missed:#?}"
    );
}

/// Mutations of the Solve refinement entry's field set and public surface.
fn solve_refinement_entry_mutations_are_detected(sources: &BoundarySources) {
    for (anchor, without_field, original_type) in [
        (
            "    fixed: Fixity,\n    state_initialization: SolveStateInitialization,",
            "    state_initialization: SolveStateInitialization,",
            "Fixity",
        ),
        (
            "    state_initialization: SolveStateInitialization,\n    variability: SolveVariableVariability,",
            "    variability: SolveVariableVariability,",
            "SolveStateInitialization",
        ),
        (
            "    state_initialization: SolveStateInitialization,\n    variability: SolveVariableVariability,\n    tunable: bool,",
            "    state_initialization: SolveStateInitialization,\n    tunable: bool,",
            "SolveVariableVariability",
        ),
        (
            "    tunable: bool,\n    causality: SolveVariableCausality,",
            "    causality: SolveVariableCausality,",
            "bool",
        ),
    ] {
        let mut omitted = sources.clone();
        omitted.solve_catalog = replace_once(&omitted.solve_catalog, anchor, without_field);
        assert_violation(&omitted, "SolveVariableRefinementEntry-field-surface");

        let mut repurposed = sources.clone();
        repurposed.solve_catalog = replace_once(
            &repurposed.solve_catalog,
            anchor,
            &anchor.replacen(original_type, "String", 1),
        );
        assert_violation(&repurposed, "SolveVariableRefinementEntry-field-signature");
    }

    let mut leaked_solve_provenance = sources.clone();
    leaked_solve_provenance.solve_catalog.push_str(
        "\nimpl SolveVariableRefinementEntry { pub fn provenance(&self) -> Span { todo!() } }\n",
    );
    assert_violation(
        &leaked_solve_provenance,
        "SolveVariableRefinementEntry-public-method-surface",
    );

    let mut retained_full_solve_entry = sources.clone();
    retained_full_solve_entry.solve_catalog = replace_once(
        &retained_full_solve_entry.solve_catalog,
        "pub struct SolveVariableRefinementEntry {",
        "pub struct SolveVariableRefinementEntry {\n    entry: SolveVariableCatalogEntry,",
    );
    assert_violation(
        &retained_full_solve_entry,
        "SolveVariableRefinementEntry-field-surface",
    );

    let mut retained_equation_domain = sources.clone();
    retained_equation_domain.solve_catalog = replace_once(
        &retained_equation_domain.solve_catalog,
        "    value_kind: SolveVariableValueKind,\n    storage: SolveVariableStorageRun,",
        "    value_kind: SolveVariableValueKind,\n    time_domain: SolveVariableTimeDomain,\n    storage: SolveVariableStorageRun,",
    );
    assert_violation(
        &retained_equation_domain,
        "SolveVariableRefinementEntry-field-surface",
    );

    let mut restored_whole_declaration = sources.clone();
    restored_whole_declaration.solve_catalog = replace_once(
        &restored_whole_declaration.solve_catalog,
        "    role: SolveVariableStorageRole,\n    value_kind: SolveVariableValueKind,",
        "    declaration: SolveVariableDeclaration,",
    );
    assert_violation(
        &restored_whole_declaration,
        "SolveVariableRefinementEntry-field-surface",
    );
}

/// Mutations that would let the refinement view retain the whole DAE.
fn dae_refinement_view_surface_mutations_are_detected(sources: &BoundarySources) {
    let mut retained_full_dae_view = sources.clone();
    retained_full_dae_view.dae_view = replace_once(
        &retained_full_dae_view.dae_view,
        "entries: Box<[DaeVariableRefinementEntry]>,",
        "entries: Box<[DaeVariableRefinementEntry]>,\n    full_dae: Option<DaeView<'static>>,",
    );
    assert_violation(
        &retained_full_dae_view,
        "DaeVariableRefinementView-field-surface",
    );

    let mut dae_view_deref = sources.clone();
    dae_view_deref.dae_view.push_str(
        "\nimpl std::ops::Deref for DaeVariableRefinementView { type Target = [DaeVariableRefinementEntry]; fn deref(&self) -> &Self::Target { &self.entries } }\n",
    );
    assert_violation(
        &dae_view_deref,
        "DaeVariableRefinementView-trait-impl:Deref",
    );
}

/// Mutations of how the refinement check is propagated and how often it runs.
fn refinement_check_propagation_mutations_are_detected(sources: &BoundarySources) {
    let mut unpropagated = sources.clone();
    unpropagated.lowering = replace_once(
        &unpropagated.lowering,
        ".map_err(|error| variable_catalog_refinement_error(context.prepared.as_dae(), error))?;",
        ".map_err(|error| variable_catalog_refinement_error(context.prepared.as_dae(), error));",
    );
    assert_violation(&unpropagated, "refinement-check-propagated-count:0");

    let mut repeated = sources.clone();
    let check_call = "                check_variable_catalog_refinement(\n                    view.variable_refinement(),\n                    model.variable_refinement(),\n                    mapping,\n                )";
    repeated.lowering = replace_once(
        &repeated.lowering,
        check_call,
        &format!("{check_call};\n{check_call}"),
    );
    assert_violation(&repeated, "refinement-check-call-count:2");

    let mut aliased_repetition = sources.clone();
    aliased_repetition.lowering = replace_once(
        &aliased_repetition.lowering,
        check_call,
        &format!(
            "                let aliased_check = check_variable_catalog_refinement;\n                let _aliased_result = aliased_check(view.variable_refinement(), model.variable_refinement(), mapping);\n{check_call}"
        ),
    );
    assert_violation(&aliased_repetition, "refinement-check-identifier-count:2");
}

#[test]
fn mutations_detect_projection_leaks_and_unpropagated_repetition() {
    let sources = production_sources();

    dae_refinement_entry_mutations_are_detected(&sources);
    solve_refinement_entry_mutations_are_detected(&sources);
    dae_refinement_view_surface_mutations_are_detected(&sources);
    refinement_check_propagation_mutations_are_detected(&sources);
}

#[test]
fn mutations_detect_receipt_or_transfer_map_fabrication() {
    let sources = production_sources();

    let mut optional_receipt = sources.clone();
    optional_receipt.lowering = replace_once(
        &optional_receipt.lowering,
        "_variable_catalog_refinement: CheckedDaeSolveVariableCatalogRefinement,",
        "_variable_catalog_refinement: Option<CheckedDaeSolveVariableCatalogRefinement>,",
    );
    assert_violation(
        &optional_receipt,
        "lowered-solve-model-receipt-not-mandatory",
    );

    let mut mutable_receipt_binding = sources.clone();
    mutable_receipt_binding.lowering = replace_once(
        &mutable_receipt_binding.lowering,
        "let variable_catalog_refinement = context",
        "let mut variable_catalog_refinement = context",
    );
    assert_violation(
        &mutable_receipt_binding,
        "refinement-check-propagated-binding-mutable",
    );

    for (type_name, anchor) in [
        (
            RECEIPT,
            "pub(crate) struct CheckedDaeSolveVariableCatalogRefinement {",
        ),
        (
            TRANSFER_MAP,
            "pub(crate) struct VariableCatalogTransferMap {",
        ),
    ] {
        for forbidden_trait in ["Clone", "Default", "Serialize", "Deserialize"] {
            let mut derivable = sources.clone();
            derivable.checker = replace_once(
                &derivable.checker,
                anchor,
                &format!("#[derive({forbidden_trait})]\n{anchor}"),
            );
            assert_violation(
                &derivable,
                &format!("{type_name}-forbidden-trait:{forbidden_trait}"),
            );
        }

        let mut manual_default = sources.clone();
        manual_default.checker.push_str(&format!(
            "\nimpl Default for {type_name} {{ fn default() -> Self {{ todo!() }} }}\n"
        ));
        assert_violation(
            &manual_default,
            &format!("{type_name}-forbidden-trait:Default"),
        );

        let mut public_mint = sources.clone();
        public_mint.checker.push_str(&format!(
            "\npub fn forge_{type_name}() -> {type_name} {{ todo!() }}\n"
        ));
        assert_violation(&public_mint, &format!("{type_name}-public-mint"));
    }

    let mut ignored_real_check = sources.clone();
    ignored_real_check.checker.push_str(&format!(
        "\npub(crate) fn forge_receipt() -> {RECEIPT} {{ {RECEIPT} {{ _private: () }} }}\n"
    ));
    ignored_real_check.lowering = replace_once(
        &ignored_real_check.lowering,
        "            variable_catalog_refinement,\n        })",
        "            variable_catalog_refinement: crate::variable_catalog_refinement::forge_receipt(),\n        })",
    );
    assert_violation(
        &ignored_real_check,
        &format!("{RECEIPT}-restricted-mint-count:2"),
    );
    assert_violation(
        &ignored_real_check,
        "lowered-solve-model-receipt-not-bound-to-propagated-check",
    );

    let mut generic_forge_and_shadow = sources.clone();
    generic_forge_and_shadow.checker.push_str(&format!(
        "\npub(crate) fn with_forged_receipt<R>(f: impl FnOnce({RECEIPT}) -> R) -> R {{ f({RECEIPT} {{ _private: () }}) }}\n"
    ));
    generic_forge_and_shadow.lowering = replace_once(
        &generic_forge_and_shadow.lowering,
        ".map_err(|error| variable_catalog_refinement_error(context.prepared.as_dae(), error))?;",
        ".map_err(|error| variable_catalog_refinement_error(context.prepared.as_dae(), error))?;\n        let variable_catalog_refinement = crate::variable_catalog_refinement::with_forged_receipt(|receipt| receipt);",
    );
    assert_violation(
        &generic_forge_and_shadow,
        &format!("{RECEIPT}-struct-literal-count:2"),
    );
    assert_violation(
        &generic_forge_and_shadow,
        "refinement-check-propagated-binding-definition-count:2",
    );

    let mut aliased_generic_forge = sources.clone();
    aliased_generic_forge.checker.push_str(&format!(
        "\ntype ForgedReceiptAlias = {RECEIPT};\npub(crate) fn with_aliased_forged_receipt<R>(f: impl FnOnce({RECEIPT}) -> R) -> R {{ f(ForgedReceiptAlias {{ _private: () }}) }}\n"
    ));
    assert_violation(&aliased_generic_forge, &format!("{RECEIPT}-alias-count:1"));
}

#[test]
fn mutations_detect_wire_identity_omission_and_schema_drift() {
    let sources = production_sources();

    for (owner, declaration) in [
        (
            "SolveVariableCatalogEntryWireRef",
            "struct SolveVariableCatalogEntryWireRef<'entry> {",
        ),
        (
            "SolveVariableCatalogEntryWire",
            "struct SolveVariableCatalogEntryWire {",
        ),
    ] {
        let mut optional_wire_identity = sources.clone();
        let anchor =
            format!("{declaration}\n    source_occurrence: rumoca_core::SourceOccurrenceId,");
        let replacement = format!(
            "{declaration}\n    source_occurrence: Option<rumoca_core::SourceOccurrenceId>,"
        );
        optional_wire_identity.wire =
            replace_once(&optional_wire_identity.wire, &anchor, &replacement);
        assert_violation(
            &optional_wire_identity,
            &format!("{owner}-source-occurrence-not-mandatory"),
        );
    }

    let mut old_schema = sources.clone();
    old_schema.wire = replace_once(
        &old_schema.wire,
        "pub const SOLVE_MODEL_SCHEMA_VERSION: u16 = 5;",
        "pub const SOLVE_MODEL_SCHEMA_VERSION: u16 = 4;",
    );
    assert_violation(&old_schema, "solve-model-wire-schema-not-5");
}

fn production_sources() -> BoundarySources {
    let root = workspace_root();
    let read = |relative: &str| {
        fs::read_to_string(root.join(relative))
            .unwrap_or_else(|error| panic!("read {relative}: {error}"))
    };
    BoundarySources {
        core: read(CORE_PATH),
        dae: read(DAE_PATH),
        dae_view: read(DAE_VIEW_PATH),
        solve_catalog: read(SOLVE_CATALOG_PATH),
        checker: read(CHECKER_PATH),
        lowering: read(LOWERING_PATH),
        wire: read(WIRE_PATH),
    }
}

fn boundary_violations(sources: &BoundarySources) -> BTreeSet<String> {
    let core = parse(&sources.core, CORE_PATH);
    let dae = parse(&sources.dae, DAE_PATH);
    let dae_view = parse(&sources.dae_view, DAE_VIEW_PATH);
    let solve_catalog = parse(&sources.solve_catalog, SOLVE_CATALOG_PATH);
    let checker = parse(&sources.checker, CHECKER_PATH);
    let lowering = parse(&sources.lowering, LOWERING_PATH);
    let wire = parse(&sources.wire, WIRE_PATH);
    let mut violations = BTreeSet::new();

    check_source_occurrence(&core, &mut violations);
    check_mandatory_field(
        &dae,
        "VariableEntry",
        "source_occurrence",
        "SourceOccurrenceId",
        "dae-variable-source-occurrence-not-mandatory",
        &mut violations,
    );
    for syntax in [
        &core,
        &dae,
        &dae_view,
        &solve_catalog,
        &checker,
        &lowering,
        &wire,
    ] {
        if syntax_mentions_identifier_in_file(syntax, LEGACY_IDENTITY) {
            violations.insert("legacy-solve-variable-source-identity".to_owned());
        }
    }
    check_refinement_projection_surfaces(&dae_view, &solve_catalog, &mut violations);
    check_checker(&checker, &mut violations);
    check_lowering(&lowering, &mut violations);
    check_capability_type(&checker, RECEIPT, &mut violations);
    check_capability_type(&checker, TRANSFER_MAP, &mut violations);
    let mut restricted_receipt_mints = Vec::new();
    collect_restricted_mints(
        &checker,
        CHECKER_PATH,
        RECEIPT,
        &mut restricted_receipt_mints,
    );
    check_restricted_receipt_mint_catalog(&restricted_receipt_mints, &mut violations);
    let mut receipt_literals = Vec::new();
    collect_owned_struct_literals(&checker, CHECKER_PATH, RECEIPT, &mut receipt_literals);
    check_receipt_literal_catalog(&receipt_literals, &mut violations);
    let mut receipt_aliases = Vec::new();
    collect_receipt_aliases(&checker, CHECKER_PATH, RECEIPT, &mut receipt_aliases);
    check_receipt_alias_catalog(&receipt_aliases, &mut violations);
    check_wire(&wire, &mut violations);

    violations
}

fn check_source_occurrence(syntax: &syn::File, violations: &mut BTreeSet<String>) {
    let Some(item) = find_struct(syntax, "SourceOccurrenceId") else {
        violations.insert("source-occurrence-missing".to_owned());
        return;
    };
    let valid_private_field = matches!(&item.fields, syn::Fields::Unnamed(fields)
        if fields.unnamed.len() == 1
            && matches!(fields.unnamed[0].vis, syn::Visibility::Inherited)
            && type_is_direct_named(&fields.unnamed[0].ty, "NonZeroU32"));
    if !valid_private_field {
        violations.insert("source-occurrence-field-not-private".to_owned());
    }
    check_forbidden_traits(syntax, item, "SourceOccurrenceId", &["Default"], violations);
}

fn check_mandatory_field(
    syntax: &syn::File,
    owner: &str,
    field_name: &str,
    field_type: &str,
    violation: &str,
    violations: &mut BTreeSet<String>,
) {
    let valid = find_struct(syntax, owner)
        .and_then(|item| named_field(item, field_name))
        .is_some_and(|field| {
            type_is_direct_named(&field.ty, field_type) && !serde_field_has_default(field)
        });
    if !valid {
        violations.insert(violation.to_owned());
    }
}

fn check_checker(syntax: &syn::File, violations: &mut BTreeSet<String>) {
    let Some(function) = find_function(syntax, CHECKER) else {
        violations.insert("refinement-checker-missing".to_owned());
        return;
    };
    if !return_type_contains(&function.sig.output, "Result") {
        violations.insert("refinement-checker-not-fallible".to_owned());
    }
    let expected_inputs = [
        "DaeVariableRefinementView",
        "SolveVariableRefinementView",
        TRANSFER_MAP,
    ];
    let inputs_are_restricted = function.sig.inputs.len() == expected_inputs.len()
        && function
            .sig
            .inputs
            .iter()
            .zip(expected_inputs)
            .all(|(input, expected)| {
                matches!(input, syn::FnArg::Typed(argument)
                    if type_is_path_named(&argument.ty, expected))
            });
    if !inputs_are_restricted {
        violations.insert("refinement-checker-inputs-not-restricted".to_owned());
    }
}

fn check_refinement_projection_surfaces(
    dae: &syn::File,
    solve: &syn::File,
    violations: &mut BTreeSet<String>,
) {
    check_projection_fields(dae, "DaeVariableRefinementView", violations);
    check_projection_fields(dae, "DaeVariableRefinementEntry", violations);
    check_projection_fields(solve, "SolveVariableRefinementView", violations);
    check_projection_fields(solve, "SolveVariableRefinementEntry", violations);
    check_projection_type(dae, "DaeVariableRefinementView", &["entries"], violations);
    check_projection_type(
        dae,
        "DaeVariableRefinementEntry",
        &[
            "causality",
            "dimensions",
            "fixed",
            "is_tunable",
            "role",
            "scalar_count",
            "scalar_type",
            "source_occurrence",
            "variability",
        ],
        violations,
    );
    check_projection_type(
        solve,
        "SolveVariableRefinementView",
        &["entries"],
        violations,
    );
    check_projection_type(
        solve,
        "SolveVariableRefinementEntry",
        &[
            "causality",
            "dimensions",
            "fixed",
            "role",
            "source_occurrence",
            "state_initialization",
            "storage",
            "tunable",
            "value_kind",
            "variability",
        ],
        violations,
    );
}

fn check_projection_fields(syntax: &syn::File, type_name: &str, violations: &mut BTreeSet<String>) {
    let expected_names = match type_name {
        "DaeVariableRefinementView" | "SolveVariableRefinementView" => ["entries"].as_slice(),
        "DaeVariableRefinementEntry" => [
            "causality",
            "dimensions",
            "fixed",
            "is_tunable",
            "role",
            "scalar_count",
            "scalar_type",
            "source_occurrence",
            "variability",
        ]
        .as_slice(),
        "SolveVariableRefinementEntry" => [
            "causality",
            "dimensions",
            "fixed",
            "role",
            "source_occurrence",
            "state_initialization",
            "storage",
            "tunable",
            "value_kind",
            "variability",
        ]
        .as_slice(),
        _ => unreachable!("projection field catalog is closed"),
    };
    let Some(item) = find_struct(syntax, type_name) else {
        violations.insert(format!("{type_name}-field-surface"));
        return;
    };
    let syn::Fields::Named(fields) = &item.fields else {
        violations.insert(format!("{type_name}-field-surface"));
        return;
    };
    let mut actual_names = fields
        .named
        .iter()
        .filter_map(|field| field.ident.as_ref())
        .map(ToString::to_string)
        .collect::<Vec<_>>();
    actual_names.sort();
    if actual_names.len() != expected_names.len()
        || actual_names
            .iter()
            .map(String::as_str)
            .ne(expected_names.iter().copied())
    {
        violations.insert(format!("{type_name}-field-surface"));
        return;
    }
    if fields.named.iter().any(|field| {
        let name = field
            .ident
            .as_ref()
            .expect("named projection field")
            .to_string();
        !projection_field_type_is_exact(type_name, &name, &field.ty)
    }) {
        violations.insert(format!("{type_name}-field-signature"));
    }
}

fn projection_field_type_is_exact(owner: &str, field: &str, ty: &syn::Type) -> bool {
    match (owner, field) {
        ("DaeVariableRefinementView", "entries") => {
            type_is_boxed_slice_of(ty, "DaeVariableRefinementEntry")
        }
        ("SolveVariableRefinementView", "entries") => {
            type_is_boxed_slice_of(ty, "SolveVariableRefinementEntry")
        }
        ("DaeVariableRefinementEntry", "source_occurrence")
        | ("SolveVariableRefinementEntry", "source_occurrence") => {
            type_is_direct_named(ty, "SourceOccurrenceId")
        }
        ("DaeVariableRefinementEntry", "role") => type_is_direct_named(ty, "VariableRole"),
        ("DaeVariableRefinementEntry", "fixed") => type_is_direct_named(ty, "Fixity"),
        ("DaeVariableRefinementEntry", "variability") => {
            type_is_direct_named(ty, "ExpressionVariability")
        }
        ("DaeVariableRefinementEntry", "is_tunable") => type_is_direct_named(ty, "bool"),
        ("DaeVariableRefinementEntry", "causality") => {
            type_is_direct_named(ty, "VariableCausality")
        }
        ("DaeVariableRefinementEntry", "scalar_type") => type_is_direct_named(ty, "ScalarType"),
        ("DaeVariableRefinementEntry", "scalar_count") => type_is_direct_named(ty, "usize"),
        ("DaeVariableRefinementEntry", "dimensions")
        | ("SolveVariableRefinementEntry", "dimensions") => type_is_boxed_slice_of(ty, "u32"),
        ("SolveVariableRefinementEntry", "causality") => {
            type_is_direct_named(ty, "SolveVariableCausality")
        }
        ("SolveVariableRefinementEntry", "fixed") => type_is_direct_named(ty, "Fixity"),
        ("SolveVariableRefinementEntry", "state_initialization") => {
            type_is_direct_named(ty, "SolveStateInitialization")
        }
        ("SolveVariableRefinementEntry", "variability") => {
            type_is_direct_named(ty, "SolveVariableVariability")
        }
        ("SolveVariableRefinementEntry", "tunable") => type_is_direct_named(ty, "bool"),
        ("SolveVariableRefinementEntry", "role") => {
            type_is_direct_named(ty, "SolveVariableStorageRole")
        }
        ("SolveVariableRefinementEntry", "value_kind") => {
            type_is_direct_named(ty, "SolveVariableValueKind")
        }
        ("SolveVariableRefinementEntry", "storage") => {
            type_is_direct_named(ty, "SolveVariableStorageRun")
        }
        _ => false,
    }
}

fn check_projection_type(
    syntax: &syn::File,
    type_name: &str,
    expected_public_methods: &[&str],
    violations: &mut BTreeSet<String>,
) {
    let private_fields = find_struct(syntax, type_name).is_some_and(|item| {
        !item.fields.is_empty()
            && item
                .fields
                .iter()
                .all(|field| matches!(field.vis, syn::Visibility::Inherited))
    });
    if !private_fields {
        violations.insert(format!("{type_name}-fields-not-private"));
    }

    for implementation in syntax.items.iter().filter_map(|item| match item {
        syn::Item::Impl(implementation)
            if type_is_path_named(&implementation.self_ty, type_name) =>
        {
            Some(implementation)
        }
        _ => None,
    }) {
        if let Some((_, trait_path, _)) = &implementation.trait_ {
            let trait_name = trait_path
                .segments
                .last()
                .map_or_else(|| "unknown".to_owned(), |segment| segment.ident.to_string());
            violations.insert(format!("{type_name}-trait-impl:{trait_name}"));
        }
    }

    let mut actual_methods = syntax
        .items
        .iter()
        .filter_map(|item| match item {
            syn::Item::Impl(implementation)
                if type_is_path_named(&implementation.self_ty, type_name) =>
            {
                Some(implementation)
            }
            _ => None,
        })
        .flat_map(|implementation| &implementation.items)
        .filter_map(|item| match item {
            syn::ImplItem::Fn(method) if matches!(method.vis, syn::Visibility::Public(_)) => {
                Some(method.sig.ident.to_string())
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    actual_methods.sort();
    let expected_methods = expected_public_methods
        .iter()
        .map(|name| (*name).to_owned())
        .collect::<Vec<_>>();
    if actual_methods != expected_methods {
        violations.insert(format!("{type_name}-public-method-surface"));
    }
    let signatures_are_exact = syntax.items.iter().all(|item| {
        let syn::Item::Impl(implementation) = item else {
            return true;
        };
        if !type_is_path_named(&implementation.self_ty, type_name) {
            return true;
        }
        implementation.items.iter().all(|item| match item {
            syn::ImplItem::Fn(method) if matches!(method.vis, syn::Visibility::Public(_)) => {
                projection_method_return_is_exact(type_name, method)
            }
            _ => true,
        })
    });
    if !signatures_are_exact {
        violations.insert(format!("{type_name}-public-method-signature"));
    }
}

fn projection_method_return_is_exact(owner: &str, method: &syn::ImplItemFn) -> bool {
    let name = method.sig.ident.to_string();
    let Some(output) = return_type(&method.sig.output) else {
        return false;
    };
    match (owner, name.as_str()) {
        ("DaeVariableRefinementView", "entries") => {
            projection_slice_signature_is_exact(method, output, "DaeVariableRefinementEntry")
        }
        ("DaeVariableRefinementEntry", "source_occurrence")
        | ("SolveVariableRefinementEntry", "source_occurrence") => {
            type_is_direct_named(output, "SourceOccurrenceId")
        }
        ("DaeVariableRefinementEntry", "role") => type_is_direct_named(output, "VariableRole"),
        ("DaeVariableRefinementEntry", "fixed") => type_is_direct_named(output, "Fixity"),
        ("DaeVariableRefinementEntry", "variability") => {
            type_is_direct_named(output, "ExpressionVariability")
        }
        ("DaeVariableRefinementEntry", "is_tunable") => type_is_direct_named(output, "bool"),
        ("DaeVariableRefinementEntry", "causality") => {
            type_is_direct_named(output, "VariableCausality")
        }
        ("DaeVariableRefinementEntry", "scalar_type") => type_is_direct_named(output, "ScalarType"),
        ("DaeVariableRefinementEntry", "scalar_count") => type_is_direct_named(output, "usize"),
        ("DaeVariableRefinementEntry", "dimensions")
        | ("SolveVariableRefinementEntry", "dimensions") => {
            type_is_reference_to_slice(output, "u32")
        }
        ("SolveVariableRefinementView", "entries") => {
            projection_slice_signature_is_exact(method, output, "SolveVariableRefinementEntry")
        }
        ("SolveVariableRefinementEntry", "fixed") => type_is_direct_named(output, "Fixity"),
        ("SolveVariableRefinementEntry", "state_initialization") => {
            type_is_direct_named(output, "SolveStateInitialization")
        }
        ("SolveVariableRefinementEntry", "variability") => {
            type_is_direct_named(output, "SolveVariableVariability")
        }
        ("SolveVariableRefinementEntry", "tunable") => type_is_direct_named(output, "bool"),
        ("SolveVariableRefinementEntry", "role") => {
            type_is_direct_named(output, "SolveVariableStorageRole")
        }
        ("SolveVariableRefinementEntry", "value_kind") => {
            type_is_direct_named(output, "SolveVariableValueKind")
        }
        ("SolveVariableRefinementEntry", "causality") => {
            type_is_direct_named(output, "SolveVariableCausality")
        }
        ("SolveVariableRefinementEntry", "storage") => {
            type_is_direct_named(output, "SolveVariableStorageRun")
        }
        _ => false,
    }
}

fn check_lowering(syntax: &syn::File, violations: &mut BTreeSet<String>) {
    check_mandatory_field(
        syntax,
        "C60CheckedSolveRoot",
        "variable_catalog_refinement",
        RECEIPT,
        "lowered-solve-model-receipt-not-mandatory",
        violations,
    );
    check_mandatory_field(
        syntax,
        "CheckedSolveRoot",
        "_variable_catalog_refinement",
        RECEIPT,
        "lowered-solve-model-receipt-not-mandatory",
        violations,
    );
    let Some(function) = find_impl_method(syntax, "C60CheckedSolveRoot", "construct") else {
        violations.insert("c60-checked-root-constructor-missing".to_owned());
        return;
    };
    let mut calls = RefinementCallVisitor {
        total: 0,
        propagated: 0,
        identifiers: 0,
    };
    calls.visit_block(&function.block);
    if calls.total != 1 {
        violations.insert(format!("refinement-check-call-count:{}", calls.total));
    }
    if calls.propagated != 1 {
        violations.insert(format!(
            "refinement-check-propagated-count:{}",
            calls.propagated
        ));
    }
    if calls.identifiers != 1 {
        violations.insert(format!(
            "refinement-check-identifier-count:{}",
            calls.identifiers
        ));
    }

    let propagated_bindings = function
        .block
        .stmts
        .iter()
        .filter_map(propagated_check_binding)
        .collect::<Vec<_>>();
    if propagated_bindings.len() != 1 {
        violations.insert(format!(
            "refinement-check-propagated-binding-count:{}",
            propagated_bindings.len()
        ));
        return;
    }
    let propagated_binding = &propagated_bindings[0];
    if propagated_binding.mutable {
        violations.insert("refinement-check-propagated-binding-mutable".to_owned());
    }
    let mut binding_integrity = BindingIntegrityVisitor {
        target: &propagated_binding.name,
        definitions: 0,
        assignments: 0,
    };
    binding_integrity.visit_block(&function.block);
    if binding_integrity.definitions != 1 {
        violations.insert(format!(
            "refinement-check-propagated-binding-definition-count:{}",
            binding_integrity.definitions
        ));
    }
    if binding_integrity.assignments != 0 {
        violations.insert(format!(
            "refinement-check-propagated-binding-assignment-count:{}",
            binding_integrity.assignments
        ));
    }

    let mut literals = LoweredSolveModelBindingVisitor {
        propagated_binding: &propagated_binding.name,
        total: 0,
        correctly_bound: 0,
    };
    literals.visit_block(&function.block);
    if literals.total != 1 {
        violations.insert(format!(
            "lowered-solve-model-literal-count:{}",
            literals.total
        ));
    }
    if literals.correctly_bound != 1 {
        violations.insert("lowered-solve-model-receipt-not-bound-to-propagated-check".to_owned());
    }
}

struct PropagatedCheckBinding {
    name: String,
    mutable: bool,
}

fn propagated_check_binding(statement: &syn::Stmt) -> Option<PropagatedCheckBinding> {
    let syn::Stmt::Local(local) = statement else {
        return None;
    };
    let syn::Pat::Ident(binding) = &local.pat else {
        return None;
    };
    let initialization = local.init.as_ref()?;
    let syn::Expr::Try(propagated) = peel_expression(&initialization.expr) else {
        return None;
    };
    is_directly_propagated_check(&propagated.expr).then(|| PropagatedCheckBinding {
        name: binding.ident.to_string(),
        mutable: binding.mutability.is_some(),
    })
}

struct BindingIntegrityVisitor<'name> {
    target: &'name str,
    definitions: usize,
    assignments: usize,
}

impl Visit<'_> for BindingIntegrityVisitor<'_> {
    fn visit_pat_ident(&mut self, pattern: &syn::PatIdent) {
        if pattern.ident == self.target {
            self.definitions += 1;
        }
        visit::visit_pat_ident(self, pattern);
    }

    fn visit_expr_assign(&mut self, assignment: &syn::ExprAssign) {
        if matches!(peel_expression(&assignment.left), syn::Expr::Path(path)
            if path.path.is_ident(self.target))
        {
            self.assignments += 1;
        }
        visit::visit_expr_assign(self, assignment);
    }
}

struct LoweredSolveModelBindingVisitor<'binding> {
    propagated_binding: &'binding str,
    total: usize,
    correctly_bound: usize,
}

impl Visit<'_> for LoweredSolveModelBindingVisitor<'_> {
    fn visit_expr_struct(&mut self, expression: &syn::ExprStruct) {
        if expression.path.is_ident("Self") {
            self.total += 1;
            let receipt = expression.fields.iter().find(|field| {
                matches!(&field.member, syn::Member::Named(name)
                    if name == "variable_catalog_refinement")
            });
            if receipt.is_some_and(|field| {
                matches!(peel_expression(&field.expr), syn::Expr::Path(path)
                    if path.path.is_ident(self.propagated_binding))
            }) {
                self.correctly_bound += 1;
            }
        }
        visit::visit_expr_struct(self, expression);
    }
}

fn check_capability_type(syntax: &syn::File, type_name: &str, violations: &mut BTreeSet<String>) {
    let Some(item) = find_struct(syntax, type_name) else {
        violations.insert(format!("{type_name}-missing"));
        return;
    };
    let privately_sealed = matches!(&item.fields, syn::Fields::Named(fields)
        if !fields.named.is_empty()
            && fields.named.iter().all(|field| matches!(field.vis, syn::Visibility::Inherited)));
    if !privately_sealed {
        violations.insert(format!("{type_name}-not-privately-sealed"));
    }
    check_forbidden_traits(
        syntax,
        item,
        type_name,
        &["Clone", "Default", "Serialize", "Deserialize"],
        violations,
    );

    let mut public_mints = PublicMintVisitor {
        target: type_name,
        found: false,
    };
    public_mints.visit_file(syntax);
    if public_mints.found {
        violations.insert(format!("{type_name}-public-mint"));
    }
}

fn check_forbidden_traits(
    syntax: &syn::File,
    item: &syn::ItemStruct,
    type_name: &str,
    forbidden: &[&str],
    violations: &mut BTreeSet<String>,
) {
    for name in derived_names(&item.attrs) {
        if forbidden.contains(&name.as_str()) {
            violations.insert(format!("{type_name}-forbidden-trait:{name}"));
        }
    }
    for trait_name in implemented_forbidden_traits(syntax, type_name, forbidden) {
        violations.insert(format!("{type_name}-forbidden-trait:{trait_name}"));
    }
}

fn implemented_forbidden_traits(
    syntax: &syn::File,
    type_name: &str,
    forbidden: &[&str],
) -> BTreeSet<String> {
    syntax
        .items
        .iter()
        .filter_map(|item| match item {
            syn::Item::Impl(implementation) => Some(implementation),
            _ => None,
        })
        .filter_map(|implementation| {
            let (_, trait_path, _) = implementation.trait_.as_ref()?;
            let trait_name = trait_path.segments.last()?.ident.to_string();
            (forbidden.contains(&trait_name.as_str())
                && type_is_path_named(&implementation.self_ty, type_name))
            .then_some(trait_name)
        })
        .collect()
}

fn check_wire(syntax: &syn::File, violations: &mut BTreeSet<String>) {
    for owner in [
        "SolveVariableCatalogEntryWireRef",
        "SolveVariableCatalogEntryWire",
    ] {
        check_mandatory_field(
            syntax,
            owner,
            "source_occurrence",
            "SourceOccurrenceId",
            &format!("{owner}-source-occurrence-not-mandatory"),
            violations,
        );
    }
    let schema_is_five = syntax.items.iter().any(|item| {
        matches!(item, syn::Item::Const(constant)
            if constant.ident == "SOLVE_MODEL_SCHEMA_VERSION"
                && matches!(constant.expr.as_ref(), syn::Expr::Lit(syn::ExprLit {
                    lit: syn::Lit::Int(value), ..
                }) if matches!(value.base10_parse::<u16>(), Ok(5))))
    });
    if !schema_is_five {
        violations.insert("solve-model-wire-schema-not-5".to_owned());
    }
}

struct RefinementCallVisitor {
    total: usize,
    propagated: usize,
    identifiers: usize,
}

impl<'syntax> Visit<'syntax> for RefinementCallVisitor {
    fn visit_expr_call(&mut self, call: &'syntax syn::ExprCall) {
        if call_is_named(call, CHECKER) {
            self.total += 1;
        }
        visit::visit_expr_call(self, call);
    }

    fn visit_expr_try(&mut self, expression: &'syntax syn::ExprTry) {
        if is_directly_propagated_check(&expression.expr) {
            self.propagated += 1;
        }
        visit::visit_expr_try(self, expression);
    }

    fn visit_ident(&mut self, ident: &'syntax syn::Ident) {
        if identifier_is(ident, CHECKER) {
            self.identifiers += 1;
        }
    }
}

fn is_directly_propagated_check(expression: &syn::Expr) -> bool {
    match peel_expression(expression) {
        syn::Expr::Call(call) => call_is_named(call, CHECKER),
        syn::Expr::MethodCall(call) if call.method == "map_err" => {
            is_directly_propagated_check(&call.receiver)
        }
        syn::Expr::MethodCall(call) if call.method == "inspect" && call.args.len() == 1 => {
            matches!(call.args.first().map(peel_expression), Some(syn::Expr::Closure(closure))
                if is_directly_propagated_check(&closure.body))
        }
        syn::Expr::Block(block) if block.block.stmts.len() == 1 => match &block.block.stmts[0] {
            syn::Stmt::Expr(expression, None) => is_directly_propagated_check(expression),
            _ => false,
        },
        _ => false,
    }
}

struct PublicMintVisitor<'name> {
    target: &'name str,
    found: bool,
}

fn collect_restricted_mints(syntax: &syn::File, path: &str, target: &str, found: &mut Vec<String>) {
    let mut visitor = RestrictedMintVisitor {
        path,
        target,
        found,
    };
    visitor.visit_file(syntax);
}

fn check_restricted_receipt_mint_catalog(found: &[String], violations: &mut BTreeSet<String>) {
    let checker_suffix = format!("{CHECKER_PATH}:fn:{CHECKER}");
    if found.len() != 1 {
        violations.insert(format!("{RECEIPT}-restricted-mint-count:{}", found.len()));
    } else if !found[0].ends_with(&checker_suffix) {
        violations.insert(format!("{RECEIPT}-restricted-mint-not-checker"));
    }
}

fn check_receipt_literal_catalog(found: &[String], violations: &mut BTreeSet<String>) {
    let checker_suffix = format!("{CHECKER_PATH}:fn:{CHECKER}");
    if found.len() != 1 {
        violations.insert(format!("{RECEIPT}-struct-literal-count:{}", found.len()));
    } else if !found[0].ends_with(&checker_suffix) {
        violations.insert(format!("{RECEIPT}-struct-literal-not-checker"));
    }
}

fn check_receipt_alias_catalog(found: &[String], violations: &mut BTreeSet<String>) {
    if !found.is_empty() {
        violations.insert(format!("{RECEIPT}-alias-count:{}", found.len()));
    }
}

struct RestrictedMintVisitor<'a> {
    path: &'a str,
    target: &'a str,
    found: &'a mut Vec<String>,
}

impl Visit<'_> for RestrictedMintVisitor<'_> {
    fn visit_item_fn(&mut self, function: &syn::ItemFn) {
        if matches!(function.vis, syn::Visibility::Restricted(_))
            && return_type_mints(&function.sig.output, self.target)
        {
            self.found
                .push(format!("{}:fn:{}", self.path, function.sig.ident));
        }
        visit::visit_item_fn(self, function);
    }

    fn visit_impl_item_fn(&mut self, function: &syn::ImplItemFn) {
        if matches!(function.vis, syn::Visibility::Restricted(_))
            && return_type_mints(&function.sig.output, self.target)
        {
            self.found
                .push(format!("{}:method:{}", self.path, function.sig.ident));
        }
        visit::visit_impl_item_fn(self, function);
    }

    fn visit_item_const(&mut self, constant: &syn::ItemConst) {
        if matches!(constant.vis, syn::Visibility::Restricted(_))
            && type_contains(&constant.ty, self.target)
        {
            self.found
                .push(format!("{}:const:{}", self.path, constant.ident));
        }
        visit::visit_item_const(self, constant);
    }

    fn visit_item_static(&mut self, item: &syn::ItemStatic) {
        if matches!(item.vis, syn::Visibility::Restricted(_))
            && type_contains(&item.ty, self.target)
        {
            self.found
                .push(format!("{}:static:{}", self.path, item.ident));
        }
        visit::visit_item_static(self, item);
    }
}

fn collect_receipt_aliases(syntax: &syn::File, path: &str, target: &str, found: &mut Vec<String>) {
    let mut visitor = ReceiptAliasVisitor {
        path,
        target,
        found,
    };
    visitor.visit_file(syntax);
}

struct ReceiptAliasVisitor<'a> {
    path: &'a str,
    target: &'a str,
    found: &'a mut Vec<String>,
}

impl Visit<'_> for ReceiptAliasVisitor<'_> {
    fn visit_item_type(&mut self, alias: &syn::ItemType) {
        if type_contains(&alias.ty, self.target) {
            self.found
                .push(format!("{}:type:{}", self.path, alias.ident));
        }
        visit::visit_item_type(self, alias);
    }

    fn visit_item_use(&mut self, item: &syn::ItemUse) {
        if use_tree_renames(&item.tree, self.target) {
            self.found.push(format!("{}:use-alias", self.path));
        }
        visit::visit_item_use(self, item);
    }
}

fn use_tree_renames(tree: &syn::UseTree, target: &str) -> bool {
    match tree {
        syn::UseTree::Path(path) => use_tree_renames(&path.tree, target),
        syn::UseTree::Group(group) => group
            .items
            .iter()
            .any(|item| use_tree_renames(item, target)),
        syn::UseTree::Rename(rename) => rename.ident == target,
        syn::UseTree::Name(_) | syn::UseTree::Glob(_) => false,
    }
}

fn collect_owned_struct_literals(
    syntax: &syn::File,
    path: &str,
    target: &str,
    found: &mut Vec<String>,
) {
    let mut visitor = OwnedStructLiteralVisitor {
        path,
        target,
        owner: None,
        inside_target_impl: false,
        found,
    };
    visitor.visit_file(syntax);
}

struct OwnedStructLiteralVisitor<'a> {
    path: &'a str,
    target: &'a str,
    owner: Option<String>,
    inside_target_impl: bool,
    found: &'a mut Vec<String>,
}

impl Visit<'_> for OwnedStructLiteralVisitor<'_> {
    fn visit_item_impl(&mut self, implementation: &syn::ItemImpl) {
        let previous = self.inside_target_impl;
        self.inside_target_impl = type_is_path_named(&implementation.self_ty, self.target);
        visit::visit_item_impl(self, implementation);
        self.inside_target_impl = previous;
    }

    fn visit_item_fn(&mut self, function: &syn::ItemFn) {
        let previous = self.owner.replace(format!("fn:{}", function.sig.ident));
        visit::visit_item_fn(self, function);
        self.owner = previous;
    }

    fn visit_impl_item_fn(&mut self, function: &syn::ImplItemFn) {
        let previous = self.owner.replace(format!("method:{}", function.sig.ident));
        visit::visit_impl_item_fn(self, function);
        self.owner = previous;
    }

    fn visit_expr_struct(&mut self, expression: &syn::ExprStruct) {
        let name = expression
            .path
            .segments
            .last()
            .map(|segment| &segment.ident);
        if name
            .is_some_and(|name| name == self.target || (name == "Self" && self.inside_target_impl))
        {
            self.found.push(format!(
                "{}:{}",
                self.path,
                self.owner.as_deref().unwrap_or("outside-function")
            ));
        }
        visit::visit_expr_struct(self, expression);
    }
}

fn collect_struct_literals(syntax: &syn::File, path: &str, target: &str, found: &mut Vec<String>) {
    let mut visitor = StructLiteralVisitor {
        path,
        target,
        found,
    };
    visitor.visit_file(syntax);
}

struct StructLiteralVisitor<'a> {
    path: &'a str,
    target: &'a str,
    found: &'a mut Vec<String>,
}

impl Visit<'_> for StructLiteralVisitor<'_> {
    fn visit_expr_struct(&mut self, expression: &syn::ExprStruct) {
        if expression
            .path
            .segments
            .last()
            .is_some_and(|segment| segment.ident == self.target)
        {
            self.found.push(self.path.to_owned());
        }
        visit::visit_expr_struct(self, expression);
    }
}

impl Visit<'_> for PublicMintVisitor<'_> {
    fn visit_item_fn(&mut self, function: &syn::ItemFn) {
        if matches!(function.vis, syn::Visibility::Public(_))
            && return_type_mints(&function.sig.output, self.target)
        {
            self.found = true;
        }
        visit::visit_item_fn(self, function);
    }

    fn visit_impl_item_fn(&mut self, function: &syn::ImplItemFn) {
        if matches!(function.vis, syn::Visibility::Public(_))
            && return_type_mints(&function.sig.output, self.target)
        {
            self.found = true;
        }
        visit::visit_impl_item_fn(self, function);
    }

    fn visit_item_const(&mut self, constant: &syn::ItemConst) {
        if matches!(constant.vis, syn::Visibility::Public(_))
            && type_contains(&constant.ty, self.target)
        {
            self.found = true;
        }
        visit::visit_item_const(self, constant);
    }

    fn visit_item_static(&mut self, item: &syn::ItemStatic) {
        if matches!(item.vis, syn::Visibility::Public(_)) && type_contains(&item.ty, self.target) {
            self.found = true;
        }
        visit::visit_item_static(self, item);
    }
}

struct IdentifierVisitor<'name> {
    target: &'name str,
    found: bool,
}

impl Visit<'_> for IdentifierVisitor<'_> {
    fn visit_ident(&mut self, ident: &syn::Ident) {
        if identifier_is(ident, self.target) {
            self.found = true;
        }
    }

    fn visit_macro(&mut self, item: &syn::Macro) {
        if token_stream_mentions_identifier(&item.tokens, self.target) {
            self.found = true;
        }
        visit::visit_macro(self, item);
    }
}

fn syntax_mentions_identifier_in_file(syntax: &syn::File, target: &str) -> bool {
    let mut visitor = IdentifierVisitor {
        target,
        found: false,
    };
    visitor.visit_file(syntax);
    visitor.found
}

fn derived_names(attributes: &[syn::Attribute]) -> BTreeSet<String> {
    let mut names = BTreeSet::new();
    for attribute in attributes {
        let syn::Meta::List(list) = &attribute.meta else {
            continue;
        };
        if !list.path.is_ident("derive") {
            continue;
        }
        let paths = list
            .parse_args_with(
                syn::punctuated::Punctuated::<syn::Path, syn::Token![,]>::parse_terminated,
            )
            .expect("derive arguments must parse");
        names.extend(
            paths
                .iter()
                .filter_map(|path| path.segments.last())
                .map(|segment| segment.ident.to_string()),
        );
    }
    names
}

fn find_struct<'syntax>(
    syntax: &'syntax syn::File,
    name: &str,
) -> Option<&'syntax syn::ItemStruct> {
    syntax.items.iter().find_map(|item| match item {
        syn::Item::Struct(item) if item.ident == name => Some(item),
        _ => None,
    })
}

fn find_function<'syntax>(syntax: &'syntax syn::File, name: &str) -> Option<&'syntax syn::ItemFn> {
    syntax.items.iter().find_map(|item| match item {
        syn::Item::Fn(item) if item.sig.ident == name => Some(item),
        _ => None,
    })
}

fn find_impl_method<'syntax>(
    syntax: &'syntax syn::File,
    owner: &str,
    name: &str,
) -> Option<&'syntax syn::ImplItemFn> {
    syntax.items.iter().find_map(|item| {
        let syn::Item::Impl(item) = item else {
            return None;
        };
        if !type_is_path_named(&item.self_ty, owner) {
            return None;
        }
        item.items.iter().find_map(|member| match member {
            syn::ImplItem::Fn(method) if method.sig.ident == name => Some(method),
            _ => None,
        })
    })
}

fn named_field<'syntax>(item: &'syntax syn::ItemStruct, name: &str) -> Option<&'syntax syn::Field> {
    let syn::Fields::Named(fields) = &item.fields else {
        return None;
    };
    fields
        .named
        .iter()
        .find(|field| field.ident.as_ref().is_some_and(|ident| ident == name))
}

fn type_is_direct_named(ty: &syn::Type, expected: &str) -> bool {
    let syn::Type::Path(path) = ty else {
        return false;
    };
    path.qself.is_none()
        && path.path.segments.last().is_some_and(|segment| {
            segment.ident == expected && matches!(segment.arguments, syn::PathArguments::None)
        })
}

fn type_is_path_named(ty: &syn::Type, expected: &str) -> bool {
    matches!(ty, syn::Type::Path(path)
        if path.qself.is_none()
            && path.path.segments.last().is_some_and(|segment| segment.ident == expected))
}

fn projection_slice_signature_is_exact(
    method: &syn::ImplItemFn,
    output: &syn::Type,
    entry: &str,
) -> bool {
    method.sig.inputs.len() == 1
        && matches!(method.sig.inputs.first(), Some(syn::FnArg::Receiver(receiver))
            if receiver.reference.is_some()
                && receiver.mutability.is_none()
                && receiver.colon_token.is_none())
        && method.sig.generics.params.is_empty()
        && method.sig.generics.where_clause.is_none()
        && type_is_reference_to_slice(output, entry)
}

fn type_is_boxed_slice_of(ty: &syn::Type, expected_element: &str) -> bool {
    let syn::Type::Path(path) = ty else {
        return false;
    };
    let Some(segment) = path.path.segments.last() else {
        return false;
    };
    matches!(&segment.arguments, syn::PathArguments::AngleBracketed(arguments)
        if segment.ident == "Box"
            && arguments.args.len() == 1
            && arguments.args.first().is_some_and(|argument| matches!(argument,
                syn::GenericArgument::Type(syn::Type::Slice(slice))
                    if type_is_direct_named(&slice.elem, expected_element))))
}

fn type_is_reference_to_slice(ty: &syn::Type, expected_element: &str) -> bool {
    matches!(ty, syn::Type::Reference(reference)
        if reference.mutability.is_none()
            && matches!(reference.elem.as_ref(), syn::Type::Slice(slice)
            if type_is_direct_named(&slice.elem, expected_element)))
}

fn type_contains(ty: &syn::Type, expected: &str) -> bool {
    match ty {
        syn::Type::Path(path) => path.path.segments.iter().any(|segment| {
            segment.ident == expected
                || matches!(&segment.arguments, syn::PathArguments::AngleBracketed(arguments)
                if arguments.args.iter().any(|argument| match argument {
                    syn::GenericArgument::Type(ty) => type_contains(ty, expected),
                    syn::GenericArgument::AssocType(binding) => {
                        type_contains(&binding.ty, expected)
                    }
                    _ => false,
                }))
        }),
        syn::Type::Reference(reference) => type_contains(&reference.elem, expected),
        syn::Type::Tuple(tuple) => tuple.elems.iter().any(|ty| type_contains(ty, expected)),
        syn::Type::Paren(paren) => type_contains(&paren.elem, expected),
        syn::Type::Group(group) => type_contains(&group.elem, expected),
        syn::Type::Array(array) => type_contains(&array.elem, expected),
        syn::Type::Slice(slice) => type_contains(&slice.elem, expected),
        syn::Type::ImplTrait(implementation) => implementation.bounds.iter().any(|bound| {
            matches!(bound, syn::TypeParamBound::Trait(bound)
            if bound.path.segments.iter().any(|segment| {
                segment.ident == expected
                    || matches!(&segment.arguments,
                        syn::PathArguments::AngleBracketed(arguments)
                        if arguments.args.iter().any(|argument| match argument {
                            syn::GenericArgument::Type(ty) => type_contains(ty, expected),
                            syn::GenericArgument::AssocType(binding) => {
                                type_contains(&binding.ty, expected)
                            }
                            _ => false,
                        }))
            }))
        }),
        _ => false,
    }
}

fn return_type(output: &syn::ReturnType) -> Option<&syn::Type> {
    match output {
        syn::ReturnType::Default => None,
        syn::ReturnType::Type(_, ty) => Some(ty),
    }
}

fn return_type_contains(output: &syn::ReturnType, expected: &str) -> bool {
    matches!(output, syn::ReturnType::Type(_, ty) if type_contains(ty, expected))
}

fn return_type_mints(output: &syn::ReturnType, expected: &str) -> bool {
    matches!(output, syn::ReturnType::Type(_, ty)
        if !matches!(ty.as_ref(), syn::Type::Reference(_)) && type_contains(ty, expected))
}

fn identifier_is(ident: &syn::Ident, expected: &str) -> bool {
    let spelling = ident.to_string();
    spelling.strip_prefix("r#").unwrap_or(&spelling) == expected
}

fn token_stream_mentions_identifier(stream: &proc_macro2::TokenStream, expected: &str) -> bool {
    stream.clone().into_iter().any(|token| match token {
        proc_macro2::TokenTree::Ident(ident) => identifier_is(&ident, expected),
        proc_macro2::TokenTree::Group(group) => {
            token_stream_mentions_identifier(&group.stream(), expected)
        }
        proc_macro2::TokenTree::Punct(_) | proc_macro2::TokenTree::Literal(_) => false,
    })
}

fn serde_field_has_default(field: &syn::Field) -> bool {
    field.attrs.iter().any(|attribute| {
        attribute.path().is_ident("serde")
            && matches!(&attribute.meta, syn::Meta::List(list) if list.tokens.to_string().split(|character: char| !character.is_alphanumeric() && character != '_').any(|token| token == "default"))
    })
}

fn call_is_named(call: &syn::ExprCall, expected: &str) -> bool {
    matches!(peel_expression(&call.func), syn::Expr::Path(path)
        if path.path.segments.last().is_some_and(|segment| segment.ident == expected))
}

fn peel_expression(expression: &syn::Expr) -> &syn::Expr {
    match expression {
        syn::Expr::Group(group) => peel_expression(&group.expr),
        syn::Expr::Paren(paren) => peel_expression(&paren.expr),
        expression => expression,
    }
}

fn parse(source: &str, path: &str) -> syn::File {
    syn::parse_file(source).unwrap_or_else(|error| panic!("parse {path}: {error}"))
}

fn replace_once(source: &str, old: &str, new: &str) -> String {
    assert_eq!(
        source.match_indices(old).count(),
        1,
        "mutation anchor must occur exactly once: {old}"
    );
    source.replacen(old, new, 1)
}

fn assert_violation(sources: &BoundarySources, expected: &str) {
    let violations = boundary_violations(sources);
    assert!(
        violations.contains(expected),
        "mutation must produce {expected:?}; found {violations:#?}"
    );
}
