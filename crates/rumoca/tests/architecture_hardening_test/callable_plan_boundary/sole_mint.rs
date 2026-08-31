//! Sole-mint tombstone for the callable plan root and its storage.
//!
//! SPEC_0036 requires the checked aggregate to be unrepresentable outside its
//! construction authority. Rust field privacy is what actually enforces that:
//! `CallablePlan` and `PlanStorage` declare private fields inside the module
//! that also owns their mint, so no sibling module can write the literal.
//!
//! This gate is the backstop for that arrangement, not the arrangement itself.
//! It fails when a second mint site appears anywhere in the crate's production
//! graph, and when an invariant field's visibility grows past the module (for
//! the plan root and its storage) or past the construction authority (for the
//! entries the authority assembles).

use std::collections::{BTreeMap, BTreeSet};

use syn::visit::{self, Visit};

use super::super::architecture_hardening_support::{
    attributes_require_test, production_rust_source_contexts,
};
use super::PLAN_CRATE;

/// The plan root and its storage. Every field must be private to the module
/// that mints them, and each may be minted at exactly one site.
const PLAN_ROOT_TYPES: &[&str] = &["CallablePlan", "PlanStorage"];

/// The correlated entries the construction authority assembles. Their fields
/// may be named inside `crate::construction` and nowhere else.
const PLAN_ENTRY_TYPES: &[&str] = &[
    "AcyclicCallEdge",
    "AcyclicReceipt",
    "CallProjectionSourceEntry",
    "EffectEntry",
    "OperationEntry",
    "OwnerEntry",
    "ProjectionEntry",
    "ScopeEntry",
    "StructuredRegionEntry",
    "ValueEntry",
];

/// Payload enums stored inside the entries. Rust cannot restrict a variant's
/// fields below its enum, so the enum itself must stay crate-internal; a bare
/// value is inert because no entry field that could hold it is nameable.
const PLAN_DETAIL_ENUMS: &[&str] = &[
    "CallableOperationDetail",
    "OperationSource",
    "StructuredRegionDetail",
    "ValueProducer",
];

/// The module that owns the plan root, its storage, and their sole mint.
const MINT_MODULE: [&str; 2] = ["construction", "plan"];

#[derive(Default)]
struct MintScan {
    mints: BTreeSet<String>,
    findings: BTreeSet<String>,
}

struct MintVisitor {
    scan: MintScan,
    current_self: Vec<Option<String>>,
}

impl MintVisitor {
    fn record(&mut self, message: impl Into<String>) {
        self.scan.findings.insert(message.into());
    }

    fn resolved_literal_name(&self, path: &syn::Path) -> Option<String> {
        let last = path.segments.last()?.ident.to_string();
        if last == "Self" {
            return self.current_self.last().cloned().flatten();
        }
        Some(last)
    }

    fn check_fields(&mut self, owner: &str, fields: &syn::Fields) {
        let root = PLAN_ROOT_TYPES.contains(&owner);
        let entry = PLAN_ENTRY_TYPES.contains(&owner);
        if !root && !entry {
            return;
        }
        for (ordinal, field) in fields.iter().enumerate() {
            let name = field
                .ident
                .as_ref()
                .map_or_else(|| ordinal.to_string(), std::string::ToString::to_string);
            let allowed = if root {
                matches!(field.vis, syn::Visibility::Inherited)
            } else {
                matches!(field.vis, syn::Visibility::Inherited)
                    || is_construction_scoped(&field.vis)
            };
            if !allowed {
                let scope = if root {
                    "its minting module"
                } else {
                    "the construction authority"
                };
                self.record(format!(
                    "`{owner}` field `{name}` is visible beyond {scope} ({})",
                    visibility_label(&field.vis)
                ));
            }
        }
    }
}

impl<'ast> Visit<'ast> for MintVisitor {
    fn visit_item(&mut self, item: &'ast syn::Item) {
        if item_attributes(item).is_some_and(attributes_require_test) {
            return;
        }
        visit::visit_item(self, item);
    }

    fn visit_item_struct(&mut self, item: &'ast syn::ItemStruct) {
        let name = item.ident.to_string();
        if name == "PlanStorage" && !matches!(item.vis, syn::Visibility::Inherited) {
            self.record(format!(
                "`PlanStorage` is nameable outside its minting module ({})",
                visibility_label(&item.vis)
            ));
        }
        self.check_fields(&name, &item.fields);
        visit::visit_item_struct(self, item);
    }

    fn visit_item_enum(&mut self, item: &'ast syn::ItemEnum) {
        let name = item.ident.to_string();
        if PLAN_DETAIL_ENUMS.contains(&name.as_str())
            && matches!(item.vis, syn::Visibility::Public(_))
        {
            self.record(format!(
                "plan payload enum `{name}` escapes the crate as `pub`"
            ));
        }
        visit::visit_item_enum(self, item);
    }

    fn visit_item_impl(&mut self, item: &'ast syn::ItemImpl) {
        self.current_self.push(type_path_last(&item.self_ty));
        visit::visit_item_impl(self, item);
        self.current_self.pop();
    }

    fn visit_expr_struct(&mut self, expression: &'ast syn::ExprStruct) {
        if let Some(name) = self.resolved_literal_name(&expression.path)
            && PLAN_ROOT_TYPES.contains(&name.as_str())
        {
            self.scan.mints.insert(name);
        }
        visit::visit_expr_struct(self, expression);
    }
}

fn type_path_last(ty: &syn::Type) -> Option<String> {
    let syn::Type::Path(path) = ty else {
        return None;
    };
    path.path
        .segments
        .last()
        .map(|segment| segment.ident.to_string())
}

fn restricted_segments(visibility: &syn::Visibility) -> Option<(bool, Vec<String>)> {
    let syn::Visibility::Restricted(restricted) = visibility else {
        return None;
    };
    Some((
        restricted.in_token.is_some(),
        restricted
            .path
            .segments
            .iter()
            .map(|segment| segment.ident.to_string())
            .collect(),
    ))
}

fn is_construction_scoped(visibility: &syn::Visibility) -> bool {
    restricted_segments(visibility)
        .is_some_and(|(has_in, segments)| has_in && segments == ["crate", "construction"])
}

fn visibility_label(visibility: &syn::Visibility) -> String {
    match visibility {
        syn::Visibility::Inherited => "private".to_owned(),
        syn::Visibility::Public(_) => "pub".to_owned(),
        syn::Visibility::Restricted(_) => {
            let (has_in, segments) = restricted_segments(visibility).expect("restricted");
            let path = segments.join("::");
            if has_in {
                format!("pub(in {path})")
            } else {
                format!("pub({path})")
            }
        }
    }
}

fn item_attributes(item: &syn::Item) -> Option<&[syn::Attribute]> {
    match item {
        syn::Item::Const(item) => Some(&item.attrs),
        syn::Item::Enum(item) => Some(&item.attrs),
        syn::Item::Fn(item) => Some(&item.attrs),
        syn::Item::Impl(item) => Some(&item.attrs),
        syn::Item::Mod(item) => Some(&item.attrs),
        syn::Item::Static(item) => Some(&item.attrs),
        syn::Item::Struct(item) => Some(&item.attrs),
        syn::Item::Trait(item) => Some(&item.attrs),
        syn::Item::Type(item) => Some(&item.attrs),
        syn::Item::Union(item) => Some(&item.attrs),
        syn::Item::Use(item) => Some(&item.attrs),
        _ => None,
    }
}

fn scan_module(label: &str, source: &str) -> MintScan {
    let syntax = syn::parse_file(source).unwrap_or_else(|error| panic!("parse {label}: {error}"));
    let mut visitor = MintVisitor {
        scan: MintScan::default(),
        current_self: Vec::new(),
    };
    visitor.visit_file(&syntax);
    visitor.scan
}

/// `(module path, findings)` and `(minted type, module paths)` over a set of
/// already-read production modules.
fn scan_modules(
    modules: &[(Vec<String>, String, String)],
) -> (BTreeSet<String>, BTreeMap<String, Vec<String>>) {
    let mut findings = BTreeSet::new();
    let mut mints: BTreeMap<String, Vec<String>> = BTreeMap::new();
    for (module_path, label, source) in modules {
        let scan = scan_module(label, source);
        findings.extend(
            scan.findings
                .into_iter()
                .map(|finding| format!("{label}: {finding}")),
        );
        for minted in scan.mints {
            mints
                .entry(minted)
                .or_default()
                .push(module_path.join("::"));
        }
    }
    for sites in mints.values_mut() {
        sites.sort();
        sites.dedup();
    }
    (findings, mints)
}

fn plan_crate_modules() -> Vec<(Vec<String>, String, String)> {
    let root = super::super::workspace_root();
    let crate_root = root.join("crates").join(PLAN_CRATE);
    production_rust_source_contexts(&crate_root, &root)
        .into_iter()
        .map(|context| {
            (
                context.module_path,
                context.path.display().to_string(),
                context.source,
            )
        })
        .collect()
}

#[test]
fn the_plan_root_and_its_storage_have_exactly_one_mint_site() {
    let (_, mints) = scan_modules(&plan_crate_modules());
    let expected_module = MINT_MODULE.join("::");
    for root_type in PLAN_ROOT_TYPES {
        let sites = mints
            .get(*root_type)
            .unwrap_or_else(|| panic!("`{root_type}` must be minted somewhere"));
        assert_eq!(
            sites,
            &[expected_module.clone()],
            "`{root_type}` must be minted only in `{expected_module}`, the module that also \
declares its private fields; a second site means a plan can exist without the checks that \
module performs"
        );
    }
}

#[test]
fn plan_invariant_fields_do_not_leave_their_owning_module() {
    let (findings, _) = scan_modules(&plan_crate_modules());
    assert!(
        findings.is_empty(),
        "callable plan invariant state widened past its construction authority. Field privacy \
is the enforcement; this gate only reports that it was weakened: {findings:#?}"
    );
}

#[test]
fn sole_mint_gate_detects_a_second_mint_site() {
    let authority = (
        MINT_MODULE
            .iter()
            .map(|s| (*s).to_owned())
            .collect::<Vec<_>>(),
        "plan.rs".to_owned(),
        r"
            struct PlanStorage { owners: Box<[OwnerEntry]> }
            pub struct CallablePlan { dae: Dae, storage: PlanStorage }
            impl CallablePlan {
                pub fn construct(dae: Dae) -> Self {
                    Self { dae, storage: PlanStorage { owners: Box::new([]) } }
                }
            }
        "
        .to_owned(),
    );
    let (findings, mints) = scan_modules(std::slice::from_ref(&authority));
    assert!(findings.is_empty(), "the authority module is clean");
    assert_eq!(mints["CallablePlan"], ["construction::plan"]);
    assert_eq!(mints["PlanStorage"], ["construction::plan"]);

    for forgery in [
        r"
            fn never<T>() -> T { loop {} }
            fn forge() -> CallablePlan { CallablePlan { dae: never(), storage: never() } }
        ",
        r"
            fn never<T>() -> T { loop {} }
            impl CallablePlan { fn forge() -> Self { Self { dae: never(), storage: never() } } }
        ",
        r"
            fn never<T>() -> T { loop {} }
            fn forge(other: CallablePlan) -> CallablePlan { CallablePlan { ..other } }
        ",
    ] {
        let modules = vec![
            authority.clone(),
            (
                vec!["view".to_owned()],
                "view.rs".to_owned(),
                forgery.to_owned(),
            ),
        ];
        let (_, mints) = scan_modules(&modules);
        assert_eq!(
            mints["CallablePlan"],
            ["construction::plan", "view"],
            "a second `CallablePlan` mint site must be visible to the gate: {forgery}"
        );
    }

    let storage_forgery = vec![
        authority,
        (
            vec!["view".to_owned()],
            "view.rs".to_owned(),
            "fn forge() -> PlanStorage { PlanStorage { owners: Box::new([]) } }".to_owned(),
        ),
    ];
    let (_, mints) = scan_modules(&storage_forgery);
    assert_eq!(mints["PlanStorage"], ["construction::plan", "view"]);
}

#[test]
fn sole_mint_gate_detects_widened_invariant_fields() {
    for mutation in [
        "pub struct CallablePlan { dae: Dae, pub(crate) storage: PlanStorage }",
        "pub struct CallablePlan { pub dae: Dae, storage: PlanStorage }",
        "pub struct CallablePlan { dae: Dae, pub(in crate::construction) storage: PlanStorage }",
        "struct PlanStorage { pub owners: Box<[OwnerEntry]> }",
        "struct PlanStorage { pub(crate) owners: Box<[OwnerEntry]> }",
        "pub(crate) struct PlanStorage { owners: Box<[OwnerEntry]> }",
        "pub struct PlanStorage { owners: Box<[OwnerEntry]> }",
        "pub(crate) struct OwnerEntry { pub(crate) source_function: u32 }",
        "pub(crate) struct OwnerEntry { pub source_function: u32 }",
        "pub(crate) struct ValueEntry { pub(super) owner: u32 }",
        "pub enum OperationSource { Expression(u32) }",
        "pub enum ValueProducer { Operation(u32) }",
    ] {
        let scan = scan_module("fixture.rs", mutation);
        assert!(
            !scan.findings.is_empty(),
            "a widened plan field or payload enum escaped the gate: {mutation}"
        );
    }
}

#[test]
fn sole_mint_gate_accepts_the_required_shape_and_skips_cfg_test_items() {
    let accepted = r#"
        struct PlanStorage { owners: Box<[OwnerEntry]> }
        pub struct CallablePlan { dae: Dae, storage: PlanStorage }
        pub(crate) struct OwnerEntry { pub(in crate::construction) source_function: u32 }
        pub(crate) enum OperationSource { Expression(u32) }
        #[cfg(test)]
        pub struct PlanStorage { pub owners: Box<[OwnerEntry]> }
    "#;
    let scan = scan_module("fixture.rs", accepted);
    assert!(
        scan.findings.is_empty(),
        "the required shape must pass: {:#?}",
        scan.findings
    );
}
