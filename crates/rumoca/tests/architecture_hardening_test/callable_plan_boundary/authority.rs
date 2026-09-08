//! Retained-inspection-only DAE authority boundary for the callable crates.
//!
//! This is syntax coverage, not a proof of Rust name resolution. It closes the
//! reviewed direct, alias, re-export, dependency-rename, decode, and serde
//! routes by which a callable crate could reacquire DAE construction, replay,
//! or mutation authority while preserving its required owned-DAE retention and
//! borrowed inspection corridor.
//!
//! Aliases resolve across the whole crate, not one file: every module's
//! bindings join one pool, so `crate::`, `self::`, and `super::` paths reach an
//! alias introduced anywhere in the crate. The pool over-approximates on
//! purpose. A same-named alias in an unrelated module produces a finding that a
//! reviewer must clear, which is the safe direction for an authority gate.
//!
//! `#[cfg(test)]` items are out of the production graph and are skipped.
//! `#[cfg(any(test, feature = "..."))]` does not imply a test build and stays
//! scanned.

use std::collections::{BTreeMap, BTreeSet};

use syn::visit::{self, Visit};

use super::super::architecture_hardening_support::{
    attributes_require_test, production_rust_source_contexts,
};
use super::CALLABLE_CRATES;

const DAE_CRATE: &str = "rumoca_ir_dae";

const MUTATION_AUTHORITIES: &[&str] = &[
    "Clocks",
    "Conditions",
    "ContinuousEquations",
    "DaeConstruction",
    "DiscreteEquations",
    "DiscreteValueOwner",
    "DiscreteValueTopology",
    "Domains",
    "Events",
    "ExpressionAt",
    "Expressions",
    "FunctionBody",
    "FunctionLoop",
    "FunctionReservation",
    "Functions",
    "InitializationEquations",
    "ModelEventDefinition",
    "ModelEventStep",
    "ModelEventTransactions",
    "PositiveParameter",
    "QuotientReplayToken",
    "ResidualEquation",
    "StructuredResiduals",
    "Temporal",
    "ValueTypes",
    "VariableReservation",
    "Variables",
];

/// Function names that read a value back out of an encoded form.
///
/// The list is owner-agnostic on purpose. A callable crate has no lawful
/// reason to decode anything, so naming one of these in production is a
/// finding regardless of which crate provides it; that is what makes an
/// arbitrary third-party decoder visible to this gate.
const DECODER_FUNCTIONS: &[&str] = &[
    "decode",
    "decode_from_slice",
    "decode_from_std_read",
    "deserialize",
    "from_bytes",
    "from_reader",
    "from_slice",
    "from_str",
    "from_value",
];

/// Serde traits a hand-written implementation could use to rebuild a checked
/// aggregate without its constructor.
const SERDE_TRAITS: &[&str] = &[
    "Deserialize",
    "DeserializeOwned",
    "DeserializeSeed",
    "Serialize",
    "Visitor",
];

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum BoundSymbol {
    DaeCrate,
    DaeOwner,
    DaeView,
    Decoder,
    MutationAuthority,
}

#[derive(Clone)]
struct UseBinding {
    alias: String,
    path: Vec<String>,
    glob: bool,
    public: bool,
}

/// One production Rust module of a callable crate, with the Cargo dependency
/// aliases in scope for it.
#[derive(Clone)]
pub(super) struct CallableModule {
    pub(super) label: String,
    pub(super) crate_aliases: BTreeMap<String, String>,
    pub(super) source: String,
}

#[derive(Default)]
struct AliasCatalog {
    bindings: BTreeMap<String, BoundSymbol>,
    crate_aliases: BTreeMap<String, String>,
    uses: Vec<UseBinding>,
    type_aliases: Vec<(String, syn::Type)>,
}

impl AliasCatalog {
    fn from_file(file: &syn::File, crate_aliases: &BTreeMap<String, String>) -> Self {
        let mut catalog = Self {
            crate_aliases: crate_aliases.clone(),
            ..Self::default()
        };
        AliasCollector {
            uses: &mut catalog.uses,
            type_aliases: &mut catalog.type_aliases,
        }
        .visit_file(file);
        catalog.resolve();
        catalog
    }

    fn resolve(&mut self) {
        while self.resolve_one_pass() {}
    }

    fn resolve_one_pass(&mut self) -> bool {
        let mut discovered = Vec::new();
        for binding in &self.uses {
            if let Some(symbol) = self.classify_segments(&binding.path) {
                discovered.push((binding.alias.clone(), symbol));
            }
        }
        for (alias, ty) in &self.type_aliases {
            if let syn::Type::Path(path) = ty
                && let Some(symbol) = self.classify_path(&path.path)
            {
                discovered.push((alias.clone(), symbol));
            }
        }
        let old_len = self.bindings.len();
        self.bindings.extend(discovered);
        self.bindings.len() != old_len
    }

    /// Whether `name` reaches the DAE crate root: by its own name, by a
    /// renamed Cargo dependency (`alias = { package = "rumoca-ir-dae" }`), or
    /// by an alias an `extern crate`/`use` already bound.
    fn names_dae_crate(&self, name: &str) -> bool {
        name == DAE_CRATE
            || self.crate_aliases.get(name).map(String::as_str) == Some(DAE_CRATE)
            || self.bindings.get(name) == Some(&BoundSymbol::DaeCrate)
    }

    fn classify_path(&self, path: &syn::Path) -> Option<BoundSymbol> {
        self.classify_segments(&path_segments(path))
    }

    fn classify_segments(&self, segments: &[String]) -> Option<BoundSymbol> {
        let first = segments.first()?;
        if segments.len() == 1 {
            return self
                .bindings
                .get(first)
                .copied()
                .or_else(|| self.names_dae_crate(first).then_some(BoundSymbol::DaeCrate));
        }
        if self.names_dae_crate(first) {
            return segments.last().and_then(|last| dae_symbol(last));
        }
        if matches!(first.as_str(), "crate" | "self" | "super") {
            return segments
                .last()
                .and_then(|last| self.bindings.get(last))
                .copied()
                .or_else(|| decoder_symbol(segments));
        }
        // A path ending in a decode function is a decoder whoever provides it.
        // Binding it here is what lets `use some_codec::from_slice as replay;`
        // make the bare alias `replay` resolve to a decoder at its call site.
        if let Some(symbol) = decoder_symbol(segments) {
            return Some(symbol);
        }
        self.bindings
            .get(first)
            .copied()
            .filter(|symbol| matches!(symbol, BoundSymbol::DaeCrate))
    }

    fn associated_owner(&self, path: &syn::Path) -> Option<BoundSymbol> {
        let mut segments = path_segments(path);
        segments.pop()?;
        self.classify_segments(&segments)
    }
}

fn path_segments(path: &syn::Path) -> Vec<String> {
    path.segments
        .iter()
        .map(|segment| segment.ident.to_string())
        .collect()
}

fn decoder_symbol(segments: &[String]) -> Option<BoundSymbol> {
    (segments.len() > 1
        && segments
            .last()
            .is_some_and(|last| DECODER_FUNCTIONS.contains(&last.as_str())))
    .then_some(BoundSymbol::Decoder)
}

fn dae_symbol(name: &str) -> Option<BoundSymbol> {
    match name {
        "Dae" => Some(BoundSymbol::DaeOwner),
        "DaeView" => Some(BoundSymbol::DaeView),
        name if MUTATION_AUTHORITIES.contains(&name) => Some(BoundSymbol::MutationAuthority),
        _ => None,
    }
}

struct AliasCollector<'a> {
    uses: &'a mut Vec<UseBinding>,
    type_aliases: &'a mut Vec<(String, syn::Type)>,
}

impl<'ast> Visit<'ast> for AliasCollector<'_> {
    fn visit_item(&mut self, item: &'ast syn::Item) {
        if item_attributes(item).is_some_and(attributes_require_test) {
            return;
        }
        visit::visit_item(self, item);
    }

    fn visit_impl_item(&mut self, item: &'ast syn::ImplItem) {
        if impl_item_attributes(item).is_some_and(attributes_require_test) {
            return;
        }
        visit::visit_impl_item(self, item);
    }

    fn visit_trait_item(&mut self, item: &'ast syn::TraitItem) {
        if trait_item_attributes(item).is_some_and(attributes_require_test) {
            return;
        }
        visit::visit_trait_item(self, item);
    }

    fn visit_item_use(&mut self, item: &'ast syn::ItemUse) {
        let public = !matches!(item.vis, syn::Visibility::Inherited);
        flatten_use_tree(&item.tree, Vec::new(), public, self.uses);
    }

    /// `extern crate rumoca_ir_dae as dae;` binds the crate root exactly as a
    /// `use` does, so it enters the same alias pool.
    fn visit_item_extern_crate(&mut self, item: &'ast syn::ItemExternCrate) {
        let target = item.ident.to_string();
        let alias = item
            .rename
            .as_ref()
            .map_or_else(|| target.clone(), |(_, rename)| rename.to_string());
        self.uses.push(UseBinding {
            alias,
            path: vec![target],
            glob: false,
            public: !matches!(item.vis, syn::Visibility::Inherited),
        });
    }

    fn visit_item_type(&mut self, item: &'ast syn::ItemType) {
        self.type_aliases
            .push((item.ident.to_string(), (*item.ty).clone()));
    }
}

fn flatten_use_tree(
    tree: &syn::UseTree,
    prefix: Vec<String>,
    public: bool,
    bindings: &mut Vec<UseBinding>,
) {
    match tree {
        syn::UseTree::Path(path) => {
            let mut next = prefix;
            next.push(path.ident.to_string());
            flatten_use_tree(&path.tree, next, public, bindings);
        }
        syn::UseTree::Name(name) => {
            let mut path = prefix;
            path.push(name.ident.to_string());
            bindings.push(UseBinding {
                alias: name.ident.to_string(),
                path,
                glob: false,
                public,
            });
        }
        syn::UseTree::Rename(rename) => {
            let mut path = prefix;
            path.push(rename.ident.to_string());
            bindings.push(UseBinding {
                alias: rename.rename.to_string(),
                path,
                glob: false,
                public,
            });
        }
        syn::UseTree::Glob(_) => bindings.push(UseBinding {
            alias: "*".to_owned(),
            path: prefix,
            glob: true,
            public,
        }),
        syn::UseTree::Group(group) => {
            for item in &group.items {
                flatten_use_tree(item, prefix.clone(), public, bindings);
            }
        }
    }
}

struct BoundaryVisitor<'a> {
    catalog: &'a AliasCatalog,
    findings: BTreeSet<String>,
}

impl BoundaryVisitor<'_> {
    fn record(&mut self, message: impl Into<String>) {
        self.findings.insert(message.into());
    }

    /// The retained DAE owner may never be returned at any visibility, and a
    /// borrowed `DaeView` may never be returned by anything a caller outside
    /// the defining module can name.
    fn check_output(&mut self, owner: &str, escapes_module: bool, output: &syn::ReturnType) {
        let syn::ReturnType::Type(_, ty) = output else {
            return;
        };
        if type_names(ty, self.catalog, &[BoundSymbol::DaeOwner]) {
            self.record(format!("`{owner}` returns the retained Dae owner"));
            return;
        }
        if escapes_module && type_names(ty, self.catalog, &[BoundSymbol::DaeView]) {
            self.record(format!(
                "`{owner}` returns raw DaeView authority beyond its module"
            ));
        }
    }

    fn check_serde_impl(&mut self, item: &syn::ItemImpl) {
        let Some((_, path, _)) = item.trait_.as_ref() else {
            return;
        };
        let Some(trait_name) = path
            .segments
            .last()
            .map(|segment| segment.ident.to_string())
        else {
            return;
        };
        if SERDE_TRAITS.contains(&trait_name.as_str()) {
            self.record(format!(
                "production code hand-implements serde `{trait_name}`"
            ));
        }
    }

    fn check_serde_derives(&mut self, item: &syn::Item) {
        let Some(attributes) = item_attributes(item) else {
            return;
        };
        for trait_name in SERDE_TRAITS {
            if derives_trait(attributes, trait_name) {
                self.record(format!("production code derives serde `{trait_name}`"));
            }
        }
    }
}

impl<'ast> Visit<'ast> for BoundaryVisitor<'_> {
    fn visit_item(&mut self, item: &'ast syn::Item) {
        if item_attributes(item).is_some_and(attributes_require_test) {
            return;
        }
        self.check_serde_derives(item);
        visit::visit_item(self, item);
    }

    fn visit_impl_item(&mut self, item: &'ast syn::ImplItem) {
        if impl_item_attributes(item).is_some_and(attributes_require_test) {
            return;
        }
        visit::visit_impl_item(self, item);
    }

    fn visit_trait_item(&mut self, item: &'ast syn::TraitItem) {
        if trait_item_attributes(item).is_some_and(attributes_require_test) {
            return;
        }
        visit::visit_trait_item(self, item);
    }

    fn visit_local(&mut self, local: &'ast syn::Local) {
        if attributes_require_test(&local.attrs) {
            return;
        }
        visit::visit_local(self, local);
    }

    fn visit_item_use(&mut self, item: &'ast syn::ItemUse) {
        let mut uses = Vec::new();
        flatten_use_tree(
            &item.tree,
            Vec::new(),
            !matches!(item.vis, syn::Visibility::Inherited),
            &mut uses,
        );
        for binding in uses {
            let public = binding.public;
            let names_crate = binding
                .path
                .first()
                .is_some_and(|name| self.catalog.names_dae_crate(name));
            let symbol = self.catalog.classify_segments(&binding.path);
            if binding.glob && (names_crate || symbol == Some(BoundSymbol::DaeCrate)) {
                self.record("glob import exposes all DAE construction authorities");
            }
            if symbol == Some(BoundSymbol::MutationAuthority) {
                self.record(format!(
                    "import `{}` references DAE mutation authority",
                    binding.path.join("::")
                ));
            }
            if public && symbol == Some(BoundSymbol::DaeCrate) {
                self.record(format!(
                    "public import `{}` re-exports the whole DAE crate",
                    binding.path.join("::")
                ));
            }
            if public && matches!(symbol, Some(BoundSymbol::DaeOwner | BoundSymbol::DaeView)) {
                self.record(format!(
                    "public import `{}` re-exports raw Dae/DaeView",
                    binding.path.join("::")
                ));
            }
        }
    }

    fn visit_item_extern_crate(&mut self, item: &'ast syn::ItemExternCrate) {
        let target = item.ident.to_string();
        if self.catalog.names_dae_crate(&target) {
            self.record(format!("`extern crate {target}` rebinds the DAE crate"));
        }
        visit::visit_item_extern_crate(self, item);
    }

    fn visit_path(&mut self, path: &'ast syn::Path) {
        match self.catalog.classify_path(path) {
            Some(BoundSymbol::MutationAuthority) => {
                self.record("production code references DAE mutation authority");
            }
            // Catches a spelled-out `codec::from_slice` and, because the alias
            // pool carries the same symbol, a bare alias an earlier `use`
            // bound to one.
            Some(BoundSymbol::Decoder) => self.record("production code references a decoder"),
            _ => {}
        }
        if let Some(method) = path
            .segments
            .last()
            .map(|segment| segment.ident.to_string())
            && matches!(method.as_str(), "construct" | "deserialize" | "decode")
            && self.catalog.associated_owner(path) == Some(BoundSymbol::DaeOwner)
        {
            self.record(format!("production code calls Dae::{method}"));
        }
        visit::visit_path(self, path);
    }

    fn visit_expr_method_call(&mut self, expression: &'ast syn::ExprMethodCall) {
        if DECODER_FUNCTIONS.contains(&expression.method.to_string().as_str()) {
            self.record("production code invokes a decoder method");
        }
        visit::visit_expr_method_call(self, expression);
    }

    fn visit_expr_path(&mut self, expression: &'ast syn::ExprPath) {
        let authority_method = expression.path.segments.last().is_some_and(|segment| {
            matches!(
                segment.ident.to_string().as_str(),
                "construct" | "decode" | "deserialize"
            )
        });
        if authority_method
            && expression.qself.as_ref().is_some_and(|qualified| {
                type_names(
                    &qualified.ty,
                    self.catalog,
                    &[BoundSymbol::DaeOwner, BoundSymbol::DaeView],
                )
            })
        {
            self.record("qualified Dae call reacquires construction/decode authority");
        }
        visit::visit_expr_path(self, expression);
    }

    fn visit_macro(&mut self, item: &'ast syn::Macro) {
        let tokens = item.tokens.to_string();
        let identifiers = tokens
            .split(|character: char| !(character.is_alphanumeric() || character == '_'))
            .filter(|identifier| !identifier.is_empty())
            .collect::<BTreeSet<_>>();
        if MUTATION_AUTHORITIES
            .iter()
            .any(|authority| identifiers.contains(authority))
        {
            self.record("production macro references DAE mutation authority");
        }
        let mut crate_names = vec![DAE_CRATE.to_owned()];
        crate_names.extend(self.catalog.crate_aliases.keys().cloned());
        crate_names.extend(
            self.catalog
                .bindings
                .iter()
                .filter(|(_, symbol)| **symbol == BoundSymbol::DaeCrate)
                .map(|(alias, _)| alias.clone()),
        );
        let constructs_dae = self.catalog.bindings.iter().any(|(alias, symbol)| {
            *symbol == BoundSymbol::DaeOwner && tokens.contains(&format!("{alias} :: construct"))
        }) || crate_names.iter().any(|alias| {
            self.catalog.names_dae_crate(alias)
                && tokens.contains(&format!("{alias} :: Dae :: construct"))
        });
        if constructs_dae {
            self.record("production macro calls Dae::construct");
        }
        if item
            .path
            .segments
            .last()
            .is_some_and(|segment| segment.ident == "include")
        {
            self.record("production include! bypasses the reviewed module graph");
        }
        visit::visit_macro(self, item);
    }

    fn visit_item_fn(&mut self, function: &'ast syn::ItemFn) {
        self.check_output(
            &function.sig.ident.to_string(),
            !matches!(function.vis, syn::Visibility::Inherited),
            &function.sig.output,
        );
        visit::visit_item_fn(self, function);
    }

    fn visit_impl_item_fn(&mut self, function: &'ast syn::ImplItemFn) {
        self.check_output(
            &function.sig.ident.to_string(),
            !matches!(function.vis, syn::Visibility::Inherited),
            &function.sig.output,
        );
        visit::visit_impl_item_fn(self, function);
    }

    fn visit_trait_item_fn(&mut self, function: &'ast syn::TraitItemFn) {
        // A trait method is as visible as its trait, so it always escapes
        // the defining module.
        self.check_output(&function.sig.ident.to_string(), true, &function.sig.output);
        visit::visit_trait_item_fn(self, function);
    }

    fn visit_item_struct(&mut self, item: &'ast syn::ItemStruct) {
        if item.ident == "CallablePlan" && derives_trait(&item.attrs, "Clone") {
            self.record("CallablePlan derives Clone");
        }
        for field in &item.fields {
            if !matches!(field.vis, syn::Visibility::Inherited)
                && type_names(
                    &field.ty,
                    self.catalog,
                    &[BoundSymbol::DaeOwner, BoundSymbol::DaeView],
                )
            {
                self.record(format!(
                    "`{}` field exposes Dae/DaeView beyond its module",
                    item.ident
                ));
            }
        }
        visit::visit_item_struct(self, item);
    }

    fn visit_item_enum(&mut self, item: &'ast syn::ItemEnum) {
        if !matches!(item.vis, syn::Visibility::Inherited)
            && item
                .variants
                .iter()
                .flat_map(|variant| &variant.fields)
                .any(|field| {
                    type_names(
                        &field.ty,
                        self.catalog,
                        &[BoundSymbol::DaeOwner, BoundSymbol::DaeView],
                    )
                })
        {
            self.record(format!(
                "enum `{}` exposes Dae/DaeView beyond its module",
                item.ident
            ));
        }
        visit::visit_item_enum(self, item);
    }

    fn visit_item_union(&mut self, item: &'ast syn::ItemUnion) {
        if !matches!(item.vis, syn::Visibility::Inherited)
            && item.fields.named.iter().any(|field| {
                type_names(
                    &field.ty,
                    self.catalog,
                    &[BoundSymbol::DaeOwner, BoundSymbol::DaeView],
                )
            })
        {
            self.record(format!(
                "union `{}` exposes Dae/DaeView beyond its module",
                item.ident
            ));
        }
        visit::visit_item_union(self, item);
    }

    fn visit_item_type(&mut self, item: &'ast syn::ItemType) {
        if !matches!(item.vis, syn::Visibility::Inherited)
            && type_names(
                &item.ty,
                self.catalog,
                &[BoundSymbol::DaeOwner, BoundSymbol::DaeView],
            )
        {
            self.record(format!(
                "type `{}` exposes Dae/DaeView beyond its module",
                item.ident
            ));
        }
        visit::visit_item_type(self, item);
    }

    fn visit_item_impl(&mut self, item: &'ast syn::ItemImpl) {
        let callable_plan = type_path_last(&item.self_ty).as_deref() == Some("CallablePlan");
        let clone = item
            .trait_
            .as_ref()
            .and_then(|(_, path, _)| path.segments.last())
            .is_some_and(|segment| segment.ident == "Clone");
        if callable_plan && clone {
            self.record("CallablePlan implements Clone");
        }
        self.check_serde_impl(item);
        visit::visit_item_impl(self, item);
    }
}

fn type_names(ty: &syn::Type, catalog: &AliasCatalog, wanted: &[BoundSymbol]) -> bool {
    struct TypeVisitor<'a> {
        catalog: &'a AliasCatalog,
        wanted: &'a [BoundSymbol],
        found: bool,
    }
    impl<'ast> Visit<'ast> for TypeVisitor<'_> {
        fn visit_path(&mut self, path: &'ast syn::Path) {
            if let Some(symbol) = self.catalog.classify_path(path) {
                self.found |= self.wanted.contains(&symbol);
            }
            visit::visit_path(self, path);
        }
    }
    let mut visitor = TypeVisitor {
        catalog,
        wanted,
        found: false,
    };
    visitor.visit_type(ty);
    visitor.found
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

fn derives_trait(attributes: &[syn::Attribute], wanted: &str) -> bool {
    attributes.iter().any(|attribute| {
        if !attribute.path().is_ident("derive") {
            return false;
        }
        attribute
            .parse_args_with(
                syn::punctuated::Punctuated::<syn::Path, syn::Token![,]>::parse_terminated,
            )
            .is_ok_and(|traits| {
                traits.iter().any(|path| {
                    path.segments
                        .last()
                        .is_some_and(|segment| segment.ident == wanted)
                })
            })
    })
}

fn item_attributes(item: &syn::Item) -> Option<&[syn::Attribute]> {
    match item {
        syn::Item::Const(item) => Some(&item.attrs),
        syn::Item::Enum(item) => Some(&item.attrs),
        syn::Item::ExternCrate(item) => Some(&item.attrs),
        syn::Item::Fn(item) => Some(&item.attrs),
        syn::Item::ForeignMod(item) => Some(&item.attrs),
        syn::Item::Impl(item) => Some(&item.attrs),
        syn::Item::Macro(item) => Some(&item.attrs),
        syn::Item::Mod(item) => Some(&item.attrs),
        syn::Item::Static(item) => Some(&item.attrs),
        syn::Item::Struct(item) => Some(&item.attrs),
        syn::Item::Trait(item) => Some(&item.attrs),
        syn::Item::TraitAlias(item) => Some(&item.attrs),
        syn::Item::Type(item) => Some(&item.attrs),
        syn::Item::Union(item) => Some(&item.attrs),
        syn::Item::Use(item) => Some(&item.attrs),
        _ => None,
    }
}

fn impl_item_attributes(item: &syn::ImplItem) -> Option<&[syn::Attribute]> {
    match item {
        syn::ImplItem::Const(item) => Some(&item.attrs),
        syn::ImplItem::Fn(item) => Some(&item.attrs),
        syn::ImplItem::Type(item) => Some(&item.attrs),
        syn::ImplItem::Macro(item) => Some(&item.attrs),
        _ => None,
    }
}

fn trait_item_attributes(item: &syn::TraitItem) -> Option<&[syn::Attribute]> {
    match item {
        syn::TraitItem::Const(item) => Some(&item.attrs),
        syn::TraitItem::Fn(item) => Some(&item.attrs),
        syn::TraitItem::Type(item) => Some(&item.attrs),
        syn::TraitItem::Macro(item) => Some(&item.attrs),
        _ => None,
    }
}

/// Findings over a whole crate: every module's aliases join one pool before
/// any module is judged, so a cross-module alias cannot hide an authority.
fn crate_boundary_findings(modules: &[CallableModule]) -> BTreeSet<String> {
    let parsed = modules
        .iter()
        .map(|module| {
            let syntax = syn::parse_file(&module.source)
                .unwrap_or_else(|error| panic!("parse {}: {error}", module.label));
            let catalog = AliasCatalog::from_file(&syntax, &module.crate_aliases);
            (module, syntax, catalog)
        })
        .collect::<Vec<_>>();

    let mut pool = BTreeMap::new();
    let mut pooled_aliases = BTreeMap::new();
    for (module, _, catalog) in &parsed {
        pool.extend(catalog.bindings.clone());
        pooled_aliases.extend(module.crate_aliases.clone());
    }

    let mut findings = BTreeSet::new();
    for (module, syntax, catalog) in &parsed {
        let mut catalog = AliasCatalog {
            bindings: pool.clone(),
            crate_aliases: pooled_aliases.clone(),
            uses: catalog.uses.clone(),
            type_aliases: catalog.type_aliases.clone(),
        };
        catalog.resolve();
        let mut visitor = BoundaryVisitor {
            catalog: &catalog,
            findings: BTreeSet::new(),
        };
        visitor.visit_file(syntax);
        findings.extend(
            visitor
                .findings
                .into_iter()
                .map(|finding| format!("{}: {finding}", module.label)),
        );
    }
    findings
}

/// Findings for one standalone fixture, with no Cargo aliases in scope.
fn callable_boundary_findings(source: &str) -> BTreeSet<String> {
    crate_boundary_findings(&[CallableModule {
        label: "fixture.rs".to_owned(),
        crate_aliases: BTreeMap::new(),
        source: source.to_owned(),
    }])
}

/// Every production module of one callable crate. Alias pooling stays inside
/// a crate, because a `crate::` path never resolves across a crate boundary.
fn callable_production_modules(crate_name: &str) -> Vec<CallableModule> {
    let root = super::super::workspace_root();
    let crate_root = root.join("crates").join(crate_name);
    production_rust_source_contexts(&crate_root, &root)
        .into_iter()
        .map(|context| CallableModule {
            label: context.path.display().to_string(),
            crate_aliases: context.crate_aliases,
            source: context.source,
        })
        .collect()
}

#[test]
fn callable_crates_cannot_reacquire_dae_construction_or_replay_authority() {
    let findings = CALLABLE_CRATES
        .iter()
        .flat_map(|crate_name| crate_boundary_findings(&callable_production_modules(crate_name)))
        .collect::<BTreeSet<_>>();
    assert!(
        findings.is_empty(),
        "callable production crossed its retained-inspection-only DAE authority boundary: {findings:#?}"
    );
}

#[test]
fn callable_boundary_detects_direct_alias_reexport_decode_and_escape_mutations() {
    for mutation in [
        "fn bad() { let _ = rumoca_ir_dae::Dae::construct(source(), |_| Ok(())); }",
        "use rumoca_ir_dae::Dae as Root; fn bad() { let _ = Root::construct(source(), |_| Ok(())); }",
        "use rumoca_ir_dae::DaeConstruction as Builder; fn bad(_: &mut Builder<'_>) {}",
        "pub use rumoca_ir_dae::DaeConstruction as Builder;",
        "fn bad(bytes: &[u8]) { let _: rumoca_ir_dae::Dae = serde_json::from_slice(bytes).unwrap(); }",
        "use serde_json::from_slice as replay; fn bad(bytes: &[u8]) { let _ = replay::<rumoca_ir_dae::Dae>(bytes); }",
        "fn bad() { let _ = <rumoca_ir_dae::Dae as serde::Deserialize>::deserialize(input()); }",
        "use rumoca_ir_dae::Dae; pub fn leak(root: Dae) -> Dae { root }",
        "pub use rumoca_ir_dae::Dae as EscapedDae;",
        "use rumoca_ir_dae::Dae as Root; mod export { pub use super::Root as Leaked; }",
        "#[derive(Clone)] pub struct CallablePlan;",
        "macro_rules! bad { () => { rumoca_ir_dae::Dae::construct(source(), |_| Ok(())) } }",
    ] {
        assert!(
            !callable_boundary_findings(mutation).is_empty(),
            "callable authority mutation escaped: {mutation}"
        );
    }
}

#[test]
fn callable_boundary_detects_restricted_and_private_dae_owner_returns() {
    for mutation in [
        "use rumoca_ir_dae::Dae; pub(crate) fn leak(root: Dae) -> Dae { root }",
        "use rumoca_ir_dae::Dae; pub(super) fn leak(root: Dae) -> Dae { root }",
        "use rumoca_ir_dae::Dae; pub(in crate::plan) fn leak(root: Dae) -> Dae { root }",
        "use rumoca_ir_dae::Dae; fn leak(root: Dae) -> Dae { root }",
        "use rumoca_ir_dae::Dae; struct Holder; impl Holder { fn leak(self, root: Dae) -> Dae { root } }",
        "use rumoca_ir_dae::DaeView; pub(crate) fn lend<'a>(v: DaeView<'a>) -> DaeView<'a> { v }",
        "use rumoca_ir_dae::Dae; pub(crate) struct Holder { pub(crate) dae: Dae }",
    ] {
        assert!(
            !callable_boundary_findings(mutation).is_empty(),
            "restricted or private DAE owner escape mutation escaped: {mutation}"
        );
    }
}

#[test]
fn callable_boundary_detects_extern_crate_whole_crate_and_renamed_dependency_routes() {
    for mutation in [
        "extern crate rumoca_ir_dae;",
        "extern crate rumoca_ir_dae as dae; fn bad() { let _ = dae::Dae::construct(s(), |_| Ok(())); }",
        "pub use rumoca_ir_dae;",
        "pub use rumoca_ir_dae::*;",
        "use rumoca_ir_dae as dae; pub use dae::Dae as Escaped;",
    ] {
        assert!(
            !callable_boundary_findings(mutation).is_empty(),
            "crate-level rebinding mutation escaped: {mutation}"
        );
    }

    // A renamed Cargo dependency spells the DAE crate under another ident. The
    // scan learns the rename from the manifest, so the alias resolves to the
    // same authority as the real name.
    let renamed = crate_boundary_findings(&[CallableModule {
        label: "fixture.rs".to_owned(),
        crate_aliases: BTreeMap::from([("innocent".to_owned(), "rumoca_ir_dae".to_owned())]),
        source: "use innocent::DaeConstruction as Builder; fn bad(_: &mut Builder<'_>) {}"
            .to_owned(),
    }]);
    assert!(
        !renamed.is_empty(),
        "a renamed Cargo dependency hid the DAE crate"
    );
}

#[test]
fn callable_boundary_detects_cross_module_aliases_and_arbitrary_decoders() {
    // The alias is introduced in one module and used from another; only a
    // crate-wide pool sees the connection.
    let cross_module = crate_boundary_findings(&[
        CallableModule {
            label: "alias.rs".to_owned(),
            crate_aliases: BTreeMap::new(),
            source: "pub(crate) use rumoca_ir_dae::Dae as Root;".to_owned(),
        },
        CallableModule {
            label: "user.rs".to_owned(),
            crate_aliases: BTreeMap::new(),
            source: "fn bad() { let _ = crate::alias::Root::construct(s(), |_| Ok(())); }"
                .to_owned(),
        },
    ]);
    assert!(
        cross_module
            .iter()
            .any(|finding| finding.starts_with("user.rs")),
        "a parent/child module alias hid Dae::construct: {cross_module:#?}"
    );

    for mutation in [
        "fn bad(bytes: &[u8]) { let _ = exotic_codec::from_slice(bytes); }",
        "fn bad(bytes: &[u8]) { let _ = vendor::wire::decode_from_slice(bytes); }",
        "fn bad(reader: R) { let _ = reader.deserialize(); }",
        "fn bad(reader: R) { let _ = reader.from_reader(); }",
        "#[derive(serde::Deserialize)] pub struct Plan;",
        "#[derive(Serialize)] pub struct Plan;",
        "impl<'de> serde::Deserialize<'de> for Plan { fn deserialize<D>(_: D) -> R { r() } }",
        "impl serde::Serialize for Plan { fn serialize<S>(&self, _: S) -> R { r() } }",
    ] {
        assert!(
            !callable_boundary_findings(mutation).is_empty(),
            "decoder or serde mutation escaped: {mutation}"
        );
    }
}

#[test]
fn callable_boundary_allows_retention_and_skips_only_cfg_test_bodies() {
    let allowed = r#"
        use rumoca_ir_dae::{Dae, DaeView, DiscreteValueBranches};
        pub struct Plan { dae: Dae }
        impl Plan {
            pub fn inspect<R>(&self, read: impl for<'dae> FnOnce(DaeView<'dae>) -> R) -> R {
                self.dae.inspect(read)
            }
        }
        pub fn transfer(dae: Dae) -> Plan { Plan { dae } }
        fn inspect_branches(_: DiscreteValueBranches<'_>) {}
        #[cfg(test)]
        mod tests {
            fn fixture() { let _ = rumoca_ir_dae::Dae::construct(source(), |_| Ok(())); }
        }
    "#;
    assert!(callable_boundary_findings(allowed).is_empty());

    let production_after_test = format!(
        "{allowed}\nfn bad() {{ let _ = rumoca_ir_dae::Dae::construct(source(), |_| Ok(())); }}"
    );
    assert!(!callable_boundary_findings(&production_after_test).is_empty());
}

#[test]
fn callable_boundary_still_scans_cfg_any_test_feature_items() {
    let scanned = r#"
        #[cfg(any(test, feature = "replay"))]
        mod maybe_production {
            fn bad() { let _ = rumoca_ir_dae::Dae::construct(source(), |_| Ok(())); }
        }
    "#;
    assert!(
        !callable_boundary_findings(scanned).is_empty(),
        "`any(test, feature = ...)` does not imply a test build and must stay scanned"
    );
}
