//! Reference resolution behind the dead public-surface gate.
//!
//! The gate credits a public declaration only when some workspace reference
//! resolves *to that declaration*, where a declaration's identity is
//! `(crate, module path, name)`. Counting how often a bare identifier occurs
//! anywhere in the workspace is not resolution: two crates that both name
//! something `helpers` would vouch for each other, and one live `syn::parse_file`
//! call would keep every unrelated `parse_file` alive.
//!
//! Names are collected from the constructs that can actually reach a
//! declaration (paths, `use` trees, method calls, macro token streams), never
//! from a bare identifier visit, because that would also count field names and
//! binding patterns.

use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};
use syn::visit::Visit;

/// Attributes whose presence *is* the external contract: the caller lives
/// outside Rust, so no workspace reference can resolve to the item and its
/// deadness cannot be decided by this gate.
const FFI_EXPORT_ATTRIBUTES: &[&str] = &[
    "wasm_bindgen",
    "pyfunction",
    "pymethods",
    "pyclass",
    "pymodule",
    "pyo3",
    "no_mangle",
    "export_name",
];

/// Path segments that name a module relative to where the reference is written
/// rather than a module of their own.
const RELATIVE_PATH_SEGMENTS: &[&str] = &["self", "super", "crate"];

/// The identity a caller has to name to reach a public item: its crate, the
/// module path inside that crate (with the implementing type appended for
/// inherent methods) and the item name.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(super) struct Declaration {
    krate: String,
    module: Vec<String>,
    pub(super) name: String,
    pub(super) path: PathBuf,
    /// The item is exported to a non-Rust caller, so no workspace reference can
    /// resolve to it.
    ffi_exported: bool,
}

/// One written occurrence of a name, with the path segments that qualify it.
#[derive(Clone, Debug)]
struct Reference {
    /// Crate the reference is written in, when it is Rust source under
    /// `crates/`.
    krate: Option<String>,
    /// Path segments before the name: `rumoca_core::Span` qualifies `Span` with
    /// `["rumoca_core"]`.
    qualifier: Vec<String>,
    /// A method call (`value.step()`) carries no path to resolve, so it is
    /// credited to every declaration of the name.
    unqualifiable: bool,
}

impl Reference {
    /// Does this reference name *this* declaration?
    ///
    /// A qualified path resolves by identity: the crate ident (`rumoca_core::…`)
    /// or the owning module or implementing type (`errors::…`, `Session::…`,
    /// with `Self` and `use … as` aliases already substituted). A bare name
    /// resolves inside its own crate, through a relative path segment, or when
    /// exactly one crate declares the name, so a plain call still counts while a
    /// same-named item in an unrelated crate no longer vouches for it.
    fn resolves_to(&self, declaration: &Declaration, declared_in_one_crate: bool) -> bool {
        if self.unqualifiable {
            return true;
        }
        if self.qualifier.contains(&declaration.krate) {
            return true;
        }
        let owner = declaration.module.last();
        if owner.is_some() && self.qualifier.last() == owner {
            return true;
        }
        let same_crate = self.krate.as_deref() == Some(declaration.krate.as_str());
        match self.qualifier.last() {
            None => same_crate || declared_in_one_crate,
            Some(segment) => same_crate && RELATIVE_PATH_SEGMENTS.contains(&segment.as_str()),
        }
    }
}

/// Declarations and references, indexed by name so resolution stays linear in
/// the number of references that share a name with a declaration.
#[derive(Default)]
pub(super) struct Corpus {
    pub(super) declarations: Vec<Declaration>,
    references: BTreeMap<String, Vec<Reference>>,
    /// Names written by a non-Rust caller, which resolve to no module path.
    text_names: BTreeSet<String>,
}

impl Corpus {
    pub(super) fn add_rust_source(&mut self, krate: Option<&str>, path: &Path, source: &str) {
        let file = syn::parse_file(source)
            .unwrap_or_else(|error| panic!("parse {} for the API audit: {error}", path.display()));
        let mut aliases = AliasScan::default();
        aliases.visit_file(&file);
        let mut scan = SourceScan {
            krate: krate.unwrap_or_default().to_string(),
            in_crate: krate.is_some(),
            module: module_path(path),
            implementing_type: None,
            ffi_scope: 0,
            aliases: aliases.aliases,
            path: path.to_path_buf(),
            declarations: Vec::new(),
            references: Vec::new(),
        };
        scan.visit_file(&file);
        self.declarations.extend(scan.declarations);
        for (name, reference) in scan.references {
            self.references.entry(name).or_default().push(reference);
        }
    }

    /// Every identifier-shaped word of a non-Rust caller. Nothing in those
    /// languages resolves to a Rust module path, so each word is credited to
    /// every declaration that carries it.
    pub(super) fn add_text_source(&mut self, source: &str) {
        for word in source.split(|ch: char| !(ch.is_alphanumeric() || ch == '_')) {
            if !word.is_empty() {
                self.text_names.insert(word.to_string());
            }
        }
    }

    fn declaring_crates(&self) -> BTreeMap<&str, BTreeSet<&str>> {
        let mut crates: BTreeMap<&str, BTreeSet<&str>> = BTreeMap::new();
        for declaration in &self.declarations {
            crates
                .entry(declaration.name.as_str())
                .or_default()
                .insert(declaration.krate.as_str());
        }
        crates
    }

    /// Does any reference in the corpus resolve to this declaration?
    fn is_reached(&self, declaration: &Declaration, declared_in_one_crate: bool) -> bool {
        if declaration.ffi_exported || self.text_names.contains(&declaration.name) {
            return true;
        }
        let Some(references) = self.references.get(&declaration.name) else {
            return false;
        };
        references
            .iter()
            .any(|reference| reference.resolves_to(declaration, declared_in_one_crate))
    }

    /// Public declarations that no reference in the corpus resolves to.
    pub(super) fn unreferenced_public_declarations(&self) -> BTreeSet<(PathBuf, String)> {
        let declaring_crates = self.declaring_crates();
        self.declarations
            .iter()
            .filter(|declaration| {
                let declared_in_one_crate = declaring_crates
                    .get(declaration.name.as_str())
                    .is_some_and(|crates| crates.len() == 1);
                !self.is_reached(declaration, declared_in_one_crate)
            })
            .map(|declaration| (declaration.path.clone(), declaration.name.clone()))
            .collect()
    }
}

/// `crates/rumoca-core/src/lib.rs` -> `rumoca_core`.
pub(super) fn crate_name(path: &Path) -> Option<String> {
    let mut components = path.components().map(|it| it.as_os_str().to_string_lossy());
    components.find(|it| it == "crates")?;
    let directory = components.next()?;
    Some(directory.replace('-', "_"))
}

/// Module path a file owns inside its crate: `src/session/adapters.rs` ->
/// `["session", "adapters"]`, `src/runtime/mod.rs` -> `["runtime"]`.
fn module_path(path: &Path) -> Vec<String> {
    let mut segments: Vec<String> = Vec::new();
    let mut inside_src = false;
    for component in path.components() {
        let name = component.as_os_str().to_string_lossy();
        if !inside_src {
            inside_src = name == "src";
            continue;
        }
        segments.push(name.trim_end_matches(".rs").to_string());
    }
    if segments
        .last()
        .is_some_and(|last| last == "mod" || last == "lib" || last == "main")
    {
        segments.pop();
    }
    segments
}

/// Does any of these attributes hand the item to a non-Rust caller?
fn exports_to_foreign_caller(attrs: &[syn::Attribute]) -> bool {
    attrs.iter().any(|attribute| {
        attribute
            .path()
            .segments
            .iter()
            .any(|segment| FFI_EXPORT_ATTRIBUTES.contains(&segment.ident.to_string().as_str()))
    })
}

/// The `a::b::C` path being read out of a macro token stream, which carries no
/// syntax tree of its own.
#[derive(Default)]
struct TokenPath {
    segments: Vec<String>,
    colons: usize,
}

impl TokenPath {
    fn restart(&mut self) {
        self.segments.clear();
        self.colons = 0;
    }

    fn punctuate(&mut self, punctuation: char) {
        if punctuation == ':' {
            self.colons += 1;
        } else {
            self.restart();
        }
    }

    /// Segments qualifying the identifier that comes next; a name not preceded
    /// by `::` starts a new path.
    fn qualifier(&mut self) -> Vec<String> {
        if self.colons == 2 {
            return self.segments.clone();
        }
        self.segments.clear();
        Vec::new()
    }

    fn extend(&mut self, segment: String) {
        self.segments.push(segment);
        self.colons = 0;
    }
}

/// Module aliases a file introduces: `use crate::diagnostic_codes as codes;`
/// makes `codes::EL001` a reference into `diagnostic_codes`.
#[derive(Default)]
struct AliasScan {
    aliases: BTreeMap<String, String>,
}

impl<'ast> Visit<'ast> for AliasScan {
    fn visit_use_rename(&mut self, node: &'ast syn::UseRename) {
        self.aliases
            .insert(node.rename.to_string(), node.ident.to_string());
    }
}

/// Collects declarations and references from one parsed file.
struct SourceScan {
    krate: String,
    in_crate: bool,
    module: Vec<String>,
    implementing_type: Option<String>,
    /// Nesting depth of enclosing `impl`/`mod` items carrying an FFI export
    /// attribute.
    ffi_scope: usize,
    aliases: BTreeMap<String, String>,
    path: PathBuf,
    declarations: Vec<Declaration>,
    references: Vec<(String, Reference)>,
}

impl SourceScan {
    /// Module path an item declared here belongs to, with the implementing type
    /// appended for inherent methods.
    fn owning_module(&self) -> Vec<String> {
        let mut module = self.module.clone();
        module.extend(self.implementing_type.clone());
        module
    }

    fn declare(
        &mut self,
        visibility: &syn::Visibility,
        ident: &syn::Ident,
        attrs: &[syn::Attribute],
    ) {
        if self.in_crate && matches!(visibility, syn::Visibility::Public(_)) {
            self.declarations.push(Declaration {
                krate: self.krate.clone(),
                module: self.owning_module(),
                name: ident.to_string(),
                path: self.path.clone(),
                ffi_exported: self.ffi_scope > 0 || exports_to_foreign_caller(attrs),
            });
        }
    }

    /// Rewrites the segments that name something relative to this file: `Self`
    /// is the implementing type, and an aliased import is its real module.
    fn resolve_qualifier(&self, qualifier: Vec<String>) -> Vec<String> {
        qualifier
            .into_iter()
            .map(|segment| {
                if segment == "Self" {
                    return self.implementing_type.clone().unwrap_or(segment);
                }
                self.aliases.get(&segment).cloned().unwrap_or(segment)
            })
            .collect()
    }

    fn refer(&mut self, name: String, qualifier: Vec<String>, unqualifiable: bool) {
        let krate = self.in_crate.then(|| self.krate.clone());
        let qualifier = self.resolve_qualifier(qualifier);
        self.references.push((
            name,
            Reference {
                krate,
                qualifier,
                unqualifiable,
            },
        ));
    }

    fn refer_use_tree(&mut self, prefix: &mut Vec<String>, tree: &syn::UseTree) {
        match tree {
            syn::UseTree::Path(node) => {
                let segment = node.ident.to_string();
                self.refer(segment.clone(), prefix.clone(), false);
                prefix.push(segment);
                self.refer_use_tree(prefix, &node.tree);
                prefix.pop();
            }
            syn::UseTree::Name(node) => self.refer(node.ident.to_string(), prefix.clone(), false),
            syn::UseTree::Rename(node) => self.refer(node.ident.to_string(), prefix.clone(), false),
            syn::UseTree::Group(node) => {
                for item in &node.items {
                    self.refer_use_tree(prefix, item);
                }
            }
            syn::UseTree::Glob(_) => {}
        }
    }

    fn refer_tokens(&mut self, tokens: proc_macro2::TokenStream) {
        let mut walk = TokenPath::default();
        for token in tokens {
            self.refer_token(token, &mut walk);
        }
    }

    fn refer_token(&mut self, token: proc_macro2::TokenTree, walk: &mut TokenPath) {
        match token {
            proc_macro2::TokenTree::Ident(ident) => {
                let name = ident.to_string();
                let qualifier = walk.qualifier();
                self.refer(name.clone(), qualifier, false);
                walk.extend(name);
            }
            proc_macro2::TokenTree::Punct(punct) => walk.punctuate(punct.as_char()),
            proc_macro2::TokenTree::Group(group) => {
                walk.restart();
                self.refer_tokens(group.stream());
            }
            proc_macro2::TokenTree::Literal(_) => walk.restart(),
        }
    }
}

impl<'ast> Visit<'ast> for SourceScan {
    fn visit_item_mod(&mut self, item: &'ast syn::ItemMod) {
        for attr in &item.attrs {
            self.visit_attribute(attr);
        }
        let foreign = exports_to_foreign_caller(&item.attrs);
        self.ffi_scope += usize::from(foreign);
        self.module.push(item.ident.to_string());
        if let Some((_, items)) = &item.content {
            for item in items {
                self.visit_item(item);
            }
        }
        self.module.pop();
        self.ffi_scope -= usize::from(foreign);
    }

    fn visit_item_impl(&mut self, item: &'ast syn::ItemImpl) {
        for attr in &item.attrs {
            self.visit_attribute(attr);
        }
        self.visit_generics(&item.generics);
        if let Some((_, path, _)) = &item.trait_ {
            self.visit_path(path);
        }
        self.visit_type(&item.self_ty);

        let implementing_type = match item.self_ty.as_ref() {
            syn::Type::Path(type_path) => type_path
                .path
                .segments
                .last()
                .map(|segment| segment.ident.to_string()),
            _ => None,
        };
        let foreign = exports_to_foreign_caller(&item.attrs);
        self.ffi_scope += usize::from(foreign);
        let outer = std::mem::replace(&mut self.implementing_type, implementing_type);
        for impl_item in &item.items {
            self.visit_impl_item(impl_item);
        }
        self.implementing_type = outer;
        self.ffi_scope -= usize::from(foreign);
    }

    fn visit_item_fn(&mut self, item: &'ast syn::ItemFn) {
        self.declare(&item.vis, &item.sig.ident, &item.attrs);
        for attr in &item.attrs {
            self.visit_attribute(attr);
        }
        self.visit_generics(&item.sig.generics);
        for input in &item.sig.inputs {
            self.visit_fn_arg(input);
        }
        self.visit_return_type(&item.sig.output);
        self.visit_block(&item.block);
    }

    fn visit_impl_item_fn(&mut self, item: &'ast syn::ImplItemFn) {
        self.declare(&item.vis, &item.sig.ident, &item.attrs);
        for attr in &item.attrs {
            self.visit_attribute(attr);
        }
        self.visit_generics(&item.sig.generics);
        for input in &item.sig.inputs {
            self.visit_fn_arg(input);
        }
        self.visit_return_type(&item.sig.output);
        self.visit_block(&item.block);
    }

    fn visit_item_struct(&mut self, item: &'ast syn::ItemStruct) {
        self.declare(&item.vis, &item.ident, &item.attrs);
        for attr in &item.attrs {
            self.visit_attribute(attr);
        }
        self.visit_generics(&item.generics);
        self.visit_fields(&item.fields);
    }

    fn visit_item_enum(&mut self, item: &'ast syn::ItemEnum) {
        self.declare(&item.vis, &item.ident, &item.attrs);
        for attr in &item.attrs {
            self.visit_attribute(attr);
        }
        self.visit_generics(&item.generics);
        for variant in &item.variants {
            self.visit_variant(variant);
        }
    }

    fn visit_item_const(&mut self, item: &'ast syn::ItemConst) {
        self.declare(&item.vis, &item.ident, &item.attrs);
        for attr in &item.attrs {
            self.visit_attribute(attr);
        }
        self.visit_type(&item.ty);
        self.visit_expr(&item.expr);
    }

    fn visit_item_static(&mut self, item: &'ast syn::ItemStatic) {
        self.declare(&item.vis, &item.ident, &item.attrs);
        for attr in &item.attrs {
            self.visit_attribute(attr);
        }
        self.visit_type(&item.ty);
        self.visit_expr(&item.expr);
    }

    fn visit_item_trait(&mut self, item: &'ast syn::ItemTrait) {
        self.declare(&item.vis, &item.ident, &item.attrs);
        for attr in &item.attrs {
            self.visit_attribute(attr);
        }
        self.visit_generics(&item.generics);
        for supertrait in &item.supertraits {
            self.visit_type_param_bound(supertrait);
        }
        for trait_item in &item.items {
            self.visit_trait_item(trait_item);
        }
    }

    fn visit_item_type(&mut self, item: &'ast syn::ItemType) {
        self.declare(&item.vis, &item.ident, &item.attrs);
        for attr in &item.attrs {
            self.visit_attribute(attr);
        }
        self.visit_generics(&item.generics);
        self.visit_type(&item.ty);
    }

    fn visit_item_use(&mut self, item: &'ast syn::ItemUse) {
        let mut prefix = Vec::new();
        self.refer_use_tree(&mut prefix, &item.tree);
    }

    fn visit_path(&mut self, path: &'ast syn::Path) {
        let mut qualifier: Vec<String> = Vec::new();
        for segment in &path.segments {
            let name = segment.ident.to_string();
            self.refer(name.clone(), qualifier.clone(), false);
            qualifier.push(name);
            self.visit_path_arguments(&segment.arguments);
        }
    }

    fn visit_expr_method_call(&mut self, call: &'ast syn::ExprMethodCall) {
        self.refer(call.method.to_string(), Vec::new(), true);
        self.visit_expr(&call.receiver);
        for argument in &call.args {
            self.visit_expr(argument);
        }
        if let Some(turbofish) = &call.turbofish {
            self.visit_angle_bracketed_generic_arguments(turbofish);
        }
    }

    fn visit_macro(&mut self, item: &'ast syn::Macro) {
        self.visit_path(&item.path);
        self.refer_tokens(item.tokens.clone());
    }

    fn visit_attribute(&mut self, attribute: &'ast syn::Attribute) {
        match &attribute.meta {
            syn::Meta::Path(path) => self.visit_path(path),
            syn::Meta::List(list) => {
                self.visit_path(&list.path);
                self.refer_tokens(list.tokens.clone());
            }
            syn::Meta::NameValue(pair) => {
                self.visit_path(&pair.path);
                self.visit_expr(&pair.value);
            }
        }
    }

    fn visit_ident(&mut self, _: &'ast syn::Ident) {
        // Names are recorded through the constructs that can reference a
        // declaration (paths, use trees, method calls, macro tokens); a bare
        // ident visit would also count field names and binding patterns.
    }
}

mod resolution_tests {
    use super::*;

    fn corpus(sources: &[(&str, &str, &str)]) -> Corpus {
        let mut corpus = Corpus::default();
        for (krate, path, source) in sources {
            corpus.add_rust_source(Some(krate), Path::new(path), source);
        }
        corpus
    }

    fn dead_names(corpus: &Corpus) -> Vec<String> {
        corpus
            .unreferenced_public_declarations()
            .into_iter()
            .map(|(_, name)| name)
            .collect()
    }

    #[test]
    fn a_called_function_is_reached_through_its_crate_qualified_import() {
        let corpus = corpus(&[
            (
                "rumoca_core",
                "crates/rumoca-core/src/spans.rs",
                "pub fn widen_span() {}",
            ),
            (
                "rumoca_sim",
                "crates/rumoca-sim/src/lib.rs",
                "use rumoca_core::spans::widen_span;\nfn run() { widen_span(); }",
            ),
        ]);
        assert!(dead_names(&corpus).is_empty());
    }

    #[test]
    fn a_facade_reexport_reaches_the_item_it_forwards() {
        let corpus = corpus(&[(
            "rumoca_ir_ast",
            "crates/rumoca-ir-ast/src/lib.rs",
            "mod nodes { pub struct Instance; }\npub use nodes::Instance;",
        )]);
        assert!(dead_names(&corpus).is_empty());
    }

    #[test]
    fn a_root_reexport_carries_a_crate_qualified_call() {
        let corpus = corpus(&[
            (
                "rumoca_core",
                "crates/rumoca-core/src/names.rs",
                "pub fn name() -> &'static str { \"n\" }",
            ),
            (
                "rumoca_core",
                "crates/rumoca-core/src/lib.rs",
                "mod names;\npub use names::name;",
            ),
            (
                "rumoca_sim",
                "crates/rumoca-sim/src/lib.rs",
                "fn run() { let _ = rumoca_core::name(); }",
            ),
        ]);
        assert!(
            dead_names(&corpus).is_empty(),
            "a crate-qualified call through a root re-export must reach the declaration"
        );
    }

    #[test]
    fn a_name_written_only_in_its_own_documentation_is_dead() {
        let corpus = corpus(&[(
            "rumoca_core",
            "crates/rumoca-core/src/lib.rs",
            "/// `dead_leaf` is mentioned only by its own documentation.\npub fn dead_leaf() {}",
        )]);
        assert_eq!(dead_names(&corpus), ["dead_leaf"]);
    }

    #[test]
    fn a_dead_item_is_reported_although_another_crate_uses_the_same_name() {
        let corpus = corpus(&[
            (
                "rumoca_ir_flat",
                "crates/rumoca-ir-flat/src/connections.rs",
                "pub fn num_nodes() -> usize { 0 }",
            ),
            (
                "rumoca_ir_dae",
                "crates/rumoca-ir-dae/src/graph.rs",
                "pub fn num_nodes() -> usize { 1 }\nfn count() -> usize { num_nodes() }",
            ),
        ]);
        assert_eq!(
            dead_names(&corpus),
            ["num_nodes"],
            "the unused declaration must not be vouched for by an unrelated crate's caller"
        );
    }

    #[test]
    fn a_dead_item_is_reported_although_a_sibling_module_uses_the_same_name() {
        let corpus = corpus(&[(
            "rumoca_solver",
            "crates/rumoca-solver/src/runtime/plans.rs",
            "pub mod dead { pub fn refresh() {} }\npub mod live { pub fn refresh() {} }\nfn tick() { live::refresh(); }",
        )]);
        assert_eq!(
            dead_names(&corpus),
            ["refresh"],
            "only the module-qualified declaration is reached"
        );
    }

    #[test]
    fn a_third_party_call_does_not_vouch_for_a_workspace_item_of_the_same_name() {
        let corpus = corpus(&[
            (
                "rumoca_phase_parse",
                "crates/rumoca-phase-parse/src/lib.rs",
                "pub fn parse_file() {}",
            ),
            (
                "rumoca",
                "crates/rumoca/tests/audit.rs",
                "fn scan(text: &str) { let _ = syn::parse_file(text); }",
            ),
        ]);
        assert_eq!(
            dead_names(&corpus),
            ["parse_file"],
            "`syn::parse_file` is a different declaration and must not keep ours alive"
        );
    }

    #[test]
    fn a_read_field_does_not_vouch_for_the_accessor_that_shares_its_name() {
        let corpus = corpus(&[(
            "rumoca_transport_udp",
            "crates/rumoca-transport-udp/src/lib.rs",
            "pub struct Link { send_addr: String }\nimpl Link { pub fn send_addr(&self) -> &str { &self.send_addr } }",
        )]);
        assert_eq!(
            dead_names(&corpus),
            ["send_addr"],
            "reading the field is not a call of the accessor"
        );
    }

    #[test]
    fn a_macro_argument_reaches_the_constant_it_names() {
        let corpus = corpus(&[(
            "rumoca_core",
            "crates/rumoca-core/src/lib.rs",
            "pub const SAMPLE_NAME: &str = \"sample\";\nfn show() { println!(\"{}\", SAMPLE_NAME); }",
        )]);
        assert!(dead_names(&corpus).is_empty());
    }

    #[test]
    fn a_method_call_reaches_the_inherent_method_it_names() {
        let corpus = corpus(&[
            (
                "rumoca_solver",
                "crates/rumoca-solver/src/session.rs",
                "pub struct Session;\nimpl Session { pub fn step(&self) {} }",
            ),
            (
                "rumoca_sim",
                "crates/rumoca-sim/src/lib.rs",
                "fn run(session: &Session) { session.step(); }",
            ),
        ]);
        assert!(dead_names(&corpus).is_empty());
    }

    #[test]
    fn an_item_exported_to_a_foreign_caller_is_not_reported() {
        let corpus = corpus(&[(
            "rumoca_bind_wasm",
            "crates/rumoca-bind-wasm/src/lib.rs",
            "#[wasm_bindgen]\npub fn compile_model() {}",
        )]);
        assert!(
            dead_names(&corpus).is_empty(),
            "the caller lives outside Rust, so no workspace reference can resolve to it"
        );
    }

    #[test]
    fn a_text_caller_reaches_every_declaration_of_the_name() {
        let mut corpus = corpus(&[(
            "rumoca_bind_python",
            "crates/rumoca-bind-python/src/lib.rs",
            "pub fn compile_model() {}",
        )]);
        corpus.add_text_source("rumoca.compile_model(path)");
        assert!(dead_names(&corpus).is_empty());
    }
}
