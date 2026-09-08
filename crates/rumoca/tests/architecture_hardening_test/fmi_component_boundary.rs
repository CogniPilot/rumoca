//! SPEC_0029/0041, SPEC_0036/0043, and SPEC_0044 FMI absorption boundary.

use super::*;
use blake3::Hasher;
use quote::ToTokens;
use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

fn manifest(crate_name: &str) -> toml::Value {
    let path = workspace_root()
        .join("crates")
        .join(crate_name)
        .join("Cargo.toml");
    let source = fs::read_to_string(&path)
        .unwrap_or_else(|error| panic!("read {}: {error}", path.display()));
    toml::from_str(&source).unwrap_or_else(|error| panic!("parse {}: {error}", path.display()))
}

fn feature_members(manifest: &toml::Value, feature: &str) -> Vec<String> {
    manifest["features"][feature]
        .as_array()
        .unwrap_or_else(|| panic!("feature `{feature}` must be an array"))
        .iter()
        .map(|member| {
            member
                .as_str()
                .unwrap_or_else(|| panic!("feature `{feature}` members must be strings"))
                .to_string()
        })
        .collect()
}

fn function_signature<'a>(source: &'a str, name: &str) -> &'a str {
    let marker = format!("fn {name}");
    let tail = source
        .split_once(&marker)
        .unwrap_or_else(|| panic!("missing `{marker}`"))
        .1;
    tail.split_once(") ->")
        .unwrap_or_else(|| panic!("unterminated signature for `{name}`"))
        .0
}

fn derive_prefix<'a>(source: &'a str, declaration: &str) -> &'a str {
    let before = source
        .split_once(declaration)
        .unwrap_or_else(|| panic!("missing `{declaration}`"))
        .0;
    before
        .rsplit_once("#[derive(")
        .unwrap_or_else(|| panic!("missing derive before `{declaration}`"))
        .1
}

const FMI_SEMANTIC_CARRIERS: [&str; 4] = [
    "FmiLinkedRuntimeFacts",
    "FmiEventIndicatorPlan",
    "FmiComponent",
    "FmiRuntimeView",
];

const FMI_CAPABILITY_TYPES: [&str; 4] = [
    "FmiComponent",
    "FmiRuntimeView",
    "FmiLinkedRuntimeFacts",
    "SolveModel",
];

const FMI_SCALAR_DERIVATIVE_CARRIER: &str = "Fmi3ScalarConstantDerivativeCarrier";
const FMI_EVENT_FREE_VIEW: &str = "FmiEventFreeCodegenView";

#[derive(Clone, Debug, Default, Eq, PartialEq)]
struct TypeExposure {
    arc_owned: BTreeSet<String>,
    borrowed: BTreeSet<String>,
    owned: BTreeSet<String>,
    unexpanded_macro: bool,
}

impl TypeExposure {
    fn merge(&mut self, other: Self) {
        self.arc_owned.extend(other.arc_owned);
        self.borrowed.extend(other.borrowed);
        self.owned.extend(other.owned);
        self.unexpanded_macro |= other.unexpanded_macro;
    }
}

#[derive(Clone, Debug)]
struct TypeNames {
    arc_names: BTreeSet<String>,
    protected_names: BTreeMap<String, String>,
}

impl Default for TypeNames {
    fn default() -> Self {
        Self {
            arc_names: BTreeSet::from(["Arc".to_owned()]),
            protected_names: FMI_CAPABILITY_TYPES
                .map(|name| (name.to_owned(), name.to_owned()))
                .into_iter()
                .collect(),
        }
    }
}

#[derive(Debug, Default)]
struct FmiCapabilityInventory {
    arc_fields: BTreeSet<String>,
    declarations: BTreeSet<String>,
    declaration_visibilities: BTreeMap<String, String>,
    macro_fingerprints: BTreeMap<String, String>,
    mints: BTreeSet<String>,
    owned_fields: BTreeSet<String>,
    owned_inputs: BTreeMap<String, BTreeSet<String>>,
    owned_outputs: BTreeMap<String, BTreeSet<String>>,
    reexports: BTreeSet<String>,
    reexport_visibilities: BTreeMap<String, String>,
    self_consumers: BTreeSet<String>,
    trait_impls: BTreeSet<String>,
    violations: BTreeSet<String>,
}

fn source_label(path: &Path) -> String {
    let components = path.components().collect::<Vec<_>>();
    let Some(src) = components
        .iter()
        .rposition(|component| component.as_os_str() == "src")
    else {
        return path.display().to_string();
    };
    components[src..]
        .iter()
        .collect::<PathBuf>()
        .display()
        .to_string()
}

fn route(source: &str, modules: &[String], owner: Option<&str>, item: &str) -> String {
    let mut parts = vec![source.to_owned()];
    parts.extend(modules.iter().cloned());
    if let Some(owner) = owner {
        parts.push(owner.to_owned());
    }
    parts.push(item.to_owned());
    parts.join("::")
}

fn visible(visibility: &syn::Visibility) -> bool {
    !matches!(visibility, syn::Visibility::Inherited)
}

fn visibility_label(visibility: &syn::Visibility) -> String {
    if matches!(visibility, syn::Visibility::Inherited) {
        "private".to_owned()
    } else {
        visibility.to_token_stream().to_string()
    }
}

fn macro_fingerprint(mac: &syn::Macro) -> String {
    let mut digest = Hasher::new();
    digest.update(mac.path.to_token_stream().to_string().as_bytes());
    digest.update(&[0]);
    digest.update(mac.tokens.to_string().as_bytes());
    digest.finalize().to_hex().to_string()
}

fn record_macro_fingerprint(
    inventory: &mut FmiCapabilityInventory,
    route: String,
    mac: &syn::Macro,
) {
    match inventory.macro_fingerprints.entry(route.clone()) {
        std::collections::btree_map::Entry::Vacant(entry) => {
            entry.insert(macro_fingerprint(mac));
        }
        std::collections::btree_map::Entry::Occupied(_) => {
            inventory
                .violations
                .insert(format!("duplicate-macro-fingerprint:{route}"));
        }
    }
}

fn bounds_exposure(
    bounds: &syn::punctuated::Punctuated<syn::TypeParamBound, syn::token::Plus>,
    names: &TypeNames,
    self_type: Option<&str>,
) -> TypeExposure {
    bounds
        .iter()
        .fold(TypeExposure::default(), |mut exposure, bound| {
            exposure.merge(bound_exposure(bound, names, self_type));
            exposure
        })
}

fn generic_argument_exposure(
    argument: &syn::GenericArgument,
    names: &TypeNames,
    self_type: Option<&str>,
    borrowed: bool,
    inside_arc: bool,
) -> TypeExposure {
    match argument {
        syn::GenericArgument::Type(ty) => {
            type_exposure_in(ty, names, self_type, borrowed, inside_arc)
        }
        syn::GenericArgument::AssocType(binding) => {
            type_exposure_in(&binding.ty, names, self_type, false, inside_arc)
        }
        syn::GenericArgument::Constraint(constraint) => {
            bounds_exposure(&constraint.bounds, names, self_type)
        }
        _ => TypeExposure::default(),
    }
}

fn path_arguments_exposure(
    arguments: &syn::PathArguments,
    names: &TypeNames,
    self_type: Option<&str>,
    borrowed: bool,
    inside_arc: bool,
) -> TypeExposure {
    let mut exposure = TypeExposure::default();
    match arguments {
        syn::PathArguments::None => {}
        syn::PathArguments::AngleBracketed(arguments) => {
            for argument in &arguments.args {
                exposure.merge(generic_argument_exposure(
                    argument, names, self_type, borrowed, inside_arc,
                ));
            }
        }
        syn::PathArguments::Parenthesized(arguments) => {
            for input in &arguments.inputs {
                exposure.merge(type_exposure_in(
                    input, names, self_type, borrowed, inside_arc,
                ));
            }
            if let syn::ReturnType::Type(_, output) = &arguments.output {
                exposure.merge(type_exposure_in(
                    output, names, self_type, false, inside_arc,
                ));
            }
        }
    }
    exposure
}

fn path_exposure(
    path: &syn::Path,
    names: &TypeNames,
    self_type: Option<&str>,
    borrowed: bool,
    inside_arc: bool,
) -> TypeExposure {
    let mut exposure = TypeExposure::default();
    for segment in &path.segments {
        let spelling = segment.ident.to_string();
        let protected = if spelling == "Self" {
            self_type.map(str::to_owned)
        } else {
            names.protected_names.get(&spelling).cloned()
        };
        if let Some(protected) = protected {
            if inside_arc {
                exposure.arc_owned.insert(protected);
            } else if borrowed {
                exposure.borrowed.insert(protected);
            } else {
                exposure.owned.insert(protected);
            }
        }
        let segment_is_arc = names.arc_names.contains(&spelling);
        exposure.merge(path_arguments_exposure(
            &segment.arguments,
            names,
            self_type,
            borrowed,
            inside_arc || segment_is_arc,
        ));
    }
    exposure
}

fn bound_exposure(
    bound: &syn::TypeParamBound,
    names: &TypeNames,
    self_type: Option<&str>,
) -> TypeExposure {
    match bound {
        syn::TypeParamBound::Trait(bound) => {
            path_exposure(&bound.path, names, self_type, false, false)
        }
        _ => TypeExposure::default(),
    }
}

fn type_exposure(ty: &syn::Type, names: &TypeNames, self_type: Option<&str>) -> TypeExposure {
    type_exposure_in(ty, names, self_type, false, false)
}

fn type_exposure_in(
    ty: &syn::Type,
    names: &TypeNames,
    self_type: Option<&str>,
    borrowed: bool,
    inside_arc: bool,
) -> TypeExposure {
    let recurse = |ty| type_exposure_in(ty, names, self_type, borrowed, inside_arc);
    match ty {
        syn::Type::Array(array) => recurse(&array.elem),
        syn::Type::BareFn(function) => {
            let mut exposure = TypeExposure::default();
            for input in &function.inputs {
                exposure.merge(type_exposure_in(
                    &input.ty, names, self_type, borrowed, inside_arc,
                ));
            }
            if let syn::ReturnType::Type(_, output) = &function.output {
                exposure.merge(type_exposure_in(
                    output, names, self_type, false, inside_arc,
                ));
            }
            exposure
        }
        syn::Type::Group(group) => recurse(&group.elem),
        syn::Type::ImplTrait(bounds) => {
            bounds
                .bounds
                .iter()
                .fold(TypeExposure::default(), |mut exposure, bound| {
                    exposure.merge(bound_exposure(bound, names, self_type));
                    exposure
                })
        }
        syn::Type::Macro(_) | syn::Type::Verbatim(_) => TypeExposure {
            unexpanded_macro: true,
            ..TypeExposure::default()
        },
        syn::Type::Paren(paren) => recurse(&paren.elem),
        syn::Type::Path(path) => path_exposure(&path.path, names, self_type, borrowed, inside_arc),
        syn::Type::Ptr(pointer) => {
            type_exposure_in(&pointer.elem, names, self_type, false, inside_arc)
        }
        syn::Type::Reference(reference) => {
            type_exposure_in(&reference.elem, names, self_type, true, inside_arc)
        }
        syn::Type::Slice(slice) => recurse(&slice.elem),
        syn::Type::TraitObject(bounds) => {
            bounds
                .bounds
                .iter()
                .fold(TypeExposure::default(), |mut exposure, bound| {
                    exposure.merge(bound_exposure(bound, names, self_type));
                    exposure
                })
        }
        syn::Type::Tuple(tuple) => {
            tuple
                .elems
                .iter()
                .map(recurse)
                .fold(TypeExposure::default(), |mut exposure, next| {
                    exposure.merge(next);
                    exposure
                })
        }
        _ => TypeExposure::default(),
    }
}

fn generics_exposure(
    generics: &syn::Generics,
    names: &TypeNames,
    self_type: Option<&str>,
) -> TypeExposure {
    let mut exposure = TypeExposure::default();
    for parameter in &generics.params {
        if let syn::GenericParam::Type(parameter) = parameter {
            if let Some(default) = &parameter.default {
                exposure.merge(type_exposure(default, names, self_type));
            }
            for bound in &parameter.bounds {
                exposure.merge(bound_exposure(bound, names, self_type));
            }
        }
    }
    if let Some(where_clause) = &generics.where_clause {
        for predicate in &where_clause.predicates {
            if let syn::WherePredicate::Type(predicate) = predicate {
                exposure.merge(type_exposure(&predicate.bounded_ty, names, self_type));
                exposure.merge(bounds_exposure(&predicate.bounds, names, self_type));
            }
        }
    }
    exposure
}

fn use_leaves(tree: &syn::UseTree, prefix: &mut Vec<String>, leaves: &mut Vec<(String, String)>) {
    match tree {
        syn::UseTree::Path(path) => {
            prefix.push(path.ident.to_string());
            use_leaves(&path.tree, prefix, leaves);
            prefix.pop();
        }
        syn::UseTree::Name(name) => {
            let original = name.ident.to_string();
            prefix.push(original.clone());
            leaves.push((prefix.join("::"), original));
            prefix.pop();
        }
        syn::UseTree::Rename(rename) => {
            prefix.push(rename.ident.to_string());
            leaves.push((prefix.join("::"), rename.rename.to_string()));
            prefix.pop();
        }
        syn::UseTree::Glob(_) => leaves.push((format!("{}::*", prefix.join("::")), "*".into())),
        syn::UseTree::Group(group) => {
            for item in &group.items {
                use_leaves(item, prefix, leaves);
            }
        }
    }
}

fn collect_type_names(files: &[syn::File]) -> TypeNames {
    let mut names = TypeNames::default();
    resolve_type_aliases(files, &mut names);
    names
}

fn collect_proof_affinity_type_names(files: &[syn::File]) -> TypeNames {
    let mut names = TypeNames {
        arc_names: BTreeSet::from(["Arc".to_owned()]),
        protected_names: [FMI_SCALAR_DERIVATIVE_CARRIER, FMI_EVENT_FREE_VIEW]
            .map(|name| (name.to_owned(), name.to_owned()))
            .into_iter()
            .collect(),
    };
    resolve_type_aliases(files, &mut names);
    names
}

fn resolve_type_aliases(files: &[syn::File], names: &mut TypeNames) {
    let mut aliases = Vec::new();
    for file in files {
        collect_alias_candidates(&file.items, &mut aliases);
    }
    for _ in 0..=aliases.len() {
        let mut changed = false;
        for (alias, target) in &aliases {
            if names.arc_names.contains(target) {
                changed |= names.arc_names.insert(alias.clone());
            }
            if let Some(protected) = names.protected_names.get(target).cloned() {
                changed |= names
                    .protected_names
                    .insert(alias.clone(), protected.clone())
                    .as_ref()
                    != Some(&protected);
            }
        }
        if !changed {
            break;
        }
    }
}

fn collect_alias_candidates(items: &[syn::Item], aliases: &mut Vec<(String, String)>) {
    for item in items {
        match item {
            syn::Item::Mod(module) => {
                if let Some((_, items)) = &module.content {
                    collect_alias_candidates(items, aliases);
                }
            }
            syn::Item::Type(alias) => {
                if let syn::Type::Path(path) = alias.ty.as_ref()
                    && let Some(target) = path.path.segments.last()
                {
                    aliases.push((alias.ident.to_string(), target.ident.to_string()));
                }
            }
            syn::Item::Use(item_use) => {
                collect_use_alias_candidates(item_use, aliases);
            }
            _ => {}
        }
    }
}

fn collect_use_alias_candidates(item_use: &syn::ItemUse, aliases: &mut Vec<(String, String)>) {
    let mut leaves = Vec::new();
    use_leaves(&item_use.tree, &mut Vec::new(), &mut leaves);
    for (path, alias) in leaves {
        let Some(target) = path.rsplit("::").next() else {
            continue;
        };
        if alias != "*" {
            aliases.push((alias, target.to_owned()));
        }
    }
}

fn exposure_names(exposure: &TypeExposure) -> BTreeSet<String> {
    exposure
        .arc_owned
        .iter()
        .chain(&exposure.owned)
        .cloned()
        .collect()
}

fn record_exposure(
    inventory: &mut FmiCapabilityInventory,
    route: &str,
    position: &str,
    exposure: TypeExposure,
) {
    if exposure.unexpanded_macro {
        inventory
            .violations
            .insert(format!("unexpanded-type-macro:{position}:{route}"));
    }
    if !exposure.arc_owned.is_empty() {
        inventory.violations.insert(format!(
            "arc-owned-{position}:{route}:{:?}",
            exposure.arc_owned
        ));
    }
    let owned = exposure_names(&exposure);
    if owned.is_empty() {
        return;
    }
    match position {
        "input" => {
            inventory.owned_inputs.insert(route.to_owned(), owned);
        }
        "output" => {
            inventory.owned_outputs.insert(route.to_owned(), owned);
        }
        "generic" => {
            inventory
                .violations
                .insert(format!("owned-generic:{route}:{owned:?}"));
        }
        _ => {
            inventory
                .violations
                .insert(format!("owned-{position}:{route}:{owned:?}"));
        }
    }
}

fn inspect_signature(
    signature: &syn::Signature,
    route: &str,
    self_type: Option<&str>,
    names: &TypeNames,
    inventory: &mut FmiCapabilityInventory,
) {
    record_exposure(
        inventory,
        route,
        "generic",
        generics_exposure(&signature.generics, names, self_type),
    );
    for input in &signature.inputs {
        match input {
            syn::FnArg::Receiver(receiver) if receiver.reference.is_none() => {
                if self_type.is_some_and(|name| FMI_CAPABILITY_TYPES.contains(&name)) {
                    inventory.self_consumers.insert(route.to_owned());
                }
            }
            syn::FnArg::Receiver(_) => {}
            syn::FnArg::Typed(input) => record_exposure(
                inventory,
                route,
                "input",
                type_exposure(&input.ty, names, self_type),
            ),
        }
    }
    if let syn::ReturnType::Type(_, output) = &signature.output {
        record_exposure(
            inventory,
            route,
            "output",
            type_exposure(output, names, self_type),
        );
    }
}

fn protected_self_type(ty: &syn::Type, names: &TypeNames) -> Option<String> {
    let syn::Type::Path(path) = ty else {
        return None;
    };
    path.path.segments.last().and_then(|segment| {
        names
            .protected_names
            .get(&segment.ident.to_string())
            .cloned()
    })
}

fn record_function_mints(
    function: &syn::ImplItemFn,
    route: &str,
    self_type: Option<&str>,
    names: &TypeNames,
    inventory: &mut FmiCapabilityInventory,
) {
    struct MintVisitor<'a> {
        names: &'a TypeNames,
        route: &'a str,
        self_type: Option<&'a str>,
        mints: &'a mut BTreeSet<String>,
    }
    impl<'ast> syn::visit::Visit<'ast> for MintVisitor<'_> {
        fn visit_expr_struct(&mut self, expression: &'ast syn::ExprStruct) {
            if let Some(segment) = expression.path.segments.last()
                && let Some(protected) = if segment.ident == "Self" {
                    self.self_type.map(str::to_owned)
                } else {
                    self.names
                        .protected_names
                        .get(&segment.ident.to_string())
                        .cloned()
                }
            {
                self.mints.insert(format!("{}@{}", protected, self.route));
            }
            syn::visit::visit_expr_struct(self, expression);
        }
    }
    use syn::visit::Visit;
    MintVisitor {
        names,
        route,
        self_type,
        mints: &mut inventory.mints,
    }
    .visit_block(&function.block);
}

fn inspect_impl(
    item: &syn::ItemImpl,
    source: &str,
    modules: &[String],
    names: &TypeNames,
    inventory: &mut FmiCapabilityInventory,
) {
    let self_type = protected_self_type(&item.self_ty, names);
    let rendered_owner = item.self_ty.to_token_stream().to_string();
    let owner = self_type.as_deref().unwrap_or(&rendered_owner);
    if let Some((_, trait_path, _)) = &item.trait_ {
        record_exposure(
            inventory,
            &route(source, modules, Some(owner), "implemented-trait"),
            "implemented-trait",
            path_exposure(trait_path, names, self_type.as_deref(), false, false),
        );
    }
    if let Some((_, trait_path, _)) = &item.trait_
        && let Some(self_type) = &self_type
    {
        let trait_name = trait_path.to_token_stream().to_string();
        let route = route(source, modules, Some(self_type), &trait_name);
        inventory.trait_impls.insert(route.clone());
        if [
            "Clone",
            "Default",
            "Serialize",
            "Deserialize",
            "DeserializeOwned",
            "From",
            "TryFrom",
        ]
        .iter()
        .any(|forbidden| {
            trait_path
                .segments
                .last()
                .is_some_and(|segment| segment.ident == *forbidden)
        }) {
            inventory
                .violations
                .insert(format!("forbidden-trait-impl:{route}"));
        }
    }
    record_exposure(
        inventory,
        &route(source, modules, Some(owner), "impl-generics"),
        "generic",
        generics_exposure(&item.generics, names, self_type.as_deref()),
    );
    for child in &item.items {
        match child {
            syn::ImplItem::Fn(function) => {
                let item_route = route(
                    source,
                    modules,
                    self_type.as_deref().or(Some(owner)),
                    &function.sig.ident.to_string(),
                );
                inspect_signature(
                    &function.sig,
                    &item_route,
                    self_type.as_deref(),
                    names,
                    inventory,
                );
                record_function_mints(
                    function,
                    &item_route,
                    self_type.as_deref(),
                    names,
                    inventory,
                );
            }
            syn::ImplItem::Type(item_type) => record_exposure(
                inventory,
                &route(source, modules, Some(owner), &item_type.ident.to_string()),
                "associated-type",
                type_exposure(&item_type.ty, names, self_type.as_deref()),
            ),
            syn::ImplItem::Const(item_const) => record_exposure(
                inventory,
                &route(source, modules, Some(owner), &item_const.ident.to_string()),
                "associated-const",
                type_exposure(&item_const.ty, names, self_type.as_deref()),
            ),
            syn::ImplItem::Macro(item_macro) => {
                record_macro_fingerprint(
                    inventory,
                    format!(
                        "unexpanded-impl-macro:{}",
                        route(
                            source,
                            modules,
                            Some(owner),
                            &item_macro.mac.path.to_token_stream().to_string()
                        )
                    ),
                    &item_macro.mac,
                );
            }
            _ => {}
        }
    }
}

fn inspect_struct(
    item: &syn::ItemStruct,
    source: &str,
    modules: &[String],
    names: &TypeNames,
    inventory: &mut FmiCapabilityInventory,
) {
    let name = item.ident.to_string();
    let declaration = route(source, modules, None, &name);
    if FMI_CAPABILITY_TYPES.contains(&name.as_str()) {
        inventory.declarations.insert(declaration.clone());
        inventory
            .declaration_visibilities
            .insert(declaration.clone(), visibility_label(&item.vis));
    }
    record_exposure(
        inventory,
        &declaration,
        "generic",
        generics_exposure(&item.generics, names, Some(&name)),
    );
    inspect_fields(&item.fields, &declaration, false, &name, names, inventory);
    if FMI_SEMANTIC_CARRIERS.contains(&name.as_str()) {
        inspect_carrier_derives(&item.attrs, &declaration, inventory);
    }
}

fn inspect_fields(
    fields: &syn::Fields,
    declaration: &str,
    owner_is_visible: bool,
    owner: &str,
    names: &TypeNames,
    inventory: &mut FmiCapabilityInventory,
) {
    for (index, field) in fields.iter().enumerate() {
        let field_name = field
            .ident
            .as_ref()
            .map_or_else(|| index.to_string(), ToString::to_string);
        let field_route = format!("{declaration}::{field_name}");
        let exposure = type_exposure(&field.ty, names, Some(owner));
        if !exposure.arc_owned.is_empty() {
            inventory.arc_fields.insert(field_route.clone());
        }
        if !exposure.owned.is_empty() {
            inventory.owned_fields.insert(field_route.clone());
        }
        if (owner_is_visible || visible(&field.vis))
            && (!exposure.arc_owned.is_empty() || !exposure.owned.is_empty())
        {
            inventory
                .violations
                .insert(format!("visible-owned-field:{field_route}"));
        }
    }
}

fn inspect_carrier_derives(
    attributes: &[syn::Attribute],
    declaration: &str,
    inventory: &mut FmiCapabilityInventory,
) {
    for attribute in attributes {
        if !attribute.path().is_ident("derive") {
            continue;
        }
        attribute
            .parse_nested_meta(|meta| {
                if let Some(ident) = meta.path.get_ident()
                    && ["Clone", "Default", "Serialize", "Deserialize"]
                        .contains(&ident.to_string().as_str())
                {
                    inventory
                        .violations
                        .insert(format!("forbidden-derive:{declaration}::{ident}"));
                }
                Ok(())
            })
            .expect("parse FMI carrier derive");
    }
}

fn inspect_reexport(
    item: &syn::ItemUse,
    source: &str,
    modules: &[String],
    names: &TypeNames,
    inventory: &mut FmiCapabilityInventory,
) {
    if !visible(&item.vis) {
        return;
    }
    let mut leaves = Vec::new();
    use_leaves(&item.tree, &mut Vec::new(), &mut leaves);
    for (path, alias) in leaves {
        let reexport_route = route(source, modules, None, &format!("pub-use:{path}"));
        if alias == "*" {
            inventory.reexports.insert(format!("{reexport_route}=>*"));
            inventory
                .reexport_visibilities
                .insert(reexport_route, visibility_label(&item.vis));
            continue;
        }
        let target = path.rsplit("::").next().unwrap_or_default();
        if let Some(protected) = names.protected_names.get(target) {
            inventory
                .reexports
                .insert(format!("{}=>{}", reexport_route, alias));
            inventory
                .reexport_visibilities
                .insert(reexport_route.clone(), visibility_label(&item.vis));
            if alias != *protected {
                inventory.violations.insert(format!(
                    "renamed-protected-reexport:{}=>{alias}",
                    reexport_route
                ));
            }
        }
    }
}

fn inspect_items(
    items: &[syn::Item],
    source: &str,
    modules: &mut Vec<String>,
    names: &TypeNames,
    inventory: &mut FmiCapabilityInventory,
) {
    for item in items {
        if item_attributes(item).is_some_and(attributes_require_test) {
            continue;
        }
        inspect_item(item, source, modules, names, inventory);
    }
}

fn inspect_item(
    item: &syn::Item,
    source: &str,
    modules: &mut Vec<String>,
    names: &TypeNames,
    inventory: &mut FmiCapabilityInventory,
) {
    match item {
        syn::Item::Fn(function) => {
            let item_route = route(source, modules, None, &function.sig.ident.to_string());
            inspect_signature(&function.sig, &item_route, None, names, inventory);
        }
        syn::Item::ForeignMod(item_foreign) => {
            inspect_foreign_mod(item_foreign, source, modules, names, inventory);
        }
        syn::Item::Impl(item_impl) => inspect_impl(item_impl, source, modules, names, inventory),
        syn::Item::Macro(item_macro) => {
            inspect_item_macro(item_macro, source, modules, inventory);
        }
        syn::Item::Mod(module) => inspect_module(module, source, modules, names, inventory),
        syn::Item::Struct(item_struct) => {
            inspect_struct(item_struct, source, modules, names, inventory);
        }
        syn::Item::Enum(item_enum) => {
            inspect_enum(item_enum, source, modules, names, inventory);
        }
        syn::Item::Union(item_union) => {
            inspect_union(item_union, source, modules, names, inventory);
        }
        syn::Item::Static(item_static) => record_exposure(
            inventory,
            &route(source, modules, None, &item_static.ident.to_string()),
            "static",
            type_exposure(&item_static.ty, names, None),
        ),
        syn::Item::Const(item_const) => record_exposure(
            inventory,
            &route(source, modules, None, &item_const.ident.to_string()),
            "const",
            type_exposure(&item_const.ty, names, None),
        ),
        syn::Item::Trait(item_trait) => {
            inspect_trait(item_trait, source, modules, names, inventory);
        }
        syn::Item::Type(alias) => inspect_alias(alias, source, modules, names, inventory),
        syn::Item::Use(item_use) => {
            inspect_reexport(item_use, source, modules, names, inventory);
        }
        _ => {}
    }
}

fn inspect_item_macro(
    item: &syn::ItemMacro,
    source: &str,
    modules: &[String],
    inventory: &mut FmiCapabilityInventory,
) {
    let macro_name = item.ident.as_ref().map_or_else(
        || item.mac.path.to_token_stream().to_string(),
        ToString::to_string,
    );
    record_macro_fingerprint(
        inventory,
        format!(
            "unexpanded-item-macro:{}",
            route(source, modules, None, &macro_name)
        ),
        &item.mac,
    );
}

fn inspect_foreign_mod(
    item: &syn::ItemForeignMod,
    source: &str,
    modules: &[String],
    names: &TypeNames,
    inventory: &mut FmiCapabilityInventory,
) {
    for child in &item.items {
        if foreign_item_attributes(child).is_some_and(attributes_require_test) {
            continue;
        }
        inspect_foreign_item(child, source, modules, names, inventory);
    }
}

fn inspect_foreign_item(
    item: &syn::ForeignItem,
    source: &str,
    modules: &[String],
    names: &TypeNames,
    inventory: &mut FmiCapabilityInventory,
) {
    match item {
        syn::ForeignItem::Fn(function) => inspect_signature(
            &function.sig,
            &route(
                source,
                modules,
                Some("extern"),
                &function.sig.ident.to_string(),
            ),
            None,
            names,
            inventory,
        ),
        syn::ForeignItem::Static(item_static) => record_exposure(
            inventory,
            &route(
                source,
                modules,
                Some("extern"),
                &item_static.ident.to_string(),
            ),
            "foreign-static",
            type_exposure(&item_static.ty, names, None),
        ),
        syn::ForeignItem::Type(item_type) => record_exposure(
            inventory,
            &route(
                source,
                modules,
                Some("extern"),
                &item_type.ident.to_string(),
            ),
            "generic",
            generics_exposure(&item_type.generics, names, None),
        ),
        syn::ForeignItem::Macro(item_macro) => record_macro_fingerprint(
            inventory,
            format!(
                "unexpanded-foreign-macro:{}",
                route(
                    source,
                    modules,
                    Some("extern"),
                    &item_macro.mac.path.to_token_stream().to_string()
                )
            ),
            &item_macro.mac,
        ),
        _ => {}
    }
}

fn inspect_module(
    module: &syn::ItemMod,
    source: &str,
    modules: &mut Vec<String>,
    names: &TypeNames,
    inventory: &mut FmiCapabilityInventory,
) {
    let Some((_, children)) = &module.content else {
        return;
    };
    modules.push(module.ident.to_string());
    inspect_items(children, source, modules, names, inventory);
    modules.pop();
}

fn inspect_enum(
    item: &syn::ItemEnum,
    source: &str,
    modules: &[String],
    names: &TypeNames,
    inventory: &mut FmiCapabilityInventory,
) {
    let name = item.ident.to_string();
    let declaration = route(source, modules, None, &name);
    record_exposure(
        inventory,
        &declaration,
        "generic",
        generics_exposure(&item.generics, names, Some(&name)),
    );
    for variant in &item.variants {
        inspect_fields(
            &variant.fields,
            &format!("{declaration}::{}", variant.ident),
            visible(&item.vis),
            &name,
            names,
            inventory,
        );
    }
}

fn inspect_union(
    item: &syn::ItemUnion,
    source: &str,
    modules: &[String],
    names: &TypeNames,
    inventory: &mut FmiCapabilityInventory,
) {
    let name = item.ident.to_string();
    let declaration = route(source, modules, None, &name);
    record_exposure(
        inventory,
        &declaration,
        "generic",
        generics_exposure(&item.generics, names, Some(&name)),
    );
    inspect_fields(
        &syn::Fields::Named(item.fields.clone()),
        &declaration,
        visible(&item.vis),
        &name,
        names,
        inventory,
    );
}

fn inspect_alias(
    alias: &syn::ItemType,
    source: &str,
    modules: &[String],
    names: &TypeNames,
    inventory: &mut FmiCapabilityInventory,
) {
    let alias_route = route(source, modules, None, &alias.ident.to_string());
    record_exposure(
        inventory,
        &alias_route,
        "alias",
        type_exposure(&alias.ty, names, None),
    );
    record_exposure(
        inventory,
        &alias_route,
        "generic",
        generics_exposure(&alias.generics, names, None),
    );
}

fn inspect_trait(
    item: &syn::ItemTrait,
    source: &str,
    modules: &[String],
    names: &TypeNames,
    inventory: &mut FmiCapabilityInventory,
) {
    let owner = format!("trait {}", item.ident);
    let trait_route = route(source, modules, Some(&owner), "generics");
    record_exposure(
        inventory,
        &trait_route,
        "generic",
        generics_exposure(&item.generics, names, None),
    );
    for bound in &item.supertraits {
        record_exposure(
            inventory,
            &route(source, modules, Some(&owner), "supertraits"),
            "supertrait",
            bound_exposure(bound, names, None),
        );
    }
    for child in &item.items {
        match child {
            syn::TraitItem::Fn(function) => inspect_signature(
                &function.sig,
                &route(
                    source,
                    modules,
                    Some(&owner),
                    &function.sig.ident.to_string(),
                ),
                None,
                names,
                inventory,
            ),
            syn::TraitItem::Type(item_type) => {
                let item_route = route(source, modules, Some(&owner), &item_type.ident.to_string());
                for bound in &item_type.bounds {
                    record_exposure(
                        inventory,
                        &item_route,
                        "associated-type-bound",
                        bound_exposure(bound, names, None),
                    );
                }
                if let Some((_, default)) = &item_type.default {
                    record_exposure(
                        inventory,
                        &item_route,
                        "associated-type",
                        type_exposure(default, names, None),
                    );
                }
            }
            syn::TraitItem::Const(item_const) => record_exposure(
                inventory,
                &route(source, modules, Some(&owner), &item_const.ident.to_string()),
                "associated-const",
                type_exposure(&item_const.ty, names, None),
            ),
            syn::TraitItem::Macro(item_macro) => {
                record_macro_fingerprint(
                    inventory,
                    format!(
                        "unexpanded-trait-macro:{}",
                        route(
                            source,
                            modules,
                            Some(&owner),
                            &item_macro.mac.path.to_token_stream().to_string()
                        )
                    ),
                    &item_macro.mac,
                );
            }
            _ => {}
        }
    }
}

fn foreign_item_attributes(item: &syn::ForeignItem) -> Option<&[syn::Attribute]> {
    match item {
        syn::ForeignItem::Fn(item) => Some(&item.attrs),
        syn::ForeignItem::Macro(item) => Some(&item.attrs),
        syn::ForeignItem::Static(item) => Some(&item.attrs),
        syn::ForeignItem::Type(item) => Some(&item.attrs),
        _ => None,
    }
}

fn item_attributes(item: &syn::Item) -> Option<&[syn::Attribute]> {
    match item {
        syn::Item::Const(item) => Some(&item.attrs),
        syn::Item::Enum(item) => Some(&item.attrs),
        syn::Item::Fn(item) => Some(&item.attrs),
        syn::Item::ForeignMod(item) => Some(&item.attrs),
        syn::Item::Impl(item) => Some(&item.attrs),
        syn::Item::Macro(item) => Some(&item.attrs),
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

fn exposure_owns(exposure: &TypeExposure, name: &str) -> bool {
    exposure.owned.contains(name) || exposure.arc_owned.contains(name)
}

fn record_proof_affinity_signature(
    signature: &syn::Signature,
    route: &str,
    self_type: Option<&str>,
    names: &TypeNames,
    inventory: &mut FmiCapabilityInventory,
) {
    let mut carrier_input = false;
    for input in &signature.inputs {
        match input {
            syn::FnArg::Receiver(receiver) => {
                carrier_input |= receiver.reference.is_none()
                    && self_type == Some(FMI_SCALAR_DERIVATIVE_CARRIER);
            }
            syn::FnArg::Typed(input) => {
                carrier_input |= exposure_owns(
                    &type_exposure(&input.ty, names, self_type),
                    FMI_SCALAR_DERIVATIVE_CARRIER,
                );
            }
        }
    }

    let output = match &signature.output {
        syn::ReturnType::Default => TypeExposure::default(),
        syn::ReturnType::Type(_, output) => type_exposure(output, names, self_type),
    };
    let generics = generics_exposure(&signature.generics, names, self_type);
    let generic_carrier = exposure_owns(&generics, FMI_SCALAR_DERIVATIVE_CARRIER);
    let generic_view = exposure_owns(&generics, FMI_EVENT_FREE_VIEW);
    let output_carrier = exposure_owns(&output, FMI_SCALAR_DERIVATIVE_CARRIER);
    let output_view = exposure_owns(&output, FMI_EVENT_FREE_VIEW);

    if (carrier_input && (output_view || generic_view))
        || (generic_carrier && output_view)
        || (output_carrier && output_view)
    {
        inventory
            .violations
            .insert(format!("proof-affinity-owned-extraction:{route}"));
    }
}

fn inspect_proof_affinity_impl(
    item: &syn::ItemImpl,
    source: &str,
    modules: &[String],
    names: &TypeNames,
    inventory: &mut FmiCapabilityInventory,
) {
    let self_type = protected_self_type(&item.self_ty, names);
    let rendered_owner = item.self_ty.to_token_stream().to_string();
    let owner = self_type.as_deref().unwrap_or(&rendered_owner);
    let associated_view = item.items.iter().any(|child| {
        let syn::ImplItem::Type(item_type) = child else {
            return false;
        };
        exposure_owns(
            &type_exposure(&item_type.ty, names, self_type.as_deref()),
            FMI_EVENT_FREE_VIEW,
        )
    });
    if let Some((_, trait_path, _)) = &item.trait_ {
        let self_exposure = type_exposure(&item.self_ty, names, None);
        let trait_exposure = path_exposure(trait_path, names, None, false, false);
        let carrier_to_view = exposure_owns(&self_exposure, FMI_EVENT_FREE_VIEW)
            && exposure_owns(&trait_exposure, FMI_SCALAR_DERIVATIVE_CARRIER);
        let into_view = exposure_owns(&self_exposure, FMI_SCALAR_DERIVATIVE_CARRIER)
            && exposure_owns(&trait_exposure, FMI_EVENT_FREE_VIEW);
        if carrier_to_view || into_view {
            inventory.violations.insert(format!(
                "proof-affinity-trait-extraction:{}",
                route(source, modules, Some(owner), "implemented-trait")
            ));
        }
    }

    for child in &item.items {
        let syn::ImplItem::Fn(function) = child else {
            continue;
        };
        if item.trait_.is_none() && !visible(&function.vis) {
            continue;
        }
        let item_route = route(
            source,
            modules,
            self_type.as_deref().or(Some(owner)),
            &function.sig.ident.to_string(),
        );
        record_proof_affinity_signature(
            &function.sig,
            &item_route,
            self_type.as_deref(),
            names,
            inventory,
        );
        let consumes_carrier = self_type.as_deref() == Some(FMI_SCALAR_DERIVATIVE_CARRIER)
            && function
                .sig
                .inputs
                .iter()
                .any(|input| matches!(input, syn::FnArg::Receiver(receiver) if receiver.reference.is_none()));
        if associated_view && consumes_carrier {
            inventory
                .violations
                .insert(format!("proof-affinity-trait-extraction:{item_route}"));
        }
    }
}

/// Record the visible `extern` block signatures of one foreign module.
///
/// Split out of `inspect_proof_affinity_items` so the per-child filter is not
/// nested inside the item match; the recorded route and arguments are unchanged.
fn record_proof_affinity_foreign_items(
    items: &[syn::ForeignItem],
    source: &str,
    modules: &[String],
    names: &TypeNames,
    inventory: &mut FmiCapabilityInventory,
) {
    for child in items {
        if let syn::ForeignItem::Fn(function) = child
            && visible(&function.vis)
        {
            record_proof_affinity_signature(
                &function.sig,
                &route(
                    source,
                    modules,
                    Some("extern"),
                    &function.sig.ident.to_string(),
                ),
                None,
                names,
                inventory,
            );
        }
    }
}

/// Record the signatures declared by one visible trait.
///
/// Split out for the same reason as the foreign-module case; the route retains
/// the `trait <Ident>` qualifier exactly as before.
fn record_proof_affinity_trait_items(
    item_trait: &syn::ItemTrait,
    source: &str,
    modules: &[String],
    names: &TypeNames,
    inventory: &mut FmiCapabilityInventory,
) {
    for child in &item_trait.items {
        if let syn::TraitItem::Fn(function) = child {
            record_proof_affinity_signature(
                &function.sig,
                &route(
                    source,
                    modules,
                    Some(&format!("trait {}", item_trait.ident)),
                    &function.sig.ident.to_string(),
                ),
                None,
                names,
                inventory,
            );
        }
    }
}

fn inspect_proof_affinity_items(
    items: &[syn::Item],
    source: &str,
    modules: &mut Vec<String>,
    names: &TypeNames,
    inventory: &mut FmiCapabilityInventory,
) {
    for item in items {
        if item_attributes(item).is_some_and(attributes_require_test) {
            continue;
        }
        match item {
            syn::Item::Fn(function) if visible(&function.vis) => {
                record_proof_affinity_signature(
                    &function.sig,
                    &route(source, modules, None, &function.sig.ident.to_string()),
                    None,
                    names,
                    inventory,
                );
            }
            syn::Item::Impl(item_impl) => {
                inspect_proof_affinity_impl(item_impl, source, modules, names, inventory);
            }
            syn::Item::ForeignMod(item_foreign) => {
                record_proof_affinity_foreign_items(
                    &item_foreign.items,
                    source,
                    modules,
                    names,
                    inventory,
                );
            }
            syn::Item::Mod(module) => {
                let Some((_, children)) = &module.content else {
                    continue;
                };
                modules.push(module.ident.to_string());
                inspect_proof_affinity_items(children, source, modules, names, inventory);
                modules.pop();
            }
            syn::Item::Trait(item_trait) if visible(&item_trait.vis) => {
                record_proof_affinity_trait_items(item_trait, source, modules, names, inventory);
            }
            _ => {}
        }
    }
}

fn fmi_capability_inventory(sources: &[(PathBuf, String)]) -> FmiCapabilityInventory {
    let mut inventory = FmiCapabilityInventory::default();
    let files = sources
        .iter()
        .map(|(path, source)| {
            syn::parse_file(source)
                .unwrap_or_else(|error| panic!("parse {}: {error}", path.display()))
        })
        .collect::<Vec<_>>();
    let names = collect_type_names(&files);
    let proof_affinity_names = collect_proof_affinity_type_names(&files);
    for ((path, _), syntax) in sources.iter().zip(&files) {
        let source = source_label(path);
        inspect_items(
            &syntax.items,
            &source,
            &mut Vec::new(),
            &names,
            &mut inventory,
        );
        inspect_proof_affinity_items(
            &syntax.items,
            &source,
            &mut Vec::new(),
            &proof_affinity_names,
            &mut inventory,
        );
    }
    inventory
}

fn expected_owned_routes() -> BTreeMap<String, BTreeSet<String>> {
    BTreeMap::from([
        (
            "src/fmi.rs::FmiComponent::construct".to_owned(),
            BTreeSet::from(["FmiComponent".to_owned()]),
        ),
        (
            "src/fmi.rs::FmiComponent::into_runtime_view".to_owned(),
            BTreeSet::from(["FmiRuntimeView".to_owned()]),
        ),
        (
            "src/fmi/linked_runtime.rs::FmiLinkedRuntimeFacts::construct".to_owned(),
            BTreeSet::from(["FmiLinkedRuntimeFacts".to_owned()]),
        ),
        (
            "src/model.rs::SolveModel::construct".to_owned(),
            BTreeSet::from(["SolveModel".to_owned()]),
        ),
    ])
}

fn expected_owned_inputs() -> BTreeMap<String, BTreeSet<String>> {
    BTreeMap::from([(
        "src/fmi.rs::FmiComponent::construct".to_owned(),
        BTreeSet::from(["SolveModel".to_owned()]),
    )])
}

fn expected_self_consumers() -> BTreeSet<String> {
    BTreeSet::from([
        "src/fmi.rs::FmiComponent::into_codegen_view".to_owned(),
        "src/fmi.rs::FmiComponent::into_runtime_view".to_owned(),
    ])
}

fn expected_mints() -> BTreeSet<String> {
    BTreeSet::from([
        "FmiComponent@src/fmi.rs::FmiComponent::construct".to_owned(),
        "FmiLinkedRuntimeFacts@src/fmi/linked_runtime.rs::FmiLinkedRuntimeFacts::construct"
            .to_owned(),
        "FmiRuntimeView@src/fmi.rs::FmiComponent::into_runtime_view".to_owned(),
        "SolveModel@src/model.rs::SolveModel::construct".to_owned(),
    ])
}

fn expected_declarations() -> BTreeSet<String> {
    BTreeSet::from([
        "src/fmi.rs::FmiComponent".to_owned(),
        "src/fmi.rs::FmiRuntimeView".to_owned(),
        "src/fmi/linked_runtime.rs::FmiLinkedRuntimeFacts".to_owned(),
        "src/model.rs::SolveModel".to_owned(),
    ])
}

fn expected_declaration_visibilities() -> BTreeMap<String, String> {
    expected_declarations()
        .into_iter()
        .map(|declaration| (declaration, "pub".to_owned()))
        .collect()
}

fn expected_arc_fields() -> BTreeSet<String> {
    BTreeSet::from([
        "src/fmi.rs::FmiCodegenView::model".to_owned(),
        "src/fmi.rs::FmiComponent::linked_runtime_facts".to_owned(),
        "src/fmi.rs::FmiComponent::model".to_owned(),
        "src/fmi.rs::FmiRuntimeView::linked_runtime_facts".to_owned(),
        "src/fmi.rs::FmiRuntimeView::model".to_owned(),
        "src/fmi/event_free.rs::FmiEventFreeCodegenView::model".to_owned(),
    ])
}

fn expected_macro_fingerprints() -> BTreeMap<String, String> {
    BTreeMap::from([
        (
            "unexpanded-impl-macro:src/fmi/projection.rs::Fmi2DerivativeVariable::derivative_accessors".to_owned(),
            "3c399805703aafe6dd2dcce1386538ee1a00d4b5d21b9419ba50b74b27e8aaa2".to_owned(),
        ),
        (
            "unexpanded-impl-macro:src/fmi/projection.rs::Fmi2Projection::projection_accessors".to_owned(),
            "818120b3e50c4f88766a50b4384919107ec9593d3a04cbb2ae7270b75d2b8645".to_owned(),
        ),
        (
            "unexpanded-impl-macro:src/fmi/projection.rs::Fmi2ScalarVariable::scalar_accessors".to_owned(),
            "203f81fc7cebb07fcfc8091b8d72e77af814b3eab38322213bd1d8a74fdf57d3".to_owned(),
        ),
        (
            "unexpanded-impl-macro:src/fmi/projection.rs::Fmi3DerivativeVariable::derivative_accessors".to_owned(),
            "3c399805703aafe6dd2dcce1386538ee1a00d4b5d21b9419ba50b74b27e8aaa2".to_owned(),
        ),
        (
            "unexpanded-item-macro:src/fmi/projection.rs::derivative_accessors".to_owned(),
            "b11639da496ce98f630c24d0eec5a6b5db72431af69ed8c979be3404db6ecb7b".to_owned(),
        ),
        (
            "unexpanded-item-macro:src/fmi/projection.rs::projection_accessors".to_owned(),
            "c655a6abf8fbc80f90bfd57f9dcf9e952fbbdca5eff263ad1778cddcdf3a13ee".to_owned(),
        ),
        (
            "unexpanded-item-macro:src/fmi/projection.rs::scalar_accessors".to_owned(),
            "12727bf2c657cb97d40d19351492ecec104151cd95bee742100ed5a0591649a1".to_owned(),
        ),
    ])
}

fn expected_reexports() -> BTreeSet<String> {
    BTreeSet::from([
        "src/fmi.rs::pub-use:linked_runtime::FmiLinkedRuntimeFacts=>FmiLinkedRuntimeFacts"
            .to_owned(),
        "src/lib.rs::pub-use:model::*=>*".to_owned(),
        "src/lib.rs::pub-use:refresh::*=>*".to_owned(),
        "src/lib.rs::pub-use:typed_program::*=>*".to_owned(),
        "src/lib.rs::pub-use:variable_catalog::*=>*".to_owned(),
        "src/model.rs::pub-use:event_transaction::*=>*".to_owned(),
    ])
}

fn expected_reexport_visibilities() -> BTreeMap<String, String> {
    expected_reexports()
        .into_iter()
        .map(|reexport| {
            let (route, _) = reexport
                .rsplit_once("=>")
                .expect("expected re-export route has an alias");
            (route.to_owned(), "pub".to_owned())
        })
        .collect()
}

fn record_map_delta(
    actual: &BTreeMap<String, BTreeSet<String>>,
    expected: &BTreeMap<String, BTreeSet<String>>,
    label: &str,
    violations: &mut BTreeSet<String>,
) {
    for (route, types) in actual {
        if expected.get(route) != Some(types) {
            violations.insert(format!("unexpected-{label}:{route}:{types:?}"));
        }
    }
    for (route, types) in expected {
        if actual.get(route) != Some(types) {
            violations.insert(format!("missing-{label}:{route}:{types:?}"));
        }
    }
}

fn record_string_map_delta(
    actual: &BTreeMap<String, String>,
    expected: &BTreeMap<String, String>,
    label: &str,
    violations: &mut BTreeSet<String>,
) {
    for (route, value) in actual {
        if expected.get(route) != Some(value) {
            violations.insert(format!("unexpected-{label}:{route}:{value}"));
        }
    }
    for (route, value) in expected {
        if actual.get(route) != Some(value) {
            violations.insert(format!("missing-{label}:{route}:{value}"));
        }
    }
}

fn record_set_delta(
    actual: &BTreeSet<String>,
    expected: &BTreeSet<String>,
    label: &str,
    violations: &mut BTreeSet<String>,
) {
    for item in actual.difference(expected) {
        violations.insert(format!("unexpected-{label}:{item}"));
    }
    for item in expected.difference(actual) {
        violations.insert(format!("missing-{label}:{item}"));
    }
}

fn fmi_capability_surface_violations(inventory: &FmiCapabilityInventory) -> BTreeSet<String> {
    let mut violations = inventory.violations.clone();
    record_string_map_delta(
        &inventory.declaration_visibilities,
        &expected_declaration_visibilities(),
        "declaration-visibility",
        &mut violations,
    );
    record_string_map_delta(
        &inventory.reexport_visibilities,
        &expected_reexport_visibilities(),
        "reexport-visibility",
        &mut violations,
    );
    record_string_map_delta(
        &inventory.macro_fingerprints,
        &expected_macro_fingerprints(),
        "macro-fingerprint",
        &mut violations,
    );
    record_map_delta(
        &inventory.owned_outputs,
        &expected_owned_routes(),
        "owned-output",
        &mut violations,
    );
    record_map_delta(
        &inventory.owned_inputs,
        &expected_owned_inputs(),
        "owned-input",
        &mut violations,
    );
    for (actual, expected, label) in [
        (
            &inventory.self_consumers,
            expected_self_consumers(),
            "self-consumer",
        ),
        (&inventory.mints, expected_mints(), "mint"),
        (&inventory.owned_fields, BTreeSet::new(), "owned-field"),
        (
            &inventory.declarations,
            expected_declarations(),
            "declaration",
        ),
        (&inventory.arc_fields, expected_arc_fields(), "arc-field"),
        (&inventory.reexports, expected_reexports(), "reexport"),
    ] {
        record_set_delta(actual, &expected, label, &mut violations);
    }
    violations
}

#[test]
fn fmi_semantic_carriers_have_one_structural_construction_surface() {
    let root = workspace_root();
    let sources = production_rust_sources(&root.join("crates/rumoca-ir-solve"), &root);
    let violations = fmi_capability_surface_violations(&fmi_capability_inventory(&sources));
    assert!(
        violations.is_empty(),
        "FMI capability surface violations: {violations:#?}"
    );
    assert_fmi_capability_mutations(&sources);
}

fn append_mutation(
    sources: &[(PathBuf, String)],
    suffix: &str,
    addition: &str,
) -> Vec<(PathBuf, String)> {
    let mut mutation = sources.to_vec();
    let (_, source) = mutation
        .iter_mut()
        .find(|(path, _)| path.ends_with(suffix))
        .unwrap_or_else(|| panic!("missing mutation owner `{suffix}`"));
    source.push_str(addition);
    mutation
}

fn replace_mutation(
    sources: &[(PathBuf, String)],
    suffix: &str,
    before: &str,
    after: &str,
) -> Vec<(PathBuf, String)> {
    let mut mutation = sources.to_vec();
    let (_, source) = mutation
        .iter_mut()
        .find(|(path, _)| path.ends_with(suffix))
        .unwrap_or_else(|| panic!("missing mutation owner `{suffix}`"));
    assert_eq!(source.matches(before).count(), 1, "mutation anchor drift");
    *source = source.replacen(before, after, 1);
    mutation
}

fn assert_exact_mutation(sources: &[(PathBuf, String)], expected: &str) {
    let violations = fmi_capability_surface_violations(&fmi_capability_inventory(sources));
    assert!(
        violations.contains(expected),
        "mutation must reject exact route `{expected}`: {violations:#?}"
    );
}

fn assert_mutation_prefix(sources: &[(PathBuf, String)], expected_prefix: &str) {
    let violations = fmi_capability_surface_violations(&fmi_capability_inventory(sources));
    assert!(
        violations
            .iter()
            .any(|violation| violation.starts_with(expected_prefix)),
        "mutation must reject route prefix `{expected_prefix}`: {violations:#?}"
    );
}

fn assert_fmi_capability_mutations(sources: &[(PathBuf, String)]) {
    assert_arc_reference_and_container_mutations(sources);
    assert_tuple_generic_and_reexport_mutations(sources);
    assert_sibling_and_method_rename_mutations(sources);
    assert_scalar_derivative_proof_affinity_mutations(sources);
    assert_foreign_signature_mutation(sources);
    assert_aggregate_and_value_item_mutations(sources);
    assert_trait_path_and_default_surface_mutations(sources);
    assert_glob_visibility_and_macro_mutations(sources);
}

fn assert_scalar_derivative_proof_affinity_mutations(sources: &[(PathBuf, String)]) {
    let renamed_method = append_mutation(
        sources,
        "crates/rumoca-ir-solve/src/fmi/scalar_constant_derivative.rs",
        "\nimpl Fmi3ScalarConstantDerivativeCarrier { pub(crate) fn release_checked_view(self) -> FmiEventFreeCodegenView { self.view } }\n",
    );
    assert_exact_mutation(
        &renamed_method,
        "proof-affinity-owned-extraction:src/fmi/scalar_constant_derivative.rs::Fmi3ScalarConstantDerivativeCarrier::release_checked_view",
    );

    let visible_free_function = append_mutation(
        sources,
        "crates/rumoca-ir-solve/src/fmi/scalar_constant_derivative.rs",
        "\npub(crate) type HiddenScalarCarrier = Fmi3ScalarConstantDerivativeCarrier;\npub(crate) fn release_checked_view(carrier: Option<HiddenScalarCarrier>) -> Result<FmiEventFreeCodegenView, ()> { Ok(carrier.unwrap().view) }\n",
    );
    assert_exact_mutation(
        &visible_free_function,
        "proof-affinity-owned-extraction:src/fmi/scalar_constant_derivative.rs::release_checked_view",
    );

    for (addition, expected) in [
        (
            "\nimpl From<Fmi3ScalarConstantDerivativeCarrier> for FmiEventFreeCodegenView { fn from(carrier: Fmi3ScalarConstantDerivativeCarrier) -> Self { carrier.view } }\n",
            "proof-affinity-trait-extraction:src/fmi/scalar_constant_derivative.rs::FmiEventFreeCodegenView::implemented-trait",
        ),
        (
            "\nimpl Into<FmiEventFreeCodegenView> for Fmi3ScalarConstantDerivativeCarrier { fn into(self) -> FmiEventFreeCodegenView { self.view } }\n",
            "proof-affinity-trait-extraction:src/fmi/scalar_constant_derivative.rs::Fmi3ScalarConstantDerivativeCarrier::implemented-trait",
        ),
        (
            "\nimpl IntoIterator for Fmi3ScalarConstantDerivativeCarrier { type Item = FmiEventFreeCodegenView; type IntoIter = std::option::IntoIter<FmiEventFreeCodegenView>; fn into_iter(self) -> Self::IntoIter { Some(self.view).into_iter() } }\n",
            "proof-affinity-trait-extraction:src/fmi/scalar_constant_derivative.rs::Fmi3ScalarConstantDerivativeCarrier::into_iter",
        ),
    ] {
        let trait_route = append_mutation(
            sources,
            "crates/rumoca-ir-solve/src/fmi/scalar_constant_derivative.rs",
            addition,
        );
        assert_exact_mutation(&trait_route, expected);
    }
}

fn assert_arc_reference_and_container_mutations(sources: &[(PathBuf, String)]) {
    let reference = append_mutation(
        sources,
        "crates/rumoca-ir-solve/src/fmi.rs",
        "\nimpl FmiRuntimeView { pub fn escaped_facts(&self) -> &Arc<FmiLinkedRuntimeFacts> { &self.linked_runtime_facts } }\n",
    );
    assert_exact_mutation(
        &reference,
        "arc-owned-output:src/fmi.rs::FmiRuntimeView::escaped_facts:{\"FmiLinkedRuntimeFacts\"}",
    );

    let container = append_mutation(
        sources,
        "crates/rumoca-ir-solve/src/fmi.rs",
        "\npub fn escaped_models() -> Option<Vec<Arc<SolveModel>>> { None }\n",
    );
    assert_exact_mutation(
        &container,
        "arc-owned-output:src/fmi.rs::escaped_models:{\"SolveModel\"}",
    );
}

fn assert_tuple_generic_and_reexport_mutations(sources: &[(PathBuf, String)]) {
    let tuple = append_mutation(
        sources,
        "crates/rumoca-ir-solve/src/fmi.rs",
        "\npub fn split_view(view: FmiRuntimeView) -> (usize, FmiRuntimeView) { (0, view) }\n",
    );
    assert_exact_mutation(
        &tuple,
        "unexpected-owned-output:src/fmi.rs::split_view:{\"FmiRuntimeView\"}",
    );

    let generic = append_mutation(
        sources,
        "crates/rumoca-ir-solve/src/fmi.rs",
        "\npub fn generic_escape<T>() -> T where T: From<Arc<SolveModel>> { unreachable!() }\n",
    );
    assert_exact_mutation(
        &generic,
        "arc-owned-generic:src/fmi.rs::generic_escape:{\"SolveModel\"}",
    );

    let reexport = append_mutation(
        sources,
        "crates/rumoca-ir-solve/src/lib.rs",
        "\npub use crate::fmi::FmiRuntimeView as EscapedRuntimeView;\n",
    );
    assert_exact_mutation(
        &reexport,
        "renamed-protected-reexport:src/lib.rs::pub-use:crate::fmi::FmiRuntimeView=>EscapedRuntimeView",
    );
}

fn assert_sibling_and_method_rename_mutations(sources: &[(PathBuf, String)]) {
    let temporary = tempfile::tempdir().expect("temporary module-closure fixture");
    let src = temporary.path().join("src");
    fs::create_dir_all(&src).expect("create fixture source directory");
    fs::write(
        temporary.path().join("Cargo.toml"),
        "[package]\nname='fmi-capability-fixture'\nversion='0.0.0'\nedition='2024'\n",
    )
    .expect("write fixture manifest");
    fs::write(src.join("lib.rs"), "mod child;\n").expect("write fixture root");
    fs::write(
        src.join("child.rs"),
        "impl FmiRuntimeView { pub fn sibling_escape(&self) -> std::sync::Arc<SolveModel> { unreachable!() } }\n",
    )
    .expect("write fixture child");
    let child_sources = production_rust_sources(temporary.path(), temporary.path());
    let child = child_sources
        .iter()
        .find(|(path, _)| path.ends_with("src/child.rs"))
        .expect("module-closed scanner discovers sibling child")
        .clone();
    let mut sibling = sources.to_vec();
    sibling.push(child.clone());
    assert_exact_mutation(
        &sibling,
        "arc-owned-output:src/child.rs::FmiRuntimeView::sibling_escape:{\"SolveModel\"}",
    );

    let aliased_root = append_mutation(
        sources,
        "crates/rumoca-ir-solve/src/lib.rs",
        "\nuse crate::fmi::FmiRuntimeView as HiddenRuntimeView;\n",
    );
    let aliased_child = append_mutation(
        &aliased_root,
        "crates/rumoca-ir-solve/src/model.rs",
        "\npub fn aliased_child_escape(view: HiddenRuntimeView) { drop(view); }\n",
    );
    assert_exact_mutation(
        &aliased_child,
        "unexpected-owned-input:src/model.rs::aliased_child_escape:{\"FmiRuntimeView\"}",
    );

    let renamed = replace_mutation(
        sources,
        "crates/rumoca-ir-solve/src/fmi.rs",
        "pub fn into_runtime_view(self) -> FmiRuntimeView",
        "pub fn release_runtime_view(self) -> FmiRuntimeView",
    );
    assert_exact_mutation(
        &renamed,
        "unexpected-owned-output:src/fmi.rs::FmiComponent::release_runtime_view:{\"FmiRuntimeView\"}",
    );
}

fn assert_foreign_signature_mutation(sources: &[(PathBuf, String)]) {
    let foreign = append_mutation(
        sources,
        "crates/rumoca-ir-solve/src/fmi.rs",
        "\nunsafe extern \"C\" { pub fn escaped_foreign_model() -> Arc<SolveModel>; }\n",
    );
    assert_exact_mutation(
        &foreign,
        "arc-owned-output:src/fmi.rs::extern::escaped_foreign_model:{\"SolveModel\"}",
    );
}

fn assert_aggregate_and_value_item_mutations(sources: &[(PathBuf, String)]) {
    let enumeration = append_mutation(
        sources,
        "crates/rumoca-ir-solve/src/fmi.rs",
        "\nenum EscapedEnum { View(Arc<SolveModel>) }\n",
    );
    assert_exact_mutation(
        &enumeration,
        "unexpected-arc-field:src/fmi.rs::EscapedEnum::View::0",
    );

    let union = append_mutation(
        sources,
        "crates/rumoca-ir-solve/src/fmi.rs",
        "\nunion EscapedUnion { view: std::mem::ManuallyDrop<FmiRuntimeView> }\n",
    );
    assert_exact_mutation(
        &union,
        "unexpected-owned-field:src/fmi.rs::EscapedUnion::view",
    );

    let static_item = append_mutation(
        sources,
        "crates/rumoca-ir-solve/src/fmi.rs",
        "\nstatic ESCAPED_FACTS: Option<Arc<FmiLinkedRuntimeFacts>> = None;\n",
    );
    assert_exact_mutation(
        &static_item,
        "arc-owned-static:src/fmi.rs::ESCAPED_FACTS:{\"FmiLinkedRuntimeFacts\"}",
    );

    let const_item = append_mutation(
        sources,
        "crates/rumoca-ir-solve/src/fmi.rs",
        "\nconst ESCAPED_VIEW: Option<FmiRuntimeView> = None;\n",
    );
    assert_exact_mutation(
        &const_item,
        "owned-const:src/fmi.rs::ESCAPED_VIEW:{\"FmiRuntimeView\"}",
    );

    let associated_const = append_mutation(
        sources,
        "crates/rumoca-ir-solve/src/fmi.rs",
        "\ntrait EscapedConst { const VIEW: Option<Arc<SolveModel>>; }\n",
    );
    assert_exact_mutation(
        &associated_const,
        "arc-owned-associated-const:src/fmi.rs::trait EscapedConst::VIEW:{\"SolveModel\"}",
    );

    let impl_const = append_mutation(
        sources,
        "crates/rumoca-ir-solve/src/fmi.rs",
        "\nimpl FmiRuntimeView { const FACTS: Option<Arc<FmiLinkedRuntimeFacts>> = None; }\n",
    );
    assert_exact_mutation(
        &impl_const,
        "arc-owned-associated-const:src/fmi.rs::FmiRuntimeView::FACTS:{\"FmiLinkedRuntimeFacts\"}",
    );
}

fn assert_trait_path_and_default_surface_mutations(sources: &[(PathBuf, String)]) {
    let implemented_trait = append_mutation(
        sources,
        "crates/rumoca-ir-solve/src/fmi.rs",
        "\ntrait EscapedCarrier<T> {} struct EscapedSink; impl EscapedCarrier<Arc<SolveModel>> for EscapedSink {}\n",
    );
    assert_exact_mutation(
        &implemented_trait,
        "arc-owned-implemented-trait:src/fmi.rs::EscapedSink::implemented-trait:{\"SolveModel\"}",
    );

    let default_surface = append_mutation(
        sources,
        "crates/rumoca-ir-solve/src/fmi.rs",
        "\ntrait EscapedDefault<T = Arc<FmiLinkedRuntimeFacts>> {}\n",
    );
    assert_exact_mutation(
        &default_surface,
        "arc-owned-generic:src/fmi.rs::trait EscapedDefault::generics:{\"FmiLinkedRuntimeFacts\"}",
    );

    let supertrait = append_mutation(
        sources,
        "crates/rumoca-ir-solve/src/fmi.rs",
        "\ntrait EscapedSuperCarrier<T> {} trait EscapedSuper: EscapedSuperCarrier<Arc<SolveModel>> {}\n",
    );
    assert_exact_mutation(
        &supertrait,
        "arc-owned-supertrait:src/fmi.rs::trait EscapedSuper::supertraits:{\"SolveModel\"}",
    );
}

fn assert_glob_visibility_and_macro_mutations(sources: &[(PathBuf, String)]) {
    let glob = append_mutation(
        sources,
        "crates/rumoca-ir-solve/src/lib.rs",
        "\npub mod escaped_glob { pub use crate::fmi::*; }\n",
    );
    assert_exact_mutation(
        &glob,
        "unexpected-reexport:src/lib.rs::escaped_glob::pub-use:crate::fmi::*=>*",
    );

    let glob_visibility = replace_mutation(
        sources,
        "crates/rumoca-ir-solve/src/lib.rs",
        "pub use model::*;",
        "pub(crate) use model::*;",
    );
    assert_exact_mutation(
        &glob_visibility,
        "unexpected-reexport-visibility:src/lib.rs::pub-use:model::*:pub (crate)",
    );

    let visibility = replace_mutation(
        sources,
        "crates/rumoca-ir-solve/src/fmi.rs",
        "pub struct FmiRuntimeView",
        "pub(crate) struct FmiRuntimeView",
    );
    assert_exact_mutation(
        &visibility,
        "unexpected-declaration-visibility:src/fmi.rs::FmiRuntimeView:pub (crate)",
    );

    let reviewed_macro_body = replace_mutation(
        sources,
        "crates/rumoca-ir-solve/src/fmi/projection.rs",
        "macro_rules! scalar_accessors {",
        "macro_rules! scalar_accessors { const _: () = ();",
    );
    assert_mutation_prefix(
        &reviewed_macro_body,
        "unexpected-macro-fingerprint:unexpanded-item-macro:src/fmi/projection.rs::scalar_accessors:",
    );

    let duplicate_macro = append_mutation(
        sources,
        "crates/rumoca-ir-solve/src/fmi/projection.rs",
        "\nmacro_rules! scalar_accessors { () => {}; }\n",
    );
    assert_exact_mutation(
        &duplicate_macro,
        "duplicate-macro-fingerprint:unexpanded-item-macro:src/fmi/projection.rs::scalar_accessors",
    );

    for (addition, expected_prefix) in [
        (
            "\nescaped_item_macro!();\n",
            "unexpected-macro-fingerprint:unexpanded-item-macro:src/fmi.rs::escaped_item_macro:",
        ),
        (
            "\nimpl FmiRuntimeView { escaped_impl_macro!(); }\n",
            "unexpected-macro-fingerprint:unexpanded-impl-macro:src/fmi.rs::FmiRuntimeView::escaped_impl_macro:",
        ),
        (
            "\ntrait EscapedMacroTrait { escaped_trait_macro!(); }\n",
            "unexpected-macro-fingerprint:unexpanded-trait-macro:src/fmi.rs::trait EscapedMacroTrait::escaped_trait_macro:",
        ),
    ] {
        let mutation = append_mutation(sources, "crates/rumoca-ir-solve/src/fmi.rs", addition);
        assert_mutation_prefix(&mutation, expected_prefix);
    }

    let type_macro = append_mutation(
        sources,
        "crates/rumoca-ir-solve/src/fmi.rs",
        "\ntype EscapedMacroType = escaped_type_macro!();\n",
    );
    assert_exact_mutation(
        &type_macro,
        "unexpanded-type-macro:alias:src/fmi.rs::EscapedMacroType",
    );
}

#[test]
fn obsolete_fmi_crates_are_absorbed_without_shims() {
    let root = workspace_root();
    for removed in ["rumoca-ir-fmi", "rumoca-phase-fmi"] {
        assert!(
            !root.join("crates").join(removed).exists(),
            "{removed} must be deleted, not retained as a compatibility shim"
        );
    }

    let workspace = fs::read_to_string(root.join("Cargo.toml")).expect("read workspace manifest");
    assert!(!workspace.contains("rumoca-ir-fmi"));
    assert!(!workspace.contains("rumoca-phase-fmi"));

    let ir_root = fs::read_to_string(root.join("crates/rumoca-ir-solve/src/lib.rs"))
        .expect("read Solve IR root");
    assert!(ir_root.lines().any(|line| line == "pub mod fmi;"));

    let phase_root = fs::read_to_string(root.join("crates/rumoca-phase-solve/src/lib.rs"))
        .expect("read Solve phase root");
    assert!(phase_root.lines().any(|line| line == "pub mod fmi;"));
}

#[test]
fn fmi_runtime_projection_is_unconditional_but_export_apis_remain_feature_scoped() {
    let phase = manifest("rumoca-phase-solve");
    assert!(
        phase["features"].get("fmi").is_none(),
        "the canonical runtime projection is not an optional phase capability"
    );
    assert!(phase["dependencies"]["rumoca-eval-dae"].is_table());

    let sim = manifest("rumoca-sim");
    assert!(feature_members(&sim, "fmi").is_empty());

    let cli = manifest("rumoca");
    assert_eq!(feature_members(&cli, "fmi"), ["rumoca-sim/fmi"]);
    assert!(feature_members(&cli, "fmu-packaging").contains(&"fmi".to_string()));
    assert!(
        cli["dependencies"].get("rumoca-phase-solve").is_none(),
        "the CLI must reach FMI lowering only through rumoca-sim"
    );
}

#[test]
fn fmi_codegen_retains_one_nonconstructible_correlated_aggregate() {
    let root = workspace_root();
    let component = fs::read_to_string(root.join("crates/rumoca-ir-solve/src/fmi.rs"))
        .expect("read FMI component owner");
    assert!(component.contains("model: Arc<SolveModel>"));
    assert!(!derive_prefix(&component, "pub struct FmiComponent").contains("Clone"));
    assert!(!derive_prefix(&component, "pub struct FmiRuntimeView").contains("Clone"));
    assert!(!component.contains("fn shared_model"));
    assert!(!derive_prefix(&component, "pub struct FmiCodegenView").contains("Clone"));
    assert!(component.contains("pub fn into_codegen_view(self) -> FmiCodegenView"));
    for escape in [
        "into_solve",
        "into_retained_model",
        "-> Arc<SolveModel>",
        "pub fn new(",
    ] {
        assert!(
            !component.contains(escape),
            "FMI checked aggregate exposes banned construction/ownership escape `{escape}`"
        );
    }

    let lazy =
        fs::read_to_string(root.join("crates/rumoca-phase-codegen/src/codegen/solve_lazy.rs"))
            .expect("read Solve lazy renderer");
    assert!(lazy.contains("pub(super) enum SolveRenderHandle"));
    assert!(lazy.contains("Fmi(Arc<solve::fmi::FmiEventFreeCodegenView>)"));
    assert!(lazy.contains("Self::Fmi(component) => component.problem()"));
    assert!(lazy.contains("Self::Fmi(component) => component.artifacts()"));
    assert!(lazy.contains("Self::Standalone(_) => None"));
    assert!(
        lazy.contains("Self::Fmi(component) => Some(Value::from_serialize(component.as_ref()))")
    );
    assert!(
        !lazy.contains("\"algebraic_assignment_complete\""),
        "the lazy template context must not recreate an FMI admission decision"
    );

    let renderer =
        fs::read_to_string(root.join("crates/rumoca-phase-codegen/src/codegen/solve_renderer.rs"))
            .expect("read Solve renderer");
    let fmi_owner = renderer
        .split_once("impl PreparedFmiComponentRendering")
        .map(|(_, owner)| owner)
        .expect("locate prepared FMI rendering owner");
    let fmi_constructor = function_signature(fmi_owner, "prepare");
    assert!(fmi_constructor.contains("FmiEventFreeCodegenView"));
    assert!(!fmi_constructor.contains("SolveArtifacts"));
    assert!(!fmi_constructor.contains("artifacts"));
    assert!(renderer.contains("SolveRenderHandle::fmi(component)"));
    assert_eq!(
        renderer
            .matches("explicit_algebraic_assignment_complete(problem)")
            .count(),
        1,
        "FMI algebraic completeness must be decided exactly once by admission"
    );
    let context_constructor =
        function_signature(&renderer, "solve_render_context_value_with_handles");
    assert!(
        !context_constructor.contains("fmi_entry"),
        "serialized FMI metadata must be derived from the retained handle, not supplied separately"
    );
    assert!(renderer.contains("let fmi_entry = handle.fmi_value();"));
}
