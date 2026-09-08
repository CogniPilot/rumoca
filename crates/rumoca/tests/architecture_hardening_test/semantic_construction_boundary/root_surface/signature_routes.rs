use super::*;

pub(super) fn collect_signature_route(
    context: SignatureRouteContext<'_>,
    carriers: &BTreeSet<String>,
    routes: &mut BTreeSet<RouteSignature>,
) {
    let SignatureRouteContext {
        identity,
        attributes,
        visibility,
        signature,
        self_is_carrier,
        trait_path,
    } = context;
    let mut kinds = BTreeSet::new();
    let generic_facts = generic_root_capabilities(&signature.generics, carriers);
    if generic_facts.mentions_root {
        kinds.insert(RouteKind::GenericConstraint);
    }
    if generic_facts.factory {
        kinds.insert(RouteKind::Factory);
    }
    if generic_facts.mutates_root {
        kinds.insert(RouteKind::MutatesRoot);
    }
    let output = match &signature.output {
        ReturnType::Default => None,
        ReturnType::Type(_, ty) => Some(ty.as_ref()),
    };
    if let Some(output) = output {
        let output_mentions_root = type_mentions_names(output, carriers)
            || (self_is_carrier && type_mentions_self(output));
        if output_mentions_root {
            if type_contains_reference(output) {
                kinds.insert(RouteKind::BorrowsRoot);
            } else {
                kinds.insert(RouteKind::ProducesRoot);
            }
        }
        if type_is_root_factory(output, carriers) {
            kinds.insert(RouteKind::Factory);
        }
        if (self_is_carrier || output_mentions_root) && type_contains_mutable_capability(output) {
            kinds.insert(RouteKind::MutableProjection);
        }
    }
    for input in &signature.inputs {
        match input {
            FnArg::Receiver(receiver) if self_is_carrier => {
                if receiver.reference.is_none() {
                    kinds.insert(RouteKind::ConsumesRoot);
                } else if receiver.mutability.is_some() {
                    kinds.insert(RouteKind::MutatesRoot);
                } else {
                    kinds.insert(RouteKind::BorrowsRoot);
                }
            }
            FnArg::Typed(argument) if type_mentions_names(&argument.ty, carriers) => {
                if type_contains_mutable_capability(&argument.ty) {
                    kinds.insert(RouteKind::MutatesRoot);
                } else if type_contains_reference(&argument.ty) {
                    kinds.insert(RouteKind::BorrowsRoot);
                } else {
                    kinds.insert(RouteKind::ConsumesRoot);
                }
            }
            FnArg::Typed(argument) if type_callback_mutates_root(&argument.ty, carriers) => {
                kinds.insert(RouteKind::MutatesRoot);
            }
            _ => {}
        }
    }
    if self_is_carrier && is_semantic_check_name(&signature.ident.to_string()) {
        kinds.insert(RouteKind::SemanticCheckCandidate);
    }
    if self_is_carrier
        && trait_path.is_some_and(|path| {
            ["AsMut", "BorrowMut", "DerefMut", "IndexMut"]
                .iter()
                .any(|name| token_string_mentions(path, name))
        })
    {
        kinds.insert(RouteKind::MutableTrait);
    }
    if kinds.is_empty() {
        return;
    }
    insert_route_set(
        routes,
        identity,
        function_signature(attributes, visibility, signature),
        kinds,
    );
}

pub(super) fn type_mentions_self(ty: &Type) -> bool {
    match ty {
        Type::Array(array) => type_mentions_self(&array.elem),
        Type::BareFn(function) => {
            function
                .inputs
                .iter()
                .any(|input| type_mentions_self(&input.ty))
                || matches!(
                    &function.output,
                    ReturnType::Type(_, ty) if type_mentions_self(ty)
                )
        }
        Type::Group(group) => type_mentions_self(&group.elem),
        Type::Paren(parenthesized) => type_mentions_self(&parenthesized.elem),
        Type::Path(path) => {
            (path.qself.is_none() && path.path.segments.len() == 1 && path.path.is_ident("Self"))
                || path
                    .path
                    .segments
                    .iter()
                    .any(|segment| {
                        match &segment.arguments {
                    PathArguments::AngleBracketed(arguments) => arguments.args.iter().any(|arg| {
                        matches!(arg, GenericArgument::Type(ty) if type_mentions_self(ty))
                    }),
                    PathArguments::Parenthesized(arguments) => {
                        arguments.inputs.iter().any(type_mentions_self)
                            || matches!(
                                &arguments.output,
                                ReturnType::Type(_, ty) if type_mentions_self(ty)
                            )
                    }
                    PathArguments::None => false,
                }
                    })
        }
        Type::Ptr(pointer) => type_mentions_self(&pointer.elem),
        Type::Reference(reference) => type_mentions_self(&reference.elem),
        Type::Slice(slice) => type_mentions_self(&slice.elem),
        Type::Tuple(tuple) => tuple.elems.iter().any(type_mentions_self),
        _ => false,
    }
}

pub(super) fn collect_reexport_route(
    item_use: &syn::ItemUse,
    module_path: &[String],
    spec: &RootSpec,
    carriers: &BTreeSet<String>,
    routes: &mut BTreeSet<RouteSignature>,
) {
    let tokens = item_use.tree.to_token_stream().to_string();
    let mut kinds = BTreeSet::new();
    if carriers
        .iter()
        .any(|name| token_string_mentions(&tokens, name))
    {
        kinds.insert(RouteKind::Reexport);
    }
    if use_tree_contains_glob(&item_use.tree) {
        kinds.insert(RouteKind::UnresolvedGlobReexportBoundary);
    }
    if kinds.is_empty() {
        return;
    }
    insert_route_set(
        routes,
        item_identity(spec, module_path, &format!("use:{tokens}")),
        use_signature(item_use),
        kinds,
    );
}

pub(super) fn use_tree_contains_glob(tree: &syn::UseTree) -> bool {
    match tree {
        syn::UseTree::Glob(_) => true,
        syn::UseTree::Group(group) => group.items.iter().any(use_tree_contains_glob),
        syn::UseTree::Name(_) | syn::UseTree::Rename(_) => false,
        syn::UseTree::Path(path) => use_tree_contains_glob(&path.tree),
    }
}

pub(super) fn insert_route<const N: usize>(
    routes: &mut BTreeSet<RouteSignature>,
    identity: String,
    signature: String,
    kinds: [RouteKind; N],
) {
    insert_route_set(routes, identity, signature, kinds.into_iter().collect());
}

pub(super) fn insert_route_set(
    routes: &mut BTreeSet<RouteSignature>,
    identity: String,
    signature: String,
    kinds: BTreeSet<RouteKind>,
) {
    let route = RouteSignature {
        identity,
        signature,
        kinds,
        disposition: RouteDisposition::MigrationDebt,
    };
    assert!(
        routes.insert(route.clone()),
        "duplicate exact root-route record: {route:#?}"
    );
}

pub(super) fn item_identity(spec: &RootSpec, module_path: &[String], item: &str) -> String {
    let module = module_path.join("::");
    if module.is_empty() {
        format!("{}::{item}", spec.crate_name)
    } else {
        format!("{}::{module}::{item}", spec.crate_name)
    }
}

pub(super) fn impl_identity(
    spec: &RootSpec,
    module_path: &[String],
    implementation: &syn::ItemImpl,
) -> String {
    let owner = implementation.self_ty.to_token_stream().to_string();
    let item = implementation.trait_.as_ref().map_or_else(
        || format!("<{owner}>::<inherent>"),
        |(_, path, _)| format!("<{owner} as {}>", path.to_token_stream()),
    );
    item_identity(spec, module_path, &item)
}

pub(super) fn method_identity(
    spec: &RootSpec,
    module_path: &[String],
    implementation: &syn::ItemImpl,
    method: &str,
) -> String {
    format!(
        "{}::{method}",
        impl_identity(spec, module_path, implementation)
    )
}

pub(super) fn function_signature(
    attributes: &[syn::Attribute],
    visibility: &Visibility,
    signature: &syn::Signature,
) -> String {
    format!(
        "{}{} {}",
        normalized_attributes(attributes),
        visibility.to_token_stream(),
        signature.to_token_stream()
    )
    .trim()
    .to_owned()
}

pub(super) fn retain_signature_attributes(attributes: &mut Vec<syn::Attribute>) {
    attributes.retain(|attribute| !attribute.path().is_ident("doc"));
}

pub(super) fn retain_field_signature_attributes(fields: &mut Fields) {
    for field in fields.iter_mut() {
        retain_signature_attributes(&mut field.attrs);
    }
}

pub(super) fn struct_signature(item: &syn::ItemStruct) -> String {
    let mut item = item.clone();
    retain_signature_attributes(&mut item.attrs);
    retain_field_signature_attributes(&mut item.fields);
    item.to_token_stream().to_string()
}

pub(super) fn enum_signature(item: &syn::ItemEnum) -> String {
    let mut item = item.clone();
    retain_signature_attributes(&mut item.attrs);
    for variant in &mut item.variants {
        retain_signature_attributes(&mut variant.attrs);
        retain_field_signature_attributes(&mut variant.fields);
    }
    item.to_token_stream().to_string()
}

pub(super) fn union_signature(item: &syn::ItemUnion) -> String {
    let mut item = item.clone();
    retain_signature_attributes(&mut item.attrs);
    for field in &mut item.fields.named {
        retain_signature_attributes(&mut field.attrs);
    }
    item.to_token_stream().to_string()
}

pub(super) fn type_alias_signature(item: &syn::ItemType) -> String {
    let mut item = item.clone();
    retain_signature_attributes(&mut item.attrs);
    item.to_token_stream().to_string()
}

pub(super) fn use_signature(item: &syn::ItemUse) -> String {
    let mut item = item.clone();
    retain_signature_attributes(&mut item.attrs);
    item.to_token_stream().to_string()
}

pub(super) fn const_signature(item: &syn::ItemConst) -> String {
    format!(
        "{}{} const {} : {}",
        normalized_attributes(&item.attrs),
        item.vis.to_token_stream(),
        item.ident,
        item.ty.to_token_stream()
    )
}

pub(super) fn static_signature(item: &syn::ItemStatic) -> String {
    format!(
        "{}{} static {} {} : {}",
        normalized_attributes(&item.attrs),
        item.vis.to_token_stream(),
        item.mutability.to_token_stream(),
        item.ident,
        item.ty.to_token_stream()
    )
}

pub(super) fn foreign_static_signature(item: &syn::ForeignItemStatic) -> String {
    format!(
        "{}{} static {} {} : {}",
        normalized_attributes(&item.attrs),
        item.vis.to_token_stream(),
        item.mutability.to_token_stream(),
        item.ident,
        item.ty.to_token_stream()
    )
}

pub(super) fn impl_const_signature(item: &syn::ImplItemConst) -> String {
    format!(
        "{}{} const {} {} : {}{}",
        normalized_attributes(&item.attrs),
        item.vis.to_token_stream(),
        item.defaultness.to_token_stream(),
        item.ident,
        item.ty.to_token_stream(),
        item.generics.where_clause.to_token_stream()
    )
}

pub(super) fn impl_type_signature(item: &syn::ImplItemType) -> String {
    let mut item = item.clone();
    retain_signature_attributes(&mut item.attrs);
    item.to_token_stream().to_string()
}

pub(super) fn trait_const_signature(item: &syn::TraitItemConst) -> String {
    let mut item = item.clone();
    item.default = None;
    retain_signature_attributes(&mut item.attrs);
    item.to_token_stream().to_string()
}

pub(super) fn trait_type_signature(item: &syn::TraitItemType) -> String {
    let mut item = item.clone();
    retain_signature_attributes(&mut item.attrs);
    item.to_token_stream().to_string()
}

pub(super) fn field_signature(field: &syn::Field) -> String {
    let name = field
        .ident
        .as_ref()
        .map(ToString::to_string)
        .unwrap_or_default();
    format!(
        "{}{} {name} : {}",
        normalized_attributes(&field.attrs),
        field.vis.to_token_stream(),
        field.ty.to_token_stream()
    )
    .trim()
    .to_owned()
}

pub(super) fn impl_header_signature(implementation: &syn::ItemImpl) -> String {
    let polarity = implementation
        .trait_
        .as_ref()
        .and_then(|(polarity, _, _)| polarity.as_ref())
        .map_or("", |_| "!");
    let trait_name = implementation
        .trait_
        .as_ref()
        .map_or_else(String::new, |(_, path, _)| {
            format!("{polarity}{} for ", path.to_token_stream())
        });
    format!(
        "{}impl {}{}{} {}",
        normalized_attributes(&implementation.attrs),
        implementation.generics.to_token_stream(),
        trait_name,
        implementation.self_ty.to_token_stream(),
        implementation.generics.where_clause.to_token_stream()
    )
    .trim()
    .to_owned()
}

pub(super) fn normalized_attributes(attributes: &[syn::Attribute]) -> String {
    let tokens = attributes
        .iter()
        .filter(|attribute| !attribute.path().is_ident("doc"))
        .map(|attribute| attribute.to_token_stream().to_string())
        .collect::<Vec<_>>()
        .join(" ");
    if tokens.is_empty() {
        String::new()
    } else {
        format!("{tokens} ")
    }
}

pub(super) fn token_string_mentions(tokens: &str, expected: &str) -> bool {
    tokens
        .split(|character: char| !character.is_alphanumeric() && character != '_')
        .any(|token| token == expected)
}

pub(super) fn production_root_surface(
    crate_root: &Path,
    workspace: &Path,
    spec: &RootSpec,
) -> RootSurface {
    let mut surface = RootSurface::default();
    let mut declarations = 0usize;
    let contexts = production_rust_source_contexts(crate_root, workspace);
    for context in &contexts {
        let parsed = syn::parse_file(&context.source).unwrap_or_else(|error| {
            panic!(
                "parse {} for root-surface gate: {error}",
                context.path.display()
            )
        });
        collect_context_surface(
            &parsed.items,
            &context.module_path,
            spec,
            &mut declarations,
            &mut surface,
        );
    }
    assert_eq!(
        declarations, 1,
        "expected exactly one declaration of {}::{} in module `{}`",
        spec.crate_name, spec.root, spec.declaration_module
    );
    surface.routes = collect_route_inventory(&contexts, spec);
    surface
}

pub(super) fn collect_context_surface(
    items: &[Item],
    module_path: &[String],
    spec: &RootSpec,
    declarations: &mut usize,
    surface: &mut RootSurface,
) {
    let module = module_path.join("::");
    for item in items {
        match item {
            Item::Struct(structure)
                if !attributes_require_test(&structure.attrs)
                    && module == spec.declaration_module
                    && structure.ident == spec.root =>
            {
                *declarations += 1;
                collect_struct_surface(structure, surface);
            }
            Item::Impl(implementation)
                if !attributes_require_test(&implementation.attrs)
                    && simple_type_name(&implementation.self_ty).as_deref() == Some(spec.root) =>
            {
                collect_impl_surface(implementation, &module, spec.root, surface);
            }
            Item::Mod(nested) if !attributes_require_test(&nested.attrs) => {
                let Some((_, items)) = &nested.content else {
                    continue;
                };
                let mut child_path = module_path.to_vec();
                child_path.push(nested.ident.to_string());
                collect_context_surface(items, &child_path, spec, declarations, surface);
            }
            _ => {}
        }
    }
}

pub(super) fn collect_struct_surface(structure: &syn::ItemStruct, surface: &mut RootSurface) {
    surface
        .derived_traits
        .extend(derived_trait_names(&structure.attrs));
    if derives_default(&structure.attrs) {
        surface.default_authorities.insert("derive".to_string());
    }
    surface.deserialization_authorities += deserialization_derive_count(&structure.attrs);
    surface.serde_attributes += serde_attribute_count(&structure.attrs);
    match &structure.fields {
        Fields::Named(fields) => {
            for field in &fields.named {
                surface.serde_attributes += serde_attribute_count(&field.attrs);
                if is_public(&field.vis) {
                    surface.public_fields.insert(
                        field
                            .ident
                            .as_ref()
                            .expect("named field has an identifier")
                            .to_string(),
                    );
                }
            }
        }
        Fields::Unnamed(fields) => {
            for (index, field) in fields.unnamed.iter().enumerate() {
                surface.serde_attributes += serde_attribute_count(&field.attrs);
                if is_public(&field.vis) {
                    surface.public_fields.insert(format!("#{index}"));
                }
            }
        }
        Fields::Unit => {}
    }
}

pub(super) fn collect_impl_surface(
    implementation: &syn::ItemImpl,
    module: &str,
    root: &str,
    surface: &mut RootSurface,
) {
    if let Some((_, path, _)) = &implementation.trait_
        && let Some(trait_name) = path
            .segments
            .last()
            .map(|segment| segment.ident.to_string())
    {
        surface
            .trait_impls
            .insert(path.to_token_stream().to_string());
        surface.public_mutators += implementation
            .items
            .iter()
            .filter(|item| {
                let ImplItem::Fn(method) = item else {
                    return false;
                };
                !attributes_require_test(&method.attrs) && has_mutable_receiver(&method.sig)
            })
            .count();
        for item in &implementation.items {
            let ImplItem::Fn(method) = item else {
                continue;
            };
            let route = format!(
                "<{} as {}>::{}",
                root,
                path.to_token_stream(),
                method.sig.ident
            );
            if return_type_constructs_root(&method.sig.output, root) {
                surface.public_root_producers.insert(route.clone());
            }
            if returns_mutable_reference(&method.sig.output) {
                surface.public_mutable_projections.insert(route.clone());
            }
            if owned_self_receiver(&method.sig)
                && !return_type_constructs_root(&method.sig.output, root)
            {
                surface.public_consuming_extractions.insert(route.clone());
            }
            if is_semantic_check_name(&method.sig.ident.to_string()) {
                surface.public_semantic_checks.insert(route);
            }
        }
        match trait_name.as_str() {
            "Default" => {
                surface
                    .default_authorities
                    .insert(format!("impl{}", module_suffix(module)));
            }
            "Deserialize" | "DeserializeOwned" => surface.deserialization_authorities += 1,
            "From" | "TryFrom" | "FromIterator" => surface.conversion_constructors += 1,
            "Deref" => surface.deref += 1,
            "DerefMut" => surface.deref_mut += 1,
            _ => {}
        }
        return;
    }
    for item in &implementation.items {
        let ImplItem::Fn(method) = item else {
            continue;
        };
        if attributes_require_test(&method.attrs) || !is_public(&method.vis) {
            continue;
        }
        if has_mutable_receiver(&method.sig) {
            surface.public_mutators += 1;
        }
        let route = method.sig.ident.to_string();
        if return_type_constructs_root(&method.sig.output, root) {
            surface.public_root_producers.insert(route.clone());
        }
        if returns_mutable_reference(&method.sig.output) {
            surface.public_mutable_projections.insert(route.clone());
        }
        if owned_self_receiver(&method.sig)
            && !return_type_constructs_root(&method.sig.output, root)
        {
            surface.public_consuming_extractions.insert(route.clone());
        }
        if is_semantic_check_name(&route) {
            surface.public_semantic_checks.insert(route.clone());
        }
        match method.sig.ident.to_string().as_str() {
            "new" => surface.public_new += 1,
            "validate" => surface.public_validate += 1,
            "construct" if returns_result(&method.sig.output) => {
                surface.public_result_construct += 1;
            }
            _ => {}
        }
    }
}

pub(super) fn return_type_constructs_root(output: &ReturnType, root: &str) -> bool {
    let ReturnType::Type(_, ty) = output else {
        return false;
    };
    type_constructs_root(ty, root)
}

pub(super) fn type_constructs_root(ty: &Type, root: &str) -> bool {
    match ty {
        Type::Path(path) => path.path.segments.iter().any(|segment| {
            segment.ident == "Self"
                || segment.ident == root
                || match &segment.arguments {
                    PathArguments::AngleBracketed(arguments) => arguments.args.iter().any(|arg| {
                        matches!(arg, GenericArgument::Type(ty) if type_constructs_root(ty, root))
                    }),
                    PathArguments::Parenthesized(arguments) => {
                        arguments
                            .inputs
                            .iter()
                            .any(|ty| type_constructs_root(ty, root))
                            || matches!(
                                &arguments.output,
                                ReturnType::Type(_, ty) if type_constructs_root(ty, root)
                            )
                    }
                    PathArguments::None => false,
                }
        }),
        Type::Group(group) => type_constructs_root(&group.elem, root),
        Type::Paren(parenthesized) => type_constructs_root(&parenthesized.elem, root),
        Type::Tuple(tuple) => tuple
            .elems
            .iter()
            .any(|element| type_constructs_root(element, root)),
        // The source scanner cannot resolve an opaque return. It must list the
        // route for review rather than assume the hidden type is harmless.
        Type::ImplTrait(_) => true,
        // A borrowed root is a projection, not a newly constructible value.
        Type::Reference(_) | Type::Ptr(_) => false,
        _ => false,
    }
}

pub(super) fn returns_mutable_reference(output: &ReturnType) -> bool {
    let ReturnType::Type(_, ty) = output else {
        return false;
    };
    type_contains_mutable_reference(ty)
}

pub(super) fn type_contains_mutable_reference(ty: &Type) -> bool {
    match ty {
        Type::Reference(reference) => reference.mutability.is_some(),
        Type::Group(group) => type_contains_mutable_reference(&group.elem),
        Type::Paren(parenthesized) => type_contains_mutable_reference(&parenthesized.elem),
        Type::Tuple(tuple) => tuple.elems.iter().any(type_contains_mutable_reference),
        Type::Path(path) => path.path.segments.iter().any(|segment| {
            let PathArguments::AngleBracketed(arguments) = &segment.arguments else {
                return false;
            };
            arguments.args.iter().any(|argument| {
                matches!(argument, GenericArgument::Type(ty) if type_contains_mutable_reference(ty))
            })
        }),
        _ => false,
    }
}

pub(super) fn owned_self_receiver(signature: &syn::Signature) -> bool {
    let Some(receiver) = signature.receiver() else {
        return false;
    };
    if receiver.reference.is_some() {
        return false;
    }
    !matches!(receiver.ty.as_ref(), Type::Reference(_))
}

pub(super) fn is_semantic_check_name(name: &str) -> bool {
    ["validate", "check", "verify", "repair", "finalize", "close"]
        .iter()
        .any(|prefix| name == *prefix || name.starts_with(&format!("{prefix}_")))
}

pub(super) fn has_mutable_receiver(signature: &syn::Signature) -> bool {
    let Some(receiver) = signature.receiver() else {
        return false;
    };
    if receiver.reference.is_some() && receiver.mutability.is_some() {
        return true;
    }
    matches!(
        receiver.ty.as_ref(),
        Type::Reference(reference) if reference.mutability.is_some()
    )
}

pub(super) fn module_suffix(module: &str) -> String {
    if module.is_empty() {
        String::new()
    } else {
        format!("@{module}")
    }
}

pub(super) fn derives_default(attributes: &[syn::Attribute]) -> bool {
    attributes.iter().any(|attribute| {
        if !attribute.path().is_ident("derive") {
            return false;
        }
        attribute
            .parse_args_with(Punctuated::<syn::Path, syn::Token![,]>::parse_terminated)
            .is_ok_and(|paths| {
                paths.iter().any(|path| {
                    path.segments
                        .last()
                        .is_some_and(|segment| segment.ident == "Default")
                })
            })
    })
}

pub(super) fn derived_trait_names(attributes: &[syn::Attribute]) -> BTreeSet<String> {
    attributes
        .iter()
        .filter(|attribute| attribute.path().is_ident("derive"))
        .filter_map(|attribute| {
            attribute
                .parse_args_with(Punctuated::<syn::Path, syn::Token![,]>::parse_terminated)
                .ok()
        })
        .flatten()
        .map(|path| path.to_token_stream().to_string())
        .collect()
}

pub(super) fn deserialization_derive_count(attributes: &[syn::Attribute]) -> usize {
    attributes
        .iter()
        .filter(|attribute| attribute.path().is_ident("derive"))
        .filter_map(|attribute| {
            attribute
                .parse_args_with(Punctuated::<syn::Path, syn::Token![,]>::parse_terminated)
                .ok()
        })
        .flatten()
        .filter(|path| {
            path.segments.last().is_some_and(|segment| {
                matches!(
                    segment.ident.to_string().as_str(),
                    "Deserialize" | "DeserializeOwned"
                )
            })
        })
        .count()
}

pub(super) fn serde_attribute_count(attributes: &[syn::Attribute]) -> usize {
    attributes
        .iter()
        .filter(|attribute| attribute.path().is_ident("serde"))
        .count()
}

pub(super) fn returns_result(output: &ReturnType) -> bool {
    let ReturnType::Type(_, ty) = output else {
        return false;
    };
    let Type::Path(path) = ty.as_ref() else {
        return false;
    };
    path.path
        .segments
        .last()
        .is_some_and(|segment| segment.ident == "Result")
}

pub(super) fn simple_type_name(ty: &Type) -> Option<String> {
    let Type::Path(path) = ty else {
        return None;
    };
    path.path
        .segments
        .last()
        .map(|segment| segment.ident.to_string())
}

pub(super) fn is_public(visibility: &Visibility) -> bool {
    matches!(visibility, Visibility::Public(_))
}

pub(super) fn is_visible(visibility: &Visibility) -> bool {
    !matches!(visibility, Visibility::Inherited)
}

#[cfg(test)]
pub(in crate::semantic_construction_boundary) fn fixture_root_surface(
    source: &str,
    root: &str,
) -> RootSurface {
    let parsed = syn::parse_file(source).expect("parse root-surface mutation fixture");
    let mut surface = RootSurface::default();
    for item in &parsed.items {
        match item {
            Item::Struct(structure) if structure.ident == root => {
                collect_struct_surface(structure, &mut surface);
            }
            Item::Impl(implementation)
                if simple_type_name(&implementation.self_ty).as_deref() == Some(root) =>
            {
                collect_impl_surface(implementation, "", root, &mut surface);
            }
            _ => {}
        }
    }
    surface
}

#[cfg(test)]
pub(in crate::semantic_construction_boundary) fn fixture_route_inventory(
    source: &str,
    root: &'static str,
) -> BTreeSet<RouteSignature> {
    let context = ProductionRustSourceContext {
        canonical_path: Path::new("fixture.rs").to_path_buf(),
        crate_aliases: Default::default(),
        module_path: Vec::new(),
        path: Path::new("fixture.rs").to_path_buf(),
        source: source.to_owned(),
        target: "lib".to_owned(),
    };
    let spec = fixture_root_spec(root);
    collect_route_inventory(&[context], &spec)
}

#[cfg(test)]
pub(in crate::semantic_construction_boundary) fn fixture_closed_proof_route_inventory(
    source: &str,
    root: &'static str,
) -> BTreeSet<RouteSignature> {
    let context = ProductionRustSourceContext {
        canonical_path: Path::new("fixture.rs").to_path_buf(),
        crate_aliases: Default::default(),
        module_path: Vec::new(),
        path: Path::new("fixture.rs").to_path_buf(),
        source: source.to_owned(),
        target: "lib".to_owned(),
    };
    let mut spec = fixture_root_spec(root);
    spec.scan_private_routes = true;
    collect_route_inventory(&[context], &spec)
}

#[cfg(test)]
pub(super) fn fixture_root_spec(root: &'static str) -> RootSpec {
    RootSpec {
        crate_name: "fixture",
        declaration_module: "",
        root,
        expected_public_fields: &[],
        expected_derived_traits: &[],
        expected_trait_impls: &[],
        expected_default_authorities: &[],
        deserialization_authorities: 0,
        serde_attributes: 0,
        conversion_constructors: 0,
        deref: 0,
        public_new: false,
        public_validate: false,
        public_mutators: 0,
        deref_mut: false,
        public_result_construct: false,
        expected_public_root_producers: &[],
        expected_public_mutable_projections: &[],
        expected_public_consuming_extractions: &[],
        expected_public_semantic_checks: &[],
        route_overrides: &[],
        expected_route_catalog: RouteCatalogExpectation::ExactOverrides,
        expected_disposition_counts: [0; 3],
        scan_private_routes: false,
        route_catalog_id: "SPEC_0043/ROOT-FIXTURE",
    }
}
