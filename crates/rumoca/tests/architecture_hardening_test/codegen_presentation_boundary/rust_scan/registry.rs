//! Exact inventory of Rust callables exposed to built-in templates.

use super::super::super::architecture_hardening_support::attributes_require_test;
use super::super::content_fingerprint;
use quote::ToTokens;
use std::collections::{BTreeMap, BTreeSet};
use std::path::PathBuf;
use syn::visit::{self, Visit};

#[derive(Clone)]
struct Callable {
    file: String,
    owner: String,
    fingerprint: String,
    exact_modelica_codec_delegate: bool,
}

struct Registration {
    file: String,
    owner: String,
    owner_fingerprint: String,
    kind: String,
    public_name: String,
    value_fingerprint: String,
    callable: Option<String>,
}

struct RegistryInventory {
    callables: BTreeMap<String, Vec<Callable>>,
    registrations: Vec<Registration>,
    environment_policy: EnvironmentPolicy,
    surface_tokens: TemplateSurfaceTokenCounts,
}

fn collect_registry_inventory(sources: &[(PathBuf, String)]) -> RegistryInventory {
    let mut callables = BTreeMap::<String, Vec<Callable>>::new();
    let mut registrations = Vec::new();
    let mut environment_policy = EnvironmentPolicy::default();
    let mut surface_tokens = TemplateSurfaceTokenCounts::default();
    for (path, source) in sources {
        let syntax = syn::parse_file(source)
            .unwrap_or_else(|error| panic!("parse {}: {error}", path.display()));
        surface_tokens += count_template_surface_tokens(&syntax.items);
        collect_items(
            &syntax.items,
            &path.display().to_string(),
            &mut Vec::new(),
            &mut callables,
            &mut registrations,
            &mut environment_policy,
        );
    }
    RegistryInventory {
        callables,
        registrations,
        environment_policy,
        surface_tokens,
    }
}

pub(super) fn analyze_registry(sources: &[(PathBuf, String)]) -> BTreeSet<String> {
    let RegistryInventory {
        callables,
        registrations,
        environment_policy,
        surface_tokens,
    } = collect_registry_inventory(sources);

    let mut findings = BTreeSet::new();
    record_surface_token_findings(&surface_tokens, &registrations, &mut findings);
    record_environment_policy_finding(sources, &surface_tokens, &environment_policy, &mut findings);
    let mut public_names = BTreeMap::<&str, usize>::new();
    for registration in &registrations {
        *public_names.entry(&registration.public_name).or_default() += 1;
    }
    findings.extend(
        public_names
            .into_iter()
            .filter(|(_, count)| *count > 1)
            .map(|(name, count)| {
                format!(
                    "<registry>::<environment>:template-registry-duplicate-public-name:{name}:{count}"
                )
            }),
    );
    findings.extend(
        registrations
            .iter()
            .map(|registration| registration_identity(registration, &callables)),
    );
    findings
}

pub(super) fn analyze_registry_usage(
    sources: &[(PathBuf, String)],
    templates: &[(String, String)],
) -> BTreeSet<String> {
    let inventory = collect_registry_inventory(sources);
    inventory
        .registrations
        .iter()
        .filter(|registration| matches!(registration.kind.as_str(), "filter" | "function"))
        .filter(|registration| {
            registry_disposition(
                registration,
                resolve_callable(registration, &inventory.callables),
            ) != "diagnostic-only"
        })
        .filter(|registration| {
            !templates.iter().any(|(_, source)| {
                super::super::jinja_scan::template_uses_registered_command(
                    source,
                    &registration.kind,
                    &registration.public_name,
                )
                .unwrap_or(false)
            })
        })
        .map(|registration| {
            format!(
                "<registry>::<templates>:template-registry-unused-public-command:{}:{}",
                registration.kind, registration.public_name
            )
        })
        .collect()
}

fn record_surface_token_findings(
    surface_tokens: &TemplateSurfaceTokenCounts,
    registrations: &[Registration],
    findings: &mut BTreeSet<String>,
) {
    if surface_tokens.registrations != registrations.len() {
        findings.insert(format!(
            "<registry>::<environment>:template-registry-unresolved-token-count:{}:inventoried-{}",
            surface_tokens.registrations,
            registrations.len()
        ));
    }
    let inventoried_callable_constructors = registrations
        .iter()
        .filter(|registration| registration.kind == "global" && registration.callable.is_some())
        .count();
    if surface_tokens.callable_constructors != inventoried_callable_constructors {
        findings.insert(format!(
            "<registry>::<environment>:template-callable-constructor-unresolved-token-count:{}:inventoried-{inventoried_callable_constructors}",
            surface_tokens.callable_constructors
        ));
    }
    if surface_tokens.object_call_methods != 0 {
        findings.insert(format!(
            "<registry>::<environment>:template-object-call-method-token-count:{}",
            surface_tokens.object_call_methods
        ));
    }
    if surface_tokens.object_calls != 0 {
        findings.insert(format!(
            "<registry>::<environment>:template-object-call-token-count:{}",
            surface_tokens.object_calls
        ));
    }
    if surface_tokens.object_trait_aliases != 0 {
        findings.insert(format!(
            "<registry>::<environment>:template-object-trait-alias-token-count:{}",
            surface_tokens.object_trait_aliases
        ));
    }
    if surface_tokens.environment_aliases != 0 {
        findings.insert(format!(
            "<registry>::<environment>:template-environment-alias-token-count:{}",
            surface_tokens.environment_aliases
        ));
    }
    if surface_tokens.environment_type_aliases != 0 {
        findings.insert(format!(
            "<registry>::<environment>:template-environment-type-alias-token-count:{}",
            surface_tokens.environment_type_aliases
        ));
    }
    if surface_tokens.unknown_method_callbacks != 0 {
        findings.insert(format!(
            "<registry>::<environment>:template-unknown-method-callback-token-count:{}",
            surface_tokens.unknown_method_callbacks
        ));
    }
    if surface_tokens.custom_formatters != 0 {
        findings.insert(format!(
            "<registry>::<environment>:template-custom-formatter-token-count:{}",
            surface_tokens.custom_formatters
        ));
    }
    if surface_tokens.ambiguous_default_constructors != 0 {
        findings.insert(format!(
            "<registry>::<environment>:template-ambiguous-default-constructor-token-count:{}",
            surface_tokens.ambiguous_default_constructors
        ));
    }
}

fn record_environment_policy_finding(
    sources: &[(PathBuf, String)],
    surface_tokens: &TemplateSurfaceTokenCounts,
    environment_policy: &EnvironmentPolicy,
    findings: &mut BTreeSet<String>,
) {
    let expected_environment_owner =
        "crates/rumoca-phase-codegen/src/codegen/mod.rs::target_template_environment";
    let environment_policy_in_scope = sources.iter().any(|(path, _)| {
        path == std::path::Path::new("crates/rumoca-phase-codegen/src/codegen/mod.rs")
    }) || surface_tokens.environment_identifiers != 0
        || surface_tokens.environment_constructions != 0
        || surface_tokens.alternative_environment_constructions != 0
        || surface_tokens.strict_policies != 0
        || surface_tokens.undefined_behavior_setters != 0;
    if environment_policy_in_scope
        && (surface_tokens.environment_constructions != 1
            || surface_tokens.alternative_environment_constructions != 0
            || surface_tokens.strict_policies != 1
            || surface_tokens.environment_identifiers != 5
            || surface_tokens.undefined_behavior_setters != 1
            || environment_policy.constructors
                != BTreeSet::from([expected_environment_owner.to_string()])
            || environment_policy.strict_policies
                != BTreeSet::from([expected_environment_owner.to_string()]))
    {
        findings.insert(format!(
            "<registry>::<environment>:template-environment-policy:new-tokens-{}:alternative-constructor-tokens-{}:strict-tokens-{}:environment-identifiers-{}:undefined-setters-{}:constructor-owners-{:?}:strict-owners-{:?}",
            surface_tokens.environment_constructions,
            surface_tokens.alternative_environment_constructions,
            surface_tokens.strict_policies,
            surface_tokens.environment_identifiers,
            surface_tokens.undefined_behavior_setters,
            environment_policy.constructors,
            environment_policy.strict_policies
        ));
    }
}

#[derive(Default)]
struct TemplateSurfaceTokenCounts {
    registrations: usize,
    callable_constructors: usize,
    object_call_methods: usize,
    object_calls: usize,
    object_trait_aliases: usize,
    environment_aliases: usize,
    environment_type_aliases: usize,
    unknown_method_callbacks: usize,
    custom_formatters: usize,
    environment_constructions: usize,
    alternative_environment_constructions: usize,
    ambiguous_default_constructors: usize,
    strict_policies: usize,
    environment_identifiers: usize,
    undefined_behavior_setters: usize,
}

#[derive(Default)]
struct EnvironmentPolicy {
    constructors: BTreeSet<String>,
    strict_policies: BTreeSet<String>,
}

impl std::ops::AddAssign for TemplateSurfaceTokenCounts {
    fn add_assign(&mut self, rhs: Self) {
        self.registrations += rhs.registrations;
        self.callable_constructors += rhs.callable_constructors;
        self.object_call_methods += rhs.object_call_methods;
        self.object_calls += rhs.object_calls;
        self.object_trait_aliases += rhs.object_trait_aliases;
        self.environment_aliases += rhs.environment_aliases;
        self.environment_type_aliases += rhs.environment_type_aliases;
        self.unknown_method_callbacks += rhs.unknown_method_callbacks;
        self.custom_formatters += rhs.custom_formatters;
        self.environment_constructions += rhs.environment_constructions;
        self.alternative_environment_constructions += rhs.alternative_environment_constructions;
        self.ambiguous_default_constructors += rhs.ambiguous_default_constructors;
        self.strict_policies += rhs.strict_policies;
        self.environment_identifiers += rhs.environment_identifiers;
        self.undefined_behavior_setters += rhs.undefined_behavior_setters;
    }
}

fn count_template_surface_tokens(items: &[syn::Item]) -> TemplateSurfaceTokenCounts {
    let mut counts = count_template_surface_raw_tokens(items);
    let structural = count_structural_template_surfaces(items);
    counts.object_calls = structural.object_calls;
    counts.object_call_methods = structural.object_call_methods;
    counts.object_trait_aliases = structural.object_trait_aliases;
    counts.environment_aliases = structural.environment_aliases;
    counts.environment_type_aliases = structural.environment_type_aliases;
    counts
}

fn count_template_surface_raw_tokens(items: &[syn::Item]) -> TemplateSurfaceTokenCounts {
    items
        .iter()
        .filter(|item| !attributes_require_test(item_attributes(item)))
        .map(|item| match item {
            syn::Item::Mod(module) if module.content.is_some() => {
                count_template_surface_raw_tokens(
                    &module.content.as_ref().expect("guarded content").1,
                )
            }
            syn::Item::Impl(implementation) => implementation
                .items
                .iter()
                .filter(|item| !attributes_require_test(impl_item_attributes(item)))
                .map(|item| count_template_surface_idents(item.to_token_stream()))
                .fold(
                    TemplateSurfaceTokenCounts::default(),
                    |mut total, counts| {
                        total += counts;
                        total
                    },
                ),
            syn::Item::Trait(item_trait) => item_trait
                .items
                .iter()
                .filter(|item| !attributes_require_test(trait_item_attributes(item)))
                .map(|item| count_template_surface_idents(item.to_token_stream()))
                .fold(
                    TemplateSurfaceTokenCounts::default(),
                    |mut total, counts| {
                        total += counts;
                        total
                    },
                ),
            other => count_template_surface_idents(other.to_token_stream()),
        })
        .fold(
            TemplateSurfaceTokenCounts::default(),
            |mut total, counts| {
                total += counts;
                total
            },
        )
}

fn count_structural_template_surfaces(items: &[syn::Item]) -> TemplateSurfaceTokenCounts {
    struct SurfaceVisitor {
        counts: TemplateSurfaceTokenCounts,
    }

    impl<'ast> Visit<'ast> for SurfaceVisitor {
        fn visit_item(&mut self, item: &'ast syn::Item) {
            if attributes_require_test(item_attributes(item)) {
                return;
            }
            visit::visit_item(self, item);
        }

        fn visit_impl_item(&mut self, item: &'ast syn::ImplItem) {
            if attributes_require_test(impl_item_attributes(item)) {
                return;
            }
            visit::visit_impl_item(self, item);
        }

        fn visit_trait_item(&mut self, item: &'ast syn::TraitItem) {
            if attributes_require_test(trait_item_attributes(item)) {
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

        fn visit_item_impl(&mut self, implementation: &'ast syn::ItemImpl) {
            let (calls, call_methods) = object_call_counts(implementation);
            self.counts.object_calls += calls;
            self.counts.object_call_methods += call_methods;
            visit::visit_item_impl(self, implementation);
        }

        fn visit_item_use(&mut self, item: &'ast syn::ItemUse) {
            self.counts.object_trait_aliases +=
                count_minijinja_aliases(&item.tree, false, "Object");
            self.counts.environment_aliases +=
                count_minijinja_aliases(&item.tree, false, "Environment");
            visit::visit_item_use(self, item);
        }

        fn visit_item_type(&mut self, item: &'ast syn::ItemType) {
            self.counts.environment_type_aliases +=
                usize::from(type_mentions_path_name(&item.ty, "Environment"));
            visit::visit_item_type(self, item);
        }

        fn visit_impl_item_type(&mut self, item: &'ast syn::ImplItemType) {
            self.counts.environment_type_aliases +=
                usize::from(type_mentions_path_name(&item.ty, "Environment"));
            visit::visit_impl_item_type(self, item);
        }

        fn visit_trait_item_type(&mut self, item: &'ast syn::TraitItemType) {
            if let Some((_, ty)) = &item.default {
                self.counts.environment_type_aliases +=
                    usize::from(type_mentions_path_name(ty, "Environment"));
            }
            visit::visit_trait_item_type(self, item);
        }
    }

    let mut visitor = SurfaceVisitor {
        counts: TemplateSurfaceTokenCounts::default(),
    };
    for item in items {
        visitor.visit_item(item);
    }
    visitor.counts
}

fn type_mentions_path_name(ty: &syn::Type, expected: &str) -> bool {
    struct PathNameVisitor<'a> {
        expected: &'a str,
        found: bool,
    }

    impl<'ast> Visit<'ast> for PathNameVisitor<'_> {
        fn visit_path_segment(&mut self, segment: &'ast syn::PathSegment) {
            self.found |= segment.ident == self.expected;
            visit::visit_path_segment(self, segment);
        }
    }

    let mut visitor = PathNameVisitor {
        expected,
        found: false,
    };
    visitor.visit_type(ty);
    visitor.found
}

fn count_template_surface_idents(tokens: proc_macro2::TokenStream) -> TemplateSurfaceTokenCounts {
    let tokens = tokens.into_iter().collect::<Vec<_>>();
    let mut counts = TemplateSurfaceTokenCounts {
        environment_constructions: count_path_pair(&tokens, "Environment", "new"),
        alternative_environment_constructions: count_path_pair(&tokens, "Environment", "empty")
            + count_path_pair(&tokens, "Environment", "default"),
        ambiguous_default_constructors: count_path_pair(&tokens, "Default", "default"),
        strict_policies: count_path_pair(&tokens, "UndefinedBehavior", "Strict"),
        ..TemplateSurfaceTokenCounts::default()
    };
    for token in tokens {
        match token {
            proc_macro2::TokenTree::Ident(ident) => match ident.to_string().as_str() {
                method if registration_kind(method).is_some() => counts.registrations += 1,
                "from_function" | "from_safe_function" => counts.callable_constructors += 1,
                "set_unknown_method_callback" => counts.unknown_method_callbacks += 1,
                "set_formatter" => counts.custom_formatters += 1,
                "Environment" => counts.environment_identifiers += 1,
                "set_undefined_behavior" => counts.undefined_behavior_setters += 1,
                _ => {}
            },
            proc_macro2::TokenTree::Group(group) => {
                counts += count_template_surface_idents(group.stream());
            }
            _ => {}
        }
    }
    counts
}

fn count_path_pair(tokens: &[proc_macro2::TokenTree], owner: &str, member: &str) -> usize {
    tokens
        .windows(4)
        .filter(|window| {
            matches!(&window[0], proc_macro2::TokenTree::Ident(ident) if ident == owner)
                && matches!(&window[1], proc_macro2::TokenTree::Punct(punct) if punct.as_char() == ':')
                && matches!(&window[2], proc_macro2::TokenTree::Punct(punct) if punct.as_char() == ':')
                && matches!(&window[3], proc_macro2::TokenTree::Ident(ident) if ident == member)
        })
        .count()
}

fn object_call_counts(implementation: &syn::ItemImpl) -> (usize, usize) {
    if !is_object_impl(implementation) {
        return (0, 0);
    }
    implementation
        .items
        .iter()
        .filter_map(|item| match item {
            syn::ImplItem::Fn(method) if !attributes_require_test(&method.attrs) => Some(method),
            _ => None,
        })
        .fold((0, 0), |(calls, call_methods), method| {
            (
                calls + usize::from(method.sig.ident == "call"),
                call_methods + usize::from(method.sig.ident == "call_method"),
            )
        })
}

fn count_minijinja_aliases(
    tree: &syn::UseTree,
    under_minijinja: bool,
    imported_name: &str,
) -> usize {
    match tree {
        syn::UseTree::Path(path) => count_minijinja_aliases(
            path.tree.as_ref(),
            under_minijinja || path.ident == "minijinja",
            imported_name,
        ),
        syn::UseTree::Rename(rename)
            if under_minijinja
                && rename.ident == imported_name
                && rename.rename != imported_name =>
        {
            1
        }
        syn::UseTree::Group(group) => group
            .items
            .iter()
            .map(|tree| count_minijinja_aliases(tree, under_minijinja, imported_name))
            .sum(),
        _ => 0,
    }
}

fn is_object_impl(implementation: &syn::ItemImpl) -> bool {
    implementation
        .trait_
        .as_ref()
        .and_then(|(_, path, _)| path.segments.last())
        .is_some_and(|part| part.ident == "Object")
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
        syn::Item::Verbatim(_) => &[],
        _ => &[],
    }
}

fn impl_item_attributes(item: &syn::ImplItem) -> &[syn::Attribute] {
    match item {
        syn::ImplItem::Const(item) => &item.attrs,
        syn::ImplItem::Fn(item) => &item.attrs,
        syn::ImplItem::Type(item) => &item.attrs,
        syn::ImplItem::Macro(item) => &item.attrs,
        syn::ImplItem::Verbatim(_) => &[],
        _ => &[],
    }
}

fn trait_item_attributes(item: &syn::TraitItem) -> &[syn::Attribute] {
    match item {
        syn::TraitItem::Const(item) => &item.attrs,
        syn::TraitItem::Fn(item) => &item.attrs,
        syn::TraitItem::Type(item) => &item.attrs,
        syn::TraitItem::Macro(item) => &item.attrs,
        syn::TraitItem::Verbatim(_) => &[],
        _ => &[],
    }
}

fn collect_items(
    items: &[syn::Item],
    file: &str,
    module: &mut Vec<String>,
    callables: &mut BTreeMap<String, Vec<Callable>>,
    registrations: &mut Vec<Registration>,
    environment_policy: &mut EnvironmentPolicy,
) {
    for item in items {
        match item {
            syn::Item::Fn(function) if !attributes_require_test(&function.attrs) => {
                let owner = owner_name(module, &function.sig.ident.to_string());
                record_callable(file, &owner, &function.sig, &function.block, callables);
                collect_registrations(
                    file,
                    &owner,
                    &function.sig,
                    &function.block,
                    registrations,
                    environment_policy,
                );
            }
            syn::Item::Impl(implementation) if !attributes_require_test(&implementation.attrs) => {
                collect_impl_items(
                    implementation,
                    file,
                    module,
                    callables,
                    registrations,
                    environment_policy,
                );
            }
            syn::Item::Trait(item_trait) if !attributes_require_test(&item_trait.attrs) => {
                collect_trait_items(
                    item_trait,
                    file,
                    module,
                    callables,
                    registrations,
                    environment_policy,
                );
            }
            syn::Item::Mod(item_module) if !attributes_require_test(&item_module.attrs) => {
                if let Some((_, nested)) = &item_module.content {
                    module.push(item_module.ident.to_string());
                    collect_items(
                        nested,
                        file,
                        module,
                        callables,
                        registrations,
                        environment_policy,
                    );
                    module.pop();
                }
            }
            _ => {}
        }
    }
}

fn collect_impl_items(
    implementation: &syn::ItemImpl,
    file: &str,
    module: &mut Vec<String>,
    callables: &mut BTreeMap<String, Vec<Callable>>,
    registrations: &mut Vec<Registration>,
    environment_policy: &mut EnvironmentPolicy,
) {
    module.push(type_name(&implementation.self_ty));
    for item in &implementation.items {
        if let syn::ImplItem::Fn(method) = item
            && !attributes_require_test(&method.attrs)
        {
            let owner = owner_name(module, &method.sig.ident.to_string());
            record_callable(file, &owner, &method.sig, &method.block, callables);
            collect_registrations(
                file,
                &owner,
                &method.sig,
                &method.block,
                registrations,
                environment_policy,
            );
        }
    }
    module.pop();
}

fn collect_trait_items(
    item_trait: &syn::ItemTrait,
    file: &str,
    module: &mut Vec<String>,
    callables: &mut BTreeMap<String, Vec<Callable>>,
    registrations: &mut Vec<Registration>,
    environment_policy: &mut EnvironmentPolicy,
) {
    module.push(item_trait.ident.to_string());
    for item in &item_trait.items {
        if let syn::TraitItem::Fn(method) = item
            && !attributes_require_test(&method.attrs)
            && let Some(body) = &method.default
        {
            let owner = owner_name(module, &method.sig.ident.to_string());
            record_callable(file, &owner, &method.sig, body, callables);
            collect_registrations(
                file,
                &owner,
                &method.sig,
                body,
                registrations,
                environment_policy,
            );
        }
    }
    module.pop();
}

fn record_callable(
    file: &str,
    owner: &str,
    signature: &syn::Signature,
    body: &syn::Block,
    callables: &mut BTreeMap<String, Vec<Callable>>,
) {
    let callable = Callable {
        file: file.to_string(),
        owner: owner.to_string(),
        exact_modelica_codec_delegate: super::is_exact_modelica_codec_delegate(
            file, owner, signature, body,
        ),
        fingerprint: content_fingerprint(&format!(
            "{} {}",
            signature.to_token_stream(),
            body.to_token_stream()
        )),
    };
    callables
        .entry(signature.ident.to_string())
        .or_default()
        .push(callable);
}

fn collect_registrations(
    file: &str,
    owner: &str,
    signature: &syn::Signature,
    body: &syn::Block,
    registrations: &mut Vec<Registration>,
    environment_policy: &mut EnvironmentPolicy,
) {
    let owner_fingerprint = content_fingerprint(&format!(
        "{} {}",
        signature.to_token_stream(),
        body.to_token_stream()
    ));
    let mut visitor = RegistrationVisitor {
        file,
        owner,
        owner_fingerprint: &owner_fingerprint,
        registrations,
    };
    visitor.visit_block(body);
    collect_environment_policy(file, owner, body, environment_policy);
}

fn collect_environment_policy(
    file: &str,
    owner: &str,
    body: &syn::Block,
    environment_policy: &mut EnvironmentPolicy,
) {
    let mut visitor = EnvironmentPolicyVisitor {
        owner: format!("{file}::{owner}"),
        environment_policy,
    };
    visitor.visit_block(body);
}

struct EnvironmentPolicyVisitor<'a> {
    owner: String,
    environment_policy: &'a mut EnvironmentPolicy,
}

impl<'ast> Visit<'ast> for EnvironmentPolicyVisitor<'_> {
    fn visit_expr_call(&mut self, expression: &'ast syn::ExprCall) {
        if path_has_terminal_pair(expression.func.as_ref(), "Environment", "new") {
            self.environment_policy
                .constructors
                .insert(self.owner.clone());
        }
        visit::visit_expr_call(self, expression);
    }

    fn visit_expr_method_call(&mut self, expression: &'ast syn::ExprMethodCall) {
        if expression.method == "set_undefined_behavior"
            && expression.args.len() == 1
            && expression.args.first().is_some_and(|argument| {
                path_has_terminal_pair(argument, "UndefinedBehavior", "Strict")
            })
        {
            self.environment_policy
                .strict_policies
                .insert(self.owner.clone());
        }
        visit::visit_expr_method_call(self, expression);
    }
}

fn path_has_terminal_pair(expression: &syn::Expr, owner: &str, member: &str) -> bool {
    let syn::Expr::Path(path) = expression else {
        return false;
    };
    let mut segments = path.path.segments.iter().rev();
    segments.next().is_some_and(|part| part.ident == member)
        && segments.next().is_some_and(|part| part.ident == owner)
}

struct RegistrationVisitor<'a> {
    file: &'a str,
    owner: &'a str,
    owner_fingerprint: &'a str,
    registrations: &'a mut Vec<Registration>,
}

impl<'ast> Visit<'ast> for RegistrationVisitor<'_> {
    fn visit_expr_method_call(&mut self, call: &'ast syn::ExprMethodCall) {
        if let Some(kind) = registration_kind(&call.method.to_string()) {
            self.record(
                kind,
                call.args.first().and_then(string_literal),
                call.args.iter().nth(1),
            );
        }
        visit::visit_expr_method_call(self, call);
    }

    fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
        if let syn::Expr::Path(function) = call.func.as_ref()
            && let Some(method) = function.path.segments.last()
            && let Some(kind) = registration_kind(&method.ident.to_string())
        {
            self.record(
                kind,
                call.args.iter().nth(1).and_then(string_literal),
                call.args.iter().nth(2),
            );
        }
        visit::visit_expr_call(self, call);
    }
}

impl RegistrationVisitor<'_> {
    fn record(&mut self, kind: &str, public_name: Option<String>, value: Option<&syn::Expr>) {
        if let (Some(public_name), Some(value)) = (public_name, value) {
            self.registrations.push(Registration {
                file: self.file.to_string(),
                owner: self.owner.to_string(),
                owner_fingerprint: self.owner_fingerprint.to_string(),
                kind: kind.to_string(),
                public_name,
                value_fingerprint: content_fingerprint(&value.to_token_stream().to_string()),
                callable: if kind == "global" {
                    registered_global_callable(value)
                } else {
                    callable_path(value)
                },
            });
        } else {
            self.registrations.push(Registration {
                file: self.file.to_string(),
                owner: self.owner.to_string(),
                owner_fingerprint: self.owner_fingerprint.to_string(),
                kind: kind.to_string(),
                public_name: "<non-literal>".to_string(),
                value_fingerprint: "<missing-value>".to_string(),
                callable: None,
            });
        }
    }
}

fn registration_kind(method: &str) -> Option<&'static str> {
    match method {
        "add_filter" => Some("filter"),
        "add_function" => Some("function"),
        "add_test" => Some("test"),
        "add_global" => Some("global"),
        _ => None,
    }
}

fn string_literal(expression: &syn::Expr) -> Option<String> {
    let syn::Expr::Lit(literal) = expression else {
        return None;
    };
    let syn::Lit::Str(value) = &literal.lit else {
        return None;
    };
    Some(value.value())
}

fn callable_path(expression: &syn::Expr) -> Option<String> {
    let syn::Expr::Path(path) = expression else {
        return None;
    };
    Some(
        path.path
            .segments
            .iter()
            .map(|segment| segment.ident.to_string())
            .collect::<Vec<_>>()
            .join("::"),
    )
}

fn registered_global_callable(expression: &syn::Expr) -> Option<String> {
    let syn::Expr::Call(call) = expression else {
        return None;
    };
    let syn::Expr::Path(constructor) = call.func.as_ref() else {
        return None;
    };
    let callable_constructor = constructor.path.segments.last().is_some_and(|segment| {
        matches!(
            segment.ident.to_string().as_str(),
            "from_function" | "from_safe_function"
        )
    });
    callable_constructor
        .then(|| call.args.first().and_then(callable_path))
        .flatten()
}

fn registration_identity(
    registration: &Registration,
    callables: &BTreeMap<String, Vec<Callable>>,
) -> String {
    let resolved_callable = resolve_callable(registration, callables);
    let resolution = if let Some(callable) = resolved_callable {
        format!(
            "{}::{}:{}",
            callable.file, callable.owner, callable.fingerprint
        )
    } else if let Some(callable_path) = &registration.callable {
        format!("<unresolved-or-ambiguous:{callable_path}>")
    } else {
        "<data-expression>".to_string()
    };
    format!(
        "{}::{}:template-registry:{}:{}=>{}:owner-body-{}:expression-{}:disposition-{}",
        registration.file,
        registration.owner,
        registration.kind,
        registration.public_name,
        resolution,
        registration.owner_fingerprint,
        registration.value_fingerprint,
        registry_disposition(registration, resolved_callable)
    )
}

fn resolve_callable<'a>(
    registration: &Registration,
    callables: &'a BTreeMap<String, Vec<Callable>>,
) -> Option<&'a Callable> {
    let callable_path = registration.callable.as_ref()?;
    let leaf = callable_path.rsplit("::").next().unwrap_or(callable_path);
    let candidates = callables.get(leaf).map(Vec::as_slice).unwrap_or(&[]);
    (candidates.len() == 1).then(|| &candidates[0])
}

fn registry_disposition(registration: &Registration, callable: Option<&Callable>) -> &'static str {
    if registration.kind == "global"
        && registration.public_name == "galec_kernels_version"
        && registration.file == "crates/rumoca-phase-codegen/src/codegen/mod.rs"
        && registration.owner == "target_template_environment"
        && callable.is_none()
    {
        return "reviewed-target-neutral-data";
    }
    let Some(callable) = callable else {
        return "unclassified";
    };
    let Some((registration_file, registration_owner, callable_file, callable_owner)) =
        expected_callable_owner(&registration.kind, &registration.public_name)
    else {
        return "unclassified";
    };
    if registration.file != registration_file
        || registration.owner != registration_owner
        || callable.file != callable_file
        || callable.owner != callable_owner
    {
        return "unclassified";
    }
    if registration.kind == "filter"
        && registration.public_name == "modelica_string_escape"
        && callable.exact_modelica_codec_delegate
    {
        "reviewed-target-neutral-lexical-codec"
    } else if registration.kind == "function"
        && matches!(registration.public_name.as_str(), "fail" | "fail_at")
    {
        // These exact, owner-checked commands only abort rendering with a
        // diagnostic; a product template need not exercise every refusal path.
        "diagnostic-only"
    } else {
        "reviewed-non-neutral-debt"
    }
}

fn expected_callable_owner(
    kind: &str,
    public_name: &str,
) -> Option<(&'static str, &'static str, &'static str, &'static str)> {
    const MOD: &str = "crates/rumoca-phase-codegen/src/codegen/mod.rs";
    const DIAGNOSTICS: &str = "crates/rumoca-phase-codegen/src/codegen/dae_diagnostics.rs";
    const DENSE: &str =
        "crates/rumoca-phase-codegen/src/codegen/render_solve/dense_solve_render.rs";
    const PARTITION: &str =
        "crates/rumoca-phase-codegen/src/codegen/render_solve/template_partition.rs";
    const MLIR: &str = "crates/rumoca-phase-codegen/src/codegen/render_solve/mlir_family.rs";
    let owner = match (kind, public_name) {
        ("filter", "sanitize") => (MOD, "sanitize_filter"),
        ("filter", "modelica_string_escape") => (MOD, "modelica_string_escape_filter"),
        ("filter", "product") => (MOD, "product_filter"),
        ("filter", "last_segment") => (MOD, "last_segment_filter"),
        ("filter", "xml_escape") => (MOD, "xml_escape_filter"),
        ("filter", "xs_double") => (MOD, "xs_double_filter"),
        ("function", "source_ref") => (MOD, "source_ref_function"),
        ("function", "render_solve_row_output_wgsl") => {
            (DENSE, "render_solve_row_output_wgsl_function")
        }
        ("function", "render_solve_native_family_wgsl") => {
            (PARTITION, "render_solve_native_family_wgsl_function")
        }
        ("function", "render_solve_native_family_mlir") => {
            (MLIR, "render_solve_native_family_mlir_function")
        }
        ("function", "render_solve_native_family_output_index_wgsl") => (
            PARTITION,
            "render_solve_native_family_output_index_wgsl_function",
        ),
        ("function", "render_solve_native_family_output_map_start") => (
            PARTITION,
            "render_solve_native_family_output_map_start_function",
        ),
        ("function", "wgsl_kernel_schedule_json") => {
            (PARTITION, "render_wgsl_kernel_schedule_json_function")
        }
        ("function", "wgsl_kernel_workgroup_total") => {
            (PARTITION, "render_wgsl_kernel_workgroup_total_function")
        }
        ("function", "wgsl_native_family_inventory_json") => (
            PARTITION,
            "render_wgsl_native_family_inventory_json_function",
        ),
        ("function", "render_matmul_mlir") => (DENSE, "render_matmul_mlir_function"),
        ("function", "render_linsolve_mlir") => (DENSE, "render_linsolve_mlir_function"),
        ("function", "fail") => (MOD, "fail_function"),
        ("function", "fail_at") => (DIAGNOSTICS, "fail_at_function"),
        _ => return None,
    };
    let registration = if public_name == "fail_at" {
        (DIAGNOSTICS, "register")
    } else {
        (MOD, "target_template_environment")
    };
    Some((registration.0, registration.1, owner.0, owner.1))
}

fn owner_name(module: &[String], item: &str) -> String {
    module
        .iter()
        .map(String::as_str)
        .chain(std::iter::once(item))
        .collect::<Vec<_>>()
        .join("::")
}

fn type_name(ty: &syn::Type) -> String {
    if let syn::Type::Path(path) = ty
        && let Some(segment) = path.path.segments.last()
    {
        return segment.ident.to_string();
    }
    "<impl>".to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn known_filter_source() -> Vec<(PathBuf, String)> {
        vec![(
            PathBuf::from("crates/rumoca-phase-codegen/src/codegen/mod.rs"),
            r#"
                fn sanitize_filter(value: String) -> String { value }
                fn target_template_environment() {
                    let mut env = Environment::new();
                    env.add_filter("sanitize", sanitize_filter);
                }
            "#
            .to_string(),
        )]
    }

    #[test]
    fn unused_public_registration_is_absolute_and_only_real_template_use_discharge_it() {
        let sources = known_filter_source();
        let non_uses = vec![(
            "templates/probe.jinja".to_string(),
            r#"{# {{ value | sanitize }} #}{% raw %}{{ value | sanitize }}{% endraw %}{{ "|sanitize" }}{{ value | SANITIZE }}{{ value | sanitize.extra }}"#
                .to_string(),
        )];
        let findings = analyze_registry_usage(&sources, &non_uses);
        let expected =
            "<registry>::<templates>:template-registry-unused-public-command:filter:sanitize";
        assert!(findings.contains(expected));
        assert!(
            super::super::super::absolute_registry_findings(&findings).contains(&expected),
            "an unused public registration must never be ledger-admittable"
        );

        let uses = vec![(
            "templates/probe.jinja".to_string(),
            "{{ model_name | sanitize }}".to_string(),
        )];
        assert!(analyze_registry_usage(&sources, &uses).is_empty());
    }

    #[test]
    fn renamed_reintroduced_callable_remains_an_unused_public_registration() {
        let sources = vec![(
            PathBuf::from("crates/rumoca-phase-codegen/src/codegen/mod.rs"),
            r#"
                fn render_solve_row_wgsl_function() -> String { String::new() }
                fn target_template_environment() {
                    let mut env = Environment::new();
                    env.add_function(
                        "renamed_solve_row",
                        render_solve_row_wgsl_function,
                    );
                }
            "#
            .to_string(),
        )];
        let non_uses = vec![(
            "templates/probe.jinja".to_string(),
            r#"{{ RENAMED_SOLVE_ROW() }}{% set (renamed_solve_row, other) = callables %}{{ renamed_solve_row() }}"#
                .to_string(),
        )];
        let findings = analyze_registry_usage(&sources, &non_uses);
        let expected = "<registry>::<templates>:template-registry-unused-public-command:function:renamed_solve_row";
        assert!(findings.contains(expected));
        assert!(
            super::super::super::absolute_registry_findings(&findings).contains(&expected),
            "renaming a reintroduced callable must not make the unused registration ledger-admittable"
        );

        let uses = vec![(
            "templates/probe.jinja".to_string(),
            "{% set rendered = renamed_solve_row() %}{{ rendered }}".to_string(),
        )];
        assert!(analyze_registry_usage(&sources, &uses).is_empty());
    }
}
