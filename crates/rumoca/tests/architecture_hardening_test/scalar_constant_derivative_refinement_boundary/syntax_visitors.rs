use std::collections::BTreeMap;

use syn::visit::{self, Visit};
use syn::{Expr, Pat};

use super::C61_RECEIPT;

pub(super) fn strip_wrappers(expression: &Expr) -> &Expr {
    match expression {
        Expr::Try(expression) => strip_wrappers(&expression.expr),
        Expr::Reference(expression) => strip_wrappers(&expression.expr),
        Expr::Paren(expression) => strip_wrappers(&expression.expr),
        Expr::Group(expression) => strip_wrappers(&expression.expr),
        _ => expression,
    }
}

pub(super) fn expression_ident(expression: &Expr) -> Option<String> {
    let Expr::Path(path) = strip_wrappers(expression) else {
        return None;
    };
    path.path.get_ident().map(|ident| ident.to_string())
}

pub(super) fn type_last_name(ty: &syn::Type) -> Option<String> {
    let syn::Type::Path(path) = ty else {
        return None;
    };
    path.path
        .segments
        .last()
        .map(|segment| segment.ident.to_string())
}

pub(super) fn type_contains_name(ty: &syn::Type, target: &str) -> bool {
    struct Finder<'name> {
        target: &'name str,
        found: bool,
    }
    impl<'ast> Visit<'ast> for Finder<'_> {
        fn visit_type_path(&mut self, path: &'ast syn::TypePath) {
            if path
                .path
                .segments
                .iter()
                .any(|segment| segment.ident == self.target)
            {
                self.found = true;
            }
            visit::visit_type_path(self, path);
        }
    }
    let mut finder = Finder {
        target,
        found: false,
    };
    finder.visit_type(ty);
    finder.found
}

pub(super) fn return_contains(output: &syn::ReturnType, target: &str) -> bool {
    matches!(output, syn::ReturnType::Type(_, ty) if type_contains_name(ty, target))
}

pub(super) fn forbidden_derives(attributes: &[syn::Attribute]) -> Vec<String> {
    const FORBIDDEN: [&str; 5] = ["Clone", "Copy", "Default", "Serialize", "Deserialize"];

    fn token_identifiers(stream: proc_macro2::TokenStream, names: &mut Vec<String>) {
        for token in stream {
            match token {
                proc_macro2::TokenTree::Ident(identifier) => names.push(identifier.to_string()),
                proc_macro2::TokenTree::Group(group) => {
                    token_identifiers(group.stream(), names);
                }
                proc_macro2::TokenTree::Punct(_) | proc_macro2::TokenTree::Literal(_) => {}
            }
        }
    }

    let mut forbidden = Vec::new();
    for attribute in attributes {
        if attribute.path().is_ident("derive") {
            forbidden.extend(
                attribute
                    .parse_args_with(
                        syn::punctuated::Punctuated::<syn::Path, syn::Token![,]>::parse_terminated,
                    )
                    .unwrap_or_else(|error| panic!("parse derive: {error}"))
                    .iter()
                    .filter_map(|path| path.segments.last())
                    .map(|segment| segment.ident.to_string())
                    .filter(|name| FORBIDDEN.contains(&name.as_str())),
            );
        } else if attribute.path().is_ident("cfg_attr") {
            let mut identifiers = Vec::new();
            if let syn::Meta::List(meta) = &attribute.meta {
                token_identifiers(meta.tokens.clone(), &mut identifiers);
            }
            if identifiers.iter().any(|name| name == "derive") {
                forbidden.extend(
                    identifiers
                        .into_iter()
                        .filter(|name| FORBIDDEN.contains(&name.as_str())),
                );
            }
        }
    }
    forbidden
}

pub(super) fn macro_identifier_counts(file: &syn::File, name: &str) -> BTreeMap<String, usize> {
    fn collect(stream: proc_macro2::TokenStream, counts: &mut BTreeMap<String, usize>) {
        for token in stream {
            match token {
                proc_macro2::TokenTree::Ident(ident) => {
                    *counts.entry(ident.to_string()).or_default() += 1;
                }
                proc_macro2::TokenTree::Group(group) => collect(group.stream(), counts),
                proc_macro2::TokenTree::Punct(_) | proc_macro2::TokenTree::Literal(_) => {}
            }
        }
    }
    let mut counts = BTreeMap::new();
    if let Some(item) = file.items.iter().find_map(|item| match item {
        syn::Item::Macro(item) if item.ident.as_ref().is_some_and(|ident| ident == name) => {
            Some(item)
        }
        _ => None,
    }) {
        collect(item.mac.tokens.clone(), &mut counts);
    }
    counts
}

fn generic_argument_owns_name(argument: &syn::GenericArgument, target: &str) -> bool {
    matches!(argument, syn::GenericArgument::Type(ty) if type_owns_name(ty, target))
}

fn path_arguments_own_name(arguments: &syn::PathArguments, target: &str) -> bool {
    let syn::PathArguments::AngleBracketed(arguments) = arguments else {
        return false;
    };
    arguments
        .args
        .iter()
        .any(|argument| generic_argument_owns_name(argument, target))
}

fn type_owns_name(ty: &syn::Type, target: &str) -> bool {
    match ty {
        syn::Type::Reference(_) => false,
        syn::Type::Path(path) => path.path.segments.iter().any(|segment| {
            segment.ident == target || path_arguments_own_name(&segment.arguments, target)
        }),
        syn::Type::Tuple(tuple) => tuple.elems.iter().any(|ty| type_owns_name(ty, target)),
        syn::Type::Array(array) => type_owns_name(&array.elem, target),
        syn::Type::Paren(paren) => type_owns_name(&paren.elem, target),
        syn::Type::Group(group) => type_owns_name(&group.elem, target),
        _ => false,
    }
}

pub(super) fn return_owns(output: &syn::ReturnType, target: &str) -> bool {
    matches!(output, syn::ReturnType::Type(_, ty) if type_owns_name(ty, target))
}

pub(super) fn return_owns_resolved(
    output: &syn::ReturnType,
    target: &str,
    aliases: &BTreeMap<String, String>,
) -> bool {
    matches!(output, syn::ReturnType::Type(_, ty)
        if type_owns_resolved(ty, aliases, &[target]))
}

pub(super) fn type_owns_resolved(
    ty: &syn::Type,
    aliases: &BTreeMap<String, String>,
    targets: &[&str],
) -> bool {
    fn path_arguments_own(
        arguments: &syn::PathArguments,
        aliases: &BTreeMap<String, String>,
        targets: &[&str],
    ) -> bool {
        match arguments {
            syn::PathArguments::None => false,
            syn::PathArguments::AngleBracketed(arguments) => {
                arguments.args.iter().any(|argument| match argument {
                    syn::GenericArgument::Type(ty)
                    | syn::GenericArgument::AssocType(syn::AssocType { ty, .. }) => {
                        type_owns_resolved(ty, aliases, targets)
                    }
                    syn::GenericArgument::Constraint(constraint) => constraint
                        .bounds
                        .iter()
                        .any(|bound| type_bound_owns(bound, aliases, targets)),
                    syn::GenericArgument::Lifetime(_)
                    | syn::GenericArgument::Const(_)
                    | syn::GenericArgument::AssocConst(_) => false,
                    _ => false,
                })
            }
            syn::PathArguments::Parenthesized(arguments) => {
                arguments
                    .inputs
                    .iter()
                    .any(|ty| type_owns_resolved(ty, aliases, targets))
                    || matches!(&arguments.output, syn::ReturnType::Type(_, ty)
                        if type_owns_resolved(ty, aliases, targets))
            }
        }
    }

    fn type_bound_owns(
        bound: &syn::TypeParamBound,
        aliases: &BTreeMap<String, String>,
        targets: &[&str],
    ) -> bool {
        let syn::TypeParamBound::Trait(bound) = bound else {
            return false;
        };
        bound.path.segments.iter().any(|segment| {
            aliases
                .get(&segment.ident.to_string())
                .is_some_and(|canonical| targets.contains(&canonical.as_str()))
                || path_arguments_own(&segment.arguments, aliases, targets)
        })
    }

    match ty {
        syn::Type::Reference(_) => false,
        syn::Type::Path(path) => path.path.segments.iter().any(|segment| {
            aliases
                .get(&segment.ident.to_string())
                .is_some_and(|canonical| targets.contains(&canonical.as_str()))
                || path_arguments_own(&segment.arguments, aliases, targets)
        }),
        syn::Type::Tuple(tuple) => tuple
            .elems
            .iter()
            .any(|ty| type_owns_resolved(ty, aliases, targets)),
        syn::Type::Array(array) => type_owns_resolved(&array.elem, aliases, targets),
        syn::Type::Paren(paren) => type_owns_resolved(&paren.elem, aliases, targets),
        syn::Type::Group(group) => type_owns_resolved(&group.elem, aliases, targets),
        syn::Type::Slice(slice) => type_owns_resolved(&slice.elem, aliases, targets),
        syn::Type::Ptr(pointer) => type_owns_resolved(&pointer.elem, aliases, targets),
        syn::Type::BareFn(function) => {
            function
                .inputs
                .iter()
                .any(|argument| type_owns_resolved(&argument.ty, aliases, targets))
                || matches!(&function.output, syn::ReturnType::Type(_, ty)
                    if type_owns_resolved(ty, aliases, targets))
        }
        syn::Type::ImplTrait(bounds) => bounds
            .bounds
            .iter()
            .any(|bound| type_bound_owns(bound, aliases, targets)),
        syn::Type::TraitObject(bounds) => bounds
            .bounds
            .iter()
            .any(|bound| type_bound_owns(bound, aliases, targets)),
        _ => false,
    }
}

pub(super) fn type_contains_resolved_name(
    ty: &syn::Type,
    target: &str,
    aliases: &BTreeMap<String, String>,
) -> bool {
    struct Finder<'names> {
        aliases: &'names BTreeMap<String, String>,
        target: &'names str,
        found: bool,
    }
    impl<'ast> Visit<'ast> for Finder<'_> {
        fn visit_type_path(&mut self, path: &'ast syn::TypePath) {
            self.found |= path.path.segments.iter().any(|segment| {
                self.aliases
                    .get(&segment.ident.to_string())
                    .is_some_and(|canonical| canonical == self.target)
            });
            visit::visit_type_path(self, path);
        }
    }
    let mut finder = Finder {
        aliases,
        target,
        found: false,
    };
    finder.visit_type(ty);
    finder.found
}

fn named_literal_field(field: &syn::FieldValue) -> Option<(String, String)> {
    let syn::Member::Named(member) = &field.member else {
        return None;
    };
    Some((member.to_string(), expression_ident(&field.expr)?))
}

pub(super) fn explicit_literal_fields(
    block: &syn::Block,
    target: &str,
) -> Vec<BTreeMap<String, String>> {
    struct Literals<'name> {
        target: &'name str,
        fields: Vec<BTreeMap<String, String>>,
    }
    impl<'ast> Visit<'ast> for Literals<'_> {
        fn visit_expr_struct(&mut self, expression: &'ast syn::ExprStruct) {
            if expression
                .path
                .segments
                .last()
                .is_some_and(|segment| segment.ident == self.target)
            {
                self.fields.push(
                    expression
                        .fields
                        .iter()
                        .filter_map(named_literal_field)
                        .collect(),
                );
            }
            visit::visit_expr_struct(self, expression);
        }
    }
    let mut literals = Literals {
        target,
        fields: Vec::new(),
    };
    literals.visit_block(block);
    literals.fields
}

pub(super) fn self_literal_count(block: &syn::Block, expected_fields: &[&str]) -> usize {
    explicit_literal_fields(block, "Self")
        .into_iter()
        .filter(|fields| {
            fields.len() == expected_fields.len()
                && expected_fields
                    .iter()
                    .all(|field| fields.get(*field).is_some_and(|value| value == field))
        })
        .count()
}

pub(super) fn explicit_struct_literal_count(block: &syn::Block, target: &str) -> usize {
    struct Counter<'name> {
        target: &'name str,
        count: usize,
    }
    impl<'ast> Visit<'ast> for Counter<'_> {
        fn visit_expr_struct(&mut self, expression: &'ast syn::ExprStruct) {
            if expression
                .path
                .segments
                .last()
                .is_some_and(|segment| segment.ident == self.target)
            {
                self.count += 1;
            }
            visit::visit_expr_struct(self, expression);
        }
    }
    let mut counter = Counter { target, count: 0 };
    counter.visit_block(block);
    counter.count
}

pub(super) fn try_contains_call(block: &syn::Block, target: &str) -> bool {
    struct Finder<'name> {
        target: &'name str,
        found: bool,
    }
    impl<'ast> Visit<'ast> for Finder<'_> {
        fn visit_expr_try(&mut self, expression: &'ast syn::ExprTry) {
            let mut calls = NamedCallCounter {
                target: self.target,
                count: 0,
            };
            calls.visit_expr(&expression.expr);
            self.found |= calls.count > 0;
            visit::visit_expr_try(self, expression);
        }
    }
    let mut finder = Finder {
        target,
        found: false,
    };
    finder.visit_block(block);
    finder.found
}

pub(super) fn gate_event_order(block: &syn::Block, checker: &str) -> Vec<&'static str> {
    struct Events<'name> {
        checker: &'name str,
        events: Vec<&'static str>,
    }
    impl<'ast> Visit<'ast> for Events<'_> {
        fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
            if matches!(call.func.as_ref(), Expr::Path(path)
                if path.path.segments.last().is_some_and(|segment|
                    segment.ident == self.checker))
            {
                self.events.push("check");
            }
            visit::visit_expr_call(self, call);
        }
        fn visit_expr_struct(&mut self, expression: &'ast syn::ExprStruct) {
            if expression
                .path
                .segments
                .last()
                .is_some_and(|segment| segment.ident == C61_RECEIPT)
            {
                self.events.push("mint");
            }
            visit::visit_expr_struct(self, expression);
        }
    }
    let mut events = Events {
        checker,
        events: Vec::new(),
    };
    events.visit_block(block);
    events.events
}

pub(super) fn call_count(block: &syn::Block, target: &str) -> usize {
    let mut visitor = NamedCallCounter { target, count: 0 };
    visitor.visit_block(block);
    visitor.count
}

pub(super) fn method_call_count(block: &syn::Block, target: &str) -> usize {
    struct Counter<'name> {
        target: &'name str,
        count: usize,
    }
    impl<'ast> Visit<'ast> for Counter<'_> {
        fn visit_expr_method_call(&mut self, call: &'ast syn::ExprMethodCall) {
            if call.method == self.target {
                self.count += 1;
            }
            visit::visit_expr_method_call(self, call);
        }
    }
    let mut visitor = Counter { target, count: 0 };
    visitor.visit_block(block);
    visitor.count
}

pub(super) fn file_call_count(file: &syn::File, target: &str) -> usize {
    let mut visitor = NamedCallCounter { target, count: 0 };
    visitor.visit_file(file);
    visitor.count
}

pub(super) fn qualified_file_call_count(file: &syn::File, qualifier: &str, target: &str) -> usize {
    struct Counter<'name> {
        qualifier: &'name str,
        target: &'name str,
        count: usize,
    }
    impl<'ast> Visit<'ast> for Counter<'_> {
        fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
            if matches!(call.func.as_ref(), Expr::Path(path)
                if path.path.segments.last().is_some_and(|segment| segment.ident == self.target)
                    && path.path.segments.iter().rev().nth(1)
                        .is_some_and(|segment| segment.ident == self.qualifier))
            {
                self.count += 1;
            }
            visit::visit_expr_call(self, call);
        }
    }
    let mut visitor = Counter {
        qualifier,
        target,
        count: 0,
    };
    visitor.visit_file(file);
    visitor.count
}

struct NamedCallCounter<'name> {
    target: &'name str,
    count: usize,
}

impl<'ast> Visit<'ast> for NamedCallCounter<'_> {
    fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
        if matches!(call.func.as_ref(), Expr::Path(path)
            if path.path.segments.last().is_some_and(|segment| segment.ident == self.target))
        {
            self.count += 1;
        }
        visit::visit_expr_call(self, call);
    }
}

pub(super) struct LiteralCensus<'aliases> {
    pub(super) counts: BTreeMap<String, usize>,
    aliases: &'aliases BTreeMap<String, String>,
}

impl<'aliases> LiteralCensus<'aliases> {
    pub(super) fn new(
        names: impl IntoIterator<Item = &'static str>,
        aliases: &'aliases BTreeMap<String, String>,
    ) -> Self {
        Self {
            counts: names.into_iter().map(|name| (name.to_owned(), 0)).collect(),
            aliases,
        }
    }
}

impl<'ast> Visit<'ast> for LiteralCensus<'_> {
    fn visit_expr_struct(&mut self, expression: &'ast syn::ExprStruct) {
        if let Some(name) = expression
            .path
            .segments
            .last()
            .map(|segment| segment.ident.to_string())
            && let Some(canonical) = self.aliases.get(&name)
            && let Some(count) = self.counts.get_mut(canonical)
        {
            *count += 1;
        }
        visit::visit_expr_struct(self, expression);
    }
}

struct SelfLiteralCounter {
    count: usize,
}

impl<'ast> Visit<'ast> for SelfLiteralCounter {
    fn visit_expr_struct(&mut self, expression: &'ast syn::ExprStruct) {
        self.count += usize::from(expression.path.is_ident("Self"));
        visit::visit_expr_struct(self, expression);
    }
}

fn self_literal_count_in_impl(item: &syn::ItemImpl) -> usize {
    let mut counter = SelfLiteralCounter { count: 0 };
    counter.visit_item_impl(item);
    counter.count
}

pub(super) fn self_literal_count_in_impls(
    file: &syn::File,
    owner: &str,
    aliases: &BTreeMap<String, String>,
) -> usize {
    file.items
        .iter()
        .filter_map(|item| match item {
            syn::Item::Impl(item)
                if type_last_name(&item.self_ty)
                    .and_then(|name| aliases.get(&name))
                    .is_some_and(|canonical| canonical == owner) =>
            {
                Some(item)
            }
            _ => None,
        })
        .map(self_literal_count_in_impl)
        .sum()
}

pub(super) fn pattern_is_single_tuple_binding(
    pattern: &Pat,
    constructor: &str,
    binding: &str,
) -> bool {
    let Pat::TupleStruct(pattern) = pattern else {
        return false;
    };
    pattern.path.is_ident(constructor)
        && pattern.elems.len() == 1
        && matches!(pattern.elems.first(), Some(Pat::Ident(identifier))
            if identifier.ident == binding
                && identifier.by_ref.is_none()
                && identifier.mutability.is_none()
                && identifier.subpat.is_none())
}

pub(super) fn witness_lowering_initializer_is_direct(expression: &Expr) -> bool {
    let Expr::MethodCall(expect) = strip_wrappers(expression) else {
        return false;
    };
    expect.method == "expect"
        && expect.args.len() == 1
        && matches!(strip_wrappers(&expect.receiver), Expr::Call(call)
            if matches!(call.func.as_ref(), Expr::Path(path)
                if path.path.segments.last().is_some_and(|segment|
                    segment.ident == "lower_correlated_for_simulation_with_overrides")))
}

pub(super) fn witness_receipt_check_is_direct(block: &syn::Block) -> bool {
    let Some(syn::Stmt::Expr(Expr::If(check), None)) = block.stmts.last() else {
        return false;
    };
    if check.else_branch.is_some() || check.then_branch.stmts.len() != 1 {
        return false;
    }
    let Expr::Let(condition) = strip_wrappers(&check.cond) else {
        return false;
    };
    if !pattern_is_single_tuple_binding(&condition.pat, "Err", "unsupported") {
        return false;
    }
    let Expr::MethodCall(receipt) = strip_wrappers(&condition.expr) else {
        return false;
    };
    let fails_on_refusal = matches!(check.then_branch.stmts.first(),
        Some(syn::Stmt::Macro(statement)) if statement.mac.path.is_ident("panic"));
    receipt.method == "scalar_constant_derivative_refinement"
        && receipt.args.is_empty()
        && expression_ident(&receipt.receiver).as_deref() == Some("lowered")
        && fails_on_refusal
}

fn collect_use_imports(
    tree: &syn::UseTree,
    prefix: &mut Vec<String>,
    imports: &mut Vec<(Vec<String>, String)>,
) {
    match tree {
        syn::UseTree::Path(path) => {
            prefix.push(path.ident.to_string());
            collect_use_imports(&path.tree, prefix, imports);
            prefix.pop();
        }
        syn::UseTree::Name(name) => {
            let mut source = prefix.clone();
            source.push(name.ident.to_string());
            imports.push((source, name.ident.to_string()));
        }
        syn::UseTree::Rename(rename) => {
            let mut source = prefix.clone();
            source.push(rename.ident.to_string());
            imports.push((source, rename.rename.to_string()));
        }
        syn::UseTree::Group(group) => {
            for item in &group.items {
                collect_use_imports(item, prefix, imports);
            }
        }
        syn::UseTree::Glob(_) => {}
    }
}

pub(super) fn exact_import_count(file: &syn::File, expected: &[&str]) -> usize {
    let expected = expected
        .iter()
        .map(|segment| (*segment).to_owned())
        .collect::<Vec<_>>();
    file.items
        .iter()
        .filter_map(|item| match item {
            syn::Item::Use(item) => Some(item),
            _ => None,
        })
        .flat_map(|item| {
            let mut imports = Vec::new();
            collect_use_imports(&item.tree, &mut Vec::new(), &mut imports);
            imports
        })
        .filter(|(source, local)| {
            source == &expected && source.last().is_some_and(|name| name == local)
        })
        .count()
}

pub(super) fn use_may_shadow_reviewed_macro(tree: &syn::UseTree) -> bool {
    const REVIEWED_MACROS: [&str; 10] = [
        "format",
        "matches",
        "vec",
        "write",
        "Debug",
        "Clone",
        "Copy",
        "PartialEq",
        "Eq",
        "Error",
    ];
    match tree {
        syn::UseTree::Path(path) => use_may_shadow_reviewed_macro(&path.tree),
        syn::UseTree::Name(name) => REVIEWED_MACROS.contains(&name.ident.to_string().as_str()),
        syn::UseTree::Rename(rename) => {
            REVIEWED_MACROS.contains(&rename.rename.to_string().as_str())
        }
        syn::UseTree::Group(group) => group.items.iter().any(use_may_shadow_reviewed_macro),
        // `syn` cannot resolve which namespaces a glob populates. Rejecting it
        // is the only closed-world proof that an allowlisted macro was not
        // replaced by an opaque imported expansion.
        syn::UseTree::Glob(_) => true,
    }
}

struct Definitions<'name> {
    target: &'name str,
    count: usize,
}

impl Visit<'_> for Definitions<'_> {
    fn visit_pat_ident(&mut self, pattern: &syn::PatIdent) {
        self.count += usize::from(pattern.ident == self.target);
        visit::visit_pat_ident(self, pattern);
    }

    fn visit_item_fn(&mut self, item: &syn::ItemFn) {
        self.count += usize::from(item.sig.ident == self.target);
        visit::visit_item_fn(self, item);
    }

    fn visit_item_const(&mut self, item: &syn::ItemConst) {
        self.count += usize::from(item.ident == self.target);
        visit::visit_item_const(self, item);
    }

    fn visit_item_static(&mut self, item: &syn::ItemStatic) {
        self.count += usize::from(item.ident == self.target);
        visit::visit_item_static(self, item);
    }

    fn visit_item_use(&mut self, item: &syn::ItemUse) {
        let mut imports = Vec::new();
        collect_use_imports(&item.tree, &mut Vec::new(), &mut imports);
        self.count += imports
            .iter()
            .filter(|(_, local)| local == self.target)
            .count();
        visit::visit_item_use(self, item);
    }
}

pub(super) fn block_value_definition_count(block: &syn::Block, target: &str) -> usize {
    let mut definitions = Definitions { target, count: 0 };
    definitions.visit_block(block);
    definitions.count
}

struct Bindings<'name> {
    target: &'name str,
    count: usize,
    invalid: bool,
}

impl<'ast> Visit<'ast> for Bindings<'_> {
    fn visit_pat_ident(&mut self, pattern: &'ast syn::PatIdent) {
        if pattern.ident == self.target {
            self.count += 1;
            self.invalid |= pattern.by_ref.is_some()
                || pattern.mutability.is_some()
                || pattern.subpat.is_some();
        }
        visit::visit_pat_ident(self, pattern);
    }
}

pub(super) fn binding_count(block: &syn::Block, target: &str) -> usize {
    let mut bindings = Bindings {
        target,
        count: 0,
        invalid: false,
    };
    bindings.visit_block(block);
    if bindings.invalid {
        usize::MAX
    } else {
        bindings.count
    }
}

pub(super) fn match_has_variant_without_wildcard(block: &syn::Block, variant: &str) -> bool {
    struct Matches<'name> {
        variant: &'name str,
        found: bool,
        wildcard: bool,
    }
    impl<'ast> Visit<'ast> for Matches<'_> {
        fn visit_expr_match(&mut self, expression: &'ast syn::ExprMatch) {
            self.wildcard |= expression
                .arms
                .iter()
                .any(|arm| matches!(arm.pat, Pat::Wild(_)));
            self.found |= expression.arms.iter().any(|arm| {
                matches!(&arm.pat, Pat::Struct(pattern)
                    if pattern.path.segments.last()
                        .is_some_and(|segment| segment.ident == self.variant))
            });
            visit::visit_expr_match(self, expression);
        }
    }
    let mut matches = Matches {
        variant,
        found: false,
        wildcard: false,
    };
    matches.visit_block(block);
    matches.found && !matches.wildcard
}
