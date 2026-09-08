//! Syntax-aware fail-closed fallback inventory for production codegen items.

use super::super::architecture_hardening_support::attributes_require_test;
use quote::ToTokens;
use std::collections::BTreeSet;
use std::path::Path;
use syn::visit::{self, Visit};

#[derive(Default)]
pub(super) struct CodegenFallbackFindings {
    pub(super) value_undefined: Vec<String>,
    pub(super) optional_empty: Vec<String>,
    pub(super) unwrap_or_default: Vec<String>,
}

pub(super) fn codegen_fallback_findings(path: &Path, source: &str) -> CodegenFallbackFindings {
    let syntax = syn::parse_file(source)
        .unwrap_or_else(|error| panic!("parse production codegen {}: {error}", path.display()));
    let value_names = minijinja_value_names(&syntax);
    let mut visitor = CodegenFallbackVisitor {
        path,
        value_names,
        allow_optional_empty: false,
        findings: CodegenFallbackFindings::default(),
    };
    visitor.visit_file(&syntax);
    visitor.findings
}

struct CodegenFallbackVisitor<'a> {
    path: &'a Path,
    value_names: BTreeSet<String>,
    allow_optional_empty: bool,
    findings: CodegenFallbackFindings,
}

impl CodegenFallbackVisitor<'_> {
    fn location(&self, expression: impl ToTokens) -> String {
        format!("{}: {}", self.path.display(), expression.to_token_stream())
    }
}

impl<'ast> Visit<'ast> for CodegenFallbackVisitor<'_> {
    fn visit_item_fn(&mut self, item: &'ast syn::ItemFn) {
        if attributes_require_test(&item.attrs) {
            return;
        }
        let previous = self.allow_optional_empty;
        self.allow_optional_empty = is_exact_no_output_target(self.path, item);
        visit::visit_item_fn(self, item);
        self.allow_optional_empty = previous;
    }

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

    fn visit_expr_path(&mut self, expression: &'ast syn::ExprPath) {
        let parts = expression
            .path
            .segments
            .iter()
            .map(|part| part.ident.to_string())
            .collect::<Vec<_>>();
        let direct_value_member = parts.len() >= 2
            && parts
                .get(parts.len() - 2)
                .is_some_and(|part| self.value_names.contains(part));
        let qself_value_member = expression.qself.as_ref().is_some_and(|qself| {
            type_terminal_name(&qself.ty).is_some_and(|name| self.value_names.contains(&name))
        });
        let forbidden_member = parts
            .last()
            .is_some_and(|part| part == "UNDEFINED" || part == "default");
        if forbidden_member && (direct_value_member || qself_value_member) {
            self.findings
                .value_undefined
                .push(self.location(expression));
        }
        visit::visit_expr_path(self, expression);
    }

    fn visit_expr_call(&mut self, expression: &'ast syn::ExprCall) {
        let is_ok = path_expression_ends_with(expression.func.as_ref(), "Ok");
        let is_none = expression
            .args
            .first()
            .is_some_and(|argument| is_none_path(peel_expression(argument)));
        if is_ok && expression.args.len() == 1 && is_none && !self.allow_optional_empty {
            self.findings.optional_empty.push(self.location(expression));
        }
        visit::visit_expr_call(self, expression);
    }

    fn visit_expr_method_call(&mut self, expression: &'ast syn::ExprMethodCall) {
        if expression.method == "unwrap_or_default" && expression.args.is_empty() {
            self.findings
                .unwrap_or_default
                .push(self.location(expression));
        }
        visit::visit_expr_method_call(self, expression);
    }
}

fn is_none_path(expression: &syn::Expr) -> bool {
    let syn::Expr::Path(path) = expression else {
        return false;
    };
    path.qself.is_none()
        && path
            .path
            .segments
            .last()
            .is_some_and(|part| part.ident == "None")
}

fn is_exact_no_output_target(path: &Path, item: &syn::ItemFn) -> bool {
    path.ends_with("crates/rumoca-phase-codegen/src/codegen/scalar_program_plan.rs")
        && item.sig.ident == "no_output_target"
        && item.block.stmts.len() == 1
        && matches!(item.block.stmts.first(), Some(syn::Stmt::Expr(expression, None))
            if matches!(expression, syn::Expr::Call(call)
                if path_expression_ends_with(call.func.as_ref(), "Ok")
                    && call.args.len() == 1
                    && call.args.first().is_some_and(|argument| is_none_path(peel_expression(argument)))))
}

fn minijinja_value_names(file: &syn::File) -> BTreeSet<String> {
    struct AliasVisitor {
        names: BTreeSet<String>,
    }

    impl<'ast> Visit<'ast> for AliasVisitor {
        fn visit_item(&mut self, item: &'ast syn::Item) {
            if attributes_require_test(item_attributes(item)) {
                return;
            }
            visit::visit_item(self, item);
        }

        fn visit_item_use(&mut self, item: &'ast syn::ItemUse) {
            collect_value_use_aliases(&item.tree, false, &mut self.names);
            visit::visit_item_use(self, item);
        }
    }

    let mut visitor = AliasVisitor {
        names: BTreeSet::from(["Value".to_string()]),
    };
    visitor.visit_file(file);
    let mut names = visitor.names;
    for _ in 0..=8 {
        let before = names.len();
        let mut aliases = ValueTypeAliasVisitor { names: &mut names };
        aliases.visit_file(file);
        if names.len() == before {
            break;
        }
    }
    names
}

struct ValueTypeAliasVisitor<'a> {
    names: &'a mut BTreeSet<String>,
}

impl<'ast> Visit<'ast> for ValueTypeAliasVisitor<'_> {
    fn visit_item(&mut self, item: &'ast syn::Item) {
        if attributes_require_test(item_attributes(item)) {
            return;
        }
        visit::visit_item(self, item);
    }

    fn visit_item_type(&mut self, item: &'ast syn::ItemType) {
        self.record(&item.ident, &item.ty);
        visit::visit_item_type(self, item);
    }

    fn visit_impl_item_type(&mut self, item: &'ast syn::ImplItemType) {
        self.record(&item.ident, &item.ty);
        visit::visit_impl_item_type(self, item);
    }

    fn visit_trait_item_type(&mut self, item: &'ast syn::TraitItemType) {
        if let Some((_, ty)) = &item.default {
            self.record(&item.ident, ty);
        }
        visit::visit_trait_item_type(self, item);
    }
}

impl ValueTypeAliasVisitor<'_> {
    fn record(&mut self, alias: &syn::Ident, ty: &syn::Type) {
        if type_terminal_name(ty).is_some_and(|name| self.names.contains(&name)) {
            self.names.insert(alias.to_string());
        }
    }
}

fn type_terminal_name(ty: &syn::Type) -> Option<String> {
    let syn::Type::Path(path) = ty else {
        return None;
    };
    path.path.segments.last().map(|part| part.ident.to_string())
}

fn collect_value_use_aliases(
    tree: &syn::UseTree,
    under_minijinja: bool,
    names: &mut BTreeSet<String>,
) {
    match tree {
        syn::UseTree::Path(path) => collect_value_use_aliases(
            path.tree.as_ref(),
            under_minijinja || path.ident == "minijinja",
            names,
        ),
        syn::UseTree::Rename(rename) if under_minijinja && rename.ident == "Value" => {
            names.insert(rename.rename.to_string());
        }
        syn::UseTree::Name(name) if under_minijinja && name.ident == "Value" => {
            names.insert(name.ident.to_string());
        }
        syn::UseTree::Group(group) => {
            for item in &group.items {
                collect_value_use_aliases(item, under_minijinja, names);
            }
        }
        _ => {}
    }
}

fn peel_expression(mut expression: &syn::Expr) -> &syn::Expr {
    loop {
        expression = match expression {
            syn::Expr::Group(group) => &group.expr,
            syn::Expr::Paren(paren) => &paren.expr,
            _ => return expression,
        };
    }
}

fn path_expression_ends_with(expression: &syn::Expr, expected: &str) -> bool {
    let syn::Expr::Path(path) = expression else {
        return false;
    };
    path.path
        .segments
        .last()
        .is_some_and(|part| part.ident == expected)
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
