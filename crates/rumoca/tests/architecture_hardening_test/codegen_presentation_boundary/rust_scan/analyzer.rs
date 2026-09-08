//! Syn-based semantic-consumer, transform, and target-text analysis.

mod support;

use super::super::super::architecture_hardening_support::{
    ProductionRustSourceContext, attributes_require_test,
};
use super::super::content_fingerprint;
use std::collections::{BTreeMap, BTreeSet};
use std::path::PathBuf;
use support::*;
use syn::visit::{self, Visit};

#[derive(Default)]
struct FunctionFacts {
    file: String,
    owner: String,
    fingerprint: String,
    sinks: Vec<String>,
    actual_sinks_exempt: bool,
    unconditional_findings: Vec<String>,
}

struct AnalysisContext {
    semantic_types: SemanticTypeIndex,
}

pub(crate) fn analyze_sources(sources: &[(PathBuf, String)]) -> BTreeSet<String> {
    let contexts = sources
        .iter()
        .enumerate()
        .map(|(ordinal, (path, source))| ProductionRustSourceContext {
            canonical_path: path.clone(),
            crate_aliases: BTreeMap::new(),
            module_path: Vec::new(),
            path: path.clone(),
            source: source.clone(),
            target: format!("fixture-{ordinal}"),
        })
        .collect::<Vec<_>>();
    analyze_source_contexts(&contexts)
}

pub(crate) fn analyze_source_contexts(sources: &[ProductionRustSourceContext]) -> BTreeSet<String> {
    let context_counts = sources.iter().fold(BTreeMap::new(), |mut counts, context| {
        *counts.entry(&context.canonical_path).or_insert(0_usize) += 1;
        counts
    });
    let parsed_sources = sources
        .iter()
        .map(|context| {
            let display_path = context.path.display().to_string();
            let file = if context_counts[&context.canonical_path] > 1 {
                let module = if context.module_path.is_empty() {
                    "<root>".to_string()
                } else {
                    context.module_path.join("::")
                };
                format!("{display_path}[target={} module={module}]", context.target)
            } else {
                display_path
            };
            let syntax = syn::parse_file(&context.source)
                .unwrap_or_else(|error| panic!("parse {}: {error}", context.path.display()));
            let mut module = vec![format!("target={}", context.target)];
            module.extend(context.module_path.iter().cloned());
            ParsedRustSource {
                external_crates: context.crate_aliases.clone(),
                file,
                module,
                syntax,
            }
        })
        .collect::<Vec<_>>();
    let semantic_types = SemanticTypeIndex::new(&parsed_sources);
    let context = AnalysisContext { semantic_types };
    let mut functions = Vec::new();
    let mut standalone = Vec::new();
    for source in &parsed_sources {
        ItemCollector {
            context: &context,
            file: &source.file,
            functions: &mut functions,
            module: Vec::new(),
            semantic_module: source.module.clone(),
            standalone: &mut standalone,
        }
        .collect_items(&source.syntax.items);
    }

    let mut findings = standalone.into_iter().collect::<BTreeSet<_>>();
    for facts in &functions {
        findings.extend(facts.unconditional_findings.iter().cloned());
        if facts.actual_sinks_exempt {
            continue;
        }
        let mut ordinals = BTreeMap::<&str, usize>::new();
        for sink in &facts.sinks {
            let ordinal = ordinals.entry(sink).or_default();
            *ordinal += 1;
            findings.insert(format!(
                "{}::{}:generated-text:{sink}:{}#{ordinal}",
                facts.file, facts.owner, facts.fingerprint
            ));
        }
    }
    findings
}

struct ItemCollector<'a> {
    context: &'a AnalysisContext,
    file: &'a str,
    functions: &'a mut Vec<FunctionFacts>,
    module: Vec<String>,
    semantic_module: Vec<String>,
    standalone: &'a mut Vec<String>,
}

impl ItemCollector<'_> {
    fn collect_items(&mut self, items: &[syn::Item]) {
        for item in items {
            if attributes_require_test(item_attributes(item)) {
                continue;
            }
            match item {
                syn::Item::Fn(function) => self.collect_function(function),
                syn::Item::Impl(implementation) => self.collect_impl(implementation),
                syn::Item::Trait(item_trait) => self.collect_trait(item_trait),
                syn::Item::Mod(item_module) => self.collect_module(item_module),
                syn::Item::Use(item_use) => self.collect_use(item_use),
                syn::Item::Type(item_type) => self.collect_type_alias(item_type),
                syn::Item::Struct(item_struct) => self.collect_struct(item_struct),
                syn::Item::Enum(item_enum) => self.collect_enum(item_enum),
                syn::Item::Union(item_union) => self.collect_union(item_union),
                syn::Item::Const(item_const) => self.collect_const(item_const),
                syn::Item::Static(item_static) => self.collect_static(item_static),
                syn::Item::Macro(item_macro) => self.collect_macro(item_macro),
                _ => {}
            }
        }
    }

    fn collect_function(&mut self, function: &syn::ItemFn) {
        let facts = analyze_function(
            self.function_scope(function.sig.ident.to_string(), false, false),
            &function.sig,
            &function.block,
        );
        self.functions.push(facts);
    }

    fn collect_impl(&mut self, implementation: &syn::ItemImpl) {
        let type_name = impl_type_name(&implementation.self_ty);
        let semantic_self = self
            .context
            .semantic_types
            .type_mentions_semantic(&self.semantic_module, &implementation.self_ty);
        let display_impl = implementation.trait_.as_ref().is_some_and(|(_, path, _)| {
            path.segments
                .last()
                .is_some_and(|segment| segment.ident == "Display")
        }) && is_known_diagnostic_type_name(&type_name);
        self.module.push(type_name);
        for method in &implementation.items {
            let syn::ImplItem::Fn(method) = method else {
                continue;
            };
            if attributes_require_test(&method.attrs) {
                continue;
            }
            let facts = analyze_function(
                self.function_scope(
                    method.sig.ident.to_string(),
                    display_impl && method.sig.ident == "fmt",
                    semantic_self,
                ),
                &method.sig,
                &method.block,
            );
            self.functions.push(facts);
        }
        self.module.pop();
    }

    fn collect_trait(&mut self, item_trait: &syn::ItemTrait) {
        self.module.push(item_trait.ident.to_string());
        for item in &item_trait.items {
            let syn::TraitItem::Fn(method) = item else {
                continue;
            };
            if attributes_require_test(&method.attrs) {
                continue;
            }
            let Some(default) = &method.default else {
                continue;
            };
            let facts = analyze_function(
                self.function_scope(method.sig.ident.to_string(), false, false),
                &method.sig,
                default,
            );
            self.functions.push(facts);
        }
        self.module.pop();
    }

    fn collect_module(&mut self, item_module: &syn::ItemMod) {
        let Some((_, nested)) = &item_module.content else {
            return;
        };
        self.module.push(item_module.ident.to_string());
        self.semantic_module.push(item_module.ident.to_string());
        self.collect_items(nested);
        self.semantic_module.pop();
        self.module.pop();
    }

    fn collect_use(&mut self, item_use: &syn::ItemUse) {
        for alias in self
            .context
            .semantic_types
            .semantic_import_aliases(&self.semantic_module, &item_use.tree)
        {
            self.standalone.push(format!(
                "{}::{}:semantic-ir-alias:{alias}",
                self.file,
                owner_name(&self.module, "<module>")
            ));
        }
        for path in flattened_use_paths(&item_use.tree) {
            let is_semantic_import = path
                .iter()
                .any(|segment| is_semantic_transform_name(segment))
                || path
                    .first()
                    .is_some_and(|segment| segment.starts_with("rumoca_eval_"));
            if is_semantic_import {
                self.standalone.push(format!(
                    "{}::{}:semantic-import:{}",
                    self.file,
                    owner_name(&self.module, "<module>"),
                    path.join("::")
                ));
            }
        }
    }

    fn collect_type_alias(&mut self, item: &syn::ItemType) {
        if self
            .context
            .semantic_types
            .type_mentions_semantic(&self.semantic_module, &item.ty)
        {
            self.push_module_finding("semantic-ir-type-alias", &item.ident.to_string());
        }
    }

    fn collect_struct(&mut self, item: &syn::ItemStruct) {
        let needs_review = item.fields.iter().any(|field| {
            self.context
                .semantic_types
                .semantic_field_requires_review(&self.semantic_module, &field.ty)
        });
        if needs_review {
            self.push_module_finding("semantic-ir-wrapper", &item.ident.to_string());
        }
    }

    fn collect_enum(&mut self, item: &syn::ItemEnum) {
        let needs_review = item
            .variants
            .iter()
            .flat_map(|variant| &variant.fields)
            .any(|field| {
                self.context
                    .semantic_types
                    .semantic_field_requires_review(&self.semantic_module, &field.ty)
            });
        if needs_review {
            self.push_module_finding("semantic-ir-wrapper", &item.ident.to_string());
        }
    }

    fn collect_union(&mut self, item: &syn::ItemUnion) {
        let needs_review = item.fields.named.iter().any(|field| {
            self.context
                .semantic_types
                .semantic_field_requires_review(&self.semantic_module, &field.ty)
        });
        if needs_review {
            self.push_module_finding("semantic-ir-wrapper", &item.ident.to_string());
        }
    }

    fn collect_const(&mut self, item: &syn::ItemConst) {
        let kind = if self
            .context
            .semantic_types
            .type_contains_owned_semantic_ir(&self.semantic_module, &item.ty)
        {
            Some("semantic-ir-const")
        } else if self
            .context
            .semantic_types
            .type_contains_text(&self.semantic_module, &item.ty)
        {
            Some("generated-text:text-const")
        } else {
            None
        };
        if let Some(kind) = kind {
            self.push_fingerprinted_item(kind, &item.ident.to_string(), item);
        }
    }

    fn collect_static(&mut self, item: &syn::ItemStatic) {
        let kind = if self
            .context
            .semantic_types
            .type_contains_owned_semantic_ir(&self.semantic_module, &item.ty)
        {
            Some("semantic-ir-static")
        } else if self
            .context
            .semantic_types
            .type_contains_text(&self.semantic_module, &item.ty)
        {
            Some("generated-text:text-static")
        } else {
            None
        };
        if let Some(kind) = kind {
            self.push_fingerprinted_item(kind, &item.ident.to_string(), item);
        }
    }

    fn collect_macro(&mut self, item: &syn::ItemMacro) {
        if macro_name(&item.mac).as_deref() == Some("include") {
            self.push_module_finding("source-include", "include");
        }
        let Some(identity) = &item.ident else {
            return;
        };
        if macro_rules_assembles_text(&item.mac.tokens.to_string()) {
            self.push_fingerprinted_item("generated-text:macro-rules", &identity.to_string(), item);
        }
    }

    fn function_scope(
        &self,
        item: String,
        display_fmt: bool,
        semantic_self: bool,
    ) -> FunctionScope<'_> {
        FunctionScope {
            context: self.context,
            display_fmt,
            file: self.file,
            owner: owner_name(&self.module, &item),
            semantic_module: &self.semantic_module,
            semantic_self,
        }
    }

    fn push_module_finding(&mut self, kind: &str, detail: &str) {
        self.standalone.push(format!(
            "{}::{}:{kind}:{detail}",
            self.file,
            owner_name(&self.module, "<module>")
        ));
    }

    fn push_fingerprinted_item<T: quote::ToTokens>(&mut self, kind: &str, item: &str, value: &T) {
        self.standalone.push(format!(
            "{}::{}:{kind}:{}",
            self.file,
            owner_name(&self.module, item),
            normalized_tokens_fingerprint(value)
        ));
    }
}

fn owner_name(module: &[String], item: &str) -> String {
    module
        .iter()
        .map(String::as_str)
        .chain(std::iter::once(item))
        .collect::<Vec<_>>()
        .join("::")
}

fn impl_type_name(ty: &syn::Type) -> String {
    if let syn::Type::Path(path) = ty
        && let Some(segment) = path.path.segments.last()
    {
        return segment.ident.to_string();
    }
    "<impl>".to_string()
}

struct FunctionScope<'a> {
    context: &'a AnalysisContext,
    display_fmt: bool,
    file: &'a str,
    owner: String,
    semantic_module: &'a [String],
    semantic_self: bool,
}

fn analyze_function(
    scope: FunctionScope<'_>,
    signature: &syn::Signature,
    body: &syn::Block,
) -> FunctionFacts {
    let FunctionScope {
        context,
        display_fmt,
        file,
        owner,
        semantic_module,
        semantic_self,
    } = scope;
    let fingerprint = normalized_function_fingerprint(signature, body);
    let mut semantic_parameters = BTreeSet::new();
    let mut string_values = BTreeSet::new();
    let mut consumes_semantic = false;
    let mut unconditional_findings = Vec::new();
    for argument in &signature.inputs {
        match argument {
            syn::FnArg::Receiver(receiver) if semantic_self => {
                consumes_semantic = true;
                semantic_parameters.insert("self".to_string());
                if receiver.mutability.is_some() {
                    unconditional_findings
                        .push(format!("{file}::{owner}:semantic-mutable-parameter"));
                }
            }
            syn::FnArg::Typed(argument) => {
                let semantic_argument = context
                    .semantic_types
                    .type_mentions_semantic(semantic_module, &argument.ty);
                if semantic_argument {
                    consumes_semantic = true;
                    pattern_names(&argument.pat, &mut semantic_parameters);
                }
                if semantic_argument && type_is_mutable_reference(&argument.ty) {
                    unconditional_findings
                        .push(format!("{file}::{owner}:semantic-mutable-parameter"));
                }
                if context
                    .semantic_types
                    .type_contains_text(semantic_module, &argument.ty)
                {
                    pattern_names(&argument.pat, &mut string_values);
                }
            }
            syn::FnArg::Receiver(_) => {}
        }
    }
    if let syn::ReturnType::Type(_, output) = &signature.output
        && context
            .semantic_types
            .type_contains_owned_semantic_ir(semantic_module, output)
    {
        unconditional_findings.push(format!("{file}::{owner}:semantic-ir-owned-return"));
    }
    let mut visitor = BodyVisitor {
        semantic_module: semantic_module.to_vec(),
        semantic_types: &context.semantic_types,
        semantic_parameters,
        string_values,
        sinks: Vec::new(),
        schema_findings: Vec::new(),
        transform_findings: Vec::new(),
    };
    visitor.visit_block(body);
    if returns_bare_string_literal(body) {
        visitor.sinks.push("literal-return".to_string());
    }
    let actual_sinks_exempt = is_pure_diagnostic_format(signature, body, display_fmt)
        || super::is_exact_modelica_codec_delegate(file, &owner, signature, body);
    if consumes_semantic {
        unconditional_findings.push(format!("{file}::{owner}:semantic-consumer:{}", fingerprint));
    }
    if signature_returns_text(&context.semantic_types, semantic_module, signature)
        && visitor.sinks.is_empty()
    {
        visitor.sinks.push("return-text".to_string());
    }
    unconditional_findings.extend(visitor.schema_findings.into_iter().enumerate().map(
        |(ordinal, finding)| format!("{file}::{owner}:{finding}:{fingerprint}#{}", ordinal + 1),
    ));
    unconditional_findings.extend(
        visitor
            .transform_findings
            .into_iter()
            .map(|finding| format!("{file}::{owner}:{finding}")),
    );
    FunctionFacts {
        file: file.to_string(),
        owner,
        fingerprint,
        sinks: visitor.sinks,
        actual_sinks_exempt,
        unconditional_findings,
    }
}

fn normalized_function_fingerprint(signature: &syn::Signature, body: &syn::Block) -> String {
    use quote::ToTokens;

    content_fingerprint(&format!(
        "{} {}",
        signature.to_token_stream(),
        body.to_token_stream()
    ))
}

fn normalized_tokens_fingerprint<T: quote::ToTokens>(item: &T) -> String {
    content_fingerprint(&item.to_token_stream().to_string())
}

fn signature_returns_text(
    semantic_types: &SemanticTypeIndex,
    semantic_module: &[String],
    signature: &syn::Signature,
) -> bool {
    matches!(&signature.output, syn::ReturnType::Type(_, output)
        if semantic_types.type_contains_text(semantic_module, output))
}

fn signature_returns_direct_diagnostic(signature: &syn::Signature) -> bool {
    matches!(&signature.output, syn::ReturnType::Type(_, output) if type_is_direct_diagnostic(output))
}

fn is_pure_diagnostic_format(
    signature: &syn::Signature,
    body: &syn::Block,
    display_fmt: bool,
) -> bool {
    if body.stmts.len() != 1 {
        return false;
    }
    let Some(syn::Stmt::Expr(expression, _)) = body.stmts.first() else {
        return false;
    };
    let expression = match expression {
        syn::Expr::Return(returned) => returned.expr.as_deref().unwrap_or(expression),
        other => other,
    };
    if display_fmt {
        return expression_is_diagnostic_write(expression);
    }
    signature_returns_direct_diagnostic(signature)
        && expression_constructs_known_diagnostic(expression)
}

fn expression_is_diagnostic_write(expression: &syn::Expr) -> bool {
    match expression {
        syn::Expr::Macro(item) => {
            macro_name(&item.mac).is_some_and(|name| matches!(name.as_str(), "write" | "writeln"))
        }
        syn::Expr::MethodCall(call) => {
            matches!(call.method.to_string().as_str(), "write_str" | "write_fmt")
        }
        syn::Expr::Paren(paren) => expression_is_diagnostic_write(&paren.expr),
        syn::Expr::Group(group) => expression_is_diagnostic_write(&group.expr),
        _ => false,
    }
}

fn expression_constructs_known_diagnostic(expression: &syn::Expr) -> bool {
    match expression {
        syn::Expr::Call(call) => matches!(call.func.as_ref(), syn::Expr::Path(path)
        if path.path.segments.iter().any(|segment| {
            is_known_diagnostic_type_name(&segment.ident.to_string())
        })),
        syn::Expr::Struct(item) => item
            .path
            .segments
            .iter()
            .any(|segment| is_known_diagnostic_type_name(&segment.ident.to_string())),
        syn::Expr::Paren(paren) => expression_constructs_known_diagnostic(&paren.expr),
        syn::Expr::Group(group) => expression_constructs_known_diagnostic(&group.expr),
        _ => false,
    }
}

fn type_is_direct_diagnostic(ty: &syn::Type) -> bool {
    match ty {
        syn::Type::Path(path) => {
            let segments = path
                .path
                .segments
                .iter()
                .map(|segment| segment.ident.to_string())
                .collect::<Vec<_>>();
            segments
                .last()
                .is_some_and(|name| is_known_diagnostic_type_name(name))
                || segments.ends_with(&["minijinja".to_string(), "Error".to_string()])
        }
        syn::Type::Group(group) => type_is_direct_diagnostic(&group.elem),
        syn::Type::Paren(paren) => type_is_direct_diagnostic(&paren.elem),
        _ => false,
    }
}

fn is_known_diagnostic_type_name(name: &str) -> bool {
    matches!(
        name,
        "CodegenError" | "DaeBackendError" | "TemplateSemanticError"
    )
}

fn macro_rules_assembles_text(tokens: &str) -> bool {
    let compact = tokens
        .chars()
        .filter(|character| !character.is_whitespace())
        .collect::<String>();
    compact.contains("format!(")
        || compact.contains("write!(")
        || compact.contains("writeln!(")
        || compact.contains("push_str(")
        || compact.contains(".to_string(")
        || compact.contains('"')
}

struct BodyVisitor<'a> {
    semantic_module: Vec<String>,
    semantic_parameters: BTreeSet<String>,
    semantic_types: &'a SemanticTypeIndex,
    string_values: BTreeSet<String>,
    sinks: Vec<String>,
    schema_findings: Vec<String>,
    transform_findings: Vec<String>,
}

impl<'ast> Visit<'ast> for BodyVisitor<'_> {
    fn visit_item_fn(&mut self, item: &'ast syn::ItemFn) {
        if self
            .semantic_types
            .signature_has_semantic_typed_input(&self.semantic_module, &item.sig)
        {
            self.transform_findings
                .push("nested-semantic-consumer:function".to_string());
        }
        visit::visit_item_fn(self, item);
    }

    fn visit_expr_closure(&mut self, item: &'ast syn::ExprClosure) {
        if item.inputs.iter().any(|pattern| {
            self.semantic_types
                .pattern_mentions_semantic_type(&self.semantic_module, pattern)
        }) {
            self.transform_findings
                .push("nested-semantic-consumer:closure".to_string());
        }
        visit::visit_expr_closure(self, item);
    }

    fn visit_expr_struct(&mut self, item: &'ast syn::ExprStruct) {
        if item
            .path
            .segments
            .iter()
            .any(|segment| is_owned_semantic_ir_name(&segment.ident.to_string()))
        {
            self.transform_findings
                .push("semantic-ir-construction:struct".to_string());
        }
        visit::visit_expr_struct(self, item);
    }

    fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
        if let syn::Expr::Path(path) = call.func.as_ref()
            && let Some(last) = path.path.segments.last()
        {
            let name = last.ident.to_string();
            if path
                .path
                .segments
                .iter()
                .take(path.path.segments.len().saturating_sub(1))
                .any(|segment| is_owned_semantic_ir_name(&segment.ident.to_string()))
            {
                self.transform_findings
                    .push(format!("semantic-ir-construction:{name}"));
            }
            if path
                .path
                .segments
                .first()
                .is_some_and(|segment| segment.ident.to_string().starts_with("rumoca_eval_"))
            {
                self.transform_findings
                    .push(format!("evaluator-package-call:{name}"));
            } else if is_semantic_transform_name(&name) {
                self.transform_findings
                    .push(format!("semantic-transform-call:{name}"));
            }
            let mut constructor_owner = path.path.clone();
            constructor_owner.segments.pop();
            if self
                .semantic_types
                .path_is_text(&self.semantic_module, &constructor_owner)
                && matches!(
                    name.as_str(),
                    "new" | "from" | "from_iter" | "with_capacity"
                )
            {
                self.sinks.push(format!("String::{name}"));
            }
            let injects_text_value = name == "from"
                && path
                    .path
                    .segments
                    .iter()
                    .rev()
                    .nth(1)
                    .is_some_and(|segment| segment.ident == "Value")
                && call.args.iter().any(|argument| {
                    expression_constructs_string(
                        argument,
                        &self.string_values,
                        self.semantic_types,
                        &self.semantic_module,
                    )
                });
            if injects_text_value {
                self.sinks.push("template-value-text".to_string());
            }
        }
        visit::visit_expr_call(self, call);
    }

    fn visit_expr_path(&mut self, path: &'ast syn::ExprPath) {
        if let Some(sink) = self
            .semantic_types
            .expression_path_text_sink(&self.semantic_module, path)
        {
            self.sinks.push(format!("call-{sink}"));
        }
        visit::visit_expr_path(self, path);
    }

    fn visit_expr_method_call(&mut self, call: &'ast syn::ExprMethodCall) {
        let name = call.method.to_string();
        if is_semantic_transform_name(&name) {
            self.transform_findings
                .push(format!("semantic-transform-call:{name}"));
        }
        let string_mutation = matches!(
            name.as_str(),
            "push" | "extend" | "extend_from_slice" | "insert" | "insert_str" | "replace_range"
        ) && (expression_root_name(&call.receiver)
            .is_some_and(|root| self.string_values.contains(&root))
            || call.args.iter().any(|argument| {
                expression_constructs_string(
                    argument,
                    &self.string_values,
                    self.semantic_types,
                    &self.semantic_module,
                )
            }));
        let string_collect = name == "collect"
            && call.turbofish.as_ref().is_some_and(|arguments| {
                arguments.args.iter().any(|argument| {
                    matches!(argument, syn::GenericArgument::Type(ty)
                        if self.semantic_types.type_contains_text(&self.semantic_module, ty))
                })
            });
        if string_mutation
            || string_collect
            || matches!(
                name.as_str(),
                "push_str"
                    | "replace"
                    | "join"
                    | "to_string"
                    | "to_owned"
                    | "write_all"
                    | "write_char"
                    | "write_fmt"
                    | "write_str"
            )
        {
            self.sinks.push(format!("method-{name}"));
        }
        if is_mutator_name(&name)
            && expression_root_name(&call.receiver)
                .is_some_and(|root| self.semantic_parameters.contains(&root))
        {
            self.transform_findings
                .push(format!("semantic-input-mutation:{name}"));
        }
        visit::visit_expr_method_call(self, call);
    }

    fn visit_expr_assign(&mut self, assignment: &'ast syn::ExprAssign) {
        if expression_root_name(&assignment.left)
            .is_some_and(|root| self.semantic_parameters.contains(&root))
        {
            self.transform_findings
                .push("semantic-input-mutation:assign".to_string());
        }
        visit::visit_expr_assign(self, assignment);
    }

    fn visit_expr_binary(&mut self, binary: &'ast syn::ExprBinary) {
        if is_assignment_operator(&binary.op)
            && expression_root_name(&binary.left)
                .is_some_and(|root| self.semantic_parameters.contains(&root))
        {
            self.transform_findings
                .push("semantic-input-mutation:assign-op".to_string());
        }
        let text_lhs = expression_is_string_value(&binary.left, &self.string_values);
        let text_rhs = expression_is_string_value(&binary.right, &self.string_values);
        if matches!(binary.op, syn::BinOp::Add(_)) && (text_lhs || text_rhs) {
            self.sinks.push("operator-add".to_string());
        }
        if matches!(binary.op, syn::BinOp::AddAssign(_)) && text_lhs {
            self.sinks.push("operator-add-assign".to_string());
        }
        visit::visit_expr_binary(self, binary);
    }

    fn visit_local(&mut self, local: &'ast syn::Local) {
        let explicitly_string = matches!(&local.pat, syn::Pat::Type(typed)
            if self.semantic_types.type_contains_text(&self.semantic_module, &typed.ty));
        if explicitly_string
            || local.init.as_ref().is_some_and(|initialization| {
                expression_constructs_string(
                    &initialization.expr,
                    &self.string_values,
                    self.semantic_types,
                    &self.semantic_module,
                )
            })
        {
            pattern_names(&local.pat, &mut self.string_values);
        }
        visit::visit_local(self, local);
    }

    fn visit_macro(&mut self, item: &'ast syn::Macro) {
        if let Some(name) = macro_name(item) {
            if matches!(
                name.as_str(),
                "format" | "format_args" | "concat" | "write" | "writeln"
            ) {
                self.sinks.push(format!("macro-{name}"));
            }
            if name == "include" {
                self.transform_findings
                    .push("source-include:include".to_string());
            }
            if name == "json" {
                self.schema_findings
                    .push("generated-schema:macro-json".to_string());
            }
            if name == "context" {
                self.schema_findings
                    .push("template-context:macro-context".to_string());
            }
        }
        visit::visit_macro(self, item);
    }
}
