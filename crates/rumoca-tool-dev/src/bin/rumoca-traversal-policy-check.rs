use anyhow::{Context, Result, bail};
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fs;
use std::path::{Path, PathBuf};
use syn::visit::Visit;
use syn::{
    FnArg, GenericArgument, ImplItem, Item, ItemFn, ItemImpl, PathArguments, Type, TypeParamBound,
};

const COVERED_FILES: &[&str] = &[
    "crates/rumoca-phase-resolve/src/contents.rs",
    "crates/rumoca-phase-resolve/src/validation.rs",
    "crates/rumoca-phase-resolve/src/semantic_checks.rs",
    "crates/rumoca-phase-resolve/src/semantic_checks_expr.rs",
    "crates/rumoca-phase-typecheck/src/typechecker/late_methods.rs",
    "crates/rumoca-session/src/session/dependency_fingerprint.rs",
    "crates/rumoca-tool-lsp/src/handlers/semantic_tokens.rs",
    "crates/rumoca-tool-lsp/src/handlers/inlay_hints.rs",
    "crates/rumoca-phase-dae/src/scalar_inference/parts.rs",
];

fn main() -> Result<()> {
    let repo_root = repo_root();
    let mut violations = Vec::new();

    for rel_path in COVERED_FILES {
        let file_path = repo_root.join(rel_path);
        let source = fs::read_to_string(&file_path)
            .with_context(|| format!("failed to read {}", file_path.display()))?;
        let syntax = syn::parse_file(&source)
            .with_context(|| format!("failed to parse {}", file_path.display()))?;

        let candidates = collect_candidates(syntax.items);

        if candidates.is_empty() {
            continue;
        }

        let allowed_recursive = allowed_recursive_functions(rel_path);
        let mut graph: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
        for function in candidates.values() {
            if allowed_recursive.contains(function.simple_name.as_str()) {
                continue;
            }
            let outgoing = candidates
                .values()
                .filter(|candidate| {
                    candidate.scope == function.scope
                        && function.calls.contains(&candidate.simple_name)
                        && !allowed_recursive.contains(candidate.simple_name.as_str())
                })
                .map(|candidate| candidate.key.clone())
                .collect();
            graph.insert(function.key.clone(), outgoing);
        }

        if let Some(cycle) = detect_cycle(&graph) {
            violations.push(format!(
                "{}: recursive traversal helpers are disallowed in covered modules: {}",
                rel_path,
                cycle.join(" -> ")
            ));
        }
    }

    if !violations.is_empty() {
        eprintln!("Traversal policy check failed:");
        for violation in violations {
            eprintln!("  - {violation}");
        }
        bail!("traversal policy violations detected");
    }

    println!(
        "Traversal policy check passed for {} covered files.",
        COVERED_FILES.len()
    );
    Ok(())
}

fn collect_candidates(items: Vec<Item>) -> BTreeMap<String, CandidateFunction> {
    let mut candidates = BTreeMap::new();
    for item in items {
        match item {
            Item::Fn(item_fn) => {
                insert_candidate_if_traversal(
                    &mut candidates,
                    CandidateFunction::from_top_level_fn(item_fn),
                );
            }
            Item::Impl(item_impl) => {
                for function in CandidateFunction::from_impl(item_impl) {
                    insert_candidate_if_traversal(&mut candidates, function);
                }
            }
            _ => {}
        }
    }
    candidates
}

fn insert_candidate_if_traversal(
    candidates: &mut BTreeMap<String, CandidateFunction>,
    function: CandidateFunction,
) {
    if !function.is_traversal_candidate {
        return;
    }
    candidates.insert(function.key.clone(), function);
}

fn allowed_recursive_functions(file: &str) -> BTreeSet<&'static str> {
    match file {
        // Class-container recursion over nested classes remains intentional in these modules.
        "crates/rumoca-phase-resolve/src/contents.rs" => BTreeSet::from(["resolve_contents_class"]),
        "crates/rumoca-phase-resolve/src/validation.rs" => BTreeSet::from(["visit_class_def"]),
        "crates/rumoca-phase-resolve/src/semantic_checks.rs" => BTreeSet::from(["visit_class_def"]),
        "crates/rumoca-phase-resolve/src/semantic_checks_expr.rs" => {
            BTreeSet::from(["visit_class_def"])
        }
        "crates/rumoca-phase-typecheck/src/typechecker/late_methods.rs" => {
            BTreeSet::from(["check_class", "infer_expression_type"])
        }
        // This recursion decomposes nested array literals for scalar sizing.
        "crates/rumoca-phase-dae/src/scalar_inference/parts.rs" => {
            BTreeSet::from(["count_array_lhs_scalar_elements"])
        }
        _ => BTreeSet::new(),
    }
}

fn repo_root() -> PathBuf {
    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    manifest_dir
        .ancestors()
        .nth(2)
        .unwrap_or(manifest_dir)
        .to_path_buf()
}

#[derive(Debug, Clone)]
struct CandidateFunction {
    key: String,
    simple_name: String,
    scope: CandidateScope,
    is_traversal_candidate: bool,
    calls: BTreeSet<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum CandidateScope {
    TopLevel,
    Impl(String),
}

impl CandidateFunction {
    fn from_top_level_fn(item_fn: ItemFn) -> Self {
        let simple_name = item_fn.sig.ident.to_string();
        let is_traversal_candidate = item_fn.sig.inputs.iter().any(is_traversal_param_fn_arg);
        let mut collector = CallCollector::default();
        collector.visit_block(&item_fn.block);
        Self {
            key: simple_name.clone(),
            simple_name,
            scope: CandidateScope::TopLevel,
            is_traversal_candidate,
            calls: collector.calls,
        }
    }

    fn from_impl(item_impl: ItemImpl) -> Vec<Self> {
        let scope_name = impl_scope_name(&item_impl);
        let scope = CandidateScope::Impl(scope_name.clone());
        let mut functions = Vec::new();
        for item in item_impl.items {
            let ImplItem::Fn(item_fn) = item else {
                continue;
            };
            let simple_name = item_fn.sig.ident.to_string();
            let is_traversal_candidate = item_fn.sig.inputs.iter().any(is_traversal_param_fn_arg);
            let mut collector = CallCollector::default();
            collector.visit_block(&item_fn.block);
            functions.push(Self {
                key: format!("impl::{scope_name}::{simple_name}"),
                simple_name,
                scope: scope.clone(),
                is_traversal_candidate,
                calls: collector.calls,
            });
        }
        functions
    }
}

fn impl_scope_name(item_impl: &ItemImpl) -> String {
    let self_ty_name = type_display_name(item_impl.self_ty.as_ref());
    if let Some((_, path, _)) = &item_impl.trait_ {
        let trait_name = path
            .segments
            .last()
            .map(|segment| segment.ident.to_string())
            .unwrap_or_else(|| "Trait".to_string());
        format!("{trait_name} for {self_ty_name}")
    } else {
        self_ty_name
    }
}

fn type_display_name(ty: &Type) -> String {
    match ty {
        Type::Path(type_path) => type_path
            .path
            .segments
            .last()
            .map(|segment| segment.ident.to_string())
            .unwrap_or_else(|| "Type".to_string()),
        Type::Reference(reference) => type_display_name(reference.elem.as_ref()),
        Type::Paren(paren) => type_display_name(paren.elem.as_ref()),
        Type::Group(group) => type_display_name(group.elem.as_ref()),
        _ => "Type".to_string(),
    }
}

#[derive(Default)]
struct CallCollector {
    calls: BTreeSet<String>,
}

impl<'ast> Visit<'ast> for CallCollector {
    fn visit_expr_call(&mut self, node: &'ast syn::ExprCall) {
        if let syn::Expr::Path(path_expr) = node.func.as_ref() {
            let segments = &path_expr.path.segments;
            let Some(last) = segments.last() else {
                syn::visit::visit_expr_call(self, node);
                return;
            };
            let accepts_call = path_expr.qself.is_some()
                || segments.len() == 1
                || (segments.len() == 2 && segments[0].ident == "Self");
            if accepts_call {
                self.calls.insert(last.ident.to_string());
            }
        }
        syn::visit::visit_expr_call(self, node);
    }

    fn visit_expr_method_call(&mut self, node: &'ast syn::ExprMethodCall) {
        if receiver_is_self(node.receiver.as_ref()) {
            self.calls.insert(node.method.to_string());
        }
        syn::visit::visit_expr_method_call(self, node);
    }
}

fn receiver_is_self(expr: &syn::Expr) -> bool {
    matches!(
        expr,
        syn::Expr::Path(path_expr)
            if path_expr.qself.is_none()
                && path_expr.path.segments.len() == 1
                && path_expr.path.segments[0].ident == "self"
    )
}

fn is_traversal_param_fn_arg(arg: &FnArg) -> bool {
    match arg {
        FnArg::Typed(pat_type) => type_mentions_tree_type(&pat_type.ty),
        FnArg::Receiver(_) => false,
    }
}

fn type_mentions_tree_type(ty: &Type) -> bool {
    match ty {
        Type::Path(type_path) => path_mentions_tree_type(&type_path.path),
        Type::Reference(reference) => type_mentions_tree_type(&reference.elem),
        Type::Slice(slice) => type_mentions_tree_type(&slice.elem),
        Type::Array(array) => type_mentions_tree_type(&array.elem),
        Type::Tuple(tuple) => tuple.elems.iter().any(type_mentions_tree_type),
        Type::Paren(paren) => type_mentions_tree_type(&paren.elem),
        Type::Group(group) => type_mentions_tree_type(&group.elem),
        Type::Ptr(ptr) => type_mentions_tree_type(&ptr.elem),
        Type::ImplTrait(impl_trait) => impl_trait.bounds.iter().any(bound_mentions_tree_type),
        Type::TraitObject(trait_object) => trait_object.bounds.iter().any(bound_mentions_tree_type),
        _ => false,
    }
}

fn bound_mentions_tree_type(bound: &TypeParamBound) -> bool {
    match bound {
        TypeParamBound::Trait(trait_bound) => path_mentions_tree_type(&trait_bound.path),
        TypeParamBound::Lifetime(_) => false,
        _ => false,
    }
}

fn path_mentions_tree_type(path: &syn::Path) -> bool {
    path.segments.iter().any(|segment| {
        is_tree_type_name(segment.ident.to_string().as_str())
            || match &segment.arguments {
                PathArguments::AngleBracketed(args) => args.args.iter().any(|arg| match arg {
                    GenericArgument::Type(ty) => type_mentions_tree_type(ty),
                    GenericArgument::AssocType(assoc_type) => {
                        type_mentions_tree_type(&assoc_type.ty)
                    }
                    GenericArgument::Constraint(constraint) => {
                        constraint.bounds.iter().any(bound_mentions_tree_type)
                    }
                    GenericArgument::AssocConst(_)
                    | GenericArgument::Lifetime(_)
                    | GenericArgument::Const(_) => false,
                    _ => false,
                }),
                PathArguments::Parenthesized(parenthesized) => {
                    parenthesized.inputs.iter().any(type_mentions_tree_type)
                        || match &parenthesized.output {
                            syn::ReturnType::Default => false,
                            syn::ReturnType::Type(_, ty) => type_mentions_tree_type(ty),
                        }
                }
                PathArguments::None => false,
            }
    })
}

fn is_tree_type_name(name: &str) -> bool {
    matches!(
        name,
        "Expression"
            | "Statement"
            | "Equation"
            | "Subscript"
            | "StoredDefinition"
            | "ClassDef"
            | "ClassSection"
            | "Class"
            | "Element"
            | "ComponentReference"
            | "ComprehensionIndex"
            | "ForIndex"
            | "StatementBlock"
            | "TypeName"
            | "Import"
            | "NamedArgument"
            | "ExtendsClause"
    )
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum VisitState {
    Visiting,
    Done,
}

fn dfs_cycle(
    node: &str,
    graph: &BTreeMap<String, BTreeSet<String>>,
    states: &mut HashMap<String, VisitState>,
    stack: &mut Vec<String>,
) -> Option<Vec<String>> {
    states.insert(node.to_string(), VisitState::Visiting);
    stack.push(node.to_string());

    if let Some(neighbors) = graph.get(node) {
        for neighbor in neighbors {
            if let Some(cycle) = traverse_neighbor(neighbor, graph, states, stack) {
                return Some(cycle);
            }
        }
    }

    stack.pop();
    states.insert(node.to_string(), VisitState::Done);
    None
}

fn traverse_neighbor(
    neighbor: &str,
    graph: &BTreeMap<String, BTreeSet<String>>,
    states: &mut HashMap<String, VisitState>,
    stack: &mut Vec<String>,
) -> Option<Vec<String>> {
    match states.get(neighbor).copied() {
        Some(VisitState::Done) => None,
        Some(VisitState::Visiting) => cycle_from_back_edge(stack, neighbor),
        None => dfs_cycle(neighbor, graph, states, stack),
    }
}

fn cycle_from_back_edge(stack: &[String], neighbor: &str) -> Option<Vec<String>> {
    let start = stack.iter().position(|name| name == neighbor)?;
    let mut cycle = stack[start..].to_vec();
    cycle.push(neighbor.to_string());
    Some(cycle)
}

fn detect_cycle(graph: &BTreeMap<String, BTreeSet<String>>) -> Option<Vec<String>> {
    let mut states: HashMap<String, VisitState> = HashMap::new();
    let mut stack = Vec::new();

    for node in graph.keys() {
        if states.contains_key(node) {
            continue;
        }
        if let Some(cycle) = dfs_cycle(node, graph, &mut states, &mut stack) {
            return Some(cycle);
        }
    }
    None
}
