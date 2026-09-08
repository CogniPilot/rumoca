use std::collections::BTreeMap;
use std::fs;
use std::path::Path;

use quote::ToTokens;
use syn::visit::Visit;

use super::{collect_rs_files, workspace_root};

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
struct RouteCall {
    file: String,
    owner: String,
    kind: &'static str,
}

struct RouteVisitor {
    file: String,
    owner: Vec<String>,
    aliases: BTreeMap<String, String>,
    calls: Vec<RouteCall>,
}

impl RouteVisitor {
    fn record(&mut self, kind: &'static str) {
        self.calls.push(RouteCall {
            file: self.file.clone(),
            owner: self.owner.join("::"),
            kind,
        });
    }

    fn classify_path(&self, path: &syn::Path) -> Option<&'static str> {
        let mut segments = path
            .segments
            .iter()
            .map(|segment| segment.ident.to_string())
            .collect::<Vec<_>>();
        if segments.len() == 1
            && let Some(canonical) = self.aliases.get(&segments[0])
        {
            segments = canonical.split("::").map(str::to_string).collect();
        }
        match segments.as_slice() {
            [.., function] if function == "derive_runtime_assignment_projection" => Some("issue"),
            [.., function] if function == "append_collected_actions" => {
                Some("finalize_root_actions")
            }
            [.., owner, function]
                if owner == "SolveProblem" && function == "construct_prepared" =>
            {
                Some("construct_prepared")
            }
            [.., owner, function] if owner == "SolveProblem" && function == "construct" => {
                Some("raw_construct")
            }
            [alias, function]
                if self
                    .aliases
                    .get(alias)
                    .is_some_and(|path| path.ends_with("::SolveProblem"))
                    && function == "construct_prepared" =>
            {
                Some("construct_prepared")
            }
            [alias, function]
                if self
                    .aliases
                    .get(alias)
                    .is_some_and(|path| path.ends_with("::SolveProblem"))
                    && function == "construct" =>
            {
                Some("raw_construct")
            }
            _ => None,
        }
    }
}

impl<'ast> Visit<'ast> for RouteVisitor {
    fn visit_item_fn(&mut self, item: &'ast syn::ItemFn) {
        if is_test_only(&item.attrs) {
            return;
        }
        self.owner.push(item.sig.ident.to_string());
        syn::visit::visit_item_fn(self, item);
        self.owner.pop();
    }

    fn visit_impl_item_fn(&mut self, item: &'ast syn::ImplItemFn) {
        if is_test_only(&item.attrs) {
            return;
        }
        self.owner.push(item.sig.ident.to_string());
        syn::visit::visit_impl_item_fn(self, item);
        self.owner.pop();
    }

    fn visit_item_mod(&mut self, item: &'ast syn::ItemMod) {
        if !is_test_only(&item.attrs) {
            syn::visit::visit_item_mod(self, item);
        }
    }

    fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
        if let syn::Expr::Path(path) = &*call.func
            && let Some(kind) = self.classify_path(&path.path)
        {
            self.record(kind);
        }
        syn::visit::visit_expr_call(self, call);
    }

    fn visit_expr_method_call(&mut self, call: &'ast syn::ExprMethodCall) {
        if call.method == "prepare"
            && matches!(&*call.receiver, syn::Expr::Path(path) if path.path.is_ident("runtime_projection"))
        {
            self.record("prepare");
        } else if call.method == "finish"
            && matches!(&*call.receiver, syn::Expr::Path(path) if path.path.is_ident("discrete"))
        {
            self.record("finish_discrete");
        }
        syn::visit::visit_expr_method_call(self, call);
    }
}

fn is_test_only(attributes: &[syn::Attribute]) -> bool {
    attributes.iter().any(|attribute| {
        attribute.path().is_ident("test")
            || (attribute.path().is_ident("cfg")
                && attribute
                    .meta
                    .to_token_stream()
                    .to_string()
                    .contains("test"))
    })
}

fn collect_aliases(
    tree: &syn::UseTree,
    prefix: &mut Vec<String>,
    out: &mut BTreeMap<String, String>,
) {
    match tree {
        syn::UseTree::Path(path) => {
            prefix.push(path.ident.to_string());
            collect_aliases(&path.tree, prefix, out);
            prefix.pop();
        }
        syn::UseTree::Name(name) => {
            let mut path = prefix.clone();
            path.push(name.ident.to_string());
            out.insert(name.ident.to_string(), path.join("::"));
        }
        syn::UseTree::Rename(rename) => {
            let mut path = prefix.clone();
            path.push(rename.ident.to_string());
            out.insert(rename.rename.to_string(), path.join("::"));
        }
        syn::UseTree::Group(group) => {
            for item in &group.items {
                collect_aliases(item, prefix, out);
            }
        }
        syn::UseTree::Glob(_) => {}
    }
}

fn census_source(file: &str, source: &str) -> Vec<RouteCall> {
    let mut calls = census_source_in_order(file, source);
    calls.sort();
    calls
}

fn census_source_in_order(file: &str, source: &str) -> Vec<RouteCall> {
    let syntax = syn::parse_file(source).unwrap_or_else(|error| panic!("parse {file}: {error}"));
    let mut aliases = BTreeMap::new();
    for item in &syntax.items {
        if let syn::Item::Use(item) = item {
            collect_aliases(&item.tree, &mut Vec::new(), &mut aliases);
        }
    }
    let mut visitor = RouteVisitor {
        file: file.to_string(),
        owner: Vec::new(),
        aliases,
        calls: Vec::new(),
    };
    visitor.visit_file(&syntax);
    visitor.calls
}

fn production_source(path: &Path) -> bool {
    path.file_name().is_some_and(|name| name != "tests.rs")
        && !path.components().any(|part| part.as_os_str() == "tests")
}

#[test]
fn solve_runtime_projection_has_one_syntax_owned_production_route() {
    let root = workspace_root().join("crates/rumoca-phase-solve/src");
    let mut files = Vec::new();
    collect_rs_files(&root, &mut files);
    let mut actual = Vec::new();
    for path in files.into_iter().filter(|path| production_source(path)) {
        let relative = path
            .strip_prefix(&root)
            .expect("collected source is below phase-solve src")
            .to_string_lossy()
            .replace('\\', "/");
        let source = fs::read_to_string(&path)
            .unwrap_or_else(|error| panic!("read {}: {error}", path.display()));
        actual.extend(census_source(&relative, &source));
    }
    actual.sort();
    assert_eq!(
        actual,
        vec![
            RouteCall {
                file: "lower.rs".into(),
                owner: "lower_solve_problem".into(),
                kind: "construct_prepared",
            },
            RouteCall {
                file: "lower.rs".into(),
                owner: "lower_solve_problem".into(),
                kind: "prepare",
            },
            RouteCall {
                file: "lower/events.rs".into(),
                owner: "finish".into(),
                kind: "issue",
            },
            RouteCall {
                file: "lower/events.rs".into(),
                owner: "lower_discrete_and_events".into(),
                kind: "finalize_root_actions",
            },
            RouteCall {
                file: "lower/events.rs".into(),
                owner: "lower_discrete_and_events".into(),
                kind: "finish_discrete",
            },
        ]
    );

    let events_source =
        fs::read_to_string(root.join("lower/events.rs")).expect("read final event-lowering source");
    let finalization_order = census_source_in_order("lower/events.rs", &events_source)
        .into_iter()
        .filter(|call| call.owner == "lower_discrete_and_events")
        .map(|call| call.kind)
        .collect::<Vec<_>>();
    assert_eq!(
        finalization_order,
        ["finalize_root_actions", "finish_discrete"],
        "call-scoped roots, including every None target, must finalize before projection minting"
    );
}

#[test]
fn route_census_detects_alias_wrappers_owner_swaps_and_test_cfgs() {
    let source = r#"
        use rumoca_ir_solve::derive_runtime_assignment_projection as mint;
        use rumoca_ir_solve::SolveProblem as Root;
        fn wrapper() { mint(); }
        fn wrong_owner() {
            runtime_projection.prepare();
            Root::construct_prepared();
            Root::construct();
        }
        #[cfg(test)] fn ignored() { mint(); }
    "#;
    let calls = census_source("sibling.rs", source);
    assert_eq!(
        calls
            .iter()
            .map(|call| (call.owner.as_str(), call.kind))
            .collect::<Vec<_>>(),
        [
            ("wrapper", "issue"),
            ("wrong_owner", "construct_prepared"),
            ("wrong_owner", "prepare"),
            ("wrong_owner", "raw_construct"),
        ]
    );
}
