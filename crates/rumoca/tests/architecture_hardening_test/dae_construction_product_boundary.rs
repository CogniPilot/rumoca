//! Tombstones the duplicate Flat-analysis route at the public ToDAE boundary.

use syn::visit::{self, Visit};

use super::workspace_root;

struct NamedCallCounter<'name> {
    name: &'name str,
    calls: usize,
}

struct NamedPathCounter<'name> {
    name: &'name str,
    paths: usize,
}

impl<'ast> Visit<'ast> for NamedPathCounter<'_> {
    fn visit_expr_path(&mut self, path: &'ast syn::ExprPath) {
        if path
            .path
            .segments
            .last()
            .is_some_and(|segment| segment.ident == self.name)
        {
            self.paths += 1;
        }
        visit::visit_expr_path(self, path);
    }
}

struct NamedMethodCallCounter<'name> {
    name: &'name str,
    calls: usize,
}

impl<'ast> Visit<'ast> for NamedMethodCallCounter<'_> {
    fn visit_expr_method_call(&mut self, call: &'ast syn::ExprMethodCall) {
        if call.method == self.name {
            self.calls += 1;
        }
        visit::visit_expr_method_call(self, call);
    }
}

impl<'ast> Visit<'ast> for NamedCallCounter<'_> {
    fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
        if matches!(
            call.func.as_ref(),
            syn::Expr::Path(path)
                if path.path.segments.last().is_some_and(|segment| segment.ident == self.name)
        ) {
            self.calls += 1;
        }
        visit::visit_expr_call(self, call);
    }
}

fn named_call_count(source: &str, name: &str) -> usize {
    let syntax = syn::parse_file(source).expect("Flat-to-DAE boundary source parses");
    let mut counter = NamedCallCounter { name, calls: 0 };
    counter.visit_file(&syntax);
    counter.calls
}

fn named_path_count(source: &str, name: &str) -> usize {
    let syntax = syn::parse_file(source).expect("Flat-to-DAE boundary source parses");
    let mut counter = NamedPathCounter { name, paths: 0 };
    counter.visit_file(&syntax);
    counter.paths
}

fn named_method_call_count(source: &str, name: &str) -> usize {
    let syntax = syn::parse_file(source).expect("Flat-to-DAE boundary source parses");
    let mut counter = NamedMethodCallCounter { name, calls: 0 };
    counter.visit_file(&syntax);
    counter.calls
}

fn named_call_count_in_function(source: &str, function_name: &str, call_name: &str) -> usize {
    let syntax = syn::parse_file(source).expect("Flat-to-DAE boundary source parses");
    let function = syntax
        .items
        .iter()
        .find_map(|item| match item {
            syn::Item::Fn(function) if function.sig.ident == function_name => Some(function),
            _ => None,
        })
        .unwrap_or_else(|| panic!("missing `{function_name}` boundary function"));
    let mut counter = NamedCallCounter {
        name: call_name,
        calls: 0,
    };
    counter.visit_block(&function.block);
    counter.calls
}

fn public_free_function_names(source: &str) -> Vec<String> {
    let syntax = syn::parse_file(source).expect("phase-DAE public source parses");
    syntax
        .items
        .into_iter()
        .filter_map(|item| match item {
            syn::Item::Fn(function) if matches!(function.vis, syn::Visibility::Public(_)) => {
                Some(function.sig.ident.to_string())
            }
            _ => None,
        })
        .collect()
}

fn collect_use_leaf_names(tree: &syn::UseTree, names: &mut Vec<String>) {
    match tree {
        syn::UseTree::Path(path) => collect_use_leaf_names(&path.tree, names),
        syn::UseTree::Name(name) => names.push(name.ident.to_string()),
        syn::UseTree::Rename(rename) => names.push(rename.rename.to_string()),
        syn::UseTree::Group(group) => {
            for item in &group.items {
                collect_use_leaf_names(item, names);
            }
        }
        syn::UseTree::Glob(_) => names.push("*".to_owned()),
    }
}

fn public_use_leaf_names(source: &str) -> Vec<String> {
    let syntax = syn::parse_file(source).expect("phase-DAE public source parses");
    let mut names = Vec::new();
    for item in syntax.items {
        if let syn::Item::Use(item) = item
            && matches!(item.vis, syn::Visibility::Public(_))
        {
            collect_use_leaf_names(&item.tree, &mut names);
        }
    }
    names.sort();
    names
}

fn item_macro_count(source: &str) -> usize {
    let syntax = syn::parse_file(source).expect("phase-DAE public source parses");
    syntax
        .items
        .iter()
        .filter(|item| matches!(item, syn::Item::Macro(_)))
        .count()
}

fn construction_product_field_names(source: &str) -> Vec<String> {
    let syntax = syn::parse_file(source).expect("phase-DAE product source parses");
    let product = syntax
        .items
        .into_iter()
        .find_map(|item| match item {
            syn::Item::Struct(item) if item.ident == "DaeConstructionProduct" => Some(item),
            _ => None,
        })
        .expect("DaeConstructionProduct remains the public phase aggregate");
    product
        .fields
        .iter()
        .map(|field| {
            assert!(
                matches!(field.vis, syn::Visibility::Inherited),
                "DaeConstructionProduct fields must remain private"
            );
            field
                .ident
                .as_ref()
                .expect("construction-product fields are named")
                .to_string()
        })
        .collect()
}

fn public_construction_product_methods(source: &str) -> Vec<String> {
    let syntax = syn::parse_file(source).expect("phase-DAE product source parses");
    syntax
        .items
        .into_iter()
        .filter_map(|item| match item {
            syn::Item::Impl(item)
                if matches!(
                    item.self_ty.as_ref(),
                    syn::Type::Path(path)
                        if path.path.segments.last().is_some_and(|segment| {
                            segment.ident == "DaeConstructionProduct"
                        })
                ) =>
            {
                Some(item)
            }
            _ => None,
        })
        .flat_map(|item| item.items.into_iter())
        .filter_map(|item| match item {
            syn::ImplItem::Fn(function) if matches!(function.vis, syn::Visibility::Public(_)) => {
                Some(function.sig.ident.to_string())
            }
            _ => None,
        })
        .collect()
}

#[test]
fn phase_dae_has_one_public_product_route_and_one_flat_analysis() {
    let root = workspace_root();
    let public_source = std::fs::read_to_string(root.join("crates/rumoca-phase-dae/src/lib.rs"))
        .expect("read phase-DAE public boundary");
    let balance_source =
        std::fs::read_to_string(root.join("crates/rumoca-phase-dae/src/balance.rs"))
            .expect("read phase-DAE public balance module");
    let construction_source =
        std::fs::read_to_string(root.join("crates/rumoca-phase-dae/src/construction.rs"))
            .expect("read phase-DAE construction boundary");
    let compile_source =
        std::fs::read_to_string(root.join("crates/rumoca-compile/src/session/compile_support.rs"))
            .expect("read compiler ToDAE consumer");

    assert_eq!(public_free_function_names(&public_source), ["construct"]);
    assert_eq!(
        public_use_leaf_names(&public_source),
        [
            "BalanceBreakdown",
            "BalanceDetail",
            "ToDaeError",
            "ToDaeResult"
        ]
    );
    assert_eq!(item_macro_count(&public_source), 0);
    assert!(public_free_function_names(&balance_source).is_empty());
    assert_eq!(item_macro_count(&balance_source), 0);
    assert_eq!(
        construction_product_field_names(&public_source),
        ["dae", "balance_detail"]
    );
    assert_eq!(
        public_construction_product_methods(&public_source),
        ["dae", "balance_detail", "into_parts"]
    );
    assert_eq!(named_call_count(&construction_source, "analyze"), 1);
    assert_eq!(named_path_count(&construction_source, "analyze"), 1);
    assert_eq!(
        named_call_count_in_function(&construction_source, "construct", "analyze"),
        1
    );
    assert_eq!(named_call_count(&compile_source, "construct_dae"), 1);
    assert_eq!(named_method_call_count(&compile_source, "into_parts"), 1);
}

#[test]
fn single_pass_tombstone_detects_duplicate_analysis_and_public_balance_mutations() {
    let one_pass = "fn construct(flat: &Flat) { analyze(flat); }";
    let duplicate = "fn construct(flat: &Flat) { analyze(flat); analyze(flat); }";
    let aliased = "fn construct(flat: &Flat) { let second = analyze; second(flat); }";
    assert_eq!(named_call_count(one_pass, "analyze"), 1);
    assert_eq!(named_call_count(duplicate, "analyze"), 2);
    assert_eq!(named_call_count(aliased, "analyze"), 0);
    assert_eq!(named_path_count(aliased, "analyze"), 1);

    let sole_route = "pub fn construct() {}";
    let separate_balance = "pub fn construct() {} pub fn balance_detail() {}";
    assert_eq!(public_free_function_names(sole_route), ["construct"]);
    assert_eq!(
        public_free_function_names(separate_balance),
        ["construct", "balance_detail"]
    );
    assert_eq!(
        public_use_leaf_names("pub use analysis::balance_detail as inspect_balance;"),
        ["inspect_balance"]
    );

    let sole_extraction = r#"
        pub struct DaeConstructionProduct { dae: (), balance_detail: () }
        impl DaeConstructionProduct { pub fn into_parts(self) {} }
    "#;
    let dae_only_extraction = r#"
        pub struct DaeConstructionProduct { dae: (), balance_detail: () }
        impl DaeConstructionProduct {
            pub fn into_parts(self) {}
            pub fn into_dae(self) {}
        }
    "#;
    assert_eq!(
        public_construction_product_methods(sole_extraction),
        ["into_parts"]
    );
    assert_eq!(
        public_construction_product_methods(dae_only_extraction),
        ["into_parts", "into_dae"]
    );
}
