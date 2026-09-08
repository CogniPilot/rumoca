use quote::ToTokens;
use std::fs;
use std::path::{Path, PathBuf};

use crate::architecture_hardening_support::{collect_rs_files, workspace_root};

fn normalized_tokens(tokens: impl ToTokens) -> String {
    tokens
        .into_token_stream()
        .to_string()
        .chars()
        .filter(|character| !character.is_whitespace())
        .collect()
}

fn is_test_module(item: &syn::Item) -> bool {
    let syn::Item::Mod(module) = item else {
        return false;
    };
    module
        .attrs
        .iter()
        .any(|attribute| normalized_tokens(attribute) == "#[cfg(test)]")
}

fn production_constructor_count(source: &str) -> usize {
    let file = syn::parse_file(source).expect("architecture fixture must parse as Rust");
    file.items
        .into_iter()
        .filter(|item| !is_test_module(item))
        .map(normalized_tokens)
        .map(|tokens| tokens.matches("ClassDefIndex::from_tree").count())
        .sum()
}

struct TestModuleSource {
    file: PathBuf,
    descendant_root: PathBuf,
}

fn module_directory(source: &Path) -> PathBuf {
    match source.file_name().and_then(|name| name.to_str()) {
        Some("lib.rs" | "main.rs" | "mod.rs") => source
            .parent()
            .expect("Rust module source has a parent")
            .to_path_buf(),
        Some(_) => source.with_extension(""),
        None => panic!("Rust module source has a file name"),
    }
}

fn test_module_sources(paths: &[PathBuf]) -> Vec<TestModuleSource> {
    let mut sources = Vec::new();
    for path in paths {
        let source = fs::read_to_string(path).expect("read Instantiate module source");
        let file = syn::parse_file(&source).expect("Instantiate module source must parse");
        for module in file.items.iter().filter_map(|item| match item {
            syn::Item::Mod(module) if module.content.is_none() && is_test_module(item) => {
                Some(module)
            }
            _ => None,
        }) {
            let directory = module_directory(path);
            let flat = directory.join(format!("{}.rs", module.ident));
            let nested = directory.join(module.ident.to_string()).join("mod.rs");
            let (file, descendant_root) = match (flat.is_file(), nested.is_file()) {
                (true, false) => (flat.clone(), flat.with_extension("")),
                (false, true) => (
                    nested.clone(),
                    nested
                        .parent()
                        .expect("nested module source has a parent")
                        .to_path_buf(),
                ),
                (false, false) => panic!(
                    "cfg(test) module `{}` declared by {} has no source",
                    module.ident,
                    path.display()
                ),
                (true, true) => panic!(
                    "cfg(test) module `{}` declared by {} has ambiguous sources",
                    module.ident,
                    path.display()
                ),
            };
            sources.push(TestModuleSource {
                file,
                descendant_root,
            });
        }
    }
    sources
}

fn is_test_file(path: &Path, test_modules: &[TestModuleSource]) -> bool {
    test_modules
        .iter()
        .any(|module| path == module.file.as_path() || path.starts_with(&module.descendant_root))
}

#[test]
fn resolved_identity_index_is_issued_once_per_instantiation_root() {
    let root = workspace_root();
    let source_root = root.join("crates/rumoca-phase-instantiate/src");
    let mut paths = Vec::new();
    collect_rs_files(&source_root, &mut paths);
    let test_modules = test_module_sources(&paths);
    let constructors = paths
        .into_iter()
        .filter(|path| !is_test_file(path, &test_modules))
        .filter_map(|path| {
            let source = fs::read_to_string(&path).expect("read Instantiate production source");
            let count = production_constructor_count(&source);
            (count != 0).then(|| {
                (
                    path.strip_prefix(&root)
                        .expect("Instantiate path is under workspace")
                        .display()
                        .to_string(),
                    count,
                )
            })
        })
        .collect::<Vec<_>>();
    assert_eq!(
        constructors,
        vec![(
            "crates/rumoca-phase-instantiate/src/entry.rs".to_string(),
            1
        )],
        "ClassDefIndex must be minted exactly once at the instantiation root and borrowed by every hot consumer"
    );

    let shape = fs::read_to_string(
        root.join("crates/rumoca-eval-ast/src/eval_instantiate/function_eval/shape.rs"),
    )
    .expect("read shape evaluator");
    for retired_wrapper in [
        "pub fn evaluate_array_dimensions(",
        "pub fn try_eval_integer_shape_expr(",
    ] {
        assert!(
            !shape.contains(retired_wrapper),
            "shape evaluation recreated a per-call index constructor: {retired_wrapper}"
        );
    }
}

#[test]
fn constructor_inventory_ignores_tests_but_detects_every_production_item() {
    let fixture = r#"
        fn root(tree: &ClassTree) { ClassDefIndex::from_tree(tree); }
        fn bypass(tree: &ClassTree) { ClassDefIndex::from_tree(tree); }
        #[cfg(test)] mod tests {
            fn fixture(tree: &ClassTree) { ClassDefIndex::from_tree(tree); }
        }
        #[cfg(any(test, feature = "runtime"))] mod runtime {
            fn feature_path(tree: &ClassTree) { ClassDefIndex::from_tree(tree); }
        }
    "#;
    assert_eq!(production_constructor_count(fixture), 3);
}
