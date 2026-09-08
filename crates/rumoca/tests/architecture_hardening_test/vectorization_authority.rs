//! Automatic-vectorization authority ownership and consumer drift backstops.

use quote::ToTokens;
use syn::visit::{self, Visit};

const CONSUMERS: &[(&str, &str)] = &[
    (
        "crates/rumoca-eval-flat/src/constant/expr_eval.rs",
        "vectorization_authority",
    ),
    (
        "crates/rumoca-ir-flat/src/wire/semantics.rs",
        "require_automatic_vectorization_authority",
    ),
    (
        "crates/rumoca-phase-dae/src/construction/function_shapes/mod.rs",
        "require_exact_vectorization_owner",
    ),
];

struct NamedBodyVisitor<'a> {
    target: &'a str,
    bodies: Vec<String>,
}

impl<'ast> Visit<'ast> for NamedBodyVisitor<'_> {
    fn visit_item_fn(&mut self, node: &'ast syn::ItemFn) {
        if node.sig.ident == self.target {
            self.bodies.push(node.block.to_token_stream().to_string());
        }
        visit::visit_item_fn(self, node);
    }

    fn visit_impl_item_fn(&mut self, node: &'ast syn::ImplItemFn) {
        if node.sig.ident == self.target {
            self.bodies.push(node.block.to_token_stream().to_string());
        }
        visit::visit_impl_item_fn(self, node);
    }
}

fn owner_uses_only_shared_authority(source: &str, owner: &str) -> bool {
    let syntax = syn::parse_file(source).expect("vectorization-authority source parses");
    let mut visitor = NamedBodyVisitor {
        target: owner,
        bodies: Vec::new(),
    };
    visitor.visit_file(&syntax);
    assert_eq!(
        visitor.bodies.len(),
        1,
        "vectorization authority consumer `{owner}` must remain unique"
    );
    let body = &visitor.bodies[0];
    body.contains("automatic_vectorization_authority")
        && !body.contains("transitively_non_replaceable")
        && !body.contains("instance_id ==")
        && !body.contains("instance_id !=")
}

#[test]
fn automatic_vectorization_consumers_use_the_core_authority() {
    let root = super::workspace_root();
    for (relative, owner) in CONSUMERS {
        let source = std::fs::read_to_string(root.join(relative))
            .unwrap_or_else(|error| panic!("read {relative}: {error}"));
        assert!(
            owner_uses_only_shared_authority(&source, owner),
            "{relative}::{owner} must consume rumoca-core's exact automatic-vectorization authority without restating its Boolean/identity conjunction"
        );
    }
}

#[test]
fn automatic_vectorization_authority_gate_detects_reassembled_mutations() {
    for mutation in [
        r#"
            fn vectorization_authority() -> bool {
                resolved.transitively_non_replaceable
                    && function.transitively_non_replaceable
                    && function.instance_id == Some(resolved.instance_id)
            }
        "#,
        r#"
            fn require_exact_vectorization_owner() -> bool {
                function.automatic_vectorization_authority(resolved).is_ok()
                    && function.instance_id != Some(foreign.instance_id)
            }
        "#,
    ] {
        let owner = if mutation.contains("fn vectorization_authority") {
            "vectorization_authority"
        } else {
            "require_exact_vectorization_owner"
        };
        assert!(!owner_uses_only_shared_authority(mutation, owner));
    }
}
