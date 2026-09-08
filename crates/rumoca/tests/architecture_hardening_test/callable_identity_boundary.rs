//! Exact callable identity and builtin-variant drift backstops.
//!
//! SPEC_0001 makes rendered names presentation data after Resolve, and
//! SPEC_0043 §7 requires each effective function specialization to retain its
//! exact identity. These source checks are deliberately narrow tombstones for
//! the two removed implementation shapes; the evaluator mutation tests prove
//! their semantic consequences.

use quote::ToTokens;
use syn::visit::{self, Visit};

const USER_CALL_OWNERS: &[(&str, &str)] = &[
    (
        "crates/rumoca-eval-flat/src/constant/expr_eval.rs",
        "eval_fn_call",
    ),
    (
        "crates/rumoca-eval-flat/src/constant/function_eval.rs",
        "eval_fn_call_stmt",
    ),
    (
        "crates/rumoca-eval-flat/src/constant/function_eval/expression_eval.rs",
        "eval_fn_call_expr",
    ),
    (
        "crates/rumoca-eval-flat/src/constant/function_eval/validation.rs",
        "validate_static_call_shape",
    ),
];

const SPELLING_DISPATCH_TOKENS: &[&str] = &[
    "is_builtin",
    "from_name",
    "eval_builtin_in_context",
    "validate_builtin_arity",
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

fn spelling_dispatch_offenders(source: &str, owner: &str) -> Vec<String> {
    let syntax = syn::parse_file(source).expect("callable-owner source parses");
    let mut visitor = NamedBodyVisitor {
        target: owner,
        bodies: Vec::new(),
    };
    visitor.visit_file(&syntax);
    assert_eq!(
        visitor.bodies.len(),
        1,
        "callable owner `{owner}` must remain unique"
    );
    SPELLING_DISPATCH_TOKENS
        .iter()
        .filter(|token| visitor.bodies[0].contains(*token))
        .map(|token| (*token).to_string())
        .collect()
}

fn rendered_key_offenders(source: &str) -> Vec<&'static str> {
    ["EvalIndexMap<Function>", "names_by_instance"]
        .into_iter()
        .filter(|token| source.contains(token))
        .collect()
}

#[test]
fn function_call_evaluation_never_dispatches_builtins_by_spelling() {
    let root = super::workspace_root();
    let mut offenders = Vec::new();
    for (relative, owner) in USER_CALL_OWNERS {
        let source = std::fs::read_to_string(root.join(relative)).expect("read callable owner");
        for token in spelling_dispatch_offenders(&source, owner) {
            offenders.push(format!("{relative}::{owner}:{token}"));
        }
    }
    assert!(
        offenders.is_empty(),
        "Expression::FunctionCall and statement user calls require exact function identity; only Expression::BuiltinCall may select builtin expression semantics: {offenders:#?}"
    );
}

#[test]
fn finalized_callable_catalog_has_no_rendered_name_key() {
    let source = std::fs::read_to_string(
        super::workspace_root().join("crates/rumoca-eval-flat/src/constant/context.rs"),
    )
    .expect("read callable catalog");
    let offenders = rendered_key_offenders(&source);
    assert!(
        offenders.is_empty(),
        "post-Resolve callable catalogs must key effective specializations by FunctionInstanceId, never rendered names: {offenders:?}"
    );
    assert!(
        source.contains("IndexMap<FunctionInstanceId, Function"),
        "the finalized callable authority must visibly retain FunctionInstanceId as its key"
    );
}

#[test]
fn callable_identity_tombstones_detect_mutations() {
    let spelling_mutation = r#"
        fn eval_fn_call() {
            if is_builtin(name.as_str()) {
                eval_builtin_in_context(name.as_str(), args);
            }
        }
    "#;
    assert_eq!(
        spelling_dispatch_offenders(spelling_mutation, "eval_fn_call"),
        ["is_builtin", "eval_builtin_in_context"]
    );

    let rendered_key_mutation =
        "struct Catalog { functions: EvalIndexMap<Function>, names_by_instance: Map }";
    assert_eq!(
        rendered_key_offenders(rendered_key_mutation),
        ["EvalIndexMap<Function>", "names_by_instance"]
    );
}
