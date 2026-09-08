use std::fs;

use syn::visit::{self, Visit};
use syn::{ExprCall, ImplItemFn, Item, ItemFn, ItemMod, Signature, Visibility};

use super::architecture_hardening_support::{
    attributes_require_test, production_rust_sources, workspace_root,
};

const TRACE_OWNER: &str = "crates/rumoca-ir-galec/src/traced_product.rs";
const LOWERING_OWNER: &str = "crates/rumoca-phase-galec/src/lower.rs";

#[test]
fn origin_candidate_and_issuer_are_affine_private_construction_authority() {
    let source = fs::read_to_string(workspace_root().join(TRACE_OWNER))
        .expect("read Algorithm Code trace owner");
    let syntax = syn::parse_file(&source).expect("parse Algorithm Code trace owner");

    for name in [
        "AlgorithmCodePackageIssuer",
        "OriginBoundAlgorithmCodePackage",
        "TracedAlgorithmCodeProduct",
    ] {
        let item = syntax
            .items
            .iter()
            .find_map(|item| match item {
                Item::Struct(item) if item.ident == name => Some(item),
                _ => None,
            })
            .unwrap_or_else(|| panic!("missing {name}"));
        assert!(
            item.fields
                .iter()
                .all(|field| matches!(field.vis, Visibility::Inherited)),
            "{name} fields must remain private"
        );
        let derives = item
            .attrs
            .iter()
            .filter(|attribute| attribute.path().is_ident("derive"))
            .map(|attribute| quote::quote!(#attribute).to_string())
            .collect::<Vec<_>>()
            .join(" ");
        for forbidden in ["Clone", "Copy", "Default", "Serialize", "Deserialize"] {
            assert!(
                !derives
                    .split(|character: char| !character.is_ascii_alphanumeric())
                    .any(|word| word == forbidden),
                "{name} must remain affine and non-wire: {derives}"
            );
        }
        for item in &syntax.items {
            let Item::Impl(implementation) = item else {
                continue;
            };
            let self_ty = &implementation.self_ty;
            let self_type = quote::quote!(#self_ty).to_string();
            let Some((_, trait_path, _)) = &implementation.trait_ else {
                continue;
            };
            let implemented = trait_path
                .segments
                .last()
                .map(|segment| segment.ident.to_string())
                .unwrap_or_default();
            assert!(
                !self_type.contains(name)
                    || !["Clone", "Copy", "Default", "Serialize", "Deserialize"]
                        .contains(&implemented.as_str()),
                "{name} must not manually implement affine/wire-forbidden trait {implemented}"
            );
        }
    }

    assert!(
        source.contains("pub fn construct(\n        self,"),
        "the origin issuer must consume itself while closing Block + Metadata"
    );
    for forbidden in [
        "bind_lowering_origin",
        "OriginBoundAlgorithmCodePackage::from",
        "fn into_parts",
        "fn into_package",
    ] {
        assert!(
            !source.contains(forbidden),
            "raw package admission/extraction route reappeared: {forbidden}"
        );
    }
}

#[test]
fn phase_galec_is_the_sole_production_origin_session_and_uses_its_dae_map() {
    let root = workspace_root();
    let mut occurrences = Vec::new();
    for entry in fs::read_dir(root.join("crates")).expect("read workspace crates") {
        let crate_root = entry.expect("crate entry").path();
        if !crate_root.join("Cargo.toml").is_file() {
            continue;
        }
        for (path, source) in production_rust_sources(&crate_root, &root) {
            let syntax = syn::parse_file(&source)
                .unwrap_or_else(|error| panic!("parse {}: {error}", path.display()));
            let mut visitor = OriginProjectionCallVisitor {
                path: &path,
                occurrences: &mut occurrences,
            };
            visitor.visit_file(&syntax);
        }
    }
    assert_eq!(
        occurrences.len(),
        1,
        "the phase-galec lowering owner must be the sole production origin session: {occurrences:?}"
    );
    assert!(
        occurrences[0].starts_with(LOWERING_OWNER),
        "the sole origin session must belong to DAE -> GALEC lowering: {occurrences:?}"
    );

    let lowering = fs::read_to_string(root.join(LOWERING_OWNER)).expect("read GALEC lowering");
    let start = lowering
        .find("TracedAlgorithmCodeProduct::project_from_origin(")
        .expect("lowering starts one origin session");
    let origin_call = &lowering[start..];
    let source_map = origin_call
        .find("input.dae.source_map()")
        .expect("origin session snapshots the exact input DAE source map");
    let inspection = origin_call
        .find("input.dae.inspect(|view|")
        .expect("whole DAE inspection runs inside the origin session");
    assert!(
        source_map < inspection,
        "the exact DAE map must be selected before semantic inspection/lowering"
    );
    assert!(
        !function_signature(&lowering, "pub fn lower_to_algorithm_code").contains("SourceMap"),
        "the public lowering API must not admit a caller-selected source map"
    );

    // This sole-caller gate is load-bearing for map <-> Block correspondence:
    // the origin brand proves candidate/session identity, not arbitrary closure
    // semantics. The distinction is intentional and must stay explicit.
}

struct OriginProjectionCallVisitor<'a> {
    path: &'a std::path::Path,
    occurrences: &'a mut Vec<String>,
}

impl<'ast> Visit<'ast> for OriginProjectionCallVisitor<'_> {
    fn visit_expr_call(&mut self, call: &'ast ExprCall) {
        if let syn::Expr::Path(function) = call.func.as_ref()
            && function
                .path
                .segments
                .last()
                .is_some_and(|segment| segment.ident == "project_from_origin")
        {
            self.occurrences.push(self.path.display().to_string());
        }
        visit::visit_expr_call(self, call);
    }

    fn visit_item_fn(&mut self, function: &'ast ItemFn) {
        if !attributes_require_test(&function.attrs) {
            visit::visit_item_fn(self, function);
        }
    }

    fn visit_item_mod(&mut self, module: &'ast ItemMod) {
        if !attributes_require_test(&module.attrs) {
            visit::visit_item_mod(self, module);
        }
    }
}

#[test]
fn render_surfaces_cannot_replace_or_fabricate_trace_authority() {
    let root = workspace_root();
    for relative in [
        "crates/rumoca-phase-codegen/src/codegen/algorithm_code_renderer.rs",
        "crates/rumoca-phase-codegen/src/codegen/solve_algorithm_production_renderer.rs",
        "crates/rumoca-phase-codegen/src/codegen/solve_algorithm_production_renderer/production_code_manifest_view.rs",
        "crates/rumoca-phase-codegen/src/template_file.rs",
        "crates/rumoca-phase-codegen/src/rendered_template_file.rs",
        "crates/rumoca-phase-codegen/src/views/algorithm_code.rs",
        "crates/rumoca-phase-codegen/src/views/solve_algorithm_production.rs",
        "crates/rumoca-phase-codegen/src/views/solve_algorithm_production/presentation.rs",
        "crates/rumoca-phase-solve/src/algorithm.rs",
    ] {
        let source = fs::read_to_string(root.join(relative))
            .unwrap_or_else(|error| panic!("read {relative}: {error}"));
        let syntax =
            syn::parse_file(&source).unwrap_or_else(|error| panic!("parse {relative}: {error}"));
        let mut visitor = TraceSurfaceVisitor {
            relative,
            violations: Vec::new(),
        };
        visitor.visit_file(&syntax);
        assert!(
            visitor.violations.is_empty(),
            "traced Solve/preparation/rendering surfaces must borrow trace/model authority only from their prepared product: {:?}",
            visitor.violations
        );
    }
}

struct TraceSurfaceVisitor<'a> {
    relative: &'a str,
    violations: Vec<String>,
}

impl TraceSurfaceVisitor<'_> {
    fn inspect_signature(&mut self, signature: &Signature) {
        for input in &signature.inputs {
            let syn::FnArg::Typed(input) = input else {
                continue;
            };
            let parameter = quote::quote!(#input).to_string();
            let ty = &input.ty;
            let ty = quote::quote!(#ty).to_string();
            let admits_raw_model_name = ["model_name", "semantic_model", "source_model_name"]
                .iter()
                .any(|name| parameter.split_whitespace().any(|token| token == *name))
                && (ty.contains("str") || ty.contains("String"));
            if ty.contains("SourceMap") || admits_raw_model_name {
                self.violations.push(format!(
                    "{}: {} admits raw trace/model input `{parameter}`",
                    self.relative, signature.ident
                ));
            }
        }
    }
}

impl<'ast> Visit<'ast> for TraceSurfaceVisitor<'_> {
    fn visit_item_fn(&mut self, function: &'ast ItemFn) {
        if !attributes_require_test(&function.attrs) {
            self.inspect_signature(&function.sig);
            visit::visit_item_fn(self, function);
        }
    }

    fn visit_impl_item_fn(&mut self, function: &'ast ImplItemFn) {
        if !attributes_require_test(&function.attrs) {
            self.inspect_signature(&function.sig);
            visit::visit_impl_item_fn(self, function);
        }
    }

    fn visit_item_mod(&mut self, module: &'ast ItemMod) {
        if !attributes_require_test(&module.attrs) {
            visit::visit_item_mod(self, module);
        }
    }
}

#[test]
fn algorithm_code_and_production_layouts_retain_target_issued_order() {
    let root = workspace_root();
    for relative in [
        "crates/rumoca-phase-codegen/src/views/algorithm_code_artifact_layout.rs",
        "crates/rumoca-phase-codegen/src/views/solve_algorithm_production/artifact_layout.rs",
    ] {
        let source = fs::read_to_string(root.join(relative))
            .unwrap_or_else(|error| panic!("read {relative}: {error}"));
        let production = source.split("#[cfg(test)]").next().unwrap_or(&source);
        assert!(
            !production.contains("members.sort")
                && !production.contains("members.sort_by")
                && !production.contains("members.sort_unstable"),
            "checked artifact layouts must never replace target-issued order: {relative}"
        );
    }
    let production = fs::read_to_string(root.join(
        "crates/rumoca-phase-codegen/src/views/solve_algorithm_production/artifact_layout.rs",
    ))
    .expect("read production layout");
    assert!(
        production.contains("ProductionArtifactLayoutError::NonSchemaAfterSchema"),
        "Production layout must prove schema members form the target-issued trailing suffix"
    );
}

fn function_signature<'a>(source: &'a str, prefix: &str) -> &'a str {
    let start = source
        .find(prefix)
        .unwrap_or_else(|| panic!("missing {prefix}"));
    let tail = &source[start..];
    let end = tail
        .find('{')
        .unwrap_or_else(|| panic!("unterminated function signature {prefix}"));
    &tail[..end]
}
