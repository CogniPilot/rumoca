//! Production-module reachability and syntax-aware Rust boundary analysis.

mod analyzer;
mod registry;

const CODEGEN_MODULE: &str = "crates/rumoca-phase-codegen/src/codegen/mod.rs";
const MODELICA_CODEC_FILTER: &str = "modelica_string_escape_filter";

/// Recognize the sole lexical-codec exception from SPEC_0029 section 12.
///
/// Every part of the owner, signature, and one-expression body is exact. A
/// branch, intermediate statement, wrapper operation, alternate path, or
/// syntax token therefore stops matching and re-enters the ordinary generated
/// text debt gate.
fn is_exact_modelica_codec_delegate(
    file: &str,
    owner: &str,
    signature: &syn::Signature,
    body: &syn::Block,
) -> bool {
    if file != CODEGEN_MODULE
        || owner != MODELICA_CODEC_FILTER
        || signature.ident != MODELICA_CODEC_FILTER
        || signature.constness.is_some()
        || signature.asyncness.is_some()
        || signature.unsafety.is_some()
        || signature.abi.is_some()
        || signature.variadic.is_some()
        || !signature.generics.params.is_empty()
        || signature.generics.where_clause.is_some()
        || signature.inputs.len() != 1
        || !matches!(
            &signature.output,
            syn::ReturnType::Type(_, ty) if type_is_exact_path(ty, &["String"])
        )
    {
        return false;
    }

    let Some(syn::FnArg::Typed(argument)) = signature.inputs.first() else {
        return false;
    };
    let syn::Pat::Ident(pattern) = argument.pat.as_ref() else {
        return false;
    };
    if pattern.ident != "value"
        || pattern.by_ref.is_some()
        || pattern.mutability.is_some()
        || pattern.subpat.is_some()
        || !type_is_exact_str_ref(&argument.ty)
        || body.stmts.len() != 1
    {
        return false;
    }

    let Some(syn::Stmt::Expr(syn::Expr::Call(call), None)) = body.stmts.first() else {
        return false;
    };
    let syn::Expr::Path(function) = call.func.as_ref() else {
        return false;
    };
    if !path_is_exact_external(&function.path, &["rumoca_core", "escape_modelica_string"])
        || call.args.len() != 1
    {
        return false;
    }
    matches!(call.args.first(), Some(syn::Expr::Path(value))
        if path_is_exact(&value.path, &["value"]))
}

fn type_is_exact_path(ty: &syn::Type, expected: &[&str]) -> bool {
    matches!(ty, syn::Type::Path(path) if path.qself.is_none() && path_is_exact(&path.path, expected))
}

fn type_is_exact_str_ref(ty: &syn::Type) -> bool {
    matches!(ty, syn::Type::Reference(reference)
        if reference.lifetime.is_none()
            && reference.mutability.is_none()
            && type_is_exact_path(&reference.elem, &["str"]))
}

fn path_is_exact(path: &syn::Path, expected: &[&str]) -> bool {
    path.leading_colon.is_none() && path_segments_are_exact(path, expected)
}

fn path_is_exact_external(path: &syn::Path, expected: &[&str]) -> bool {
    path.leading_colon.is_some() && path_segments_are_exact(path, expected)
}

fn path_segments_are_exact(path: &syn::Path, expected: &[&str]) -> bool {
    path.segments.len() == expected.len()
        && path
            .segments
            .iter()
            .zip(expected)
            .all(|(segment, expected)| {
                segment.ident == *expected && matches!(segment.arguments, syn::PathArguments::None)
            })
}

pub(super) use super::super::architecture_hardening_support::{
    ProductionRustSourceContext, production_rust_source_contexts, production_rust_sources,
    production_rust_target_roots,
};

pub(super) fn analyze_sources(
    sources: &[(std::path::PathBuf, String)],
) -> std::collections::BTreeSet<String> {
    let mut findings = analyzer::analyze_sources(sources);
    findings.extend(registry::analyze_registry(sources));
    findings
}

pub(super) fn analyze_source_contexts(
    contexts: &[ProductionRustSourceContext],
) -> std::collections::BTreeSet<String> {
    let mut findings = analyzer::analyze_source_contexts(contexts);
    let sources = contexts
        .iter()
        .map(|context| {
            (
                context.canonical_path.clone(),
                (context.path.clone(), context.source.clone()),
            )
        })
        .collect::<std::collections::BTreeMap<_, _>>()
        .into_values()
        .collect::<Vec<_>>();
    findings.extend(registry::analyze_registry(&sources));
    findings
}

pub(super) fn analyze_registry_usage(
    sources: &[(std::path::PathBuf, String)],
    templates: &[(String, String)],
) -> std::collections::BTreeSet<String> {
    registry::analyze_registry_usage(sources, templates)
}

pub(super) fn production_source_set_finding(
    manifest: &str,
    roots: &[(String, std::path::PathBuf)],
    sources: &[(std::path::PathBuf, String)],
) -> String {
    let mut ordered_roots = roots.iter().collect::<Vec<_>>();
    ordered_roots.sort();
    let mut ordered = sources.iter().collect::<Vec<_>>();
    ordered.sort_by(|left, right| left.0.cmp(&right.0));
    let mut hasher = blake3::Hasher::new();
    hasher.update(&(manifest.len() as u64).to_le_bytes());
    hasher.update(manifest.as_bytes());
    for (kind, path) in &ordered_roots {
        hasher.update(&(kind.len() as u64).to_le_bytes());
        hasher.update(kind.as_bytes());
        let path = path.to_string_lossy();
        hasher.update(&(path.len() as u64).to_le_bytes());
        hasher.update(path.as_bytes());
    }
    for (path, source) in &ordered {
        let path = path.to_string_lossy();
        hasher.update(&(path.len() as u64).to_le_bytes());
        hasher.update(path.as_bytes());
        hasher.update(&(source.len() as u64).to_le_bytes());
        hasher.update(source.as_bytes());
    }
    format!(
        "<phase-codegen>::<production-rust-set>:source-review:manifest-1:roots-{}:modules-{}:{}",
        ordered_roots.len(),
        ordered.len(),
        hasher.finalize().to_hex()
    )
}
