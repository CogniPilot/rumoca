use std::fs;
use std::path::Path;

use syn::{Attribute, Item};

use super::architecture_hardening_support::{collect_rs_files, workspace_root};

const CLI_ENTRY: &str = "crates/rumoca/src/cli/artifact_session_input.rs";
const PYTHON_ENTRY: &str = "crates/rumoca-bind-python/src/lib.rs";
const SESSION_OWNER: &str =
    "crates/rumoca-compile/src/codegen_target/checked_plan/target_artifact.rs";
const RENDER_OWNER: &str =
    "crates/rumoca-compile/src/codegen_target/checked_plan/target_artifact/rendering.rs";
const ROOT_TARGET_FACADE: &str = "crates/rumoca/src/target_manifest.rs";

macro_rules! assert_not_trait {
    ($ty:ty, $bound:path) => {
        const _: fn() = || {
            trait AmbiguousIfImplemented<Marker> {
                fn probe() {}
            }
            impl<T: ?Sized> AmbiguousIfImplemented<()> for T {}
            struct ImplementsForbidden;
            impl<T: ?Sized + $bound> AmbiguousIfImplemented<ImplementsForbidden> for T {}
            let _ = <$ty as AmbiguousIfImplemented<_>>::probe;
        };
    };
}

assert_not_trait!(
    rumoca_compile::codegen::targets::ArtifactSessionInput,
    ::core::clone::Clone
);
assert_not_trait!(
    rumoca_compile::codegen::targets::ArtifactSessionInput,
    ::core::marker::Copy
);
assert_not_trait!(
    rumoca_compile::codegen::targets::ArtifactSessionInput,
    ::core::default::Default
);
assert_not_trait!(
    rumoca_compile::codegen::targets::ArtifactSessionInput,
    ::serde::Serialize
);
assert_not_trait!(
    rumoca_compile::codegen::targets::ArtifactSessionInput,
    ::serde::de::DeserializeOwned
);

#[test]
fn artifact_session_authority_is_affine_private_and_constructed_once() {
    let root = workspace_root();
    let target_artifact =
        fs::read_to_string(root.join(SESSION_OWNER)).expect("read compile-owned artifact source");
    let syntax = syn::parse_file(&target_artifact).expect("parse compile-owned artifact source");
    let rendering = fs::read_to_string(root.join(RENDER_OWNER))
        .expect("read compile-owned artifact rendering source");
    let rendering_syntax =
        syn::parse_file(&rendering).expect("parse compile-owned artifact rendering source");

    assert_session_and_input_shapes(&syntax);
    assert_private_session_constructor(&syntax);
    assert_explicit_render_target_signature(&rendering_syntax);

    let occurrences = production_occurrences(&root, "ArtifactSession::construct(");
    assert_eq!(
        occurrences.len(),
        1,
        "StrictCompilation target closure must contain the sole production session mint"
    );
    assert!(
        occurrences[0].starts_with(&format!("{RENDER_OWNER}:")),
        "the sole production session mint must belong to compile-owned target_artifact: {occurrences:?}"
    );
}

fn assert_session_and_input_shapes(syntax: &syn::File) {
    let session = named_struct(&syntax.items, "ArtifactSession");
    assert!(
        !matches!(session.vis, syn::Visibility::Public(_)),
        "ArtifactSession must never enter the public crate API"
    );
    assert!(
        session
            .fields
            .iter()
            .all(|field| matches!(field.vis, syn::Visibility::Inherited)),
        "ArtifactSession facts must be writable only by its sole constructor"
    );
    assert_no_derive(
        session.attrs.as_slice(),
        &["Clone", "Copy", "Default", "Deserialize"],
    );
    assert_no_forbidden_trait_impl(syntax, "ArtifactSession");

    let input = named_struct(&syntax.items, "ArtifactSessionInput");
    assert!(
        matches!(input.vis, syn::Visibility::Public(_)),
        "callers must supply the explicit ArtifactSessionInput"
    );
    assert!(
        input
            .fields
            .iter()
            .all(|field| matches!(field.vis, syn::Visibility::Inherited)),
        "ArtifactSessionInput must be constructible only from its two checked explicit facts"
    );
    assert_no_derive(
        input.attrs.as_slice(),
        &["Clone", "Copy", "Default", "Serialize", "Deserialize"],
    );
    assert_no_forbidden_trait_impl(syntax, "ArtifactSessionInput");
}

fn assert_private_session_constructor(syntax: &syn::File) {
    let session_constructor = inherent_method(syntax, "ArtifactSession", "construct");
    assert!(
        matches!(session_constructor.vis, syn::Visibility::Inherited),
        "only compile-owned StrictCompilation target closure may mint an ArtifactSession"
    );
}

fn assert_explicit_render_target_signature(syntax: &syn::File) {
    let render_target = inherent_method(syntax, "StrictCompilation", "render_target");
    assert!(
        matches!(render_target.vis, syn::Visibility::Public(_)),
        "StrictCompilation::render_target must be the sole public semantic-to-byte operation"
    );
    let render_signature = &render_target.sig;
    let signature = quote::quote!(#render_signature).to_string();
    for required in [
        "target : CheckedTargetBundle",
        "input : ArtifactSessionInput",
        "Result < CompletedTargetArtifact >",
    ] {
        assert!(
            signature.contains(required),
            "StrictCompilation::render_target lost explicit checked input/result `{required}`: {signature}"
        );
    }
    assert!(
        !signature.contains("Option <"),
        "StrictCompilation::render_target must not admit a missing/default target or artifact input: {signature}"
    );
}

#[test]
fn ambient_artifact_time_and_randomness_exist_only_at_interactive_entries() {
    let root = workspace_root();
    for needle in ["OffsetDateTime::now_utc(", "getrandom::fill("] {
        let occurrences = production_occurrences(&root, needle);
        let paths = occurrences
            .iter()
            .map(|location| location.rsplit_once(':').expect("line suffix").0)
            .collect::<Vec<_>>();
        assert_eq!(
            paths,
            vec![PYTHON_ENTRY, CLI_ENTRY],
            "ambient artifact input `{needle}` must stay confined to explicit interactive entry adapters"
        );
    }
    assert!(
        production_occurrences(&root, "Uuid::new_v4(").is_empty(),
        "infallible UUID helpers can panic on entropy failure; entry adapters must use fallible entropy"
    );
}

#[test]
fn root_target_facade_forwards_explicit_input_without_reconstructing_authority() {
    let root = workspace_root();
    let target_manifest =
        fs::read_to_string(root.join(ROOT_TARGET_FACADE)).expect("read target manifest facade");

    for function in ["pub fn render_target_files(", "pub fn compile_target("] {
        let body = function_prefix(&target_manifest, function);
        assert!(
            body.contains("artifact_input: ArtifactSessionInput"),
            "{function} must require an explicit artifact-session input"
        );
        assert!(
            !body.contains("Option<ArtifactSessionInput>"),
            "{function} must not admit an ambient/default fallback mode"
        );
    }
    assert!(
        target_manifest.contains("result.strict().render_target(target, artifact_input)"),
        "the root facade must consume one checked target through StrictCompilation::render_target"
    );
    for forbidden in [
        "ArtifactSession::construct(",
        "with_target_invocation_brand(",
        "with_member_brand(",
        "close_rendered(",
        "CheckedTargetPackageFold",
        "SealedRenderedTargetFile",
        "render_web(",
        "render_and_package(",
    ] {
        assert!(
            !target_manifest.contains(forbidden),
            "the root target facade reintroduced compile-owned construction `{forbidden}`"
        );
    }
}

#[test]
fn batch_compile_completion_never_recompiles_a_missing_cache_entry() {
    let root = workspace_root();
    let source = fs::read_to_string(root.join("crates/rumoca-compile/src/session/session_impl.rs"))
        .expect("read compile session implementation");
    let syntax = syn::parse_file(&source).expect("parse compile session implementation");
    let method = inherent_method(&syntax, "Session", "compile_models_with_cache");
    let body = &method.block;
    let body = quote::quote!(#body).to_string();

    assert!(
        !body.contains("Defensive fallback") && !source.contains("cache entry is absent"),
        "batch compile completion must consume its closed Hit/Miss results, never repair a missing cache entry"
    );
    assert!(
        body.matches(". compile_phase_result_query (").count() <= 1,
        "compile_models_with_cache may directly query only its single-model path; batch completion must not recompile after its parallel fold: {body}"
    );
}

#[test]
fn navigation_authority_never_falls_back_after_semantic_failure() {
    let root = workspace_root();
    let wasm = fs::read_to_string(root.join("crates/rumoca-bind-wasm/src/lib.rs"))
        .expect("read WASM adapter");
    let goto =
        fs::read_to_string(root.join("crates/rumoca-tool-lsp/src/handlers/goto_definition.rs"))
            .expect("read go-to-definition handler");

    assert!(
        !wasm.contains("resolved_cached()"),
        "WASM navigation must not replace a failed semantic build with an unrelated cached tree"
    );
    assert!(
        !goto.contains("unwrap_or_else(|| fallback_uri.clone())"),
        "a missing or invalid foreign target URI must not be relabeled as the current document"
    );
    let semantic_start = goto
        .find("if let Some(tree) = tree {")
        .expect("semantic navigation branch");
    let syntax_start = goto[semantic_start..]
        .find("// Syntax-only recovery")
        .map(|offset| semantic_start + offset)
        .expect("explicit syntax-only boundary");
    let semantic_branch = &goto[semantic_start..syntax_start];
    assert!(
        semantic_branch.contains("return None;") && !semantic_branch.contains("ast_lookup("),
        "once semantic authority exists, lookup failure must remain failure rather than retrying the same spelling against AST"
    );
}

fn inherent_method<'a>(syntax: &'a syn::File, owner: &str, method: &str) -> &'a syn::ImplItemFn {
    syntax
        .items
        .iter()
        .filter_map(|item| match item {
            Item::Impl(item)
                if matches!(
                    item.self_ty.as_ref(),
                    syn::Type::Path(path)
                        if path.path.segments.last().is_some_and(|segment| segment.ident == owner)
                ) =>
            {
                Some(item)
            }
            _ => None,
        })
        .flat_map(|item| &item.items)
        .find_map(|item| match item {
            syn::ImplItem::Fn(function) if function.sig.ident == method => Some(function),
            _ => None,
        })
        .unwrap_or_else(|| panic!("missing {owner}::{method}"))
}

fn named_struct<'a>(items: &'a [Item], name: &str) -> &'a syn::ItemStruct {
    items
        .iter()
        .find_map(|item| match item {
            Item::Struct(item) if item.ident == name => Some(item),
            _ => None,
        })
        .unwrap_or_else(|| panic!("missing struct {name}"))
}

fn assert_no_derive(attributes: &[Attribute], forbidden: &[&str]) {
    let source = attributes
        .iter()
        .filter(|attribute| attribute.path().is_ident("derive"))
        .map(|attribute| quote::quote!(#attribute).to_string())
        .collect::<Vec<_>>()
        .join(" ");
    for name in forbidden {
        assert!(
            !source
                .split(|ch: char| !ch.is_ascii_alphanumeric())
                .any(|word| word == *name),
            "artifact authority must not derive {name}: {source}"
        );
    }
}

fn assert_no_forbidden_trait_impl(syntax: &syn::File, target: &str) {
    let type_aliases = syntax
        .items
        .iter()
        .filter_map(|item| match item {
            Item::Type(alias) => {
                type_path_name(&alias.ty).map(|resolved| (alias.ident.to_string(), resolved))
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    let mut trait_aliases = Vec::new();
    for item in &syntax.items {
        if let Item::Use(item) = item {
            collect_renamed_imports(&item.tree, &mut trait_aliases);
        }
    }
    for item in &syntax.items {
        let Item::Impl(item) = item else {
            continue;
        };
        let Some((_, trait_path, _)) = &item.trait_ else {
            continue;
        };
        let Some(mut trait_name) = trait_path
            .segments
            .last()
            .map(|segment| segment.ident.to_string())
        else {
            continue;
        };
        if let Some((_, original)) = trait_aliases.iter().find(|(alias, _)| alias == &trait_name) {
            trait_name.clone_from(original);
        }
        if !["Clone", "Copy", "Default", "Serialize", "Deserialize"].contains(&trait_name.as_str())
        {
            continue;
        }
        let Some(mut self_name) = type_path_name(&item.self_ty) else {
            continue;
        };
        for _ in 0..=type_aliases.len() {
            let Some((_, resolved)) = type_aliases.iter().find(|(alias, _)| alias == &self_name)
            else {
                break;
            };
            self_name.clone_from(resolved);
        }
        assert_ne!(
            self_name, target,
            "{target} must not gain a handwritten or aliased {trait_name} implementation"
        );
    }
}

fn type_path_name(ty: &syn::Type) -> Option<String> {
    match ty {
        syn::Type::Path(path) => path
            .path
            .segments
            .last()
            .map(|segment| segment.ident.to_string()),
        _ => None,
    }
}

fn collect_renamed_imports(tree: &syn::UseTree, aliases: &mut Vec<(String, String)>) {
    match tree {
        syn::UseTree::Path(path) => collect_renamed_imports(&path.tree, aliases),
        syn::UseTree::Rename(rename) => {
            aliases.push((rename.rename.to_string(), rename.ident.to_string()))
        }
        syn::UseTree::Group(group) => {
            for tree in &group.items {
                collect_renamed_imports(tree, aliases);
            }
        }
        syn::UseTree::Name(_) | syn::UseTree::Glob(_) => {}
    }
}

fn production_occurrences(root: &Path, needle: &str) -> Vec<String> {
    let mut sources = Vec::new();
    collect_rs_files(&root.join("crates"), &mut sources);
    let mut occurrences = Vec::new();
    for path in sources {
        let relative = path
            .strip_prefix(root)
            .expect("workspace source path")
            .to_string_lossy()
            .replace('\\', "/");
        if relative.contains("/tests/") || relative.ends_with("/tests.rs") {
            continue;
        }
        let source = fs::read_to_string(&path).expect("read production Rust source");
        for (line, text) in source.lines().enumerate() {
            if text.contains(needle) {
                occurrences.push(format!("{relative}:{}", line + 1));
            }
        }
    }
    occurrences.sort();
    occurrences
}

fn function_prefix<'a>(source: &'a str, signature: &str) -> &'a str {
    let start = source
        .find(signature)
        .unwrap_or_else(|| panic!("missing function {signature}"));
    let suffix = &source[start..];
    let end = suffix.find('{').expect("function body start");
    &suffix[..end]
}
