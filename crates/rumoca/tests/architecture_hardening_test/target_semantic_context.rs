use std::any::TypeId;
use std::collections::BTreeSet;
use std::fs;
use std::path::Path;

use rumoca_compile::codegen::targets::{TargetSemanticContext, TargetSemanticView};
use syn::visit::Visit;

use super::architecture_hardening_support::{collect_rs_files, workspace_root};

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

macro_rules! assert_affine_non_wire {
    ($ty:ty) => {
        assert_not_trait!($ty, ::core::clone::Clone);
        assert_not_trait!($ty, ::core::marker::Copy);
        assert_not_trait!($ty, ::core::default::Default);
        assert_not_trait!($ty, ::serde::Serialize);
        assert_not_trait!($ty, ::serde::de::DeserializeOwned);
    };
}

assert_affine_non_wire!(rumoca_compile::codegen::targets::CompletedTargetArtifact);
assert_affine_non_wire!(rumoca_compile::codegen::targets::CompletedPackage);
assert_affine_non_wire!(rumoca_compile::codegen::targets::CompletedUnpackaged);
assert_affine_non_wire!(rumoca_compile::codegen::targets::CompletedRenderedFile);

#[test]
fn target_semantic_contexts_match_ir_crate_names_exactly() {
    let crates_dir = workspace_root().join("crates");
    let ir_crates = fs::read_dir(&crates_dir)
        .expect("read workspace crates")
        .map(|entry| entry.expect("read workspace crate entry"))
        .filter(|entry| entry.file_type().is_ok_and(|kind| kind.is_dir()))
        .filter_map(|entry| {
            entry
                .file_name()
                .to_str()
                .and_then(|name| name.strip_prefix("rumoca-ir-").map(str::to_owned))
        })
        .collect::<BTreeSet<_>>();

    // ALL and the enum are generated from one macro invocation, so comparing
    // ALL here compares the complete closed enum rather than a second list
    // that could silently omit a variant.
    let contexts = TargetSemanticContext::ALL
        .iter()
        .map(|context| context.as_str().to_owned())
        .collect::<BTreeSet<_>>();

    assert_eq!(
        contexts, ir_crates,
        "target semantic contexts must be exactly the rumoca-ir-* crate names; roots, views, products, output formats, and target names belong below this boundary"
    );
}

#[test]
fn target_semantic_views_map_one_to_one_to_real_checked_ir_types() {
    fn type_id<T: 'static>() -> TypeId {
        TypeId::of::<T>()
    }

    let mappings = [
        (
            TargetSemanticView::ClassTree,
            TargetSemanticContext::Ast,
            type_id::<rumoca_ir_ast::ClassTree>(),
        ),
        (
            TargetSemanticView::FlatModel,
            TargetSemanticContext::Flat,
            type_id::<rumoca_ir_flat::Model>(),
        ),
        (
            TargetSemanticView::Dae,
            TargetSemanticContext::Dae,
            type_id::<rumoca_ir_dae::Dae>(),
        ),
        (
            TargetSemanticView::AlgorithmCodePackage,
            TargetSemanticContext::Galec,
            type_id::<rumoca_ir_galec::package::AlgorithmCodePackage>(),
        ),
        (
            TargetSemanticView::SolveModel,
            TargetSemanticContext::Solve,
            type_id::<rumoca_ir_solve::SolveModel>(),
        ),
        (
            TargetSemanticView::FmiComponent,
            TargetSemanticContext::Solve,
            type_id::<rumoca_ir_solve::fmi::FmiComponent>(),
        ),
        (
            TargetSemanticView::SolveAlgorithmBlock,
            TargetSemanticContext::Solve,
            type_id::<rumoca_ir_solve::SolveAlgorithmBlock>(),
        ),
    ];

    let declared = TargetSemanticView::ALL
        .iter()
        .copied()
        .collect::<BTreeSet<_>>();
    let mapped = mappings
        .iter()
        .map(|(view, _, _)| *view)
        .collect::<BTreeSet<_>>();
    assert_eq!(
        declared, mapped,
        "every manifest view must map to one real IR type"
    );

    let distinct_types = mappings
        .iter()
        .map(|(_, _, type_id)| *type_id)
        .collect::<BTreeSet<_>>();
    assert_eq!(
        distinct_types.len(),
        mappings.len(),
        "two manifest views must not alias the same checked IR type"
    );
    for (view, context, _) in mappings {
        assert_eq!(
            view.semantic_context(),
            context,
            "a checked view must retain its actual rumoca-ir-* owner"
        );
    }
}

struct TargetArtifactBoundarySources {
    schema_source: String,
    artifact_source: String,
    publication_source: String,
    root_facade: String,
    root_library: String,
    compile_library: String,
}

fn target_artifact_boundary_sources(root: &Path) -> TargetArtifactBoundarySources {
    let schema_source =
        fs::read_to_string(root.join("crates/rumoca-compile/src/codegen_target/checked_plan.rs"))
            .expect("read checked target plan source");
    let artifact_root = fs::read_to_string(
        root.join("crates/rumoca-compile/src/codegen_target/checked_plan/target_artifact.rs"),
    )
    .expect("read compile-owned target artifact source");
    let artifact_rendering = fs::read_to_string(root.join(
        "crates/rumoca-compile/src/codegen_target/checked_plan/target_artifact/rendering.rs",
    ))
    .expect("read compile-owned target artifact rendering source");
    let artifact_source = format!("{artifact_root}\n{artifact_rendering}");
    let publication_source = fs::read_to_string(root.join(
        "crates/rumoca-compile/src/codegen_target/checked_plan/target_artifact/publication.rs",
    ))
    .expect("read compile-owned publication source");
    let root_facade = fs::read_to_string(root.join("crates/rumoca/src/target_manifest.rs"))
        .expect("read root target facade");
    let root_library = fs::read_to_string(root.join("crates/rumoca/src/lib.rs"))
        .expect("read rumoca public surface");
    let compile_library = fs::read_to_string(root.join("crates/rumoca-compile/src/lib.rs"))
        .expect("read compile public surface");
    TargetArtifactBoundarySources {
        schema_source,
        artifact_source,
        publication_source,
        root_facade,
        root_library,
        compile_library,
    }
}

#[test]
fn package_publication_consumes_one_ordered_member_stream() {
    let source = fs::read_to_string(workspace_root().join(
        "crates/rumoca-compile/src/codegen_target/checked_plan/target_artifact/publication.rs",
    ))
    .expect("read compile-owned publication source");
    assert!(publication_member_stream_violations(&source).is_empty());

    let wrong_bytes = source.replacen(
        ".write_all(member.bytes())",
        ".write_all(b\"detached bytes\")",
        1,
    );
    assert_eq!(
        publication_member_stream_violations(&wrong_bytes),
        vec!["member loop must write bytes from that same member"]
    );

    let parallel_stream = source.replacen(
        "for member in package.members() {",
        "for member in package.members() {}\n    for member in package.members() {",
        1,
    );
    let violations = publication_member_stream_violations(&parallel_stream);
    assert!(violations.contains(&"stage_archive must consume exactly one package member stream"));
    assert!(violations.contains(&"package members may have exactly one consumer"));
}

#[derive(Default)]
struct PublicationMemberInventory {
    member_calls: usize,
    member_loops: usize,
    loop_start_file_calls: usize,
    loop_member_path_calls: usize,
    loop_write_all_calls: usize,
    loop_member_bytes_calls: usize,
}

impl<'ast> Visit<'ast> for PublicationMemberInventory {
    fn visit_expr_method_call(&mut self, call: &'ast syn::ExprMethodCall) {
        if call.method == "members" && receiver_is(&call.receiver, "package") {
            self.member_calls += 1;
        }
        syn::visit::visit_expr_method_call(self, call);
    }

    fn visit_expr_for_loop(&mut self, loop_expression: &'ast syn::ExprForLoop) {
        let syn::Expr::MethodCall(iteration) = &*loop_expression.expr else {
            syn::visit::visit_expr_for_loop(self, loop_expression);
            return;
        };
        if iteration.method != "members"
            || !iteration.args.is_empty()
            || !receiver_is(&iteration.receiver, "package")
        {
            syn::visit::visit_expr_for_loop(self, loop_expression);
            return;
        }
        self.member_loops += 1;
        let mut body = PublicationMemberBody::default();
        body.visit_block(&loop_expression.body);
        self.loop_start_file_calls += body.start_file_calls;
        self.loop_member_path_calls += body.member_path_calls;
        self.loop_write_all_calls += body.write_all_calls;
        self.loop_member_bytes_calls += body.member_bytes_calls;
        syn::visit::visit_expr_for_loop(self, loop_expression);
    }
}

#[derive(Default)]
struct PublicationMemberBody {
    start_file_calls: usize,
    member_path_calls: usize,
    write_all_calls: usize,
    member_bytes_calls: usize,
}

impl<'ast> Visit<'ast> for PublicationMemberBody {
    fn visit_expr_method_call(&mut self, call: &'ast syn::ExprMethodCall) {
        if call.method == "start_file" && receiver_is(&call.receiver, "archive") {
            self.start_file_calls += 1;
            if call
                .args
                .first()
                .is_some_and(|argument| zero_arg_method_on(argument, "member", "path"))
            {
                self.member_path_calls += 1;
            }
        }
        if call.method == "write_all" && receiver_is(&call.receiver, "archive") {
            self.write_all_calls += 1;
            if call
                .args
                .first()
                .is_some_and(|argument| zero_arg_method_on(argument, "member", "bytes"))
            {
                self.member_bytes_calls += 1;
            }
        }
        syn::visit::visit_expr_method_call(self, call);
    }
}

fn receiver_is(receiver: &syn::Expr, expected: &str) -> bool {
    matches!(receiver, syn::Expr::Path(path) if path.path.is_ident(expected))
}

fn zero_arg_method_on(expression: &syn::Expr, receiver: &str, method: &str) -> bool {
    matches!(
        expression,
        syn::Expr::MethodCall(call)
            if call.method == method
                && call.args.is_empty()
                && receiver_is(&call.receiver, receiver)
    )
}

fn publication_member_stream_violations(source: &str) -> Vec<&'static str> {
    let Ok(syntax) = syn::parse_file(source) else {
        return vec!["publication source must parse"];
    };
    let Some(stage_archive) = syntax.items.iter().find_map(|item| match item {
        syn::Item::Fn(function) if function.sig.ident == "stage_archive" => Some(function),
        _ => None,
    }) else {
        return vec!["stage_archive must exist"];
    };

    let mut inventory = PublicationMemberInventory::default();
    inventory.visit_block(&stage_archive.block);
    let mut violations = Vec::new();
    if inventory.member_loops != 1 {
        violations.push("stage_archive must consume exactly one package member stream");
    }
    if inventory.member_calls != 1 {
        violations.push("package members may have exactly one consumer");
    }
    if inventory.loop_start_file_calls != 1 || inventory.loop_member_path_calls != 1 {
        violations.push("member loop must start one file from that same member path");
    }
    if inventory.loop_write_all_calls != 1 || inventory.loop_member_bytes_calls != 1 {
        violations.push("member loop must write bytes from that same member");
    }
    violations
}

fn assert_deleted_target_bridges_absent(
    artifact_source: &str,
    publication_source: &str,
    root_facade: &str,
    root_library: &str,
    compile_library: &str,
) {
    for forbidden in [
        "SealedRenderedTargetFile",
        "CheckedTargetPackageFold",
        "CheckedPackagedTargetFileIssuer",
        "CheckedPackagedTargetFileCompletion",
        "ArtifactRenderContext",
        "fn render_web(",
        "fn render_web_files(",
        "fn render_and_package(",
        "Result<RenderedTargetFile>",
    ] {
        for (surface, source) in [
            ("compile-owned target closure", artifact_source),
            ("compile-owned publication", publication_source),
            ("root target facade", root_facade),
        ] {
            assert!(
                !source.contains(forbidden),
                "{surface} reintroduced deleted packaging/file-seal compatibility `{forbidden}`"
            );
        }
    }
    for forbidden_export in [
        "pub mod packaging",
        "mod packaging;",
        "SealedRenderedTargetFile",
        "CheckedTargetPackageFold",
        "CheckedPackagedTargetFileIssuer",
        "render_web,",
        "render_web_files,",
        "render_and_package",
        "ArtifactRenderContext",
        "ArtifactSession,",
    ] {
        for (surface, source) in [
            ("rumoca", root_library),
            ("rumoca-compile", compile_library),
        ] {
            assert!(
                !source.contains(forbidden_export),
                "{surface} public facade restored legacy target authority `{forbidden_export}`"
            );
        }
    }
}

#[test]
fn completed_target_artifact_replaces_root_packaging_and_file_seal_bridges() {
    let root = workspace_root();
    let deleted_packaging = root.join("crates/rumoca/src/packaging.rs");
    assert!(
        !deleted_packaging.exists(),
        "the deleted root packaging module is a tombstone; target closure belongs to rumoca-compile"
    );
    let TargetArtifactBoundarySources {
        schema_source,
        artifact_source,
        publication_source,
        root_facade,
        root_library,
        compile_library,
    } = target_artifact_boundary_sources(&root);

    let schema_syntax = syn::parse_file(&schema_source).expect("parse checked target plan source");
    let target_artifact_module = schema_syntax
        .items
        .iter()
        .find_map(|item| match item {
            syn::Item::Mod(module) if module.ident == "target_artifact" => Some(module),
            _ => None,
        })
        .expect("target_artifact module declaration");
    assert!(
        matches!(target_artifact_module.vis, syn::Visibility::Inherited),
        "target_artifact must remain private, not pub or pub(crate)"
    );
    assert_target_artifact_reexports_are_closed(&schema_syntax);
    for (surface, source) in [
        ("rumoca", root_library.as_str()),
        ("rumoca-compile", compile_library.as_str()),
    ] {
        let syntax =
            syn::parse_file(source).unwrap_or_else(|error| panic!("parse {surface}: {error}"));
        let paths = resolved_public_target_artifact_items(&syntax.items);
        assert!(
            paths.is_empty(),
            "{surface} facade must not bypass codegen_target through target_artifact: {paths:#?}"
        );
    }
    assert!(
        artifact_source.contains("pub enum CompletedTargetArtifact"),
        "compile-owned closure must expose one completed-artifact result"
    );
    assert!(
        artifact_source.contains("pub fn render_target(")
            && artifact_source.contains("Result<CompletedTargetArtifact>"),
        "StrictCompilation::render_target must be the only public semantic-to-byte target operation"
    );
    assert!(
        root_facade.contains("result.strict().render_target(target, artifact_input)"),
        "the root facade must delegate the closed target without rebuilding its authority"
    );
    assert!(
        publication_source.contains("use super::{")
            && publication_source.contains("CompletedPackage")
            && publication_source.contains("CompletedUnpackaged"),
        "publication must consume only compile-completed artifact products"
    );

    assert_deleted_target_bridges_absent(
        &artifact_source,
        &publication_source,
        &root_facade,
        &root_library,
        &compile_library,
    );

    assert_completed_artifact_shape(&artifact_source);
}

#[test]
fn compile_facade_exposes_checked_target_authority_not_raw_manifest_parts() {
    let root = workspace_root();
    let schema_source =
        fs::read_to_string(root.join("crates/rumoca-compile/src/codegen_target.rs"))
            .expect("read checked target source");
    let checked_plan_source =
        fs::read_to_string(root.join("crates/rumoca-compile/src/codegen_target/checked_plan.rs"))
            .expect("read checked target plan source");
    let facade_source = fs::read_to_string(root.join("crates/rumoca-compile/src/lib.rs"))
        .expect("read compile facade");
    let schema = syn::parse_file(&schema_source).expect("parse checked target source");
    let checked_plan =
        syn::parse_file(&checked_plan_source).expect("parse checked target plan source");
    let facade = syn::parse_file(&facade_source).expect("parse compile facade");
    let forbidden = [
        "TargetManifest",
        "TargetFile",
        "ChecksumNeed",
        "TargetResolvedChecksumBinding",
        "CheckedTargetArchivePolicy",
        "CheckedTargetOutputPathTemplate",
        "CheckedTargetPackagePath",
        "parse_target_manifest",
        "validate_dae_target_capabilities",
        "validate_solve_target_capabilities",
        "validate_solve_tensor_inventory",
    ];

    assert_no_public_raw_target_surface(&schema.items, &forbidden);
    assert_no_public_raw_target_surface(&checked_plan.items, &forbidden);

    let public_names = public_use_paths(&facade.items)
        .into_iter()
        .filter_map(|path| path.rsplit("::").next().map(str::to_owned))
        .collect::<BTreeSet<_>>();
    let escaped = forbidden
        .into_iter()
        .filter(|name| public_names.contains(*name))
        .collect::<Vec<_>>();
    assert!(
        escaped.is_empty(),
        "rumoca-compile publicly reexported raw target construction authority: {escaped:#?}"
    );

    let descriptor = schema
        .items
        .iter()
        .find_map(|item| match item {
            syn::Item::Struct(item) if item.ident == "BuiltinTargetDescriptor" => Some(item),
            _ => None,
        })
        .expect("BuiltinTargetDescriptor declaration");
    assert!(
        descriptor
            .fields
            .iter()
            .all(|field| field.ident.as_ref().is_none_or(|ident| ident != "manifest")),
        "a passive serializable built-in descriptor must not retain raw manifest text"
    );
    for item in &schema.items {
        let syn::Item::Impl(item) = item else {
            continue;
        };
        if type_path_name(&item.self_ty).as_deref() != Some("BuiltinTargetDescriptor") {
            continue;
        }
        for member in &item.items {
            if let syn::ImplItem::Fn(function) = member
                && matches!(function.vis, syn::Visibility::Public(_))
            {
                assert!(
                    !function.sig.ident.to_string().contains("manifest"),
                    "a passive built-in descriptor must not expose raw manifest text through {}",
                    function.sig.ident
                );
            }
        }
    }

    for client in ["crates/rumoca-bind-wasm/src", "crates/rumoca-tool-lsp/src"] {
        let client_source = non_test_rust_source_tree(&root, client);
        for forbidden_route in [
            ".parse_manifest(",
            "parse_target_manifest(",
            "validate_dae_target_capabilities(",
            "validate_solve_target_capabilities(",
            "validate_solve_tensor_inventory(",
        ] {
            assert!(
                !client_source.contains(forbidden_route),
                "{client} restored client-side raw target inspection `{forbidden_route}`; check the bundle and call StrictCompilation::render_target"
            );
        }
    }
}

fn assert_no_public_raw_target_surface(items: &[syn::Item], forbidden: &[&str]) {
    let mut raw_names = forbidden
        .iter()
        .copied()
        .map(str::to_owned)
        .collect::<BTreeSet<_>>();
    loop {
        let previous_len = raw_names.len();
        collect_raw_target_aliases(items, &mut raw_names);
        if raw_names.len() == previous_len {
            break;
        }
    }
    let mut offenders = Vec::new();
    collect_public_raw_target_surface(items, &raw_names, &mut offenders);
    offenders.sort();
    assert!(
        offenders.is_empty(),
        "raw target construction authority must remain compile-private: {offenders:#?}"
    );
}

fn collect_raw_target_aliases(items: &[syn::Item], raw_names: &mut BTreeSet<String>) {
    for item in items {
        match item {
            syn::Item::Type(alias)
                if type_path_name(&alias.ty).is_some_and(|name| raw_names.contains(&name)) =>
            {
                raw_names.insert(alias.ident.to_string());
            }
            syn::Item::Use(item) => collect_raw_target_use_aliases(&item.tree, raw_names),
            syn::Item::Mod(module) => {
                if let Some((_, nested)) = &module.content {
                    collect_raw_target_aliases(nested, raw_names);
                }
            }
            _ => {}
        }
    }
}

fn collect_raw_target_use_aliases(tree: &syn::UseTree, raw_names: &mut BTreeSet<String>) {
    match tree {
        syn::UseTree::Path(path) => collect_raw_target_use_aliases(&path.tree, raw_names),
        syn::UseTree::Rename(rename) if raw_names.contains(&rename.ident.to_string()) => {
            raw_names.insert(rename.rename.to_string());
        }
        syn::UseTree::Group(group) => {
            for item in &group.items {
                collect_raw_target_use_aliases(item, raw_names);
            }
        }
        syn::UseTree::Name(_) | syn::UseTree::Rename(_) | syn::UseTree::Glob(_) => {}
    }
}

fn record_public_raw_struct(
    item: &syn::ItemStruct,
    raw_names: &BTreeSet<String>,
    offenders: &mut Vec<String>,
) {
    if raw_names.contains(&item.ident.to_string()) {
        offenders.push(format!("type {}", item.ident));
    }
    for field in &item.fields {
        if !matches!(field.vis, syn::Visibility::Public(_)) {
            continue;
        }
        if tokens_name_one_of(&quote::quote!(#field).to_string(), raw_names) {
            offenders.push(format!("{} public field", item.ident));
        }
    }
}

fn record_public_raw_enum(
    item: &syn::ItemEnum,
    raw_names: &BTreeSet<String>,
    offenders: &mut Vec<String>,
) {
    if raw_names.contains(&item.ident.to_string()) {
        offenders.push(format!("type {}", item.ident));
    }
    for variant in &item.variants {
        if tokens_name_one_of(&quote::quote!(#variant).to_string(), raw_names) {
            offenders.push(format!("{}::{}", item.ident, variant.ident));
        }
    }
}

fn record_public_raw_impl(
    item: &syn::ItemImpl,
    raw_names: &BTreeSet<String>,
    offenders: &mut Vec<String>,
) {
    let owner = type_path_name(&item.self_ty).unwrap_or_else(|| "<unknown>".to_owned());
    for member in &item.items {
        let syn::ImplItem::Fn(function) = member else {
            continue;
        };
        if !matches!(function.vis, syn::Visibility::Public(_)) {
            continue;
        }
        let signature = &function.sig;
        if tokens_name_one_of(&quote::quote!(#signature).to_string(), raw_names) {
            offenders.push(format!("{owner}::{}", function.sig.ident));
        }
    }
}

fn record_public_raw_use(
    item: &syn::ItemUse,
    raw_names: &BTreeSet<String>,
    offenders: &mut Vec<String>,
) {
    let mut bindings = Vec::new();
    collect_use_tree_bindings(&item.tree, Vec::new(), true, &mut bindings);
    for binding in bindings {
        if binding.path.iter().any(|name| raw_names.contains(name)) {
            offenders.push(format!("public use {}", binding.path.join("::")));
        }
    }
}

fn collect_public_raw_target_surface(
    items: &[syn::Item],
    raw_names: &BTreeSet<String>,
    offenders: &mut Vec<String>,
) {
    for item in items {
        match item {
            syn::Item::Struct(item) if matches!(item.vis, syn::Visibility::Public(_)) => {
                record_public_raw_struct(item, raw_names, offenders);
            }
            syn::Item::Enum(item) if matches!(item.vis, syn::Visibility::Public(_)) => {
                record_public_raw_enum(item, raw_names, offenders);
            }
            syn::Item::Type(item)
                if matches!(item.vis, syn::Visibility::Public(_))
                    && raw_names.contains(&item.ident.to_string()) =>
            {
                offenders.push(format!("type alias {}", item.ident));
            }
            syn::Item::Fn(item) if matches!(item.vis, syn::Visibility::Public(_)) => {
                let signature = &item.sig;
                if tokens_name_one_of(&quote::quote!(#signature).to_string(), raw_names) {
                    offenders.push(format!("function {}", item.sig.ident));
                }
            }
            syn::Item::Impl(item) => {
                record_public_raw_impl(item, raw_names, offenders);
            }
            syn::Item::Use(item) if matches!(item.vis, syn::Visibility::Public(_)) => {
                record_public_raw_use(item, raw_names, offenders);
            }
            syn::Item::Mod(module) => {
                if let Some((_, nested)) = &module.content {
                    collect_public_raw_target_surface(nested, raw_names, offenders);
                }
            }
            _ => {}
        }
    }
}

fn tokens_name_one_of(tokens: &str, names: &BTreeSet<String>) -> bool {
    tokens
        .split(|ch: char| !ch.is_ascii_alphanumeric() && ch != '_')
        .any(|token| names.contains(token))
}

#[test]
fn target_artifact_reexport_census_resolves_private_alias_chains() {
    let fixture = syn::parse_file(
        r#"
        use target_artifact::ArtifactSession as HiddenSession;
        use HiddenSession as IndirectSession;
        pub use IndirectSession as EscapedSession;
        "#,
    )
    .expect("parse target-artifact alias adversary");
    assert_eq!(
        resolved_public_target_artifact_items(&fixture.items),
        BTreeSet::from(["ArtifactSession".to_owned()]),
        "private import aliases must not hide a public target-artifact reexport"
    );
}

fn non_test_rust_source_tree(root: &Path, relative: &str) -> String {
    let mut paths = Vec::new();
    collect_rs_files(&root.join(relative), &mut paths);
    paths.sort();
    paths
        .into_iter()
        .filter(|path| {
            let relative = path
                .strip_prefix(root)
                .expect("source beneath workspace")
                .to_string_lossy();
            !relative.contains("/tests/") && !relative.ends_with("/tests.rs")
        })
        .map(|path| {
            fs::read_to_string(&path)
                .unwrap_or_else(|error| panic!("read {}: {error}", path.display()))
        })
        .collect::<Vec<_>>()
        .join("\n")
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

fn assert_target_artifact_reexports_are_closed(syntax: &syn::File) {
    let actual = resolved_public_target_artifact_items(&syntax.items);
    let expected = [
        "ArtifactGenerationInstant",
        "ArtifactIdentitySeed",
        "ArtifactSessionInput",
        "ArtifactSessionInputError",
        "CompletedArtifactMemberRef",
        "CompletedPackage",
        "CompletedRenderedFile",
        "CompletedRenderedFileRef",
        "CompletedTargetArtifact",
        "CompletedUnpackaged",
        "PublishedTargetArtifact",
    ]
    .into_iter()
    .map(str::to_owned)
    .collect::<BTreeSet<_>>();
    assert_eq!(
        actual, expected,
        "target_artifact may publicly lend only explicit session inputs and passive completed products"
    );
}

#[derive(Debug)]
struct UseBinding {
    path: Vec<String>,
    local: String,
    public: bool,
}

fn resolved_public_target_artifact_items(items: &[syn::Item]) -> BTreeSet<String> {
    let mut bindings = Vec::new();
    collect_use_bindings(items, &mut bindings);
    let mut resolved = std::collections::BTreeMap::<String, String>::new();
    loop {
        let previous_len = resolved.len();
        for binding in &bindings {
            let direct = binding
                .path
                .iter()
                .position(|segment| segment == "target_artifact")
                .and_then(|position| binding.path.get(position + 1))
                .cloned();
            let indirect = binding
                .path
                .iter()
                .find_map(|segment| resolved.get(segment))
                .cloned();
            if let Some(item) = direct.or(indirect) {
                resolved.insert(binding.local.clone(), item);
            }
        }
        if resolved.len() == previous_len {
            break;
        }
    }

    let mut public = bindings
        .iter()
        .filter(|binding| binding.public)
        .filter_map(|binding| resolved.get(&binding.local).cloned())
        .collect::<BTreeSet<_>>();
    if bindings.iter().any(|binding| {
        binding.path.iter().any(|part| part == "target_artifact")
            && binding.path.last().is_some_and(|part| part == "*")
    }) {
        public.insert("*".to_owned());
    }
    public
}

fn collect_use_bindings(items: &[syn::Item], bindings: &mut Vec<UseBinding>) {
    for item in items {
        match item {
            syn::Item::Use(item) => collect_use_tree_bindings(
                &item.tree,
                Vec::new(),
                matches!(item.vis, syn::Visibility::Public(_)),
                bindings,
            ),
            syn::Item::Mod(module) => {
                if let Some((_, nested)) = &module.content {
                    collect_use_bindings(nested, bindings);
                }
            }
            _ => {}
        }
    }
}

fn collect_use_tree_bindings(
    tree: &syn::UseTree,
    prefix: Vec<String>,
    public: bool,
    bindings: &mut Vec<UseBinding>,
) {
    match tree {
        syn::UseTree::Path(path) => {
            let mut prefix = prefix;
            prefix.push(path.ident.to_string());
            collect_use_tree_bindings(&path.tree, prefix, public, bindings);
        }
        syn::UseTree::Name(name) => {
            let mut path = prefix;
            path.push(name.ident.to_string());
            bindings.push(UseBinding {
                local: name.ident.to_string(),
                path,
                public,
            });
        }
        syn::UseTree::Rename(rename) => {
            let mut path = prefix;
            path.push(rename.ident.to_string());
            bindings.push(UseBinding {
                local: rename.rename.to_string(),
                path,
                public,
            });
        }
        syn::UseTree::Glob(_) => {
            let mut path = prefix;
            path.push("*".to_owned());
            bindings.push(UseBinding {
                local: "*".to_owned(),
                path,
                public,
            });
        }
        syn::UseTree::Group(group) => {
            for item in &group.items {
                collect_use_tree_bindings(item, prefix.clone(), public, bindings);
            }
        }
    }
}

fn public_use_paths(items: &[syn::Item]) -> Vec<String> {
    let mut paths = Vec::new();
    for item in items {
        match item {
            syn::Item::Use(item) if matches!(item.vis, syn::Visibility::Public(_)) => {
                collect_use_tree_paths(&item.tree, Vec::new(), &mut paths);
            }
            syn::Item::Mod(module) => {
                if let Some((_, nested)) = &module.content {
                    paths.extend(public_use_paths(nested));
                }
            }
            _ => {}
        }
    }
    paths
}

fn collect_use_tree_paths(tree: &syn::UseTree, prefix: Vec<String>, paths: &mut Vec<String>) {
    match tree {
        syn::UseTree::Path(path) => {
            let mut prefix = prefix;
            prefix.push(path.ident.to_string());
            collect_use_tree_paths(&path.tree, prefix, paths);
        }
        syn::UseTree::Name(name) => {
            let mut path = prefix;
            path.push(name.ident.to_string());
            paths.push(path.join("::"));
        }
        syn::UseTree::Rename(rename) => {
            let mut path = prefix;
            path.push(rename.ident.to_string());
            paths.push(path.join("::"));
        }
        syn::UseTree::Glob(_) => {
            let mut path = prefix;
            path.push("*".to_owned());
            paths.push(path.join("::"));
        }
        syn::UseTree::Group(group) => {
            for tree in &group.items {
                collect_use_tree_paths(tree, prefix.clone(), paths);
            }
        }
    }
}

fn assert_completed_artifact_shape(artifact_source: &str) {
    let syntax = syn::parse_file(artifact_source).expect("parse compile-owned target artifact");
    let completed = syntax
        .items
        .iter()
        .find_map(|item| match item {
            syn::Item::Enum(item) if item.ident == "CompletedTargetArtifact" => Some(item),
            _ => None,
        })
        .expect("completed target artifact type");
    let variants = completed
        .variants
        .iter()
        .map(|variant| variant.ident.to_string())
        .collect::<BTreeSet<_>>();
    assert_eq!(
        variants,
        ["Packaged", "Unpackaged"]
            .into_iter()
            .map(str::to_owned)
            .collect(),
        "the completed target result must remain a closed package/unpackaged sum"
    );
    let derives = completed
        .attrs
        .iter()
        .filter(|attribute| attribute.path().is_ident("derive"))
        .map(|attribute| quote::quote!(#attribute).to_string())
        .collect::<String>();
    for forbidden in ["Clone", "Copy", "Default", "Serialize", "Deserialize"] {
        assert!(
            !derives
                .split(|ch: char| !ch.is_ascii_alphanumeric())
                .any(|word| word == forbidden),
            "the completed target authority must remain affine and non-wire; found {forbidden}"
        );
    }
    for name in [
        "CompletedPackage",
        "CompletedUnpackaged",
        "CompletedRenderedFile",
    ] {
        let carrier = syntax
            .items
            .iter()
            .find_map(|item| match item {
                syn::Item::Struct(item) if item.ident == name => Some(item),
                _ => None,
            })
            .unwrap_or_else(|| panic!("missing completed carrier {name}"));
        assert!(
            carrier
                .fields
                .iter()
                .all(|field| matches!(field.vis, syn::Visibility::Inherited)),
            "{name} must not admit caller-authored path/bytes into CompletedTargetArtifact"
        );
        let derives = carrier
            .attrs
            .iter()
            .filter(|attribute| attribute.path().is_ident("derive"))
            .map(|attribute| quote::quote!(#attribute).to_string())
            .collect::<String>();
        for forbidden in ["Clone", "Copy", "Default", "Serialize", "Deserialize"] {
            assert!(
                !derives
                    .split(|ch: char| !ch.is_ascii_alphanumeric())
                    .any(|word| word == forbidden),
                "{name} must remain an affine, non-wire completed carrier; found {forbidden}"
            );
        }
    }
}
