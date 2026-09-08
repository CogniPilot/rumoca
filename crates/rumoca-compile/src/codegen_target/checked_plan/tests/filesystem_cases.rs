use super::*;

#[cfg(unix)]
#[test]
fn checked_directory_target_rejects_symlink_and_oversized_template_inputs() {
    use std::os::unix::fs::symlink;

    let directory = tempfile::tempdir().expect("isolated no-follow target directory");
    std::fs::write(directory.path().join("real.jinja"), "safe").expect("write real template");
    symlink(
        directory.path().join("real.jinja"),
        directory.path().join("linked.jinja"),
    )
    .expect("create final-component symlink");
    let manifest = r#"
version = 1
name = "no-follow"
[[files]]
artifact_kind = "text"
semantic_context = "ast"
path = "output.txt"
template = "linked.jinja"
"#;
    let error = directory_target_bundle(directory.path(), manifest.to_owned())
        .check()
        .expect_err("checked template input must not follow a final symlink");
    assert!(
        format!("{error:#}").contains("without following"),
        "{error:#}"
    );

    let oversized = directory.path().join("oversized.jinja");
    let file = std::fs::File::create(&oversized).expect("create sparse oversized input");
    file.set_len(super::super::MAX_TARGET_INPUT_FILE_BYTES + 1)
        .expect("set sparse size");
    let error = directory_target_bundle(
        directory.path(),
        manifest.replace("linked.jinja", "oversized.jinja"),
    )
    .check()
    .expect_err("oversized template input must reject before reading");
    assert!(
        format!("{error:#}").contains("regular file of at most"),
        "{error:#}"
    );
}

fn assert_artifact_identity_construction(artifact_sources: &[&str], strict_source: &str) {
    for source in artifact_sources.iter().copied().chain([strict_source]) {
        assert!(
            !source.contains("model_name.replace"),
            "artifact identity must not sanitize a display model name"
        );
    }
    for required in [
        "struct CanonicalModelIdentity",
        "ClassDefIndex::from_tree(resolved.inner())",
        ".def_ancestry(def_id)",
        "CheckedTargetArtifactStem::from_model_components",
    ] {
        assert!(
            strict_source.contains(required),
            "strict compilation lost canonical identity construction `{required}`"
        );
    }
    for required in [
        "model_identity.components()",
        "model_identity.artifact_stem()",
        "rumoca-artifact-identity-v1",
        "fn scoped_artifact_identities(",
        "fn template_bindings(",
    ] {
        assert!(
            artifact_sources
                .iter()
                .any(|source| source.contains(required)),
            "artifact session lost canonical identity framing `{required}`"
        );
    }

    let session_source = artifact_sources[0];
    let render_source = artifact_sources[1];
    for forbidden in [
        "Value::from_serialize(session",
        "Value::from_serialize(&session",
        "template_bindings(&self.identities",
        "template_bindings(self.identities",
    ] {
        assert!(
            !artifact_sources
                .iter()
                .any(|source| source.contains(forbidden)),
            "artifact session must not expose target-wide serialization authority `{forbidden}`"
        );
    }
    assert!(
        session_source.contains("identities: BTreeMap<String, String>"),
        "artifact session must privately retain the complete target-issued identity catalog"
    );
    assert!(
        render_source.contains(".scoped_artifact_identities("),
        "rendering must derive a checked per-file identity subset before context construction"
    );
}

#[test]
fn template_artifact_identity_visibility_is_exactly_the_checked_dependency_set() {
    let mut session = crate::session::Session::default();
    session
        .add_document(
            "identity-visibility.mo",
            r#"
model IdentityVisibility
  Real x(start = 0.0, fixed = true);
equation
  der(x) = 1.0;
end IdentityVisibility;
"#,
        )
        .expect("parse FMI identity visibility fixture");
    let compilation = session
        .compile_model_strict("IdentityVisibility")
        .unwrap_or_else(|report| panic!("compile FMI identity visibility fixture: {report:?}"));
    let target = TargetBundle::builtin("fmi-ls-wasm")
        .expect("FMI-LS-Wasm target is registered")
        .check()
        .expect("built-in identity dependencies close before rendering");
    let completed = compilation
        .render_target(target, test_artifact_input())
        .expect("checked dependency exposes its issued identity");
    let files = completed.into_rendered_files();
    let library = files
        .iter()
        .find(|file| file.path() == "src/lib.rs")
        .expect("FMI-LS-Wasm library source is rendered");
    assert!(
        !library
            .content()
            .contains("__rumoca_artifact_identity_v1_fmu")
    );

    let builtin = templates::builtin_target("fmi-ls-wasm").expect("registered target bytes");
    let mutated = builtin
        .manifest
        .replace("required_artifact_identities = [\"fmu\"]\n", "");
    let templates = builtin
        .templates
        .iter()
        .map(|template| (template.path.to_owned(), template.source.to_owned()))
        .collect();
    let assets = ["wit", "upstream"]
        .into_iter()
        .map(|source| {
            let files = builtin
                .asset_files(source)
                .expect("registered FMI-LS-Wasm asset inventory")
                .into_iter()
                .map(|(path, bytes)| (path.to_owned(), bytes.to_vec()))
                .collect();
            (source.to_owned(), files)
        })
        .collect();
    let error = TargetBundle::check_in_memory(
        "identity-visibility-mutation".to_owned(),
        mutated,
        templates,
        assets,
    )
    .expect_err("a template identity use without its declaration must not construct");
    let diagnostic = format!("{error:#}");
    assert!(
        diagnostic.contains(
            "uses undeclared artifact identity scalars [__rumoca_artifact_identity_v1_fmu]"
        ),
        "{diagnostic}"
    );
    assert!(
        diagnostic.contains("lib.rs.jinja"),
        "identity dependency diagnostic must remain anchored to the failed template: {diagnostic}"
    );
}

#[test]
fn directory_target_uses_the_same_flattened_identity_analysis() {
    let directory = tempfile::tempdir().expect("isolated identity target directory");
    let template_path = directory.path().join("artifact.jinja");
    std::fs::write(&template_path, "{{ __rumoca_artifact_identity_v1_alpha }}")
        .expect("write flattened identity template");
    let manifest = r#"
version = 1
name = "directory-identity"
[[files]]
artifact_kind = "text"
semantic_context = "ast"
path = "artifact.txt"
template = "artifact.jinja"
id = "alpha"
required_artifact_identities = ["alpha"]
"#;
    directory_target_bundle(directory.path(), manifest.to_owned())
        .check()
        .expect("directory target uses the canonical flattened identity scalar");

    std::fs::write(&template_path, "{{ artifact.identities.alpha }}")
        .expect("write removed nested identity spelling");
    let error = directory_target_bundle(directory.path(), manifest.to_owned())
        .check()
        .expect_err("directory target must reject the removed identity map");
    assert!(
        format!("{error:#}").contains("uses the removed artifact.identities map"),
        "unexpected directory identity diagnostic: {error:#}"
    );
}

#[test]
fn checked_target_public_surface_has_no_foreign_member_join() {
    let authority_sources = [
        include_str!("../../../codegen_target.rs"),
        include_str!("../../checked_plan.rs"),
        include_str!("../bundle.rs"),
    ];
    let artifact_sources = [
        include_str!("../target_artifact.rs"),
        include_str!("../target_artifact/rendering.rs"),
    ];
    let strict_source = include_str!("../../../session/strict_compile_report.rs");
    for forbidden in [
        "pub fn template_source_for",
        "pub fn asset_files",
        "pub fn semantic_template_file",
        "pub fn declaration_index",
        "pub fn render_plan(",
        "pub enum CheckedTargetPackageMember",
        "pub fn algorithm_code_source(self) -> CheckedTargetPackageFileMemberRef",
        "pub struct TargetSnapshottedFile",
        "pub fn snapshot_render_authority",
        "pub fn validate_artifact_identity_dependencies",
        "pub fn members(&self) -> impl ExactSizeIterator<Item = CheckedTargetRenderStep",
        "pub fn members(&self) -> impl ExactSizeIterator<Item = CheckedTargetPackageMember",
        "declaration_index",
        "files[",
        "templates[",
        "with_member_brand<R>",
    ] {
        assert!(
            authority_sources
                .iter()
                .all(|source| !source.contains(forbidden)),
            "checked target authority must not expose foreign/index join `{forbidden}`"
        );
    }
    for required in [
        "fn into_plan(self) -> CheckedTargetRenderPlan",
        "fn into_product_plan(self) -> CheckedTargetPackageProductPlan",
        "struct CheckedTargetPackageAsset",
        "bytes: Arc<[u8]>",
        "source: Arc<str>",
        "relative_path: Arc<str>",
        "position: usize",
        "template: rumoca_phase_codegen::PackagedAlgorithmCodeTemplateSpec",
    ] {
        assert!(
            authority_sources
                .iter()
                .any(|source| source.contains(required)),
            "checked target authority lost construction boundary `{required}`"
        );
    }
    for required in [
        "struct PackagedAlgorithmCodeIssuer<'member, 'operation, 'inv>",
        "struct MemberBrand<'member>",
        "fn mint(_scope: &'member mut MemberScope) -> Self",
        "PhantomData<fn(&'member mut ()) -> &'member mut ()>",
        "fn close_rendered(",
        "session: &'operation ArtifactSession<'inv>",
        "enum PackageMemberCompletion<'member, 'operation, 'inv>",
        "enum UnpackagedMemberCompletion<'member>",
        "let completion = operation.close(MemberBrand::mint(&mut scope))?;",
        "Ok(completion.erase())",
    ] {
        assert!(
            artifact_sources
                .iter()
                .any(|source| source.contains(required)),
            "private target close lost construction boundary `{required}`"
        );
    }
    assert_eq!(
        artifact_sources
            .iter()
            .map(|source| source.matches("MemberBrand {").count())
            .sum::<usize>(),
        0,
        "member brands must be minted only through the scope-borrowing constructor"
    );
    assert_eq!(
        authority_sources
            .iter()
            .map(|source| {
                source
                    .matches("template_body: source.into_owned().into_boxed_str()")
                    .count()
            })
            .sum::<usize>(),
        1,
        "the exact analyzed template bytes must enter the private snapshot exactly once"
    );
    assert_artifact_identity_construction(&artifact_sources, strict_source);
}

#[test]
fn algorithm_code_target_requires_an_explicit_arithmetic_table() {
    let error = parse_target_manifest(
        r#"
version = 1
name = "missing-arithmetic"

[capabilities]
scalar_fallback = false

[[files]]
artifact_kind = "algorithm-code"
semantic_context = "galec"
path = "model.alg"
template = "model.alg.jinja"
"#,
    )
    .expect_err("Algorithm Code arithmetic cannot be omitted");

    assert!(
        format!("{error:#}").contains("must declare an [arithmetic] table"),
        "unexpected error: {error:#}"
    );
}

#[test]
fn algorithm_code_target_requires_an_explicit_source_real() {
    let error = parse_target_manifest(
        r#"
version = 1
name = "missing-real-format"

[arithmetic]
real_matrix_multiply = "separate_mul_add_ascending_positive_zero"

[capabilities]
scalar_fallback = false

[[files]]
artifact_kind = "algorithm-code"
semantic_context = "galec"
path = "model.alg"
template = "model.alg.jinja"
"#,
    )
    .expect_err("Algorithm Code Real format cannot be defaulted");

    assert!(
        format!("{error:#}").contains("missing field `source_real`"),
        "unexpected error: {error:#}"
    );
}

#[test]
fn algorithm_code_target_requires_an_explicit_source_integer() {
    let error = parse_target_manifest(
        r#"
version = 1
name = "missing-integer-domain"

[arithmetic]
source_real = "binary64"
real_matrix_multiply = "separate_mul_add_ascending_positive_zero"

[capabilities]
scalar_fallback = false

[[files]]
artifact_kind = "algorithm-code"
semantic_context = "galec"
path = "model.alg"
template = "model.alg.jinja"
"#,
    )
    .expect_err("Algorithm Code Integer semantics cannot be omitted");

    assert!(
        format!("{error:#}").contains("missing field `source_integer`"),
        "unexpected error: {error:#}"
    );
}

#[test]
fn algorithm_code_numeric_profile_denies_unknown_fields() {
    for (addition, rejected) in [
        ("real_format = \"binary64\"", "real_format"),
        ("allowed_reals = [\"binary64\"]", "allowed_reals"),
        ("allowed_integers = [\"i32\"]", "allowed_integers"),
    ] {
        let source = format!(
            r#"
version = 1
name = "unknown-numeric-profile-field"

[arithmetic]
source_real = "binary64"
source_integer = "i32"
real_matrix_multiply = "separate_mul_add_ascending_positive_zero"
{addition}

[capabilities]
scalar_fallback = false

[[files]]
artifact_kind = "algorithm-code"
semantic_context = "galec"
path = "model.alg"
template = "model.alg.jinja"
"#
        );
        let error = parse_target_manifest(&source)
            .expect_err("a second numeric authority cannot enter the checked profile");

        let rendered = format!("{error:#}");
        assert!(
            rendered.contains("unknown field") && rendered.contains(rejected),
            "unexpected error: {error:#}"
        );
    }
}

#[test]
fn algorithm_code_target_rejects_superseded_second_numeric_tables() {
    let integer_table = r#"
version = 1
name = "second-integer-authority"

[arithmetic]
source_real = "binary64"
source_integer = "i32"
real_matrix_multiply = "separate_mul_add_ascending_positive_zero"

[integer]
minimum = -2147483648
maximum = 2147483647

[capabilities]
scalar_fallback = false

[[files]]
artifact_kind = "algorithm-code"
semantic_context = "galec"
path = "model.alg"
template = "model.alg.jinja"
"#;
    let error = parse_target_manifest(integer_table)
        .expect_err("an independent Integer table cannot survive profile normalization");
    let rendered = format!("{error:#}");
    assert!(
        rendered.contains("unknown field") && rendered.contains("integer"),
        "unexpected error: {error:#}"
    );

    let numeric_table = correlated_efmu_manifest_source().replace(
        "[solve_executable.value_capabilities]",
        "[solve_executable.numeric]\nsource_real = \"binary32\"\nsource_integer = \"i32\"\n\n[solve_executable.value_capabilities]",
    );
    let error = parse_target_manifest(&numeric_table)
        .expect_err("Solve cannot carry a second normalized numeric profile");
    let rendered = format!("{error:#}");
    assert!(
        rendered.contains("unknown field") && rendered.contains("numeric"),
        "unexpected error: {error:#}"
    );
}

#[test]
fn non_algorithm_code_target_rejects_an_arithmetic_table() {
    let error = parse_target_manifest(
        r#"
version = 1
name = "misplaced-arithmetic"

[arithmetic]
source_real = "binary64"
source_integer = "i32"
real_matrix_multiply = "separate_mul_add_ascending_first_product"

[[files]]
artifact_kind = "text"
semantic_context = "solve"
path = "model.txt"
template = "model.txt.jinja"
"#,
    )
    .expect_err("non-Algorithm-Code targets cannot claim Algorithm Code arithmetic");

    assert!(
        format!("{error:#}").contains("must not declare an [arithmetic] table"),
        "unexpected error: {error:#}"
    );
}

#[test]
fn algorithm_code_target_constructs_each_declared_real_matrix_relation() {
    for (declared, expected) in [
        (
            "separate_mul_add_ascending_positive_zero",
            RealMatrixMultiplySemantics::SeparateMulAddAscendingPositiveZero,
        ),
        (
            "separate_mul_add_ascending_first_product",
            RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
        ),
    ] {
        let manifest = parse_target_manifest(&format!(
            r#"
version = 1
name = "explicit-arithmetic"

[arithmetic]
source_real = "binary32"
source_integer = "i32"
real_matrix_multiply = "{declared}"

[capabilities]
scalar_fallback = false

[[files]]
artifact_kind = "algorithm-code"
semantic_context = "galec"
path = "model.alg"
template = "model.alg.jinja"
"#
        ))
        .expect("declared arithmetic relation constructs a manifest");

        assert_eq!(
            manifest
                .algorithm_code_arithmetic()
                .expect("Algorithm Code manifests carry arithmetic")
                .real_matrix_multiply,
            expected
        );
    }
}

#[test]
fn algorithm_code_target_constructs_each_signed_source_integer_representation() {
    for (declared, expected) in [
        (
            "i8",
            rumoca_ir_galec::package::AlgorithmCodeIntegerFormat::I8,
        ),
        (
            "i16",
            rumoca_ir_galec::package::AlgorithmCodeIntegerFormat::I16,
        ),
        (
            "i32",
            rumoca_ir_galec::package::AlgorithmCodeIntegerFormat::I32,
        ),
        (
            "i64",
            rumoca_ir_galec::package::AlgorithmCodeIntegerFormat::I64,
        ),
    ] {
        let manifest = parse_target_manifest(&format!(
            r#"
version = 1
name = "explicit-source-integer"

[arithmetic]
source_real = "binary64"
source_integer = "{declared}"
real_matrix_multiply = "separate_mul_add_ascending_positive_zero"

[capabilities]
scalar_fallback = false

[[files]]
artifact_kind = "algorithm-code"
semantic_context = "galec"
path = "model.alg"
template = "model.alg.jinja"
"#
        ))
        .expect("a signed source Integer representation constructs");

        assert_eq!(
            manifest
                .algorithm_code_arithmetic()
                .expect("Algorithm Code manifests carry one numeric profile")
                .source_integer,
            expected
        );
    }
}

#[test]
fn each_source_specialization_changes_canonical_target_identity() {
    let source = r#"
version = 1
name = "numeric-identity"

[arithmetic]
source_real = "binary32"
source_integer = "i32"
real_matrix_multiply = "separate_mul_add_ascending_positive_zero"

[capabilities]
scalar_fallback = false

[[files]]
artifact_kind = "algorithm-code"
semantic_context = "galec"
path = "model.alg"
template = "model.alg.jinja"
"#;
    let base = parse_target_manifest(source).expect("base numeric profile");
    let binary64 = parse_target_manifest(&source.replace("binary32", "binary64"))
        .expect("changed source Real profile");
    let integer64 = parse_target_manifest(&source.replace("i32", "i64"))
        .expect("changed source Integer profile");
    let base_digest = base
        .canonical_artifact_identity_digest()
        .expect("base canonical target identity");

    assert_ne!(
        base_digest,
        binary64
            .canonical_artifact_identity_digest()
            .expect("Binary64 canonical target identity")
    );
    assert_ne!(
        base_digest,
        integer64
            .canonical_artifact_identity_digest()
            .expect("Integer64 canonical target identity")
    );
}

#[test]
fn builtin_algorithm_code_targets_pin_their_real_matrix_relation() {
    let (name, expected) = (
        "galec",
        RealMatrixMultiplySemantics::SeparateMulAddAscendingPositiveZero,
    );
    let target = templates::builtin_targets()
        .iter()
        .find(|target| target.name == name)
        .unwrap_or_else(|| panic!("built-in target `{name}` is missing"));
    let manifest = parse_target_manifest(target.manifest)
        .unwrap_or_else(|error| panic!("built-in target `{name}` must parse: {error}"));

    assert_eq!(
        manifest.required_product(),
        TargetRequiredProduct::AlgorithmCodePackage,
        "{name}"
    );
    assert_eq!(
        manifest
            .algorithm_code_arithmetic()
            .expect("Algorithm Code target must carry arithmetic")
            .real_matrix_multiply,
        expected,
        "built-in target `{name}` changed its value-affecting matrix relation"
    );
}

#[test]
fn dae_target_rejects_exact_algebraic_assignment_capability() {
    let error = parse_target_manifest(
        r#"
version = 1
name = "invalid-dae-capability"
readiness_level = 1

[capabilities]
scalar_fallback = false
exact_algebraic_assignments = true

[[files]]
artifact_kind = "text"
semantic_context = "dae"
path = "model.txt"
template = "model.txt.jinja"
"#,
    )
    .expect_err("only Solve-derived targets may consume exact algebraic schedules");
    assert!(
        error
            .to_string()
            .contains("exact_algebraic_assignments capability is only valid")
    );
}

#[test]
fn builtin_structured_dae_capability_matches_template_consumption() {
    for target in templates::builtin_targets() {
        let manifest = parse_target_manifest(target.manifest).unwrap_or_else(|err| {
            panic!("built-in target '{}' failed to parse: {err}", target.name)
        });
        let family_aware = manifest
            .capabilities
            .as_ref()
            .and_then(|capabilities| capabilities.structured_equation_families)
            == Some(true);
        if !family_aware {
            continue;
        }
        for owner_path in [
            "dae.systems.continuous.owners",
            "dae.systems.initialization.owners",
        ] {
            assert!(
                target
                    .templates
                    .iter()
                    .any(|template| template.source.contains(owner_path)),
                "built-in target '{}' declares structured family ownership but no template \
                     consumes checked owner projection `{owner_path}`",
                target.name
            );
        }
        assert!(
            target
                .templates
                .iter()
                .all(|template| !template.source.contains("dae.f_x")),
            "built-in target '{}' declares structured family ownership but reads the removed \
                 scalar residual field",
            target.name
        );
    }
}

#[test]
fn builtin_dae_consumers_use_only_the_checked_template_schema() {
    fn dae_root_fields(source: &str) -> impl Iterator<Item = &str> {
        source.match_indices("dae.").filter_map(|(start, _)| {
            let field = &source[start + "dae.".len()..];
            let end = field
                .find(|character: char| !character.is_ascii_alphanumeric() && character != '_')
                .unwrap_or(field.len());
            (end != 0).then_some(&field[..end])
        })
    }

    const CHECKED_ROOT_FIELDS: &[&str] = &[
        "schema",
        "value_types",
        "variables",
        "functions",
        "domains",
        "expressions",
        "modelica",
        "systems",
    ];
    let mut offenders = Vec::new();
    for target in templates::builtin_targets() {
        let manifest = parse_target_manifest(target.manifest).unwrap_or_else(|error| {
            panic!("built-in target '{}' failed to parse: {error}", target.name)
        });
        if matches!(
            manifest.required_product(),
            TargetRequiredProduct::Ast | TargetRequiredProduct::Flat
        ) {
            continue;
        }
        for template in target.templates {
            offenders.extend(
                dae_root_fields(template.source)
                    .filter(|field| !CHECKED_ROOT_FIELDS.contains(field))
                    .map(|field| format!("{}:{}:dae.{field}", target.name, template.path)),
            );
        }
    }
    assert!(
        offenders.is_empty(),
        "built-in templates still consume fields outside the checked DAE schema: \
             {offenders:#?}"
    );
}

#[test]
fn all_builtin_target_manifests_describe_matrix_axes() {
    for target in templates::builtin_targets() {
        let manifest = parse_target_manifest(target.manifest).unwrap_or_else(|err| {
            panic!("built-in target '{}' failed to parse: {err}", target.name)
        });
        assert!(
            manifest.execution_mode.is_some(),
            "built-in target '{}' must declare execution_mode",
            target.name
        );
        assert!(
            manifest.deployment_class.is_some(),
            "built-in target '{}' must declare deployment_class",
            target.name
        );
    }
}

#[test]
fn builtin_target_compatibility_matrix_reports_solve_tensor_fallback() {
    let matrix = builtin_target_compatibility_matrix()
        .expect("built-in target compatibility matrix should build");
    let mlir = matrix
        .iter()
        .find(|entry| entry.id == "mlir")
        .expect("mlir target should be listed");
    assert_eq!(mlir.readiness_level, Some(1));
    assert_eq!(mlir.forward_ad, TargetFeatureSupport::Native);
    assert_eq!(mlir.reverse_ad, TargetFeatureSupport::Unsupported);

    let rust_ode = matrix
        .iter()
        .find(|entry| entry.id == "rust-ode")
        .expect("rust-ode target should be listed");
    assert_eq!(rust_ode.readiness_level, Some(2));
    assert_eq!(rust_ode.matmul, TargetFeatureSupport::Scalar);

    let rust_fixed_ode = matrix
        .iter()
        .find(|entry| entry.id == "rust-fixed-ode")
        .expect("rust-fixed-ode target should be listed");
    assert_eq!(rust_fixed_ode.readiness_level, Some(2));
    assert_eq!(rust_fixed_ode.deployment_class.as_deref(), Some("cpu"));
    assert_eq!(rust_fixed_ode.execution_mode.as_deref(), Some("compiled"));
    assert_eq!(rust_fixed_ode.matmul, TargetFeatureSupport::Scalar);
    assert_eq!(rust_fixed_ode.linsolve, TargetFeatureSupport::Unsupported);
    assert_eq!(rust_fixed_ode.sparse, TargetFeatureSupport::Unsupported);
    assert_eq!(rust_fixed_ode.dtypes, vec!["f64"]);

    let cuda_ode = matrix
        .iter()
        .find(|entry| entry.id == "cuda-ode")
        .expect("cuda-ode target should be listed");
    assert_eq!(cuda_ode.readiness_level, Some(1));
    assert_eq!(cuda_ode.deployment_class.as_deref(), Some("gpu"));
    assert_eq!(cuda_ode.matmul, TargetFeatureSupport::Scalar);
    assert_eq!(cuda_ode.linsolve, TargetFeatureSupport::Unsupported);
    assert_eq!(cuda_ode.sparse, TargetFeatureSupport::Unsupported);
    assert_eq!(cuda_ode.dtypes, vec!["f64"]);

    let wgsl_ode = matrix
        .iter()
        .find(|entry| entry.id == "wgsl-ode")
        .expect("wgsl-ode target should be listed");
    assert_eq!(wgsl_ode.readiness_level, Some(0));
    assert_eq!(wgsl_ode.deployment_class.as_deref(), Some("gpu"));
    assert_eq!(wgsl_ode.matmul, TargetFeatureSupport::Scalar);
    assert_eq!(wgsl_ode.elementwise, TargetFeatureSupport::Native);
    assert_eq!(wgsl_ode.stencil, TargetFeatureSupport::Native);
}

#[test]
fn removed_analysis_targets_stay_absent() {
    let matrix = builtin_target_compatibility_matrix()
        .expect("built-in target compatibility matrix should build");
    for removed in [
        "casadi-mx",
        "casadi-sx",
        "jax",
        "julia-mtk",
        "onnx",
        "symforce",
        "sympy",
    ] {
        assert!(
            matrix.iter().all(|entry| entry.id != removed),
            "target `{removed}` consumed the removed DAE template schema"
        );
    }
}

#[test]
fn builtin_fmi_targets_report_the_exact_checked_fmi_view() {
    let matrix = builtin_target_compatibility_matrix()
        .expect("built-in target compatibility matrix should build");
    for fmi in ["fmi2", "fmi3", "fmi-ls-wasm"] {
        let entry = matrix
            .iter()
            .find(|entry| entry.id == fmi)
            .unwrap_or_else(|| panic!("{fmi} target should be listed"));
        assert_eq!(entry.required_product, TargetRequiredProduct::FmiComponent);
        let target = templates::builtin_target(fmi)
            .unwrap_or_else(|| panic!("{fmi} built-in target should exist"));
        let manifest = parse_target_manifest(target.manifest)
            .unwrap_or_else(|error| panic!("{fmi} manifest should parse: {error}"));
        assert!(manifest.files().iter().all(|file| {
            file.semantic_context() == super::super::TargetSemanticContext::Solve
                && file.semantic_view() == TargetSemanticView::FmiComponent
        }));
        let expected_deployment = if fmi == "fmi-ls-wasm" {
            "wasm-component"
        } else {
            "fmu"
        };
        assert_eq!(entry.deployment_class.as_deref(), Some(expected_deployment));
        assert_eq!(entry.events, TargetFeatureSupport::Unsupported);
    }
}

#[test]
fn removed_file_render_context_is_rejected() {
    parse_target_manifest(
        r#"
version = 1
name = "removed-context"

[[files]]
artifact_kind = "xml"
semantic_context = "solve"
path = "modelDescription.xml"
template = "modelDescription.xml.jinja"
render_context = "fmi-model-description"
"#,
    )
    .expect_err("removed per-file render contexts must not parse");
}

#[test]
fn target_manifest_parses_solve_tensor_capabilities() {
    let manifest = parse_manifest_with_context_capabilities(
        "solve",
        r#"
[capabilities]
scalar_fallback = true

[capabilities.tensor]
matmul = "native"
linsolve = "scalar"
stencil = "native"
layout = "row-major"
supports_dynamic_shapes = false
sparse = false
dtypes = ["f32", "f64"]
"#,
    );
    assert_eq!(
        manifest.required_product(),
        TargetRequiredProduct::SolveModel
    );
    let capabilities = manifest.capabilities.expect("capabilities table");
    let tensor = capabilities.tensor.expect("tensor capabilities");

    assert!(capabilities.scalar_fallback);
    assert_eq!(tensor.matmul, Some(TensorCapability::Native));
    assert_eq!(tensor.linsolve, Some(TensorCapability::Scalar));
    assert_eq!(tensor.stencil, Some(TensorCapability::Native));
    assert_eq!(tensor.layout, Some(TensorLayoutCapability::RowMajor));
    assert_eq!(tensor.supports_dynamic_shapes, Some(false));
    assert_eq!(tensor.sparse, Some(false));
    assert_eq!(
        tensor.dtypes,
        Some(vec!["f32".to_string(), "f64".to_string()])
    );
}

#[test]
fn target_manifest_rejects_omitted_scalar_fallback_policy() {
    let source = r#"
version = 1
name = "missing-scalar-policy"

[capabilities]
events = false

[[files]]
artifact_kind = "text"
semantic_context = "solve"
path = "model.txt"
template = "model.txt.jinja"
"#;
    let error = toml::from_str::<TargetManifest>(source)
        .expect_err("omitting scalar_fallback must not license scalarization");
    assert!(error.to_string().contains("scalar_fallback"), "{error}");
}

#[test]
fn solve_target_rejects_event_partition_without_event_support() {
    let mut events = rumoca_ir_solve::SolveEventPartition::default();
    events.scheduled_time_events.push(1.0);
    let solve = rumoca_ir_solve::SolveProblem::construct(
        rumoca_ir_solve::VarLayout::default(),
        rumoca_ir_solve::SolveLayout::default(),
        empty_continuous_system(),
        rumoca_ir_solve::InitializationSolveSystem::empty(),
        rumoca_ir_solve::DiscreteSolveSystem::default(),
        events,
        rumoca_ir_solve::SolveClockPartition::default(),
    )
    .expect("one scheduled event is valid checked Solve input");
    let manifest = parse_manifest_with_context_capabilities(
        "solve",
        r#"
[capabilities]
scalar_fallback = false
events = false
"#,
    );
    let capabilities = manifest.capabilities.as_ref().expect("capabilities");

    let error = validate_solve_target_capabilities(&solve, &manifest, capabilities)
        .expect_err("an event-free target must reject an event partition");

    assert!(error.to_string().contains("unsupported-feature:events"));
}

#[test]
fn explicit_rhs_target_rejects_required_algebraic_projection() {
    let solve = solve_with_residual_algebraic_projection();
    let manifest = parse_manifest_with_context_capabilities(
        "solve",
        r#"
[capabilities]
scalar_fallback = false
residual_equations = false
"#,
    );
    let capabilities = manifest.capabilities.as_ref().expect("capabilities");

    let error = validate_solve_target_capabilities(&solve, &manifest, capabilities)
        .expect_err("an explicit RHS target must reject an algebraic projection");

    assert!(
        error
            .to_string()
            .contains("unsupported-feature:residual_equations")
    );
}

#[test]
fn explicit_rhs_target_accepts_issued_exact_algebraic_assignments() {
    let solve = solve_with_issued_exact_algebraic_assignment();
    let target = templates::builtin_targets()
        .iter()
        .find(|target| target.name == "fmi3")
        .expect("fmi3 target is built in");
    let manifest = parse_target_manifest(target.manifest).expect("fmi3 manifest is valid");
    let capabilities = manifest.capabilities.as_ref().expect("capabilities");

    validate_solve_target_capabilities(&solve, &manifest, capabilities)
        .expect("an issued exact assignment schedule needs no residual solver");
}

#[test]
fn target_without_exact_assignment_consumer_rejects_exact_algebraic_schedule() {
    let solve = solve_with_issued_exact_algebraic_assignment();
    let target = templates::builtin_targets()
        .iter()
        .find(|target| target.name == "rust-ode")
        .expect("rust-ode target is built in");
    let manifest = parse_target_manifest(target.manifest).expect("rust-ode manifest is valid");
    let capabilities = manifest.capabilities.as_ref().expect("capabilities");

    let error = validate_solve_target_capabilities(&solve, &manifest, capabilities)
        .expect_err("a target that does not consume exact schedules must fail closed");
    assert!(
        error
            .to_string()
            .contains("unsupported-feature:residual_equations")
    );
}

#[test]
fn exact_assignment_consumer_rejects_incomplete_algebraic_schedule() {
    let solve = solve_with_residual_algebraic_projection();
    let target = templates::builtin_targets()
        .iter()
        .find(|target| target.name == "fmi3")
        .expect("fmi3 target is built in");
    let manifest = parse_target_manifest(target.manifest).expect("fmi3 manifest is valid");
    let capabilities = manifest.capabilities.as_ref().expect("capabilities");

    let error = validate_solve_target_capabilities(&solve, &manifest, capabilities)
        .expect_err("an incomplete schedule must still require residual support");
    assert!(
        error
            .to_string()
            .contains("unsupported-feature:residual_equations")
    );
}

#[test]
fn residual_kernel_target_accepts_algebraic_projection_contract() {
    let solve = solve_with_residual_algebraic_projection();
    let manifest = parse_manifest_with_context_capabilities(
        "solve",
        r#"
[capabilities]
scalar_fallback = false
residual_equations = true
"#,
    );
    let capabilities = manifest.capabilities.as_ref().expect("capabilities");

    validate_solve_target_capabilities(&solve, &manifest, capabilities)
        .expect("a residual-kernel target may expose the projection contract");
}

#[test]
fn fmi_projection_defers_residual_classification_until_checked_solve() {
    let dae = dae_with_placeholder_family();
    let manifest = parse_manifest_with_context_capabilities(
        "solve",
        r#"
[capabilities]
scalar_fallback = false
continuous_states = true
residual_equations = false
"#,
    );
    let capabilities = manifest.capabilities.as_ref().expect("capabilities");

    validate_dae_target_capabilities(&dae, &manifest, capabilities)
        .expect("FMI must classify derivative and algebraic rows from checked Solve");
}

#[test]
fn algorithm_code_defers_algebraic_owner_classification_to_its_projection() {
    let dae = dae_with_placeholder_family();
    let manifest = parse_algorithm_code_manifest_with_capabilities(
        r#"
[capabilities]
scalar_fallback = false
continuous_states = false
residual_equations = false
"#,
    );
    let capabilities = manifest.capabilities.as_ref().expect("capabilities");

    validate_dae_target_capabilities(&dae, &manifest, capabilities)
        .expect("Algorithm Code must classify algebraic owners in its checked projection");
}

fn checked_terminal_event_partition() -> rumoca_ir_solve::SolveEventPartition {
    let empty_program_block = || {
        rumoca_ir_solve::ScalarProgramBlock::with_program_spans(Vec::new(), Vec::new())
            .expect("an empty fixture program block is valid")
    };
    rumoca_ir_solve::SolveEventPartition {
        root_conditions: empty_program_block(),
        root_relation_memory_targets: Vec::new(),
        root_zero_domains: Vec::new(),
        root_relation_refresh_roles: Vec::new(),
        condition_memory_parameter_indices: Vec::new(),
        scheduled_root_conditions: Vec::new(),
        scheduled_time_events: Vec::new(),
        dynamic_time_event_names: Vec::new(),
        dynamic_time_event_rhs: empty_program_block(),
        action_conditions: empty_program_block(),
        actions: Vec::new(),
        has_terminal_event: true,
        delays: rumoca_ir_solve::SolveDelayPartition {
            source_rhs: empty_program_block(),
            delay_time_rhs: empty_program_block(),
            delay_max_rhs: empty_program_block(),
            value_parameter_indices: Vec::new(),
            source_is_discrete: Vec::new(),
        },
    }
}

#[test]
fn solve_target_rejects_terminal_runtime_without_runtime_event_support() {
    let solve = rumoca_ir_solve::SolveProblem::construct(
        rumoca_ir_solve::VarLayout::from_parts(indexmap::IndexMap::new(), 0, 1),
        rumoca_ir_solve::SolveLayout {
            terminal_event_parameter_index: Some(0),
            ..rumoca_ir_solve::SolveLayout::default()
        },
        empty_continuous_system(),
        rumoca_ir_solve::InitializationSolveSystem::empty(),
        rumoca_ir_solve::DiscreteSolveSystem::default(),
        checked_terminal_event_partition(),
        rumoca_ir_solve::SolveClockPartition::default(),
    )
    .expect("the terminal-event parameter and event flag form one checked root");
    let manifest = parse_manifest_with_context_capabilities(
        "solve",
        r#"
[capabilities]
scalar_fallback = false
events = true
runtime_events = false
"#,
    );
    let capabilities = manifest.capabilities.as_ref().expect("capabilities");

    let error = validate_solve_target_capabilities(&solve, &manifest, capabilities)
        .expect_err("a runtime-event-free target must reject termination");

    assert!(
        error
            .to_string()
            .contains("unsupported-feature:runtime_events"),
        "unexpected error: {error:#}"
    );
}

#[test]
fn solve_target_rejects_clock_partition_without_clock_support() {
    let mut clocks = rumoca_ir_solve::SolveClockPartition::default();
    let lattice = rumoca_core::ClockLattice::from_interval_counter(1, 1)
        .expect("the fixture's one-second lattice is exact");
    clocks.periodic_event_schedules.push(
        rumoca_ir_solve::PeriodicEventSchedule::new(lattice)
            .expect("the fixture's one-second periodic schedule is valid"),
    );
    clocks.activation_parameter_indices.push(0);
    let solve = rumoca_ir_solve::SolveProblem::construct(
        rumoca_ir_solve::VarLayout::from_parts(indexmap::IndexMap::new(), 0, 1),
        rumoca_ir_solve::SolveLayout::default(),
        empty_continuous_system(),
        rumoca_ir_solve::InitializationSolveSystem::empty(),
        rumoca_ir_solve::DiscreteSolveSystem::default(),
        rumoca_ir_solve::SolveEventPartition::default(),
        clocks,
    )
    .expect("one periodic schedule with one activation lane is a checked root");
    let manifest = parse_manifest_with_context_capabilities(
        "solve",
        r#"
[capabilities]
scalar_fallback = false
events = true
clocks = false
"#,
    );
    let capabilities = manifest.capabilities.as_ref().expect("capabilities");

    let error = validate_solve_target_capabilities(&solve, &manifest, capabilities)
        .expect_err("a clock-free target must reject a clock partition");

    assert!(
        error.to_string().contains("unsupported-feature:clocks"),
        "unexpected error: {error:#}"
    );
}

/// The invariant that licenses `dae_has_external_functions` to `expect` its
/// way through the function table instead of carrying a fail-closed arm: a
/// reserved-but-undefined function cannot reach a finalized `Dae`, so there is
/// no "unreadable body" state for the probe to be conservative about.
#[test]
fn an_undefined_function_cannot_reach_a_finalized_dae() {
    let source_text = "function f input Real u; output Real y; end f;";
    let mut source_map = SourceMap::new();
    let source_id = source_map.add("undefined-function.mo", source_text);
    let at = DaeProvenance::source(rumoca_core::Span::from_offsets(
        source_id,
        0,
        source_text.len(),
    ))
    .expect("fixture source span is exact");

    let error = Dae::construct(source_map, |dae| {
        let real = dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), at))?;
        dae.function(
            FunctionSignature::new(VarName::new("f"), [real], [real], at),
            // Reserve the function and never define a body for it.
            |_, _| Ok(()),
        )
        .map(|_| ())
    })
    .expect_err("a function reserved without a body must not finalize");

    assert!(
        matches!(
            error,
            rumoca_ir_dae::DaeConstructionError::IncompleteDefinition {
                kind: "function",
                ..
            }
        ),
        "expected an incomplete-definition rejection, got: {error:?}"
    );
}

/// SEV-155 NEGATIVE. The gate at `codegen_target.rs` must FIRE for a DAE that
/// carries an external interface. This test cannot pass against the former
/// constant-`false` probe.
#[test]
fn dae_target_rejects_external_interface_without_external_function_support() {
    let dae = dae_with_external_function();
    let manifest = manifest_with_capabilities(
        r#"
[capabilities]
scalar_fallback = false
residual_equations = true
external_functions = false
"#,
    );
    let capabilities = manifest.capabilities.as_ref().expect("capabilities");

    let error = validate_dae_target_capabilities(&dae, &manifest, capabilities)
        .expect_err("a target without external-function support must reject an external interface");

    assert!(
        error
            .to_string()
            .contains("unsupported-feature:external_functions"),
        "external-function capability must fail closed, got: {error}"
    );
}

/// SEV-156's total-refusal arm: a capability declaration cannot admit syntax
/// for which Solve has no Invoke/Effect grammar.
#[test]
fn dae_target_rejects_external_interface_even_when_the_capability_is_declared() {
    let dae = dae_with_external_function();
    let manifest = manifest_with_capabilities(
        r#"
[capabilities]
scalar_fallback = false
residual_equations = true
external_functions = true
"#,
    );
    let capabilities = manifest.capabilities.as_ref().expect("capabilities");

    let error = validate_dae_target_capabilities(&dae, &manifest, capabilities)
        .expect_err("an external-function capability cannot bypass the current grammar refusal");

    assert!(
        error
            .to_string()
            .contains("unsupported-feature:external_functions"),
        "external-function refusal must be exact, got: {error}"
    );
}

/// The POSITIVE guard against an over-firing probe: a DAE whose function table
/// is nonempty but holds only a Modelica body must stay admissible on an
/// `external_functions = false` target. A derivation mutated to a constant
/// `true` turns this red.
#[test]
fn external_function_free_dae_stays_admissible_without_external_function_support() {
    let dae = dae_with_modelica_function();
    let manifest = manifest_with_capabilities(
        r#"
[capabilities]
scalar_fallback = false
residual_equations = true
external_functions = false
"#,
    );
    let capabilities = manifest.capabilities.as_ref().expect("capabilities");

    validate_dae_target_capabilities(&dae, &manifest, capabilities)
        .expect("a DAE with no external interface must not trip the external-function gate");
}

/// Every built-in target is accounted for without a permissive skip. MLIR is
/// Solve-only, so its manifest does not fabricate a DAE capability; the shared
/// pre-Solve DAE boundary still rejects the external interface.
#[test]
fn every_builtin_target_rejects_an_external_interface() {
    let dae = dae_with_external_function();
    for target in templates::builtin_targets() {
        let manifest = parse_target_manifest(target.manifest).unwrap_or_else(|err| {
            panic!("built-in target '{}' failed to parse: {err}", target.name)
        });
        let capabilities = manifest
            .capabilities
            .as_ref()
            .unwrap_or_else(|| panic!("built-in target '{}' lacks capabilities", target.name));
        if target.name == "mlir" {
            assert_eq!(
                manifest.required_product(),
                TargetRequiredProduct::SolveModel
            );
            assert!(
                manifest
                    .files()
                    .iter()
                    .all(|file| file.semantic_context() == TargetSemanticContext::Solve),
                "MLIR's absent DAE capability is licensed only by exact Solve-only ownership"
            );
            assert_eq!(capabilities.external_functions, None);
        } else {
            assert_eq!(
                capabilities.external_functions,
                Some(false),
                "built-in target '{}' must configure current external-function refusal",
                target.name
            );
        }
        let error = validate_dae_target_capabilities(&dae, &manifest, capabilities)
            .expect_err("no built-in target supports external functions");
        assert!(
            error
                .to_string()
                .contains("unsupported-feature:external_functions"),
            "built-in target '{}' must reject on external_functions, got: {error}",
            target.name
        );
    }
}

/// A resolved local source witness reaches the same boundary. The DAE retains
/// its source-backed function owner span, and rendering refuses before Solve
/// lowering.
#[test]
fn resolved_local_external_function_reaches_capability_refusal_with_source_span() {
    let compilation = strict_external_function_compilation();
    let (span, source_name, source_text) = compilation.result().dae.inspect(|view| {
        let external = (0..view.function_count()).find_map(|index| {
            let function = view.function(view.function_id(index)?)?;
            function
                .external()
                .map(|external| external.provenance().span())
        });
        let span = external.expect("resolved local external function reaches checked DAE");
        let (source_name, source_text) = compilation
            .result()
            .dae
            .source_map()
            .get_source(span.source)
            .expect("external provenance resolves in the DAE source map");
        (span, source_name.to_owned(), source_text.to_string())
    });
    assert_eq!(source_name, "external-capability-source.mo");
    assert!(!span.is_dummy());
    assert!(
        source_text
            .get(span.start.0..span.end.0)
            .is_some_and(|owner| !owner.is_empty()),
        "external provenance must be a valid non-empty source owner span"
    );

    let target = TargetBundle::builtin("mlir")
        .expect("MLIR is a registered Solve-only target")
        .check()
        .expect("MLIR target bytes and manifest are checked");
    let error = match compilation.render_target(target, test_artifact_input()) {
        Ok(_) => panic!("external source must refuse before MLIR Solve lowering"),
        Err(error) => error,
    };
    assert!(
        error
            .to_string()
            .contains("unsupported-feature:external_functions"),
        "source witness must reach the exact capability diagnostic: {error:#}"
    );
}

mod semantic_capability_cases;
