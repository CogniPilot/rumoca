use super::*;
use crate::codegen_target::descriptors::{CapabilityTarget, validate_solve_tensor_feature};

#[test]
fn dae_target_must_declare_structured_family_consumption() {
    let dae = dae_with_placeholder_family();
    let manifest = manifest_with_capabilities(
        r#"
[capabilities]
scalar_fallback = false
residual_equations = true
"#,
    );
    let capabilities = manifest.capabilities.as_ref().expect("capabilities");

    let error = validate_dae_target_capabilities(&dae, &manifest, capabilities)
        .expect_err("scalar-only DAE target must reject placeholder rows");

    assert!(
        error
            .to_string()
            .contains("unsupported-feature:structured_equation_families")
    );
}

#[test]
fn family_aware_dae_target_accepts_canonical_structured_owner() {
    let dae = dae_with_placeholder_family();
    let manifest = manifest_with_capabilities(
        r#"
[capabilities]
scalar_fallback = false
residual_equations = true
structured_equation_families = true
"#,
    );
    let capabilities = manifest.capabilities.as_ref().expect("capabilities");

    validate_dae_target_capabilities(&dae, &manifest, capabilities)
        .expect("declared family-aware target may consume the compact owner");
}

#[test]
fn target_manifest_rejects_tensor_capabilities_without_a_solve_product() {
    let err = parse_target_manifest(
        r#"
version = 1
name = "custom"

[capabilities]
scalar_fallback = false

[capabilities.tensor]
matmul = "native"

[[files]]
artifact_kind = "text"
semantic_context = "dae"
path = "model.txt"
template = "model.txt.jinja"
"#,
    )
    .expect_err("tensor capabilities should require a Solve-owned product");

    assert!(
        err.to_string()
            .contains("tensor capabilities are only valid")
    );
}

#[test]
fn target_manifest_rejects_scalar_tensor_ops_without_scalar_fallback() {
    let manifest = parse_manifest_with_context_capabilities(
        "solve",
        r#"
[capabilities]
scalar_fallback = false

[capabilities.tensor]
matmul = "native"
linsolve = "native"
"#,
    );
    let capabilities = manifest.capabilities.as_ref().expect("capabilities");
    assert!(!capabilities.scalar_fallback);

    let err = parse_target_manifest(
        r#"
version = 1
name = "custom"

[capabilities]
scalar_fallback = false

[capabilities.tensor]
matmul = "scalar"

[[files]]
artifact_kind = "text"
semantic_context = "solve"
path = "model.txt"
template = "model.txt.jinja"
"#,
    )
    .expect_err("scalar tensor op should require scalar fallback");

    assert!(err.to_string().contains("scalar_fallback = false"));
}

#[test]
fn tensor_capability_refusal_retains_the_typed_codegen_error() {
    let source_span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("tensor-capability.mo"),
        12,
        18,
    );
    let error = validate_solve_tensor_feature(
        CapabilityTarget::for_solve_product("typed-target", TargetRequiredProduct::SolveModel),
        "tensor.matmul",
        "MatMul",
        1,
        Some(TensorCapability::Unsupported),
        false,
        Some(source_span),
    )
    .expect_err("an unsupported tensor node must be refused");

    assert!(matches!(
        error.downcast_ref::<rumoca_phase_codegen::CodegenError>(),
        Some(rumoca_phase_codegen::CodegenError::UnsupportedTargetFeature {
            target,
            feature: "tensor.matmul",
            detail,
            span: Some(span),
        }) if target == "typed-target"
            && detail == "MatMul nodes are present but the target declares tensor.matmul unsupported"
            && *span == source_span
    ));
}

#[test]
fn solve_side_refusal_in_a_mixed_product_is_not_attributed_to_galec() {
    let error = validate_solve_tensor_feature(
        CapabilityTarget::for_solve_product(
            "mixed-product",
            TargetRequiredProduct::SolveAlgorithmProduct,
        ),
        "tensor.matmul",
        "MatMul",
        1,
        Some(TensorCapability::Unsupported),
        false,
        None,
    )
    .expect_err("the Solve-owned Production Code arm must reject the tensor node");

    assert!(matches!(
        error.downcast_ref::<rumoca_phase_codegen::CodegenError>(),
        Some(rumoca_phase_codegen::CodegenError::UnsupportedTargetFeature {
            target,
            feature: "tensor.matmul",
            detail,
            span: None,
        }) if target == "mixed-product"
            && detail == "MatMul nodes are present but the target declares tensor.matmul unsupported"
            && !detail.contains("GALEC")
    ));
}

#[test]
fn target_manifest_rejects_invalid_readiness_level() {
    let err = parse_target_manifest(
        r#"
version = 1
name = "invalid"
readiness_level = 6

[[files]]
artifact_kind = "text"
semantic_context = "solve"
path = "model.txt"
template = "model.txt.jinja"
"#,
    )
    .expect_err("readiness level above 5 should fail");

    assert!(err.to_string().contains("readiness_level"), "{err}");
}

#[test]
fn target_manifest_rejects_manifest_only_readiness_zero() {
    let error = parse_target_manifest(
        r#"
version = 1
name = "future-target"
readiness_level = 0
"#,
    )
    .expect_err("readiness level 0 must not permit a manifest-only placeholder");

    assert!(error.to_string().contains("file entry"), "{error}");
}

#[test]
fn target_manifest_rejects_missing_files_after_readiness_zero() {
    let err = parse_target_manifest(
        r#"
version = 1
name = "unfinished"
readiness_level = 1
"#,
    )
    .expect_err("every target requires generated files");

    assert!(err.to_string().contains("file entry"), "{err}");
}

#[test]
fn target_manifest_rejects_empty_tensor_dtype() {
    let err = parse_target_manifest(
        r#"
version = 1
name = "invalid-dtypes"

[capabilities]
scalar_fallback = false

[capabilities.tensor]
dtypes = ["f64", ""]

[[files]]
artifact_kind = "text"
semantic_context = "solve"
path = "model.txt"
template = "model.txt.jinja"
"#,
    )
    .expect_err("empty tensor dtype should fail");

    assert!(err.to_string().contains("dtypes"), "{err}");
}

#[test]
fn target_manifest_rejects_removed_capability_aliases() {
    for alias in ["requirements", "requires"] {
        let source = format!(
            r#"
version = 1
name = "removed-capability-alias"

[{alias}]
continuous_states = false

[[files]]
artifact_kind = "text"
semantic_context = "dae"
path = "model.txt"
template = "model.txt.jinja"
"#
        );
        let error = match parse_target_manifest(&source) {
            Ok(_) => panic!("removed [{alias}] alias must be rejected"),
            Err(error) => error,
        };
        assert!(
            error.to_string().contains("unknown field"),
            "unexpected error for [{alias}]: {error:#}"
        );
    }
}

#[test]
fn galec_context_admits_only_algorithm_code_and_xml_artifacts() {
    for (artifact_kind, path) in [
        ("c-source", "model.c"),
        ("c-header", "model.h"),
        ("cuda-source", "model.cu"),
        ("markdown", "model.md"),
    ] {
        let source = format!(
            r#"
version = 1
name = "forbidden-algorithm-code-c"

[arithmetic]
source_real = "binary32"
source_integer = "i32"
real_matrix_multiply = "separate_mul_add_ascending_positive_zero"

[capabilities]
scalar_fallback = false

[[files]]
artifact_kind = "{artifact_kind}"
semantic_context = "galec"
path = "{path}"
template = "model.jinja"
"#
        );
        expect_target_error(&source, "forbidden semantic_context");
    }
}

#[test]
fn artifact_kind_is_bound_to_one_output_suffix() {
    for (artifact_kind, path, suffix) in [
        ("text", "model.c", ".txt"),
        ("c-source", "model.txt", ".c"),
        ("c-header", "model.c", ".h"),
    ] {
        let source = format!(
            r#"
version = 1
name = "lying-artifact-kind"

[[files]]
artifact_kind = "{artifact_kind}"
semantic_context = "solve"
path = "{path}"
template = "model.jinja"
"#
        );
        expect_target_error(&source, suffix);
    }
}

#[test]
fn rendered_file_contract_fields_are_mandatory_and_closed() {
    for (label, declaration, needle) in [
        (
            "missing artifact kind",
            r#"semantic_context = "solve""#,
            "artifact_kind",
        ),
        (
            "missing semantic context",
            r#"artifact_kind = "text""#,
            "semantic_context",
        ),
        (
            "unknown artifact kind",
            r#"artifact_kind = "object-code"
semantic_context = "solve""#,
            "unknown variant",
        ),
        (
            "unknown semantic context",
            r#"artifact_kind = "text"
semantic_context = "inferred""#,
            "unknown variant",
        ),
    ] {
        let source = format!(
            r#"
version = 1
name = "closed-file-contract"

[[files]]
{declaration}
path = "model.txt"
template = "model.txt.jinja"
"#
        );
        let error = parse_target_manifest(&source)
            .err()
            .unwrap_or_else(|| panic!("{label} must be rejected"));
        assert!(
            error.to_string().contains(needle),
            "unexpected error for {label}: {error:#}"
        );
    }
}

#[test]
fn global_partial_and_shared_alias_fields_are_rejected() {
    expect_target_error(
        r#"
version = 1
name = "forbidden-global-partial"

[[files]]
artifact_kind = "text"
semantic_context = "solve"
path = "model.txt"
template = "model.txt.jinja"

[[partials]]
template = "support.jinja"
name = "support.jinja"
semantic_context = "galec"
"#,
        "unknown field",
    );
    expect_target_error(
        r#"
version = 1
name = "forbidden-shared-alias"

[[files]]
artifact_kind = "text"
semantic_context = "solve"
path = "model.txt"
template = "model.txt.jinja"
shared_as = "global-template.jinja"
"#,
        "unknown field",
    );
}

#[test]
fn packaged_algorithm_code_issues_one_complete_exact_layout() {
    let target = super::super::super::TargetBundle::builtin("galec")
        .expect("registered packaged Algorithm Code target")
        .check()
        .expect("complete standalone package role family must construct");
    let super::super::super::CheckedTargetBundle {
        render_authority, ..
    } = target;
    let render_plan = render_authority.into_plan();
    let super::super::super::CheckedTargetRenderPlan::Packaged(package_plan) = render_plan else {
        panic!("packaged Algorithm Code must issue package authority")
    };
    let super::super::super::CheckedTargetPackageProductPlan::AlgorithmCode(product_plan) =
        package_plan.into_product_plan()
    else {
        panic!("packaged Algorithm Code must issue its narrowed product layout")
    };
    let (_policy, _capability, arithmetic, layout, _fold) = product_plan.into_parts();
    assert_eq!(
        arithmetic.real_matrix_multiply,
        rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingPositiveZero
    );
    assert_eq!(
        layout
            .members()
            .map(|member| match member {
                super::super::super::CheckedAlgorithmCodeLayoutMember::AlgorithmCode {
                    role,
                    path,
                } => {
                    (role, path.as_str())
                }
            })
            .collect::<Vec<_>>(),
        vec![
            (
                rumoca_phase_codegen::AlgorithmCodeArtifactRole::AlgorithmCodeSource,
                "AlgorithmCode/model.alg",
            ),
            (
                rumoca_phase_codegen::AlgorithmCodeArtifactRole::AlgorithmCodeManifest,
                "AlgorithmCode/manifest.xml",
            ),
            (
                rumoca_phase_codegen::AlgorithmCodeArtifactRole::PackageManifest,
                "__content.xml",
            ),
        ]
    );
}

#[test]
fn packaged_algorithm_code_rejects_partial_wrong_dynamic_and_colliding_roles() {
    let complete = packaged_algorithm_code_manifest_source();
    for (source, expected) in [
        (
            complete.replacen("product_role = \"package-manifest\"\n", "", 1),
            "must declare one closed product_role",
        ),
        (
            complete.replacen(
                "product_role = \"algorithm-code-manifest\"",
                "product_role = \"production-manifest\"",
                1,
            ),
            "cannot declare product_role = 'production-manifest'",
        ),
        (
            complete.replacen(
                "path = \"AlgorithmCode/model.alg\"",
                "path = \"AlgorithmCode/{{ model_name }}.alg\"",
                1,
            ),
            "must be a static path",
        ),
        (
            complete.replacen(
                "artifact_kind = \"xml\"\nsemantic_context = \"galec\"\nproduct_role = \"algorithm-code-manifest\"",
                "artifact_kind = \"algorithm-code\"\nsemantic_context = \"galec\"\nproduct_role = \"algorithm-code-manifest\"",
                1,
            ),
            "must end in '.alg'",
        ),
        (
            complete.replacen(
                "semantic_context = \"galec\"\nproduct_role = \"algorithm-code-manifest\"",
                "semantic_context = \"solve\"\nproduct_role = \"algorithm-code-manifest\"",
                1,
            ),
            "incompatible checked IR views",
        ),
    ] {
        let error = parse_target_manifest(&source).expect_err("invalid package roles must reject");
        assert!(
            format!("{error:#}").contains(expected),
            "unexpected error for expected `{expected}`: {error:#}"
        );
    }

    let builtin = templates::builtin_target("galec").expect("registered GALEC target");
    let templates = builtin
        .templates
        .iter()
        .map(|template| (template.path.to_owned(), template.source.to_owned()))
        .collect();
    let schemas = builtin
        .asset_files("schemas")
        .expect("GALEC schema inventory")
        .into_iter()
        .map(|(path, bytes)| (path.to_owned(), bytes.to_vec()))
        .collect();
    let error = super::super::super::TargetBundle::check_in_memory(
        "wrong-standard-path".to_owned(),
        builtin.manifest.replacen(
            "path = \"__content.xml\"",
            "path = \"wrong/MANIFEST.xml\"",
            1,
        ),
        templates,
        BTreeMap::from([("schemas".to_owned(), schemas)]),
    )
    .expect_err("wrong standard package-root path must reject checked construction");
    assert!(
        format!("{error:#}").contains("complete packaged Algorithm Code member layout"),
        "{error:#}"
    );
}

#[test]
fn generic_algorithm_code_is_source_only_and_has_no_package_layout() {
    let source = algorithm_code_manifest_with_capabilities_source("[capabilities]");
    let construction = super::super::super::parse_target_manifest_construction(&source)
        .expect("source-only Algorithm Code construction");
    let manifest = construction.manifest;
    let super::super::super::TargetDeclaredRenderAuthority::Unpackaged(steps) =
        construction.render_authority
    else {
        panic!("source-only Algorithm Code must not construct package authority")
    };
    let super::super::super::TargetPreparedMemberPlan::AlgorithmCodeSource {
        output_path_template,
    } = &steps[0].prepared_member
    else {
        panic!("source-only declaration must retain its typed route")
    };
    assert_eq!(output_path_template.as_str(), "model.alg");
    assert_eq!(
        manifest.required_product(),
        TargetRequiredProduct::AlgorithmCodePackage
    );

    let source = format!(
        "{}\n{}",
        algorithm_code_manifest_with_capabilities_source("[capabilities]"),
        r#"
[[files]]
artifact_kind = "xml"
semantic_context = "galec"
path = "manifest.xml"
template = "manifest.xml.jinja"
"#
    );
    let error = parse_target_manifest(&source).expect_err("generic XML must not construct");
    assert!(
        format!("{error:#}").contains("package XML requires one complete product_role family"),
        "unexpected error: {error:#}"
    );
}

#[test]
fn source_only_algorithm_code_requires_one_static_portable_basename() {
    let valid = r#"
version = 1
name = "source-only-path-probe"

[arithmetic]
source_real = "binary32"
source_integer = "i32"
real_matrix_multiply = "separate_mul_add_ascending_positive_zero"

[capabilities]
scalar_fallback = false

[[files]]
artifact_kind = "algorithm-code"
semantic_context = "galec"
path = "checked-model.alg"
template = "model.alg.jinja"
"#;
    let construction = super::super::super::parse_target_manifest_construction(valid)
        .expect("portable static basename must construct");
    let super::super::super::TargetDeclaredRenderAuthority::Unpackaged(steps) =
        construction.render_authority
    else {
        panic!("source-only Algorithm Code must not construct package authority")
    };
    assert_eq!(
        match &steps[0].prepared_member {
            super::super::super::TargetPreparedMemberPlan::AlgorithmCodeSource {
                output_path_template,
            } => output_path_template.as_str(),
            _ => panic!("one source-only route"),
        },
        "checked-model.alg"
    );

    for path in [
        "AlgorithmCode/model.alg",
        "../model.alg",
        "/model.alg",
        "model\\\\nested.alg",
        "{{ model_name }}.alg",
        ".hidden.alg",
        "COM1.alg",
    ] {
        let source = valid.replace("checked-model.alg", path);
        let error = parse_target_manifest(&source)
            .expect_err("unsafe, packaged, or dynamic source-only path must reject");
        assert!(
            format!("{error:#}").contains(
                "source-only Algorithm Code output must be one static portable `.alg` basename"
            ),
            "unexpected error for `{path}`: {error:#}"
        );
    }
}

#[test]
fn source_only_algorithm_code_cannot_declare_package_or_assets() {
    for addition in [
        "[package]\nroot = \"Model\"\nrequired_files = [\"model.alg\"]\n\n[[package.members]]\nkind = \"file\"\nfile = \"source\"\n",
        "[[assets]]\nsource = \"schemas\"\ndest = \"schemas/\"\n",
    ] {
        let source = format!(
            r#"
version = 1
name = "source-only-package-probe"

[arithmetic]
source_real = "binary32"
source_integer = "i32"
real_matrix_multiply = "separate_mul_add_ascending_positive_zero"

[capabilities]
scalar_fallback = false

{addition}

[[files]]
id = "source"
artifact_kind = "algorithm-code"
semantic_context = "galec"
path = "model.alg"
template = "model.alg.jinja"
"#
        );
        let error = parse_target_manifest(&source)
            .expect_err("source-only Algorithm Code must not mint package membership");
        assert!(
            format!("{error:#}").contains("source-only AlgorithmCodePackage cannot declare"),
            "unexpected error: {error:#}"
        );
    }
}

#[test]
fn mixed_efmi_files_require_one_correlated_solve_algorithm_product() {
    let target = super::super::super::TargetBundle::builtin("efmu")
        .expect("registered correlated eFMI target")
        .check()
        .expect("the closed eFMI pair must construct without target-name dispatch");

    let super::super::super::CheckedTargetBundle {
        render_authority, ..
    } = target;
    let render_plan = render_authority.into_plan();
    let super::super::super::CheckedTargetRenderPlan::Packaged(package_plan) = render_plan else {
        panic!("correlated target must issue package authority")
    };
    let super::super::super::CheckedTargetPackageProductPlan::SolveAlgorithm(product_plan) =
        package_plan.into_product_plan()
    else {
        panic!("correlated target must issue its narrowed Solve product layout")
    };
    let (_policy, _capability, arithmetic, _production_profile, layout, _fold) =
        product_plan.into_parts();
    assert_eq!(
        arithmetic.real_matrix_multiply,
        rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct
    );
    let members = layout.members().collect::<Vec<_>>();
    assert_eq!(
        members
            .iter()
            .take(6)
            .map(|member| member.production_artifact_role())
            .collect::<Vec<_>>(),
        vec![
            rumoca_phase_codegen::ProductionArtifactRole::AlgorithmCodeSource,
            rumoca_phase_codegen::ProductionArtifactRole::AlgorithmCodeManifest,
            rumoca_phase_codegen::ProductionArtifactRole::ProductionHeader,
            rumoca_phase_codegen::ProductionArtifactRole::ProductionSource,
            rumoca_phase_codegen::ProductionArtifactRole::ProductionManifest,
            rumoca_phase_codegen::ProductionArtifactRole::PackageManifest,
        ],
        "the construction-issued member plan retains every narrowed file role in package order"
    );
    assert!(
        members[6..].iter().all(|member| matches!(
            member,
            super::super::super::CheckedSolveAlgorithmLayoutMember::Schema { .. }
        )),
        "schemas are construction-proved as one trailing package suffix"
    );
}

#[test]
fn correlated_member_roles_reject_omission_dynamic_paths_and_wrong_owners() {
    for (source, expected) in [
        (
            correlated_efmu_manifest_source().replacen(
                "product_role = \"production-source\"\n",
                "",
                1,
            ),
            "must declare one closed product_role",
        ),
        (
            correlated_efmu_manifest_source().replacen(
                "path = \"ProductionCode/sources/production.c\"",
                "path = \"ProductionCode/sources/{{ model_name }}.c\"",
                1,
            ),
            "must be a static path",
        ),
        (
            correlated_efmu_manifest_source().replacen(
                "product_role = \"production-source\"",
                "product_role = \"production-header\"",
                1,
            ),
            "incompatible artifact_kind/view",
        ),
        (
            correlated_efmu_manifest_source().replacen("product_role = \"schema\"\n", "", 1),
            "every SolveAlgorithmProduct [[assets]] bundle",
        ),
    ] {
        let error = parse_target_manifest(&source).expect_err("invalid member role must reject");
        assert!(
            format!("{error:#}").contains(expected),
            "unexpected error: {error:#}"
        );
    }
}

#[test]
fn mixed_efmi_product_rejects_every_missing_solve_executable_profile() {
    let source = r#"
version = 1
name = "profile-less-mixed-efmi"

[arithmetic]
source_real = "binary32"
source_integer = "i32"
real_matrix_multiply = "separate_mul_add_ascending_positive_zero"

[capabilities]
scalar_fallback = false

[[files]]
artifact_kind = "algorithm-code"
semantic_context = "galec"
path = "AlgorithmCode/model.alg"
template = "model.alg.jinja"

[[files]]
artifact_kind = "c-source"
semantic_context = "solve"
view = "solve-algorithm-block"
path = "ProductionCode/model.c"
template = "model.c.jinja"
"#;
    let error = parse_target_manifest(source).expect_err("profiles are mandatory");
    assert!(error.to_string().contains("complete [solve_executable]"));
}

#[test]
fn mixed_efmi_product_derives_binary64_capability_from_the_sole_numeric_profile() {
    let source = correlated_efmu_manifest_source()
        .replace("source_real = \"binary32\"", "source_real = \"binary64\"");
    let manifest = parse_target_manifest(&source)
        .expect("Production preparation derives its Real ABI from source_real");

    assert_eq!(
        manifest
            .algorithm_code_arithmetic()
            .expect("a correlated product retains its sole numeric profile")
            .source_real,
        rumoca_ir_galec::package::AlgorithmCodeRealFormat::Binary64
    );
    assert!(manifest.solve_algorithm_production_profile().is_some());
}

#[test]
fn mixed_efmi_product_refuses_an_unsupported_source_integer_representation() {
    let source = correlated_efmu_manifest_source()
        .replace("source_integer = \"i32\"", "source_integer = \"i64\"");
    let error = parse_target_manifest(&source)
        .expect_err("Production C does not yet implement the source I64 ABI");

    assert!(
        format!("{error:#}")
            .contains("current Production C refinement admits only source_integer = 'i32'"),
        "unexpected error: {error:#}"
    );
}

#[test]
fn mixed_efmi_product_rejects_a_second_real_format_capability_authority() {
    let source = correlated_efmu_manifest_source().replace(
        "real_scalar = true",
        "real_scalar = true\nbinary32_scalar = true",
    );
    let error = parse_target_manifest(&source)
        .expect_err("the value-capability profile cannot restate source_real");

    let rendered = format!("{error:#}");
    assert!(
        rendered.contains("unknown field") && rendered.contains("binary32_scalar"),
        "unexpected error: {error:#}"
    );
}

#[test]
fn algorithm_code_target_rejects_unsigned_source_integer() {
    let source = format!(
        r#"
version = 1
name = "unsigned-source-integer"

[arithmetic]
source_real = "binary32"
source_integer = "u32"
real_matrix_multiply = "separate_mul_add_ascending_positive_zero"

[capabilities]
scalar_fallback = false

{SCALAR_LITERAL_SOLVE_EXECUTABLE_PROFILE}

[[files]]
artifact_kind = "algorithm-code"
semantic_context = "galec"
path = "AlgorithmCode/model.alg"
template = "model.alg.jinja"

[[files]]
artifact_kind = "c-source"
semantic_context = "solve"
view = "solve-algorithm-block"
path = "ProductionCode/model.c"
template = "model.c.jinja"
"#
    );
    let error = parse_target_manifest(&source)
        .expect_err("source Modelica Integer cannot be reinterpreted as unsigned");

    let rendered = format!("{error:#}");
    assert!(
        rendered.contains("unknown variant") && rendered.contains("u32"),
        "unexpected error: {error:#}"
    );
}

#[test]
fn root_and_product_names_cannot_be_manifest_semantic_contexts() {
    for forbidden in [
        "solve-problem",
        "fmi-component",
        "algorithm-code",
        "solve-algorithm-block",
        "solve-algorithm-product",
        "efmu",
    ] {
        let source = format!(
            r#"
version = 1
name = "root-name-context-probe"

[[files]]
artifact_kind = "text"
semantic_context = "{forbidden}"
path = "model.txt"
template = "model.txt.jinja"
"#
        );
        expect_target_error(&source, "unknown variant");
    }
}

#[test]
fn unrelated_semantic_contexts_cannot_share_one_target() {
    expect_target_error(
        r#"
version = 1
name = "illegal-root-mixture"

[capabilities]
scalar_fallback = false

[[files]]
artifact_kind = "modelica-source"
semantic_context = "dae"
path = "model.mo"
template = "model.mo.jinja"

[[files]]
artifact_kind = "text"
semantic_context = "solve"
path = "solve.txt"
template = "solve.txt.jinja"
"#,
        "incompatible checked IR views",
    );
}

#[test]
fn file_view_must_name_a_checked_type_inside_its_context() {
    expect_target_error(
        r#"
version = 1
name = "cross-context-view"

[[files]]
artifact_kind = "text"
semantic_context = "dae"
view = "solve-model"
path = "model.txt"
template = "model.txt.jinja"
"#,
        "outside semantic_context",
    );

    for fake_view in ["c-source", "efmu", "solve-algorithm-product"] {
        let source = format!(
            r#"
version = 1
name = "fake-view"

[[files]]
artifact_kind = "text"
semantic_context = "solve"
view = "{fake_view}"
path = "model.txt"
template = "model.txt.jinja"
"#,
        );
        expect_target_error(&source, "unknown variant");
    }
}

#[test]
fn absent_view_resolves_once_to_the_contexts_canonical_checked_root() {
    let manifest = parse_manifest_with_context_capabilities("solve", "[capabilities]");
    assert_eq!(
        manifest.required_product(),
        TargetRequiredProduct::SolveModel
    );
    assert_eq!(
        manifest.files()[0].semantic_view(),
        TargetSemanticView::SolveModel
    );
}

#[test]
fn solve_algorithm_block_cannot_be_a_standalone_product() {
    expect_target_error(
        r#"
version = 1
name = "standalone-solve-algorithm-block"

[[files]]
artifact_kind = "c-source"
semantic_context = "solve"
view = "solve-algorithm-block"
path = "model.c"
template = "model.c.jinja"
"#,
        "only admitted as the correlated Production Code view",
    );
}

#[test]
fn galec_plus_canonical_solve_does_not_imply_a_correlated_block() {
    expect_target_error(
        r#"
version = 1
name = "uncorrelated-root-pair"

[arithmetic]
source_real = "binary64"
source_integer = "i32"
real_matrix_multiply = "separate_mul_add_ascending_positive_zero"

[capabilities]
scalar_fallback = false

[[files]]
artifact_kind = "algorithm-code"
semantic_context = "galec"
path = "model.alg"
template = "model.alg.jinja"

[[files]]
artifact_kind = "c-source"
semantic_context = "solve"
path = "model.c"
template = "model.c.jinja"
"#,
        "incompatible checked IR views",
    );
}

#[test]
fn incompatible_solve_roots_cannot_share_one_target() {
    expect_target_error(
        r#"
version = 1
name = "mixed-solve-roots"

[capabilities]
scalar_fallback = false

[[files]]
artifact_kind = "text"
semantic_context = "solve"
path = "solve.txt"
template = "solve.txt.jinja"

[[files]]
artifact_kind = "xml"
semantic_context = "solve"
view = "fmi-component"
path = "model.xml"
template = "model.xml.jinja"
"#,
        "incompatible checked IR views",
    );
}

#[test]
fn efmu_product_identity_is_registered() {
    assert!(templates::builtin_target("efmu").is_some());
}

/// A well-formed checksum web (one producer, one consumer edge) parses and
/// validates — the positive control for the rejection tests below.
#[test]
fn checksum_web_accepts_a_wellformed_declaration() {
    let target = checked_target_from_manifest_source(
        r#"
version = 1
name = "checksum-web"
[capabilities]
scalar_fallback = false
[[files]]
artifact_kind = "text"
semantic_context = "solve"
path = "a.txt"
template = "a.jinja"
id = "a"
[[files]]
artifact_kind = "text"
semantic_context = "solve"
path = "b.txt"
template = "b.jinja"
[[files.checksums]]
of = "a"
  algorithm = "sha1"
as = "a_sha1"
"#,
    );
    assert_eq!(
        checked_unpacked_checksum_bindings(target),
        vec![
            vec![],
            vec![(
                0,
                super::super::super::ChecksumAlgorithm::Sha1,
                "a_sha1".to_owned()
            )],
        ]
    );
}

#[test]
fn checksum_web_retains_declaration_order_without_sorting_or_reconstruction() {
    let source = r#"
version = 1
name = "checksum-order"
[capabilities]
scalar_fallback = false
[[files]]
artifact_kind = "text"
semantic_context = "solve"
path = "producer.txt"
template = "producer.jinja"
id = "producer"
[[files]]
artifact_kind = "text"
semantic_context = "solve"
path = "unrelated.txt"
template = "unrelated.jinja"
[[files]]
artifact_kind = "text"
semantic_context = "solve"
path = "consumer.txt"
template = "consumer.jinja"
[[files.checksums]]
of = "producer"
algorithm = "sha1"
as = "producer_sha1"
"#;
    let first = checked_unpacked_checksum_bindings(checked_target_from_manifest_source(source));
    let second = checked_unpacked_checksum_bindings(checked_target_from_manifest_source(source));
    assert_eq!(
        first, second,
        "construction must retain declaration order for identical manifests"
    );

    assert_eq!(
        first,
        vec![
            vec![],
            vec![],
            vec![(
                0,
                super::super::super::ChecksumAlgorithm::Sha1,
                "producer_sha1".to_owned(),
            )],
        ]
    );
}

#[test]
fn checksum_web_rejects_backward_edge_with_exact_endpoint_facts() {
    let error = parse_target_manifest(
        r#"
version = 1
name = "checksum-cycle"
[capabilities]
scalar_fallback = false
[[files]]
artifact_kind = "text"
semantic_context = "solve"
path = "a.txt"
template = "a.jinja"
id = "a"
[[files.checksums]]
of = "b"
algorithm = "sha1"
as = "b_sha1"
[[files]]
artifact_kind = "text"
semantic_context = "solve"
path = "b.txt"
template = "b.jinja"
id = "b"
"#,
    )
    .expect_err("a consumer cannot precede its producer")
    .to_string();
    assert!(error.contains("strict producer-before-consumer target-issued order"));
    assert!(error.contains("consumer position 1 role = 'solve-model' path 'a.txt'"));
    assert!(error.contains("producer position 2 role = 'solve-model' path 'b.txt'"));
}

fn expect_target_error(source: &str, needle: &str) {
    let err = parse_target_manifest(source).expect_err("malformed target.toml must be rejected");
    assert!(
        err.to_string().contains(needle),
        "error `{err}` should mention `{needle}`"
    );
}

#[test]
fn checksum_web_rejects_duplicate_file_ids() {
    expect_target_error(
        r#"
version = 1
name = "dup-id"
[capabilities]
scalar_fallback = false
[[files]]
artifact_kind = "text"
semantic_context = "solve"
path = "a.txt"
template = "a.jinja"
id = "x"
[[files]]
artifact_kind = "text"
semantic_context = "solve"
path = "b.txt"
template = "b.jinja"
id = "x"
"#,
        "duplicate [[files]] id",
    );
}

#[test]
fn checksum_web_rejects_empty_file_ids() {
    expect_target_error(
        r#"
version = 1
name = "empty-id"
[[files]]
artifact_kind = "text"
semantic_context = "ast"
path = "a.txt"
template = "a.jinja"
id = ""
"#,
        "must use the exact Jinja dot-addressable ASCII key grammar [a-z_][a-z0-9_]*",
    );
}

#[test]
fn artifact_identity_keys_use_exact_jinja_dot_addressable_grammar() {
    let manifest = |id: &str| {
        format!(
            r#"
version = 1
[[files]]
artifact_kind = "text"
semantic_context = "ast"
path = "artifact.txt"
template = "artifact.jinja"
id = {id}
"#
        )
    };

    for valid in ["\"_\"", "\"a\"", "\"a0\"", "\"_a0\"", "\"artifact_key\""] {
        parse_target_manifest(&manifest(valid)).unwrap_or_else(|error| {
            panic!("valid artifact identity key {valid} rejected: {error:#}")
        });
    }

    for invalid in [
        "\" artifact\"",
        "\"artifact \"",
        "\"Artifact\"",
        "\"1artifact\"",
        "\"artifact-key\"",
        r#""artifact\u0001""#,
        "\"café\"",
    ] {
        let error = parse_target_manifest(&manifest(invalid))
            .expect_err("mutated artifact identity key must reject during catalog construction");
        assert!(
            format!("{error:#}")
                .contains("exact Jinja dot-addressable ASCII key grammar [a-z_][a-z0-9_]*"),
            "unexpected error for invalid key {invalid}: {error:#}"
        );
    }
}

fn identity_template_bundle(
    template: &str,
    declares_alpha: bool,
) -> anyhow::Result<crate::codegen_target::CheckedTargetBundle> {
    let dependency = if declares_alpha {
        "required_artifact_identities = [\"alpha\"]"
    } else {
        ""
    };
    let manifest = format!(
        r#"
version = 1
[[files]]
artifact_kind = "text"
semantic_context = "ast"
path = "artifact.txt"
template = "artifact.jinja"
id = "alpha"
{dependency}
"#
    );
    super::super::super::TargetBundle::check_in_memory(
        "identity-template-analysis".to_owned(),
        manifest,
        BTreeMap::from([("artifact.jinja".to_owned(), template.to_owned())]),
        BTreeMap::new(),
    )
}

fn check_identity_template(template: &str, declares_alpha: bool) -> anyhow::Result<()> {
    identity_template_bundle(template, declares_alpha).map(|_| ())
}

#[test]
fn template_identity_analysis_covers_branches_and_matches_declarations_both_ways() {
    check_identity_template(
        "{% if enabled %}{{ __rumoca_artifact_identity_v1_alpha }}{% else %}inactive{% endif %}",
        true,
    )
    .expect("all-branch MiniJinja analysis finds the declared identity");

    let under = check_identity_template("{{ __rumoca_artifact_identity_v1_alpha }}", false)
        .expect_err("an identity use without its declaration must reject");
    assert!(
        format!("{under:#}").contains(
            "uses undeclared artifact identity scalars [__rumoca_artifact_identity_v1_alpha]"
        ),
        "unexpected under-declaration error: {under:#}"
    );

    let over = check_identity_template("no identity use", true)
        .expect_err("an identity declaration without its use must reject");
    assert!(
        format!("{over:#}").contains(
            "declares unused artifact identity scalars [__rumoca_artifact_identity_v1_alpha]"
        ),
        "unexpected over-declaration error: {over:#}"
    );
}

#[test]
fn template_identity_analysis_rejects_dynamic_aliases_and_composition() {
    for dynamic in [
        r#"{{ artifact["identities"]["alpha"] }}"#,
        "{% set identities = artifact.identities %}{{ identities.alpha }}",
        "{{ artifact }}",
        "{{ artifact.identities }}",
    ] {
        let error = check_identity_template(dynamic, true)
            .expect_err("dynamic or aliased identity access must reject");
        assert!(
            format!("{error:#}").contains("uses the removed artifact.identities map"),
            "unexpected dynamic-access error for `{dynamic}`: {error:#}"
        );
    }

    let malformed = check_identity_template("{{ __rumoca_artifact_identity_v1_Alpha }}", false)
        .expect_err("a noncanonical reserved scalar name must reject");
    assert!(
        format!("{malformed:#}").contains("is not an exact flattened canonical identity scalar"),
        "unexpected malformed identity-scalar diagnostic: {malformed:#}"
    );

    for composition in [
        r#"{% include "other.jinja" %}"#,
        r#"{% if enabled %}{% include "other.jinja" %}{% endif %}"#,
        r#"{% import "other.jinja" as other %}"#,
        r#"{% from "other.jinja" import helper %}"#,
        r#"{% extends "other.jinja" %}"#,
    ] {
        let error = check_identity_template(composition, false)
            .expect_err("composition is absent from the admitted MiniJinja grammar");
        assert!(
            format!("{error:#}").contains("Compile snapshotted target template"),
            "composition must fail the proven parse before analysis: {error:#}"
        );
    }
}

#[test]
fn template_identity_analysis_cannot_turn_parse_failure_into_an_empty_set() {
    let error = check_identity_template("{{ __rumoca_artifact_identity_v1_alpha", true)
        .expect_err("a parse failure must precede undeclared-variable analysis");
    let diagnostic = format!("{error:#}");
    assert!(
        diagnostic.contains("Compile snapshotted target template"),
        "parse failure must not become an empty dependency set: {diagnostic}"
    );
    assert!(
        !diagnostic.contains("declares unused artifact identity scalars"),
        "parse failure must not reach the empty-set comparison: {diagnostic}"
    );
}

#[test]
fn self_shadow_cannot_recover_an_identity_absent_from_the_file_context() {
    let mut session = crate::session::Session::default();
    session
        .add_document(
            "identity-shadow.mo",
            "model IdentityShadow Real x; equation x = 1; end IdentityShadow;",
        )
        .expect("parse identity self-shadow fixture");
    let compilation = session
        .compile_model_strict("IdentityShadow")
        .unwrap_or_else(|report| panic!("compile identity self-shadow fixture: {report:#?}"));

    let no_dead_reference = identity_template_bundle(
        "{% set __rumoca_artifact_identity_v1_alpha = __rumoca_artifact_identity_v1_alpha %}{{ __rumoca_artifact_identity_v1_alpha }}",
        false,
    )
    .expect("MiniJinja self-shadow ordering hides the absent scalar from static analysis");
    let error = match compilation.render_target(no_dead_reference, test_artifact_input()) {
        Ok(_) => panic!("an undeclared identity scalar must be absent and fail strict rendering"),
        Err(error) => error,
    };
    assert!(
        format!("{error:#}").contains("undefined value"),
        "unexpected no-dead-reference diagnostic: {error:#}"
    );

    let with_dead_reference = identity_template_bundle(
        "{% if false %}{{ __rumoca_artifact_identity_v1_alpha }}{% endif %}{% set __rumoca_artifact_identity_v1_beta = __rumoca_artifact_identity_v1_beta %}{{ __rumoca_artifact_identity_v1_beta }}",
        true,
    )
    .expect("dead declared reference closes while a different self-shadow remains invisible");
    let error = match compilation.render_target(with_dead_reference, test_artifact_input()) {
        Ok(_) => panic!("self-shadow must not recover a scalar outside the declared file subset"),
        Err(error) => error,
    };
    assert!(
        format!("{error:#}").contains("undefined value"),
        "unexpected dead-reference self-shadow diagnostic: {error:#}"
    );
}

#[test]
fn debug_context_dump_cannot_emit_a_declared_identity_scalar() {
    let target = identity_template_bundle(
        "{% if false %}{{ __rumoca_artifact_identity_v1_alpha }}{% endif %}{{ debug() }}",
        true,
    )
    .expect("the debug mutation reaches execution after exact identity closure");
    let mut session = crate::session::Session::default();
    session
        .add_document(
            "identity-debug.mo",
            "model IdentityDebug Real x; equation x = 1; end IdentityDebug;",
        )
        .expect("parse identity debug fixture");
    let compilation = session
        .compile_model_strict("IdentityDebug")
        .unwrap_or_else(|report| panic!("compile identity debug fixture: {report:#?}"));

    let error = match compilation.render_target(target, test_artifact_input()) {
        Ok(_) => panic!("the removed debug global must not dump an identity-bearing context"),
        Err(error) => error,
    };
    let diagnostic = format!("{error:#}");
    assert!(
        diagnostic.contains("unknown function") && diagnostic.contains("debug"),
        "unexpected removed-debug diagnostic: {diagnostic}"
    );
}

#[test]
fn artifact_identity_catalog_is_construction_sorted() {
    let manifest = parse_target_manifest(
        r#"
version = 1
[[files]]
artifact_kind = "text"
semantic_context = "ast"
path = "z.txt"
template = "z.jinja"
id = "zeta"
[[files]]
artifact_kind = "text"
semantic_context = "ast"
path = "a.txt"
template = "a.jinja"
id = "alpha"
"#,
    )
    .expect("the identity catalog is checked with the manifest");

    assert_eq!(manifest.artifact_identity_keys(), ["alpha", "zeta"]);
}

#[test]
fn artifact_identity_dependencies_are_checked_and_construction_sorted() {
    let manifest = parse_target_manifest(
        r#"
version = 1
[[files]]
artifact_kind = "text"
semantic_context = "ast"
path = "z.txt"
template = "z.jinja"
id = "zeta"
required_artifact_identities = ["zeta", "alpha"]
[[files]]
artifact_kind = "text"
semantic_context = "ast"
path = "a.txt"
template = "a.jinja"
id = "alpha"
"#,
    )
    .expect("per-file identity dependencies are closed with the manifest");

    assert_eq!(
        manifest.files()[0].artifact_identity_dependencies(),
        ["alpha", "zeta"]
    );
}

#[test]
fn artifact_identity_dependencies_reject_missing_empty_and_duplicate_keys() {
    let base = r#"
version = 1
[[files]]
artifact_kind = "text"
semantic_context = "ast"
path = "artifact.txt"
template = "artifact.jinja"
id = "artifact"
required_artifact_identities = [DEPENDENCIES]
"#;
    for (dependencies, expected) in [
        ("\"missing\"", "names no target-issued [[files]] id"),
        ("\"\"", "must not contain an empty key"),
        (
            "\"artifact\", \"artifact\"",
            "contains duplicate key 'artifact'",
        ),
    ] {
        let source = base.replace("DEPENDENCIES", dependencies);
        let error = parse_target_manifest(&source)
            .expect_err("invalid artifact identity dependency must reject construction");
        assert!(
            format!("{error:#}").contains(expected),
            "unexpected error for `{dependencies}`: {error:#}"
        );
    }
}

#[test]
fn custom_target_identity_uses_checked_manifest_facts_not_display_or_formatting() {
    let left_source = r#"
version = 1
name = "Display A"
description = "First description"
[[files]]
artifact_kind = "text"
semantic_context = "ast"
path = "artifact.txt"
template = "artifact.jinja"
id = "artifact"
"#;
    let right_source = r#"version=1
name="Display B"
description="Second description"
[[files]]
artifact_kind="text"
semantic_context="ast"
path="artifact.txt"
template="artifact.jinja"
id="artifact"
"#;
    let changed_source = right_source.replace("artifact.txt", "changed.txt");
    let dependency_source = right_source.replace(
        "id=\"artifact\"",
        "id=\"artifact\"\nrequired_artifact_identities=[\"artifact\"]",
    );
    let left = parse_target_manifest(left_source).expect("left manifest is checked");
    let right = parse_target_manifest(right_source).expect("right manifest is checked");
    let changed = parse_target_manifest(&changed_source).expect("changed manifest is checked");
    let dependency =
        parse_target_manifest(&dependency_source).expect("dependency manifest is checked");
    let bundle = directory_target_bundle("unused-custom-target", String::new());

    let left_scope = bundle
        .construct_artifact_identity_scope(&left)
        .expect("left scope is issued");
    let right_scope = bundle
        .construct_artifact_identity_scope(&right)
        .expect("right scope is issued");
    let changed_scope = bundle
        .construct_artifact_identity_scope(&changed)
        .expect("changed scope is issued");
    let dependency_scope = bundle
        .construct_artifact_identity_scope(&dependency)
        .expect("dependency scope is issued");
    assert_eq!(
        left_scope.kind(),
        super::super::super::TargetArtifactIdentityScopeKind::CanonicalManifestDigest
    );
    assert_eq!(left_scope, right_scope);
    assert_ne!(left_scope, changed_scope);
    assert_ne!(right_scope, dependency_scope);
}

#[test]
fn checksum_web_rejects_dangling_of() {
    expect_target_error(
        r#"
version = 1
name = "dangling"
[capabilities]
scalar_fallback = false
[[files]]
artifact_kind = "text"
semantic_context = "solve"
path = "b.txt"
template = "b.jinja"
[[files.checksums]]
of = "ghost"
  algorithm = "sha1"
as = "ghost_sha1"
"#,
        "names no [[files]] id",
    );
}

#[test]
fn checksum_web_rejects_self_hash() {
    expect_target_error(
        r#"
version = 1
name = "self-hash"
[capabilities]
scalar_fallback = false
[[files]]
artifact_kind = "text"
semantic_context = "solve"
path = "a.txt"
template = "a.jinja"
id = "a"
[[files.checksums]]
of = "a"
  algorithm = "sha1"
as = "a_sha1"
"#,
        "consumer position 1 role = 'solve-model' path 'a.txt' references producer position 1 role = 'solve-model' path 'a.txt'",
    );
}

#[test]
fn checksum_web_rejects_empty_as_key() {
    expect_target_error(
        r#"
version = 1
name = "empty-as"
[capabilities]
scalar_fallback = false
[[files]]
artifact_kind = "text"
semantic_context = "solve"
path = "a.txt"
template = "a.jinja"
id = "a"
[[files]]
artifact_kind = "text"
semantic_context = "solve"
path = "b.txt"
template = "b.jinja"
[[files.checksums]]
of = "a"
  algorithm = "sha1"
as = ""
"#,
        "`as` must not be empty",
    );
}

#[test]
fn checksum_web_rejects_duplicate_as_key_on_one_file() {
    expect_target_error(
        r#"
version = 1
name = "dup-as"
[capabilities]
scalar_fallback = false
[[files]]
artifact_kind = "text"
semantic_context = "solve"
path = "a.txt"
template = "a.jinja"
id = "a"
[[files]]
artifact_kind = "text"
semantic_context = "solve"
path = "c.txt"
template = "c.jinja"
id = "c"
[[files]]
artifact_kind = "text"
semantic_context = "solve"
path = "b.txt"
template = "b.jinja"
[[files.checksums]]
of = "a"
  algorithm = "sha1"
as = "sha1"
[[files.checksums]]
of = "c"
  algorithm = "sha1"
as = "sha1"
"#,
        "declared twice",
    );
}

#[test]
fn asset_tree_rejects_empty_source_and_dest() {
    expect_target_error(
        r#"
version = 1
name = "empty-bundle"
[capabilities]
scalar_fallback = false
[[files]]
artifact_kind = "text"
semantic_context = "solve"
path = "a.txt"
template = "a.jinja"
[[assets]]
source = ""
dest = "schemas/"
"#,
        "source must not be empty",
    );
    expect_target_error(
        r#"
version = 1
name = "empty-dest"
[capabilities]
scalar_fallback = false
[[files]]
artifact_kind = "text"
semantic_context = "solve"
path = "a.txt"
template = "a.jinja"
[[assets]]
source = "schemas"
dest = ""
"#,
        "dest",
    );
}

#[test]
fn target_asset_relative_path_is_portable_and_nested() {
    let root = Path::new("target/assets");
    let path = root.join("schémas").join("nested").join("model.xsd");

    assert_eq!(
        target_asset_relative_path(root, &path).expect("nested UTF-8 asset path"),
        "schémas/nested/model.xsd"
    );
}

#[test]
fn target_asset_relative_path_rejects_paths_outside_root() {
    let error = target_asset_relative_path(
        Path::new("target/assets"),
        Path::new("target/templates/model.jinja"),
    )
    .expect_err("asset outside its declared root must fail");

    assert!(
        error.to_string().contains("is not beneath source root"),
        "unexpected error: {error:#}"
    );
}
