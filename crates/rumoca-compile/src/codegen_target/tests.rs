use super::{
    TargetFeatureSupport, TargetManifest, TargetTemplateIr, TensorCapability,
    TensorLayoutCapability, builtin_target_compatibility_matrix, parse_target_manifest,
    safe_target_join, target_asset_relative_path, templates, validate_dae_target_capabilities,
    validate_solve_target_capabilities, validate_target_manifest,
};
use rumoca_core::{SourceMap, StructuredIndexBinder, StructuredIndexDomain};
use rumoca_ir_dae::{Dae, DaeLiteral, DaeProvenance};
use std::path::Path;

fn dae_with_placeholder_family() -> Dae {
    let source_text = "for i in 1:4 loop 0.0 = 0.0; end for;";
    let mut source_map = SourceMap::new();
    let source_id = source_map.add("target-capability.mo", source_text);
    let owner = DaeProvenance::source(rumoca_core::Span::from_offsets(
        source_id,
        0,
        source_text.len(),
    ))
    .expect("fixture source span is exact");
    Dae::construct(source_map, |dae| {
        let domain = dae.domains(|domains| {
            domains.structured(
                StructuredIndexDomain {
                    binders: vec![StructuredIndexBinder {
                        id: 0,
                        display_name: "i".to_string(),
                        lower: 1,
                        upper: 4,
                        step: 1,
                    }],
                },
                owner,
            )
        })?;
        let residual =
            dae.expressions(|expressions| expressions.at(owner).literal(DaeLiteral::Real(0.0)))?;
        dae.continuous(|equations| {
            equations.structured_family(
                owner,
                domain,
                rumoca_core::ComprehensionScalarView::BinderSubstitution,
                |family| family.body(residual),
            )
        })?;
        Ok(())
    })
    .expect("checked structured-family fixture is valid")
}

fn manifest_with_capabilities(capabilities: &str) -> TargetManifest {
    toml::from_str(&format!(
        r#"
version = 1
ir = "dae"
name = "custom"
readiness_level = 3

{capabilities}

[[files]]
path = "model.out"
template = "model.out.jinja"
"#
    ))
    .expect("parse target manifest")
}

fn parse_manifest_with_ir_capabilities(ir: &str, capabilities: &str) -> TargetManifest {
    super::parse_target_manifest(&format!(
        r#"
version = 1
ir = "{ir}"
name = "custom"
readiness_level = 1

{capabilities}

[[files]]
path = "model.out"
template = "model.out.jinja"
"#
    ))
    .expect("parse and validate target manifest")
}

#[test]
fn target_manifest_rejects_escaping_paths() {
    let root = Path::new("out");
    assert!(safe_target_join(root, "../escape").is_err());
    assert!(safe_target_join(root, "/absolute").is_err());
    assert_eq!(
        safe_target_join(root, "nested/file.c").unwrap(),
        root.join("nested/file.c")
    );
}

#[test]
fn target_manifest_parses_capabilities_table() {
    let manifest = manifest_with_capabilities(
        r#"
[capabilities]
external_functions = false
events = true
runtime_events = false
forward_ad = true
reverse_ad = false
dynamic_control_flow = true
host_callbacks = false
"#,
    );
    let capabilities = manifest.capabilities.expect("capabilities table");

    assert_eq!(manifest.readiness_level, Some(3));
    assert_eq!(capabilities.external_functions, Some(false));
    assert_eq!(capabilities.external_tables, None);
    assert_eq!(capabilities.events, Some(true));
    assert_eq!(capabilities.runtime_events, Some(false));
    assert_eq!(capabilities.forward_ad, Some(true));
    assert_eq!(capabilities.reverse_ad, Some(false));
    assert_eq!(capabilities.dynamic_control_flow, Some(true));
    assert_eq!(capabilities.host_callbacks, Some(false));
}

#[test]
fn all_builtin_target_manifests_parse() {
    for target in templates::builtin_targets() {
        parse_target_manifest(target.manifest).unwrap_or_else(|err| {
            panic!("built-in target '{}' failed to parse: {err}", target.name)
        });
    }
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
        if !matches!(
            manifest.ir,
            TargetTemplateIr::Dae
                | TargetTemplateIr::Solve
                | TargetTemplateIr::Fmi
                | TargetTemplateIr::AlgorithmCode
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
    let c_ode = matrix
        .iter()
        .find(|entry| entry.id == "c-ode")
        .expect("c-ode target should be listed");
    assert_eq!(c_ode.ir, TargetTemplateIr::Solve);
    assert_eq!(c_ode.readiness_level, Some(2));
    assert_eq!(c_ode.scalar_programs, TargetFeatureSupport::Native);
    assert_eq!(c_ode.matmul, TargetFeatureSupport::Scalar);
    assert_eq!(c_ode.linsolve, TargetFeatureSupport::Scalar);
    assert_eq!(c_ode.elementwise, TargetFeatureSupport::Unknown);
    assert_eq!(c_ode.sparse, TargetFeatureSupport::Unsupported);
    assert_eq!(c_ode.dtypes, vec!["f64"]);
    assert_eq!(c_ode.events, TargetFeatureSupport::Unsupported);
    assert_eq!(c_ode.runtime_events, TargetFeatureSupport::Unsupported);
    assert_eq!(c_ode.forward_ad, TargetFeatureSupport::Unsupported);
    assert_eq!(c_ode.reverse_ad, TargetFeatureSupport::Unsupported);
    assert_eq!(
        c_ode.dynamic_control_flow,
        TargetFeatureSupport::Unsupported
    );
    assert_eq!(c_ode.host_callbacks, TargetFeatureSupport::Unsupported);

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
fn builtin_fmi_targets_report_one_checked_projection() {
    let matrix = builtin_target_compatibility_matrix()
        .expect("built-in target compatibility matrix should build");
    for fmi in ["fmi2", "fmi3"] {
        let entry = matrix
            .iter()
            .find(|entry| entry.id == fmi)
            .unwrap_or_else(|| panic!("{fmi} target should be listed"));
        assert_eq!(entry.ir, TargetTemplateIr::Fmi);
        assert_eq!(entry.deployment_class.as_deref(), Some("fmu"));
        assert_eq!(entry.events, TargetFeatureSupport::Unsupported);
    }
}

#[test]
fn removed_file_render_context_is_rejected() {
    parse_target_manifest(
        r#"
version = 1
ir = "solve"
name = "removed-context"

[[files]]
path = "modelDescription.xml"
template = "modelDescription.xml.jinja"
render_context = "fmi-model-description"
"#,
    )
    .expect_err("removed per-file render contexts must not parse");
}

#[test]
fn target_manifest_parses_solve_tensor_capabilities() {
    let manifest = parse_manifest_with_ir_capabilities(
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
    let capabilities = manifest.capabilities.expect("capabilities table");
    let tensor = capabilities.tensor.expect("tensor capabilities");

    assert_eq!(manifest.ir, TargetTemplateIr::Solve);
    assert_eq!(capabilities.scalar_fallback, Some(true));
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
fn solve_target_rejects_event_partition_without_event_support() {
    let mut solve = rumoca_ir_solve::SolveProblem::default();
    solve.events.scheduled_time_events.push(1.0);
    let manifest = parse_manifest_with_ir_capabilities(
        "solve",
        r#"
[capabilities]
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
    let mut solve = rumoca_ir_solve::SolveProblem::default();
    solve
        .continuous
        .algebraic_projection_plan
        .blocks
        .push(rumoca_ir_solve::AlgebraicProjectionBlock::default());
    let manifest = parse_manifest_with_ir_capabilities(
        "solve",
        r#"
[capabilities]
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
fn residual_kernel_target_accepts_algebraic_projection_contract() {
    let mut solve = rumoca_ir_solve::SolveProblem::default();
    solve
        .continuous
        .algebraic_projection_plan
        .blocks
        .push(rumoca_ir_solve::AlgebraicProjectionBlock::default());
    let manifest = parse_manifest_with_ir_capabilities(
        "solve",
        r#"
[capabilities]
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
    let manifest = parse_manifest_with_ir_capabilities(
        "fmi",
        r#"
[capabilities]
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
    let manifest = parse_manifest_with_ir_capabilities(
        "algorithm-code",
        r#"
[capabilities]
continuous_states = false
residual_equations = false
"#,
    );
    let capabilities = manifest.capabilities.as_ref().expect("capabilities");

    validate_dae_target_capabilities(&dae, &manifest, capabilities)
        .expect("Algorithm Code must classify algebraic owners in its checked projection");
}

#[test]
fn solve_target_rejects_terminal_runtime_without_runtime_event_support() {
    let mut solve = rumoca_ir_solve::SolveProblem::default();
    solve.events.has_terminal_event = true;
    let manifest = parse_manifest_with_ir_capabilities(
        "solve",
        r#"
[capabilities]
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
            .contains("unsupported-feature:runtime_events")
    );
}

#[test]
fn solve_target_rejects_clock_partition_without_clock_support() {
    let mut solve = rumoca_ir_solve::SolveProblem::default();
    solve
        .clocks
        .periodic_event_schedules
        .push(rumoca_ir_solve::PeriodicEventSchedule::default());
    let manifest = parse_manifest_with_ir_capabilities(
        "solve",
        r#"
[capabilities]
events = true
clocks = false
"#,
    );
    let capabilities = manifest.capabilities.as_ref().expect("capabilities");

    let error = validate_solve_target_capabilities(&solve, &manifest, capabilities)
        .expect_err("a clock-free target must reject a clock partition");

    assert!(error.to_string().contains("unsupported-feature:clocks"));
}

#[test]
fn dae_target_must_declare_structured_family_consumption() {
    let dae = dae_with_placeholder_family();
    let manifest = manifest_with_capabilities(
        r#"
[capabilities]
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
residual_equations = true
structured_equation_families = true
"#,
    );
    let capabilities = manifest.capabilities.as_ref().expect("capabilities");

    validate_dae_target_capabilities(&dae, &manifest, capabilities)
        .expect("declared family-aware target may consume the compact owner");
}

#[test]
fn target_manifest_rejects_tensor_capabilities_for_non_solve_ir() {
    let err = super::parse_target_manifest(
        r#"
version = 1
ir = "dae"
name = "custom"

[capabilities.tensor]
matmul = "native"

[[files]]
path = "model.out"
template = "model.out.jinja"
"#,
    )
    .expect_err("tensor capabilities should require solve IR");

    assert!(
        err.to_string()
            .contains("tensor capabilities are only valid")
    );
}

#[test]
fn target_manifest_rejects_scalar_tensor_ops_without_scalar_fallback() {
    let manifest = parse_manifest_with_ir_capabilities(
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
    validate_target_manifest(&manifest).expect("native tensor ops need no scalar fallback");
    assert_eq!(capabilities.scalar_fallback, Some(false));

    let err = super::parse_target_manifest(
        r#"
version = 1
ir = "solve"
name = "custom"

[capabilities]
scalar_fallback = false

[capabilities.tensor]
matmul = "scalar"

[[files]]
path = "model.out"
template = "model.out.jinja"
"#,
    )
    .expect_err("scalar tensor op should require scalar fallback");

    assert!(err.to_string().contains("scalar_fallback = false"));
}

#[test]
fn target_manifest_rejects_invalid_readiness_level() {
    let err = super::parse_target_manifest(
        r#"
version = 1
ir = "solve"
name = "invalid"
readiness_level = 6

[[files]]
path = "model.out"
template = "model.out.jinja"
"#,
    )
    .expect_err("readiness level above 5 should fail");

    assert!(err.to_string().contains("readiness_level"), "{err}");
}

#[test]
fn target_manifest_rejects_manifest_only_readiness_zero() {
    let error = super::parse_target_manifest(
        r#"
version = 1
ir = "solve"
name = "future-target"
readiness_level = 0
"#,
    )
    .expect_err("readiness level 0 must not permit a manifest-only placeholder");

    assert!(error.to_string().contains("file entry"), "{error}");
}

#[test]
fn target_manifest_rejects_missing_files_after_readiness_zero() {
    let err = super::parse_target_manifest(
        r#"
version = 1
ir = "solve"
name = "unfinished"
readiness_level = 1
"#,
    )
    .expect_err("every target requires generated files");

    assert!(err.to_string().contains("file entry"), "{err}");
}

#[test]
fn target_manifest_rejects_empty_tensor_dtype() {
    let err = super::parse_target_manifest(
        r#"
version = 1
ir = "solve"
name = "invalid-dtypes"

[capabilities.tensor]
dtypes = ["f64", ""]

[[files]]
path = "model.out"
template = "model.out.jinja"
"#,
    )
    .expect_err("empty tensor dtype should fail");

    assert!(err.to_string().contains("dtypes"), "{err}");
}

#[test]
fn target_manifest_accepts_requirements_as_capabilities_alias() {
    let manifest = manifest_with_capabilities(
        r#"
[requirements]
continuous_states = false
residual_equations = false
"#,
    );
    let capabilities = manifest.capabilities.expect("requirements alias");

    assert_eq!(capabilities.continuous_states, Some(false));
    assert_eq!(capabilities.residual_equations, Some(false));
}

#[test]
fn shared_dae_renderer_blocks_placeholder_scalar_residuals() {
    let manifest = manifest_with_capabilities(
        r#"
[capabilities]
residual_equations = true
"#,
    );
    let templates =
        std::collections::BTreeMap::from([("model.out.jinja".to_string(), String::new())]);

    let error =
        super::render_dae_target_files(&templates, &manifest, &dae_with_placeholder_family(), "M")
            .expect_err("shared DAE rendering must not expose placeholder residuals");

    assert!(
        error
            .to_string()
            .contains("unsupported-feature:structured_equation_families"),
        "{error}"
    );
}

#[test]
fn shared_dae_renderer_allows_declared_structured_owner_consumer() {
    let manifest = manifest_with_capabilities(
        r#"
[capabilities]
residual_equations = true
structured_equation_families = true
"#,
    );
    let templates = std::collections::BTreeMap::from([(
        "model.out.jinja".to_string(),
        "{{ dae.systems.continuous.owners | length }}".to_string(),
    )]);

    let files =
        super::render_dae_target_files(&templates, &manifest, &dae_with_placeholder_family(), "M")
            .expect("declared family-aware consumer may render the canonical owner");

    assert_eq!(files[0].content, "1");
}

// --- checksum-web / asset-bundle validators (each fail-early branch) ---

/// A well-formed checksum web (one producer, one consumer edge) parses and
/// validates — the positive control for the rejection tests below.
#[test]
fn checksum_web_accepts_a_wellformed_declaration() {
    super::parse_target_manifest(
        r#"
version = 1
ir = "solve"
name = "checksum-web"
[[files]]
path = "a.txt"
template = "a.jinja"
id = "a"
[[files]]
path = "b.txt"
template = "b.jinja"
[[files.checksums]]
of = "a"
  algorithm = "sha1"
as = "a_sha1"
"#,
    )
    .expect("a well-formed checksum web validates");
}

fn expect_target_error(source: &str, needle: &str) {
    let err =
        super::parse_target_manifest(source).expect_err("malformed target.toml must be rejected");
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
ir = "solve"
name = "dup-id"
[[files]]
path = "a.txt"
template = "a.jinja"
id = "x"
[[files]]
path = "b.txt"
template = "b.jinja"
id = "x"
"#,
        "duplicate [[files]] id",
    );
}

#[test]
fn checksum_web_rejects_dangling_of() {
    expect_target_error(
        r#"
version = 1
ir = "solve"
name = "dangling"
[[files]]
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
ir = "solve"
name = "self-hash"
[[files]]
path = "a.txt"
template = "a.jinja"
id = "a"
[[files.checksums]]
of = "a"
  algorithm = "sha1"
as = "a_sha1"
"#,
        "checksums itself",
    );
}

#[test]
fn checksum_web_rejects_empty_as_key() {
    expect_target_error(
        r#"
version = 1
ir = "solve"
name = "empty-as"
[[files]]
path = "a.txt"
template = "a.jinja"
id = "a"
[[files]]
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
ir = "solve"
name = "dup-as"
[[files]]
path = "a.txt"
template = "a.jinja"
id = "a"
[[files]]
path = "c.txt"
template = "c.jinja"
id = "c"
[[files]]
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
ir = "solve"
name = "empty-bundle"
[[files]]
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
ir = "solve"
name = "empty-dest"
[[files]]
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
