use super::{
    TargetBundle, TargetFeatureSupport, TargetManifest, TargetRequiredProduct,
    TargetSemanticContext, TargetSemanticView, TensorCapability, TensorLayoutCapability,
    builtin_target_compatibility_matrix, safe_target_join, target_asset_relative_path, templates,
};
use rumoca_core::{
    RealMatrixMultiplySemantics, SourceMap, StructuredIndexBinder, StructuredIndexDomain, VarName,
};
use rumoca_ir_dae::{
    CoordinateInput, Dae, DaeLiteral, DaeProvenance, ExternalArgument, ExternalFunctionBody,
    ExternalLanguage, ExternalLinkage, FunctionPurity, FunctionSignature, ScalarType, ValueType,
};
use std::collections::BTreeMap;
use std::path::Path;

fn parse_target_manifest(source: &str) -> anyhow::Result<TargetManifest> {
    super::parse_target_manifest_construction(source).map(|construction| construction.manifest)
}

fn directory_target_bundle(
    dir: impl Into<std::path::PathBuf>,
    manifest: String,
) -> super::TargetBundle {
    super::TargetBundle {
        source: super::TargetBundleSource::Directory {
            dir: dir.into(),
            manifest,
        },
    }
}

fn validate_dae_target_capabilities(
    dae: &Dae,
    manifest: &TargetManifest,
    capabilities: &super::TargetCapabilities,
) -> anyhow::Result<()> {
    super::validate_dae_capabilities(
        dae,
        super::CapabilityTarget::for_dae_source(
            manifest.name.as_deref().unwrap_or("<unnamed>"),
            manifest.required_product,
        ),
        capabilities,
    )
}

fn validate_solve_target_capabilities(
    solve: &rumoca_ir_solve::SolveProblem,
    manifest: &TargetManifest,
    capabilities: &super::TargetCapabilities,
) -> anyhow::Result<()> {
    super::validate_solve_capabilities(
        solve,
        super::CapabilityTarget::for_solve_product(
            manifest.name.as_deref().unwrap_or("<unnamed>"),
            manifest.required_product,
        ),
        capabilities,
    )
}

impl TargetManifest {
    fn artifact_identity_keys(&self) -> &[String] {
        &self.artifact_identity_keys
    }
}

impl super::TargetFile {
    fn semantic_context(&self) -> super::TargetSemanticContext {
        self.semantic_context
    }

    fn semantic_view(&self) -> super::TargetSemanticView {
        self.semantic_view
    }

    fn artifact_identity_dependencies(&self) -> &[String] {
        self.required_artifact_identities.keys()
    }
}

const SCALAR_LITERAL_SOLVE_EXECUTABLE_PROFILE: &str = r#"
[solve_executable.value_capabilities]
boolean_scalar = true
boolean_tensor = false
signed_integer_scalar = true
signed_integer_tensor = false
unsigned_integer_scalar = false
unsigned_integer_tensor = false
real_scalar = true
real_tensor = false
nested_record_arrays = false
empty_fields = false
empty_values = false
enum_brands = false
opaque_value_handles = false

[solve_executable.operation_effect_capabilities]
admitted = ["constant", "declaration-initialization", "error-signal-reset", "lifecycle-method", "store"]

[solve_executable.environment]
environment = "freestanding"
allocation = "forbidden"
recursion = "forbidden"
max_automatic_payload_bytes = 65536
failure = "returned-status"
runtime_math = []
concurrency = "single-threaded"
atomics = "none"
admitted_library_contracts = []
isa_features = []

[solve_executable.emission]
kind = "loop"
"#;

const CORRELATED_EFMU_MEMBER_DECLARATIONS: &str = r#"
[package]
root = "MixedEfmi"
required_files = ["__content.xml"]

[[package.members]]
kind = "file"
file = "alg"

[[package.members]]
kind = "file"
file = "ac_manifest"

[[package.members]]
kind = "file"
file = "production_header"

[[package.members]]
kind = "file"
file = "production_source"

[[package.members]]
kind = "file"
file = "pc_manifest"

[[package.members]]
kind = "file"
file = "content"

[[assets]]
source = "schemas"
dest = "schemas/"
product_role = "schema"

[[files]]
id = "alg"
artifact_kind = "algorithm-code"
semantic_context = "galec"
view = "algorithm-code-package"
product_role = "algorithm-code-source"
path = "AlgorithmCode/model.alg"
template = "model.alg.jinja"

[[files]]
id = "ac_manifest"
artifact_kind = "xml"
semantic_context = "galec"
view = "algorithm-code-package"
product_role = "algorithm-code-manifest"
path = "AlgorithmCode/manifest.xml"
template = "ac_manifest.xml.jinja"

[[files]]
id = "production_header"
artifact_kind = "c-header"
semantic_context = "solve"
view = "solve-algorithm-block"
product_role = "production-header"
path = "ProductionCode/sources/production.h"
template = "production.h.jinja"

[[files]]
id = "production_source"
artifact_kind = "c-source"
semantic_context = "solve"
view = "solve-algorithm-block"
product_role = "production-source"
path = "ProductionCode/sources/production.c"
template = "production.c.jinja"

[[files]]
id = "pc_manifest"
artifact_kind = "xml"
semantic_context = "solve"
view = "solve-algorithm-block"
product_role = "production-manifest"
path = "ProductionCode/manifest.xml"
template = "pc_manifest.xml.jinja"

[[files]]
id = "content"
artifact_kind = "xml"
semantic_context = "galec"
view = "algorithm-code-package"
product_role = "package-manifest"
path = "__content.xml"
template = "__content.xml.jinja"
"#;

fn correlated_efmu_manifest_source() -> String {
    format!(
        r#"
version = 1
name = "mixed-efmi-schema-probe"

[arithmetic]
source_real = "binary32"
source_integer = "i32"
real_matrix_multiply = "separate_mul_add_ascending_positive_zero"

[capabilities]
scalar_fallback = false

{SCALAR_LITERAL_SOLVE_EXECUTABLE_PROFILE}

{CORRELATED_EFMU_MEMBER_DECLARATIONS}
"#
    )
}

fn packaged_algorithm_code_manifest_source() -> String {
    r#"
version = 1
name = "packaged-algorithm-code"

[arithmetic]
source_real = "binary32"
source_integer = "i32"
real_matrix_multiply = "separate_mul_add_ascending_positive_zero"

[capabilities]
scalar_fallback = false

[package]
root = "PackagedModel"
required_files = ["__content.xml"]

[[package.members]]
kind = "file"
file = "algorithm_code_source"

[[package.members]]
kind = "file"
file = "algorithm_code_manifest"

[[package.members]]
kind = "file"
file = "package_manifest"

[[assets]]
source = "schemas"
dest = "schemas/"
product_role = "schema"

[[files]]
id = "algorithm_code_source"
artifact_kind = "algorithm-code"
semantic_context = "galec"
product_role = "algorithm-code-source"
path = "AlgorithmCode/model.alg"
template = "model.alg.jinja"

[[files]]
id = "algorithm_code_manifest"
artifact_kind = "xml"
semantic_context = "galec"
product_role = "algorithm-code-manifest"
path = "AlgorithmCode/manifest.xml"
template = "manifest.xml.jinja"

[[files]]
id = "package_manifest"
artifact_kind = "xml"
semantic_context = "galec"
product_role = "package-manifest"
path = "__content.xml"
template = "__content.xml.jinja"
"#
    .to_owned()
}

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
                        id: rumoca_core::StructuredIndexBinderId::new(0),
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

/// One checked DAE whose only function is an MLS §12.9 external interface:
/// `pure function f input Real u; output Real y; external "C" y = my_func(u); end f;`
///
/// SEV-155's witness input: the `external_functions` capability gate must fire
/// on this DAE.
fn dae_with_external_function() -> Dae {
    let source_text =
        "pure function f input Real u; output Real y; external \"C\" y = my_func(u); end f;";
    let mut source_map = SourceMap::new();
    let source_id = source_map.add("external-capability.mo", source_text);
    let at = DaeProvenance::source(rumoca_core::Span::from_offsets(
        source_id,
        0,
        source_text.len(),
    ))
    .expect("fixture source span is exact");
    Dae::construct(source_map, |dae| {
        let real = dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), at))?;
        dae.function(
            FunctionSignature::new(VarName::new("f"), [real], [real], at),
            |dae, reservation| {
                let parameter = dae.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("u"), 0, at)
                })?;
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, at)
                })?;
                let argument = dae.expressions(|expressions| {
                    expressions
                        .at(at)
                        .coordinate(CoordinateInput::FunctionParameter(parameter))
                })?;
                let body = ExternalFunctionBody::new(
                    FunctionPurity::Pure,
                    ExternalLanguage::C,
                    VarName::new("my_func"),
                    [ExternalArgument::Input(argument)],
                    Some(output),
                    ExternalLinkage::new(["ModelicaExternalC".to_string()], None, None, None),
                );
                dae.functions(|functions| functions.define_external(reservation, body, at))
            },
        )
        .map(|_| ())
    })
    .expect("a checked external interface defines its reserved function")
}

/// The mirror fixture: the same one-function DAE with a *Modelica* body,
/// `function f input Real u; output Real y; algorithm y := u; end f;`.
///
/// It is what keeps the SEV-155 probe from being satisfied by a constant
/// `true`: the checked function table is nonempty, so only a real per-function
/// body test tells the two fixtures apart.
fn dae_with_modelica_function() -> Dae {
    let source_text = "function f input Real u; output Real y; algorithm y := u; end f;";
    let mut source_map = SourceMap::new();
    let source_id = source_map.add("modelica-function.mo", source_text);
    let at = DaeProvenance::source(rumoca_core::Span::from_offsets(
        source_id,
        0,
        source_text.len(),
    ))
    .expect("fixture source span is exact");
    Dae::construct(source_map, |dae| {
        let real = dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), at))?;
        dae.function(
            FunctionSignature::new(VarName::new("f"), [real], [real], at),
            |dae, reservation| {
                let parameter = dae.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("u"), 0, at)
                })?;
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, at)
                })?;
                let value = dae.expressions(|expressions| {
                    expressions
                        .at(at)
                        .coordinate(CoordinateInput::FunctionParameter(parameter))
                })?;
                let mut body = dae.functions(|functions| functions.begin(reservation, at))?;
                dae.functions(|functions| functions.assign(&mut body, output, value, at))?;
                dae.functions(|functions| functions.define(body, at))
            },
        )
        .map(|_| ())
    })
    .expect("a checked Modelica function body defines its reserved function")
}

fn manifest_with_capabilities(capabilities: &str) -> TargetManifest {
    toml::from_str(&manifest_with_capabilities_source(capabilities)).expect("parse target manifest")
}

fn manifest_with_capabilities_source(capabilities: &str) -> String {
    let capabilities = explicit_scalar_fallback(capabilities);
    format!(
        r#"
version = 1
name = "custom"
readiness_level = 3

{capabilities}

[[files]]
artifact_kind = "text"
semantic_context = "dae"
path = "model.txt"
template = "model.txt.jinja"
"#
    )
}

fn checked_target_from_manifest_source(source: &str) -> super::CheckedTargetBundle {
    let manifest = parse_target_manifest(source).expect("test manifest must be checked");
    let directory = tempfile::tempdir().expect("isolated checked target directory");
    for file in manifest.files() {
        if file.template_shared_from().is_some() {
            continue;
        }
        let path = directory.path().join(file.template());
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent).expect("create template parent");
        }
        std::fs::write(path, "checked test template").expect("write checked template");
    }
    directory_target_bundle(directory.path(), source.to_owned())
        .check()
        .expect("snapshot checked test target")
}

#[derive(Debug, PartialEq, Eq)]
struct CheckedSnapshotMember {
    path: String,
    bytes: Vec<u8>,
}

fn strict_test_compilation() -> crate::session::StrictCompilation {
    let mut session = crate::session::Session::default();
    session
        .add_document(
            "target-test.mo",
            r#"
model TargetTest
  discrete Real x(start = 0.0);
equation
  when sample(0.0, 0.01) then
    x = 1.0;
  end when;
end TargetTest;
"#,
        )
        .expect("parse target rendering fixture");
    session
        .compile_model_strict("TargetTest")
        .unwrap_or_else(|report| panic!("compile target rendering fixture: {report:?}"))
}

fn strict_external_function_compilation() -> crate::session::StrictCompilation {
    let mut session = crate::session::Session::default();
    session
        .add_document(
            "external-capability-source.mo",
            r#"
model ExternalCapabilitySource
  pure function foreign_square
    input Real u;
    output Real y;
  external "C" y = foreign_square_impl(u);
  end foreign_square;
  Real y;
equation
  y = foreign_square(time);
end ExternalCapabilitySource;
"#,
        )
        .expect("parse the local external-function source witness");
    session
        .compile_model_strict("ExternalCapabilitySource")
        .unwrap_or_else(|report| panic!("compile external-function source witness: {report:?}"))
}

fn test_artifact_input() -> super::ArtifactSessionInput {
    super::ArtifactSessionInput::construct(
        "2026-01-01T00:00:00Z"
            .parse()
            .expect("canonical test instant"),
        "00000000-0000-0000-0000-000000000001"
            .parse()
            .expect("canonical test identity seed"),
    )
}

fn checked_package_snapshots(target: super::CheckedTargetBundle) -> Vec<CheckedSnapshotMember> {
    let completed = strict_test_compilation()
        .render_target(target, test_artifact_input())
        .expect("render checked target snapshot");
    let super::CompletedTargetArtifact::Packaged(package) = completed else {
        panic!("expected packaged target artifact");
    };
    package
        .members()
        .map(|member| CheckedSnapshotMember {
            path: member.path().to_owned(),
            bytes: member.bytes().to_vec(),
        })
        .collect()
}

fn checked_unpacked_checksum_bindings(
    target: super::CheckedTargetBundle,
) -> Vec<Vec<(usize, super::ChecksumAlgorithm, String)>> {
    fn from_fold<T>(
        fold: super::CheckedDirectUnpackagedFold<T>,
    ) -> Vec<Vec<(usize, super::ChecksumAlgorithm, String)>> {
        fold.steps
            .into_vec()
            .into_iter()
            .map(|step| {
                step.core
                    .incoming_checksums
                    .into_iter()
                    .map(|binding| match binding {
                        super::TargetResolvedChecksumBinding::Sha1 { producer, as_key } => {
                            (producer.0, super::ChecksumAlgorithm::Sha1, as_key)
                        }
                    })
                    .collect()
            })
            .collect()
    }

    let super::CheckedTargetBundle {
        render_authority, ..
    } = target;
    let plan = render_authority.into_plan();
    let super::CheckedTargetRenderPlan::Unpackaged(plan) = plan else {
        panic!("expected unpackaged target plan");
    };
    let super::CheckedUnpackagedTargetProductPlan::Other(plan) = plan
        .into_product_plan()
        .expect("close direct unpackaged product")
    else {
        panic!("expected direct unpackaged product");
    };
    match plan {
        super::CheckedOtherUnpackagedTargetPlan::Ast(fold) => from_fold(fold),
        super::CheckedOtherUnpackagedTargetPlan::Flat(fold) => from_fold(fold),
        super::CheckedOtherUnpackagedTargetPlan::Dae { fold, .. } => from_fold(fold),
        super::CheckedOtherUnpackagedTargetPlan::SolveModel { fold, .. } => from_fold(fold),
        super::CheckedOtherUnpackagedTargetPlan::FmiComponent { fold, .. } => from_fold(fold),
    }
}

fn parse_manifest_with_context_capabilities(
    semantic_context: &str,
    capabilities: &str,
) -> TargetManifest {
    let capabilities = explicit_scalar_fallback(capabilities);
    parse_target_manifest(&format!(
        r#"
version = 1
name = "custom"
readiness_level = 1

{capabilities}

[[files]]
artifact_kind = "text"
semantic_context = "{semantic_context}"
path = "model.txt"
template = "model.txt.jinja"
"#
    ))
    .expect("parse and validate target manifest")
}

fn parse_algorithm_code_manifest_with_capabilities(capabilities: &str) -> TargetManifest {
    parse_target_manifest(&algorithm_code_manifest_with_capabilities_source(
        capabilities,
    ))
    .expect("parse and validate Algorithm Code target manifest")
}

fn algorithm_code_manifest_with_capabilities_source(capabilities: &str) -> String {
    let capabilities = explicit_scalar_fallback(capabilities);
    format!(
        r#"
version = 1
name = "custom"
readiness_level = 1

[arithmetic]
source_real = "binary32"
source_integer = "i32"
real_matrix_multiply = "separate_mul_add_ascending_positive_zero"

{capabilities}

[[files]]
artifact_kind = "algorithm-code"
semantic_context = "galec"
path = "model.alg"
template = "model.alg.jinja"
"#
    )
}

fn explicit_scalar_fallback(capabilities: &str) -> String {
    if capabilities.contains("[capabilities]") && !capabilities.contains("scalar_fallback") {
        capabilities.replacen(
            "[capabilities]",
            "[capabilities]\nscalar_fallback = false",
            1,
        )
    } else {
        capabilities.to_owned()
    }
}

struct ExactAlgebraicParts {
    layout: rumoca_ir_solve::VarLayout,
    solve_layout: rumoca_ir_solve::SolveLayout,
    implicit_rhs: rumoca_ir_solve::ComputeBlock,
    implicit_row_targets: Vec<Option<rumoca_ir_solve::ScalarSlot>>,
    algebraic_projection_plan: rumoca_ir_solve::AlgebraicProjectionPlan,
    residual: rumoca_ir_solve::ComputeBlock,
    refresh_plans: rumoca_ir_solve::ContinuousRefreshPlanInputs,
}

type ExactAlgebraicResult =
    Result<rumoca_ir_solve::SolveProblem, Box<rumoca_ir_solve::SolveProblemShapeContractError>>;

impl ExactAlgebraicParts {
    fn construct(self) -> ExactAlgebraicResult {
        let continuous = checked_continuous_system(
            &self.solve_layout,
            self.implicit_rhs,
            self.implicit_row_targets,
            self.algebraic_projection_plan,
            self.residual,
            rumoca_ir_solve::ComputeBlock::default(),
            self.refresh_plans,
        );
        rumoca_ir_solve::SolveProblem::construct(
            self.layout,
            self.solve_layout,
            continuous,
            rumoca_ir_solve::InitializationSolveSystem::empty(),
            rumoca_ir_solve::DiscreteSolveSystem::default(),
            rumoca_ir_solve::SolveEventPartition::default(),
            rumoca_ir_solve::SolveClockPartition::default(),
        )
        .map_err(Box::new)
    }
}

fn checked_continuous_system(
    solve_layout: &rumoca_ir_solve::SolveLayout,
    implicit_rhs: rumoca_ir_solve::ComputeBlock,
    implicit_row_targets: Vec<Option<rumoca_ir_solve::ScalarSlot>>,
    algebraic_projection_plan: rumoca_ir_solve::AlgebraicProjectionPlan,
    residual: rumoca_ir_solve::ComputeBlock,
    derivative_rhs: rumoca_ir_solve::ComputeBlock,
    refresh_plans: rumoca_ir_solve::ContinuousRefreshPlanInputs,
) -> rumoca_ir_solve::ContinuousSolveSystem {
    rumoca_ir_solve::ContinuousSolveSystem::construct(
        solve_layout,
        rumoca_ir_solve::ContinuousSolveSystemInputs::new(
            implicit_rhs,
            implicit_row_targets,
            algebraic_projection_plan,
            residual,
            (
                rumoca_ir_solve::ComputeBlock::default(),
                rumoca_ir_solve::AlgebraicProjectionPlan::default(),
            ),
            derivative_rhs,
            refresh_plans,
        ),
    )
    .expect("fixture inputs and issued refresh ownership must form one continuous system")
}

fn empty_continuous_system() -> rumoca_ir_solve::ContinuousSolveSystem {
    let implicit_rhs = rumoca_ir_solve::ComputeBlock::default();
    let solve_layout = rumoca_ir_solve::SolveLayout::default();
    checked_continuous_system(
        &solve_layout,
        implicit_rhs,
        Vec::new(),
        rumoca_ir_solve::AlgebraicProjectionPlan::default(),
        rumoca_ir_solve::ComputeBlock::default(),
        rumoca_ir_solve::ComputeBlock::default(),
        rumoca_ir_solve::ContinuousRefreshPlanInputs::empty(),
    )
}

fn issued_exact_algebraic_parts() -> ExactAlgebraicParts {
    use rumoca_ir_solve as solve;

    let implicit_rhs = exact_algebraic_fixture_source();
    let row = solve::AlgebraicRefreshRow::checked(solve::AlgebraicRefreshRowDraft {
        owner_id: solve::RefreshRowOwnerId::checked(0).expect("fixture owner fits"),
        source: solve::RefreshScalarProgramSource::checked(0, 0).expect("fixture source fits"),
        equation_index: 0,
        output_offset: 0,
        target_index: 0,
        assignment_target: Some(0),
        assignment_shape: Some(solve::TargetAssignmentShape::Direct {
            target_y_index: 0,
            expr_reg: 1,
            target_scale: 1.0,
            expr_eval_len: 2,
        }),
        direct_assignment_certified: true,
        exact_assignment_certified: true,
    })
    .expect("fixture refresh row is valid");
    let algebraic = solve::RefreshPlan {
        simultaneous_plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![0],
                y_indices: vec![0],
                tearing: None,
            }],
        },
        simultaneous_block_indices: vec![0],
        rows: vec![row],
        causal_seed_rows: solve::RefreshRowSelection::checked(1, [0])
            .expect("fixture selection is valid"),
        dynamic_causal_seed_rows: solve::RefreshRowSelection::checked(1, [0])
            .expect("fixture selection is valid"),
        value_stages: vec![solve::RefreshStage::ExactAssignments {
            static_rows: solve::RefreshRowSelection::empty(),
            dynamic_rows: solve::RefreshRowSelection::checked(1, [0])
                .expect("fixture selection is valid"),
        }],
        ..solve::RefreshPlan::empty()
    };
    let solve_layout = solve::SolveLayout {
        solver_maps: solve::SolverNameIndexMaps {
            names: vec!["y".to_string()],
            name_to_idx: indexmap::IndexMap::from([("y".to_string(), 0)]),
            base_to_indices: indexmap::IndexMap::from([("y".to_string(), vec![0])]),
        },
        algebraic_scalar_count: 1,
        ..solve::SolveLayout::default()
    };
    let refresh_plans = solve::ContinuousRefreshPlanInputs::new(
        algebraic,
        solve::RefreshPlan::empty(),
        solve::RefreshPlan::empty(),
        solve::RefreshPlan::empty(),
        Vec::new(),
    );
    let residual = implicit_rhs.clone();
    let implicit_row_targets = vec![Some(solve::ScalarSlot::Y { index: 0 })];
    let algebraic_projection_plan = solve::AlgebraicProjectionPlan {
        blocks: vec![solve::AlgebraicProjectionBlock {
            rows: vec![0],
            y_indices: vec![0],
            tearing: None,
        }],
    };
    ExactAlgebraicParts {
        layout: solve::VarLayout::from_parts(indexmap::IndexMap::new(), 1, 0),
        solve_layout,
        implicit_rhs,
        implicit_row_targets,
        algebraic_projection_plan,
        residual,
        refresh_plans,
    }
}

fn solve_with_issued_exact_algebraic_assignment() -> rumoca_ir_solve::SolveProblem {
    issued_exact_algebraic_parts()
        .construct()
        .expect("the exact-algebraic fixture must satisfy the root construction relation")
}

fn solve_with_residual_algebraic_projection() -> rumoca_ir_solve::SolveProblem {
    let mut parts = issued_exact_algebraic_parts();
    parts.refresh_plans = rumoca_ir_solve::ContinuousRefreshPlanInputs::empty();
    parts
        .construct()
        .expect("the 1x1 implicit fixture retains a real residual projection")
}

fn exact_algebraic_fixture_source() -> rumoca_ir_solve::ComputeBlock {
    use rumoca_ir_solve as solve;

    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("exact-algebraic-target.mo"),
        0,
        1,
    );
    let provenance = rumoca_core::ProvenanceSpan::new(span, "exact algebraic target fixture")
        .expect("fixture provenance is valid");
    solve::ComputeBlock::from_scalar_program_block(
        solve::ScalarProgramBlock::with_source_span(
            vec![vec![
                solve::LinearOp::LoadY { dst: 0, index: 0 },
                solve::LinearOp::Const { dst: 1, value: 1.0 },
                solve::LinearOp::Binary {
                    dst: 2,
                    op: solve::BinaryOp::Sub,
                    lhs: 0,
                    rhs: 1,
                },
                solve::LinearOp::StoreOutput { src: 2 },
            ]],
            provenance,
        )
        .expect("fixture scalar program is valid"),
    )
}

fn unrelated_single_output_block() -> rumoca_ir_solve::ComputeBlock {
    use rumoca_ir_solve as solve;

    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("forged-exact-algebraic.mo"),
        0,
        1,
    );
    let provenance = rumoca_core::ProvenanceSpan::new(span, "forged exact algebraic fixture")
        .expect("fixture provenance is valid");
    solve::ComputeBlock::from_scalar_program_block(
        solve::ScalarProgramBlock::with_source_span(
            vec![vec![
                solve::LinearOp::Const { dst: 0, value: 7.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]],
            provenance,
        )
        .expect("fixture scalar program is valid"),
    )
}

fn block_with_unowned_second_output() -> rumoca_ir_solve::ComputeBlock {
    use rumoca_ir_solve as solve;

    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("unowned-exact-algebraic.mo"),
        0,
        1,
    );
    let provenance = rumoca_core::ProvenanceSpan::new(span, "unowned exact algebraic fixture")
        .expect("fixture provenance is valid");
    solve::ComputeBlock::from_scalar_program_block(
        solve::ScalarProgramBlock::with_source_span(
            vec![
                vec![
                    solve::LinearOp::LoadY { dst: 0, index: 0 },
                    solve::LinearOp::Const { dst: 1, value: 1.0 },
                    solve::LinearOp::Binary {
                        dst: 2,
                        op: solve::BinaryOp::Sub,
                        lhs: 0,
                        rhs: 1,
                    },
                    solve::LinearOp::StoreOutput { src: 2 },
                ],
                vec![
                    solve::LinearOp::Const { dst: 0, value: 9.0 },
                    solve::LinearOp::StoreOutput { src: 0 },
                ],
            ],
            provenance,
        )
        .expect("fixture scalar programs are valid"),
    )
}

fn post_output_fresh_definition_block() -> rumoca_ir_solve::ComputeBlock {
    use rumoca_ir_solve as solve;

    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("post-output-redefinition.mo"),
        0,
        1,
    );
    let provenance = rumoca_core::ProvenanceSpan::new(span, "post-output redefinition fixture")
        .expect("fixture provenance is valid");
    solve::ComputeBlock::from_scalar_program_block(
        solve::ScalarProgramBlock::with_source_span(
            vec![vec![
                solve::LinearOp::LoadY { dst: 0, index: 0 },
                solve::LinearOp::Const { dst: 1, value: 1.0 },
                solve::LinearOp::Binary {
                    dst: 2,
                    op: solve::BinaryOp::Sub,
                    lhs: 0,
                    rhs: 1,
                },
                solve::LinearOp::StoreOutput { src: 2 },
                solve::LinearOp::Const { dst: 3, value: 2.0 },
            ]],
            provenance,
        )
        .expect("fixture scalar program is valid"),
    )
}

fn dependent_algebraic_fixture_source() -> rumoca_ir_solve::ComputeBlock {
    use rumoca_ir_solve as solve;

    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("dependent-algebraic-wire.mo"),
        0,
        1,
    );
    let provenance = rumoca_core::ProvenanceSpan::new(span, "dependent algebraic wire fixture")
        .expect("fixture provenance is valid");
    solve::ComputeBlock::from_scalar_program_block(
        solve::ScalarProgramBlock::with_source_span(
            vec![
                vec![
                    solve::LinearOp::LoadY { dst: 0, index: 1 },
                    solve::LinearOp::LoadY { dst: 1, index: 0 },
                    solve::LinearOp::Binary {
                        dst: 2,
                        op: solve::BinaryOp::Sub,
                        lhs: 0,
                        rhs: 1,
                    },
                    solve::LinearOp::StoreOutput { src: 2 },
                ],
                vec![
                    solve::LinearOp::LoadY { dst: 0, index: 2 },
                    solve::LinearOp::LoadY { dst: 1, index: 1 },
                    solve::LinearOp::Binary {
                        dst: 2,
                        op: solve::BinaryOp::Sub,
                        lhs: 0,
                        rhs: 1,
                    },
                    solve::LinearOp::StoreOutput { src: 2 },
                ],
            ],
            provenance,
        )
        .expect("fixture scalar programs are valid"),
    )
}

fn dependent_algebraic_fixture_row(
    ordinal: usize,
    expression: u32,
) -> rumoca_ir_solve::AlgebraicRefreshRow {
    use rumoca_ir_solve as solve;

    solve::AlgebraicRefreshRow::checked(solve::AlgebraicRefreshRowDraft {
        owner_id: solve::RefreshRowOwnerId::checked(ordinal).expect("fixture owner fits"),
        source: solve::RefreshScalarProgramSource::checked(0, ordinal)
            .expect("fixture source fits"),
        equation_index: ordinal,
        output_offset: 0,
        target_index: ordinal + 1,
        assignment_target: Some(ordinal + 1),
        assignment_shape: Some(solve::TargetAssignmentShape::Direct {
            target_y_index: ordinal + 1,
            expr_reg: expression,
            target_scale: 1.0,
            expr_eval_len: 2,
        }),
        direct_assignment_certified: true,
        exact_assignment_certified: true,
    })
    .expect("fixture refresh row is valid")
}

fn solve_with_dependent_algebraic_assignments() -> rumoca_ir_solve::SolveProblem {
    use rumoca_ir_solve as solve;

    let implicit_rhs = dependent_algebraic_fixture_source();
    let projection = solve::AlgebraicProjectionPlan {
        blocks: vec![
            solve::AlgebraicProjectionBlock {
                rows: vec![0],
                y_indices: vec![1],
                tearing: None,
            },
            solve::AlgebraicProjectionBlock {
                rows: vec![1],
                y_indices: vec![2],
                tearing: None,
            },
        ],
    };
    let algebraic = solve::RefreshPlan {
        simultaneous_plan: projection.clone(),
        simultaneous_block_indices: vec![0, 1],
        rows: vec![
            dependent_algebraic_fixture_row(0, 1),
            dependent_algebraic_fixture_row(1, 1),
        ],
        causal_seed_rows: solve::RefreshRowSelection::checked(2, [0, 1])
            .expect("fixture selection is valid"),
        dynamic_causal_seed_rows: solve::RefreshRowSelection::checked(2, [0, 1])
            .expect("fixture selection is valid"),
        value_stages: vec![solve::RefreshStage::ExactAssignments {
            static_rows: solve::RefreshRowSelection::empty(),
            dynamic_rows: solve::RefreshRowSelection::checked(2, [0, 1])
                .expect("fixture selection is valid"),
        }],
        ..solve::RefreshPlan::empty()
    };
    let solve_layout = solve::SolveLayout {
        solver_maps: solve::SolverNameIndexMaps {
            names: vec!["x".to_string(), "y0".to_string(), "y1".to_string()],
            name_to_idx: indexmap::IndexMap::from([
                ("x".to_string(), 0),
                ("y0".to_string(), 1),
                ("y1".to_string(), 2),
            ]),
            base_to_indices: indexmap::IndexMap::from([
                ("x".to_string(), vec![0]),
                ("y0".to_string(), vec![1]),
                ("y1".to_string(), vec![2]),
            ]),
        },
        state_scalar_count: 1,
        algebraic_scalar_count: 2,
        ..solve::SolveLayout::default()
    };
    let refresh_plans = solve::ContinuousRefreshPlanInputs::new(
        algebraic,
        solve::RefreshPlan::empty(),
        solve::RefreshPlan::empty(),
        solve::RefreshPlan::empty(),
        Vec::new(),
    );
    let continuous = checked_continuous_system(
        &solve_layout,
        implicit_rhs.clone(),
        vec![
            Some(solve::ScalarSlot::Y { index: 1 }),
            Some(solve::ScalarSlot::Y { index: 2 }),
        ],
        projection,
        implicit_rhs,
        unrelated_single_output_block(),
        refresh_plans,
    );
    solve::SolveProblem::construct(
        solve::VarLayout::from_parts(indexmap::IndexMap::new(), 3, 0),
        solve_layout,
        continuous,
        solve::InitializationSolveSystem::empty(),
        solve::DiscreteSolveSystem::default(),
        solve::SolveEventPartition::default(),
        solve::SolveClockPartition::default(),
    )
    .expect("the dependent exact-algebraic fixture must satisfy root construction")
}

fn pre_output_redefinition_error() -> rumoca_ir_solve::SolveProblemShapeContractError {
    use rumoca_ir_solve as solve;

    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("pre-output-redefinition.mo"),
        0,
        1,
    );
    let provenance = rumoca_core::ProvenanceSpan::new(span, "pre-output redefinition fixture")
        .expect("fixture provenance is valid");
    solve::ScalarProgramBlock::with_source_span(
        vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::Const { dst: 0, value: 7.0 },
            solve::LinearOp::Const { dst: 1, value: 1.0 },
            solve::LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Sub,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::StoreOutput { src: 2 },
        ]],
        provenance,
    )
    .expect_err("overlapping destination writes must not mint a scalar-program block")
}

#[test]
fn exact_algebraic_assignment_completeness_is_rederived_on_wire_replay() {
    let problem = solve_with_issued_exact_algebraic_assignment();
    assert!(rumoca_phase_codegen::explicit_algebraic_assignment_complete(&problem));

    let json = serde_json::to_string(&problem).expect("serialize exact algebraic Solve problem");
    assert!(!json.contains("algebraic_assignment_complete"));
    let replayed_json: rumoca_ir_solve::SolveProblem =
        serde_json::from_str(&json).expect("replay exact algebraic Solve problem from JSON");
    assert!(rumoca_phase_codegen::explicit_algebraic_assignment_complete(&replayed_json));

    let bytes = bincode::serialize(&problem).expect("serialize exact algebraic Solve problem");
    let replayed_binary: rumoca_ir_solve::SolveProblem =
        bincode::deserialize(&bytes).expect("replay exact algebraic Solve problem from bincode");
    assert!(rumoca_phase_codegen::explicit_algebraic_assignment_complete(&replayed_binary));
}

#[test]
fn solve_wire_rejects_a_forged_exact_assignment_register() {
    let mut wire = serde_json::to_value(solve_with_issued_exact_algebraic_assignment())
        .expect("serialize exact algebraic Solve problem");
    wire["continuous"]["refresh_owners"]["algebraic"]["rows"][0]["assignment_shape"]["Direct"]["expr_reg"] =
        serde_json::json!(0);

    let error = serde_json::from_value::<rumoca_ir_solve::SolveProblem>(wire)
        .expect_err("wire replay must rederive the assignment shape from its canonical source");
    assert!(
        error
            .to_string()
            .contains("assignment certificate disagrees"),
        "unexpected replay error: {error}"
    );
}

#[test]
fn solve_wire_rejects_forged_direct_assignment_metadata() {
    for (field, value, expected) in [
        (
            "expr_eval_len",
            serde_json::json!(3),
            "assignment certificate disagrees",
        ),
        (
            "target_scale",
            serde_json::json!(-1.0),
            "assignment certificate disagrees",
        ),
        (
            "target_y_index",
            serde_json::json!(1),
            "assignment certificate belongs to another target",
        ),
    ] {
        let mut wire = serde_json::to_value(solve_with_issued_exact_algebraic_assignment())
            .expect("serialize exact algebraic Solve problem");
        wire["continuous"]["refresh_owners"]["algebraic"]["rows"][0]["assignment_shape"]["Direct"]
            [field] = value;

        let error = serde_json::from_value::<rumoca_ir_solve::SolveProblem>(wire)
            .expect_err("wire replay must reject every forged Direct certificate field");
        assert!(
            error.to_string().contains(expected),
            "forged {field} returned the wrong replay error: {error}"
        );
    }
}

#[test]
fn solve_construction_rejects_a_certificate_for_another_source() {
    let parts = issued_exact_algebraic_parts();
    let unrelated = unrelated_single_output_block();
    let error = rumoca_ir_solve::ContinuousSolveSystem::construct(
        &parts.solve_layout,
        rumoca_ir_solve::ContinuousSolveSystemInputs::new(
            unrelated.clone(),
            parts.implicit_row_targets,
            parts.algebraic_projection_plan,
            unrelated,
            (
                rumoca_ir_solve::ComputeBlock::default(),
                rumoca_ir_solve::AlgebraicProjectionPlan::default(),
            ),
            rumoca_ir_solve::ComputeBlock::default(),
            parts.refresh_plans,
        ),
    )
    .expect_err("a foreign assignment certificate must not mint a continuous system");
    assert!(
        error
            .to_string()
            .contains("assignment certificate disagrees")
    );
}

#[test]
fn exact_assignment_consumer_uses_the_definition_reaching_the_output() {
    let mut parts = issued_exact_algebraic_parts();
    let source = post_output_fresh_definition_block();
    parts.implicit_rhs = source.clone();
    parts.residual = source;
    let solve = parts
        .construct()
        .expect("a fresh register defined after the output preserves SSA and its certificate");
    let target = templates::builtin_targets()
        .iter()
        .find(|target| target.name == "fmi3")
        .expect("fmi3 target is built in");
    let manifest = parse_target_manifest(target.manifest).expect("fmi3 manifest is valid");
    let capabilities = manifest.capabilities.as_ref().expect("capabilities");

    validate_solve_target_capabilities(&solve, &manifest, capabilities)
        .expect("a later fresh definition cannot change the certified output value");
}

#[test]
fn scalar_program_construction_rejects_a_pre_output_register_redefinition() {
    let error = pre_output_redefinition_error();

    assert!(matches!(
        error,
        rumoca_ir_solve::SolveProblemShapeContractError::ScalarProgramRegisterFlow {
            program_index: 0,
            error: rumoca_ir_solve::ScalarProgramRegisterError::DestinationRegisterAlreadyDefined {
                op_index: 1,
                operation: "Const",
                register: 0,
            },
            ..
        }
    ));
}

#[test]
fn exact_assignment_consumer_rejects_reordered_dependent_algebraics_after_replay() {
    let mut wire = serde_json::to_value(solve_with_dependent_algebraic_assignments())
        .expect("serialize the causal exact schedule");
    wire["continuous"]["algebraic_projection_plan"]["blocks"]
        .as_array_mut()
        .expect("root projection is an array")
        .reverse();
    wire["continuous"]["refresh_owners"]["algebraic"]["simultaneous_plan"]["blocks"]
        .as_array_mut()
        .expect("owner projection is an array")
        .reverse();
    for selection in ["causal_seed_rows", "dynamic_causal_seed_rows"] {
        wire["continuous"]["refresh_owners"]["algebraic"][selection] = serde_json::json!([1, 0]);
    }
    wire["continuous"]["refresh_owners"]["algebraic"]["value_stages"][0]["ExactAssignments"]["dynamic_rows"] =
        serde_json::json!([1, 0]);

    let error = serde_json::from_value::<rumoca_ir_solve::SolveProblem>(wire)
        .expect_err("wire replay must reject a non-causal exact schedule");
    assert!(
        error.to_string().contains("non-causal"),
        "unexpected replay error: {error}"
    );
}

#[test]
fn exact_assignment_consumer_accepts_causal_dependent_algebraics_after_replay() {
    let problem = solve_with_dependent_algebraic_assignments();
    assert!(rumoca_phase_codegen::explicit_algebraic_assignment_complete(&problem));

    let json = serde_json::to_string(&problem).expect("serialize the causal exact schedule");
    let replayed: rumoca_ir_solve::SolveProblem =
        serde_json::from_str(&json).expect("replay the causal exact schedule");
    assert!(rumoca_phase_codegen::explicit_algebraic_assignment_complete(&replayed));
}

#[test]
fn solve_construction_rejects_an_unowned_implicit_output() {
    let mut parts = issued_exact_algebraic_parts();
    let implicit_rhs = block_with_unowned_second_output();
    parts.implicit_rhs = implicit_rhs.clone();
    parts.residual = implicit_rhs;
    parts.implicit_row_targets.push(None);

    let error = parts
        .construct()
        .expect_err("an unowned implicit output must not mint a Solve root");
    assert!(matches!(
        *error,
        rumoca_ir_solve::SolveProblemShapeContractError::ContinuousRefreshOwner { ref detail }
            if detail.contains("do not cover the required algebraic Y inventory")
    ));
}

#[test]
fn solve_construction_rejects_an_unissued_algebraic_suffix() {
    let error = rumoca_ir_solve::SolveProblem::construct(
        rumoca_ir_solve::VarLayout::from_parts(indexmap::IndexMap::new(), 1, 0),
        rumoca_ir_solve::SolveLayout {
            solver_maps: rumoca_ir_solve::SolverNameIndexMaps {
                names: vec!["y".to_string()],
                name_to_idx: indexmap::IndexMap::from([("y".to_string(), 0)]),
                base_to_indices: indexmap::IndexMap::from([("y".to_string(), vec![0])]),
            },
            algebraic_scalar_count: 1,
            ..rumoca_ir_solve::SolveLayout::default()
        },
        empty_continuous_system(),
        rumoca_ir_solve::InitializationSolveSystem::empty(),
        rumoca_ir_solve::DiscreteSolveSystem::default(),
        rumoca_ir_solve::SolveEventPartition::default(),
        rumoca_ir_solve::SolveClockPartition::default(),
    )
    .expect_err("an algebraic suffix without issued rows must not mint a Solve root");
    assert!(matches!(
        error,
        rumoca_ir_solve::SolveProblemShapeContractError::ContinuousRefreshOwner { ref detail }
            if detail.contains("do not cover the required algebraic Y inventory")
    ));
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
    let manifest = parse_manifest_with_context_capabilities(
        "solve",
        r#"
[capabilities]
scalar_fallback = false
external_functions = false
events = true
runtime_events = false
exact_algebraic_assignments = true
forward_ad = true
reverse_ad = false
dynamic_control_flow = true
host_callbacks = false
"#,
    );
    let capabilities = manifest.capabilities.expect("capabilities table");

    assert_eq!(manifest.readiness_level, Some(1));
    assert_eq!(capabilities.external_functions, Some(false));
    assert_eq!(capabilities.events, Some(true));
    assert_eq!(capabilities.runtime_events, Some(false));
    assert_eq!(capabilities.exact_algebraic_assignments, Some(true));
    assert_eq!(capabilities.forward_ad, Some(true));
    assert_eq!(capabilities.reverse_ad, Some(false));
    assert_eq!(capabilities.dynamic_control_flow, Some(true));
    assert_eq!(capabilities.host_callbacks, Some(false));
}

#[test]
fn all_builtin_target_manifests_and_templates_construct() {
    for target in templates::builtin_targets() {
        parse_target_manifest(target.manifest).unwrap_or_else(|err| {
            panic!("built-in target '{}' failed to parse: {err}", target.name)
        });
        TargetBundle::builtin(target.name)
            .expect("enumerated built-in target remains registered")
            .check()
            .unwrap_or_else(|error| {
                panic!(
                    "built-in target '{}' failed template construction: {error:#}",
                    target.name
                )
            });
    }
}

#[test]
fn efmu_borrows_the_canonical_galec_template_bytes_for_builtin_and_directory_targets() {
    let owner = templates::builtin_target("galec").expect("registered GALEC owner");
    let expected = owner
        .template_source("model.alg.jinja")
        .expect("GALEC owns its complete Algorithm Code template");
    let efmu = templates::builtin_target("efmu").expect("registered eFMU borrower");
    let manifest = parse_target_manifest(efmu.manifest).expect("checked eFMU manifest");
    let builtin = super::TargetBundle::builtin("efmu")
        .expect("registered eFMU borrower")
        .check()
        .expect("checked eFMU target snapshot");
    let file = manifest
        .files()
        .iter()
        .find(|file| file.template() == "model.alg.jinja")
        .expect("eFMU Algorithm Code source declaration");
    assert_eq!(file.template_shared_from(), Some("galec"));
    let builtin_bytes = checked_package_snapshots(builtin)
        .into_iter()
        .find(|member| member.path == "AlgorithmCode/model.alg")
        .expect("borrowed Algorithm Code output")
        .bytes;
    assert!(
        templates::builtin_target("efmu")
            .expect("registered eFMU borrower")
            .template_source("model.alg.jinja")
            .is_none(),
        "the borrower must not embed a second local copy"
    );
    let independently_owned = parse_target_manifest(
        &templates::builtin_target("efmu")
            .expect("registered eFMU borrower")
            .manifest
            .replace("template_shared_from = \"galec\"\n", ""),
    )
    .expect("ownership edge is the only changed manifest fact");
    assert_ne!(
        manifest
            .canonical_artifact_identity_digest()
            .expect("borrowed identity digest"),
        independently_owned
            .canonical_artifact_identity_digest()
            .expect("independent identity digest"),
        "template ownership must participate in canonical custom-target identity"
    );

    let directory = tempfile::tempdir().expect("isolated copied target directory");
    for declaration in manifest.files() {
        if declaration.template_shared_from().is_some() {
            continue;
        }
        std::fs::write(
            directory.path().join(declaration.template()),
            efmu.template_source(declaration.template())
                .expect("eFMU owns every nonborrowed template"),
        )
        .expect("copy eFMU-owned template into directory target");
    }
    let copied = directory_target_bundle(directory.path(), efmu.manifest.to_owned())
        .check()
        .expect("copied target retains and snapshots the borrowing edge");
    let copied_bytes = checked_package_snapshots(copied)
        .into_iter()
        .find(|member| member.path == "AlgorithmCode/model.alg")
        .expect("directory borrowed Algorithm Code output")
        .bytes;
    assert_eq!(builtin_bytes, copied_bytes);
    assert!(
        !expected.is_empty(),
        "canonical owner template must be nonempty"
    );
}

#[test]
fn template_borrowing_rejects_invalid_owners_and_local_duplicate_bytes() {
    let source = templates::builtin_target("efmu")
        .expect("registered eFMU borrower")
        .manifest;
    for (owner, needle) in [
        ("efmu", "cannot borrow from its declaring target"),
        ("missing-owner", "unknown built-in target"),
        ("fmi3", "owns no such template"),
    ] {
        let changed = source.replace(
            "template_shared_from = \"galec\"",
            &format!("template_shared_from = \"{owner}\""),
        );
        let error = parse_target_manifest(&changed)
            .expect_err("an invalid complete-template owner must reject construction");
        assert!(
            format!("{error:#}").contains(needle),
            "unexpected error for owner {owner}: {error:#}"
        );
    }

    let directory = tempfile::tempdir().expect("isolated duplicate target directory");
    std::fs::write(directory.path().join("model.alg.jinja"), "duplicate")
        .expect("write forbidden local duplicate");
    let duplicate = directory_target_bundle(directory.path(), source.to_owned());
    let error = duplicate
        .check()
        .expect_err("a borrower with local bytes must not construct");
    assert!(format!("{error:#}").contains("also exists locally"));
}

#[test]
fn checked_target_snapshots_templates_assets_and_declaration_order_once() {
    let directory = tempfile::tempdir().expect("isolated target snapshot directory");
    std::fs::create_dir(directory.path().join("assets")).expect("create asset source");
    std::fs::write(directory.path().join("first.jinja"), "first-v1").expect("write first template");
    std::fs::write(directory.path().join("second.jinja"), "second-v1")
        .expect("write second template");
    std::fs::write(directory.path().join("assets/z.txt"), b"z-v1").expect("write z asset");
    std::fs::write(directory.path().join("assets/a.txt"), b"a-v1").expect("write a asset");
    let manifest = r#"
version = 1
name = "snapshot-order"

[package]
root = "snapshot"

[[package.members]]
kind = "file"
file = "z_output"

[[package.members]]
kind = "file"
file = "a_output"

[[package.members]]
kind = "asset"
source = "assets"
path = "a.txt"

[[package.members]]
kind = "asset"
source = "assets"
path = "z.txt"

[[assets]]
source = "assets"
dest = "copied/"

[[files]]
id = "z_output"
artifact_kind = "text"
semantic_context = "ast"
path = "z-output.txt"
template = "first.jinja"

[[files]]
id = "a_output"
artifact_kind = "text"
semantic_context = "ast"
path = "a-output.txt"
template = "second.jinja"
"#;
    let target = directory_target_bundle(directory.path(), manifest.to_owned())
        .check()
        .expect("snapshot exact target bytes");

    std::fs::write(directory.path().join("first.jinja"), "first-v2")
        .expect("mutate source template after check");
    std::fs::write(directory.path().join("assets/a.txt"), b"a-v2")
        .expect("mutate source asset after check");
    std::fs::write(directory.path().join("assets/new.txt"), b"new")
        .expect("add source asset after check");

    let members = checked_package_snapshots(target)
        .into_iter()
        .map(|member| (member.path, member.bytes))
        .collect::<Vec<_>>();
    assert_eq!(
        members,
        vec![
            ("z-output.txt".to_owned(), b"first-v1".to_vec()),
            ("a-output.txt".to_owned(), b"second-v1".to_vec()),
            ("copied/a.txt".to_owned(), b"a-v1".to_vec()),
            ("copied/z.txt".to_owned(), b"z-v1".to_vec()),
        ],
        "one mixed order and all bytes come only from the checked snapshot"
    );
}

#[test]
fn in_memory_target_checks_one_exact_closed_byte_inventory() {
    let manifest = r#"
version = 1
name = "memory-package"

[package]
root = "memory"
required_files = ["model.txt", "assets/member.bin"]

[[package.members]]
kind = "file"
file = "model"

[[package.members]]
kind = "asset"
source = "assets"
path = "member.bin"

[[assets]]
source = "assets"
dest = "assets/"

[[files]]
id = "model"
artifact_kind = "text"
semantic_context = "ast"
path = "model.txt"
template = "model.jinja"
"#;
    let target = super::TargetBundle::check_in_memory(
        "memory-input".to_owned(),
        manifest.to_owned(),
        BTreeMap::from([("model.jinja".to_owned(), "snapshotted".to_owned())]),
        BTreeMap::from([(
            "assets".to_owned(),
            BTreeMap::from([("member.bin".to_owned(), b"exact-bytes".to_vec())]),
        )]),
    )
    .expect("exact in-memory target bytes close into one authority");
    let members = checked_package_snapshots(target)
        .into_iter()
        .map(|member| (member.path, member.bytes))
        .collect::<Vec<_>>();
    assert_eq!(
        members,
        vec![
            ("model.txt".to_owned(), b"snapshotted".to_vec()),
            ("assets/member.bin".to_owned(), b"exact-bytes".to_vec()),
        ]
    );
}

#[test]
fn in_memory_target_rejects_missing_foreign_and_over_budget_bytes() {
    let manifest = manifest_with_capabilities_source("[capabilities]");
    for templates in [
        BTreeMap::new(),
        BTreeMap::from([
            ("model.txt.jinja".to_owned(), "model".to_owned()),
            ("foreign.jinja".to_owned(), "foreign".to_owned()),
        ]),
    ] {
        let error = super::TargetBundle::check_in_memory(
            "memory-input".to_owned(),
            manifest.clone(),
            templates,
            BTreeMap::new(),
        )
        .expect_err("in-memory template inventory must be exact");
        assert!(
            format!("{error:#}").contains("template inventory is not exact"),
            "{error:#}"
        );
    }

    let error = super::TargetBundle::check_in_memory(
        "memory-input".to_owned(),
        manifest,
        BTreeMap::from([(
            "model.txt.jinja".to_owned(),
            "x".repeat((super::MAX_TARGET_INPUT_FILE_BYTES + 1) as usize),
        )]),
        BTreeMap::new(),
    )
    .expect_err("in-memory bytes share the bounded snapshot admission path");
    assert!(format!("{error:#}").contains("member bound"), "{error:#}");

    let packaged_with_asset = r#"
version = 1
name = "memory-missing-asset"

[package]
root = "memory"

[[package.members]]
kind = "file"
file = "model"

[[assets]]
source = "assets"
dest = "assets/"

[[files]]
id = "model"
artifact_kind = "text"
semantic_context = "ast"
path = "model.txt"
template = "model.jinja"
"#;
    let error = super::TargetBundle::check_in_memory(
        "memory-input".to_owned(),
        packaged_with_asset.to_owned(),
        BTreeMap::from([("model.jinja".to_owned(), "model".to_owned())]),
        BTreeMap::new(),
    )
    .expect_err("declared in-memory assets require their exact bytes");
    assert!(
        format!("{error:#}").contains("asset inventory is not exact"),
        "{error:#}"
    );
}

#[test]
fn package_plan_maps_one_mixed_sequence_with_file_only_checksum_producers() {
    let directory = tempfile::tempdir().expect("isolated mixed package target");
    std::fs::create_dir(directory.path().join("assets")).expect("create asset directory");
    std::fs::write(directory.path().join("producer.jinja"), "producer")
        .expect("write producer template");
    std::fs::write(directory.path().join("consumer.jinja"), "consumer")
        .expect("write consumer template");
    std::fs::write(directory.path().join("assets/member.bin"), b"asset")
        .expect("write asset member");
    let manifest = r#"
version = 1
name = "mixed-package-map"

[package]
root = "package"
required_files = ["producer.txt", "consumer.txt", "assets/member.bin"]

[[package.members]]
kind = "file"
file = "producer"

[[package.members]]
kind = "asset"
source = "assets"
path = "member.bin"

[[package.members]]
kind = "file"
file = "consumer"

[[assets]]
source = "assets"
dest = "assets/"

[[files]]
id = "consumer"
artifact_kind = "text"
semantic_context = "ast"
path = "consumer.txt"
template = "consumer.jinja"
  [[files.checksums]]
  of = "producer"
  algorithm = "sha1"
  as = "producer_sha1"

[[files]]
id = "producer"
artifact_kind = "text"
semantic_context = "ast"
path = "producer.txt"
template = "producer.jinja"
"#;
    let target = directory_target_bundle(directory.path(), manifest.to_owned())
        .check()
        .expect("mixed package construction follows package order, not file declaration order");
    assert_eq!(
        checked_package_snapshots(target)
            .into_iter()
            .map(|member| (member.path, member.bytes))
            .collect::<Vec<_>>(),
        vec![
            ("producer.txt".to_owned(), b"producer".to_vec()),
            ("assets/member.bin".to_owned(), b"asset".to_vec()),
            ("consumer.txt".to_owned(), b"consumer".to_vec()),
        ]
    );
}

#[test]
fn package_construction_rejects_file_tree_ancestor_collisions_in_both_orders() {
    let directory = tempfile::tempdir().expect("isolated file-tree collision target");
    std::fs::write(directory.path().join("first.jinja"), "first").expect("write template");
    std::fs::write(directory.path().join("second.jinja"), "second").expect("write template");
    for (first, second) in [
        ("node.txt", "node.txt/child.txt"),
        ("NODE.TXT/child.txt", "node.txt"),
    ] {
        let manifest = format!(
            r#"
version = 1
name = "file-tree-collision"

[package]
root = "package"

[[package.members]]
kind = "file"
file = "first"

[[package.members]]
kind = "file"
file = "second"

[[files]]
id = "first"
artifact_kind = "text"
semantic_context = "ast"
path = "{first}"
template = "first.jinja"

[[files]]
id = "second"
artifact_kind = "text"
semantic_context = "ast"
path = "{second}"
template = "second.jinja"
"#
        );
        let error = directory_target_bundle(directory.path(), manifest)
            .check()
            .expect_err("file/ancestor package paths cannot coexist portably");
        assert!(
            format!("{error:#}").contains("one path is an ancestor of the other"),
            "{error:#}"
        );
    }
}

#[test]
fn packaged_paths_are_static_and_modes_reject_during_manifest_construction() {
    let base = r#"
version = 1
name = "static-package-path"

[package]
root = "{{ model_name }}"

[[package.members]]
kind = "file"
file = "member"

[[files]]
id = "member"
artifact_kind = "text"
semantic_context = "ast"
path = "member.txt"
template = "member.jinja"
"#;
    let dynamic = base.replace("member.txt", "{{ model_name }}.txt");
    let error =
        parse_target_manifest(&dynamic).expect_err("packaged file path interpolation rejects");
    assert!(format!("{error:#}").contains("must have one static portable path"));

    let mode = base.replace(
        "template = \"member.jinja\"",
        "template = \"member.jinja\"\nmode = \"0o644\"",
    );
    let error =
        parse_target_manifest(&mode).expect_err("packaged file mode rejects at construction");
    assert!(format!("{error:#}").contains("do not support per-file `mode`"));
}

#[test]
fn checked_efmu_steps_issue_exact_closed_carriers_in_declaration_order() {
    let target = super::TargetBundle::builtin("efmu")
        .expect("registered eFMU target")
        .check()
        .expect("snapshot checked eFMU target");
    let super::CheckedTargetBundle {
        render_authority, ..
    } = target;
    let plan = render_authority.into_plan();
    let super::CheckedTargetRenderPlan::Packaged(plan) = plan else {
        panic!("eFMU must be packaged");
    };
    let super::CheckedTargetPackageProductPlan::SolveAlgorithm(plan) = plan.into_product_plan()
    else {
        panic!("eFMU must carry the Solve Algorithm product");
    };
    let (_, _, _, _, layout, _) = plan.into_parts();
    let steps = layout
        .members()
        .filter_map(|member| match member {
            super::CheckedSolveAlgorithmLayoutMember::CorrelatedAlgorithmCode { path, .. } => {
                Some((path.as_str(), "correlated-ac"))
            }
            super::CheckedSolveAlgorithmLayoutMember::ProductionCode { path, .. } => {
                Some((path.as_str(), "production"))
            }
            super::CheckedSolveAlgorithmLayoutMember::Schema { path } => {
                assert!(path.as_str().starts_with("schemas/"));
                None
            }
        })
        .collect::<Vec<_>>();
    assert_eq!(
        steps.iter().map(|(path, _)| *path).collect::<Vec<_>>(),
        vec![
            "AlgorithmCode/model.alg",
            "AlgorithmCode/manifest.xml",
            "ProductionCode/sources/production.h",
            "ProductionCode/sources/production.c",
            "ProductionCode/manifest.xml",
            "__content.xml",
        ]
    );
    assert_eq!(
        steps.iter().map(|(_, kind)| *kind).collect::<Vec<_>>(),
        vec![
            "correlated-ac",
            "correlated-ac",
            "production",
            "production",
            "production",
            "correlated-ac",
        ]
    );
}

/// End to end, the strict-undefined gate is a physical absence, not a
/// declaration lint: a file that reads a checksum key it declared renders the
/// producer's exact digest, while the same read without the declaration fails
/// the render because the key never entered that file's context.
///
/// This cannot pass vacuously: were the per-file checksum scope replaced by the
/// whole target's checksum map, the undeclared read would resolve and the
/// `expect_err` would fail; were the declared producer's digest not bound, the
/// digest equality would fail.
#[test]
fn strict_undefined_gate_requires_a_declared_checksum_dependency() {
    let templates = BTreeMap::from([
        ("producer.jinja".to_owned(), "producer-body".to_owned()),
        (
            "consumer.jinja".to_owned(),
            "{{ artifact.checksums.prod_digest }}".to_owned(),
        ),
    ]);

    let declared = r#"
version = 1
name = "checksum-declared"
[[files]]
artifact_kind = "text"
semantic_context = "ast"
path = "producer.txt"
template = "producer.jinja"
id = "prod"
[[files]]
artifact_kind = "text"
semantic_context = "ast"
path = "consumer.txt"
template = "consumer.jinja"
checksums = [{ of = "prod", algorithm = "sha1", as = "prod_digest" }]
"#;
    let bundle = TargetBundle::check_in_memory(
        "checksum-declared".to_owned(),
        declared.to_owned(),
        templates.clone(),
        BTreeMap::new(),
    )
    .expect("a declared checksum dependency constructs");
    let files = strict_test_compilation()
        .render_target(bundle, test_artifact_input())
        .expect("the declared checksum dependency renders")
        .into_rendered_files();
    let producer = files
        .iter()
        .find(|file| file.path() == "producer.txt")
        .expect("the producer is rendered");
    let consumer = files
        .iter()
        .find(|file| file.path() == "consumer.txt")
        .expect("the consumer is rendered");
    assert_eq!(
        consumer.content(),
        super::sha1_hex(producer.content().as_bytes()),
        "the declared consumer renders the producer's exact SHA-1 digest"
    );

    let undeclared = r#"
version = 1
name = "checksum-undeclared"
[[files]]
artifact_kind = "text"
semantic_context = "ast"
path = "producer.txt"
template = "producer.jinja"
id = "prod"
[[files]]
artifact_kind = "text"
semantic_context = "ast"
path = "consumer.txt"
template = "consumer.jinja"
"#;
    let bundle = TargetBundle::check_in_memory(
        "checksum-undeclared".to_owned(),
        undeclared.to_owned(),
        templates,
        BTreeMap::new(),
    )
    .expect("an undeclared checksum read constructs; the gate is at render");
    let error = strict_test_compilation()
        .render_target(bundle, test_artifact_input())
        .map(|_| ())
        .expect_err("an undeclared checksum read fails the render");
    let chain = format!("{error:#}").to_lowercase();
    assert!(
        chain.contains("undefined"),
        "the undeclared checksum read must fail with undefined in the chain: {chain}"
    );
}

mod filesystem_cases;
