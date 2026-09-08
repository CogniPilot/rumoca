//! Render-coverage CI for every built-in code-gen target (SPEC_0034 GAL-012:
//! real fixtures, never skip-and-mark-covered).
//!
//! Each target's `[[files]]` render through [`rumoca::render_target_files`] —
//! the in-memory twin of `compile --target` — so CI exercises the exact CLI
//! path: construction-issued product dispatch, capability validation, and the
//! exact checked context/view declared by each file. Every target first sees
//! the continuous fixture. A target that explicitly refuses continuous states
//! records that typed refusal before its declared outputs are rendered from a
//! dedicated fixed-sample discrete fixture. No checked target descriptor or
//! declared output is skipped, and a discrete render is never evidence that
//! the continuous fixture rendered.

use std::collections::BTreeSet;
use std::fs;
use std::path::PathBuf;

use miette::Diagnostic as _;
use quick_xml::Reader;
use quick_xml::events::Event;
use rumoca::{CompilationResult, Compiler, render_target_files};
use rumoca_compile::codegen::targets::{
    BuiltinTargetDescriptor, CompletedRenderedFile, builtin_target_descriptors,
};
use rumoca_core::Span;
use rumoca_phase_codegen::CodegenError;
use rumoca_phase_codegen::templates;
use sha1::{Digest as _, Sha1};

use crate::artifact_session::pinned_artifact_input;

const SMOKE_MODEL: &str = "Smoke";
const SMOKE_SOURCE: &str = r#"
model Smoke
  Real x(start = 1, fixed = true);
  parameter Real k = 2;
equation
  der(x) = -k * x;
end Smoke;
"#;

/// Fixed-sample discrete fixture for targets that reject continuous states:
/// an output updated by one `when sample(...)` clock.
const DISCRETE_SMOKE_MODEL: &str = "DiscreteSmoke";
const DISCRETE_SMOKE_SOURCE: &str = r#"
model DiscreteSmoke
  constant Real samplePeriod = 0.1;
  discrete output Real y(start = 0.0);
equation
  when sample(0.0, samplePeriod) then
    y = 1.25;
  end when;
end DiscreteSmoke;
"#;

/// A compiled smoke model plus the name the CLI would render it under.
struct Fixture {
    identity: FixtureIdentity,
    model_name: &'static str,
    compiled: CompilationResult,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FixtureIdentity {
    ContinuousSmoke,
    DiscreteSmoke,
}

fn compile_fixture(identity: FixtureIdentity, model_name: &'static str, source: &str) -> Fixture {
    let compiled = Compiler::new()
        .model(model_name)
        .compile_str(source, &format!("{model_name}.mo"))
        .unwrap_or_else(|err| panic!("compile template target fixture {model_name}: {err}"));
    Fixture {
        identity,
        model_name,
        compiled,
    }
}

/// Both render fixtures, compiled once for the whole target sweep.
struct Fixtures {
    continuous: Fixture,
    discrete: Fixture,
}

impl Fixtures {
    fn compile() -> Self {
        Self {
            continuous: compile_fixture(
                FixtureIdentity::ContinuousSmoke,
                SMOKE_MODEL,
                SMOKE_SOURCE,
            ),
            discrete: compile_fixture(
                FixtureIdentity::DiscreteSmoke,
                DISCRETE_SMOKE_MODEL,
                DISCRETE_SMOKE_SOURCE,
            ),
        }
    }
}

fn codegen_template_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../crates/rumoca-phase-codegen/src/templates")
}

fn discovered_codegen_template_dirs() -> BTreeSet<String> {
    fs::read_dir(codegen_template_root())
        .expect("read codegen template root")
        .map(|entry| entry.expect("read codegen template directory entry"))
        .map(|entry| entry.path())
        .filter(|path| path.is_dir())
        .map(|path| {
            path.file_name()
                .expect("template directory should have a name")
                .to_string_lossy()
                .to_string()
        })
        .collect()
}

#[test]
fn every_builtin_template_target_renders_every_declared_artifact() {
    let fixtures = Fixtures::compile();
    let builtin_names = templates::builtin_targets()
        .iter()
        .map(|target| target.name.to_string())
        .collect::<BTreeSet<_>>();

    assert_eq!(
        discovered_codegen_template_dirs(),
        builtin_names,
        "every codegen template directory must be registered as a built-in target"
    );

    let coverage = render_builtin_template_targets(&fixtures);

    assert_eq!(
        coverage.keys().cloned().collect::<BTreeSet<_>>(),
        builtin_names,
        "every checked built-in target descriptor must have an exact fixture outcome"
    );

    assert_continuous_refusal_and_discrete_render(
        coverage.get("galec").expect("galec target coverage"),
        "galec",
    );
    assert_continuous_refusal_and_discrete_render(
        coverage.get("efmu").expect("efmu target coverage"),
        "efmu",
    );
    assert_continuous_render(coverage.get("fmi2").expect("fmi2 target coverage"), "fmi2");
    assert_continuous_render(coverage.get("fmi3").expect("fmi3 target coverage"), "fmi3");
}

#[test]
fn removed_targets_are_rejected_before_rendering() {
    let fixture = compile_fixture(FixtureIdentity::ContinuousSmoke, SMOKE_MODEL, SMOKE_SOURCE);
    for target in [
        "base-modelica",
        "casadi-mx",
        "casadi-solve",
        "casadi-sx",
        "c-solve",
        "cranelift-solve-jit",
        "cuda-c",
        "cuda-nvrtc-solve-jit",
        "flat-modelica",
        "jax",
        "jax-solve",
        "julia-mtk",
        "modelica",
        "onnx",
        "rust-fixed-solve",
        "rust-solve",
        "symforce",
        "sympy",
        "wgsl-rhs",
        "wgsl-solve",
    ] {
        let error = render_target_files(&fixture.compiled, target, pinned_artifact_input())
            .expect_err("removed target must not render");
        let message = format!("{error:#}");
        assert!(
            message.to_ascii_lowercase().contains("unknown target"),
            "removed target `{target}` did not fail as an unknown target: {message}"
        );
    }
}

#[test]
fn fmi_targets_render_the_checked_continuous_fixture() {
    let fixture = compile_fixture(FixtureIdentity::ContinuousSmoke, SMOKE_MODEL, SMOKE_SOURCE);
    for target in ["fmi2", "fmi3"] {
        let files = render_target_files(&fixture.compiled, target, pinned_artifact_input())
            .unwrap_or_else(|error| panic!("{target} must render: {error:#}"));
        assert!(
            files
                .iter()
                .any(|file| file.path() == "modelDescription.xml")
        );
    }
}

/// The galec target renders a non-empty `<Model>.alg` (typed-printer output
/// with the mandatory block methods) and a well-formed Algorithm Code
/// `manifest.xml` for the discrete fixture, through the real CLI path.
#[test]
fn galec_target_renders_alg_and_wellformed_manifest_for_discrete_fixture() {
    let fixture = compile_fixture(
        FixtureIdentity::DiscreteSmoke,
        DISCRETE_SMOKE_MODEL,
        DISCRETE_SMOKE_SOURCE,
    );
    let files = render_target_files(&fixture.compiled, "galec", pinned_artifact_input())
        .expect("galec target should render the discrete smoke fixture");

    // The galec target renders the eFMU AlgorithmCode/ container layout plus
    // the root `__content.xml` registry through the declarative checksum web
    // (contract §9 WI-5).
    let alg = find_rendered_file(&files, "AlgorithmCode/model.alg");
    assert!(
        alg.content().contains("method DoStep"),
        "galec .alg output must contain the DoStep method:\n{}",
        alg.content()
    );

    let manifest = find_rendered_file(&files, "AlgorithmCode/manifest.xml");
    assert!(
        !manifest.content().trim().is_empty(),
        "galec manifest.xml must not be empty"
    );
    let root = assert_well_formed_xml(manifest.content());
    assert_eq!(
        root, "Manifest",
        "Algorithm Code manifest root element must be <Manifest>"
    );

    // The web-injected representation checksum flows into `__content.xml`: it
    // is the SHA-1 of the exact rendered manifest bytes (GAL-021, no placeholder).
    let content = find_rendered_file(&files, "__content.xml");
    let manifest_sha1 = format!("{:x}", Sha1::digest(manifest.content().as_bytes()));
    assert!(
        content
            .content()
            .contains(&format!("checksum=\"{manifest_sha1}\"")),
        "__content.xml must carry the SHA-1 of the rendered manifest.xml:\n{}",
        content.content()
    );
}

/// The generic capability gate (GAL-006) rejects a continuous model on the
/// same real render path — the discrete-fixture routing above must never
/// paper over that gate.
#[test]
fn galec_target_rejects_continuous_fixture_via_capability_gate() {
    let fixture = compile_fixture(FixtureIdentity::ContinuousSmoke, SMOKE_MODEL, SMOKE_SOURCE);
    let error = render_target_files(&fixture.compiled, "galec", pinned_artifact_input())
        .expect_err("galec must reject the continuous smoke fixture");
    let message = format!("{error:#}");
    assert!(
        message.contains("unsupported-feature:continuous_states"),
        "expected the generic continuous_states capability diagnostic, got: {message}"
    );
}

fn find_rendered_file<'a>(
    files: &'a [CompletedRenderedFile],
    path: &str,
) -> &'a CompletedRenderedFile {
    files
        .iter()
        .find(|file| file.path() == path)
        .unwrap_or_else(|| {
            let paths = files
                .iter()
                .map(CompletedRenderedFile::path)
                .collect::<Vec<_>>();
            panic!("expected rendered file '{path}', got {paths:?}")
        })
}

/// Full event-scan well-formedness check; returns the root element name.
fn assert_well_formed_xml(xml: &str) -> String {
    let mut reader = Reader::from_str(xml);
    let mut root = None;
    loop {
        match reader.read_event() {
            Ok(Event::Eof) => break,
            Ok(Event::Start(element) | Event::Empty(element)) => {
                if root.is_none() {
                    root = Some(String::from_utf8_lossy(element.name().as_ref()).into_owned());
                }
            }
            Ok(_) => {}
            Err(err) => panic!("not well-formed XML: {err}\n{xml}"),
        }
    }
    root.expect("XML document has no root element")
}

#[derive(Debug, PartialEq, Eq)]
enum TargetFixtureOutcome {
    Rendered {
        fixture: FixtureIdentity,
    },
    RefusedByCapability {
        fixture: FixtureIdentity,
        feature: &'static str,
        code: String,
        span: Span,
    },
}

#[derive(Debug, PartialEq, Eq)]
struct TemplateTargetCoverage {
    original: TargetFixtureOutcome,
    capability_compatible_render: Option<TargetFixtureOutcome>,
}

fn render_builtin_template_targets(
    fixtures: &Fixtures,
) -> std::collections::BTreeMap<String, TemplateTargetCoverage> {
    let mut coverage = std::collections::BTreeMap::new();
    let descriptors = builtin_target_descriptors().expect("check every built-in target bundle");
    for descriptor in &descriptors {
        let previous = coverage.insert(
            descriptor.id.clone(),
            render_builtin_template_target(fixtures, descriptor),
        );
        assert!(
            previous.is_none(),
            "built-in target {} was covered more than once",
            descriptor.id
        );
    }
    coverage
}

fn render_builtin_template_target(
    fixtures: &Fixtures,
    descriptor: &BuiltinTargetDescriptor,
) -> TemplateTargetCoverage {
    assert_target_descriptor(descriptor);
    let rejects_continuous = descriptor
        .capabilities
        .as_ref()
        .is_some_and(|capabilities| capabilities.continuous_states == Some(false));

    match render_target_descriptor_files(&fixtures.continuous, descriptor) {
        Ok(()) if !rejects_continuous => TemplateTargetCoverage {
            original: TargetFixtureOutcome::Rendered {
                fixture: fixtures.continuous.identity,
            },
            capability_compatible_render: None,
        },
        Ok(()) => panic!(
            "target {} rendered continuous fixture despite continuous_states = false",
            descriptor.id
        ),
        Err(error) if rejects_continuous => {
            let refusal = capability_refusal_outcome(&fixtures.continuous, descriptor, &error);
            render_target_descriptor_files(&fixtures.discrete, descriptor).unwrap_or_else(|err| {
                panic!(
                    "target {} refused its capability-compatible {} fixture: {err:#}",
                    descriptor.id, fixtures.discrete.model_name
                )
            });
            TemplateTargetCoverage {
                original: refusal,
                capability_compatible_render: Some(TargetFixtureOutcome::Rendered {
                    fixture: fixtures.discrete.identity,
                }),
            }
        }
        Err(error) => panic!(
            "target {} unexpectedly refused the continuous fixture: {error:#}",
            descriptor.id
        ),
    }
}

fn capability_refusal_outcome(
    fixture: &Fixture,
    descriptor: &BuiltinTargetDescriptor,
    error: &anyhow::Error,
) -> TargetFixtureOutcome {
    let typed = error.downcast_ref::<CodegenError>().unwrap_or_else(|| {
        panic!(
            "target {} capability refusal lost CodegenError: {error:#}",
            descriptor.id
        )
    });
    let code = typed
        .code()
        .map(|code| code.to_string())
        .expect("target capability refusal must retain its diagnostic code");
    let CodegenError::UnsupportedTargetFeature {
        target,
        feature,
        span: Some(span),
        ..
    } = typed
    else {
        panic!(
            "target {} did not return an exact-span capability refusal: {error:#}",
            descriptor.id
        );
    };
    assert_eq!(target, &descriptor.id);
    assert_eq!(*feature, "continuous_states");
    assert_eq!(code, "rumoca::codegen::EC009");
    assert!(!span.is_dummy(), "EC009 must retain the state source span");
    TargetFixtureOutcome::RefusedByCapability {
        fixture: fixture.identity,
        feature,
        code,
        span: *span,
    }
}

fn assert_continuous_refusal_and_discrete_render(coverage: &TemplateTargetCoverage, target: &str) {
    match &coverage.original {
        TargetFixtureOutcome::RefusedByCapability {
            fixture,
            feature,
            code,
            span,
        } => {
            assert_eq!(*fixture, FixtureIdentity::ContinuousSmoke);
            assert_eq!(*feature, "continuous_states");
            assert_eq!(code, "rumoca::codegen::EC009");
            assert!(
                !span.is_dummy(),
                "{target} refusal must retain a source span"
            );
        }
        outcome => panic!("{target} must refuse the continuous fixture, got {outcome:?}"),
    }
    assert_eq!(
        coverage.capability_compatible_render,
        Some(TargetFixtureOutcome::Rendered {
            fixture: FixtureIdentity::DiscreteSmoke,
        }),
        "{target} discrete render must stay distinct from its continuous refusal"
    );
}

fn assert_continuous_render(coverage: &TemplateTargetCoverage, target: &str) {
    assert_eq!(
        coverage.original,
        TargetFixtureOutcome::Rendered {
            fixture: FixtureIdentity::ContinuousSmoke,
        },
        "{target} must render the original continuous fixture"
    );
    assert_eq!(
        coverage.capability_compatible_render, None,
        "{target} must not substitute a second fixture"
    );
}

fn assert_target_descriptor(descriptor: &BuiltinTargetDescriptor) {
    assert!(
        !descriptor.file_plans.is_empty(),
        "built-in target {} must declare at least one checked artifact",
        descriptor.id
    );
    for file in &descriptor.file_plans {
        assert_eq!(
            file.semantic_view.semantic_context(),
            file.semantic_context,
            "target {} file {} escaped its checked IR owner",
            descriptor.id,
            file.path
        );
    }
}

/// Render every `[[files]]` entry through the real CLI path (capability
/// validation, checked-product dispatch, and path templates) and assert each
/// rendered file is non-empty.
fn render_target_descriptor_files(
    fixture: &Fixture,
    descriptor: &BuiltinTargetDescriptor,
) -> anyhow::Result<()> {
    let files = render_target_files(&fixture.compiled, &descriptor.id, pinned_artifact_input())?;
    assert_eq!(
        files.len(),
        descriptor.file_plans.len(),
        "target {} rendered a different file count than its checked descriptor declares",
        descriptor.id
    );
    for file in &files {
        assert!(
            !file.path().is_empty(),
            "target {} rendered an empty output path",
            descriptor.id
        );
        assert!(
            !file.content().trim().is_empty(),
            "target {} rendered empty content for {}",
            descriptor.id,
            file.path()
        );
    }
    Ok(())
}

/// Copy a built-in target directory into a scratch directory so the external
/// (directory) target path can be exercised against a real bundle.
fn copy_builtin_target_dir(target: &str, into: &std::path::Path) -> PathBuf {
    let source = codegen_template_root().join(target);
    let dest = into.join(target);
    fs::create_dir_all(&dest).expect("create scratch target directory");
    for entry in fs::read_dir(&source).expect("read built-in target directory") {
        let entry = entry.expect("read built-in target entry");
        if entry.file_type().expect("stat entry").is_file() {
            fs::copy(entry.path(), dest.join(entry.file_name())).expect("copy target file");
        }
    }
    dest
}

/// A verbatim copy of a target directory keeps rendering with the same checked
/// file plans and bytes.
#[test]
fn copied_target_directory_still_renders_identically() {
    let fixture = compile_fixture(FixtureIdentity::ContinuousSmoke, SMOKE_MODEL, SMOKE_SOURCE);
    let scratch = tempfile::tempdir().expect("scratch dir");
    let dir = copy_builtin_target_dir("dae-modelica", scratch.path());

    let files = render_target_files(
        &fixture.compiled,
        dir.to_str().expect("utf-8 scratch path"),
        pinned_artifact_input(),
    )
    .expect("verbatim copy of a built-in target must render");
    let builtin = render_target_files(&fixture.compiled, "dae-modelica", pinned_artifact_input())
        .expect("built-in target must render");
    assert_eq!(
        files
            .iter()
            .map(|file| (file.path(), file.content()))
            .collect::<Vec<_>>(),
        builtin
            .iter()
            .map(|file| (file.path(), file.content()))
            .collect::<Vec<_>>(),
    );
}
