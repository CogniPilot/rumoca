//! Render-coverage CI for every built-in code-gen target (SPEC_0034 GAL-012:
//! real fixtures, never skip-and-mark-covered).
//!
//! Each target's `[[files]]` render through [`rumoca::render_target_files`] —
//! the in-memory twin of `compile --target` — so CI exercises the exact CLI
//! path: capability validation plus the name-dispatched renderers
//! (`wgsl-solve`, `galec`, `embedded-c-galec`) that the generic DAE-JSON
//! template context cannot reach. Targets that declare
//! `continuous_states = false` (the GALEC-derived targets) render against a
//! dedicated fixed-sample discrete fixture; every other target keeps the
//! continuous fixture. No target is skipped.

use std::collections::BTreeSet;
use std::fs;
use std::path::PathBuf;

use quick_xml::Reader;
use quick_xml::events::Event;
use rumoca::{CompilationResult, Compiler, TemplateIr, render_target_files};
use rumoca_compile::codegen::targets::{
    RenderedTargetFile, TargetManifest, TargetTemplateIr, parse_target_manifest,
};
use rumoca_phase_codegen::templates;

const SMOKE_MODEL: &str = "Smoke";
const SMOKE_SOURCE: &str = r#"
model Smoke
  Real x(start = 1);
  parameter Real k = 2;
equation
  der(x) = -k * x;
end Smoke;
"#;

/// Fixed-sample discrete fixture for targets that reject continuous states:
/// a parameter, a `pre()` state, an output, and one `when sample(...)` clock.
const DISCRETE_SMOKE_MODEL: &str = "DiscreteSmoke";
const DISCRETE_SMOKE_SOURCE: &str = r#"
model DiscreteSmoke
  constant Real samplePeriod = 0.1;
  parameter Real gain = 2.0;
  discrete output Real y(start = 0.0);
equation
  when sample(0.0, samplePeriod) then
    y = gain * (pre(y) + 1.0);
  end when;
end DiscreteSmoke;
"#;

fn template_ir(ir: TargetTemplateIr) -> TemplateIr {
    match ir {
        TargetTemplateIr::Dae => TemplateIr::Dae,
        TargetTemplateIr::Solve => TemplateIr::Solve,
        TargetTemplateIr::Flat => TemplateIr::Flat,
        TargetTemplateIr::Ast => TemplateIr::Ast,
    }
}

/// A compiled smoke model plus the name the CLI would render it under.
struct Fixture {
    model_name: &'static str,
    compiled: CompilationResult,
}

fn compile_fixture(model_name: &'static str, source: &str) -> Fixture {
    let compiled = Compiler::new()
        .model(model_name)
        .compile_str(source, &format!("{model_name}.mo"))
        .unwrap_or_else(|err| panic!("compile template target fixture {model_name}: {err}"));
    Fixture {
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
            continuous: compile_fixture(SMOKE_MODEL, SMOKE_SOURCE),
            discrete: compile_fixture(DISCRETE_SMOKE_MODEL, DISCRETE_SMOKE_SOURCE),
        }
    }

    /// A target that declares it cannot take continuous states renders
    /// against the discrete fixture; everything else keeps the continuous
    /// one. Driven by the manifest capability, not by target name, so new
    /// discrete-only targets are routed automatically.
    fn for_manifest(&self, manifest: &TargetManifest) -> &Fixture {
        let rejects_continuous = manifest
            .capabilities
            .as_ref()
            .is_some_and(|capabilities| capabilities.continuous_states == Some(false));
        if rejects_continuous {
            &self.discrete
        } else {
            &self.continuous
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
        .filter_map(Result::ok)
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
fn builtin_template_targets_render_or_are_explicit_readiness_zero_manifests() {
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

    assert!(
        coverage.rendered_targets.contains(&"embedded-c"),
        "embedded-c must be covered by target render CI"
    );
    assert!(
        coverage.rendered_targets.contains(&"fmi2") && coverage.rendered_targets.contains(&"fmi3"),
        "FMI targets must be covered by target render CI"
    );
    assert!(
        coverage.rendered_targets.contains(&"galec"),
        "galec must be covered by target render CI (GAL-012)"
    );
    assert_eq!(
        coverage.manifest_only_targets,
        vec!["cranelift-solve-jit", "cuda-nvrtc-solve-jit"],
        "readiness-0 manifest-only target placeholders must be explicitly accounted for"
    );
    assert!(
        coverage
            .support_templates
            .contains(&"fmi2:test_driver.c.jinja".to_string())
            && coverage
                .support_templates
                .contains(&"fmi3:test_driver.c.jinja".to_string()),
        "support templates that are not emitted as target files must still render in CI"
    );
}

/// The galec target renders a non-empty `<Model>.alg` (typed-printer output
/// with the mandatory block methods) and a well-formed Algorithm Code
/// `manifest.xml` for the discrete fixture, through the real CLI path.
#[test]
fn galec_target_renders_alg_and_wellformed_manifest_for_discrete_fixture() {
    let fixture = compile_fixture(DISCRETE_SMOKE_MODEL, DISCRETE_SMOKE_SOURCE);
    let files = render_target_files(&fixture.compiled, fixture.model_name, "galec", None)
        .expect("galec target should render the discrete smoke fixture");

    // The galec target renders the eFMU AlgorithmCode/ container layout plus
    // the root `__content.xml` registry through the declarative checksum web
    // (contract §9 WI-5).
    let alg = find_rendered_file(&files, &format!("AlgorithmCode/{DISCRETE_SMOKE_MODEL}.alg"));
    assert!(
        alg.content.contains("method DoStep"),
        "galec .alg output must contain the DoStep method:\n{}",
        alg.content
    );

    let manifest = find_rendered_file(&files, "AlgorithmCode/manifest.xml");
    assert!(
        !manifest.content.trim().is_empty(),
        "galec manifest.xml must not be empty"
    );
    let root = assert_well_formed_xml(&manifest.content);
    assert_eq!(
        root, "Manifest",
        "Algorithm Code manifest root element must be <Manifest>"
    );

    // The web-injected representation checksum flows into `__content.xml`: it
    // is the SHA-1 of the exact rendered manifest bytes (GAL-021, no placeholder).
    let content = find_rendered_file(&files, "__content.xml");
    let manifest_sha1 = rumoca_galec_codegen::Sha1Hex::of_bytes(manifest.content.as_bytes());
    assert!(
        content
            .content
            .contains(&format!("checksum=\"{}\"", manifest_sha1.as_str())),
        "__content.xml must carry the SHA-1 of the rendered manifest.xml:\n{}",
        content.content
    );
}

/// The generic capability gate (GAL-006) rejects a continuous model on the
/// same real render path — the discrete-fixture routing above must never
/// paper over that gate.
#[test]
fn galec_target_rejects_continuous_fixture_via_capability_gate() {
    let fixture = compile_fixture(SMOKE_MODEL, SMOKE_SOURCE);
    let error = render_target_files(&fixture.compiled, fixture.model_name, "galec", None)
        .expect_err("galec must reject the continuous smoke fixture");
    let message = format!("{error:#}");
    assert!(
        message.contains("unsupported-feature:continuous_states"),
        "expected the generic continuous_states capability diagnostic, got: {message}"
    );
}

fn find_rendered_file<'a>(files: &'a [RenderedTargetFile], path: &str) -> &'a RenderedTargetFile {
    files
        .iter()
        .find(|file| file.path == path)
        .unwrap_or_else(|| {
            let paths = files
                .iter()
                .map(|file| file.path.as_str())
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

struct TemplateTargetCoverage {
    rendered_targets: Vec<&'static str>,
    manifest_only_targets: Vec<&'static str>,
    support_templates: Vec<String>,
}

fn render_builtin_template_targets(fixtures: &Fixtures) -> TemplateTargetCoverage {
    let mut coverage = TemplateTargetCoverage {
        rendered_targets: Vec::new(),
        manifest_only_targets: Vec::new(),
        support_templates: Vec::new(),
    };
    for target in templates::builtin_targets() {
        render_builtin_template_target(fixtures, target, &mut coverage);
    }
    coverage
}

fn render_builtin_template_target(
    fixtures: &Fixtures,
    target: &'static templates::BuiltinTarget,
    coverage: &mut TemplateTargetCoverage,
) {
    let manifest = parse_target_manifest(target.manifest)
        .unwrap_or_else(|err| panic!("target {} manifest should parse: {err}", target.name));
    assert_target_manifest_metadata(target, &manifest);
    if manifest.files.is_empty() {
        assert_manifest_only_target(target, &manifest);
        coverage.manifest_only_targets.push(target.name);
        return;
    }
    let fixture = fixtures.for_manifest(&manifest);
    render_manifest_target_files(fixture, target, &manifest);
    render_support_templates(fixture, target, &manifest, &mut coverage.support_templates);
    coverage.rendered_targets.push(target.name);
}

fn assert_target_manifest_metadata(target: &templates::BuiltinTarget, manifest: &TargetManifest) {
    assert_eq!(manifest.name.as_deref(), Some(target.name));
    if matches!(target.name, "embedded-c" | "fmi2" | "fmi3") {
        assert_eq!(manifest.ir, TargetTemplateIr::Solve);
    }
}

fn assert_manifest_only_target(target: &templates::BuiltinTarget, manifest: &TargetManifest) {
    assert_eq!(manifest.readiness_level, Some(0));
    assert!(
        target.templates.is_empty(),
        "manifest-only readiness-0 target {} must not contain unrendered template files",
        target.name
    );
}

/// Render every `[[files]]` entry through the real CLI path (capability
/// validation, path templates, name-dispatched renderers) and assert each
/// rendered file is non-empty.
fn render_manifest_target_files(
    fixture: &Fixture,
    target: &'static templates::BuiltinTarget,
    manifest: &TargetManifest,
) {
    let files = render_target_files(&fixture.compiled, fixture.model_name, target.name, None)
        .unwrap_or_else(|err| {
            panic!(
                "target {} must render against the {} fixture: {err:#}",
                target.name, fixture.model_name
            )
        });
    assert_eq!(
        files.len(),
        manifest.files.len(),
        "target {} rendered a different file count than its manifest declares",
        target.name
    );
    for file in &files {
        assert!(
            !file.path.is_empty(),
            "target {} rendered an empty output path",
            target.name
        );
        assert!(
            !file.content.trim().is_empty(),
            "target {} rendered empty content for {}",
            target.name,
            file.path
        );
    }
}

fn render_support_templates(
    fixture: &Fixture,
    target: &'static templates::BuiltinTarget,
    manifest: &TargetManifest,
    support_templates: &mut Vec<String>,
) {
    let ir = template_ir(manifest.ir);
    let manifest_templates = manifest
        .files
        .iter()
        .map(|file| file.template.as_str())
        .collect::<BTreeSet<_>>();
    for template in target.templates {
        if manifest_templates.contains(template.path) {
            continue;
        }
        assert_rendered_support_template(fixture, target.name, template, ir);
        support_templates.push(format!("{}:{}", target.name, template.path));
    }
}

fn assert_rendered_support_template(
    fixture: &Fixture,
    target_name: &str,
    template: &templates::BuiltinTargetTemplate,
    ir: TemplateIr,
) {
    let content = fixture
        .compiled
        .render_template_str_with_name_and_ir(template.source, fixture.model_name, ir)
        .unwrap_or_else(|err| {
            panic!(
                "render support template {}:{}: {err}",
                target_name, template.path
            )
        });
    assert!(
        !content.trim().is_empty(),
        "target {} rendered empty support template {}",
        target_name,
        template.path
    );
}
