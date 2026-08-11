//! Render-coverage CI for every built-in code-gen target (SPEC_0034 GAL-012:
//! real fixtures, never skip-and-mark-covered).
//!
//! Each target's `[[files]]` render through [`rumoca::render_target_files`] —
//! the in-memory twin of `compile --target` — so CI exercises the exact CLI
//! path: capability validation plus the name-dispatched renderers
//! (`wgsl-ode`, `galec`, `embedded-c-galec`) that the generic DAE-JSON
//! template context cannot reach. Targets that declare
//! `continuous_states = false` (the GALEC-derived targets) render against a
//! dedicated fixed-sample discrete fixture; every other target keeps the
//! continuous fixture. No target is skipped.
//!
//! # Support partials
//!
//! Not every bundled template renders a product file. A **support partial**
//! (`[[partials]]` in `target.toml`) renders none by definition: it exists to
//! be `import`ed, `include`d, or `extends`ed, and rendering it standalone
//! yields nothing. "Every bundled template must be a `[[files]]` entry" is
//! therefore the wrong invariant; the right one, checked here, is that every
//! bundled template is declared exactly once — as a `[[files]]` artifact or as
//! a `[[partials]]` support partial — and that the two sets are disjoint. That
//! keeps render coverage total (no template goes unclassified) without a
//! per-file carve-out.

use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::PathBuf;

use quick_xml::Reader;
use quick_xml::events::Event;
use rumoca::{CompilationResult, Compiler, render_target_files};
use rumoca_compile::codegen::targets::{
    RenderedTargetFile, TargetManifest, TargetTemplateIr, parse_target_manifest,
};
use rumoca_phase_codegen::templates;
use rumoca_phase_codegen::templates::BuiltinTemplateRole;

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
        coverage.rendered_targets.contains(&"c-ode"),
        "c-ode must be covered by target render CI"
    );
    assert!(
        coverage.rendered_targets.contains(&"galec"),
        "galec must be covered by target render CI (GAL-012)"
    );
    assert!(coverage.rendered_targets.contains(&"fmi2"));
    assert!(coverage.rendered_targets.contains(&"fmi3"));
    assert!(
        coverage.manifest_only_targets.is_empty(),
        "built-in targets must emit artifacts, found manifest-only targets: {:?}",
        coverage.manifest_only_targets
    );
    // Support partials are the only bundled templates the sweep does not
    // render as a product file, and they are exempt because a manifest
    // declares them so — not because CI skips them.
    assert_eq!(
        coverage.support_partials,
        vec!["embedded-c-galec:symbols.jinja".to_string()],
        "the declared support partials changed"
    );
}

#[test]
fn removed_targets_are_rejected_before_rendering() {
    let fixture = compile_fixture(SMOKE_MODEL, SMOKE_SOURCE);
    for target in [
        "casadi-mx",
        "casadi-solve",
        "casadi-sx",
        "c-solve",
        "cranelift-solve-jit",
        "cuda-c",
        "cuda-nvrtc-solve-jit",
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
        let error = render_target_files(&fixture.compiled, fixture.model_name, target, None)
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
    let fixture = compile_fixture(SMOKE_MODEL, SMOKE_SOURCE);
    for target in ["fmi2", "fmi3"] {
        let files = render_target_files(&fixture.compiled, fixture.model_name, target, None)
            .unwrap_or_else(|error| panic!("{target} must render: {error:#}"));
        assert!(files.iter().any(|file| file.path == "modelDescription.xml"));
    }
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
    let manifest_sha1 = rumoca::sha1_hex(manifest.content.as_bytes());
    assert!(
        content
            .content
            .contains(&format!("checksum=\"{manifest_sha1}\"")),
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
    /// `target:path` of every declared support partial seen in the sweep.
    support_partials: Vec<String>,
}

fn render_builtin_template_targets(fixtures: &Fixtures) -> TemplateTargetCoverage {
    let mut coverage = TemplateTargetCoverage {
        rendered_targets: Vec::new(),
        manifest_only_targets: Vec::new(),
        support_partials: Vec::new(),
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
    assert_template_declarations(target, &manifest, &mut coverage.support_partials);
    render_manifest_target_files(fixture, target, &manifest);
    coverage.rendered_targets.push(target.name);
}

fn assert_target_manifest_metadata(target: &templates::BuiltinTarget, manifest: &TargetManifest) {
    assert_eq!(manifest.name.as_deref(), Some(target.name));
    assert!(
        manifest.readiness_level.is_some(),
        "target {} must declare readiness_level explicitly",
        target.name
    );
    if target.name == "c-ode" {
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

/// Every bundled template is declared exactly once, and the two declarations
/// are disjoint: a `[[files]]` artifact renders one product file, a
/// `[[partials]]` support partial renders none.
///
/// A support partial must NOT appear in `[[files]]` — that is what makes it a
/// partial — so this is the check that replaces "every bundled template is a
/// `[[files]]` entry". It is total: an undeclared template fails, a
/// double-declared template fails, and a declared-but-unbundled template
/// fails, for every target and every IR alike.
fn assert_template_declarations(
    target: &'static templates::BuiltinTarget,
    manifest: &TargetManifest,
    support_partials: &mut Vec<String>,
) {
    let artifacts = manifest
        .files
        .iter()
        .map(|file| file.template.as_str())
        .collect::<BTreeSet<_>>();
    let partials = manifest
        .partials
        .iter()
        .map(|partial| (partial.template.as_str(), partial.name.as_str()))
        .collect::<BTreeMap<_, _>>();

    for template in target.templates {
        match template.role {
            BuiltinTemplateRole::Artifact => assert!(
                artifacts.contains(template.path),
                "target {} bundles {} as an artifact, but no [[files]] entry renders it",
                target.name,
                template.path
            ),
            BuiltinTemplateRole::SupportPartial => {
                let shared_name = partials.get(template.path).unwrap_or_else(|| {
                    panic!(
                        "target {} bundles support partial {} without a [[partials]] \
                         declaration",
                        target.name, template.path
                    )
                });
                assert!(
                    !artifacts.contains(template.path),
                    "support partial {}/{} must not also be a [[files]] entry: it renders \
                     no product file",
                    target.name,
                    template.path
                );
                assert_eq!(
                    template.shared_name,
                    Some(*shared_name),
                    "support partial {}/{} must be published under its declared name",
                    target.name,
                    template.path
                );
                support_partials.push(format!("{}:{}", target.name, template.path));
            }
        }
    }

    for template in artifacts.iter().chain(partials.keys()) {
        assert!(
            target
                .templates
                .iter()
                .any(|bundled| &bundled.path == template),
            "target {} declares {template} but does not bundle it",
            target.name
        );
    }
    for file in &manifest.files {
        let Some(shared_as) = file.shared_as.as_deref() else {
            continue;
        };
        let bundled = target
            .templates
            .iter()
            .find(|bundled| bundled.path == file.template)
            .expect("declared artifact template must be bundled");
        assert_eq!(
            bundled.shared_name,
            Some(shared_as),
            "target {} must publish {} under its declared shared_as name",
            target.name,
            file.template
        );
    }
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

/// A verbatim copy of a target directory keeps rendering: the copy's declared
/// partial resolves to the identical built-in text, so the resolution order
/// (shared names come from the built-in registry) changes nothing observable.
#[test]
fn copied_target_directory_with_an_unmodified_partial_still_renders() {
    let fixture = compile_fixture(DISCRETE_SMOKE_MODEL, DISCRETE_SMOKE_SOURCE);
    let scratch = tempfile::tempdir().expect("scratch dir");
    let dir = copy_builtin_target_dir("embedded-c-galec", scratch.path());

    let files = render_target_files(
        &fixture.compiled,
        fixture.model_name,
        dir.to_str().expect("utf-8 scratch path"),
        None,
    )
    .expect("verbatim copy of a built-in target must render");
    let builtin = render_target_files(
        &fixture.compiled,
        fixture.model_name,
        "embedded-c-galec",
        None,
    )
    .expect("built-in target must render");
    assert_eq!(
        files
            .iter()
            .map(|file| (file.path.clone(), file.content.clone()))
            .collect::<Vec<_>>(),
        builtin
            .iter()
            .map(|file| (file.path.clone(), file.content.clone()))
            .collect::<Vec<_>>(),
    );
}

/// The honest half of the shared-name resolution order: an external directory
/// cannot register or override a shared name, so a copied target whose partial
/// was edited must be REJECTED rather than silently rendered from the built-in
/// text. This is the trap the loader closes.
#[test]
fn copied_target_directory_with_an_edited_partial_is_rejected() {
    let fixture = compile_fixture(DISCRETE_SMOKE_MODEL, DISCRETE_SMOKE_SOURCE);
    let scratch = tempfile::tempdir().expect("scratch dir");
    let dir = copy_builtin_target_dir("embedded-c-galec", scratch.path());

    let partial = dir.join("symbols.jinja");
    let edited = fs::read_to_string(&partial)
        .expect("read copied partial")
        .replace(
            "\"self\", \"rumoca_galec_sign\"",
            "\"self\", \"gain\", \"rumoca_galec_sign\"",
        );
    fs::write(&partial, &edited).expect("write edited partial");

    let error = render_target_files(
        &fixture.compiled,
        fixture.model_name,
        dir.to_str().expect("utf-8 scratch path"),
        None,
    )
    .expect_err("an edited external partial must not silently no-op");
    let message = format!("{error:#}");
    assert!(
        message.contains("galec-c-symbols.jinja") && message.contains("silently"),
        "the rejection must name the shared partial and the silent no-op: {message}"
    );
}

/// An external directory that invents a shared name is rejected at load, not
/// deep inside a render as a missing template.
#[test]
fn external_target_directory_cannot_add_a_shared_name() {
    let fixture = compile_fixture(DISCRETE_SMOKE_MODEL, DISCRETE_SMOKE_SOURCE);
    let scratch = tempfile::tempdir().expect("scratch dir");
    let dir = copy_builtin_target_dir("embedded-c-galec", scratch.path());

    let manifest_path = dir.join("target.toml");
    let manifest = fs::read_to_string(&manifest_path)
        .expect("read copied manifest")
        .replace(
            "name = \"galec-c-symbols.jinja\"",
            "name = \"my-own-symbols.jinja\"",
        );
    fs::write(&manifest_path, &manifest).expect("write copied manifest");

    let error = render_target_files(
        &fixture.compiled,
        fixture.model_name,
        dir.to_str().expect("utf-8 scratch path"),
        None,
    )
    .expect_err("an invented shared name must be rejected at load");
    let message = format!("{error:#}");
    assert!(
        message.contains("my-own-symbols.jinja") && message.contains("built-in"),
        "the rejection must name the unknown shared name: {message}"
    );
}

/// The shared render-environment namespace is global and flat: one name, one
/// owning target manifest, resolved the same way for every target that
/// imports it. Pinning the inventory here keeps a new shared name a reviewed
/// decision rather than a side effect.
#[test]
fn shared_template_names_are_globally_unique_and_target_owned() {
    let mut owners = BTreeMap::<&str, String>::new();
    for shared in templates::shared_templates() {
        let owner = format!("{}/{}", shared.target, shared.path);
        assert!(
            owners.insert(shared.name, owner.clone()).is_none(),
            "shared render-environment name {} is declared more than once",
            shared.name
        );
        let manifest = parse_target_manifest(
            templates::builtin_target(shared.target)
                .expect("shared template must name a built-in target")
                .manifest,
        )
        .expect("owning target manifest should parse");
        let declared = manifest
            .partials
            .iter()
            .any(|partial| partial.template == shared.path && partial.name == shared.name)
            || manifest.files.iter().any(|file| {
                file.template == shared.path && file.shared_as.as_deref() == Some(shared.name)
            });
        assert!(
            declared,
            "{} must be declared by {owner}'s target.toml",
            shared.name
        );
    }
    assert_eq!(
        owners
            .iter()
            .map(|(name, owner)| format!("{name} <- {owner}"))
            .collect::<Vec<_>>(),
        vec![
            "algorithm-code-manifest.jinja <- galec/manifest.xml.jinja".to_string(),
            "algorithm-code-source.jinja <- galec/model.alg.jinja".to_string(),
            "galec-c-symbols.jinja <- embedded-c-galec/symbols.jinja".to_string(),
            "galec-model.c.jinja <- embedded-c-galec/model.c.jinja".to_string(),
            "galec-model.h.jinja <- embedded-c-galec/model.h.jinja".to_string(),
        ],
        "the shared render-environment inventory changed"
    );
}
