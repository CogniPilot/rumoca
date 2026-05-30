use std::collections::BTreeSet;
use std::fs;
use std::path::PathBuf;

use rumoca::{CompilationResult, Compiler, TemplateIr};
use rumoca_compile::codegen::targets::{
    TargetBundle, TargetManifest, TargetTemplateIr, TargetTemplateSource, parse_target_manifest,
};
use rumoca_phase_codegen::templates;

const SMOKE_SOURCE: &str = r#"
model Smoke
  Real x(start = 1);
  parameter Real k = 2;
equation
  der(x) = -k * x;
end Smoke;
"#;

fn template_ir(ir: TargetTemplateIr) -> TemplateIr {
    match ir {
        TargetTemplateIr::Dae => TemplateIr::Dae,
        TargetTemplateIr::Solve => TemplateIr::Solve,
        TargetTemplateIr::Flat => TemplateIr::Flat,
        TargetTemplateIr::Ast => TemplateIr::Ast,
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
    let compiled = Compiler::new()
        .model("Smoke")
        .compile_str(SMOKE_SOURCE, "Smoke.mo")
        .expect("compile template target smoke model");
    let builtin_names = templates::builtin_targets()
        .iter()
        .map(|target| target.name.to_string())
        .collect::<BTreeSet<_>>();

    assert_eq!(
        discovered_codegen_template_dirs(),
        builtin_names,
        "every codegen template directory must be registered as a built-in target"
    );

    let coverage = render_builtin_template_targets(&compiled);

    assert!(
        coverage.rendered_targets.contains(&"embedded-c"),
        "embedded-c must be covered by target render CI"
    );
    assert!(
        coverage.rendered_targets.contains(&"fmi2") && coverage.rendered_targets.contains(&"fmi3"),
        "FMI targets must be covered by target render CI"
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

struct TemplateTargetCoverage {
    rendered_targets: Vec<&'static str>,
    manifest_only_targets: Vec<&'static str>,
    support_templates: Vec<String>,
}

fn render_builtin_template_targets(compiled: &CompilationResult) -> TemplateTargetCoverage {
    let mut coverage = TemplateTargetCoverage {
        rendered_targets: Vec::new(),
        manifest_only_targets: Vec::new(),
        support_templates: Vec::new(),
    };
    for target in templates::builtin_targets() {
        render_builtin_template_target(compiled, target, &mut coverage);
    }
    coverage
}

fn render_builtin_template_target(
    compiled: &CompilationResult,
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
    render_manifest_target_files(compiled, target, &manifest);
    render_support_templates(compiled, target, &manifest, &mut coverage.support_templates);
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

fn render_manifest_target_files(
    compiled: &CompilationResult,
    target: &templates::BuiltinTarget,
    manifest: &TargetManifest,
) {
    let bundle = TargetBundle::builtin(target.name).expect("builtin target bundle");
    let ir = template_ir(manifest.ir);
    for file in &manifest.files {
        assert_rendered_path(
            compiled,
            target.name,
            file.path.as_str(),
            file.template.as_str(),
            ir,
        );
        assert_rendered_template(
            compiled,
            target.name,
            file.template.as_str(),
            bundle
                .template_source(&file.template)
                .expect("manifest template source")
                .as_ref(),
            ir,
        );
    }
}

fn assert_rendered_path(
    compiled: &CompilationResult,
    target_name: &str,
    path_template: &str,
    template_name: &str,
    ir: TemplateIr,
) {
    let path = compiled
        .render_template_str_with_name_and_ir(path_template, "Smoke", ir)
        .unwrap_or_else(|err| panic!("render path for {target_name}:{path_template}: {err}"));
    assert!(
        !path.trim().is_empty(),
        "target {target_name} rendered an empty output path for {template_name}"
    );
}

fn assert_rendered_template(
    compiled: &CompilationResult,
    target_name: &str,
    template_name: &str,
    template: &str,
    ir: TemplateIr,
) {
    let content = compiled
        .render_template_str_with_name_and_ir(template, "Smoke", ir)
        .unwrap_or_else(|err| panic!("render template {target_name}:{template_name}: {err}"));
    assert!(
        !content.trim().is_empty(),
        "target {target_name} rendered empty content for {template_name}"
    );
}

fn render_support_templates(
    compiled: &CompilationResult,
    target: &templates::BuiltinTarget,
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
        assert_rendered_support_template(compiled, target.name, template, ir);
        support_templates.push(format!("{}:{}", target.name, template.path));
    }
}

fn assert_rendered_support_template(
    compiled: &CompilationResult,
    target_name: &str,
    template: &templates::BuiltinTargetTemplate,
    ir: TemplateIr,
) {
    let content = compiled
        .render_template_str_with_name_and_ir(template.source, "Smoke", ir)
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
