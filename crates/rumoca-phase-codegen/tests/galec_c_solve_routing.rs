//! #483: every GALEC C model unit prints from a Solve refinement.
//!
//! Two properties together make that a routing guarantee rather than a
//! convention each caller has to remember:
//!
//! * every built-in artifact template that prints GALEC C from the Solve view
//!   (it reads both the Algorithm Code and Solve views) belongs to a
//!   target whose manifest declares `ir = "algorithm-block"`, the IR every C
//!   entry point (CLI, LSP preview, both WASM bindings) refines before it
//!   renders; and
//! * those templates fail closed when rendered without a refinement, so a
//!   path that forgot to refine — including a custom manifest that inherits
//!   them under another IR — gets an error, never the pre-Solve C; and
//! * with both views present, the C bodies are owned by the Solve block: a
//!   change to the old GALEC method expressions alone leaves the C unchanged.

use rumoca_ir_galec::ast as galec;
use rumoca_ir_galec::package::CheckedAlgorithmBlock;
use rumoca_phase_codegen::{
    AlgorithmCodeTemplateRenderer, render_algorithm_code_template_with_artifact,
    render_checked_algorithm_block_template_with_artifact, templates,
};
use serde_json::json;

mod support;

const MODEL: &str = "RoutingFixture";

/// Whether `source` reads both the Algorithm Code view and the Solve view: a
/// GALEC model unit printed from its refinement. (The ODE targets read a
/// `solve_block` of their own IR and no Algorithm Code view.)
fn reads_refined_galec(source: &str) -> bool {
    reads(source, "algorithm_code") && reads(source, "solve_block")
}

/// Whether `source` names `view`, directly or through a shared template it
/// extends, includes or imports (each followed once: partials import one
/// another).
fn reads(source: &str, view: &str) -> bool {
    let mut pending = vec![source];
    let mut seen = std::collections::BTreeSet::new();
    while let Some(source) = pending.pop() {
        if source.contains(view) {
            return true;
        }
        for name in shared_references(source) {
            if let Some(shared) = templates::shared_template(name).filter(|_| seen.insert(name)) {
                pending.push(shared.source);
            }
        }
    }
    false
}

/// The shared template names `source` pulls in.
fn shared_references(source: &str) -> Vec<&str> {
    let mut names = Vec::new();
    for keyword in ["extends \"", "include \"", "import \"", "from \""] {
        for (at, _) in source.match_indices(keyword) {
            names.extend(source[at + keyword.len()..].split('"').next());
        }
    }
    names
}

/// The built-in (target, artifact template) pairs that print a GALEC model
/// unit from its Solve refinement, in target then path order.
fn solve_c_artifacts() -> Vec<(&'static str, &'static str)> {
    templates::builtin_targets()
        .iter()
        .flat_map(|target| {
            target
                .templates
                .iter()
                .filter(|template| template.role == templates::BuiltinTemplateRole::Artifact)
                .filter(|template| reads_refined_galec(template.source))
                .map(|template| (target.name, template.path))
        })
        .collect::<std::collections::BTreeSet<_>>()
        .into_iter()
        .collect()
}

#[test]
fn every_template_that_prints_solve_c_belongs_to_an_algorithm_block_target() {
    let artifacts = solve_c_artifacts();
    // Positive control: both C targets' model units are found, so a scan that
    // matched nothing cannot pass vacuously.
    assert_eq!(
        artifacts,
        [
            ("embedded-c-galec", "model.c.jinja"),
            ("embedded-c-galec", "model.h.jinja"),
            ("galec-production", "model.c.jinja"),
            ("galec-production", "model.h.jinja"),
        ],
        "the Solve-reading artifact templates changed; review their routing"
    );
    for (name, path) in artifacts {
        let target = templates::builtin_target(name).expect("built-in target");
        assert!(
            target
                .manifest
                .lines()
                .any(|line| line.trim() == r#"ir = "algorithm-block""#),
            "{name}/{path} reads the Solve view, so {name} must declare the \
             algorithm-block IR every C entry point refines for"
        );
    }
}

fn fixture() -> CheckedAlgorithmBlock {
    let mut block = galec::Block::new(galec::Name::ident(MODEL));
    block.interface = vec![galec::InterfaceVariable {
        kind: galec::InterfaceKind::Output,
        decl: galec::VariableDeclaration::scalar(galec::ScalarType::Real, galec::Name::ident("y")),
        start: None,
    }];
    block.do_step.statements = vec![galec::Spanned::new(
        galec::Statement::Assignment {
            target: galec::Reference::state(galec::Name::ident("y")),
            value: galec::Expression::Real(2.0),
        },
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name("r.mo"), 0, 1),
    )];
    CheckedAlgorithmBlock::construct(block).expect("routing fixture must be valid GALEC")
}

#[test]
fn the_solve_c_templates_render_only_from_a_refinement() {
    let block = fixture();
    let (package, _) = support::refine(&block).expect("routing fixture must refine");
    for (name, path) in solve_c_artifacts() {
        let template = templates::builtin_template_source(name, path).expect("built-in template");
        // Positive control: the refined route renders.
        support::render_refined(&block, &rumoca_core::SourceMap::new(), template, MODEL)
            .unwrap_or_else(|error| panic!("{name}/{path} must render when refined: {error}"));
        // The package route a non-algorithm-block manifest takes, and the
        // checked-block route of the unrefined `.alg` boundary, both refuse.
        let unrefined = [
            render_algorithm_code_template_with_artifact(&package, &json!({}), template, MODEL),
            render_checked_algorithm_block_template_with_artifact(
                &block,
                &json!({}),
                template,
                MODEL,
            ),
        ];
        for result in unrefined {
            assert!(
                result.is_err(),
                "{name}/{path} rendered without a Solve refinement:\n{}",
                result.unwrap_or_default()
            );
        }
    }
}

/// A block whose DoStep computes `y := <rhs>` from an input `u`. Every
/// declaration and span is the same for each `rhs`.
fn paired_fixture(rhs: galec::Expression) -> CheckedAlgorithmBlock {
    let variable = |kind, name| galec::InterfaceVariable {
        kind,
        decl: galec::VariableDeclaration::scalar(galec::ScalarType::Real, galec::Name::ident(name)),
        start: None,
    };
    let mut block = galec::Block::new(galec::Name::ident(MODEL));
    block.interface = vec![
        variable(galec::InterfaceKind::Input, "u"),
        variable(galec::InterfaceKind::Output, "y"),
    ];
    block.do_step.statements = vec![galec::Spanned::new(
        galec::Statement::Assignment {
            target: galec::Reference::state(galec::Name::ident("y")),
            value: rhs,
        },
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name("r.mo"), 0, 1),
    )];
    CheckedAlgorithmBlock::construct(block).expect("paired fixture must be valid GALEC")
}

fn u_times_or_plus(op: galec::BinaryOp, literal: f64) -> galec::Expression {
    galec::Expression::binary(
        op,
        galec::Expression::Ref(galec::Reference::state(galec::Name::ident("u"))),
        galec::Expression::Real(literal),
    )
}

/// Render `template` from `package` and `refined` with every other input of
/// the paired control fixed: no source map, the fixture model name, no
/// artifact facts and the renderer's default options.
fn paired_render(
    package: &rumoca_ir_galec::package::AlgorithmCodePackage,
    refined: &rumoca_ir_solve::SolveAlgorithmBlock,
    template: &str,
) -> String {
    AlgorithmCodeTemplateRenderer::with_solve_block(
        package,
        refined,
        &rumoca_core::SourceMap::new(),
    )
    .and_then(|renderer| renderer.render_with_name_and_artifact(template, MODEL, &()))
    .expect("paired render")
}

/// Expression ownership, the paired-view control.
///
/// The input boundary held fixed for every render: the refined Solve block
/// (one value, refined once from the base), its binary32/signed32 profile,
/// the block name, interface declarations and spans (so the symbol policy
/// and state field map), the template bytes, the model name, the artifact
/// facts, the source map and the renderer's options. The only input that
/// varies is the GALEC package's DoStep right-hand side: a changed literal
/// and a changed operation. The C must not change; a body that re-read the
/// GALEC operations would.
#[test]
fn c_bodies_are_owned_by_the_solve_block_not_the_galec_expressions() {
    let base = paired_fixture(u_times_or_plus(galec::BinaryOp::Mul, 2.0));
    let (base_package, refined) = support::refine(&base).expect("base must refine");
    for (change, rhs) in [
        ("literal", u_times_or_plus(galec::BinaryOp::Mul, 3.0)),
        ("operation", u_times_or_plus(galec::BinaryOp::Add, 2.0)),
    ] {
        let variant = paired_fixture(rhs);
        let variant_package = support::package(&variant).expect("variant must package");
        // The change is real in the GALEC view…
        assert_ne!(
            format!("{base_package:?}"),
            format!("{variant_package:?}"),
            "the {change} change must reach the GALEC package"
        );
        // …and reaches the C when the Solve block carries it (sensitivity).
        let (_, variant_refined) = support::refine(&variant).expect("variant must refine");
        let model_c = templates::builtin_template_source("embedded-c-galec", "model.c.jinja")
            .expect("built-in template");
        assert_ne!(
            paired_render(&base_package, &refined, model_c),
            paired_render(&variant_package, &variant_refined, model_c),
            "the {change} change must show in C printed from its own refinement"
        );
        for (name, path) in solve_c_artifacts() {
            let template =
                templates::builtin_template_source(name, path).expect("built-in template");
            assert_eq!(
                paired_render(&base_package, &refined, template),
                paired_render(&variant_package, &refined, template),
                "{name}/{path}: with the Solve block fixed, a GALEC {change} change \
                 alone must not change the C"
            );
        }
    }
}
