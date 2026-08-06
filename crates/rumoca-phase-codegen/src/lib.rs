//! Template-based code generation phase for the Rumoca compiler.
//!
//! This crate implements code generation from AST/Flat/DAE IR to various
//! target languages using the minijinja template engine.
//!
//! # Design Philosophy
//!
//! Templates receive a phase-specific semantic projection. A DAE template sees
//! dense checked variables, expressions, domains, and owner systems; a Solve
//! template sees executable blocks and their typed layout.
//!
//! # Template Loading
//!
//! Templates can be loaded from files (recommended for customization) or
//! the built-in defaults can be used for convenience:
//!
//! ```ignore
//! use rumoca_phase_codegen::{render_template, render_template_file};
//!
//! // From file (recommended - users can customize)
//! let code = render_template_file(&dae, "my_template.py.jinja")?;
//!
//! // From built-in (convenience for quick use)
//! use rumoca_phase_codegen::templates;
//! let target = templates::builtin_target("dae-modelica").unwrap();
//! let code = render_template(
//!     &dae,
//!     target.template_source("dae_modelica.mo.jinja").unwrap(),
//! )?;
//! ```
//!
//! # Writing Templates
//!
//! Templates use Jinja2 syntax. The DAE is passed as `dae` with fields:
//! - `dae.variables` - dense variables with explicit `role` and `id`
//! - `dae.expressions` - dense expressions whose operands are expression IDs
//! - `dae.domains` - compact structured iteration domains
//! - `dae.systems` - continuous, initialization, event, clock, and temporal owners
//! - `dae.modelica` - Modelica presentation derived from the checked arena
//!
//! Expression references are stable indices into `dae.expressions`:
//! ```jinja
//! {% for owner in dae.systems.continuous.owners %}
//! expression {{ owner.residual }}: {{ dae.expressions[owner.residual].operation.kind }}
//! {% endfor %}
//! ```
//!
//! # Custom Filters
//!
//! - `sanitize` - Replace dots with underscores: `{{ name | sanitize }}`
//! - Standard minijinja filters (length, upper, lower, etc.)

mod codegen;
mod errors;
pub mod views;

pub use codegen::{
    CodegenInput, SolveTemplateRenderer, dae_template_json,
    render_algorithm_code_template_with_artifact, render_ast_template,
    render_ast_template_with_name, render_checked_algorithm_block_template_with_artifact,
    render_flat_template_with_name, render_solve_template_with_name, render_template,
    render_template_file, render_template_for_input, render_template_with_name,
    render_template_with_name_for_input,
};
pub use errors::CodegenError;

/// Built-in template sources.
///
/// These are embedded in the binary as a convenience. For customization,
/// copy these templates to files and modify as needed.
///
/// The template source files are in `crates/rumoca-phase-codegen/src/templates/`.
pub mod templates {
    /// Built-in target directory bundled into the binary.
    #[derive(Clone, Copy, Debug)]
    pub struct BuiltinTarget {
        pub name: &'static str,
        pub manifest: &'static str,
        pub readme: &'static str,
        pub templates: &'static [BuiltinTargetTemplate],
        pub assets: &'static [BuiltinTargetAsset],
    }

    /// Built-in template source addressed by a target manifest-local path.
    #[derive(Clone, Copy, Debug)]
    pub struct BuiltinTargetTemplate {
        pub path: &'static str,
        pub source: &'static str,
    }

    /// Non-template file embedded from a built-in target directory.
    #[derive(Clone, Copy, Debug)]
    pub struct BuiltinTargetAsset {
        pub path: &'static str,
        pub bytes: &'static [u8],
    }

    impl BuiltinTarget {
        pub fn template_source(&self, path: &str) -> Option<&'static str> {
            self.templates
                .iter()
                .find(|template| template.path == path)
                .map(|template| template.source)
        }

        pub fn asset_files(&self, source: &str) -> Option<Vec<(&'static str, &'static [u8])>> {
            let prefix = source.trim_end_matches('/');
            let prefix_with_separator = format!("{prefix}/");
            let files = self
                .assets
                .iter()
                .filter_map(|asset| {
                    asset
                        .path
                        .strip_prefix(&prefix_with_separator)
                        .map(|relative| (relative, asset.bytes))
                })
                .collect::<Vec<_>>();
            (!files.is_empty()).then_some(files)
        }
    }

    pub fn builtin_target(name: &str) -> Option<&'static BuiltinTarget> {
        BUILTIN_TARGETS.iter().find(|target| target.name == name)
    }

    pub fn builtin_targets() -> &'static [BuiltinTarget] {
        BUILTIN_TARGETS
    }

    pub fn builtin_template_source(target: &str, template: &str) -> Option<&'static str> {
        builtin_target(target).and_then(|target| target.template_source(template))
    }

    include!(concat!(env!("OUT_DIR"), "/templates_generated.rs"));
}
