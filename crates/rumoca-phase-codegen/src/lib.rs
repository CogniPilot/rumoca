#![allow(clippy::too_many_lines)]

//! Template-based code generation phase for the Rumoca compiler.
//!
//! This crate implements code generation from AST/Flat/DAE IR to various
//! target languages using the minijinja template engine.
//!
//! # Design Philosophy
//!
//! Templates receive the full selected IR structure and can walk the expression
//! tree themselves. This provides maximum flexibility - any target can be
//! supported by writing a new template, with no Rust code changes needed.
//!
//! The selected IR is serialized and passed to minijinja, which allows
//! templates to access any field using standard Jinja2 syntax.
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
//! let target = templates::builtin_target("casadi-sx").unwrap();
//! let code = render_template(&dae, target.template_source("casadi_sx.py.jinja").unwrap())?;
//! ```
//!
//! # Writing Templates
//!
//! Templates use Jinja2 syntax. The DAE is passed as `dae` with fields:
//! - `dae.x` - State variables
//! - `dae.y` - Algebraic variables
//! - `dae.p` - Parameters
//! - `dae.u` - Input variables
//! - `dae.variables.constants` - Constants
//! - `dae.f_x` - Continuous implicit equations (MLS B.1a)
//!
//! Expression trees are nested dictionaries that templates can walk:
//! ```jinja
//! {% macro render_expr(expr) -%}
//! {% if expr.Binary %}
//! ({{ render_expr(expr.Binary.lhs) }} + {{ render_expr(expr.Binary.rhs) }})
//! {% elif expr.VarRef %}
//! {{ expr.VarRef.name | sanitize }}
//! {% endif %}
//! {%- endmacro %}
//! ```
//!
//! # Custom Filters
//!
//! - `sanitize` - Replace dots with underscores: `{{ name | sanitize }}`
//! - Standard minijinja filters (length, upper, lower, etc.)

mod codegen;
mod errors;

pub use codegen::{
    CodegenInput, DaeTemplateContext, dae_template_json, render_ast_template,
    render_ast_template_with_name, render_flat_template_with_name, render_solve_template_with_name,
    render_template, render_template_file, render_template_for_input,
    render_template_with_dae_json, render_template_with_dae_json_and_name,
    render_template_with_name, render_template_with_name_for_input,
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
        pub templates: &'static [BuiltinTargetTemplate],
    }

    /// Built-in template source addressed by a target manifest-local path.
    #[derive(Clone, Copy, Debug)]
    pub struct BuiltinTargetTemplate {
        pub path: &'static str,
        pub source: &'static str,
    }

    impl BuiltinTarget {
        pub fn template_source(&self, path: &str) -> Option<&'static str> {
            self.templates
                .iter()
                .find(|template| template.path == path)
                .map(|template| template.source)
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
