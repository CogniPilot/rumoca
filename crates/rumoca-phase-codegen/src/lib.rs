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
//! let code = render_template(&dae, templates::CASADI_SX)?;
//! ```
//!
//! # Writing Templates
//!
//! Templates use Jinja2 syntax. The DAE is passed as `dae` with fields:
//! - `dae.states` - List of (name, var) tuples
//! - `dae.algebraics` - Algebraic variables
//! - `dae.parameters` - Parameters
//! - `dae.inputs` - Input variables
//! - `dae.constants` - Constants
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
    CodegenInput, dae_template_json, render_ast_template, render_ast_template_with_name,
    render_flat_template_with_name, render_template, render_template_file,
    render_template_for_input, render_template_with_dae_json, render_template_with_name,
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
    /// CasADi SX template (Python) — scalar symbolic expressions.
    pub const CASADI_SX: &str = include_str!("templates/casadi_sx.py.jinja");
    /// CasADi MX template (Python) — matrix symbolic with vector variables and casadi.Function DAE.
    pub const CASADI_MX: &str = include_str!("templates/casadi_mx.py.jinja");
    /// Cyecca template (Python).
    pub const CYECCA: &str = include_str!("templates/cyecca.py.jinja");
    /// Julia ModelingToolkit template.
    pub const JULIA_MTK: &str = include_str!("templates/julia_mtk.jl.jinja");
    /// JAX/Diffrax template (Python).
    pub const JAX: &str = include_str!("templates/jax.py.jinja");
/// ONNX model builder template (Python).
    pub const ONNX: &str = include_str!("templates/onnx.py.jinja");
    /// DAE Modelica template (renders Dae IR with classified variables and split equations).
    pub const DAE_MODELICA: &str = include_str!("templates/dae_modelica.mo.jinja");
    /// Flat Modelica template (renders Model for OMC comparison).
    pub const FLAT_MODELICA: &str = include_str!("templates/flat_modelica.mo.jinja");
    /// FMI 2.0 Model Exchange — modelDescription.xml template.
    pub const FMI2_MODEL_DESCRIPTION: &str =
        include_str!("templates/fmi2_model_description.xml.jinja");
    /// FMI 2.0 Model Exchange — C source implementing the FMI2 API.
    pub const FMI2_MODEL: &str = include_str!("templates/fmi2_model.c.jinja");
    /// FMI 2.0 test driver — standalone main() that simulates via FMI2 ME and outputs CSV.
    pub const FMI2_TEST_DRIVER: &str = include_str!("templates/fmi2_test_driver.c.jinja");
    /// Embedded C template — bare-metal flight controller target with RK4 integrator.
    /// No dynamic memory, no OS dependencies, configurable float/double precision.
    pub const EMBEDDED_C: &str = include_str!("templates/embedded_c.c.jinja");
}
