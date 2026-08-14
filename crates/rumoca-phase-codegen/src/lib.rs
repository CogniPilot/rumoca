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
    AlgorithmCodeTemplateRenderer, CodegenInput, SolveTemplateRenderer, dae_template_json,
    explicit_algebraic_assignment_complete, render_algorithm_code_template_with_artifact,
    render_ast_template, render_ast_template_with_name,
    render_checked_algorithm_block_template_with_artifact,
    render_checked_algorithm_block_template_with_sources, render_flat_template_with_name,
    render_solve_template_with_name, render_template, render_template_file,
    render_template_for_input, render_template_with_name, render_template_with_name_for_input,
};
pub use errors::CodegenError;

/// Built-in template sources.
///
/// These are embedded in the binary as a convenience. For customization,
/// copy these templates to files and modify as needed.
///
/// The template source files are in `crates/rumoca-phase-codegen/src/templates/`.
///
/// # Support partials and shared templates
///
/// A target directory holds two kinds of template, distinguished by the
/// manifest declaration that owns them:
///
/// * an **artifact template** is declared by a `[[files]]` entry and renders
///   exactly one product file; and
/// * a **support partial** is declared by a `[[partials]]` entry, renders no
///   product file at all, and exists only to be `import`ed, `include`d, or
///   `extends`ed by artifact templates. `embedded-c-galec/symbols.jinja` — the
///   single declaration site of the GALEC-derived C symbol policy — is one.
///
/// Either kind may be published to the shared render environment under a
/// globally unique name: `[[partials]].name` for a support partial,
/// `[[files]].shared_as` for an artifact template another target extends.
/// [`templates::shared_templates`] is that registry, generated from the
/// manifests by `build.rs`, and it is the *only* thing the internal render
/// environment registers — a template becomes importable by declaring it,
/// never by adding Rust.
///
/// Resolution order is deliberately flat and total: shared names live in one
/// global namespace owned by the built-in target manifests, `build.rs` rejects
/// a duplicated name, and it rejects any bundled `.jinja` file that no
/// declaration claims. An external (directory) target therefore cannot
/// introduce or override a shared name by copying a target directory; the
/// loader in `rumoca-compile` rejects such a manifest instead of letting the
/// copy's partial silently no-op.
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

    /// Which manifest declaration owns a bundled template.
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub enum BuiltinTemplateRole {
        /// Declared by a `[[files]]` entry: renders one product file.
        Artifact,
        /// Declared by a `[[partials]]` entry: renders no product file, and is
        /// reachable only through the shared render environment.
        SupportPartial,
    }

    /// Built-in template source addressed by a target manifest-local path.
    #[derive(Clone, Copy, Debug)]
    pub struct BuiltinTargetTemplate {
        pub path: &'static str,
        pub source: &'static str,
        /// Shared render-environment name this template is published under,
        /// when its manifest declares one.
        pub shared_name: Option<&'static str>,
        pub role: BuiltinTemplateRole,
    }

    /// One entry of the shared render-environment registry: the template a
    /// `{% import %}`/`{% include %}`/`{% extends %}` of `name` resolves to,
    /// and the target manifest that published it.
    #[derive(Clone, Copy, Debug)]
    pub struct BuiltinSharedTemplate {
        pub name: &'static str,
        pub target: &'static str,
        pub path: &'static str,
        pub source: &'static str,
        pub role: BuiltinTemplateRole,
    }

    /// Non-template file embedded from a built-in target directory.
    #[derive(Clone, Copy, Debug)]
    pub struct BuiltinTargetAsset {
        pub path: &'static str,
        pub bytes: &'static [u8],
    }

    impl BuiltinTarget {
        /// The target's support partials: bundled templates that render no
        /// product file and therefore have no `[[files]]` entry.
        pub fn support_partials(&self) -> impl Iterator<Item = &'static BuiltinTargetTemplate> {
            self.templates
                .iter()
                .filter(|template| template.role == BuiltinTemplateRole::SupportPartial)
        }

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

    /// The two templates whose bytes define the emitted GALEC kernel library.
    const GALEC_KERNEL_LIBRARY_TEMPLATES: [(&str, &str); 2] = [
        ("embedded-c-galec", "kernels.h.jinja"),
        ("embedded-c-galec", "kernels.c.jinja"),
    ];

    /// Content identity of the GALEC array-kernel library this build emits,
    /// printed into the generated `rumoca_galec_kernels.h` as
    /// `RUMOCA_GALEC_KERNELS_VERSION` and checked by every generated model
    /// source.
    ///
    /// It is a hash of the *templates*, not of a build timestamp or a version
    /// string someone has to remember to bump. Two rumoca builds therefore
    /// agree on it exactly when they emit byte-identical kernels, which is the
    /// property the `#error` in the generated sources needs: mixing a model
    /// `.c` with kernel sources from a build whose kernels differ fails to
    /// compile, and mixing it with a build whose kernels are identical — the
    /// case an integrator relies on when deduplicating the library across
    /// several eFMUs — does not.
    ///
    /// Hashing the templates rather than the rendered files avoids a
    /// circularity: the rendered files contain this value. It loses nothing,
    /// because the rendering is a pure function of the template bytes (neither
    /// template reads the model view).
    ///
    /// FNV-1a/32. The digest only has to be stable and well mixed; nothing here
    /// is a security property.
    pub fn galec_kernel_library_version() -> u32 {
        let mut hash: u32 = 0x811c_9dc5;
        for (target, template) in GALEC_KERNEL_LIBRARY_TEMPLATES {
            let source = builtin_template_source(target, template)
                .expect("the GALEC kernel library templates are bundled with the compiler");
            for byte in source.as_bytes() {
                hash ^= u32::from(*byte);
                hash = hash.wrapping_mul(0x0100_0193);
            }
        }
        hash
    }

    /// Every template published to the shared render environment, sorted by
    /// shared name. Generated from the target manifests; see the module docs
    /// for the declaration rules `build.rs` enforces.
    pub fn shared_templates() -> &'static [BuiltinSharedTemplate] {
        SHARED_TEMPLATES
    }

    /// The shared-registry entry a `{% import "name" %}` resolves to.
    pub fn shared_template(name: &str) -> Option<&'static BuiltinSharedTemplate> {
        SHARED_TEMPLATES.iter().find(|shared| shared.name == name)
    }

    include!(concat!(env!("OUT_DIR"), "/templates_generated.rs"));
}
