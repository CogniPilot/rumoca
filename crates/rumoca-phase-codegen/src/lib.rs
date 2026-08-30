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
//! Every render function in this crate takes template **source text**, never a
//! path: loading is the caller's, so that one renderer serves both a built-in
//! target and a user's directory target without a second code path.
//!
//! The compiler's own path is the built-in target bundles that `build.rs`
//! embeds from `src/templates/`. A bundle names its templates, its manifest,
//! and its assets, and it is what `--target <name>` resolves to:
//!
//! ```ignore
//! use rumoca_phase_codegen::{render_template, templates};
//!
//! let target = templates::builtin_target("dae-modelica").unwrap();
//! let code = render_template(
//!     &dae,
//!     target.template_source("dae_modelica.mo.jinja").unwrap(),
//! )?;
//! ```
//!
//! A user customizing a target copies the directory out, edits it, and passes
//! the directory to `--target`; `rumoca-compile` reads its `target.toml` and
//! its template files off disk and calls the same render functions with the
//! text it read. A copied directory emits what the built-in emits, including
//! any `[[assets]]` bundle the manifest borrows with `shared_from`: those
//! bytes are read out of the lending target's embedded bundle, since the
//! borrower has no directory of its own to copy.
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
pub(crate) mod views;

pub use codegen::{
    AlgorithmCodeTemplateRenderer, CodegenInput, SolveTemplateRenderer, dae_template_json,
    explicit_algebraic_assignment_complete, render_algorithm_code_template_with_artifact,
    render_ast_template_with_name, render_checked_algorithm_block_template_with_artifact,
    render_checked_algorithm_block_template_with_sources, render_flat_template_with_name,
    render_solve_template_with_name, render_template, render_template_for_input,
    render_template_with_name, render_template_with_name_for_input,
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
///   `extends`ed by artifact templates.
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

        /// Return the exact bytes of one manifest-declared target asset.
        pub fn asset_bytes(&self, path: &str) -> Option<&'static [u8]> {
            self.assets
                .iter()
                .find(|asset| asset.path == path)
                .map(|asset| asset.bytes)
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

    /// A PERMANENTLY RETIRED target name and the message resolution reports.
    /// A retired identity never returns.
    ///
    /// Retirement is authoritative over the registry: [`builtin_target`]
    /// refuses a retired name even if a directory under that name were ever
    /// bundled again, and `build.rs` fails the build first. Removing an entry
    /// is an explicit reviewed decision, never a side effect of adding files.
    #[derive(Clone, Copy, Debug)]
    pub struct RetiredTarget {
        pub name: &'static str,
        pub message: &'static str,
    }

    /// A SUSPENDED target name and the message resolution reports. Distinct
    /// from retirement: the identity is valid architecture and RETURNS once
    /// the checked roots its message names have landed; removing its entry is
    /// that re-registration act, reviewed against those roots.
    #[derive(Clone, Copy, Debug)]
    pub struct SuspendedTarget {
        pub name: &'static str,
        pub message: &'static str,
    }

    /// The retirement record for `name`, when it names a retired target.
    pub fn retired_target(name: &str) -> Option<&'static RetiredTarget> {
        RETIRED_TARGETS.iter().find(|target| target.name == name)
    }

    /// Every permanently retired target name.
    pub fn retired_targets() -> &'static [RetiredTarget] {
        RETIRED_TARGETS
    }

    /// The suspension record for `name`, when it names a suspended target.
    pub fn suspended_target(name: &str) -> Option<&'static SuspendedTarget> {
        SUSPENDED_TARGETS.iter().find(|target| target.name == name)
    }

    /// Every currently suspended target name.
    pub fn suspended_targets() -> &'static [SuspendedTarget] {
        SUSPENDED_TARGETS
    }

    pub fn builtin_target(name: &str) -> Option<&'static BuiltinTarget> {
        if retired_target(name).is_some() || suspended_target(name).is_some() {
            return None;
        }
        BUILTIN_TARGETS.iter().find(|target| target.name == name)
    }

    pub fn builtin_targets() -> &'static [BuiltinTarget] {
        BUILTIN_TARGETS
    }

    pub fn builtin_template_source(target: &str, template: &str) -> Option<&'static str> {
        builtin_target(target).and_then(|target| target.template_source(template))
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
