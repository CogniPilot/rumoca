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
//! Every renderer accepts an opaque checked per-file plan whose source,
//! artifact kind, semantic context, and checked view were bound together
//! before rendering. Standalone template strings carry no artifact authority.
//!
//! The compiler's own path is the built-in target bundles that `build.rs`
//! embeds from `src/templates/`. A bundle names its templates, its manifest,
//! and its assets, and it is what `--target <name>` resolves to:
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
mod rendered_template_file;
mod template_bindings;
mod template_file;
pub(crate) mod views;

pub use codegen::{
    AdmittedFmiRenderingInput, AlgorithmCodeTemplateRenderer, PreparedAstRendering,
    PreparedCompletionMessage, PreparedDaeRendering, PreparedFlatRendering,
    PreparedFmiComponentRendering, PreparedSolveModelRendering, dae_template_json,
    explicit_algebraic_assignment_complete, prepare_ast_rendering, prepare_dae_rendering,
    prepare_flat_rendering, render_ast_template_content, render_casadi_execution_model,
    render_checked_algorithm_block_source, render_correlated_algorithm_code_file,
    render_dae_template_content, render_flat_template_content, render_mlir_execution_model,
    render_output_path, render_packaged_algorithm_code_file,
    render_solve_algorithm_production_file, target_template_environment,
};
pub use errors::CodegenError;
pub use rendered_template_file::{
    RenderedAlgorithmCodeSourceFile, RenderedCorrelatedAlgorithmCodeFile,
    RenderedPackagedAlgorithmCodeFile, RenderedProductionCodeFile, UntrustedRenderedText,
};
pub use template_bindings::{TemplateBindingError, TemplateBindingValue, TemplateBindings};
pub use template_file::{
    AlgorithmCodeSourceOutputPathError, AlgorithmCodeSourceOutputPathTemplate,
    AlgorithmCodeTemplateFile, AlgorithmCodeTemplateSpec, CorrelatedAlgorithmCodeArtifactRole,
    CorrelatedAlgorithmCodeTemplateFile, CorrelatedAlgorithmCodeTemplateSpec,
    PackagedAlgorithmCodeTemplateFile, PackagedAlgorithmCodeTemplateSpec,
    PreparedCorrelatedAlgorithmCodeTemplateFile, PreparedPackagedAlgorithmCodeTemplateFile,
    PreparedSolveAlgorithmTemplateFile, ProductionCodeFileRole, SolveAlgorithmTemplateFile,
    SolveAlgorithmTemplateSpec, TemplateArtifactKind, TemplateFilePlanError,
    TemplateSemanticContext, algorithm_code_template_spec, correlated_algorithm_code_template_spec,
    packaged_algorithm_code_template_spec, prepare_correlated_algorithm_code_template_file,
    prepare_packaged_algorithm_code_template_file, prepare_solve_algorithm_template_file,
    solve_algorithm_template_spec,
};
pub use views::{
    AlgorithmCodeArtifactLayout, AlgorithmCodeArtifactLayoutError,
    AlgorithmCodeArtifactLayoutMember, AlgorithmCodeArtifactLayoutSpec, AlgorithmCodeArtifactRole,
    AlgorithmCodePortableMemberPath, AlgorithmCodeRepresentationFile, PreparedAlgorithmCodePackage,
    PreparedSolveAlgorithmDeclaration, PreparedSolveAlgorithmDeclarationRole,
    PreparedSolveAlgorithmDimension, PreparedSolveAlgorithmInitialization,
    PreparedSolveAlgorithmInitializationValue, PreparedSolveAlgorithmLiteral,
    PreparedSolveAlgorithmMethod, PreparedSolveAlgorithmMethodStatus,
    PreparedSolveAlgorithmOperation, PreparedSolveAlgorithmProduction,
    PreparedSolveAlgorithmRealLiteral, PreparedSolveAlgorithmRegister,
    PreparedSolveAlgorithmScalarType, PreparedSolveAlgorithmStorageOwner, ProductionArtifactLayout,
    ProductionArtifactLayoutError, ProductionArtifactLayoutMember, ProductionArtifactLayoutSpec,
    ProductionArtifactRepresentationFile, ProductionArtifactRole, ProductionClockPresentation,
    ProductionCodeContainerProfile, ProductionCodeLanguage, ProductionCodeLanguageStandard,
    ProductionCodePlatform, ProductionDeclarationPresentation, ProductionHeaderInclude,
    ProductionLifecycleStorageAbi, ProductionLogicalDataAnchorConvention,
    ProductionLogicalDataPresentation, ProductionManifestName, ProductionManifestPresentation,
    ProductionMethodPresentation, ProductionPortableMemberPath, ProductionPresentationIdentifier,
    ProductionPresentationPlan, ProductionRegisterPresentation, ProductionRepresentationFileName,
    ProductionRepresentationFilePath, ProductionRepresentationName,
    ProductionSuccessStatusPresentation, SolveAlgorithmProductionFailureTransport,
    SolveAlgorithmProductionPreparationError, SolveAlgorithmProductionProfile,
    SolveAlgorithmProductionRequirement, prepare_algorithm_code_package,
    prepare_solve_algorithm_production,
};

/// Built-in template sources.
///
/// These are embedded in the binary as a convenience. For customization,
/// copy these templates to files and modify as needed.
///
/// The template source files are in `crates/rumoca-phase-codegen/src/templates/`.
///
/// Every bundled template is one artifact declared by one `[[files]]` row.
/// Global partials and shared aliases are forbidden because they can bridge
/// semantic contexts; uniquely owned syntax helpers stay local to their
/// artifact template.
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

    pub(crate) const fn mlir_execution_template() -> &'static str {
        MLIR_MLIR_MLIR_JINJA
    }

    pub(crate) const fn casadi_execution_template() -> &'static str {
        CASADI_ODE_CASADI_ODE_PY_JINJA
    }

    /// Exact C support bytes owned by the MLIR execution product.
    pub const fn mlir_runtime_support_c() -> &'static [u8] {
        MLIR_ASSET_RUNTIME_RUMOCA_RUNTIME_C
    }

    /// Exact LLVM support bytes owned by the MLIR Euler execution product.
    pub const fn mlir_euler_update_llvm() -> &'static [u8] {
        MLIR_ASSET_RUNTIME_EULER_UPDATE_LL
    }

    include!(concat!(env!("OUT_DIR"), "/templates_generated.rs"));
}
