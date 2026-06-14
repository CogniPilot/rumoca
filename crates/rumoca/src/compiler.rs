//! High-level API for compiling Modelica models to DAE representations.
//!
//! This module provides a clean, ergonomic interface for using rumoca as a library.
//! The main entry point is the [`Compiler`] struct, which uses a builder pattern
//! for configuration.
//!
//! # Examples
//!
//! Basic usage:
//!
//! ```ignore
//! use rumoca::Compiler;
//!
//! let result = Compiler::new()
//!     .model("MyModel")
//!     .compile_file("model.mo")?;
//! ```
//!
//! Compiling from a string:
//!
//! ```ignore
//! use rumoca::Compiler;
//!
//! let modelica_code = r#"
//!     model Integrator
//!         Real x(start=0);
//!     equation
//!         der(x) = 1;
//!     end Integrator;
//! "#;
//!
//! let result = Compiler::new()
//!     .model("Integrator")
//!     .compile_str(modelica_code, "Integrator.mo")?;
//! ```

use std::collections::HashSet;
use std::fs;
use std::path::Path;

use rumoca_compile::analysis as dae_analysis;
use rumoca_compile::codegen::{
    CodegenError, dae_to_template_json, render_ast_template_with_name,
    render_dae_template_with_json, render_dae_template_with_json_and_name,
    render_flat_template_with_name, render_solve_template_with_dae_and_name,
};
use rumoca_compile::compile::{
    Dae, DaeCompilationResult as CompileDaeCompilationResult, FlatModel, PhaseResult, ResolvedTree,
    Session, SessionConfig, SourceRootKind,
};
use rumoca_compile::parsing::collect_compile_unit_source_files;
use rumoca_compile::source_roots::{
    PackageLayoutError, canonical_path_key, parse_source_root_with_cache, plan_source_root_loads,
    referenced_unloaded_source_root_paths, render_source_root_status_message,
    resolve_source_root_cache_dir, source_root_source_set_key,
};
use rumoca_sim::{lower_solve_artifacts, lower_solve_problem};
use serde_json::{Map, Value};

use crate::error::CompilerError;

/// Result of a successful compilation.
#[derive(Debug)]
pub struct CompilationResult {
    /// The DAE representation.
    pub dae: Dae,
    /// Detailed continuous balance inputs validated during DAE construction.
    pub balance_detail: dae_analysis::BalanceDetail,
    /// The flat model (intermediate).
    pub flat: FlatModel,
    /// The resolved tree (intermediate, before instantiation and typechecking).
    pub resolved: ResolvedTree,
}

/// Lean result of a successful DAE-only compilation.
pub type DaeCompilationResult = CompileDaeCompilationResult;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TemplateIr {
    Dae,
    Solve,
    Flat,
    Ast,
}

/// Return a scalarized clone of `dae` — vector equations like
/// `der(x) = -x` for `x: Real[3]` are expanded to one equation per element.
///
/// For scalar-only models this is a no-op on the resulting DAE, so it is
/// safe to apply unconditionally before template rendering.
fn scalarized_dae(dae: &Dae) -> Result<Dae, CodegenError> {
    let mut dae = dae.clone();
    rumoca_compile::phase_structural::scalarize_equations(&mut dae)
        .map_err(|err| CodegenError::template(err.to_string()))?;
    Ok(dae)
}

/// Whether a template references the AD-Jacobian artifacts. Only the AD targets
/// (casadi, symforce, mlir, fmi3) do; for the rest we skip the (expensive)
/// `lower_solve_artifacts` build entirely.
fn template_uses_solve_artifacts(template: &str) -> bool {
    template.contains("artifacts") || template.contains("jacobian")
}

fn render_solve_template_with_name(
    dae_model: &Dae,
    template: &str,
    model_name: &str,
) -> Result<String, CodegenError> {
    let dae = scalarized_dae(dae_model)?;
    // Build the lowered solve IR once and hand it to the codegen crate's lazy
    // `from_serialize` render path, instead of materializing the whole IR as a
    // `serde_json::Value` tree (the codegen-context OOM on large scalar models).
    let solve =
        lower_solve_problem(&dae).map_err(|err| CodegenError::template(err.to_string()))?;
    let artifacts = if template_uses_solve_artifacts(template) {
        lower_solve_artifacts(&solve).map_err(|err| CodegenError::template(err.to_string()))?
    } else {
        Default::default()
    };
    // The `dae` context object is small (the DAE keeps function calls as calls,
    // not the lowered scalar ops), so it stays a serde_json::Value.
    let dae_json = dae_to_template_json(&dae)?;
    render_solve_template_with_dae_and_name(&dae_json, &solve, &artifacts, template, Some(model_name))
}

impl CompilationResult {
    fn template_json_dae_only(&self) -> Result<Value, CompilerError> {
        dae_to_template_json(&self.dae).map_err(CompilerError::TemplateError)
    }

    fn is_prunable_child(child: &Value) -> bool {
        match child {
            Value::Null => true,
            Value::Object(map) => map.is_empty(),
            Value::Array(items) => items.is_empty(),
            _ => false,
        }
    }

    fn prune_json_object(object: &mut Map<String, Value>) {
        let keys: Vec<String> = object.keys().cloned().collect();
        let mut to_remove = Vec::new();
        for key in keys {
            let Some(child) = object.get_mut(&key) else {
                continue;
            };
            Self::prune_json_value(child);
            if Self::is_prunable_child(child) {
                to_remove.push(key);
            }
        }
        for key in to_remove {
            object.remove(&key);
        }
    }

    fn prune_json_array(items: &mut Vec<Value>) {
        for child in items.iter_mut() {
            Self::prune_json_value(child);
        }
        items.retain(|child| !matches!(child, Value::Null));
    }

    fn strip_scalar_count_default(object: &mut Map<String, Value>) {
        let scalar_count_is_one = object
            .get("scalar_count")
            .and_then(Value::as_u64)
            .is_some_and(|count| count == 1);
        if scalar_count_is_one {
            object.remove("scalar_count");
        }
    }

    fn strip_empty_origin(object: &mut Map<String, Value>) {
        let origin_is_empty = object
            .get("origin")
            .and_then(Value::as_str)
            .is_some_and(str::is_empty);
        if origin_is_empty {
            object.remove("origin");
        }
    }

    fn strip_common_defaults(object: &mut Map<String, Value>) {
        Self::strip_scalar_count_default(object);
        Self::strip_empty_origin(object);
    }

    fn move_rhs_field(object: &mut Map<String, Value>, field_name: &str) {
        let Some(rhs) = object.remove("rhs") else {
            return;
        };
        object.insert(field_name.to_string(), rhs);
    }

    fn normalize_residual_row(object: &mut Map<String, Value>) {
        object.remove("lhs");
        Self::move_rhs_field(object, "residual");
        Self::strip_common_defaults(object);
    }

    fn normalize_assignment_row(object: &mut Map<String, Value>) {
        if object.get("lhs").is_some_and(Value::is_null) {
            object.remove("lhs");
        }
        Self::strip_common_defaults(object);
    }

    fn normalize_initial_row(object: &mut Map<String, Value>) {
        let lhs = object.remove("lhs");
        let Some(rhs) = object.remove("rhs") else {
            Self::strip_common_defaults(object);
            return;
        };
        let has_lhs = lhs.as_ref().is_some_and(|value| !value.is_null());
        let kind = if has_lhs { "assignment" } else { "residual" };
        object.insert("kind".to_string(), Value::String(kind.to_string()));
        if let Some(lhs) = lhs
            && !lhs.is_null()
        {
            object.insert("lhs".to_string(), lhs);
        }
        object.insert("expr".to_string(), rhs);
        Self::strip_common_defaults(object);
    }

    fn prune_json_value(value: &mut Value) {
        match value {
            Value::Object(object) => Self::prune_json_object(object),
            Value::Array(items) => Self::prune_json_array(items),
            _ => {}
        }
    }

    fn push_nonempty<T: serde::Serialize>(
        out: &mut Map<String, Value>,
        key: &str,
        value: &T,
    ) -> Result<(), CompilerError> {
        let mut json =
            serde_json::to_value(value).map_err(|e| CompilerError::JsonError(e.to_string()))?;
        Self::prune_json_value(&mut json);
        let is_empty = match &json {
            Value::Array(values) => values.is_empty(),
            Value::Object(values) => values.is_empty(),
            _ => false,
        };
        if !is_empty {
            out.insert(key.to_string(), json);
        }
        Ok(())
    }

    fn residuals_to_minimal_json<T: serde::Serialize>(
        residuals: &[T],
    ) -> Result<Vec<Value>, CompilerError> {
        residuals
            .iter()
            .map(|residual| {
                let mut value = serde_json::to_value(residual)
                    .map_err(|e| CompilerError::JsonError(e.to_string()))?;
                if let Value::Object(object) = &mut value {
                    Self::normalize_residual_row(object);
                }
                Ok(value)
            })
            .collect()
    }

    fn assignments_to_minimal_json<T: serde::Serialize>(
        assignments: &[T],
    ) -> Result<Vec<Value>, CompilerError> {
        assignments
            .iter()
            .map(|assignment| {
                let mut value = serde_json::to_value(assignment)
                    .map_err(|e| CompilerError::JsonError(e.to_string()))?;
                if let Value::Object(object) = &mut value {
                    Self::normalize_assignment_row(object);
                }
                Ok(value)
            })
            .collect()
    }

    fn initial_to_minimal_json<T: serde::Serialize>(
        initial_rows: &[T],
    ) -> Result<Vec<Value>, CompilerError> {
        initial_rows
            .iter()
            .map(|row| {
                let mut value = serde_json::to_value(row)
                    .map_err(|e| CompilerError::JsonError(e.to_string()))?;
                if let Value::Object(object) = &mut value {
                    Self::normalize_initial_row(object);
                }
                Ok(value)
            })
            .collect()
    }

    /// Render the DAE using a template file.
    pub fn render_template(&self, template_path: &str) -> Result<String, CompilerError> {
        let template_content = fs::read_to_string(template_path)
            .map_err(|e| CompilerError::io_error(template_path, e.to_string()))?;

        self.render_template_str(&template_content)
    }

    /// Render the DAE using a template string.
    pub fn render_template_str(&self, template: &str) -> Result<String, CompilerError> {
        let dae_json = self.template_json_dae_only()?;
        render_dae_template_with_json(&dae_json, template).map_err(CompilerError::TemplateError)
    }

    /// Render the DAE using a template string with an explicit model name.
    ///
    /// The model name is exposed as `model_name` in the template context.
    pub fn render_template_str_with_name(
        &self,
        template: &str,
        model_name: &str,
    ) -> Result<String, CompilerError> {
        let dae_json = self.template_json_dae_only()?;
        render_dae_template_with_json_and_name(&dae_json, template, model_name)
            .map_err(CompilerError::TemplateError)
    }

    pub fn render_template_str_with_name_and_ir(
        &self,
        template: &str,
        model_name: &str,
        ir: TemplateIr,
    ) -> Result<String, CompilerError> {
        match ir {
            TemplateIr::Dae => self.render_template_str_with_name(template, model_name),
            TemplateIr::Solve => render_solve_template_with_name(&self.dae, template, model_name)
                .map_err(CompilerError::TemplateError),
            TemplateIr::Flat => render_flat_template_with_name(&self.flat, template, model_name)
                .map_err(CompilerError::TemplateError),
            TemplateIr::Ast => {
                render_ast_template_with_name(self.resolved.inner(), template, model_name)
                    .map_err(CompilerError::TemplateError)
            }
        }
    }

    pub fn to_ir_json(&self, ir: TemplateIr) -> Result<String, CompilerError> {
        match ir {
            TemplateIr::Dae => self.to_json(),
            TemplateIr::Solve => {
                let solve = lower_solve_problem(&self.dae)
                    .map_err(|err| CompilerError::JsonError(err.to_string()))?;
                serde_json::to_string_pretty(&solve)
                    .map_err(|err| CompilerError::JsonError(err.to_string()))
            }
            TemplateIr::Flat => serde_json::to_string_pretty(&self.flat)
                .map_err(|err| CompilerError::JsonError(err.to_string())),
            TemplateIr::Ast => serde_json::to_string_pretty(self.resolved.inner())
                .map_err(|err| CompilerError::JsonError(err.to_string())),
        }
    }

    /// Equation balance (equations - unknowns).
    pub fn balance(&self) -> i64 {
        self.balance_detail.balance()
    }

    /// Whether equation/unknown balance is exact.
    pub fn is_balanced(&self) -> bool {
        self.balance_detail.is_balanced()
    }

    /// Convert the DAE to JSON.
    pub fn to_json(&self) -> Result<String, CompilerError> {
        let mut p = self.dae.variables.parameters.clone();
        // MLS Appendix B groups parameters and constants together in p.
        for (name, var) in &self.dae.variables.constants {
            p.entry(name.clone()).or_insert_with(|| var.clone());
        }

        let f_x = Self::residuals_to_minimal_json(&self.dae.continuous.equations)?;
        let f_z = Self::assignments_to_minimal_json(&self.dae.discrete.real_updates)?;
        let f_m = Self::assignments_to_minimal_json(&self.dae.discrete.valued_updates)?;
        let f_c = Self::assignments_to_minimal_json(&self.dae.conditions.equations)?;
        let initial = Self::initial_to_minimal_json(&self.dae.initialization.equations)?;

        let mut canonical = Map::new();
        Self::push_nonempty(&mut canonical, "p", &p)?;
        Self::push_nonempty(&mut canonical, "x", &self.dae.variables.states)?;
        Self::push_nonempty(&mut canonical, "y", &self.dae.variables.algebraics)?;
        Self::push_nonempty(&mut canonical, "z", &self.dae.variables.discrete_reals)?;
        Self::push_nonempty(&mut canonical, "m", &self.dae.variables.discrete_valued)?;
        Self::push_nonempty(&mut canonical, "f_x", &f_x)?;
        Self::push_nonempty(&mut canonical, "f_z", &f_z)?;
        Self::push_nonempty(&mut canonical, "f_m", &f_m)?;
        Self::push_nonempty(&mut canonical, "f_c", &f_c)?;
        Self::push_nonempty(&mut canonical, "relation", &self.dae.conditions.relations)?;
        Self::push_nonempty(&mut canonical, "initial", &initial)?;
        Self::push_nonempty(&mut canonical, "functions", &self.dae.symbols.functions)?;

        serde_json::to_string_pretty(&Value::Object(canonical))
            .map_err(|e| CompilerError::JsonError(e.to_string()))
    }
}

/// A high-level compiler for Modelica models.
///
/// This struct provides a builder-pattern interface for configuring and executing
/// the compilation pipeline from Modelica source code to DAE representation.
#[derive(Debug, Clone, Default)]
pub struct Compiler {
    /// The main model to compile.
    model_name: Option<String>,
    /// Additional source-root paths to load.
    source_root_paths: Vec<String>,
    /// Enable verbose output.
    verbose: bool,
}

impl Compiler {
    fn log_verbose(&self, message: impl AsRef<str>) {
        if self.verbose {
            eprintln!("{}", message.as_ref());
        }
    }

    /// Create a new compiler with default settings.
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the main model to compile.
    pub fn model(mut self, name: &str) -> Self {
        self.model_name = Some(name.to_string());
        self
    }

    /// Enable or disable verbose output.
    pub fn verbose(mut self, verbose: bool) -> Self {
        self.verbose = verbose;
        self
    }

    /// Add a source-root path to load before compiling.
    ///
    /// Source-root paths can be either:
    /// - A single .mo file
    /// - A directory containing .mo files
    pub fn source_root(mut self, path: &str) -> Self {
        self.source_root_paths.push(path.to_string());
        self
    }

    /// Add multiple source-root paths.
    pub fn source_roots(mut self, paths: &[String]) -> Self {
        self.source_root_paths.extend(paths.iter().cloned());
        self
    }

    /// Load a source-root path into the session.
    ///
    /// Handles both single files and directories recursively.
    fn load_source_root_into_session(
        &self,
        session: &mut Session,
        path: &str,
    ) -> Result<(), CompilerError> {
        let path_obj = Path::new(path);
        let parsed_source_root = parse_source_root_with_cache(path_obj).map_err(|e| {
            if let Some(package_layout_error) = e.downcast_ref::<PackageLayoutError>() {
                return CompilerError::SourceDiagnosticsError {
                    summary: package_layout_error.to_string(),
                    diagnostics: package_layout_error.diagnostics().to_vec(),
                    source_map: Box::new(package_layout_error.source_map().clone()),
                };
            }
            CompilerError::ParseError(format!("{}: {}", path, e))
        })?;

        let source_root_key = source_root_source_set_key(path);
        session.replace_parsed_source_set(
            &source_root_key,
            SourceRootKind::External,
            parsed_source_root.documents,
            None,
        );
        let cache_dir = resolve_source_root_cache_dir();
        let _ = session.sync_source_root_semantic_summary_cache(
            &source_root_key,
            path_obj,
            cache_dir.as_deref(),
        );

        if self.verbose
            && let Some(status) = session.source_root_status(&source_root_key)
        {
            eprintln!("{}", render_source_root_status_message(&status));
        }
        Ok(())
    }

    fn load_required_source_roots(
        &self,
        session: &mut Session,
        source: &str,
    ) -> Result<(), CompilerError> {
        let loaded_source_root_path_keys = HashSet::new();
        let referenced_source_root_paths = referenced_unloaded_source_root_paths(
            source,
            &self.source_root_paths,
            &loaded_source_root_path_keys,
        );
        let referenced_path_keys = referenced_source_root_paths
            .iter()
            .map(|path| canonical_path_key(path))
            .collect::<HashSet<_>>();
        for source_root_path in &self.source_root_paths {
            let path_key = canonical_path_key(source_root_path);
            if referenced_path_keys.contains(&path_key) {
                continue;
            }
            self.log_verbose(format!(
                "[rumoca] Skipping unused source root: {}",
                source_root_path
            ));
        }

        let load_plan =
            plan_source_root_loads(&referenced_source_root_paths, &loaded_source_root_path_keys);
        for skipped in &load_plan.duplicate_root_skips {
            self.log_verbose(format!(
                "[rumoca] Skipping source root {} (duplicate root '{}' already loaded from {})",
                skipped.source_root_path, skipped.root_name, skipped.provider_path
            ));
        }

        for source_root_path in &load_plan.load_paths {
            self.log_verbose(format!(
                "[rumoca] Loading source root: {}",
                source_root_path
            ));
            self.load_source_root_into_session(session, source_root_path)?;
        }

        Ok(())
    }

    fn load_local_compile_unit(
        &self,
        session: &mut Session,
        source: &str,
        file_name: &str,
    ) -> Result<(), CompilerError> {
        let path = Path::new(file_name);
        if !path.is_file() {
            let _ = session.update_document(file_name, source);
            return Ok(());
        }

        let files = collect_compile_unit_source_files(path)
            .map_err(|e| CompilerError::ParseError(format!("{}", e)))?;
        // The directory scan can yield the requested file under a different
        // spelling (`./Solo.mo` vs `Solo.mo`), so compare canonical paths —
        // a raw `Path` comparison registers the file twice and reports it as
        // a duplicate class of itself.
        let requested_canonical = path.canonicalize().ok();
        for sibling in files {
            if sibling == path
                || (requested_canonical.is_some()
                    && sibling.canonicalize().ok() == requested_canonical)
            {
                continue;
            }
            let sibling_path = sibling.to_string_lossy().to_string();
            // A sibling the user never referenced must not poison the
            // compile: skip unreadable files with a warning. If the model
            // actually needs the file, resolution reports the missing names.
            let sibling_source = match fs::read_to_string(&sibling) {
                Ok(sibling_source) => sibling_source,
                Err(error) => {
                    eprintln!(
                        "[rumoca] warning: skipping unreadable sibling source `{sibling_path}`: {error}"
                    );
                    continue;
                }
            };
            let _ = session.update_document(&sibling_path, &sibling_source);
        }

        let _ = session.update_document(file_name, source);
        Ok(())
    }

    /// Compile a Modelica file.
    pub fn compile_file(&self, path: &str) -> Result<CompilationResult, CompilerError> {
        let source =
            fs::read_to_string(path).map_err(|e| CompilerError::io_error(path, e.to_string()))?;

        self.compile_str(&source, path)
    }

    /// Compile a Modelica file through the resolved AST stage only.
    pub fn compile_file_ast(&self, path: &str) -> Result<ResolvedTree, CompilerError> {
        let source =
            fs::read_to_string(path).map_err(|e| CompilerError::io_error(path, e.to_string()))?;

        self.compile_str_ast(&source, path)
    }

    /// Compile a Modelica file through the flat-model stage only.
    pub fn compile_file_flat(&self, path: &str) -> Result<FlatModel, CompilerError> {
        let source =
            fs::read_to_string(path).map_err(|e| CompilerError::io_error(path, e.to_string()))?;

        self.compile_str_flat(&source, path)
    }

    /// Compile a Modelica file from a Path.
    pub fn compile_path(&self, path: &Path) -> Result<CompilationResult, CompilerError> {
        let path_str = path.to_string_lossy().to_string();
        self.compile_file(&path_str)
    }

    /// Compile a Modelica file through DAE only.
    ///
    /// This avoids retaining Flat and Resolved artifacts in callers that only
    /// need simulation-ready DAE output.
    pub fn compile_file_dae(&self, path: &str) -> Result<DaeCompilationResult, CompilerError> {
        let source =
            fs::read_to_string(path).map_err(|e| CompilerError::io_error(path, e.to_string()))?;

        self.compile_str_dae(&source, path)
    }

    /// Compile a Modelica file from a Path through DAE only.
    pub fn compile_path_dae(&self, path: &Path) -> Result<DaeCompilationResult, CompilerError> {
        let path_str = path.to_string_lossy().to_string();
        self.compile_file_dae(&path_str)
    }

    /// Compile Modelica source code.
    pub fn compile_str(
        &self,
        source: &str,
        file_name: &str,
    ) -> Result<CompilationResult, CompilerError> {
        let model_name = self
            .model_name
            .as_ref()
            .ok_or(CompilerError::NoModelSpecified)?;

        if self.verbose {
            eprintln!("[rumoca] Compiling model: {}", model_name);
            eprintln!("[rumoca] Source file: {}", file_name);
        }

        // Create a session and add the document
        let mut session = Session::new(SessionConfig::default());
        self.load_required_source_roots(&mut session, source)?;

        if self.verbose {
            eprintln!("[rumoca] Phase 1-2: Parsing and resolving...");
        }
        self.load_local_compile_unit(&mut session, source, file_name)?;

        if self.verbose {
            eprintln!(
                "[rumoca] Phase 3-6: Strict-reachable compile (with recovery diagnostics)..."
            );
        }

        let mut report = session.compile_model_strict_reachable_with_recovery(model_name);
        let failure_summary = report.failure_summary(usize::MAX);
        let result = match report.requested_result.take() {
            Some(PhaseResult::Success(result)) => {
                if !report.failures.is_empty() {
                    return Err(CompilerError::CompileDiagnosticsError {
                        summary: failure_summary,
                        failures: report.failures,
                        source_map: report.source_map.map(Box::new),
                    });
                }
                *result
            }
            // Phase failures carry spanned diagnostics through
            // `report.failures`; render them like any other compile
            // diagnostics instead of flattening to a string.
            Some(PhaseResult::NeedsInner { .. }) | Some(PhaseResult::Failed { .. }) | None => {
                return Err(CompilerError::CompileDiagnosticsError {
                    summary: failure_summary,
                    failures: report.failures,
                    source_map: report.source_map.map(Box::new),
                });
            }
        };

        // Get the resolved tree for successful compilations.
        let resolved = session.resolved_cached().ok_or_else(|| {
            CompilerError::ResolveError(
                "strict compile produced no cached resolved tree".to_string(),
            )
        })?;

        if self.verbose {
            eprintln!("[rumoca] Compilation complete.");
            eprintln!("[rumoca]   States: {}", result.dae.variables.states.len());
            eprintln!(
                "[rumoca]   Algebraics: {}",
                result.dae.variables.algebraics.len()
            );
            eprintln!(
                "[rumoca]   Parameters: {}",
                result.dae.variables.parameters.len()
            );
            eprintln!(
                "[rumoca]   Continuous equations (f_x): {}",
                result.dae.continuous.equations.len()
            );
            eprintln!("[rumoca]   Balance: {}", result.balance_detail.balance());
        }

        Ok(CompilationResult {
            dae: result.dae,
            balance_detail: result.balance_detail,
            flat: result.flat,
            resolved,
        })
    }

    /// Compile Modelica source code through the resolved AST stage only.
    pub fn compile_str_ast(
        &self,
        source: &str,
        file_name: &str,
    ) -> Result<ResolvedTree, CompilerError> {
        if self.verbose {
            eprintln!("[rumoca] Compiling source through AST: {}", file_name);
            eprintln!("[rumoca] Phase 1-2: Parsing and resolving...");
        }

        let mut session = Session::new(SessionConfig::default());
        self.load_required_source_roots(&mut session, source)?;
        self.load_local_compile_unit(&mut session, source, file_name)?;
        session
            .resolved()
            .map_err(|err| CompilerError::ResolveError(err.to_string()))
    }

    /// Compile Modelica source code through the flat-model stage only.
    pub fn compile_str_flat(
        &self,
        source: &str,
        file_name: &str,
    ) -> Result<FlatModel, CompilerError> {
        let model_name = self
            .model_name
            .as_ref()
            .ok_or(CompilerError::NoModelSpecified)?;

        if self.verbose {
            eprintln!("[rumoca] Compiling model through Flat: {}", model_name);
            eprintln!("[rumoca] Source file: {}", file_name);
            eprintln!("[rumoca] Phase 1-5: Parsing, resolving, and flattening...");
        }

        let mut session = Session::new(SessionConfig::default());
        self.load_required_source_roots(&mut session, source)?;
        self.load_local_compile_unit(&mut session, source, file_name)?;
        session
            .compile_model_flat_strict_reachable_uncached_with_recovery(model_name)
            .map_err(CompilerError::FlattenError)
    }

    /// Compile Modelica source code through DAE only.
    pub fn compile_str_dae(
        &self,
        source: &str,
        file_name: &str,
    ) -> Result<DaeCompilationResult, CompilerError> {
        let model_name = self
            .model_name
            .as_ref()
            .ok_or(CompilerError::NoModelSpecified)?;

        if self.verbose {
            eprintln!("[rumoca] Compiling model through DAE: {}", model_name);
            eprintln!("[rumoca] Source file: {}", file_name);
        }

        let mut session = Session::new(SessionConfig::default());
        self.load_required_source_roots(&mut session, source)?;

        if self.verbose {
            eprintln!("[rumoca] Phase 1-2: Parsing and resolving...");
        }
        self.load_local_compile_unit(&mut session, source, file_name)?;

        if self.verbose {
            eprintln!("[rumoca] Phase 3-6: Strict-reachable DAE compile...");
        }

        let result =
            match session.compile_model_dae_strict_reachable_uncached_with_recovery(model_name) {
                Ok(result) => result,
                Err(summary) => {
                    let report =
                        session.compile_model_strict_reachable_uncached_with_recovery(model_name);
                    let summary = if report.failures.is_empty() {
                        summary
                    } else {
                        report.failure_summary(usize::MAX)
                    };
                    return Err(CompilerError::CompileDiagnosticsError {
                        summary,
                        failures: report.failures,
                        source_map: report.source_map.map(Box::new),
                    });
                }
            };

        if self.verbose {
            eprintln!("[rumoca] DAE compilation complete.");
            eprintln!("[rumoca]   States: {}", result.dae.variables.states.len());
            eprintln!(
                "[rumoca]   Algebraics: {}",
                result.dae.variables.algebraics.len()
            );
            eprintln!(
                "[rumoca]   Parameters: {}",
                result.dae.variables.parameters.len()
            );
            eprintln!(
                "[rumoca]   Continuous equations (f_x): {}",
                result.dae.continuous.equations.len()
            );
            eprintln!("[rumoca]   Balance: {}", result.balance_detail.balance());
        }

        Ok(*result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[test]
    fn test_simple_model() {
        let source = r#"
            model Test
                Real x(start=0);
            equation
                der(x) = 1;
            end Test;
        "#;

        let result = Compiler::new().model("Test").compile_str(source, "test.mo");

        assert!(result.is_ok(), "Compilation failed: {:?}", result.err());
        let result = result.unwrap();
        assert_eq!(result.dae.variables.states.len(), 1);
    }

    #[test]
    fn test_dae_only_compile_returns_dae_without_full_artifacts() {
        let source = r#"
            model Test
                Real x(start=0);
            equation
                der(x) = 1;
            end Test;
        "#;

        let result = Compiler::new()
            .model("Test")
            .compile_str_dae(source, "test.mo")
            .expect("DAE-only compile should succeed");

        assert_eq!(result.dae.variables.states.len(), 1);
        assert_eq!(result.balance_detail.state_unknowns, 1);
    }

    #[test]
    fn test_dae_only_compile_preserves_structured_parse_diagnostics() {
        let err = Compiler::new()
            .model("Broken")
            .compile_str_dae("model Broken\n  Real x\nend Broken;\n", "Broken.mo")
            .expect_err("broken active document must fail DAE-only strict compile");

        match err {
            CompilerError::CompileDiagnosticsError {
                failures,
                source_map,
                ..
            } => {
                assert!(
                    source_map.is_some(),
                    "DAE-only compile must preserve the source map for CLI diagnostics"
                );
                assert!(
                    failures
                        .iter()
                        .any(|failure| failure.error_code.as_deref() == Some("EP001")
                            && failure.primary_label.is_some()),
                    "DAE-only compile must preserve structured parse failures: {failures:?}"
                );
            }
            other => panic!("expected structured compile diagnostics, got {other:?}"),
        }
    }

    #[test]
    fn test_typecheck_failure_carries_spanned_diagnostics_to_the_cli() {
        // Phase failures must reach the CLI as structured diagnostics with
        // the error's real source label, not a stringified summary anchored
        // at the class header ("phase failed").
        let source = r#"
            model BadDim
                constant Integer n = -1;
                Real x[n];
            equation
                der(x[1]) = 1;
            end BadDim;
        "#;

        let err = Compiler::new()
            .model("BadDim")
            .compile_str(source, "BadDim.mo")
            .expect_err("unevaluable dimensions must fail typecheck");

        match err {
            CompilerError::CompileDiagnosticsError {
                failures,
                source_map,
                ..
            } => {
                assert!(source_map.is_some(), "source map required for rendering");
                let failure = failures
                    .iter()
                    .find(|failure| failure.error_code.as_deref() == Some("ET004"))
                    .unwrap_or_else(|| panic!("expected an ET004 failure: {failures:?}"));
                let label = failure
                    .primary_label
                    .as_ref()
                    .expect("typecheck failure must carry its own source label");
                assert_ne!(
                    label.message.as_deref(),
                    Some("phase failed"),
                    "label must be the diagnostic's own span, not the class-header fallback"
                );
            }
            other => panic!("expected structured compile diagnostics, got {other:?}"),
        }
    }

    #[test]
    fn test_resolve_failure_does_not_cascade_into_later_phases() {
        // One user error must produce one diagnostic: an unresolved reference
        // is reported by resolve (ER002) and must not be re-reported by
        // flatten/ToDae (ED008 etc.) after the pipeline runs on anyway.
        let source = r#"
            model Cascade
                Real x;
            equation
                x = y + 1;
            end Cascade;
        "#;

        let err = Compiler::new()
            .model("Cascade")
            .compile_str(source, "Cascade.mo")
            .expect_err("unresolved reference must fail compile");

        match err {
            CompilerError::CompileDiagnosticsError { failures, .. } => {
                assert!(
                    failures
                        .iter()
                        .any(|failure| failure.error_code.as_deref() == Some("ER002")),
                    "resolve failure must be reported: {failures:?}"
                );
                assert!(
                    failures.iter().all(|failure| failure.phase.is_none()),
                    "no later phase may run (and re-report) after a resolve \
                     failure in the target's files: {failures:?}"
                );
            }
            other => panic!("expected structured compile diagnostics, got {other:?}"),
        }
    }

    #[test]
    fn test_compile_file_tolerates_path_spelling_variants() {
        // The directory scan yields the requested file under its own
        // spelling; a different but equivalent argument spelling (`./`
        // segment, bare relative name) must not register the file twice and
        // report EM001 "duplicate class" against itself.
        let temp = tempdir().expect("tempdir");
        let solo = temp.path().join("Solo.mo");
        fs::write(
            &solo,
            r#"
            model Solo
                Real x(start=0, fixed=true);
            equation
                der(x) = 1;
            end Solo;
            "#,
        )
        .expect("write solo");

        let dotted = format!("{}/./Solo.mo", temp.path().display());
        let result = Compiler::new().model("Solo").compile_file(&dotted);
        assert!(
            result.is_ok(),
            "equivalent path spelling must not self-duplicate: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_compile_file_skips_unreadable_sibling_sources() {
        // A non-UTF-8 sibling the user never referenced must not poison the
        // compile of an unrelated file in the same directory.
        let temp = tempdir().expect("tempdir");
        let solo = temp.path().join("Solo.mo");
        fs::write(
            &solo,
            r#"
            model Solo
                Real x(start=0, fixed=true);
            equation
                der(x) = 1;
            end Solo;
            "#,
        )
        .expect("write solo");
        fs::write(temp.path().join("junk.mo"), [0xff, 0xfe, 0xff]).expect("write junk");

        let result = Compiler::new()
            .model("Solo")
            .compile_file(&solo.to_string_lossy());
        assert!(
            result.is_ok(),
            "unreadable sibling must be skipped, not fatal: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_compile_file_loads_same_directory_siblings() {
        let temp = tempdir().expect("tempdir");
        let helper = temp.path().join("Helper.mo");
        let root = temp.path().join("Root.mo");
        fs::write(
            &helper,
            r#"
            model Helper
                Real x(start=0);
            equation
                der(x) = 1;
            end Helper;
            "#,
        )
        .expect("write helper");
        fs::write(
            &root,
            r#"
            model Root
                Helper h;
            end Root;
            "#,
        )
        .expect("write root");

        let result = Compiler::new()
            .model("Root")
            .compile_file(&root.to_string_lossy());

        assert!(
            result.is_ok(),
            "sibling files in the same directory must be part of the compile unit: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_compile_file_ignores_unrelated_sibling_parse_error() {
        let temp = tempdir().expect("tempdir");
        let helper = temp.path().join("Helper.mo");
        let broken = temp.path().join("Broken.mo");
        let root = temp.path().join("Root.mo");
        fs::write(
            &helper,
            r#"
            model Helper
                Real x(start=0);
            equation
                der(x) = 1;
            end Helper;
            "#,
        )
        .expect("write helper");
        fs::write(&broken, "model Broken\n  Real x\nend Broken;\n").expect("write broken");
        fs::write(
            &root,
            r#"
            model Root
                Helper h;
            end Root;
            "#,
        )
        .expect("write root");

        let result = Compiler::new()
            .model("Root")
            .compile_file(&root.to_string_lossy());

        assert!(
            result.is_ok(),
            "strict target compile must ignore unrelated sibling parse errors: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_compile_file_reports_required_sibling_parse_error() {
        let temp = tempdir().expect("tempdir");
        let helper = temp.path().join("Helper.mo");
        let root = temp.path().join("Root.mo");
        fs::write(&helper, "model Helper\n  Real x\nend Helper;\n").expect("write helper");
        fs::write(
            &root,
            r#"
            model Root
                Helper h;
            end Root;
            "#,
        )
        .expect("write root");

        let err = Compiler::new()
            .model("Root")
            .compile_file(&root.to_string_lossy())
            .expect_err("required broken sibling must fail strict compile");
        let message = err.to_string();
        assert!(
            message.contains(&helper.to_string_lossy().to_string()),
            "original helper parse error should be surfaced: {message}"
        );
        assert!(
            !message.contains("unresolved type reference"),
            "required broken sibling must not degrade into unresolved type errors: {message}"
        );
    }

    #[test]
    fn test_compile_file_reports_active_parse_error_via_compile_diagnostics() {
        let temp = tempdir().expect("tempdir");
        let broken = temp.path().join("Broken.mo");
        fs::write(&broken, "model Broken\n  Real x\nend Broken;\n").expect("write broken");

        let err = Compiler::new()
            .model("Broken")
            .compile_file(&broken.to_string_lossy())
            .expect_err("broken active document must fail strict compile");
        match err {
            CompilerError::CompileDiagnosticsError { failures, .. } => {
                assert!(
                    failures
                        .iter()
                        .any(|failure| failure.error_code.as_deref() == Some("EP001")),
                    "active parse errors must surface as structured parse diagnostics: {failures:?}"
                );
            }
            other => panic!("expected structured compile diagnostics, got {other:?}"),
        }
    }

    #[test]
    fn test_compile_file_loads_enclosing_package_tree() {
        let temp = tempdir().expect("tempdir");
        let pkg = temp.path().join("Pkg");
        let sub = pkg.join("Sub");
        fs::create_dir_all(&sub).expect("mkdir");
        fs::write(pkg.join("package.mo"), "package Pkg end Pkg;").expect("write package");
        fs::write(sub.join("package.mo"), "within Pkg; package Sub end Sub;")
            .expect("write sub package");
        fs::write(
            sub.join("Helper.mo"),
            r#"
            within Pkg.Sub;
            model Helper
                Real x(start=0);
            equation
                der(x) = 1;
            end Helper;
            "#,
        )
        .expect("write helper");
        let root = sub.join("Root.mo");
        fs::write(
            &root,
            r#"
            within Pkg.Sub;
            model Root
                Helper h;
            end Root;
            "#,
        )
        .expect("write root");
        fs::write(
            temp.path().join("Unrelated.mo"),
            "model Unrelated end Unrelated;",
        )
        .expect("write unrelated");

        let result = Compiler::new()
            .model("Pkg.Sub.Root")
            .compile_file(&root.to_string_lossy());

        assert!(
            result.is_ok(),
            "compile unit must include the enclosing package tree without unrelated parents: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_no_model_specified() {
        let source = "model Test end Test;";
        let result = Compiler::new().compile_str(source, "test.mo");
        assert!(matches!(result, Err(CompilerError::NoModelSpecified)));
    }

    #[test]
    fn test_to_json() {
        let source = r#"
            model Test
                Real x(start=0);
            equation
                der(x) = 1;
            end Test;
        "#;

        let result = Compiler::new()
            .model("Test")
            .compile_str(source, "test.mo")
            .unwrap();

        let json = result.to_json();
        assert!(json.is_ok());
        let value: serde_json::Value = serde_json::from_str(&json.unwrap()).unwrap();
        let obj = value.as_object().expect("DAE JSON should be an object");
        assert!(obj.contains_key("x"));
        assert!(obj.contains_key("f_x"));
        assert!(!obj.contains_key("y"));
        assert!(!obj.contains_key("p"));
        assert!(!obj.contains_key("z"));
        assert!(!obj.contains_key("m"));
        assert!(!obj.contains_key("f_z"));
        assert!(!obj.contains_key("f_m"));
        assert!(!obj.contains_key("f_c"));
        assert!(!obj.contains_key("relation"));
        assert!(!obj.contains_key("initial_equations"));
        assert!(!obj.contains_key("initial"));
        assert!(!obj.contains_key("functions"));
        let f_x = obj
            .get("f_x")
            .and_then(serde_json::Value::as_array)
            .expect("f_x should be an array");
        let first = f_x
            .first()
            .and_then(serde_json::Value::as_object)
            .expect("f_x entries should be objects");
        assert!(
            !first.contains_key("lhs"),
            "residual f_x equation must omit lhs"
        );
        assert!(
            first.contains_key("residual"),
            "residual f_x entry must include residual expression"
        );
        assert!(
            first.contains_key("origin"),
            "json should preserve origin traceability"
        );
        assert!(
            first.contains_key("span"),
            "json should preserve source span traceability"
        );
        assert!(!obj.contains_key("states"));
        assert!(!obj.contains_key("when_clauses"));
        assert!(!obj.contains_key("algorithms"));
        assert!(!obj.contains_key("initial_algorithms"));
    }

    #[test]
    fn test_to_json_hybrid_includes_runtime_partitions() {
        let source = r#"
            model Hybrid
                parameter Real k = 1;
                Real x(start=0);
                discrete Real zr(start=0);
                discrete Integer mi(start=0);
            initial equation
                x = 0;
            equation
                der(x) = k;
                when x > 0.5 then
                    zr = pre(zr) + 1;
                    mi = pre(mi) + 1;
                end when;
            end Hybrid;
        "#;

        let result = Compiler::new()
            .model("Hybrid")
            .compile_str(source, "hybrid.mo")
            .unwrap();

        let value: serde_json::Value = serde_json::from_str(&result.to_json().unwrap()).unwrap();
        let obj = value.as_object().expect("DAE JSON should be an object");
        for key in [
            "p", "x", "z", "m", "f_x", "f_z", "f_m", "f_c", "relation", "initial",
        ] {
            assert!(
                obj.contains_key(key),
                "hybrid runtime JSON should contain key `{key}`"
            );
        }
    }

    #[test]
    fn test_solve_ir_rendering_preserves_tensor_nodes_from_native_dae() {
        let source = r#"
            model TensorTargetDemo
              Real omega[2](start={0, 0});
              parameter Real J[2,2] = [2, 0; 0, 4];
              parameter Real tau[2] = {8, 20};
            equation
              J * der(omega) = tau;
            end TensorTargetDemo;
        "#;

        let result = Compiler::new()
            .model("TensorTargetDemo")
            .compile_str(source, "TensorTargetDemo.mo")
            .expect("tensor target demo should compile");

        let solve_json = result
            .to_ir_json(TemplateIr::Solve)
            .expect("Solve IR JSON should render");
        assert!(
            solve_json.contains("\"LinSolve\""),
            "Solve JSON should preserve the tensor LinSolve node from the native DAE: {solve_json}"
        );

        let rendered = result
            .render_template_str_with_name_and_ir(
                "{{ solve_blocks.continuous.derivative_rhs.tensor_node_count }}",
                "TensorTargetDemo",
                TemplateIr::Solve,
            )
            .expect("Solve template should render");
        assert_eq!(
            rendered.trim(),
            "1",
            "Solve templates should see tensor nodes before scalar fallback"
        );
    }

    #[test]
    fn test_render_template_exposes_orbit_algebraics_from_native_dae() {
        let source = r#"
            model SatelliteOrbit2D
              parameter Real mu = 398600.4418;
              parameter Real r0 = 7000;
              parameter Real v0 = sqrt(mu / r0);
              Real rx(start = r0, fixed = true);
              Real ry(start = 0, fixed = true);
              Real vx(start = 0, fixed = true);
              Real vy(start = v0, fixed = true);
              Real inv_r;
              Real inv_v2;
              Real inv_h;
              Real inv_energy;
              Real inv_a;
              Real inv_rv;
              Real inv_ex;
              Real inv_ey;
              Real inv_ecc;
            equation
              der(rx) = vx;
              der(ry) = vy;
              inv_r = sqrt(rx * rx + ry * ry);
              inv_v2 = vx * vx + vy * vy;
              inv_h = rx * vy - ry * vx;
              inv_energy = 0.5 * inv_v2 - mu / inv_r;
              inv_a = 1 / (2 / inv_r - inv_v2 / mu);
              inv_rv = rx * vx + ry * vy;
              inv_ex = ((inv_v2 - mu / inv_r) * rx - inv_rv * vx) / mu;
              inv_ey = ((inv_v2 - mu / inv_r) * ry - inv_rv * vy) / mu;
              inv_ecc = sqrt(inv_ex * inv_ex + inv_ey * inv_ey);
              der(vx) = -mu * rx / (inv_r ^ 3);
              der(vy) = -mu * ry / (inv_r ^ 3);
            end SatelliteOrbit2D;
        "#;

        let result = Compiler::new()
            .model("SatelliteOrbit2D")
            .compile_str(source, "orbit.mo")
            .expect("compilation should succeed");
        let rendered = result
            .render_template_str("{% for name, comp in dae.y | items %}{{ name }}\n{% endfor %}")
            .expect("template render should succeed");

        for expected in [
            "inv_r",
            "inv_v2",
            "inv_h",
            "inv_energy",
            "inv_a",
            "inv_rv",
            "inv_ex",
            "inv_ey",
            "inv_ecc",
        ] {
            assert!(
                rendered.lines().any(|line| line.trim() == expected),
                "expected algebraic `{expected}` in template output; got:\n{rendered}"
            );
        }
    }

    #[test]
    fn test_dae_template_rendering_does_not_lower_solve_ir() {
        let source = r#"
            model DaeOnlyTemplate
              parameter Real p = 2;
              Real x[2](start = {1, 2});
            equation
              der(x) = {-p * x[1], -p * x[2]};
            end DaeOnlyTemplate;
        "#;

        let result = Compiler::new()
            .model("DaeOnlyTemplate")
            .compile_str(source, "DaeOnlyTemplate.mo")
            .expect("compilation should succeed");
        let rendered = result
            .render_template_str_with_name_and_ir(
                "{{ dae.f_x | length }} {{ dae.x.x.dims | join(\",\") }}",
                "DaeOnlyTemplate",
                TemplateIr::Dae,
            )
            .expect("DAE template should render from native DAE context");

        assert_eq!(rendered.trim(), "1 2");
    }

    #[test]
    fn test_strict_reachable_requested_success_ignores_unreachable_failures() {
        let source = r#"
            package P
              model Good
                Real x(start=0);
              equation
                der(x) = 1;
              end Good;

              model BadNeedsInner
                outer Real shared;
              equation
                shared = 1;
              end BadNeedsInner;
            end P;
        "#;

        let result = Compiler::new()
            .model("P.Good")
            .compile_str(source, "test.mo");
        assert!(
            result.is_ok(),
            "Compilation failed unexpectedly: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_strict_reachable_requested_failure_excludes_unreachable_context() {
        let source = r#"
            package P
              model Good
                Real x(start=0);
              equation
                der(x) = 1;
              end Good;

              model BadNeedsInner
                outer Real shared;
              equation
                shared = 1;
              end BadNeedsInner;

              model BadNeedsInner2
                outer Real shared2;
              equation
                shared2 = 2;
              end BadNeedsInner2;
            end P;
        "#;

        let err = Compiler::new()
            .model("P.BadNeedsInner")
            .compile_str(source, "test.mo")
            .expect_err("Requested model should fail");
        let msg = err.to_string();
        assert!(!msg.contains("Related failures"));
        assert!(!msg.contains("P.BadNeedsInner2"));
    }

    #[test]
    fn test_strict_reachable_fails_when_instantiated_dependency_fails() {
        let source = r#"
            package P
              model BadDep
                outer Real shared;
              equation
                shared = 1;
              end BadDep;

              model Root
                BadDep dep;
                Real x(start=0);
              equation
                der(x) = 1;
              end Root;
            end P;
        "#;

        let err = Compiler::new()
            .model("P.Root")
            .compile_str(source, "test.mo")
            .expect_err("reachable dependency failure must fail strict compile");
        let msg = err.to_string();
        assert!(
            msg.contains("requires inner declarations"),
            "actual message: {msg}"
        );
        assert!(msg.contains("shared"), "actual message: {msg}");
    }
}
