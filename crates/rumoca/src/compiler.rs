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

use rumoca_session::compile::{
    Dae, FailedPhase, FlatModel, PhaseResult, ResolvedTree, Session, SessionConfig, SourceRootKind,
};
use rumoca_session::parsing::collect_compile_unit_source_files;
use rumoca_session::runtime::{
    dae_balance, dae_is_balanced, dae_to_template_json, prepare_dae_for_template_codegen,
    render_dae_template, render_dae_template_with_json, render_dae_template_with_json_and_name,
    render_dae_template_with_name,
};
use rumoca_session::source_roots::{
    PackageLayoutError, canonical_path_key, parse_source_root_with_cache, plan_source_root_loads,
    referenced_unloaded_source_root_paths, render_source_root_status_message,
    resolve_source_root_cache_dir, source_root_source_set_key,
};
use serde_json::{Map, Value};

use crate::error::CompilerError;

fn as_object_mut(value: &mut Value) -> Option<&mut Map<String, Value>> {
    value.as_object_mut()
}

fn expr_var_name(expr: &Value) -> Option<String> {
    let obj = expr.as_object()?;
    if let Some(vr) = obj.get("VarRef").and_then(Value::as_object)
        && let Some(n) = vr.get("name").and_then(Value::as_str)
    {
        return Some(n.to_string());
    }
    None
}

fn lhs_var_name(lhs: &Value) -> Option<String> {
    if let Some(obj) = lhs.as_object()
        && let Some(vr) = obj.get("VarRef")
    {
        return expr_var_name(vr);
    }
    if let Some(s) = lhs.as_str() {
        return Some(s.to_string());
    }
    expr_var_name(lhs)
}

fn extract_residual_assignment_expr(expr: &Value, target: &str) -> Option<Value> {
    let obj = expr.as_object()?;
    let bin = obj.get("Binary")?.as_object()?;
    let lhs = bin.get("lhs")?;
    let rhs = bin.get("rhs")?;
    let op = bin.get("op")?.as_object()?;
    if !op.contains_key("Sub") {
        return None;
    }

    if expr_var_name(lhs).is_some_and(|n| n == target) {
        return Some(rhs.clone());
    }
    if expr_var_name(rhs).is_some_and(|n| n == target) {
        let mut unary = Map::new();
        unary.insert("op".to_string(), Value::String("-".to_string()));
        unary.insert("arg".to_string(), lhs.clone());
        let mut wrap = Map::new();
        wrap.insert("Unary".to_string(), Value::Object(unary));
        return Some(Value::Object(wrap));
    }
    None
}

fn extract_direct_residual_assignment_expr(expr: &Value, target: &str) -> Option<Value> {
    let obj = expr.as_object()?;
    let bin = obj.get("Binary")?.as_object()?;
    let lhs = bin.get("lhs")?;
    let rhs = bin.get("rhs")?;
    let op = bin.get("op")?.as_object()?;
    if !op.contains_key("Sub") {
        return None;
    }
    if expr_var_name(lhs).is_some_and(|n| n == target) {
        return Some(rhs.clone());
    }
    None
}

fn collect_observable_expr_candidates_from_native(native: &Value, target: &str) -> Vec<Value> {
    let Some(obj) = native.as_object() else {
        return Vec::new();
    };
    let mut out: Vec<Value> = Vec::new();

    for key in ["f_z", "f_m", "f_c"] {
        if let Some(rows) = obj.get(key).and_then(Value::as_array) {
            out.extend(
                rows.iter()
                    .filter_map(Value::as_object)
                    .filter(|row_obj| {
                        row_obj
                            .get("lhs")
                            .and_then(lhs_var_name)
                            .is_some_and(|name| name == target)
                    })
                    .filter_map(|row_obj| row_obj.get("rhs").cloned()),
            );
        }
    }

    for key in ["f_x", "fx"] {
        if let Some(rows) = obj.get(key).and_then(Value::as_array) {
            out.extend(
                rows.iter()
                    .filter_map(Value::as_object)
                    .filter_map(|row_obj| {
                        row_obj
                            .get("residual")
                            .or_else(|| row_obj.get("rhs"))
                            .and_then(|expr| extract_residual_assignment_expr(expr, target))
                    }),
            );
        }
    }

    out
}

fn collect_direct_assignment_expr_candidates_from_native(native: &Value, target: &str) -> Vec<Value> {
    let Some(obj) = native.as_object() else {
        return Vec::new();
    };
    let mut out: Vec<Value> = Vec::new();

    for key in ["f_z", "f_m", "f_c"] {
        if let Some(rows) = obj.get(key).and_then(Value::as_array) {
            out.extend(
                rows.iter()
                    .filter_map(Value::as_object)
                    .filter(|row_obj| {
                        row_obj
                            .get("lhs")
                            .and_then(lhs_var_name)
                            .is_some_and(|name| name == target)
                    })
                    .filter_map(|row_obj| row_obj.get("rhs").cloned()),
            );
        }
    }

    for key in ["f_x", "fx"] {
        if let Some(rows) = obj.get(key).and_then(Value::as_array) {
            out.extend(
                rows.iter()
                    .filter_map(Value::as_object)
                    .filter_map(|row_obj| {
                        row_obj
                            .get("residual")
                            .or_else(|| row_obj.get("rhs"))
                            .and_then(|expr| extract_direct_residual_assignment_expr(expr, target))
                    }),
            );
        }
    }

    out
}

fn expr_complexity(expr: &Value) -> usize {
    match expr {
        Value::Object(map) => {
            1 + map
                .values()
                .map(expr_complexity)
                .fold(0usize, |acc, n| acc.saturating_add(n))
        }
        Value::Array(items) => {
            1 + items
                .iter()
                .map(expr_complexity)
                .fold(0usize, |acc, n| acc.saturating_add(n))
        }
        _ => 1,
    }
}

fn is_simple_alias_expr(expr: &Value) -> bool {
    let is_var_like = |v: &Value| {
        v.as_object()
            .is_some_and(|m| m.contains_key("VarRef") || m.contains_key("ComponentReference"))
    };
    if is_var_like(expr) {
        return true;
    }
    let Some(obj) = expr.as_object() else {
        return false;
    };
    let Some(unary) = obj.get("Unary").and_then(Value::as_object) else {
        return false;
    };
    unary.get("arg").is_some_and(is_var_like)
}

fn find_observable_expr_from_native(native: &Value, target: &str) -> Option<Value> {
    let mut candidates = collect_direct_assignment_expr_candidates_from_native(native, target);
    if candidates.is_empty() {
        candidates = collect_observable_expr_candidates_from_native(native, target);
    }
    if candidates.is_empty() {
        return None;
    }
    if let Some(best_alias) = candidates
        .iter()
        .filter(|expr| is_simple_alias_expr(expr))
        .min_by_key(|expr| expr_complexity(expr))
        .cloned()
    {
        return Some(best_alias);
    }
    candidates.into_iter().max_by_key(|expr| {
        let non_alias = if is_simple_alias_expr(expr) {
            0usize
        } else {
            1usize
        };
        (non_alias, expr_complexity(expr))
    })
}

fn find_bridge_expr_from_native(native: &Value, target: &str) -> Option<Value> {
    let mut candidates = collect_direct_assignment_expr_candidates_from_native(native, target);
    if candidates.is_empty() {
        candidates = collect_observable_expr_candidates_from_native(native, target);
    }
    if candidates.is_empty() {
        return None;
    }
    candidates.into_iter().min_by_key(|expr| {
        let alias_penalty = if is_simple_alias_expr(expr) {
            0usize
        } else {
            1usize
        };
        (alias_penalty, expr_complexity(expr))
    })
}

fn component_ref_name(expr: &Value) -> Option<String> {
    let obj = expr.as_object()?;
    let cr = obj.get("ComponentReference")?.as_object()?;
    let parts = cr.get("parts")?.as_array()?;
    let mut segs: Vec<String> = Vec::new();
    for part in parts {
        let part_obj = part.as_object()?;
        let ident = part_obj.get("ident")?.as_object()?;
        let text = ident.get("text")?.as_str()?;
        segs.push(text.to_string());
    }
    if segs.is_empty() {
        return None;
    }
    Some(segs.join("."))
}

fn collect_prepared_symbol_names(prepared_obj: &Map<String, Value>) -> HashSet<String> {
    let mut out = HashSet::new();
    for key in [
        "p",
        "constants",
        "cp",
        "x",
        "y",
        "z",
        "m",
        "w",
        "u",
        "x_dot_alias",
    ] {
        if let Some(map) = prepared_obj.get(key).and_then(Value::as_object) {
            out.extend(map.keys().cloned());
        }
    }
    out
}

fn rewrite_observable_expr_with_native_aliases(
    native_json: &Value,
    expr: &Value,
    prepared_symbols: &HashSet<String>,
    visiting: &mut HashSet<String>,
    depth: usize,
) -> Value {
    if depth > 24 {
        return expr.clone();
    }

        if let Some(obj) = expr.as_object() {
        if let Some(vr) = obj.get("VarRef").and_then(Value::as_object)
            && let Some(name) = vr.get("name").and_then(Value::as_str)
            && !prepared_symbols.contains(name)
            && !visiting.contains(name)
            && let Some(alias_expr) = find_bridge_expr_from_native(native_json, name)
        {
            visiting.insert(name.to_string());
            let rewritten = rewrite_observable_expr_with_native_aliases(
                native_json,
                &alias_expr,
                prepared_symbols,
                visiting,
                depth + 1,
            );
            visiting.remove(name);
            return rewritten;
        }

        if let Some(name) = component_ref_name(expr)
            && !prepared_symbols.contains(&name)
            && !visiting.contains(&name)
            && let Some(alias_expr) = find_bridge_expr_from_native(native_json, &name)
        {
            visiting.insert(name.clone());
            let rewritten = rewrite_observable_expr_with_native_aliases(
                native_json,
                &alias_expr,
                prepared_symbols,
                visiting,
                depth + 1,
            );
            visiting.remove(&name);
            return rewritten;
        }

        let mut out = Map::new();
        for (k, v) in obj {
            out.insert(
                k.clone(),
                rewrite_observable_expr_with_native_aliases(
                    native_json,
                    v,
                    prepared_symbols,
                    visiting,
                    depth + 1,
                ),
            );
        }
        return Value::Object(out);
    }

    if let Some(arr) = expr.as_array() {
        return Value::Array(
            arr.iter()
                .map(|v| {
                    rewrite_observable_expr_with_native_aliases(
                        native_json,
                        v,
                        prepared_symbols,
                        visiting,
                        depth + 1,
                    )
                })
                .collect(),
        );
    }

    expr.clone()
}

fn augment_prepared_with_native_observables(
    native_json: &Value,
    prepared_json: &mut Value,
) -> Option<usize> {
    let native_obj = native_json.as_object()?;
    let prepared_obj = as_object_mut(prepared_json)?;
    let prepared_y = prepared_obj.get("y").and_then(Value::as_object);
    let prepared_w = prepared_obj.get("w").and_then(Value::as_object);
    let prepared_symbols = collect_prepared_symbol_names(prepared_obj);

    let mut observables: Vec<Value> = Vec::new();
    for (section_name, causality, native_entries) in [
        ("y", "local", native_obj.get("y").and_then(Value::as_object)),
        ("w", "output", native_obj.get("w").and_then(Value::as_object)),
    ] {
        let Some(native_entries) = native_entries else {
            continue;
        };
        for (name, comp) in native_entries {
            if section_name == "w" && (name.contains('.') || name.contains('[')) {
                continue;
            }
            if prepared_y.is_some_and(|m| m.contains_key(name))
                || prepared_w.is_some_and(|m| m.contains_key(name))
            {
                continue;
            }
            let Some(expr_raw) = find_observable_expr_from_native(native_json, name) else {
                continue;
            };
            let mut visiting = HashSet::new();
            let expr = rewrite_observable_expr_with_native_aliases(
                native_json,
                &expr_raw,
                &prepared_symbols,
                &mut visiting,
                0,
            );
            let mut entry = Map::new();
            entry.insert("name".to_string(), Value::String(name.clone()));
            entry.insert("dims".to_string(), Value::Array(Vec::new()));
            entry.insert("expr".to_string(), expr);
            entry.insert(
                "causality".to_string(),
                Value::String(causality.to_string()),
            );
            entry.insert(
                "section".to_string(),
                Value::String(section_name.to_string()),
            );
            entry.insert("description".to_string(), Value::Null);
            entry.insert("nominal".to_string(), Value::Null);
            entry.insert("min".to_string(), Value::Null);
            entry.insert("max".to_string(), Value::Null);
            entry.insert("fixed".to_string(), Value::Bool(false));
            entry.insert("is_tunable".to_string(), Value::Bool(false));
            let start = comp
                .as_object()
                .and_then(|comp_obj| comp_obj.get("start"))
                .cloned()
                .unwrap_or(Value::Null);
            let unit = comp
                .as_object()
                .and_then(|comp_obj| {
                    comp_obj
                        .get("unit")
                        .or_else(|| comp_obj.get("displayUnit"))
                        .or_else(|| comp_obj.get("display_unit"))
                })
                .cloned()
                .unwrap_or(Value::Null);
            entry.insert("start".to_string(), start);
            entry.insert("unit".to_string(), unit);
            observables.push(Value::Object(entry));
        }
    }

    if observables.is_empty() {
        return Some(0);
    }
    let n = observables.len();
    prepared_obj.insert(
        "__rumoca_observables".to_string(),
        Value::Array(observables),
    );
    Some(n)
}

/// Result of a successful compilation.
#[derive(Debug)]
pub struct CompilationResult {
    /// The DAE representation.
    pub dae: Dae,
    /// The flat model (intermediate).
    pub flat: FlatModel,
    /// The resolved tree (intermediate, before instantiation and typechecking).
    pub resolved: ResolvedTree,
}

impl CompilationResult {
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

    /// Render a structurally prepared DAE using a template file.
    ///
    /// This runs the same template-preparation pass used by the simulation
    /// pipeline (without solver-only artifacts), then renders against the
    /// prepared DAE.
    pub fn render_template_prepared(
        &self,
        template_path: &str,
        scalarize: bool,
    ) -> Result<String, CompilerError> {
        let template_content = fs::read_to_string(template_path)
            .map_err(|e| CompilerError::io_error(template_path, e.to_string()))?;

        self.render_template_str_prepared(&template_content, scalarize)
    }

    /// Render the DAE using a template string.
    pub fn render_template_str(&self, template: &str) -> Result<String, CompilerError> {
        // Use the codegen module's render function which sets up the context properly
        // with the DAE as `dae` and includes custom filters/functions
        render_dae_template(&self.dae, template).map_err(CompilerError::TemplateError)
    }

    /// Render the DAE using a template string with an explicit model name.
    ///
    /// The model name is exposed as `model_name` in the template context.
    pub fn render_template_str_with_name(
        &self,
        template: &str,
        model_name: &str,
    ) -> Result<String, CompilerError> {
        render_dae_template_with_name(&self.dae, template, model_name)
            .map_err(CompilerError::TemplateError)
    }

    /// Render a structurally prepared DAE using a template string.
    pub fn render_template_str_prepared(
        &self,
        template: &str,
        scalarize: bool,
    ) -> Result<String, CompilerError> {
        let prepared = prepare_dae_for_template_codegen(&self.dae, scalarize)
            .map_err(CompilerError::TemplateError)?;
        let native_json = dae_to_template_json(&self.dae);
        let mut prepared_json = dae_to_template_json(&prepared);
        let _ = augment_prepared_with_native_observables(&native_json, &mut prepared_json);
        render_dae_template_with_json(&prepared_json, template)
            .map_err(CompilerError::TemplateError)
    }

    /// Render a structurally prepared DAE using a template string with model name.
    pub fn render_template_str_prepared_with_name(
        &self,
        template: &str,
        model_name: &str,
        scalarize: bool,
    ) -> Result<String, CompilerError> {
        let prepared = prepare_dae_for_template_codegen(&self.dae, scalarize)
            .map_err(CompilerError::TemplateError)?;
        let native_json = dae_to_template_json(&self.dae);
        let mut prepared_json = dae_to_template_json(&prepared);
        let _ = augment_prepared_with_native_observables(&native_json, &mut prepared_json);
        render_dae_template_with_json_and_name(&prepared_json, template, model_name)
            .map_err(CompilerError::TemplateError)
    }

    /// Equation balance (equations - unknowns).
    pub fn balance(&self) -> i64 {
        dae_balance(&self.dae)
    }

    /// Whether equation/unknown balance is exact.
    pub fn is_balanced(&self) -> bool {
        dae_is_balanced(&self.dae)
    }

    /// Convert the DAE to JSON.
    pub fn to_json(&self) -> Result<String, CompilerError> {
        let mut p = self.dae.parameters.clone();
        // MLS Appendix B groups parameters and constants together in p.
        for (name, var) in &self.dae.constants {
            p.entry(name.clone()).or_insert_with(|| var.clone());
        }

        let f_x = Self::residuals_to_minimal_json(&self.dae.f_x)?;
        let f_z = Self::assignments_to_minimal_json(&self.dae.f_z)?;
        let f_m = Self::assignments_to_minimal_json(&self.dae.f_m)?;
        let f_c = Self::assignments_to_minimal_json(&self.dae.f_c)?;
        let initial = Self::initial_to_minimal_json(&self.dae.initial_equations)?;

        let mut canonical = Map::new();
        Self::push_nonempty(&mut canonical, "p", &p)?;
        Self::push_nonempty(&mut canonical, "x", &self.dae.states)?;
        Self::push_nonempty(&mut canonical, "y", &self.dae.algebraics)?;
        Self::push_nonempty(&mut canonical, "z", &self.dae.discrete_reals)?;
        Self::push_nonempty(&mut canonical, "m", &self.dae.discrete_valued)?;
        Self::push_nonempty(&mut canonical, "f_x", &f_x)?;
        Self::push_nonempty(&mut canonical, "f_z", &f_z)?;
        Self::push_nonempty(&mut canonical, "f_m", &f_m)?;
        Self::push_nonempty(&mut canonical, "f_c", &f_c)?;
        Self::push_nonempty(&mut canonical, "relation", &self.dae.relation)?;
        Self::push_nonempty(&mut canonical, "initial", &initial)?;
        Self::push_nonempty(&mut canonical, "functions", &self.dae.functions)?;

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
                    source_map: package_layout_error.source_map().clone(),
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
        for sibling in files {
            if sibling == path {
                continue;
            }
            let sibling_path = sibling.to_string_lossy().to_string();
            let sibling_source = fs::read_to_string(&sibling)
                .map_err(|e| CompilerError::io_error(&sibling_path, e.to_string()))?;
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

    /// Compile a Modelica file from a Path.
    pub fn compile_path(&self, path: &Path) -> Result<CompilationResult, CompilerError> {
        let path_str = path.to_string_lossy().to_string();
        self.compile_file(&path_str)
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
                        source_map: report.source_map,
                    });
                }
                *result
            }
            Some(PhaseResult::NeedsInner { .. }) => {
                return Err(CompilerError::InstantiateError(failure_summary));
            }
            Some(PhaseResult::Failed { phase, .. }) => {
                let err = match phase {
                    FailedPhase::Instantiate => CompilerError::InstantiateError(failure_summary),
                    FailedPhase::Typecheck => CompilerError::TypeCheckError(failure_summary),
                    FailedPhase::Flatten => CompilerError::FlattenError(failure_summary),
                    FailedPhase::ToDae => CompilerError::ToDaeError(failure_summary),
                };
                return Err(err);
            }
            None => {
                return Err(CompilerError::CompileDiagnosticsError {
                    summary: failure_summary,
                    failures: report.failures,
                    source_map: report.source_map,
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
            eprintln!("[rumoca]   States: {}", result.dae.states.len());
            eprintln!("[rumoca]   Algebraics: {}", result.dae.algebraics.len());
            eprintln!("[rumoca]   Parameters: {}", result.dae.parameters.len());
            eprintln!(
                "[rumoca]   Continuous equations (f_x): {}",
                result.dae.f_x.len()
            );
            eprintln!("[rumoca]   Balance: {}", dae_balance(&result.dae));
        }

        Ok(CompilationResult {
            dae: result.dae,
            flat: result.flat,
            resolved,
        })
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
        assert_eq!(result.dae.states.len(), 1);
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
    fn test_render_template_prepared_retains_orbit_observables() {
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
            .render_template_str_prepared(
                "{% for o in dae.__rumoca_observables %}{{ o.name }}\n{% endfor %}",
                true,
            )
            .expect("prepared template render should succeed");

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
                "expected observable `{expected}` in prepared template output; got:\n{rendered}"
            );
        }
    }

    #[test]
    fn test_render_template_prepared_with_name_retains_orbit_observables() {
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
            .render_template_str_prepared_with_name(
                "{% for o in dae.__rumoca_observables %}{{ o.name }}\n{% endfor %}",
                "SatelliteOrbit2D",
                true,
            )
            .expect("prepared named template render should succeed");

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
                "expected observable `{expected}` in prepared named template output; got:\n{rendered}"
            );
        }
    }

    #[test]
    fn test_render_fmi2_model_description_with_name_restores_output_observables() {
        let source = r#"
            model OutputAlias
              Real x(start = 2, fixed = true);
              output Real y;
              output Real negY;
            equation
              der(x) = -x;
              y = x;
              negY = -y;
            end OutputAlias;
        "#;

        let result = Compiler::new()
            .model("OutputAlias")
            .compile_str(source, "output_alias.mo")
            .expect("compilation should succeed");
        let rendered = result
            .render_template_str_prepared_with_name(
                rumoca_phase_codegen::templates::FMI2_MODEL_DESCRIPTION,
                "OutputAlias",
                true,
            )
            .expect("prepared named FMI2 modelDescription render should succeed");

        assert!(
            rendered.contains(r#"name="y""#),
            "expected restored output alias `y` in FMI2 modelDescription; got:\n{rendered}"
        );
        assert!(
            rendered.contains(r#"name="negY""#),
            "expected restored output alias `negY` in FMI2 modelDescription; got:\n{rendered}"
        );
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
