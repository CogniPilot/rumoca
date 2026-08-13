//! eFMU manifest packaging plan for the `galec`/`galec-production` targets.
//!
//! [`GalecPlan`] pre-renders the GALEC code files and pre-computes all eFMU
//! identifiers and timestamps exactly once. [`GalecPlan::template_ctx`] is the
//! per-file context factory that the declarative checksum-web render closure in
//! `rumoca::target_manifest` drives to produce manifest XML.

use std::collections::{BTreeMap, BTreeSet};

use minijinja::Error;
use serde_json::{Value, json};
use sha1::{Digest, Sha1};
use uuid::Uuid;

use crate::galec::{DataRole, Galec, GalecExpr, GalecLiteral};
use crate::render::{render, render_c_header, render_c_source};

// ─── Public utility ────────────────────────────────────────────────────────

/// SHA-1 hex digest of `bytes` (lowercase, no separators).
pub fn sha1_hex(bytes: &[u8]) -> String {
    format!("{:x}", Sha1::digest(bytes))
}

// ─── Private helpers ────────────────────────────────────────────────────────

fn new_uuid() -> String {
    format!("{{{}}}", Uuid::new_v4())
}

fn utc_now() -> String {
    // XSD pattern `[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z` forbids
    // fractional seconds, so truncate to whole seconds before formatting.
    let now = time::OffsetDateTime::now_utc()
        .replace_nanosecond(0)
        .unwrap_or(time::OffsetDateTime::UNIX_EPOCH);
    use time::format_description::well_known::Rfc3339;
    now.format(&Rfc3339)
        .unwrap_or_else(|_| "1970-01-01T00:00:00Z".to_string())
}

fn format_f64(v: f64) -> String {
    let s = format!("{v}");
    if s.contains('.') || s.contains('e') {
        s
    } else {
        format!("{s}.0")
    }
}

fn eval_start(expr: &GalecExpr) -> String {
    match expr {
        GalecExpr::Literal(GalecLiteral::Real(v)) => format_f64(*v),
        GalecExpr::Literal(GalecLiteral::Integer(v)) => v.to_string(),
        GalecExpr::Literal(GalecLiteral::Boolean(v)) => {
            if *v { "true" } else { "false" }.to_string()
        }
        _ => String::from("0"),
    }
}

fn block_causality(role: &DataRole) -> &'static str {
    match role {
        DataRole::Input => "input",
        DataRole::Output => "output",
        DataRole::State => "state",
        DataRole::Constant => "constant",
        DataRole::TunableParameter => "tunableParameter",
        DataRole::DependentParameter => "dependentParameter",
    }
}

fn literal_kind(ty: &GalecLiteral) -> &'static str {
    match ty {
        GalecLiteral::Real(_) => "Real",
        GalecLiteral::Integer(_) => "Integer",
        GalecLiteral::Boolean(_) => "Boolean",
    }
}

fn typedef_id(ty: &GalecLiteral) -> &'static str {
    match ty {
        GalecLiteral::Real(_) => "TD_F64",
        GalecLiteral::Integer(_) => "TD_I32",
        GalecLiteral::Boolean(_) => "TD_BOOL",
    }
}

// ─── Per-variable metadata ──────────────────────────────────────────────────

/// Compact pre-computed view of one GALEC variable for manifest contexts.
struct Var {
    index: usize,
    name: String,
    causality: &'static str,
    kind: &'static str,
    td: &'static str,
    dims: Vec<usize>,
    start: Option<String>,
}

impl Var {
    fn from_galec(v: &crate::galec::GalecVariable, index: usize) -> Self {
        Var {
            index,
            name: v.name.clone(),
            causality: block_causality(&v.role),
            kind: literal_kind(&v.ty),
            td: typedef_id(&v.ty),
            dims: v.dims.clone(),
            start: v.start.as_ref().map(eval_start),
        }
    }

    fn ac_id(&self) -> String {
        format!("V_{}", self.index)
    }

    fn comp_id(&self) -> String {
        format!("CO_{}", self.index)
    }

    fn to_ac_var(&self) -> Value {
        json!({
            "kind": self.kind,
            "id": self.ac_id(),
            "name": self.name,
            "description": null,
            "block_causality": self.causality,
            "dimensions": self.dims,
            "start": self.start,
            "annotations": [],
            "unit_ref_id": null,
            "relative_quantity": false,
            "min": null,
            "max": null,
            "nominal": null,
        })
    }

    fn to_component(&self) -> Value {
        json!({
            "id": self.comp_id(),
            "name": self.name,
            "type_def_ref_id": self.td,
            "dimensions": self.dims,
            "pointer": false,
        })
    }

    fn to_data_ref(&self) -> Value {
        json!({
            "manifest_reference_ref_id": "MR_AC",
            "foreign_ref_id": self.ac_id(),
            "formal_parameter_ref_id": "FP_DOSTEP",
            "component_identifier": self.name,
        })
    }
}

// ─── Plan ──────────────────────────────────────────────────────────────────

/// Packaging plan for one GALEC eFMU export invocation.
///
/// Pre-renders all code files and pins all manifest identifiers + timestamps
/// so every template in the checksum-web DAG sees a consistent, deterministic
/// context. Construct with [`GalecPlan::new_ac`] for Algorithm Code only or
/// [`GalecPlan::new_production`] for the AC + Production Code pair.
pub struct GalecPlan {
    alg: String,
    c_header: String,
    c_source: String,
    model_name: String,
    /// Original dotted model name (e.g. `Pkg.Inner`) for `Content/@name`.
    /// Distinct from `model_name` which uses underscores for code identifiers.
    source_name: String,
    generation_tool: String,
    vars: Vec<Var>,
    prev_vars: Vec<(String, &'static str)>,
    generated_at: String,
    ac_manifest_id: String,
    content_id: String,
    pc_manifest_id: String,
    with_production: bool,
}

impl GalecPlan {
    /// Build an Algorithm Code-only plan (for the `galec` target).
    ///
    /// `source_name` is the original dotted Modelica name (e.g. `Pkg.Inner`)
    /// used for `Content/@name`; `galec.name` is the underscore identifier
    /// used for code and the AC manifest.
    pub fn new_ac(galec: &Galec, source_name: &str, generation_tool: &str) -> Result<Self, Error> {
        let alg = render(galec)?;
        Ok(Self::build(
            galec,
            alg,
            String::new(),
            String::new(),
            false,
            source_name,
            generation_tool,
        ))
    }

    /// Build an Algorithm Code + Production Code plan (for `galec-production`).
    pub fn new_production(
        galec: &Galec,
        source_name: &str,
        generation_tool: &str,
    ) -> Result<Self, Error> {
        let alg = render(galec)?;
        let c_header = render_c_header(galec)?;
        let c_source = render_c_source(galec)?;
        Ok(Self::build(
            galec,
            alg,
            c_header,
            c_source,
            true,
            source_name,
            generation_tool,
        ))
    }

    fn build(
        galec: &Galec,
        alg: String,
        c_header: String,
        c_source: String,
        with_production: bool,
        source_name: &str,
        generation_tool: &str,
    ) -> Self {
        GalecPlan {
            alg,
            c_header,
            c_source,
            model_name: galec.name.clone(),
            source_name: source_name.to_string(),
            generation_tool: generation_tool.to_string(),
            vars: galec
                .variables
                .iter()
                .enumerate()
                .map(|(i, v)| Var::from_galec(v, i))
                .collect(),
            prev_vars: collect_prev_refs(galec),
            generated_at: utc_now(),
            ac_manifest_id: new_uuid(),
            content_id: new_uuid(),
            pc_manifest_id: new_uuid(),
            with_production,
        }
    }

    /// Pre-rendered GALEC `.alg` source text (passthrough in `model.alg.jinja`).
    pub fn alg_text(&self) -> &str {
        &self.alg
    }

    /// Pre-rendered C header source (passthrough in `model.h.jinja`; empty for AC-only).
    pub fn c_header(&self) -> &str {
        &self.c_header
    }

    /// Pre-rendered C source (passthrough in `model.c.jinja`; empty for AC-only).
    pub fn c_source(&self) -> &str {
        &self.c_source
    }

    /// Build the serde-serializable `ctx` value for one template + checksum map.
    ///
    /// Returns `null` for passthrough templates (`.alg`, `.h`, `.c`) since their
    /// content is served by the top-level `galec_*` keys in the render closure.
    /// Manifest templates receive the full eFMI context tree under the `ctx` key.
    pub fn template_ctx(&self, template: &str, checksums: &BTreeMap<String, String>) -> Value {
        match template {
            "model.alg.jinja" | "model.h.jinja" | "model.c.jinja" => json!(null),
            "manifest.xml.jinja" | "ac_manifest.xml.jinja" => self.ac_ctx(checksums),
            "__content.xml.jinja" => self.content_ctx(checksums),
            "pc_manifest.xml.jinja" => self.pc_ctx(checksums),
            _ => json!(null),
        }
    }

    fn attrs(&self, id: &str, name: &str) -> Value {
        json!({
            "id": id,
            "name": name,
            "description": null,
            "version": null,
            "generation_date_and_time": self.generated_at,
            "generation_tool": self.generation_tool,
            "copyright": null,
            "license": null,
        })
    }

    fn ac_ctx(&self, checksums: &BTreeMap<String, String>) -> Value {
        let alg_sha1 = checksums.get("alg_sha1").map_or("", String::as_str);
        // The clock variable is the constant that carries the sample period.
        // Use its actual manifest ID so the <Clock> element and the variable
        // list are consistent without a separate hardcoded V_SAMPLE_PERIOD slot.
        let clock_var_id = self
            .vars
            .iter()
            .find(|v| v.causality == "constant")
            .map(|v| v.ac_id())
            .unwrap_or_else(|| "V_SAMPLE_PERIOD".to_string());
        let mut variables: Vec<Value> = self.vars.iter().map(Var::to_ac_var).collect();
        // Add a manifest State variable for each previous-value slot.
        // These appear as `previous(x)` in the GALEC block interface and are
        // distinct from the user-facing output/state variable they shadow.
        for (i, (pname, td)) in self.prev_vars.iter().enumerate() {
            let kind = if *td == "TD_I32" { "Integer" } else { "Real" };
            let start_val = self
                .vars
                .iter()
                .find(|v| v.name == *pname)
                .and_then(|v| v.start.as_deref())
                .unwrap_or("0.0");
            variables.push(json!({
                "kind": kind,
                "id": format!("V_PREV_{i}"),
                "name": format!("previous({})", pname),
                "description": null,
                "block_causality": "state",
                "dimensions": [],
                "start": start_val,
                "annotations": [],
                "unit_ref_id": null,
                "relative_quantity": false,
                "min": null,
                "max": null,
                "nominal": null,
            }));
        }
        json!({
            "ac": {
                "attributes": self.attrs(&self.ac_manifest_id, &self.model_name),
                "file_ref_id": "F_ALG",
                "files": [{
                    "id": "F_ALG",
                    "name": format!("{}.alg", self.model_name),
                    "path": "./", "needs_checksum": true, "checksum": alg_sha1,
                    "role": "Code", "description": null,
                }],
                "clock": { "id": "CLK", "variable_ref_id": clock_var_id },
                "block_methods": {
                    "startup": { "id": "BM_STARTUP", "kind": "Startup", "signals": [] },
                    "recalibrate": { "id": "BM_RECALIBRATE", "kind": "Recalibrate", "signals": [] },
                    "do_step": { "id": "BM_DOSTEP", "kind": "DoStep", "signals": [] },
                },
                "error_signal_status_id": "ESS",
                "variables": variables,
                "annotations": [],
            },
            "units": [],
        })
    }

    fn content_ctx(&self, checksums: &BTreeMap<String, String>) -> Value {
        let ac_sha1 = checksums.get("ac_manifest_sha1").map_or("", String::as_str);
        let mut representations = vec![json!({
            "name": "AlgorithmCode", "kind": "AlgorithmCode",
            "manifest": "manifest.xml", "checksum": ac_sha1,
            "manifest_ref_id": self.ac_manifest_id,
        })];
        if self.with_production {
            let pc_sha1 = checksums.get("pc_manifest_sha1").map_or("", String::as_str);
            representations.push(json!({
                "name": "ProductionCode", "kind": "ProductionCode",
                "manifest": "manifest.xml", "checksum": pc_sha1,
                "manifest_ref_id": self.pc_manifest_id,
            }));
        }
        json!({
            "content": {
                "attributes": self.attrs(&self.content_id, &self.source_name),
                "active_fmu": null,
                "representations": representations,
            }
        })
    }

    fn pc_ctx(&self, checksums: &BTreeMap<String, String>) -> Value {
        let ac_sha1 = checksums.get("ac_manifest_sha1").map_or("", String::as_str);
        let h_sha1 = checksums.get("c_header_sha1").map_or("", String::as_str);
        let c_sha1 = checksums.get("c_source_sha1").map_or("", String::as_str);
        json!({
            "pc": {
                "attributes": self.attrs(&self.pc_manifest_id, &self.model_name),
                "manifest_reference": {
                    "id": "MR_AC",
                    "manifest_ref_id": self.ac_manifest_id,
                    "checksum": ac_sha1,
                },
                "files": self.pc_files(h_sha1, c_sha1),
                "code_container": self.code_container(),
                "annotations": [],
            }
        })
    }

    fn pc_files(&self, h_sha1: &str, c_sha1: &str) -> Value {
        json!([
            {
                "id": "F_H", "name": format!("{}.h", self.model_name),
                "path": "./", "needs_checksum": true, "checksum": h_sha1,
                "role": "Code", "description": null,
            },
            {
                "id": "F_C", "name": format!("{}.c", self.model_name),
                "path": "./", "needs_checksum": true, "checksum": c_sha1,
                "role": "Code", "description": null,
            },
        ])
    }

    fn code_container(&self) -> Value {
        let typedefs = self.build_typedefs();
        let functions = self.build_functions();
        let (data_refs, func_refs) = self.build_logical_data();
        json!({
            "language": "C", "standard": "C99", "platform": "Legacy",
            "float_precision": "64-bit", "description": null, "target": "Generic",
            "target_types": [
                { "id": "TT_VOID", "kind": "efmiVoid",    "coded_type": "void"    },
                { "id": "TT_F64",  "kind": "efmiFloat64", "coded_type": "double"  },
                { "id": "TT_I32",  "kind": "efmiInteger32", "coded_type": "int32_t" },
                { "id": "TT_BOOL", "kind": "efmiBool",    "coded_type": "bool"    },
            ],
            "code_files": [
                {
                    "id": "CF_H", "file_type": "ProductionCode", "code_type": "HeaderFile",
                    "file_ref_id": "F_H", "file_ref_kind": null,
                    "includes": [], "typedefs": typedefs, "functions": [],
                },
                {
                    "id": "CF_C", "file_type": "ProductionCode", "code_type": "SourceFile",
                    "file_ref_id": "F_C", "file_ref_kind": null,
                    "includes": ["CF_H"], "typedefs": [], "functions": functions,
                },
            ],
            "logical_data": {
                "data_references": data_refs,
                "function_references": func_refs,
            },
        })
    }

    fn build_typedefs(&self) -> Vec<Value> {
        let mut result: Vec<Value> = vec![
            json!({ "id": "TD_VOID", "name": "void",    "alias": { "target_type_ref_id": "TT_VOID", "type_def_ref_id": null }, "components": null }),
            json!({ "id": "TD_F64",  "name": "double",  "alias": { "target_type_ref_id": "TT_F64",  "type_def_ref_id": null }, "components": null }),
            json!({ "id": "TD_I32",  "name": "int32_t", "alias": { "target_type_ref_id": "TT_I32",  "type_def_ref_id": null }, "components": null }),
            json!({ "id": "TD_BOOL", "name": "bool",    "alias": { "target_type_ref_id": "TT_BOOL", "type_def_ref_id": null }, "components": null }),
        ];
        let mut components: Vec<Value> = self.vars.iter().map(Var::to_component).collect();
        for (i, (pname, td)) in self.prev_vars.iter().enumerate() {
            components.push(json!({
                "id": format!("CO_PREV_{i}"),
                "name": format!("{pname}_prev"),
                "type_def_ref_id": td,
                "dimensions": [],
                "pointer": false,
            }));
        }
        result.push(json!({
            "id": "TD_STATE",
            "name": format!("{}State", self.model_name),
            "alias": null,
            "components": components,
        }));
        result
    }

    fn build_functions(&self) -> Vec<Value> {
        let n = &self.model_name;
        vec![
            build_fn_entry(
                "FN_STARTUP",
                &format!("{n}_startup"),
                "RP_STARTUP",
                "FP_STARTUP",
            ),
            build_fn_entry(
                "FN_RECALIBRATE",
                &format!("{n}_recalibrate"),
                "RP_RECALIBRATE",
                "FP_RECALIBRATE",
            ),
            build_fn_entry(
                "FN_DOSTEP",
                &format!("{n}_dostep"),
                "RP_DOSTEP",
                "FP_DOSTEP",
            ),
        ]
    }

    fn build_logical_data(&self) -> (Vec<Value>, Vec<Value>) {
        let mut data_refs: Vec<Value> = self.vars.iter().map(Var::to_data_ref).collect();
        // Mirror the AC manifest's V_PREV_i entries with PC data references
        // pointing at the <name>_prev C struct fields.
        for (i, (pname, _)) in self.prev_vars.iter().enumerate() {
            data_refs.push(json!({
                "manifest_reference_ref_id": "MR_AC",
                "foreign_ref_id": format!("V_PREV_{i}"),
                "formal_parameter_ref_id": "FP_DOSTEP",
                "component_identifier": format!("{pname}_prev"),
            }));
        }
        let func_refs = vec![
            json!({ "manifest_reference_ref_id": "MR_AC", "foreign_ref_id": "BM_STARTUP",    "function_ref_id": "FN_STARTUP"    }),
            json!({ "manifest_reference_ref_id": "MR_AC", "foreign_ref_id": "BM_RECALIBRATE","function_ref_id": "FN_RECALIBRATE" }),
            json!({ "manifest_reference_ref_id": "MR_AC", "foreign_ref_id": "BM_DOSTEP",     "function_ref_id": "FN_DOSTEP"     }),
        ];
        (data_refs, func_refs)
    }
}

fn build_fn_entry(fn_id: &str, name: &str, rp_id: &str, fp_id: &str) -> Value {
    json!({
        "id": fn_id, "name": name,
        "return_parameter": {
            "id": rp_id, "type_def_ref_id": "TD_VOID",
            "constant": false, "pointer": false, "const_pointer": false, "dimensions": [],
        },
        "formal_parameters": [{
            "name": "self",
            "core": {
                "id": fp_id, "type_def_ref_id": "TD_STATE",
                "constant": false, "pointer": true, "const_pointer": false, "dimensions": [],
            },
        }],
    })
}

pub(crate) fn collect_prev_refs(galec: &Galec) -> Vec<(String, &'static str)> {
    let var_td: std::collections::HashMap<&str, &'static str> = galec
        .variables
        .iter()
        .map(|v| (v.name.as_str(), typedef_id(&v.ty)))
        .collect();
    let mut names = BTreeSet::new();
    for group in &galec.do_step {
        if let Some(cond) = &group.condition {
            scan_prev_expr(cond, &mut names);
        }
        for stmt in &group.statements {
            let crate::galec::GalecStatement::Assign { rhs, .. } = stmt;
            scan_prev_expr(rhs, &mut names);
        }
    }
    names
        .into_iter()
        .map(|name| {
            let td = var_td.get(name.as_str()).copied().unwrap_or("TD_F64");
            (name, td)
        })
        .collect()
}

fn scan_prev_expr(expr: &GalecExpr, out: &mut BTreeSet<String>) {
    match expr {
        GalecExpr::PreviousRef { name } => {
            out.insert(name.clone());
        }
        GalecExpr::Binary { lhs, rhs, .. } => {
            scan_prev_expr(lhs, out);
            scan_prev_expr(rhs, out);
        }
        GalecExpr::Negate(inner) => scan_prev_expr(inner, out),
        GalecExpr::Call { args, .. } => args.iter().for_each(|a| scan_prev_expr(a, out)),
        GalecExpr::If {
            condition,
            then_branch,
            else_branch,
        } => {
            scan_prev_expr(condition, out);
            scan_prev_expr(then_branch, out);
            scan_prev_expr(else_branch, out);
        }
        GalecExpr::Index { base, subscripts } => {
            scan_prev_expr(base, out);
            subscripts.iter().for_each(|s| scan_prev_expr(s, out));
        }
        GalecExpr::Literal(_) | GalecExpr::SelfRef { .. } => {}
    }
}
