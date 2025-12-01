//! Native DAE IR JSON serialization using serde_json.
//!
//! This module provides native Rust serialization for the DAE IR format (dae-0.1.0),
//! which is a superset of Base Modelica IR (MCP-0031) that adds explicit variable
//! classification matching the Modelica specification's DAE formalism (Appendix B).
//!
//! Key advantages over Base Modelica IR:
//! - Explicit state/derivative/algebraic classification (no der() scanning needed)
//! - Direct state/derivative linkage (like FMI's derivative attribute)
//! - Event indicators for zero-crossing detection
//! - Structural metadata (n_states, n_algebraic, dae_index)

use crate::dae::ast::Dae;
use crate::ir::ast::{
    Component, ComponentReference, Equation, EquationBlock, Expression, Name, OpBinary, OpUnary,
    Statement, Subscript, TerminalType,
};
use indexmap::IndexMap;
use serde::ser::{Serialize, SerializeMap, SerializeSeq, Serializer};
use serde_json::json;

/// Wrapper struct for the complete DAE IR
#[derive(Debug)]
pub struct DaeIR<'a> {
    dae: &'a Dae,
}

impl<'a> DaeIR<'a> {
    pub fn from_dae(dae: &'a Dae) -> Self {
        Self { dae }
    }
}

impl<'a> Serialize for DaeIR<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(12))?;

        map.serialize_entry("ir_version", "dae-0.1.0")?;
        map.serialize_entry("base_modelica_version", "0.1")?;
        map.serialize_entry("model_name", "GeneratedModel")?;

        // Variables - classified according to DAE formalism
        map.serialize_entry("variables", &ClassifiedVariables { dae: self.dae })?;

        // Equations - classified by type
        map.serialize_entry("equations", &ClassifiedEquations { dae: self.dae })?;

        // Event indicators from conditions
        map.serialize_entry("event_indicators", &EventIndicators { dae: self.dae })?;

        // Algorithms - reconstruct from fr (reset expressions)
        map.serialize_entry("algorithms", &Algorithms { dae: self.dae })?;

        map.serialize_entry("initial_algorithms", &EmptyArray)?;
        map.serialize_entry("functions", &EmptyArray)?;

        // Structure metadata
        map.serialize_entry("structure", &Structure { dae: self.dae })?;

        map.serialize_entry("source_info", &EmptyObject)?;

        // Metadata
        map.serialize_entry("metadata", &Metadata { dae: self.dae })?;

        map.end()
    }
}

/// Helper for empty JSON arrays
struct EmptyArray;
impl Serialize for EmptyArray {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let seq = serializer.serialize_seq(Some(0))?;
        seq.end()
    }
}

/// Helper for empty JSON objects
struct EmptyObject;
impl Serialize for EmptyObject {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let map = serializer.serialize_map(Some(0))?;
        map.end()
    }
}

/// Classified variables according to DAE formalism
struct ClassifiedVariables<'a> {
    dae: &'a Dae,
}

impl<'a> Serialize for ClassifiedVariables<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(8))?;

        // States (x) - variables that appear in der()
        // Note: derivatives are not listed separately - they appear as der(state) in equations
        map.serialize_entry("states", &StateVariables { dae: self.dae })?;

        // Algebraic (y) - continuous but not differentiated
        map.serialize_entry(
            "algebraic",
            &VariableArray {
                components: &self.dae.y,
            },
        )?;

        // Discrete Real (z)
        map.serialize_entry(
            "discrete_real",
            &VariableArray {
                components: &self.dae.z,
            },
        )?;

        // Discrete valued (m) - Boolean, Integer
        map.serialize_entry(
            "discrete_valued",
            &VariableArray {
                components: &self.dae.m,
            },
        )?;

        // Parameters (p)
        map.serialize_entry(
            "parameters",
            &VariableArray {
                components: &self.dae.p,
            },
        )?;

        // Constants (cp)
        map.serialize_entry(
            "constants",
            &VariableArray {
                components: &self.dae.cp,
            },
        )?;

        // Inputs (u)
        map.serialize_entry(
            "inputs",
            &VariableArray {
                components: &self.dae.u,
            },
        )?;

        // Outputs - empty for now
        map.serialize_entry("outputs", &EmptyArray)?;

        map.end()
    }
}

/// State variables with derivative linkage
struct StateVariables<'a> {
    dae: &'a Dae,
}

impl<'a> Serialize for StateVariables<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut seq = serializer.serialize_seq(Some(self.dae.x.len()))?;
        for (idx, (name, comp)) in self.dae.x.iter().enumerate() {
            seq.serialize_element(&StateVariableWrapper {
                name,
                comp,
                state_index: idx,
            })?;
        }
        seq.end()
    }
}

/// Wrapper for a single state variable
struct StateVariableWrapper<'a> {
    name: &'a str,
    comp: &'a Component,
    state_index: usize,
}

impl<'a> Serialize for StateVariableWrapper<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let has_comment = !self.comp.description.is_empty();
        let map_size = if has_comment { 5 } else { 4 };

        let mut map = serializer.serialize_map(Some(map_size))?;

        map.serialize_entry("name", self.name)?;
        map.serialize_entry("vartype", &NameWrapper(&self.comp.type_name))?;
        map.serialize_entry("state_index", &self.state_index)?;
        map.serialize_entry("start", &StartValueWrapper(&self.comp.start))?;

        if has_comment {
            let comment: String = self
                .comp
                .description
                .iter()
                .map(|t| t.text.as_str())
                .collect::<Vec<_>>()
                .join(" ");
            map.serialize_entry("comment", &comment)?;
        }

        map.end()
    }
}

/// Array of basic variables
struct VariableArray<'a> {
    components: &'a IndexMap<String, Component>,
}

impl<'a> Serialize for VariableArray<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut seq = serializer.serialize_seq(Some(self.components.len()))?;
        for (name, comp) in self.components {
            seq.serialize_element(&BasicVariableWrapper { name, comp })?;
        }
        seq.end()
    }
}

/// Basic variable wrapper (no state/derivative linkage)
struct BasicVariableWrapper<'a> {
    name: &'a str,
    comp: &'a Component,
}

impl<'a> Serialize for BasicVariableWrapper<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let has_comment = !self.comp.description.is_empty();
        let map_size = if has_comment { 4 } else { 3 };

        let mut map = serializer.serialize_map(Some(map_size))?;

        map.serialize_entry("name", self.name)?;
        map.serialize_entry("vartype", &NameWrapper(&self.comp.type_name))?;
        map.serialize_entry("start", &StartValueWrapper(&self.comp.start))?;

        if has_comment {
            let comment: String = self
                .comp
                .description
                .iter()
                .map(|t| t.text.as_str())
                .collect::<Vec<_>>()
                .join(" ");
            map.serialize_entry("comment", &comment)?;
        }

        map.end()
    }
}

/// Wrapper for Name serialization
struct NameWrapper<'a>(&'a Name);

impl<'a> Serialize for NameWrapper<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let parts: Vec<&str> = self.0.name.iter().map(|t| t.text.as_str()).collect();
        serializer.serialize_str(&parts.join("."))
    }
}

/// Wrapper for simple start value extraction (number, string, boolean, or null)
struct StartValueWrapper<'a>(&'a Expression);

impl<'a> Serialize for StartValueWrapper<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self.0 {
            Expression::Empty => serializer.serialize_none(),
            Expression::Terminal { terminal_type, token } => {
                match terminal_type {
                    TerminalType::UnsignedInteger => {
                        if let Ok(val) = token.text.parse::<i64>() {
                            serializer.serialize_i64(val)
                        } else {
                            serializer.serialize_str(&token.text)
                        }
                    }
                    TerminalType::UnsignedReal => {
                        if let Ok(val) = token.text.parse::<f64>() {
                            serializer.serialize_f64(val)
                        } else {
                            serializer.serialize_str(&token.text)
                        }
                    }
                    TerminalType::Bool => {
                        let val = token.text.to_lowercase() == "true";
                        serializer.serialize_bool(val)
                    }
                    TerminalType::String => {
                        serializer.serialize_str(&token.text)
                    }
                    _ => serializer.serialize_str(&token.text)
                }
            }
            // For complex expressions, we fall back to null
            // (schema says start is optional)
            _ => serializer.serialize_none()
        }
    }
}

/// Classified equations
struct ClassifiedEquations<'a> {
    dae: &'a Dae,
}

impl<'a> Serialize for ClassifiedEquations<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(5))?;

        // Continuous equations (fx)
        map.serialize_entry("continuous", &EquationList { eqs: &self.dae.fx })?;

        // Event equations
        map.serialize_entry("event", &EmptyArray)?;

        // Discrete real equations (fz)
        map.serialize_entry("discrete_real", &EquationList { eqs: &self.dae.fz })?;

        // Discrete valued equations (fm)
        map.serialize_entry(
            "discrete_valued",
            &EquationList { eqs: &self.dae.fm },
        )?;

        // Initial equations
        map.serialize_entry(
            "initial",
            &EquationList {
                eqs: &self.dae.fx_init,
            },
        )?;

        map.end()
    }
}

/// List of equations
struct EquationList<'a> {
    eqs: &'a Vec<Equation>,
}

impl<'a> Serialize for EquationList<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut seq = serializer.serialize_seq(Some(self.eqs.len()))?;
        for (idx, eq) in self.eqs.iter().enumerate() {
            seq.serialize_element(&EquationWrapper { eq, index: idx + 1 })?;
        }
        seq.end()
    }
}

/// Event indicators from conditions
struct EventIndicators<'a> {
    dae: &'a Dae,
}

impl<'a> Serialize for EventIndicators<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut seq = serializer.serialize_seq(Some(self.dae.fc.len()))?;
        for (name, expr) in &self.dae.fc {
            seq.serialize_element(&EventIndicator { name, expr })?;
        }
        seq.end()
    }
}

/// Single event indicator
struct EventIndicator<'a> {
    name: &'a str,
    expr: &'a Expression,
}

impl<'a> Serialize for EventIndicator<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(3))?;
        map.serialize_entry("name", self.name)?;
        map.serialize_entry("expression", &ExpressionWrapper(self.expr))?;
        map.serialize_entry("direction", "both")?;
        map.end()
    }
}

/// Algorithms from reset expressions
struct Algorithms<'a> {
    dae: &'a Dae,
}

impl<'a> Serialize for Algorithms<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        if self.dae.fr.is_empty() {
            let seq = serializer.serialize_seq(Some(0))?;
            return seq.end();
        }

        // Convert reset statements to algorithm section
        let mut seq = serializer.serialize_seq(Some(1))?;
        seq.serialize_element(&AlgorithmSection { dae: self.dae })?;
        seq.end()
    }
}

/// Single algorithm section
struct AlgorithmSection<'a> {
    dae: &'a Dae,
}

impl<'a> Serialize for AlgorithmSection<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(1))?;
        map.serialize_entry("statements", &ResetStatements { dae: self.dae })?;
        map.end()
    }
}

/// Reset statements as algorithm statements
struct ResetStatements<'a> {
    dae: &'a Dae,
}

impl<'a> Serialize for ResetStatements<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut seq = serializer.serialize_seq(Some(self.dae.fr.len()))?;
        for (cond_name, stmt) in &self.dae.fr {
            seq.serialize_element(&StatementWrapper {
                stmt,
                source_ref: cond_name,
            })?;
        }
        seq.end()
    }
}

/// Statement wrapper
struct StatementWrapper<'a> {
    stmt: &'a Statement,
    source_ref: &'a str,
}

impl<'a> Serialize for StatementWrapper<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self.stmt {
            Statement::Assignment { comp, value } => {
                let mut map = serializer.serialize_map(Some(4))?;
                map.serialize_entry("stmt", "reinit")?;
                map.serialize_entry("target", &ComponentRefParts(comp))?;
                map.serialize_entry("expr", &ExpressionWrapper(value))?;
                map.serialize_entry("source_ref", self.source_ref)?;
                map.end()
            }
            _ => {
                let map = serializer.serialize_map(Some(0))?;
                map.end()
            }
        }
    }
}

/// Structure metadata
struct Structure<'a> {
    dae: &'a Dae,
}

impl<'a> Serialize for Structure<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let n_states = self.dae.x.len();
        let n_algebraic = self.dae.y.len();
        let n_equations = self.dae.fx.len();
        let is_ode = n_algebraic == 0;

        let mut map = serializer.serialize_map(Some(5))?;
        map.serialize_entry("n_states", &n_states)?;
        map.serialize_entry("n_algebraic", &n_algebraic)?;
        map.serialize_entry("n_equations", &n_equations)?;
        map.serialize_entry("dae_index", &0)?; // TODO: compute actual index
        map.serialize_entry("is_ode", &is_ode)?;
        map.end()
    }
}

/// Metadata section
struct Metadata<'a> {
    dae: &'a Dae,
}

impl<'a> Serialize for Metadata<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(5))?;
        map.serialize_entry(
            "description",
            &format!("Generated by Rumoca {}", self.dae.rumoca_version),
        )?;
        map.serialize_entry("generator", "rumoca")?;
        map.serialize_entry("rumoca_version", &self.dae.rumoca_version)?;
        map.serialize_entry("git_version", &self.dae.git_version)?;
        map.serialize_entry("model_hash", &self.dae.model_hash)?;
        map.end()
    }
}

/// Wrapper for Expression serialization
struct ExpressionWrapper<'a>(&'a Expression);

impl<'a> Serialize for ExpressionWrapper<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self.0 {
            Expression::Empty => {
                let mut map = serializer.serialize_map(Some(2))?;
                map.serialize_entry("op", "literal")?;
                map.serialize_entry("value", &0)?;
                map.end()
            }
            Expression::Terminal {
                terminal_type,
                token,
            } => {
                let mut map = serializer.serialize_map(Some(2))?;
                map.serialize_entry("op", "literal")?;

                match terminal_type {
                    TerminalType::UnsignedInteger => {
                        if let Ok(val) = token.text.parse::<i64>() {
                            map.serialize_entry("value", &val)?;
                        } else {
                            map.serialize_entry("value", &token.text)?;
                        }
                    }
                    TerminalType::UnsignedReal => {
                        if let Ok(val) = token.text.parse::<f64>() {
                            map.serialize_entry("value", &val)?;
                        } else {
                            map.serialize_entry("value", &token.text)?;
                        }
                    }
                    TerminalType::Bool => {
                        let val = token.text.to_lowercase() == "true";
                        map.serialize_entry("value", &val)?;
                    }
                    TerminalType::String => {
                        map.serialize_entry("value", &token.text)?;
                    }
                    _ => {
                        map.serialize_entry("value", &token.text)?;
                    }
                }

                map.end()
            }
            Expression::ComponentReference(comp_ref) => {
                let mut map = serializer.serialize_map(Some(2))?;
                map.serialize_entry("op", "component_ref")?;
                map.serialize_entry("parts", &ComponentRefParts(comp_ref))?;
                map.end()
            }
            Expression::Unary { op, rhs } => {
                let mut map = serializer.serialize_map(Some(2))?;
                let op_name = match op {
                    OpUnary::Minus(_) | OpUnary::DotMinus(_) => "neg",
                    OpUnary::Not(_) => "not",
                    OpUnary::Plus(_) | OpUnary::DotPlus(_) => "pos",
                    _ => "unknown_unary",
                };
                map.serialize_entry("op", op_name)?;
                map.serialize_entry("args", &vec![ExpressionWrapper(rhs)])?;
                map.end()
            }
            Expression::Binary { op, lhs, rhs } => {
                let mut map = serializer.serialize_map(Some(2))?;
                let op_name = match op {
                    OpBinary::Add(_) | OpBinary::AddElem(_) => "+",
                    OpBinary::Sub(_) | OpBinary::SubElem(_) => "-",
                    OpBinary::Mul(_) | OpBinary::MulElem(_) => "*",
                    OpBinary::Div(_) | OpBinary::DivElem(_) => "/",
                    OpBinary::Exp(_) => "^",
                    OpBinary::Lt(_) => "<",
                    OpBinary::Le(_) => "<=",
                    OpBinary::Gt(_) => ">",
                    OpBinary::Ge(_) => ">=",
                    OpBinary::Eq(_) => "==",
                    OpBinary::Neq(_) => "!=",
                    OpBinary::And(_) => "and",
                    OpBinary::Or(_) => "or",
                    _ => "unknown_binary",
                };
                map.serialize_entry("op", op_name)?;
                map.serialize_entry(
                    "args",
                    &vec![ExpressionWrapper(lhs), ExpressionWrapper(rhs)],
                )?;
                map.end()
            }
            Expression::FunctionCall { comp, args } => {
                let mut map = serializer.serialize_map(Some(2))?;
                let func_name = comp.to_string();
                map.serialize_entry("op", &func_name)?;
                let arg_wrappers: Vec<ExpressionWrapper> =
                    args.iter().map(ExpressionWrapper).collect();
                map.serialize_entry("args", &arg_wrappers)?;
                map.end()
            }
            Expression::Array { elements } => {
                let mut map = serializer.serialize_map(Some(2))?;
                map.serialize_entry("op", "array")?;
                let elem_wrappers: Vec<ExpressionWrapper> =
                    elements.iter().map(ExpressionWrapper).collect();
                map.serialize_entry("values", &elem_wrappers)?;
                map.end()
            }
            Expression::Range { start, step, end } => {
                let has_step = step.is_some();
                let map_size = if has_step { 4 } else { 3 };
                let mut map = serializer.serialize_map(Some(map_size))?;
                map.serialize_entry("op", "range")?;
                map.serialize_entry("start", &ExpressionWrapper(start))?;
                if let Some(step_expr) = step {
                    map.serialize_entry("step", &ExpressionWrapper(step_expr))?;
                }
                map.serialize_entry("end", &ExpressionWrapper(end))?;
                map.end()
            }
            Expression::Tuple { elements } => {
                let mut map = serializer.serialize_map(Some(2))?;
                map.serialize_entry("op", "tuple")?;
                let elem_wrappers: Vec<ExpressionWrapper> =
                    elements.iter().map(ExpressionWrapper).collect();
                map.serialize_entry("elements", &elem_wrappers)?;
                map.end()
            }
            Expression::If {
                branches,
                else_branch,
            } => {
                let mut map = serializer.serialize_map(Some(3))?;
                map.serialize_entry("op", "if")?;

                let branch_pairs: Vec<[ExpressionWrapper; 2]> = branches
                    .iter()
                    .map(|(cond, expr)| [ExpressionWrapper(cond), ExpressionWrapper(expr)])
                    .collect();
                map.serialize_entry("branches", &branch_pairs)?;
                map.serialize_entry("else", &ExpressionWrapper(else_branch))?;
                map.end()
            }
        }
    }
}

/// Wrapper for component reference parts
struct ComponentRefParts<'a>(&'a ComponentReference);

impl<'a> Serialize for ComponentRefParts<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut seq = serializer.serialize_seq(Some(self.0.parts.len()))?;
        for part in &self.0.parts {
            let mut map = serde_json::Map::new();
            map.insert("name".to_string(), json!(part.ident.text));

            // Serialize subscripts if present
            let subscripts: Vec<serde_json::Value> = match &part.subs {
                Some(subs) => subs
                    .iter()
                    .filter_map(|sub| match sub {
                        Subscript::Expression(expr) => {
                            serde_json::to_value(ExpressionWrapper(expr)).ok()
                        }
                        Subscript::Range { .. } => Some(json!({"op": "colon"})),
                        Subscript::Empty => None,
                    })
                    .collect(),
                None => vec![],
            };
            map.insert("subscripts".to_string(), json!(subscripts));

            seq.serialize_element(&map)?;
        }
        seq.end()
    }
}

/// Wrapper for a single equation
struct EquationWrapper<'a> {
    eq: &'a Equation,
    index: usize,
}

impl<'a> Serialize for EquationWrapper<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self.eq {
            Equation::Empty => {
                let mut map = serializer.serialize_map(Some(4))?;
                map.serialize_entry("eq_type", "simple")?;
                map.serialize_entry("lhs", &json!({"op": "literal", "value": 0}))?;
                map.serialize_entry("rhs", &json!({"op": "literal", "value": 0}))?;
                map.serialize_entry("source_ref", &format!("empty_{}", self.index))?;
                map.end()
            }
            Equation::Simple { lhs, rhs } => {
                let mut map = serializer.serialize_map(Some(4))?;
                map.serialize_entry("eq_type", "simple")?;
                map.serialize_entry("lhs", &ExpressionWrapper(lhs))?;
                map.serialize_entry("rhs", &ExpressionWrapper(rhs))?;
                map.serialize_entry("source_ref", &format!("eq_{}", self.index))?;
                map.end()
            }
            Equation::When(branches) => {
                let mut map = serializer.serialize_map(Some(3))?;
                map.serialize_entry("eq_type", "when")?;
                map.serialize_entry("branches", &WhenBranches { branches })?;
                map.serialize_entry("source_ref", &format!("when_{}", self.index))?;
                map.end()
            }
            Equation::For { indices, equations } => {
                let mut map = serializer.serialize_map(Some(4))?;
                map.serialize_entry("eq_type", "for")?;
                let indices_json: Vec<serde_json::Value> = indices
                    .iter()
                    .map(|idx| {
                        json!({
                            "index": idx.ident.text,
                            "range": serde_json::to_value(ExpressionWrapper(&idx.range)).unwrap()
                        })
                    })
                    .collect();
                map.serialize_entry("indices", &indices_json)?;
                let eqs_json: Vec<serde_json::Value> = equations
                    .iter()
                    .enumerate()
                    .map(|(i, eq)| {
                        serde_json::to_value(EquationWrapper { eq, index: i + 1 }).unwrap()
                    })
                    .collect();
                map.serialize_entry("equations", &eqs_json)?;
                map.serialize_entry("source_ref", &format!("for_{}", self.index))?;
                map.end()
            }
            Equation::If {
                cond_blocks,
                else_block,
            } => {
                let mut map = serializer.serialize_map(Some(4))?;
                map.serialize_entry("eq_type", "if")?;
                let branches_json: Vec<serde_json::Value> = cond_blocks
                    .iter()
                    .map(|block| {
                        let eqs: Vec<serde_json::Value> = block
                            .eqs
                            .iter()
                            .enumerate()
                            .map(|(i, eq)| {
                                serde_json::to_value(EquationWrapper { eq, index: i + 1 }).unwrap()
                            })
                            .collect();
                        json!({
                            "condition": serde_json::to_value(ExpressionWrapper(&block.cond)).unwrap(),
                            "equations": eqs
                        })
                    })
                    .collect();
                map.serialize_entry("branches", &branches_json)?;
                if let Some(else_eqs) = else_block {
                    let else_json: Vec<serde_json::Value> = else_eqs
                        .iter()
                        .enumerate()
                        .map(|(i, eq)| {
                            serde_json::to_value(EquationWrapper { eq, index: i + 1 }).unwrap()
                        })
                        .collect();
                    map.serialize_entry("else_equations", &else_json)?;
                }
                map.serialize_entry("source_ref", &format!("if_{}", self.index))?;
                map.end()
            }
            Equation::Connect { lhs, rhs } => {
                let mut map = serializer.serialize_map(Some(4))?;
                map.serialize_entry("eq_type", "connect")?;
                map.serialize_entry("lhs", &ComponentRefParts(lhs))?;
                map.serialize_entry("rhs", &ComponentRefParts(rhs))?;
                map.serialize_entry("source_ref", &format!("connect_{}", self.index))?;
                map.end()
            }
            Equation::FunctionCall { comp, args } => {
                let mut map = serializer.serialize_map(Some(4))?;
                map.serialize_entry("eq_type", "call")?;
                map.serialize_entry("func", &comp.to_string())?;
                let args_json: Vec<serde_json::Value> = args
                    .iter()
                    .map(|arg| serde_json::to_value(ExpressionWrapper(arg)).unwrap())
                    .collect();
                map.serialize_entry("args", &args_json)?;
                map.serialize_entry("source_ref", &format!("call_{}", self.index))?;
                map.end()
            }
        }
    }
}

/// Wrapper for when branches
struct WhenBranches<'a> {
    branches: &'a [EquationBlock],
}

impl<'a> Serialize for WhenBranches<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut seq = serializer.serialize_seq(Some(self.branches.len()))?;
        for branch in self.branches {
            let mut map = serde_json::Map::new();
            map.insert(
                "condition".to_string(),
                serde_json::to_value(ExpressionWrapper(&branch.cond)).unwrap(),
            );
            let equations: Vec<serde_json::Value> = branch
                .eqs
                .iter()
                .enumerate()
                .map(|(idx, eq)| {
                    serde_json::to_value(EquationWrapper { eq, index: idx + 1 }).unwrap()
                })
                .collect();
            map.insert("equations".to_string(), json!(equations));
            seq.serialize_element(&map)?;
        }
        seq.end()
    }
}
