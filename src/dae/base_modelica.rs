//! Native Base Modelica JSON serialization using serde_json.
//!
//! This module provides native Rust serialization for the Base Modelica IR format (MCP-0031),
//! replacing the template-based approach with type-safe, performant Rust code.

use crate::dae::ast::Dae;
use crate::ir::ast::{
    Component, ComponentRefPart, ComponentReference, Equation, EquationBlock, Expression, Location,
    Name, OpBinary, OpUnary, TerminalType, Token,
};
use indexmap::IndexMap;
use serde::ser::{Serialize, SerializeMap, SerializeSeq, Serializer};
use serde_json::json;

/// Wrapper struct for the complete Base Modelica IR
#[derive(Debug)]
pub struct BaseModelicaIR<'a> {
    dae: &'a Dae,
}

impl<'a> BaseModelicaIR<'a> {
    pub fn from_dae(dae: &'a Dae) -> Self {
        Self { dae }
    }
}

impl<'a> Serialize for BaseModelicaIR<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(12))?;

        map.serialize_entry("ir_version", "base-0.1.0")?;
        map.serialize_entry("base_modelica_version", "0.1")?;
        map.serialize_entry("model_name", "GeneratedModel")?;

        // Constants
        map.serialize_entry(
            "constants",
            &ComponentList {
                components: &self.dae.cp,
                variability: "constant",
                causality: "local",
            },
        )?;

        // Parameters
        map.serialize_entry(
            "parameters",
            &ComponentList {
                components: &self.dae.p,
                variability: "parameter",
                causality: "local",
            },
        )?;

        // Variables (x, y, u, z combined)
        map.serialize_entry("variables", &VariableList { dae: self.dae })?;

        // Equations (continuous + when equations)
        map.serialize_entry("equations", &AllEquationsList { dae: self.dae })?;

        map.serialize_entry("initial_equations", &EmptyArray)?;
        map.serialize_entry("algorithms", &EmptyArray)?;
        map.serialize_entry("initial_algorithms", &EmptyArray)?;
        map.serialize_entry("functions", &EmptyArray)?;
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

/// Serializer for a list of components
struct ComponentList<'a> {
    components: &'a IndexMap<String, Component>,
    variability: &'a str,
    causality: &'a str,
}

impl<'a> Serialize for ComponentList<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut seq = serializer.serialize_seq(Some(self.components.len()))?;
        for (name, comp) in self.components {
            seq.serialize_element(&ComponentWrapper {
                name,
                comp,
                variability: self.variability,
                causality: self.causality,
            })?;
        }
        seq.end()
    }
}

/// Combined variables list (x, y, u, z)
struct VariableList<'a> {
    dae: &'a Dae,
}

impl<'a> Serialize for VariableList<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let total_len = self.dae.x.len() + self.dae.y.len() + self.dae.u.len() + self.dae.z.len();
        let mut seq = serializer.serialize_seq(Some(total_len))?;

        // Continuous states (x)
        for (name, comp) in &self.dae.x {
            seq.serialize_element(&ComponentWrapper {
                name,
                comp,
                variability: "continuous",
                causality: "local",
            })?;
        }

        // Algebraic variables (y)
        for (name, comp) in &self.dae.y {
            seq.serialize_element(&ComponentWrapper {
                name,
                comp,
                variability: "continuous",
                causality: "local",
            })?;
        }

        // Inputs (u)
        for (name, comp) in &self.dae.u {
            seq.serialize_element(&ComponentWrapper {
                name,
                comp,
                variability: "continuous",
                causality: "input",
            })?;
        }

        // Discrete variables (z)
        for (name, comp) in &self.dae.z {
            seq.serialize_element(&ComponentWrapper {
                name,
                comp,
                variability: "discrete",
                causality: "local",
            })?;
        }

        seq.end()
    }
}

/// Wrapper for a single component with metadata
struct ComponentWrapper<'a> {
    name: &'a str,
    comp: &'a Component,
    variability: &'a str,
    causality: &'a str,
}

impl<'a> Serialize for ComponentWrapper<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let has_comment = !self.comp.description.is_empty();
        let map_size = if has_comment { 7 } else { 6 };

        let mut map = serializer.serialize_map(Some(map_size))?;

        map.serialize_entry("name", self.name)?;
        map.serialize_entry("vartype", &NameWrapper(&self.comp.type_name))?;
        map.serialize_entry("variability", self.variability)?;
        map.serialize_entry("causality", self.causality)?;
        // Use actual shape from component (empty vec for scalars, [2,3] for matrices)
        map.serialize_entry("shape", &self.comp.shape)?;
        map.serialize_entry("start", &ExpressionWrapper(&self.comp.start))?;

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

        map.serialize_entry(
            "source_ref",
            &format!("var_{}", self.name.replace(".", "_")),
        )?;

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
                // Serialize as: {"op": "if", "branches": [[cond, expr], ...], "else": expr}
                let mut map = serializer.serialize_map(Some(3))?;
                map.serialize_entry("op", "if")?;

                // Serialize branches as array of [condition, expression] pairs
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
            map.insert("subscripts".to_string(), json!([]));
            seq.serialize_element(&map)?;
        }
        seq.end()
    }
}

/// Wrapper for all equations (continuous + when equations reconstructed from DAE)
struct AllEquationsList<'a> {
    dae: &'a Dae,
}

impl<'a> Serialize for AllEquationsList<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        // Count total equations: continuous + when equations
        let num_when_eqs = if !self.dae.fr.is_empty() { 1 } else { 0 };
        let total_eqs = self.dae.fx.len() + num_when_eqs;

        let mut seq = serializer.serialize_seq(Some(total_eqs))?;

        // Serialize continuous equations
        for (idx, eq) in self.dae.fx.iter().enumerate() {
            seq.serialize_element(&EquationWrapper { eq, index: idx + 1 })?;
        }

        // Reconstruct and serialize when equations from fr (reset expressions)
        if !self.dae.fr.is_empty() {
            let when_eq = reconstruct_when_equations(self.dae);
            let idx = self.dae.fx.len() + 1;
            seq.serialize_element(&EquationWrapper {
                eq: &when_eq,
                index: idx,
            })?;
        }

        seq.end()
    }
}

/// Reconstruct When equations from DAE reset expressions (fr) and conditions (fc)
fn reconstruct_when_equations(dae: &Dae) -> Equation {
    use crate::ir::ast::Statement;

    // Group reset statements by condition
    let mut branches = Vec::new();

    for (cond_name, stmt) in &dae.fr {
        // Get the condition expression from fc
        let cond_expr = dae.fc.get(cond_name).cloned().unwrap_or_else(|| {
            // If condition expression not in fc, create a reference to the condition variable
            Expression::ComponentReference(ComponentReference {
                local: false,
                parts: vec![ComponentRefPart {
                    ident: Token {
                        text: cond_name.clone(),
                        location: Location::default(),
                        token_number: 0,
                        token_type: 0,
                    },
                    subs: None,
                }],
            })
        });

        // Convert statement to equation
        let eqs = match stmt {
            Statement::Assignment { comp, value } => {
                vec![Equation::Simple {
                    lhs: Expression::ComponentReference(comp.clone()),
                    rhs: value.clone(),
                }]
            }
            _ => Vec::new(),
        };

        branches.push(EquationBlock {
            cond: cond_expr,
            eqs,
        });
    }

    Equation::When(branches)
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
            _ => {
                // For other equation types, serialize as unknown with debug info
                let mut map = serializer.serialize_map(Some(3))?;
                map.serialize_entry("eq_type", "unknown")?;
                map.serialize_entry("debug", &format!("{:?}", self.eq))?;
                map.serialize_entry("source_ref", &format!("unknown_{}", self.index))?;
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

/// Metadata section
struct Metadata<'a> {
    dae: &'a Dae,
}

impl<'a> Serialize for Metadata<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(6))?;
        map.serialize_entry(
            "description",
            &format!("Generated by Rumoca {}", self.dae.rumoca_version),
        )?;
        map.serialize_entry("generator", "rumoca")?;
        map.serialize_entry("rumoca_version", &self.dae.rumoca_version)?;
        map.serialize_entry("git_version", &self.dae.git_version)?;
        map.serialize_entry("model_hash", &self.dae.model_hash)?;
        map.serialize_entry("base_modelica_compliance", "0.1")?;
        map.end()
    }
}
