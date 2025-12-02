//! Equation serialization for DAE IR.
//!
//! This module provides serialization for classified equations.

use crate::dae::ast::Dae;
use crate::ir::ast::{Equation, EquationBlock, Expression, Statement};
use serde::ser::{Serialize, SerializeMap, SerializeSeq, Serializer};
use serde_json::json;

use super::expressions::{ComponentRefParts, ExpressionWrapper};
use super::helpers::EmptyArray;

/// Classified equations
pub struct ClassifiedEquations<'a> {
    pub dae: &'a Dae,
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
        map.serialize_entry("discrete_valued", &EquationList { eqs: &self.dae.fm })?;

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
pub struct EquationList<'a> {
    pub eqs: &'a Vec<Equation>,
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

/// Wrapper for a single equation
pub struct EquationWrapper<'a> {
    pub eq: &'a Equation,
    pub index: usize,
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

/// Event indicators from conditions
pub struct EventIndicators<'a> {
    pub dae: &'a Dae,
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
pub struct Algorithms<'a> {
    pub dae: &'a Dae,
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
