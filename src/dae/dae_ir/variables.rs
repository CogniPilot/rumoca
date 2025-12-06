//! Variable serialization for DAE IR.
//!
//! This module provides serialization for classified variables according to DAE formalism.

use crate::dae::ast::Dae;
use crate::ir::ast::{Component, Expression, Name, OpUnary, TerminalType};
use indexmap::IndexMap;
use serde::ser::{Serialize, SerializeMap, SerializeSeq, Serializer};

use super::helpers::EmptyArray;

/// Classified variables according to DAE formalism
pub struct ClassifiedVariables<'a> {
    pub dae: &'a Dae,
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
        let has_annotation = !self.comp.annotation.is_empty();
        let mut map_size = 4;
        if has_comment {
            map_size += 1;
        }
        if has_annotation {
            map_size += 1;
        }

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

        if has_annotation {
            map.serialize_entry("annotation", &AnnotationWrapper(&self.comp.annotation))?;
        }

        map.end()
    }
}

/// Array of basic variables
pub struct VariableArray<'a> {
    pub components: &'a IndexMap<String, Component>,
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
        let has_annotation = !self.comp.annotation.is_empty();
        let mut map_size = 3;
        if has_comment {
            map_size += 1;
        }
        if has_annotation {
            map_size += 1;
        }

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

        if has_annotation {
            map.serialize_entry("annotation", &AnnotationWrapper(&self.comp.annotation))?;
        }

        map.end()
    }
}

/// Wrapper for Name serialization
pub struct NameWrapper<'a>(pub &'a Name);

impl<'a> Serialize for NameWrapper<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let parts: Vec<&str> = self.0.name.iter().map(|t| t.text.as_str()).collect();
        serializer.serialize_str(&parts.join("."))
    }
}

/// Wrapper for annotation serialization (array of expressions)
struct AnnotationWrapper<'a>(&'a Vec<Expression>);

impl<'a> Serialize for AnnotationWrapper<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        use super::expressions::ExpressionWrapper;

        let mut seq = serializer.serialize_seq(Some(self.0.len()))?;
        for expr in self.0 {
            seq.serialize_element(&ExpressionWrapper(expr))?;
        }
        seq.end()
    }
}

/// Wrapper for simple start value extraction (number, string, boolean, or null)
pub struct StartValueWrapper<'a>(pub &'a Expression);

impl<'a> Serialize for StartValueWrapper<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serialize_start_value(self.0, serializer)
    }
}

/// Recursively serialize a start value, handling unary +/- operators
fn serialize_start_value<S>(expr: &Expression, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    match expr {
        Expression::Empty => serializer.serialize_none(),
        Expression::Terminal {
            terminal_type,
            token,
        } => match terminal_type {
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
            TerminalType::String => serializer.serialize_str(&token.text),
            _ => serializer.serialize_str(&token.text),
        },
        // Handle unary operators (+1, -1, etc.)
        Expression::Unary { op, rhs } => {
            // Try to extract a numeric value from the operand
            if let Some(val) = extract_numeric_value(rhs) {
                match op {
                    OpUnary::Plus(_) => serializer.serialize_f64(val),
                    OpUnary::Minus(_) => serializer.serialize_f64(-val),
                    _ => serializer.serialize_none(),
                }
            } else {
                serializer.serialize_none()
            }
        }
        // For complex expressions, we fall back to null
        // (schema says start is optional)
        _ => serializer.serialize_none(),
    }
}

/// Extract a numeric value from an expression if it's a simple number
fn extract_numeric_value(expr: &Expression) -> Option<f64> {
    match expr {
        Expression::Terminal {
            terminal_type: TerminalType::UnsignedInteger | TerminalType::UnsignedReal,
            token,
        } => token.text.parse::<f64>().ok(),
        Expression::Terminal { .. } => None,
        Expression::Unary { op, rhs } => {
            let val = extract_numeric_value(rhs)?;
            match op {
                OpUnary::Plus(_) => Some(val),
                OpUnary::Minus(_) => Some(-val),
                _ => None,
            }
        }
        _ => None,
    }
}
