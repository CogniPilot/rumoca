//! Expression serialization for DAE IR.
//!
//! This module provides serialization for Modelica expressions to JSON format.

use crate::ir::ast::{ComponentReference, Expression, OpBinary, OpUnary, Subscript, TerminalType};
use serde::ser::{Serialize, SerializeMap, SerializeSeq, Serializer};
use serde_json::json;

/// Wrapper for Expression serialization
pub struct ExpressionWrapper<'a>(pub &'a Expression);

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
            Expression::Parenthesized { inner } => {
                // Parentheses are transparent for serialization - just serialize the inner expression
                ExpressionWrapper(inner).serialize(serializer)
            }
            Expression::ArrayComprehension { expr, indices } => {
                let mut map = serializer.serialize_map(Some(3))?;
                map.serialize_entry("type", "array_comprehension")?;
                map.serialize_entry("expr", &ExpressionWrapper(expr))?;
                map.serialize_entry("indices", &indices)?;
                map.end()
            }
        }
    }
}

/// Wrapper for component reference parts
pub struct ComponentRefParts<'a>(pub &'a ComponentReference);

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
