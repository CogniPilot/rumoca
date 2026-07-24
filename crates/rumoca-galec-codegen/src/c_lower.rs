//! GALEC AST → structured C codegen IR for the `embedded-c-galec` export (SPEC_0034
//! GAL-024).
//!
//! Scope: exactly the AST shape [`crate::lower`] emits today — sequential
//! [`Statement::Assignment`]s over single-part `self.` state references,
//! with expressions built from literals, references, the emittable §3.2.6
//! builtin calls ([`crate::lower::emittable_builtin_targets`]),
//! parentheses, if-expressions, `not`, unary minus over references, binary
//! operations, and whole-array start literals. Anything outside that shape
//! fails with a typed `ET023` — never silently dropped (GAL-007).
//!
//! Semantics represented for the C templates:
//!
//! - `and`/`or`/`not` → `&&`/`||`/`!`; `<>` → `!=`; `^` → `pow(…)`
//!   (GALEC `^` returns Real for numeric operands);
//! - every composite subexpression is parenthesized, so the AST shape — the
//!   normative GALEC evaluation order (trap T6) — survives verbatim and
//!   nested unary/binary forms can never re-associate;
//! - Real literals reuse the strict GALEC formatter (trap T7); its output
//!   (`1.0e+5`) is a valid C `double` literal;
//! - GALEC subscripts are 1-based, C subscripts 0-based: literal indices
//!   shift at print time, expression indices print as `(… - 1)`;
//! - Integer is `int32_t`, so literals outside its range are rejected
//!   rather than truncated;
//! - builtin identity and arguments stay semantic in the context; the
//!   templates own C99 names and helper selection.

use rumoca_ir_galec::ast::{BinaryOp, Expression, IfExpression, Name, Reference, Statement};

use crate::c_mangle::CNameTable;
use crate::diagnostic::GalecTargetError;

/// GALEC-statement/-expression → structured C codegen IR lowerer over one
/// package's collision-checked C name table.
pub struct CContextLowerer<'a> {
    names: &'a CNameTable,
}

impl<'a> CContextLowerer<'a> {
    /// Lowerer over the package's C name table.
    #[must_use]
    pub fn new(names: &'a CNameTable) -> Self {
        Self { names }
    }

    /// Lower one GALEC statement to structured, serializable C codegen IR.
    ///
    /// This is the production codegen boundary: array-valued assignments are
    /// normalized here, but no C source text is produced. The minijinja
    /// templates own all C spelling and punctuation.
    pub fn statement_contexts(
        &self,
        statement: &Statement,
    ) -> Result<Vec<serde_json::Value>, GalecTargetError> {
        let Statement::Assignment { target, value } = statement else {
            return Err(match statement {
                Statement::MultiAssignment { .. } => {
                    unsupported_statement("a multi-assignment statement")
                }
                Statement::Call(_) => unsupported_statement("a bare call statement"),
                Statement::If(_) => unsupported_statement("an if statement"),
                Statement::For(_) => unsupported_statement("a for loop"),
                Statement::Limit(_) => unsupported_statement("a limit statement"),
                Statement::Signal(_) => unsupported_statement("a signal statement"),
                Statement::Assignment { .. } => unreachable!(),
            });
        };
        if let Expression::Array(elements) = value {
            let mut assignments = Vec::new();
            self.array_assignment_context(target, elements, &mut Vec::new(), &mut assignments)?;
            return Ok(assignments);
        }
        if let Expression::Ref(source) = value
            && self.is_whole_array_reference(target)
            && self.is_whole_array_reference(source)
        {
            return Ok(vec![serde_json::json!({
                "kind": "copy",
                "target": self.reference_context(target)?,
                "source": self.reference_context(source)?,
            })]);
        }
        if let Some(dimensions) = self.whole_array_dimensions(target) {
            let mut assignments = Vec::new();
            self.array_expression_context(
                target,
                dimensions,
                value,
                &mut Vec::new(),
                &mut assignments,
            )?;
            return Ok(assignments);
        }
        Ok(vec![self.assignment_context(target, value)?])
    }

    fn array_assignment_context(
        &self,
        target: &Reference,
        elements: &[Expression],
        indices: &mut Vec<i64>,
        assignments: &mut Vec<serde_json::Value>,
    ) -> Result<(), GalecTargetError> {
        for (index, element) in elements.iter().enumerate() {
            let one_based =
                i64::try_from(index + 1).map_err(|_| GalecTargetError::LoweringInternal {
                    detail: "C export array index exceeds i64".to_owned(),
                })?;
            indices.push(one_based);
            match element {
                Expression::Array(nested) => {
                    self.array_assignment_context(target, nested, indices, assignments)?;
                }
                scalar => assignments.push(self.assignment_context(
                    &self.reference_with_static_subscripts(target, indices)?,
                    scalar,
                )?),
            }
            indices.pop();
        }
        Ok(())
    }

    fn array_expression_context(
        &self,
        target: &Reference,
        dimensions: &[i64],
        value: &Expression,
        indices: &mut Vec<i64>,
        assignments: &mut Vec<serde_json::Value>,
    ) -> Result<(), GalecTargetError> {
        let Some((first, rest)) = dimensions.split_first() else {
            let indexed_target = self.reference_with_static_subscripts(target, indices)?;
            let indexed_value = self.indexed_expression(value, indices)?;
            assignments.push(self.assignment_context(&indexed_target, &indexed_value)?);
            return Ok(());
        };
        let size = usize::try_from(*first)
            .ok()
            .filter(|size| *size >= 1)
            .ok_or_else(|| GalecTargetError::LoweringInternal {
                detail: format!("C export saw non-positive array dimension {first}"),
            })?;
        for index in 0..size {
            let one_based =
                i64::try_from(index + 1).map_err(|_| GalecTargetError::LoweringInternal {
                    detail: "C export array index exceeds i64".to_owned(),
                })?;
            indices.push(one_based);
            self.array_expression_context(target, rest, value, indices, assignments)?;
            indices.pop();
        }
        Ok(())
    }

    fn assignment_context(
        &self,
        target: &Reference,
        value: &Expression,
    ) -> Result<serde_json::Value, GalecTargetError> {
        Ok(serde_json::json!({
            "kind": "assign",
            "target": self.reference_context(target)?,
            "value": self.expression_context(value)?,
        }))
    }

    fn reference_context(
        &self,
        reference: &Reference,
    ) -> Result<serde_json::Value, GalecTargetError> {
        let Reference::State(parts) = reference else {
            return Err(GalecTargetError::CExportUnsupported {
                construct: "a local (non-`self.`) reference",
                detail: "the current DAE lowering emits no method locals or loop iterators"
                    .to_owned(),
            });
        };
        let [part] = parts.as_slice() else {
            return Err(GalecTargetError::CExportUnsupported {
                construct: "a multi-part state reference",
                detail: "the current DAE lowering emits no state compartments".to_owned(),
            });
        };
        let indices = part
            .subscripts
            .iter()
            .map(|subscript| match subscript {
                Expression::Integer(value) if *value >= 1 => Ok(serde_json::json!({
                    "kind": "literal",
                    "value": value - 1,
                })),
                Expression::Integer(value) => Err(GalecTargetError::LoweringInternal {
                    detail: format!(
                        "C export met the GALEC subscript {value}; valid subscripts are 1-based positive integers"
                    ),
                }),
                other => Ok(serde_json::json!({
                    "kind": "expression",
                    "value": self.expression_context(other)?,
                })),
            })
            .collect::<Result<Vec<_>, _>>()?;
        Ok(serde_json::json!({
            "name": self.names.c_name(&part.name)?,
            "indices": indices,
        }))
    }

    fn expression_context(
        &self,
        expression: &Expression,
    ) -> Result<serde_json::Value, GalecTargetError> {
        Ok(match expression {
            Expression::Bool(value) => serde_json::json!({"kind": "bool", "value": value}),
            Expression::Integer(value) => {
                validate_integer_literal(*value)?;
                serde_json::json!({"kind": "integer", "value": value})
            }
            Expression::Real(value) => {
                let literal = rumoca_ir_galec::format_real_literal(*value).map_err(|error| {
                    GalecTargetError::LoweringInternal {
                        detail: format!("C export met an unprintable Real literal: {error}"),
                    }
                })?;
                serde_json::json!({"kind": "real", "literal": literal, "negative": value.is_sign_negative()})
            }
            Expression::Ref(reference) => {
                serde_json::json!({"kind": "ref", "reference": self.reference_context(reference)?})
            }
            Expression::Call(call) => {
                let Name::Ident(function, _) = &call.function else {
                    return Err(GalecTargetError::LoweringInternal {
                        detail: "C export met a call to a quoted function name".to_owned(),
                    });
                };
                let Some((_, arity)) = crate::lower::emittable_builtin_targets()
                    .into_iter()
                    .find(|(name, _)| *name == function.as_str())
                else {
                    return Err(GalecTargetError::LoweringInternal {
                        detail: format!("C export has no mapping for `{}`", function.as_str()),
                    });
                };
                if call.arguments.len() != arity {
                    return Err(GalecTargetError::LoweringInternal {
                        detail: format!(
                            "C export met `{}` with {} argument(s), expected {arity}",
                            function.as_str(),
                            call.arguments.len()
                        ),
                    });
                }
                serde_json::json!({
                    "kind": "call",
                    "function": function.as_str(),
                    "arguments": call.arguments.iter()
                        .map(|arg| self.expression_context(arg))
                        .collect::<Result<Vec<_>, _>>()?,
                })
            }
            Expression::Paren(inner) => {
                serde_json::json!({"kind": "paren", "value": self.expression_context(inner)?})
            }
            Expression::If(if_expression) => serde_json::json!({
                "kind": "if",
                "branches": if_expression.branches.iter().map(|(condition, value)| {
                    Ok(serde_json::json!({
                        "condition": self.expression_context(condition)?,
                        "value": self.expression_context(value)?,
                    }))
                }).collect::<Result<Vec<_>, GalecTargetError>>()?,
                "else_value": self.expression_context(&if_expression.else_value)?,
            }),
            Expression::Neg(reference) => {
                serde_json::json!({"kind": "neg", "reference": self.reference_context(reference)?})
            }
            Expression::Not(inner) => {
                serde_json::json!({"kind": "not", "value": self.expression_context(inner)?})
            }
            Expression::Binary { op, lhs, rhs } => serde_json::json!({
                "kind": "binary",
                "operator": binary_op_name(*op),
                "lhs": self.expression_context(lhs)?,
                "rhs": self.expression_context(rhs)?,
            }),
            Expression::Size { .. } => {
                return Err(GalecTargetError::CExportUnsupported {
                    construct: "a `size(…)` expression",
                    detail: "the current DAE lowering never emits dimension queries".to_owned(),
                });
            }
            Expression::Array(_) => {
                return Err(GalecTargetError::CExportUnsupported {
                    construct: "an array constructor outside a whole-array assignment",
                    detail: "C has no array-valued expressions".to_owned(),
                });
            }
        })
    }

    fn indexed_expression(
        &self,
        expression: &Expression,
        indices: &[i64],
    ) -> Result<Expression, GalecTargetError> {
        if indices.is_empty() {
            return Ok(expression.clone());
        }
        match expression {
            Expression::Ref(reference) if self.is_whole_array_reference(reference) => Ok(
                Expression::Ref(self.reference_with_static_subscripts(reference, indices)?),
            ),
            Expression::Ref(_) => Ok(expression.clone()),
            Expression::Neg(reference) if self.is_whole_array_reference(reference) => Ok(
                Expression::Neg(self.reference_with_static_subscripts(reference, indices)?),
            ),
            Expression::Neg(_) => Ok(expression.clone()),
            Expression::Array(elements) => self.indexed_array_element(elements, indices),
            Expression::If(if_expression) => Ok(Expression::If(IfExpression {
                branches: if_expression
                    .branches
                    .iter()
                    .map(|(condition, value)| {
                        Ok((
                            condition.clone(),
                            self.index_value_if_array(value, indices)?,
                        ))
                    })
                    .collect::<Result<Vec<_>, GalecTargetError>>()?,
                else_value: Box::new(
                    self.index_value_if_array(&if_expression.else_value, indices)?,
                ),
            })),
            Expression::Paren(inner) if self.expression_needs_indexing(inner) => Ok(
                Expression::Paren(Box::new(self.indexed_expression(inner, indices)?)),
            ),
            Expression::Binary { op, lhs, rhs } => Ok(Expression::Binary {
                op: *op,
                lhs: Box::new(self.index_value_if_array(lhs, indices)?),
                rhs: Box::new(self.index_value_if_array(rhs, indices)?),
            }),
            Expression::Bool(_)
            | Expression::Integer(_)
            | Expression::Real(_)
            | Expression::Call(_)
            | Expression::Paren(_)
            | Expression::Not(_)
            | Expression::Size { .. } => Ok(expression.clone()),
        }
    }

    fn index_value_if_array(
        &self,
        expression: &Expression,
        indices: &[i64],
    ) -> Result<Expression, GalecTargetError> {
        if self.expression_needs_indexing(expression) {
            self.indexed_expression(expression, indices)
        } else {
            Ok(expression.clone())
        }
    }

    fn indexed_array_element(
        &self,
        elements: &[Expression],
        indices: &[i64],
    ) -> Result<Expression, GalecTargetError> {
        let Some((first, rest)) = indices.split_first() else {
            return Err(GalecTargetError::LoweringInternal {
                detail: "C export array element selection called without indices".to_owned(),
            });
        };
        let index = usize::try_from(*first)
            .ok()
            .filter(|index| *index >= 1 && *index <= elements.len())
            .ok_or_else(|| GalecTargetError::LoweringInternal {
                detail: format!(
                    "C export array constructor index {first} is outside 1..{}",
                    elements.len()
                ),
            })?;
        let selected = &elements[index - 1];
        if rest.is_empty() {
            return Ok(selected.clone());
        }
        if matches!(selected, Expression::Array(_)) || self.expression_needs_indexing(selected) {
            return self.indexed_expression(selected, rest);
        }
        Err(GalecTargetError::LoweringInternal {
            detail: "C export array constructor rank does not match target dimensions".to_owned(),
        })
    }

    fn expression_needs_indexing(&self, expression: &Expression) -> bool {
        match expression {
            Expression::Ref(reference) | Expression::Neg(reference) => {
                self.is_whole_array_reference(reference)
            }
            Expression::Array(_) => true,
            Expression::If(if_expression) => {
                if_expression
                    .branches
                    .iter()
                    .any(|(_, value)| self.expression_needs_indexing(value))
                    || self.expression_needs_indexing(&if_expression.else_value)
            }
            Expression::Paren(inner) | Expression::Not(inner) => {
                self.expression_needs_indexing(inner)
            }
            Expression::Binary { lhs, rhs, .. } => {
                self.expression_needs_indexing(lhs) || self.expression_needs_indexing(rhs)
            }
            Expression::Bool(_)
            | Expression::Integer(_)
            | Expression::Real(_)
            | Expression::Call(_)
            | Expression::Size { .. } => false,
        }
    }

    fn reference_with_static_subscripts(
        &self,
        reference: &Reference,
        indices: &[i64],
    ) -> Result<Reference, GalecTargetError> {
        let Reference::State(parts) = reference else {
            return Err(GalecTargetError::LoweringInternal {
                detail: "C export can only index whole-array state references".to_owned(),
            });
        };
        let [part] = parts.as_slice() else {
            return Err(GalecTargetError::LoweringInternal {
                detail: "C export can only index single-part state references".to_owned(),
            });
        };
        let mut part = part.clone();
        part.subscripts = indices
            .iter()
            .copied()
            .map(Expression::Integer)
            .collect::<Vec<_>>();
        Ok(Reference::State(vec![part]))
    }

    /// Literal dimensions when `reference` is a whole-array state reference:
    /// a single-part `self.x` reference with NO subscripts whose declaration is
    /// an array. Indexed elements and scalars return `None`.
    fn whole_array_dimensions(&self, reference: &Reference) -> Option<&[i64]> {
        let Reference::State(parts) = reference else {
            return None;
        };
        let [part] = parts.as_slice() else {
            return None;
        };
        if part.subscripts.is_empty() {
            self.names.array_dimensions(&part.name)
        } else {
            None
        }
    }

    /// Whether `reference` is a whole-array state reference.
    fn is_whole_array_reference(&self, reference: &Reference) -> bool {
        self.whole_array_dimensions(reference).is_some()
    }
}

/// GALEC Integer is C `int32_t`: literals outside its range are rejected,
/// never truncated (SPEC_0008).
fn validate_integer_literal(value: i64) -> Result<(), GalecTargetError> {
    if i32::try_from(value).is_err() {
        return Err(GalecTargetError::CExportUnsupported {
            construct: "an Integer literal beyond int32_t",
            detail: format!("literal {value} does not fit the C Integer type int32_t"),
        });
    }
    Ok(())
}

pub(crate) fn binary_op_name(op: BinaryOp) -> &'static str {
    match op {
        BinaryOp::Pow => "pow",
        BinaryOp::Mul => "mul",
        BinaryOp::Div => "div",
        BinaryOp::Add => "add",
        BinaryOp::Sub => "sub",
        BinaryOp::Lt => "lt",
        BinaryOp::Gt => "gt",
        BinaryOp::Le => "le",
        BinaryOp::Ge => "ge",
        BinaryOp::Eq => "eq",
        BinaryOp::Ne => "ne",
        BinaryOp::And => "and",
        BinaryOp::Or => "or",
    }
}

fn unsupported_statement(construct: &'static str) -> GalecTargetError {
    GalecTargetError::CExportUnsupported {
        construct,
        detail: "the current DAE lowering emits sequential assignments only \
                 (crate::lower); this statement kind cannot have come from it"
            .to_owned(),
    }
}
