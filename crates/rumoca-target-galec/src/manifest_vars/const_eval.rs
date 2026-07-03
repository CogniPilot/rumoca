//! Constant evaluator for manifest attribute expressions.
//!
//! DAE variable attributes (`start`, `min`, `max`, `nominal`) arrive as
//! `rumoca_core::Expression` trees, not scalars — e.g. `-1e5` is
//! `Unary Minus(Literal 1e5)` and a dependent parameter's defining
//! expression is preserved symbolically in `start` (`-limiterUMax`). The
//! manifest needs literal values, so this evaluator folds the small
//! Modelica-constant subset the projection accepts:
//!
//! - Real / Integer / Boolean literals;
//! - unary `+` / `-` / `not`;
//! - binary `+`, `-`, `*` (Integer stays Integer, mixed operands widen to
//!   Real per Modelica), `/` (always Real), `^` (always Real);
//! - references to *scalar* parameter/constant defaults, resolved
//!   recursively with cycle detection;
//! - array constructors (top level only, flattened row-major for `start`).
//!
//! Everything else fails with a diagnostic — evaluation never invents a
//! value (SPEC_0008). Evaluation produces **values** only; the variable's
//! scalar *type* always comes from classification, never from what the
//! expression evaluates to (S8).

use std::collections::HashMap;

use rumoca_core::{Expression, Literal, OpBinary, OpUnary, Span};
use rumoca_ir_dae::Variable;

use crate::classify::Classification;
use crate::diagnostic::GalecTargetError;

/// A folded constant value.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ConstValue {
    Real(f64),
    Integer(i64),
    Boolean(bool),
}

impl ConstValue {
    /// Human-readable type name for diagnostics.
    #[must_use]
    pub const fn type_name(&self) -> &'static str {
        match self {
            Self::Real(_) => "Real",
            Self::Integer(_) => "Integer",
            Self::Boolean(_) => "Boolean",
        }
    }

    /// Coerce to Real; Integer values widen (Modelica value conversion).
    /// The error payload is the expected-type name for diagnostics.
    pub fn as_real(&self) -> Result<f64, &'static str> {
        match self {
            Self::Real(value) => Ok(*value),
            Self::Integer(value) => Ok(*value as f64),
            Self::Boolean(_) => Err("Real"),
        }
    }

    /// Coerce to a 32-bit Integer (the manifest's Integer value space).
    pub fn as_i32(&self) -> Result<i32, &'static str> {
        match self {
            Self::Integer(value) => i32::try_from(*value).map_err(|_| "32-bit Integer"),
            Self::Real(_) | Self::Boolean(_) => Err("Integer"),
        }
    }

    /// Coerce to Boolean.
    pub fn as_boolean(&self) -> Result<bool, &'static str> {
        match self {
            Self::Boolean(value) => Ok(*value),
            Self::Real(_) | Self::Integer(_) => Err("Boolean"),
        }
    }
}

/// Shape of an evaluated `start` expression.
#[derive(Debug, Clone, PartialEq)]
pub enum StartShape {
    /// Scalar value (broadcast over array variables).
    Scalar(ConstValue),
    /// Row-major flattened array value.
    Array(Vec<ConstValue>),
}

/// Why evaluation failed; converted to a [`GalecTargetError`] with the
/// owning variable/attribute context by [`EvalFailure::into_error`].
#[derive(Debug, Clone, PartialEq)]
pub enum EvalFailure {
    /// The expression is outside the supported constant subset.
    NotEvaluable { reason: String, span: Option<Span> },
    /// Default expressions reference each other in a cycle.
    Cycle { through: String },
}

impl EvalFailure {
    fn not_evaluable(reason: impl Into<String>, span: Option<Span>) -> Self {
        Self::NotEvaluable {
            reason: reason.into(),
            span,
        }
    }

    /// Attach the owning variable/attribute context.
    #[must_use]
    pub fn into_error(self, variable: &Variable, attribute: &'static str) -> GalecTargetError {
        match self {
            Self::NotEvaluable { reason, span } => GalecTargetError::AttributeNotEvaluable {
                variable: variable.name.as_str().to_owned(),
                attribute,
                reason,
                span,
            },
            Self::Cycle { through } => GalecTargetError::StartDependencyCycle { through },
        }
    }
}

/// Evaluation environment: scalar parameter/constant defaults referencable
/// by name (tunable parameters, dependent parameters, and constants — the
/// classes whose `start` carries a default expression).
#[derive(Debug, Default)]
pub struct ConstEnv<'a> {
    definitions: HashMap<&'a str, &'a Variable>,
}

impl<'a> ConstEnv<'a> {
    /// Environment over a classification's parameter/constant defaults.
    #[must_use]
    pub fn from_classification(classification: &Classification<'a>) -> Self {
        use crate::classify::VariableClass as Class;
        let definitions = classification
            .variables
            .iter()
            .filter(|classified| {
                matches!(
                    classified.class,
                    Class::TunableParameter | Class::DependentParameter | Class::Constant
                )
            })
            .map(|classified| (classified.variable.name.as_str(), classified.variable))
            .collect();
        Self { definitions }
    }

    /// Evaluate an expression that must fold to one scalar constant.
    pub fn evaluate_scalar(&self, expr: &Expression) -> Result<ConstValue, EvalFailure> {
        self.eval(expr, &mut Vec::new())
    }

    /// Evaluate a `start` expression preserving its scalar/array shape.
    /// Array constructors are legal at the top level only and flatten
    /// row-major (nested constructors are matrix rows).
    pub fn evaluate_start_shape(&self, expr: &Expression) -> Result<StartShape, EvalFailure> {
        if let Expression::Array { .. } = expr {
            let mut values = Vec::new();
            self.flatten_array(expr, &mut values)?;
            Ok(StartShape::Array(values))
        } else {
            self.evaluate_scalar(expr).map(StartShape::Scalar)
        }
    }

    fn flatten_array(
        &self,
        expr: &Expression,
        values: &mut Vec<ConstValue>,
    ) -> Result<(), EvalFailure> {
        if let Expression::Array { elements, .. } = expr {
            for element in elements {
                self.flatten_array(element, values)?;
            }
            Ok(())
        } else {
            values.push(self.eval(expr, &mut Vec::new())?);
            Ok(())
        }
    }

    fn eval(
        &self,
        expr: &Expression,
        visiting: &mut Vec<&'a str>,
    ) -> Result<ConstValue, EvalFailure> {
        match expr {
            Expression::Literal { value, span } => eval_literal(value, *span),
            Expression::Unary { op, rhs, span } => {
                let value = self.eval(rhs, visiting)?;
                eval_unary(op.clone(), value, *span)
            }
            Expression::Binary { op, lhs, rhs, span } => {
                let lhs = self.eval(lhs, visiting)?;
                let rhs = self.eval(rhs, visiting)?;
                eval_binary(op.clone(), lhs, rhs, *span)
            }
            Expression::VarRef {
                name,
                subscripts,
                span,
            } => self.eval_reference(name.as_str(), subscripts.is_empty(), *span, visiting),
            other => Err(EvalFailure::not_evaluable(
                "expression form is not supported in constant evaluation \
                 (supported: literals, unary +/-/not, +, -, *, /, ^, and \
                 references to scalar parameter/constant defaults)",
                other.span(),
            )),
        }
    }

    fn eval_reference(
        &self,
        name: &str,
        unsubscripted: bool,
        span: Span,
        visiting: &mut Vec<&'a str>,
    ) -> Result<ConstValue, EvalFailure> {
        let span = optional(span);
        if !unsubscripted {
            return Err(EvalFailure::not_evaluable(
                format!("subscripted reference `{name}` is not supported in constant evaluation"),
                span,
            ));
        }
        let Some((key, definition)) = self.definitions.get_key_value(name) else {
            return Err(EvalFailure::not_evaluable(
                format!("`{name}` is not a scalar parameter or constant with a default"),
                span,
            ));
        };
        if !definition.dims.is_empty() {
            return Err(EvalFailure::not_evaluable(
                format!("`{name}` is array-valued; only scalar references fold"),
                span,
            ));
        }
        if visiting.contains(key) {
            return Err(EvalFailure::Cycle {
                through: name.to_owned(),
            });
        }
        let Some(default) = &definition.start else {
            return Err(EvalFailure::not_evaluable(
                format!("`{name}` has no default expression to fold"),
                span,
            ));
        };
        visiting.push(key);
        let value = self.eval(default, visiting);
        visiting.pop();
        value
    }
}

fn optional(span: Span) -> Option<Span> {
    (!span.is_dummy()).then_some(span)
}

fn eval_literal(value: &Literal, span: Span) -> Result<ConstValue, EvalFailure> {
    match value {
        Literal::Real(value) => Ok(ConstValue::Real(*value)),
        Literal::Integer(value) => Ok(ConstValue::Integer(*value)),
        Literal::Boolean(value) => Ok(ConstValue::Boolean(*value)),
        Literal::String(_) => Err(EvalFailure::not_evaluable(
            "String literals have no GALEC value (GALEC has no String type)",
            optional(span),
        )),
    }
}

fn eval_unary(op: OpUnary, value: ConstValue, span: Span) -> Result<ConstValue, EvalFailure> {
    let span = optional(span);
    let mistyped = |op: &str, value: ConstValue| {
        EvalFailure::not_evaluable(
            format!("unary `{op}` applied to a {} value", value.type_name()),
            span,
        )
    };
    match op {
        OpUnary::Minus | OpUnary::DotMinus => match value {
            ConstValue::Real(v) => Ok(ConstValue::Real(-v)),
            ConstValue::Integer(v) => v
                .checked_neg()
                .map(ConstValue::Integer)
                .ok_or_else(|| mistyped("-", value)),
            ConstValue::Boolean(_) => Err(mistyped("-", value)),
        },
        OpUnary::Plus | OpUnary::DotPlus => match value {
            ConstValue::Real(_) | ConstValue::Integer(_) => Ok(value),
            ConstValue::Boolean(_) => Err(mistyped("+", value)),
        },
        OpUnary::Not => match value {
            ConstValue::Boolean(v) => Ok(ConstValue::Boolean(!v)),
            _ => Err(mistyped("not", value)),
        },
        OpUnary::Empty => Err(EvalFailure::not_evaluable("empty unary operator", span)),
    }
}

fn eval_binary(
    op: OpBinary,
    lhs: ConstValue,
    rhs: ConstValue,
    span: Span,
) -> Result<ConstValue, EvalFailure> {
    let span = optional(span);
    match op {
        OpBinary::Add | OpBinary::AddElem => {
            numeric(op, lhs, rhs, i64::checked_add, |a, b| a + b, span)
        }
        OpBinary::Sub | OpBinary::SubElem => {
            numeric(op, lhs, rhs, i64::checked_sub, |a, b| a - b, span)
        }
        OpBinary::Mul | OpBinary::MulElem => {
            numeric(op, lhs, rhs, i64::checked_mul, |a, b| a * b, span)
        }
        // Modelica `/` and `^` always produce Real.
        OpBinary::Div | OpBinary::DivElem => real_only(op, lhs, rhs, |a, b| a / b, span),
        OpBinary::Exp | OpBinary::ExpElem => real_only(op, lhs, rhs, f64::powf, span),
        other => Err(EvalFailure::not_evaluable(
            format!("binary operator `{other:?}` is not supported in constant evaluation"),
            span,
        )),
    }
}

/// Integer-preserving numeric operator: Integer×Integer stays Integer
/// (checked, overflow fails); any Real operand widens the result to Real.
fn numeric(
    op: OpBinary,
    lhs: ConstValue,
    rhs: ConstValue,
    int_op: impl Fn(i64, i64) -> Option<i64>,
    real_op: impl Fn(f64, f64) -> f64,
    span: Option<Span>,
) -> Result<ConstValue, EvalFailure> {
    if let (ConstValue::Integer(a), ConstValue::Integer(b)) = (lhs, rhs) {
        return int_op(a, b).map(ConstValue::Integer).ok_or_else(|| {
            EvalFailure::not_evaluable(format!("Integer overflow in `{op:?}`"), span)
        });
    }
    real_only(op, lhs, rhs, real_op, span)
}

fn real_only(
    op: OpBinary,
    lhs: ConstValue,
    rhs: ConstValue,
    real_op: impl Fn(f64, f64) -> f64,
    span: Option<Span>,
) -> Result<ConstValue, EvalFailure> {
    let operand = |value: ConstValue| {
        value.as_real().map_err(|_| {
            EvalFailure::not_evaluable(
                format!("binary `{op:?}` applied to a {} value", value.type_name()),
                span,
            )
        })
    };
    Ok(ConstValue::Real(real_op(operand(lhs)?, operand(rhs)?)))
}
