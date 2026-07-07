//! Typed DAE-expression → GALEC-expression lowering.
//!
//! GALEC has no implicit `Integer`↔`Real` promotion (trap T5), so lowering
//! tracks the scalar type of every subexpression and inserts explicit
//! `real(…)` widening casts where Modelica would promote implicitly.
//! Narrowing (`Real` → `Integer`) is never inserted: GALEC's `integer()`
//! truncates toward zero and can signal `NAN`/`OVERFLOW` while Modelica's
//! `integer()` floors, so any required narrowing conversion is rejected as
//! an unsupported feature until the projection implements the
//! floor-semantics rewrite (`integer(roundDown(x))`) together with the
//! escape-set accounting its signals require (see the D8 notes in
//! [`crate::lower`]).
//!
//! Builtin calls map through [`BUILTIN_MAP`] — the T8 table as data.
//! Modelica names absent from the table (or absent from the GALEC §3.2.6
//! catalog entirely, like `mod`/`rem` whose GALEC counterpart
//! `remainderDown` is only Appendix-C reserved) fail with stable
//! `unsupported-feature:` diagnostics; nothing is emitted that the catalog
//! does not define (GAL-005).
//!
//! References:
//!
//! - `c[i]` (the generated condition vector) inlines to the defining `f_c`
//!   Boolean expression — condition bookkeeping never survives into emitted
//!   code;
//! - `__pre__.x` becomes a read of the protected state `'previous(x)'`
//!   (trap T2) and is recorded in the referenced-pre set so exactly the
//!   read slots become states with end-of-`DoStep` commits;
//! - everything else resolves through the GAL-020 classification to a
//!   `self.<name>` state reference.

mod function_inline;
mod references;

use std::collections::HashSet;

use rumoca_core::{
    BuiltinFunction, ComprehensionIndex, Expression, Function, Literal, OpBinary, OpUnary, Span,
    Subscript, VarName,
};
use rumoca_ir_dae::DaeSymbolTable;
use rumoca_ir_galec::ast::{
    self as gast, BinaryOp, FunctionCall, IfExpression, Name, RefPart, Reference, ScalarType,
};

use crate::classify::Classification;
use crate::diagnostic::GalecTargetError;
use crate::lower::conditions::ConditionTable;
use function_inline::{inline_function_call, inline_function_output_call};

/// A lowered expression together with its GALEC scalar (element) type.
pub(crate) struct Typed {
    pub expr: gast::Expression,
    /// GALEC scalar element type.
    pub ty: ScalarType,
    /// Empty means scalar; non-empty means an array expression with literal
    /// dimensions in row-major order.
    pub shape: Vec<i64>,
}

impl Typed {
    fn new(expr: gast::Expression, ty: ScalarType) -> Self {
        Self {
            expr,
            ty,
            shape: Vec::new(),
        }
    }

    fn array(expr: gast::Expression, ty: ScalarType, shape: Vec<i64>) -> Self {
        Self { expr, ty, shape }
    }

    fn is_scalar(&self) -> bool {
        self.shape.is_empty()
    }

    fn rank(&self) -> usize {
        self.shape.len()
    }
}

enum InlineFunctionTarget {
    Whole {
        function: Function,
    },
    Output {
        function: Function,
        output_name: String,
    },
}

impl InlineFunctionTarget {
    fn active_name(&self) -> &str {
        match self {
            Self::Whole { function } | Self::Output { function, .. } => function.name.as_str(),
        }
    }

    fn inline(&self, args: &[Expression], span: Span) -> Result<Expression, GalecTargetError> {
        match self {
            Self::Whole { function } => inline_function_call(function, args, span),
            Self::Output {
                function,
                output_name,
            } => inline_function_output_call(function, args, output_name, span),
        }
    }
}

/// The generated `__pre__.` slots read by lowered code, keyed by the DAE
/// slot name (e.g. `__pre__.pidIx`). Membership only — emission order of
/// declarations and commits comes from classification order.
#[derive(Default)]
pub(crate) struct ReferencedPre {
    members: HashSet<String>,
}

impl ReferencedPre {
    fn record(&mut self, slot_name: &str) {
        self.members.insert(slot_name.to_owned());
    }

    pub(crate) fn contains(&self, slot_name: &str) -> bool {
        self.members.contains(slot_name)
    }
}

/// Expression lowerer over one classification + condition table.
pub(crate) struct ExprLowerer<'a> {
    classification: &'a Classification<'a>,
    conditions: &'a ConditionTable<'a>,
    functions: &'a DaeSymbolTable,
    referenced_pre: ReferencedPre,
    /// Condition slots currently being inlined (cycle guard).
    inlining: Vec<usize>,
    /// User-defined functions currently being inlined (recursion guard).
    inlined_functions: Vec<String>,
}

impl<'a> ExprLowerer<'a> {
    pub(crate) fn new(
        classification: &'a Classification<'a>,
        conditions: &'a ConditionTable<'a>,
        functions: &'a DaeSymbolTable,
    ) -> Self {
        Self {
            classification,
            conditions,
            functions,
            referenced_pre: ReferencedPre::default(),
            inlining: Vec::new(),
            inlined_functions: Vec::new(),
        }
    }

    /// The `__pre__.` slots read by everything lowered so far.
    pub(crate) fn into_referenced_pre(self) -> ReferencedPre {
        self.referenced_pre
    }

    /// Lower one DAE expression to a typed GALEC expression.
    pub(crate) fn lower(&mut self, expr: &Expression) -> Result<Typed, GalecTargetError> {
        match expr {
            Expression::Literal { value, span } => lower_literal(value, *span),
            Expression::VarRef {
                name,
                subscripts,
                span,
            } => self.lower_var_ref(name.as_str(), subscripts, *span),
            Expression::Unary { op, rhs, span } => self.lower_unary(op, rhs, *span),
            Expression::Binary { op, lhs, rhs, span } => self.lower_binary(op, lhs, rhs, *span),
            Expression::If {
                branches,
                else_branch,
                span,
            } => self.lower_if(branches, else_branch, *span),
            Expression::BuiltinCall {
                function,
                args,
                span,
            } => self.lower_builtin(*function, args, *span),
            Expression::FunctionCall {
                name,
                args,
                is_constructor,
                span,
            } => self.lower_function_call(name, args, *is_constructor, *span),
            Expression::Array { elements, span, .. } => self.lower_array(elements, *span),
            Expression::Index {
                base,
                subscripts,
                span,
            } => self.lower_index(base, subscripts, *span),
            other => Err(unsupported(
                "expression-form".to_owned(),
                format!("expression form {} in a lowered position", form_name(other)),
                other.span(),
            )),
        }
    }

    /// Lower an expression and widen it to `Real` with an explicit `real()`
    /// cast when it is `Integer` (trap T5).
    pub(crate) fn lower_as_real(
        &mut self,
        expr: &Expression,
        context: &str,
    ) -> Result<gast::Expression, GalecTargetError> {
        let typed = self.lower(expr)?;
        if !typed.is_scalar() {
            return Err(GalecTargetError::LoweringTypeMismatch {
                context: context.to_owned(),
                expected: "Real",
                found: "array",
                span: expr.span(),
            });
        }
        widen_to_real(typed, context, expr.span())
    }

    /// Lower an expression that must be Boolean.
    fn lower_as_boolean(
        &mut self,
        expr: &Expression,
        context: &str,
    ) -> Result<gast::Expression, GalecTargetError> {
        let typed = self.lower(expr)?;
        if typed.ty != ScalarType::Boolean || !typed.is_scalar() {
            return Err(mismatch(context, "Boolean", typed.ty, expr.span()));
        }
        Ok(typed.expr)
    }

    // -----------------------------------------------------------------
    // Operators
    // -----------------------------------------------------------------

    fn lower_unary(
        &mut self,
        op: &OpUnary,
        rhs: &Expression,
        span: Span,
    ) -> Result<Typed, GalecTargetError> {
        let typed = self.lower(rhs)?;
        match op {
            OpUnary::Minus | OpUnary::DotMinus => match typed.ty {
                ScalarType::Real => Ok(Typed::array(
                    gast::Expression::negated_real(typed.expr),
                    ScalarType::Real,
                    typed.shape,
                )),
                ScalarType::Integer => Ok(Typed::array(
                    gast::Expression::negated_integer(typed.expr),
                    ScalarType::Integer,
                    typed.shape,
                )),
                ScalarType::Boolean => Err(mismatch(
                    "unary minus operand",
                    "numeric",
                    typed.ty,
                    Some(span),
                )),
            },
            OpUnary::Plus | OpUnary::DotPlus => match typed.ty {
                ScalarType::Real | ScalarType::Integer => Ok(typed),
                ScalarType::Boolean => Err(mismatch(
                    "unary plus operand",
                    "numeric",
                    typed.ty,
                    Some(span),
                )),
            },
            OpUnary::Not => {
                if typed.ty != ScalarType::Boolean || !typed.is_scalar() {
                    return Err(mismatch("not operand", "Boolean", typed.ty, Some(span)));
                }
                Ok(Typed::new(
                    gast::Expression::Not(Box::new(typed.expr)),
                    ScalarType::Boolean,
                ))
            }
            OpUnary::Empty => Err(GalecTargetError::LoweringInternal {
                detail: "empty unary operator in canonical DAE expression".to_owned(),
            }),
        }
    }

    fn lower_binary(
        &mut self,
        op: &OpBinary,
        lhs: &Expression,
        rhs: &Expression,
        span: Span,
    ) -> Result<Typed, GalecTargetError> {
        let left = self.lower(lhs)?;
        let right = self.lower(rhs)?;
        match op {
            OpBinary::Add | OpBinary::AddElem => self.arithmetic(BinaryOp::Add, left, right, span),
            OpBinary::Sub | OpBinary::SubElem => self.arithmetic(BinaryOp::Sub, left, right, span),
            OpBinary::Mul => {
                if vector_dot_shape(&left, &right).is_some() {
                    return self.lower_vector_dot(lhs, rhs, span);
                }
                if !left.is_scalar() && !right.is_scalar() {
                    return Err(unsupported(
                        "array-multiplication".to_owned(),
                        "non-vector array `*` requires Modelica matrix/vector product semantics; \
                         use element-wise `.*` or wait for explicit matrix-product lowering"
                            .to_owned(),
                        Some(span),
                    ));
                }
                self.arithmetic(BinaryOp::Mul, left, right, span)
            }
            OpBinary::MulElem => self.arithmetic(BinaryOp::Mul, left, right, span),
            // Modelica `/` and GALEC `/` are both Real division; GALEC
            // additionally requires Real-typed operands (trap T5).
            OpBinary::Div | OpBinary::DivElem => self.real_arithmetic_div(left, right, span),
            // GALEC `^` accepts numeric operands and returns Real.
            OpBinary::Exp | OpBinary::ExpElem => {
                require_numeric(&left, "`^` operand", span)?;
                require_numeric(&right, "`^` operand", span)?;
                let shape = arithmetic_shape(&left, &right, span)?;
                let left = widen_to_real(left, "`^` operand", Some(span))?;
                let right = widen_to_real(right, "`^` operand", Some(span))?;
                Ok(Typed::array(
                    gast::Expression::binary(BinaryOp::Pow, left, right),
                    ScalarType::Real,
                    shape,
                ))
            }
            OpBinary::Lt => self.comparison(BinaryOp::Lt, left, right, span),
            OpBinary::Le => self.comparison(BinaryOp::Le, left, right, span),
            OpBinary::Gt => self.comparison(BinaryOp::Gt, left, right, span),
            OpBinary::Ge => self.comparison(BinaryOp::Ge, left, right, span),
            OpBinary::Eq => self.equality(BinaryOp::Eq, left, right, span),
            OpBinary::Neq => self.equality(BinaryOp::Ne, left, right, span),
            OpBinary::And => self.logical(BinaryOp::And, left, right, span),
            OpBinary::Or => self.logical(BinaryOp::Or, left, right, span),
            OpBinary::Empty | OpBinary::Assign => Err(GalecTargetError::LoweringInternal {
                detail: format!("binary operator `{op:?}` in canonical DAE expression"),
            }),
        }
    }

    fn real_arithmetic_div(
        &mut self,
        left: Typed,
        right: Typed,
        span: Span,
    ) -> Result<Typed, GalecTargetError> {
        let shape = arithmetic_shape(&left, &right, span)?;
        let left = widen_to_real(left, "`/` operand", Some(span))?;
        let right = widen_to_real(right, "`/` operand", Some(span))?;
        Ok(Typed::array(
            gast::Expression::binary(BinaryOp::Div, left, right),
            ScalarType::Real,
            shape,
        ))
    }

    fn lower_vector_dot(
        &mut self,
        lhs: &Expression,
        rhs: &Expression,
        span: Span,
    ) -> Result<Typed, GalecTargetError> {
        let left = self.lower(lhs)?;
        let right = self.lower(rhs)?;
        let Some(length) = vector_dot_shape(&left, &right) else {
            return Err(GalecTargetError::LoweringInternal {
                detail: "vector dot lowering called for non-vector operands".to_owned(),
            });
        };
        let mut result = None;
        for index in 1..=length {
            let left = self.lower(&indexed_expression(
                lhs.to_owned(),
                vec![Subscript::index(index, span)],
                span,
            ))?;
            let right = self.lower(&indexed_expression(
                rhs.to_owned(),
                vec![Subscript::index(index, span)],
                span,
            ))?;
            let product = self.arithmetic(BinaryOp::Mul, left, right, span)?;
            result = Some(match result {
                Some(acc) => self.arithmetic(BinaryOp::Add, acc, product, span)?,
                None => product,
            });
        }
        result.ok_or_else(|| GalecTargetError::LoweringInternal {
            detail: "vector dot product over an empty vector".to_owned(),
        })
    }

    /// `+`/`-`/`*`: Integer×Integer stays Integer; mixed operands widen to
    /// Real via explicit `real()` casts (trap T5).
    fn arithmetic(
        &mut self,
        op: BinaryOp,
        left: Typed,
        right: Typed,
        span: Span,
    ) -> Result<Typed, GalecTargetError> {
        let context = "arithmetic operand";
        require_numeric(&left, context, span)?;
        require_numeric(&right, context, span)?;
        let shape = arithmetic_shape(&left, &right, span)?;
        if left.ty == ScalarType::Integer && right.ty == ScalarType::Integer {
            return Ok(Typed::array(
                gast::Expression::binary(op, left.expr, right.expr),
                ScalarType::Integer,
                shape,
            ));
        }
        let left = widen_to_real(left, context, Some(span))?;
        let right = widen_to_real(right, context, Some(span))?;
        Ok(Typed::array(
            gast::Expression::binary(op, left, right),
            ScalarType::Real,
            shape,
        ))
    }

    /// Real relationals lower with **empty escape-set accounting** — the
    /// documented slice-1 stance; see the D8 notes on [`crate::lower`].
    fn comparison(
        &mut self,
        op: BinaryOp,
        left: Typed,
        right: Typed,
        span: Span,
    ) -> Result<Typed, GalecTargetError> {
        let context = "relational operand";
        require_numeric(&left, context, span)?;
        require_numeric(&right, context, span)?;
        require_scalar(&left, context, span)?;
        require_scalar(&right, context, span)?;
        let (left, right) = equalize_numeric(left, right, context, span)?;
        Ok(Typed::new(
            gast::Expression::binary(op, left, right),
            ScalarType::Boolean,
        ))
    }

    fn equality(
        &mut self,
        op: BinaryOp,
        left: Typed,
        right: Typed,
        span: Span,
    ) -> Result<Typed, GalecTargetError> {
        require_scalar(&left, "equality operand", span)?;
        require_scalar(&right, "equality operand", span)?;
        if left.ty == ScalarType::Boolean && right.ty == ScalarType::Boolean {
            return Ok(Typed::new(
                gast::Expression::binary(op, left.expr, right.expr),
                ScalarType::Boolean,
            ));
        }
        self.comparison(op, left, right, span)
    }

    fn logical(
        &mut self,
        op: BinaryOp,
        left: Typed,
        right: Typed,
        span: Span,
    ) -> Result<Typed, GalecTargetError> {
        for operand in [&left, &right] {
            if operand.ty != ScalarType::Boolean || !operand.is_scalar() {
                return Err(mismatch(
                    "logical operand",
                    "Boolean",
                    operand.ty,
                    Some(span),
                ));
            }
        }
        Ok(Typed::new(
            gast::Expression::binary(op, left.expr, right.expr),
            ScalarType::Boolean,
        ))
    }

    // -----------------------------------------------------------------
    // If-expressions, arrays, builtins
    // -----------------------------------------------------------------

    fn lower_if(
        &mut self,
        branches: &[(Expression, Expression)],
        else_branch: &Expression,
        span: Span,
    ) -> Result<Typed, GalecTargetError> {
        if branches.is_empty() {
            return Err(GalecTargetError::LoweringInternal {
                detail: "if-expression without branches in canonical DAE".to_owned(),
            });
        }
        let mut lowered: Vec<(gast::Expression, Typed)> = Vec::with_capacity(branches.len());
        for (condition, value) in branches {
            let condition = self.lower_as_boolean(condition, "if-expression condition")?;
            lowered.push((condition, self.lower(value)?));
        }
        let else_value = self.lower(else_branch)?;
        let result_ty =
            unify_branch_types(lowered.iter().map(|(_, value)| value), &else_value, span)?;
        let result_shape =
            unify_branch_shapes(lowered.iter().map(|(_, value)| value), &else_value, span)?;
        let branches = lowered
            .into_iter()
            .map(|(condition, value)| {
                Ok((
                    condition,
                    coerce_branch(value, result_ty, &result_shape, span)?,
                ))
            })
            .collect::<Result<Vec<_>, GalecTargetError>>()?;
        let else_value = coerce_branch(else_value, result_ty, &result_shape, span)?;
        Ok(Typed::array(
            gast::Expression::If(IfExpression {
                branches,
                else_value: Box::new(else_value),
            }),
            result_ty,
            result_shape,
        ))
    }

    fn lower_array(
        &mut self,
        elements: &[Expression],
        span: Span,
    ) -> Result<Typed, GalecTargetError> {
        if elements.is_empty() {
            return Err(unsupported(
                "empty-array".to_owned(),
                "empty array constructor".to_owned(),
                Some(span),
            ));
        }
        let lowered = elements
            .iter()
            .map(|element| self.lower(element))
            .collect::<Result<Vec<_>, _>>()?;
        let element_ty = lowered[0].ty;
        let element_shape = lowered[0].shape.clone();
        for element in &lowered {
            if element.ty != element_ty || element.shape != element_shape {
                return Err(mismatch(
                    "array constructor elements",
                    "same element type and shape",
                    element.ty,
                    Some(span),
                ));
            }
        }
        let mut shape = vec![i64::try_from(elements.len()).map_err(|_| {
            GalecTargetError::LoweringInternal {
                detail: "array constructor length exceeds i64".to_owned(),
            }
        })?];
        shape.extend(element_shape);
        Ok(Typed::array(
            gast::Expression::Array(lowered.into_iter().map(|typed| typed.expr).collect()),
            element_ty,
            shape,
        ))
    }

    fn lower_function_call(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[Expression],
        is_constructor: bool,
        span: Span,
    ) -> Result<Typed, GalecTargetError> {
        self.lower_inlined_function_call(name, args, is_constructor, span, |lowerer, expression| {
            lowerer.lower(&expression)
        })
    }

    fn lower_inlined_function_call(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[Expression],
        is_constructor: bool,
        span: Span,
        lower: impl FnOnce(&mut Self, Expression) -> Result<Typed, GalecTargetError>,
    ) -> Result<Typed, GalecTargetError> {
        if name.as_str() == rumoca_core::INTERNAL_SAMPLE_FUNCTION_NAME {
            return Err(unsupported(
                "sample-in-expression".to_owned(),
                format!("call to `{}` in a lowered expression", name.as_str()),
                Some(span),
            ));
        }
        if is_constructor {
            return Err(unsupported(
                format!("user-function-constructor:{}", name.as_str()),
                format!(
                    "constructor call `{}` in a lowered expression",
                    name.as_str()
                ),
                Some(span),
            ));
        }
        let args = self.inline_expression_function_calls_in_slice(args)?;
        let Some(target) = self.inline_function_target(name.as_str()) else {
            return Err(unsupported(
                format!("user-function-call:{}", name.as_str()),
                format!("call to `{}` in a lowered expression", name.as_str()),
                Some(span),
            ));
        };
        let active_name = target.active_name().to_owned();
        if self
            .inlined_functions
            .iter()
            .any(|active| active == active_name.as_str())
        {
            return Err(unsupported(
                format!("recursive-user-function:{}", name.as_str()),
                format!(
                    "recursive call to `{}` in a lowered expression",
                    name.as_str()
                ),
                Some(span),
            ));
        }
        self.inlined_functions.push(active_name);
        let expression = target.inline(&args, span);
        let result = match expression {
            Ok(expression) => lower(self, expression),
            Err(error) => Err(error),
        };
        self.inlined_functions.pop();
        result
    }

    fn inline_expression_function_calls_in_slice(
        &mut self,
        expressions: &[Expression],
    ) -> Result<Vec<Expression>, GalecTargetError> {
        expressions
            .iter()
            .map(|expr| self.inline_expression_function_calls(expr))
            .collect()
    }

    fn inline_expression_function_calls(
        &mut self,
        expr: &Expression,
    ) -> Result<Expression, GalecTargetError> {
        match expr {
            Expression::Binary { op, lhs, rhs, span } => Ok(Expression::Binary {
                op: op.clone(),
                lhs: Box::new(self.inline_expression_function_calls(lhs)?),
                rhs: Box::new(self.inline_expression_function_calls(rhs)?),
                span: *span,
            }),
            Expression::Unary { op, rhs, span } => Ok(Expression::Unary {
                op: op.clone(),
                rhs: Box::new(self.inline_expression_function_calls(rhs)?),
                span: *span,
            }),
            Expression::VarRef {
                name,
                subscripts,
                span,
            } => Ok(Expression::VarRef {
                name: name.clone(),
                subscripts: self.inline_function_calls_in_subscripts(subscripts)?,
                span: *span,
            }),
            Expression::BuiltinCall {
                function,
                args,
                span,
            } => Ok(Expression::BuiltinCall {
                function: *function,
                args: self.inline_expression_function_calls_in_slice(args)?,
                span: *span,
            }),
            Expression::FunctionCall {
                name,
                args,
                is_constructor,
                span,
            } => self.inline_expression_function_call_expr(name, args, *is_constructor, *span),
            Expression::Literal { value, span } => Ok(Expression::Literal {
                value: value.clone(),
                span: *span,
            }),
            Expression::If {
                branches,
                else_branch,
                span,
            } => self.inline_function_calls_in_if(branches, else_branch, *span),
            Expression::Array {
                elements,
                is_matrix,
                span,
            } => Ok(Expression::Array {
                elements: self.inline_expression_function_calls_in_slice(elements)?,
                is_matrix: *is_matrix,
                span: *span,
            }),
            Expression::Tuple { elements, span } => Ok(Expression::Tuple {
                elements: self.inline_expression_function_calls_in_slice(elements)?,
                span: *span,
            }),
            Expression::Range {
                start,
                step,
                end,
                span,
            } => Ok(Expression::Range {
                start: Box::new(self.inline_expression_function_calls(start)?),
                step: step
                    .as_deref()
                    .map(|expr| self.inline_expression_function_calls(expr).map(Box::new))
                    .transpose()?,
                end: Box::new(self.inline_expression_function_calls(end)?),
                span: *span,
            }),
            Expression::ArrayComprehension {
                expr,
                indices,
                filter,
                span,
            } => self.inline_function_calls_in_comprehension(expr, indices, filter, *span),
            Expression::Index {
                base,
                subscripts,
                span,
            } => Ok(Expression::Index {
                base: Box::new(self.inline_expression_function_calls(base)?),
                subscripts: self.inline_function_calls_in_subscripts(subscripts)?,
                span: *span,
            }),
            Expression::FieldAccess { base, field, span } => Ok(Expression::FieldAccess {
                base: Box::new(self.inline_expression_function_calls(base)?),
                field: field.clone(),
                span: *span,
            }),
            Expression::Empty { span } => Ok(Expression::Empty { span: *span }),
        }
    }

    fn inline_expression_function_call_expr(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[Expression],
        is_constructor: bool,
        span: Span,
    ) -> Result<Expression, GalecTargetError> {
        let args = self.inline_expression_function_calls_in_slice(args)?;
        if is_constructor || name.as_str() == rumoca_core::INTERNAL_SAMPLE_FUNCTION_NAME {
            return Ok(Expression::FunctionCall {
                name: name.clone(),
                args,
                is_constructor,
                span,
            });
        }
        let Some(target) = self.inline_function_target(name.as_str()) else {
            return Ok(Expression::FunctionCall {
                name: name.clone(),
                args,
                is_constructor,
                span,
            });
        };
        self.inline_user_function_expression(name.as_str(), target, &args, span)
    }

    fn inline_user_function_expression(
        &mut self,
        name: &str,
        target: InlineFunctionTarget,
        args: &[Expression],
        span: Span,
    ) -> Result<Expression, GalecTargetError> {
        let active_name = target.active_name().to_owned();
        if self
            .inlined_functions
            .iter()
            .any(|active| active == active_name.as_str())
        {
            return Err(unsupported(
                format!("recursive-user-function:{name}"),
                format!("recursive call to `{name}` in a lowered expression"),
                Some(span),
            ));
        }
        self.inlined_functions.push(active_name);
        let expression = target.inline(args, span);
        let result = match expression {
            Ok(expression) => self.inline_expression_function_calls(&expression),
            Err(error) => Err(error),
        };
        self.inlined_functions.pop();
        result
    }

    fn inline_function_target(&self, name: &str) -> Option<InlineFunctionTarget> {
        if let Some(function) = self.functions.functions.get(&VarName::new(name)) {
            return Some(InlineFunctionTarget::Whole {
                function: function.clone(),
            });
        }
        rumoca_core::find_map_top_level_splits_rev(name, |base, suffix| {
            let function = self.functions.functions.get(&VarName::new(base))?;
            function
                .outputs
                .iter()
                .any(|output| output.name == suffix)
                .then(|| InlineFunctionTarget::Output {
                    function: function.clone(),
                    output_name: suffix.to_owned(),
                })
        })
    }

    fn inline_function_calls_in_if(
        &mut self,
        branches: &[(Expression, Expression)],
        else_branch: &Expression,
        span: Span,
    ) -> Result<Expression, GalecTargetError> {
        let branches = branches
            .iter()
            .map(|(condition, value)| {
                Ok((
                    self.inline_expression_function_calls(condition)?,
                    self.inline_expression_function_calls(value)?,
                ))
            })
            .collect::<Result<Vec<_>, GalecTargetError>>()?;
        Ok(Expression::If {
            branches,
            else_branch: Box::new(self.inline_expression_function_calls(else_branch)?),
            span,
        })
    }

    fn inline_function_calls_in_comprehension(
        &mut self,
        expr: &Expression,
        indices: &[ComprehensionIndex],
        filter: &Option<Box<Expression>>,
        span: Span,
    ) -> Result<Expression, GalecTargetError> {
        let indices = indices
            .iter()
            .map(|index| {
                Ok(ComprehensionIndex {
                    name: index.name.clone(),
                    range: self.inline_expression_function_calls(&index.range)?,
                })
            })
            .collect::<Result<Vec<_>, GalecTargetError>>()?;
        Ok(Expression::ArrayComprehension {
            expr: Box::new(self.inline_expression_function_calls(expr)?),
            indices,
            filter: filter
                .as_deref()
                .map(|expr| self.inline_expression_function_calls(expr).map(Box::new))
                .transpose()?,
            span,
        })
    }

    fn inline_function_calls_in_subscripts(
        &mut self,
        subscripts: &[Subscript],
    ) -> Result<Vec<Subscript>, GalecTargetError> {
        subscripts
            .iter()
            .map(|subscript| match subscript {
                Subscript::Index { value, span } => Ok(Subscript::index(*value, *span)),
                Subscript::Colon { span } => Ok(Subscript::colon(*span)),
                Subscript::Expr { expr, span } => Ok(Subscript::expr(
                    Box::new(self.inline_expression_function_calls(expr)?),
                    *span,
                )),
            })
            .collect()
    }

    fn lower_builtin(
        &mut self,
        function: BuiltinFunction,
        args: &[Expression],
        span: Span,
    ) -> Result<Typed, GalecTargetError> {
        let mapping = BUILTIN_MAP
            .iter()
            .find(|(modelica, _)| *modelica == function)
            .map(|(_, mapping)| mapping);
        let Some(mapping) = mapping else {
            return Err(unsupported(
                format!("builtin:{}", builtin_feature_name(function)),
                format!(
                    "Modelica builtin `{}` has no GALEC §3.2.6 catalog mapping",
                    builtin_feature_name(function)
                ),
                Some(span),
            ));
        };
        match mapping {
            BuiltinMapping::RealUnary(catalog) => {
                let [arg] = args else {
                    return Err(arity_error(function, 1, args.len(), span));
                };
                let arg = self.lower_as_real(arg, "builtin argument")?;
                Ok(Typed::new(call(catalog, vec![arg]), ScalarType::Real))
            }
            BuiltinMapping::RealBinary(catalog) => {
                let [first, second] = args else {
                    return Err(arity_error(function, 2, args.len(), span));
                };
                let first = self.lower_as_real(first, "builtin argument")?;
                let second = self.lower_as_real(second, "builtin argument")?;
                Ok(Typed::new(
                    call(catalog, vec![first, second]),
                    ScalarType::Real,
                ))
            }
            BuiltinMapping::MinMax { integer, real } => {
                self.lower_min_max(integer, real, function, args, span)
            }
            BuiltinMapping::IntegerBinary(catalog) => {
                let [first, second] = args else {
                    return Err(arity_error(function, 2, args.len(), span));
                };
                let first = self.lower_as_integer(first, span)?;
                let second = self.lower_as_integer(second, span)?;
                Ok(Typed::new(
                    call(catalog, vec![first, second]),
                    ScalarType::Integer,
                ))
            }
            BuiltinMapping::PassThrough => {
                let [arg] = args else {
                    return Err(arity_error(function, 1, args.len(), span));
                };
                self.lower(arg)
            }
            BuiltinMapping::Unsupported(feature) => Err(unsupported(
                (*feature).to_owned(),
                format!(
                    "Modelica builtin `{}` cannot be lowered to the GALEC §3.2.6 catalog",
                    builtin_feature_name(function)
                ),
                Some(span),
            )),
        }
    }

    /// 2-argument min/max: `imin`/`imax` for Integer operands, `min`/`max`
    /// (widening) otherwise. The 1-argument Modelica form is the array
    /// reduction GALEC does not have (trap T8).
    fn lower_min_max(
        &mut self,
        integer: &'static str,
        real: &'static str,
        function: BuiltinFunction,
        args: &[Expression],
        span: Span,
    ) -> Result<Typed, GalecTargetError> {
        let [first, second] = args else {
            return Err(unsupported(
                format!("array-reduction:{}", builtin_feature_name(function)),
                format!(
                    "array-reduction form of `{}` (GALEC min/max are 2-argument \
                     scalar functions only)",
                    builtin_feature_name(function)
                ),
                Some(span),
            ));
        };
        let first = self.lower(first)?;
        let second = self.lower(second)?;
        let context = "min/max argument";
        require_numeric(&first, context, span)?;
        require_numeric(&second, context, span)?;
        require_scalar(&first, context, span)?;
        require_scalar(&second, context, span)?;
        if first.ty == ScalarType::Integer && second.ty == ScalarType::Integer {
            return Ok(Typed::new(
                call(integer, vec![first.expr, second.expr]),
                ScalarType::Integer,
            ));
        }
        let first = widen_to_real(first, context, Some(span))?;
        let second = widen_to_real(second, context, Some(span))?;
        Ok(Typed::new(
            call(real, vec![first, second]),
            ScalarType::Real,
        ))
    }

    /// Lower an expression that must already be Integer (no narrowing is
    /// ever inserted, trap T5).
    fn lower_as_integer(
        &mut self,
        expr: &Expression,
        span: Span,
    ) -> Result<gast::Expression, GalecTargetError> {
        let typed = self.lower(expr)?;
        if typed.ty != ScalarType::Integer || !typed.is_scalar() {
            return Err(mismatch(
                "integer builtin argument",
                "Integer",
                typed.ty,
                Some(span),
            ));
        }
        Ok(typed.expr)
    }
}

// ---------------------------------------------------------------------------
// The T8 builtin mapping table (data)
// ---------------------------------------------------------------------------

/// How one Modelica builtin lowers.
enum BuiltinMapping {
    /// One Real argument (Integer widens via `real()`), Real result.
    RealUnary(&'static str),
    /// Two Real arguments, Real result (`atan2(y, x)` keeps its Modelica
    /// argument order — the catalog signature is also `(y, x)`).
    RealBinary(&'static str),
    /// 2-argument numeric min/max: Integer×Integer → `imin`/`imax`, any
    /// Real operand widens both to Real → `min`/`max`.
    MinMax {
        integer: &'static str,
        real: &'static str,
    },
    /// Two Integer arguments, Integer result.
    IntegerBinary(&'static str),
    /// Semantic pass-through of the single argument (`noEvent`); exact
    /// arity, like every other mapping — extra arguments are a diagnostic,
    /// never silently dropped.
    PassThrough,
    /// Named unlowerable feature (stable feature id).
    Unsupported(&'static str),
}

/// Modelica builtin → GALEC §3.2.6 catalog mapping (trap T8, GAL-005):
///
/// | Modelica | GALEC |
/// |----------|-------|
/// | `abs`    | `absolute` (Real; Modelica's Integer overload widens) |
/// | `sign`   | `sign` (returns Real in GALEC) |
/// | `floor`  | `roundDown` (both are Real→Real floor) |
/// | `ceil`   | `roundUp` |
/// | `log`    | `ln` |
/// | `log10`  | `lg` |
/// | `min`/`max` | `min`/`max` (Real) or `imin`/`imax` (Integer), 2-arg only |
/// | `div`    | `divisionTowardsZero` (both truncate toward zero) |
/// | `mod`/`rem` | unsupported — GALEC `remainderDown` is Appendix-C |
/// |          | reserved, not callable in Beta 1 |
/// | `sqrt`, `exp`, trig, hyperbolic, `atan2` | same catalog names |
/// | `noEvent` | pass-through (no events exist inside a GALEC tick) |
static BUILTIN_MAP: &[(BuiltinFunction, BuiltinMapping)] = &[
    (BuiltinFunction::Abs, BuiltinMapping::RealUnary("absolute")),
    (BuiltinFunction::Sign, BuiltinMapping::RealUnary("sign")),
    (BuiltinFunction::Sqrt, BuiltinMapping::RealUnary("sqrt")),
    (BuiltinFunction::Exp, BuiltinMapping::RealUnary("exp")),
    (BuiltinFunction::Log, BuiltinMapping::RealUnary("ln")),
    (BuiltinFunction::Log10, BuiltinMapping::RealUnary("lg")),
    (
        BuiltinFunction::Floor,
        BuiltinMapping::RealUnary("roundDown"),
    ),
    (BuiltinFunction::Ceil, BuiltinMapping::RealUnary("roundUp")),
    (BuiltinFunction::Sin, BuiltinMapping::RealUnary("sin")),
    (BuiltinFunction::Cos, BuiltinMapping::RealUnary("cos")),
    (BuiltinFunction::Tan, BuiltinMapping::RealUnary("tan")),
    (BuiltinFunction::Asin, BuiltinMapping::RealUnary("asin")),
    (BuiltinFunction::Acos, BuiltinMapping::RealUnary("acos")),
    (BuiltinFunction::Atan, BuiltinMapping::RealUnary("atan")),
    (BuiltinFunction::Atan2, BuiltinMapping::RealBinary("atan2")),
    (BuiltinFunction::Sinh, BuiltinMapping::RealUnary("sinh")),
    (BuiltinFunction::Cosh, BuiltinMapping::RealUnary("cosh")),
    (BuiltinFunction::Tanh, BuiltinMapping::RealUnary("tanh")),
    (
        BuiltinFunction::Min,
        BuiltinMapping::MinMax {
            integer: "imin",
            real: "min",
        },
    ),
    (
        BuiltinFunction::Max,
        BuiltinMapping::MinMax {
            integer: "imax",
            real: "max",
        },
    ),
    (
        BuiltinFunction::Div,
        BuiltinMapping::IntegerBinary("divisionTowardsZero"),
    ),
    (
        BuiltinFunction::Mod,
        BuiltinMapping::Unsupported("builtin:mod"),
    ),
    (
        BuiltinFunction::Rem,
        BuiltinMapping::Unsupported("builtin:rem"),
    ),
    (BuiltinFunction::NoEvent, BuiltinMapping::PassThrough),
];

/// Every GALEC catalog function name this lowering can emit, with its call
/// arity — derived from the `BUILTIN_MAP` table plus the explicit `real()`
/// widening cast (trap T5), so it cannot drift from the emission paths.
/// This is the GAL-005 parity surface: the test battery walks it against
/// `rumoca_ir_galec::builtins::BUILTINS`.
#[must_use]
pub fn emittable_builtin_targets() -> Vec<(&'static str, usize)> {
    let mut targets = vec![("real", 1)];
    for (_, mapping) in BUILTIN_MAP {
        match mapping {
            BuiltinMapping::RealUnary(name) => targets.push((name, 1)),
            BuiltinMapping::RealBinary(name) | BuiltinMapping::IntegerBinary(name) => {
                targets.push((name, 2));
            }
            BuiltinMapping::MinMax { integer, real } => {
                targets.push((integer, 2));
                targets.push((real, 2));
            }
            BuiltinMapping::PassThrough | BuiltinMapping::Unsupported(_) => {}
        }
    }
    targets
}

// ---------------------------------------------------------------------------
// Shared helpers
// ---------------------------------------------------------------------------

/// `self.<name>[subscripts]` state reference.
pub(crate) fn state_ref(name: Name, subscripts: Vec<gast::Expression>) -> gast::Expression {
    gast::Expression::Ref(Reference::State(vec![RefPart {
        name,
        subscripts,
        span: Span::DUMMY,
    }]))
}

fn call(function: &'static str, arguments: Vec<gast::Expression>) -> gast::Expression {
    gast::Expression::Call(FunctionCall {
        function: Name::ident(function),
        arguments,
    })
}

/// Wrap an Integer-typed expression in an explicit `real()` cast; Real
/// passes through; Boolean fails (trap T5 — never a silent conversion).
pub(crate) fn widen_to_real(
    typed: Typed,
    context: &str,
    span: Option<Span>,
) -> Result<gast::Expression, GalecTargetError> {
    match typed.ty {
        ScalarType::Real => Ok(typed.expr),
        ScalarType::Integer if typed.is_scalar() => Ok(call("real", vec![typed.expr])),
        ScalarType::Integer => Err(unsupported(
            "array-integer-real-promotion".to_owned(),
            format!(
                "{context} needs Integer-array to Real-array promotion, but the current \
                 GALEC projection only inserts scalar `real(...)` casts"
            ),
            span,
        )),
        ScalarType::Boolean => Err(mismatch(context, "numeric", ScalarType::Boolean, span)),
    }
}

fn equalize_numeric(
    left: Typed,
    right: Typed,
    context: &str,
    span: Span,
) -> Result<(gast::Expression, gast::Expression), GalecTargetError> {
    if left.ty == right.ty {
        return Ok((left.expr, right.expr));
    }
    Ok((
        widen_to_real(left, context, Some(span))?,
        widen_to_real(right, context, Some(span))?,
    ))
}

fn arithmetic_shape(left: &Typed, right: &Typed, span: Span) -> Result<Vec<i64>, GalecTargetError> {
    if left.shape == right.shape {
        return Ok(left.shape.clone());
    }
    if left.is_scalar() {
        return Ok(right.shape.clone());
    }
    if right.is_scalar() {
        return Ok(left.shape.clone());
    }
    Err(GalecTargetError::LoweringTypeMismatch {
        context: "array arithmetic operands".to_owned(),
        expected: "equal array shapes or scalar broadcast",
        found: "different array shapes",
        span: optional(span),
    })
}

fn integer_bound(expr: &Expression) -> Option<i64> {
    match expr {
        Expression::Literal {
            value: Literal::Integer(value),
            ..
        } => Some(*value),
        Expression::Unary {
            op: OpUnary::Minus,
            rhs,
            ..
        } => integer_bound(rhs)?.checked_neg(),
        _ => None,
    }
}

fn extend_index_combinations(prefixes: Vec<Vec<i64>>, values: &[i64]) -> Vec<Vec<i64>> {
    prefixes
        .into_iter()
        .flat_map(|prefix| {
            values.iter().copied().map(move |value| {
                let mut combined = prefix.clone();
                combined.push(value);
                combined
            })
        })
        .collect()
}

fn vector_dot_shape(left: &Typed, right: &Typed) -> Option<i64> {
    (left.rank() == 1
        && right.rank() == 1
        && left.shape == right.shape
        && left
            .shape
            .first()
            .copied()
            .is_some_and(|length| length >= 1))
    .then(|| left.shape[0])
}

fn unify_branch_types<'a>(
    branch_values: impl Iterator<Item = &'a Typed>,
    else_value: &Typed,
    span: Span,
) -> Result<ScalarType, GalecTargetError> {
    let mut result = else_value.ty;
    for value in branch_values {
        let ty = value.ty;
        result = match (result, ty) {
            (a, b) if a == b => a,
            (ScalarType::Real, ScalarType::Integer) | (ScalarType::Integer, ScalarType::Real) => {
                ScalarType::Real
            }
            (expected, found) => {
                return Err(mismatch(
                    "if-expression branches",
                    expected.keyword(),
                    found,
                    Some(span),
                ));
            }
        };
    }
    Ok(result)
}

fn unify_branch_shapes<'a>(
    branch_values: impl Iterator<Item = &'a Typed>,
    else_value: &Typed,
    span: Span,
) -> Result<Vec<i64>, GalecTargetError> {
    let mut result = else_value.shape.clone();
    for value in branch_values {
        if value.shape != result {
            return Err(GalecTargetError::LoweringTypeMismatch {
                context: "if-expression branches".to_owned(),
                expected: "same array shape",
                found: "different array shape",
                span: optional(span),
            });
        }
        result = value.shape.clone();
    }
    Ok(result)
}

fn coerce_branch(
    value: Typed,
    target: ScalarType,
    target_shape: &[i64],
    span: Span,
) -> Result<gast::Expression, GalecTargetError> {
    if value.shape != target_shape {
        return Err(GalecTargetError::LoweringTypeMismatch {
            context: "if-expression branch".to_owned(),
            expected: "same array shape",
            found: "different array shape",
            span: optional(span),
        });
    }
    if value.ty == target {
        return Ok(value.expr);
    }
    if target == ScalarType::Real && value.ty == ScalarType::Integer {
        return widen_to_real(value, "if-expression branch", Some(span));
    }
    Err(mismatch(
        "if-expression branch",
        target.keyword(),
        value.ty,
        Some(span),
    ))
}

fn require_numeric(typed: &Typed, context: &str, span: Span) -> Result<(), GalecTargetError> {
    if typed.ty == ScalarType::Boolean {
        return Err(mismatch(context, "numeric", typed.ty, Some(span)));
    }
    Ok(())
}

fn require_scalar(typed: &Typed, context: &str, span: Span) -> Result<(), GalecTargetError> {
    if !typed.is_scalar() {
        return Err(GalecTargetError::LoweringTypeMismatch {
            context: context.to_owned(),
            expected: "scalar",
            found: "array",
            span: optional(span),
        });
    }
    Ok(())
}

fn lower_literal(value: &Literal, span: Span) -> Result<Typed, GalecTargetError> {
    match value {
        Literal::Real(value) => Ok(Typed::new(gast::Expression::Real(*value), ScalarType::Real)),
        Literal::Integer(value) => Ok(Typed::new(
            gast::Expression::Integer(*value),
            ScalarType::Integer,
        )),
        Literal::Boolean(value) => Ok(Typed::new(
            gast::Expression::Bool(*value),
            ScalarType::Boolean,
        )),
        Literal::String(_) => Err(unsupported(
            "string-value".to_owned(),
            "String literal (GALEC has no String type)".to_owned(),
            Some(span),
        )),
    }
}

fn indexed_expression(base: Expression, subscripts: Vec<Subscript>, span: Span) -> Expression {
    Expression::Index {
        base: Box::new(base),
        subscripts,
        span,
    }
}

fn conjunction(mut conditions: Vec<gast::Expression>) -> Option<gast::Expression> {
    let first = conditions.pop()?;
    Some(conditions.into_iter().rev().fold(first, |rhs, lhs| {
        gast::Expression::binary(BinaryOp::And, lhs, rhs)
    }))
}

fn mismatch(
    context: &str,
    expected: &'static str,
    found: ScalarType,
    span: Option<Span>,
) -> GalecTargetError {
    GalecTargetError::LoweringTypeMismatch {
        context: context.to_owned(),
        expected,
        found: found.keyword(),
        span: span.filter(|span| !span.is_dummy()),
    }
}

fn unsupported(feature: String, detail: String, span: Option<Span>) -> GalecTargetError {
    GalecTargetError::UnsupportedFeature {
        feature,
        detail,
        span: span.filter(|span| !span.is_dummy()),
    }
}

fn arity_error(
    function: BuiltinFunction,
    expected: usize,
    found: usize,
    span: Span,
) -> GalecTargetError {
    GalecTargetError::LoweringInternal {
        detail: format!(
            "builtin `{}` called with {found} argument(s), expected {expected} \
             (span {span:?})",
            builtin_feature_name(function)
        ),
    }
}

fn builtin_feature_name(function: BuiltinFunction) -> String {
    format!("{function:?}")
}

fn form_name(expr: &Expression) -> &'static str {
    match expr {
        Expression::Binary { .. } => "binary operation",
        Expression::Unary { .. } => "unary operation",
        Expression::VarRef { .. } => "variable reference",
        Expression::BuiltinCall { .. } => "builtin call",
        Expression::FunctionCall { .. } => "function call",
        Expression::Literal { .. } => "literal",
        Expression::If { .. } => "if-expression",
        Expression::Array { .. } => "array constructor",
        Expression::Tuple { .. } => "tuple",
        Expression::Range { .. } => "range",
        Expression::ArrayComprehension { .. } => "array comprehension",
        Expression::Index { .. } => "indexed expression",
        Expression::FieldAccess { .. } => "field access",
        Expression::Empty { .. } => "empty expression",
    }
}

fn is_slice_subscript(subscript: &Subscript) -> bool {
    match subscript {
        Subscript::Colon { .. } => true,
        Subscript::Expr { expr, .. } => matches!(expr.as_ref(), Expression::Range { .. }),
        Subscript::Index { .. } => false,
    }
}

fn slice_len_to_i64(len: usize) -> Result<i64, GalecTargetError> {
    i64::try_from(len).map_err(|_| GalecTargetError::LoweringInternal {
        detail: "slice result length exceeds i64".to_owned(),
    })
}

fn optional(span: Span) -> Option<Span> {
    (!span.is_dummy()).then_some(span)
}
