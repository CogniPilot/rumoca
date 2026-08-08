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
                    return self.lower_array_multiplication(lhs, rhs, &left, &right, span);
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

    /// Array×array `*` after the vector-dot case: the MLS §10.6.4 product
    /// forms element-unroll (GAL-027); rank-compatible operands with
    /// mismatched inner dimensions are a type error; anything else (rank ≥ 3)
    /// has no Modelica `*` definition.
    fn lower_array_multiplication(
        &mut self,
        lhs: &Expression,
        rhs: &Expression,
        left: &Typed,
        right: &Typed,
        span: Span,
    ) -> Result<Typed, GalecTargetError> {
        match matrix_product_shape(left, right) {
            Some(product) => self.lower_matrix_product(lhs, rhs, product, span),
            None if matrix_product_ranks(left, right) => {
                Err(GalecTargetError::LoweringTypeMismatch {
                    context: "matrix product operands".to_owned(),
                    expected: "matching inner dimensions",
                    found: "mismatched inner dimensions",
                    span: optional(span),
                })
            }
            None => Err(unsupported(
                "array-multiplication".to_owned(),
                "array `*` is defined for vector·vector, matrix×vector, \
                 vector×matrix, and matrix×matrix operands only; use \
                 element-wise `.*` for other shapes"
                    .to_owned(),
                Some(span),
            )),
        }
    }

    /// Modelica matrix/vector `*` (MLS §10.6.4) element-unrolled into nested
    /// `{…}` constructors of ascending-index sum trees (GAL-027). The unroll
    /// order is the normative evaluation order (trap T6): element `(i, j)`
    /// is `a[i,1]*b[1,j] + a[i,2]*b[2,j] + …`, never re-associated.
    fn lower_matrix_product(
        &mut self,
        lhs: &Expression,
        rhs: &Expression,
        product: MatrixProduct,
        span: Span,
    ) -> Result<Typed, GalecTargetError> {
        match product {
            MatrixProduct::MatVec { rows, inner } => {
                let mut elements = Vec::with_capacity(usize::try_from(rows).unwrap_or_default());
                for row in 1..=rows {
                    elements.push(self.product_sum(lhs, rhs, Some(row), None, inner, span)?);
                }
                typed_array(elements, vec![rows])
            }
            MatrixProduct::VecMat { inner, cols } => {
                let mut elements = Vec::with_capacity(usize::try_from(cols).unwrap_or_default());
                for col in 1..=cols {
                    elements.push(self.product_sum(lhs, rhs, None, Some(col), inner, span)?);
                }
                typed_array(elements, vec![cols])
            }
            MatrixProduct::MatMat { rows, inner, cols } => {
                let mut out_rows = Vec::with_capacity(usize::try_from(rows).unwrap_or_default());
                for row in 1..=rows {
                    out_rows.push(self.matrix_product_row(lhs, rhs, row, inner, cols, span)?);
                }
                typed_array(out_rows, vec![rows, cols])
            }
        }
    }

    /// One matrix×matrix result row: `cols` unrolled sum-tree elements.
    fn matrix_product_row(
        &mut self,
        lhs: &Expression,
        rhs: &Expression,
        row: i64,
        inner: i64,
        cols: i64,
        span: Span,
    ) -> Result<Typed, GalecTargetError> {
        let mut elements = Vec::with_capacity(usize::try_from(cols).unwrap_or_default());
        for col in 1..=cols {
            elements.push(self.product_sum(lhs, rhs, Some(row), Some(col), inner, span)?);
        }
        typed_array(elements, vec![cols])
    }

    /// One matrix-product element: `Σ_k lhs[row, k] * rhs[k, col]` with the
    /// vector operand (row/col `None`) indexed by `k` alone. Operands are
    /// re-lowered per element like [`Self::lower_vector_dot`].
    fn product_sum(
        &mut self,
        lhs: &Expression,
        rhs: &Expression,
        row: Option<i64>,
        col: Option<i64>,
        inner: i64,
        span: Span,
    ) -> Result<Typed, GalecTargetError> {
        let mut result = None;
        for k in 1..=inner {
            let left_subscripts = match row {
                Some(row) => vec![Subscript::index(row, span), Subscript::index(k, span)],
                None => vec![Subscript::index(k, span)],
            };
            let right_subscripts = match col {
                Some(col) => vec![Subscript::index(k, span), Subscript::index(col, span)],
                None => vec![Subscript::index(k, span)],
            };
            let left = self.lower(&indexed_expression(lhs.to_owned(), left_subscripts, span))?;
            let right = self.lower(&indexed_expression(rhs.to_owned(), right_subscripts, span))?;
            let product = self.arithmetic(BinaryOp::Mul, left, right, span)?;
            result = Some(match result {
                Some(acc) => self.arithmetic(BinaryOp::Add, acc, product, span)?,
                None => product,
            });
        }
        result.ok_or_else(|| GalecTargetError::LoweringInternal {
            detail: "matrix product over an empty inner dimension".to_owned(),
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
        // D13: MSL linear-algebra functions with LAPACK-external bodies map
        // by name to GALEC catalog builtins instead of inlining.
        if let Some(result) = self.lower_matrices_library_call(name.as_str(), args, span)? {
            return Ok(result);
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

    /// Recognize `Modelica.Math.Matrices` calls whose MSL bodies are
    /// LAPACK-external and can never inline (D13): `solve` maps to the
    /// GALEC `solveLinearEquations` builtin (whose
    /// `SOLVE_LINEAR_EQUATIONS_FAILED` escape the caller's method declares,
    /// GAL-029); `inv` and `solve2` get targeted guidance.
    fn lower_matrices_library_call(
        &mut self,
        name: &str,
        args: &[Expression],
        span: Span,
    ) -> Result<Option<Typed>, GalecTargetError> {
        match name {
            "Modelica.Math.Matrices.solve" => self.lower_linear_solve(args, span).map(Some),
            "Modelica.Math.Matrices.solve2" => Err(unsupported(
                "matrix-solve2".to_owned(),
                "`Matrices.solve2` (matrix right-hand side) has no GALEC builtin; \
                 solve per column with `Matrices.solve`"
                    .to_owned(),
                Some(span),
            )),
            "Modelica.Math.Matrices.inv" => Err(unsupported(
                "matrix-inverse".to_owned(),
                "`Matrices.inv` has no GALEC builtin; rewrite `inv(A) * b` as \
                 `Matrices.solve(A, b)` (better numerics, maps to the \
                 `solveLinearEquations` builtin)"
                    .to_owned(),
                Some(span),
            )),
            _ => Ok(None),
        }
    }

    /// `Matrices.solve(A, b)` → `solveLinearEquations(A, b)`: `A` square
    /// Real `[n, n]`, `b` Real `[n]`, result `[n]` (§3.2.6).
    fn lower_linear_solve(
        &mut self,
        args: &[Expression],
        span: Span,
    ) -> Result<Typed, GalecTargetError> {
        let [a, b] = args else {
            return Err(GalecTargetError::LoweringTypeMismatch {
                context: "Matrices.solve call".to_owned(),
                expected: "solve(A, b) with two arguments",
                found: "different arity",
                span: optional(span),
            });
        };
        let a = self.lower(a)?;
        let b = self.lower(b)?;
        let ([rows, cols], [len]) = (&a.shape[..], &b.shape[..]) else {
            return Err(GalecTargetError::LoweringTypeMismatch {
                context: "Matrices.solve operands".to_owned(),
                expected: "matrix A and vector b",
                found: "other shapes",
                span: optional(span),
            });
        };
        if rows != cols || rows != len {
            return Err(GalecTargetError::LoweringTypeMismatch {
                context: "Matrices.solve operands".to_owned(),
                expected: "square A[n, n] with matching b[n]",
                found: "mismatched dimensions",
                span: optional(span),
            });
        }
        let length = *len;
        let a = widen_to_real(a, "Matrices.solve matrix", Some(span))?;
        let b = widen_to_real(b, "Matrices.solve vector", Some(span))?;
        Ok(Typed::array(
            call("solveLinearEquations", vec![a, b]),
            ScalarType::Real,
            vec![length],
        ))
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
        // Array-shape builtins need shape logic the data-driven table cannot
        // express (GAL-027); they unroll to `{…}` constructors here.
        match function {
            BuiltinFunction::Transpose => return self.lower_transpose(function, args, span),
            BuiltinFunction::Identity => return self.lower_identity(function, args, span),
            _ => {}
        }
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

    /// `transpose(A)` element-unrolls to a reindexed `{…}` constructor
    /// (GAL-027): result element `(i, j)` re-lowers `A[j, i]`.
    fn lower_transpose(
        &mut self,
        function: BuiltinFunction,
        args: &[Expression],
        span: Span,
    ) -> Result<Typed, GalecTargetError> {
        let [arg] = args else {
            return Err(arity_error(function, 1, args.len(), span));
        };
        let typed = self.lower(arg)?;
        let [rows, cols] = typed.shape[..] else {
            return Err(GalecTargetError::LoweringTypeMismatch {
                context: "transpose argument".to_owned(),
                expected: "rank-2 array",
                found: if typed.is_scalar() { "scalar" } else { "array" },
                span: optional(span),
            });
        };
        let mut out_rows = Vec::with_capacity(usize::try_from(cols).unwrap_or_default());
        for i in 1..=cols {
            let mut elements = Vec::with_capacity(usize::try_from(rows).unwrap_or_default());
            for j in 1..=rows {
                elements.push(self.lower(&indexed_expression(
                    arg.to_owned(),
                    vec![Subscript::index(j, span), Subscript::index(i, span)],
                    span,
                ))?);
            }
            out_rows.push(typed_array(elements, vec![rows])?);
        }
        typed_array(out_rows, vec![cols, rows])
    }

    /// `identity(n)` with a statically-evaluable `n` becomes an Integer
    /// `{…}` constructor of 1/0 literals (MLS: `identity` is Integer-typed;
    /// Real contexts widen through the array-literal `real` distribution).
    fn lower_identity(
        &mut self,
        function: BuiltinFunction,
        args: &[Expression],
        span: Span,
    ) -> Result<Typed, GalecTargetError> {
        let [arg] = args else {
            return Err(arity_error(function, 1, args.len(), span));
        };
        let Some(order) = self.static_integer_expression(arg) else {
            return Err(unsupported(
                "identity-dynamic-order".to_owned(),
                "`identity` needs a statically-evaluable Integer order \
                 (GALEC dimensions are literal, trap T11)"
                    .to_owned(),
                Some(span),
            ));
        };
        if order < 1 {
            return Err(unsupported(
                "identity-dynamic-order".to_owned(),
                format!("`identity` order {order} is not a positive Integer"),
                Some(span),
            ));
        }
        let mut out_rows = Vec::with_capacity(usize::try_from(order).unwrap_or_default());
        for i in 1..=order {
            let mut elements = Vec::with_capacity(usize::try_from(order).unwrap_or_default());
            for j in 1..=order {
                elements.push(Typed::new(
                    gast::Expression::Integer(i64::from(i == j)),
                    ScalarType::Integer,
                ));
            }
            out_rows.push(typed_array(elements, vec![order])?);
        }
        typed_array(out_rows, vec![order, order])
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
    // `real` is the trap-T5 widening cast; `solveLinearEquations` is the
    // D13 `Matrices.solve` mapping (`lower_linear_solve`).
    let mut targets = vec![("real", 1), ("solveLinearEquations", 2)];
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
        ScalarType::Integer => {
            if let Some(widened) = widen_integer_array_literal(&typed.expr) {
                return Ok(widened);
            }
            Err(unsupported(
                "array-integer-real-promotion".to_owned(),
                format!(
                    "{context} needs Integer-array to Real-array promotion, but the \
                     current GALEC projection only widens scalars and Integer array \
                     literals"
                ),
                span,
            ))
        }
        ScalarType::Boolean => Err(mismatch(context, "numeric", ScalarType::Boolean, span)),
    }
}

/// Distribute Integer→Real widening over a `{…}` constructor: Integer
/// literals become Real literals; other scalar elements get explicit
/// `real(…)` casts (trap T5 — the conversion stays visible). Non-literal
/// arrays (e.g. whole-array references) stay unsupported.
fn widen_integer_array_literal(expr: &gast::Expression) -> Option<gast::Expression> {
    match expr {
        gast::Expression::Array(elements) => {
            let widened = elements
                .iter()
                .map(widen_integer_array_element)
                .collect::<Option<Vec<_>>>()?;
            Some(gast::Expression::Array(widened))
        }
        _ => None,
    }
}

#[allow(clippy::cast_precision_loss)]
fn widen_integer_array_element(expr: &gast::Expression) -> Option<gast::Expression> {
    match expr {
        gast::Expression::Array(_) => widen_integer_array_literal(expr),
        gast::Expression::Integer(value) => Some(gast::Expression::Real(*value as f64)),
        other => Some(call("real", vec![other.clone()])),
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

/// The Modelica matrix/vector product forms of `*` (MLS §10.6.4) that
/// element-unroll per GAL-027.
#[derive(Clone, Copy)]
enum MatrixProduct {
    /// `[rows, inner] * [inner]` → `[rows]`.
    MatVec { rows: i64, inner: i64 },
    /// `[inner] * [inner, cols]` → `[cols]`.
    VecMat { inner: i64, cols: i64 },
    /// `[rows, inner] * [inner, cols]` → `[rows, cols]`.
    MatMat { rows: i64, inner: i64, cols: i64 },
}

fn matrix_product_shape(left: &Typed, right: &Typed) -> Option<MatrixProduct> {
    let positive = |dims: &[i64]| dims.iter().all(|&d| d >= 1);
    match (left.shape.as_slice(), right.shape.as_slice()) {
        ([rows, inner], [rhs_inner]) if inner == rhs_inner && positive(&[*rows, *inner]) => {
            Some(MatrixProduct::MatVec {
                rows: *rows,
                inner: *inner,
            })
        }
        ([inner], [rhs_inner, cols]) if inner == rhs_inner && positive(&[*inner, *cols]) => {
            Some(MatrixProduct::VecMat {
                inner: *inner,
                cols: *cols,
            })
        }
        ([rows, inner], [rhs_inner, cols])
            if inner == rhs_inner && positive(&[*rows, *inner, *cols]) =>
        {
            Some(MatrixProduct::MatMat {
                rows: *rows,
                inner: *inner,
                cols: *cols,
            })
        }
        _ => None,
    }
}

/// Whether the operand ranks alone form a Modelica `*` product (used to
/// distinguish an inner-dimension mismatch from an undefined shape combo).
fn matrix_product_ranks(left: &Typed, right: &Typed) -> bool {
    matches!(
        (left.rank(), right.rank()),
        (2, 1) | (1, 2) | (2, 2) | (1, 1)
    )
}

/// Assemble unrolled elements into one `{…}` constructor, requiring the
/// element types the arithmetic produced to agree (they always do — every
/// element lowers through the same operand expressions).
fn typed_array(elements: Vec<Typed>, shape: Vec<i64>) -> Result<Typed, GalecTargetError> {
    let Some(ty) = elements.first().map(|element| element.ty) else {
        return Err(GalecTargetError::LoweringInternal {
            detail: "array assembly over zero elements".to_owned(),
        });
    };
    if elements.iter().any(|element| element.ty != ty) {
        return Err(GalecTargetError::LoweringInternal {
            detail: "array assembly produced mixed element types".to_owned(),
        });
    }
    Ok(Typed::array(
        gast::Expression::Array(elements.into_iter().map(|element| element.expr).collect()),
        ty,
        shape,
    ))
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
