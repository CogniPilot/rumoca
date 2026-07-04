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

use std::collections::HashSet;

use rumoca_core::{
    BuiltinFunction, Expression, Literal, OpBinary, OpUnary, Span, Subscript,
    component_path_trailing_index, is_pre_slot, pre_slot_base,
};
use rumoca_ir_galec::ast::{
    self as gast, BinaryOp, FunctionCall, IfExpression, Name, RefPart, Reference, ScalarType,
};

use crate::classify::Classification;
use crate::diagnostic::GalecTargetError;
use crate::lower::conditions::{ConditionTable, condition_ref_index, pre_condition_ref_index};

/// A lowered expression together with its GALEC scalar (element) type.
pub(crate) struct Typed {
    pub expr: gast::Expression,
    pub ty: ScalarType,
}

impl Typed {
    fn new(expr: gast::Expression, ty: ScalarType) -> Self {
        Self { expr, ty }
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
    referenced_pre: ReferencedPre,
    /// Condition slots currently being inlined (cycle guard).
    inlining: Vec<usize>,
}

impl<'a> ExprLowerer<'a> {
    pub(crate) fn new(
        classification: &'a Classification<'a>,
        conditions: &'a ConditionTable<'a>,
    ) -> Self {
        Self {
            classification,
            conditions,
            referenced_pre: ReferencedPre::default(),
            inlining: Vec::new(),
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
            Expression::FunctionCall { name, span, .. } => Err(unsupported(
                if name.as_str() == rumoca_core::INTERNAL_SAMPLE_FUNCTION_NAME {
                    "sample-in-expression".to_owned()
                } else {
                    format!("user-function-call:{}", name.as_str())
                },
                format!("call to `{}` in a lowered expression", name.as_str()),
                Some(*span),
            )),
            Expression::Array { elements, span, .. } => self.lower_array(elements, *span),
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
        widen_to_real(typed, context, expr.span())
    }

    /// Lower an expression that must be Boolean.
    fn lower_as_boolean(
        &mut self,
        expr: &Expression,
        context: &str,
    ) -> Result<gast::Expression, GalecTargetError> {
        let typed = self.lower(expr)?;
        if typed.ty != ScalarType::Boolean {
            return Err(mismatch(context, "Boolean", typed.ty, expr.span()));
        }
        Ok(typed.expr)
    }

    // -----------------------------------------------------------------
    // References
    // -----------------------------------------------------------------

    fn lower_var_ref(
        &mut self,
        name: &str,
        subscripts: &[Subscript],
        span: Span,
    ) -> Result<Typed, GalecTargetError> {
        if name == "time" && subscripts.is_empty() {
            return Err(unsupported(
                "time-in-discrete-block".to_owned(),
                "reference to the continuous independent variable `time`".to_owned(),
                Some(span),
            ));
        }
        if let Some(condition_base) = &self.conditions.base_name {
            let as_expr = Expression::VarRef {
                name: rumoca_core::Reference::new(name),
                subscripts: subscripts.to_vec(),
                span,
            };
            if let Some(index) = condition_ref_index(&as_expr, condition_base) {
                return self.inline_condition(index, span);
            }
            if pre_condition_ref_index(&as_expr, condition_base).is_some()
                || name == condition_base
                || pre_slot_base(name) == Some(condition_base.as_str())
            {
                return Err(unsupported(
                    "condition-memory-outside-guard".to_owned(),
                    format!(
                        "generated when-edge condition machinery `{name}` referenced \
                         outside a recognizable when-edge guard"
                    ),
                    Some(span),
                ));
            }
        }
        if is_pre_slot(name) {
            return self.lower_pre_ref(name, subscripts, span);
        }
        self.lower_state_ref(name, subscripts, span)
    }

    /// `__pre__.x` → `self.'previous(x)'`, recorded in the referenced set.
    /// Like [`Self::lower_state_ref`], the rendered element form
    /// (`__pre__.x[2]`) resolves through its base slot with the index as a
    /// literal subscript.
    fn lower_pre_ref(
        &mut self,
        slot_name: &str,
        subscripts: &[Subscript],
        span: Span,
    ) -> Result<Typed, GalecTargetError> {
        let (classified, mut galec_subscripts) = match self.classification.find(slot_name) {
            Some(classified) => (classified, Vec::new()),
            None => {
                let Some((base, index)) = component_path_trailing_index(slot_name) else {
                    return Err(GalecTargetError::UnknownVariableReference {
                        name: slot_name.to_owned(),
                        span: optional(span),
                    });
                };
                let Some(classified) = self.classification.find(&base) else {
                    return Err(GalecTargetError::UnknownVariableReference {
                        name: slot_name.to_owned(),
                        span: optional(span),
                    });
                };
                let index =
                    i64::try_from(index).map_err(|_| GalecTargetError::LoweringInternal {
                        detail: format!("index of `{slot_name}` exceeds i64"),
                    })?;
                (classified, vec![gast::Expression::Integer(index)])
            }
        };
        if classified.pre_base.is_none() {
            return Err(GalecTargetError::LoweringInternal {
                detail: format!(
                    "variable `{slot_name}` carries the pre-slot prefix but did not \
                     classify as a pre slot"
                ),
            });
        }
        // Record the SLOT name (`__pre__.x`), not the rendered element name:
        // the kept-slot filter and the commit builder match on it.
        self.referenced_pre
            .record(classified.variable.name.as_str());
        galec_subscripts.extend(self.lower_subscripts(subscripts)?);
        Ok(Typed::new(
            state_ref(classified.galec_name.clone(), galec_subscripts),
            classified.scalar_type,
        ))
    }

    fn lower_state_ref(
        &mut self,
        name: &str,
        subscripts: &[Subscript],
        span: Span,
    ) -> Result<Typed, GalecTargetError> {
        // Exact-name match first (scalar variables), then base-name match
        // with the rendered index as an extra subscript (`x[2]`).
        let (classified, mut galec_subscripts) = match self.classification.find(name) {
            Some(classified) => (classified, Vec::new()),
            None => {
                let Some((base, index)) = component_path_trailing_index(name) else {
                    return Err(GalecTargetError::UnknownVariableReference {
                        name: name.to_owned(),
                        span: optional(span),
                    });
                };
                let Some(classified) = self.classification.find(&base) else {
                    return Err(GalecTargetError::UnknownVariableReference {
                        name: name.to_owned(),
                        span: optional(span),
                    });
                };
                let index =
                    i64::try_from(index).map_err(|_| GalecTargetError::LoweringInternal {
                        detail: format!("index of `{name}` exceeds i64"),
                    })?;
                (classified, vec![gast::Expression::Integer(index)])
            }
        };
        if classified.projection_internal {
            return Err(unsupported(
                "condition-memory-outside-guard".to_owned(),
                format!("generated condition machinery `{name}` referenced as a value"),
                Some(span),
            ));
        }
        galec_subscripts.extend(self.lower_subscripts(subscripts)?);
        Ok(Typed::new(
            state_ref(classified.galec_name.clone(), galec_subscripts),
            classified.scalar_type,
        ))
    }

    fn lower_subscripts(
        &mut self,
        subscripts: &[Subscript],
    ) -> Result<Vec<gast::Expression>, GalecTargetError> {
        subscripts
            .iter()
            .map(|subscript| match subscript {
                Subscript::Index { value, .. } => Ok(gast::Expression::Integer(*value)),
                Subscript::Expr { expr, span } => {
                    let typed = self.lower(expr)?;
                    if typed.ty != ScalarType::Integer {
                        return Err(mismatch(
                            "array subscript",
                            "Integer",
                            typed.ty,
                            Some(*span),
                        ));
                    }
                    Ok(typed.expr)
                }
                Subscript::Colon { span } => Err(unsupported(
                    "array-slice".to_owned(),
                    "`:` array slice subscript".to_owned(),
                    Some(*span),
                )),
            })
            .collect()
    }

    /// Inline `c[index]` to its defining Boolean expression.
    fn inline_condition(&mut self, index: usize, span: Span) -> Result<Typed, GalecTargetError> {
        let Some(entry) = self.conditions.entry(index) else {
            return Err(GalecTargetError::LoweringInternal {
                detail: format!("reference to undefined condition slot c[{index}]"),
            });
        };
        if entry.is_sample {
            return Err(unsupported(
                "sample-condition-as-value".to_owned(),
                "the clock sample-tick condition used as a value expression".to_owned(),
                Some(span),
            ));
        }
        if self.inlining.contains(&index) {
            return Err(GalecTargetError::LoweringInternal {
                detail: format!("condition slot c[{index}] is defined in terms of itself"),
            });
        }
        self.inlining.push(index);
        let result = self.lower(entry.rhs);
        self.inlining.pop();
        let typed = result?;
        if typed.ty != ScalarType::Boolean {
            return Err(mismatch(
                "inlined condition expression",
                "Boolean",
                typed.ty,
                optional(span),
            ));
        }
        Ok(typed)
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
                ScalarType::Real => Ok(Typed::new(
                    gast::Expression::negated_real(typed.expr),
                    ScalarType::Real,
                )),
                ScalarType::Integer => Ok(Typed::new(
                    gast::Expression::negated_integer(typed.expr),
                    ScalarType::Integer,
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
                if typed.ty != ScalarType::Boolean {
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
            OpBinary::Mul | OpBinary::MulElem => self.arithmetic(BinaryOp::Mul, left, right, span),
            // Modelica `/` and GALEC `/` are both Real division; GALEC
            // additionally requires Real-typed operands (trap T5).
            OpBinary::Div | OpBinary::DivElem => {
                let left = widen_to_real(left, "`/` operand", Some(span))?;
                let right = widen_to_real(right, "`/` operand", Some(span))?;
                Ok(Typed::new(
                    gast::Expression::binary(BinaryOp::Div, left, right),
                    ScalarType::Real,
                ))
            }
            // GALEC `^` accepts numeric operands and returns Real.
            OpBinary::Exp | OpBinary::ExpElem => {
                require_numeric(&left, "`^` operand", span)?;
                require_numeric(&right, "`^` operand", span)?;
                Ok(Typed::new(
                    gast::Expression::binary(BinaryOp::Pow, left.expr, right.expr),
                    ScalarType::Real,
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
        if left.ty == ScalarType::Integer && right.ty == ScalarType::Integer {
            return Ok(Typed::new(
                gast::Expression::binary(op, left.expr, right.expr),
                ScalarType::Integer,
            ));
        }
        let left = widen_to_real(left, context, Some(span))?;
        let right = widen_to_real(right, context, Some(span))?;
        Ok(Typed::new(
            gast::Expression::binary(op, left, right),
            ScalarType::Real,
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
            if operand.ty != ScalarType::Boolean {
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
        let result_ty = unify_branch_types(
            lowered.iter().map(|(_, value)| value.ty),
            else_value.ty,
            span,
        )?;
        let branches = lowered
            .into_iter()
            .map(|(condition, value)| Ok((condition, coerce_branch(value, result_ty, span)?)))
            .collect::<Result<Vec<_>, GalecTargetError>>()?;
        let else_value = coerce_branch(else_value, result_ty, span)?;
        Ok(Typed::new(
            gast::Expression::If(IfExpression {
                branches,
                else_value: Box::new(else_value),
            }),
            result_ty,
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
        for element in &lowered {
            if element.ty != element_ty {
                return Err(mismatch(
                    "array constructor elements",
                    element_ty.keyword(),
                    element.ty,
                    Some(span),
                ));
            }
        }
        Ok(Typed::new(
            gast::Expression::Array(lowered.into_iter().map(|typed| typed.expr).collect()),
            element_ty,
        ))
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
        if typed.ty != ScalarType::Integer {
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
        ScalarType::Integer => Ok(call("real", vec![typed.expr])),
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

fn unify_branch_types(
    branch_types: impl Iterator<Item = ScalarType>,
    else_type: ScalarType,
    span: Span,
) -> Result<ScalarType, GalecTargetError> {
    let mut result = else_type;
    for ty in branch_types {
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

fn coerce_branch(
    value: Typed,
    target: ScalarType,
    span: Span,
) -> Result<gast::Expression, GalecTargetError> {
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

fn optional(span: Span) -> Option<Span> {
    (!span.is_dummy()).then_some(span)
}
