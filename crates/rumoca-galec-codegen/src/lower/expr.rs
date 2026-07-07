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

use std::collections::HashSet;

use rumoca_core::{
    BuiltinFunction, Expression, Literal, OpBinary, OpUnary, Span, Subscript, VarName,
    component_path_trailing_index, is_pre_slot, pre_slot_base,
};
use rumoca_ir_dae::DaeSymbolTable;
use rumoca_ir_galec::ast::{
    self as gast, BinaryOp, FunctionCall, IfExpression, Name, RefPart, Reference, ScalarType,
};

use crate::classify::{Classification, VariableClass};
use crate::diagnostic::GalecTargetError;
use crate::lower::conditions::{ConditionTable, condition_ref_index, pre_condition_ref_index};
use function_inline::inline_function_call;

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
        let (classified, prefix_indices) = match self.classification.find(slot_name) {
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
                (classified, vec![index])
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
        self.lower_reference_access(classified, prefix_indices, subscripts, span)
    }

    fn lower_state_ref(
        &mut self,
        name: &str,
        subscripts: &[Subscript],
        span: Span,
    ) -> Result<Typed, GalecTargetError> {
        // Exact-name match first (scalar variables), then base-name match
        // with the rendered index as an extra subscript (`x[2]`).
        let (classified, prefix_indices) = match self.classification.find(name) {
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
                (classified, vec![index])
            }
        };
        if classified.projection_internal {
            return Err(unsupported(
                "condition-memory-outside-guard".to_owned(),
                format!("generated condition machinery `{name}` referenced as a value"),
                Some(span),
            ));
        }
        self.lower_reference_access(classified, prefix_indices, subscripts, span)
    }

    fn lower_reference_access(
        &mut self,
        classified: &crate::classify::ClassifiedVariable<'_>,
        prefix_indices: Vec<i64>,
        subscripts: &[Subscript],
        span: Span,
    ) -> Result<Typed, GalecTargetError> {
        let dims = &classified.variable.dims;
        let subscript_count = prefix_indices.len() + subscripts.len();
        if subscript_count == 0 {
            return Ok(Typed::array(
                state_ref(classified.galec_name.clone(), Vec::new()),
                classified.scalar_type,
                dims.clone(),
            ));
        }
        if subscript_count != dims.len() {
            return Err(unsupported(
                "array-partial-subscript".to_owned(),
                format!(
                    "reference to `{}` uses {subscript_count} subscript(s) for {} dimension(s)",
                    classified.variable.name.as_str(),
                    dims.len()
                ),
                Some(span),
            ));
        }
        if subscripts
            .iter()
            .any(|subscript| matches!(subscript, Subscript::Colon { .. }))
        {
            return self.lower_slice_reference(classified, prefix_indices, subscripts, span);
        }
        if subscripts
            .iter()
            .any(|subscript| self.static_index_value(subscript).is_none())
        {
            if !prefix_indices.is_empty() {
                return Err(unsupported(
                    "dynamic-subscript-after-rendered-index".to_owned(),
                    format!(
                        "dynamic subscript on scalarized element name `{}`",
                        classified.variable.name.as_str()
                    ),
                    Some(span),
                ));
            }
            return self.lower_dynamic_reference_selection(classified, subscripts, span);
        }
        let mut galec_subscripts = prefix_indices
            .into_iter()
            .map(gast::Expression::Integer)
            .collect::<Vec<_>>();
        galec_subscripts.extend(self.lower_subscripts(subscripts)?);
        Ok(Typed::new(
            state_ref(classified.galec_name.clone(), galec_subscripts),
            classified.scalar_type,
        ))
    }

    fn lower_slice_reference(
        &mut self,
        classified: &crate::classify::ClassifiedVariable<'_>,
        prefix_indices: Vec<i64>,
        subscripts: &[Subscript],
        span: Span,
    ) -> Result<Typed, GalecTargetError> {
        let mut full_subscripts = prefix_indices
            .into_iter()
            .map(|value| Subscript::index(value, Span::DUMMY))
            .collect::<Vec<_>>();
        full_subscripts.extend_from_slice(subscripts);
        let shape = full_subscripts
            .iter()
            .zip(&classified.variable.dims)
            .filter_map(|(subscript, dimension)| {
                matches!(subscript, Subscript::Colon { .. }).then_some(*dimension)
            })
            .collect::<Vec<_>>();
        let expr = self.slice_array_expression(classified, &mut full_subscripts, 0, span)?;
        Ok(Typed::array(expr, classified.scalar_type, shape))
    }

    fn slice_array_expression(
        &mut self,
        classified: &crate::classify::ClassifiedVariable<'_>,
        subscripts: &mut [Subscript],
        position: usize,
        span: Span,
    ) -> Result<gast::Expression, GalecTargetError> {
        let Some(colon_position) =
            subscripts
                .iter()
                .enumerate()
                .skip(position)
                .find_map(|(index, subscript)| {
                    matches!(subscript, Subscript::Colon { .. }).then_some(index)
                })
        else {
            let typed = self.lower_reference_access(classified, Vec::new(), subscripts, span)?;
            if !typed.is_scalar() {
                return Err(GalecTargetError::LoweringInternal {
                    detail: "array slice expansion produced a non-scalar element".to_owned(),
                });
            }
            return Ok(typed.expr);
        };
        let dimension = classified.variable.dims[colon_position];
        if dimension < 1 {
            return Err(GalecTargetError::LoweringInternal {
                detail: format!(
                    "non-positive slice dimension {dimension} on `{}` survived admissibility",
                    classified.variable.name.as_str()
                ),
            });
        }
        let original = subscripts[colon_position].clone();
        let mut elements = Vec::new();
        for index in 1..=dimension {
            subscripts[colon_position] = Subscript::index(index, span);
            elements.push(self.slice_array_expression(
                classified,
                subscripts,
                colon_position + 1,
                span,
            )?);
        }
        subscripts[colon_position] = original;
        Ok(gast::Expression::Array(elements))
    }

    fn lower_dynamic_reference_selection(
        &mut self,
        classified: &crate::classify::ClassifiedVariable<'_>,
        subscripts: &[Subscript],
        _span: Span,
    ) -> Result<Typed, GalecTargetError> {
        let dims = &classified.variable.dims;
        let combinations =
            self.static_index_combinations(dims, subscripts, classified.variable.name.as_str())?;
        let Some((else_indices, branch_indices)) = combinations.split_last() else {
            return Err(GalecTargetError::LoweringInternal {
                detail: "dynamic subscript expansion saw an empty index domain".to_owned(),
            });
        };
        let mut branches = Vec::with_capacity(branch_indices.len());
        for indices in branch_indices {
            let condition = self.dynamic_index_condition(subscripts, indices)?;
            branches.push((condition, self.static_reference_value(classified, indices)));
        }
        Ok(Typed::new(
            gast::Expression::If(IfExpression {
                branches,
                else_value: Box::new(self.static_reference_value(classified, else_indices)),
            }),
            classified.scalar_type,
        ))
    }

    fn static_reference_value(
        &self,
        classified: &crate::classify::ClassifiedVariable<'_>,
        indices: &[i64],
    ) -> gast::Expression {
        let subscripts = indices
            .iter()
            .copied()
            .map(gast::Expression::Integer)
            .collect();
        state_ref(classified.galec_name.clone(), subscripts)
    }

    fn dynamic_index_condition(
        &mut self,
        subscripts: &[Subscript],
        indices: &[i64],
    ) -> Result<gast::Expression, GalecTargetError> {
        let mut conditions = Vec::new();
        for (subscript, index) in subscripts.iter().zip(indices) {
            if self.static_index_value(subscript).is_some() {
                continue;
            }
            let Subscript::Expr { expr, span } = subscript else {
                return Err(unsupported(
                    "array-slice".to_owned(),
                    "`:` array slice subscript".to_owned(),
                    Some(subscript.span()),
                ));
            };
            let lhs = self.lower_as_integer(expr, *span)?;
            conditions.push(gast::Expression::binary(
                BinaryOp::Eq,
                lhs,
                gast::Expression::Integer(*index),
            ));
        }
        conjunction(conditions).ok_or_else(|| GalecTargetError::LoweringInternal {
            detail: "dynamic subscript expansion produced no dynamic conditions".to_owned(),
        })
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
                    if let Some(value) = self.static_index_value(subscript) {
                        return Ok(gast::Expression::Integer(value));
                    }
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

    fn static_index_value(&self, subscript: &Subscript) -> Option<i64> {
        match subscript {
            Subscript::Index { value, .. } => Some(*value),
            Subscript::Expr { expr, .. } => self.static_integer_expression(expr),
            Subscript::Colon { .. } => None,
        }
    }

    fn static_integer_expression(&self, expr: &Expression) -> Option<i64> {
        match expr {
            Expression::Literal {
                value: Literal::Integer(value),
                ..
            } => Some(*value),
            Expression::VarRef {
                name, subscripts, ..
            } if subscripts.is_empty() => self.constant_integer_value(name.as_str()),
            _ => None,
        }
    }

    fn constant_integer_value(&self, name: &str) -> Option<i64> {
        let classified = self.classification.find(name)?;
        if classified.class != VariableClass::Constant || !classified.variable.dims.is_empty() {
            return None;
        }
        match classified.variable.start.as_ref()? {
            Expression::Literal {
                value: Literal::Integer(value),
                ..
            } => Some(*value),
            _ => None,
        }
    }

    fn static_index_combinations(
        &self,
        dims: &[i64],
        subscripts: &[Subscript],
        variable: &str,
    ) -> Result<Vec<Vec<i64>>, GalecTargetError> {
        let mut choices = Vec::with_capacity(dims.len());
        for (dimension, subscript) in dims.iter().zip(subscripts) {
            choices.push(self.subscript_index_choices(*dimension, subscript, variable)?);
        }
        let mut result = vec![Vec::new()];
        for values in choices {
            result = extend_index_combinations(result, &values);
        }
        Ok(result)
    }

    fn subscript_index_choices(
        &self,
        dimension: i64,
        subscript: &Subscript,
        variable: &str,
    ) -> Result<Vec<i64>, GalecTargetError> {
        if let Some(index) = self.static_index_value(subscript) {
            return Ok(vec![index]);
        }
        if dimension < 1 {
            return Err(GalecTargetError::LoweringInternal {
                detail: format!(
                    "non-positive dimension {dimension} on `{variable}` survived admissibility"
                ),
            });
        }
        self.require_dynamic_subscript_range(subscript, dimension, variable)?;
        Ok((1..=dimension).collect())
    }

    fn require_dynamic_subscript_range(
        &self,
        subscript: &Subscript,
        dimension: i64,
        variable: &str,
    ) -> Result<(), GalecTargetError> {
        let Subscript::Expr { expr, span } = subscript else {
            return Ok(());
        };
        let Some((min, max)) = self.integer_expression_range(expr) else {
            return Err(unsupported(
                "dynamic-array-subscript-range".to_owned(),
                format!(
                    "dynamic subscript into `{variable}` must have statically known Integer \
                     bounds inside 1..{dimension}"
                ),
                Some(*span),
            ));
        };
        if min < 1 || max > dimension {
            return Err(unsupported(
                "dynamic-array-subscript-range".to_owned(),
                format!(
                    "dynamic subscript into `{variable}` has proven bounds {min}..{max}, \
                     outside 1..{dimension}"
                ),
                Some(*span),
            ));
        }
        Ok(())
    }

    fn integer_expression_range(&self, expr: &Expression) -> Option<(i64, i64)> {
        match expr {
            Expression::Literal {
                value: Literal::Integer(value),
                ..
            } => Some((*value, *value)),
            Expression::VarRef {
                name, subscripts, ..
            } if subscripts.is_empty() => self.variable_integer_range(name.as_str()),
            Expression::Unary {
                op: OpUnary::Minus,
                rhs,
                ..
            } => {
                let (min, max) = self.integer_expression_range(rhs)?;
                Some((max.checked_neg()?, min.checked_neg()?))
            }
            Expression::Binary {
                op: OpBinary::Add,
                lhs,
                rhs,
                ..
            } => {
                let (left_min, left_max) = self.integer_expression_range(lhs)?;
                let (right_min, right_max) = self.integer_expression_range(rhs)?;
                Some((
                    left_min.checked_add(right_min)?,
                    left_max.checked_add(right_max)?,
                ))
            }
            Expression::Binary {
                op: OpBinary::Sub,
                lhs,
                rhs,
                ..
            } => {
                let (left_min, left_max) = self.integer_expression_range(lhs)?;
                let (right_min, right_max) = self.integer_expression_range(rhs)?;
                Some((
                    left_min.checked_sub(right_max)?,
                    left_max.checked_sub(right_min)?,
                ))
            }
            _ => None,
        }
    }

    fn variable_integer_range(&self, name: &str) -> Option<(i64, i64)> {
        self.classified_integer_range(name)
            .or_else(|| pre_slot_base(name).and_then(|base| self.classified_integer_range(base)))
    }

    fn classified_integer_range(&self, name: &str) -> Option<(i64, i64)> {
        let classified = self.classification.find(name)?;
        let min = integer_bound(classified.variable.min.as_ref()?)?;
        let max = integer_bound(classified.variable.max.as_ref()?)?;
        (min <= max).then_some((min, max))
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

    fn lower_index(
        &mut self,
        base: &Expression,
        subscripts: &[Subscript],
        span: Span,
    ) -> Result<Typed, GalecTargetError> {
        if subscripts.is_empty() {
            return self.lower(base);
        }
        match base {
            Expression::VarRef {
                name,
                subscripts: base_subscripts,
                span,
            } => {
                let mut combined = base_subscripts.clone();
                combined.extend_from_slice(subscripts);
                self.lower_var_ref(name.as_str(), &combined, *span)
            }
            Expression::Array { elements, .. } => {
                let selected = self.select_array_element(elements, subscripts, span)?;
                self.lower(selected)
            }
            Expression::If {
                branches,
                else_branch,
                span,
            } => {
                let indexed_branches = branches
                    .iter()
                    .map(|(condition, value)| {
                        (
                            condition.clone(),
                            indexed_expression(value.clone(), subscripts.to_vec(), *span),
                        )
                    })
                    .collect();
                let indexed_else =
                    indexed_expression(else_branch.as_ref().clone(), subscripts.to_vec(), *span);
                self.lower(&Expression::If {
                    branches: indexed_branches,
                    else_branch: Box::new(indexed_else),
                    span: *span,
                })
            }
            Expression::Binary { op, lhs, rhs, span } => {
                let left = self.lower(lhs)?;
                let right = self.lower(rhs)?;
                if matches!(op, OpBinary::Mul) && vector_dot_shape(&left, &right).is_some() {
                    return Err(unsupported(
                        "scalar-indexed-expression".to_owned(),
                        "indexed expression over a vector dot-product result".to_owned(),
                        Some(*span),
                    ));
                }
                if left.is_scalar() && right.is_scalar() {
                    return Err(unsupported(
                        "scalar-indexed-expression".to_owned(),
                        "indexed expression over a scalar binary result".to_owned(),
                        Some(*span),
                    ));
                }
                let lhs = if left.is_scalar() {
                    lhs.as_ref().clone()
                } else {
                    indexed_expression(lhs.as_ref().clone(), subscripts.to_vec(), *span)
                };
                let rhs = if right.is_scalar() {
                    rhs.as_ref().clone()
                } else {
                    indexed_expression(rhs.as_ref().clone(), subscripts.to_vec(), *span)
                };
                self.lower(&Expression::Binary {
                    op: op.clone(),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    span: *span,
                })
            }
            Expression::Unary { op, rhs, span } => {
                let value = self.lower(rhs)?;
                if value.is_scalar() {
                    return Err(unsupported(
                        "scalar-indexed-expression".to_owned(),
                        "indexed expression over a scalar unary result".to_owned(),
                        Some(*span),
                    ));
                }
                self.lower(&Expression::Unary {
                    op: op.clone(),
                    rhs: Box::new(indexed_expression(
                        rhs.as_ref().clone(),
                        subscripts.to_vec(),
                        *span,
                    )),
                    span: *span,
                })
            }
            _ => Err(unsupported(
                "indexed-expression-base".to_owned(),
                format!("indexed expression over {}", form_name(base)),
                Some(span),
            )),
        }
    }

    fn lower_function_call(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[Expression],
        is_constructor: bool,
        span: Span,
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
        let Some(function) = self.functions.functions.get(&VarName::new(name.as_str())) else {
            return Err(unsupported(
                format!("user-function-call:{}", name.as_str()),
                format!("call to `{}` in a lowered expression", name.as_str()),
                Some(span),
            ));
        };
        if self
            .inlined_functions
            .iter()
            .any(|active| active == name.as_str())
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
        self.inlined_functions.push(name.as_str().to_owned());
        let expression = inline_function_call(function, args, span);
        let result = match expression {
            Ok(expression) => self.lower(&expression),
            Err(error) => Err(error),
        };
        self.inlined_functions.pop();
        result
    }

    fn select_array_element<'b>(
        &self,
        elements: &'b [Expression],
        subscripts: &[Subscript],
        span: Span,
    ) -> Result<&'b Expression, GalecTargetError> {
        let Some((first, rest)) = subscripts.split_first() else {
            return Err(GalecTargetError::LoweringInternal {
                detail: "array element selection called without subscripts".to_owned(),
            });
        };
        let Some(index) = self.static_index_value(first) else {
            return Err(unsupported(
                "dynamic-indexed-array-constructor".to_owned(),
                "dynamic subscript into an array constructor".to_owned(),
                Some(first.span()),
            ));
        };
        let index = usize::try_from(index)
            .ok()
            .filter(|index| *index >= 1 && *index <= elements.len())
            .ok_or_else(|| GalecTargetError::LoweringInternal {
                detail: format!(
                    "array constructor index {index} is outside 1..{}",
                    elements.len()
                ),
            })?;
        let selected = &elements[index - 1];
        if rest.is_empty() {
            return Ok(selected);
        }
        match selected {
            Expression::Array { elements, .. } => self.select_array_element(elements, rest, span),
            _ => Err(unsupported(
                "array-constructor-rank".to_owned(),
                "too many subscripts for array constructor rank".to_owned(),
                Some(span),
            )),
        }
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

fn optional(span: Span) -> Option<Span> {
    (!span.is_dummy()).then_some(span)
}
