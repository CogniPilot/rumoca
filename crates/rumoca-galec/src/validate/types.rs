//! Type analysis (S-3.4, S-2.7, trap T5): equal-typed binary operands with
//! no implicit Integer<->Real promotion, `/` Real-only, `^` returning Real,
//! relational/logical typing, equally-typed if-expression branches (the
//! mandatory `else` is unrepresentable without one), call signatures,
//! Integer-typed declared dimensions (S-3.1), and the component-type
//! restrictions (no component-typed parameters/locals, components legal
//! only in `size()`/`limit`).
//!
//! Array-nativeness (GAL-026): S-3.4 states no dimensionality restriction,
//! and the normative §3.2.7 LinearEquationSystem example uses vector
//! arithmetic with scalar broadcast (`x + stepSize * 'derivative(x)'`).
//! Arithmetic operators therefore apply element-type-wise with shape
//! agreement (equal ranks, or one scalar operand broadcasting). Relational,
//! equality, and logical operators stay scalar-only: their conceptual
//! Beta-1 signatures (§3.2.5 §2's `f_⊕`) are scalar, and element-wise
//! variants exist only as lifted builtins.
//!
//! This analysis is also the single reporter of resolution failures
//! (EG014/EG015): the other analyses resolve silently so each unresolved
//! name is diagnosed exactly once.

use crate::ast::{
    BinaryOp, Condition, Dimension, Expression, FunctionCall, IfExpression, IfStatement,
    LimitTarget, PrecedenceClass, Reference, ScalarType, Statement, TypeRef, VariableDeclaration,
};
use crate::diagnostic::{GalecError, PathSegment};

use super::context::{
    BlockContext, BodyView, Cursor, FunctionScope, Resolved, Ty, lexeme, reference_lexeme,
    reference_parts, resolve, resolve_call,
};

pub(super) fn check(ctx: &BlockContext<'_>, diags: &mut Vec<GalecError>) {
    for body in ctx.bodies() {
        let mut checker = TypeChecker {
            ctx,
            scope: FunctionScope::new(&body),
            cursor: Cursor::for_body(ctx, &body),
            diags,
        };
        checker.declarations(&body);
        checker.statements(body.statements);
    }
}

struct TypeChecker<'a, 'd> {
    ctx: &'a BlockContext<'a>,
    scope: FunctionScope<'a>,
    cursor: Cursor,
    diags: &'d mut Vec<GalecError>,
}

impl<'a> TypeChecker<'a, '_> {
    fn declarations(&mut self, body: &BodyView<'a>) {
        let decls = body
            .parameters
            .iter()
            .map(|p| &p.decl)
            .chain(body.locals.iter());
        for decl in decls {
            self.cursor.push(PathSegment::Local(lexeme(&decl.name)));
            if matches!(decl.ty, TypeRef::Compartment(_)) {
                self.diags.push(GalecError::ComponentTypedDeclaration {
                    location: self.cursor.here(),
                    name: lexeme(&decl.name),
                });
            }
            self.declaration_dims(decl);
            self.cursor.pop();
        }
    }

    /// Declared dimensions are constant-scalar-integer-expressions (S-3.1):
    /// Integer-typed and scalar. Staticness is the dimensionality analysis'
    /// job; block-level dims are covered by its literal check (EG024).
    fn declaration_dims(&mut self, decl: &'a VariableDeclaration) {
        for dimension in &decl.dimensions {
            if let Dimension::Expr(expression) = dimension {
                self.expect_integer(expression, "declared dimension");
            }
        }
    }

    fn statements(&mut self, statements: &'a [Statement]) {
        for (index, statement) in statements.iter().enumerate() {
            self.cursor.push(PathSegment::Statement(index));
            self.statement(statement);
            self.cursor.pop();
        }
    }

    fn statement(&mut self, statement: &'a Statement) {
        match statement {
            Statement::Assignment { target, value } => self.assignment(target, value),
            Statement::MultiAssignment { targets, call } => self.multi_assignment(targets, call),
            Statement::Call(call) => {
                self.check_call(call);
            }
            Statement::If(if_statement) => self.if_statement(if_statement),
            Statement::For(for_loop) => self.for_loop(for_loop),
            Statement::Limit(targets) => self.limit(targets),
            Statement::Signal(_) => {}
        }
    }

    fn assignment(&mut self, target: &'a Reference, value: &'a Expression) {
        let target_ty = self.reference_ty(target);
        let value_ty = self.type_of(value);
        if target_ty.is_known() && value_ty.is_known() && target_ty != value_ty {
            self.mismatch(
                format!("assignment to `{}`", reference_lexeme(target)),
                target_ty.describe(),
                value_ty.describe(),
            );
        }
    }

    fn multi_assignment(&mut self, targets: &'a [Reference], call: &'a FunctionCall) {
        let target_tys: Vec<Ty> = targets.iter().map(|t| self.reference_ty(t)).collect();
        let Some(outputs) = self.check_call(call) else {
            return;
        };
        if outputs.len() != targets.len() {
            self.diags.push(GalecError::CallOutputArity {
                location: self.cursor.here(),
                function: lexeme(&call.function),
                context: "multi-assignment",
                expected: outputs.len(),
                found: targets.len(),
            });
        }
        for ((target, target_ty), output_ty) in targets.iter().zip(target_tys).zip(outputs) {
            if target_ty.is_known() && output_ty.is_known() && target_ty != output_ty {
                // Same orientation as single assignments: the target type
                // is expected, the assigned (output) type is found.
                self.mismatch(
                    format!("multi-assignment target `{}`", reference_lexeme(target)),
                    target_ty.describe(),
                    output_ty.describe(),
                );
            }
        }
    }

    fn if_statement(&mut self, if_statement: &'a IfStatement) {
        for (index, branch) in if_statement.branches.iter().enumerate() {
            self.cursor.push(PathSegment::Branch(index));
            self.condition(&branch.condition);
            self.statements(&branch.body);
            self.cursor.pop();
        }
        if let Some(else_body) = &if_statement.else_body {
            self.cursor.push(PathSegment::Else);
            self.statements(else_body);
            self.cursor.pop();
        }
    }

    /// Branch conditions are Boolean expressions or signal checks whose
    /// optional `or` fallback is Boolean (§3.2.5 §1.4).
    fn condition(&mut self, condition: &'a Condition) {
        match condition {
            Condition::Expression(expression) => {
                self.expect_boolean(expression, "if condition");
            }
            Condition::SignalCheck(check) => {
                if let Some(fallback) = &check.fallback {
                    self.expect_boolean(fallback, "signal-check fallback condition");
                }
            }
        }
    }

    fn for_loop(&mut self, for_loop: &'a crate::ast::ForLoop) {
        self.expect_integer(&for_loop.start, "for-loop bound");
        if let Some(step) = &for_loop.step {
            self.expect_integer(step, "for-loop bound");
        }
        self.expect_integer(&for_loop.stop, "for-loop bound");
        self.scope.push_iterator(for_loop.iterator.as_ref());
        self.statements(&for_loop.body);
        self.scope.pop_iterator();
    }

    /// Limit targets may be variables OR components (§3.2.4
    /// reference-typing rule; unnumbered `S-TODO` in Beta-1).
    fn limit(&mut self, targets: &'a [LimitTarget]) {
        for target in targets {
            let LimitTarget::Reference(reference) = target else {
                continue;
            };
            self.subscript_types(reference);
            if resolve(self.ctx, &self.scope, reference).is_err() {
                self.unresolved(reference_lexeme(reference));
            }
        }
    }

    // -----------------------------------------------------------------
    // Expression typing
    // -----------------------------------------------------------------

    fn type_of(&mut self, expression: &'a Expression) -> Ty {
        match expression {
            Expression::Bool(_) => Ty::Scalar(ScalarType::Boolean),
            Expression::Integer(_) => Ty::Scalar(ScalarType::Integer),
            Expression::Real(_) => Ty::Scalar(ScalarType::Real),
            Expression::Ref(reference) => self.reference_ty(reference),
            Expression::Size { array, dimension } => self.size_query(array, dimension),
            Expression::Call(call) => self.expression_call(call),
            Expression::Paren(inner) => self.type_of(inner),
            Expression::If(if_expression) => self.if_expression(if_expression),
            Expression::Array(elements) => self.array_constructor(elements),
            Expression::Neg(reference) => self.negation(reference),
            Expression::Not(inner) => {
                self.expect_boolean(inner, "not operand");
                Ty::Scalar(ScalarType::Boolean)
            }
            Expression::Binary { op, lhs, rhs } => self.binary(*op, lhs, rhs),
        }
    }

    /// Type a reference, reporting unresolved names and component-value
    /// misuse; subscripted access yields the element type.
    fn reference_ty(&mut self, reference: &'a Reference) -> Ty {
        self.subscript_types(reference);
        let resolved = match resolve(self.ctx, &self.scope, reference) {
            Ok(resolved) => resolved,
            Err(name) => {
                self.unresolved(name);
                return Ty::Unknown;
            }
        };
        let decl: &VariableDeclaration = match resolved.target {
            Resolved::Iterator => return Ty::Scalar(ScalarType::Integer),
            Resolved::Component { decl } => {
                self.diags.push(GalecError::ComponentValueUse {
                    location: self.cursor.here(),
                    name: lexeme(&decl.name),
                });
                return Ty::Unknown;
            }
            Resolved::Entity { decl, .. } | Resolved::Local(decl) => decl,
            Resolved::Parameter(parameter) => &parameter.decl,
        };
        let subscripts = resolved.parts.last().map_or(0, |p| p.part.subscripts.len());
        match Ty::of_decl(decl) {
            Ty::Array(scalar, _) if subscripts > 0 => Ty::Scalar(scalar),
            other => other,
        }
    }

    /// Subscript expressions are Integer scalars everywhere they occur
    /// (their staticness is the dimensionality analysis' concern).
    fn subscript_types(&mut self, reference: &'a Reference) {
        for part in reference_parts(reference) {
            for subscript in &part.subscripts {
                self.expect_integer(subscript, "array subscript");
            }
        }
    }

    fn size_query(&mut self, array: &'a Reference, dimension: &'a Expression) -> Ty {
        self.subscript_types(array);
        if resolve(self.ctx, &self.scope, array).is_err() {
            self.unresolved(reference_lexeme(array));
        }
        self.expect_integer(dimension, "size() dimension argument");
        Ty::Scalar(ScalarType::Integer)
    }

    /// Calls in expressions must have output-arity 1 (§3.2.4 call-typing
    /// rule; unnumbered `S-TODO` in Beta-1).
    fn expression_call(&mut self, call: &'a FunctionCall) -> Ty {
        let Some(outputs) = self.check_call(call) else {
            return Ty::Unknown;
        };
        if outputs.len() != 1 {
            self.diags.push(GalecError::CallOutputArity {
                location: self.cursor.here(),
                function: lexeme(&call.function),
                context: "an expression",
                expected: 1,
                found: outputs.len(),
            });
            return Ty::Unknown;
        }
        outputs[0]
    }

    /// Check a call's signature; returns its output types, or `None` when
    /// the callee is unknown (reported here, once).
    fn check_call(&mut self, call: &'a FunctionCall) -> Option<Vec<Ty>> {
        let Some(callee) = resolve_call(self.ctx, &call.function) else {
            for argument in &call.arguments {
                self.type_of(argument);
            }
            self.diags.push(GalecError::UnknownFunction {
                location: self.cursor.here(),
                name: lexeme(&call.function),
            });
            return None;
        };
        let inputs = callee.inputs();
        if call.arguments.len() != inputs.len() {
            self.diags.push(GalecError::CallInputArity {
                location: self.cursor.here(),
                function: callee.display_name(),
                expected: inputs.len(),
                found: call.arguments.len(),
            });
        }
        for (index, argument) in call.arguments.iter().enumerate() {
            let found = self.type_of(argument);
            if let Some(expected) = inputs.get(index)
                && expected.is_known()
                && found.is_known()
                && found != *expected
            {
                self.mismatch(
                    format!("argument {} of `{}`", index + 1, callee.display_name()),
                    expected.describe(),
                    found.describe(),
                );
            }
        }
        Some(callee.outputs())
    }

    /// If-expression branches must be equally typed (trap T12; the `else`
    /// branch is structurally mandatory).
    fn if_expression(&mut self, if_expression: &'a IfExpression) -> Ty {
        let mut result = Ty::Unknown;
        for (condition, value) in &if_expression.branches {
            self.expect_boolean(condition, "if-expression condition");
            let value_ty = self.type_of(value);
            result = self.unify_branch(result, value_ty);
        }
        let else_ty = self.type_of(&if_expression.else_value);
        self.unify_branch(result, else_ty)
    }

    fn unify_branch(&mut self, current: Ty, found: Ty) -> Ty {
        match (current.is_known(), found.is_known()) {
            (false, _) => found,
            (true, false) => current,
            (true, true) => {
                if current != found {
                    self.mismatch(
                        "if-expression branches".to_string(),
                        current.describe(),
                        found.describe(),
                    );
                }
                current
            }
        }
    }

    fn array_constructor(&mut self, elements: &'a [Expression]) -> Ty {
        let mut element_ty = Ty::Unknown;
        for element in elements {
            let found = self.type_of(element);
            if element_ty.is_known() && found.is_known() && element_ty != found {
                self.mismatch(
                    "multi-dimension constructor elements".to_string(),
                    element_ty.describe(),
                    found.describe(),
                );
            } else if !element_ty.is_known() {
                element_ty = found;
            }
        }
        match element_ty {
            Ty::Scalar(scalar) => Ty::Array(scalar, 1),
            Ty::Array(scalar, rank) => Ty::Array(scalar, rank + 1),
            Ty::Unknown => Ty::Unknown,
        }
    }

    /// Unary minus binds to references only (structural, trap T4) and
    /// requires Integer or Real elements (S-3.4); arrays negate
    /// element-wise (GAL-026).
    fn negation(&mut self, reference: &'a Reference) -> Ty {
        let ty = self.reference_ty(reference);
        let numeric = matches!(
            ty.element(),
            None | Some(ScalarType::Integer | ScalarType::Real)
        );
        if !numeric {
            self.mismatch(
                "unary minus operand".to_string(),
                "Integer or Real".to_string(),
                ty.describe(),
            );
            return Ty::Unknown;
        }
        ty
    }

    fn binary(&mut self, op: BinaryOp, lhs: &'a Expression, rhs: &'a Expression) -> Ty {
        let operands = BinaryOperands {
            op,
            lhs: self.type_of(lhs),
            rhs: self.type_of(rhs),
        };
        match op.precedence_class() {
            PrecedenceClass::Power => {
                self.require_numeric(&operands, "`^` requires Integer or Real operands");
                self.real_result(&operands)
            }
            PrecedenceClass::Multiplicative if op == BinaryOp::Div => {
                self.require_real(&operands);
                self.real_result(&operands)
            }
            PrecedenceClass::Multiplicative | PrecedenceClass::Additive => {
                self.require_numeric(&operands, "requires equally-typed Integer or Real operands");
                let element = Self::common_element(&operands);
                let rank = self.arithmetic_shape(&operands);
                match (element, rank) {
                    (Some(element), Some(0)) => Ty::Scalar(element),
                    (Some(element), Some(rank)) => Ty::Array(element, rank),
                    _ => Ty::Unknown,
                }
            }
            PrecedenceClass::Relational => {
                self.require_scalar(&operands, "relational operators compare scalars");
                self.require_numeric(&operands, "requires equally-typed Integer or Real operands");
                Ty::Scalar(ScalarType::Boolean)
            }
            PrecedenceClass::Equality => {
                self.require_scalar(&operands, "equality operators compare scalars");
                self.require_equal(&operands);
                Ty::Scalar(ScalarType::Boolean)
            }
            PrecedenceClass::LogicalAnd | PrecedenceClass::LogicalOr => {
                self.require_scalar(&operands, "logical operands must be scalar");
                self.require_boolean(&operands);
                Ty::Scalar(ScalarType::Boolean)
            }
        }
    }

    /// Shape agreement for arithmetic operators: equal ranks combine
    /// element-wise; a scalar broadcasts against an array (normative
    /// LinearEquationSystem idiom, §3.2.7). Returns the result rank, or
    /// `None` when both sides are unknown or the ranks disagree (reported).
    fn arithmetic_shape(&mut self, operands: &BinaryOperands) -> Option<usize> {
        match (operands.lhs.rank(), operands.rhs.rank()) {
            (Some(lhs), Some(rhs)) if lhs == rhs || lhs.min(rhs) == 0 => Some(lhs.max(rhs)),
            (Some(_), Some(_)) => {
                self.operands_error(
                    operands,
                    "array operands must have equal dimensionality unless one is a scalar",
                );
                None
            }
            // One unknown side: assume it conforms (already reported once).
            (known, None) | (None, known) => known,
        }
    }

    /// The shared element type of arithmetic operands, if any. Mismatched
    /// element types were already reported by [`Self::require_numeric`].
    fn common_element(operands: &BinaryOperands) -> Option<ScalarType> {
        match (operands.lhs.element(), operands.rhs.element()) {
            (Some(lhs), Some(rhs)) => (lhs == rhs).then_some(lhs),
            (element, None) | (None, element) => element,
        }
    }

    /// `^` and `/` always produce Real elements (S-3.4), with the shared
    /// arithmetic shape rules.
    fn real_result(&mut self, operands: &BinaryOperands) -> Ty {
        match self.arithmetic_shape(operands) {
            Some(0) => Ty::Scalar(ScalarType::Real),
            Some(rank) => Ty::Array(ScalarType::Real, rank),
            None => Ty::Unknown,
        }
    }

    fn require_numeric(&mut self, operands: &BinaryOperands, requirement: &'static str) {
        let bad = |ty: Ty| {
            ty.element()
                .is_some_and(|e| !matches!(e, ScalarType::Integer | ScalarType::Real))
        };
        let unequal = matches!(
            (operands.lhs.element(), operands.rhs.element()),
            (Some(lhs), Some(rhs)) if lhs != rhs
        );
        let allow_mixed = operands.op == BinaryOp::Pow;
        if bad(operands.lhs) || bad(operands.rhs) || (unequal && !allow_mixed) {
            self.operands_error(operands, requirement);
        }
    }

    fn require_real(&mut self, operands: &BinaryOperands) {
        let bad = |ty: Ty| ty.element().is_some_and(|e| e != ScalarType::Real);
        if bad(operands.lhs) || bad(operands.rhs) {
            self.operands_error(operands, "`/` requires Real operands");
        }
    }

    fn require_scalar(&mut self, operands: &BinaryOperands, requirement: &'static str) {
        if operands.any_known_array() {
            self.operands_error(operands, requirement);
        }
    }

    fn require_equal(&mut self, operands: &BinaryOperands) {
        if operands.both_known_scalars() && operands.lhs != operands.rhs {
            self.operands_error(operands, "requires equally-typed operands");
        }
    }

    fn require_boolean(&mut self, operands: &BinaryOperands) {
        let bad = |ty: Ty| ty.element().is_some_and(|e| e != ScalarType::Boolean);
        if bad(operands.lhs) || bad(operands.rhs) {
            self.operands_error(operands, "requires Boolean operands");
        }
    }

    // -----------------------------------------------------------------
    // Reporting helpers
    // -----------------------------------------------------------------

    fn expect_boolean(&mut self, expression: &'a Expression, context: &str) {
        let found = self.type_of(expression);
        if found.is_known() && found != Ty::Scalar(ScalarType::Boolean) {
            self.mismatch(context.to_string(), "Boolean".to_string(), found.describe());
        }
    }

    fn expect_integer(&mut self, expression: &'a Expression, context: &str) {
        let found = self.type_of(expression);
        if found.is_known() && found != Ty::Scalar(ScalarType::Integer) {
            self.mismatch(context.to_string(), "Integer".to_string(), found.describe());
        }
    }

    fn mismatch(&mut self, context: String, expected: String, found: String) {
        self.diags.push(GalecError::TypeMismatch {
            location: self.cursor.here(),
            detail: Box::new(crate::diagnostic::TypeMismatchDetail {
                context,
                expected,
                found,
            }),
        });
    }

    fn operands_error(&mut self, operands: &BinaryOperands, requirement: &'static str) {
        self.diags.push(GalecError::BinaryOperandTypes {
            location: self.cursor.here(),
            op: operands.op.token(),
            operands: format!(
                "{} {} {}",
                operands.lhs.describe(),
                operands.op.token(),
                operands.rhs.describe()
            ),
            requirement,
        });
    }

    fn unresolved(&mut self, name: String) {
        self.diags.push(GalecError::UnresolvedReference {
            location: self.cursor.here(),
            name,
        });
    }
}

struct BinaryOperands {
    op: BinaryOp,
    lhs: Ty,
    rhs: Ty,
}

impl BinaryOperands {
    fn any_known_array(&self) -> bool {
        matches!(self.lhs, Ty::Array(..)) || matches!(self.rhs, Ty::Array(..))
    }

    fn both_known_scalars(&self) -> bool {
        matches!(self.lhs, Ty::Scalar(_)) && matches!(self.rhs, Ty::Scalar(_))
    }
}
