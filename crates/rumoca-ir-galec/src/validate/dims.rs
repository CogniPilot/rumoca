//! Dimensionality analysis (S-2.13/S-2.14, S-3.1, trap T11): array
//! subscripts, declared dimensions, `size()` dimension arguments, and loop
//! bounds are constant scalar Integer expressions — references inside them
//! must be loop iterators (or the array operand of `size()`), calls must be
//! builtins, and signaling builtins are excluded (§3.2.6 L-2: statically
//! evaluated positions are applied at Production-Code-generation time,
//! where error signaling is not permitted); subscript counts must match the
//! declared dimensionality; literal subscripts must lie within literal
//! declared dimensions (the trivially decidable slice of §3.2.1(a) bounds
//! safety); block-level dimensions are Integer literals >= 1 (GAL-020);
//! derived dimensions `:` are legal only on function input parameters.
//!
//! Integer-typedness of these positions is the type analysis' job; this
//! module owns only the structural staticness rules, so no defect is
//! reported twice.

use crate::ast::{
    Condition, Dimension, Direction, Expression, ForLoop, Reference, Spanned, Statement,
    VariableDeclaration,
};
use crate::diagnostic::{GalecError, PathSegment};

use super::context::{
    BlockContext, BodyView, Callee, Cursor, FunctionScope, PartDims, lexeme, reference_parts,
    resolve, resolve_call,
};

pub(super) fn check(ctx: &BlockContext<'_>, diags: &mut Vec<GalecError>) {
    check_block_declarations(ctx, diags);
    for body in ctx.bodies() {
        let mut checker = DimChecker {
            ctx,
            scope: FunctionScope::new(&body),
            cursor: Cursor::for_body(ctx, &body),
            diags,
        };
        checker.declarations(&body);
        checker.statements(body.statements);
    }
}

/// Block-level (state entity and compartment entity) dimensions must be
/// Integer literals >= 1: no derived dimensions, no `size()`, no structural
/// parameters (S-2.13, GAL-020).
fn check_block_declarations(ctx: &BlockContext<'_>, diags: &mut Vec<GalecError>) {
    let block = PathSegment::Block(lexeme(&ctx.block.name));
    // Interface + protected entities: [Block, Variable].
    let flat = ctx
        .block
        .interface
        .iter()
        .map(|v| &v.decl)
        .chain(ctx.block.protected.iter().map(|e| &e.decl));
    for decl in flat {
        let location = crate::diagnostic::Location::at(vec![
            block.clone(),
            PathSegment::Variable(lexeme(&decl.name)),
        ]);
        check_declaration_dims(decl, &location, diags);
    }
    // Compartment entities are qualified `[Block, Compartment, Variable]`
    // (matching the name analysis) so `span_of` resolves the entity — a
    // `[Block, Variable]` path would mis-resolve to a same-named block
    // interface variable or fall back to the whole-block span (D11).
    for compartment in &ctx.block.compartments {
        let compartment_segment = PathSegment::Compartment(lexeme(&compartment.name));
        for entity in &compartment.entities {
            let location = crate::diagnostic::Location::at(vec![
                block.clone(),
                compartment_segment.clone(),
                PathSegment::Variable(lexeme(&entity.decl.name)),
            ]);
            check_declaration_dims(&entity.decl, &location, diags);
        }
    }
}

/// One block/compartment declaration's dimension checks against its resolved
/// `location`: block-level dims must be Integer literals >= 1 — no derived
/// dimensions, no `size()`, no structural parameters (S-2.13, GAL-020).
fn check_declaration_dims(
    decl: &VariableDeclaration,
    location: &crate::diagnostic::Location,
    diags: &mut Vec<GalecError>,
) {
    for dimension in &decl.dimensions {
        match dimension {
            Dimension::Derived => diags.push(GalecError::DerivedDimensionOutsideInput {
                location: location.clone(),
                name: lexeme(&decl.name),
            }),
            Dimension::Expr(Expression::Integer(size)) if *size >= 1 => {}
            Dimension::Expr(_) => diags.push(GalecError::BlockDimensionNotLiteral {
                location: location.clone(),
                name: lexeme(&decl.name),
            }),
        }
    }
}

struct DimChecker<'a, 'd> {
    ctx: &'a BlockContext<'a>,
    scope: FunctionScope<'a>,
    cursor: Cursor,
    diags: &'d mut Vec<GalecError>,
}

impl<'a> DimChecker<'a, '_> {
    /// Function declarations: derived dimensions only on inputs (S-2.14);
    /// output/local dimension expressions must be static (`size()` of other
    /// parameters is the intended idiom).
    fn declarations(&mut self, body: &BodyView<'a>) {
        for parameter in body.parameters {
            let allow_derived = parameter.direction == Direction::Input;
            self.declaration_dims(&parameter.decl, allow_derived);
        }
        for local in body.locals {
            self.declaration_dims(local, false);
        }
    }

    fn declaration_dims(&mut self, decl: &'a VariableDeclaration, allow_derived: bool) {
        self.cursor.push(PathSegment::Local(lexeme(&decl.name)));
        for dimension in &decl.dimensions {
            match dimension {
                Dimension::Derived if allow_derived => {}
                Dimension::Derived => self.diags.push(GalecError::DerivedDimensionOutsideInput {
                    location: self.cursor.here(),
                    name: lexeme(&decl.name),
                }),
                Dimension::Expr(expression) => {
                    self.check_static(expression, "declared dimension");
                }
            }
        }
        self.cursor.pop();
    }

    fn statements(&mut self, statements: &'a [Spanned<Statement>]) {
        for (index, statement) in statements.iter().enumerate() {
            self.cursor.push(PathSegment::Statement(index));
            self.statement(&statement.node);
            self.cursor.pop();
        }
    }

    fn statement(&mut self, statement: &'a Statement) {
        match statement {
            Statement::Assignment { target, value } => {
                self.reference(target);
                self.expression(value);
            }
            Statement::MultiAssignment { targets, call } => {
                for target in targets {
                    self.reference(target);
                }
                self.call_arguments(&call.arguments);
            }
            Statement::Call(call) => self.call_arguments(&call.arguments),
            Statement::If(if_statement) => self.if_statement(if_statement),
            Statement::For(for_loop) => self.for_loop(for_loop),
            Statement::Limit(targets) => self.limit(targets),
            Statement::Signal(_) => {}
        }
    }

    fn limit(&mut self, targets: &'a [crate::ast::LimitTarget]) {
        for target in targets {
            if let crate::ast::LimitTarget::Reference(reference) = target {
                self.reference(reference);
            }
        }
    }

    fn if_statement(&mut self, if_statement: &'a crate::ast::IfStatement) {
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

    fn condition(&mut self, condition: &'a Condition) {
        match condition {
            Condition::Expression(expression) => self.expression(expression),
            Condition::SignalCheck(check) => {
                if let Some(fallback) = &check.fallback {
                    self.expression(fallback);
                }
            }
        }
    }

    /// Loop bounds are static; the loop's own iterator is not in scope for
    /// its bounds (outer iterators are).
    fn for_loop(&mut self, for_loop: &'a ForLoop) {
        self.check_static(&for_loop.start, "for-loop bound");
        if let Some(step) = &for_loop.step {
            self.check_static(step, "for-loop bound");
        }
        self.check_static(&for_loop.stop, "for-loop bound");
        self.scope.push_iterator(for_loop.iterator.as_ref());
        self.statements(&for_loop.body);
        self.scope.pop_iterator();
    }

    // -----------------------------------------------------------------
    // Dynamic-expression walk: find every static position
    // -----------------------------------------------------------------

    fn expression(&mut self, expression: &'a Expression) {
        match expression {
            Expression::Bool(_) | Expression::Integer(_) | Expression::Real(_) => {}
            Expression::Ref(reference) | Expression::Neg(reference) => self.reference(reference),
            Expression::Size { array, dimension } => {
                self.reference(array);
                self.check_static(dimension, "size() dimension argument");
            }
            Expression::Call(call) => self.call_arguments(&call.arguments),
            Expression::Paren(inner) | Expression::Not(inner) => self.expression(inner),
            Expression::If(if_expression) => {
                for (condition, value) in &if_expression.branches {
                    self.expression(condition);
                    self.expression(value);
                }
                self.expression(&if_expression.else_value);
            }
            Expression::Array(elements) => {
                for element in elements {
                    self.expression(element);
                }
            }
            Expression::Binary { lhs, rhs, .. } => {
                self.expression(lhs);
                self.expression(rhs);
            }
        }
    }

    fn call_arguments(&mut self, arguments: &'a [Expression]) {
        for argument in arguments {
            self.expression(argument);
        }
    }

    /// Check a reference's subscripts: each is static, their count is zero
    /// (whole entity) or exactly the declared dimensionality, and literal
    /// subscripts lie within literal declared dimensions.
    fn reference(&mut self, reference: &'a Reference) {
        for part in reference_parts(reference) {
            for subscript in &part.subscripts {
                self.check_static(subscript, "array subscript");
            }
        }
        // Resolution failures are reported by the type analysis.
        let Ok(resolved) = resolve(self.ctx, &self.scope, reference) else {
            return;
        };
        for part_dims in &resolved.parts {
            let found = part_dims.part.subscripts.len();
            if found != 0 && found != part_dims.dimensions.len() {
                self.diags.push(GalecError::SubscriptArity {
                    location: self.cursor.here(),
                    name: lexeme(&part_dims.part.name),
                    declared: part_dims.dimensions.len(),
                    found,
                });
                continue;
            }
            self.literal_bounds(part_dims);
        }
    }

    /// The trivially decidable slice of §3.2.1(a) bounds safety: a literal
    /// subscript checked against a literal declared dimension (1-based).
    /// Non-literal subscripts and dimensions await a static evaluator (the
    /// "GALEC language conformance" ladder rung).
    fn literal_bounds(&mut self, part_dims: &PartDims<'a>) {
        let pairs = part_dims
            .part
            .subscripts
            .iter()
            .zip(part_dims.dimensions)
            .enumerate();
        for (position, (subscript, dimension)) in pairs {
            let (Expression::Integer(index), Dimension::Expr(Expression::Integer(size))) =
                (subscript, dimension)
            else {
                continue;
            };
            if *index < 1 || index > size {
                self.diags.push(GalecError::SubscriptOutOfBounds {
                    location: self.cursor.here(),
                    name: lexeme(&part_dims.part.name),
                    dimension: position + 1,
                    index: *index,
                    size: *size,
                });
            }
        }
    }

    // -----------------------------------------------------------------
    // Static (constant-scalar-integer) expression rules, S-3.1
    // -----------------------------------------------------------------

    /// References must be loop iterators (or live inside `size()`); calls
    /// must be builtins. Typing of the position is the type analysis' job.
    fn check_static(&mut self, expression: &'a Expression, context: &'static str) {
        match expression {
            Expression::Bool(_) | Expression::Integer(_) | Expression::Real(_) => {}
            Expression::Ref(reference) => self.static_reference(reference, context),
            Expression::Neg(reference) => self.static_reference(reference, context),
            Expression::Size { array, dimension } => {
                self.reference(array);
                self.check_static(dimension, context);
            }
            Expression::Call(call) => self.static_call(call, context),
            Expression::Paren(inner) | Expression::Not(inner) => {
                self.check_static(inner, context);
            }
            Expression::If(if_expression) => {
                for (condition, value) in &if_expression.branches {
                    self.check_static(condition, context);
                    self.check_static(value, context);
                }
                self.check_static(&if_expression.else_value, context);
            }
            Expression::Array(elements) => {
                for element in elements {
                    self.check_static(element, context);
                }
            }
            Expression::Binary { lhs, rhs, .. } => {
                self.check_static(lhs, context);
                self.check_static(rhs, context);
            }
        }
    }

    fn static_reference(&mut self, reference: &'a Reference, context: &'static str) {
        let is_iterator = match reference {
            Reference::Local(part) if part.subscripts.is_empty() => {
                self.scope.is_iterator(&lexeme(&part.name))
            }
            _ => false,
        };
        if !is_iterator {
            self.diags.push(GalecError::NonStaticExpression {
                location: self.cursor.here(),
                context,
                reason: "references must be loop iterators or the array operand of size()",
            });
        }
    }

    fn static_call(&mut self, call: &'a crate::ast::FunctionCall, context: &'static str) {
        match resolve_call(self.ctx, &call.function) {
            Some(Callee::User(_)) => self.diags.push(GalecError::NonStaticExpression {
                location: self.cursor.here(),
                context,
                reason: "calls must be builtins",
            }),
            // §3.2.6 L-2: statically-evaluated expressions are applied at
            // Production-Code-generation time, where error signaling is not
            // permitted — signaling builtins (`integer`, the linear
            // solvers) are excluded.
            Some(Callee::Builtin(builtin) | Callee::Lifted { base: builtin, .. })
                if !builtin.signals.is_empty() =>
            {
                self.diags.push(GalecError::NonStaticExpression {
                    location: self.cursor.here(),
                    context,
                    reason: "error-signaling builtins are not permitted in \
                             statically-evaluated expressions",
                });
            }
            // Unknown callees are reported by the type analysis.
            Some(Callee::Builtin(_) | Callee::Lifted { .. }) | None => {}
        }
        for argument in &call.arguments {
            self.check_static(argument, context);
        }
    }
}
