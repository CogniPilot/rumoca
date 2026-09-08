use rumoca_core::ScopeId;
use rumoca_ir_ast as ast;
use std::sync::Arc;

type ComponentReference = ast::ComponentReference;
type Equation = ast::Equation;
type Expression = ast::Expression;
type Statement = ast::Statement;

/// Callback contract for resolve traversal.
///
/// Traversal is centralized here while semantic actions stay in callbacks.
pub(crate) trait ResolveTraversalCallbacks {
    fn create_loop_scope(&mut self, enclosing: ScopeId) -> ScopeId;
    fn bind_loop_index_name(&mut self, loop_scope: ScopeId, index_name: &str);
    fn on_component_reference(&mut self, comp: &mut ComponentReference, scope: ScopeId);
    fn on_function_reference(&mut self, comp: &mut ComponentReference, scope: ScopeId);
    fn on_modification_target(&mut self, comp: &mut ComponentReference, scope: ScopeId);
    /// Prove the projected member identity of `base.field` (MLS §3.7.3).
    ///
    /// Called after the base expression has been walked, so a base that is a
    /// component reference already carries its resolved declaration identity.
    fn on_field_access(
        &mut self,
        base: &Expression,
        field: &str,
        field_def_id: &mut Option<rumoca_core::DefId>,
        span: rumoca_core::Span,
        scope: ScopeId,
    );
}

pub(crate) fn walk_equations<C: ResolveTraversalCallbacks>(
    callbacks: &mut C,
    equations: &mut [Equation],
    scope: ScopeId,
) {
    for equation in equations {
        walk_equation(callbacks, equation, scope);
    }
}

pub(crate) fn walk_equation<C: ResolveTraversalCallbacks>(
    callbacks: &mut C,
    equation: &mut Equation,
    scope: ScopeId,
) {
    match equation {
        Equation::Simple { lhs, rhs } => {
            walk_expression(callbacks, lhs, scope);
            walk_expression(callbacks, rhs, scope);
        }
        Equation::Connect { lhs, rhs } => {
            callbacks.on_component_reference(lhs, scope);
            callbacks.on_component_reference(rhs, scope);
        }
        Equation::For { indices, equations } => {
            // For-equation scoping, MLS 3.7 §11.2.2.3 with §8.3.2: nested
            // loops, so index N's range is walked in the loop scope before
            // index N binds, with indices 1..N-1 already bound - the
            // dependent form `for i in 1:3, j in 1:i` resolves `i` in `j`'s
            // range to the earlier iterator, while an index's own name in
            // its own range resolves to the enclosing binding. The loop
            // schedule owns that interleaving.
            let loop_scope = callbacks.create_loop_scope(scope);
            ast::schedule_loop_iterators_mut(indices, |step| match step {
                ast::IteratorStep::Range(range) => walk_expression(callbacks, range, loop_scope),
                ast::IteratorStep::Bind(index) => {
                    callbacks.bind_loop_index_name(loop_scope, index.ident.text.as_ref());
                }
            });
            walk_equations(callbacks, equations, loop_scope);
        }
        Equation::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                walk_expression(callbacks, &mut block.cond, scope);
                walk_equations(callbacks, &mut block.eqs, scope);
            }
            if let Some(else_equations) = else_block {
                walk_equations(callbacks, else_equations, scope);
            }
        }
        Equation::When(blocks) => {
            for block in blocks {
                walk_expression(callbacks, &mut block.cond, scope);
                walk_equations(callbacks, &mut block.eqs, scope);
            }
        }
        Equation::FunctionCall { comp, args, .. } => {
            callbacks.on_function_reference(comp, scope);
            walk_expressions(callbacks, args, scope);
        }
        Equation::Assert {
            condition,
            message,
            level,
        } => {
            walk_expression(callbacks, condition, scope);
            walk_expression(callbacks, message, scope);
            if let Some(level_expr) = level {
                walk_expression(callbacks, level_expr, scope);
            }
        }
        Equation::Empty => {}
    }
}

pub(crate) fn walk_statements<C: ResolveTraversalCallbacks>(
    callbacks: &mut C,
    statements: &mut [Statement],
    scope: ScopeId,
) {
    for statement in statements {
        walk_statement(callbacks, statement, scope);
    }
}

pub(crate) fn walk_statement<C: ResolveTraversalCallbacks>(
    callbacks: &mut C,
    statement: &mut Statement,
    scope: ScopeId,
) {
    match statement {
        Statement::Assignment { comp, value } => {
            callbacks.on_component_reference(comp, scope);
            walk_expression(callbacks, value, scope);
        }
        Statement::FunctionCall {
            comp,
            args,
            outputs,
        } => {
            callbacks.on_function_reference(comp, scope);
            walk_expressions(callbacks, args, scope);
            walk_expressions(callbacks, outputs, scope);
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                walk_expression(callbacks, &mut block.cond, scope);
                walk_statements(callbacks, &mut block.stmts, scope);
            }
            if let Some(else_statements) = else_block {
                walk_statements(callbacks, else_statements, scope);
            }
        }
        Statement::For { indices, equations } => {
            // For-statement scoping, MLS 3.7 §11.2.2.3 with §8.3.2: same
            // nested-loop rule as for-equations - range N before bind N,
            // indices 1..N-1 visible to range N.
            let loop_scope = callbacks.create_loop_scope(scope);
            ast::schedule_loop_iterators_mut(indices, |step| match step {
                ast::IteratorStep::Range(range) => walk_expression(callbacks, range, loop_scope),
                ast::IteratorStep::Bind(index) => {
                    callbacks.bind_loop_index_name(loop_scope, index.ident.text.as_ref());
                }
            });
            walk_statements(callbacks, equations, loop_scope);
        }
        Statement::While(block) => {
            walk_expression(callbacks, &mut block.cond, scope);
            walk_statements(callbacks, &mut block.stmts, scope);
        }
        Statement::When(blocks) => {
            for block in blocks {
                walk_expression(callbacks, &mut block.cond, scope);
                walk_statements(callbacks, &mut block.stmts, scope);
            }
        }
        Statement::Reinit { variable, value } => {
            callbacks.on_component_reference(variable, scope);
            walk_expression(callbacks, value, scope);
        }
        Statement::Assert {
            condition,
            message,
            level,
        } => {
            walk_expression(callbacks, condition, scope);
            walk_expression(callbacks, message, scope);
            if let Some(level_expr) = level {
                walk_expression(callbacks, level_expr, scope);
            }
        }
        Statement::Return { .. } | Statement::Break { .. } | Statement::Empty => {}
    }
}

pub(crate) fn walk_expressions<C: ResolveTraversalCallbacks>(
    callbacks: &mut C,
    expressions: &mut [Expression],
    scope: ScopeId,
) {
    for expression in expressions {
        walk_expression(callbacks, expression, scope);
    }
}

pub(crate) fn walk_expression<C: ResolveTraversalCallbacks>(
    callbacks: &mut C,
    expression: &mut Expression,
    scope: ScopeId,
) {
    match expression {
        Expression::ComponentReference(comp) => callbacks.on_component_reference(comp, scope),
        Expression::FunctionCall { comp, args, .. } => {
            callbacks.on_function_reference(comp, scope);
            walk_expressions(callbacks, args, scope);
        }
        Expression::DerivativeCall { args, .. } => walk_expressions(callbacks, args, scope),
        Expression::Binary { lhs, rhs, .. } => {
            walk_expression(callbacks, Arc::make_mut(lhs), scope);
            walk_expression(callbacks, Arc::make_mut(rhs), scope);
        }
        Expression::Unary { rhs, .. } => walk_expression(callbacks, Arc::make_mut(rhs), scope),
        Expression::Range {
            start, step, end, ..
        } => {
            walk_expression(callbacks, Arc::make_mut(start), scope);
            if let Some(step_expression) = step {
                walk_expression(callbacks, Arc::make_mut(step_expression), scope);
            }
            walk_expression(callbacks, Arc::make_mut(end), scope);
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements, .. } => {
            walk_expressions(callbacks, elements, scope);
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (condition, then_expr) in branches {
                walk_expression(callbacks, condition, scope);
                walk_expression(callbacks, then_expr, scope);
            }
            walk_expression(callbacks, Arc::make_mut(else_branch), scope);
        }
        Expression::ClassModification {
            target,
            modifications,
            ..
        } => {
            // The target of a hierarchical class modification (`limiter(u(...))`)
            // names a member of the modified instance, not a function or a
            // lexical declaration. Instantiate owns its exact lookup after
            // redeclarations have selected the concrete receiver type.
            callbacks.on_modification_target(target, scope);
            walk_expressions(callbacks, modifications, scope);
        }
        Expression::Modification { target, value, .. } => {
            callbacks.on_modification_target(target, scope);
            if let Some(value) = value {
                walk_expression(callbacks, Arc::make_mut(value), scope);
            }
        }
        Expression::NamedArgument { value, .. } => {
            walk_expression(callbacks, Arc::make_mut(value), scope);
        }
        Expression::Parenthesized { inner, .. } => {
            walk_expression(callbacks, Arc::make_mut(inner), scope);
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            // Constructor/reduction scoping: MLS 3.7 §10.4.1.2 expands
            // `{e for i in ri, j in rj}` to `{{e for i in ri} for j in rj}`
            // - the LAST textual iterator binds outermost - and §10.4.1's
            // "scope immediately enclosing the array constructor" applies
            // to the EXPANDED form, so `rj` resolves in the parent scope
            // while `ri` resolves where `j` is already bound. The
            // comprehension schedule walks and binds in reverse textual
            // order; walking each range in `loop_scope` exposes exactly the
            // later iterators already bound and falls through to `scope`
            // otherwise. Applying the flat, unexpanded reading of §10.4.1
            // here (no iterator visible in any range) is the backwards bug;
            // see the schedule's derivation before changing this.
            let loop_scope = callbacks.create_loop_scope(scope);
            ast::schedule_comprehension_iterators_mut(indices, |step| match step {
                ast::IteratorStep::Range(range) => walk_expression(callbacks, range, loop_scope),
                ast::IteratorStep::Bind(index) => {
                    callbacks.bind_loop_index_name(loop_scope, index.ident.text.as_ref());
                }
            });
            walk_expression(callbacks, Arc::make_mut(expr), loop_scope);
            if let Some(filter_expr) = filter {
                walk_expression(callbacks, Arc::make_mut(filter_expr), loop_scope);
            }
        }
        Expression::ArrayIndex {
            base, subscripts, ..
        } => {
            walk_expression(callbacks, Arc::make_mut(base), scope);
            walk_subscripts(callbacks, subscripts, scope);
        }
        Expression::FieldAccess {
            base,
            field,
            field_def_id,
            span,
        } => {
            walk_expression(callbacks, Arc::make_mut(base), scope);
            callbacks.on_field_access(base, field, field_def_id, *span, scope);
        }
        Expression::Terminal { .. } | Expression::Empty { .. } => {}
    }
}

pub(crate) fn walk_subscripts<C: ResolveTraversalCallbacks>(
    callbacks: &mut C,
    subscripts: &mut [ast::Subscript],
    scope: ScopeId,
) {
    for subscript in subscripts {
        if let ast::Subscript::Expression(expression) = subscript {
            walk_expression(callbacks, expression, scope);
        }
    }
}

/// Scope-schedule witnesses for both construct families, both dependency
/// directions. The mock records, for every reference the walker dispatches,
/// the names bound so far - so a reversed or flattened schedule changes the
/// event vector, and a reference resolved under the wrong binder set is
/// visible directly.
#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, PartialEq, Eq)]
    enum Event {
        Scope(u32),
        Bind(u32, String),
        Ref(String, u32, Vec<String>),
    }

    struct RecordingCallbacks {
        events: Vec<Event>,
        bound: Vec<String>,
        next_scope: u32,
    }

    impl RecordingCallbacks {
        fn new() -> Self {
            Self {
                events: Vec::new(),
                bound: Vec::new(),
                next_scope: 100,
            }
        }
    }

    fn ref_spelling(comp: &ComponentReference) -> String {
        comp.parts
            .iter()
            .map(|part| part.ident.text.as_ref())
            .collect::<Vec<_>>()
            .join(".")
    }

    impl ResolveTraversalCallbacks for RecordingCallbacks {
        fn create_loop_scope(&mut self, _enclosing: ScopeId) -> ScopeId {
            self.next_scope += 1;
            self.events.push(Event::Scope(self.next_scope));
            ScopeId(self.next_scope)
        }

        fn bind_loop_index_name(&mut self, loop_scope: ScopeId, index_name: &str) {
            self.bound.push(index_name.to_string());
            self.events
                .push(Event::Bind(loop_scope.0, index_name.to_string()));
        }

        fn on_component_reference(&mut self, comp: &mut ComponentReference, scope: ScopeId) {
            self.events
                .push(Event::Ref(ref_spelling(comp), scope.0, self.bound.clone()));
        }

        fn on_function_reference(&mut self, comp: &mut ComponentReference, scope: ScopeId) {
            self.events
                .push(Event::Ref(ref_spelling(comp), scope.0, self.bound.clone()));
        }

        fn on_modification_target(&mut self, _comp: &mut ComponentReference, _scope: ScopeId) {}

        fn on_field_access(
            &mut self,
            _base: &Expression,
            _field: &str,
            _field_def_id: &mut Option<rumoca_core::DefId>,
            _span: rumoca_core::Span,
            _scope: ScopeId,
        ) {
        }
    }

    fn tok(text: &str) -> rumoca_core::Token {
        rumoca_core::Token {
            text: Arc::from(text),
            ..rumoca_core::Token::default()
        }
    }

    fn cref(name: &str) -> ComponentReference {
        ComponentReference {
            local: false,
            parts: vec![ast::ComponentRefPart {
                ident: tok(name),
                subs: None,
                def_id: None,
            }],
            span: rumoca_core::Span::DUMMY,
            qualified_display_name: None,
        }
    }

    fn cref_expr(name: &str) -> Expression {
        Expression::ComponentReference(cref(name))
    }

    fn int_one() -> Expression {
        Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token: tok("1"),
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn range_to(name: &str) -> Expression {
        Expression::Range {
            start: Arc::new(int_one()),
            step: None,
            end: Arc::new(cref_expr(name)),
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn fi(name: &str, range: Expression) -> ast::ForIndex {
        ast::ForIndex {
            ident: tok(name),
            range,
        }
    }

    fn bound(names: &[&str]) -> Vec<String> {
        names.iter().map(|name| (*name).to_string()).collect()
    }

    /// For-equations, MLS 3.7 §11.2.2.3 + §8.3.2, first-textual-outer:
    /// in `for i in 1:j, j in 1:i loop`, `i`'s range is walked with nothing
    /// bound (its `j` is NOT the loop's `j` - the unlicensed direction),
    /// and `j`'s range is walked with `i` bound (the licensed dependent
    /// form `for i in 1:n, j in 1:i`). A reversed schedule swaps the two
    /// bound-set snapshots; a bind-before-range schedule shows `i` bound in
    /// its own range's snapshot. Full-vector equality catches all of them.
    #[test]
    fn for_equation_ranges_bind_first_textual_outer() {
        let mut equation = Equation::For {
            indices: vec![fi("i", range_to("j")), fi("j", range_to("i"))],
            equations: vec![Equation::Simple {
                lhs: cref_expr("x"),
                rhs: int_one(),
            }],
        };
        let mut callbacks = RecordingCallbacks::new();
        walk_equation(&mut callbacks, &mut equation, ScopeId::GLOBAL);
        assert_eq!(
            callbacks.events,
            vec![
                Event::Scope(101),
                Event::Ref("j".into(), 101, bound(&[])),
                Event::Bind(101, "i".into()),
                Event::Ref("i".into(), 101, bound(&["i"])),
                Event::Bind(101, "j".into()),
                Event::Ref("x".into(), 101, bound(&["i", "j"])),
            ],
        );
    }

    /// The for-loop outer-shadow analogue of `{j for j in 1:j}`:
    /// `for j in 1:j loop` walks the range with nothing bound, so the range
    /// `j` takes the enclosing binding, and the body sees the loop `j`.
    #[test]
    fn for_statement_own_range_never_sees_its_own_iterator() {
        let mut statement = Statement::For {
            indices: vec![fi("j", range_to("j"))],
            equations: vec![Statement::Assignment {
                comp: cref("y"),
                value: cref_expr("j"),
            }],
        };
        let mut callbacks = RecordingCallbacks::new();
        walk_statement(&mut callbacks, &mut statement, ScopeId::GLOBAL);
        assert_eq!(
            callbacks.events,
            vec![
                Event::Scope(101),
                Event::Ref("j".into(), 101, bound(&[])),
                Event::Bind(101, "j".into()),
                Event::Ref("y".into(), 101, bound(&["j"])),
                Event::Ref("j".into(), 101, bound(&["j"])),
            ],
        );
    }

    /// Array constructors, MLS 3.7 §10.4.1.2 reverse textual nesting: in
    /// `{j for i in 1:j, j in 1:m}` the LAST iterator `j` binds outermost,
    /// so `j`'s own range (`m` ref) is walked with nothing bound, and the
    /// FIRST iterator's range is walked with `j` bound - its `j` IS the
    /// constructor's iterator, the licensed direction. The body sees both.
    /// A forward (for-loop) schedule and the flat no-iterator-visible
    /// misreading of §10.4.1 both produce different vectors.
    #[test]
    fn comprehension_ranges_bind_last_textual_outer() {
        let mut expression = Expression::ArrayComprehension {
            expr: Arc::new(cref_expr("j")),
            indices: vec![fi("i", range_to("j")), fi("j", range_to("m"))],
            filter: None,
            span: rumoca_core::Span::DUMMY,
        };
        let mut callbacks = RecordingCallbacks::new();
        walk_expression(&mut callbacks, &mut expression, ScopeId::GLOBAL);
        assert_eq!(
            callbacks.events,
            vec![
                Event::Scope(101),
                Event::Ref("m".into(), 101, bound(&[])),
                Event::Bind(101, "j".into()),
                Event::Ref("j".into(), 101, bound(&["j"])),
                Event::Bind(101, "i".into()),
                Event::Ref("j".into(), 101, bound(&["j", "i"])),
            ],
        );
    }

    /// The single-iterator constructor `{j for j in 1:j}` at the resolve
    /// adapter: the range `j` is walked with nothing bound (outer binding),
    /// the body `j` with the iterator bound.
    #[test]
    fn comprehension_own_range_never_sees_its_own_iterator() {
        let mut expression = Expression::ArrayComprehension {
            expr: Arc::new(cref_expr("j")),
            indices: vec![fi("j", range_to("j"))],
            filter: None,
            span: rumoca_core::Span::DUMMY,
        };
        let mut callbacks = RecordingCallbacks::new();
        walk_expression(&mut callbacks, &mut expression, ScopeId::GLOBAL);
        assert_eq!(
            callbacks.events,
            vec![
                Event::Scope(101),
                Event::Ref("j".into(), 101, bound(&[])),
                Event::Bind(101, "j".into()),
                Event::Ref("j".into(), 101, bound(&["j"])),
            ],
        );
    }
}
