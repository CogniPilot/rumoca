use super::Visitor;
use crate::{
    ComponentReference, Equation, Expression, ExpressionContext, Statement, Subscript,
    TerminalType, walk_equation_default, walk_expression_default, walk_statement_default,
};
use rumoca_core::{ComponentPath, OpBinary, OpUnary, Span};
use std::ops::ControlFlow::{self, Break, Continue};

/// Check if an expression contains any component references matching a predicate.
pub fn contains_component_ref<F>(expr: &Expression, predicate: F) -> bool
where
    F: Fn(&ComponentReference) -> bool,
{
    struct Finder<'a, F> {
        predicate: &'a F,
        found: bool,
    }

    impl<F: Fn(&ComponentReference) -> bool> Visitor for Finder<'_, F> {
        fn visit_component_reference(&mut self, cr: &ComponentReference) -> ControlFlow<()> {
            if (self.predicate)(cr) {
                self.found = true;
                return Break(());
            }
            Continue(())
        }
    }

    let mut finder = Finder {
        predicate: &predicate,
        found: false,
    };
    let _visit_outcome = finder.visit_expression(expr);
    finder.found
}

/// Check if an expression contains a function call matching a predicate.
pub fn contains_function_call<F>(expr: &Expression, predicate: F) -> bool
where
    F: Fn(&ComponentReference, &[Expression]) -> bool,
{
    struct Finder<'a, F> {
        predicate: &'a F,
        found: bool,
    }

    impl<F: Fn(&ComponentReference, &[Expression]) -> bool> Visitor for Finder<'_, F> {
        fn visit_expr_function_call(
            &mut self,
            comp: &ComponentReference,
            args: &[Expression],
        ) -> ControlFlow<()> {
            if (self.predicate)(comp, args) {
                self.found = true;
                return Break(());
            }
            self.visit_each(args, Self::visit_expression)
        }
    }

    let mut finder = Finder {
        predicate: &predicate,
        found: false,
    };
    let _visit_outcome = finder.visit_expression(expr);
    finder.found
}

/// Helper struct for collecting component references.
struct ComponentRefCollector {
    refs: Vec<ComponentReference>,
}

impl ComponentRefCollector {
    fn new() -> Self {
        Self { refs: Vec::new() }
    }

    fn walk_subscripts(&mut self, cr: &ComponentReference) -> ControlFlow<()> {
        for part in &cr.parts {
            let Some(subs) = &part.subs else { continue };
            self.visit_each(subs, Self::visit_subscript)?;
        }
        Continue(())
    }
}

impl Visitor for ComponentRefCollector {
    fn visit_component_reference(&mut self, cr: &ComponentReference) -> ControlFlow<()> {
        self.refs.push(cr.clone());
        self.walk_subscripts(cr)
    }
}

/// Collect all component references in an expression.
pub fn collect_component_refs(expr: &Expression) -> Vec<ComponentReference> {
    let mut collector = ComponentRefCollector::new();
    let _visit_outcome = collector.visit_expression(expr);
    collector.refs
}

/// Return the structured component path denoted by a path-shaped expression.
///
/// Instantiation can represent a projected reference as `FieldAccess` and
/// `ArrayIndex` nodes rather than a single `ComponentReference`. Keeping this
/// conversion in the AST crate gives evaluators and semantic phases one
/// definition of the path spelling without re-parsing rendered expressions.
pub fn expression_component_path(expr: &Expression) -> Option<ComponentPath> {
    match expr {
        Expression::ComponentReference(reference) if !reference.parts.is_empty() => Some(
            ComponentPath::from_parts(reference.parts.iter().map(ToString::to_string)),
        ),
        Expression::Parenthesized { inner, .. } => expression_component_path(inner),
        Expression::FieldAccess { base, field, .. } => {
            let base = expression_component_path(base)?;
            Some(base.join(&ComponentPath::from_parts([field.clone()])))
        }
        Expression::ArrayIndex {
            base, subscripts, ..
        } => {
            let base = expression_component_path(base)?;
            let mut parts = base.into_parts();
            let last = parts.last_mut()?;
            let subscripts = subscripts
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(",");
            last.push('[');
            last.push_str(&subscripts);
            last.push(']');
            Some(ComponentPath::from_parts(parts))
        }
        _ => None,
    }
}

#[derive(Default)]
struct RequiredRecoveryFinder {
    found: bool,
}

impl Visitor for RequiredRecoveryFinder {
    fn visit_expression(&mut self, expression: &Expression) -> ControlFlow<()> {
        if matches!(expression, Expression::Empty { .. })
            || matches!(
                expression,
                Expression::Unary {
                    op: OpUnary::Empty,
                    ..
                }
            )
            || matches!(
                expression,
                Expression::Binary {
                    op: OpBinary::Empty,
                    ..
                }
            )
            || matches!(
                expression,
                Expression::Terminal {
                    terminal_type: crate::TerminalType::Empty,
                    ..
                }
            )
        {
            self.found = true;
            return Break(());
        }
        walk_expression_default(self, expression)
    }

    fn visit_subscript(&mut self, subscript: &Subscript) -> ControlFlow<()> {
        if matches!(subscript, Subscript::Empty) {
            self.found = true;
            return Break(());
        }
        match subscript {
            Subscript::Expression(expression) => self.visit_expression(expression),
            Subscript::Range { .. } => Continue(()),
            Subscript::Empty => unreachable!("handled above"),
        }
    }

    fn visit_expression_ctx(
        &mut self,
        expression: &Expression,
        context: ExpressionContext,
    ) -> ControlFlow<()> {
        if matches!(context, ExpressionContext::StatementFunctionOutput)
            && matches!(expression, Expression::Empty { .. })
        {
            return Continue(());
        }
        self.visit_expression(expression)
    }

    fn visit_equation(&mut self, equation: &Equation) -> ControlFlow<()> {
        if matches!(equation, Equation::Empty) {
            self.found = true;
            return Break(());
        }
        walk_equation_default(self, equation)
    }

    fn visit_simple_equation(&mut self, lhs: &Expression, rhs: &Expression) -> ControlFlow<()> {
        if is_invocation_tuple_equation(lhs, rhs) {
            self.visit_tuple_receiver(lhs)?;
        } else {
            self.visit_expression(lhs)?;
        }
        self.visit_expression(rhs)
    }

    fn visit_statement(&mut self, statement: &Statement) -> ControlFlow<()> {
        if matches!(statement, Statement::Empty) {
            self.found = true;
            return Break(());
        }
        walk_statement_default(self, statement)
    }
}

impl RequiredRecoveryFinder {
    fn visit_tuple_receiver(&mut self, expression: &Expression) -> ControlFlow<()> {
        match expression {
            Expression::Parenthesized { inner, .. } => self.visit_tuple_receiver(inner),
            Expression::Tuple { elements, .. } => visit_required_tuple_elements(self, elements),
            _ => self.visit_expression(expression),
        }
    }
}

fn visit_required_tuple_elements(
    visitor: &mut impl Visitor,
    elements: &[Expression],
) -> ControlFlow<()> {
    for element in elements
        .iter()
        .filter(|element| !matches!(element, Expression::Empty { .. }))
    {
        visitor.visit_expression(element)?;
    }
    Continue(())
}

/// Whether an equation is the source-language multi-result invocation form.
///
/// Only direct empty tuple elements in this exact context denote omitted
/// receivers. Parentheses do not change that semantic context.
pub fn is_invocation_tuple_equation(lhs: &Expression, rhs: &Expression) -> bool {
    is_tuple_receiver(lhs)
        && matches!(
            rhs,
            Expression::FunctionCall {
                is_partial_application: false,
                ..
            }
        )
}

fn is_tuple_receiver(expression: &Expression) -> bool {
    match expression {
        Expression::Tuple { .. } => true,
        Expression::Parenthesized { inner, .. } => is_tuple_receiver(inner),
        _ => false,
    }
}

/// Whether a required expression position contains parser-recovery syntax.
pub fn expression_contains_required_recovery(expression: &Expression) -> bool {
    let mut finder = RequiredRecoveryFinder::default();
    let _visit_outcome = finder.visit_expression(expression);
    finder.found
}

/// Whether an equation contains recovery syntax outside legal omitted receivers.
pub fn equation_contains_required_recovery(equation: &Equation) -> bool {
    let mut finder = RequiredRecoveryFinder::default();
    let _visit_outcome = finder.visit_equation(equation);
    finder.found
}

/// Whether a statement contains recovery syntax outside legal omitted outputs.
pub fn statement_contains_required_recovery(statement: &Statement) -> bool {
    let mut finder = RequiredRecoveryFinder::default();
    let _visit_outcome = finder.visit_statement(statement);
    finder.found
}

/// A syntax shape that cannot denote an executable value in its current context.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RequiredValueViolationKind {
    /// A parser-recovery placeholder survived into a required value.
    ParserRecovery(&'static str),
    /// The modification assignment operator appeared as a value operator.
    AssignmentOperator,
    /// The context-only `end` terminal appeared outside an array subscript.
    EndOutsideSubscript,
    /// A named call argument appeared outside a function-call argument list.
    NamedArgumentOutsideCall,
    /// A component modification appeared outside a modification carrier.
    ModificationOutsideCarrier,
}

impl RequiredValueViolationKind {
    /// Stable diagnostic text for the invalid syntax shape.
    pub fn description(self) -> &'static str {
        match self {
            Self::ParserRecovery(description) => description,
            Self::AssignmentOperator => {
                "OpBinary::Assign is a component-modification carrier, not a value operator"
            }
            Self::EndOutsideSubscript => {
                "TerminalType::End is only meaningful inside an array subscript"
            }
            Self::NamedArgumentOutsideCall => {
                "Expression::NamedArgument is only meaningful as a function-call argument"
            }
            Self::ModificationOutsideCarrier => {
                "Expression::Modification is only meaningful as a modifier or named-call carrier"
            }
        }
    }
}

/// The first invalid syntax shape in a required value and its span, when the
/// AST node owns one.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RequiredValueViolation {
    pub kind: RequiredValueViolationKind,
    pub span: Option<Span>,
}

#[derive(Default)]
struct RequiredValueFinder {
    violation: Option<RequiredValueViolation>,
    owner_span: Option<Span>,
    subscript_depth: usize,
}

impl RequiredValueFinder {
    fn reject(&mut self, kind: RequiredValueViolationKind, span: Option<Span>) -> ControlFlow<()> {
        self.violation = Some(RequiredValueViolation { kind, span });
        Break(())
    }

    fn visit_modifier_carrier(&mut self, expression: &Expression) -> ControlFlow<()> {
        let previous_owner = self.owner_span.replace(expression.span());
        let result = match expression {
            Expression::Binary {
                op: OpBinary::Assign,
                lhs,
                rhs,
                ..
            } if matches!(lhs.as_ref(), Expression::ClassModification { .. }) => {
                self.visit_class_modification(lhs)?;
                self.visit_expression(rhs)
            }
            Expression::ClassModification { .. } => self.visit_class_modification(expression),
            Expression::Modification { target, value, .. } => {
                self.visit_component_reference(target)?;
                match value {
                    Some(value) => self.visit_expression(value),
                    None => ControlFlow::Continue(()),
                }
            }
            _ => self.visit_expression(expression),
        };
        self.owner_span = previous_owner;
        result
    }

    fn visit_class_modification(&mut self, expression: &Expression) -> ControlFlow<()> {
        let Expression::ClassModification {
            target,
            modifications,
            ..
        } = expression
        else {
            return self.visit_expression(expression);
        };
        let previous_owner = self.owner_span.replace(expression.span());
        let result = (|| {
            self.visit_component_reference(target)?;
            for modification in modifications {
                self.visit_modifier_carrier(modification)?;
            }
            Continue(())
        })();
        self.owner_span = previous_owner;
        result
    }

    fn visit_tuple_receiver(&mut self, expression: &Expression) -> ControlFlow<()> {
        match expression {
            Expression::Parenthesized { inner, .. } => self.visit_tuple_receiver(inner),
            Expression::Tuple { elements, .. } => visit_required_tuple_elements(self, elements),
            _ => self.visit_expression(expression),
        }
    }

    fn visit_call_argument(&mut self, expression: &Expression) -> ControlFlow<()> {
        match expression {
            Expression::NamedArgument { value, .. } => self.visit_expression(value),
            Expression::Modification { .. } => self.visit_modifier_carrier(expression),
            _ => self.visit_expression(expression),
        }
    }
}

impl Visitor for RequiredValueFinder {
    fn visit_expression(&mut self, expression: &Expression) -> ControlFlow<()> {
        let previous_owner = self.owner_span.replace(expression.span());
        let invalid = match expression {
            Expression::Empty { .. } => Some(RequiredValueViolationKind::ParserRecovery(
                "Expression::Empty is a parser-recovery node",
            )),
            Expression::Unary {
                op: OpUnary::Empty, ..
            } => Some(RequiredValueViolationKind::ParserRecovery(
                "OpUnary::Empty is a parser-recovery operator",
            )),
            Expression::Binary {
                op: OpBinary::Empty,
                ..
            } => Some(RequiredValueViolationKind::ParserRecovery(
                "OpBinary::Empty is a parser-recovery operator",
            )),
            Expression::Terminal {
                terminal_type: TerminalType::Empty,
                ..
            } => Some(RequiredValueViolationKind::ParserRecovery(
                "TerminalType::Empty is a parser-recovery terminal",
            )),
            Expression::Binary {
                op: OpBinary::Assign,
                ..
            } => Some(RequiredValueViolationKind::AssignmentOperator),
            Expression::Terminal {
                terminal_type: TerminalType::End,
                ..
            } if self.subscript_depth == 0 => Some(RequiredValueViolationKind::EndOutsideSubscript),
            Expression::NamedArgument { .. } => {
                Some(RequiredValueViolationKind::NamedArgumentOutsideCall)
            }
            Expression::Modification { .. } => {
                Some(RequiredValueViolationKind::ModificationOutsideCarrier)
            }
            _ => None,
        };
        let result = if let Some(kind) = invalid {
            self.reject(kind, Some(expression.span()))
        } else if matches!(expression, Expression::ClassModification { .. }) {
            self.visit_class_modification(expression)
        } else {
            walk_expression_default(self, expression)
        };
        self.owner_span = previous_owner;
        result
    }

    fn visit_subscript(&mut self, subscript: &Subscript) -> ControlFlow<()> {
        match subscript {
            Subscript::Empty => self.reject(
                RequiredValueViolationKind::ParserRecovery(
                    "Subscript::Empty is a parser-recovery node",
                ),
                self.owner_span,
            ),
            Subscript::Range { .. } => Continue(()),
            Subscript::Expression(expression) => {
                self.subscript_depth += 1;
                let result = self.visit_expression(expression);
                self.subscript_depth -= 1;
                result
            }
        }
    }

    fn visit_expr_function_call(
        &mut self,
        _component: &ComponentReference,
        arguments: &[Expression],
    ) -> ControlFlow<()> {
        for argument in arguments {
            self.visit_call_argument(argument)?;
        }
        Continue(())
    }

    fn visit_expression_ctx(
        &mut self,
        expression: &Expression,
        context: ExpressionContext,
    ) -> ControlFlow<()> {
        if matches!(context, ExpressionContext::StatementFunctionOutput)
            && matches!(expression, Expression::Empty { .. })
        {
            return Continue(());
        }
        if matches!(
            context,
            ExpressionContext::ComponentModification
                | ExpressionContext::ExtendModification
                | ExpressionContext::ComponentAnnotation
                | ExpressionContext::ClassAnnotation
                | ExpressionContext::ExtendAnnotation
                | ExpressionContext::ExternalAnnotation
        ) {
            return self.visit_modifier_carrier(expression);
        }
        self.visit_expression(expression)
    }

    fn visit_equation(&mut self, equation: &Equation) -> ControlFlow<()> {
        if matches!(equation, Equation::Empty) {
            return self.reject(
                RequiredValueViolationKind::ParserRecovery(
                    "Equation::Empty is a parser-recovery node",
                ),
                self.owner_span,
            );
        }
        walk_equation_default(self, equation)
    }

    fn visit_simple_equation(&mut self, lhs: &Expression, rhs: &Expression) -> ControlFlow<()> {
        if is_invocation_tuple_equation(lhs, rhs) {
            self.visit_tuple_receiver(lhs)?;
        } else {
            self.visit_expression(lhs)?;
        }
        self.visit_expression(rhs)
    }

    fn visit_statement(&mut self, statement: &Statement) -> ControlFlow<()> {
        if matches!(statement, Statement::Empty) {
            return self.reject(
                RequiredValueViolationKind::ParserRecovery(
                    "Statement::Empty is a parser-recovery node",
                ),
                self.owner_span,
            );
        }
        walk_statement_default(self, statement)
    }
}

/// Return the first syntax shape that cannot denote a required value.
pub fn expression_required_value_violation(
    expression: &Expression,
) -> Option<RequiredValueViolation> {
    let mut finder = RequiredValueFinder::default();
    let _visit_outcome = finder.visit_expression(expression);
    finder.violation
}

/// Return the first invalid required-value shape in an equation.
pub fn equation_required_value_violation(equation: &Equation) -> Option<RequiredValueViolation> {
    let mut finder = RequiredValueFinder::default();
    let _visit_outcome = finder.visit_equation(equation);
    finder.violation
}

/// Return the first invalid required-value shape in a statement.
pub fn statement_required_value_violation(statement: &Statement) -> Option<RequiredValueViolation> {
    let mut finder = RequiredValueFinder::default();
    let _visit_outcome = finder.visit_statement(statement);
    finder.violation
}

/// Return the first invalid shape in a subscript, preserving `end` as legal
/// context-only syntax inside the subscript expression.
pub fn subscript_required_value_violation(subscript: &Subscript) -> Option<RequiredValueViolation> {
    let mut finder = RequiredValueFinder::default();
    let _visit_outcome = finder.visit_subscript(subscript);
    finder.violation
}

/// Return the first invalid shape in a declaration dimension.
///
/// Unlike an array-access selector, the declaration subscript itself does not
/// establish a context in which `end` has an array bound to denote. Array
/// accesses nested inside the dimension expression still establish their own
/// selector context.
pub fn declaration_subscript_required_value_violation(
    subscript: &Subscript,
) -> Option<RequiredValueViolation> {
    let mut finder = RequiredValueFinder::default();
    match subscript {
        Subscript::Expression(expression) => {
            let _visit_outcome = finder.visit_expression(expression);
        }
        Subscript::Range { .. } => {}
        Subscript::Empty => {
            let _visit_outcome = finder.visit_subscript(subscript);
        }
    }
    finder.violation
}

/// Return the first invalid required-value shape inside a component-modifier
/// carrier while accepting the carrier's exact assignment syntax.
pub fn modifier_required_value_violation(
    expression: &Expression,
) -> Option<RequiredValueViolation> {
    let mut finder = RequiredValueFinder::default();
    let _visit_outcome = finder.visit_modifier_carrier(expression);
    finder.violation
}

#[cfg(test)]
mod recovery_tests {
    use super::*;
    use crate::{ComponentRefPart, ComponentReference, ForIndex, StatementBlock, TerminalType};
    use std::sync::Arc;

    fn span() -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("recovery_query.mo"),
            1,
            2,
        )
    }

    fn integer() -> Expression {
        Expression::Terminal {
            terminal_type: TerminalType::UnsignedInteger,
            token: rumoca_core::Token {
                text: Arc::from("1"),
                ..rumoca_core::Token::default()
            },
            span: span(),
        }
    }

    fn invocation() -> Expression {
        Expression::FunctionCall {
            comp: component_reference(),
            args: Vec::new(),
            is_partial_application: false,
            span: span(),
        }
    }

    fn component_reference() -> ComponentReference {
        ComponentReference {
            local: false,
            parts: Vec::new(),
            span: span(),
            qualified_display_name: None,
        }
    }

    #[test]
    fn recovery_query_distinguishes_required_nodes_from_omitted_receivers() {
        let recovery = Expression::Empty { span: span() };
        assert!(expression_contains_required_recovery(&Expression::Unary {
            op: OpUnary::Empty,
            rhs: Arc::new(integer()),
            span: span(),
        }));
        assert!(statement_contains_required_recovery(&Statement::If {
            cond_blocks: vec![StatementBlock {
                cond: recovery.clone(),
                stmts: Vec::new(),
            }],
            else_block: None,
        }));
        assert!(!statement_contains_required_recovery(
            &Statement::FunctionCall {
                comp: component_reference(),
                args: Vec::new(),
                outputs: vec![recovery.clone()],
            }
        ));

        let tuple = Expression::Tuple {
            elements: vec![integer(), recovery],
            span: span(),
        };
        assert!(!equation_contains_required_recovery(&Equation::Simple {
            lhs: tuple.clone(),
            rhs: invocation(),
        }));
        assert!(equation_contains_required_recovery(&Equation::Simple {
            lhs: tuple,
            rhs: integer(),
        }));
    }

    #[test]
    fn recovery_query_covers_every_expression_level_recovery_shape() {
        let terminal = Expression::Terminal {
            terminal_type: TerminalType::Empty,
            token: rumoca_core::Token::default(),
            span: span(),
        };
        let binary = Expression::Binary {
            op: OpBinary::Empty,
            lhs: Arc::new(integer()),
            rhs: Arc::new(integer()),
            span: span(),
        };
        let indexed_reference = Expression::ComponentReference(ComponentReference {
            local: false,
            parts: vec![ComponentRefPart {
                ident: rumoca_core::Token::default(),
                subs: Some(vec![Subscript::Empty]),
                def_id: None,
            }],
            span: span(),
            qualified_display_name: None,
        });
        for expression in [terminal, binary, indexed_reference] {
            assert!(expression_contains_required_recovery(&expression));
        }

        assert!(statement_contains_required_recovery(&Statement::For {
            indices: vec![ForIndex {
                ident: rumoca_core::Token::default(),
                range: Expression::Empty { span: span() },
            }],
            equations: Vec::new(),
        }));
    }

    #[test]
    fn nested_parentheses_preserve_the_omitted_tuple_receiver_context() {
        let tuple = Expression::Tuple {
            elements: vec![Expression::Empty { span: span() }, integer()],
            span: span(),
        };
        let nested = Expression::Parenthesized {
            inner: Arc::new(Expression::Parenthesized {
                inner: Arc::new(tuple),
                span: span(),
            }),
            span: span(),
        };
        assert!(is_invocation_tuple_equation(&nested, &invocation()));
        assert!(!equation_contains_required_recovery(&Equation::Simple {
            lhs: nested,
            rhs: invocation(),
        }));
    }

    #[test]
    fn required_value_query_rejects_context_only_syntax_without_rejecting_its_contexts() {
        required_value_query_assignment_and_end_cases();
        required_value_query_modifier_cases();
        required_value_query_call_cases();
    }

    fn required_value_query_assignment_and_end_cases() {
        let assignment = Expression::Binary {
            op: OpBinary::Assign,
            lhs: Arc::new(integer()),
            rhs: Arc::new(integer()),
            span: span(),
        };
        assert!(matches!(
            expression_required_value_violation(&assignment),
            Some(RequiredValueViolation {
                kind: RequiredValueViolationKind::AssignmentOperator,
                ..
            })
        ));
        let end = Expression::Terminal {
            terminal_type: TerminalType::End,
            token: rumoca_core::Token::default(),
            span: span(),
        };
        assert!(matches!(
            expression_required_value_violation(&end),
            Some(RequiredValueViolation {
                kind: RequiredValueViolationKind::EndOutsideSubscript,
                ..
            })
        ));
        let indexed = Expression::ArrayIndex {
            base: Arc::new(Expression::ComponentReference(component_reference())),
            subscripts: vec![Subscript::Expression(end.clone())],
            span: span(),
        };
        assert_eq!(expression_required_value_violation(&indexed), None);
        assert_eq!(
            subscript_required_value_violation(&Subscript::Expression(end.clone())),
            None
        );
        assert!(matches!(
            declaration_subscript_required_value_violation(&Subscript::Expression(end.clone())),
            Some(RequiredValueViolation {
                kind: RequiredValueViolationKind::EndOutsideSubscript,
                ..
            })
        ));
        assert!(matches!(
            declaration_subscript_required_value_violation(&Subscript::Expression(
                Expression::Parenthesized {
                    inner: Arc::new(end),
                    span: span(),
                }
            )),
            Some(RequiredValueViolation {
                kind: RequiredValueViolationKind::EndOutsideSubscript,
                ..
            })
        ));
        assert_eq!(
            declaration_subscript_required_value_violation(&Subscript::Expression(indexed)),
            None
        );
    }

    fn required_value_query_modifier_cases() {
        let modifier = Expression::ClassModification {
            target: component_reference(),
            modifications: vec![Expression::Binary {
                op: OpBinary::Assign,
                lhs: Arc::new(Expression::ClassModification {
                    target: component_reference(),
                    modifications: Vec::new(),
                    each_flags: Vec::new(),
                    final_flags: Vec::new(),
                    redeclare_flags: Vec::new(),
                    span: span(),
                }),
                rhs: Arc::new(integer()),
                span: span(),
            }],
            each_flags: vec![false],
            final_flags: vec![false],
            redeclare_flags: vec![false],
            span: span(),
        };
        assert_eq!(expression_required_value_violation(&modifier), None);
        assert_eq!(modifier_required_value_violation(&modifier), None);
        let modification = Expression::Modification {
            target: component_reference(),
            value: Some(Arc::new(integer())),
            span: span(),
        };
        assert!(matches!(
            expression_required_value_violation(&modification),
            Some(RequiredValueViolation {
                kind: RequiredValueViolationKind::ModificationOutsideCarrier,
                ..
            })
        ));
        assert_eq!(modifier_required_value_violation(&modification), None);
    }

    fn required_value_query_call_cases() {
        let named = Expression::NamedArgument {
            name: rumoca_core::Token::default(),
            value: Arc::new(integer()),
            span: span(),
        };
        assert!(matches!(
            expression_required_value_violation(&named),
            Some(RequiredValueViolation {
                kind: RequiredValueViolationKind::NamedArgumentOutsideCall,
                ..
            })
        ));
        let modification = Expression::Modification {
            target: component_reference(),
            value: Some(Arc::new(integer())),
            span: span(),
        };
        let call = Expression::FunctionCall {
            comp: component_reference(),
            args: vec![named],
            is_partial_application: false,
            span: span(),
        };
        assert_eq!(expression_required_value_violation(&call), None);
        let call = Expression::FunctionCall {
            comp: component_reference(),
            args: vec![modification],
            is_partial_application: false,
            span: span(),
        };
        assert_eq!(expression_required_value_violation(&call), None);
        let mut malformed_target = component_reference();
        malformed_target.parts = vec![ComponentRefPart {
            ident: rumoca_core::Token::default(),
            subs: Some(vec![Subscript::Empty]),
            def_id: None,
        }];
        let malformed_modifier = Expression::Modification {
            target: malformed_target,
            value: Some(Arc::new(integer())),
            span: span(),
        };
        assert!(matches!(
            modifier_required_value_violation(&malformed_modifier),
            Some(RequiredValueViolation {
                kind: RequiredValueViolationKind::ParserRecovery(_),
                ..
            })
        ));
        let assignment = Expression::Binary {
            op: OpBinary::Assign,
            lhs: Arc::new(integer()),
            rhs: Arc::new(integer()),
            span: span(),
        };
        let nested_assignment = Expression::Modification {
            target: component_reference(),
            value: Some(Arc::new(assignment)),
            span: span(),
        };
        assert!(matches!(
            modifier_required_value_violation(&nested_assignment),
            Some(RequiredValueViolation {
                kind: RequiredValueViolationKind::AssignmentOperator,
                ..
            })
        ));
    }
}
