//! Validation of parser-recovery shapes at the flatten input boundary.

use std::ops::ControlFlow::{self, Break};

use rumoca_ir_ast::{self as ast, ExpressionContext, SubscriptContext, Visitor};

use crate::FlattenError;

struct FlattenInputRecoveryValidator {
    owner_span: Option<rumoca_core::Span>,
    invalid_span: Option<rumoca_core::Span>,
    invalid_node: Option<InvalidRecoveryNode>,
    subscript_depth: usize,
}

#[derive(Clone, Copy)]
enum InvalidRecoveryNode {
    SemanticNode(&'static str),
    LiteralTerminal,
    Subscript,
}

impl FlattenInputRecoveryValidator {
    fn new() -> Self {
        Self {
            owner_span: None,
            invalid_span: None,
            invalid_node: None,
            subscript_depth: 0,
        }
    }

    fn enter_owner(&mut self, candidate: rumoca_core::Span) -> Option<rumoca_core::Span> {
        let previous = self.owner_span;
        if let Ok(provenance) = candidate.require_provenance("flatten-input AST owner") {
            self.owner_span = Some(provenance.span());
        }
        previous
    }

    fn reset_owner(&mut self, candidate: rumoca_core::Span) {
        self.owner_span = candidate
            .require_provenance("flatten-input AST owner")
            .ok()
            .map(|provenance| provenance.span());
    }
    fn reject(&mut self, invalid_node: InvalidRecoveryNode) -> ControlFlow<()> {
        self.invalid_node = Some(invalid_node);
        self.invalid_span = self.owner_span;
        Break(())
    }

    fn visit_tuple_receiver(&mut self, expression: &ast::Expression) -> ControlFlow<()> {
        let previous = self.enter_owner(expression.span());
        let result = match expression {
            ast::Expression::Parenthesized { inner, .. } => self.visit_tuple_receiver(inner),
            ast::Expression::Tuple { elements, .. } => self.visit_tuple_elements(elements),
            _ => self.visit_expression(expression),
        };
        self.owner_span = previous;
        result
    }

    fn visit_tuple_elements(&mut self, elements: &[ast::Expression]) -> ControlFlow<()> {
        for element in elements {
            if !matches!(element, ast::Expression::Empty { .. }) {
                self.visit_expression(element)?;
            }
        }
        ControlFlow::Continue(())
    }

    fn visit_modifier_carrier(&mut self, expression: &ast::Expression) -> ControlFlow<()> {
        match expression {
            ast::Expression::Binary {
                op: rumoca_core::OpBinary::Assign,
                lhs,
                rhs,
                ..
            } if matches!(lhs.as_ref(), ast::Expression::ClassModification { .. }) => {
                self.visit_class_modification(lhs)?;
                self.visit_expression(rhs)
            }
            ast::Expression::ClassModification { .. } => self.visit_class_modification(expression),
            ast::Expression::Modification { target, value, .. } => {
                self.visit_component_reference(target)?;
                // A value-less modification (`annotation(Dialog)`) is legal
                // (MLS §18.2) and has no expression to check. Its absence is
                // typed as `None`, so a present value is always an actual
                // expression and is checked in full: a recovery node there is
                // a parse failure, never an omitted value.
                match value {
                    Some(value) => self.visit_expression(value),
                    None => ControlFlow::Continue(()),
                }
            }
            _ => self.visit_expression(expression),
        }
    }

    fn visit_class_modification(&mut self, expression: &ast::Expression) -> ControlFlow<()> {
        let ast::Expression::ClassModification {
            target,
            modifications,
            ..
        } = expression
        else {
            return self.visit_expression(expression);
        };
        self.visit_component_reference(target)?;
        for modification in modifications {
            self.visit_modifier_carrier(modification)?;
        }
        ControlFlow::Continue(())
    }

    fn visit_call_argument(&mut self, expression: &ast::Expression) -> ControlFlow<()> {
        match expression {
            ast::Expression::NamedArgument { value, .. } => self.visit_expression(value),
            ast::Expression::Modification { .. } => self.visit_modifier_carrier(expression),
            _ => self.visit_expression(expression),
        }
    }

    fn visit_class_override(&mut self, class_override: &ast::ClassOverride) -> ControlFlow<()> {
        if let Some(target) = &class_override.target_ref {
            self.visit_component_reference(target)?;
        }
        for argument in &class_override.modifier_args {
            self.visit_modifier_carrier(argument)?;
        }
        ControlFlow::Continue(())
    }
}

impl Visitor for FlattenInputRecoveryValidator {
    fn visit_class_def(&mut self, class: &ast::ClassDef) -> ControlFlow<()> {
        let previous = self.enter_owner(class.location.span());
        let result = ast::walk_class_def_default(self, class);
        self.owner_span = previous;
        result
    }

    fn visit_component_reference(
        &mut self,
        reference: &ast::ComponentReference,
    ) -> ControlFlow<()> {
        let previous = self.enter_owner(reference.span);
        let result = ast::walk_component_reference_default(self, reference);
        self.owner_span = previous;
        result
    }

    fn visit_expression(&mut self, expression: &ast::Expression) -> ControlFlow<()> {
        let previous = self.enter_owner(expression.span());
        match expression {
            ast::Expression::Empty { .. } => {
                return self.reject(InvalidRecoveryNode::SemanticNode(
                    "Expression::Empty is a parser-recovery node",
                ));
            }
            ast::Expression::Unary {
                op: rumoca_core::OpUnary::Empty,
                ..
            } => {
                return self.reject(InvalidRecoveryNode::SemanticNode(
                    "OpUnary::Empty is a parser-recovery operator",
                ));
            }
            ast::Expression::Binary {
                op: rumoca_core::OpBinary::Empty,
                ..
            } => {
                return self.reject(InvalidRecoveryNode::SemanticNode(
                    "OpBinary::Empty is a parser-recovery operator",
                ));
            }
            ast::Expression::Terminal {
                terminal_type: ast::TerminalType::Empty,
                ..
            } => {
                self.invalid_node = Some(InvalidRecoveryNode::LiteralTerminal);
                self.invalid_span = self.owner_span;
                return Break(());
            }
            ast::Expression::Binary {
                op: rumoca_core::OpBinary::Assign,
                ..
            } => {
                return self.reject(InvalidRecoveryNode::SemanticNode(
                    "OpBinary::Assign is a component-modification carrier, not a value operator",
                ));
            }
            ast::Expression::Terminal {
                terminal_type: ast::TerminalType::End,
                ..
            } if self.subscript_depth == 0 => {
                return self.reject(InvalidRecoveryNode::SemanticNode(
                    "TerminalType::End is only meaningful inside an array subscript",
                ));
            }
            ast::Expression::NamedArgument { .. } => {
                return self.reject(InvalidRecoveryNode::SemanticNode(
                    "Expression::NamedArgument is only meaningful as a function-call argument",
                ));
            }
            ast::Expression::Modification { .. } => {
                return self.reject(InvalidRecoveryNode::SemanticNode(
                    "Expression::Modification is only meaningful as a modifier or named-call carrier",
                ));
            }
            _ => {}
        }
        let result = if matches!(expression, ast::Expression::ClassModification { .. }) {
            self.visit_class_modification(expression)
        } else {
            ast::walk_expression_default(self, expression)
        };
        self.owner_span = previous;
        result
    }

    fn visit_expr_function_call(
        &mut self,
        _component: &ast::ComponentReference,
        arguments: &[ast::Expression],
    ) -> ControlFlow<()> {
        for argument in arguments {
            self.visit_call_argument(argument)?;
        }
        ControlFlow::Continue(())
    }

    fn visit_expression_ctx(
        &mut self,
        expression: &ast::Expression,
        context: ExpressionContext,
    ) -> ControlFlow<()> {
        if matches!(context, ExpressionContext::StatementFunctionOutput)
            && matches!(expression, ast::Expression::Empty { .. })
        {
            return ControlFlow::Continue(());
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

    fn visit_equation(&mut self, equation: &ast::Equation) -> ControlFlow<()> {
        let previous = self.owner_span;
        if let Some(location) = equation.get_location() {
            let _ = self.enter_owner(location.span());
        }
        if matches!(equation, ast::Equation::Empty) {
            return self.reject(InvalidRecoveryNode::SemanticNode(
                "Equation::Empty is a parser-recovery node; an empty equation section is an empty list",
            ));
        }
        let result = ast::walk_equation_default(self, equation);
        self.owner_span = previous;
        result
    }

    fn visit_statement(&mut self, statement: &ast::Statement) -> ControlFlow<()> {
        let previous = self.owner_span;
        if let Some(location) = statement.get_location() {
            let _ = self.enter_owner(location.span());
        }
        if matches!(statement, ast::Statement::Empty) {
            return self.reject(InvalidRecoveryNode::SemanticNode(
                "Statement::Empty is a parser-recovery node; an empty algorithm body is an empty list",
            ));
        }
        let result = ast::walk_statement_default(self, statement);
        self.owner_span = previous;
        result
    }

    fn visit_simple_equation(
        &mut self,
        lhs: &ast::Expression,
        rhs: &ast::Expression,
    ) -> ControlFlow<()> {
        if ast::is_invocation_tuple_equation(lhs, rhs) {
            self.visit_tuple_receiver(lhs)?;
        } else {
            self.visit_expression(lhs)?;
        }
        self.visit_expression(rhs)
    }

    fn visit_component(&mut self, component: &ast::Component) -> ControlFlow<()> {
        let previous = self.enter_owner(component.name_token.location.span());
        if component.has_explicit_binding != component.binding.is_some() {
            return self.reject(InvalidRecoveryNode::SemanticNode(
                "Component explicit-binding marker and binding expression disagree",
            ));
        }
        let result = ast::walk_component_default(self, component);
        self.owner_span = previous;
        result
    }

    fn visit_subscript_ctx(
        &mut self,
        subscript: &ast::Subscript,
        context: SubscriptContext,
    ) -> ControlFlow<()> {
        if matches!(subscript, ast::Subscript::Empty) {
            return self.reject(InvalidRecoveryNode::Subscript);
        }
        let ast::Subscript::Expression(expression) = subscript else {
            return ControlFlow::Continue(());
        };
        if matches!(
            context,
            SubscriptContext::ArrayIndex | SubscriptContext::ComponentReferencePart
        ) {
            self.subscript_depth += 1;
            let result = self.visit_expression(expression);
            self.subscript_depth -= 1;
            result
        } else {
            self.visit_expression(expression)
        }
    }
}

pub(crate) fn validate_flatten_input(
    tree: &ast::ClassTree,
    overlay: &ast::InstanceOverlay,
) -> Result<(), FlattenError> {
    let mut validator = FlattenInputRecoveryValidator::new();
    let _visit_outcome = validator.visit_stored_definition(&tree.definitions);

    'components: for instance in overlay.components.values() {
        if validator.invalid_node.is_some() {
            break;
        }
        validator.reset_owner(instance.source_location.span());
        for subscript in &instance.dims_expr {
            if validator
                .visit_subscript_ctx(subscript, SubscriptContext::ComponentShape)
                .is_break()
            {
                break 'components;
            }
        }
        for expression in [
            instance.start.as_ref(),
            instance.min.as_ref(),
            instance.max.as_ref(),
            instance.nominal.as_ref(),
            instance.binding.as_ref(),
            instance.binding_source.as_ref(),
        ]
        .into_iter()
        .flatten()
        {
            if validator.visit_expression(expression).is_break() {
                break 'components;
            }
        }
        for class_override in instance.class_overrides.values() {
            if validator.visit_class_override(class_override).is_break() {
                break 'components;
            }
        }
    }

    'classes: for instance in overlay.classes.values() {
        if validator.invalid_node.is_some() {
            break;
        }
        for class_override in instance.class_overrides.values() {
            validator.owner_span = None;
            if validator.visit_class_override(class_override).is_break() {
                break 'classes;
            }
        }
        for equation in instance.equations.iter().chain(&instance.initial_equations) {
            validator.reset_owner(equation.span);
            if validator.visit_equation(&equation.equation).is_break() {
                break 'classes;
            }
        }
        for statement in instance
            .algorithms
            .iter()
            .chain(&instance.initial_algorithms)
            .flatten()
        {
            validator.reset_owner(statement.span);
            if validator.visit_statement(&statement.statement).is_break() {
                break 'classes;
            }
        }
    }

    let Some(invalid_node) = validator.invalid_node else {
        return Ok(());
    };
    let span = validator.invalid_span.ok_or_else(|| {
        FlattenError::missing_source_context(
            "flatten-input AST recovery node has no source-provenance owner",
        )
    })?;
    let span = crate::source_spans::required_span(span, "flatten-input AST recovery-node owner")?;
    match invalid_node {
        InvalidRecoveryNode::SemanticNode(description) => {
            Err(FlattenError::invalid_ast_recovery(description, span))
        }
        InvalidRecoveryNode::LiteralTerminal => Err(FlattenError::InvalidLiteralTerminal {
            description: "TerminalType::Empty is a parser-recovery terminal".to_string(),
            span,
        }),
        InvalidRecoveryNode::Subscript => Err(FlattenError::invalid_ast_subscript(
            "Subscript::Empty is a parser-recovery node, not a whole-dimension selector",
            span,
        )),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn span_at(start: usize, end: usize) -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("ast_validation_test.mo"),
            start,
            end,
        )
    }

    fn terminal(value: &str, span: rumoca_core::Span) -> ast::Expression {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token: rumoca_core::Token {
                text: std::sync::Arc::from(value),
                ..Default::default()
            },
            span,
        }
    }

    fn invocation(span: rumoca_core::Span) -> ast::Expression {
        ast::Expression::FunctionCall {
            comp: ast::ComponentReference {
                local: false,
                parts: Vec::new(),
                span,
                qualified_display_name: None,
            },
            args: Vec::new(),
            is_partial_application: false,
            span,
        }
    }

    fn reference(name: &str, span: rumoca_core::Span) -> ast::ComponentReference {
        ast::ComponentReference {
            local: false,
            parts: vec![ast::ComponentRefPart {
                ident: rumoca_core::Token {
                    text: std::sync::Arc::from(name),
                    ..Default::default()
                },
                subs: None,
                def_id: None,
            }],
            span,
            qualified_display_name: None,
        }
    }

    fn overlay_equation(equation: ast::Equation, span: rumoca_core::Span) -> ast::InstanceOverlay {
        let mut overlay = ast::InstanceOverlay::default();
        let instance_id = rumoca_core::InstanceId::new(1);
        overlay.classes.insert(
            instance_id,
            ast::ClassInstanceData {
                instance_id,
                equations: vec![ast::InstanceEquation {
                    equation,
                    origin: ast::QualifiedName::default(),
                    source_scope: None,
                    source_scope_id: None,
                    span,
                }],
                ..Default::default()
            },
        );
        overlay
    }

    fn overlay_statement(
        statement: ast::Statement,
        span: rumoca_core::Span,
    ) -> ast::InstanceOverlay {
        let mut overlay = ast::InstanceOverlay::default();
        let instance_id = rumoca_core::InstanceId::new(1);
        overlay.classes.insert(
            instance_id,
            ast::ClassInstanceData {
                instance_id,
                algorithms: vec![vec![ast::InstanceStatement {
                    statement,
                    origin: ast::QualifiedName::default(),
                    source_scope: None,
                    source_scope_id: None,
                    span,
                }]],
                ..Default::default()
            },
        );
        overlay
    }

    #[test]
    fn empty_instance_dimension_is_rejected_at_its_owner() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("invalid_subscript.mo"),
            12,
            18,
        );
        let id = rumoca_core::InstanceId::new(1);
        let mut overlay = ast::InstanceOverlay::default();
        overlay.components.insert(
            id,
            ast::InstanceData {
                instance_id: id,
                source_location: rumoca_core::Location {
                    source: span.source,
                    start: span.start.0 as u32,
                    end: span.end.0 as u32,
                    ..Default::default()
                },
                dims_expr: vec![ast::Subscript::Empty],
                ..Default::default()
            },
        );

        let error = validate_flatten_input(&ast::ClassTree::new(), &overlay)
            .expect_err("recovery subscript must not cross the flatten boundary");
        assert!(matches!(
            error,
            FlattenError::InvalidAstSubscript { span: actual, .. } if actual == span
        ));
    }

    #[test]
    fn source_range_dimension_is_accepted() {
        let id = rumoca_core::InstanceId::new(1);
        let mut overlay = ast::InstanceOverlay::default();
        overlay.components.insert(
            id,
            ast::InstanceData {
                instance_id: id,
                dims_expr: vec![ast::Subscript::Range {
                    token: rumoca_core::Token::default(),
                }],
                ..Default::default()
            },
        );

        validate_flatten_input(&ast::ClassTree::new(), &overlay)
            .expect("source colon remains a legal dimension selector");
    }

    #[test]
    fn empty_instance_dimension_without_provenance_fails_explicitly() {
        let id = rumoca_core::InstanceId::new(1);
        let mut overlay = ast::InstanceOverlay::default();
        overlay.components.insert(
            id,
            ast::InstanceData {
                instance_id: id,
                dims_expr: vec![ast::Subscript::Empty],
                ..Default::default()
            },
        );

        let error = validate_flatten_input(&ast::ClassTree::new(), &overlay)
            .expect_err("a forged recovery node must not acquire a dummy diagnostic span");
        assert!(matches!(error, FlattenError::MissingSourceContext { .. }));
    }

    #[test]
    fn unrelated_valid_owner_cannot_lend_provenance_to_a_forged_component() {
        let valid_span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("unrelated.mo"),
            1,
            4,
        );
        let mut overlay = ast::InstanceOverlay::default();
        overlay.components.insert(
            rumoca_core::InstanceId::new(1),
            ast::InstanceData {
                instance_id: rumoca_core::InstanceId::new(1),
                source_location: rumoca_core::Location {
                    source: valid_span.source,
                    start: valid_span.start.0 as u32,
                    end: valid_span.end.0 as u32,
                    ..Default::default()
                },
                ..Default::default()
            },
        );
        overlay.components.insert(
            rumoca_core::InstanceId::new(2),
            ast::InstanceData {
                instance_id: rumoca_core::InstanceId::new(2),
                dims_expr: vec![ast::Subscript::Empty],
                ..Default::default()
            },
        );

        let error = validate_flatten_input(&ast::ClassTree::new(), &overlay)
            .expect_err("provenance must never leak between component owners");
        assert!(matches!(error, FlattenError::MissingSourceContext { .. }));
    }

    #[test]
    fn required_recovery_expressions_fail_at_the_exact_expression_span() {
        let expression_span = span_at(20, 24);
        let owner_span = span_at(10, 30);
        let invalid = [
            (
                ast::Expression::Empty {
                    span: expression_span,
                },
                false,
            ),
            (
                ast::Expression::Unary {
                    op: rumoca_core::OpUnary::Empty,
                    rhs: std::sync::Arc::new(terminal("1", expression_span)),
                    span: expression_span,
                },
                false,
            ),
            (
                ast::Expression::Binary {
                    op: rumoca_core::OpBinary::Empty,
                    lhs: std::sync::Arc::new(terminal("1", expression_span)),
                    rhs: std::sync::Arc::new(terminal("2", expression_span)),
                    span: expression_span,
                },
                false,
            ),
            (
                ast::Expression::Terminal {
                    terminal_type: ast::TerminalType::Empty,
                    token: rumoca_core::Token::default(),
                    span: expression_span,
                },
                true,
            ),
        ];

        for (expression, is_literal_terminal) in invalid {
            let overlay = overlay_equation(
                ast::Equation::Simple {
                    lhs: terminal("1", owner_span),
                    rhs: expression,
                },
                owner_span,
            );
            let error = validate_flatten_input(&ast::ClassTree::new(), &overlay)
                .expect_err("a required recovery expression must not enter Flat lowering");
            if is_literal_terminal {
                assert!(matches!(
                    error,
                    FlattenError::InvalidLiteralTerminal { span, .. }
                        if span == expression_span
                ));
            } else {
                assert!(matches!(
                    error,
                    FlattenError::InvalidAstRecovery { span, .. }
                        if span == expression_span
                ));
            }
        }
    }

    #[test]
    fn recovery_equations_and_statements_fail_at_their_owner_span() {
        let owner_span = span_at(40, 52);
        for overlay in [
            overlay_equation(ast::Equation::Empty, owner_span),
            overlay_statement(ast::Statement::Empty, owner_span),
        ] {
            let error = validate_flatten_input(&ast::ClassTree::new(), &overlay)
                .expect_err("a recovery equation or statement must not enter Flat lowering");
            assert!(matches!(
                error,
                FlattenError::InvalidAstRecovery { span, .. } if span == owner_span
            ));
        }
    }

    #[test]
    fn genuinely_empty_equation_and_algorithm_sections_are_accepted() {
        let instance_id = rumoca_core::InstanceId::new(1);
        let mut overlay = ast::InstanceOverlay::default();
        overlay.classes.insert(
            instance_id,
            ast::ClassInstanceData {
                instance_id,
                algorithms: vec![Vec::new()],
                initial_algorithms: vec![Vec::new()],
                ..Default::default()
            },
        );

        validate_flatten_input(&ast::ClassTree::new(), &overlay)
            .expect("empty source sections are represented by empty lists, not recovery nodes");
    }

    #[test]
    fn legal_absent_start_and_tuple_receiver_slots_remain_accepted() {
        let owner_span = span_at(1, 40);
        let omitted_span = span_at(4, 5);
        let mut tree = ast::ClassTree::new();
        let mut class = ast::ClassDef {
            location: rumoca_core::Location {
                source: owner_span.source,
                start: owner_span.start.0 as u32,
                end: owner_span.end.0 as u32,
                ..Default::default()
            },
            ..Default::default()
        };
        class.components.insert(
            "record_value".to_string(),
            ast::Component::empty_with_span(omitted_span),
        );
        tree.definitions.classes.insert("M".to_string(), class);

        let tuple = ast::Expression::Parenthesized {
            inner: std::sync::Arc::new(ast::Expression::Parenthesized {
                inner: std::sync::Arc::new(ast::Expression::Tuple {
                    elements: vec![
                        ast::Expression::Empty { span: omitted_span },
                        terminal("1", span_at(8, 9)),
                    ],
                    span: span_at(3, 10),
                }),
                span: span_at(2, 11),
            }),
            span: span_at(1, 12),
        };
        let mut overlay = overlay_equation(
            ast::Equation::Simple {
                lhs: tuple,
                rhs: invocation(span_at(13, 17)),
            },
            owner_span,
        );
        let class_instance = overlay
            .classes
            .values_mut()
            .next()
            .expect("equation helper creates one class instance");
        class_instance.algorithms.push(vec![ast::InstanceStatement {
            statement: ast::Statement::FunctionCall {
                comp: ast::ComponentReference {
                    local: false,
                    parts: Vec::new(),
                    span: owner_span,
                    qualified_display_name: None,
                },
                args: Vec::new(),
                outputs: vec![ast::Expression::Empty { span: omitted_span }],
            },
            origin: ast::QualifiedName::default(),
            source_scope: None,
            source_scope_id: None,
            span: owner_span,
        }]);

        validate_flatten_input(&tree, &overlay)
            .expect("source-language absence is not parser recovery in these positions");
    }

    #[test]
    fn empty_expression_nested_below_a_retained_tuple_receiver_is_rejected() {
        let owner_span = span_at(1, 30);
        let invalid_span = span_at(7, 8);
        let nested = ast::Expression::Array {
            elements: vec![ast::Expression::Empty { span: invalid_span }],
            is_matrix: false,
            span: span_at(6, 9),
        };
        let overlay = overlay_equation(
            ast::Equation::Simple {
                lhs: ast::Expression::Tuple {
                    elements: vec![nested, terminal("1", span_at(11, 12))],
                    span: span_at(4, 13),
                },
                rhs: invocation(span_at(16, 20)),
            },
            owner_span,
        );

        let error = validate_flatten_input(&ast::ClassTree::new(), &overlay)
            .expect_err("only a tuple's direct omitted slots are legal absence");
        assert!(matches!(
            error,
            FlattenError::InvalidAstRecovery { span, .. } if span == invalid_span
        ));
    }

    #[test]
    fn direct_tuple_omission_without_an_invocation_rhs_is_rejected() {
        let owner_span = span_at(1, 30);
        let invalid_span = span_at(7, 8);
        let overlay = overlay_equation(
            ast::Equation::Simple {
                lhs: ast::Expression::Tuple {
                    elements: vec![
                        ast::Expression::Empty { span: invalid_span },
                        terminal("1", span_at(11, 12)),
                    ],
                    span: span_at(4, 13),
                },
                rhs: terminal("2", span_at(16, 17)),
            },
            owner_span,
        );

        let error = validate_flatten_input(&ast::ClassTree::new(), &overlay)
            .expect_err("tuple omission has meaning only for an invocation result");
        assert!(matches!(
            error,
            FlattenError::InvalidAstRecovery { span, .. } if span == invalid_span
        ));
    }

    #[test]
    fn context_only_value_syntax_is_rejected_before_flat_lowering() {
        let owner_span = span_at(1, 30);
        let invalid_span = span_at(12, 17);
        let invalid = [
            ast::Expression::Binary {
                op: rumoca_core::OpBinary::Assign,
                lhs: std::sync::Arc::new(terminal("1", invalid_span)),
                rhs: std::sync::Arc::new(terminal("2", invalid_span)),
                span: invalid_span,
            },
            ast::Expression::Terminal {
                terminal_type: ast::TerminalType::End,
                token: rumoca_core::Token::default(),
                span: invalid_span,
            },
            ast::Expression::NamedArgument {
                name: rumoca_core::Token::default(),
                value: std::sync::Arc::new(terminal("2", invalid_span)),
                span: invalid_span,
            },
            ast::Expression::Modification {
                target: reference("x", invalid_span),
                value: Some(std::sync::Arc::new(terminal("2", invalid_span))),
                span: invalid_span,
            },
        ];

        for expression in invalid {
            let overlay = overlay_equation(
                ast::Equation::Simple {
                    lhs: terminal("1", owner_span),
                    rhs: expression,
                },
                owner_span,
            );
            let error = validate_flatten_input(&ast::ClassTree::new(), &overlay)
                .expect_err("context-only syntax must not become an executable Flat value");
            assert!(matches!(
                error,
                FlattenError::InvalidAstRecovery { span, .. } if span == invalid_span
            ));
        }
    }

    #[test]
    fn end_subscript_and_nested_component_modifier_assignment_remain_legal() {
        let owner_span = span_at(1, 60);
        let end = ast::Expression::Terminal {
            terminal_type: ast::TerminalType::End,
            token: rumoca_core::Token::default(),
            span: span_at(18, 21),
        };
        let indexed = ast::Expression::ArrayIndex {
            base: std::sync::Arc::new(ast::Expression::ComponentReference(reference(
                "a",
                span_at(16, 17),
            ))),
            subscripts: vec![ast::Subscript::Expression(end)],
            span: span_at(16, 22),
        };
        let overlay = overlay_equation(
            ast::Equation::Simple {
                lhs: terminal("1", owner_span),
                rhs: ast::Expression::FunctionCall {
                    comp: reference("f", span_at(12, 13)),
                    args: vec![
                        ast::Expression::NamedArgument {
                            name: rumoca_core::Token {
                                text: std::sync::Arc::from("x"),
                                ..rumoca_core::Token::default()
                            },
                            value: std::sync::Arc::new(indexed),
                            span: span_at(14, 22),
                        },
                        ast::Expression::Modification {
                            target: reference("y", span_at(24, 25)),
                            value: Some(std::sync::Arc::new(terminal("2", span_at(28, 29)))),
                            span: span_at(24, 29),
                        },
                    ],
                    is_partial_application: false,
                    span: span_at(12, 23),
                },
            },
            owner_span,
        );

        let mut component = ast::Component::empty_with_span(owner_span);
        component.source_modifications = vec![ast::Expression::ClassModification {
            target: reference("field", span_at(30, 35)),
            modifications: vec![ast::Expression::Binary {
                op: rumoca_core::OpBinary::Assign,
                lhs: std::sync::Arc::new(ast::Expression::ClassModification {
                    target: reference("start", span_at(36, 41)),
                    modifications: Vec::new(),
                    each_flags: Vec::new(),
                    final_flags: Vec::new(),
                    redeclare_flags: Vec::new(),
                    span: span_at(36, 41),
                }),
                rhs: std::sync::Arc::new(terminal("2", span_at(44, 45))),
                span: span_at(36, 45),
            }],
            each_flags: vec![false],
            final_flags: vec![false],
            redeclare_flags: vec![false],
            span: span_at(30, 46),
        }];
        let mut class = ast::ClassDef {
            location: rumoca_core::Location {
                source: owner_span.source,
                start: owner_span.start.0 as u32,
                end: owner_span.end.0 as u32,
                ..Default::default()
            },
            ..Default::default()
        };
        class.components.insert("x".to_string(), component);
        let mut tree = ast::ClassTree::new();
        tree.definitions.classes.insert("M".to_string(), class);

        validate_flatten_input(&tree, &overlay)
            .expect("context-only syntax remains valid in its exact source context");
    }

    #[test]
    fn end_in_a_declaration_shape_is_rejected_without_an_indexed_array_bound() {
        let owner_span = span_at(1, 30);
        let invalid_span = span_at(12, 15);
        let mut component = ast::Component::empty_with_span(owner_span);
        component.shape_expr = vec![ast::Subscript::Expression(ast::Expression::Terminal {
            terminal_type: ast::TerminalType::End,
            token: rumoca_core::Token::default(),
            span: invalid_span,
        })];
        let mut class = ast::ClassDef {
            location: rumoca_core::Location {
                source: owner_span.source,
                start: owner_span.start.0 as u32,
                end: owner_span.end.0 as u32,
                ..Default::default()
            },
            ..Default::default()
        };
        class.components.insert("x".to_string(), component);
        let mut tree = ast::ClassTree::new();
        tree.definitions.classes.insert("M".to_string(), class);

        let error = validate_flatten_input(&tree, &ast::InstanceOverlay::default())
            .expect_err("a declaration shape does not establish an `end` selector context");
        assert!(matches!(
            error,
            FlattenError::InvalidAstRecovery { span, .. } if span == invalid_span
        ));
    }

    #[test]
    fn parser_produced_component_modifiers_remain_legal() {
        let stored = rumoca_phase_parse::parse_to_ast(
            "model M\n  Real x(start = 1);\nend M;\n",
            "legal_modifier.mo",
        )
        .expect("ordinary source modifier parses");
        let mut tree = ast::ClassTree::new();
        tree.definitions = stored;

        validate_flatten_input(&tree, &ast::InstanceOverlay::default())
            .expect("an ordinary parsed start modifier is an exact legal carrier");
    }

    #[test]
    fn parser_produced_annotation_modifiers_remain_legal_in_every_owner_context() {
        let stored = rumoca_phase_parse::parse_to_ast(
            r#"
model Base
end Base;

model M
  extends Base annotation(Evaluate = true);
  Real x annotation(Evaluate = true);
  annotation(Documentation(info = "ok"));
end M;

function f
  output Real y;
  external "C" y = f() annotation(Library = "m");
end f;
"#,
            "legal_annotations.mo",
        )
        .expect("ordinary annotations parse");
        let mut tree = ast::ClassTree::new();
        tree.definitions = stored;

        validate_flatten_input(&tree, &ast::InstanceOverlay::default())
            .expect("annotations are exact modifier-carrier contexts");
    }

    #[test]
    fn malformed_modifier_target_subscript_is_not_hidden_by_the_carrier_context() {
        let owner_span = span_at(1, 40);
        let mut target = reference("field", span_at(10, 18));
        target.parts[0].subs = Some(vec![ast::Subscript::Empty]);
        let mut component = ast::Component::empty_with_span(owner_span);
        component.source_modifications = vec![ast::Expression::Modification {
            target,
            value: Some(std::sync::Arc::new(terminal("1", span_at(22, 23)))),
            span: span_at(10, 23),
        }];
        let mut class = ast::ClassDef {
            location: rumoca_core::Location {
                source: owner_span.source,
                start: owner_span.start.0 as u32,
                end: owner_span.end.0 as u32,
                ..Default::default()
            },
            ..Default::default()
        };
        class.components.insert("x".to_string(), component);
        let mut tree = ast::ClassTree::new();
        tree.definitions.classes.insert("M".to_string(), class);

        let error = validate_flatten_input(&tree, &ast::InstanceOverlay::default())
            .expect_err("a modifier carrier cannot hide recovery in its target");
        assert!(matches!(error, FlattenError::InvalidAstSubscript { .. }));
    }

    #[test]
    fn the_first_recovery_owner_wins_in_source_order() {
        let first_span = span_at(7, 8);
        let second_span = span_at(20, 21);
        let mut overlay = overlay_equation(
            ast::Equation::Simple {
                lhs: ast::Expression::Empty { span: first_span },
                rhs: terminal("1", span_at(9, 10)),
            },
            span_at(1, 12),
        );
        overlay
            .classes
            .values_mut()
            .next()
            .expect("fixture has one class")
            .equations
            .push(ast::InstanceEquation {
                equation: ast::Equation::Simple {
                    lhs: ast::Expression::Empty { span: second_span },
                    rhs: terminal("2", span_at(22, 23)),
                },
                origin: ast::QualifiedName::default(),
                source_scope: None,
                source_scope_id: None,
                span: span_at(18, 25),
            });

        let error = validate_flatten_input(&ast::ClassTree::new(), &overlay)
            .expect_err("the first invalid node must stop validation immediately");
        assert!(matches!(
            error,
            FlattenError::InvalidAstRecovery { span, .. } if span == first_span
        ));
    }

    #[test]
    fn explicit_binding_flag_without_an_expression_is_rejected_before_default_lowering() {
        let owner_span = span_at(1, 40);
        let mut component = ast::Component::empty_with_span(span_at(12, 13));
        component.has_explicit_binding = true;
        component.binding = None;
        component.start = terminal("99", span_at(20, 22));
        let mut class = ast::ClassDef {
            location: rumoca_core::Location {
                source: owner_span.source,
                start: owner_span.start.0 as u32,
                end: owner_span.end.0 as u32,
                ..Default::default()
            },
            ..Default::default()
        };
        class.components.insert("x".to_string(), component);
        let mut tree = ast::ClassTree::new();
        tree.definitions.classes.insert("M".to_string(), class);

        let error = validate_flatten_input(&tree, &ast::InstanceOverlay::default())
            .expect_err("an explicit-binding marker must carry its expression");
        assert!(matches!(
            error,
            FlattenError::InvalidAstRecovery { span, .. } if span == owner_span
        ));
    }

    #[test]
    fn binding_expression_without_its_explicit_marker_is_rejected_before_default_lowering() {
        let owner_span = span_at(1, 40);
        let mut component = ast::Component::empty_with_span(span_at(12, 13));
        component.has_explicit_binding = false;
        component.binding = Some(terminal("7", span_at(20, 21)));
        let mut class = ast::ClassDef {
            location: rumoca_core::Location {
                source: owner_span.source,
                start: owner_span.start.0 as u32,
                end: owner_span.end.0 as u32,
                ..Default::default()
            },
            ..Default::default()
        };
        class.components.insert("x".to_string(), component);
        let mut tree = ast::ClassTree::new();
        tree.definitions.classes.insert("M".to_string(), class);

        let error = validate_flatten_input(&tree, &ast::InstanceOverlay::default())
            .expect_err("a declaration binding must carry its explicit marker");
        assert!(matches!(
            error,
            FlattenError::InvalidAstRecovery { span, .. } if span == owner_span
        ));
    }

    #[test]
    fn value_less_annotation_modifier_is_a_legal_carrier() {
        let stored = rumoca_phase_parse::parse_to_ast(
            r#"
model M
  replaceable Real re "Real part of complex number" annotation(Dialog);
  parameter Real k = 1 annotation(Evaluate = true);
end M;
"#,
            "value_less_annotation.mo",
        )
        .expect("a bare-name annotation modifier parses");
        let mut tree = ast::ClassTree::new();
        tree.definitions = stored;

        validate_flatten_input(&tree, &ast::InstanceOverlay::default())
            .expect("a value-less modification has no expression to reject");
    }

    #[test]
    fn recovery_node_in_a_present_modifier_value_is_still_rejected() {
        let owner_span = span_at(1, 40);
        let value_span = span_at(30, 34);
        let mut component = ast::Component::empty_with_span(owner_span);
        // The value slot is occupied, so this is not an omitted value: it is
        // a parser-recovery node in value position and must be refused at the
        // value's own span.
        component.annotation = vec![ast::Expression::Modification {
            target: reference("Dialog", span_at(20, 26)),
            value: Some(std::sync::Arc::new(ast::Expression::Empty {
                span: value_span,
            })),
            span: span_at(20, 34),
        }];
        let mut class = ast::ClassDef {
            location: rumoca_core::Location {
                source: owner_span.source,
                start: owner_span.start.0 as u32,
                end: owner_span.end.0 as u32,
                ..Default::default()
            },
            ..Default::default()
        };
        class.components.insert("re".to_string(), component);
        let mut tree = ast::ClassTree::new();
        tree.definitions.classes.insert("M".to_string(), class);

        let error = validate_flatten_input(&tree, &ast::InstanceOverlay::default())
            .expect_err("a present recovery value is a parse failure, never an omitted value");
        assert!(matches!(
            error,
            FlattenError::InvalidAstRecovery { span, .. } if span == value_span
        ));
    }
}
