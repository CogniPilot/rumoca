use super::*;
use crate::AstIndexMap as IndexMap;
use crate::{
    ClassDef, Component, ComponentRefPart, ComponentReference, Equation, Expression, Extend,
    ExternalFunction, ForIndex, Import, Location, Name, OpBinary, Statement, StoredDefinition,
    Subscript, TerminalType, Token,
};
use std::ops::ControlFlow::{self, Break, Continue};
use std::sync::Arc;

fn test_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("visitor_test.mo"),
        1,
        2,
    )
}

fn make_var(name: &str) -> Expression {
    Expression::ComponentReference(make_comp_ref(name))
}

fn make_int(value: i64) -> Expression {
    Expression::Terminal {
        terminal_type: TerminalType::UnsignedInteger,
        token: Token {
            text: std::sync::Arc::from(value.to_string()),
            ..Default::default()
        },
        span: rumoca_core::Span::DUMMY,
    }
}

fn make_comp_ref(name: &str) -> ComponentReference {
    ComponentReference {
        local: false,
        span: rumoca_core::Span::DUMMY,
        qualified_display_name: None,
        parts: vec![ComponentRefPart {
            ident: Token {
                text: std::sync::Arc::from(name),
                ..Default::default()
            },
            subs: None,
            def_id: None,
        }],
    }
}

fn make_comp_ref_with_subscript(name: &str, sub: Expression) -> ComponentReference {
    ComponentReference {
        local: false,
        span: rumoca_core::Span::DUMMY,
        qualified_display_name: None,
        parts: vec![ComponentRefPart {
            ident: Token {
                text: std::sync::Arc::from(name),
                ..Default::default()
            },
            subs: Some(vec![Subscript::Expression(sub)]),
            def_id: None,
        }],
    }
}

#[test]
fn test_collect_component_refs() {
    let expr = Expression::Binary {
        op: OpBinary::Add,
        lhs: Arc::new(make_var("x")),
        rhs: Arc::new(make_var("y")),
        span: rumoca_core::Span::DUMMY,
    };
    let refs = collect_component_refs(&expr);
    assert_eq!(refs.len(), 2);
    assert_eq!(refs[0].to_string(), "x");
    assert_eq!(refs[1].to_string(), "y");
}

#[test]
fn expression_component_path_preserves_projected_fields_and_indices() {
    let projected = Expression::FieldAccess {
        base: Arc::new(Expression::ArrayIndex {
            base: Arc::new(Expression::FieldAccess {
                base: Arc::new(make_var("stackData")),
                field: "cellData".to_string(),
                field_def_id: None,
                span: rumoca_core::Span::DUMMY,
            }),
            subscripts: vec![
                Subscript::Expression(make_int(1)),
                Subscript::Expression(make_int(2)),
            ],
            span: rumoca_core::Span::DUMMY,
        }),
        field: "nRC".to_string(),
        field_def_id: None,
        span: rumoca_core::Span::DUMMY,
    };

    let path = expression_component_path(&projected).expect("path-shaped expression");
    assert_eq!(path.as_str(), "stackData.cellData[1,2].nRC");
}

#[test]
fn test_contains_component_ref() {
    let expr = Expression::Binary {
        op: OpBinary::Add,
        lhs: Arc::new(make_var("x")),
        rhs: Arc::new(make_int(1)),
        span: rumoca_core::Span::DUMMY,
    };
    assert!(contains_component_ref(&expr, |cr| cr.to_string() == "x"));
    assert!(!contains_component_ref(&expr, |cr| cr.to_string() == "y"));
}

/// Marks every single-part reference named `x` through the semantic editor's
/// qualified display spelling, the name-layer write a reference hook owns.
struct Marker;
impl ExpressionTransformer for Marker {
    fn transform_component_reference(
        &mut self,
        mut cr: SemanticReferenceEditor<'_>,
        _site: ComponentReferenceSite,
    ) {
        let is_x = cr.view().part_count() == 1
            && cr
                .view()
                .parts()
                .next()
                .is_some_and(|part| part.ident_text() == "x");
        if is_x {
            cr.set_qualified_display_name("marked");
        }
    }
}

fn marked(mut expr: Expression) -> Expression {
    transform_expression_in_place(&mut Marker, &mut expr);
    expr
}

fn is_marked(reference: &ComponentReference) -> bool {
    reference
        .qualified_display_name()
        .is_some_and(|name| name.as_str() == "marked")
}

fn rewrite_test_identity_slot(mut slot: PartIdentitySlot<'_>) {
    match slot.ident_text() {
        "a" => {
            assert_eq!(slot.def_id(), Some(rumoca_core::DefId::new(7)));
            slot.clear_def_id();
        }
        "b" => {
            assert_eq!(slot.def_id(), None);
            slot.set_def_id(rumoca_core::DefId::new(9));
        }
        other => panic!("unexpected part `{other}`"),
    }
}

#[test]
fn transformer_marks_reference_through_semantic_editor() {
    let result = marked(Expression::Binary {
        op: OpBinary::Add,
        lhs: Arc::new(make_var("x")),
        rhs: Arc::new(make_int(1)),
        span: rumoca_core::Span::DUMMY,
    });
    let refs = collect_component_refs(&result);
    assert!(is_marked(&refs[0]));
    assert_eq!(refs[0].to_string(), "x", "the editor writes no part idents");
}

#[test]
fn semantic_editor_slots_write_per_part_identity_in_declaration_order() {
    struct SlotWriter;
    impl ExpressionTransformer for SlotWriter {
        fn transform_component_reference(
            &mut self,
            mut reference: SemanticReferenceEditor<'_>,
            _site: ComponentReferenceSite,
        ) {
            for slot in reference.part_identity_slots() {
                rewrite_test_identity_slot(slot);
            }
        }
    }

    let mut expression = Expression::ComponentReference(ComponentReference {
        local: false,
        span: test_span(),
        qualified_display_name: None,
        parts: vec![
            ComponentRefPart {
                ident: Token {
                    text: std::sync::Arc::from("a"),
                    ..Default::default()
                },
                subs: None,
                def_id: Some(rumoca_core::DefId::new(7)),
            },
            ComponentRefPart {
                ident: Token {
                    text: std::sync::Arc::from("b"),
                    ..Default::default()
                },
                subs: None,
                def_id: None,
            },
        ],
    });
    transform_expression_in_place(&mut SlotWriter, &mut expression);
    let Expression::ComponentReference(reference) = &expression else {
        unreachable!("reference variant is preserved");
    };
    assert_eq!(reference.parts[0].def_id, None);
    assert_eq!(reference.parts[1].def_id, Some(rumoca_core::DefId::new(9)));
}

#[test]
fn substitution_replaces_nested_unshadowed_occurrences() {
    let mut expression = Expression::Binary {
        op: OpBinary::Add,
        lhs: Arc::new(Expression::Unary {
            op: rumoca_core::OpUnary::Minus,
            rhs: Arc::new(make_var("i")),
            span: test_span(),
        }),
        rhs: Arc::new(make_var("i")),
        span: test_span(),
    };
    substitute_integer_loop_index(&mut expression, "i", 9);
    let Expression::Binary { lhs, rhs, .. } = expression else {
        unreachable!("binary root must be preserved");
    };
    let Expression::Unary { rhs: nested, .. } = lhs.as_ref() else {
        unreachable!("nested unary must be preserved");
    };
    for child in [nested.as_ref(), rhs.as_ref()] {
        let Expression::Terminal {
            terminal_type,
            token,
            ..
        } = child
        else {
            panic!("substituted reference must become an integer terminal");
        };
        assert_eq!(*terminal_type, TerminalType::UnsignedInteger);
        assert_eq!(token.text.as_ref(), "9");
    }
}

#[test]
fn substitution_rewrites_derivative_arguments_without_erasing_callable_identity() {
    let mut expression = Expression::DerivativeCall {
        args: vec![make_var("i")],
        span: test_span(),
    };

    substitute_integer_loop_index(&mut expression, "i", 9);

    let Expression::DerivativeCall { args, .. } = expression else {
        panic!("loop-index substitution must preserve derivative identity");
    };
    assert!(matches!(
        args.as_slice(),
        [Expression::Terminal {
            terminal_type: TerminalType::UnsignedInteger,
            token,
            ..
        }] if token.text.as_ref() == "9"
    ));
}

#[test]
fn test_equation_visitor_collect_connects() {
    struct ConnectCollector(Vec<(String, String)>);
    impl Visitor for ConnectCollector {
        fn visit_connect(
            &mut self,
            lhs: &ComponentReference,
            rhs: &ComponentReference,
        ) -> ControlFlow<()> {
            self.0.push((lhs.to_string(), rhs.to_string()));
            Continue(())
        }
    }

    let equations = vec![
        Equation::Connect {
            lhs: make_comp_ref("a"),
            rhs: make_comp_ref("b"),
        },
        Equation::Simple {
            lhs: make_var("x"),
            rhs: make_int(1),
        },
        Equation::Connect {
            lhs: make_comp_ref("c"),
            rhs: make_comp_ref("d"),
        },
    ];

    let mut collector = ConnectCollector(Vec::new());
    for eq in &equations {
        let _visit_outcome = collector.visit_equation(eq);
    }
    assert_eq!(
        collector.0,
        vec![("a".into(), "b".into()), ("c".into(), "d".into())]
    );
}

#[test]
fn test_early_termination() {
    struct FirstConnect(Option<String>);
    impl Visitor for FirstConnect {
        fn visit_connect(
            &mut self,
            lhs: &ComponentReference,
            _: &ComponentReference,
        ) -> ControlFlow<()> {
            self.0 = Some(lhs.to_string());
            Break(())
        }
    }

    let eq = Equation::For {
        indices: vec![ForIndex {
            ident: Token {
                text: std::sync::Arc::from("i"),
                ..Default::default()
            },
            range: make_int(1),
        }],
        equations: vec![
            Equation::Connect {
                lhs: make_comp_ref("first"),
                rhs: make_comp_ref("a"),
            },
            Equation::Connect {
                lhs: make_comp_ref("second"),
                rhs: make_comp_ref("b"),
            },
        ],
    };

    let mut finder = FirstConnect(None);
    let _visit_outcome = finder.visit_equation(&eq);
    assert_eq!(finder.0, Some("first".into()));
}

#[test]
fn test_function_call_context_dispatch() {
    #[derive(Default)]
    struct ContextRecorder {
        function_contexts: Vec<FunctionCallContext>,
        component_contexts: Vec<ComponentReferenceContext>,
        statement_output_expression_contexts: usize,
    }

    impl Visitor for ContextRecorder {
        fn visit_expr_function_call_ctx(
            &mut self,
            comp: &ComponentReference,
            args: &[Expression],
            ctx: FunctionCallContext,
        ) -> ControlFlow<()> {
            self.function_contexts.push(ctx);
            walk_expr_function_call_ctx_default(self, comp, args, ctx)
        }

        fn visit_component_reference_ctx(
            &mut self,
            cr: &ComponentReference,
            ctx: ComponentReferenceContext,
        ) -> ControlFlow<()> {
            self.component_contexts.push(ctx);
            self.visit_component_reference(cr)
        }

        fn visit_expression_ctx(
            &mut self,
            expr: &Expression,
            ctx: ExpressionContext,
        ) -> ControlFlow<()> {
            self.statement_output_expression_contexts +=
                usize::from(ctx == ExpressionContext::StatementFunctionOutput);
            self.visit_expression(expr)
        }
    }

    let expr_call = Expression::FunctionCall {
        comp: make_comp_ref("f_expr"),
        args: vec![make_int(1)],
        is_partial_application: false,
        span: rumoca_core::Span::DUMMY,
    };
    let equation_call = Equation::FunctionCall {
        comp: make_comp_ref("f_eq"),
        args: vec![make_int(2)],
        span: rumoca_core::Span::DUMMY,
    };
    let statement_call = Statement::FunctionCall {
        comp: make_comp_ref("f_stmt"),
        args: vec![make_int(3)],
        outputs: vec![make_var("out")],
    };

    let mut recorder = ContextRecorder::default();
    let _expression_outcome = recorder.visit_expression(&expr_call);
    let _equation_outcome = recorder.visit_equation(&equation_call);
    let _statement_outcome = recorder.visit_statement(&statement_call);

    assert_eq!(
        recorder.function_contexts,
        vec![
            FunctionCallContext::Expression,
            FunctionCallContext::Equation,
            FunctionCallContext::Statement,
        ]
    );
    assert!(
        recorder
            .component_contexts
            .contains(&ComponentReferenceContext::ExpressionFunctionCallTarget)
    );
    assert!(
        recorder
            .component_contexts
            .contains(&ComponentReferenceContext::EquationFunctionCallTarget)
    );
    assert!(
        recorder
            .component_contexts
            .contains(&ComponentReferenceContext::StatementFunctionCallTarget)
    );
    assert_eq!(recorder.statement_output_expression_contexts, 1);
}

#[test]
fn test_type_name_context_dispatch() {
    #[derive(Default)]
    struct TypeNameRecorder {
        seen: Vec<(String, TypeNameContext)>,
    }

    impl Visitor for TypeNameRecorder {
        fn visit_type_name(&mut self, name: &Name, ctx: TypeNameContext) -> ControlFlow<()> {
            self.seen.push((name.to_string(), ctx));
            Continue(())
        }
    }

    let mut components = IndexMap::default();
    components.insert(
        "comp".to_string(),
        Component {
            type_name: Name::from_string("MyComponentType"),
            constrainedby: Some(Name::from_string("MyComponentConstraint")),
            ..Component::empty_with_span(test_span())
        },
    );

    let class = ClassDef {
        constrainedby: Some(Name::from_string("MyClassConstraint")),
        extends: vec![Extend {
            base_name: Name::from_string("MyBaseClass"),
            base_def_id: None,
            location: Location::default(),
            modifications: Vec::new(),
            break_names: Vec::new(),
            is_protected: false,
            annotation: Vec::new(),
        }],
        components,
        ..Default::default()
    };

    let mut recorder = TypeNameRecorder::default();
    let _visit_outcome = recorder.visit_class_def(&class);

    assert!(recorder.seen.contains(&(
        "MyClassConstraint".to_string(),
        TypeNameContext::ClassConstrainedBy
    )));
    assert!(
        recorder
            .seen
            .contains(&("MyBaseClass".to_string(), TypeNameContext::ExtendsBase))
    );
    assert!(recorder.seen.contains(&(
        "MyComponentType".to_string(),
        TypeNameContext::ComponentType
    )));
    assert!(recorder.seen.contains(&(
        "MyComponentConstraint".to_string(),
        TypeNameContext::ComponentConstrainedBy
    )));
}

#[test]
fn test_name_context_dispatch() {
    #[derive(Default)]
    struct NameRecorder {
        seen: Vec<(String, NameContext)>,
    }

    impl Visitor for NameRecorder {
        fn visit_name_ctx(&mut self, name: &Name, ctx: NameContext) -> ControlFlow<()> {
            self.seen.push((name.to_string(), ctx));
            Continue(())
        }
    }

    let class = ClassDef {
        imports: vec![Import::Qualified {
            path: Name::from_string("Modelica.Blocks"),
            location: Location::default(),
            global_scope: false,
        }],
        ..Default::default()
    };

    let mut classes = IndexMap::default();
    classes.insert("Outer".to_string(), class);

    let mut recorder = NameRecorder::default();
    let _visit_outcome = recorder.visit_stored_definition(&StoredDefinition {
        classes,
        within: Some(Name::from_string("Top.Level")),
    });

    assert!(
        recorder
            .seen
            .contains(&("Top.Level".to_string(), NameContext::WithinClause))
    );
    assert!(
        recorder
            .seen
            .contains(&("Modelica.Blocks".to_string(), NameContext::ImportPath))
    );
}

#[test]
fn test_subscript_context_dispatch() {
    #[derive(Default)]
    struct SubscriptRecorder {
        seen: Vec<SubscriptContext>,
    }

    impl Visitor for SubscriptRecorder {
        fn visit_subscript_ctx(
            &mut self,
            sub: &Subscript,
            ctx: SubscriptContext,
        ) -> ControlFlow<()> {
            self.seen.push(ctx);
            self.visit_subscript(sub)
        }
    }

    let expr = Expression::ArrayIndex {
        base: Arc::new(Expression::ComponentReference(
            make_comp_ref_with_subscript("a", make_int(1)),
        )),
        subscripts: vec![Subscript::Expression(make_int(2))],
        span: rumoca_core::Span::DUMMY,
    };

    let class = ClassDef {
        array_subscripts: vec![Subscript::Expression(make_int(3))],
        components: {
            let mut components = IndexMap::default();
            components.insert(
                "x".to_string(),
                Component {
                    shape_expr: vec![Subscript::Expression(make_int(4))],
                    ..Component::empty_with_span(test_span())
                },
            );
            components
        },
        ..Default::default()
    };

    let mut recorder = SubscriptRecorder::default();
    let _expression_outcome = recorder.visit_expression(&expr);
    let _class_outcome = recorder.visit_class_def(&class);

    assert!(
        recorder
            .seen
            .contains(&SubscriptContext::ComponentReferencePart)
    );
    assert!(recorder.seen.contains(&SubscriptContext::ArrayIndex));
    assert!(
        recorder
            .seen
            .contains(&SubscriptContext::ClassArraySubscript)
    );
    assert!(recorder.seen.contains(&SubscriptContext::ComponentShape));
}

fn make_expression_context_dispatch_class() -> ClassDef {
    let mut component = Component {
        type_name: Name::from_string("Real"),
        start: make_int(1),
        binding: Some(make_int(10)),
        condition: Some(make_var("cond")),
        ..Component::empty_with_span(test_span())
    };
    component.modifications.insert("k".to_string(), make_int(2));
    component.annotation.push(make_int(3));

    ClassDef {
        extends: vec![Extend {
            base_name: Name::from_string("Base"),
            modifications: vec![crate::ExtendModification {
                expr: make_int(4),
                each: false,
                final_: false,
                redeclare: false,
            }],
            annotation: vec![make_int(11)],
            base_def_id: None,
            location: Location::default(),
            break_names: Vec::new(),
            is_protected: false,
        }],
        annotation: vec![make_int(12)],
        components: {
            let mut comps = IndexMap::default();
            comps.insert("x".to_string(), component);
            comps
        },
        equations: vec![Equation::Assert {
            condition: make_var("eq_cond"),
            message: make_int(5),
            level: Some(make_int(6)),
        }],
        algorithms: vec![vec![
            Statement::Assert {
                condition: make_var("stmt_cond"),
                message: make_int(7),
                level: Some(Box::new(make_int(8))),
            },
            Statement::FunctionCall {
                comp: make_comp_ref("f"),
                args: vec![make_int(9)],
                outputs: vec![make_var("y")],
            },
        ]],
        external: Some(ExternalFunction {
            args: vec![make_int(13)],
            annotation: vec![make_int(14)],
            ..Default::default()
        }),
        ..Default::default()
    }
}

fn assert_expression_contexts_seen(seen: &[ExpressionContext]) {
    assert!(seen.contains(&ExpressionContext::ComponentStart));
    assert!(seen.contains(&ExpressionContext::ComponentBinding));
    assert!(seen.contains(&ExpressionContext::ComponentModification));
    assert!(seen.contains(&ExpressionContext::ComponentCondition));
    assert!(seen.contains(&ExpressionContext::ComponentAnnotation));
    assert!(seen.contains(&ExpressionContext::ClassAnnotation));
    assert!(seen.contains(&ExpressionContext::ExtendAnnotation));
    assert!(seen.contains(&ExpressionContext::ExtendModification));
    assert!(seen.contains(&ExpressionContext::EquationAssertCondition));
    assert!(seen.contains(&ExpressionContext::EquationAssertMessage));
    assert!(seen.contains(&ExpressionContext::EquationAssertLevel));
    assert!(seen.contains(&ExpressionContext::StatementAssertCondition));
    assert!(seen.contains(&ExpressionContext::StatementAssertMessage));
    assert!(seen.contains(&ExpressionContext::StatementAssertLevel));
    assert!(seen.contains(&ExpressionContext::StatementFunctionOutput));
    assert!(seen.contains(&ExpressionContext::ExternalArgument));
    assert!(seen.contains(&ExpressionContext::ExternalAnnotation));
}

#[test]
fn test_expression_context_dispatch() {
    #[derive(Default)]
    struct ExpressionContextRecorder {
        seen: Vec<ExpressionContext>,
    }

    impl Visitor for ExpressionContextRecorder {
        fn visit_expression_ctx(
            &mut self,
            expr: &Expression,
            ctx: ExpressionContext,
        ) -> ControlFlow<()> {
            self.seen.push(ctx);
            self.visit_expression(expr)
        }
    }

    let class = make_expression_context_dispatch_class();

    let mut recorder = ExpressionContextRecorder::default();
    let _visit_outcome = recorder.visit_class_def(&class);
    assert_expression_contexts_seen(&recorder.seen);
}

#[test]
fn test_external_output_context_dispatch() {
    #[derive(Default)]
    struct ComponentContextRecorder {
        seen: Vec<ComponentReferenceContext>,
    }

    impl Visitor for ComponentContextRecorder {
        fn visit_component_reference_ctx(
            &mut self,
            cr: &ComponentReference,
            ctx: ComponentReferenceContext,
        ) -> ControlFlow<()> {
            self.seen.push(ctx);
            self.visit_component_reference(cr)
        }
    }

    let class = ClassDef {
        external: Some(ExternalFunction {
            output: Some(make_comp_ref("result")),
            ..Default::default()
        }),
        ..Default::default()
    };

    let mut recorder = ComponentContextRecorder::default();
    let _visit_outcome = recorder.visit_class_def(&class);
    assert!(
        recorder
            .seen
            .contains(&ComponentReferenceContext::ExternalOutput)
    );
}

struct ClassNames(Vec<String>);
impl Visitor for ClassNames {
    fn visit_class_def(&mut self, class: &ClassDef) -> ControlFlow<()> {
        self.0.push(class.name.text.to_string());
        for (_, nested) in &class.classes {
            self.visit_class_def(nested)?;
        }
        Continue(())
    }
}

#[test]
fn test_class_visitor_nested() {
    let mut inner = IndexMap::default();
    inner.insert(
        "Inner".into(),
        ClassDef {
            name: Token {
                text: "Inner".into(),
                ..Default::default()
            },
            ..Default::default()
        },
    );
    let mut classes = IndexMap::default();
    classes.insert(
        "Outer".into(),
        ClassDef {
            name: Token {
                text: "Outer".into(),
                ..Default::default()
            },
            classes: inner,
            ..Default::default()
        },
    );

    let mut visitor = ClassNames(Vec::new());
    let _visit_outcome = visitor.visit_stored_definition(&StoredDefinition {
        classes,
        within: None,
    });
    assert_eq!(visitor.0, vec!["Outer", "Inner"]);
}

#[test]
fn kernel_recurses_into_populated_function_call_target_subscripts_and_args() {
    // The call is populated on both edges the kernel owns for it: the callee
    // carries a subscript expression and the argument list carries two
    // arguments (one nested). A kernel that dropped either edge would leave
    // an "x" unmarked below.
    let transformed = marked(Expression::FunctionCall {
        comp: make_comp_ref_with_subscript("f", make_var("x")),
        args: vec![
            make_var("x"),
            Expression::Unary {
                op: rumoca_core::OpUnary::Minus,
                rhs: Arc::new(make_var("x")),
                span: test_span(),
            },
        ],
        is_partial_application: false,
        span: rumoca_core::Span::DUMMY,
    });

    let Expression::FunctionCall { comp, args, .. } = &transformed else {
        panic!("expected transformed function call");
    };
    let Some(subscripts) = &comp.parts[0].subs else {
        panic!("expected transformed function-call target subscripts");
    };
    let Subscript::Expression(subscript) = &subscripts[0] else {
        panic!("expected expression subscript");
    };
    assert!(is_marked(&collect_component_refs(subscript)[0]));
    assert_eq!(
        comp.parts[0].ident.text.as_ref(),
        "f",
        "the callee itself is handed to the callee hook, not the reference hook"
    );
    assert!(
        comp.qualified_display_name().is_none(),
        "the callee reaches only the callee hook, which does not mark"
    );
    assert!(is_marked(&collect_component_refs(&args[0])[0]));
    assert!(is_marked(&collect_component_refs(&args[1])[0]));
}

struct Identity;
impl ExpressionTransformer for Identity {}

#[test]
fn transform_reuses_uniquely_owned_arc_allocation() {
    super::rewrite::reset_copy_on_write_events();
    let child = Arc::new(make_var("x"));
    let child_ptr = Arc::as_ptr(&child);
    let expr = Expression::Unary {
        op: rumoca_core::OpUnary::Minus,
        rhs: child,
        span: test_span(),
    };

    let mut expr = expr;
    transform_expression_in_place(&mut Identity, &mut expr);
    let result = expr;
    let Expression::Unary { rhs, .. } = result else {
        unreachable!("identity transform must preserve the Unary variant");
    };
    assert_eq!(
        Arc::as_ptr(&rhs),
        child_ptr,
        "a uniquely owned child Arc must be transformed in place, not reallocated"
    );
    assert_eq!(super::rewrite::copy_on_write_events(), 0);
}

#[test]
fn transform_reuses_uniquely_owned_arcs_for_binary_children() {
    let lhs = Arc::new(make_var("x"));
    let rhs = Arc::new(make_int(1));
    let lhs_ptr = Arc::as_ptr(&lhs);
    let rhs_ptr = Arc::as_ptr(&rhs);
    let expr = Expression::Binary {
        op: OpBinary::Add,
        lhs,
        rhs,
        span: test_span(),
    };

    let result = marked(expr);
    let Expression::Binary { lhs, rhs, .. } = result else {
        unreachable!("transform must preserve the Binary variant");
    };
    assert_eq!(Arc::as_ptr(&lhs), lhs_ptr);
    assert_eq!(Arc::as_ptr(&rhs), rhs_ptr);
    assert!(is_marked(&collect_component_refs(&lhs)[0]));
}

#[test]
fn transform_of_shared_arc_leaves_original_untouched() {
    let child = Arc::new(make_var("x"));
    let retained = Arc::clone(&child);
    assert_eq!(Arc::strong_count(&child), 2);
    let expr = Expression::Binary {
        op: OpBinary::Add,
        lhs: child,
        rhs: Arc::new(make_int(1)),
        span: test_span(),
    };

    let result = marked(expr);
    let Expression::Binary { lhs, .. } = result else {
        unreachable!("transform must preserve the Binary variant");
    };
    assert_ne!(
        Arc::as_ptr(&lhs),
        Arc::as_ptr(&retained),
        "a shared child must be copied on write"
    );
    assert!(is_marked(&collect_component_refs(&lhs)[0]));
    assert!(
        !is_marked(&collect_component_refs(&retained)[0]),
        "the retained clone must still hold the pre-transform reference"
    );
}

#[test]
fn shared_transform_path_copy_on_write_count_is_explicit_and_bounded() {
    super::rewrite::reset_copy_on_write_events();
    let shared = Arc::new(Expression::Binary {
        op: OpBinary::Add,
        lhs: Arc::new(make_var("x")),
        rhs: Arc::new(Expression::Unary {
            op: rumoca_core::OpUnary::Minus,
            rhs: Arc::new(make_var("y")),
            span: test_span(),
        }),
        span: test_span(),
    });
    let retained = Arc::clone(&shared);
    let expression = Expression::Unary {
        op: rumoca_core::OpUnary::Minus,
        rhs: shared,
        span: test_span(),
    };

    let transformed = marked(expression);
    let Expression::Unary { rhs: new_root, .. } = transformed else {
        unreachable!("outer expression remains unary");
    };
    let Expression::Binary {
        lhs: new_lhs,
        rhs: new_rhs,
        ..
    } = new_root.as_ref()
    else {
        unreachable!("shared subtree remains binary");
    };
    let Expression::Unary {
        rhs: new_rhs_child, ..
    } = new_rhs.as_ref()
    else {
        unreachable!("nested expression remains unary");
    };
    let Expression::Binary {
        lhs: old_lhs,
        rhs: old_rhs,
        ..
    } = retained.as_ref()
    else {
        unreachable!("retained subtree remains binary");
    };
    let Expression::Unary {
        rhs: old_rhs_child, ..
    } = old_rhs.as_ref()
    else {
        unreachable!("retained nested expression remains unary");
    };

    let detached_edges = [
        !Arc::ptr_eq(&new_root, &retained),
        !Arc::ptr_eq(new_lhs, old_lhs),
        !Arc::ptr_eq(new_rhs, old_rhs),
        !Arc::ptr_eq(new_rhs_child, old_rhs_child),
    ]
    .into_iter()
    .filter(|changed| *changed)
    .count();
    assert_eq!(
        detached_edges, 4,
        "the shared fixture must detach all four retained Arc edges"
    );
    assert_eq!(
        super::rewrite::copy_on_write_events(),
        4,
        "the traversal must invoke copy-on-write exactly once at each shared edge"
    );
    assert!(is_marked(&collect_component_refs(&new_root)[0]));
    assert!(!is_marked(&collect_component_refs(&retained)[0]));
}

#[test]
fn kernel_preserves_range_subtrees_through_arc_children() {
    // Range holds three Arc children (including the optional step); a payload
    // that lost its identity during the in-place swap would show up here.
    let expr = Expression::Range {
        start: Arc::new(make_var("a")),
        step: Some(Arc::new(make_var("b"))),
        end: Arc::new(make_var("x")),
        span: test_span(),
    };
    let result = marked(expr);
    let Expression::Range {
        start, step, end, ..
    } = result
    else {
        unreachable!("transform must preserve the Range variant");
    };
    assert_eq!(collect_component_refs(&start)[0].to_string(), "a");
    assert_eq!(
        collect_component_refs(step.as_ref().expect("step present"))[0].to_string(),
        "b"
    );
    assert!(is_marked(&collect_component_refs(&end)[0]));
}

/// Span whose start byte offset doubles as a unique node identity in trace
/// tests below.
fn sp(id: usize) -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("visitor_trace.mo"),
        id,
        id + 1,
    )
}

fn t(id: usize) -> Expression {
    Expression::Terminal {
        terminal_type: TerminalType::UnsignedInteger,
        token: Token {
            text: std::sync::Arc::from("0"),
            ..Default::default()
        },
        span: sp(id),
    }
}

fn part(name: &str, subs: Option<Vec<Subscript>>) -> ComponentRefPart {
    ComponentRefPart {
        ident: Token {
            text: std::sync::Arc::from(name),
            ..Default::default()
        },
        subs,
        def_id: None,
    }
}

fn cref(id: usize, name: &str, subs: Option<Vec<Subscript>>) -> ComponentReference {
    ComponentReference {
        local: false,
        span: sp(id),
        qualified_display_name: None,
        parts: vec![part(name, subs)],
    }
}

fn cref_expr(id: usize, name: &str) -> Expression {
    Expression::ComponentReference(cref(id, name, None))
}

fn fi(name: &str, range: Expression) -> ForIndex {
    ForIndex {
        ident: Token {
            text: std::sync::Arc::from(name),
            ..Default::default()
        },
        range,
    }
}

fn ref_name(reference: &ComponentReference) -> String {
    reference
        .parts
        .iter()
        .map(|part| part.ident.text.as_ref())
        .collect::<Vec<_>>()
        .join(".")
}

use super::rewrite::KernelTraceEvent::{self, Bind, Callee, Cref, Node, Unbind};
use super::rewrite::{reset_kernel_trace, take_kernel_trace};

/// Run one kernel entry under the test-only kernel trace and return the
/// ordered event vector: node entries keyed by span start, hook dispatches
/// keyed by dotted spelling, and iterator bind/unbind events from the
/// canonical schedule. The trace is kernel-owned instrumentation, so it
/// observes every client - hook transformers and the sealed substitution
/// alike - without any client owning a node hook.
fn traced(run: impl FnOnce()) -> Vec<KernelTraceEvent> {
    reset_kernel_trace();
    run();
    take_kernel_trace()
}

/// Elements one through three of the populated tree: a full range, nested
/// arithmetic, and a multi-part reference mixing all three subscript forms.
fn populated_arithmetic_and_reference_elements() -> Vec<Expression> {
    vec![
        Expression::Range {
            start: Arc::new(t(3)),
            step: Some(Arc::new(t(4))),
            end: Arc::new(t(5)),
            span: sp(2),
        },
        Expression::Unary {
            op: rumoca_core::OpUnary::Minus,
            rhs: Arc::new(Expression::Binary {
                op: OpBinary::Add,
                lhs: Arc::new(t(8)),
                rhs: Arc::new(t(9)),
                span: sp(7),
            }),
            span: sp(6),
        },
        Expression::ComponentReference(ComponentReference {
            local: false,
            span: sp(10),
            qualified_display_name: None,
            parts: vec![
                part(
                    "a",
                    Some(vec![
                        Subscript::Expression(t(11)),
                        Subscript::Empty,
                        Subscript::Range {
                            token: Token::default(),
                        },
                    ]),
                ),
                part("b", Some(vec![Subscript::Expression(t(12))])),
            ],
        }),
    ]
}

/// A call with a subscripted callee and two arguments, and a class
/// modification carrying a nested modification and a named argument.
fn populated_call_and_modification_elements() -> Vec<Expression> {
    vec![
        Expression::FunctionCall {
            comp: cref(90, "f", Some(vec![Subscript::Expression(t(14))])),
            args: vec![t(15), cref_expr(16, "x2")],
            is_partial_application: false,
            span: sp(13),
        },
        Expression::ClassModification {
            target: cref(91, "t", Some(vec![Subscript::Expression(t(18))])),
            modifications: vec![
                Expression::Modification {
                    target: cref(92, "m", Some(vec![Subscript::Expression(t(20))])),
                    value: Some(Arc::new(t(21))),
                    span: sp(19),
                },
                Expression::NamedArgument {
                    name: Token::default(),
                    value: Arc::new(t(23)),
                    span: sp(22),
                },
            ],
            each_flags: vec![false, false],
            final_flags: vec![false, false],
            redeclare_flags: vec![false, false],
            span: sp(17),
        },
    ]
}

/// Populated control-flow, comprehension, indexing, and collection forms.
fn populated_control_and_collection_elements() -> Vec<Expression> {
    vec![
        Expression::If {
            branches: vec![(t(25), t(26)), (t(27), t(28))],
            else_branch: Arc::new(cref_expr(29, "e")),
            span: sp(24),
        },
        Expression::Parenthesized {
            inner: Arc::new(Expression::ArrayComprehension {
                expr: Arc::new(t(32)),
                indices: vec![fi("i", t(33)), fi("j", t(34))],
                filter: Some(Arc::new(t(35))),
                span: sp(31),
            }),
            span: sp(30),
        },
        Expression::ArrayIndex {
            base: Arc::new(Expression::FieldAccess {
                base: Arc::new(cref_expr(38, "g")),
                field: "h".to_string(),
                field_def_id: None,
                span: sp(37),
            }),
            subscripts: vec![
                Subscript::Expression(t(39)),
                Subscript::Empty,
                Subscript::Expression(t(40)),
            ],
            span: sp(36),
        },
        Expression::Array {
            elements: vec![t(42), Expression::Empty { span: sp(43) }],
            is_matrix: false,
            span: sp(41),
        },
    ]
}

/// One tree holding all 17 expression variants in populated form: every
/// optional child present, every list non-empty, multiple parts, multiple
/// elements, multiple if-branches, and mixed subscript forms.
fn populated_variant_tree() -> Expression {
    let mut elements = populated_arithmetic_and_reference_elements();
    elements.extend(populated_call_and_modification_elements());
    elements.extend(populated_control_and_collection_elements());
    Expression::Tuple {
        elements,
        span: sp(1),
    }
}

/// The exact preorder event sequence the kernel owes the populated tree.
fn populated_expected_events() -> Vec<KernelTraceEvent> {
    let value = ComponentReferenceSite::Value;
    let modification = ComponentReferenceSite::ModificationTarget;
    vec![
        Node(1),
        Node(2),
        Node(3),
        Node(4),
        Node(5),
        Node(6),
        Node(7),
        Node(8),
        Node(9),
        Node(10),
        Node(11),
        Node(12),
        Cref("a.b".into(), value),
        Node(13),
        Node(14),
        Callee("f".into(), CalleeSite::ExpressionCall),
        Node(15),
        Node(16),
        Cref("x2".into(), value),
        Node(17),
        Node(18),
        Cref("t".into(), modification),
        Node(19),
        Node(20),
        Cref("m".into(), modification),
        Node(21),
        Node(22),
        Node(23),
        Node(24),
        Node(25),
        Node(26),
        Node(27),
        Node(28),
        Node(29),
        Cref("e".into(), value),
        Node(30),
        Node(31),
        Node(34),
        Bind("j".into()),
        Node(33),
        Bind("i".into()),
        Node(32),
        Node(35),
        Unbind("i".into()),
        Unbind("j".into()),
        Node(36),
        Node(37),
        Node(38),
        Cref("g".into(), value),
        Node(39),
        Node(40),
        Node(41),
        Node(42),
        Node(43),
    ]
}

/// Exact once-and-order trace over the populated form of all 17 variants,
/// including the comprehension iterator schedule (MLS 3.7 §10.4.1.2
/// reverse textual nesting): the LAST textual iterator's range and bind
/// first, then the earlier one's, then body and filter, unbinding in
/// textual order. This is the constructor-family ordering witness.
///
/// The assertion is an equality over the full ordered event vector, so a
/// kernel arm that skips, duplicates, or reorders any child edge fails
/// here, and so does a schedule mutation: forward textual order (the
/// for-loop schedule applied to a comprehension), all-ranges-before-any-
/// bind (the flat misreading of the §10.4.1 scope sentence), and
/// bind-before-range all produce a different vector. This is the
/// behavioral witness that each handled variant actually recurses; the
/// architecture scan alone cannot prove that.
#[test]
fn kernel_visits_every_populated_edge_exactly_once_in_order() {
    let mut root = populated_variant_tree();
    let events = traced(|| transform_expression_in_place(&mut Identity, &mut root));
    assert_eq!(events, populated_expected_events());
}

/// The emptiest form of all 17 variants: absent optionals, empty lists,
/// single parts without subscripts.
fn empty_variant_tree() -> Expression {
    Expression::Tuple {
        elements: vec![
            Expression::Empty { span: sp(52) },
            t(53),
            Expression::Range {
                start: Arc::new(t(55)),
                step: None,
                end: Arc::new(t(56)),
                span: sp(54),
            },
            Expression::Unary {
                op: rumoca_core::OpUnary::Minus,
                rhs: Arc::new(t(58)),
                span: sp(57),
            },
            Expression::Binary {
                op: OpBinary::Add,
                lhs: Arc::new(t(60)),
                rhs: Arc::new(t(61)),
                span: sp(59),
            },
            cref_expr(62, "v"),
            Expression::FunctionCall {
                comp: cref(93, "f0", None),
                args: vec![],
                is_partial_application: false,
                span: sp(63),
            },
            Expression::ClassModification {
                target: cref(94, "t0", None),
                modifications: vec![],
                each_flags: vec![],
                final_flags: vec![],
                redeclare_flags: vec![],
                span: sp(64),
            },
            Expression::NamedArgument {
                name: Token::default(),
                value: Arc::new(t(66)),
                span: sp(65),
            },
            Expression::Modification {
                target: cref(95, "m0", None),
                value: Some(Arc::new(t(68))),
                span: sp(67),
            },
            Expression::Array {
                elements: vec![],
                is_matrix: false,
                span: sp(69),
            },
            Expression::Tuple {
                elements: vec![],
                span: sp(70),
            },
            Expression::If {
                branches: vec![],
                else_branch: Arc::new(t(72)),
                span: sp(71),
            },
            Expression::Parenthesized {
                inner: Arc::new(t(74)),
                span: sp(73),
            },
            Expression::ArrayComprehension {
                expr: Arc::new(t(76)),
                indices: vec![],
                filter: None,
                span: sp(75),
            },
            Expression::ArrayIndex {
                base: Arc::new(t(78)),
                subscripts: vec![],
                span: sp(77),
            },
            Expression::FieldAccess {
                base: Arc::new(t(80)),
                field: "h".to_string(),
                field_def_id: None,
                span: sp(79),
            },
        ],
        span: sp(51),
    }
}

/// The exact event sequence the kernel owes the empty-form tree.
fn empty_expected_events() -> Vec<KernelTraceEvent> {
    vec![
        Node(51),
        Node(52),
        Node(53),
        Node(54),
        Node(55),
        Node(56),
        Node(57),
        Node(58),
        Node(59),
        Node(60),
        Node(61),
        Node(62),
        Cref("v".into(), ComponentReferenceSite::Value),
        Node(63),
        Callee("f0".into(), CalleeSite::ExpressionCall),
        Node(64),
        Cref("t0".into(), ComponentReferenceSite::ModificationTarget),
        Node(65),
        Node(66),
        Node(67),
        Cref("m0".into(), ComponentReferenceSite::ModificationTarget),
        Node(68),
        Node(69),
        Node(70),
        Node(71),
        Node(72),
        Node(73),
        Node(74),
        Node(75),
        Node(76),
        Node(77),
        Node(78),
        Node(79),
        Node(80),
    ]
}

/// Exact once-and-order trace over the emptiest form of all 17 variants.
///
/// This witness proves that empty and absent edges neither derail the order
/// nor swallow later siblings, and that the empty forms still reach their
/// hooks. It observes no recursion into absent children (there are none to
/// recurse into); the populated-form test above carries that claim.
#[test]
fn kernel_keeps_exact_order_across_empty_variant_forms() {
    let mut root = empty_variant_tree();
    let events = traced(|| transform_expression_in_place(&mut Identity, &mut root));
    assert_eq!(events, empty_expected_events());
}

/// Substitution installs the literal at the reference's own span and never
/// visits the replaced reference's subscript expressions: substitution ends
/// the branch. Killed mutants: a kernel that descends into the replaced
/// node's children shows `Node(3)` in the trace; one that installs the
/// literal elsewhere fails the span assertion.
#[test]
fn substitution_installs_literal_and_skips_reference_subscripts() {
    let mut expr = Expression::Unary {
        op: rumoca_core::OpUnary::Minus,
        rhs: Arc::new(Expression::ComponentReference(cref(
            2,
            "seed",
            Some(vec![Subscript::Expression(t(3))]),
        ))),
        span: sp(1),
    };
    let events = traced(|| substitute_integer_loop_index(&mut expr, "seed", 7));
    assert_eq!(
        events,
        vec![Node(1), Node(2)],
        "the replaced reference's subscript expression is never visited"
    );
    let Expression::Unary { rhs, .. } = &expr else {
        unreachable!("outer unary is preserved");
    };
    let Expression::Terminal {
        terminal_type,
        token,
        span,
    } = rhs.as_ref()
    else {
        panic!("the literal must be installed");
    };
    assert_eq!(*terminal_type, TerminalType::UnsignedInteger);
    assert_eq!(token.text.as_ref(), "7");
    assert_eq!(*span, sp(2), "the literal stands at the reference's span");
}

/// A negative value is a unary minus over its exact unsigned magnitude,
/// never a signed spelling inside an unsigned-integer token.
#[test]
fn negative_substitution_is_unary_minus_over_unsigned_magnitude() {
    for (value, magnitude_text) in [(-1_i64, "1"), (i64::MIN + 1, "9223372036854775807")] {
        let mut expr = cref_expr(2, "seed");
        substitute_integer_loop_index(&mut expr, "seed", value);
        let Expression::Unary {
            op: rumoca_core::OpUnary::Minus,
            rhs,
            span,
        } = &expr
        else {
            panic!("a negative substitution must install a unary minus, got {expr:?}");
        };
        assert_eq!(*span, sp(2));
        let Expression::Terminal {
            terminal_type,
            token,
            span: inner_span,
        } = rhs.as_ref()
        else {
            panic!("the magnitude must be a terminal");
        };
        assert_eq!(*terminal_type, TerminalType::UnsignedInteger);
        assert_eq!(
            token.text.as_ref(),
            magnitude_text,
            "the unsigned token spells the exact magnitude and carries no sign"
        );
        assert_eq!(*inner_span, sp(2));
    }
}

/// The substitution is total over `i64`: the minimum, whose magnitude has no
/// unsigned i64 spelling, is minted in the MLS 3.7 §4.9.2 lower-bound shape
/// `(-9223372036854775807) - 1`, every operand of which is representable and
/// which the checked integer evaluators execute without overflow. Killed
/// mutant: a refusal or a signed token for the minimum fails the shape
/// asserts; a wrapped magnitude fails the token equality.
#[test]
fn minimum_substitution_is_the_spec_lower_bound_shape() {
    let mut expr = cref_expr(2, "seed");
    substitute_integer_loop_index(&mut expr, "seed", i64::MIN);
    let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs,
        span,
    } = &expr
    else {
        panic!("the minimum must be a checked subtraction, got {expr:?}");
    };
    assert_eq!(*span, sp(2));
    let Expression::Unary {
        op: rumoca_core::OpUnary::Minus,
        rhs: magnitude,
        ..
    } = lhs.as_ref()
    else {
        panic!("the left operand must negate the maximum magnitude");
    };
    assert!(matches!(
        magnitude.as_ref(),
        Expression::Terminal {
            terminal_type: TerminalType::UnsignedInteger,
            token,
            ..
        } if token.text.as_ref() == "9223372036854775807"
    ));
    assert!(matches!(
        rhs.as_ref(),
        Expression::Terminal {
            terminal_type: TerminalType::UnsignedInteger,
            token,
            ..
        } if token.text.as_ref() == "1"
    ));
}

/// The language counterexample `{j for j in 1:j}`: MLS 3.7 §10.4.1 /
/// §10.3.4.1 evaluate a constructor's iterator expressions in the scope
/// immediately enclosing it, so the range `j` is the outer binding while
/// body and filter `j` are the iterator. Substituting outer `j` must
/// replace the range occurrence and leave body and filter untouched. Killed
/// mutants: a whole-comprehension prune leaves the range unsubstituted, and
/// a bind-before-range schedule suppresses the range substitution the same
/// way. The shadowed and outer names share one spelling and the range end
/// is a real reference, so the fixture can observe both failures.
#[test]
fn outer_shadow_range_witness_substitutes_range_not_body_or_filter() {
    let mut expr = Expression::ArrayComprehension {
        expr: Arc::new(cref_expr(3, "j")),
        indices: vec![fi(
            "j",
            Expression::Range {
                start: Arc::new(t(4)),
                step: None,
                end: Arc::new(cref_expr(5, "j")),
                span: sp(6),
            },
        )],
        filter: Some(Arc::new(cref_expr(7, "j"))),
        span: sp(2),
    };
    substitute_integer_loop_index(&mut expr, "j", 5);
    let Expression::ArrayComprehension {
        expr: body,
        indices,
        filter,
        ..
    } = &expr
    else {
        unreachable!("comprehension is preserved");
    };
    let Expression::Range { end, .. } = &indices[0].range else {
        unreachable!("range form is preserved");
    };
    assert!(
        matches!(end.as_ref(), Expression::Terminal { token, .. } if token.text.as_ref() == "5"),
        "the range occurrence is the outer binding and must be substituted"
    );
    assert!(
        matches!(
            body.as_ref(),
            Expression::ComponentReference(reference) if ref_name(reference) == "j"
        ),
        "the body occurrence is the iterator and must stay"
    );
    assert!(
        matches!(
            filter.as_deref(),
            Some(Expression::ComponentReference(reference)) if ref_name(reference) == "j"
        ),
        "the filter occurrence is the iterator and must stay"
    );
}

/// The unlicensed direction for constructors: in `{i for i in 1:3, j in
/// 1:i}` the §10.4.1.2 expansion is `{{i for i in 1:3} for j in 1:i}`, so
/// `j`'s range is OUTERMOST and cannot see the constructor's `i` - the `i`
/// in `j`'s range is the outer binding. Substituting outer `i` must replace
/// it and the outer control occurrence, leaving only the binder-owned body.
/// This is the reverse of the for-loop rule, where `j in 1:i` would see the
/// loop's `i` (that witness lives with the resolve adapter). Killed mutant:
/// the for-loop forward schedule applied to a comprehension binds `i`
/// before `j`'s range and wrongly suppresses this substitution.
#[test]
fn last_iterator_range_cannot_see_an_earlier_iterator() {
    let mut expr = Expression::Tuple {
        elements: vec![
            cref_expr(2, "i"),
            Expression::ArrayComprehension {
                expr: Arc::new(cref_expr(3, "i")),
                indices: vec![
                    fi(
                        "i",
                        Expression::Range {
                            start: Arc::new(t(4)),
                            step: None,
                            end: Arc::new(t(5)),
                            span: sp(6),
                        },
                    ),
                    fi(
                        "j",
                        Expression::Range {
                            start: Arc::new(t(7)),
                            step: None,
                            end: Arc::new(cref_expr(8, "i")),
                            span: sp(9),
                        },
                    ),
                ],
                filter: None,
                span: sp(10),
            },
        ],
        span: sp(1),
    };
    substitute_integer_loop_index(&mut expr, "i", 9);
    let Expression::Tuple { elements, .. } = &expr else {
        unreachable!("tuple root is preserved");
    };
    assert!(
        matches!(&elements[0], Expression::Terminal { token, .. } if token.text.as_ref() == "9"),
        "the outer occurrence is substituted: the fixture observes the substitution"
    );
    let Expression::ArrayComprehension {
        expr: body,
        indices,
        ..
    } = &elements[1]
    else {
        unreachable!("comprehension is preserved");
    };
    let Expression::Range { end, .. } = &indices[1].range else {
        unreachable!("multi-iterator range form is preserved");
    };
    assert!(
        matches!(end.as_ref(), Expression::Terminal { token, .. } if token.text.as_ref() == "9"),
        "`i` in `j`'s range is the outer binding under section 10.4.1 and must be substituted"
    );
    assert!(
        matches!(
            body.as_ref(),
            Expression::ComponentReference(reference) if ref_name(reference) == "i"
        ),
        "the body `i` is the iterator and must stay"
    );
}

/// The licensed direction for constructors: in `{i + j for i in 1:j, j in
/// 1:n}` the §10.4.1.2 expansion is `{{i + j for i in 1:j} for j in 1:n}`,
/// so the FIRST textual iterator's range sits inside the outer constructor's
/// body where `j` is already bound: the `j` in `i`'s range is the
/// constructor's iterator, not an outer binding. Substituting outer `j`
/// must therefore leave `i`'s range and the body untouched while replacing
/// the outer control occurrence and the (unbound) `n`-side spelling. Killed
/// mutants: the flat all-ranges-unbound misreading of §10.4.1 substitutes
/// the `j` in `i`'s range, and so does the for-loop forward schedule; the
/// previous witness alone cannot see either of the reversed schedules that
/// this one catches, which is why both directions are required.
#[test]
fn first_iterator_range_sees_the_later_iterator() {
    let mut expr = Expression::Tuple {
        elements: vec![
            cref_expr(2, "j"),
            Expression::ArrayComprehension {
                expr: Arc::new(cref_expr(3, "j")),
                indices: vec![
                    fi(
                        "i",
                        Expression::Range {
                            start: Arc::new(t(4)),
                            step: None,
                            end: Arc::new(cref_expr(5, "j")),
                            span: sp(6),
                        },
                    ),
                    fi(
                        "j",
                        Expression::Range {
                            start: Arc::new(t(7)),
                            step: None,
                            end: Arc::new(cref_expr(8, "j")),
                            span: sp(9),
                        },
                    ),
                ],
                filter: None,
                span: sp(10),
            },
        ],
        span: sp(1),
    };
    substitute_integer_loop_index(&mut expr, "j", 6);
    let Expression::Tuple { elements, .. } = &expr else {
        unreachable!("tuple root is preserved");
    };
    assert!(
        matches!(&elements[0], Expression::Terminal { token, .. } if token.text.as_ref() == "6"),
        "the outer occurrence is substituted: the fixture observes the substitution"
    );
    let Expression::ArrayComprehension {
        expr: body,
        indices,
        ..
    } = &elements[1]
    else {
        unreachable!("comprehension is preserved");
    };
    let Expression::Range { end: first_end, .. } = &indices[0].range else {
        unreachable!("first iterator's range form is preserved");
    };
    assert!(
        matches!(
            first_end.as_ref(),
            Expression::ComponentReference(reference) if ref_name(reference) == "j"
        ),
        "`j` in the first iterator's range is the later, outer-nested iterator and must stay"
    );
    let Expression::Range { end: last_end, .. } = &indices[1].range else {
        unreachable!("last iterator's range form is preserved");
    };
    assert!(
        matches!(last_end.as_ref(), Expression::Terminal { token, .. } if token.text.as_ref() == "6"),
        "the last iterator's own range never sees its own binding: its `j` is the outer one"
    );
    assert!(
        matches!(
            body.as_ref(),
            Expression::ComponentReference(reference) if ref_name(reference) == "j"
        ),
        "the body `j` is the iterator and must stay"
    );
}

/// A rebinding comprehension shadows the substituted index for its body; a
/// non-rebinding sibling is substituted; and an occurrence after the
/// comprehension is substituted again, so the unbind path is observed, not
/// assumed. Killed mutants: a substituter that never unbinds leaves the
/// trailing occurrence unsubstituted; one that shadows by name-independent
/// depth substitutes the shadowed body.
#[test]
fn rebinding_comprehension_shadows_and_binding_is_released_after_it() {
    let mut expr = Expression::Tuple {
        elements: vec![
            Expression::ArrayComprehension {
                expr: Arc::new(cref_expr(3, "i")),
                indices: vec![fi("i", t(4))],
                filter: None,
                span: sp(2),
            },
            Expression::ArrayComprehension {
                expr: Arc::new(cref_expr(6, "i")),
                indices: vec![fi("j", t(7))],
                filter: None,
                span: sp(5),
            },
            cref_expr(8, "i"),
        ],
        span: sp(1),
    };
    substitute_integer_loop_index(&mut expr, "i", 4);
    let Expression::Tuple { elements, .. } = &expr else {
        unreachable!("tuple root is preserved");
    };
    let Expression::ArrayComprehension {
        expr: shadowed_body,
        ..
    } = &elements[0]
    else {
        unreachable!("first comprehension is preserved");
    };
    assert!(
        matches!(
            shadowed_body.as_ref(),
            Expression::ComponentReference(reference) if ref_name(reference) == "i"
        ),
        "the shadowed body keeps the inner index reference"
    );
    let Expression::ArrayComprehension {
        expr: substituted_body,
        ..
    } = &elements[1]
    else {
        unreachable!("second comprehension is preserved");
    };
    assert!(
        matches!(
            substituted_body.as_ref(),
            Expression::Terminal { token, .. } if token.text.as_ref() == "4"
        ),
        "the non-rebinding body is substituted"
    );
    assert!(
        matches!(&elements[2], Expression::Terminal { token, .. } if token.text.as_ref() == "4"),
        "the binding is released after the rebinding comprehension"
    );
}

/// The if-expression else branch is a kernel-owned edge like any other.
#[test]
fn kernel_recurses_into_populated_if_branches_and_else_branch() {
    let transformed = marked(Expression::If {
        branches: vec![(make_var("x"), make_var("x"))],
        else_branch: Arc::new(make_var("x")),
        span: test_span(),
    });
    let refs = collect_component_refs(&transformed);
    assert_eq!(refs.len(), 3);
    for reference in refs {
        assert!(is_marked(&reference));
    }
}

#[test]
fn owned_component_reference_entry_traverses_subscripts_then_hook() {
    let mut reference = ComponentReference {
        local: false,
        span: sp(90),
        qualified_display_name: None,
        parts: vec![
            part("a", Some(vec![Subscript::Expression(t(7))])),
            part("b", Some(vec![Subscript::Expression(t(8))])),
        ],
    };
    let events = traced(|| transform_component_reference_in_place(&mut Identity, &mut reference));
    assert_eq!(
        events,
        vec![
            Node(7),
            Node(8),
            Cref("a.b".into(), ComponentReferenceSite::OwnedTarget),
        ],
    );
}

#[test]
fn owned_callee_entry_traverses_subscripts_then_callee_hook() {
    let mut callee = cref(91, "f", Some(vec![Subscript::Expression(t(7))]));
    let events = traced(|| transform_callee_in_place(&mut Identity, &mut callee));
    assert_eq!(
        events,
        vec![Node(7), Callee("f".into(), CalleeSite::OwnedCall)],
    );
}

#[test]
fn subscript_list_entry_traverses_expression_subscripts_in_order() {
    let mut subscripts = vec![
        Subscript::Expression(cref_expr(5, "x")),
        Subscript::Empty,
        Subscript::Range {
            token: Token::default(),
        },
        Subscript::Expression(t(6)),
    ];
    let events = traced(|| transform_subscripts_in_place(&mut Identity, &mut subscripts));
    assert_eq!(
        events,
        vec![
            Node(5),
            Cref("x".into(), ComponentReferenceSite::Value),
            Node(6),
        ],
    );
}

#[test]
fn for_index_entry_traverses_the_range_expression() {
    let mut index = fi(
        "i",
        Expression::Binary {
            op: OpBinary::Add,
            lhs: Arc::new(t(7)),
            rhs: Arc::new(t(8)),
            span: sp(6),
        },
    );
    let events = traced(|| transform_for_index_in_place(&mut Identity, &mut index));
    assert_eq!(events, vec![Node(6), Node(7), Node(8)]);
}
