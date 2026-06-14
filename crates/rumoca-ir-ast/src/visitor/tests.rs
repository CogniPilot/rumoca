use super::*;
use crate::AstIndexMap as IndexMap;
use crate::{
    ClassDef, Component, ComponentRefPart, ComponentReference, Equation, Expression, Extend,
    ExternalFunction, ForIndex, Import, Location, Name, OpBinary, Statement, StoredDefinition,
    Subscript, TerminalType, Token,
};
use std::ops::ControlFlow::{self, Break, Continue};
use std::sync::Arc;

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
        parts: vec![ComponentRefPart {
            ident: Token {
                text: std::sync::Arc::from(name),
                ..Default::default()
            },
            subs: None,
        }],
        def_id: None,
    }
}

fn make_comp_ref_with_subscript(name: &str, sub: Expression) -> ComponentReference {
    ComponentReference {
        local: false,
        span: rumoca_core::Span::DUMMY,
        parts: vec![ComponentRefPart {
            ident: Token {
                text: std::sync::Arc::from(name),
                ..Default::default()
            },
            subs: Some(vec![Subscript::Expression(sub)]),
        }],
        def_id: None,
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

struct Renamer;
impl ExpressionTransformer for Renamer {
    fn transform_component_reference(&mut self, mut cr: ComponentReference) -> Expression {
        if cr.to_string() == "x" {
            cr.parts[0].ident.text = std::sync::Arc::from("renamed");
        }
        Expression::ComponentReference(cr)
    }
}

#[test]
fn test_transformer_rename() {
    let expr = Expression::Binary {
        op: OpBinary::Add,
        lhs: Arc::new(make_var("x")),
        rhs: Arc::new(make_int(1)),
        span: rumoca_core::Span::DUMMY,
    };
    let result = Renamer.transform_expression(expr);
    let refs = collect_component_refs(&result);
    assert_eq!(refs[0].to_string(), "renamed");
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
        let _ = collector.visit_equation(eq);
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
    let _ = finder.visit_equation(&eq);
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
        span: rumoca_core::Span::DUMMY,
    };
    let equation_call = Equation::FunctionCall {
        comp: make_comp_ref("f_eq"),
        args: vec![make_int(2)],
    };
    let statement_call = Statement::FunctionCall {
        comp: make_comp_ref("f_stmt"),
        args: vec![make_int(3)],
        outputs: vec![make_var("out")],
    };

    let mut recorder = ContextRecorder::default();
    let _ = recorder.visit_expression(&expr_call);
    let _ = recorder.visit_equation(&equation_call);
    let _ = recorder.visit_statement(&statement_call);

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
            ..Default::default()
        },
    );

    let class = ClassDef {
        constrainedby: Some(Name::from_string("MyClassConstraint")),
        extends: vec![Extend {
            base_name: Name::from_string("MyBaseClass"),
            ..Default::default()
        }],
        components,
        ..Default::default()
    };

    let mut recorder = TypeNameRecorder::default();
    let _ = recorder.visit_class_def(&class);

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
    let _ = recorder.visit_stored_definition(&StoredDefinition {
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
                    ..Default::default()
                },
            );
            components
        },
        ..Default::default()
    };

    let mut recorder = SubscriptRecorder::default();
    let _ = recorder.visit_expression(&expr);
    let _ = recorder.visit_class_def(&class);

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
        ..Default::default()
    };
    component.modifications.insert("k".to_string(), make_int(2));
    component.annotation.push(make_int(3));

    ClassDef {
        extends: vec![Extend {
            base_name: Name::from_string("Base"),
            modifications: vec![crate::ExtendModification {
                expr: make_int(4),
                ..Default::default()
            }],
            annotation: vec![make_int(11)],
            ..Default::default()
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
    let _ = recorder.visit_class_def(&class);
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
    let _ = recorder.visit_class_def(&class);
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
    let _ = visitor.visit_stored_definition(&StoredDefinition {
        classes,
        within: None,
    });
    assert_eq!(visitor.0, vec!["Outer", "Inner"]);
}

#[test]
fn test_transformer_recurses_into_function_call_target_subscripts() {
    fn rewrite_one_subscript(mut cr: ComponentReference) -> ComponentReference {
        let Some(subscripts) = cr.parts.first_mut().and_then(|part| part.subs.as_mut()) else {
            return cr;
        };
        if matches!(
            subscripts.first(),
            Some(Subscript::Expression(Expression::Terminal {
                terminal_type: TerminalType::UnsignedInteger,
                token,
                ..
            })) if token.text.as_ref() == "1"
        ) {
            subscripts[0] = Subscript::Expression(make_int(2));
        }
        cr
    }

    struct IncrementSubscript;

    impl ExpressionTransformer for IncrementSubscript {
        fn transform_component_ref_inner(&mut self, cr: ComponentReference) -> ComponentReference {
            rewrite_one_subscript(cr)
        }
    }

    let expr = Expression::FunctionCall {
        comp: make_comp_ref_with_subscript("f", make_int(1)),
        args: vec![],
        span: rumoca_core::Span::DUMMY,
    };
    let transformed = IncrementSubscript.transform_expression(expr);

    let Expression::FunctionCall { comp, .. } = transformed else {
        panic!("expected transformed function call");
    };
    let Some(subscripts) = &comp.parts[0].subs else {
        panic!("expected transformed function-call target subscripts");
    };
    let Subscript::Expression(Expression::Terminal { token, .. }) = &subscripts[0] else {
        panic!("expected integer subscript");
    };
    assert_eq!(token.text.as_ref(), "2");
}
