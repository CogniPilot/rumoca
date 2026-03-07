use super::*;
use rumoca_ir_ast::{
    ClassType, Component, ComponentRefPart, ComponentReference, ForIndex, Token, Variability,
};
use rustc_hash::FxHashMap;

fn token(text: &str) -> Token {
    Token {
        text: text.into(),
        location: Default::default(),
        token_number: 0,
        token_type: 0,
    }
}

fn cref(name: &str) -> ComponentReference {
    ComponentReference {
        local: false,
        parts: crate::path_utils::split_path_with_indices(name)
            .into_iter()
            .map(|part| ComponentRefPart {
                ident: token(part),
                subs: None,
            })
            .collect(),
        def_id: None,
    }
}

fn int_expr(value: i64) -> Expression {
    Expression::Terminal {
        terminal_type: TerminalType::UnsignedInteger,
        token: token(&value.to_string()),
    }
}

fn real_expr(value: f64) -> Expression {
    Expression::Terminal {
        terminal_type: TerminalType::UnsignedReal,
        token: token(&value.to_string()),
    }
}

fn bool_expr(value: bool) -> Expression {
    Expression::Terminal {
        terminal_type: TerminalType::Bool,
        token: token(if value { "true" } else { "false" }),
    }
}

fn cref_expr(name: &str) -> Expression {
    Expression::ComponentReference(cref(name))
}

fn call(name: &str, args: Vec<Expression>) -> Expression {
    Expression::FunctionCall {
        comp: cref(name),
        args,
    }
}

fn binary(op: OpBinary, lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op,
        lhs: Arc::new(lhs),
        rhs: Arc::new(rhs),
    }
}

fn input_parameter(name: &str, binding: Option<Expression>) -> Component {
    let has_explicit_binding = binding.is_some();
    Component {
        name: name.to_string(),
        causality: Causality::Input(token("input")),
        variability: Variability::Parameter(token("parameter")),
        binding,
        has_explicit_binding,
        ..Component::default()
    }
}

fn output_parameter(name: &str) -> Component {
    Component {
        name: name.to_string(),
        causality: Causality::Output(token("output")),
        variability: Variability::Parameter(token("parameter")),
        ..Component::default()
    }
}

fn ns1_init_statement() -> Statement {
    Statement::Assignment {
        comp: cref("ns1"),
        value: binary(
            OpBinary::Mul(token("*")),
            int_expr(2),
            call(
                "integer",
                vec![call(
                    "ceil",
                    vec![binary(
                        OpBinary::Div(token("/")),
                        binary(
                            OpBinary::Mul(token("*")),
                            cref_expr("f_max"),
                            cref_expr("f_max_factor"),
                        ),
                        cref_expr("f_resolution"),
                    )],
                )],
            ),
        ),
    }
}

fn ns_update_statement() -> Statement {
    let mod_ns1_2 = call("mod", vec![cref_expr("ns1"), int_expr(2)]);
    Statement::Assignment {
        comp: cref("ns"),
        value: Expression::If {
            branches: vec![(
                binary(OpBinary::Eq(token("==")), mod_ns1_2, int_expr(0)),
                cref_expr("ns1"),
            )],
            else_branch: Arc::new(binary(
                OpBinary::Add(token("+")),
                cref_expr("ns1"),
                int_expr(1),
            )),
        },
    }
}

fn reduce_factors_loop(modulus: i64) -> Statement {
    let divisor = int_expr(modulus);
    Statement::While(StatementBlock {
        cond: binary(
            OpBinary::Eq(token("==")),
            call("mod", vec![cref_expr("ns1"), divisor.clone()]),
            int_expr(0),
        ),
        stmts: vec![Statement::Assignment {
            comp: cref("ns1"),
            value: call("div", vec![cref_expr("ns1"), divisor]),
        }],
    })
}

fn ns_adjustment_loop() -> Statement {
    Statement::While(StatementBlock {
        cond: bool_expr(true),
        stmts: vec![
            Statement::Assignment {
                comp: cref("ns1"),
                value: cref_expr("ns"),
            },
            reduce_factors_loop(2),
            reduce_factors_loop(3),
            reduce_factors_loop(5),
            Statement::If {
                cond_blocks: vec![StatementBlock {
                    cond: binary(OpBinary::Le(token("<=")), cref_expr("ns1"), int_expr(1)),
                    stmts: vec![Statement::Break {
                        token: token("break"),
                    }],
                }],
                else_block: None,
            },
            Statement::Assignment {
                comp: cref("ns"),
                value: binary(OpBinary::Add(token("+")), cref_expr("ns"), int_expr(2)),
            },
        ],
    })
}

fn build_sample_points_function() -> ClassDef {
    let mut function = ClassDef {
        name: token("samplePoints"),
        class_type: ClassType::Function,
        pure: true,
        ..ClassDef::default()
    };
    function
        .components
        .insert("f_max".to_string(), input_parameter("f_max", None));
    function.components.insert(
        "f_resolution".to_string(),
        input_parameter("f_resolution", None),
    );
    function.components.insert(
        "f_max_factor".to_string(),
        input_parameter("f_max_factor", Some(int_expr(5))),
    );
    function
        .components
        .insert("ns".to_string(), output_parameter("ns"));
    function.components.insert(
        "ns1".to_string(),
        Component {
            name: "ns1".to_string(),
            variability: Variability::Discrete(token("discrete")),
            ..Component::default()
        },
    );

    function.algorithms.push(vec![
        ns1_init_statement(),
        ns_update_statement(),
        ns_adjustment_loop(),
        Statement::Return {
            token: token("return"),
        },
    ]);
    function
}

fn nfi_expr() -> Expression {
    call(
        "max",
        vec![
            int_expr(1),
            call(
                "min",
                vec![
                    binary(
                        OpBinary::Add(token("+")),
                        call(
                            "integer",
                            vec![call(
                                "ceil",
                                vec![binary(
                                    OpBinary::Div(token("/")),
                                    int_expr(4),
                                    real_expr(0.2),
                                )],
                            )],
                        ),
                        int_expr(1),
                    ),
                    binary(
                        OpBinary::Add(token("+")),
                        call("div", vec![cref_expr("ns"), int_expr(2)]),
                        int_expr(1),
                    ),
                ],
            ),
        ],
    )
}

#[test]
fn eval_integer_with_scope_div_operator_requires_exact_quotient() {
    let ctx = TypeCheckEvalContext::new();
    let expr = binary(OpBinary::Div(token("/")), int_expr(7), int_expr(2));
    assert_eq!(eval_integer_with_scope(&expr, &ctx, ""), None);
}

#[test]
fn eval_integer_with_scope_div_builtin_remains_truncating() {
    let ctx = TypeCheckEvalContext::new();
    let expr = call("div", vec![int_expr(7), int_expr(2)]);
    assert_eq!(eval_integer_with_scope(&expr, &ctx, ""), Some(3));
}

#[test]
fn eval_integer_with_scope_add_elem_uses_shared_binary_semantics() {
    let ctx = TypeCheckEvalContext::new();
    let expr = binary(OpBinary::AddElem(token(".+")), int_expr(2), int_expr(3));
    assert_eq!(eval_integer_with_scope(&expr, &ctx, ""), Some(5));
}

#[test]
fn eval_integer_with_scope_exp_elem_uses_shared_binary_semantics() {
    let ctx = TypeCheckEvalContext::new();
    let expr = binary(OpBinary::ExpElem(token(".^")), int_expr(2), int_expr(5));
    assert_eq!(eval_integer_with_scope(&expr, &ctx, ""), Some(32));
}

#[test]
fn eval_boolean_with_scope_enum_eq_accepts_suffix_qualification() {
    let mut ctx = TypeCheckEvalContext::new();
    ctx.enums.insert(
        "controllerType".to_string(),
        "Modelica.Blocks.Types.SimpleController.PI".to_string(),
    );

    let expr = binary(
        OpBinary::Eq(token("==")),
        cref_expr("controllerType"),
        cref_expr("SimpleController.PI"),
    );

    assert_eq!(eval_boolean_with_scope(&expr, &ctx, ""), Some(true));
}

#[test]
fn eval_boolean_with_scope_enum_eq_accepts_shared_type_literal_tail() {
    let mut ctx = TypeCheckEvalContext::new();
    ctx.enums.insert(
        "frameResolve".to_string(),
        "sensor_frame_a2.MultiBody.Types.ResolveInFrameA.frame_resolve".to_string(),
    );

    let expr = binary(
        OpBinary::Eq(token("==")),
        cref_expr("frameResolve"),
        cref_expr("Modelica.Mechanics.MultiBody.Types.ResolveInFrameA.frame_resolve"),
    );

    assert_eq!(eval_boolean_with_scope(&expr, &ctx, ""), Some(true));
}

#[test]
fn eval_boolean_with_scope_enum_eq_rejects_different_enum_type() {
    let mut ctx = TypeCheckEvalContext::new();
    ctx.enums.insert(
        "mode".to_string(),
        "Modelica.Blocks.Types.Init.PI".to_string(),
    );

    let expr = binary(
        OpBinary::Eq(token("==")),
        cref_expr("mode"),
        cref_expr("Modelica.Blocks.Types.SimpleController.PI"),
    );

    assert_eq!(eval_boolean_with_scope(&expr, &ctx, ""), Some(false));
}

#[test]
fn eval_integer_with_scope_evaluates_while_based_function_with_real_inputs() {
    let mut functions = FxHashMap::default();
    functions.insert("samplePoints".to_string(), build_sample_points_function());

    let mut ctx = TypeCheckEvalContext::new();
    ctx.functions = Arc::new(functions);

    let expr = call("samplePoints", vec![real_expr(4.0), real_expr(0.2)]);
    assert_eq!(eval_integer_with_scope(&expr, &ctx, ""), Some(200));

    ctx.integers.insert("ns".to_string(), 200);
    assert_eq!(eval_integer_with_scope(&nfi_expr(), &ctx, ""), Some(21));
}

#[test]
fn eval_for_stmt_break_exits_only_inner_loop() {
    let mut ctx = TypeCheckEvalContext::new();
    let statements = vec![Statement::For {
        indices: vec![ForIndex {
            ident: token("i"),
            range: Expression::Range {
                start: Arc::new(int_expr(1)),
                step: None,
                end: Arc::new(int_expr(3)),
            },
        }],
        equations: vec![
            Statement::Assignment {
                comp: cref("x"),
                value: cref_expr("i"),
            },
            Statement::Break {
                token: token("break"),
            },
        ],
    }];

    assert_eq!(
        interpret_stmts(&statements, &mut ctx),
        Some(FunctionStmtFlow::Continue)
    );
    assert_eq!(ctx.integers.get("x"), Some(&1));
}
