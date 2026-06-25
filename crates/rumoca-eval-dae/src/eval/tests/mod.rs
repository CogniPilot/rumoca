use super::*;
use crate::dual::Dual;
use indexmap::IndexMap;

mod scalar_eval_tests;
type BuiltinFunction = rumoca_core::BuiltinFunction;
type Expression = rumoca_core::Expression;
type Function = rumoca_core::Function;
type FunctionParam = rumoca_core::FunctionParam;
type OpBinary = rumoca_core::OpBinary;
type Reference = rumoca_core::Reference;
type Statement = rumoca_core::Statement;
type Subscript = rumoca_core::Subscript;
type VarName = rumoca_core::VarName;
mod clock_and_tables;
mod complex_array_selection;
mod env_refresh;
mod pre_seed_regressions;
mod runtime_specials_more;
mod shift_sample_value_form;
mod strict_eval_contract;
mod string_specials;
mod table_ad_edges;
mod vector_binary_ops;

fn eval_expr_value<T: SimFloat>(expr: &rumoca_core::Expression, env: &VarEnv<T>) -> T {
    match eval_expr(expr, env) {
        Ok(value) => value,
        Err(err) => panic!("test expression should evaluate: {err}"),
    }
}

fn env_value<T: SimFloat>(env: &VarEnv<T>, name: &str) -> T {
    match env.require(name) {
        Ok(value) => value,
        Err(err) => panic!("test env binding should exist: {err}"),
    }
}

fn lit(v: f64) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(v),
        span: rumoca_core::Span::DUMMY,
    }
}

fn int_lit(v: i64) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Integer(v),
        span: rumoca_core::Span::DUMMY,
    }
}

fn bool_lit(v: bool) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Boolean(v),
        span: rumoca_core::Span::DUMMY,
    }
}

fn dae_lit(v: f64) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(v),
        span: rumoca_core::Span::DUMMY,
    }
}

fn dae_bool_lit(v: bool) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Boolean(v),
        span: rumoca_core::Span::DUMMY,
    }
}

fn dae_var(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new(name),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    }
}

#[test]
fn var_scope_child_falls_through_and_shadows_without_parent_copy() {
    let mut parent = VarScope::new();
    parent.insert("a".to_string(), 1.0);
    parent.insert("b".to_string(), 2.0);

    let mut child = VarScope::child_of(&parent);
    child.insert("b".to_string(), 20.0);
    child.insert("c".to_string(), 3.0);

    assert_eq!(parent.get("b").copied(), Some(2.0));
    assert_eq!(child.get("a").copied(), Some(1.0));
    assert_eq!(child.get("b").copied(), Some(20.0));
    assert_eq!(child.get("c").copied(), Some(3.0));

    let entries = child
        .iter()
        .map(|(name, value)| (name.as_str(), *value))
        .collect::<Vec<_>>();
    assert_eq!(entries, vec![("a", 1.0), ("b", 20.0), ("c", 3.0)]);
}

fn var(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new(name),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    }
}

fn field(base: rumoca_core::Expression, field: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::FieldAccess {
        base: Box::new(base),
        field: field.to_string(),
        span: rumoca_core::Span::DUMMY,
    }
}

fn indexed_var(name: &str, indices: &[i64]) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new(name),
        subscripts: indices
            .iter()
            .copied()
            .map(|index| rumoca_core::Subscript::generated_index(index, rumoca_core::Span::DUMMY))
            .collect(),
        span: rumoca_core::Span::DUMMY,
    }
}

fn comp_ref(name: &str) -> rumoca_core::ComponentReference {
    rumoca_core::ComponentReference {
        local: false,
        span: rumoca_core::Span::DUMMY,
        parts: vec![rumoca_core::ComponentRefPart {
            ident: name.to_string(),
            span: rumoca_core::Span::DUMMY,
            subs: vec![],
        }],
        def_id: None,
    }
}

fn comp_ref_index(name: &str, index: i64) -> rumoca_core::ComponentReference {
    rumoca_core::ComponentReference {
        local: false,
        span: rumoca_core::Span::DUMMY,
        parts: vec![rumoca_core::ComponentRefPart {
            ident: name.to_string(),
            span: rumoca_core::Span::DUMMY,
            subs: vec![rumoca_core::Subscript::generated_index(
                index,
                rumoca_core::Span::DUMMY,
            )],
        }],
        def_id: None,
    }
}

fn arr(elements: Vec<rumoca_core::Expression>, is_matrix: bool) -> rumoca_core::Expression {
    rumoca_core::Expression::Array {
        elements,
        is_matrix,
        span: rumoca_core::Span::DUMMY,
    }
}

fn fn_call(name: &str, args: Vec<rumoca_core::Expression>) -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new(name),
        args,
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    }
}

fn named_ctor_arg(name: &str, value: rumoca_core::Expression) -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new(format!("__rumoca_named_arg__.{name}")),
        args: vec![value],
        is_constructor: true,
        span: rumoca_core::Span::DUMMY,
    }
}

fn builtin(
    function: rumoca_core::BuiltinFunction,
    args: Vec<rumoca_core::Expression>,
) -> rumoca_core::Expression {
    rumoca_core::Expression::BuiltinCall {
        function,
        args,
        span: rumoca_core::Span::DUMMY,
    }
}

fn set_vector_var<T: SimFloat>(env: &mut VarEnv<T>, name: &str, values: &[T]) {
    set_array_entries(env, name, &[values.len() as i64], values);
    env.dims = Arc::new(IndexMap::from([(
        name.to_string(),
        vec![values.len() as i64],
    )]));
}

fn simple_table_expr() -> rumoca_core::Expression {
    arr(
        vec![
            arr(vec![lit(0.0), lit(10.0)], false),
            arr(vec![lit(2.0), lit(14.0)], false),
        ],
        true,
    )
}

fn columns_expr() -> rumoca_core::Expression {
    arr(vec![int_lit(2)], false)
}

fn index_expr(base: rumoca_core::Expression, index: i64) -> rumoca_core::Expression {
    rumoca_core::Expression::Index {
        base: Box::new(base),
        subscripts: vec![rumoca_core::Subscript::generated_index(
            index,
            rumoca_core::Span::DUMMY,
        )],
        span: rumoca_core::Span::DUMMY,
    }
}

#[test]
fn strict_eval_accepts_array_literal_for_user_function_input() {
    let mut env = VarEnv::<f64>::new();
    let mut functions = IndexMap::new();
    let mut function = Function::new("Pkg.firstTwoSum", rumoca_core::Span::DUMMY);
    function.add_input(
        FunctionParam::new("X", "Real", rumoca_core::Span::source_free_serde_default())
            .with_dims(vec![0])
            .with_shape_expr(vec![Subscript::generated_colon(rumoca_core::Span::DUMMY)]),
    );
    function.add_output(FunctionParam::new(
        "y",
        "Real",
        rumoca_core::Span::source_free_serde_default(),
    ));
    function.body = vec![Statement::Assignment {
        comp: comp_ref("y"),
        value: Expression::Binary {
            op: OpBinary::Add,
            lhs: Box::new(index_expr(var("X"), 1)),
            rhs: Box::new(index_expr(var("X"), 2)),
            span: rumoca_core::Span::DUMMY,
        },
        span: rumoca_core::Span::DUMMY,
    }];
    functions.insert("Pkg.firstTwoSum".to_string(), function);
    env.functions = std::sync::Arc::new(functions);

    let expr = Expression::FunctionCall {
        name: Reference::new("Pkg.firstTwoSum"),
        args: vec![arr(vec![lit(1.25), lit(2.75)], false)],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(eval_expr(&expr, &env), Ok(4.0));
}

#[test]
fn function_record_output_field_array_preserves_constructor_matrix() {
    let mut env = VarEnv::<f64>::new();
    let mut functions = IndexMap::new();

    let mut orientation = Function::new("Pkg.Orientation", rumoca_core::Span::DUMMY);
    orientation.add_input(
        FunctionParam::new("T", "Real", rumoca_core::Span::source_free_serde_default())
            .with_dims(vec![3, 3]),
    );
    orientation.add_input(
        FunctionParam::new("w", "Real", rumoca_core::Span::source_free_serde_default())
            .with_dims(vec![3]),
    );
    functions.insert("Pkg.Orientation".to_string(), orientation);

    let mut from_q = Function::new("Pkg.from_Q", rumoca_core::Span::DUMMY);
    from_q.add_input(
        FunctionParam::new("Q", "Real", rumoca_core::Span::source_free_serde_default())
            .with_dims(vec![4]),
    );
    from_q.add_input(
        FunctionParam::new("w", "Real", rumoca_core::Span::source_free_serde_default())
            .with_dims(vec![3]),
    );
    from_q.add_output(
        FunctionParam::new(
            "R",
            "Orientation",
            rumoca_core::Span::source_free_serde_default(),
        )
        .with_type_class(rumoca_core::ClassType::Record),
    );
    from_q.body = vec![Statement::Assignment {
        comp: comp_ref("R"),
        value: Expression::FunctionCall {
            name: Reference::new("Pkg.Orientation"),
            args: vec![
                arr(
                    vec![
                        arr(
                            vec![
                                Expression::Binary {
                                    op: OpBinary::Sub,
                                    lhs: Box::new(Expression::Binary {
                                        op: OpBinary::Mul,
                                        lhs: Box::new(lit(2.0)),
                                        rhs: Box::new(Expression::Binary {
                                            op: OpBinary::Mul,
                                            lhs: Box::new(indexed_var("Q", &[4])),
                                            rhs: Box::new(indexed_var("Q", &[4])),
                                            span: rumoca_core::Span::DUMMY,
                                        }),
                                        span: rumoca_core::Span::DUMMY,
                                    }),
                                    rhs: Box::new(lit(1.0)),
                                    span: rumoca_core::Span::DUMMY,
                                },
                                lit(0.0),
                                lit(0.0),
                            ],
                            false,
                        ),
                        arr(vec![lit(0.0), lit(1.0), lit(0.0)], false),
                        arr(vec![lit(0.0), lit(0.0), lit(1.0)], false),
                    ],
                    true,
                ),
                named_ctor_arg("w", var("w")),
            ],
            is_constructor: true,
            span: rumoca_core::Span::DUMMY,
        },
        span: rumoca_core::Span::DUMMY,
    }];
    functions.insert("Pkg.from_Q".to_string(), from_q);
    env.functions = Arc::new(functions);
    set_array_entries(&mut env, "Q0", &[4], &[0.0, 0.0, 0.0, 1.0]);
    set_array_entries(&mut env, "w0", &[3], &[0.0, 0.0, 0.0]);

    let field = Expression::FieldAccess {
        base: Box::new(fn_call("Pkg.from_Q", vec![var("Q0"), var("w0")])),
        field: "T".to_string(),
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(
        eval_shaped_array_values::<f64>(&field, &env, 9),
        Ok(vec![1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0])
    );
}

#[test]
fn test_eval_cat_respects_matrix_column_dimension() {
    let expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Cat,
        args: vec![
            int_lit(2),
            arr(
                vec![
                    arr(vec![lit(1.0), lit(2.0)], false),
                    arr(vec![lit(3.0), lit(4.0)], false),
                ],
                true,
            ),
            arr(
                vec![arr(vec![lit(5.0)], false), arr(vec![lit(6.0)], false)],
                true,
            ),
        ],
        span: rumoca_core::Span::DUMMY,
    };

    // MLS §10.4.2.1: cat(2, A, B) concatenates along matrix columns.
    assert_eq!(
        eval_array_values::<f64>(&expr, &VarEnv::new()),
        Ok(vec![1.0, 2.0, 5.0, 3.0, 4.0, 6.0])
    );
}

fn simple_table_if_expr() -> rumoca_core::Expression {
    rumoca_core::Expression::If {
        branches: vec![(bool_lit(true), simple_table_expr())],
        else_branch: Box::new(arr(vec![arr(vec![lit(0.0), lit(0.0)], false)], true)),
        span: rumoca_core::Span::DUMMY,
    }
}

fn interaction_time_table_expr() -> rumoca_core::Expression {
    arr(
        vec![
            arr(vec![lit(0.0), lit(0.0)], false),
            arr(vec![lit(1.0), lit(2.1)], false),
            arr(vec![lit(2.0), lit(4.2)], false),
            arr(vec![lit(3.0), lit(6.3)], false),
            arr(vec![lit(4.0), lit(4.2)], false),
            arr(vec![lit(6.0), lit(2.1)], false),
        ],
        true,
    )
}

fn abs_expr(expr: rumoca_core::Expression) -> rumoca_core::Expression {
    rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Abs,
        args: vec![expr],
        span: rumoca_core::Span::DUMMY,
    }
}

fn table_entry(row: rumoca_core::Expression, col: i64) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new("table"),
        subscripts: vec![
            rumoca_core::Subscript::generated_expr(Box::new(row), rumoca_core::Span::DUMMY),
            rumoca_core::Subscript::generated_index(col, rumoca_core::Span::DUMMY),
        ],
        span: rumoca_core::Span::DUMMY,
    }
}

fn assign_stmt(name: &str, value: rumoca_core::Expression) -> rumoca_core::Statement {
    rumoca_core::Statement::Assignment {
        comp: comp_ref(name),
        value,

        span: rumoca_core::Span::DUMMY,
    }
}

fn statement_block(
    cond: rumoca_core::Expression,
    stmts: Vec<rumoca_core::Statement>,
) -> rumoca_core::StatementBlock {
    rumoca_core::StatementBlock { cond, stmts }
}

fn interaction_time_table_locals() -> Vec<rumoca_core::FunctionParam> {
    vec![
        rumoca_core::FunctionParam::new(
            "columns",
            "Integer",
            rumoca_core::Span::source_free_serde_default(),
        )
        .with_default(int_lit(2)),
        rumoca_core::FunctionParam::new(
            "ncol",
            "Integer",
            rumoca_core::Span::source_free_serde_default(),
        )
        .with_default(int_lit(2)),
        rumoca_core::FunctionParam::new(
            "nrow",
            "Integer",
            rumoca_core::Span::source_free_serde_default(),
        )
        .with_default(rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Size,
            args: vec![var("table"), int_lit(1)],
            span: rumoca_core::Span::DUMMY,
        }),
        rumoca_core::FunctionParam::new(
            "next0",
            "Integer",
            rumoca_core::Span::source_free_serde_default(),
        ),
        rumoca_core::FunctionParam::new(
            "tp",
            "Real",
            rumoca_core::Span::source_free_serde_default(),
        ),
        rumoca_core::FunctionParam::new(
            "dt",
            "Real",
            rumoca_core::Span::source_free_serde_default(),
        ),
    ]
}

fn interaction_time_table_pre_start_block() -> rumoca_core::StatementBlock {
    statement_block(
        binop(OpBinary::Lt, var("tp"), var("startTimeScaled")),
        vec![
            assign_stmt("nextEventScaled", var("startTimeScaled")),
            assign_stmt("a", lit(0.0)),
            assign_stmt("b", var("offset")),
        ],
    )
}

fn interaction_time_table_single_row_block() -> rumoca_core::StatementBlock {
    statement_block(
        binop(OpBinary::Lt, var("nrow"), int_lit(2)),
        vec![
            assign_stmt("a", lit(0.0)),
            assign_stmt(
                "b",
                binop(OpBinary::Add, var("offset"), table_entry(int_lit(1), 2)),
            ),
        ],
    )
}

fn interaction_time_table_dt_if() -> rumoca_core::Statement {
    rumoca_core::Statement::If {
        cond_blocks: vec![statement_block(
            binop(
                OpBinary::Le,
                var("dt"),
                binop(
                    OpBinary::Mul,
                    var("TimeEps"),
                    abs_expr(table_entry(var("next"), 1)),
                ),
            ),
            vec![
                assign_stmt("a", lit(0.0)),
                assign_stmt(
                    "b",
                    binop(OpBinary::Add, var("offset"), table_entry(var("next"), 2)),
                ),
            ],
        )],
        else_block: Some(vec![
            assign_stmt(
                "a",
                binop(
                    OpBinary::Div,
                    binop(
                        OpBinary::Sub,
                        table_entry(var("next"), 2),
                        table_entry(var("next0"), 2),
                    ),
                    var("dt"),
                ),
            ),
            assign_stmt(
                "b",
                binop(
                    OpBinary::Sub,
                    binop(OpBinary::Add, var("offset"), table_entry(var("next0"), 2)),
                    binop(OpBinary::Mul, var("a"), table_entry(var("next0"), 1)),
                ),
            ),
        ]),

        span: rumoca_core::Span::DUMMY,
    }
}

fn interaction_time_table_active_statements() -> Vec<rumoca_core::Statement> {
    vec![
        assign_stmt(
            "tp",
            binop(OpBinary::Sub, var("tp"), var("shiftTimeScaled")),
        ),
        rumoca_core::Statement::While {
            block: statement_block(
                binop(
                    OpBinary::And,
                    binop(OpBinary::Lt, var("next"), var("nrow")),
                    binop(OpBinary::Ge, var("tp"), table_entry(var("next"), 1)),
                ),
                vec![assign_stmt(
                    "next",
                    binop(OpBinary::Add, var("next"), int_lit(1)),
                )],
            ),
            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Statement::If {
            cond_blocks: vec![statement_block(
                binop(OpBinary::Lt, var("next"), var("nrow")),
                vec![assign_stmt(
                    "nextEventScaled",
                    binop(
                        OpBinary::Add,
                        var("shiftTimeScaled"),
                        table_entry(var("next"), 1),
                    ),
                )],
            )],
            else_block: None,

            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Statement::If {
            cond_blocks: vec![statement_block(
                binop(OpBinary::Eq, var("next"), int_lit(1)),
                vec![assign_stmt("next", int_lit(2))],
            )],
            else_block: None,

            span: rumoca_core::Span::DUMMY,
        },
        assign_stmt("next0", binop(OpBinary::Sub, var("next"), int_lit(1))),
        assign_stmt(
            "dt",
            binop(
                OpBinary::Sub,
                table_entry(var("next"), 1),
                table_entry(var("next0"), 1),
            ),
        ),
        interaction_time_table_dt_if(),
    ]
}

fn interaction_time_table_body() -> Vec<rumoca_core::Statement> {
    vec![
        assign_stmt("next", var("last")),
        assign_stmt(
            "nextEventScaled",
            binop(
                OpBinary::Sub,
                var("timeScaled"),
                binop(OpBinary::Mul, var("TimeEps"), abs_expr(var("timeScaled"))),
            ),
        ),
        assign_stmt(
            "tp",
            binop(
                OpBinary::Add,
                var("timeScaled"),
                binop(OpBinary::Mul, var("TimeEps"), abs_expr(var("timeScaled"))),
            ),
        ),
        rumoca_core::Statement::If {
            cond_blocks: vec![interaction_time_table_pre_start_block()],
            else_block: Some(vec![rumoca_core::Statement::If {
                cond_blocks: vec![interaction_time_table_single_row_block()],
                else_block: Some(interaction_time_table_active_statements()),
                span: rumoca_core::Span::DUMMY,
            }]),

            span: rumoca_core::Span::DUMMY,
        },
        assign_stmt(
            "b",
            binop(
                OpBinary::Sub,
                var("b"),
                binop(OpBinary::Mul, var("a"), var("shiftTimeScaled")),
            ),
        ),
    ]
}

fn interaction_time_table_coeff_function() -> rumoca_core::Function {
    let mut f = rumoca_core::Function::new(
        "Modelica.Blocks.Sources.TimeTable.getInterpolationCoefficients",
        rumoca_core::Span::DUMMY,
    );
    f.add_input(
        rumoca_core::FunctionParam::new(
            "table",
            "Real",
            rumoca_core::Span::source_free_serde_default(),
        )
        .with_dims(vec![6, 2]),
    );
    f.add_input(rumoca_core::FunctionParam::new(
        "offset",
        "Real",
        rumoca_core::Span::source_free_serde_default(),
    ));
    f.add_input(rumoca_core::FunctionParam::new(
        "startTimeScaled",
        "Real",
        rumoca_core::Span::source_free_serde_default(),
    ));
    f.add_input(rumoca_core::FunctionParam::new(
        "timeScaled",
        "Real",
        rumoca_core::Span::source_free_serde_default(),
    ));
    f.add_input(rumoca_core::FunctionParam::new(
        "last",
        "Integer",
        rumoca_core::Span::source_free_serde_default(),
    ));
    f.add_input(rumoca_core::FunctionParam::new(
        "TimeEps",
        "Real",
        rumoca_core::Span::source_free_serde_default(),
    ));
    f.add_input(rumoca_core::FunctionParam::new(
        "shiftTimeScaled",
        "Real",
        rumoca_core::Span::source_free_serde_default(),
    ));
    f.add_output(rumoca_core::FunctionParam::new(
        "a",
        "Real",
        rumoca_core::Span::source_free_serde_default(),
    ));
    f.add_output(rumoca_core::FunctionParam::new(
        "b",
        "Real",
        rumoca_core::Span::source_free_serde_default(),
    ));
    f.add_output(rumoca_core::FunctionParam::new(
        "nextEventScaled",
        "Real",
        rumoca_core::Span::source_free_serde_default(),
    ));
    f.add_output(rumoca_core::FunctionParam::new(
        "next",
        "Integer",
        rumoca_core::Span::source_free_serde_default(),
    ));
    for local in interaction_time_table_locals() {
        f.add_local(local);
    }
    f.body = interaction_time_table_body();
    f
}

fn eval_table1d_dual(u: Dual, extrapolation: i64) -> Dual {
    let mut env = VarEnv::<Dual>::new();
    let constructor = fn_call(
        "ExternalCombiTable1D",
        vec![
            lit(0.0),
            lit(0.0),
            simple_table_expr(),
            columns_expr(),
            int_lit(1), // LinearSegments
            int_lit(extrapolation),
        ],
    );
    let table_id = eval_expr_value::<Dual>(&constructor, &env).real();
    assert!(table_id > 0.0);
    env.set("table_id", Dual::from_f64(table_id));
    env.set("u", u);
    eval_expr_value::<Dual>(
        &fn_call(
            "getTable1DValueNoDer",
            vec![var("table_id"), int_lit(1), var("u")],
        ),
        &env,
    )
}

fn eval_timetable_dual(t: Dual, extrapolation: i64) -> Dual {
    let mut env = VarEnv::<Dual>::new();
    let constructor = fn_call(
        "ExternalCombiTimeTable",
        vec![
            lit(0.0),
            lit(0.0),
            simple_table_expr(),
            lit(0.0), // startTime
            columns_expr(),
            int_lit(1), // LinearSegments
            int_lit(extrapolation),
        ],
    );
    let table_id = eval_expr_value::<Dual>(&constructor, &env).real();
    assert!(table_id > 0.0);
    env.set("table_id", Dual::from_f64(table_id));
    env.set("t", t);
    eval_expr_value::<Dual>(
        &fn_call(
            "getTimeTableValueNoDer",
            vec![var("table_id"), int_lit(1), var("t"), lit(0.0), lit(0.0)],
        ),
        &env,
    )
}

#[test]
fn test_eval_index_on_matrix_literal() {
    let env = VarEnv::<f64>::new();
    let expr = rumoca_core::Expression::Index {
        base: Box::new(simple_table_expr()),
        subscripts: vec![
            rumoca_core::Subscript::generated_index(2, rumoca_core::Span::DUMMY),
            rumoca_core::Subscript::generated_index(1, rumoca_core::Span::DUMMY),
        ],
        span: rumoca_core::Span::DUMMY,
    };
    let value = eval_expr_value::<f64>(&expr, &env);
    assert!((value - 2.0).abs() < 1e-12);
}

#[test]
fn test_eval_index_on_flattened_env_array_with_dims() {
    let mut env = VarEnv::<f64>::new();
    env.dims = Arc::new(IndexMap::from([("A".to_string(), vec![3, 3])]));
    for i in 1..=9 {
        env.set(&format!("A[{i}]"), i as f64);
    }

    let expr = rumoca_core::Expression::Index {
        base: Box::new(var("A")),
        subscripts: vec![
            rumoca_core::Subscript::generated_index(2, rumoca_core::Span::DUMMY),
            rumoca_core::Subscript::generated_index(3, rumoca_core::Span::DUMMY),
        ],
        span: rumoca_core::Span::DUMMY,
    };
    let value = eval_expr_value::<f64>(&expr, &env);
    assert!((value - 6.0).abs() < 1e-12);
}

#[test]
fn test_eval_array_values_var_ref_colon_slice_from_env() {
    let mut env = VarEnv::<f64>::new();
    set_array_entries(&mut env, "v", &[3], &[10.0, 20.0, 30.0]);

    let expr = rumoca_core::Expression::VarRef {
        name: Reference::new("v"),
        subscripts: vec![Subscript::generated_colon(rumoca_core::Span::DUMMY)],
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(
        eval_array_values::<f64>(&expr, &env),
        Ok(vec![10.0, 20.0, 30.0])
    );
}

#[test]
fn test_eval_array_values_var_ref_range_slice_from_env() {
    let mut env = VarEnv::<f64>::new();
    set_array_entries(&mut env, "v", &[4], &[10.0, 20.0, 30.0, 40.0]);

    let expr = rumoca_core::Expression::VarRef {
        name: Reference::new("v"),
        subscripts: vec![Subscript::generated_expr(
            Box::new(rumoca_core::Expression::Range {
                start: Box::new(int_lit(2)),
                step: None,
                end: Box::new(int_lit(3)),
                span: rumoca_core::Span::DUMMY,
            }),
            rumoca_core::Span::DUMMY,
        )],
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(eval_array_values::<f64>(&expr, &env), Ok(vec![20.0, 30.0]));
}

#[test]
fn test_eval_array_values_index_range_slice_from_array_expr() {
    let expr = rumoca_core::Expression::Index {
        base: Box::new(arr(vec![lit(1.0), lit(2.0), lit(3.0), lit(4.0)], false)),
        subscripts: vec![Subscript::generated_expr(
            Box::new(rumoca_core::Expression::Range {
                start: Box::new(int_lit(2)),
                step: None,
                end: Box::new(int_lit(3)),
                span: rumoca_core::Span::DUMMY,
            }),
            rumoca_core::Span::DUMMY,
        )],
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(
        eval_array_values::<f64>(&expr, &VarEnv::new()),
        Ok(vec![2.0, 3.0])
    );
}

#[test]
fn test_eval_index_on_transposed_env_matrix_with_dims() {
    let mut env = VarEnv::<f64>::new();
    env.dims = Arc::new(IndexMap::from([("R".to_string(), vec![3, 3])]));
    set_array_entries(
        &mut env,
        "R",
        &[3, 3],
        &[1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0],
    );

    let expr = rumoca_core::Expression::Index {
        base: Box::new(rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Transpose,
            args: vec![var("R")],
            span: rumoca_core::Span::DUMMY,
        }),
        subscripts: vec![
            rumoca_core::Subscript::generated_index(2, rumoca_core::Span::DUMMY),
            rumoca_core::Subscript::generated_index(3, rumoca_core::Span::DUMMY),
        ],
        span: rumoca_core::Span::DUMMY,
    };
    let value = eval_expr_value::<f64>(&expr, &env);
    assert!((value - 8.0).abs() < 1e-12);
}

#[test]
fn test_eval_array_values_transposed_matrix_vector_product() {
    let mut env = VarEnv::<f64>::new();
    env.dims = Arc::new(IndexMap::from([
        ("R".to_string(), vec![3, 3]),
        ("v".to_string(), vec![3]),
    ]));
    set_array_entries(
        &mut env,
        "R",
        &[3, 3],
        &[1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0],
    );
    set_array_entries(&mut env, "v", &[3], &[10.0, 20.0, 30.0]);

    let expr = rumoca_core::Expression::Binary {
        op: OpBinary::Mul,
        lhs: Box::new(rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Transpose,
            args: vec![var("R")],
            span: rumoca_core::Span::DUMMY,
        }),
        rhs: Box::new(var("v")),
        span: rumoca_core::Span::DUMMY,
    };
    let values = eval_array_values::<f64>(&expr, &env);
    assert_eq!(values, Ok(vec![300.0, 360.0, 420.0]));
    assert!((eval_expr_value::<f64>(&expr, &env) - 300.0).abs() < 1e-12);
}

#[test]
fn test_eval_array_values_matrix_matrix_product() {
    let mut env = VarEnv::<f64>::new();
    env.dims = Arc::new(IndexMap::from([
        ("A".to_string(), vec![2, 3]),
        ("B".to_string(), vec![3, 2]),
    ]));
    set_array_entries(&mut env, "A", &[2, 3], &[1.0, 2.0, 3.0, 4.0, 5.0, 6.0]);
    set_array_entries(&mut env, "B", &[3, 2], &[7.0, 8.0, 9.0, 10.0, 11.0, 12.0]);

    let expr = rumoca_core::Expression::Binary {
        op: OpBinary::Mul,
        lhs: Box::new(var("A")),
        rhs: Box::new(var("B")),
        span: rumoca_core::Span::DUMMY,
    };
    let values = eval_array_values::<f64>(&expr, &env);
    assert_eq!(values, Ok(vec![58.0, 64.0, 139.0, 154.0]));
    assert!((eval_expr_value::<f64>(&expr, &env) - 58.0).abs() < 1e-12);
}

#[test]
fn test_eval_array_values_diagonal_preserves_matrix_shape() {
    let expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Diagonal,
        args: vec![rumoca_core::Expression::Array {
            elements: vec![lit(1.0), lit(2.0), lit(3.0)],
            is_matrix: false,
            span: rumoca_core::Span::DUMMY,
        }],
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(
        eval_array_values::<f64>(&expr, &VarEnv::new()),
        Ok(vec![1.0, 0.0, 0.0, 0.0, 2.0, 0.0, 0.0, 0.0, 3.0])
    );
    assert_eq!(
        eval_shaped_array_values::<f64>(&expr, &VarEnv::new(), 9)
            .expect("diagonal should produce a shaped matrix"),
        vec![1.0, 0.0, 0.0, 0.0, 2.0, 0.0, 0.0, 0.0, 3.0]
    );
}

#[test]
fn test_eval_array_values_does_not_promote_scalar_start_expr_to_array() {
    let mut env = VarEnv::<f64>::new();
    env.set("s", 2.0);
    env.dims = Arc::new(IndexMap::from([("v".to_string(), vec![3])]));
    env.start_exprs = Arc::new(IndexMap::from([(
        "s".to_string(),
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Vector,
            args: vec![var("v")],
            span: rumoca_core::Span::DUMMY,
        },
    )]));
    set_array_entries(&mut env, "v", &[3], &[1.0, 2.0, 3.0]);

    assert_eq!(eval_array_values::<f64>(&var("s"), &env), Ok(vec![2.0]));
}

#[test]
fn test_eval_array_values_vector_arithmetic_preserves_shape() {
    let mut env = VarEnv::<f64>::new();
    env.dims = Arc::new(IndexMap::from([
        ("a".to_string(), vec![3]),
        ("b".to_string(), vec![3]),
    ]));
    set_array_entries(&mut env, "a", &[3], &[1.0, 2.0, 3.0]);
    set_array_entries(&mut env, "b", &[3], &[4.0, 5.0, 6.0]);

    let expr = binop(
        OpBinary::Add,
        binop(OpBinary::Mul, var("a"), lit(2.0)),
        binop(OpBinary::Sub, var("b"), lit(1.0)),
    );
    let values = eval_array_values::<f64>(&expr, &env);
    assert_eq!(values, Ok(vec![5.0, 8.0, 11.0]));
    assert_eq!(
        eval_shaped_array_values(&expr, &env, 3).expect("vector expression should keep shape"),
        vec![5.0, 8.0, 11.0]
    );
}

#[test]
fn test_eval_array_values_unary_vector_arithmetic_preserves_shape() {
    let mut env = VarEnv::<f64>::new();
    env.dims = Arc::new(IndexMap::from([("axis".to_string(), vec![3])]));
    set_array_entries(&mut env, "axis", &[3], &[1.0, 2.0, 3.0]);

    let expr = unary(
        rumoca_core::OpUnary::Minus,
        binop(rumoca_core::OpBinary::Mul, lit(2.0), var("axis")),
    );

    assert_eq!(
        eval_array_values::<f64>(&expr, &env),
        Ok(vec![-2.0, -4.0, -6.0])
    );
    assert_eq!(
        eval_shaped_array_values::<f64>(&expr, &env, 3)
            .expect("unary vector expression should keep shape"),
        vec![-2.0, -4.0, -6.0]
    );
}

#[test]
fn test_eval_array_values_vectorizes_scalar_function_call() {
    let mut env = VarEnv::<f64>::new();
    env.dims = Arc::new(IndexMap::from([
        ("a".to_string(), vec![3]),
        ("b".to_string(), vec![3]),
    ]));
    set_array_entries(&mut env, "a", &[3], &[4.0, 5.0, 6.0]);
    set_array_entries(&mut env, "b", &[3], &[1.0, 2.0, 3.0]);

    let mut f = Function::new("Pkg.toUnit", rumoca_core::Span::DUMMY);
    f.add_input(FunctionParam::new(
        "u",
        "Real",
        rumoca_core::Span::source_free_serde_default(),
    ));
    f.add_output(
        FunctionParam::new("y", "Real", rumoca_core::Span::source_free_serde_default())
            .with_default(var("u")),
    );
    f.body = vec![Statement::Empty {
        span: rumoca_core::Span::DUMMY,
    }];
    env.functions = Arc::new(IndexMap::from([("Pkg.toUnit".to_string(), f)]));

    let expr = fn_call("Pkg.toUnit", vec![binop(OpBinary::Sub, var("a"), var("b"))]);
    assert_eq!(
        eval_array_values::<f64>(&expr, &env),
        Ok(vec![3.0, 3.0, 3.0])
    );
    assert_eq!(
        eval_shaped_array_values(&expr, &env, 3).expect("vectorized scalar function should shape"),
        vec![3.0, 3.0, 3.0]
    );
}

#[test]
fn test_eval_function_array_output_preserves_local_array_default_shape() {
    let mut env = VarEnv::<f64>::new();
    let mut function = Function::new("Pkg.matrixFromLocalDefault", rumoca_core::Span::DUMMY);
    function.add_output(
        FunctionParam::new("y", "Real", rumoca_core::Span::source_free_serde_default())
            .with_dims(vec![2, 2]),
    );
    function.locals.push(
        FunctionParam::new(
            "col",
            "Real",
            rumoca_core::Span::source_free_serde_default(),
        )
        .with_dims(vec![2])
        .with_default(rumoca_core::Expression::Array {
            elements: vec![lit(1.0), lit(2.0)],
            is_matrix: false,
            span: rumoca_core::Span::DUMMY,
        }),
    );
    function.body = vec![Statement::Assignment {
        comp: comp_ref("y"),
        value: rumoca_core::Expression::Array {
            elements: vec![var("col"), var("col")],
            is_matrix: false,
            span: rumoca_core::Span::DUMMY,
        },
        span: rumoca_core::Span::DUMMY,
    }];
    env.functions = Arc::new(IndexMap::from([(
        "Pkg.matrixFromLocalDefault".to_string(),
        function,
    )]));

    let expr = fn_call("Pkg.matrixFromLocalDefault", Vec::new());
    assert_eq!(
        eval_shaped_array_values::<f64>(&expr, &env, 4)
            .expect("function local array default should remain shaped"),
        vec![1.0, 2.0, 1.0, 2.0]
    );
}

#[test]
fn test_eval_array_values_dynamic_function_output_uses_shape_expr() {
    let mut env = VarEnv::<f64>::new();
    let mut function = Function::new("Pkg.dynamicVector", rumoca_core::Span::DUMMY);
    function.add_input(FunctionParam::new(
        "m",
        "Integer",
        rumoca_core::Span::source_free_serde_default(),
    ));
    function.add_output(
        FunctionParam::new("y", "Real", rumoca_core::Span::source_free_serde_default())
            .with_dims(vec![0])
            .with_shape_expr(vec![Subscript::generated_expr(
                Box::new(var("m")),
                rumoca_core::Span::DUMMY,
            )]),
    );
    function.body = vec![Statement::Assignment {
        comp: comp_ref("y"),
        value: rumoca_core::Expression::ArrayComprehension {
            expr: Box::new(var("k")),
            indices: vec![rumoca_core::ComprehensionIndex {
                name: "k".to_string(),
                range: rumoca_core::Expression::Range {
                    start: Box::new(int_lit(1)),
                    step: None,
                    end: Box::new(var("m")),
                    span: rumoca_core::Span::DUMMY,
                },
            }],
            filter: None,
            span: rumoca_core::Span::DUMMY,
        },
        span: rumoca_core::Span::DUMMY,
    }];
    env.functions = Arc::new(IndexMap::from([(
        "Pkg.dynamicVector".to_string(),
        function,
    )]));

    let call = fn_call("Pkg.dynamicVector", vec![int_lit(3)]);
    assert_eq!(
        eval_array_values::<f64>(&call, &env),
        Ok(vec![1.0, 2.0, 3.0])
    );

    let negated = unary(rumoca_core::OpUnary::Minus, call);
    assert_eq!(
        eval_shaped_array_values::<f64>(&negated, &env, 3)
            .expect("dynamic function output should keep its runtime shape"),
        vec![-1.0, -2.0, -3.0]
    );
}

#[test]
fn test_eval_function_dynamic_vector_input_binds_expression_shape() {
    let mut env = VarEnv::<f64>::new();
    env.dims = Arc::new(IndexMap::from([
        ("a".to_string(), vec![3]),
        ("b".to_string(), vec![3]),
    ]));
    set_array_entries(&mut env, "a", &[3], &[1.0, 2.0, 3.0]);
    set_array_entries(&mut env, "b", &[3], &[4.0, 5.0, 6.0]);

    let mut function = Function::new("Pkg.normalizeLike", rumoca_core::Span::DUMMY);
    function.add_input(
        FunctionParam::new("v", "Real", rumoca_core::Span::source_free_serde_default())
            .with_dims(vec![0])
            .with_shape_expr(vec![Subscript::generated_colon(rumoca_core::Span::DUMMY)]),
    );
    function.add_output(
        FunctionParam::new(
            "result",
            "Real",
            rumoca_core::Span::source_free_serde_default(),
        )
        .with_dims(vec![0])
        .with_shape_expr(vec![Subscript::generated_expr(
            Box::new(builtin(BuiltinFunction::Size, vec![var("v"), int_lit(1)])),
            rumoca_core::Span::DUMMY,
        )]),
    );
    function.body = vec![Statement::Assignment {
        comp: comp_ref("result"),
        value: var("v"),
        span: rumoca_core::Span::DUMMY,
    }];
    env.functions = Arc::new(IndexMap::from([(
        "Pkg.normalizeLike".to_string(),
        function,
    )]));

    let cross = builtin(BuiltinFunction::Cross, vec![var("a"), var("b")]);
    let expr = fn_call("Pkg.normalizeLike", vec![cross]);
    assert_eq!(
        eval_shaped_array_values::<f64>(&expr, &env, 3)
            .expect("dynamic input shape should come from the array expression"),
        vec![-3.0, 6.0, -3.0]
    );
}

#[test]
fn test_eval_array_values_cross_product() {
    let mut env = VarEnv::<f64>::new();
    env.dims = Arc::new(IndexMap::from([
        ("a".to_string(), vec![3]),
        ("b".to_string(), vec![3]),
    ]));
    set_array_entries(&mut env, "a", &[3], &[1.0, 2.0, 3.0]);
    set_array_entries(&mut env, "b", &[3], &[4.0, 5.0, 6.0]);

    let expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Cross,
        args: vec![var("a"), var("b")],
        span: rumoca_core::Span::DUMMY,
    };
    let values = eval_array_values::<f64>(&expr, &env);
    assert_eq!(values, Ok(vec![-3.0, 6.0, -3.0]));
    assert!((eval_expr_value::<f64>(&expr, &env) + 3.0).abs() < 1e-12);
}

#[test]
fn test_eval_array_values_expands_range() {
    let env = VarEnv::<f64>::new();
    let ascending = rumoca_core::Expression::Range {
        start: Box::new(int_lit(1)),
        step: None,
        end: Box::new(int_lit(4)),
        span: rumoca_core::Span::DUMMY,
    };
    let descending = rumoca_core::Expression::Range {
        start: Box::new(int_lit(4)),
        step: None,
        end: Box::new(int_lit(1)),
        span: rumoca_core::Span::DUMMY,
    };

    let up = eval_array_values::<f64>(&ascending, &env);
    let down = eval_array_values::<f64>(&descending, &env);
    assert_eq!(up, Ok(vec![1.0, 2.0, 3.0, 4.0]));
    assert_eq!(down, Ok(vec![4.0, 3.0, 2.0, 1.0]));
}

fn user_function_with_default_output(name: &str, output_value: f64) -> rumoca_core::Function {
    let mut func = rumoca_core::Function::new(name, rumoca_core::Span::DUMMY);
    func.add_output(
        rumoca_core::FunctionParam::new(
            "y",
            "Real",
            rumoca_core::Span::source_free_serde_default(),
        )
        .with_default(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(output_value),
            span: rumoca_core::Span::DUMMY,
        }),
    );
    // Non-empty body is required for function-body evaluation path.
    func.body = vec![rumoca_core::Statement::Empty {
        span: rumoca_core::Span::DUMMY,
    }];
    func
}

fn binop(
    op: rumoca_core::OpBinary,
    lhs: rumoca_core::Expression,
    rhs: rumoca_core::Expression,
) -> rumoca_core::Expression {
    rumoca_core::Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn unary(op: rumoca_core::OpUnary, rhs: rumoca_core::Expression) -> rumoca_core::Expression {
    rumoca_core::Expression::Unary {
        op,
        rhs: Box::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

#[test]
fn test_eval_literal_real() {
    assert_eq!(eval_expr_value::<f64>(&lit(3.125), &VarEnv::new()), 3.125);
}

#[test]
fn test_eval_literal_integer() {
    assert_eq!(eval_expr_value::<f64>(&int_lit(42), &VarEnv::new()), 42.0);
}

#[test]
fn test_eval_literal_boolean() {
    assert_eq!(eval_expr_value::<f64>(&bool_lit(true), &VarEnv::new()), 1.0);
    assert_eq!(
        eval_expr_value::<f64>(&bool_lit(false), &VarEnv::new()),
        0.0
    );
}

#[test]
fn test_eval_var_ref() {
    let mut env = VarEnv::<f64>::new();
    env.set("x", 2.5);
    assert_eq!(eval_expr_value::<f64>(&var("x"), &env), 2.5);
}

#[test]
fn test_eval_var_ref_missing() {
    assert_eq!(
        eval_expr::<f64>(&var("missing"), &VarEnv::new()),
        Err(EvalError::MissingBinding {
            name: "missing".to_string()
        })
    );
}

#[test]
fn test_eval_expr_rejects_missing_var_ref() {
    assert_eq!(
        eval_expr::<f64>(&var("missing"), &VarEnv::new()),
        Err(EvalError::MissingBinding {
            name: "missing".to_string(),
        })
    );
}

#[test]
fn test_eval_expr_rejects_unsupported_scalar_forms() {
    let range = rumoca_core::Expression::Range {
        start: Box::new(int_lit(1)),
        step: None,
        end: Box::new(int_lit(3)),
        span: rumoca_core::Span::DUMMY,
    };
    assert_eq!(
        eval_expr::<f64>(&range, &VarEnv::new()),
        Err(EvalError::UnsupportedExpression { kind: "range" })
    );
}

#[test]
fn test_eval_var_ref_resolves_enum_literal_ordinal() {
    let mut env = VarEnv::<f64>::new();
    env.enum_literal_ordinals = Arc::new(IndexMap::from([(
        "Modelica.Electrical.Digital.Interfaces.Logic.'1'".to_string(),
        4,
    )]));
    assert_eq!(
        eval_expr_value::<f64>(
            &var("Modelica.Electrical.Digital.Interfaces.Logic.'1'"),
            &env
        ),
        4.0
    );
}

#[test]
fn test_eval_var_ref_resolves_enum_literal_ordinal_without_quotes_in_table() {
    let mut env = VarEnv::<f64>::new();
    env.enum_literal_ordinals = Arc::new(IndexMap::from([(
        "Modelica.Electrical.Digital.Interfaces.Logic.1".to_string(),
        4,
    )]));
    assert_eq!(
        eval_expr_value::<f64>(
            &var("Modelica.Electrical.Digital.Interfaces.Logic.'1'"),
            &env
        ),
        4.0
    );
}

#[test]
fn test_eval_var_ref_resolves_enum_literal_ordinal_with_quotes_in_table() {
    let mut env = VarEnv::<f64>::new();
    env.enum_literal_ordinals = Arc::new(IndexMap::from([(
        "Modelica.Electrical.Digital.Interfaces.Logic.'1'".to_string(),
        4,
    )]));
    assert_eq!(
        eval_expr_value::<f64>(&var("Modelica.Electrical.Digital.Interfaces.Logic.1"), &env),
        4.0
    );
}

#[test]
fn test_eval_var_ref_resolves_unambiguous_local_enum_alias_literal() {
    let mut env = VarEnv::<f64>::new();
    env.enum_literal_ordinals = Arc::new(IndexMap::from([
        (
            "Modelica.Electrical.Digital.Interfaces.Logic.'U'".to_string(),
            1,
        ),
        ("Logic.'U'".to_string(), 1),
        (
            "Modelica.Electrical.Digital.Interfaces.UX01.'U'".to_string(),
            1,
        ),
        ("UX01.'U'".to_string(), 1),
    ]));
    assert_eq!(
        eval_expr_value::<f64>(&var("iNV3S.inertialDelaySensitive.L.'U'"), &env),
        1.0
    );
}

#[test]
fn test_eval_var_ref_rejects_ambiguous_local_enum_alias_literal() {
    let mut env = VarEnv::<f64>::new();
    env.enum_literal_ordinals = Arc::new(IndexMap::from([
        ("Pkg.A.'Open'".to_string(), 1),
        ("Pkg.B.'Open'".to_string(), 2),
    ]));
    assert_eq!(
        eval_expr::<f64>(&var("component.Local.'Open'"), &env),
        Err(EvalError::MissingBinding {
            name: "component.Local.'Open'".to_string()
        })
    );
}

#[test]
fn test_map_var_to_env_size1_array_populates_indexed_alias() {
    let mut env = VarEnv::<f64>::new();
    let mut idx = 0usize;
    let mut arr1 = rumoca_ir_dae::Variable::new(
        rumoca_core::VarName::new("arr1"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    arr1.dims = vec![1];
    map_var_to_env(&mut env, "arr1", &arr1, &[2.5], &mut idx);
    assert_eq!(idx, 1);
    assert!((env_value(&env, "arr1") - 2.5).abs() < 1e-12);
    assert!((env_value(&env, "arr1[1]") - 2.5).abs() < 1e-12);
}

#[test]
fn test_build_env_seeds_discrete_start_values() {
    let mut dae = rumoca_ir_dae::Dae::default();
    let mut off = rumoca_ir_dae::Variable::new(
        rumoca_core::VarName::new("off"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    off.start = Some(dae_bool_lit(true));
    dae.variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("off"), off);

    let mut z = rumoca_ir_dae::Variable::new(
        rumoca_core::VarName::new("z"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    z.start = Some(dae_lit(2.5));
    dae.variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("z"), z);

    let env = build_runtime_parameter_tail_env(&dae, &[], 0.0).expect("test env should build");
    assert_eq!(env_value(&env, "off"), 1.0);
    assert!((env_value(&env, "z") - 2.5).abs() < 1e-12);
}

#[test]
fn test_build_env_seeds_fill_start_sized_by_string_array_literal() {
    let substance_names = rumoca_core::Expression::Array {
        elements: vec![
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::String("N2".to_string()),
                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::String("O2".to_string()),
                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::String("H2O".to_string()),
                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::String("CO2".to_string()),
                span: rumoca_core::Span::DUMMY,
            },
        ],
        is_matrix: false,
        span: rumoca_core::Span::DUMMY,
    };
    let size = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Size,
        args: vec![substance_names, int_lit(1)],
        span: rumoca_core::Span::DUMMY,
    };
    let mut dae = rumoca_ir_dae::Dae::default();
    let mut x = rumoca_ir_dae::Variable::new(
        rumoca_core::VarName::new("X"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    x.dims = vec![4];
    x.start = Some(rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Fill,
        args: vec![
            binop(
                rumoca_core::OpBinary::Div,
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(1),
                    span: rumoca_core::Span::DUMMY,
                },
                size.clone(),
            ),
            size,
        ],
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables.parameters.insert("X".into(), x);

    let env = build_runtime_parameter_tail_env(&dae, &[], 0.0).expect("test env should build");
    assert_eq!(
        eval_shaped_array_values::<f64>(&var("X"), &env, 4),
        Ok(vec![0.25; 4])
    );
}

#[test]
fn test_build_env_seeds_sum_size_start_for_string_array_literal() {
    let substance_names = arr(
        vec![
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::String("N2".to_string()),
                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::String("O2".to_string()),
                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::String("H2O".to_string()),
                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::String("CO2".to_string()),
                span: rumoca_core::Span::DUMMY,
            },
        ],
        false,
    );
    let size = builtin(rumoca_core::BuiltinFunction::Size, vec![substance_names]);
    let mut dae = rumoca_ir_dae::Dae::default();
    let mut n = rumoca_ir_dae::Variable::new(
        rumoca_core::VarName::new("n"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    n.start = Some(builtin(rumoca_core::BuiltinFunction::Sum, vec![size]));
    dae.variables.constants.insert("n".into(), n);

    let env = build_runtime_parameter_tail_env(&dae, &[], 0.0).expect("test env should build");

    assert_eq!(env_value(&env, "n"), 4.0);
}

#[test]
fn test_build_env_discrete_start_forward_ref_re_evaluates_and_preserves_pre_seed() {
    clear_pre_values();

    let mut dae = rumoca_ir_dae::Dae::default();

    // Insert dependent start first to exercise forward-reference handling.
    let mut a = rumoca_ir_dae::Variable::new(
        rumoca_core::VarName::new("a"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    a.start = Some(dae_var("b"));
    dae.variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("a"), a);

    let mut b = rumoca_ir_dae::Variable::new(
        rumoca_core::VarName::new("b"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    b.start = Some(dae_bool_lit(true));
    dae.variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("b"), b);

    let env = build_env(&dae, &[], &[], 0.0).expect("test env should build");
    assert_eq!(env_value(&env, "b"), 1.0);
    assert_eq!(env_value(&env, "a"), 1.0);

    // Pre-seeded values must take precedence over start expressions.
    let mut pre_env = VarEnv::<f64>::new();
    pre_env.set("a", 0.0);
    pre_env.set("b", 0.0);
    seed_pre_values_from_env(&pre_env);

    let env_from_pre = build_env_with_runtime(&dae, &[], &[], 1.0, pre_env.runtime.clone())
        .expect("test env should build");
    assert_eq!(env_value(&env_from_pre, "a"), 0.0);
    assert_eq!(env_value(&env_from_pre, "b"), 0.0);

    clear_pre_values();
}
