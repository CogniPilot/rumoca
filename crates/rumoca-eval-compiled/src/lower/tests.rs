use super::{lower_expression, lower_initial_residual, lower_residual};
use crate::layout::VarLayout;
use crate::linear_op::{BinaryOp, CompareOp, LinearOp, Reg, UnaryOp};
use indexmap::IndexMap;
use rumoca_eval_runtime::eval::{build_env, eval_expr};
use rumoca_ir_dae as dae;

fn scalar_var(name: &str) -> dae::Variable {
    dae::Variable::new(dae::VarName::new(name))
}

fn read_reg(regs: &[f64], reg: Reg) -> f64 {
    regs.get(reg as usize).copied().unwrap_or(0.0)
}

fn write_reg(regs: &mut Vec<f64>, reg: Reg, value: f64) {
    let idx = reg as usize;
    if idx >= regs.len() {
        regs.resize(idx + 1, 0.0);
    }
    regs[idx] = value;
}

fn bool_to_real(value: bool) -> f64 {
    if value { 1.0 } else { 0.0 }
}

fn eq_approx(lhs: f64, rhs: f64) -> bool {
    (lhs - rhs).abs() < f64::EPSILON
}

fn apply_unary(op: UnaryOp, value: f64) -> f64 {
    match op {
        UnaryOp::Neg => -value,
        UnaryOp::Not => bool_to_real(value == 0.0),
        UnaryOp::Abs => value.abs(),
        UnaryOp::Sign => match value.partial_cmp(&0.0) {
            Some(std::cmp::Ordering::Greater) => 1.0,
            Some(std::cmp::Ordering::Less) => -1.0,
            _ => 0.0,
        },
        UnaryOp::Sqrt => value.sqrt(),
        UnaryOp::Floor => value.floor(),
        UnaryOp::Ceil => value.ceil(),
        UnaryOp::Trunc => value.trunc(),
        UnaryOp::Sin => value.sin(),
        UnaryOp::Cos => value.cos(),
        UnaryOp::Tan => value.tan(),
        UnaryOp::Asin => value.asin(),
        UnaryOp::Acos => value.acos(),
        UnaryOp::Atan => value.atan(),
        UnaryOp::Sinh => value.sinh(),
        UnaryOp::Cosh => value.cosh(),
        UnaryOp::Tanh => value.tanh(),
        UnaryOp::Exp => value.exp(),
        UnaryOp::Log => value.ln(),
        UnaryOp::Log10 => value.log10(),
    }
}

fn apply_binary(op: BinaryOp, lhs: f64, rhs: f64) -> f64 {
    match op {
        BinaryOp::Add => lhs + rhs,
        BinaryOp::Sub => lhs - rhs,
        BinaryOp::Mul => lhs * rhs,
        BinaryOp::Div => match (rhs == 0.0, lhs == 0.0) {
            (true, true) => 0.0,
            (true, false) => f64::INFINITY,
            (false, _) => lhs / rhs,
        },
        BinaryOp::Pow => lhs.powf(rhs),
        BinaryOp::And => bool_to_real(lhs != 0.0 && rhs != 0.0),
        BinaryOp::Or => bool_to_real(lhs != 0.0 || rhs != 0.0),
        BinaryOp::Atan2 => lhs.atan2(rhs),
        BinaryOp::Min => lhs.min(rhs),
        BinaryOp::Max => lhs.max(rhs),
    }
}

fn apply_compare(op: CompareOp, lhs: f64, rhs: f64) -> f64 {
    let value = match op {
        CompareOp::Lt => lhs < rhs,
        CompareOp::Le => lhs <= rhs,
        CompareOp::Gt => lhs > rhs,
        CompareOp::Ge => lhs >= rhs,
        CompareOp::Eq => eq_approx(lhs, rhs),
        CompareOp::Ne => !eq_approx(lhs, rhs),
    };
    bool_to_real(value)
}

fn eval_linear_ops(ops: &[LinearOp], y: &[f64], p: &[f64], t: f64) -> (Vec<f64>, Option<f64>) {
    let mut regs = Vec::new();
    let mut output = None;
    for op in ops {
        match *op {
            LinearOp::Const { dst, value } => write_reg(&mut regs, dst, value),
            LinearOp::LoadTime { dst } => write_reg(&mut regs, dst, t),
            LinearOp::LoadY { dst, index } => {
                write_reg(&mut regs, dst, y.get(index).copied().unwrap_or(0.0))
            }
            LinearOp::LoadP { dst, index } => {
                write_reg(&mut regs, dst, p.get(index).copied().unwrap_or(0.0))
            }
            LinearOp::LoadSeed { dst, .. } => write_reg(&mut regs, dst, 0.0),
            LinearOp::Unary { dst, op, arg } => {
                let value = read_reg(&regs, arg);
                write_reg(&mut regs, dst, apply_unary(op, value));
            }
            LinearOp::Binary { dst, op, lhs, rhs } => {
                let l = read_reg(&regs, lhs);
                let r = read_reg(&regs, rhs);
                write_reg(&mut regs, dst, apply_binary(op, l, r));
            }
            LinearOp::Compare { dst, op, lhs, rhs } => {
                let l = read_reg(&regs, lhs);
                let r = read_reg(&regs, rhs);
                write_reg(&mut regs, dst, apply_compare(op, l, r));
            }
            LinearOp::Select {
                dst,
                cond,
                if_true,
                if_false,
            } => {
                let result = match read_reg(&regs, cond) != 0.0 {
                    true => read_reg(&regs, if_true),
                    false => read_reg(&regs, if_false),
                };
                write_reg(&mut regs, dst, result);
            }
            LinearOp::StoreOutput { src } => {
                output = Some(read_reg(&regs, src));
            }
        }
    }
    (regs, output)
}

fn component_ref(name: &str) -> dae::ComponentReference {
    dae::ComponentReference {
        local: false,
        parts: vec![dae::ComponentRefPart {
            ident: name.to_string(),
            subs: vec![],
        }],
        def_id: None,
    }
}

fn function_param(name: &str) -> dae::FunctionParam {
    dae::FunctionParam {
        name: name.to_string(),
        type_name: "Real".to_string(),
        dims: vec![],
        default: None,
        description: None,
    }
}

fn function_param_with_dims(name: &str, dims: &[i64]) -> dae::FunctionParam {
    dae::FunctionParam {
        name: name.to_string(),
        type_name: "Real".to_string(),
        dims: dims.to_vec(),
        default: None,
        description: None,
    }
}

#[test]
fn lower_expression_round_trip_matches_eval_expr() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .states
        .insert(dae::VarName::new("x"), scalar_var("x"));
    dae_model
        .algebraics
        .insert(dae::VarName::new("z"), scalar_var("z"));
    dae_model
        .outputs
        .insert(dae::VarName::new("y"), scalar_var("y"));
    dae_model
        .parameters
        .insert(dae::VarName::new("p"), scalar_var("p"));
    dae_model.constants.insert(
        dae::VarName::new("k"),
        dae::Variable {
            name: dae::VarName::new("k"),
            start: Some(dae::Expression::Literal(dae::Literal::Real(2.0))),
            ..Default::default()
        },
    );

    let expr = dae::Expression::If {
        branches: vec![(
            dae::Expression::Binary {
                op: dae::OpBinary::Gt(Default::default()),
                lhs: Box::new(dae::Expression::VarRef {
                    name: dae::VarName::new("x"),
                    subscripts: vec![],
                }),
                rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(0.0))),
            },
            dae::Expression::Binary {
                op: dae::OpBinary::Add(Default::default()),
                lhs: Box::new(dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Sin,
                    args: vec![dae::Expression::VarRef {
                        name: dae::VarName::new("x"),
                        subscripts: vec![],
                    }],
                }),
                rhs: Box::new(dae::Expression::VarRef {
                    name: dae::VarName::new("p"),
                    subscripts: vec![],
                }),
            },
        )],
        else_branch: Box::new(dae::Expression::Binary {
            op: dae::OpBinary::Mul(Default::default()),
            lhs: Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("z"),
                subscripts: vec![],
            }),
            rhs: Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("k"),
                subscripts: vec![],
            }),
        }),
    };

    let layout = VarLayout::from_dae(&dae_model);
    let lowered =
        lower_expression(&expr, &layout, &IndexMap::new()).expect("lowering should succeed");

    let y = vec![0.25, 1.5, 0.0];
    let p = vec![3.0];
    let (regs, _output) = eval_linear_ops(&lowered.ops, &y, &p, 0.4);
    let compiled = read_reg(&regs, lowered.result);

    let env = build_env(&dae_model, &y, &p, 0.4);
    let interpreted = eval_expr::<f64>(&expr, &env);
    assert!((compiled - interpreted).abs() <= 1e-12);
}

#[test]
fn lower_expression_inlines_user_function_call() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .states
        .insert(dae::VarName::new("x"), scalar_var("x"));

    let square_add_one = dae::Function {
        name: dae::VarName::new("My.squareAddOne"),
        inputs: vec![function_param("u")],
        outputs: vec![function_param("out")],
        locals: vec![],
        body: vec![dae::Statement::Assignment {
            comp: component_ref("out"),
            value: dae::Expression::Binary {
                op: dae::OpBinary::Add(Default::default()),
                lhs: Box::new(dae::Expression::Binary {
                    op: dae::OpBinary::Mul(Default::default()),
                    lhs: Box::new(dae::Expression::VarRef {
                        name: dae::VarName::new("u"),
                        subscripts: vec![],
                    }),
                    rhs: Box::new(dae::Expression::VarRef {
                        name: dae::VarName::new("u"),
                        subscripts: vec![],
                    }),
                }),
                rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(1.0))),
            },
        }],
        pure: true,
        external: None,
        derivatives: vec![],
        span: Default::default(),
    };
    dae_model
        .functions
        .insert(dae::VarName::new("My.squareAddOne"), square_add_one);

    let expr = dae::Expression::FunctionCall {
        name: dae::VarName::new("My.squareAddOne"),
        args: vec![dae::Expression::VarRef {
            name: dae::VarName::new("x"),
            subscripts: vec![],
        }],
        is_constructor: false,
    };

    let layout = VarLayout::from_dae(&dae_model);
    let lowered =
        lower_expression(&expr, &layout, &dae_model.functions).expect("lowering should succeed");
    let y = vec![3.0];
    let p = vec![];
    let (regs, _output) = eval_linear_ops(&lowered.ops, &y, &p, 0.0);
    let compiled = read_reg(&regs, lowered.result);
    assert!((compiled - 10.0).abs() <= 1e-12);
}

#[test]
fn lower_expression_handles_projected_function_output_array_element() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .states
        .insert(dae::VarName::new("th"), scalar_var("th"));

    let rot2 = dae::Function {
        name: dae::VarName::new("LieGroupsSE2.rot2"),
        inputs: vec![function_param("th")],
        outputs: vec![function_param_with_dims("R", &[2, 2])],
        locals: vec![],
        body: vec![dae::Statement::Assignment {
            comp: component_ref("R"),
            value: dae::Expression::Array {
                elements: vec![
                    dae::Expression::Array {
                        elements: vec![
                            dae::Expression::BuiltinCall {
                                function: dae::BuiltinFunction::Cos,
                                args: vec![dae::Expression::VarRef {
                                    name: dae::VarName::new("th"),
                                    subscripts: vec![],
                                }],
                            },
                            dae::Expression::Unary {
                                op: dae::OpUnary::Minus(Default::default()),
                                rhs: Box::new(dae::Expression::BuiltinCall {
                                    function: dae::BuiltinFunction::Sin,
                                    args: vec![dae::Expression::VarRef {
                                        name: dae::VarName::new("th"),
                                        subscripts: vec![],
                                    }],
                                }),
                            },
                        ],
                        is_matrix: false,
                    },
                    dae::Expression::Array {
                        elements: vec![
                            dae::Expression::BuiltinCall {
                                function: dae::BuiltinFunction::Sin,
                                args: vec![dae::Expression::VarRef {
                                    name: dae::VarName::new("th"),
                                    subscripts: vec![],
                                }],
                            },
                            dae::Expression::BuiltinCall {
                                function: dae::BuiltinFunction::Cos,
                                args: vec![dae::Expression::VarRef {
                                    name: dae::VarName::new("th"),
                                    subscripts: vec![],
                                }],
                            },
                        ],
                        is_matrix: false,
                    },
                ],
                is_matrix: true,
            },
        }],
        pure: true,
        external: None,
        derivatives: vec![],
        span: Default::default(),
    };
    dae_model
        .functions
        .insert(dae::VarName::new("LieGroupsSE2.rot2"), rot2);

    let expr = dae::Expression::FunctionCall {
        name: dae::VarName::new("LieGroupsSE2.rot2.R[2]"),
        args: vec![dae::Expression::VarRef {
            name: dae::VarName::new("th"),
            subscripts: vec![],
        }],
        is_constructor: false,
    };

    let layout = VarLayout::from_dae(&dae_model);
    let lowered = lower_expression(&expr, &layout, &dae_model.functions)
        .expect("projected function output should lower");

    let th = 0.5;
    let (regs, _) = eval_linear_ops(&lowered.ops, &[th], &[], 0.0);
    let compiled = read_reg(&regs, lowered.result);
    let expected = -th.sin();
    assert!((compiled - expected).abs() < 1e-12);
}

#[test]
fn lower_residual_applies_state_then_algebraic_sign() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .states
        .insert(dae::VarName::new("x"), scalar_var("x"));
    dae_model
        .algebraics
        .insert(dae::VarName::new("z"), scalar_var("z"));
    dae_model.f_x.push(dae::Equation::residual(
        dae::Expression::VarRef {
            name: dae::VarName::new("x"),
            subscripts: vec![],
        },
        Default::default(),
        "state row",
    ));
    dae_model.f_x.push(dae::Equation::residual(
        dae::Expression::VarRef {
            name: dae::VarName::new("z"),
            subscripts: vec![],
        },
        Default::default(),
        "algebraic row",
    ));

    let layout = VarLayout::from_dae(&dae_model);
    let rows = lower_residual(&dae_model, &layout).expect("lowering residual should succeed");
    assert_eq!(rows.len(), 2);

    let y = vec![2.0, 3.0];
    let p = vec![];

    let (_regs0, out0) = eval_linear_ops(&rows[0], &y, &p, 0.0);
    let (_regs1, out1) = eval_linear_ops(&rows[1], &y, &p, 0.0);
    assert_eq!(out0.expect("state row output"), -2.0);
    assert_eq!(out1.expect("algebraic row output"), 3.0);
}

#[test]
fn lower_expression_inlines_user_function_if_statement() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .states
        .insert(dae::VarName::new("x"), scalar_var("x"));

    let abs_like = dae::Function {
        name: dae::VarName::new("My.absLike"),
        inputs: vec![function_param("u")],
        outputs: vec![function_param("out")],
        locals: vec![],
        body: vec![dae::Statement::If {
            cond_blocks: vec![dae::StatementBlock {
                cond: dae::Expression::Binary {
                    op: dae::OpBinary::Ge(Default::default()),
                    lhs: Box::new(dae::Expression::VarRef {
                        name: dae::VarName::new("u"),
                        subscripts: vec![],
                    }),
                    rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(0.0))),
                },
                stmts: vec![dae::Statement::Assignment {
                    comp: component_ref("out"),
                    value: dae::Expression::VarRef {
                        name: dae::VarName::new("u"),
                        subscripts: vec![],
                    },
                }],
            }],
            else_block: Some(vec![dae::Statement::Assignment {
                comp: component_ref("out"),
                value: dae::Expression::Unary {
                    op: dae::OpUnary::Minus(Default::default()),
                    rhs: Box::new(dae::Expression::VarRef {
                        name: dae::VarName::new("u"),
                        subscripts: vec![],
                    }),
                },
            }]),
        }],
        pure: true,
        external: None,
        derivatives: vec![],
        span: Default::default(),
    };
    dae_model
        .functions
        .insert(dae::VarName::new("My.absLike"), abs_like);

    let expr = dae::Expression::FunctionCall {
        name: dae::VarName::new("My.absLike"),
        args: vec![dae::Expression::VarRef {
            name: dae::VarName::new("x"),
            subscripts: vec![],
        }],
        is_constructor: false,
    };

    let layout = VarLayout::from_dae(&dae_model);
    let lowered = lower_expression(&expr, &layout, &dae_model.functions)
        .expect("if-statement function should lower");

    let (regs_pos, _) = eval_linear_ops(&lowered.ops, &[2.5], &[], 0.0);
    let (regs_neg, _) = eval_linear_ops(&lowered.ops, &[-3.0], &[], 0.0);
    assert!((read_reg(&regs_pos, lowered.result) - 2.5).abs() <= 1e-12);
    assert!((read_reg(&regs_neg, lowered.result) - 3.0).abs() <= 1e-12);
}

#[test]
fn lower_expression_inlines_user_function_for_statement() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .states
        .insert(dae::VarName::new("x"), scalar_var("x"));

    let repeat_accum = dae::Function {
        name: dae::VarName::new("My.repeatAccum"),
        inputs: vec![function_param("u")],
        outputs: vec![function_param("out")],
        locals: vec![],
        body: vec![
            dae::Statement::Assignment {
                comp: component_ref("out"),
                value: dae::Expression::Literal(dae::Literal::Real(0.0)),
            },
            dae::Statement::For {
                indices: vec![dae::ForIndex {
                    ident: "i".to_string(),
                    range: dae::Expression::Range {
                        start: Box::new(dae::Expression::Literal(dae::Literal::Integer(1))),
                        step: None,
                        end: Box::new(dae::Expression::Literal(dae::Literal::Integer(3))),
                    },
                }],
                equations: vec![dae::Statement::Assignment {
                    comp: component_ref("out"),
                    value: dae::Expression::Binary {
                        op: dae::OpBinary::Add(Default::default()),
                        lhs: Box::new(dae::Expression::VarRef {
                            name: dae::VarName::new("out"),
                            subscripts: vec![],
                        }),
                        rhs: Box::new(dae::Expression::VarRef {
                            name: dae::VarName::new("u"),
                            subscripts: vec![],
                        }),
                    },
                }],
            },
        ],
        pure: true,
        external: None,
        derivatives: vec![],
        span: Default::default(),
    };
    dae_model
        .functions
        .insert(dae::VarName::new("My.repeatAccum"), repeat_accum);

    let expr = dae::Expression::FunctionCall {
        name: dae::VarName::new("My.repeatAccum"),
        args: vec![dae::Expression::VarRef {
            name: dae::VarName::new("x"),
            subscripts: vec![],
        }],
        is_constructor: false,
    };

    let layout = VarLayout::from_dae(&dae_model);
    let lowered = lower_expression(&expr, &layout, &dae_model.functions)
        .expect("for-statement function should lower");

    let (regs, _) = eval_linear_ops(&lowered.ops, &[2.0], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 6.0).abs() <= 1e-12);
}

#[test]
fn lower_expression_array_and_range_match_runtime_scalar_semantics() {
    let layout = VarLayout::default();
    let array_expr = dae::Expression::Array {
        elements: vec![dae::Expression::Literal(dae::Literal::Integer(1))],
        is_matrix: false,
    };
    let lowered = lower_expression(&array_expr, &layout, &IndexMap::new())
        .expect("array expression should lower to first scalar");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 1.0).abs() < 1e-12);

    let range_expr = dae::Expression::Range {
        start: Box::new(dae::Expression::Literal(dae::Literal::Integer(1))),
        step: None,
        end: Box::new(dae::Expression::Literal(dae::Literal::Integer(3))),
    };
    let lowered =
        lower_expression(&range_expr, &layout, &IndexMap::new()).expect("range should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!(read_reg(&regs, lowered.result).abs() < 1e-12);
}

#[test]
fn lower_expression_maps_namespaced_intrinsic_function_call() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .states
        .insert(dae::VarName::new("x"), scalar_var("x"));
    let layout = VarLayout::from_dae(&dae_model);
    let expr = dae::Expression::FunctionCall {
        name: dae::VarName::new("Modelica.Math.sin"),
        args: vec![dae::Expression::VarRef {
            name: dae::VarName::new("x"),
            subscripts: vec![],
        }],
        is_constructor: false,
    };
    let lowered = lower_expression(&expr, &layout, &IndexMap::new())
        .expect("Modelica.Math.sin should lower as intrinsic");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[0.5], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 0.5_f64.sin()).abs() < 1e-12);
}

#[test]
fn lower_expression_handles_constructor_field_access_by_signature() {
    let mut dae_model = dae::Dae::default();
    let mut constructor = dae::Function::new("My.Record", Default::default());
    constructor.inputs.push(function_param("R"));
    constructor.inputs.push(function_param("C"));
    dae_model
        .functions
        .insert(dae::VarName::new("My.Record"), constructor);

    let expr = dae::Expression::FieldAccess {
        base: Box::new(dae::Expression::FunctionCall {
            name: dae::VarName::new("My.Record"),
            args: vec![
                dae::Expression::Literal(dae::Literal::Real(2.0)),
                dae::Expression::Literal(dae::Literal::Real(3.0)),
            ],
            is_constructor: true,
        }),
        field: "C".to_string(),
    };

    let lowered = lower_expression(&expr, &VarLayout::default(), &dae_model.functions)
        .expect("constructor field access should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 3.0).abs() < 1e-12);
}

#[test]
fn lower_expression_handles_index_projection() {
    let mut dae_model = dae::Dae::default();
    dae_model.states.insert(
        dae::VarName::new("xs"),
        dae::Variable {
            name: dae::VarName::new("xs"),
            dims: vec![2],
            ..Default::default()
        },
    );
    let layout = VarLayout::from_dae(&dae_model);
    let expr = dae::Expression::Index {
        base: Box::new(dae::Expression::VarRef {
            name: dae::VarName::new("xs"),
            subscripts: vec![],
        }),
        subscripts: vec![dae::Subscript::Index(2)],
    };
    let lowered = lower_expression(&expr, &layout, &IndexMap::new()).expect("index should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[10.0, 20.0], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 20.0).abs() < 1e-12);
}

#[test]
fn lower_expression_handles_dynamic_varref_subscript_expr() {
    let mut dae_model = dae::Dae::default();
    dae_model.states.insert(
        dae::VarName::new("xs"),
        dae::Variable {
            name: dae::VarName::new("xs"),
            dims: vec![3],
            ..Default::default()
        },
    );
    dae_model
        .parameters
        .insert(dae::VarName::new("i"), scalar_var("i"));
    let layout = VarLayout::from_dae(&dae_model);
    let expr = dae::Expression::VarRef {
        name: dae::VarName::new("xs"),
        subscripts: vec![dae::Subscript::Expr(Box::new(dae::Expression::VarRef {
            name: dae::VarName::new("i"),
            subscripts: vec![],
        }))],
    };
    let lowered = lower_expression(&expr, &layout, &IndexMap::new())
        .expect("dynamic varref subscript should lower");

    let y = [10.0, 20.0, 30.0];
    for i in [1.0, 2.0, 2.7, 4.0, -1.0] {
        let (regs, _) = eval_linear_ops(&lowered.ops, &y, &[i], 0.0);
        let compiled = read_reg(&regs, lowered.result);
        let env = build_env(&dae_model, &y, &[i], 0.0);
        let interpreted = eval_expr::<f64>(&expr, &env);
        assert!(
            (compiled - interpreted).abs() < 1e-12,
            "dynamic varref mismatch for i={i}: compiled={compiled}, interpreted={interpreted}"
        );
    }
}

#[test]
fn lower_expression_handles_dynamic_index_subscript_expr() {
    let mut dae_model = dae::Dae::default();
    dae_model.states.insert(
        dae::VarName::new("xs"),
        dae::Variable {
            name: dae::VarName::new("xs"),
            dims: vec![3],
            ..Default::default()
        },
    );
    dae_model
        .parameters
        .insert(dae::VarName::new("i"), scalar_var("i"));
    let layout = VarLayout::from_dae(&dae_model);
    let expr = dae::Expression::Index {
        base: Box::new(dae::Expression::VarRef {
            name: dae::VarName::new("xs"),
            subscripts: vec![],
        }),
        subscripts: vec![dae::Subscript::Expr(Box::new(dae::Expression::VarRef {
            name: dae::VarName::new("i"),
            subscripts: vec![],
        }))],
    };
    let lowered =
        lower_expression(&expr, &layout, &IndexMap::new()).expect("dynamic index should lower");

    let y = [10.0, 20.0, 30.0];
    for i in [1.0, 2.0, 2.7, 4.0, -1.0] {
        let (regs, _) = eval_linear_ops(&lowered.ops, &y, &[i], 0.0);
        let compiled = read_reg(&regs, lowered.result);
        let env = build_env(&dae_model, &y, &[i], 0.0);
        let interpreted = eval_expr::<f64>(&expr, &env);
        assert!(
            (compiled - interpreted).abs() < 1e-12,
            "dynamic index mismatch for i={i}: compiled={compiled}, interpreted={interpreted}"
        );
    }
}

#[test]
fn lower_expression_der_builtin_returns_zero() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .states
        .insert(dae::VarName::new("x"), scalar_var("x"));
    let layout = VarLayout::from_dae(&dae_model);
    let expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Der,
        args: vec![dae::Expression::VarRef {
            name: dae::VarName::new("x"),
            subscripts: vec![],
        }],
    };
    let lowered =
        lower_expression(&expr, &layout, &IndexMap::new()).expect("der builtin should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[1.2], &[], 0.0);
    assert!(read_reg(&regs, lowered.result).abs() < 1e-12);
}

#[test]
fn lower_expression_supports_interval_intrinsic() {
    let layout = VarLayout::default();
    let expr = dae::Expression::FunctionCall {
        name: dae::VarName::new("interval"),
        args: vec![dae::Expression::FunctionCall {
            name: dae::VarName::new("Clock"),
            args: vec![dae::Expression::Literal(dae::Literal::Real(0.2))],
            is_constructor: false,
        }],
        is_constructor: false,
    };
    let lowered =
        lower_expression(&expr, &layout, &IndexMap::new()).expect("interval should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 0.2).abs() < 1e-12);
}

#[test]
fn lower_expression_supports_size_builtin_for_known_array_dims() {
    let mut dae_model = dae::Dae::default();
    dae_model.states.insert(
        dae::VarName::new("A"),
        dae::Variable {
            name: dae::VarName::new("A"),
            dims: vec![2, 3],
            ..Default::default()
        },
    );
    let layout = VarLayout::from_dae(&dae_model);
    let expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Size,
        args: vec![
            dae::Expression::VarRef {
                name: dae::VarName::new("A"),
                subscripts: vec![],
            },
            dae::Expression::Literal(dae::Literal::Integer(2)),
        ],
    };
    let lowered = lower_expression(&expr, &layout, &IndexMap::new()).expect("size should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[0.0; 6], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 3.0).abs() < 1e-12);
}

#[test]
fn lower_expression_supports_sum_builtin_for_range() {
    let layout = VarLayout::default();
    let expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Sum,
        args: vec![dae::Expression::Range {
            start: Box::new(dae::Expression::Literal(dae::Literal::Integer(1))),
            step: None,
            end: Box::new(dae::Expression::Literal(dae::Literal::Integer(4))),
        }],
    };
    let lowered = lower_expression(&expr, &layout, &IndexMap::new()).expect("sum(range) lowers");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 10.0).abs() < 1e-12);
}

#[test]
fn lower_expression_supports_scalar_array_builtins() {
    let layout = VarLayout::default();

    let zeros = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Zeros,
        args: vec![dae::Expression::Literal(dae::Literal::Integer(3))],
    };
    let lowered = lower_expression(&zeros, &layout, &IndexMap::new()).expect("zeros lowers");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!(read_reg(&regs, lowered.result).abs() < 1e-12);

    let fill = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Fill,
        args: vec![
            dae::Expression::Literal(dae::Literal::Real(2.5)),
            dae::Expression::Literal(dae::Literal::Integer(4)),
        ],
    };
    let lowered = lower_expression(&fill, &layout, &IndexMap::new()).expect("fill lowers");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 2.5).abs() < 1e-12);

    let cat = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Cat,
        args: vec![
            dae::Expression::Literal(dae::Literal::Integer(1)),
            dae::Expression::Array {
                elements: vec![
                    dae::Expression::Literal(dae::Literal::Real(7.0)),
                    dae::Expression::Literal(dae::Literal::Real(8.0)),
                ],
                is_matrix: false,
            },
        ],
    };
    let lowered = lower_expression(&cat, &layout, &IndexMap::new()).expect("cat lowers");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 7.0).abs() < 1e-12);
}

#[test]
fn lower_initial_residual_treats_initial_builtin_as_true() {
    let mut dae_model = dae::Dae::default();
    dae_model.initial_equations.push(dae::Equation::residual(
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Initial,
            args: vec![],
        },
        Default::default(),
        "initial() row",
    ));
    let rows =
        lower_initial_residual(&dae_model, &VarLayout::default()).expect("initial residual lowers");
    assert_eq!(rows.len(), 1);
    let (regs, out) = eval_linear_ops(&rows[0], &[], &[], 0.0);
    assert_eq!(out.expect("output"), 1.0);
    assert_eq!(read_reg(&regs, 0), 1.0);
}
