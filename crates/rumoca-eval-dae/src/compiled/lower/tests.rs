use super::{
    lower_discrete_rhs, lower_expression, lower_initial_expression_rows_from_expressions,
    lower_initial_residual, lower_residual,
};
use crate::compiled::layout::VarLayout;
use crate::compiled::linear_op::{BinaryOp, CompareOp, LinearOp, Reg, UnaryOp};
use crate::runtime::eval::{
    build_env, eval_expr_dae as eval_expr, eval_table_bound_value, eval_table_lookup_slope_value,
    eval_table_lookup_value, eval_time_table_next_event_value,
};
use indexmap::IndexMap;
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
            LinearOp::TableBounds { dst, table_id, max } => {
                let table_id = read_reg(&regs, table_id);
                write_reg(&mut regs, dst, eval_table_bound_value(table_id, max));
            }
            LinearOp::TableLookup {
                dst,
                table_id,
                column,
                input,
            } => {
                let table_id = read_reg(&regs, table_id);
                let column = read_reg(&regs, column);
                let input = read_reg(&regs, input);
                write_reg(
                    &mut regs,
                    dst,
                    eval_table_lookup_value(table_id, column, input),
                );
            }
            LinearOp::TableLookupSlope {
                dst,
                table_id,
                column,
                input,
            } => {
                let table_id = read_reg(&regs, table_id);
                let column = read_reg(&regs, column);
                let input = read_reg(&regs, input);
                write_reg(
                    &mut regs,
                    dst,
                    eval_table_lookup_slope_value(table_id, column, input),
                );
            }
            LinearOp::TableNextEvent {
                dst,
                table_id,
                time,
            } => {
                let table_id = read_reg(&regs, table_id);
                let time = read_reg(&regs, time);
                write_reg(
                    &mut regs,
                    dst,
                    eval_time_table_next_event_value(table_id, time),
                );
            }
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

fn named_arg(name: &str, value: dae::Expression) -> dae::Expression {
    dae::Expression::FunctionCall {
        name: dae::VarName::new(format!("__rumoca_named_arg__.{name}")),
        args: vec![value],
        is_constructor: false,
    }
}

fn complex_output_param(name: &str) -> dae::FunctionParam {
    dae::FunctionParam {
        name: name.to_string(),
        type_name: "Complex".to_string(),
        dims: vec![],
        default: None,
        description: None,
    }
}

fn insert_complex_constructor(dae_model: &mut dae::Dae, im_default: Option<dae::Expression>) {
    let mut complex_ctor = dae::Function::new("Complex", Default::default());
    complex_ctor
        .inputs
        .push(dae::FunctionParam::new("re", "Real"));
    let imag_input = dae::FunctionParam::new("im", "Real");
    complex_ctor.inputs.push(match im_default {
        Some(default) => imag_input.with_default(default),
        None => imag_input,
    });
    complex_ctor
        .outputs
        .push(dae::FunctionParam::new("res", "Complex"));
    dae_model
        .functions
        .insert(dae::VarName::new("Complex"), complex_ctor);
}

fn complex_call(args: Vec<dae::Expression>, is_constructor: bool) -> dae::Expression {
    dae::Expression::FunctionCall {
        name: dae::VarName::new("Complex"),
        args,
        is_constructor,
    }
}

fn eq_local(name: &str, value: f64) -> dae::Expression {
    dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Eq(Default::default()),
        lhs: Box::new(dae::Expression::VarRef {
            name: dae::VarName::new(name),
            subscripts: vec![],
        }),
        rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(value))),
    }
}

fn build_power_of_j_function(
    branches: Vec<(dae::Expression, dae::Expression)>,
    else_branch: dae::Expression,
) -> dae::Function {
    dae::Function {
        name: dae::VarName::new("My.powerOfJ"),
        inputs: vec![function_param("k")],
        outputs: vec![complex_output_param("x")],
        locals: vec![function_param("m")],
        body: vec![
            dae::Statement::Assignment {
                comp: component_ref("m"),
                value: dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Mod,
                    args: vec![
                        dae::Expression::VarRef {
                            name: dae::VarName::new("k"),
                            subscripts: vec![],
                        },
                        dae::Expression::Literal(dae::Literal::Real(4.0)),
                    ],
                },
            },
            dae::Statement::Assignment {
                comp: component_ref("x"),
                value: dae::Expression::If {
                    branches,
                    else_branch: Box::new(else_branch),
                },
            },
        ],
        pure: true,
        external: None,
        derivatives: vec![],
        span: Default::default(),
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
                op: rumoca_ir_core::OpBinary::Gt(Default::default()),
                lhs: Box::new(dae::Expression::VarRef {
                    name: dae::VarName::new("x"),
                    subscripts: vec![],
                }),
                rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(0.0))),
            },
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Add(Default::default()),
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
            op: rumoca_ir_core::OpBinary::Mul(Default::default()),
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
                op: rumoca_ir_core::OpBinary::Add(Default::default()),
                lhs: Box::new(dae::Expression::Binary {
                    op: rumoca_ir_core::OpBinary::Mul(Default::default()),
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
                                op: rumoca_ir_core::OpUnary::Minus(Default::default()),
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
fn lower_expression_handles_projected_complex_function_output_field() {
    let mut dae_model = dae::Dae::default();

    let power_of_j = dae::Function {
        name: dae::VarName::new("My.powerOfJ"),
        inputs: vec![function_param("k")],
        outputs: vec![dae::FunctionParam {
            name: "x".to_string(),
            type_name: "Complex".to_string(),
            dims: vec![],
            default: None,
            description: None,
        }],
        locals: vec![],
        body: vec![dae::Statement::Assignment {
            comp: component_ref("x"),
            value: dae::Expression::Array {
                elements: vec![
                    dae::Expression::Literal(dae::Literal::Real(0.0)),
                    dae::Expression::Literal(dae::Literal::Real(1.0)),
                ],
                is_matrix: false,
            },
        }],
        pure: true,
        external: None,
        derivatives: vec![],
        span: Default::default(),
    };
    dae_model
        .functions
        .insert(dae::VarName::new("My.powerOfJ"), power_of_j);

    let expr = dae::Expression::FunctionCall {
        name: dae::VarName::new("My.powerOfJ.x.re"),
        args: vec![dae::Expression::Literal(dae::Literal::Integer(1))],
        is_constructor: false,
    };

    let layout = VarLayout::default();
    let lowered = lower_expression(&expr, &layout, &dae_model.functions)
        .expect("projected complex output field should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!(read_reg(&regs, lowered.result).abs() < 1e-12);
}

#[test]
fn lower_expression_handles_projected_complex_function_output_field_from_if_constructor() {
    let mut dae_model = dae::Dae::default();

    let power_of_j = dae::Function {
        name: dae::VarName::new("My.powerOfJ"),
        inputs: vec![function_param("k")],
        outputs: vec![dae::FunctionParam {
            name: "x".to_string(),
            type_name: "Complex".to_string(),
            dims: vec![],
            default: None,
            description: None,
        }],
        locals: vec![function_param("m")],
        body: vec![
            dae::Statement::Assignment {
                comp: component_ref("m"),
                value: dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Mod,
                    args: vec![
                        dae::Expression::VarRef {
                            name: dae::VarName::new("k"),
                            subscripts: vec![],
                        },
                        dae::Expression::Literal(dae::Literal::Real(4.0)),
                    ],
                },
            },
            dae::Statement::Assignment {
                comp: component_ref("x"),
                value: dae::Expression::If {
                    branches: vec![
                        (
                            dae::Expression::Binary {
                                op: rumoca_ir_core::OpBinary::Eq(Default::default()),
                                lhs: Box::new(dae::Expression::VarRef {
                                    name: dae::VarName::new("m"),
                                    subscripts: vec![],
                                }),
                                rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(0.0))),
                            },
                            dae::Expression::FunctionCall {
                                name: dae::VarName::new("Complex"),
                                args: vec![
                                    dae::Expression::Literal(dae::Literal::Real(1.0)),
                                    dae::Expression::Literal(dae::Literal::Real(0.0)),
                                ],
                                is_constructor: true,
                            },
                        ),
                        (
                            dae::Expression::Binary {
                                op: rumoca_ir_core::OpBinary::Eq(Default::default()),
                                lhs: Box::new(dae::Expression::VarRef {
                                    name: dae::VarName::new("m"),
                                    subscripts: vec![],
                                }),
                                rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(1.0))),
                            },
                            dae::Expression::FunctionCall {
                                name: dae::VarName::new("Complex"),
                                args: vec![
                                    dae::Expression::Literal(dae::Literal::Real(0.0)),
                                    dae::Expression::Literal(dae::Literal::Real(1.0)),
                                ],
                                is_constructor: true,
                            },
                        ),
                    ],
                    else_branch: Box::new(dae::Expression::FunctionCall {
                        name: dae::VarName::new("Complex"),
                        args: vec![
                            dae::Expression::Literal(dae::Literal::Real(0.0)),
                            dae::Expression::Literal(dae::Literal::Real(-1.0)),
                        ],
                        is_constructor: true,
                    }),
                },
            },
        ],
        pure: true,
        external: None,
        derivatives: vec![],
        span: Default::default(),
    };
    dae_model
        .functions
        .insert(dae::VarName::new("My.powerOfJ"), power_of_j);

    let expr = dae::Expression::FunctionCall {
        name: dae::VarName::new("My.powerOfJ.x.im"),
        args: vec![dae::Expression::Literal(dae::Literal::Integer(1))],
        is_constructor: false,
    };

    let layout = VarLayout::default();
    let lowered = lower_expression(&expr, &layout, &dae_model.functions)
        .expect("projected complex output field from if constructor should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 1.0).abs() < 1e-12);
}

#[test]
fn lower_expression_handles_projected_complex_output_from_unflagged_constructor_calls() {
    let mut dae_model = dae::Dae::default();
    insert_complex_constructor(&mut dae_model, None);

    let power_of_j = build_power_of_j_function(
        vec![
            (
                eq_local("m", 0.0),
                complex_call(
                    vec![
                        dae::Expression::Literal(dae::Literal::Real(1.0)),
                        dae::Expression::Literal(dae::Literal::Real(0.0)),
                    ],
                    false,
                ),
            ),
            (
                eq_local("m", 1.0),
                complex_call(
                    vec![
                        dae::Expression::Literal(dae::Literal::Real(0.0)),
                        dae::Expression::Literal(dae::Literal::Real(1.0)),
                    ],
                    false,
                ),
            ),
        ],
        complex_call(
            vec![
                dae::Expression::Literal(dae::Literal::Real(0.0)),
                dae::Expression::Literal(dae::Literal::Real(-1.0)),
            ],
            false,
        ),
    );
    dae_model
        .functions
        .insert(dae::VarName::new("My.powerOfJ"), power_of_j);

    let expr = dae::Expression::FunctionCall {
        name: dae::VarName::new("My.powerOfJ.x.im"),
        args: vec![dae::Expression::Literal(dae::Literal::Integer(1))],
        is_constructor: false,
    };

    let lowered = lower_expression(&expr, &VarLayout::default(), &dae_model.functions)
        .expect("projected complex output field from unflagged constructor should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 1.0).abs() < 1e-12);
}

#[test]
fn lower_expression_handles_projected_complex_output_with_constructor_defaults_and_negation() {
    let mut dae_model = dae::Dae::default();
    insert_complex_constructor(
        &mut dae_model,
        Some(dae::Expression::Literal(dae::Literal::Real(0.0))),
    );

    let power_of_j = build_power_of_j_function(
        vec![
            (
                eq_local("m", 0.0),
                complex_call(
                    vec![dae::Expression::Literal(dae::Literal::Integer(1))],
                    true,
                ),
            ),
            (
                eq_local("m", 1.0),
                complex_call(
                    vec![
                        dae::Expression::Literal(dae::Literal::Integer(0)),
                        dae::Expression::Literal(dae::Literal::Integer(1)),
                    ],
                    false,
                ),
            ),
            (
                eq_local("m", 2.0),
                complex_call(
                    vec![dae::Expression::Unary {
                        op: rumoca_ir_core::OpUnary::Minus(Default::default()),
                        rhs: Box::new(dae::Expression::Literal(dae::Literal::Integer(1))),
                    }],
                    true,
                ),
            ),
        ],
        dae::Expression::Unary {
            op: rumoca_ir_core::OpUnary::Minus(Default::default()),
            rhs: Box::new(complex_call(
                vec![
                    dae::Expression::Literal(dae::Literal::Integer(0)),
                    dae::Expression::Literal(dae::Literal::Integer(1)),
                ],
                false,
            )),
        },
    );
    dae_model
        .functions
        .insert(dae::VarName::new("My.powerOfJ"), power_of_j);

    let expr = dae::Expression::FunctionCall {
        name: dae::VarName::new("My.powerOfJ.x.im"),
        args: vec![dae::Expression::Literal(dae::Literal::Integer(3))],
        is_constructor: false,
    };

    let lowered = lower_expression(&expr, &VarLayout::default(), &dae_model.functions)
        .expect("projected complex output with constructor defaults and negation should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) + 1.0).abs() < 1e-12);
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
                    op: rumoca_ir_core::OpBinary::Ge(Default::default()),
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
                    op: rumoca_ir_core::OpUnary::Minus(Default::default()),
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
                        op: rumoca_ir_core::OpBinary::Add(Default::default()),
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
fn lower_expression_maps_capitalized_integer_intrinsic_function_call() {
    let expr = dae::Expression::FunctionCall {
        name: dae::VarName::new("Integer"),
        args: vec![dae::Expression::Literal(dae::Literal::Real(3.7))],
        is_constructor: false,
    };
    let lowered = lower_expression(&expr, &VarLayout::default(), &IndexMap::new())
        .expect("Integer should lower as intrinsic");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 3.0).abs() < 1e-12);
}

#[test]
fn lower_expression_maps_strings_length_runtime_special() {
    let expr = dae::Expression::FunctionCall {
        name: dae::VarName::new("Modelica.Utilities.Strings.length"),
        args: vec![dae::Expression::Literal(dae::Literal::String(
            "hello".to_string(),
        ))],
        is_constructor: false,
    };
    let lowered = lower_expression(&expr, &VarLayout::default(), &IndexMap::new())
        .expect("Modelica.Utilities.Strings.length should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 5.0).abs() < 1e-12);
}

#[test]
fn lower_expression_maps_full_path_name_runtime_special_to_placeholder() {
    let expr = dae::Expression::FunctionCall {
        name: dae::VarName::new("Modelica.Utilities.Files.fullPathName"),
        args: vec![dae::Expression::Literal(dae::Literal::String(
            "a.txt".to_string(),
        ))],
        is_constructor: false,
    };
    let lowered = lower_expression(&expr, &VarLayout::default(), &IndexMap::new())
        .expect("Modelica.Utilities.Files.fullPathName should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert_eq!(read_reg(&regs, lowered.result), 0.0);
}

#[test]
fn lower_expression_prefers_find_last_runtime_special_over_user_function_body() {
    let mut functions = IndexMap::new();
    let mut find_last =
        dae::Function::new("Modelica.Utilities.Strings.findLast", Default::default());
    find_last.inputs.push(function_param("string"));
    find_last.inputs.push(function_param("searchString"));
    find_last.inputs.push(function_param("startIndex"));
    find_last.inputs.push(function_param("caseSensitive"));
    find_last.outputs.push(function_param("index"));
    find_last
        .body
        .push(dae::Statement::While(dae::StatementBlock {
            cond: dae::Expression::Literal(dae::Literal::Boolean(true)),
            stmts: vec![dae::Statement::Assignment {
                comp: component_ref("index"),
                value: dae::Expression::Literal(dae::Literal::Real(1.0)),
            }],
        }));
    functions.insert(
        dae::VarName::new("Modelica.Utilities.Strings.findLast"),
        find_last,
    );

    let expr = dae::Expression::FunctionCall {
        name: dae::VarName::new("Modelica.Utilities.Strings.findLast"),
        args: vec![
            dae::Expression::VarRef {
                name: dae::VarName::new("fileName"),
                subscripts: vec![],
            },
            dae::Expression::Literal(dae::Literal::String(".csv".to_string())),
            named_arg(
                "caseSensitive",
                dae::Expression::Literal(dae::Literal::Boolean(false)),
            ),
        ],
        is_constructor: false,
    };
    let mut dae_model = dae::Dae::default();
    dae_model
        .parameters
        .insert(dae::VarName::new("fileName"), scalar_var("fileName"));
    let layout = VarLayout::from_dae(&dae_model);
    let lowered = lower_expression(&expr, &layout, &functions)
        .expect("findLast should lower through runtime special override");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[0.0], &[], 0.0);
    assert_eq!(read_reg(&regs, lowered.result), 0.0);
}

#[test]
fn lower_expression_binds_named_function_arguments_by_name() {
    let mut functions = IndexMap::new();
    let mut function = dae::Function::new("Pkg.f", Default::default());
    function.inputs.push(function_param("a"));
    function.inputs.push(function_param("b"));
    function.outputs.push(function_param("y"));
    function.body.push(dae::Statement::Assignment {
        comp: component_ref("y"),
        value: dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(Default::default()),
            lhs: Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("a"),
                subscripts: vec![],
            }),
            rhs: Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("b"),
                subscripts: vec![],
            }),
        },
    });
    functions.insert(dae::VarName::new("Pkg.f"), function);

    let expr = dae::Expression::FunctionCall {
        name: dae::VarName::new("Pkg.f"),
        args: vec![
            named_arg("b", dae::Expression::Literal(dae::Literal::Real(2.0))),
            named_arg("a", dae::Expression::Literal(dae::Literal::Real(7.0))),
        ],
        is_constructor: false,
    };
    let lowered = lower_expression(&expr, &VarLayout::default(), &functions)
        .expect("named args should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 5.0).abs() < 1.0e-12);
}

#[test]
fn lower_initial_expression_rows_treat_pre_as_current_value() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .algebraics
        .insert(dae::VarName::new("x"), scalar_var("x"));
    let layout = VarLayout::from_dae(&dae_model);
    let expressions = vec![dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Add(Default::default()),
        lhs: Box::new(dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Pre,
            args: vec![dae::Expression::VarRef {
                name: dae::VarName::new("x"),
                subscripts: vec![],
            }],
        }),
        rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(1.0))),
    }];
    let rows =
        lower_initial_expression_rows_from_expressions(&expressions, &layout, &IndexMap::new())
            .expect("initial-mode pre should lower");
    let (regs, output) = eval_linear_ops(&rows[0], &[2.0], &[], 0.0);
    let value = output.unwrap_or_else(|| read_reg(&regs, 0));
    assert!((value - 3.0).abs() < 1.0e-12);
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
fn lower_expression_handles_dynamic_index_over_array_literal() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .parameters
        .insert(dae::VarName::new("i"), scalar_var("i"));
    let layout = VarLayout::from_dae(&dae_model);
    let expr = dae::Expression::Index {
        base: Box::new(dae::Expression::Array {
            elements: vec![
                dae::Expression::Literal(dae::Literal::Real(10.0)),
                dae::Expression::Literal(dae::Literal::Real(20.0)),
                dae::Expression::Literal(dae::Literal::Real(30.0)),
            ],
            is_matrix: false,
        }),
        subscripts: vec![dae::Subscript::Expr(Box::new(dae::Expression::VarRef {
            name: dae::VarName::new("i"),
            subscripts: vec![],
        }))],
    };
    let lowered = lower_expression(&expr, &layout, &IndexMap::new())
        .expect("dynamic array literal index should lower");

    for i in [1.0, 2.0, 2.7, 4.0, -1.0] {
        let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[i], 0.0);
        let compiled = read_reg(&regs, lowered.result);
        let env = build_env(&dae_model, &[], &[i], 0.0);
        let interpreted = eval_expr::<f64>(&expr, &env);
        assert!(
            (compiled - interpreted).abs() < 1e-12,
            "dynamic array literal index mismatch for i={i}: compiled={compiled}, interpreted={interpreted}"
        );
    }
}

#[test]
fn lower_expression_handles_projected_field_after_array_literal_index() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .parameters
        .insert(dae::VarName::new("i"), scalar_var("i"));
    dae_model
        .parameters
        .insert(dae::VarName::new("left.im"), scalar_var("left.im"));
    dae_model
        .parameters
        .insert(dae::VarName::new("right.im"), scalar_var("right.im"));
    let layout = VarLayout::from_dae(&dae_model);
    let expr = dae::Expression::FieldAccess {
        base: Box::new(dae::Expression::Index {
            base: Box::new(dae::Expression::Array {
                elements: vec![
                    dae::Expression::VarRef {
                        name: dae::VarName::new("left"),
                        subscripts: vec![],
                    },
                    dae::Expression::VarRef {
                        name: dae::VarName::new("right"),
                        subscripts: vec![],
                    },
                ],
                is_matrix: false,
            }),
            subscripts: vec![dae::Subscript::Expr(Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("i"),
                subscripts: vec![],
            }))],
        }),
        field: "im".to_string(),
    };
    let lowered = lower_expression(&expr, &layout, &IndexMap::new())
        .expect("projected field after array literal index should lower");

    for i in [1.0, 2.0, 3.0] {
        let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[i, 4.0, 9.0], 0.0);
        let compiled = read_reg(&regs, lowered.result);
        let env = build_env(&dae_model, &[], &[i, 4.0, 9.0], 0.0);
        let interpreted = eval_expr::<f64>(&expr, &env);
        assert!(
            (compiled - interpreted).abs() < 1e-12,
            "projected field after array literal index mismatch for i={i}: compiled={compiled}, interpreted={interpreted}"
        );
    }
}

#[test]
fn lower_expression_handles_nested_structural_index_over_array_literal() {
    let layout = VarLayout::default();
    let lit = |value: f64| dae::Expression::Literal(dae::Literal::Real(value));
    let expr = dae::Expression::Index {
        base: Box::new(dae::Expression::Index {
            base: Box::new(dae::Expression::Array {
                elements: vec![
                    dae::Expression::Array {
                        elements: vec![lit(1.0), lit(2.0)],
                        is_matrix: false,
                    },
                    dae::Expression::Array {
                        elements: vec![lit(3.0), lit(4.0)],
                        is_matrix: false,
                    },
                ],
                is_matrix: true,
            }),
            subscripts: vec![dae::Subscript::Expr(Box::new(lit(2.0)))],
        }),
        subscripts: vec![dae::Subscript::Expr(Box::new(lit(1.0)))],
    };
    let lowered = lower_expression(&expr, &layout, &IndexMap::new())
        .expect("nested structural index should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 3.0).abs() < 1e-12);
}

#[test]
fn lower_expression_handles_projected_complex_sum_over_array_comprehension() {
    let layout = VarLayout::default();
    let lit = |value: f64| dae::Expression::Literal(dae::Literal::Real(value));
    let expr = dae::Expression::FieldAccess {
        base: Box::new(dae::Expression::FunctionCall {
            name: dae::VarName::new("Modelica.ComplexMath.sum"),
            args: vec![dae::Expression::ArrayComprehension {
                expr: Box::new(dae::Expression::FunctionCall {
                    name: dae::VarName::new("Complex"),
                    args: vec![
                        dae::Expression::VarRef {
                            name: dae::VarName::new("k"),
                            subscripts: vec![],
                        },
                        lit(1.0),
                    ],
                    is_constructor: true,
                }),
                indices: vec![dae::ComprehensionIndex {
                    name: "k".to_string(),
                    range: dae::Expression::Range {
                        start: Box::new(lit(1.0)),
                        step: None,
                        end: Box::new(lit(2.0)),
                    },
                }],
                filter: None,
            }],
            is_constructor: false,
        }),
        field: "im".to_string(),
    };
    let lowered = lower_expression(&expr, &layout, &IndexMap::new())
        .expect("projected complex sum over array comprehension should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 2.0).abs() < 1e-12);
}

#[test]
fn lower_expression_handles_projected_complex_division_component() {
    let layout = VarLayout::default();
    let lit = |value: f64| dae::Expression::Literal(dae::Literal::Real(value));
    let expr = dae::Expression::FieldAccess {
        base: Box::new(dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Div(Default::default()),
            lhs: Box::new(lit(1.0)),
            rhs: Box::new(dae::Expression::FunctionCall {
                name: dae::VarName::new("Complex"),
                args: vec![lit(2.0), lit(3.0)],
                is_constructor: true,
            }),
        }),
        field: "im".to_string(),
    };
    let lowered = lower_expression(&expr, &layout, &IndexMap::new())
        .expect("projected complex division component should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) + 3.0 / 13.0).abs() < 1e-12);
}

#[test]
fn lower_expression_handles_projected_complex_operator_call() {
    let layout = VarLayout::default();
    let lit = |value: f64| dae::Expression::Literal(dae::Literal::Real(value));
    let expr = dae::Expression::FieldAccess {
        base: Box::new(dae::Expression::FunctionCall {
            name: dae::VarName::new("Complex.'+'"),
            args: vec![
                dae::Expression::FunctionCall {
                    name: dae::VarName::new("Complex"),
                    args: vec![lit(1.0), lit(2.0)],
                    is_constructor: true,
                },
                dae::Expression::FunctionCall {
                    name: dae::VarName::new("Complex"),
                    args: vec![lit(3.0), lit(4.0)],
                    is_constructor: true,
                },
            ],
            is_constructor: false,
        }),
        field: "im".to_string(),
    };
    let lowered = lower_expression(&expr, &layout, &IndexMap::new())
        .expect("projected complex operator call should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 6.0).abs() < 1e-12);
}

#[test]
fn lower_expression_handles_projected_field_over_array_literal_in_scalar_context() {
    let layout = VarLayout::default();
    let lit = |value: f64| dae::Expression::Literal(dae::Literal::Real(value));
    let expr = dae::Expression::FieldAccess {
        base: Box::new(dae::Expression::Array {
            elements: vec![
                dae::Expression::FunctionCall {
                    name: dae::VarName::new("Complex"),
                    args: vec![lit(2.0), lit(3.0)],
                    is_constructor: true,
                },
                dae::Expression::FunctionCall {
                    name: dae::VarName::new("Complex"),
                    args: vec![lit(5.0), lit(7.0)],
                    is_constructor: true,
                },
            ],
            is_matrix: false,
        }),
        field: "im".to_string(),
    };
    let lowered = lower_expression(&expr, &layout, &IndexMap::new())
        .expect("projected field over array literal should lower in scalar context");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 3.0).abs() < 1e-12);
}

#[test]
fn lower_expression_handles_projected_complex_division_over_array_literal_in_scalar_context() {
    let layout = VarLayout::default();
    let lit = |value: f64| dae::Expression::Literal(dae::Literal::Real(value));
    let complex_div = |re: f64, im: f64| dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Div(Default::default()),
        lhs: Box::new(lit(1.0)),
        rhs: Box::new(dae::Expression::FunctionCall {
            name: dae::VarName::new("Complex"),
            args: vec![lit(re), lit(im)],
            is_constructor: true,
        }),
    };
    let expr = dae::Expression::FieldAccess {
        base: Box::new(dae::Expression::Array {
            elements: vec![complex_div(2.0, 3.0), complex_div(5.0, 7.0)],
            is_matrix: false,
        }),
        field: "re".to_string(),
    };
    let lowered = lower_expression(&expr, &layout, &IndexMap::new())
        .expect("projected complex division over array literal should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - (2.0 / 13.0)).abs() < 1e-12);
}

#[test]
fn lower_expression_handles_projected_complex_output_with_array_literal_field_input() {
    let mut functions = IndexMap::new();
    let mut function = dae::Function::new("Pkg.pickFirstComplexField", Default::default());
    function
        .inputs
        .push(dae::FunctionParam::new("c", "Modelica.ComplexMath.Complex").with_dims(vec![1]));
    function.outputs.push(dae::FunctionParam::new(
        "result",
        "Modelica.ComplexMath.Complex",
    ));
    function.body.push(dae::Statement::Assignment {
        comp: component_ref("result"),
        value: dae::Expression::FunctionCall {
            name: dae::VarName::new("Complex"),
            args: vec![
                dae::Expression::FieldAccess {
                    base: Box::new(dae::Expression::Index {
                        base: Box::new(dae::Expression::VarRef {
                            name: dae::VarName::new("c"),
                            subscripts: vec![],
                        }),
                        subscripts: vec![dae::Subscript::Index(1)],
                    }),
                    field: "re".to_string(),
                },
                dae::Expression::FieldAccess {
                    base: Box::new(dae::Expression::Index {
                        base: Box::new(dae::Expression::VarRef {
                            name: dae::VarName::new("c"),
                            subscripts: vec![],
                        }),
                        subscripts: vec![dae::Subscript::Index(1)],
                    }),
                    field: "im".to_string(),
                },
            ],
            is_constructor: true,
        },
    });
    functions.insert(function.name.clone(), function);

    let lit = |value: f64| dae::Expression::Literal(dae::Literal::Real(value));
    let arg = dae::Expression::Array {
        elements: vec![dae::Expression::FunctionCall {
            name: dae::VarName::new("Complex"),
            args: vec![lit(2.0), lit(-3.0)],
            is_constructor: true,
        }],
        is_matrix: false,
    };
    let expr = dae::Expression::FunctionCall {
        name: dae::VarName::new("Pkg.pickFirstComplexField.result.im"),
        args: vec![arg],
        is_constructor: false,
    };
    let lowered =
        lower_expression(&expr, &VarLayout::default(), &functions).expect("function should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) + 3.0).abs() < 1e-12);
}

#[test]
fn lower_expression_handles_projected_complex_sum_with_encoded_slice_varref() {
    let mut functions = IndexMap::new();
    let mut function = dae::Function::new("Pkg.sumComplexEncoded", Default::default());
    function
        .inputs
        .push(dae::FunctionParam::new("v", "Modelica.ComplexMath.Complex").with_dims(vec![3]));
    function.outputs.push(dae::FunctionParam::new(
        "result",
        "Modelica.ComplexMath.Complex",
    ));
    function.body.push(dae::Statement::Assignment {
        comp: component_ref("result"),
        value: dae::Expression::FunctionCall {
            name: dae::VarName::new("Complex"),
            args: vec![
                dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Sum,
                    args: vec![dae::Expression::VarRef {
                        name: dae::VarName::new("v[:].re"),
                        subscripts: vec![],
                    }],
                },
                dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Sum,
                    args: vec![dae::Expression::VarRef {
                        name: dae::VarName::new("v[:].im"),
                        subscripts: vec![],
                    }],
                },
            ],
            is_constructor: true,
        },
    });
    functions.insert(function.name.clone(), function);

    let lit = |value: f64| dae::Expression::Literal(dae::Literal::Real(value));
    let arg = dae::Expression::Array {
        elements: vec![
            dae::Expression::FunctionCall {
                name: dae::VarName::new("Complex"),
                args: vec![lit(2.0), lit(-3.0)],
                is_constructor: true,
            },
            dae::Expression::FunctionCall {
                name: dae::VarName::new("Complex"),
                args: vec![lit(1.0), lit(4.0)],
                is_constructor: true,
            },
            dae::Expression::FunctionCall {
                name: dae::VarName::new("Complex"),
                args: vec![lit(-5.0), lit(2.0)],
                is_constructor: true,
            },
        ],
        is_matrix: false,
    };
    let expr = dae::Expression::FunctionCall {
        name: dae::VarName::new("Pkg.sumComplexEncoded.result.re"),
        args: vec![arg],
        is_constructor: false,
    };
    let lowered =
        lower_expression(&expr, &VarLayout::default(), &functions).expect("function should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) + 2.0).abs() < 1e-12);
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
fn lower_discrete_rhs_supports_interval_intrinsic_for_clocked_varref_metadata() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .discrete_reals
        .insert(dae::VarName::new("PI.u"), scalar_var("PI.u"));
    dae_model.clock_intervals.insert("PI.u".to_string(), 0.1);
    dae_model.f_z.push(dae::Equation {
        lhs: Some(dae::VarName::new("PI.Ts")),
        rhs: dae::Expression::FunctionCall {
            name: dae::VarName::new("interval"),
            args: vec![dae::Expression::VarRef {
                name: dae::VarName::new("PI.u"),
                subscripts: vec![],
            }],
            is_constructor: false,
        },
        span: rumoca_core::Span::DUMMY,
        // MLS §16.5.1: interval(v) uses the associated clock interval of v.
        origin: "test interval metadata".to_string(),
        scalar_count: 1,
    });

    let layout = VarLayout::from_dae(&dae_model);
    let rows = lower_discrete_rhs(&dae_model, &layout).expect("interval(varref) should lower");
    let (_, output) = eval_linear_ops(&rows[0], &[], &[], 0.0);

    assert!((output.expect("row output") - 0.1).abs() < 1e-12);
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
fn lower_expression_supports_mod_and_rem_builtins() {
    let layout = VarLayout::default();
    for function in [dae::BuiltinFunction::Mod, dae::BuiltinFunction::Rem] {
        let expr = dae::Expression::BuiltinCall {
            function,
            args: vec![
                dae::Expression::Literal(dae::Literal::Real(-5.5)),
                dae::Expression::Literal(dae::Literal::Real(2.0)),
            ],
        };
        let lowered =
            lower_expression(&expr, &layout, &IndexMap::new()).expect("mod/rem should lower");
        let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
        let compiled = read_reg(&regs, lowered.result);
        let env = build_env(&dae::Dae::default(), &[], &[], 0.0);
        let interpreted = eval_expr::<f64>(&expr, &env);
        assert!(
            (compiled - interpreted).abs() < 1e-12,
            "builtin {} mismatch: compiled={compiled}, interpreted={interpreted}",
            function.name()
        );
    }
}

#[test]
fn lower_initial_residual_treats_initial_builtin_as_true() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .algebraics
        .insert(dae::VarName::new("x"), scalar_var("x"));
    dae_model.f_x.push(dae::Equation::residual(
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
    let (regs, out) = eval_linear_ops(&rows[0], &[0.0], &[], 0.0);
    assert_eq!(out.expect("output"), 1.0);
    assert_eq!(read_reg(&regs, 0), 1.0);
}

#[test]
fn lower_initial_expression_rows_treat_initial_builtin_as_true() {
    let expression = dae::Expression::If {
        branches: vec![(
            dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Initial,
                args: vec![],
            },
            dae::Expression::Literal(dae::Literal::Real(3.0)),
        )],
        else_branch: Box::new(dae::Expression::Literal(dae::Literal::Real(-1.0))),
    };
    let rows = lower_initial_expression_rows_from_expressions(
        &[expression],
        &VarLayout::default(),
        &IndexMap::new(),
    )
    .expect("initial-mode expression rows lower");
    assert_eq!(rows.len(), 1);
    let (_regs, out) = eval_linear_ops(&rows[0], &[], &[], 0.0);
    assert_eq!(out.expect("output"), 3.0);
}
