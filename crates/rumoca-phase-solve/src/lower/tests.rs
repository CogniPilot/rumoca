// SPEC_0021 file-size exception: split plan is to move focused lowerer
// regression fixtures into owned test modules after BOPTEST parity stabilization.
use super::{
    expression_rows::lower_expression_rows_with_mode, lower_derivative_rhs,
    lower_derivative_rhs_scalar_programs, lower_discrete_rhs, lower_expression,
    lower_expression_rows_from_expressions_with_runtime_metadata,
    lower_initial_expression_rows_from_expressions, lower_initial_residual, lower_residual,
    lower_root_conditions, lower_runtime_assignment_rhs,
};
use crate::layout::build_var_layout;
use crate::lower_solve_problem;
use indexmap::IndexMap;
use rumoca_ir_dae as dae;
use rumoca_ir_solve::{
    BinaryOp, CompareOp, ComputeNode, LinearOp, Reg, ScalarSlot, UnaryOp, VarLayout,
};

mod array_operator_tests;
mod event_intrinsics;
mod explicit_residual_tests;
mod function_expression_tests;
mod function_loop_tests;
mod intrinsics;
mod projection_runtime_tests;
mod root_condition_tests;

fn scalar_var(name: &str) -> dae::Variable {
    dae::Variable::new(rumoca_core::VarName::new(name))
}

fn insert_pre_parameter(dae_model: &mut dae::Dae, name: &str, dims: &[i64]) {
    let pre_name = format!("__pre__.{name}");
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new(&pre_name),
        dae::Variable {
            name: rumoca_core::VarName::new(&pre_name),
            dims: dims.to_vec(),
            fixed: Some(true),
            state_select: rumoca_core::StateSelect::Default,
            is_tunable: false,
            ..Default::default()
        },
    );
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

fn set_p_value(layout: &VarLayout, p: &mut [f64], name: &str, value: f64) {
    let Some(ScalarSlot::P { index, .. }) = layout.binding(name) else {
        panic!("expected parameter slot for {name}");
    };
    p[index] = value;
}

fn set_y_value(layout: &VarLayout, y: &mut [f64], name: &str, value: f64) {
    let Some(ScalarSlot::Y { index, .. }) = layout.binding(name) else {
        panic!("expected solver slot for {name}");
    };
    y[index] = value;
}

fn rounded_index(value: f64) -> i64 {
    (value + value.signum() * 0.5).trunc() as i64
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
    op.compare_as_f64(lhs, rhs)
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
            LinearOp::Move { dst, src } => {
                let value = read_reg(&regs, src);
                write_reg(&mut regs, dst, value);
            }
            LinearOp::LinearSolveComponent {
                dst,
                matrix_start,
                rhs_start,
                n,
                component,
            } => {
                let value =
                    eval_linear_solve_component(&regs, matrix_start, rhs_start, n, component);
                write_reg(&mut regs, dst, value);
            }
            LinearOp::TableBounds { dst, .. } => write_reg(&mut regs, dst, 0.0),
            LinearOp::TableLookup { dst, .. } => {
                write_reg(&mut regs, dst, 0.0);
            }
            LinearOp::TableLookupSlope { dst, .. } => {
                write_reg(&mut regs, dst, 0.0);
            }
            LinearOp::TableNextEvent { dst, .. } => write_reg(&mut regs, dst, f64::INFINITY),
            LinearOp::RandomInitialState { dst, .. }
            | LinearOp::RandomResult { dst, .. }
            | LinearOp::RandomState { dst, .. }
            | LinearOp::ImpureRandomInit { dst, .. }
            | LinearOp::ImpureRandom { dst, .. }
            | LinearOp::ImpureRandomInteger { dst, .. } => write_reg(&mut regs, dst, 1.0),
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

fn eval_linear_solve_component(
    regs: &[f64],
    matrix_start: Reg,
    rhs_start: Reg,
    n: usize,
    component: usize,
) -> f64 {
    let mut matrix = vec![0.0; n * n];
    let mut rhs = vec![0.0; n];
    for row in 0..n {
        rhs[row] = read_reg(regs, rhs_start + row as Reg);
        for col in 0..n {
            matrix[row * n + col] = read_reg(regs, matrix_start + (row * n + col) as Reg);
        }
    }
    for col in 0..n {
        let pivot = (col..n)
            .max_by(|&lhs, &rhs| {
                matrix[lhs * n + col]
                    .abs()
                    .total_cmp(&matrix[rhs * n + col].abs())
            })
            .unwrap();
        for entry in 0..n {
            matrix.swap(col * n + entry, pivot * n + entry);
        }
        rhs.swap(col, pivot);
        let pivot_value = matrix[col * n + col];
        for row in col + 1..n {
            let factor = matrix[row * n + col] / pivot_value;
            matrix[row * n + col] = 0.0;
            for entry in col + 1..n {
                matrix[row * n + entry] -= factor * matrix[col * n + entry];
            }
            rhs[row] -= factor * rhs[col];
        }
    }
    let mut solution = vec![0.0; n];
    for row in (0..n).rev() {
        let tail = ((row + 1)..n)
            .map(|col| matrix[row * n + col] * solution[col])
            .sum::<f64>();
        solution[row] = (rhs[row] - tail) / matrix[row * n + row];
    }
    solution[component]
}

fn component_ref(name: &str) -> rumoca_core::ComponentReference {
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

fn component_ref_index(name: &str, index: i64) -> rumoca_core::ComponentReference {
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

fn component_ref_indices(name: &str, indices: &[i64]) -> rumoca_core::ComponentReference {
    rumoca_core::ComponentReference {
        local: false,
        span: rumoca_core::Span::DUMMY,
        parts: vec![rumoca_core::ComponentRefPart {
            ident: name.to_string(),
            span: rumoca_core::Span::DUMMY,
            subs: indices
                .iter()
                .copied()
                .map(|index| {
                    rumoca_core::Subscript::generated_index(index, rumoca_core::Span::DUMMY)
                })
                .collect(),
        }],
        def_id: None,
    }
}

fn component_ref_index_expr(
    name: &str,
    index: rumoca_core::Expression,
) -> rumoca_core::ComponentReference {
    rumoca_core::ComponentReference {
        local: false,
        span: rumoca_core::Span::DUMMY,
        parts: vec![rumoca_core::ComponentRefPart {
            ident: name.to_string(),
            span: rumoca_core::Span::DUMMY,
            subs: vec![rumoca_core::Subscript::generated_expr(Box::new(index))],
        }],
        def_id: None,
    }
}

fn function_param(name: &str) -> rumoca_core::FunctionParam {
    rumoca_core::FunctionParam {
        def_id: None,
        name: name.to_string(),
        span: rumoca_core::Span::DUMMY,
        type_name: "Real".to_string(),
        type_class: None,
        dims: vec![],
        shape_expr: Vec::new(),
        default: None,
        description: None,
    }
}

fn function_param_with_dims(name: &str, dims: &[i64]) -> rumoca_core::FunctionParam {
    rumoca_core::FunctionParam {
        def_id: None,
        name: name.to_string(),
        span: rumoca_core::Span::DUMMY,
        type_name: "Real".to_string(),
        type_class: None,
        dims: dims.to_vec(),
        shape_expr: Vec::new(),
        default: None,
        description: None,
    }
}

fn var(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::VarName::new(name).into(),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    }
}

fn var_index(name: &str, index: i64) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::VarName::new(name).into(),
        subscripts: vec![rumoca_core::Subscript::generated_index(
            index,
            rumoca_core::Span::DUMMY,
        )],
        span: rumoca_core::Span::DUMMY,
    }
}

fn var_index_expr(name: &str, index: rumoca_core::Expression) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::VarName::new(name).into(),
        subscripts: vec![rumoca_core::Subscript::generated_expr(Box::new(index))],
        span: rumoca_core::Span::DUMMY,
    }
}

fn real_lit(value: f64) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(value),
        span: rumoca_core::Span::DUMMY,
    }
}

fn int_lit(value: i64) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Integer(value),
        span: rumoca_core::Span::DUMMY,
    }
}

fn pre_var(name: &str) -> rumoca_core::Expression {
    var(&format!("__pre__.{name}"))
}

fn indexed_var(name: &str, index: i64) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::VarName::new(name).into(),
        subscripts: vec![rumoca_core::Subscript::generated_index(
            index,
            rumoca_core::Span::DUMMY,
        )],
        span: rumoca_core::Span::DUMMY,
    }
}

fn der(expr: rumoca_core::Expression) -> rumoca_core::Expression {
    rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Der,
        args: vec![expr],
        span: rumoca_core::Span::DUMMY,
    }
}

fn size_expr(expr: rumoca_core::Expression, dim: i64) -> rumoca_core::Expression {
    rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Size,
        args: vec![
            expr,
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(dim),
                span: rumoca_core::Span::DUMMY,
            },
        ],
        span: rumoca_core::Span::DUMMY,
    }
}

fn binary(
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

fn add(lhs: rumoca_core::Expression, rhs: rumoca_core::Expression) -> rumoca_core::Expression {
    binary(rumoca_core::OpBinary::Add, lhs, rhs)
}

fn sub(lhs: rumoca_core::Expression, rhs: rumoca_core::Expression) -> rumoca_core::Expression {
    binary(rumoca_core::OpBinary::Sub, lhs, rhs)
}

fn mul(lhs: rumoca_core::Expression, rhs: rumoca_core::Expression) -> rumoca_core::Expression {
    binary(rumoca_core::OpBinary::Mul, lhs, rhs)
}

fn residual(rhs: rumoca_core::Expression) -> dae::Equation {
    residual_with_origin(rhs, "test residual")
}

fn residual_with_origin(rhs: rumoca_core::Expression, origin: &str) -> dae::Equation {
    dae::Equation {
        lhs: None,
        rhs,
        span: Default::default(),
        origin: origin.to_string(),
        scalar_count: 1,
    }
}

#[test]
fn lower_solve_problem_scalarizes_early_record_residual_after_state_vars() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    dae_model
        .variables
        .outputs
        .insert(rumoca_core::VarName::new("r.a"), scalar_var("r.a"));
    dae_model
        .variables
        .outputs
        .insert(rumoca_core::VarName::new("r.b"), scalar_var("r.b"));

    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: sub(
            var("r"),
            rumoca_core::Expression::Array {
                elements: vec![real_lit(1.0), real_lit(2.0)],
                is_matrix: false,
                span: rumoca_core::Span::DUMMY,
            },
        ),
        span: rumoca_core::Span::DUMMY,
        origin: "record residual before derivative rows".to_string(),
        scalar_count: 2,
    });
    dae_model
        .continuous
        .equations
        .push(residual(sub(der(var("x")), real_lit(0.0))));

    let problem = lower_solve_problem(&dae_model)
        .expect("filtered residual rows must not infer state-derivative behavior from row index");
    assert_eq!(problem.continuous.residual.len(), 2);
}

#[test]
fn lower_discrete_rhs_recovers_if_residual_assignment_value() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("z"), scalar_var("z"));
    dae_model
        .discrete
        .real_updates
        .push(residual(rumoca_core::Expression::If {
            branches: vec![(
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Boolean(true),
                    span: rumoca_core::Span::DUMMY,
                },
                // MLS §8.3.4: an if-equation branch may contain the same
                // assignment equation written in residual form.
                sub(
                    var("z"),
                    rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Real(2.0),
                        span: rumoca_core::Span::DUMMY,
                    },
                ),
            )],
            else_branch: Box::new(sub(
                var("z"),
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(3.0),
                    span: rumoca_core::Span::DUMMY,
                },
            )),
            span: rumoca_core::Span::DUMMY,
        }));

    let layout = build_var_layout(&dae_model);
    let rows = lower_discrete_rhs(&dae_model, &layout)
        .expect("conditional residual discrete update should lower");
    let (_, output) = eval_linear_ops(&rows[0], &[], &[0.0], 0.0);

    assert_eq!(output, Some(2.0));
}

#[test]
fn normalized_discrete_updates_orient_residual_alias_chain_once() {
    let mut dae_model = dae::Dae::default();
    for name in ["a", "b", "c", "d"] {
        dae_model
            .variables
            .discrete_reals
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
    for (lhs, rhs) in [("a", "b"), ("b", "c"), ("c", "d")] {
        dae_model.discrete.real_updates.push(residual_with_origin(
            sub(var(lhs), var(rhs)),
            &format!("connection equation: {lhs} = {rhs}"),
        ));
    }

    let equations = super::normalized_discrete_update_equations(&dae_model);
    let mut lhs_counts = IndexMap::<String, usize>::new();
    let mut aliases = IndexMap::<String, String>::new();
    for equation in &equations {
        let lhs = equation.lhs.as_ref().expect("alias equation has target");
        *lhs_counts.entry(lhs.as_str().to_string()).or_default() += 1;
        let rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } = &equation.rhs
        else {
            panic!("alias equation should preserve VarRef RHS");
        };
        assert!(subscripts.is_empty());
        aliases.insert(lhs.as_str().to_string(), name.as_str().to_string());
    }

    assert_eq!(lhs_counts.values().copied().max(), Some(1));
    assert_eq!(aliases.get("b").map(String::as_str), Some("a"));
    assert_eq!(aliases.get("c").map(String::as_str), Some("b"));
    assert_eq!(aliases.get("d").map(String::as_str), Some("c"));
}

#[test]
fn normalized_discrete_updates_preserve_explicit_difference_assignment() {
    let mut dae_model = dae::Dae::default();
    for name in ["a", "b", "x"] {
        dae_model
            .variables
            .discrete_reals
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("x")),
        rhs: sub(var("a"), var("b")),
        span: Default::default(),
        origin: "x = a - b".to_string(),
        scalar_count: 1,
    });

    let equations = super::normalized_discrete_update_equations(&dae_model);

    assert_eq!(equations.len(), 1);
    assert_eq!(equations[0].lhs.as_ref().map(|lhs| lhs.as_str()), Some("x"));
    assert!(
        matches!(
            equations[0].rhs,
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                ..
            }
        ),
        "explicit difference assignments must not be treated as residual aliases"
    );
}

#[test]
fn lower_expression_prunes_unreachable_static_if_branch() {
    let expr = rumoca_core::Expression::If {
        branches: vec![(
            binary(rumoca_core::OpBinary::Eq, int_lit(1), int_lit(1)),
            real_lit(2.0),
        )],
        else_branch: Box::new(var("missing.binding")),
        span: rumoca_core::Span::DUMMY,
    };

    let lowered = lower_expression(&expr, &VarLayout::default(), &IndexMap::new())
        .expect("statically unreachable else branch should not be lowered");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);

    assert_eq!(read_reg(&regs, lowered.result), 2.0);
}

#[test]
fn lower_discrete_rhs_expands_vectorized_update_rows() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("y"),
        dae::Variable {
            name: rumoca_core::VarName::new("y"),
            dims: vec![2],
            ..Default::default()
        },
    );
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y")),
        rhs: rumoca_core::Expression::Array {
            elements: vec![
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(1.0),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(2.0),
                    span: rumoca_core::Span::DUMMY,
                },
            ],
            is_matrix: false,
            span: rumoca_core::Span::DUMMY,
        },
        span: Default::default(),
        // MLS §8.3 and §10.6: array equations update one scalar slot per
        // element after solve-IR row lowering.
        origin: "vectorized discrete update".to_string(),
        scalar_count: 2,
    });

    let layout = build_var_layout(&dae_model);
    let rows = lower_discrete_rhs(&dae_model, &layout).expect("vectorized update should lower");
    let (_, first) = eval_linear_ops(&rows[0], &[], &[0.0, 0.0], 0.0);
    let (_, second) = eval_linear_ops(&rows[1], &[], &[0.0, 0.0], 0.0);

    assert_eq!(rows.len(), 2);
    assert_eq!(first, Some(1.0));
    assert_eq!(second, Some(2.0));
}

#[test]
fn lower_discrete_rhs_uses_first_output_for_array_function_expression() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("y"),
        dae::Variable {
            name: rumoca_core::VarName::new("y"),
            dims: vec![2],
            ..Default::default()
        },
    );
    dae_model.symbols.functions.insert(
        rumoca_core::VarName::new("My.memoryLike"),
        rumoca_core::Function {
            name: rumoca_core::VarName::new("My.memoryLike"),
            def_id: None,
            inputs: vec![],
            outputs: vec![
                function_param_with_dims("out", &[2]),
                function_param("line"),
                function_param("bit"),
            ],
            locals: vec![],
            body: vec![
                rumoca_core::Statement::Assignment {
                    comp: component_ref_index("out", 1),
                    value: real_lit(4.0),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Statement::Assignment {
                    comp: component_ref_index("out", 2),
                    value: real_lit(5.0),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Statement::Assignment {
                    comp: component_ref("line"),
                    value: real_lit(99.0),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Statement::Assignment {
                    comp: component_ref("bit"),
                    value: real_lit(100.0),
                    span: rumoca_core::Span::DUMMY,
                },
            ],
            is_constructor: false,
            pure: true,
            external: None,
            derivatives: vec![],
            span: Default::default(),
        },
    );
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y")),
        rhs: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("My.memoryLike").into(),
            args: vec![],
            is_constructor: false,
            span: rumoca_core::Span::DUMMY,
        },
        span: Default::default(),
        // MLS §12.4: an expression-form function call denotes the first
        // function output. Additional outputs are visible only via tuple
        // assignment/projection and must not widen array-valued expressions.
        origin: "array function first output".to_string(),
        scalar_count: 2,
    });

    let layout = build_var_layout(&dae_model);
    let rows = lower_discrete_rhs(&dae_model, &layout)
        .expect("array-valued first function output should lower");
    let (_, first) = eval_linear_ops(&rows[0], &[], &[0.0, 0.0], 0.0);
    let (_, second) = eval_linear_ops(&rows[1], &[], &[0.0, 0.0], 0.0);

    assert_eq!(rows.len(), 2);
    assert_eq!(first, Some(4.0));
    assert_eq!(second, Some(5.0));
}

#[test]
fn lower_discrete_rhs_projects_array_function_output_by_position() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("y"),
        dae::Variable {
            name: rumoca_core::VarName::new("y"),
            dims: vec![2],
            ..Default::default()
        },
    );
    dae_model.symbols.functions.insert(
        rumoca_core::VarName::new("My.memoryLike"),
        rumoca_core::Function {
            name: rumoca_core::VarName::new("My.memoryLike"),
            def_id: None,
            inputs: vec![],
            outputs: vec![
                function_param_with_dims("out", &[2]),
                function_param("line"),
                function_param("bit"),
            ],
            locals: vec![],
            body: vec![
                rumoca_core::Statement::Assignment {
                    comp: component_ref_index("out", 1),
                    value: real_lit(6.0),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Statement::Assignment {
                    comp: component_ref_index("out", 2),
                    value: real_lit(7.0),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Statement::Assignment {
                    comp: component_ref("line"),
                    value: real_lit(98.0),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Statement::Assignment {
                    comp: component_ref("bit"),
                    value: real_lit(99.0),
                    span: rumoca_core::Span::DUMMY,
                },
            ],
            is_constructor: false,
            pure: true,
            external: None,
            derivatives: vec![],
            span: Default::default(),
        },
    );
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y")),
        rhs: rumoca_core::Expression::Index {
            base: Box::new(rumoca_core::Expression::FunctionCall {
                name: rumoca_core::VarName::new("My.memoryLike").into(),
                args: vec![],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            }),
            subscripts: vec![rumoca_core::Subscript::generated_index(
                1,
                rumoca_core::Span::DUMMY,
            )],
            span: rumoca_core::Span::DUMMY,
        },
        span: Default::default(),
        // MLS §12.4: positional indexing on a multi-output function call is
        // output projection. `f()[1]` selects the first output, not the first
        // scalar element of that output.
        origin: "array function output projection".to_string(),
        scalar_count: 2,
    });

    let layout = build_var_layout(&dae_model);
    let rows = lower_discrete_rhs(&dae_model, &layout)
        .expect("array-valued positional output projection should lower");
    let (_, first) = eval_linear_ops(&rows[0], &[], &[0.0, 0.0], 0.0);
    let (_, second) = eval_linear_ops(&rows[1], &[], &[0.0, 0.0], 0.0);

    assert_eq!(rows.len(), 2);
    assert_eq!(first, Some(6.0));
    assert_eq!(second, Some(7.0));
}

#[test]
fn lower_discrete_rhs_recovers_dynamic_function_output_shape_from_assignments() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("y"),
        dae::Variable {
            name: rumoca_core::VarName::new("y"),
            dims: vec![2],
            ..Default::default()
        },
    );
    dae_model.symbols.functions.insert(
        rumoca_core::VarName::new("My.dynamicMemoryLike"),
        rumoca_core::Function {
            name: rumoca_core::VarName::new("My.dynamicMemoryLike"),
            def_id: None,
            inputs: vec![],
            outputs: vec![
                function_param_with_dims("out", &[0, 0]),
                function_param("line"),
                function_param("bit"),
            ],
            locals: vec![],
            body: vec![
                rumoca_core::Statement::Assignment {
                    comp: component_ref_indices("out", &[1, 1]),
                    value: real_lit(8.0),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Statement::Assignment {
                    comp: component_ref_indices("out", &[1, 2]),
                    value: real_lit(9.0),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Statement::Assignment {
                    comp: component_ref("line"),
                    value: real_lit(98.0),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Statement::Assignment {
                    comp: component_ref("bit"),
                    value: real_lit(99.0),
                    span: rumoca_core::Span::DUMMY,
                },
            ],
            is_constructor: false,
            pure: true,
            external: None,
            derivatives: vec![],
            span: Default::default(),
        },
    );
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y")),
        rhs: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("My.dynamicMemoryLike").into(),
            args: vec![],
            is_constructor: false,
            span: rumoca_core::Span::DUMMY,
        },
        span: Default::default(),
        // Some MSL functions, including Digital RAM memory loading, have
        // output dimensions derived from input parameters. If those dimensions
        // are not static in FunctionParam, the indexed output assignments still
        // define the array-valued function result shape.
        origin: "dynamic array function output".to_string(),
        scalar_count: 2,
    });

    let layout = build_var_layout(&dae_model);
    let rows = lower_discrete_rhs(&dae_model, &layout)
        .expect("indexed assignments should define dynamic output shape");
    let (_, first) = eval_linear_ops(&rows[0], &[], &[0.0, 0.0], 0.0);
    let (_, second) = eval_linear_ops(&rows[1], &[], &[0.0, 0.0], 0.0);

    assert_eq!(rows.len(), 2);
    assert_eq!(first, Some(8.0));
    assert_eq!(second, Some(9.0));
}

#[test]
fn lower_expression_indexes_array_function_expression_result() {
    let mut dae_model = dae::Dae::default();
    dae_model.symbols.functions.insert(
        rumoca_core::VarName::new("My.vectorResult"),
        rumoca_core::Function {
            name: rumoca_core::VarName::new("My.vectorResult"),
            def_id: None,
            inputs: vec![],
            outputs: vec![function_param_with_dims("out", &[2])],
            locals: vec![],
            body: vec![
                rumoca_core::Statement::Assignment {
                    comp: component_ref_index("out", 1),
                    value: real_lit(7.0),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Statement::Assignment {
                    comp: component_ref_index("out", 2),
                    value: real_lit(8.0),
                    span: rumoca_core::Span::DUMMY,
                },
            ],
            is_constructor: false,
            pure: true,
            external: None,
            derivatives: vec![],
            span: Default::default(),
        },
    );

    let expr = rumoca_core::Expression::Index {
        base: Box::new(rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("My.vectorResult").into(),
            args: vec![],
            is_constructor: false,
            span: rumoca_core::Span::DUMMY,
        }),
        subscripts: vec![rumoca_core::Subscript::generated_index(
            2,
            rumoca_core::Span::DUMMY,
        )],
        span: rumoca_core::Span::DUMMY,
    };

    let lowered = lower_expression(&expr, &VarLayout::default(), &dae_model.symbols.functions)
        .expect("indexed array-valued function expression should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);

    assert_eq!(read_reg(&regs, lowered.result), 8.0);
}

#[test]
fn lower_expression_indexes_array_literal_with_dynamic_subscript() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("row"), scalar_var("row"));

    let expr = rumoca_core::Expression::Index {
        base: Box::new(rumoca_core::Expression::Array {
            elements: vec![
                rumoca_core::Expression::Array {
                    elements: vec![real_lit(10.0), real_lit(20.0)],
                    is_matrix: false,
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Expression::Array {
                    elements: vec![real_lit(30.0), real_lit(40.0)],
                    is_matrix: false,
                    span: rumoca_core::Span::DUMMY,
                },
            ],
            is_matrix: true,
            span: rumoca_core::Span::DUMMY,
        }),
        subscripts: vec![
            rumoca_core::Subscript::generated_expr(Box::new(var("row"))),
            rumoca_core::Subscript::generated_expr(Box::new(int_lit(1))),
        ],
        span: rumoca_core::Span::DUMMY,
    };

    let layout = build_var_layout(&dae_model);
    let lowered = lower_expression(&expr, &layout, &dae_model.symbols.functions)
        .expect("dynamic array literal lookup should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[2.0], &[], 0.0);

    assert_eq!(read_reg(&regs, lowered.result), 30.0);
}

#[test]
fn lower_expression_accepts_singleton_projection_of_scalarized_expression() {
    let expr = rumoca_core::Expression::Index {
        base: Box::new(rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs: Box::new(real_lit(2.0)),
            rhs: Box::new(real_lit(3.0)),
            span: rumoca_core::Span::DUMMY,
        }),
        subscripts: vec![rumoca_core::Subscript::generated_index(
            1,
            rumoca_core::Span::DUMMY,
        )],
        span: rumoca_core::Span::DUMMY,
    };

    let lowered = lower_expression(&expr, &VarLayout::default(), &IndexMap::new())
        .expect("singleton projection of scalarized expression should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);

    assert_eq!(read_reg(&regs, lowered.result), 5.0);
}

#[test]
fn lower_expression_accepts_singleton_projection_of_scalar_function_call() {
    let expr = rumoca_core::Expression::Index {
        base: Box::new(rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("sin").into(),
            args: vec![real_lit(std::f64::consts::FRAC_PI_2)],
            is_constructor: false,
            span: rumoca_core::Span::DUMMY,
        }),
        subscripts: vec![rumoca_core::Subscript::generated_index(
            1,
            rumoca_core::Span::DUMMY,
        )],
        span: rumoca_core::Span::DUMMY,
    };

    let lowered = lower_expression(&expr, &VarLayout::default(), &IndexMap::new())
        .expect("singleton projection of scalar function call should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);

    assert!((read_reg(&regs, lowered.result) - 1.0).abs() < 1.0e-12);
}

#[test]
fn lower_residual_scalarizes_indexed_record_array_fields() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("y.re"),
        dae::Variable {
            name: rumoca_core::VarName::new("y.re"),
            dims: vec![1],
            ..Default::default()
        },
    );
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("y.im"),
        dae::Variable {
            name: rumoca_core::VarName::new("y.im"),
            dims: vec![1],
            ..Default::default()
        },
    );
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("u.re"), scalar_var("u.re"));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("u.im"), scalar_var("u.im"));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("scale"), scalar_var("scale"));
    dae_model
        .continuous
        .equations
        .push(dae::Equation::explicit_with_scalar_count(
            rumoca_core::VarName::new("y"),
            rumoca_core::Expression::Array {
                elements: vec![mul(var("scale"), var("u"))],
                is_matrix: false,
                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Span::DUMMY,
            "indexed record array residual",
            2,
        ));

    let layout = build_var_layout(&dae_model);
    let rows = lower_residual(&dae_model, &layout)
        .expect("array-of-record residual fields should lower through field then index");
    let mut y = vec![0.0; layout.y_scalars()];
    let mut p = vec![0.0; layout.p_scalars()];
    set_y_value(&layout, &mut y, "y.re[1]", 6.0);
    set_y_value(&layout, &mut y, "y.im[1]", 8.0);
    set_y_value(&layout, &mut y, "u.re", 3.0);
    set_y_value(&layout, &mut y, "u.im", 4.0);
    set_p_value(&layout, &mut p, "scale", 2.0);

    let (_, first) = eval_linear_ops(&rows[0], &y, &p, 0.0);
    let (_, second) = eval_linear_ops(&rows[1], &y, &p, 0.0);

    assert_eq!(rows.len(), 2);
    assert_eq!(first, Some(0.0));
    assert_eq!(second, Some(0.0));
}

#[test]
fn lower_expression_indexes_if_array_value_with_dynamic_subscript() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("choose"), scalar_var("choose"));
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("row"), scalar_var("row"));

    let expr = rumoca_core::Expression::Index {
        base: Box::new(rumoca_core::Expression::If {
            branches: vec![(
                var("choose"),
                rumoca_core::Expression::Array {
                    elements: vec![
                        rumoca_core::Expression::Array {
                            elements: vec![real_lit(10.0), real_lit(20.0)],
                            is_matrix: false,
                            span: rumoca_core::Span::DUMMY,
                        },
                        rumoca_core::Expression::Array {
                            elements: vec![real_lit(30.0), real_lit(40.0)],
                            is_matrix: false,
                            span: rumoca_core::Span::DUMMY,
                        },
                    ],
                    is_matrix: true,
                    span: rumoca_core::Span::DUMMY,
                },
            )],
            else_branch: Box::new(rumoca_core::Expression::Array {
                elements: vec![
                    rumoca_core::Expression::Array {
                        elements: vec![real_lit(50.0), real_lit(60.0)],
                        is_matrix: false,
                        span: rumoca_core::Span::DUMMY,
                    },
                    rumoca_core::Expression::Array {
                        elements: vec![real_lit(70.0), real_lit(80.0)],
                        is_matrix: false,
                        span: rumoca_core::Span::DUMMY,
                    },
                ],
                is_matrix: true,
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        }),
        subscripts: vec![
            rumoca_core::Subscript::generated_expr(Box::new(var("row"))),
            rumoca_core::Subscript::generated_expr(Box::new(int_lit(1))),
        ],
        span: rumoca_core::Span::DUMMY,
    };

    let layout = build_var_layout(&dae_model);
    let lowered = lower_expression(&expr, &layout, &dae_model.symbols.functions)
        .expect("dynamic lookup into if-selected array value should lower");
    let (true_regs, _) = eval_linear_ops(&lowered.ops, &[1.0, 2.0], &[], 0.0);
    let (false_regs, _) = eval_linear_ops(&lowered.ops, &[0.0, 1.0], &[], 0.0);

    assert_eq!(read_reg(&true_regs, lowered.result), 30.0);
    assert_eq!(read_reg(&false_regs, lowered.result), 50.0);
}

#[test]
fn lower_discrete_rhs_indexes_if_array_value_with_dynamic_slice() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("choose"), scalar_var("choose"));
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("row"), scalar_var("row"));
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("y"),
        dae::Variable {
            name: rumoca_core::VarName::new("y"),
            dims: vec![2],
            ..Default::default()
        },
    );

    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y")),
        rhs: rumoca_core::Expression::Index {
            base: Box::new(rumoca_core::Expression::If {
                branches: vec![(
                    var("choose"),
                    rumoca_core::Expression::Array {
                        elements: vec![
                            rumoca_core::Expression::Array {
                                elements: vec![real_lit(10.0), real_lit(20.0)],
                                is_matrix: false,
                                span: rumoca_core::Span::DUMMY,
                            },
                            rumoca_core::Expression::Array {
                                elements: vec![real_lit(30.0), real_lit(40.0)],
                                is_matrix: false,
                                span: rumoca_core::Span::DUMMY,
                            },
                        ],
                        is_matrix: true,
                        span: rumoca_core::Span::DUMMY,
                    },
                )],
                else_branch: Box::new(rumoca_core::Expression::Array {
                    elements: vec![
                        rumoca_core::Expression::Array {
                            elements: vec![real_lit(50.0), real_lit(60.0)],
                            is_matrix: false,
                            span: rumoca_core::Span::DUMMY,
                        },
                        rumoca_core::Expression::Array {
                            elements: vec![real_lit(70.0), real_lit(80.0)],
                            is_matrix: false,
                            span: rumoca_core::Span::DUMMY,
                        },
                    ],
                    is_matrix: true,
                    span: rumoca_core::Span::DUMMY,
                }),
                span: rumoca_core::Span::DUMMY,
            }),
            subscripts: vec![
                rumoca_core::Subscript::generated_expr(Box::new(var("row"))),
                rumoca_core::Subscript::generated_expr(Box::new(rumoca_core::Expression::Range {
                    start: Box::new(int_lit(1)),
                    step: None,
                    end: Box::new(int_lit(2)),
                    span: rumoca_core::Span::DUMMY,
                })),
            ],
            span: rumoca_core::Span::DUMMY,
        },
        span: Default::default(),
        origin: "dynamic if-selected array row slice".to_string(),
        scalar_count: 2,
    });

    let layout = build_var_layout(&dae_model);
    let rows = lower_discrete_rhs(&dae_model, &layout)
        .expect("dynamic slice from if-selected array value should lower");
    let (_, true_first) = eval_linear_ops(&rows[0], &[1.0, 2.0], &[], 0.0);
    let (_, true_second) = eval_linear_ops(&rows[1], &[1.0, 2.0], &[], 0.0);
    let (_, false_first) = eval_linear_ops(&rows[0], &[0.0, 1.0], &[], 0.0);
    let (_, false_second) = eval_linear_ops(&rows[1], &[0.0, 1.0], &[], 0.0);

    assert_eq!(true_first, Some(30.0));
    assert_eq!(true_second, Some(40.0));
    assert_eq!(false_first, Some(50.0));
    assert_eq!(false_second, Some(60.0));

    let nested_index = rumoca_core::Expression::Index {
        base: Box::new(dae_model.discrete.real_updates[0].rhs.clone()),
        subscripts: vec![rumoca_core::Subscript::generated_expr(Box::new(int_lit(2)))],
        span: rumoca_core::Span::DUMMY,
    };
    let lowered = lower_expression(&nested_index, &layout, &dae_model.symbols.functions)
        .expect("outer index over dynamic if-selected slice should lower");
    let (true_regs, _) = eval_linear_ops(&lowered.ops, &[1.0, 2.0], &[], 0.0);
    let (false_regs, _) = eval_linear_ops(&lowered.ops, &[0.0, 1.0], &[], 0.0);
    assert_eq!(read_reg(&true_regs, lowered.result), 40.0);
    assert_eq!(read_reg(&false_regs, lowered.result), 60.0);
}

#[test]
fn lower_discrete_rhs_expands_fill_branch_in_array_if_expression() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("u"), scalar_var("u"));
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("y"),
        dae::Variable {
            name: rumoca_core::VarName::new("y"),
            dims: vec![2],
            ..Default::default()
        },
    );
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y")),
        rhs: rumoca_core::Expression::If {
            branches: vec![(
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Boolean(true),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Fill,
                    args: vec![
                        var("u"),
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Integer(2),
                            span: rumoca_core::Span::DUMMY,
                        },
                    ],
                    span: rumoca_core::Span::DUMMY,
                },
            )],
            else_branch: Box::new(rumoca_core::Expression::Array {
                elements: vec![
                    rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Real(1.0),
                        span: rumoca_core::Span::DUMMY,
                    },
                    rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Real(2.0),
                        span: rumoca_core::Span::DUMMY,
                    },
                ],
                is_matrix: false,
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        span: Default::default(),
        // MLS §10.6.2: fill(s, n) constructs an n-element array, so it can
        // participate in array-valued conditional expressions of that shape.
        origin: "fill branch array if".to_string(),
        scalar_count: 2,
    });

    let layout = build_var_layout(&dae_model);
    let rows = lower_discrete_rhs(&dae_model, &layout).expect("fill branch should lower");
    let y = vec![3.5, 0.0, 0.0];
    let (_, first) = eval_linear_ops(&rows[0], &y, &[], 0.0);
    let (_, second) = eval_linear_ops(&rows[1], &y, &[], 0.0);

    assert_eq!(rows.len(), 2);
    assert_eq!(first, Some(3.5));
    assert_eq!(second, Some(3.5));
}

#[test]
fn lower_discrete_rhs_expands_previous_range_slice_in_array_if_expression() {
    let dae_model = previous_range_slice_array_if_model();
    let layout = build_var_layout(&dae_model);
    let rows = lower_discrete_rhs(&dae_model, &layout).expect("range slice branch should lower");
    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "u", 4.0);
    set_p_value(&layout, &mut p, "u_buffer[1]", 5.0);
    set_p_value(&layout, &mut p, "u_buffer[2]", 6.0);

    let (_, first) = eval_linear_ops(&rows[0], &[], &p, 0.0);
    let (_, second) = eval_linear_ops(&rows[1], &[], &p, 0.0);
    let (_, third) = eval_linear_ops(&rows[2], &[], &p, 0.0);

    assert_eq!(rows.len(), 3);
    assert_eq!(first, Some(4.0));
    assert_eq!(second, Some(5.0));
    assert_eq!(third, Some(6.0));
}

fn previous_range_slice_array_if_model() -> dae::Dae {
    let mut dae_model = dae::Dae::default();
    insert_previous_range_slice_variables(&mut dae_model);
    let previous_slice = previous_range_slice_expr();
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y")),
        rhs: previous_range_slice_branch_expr(previous_slice),
        span: Default::default(),
        // MLS §10.5 and §16.4: array subscripts may select ranges, and
        // previous(v[1:n]) preserves the selected array shape element-wise.
        origin: "previous range slice branch".to_string(),
        scalar_count: 3,
    });

    dae_model
}

fn insert_previous_range_slice_variables(dae_model: &mut dae::Dae) {
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("n"),
        dae::Variable {
            name: rumoca_core::VarName::new("n"),
            start: Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(2),
                span: rumoca_core::Span::DUMMY,
            }),
            is_tunable: false,
            ..Default::default()
        },
    );
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("u"), scalar_var("u"));
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("u_buffer"),
        dae::Variable {
            name: rumoca_core::VarName::new("u_buffer"),
            dims: vec![3],
            ..Default::default()
        },
    );
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("y"),
        dae::Variable {
            name: rumoca_core::VarName::new("y"),
            dims: vec![3],
            ..Default::default()
        },
    );
}

fn previous_range_slice_expr() -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("previous").into(),
        args: vec![rumoca_core::Expression::VarRef {
            name: rumoca_core::VarName::new("u_buffer").into(),
            subscripts: vec![rumoca_core::Subscript::generated_expr(Box::new(
                rumoca_core::Expression::Range {
                    start: Box::new(rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Integer(1),
                        span: rumoca_core::Span::DUMMY,
                    }),
                    step: None,
                    end: Box::new(var("n")),
                    span: rumoca_core::Span::DUMMY,
                },
            ))],
            span: rumoca_core::Span::DUMMY,
        }],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    }
}

fn previous_range_slice_branch_expr(
    previous_slice: rumoca_core::Expression,
) -> rumoca_core::Expression {
    rumoca_core::Expression::If {
        branches: vec![(
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Boolean(false),
                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Expression::Array {
                elements: vec![real_lit(9.0), real_lit(9.0), real_lit(9.0)],
                is_matrix: false,
                span: rumoca_core::Span::DUMMY,
            },
        )],
        else_branch: Box::new(rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Cat,
            args: vec![
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(1),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Expression::Array {
                    elements: vec![var("u")],
                    is_matrix: false,
                    span: rumoca_core::Span::DUMMY,
                },
                previous_slice,
            ],
            span: rumoca_core::Span::DUMMY,
        }),
        span: rumoca_core::Span::DUMMY,
    }
}

#[test]
fn lower_discrete_rhs_preserves_pre_array_branch_values() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("y"),
        dae::Variable {
            name: rumoca_core::VarName::new("y"),
            dims: vec![2],
            ..Default::default()
        },
    );
    insert_pre_parameter(&mut dae_model, "y", &[2]);
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y")),
        rhs: rumoca_core::Expression::If {
            branches: vec![(
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Boolean(false),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Expression::Array {
                    elements: vec![
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Real(1.0),
                            span: rumoca_core::Span::DUMMY,
                        },
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Real(2.0),
                            span: rumoca_core::Span::DUMMY,
                        },
                    ],
                    is_matrix: false,
                    span: rumoca_core::Span::DUMMY,
                },
            )],
            else_branch: Box::new(pre_var("y")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Default::default(),
        // DAE lowering rewrites pre(y) to the __pre__.y parameter array before
        // Solve-IR lowering; the branch must preserve scalar element values.
        origin: "vectorized pre branch".to_string(),
        scalar_count: 2,
    });

    let layout = build_var_layout(&dae_model);
    let rows = lower_discrete_rhs(&dae_model, &layout).expect("pre(array) branch should lower");
    let (_, first) = eval_linear_ops(&rows[0], &[], &[5.0, 6.0], 0.0);
    let (_, second) = eval_linear_ops(&rows[1], &[], &[5.0, 6.0], 0.0);

    assert_eq!(rows.len(), 2);
    assert_eq!(first, Some(5.0));
    assert_eq!(second, Some(6.0));
}

fn named_arg(name: &str, value: rumoca_core::Expression) -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new(format!("__rumoca_named_arg__.{name}")).into(),
        args: vec![value],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    }
}
