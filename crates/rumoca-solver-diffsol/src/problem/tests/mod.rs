use super::core::{
    InitJacobianEvalContext, apply_discrete_partition_updates, build_init_jacobian_colored,
    build_init_jacobian_dense, build_runtime_alias_adjacency, collect_runtime_alias_anchor_names,
    extract_direct_assignment, propagate_runtime_alias_components_from_env,
    propagate_runtime_direct_assignments_from_env, seed_direct_assignment_initial_values,
    seed_runtime_direct_assignment_values,
    seed_runtime_direct_assignment_values_with_context_and_env,
    seed_runtime_direct_assignment_values_with_context_and_env_and_blocked_solver_cols,
    solver_vector_names,
};
use super::*;
use crate::test_support::{binop, eq_from, lit, var};
use rumoca_core::Span;
use rumoca_ir_dae as dae;
mod blt_linear;
mod core;
mod jacobian;
mod runtime_projection_seed;
mod runtime_state_chain;

use blt_linear::apply_substitutions_to_env;
use jacobian::assert_jacobians_close;

fn named_arg_expr(name: &str, value: dae::Expression) -> dae::Expression {
    dae::Expression::FunctionCall {
        name: dae::VarName::new(format!("__rumoca_named_arg__.{name}")),
        args: vec![value],
        is_constructor: false,
    }
}

fn insert_parameter_start(dae: &mut dae::Dae, name: &str, dims: &[i64], start: dae::Expression) {
    let mut var = dae::Variable::new(dae::VarName::new(name));
    var.dims = dims.to_vec();
    var.start = Some(start);
    dae.parameters.insert(dae::VarName::new(name), var);
}

fn external_table_constructor_expr(
    constructor_name: &str,
    table_var: &str,
    columns_var: &str,
) -> dae::Expression {
    dae::Expression::FunctionCall {
        name: dae::VarName::new(constructor_name),
        args: vec![
            dae::Expression::Literal(dae::Literal::String("NoName".to_string())),
            dae::Expression::Literal(dae::Literal::String("NoName".to_string())),
            var(table_var),
            lit(0.0),
            var(columns_var),
            lit(1.0),
            lit(1.0),
            lit(0.0),
            lit(1.0),
            dae::Expression::Literal(dae::Literal::Boolean(false)),
            dae::Expression::Literal(dae::Literal::String(",".to_string())),
            lit(0.0),
        ],
        is_constructor: false,
    }
}

fn next_time_event_expr(table_id_var: &str) -> dae::Expression {
    dae::Expression::FunctionCall {
        name: dae::VarName::new("getNextTimeEvent"),
        args: vec![var(table_id_var), lit(0.0)],
        is_constructor: false,
    }
}

fn parameter_values_by_name(
    dae: &dae::Dae,
    params: &[f64],
) -> std::collections::HashMap<String, Vec<f64>> {
    let mut values_by_name = std::collections::HashMap::new();
    let mut pidx = 0usize;
    for (name, var) in &dae.parameters {
        let size = var.size();
        if size == 0 {
            continue;
        }
        values_by_name.insert(
            name.as_str().to_string(),
            params[pidx..pidx + size].to_vec(),
        );
        pidx += size;
    }
    values_by_name
}

fn boolean_table_mod_comprehension(shift_index: bool) -> dae::Expression {
    let mod_arg = if shift_index {
        binop(OpBinary::Add(Default::default()), var("i"), lit(1.0))
    } else {
        var("i")
    };
    dae::Expression::ArrayComprehension {
        expr: Box::new(dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Mod,
            args: vec![mod_arg, lit(2.0)],
        }),
        indices: vec![dae::ComprehensionIndex {
            name: "i".to_string(),
            range: dae::Expression::Range {
                start: Box::new(lit(1.0)),
                step: None,
                end: Box::new(var("booleanTable.n")),
            },
        }],
        filter: None,
    }
}

fn boolean_table_rows_expr(start_value: bool) -> dae::Expression {
    dae::Expression::Array {
        elements: vec![
            dae::Expression::Array {
                elements: vec![
                    var("booleanTable.table[1]"),
                    lit(if start_value { 1.0 } else { 0.0 }),
                ],
                is_matrix: true,
            },
            dae::Expression::Array {
                elements: vec![
                    var("booleanTable.table"),
                    boolean_table_mod_comprehension(start_value),
                ],
                is_matrix: true,
            },
        ],
        is_matrix: true,
    }
}

fn boolean_table_dynamic_table_start_expr() -> dae::Expression {
    dae::Expression::If {
        branches: vec![(
            binop(
                OpBinary::Gt(Default::default()),
                var("booleanTable.n"),
                lit(0.0),
            ),
            dae::Expression::If {
                branches: vec![(
                    var("booleanTable.startValue"),
                    boolean_table_rows_expr(true),
                )],
                else_branch: Box::new(boolean_table_rows_expr(false)),
            },
        )],
        else_branch: Box::new(dae::Expression::Array {
            elements: vec![lit(0.0), lit(0.0)],
            is_matrix: true,
        }),
    }
}

fn build_zero_sized_dynamic_boolean_time_table_dae() -> dae::Dae {
    let mut dae = dae::Dae::new();
    insert_parameter_start(
        &mut dae,
        "booleanTable.table",
        &[3],
        dae::Expression::Array {
            elements: vec![lit(1.0), lit(2.0), lit(3.0)],
            is_matrix: false,
        },
    );
    insert_parameter_start(
        &mut dae,
        "booleanTable.n",
        &[],
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Size,
            args: vec![var("booleanTable.table"), lit(1.0)],
        },
    );
    insert_parameter_start(
        &mut dae,
        "booleanTable.startValue",
        &[],
        dae::Expression::Literal(dae::Literal::Boolean(false)),
    );
    insert_parameter_start(
        &mut dae,
        "booleanTable.combiTimeTable.columns",
        &[1],
        dae::Expression::Array {
            elements: vec![lit(2.0)],
            is_matrix: false,
        },
    );
    insert_parameter_start(
        &mut dae,
        "booleanTable.combiTimeTable.table",
        &[0, 2],
        boolean_table_dynamic_table_start_expr(),
    );
    insert_parameter_start(
        &mut dae,
        "booleanTable.combiTimeTable.tableID",
        &[],
        external_table_constructor_expr(
            "ExternalCombiTimeTable",
            "booleanTable.combiTimeTable.table",
            "booleanTable.combiTimeTable.columns",
        ),
    );
    insert_parameter_start(
        &mut dae,
        "probe.nextEvent",
        &[],
        next_time_event_expr("booleanTable.combiTimeTable.tableID"),
    );
    dae
}

fn build_recheck_dynamic_boolean_time_table_dae() -> dae::Dae {
    let mut dae = dae::Dae::new();
    insert_parameter_start(
        &mut dae,
        "booleanTable.table",
        &[9],
        dae::Expression::Array {
            elements: vec![
                lit(1.0),
                lit(0.0),
                lit(2.0),
                lit(1.0),
                lit(3.0),
                lit(0.0),
                lit(4.0),
                lit(1.0),
                lit(5.0),
            ],
            is_matrix: false,
        },
    );
    insert_parameter_start(
        &mut dae,
        "booleanTable.n",
        &[],
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Size,
            args: vec![var("booleanTable.table"), lit(1.0)],
        },
    );
    insert_parameter_start(
        &mut dae,
        "booleanTable.combiTimeTable.columns",
        &[],
        lit(2.0),
    );
    insert_parameter_start(
        &mut dae,
        "booleanTable.combiTimeTable.tableID",
        &[],
        external_table_constructor_expr(
            "ExternalCombiTimeTable",
            "booleanTable.combiTimeTable.table",
            "booleanTable.combiTimeTable.columns",
        ),
    );
    insert_parameter_start(
        &mut dae,
        "booleanTable.combiTimeTable.table",
        &[0, 2],
        dae::Expression::Array {
            elements: vec![
                dae::Expression::ArrayComprehension {
                    expr: Box::new(var("i")),
                    indices: vec![dae::ComprehensionIndex {
                        name: "i".to_string(),
                        range: dae::Expression::Range {
                            start: Box::new(lit(1.0)),
                            step: None,
                            end: Box::new(var("booleanTable.n")),
                        },
                    }],
                    filter: None,
                },
                var("booleanTable.table"),
            ],
            is_matrix: true,
        },
    );
    insert_parameter_start(
        &mut dae,
        "probe.nextEvent",
        &[],
        next_time_event_expr("booleanTable.combiTimeTable.tableID"),
    );
    dae
}

fn build_compiled_external_table_constructor_probe_dae() -> dae::Dae {
    let mut dae = dae::Dae::new();
    insert_parameter_start(
        &mut dae,
        "integerTable.combiTimeTable.columns",
        &[],
        lit(2.0),
    );
    insert_parameter_start(
        &mut dae,
        "integerTable.combiTimeTable.table",
        &[6, 2],
        dae::Expression::Array {
            elements: vec![
                dae::Expression::Array {
                    elements: vec![lit(0.0), lit(0.0)],
                    is_matrix: true,
                },
                dae::Expression::Array {
                    elements: vec![lit(1.0), lit(2.0)],
                    is_matrix: true,
                },
                dae::Expression::Array {
                    elements: vec![lit(2.0), lit(4.0)],
                    is_matrix: true,
                },
                dae::Expression::Array {
                    elements: vec![lit(3.0), lit(6.0)],
                    is_matrix: true,
                },
                dae::Expression::Array {
                    elements: vec![lit(4.0), lit(4.0)],
                    is_matrix: true,
                },
                dae::Expression::Array {
                    elements: vec![lit(6.0), lit(2.0)],
                    is_matrix: true,
                },
            ],
            is_matrix: true,
        },
    );
    insert_parameter_start(
        &mut dae,
        "integerTable.combiTimeTable.tableID",
        &[],
        external_table_constructor_expr(
            "Modelica.Blocks.Types.ExternalCombiTimeTable",
            "integerTable.combiTimeTable.table",
            "integerTable.combiTimeTable.columns",
        ),
    );
    insert_parameter_start(
        &mut dae,
        "probe.nextEvent",
        &[],
        next_time_event_expr("integerTable.combiTimeTable.tableID"),
    );
    insert_parameter_start(
        &mut dae,
        "probe.count",
        &[],
        dae::Expression::FunctionCall {
            name: dae::VarName::new("Integer"),
            args: vec![lit(3.7)],
            is_constructor: false,
        },
    );
    dae
}

#[test]
fn test_count_empty_dae() {
    let dae = dae::Dae::new();
    assert_eq!(count_states(&dae), 0);
    assert_eq!(count_algebraics(&dae), 0);
    assert_eq!(count_parameters(&dae), 0);
}

#[test]
fn test_default_params_empty() {
    let dae = dae::Dae::new();
    let params = default_params(&dae);
    assert!(params.is_empty());
}

#[test]
fn test_default_params_support_capitalized_integer_intrinsic() {
    let mut dae = dae::Dae::new();
    let mut p = dae::Variable::new(dae::VarName::new("p"));
    p.start = Some(dae::Expression::FunctionCall {
        name: dae::VarName::new("Integer"),
        args: vec![lit(3.7)],
        is_constructor: false,
    });
    dae.parameters.insert(dae::VarName::new("p"), p);

    let params = default_params(&dae);
    assert_eq!(params, vec![3.0]);
}

#[test]
fn test_default_params_rewrite_hidden_direct_assignment_bindings() {
    let mut dae = dae::Dae::new();
    let mut u = dae::Variable::new(dae::VarName::new("u"));
    u.start = Some(lit(2.0));
    dae.inputs.insert(dae::VarName::new("u"), u);

    let mut p = dae::Variable::new(dae::VarName::new("p"));
    p.start = Some(binop(
        rumoca_ir_core::OpBinary::Add(Default::default()),
        var("hidden"),
        lit(1.0),
    ));
    dae.parameters.insert(dae::VarName::new("p"), p);

    dae.f_x.push(eq_from(binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        var("hidden"),
        var("u"),
    )));

    let params = default_params(&dae);
    assert_eq!(params, vec![3.0]);
}

#[test]
fn test_default_params_support_flattened_field_projection_array_refs() {
    let mut dae = dae::Dae::new();

    let mut ri = dae::Variable::new(dae::VarName::new("cellData.Ri"));
    ri.start = Some(lit(0.01));
    dae.parameters.insert(dae::VarName::new("cellData.Ri"), ri);

    let mut r1 = dae::Variable::new(dae::VarName::new("cellData.rcData[1].R"));
    r1.start = Some(lit(0.002));
    dae.parameters
        .insert(dae::VarName::new("cellData.rcData[1].R"), r1);

    let mut r2 = dae::Variable::new(dae::VarName::new("cellData.rcData[2].R"));
    r2.start = Some(lit(0.001));
    dae.parameters
        .insert(dae::VarName::new("cellData.rcData[2].R"), r2);

    let mut r0 = dae::Variable::new(dae::VarName::new("cellData.R0"));
    r0.start = Some(binop(
        OpBinary::Sub(Default::default()),
        var("cellData.Ri"),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Sum,
            args: vec![var("cellData.rcData.R")],
        },
    ));
    dae.parameters.insert(dae::VarName::new("cellData.R0"), r0);

    let params = default_params(&dae);
    assert_eq!(params, vec![0.01, 0.002, 0.001, 0.007]);
}

#[test]
fn test_default_params_support_strings_length_runtime_special() {
    let mut dae = dae::Dae::new();
    let mut p = dae::Variable::new(dae::VarName::new("p"));
    p.start = Some(dae::Expression::FunctionCall {
        name: dae::VarName::new("Modelica.Utilities.Strings.length"),
        args: vec![dae::Expression::Literal(dae::Literal::String(
            "hello".to_string(),
        ))],
        is_constructor: false,
    });
    dae.parameters.insert(dae::VarName::new("p"), p);

    let params = default_params(&dae);
    assert_eq!(params, vec![5.0]);
}

#[test]
fn test_default_params_support_full_path_name_runtime_placeholder() {
    let mut dae = dae::Dae::new();
    let mut p = dae::Variable::new(dae::VarName::new("p"));
    p.start = Some(dae::Expression::FunctionCall {
        name: dae::VarName::new("Modelica.Utilities.Files.fullPathName"),
        args: vec![dae::Expression::Literal(dae::Literal::String(
            "a.txt".to_string(),
        ))],
        is_constructor: false,
    });
    dae.parameters.insert(dae::VarName::new("p"), p);

    let params = default_params(&dae);
    assert_eq!(params, vec![0.0]);
}

#[test]
fn test_default_params_prefer_find_last_runtime_special_over_user_function_body() {
    let mut dae = dae::Dae::new();

    let mut find_last =
        dae::Function::new("Modelica.Utilities.Strings.findLast", Default::default());
    find_last
        .inputs
        .push(dae::FunctionParam::new("string", "String"));
    find_last
        .inputs
        .push(dae::FunctionParam::new("searchString", "String"));
    find_last
        .inputs
        .push(dae::FunctionParam::new("startIndex", "Integer"));
    find_last
        .inputs
        .push(dae::FunctionParam::new("caseSensitive", "Boolean"));
    find_last
        .outputs
        .push(dae::FunctionParam::new("index", "Integer"));
    find_last
        .body
        .push(dae::Statement::While(dae::StatementBlock {
            cond: dae::Expression::Literal(dae::Literal::Boolean(true)),
            stmts: vec![dae::Statement::Assignment {
                comp: dae::ComponentReference {
                    local: false,
                    parts: vec![dae::ComponentRefPart {
                        ident: "index".to_string(),
                        subs: vec![],
                    }],
                    def_id: None,
                },
                value: lit(1.0),
            }],
        }));
    dae.functions.insert(
        dae::VarName::new("Modelica.Utilities.Strings.findLast"),
        find_last,
    );

    let mut p = dae::Variable::new(dae::VarName::new("p"));
    p.start = Some(dae::Expression::FunctionCall {
        name: dae::VarName::new("Modelica.Utilities.Strings.findLast"),
        args: vec![
            dae::Expression::Literal(dae::Literal::String("ab.csv".to_string())),
            dae::Expression::Literal(dae::Literal::String(".csv".to_string())),
            named_arg_expr(
                "caseSensitive",
                dae::Expression::Literal(dae::Literal::Boolean(false)),
            ),
        ],
        is_constructor: false,
    });
    dae.parameters.insert(dae::VarName::new("p"), p);

    let params = default_params(&dae);
    assert_eq!(params, vec![3.0]);
}

#[test]
fn test_default_params_support_named_function_arguments_on_compiled_path() {
    let mut dae = dae::Dae::new();
    let mut function = dae::Function::new("Pkg.f", Default::default());
    function.inputs.push(dae::FunctionParam::new("a", "Real"));
    function.inputs.push(dae::FunctionParam::new("b", "Real"));
    function.outputs.push(dae::FunctionParam::new("y", "Real"));
    function.body.push(dae::Statement::Assignment {
        comp: dae::ComponentReference {
            local: false,
            parts: vec![dae::ComponentRefPart {
                ident: "y".to_string(),
                subs: vec![],
            }],
            def_id: None,
        },
        value: binop(
            rumoca_ir_core::OpBinary::Sub(Default::default()),
            var("a"),
            var("b"),
        ),
    });
    dae.functions.insert(dae::VarName::new("Pkg.f"), function);

    let mut p = dae::Variable::new(dae::VarName::new("p"));
    p.start = Some(dae::Expression::FunctionCall {
        name: dae::VarName::new("Pkg.f"),
        args: vec![named_arg_expr("b", lit(2.0)), named_arg_expr("a", lit(7.0))],
        is_constructor: false,
    });
    dae.parameters.insert(dae::VarName::new("p"), p);

    let params = default_params(&dae);
    assert_eq!(params, vec![5.0]);
}

#[test]
fn test_default_params_with_budget_rejects_non_finite_values() {
    let mut dae = dae::Dae::new();
    let mut p = dae::Variable::new(dae::VarName::new("p"));
    p.start = Some(lit(f64::NAN));
    dae.parameters.insert(dae::VarName::new("p"), p);

    let budget = rumoca_sim::TimeoutBudget::new(None);
    let err = default_params_with_budget(&dae, &budget)
        .expect_err("non-finite parameter evaluation should fail in budgeted path");
    assert!(
        matches!(err, crate::SimError::SolverError(ref msg) if msg.contains("non-finite parameter")),
        "unexpected error: {err:?}"
    );
}

#[test]
fn test_default_params_with_budget_falls_back_for_unsupported_start_rows() {
    let mut dae = dae::Dae::new();
    let mut p = dae::Variable::new(dae::VarName::new("p"));
    p.start = Some(dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Sample,
        args: vec![lit(1.0), lit(1.0)],
    });
    dae.parameters.insert(dae::VarName::new("p"), p);

    let budget = rumoca_sim::TimeoutBudget::new(None);
    let params = default_params_with_budget(&dae, &budget)
        .expect("unsupported compiled start rows should fall back to reference evaluation");
    assert_eq!(params, vec![0.0]);
}

#[test]
fn test_default_params_with_budget_resolves_self_contained_array_parameter_starts() {
    let mut dae = dae::Dae::new();

    let mut table = dae::Variable::new(dae::VarName::new("table"));
    table.dims = vec![3];
    table.start = Some(dae::Expression::Array {
        elements: vec![lit(10.0), lit(20.0), lit(30.0)],
        is_matrix: false,
    });
    dae.parameters.insert(dae::VarName::new("table"), table);

    let mut idx = dae::Variable::new(dae::VarName::new("idx"));
    idx.start = Some(lit(2.0));
    dae.parameters.insert(dae::VarName::new("idx"), idx);

    let mut selected = dae::Variable::new(dae::VarName::new("selected"));
    selected.start = Some(dae::Expression::Index {
        base: Box::new(var("table")),
        subscripts: vec![dae::Subscript::Expr(Box::new(
            dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Integer,
                args: vec![var("idx")],
            },
        ))],
    });
    dae.parameters
        .insert(dae::VarName::new("selected"), selected);

    let budget = rumoca_sim::TimeoutBudget::new(None);
    let params = default_params_with_budget(&dae, &budget)
        .expect("self-contained array parameter starts should still seed dependent parameters");
    assert_eq!(params, vec![10.0, 20.0, 30.0, 2.0, 20.0]);
}

#[test]
fn test_default_params_with_budget_allows_infinite_parameter_defaults() {
    let mut dae = dae::Dae::new();
    let mut p = dae::Variable::new(dae::VarName::new("p"));
    p.start = Some(lit(f64::NEG_INFINITY));
    dae.parameters.insert(dae::VarName::new("p"), p);

    let budget = rumoca_sim::TimeoutBudget::new(None);
    let params = default_params_with_budget(&dae, &budget)
        .expect("infinite parameter defaults are valid Modelica sentinels");
    assert_eq!(params.len(), 1);
    assert!(params[0].is_infinite() && params[0].is_sign_negative());
}

#[test]
fn test_default_params_with_budget_allows_transient_forward_ref_nan_in_pass1() {
    let mut dae = dae::Dae::new();

    let mut p0 = dae::Variable::new(dae::VarName::new("p0"));
    // Before forward refs are populated, this is 0/0 -> NaN in pass 1.
    p0.start = Some(binop(
        rumoca_ir_core::OpBinary::Div(Default::default()),
        binop(
            rumoca_ir_core::OpBinary::Mul(Default::default()),
            var("p1"),
            var("p2"),
        ),
        binop(
            rumoca_ir_core::OpBinary::Sub(Default::default()),
            var("p1"),
            var("p2"),
        ),
    ));
    dae.parameters.insert(dae::VarName::new("p0"), p0);

    let mut p1 = dae::Variable::new(dae::VarName::new("p1"));
    p1.start = Some(lit(1.0));
    dae.parameters.insert(dae::VarName::new("p1"), p1);

    let mut p2 = dae::Variable::new(dae::VarName::new("p2"));
    p2.start = Some(lit(2.0));
    dae.parameters.insert(dae::VarName::new("p2"), p2);

    let budget = rumoca_sim::TimeoutBudget::new(None);
    let params = default_params_with_budget(&dae, &budget)
        .expect("pass-2 forward-reference re-evaluation should resolve transient NaN");
    assert_eq!(params.len(), 3);
    assert!((params[0] + 2.0).abs() < 1e-12);
    assert!((params[1] - 1.0).abs() < 1e-12);
    assert!((params[2] - 2.0).abs() < 1e-12);
}

#[test]
fn test_default_params_with_budget_resolves_multi_level_forward_refs_to_avoid_nan() {
    let mut dae = dae::Dae::new();

    // Lsigma = (1 - ratio) * L
    let mut l_sigma = dae::Variable::new(dae::VarName::new("Lsigma"));
    l_sigma.start = Some(binop(
        rumoca_ir_core::OpBinary::Mul(Default::default()),
        binop(
            rumoca_ir_core::OpBinary::Sub(Default::default()),
            lit(1.0),
            var("ratio"),
        ),
        var("L"),
    ));
    dae.parameters.insert(dae::VarName::new("Lsigma"), l_sigma);

    // L = 1 / f
    let mut l = dae::Variable::new(dae::VarName::new("L"));
    l.start = Some(binop(
        rumoca_ir_core::OpBinary::Div(Default::default()),
        lit(1.0),
        var("f"),
    ));
    dae.parameters.insert(dae::VarName::new("L"), l);

    // f = fs
    let mut f = dae::Variable::new(dae::VarName::new("f"));
    f.start = Some(var("fs"));
    dae.parameters.insert(dae::VarName::new("f"), f);

    // fs declared after f to force multi-level forward-reference settling.
    let mut fs = dae::Variable::new(dae::VarName::new("fs"));
    fs.start = Some(lit(50.0));
    dae.parameters.insert(dae::VarName::new("fs"), fs);

    let mut ratio = dae::Variable::new(dae::VarName::new("ratio"));
    ratio.start = Some(lit(1.0));
    dae.parameters.insert(dae::VarName::new("ratio"), ratio);

    let budget = rumoca_sim::TimeoutBudget::new(None);
    let params = default_params_with_budget(&dae, &budget)
        .expect("multi-level forward references should converge to finite values");
    assert!(
        params[0].is_finite(),
        "Lsigma should be finite, got {}",
        params[0]
    );
    assert!(
        params[1].is_finite(),
        "L should be finite, got {}",
        params[1]
    );
    assert!((params[0] - 0.0).abs() < 1e-12);
    assert!((params[2] - 50.0).abs() < 1e-12);
}

#[test]
fn test_default_params_constant_array_index_uses_selected_entry() {
    let mut dae = dae::Dae::new();

    let mut conversion_table = dae::Variable::new(dae::VarName::new("conversionTable"));
    conversion_table.dims = vec![3];
    conversion_table.start = Some(dae::Expression::Array {
        elements: vec![lit(31536000.0), lit(3600.0), lit(1000.0)],
        is_matrix: false,
    });
    dae.constants
        .insert(dae::VarName::new("conversionTable"), conversion_table);

    let mut resolution = dae::Variable::new(dae::VarName::new("resolution"));
    resolution.start = Some(lit(3.0));
    dae.parameters
        .insert(dae::VarName::new("resolution"), resolution);

    let mut resolution_factor = dae::Variable::new(dae::VarName::new("resolutionFactor"));
    resolution_factor.start = Some(dae::Expression::Index {
        base: Box::new(var("conversionTable")),
        subscripts: vec![dae::Subscript::Expr(Box::new(
            dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Integer,
                args: vec![var("resolution")],
            },
        ))],
    });
    dae.parameters
        .insert(dae::VarName::new("resolutionFactor"), resolution_factor);

    let budget = rumoca_sim::TimeoutBudget::new(None);
    let params = default_params_with_budget(&dae, &budget)
        .expect("constant array indexing should resolve in parameter starts");

    let mut pidx = 0usize;
    let mut resolution_value = None;
    let mut resolution_factor_value = None;
    for (name, var) in &dae.parameters {
        if var.size() > 1 {
            pidx += var.size();
            continue;
        }
        let value = params[pidx];
        if name.as_str() == "resolution" {
            resolution_value = Some(value);
        } else if name.as_str() == "resolutionFactor" {
            resolution_factor_value = Some(value);
        }
        pidx += 1;
    }

    let resolution_value = resolution_value.expect("resolution parameter missing");
    let resolution_factor_value =
        resolution_factor_value.expect("resolutionFactor parameter missing");

    assert!((resolution_value - 3.0).abs() < 1e-12);
    assert!(
        (resolution_factor_value - 1000.0).abs() < 1e-12,
        "expected indexed conversion table entry 1000.0, got {resolution_factor_value}"
    );
}

#[test]
fn test_default_params_preserve_matrix_start_row_order_in_parameter_env() {
    let mut dae = dae::Dae::new();

    let mut table = dae::Variable::new(dae::VarName::new("timeTable.table"));
    table.dims = vec![6, 2];
    // MLS Chapter 10 array literals preserve written row order; the compiled
    // parameter/start path must feed the same row-major flattened values into
    // the runtime env that interpreted array evaluation would produce.
    table.start = Some(dae::Expression::Array {
        elements: vec![
            dae::Expression::Array {
                elements: vec![lit(0.0), lit(0.0)],
                is_matrix: false,
            },
            dae::Expression::Array {
                elements: vec![lit(1.0), lit(2.1)],
                is_matrix: false,
            },
            dae::Expression::Array {
                elements: vec![lit(2.0), lit(4.2)],
                is_matrix: false,
            },
            dae::Expression::Array {
                elements: vec![lit(3.0), lit(6.3)],
                is_matrix: false,
            },
            dae::Expression::Array {
                elements: vec![lit(4.0), lit(4.2)],
                is_matrix: false,
            },
            dae::Expression::Array {
                elements: vec![lit(6.0), lit(2.1)],
                is_matrix: false,
            },
        ],
        is_matrix: true,
    });
    dae.parameters
        .insert(dae::VarName::new("timeTable.table"), table);

    let params = default_params(&dae);
    assert_eq!(
        params,
        vec![0.0, 0.0, 1.0, 2.1, 2.0, 4.2, 3.0, 6.3, 4.0, 4.2, 6.0, 2.1]
    );

    let env = rumoca_eval_dae::runtime::build_runtime_parameter_tail_env(&dae, &params, 0.0);
    assert_eq!(env.get("timeTable.table[1,1]"), 0.0);
    assert_eq!(env.get("timeTable.table[1,2]"), 0.0);
    assert_eq!(env.get("timeTable.table[2,1]"), 1.0);
    assert!((env.get("timeTable.table[2,2]") - 2.1).abs() < 1e-12);
}

#[test]
fn test_default_params_preserve_matrix_start_row_order_with_matrix_rows() {
    let mut dae = dae::Dae::new();

    let mut table = dae::Variable::new(dae::VarName::new("timeTable.table"));
    table.dims = vec![7, 2];
    table.start = Some(dae::Expression::Array {
        elements: vec![
            dae::Expression::Array {
                elements: vec![lit(0.0), lit(0.0)],
                is_matrix: true,
            },
            dae::Expression::Array {
                elements: vec![lit(1.0), lit(2.1)],
                is_matrix: true,
            },
            dae::Expression::Array {
                elements: vec![lit(2.0), lit(4.2)],
                is_matrix: true,
            },
            dae::Expression::Array {
                elements: vec![lit(3.0), lit(6.3)],
                is_matrix: true,
            },
            dae::Expression::Array {
                elements: vec![lit(4.0), lit(4.2)],
                is_matrix: true,
            },
            dae::Expression::Array {
                elements: vec![lit(6.0), lit(2.1)],
                is_matrix: true,
            },
            dae::Expression::Array {
                elements: vec![lit(6.0), lit(2.1)],
                is_matrix: true,
            },
        ],
        is_matrix: true,
    });
    dae.parameters
        .insert(dae::VarName::new("timeTable.table"), table);

    let params = default_params(&dae);
    assert_eq!(
        params,
        vec![
            0.0, 0.0, 1.0, 2.1, 2.0, 4.2, 3.0, 6.3, 4.0, 4.2, 6.0, 2.1, 6.0, 2.1
        ]
    );

    let env = rumoca_eval_dae::runtime::build_runtime_parameter_tail_env(&dae, &params, 0.0);
    assert_eq!(env.get("timeTable.table[1,1]"), 0.0);
    assert_eq!(env.get("timeTable.table[1,2]"), 0.0);
    assert_eq!(env.get("timeTable.table[2,1]"), 1.0);
    assert!((env.get("timeTable.table[2,2]") - 2.1).abs() < 1e-12);
}

#[test]
fn test_default_params_materialize_zero_sized_dynamic_time_table_starts() {
    let dae = build_zero_sized_dynamic_boolean_time_table_dae();
    let params = default_params(&dae);
    let values_by_name = parameter_values_by_name(&dae, &params);

    let table_id = values_by_name["booleanTable.combiTimeTable.tableID"][0];
    let next_event = values_by_name["probe.nextEvent"][0];
    assert!(
        table_id > 0.0,
        "expected registered external table id, got {table_id}"
    );
    assert!(
        (next_event - 1.0).abs() < 1e-12,
        "expected first time event at 1.0, got {next_event}"
    );
}

#[test]
fn test_default_params_recheck_earlier_table_id_after_dynamic_table_materializes() {
    let dae = build_recheck_dynamic_boolean_time_table_dae();
    let params = default_params(&dae);
    let values_by_name = parameter_values_by_name(&dae, &params);

    let table_id = values_by_name["booleanTable.combiTimeTable.tableID"][0];
    let next_event = values_by_name["probe.nextEvent"][0];
    assert!(
        table_id > 0.0,
        "expected registered external table id, got {table_id}"
    );
    assert!(
        (next_event - 1.0).abs() < 1.0e-12,
        "expected first time event at 1.0, got {next_event}"
    );
}

#[test]
fn test_default_params_support_qualified_external_time_table_constructor() {
    let mut dae = dae::Dae::new();

    let mut columns = dae::Variable::new(dae::VarName::new("integerTable.combiTimeTable.columns"));
    columns.start = Some(lit(2.0));
    dae.parameters.insert(
        dae::VarName::new("integerTable.combiTimeTable.columns"),
        columns,
    );

    let mut table = dae::Variable::new(dae::VarName::new("integerTable.combiTimeTable.table"));
    table.dims = vec![6, 2];
    table.start = Some(dae::Expression::Array {
        elements: vec![
            dae::Expression::Array {
                elements: vec![lit(0.0), lit(0.0)],
                is_matrix: true,
            },
            dae::Expression::Array {
                elements: vec![lit(1.0), lit(2.0)],
                is_matrix: true,
            },
            dae::Expression::Array {
                elements: vec![lit(2.0), lit(4.0)],
                is_matrix: true,
            },
            dae::Expression::Array {
                elements: vec![lit(3.0), lit(6.0)],
                is_matrix: true,
            },
            dae::Expression::Array {
                elements: vec![lit(4.0), lit(4.0)],
                is_matrix: true,
            },
            dae::Expression::Array {
                elements: vec![lit(6.0), lit(2.0)],
                is_matrix: true,
            },
        ],
        is_matrix: true,
    });
    dae.parameters.insert(
        dae::VarName::new("integerTable.combiTimeTable.table"),
        table,
    );

    let mut table_id = dae::Variable::new(dae::VarName::new("integerTable.combiTimeTable.tableID"));
    table_id.start = Some(dae::Expression::FunctionCall {
        name: dae::VarName::new("Modelica.Blocks.Types.ExternalCombiTimeTable"),
        args: vec![
            dae::Expression::Literal(dae::Literal::String("NoName".to_string())),
            dae::Expression::Literal(dae::Literal::String("NoName".to_string())),
            var("integerTable.combiTimeTable.table"),
            lit(0.0),
            var("integerTable.combiTimeTable.columns"),
            lit(1.0),
            lit(1.0),
            lit(0.0),
            lit(1.0),
            dae::Expression::Literal(dae::Literal::Boolean(false)),
            dae::Expression::Literal(dae::Literal::String(",".to_string())),
            lit(0.0),
        ],
        is_constructor: false,
    });
    dae.parameters.insert(
        dae::VarName::new("integerTable.combiTimeTable.tableID"),
        table_id,
    );

    let mut next_event = dae::Variable::new(dae::VarName::new("probe.nextEvent"));
    next_event.start = Some(dae::Expression::FunctionCall {
        name: dae::VarName::new("getNextTimeEvent"),
        args: vec![var("integerTable.combiTimeTable.tableID"), lit(0.0)],
        is_constructor: false,
    });
    dae.parameters
        .insert(dae::VarName::new("probe.nextEvent"), next_event);

    let params = default_params(&dae);
    let mut values_by_name = std::collections::HashMap::new();
    let mut pidx = 0usize;
    for (name, var) in &dae.parameters {
        let sz = var.size();
        if sz == 0 {
            continue;
        }
        values_by_name.insert(name.as_str().to_string(), params[pidx..pidx + sz].to_vec());
        pidx += sz;
    }

    let table_id = values_by_name["integerTable.combiTimeTable.tableID"][0];
    let next_event = values_by_name["probe.nextEvent"][0];
    assert!(
        table_id > 0.0,
        "expected registered external table id, got {table_id}"
    );
    assert!(
        (next_event - 1.0).abs() < 1.0e-12,
        "expected first time event at 1.0, got {next_event}"
    );
}

#[test]
fn test_default_params_skip_compiled_external_table_constructor_rows() {
    let dae = build_compiled_external_table_constructor_probe_dae();
    let ctx = build_compiled_var_start_context(
        &dae,
        dae.parameters
            .iter()
            .map(|(name, var)| (name.as_str().to_string(), var.clone())),
    )
    .expect("compiled start context should build for mixed rows")
    .expect("mixed start rows should still produce a compiled context");

    assert!(
        !ctx.rows_by_name
            .contains_key("integerTable.combiTimeTable.tableID"),
        "external table constructor rows must stay on the reference path"
    );
    assert!(
        ctx.rows_by_name.contains_key("probe.nextEvent"),
        "compiled getters should remain available after skipping constructor rows"
    );
    assert!(
        !ctx.rows_by_name.contains_key("probe.count"),
        "self-contained scalar starts should stay on the reference path"
    );
}

#[test]
fn test_default_params_initial_parameter_pass_skips_zero_sized_array_slots() {
    let mut dae = dae::Dae::new();

    let mut dyn_table = dae::Variable::new(dae::VarName::new("dyn.table"));
    dyn_table.dims = vec![0, 2];
    dyn_table.start = Some(dae::Expression::Array {
        elements: vec![
            dae::Expression::Array {
                elements: vec![lit(0.0), lit(1.0)],
                is_matrix: true,
            },
            dae::Expression::Array {
                elements: vec![lit(1.0), lit(2.0)],
                is_matrix: true,
            },
        ],
        is_matrix: true,
    });
    dae.parameters
        .insert(dae::VarName::new("dyn.table"), dyn_table);

    let mut p = dae::Variable::new(dae::VarName::new("p"));
    p.start = Some(lit(3.0));
    dae.parameters.insert(dae::VarName::new("p"), p);

    let mut q = dae::Variable::new(dae::VarName::new("q"));
    q.start = Some(lit(4.0));
    dae.parameters.insert(dae::VarName::new("q"), q);

    // MLS §8.6 initialization equations operate on the realized parameter
    // vector; zero-sized array declarations must not consume phantom scalar
    // slots during that pass.
    dae.initial_equations.push(dae::Equation::explicit(
        dae::VarName::new("q"),
        var("p"),
        Span::DUMMY,
        "test",
    ));

    let params = default_params(&dae);
    assert_eq!(params, vec![3.0, 3.0]);
}

#[test]
fn test_default_params_broadcast_array_parameter_start_chain() {
    let mut dae = dae::Dae::new();

    let mut c = dae::Variable::new(dae::VarName::new("c"));
    c.start = Some(lit(2.0));
    dae.constants.insert(dae::VarName::new("c"), c);

    let mut p = dae::Variable::new(dae::VarName::new("p"));
    p.start = Some(binop(OpBinary::Add(Default::default()), var("c"), lit(1.0)));
    dae.parameters.insert(dae::VarName::new("p"), p);

    let mut arr = dae::Variable::new(dae::VarName::new("arr"));
    arr.dims = vec![2];
    arr.start = Some(var("p"));
    dae.parameters.insert(dae::VarName::new("arr"), arr);

    let params = default_params(&dae);
    assert_eq!(params, vec![3.0, 3.0, 3.0]);
}

#[test]
fn test_initialize_state_vector_respects_state_then_algebraic_order() {
    let mut dae = dae::Dae::new();
    dae.states.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    dae.algebraics.insert(
        dae::VarName::new("z"),
        dae::Variable::new(dae::VarName::new("z")),
    );
    let mut y = vec![1.0, 2.0];
    initialize_state_vector(&dae, &mut y);
    assert_eq!(y, vec![0.0, 0.0]);
}

#[test]
fn test_initialize_state_vector_uses_runtime_tail_parameter_chain() {
    let mut dae = dae::Dae::new();

    let mut c = dae::Variable::new(dae::VarName::new("c"));
    c.start = Some(lit(2.0));
    dae.constants.insert(dae::VarName::new("c"), c);

    let mut p = dae::Variable::new(dae::VarName::new("p"));
    p.start = Some(binop(OpBinary::Add(Default::default()), var("c"), lit(1.0)));
    dae.parameters.insert(dae::VarName::new("p"), p);

    let mut x = dae::Variable::new(dae::VarName::new("x"));
    x.start = Some(binop(OpBinary::Add(Default::default()), var("p"), lit(2.0)));
    dae.states.insert(dae::VarName::new("x"), x);

    let mut y = vec![0.0; 1];
    initialize_state_vector(&dae, &mut y);
    assert!((y[0] - 5.0).abs() < 1.0e-12);
}

#[test]
fn test_initialize_state_vector_rewrites_known_direct_assignment_chain() {
    let mut dae = dae::Dae::new();

    let mut u = dae::Variable::new(dae::VarName::new("u"));
    u.start = Some(lit(2.0));
    dae.inputs.insert(dae::VarName::new("u"), u);

    dae.algebraics.insert(
        dae::VarName::new("a"),
        dae::Variable::new(dae::VarName::new("a")),
    );
    dae.outputs.insert(
        dae::VarName::new("y"),
        dae::Variable::new(dae::VarName::new("y")),
    );

    let mut x = dae::Variable::new(dae::VarName::new("x"));
    x.start = Some(binop(OpBinary::Add(Default::default()), var("y"), lit(1.0)));
    dae.states.insert(dae::VarName::new("x"), x);

    dae.f_x.push(eq_from(binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        var("a"),
        var("u"),
    )));
    dae.f_x.push(eq_from(binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        var("y"),
        var("a"),
    )));

    let mut y = vec![0.0; 3];
    initialize_state_vector(&dae, &mut y);
    assert!((y[0] - 3.0).abs() < 1.0e-12);
}

#[test]
fn test_initialize_state_vector_rewrites_missing_live_alias_binding() {
    let mut dae = dae::Dae::new();

    dae.outputs.insert(
        dae::VarName::new("manualSeed1_y"),
        dae::Variable::new(dae::VarName::new("manualSeed1_y")),
    );

    let mut probe = dae::Variable::new(dae::VarName::new("probe"));
    probe.start = Some(binop(
        OpBinary::Add(Default::default()),
        var("manualSeed1.y"),
        lit(1.0),
    ));
    dae.algebraics.insert(dae::VarName::new("probe"), probe);

    dae.f_x.push(dae::Equation {
        lhs: Some(dae::VarName::new("manualSeed1_y")),
        rhs: var("manualSeed1.y"),
        span: rumoca_core::Span::DUMMY,
        origin: String::new(),
        scalar_count: 1,
    });

    let mut y = vec![0.0; 2];
    initialize_state_vector(&dae, &mut y);
    assert!((y[0] - 1.0).abs() < 1.0e-12);
}

#[test]
fn test_initialize_state_vector_scalarizes_array_start_values() {
    let mut dae = dae::Dae::new();

    let mut p = dae::Variable::new(dae::VarName::new("p"));
    p.start = Some(lit(2.0));
    dae.parameters.insert(dae::VarName::new("p"), p);

    let mut x = dae::Variable::new(dae::VarName::new("x"));
    x.dims = vec![2];
    x.start = Some(dae::Expression::Array {
        elements: vec![
            var("p"),
            binop(OpBinary::Add(Default::default()), var("p"), lit(1.0)),
        ],
        is_matrix: false,
    });
    dae.states.insert(dae::VarName::new("x"), x);

    let mut y = vec![0.0; 2];
    initialize_state_vector(&dae, &mut y);
    assert_eq!(y, vec![2.0, 3.0]);
}

#[test]
fn test_initialize_state_vector_falls_back_when_compiled_start_row_is_unsupported() {
    let mut dae = dae::Dae::new();
    let mut x = dae::Variable::new(dae::VarName::new("x"));
    x.start = Some(dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Sample,
        args: vec![lit(1.0), lit(1.0)],
    });
    dae.states.insert(dae::VarName::new("x"), x);

    let mut y = vec![0.0];
    initialize_state_vector(&dae, &mut y);
    assert_eq!(y, vec![0.0]);
}

#[test]
fn test_expr_contains_der_of_varref_direct() {
    // der(x) — VarRef directly inside BuiltinCall
    let expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Der,
        args: vec![dae::Expression::VarRef {
            name: dae::VarName::new("x"),
            subscripts: vec![],
        }],
    };
    assert!(expr_contains_der_of(&expr, &dae::VarName::new("x")));
    assert!(!expr_contains_der_of(&expr, &dae::VarName::new("y")));
}

#[test]
fn test_expr_contains_der_of_varref_with_subscripts() {
    // der(x[1]) as VarRef { name: "x", subscripts: [Index(1)] }
    let expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Der,
        args: vec![dae::Expression::VarRef {
            name: dae::VarName::new("x"),
            subscripts: vec![dae::Subscript::Index(1)],
        }],
    };
    assert!(expr_contains_der_of(&expr, &dae::VarName::new("x")));
}

#[test]
fn test_expr_contains_der_of_varref_with_subscripts_does_not_cross_match_other_index() {
    let expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Der,
        args: vec![dae::Expression::VarRef {
            name: dae::VarName::new("x"),
            subscripts: vec![dae::Subscript::Index(1)],
        }],
    };
    assert!(!expr_contains_der_of(&expr, &dae::VarName::new("x[2]")));
}

#[test]
fn test_expr_contains_der_of_index_wrapping_varref() {
    // der(x[1]) as Index { base: VarRef { name: "x" }, subscripts: [Index(1)] }
    let expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Der,
        args: vec![dae::Expression::Index {
            base: Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("x"),
                subscripts: vec![],
            }),
            subscripts: vec![dae::Subscript::Index(1)],
        }],
    };
    assert!(expr_contains_der_of(&expr, &dae::VarName::new("x")));
    assert!(!expr_contains_der_of(&expr, &dae::VarName::new("y")));
}

#[test]
fn test_component_base_name_simple() {
    assert_eq!(dae::component_base_name("x").as_deref(), Some("x"));
    assert_eq!(dae::component_base_name("x[1]").as_deref(), Some("x"));
    assert_eq!(dae::component_base_name("x[1][2]").as_deref(), Some("x"));
}

#[test]
fn test_component_base_name_mid_path() {
    assert_eq!(
        dae::component_base_name("support[1].phi").as_deref(),
        Some("support.phi")
    );
    assert_eq!(
        dae::component_base_name("a[1].b[2].c").as_deref(),
        Some("a.b.c")
    );
    assert_eq!(
        dae::component_base_name("foo.bar[3].baz").as_deref(),
        Some("foo.bar.baz")
    );
}

#[test]
fn test_expr_refers_to_var_mid_path_subscript() {
    // der(support[1].phi) should match state "support.phi"
    let expr = dae::Expression::VarRef {
        name: dae::VarName::new("support[1].phi"),
        subscripts: vec![],
    };
    assert!(expr_refers_to_var(&expr, &dae::VarName::new("support.phi")));
    assert!(!expr_refers_to_var(&expr, &dae::VarName::new("other.phi")));
}

#[test]
fn test_expr_refers_to_var_trailing_subscript() {
    // "x[1]" should match "x"
    let expr = dae::Expression::VarRef {
        name: dae::VarName::new("x[1]"),
        subscripts: vec![],
    };
    assert!(expr_refers_to_var(&expr, &dae::VarName::new("x")));
}

#[test]
fn test_expr_refers_to_var_indexed_target_requires_exact_index() {
    let expr = dae::Expression::VarRef {
        name: dae::VarName::new("x[1]"),
        subscripts: vec![],
    };
    assert!(!expr_refers_to_var(&expr, &dae::VarName::new("x[2]")));
}

#[test]
fn test_expr_refers_to_var_indexed_component_requires_exact_index() {
    let expr = dae::Expression::VarRef {
        name: dae::VarName::new("support[1].phi.im"),
        subscripts: vec![],
    };
    assert!(!expr_refers_to_var(
        &expr,
        &dae::VarName::new("support[2].phi.im")
    ));
}

#[test]
fn test_extract_direct_assignment_with_indexed_target() {
    let rhs = dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Sub(Default::default()),
        lhs: Box::new(dae::Expression::VarRef {
            name: dae::VarName::new("aux"),
            subscripts: vec![dae::Subscript::Index(1)],
        }),
        rhs: Box::new(lit(2.5)),
    };
    let (target, solution) =
        extract_direct_assignment(&rhs).expect("expected direct assignment extraction");
    assert_eq!(target, "aux[1]");
    assert!((eval_expr::<f64>(solution, &VarEnv::new()) - 2.5).abs() < 1e-12);
}

mod jacobian_and_newton;
mod seed_runtime;
