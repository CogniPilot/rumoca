use super::{
    DirectAssignmentValue, LowerBuilder, Scope, expression_rows::lower_expression_rows_with_mode,
    lower_derivative_rhs, lower_derivative_rhs_scalar_programs, lower_discrete_rhs,
    lower_expression, lower_expression_rows_from_expressions_with_runtime_metadata,
    lower_initial_expression_rows_from_expressions, lower_initial_residual, lower_residual,
    lower_root_conditions, lower_runtime_assignment_rhs,
};
use crate::layout::build_var_layout;
use crate::lower_solve_problem;
use indexmap::IndexMap;
use rumoca_core::ComponentPath;
use rumoca_ir_dae as dae;
use rumoca_ir_solve::{
    BinaryOp, CompareOp, ComputeNode, IndexedScalarSlot, LinearOp, Reg, ScalarSlot, UnaryOp,
    VarLayout,
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

#[test]
fn scalar_context_full_slice_loads_single_indexed_binding() {
    let mut bindings = IndexMap::new();
    bindings.insert(
        "a[1]".to_string(),
        ScalarSlot::Y {
            index: 0,
            byte_offset: 0,
        },
    );
    let indexed_bindings = IndexMap::from([(
        ComponentPath::from_flat_path("a"),
        vec![IndexedScalarSlot {
            indices: vec![1],
            slot: ScalarSlot::Y {
                index: 0,
                byte_offset: 0,
            },
        }],
    )]);
    let layout = VarLayout::from_parts_with_shapes_and_indexed_bindings(
        bindings,
        IndexMap::new(),
        indexed_bindings,
        1,
        0,
    );
    let expr = rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new("a"),
        subscripts: vec![rumoca_core::Subscript::generated_colon(
            rumoca_core::Span::DUMMY,
        )],
        span: rumoca_core::Span::DUMMY,
    };

    let lowered =
        lower_expression(&expr, &layout, &IndexMap::new()).expect("singleton slice should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[42.0], &[], 0.0);

    assert_eq!(read_reg(&regs, lowered.result), 42.0);
}

#[test]
fn direct_assignment_empty_scope_reuses_lowered_values() {
    let mut dae_model = dae::Dae::new();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("u"), scalar_var("u"));
    let layout = build_var_layout(&dae_model);
    let mut direct_assignments = IndexMap::new();
    direct_assignments.insert(
        "x".to_string(),
        DirectAssignmentValue::full(rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs: Box::new(var("u")),
            rhs: Box::new(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(1.0),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        }),
    );
    let mut builder = LowerBuilder::new(&layout, &dae_model.symbols.functions)
        .with_direct_assignments(direct_assignments);
    let scope = Scope::new();

    let first = builder
        .lower_array_like_values(&var("x"), &scope, 0)
        .expect("first direct assignment read should lower");
    let ops_after_first = builder.ops.len();
    let second = builder
        .lower_array_like_values(&var("x"), &scope, 0)
        .expect("second direct assignment read should hit cache");

    assert_eq!(first, second);
    assert_eq!(builder.ops.len(), ops_after_first);
}

#[test]
fn scalarized_component_child_slots_use_shared_index() {
    let mut dae_model = dae::Dae::new();
    for name in ["rec.a", "rec.b", "other"] {
        dae_model
            .variables
            .algebraics
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
    let layout = build_var_layout(&dae_model);
    let mut builder = LowerBuilder::new(&layout, &dae_model.symbols.functions);
    let base_path = ComponentPath::from_flat_path("rec");

    let first = builder.scalarized_component_child_slots(&base_path);
    let second = builder.scalarized_component_child_slots(&base_path);
    let names = first
        .iter()
        .map(|(path, _slot)| path.as_str())
        .collect::<Vec<_>>();

    assert_eq!(first, second);
    assert_eq!(names, vec!["rec.a", "rec.b"]);
}

#[test]
fn scalar_context_full_slice_index_loads_single_indexed_binding() {
    let mut bindings = IndexMap::new();
    bindings.insert(
        "a[1]".to_string(),
        ScalarSlot::Y {
            index: 0,
            byte_offset: 0,
        },
    );
    let indexed_bindings = IndexMap::from([(
        ComponentPath::from_flat_path("a"),
        vec![IndexedScalarSlot {
            indices: vec![1],
            slot: ScalarSlot::Y {
                index: 0,
                byte_offset: 0,
            },
        }],
    )]);
    let layout = VarLayout::from_parts_with_shapes_and_indexed_bindings(
        bindings,
        IndexMap::new(),
        indexed_bindings,
        1,
        0,
    );
    let expr = rumoca_core::Expression::Index {
        base: Box::new(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("a"),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }),
        subscripts: vec![rumoca_core::Subscript::generated_colon(
            rumoca_core::Span::DUMMY,
        )],
        span: rumoca_core::Span::DUMMY,
    };

    let lowered =
        lower_expression(&expr, &layout, &IndexMap::new()).expect("singleton slice should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[42.0], &[], 0.0);

    assert_eq!(read_reg(&regs, lowered.result), 42.0);
}

#[test]
fn residual_rows_project_nested_indexed_record_field_slice() {
    let mut dae_model = dae::Dae::new();
    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::new("T"),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(rumoca_core::Expression::FieldAccess {
                base: Box::new(rumoca_core::Expression::FieldAccess {
                    base: Box::new(rumoca_core::Expression::FieldAccess {
                        base: Box::new(rumoca_core::Expression::Index {
                            base: Box::new(rumoca_core::Expression::VarRef {
                                name: rumoca_core::Reference::new("ele"),
                                subscripts: vec![],
                                span: rumoca_core::Span::DUMMY,
                            }),
                            subscripts: vec![rumoca_core::Subscript::generated_colon(
                                rumoca_core::Span::DUMMY,
                            )],
                            span: rumoca_core::Span::DUMMY,
                        }),
                        field: "con1".to_string(),
                        span: rumoca_core::Span::DUMMY,
                    }),
                    field: "solid".to_string(),
                    span: rumoca_core::Span::DUMMY,
                }),
                field: "T".to_string(),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        origin: "binding equation for T".to_string(),
        span: rumoca_core::Span::DUMMY,
        scalar_count: 2,
    });
    let mut bindings = IndexMap::new();
    for (name, index) in [
        ("T", 0usize),
        ("ele[1].con1.solid.T", 2),
        ("ele[2].con1.solid.T", 3),
    ] {
        bindings.insert(
            name.to_string(),
            ScalarSlot::Y {
                index,
                byte_offset: index * std::mem::size_of::<f64>(),
            },
        );
    }
    let layout = VarLayout::from_parts_with_shapes(
        bindings,
        IndexMap::from([("T".to_string(), vec![2])]),
        4,
        0,
    );

    let rows = lower_residual(&dae_model, &layout).expect("nested record field slice lowers");

    assert_eq!(rows.len(), 2);
    let (_, row0) = eval_linear_ops(&rows[0], &[10.0, 20.0, 1.0, 2.0], &[], 0.0);
    let (_, row1) = eval_linear_ops(&rows[1], &[10.0, 20.0, 1.0, 2.0], &[], 0.0);
    assert_eq!(row0, Some(9.0));
    assert_eq!(row1, Some(18.0));
}

#[test]
fn scalar_if_uses_structural_parameter_to_skip_missing_connector_branch() {
    let mut dae_model = dae::Dae::new();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("vol.p"), scalar_var("vol.p"));
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("vol.nPorts"),
        dae::Variable {
            name: rumoca_core::VarName::new("vol.nPorts"),
            start: Some(int_lit(0)),
            is_tunable: false,
            ..Default::default()
        },
    );
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("vol.p_start"),
        dae::Variable {
            name: rumoca_core::VarName::new("vol.p_start"),
            start: Some(real_lit(101325.0)),
            is_tunable: false,
            ..Default::default()
        },
    );

    let missing_port_pressure = rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::Index {
            base: Box::new(rumoca_core::Expression::FieldAccess {
                base: Box::new(var("vol")),
                field: "ports".to_string(),
                span: rumoca_core::Span::DUMMY,
            }),
            subscripts: vec![rumoca_core::Subscript::generated_index(
                1,
                rumoca_core::Span::DUMMY,
            )],
            span: rumoca_core::Span::DUMMY,
        }),
        field: "p".to_string(),
        span: rumoca_core::Span::DUMMY,
    };
    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(var("vol.p")),
            rhs: Box::new(rumoca_core::Expression::If {
                branches: vec![(
                    rumoca_core::Expression::Binary {
                        op: rumoca_core::OpBinary::Gt,
                        lhs: Box::new(var("vol.nPorts")),
                        rhs: Box::new(int_lit(0)),
                        span: rumoca_core::Span::DUMMY,
                    },
                    missing_port_pressure,
                )],
                else_branch: Box::new(var("vol.p_start")),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        origin: "binding equation for vol.p".to_string(),
        span: rumoca_core::Span::DUMMY,
        scalar_count: 1,
    });

    let layout = build_var_layout(&dae_model);
    let rows = lower_residual(&dae_model, &layout)
        .expect("structural false branch must not lower missing connector array");
    let mut y = vec![0.0; layout.y_scalars()];
    let mut p = vec![0.0; layout.p_scalars()];
    set_y_value(&layout, &mut y, "vol.p", 101335.0);
    set_p_value(&layout, &mut p, "vol.p_start", 101325.0);

    let (_, row0) = eval_linear_ops(&rows[0], &y, &p, 0.0);

    assert_eq!(rows.len(), 1);
    assert_eq!(row0, Some(10.0));
}

#[test]
fn residual_lowering_skips_zero_scalar_count_equation() {
    let mut dae_model = dae::Dae::new();
    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: var("missing.zero.width"),
        origin: "zero-width medium composition".to_string(),
        span: rumoca_core::Span::DUMMY,
        scalar_count: 0,
    });

    let layout = build_var_layout(&dae_model);
    let rows = lower_residual(&dae_model, &layout)
        .expect("zero-width residual equations should not lower expressions");

    assert!(rows.is_empty());
}

#[test]
fn array_comprehension_index_projects_indexed_record_field() {
    let mut dae_model = dae::Dae::new();
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("mWat_flow"),
        scalar_var("mWat_flow"),
    );
    for name in ["ele[1].vol2.mWat_flow", "ele[2].vol2.mWat_flow"] {
        dae_model
            .variables
            .algebraics
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("nEle"),
        dae::Variable {
            name: rumoca_core::VarName::new("nEle"),
            start: Some(int_lit(2)),
            is_tunable: false,
            ..Default::default()
        },
    );

    let ele_i = rumoca_core::Expression::Index {
        base: Box::new(var("ele")),
        subscripts: vec![rumoca_core::Subscript::Expr {
            expr: Box::new(var("i")),
            span: rumoca_core::Span::DUMMY,
        }],
        span: rumoca_core::Span::DUMMY,
    };
    let rhs = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Sum,
        args: vec![rumoca_core::Expression::ArrayComprehension {
            expr: Box::new(rumoca_core::Expression::FieldAccess {
                base: Box::new(rumoca_core::Expression::FieldAccess {
                    base: Box::new(ele_i),
                    field: "vol2".to_string(),
                    span: rumoca_core::Span::DUMMY,
                }),
                field: "mWat_flow".to_string(),
                span: rumoca_core::Span::DUMMY,
            }),
            indices: vec![rumoca_core::ComprehensionIndex {
                name: "i".to_string(),
                range: rumoca_core::Expression::Range {
                    start: Box::new(int_lit(1)),
                    step: None,
                    end: Box::new(var("nEle")),
                    span: rumoca_core::Span::DUMMY,
                },
            }],
            filter: None,
            span: rumoca_core::Span::DUMMY,
        }],
        span: rumoca_core::Span::DUMMY,
    };
    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(var("mWat_flow")),
            rhs: Box::new(rhs),
            span: rumoca_core::Span::DUMMY,
        },
        origin: "binding equation for mWat_flow".to_string(),
        span: rumoca_core::Span::DUMMY,
        scalar_count: 1,
    });

    let layout = build_var_layout(&dae_model);
    let rows = lower_residual(&dae_model, &layout)
        .expect("array comprehension index should project record field bindings");
    let mut y = vec![0.0; layout.y_scalars()];
    set_y_value(&layout, &mut y, "mWat_flow", 7.0);
    set_y_value(&layout, &mut y, "ele[1].vol2.mWat_flow", 2.0);
    set_y_value(&layout, &mut y, "ele[2].vol2.mWat_flow", 3.0);

    let (_, row0) = eval_linear_ops(&rows[0], &y, &[], 0.0);

    assert_eq!(rows.len(), 1);
    assert_eq!(row0, Some(2.0));
}

#[test]
fn scalar_binding_projects_child_array_field_from_last_target_index() {
    let mut dae_model = dae::Dae::new();
    for name in [
        "zone[2].TAirIn[1].y",
        "zone[2].ports[1].p",
        "zone[2].ports[2].p",
    ] {
        dae_model
            .variables
            .algebraics
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }

    dae_model.continuous.equations.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("zone[2].TAirIn[1].y")),
        rhs: rumoca_core::Expression::FieldAccess {
            base: Box::new(rumoca_core::Expression::FieldAccess {
                base: Box::new(rumoca_core::Expression::Index {
                    base: Box::new(var("zone")),
                    subscripts: vec![rumoca_core::Subscript::generated_index(
                        2,
                        rumoca_core::Span::DUMMY,
                    )],
                    span: rumoca_core::Span::DUMMY,
                }),
                field: "ports".to_string(),
                span: rumoca_core::Span::DUMMY,
            }),
            field: "p".to_string(),
            span: rumoca_core::Span::DUMMY,
        },
        origin: "binding equation for zone[2].TAirIn[1].y".to_string(),
        span: rumoca_core::Span::DUMMY,
        scalar_count: 1,
    });

    let layout = build_var_layout(&dae_model);
    let rows = lower_residual(&dae_model, &layout)
        .expect("target index should project child array field access");
    let mut y = vec![0.0; layout.y_scalars()];
    set_y_value(&layout, &mut y, "zone[2].TAirIn[1].y", 500000.0);
    set_y_value(&layout, &mut y, "zone[2].ports[1].p", 101001.0);
    set_y_value(&layout, &mut y, "zone[2].ports[2].p", 202002.0);

    let (_, row0) = eval_linear_ops(&rows[0], &y, &[], 0.0);

    assert_eq!(rows.len(), 1);
    assert_eq!(row0, Some(398999.0));
}

#[test]
fn scalar_binding_projects_base_vector_alias_field_from_target_slot() {
    let mut dae_model = dae::Dae::new();
    for name in [
        "coil.T1",
        "coil.T1[1]",
        "coil.T1[2]",
        "coil.ele[1].vol1.T",
        "coil.ele[2].vol1.T",
    ] {
        dae_model
            .variables
            .algebraics
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }

    let rhs = rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::FieldAccess {
            base: Box::new(rumoca_core::Expression::Index {
                base: Box::new(var("coil.ele")),
                subscripts: vec![rumoca_core::Subscript::generated_expr(Box::new(
                    rumoca_core::Expression::Range {
                        start: Box::new(int_lit(1)),
                        step: None,
                        end: Box::new(int_lit(2)),
                        span: rumoca_core::Span::DUMMY,
                    },
                ))],
                span: rumoca_core::Span::DUMMY,
            }),
            field: "vol1".to_string(),
            span: rumoca_core::Span::DUMMY,
        }),
        field: "T".to_string(),
        span: rumoca_core::Span::DUMMY,
    };
    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: sub(var("coil.T1"), rhs),
        origin: "binding equation for coil.T1".to_string(),
        span: rumoca_core::Span::DUMMY,
        scalar_count: 1,
    });

    let layout = build_var_layout(&dae_model);
    let rows = lower_residual(&dae_model, &layout)
        .expect("base vector alias field projection should lower");
    let mut y = vec![0.0; layout.y_scalars()];
    set_y_value(&layout, &mut y, "coil.T1", 100.0);
    set_y_value(&layout, &mut y, "coil.ele[1].vol1.T", 95.0);
    set_y_value(&layout, &mut y, "coil.ele[2].vol1.T", 90.0);

    let (_, row0) = eval_linear_ops(&rows[0], &y, &[], 0.0);

    assert_eq!(rows.len(), 1);
    assert_eq!(row0, Some(5.0));

    lower_solve_problem(&dae_model).expect("base vector alias solve problem should lower");
}

#[test]
fn size_builtin_of_scalar_literal_lowers_to_one() {
    let dae_model = dae::Dae::new();
    let layout = build_var_layout(&dae_model);
    let expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Size,
        args: vec![real_lit(2.0), int_lit(1)],
        span: rumoca_core::Span::DUMMY,
    };

    let lowered = lower_expression(&expr, &layout, &dae_model.symbols.functions)
        .expect("size of scalar literal should lower as scalar extent");
    let mut ops = lowered.ops;
    ops.push(LinearOp::StoreOutput {
        src: lowered.result,
    });
    let (_, value) = eval_linear_ops(&ops, &[], &[], 0.0);

    assert_eq!(value, Some(1.0));
}

#[test]
fn singleton_index_of_scalar_literal_lowers_to_literal() {
    let dae_model = dae::Dae::new();
    let layout = build_var_layout(&dae_model);
    let expr = rumoca_core::Expression::Index {
        base: Box::new(real_lit(2.5)),
        subscripts: vec![rumoca_core::Subscript::generated_index(
            1,
            rumoca_core::Span::DUMMY,
        )],
        span: rumoca_core::Span::DUMMY,
    };

    let lowered = lower_expression(&expr, &layout, &dae_model.symbols.functions)
        .expect("singleton scalar literal projection should lower");
    let mut ops = lowered.ops;
    ops.push(LinearOp::StoreOutput {
        src: lowered.result,
    });
    let (_, value) = eval_linear_ops(&ops, &[], &[], 0.0);

    assert_eq!(value, Some(2.5));
}

#[test]
fn for_loop_range_accepts_size_of_scalar_literal() {
    let mut dae_model = dae::Dae::new();
    dae_model.symbols.functions.insert(
        rumoca_core::VarName::new("Pkg.loopScalarSize"),
        rumoca_core::Function {
            name: rumoca_core::VarName::new("Pkg.loopScalarSize"),
            def_id: None,
            inputs: Vec::new(),
            outputs: vec![function_param("out")],
            locals: Vec::new(),
            body: vec![
                rumoca_core::Statement::Assignment {
                    comp: component_ref("out"),
                    value: real_lit(0.0),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Statement::For {
                    indices: vec![rumoca_core::ForIndex {
                        ident: "i".to_string(),
                        range: rumoca_core::Expression::Range {
                            start: Box::new(int_lit(1)),
                            step: None,
                            end: Box::new(rumoca_core::Expression::BuiltinCall {
                                function: rumoca_core::BuiltinFunction::Size,
                                args: vec![real_lit(2.0), int_lit(1)],
                                span: rumoca_core::Span::DUMMY,
                            }),
                            span: rumoca_core::Span::DUMMY,
                        },
                    }],
                    equations: vec![rumoca_core::Statement::Assignment {
                        comp: component_ref("out"),
                        value: add(var("out"), real_lit(1.0)),
                        span: rumoca_core::Span::DUMMY,
                    }],
                    span: rumoca_core::Span::DUMMY,
                },
            ],
            is_constructor: false,
            pure: true,
            external: None,
            derivatives: Vec::new(),
            span: rumoca_core::Span::DUMMY,
        },
    );
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("Pkg.loopScalarSize"),
        args: Vec::new(),
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    };

    let lowered = lower_expression(&expr, &VarLayout::default(), &dae_model.symbols.functions)
        .expect("for-loop range size of scalar literal should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);

    assert_eq!(read_reg(&regs, lowered.result), 1.0);
}

#[test]
fn scalar_function_input_projects_child_array_field_from_last_target_index() {
    let mut dae_model = dae::Dae::new();
    for name in [
        "zone[2].TAirIn[1].y",
        "zone[2].ports[1].p",
        "zone[2].ports[2].p",
    ] {
        dae_model
            .variables
            .algebraics
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
    dae_model.symbols.functions.insert(
        rumoca_core::VarName::new("Pkg.identity"),
        rumoca_core::Function {
            name: rumoca_core::VarName::new("Pkg.identity"),
            def_id: None,
            inputs: vec![function_param("p")],
            outputs: vec![function_param("y")],
            locals: vec![],
            body: vec![rumoca_core::Statement::Assignment {
                comp: component_ref("y"),
                value: var("p"),
                span: rumoca_core::Span::DUMMY,
            }],
            is_constructor: false,
            pure: true,
            external: None,
            derivatives: vec![],
            span: rumoca_core::Span::DUMMY,
        },
    );

    let port_pressure = rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::FieldAccess {
            base: Box::new(rumoca_core::Expression::Index {
                base: Box::new(var("zone")),
                subscripts: vec![rumoca_core::Subscript::generated_index(
                    2,
                    rumoca_core::Span::DUMMY,
                )],
                span: rumoca_core::Span::DUMMY,
            }),
            field: "ports".to_string(),
            span: rumoca_core::Span::DUMMY,
        }),
        field: "p".to_string(),
        span: rumoca_core::Span::DUMMY,
    };
    dae_model.continuous.equations.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("zone[2].TAirIn[1].y")),
        rhs: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("Pkg.identity"),
            args: vec![named_arg("p", port_pressure)],
            is_constructor: false,
            span: rumoca_core::Span::DUMMY,
        },
        origin: "binding equation for zone[2].TAirIn[1].y".to_string(),
        span: rumoca_core::Span::DUMMY,
        scalar_count: 1,
    });

    let layout = build_var_layout(&dae_model);
    let rows = lower_residual(&dae_model, &layout)
        .expect("scalar function input should project child array field access");
    let mut y = vec![0.0; layout.y_scalars()];
    set_y_value(&layout, &mut y, "zone[2].TAirIn[1].y", 500000.0);
    set_y_value(&layout, &mut y, "zone[2].ports[1].p", 101001.0);
    set_y_value(&layout, &mut y, "zone[2].ports[2].p", 202002.0);

    let (_, row0) = eval_linear_ops(&rows[0], &y, &[], 0.0);

    assert_eq!(rows.len(), 1);
    assert_eq!(row0, Some(398999.0));
}

#[test]
fn scalar_residual_binary_projects_child_array_field_from_last_lhs_index() {
    let mut dae_model = dae::Dae::new();
    for name in [
        "zone[2].TAirIn[1].y",
        "zone[2].ports[1].p",
        "zone[2].ports[2].p",
    ] {
        dae_model
            .variables
            .algebraics
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
    dae_model.symbols.functions.insert(
        rumoca_core::VarName::new("Pkg.identity"),
        rumoca_core::Function {
            name: rumoca_core::VarName::new("Pkg.identity"),
            def_id: None,
            inputs: vec![function_param("p")],
            outputs: vec![function_param("y")],
            locals: vec![],
            body: vec![rumoca_core::Statement::Assignment {
                comp: component_ref("y"),
                value: var("p"),
                span: rumoca_core::Span::DUMMY,
            }],
            is_constructor: false,
            pure: true,
            external: None,
            derivatives: vec![],
            span: rumoca_core::Span::DUMMY,
        },
    );

    let port_pressure = rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::FieldAccess {
            base: Box::new(rumoca_core::Expression::Index {
                base: Box::new(var("zone")),
                subscripts: vec![rumoca_core::Subscript::generated_index(
                    2,
                    rumoca_core::Span::DUMMY,
                )],
                span: rumoca_core::Span::DUMMY,
            }),
            field: "ports".to_string(),
            span: rumoca_core::Span::DUMMY,
        }),
        field: "p".to_string(),
        span: rumoca_core::Span::DUMMY,
    };
    let rhs_call = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("Pkg.identity"),
        args: vec![named_arg("p", port_pressure)],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    };
    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(var("zone[2].TAirIn[1].y")),
            rhs: Box::new(rhs_call),
            span: rumoca_core::Span::DUMMY,
        },
        origin: "binding equation for zone[2].TAirIn[1].y".to_string(),
        span: rumoca_core::Span::DUMMY,
        scalar_count: 1,
    });

    let layout = build_var_layout(&dae_model);
    let rows = lower_residual(&dae_model, &layout)
        .expect("residual binary lhs should provide projection target");
    let mut y = vec![0.0; layout.y_scalars()];
    set_y_value(&layout, &mut y, "zone[2].TAirIn[1].y", 500000.0);
    set_y_value(&layout, &mut y, "zone[2].ports[1].p", 101001.0);
    set_y_value(&layout, &mut y, "zone[2].ports[2].p", 202002.0);

    let (_, row0) = eval_linear_ops(&rows[0], &y, &[], 0.0);

    assert_eq!(rows.len(), 1);
    assert_eq!(row0, Some(398999.0));
}

#[test]
fn scalar_function_record_input_binds_components_from_spanned_missing_actual() {
    let mut dae_model = dae::Dae::new();
    for name in ["y", "curve.r_V[1]"] {
        dae_model
            .variables
            .algebraics
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
    dae_model.symbols.functions.insert(
        rumoca_core::VarName::new("Pkg.curveFirst"),
        rumoca_core::Function {
            name: rumoca_core::VarName::new("Pkg.curveFirst"),
            def_id: None,
            inputs: vec![rumoca_core::FunctionParam {
                type_name: "Pkg.Curve".to_string(),
                type_class: Some(rumoca_core::ClassType::Record),
                ..function_param("per")
            }],
            outputs: vec![function_param("out")],
            locals: vec![],
            body: vec![rumoca_core::Statement::Assignment {
                comp: component_ref("out"),
                value: rumoca_core::Expression::Index {
                    base: Box::new(rumoca_core::Expression::FieldAccess {
                        base: Box::new(var("per")),
                        field: "r_V".to_string(),
                        span: rumoca_core::Span::DUMMY,
                    }),
                    subscripts: vec![rumoca_core::Subscript::generated_index(
                        1,
                        rumoca_core::Span::DUMMY,
                    )],
                    span: rumoca_core::Span::DUMMY,
                },
                span: rumoca_core::Span::DUMMY,
            }],
            is_constructor: false,
            pure: true,
            external: None,
            derivatives: vec![],
            span: rumoca_core::Span::DUMMY,
        },
    );

    let spanned_curve = rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new("curve"),
        subscripts: vec![],
        span: rumoca_core::Span::from_offsets(rumoca_core::SourceId(1), 10, 15),
    };
    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(var("y")),
            rhs: Box::new(rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.curveFirst"),
                args: vec![named_arg("per", spanned_curve)],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        origin: "binding equation for y".to_string(),
        span: rumoca_core::Span::DUMMY,
        scalar_count: 1,
    });

    let layout = build_var_layout(&dae_model);
    let rows = lower_residual(&dae_model, &layout)
        .expect("record actual components should bind through spanned missing actual");
    let mut y = vec![0.0; layout.y_scalars()];
    set_y_value(&layout, &mut y, "y", 10.0);
    set_y_value(&layout, &mut y, "curve.r_V[1]", 3.0);

    let (_, row0) = eval_linear_ops(&rows[0], &y, &[], 0.0);

    assert_eq!(rows.len(), 1);
    assert_eq!(row0, Some(7.0));
}

#[test]
fn scalar_function_record_input_size_unrolls_for_loop_from_component_shape() {
    let mut dae_model = dae::Dae::new();
    for name in ["y", "curve.r_V[1]", "curve.r_V[2]"] {
        dae_model
            .variables
            .algebraics
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }

    let mut n = function_param("n");
    n.default = Some(rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Size,
        args: vec![
            rumoca_core::Expression::FieldAccess {
                base: Box::new(var("per")),
                field: "r_V".to_string(),
                span: rumoca_core::Span::DUMMY,
            },
            int_lit(1),
        ],
        span: rumoca_core::Span::DUMMY,
    });
    dae_model.symbols.functions.insert(
        rumoca_core::VarName::new("Pkg.curveSum"),
        rumoca_core::Function {
            name: rumoca_core::VarName::new("Pkg.curveSum"),
            def_id: None,
            inputs: vec![rumoca_core::FunctionParam {
                type_name: "Pkg.Curve".to_string(),
                type_class: Some(rumoca_core::ClassType::Record),
                ..function_param("per")
            }],
            outputs: vec![function_param("out")],
            locals: vec![n],
            body: vec![
                rumoca_core::Statement::Assignment {
                    comp: component_ref("out"),
                    value: real_lit(0.0),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Statement::For {
                    indices: vec![rumoca_core::ForIndex {
                        ident: "j".to_string(),
                        range: rumoca_core::Expression::Range {
                            start: Box::new(int_lit(1)),
                            step: None,
                            end: Box::new(var("n")),
                            span: rumoca_core::Span::DUMMY,
                        },
                    }],
                    equations: vec![rumoca_core::Statement::Assignment {
                        comp: component_ref("out"),
                        value: add(
                            var("out"),
                            rumoca_core::Expression::Index {
                                base: Box::new(rumoca_core::Expression::FieldAccess {
                                    base: Box::new(var("per")),
                                    field: "r_V".to_string(),
                                    span: rumoca_core::Span::DUMMY,
                                }),
                                subscripts: vec![rumoca_core::Subscript::generated_expr(Box::new(
                                    var("j"),
                                ))],
                                span: rumoca_core::Span::DUMMY,
                            },
                        ),
                        span: rumoca_core::Span::DUMMY,
                    }],
                    span: rumoca_core::Span::DUMMY,
                },
            ],
            is_constructor: false,
            pure: true,
            external: None,
            derivatives: vec![],
            span: rumoca_core::Span::DUMMY,
        },
    );

    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(var("y")),
            rhs: Box::new(rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.curveSum"),
                args: vec![named_arg("per", var("curve"))],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        origin: "binding equation for y".to_string(),
        span: rumoca_core::Span::DUMMY,
        scalar_count: 1,
    });

    let layout = build_var_layout(&dae_model);
    let rows = lower_residual(&dae_model, &layout)
        .expect("record component shape should drive size-based for-loop unrolling");
    let mut y = vec![0.0; layout.y_scalars()];
    set_y_value(&layout, &mut y, "y", 10.0);
    set_y_value(&layout, &mut y, "curve.r_V[1]", 3.0);
    set_y_value(&layout, &mut y, "curve.r_V[2]", 4.0);

    let (_, row0) = eval_linear_ops(&rows[0], &y, &[], 0.0);

    assert_eq!(rows.len(), 1);
    assert_eq!(row0, Some(3.0));
}

#[test]
fn flattened_record_input_alias_shape_unrolls_dotted_size_for_loop() {
    let mut dae_model = dae::Dae::new();
    for name in ["y", "holder.curve.r_V[1]", "holder.curve.r_V[2]"] {
        dae_model
            .variables
            .algebraics
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }

    let mut n = function_param("n");
    n.default = Some(rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Size,
        args: vec![var("per.r_V"), int_lit(1)],
        span: rumoca_core::Span::DUMMY,
    });
    dae_model.symbols.functions.insert(
        rumoca_core::VarName::new("Pkg.flatCurveSum"),
        rumoca_core::Function {
            name: rumoca_core::VarName::new("Pkg.flatCurveSum"),
            def_id: None,
            inputs: vec![function_param("per_r_V")],
            outputs: vec![function_param("out")],
            locals: vec![n],
            body: vec![
                rumoca_core::Statement::Assignment {
                    comp: component_ref("out"),
                    value: real_lit(0.0),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Statement::For {
                    indices: vec![rumoca_core::ForIndex {
                        ident: "j".to_string(),
                        range: rumoca_core::Expression::Range {
                            start: Box::new(int_lit(1)),
                            step: None,
                            end: Box::new(var("n")),
                            span: rumoca_core::Span::DUMMY,
                        },
                    }],
                    equations: vec![rumoca_core::Statement::Assignment {
                        comp: component_ref("out"),
                        value: add(
                            var("out"),
                            rumoca_core::Expression::Index {
                                base: Box::new(var("per_r_V")),
                                subscripts: vec![rumoca_core::Subscript::generated_expr(Box::new(
                                    var("j"),
                                ))],
                                span: rumoca_core::Span::DUMMY,
                            },
                        ),
                        span: rumoca_core::Span::DUMMY,
                    }],
                    span: rumoca_core::Span::DUMMY,
                },
            ],
            is_constructor: false,
            pure: true,
            external: None,
            derivatives: vec![],
            span: rumoca_core::Span::DUMMY,
        },
    );

    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(var("y")),
            rhs: Box::new(rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.flatCurveSum"),
                args: vec![rumoca_core::Expression::FieldAccess {
                    base: Box::new(var("holder")),
                    field: "curve".to_string(),
                    span: rumoca_core::Span::DUMMY,
                }],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        origin: "binding equation for y".to_string(),
        span: rumoca_core::Span::DUMMY,
        scalar_count: 1,
    });

    let layout = build_var_layout(&dae_model);
    let rows = lower_residual(&dae_model, &layout)
        .expect("flattened record input should alias dotted size shape");
    let mut y = vec![0.0; layout.y_scalars()];
    set_y_value(&layout, &mut y, "y", 10.0);
    set_y_value(&layout, &mut y, "holder.curve.r_V[1]", 3.0);
    set_y_value(&layout, &mut y, "holder.curve.r_V[2]", 4.0);

    let (_, row0) = eval_linear_ops(&rows[0], &y, &[], 0.0);

    assert_eq!(rows.len(), 1);
    assert_eq!(row0, Some(3.0));
}

#[test]
fn flattened_record_singleton_input_preserves_dotted_size() {
    let mut dae_model = dae::Dae::new();
    for name in ["y", "holder.curve.r_V[1]"] {
        dae_model
            .variables
            .algebraics
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }

    let mut n = function_param("n");
    n.default = Some(rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Size,
        args: vec![var("per.r_V"), int_lit(1)],
        span: rumoca_core::Span::DUMMY,
    });
    dae_model.symbols.functions.insert(
        rumoca_core::VarName::new("Pkg.flatSingletonCurveSize"),
        rumoca_core::Function {
            name: rumoca_core::VarName::new("Pkg.flatSingletonCurveSize"),
            def_id: None,
            inputs: vec![function_param("per_r_V")],
            outputs: vec![function_param("out")],
            locals: vec![n],
            body: vec![rumoca_core::Statement::Assignment {
                comp: component_ref("out"),
                value: var("n"),
                span: rumoca_core::Span::DUMMY,
            }],
            is_constructor: false,
            pure: true,
            external: None,
            derivatives: vec![],
            span: rumoca_core::Span::DUMMY,
        },
    );

    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: sub(
            var("y"),
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.flatSingletonCurveSize"),
                args: vec![rumoca_core::Expression::FieldAccess {
                    base: Box::new(var("holder")),
                    field: "curve".to_string(),
                    span: rumoca_core::Span::DUMMY,
                }],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            },
        ),
        origin: "binding equation for y".to_string(),
        span: rumoca_core::Span::DUMMY,
        scalar_count: 1,
    });

    let layout = build_var_layout(&dae_model);
    let rows = lower_residual(&dae_model, &layout)
        .expect("singleton flattened record input should preserve dotted size metadata");
    let mut y = vec![0.0; layout.y_scalars()];
    set_y_value(&layout, &mut y, "y", 5.0);
    set_y_value(&layout, &mut y, "holder.curve.r_V[1]", 3.0);

    let (_, row0) = eval_linear_ops(&rows[0], &y, &[], 0.0);

    assert_eq!(rows.len(), 1);
    assert_eq!(row0, Some(4.0));
}

#[test]
fn flattened_record_input_array_field_preserves_indexed_root_actual_values() {
    let mut dae_model = dae::Dae::new();
    for name in [
        "y",
        "ct[1].yorkCalc.fanRelPow.r_V[1]",
        "ct[1].yorkCalc.fanRelPow.r_V[2]",
    ] {
        dae_model
            .variables
            .algebraics
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }

    let mut n = function_param("n");
    n.default = Some(rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Size,
        args: vec![var("per.r_V"), int_lit(1)],
        span: rumoca_core::Span::DUMMY,
    });
    dae_model.symbols.functions.insert(
        rumoca_core::VarName::new("Pkg.flatIndexedRootCurveSum"),
        rumoca_core::Function {
            name: rumoca_core::VarName::new("Pkg.flatIndexedRootCurveSum"),
            def_id: None,
            inputs: vec![function_param("per_r_V")],
            outputs: vec![function_param("out")],
            locals: vec![n],
            body: vec![
                rumoca_core::Statement::Assignment {
                    comp: component_ref("out"),
                    value: real_lit(0.0),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Statement::For {
                    indices: vec![rumoca_core::ForIndex {
                        ident: "j".to_string(),
                        range: rumoca_core::Expression::Range {
                            start: Box::new(int_lit(1)),
                            step: None,
                            end: Box::new(var("n")),
                            span: rumoca_core::Span::DUMMY,
                        },
                    }],
                    equations: vec![rumoca_core::Statement::Assignment {
                        comp: component_ref("out"),
                        value: add(
                            var("out"),
                            rumoca_core::Expression::Index {
                                base: Box::new(var("per_r_V")),
                                subscripts: vec![rumoca_core::Subscript::generated_expr(Box::new(
                                    var("j"),
                                ))],
                                span: rumoca_core::Span::DUMMY,
                            },
                        ),
                        span: rumoca_core::Span::DUMMY,
                    }],
                    span: rumoca_core::Span::DUMMY,
                },
            ],
            is_constructor: false,
            pure: true,
            external: None,
            derivatives: vec![],
            span: rumoca_core::Span::DUMMY,
        },
    );

    let actual = rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::FieldAccess {
            base: Box::new(rumoca_core::Expression::Index {
                base: Box::new(var("ct")),
                subscripts: vec![rumoca_core::Subscript::generated_index(
                    1,
                    rumoca_core::Span::DUMMY,
                )],
                span: rumoca_core::Span::DUMMY,
            }),
            field: "yorkCalc".to_string(),
            span: rumoca_core::Span::DUMMY,
        }),
        field: "fanRelPow".to_string(),
        span: rumoca_core::Span::DUMMY,
    };
    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(var("y")),
            rhs: Box::new(rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.flatIndexedRootCurveSum"),
                args: vec![actual],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        origin: "binding equation for y".to_string(),
        span: rumoca_core::Span::DUMMY,
        scalar_count: 1,
    });

    let layout = build_var_layout(&dae_model);
    let rows = lower_residual(&dae_model, &layout)
        .expect("indexed record actual should bind all array field elements");
    let mut y = vec![0.0; layout.y_scalars()];
    set_y_value(&layout, &mut y, "y", 10.0);
    set_y_value(&layout, &mut y, "ct[1].yorkCalc.fanRelPow.r_V[1]", 3.0);
    set_y_value(&layout, &mut y, "ct[1].yorkCalc.fanRelPow.r_V[2]", 4.0);

    let (_, row0) = eval_linear_ops(&rows[0], &y, &[], 0.0);

    assert_eq!(rows.len(), 1);
    assert_eq!(row0, Some(3.0));
}

#[test]
fn flattened_record_input_keeps_actual_that_is_already_target_field() {
    let mut dae_model = dae::Dae::new();
    for name in [
        "y",
        "pCur3.V_flow[1]",
        "pCur3.V_flow[2]",
        "pCur3.dp[1]",
        "pCur3.dp[2]",
    ] {
        dae_model
            .variables
            .algebraics
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }

    dae_model.symbols.functions.insert(
        rumoca_core::VarName::new("Pkg.flatPressureLike"),
        rumoca_core::Function {
            name: rumoca_core::VarName::new("Pkg.flatPressureLike"),
            def_id: None,
            inputs: vec![
                function_param_with_dims("per_V_flow", &[0]),
                function_param_with_dims("per_dp", &[0]),
            ],
            outputs: vec![function_param("out")],
            locals: vec![],
            body: vec![rumoca_core::Statement::Assignment {
                comp: component_ref("out"),
                value: add(
                    rumoca_core::Expression::Index {
                        base: Box::new(var("per_V_flow")),
                        subscripts: vec![rumoca_core::Subscript::generated_index(
                            2,
                            rumoca_core::Span::DUMMY,
                        )],
                        span: rumoca_core::Span::DUMMY,
                    },
                    rumoca_core::Expression::Index {
                        base: Box::new(var("per_dp")),
                        subscripts: vec![rumoca_core::Subscript::generated_index(
                            1,
                            rumoca_core::Span::DUMMY,
                        )],
                        span: rumoca_core::Span::DUMMY,
                    },
                ),
                span: rumoca_core::Span::DUMMY,
            }],
            is_constructor: false,
            pure: true,
            external: None,
            derivatives: vec![],
            span: rumoca_core::Span::DUMMY,
        },
    );

    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: sub(
            var("y"),
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.flatPressureLike"),
                args: vec![var("pCur3.V_flow")],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            },
        ),
        origin: "binding equation for y".to_string(),
        span: rumoca_core::Span::DUMMY,
        scalar_count: 1,
    });

    let layout = build_var_layout(&dae_model);
    let rows = lower_residual(&dae_model, &layout)
        .expect("flattened record field actual should not be projected twice");
    let mut y = vec![0.0; layout.y_scalars()];
    set_y_value(&layout, &mut y, "y", 10.0);
    set_y_value(&layout, &mut y, "pCur3.V_flow[1]", 3.0);
    set_y_value(&layout, &mut y, "pCur3.V_flow[2]", 4.0);
    set_y_value(&layout, &mut y, "pCur3.dp[1]", 5.0);
    set_y_value(&layout, &mut y, "pCur3.dp[2]", 6.0);

    let (_, row0) = eval_linear_ops(&rows[0], &y, &[], 0.0);

    assert_eq!(rows.len(), 1);
    assert_eq!(row0, Some(1.0));
}

#[test]
fn function_output_shape_uses_flattened_record_field_actual_not_record_width() {
    let mut dae_model = dae::Dae::new();
    for name in [
        "preDer1[1]",
        "preDer1[2]",
        "pum[1].pCur1[1]",
        "pum[1].pCur1[2]",
        "pum[1].pCur1[3]",
        "pum[1].pCur1[4]",
        "pum[1].pCur1[5]",
        "pum[1].pCur1[6]",
        "pum[1].pCur1.V_flow[1]",
        "pum[1].pCur1.V_flow[2]",
        "pum[1].pCur1.dp[1]",
        "pum[1].pCur1.dp[2]",
    ] {
        dae_model
            .variables
            .algebraics
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }

    dae_model.symbols.functions.insert(
        rumoca_core::VarName::new("Pkg.splineLike"),
        rumoca_core::Function {
            name: rumoca_core::VarName::new("Pkg.splineLike"),
            def_id: None,
            inputs: vec![
                function_param_with_dims("x", &[0]),
                function_param_with_dims("y", &[0]),
            ],
            outputs: vec![rumoca_core::FunctionParam {
                def_id: None,
                name: "d".to_string(),
                span: rumoca_core::Span::DUMMY,
                type_name: "Real".to_string(),
                type_class: None,
                dims: vec![0],
                shape_expr: vec![rumoca_core::Subscript::generated_expr(Box::new(
                    rumoca_core::Expression::BuiltinCall {
                        function: rumoca_core::BuiltinFunction::Size,
                        args: vec![var("x"), int_lit(1)],
                        span: rumoca_core::Span::DUMMY,
                    },
                ))],
                default: None,
                description: None,
            }],
            locals: vec![
                rumoca_core::FunctionParam {
                    def_id: None,
                    name: "n".to_string(),
                    span: rumoca_core::Span::DUMMY,
                    type_name: "Integer".to_string(),
                    type_class: None,
                    dims: vec![],
                    shape_expr: vec![],
                    default: Some(rumoca_core::Expression::BuiltinCall {
                        function: rumoca_core::BuiltinFunction::Size,
                        args: vec![var("x"), int_lit(1)],
                        span: rumoca_core::Span::DUMMY,
                    }),
                    description: None,
                },
                rumoca_core::FunctionParam {
                    def_id: None,
                    name: "delta".to_string(),
                    span: rumoca_core::Span::DUMMY,
                    type_name: "Real".to_string(),
                    type_class: None,
                    dims: vec![0],
                    shape_expr: vec![rumoca_core::Subscript::generated_expr(Box::new(sub(
                        var("n"),
                        int_lit(1),
                    )))],
                    default: None,
                    description: None,
                },
            ],
            body: vec![rumoca_core::Statement::If {
                cond_blocks: vec![rumoca_core::StatementBlock {
                    cond: binary(rumoca_core::OpBinary::Eq, var("n"), int_lit(2)),
                    stmts: vec![
                        rumoca_core::Statement::Assignment {
                            comp: component_ref("delta[1]"),
                            value: binary(
                                rumoca_core::OpBinary::Div,
                                sub(
                                    rumoca_core::Expression::Index {
                                        base: Box::new(var("y")),
                                        subscripts: vec![rumoca_core::Subscript::generated_index(
                                            2,
                                            rumoca_core::Span::DUMMY,
                                        )],
                                        span: rumoca_core::Span::DUMMY,
                                    },
                                    rumoca_core::Expression::Index {
                                        base: Box::new(var("y")),
                                        subscripts: vec![rumoca_core::Subscript::generated_index(
                                            1,
                                            rumoca_core::Span::DUMMY,
                                        )],
                                        span: rumoca_core::Span::DUMMY,
                                    },
                                ),
                                sub(
                                    rumoca_core::Expression::Index {
                                        base: Box::new(var("x")),
                                        subscripts: vec![rumoca_core::Subscript::generated_index(
                                            2,
                                            rumoca_core::Span::DUMMY,
                                        )],
                                        span: rumoca_core::Span::DUMMY,
                                    },
                                    rumoca_core::Expression::Index {
                                        base: Box::new(var("x")),
                                        subscripts: vec![rumoca_core::Subscript::generated_index(
                                            1,
                                            rumoca_core::Span::DUMMY,
                                        )],
                                        span: rumoca_core::Span::DUMMY,
                                    },
                                ),
                            ),
                            span: rumoca_core::Span::DUMMY,
                        },
                        rumoca_core::Statement::Assignment {
                            comp: component_ref("d[1]"),
                            value: var("delta[1]"),
                            span: rumoca_core::Span::DUMMY,
                        },
                        rumoca_core::Statement::Assignment {
                            comp: component_ref("d[2]"),
                            value: var("d[1]"),
                            span: rumoca_core::Span::DUMMY,
                        },
                    ],
                }],
                else_block: Some(vec![
                    rumoca_core::Statement::Assignment {
                        comp: component_ref("d[1]"),
                        value: real_lit(0.0),
                        span: rumoca_core::Span::DUMMY,
                    },
                    rumoca_core::Statement::Assignment {
                        comp: component_ref("d[2]"),
                        value: real_lit(0.0),
                        span: rumoca_core::Span::DUMMY,
                    },
                ]),
                span: rumoca_core::Span::DUMMY,
            }],
            is_constructor: false,
            pure: true,
            external: None,
            derivatives: vec![],
            span: rumoca_core::Span::DUMMY,
        },
    );

    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: sub(
            var("preDer1"),
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.splineLike"),
                args: vec![
                    named_arg("x", var("pum[1].pCur1.V_flow")),
                    named_arg("y", var("pum[1].pCur1.dp")),
                ],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            },
        ),
        origin: "spline derivative binding".to_string(),
        span: rumoca_core::Span::DUMMY,
        scalar_count: 2,
    });

    let layout = build_var_layout(&dae_model);
    let rows = lower_initial_residual(&dae_model, &layout)
        .expect("function output shape should follow field actual width");
    let mut y = vec![0.0; layout.y_scalars()];
    set_y_value(&layout, &mut y, "preDer1[1]", 10.0);
    set_y_value(&layout, &mut y, "preDer1[2]", 20.0);
    set_y_value(&layout, &mut y, "pum[1].pCur1.V_flow[1]", 1.0);
    set_y_value(&layout, &mut y, "pum[1].pCur1.V_flow[2]", 2.0);
    set_y_value(&layout, &mut y, "pum[1].pCur1.dp[1]", 4.0);
    set_y_value(&layout, &mut y, "pum[1].pCur1.dp[2]", 7.0);

    let (_, row0) = eval_linear_ops(&rows[0], &y, &[], 0.0);
    let (_, row1) = eval_linear_ops(&rows[1], &y, &[], 0.0);

    assert_eq!(rows.len(), 2);
    assert_eq!(row0, Some(7.0));
    assert_eq!(row1, Some(17.0));
}

#[test]
fn direct_assignment_cache_scalarizes_vector_targets_without_base_full_rhs() {
    let mut dae_model = dae::Dae::new();
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("curve"),
        dae::Variable {
            name: rumoca_core::VarName::new("curve"),
            dims: vec![2],
            ..Default::default()
        },
    );
    for name in ["curve[1]", "curve[2]"] {
        dae_model
            .variables
            .algebraics
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: sub(
            var("curve"),
            rumoca_core::Expression::Array {
                elements: vec![real_lit(1.0), real_lit(2.0)],
                is_matrix: false,
                span: rumoca_core::Span::DUMMY,
            },
        ),
        origin: "direct vector binding".to_string(),
        span: rumoca_core::Span::DUMMY,
        scalar_count: 2,
    });

    let assignments = super::derivative_rhs::collect_direct_assignments(&dae_model, &[false]);

    assert!(!assignments.contains_key("curve"));
    assert!(assignments.contains_key("curve[1]"));
    assert!(assignments.contains_key("curve[2]"));
}

#[test]
fn function_dynamic_output_shape_limits_extra_indexed_branch_assignments() {
    let mut dae_model = dae::Dae::new();
    for name in ["y[1]", "y[2]", "x[1]", "x[2]"] {
        dae_model
            .variables
            .algebraics
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }

    let mut function = rumoca_core::Function::new("Pkg.dynamicOut", rumoca_core::Span::DUMMY);
    function.inputs.push(function_param_with_dims("x", &[0]));
    function.outputs.push(rumoca_core::FunctionParam {
        def_id: None,
        name: "d".to_string(),
        span: rumoca_core::Span::DUMMY,
        type_name: "Real".to_string(),
        type_class: None,
        dims: vec![0],
        shape_expr: vec![rumoca_core::Subscript::generated_expr(Box::new(
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Size,
                args: vec![var("x"), int_lit(1)],
                span: rumoca_core::Span::DUMMY,
            },
        ))],
        default: None,
        description: None,
    });
    function.body = (1..=6)
        .map(|idx| rumoca_core::Statement::Assignment {
            comp: component_ref_index("d", idx),
            value: real_lit(idx as f64),
            span: rumoca_core::Span::DUMMY,
        })
        .collect();
    function.body.push(rumoca_core::Statement::If {
        cond_blocks: vec![rumoca_core::StatementBlock {
            cond: binary(
                rumoca_core::OpBinary::And,
                binary(rumoca_core::OpBinary::Gt, int_lit(2), int_lit(3)),
                var("runtimeFlag"),
            ),
            stmts: vec![rumoca_core::Statement::Assignment {
                comp: component_ref_index("d", 1),
                value: indexed_var("delta", 1),
                span: rumoca_core::Span::DUMMY,
            }],
        }],
        else_block: None,
        span: rumoca_core::Span::DUMMY,
    });
    dae_model
        .symbols
        .functions
        .insert(function.name.clone(), function);

    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: sub(
            var("y"),
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.dynamicOut"),
                args: vec![named_arg("x", var("x"))],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            },
        ),
        origin: "dynamic output shape residual".to_string(),
        span: rumoca_core::Span::DUMMY,
        scalar_count: 2,
    });

    let layout = build_var_layout(&dae_model);
    let rows = lower_residual(&dae_model, &layout)
        .expect("resolved dynamic output shape should limit indexed branch assignments");

    assert_eq!(rows.len(), 2);
}

#[test]
fn initial_function_input_shape_uses_parameter_declared_dims_not_longer_start() {
    let mut dae_model = dae::Dae::new();
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("curve"),
        dae::Variable {
            name: rumoca_core::VarName::new("curve"),
            dims: vec![2],
            start: Some(rumoca_core::Expression::If {
                branches: vec![(
                    bool_lit(true),
                    rumoca_core::Expression::ArrayComprehension {
                        expr: Box::new(rumoca_core::Expression::VarRef {
                            name: rumoca_core::VarName::new("source").into(),
                            subscripts: vec![rumoca_core::Subscript::generated_expr(Box::new(
                                var("i"),
                            ))],
                            span: rumoca_core::Span::DUMMY,
                        }),
                        indices: vec![rumoca_core::ComprehensionIndex {
                            name: "i".to_string(),
                            range: rumoca_core::Expression::Range {
                                start: Box::new(int_lit(1)),
                                step: None,
                                end: Box::new(var("nOri")),
                                span: rumoca_core::Span::DUMMY,
                            },
                        }],
                        filter: None,
                        span: rumoca_core::Span::DUMMY,
                    },
                )],
                else_branch: Box::new(rumoca_core::Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Zeros,
                    args: vec![var("nOri")],
                    span: rumoca_core::Span::DUMMY,
                }),
                span: rumoca_core::Span::DUMMY,
            }),
            fixed: Some(true),
            is_tunable: true,
            ..Default::default()
        },
    );
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("source"),
        dae::Variable {
            name: rumoca_core::VarName::new("source"),
            dims: vec![6],
            fixed: Some(true),
            is_tunable: true,
            ..Default::default()
        },
    );
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("nOri"),
        dae::Variable {
            name: rumoca_core::VarName::new("nOri"),
            start: Some(int_lit(6)),
            fixed: Some(true),
            is_tunable: false,
            ..Default::default()
        },
    );
    for name in ["haveVMax", "haveDPMax"] {
        dae_model.variables.algebraics.insert(
            rumoca_core::VarName::new(name),
            dae::Variable {
                name: rumoca_core::VarName::new(name),
                ..Default::default()
            },
        );
    }
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("preDer1"),
        dae::Variable {
            name: rumoca_core::VarName::new("preDer1"),
            dims: vec![2],
            ..Default::default()
        },
    );
    let mut function = rumoca_core::Function::new("Pkg.dynamicOut", rumoca_core::Span::DUMMY);
    function.inputs.push(function_param_with_dims("x", &[0]));
    function.outputs.push(rumoca_core::FunctionParam {
        def_id: None,
        name: "d".to_string(),
        span: rumoca_core::Span::DUMMY,
        type_name: "Real".to_string(),
        type_class: None,
        dims: vec![0],
        shape_expr: vec![rumoca_core::Subscript::generated_expr(Box::new(
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Size,
                args: vec![var("x"), int_lit(1)],
                span: rumoca_core::Span::DUMMY,
            },
        ))],
        default: None,
        description: None,
    });
    function.body = (1..=6)
        .map(|idx| rumoca_core::Statement::Assignment {
            comp: component_ref_index("d", idx),
            value: real_lit(idx as f64),
            span: rumoca_core::Span::DUMMY,
        })
        .collect();
    function.body.push(rumoca_core::Statement::If {
        cond_blocks: vec![rumoca_core::StatementBlock {
            cond: binary(
                rumoca_core::OpBinary::And,
                binary(rumoca_core::OpBinary::Gt, int_lit(2), int_lit(3)),
                var("runtimeFlag"),
            ),
            stmts: vec![rumoca_core::Statement::Assignment {
                comp: component_ref_index("d", 1),
                value: indexed_var("delta", 1),
                span: rumoca_core::Span::DUMMY,
            }],
        }],
        else_block: None,
        span: rumoca_core::Span::DUMMY,
    });
    dae_model
        .symbols
        .functions
        .insert(function.name.clone(), function);

    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: rumoca_core::Expression::If {
            branches: vec![(
                binary(
                    rumoca_core::OpBinary::Or,
                    binary(
                        rumoca_core::OpBinary::And,
                        var("haveVMax"),
                        var("haveDPMax"),
                    ),
                    binary(rumoca_core::OpBinary::Eq, var("nOri"), int_lit(2)),
                ),
                sub(
                    var("preDer1"),
                    rumoca_core::Expression::FunctionCall {
                        name: rumoca_core::Reference::new("Pkg.dynamicOut"),
                        args: vec![named_arg("x", var("curve"))],
                        is_constructor: false,
                        span: rumoca_core::Span::DUMMY,
                    },
                ),
            )],
            else_branch: Box::new(rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(var("preDer1")),
                rhs: Box::new(rumoca_core::Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Zeros,
                    args: vec![var("nOri")],
                    span: rumoca_core::Span::DUMMY,
                }),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        origin: "parameter start should not widen function actual".to_string(),
        span: rumoca_core::Span::DUMMY,
        scalar_count: 2,
    });

    let layout = build_var_layout(&dae_model);
    let rows = lower_initial_residual(&dae_model, &layout)
        .expect("parameter declared dims should bound function actual shape");

    assert_eq!(rows.len(), 2);
}

#[test]
fn scalar_function_input_name_shadows_global_vector_layout_shape() {
    let mut dae_model = dae::Dae::new();
    for name in ["y", "u", "x[1]", "x[2]"] {
        dae_model
            .variables
            .algebraics
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }

    dae_model.symbols.functions.insert(
        rumoca_core::VarName::new("Pkg.scalarIf"),
        rumoca_core::Function {
            name: rumoca_core::VarName::new("Pkg.scalarIf"),
            def_id: None,
            inputs: vec![function_param("x")],
            outputs: vec![function_param("out")],
            locals: vec![],
            body: vec![rumoca_core::Statement::Assignment {
                comp: component_ref("out"),
                value: rumoca_core::Expression::If {
                    branches: vec![(
                        binary(rumoca_core::OpBinary::Gt, var("x"), real_lit(0.0)),
                        var("x"),
                    )],
                    else_branch: Box::new(real_lit(0.0)),
                    span: rumoca_core::Span::DUMMY,
                },
                span: rumoca_core::Span::DUMMY,
            }],
            is_constructor: false,
            pure: true,
            external: None,
            derivatives: vec![],
            span: rumoca_core::Span::DUMMY,
        },
    );

    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: sub(
            var("y"),
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.scalarIf"),
                args: vec![named_arg("x", var("u"))],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            },
        ),
        origin: "binding equation for y".to_string(),
        span: rumoca_core::Span::DUMMY,
        scalar_count: 1,
    });

    let layout = build_var_layout(&dae_model);
    let rows = lower_residual(&dae_model, &layout)
        .expect("local scalar input x should shadow global vector layout x");
    let mut y = vec![0.0; layout.y_scalars()];
    set_y_value(&layout, &mut y, "y", 10.0);
    set_y_value(&layout, &mut y, "u", 3.0);

    let (_, row0) = eval_linear_ops(&rows[0], &y, &[], 0.0);

    assert_eq!(rows.len(), 1);
    assert_eq!(row0, Some(7.0));
}

#[test]
fn function_output_shape_expr_uses_actual_input_size() {
    let mut dae_model = dae::Dae::new();
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("u"),
        dae::Variable {
            name: rumoca_core::VarName::new("u"),
            dims: vec![3],
            ..Default::default()
        },
    );

    let mut function = rumoca_core::Function::new("Pkg.shapeFromInput", rumoca_core::Span::DUMMY);
    function.inputs.push(rumoca_core::FunctionParam {
        def_id: None,
        name: "x".to_string(),
        span: rumoca_core::Span::DUMMY,
        type_name: "Real".to_string(),
        type_class: None,
        dims: vec![0],
        shape_expr: vec![rumoca_core::Subscript::Colon {
            span: rumoca_core::Span::DUMMY,
        }],
        default: None,
        description: None,
    });
    function.outputs.push(rumoca_core::FunctionParam {
        def_id: None,
        name: "d".to_string(),
        span: rumoca_core::Span::DUMMY,
        type_name: "Real".to_string(),
        type_class: None,
        dims: vec![0],
        shape_expr: vec![rumoca_core::Subscript::generated_expr(Box::new(
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Size,
                args: vec![var("x"), int_lit(1)],
                span: rumoca_core::Span::DUMMY,
            },
        ))],
        default: None,
        description: None,
    });
    dae_model
        .symbols
        .functions
        .insert(rumoca_core::VarName::new("Pkg.shapeFromInput"), function);

    let layout = build_var_layout(&dae_model);
    let builder = LowerBuilder::new(&layout, &dae_model.symbols.functions);
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("Pkg.shapeFromInput"),
        args: vec![var("u")],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(builder.infer_expr_dims(&expr, &Scope::new()), vec![3]);
}

#[test]
fn function_output_shape_expr_uses_integer_input_actual() {
    let mut dae_model = dae::Dae::new();
    let mut function = rumoca_core::Function::new(
        "Buildings.ThermalZones.EnergyPlus_9_6_0.BaseClasses.getParameters",
        rumoca_core::Span::DUMMY,
    );
    function.inputs.push(function_param("adapter"));
    function.inputs.push(function_param("nParOut"));
    function.inputs.push(function_param("isSynchronized"));
    function.outputs.push(rumoca_core::FunctionParam {
        def_id: None,
        name: "parOut".to_string(),
        span: rumoca_core::Span::DUMMY,
        type_name: "Real".to_string(),
        type_class: None,
        dims: vec![0],
        shape_expr: vec![rumoca_core::Subscript::generated_expr(Box::new(var(
            "nParOut",
        )))],
        default: None,
        description: None,
    });
    dae_model
        .symbols
        .functions
        .insert(function.name.clone(), function);

    let layout = build_var_layout(&dae_model);
    let builder = LowerBuilder::new(&layout, &dae_model.symbols.functions);
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new(
            "Buildings.ThermalZones.EnergyPlus_9_6_0.BaseClasses.getParameters",
        ),
        args: vec![
            var("adapter"),
            named_arg("nParOut", int_lit(3)),
            named_arg("isSynchronized", var("nObj")),
        ],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(builder.infer_expr_dims(&expr, &Scope::new()), vec![3]);
}

#[test]
fn function_output_shape_expr_uses_integer_input_start_actual() {
    let mut function = rumoca_core::Function::new("Pkg.getParameters", rumoca_core::Span::DUMMY);
    function.inputs.push(function_param("nParOut"));
    function.outputs.push(rumoca_core::FunctionParam {
        def_id: None,
        name: "parOut".to_string(),
        span: rumoca_core::Span::DUMMY,
        type_name: "Real".to_string(),
        type_class: None,
        dims: vec![0],
        shape_expr: vec![rumoca_core::Subscript::generated_expr(Box::new(var(
            "nParOut",
        )))],
        default: None,
        description: None,
    });
    let functions = IndexMap::from([(function.name.clone(), function)]);
    let starts = IndexMap::from([("obj.nParOut".to_string(), int_lit(3))]);
    let layout = VarLayout::from_parts_with_shapes_and_indexed_bindings(
        IndexMap::new(),
        IndexMap::new(),
        IndexMap::new(),
        0,
        0,
    );
    let builder = LowerBuilder::new_with_metadata(
        &layout,
        &functions,
        super::LowerBuilderMetadata {
            variable_starts: Some(&starts),
            ..Default::default()
        },
    );
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("Pkg.getParameters"),
        args: vec![named_arg("nParOut", var("obj.nParOut"))],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(builder.infer_expr_dims(&expr, &Scope::new()), vec![3]);
}

#[test]
fn energyplus_external_initialize_lowers_to_external_call_op() {
    let mut dae_model = dae::Dae::new();
    for name in ["nObj", "isSynchronized"] {
        dae_model
            .variables
            .algebraics
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
    let mut function = rumoca_core::Function::new(
        "Buildings.ThermalZones.EnergyPlus_9_6_0.BaseClasses.initialize",
        rumoca_core::Span::DUMMY,
    );
    function.inputs.push(function_param("adapter"));
    function.inputs.push(function_param("isSynchronized"));
    function.outputs.push(function_param("nObj"));
    function.external = Some(rumoca_core::ExternalFunction {
        language: "C".to_string(),
        function_name: Some("initialize_Modelica_EnergyPlus_9_6_0".to_string()),
        arg_names: vec![
            "adapter".to_string(),
            "isSynchronized".to_string(),
            "nObj".to_string(),
        ],
        libraries: vec![
            "ModelicaBuildingsEnergyPlus_9_6_0".to_string(),
            "fmilib_shared".to_string(),
        ],
        ..Default::default()
    });
    dae_model
        .symbols
        .functions
        .insert(function.name.clone(), function);

    dae_model.continuous.equations.push(dae::Equation::residual(
        sub(
            var("nObj"),
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new(
                    "Buildings.ThermalZones.EnergyPlus_9_6_0.BaseClasses.initialize",
                ),
                args: vec![
                    rumoca_core::Expression::FunctionCall {
                        name: rumoca_core::Reference::new(
                            "Buildings.ThermalZones.EnergyPlus_9_6_0.BaseClasses.SpawnExternalObject",
                        ),
                        args: vec![var("adapter")],
                        is_constructor: true,
                        span: rumoca_core::Span::DUMMY,
                    },
                    named_arg("isSynchronized", var("isSynchronized")),
                ],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            },
        ),
        rumoca_core::Span::DUMMY,
        "EnergyPlus initialize residual",
    ));

    let layout = build_var_layout(&dae_model);
    let rows = lower_residual(&dae_model, &layout)
        .expect("EnergyPlus external initialize should remain explicit in Solve IR");

    assert!(rows[0].iter().any(|op| matches!(
        op,
        LinearOp::ExternalCall {
            function: rumoca_ir_solve::ExternalFunctionKind::BuildingsEnergyPlusInitialize,
            ..
        }
    )));
}

#[test]
fn energyplus_spawn_external_object_skips_string_metadata_args() {
    let mut function = rumoca_core::Function::new(
        "Buildings.ThermalZones.EnergyPlus_9_6_0.BaseClasses.SpawnExternalObject",
        rumoca_core::Span::DUMMY,
    );
    function.external = Some(rumoca_core::ExternalFunction {
        language: "C".to_string(),
        function_name: Some("allocate_Modelica_EnergyPlus_9_6_0".to_string()),
        arg_names: vec![
            "objectType".to_string(),
            "startTime".to_string(),
            "modelicaNameBuilding".to_string(),
        ],
        libraries: vec![
            "ModelicaBuildingsEnergyPlus_9_6_0".to_string(),
            "fmilib_shared".to_string(),
        ],
        ..Default::default()
    });
    let functions = IndexMap::from([(function.name.clone(), function)]);
    let layout = build_var_layout(&dae::Dae::new());
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new(
            "Buildings.ThermalZones.EnergyPlus_9_6_0.BaseClasses.SpawnExternalObject",
        ),
        args: vec![
            named_arg("objectType", int_lit(1)),
            named_arg("startTime", real_lit(0.0)),
            named_arg("modelicaNameBuilding", var("missingString")),
            named_arg("modelicaInstanceName", var("missingInstanceString")),
            named_arg("spawnExe", var("missingSpawnString")),
            named_arg("idfVersion", var("missingVersionString")),
            named_arg("idfName", var("missingIdfString")),
            named_arg("epwName", var("missingEpwString")),
            named_arg("relativeSurfaceTolerance", real_lit(0.01)),
            named_arg("epName", var("missingEpNameString")),
            named_arg("usePrecompiledFMU", bool_lit(false)),
            named_arg("fmuName", var("missingFmuString")),
            named_arg("buildingsRootFileLocation", var("missingRootString")),
            named_arg("logLevel", int_lit(1)),
            named_arg("printUnit", bool_lit(false)),
            named_arg("jsonName", var("missingJsonNameString")),
            named_arg("jsonKeysValues", var("missingJsonKeysString")),
            named_arg("parOutNames", var("missingParOutNames")),
            named_arg("parOutUnits", var("missingParOutUnits")),
            named_arg("nParOut", int_lit(0)),
            named_arg("inpNames", var("missingInpNames")),
            named_arg("inpUnits", var("missingInpUnits")),
            named_arg("nInp", int_lit(0)),
            named_arg("outNames", var("missingOutNames")),
            named_arg("outUnits", var("missingOutUnits")),
            named_arg("nOut", int_lit(0)),
            named_arg(
                "derivatives_structure",
                rumoca_core::Expression::Array {
                    elements: Vec::new(),
                    is_matrix: true,
                    span: rumoca_core::Span::DUMMY,
                },
            ),
            named_arg("nDer", int_lit(0)),
            named_arg(
                "derivatives_delta",
                rumoca_core::Expression::Array {
                    elements: Vec::new(),
                    is_matrix: false,
                    span: rumoca_core::Span::DUMMY,
                },
            ),
        ],
        is_constructor: true,
        span: rumoca_core::Span::DUMMY,
    };

    let _lowered = lower_expression(&expr, &layout, &functions)
        .expect("EnergyPlus spawn constructor string metadata should not require numeric slots");
}

#[test]
fn initial_residual_skips_energyplus_adapter_binding() {
    let mut dae_model = dae::Dae::new();
    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: var("zone.fmuZon.adapter"),
        origin: "binding equation for zone.fmuZon.adapter".to_string(),
        span: rumoca_core::Span::DUMMY,
        scalar_count: 1,
    });

    let rows = lower_initial_residual(&dae_model, &VarLayout::default())
        .expect("EnergyPlus adapter binding should not enter initialization residual");

    assert!(rows.is_empty());
}

#[test]
fn energyplus_external_get_parameters_lowers_vector_outputs() {
    let mut dae_model = dae::Dae::new();
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("parOut"),
        dae::Variable {
            name: rumoca_core::VarName::new("parOut"),
            dims: vec![3],
            ..Default::default()
        },
    );
    for name in ["nParOut", "nObj"] {
        dae_model
            .variables
            .parameters
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
    dae_model
        .metadata
        .variable_starts
        .insert("nParOut".to_string(), int_lit(3));

    let mut function = rumoca_core::Function::new(
        "Buildings.ThermalZones.EnergyPlus_9_6_0.BaseClasses.getParameters",
        rumoca_core::Span::DUMMY,
    );
    function.inputs.push(function_param("adapter"));
    function.inputs.push(function_param("nParOut"));
    function.inputs.push(function_param("isSynchronized"));
    function.outputs.push(rumoca_core::FunctionParam {
        def_id: None,
        name: "parOut".to_string(),
        span: rumoca_core::Span::DUMMY,
        type_name: "Real".to_string(),
        type_class: None,
        dims: vec![0],
        shape_expr: vec![rumoca_core::Subscript::generated_expr(Box::new(var(
            "nParOut",
        )))],
        default: None,
        description: None,
    });
    function.external = Some(rumoca_core::ExternalFunction {
        language: "C".to_string(),
        function_name: Some("getParameters_Modelica_EnergyPlus_9_6_0".to_string()),
        arg_names: vec![
            "adapter".to_string(),
            "isSynchronized".to_string(),
            "parOut".to_string(),
        ],
        libraries: vec![
            "ModelicaBuildingsEnergyPlus_9_6_0".to_string(),
            "fmilib_shared".to_string(),
        ],
        ..Default::default()
    });
    dae_model
        .symbols
        .functions
        .insert(function.name.clone(), function);

    dae_model.continuous.equations.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("parOut")),
        rhs: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new(
                "Buildings.ThermalZones.EnergyPlus_9_6_0.BaseClasses.getParameters",
            ),
            args: vec![
                rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::Reference::new(
                        "Buildings.ThermalZones.EnergyPlus_9_6_0.BaseClasses.SpawnExternalObject",
                    ),
                    args: vec![var("adapter")],
                    is_constructor: true,
                    span: rumoca_core::Span::DUMMY,
                },
                named_arg("nParOut", var("nParOut")),
                named_arg("isSynchronized", var("nObj")),
            ],
            is_constructor: false,
            span: rumoca_core::Span::DUMMY,
        },
        span: rumoca_core::Span::DUMMY,
        origin: "EnergyPlus getParameters vector residual".to_string(),
        scalar_count: 3,
    });

    let layout = build_var_layout(&dae_model);
    let rows = lower_residual(&dae_model, &layout)
        .expect("EnergyPlus getParameters vector output should remain explicit in Solve IR");
    let output_indices = rows
        .iter()
        .flat_map(|row| row.iter())
        .filter_map(|op| match op {
            LinearOp::ExternalCall {
                function: rumoca_ir_solve::ExternalFunctionKind::BuildingsEnergyPlusGetParameters,
                output_index,
                ..
            } => Some(*output_index),
            _ => None,
        })
        .collect::<Vec<_>>();

    assert_eq!(rows.len(), 3);
    assert_eq!(output_indices, vec![0, 1, 2, 0, 1, 2, 0, 1, 2]);
}

#[test]
fn energyplus_outer_building_var_ref_uses_inner_binding() {
    let mut bindings = IndexMap::new();
    bindings.insert(
        "building.isSynchronized".to_string(),
        ScalarSlot::Y {
            index: 0,
            byte_offset: 0,
        },
    );
    let layout = VarLayout::from_parts_with_shapes_and_indexed_bindings(
        bindings,
        IndexMap::new(),
        IndexMap::new(),
        1,
        0,
    );
    let expr = var("zone.fmuZon.building.isSynchronized");

    let lowered =
        lower_expression(&expr, &layout, &IndexMap::new()).expect("outer building should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[1.0], &[], 0.0);

    assert_eq!(read_reg(&regs, lowered.result), 1.0);
}

#[test]
fn energyplus_outer_building_var_ref_uses_inner_start_when_slot_is_truncated() {
    let layout = VarLayout::from_parts_with_shapes_and_indexed_bindings(
        IndexMap::new(),
        IndexMap::new(),
        IndexMap::new(),
        0,
        0,
    );
    let starts = IndexMap::from([("building.isSynchronized".to_string(), real_lit(0.25))]);
    let functions = IndexMap::new();
    let mut builder = LowerBuilder::new_with_metadata(
        &layout,
        &functions,
        super::LowerBuilderMetadata {
            variable_starts: Some(&starts),
            ..Default::default()
        },
    );

    let value = builder
        .lower_expr(
            &var("zone.fmuZon.building.isSynchronized"),
            &Scope::new(),
            0,
        )
        .expect("outer building start should lower");
    let (regs, _) = eval_linear_ops(&builder.ops, &[], &[], 0.0);

    assert_eq!(read_reg(&regs, value), 0.25);
}

#[test]
fn energyplus_outer_building_field_access_uses_inner_start_when_slot_is_truncated() {
    let layout = VarLayout::from_parts_with_shapes_and_indexed_bindings(
        IndexMap::new(),
        IndexMap::new(),
        IndexMap::new(),
        0,
        0,
    );
    let starts = IndexMap::from([("building.isSynchronized".to_string(), real_lit(0.5))]);
    let functions = IndexMap::new();
    let mut builder = LowerBuilder::new_with_metadata(
        &layout,
        &functions,
        super::LowerBuilderMetadata {
            variable_starts: Some(&starts),
            ..Default::default()
        },
    );
    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(var("zone.fmuZon.building")),
        field: "isSynchronized".to_string(),
        span: rumoca_core::Span::DUMMY,
    };

    let value = builder
        .lower_expr(&expr, &Scope::new(), 0)
        .expect("outer building field access start should lower");
    let (regs, _) = eval_linear_ops(&builder.ops, &[], &[], 0.0);

    assert_eq!(read_reg(&regs, value), 0.5);
}

#[test]
fn field_access_resolves_singleton_connector_array_alias() {
    let mut dae_model = dae::Dae::new();
    for name in ["infAir[1].ports[1].C_outflow", "infAir[1].C_in_internal"] {
        dae_model
            .variables
            .algebraics
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }

    let lhs = rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::FieldAccess {
            base: Box::new(rumoca_core::Expression::Index {
                base: Box::new(var("infAir")),
                subscripts: vec![rumoca_core::Subscript::generated_index(
                    1,
                    rumoca_core::Span::DUMMY,
                )],
                span: rumoca_core::Span::DUMMY,
            }),
            field: "ports".to_string(),
            span: rumoca_core::Span::DUMMY,
        }),
        field: "C_outflow".to_string(),
        span: rumoca_core::Span::DUMMY,
    };

    dae_model
        .continuous
        .equations
        .push(residual(sub(lhs, var("infAir[1].C_in_internal"))));

    let layout = build_var_layout(&dae_model);
    let rows = lower_residual(&dae_model, &layout)
        .expect("singleton connector array FieldAccess should resolve to [1]");
    let mut y = vec![0.0; layout.y_scalars()];
    set_y_value(&layout, &mut y, "infAir[1].ports[1].C_outflow", 12.0);
    set_y_value(&layout, &mut y, "infAir[1].C_in_internal", 5.0);

    let (_, row0) = eval_linear_ops(&rows[0], &y, &[], 0.0);

    assert_eq!(rows.len(), 1);
    assert_eq!(row0, Some(7.0));
}

#[test]
fn scalar_context_zero_length_array_var_ref_lowers_to_zero() {
    let mut dae_model = dae::Dae::new();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("y"), scalar_var("y"));
    let mut xi = scalar_var("port.Xi_outflow");
    xi.dims = vec![0];
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("port.Xi_outflow"), xi);

    dae_model
        .continuous
        .equations
        .push(residual(sub(var("y"), var("port.Xi_outflow"))));

    let layout = build_var_layout(&dae_model);
    let rows = lower_residual(&dae_model, &layout)
        .expect("zero-length connector species should be scalar-safe as zero");
    let mut y = vec![0.0; layout.y_scalars()];
    set_y_value(&layout, &mut y, "y", 5.0);

    let (_, row0) = eval_linear_ops(&rows[0], &y, &[], 0.0);

    assert_eq!(rows.len(), 1);
    assert_eq!(row0, Some(5.0));
}

#[test]
fn function_array_input_dynamic_index_uses_all_actual_elements() {
    let mut dae_model = dae::Dae::new();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("y"), scalar_var("y"));
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("der"),
        dae::Variable {
            name: rumoca_core::VarName::new("der"),
            dims: vec![3],
            fixed: Some(true),
            ..Default::default()
        },
    );
    dae_model.symbols.functions.insert(
        rumoca_core::VarName::new("Pkg.pickDerivative"),
        rumoca_core::Function {
            name: rumoca_core::VarName::new("Pkg.pickDerivative"),
            def_id: None,
            inputs: vec![
                function_param("selector"),
                function_param_with_dims("d", &[0]),
            ],
            outputs: vec![function_param("out")],
            locals: vec![function_param("i")],
            body: vec![
                rumoca_core::Statement::If {
                    cond_blocks: vec![rumoca_core::StatementBlock {
                        cond: rumoca_core::Expression::Binary {
                            op: rumoca_core::OpBinary::Gt,
                            lhs: Box::new(var("selector")),
                            rhs: Box::new(real_lit(0.5)),
                            span: rumoca_core::Span::DUMMY,
                        },
                        stmts: vec![rumoca_core::Statement::Assignment {
                            comp: component_ref("i"),
                            value: int_lit(2),
                            span: rumoca_core::Span::DUMMY,
                        }],
                    }],
                    else_block: Some(vec![rumoca_core::Statement::Assignment {
                        comp: component_ref("i"),
                        value: int_lit(1),
                        span: rumoca_core::Span::DUMMY,
                    }]),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Statement::Assignment {
                    comp: component_ref("out"),
                    value: rumoca_core::Expression::Index {
                        base: Box::new(var("d")),
                        subscripts: vec![rumoca_core::Subscript::generated_expr(Box::new(var(
                            "i",
                        )))],
                        span: rumoca_core::Span::DUMMY,
                    },
                    span: rumoca_core::Span::DUMMY,
                },
            ],
            is_constructor: false,
            pure: true,
            external: None,
            derivatives: vec![],
            span: rumoca_core::Span::DUMMY,
        },
    );

    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(var("y")),
            rhs: Box::new(rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.pickDerivative"),
                args: vec![real_lit(1.0), var("der")],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        origin: "binding equation for y".to_string(),
        span: rumoca_core::Span::DUMMY,
        scalar_count: 1,
    });

    let layout = build_var_layout(&dae_model);
    let rows = lower_residual(&dae_model, &layout)
        .expect("array input dynamic index should see all actual elements");
    let mut y = vec![0.0; layout.y_scalars()];
    let mut p = vec![0.0; layout.p_scalars()];
    set_y_value(&layout, &mut y, "y", 10.0);
    set_p_value(&layout, &mut p, "der[1]", 3.0);
    set_p_value(&layout, &mut p, "der[2]", 4.0);
    set_p_value(&layout, &mut p, "der[3]", 5.0);

    let (_, row0) = eval_linear_ops(&rows[0], &y, &p, 0.0);

    assert_eq!(rows.len(), 1);
    assert_eq!(row0, Some(6.0));
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
            LinearOp::ExternalCall { dst, .. } => write_reg(&mut regs, dst, f64::NAN),
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

fn bool_lit(value: bool) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Boolean(value),
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
