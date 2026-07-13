use super::*;

fn test_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_lower_function_calls_tests.mo"),
        1,
        2,
    )
}

fn real(value: f64) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(value),
        span: test_span(),
    }
}

fn int(value: i64) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Integer(value),
        span: test_span(),
    }
}

fn bool_lit(value: bool) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Boolean(value),
        span: test_span(),
    }
}

fn string(value: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::String(value.to_string()),
        span: test_span(),
    }
}

fn var_ref(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new(name),
        subscripts: Vec::new(),
        span: test_span(),
    }
}

fn array(elements: Vec<rumoca_core::Expression>) -> rumoca_core::Expression {
    array_with_matrix_flag(elements, false)
}

fn matrix(elements: Vec<rumoca_core::Expression>) -> rumoca_core::Expression {
    array_with_matrix_flag(elements, true)
}

fn array_with_matrix_flag(
    elements: Vec<rumoca_core::Expression>,
    is_matrix: bool,
) -> rumoca_core::Expression {
    rumoca_core::Expression::Array {
        elements,
        is_matrix,
        span: test_span(),
    }
}

fn external_time_table_constructor() -> rumoca_core::Expression {
    external_time_table_constructor_with(
        matrix(vec![
            array(vec![real(0.0), real(2.0)]),
            array(vec![real(1.0), real(4.0)]),
        ]),
        real(0.0),
    )
}

fn external_time_table_constructor_with(
    table: rumoca_core::Expression,
    start_time: rumoca_core::Expression,
) -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from("Modelica.Blocks.Types.ExternalCombiTimeTable"),
        args: vec![
            string("NoName"),
            string("NoName"),
            table,
            start_time,
            array(vec![int(2)]),
            int(1),
            int(1),
            real(0.0),
            int(1),
            bool_lit(false),
            string(","),
            int(0),
        ],
        is_constructor: true,
        span: test_span(),
    }
}

#[test]
fn checked_usize_dims_to_i64_rejects_overflow_with_span() {
    let Some(dim) = usize::try_from(i64::MAX)
        .ok()
        .and_then(|value| value.checked_add(1))
    else {
        return;
    };
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_function_calls_tests_source_45.mo",
        ),
        2,
        10,
    );

    let err = checked_usize_dims_to_i64(&[dim], "function input actual shape", span)
        .expect_err("function-call dimensions must fit in Modelica integer range");

    assert_eq!(err.source_span(), Some(span));
    assert_eq!(
        err.reason(),
        format!(
            "invalid IR contract: function input actual shape dimension {dim} exceeds i64 range"
        )
    );
}

#[test]
fn lower_external_table_lookup_accepts_flattened_time_table_record_args() {
    let table = matrix(vec![
        array(vec![real(0.0), real(2.0)]),
        array(vec![real(1.0), real(4.0)]),
    ]);
    let columns = array(vec![int(2)]);
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from(
            "Modelica.Blocks.Tables.Internal.getTimeTableValueNoDer",
        ),
        args: vec![
            string("NoName"),
            string("NoName"),
            table,
            real(0.0),
            columns,
            int(1),
            int(1),
            real(0.0),
            int(1),
            bool_lit(false),
            string(","),
            int(0),
            int(1),
            real(0.5),
            real(1.0),
            real(0.0),
        ],
        is_constructor: false,
        span: test_span(),
    };

    let lowered = lower_expression(
        &expr,
        &rumoca_ir_solve::VarLayout::default(),
        &IndexMap::new(),
    )
    .expect("flattened table record args should lower as external table lookup");

    assert!(
        lowered
            .ops
            .iter()
            .any(|op| matches!(op, rumoca_ir_solve::LinearOp::TableLookup { .. })),
        "lowered ops should include a host-backed table lookup: {:?}",
        lowered.ops
    );
}

#[test]
fn lower_external_table_lookup_consumes_repeated_flattened_constructor_record_args() {
    let constructor = external_time_table_constructor();
    let mut args =
        vec![constructor; ExternalTableRecordKind::CombiTimeTable.flattened_field_count()];
    args.extend([int(1), real(0.5), real(1.0), real(0.0)]);
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from(
            "Modelica.Blocks.Tables.Internal.getTimeTableValueNoDer",
        ),
        args,
        is_constructor: false,
        span: test_span(),
    };

    let lowered = lower_expression(
        &expr,
        &rumoca_ir_solve::VarLayout::default(),
        &IndexMap::new(),
    )
    .expect("repeated flattened constructor record args should lower");

    assert!(
        lowered
            .ops
            .iter()
            .any(|op| matches!(op, rumoca_ir_solve::LinearOp::TableLookup { .. })),
        "lowered ops should include a host-backed table lookup: {:?}",
        lowered.ops
    );
}

#[test]
fn lower_external_table_lookup_accepts_projected_output_with_constructor_arg() {
    let mut args = vec![external_time_table_constructor()];
    args.extend([int(1), real(0.5), real(1.0), real(0.0)]);
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from(
            "Modelica.Blocks.Tables.Internal.getTimeTableValueNoDer.y",
        ),
        args,
        is_constructor: false,
        span: test_span(),
    };

    let lowered = lower_expression(
        &expr,
        &rumoca_ir_solve::VarLayout::default(),
        &IndexMap::new(),
    )
    .expect("projected table lookup with constructor arg should lower");

    assert!(
        lowered
            .ops
            .iter()
            .any(|op| matches!(op, rumoca_ir_solve::LinearOp::TableLookup { .. })),
        "lowered ops should include a host-backed table lookup: {:?}",
        lowered.ops
    );
}

#[test]
fn lower_external_table_lookup_reuses_structural_table_id_for_flattened_record_fields() {
    let mut args = vec![
        var_ref("block.table.tableName"),
        var_ref("block.table.fileName"),
        var_ref("block.table.table"),
        var_ref("block.table.startTime"),
        var_ref("block.table.columns"),
        var_ref("block.table.smoothness"),
        var_ref("block.table.extrapolation"),
        var_ref("block.table.shiftTime"),
        var_ref("block.table.timeEvents"),
        var_ref("block.table.verboseRead"),
        var_ref("block.table.delimiter"),
        var_ref("block.table.nHeaderLines"),
    ];
    args.extend([int(1), real(0.5), real(1.0), real(0.0)]);
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from(
            "Modelica.Blocks.Tables.Internal.getTimeTableValueNoDer",
        ),
        args,
        is_constructor: false,
        span: test_span(),
    };
    let structural_bindings = Arc::new(IndexMap::from([(
        "block.table.tableID".to_string(),
        12345.0,
    )]));
    let layout = rumoca_ir_solve::VarLayout::default();
    let functions = IndexMap::new();
    let mut builder =
        LowerBuilder::new(&layout, &functions).with_structural_bindings(structural_bindings);

    builder
        .lower_expr(&expr, &Scope::new(), 0)
        .expect("flattened table record should lower via structural tableID");

    assert!(
        builder.ops.iter().any(
            |op| matches!(op, rumoca_ir_solve::LinearOp::Const { value, .. } if *value == 12345.0)
        ),
        "lowered ops should load the structural table id: {:?}",
        builder.ops
    );
    assert!(
        builder
            .ops
            .iter()
            .any(|op| matches!(op, rumoca_ir_solve::LinearOp::TableLookup { .. })),
        "lowered ops should include a host-backed table lookup: {:?}",
        builder.ops
    );
}

#[test]
fn lower_external_table_lookup_reuses_structural_table_id_for_repeated_constructor_record_args() {
    let constructor =
        external_time_table_constructor_with(matrix(Vec::new()), var_ref("block.table.startTime"));
    let mut args =
        vec![constructor; ExternalTableRecordKind::CombiTimeTable.flattened_field_count()];
    args.extend([int(1), real(0.5), real(1.0), real(0.0)]);
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from(
            "Modelica.Blocks.Tables.Internal.getTimeTableValueNoDer",
        ),
        args,
        is_constructor: false,
        span: test_span(),
    };
    let structural_bindings = Arc::new(IndexMap::from([(
        "block.table.tableID".to_string(),
        12345.0,
    )]));
    let layout = rumoca_ir_solve::VarLayout::default();
    let functions = IndexMap::new();
    let mut builder =
        LowerBuilder::new(&layout, &functions).with_structural_bindings(structural_bindings);

    builder
        .lower_expr(&expr, &Scope::new(), 0)
        .expect("repeated constructor record args should reuse structural tableID");

    assert!(
        builder.ops.iter().any(
            |op| matches!(op, rumoca_ir_solve::LinearOp::Const { value, .. } if *value == 12345.0)
        ),
        "lowered ops should load the structural table id: {:?}",
        builder.ops
    );
    assert!(
        builder
            .ops
            .iter()
            .any(|op| matches!(op, rumoca_ir_solve::LinearOp::TableLookup { .. })),
        "lowered ops should include a host-backed table lookup: {:?}",
        builder.ops
    );
}
