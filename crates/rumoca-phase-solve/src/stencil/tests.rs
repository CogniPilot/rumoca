use super::*;

fn unspanned_stencil_test_span() -> rumoca_core::Span {
    rumoca_core::Span::DUMMY
}

fn push_structured_programs_fixture(
    nodes: &mut Vec<solve::ComputeNode>,
    rows: &mut Vec<StructuredProgram>,
    structured_equations: &[dae::StructuredEquationFamily],
    dae_equations: &[dae::Equation],
) {
    push_structured_programs(nodes, rows, structured_equations, dae_equations)
        .expect("structured program fixture metadata should match row count");
}

fn load_y_range(op_position: usize, y_range: std::ops::Range<usize>) -> StructuredLoadYRange {
    StructuredLoadYRange {
        op_position,
        y_range,
    }
}

fn domain_scalar_count(domain: &rumoca_core::StructuredIndexDomain) -> usize {
    domain
        .scalar_count()
        .expect("fixture domain should have a valid scalar count")
}

fn stencil_test_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_stencil_fixture.mo"),
        1,
        2,
    )
}

#[test]
fn structured_y_slot_ranges_rejects_index_end_overflow() {
    let mut bindings = IndexMap::new();
    bindings.insert(
        "u[1]".to_string(),
        solve::ScalarSlot::Y {
            index: usize::MAX,
            byte_offset: 0,
        },
    );
    let layout = solve::VarLayout::from_parts(bindings, 0, 0);

    let err = structured_y_slot_ranges(&layout)
        .expect_err("Y-slot range construction should reject overflowing slot end");

    assert_eq!(err.source_span(), None);
    assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
    assert!(err.reason().contains("Y-slot range end overflows"));
}

#[test]
fn same_component_load_rejects_inverted_ranges() {
    assert!(!is_same_component_load(
        4,
        0,
        0,
        &std::ops::Range { start: 4, end: 1 },
        &[load_y_range(0, 0..4)],
    ));
    assert!(!is_same_component_load(
        4,
        0,
        0,
        &(0..4),
        &[load_y_range(0, std::ops::Range { start: 4, end: 1 })],
    ));
}

fn access_proof(operands: Vec<StructuredAccessOperand>) -> StructuredAccessProof {
    StructuredAccessProof { operands }
}

fn literal_expr(value: rumoca_core::Literal) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value,
        span: stencil_test_span(),
    }
}

#[test]
fn structured_access_proof_rejects_string_literals() {
    let mut builder = StructuredAccessProofBuilder::new();
    assert!(
        builder
            .collect_expression_result::<_, LowerError>(
                &literal_expr(rumoca_core::Literal::String("not numeric".to_string())),
                |_, _, _, _| Ok(None),
            )
            .expect("string literal proof should not error")
            .is_none(),
        "string literals should not become numeric tensor proof constants"
    );
}

fn decay_row(y: usize, p: usize, dae_equation_index: usize) -> StructuredProgram {
    StructuredProgram {
        dae_equation_index: Some(dae_equation_index),
        access_proof: Some(access_proof(vec![
            StructuredAccessOperand::LoadP(p),
            StructuredAccessOperand::LoadY(y),
        ])),
        output_index: y,
        pointwise_output_y_index: Some(y),
        span: stencil_test_span(),
        output_y_range: y..y + 1,
        load_y_ranges: vec![load_y_range(1, y..y + 1)],
        ops: vec![
            solve::LinearOp::LoadP { dst: 0, index: p },
            solve::LinearOp::LoadY { dst: 1, index: y },
            solve::LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Mul,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::Unary {
                dst: 3,
                op: solve::UnaryOp::Neg,
                arg: 2,
            },
            solve::LinearOp::StoreOutput { src: 3 },
        ],
    }
}

fn family(start: usize, count: usize) -> dae::StructuredEquationFamily {
    dae::StructuredEquationFamily {
        domain: test_domain(count),
        first_equation_index: start,
        equations_per_point: 1,
        span: stencil_test_span(),
        origin: "test".to_string(),
        regular: None,
        template: None,
        interiors_materialized: true,
    }
}

fn multi_equation_family(
    start: usize,
    count: usize,
    equation_count: usize,
) -> dae::StructuredEquationFamily {
    dae::StructuredEquationFamily {
        domain: test_domain(count),
        first_equation_index: start,
        equations_per_point: equation_count,
        span: stencil_test_span(),
        origin: "test".to_string(),
        regular: None,
        template: None,
        interiors_materialized: true,
    }
}

fn test_domain(count: usize) -> rumoca_core::StructuredIndexDomain {
    rumoca_core::StructuredIndexDomain {
        binders: vec![rumoca_core::StructuredIndexBinder {
            id: 0,
            display_name: "i".to_string(),
            lower: 1,
            upper: count as i64,
            step: 1,
        }],
    }
}

fn test_domain_2d(rows: usize, cols: usize) -> rumoca_core::StructuredIndexDomain {
    rumoca_core::StructuredIndexDomain {
        binders: vec![
            rumoca_core::StructuredIndexBinder {
                id: 0,
                display_name: "i".to_string(),
                lower: 1,
                upper: rows as i64,
                step: 1,
            },
            rumoca_core::StructuredIndexBinder {
                id: 1,
                display_name: "j".to_string(),
                lower: 1,
                upper: cols as i64,
                step: 1,
            },
        ],
    }
}

fn test_domain_3d(d0: usize, d1: usize, d2: usize) -> rumoca_core::StructuredIndexDomain {
    rumoca_core::StructuredIndexDomain {
        binders: vec![
            rumoca_core::StructuredIndexBinder {
                id: 0,
                display_name: "i".to_string(),
                lower: 1,
                upper: d0 as i64,
                step: 1,
            },
            rumoca_core::StructuredIndexBinder {
                id: 1,
                display_name: "j".to_string(),
                lower: 1,
                upper: d1 as i64,
                step: 1,
            },
            rumoca_core::StructuredIndexBinder {
                id: 2,
                display_name: "k".to_string(),
                lower: 1,
                upper: d2 as i64,
                step: 1,
            },
        ],
    }
}

fn family_2d(start: usize, rows: usize, cols: usize) -> dae::StructuredEquationFamily {
    dae::StructuredEquationFamily {
        domain: test_domain_2d(rows, cols),
        first_equation_index: start,
        equations_per_point: 1,
        span: stencil_test_span(),
        origin: "test".to_string(),
        regular: None,
        template: None,
        interiors_materialized: true,
    }
}

fn stencil_row(y: usize, dae_equation_index: usize) -> StructuredProgram {
    StructuredProgram {
        dae_equation_index: Some(dae_equation_index),
        access_proof: Some(access_proof(vec![
            StructuredAccessOperand::LoadY(y),
            StructuredAccessOperand::LoadY(y + 1),
        ])),
        output_index: y,
        pointwise_output_y_index: Some(y),
        span: stencil_test_span(),
        output_y_range: 0..4,
        load_y_ranges: vec![load_y_range(0, 0..5), load_y_range(1, 0..5)],
        ops: vec![
            solve::LinearOp::LoadY { dst: 0, index: y },
            solve::LinearOp::LoadY {
                dst: 1,
                index: y + 1,
            },
            solve::LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Add,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::StoreOutput { src: 2 },
        ],
    }
}

fn nonlinear_stencil_row(y: usize, dae_equation_index: usize) -> StructuredProgram {
    StructuredProgram {
        dae_equation_index: Some(dae_equation_index),
        access_proof: Some(access_proof(vec![
            StructuredAccessOperand::LoadY(y + 1),
            StructuredAccessOperand::LoadY(y),
            StructuredAccessOperand::Const(2.0),
        ])),
        output_index: y,
        pointwise_output_y_index: Some(y),
        span: stencil_test_span(),
        output_y_range: 0..4,
        load_y_ranges: vec![load_y_range(0, 0..5), load_y_range(2, 0..5)],
        ops: vec![
            solve::LinearOp::LoadY {
                dst: 0,
                index: y + 1,
            },
            solve::LinearOp::Unary {
                dst: 1,
                op: solve::UnaryOp::Sin,
                arg: 0,
            },
            solve::LinearOp::LoadY { dst: 2, index: y },
            solve::LinearOp::Const { dst: 3, value: 2.0 },
            solve::LinearOp::Binary {
                dst: 4,
                op: solve::BinaryOp::Pow,
                lhs: 2,
                rhs: 3,
            },
            solve::LinearOp::Binary {
                dst: 5,
                op: solve::BinaryOp::Add,
                lhs: 1,
                rhs: 4,
            },
            solve::LinearOp::StoreOutput { src: 5 },
        ],
    }
}

fn pointwise_row(y: usize, dae_equation_index: usize) -> StructuredProgram {
    StructuredProgram {
        dae_equation_index: Some(dae_equation_index),
        access_proof: Some(access_proof(vec![StructuredAccessOperand::LoadY(y)])),
        output_index: y,
        pointwise_output_y_index: Some(y),
        span: stencil_test_span(),
        output_y_range: y..y + 1,
        load_y_ranges: vec![load_y_range(0, y..y + 1)],
        ops: vec![
            solve::LinearOp::LoadY { dst: 0, index: y },
            solve::LinearOp::StoreOutput { src: 0 },
        ],
    }
}

fn scaled_row(y: usize, coefficient: f64, dae_equation_index: usize) -> StructuredProgram {
    StructuredProgram {
        dae_equation_index: Some(dae_equation_index),
        access_proof: Some(access_proof(vec![
            StructuredAccessOperand::Const(coefficient),
            StructuredAccessOperand::LoadY(y),
        ])),
        output_index: y,
        pointwise_output_y_index: Some(y),
        span: stencil_test_span(),
        output_y_range: y..y + 1,
        load_y_ranges: vec![load_y_range(1, y..y + 1)],
        ops: vec![
            solve::LinearOp::Const {
                dst: 0,
                value: coefficient,
            },
            solve::LinearOp::LoadY { dst: 1, index: y },
            solve::LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Mul,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::StoreOutput { src: 2 },
        ],
    }
}

fn shifted_single_load_row(y: usize, dae_equation_index: usize) -> StructuredProgram {
    StructuredProgram {
        dae_equation_index: Some(dae_equation_index),
        access_proof: Some(access_proof(vec![StructuredAccessOperand::LoadY(y + 1)])),
        output_index: y,
        pointwise_output_y_index: Some(y),
        span: stencil_test_span(),
        output_y_range: 0..5,
        load_y_ranges: vec![load_y_range(0, 0..5)],
        ops: vec![
            solve::LinearOp::LoadY {
                dst: 0,
                index: y + 1,
            },
            solve::LinearOp::StoreOutput { src: 0 },
        ],
    }
}

fn pointwise_other_array_row(y: usize, dae_equation_index: usize) -> StructuredProgram {
    StructuredProgram {
        dae_equation_index: Some(dae_equation_index),
        access_proof: Some(access_proof(vec![StructuredAccessOperand::LoadY(y + 4)])),
        output_index: y,
        pointwise_output_y_index: Some(y),
        span: stencil_test_span(),
        output_y_range: 0..4,
        load_y_ranges: vec![load_y_range(0, 4..8)],
        ops: vec![
            solve::LinearOp::LoadY {
                dst: 0,
                index: y + 4,
            },
            solve::LinearOp::StoreOutput { src: 0 },
        ],
    }
}

fn shifted_other_array_row(y: usize, dae_equation_index: usize) -> StructuredProgram {
    StructuredProgram {
        dae_equation_index: Some(dae_equation_index),
        access_proof: Some(access_proof(vec![StructuredAccessOperand::LoadY(y + 5)])),
        output_index: y,
        pointwise_output_y_index: Some(y),
        span: stencil_test_span(),
        output_y_range: 0..4,
        load_y_ranges: vec![load_y_range(0, 4..8)],
        ops: vec![
            solve::LinearOp::LoadY {
                dst: 0,
                index: y + 5,
            },
            solve::LinearOp::StoreOutput { src: 0 },
        ],
    }
}

fn var_ref(name: &str, index: i64) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new(name),
        subscripts: vec![rumoca_core::Subscript::Index {
            value: index,
            span: stencil_test_span(),
        }],
        span: stencil_test_span(),
    }
}

fn der(expr: rumoca_core::Expression) -> rumoca_core::Expression {
    rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Der,
        args: vec![expr],
        span: stencil_test_span(),
    }
}

fn residual_equation(lhs: rumoca_core::Expression, rhs: rumoca_core::Expression) -> dae::Equation {
    dae::Equation::residual(
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: stencil_test_span(),
        },
        stencil_test_span(),
        "test",
    )
}

fn matching_dae_equations(count: usize) -> Vec<dae::Equation> {
    (0..count)
        .map(|offset| {
            let index = offset as i64 + 1;
            residual_equation(der(var_ref("u", index)), var_ref("w", index))
        })
        .collect()
}

fn tensor_decline_at(
    rows: &[StructuredProgram],
    structured_equations: &[dae::StructuredEquationFamily],
    dae_equations: &[dae::Equation],
) -> Result<StructuredTensorDecline, LowerError> {
    let slots = structured_slots_for_rows(rows, structured_equations)?;
    let consumed = vec![false; rows.len()];
    match structured_tensor_at(
        rows,
        &slots,
        0,
        structured_equations,
        dae_equations,
        consumed.as_slice(),
    )? {
        StructuredTensorDecision::Scalar { decline, .. } => Ok(decline),
        StructuredTensorDecision::Preserve(_) => {
            panic!("expected structured tensor candidate to decline")
        }
    }
}

#[test]
fn structured_slot_row_lookup_preserves_first_unconsumed_row() -> Result<(), LowerError> {
    let first = dae::StructuredEquationSlot {
        family_index: 0,
        iteration_index: 0,
        equation_position: 0,
        equation_count: 1,
    };
    let second = dae::StructuredEquationSlot {
        family_index: 0,
        iteration_index: 1,
        equation_position: 0,
        equation_count: 1,
    };
    let slots = vec![Some(first), Some(second), Some(first), None];
    let consumed = vec![true, false, false, false];

    let lookup = structured_slot_row_lookup(&slots, &consumed, stencil_test_span())?;

    assert_eq!(lookup.get(&first), Some(&2));
    assert_eq!(lookup.get(&second), Some(&1));
    assert_eq!(lookup.values().copied().collect::<Vec<_>>(), vec![1, 2]);
    Ok(())
}

#[test]
fn structured_slot_row_lookup_rejects_consumed_slot_count_mismatch() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_stencil_tests_source_31.mo"),
        2,
        9,
    );
    let slot = dae::StructuredEquationSlot {
        family_index: 0,
        iteration_index: 0,
        equation_position: 0,
        equation_count: 1,
    };
    let slots = vec![Some(slot), None];
    let consumed = vec![false];

    let err = structured_slot_row_lookup(&slots, &consumed, span)
        .expect_err("parallel consumed flags must match structured slots");

    assert_eq!(err.source_span(), Some(span));
    assert!(matches!(err, LowerError::ContractViolation { .. }));
    assert!(
        err.reason()
            .contains("structured consumed flag count 1 does not match slot count 2")
    );
}

#[test]
fn compact_domain_bubbles_step_overflow_with_span() {
    let source_domain = rumoca_core::StructuredIndexDomain {
        binders: vec![rumoca_core::StructuredIndexBinder {
            id: 0,
            display_name: "i".to_string(),
            lower: i64::MIN,
            upper: i64::MAX,
            step: 1,
        }],
    };
    let tuples = vec![vec![i64::MIN], vec![i64::MAX]];
    let span =
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name("test"), 4, 8);

    let err = match compact_domain_from_tuples(&source_domain, &tuples, span) {
        Err(err) => err,
        Ok(value) => panic!("expected compact-domain overflow error, got {value:?}"),
    };

    let LowerError::ContractViolation {
        reason,
        span: actual_span,
    } = err
    else {
        panic!("expected compact-domain contract violation");
    };
    assert_eq!(actual_span, span);
    assert!(reason.contains("compact domain binder step overflows"));
}

fn rhs_plus_one(expr: rumoca_core::Expression) -> rumoca_core::Expression {
    rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Add,
        lhs: Box::new(expr),
        rhs: Box::new(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(1),
            span: stencil_test_span(),
        }),
        span: stencil_test_span(),
    }
}

fn rhs_times_integer(value: i64, expr: rumoca_core::Expression) -> rumoca_core::Expression {
    rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Mul,
        lhs: Box::new(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(value),
            span: stencil_test_span(),
        }),
        rhs: Box::new(expr),
        span: stencil_test_span(),
    }
}

#[test]
fn preserves_family_as_map_node() {
    let dae_equations = matching_dae_equations(4);
    let mut rows = (0..4).map(|offset| decay_row(offset, 0, offset)).collect();
    let mut nodes = Vec::new();
    push_structured_programs_fixture(&mut nodes, &mut rows, &[family(0, 4)], &dae_equations);
    assert_eq!(nodes.len(), 1);
    let solve::ComputeNode::Map {
        domain,
        load_strides,
        ..
    } = &nodes[0]
    else {
        panic!("expected AffineStencil");
    };
    assert_eq!(domain_scalar_count(domain), 4);
    assert_eq!(domain.binders[0].display_name, "i");
    assert_eq!(
        domain
            .index_tuples()
            .expect("fixture domain should enumerate index tuples"),
        vec![vec![1], vec![2], vec![3], vec![4]]
    );
    assert_eq!(
        load_strides,
        &vec![solve::AffineStencilLoadStride {
            op_position: 1,
            terms: vec![solve::AffineStencilIndexStrideTerm {
                dimension: 0,
                stride: 1,
            }],
        }]
    );
}

#[test]
fn leaves_anonymous_scalar_rows_scalar_without_structured_slots() -> Result<(), LowerError> {
    let mut rows: Vec<_> = (0..4)
        .map(|offset| {
            let mut row = decay_row(offset, 0, 10 + offset);
            row.dae_equation_index = None;
            row
        })
        .collect();
    assert_eq!(
        tensor_decline_at(rows.as_slice(), &[], &[])?,
        StructuredTensorDecline::MissingStructuredSlot
    );
    let mut nodes = Vec::new();
    push_structured_programs_fixture(&mut nodes, &mut rows, &[], &[]);

    assert!(matches!(
        &nodes[..],
        [solve::ComputeNode::ScalarPrograms(block)] if block.programs.len() == 4
    ));
    Ok(())
}

#[test]
fn leaves_unproven_structured_rows_scalar() -> Result<(), LowerError> {
    let dae_equations = matching_dae_equations(4);
    let mut rows: Vec<_> = (0..4)
        .map(|offset| {
            let mut row = decay_row(offset, 0, offset);
            row.access_proof = None;
            row
        })
        .collect();
    assert_eq!(
        tensor_decline_at(rows.as_slice(), &[family(0, 4)], &dae_equations)?,
        StructuredTensorDecline::MissingAffineAccessProof
    );
    let mut nodes = Vec::new();
    push_structured_programs_fixture(&mut nodes, &mut rows, &[family(0, 4)], &dae_equations);

    assert!(matches!(
        &nodes[..],
        [solve::ComputeNode::ScalarPrograms(block)] if block.programs.len() == 4
    ));
    Ok(())
}

#[test]
fn leaves_structured_rows_scalar_without_dae_equation_bodies() -> Result<(), LowerError> {
    let mut rows: Vec<_> = (0..4).map(|offset| decay_row(offset, 0, offset)).collect();
    assert_eq!(
        tensor_decline_at(rows.as_slice(), &[family(0, 4)], &[])?,
        StructuredTensorDecline::MismatchedDaeBodyShape
    );
    let mut nodes = Vec::new();
    push_structured_programs_fixture(&mut nodes, &mut rows, &[family(0, 4)], &[]);

    assert!(matches!(
        &nodes[..],
        [solve::ComputeNode::ScalarPrograms(block)] if block.programs.len() == 4
    ));
    Ok(())
}

#[test]
fn leaves_single_structured_row_scalar() -> Result<(), LowerError> {
    let mut rows = vec![decay_row(0, 0, 0)];
    let dae_equations = matching_dae_equations(1);
    assert_eq!(
        tensor_decline_at(rows.as_slice(), &[family(0, 1)], &dae_equations)?,
        StructuredTensorDecline::TooFewStructuredRows
    );
    let mut nodes = Vec::new();
    push_structured_programs_fixture(&mut nodes, &mut rows, &[family(0, 1)], &dae_equations);

    assert!(matches!(
        &nodes[..],
        [solve::ComputeNode::ScalarPrograms(block)] if block.programs.len() == 1
    ));
    Ok(())
}

#[test]
fn leaves_orphaned_structured_slot_scalar() -> Result<(), LowerError> {
    let rows = vec![decay_row(0, 0, 0), decay_row(1, 0, 1)];
    let slots = vec![
        Some(dae::StructuredEquationSlot {
            family_index: 99,
            iteration_index: 0,
            equation_position: 0,
            equation_count: 1,
        }),
        Some(dae::StructuredEquationSlot {
            family_index: 99,
            iteration_index: 1,
            equation_position: 0,
            equation_count: 1,
        }),
    ];
    let consumed = vec![false; rows.len()];

    let StructuredTensorDecision::Scalar { decline, .. } =
        structured_tensor_at(&rows, &slots, 0, &[], &[], consumed.as_slice())?
    else {
        panic!("expected orphaned structured slot to decline")
    };
    assert_eq!(decline, StructuredTensorDecline::MissingStructuredFamily);
    assert_eq!(decline.code(), "solve:missing-structured-family");
    Ok(())
}

#[test]
fn preserves_two_dimensional_family_as_one_map_node() {
    let dae_equations = matching_dae_equations(6);
    let mut rows = (0..6).map(|offset| pointwise_row(offset, offset)).collect();
    let mut nodes = Vec::new();
    push_structured_programs_fixture(&mut nodes, &mut rows, &[family_2d(0, 2, 3)], &dae_equations);
    assert_eq!(nodes.len(), 1);
    let solve::ComputeNode::Map {
        domain,
        load_strides,
        ..
    } = &nodes[0]
    else {
        panic!("expected Map");
    };
    assert_eq!(domain_scalar_count(domain), 6);
    assert_eq!(
        load_strides,
        &vec![solve::AffineStencilLoadStride {
            op_position: 0,
            terms: vec![
                solve::AffineStencilIndexStrideTerm {
                    dimension: 0,
                    stride: 3,
                },
                solve::AffineStencilIndexStrideTerm {
                    dimension: 1,
                    stride: 1,
                },
            ],
        }]
    );
}

#[test]
fn preserves_interleaved_family_as_sparse_output_map_node() {
    let dae_equations = matching_dae_equations(3);
    let mut residual = pointwise_row(1, 99);
    residual.dae_equation_index = None;
    let mut rows = vec![
        pointwise_row(0, 0),
        residual,
        pointwise_row(2, 1),
        pointwise_row(4, 2),
    ];
    let mut nodes = Vec::new();
    push_structured_programs_fixture(&mut nodes, &mut rows, &[family(0, 3)], &dae_equations);

    let [
        solve::ComputeNode::Map {
            domain, output_map, ..
        },
        solve::ComputeNode::ScalarPrograms(block),
    ] = &nodes[..]
    else {
        panic!("expected sparse Map followed by scalar residual");
    };
    assert_eq!(domain_scalar_count(domain), 3);
    assert_eq!(output_map.start, 0);
    assert_eq!(
        output_map.strides,
        vec![solve::AffineStencilIndexStrideTerm {
            dimension: 0,
            stride: 2,
        }]
    );
    assert_eq!(block.output_indices, vec![1]);
}

#[test]
fn preserves_full_array_output_stride_for_compact_two_dimensional_domain() {
    let dae_equations = matching_dae_equations(6);
    let output_indices = [5, 6, 7, 10, 11, 12];
    let mut rows = output_indices
        .into_iter()
        .enumerate()
        .map(|(offset, output_index)| pointwise_row(output_index, offset))
        .collect();
    let mut nodes = Vec::new();
    push_structured_programs_fixture(&mut nodes, &mut rows, &[family_2d(0, 2, 3)], &dae_equations);

    let [
        solve::ComputeNode::Map {
            domain,
            output_map,
            load_strides,
            ..
        },
    ] = &nodes[..]
    else {
        panic!("expected one sparse 2-D Map");
    };
    assert_eq!(domain_scalar_count(domain), 6);
    let expected_terms = vec![
        solve::AffineStencilIndexStrideTerm {
            dimension: 0,
            stride: 5,
        },
        solve::AffineStencilIndexStrideTerm {
            dimension: 1,
            stride: 1,
        },
    ];
    assert_eq!(output_map.strides, expected_terms);
    assert_eq!(load_strides[0].terms, output_map.strides);
}

#[test]
fn preserves_multi_load_family_as_stencil_node() {
    let dae_equations = matching_dae_equations(4);
    let mut rows = (0..4).map(|offset| stencil_row(offset, offset)).collect();
    let mut nodes = Vec::new();
    push_structured_programs_fixture(&mut nodes, &mut rows, &[family(0, 4)], &dae_equations);
    assert!(matches!(
        &nodes[..],
        [solve::ComputeNode::AffineStencil { domain, .. }] if domain_scalar_count(domain) == 4
    ));
}

#[test]
fn preserves_nonlinear_affine_neighborhood_as_stencil_node() {
    let dae_equations = matching_dae_equations(4);
    let mut rows = (0..4)
        .map(|offset| nonlinear_stencil_row(offset, offset))
        .collect();
    let mut nodes = Vec::new();
    push_structured_programs_fixture(&mut nodes, &mut rows, &[family(0, 4)], &dae_equations);
    assert!(matches!(
        &nodes[..],
        [solve::ComputeNode::AffineStencil { domain, .. }] if domain_scalar_count(domain) == 4
    ));
}

#[test]
fn preserves_shifted_single_load_family_as_stencil_node() {
    let dae_equations = matching_dae_equations(4);
    let mut rows = (0..4)
        .map(|offset| shifted_single_load_row(offset, offset))
        .collect();
    let mut nodes = Vec::new();
    push_structured_programs_fixture(&mut nodes, &mut rows, &[family(0, 4)], &dae_equations);
    assert!(matches!(
        &nodes[..],
        [solve::ComputeNode::AffineStencil { domain, .. }] if domain_scalar_count(domain) == 4
    ));
}

#[test]
fn preserves_same_domain_other_array_assignment_as_map_node() {
    let dae_equations = matching_dae_equations(4);
    let mut rows = (0..4)
        .map(|offset| pointwise_other_array_row(offset, offset))
        .collect();
    let mut nodes = Vec::new();
    push_structured_programs_fixture(&mut nodes, &mut rows, &[family(0, 4)], &dae_equations);
    assert!(matches!(
        &nodes[..],
        [solve::ComputeNode::Map { domain, .. }] if domain_scalar_count(domain) == 4
    ));
}

#[test]
fn preserves_shifted_other_array_assignment_as_stencil_node() {
    let dae_equations = matching_dae_equations(3);
    let mut rows = (0..3)
        .map(|offset| shifted_other_array_row(offset, offset))
        .collect();
    let mut nodes = Vec::new();
    push_structured_programs_fixture(&mut nodes, &mut rows, &[family(0, 3)], &dae_equations);
    assert!(matches!(
        &nodes[..],
        [solve::ComputeNode::AffineStencil { domain, .. }] if domain_scalar_count(domain) == 3
    ));
}

#[test]
fn leaves_matching_rows_scalar_without_family() {
    let mut rows = (0..4)
        .map(|offset| decay_row(offset, 0, 10 + offset))
        .collect();
    let mut nodes = Vec::new();
    push_structured_programs_fixture(&mut nodes, &mut rows, &[], &[]);
    assert!(matches!(
        &nodes[..],
        [solve::ComputeNode::ScalarPrograms(block)] if block.programs.len() == 4
    ));
}

#[test]
fn leaves_rows_scalar_when_structured_dae_bodies_do_not_match() -> Result<(), LowerError> {
    let mut rows: Vec<_> = (0..3).map(|offset| pointwise_row(offset, offset)).collect();
    let dae_equations = vec![
        residual_equation(der(var_ref("u", 1)), var_ref("w", 1)),
        residual_equation(der(var_ref("u", 2)), rhs_plus_one(var_ref("w", 2))),
        residual_equation(der(var_ref("u", 3)), var_ref("w", 3)),
    ];
    assert_eq!(
        tensor_decline_at(rows.as_slice(), &[family(0, 3)], &dae_equations)?,
        StructuredTensorDecline::MismatchedDaeBodyShape
    );
    let mut nodes = Vec::new();
    push_structured_programs_fixture(&mut nodes, &mut rows, &[family(0, 3)], &dae_equations);

    assert!(matches!(
        &nodes[..],
        [solve::ComputeNode::ScalarPrograms(block)] if block.programs.len() == 3
    ));
    Ok(())
}

#[test]
fn preserves_matching_body_shape_prefix_as_tensor_node() {
    let mut rows = (0..3).map(|offset| pointwise_row(offset, offset)).collect();
    let dae_equations = vec![
        residual_equation(der(var_ref("u", 1)), var_ref("w", 1)),
        residual_equation(der(var_ref("u", 2)), var_ref("w", 2)),
        residual_equation(der(var_ref("u", 3)), rhs_plus_one(var_ref("w", 3))),
    ];
    let mut nodes = Vec::new();
    push_structured_programs_fixture(&mut nodes, &mut rows, &[family(0, 3)], &dae_equations);

    assert!(matches!(
        &nodes[..],
        [
            solve::ComputeNode::Map { domain, .. },
            solve::ComputeNode::ScalarPrograms(block)
        ] if domain_scalar_count(domain) == 2 && block.programs.len() == 1
    ));
}

#[test]
fn treats_scalarized_literal_values_as_affine_tensor_strides() {
    let mut rows = (0..3)
        .map(|offset| scaled_row(offset, (offset + 1) as f64, offset))
        .collect();
    let dae_equations = vec![
        residual_equation(der(var_ref("u", 1)), rhs_times_integer(1, var_ref("u", 1))),
        residual_equation(der(var_ref("u", 2)), rhs_times_integer(2, var_ref("u", 2))),
        residual_equation(der(var_ref("u", 3)), rhs_times_integer(3, var_ref("u", 3))),
    ];
    let mut nodes = Vec::new();
    push_structured_programs_fixture(&mut nodes, &mut rows, &[family(0, 3)], &dae_equations);

    let [
        solve::ComputeNode::Map {
            domain,
            const_strides,
            ..
        },
    ] = &nodes[..]
    else {
        panic!("expected one affine Map");
    };
    assert_eq!(domain_scalar_count(domain), 3);
    assert_eq!(
        const_strides,
        &vec![solve::AffineStencilConstStride {
            op_position: 0,
            terms: vec![solve::AffineStencilConstStrideTerm {
                dimension: 0,
                stride: 1.0,
            }],
        }]
    );
}

#[test]
fn splits_non_affine_family_into_affine_subdomains() {
    let dae_equations = matching_dae_equations(4);
    let mut second = decay_row(1, 0, 1);
    second
        .ops
        .insert(0, solve::LinearOp::Const { dst: 9, value: 1.0 });
    let mut fourth = decay_row(9, 0, 3);
    fourth
        .ops
        .insert(0, solve::LinearOp::Const { dst: 9, value: 2.0 });
    let mut rows = vec![decay_row(0, 0, 0), second, decay_row(4, 0, 2), fourth];
    let mut nodes = Vec::new();
    push_structured_programs_fixture(&mut nodes, &mut rows, &[family(0, 4)], &dae_equations);
    assert!(matches!(
        &nodes[..],
        [
            solve::ComputeNode::Map { domain: first, .. },
            solve::ComputeNode::Map { domain: second, .. },
        ] if domain_scalar_count(first) == 2 && domain_scalar_count(second) == 2
    ));
}

#[test]
fn preserves_fixed_equation_position_in_multi_equation_loop() {
    let dae_equations = matching_dae_equations(12);
    let mut rows = (0..4)
        .map(|offset| decay_row(offset, 0, offset * 3 + 1))
        .collect();
    let mut nodes = Vec::new();
    push_structured_programs_fixture(
        &mut nodes,
        &mut rows,
        &[multi_equation_family(0, 4, 3)],
        &dae_equations,
    );
    assert!(matches!(
        &nodes[..],
        [solve::ComputeNode::Map { domain, .. }] if domain_scalar_count(domain) == 4
    ));
}

#[test]
fn preserves_source_subdomain_after_non_affine_boundary_iteration() {
    let dae_equations = matching_dae_equations(4);
    let mut boundary = decay_row(0, 0, 0);
    boundary
        .ops
        .insert(0, solve::LinearOp::Const { dst: 9, value: 1.0 });
    boundary.access_proof = None;
    let mut rows = vec![boundary];
    rows.extend((1..4).map(|offset| decay_row(offset, 0, offset)));
    let mut nodes = Vec::new();
    push_structured_programs_fixture(&mut nodes, &mut rows, &[family(0, 4)], &dae_equations);
    assert!(matches!(
        &nodes[..],
        [
            solve::ComputeNode::ScalarPrograms(block),
            solve::ComputeNode::Map { domain, .. }
        ] if block.programs.len() == 1
            && domain_scalar_count(domain) == 3
            && domain
                .index_tuples()
                .expect("fixture domain should enumerate index tuples")
                .first()
                .map(Vec::as_slice)
                == Some(&[2])
    ));
}

#[test]
fn ordinal_delta_rejects_i64_underflow() -> Result<(), LowerError> {
    let domain = test_domain(2);

    let err = ordinal_delta_with_span(
        0,
        &domain,
        &[i64::MAX],
        &[i64::MIN],
        unspanned_stencil_test_span(),
    )
    .expect_err("overflowing ordinal delta must fail");
    assert_eq!(err.source_span(), None);
    assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
    Ok(())
}

#[test]
fn apply_integer_terms_rejects_host_overflow() -> Result<(), LowerError> {
    let domain = test_domain(2);

    let err = apply_integer_terms_with_span(
        usize::MAX,
        &[1],
        &domain,
        &[1],
        &[2],
        unspanned_stencil_test_span(),
    )
    .expect_err("oversized affine index base must fail");
    assert_eq!(err.source_span(), None);
    assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
    Ok(())
}

#[test]
fn apply_float_terms_rejects_non_finite_result() -> Result<(), LowerError> {
    let domain = test_domain(2);

    let err = apply_float_terms_with_span(
        f64::MAX,
        &[f64::MAX],
        &domain,
        &[1],
        &[2],
        unspanned_stencil_test_span(),
    )
    .expect_err("non-finite affine float term must fail");
    assert_eq!(err.source_span(), None);
    assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
    Ok(())
}

#[test]
fn corner_rows_reproduce_full_row_affine_strides_for_2d_family() {
    // A 3x3 row-major family: cell (i, j) lives at Y-index (i-1)*3 + (j-1), so a
    // unit step of binder i moves the loaded index by 3 (cols) and a unit step of
    // binder j by 1. Each row loads its cell and the next (stencil_row: y, y+1).
    let domain = test_domain_2d(3, 3);
    let count = domain_scalar_count(&domain);
    let rows: Vec<StructuredProgram> = (0..count).map(|y| stencil_row(y, y)).collect();
    let row_indices: Vec<usize> = (0..count).collect();
    let span = stencil_test_span();

    let full = affine_strides_from_access_proofs(&rows, &row_indices, &domain, span)
        .expect("full-row strides should compute")
        .expect("full-row strides should be present");
    let corner = affine_strides_from_corner_rows(&rows, &row_indices, &domain, span)
        .expect("corner-row strides should compute")
        .expect("corner-row strides should be present");

    // The keystone: only the base + 2 neighbor rows are consulted, yet the strides
    // are identical to diffing all 9.
    assert_eq!(corner, full);
    // ...and they are the expected [i->3, j->1] stencil strides.
    assert_eq!(full.load_strides.len(), 2);
    for load in &full.load_strides {
        let dense: Vec<(usize, isize)> =
            load.terms.iter().map(|t| (t.dimension, t.stride)).collect();
        assert_eq!(dense, vec![(0, 3), (1, 1)]);
    }

    // The output map -- the other row-derived part of the node -- is likewise
    // reproduced from the corner rows, so the ENTIRE node is corner-derivable.
    let full_output = output_map_for_rows(&rows, &row_indices, &domain, span)
        .expect("full-row output map should compute")
        .expect("full-row output map should be present");
    let corner_output = output_map_from_corner_rows(&rows, &row_indices, &domain, span)
        .expect("corner-row output map should compute")
        .expect("corner-row output map should be present");
    assert_eq!(corner_output, full_output);
    assert_eq!(full_output.start, 0);
}

#[test]
fn corner_rows_support_singleton_dimensions() {
    // With j pinned to a single value its stride is irrelevant. The compact corner
    // selection needs only the base and the i-neighbor.
    let domain = test_domain_2d(3, 1);
    let count = domain_scalar_count(&domain);
    let rows: Vec<StructuredProgram> = (0..count).map(|y| stencil_row(y, y)).collect();
    let row_indices: Vec<usize> = (0..count).collect();
    let span = stencil_test_span();

    let corner = affine_strides_from_corner_rows(&rows, &row_indices, &domain, span)
        .expect("corner-row strides should compute");
    let full = affine_strides_from_access_proofs(&rows, &row_indices, &domain, span)
        .expect("full-row strides should compute");
    assert_eq!(corner, full);
}

#[test]
fn affine_strides_for_family_uses_corners_with_bounded_full_scan_oracle() {
    let span = stencil_test_span();

    // 3x3: corners succeed, so the production wrapper returns the corner result,
    // which equals the full-row scan.
    let domain = test_domain_2d(3, 3);
    let count = domain_scalar_count(&domain);
    let rows: Vec<StructuredProgram> = (0..count).map(|y| stencil_row(y, y)).collect();
    let row_indices: Vec<usize> = (0..count).collect();
    let full = affine_strides_from_access_proofs(&rows, &row_indices, &domain, span)
        .expect("full-row strides should compute");
    assert!(full.is_some(), "3x3 family lowers as a stencil");
    assert_eq!(
        affine_strides_for_family(&rows, &row_indices, &domain, span, true)
            .expect("family strides should compute"),
        full,
        "with corners available the wrapper returns the corner result (== full)"
    );

    // 3x1: the singleton j dimension needs no neighbor, so corners still reproduce
    // the full-row result.
    let domain = test_domain_2d(3, 1);
    let count = domain_scalar_count(&domain);
    let rows: Vec<StructuredProgram> = (0..count).map(|y| stencil_row(y, y)).collect();
    let row_indices: Vec<usize> = (0..count).collect();
    let corner = affine_strides_from_corner_rows(&rows, &row_indices, &domain, span)
        .expect("corner-row strides should compute");
    assert!(
        corner.is_some(),
        "singleton dimensions stay corner-derivable"
    );
    assert_eq!(
        affine_strides_for_family(&rows, &row_indices, &domain, span, true)
            .expect("family strides should compute"),
        affine_strides_from_access_proofs(&rows, &row_indices, &domain, span)
            .expect("full-row strides should compute"),
        "corner inference matches the bounded full-row oracle"
    );
}

#[test]
fn corner_rows_reproduce_full_row_node_for_3d_family() {
    // 3x3x3 row-major family: cell (i, j, k) lives at flat Y-index i*9 + j*3 + k, so
    // the corner cells are the base plus ONE neighbor per binder -- 4 cells, not 27.
    // This is the dimension-general claim: an N-D regular family is rebuilt from
    // N+1 corners regardless of grid size, so a 3-D PDE stencil works exactly as the
    // 2-D one does (and skips even more interior cells).
    let domain = test_domain_3d(3, 3, 3);
    let count = domain_scalar_count(&domain);
    assert_eq!(count, 27);
    let rows: Vec<StructuredProgram> = (0..count).map(|y| stencil_row(y, y)).collect();
    let row_indices: Vec<usize> = (0..count).collect();
    let span = stencil_test_span();

    let full = affine_strides_from_access_proofs(&rows, &row_indices, &domain, span)
        .expect("full-row strides should compute")
        .expect("full-row strides should be present");
    let corner = affine_strides_from_corner_rows(&rows, &row_indices, &domain, span)
        .expect("corner-row strides should compute")
        .expect("corner-row strides should be present");
    assert_eq!(
        corner, full,
        "4 corners reproduce the strides of all 27 cells"
    );

    // Each load carries the row-major stencil strides for all three binders:
    // i -> 9 (rows*cols), j -> 3 (cols), k -> 1 (contiguous).
    assert_eq!(full.load_strides.len(), 2);
    for load in &full.load_strides {
        let dense: Vec<(usize, isize)> =
            load.terms.iter().map(|t| (t.dimension, t.stride)).collect();
        assert_eq!(dense, vec![(0, 9), (1, 3), (2, 1)]);
    }

    // The output map -- the other row-derived part of the node -- is corner-derivable
    // in 3-D too, so the ENTIRE node is reconstructible from the 4 corners.
    let full_output = output_map_for_rows(&rows, &row_indices, &domain, span)
        .expect("full-row output map should compute")
        .expect("full-row output map should be present");
    let corner_output = output_map_from_corner_rows(&rows, &row_indices, &domain, span)
        .expect("corner-row output map should compute")
        .expect("corner-row output map should be present");
    assert_eq!(corner_output, full_output);

    // And the production wrapper returns the corner result for the 3-D family.
    assert_eq!(
        affine_strides_for_family(&rows, &row_indices, &domain, span, true)
            .expect("family strides should compute"),
        Some(full),
    );
}
