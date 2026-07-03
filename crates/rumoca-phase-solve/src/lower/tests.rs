// SPEC_0021 file-size exception: solve-lower tests still share setup helpers
// across expression, residual, derivative, and event rows. split plan: move
// behavior families into focused test modules with shared fixtures.

use super::{
    LowerBuilder, Scope, expression_rows::lower_expression_rows_with_mode, lower_derivative_rhs,
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
    BinaryOp, CompareOp, ComputeNode, ExternalFunctionKind, LinearOp, Reg, ScalarSlot, UnaryOp,
    VarLayout,
};
use std::sync::Arc;

mod array_operator_tests;
mod discrete_expression_tests;
mod event_intrinsics;
mod explicit_residual_tests;
mod function_expression_tests;
mod function_loop_tests;
mod intrinsics;
mod projection_derivative_tests;
mod projection_runtime_tests;
mod root_condition_tests;

fn lower_test_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("lower_test_fixture.mo"),
        0,
        1,
    )
}

fn unspanned_test_span() -> rumoca_core::Span {
    rumoca_core::Span::DUMMY
}

fn scalar_program_block_fixture(
    block: &rumoca_ir_solve::ComputeBlock,
) -> rumoca_ir_solve::ScalarProgramBlock {
    rumoca_eval_solve::to_scalar_program_block(block)
        .expect("valid solve-lowering fixture should scalarize")
}

#[test]
fn lower_builder_try_pack_registers_rejects_overflow() {
    let layout = VarLayout::default();
    let functions = IndexMap::new();
    let mut builder = super::LowerBuilder::new(&layout, &functions);
    builder.next_reg = Reg::MAX;

    let err = builder
        .try_pack_registers(&[0], unspanned_test_span())
        .expect_err("register pack should reject allocation overflow");

    assert_eq!(err.source_span(), None);
    assert!(matches!(
        err,
        super::LowerError::UnspannedContractViolation { .. }
    ));
    assert!(err.reason().contains("Solve register allocation overflow"));
}

#[test]
fn lower_expression_emits_energyplus_initialize_external_call() {
    let span = lower_test_span();
    let mut functions = IndexMap::new();
    let mut initialize = rumoca_core::Function::new(
        "Buildings.ThermalZones.EnergyPlus_9_6_0.BaseClasses.initialize",
        span,
    );
    initialize.external = Some(rumoca_core::ExternalFunction::default());
    initialize.add_input(rumoca_core::FunctionParam::new(
        "isSynchronized",
        "Real",
        span,
    ));
    initialize
        .outputs
        .push(rumoca_core::FunctionParam::new("nObj", "Integer", span));
    functions.insert(
        rumoca_core::VarName::new("Buildings.ThermalZones.EnergyPlus_9_6_0.BaseClasses.initialize"),
        initialize,
    );
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new(
            "Buildings.ThermalZones.EnergyPlus_9_6_0.BaseClasses.initialize",
        )
        .into(),
        args: vec![rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("__rumoca_named_arg__.isSynchronized").into(),
            args: vec![rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(1.0),
                span,
            }],
            is_constructor: true,
            span,
        }],
        is_constructor: false,
        span,
    };

    let lowered = lower_expression(&expr, &VarLayout::default(), &functions)
        .expect("supported EnergyPlus external initialize should lower");
    assert!(matches!(
        lowered.ops.as_slice(),
        [
            LinearOp::Const { .. },
            LinearOp::ExternalCall {
                function: ExternalFunctionKind::BuildingsEnergyPlusInitialize,
                arg_count: 1,
                output_index: 0,
                ..
            }
        ]
    ));
}

#[test]
fn dynamic_layout_entries_rejects_missing_source_binding_group_with_span() {
    let layout = VarLayout::default();
    let functions = IndexMap::new();
    let builder = super::LowerBuilder::new(&layout, &functions);
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("missing_indexed_group.mo"),
        12,
        20,
    );
    let component_ref = rumoca_core::ComponentReference::from_flat_segments(
        "arr",
        span,
        Some(rumoca_core::DefId::new(42)),
    );
    let source_key =
        rumoca_ir_solve::ComponentReferenceKey::from_component_reference(&component_ref)
            .expect("test component reference should be static");
    let target = super::DynamicBindingTarget::field("arr", Some(source_key), Some(span));

    let err = builder
        .dynamic_layout_entries(&target, Some(span))
        .expect_err("resolved source metadata without an indexed group must fail");

    assert_eq!(err.source_span(), Some(span));
    assert_eq!(
        err.reason(),
        "invalid IR contract: indexed solve-layout lookup for `arr` resolved source metadata but no indexed binding group"
    );
}

#[test]
fn lower_record_constructor_values_rejects_missing_required_field_with_input_span() {
    let source_id = rumoca_core::SourceId::from_source_name("missing_record_field.mo");
    let function_span = rumoca_core::Span::from_offsets(source_id, 0, 32);
    let field_span = rumoca_core::Span::from_offsets(source_id, 18, 19);
    let mut constructor = rumoca_core::Function::new("Pkg.RequiredRecord", function_span);
    constructor.is_constructor = true;
    constructor
        .inputs
        .push(rumoca_core::FunctionParam::new("x", "Real", field_span).with_span(field_span));

    let mut functions = IndexMap::new();
    functions.insert(constructor.name.clone(), constructor);
    let layout = rumoca_ir_solve::VarLayout::default();
    let mut builder = super::LowerBuilder::new(&layout, &functions);
    let name = rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
        "Pkg.RequiredRecord",
    ));

    let result =
        builder.lower_record_constructor_values(&name, &[], Some(function_span), &Scope::new(), 0);
    let Err(err) = result else {
        panic!("missing required record constructor field should fail");
    };

    assert_eq!(err.source_span(), Some(field_span));
    assert!(builder.ops.is_empty());
    match &err {
        super::LowerError::MissingActualArgument {
            function,
            what,
            input,
            span,
        } => {
            assert_eq!(function, "Pkg.RequiredRecord");
            assert_eq!(*what, "record constructor field");
            assert_eq!(input, "x");
            assert_eq!(*span, field_span);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn random_result_rejects_register_allocation_overflow() {
    let layout = VarLayout::default();
    let functions = IndexMap::new();
    let mut builder = super::LowerBuilder::new(&layout, &functions);
    builder.next_reg = Reg::MAX;

    let err = builder
        .emit_random_result(
            rumoca_ir_solve::RandomGenerator::Xorshift1024Star,
            &[0],
            unspanned_test_span(),
        )
        .expect_err("random result should reject allocation overflow");

    assert_eq!(err.source_span(), None);
    assert!(matches!(
        err,
        super::LowerError::UnspannedContractViolation { .. }
    ));
    assert!(err.reason().contains("Solve register allocation overflow"));
}

#[test]
fn random_result_overflow_does_not_emit_partial_pack() {
    let layout = VarLayout::default();
    let functions = IndexMap::new();
    let mut builder = super::LowerBuilder::new(&layout, &functions);
    builder.next_reg = Reg::MAX - 1;

    let err = builder
        .emit_random_result(
            rumoca_ir_solve::RandomGenerator::Xorshift1024Star,
            &[0],
            unspanned_test_span(),
        )
        .expect_err("random result should preflight pack plus result allocation");

    assert_eq!(err.source_span(), None);
    assert!(matches!(
        err,
        super::LowerError::UnspannedContractViolation { .. }
    ));
    assert!(builder.ops.is_empty());
    assert_eq!(builder.next_reg, Reg::MAX - 1);
}

#[test]
fn slot_load_rejects_register_allocation_overflow_with_span() {
    let layout = VarLayout::default();
    let functions = IndexMap::new();
    let mut builder = super::LowerBuilder::new(&layout, &functions);
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_lower_tests_source_15.mo"),
        8,
        12,
    );
    builder.next_reg = Reg::MAX;

    let err = builder
        .emit_slot_load(ScalarSlot::Time, span)
        .expect_err("slot load should reject register overflow");

    assert_eq!(err.source_span(), Some(span));
    assert!(matches!(
        err,
        super::LowerError::ContractViolation { reason, span: actual }
            if actual == span && reason.contains("Solve register allocation overflow")
    ));
    assert!(builder.ops.is_empty());
}

#[test]
fn binding_key_load_rejects_register_allocation_overflow_with_selection_span() {
    let mut bindings = IndexMap::new();
    bindings.insert(
        "x[1]".to_string(),
        ScalarSlot::Y {
            index: 0,
            byte_offset: 0,
        },
    );
    let layout = VarLayout::from_parts(bindings, 1, 0);
    let functions = IndexMap::new();
    let mut builder = super::LowerBuilder::new(&layout, &functions);
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("binding_slice.mo"),
        4,
        8,
    );
    builder.next_reg = Reg::MAX;

    let err = builder
        .load_binding_keys(&["x[1]".to_string()], span)
        .expect_err("binding key load should reject register overflow");

    assert_eq!(err.source_span(), Some(span));
    assert!(matches!(
        err,
        super::LowerError::ContractViolation { reason, span: actual }
            if actual == span && reason.contains("Solve register allocation overflow")
    ));
    assert!(builder.ops.is_empty());
}

#[test]
fn slice_selections_reports_excess_subscripts_with_selection_span() {
    let layout = VarLayout::default();
    let functions = IndexMap::new();
    let builder = super::LowerBuilder::new(&layout, &functions);
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("slice_rank.mo"),
        11,
        16,
    );
    let subscripts = vec![
        rumoca_core::Subscript::colon(span),
        rumoca_core::Subscript::colon(span),
    ];

    let err = builder
        .slice_selections(&subscripts, &[3], span, &Scope::new())
        .expect_err("slice with too many subscripts must fail");

    assert_eq!(err.source_span(), Some(span));
    assert_eq!(
        err.reason(),
        "array slice has more subscripts than dimensions"
    );
}

#[test]
fn const_emit_at_rejects_register_allocation_overflow_with_span() {
    let layout = VarLayout::default();
    let functions = IndexMap::new();
    let mut builder = super::LowerBuilder::new(&layout, &functions);
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_lower_tests_source_16.mo"),
        2,
        7,
    );
    builder.next_reg = Reg::MAX;

    let err = builder
        .emit_const_at(1.0, span)
        .expect_err("fallible const emit should reject register overflow");

    assert_eq!(err.source_span(), Some(span));
    assert!(matches!(
        err,
        super::LowerError::ContractViolation { reason, span: actual }
            if actual == span && reason.contains("Solve register allocation overflow")
    ));
    assert!(builder.ops.is_empty());
}

#[test]
fn scalar_binary_rejects_register_allocation_overflow_with_span() {
    let layout = VarLayout::default();
    let functions = IndexMap::new();
    let mut builder = super::LowerBuilder::new(&layout, &functions);
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_lower_tests_source_25.mo"),
        9,
        13,
    );
    builder.next_reg = Reg::MAX;

    let err = builder
        .lower_binary(rumoca_core::OpBinary::Add, 0, 1, span)
        .expect_err("scalar binary lowering should reject register overflow");

    assert_eq!(err.source_span(), Some(span));
    assert!(matches!(
        err,
        super::LowerError::ContractViolation { reason, span: actual }
            if actual == span && reason.contains("Solve register allocation overflow")
    ));
    assert!(builder.ops.is_empty());
}

#[test]
fn scalar_unary_rejects_register_allocation_overflow_with_span() {
    let layout = VarLayout::default();
    let functions = IndexMap::new();
    let mut builder = super::LowerBuilder::new(&layout, &functions);
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_lower_tests_source_26.mo"),
        4,
        10,
    );
    builder.next_reg = Reg::MAX;

    let err = builder
        .lower_unary(rumoca_core::OpUnary::Minus, 0, span)
        .expect_err("scalar unary lowering should reject register overflow");

    assert_eq!(err.source_span(), Some(span));
    assert!(matches!(
        err,
        super::LowerError::ContractViolation { reason, span: actual }
            if actual == span && reason.contains("Solve register allocation overflow")
    ));
    assert!(builder.ops.is_empty());
}

#[test]
fn periodic_tick_rejects_register_allocation_overflow_with_span() {
    let layout = VarLayout::default();
    let functions = IndexMap::new();
    let mut builder = super::LowerBuilder::new(&layout, &functions);
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_lower_tests_source_17.mo"),
        4,
        11,
    );
    builder.next_reg = Reg::MAX;

    let err = builder
        .emit_periodic_tick(0, 1, span)
        .expect_err("periodic tick emit should reject register overflow");

    assert_eq!(err.source_span(), Some(span));
    assert!(matches!(
        err,
        super::LowerError::ContractViolation { reason, span: actual }
            if actual == span && reason.contains("Solve register allocation overflow")
    ));
    assert!(builder.ops.is_empty());
}

#[test]
fn select_emit_at_rejects_register_allocation_overflow_with_span() {
    let layout = VarLayout::default();
    let functions = IndexMap::new();
    let mut builder = super::LowerBuilder::new(&layout, &functions);
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_lower_tests_source_18.mo"),
        6,
        14,
    );
    builder.next_reg = Reg::MAX;

    let err = builder
        .emit_select_at(0, 1, 2, span)
        .expect_err("fallible select emit should reject register overflow");

    assert_eq!(err.source_span(), Some(span));
    assert!(matches!(
        err,
        super::LowerError::ContractViolation { reason, span: actual }
            if actual == span && reason.contains("Solve register allocation overflow")
    ));
    assert!(builder.ops.is_empty());
}

#[test]
fn size_builtin_rejects_missing_base_argument_with_span() {
    let layout = VarLayout::default();
    let functions = IndexMap::new();
    let mut builder = super::LowerBuilder::new(&layout, &functions);
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_lower_tests_source_21.mo"),
        4,
        10,
    );

    let err = builder
        .lower_size_builtin(&[], span, &Scope::new(), 0)
        .expect_err("size() without a base argument should fail");

    assert_eq!(err.source_span(), Some(span));
    assert!(matches!(
        err,
        super::LowerError::ContractViolation { reason, span: actual }
            if actual == span && reason == "size() requires at least 1 argument"
    ));
    assert!(builder.ops.is_empty());
}

#[test]
fn size_builtin_rejects_unspanned_base_without_fabricating_span() {
    let layout = VarLayout::default();
    let functions = IndexMap::new();
    let mut builder = super::LowerBuilder::new(&layout, &functions);
    let base = rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Integer(1),
        span: unspanned_test_span(),
    };

    let err = builder
        .lower_size_builtin(&[base], unspanned_test_span(), &Scope::new(), 0)
        .expect_err("unspanned size() base should fail before emitting code");

    assert_eq!(err.source_span(), None);
    assert!(matches!(
        err,
        super::LowerError::UnspannedContractViolation { reason }
            if reason == "size() base argument requires source span"
    ));
    assert!(builder.ops.is_empty());
}

#[test]
fn size_from_dims_rejects_register_allocation_overflow_with_span() {
    let layout = VarLayout::default();
    let functions = IndexMap::new();
    let mut builder = super::LowerBuilder::new(&layout, &functions);
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_lower_tests_source_19.mo"),
        3,
        9,
    );
    let base = rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Integer(1),
        span,
    };
    builder.next_reg = Reg::MAX;

    let err = builder
        .lower_size_from_dims(&[2], &[base], span, &Scope::new(), 0)
        .expect_err("size() selector setup should reject register overflow");

    assert_eq!(err.source_span(), Some(span));
    assert!(matches!(
        err,
        super::LowerError::ContractViolation { reason, span: actual }
            if actual == span && reason.contains("Solve register allocation overflow")
    ));
    assert!(builder.ops.is_empty());
}

#[test]
fn structural_index_selector_rejects_register_allocation_overflow_with_span() {
    let layout = VarLayout::default();
    let functions = IndexMap::new();
    let mut builder = super::LowerBuilder::new(&layout, &functions);
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_lower_tests_source_20.mo"),
        12,
        13,
    );
    let subscript = rumoca_core::Subscript::index(1, span);
    builder.next_reg = Reg::MAX;

    let err = builder
        .lower_structural_index_selector(&subscript, unspanned_test_span(), &Scope::new(), 0)
        .expect_err("structural index selector should reject register overflow");

    assert_eq!(err.source_span(), Some(span));
    assert!(matches!(
        err,
        super::LowerError::ContractViolation { reason, span: actual }
            if actual == span && reason.contains("Solve register allocation overflow")
    ));
    assert!(builder.ops.is_empty());
}

#[test]
fn structural_index_selector_uses_context_span_for_generated_subscript() {
    let layout = VarLayout::default();
    let functions = IndexMap::new();
    let mut builder = super::LowerBuilder::new(&layout, &functions);
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_lower_tests_source_20.mo"),
        18,
        27,
    );
    let subscript = rumoca_core::Subscript::generated_index(1, unspanned_test_span());
    builder.next_reg = Reg::MAX;

    let err = builder
        .lower_structural_index_selector(&subscript, span, &Scope::new(), 0)
        .expect_err("generated structural index selector should reject register overflow");

    assert_eq!(err.source_span(), Some(span));
    assert!(matches!(
        err,
        super::LowerError::ContractViolation { reason, span: actual }
            if actual == span && reason.contains("Solve register allocation overflow")
    ));
    assert!(builder.ops.is_empty());
}

#[test]
fn standard_normal_quantile_rejects_register_allocation_overflow_with_span() {
    let layout = VarLayout::default();
    let functions = IndexMap::new();
    let mut builder = super::LowerBuilder::new(&layout, &functions);
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_lower_tests_source_21.mo"),
        5,
        18,
    );
    builder.next_reg = Reg::MAX;

    let err = builder
        .emit_standard_normal_quantile(0, span)
        .expect_err("standard normal quantile should reject register overflow");

    assert_eq!(err.source_span(), Some(span));
    assert!(matches!(
        err,
        super::LowerError::ContractViolation { reason, span: actual }
            if actual == span && reason.contains("Solve register allocation overflow")
    ));
    assert!(builder.ops.is_empty());
}

#[test]
fn raw_real_fft_rejects_register_allocation_overflow_with_span() {
    let layout = VarLayout::default();
    let functions = IndexMap::new();
    let mut builder = super::LowerBuilder::new(&layout, &functions);
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_lower_tests_source_22.mo"),
        1,
        19,
    );
    builder.next_reg = Reg::MAX;

    let err = builder
        .lower_raw_real_fft_values(&[0, 1], false, span)
        .expect_err("raw real FFT lowering should reject register overflow");

    assert_eq!(err.source_span(), Some(span));
    assert!(matches!(
        err,
        super::LowerError::ContractViolation { reason, span: actual }
            if actual == span && reason.contains("Solve register allocation overflow")
    ));
    assert!(builder.ops.is_empty());
}

#[test]
fn array_builtin_zero_rejects_register_allocation_overflow_with_span() {
    let layout = VarLayout::default();
    let functions = IndexMap::new();
    let mut builder = super::LowerBuilder::new(&layout, &functions);
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_lower_tests_source_23.mo"),
        7,
        8,
    );
    let expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Zeros,
        args: vec![rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(1),
            span,
        }],
        span,
    };
    builder.next_reg = Reg::MAX;

    let err = builder
        .lower_array_like_values(&expr, &Scope::new(), 0)
        .expect_err("zeros() array lowering should reject register overflow");

    assert_eq!(err.source_span(), Some(span));
    assert!(matches!(
        err,
        super::LowerError::ContractViolation { reason, span: actual }
            if actual == span && reason.contains("Solve register allocation overflow")
    ));
    assert!(builder.ops.is_empty());
}

#[test]
fn min_max_empty_rejects_register_allocation_overflow() {
    let layout = VarLayout::default();
    let functions = IndexMap::new();
    let mut builder = super::LowerBuilder::new(&layout, &functions);
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_lower_tests_source_31.mo"),
        2,
        6,
    );
    builder.next_reg = Reg::MAX;

    let err = builder
        .lower_min_max_builtin(
            &[],
            &Scope::new(),
            0,
            span,
            BinaryOp::Max,
            f64::NEG_INFINITY,
        )
        .expect_err("empty min/max identity should reject register overflow");

    assert_eq!(err.source_span(), Some(span));
    assert!(matches!(
        err,
        super::LowerError::ContractViolation {
            span: actual,
            ..
        } if actual == span
    ));
    assert!(err.reason().contains("Solve register allocation overflow"));
    assert!(builder.ops.is_empty());
}

#[test]
fn subscript_match_rejects_register_allocation_overflow_with_span() {
    let layout = VarLayout::default();
    let functions = IndexMap::new();
    let mut builder = super::LowerBuilder::new(&layout, &functions);
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_lower_tests_source_24.mo"),
        2,
        6,
    );
    builder.next_reg = Reg::MAX;

    let err = builder
        .emit_subscript_match_at(&[0], &[1], span)
        .expect_err("subscript match should reject register overflow");

    assert_eq!(err.source_span(), Some(span));
    assert!(matches!(
        err,
        super::LowerError::ContractViolation { reason, span: actual }
            if actual == span && reason.contains("Solve register allocation overflow")
    ));
    assert!(builder.ops.is_empty());
}

#[test]
fn assignment_control_guard_rejects_register_allocation_overflow_with_span() {
    let layout = VarLayout::default();
    let functions = IndexMap::new();
    let mut builder = super::LowerBuilder::new(&layout, &functions);
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_lower_tests_source_27.mo"),
        11,
        17,
    );
    let mut scope = Scope::new();
    scope.insert(super::generated_scope_key(super::RETURN_FLAG_BINDING), 0);
    scope.insert(super::generated_scope_key(super::BREAK_FLAG_BINDING), 1);
    scope.insert(super::generated_scope_key("x"), 2);
    builder.next_reg = Reg::MAX;

    let err = builder
        .guard_assignment_after_return(&scope, "x", vec![3], span)
        .expect_err("assignment guard should reject register overflow");

    assert_eq!(err.source_span(), Some(span));
    assert!(matches!(
        err,
        super::LowerError::ContractViolation { reason, span: actual }
            if actual == span && reason.contains("Solve register allocation overflow")
    ));
    assert!(builder.ops.is_empty());
}

fn scalar_var(name: &str) -> dae::Variable {
    dae::Variable {
        component_ref: Some(test_component_ref_from_name(name)),
        ..dae::Variable::new(
            rumoca_core::VarName::new(name),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        )
    }
}

fn source_scalar_var(name: &str) -> dae::Variable {
    let span = lower_test_span();
    dae::Variable {
        component_ref: Some(source_component_ref_from_name(name)),
        source_span: span,
        origin: dae::VariableOrigin::Source,
        ..dae::Variable::new(rumoca_core::VarName::new(name), span)
    }
}

fn source_array_var(name: &str, dims: &[i64]) -> dae::Variable {
    let span = lower_test_span();
    dae::Variable {
        component_ref: Some(source_component_ref_from_name(name)),
        source_span: span,
        origin: dae::VariableOrigin::Source,
        dims: dims.to_vec(),
        ..dae::Variable::new(rumoca_core::VarName::new(name), span)
    }
}

fn array_var(name: &str, dims: &[i64]) -> dae::Variable {
    dae::Variable {
        component_ref: Some(test_component_ref_from_name(name)),
        dims: dims.to_vec(),
        ..dae::Variable::new(
            rumoca_core::VarName::new(name),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        )
    }
}

fn test_component_ref_from_name(name: &str) -> rumoca_core::ComponentReference {
    crate::test_support::component_ref_from_name(name)
}

fn source_component_ref_from_name(name: &str) -> rumoca_core::ComponentReference {
    let span = lower_test_span();
    let mut component_ref = test_component_ref_from_name(name);
    component_ref.span = span;
    component_ref.def_id = Some(source_fixture_def_id(name));
    for part in &mut component_ref.parts {
        part.span = span;
        for subscript in &mut part.subs {
            match subscript {
                rumoca_core::Subscript::Index { span: sub_span, .. }
                | rumoca_core::Subscript::Colon { span: sub_span }
                | rumoca_core::Subscript::Expr { span: sub_span, .. } => *sub_span = span,
            }
        }
    }
    component_ref
}

fn source_ref(name: &str) -> rumoca_core::Reference {
    rumoca_core::Reference::from_component_reference(source_component_ref_from_name(name))
}

#[test]
fn compile_time_subscript_indices_use_structural_bindings() {
    let span = lower_test_span();
    let mut structural_bindings = IndexMap::new();
    structural_bindings.insert("nEle".to_string(), 4.0);
    let layout = VarLayout::default();
    let functions = IndexMap::new();
    let builder = LowerBuilder::new(&layout, &functions)
        .with_structural_bindings(Arc::new(structural_bindings));
    let subscripts = vec![rumoca_core::Subscript::generated_expr(
        Box::new(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("nEle"),
            subscripts: Vec::new(),
            span,
        }),
        span,
    )];

    let indices = builder
        .compile_time_subscript_indices(&subscripts, span)
        .expect("structural subscript folding should not fail")
        .expect("structural parameter subscript should fold");

    assert_eq!(indices, vec![4]);
}

#[test]
fn indexed_field_access_uses_structural_subscript_bindings() {
    let span = lower_test_span();
    let key = "ele[4].T";
    let layout = VarLayout::from_parts_with_shapes_spans_and_indexed_bindings(
        IndexMap::from([(key.to_string(), rumoca_ir_solve::scalar_slot_y(0))]),
        IndexMap::new(),
        IndexMap::new(),
        IndexMap::new(),
        1,
        0,
    )
    .expect("indexed field fixture layout should satisfy shape contract");
    let functions = IndexMap::new();
    let mut structural_bindings = IndexMap::new();
    structural_bindings.insert("nEle".to_string(), 4.0);
    let mut builder = LowerBuilder::new(&layout, &functions)
        .with_structural_bindings(Arc::new(structural_bindings));
    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::Index {
            base: Box::new(source_var("ele")),
            subscripts: vec![rumoca_core::Subscript::generated_expr(
                Box::new(rumoca_core::Expression::VarRef {
                    name: rumoca_core::Reference::new("nEle"),
                    subscripts: Vec::new(),
                    span,
                }),
                span,
            )],
            span,
        }),
        field: "T".to_string(),
        span,
    };

    let reg = builder
        .lower_expr(&expr, &Scope::new(), 0)
        .expect("structural indexed field access should lower");
    let (regs, _) = eval_linear_ops(&builder.ops, &[289.0], &[], 0.0);

    assert!((read_reg(&regs, reg) - 289.0).abs() < 1e-12);
}

#[test]
fn nested_indexed_field_access_uses_structural_subscript_bindings() {
    let span = lower_test_span();
    let key = "ele[4].vol2.T";
    let layout = VarLayout::from_parts_with_shapes_spans_and_indexed_bindings(
        IndexMap::from([(key.to_string(), rumoca_ir_solve::scalar_slot_y(0))]),
        IndexMap::new(),
        IndexMap::new(),
        IndexMap::new(),
        1,
        0,
    )
    .expect("nested indexed field fixture layout should satisfy shape contract");
    let functions = IndexMap::new();
    let mut structural_bindings = IndexMap::new();
    structural_bindings.insert("nEle".to_string(), 4.0);
    let mut builder = LowerBuilder::new(&layout, &functions)
        .with_structural_bindings(Arc::new(structural_bindings));
    let indexed_ele = rumoca_core::Expression::Index {
        base: Box::new(source_var("ele")),
        subscripts: vec![rumoca_core::Subscript::generated_expr(
            Box::new(rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::new("nEle"),
                subscripts: Vec::new(),
                span,
            }),
            span,
        )],
        span,
    };
    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::FieldAccess {
            base: Box::new(indexed_ele),
            field: "vol2".to_string(),
            span,
        }),
        field: "T".to_string(),
        span,
    };

    let reg = builder
        .lower_expr(&expr, &Scope::new(), 0)
        .expect("nested structural indexed field access should lower");
    let (regs, _) = eval_linear_ops(&builder.ops, &[291.0], &[], 0.0);

    assert!((read_reg(&regs, reg) - 291.0).abs() < 1e-12);
}

#[test]
fn array_comprehension_index_lowers_as_compile_time_subscript() {
    let span = lower_test_span();
    let layout = VarLayout::from_parts_with_shapes_spans_and_indexed_bindings(
        IndexMap::from([
            (
                "ele[1].vol2.mWat_flow".to_string(),
                rumoca_ir_solve::scalar_slot_y(0),
            ),
            (
                "ele[2].vol2.mWat_flow".to_string(),
                rumoca_ir_solve::scalar_slot_y(1),
            ),
            (
                "ele[3].vol2.mWat_flow".to_string(),
                rumoca_ir_solve::scalar_slot_y(2),
            ),
            (
                "ele[4].vol2.mWat_flow".to_string(),
                rumoca_ir_solve::scalar_slot_y(3),
            ),
        ]),
        IndexMap::new(),
        IndexMap::new(),
        IndexMap::new(),
        4,
        0,
    )
    .expect("comprehension fixture layout should satisfy shape contract");
    let functions = IndexMap::new();
    let mut builder = LowerBuilder::new(&layout, &functions);
    let indexed_ele = rumoca_core::Expression::Index {
        base: Box::new(source_var("ele")),
        subscripts: vec![rumoca_core::Subscript::generated_expr(
            Box::new(var("i")),
            span,
        )],
        span,
    };
    let member_expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::FieldAccess {
            base: Box::new(indexed_ele),
            field: "vol2".to_string(),
            span,
        }),
        field: "mWat_flow".to_string(),
        span,
    };
    let expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Sum,
        args: vec![rumoca_core::Expression::ArrayComprehension {
            expr: Box::new(member_expr),
            indices: vec![rumoca_core::ComprehensionIndex {
                name: "i".to_string(),
                range: rumoca_core::Expression::Range {
                    start: Box::new(int_lit(1)),
                    step: None,
                    end: Box::new(int_lit(4)),
                    span,
                },
            }],
            filter: None,
            span,
        }],
        span,
    };

    let reg = builder
        .lower_expr(&expr, &Scope::new(), 0)
        .expect("array comprehension index should lower as compile-time subscript");
    let (regs, _) = eval_linear_ops(&builder.ops, &[1.0, 2.0, 3.0, 4.0], &[], 0.0);

    assert!((read_reg(&regs, reg) - 10.0).abs() < 1e-12);
}

#[test]
fn scalar_field_access_reads_first_record_array_member_field() {
    let span = lower_test_span();
    let layout = VarLayout::from_parts_with_shapes_spans_and_indexed_bindings(
        IndexMap::from([(
            "zon[1].ports[1].p".to_string(),
            rumoca_ir_solve::scalar_slot_y(0),
        )]),
        IndexMap::new(),
        IndexMap::new(),
        IndexMap::new(),
        1,
        0,
    )
    .expect("record-array member fixture layout should satisfy shape contract");
    let functions = IndexMap::new();
    let mut builder = LowerBuilder::new(&layout, &functions);
    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::FieldAccess {
            base: Box::new(var("zon[1]")),
            field: "ports".to_string(),
            span,
        }),
        field: "p".to_string(),
        span,
    };

    let reg = builder
        .lower_expr(&expr, &Scope::new(), 0)
        .expect("scalar field access should read first record-array member");
    let (regs, _) = eval_linear_ops(&builder.ops, &[101325.0], &[], 0.0);

    assert!((read_reg(&regs, reg) - 101325.0).abs() < 1e-12);
}

#[test]
fn scalar_field_access_reads_indexed_source_record_array_member_without_def_id() {
    let span = lower_test_span();
    let layout = VarLayout::from_parts_with_shapes_spans_and_indexed_bindings(
        IndexMap::from([(
            "zon[1].ports[1].p".to_string(),
            rumoca_ir_solve::scalar_slot_y(0),
        )]),
        IndexMap::new(),
        IndexMap::new(),
        IndexMap::new(),
        1,
        0,
    )
    .expect("indexed source record-array fixture layout should satisfy shape contract");
    let functions = IndexMap::new();
    let mut builder = LowerBuilder::new(&layout, &functions);
    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::FieldAccess {
            base: Box::new(unresolved_source_var("zon[1]")),
            field: "ports".to_string(),
            span,
        }),
        field: "p".to_string(),
        span,
    };

    let reg = builder
        .lower_expr(&expr, &Scope::new(), 0)
        .expect("scalar field access should use scalarized layout before requiring def identity");
    let (regs, _) = eval_linear_ops(&builder.ops, &[101325.0], &[], 0.0);

    assert!((read_reg(&regs, reg) - 101325.0).abs() < 1e-12);
}

#[test]
fn size_infers_indexed_source_field_shape_without_def_id() {
    let span = lower_test_span();
    let layout = VarLayout::from_parts_with_shapes(
        IndexMap::from([(
            "zon[1].ports".to_string(),
            rumoca_ir_solve::scalar_slot_y(0),
        )]),
        IndexMap::from([("zon[1].ports".to_string(), vec![5])]),
        5,
        0,
    )
    .expect("indexed source field shape fixture layout should satisfy shape contract");
    let functions = IndexMap::new();
    let mut builder = LowerBuilder::new(&layout, &functions);
    let expr = size_expr(
        rumoca_core::Expression::FieldAccess {
            base: Box::new(unresolved_source_var("zon[1]")),
            field: "ports".to_string(),
            span,
        },
        1,
    );

    let reg = builder
        .lower_expr(&expr, &Scope::new(), 0)
        .expect("size() should infer display-keyed field shape before requiring def identity");
    let (regs, _) = eval_linear_ops(&builder.ops, &[], &[], 0.0);

    assert!((read_reg(&regs, reg) - 5.0).abs() < 1e-12);
}

#[test]
fn size_of_unresolved_indexed_source_ref_does_not_require_def_id() {
    let layout = VarLayout::default();
    let functions = IndexMap::new();
    let mut builder = LowerBuilder::new(&layout, &functions);
    let expr = size_expr(unresolved_source_var("zon[1]"), 1);

    let reg = builder
        .lower_expr(&expr, &Scope::new(), 0)
        .expect("size() fallback should not require missing source def identity");
    let (regs, _) = eval_linear_ops(&builder.ops, &[], &[], 0.0);

    assert!((read_reg(&regs, reg) - 1.0).abs() < 1e-12);
}

#[test]
fn size_of_stream_passthrough_uses_argument_shape() {
    let layout = VarLayout::from_parts_with_shapes(
        IndexMap::from([("x".to_string(), rumoca_ir_solve::scalar_slot_y(0))]),
        IndexMap::from([("x".to_string(), vec![3])]),
        3,
        0,
    )
    .expect("stream passthrough size fixture layout should satisfy shape contract");
    let functions = IndexMap::new();
    let mut builder = LowerBuilder::new(&layout, &functions);
    let expr = size_expr(stream_passthrough_expr("inStream", var("x")), 1);

    let reg = builder
        .lower_expr(&expr, &Scope::new(), 0)
        .expect("size() should use stream passthrough argument shape");
    let (regs, _) = eval_linear_ops(&builder.ops, &[], &[], 0.0);

    assert!((read_reg(&regs, reg) - 3.0).abs() < 1e-12);
}

#[test]
fn index_over_stream_passthrough_uses_argument_bindings() {
    let span = lower_test_span();
    let layout = VarLayout::from_parts_with_shapes(
        IndexMap::from([("x".to_string(), rumoca_ir_solve::scalar_slot_y(0))]),
        IndexMap::from([("x".to_string(), vec![3])]),
        3,
        0,
    )
    .expect("stream passthrough index fixture layout should satisfy shape contract");
    let functions = IndexMap::new();
    let mut builder = LowerBuilder::new(&layout, &functions);
    let expr = rumoca_core::Expression::Index {
        base: Box::new(stream_passthrough_expr("inStream", var("x"))),
        subscripts: vec![rumoca_core::Subscript::generated_index(2, span)],
        span,
    };

    let reg = builder
        .lower_expr(&expr, &Scope::new(), 0)
        .expect("index should use stream passthrough argument binding");
    let (regs, _) = eval_linear_ops(&builder.ops, &[10.0, 20.0, 30.0], &[], 0.0);

    assert!((read_reg(&regs, reg) - 20.0).abs() < 1e-12);
}

#[test]
fn zero_length_function_input_accepts_unknown_rank_empty_actual() {
    let span = lower_test_span();
    let mut function = rumoca_core::Function::new("Pkg.zeroLength", span);
    function.inputs.push(function_param_with_dims("X", &[0]));
    function.outputs.push(function_param("y"));
    function.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("y"),
        value: real_lit(1.0),
        span,
    });
    let mut functions = IndexMap::new();
    functions.insert(function.name.clone(), function);
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("Pkg.zeroLength").into(),
        args: vec![stream_passthrough_expr("inStream", var("missingXi"))],
        is_constructor: false,
        span,
    };

    let lowered = lower_expression(&expr, &VarLayout::default(), &functions)
        .expect("zero-length formal should bind unknown-rank empty actual as an empty array");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);

    assert!((read_reg(&regs, lowered.result) - 1.0).abs() < 1e-12);
}

#[test]
fn stream_passthrough_zero_shape_lowers_to_empty_array_values() {
    let layout = VarLayout::from_parts_with_shapes(
        IndexMap::new(),
        IndexMap::from([("x".to_string(), vec![0])]),
        0,
        0,
    )
    .expect("zero-size stream argument fixture layout should satisfy shape contract");
    let functions = IndexMap::new();
    let mut builder = LowerBuilder::new(&layout, &functions);
    let values = builder
        .lower_array_like_values(
            &stream_passthrough_expr("inStream", var("x")),
            &Scope::new(),
            0,
        )
        .expect("zero-size stream passthrough should lower to empty values");

    assert!(values.is_empty());
}

#[test]
fn multiply_zero_shape_stream_passthrough_lowers_to_empty_array_values() {
    let span = lower_test_span();
    let layout = VarLayout::from_parts_with_shapes(
        IndexMap::new(),
        IndexMap::from([("x".to_string(), vec![0])]),
        0,
        0,
    )
    .expect("zero-size stream multiplication fixture layout should satisfy shape contract");
    let functions = IndexMap::new();
    let mut builder = LowerBuilder::new(&layout, &functions);
    let expr = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Mul,
        lhs: Box::new(stream_passthrough_expr("inStream", var("x"))),
        rhs: Box::new(real_lit(2.0)),
        span,
    };
    let values = builder
        .lower_array_like_values(&expr, &Scope::new(), 0)
        .expect("zero-size stream multiplication should lower to empty values");

    assert!(values.is_empty());
}

#[test]
fn elementwise_zero_shape_stream_passthrough_lowers_to_empty_array_values() {
    let span = lower_test_span();
    let layout = VarLayout::from_parts_with_shapes(
        IndexMap::new(),
        IndexMap::from([("x".to_string(), vec![0])]),
        0,
        0,
    )
    .expect("zero-size stream elementwise fixture layout should satisfy shape contract");
    let functions = IndexMap::new();
    let mut builder = LowerBuilder::new(&layout, &functions);
    let expr = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Add,
        lhs: Box::new(stream_passthrough_expr("inStream", var("x"))),
        rhs: Box::new(real_lit(2.0)),
        span,
    };
    let values = builder
        .lower_array_like_values(&expr, &Scope::new(), 0)
        .expect("zero-size stream elementwise operation should lower to empty values");

    assert!(values.is_empty());
}

#[test]
fn scalar_left_elementwise_zero_shape_stream_passthrough_lowers_to_empty_array_values() {
    let span = lower_test_span();
    let layout = VarLayout::from_parts_with_shapes(
        IndexMap::new(),
        IndexMap::from([("x".to_string(), vec![0])]),
        0,
        0,
    )
    .expect("zero-size stream elementwise fixture layout should satisfy shape contract");
    let functions = IndexMap::new();
    let mut builder = LowerBuilder::new(&layout, &functions);
    let expr = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Add,
        lhs: Box::new(real_lit(2.0)),
        rhs: Box::new(stream_passthrough_expr("inStream", var("x"))),
        span,
    };
    let values = builder
        .lower_array_like_values(&expr, &Scope::new(), 0)
        .expect("scalar plus zero-size stream elementwise operation should lower to empty values");

    assert!(values.is_empty());
}

#[test]
fn divide_zero_shape_stream_passthrough_lowers_to_empty_array_values() {
    let span = lower_test_span();
    let layout = VarLayout::from_parts_with_shapes(
        IndexMap::new(),
        IndexMap::from([("x".to_string(), vec![0]), ("y".to_string(), vec![0])]),
        0,
        0,
    )
    .expect("zero-size stream division fixture layout should satisfy shape contract");
    let functions = IndexMap::new();
    let mut builder = LowerBuilder::new(&layout, &functions);
    let expr = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Div,
        lhs: Box::new(stream_passthrough_expr("inStream", var("x"))),
        rhs: Box::new(stream_passthrough_expr("inStream", var("y"))),
        span,
    };
    let values = builder
        .lower_array_like_values(&expr, &Scope::new(), 0)
        .expect("zero-size stream division should lower to empty values");

    assert!(values.is_empty());
}

fn source_fixture_def_id(name: &str) -> rumoca_core::DefId {
    let hash = name.bytes().fold(2_166_136_261_u32, |hash, byte| {
        hash.wrapping_mul(16_777_619) ^ u32::from(byte)
    });
    rumoca_core::DefId::new(hash.max(1))
}

fn insert_pre_parameter(dae_model: &mut dae::Dae, name: &str, dims: &[i64]) {
    let pre_name = format!("__pre__.{name}");
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new(&pre_name),
        dae::Variable {
            dims: dims.to_vec(),
            fixed: Some(true),
            state_select: rumoca_core::StateSelect::Default,
            is_tunable: false,
            ..dae::Variable::new(rumoca_core::VarName::new(&pre_name), lower_test_span())
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

/// Evaluate a single program and return its register file plus the value of its
/// last `StoreOutput` (historical single-output behavior, used by most tests).
fn eval_linear_ops(ops: &[LinearOp], y: &[f64], p: &[f64], t: f64) -> (Vec<f64>, Option<f64>) {
    let (regs, outputs) = eval_linear_ops_collect(ops, y, p, t);
    (regs, outputs.last().copied())
}

/// Evaluate output `output_index` of a (possibly multi-output) scalar program
/// block, regardless of which program emits it. Replaces the old
/// `eval_linear_ops(&block.programs[output_index], ..)` now that matmul/linsolve
/// nodes lower to a single multi-`StoreOutput` program.
fn eval_block_output(
    block: &rumoca_ir_solve::ScalarProgramBlock,
    output_index: usize,
    y: &[f64],
    p: &[f64],
    t: f64,
) -> (Vec<f64>, Option<f64>) {
    let program_index = block
        .program_index_for_output(output_index)
        .expect("output index within block");
    let prior: usize = block.programs[..program_index]
        .iter()
        .map(|program| rumoca_ir_solve::ScalarProgramBlock::program_output_count(program))
        .sum();
    let (regs, outputs) = eval_linear_ops_collect(&block.programs[program_index], y, p, t);
    (regs, outputs.get(output_index - prior).copied())
}

/// Evaluate every output of a (possibly multi-output) scalar program block, in
/// output order. Replaces the old `block.programs.iter().map(|row|
/// eval_linear_ops(row, ..).1)` idiom now that one program may emit several
/// outputs.
fn eval_block_all_outputs(
    block: &rumoca_ir_solve::ScalarProgramBlock,
    y: &[f64],
    p: &[f64],
    t: f64,
) -> Vec<f64> {
    eval_programs_all_outputs(&block.programs, y, p, t)
}

/// Same as [`eval_block_all_outputs`] for callers that hold the raw program list
/// (`Vec<Vec<LinearOp>>`) returned by `lower_residual` / `lower_discrete_rhs`.
fn eval_programs_all_outputs(programs: &[Vec<LinearOp>], y: &[f64], p: &[f64], t: f64) -> Vec<f64> {
    programs
        .iter()
        .flat_map(|program| eval_linear_ops_collect(program, y, p, t).1)
        .collect()
}

/// Evaluate a single program, returning its register file and every
/// `StoreOutput` value in order.
fn eval_linear_ops_collect(ops: &[LinearOp], y: &[f64], p: &[f64], t: f64) -> (Vec<f64>, Vec<f64>) {
    let mut regs = Vec::new();
    let mut outputs = Vec::new();
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
            LinearOp::LoadIndexedP {
                dst,
                base,
                count,
                index,
            } => {
                let slot =
                    rumoca_ir_solve::resolve_indexed_slot(read_reg(&regs, index), base, count);
                write_reg(&mut regs, dst, p.get(slot).copied().unwrap_or(0.0));
            }
            LinearOp::LoadSeed { dst, .. } => write_reg(&mut regs, dst, 0.0),
            LinearOp::LoadIndexedSeed { dst, .. } => write_reg(&mut regs, dst, 0.0),
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
            LinearOp::ExternalCall { dst, .. } => write_reg(&mut regs, dst, 0.0),
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
                outputs.push(read_reg(&regs, src));
            }
        }
    }
    (regs, outputs)
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
    test_component_ref_from_name(name)
}

fn component_ref_index(name: &str, index: i64) -> rumoca_core::ComponentReference {
    let span = lower_test_span();
    let mut component_ref = source_component_ref_from_name(name);
    component_ref
        .parts
        .last_mut()
        .unwrap()
        .subs
        .push(rumoca_core::Subscript::generated_index(index, span));
    component_ref
}

fn component_ref_indices(name: &str, indices: &[i64]) -> rumoca_core::ComponentReference {
    let span = lower_test_span();
    let mut component_ref = source_component_ref_from_name(name);
    component_ref.parts.last_mut().unwrap().subs.extend(
        indices
            .iter()
            .copied()
            .map(|index| rumoca_core::Subscript::generated_index(index, span)),
    );
    component_ref
}

fn component_ref_index_expr(
    name: &str,
    index: rumoca_core::Expression,
) -> rumoca_core::ComponentReference {
    let span = index.span().unwrap_or_else(lower_test_span);
    let mut component_ref = source_component_ref_from_name(name);
    component_ref
        .parts
        .last_mut()
        .unwrap()
        .subs
        .push(rumoca_core::Subscript::generated_expr(
            Box::new(index),
            span,
        ));
    component_ref
}

fn function_param(name: &str) -> rumoca_core::FunctionParam {
    rumoca_core::FunctionParam {
        def_id: None,
        name: name.to_string(),
        span: lower_test_span(),
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
        span: lower_test_span(),
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
        name: rumoca_core::Reference::generated(name),
        subscripts: vec![],
        span: unspanned_test_span(),
    }
}

fn source_var(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::from_component_reference(source_component_ref_from_name(
            name,
        )),
        subscripts: vec![],
        span: lower_test_span(),
    }
}

fn unresolved_source_var(name: &str) -> rumoca_core::Expression {
    let mut component_ref = test_component_ref_from_name(name);
    component_ref.span = lower_test_span();
    for part in &mut component_ref.parts {
        part.span = lower_test_span();
    }
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::from_component_reference(component_ref),
        subscripts: vec![],
        span: lower_test_span(),
    }
}

fn var_index(name: &str, index: i64) -> rumoca_core::Expression {
    let span = lower_test_span();
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::from_component_reference(source_component_ref_from_name(
            name,
        )),
        subscripts: vec![rumoca_core::Subscript::generated_index(index, span)],
        span,
    }
}

fn var_index_expr(name: &str, index: rumoca_core::Expression) -> rumoca_core::Expression {
    let span = lower_test_span();
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::from_component_reference(source_component_ref_from_name(
            name,
        )),
        subscripts: vec![rumoca_core::Subscript::generated_expr(
            Box::new(index),
            span,
        )],
        span,
    }
}

fn real_lit(value: f64) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(value),
        span: lower_test_span(),
    }
}

fn int_lit(value: i64) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Integer(value),
        span: lower_test_span(),
    }
}

fn pre_var(name: &str) -> rumoca_core::Expression {
    var(&format!("__pre__.{name}"))
}

fn indexed_var(name: &str, index: i64) -> rumoca_core::Expression {
    let span = lower_test_span();
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::from_component_reference(source_component_ref_from_name(
            name,
        )),
        subscripts: vec![rumoca_core::Subscript::generated_index(index, span)],
        span,
    }
}

fn der(expr: rumoca_core::Expression) -> rumoca_core::Expression {
    rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Der,
        args: vec![expr],
        span: lower_test_span(),
    }
}

fn stream_passthrough_expr(name: &str, arg: rumoca_core::Expression) -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new(name).into(),
        args: vec![arg],
        is_constructor: false,
        span: lower_test_span(),
    }
}

fn size_expr(expr: rumoca_core::Expression, dim: i64) -> rumoca_core::Expression {
    let span = lower_test_span();
    rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Size,
        args: vec![
            expr,
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(dim),
                span,
            },
        ],
        span,
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
        span: lower_test_span(),
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
        span: lower_test_span(),
        origin: origin.to_string(),
        scalar_count: 1,
    }
}

fn named_arg(name: &str, value: rumoca_core::Expression) -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new(format!("__rumoca_named_arg__.{name}")).into(),
        args: vec![value],
        is_constructor: false,
        span: lower_test_span(),
    }
}

#[test]
fn lower_discrete_rhs_keeps_pre_selector_branch_as_runtime_select() {
    // Regression: an inner `if pre(s) == 1` nested inside an array-valued update
    // must lower to a run-time `Select` reading `__pre__.s`, never collapse to
    // the selector's start-value branch. `__pre__.s` is the previous-step memory
    // of a discrete state (mutable, rewritten every tick) even though the DAE
    // registers it as a non-tunable parameter with a `start`. The array lowering
    // path used to treat that parameter as a translation-time constant and fold
    // the branch, while the scalar path kept the `Select` -- so the same selector
    // resolved two different ways across an equation (cross-track guidance bug).
    let mut dae_model = dae::Dae::default();
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("__pre__.s"),
        dae::Variable {
            start: Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(1.0),
                span: lower_test_span(),
            }),
            fixed: Some(true),
            is_tunable: false,
            ..dae::Variable::new(rumoca_core::VarName::new("__pre__.s"), lower_test_span())
        },
    );
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("u"), scalar_var("u"));
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("y"),
        dae::Variable {
            dims: vec![2],
            ..dae::Variable::new(rumoca_core::VarName::new("y"), lower_test_span())
        },
    );
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y").into()),
        rhs: rumoca_core::Expression::Array {
            elements: vec![
                rumoca_core::Expression::If {
                    branches: vec![(
                        rumoca_core::Expression::Binary {
                            op: rumoca_core::OpBinary::Eq,
                            lhs: Box::new(var("__pre__.s")),
                            rhs: Box::new(rumoca_core::Expression::Literal {
                                value: rumoca_core::Literal::Integer(1),
                                span: lower_test_span(),
                            }),
                            span: lower_test_span(),
                        },
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Real(100.0),
                            span: lower_test_span(),
                        },
                    )],
                    else_branch: Box::new(var("u")),
                    span: lower_test_span(),
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(7.0),
                    span: lower_test_span(),
                },
            ],
            is_matrix: false,
            span: lower_test_span(),
        },
        span: lower_test_span(),
        origin: "pre-selector array if".to_string(),
        scalar_count: 2,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_discrete_rhs(&dae_model, &layout).expect("pre-selector branch should lower");
    assert_eq!(rows.len(), 2);

    // pre(s) == 1 at runtime -> then-branch constant.
    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "__pre__.s", 1.0);
    set_p_value(&layout, &mut p, "u", 42.0);
    let (_, then_value) = eval_linear_ops(&rows[0], &[], &p, 0.0);
    assert_eq!(then_value, Some(100.0));

    // pre(s) advances to 2 -> else-branch reads `u`. A folded branch would still
    // return 100.0 here, ignoring the run-time selector.
    set_p_value(&layout, &mut p, "__pre__.s", 2.0);
    let (_, else_value) = eval_linear_ops(&rows[0], &[], &p, 0.0);
    assert_eq!(else_value, Some(42.0));
}
