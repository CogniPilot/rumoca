//! Per-area fixture provenance regressions (part 1).

use super::super::*;
use super::helpers::*;

#[test]
fn test_phase_dae_root_fixtures_do_not_use_dummy_spans() {
    let root = workspace_root();
    let fixture_root = root.join("crates/rumoca-phase-dae/src/tests/root");
    let mut rs_files = Vec::new();
    collect_rs_files(&fixture_root, &mut rs_files);

    let offenders = rs_files
        .into_iter()
        .flat_map(|path| dummy_span_use_locations(&path))
        .collect::<Vec<_>>();

    assert!(
        offenders.is_empty(),
        "phase-DAE root fixtures must use source-derived spans or \
explicit deterministic test spans, not Span::DUMMY. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_structural_incidence_tests_do_not_use_dummy_spans() {
    let path = workspace_root().join("crates/rumoca-phase-structural/src/incidence.rs");
    let offenders = dummy_span_use_locations(&path);

    assert!(
        offenders.is_empty(),
        "phase-structural incidence tests must use source-named fixture spans, \
not Span::DUMMY. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_residual_compute_block_tests_only_use_dummy_spans_for_unspanned_regressions() {
    let path = workspace_root().join("crates/rumoca-phase-solve/src/residual_compute_block.rs");
    let content = fs::read_to_string(&path).expect("read residual_compute_block tests");
    let mut current_test: Option<&str> = None;
    let mut offenders = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if let Some(fn_name) = trimmed.strip_prefix("fn ") {
            current_test = fn_name.split('(').next();
        }
        let direct_dummy_allowed = matches!(current_test, Some("unspanned_residual_test_span"));
        let unspanned_helper_allowed = matches!(
            current_test,
            Some("unspanned_residual_test_span")
                | Some("residual_equation_scalar_count_does_not_fabricate_dummy_span")
                | Some("residual_output_y_range_does_not_fabricate_dummy_span")
        );
        if (line_has_dummy_span_use(line) && !direct_dummy_allowed)
            || (line_has_unspanned_test_span_use(line) && !unspanned_helper_allowed)
        {
            offenders.push(format!("{}:{}", path.display(), idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "residual compute block tests must use source-named fixture spans except \
in explicit unspanned-error regressions. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_runtime_assignments_tests_only_use_dummy_spans_for_unspanned_regressions() {
    let path = workspace_root().join("crates/rumoca-phase-solve/src/runtime_assignments.rs");
    let content = fs::read_to_string(&path).expect("read runtime_assignments tests");
    let mut current_test: Option<&str> = None;
    let mut offenders = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if let Some(fn_name) = trimmed.strip_prefix("fn ") {
            current_test = fn_name.split('(').next();
        }
        let direct_dummy_allowed =
            matches!(current_test, Some("unspanned_runtime_assignment_test_span"));
        let unspanned_helper_allowed = matches!(
            current_test,
            Some("unspanned_runtime_assignment_test_span")
                | Some(
                    "runtime_assignment_scalar_name_reports_missing_array_lhs_without_dummy_span"
                )
        );
        if (line_has_dummy_span_use(line) && !direct_dummy_allowed)
            || (line_has_unspanned_test_span_use(line) && !unspanned_helper_allowed)
        {
            offenders.push(format!("{}:{}", path.display(), idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "runtime assignment tests must use source-named fixture spans except in \
explicit unspanned-error regressions. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_dynamic_events_tests_only_use_dummy_spans_for_missing_provenance() {
    let path = workspace_root().join("crates/rumoca-phase-solve/src/dynamic_events.rs");
    let content = fs::read_to_string(&path).expect("read dynamic_events tests");
    let mut current_test: Option<&str> = None;
    let mut offenders = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if let Some(fn_name) = trimmed.strip_prefix("fn ") {
            current_test = fn_name.split('(').next();
        }
        let direct_dummy_allowed =
            matches!(current_test, Some("unspanned_dynamic_event_test_span"));
        let unspanned_helper_allowed = matches!(
            current_test,
            Some("unspanned_dynamic_event_test_span")
                | Some("next_period_event_expr_rejects_missing_source_span")
        );
        if (line_has_dummy_span_use(line) && !direct_dummy_allowed)
            || (line_has_unspanned_test_span_use(line) && !unspanned_helper_allowed)
        {
            offenders.push(format!("{}:{}", path.display(), idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "dynamic event tests must use source-named fixture spans except in \
explicit missing-provenance regressions. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_observation_aliases_tests_do_not_use_dummy_spans() {
    let path = workspace_root().join("crates/rumoca-phase-solve/src/tests/observation_aliases.rs");
    let content = fs::read_to_string(&path).expect("read observation_aliases tests");
    let offenders = content
        .lines()
        .enumerate()
        .filter(|&(_idx, line)| line_has_dummy_span_use(line))
        .map(|(idx, _line)| format!("{}:{}", path.display(), idx + 1))
        .collect::<Vec<_>>();

    assert!(
        offenders.is_empty(),
        "observation alias tests must use source-named fixture spans, not \
Span::DUMMY. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_initial_values_tests_do_not_use_dummy_spans() {
    let path = workspace_root().join("crates/rumoca-phase-solve/src/initial_values.rs");
    let content = fs::read_to_string(&path).expect("read initial_values tests");
    let offenders = content
        .lines()
        .enumerate()
        .filter(|&(_idx, line)| line_has_dummy_span_use(line))
        .map(|(idx, _line)| format!("{}:{}", path.display(), idx + 1))
        .collect::<Vec<_>>();

    assert!(
        offenders.is_empty(),
        "initial values tests must use source-named fixture spans, not \
Span::DUMMY. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_implicit_rhs_tests_do_not_use_dummy_spans() {
    let path = workspace_root().join("crates/rumoca-phase-solve/src/implicit_rhs.rs");
    let content = fs::read_to_string(&path).expect("read implicit_rhs tests");
    let offenders = content
        .lines()
        .enumerate()
        .filter(|&(_idx, line)| line_has_dummy_span_use(line))
        .map(|(idx, _line)| format!("{}:{}", path.display(), idx + 1))
        .collect::<Vec<_>>();

    assert!(
        offenders.is_empty(),
        "implicit RHS tests must use source-named fixture spans, not \
Span::DUMMY. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_layout_tests_only_use_dummy_spans_for_source_free_regressions() {
    let path = workspace_root().join("crates/rumoca-phase-solve/src/layout.rs");
    let content = fs::read_to_string(&path).expect("read layout tests");
    let mut current_test: Option<&str> = None;
    let mut offenders = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if let Some(fn_name) = trimmed.strip_prefix("fn ") {
            current_test = fn_name.split('(').next();
        }
        let direct_dummy_allowed = matches!(current_test, Some("unspanned_layout_test_span"));
        let unspanned_helper_allowed = matches!(
            current_test,
            Some("unspanned_layout_test_span")
                | Some("scalar_slot_reports_byte_offset_overflow_without_dummy_span")
                | Some("reserve_indexed_slot_capacity_reports_overflow_without_dummy_span")
                | Some("expand_values_to_size_reports_capacity_overflow_without_dummy_span")
                | Some("build_var_layout_reports_invalid_dims_without_dummy_span")
        );
        if (line_has_dummy_span_use(line) && !direct_dummy_allowed)
            || (line_has_unspanned_test_span_use(line) && !unspanned_helper_allowed)
        {
            offenders.push(format!("{}:{}", path.display(), idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "layout tests must use source-named fixture spans except in explicit \
source-free regressions. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_solve_model_tests_only_use_dummy_spans_for_source_free_regressions() {
    let path = workspace_root().join("crates/rumoca-phase-solve/src/solve_model/tests.rs");
    let content = fs::read_to_string(&path).expect("read solve_model tests");
    let mut current_test: Option<&str> = None;
    let mut offenders = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if let Some(fn_name) = trimmed.strip_prefix("fn ") {
            current_test = fn_name.split('(').next();
        }
        let direct_dummy_allowed = matches!(current_test, Some("unspanned_solve_model_test_span"));
        let unspanned_helper_allowed = matches!(
            current_test,
            Some("unspanned_solve_model_test_span")
                | Some("expand_values_to_size_reports_capacity_overflow_without_dummy_span")
                | Some("visible_subscripts_report_integer_range_overflow_without_dummy_span")
                | Some("state_derivative_order_flags_report_capacity_overflow_without_dummy_span")
        );
        if (line_has_dummy_span_use(line) && !direct_dummy_allowed)
            || (line_has_unspanned_test_span_use(line) && !unspanned_helper_allowed)
        {
            offenders.push(format!("{}:{}", path.display(), idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "solve-model tests must use source-named fixture spans except in \
explicit source-free regressions. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_lower_small_fixture_tests_do_not_use_dummy_spans() {
    let paths = [
        "crates/rumoca-phase-solve/src/lower/tests/explicit_residual_tests.rs",
        "crates/rumoca-phase-solve/src/lower/tests/root_condition_tests.rs",
        "crates/rumoca-phase-solve/src/lower/tests/event_intrinsics.rs",
    ];
    let mut offenders = Vec::new();
    for relative_path in paths {
        let path = workspace_root().join(relative_path);
        let content = fs::read_to_string(&path).expect("read lower fixture tests");
        offenders.extend(
            content
                .lines()
                .enumerate()
                .filter(|&(_idx, line)| line_has_dummy_span_use(line))
                .map(|(idx, _line)| format!("{}:{}", path.display(), idx + 1)),
        );
    }

    assert!(
        offenders.is_empty(),
        "lower fixture tests must use source-named fixture spans, not \
Span::DUMMY. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_phase_solve_test_support_does_not_use_dummy_spans_as_fixture_markers() {
    let path = workspace_root().join("crates/rumoca-phase-solve/src/tests/test_support.rs");
    let offenders = dummy_span_use_locations(&path);

    assert!(
        offenders.is_empty(),
        "phase-solve test support must use explicit source-named fixture \
markers, not Span::DUMMY. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_phase_solve_root_tests_only_use_unspanned_helper_for_source_free_regressions() {
    let path = workspace_root().join("crates/rumoca-phase-solve/src/tests.rs");
    let content = fs::read_to_string(&path).expect("read phase-solve root tests");
    let mut current_fn: Option<&str> = None;
    let mut offenders = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if let Some(fn_name) = trimmed.strip_prefix("fn ") {
            current_fn = fn_name.split('(').next();
        }
        let allowed_source_free_regression = matches!(
            current_fn,
            Some("unspanned_solve_test_span")
                | Some("lower_vec_with_capacity_reports_capacity_overflow_without_dummy_span")
                | Some("lower_hash_set_with_capacity_reports_capacity_overflow_without_dummy_span")
                | Some(
                    "reserve_lower_index_map_capacity_reports_capacity_overflow_without_dummy_span"
                )
                | Some("checked_layout_remainder_reports_underflow_without_dummy_span")
                | Some("algebraic_projection_plan_reports_range_underflow_without_dummy_span")
                | Some("checked_literal_positive_indices_rejects_missing_source_span")
                | Some("checked_solver_scalar_index_rejects_overflow_without_dummy_span")
                | Some("checked_solver_scalar_offset_rejects_overflow_without_dummy_span")
                | Some(
                    "continuous_equation_scalar_name_reports_missing_array_lhs_without_dummy_span"
                )
                | Some("discrete_update_scalar_name_reports_missing_array_lhs_without_dummy_span")
                | Some("implicit_rhs_reports_solver_sized_buffer_overflow")
        );
        if (line_has_dummy_span_use(line) || line_has_unspanned_test_span_use(line))
            && !allowed_source_free_regression
        {
            offenders.push(format!("{}:{}", path.display(), idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "phase-solve root tests must use source-named fixture spans except in \
explicit source-free regressions routed through unspanned_solve_test_span(). \
Offenders: {offenders:#?}"
    );
}

#[test]
fn test_lower_parent_tests_only_use_dummy_spans_for_source_free_regressions() {
    let path = workspace_root().join("crates/rumoca-phase-solve/src/lower/tests.rs");
    let content = fs::read_to_string(&path).expect("read lower parent tests");
    let mut current_fn: Option<&str> = None;
    let mut offenders = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if let Some(fn_name) = trimmed.strip_prefix("fn ") {
            current_fn = fn_name.split('(').next();
        }
        let allowed_source_free_boundary = matches!(
            current_fn,
            Some("unspanned_test_span")
                | Some("lower_builder_try_pack_registers_rejects_overflow")
                | Some("random_result_rejects_register_allocation_overflow")
                | Some("random_result_overflow_does_not_emit_partial_pack")
                | Some("size_builtin_rejects_unspanned_base_without_fabricating_span")
                | Some("structural_index_selector_rejects_register_allocation_overflow_with_span")
                | Some("structural_index_selector_uses_context_span_for_generated_subscript")
                | Some("var")
        );
        if (line_has_dummy_span_use(line) || line_has_unspanned_test_span_use(line))
            && !allowed_source_free_boundary
        {
            offenders.push(format!("{}:{}", path.display(), idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "lower parent tests must use source-named fixture spans except in \
explicit source-free regressions and the generated VarRef fixture helper. \
Offenders: {offenders:#?}"
    );
}

#[test]
fn test_discrete_expression_tests_do_not_use_dummy_spans() {
    let path = workspace_root()
        .join("crates/rumoca-phase-solve/src/lower/tests/discrete_expression_tests.rs");
    let offenders = dummy_span_use_locations(&path);

    assert!(
        offenders.is_empty(),
        "discrete expression tests must use source-named fixture spans, not \
Span::DUMMY. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_array_operator_tests_only_use_dummy_spans_for_source_free_regressions() {
    let path =
        workspace_root().join("crates/rumoca-phase-solve/src/lower/tests/array_operator_tests.rs");
    let content = fs::read_to_string(&path).expect("read array_operator tests");
    let mut current_test: Option<&str> = None;
    let mut offenders = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if let Some(fn_name) = trimmed.strip_prefix("fn ") {
            current_test = fn_name.split('(').next();
        }
        let allowed_source_free_regression = matches!(
            current_test,
            Some("record_array_member_slice_with_numbering_gap_is_a_contract_violation")
                | Some("scalar_structural_index_rejects_unspanned_function_call_context")
        );
        if (line_has_dummy_span_use(line) || line_has_unspanned_test_span_use(line))
            && !allowed_source_free_regression
        {
            offenders.push(format!("{}:{}", path.display(), idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "array operator tests must use source-named fixture spans except in \
explicit source-free regressions. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_function_shape_diagnostic_tests_do_not_use_dummy_spans() {
    let path = workspace_root().join(
        "crates/rumoca-phase-solve/src/lower/tests/function_expression_tests/shape_diagnostic_tests.rs",
    );
    let offenders = dummy_span_use_locations(&path);

    assert!(
        offenders.is_empty(),
        "function shape diagnostic tests must use source-named fixture spans, \
not Span::DUMMY. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_vector_derivative_projection_tests_do_not_use_dummy_spans() {
    let path = workspace_root().join(
        "crates/rumoca-phase-solve/src/lower/tests/array_operator_tests/derivative_projection_tests/vector_projection_tests.rs",
    );
    let offenders = dummy_span_use_locations(&path);

    assert!(
        offenders.is_empty(),
        "vector derivative projection tests must use source-named fixture \
spans, not Span::DUMMY. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_record_derivative_projection_tests_do_not_use_dummy_spans() {
    let path = workspace_root().join(
        "crates/rumoca-phase-solve/src/lower/tests/array_operator_tests/derivative_projection_tests/record_projection_tests.rs",
    );
    let offenders = dummy_span_use_locations(&path);

    assert!(
        offenders.is_empty(),
        "record derivative projection tests must use source-named fixture \
spans, not Span::DUMMY. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_complex_projection_runtime_tests_do_not_use_dummy_spans() {
    let path = workspace_root().join(
        "crates/rumoca-phase-solve/src/lower/tests/projection_runtime_tests/complex_projection.rs",
    );
    let offenders = dummy_span_use_locations(&path);

    assert!(
        offenders.is_empty(),
        "complex projection runtime tests must use source-named fixture spans, \
not Span::DUMMY. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_projection_runtime_tests_do_not_use_dummy_spans() {
    let path = workspace_root()
        .join("crates/rumoca-phase-solve/src/lower/tests/projection_runtime_tests.rs");
    let offenders = dummy_span_use_locations(&path);

    assert!(
        offenders.is_empty(),
        "projection runtime tests must use source-named fixture spans, not \
Span::DUMMY. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_discrete_array_tests_only_use_dummy_spans_for_unspanned_regressions() {
    let path = workspace_root().join(
        "crates/rumoca-phase-solve/src/lower/tests/array_operator_tests/discrete_array_tests.rs",
    );
    let content = fs::read_to_string(&path).expect("read discrete_array tests");
    let mut current_test: Option<&str> = None;
    let mut offenders = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if let Some(fn_name) = trimmed.strip_prefix("fn ") {
            current_test = fn_name.split('(').next();
        }
        let allowed_unspanned_regression =
            current_test == Some("lower_expression_rows_rejects_unspanned_matmul_node");
        if line_has_dummy_span_use(line) && !allowed_unspanned_regression {
            offenders.push(format!("{}:{}", path.display(), idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "discrete array tests must use source-named fixture spans except in \
explicit unspanned regressions. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_function_statement_projection_tests_do_not_use_dummy_spans() {
    let path = workspace_root().join(
        "crates/rumoca-phase-solve/src/lower/tests/function_expression_tests/statement_and_projection_tests.rs",
    );
    let offenders = dummy_span_use_locations(&path);

    assert!(
        offenders.is_empty(),
        "function statement/projection tests must use source-named fixture \
spans, not Span::DUMMY. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_function_expression_parent_tests_do_not_use_dummy_spans() {
    let path = workspace_root()
        .join("crates/rumoca-phase-solve/src/lower/tests/function_expression_tests.rs");
    let offenders = dummy_span_use_locations(&path);

    assert!(
        offenders.is_empty(),
        "function expression parent tests must use source-named fixture spans, \
not Span::DUMMY. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_statement_tests_only_use_dummy_spans_for_source_free_regressions() {
    let path = workspace_root().join("crates/rumoca-phase-solve/src/lower/statements/tests.rs");
    let content = fs::read_to_string(&path).expect("read statement tests");
    let mut current_test: Option<&str> = None;
    let mut offenders = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if let Some(fn_name) = trimmed.strip_prefix("fn ") {
            current_test = fn_name.split('(').next();
        }
        let direct_dummy_allowed = matches!(current_test, Some("unspanned_statement_test_span"));
        let unspanned_helper_allowed = matches!(
            current_test,
            Some("unspanned_statement_test_span")
                | Some(
                    "missing_guarded_assignment_binding_rejects_dummy_span_without_fabricating_span"
                )
                | Some("checked_usize_dims_to_i64_rejects_overflow_without_fabricating_span")
        );
        if (line_has_dummy_span_use(line) && !direct_dummy_allowed)
            || (line_has_unspanned_test_span_use(line) && !unspanned_helper_allowed)
        {
            offenders.push(format!("{}:{}", path.display(), idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "statement tests must use source-named fixture spans except in explicit \
source-free regressions. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_lower_misc_tests_only_use_unspanned_helpers_for_provenance_regressions() {
    let files = [
        (
            "crates/rumoca-phase-solve/src/lower/error.rs",
            "unspanned_lower_error_test_span",
            "contract_violation_does_not_fabricate_dummy_span",
        ),
        (
            "crates/rumoca-phase-solve/src/lower/emit.rs",
            "unspanned_emit_test_span",
            "allocation_error_does_not_fabricate_dummy_span",
        ),
        (
            "crates/rumoca-phase-solve/src/lower/expression_rows.rs",
            "unspanned_expression_rows_test_span",
            "scalar_row_namespace_rejects_overflow_without_dummy_span",
        ),
    ];
    let mut offenders = Vec::new();

    for (relative_path, helper_name, regression_name) in files {
        let path = workspace_root().join(relative_path);
        let content = fs::read_to_string(&path).expect("read lower misc tests");
        let helper_call = format!("{helper_name}()");
        let mut current_test: Option<&str> = None;

        for (idx, line) in content.lines().enumerate() {
            let trimmed = line.trim_start();
            if let Some(fn_name) = trimmed.strip_prefix("fn ") {
                current_test = fn_name.split('(').next();
            }
            let direct_dummy_allowed = current_test == Some(helper_name);
            let helper_allowed =
                current_test == Some(helper_name) || current_test == Some(regression_name);
            if (line_has_dummy_span_use(line) && !direct_dummy_allowed)
                || (line_has_named_unspanned_test_span_use(line, &helper_call) && !helper_allowed)
            {
                offenders.push(format!("{}:{}", path.display(), idx + 1));
            }
        }
    }

    assert!(
        offenders.is_empty(),
        "lower misc tests must route unspanned provenance regressions through \
named helpers. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_derivative_rhs_tests_only_use_dummy_spans_for_source_free_regressions() {
    let path = workspace_root().join("crates/rumoca-phase-solve/src/lower/derivative_rhs/tests.rs");
    let content = fs::read_to_string(&path).expect("read derivative_rhs tests");
    let mut current_test: Option<&str> = None;
    let mut offenders = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if let Some(fn_name) = trimmed.strip_prefix("fn ") {
            current_test = fn_name.split('(').next();
        }
        let direct_dummy_allowed =
            matches!(current_test, Some("unspanned_derivative_rhs_test_span"));
        let unspanned_helper_allowed = matches!(
            current_test,
            Some("unspanned_derivative_rhs_test_span")
                | Some("derivative_vec_with_capacity_dummy_span_stays_unspanned")
                | Some("lower_derivative_rhs_uses_equation_span_when_state_span_is_missing")
        );
        if (line_has_dummy_span_use(line) && !direct_dummy_allowed)
            || (line_has_unspanned_test_span_use(line) && !unspanned_helper_allowed)
        {
            offenders.push(format!("{}:{}", path.display(), idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "derivative RHS tests must use source-named fixture spans except in \
explicit source-free regressions. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_projection_selection_tests_only_use_dummy_spans_for_source_free_regressions() {
    let path = workspace_root()
        .join("crates/rumoca-phase-solve/src/lower/derivative_rhs/projection_selection.rs");
    let content = fs::read_to_string(&path).expect("read projection_selection tests");
    let mut current_test: Option<&str> = None;
    let mut offenders = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if let Some(fn_name) = trimmed.strip_prefix("fn ") {
            current_test = fn_name.split('(').next();
        }
        let direct_dummy_allowed = matches!(
            current_test,
            Some("unspanned_projection_selection_test_span")
        );
        let unspanned_helper_allowed = matches!(
            current_test,
            Some("unspanned_projection_selection_test_span")
                | Some("reserve_projection_capacity_dummy_span_stays_unspanned")
        );
        if (line_has_dummy_span_use(line) && !direct_dummy_allowed)
            || (line_has_unspanned_test_span_use(line) && !unspanned_helper_allowed)
        {
            offenders.push(format!("{}:{}", path.display(), idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "projection-selection tests must use source-named fixture spans except \
in explicit source-free regressions. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_derivative_projection_tests_only_use_dummy_spans_for_unspanned_regressions() {
    let path = workspace_root()
        .join("crates/rumoca-phase-solve/src/lower/derivative_rhs/projection/tests.rs");
    let content = fs::read_to_string(&path).expect("read derivative projection tests");
    let mut current_test: Option<&str> = None;
    let mut offenders = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if let Some(fn_name) = trimmed.strip_prefix("fn ") {
            current_test = fn_name.split('(').next();
        }
        let allowed_unspanned_regression = matches!(
            current_test,
            Some("checked_projection_offset_dummy_span_stays_unspanned")
                | Some("checked_usize_scalar_count_dummy_span_stays_unspanned")
                | Some("next_range_value_dummy_span_stays_unspanned")
                | Some("expression_result_dims_rejects_synthetic_der_without_argument_unspanned")
                | Some("expression_result_dims_rejects_synthetic_fill_without_dimension_unspanned")
        );
        if line_has_dummy_span_use(line) && !allowed_unspanned_regression {
            offenders.push(format!("{}:{}", path.display(), idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "derivative projection tests must use source-named fixture spans except \
in explicit unspanned regressions. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_derivative_function_projection_tests_only_use_dummy_spans_for_unspanned_regressions() {
    let path = workspace_root()
        .join("crates/rumoca-phase-solve/src/lower/derivative_rhs/function_projection/tests.rs");
    let content = fs::read_to_string(&path).expect("read derivative function_projection tests");
    let mut current_test: Option<&str> = None;
    let mut offenders = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if let Some(fn_name) = trimmed.strip_prefix("fn ") {
            current_test = fn_name.split('(').next();
        }
        let allowed_unspanned_regression = matches!(
            current_test,
            Some("checked_projection_offset_dummy_span_stays_unspanned")
                | Some("checked_usize_dims_to_i64_dummy_span_stays_unspanned")
                | Some("reserve_projection_capacity_dummy_span_stays_unspanned")
                | Some("scalar_count_dummy_span_stays_unspanned")
                | Some("generated_function_call_projection_errors_use_owner_span")
        );
        if line_has_dummy_span_use(line) && !allowed_unspanned_regression {
            offenders.push(format!("{}:{}", path.display(), idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "derivative function-projection tests must use source-named fixture \
spans except in explicit unspanned regressions. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_lower_projection_derivative_tests_do_not_use_dummy_spans() {
    let path = workspace_root()
        .join("crates/rumoca-phase-solve/src/lower/tests/projection_derivative_tests.rs");
    let offenders = dummy_span_use_locations(&path);

    assert!(
        offenders.is_empty(),
        "lower projection-derivative tests must use source-named fixture spans, \
not Span::DUMMY. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_function_loop_tests_do_not_use_dummy_spans() {
    let path =
        workspace_root().join("crates/rumoca-phase-solve/src/lower/tests/function_loop_tests.rs");
    let offenders = dummy_span_use_locations(&path);

    assert!(
        offenders.is_empty(),
        "function loop tests must use source-named fixture spans, not \
Span::DUMMY. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_fft_tests_only_use_dummy_spans_for_source_free_regressions() {
    let path = workspace_root().join("crates/rumoca-phase-solve/src/lower/fft.rs");
    let content = fs::read_to_string(&path).expect("read fft tests");
    let mut current_test: Option<&str> = None;
    let mut offenders = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if let Some(fn_name) = trimmed.strip_prefix("fn ") {
            current_test = fn_name.split('(').next();
        }
        let direct_dummy_allowed = matches!(current_test, Some("unspanned_fft_test_span"));
        let unspanned_helper_allowed = matches!(
            current_test,
            Some("unspanned_fft_test_span")
                | Some(
                    "checked_fft_frequency_count_rejects_host_overflow_without_fabricating_span"
                )
                | Some("fft_projection_error_is_unspanned_without_projection_span")
        );
        if (line_has_dummy_span_use(line) && !direct_dummy_allowed)
            || (line_has_unspanned_test_span_use(line) && !unspanned_helper_allowed)
        {
            offenders.push(format!("{}:{}", path.display(), idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "FFT tests must use source-named fixture spans except in explicit \
source-free regressions. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_compile_time_tests_only_use_dummy_spans_for_source_free_regressions() {
    let path = workspace_root().join("crates/rumoca-phase-solve/src/lower/compile_time.rs");
    let content = fs::read_to_string(&path).expect("read compile_time tests");
    let mut current_test: Option<&str> = None;
    let mut offenders = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if let Some(fn_name) = trimmed.strip_prefix("fn ") {
            current_test = fn_name.split('(').next();
        }
        let direct_dummy_allowed = matches!(current_test, Some("unspanned_compile_time_test_span"));
        let unspanned_helper_allowed = matches!(
            current_test,
            Some("unspanned_compile_time_test_span")
                | Some("expand_values_to_size_reports_capacity_overflow_without_fabricating_span")
        );
        if (line_has_dummy_span_use(line) && !direct_dummy_allowed)
            || (line_has_unspanned_test_span_use(line) && !unspanned_helper_allowed)
        {
            offenders.push(format!("{}:{}", path.display(), idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "compile-time tests must use source-named fixture spans except in \
explicit source-free regressions. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_random_function_call_tests_only_use_dummy_spans_for_source_free_regressions() {
    let path =
        workspace_root().join("crates/rumoca-phase-solve/src/lower/function_calls/random.rs");
    let content = fs::read_to_string(&path).expect("read random function call tests");
    let mut current_test: Option<&str> = None;
    let mut offenders = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if let Some(fn_name) = trimmed.strip_prefix("fn ") {
            current_test = fn_name.split('(').next();
        }
        let direct_dummy_allowed = matches!(current_test, Some("unspanned_random_test_span"));
        let unspanned_helper_allowed = matches!(
            current_test,
            Some("unspanned_random_test_span")
                | Some("checked_random_value_capacity_does_not_fabricate_dummy_span")
        );
        if (line_has_dummy_span_use(line) && !direct_dummy_allowed)
            || (line_has_unspanned_test_span_use(line) && !unspanned_helper_allowed)
        {
            offenders.push(format!("{}:{}", path.display(), idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "random function-call tests must use source-named fixture spans except \
in explicit source-free regressions. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_discrete_update_tests_only_use_dummy_spans_for_source_free_regressions() {
    let path = workspace_root().join("crates/rumoca-phase-solve/src/lower/discrete_updates.rs");
    let content = fs::read_to_string(&path).expect("read discrete_updates tests");
    let mut current_test: Option<&str> = None;
    let mut offenders = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if let Some(fn_name) = trimmed.strip_prefix("fn ") {
            current_test = fn_name.split('(').next();
        }
        let direct_dummy_allowed =
            matches!(current_test, Some("unspanned_discrete_update_test_span"));
        let unspanned_helper_allowed = matches!(
            current_test,
            Some("unspanned_discrete_update_test_span")
                | Some(
                    "discrete_vec_with_capacity_reports_capacity_overflow_without_fabricating_span"
                )
                | Some("checked_relation_offset_end_rejects_overflow_without_fabricating_span")
                | Some("checked_relation_subscript_rejects_i64_overflow_without_fabricating_span")
                | Some("component_reference_with_scalar_indices_rejects_dummy_span")
        );
        if (line_has_dummy_span_use(line) && !direct_dummy_allowed)
            || (line_has_unspanned_test_span_use(line) && !unspanned_helper_allowed)
        {
            offenders.push(format!("{}:{}", path.display(), idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "discrete update tests must use source-named fixture spans except in \
explicit source-free regressions. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_derivative_linear_parts_tests_only_use_dummy_spans_for_source_free_regressions() {
    let path =
        workspace_root().join("crates/rumoca-phase-solve/src/lower/derivative_rhs/linear_parts.rs");
    let content = fs::read_to_string(&path).expect("read derivative linear parts tests");
    let mut current_test: Option<&str> = None;
    let mut offenders = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if let Some(fn_name) = trimmed.strip_prefix("fn ") {
            current_test = fn_name.split('(').next();
        }
        let direct_dummy_allowed = matches!(current_test, Some("unspanned_linear_parts_test_span"));
        let unspanned_helper_allowed = matches!(
            current_test,
            Some("unspanned_linear_parts_test_span")
                | Some("linear_index_map_capacity_dummy_span_stays_unspanned")
                | Some("der_state_name_missing_argument_dummy_span_stays_unspanned")
        );
        if (line_has_dummy_span_use(line) && !direct_dummy_allowed)
            || (line_has_unspanned_test_span_use(line) && !unspanned_helper_allowed)
        {
            offenders.push(format!("{}:{}", path.display(), idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "derivative linear-part tests must use source-named fixture spans \
except in explicit source-free regressions. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_stencil_tests_only_use_dummy_spans_for_source_free_regressions() {
    let path = workspace_root().join("crates/rumoca-phase-solve/src/stencil/tests.rs");
    let content = fs::read_to_string(&path).expect("read stencil tests");
    let mut current_test: Option<&str> = None;
    let mut offenders = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if let Some(fn_name) = trimmed.strip_prefix("fn ") {
            current_test = fn_name.split('(').next();
        }
        let direct_dummy_allowed = matches!(current_test, Some("unspanned_stencil_test_span"));
        let unspanned_helper_allowed = matches!(
            current_test,
            Some("unspanned_stencil_test_span")
                | Some("ordinal_delta_rejects_i64_underflow")
                | Some("apply_integer_terms_rejects_host_overflow")
                | Some("apply_float_terms_rejects_non_finite_result")
        );
        if (line_has_dummy_span_use(line) && !direct_dummy_allowed)
            || (line_has_unspanned_test_span_use(line) && !unspanned_helper_allowed)
        {
            offenders.push(format!("{}:{}", path.display(), idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "stencil tests must use source-named fixture spans except in explicit \
source-free regressions. Offenders: {offenders:#?}"
    );
}
