//! Per-area fixture provenance regressions (part 2).

use super::super::*;
use super::helpers::*;

#[test]
fn test_root_condition_tests_only_use_dummy_spans_for_source_free_regressions() {
    let path = workspace_root().join("crates/rumoca-phase-solve/src/lower/root_conditions.rs");
    let content = fs::read_to_string(&path).expect("read root_conditions tests");
    let mut current_test: Option<&str> = None;
    let mut offenders = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if let Some(fn_name) = trimmed.strip_prefix("fn ") {
            current_test = fn_name.split('(').next();
        }
        let direct_dummy_allowed =
            matches!(current_test, Some("unspanned_root_condition_test_span"));
        let unspanned_helper_allowed = matches!(
            current_test,
            Some("unspanned_root_condition_test_span")
                | Some("checked_relation_offset_end_rejects_overflow_without_dummy_span")
                | Some("root_vec_with_capacity_rejects_impossible_capacity_without_dummy_span")
        );
        if (line_has_dummy_span_use(line) && !direct_dummy_allowed)
            || (line_has_unspanned_test_span_use(line) && !unspanned_helper_allowed)
        {
            offenders.push(format!("{}:{}", path.display(), idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "root-condition tests must use source-named fixture spans except in \
explicit source-free regressions. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_function_projection_tests_only_use_dummy_spans_for_source_free_regressions() {
    let path = workspace_root().join("crates/rumoca-phase-solve/src/lower/function_projection.rs");
    let content = fs::read_to_string(&path).expect("read function_projection tests");
    let mut current_test: Option<&str> = None;
    let mut offenders = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if let Some(fn_name) = trimmed.strip_prefix("fn ") {
            current_test = fn_name.split('(').next();
        }
        let direct_dummy_allowed = matches!(
            current_test,
            Some("unspanned_function_projection_test_span")
        );
        let unspanned_helper_allowed = matches!(
            current_test,
            Some("unspanned_function_projection_test_span")
                | Some("normalize_projection_indices_declines_overflowing_shape_product")
                | Some("scope_indices_for_projection_declines_zero_linear_index")
        );
        if (line_has_dummy_span_use(line) && !direct_dummy_allowed)
            || (line_has_unspanned_test_span_use(line) && !unspanned_helper_allowed)
        {
            offenders.push(format!("{}:{}", path.display(), idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "function projection tests must use source-named fixture spans except in \
explicit source-free regressions. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_complex_operator_tests_only_use_dummy_spans_for_source_free_regressions() {
    let path =
        workspace_root().join("crates/rumoca-phase-solve/src/lower/complex_operator_tests.rs");
    let content = fs::read_to_string(&path).expect("read complex_operator_tests");
    let mut current_test: Option<&str> = None;
    let mut offenders = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if let Some(fn_name) = trimmed.strip_prefix("fn ") {
            current_test = fn_name.split('(').next();
        }
        let direct_dummy_allowed =
            matches!(current_test, Some("unspanned_complex_operator_test_span"));
        let unspanned_helper_allowed = matches!(
            current_test,
            Some("unspanned_complex_operator_test_span")
                | Some("builtin_argument_error_does_not_fabricate_dummy_span")
                | Some("complex_projection_source_reference_requires_span_metadata")
        );
        if (line_has_dummy_span_use(line) && !direct_dummy_allowed)
            || (line_has_unspanned_test_span_use(line) && !unspanned_helper_allowed)
        {
            offenders.push(format!("{}:{}", path.display(), idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "complex operator tests must use source-named fixture spans except in \
explicit source-free regressions. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_event_action_tests_only_use_dummy_spans_for_missing_provenance() {
    let path = workspace_root().join("crates/rumoca-phase-solve/src/event_actions.rs");
    let content = fs::read_to_string(&path).expect("read event_actions tests");
    let mut current_test: Option<&str> = None;
    let mut offenders = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if let Some(fn_name) = trimmed.strip_prefix("fn ") {
            current_test = fn_name.split('(').next();
        }
        let allowed_missing_provenance = matches!(
            current_test,
            Some("unspanned_event_action_test_span")
                | Some("lower_event_action_rejects_missing_source_span")
                | Some("event_vec_with_capacity_does_not_fabricate_dummy_span")
        );
        if (line_has_dummy_span_use(line) || line_has_unspanned_test_span_use(line))
            && !allowed_missing_provenance
        {
            offenders.push(format!("{}:{}", path.display(), idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "event action tests must use source-named fixture spans except in \
explicit missing-provenance regressions. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_array_values_tests_only_use_dummy_spans_for_provenance_regressions() {
    let paths = [
        "crates/rumoca-phase-solve/src/lower/array_values/tests.rs",
        "crates/rumoca-phase-solve/src/lower/array_values/inference.rs",
        "crates/rumoca-phase-solve/src/lower/array_values/helpers.rs",
        "crates/rumoca-phase-solve/src/lower/array_values/builtins.rs",
        "crates/rumoca-phase-solve/src/lower/array_values/dynamic_selection.rs",
        "crates/rumoca-phase-solve/src/lower/array_values/structural_standard.rs",
    ];
    let mut offenders = Vec::new();

    for relative_path in paths {
        let path = workspace_root().join(relative_path);
        let content = fs::read_to_string(&path).expect("read array_values tests");
        let mut current_test: Option<&str> = None;
        for (idx, line) in content.lines().enumerate() {
            let trimmed = line.trim_start();
            if let Some(fn_name) = trimmed.strip_prefix("fn ") {
                current_test = fn_name.split('(').next();
            }
            if relative_path == "crates/rumoca-phase-solve/src/lower/array_values/tests.rs" {
                let direct_dummy_allowed =
                    matches!(current_test, Some("unspanned_array_values_test_span"));
                let unspanned_helper_allowed = matches!(
                    current_test,
                    Some("unspanned_array_values_test_span")
                        | Some("reserve_array_capacity_dummy_span_stays_unspanned")
                        | Some("lower_array_operand_rejects_unspanned_shaped_operand")
                        | Some("lower_array_like_values_rejects_unspanned_scalar_value")
                        | Some("lower_array_like_values_rejects_unspanned_tuple")
                        | Some("lower_array_like_values_uses_tuple_owner_for_unspanned_elements")
                        | Some("lower_min_max_builtin_rejects_empty_args_without_dummy_span")
                        | Some("lower_min_max_builtin_rejects_unspanned_reduction_arg")
                );
                let line_violates_helper_rule = (line_has_dummy_span_use(line)
                    && !direct_dummy_allowed)
                    || (line_has_unspanned_test_span_use(line) && !unspanned_helper_allowed);
                offenders.extend(
                    line_violates_helper_rule.then(|| format!("{}:{}", path.display(), idx + 1)),
                );
                continue;
            }
            let allowed_provenance_regression = matches!(
                (relative_path, current_test),
                (
                    "crates/rumoca-phase-solve/src/lower/array_values/inference.rs",
                    Some("slice_expr_indices_reports_context_span_for_generated_expr")
                        | Some(
                            "synchronous_dim_inference_missing_arg_without_span_stays_unspanned"
                        )
                        | Some("fill_dim_inference_uses_call_span_when_dim_is_generated")
                        | Some("fill_dim_inference_missing_args_without_span_stays_unspanned")
                        | Some("concrete_i64_dims_rejects_negative_dim_without_fabricating_span")
                ) | (
                    "crates/rumoca-phase-solve/src/lower/array_values/helpers.rs",
                    Some(
                        "fallible_vec_with_capacity_rejects_impossible_capacity_without_fabricating_span"
                    ) | Some("checked_dim_product_rejects_overflow_without_fabricating_span")
                ) | (
                    "crates/rumoca-phase-solve/src/lower/array_values/builtins.rs",
                    Some("array_like_builtin_missing_arg_does_not_fabricate_dummy_span")
                        | Some(
                            "reserve_reg_capacity_rejects_impossible_capacity_without_dummy_span"
                        )
                ) | (
                    "crates/rumoca-phase-solve/src/lower/array_values/dynamic_selection.rs",
                    Some("projected_record_field_expression_rejects_unspanned_value")
                        | Some("function_output_dims_invalid_dummy_span_stays_unspanned")
                ) | (
                    "crates/rumoca-phase-solve/src/lower/array_values/structural_standard.rs",
                    Some("structural_symmetric_orientation_rejects_unspanned_dimension_argument")
                )
            );
            if line_has_dummy_span_use(line) && !allowed_provenance_regression {
                offenders.push(format!("{}:{}", path.display(), idx + 1));
            }
        }
    }

    assert!(
        offenders.is_empty(),
        "array-values tests must use source-named fixture spans except in \
explicit provenance regressions. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_ad_tests_only_use_dummy_spans_for_unspanned_regressions() {
    let path = workspace_root().join("crates/rumoca-phase-solve/src/ad/tests.rs");
    let content = fs::read_to_string(&path).expect("read AD tests");
    let mut current_test: Option<&str> = None;
    let mut offenders = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if let Some(fn_name) = trimmed.strip_prefix("fn ") {
            current_test = fn_name.split('(').next();
        }
        let direct_dummy_allowed = matches!(current_test, Some("unspanned_ad_test_span"));
        let unspanned_helper_allowed = matches!(
            current_test,
            Some("unspanned_ad_test_span")
                | Some("unspanned_scalar_program_ad_rejects_mismatched_span_count")
                | Some("scalar_ad_rejects_register_allocation_overflow")
        );
        if (line_has_dummy_span_use(line) && !direct_dummy_allowed)
            || (line_has_unspanned_test_span_use(line) && !unspanned_helper_allowed)
        {
            offenders.push(format!("{}:{}", path.display(), idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "AD tests must use source-named fixture spans except in explicit \
unspanned regressions. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_runtime_intrinsics_tests_only_use_dummy_spans_for_provenance_regressions() {
    let path = workspace_root()
        .join("crates/rumoca-phase-solve/src/lower/function_calls/runtime_intrinsics.rs");
    let content = fs::read_to_string(&path).expect("read runtime_intrinsics tests");
    let mut current_test: Option<&str> = None;
    let mut offenders = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if let Some(fn_name) = trimmed.strip_prefix("fn ") {
            current_test = fn_name.split('(').next();
        }
        let direct_dummy_allowed =
            matches!(current_test, Some("unspanned_runtime_intrinsics_test_span"));
        let unspanned_helper_allowed = matches!(
            current_test,
            Some("unspanned_runtime_intrinsics_test_span")
                | Some("random_state_len_arg_rejects_unspanned_non_literal")
                | Some("random_state_len_arg_uses_owner_span_for_non_literal")
        );
        if (line_has_dummy_span_use(line) && !direct_dummy_allowed)
            || (line_has_unspanned_test_span_use(line) && !unspanned_helper_allowed)
        {
            offenders.push(format!("{}:{}", path.display(), idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "runtime intrinsic tests must use source-named fixture spans except in \
explicit provenance regressions. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_lower_intrinsics_tests_do_not_use_dummy_spans() {
    let path = workspace_root().join("crates/rumoca-phase-solve/src/lower/tests/intrinsics.rs");
    let offenders = dummy_span_use_locations(&path);

    assert!(
        offenders.is_empty(),
        "lower intrinsic tests must use source-named fixture spans, not \
Span::DUMMY. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_clocked_initial_tests_only_use_dummy_spans_for_start_guard_regressions() {
    let path = workspace_root()
        .join("crates/rumoca-phase-solve/src/lower/tests/intrinsics/clocked_initial_tests.rs");
    let content = fs::read_to_string(&path).expect("read clocked_initial tests");
    let mut current_test: Option<&str> = None;
    let mut offenders = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if let Some(fn_name) = trimmed.strip_prefix("fn ") {
            current_test = fn_name.split('(').next();
        }
        let allowed_start_guard_regression = matches!(
            current_test,
            Some("lower_runtime_assignment_uses_equation_span_for_unspanned_start_guard_rhs")
                | Some("lower_runtime_assignment_rejects_unspanned_start_guard_context")
        );
        if (line_has_dummy_span_use(line) || line_has_unspanned_test_span_use(line))
            && !allowed_start_guard_regression
        {
            offenders.push(format!("{}:{}", path.display(), idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "clocked initial tests must use source-named fixture spans except in \
explicit start-guard provenance regressions. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_function_validation_tests_do_not_use_dummy_spans() {
    let path = workspace_root().join("crates/rumoca-phase-solve/src/function_validation.rs");
    let offenders = dummy_span_use_locations(&path);

    assert!(
        offenders.is_empty(),
        "function validation tests must use source-named fixture spans, not \
Span::DUMMY. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_solve_model_external_table_random_tests_do_not_use_dummy_spans() {
    let path = workspace_root()
        .join("crates/rumoca-phase-solve/src/solve_model/tests/external_table_and_random_tests.rs");
    let offenders = dummy_span_use_locations(&path);

    assert!(
        offenders.is_empty(),
        "solve-model external-table/random tests must use source-named fixture \
spans, not Span::DUMMY. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_pre_lowering_tests_only_use_dummy_spans_for_missing_provenance() {
    let path = workspace_root().join("crates/rumoca-phase-dae/src/pre_lowering/tests.rs");
    let content = fs::read_to_string(&path).expect("read pre_lowering tests");
    let mut current_test: Option<&str> = None;
    let mut offenders = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if let Some(fn_name) = trimmed.strip_prefix("fn ") {
            current_test = fn_name.split('(').next();
        }
        if line_has_dummy_span_use(line)
            && current_test != Some("test_lower_pre_rejects_missing_source_provenance")
        {
            offenders.push(format!("{}:{}", path.display(), idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "pre-lowering tests must use file-derived fixture spans except in the \
dedicated missing-provenance regression. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_runtime_precompute_dynamic_clock_tests_do_not_use_dummy_spans() {
    let path = workspace_root()
        .join("crates/rumoca-phase-dae/src/runtime_precompute/tests/dynamic_clock_tests.rs");
    let offenders = dummy_span_use_locations(&path);

    assert!(
        offenders.is_empty(),
        "runtime precompute dynamic-clock tests must use file-derived fixture \
spans, not Span::DUMMY. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_runtime_precompute_clock_alias_tests_do_not_use_dummy_spans() {
    let path = workspace_root()
        .join("crates/rumoca-phase-dae/src/runtime_precompute/tests/clock_alias_tests.rs");
    let offenders = dummy_span_use_locations(&path);

    assert!(
        offenders.is_empty(),
        "runtime precompute clock-alias tests must use file-derived fixture \
spans, not Span::DUMMY. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_runtime_precompute_parent_tests_only_use_dummy_spans_for_missing_provenance() {
    let path = workspace_root().join("crates/rumoca-phase-dae/src/runtime_precompute/tests/mod.rs");
    let content = fs::read_to_string(&path).expect("read runtime_precompute tests");
    let mut current_test: Option<&str> = None;
    let mut offenders = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if let Some(fn_name) = trimmed.strip_prefix("fn ") {
            current_test = fn_name.split('(').next();
        }
        let allowed_missing_provenance_test = current_test
            == Some(
                "test_runtime_precompute_rejects_static_clock_constructor_without_source_provenance",
            );
        if (line_has_dummy_span_use(line) || line_has_dummy_source_id_use(line))
            && !allowed_missing_provenance_test
        {
            offenders.push(format!("{}:{}", path.display(), idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "runtime precompute parent tests must use file-derived fixture spans \
except in the dedicated missing-provenance regression. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_runtime_precompute_clock_tests_only_use_dummy_spans_for_ownerless_aliases() {
    let path = workspace_root().join("crates/rumoca-phase-dae/src/runtime_precompute/clock.rs");
    let content = fs::read_to_string(&path).expect("read runtime_precompute clock tests");
    let mut current_test: Option<&str> = None;
    let mut in_test_module = false;
    let mut offenders = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if trimmed == "mod tests {" {
            in_test_module = true;
        }
        if let Some(fn_name) = trimmed.strip_prefix("fn ") {
            current_test = fn_name.split('(').next();
        }
        let allowed_ownerless_alias_test =
            current_test == Some("reverse_alias_index_skips_ownerless_alias_expression");
        if in_test_module && line_has_dummy_span_use(line) && !allowed_ownerless_alias_test {
            offenders.push(format!("{}:{}", path.display(), idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "runtime precompute clock tests must use file-derived fixture spans \
except in the dedicated ownerless-alias regression. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_equation_conversion_tests_only_use_dummy_spans_for_provenance_failures() {
    let path = workspace_root().join("crates/rumoca-phase-dae/src/equation_conversion/tests.rs");
    let content = fs::read_to_string(&path).expect("read equation_conversion tests");
    let mut current_test: Option<&str> = None;
    let mut offenders = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if let Some(fn_name) = trimmed.strip_prefix("fn ") {
            current_test = fn_name.split('(').next();
        }
        let allowed_provenance_test = matches!(
            current_test,
            Some("test_zero_discrete_assignment_requires_equation_span")
                | Some("test_explicit_lhs_reference_requires_scalar_target_subscript_provenance")
        );
        if line_has_dummy_span_use(line) && !allowed_provenance_test {
            offenders.push(format!("{}:{}", path.display(), idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "equation conversion tests must use file-derived fixture spans except \
in dedicated provenance-failure regressions. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_dae_record_lowering_tests_only_use_dummy_spans_for_provenance_failures() {
    let path =
        workspace_root().join("crates/rumoca-phase-dae/src/dae_lowering/record_lowering_tests.rs");
    let content = fs::read_to_string(&path).expect("read DAE record lowering tests");
    let mut current_test: Option<&str> = None;
    let mut offenders = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if let Some(fn_name) = trimmed.strip_prefix("fn ") {
            current_test = fn_name.split('(').next();
        }
        let allowed_provenance_test = matches!(
            current_test,
            Some("dae_record_param_lowering_rejects_unspanned_generated_fields")
                | Some("insert_array_size_args_rejects_unspanned_generated_size_call")
        );
        if line_has_dummy_span_use(line) && !allowed_provenance_test {
            offenders.push(format!("{}:{}", path.display(), idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "DAE record lowering tests must use file-derived fixture spans except \
in dedicated provenance-failure regressions. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_classification_unit_tests_do_not_use_dummy_spans() {
    let path = workspace_root().join("crates/rumoca-phase-dae/src/analysis/classification.rs");
    let offenders = dummy_span_use_locations(&path);

    assert!(
        offenders.is_empty(),
        "classification unit tests must use file-derived fixture spans, not \
Span::DUMMY. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_definition_analysis_unit_tests_do_not_use_dummy_spans() {
    let path = workspace_root().join("crates/rumoca-phase-dae/src/analysis/definition_analysis.rs");
    let offenders = dummy_span_use_locations(&path);

    assert!(
        offenders.is_empty(),
        "definition analysis unit tests must use file-derived fixture spans, \
not Span::DUMMY. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_when_guard_tests_only_use_dummy_spans_for_provenance_cases() {
    let path = workspace_root().join("crates/rumoca-phase-dae/src/when_guard.rs");
    let content = fs::read_to_string(&path).expect("read when_guard tests");
    let mut current_test: Option<&str> = None;
    let mut in_test_module = false;
    let mut offenders = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if trimmed == "mod tests {" {
            in_test_module = true;
        }
        if let Some(fn_name) = trimmed.strip_prefix("fn ") {
            current_test = fn_name.split('(').next();
        }
        let allowed_provenance_test = matches!(
            current_test,
            Some("ownerless_when_guard_activation_fails_without_dummy_span")
                | Some("when_guard_activation_uses_enclosing_owner_span")
        );
        if in_test_module && line_has_dummy_span_use(line) && !allowed_provenance_test {
            offenders.push(format!("{}:{}", path.display(), idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "when guard tests must use file-derived fixture spans except in \
dedicated provenance regressions. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_when_conversion_unit_tests_do_not_use_dummy_spans() {
    let path = workspace_root().join("crates/rumoca-phase-dae/src/when_conversion.rs");
    let offenders = dummy_span_use_locations(&path);

    assert!(
        offenders.is_empty(),
        "when conversion unit tests must use file-derived fixture spans, not \
Span::DUMMY. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_convert_unit_tests_only_use_dummy_spans_for_provenance_cases() {
    let path = workspace_root().join("crates/rumoca-phase-dae/src/convert.rs");
    let content = fs::read_to_string(&path).expect("read phase-DAE convert tests");
    let mut current_test: Option<&str> = None;
    let mut in_test_module = false;
    let mut offenders = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if trimmed == "mod tests {" {
            in_test_module = true;
        }
        if let Some(fn_name) = trimmed.strip_prefix("fn ") {
            current_test = fn_name.split('(').next();
        }
        let allowed_provenance_test = matches!(
            current_test,
            Some("structured_target_reference_uses_flat_variable_span_for_dummy_owner")
                | Some("structured_target_reference_rejects_dummy_owner_without_flat_provenance")
                | Some("structured_target_reference_uses_dae_variable_span_for_dummy_owner")
                | Some("flat_to_dae_expression_rejects_unspanned_embedded_scalar_indices")
        );
        if in_test_module && line_has_dummy_span_use(line) && !allowed_provenance_test {
            offenders.push(format!("{}:{}", path.display(), idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "phase-DAE convert tests must use file-derived fixture spans except \
in dedicated provenance regressions. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_scalar_inference_unit_tests_only_use_dummy_spans_for_provenance_cases() {
    let path = workspace_root()
        .join("crates/rumoca-phase-dae/src/scalar_inference/inference_and_bindings.rs");
    let content = fs::read_to_string(&path).expect("read scalar inference tests");
    let mut current_test: Option<&str> = None;
    let mut in_test_module = false;
    let mut offenders = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if trimmed == "mod tests {" {
            in_test_module = true;
        }
        if let Some(fn_name) = trimmed.strip_prefix("fn ") {
            current_test = fn_name.split('(').next();
        }
        let allowed_provenance_test =
            current_test == Some("create_dae_variable_rejects_unspanned_start_attribute");
        if in_test_module && line_has_dummy_span_use(line) && !allowed_provenance_test {
            offenders.push(format!("{}:{}", path.display(), idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "scalar inference tests must use file-derived fixture spans except in \
dedicated provenance regressions. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_algorithm_lowering_unit_tests_only_use_dummy_spans_for_provenance_cases() {
    let path = workspace_root().join("crates/rumoca-phase-dae/src/algorithm_lowering/tests.rs");
    let content = fs::read_to_string(&path).expect("read algorithm lowering tests");
    let mut current_test: Option<&str> = None;
    let mut offenders = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if let Some(fn_name) = trimmed.strip_prefix("fn ") {
            current_test = fn_name.split('(').next();
        }
        let allowed_provenance_test =
            current_test == Some("lower_for_statement_assignments_rejects_dummy_owner_span");
        if line_has_dummy_span_use(line) && !allowed_provenance_test {
            offenders.push(format!("{}:{}", path.display(), idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "algorithm lowering tests must use file-derived fixture spans except \
in dedicated provenance regressions. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_binding_conversion_unit_tests_only_use_dummy_spans_for_provenance_cases() {
    let path = workspace_root().join("crates/rumoca-phase-dae/src/binding_conversion.rs");
    let content = fs::read_to_string(&path).expect("read binding conversion tests");
    let mut current_test: Option<&str> = None;
    let mut in_test_module = false;
    let mut offenders = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if trimmed == "mod tests {" {
            in_test_module = true;
        }
        if let Some(fn_name) = trimmed.strip_prefix("fn ") {
            current_test = fn_name.split('(').next();
        }
        let allowed_provenance_test = matches!(
            current_test,
            Some("binding_equation_span_uses_declaration_when_binding_is_unspanned")
                | Some("binding_equation_span_rejects_unspanned_binding_and_declaration")
        );
        if in_test_module && line_has_dummy_span_use(line) && !allowed_provenance_test {
            offenders.push(format!("{}:{}", path.display(), idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "binding conversion tests must use file-derived fixture spans except \
in dedicated provenance regressions. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_when_analysis_unit_tests_do_not_use_dummy_spans() {
    let path = workspace_root().join("crates/rumoca-phase-dae/src/when_analysis.rs");
    let offenders = dummy_span_use_locations(&path);

    assert!(
        offenders.is_empty(),
        "when analysis unit tests must use file-derived fixture spans, not \
Span::DUMMY. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_scalar_size_unit_tests_do_not_use_dummy_spans() {
    let path = workspace_root().join("crates/rumoca-phase-dae/src/scalar_size.rs");
    let offenders = dummy_span_use_locations(&path);

    assert!(
        offenders.is_empty(),
        "scalar size unit tests must use file-derived fixture spans, not \
Span::DUMMY. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_name_resolution_unit_tests_do_not_use_dummy_spans() {
    let path = workspace_root().join("crates/rumoca-phase-dae/src/name_resolution.rs");
    let offenders = dummy_span_use_locations(&path);

    assert!(
        offenders.is_empty(),
        "name resolution unit tests must use file-derived fixture spans, not \
Span::DUMMY. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_balance_unit_tests_do_not_use_dummy_spans() {
    let path = workspace_root().join("crates/rumoca-phase-dae/src/balance.rs");
    let offenders = dummy_span_use_locations(&path);

    assert!(
        offenders.is_empty(),
        "balance unit tests must use file-derived fixture spans, not \
Span::DUMMY. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_dae_lowering_tests_only_use_dummy_spans_for_provenance_cases() {
    let path = workspace_root().join("crates/rumoca-phase-dae/src/dae_lowering/tests.rs");
    let content = fs::read_to_string(&path).expect("read DAE lowering tests");
    let mut current_test: Option<&str> = None;
    let mut offenders = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if let Some(fn_name) = trimmed.strip_prefix("fn ") {
            current_test = fn_name.split('(').next();
        }
        let allowed_provenance_test =
            current_test == Some("embedded_scalar_reference_canonicalization_requires_provenance");
        if line_has_dummy_span_use(line) && !allowed_provenance_test {
            offenders.push(format!("{}:{}", path.display(), idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "DAE lowering tests must use file-derived fixture spans except in \
dedicated provenance regressions. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_appendix_b_validation_tests_only_use_dummy_spans_for_provenance_cases() {
    let path = workspace_root().join("crates/rumoca-phase-dae/src/appendix_b_validation.rs");
    let content = fs::read_to_string(&path).expect("read Appendix B validation tests");
    let mut current_test: Option<&str> = None;
    let mut in_test_module = false;
    let mut offenders = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if trimmed == "mod tests {" {
            in_test_module = true;
        }
        if let Some(fn_name) = trimmed.strip_prefix("fn ") {
            current_test = fn_name.split('(').next();
        }
        let allowed_provenance_test =
            current_test == Some("unspanned_triggered_clock_condition_is_rejected");
        if in_test_module && line_has_dummy_span_use(line) && !allowed_provenance_test {
            offenders.push(format!("{}:{}", path.display(), idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "Appendix B validation tests must use file-derived fixture spans except \
in dedicated provenance regressions. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_initial_unit_tests_only_use_dummy_spans_for_provenance_cases() {
    let path = workspace_root().join("crates/rumoca-phase-dae/src/initial.rs");
    let content = fs::read_to_string(&path).expect("read initial tests");
    let mut current_test: Option<&str> = None;
    let mut in_test_module = false;
    let mut offenders = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if trimmed == "mod tests {" {
            in_test_module = true;
        }
        if let Some(fn_name) = trimmed.strip_prefix("fn ") {
            current_test = fn_name.split('(').next();
        }
        let allowed_provenance_test =
            current_test == Some("fixed_start_equation_rejects_missing_default_start_provenance");
        if in_test_module && line_has_dummy_span_use(line) && !allowed_provenance_test {
            offenders.push(format!("{}:{}", path.display(), idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "initial tests must use file-derived fixture spans except in dedicated \
provenance regressions. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_condition_lowering_unit_tests_only_use_dummy_spans_for_provenance_cases() {
    let path = workspace_root().join("crates/rumoca-phase-dae/src/condition_lowering.rs");
    let content = fs::read_to_string(&path).expect("read condition lowering tests");
    let mut current_test: Option<&str> = None;
    let mut in_test_module = false;
    let mut offenders = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if trimmed == "mod tests {" {
            in_test_module = true;
        }
        if let Some(fn_name) = trimmed.strip_prefix("fn ") {
            current_test = fn_name.split('(').next();
        }
        let allowed_provenance_test =
            current_test == Some("canonical_condition_subscript_requires_source_provenance");
        if in_test_module && line_has_dummy_span_use(line) && !allowed_provenance_test {
            offenders.push(format!("{}:{}", path.display(), idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "condition lowering tests must use file-derived fixture spans except in \
dedicated provenance regressions. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_reference_validation_unit_tests_only_use_dummy_spans_for_provenance_cases() {
    let path = workspace_root().join("crates/rumoca-phase-dae/src/reference_validation.rs");
    let content = fs::read_to_string(&path).expect("read reference validation tests");
    let mut current_test: Option<&str> = None;
    let mut in_test_module = false;
    let mut offenders = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if trimmed == "mod tests {" {
            in_test_module = true;
        }
        if let Some(fn_name) = trimmed.strip_prefix("fn ") {
            current_test = fn_name.split('(').next();
        }
        let allowed_provenance_test = matches!(
            current_test,
            Some("component_reference_target_span_uses_context_span_when_component_is_unspanned")
                | Some("component_reference_target_span_rejects_missing_provenance")
        );
        if in_test_module && line_has_dummy_span_use(line) && !allowed_provenance_test {
            offenders.push(format!("{}:{}", path.display(), idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "reference validation tests must use file-derived fixture spans except \
in dedicated provenance regressions. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_current_value_rewrite_tests_only_use_dummy_spans_for_provenance_cases() {
    let path = workspace_root()
        .join("crates/rumoca-phase-dae/src/algorithm_lowering/current_value_rewrite.rs");
    let content = fs::read_to_string(&path).expect("read current value rewrite tests");
    let mut current_test: Option<&str> = None;
    let mut in_test_module = false;
    let mut offenders = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if trimmed == "mod tests {" {
            in_test_module = true;
        }
        if let Some(fn_name) = trimmed.strip_prefix("fn ") {
            current_test = fn_name.split('(').next();
        }
        let allowed_provenance_test =
            current_test == Some("dynamic_current_rewrite_rejects_unspanned_selector");
        if in_test_module && line_has_dummy_span_use(line) && !allowed_provenance_test {
            offenders.push(format!("{}:{}", path.display(), idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "current-value rewrite tests must use file-derived fixture spans except \
in dedicated provenance regressions. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_fold_start_values_unit_tests_only_use_dummy_spans_for_provenance_cases() {
    let path = workspace_root().join("crates/rumoca-phase-dae/src/fold_start_values.rs");
    let content = fs::read_to_string(&path).expect("read fold start values tests");
    let mut current_test: Option<&str> = None;
    let mut in_test_module = false;
    let mut offenders = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if trimmed == "mod tests {" {
            in_test_module = true;
        }
        if let Some(fn_name) = trimmed.strip_prefix("fn ") {
            current_test = fn_name.split('(').next();
        }
        let allowed_provenance_test =
            current_test == Some("folded_start_literal_rejects_missing_provenance");
        if in_test_module && line_has_dummy_span_use(line) && !allowed_provenance_test {
            offenders.push(format!("{}:{}", path.display(), idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "fold-start-values tests must use file-derived fixture spans except in \
dedicated provenance regressions. Offenders: {offenders:#?}"
    );
}
