use super::*;

fn layout_with_bindings(names: &[&str]) -> VarLayout {
    let mut bindings = IndexMap::new();
    for (index, name) in names.iter().enumerate() {
        bindings.insert(
            (*name).to_string(),
            ScalarSlot::Y {
                index,
                byte_offset: index * std::mem::size_of::<f64>(),
            },
        );
    }
    VarLayout::from_parts(bindings, names.len(), 0)
}

#[test]
fn scalarized_record_fields_ignore_only_top_level_nested_suffixes() {
    let layout = layout_with_bindings(&[
        "state.p",
        "state.v[index.with.dot]",
        "state.nested.q",
        "other.p",
    ]);

    let fields = scalarized_record_fields("state", &layout).expect("state fields");
    let suffixes = fields
        .iter()
        .map(|field| field.suffix.as_str())
        .collect::<Vec<_>>();

    assert!(suffixes.contains(&"p"));
    assert!(suffixes.contains(&"v[index.with.dot]"));
    assert!(!suffixes.contains(&"nested.q"));
}

#[test]
fn scalarized_record_fields_ignore_scalar_binding_with_enum_alias_prefix() {
    let layout = layout_with_bindings(&["Th", "Th.default"]);

    assert!(scalarized_record_fields("Th", &layout).is_none());
}

#[test]
fn scalar_row_namespace_rejects_overflow() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_lower_expression_rows_source_19.mo"),
        1,
        4,
    );
    let err = scalar_row_namespace(u64::MAX, 1, span)
        .expect_err("row namespace multiplication should reject overflow");

    assert_eq!(err.source_span(), Some(span));
    assert!(matches!(err, LowerError::ContractViolation { .. }));
}

fn unspanned_expression_rows_test_span() -> rumoca_core::Span {
    rumoca_core::Span::DUMMY
}

#[test]
fn scalar_row_namespace_rejects_overflow_without_dummy_span() {
    let err = scalar_row_namespace(u64::MAX, 1, unspanned_expression_rows_test_span())
        .expect_err("row namespace multiplication should reject overflow");

    assert_eq!(err.source_span(), None);
    assert!(
        err.reason().contains("scalar row namespace overflows"),
        "{err:?}"
    );
}

#[test]
fn row_namespace_rejects_u64_overflow_with_span() {
    let Some(row_idx) = usize::try_from(u64::MAX)
        .ok()
        .and_then(|value| value.checked_add(1))
    else {
        return;
    };
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_lower_expression_rows_source_21.mo"),
        2,
        7,
    );

    let err =
        row_namespace_from_usize(row_idx, Some(span)).expect_err("row namespace must fit in u64");

    assert_eq!(err.source_span(), Some(span));
    assert!(matches!(err, LowerError::ContractViolation { .. }));
    assert!(err.reason().contains("exceeds u64 namespace"));
}

#[test]
fn expression_contract_violation_without_span_stays_unspanned() {
    let err = expression_contract_violation("expression row metadata mismatch", None);

    assert_eq!(err.source_span(), None);
    assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
    assert!(
        err.reason().contains("expression row metadata mismatch"),
        "{err:?}"
    );
}

#[test]
fn generated_subscript_rejects_i64_overflow_with_span() {
    let Some(index) = usize::try_from(i64::MAX)
        .ok()
        .and_then(|value| value.checked_add(1))
    else {
        return;
    };
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_lower_expression_rows_source_20.mo"),
        3,
        8,
    );

    let err = generated_subscript_from_usize(index, span)
        .expect_err("generated subscript must fit in Modelica integer range");

    assert_eq!(err.source_span(), Some(span));
    assert!(matches!(
        err,
        LowerError::ContractViolation { reason, span: actual }
            if actual == span && reason.contains("exceeds i64 range")
    ));
}

#[test]
fn indexed_sample_value_rejects_i64_dimension_overflow_with_span() {
    let Some(dim) = usize::try_from(i64::MAX)
        .ok()
        .and_then(|value| value.checked_add(1))
    else {
        return;
    };
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_lower_expression_rows_source_21.mo"),
        5,
        11,
    );
    let value = rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(1.0),
        span,
    };

    let err = indexed_sample_value(&value, &[dim], 0, span)
        .expect_err("sample array dimension must fit in Modelica integer range");

    assert_eq!(err.source_span(), Some(span));
    assert!(matches!(
        err,
        LowerError::ContractViolation { reason, span: actual }
            if actual == span && reason.contains("sample array dimension")
    ));
}

#[test]
fn expand_row_values_reports_capacity_overflow_with_owner_span() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_lower_expression_rows_source_22.mo"),
        1,
        4,
    );
    let err = match expand_row_values(vec![0], usize::MAX, span) {
        Ok(_) => panic!("oversized expression value expansion should fail before allocating"),
        Err(err) => err,
    };

    assert_eq!(err.source_span(), Some(span));
    assert!(
        err.reason()
            .contains("expanded expression value count capacity"),
        "unexpected error: {err}"
    );
}
