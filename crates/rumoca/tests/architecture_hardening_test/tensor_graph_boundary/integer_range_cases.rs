use super::*;

fn assert_integer_construction_range_contract(construction: &str, tensor_construction: &str) {
    for (source, function, required_operators) in [
        (construction, "unary", &["Negate", "Abs", "Sign"][..]),
        (
            construction,
            "convert",
            &[
                "RealToIntegerTowardZero",
                "RealToIntegerTowardNegativeInfinity",
            ][..],
        ),
        (
            tensor_construction,
            "broadcast_binary",
            &["integer_binary_result_range_is_unproved"][..],
        ),
        (
            tensor_construction,
            "scale",
            &["Multiply", "integer_binary_result_range_is_unproved"][..],
        ),
        (
            tensor_construction,
            "diagonal",
            &["UnprovedIntegerRange", "domain.contains(0)"][..],
        ),
        (
            tensor_construction,
            "identity",
            &[
                "UnprovedIntegerRange",
                "domain.contains(0)",
                "domain.contains(1)",
            ][..],
        ),
    ] {
        let body = rust_function(source, function);
        for operator in required_operators {
            assert!(
                body.contains(operator),
                "TypedProgram::{function} lost the `{operator}` result-range obligation"
            );
        }
        let refusal = body
            .find("UnprovedIntegerRange")
            .unwrap_or_else(|| panic!("TypedProgram::{function} lost its typed range refusal"));
        let destination = body.find("issue_register").unwrap_or_else(|| {
            panic!("TypedProgram::{function} no longer exposes its commit boundary")
        });
        assert!(
            refusal < destination,
            "TypedProgram::{function} must refuse before issuing a destination"
        );
    }

    let binary = rust_function(construction, "binary");
    let binary_proof = rust_function(construction, "integer_binary_result_range_is_unproved");
    for operator in ["Add", "Subtract", "Multiply"] {
        assert!(
            binary_proof.contains(operator),
            "Integer binary construction lost the `{operator}` result-range obligation"
        );
    }
    let refusal = binary
        .find("integer_binary_result_range_is_unproved")
        .expect("TypedProgram::binary lost its construction-owned range proof");
    let destination = binary
        .find("issue_register")
        .expect("TypedProgram::binary no longer exposes its commit boundary");
    assert!(
        refusal < destination && binary.contains("UnprovedIntegerRange"),
        "TypedProgram::binary must refuse an unproved Integer result before issuing a destination"
    );
}

fn assert_integer_sign_range_contract(construction: &str) {
    let sign_proof = rust_function(construction, "integer_unary_result_range_is_unproved");
    for required in [
        "SolveUnaryOperator::Sign",
        "domain.minimum() < 0",
        "domain.contains(-1)",
        "domain.maximum() > 0",
        "domain.contains(1)",
    ] {
        assert!(
            sign_proof.contains(required),
            "Integer Sign construction lost `{required}` image containment evidence"
        );
    }
}

fn assert_optional_real_to_integer_is_repair_free(relative: &str, source: &str) {
    if !relative.contains("rumoca-eval-solve") {
        return;
    }
    let Some(body) = rust_function_if_present(source, "convert_real_to_integer") else {
        return;
    };
    let findings = integer_result_range_runtime_repair_findings(body);
    assert!(
        findings.is_empty(),
        "{relative}::convert_real_to_integer rechecks or repairs a construction-proved conversion: {}",
        findings.join(", ")
    );
}

fn assert_integer_scalar_consumers_are_repair_free(root: &Path) {
    let consumers = [
        (
            "crates/rumoca-eval-solve/src/typed_program/number.rs",
            &[
                "eval_integer_unary",
                "eval_integer_binary",
                "convert_element",
            ][..],
        ),
        (
            "crates/rumoca-exec-cranelift/src/emit/typed_program.rs",
            &["unary_element", "binary_element", "convert_element"][..],
        ),
    ];
    for (relative, functions) in consumers {
        let source = fs::read_to_string(root.join(relative))
            .unwrap_or_else(|error| panic!("read {relative}: {error}"));
        for function in functions {
            let body = rust_function(&source, function);
            let findings = integer_result_range_runtime_repair_findings(body);
            assert!(
                findings.is_empty(),
                "{relative}::{function} rechecks or repairs construction-proved Integer results: {}",
                findings.join(", ")
            );
            let emissions = unproved_integer_runtime_emission_findings(function, body);
            assert!(
                emissions.is_empty(),
                "{relative}::{function} emits an Integer operation whose result range construction refuses: {}",
                emissions.join(", ")
            );
            assert!(
                body.contains("unreachable!")
                    && body.contains("typed-program construction excludes"),
                "{relative}::{function} must make the construction-refused arm explicitly unreachable"
            );
        }
        assert_optional_real_to_integer_is_repair_free(relative, &source);
    }
}

fn assert_integer_tensor_consumers_trust_construction(root: &Path) {
    for (relative, function) in [
        (
            "crates/rumoca-eval-solve/src/typed_program/tensor.rs",
            "eval_diagonal",
        ),
        (
            "crates/rumoca-exec-cranelift/src/emit/typed_program/tensor.rs",
            "lower_diagonal",
        ),
        (
            "crates/rumoca-eval-solve/src/typed_program/tensor.rs",
            "eval_identity",
        ),
        (
            "crates/rumoca-exec-cranelift/src/emit/typed_program/tensor.rs",
            "lower_identity",
        ),
    ] {
        let source = fs::read_to_string(root.join(relative))
            .unwrap_or_else(|error| panic!("read {relative}: {error}"));
        let body = rust_function(&source, function);
        assert!(
            !body.contains("domain.contains("),
            "{relative}::{function} must trust the construction-owned diagonal zero proof"
        );
    }
}

#[test]
fn integer_result_range_evidence_is_construction_owned_and_runtime_has_no_repair_path() {
    let root = workspace_root();
    let construction =
        fs::read_to_string(root.join("crates/rumoca-ir-solve/src/typed_program/program.rs"))
            .expect("read typed-program construction");
    let tensor_construction =
        fs::read_to_string(root.join("crates/rumoca-ir-solve/src/typed_program/program/tensor.rs"))
            .expect("read tensor-program construction");
    assert_integer_construction_range_contract(&construction, &tensor_construction);
    assert_integer_sign_range_contract(&construction);
    assert_integer_scalar_consumers_are_repair_free(&root);
    assert_integer_tensor_consumers_trust_construction(&root);
}

#[test]
fn integer_result_range_runtime_repair_mutations_are_detected() {
    for (mutation, expected) in [
        ("value.checked_neg()", "checked arithmetic"),
        ("i64::checked_add(lhs, rhs)", "checked arithmetic"),
        ("lhs.wrapping_add(rhs)", "wrapping arithmetic"),
        ("value.saturating_mul(rhs)", "saturating arithmetic"),
        ("lhs.overflowing_sub(rhs)", "overflowing arithmetic"),
        ("domain.contains(value)", "runtime domain recheck"),
        ("value.is_finite()", "runtime finite recheck"),
        ("i64::try_from(value)", "fallible runtime conversion"),
        (
            "TypedProgramEvalError::IntegerArithmetic",
            "runtime Integer arithmetic failure",
        ),
        (
            "TypedProgramEvalError::InvalidIntegerConversion",
            "runtime Integer conversion failure",
        ),
    ] {
        assert!(
            integer_result_range_runtime_repair_findings(mutation).contains(&expected),
            "Integer result-range repair mutation escaped: {mutation}"
        );
    }

    for (function, mutation, expected) in [
        ("eval_integer_unary", "-value", "Integer Negate emission"),
        (
            "unary_element",
            "builder.ins().ineg(value)",
            "Integer Negate emission",
        ),
        ("eval_integer_binary", "lhs + rhs", "Integer Add emission"),
        (
            "binary_element",
            "builder.ins().imul(lhs, rhs)",
            "Integer Multiply emission",
        ),
        (
            "convert_element",
            "value as i64",
            "Real-to-Integer emission",
        ),
        (
            "convert_element",
            "fcvt_to_sint(source)",
            "Real-to-Integer emission",
        ),
    ] {
        assert!(
            unproved_integer_runtime_emission_findings(function, mutation).contains(&expected),
            "unproved Integer emission mutation escaped: {mutation}"
        );
    }
}

fn integer_result_range_runtime_repair_findings(source: &str) -> Vec<&'static str> {
    let compact = source
        .chars()
        .filter(|character| !character.is_whitespace())
        .collect::<String>();
    [
        ("checked_", "checked arithmetic"),
        ("wrapping_", "wrapping arithmetic"),
        ("saturating_", "saturating arithmetic"),
        ("overflowing_", "overflowing arithmetic"),
        ("domain.contains(", "runtime domain recheck"),
        (".is_finite()", "runtime finite recheck"),
        ("try_from(", "fallible runtime conversion"),
        (".try_into(", "fallible runtime conversion"),
        (
            "TypedProgramEvalError::IntegerArithmetic",
            "runtime Integer arithmetic failure",
        ),
        (
            "TypedProgramEvalError::InvalidIntegerConversion",
            "runtime Integer conversion failure",
        ),
    ]
    .into_iter()
    .filter_map(|(needle, finding)| compact.contains(needle).then_some(finding))
    .collect()
}

fn unproved_integer_runtime_emission_findings(function: &str, source: &str) -> Vec<&'static str> {
    let compact = source
        .chars()
        .filter(|character| !character.is_whitespace())
        .collect::<String>();
    let needles: &[(&str, &str)] = match function {
        "eval_integer_unary" | "unary_element" => &[
            ("-value", "Integer Negate emission"),
            ("value.abs()", "Integer Abs emission"),
            (".ineg(", "Integer Negate emission"),
        ],
        "eval_integer_binary" | "binary_element" => &[
            ("lhs+rhs", "Integer Add emission"),
            ("lhs-rhs", "Integer Subtract emission"),
            ("lhs*rhs", "Integer Multiply emission"),
            (".iadd(", "Integer Add emission"),
            (".isub(", "Integer Subtract emission"),
            (".imul(", "Integer Multiply emission"),
        ],
        "convert_element" => &[
            ("asi64", "Real-to-Integer emission"),
            ("fcvt_to_sint", "Real-to-Integer emission"),
        ],
        _ => panic!("unclassified Integer result-range consumer `{function}`"),
    };
    needles
        .iter()
        .filter_map(|(needle, finding)| compact.contains(needle).then_some(*finding))
        .collect()
}

#[test]
fn runtime_tensor_indices_have_one_construction_proof_and_clean_direct_consumer_arms() {
    let root = workspace_root();
    let ir = fs::read_to_string(root.join("crates/rumoca-ir-solve/src/linear_op.rs"))
        .expect("read Solve scalar-program construction");
    let proof = rust_function(&ir, "validate_runtime_index_evidence");
    for operation in [
        "LoadIndexedRegister",
        "LoadIndexedFoldCarried",
        "LoadIndexedFoldCapture",
        "TensorUpdate",
        "StoreOutputFoldTensorUpdate",
    ] {
        assert!(
            proof.contains(operation),
            "runtime tensor-index proof omitted `{operation}`"
        );
    }
    assert!(
        proof.contains("lacks a construction-issued exact integer domain")
            && proof.contains("domain escapes its selected axis"),
        "runtime tensor-index proof lost its positive admission and refusal cases"
    );
    for evidence_owner in [
        "struct RegisterProducer",
        "struct ExactIntegerEvidence",
        "struct OneBasedTensorCoordinateEvidence",
    ] {
        assert!(
            ir.contains(evidence_owner),
            "runtime tensor-index proof lost `{evidence_owner}`"
        );
    }

    for &(relative, functions) in RUNTIME_TENSOR_EXECUTION_CONSUMERS {
        let source = fs::read_to_string(root.join(relative))
            .unwrap_or_else(|error| panic!("read {relative}: {error}"));
        for function in functions {
            let body = rust_function(&source, function);
            let audit = runtime_coordinate_audit(body);
            let findings = tensor_index_fallback_findings(&audit.source);
            assert!(
                findings.is_empty(),
                "{relative}::{function} rechecks or substitutes an invalid runtime tensor index: {}",
                findings.join(", ")
            );
        }
    }
}

#[test]
fn runtime_tensor_coordinate_consumer_inventory_is_exact() {
    let root = workspace_root();
    let actual = runtime_tensor_coordinate_sites(&workspace_production_sources(&root));
    let expected = BTreeSet::from([
        "crates/rumoca-eval-solve/src/compute_block_scalarize/dense.rs::max_reg_in_op".to_string(),
        "crates/rumoca-eval-solve/src/lib.rs::push_lazy_dependencies".to_string(),
        "crates/rumoca-eval-solve/src/lib.rs::push_lazy_tensor_update_dependencies".to_string(),
        "crates/rumoca-eval-solve/src/lib.rs::tensor_index_coordinate".to_string(),
        "crates/rumoca-eval-solve/src/lib.rs::tensor_update_register_value_offset".to_string(),
        "crates/rumoca-exec-cranelift/src/emit.rs::constant_tensor_coordinate".to_string(),
        "crates/rumoca-exec-cranelift/src/emit.rs::create_fold_register_tape".to_string(),
        "crates/rumoca-exec-cranelift/src/emit.rs::lower_fold_tensor_update".to_string(),
        "crates/rumoca-exec-cranelift/src/emit.rs::lower_tensor_offset".to_string(),
        "crates/rumoca-exec-cranelift/src/emit.rs::lower_tensor_update".to_string(),
        "crates/rumoca-exec-cranelift/src/emit.rs::max_reg_index".to_string(),
        "crates/rumoca-exec-cranelift/src/emit.rs::validate_row_sources".to_string(),
        "crates/rumoca-exec-cranelift/src/emit/interpreter.rs::execute_general_op".to_string(),
        "crates/rumoca-exec-cranelift/src/emit/interpreter.rs::tensor_index_coordinate".to_string(),
        "crates/rumoca-ir-solve/src/linear_op.rs::fold_tensor_subscript".to_string(),
        "crates/rumoca-ir-solve/src/linear_op.rs::indexed_fold_runtime_indices".to_string(),
        "crates/rumoca-ir-solve/src/linear_op.rs::indexed_register".to_string(),
        "crates/rumoca-ir-solve/src/linear_op.rs::tensor_update_subscript".to_string(),
        "crates/rumoca-ir-solve/src/linear_op.rs::validate_runtime_index_evidence".to_string(),
        "crates/rumoca-ir-solve/src/linear_op.rs::validate_tensor_update_index_domains".to_string(),
        "crates/rumoca-ir-solve/src/linear_op.rs::validate_tensor_update_slice_index_domains"
            .to_string(),
        "crates/rumoca-ir-solve/src/structural_pattern.rs::tensor_update_patch".to_string(),
        "crates/rumoca-ir-solve/src/structural_pattern.rs::union_fold_tensor_update".to_string(),
        "crates/rumoca-ir-solve/src/structural_pattern.rs::union_runtime_indices".to_string(),
        "crates/rumoca-phase-codegen/src/codegen/render_solve.rs::render_solve_op_typed"
            .to_string(),
        "crates/rumoca-phase-solve/src/ad.rs::lower_load_indexed_fold_capture".to_string(),
        "crates/rumoca-phase-solve/src/ad.rs::lower_load_indexed_fold_carried".to_string(),
        "crates/rumoca-phase-solve/src/ad.rs::lower_load_indexed_register".to_string(),
        "crates/rumoca-phase-solve/src/ad.rs::lower_store_fold_tensor_update".to_string(),
        "crates/rumoca-phase-solve/src/ad.rs::lower_tensor_update".to_string(),
        "crates/rumoca-phase-solve/src/lower/scalar/arrays.rs::dynamic_index_subscript".to_string(),
        "crates/rumoca-phase-solve/src/lower/scalar/arrays.rs::dynamic_scalar_index".to_string(),
        "crates/rumoca-phase-solve/src/lower/scalar/arrays.rs::pack_update_subscript".to_string(),
        "crates/rumoca-phase-solve/src/lower/scalar/functions.rs::compact_fold_tensor_update"
            .to_string(),
        "crates/rumoca-phase-solve/src/lower/scalar/functions.rs::dynamic_indexed_record_field"
            .to_string(),
        "crates/rumoca-phase-solve/src/lower/scalar/functions.rs::emit_fold_tensor_update_plan"
            .to_string(),
    ]);
    assert_eq!(
        actual, expected,
        "runtime tensor-coordinate producer/consumer inventory changed; review every added, moved, or removed site before updating the ledger"
    );

    let template_consumers = runtime_tensor_coordinate_template_consumers(&root);
    assert!(
        template_consumers.is_empty(),
        "templates must not interpret runtime tensor-coordinate IR: {template_consumers:?}"
    );
}
