use super::*;

fn assert_miette_template_span(err: &crate::errors::CodegenError) {
    use miette::Diagnostic;

    assert_eq!(
        err.code().map(|code| code.to_string()),
        Some("rumoca::codegen::EC002".to_string()),
        "expected miette template render diagnostic, got: {err}"
    );
    match err {
        crate::errors::CodegenError::TemplateRenderError { span, .. } => {
            assert!(
                !span.is_empty(),
                "expected non-empty source span for template render diagnostic: {err}"
            );
        }
        other => panic!("expected TemplateRenderError with source span, got: {other:?}"),
    }
}

#[test]
fn test_render_error_contains_context() {
    // Verify that errors from custom functions propagate with context
    let dae = dae::Dae::new();
    // Use a template that calls render_expr with invalid data
    let template = r#"{{ render_expr(none, {}) }}"#;
    let err = render_template(&dae, template).unwrap_err();
    let msg = format!("{err}");
    assert!(
        msg.contains("template") || msg.contains("error"),
        "error should contain diagnostic info, got: {msg}"
    );
}

#[test]
fn test_dae_template_missing_solve_field_fails_under_strict_undefined() {
    let dae = dae::Dae::new();
    let err = render_template(&dae, "{{ solve.continuous }}")
        .expect_err("missing solve fields in DAE-only context must fail");

    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("undefined") || msg.contains("continuous"),
        "expected strict missing solve field diagnostic, got: {msg}"
    );
}

#[test]
fn test_render_if_rejects_unrenderable_else_branch() {
    let dae = dae::Dae::new();
    let template = r#"
{{ render_expr({
    "If": {
        "branches": [
            [
                {"Literal": {"value": {"Boolean": true}}},
                {"Literal": {"value": {"Real": 1.0}}}
            ]
        ],
        "else_branch": {"UnsupportedElseBranch": true}
    }
}, {}) }}
"#;
    let err =
        render_template(&dae, template).expect_err("unrenderable else branch must be reported");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("If expression invalid else_branch") && msg.contains("UnsupportedElseBranch"),
        "expected strict If else-branch diagnostic, got: {msg}"
    );
}

#[test]
fn test_render_if_rejects_malformed_branch() {
    let dae = dae::Dae::new();
    let template = r#"
{{ render_expr({
    "If": {
        "branches": [
            [
                {"Literal": {"value": {"Boolean": true}}}
            ]
        ],
        "else_branch": {"Literal": {"value": {"Real": 0.0}}}
    }
}, {}) }}
"#;
    let err = render_template(&dae, template).expect_err("malformed If branch must be reported");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("If expression branch") && msg.contains("missing value"),
        "expected strict If branch diagnostic, got: {msg}"
    );
}

#[test]
fn test_render_var_ref_requires_name() {
    let dae = dae::Dae::new();
    let template = r#"
{{ render_expr({"VarRef": {"subscripts": []}}, {}) }}
"#;
    let err = render_template(&dae, template).expect_err("missing VarRef name must fail");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("VarRef missing 'name' field"),
        "expected strict VarRef name diagnostic, got: {msg}"
    );
}

#[test]
fn test_render_var_ref_unwraps_spanned_literal_subscripts() {
    let dae = dae::Dae::new();
    let template = r#"
{{ render_expr({
    "VarRef": {
        "name": "R",
        "subscripts": [
            {"Expr": {"Literal": {"value": {"Integer": {"value": 1, "span": {"source": 1, "start": 2, "end": 3}}}}}},
            {"Expr": {"Literal": {"value": {"Integer": {"value": 2, "span": {"source": 1, "start": 4, "end": 5}}}}}}
        ]
    }
}, {"subscript_underscore": true}) }}
"#;
    let rendered = render_template(&dae, template).expect("spanned subscripts should render");
    assert_eq!(rendered.trim(), "R_1_2");
}

#[test]
fn test_render_builtin_requires_function_name() {
    let dae = dae::Dae::new();
    let template = r#"
{{ render_expr({"BuiltinCall": {"args": []}}, {}) }}
"#;
    let err = render_template(&dae, template).expect_err("missing BuiltinCall function must fail");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("BuiltinCall missing 'function' field"),
        "expected strict BuiltinCall function diagnostic, got: {msg}"
    );
}

#[test]
fn test_render_builtin_wrapper_requires_required_argument() {
    let dae = dae::Dae::new();
    let template = r#"
{{ render_expr({"BuiltinCall": {"function": "Previous", "args": []}}, {}) }}
"#;
    let err = render_template(&dae, template).expect_err("missing wrapper arg must fail");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("BuiltinCall Previous missing required argument 0"),
        "expected strict builtin wrapper diagnostic, got: {msg}"
    );
}

#[test]
fn test_render_smooth_requires_expression_argument() {
    let dae = dae::Dae::new();
    let template = r#"
{{ render_expr({
    "BuiltinCall": {
        "function": "Smooth",
        "args": [{"Literal": {"value": {"Integer": 1}}}]
    }
}, {}) }}
"#;
    let err = render_template(&dae, template).expect_err("missing smooth expression must fail");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("BuiltinCall Smooth missing required argument 1"),
        "expected strict smooth diagnostic, got: {msg}"
    );
}

#[test]
fn test_render_inferred_clock_sample_renders_sampled_value() {
    let dae = dae::Dae::new();
    let template = r#"
{{ render_expr({
    "BuiltinCall": {
        "function": "Sample",
        "args": [{"VarRef": {"name": "sampled.u", "subscripts": []}}]
    }
}, {}) }}
"#;
    let out = render_template(&dae, template).expect("sample(u) should render sampled value");
    assert!(
        out.contains("sampled_u"),
        "expected sample(u) to render its sampled expression, got: {out}"
    );
}

#[test]
fn test_render_sample_requires_argument() {
    let dae = dae::Dae::new();
    let template = r#"
{{ render_expr({
    "BuiltinCall": {
        "function": "Sample",
        "args": []
    }
}, {}) }}
"#;
    let err = render_template(&dae, template).expect_err("empty sample call must fail");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("BuiltinCall Sample missing required argument 0"),
        "expected strict sample diagnostic, got: {msg}"
    );
}

#[test]
fn test_render_periodic_sample_requires_prior_lowering() {
    let dae = dae::Dae::new();
    let template = r#"
{{ render_expr({
    "BuiltinCall": {
        "function": "Sample",
        "args": [
            {"Literal": {"value": {"Real": 0.0}}},
            {"Literal": {"value": {"Real": 0.1}}}
        ]
    }
}, {}) }}
"#;
    let err = render_template(&dae, template)
        .expect_err("periodic sample call must be lowered before rendering");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains(
            "BuiltinCall Sample with 2 arguments must be lowered before template rendering"
        ),
        "expected strict periodic sample diagnostic, got: {msg}"
    );
}

#[test]
fn test_render_function_call_requires_name() {
    let dae = dae::Dae::new();
    let template = r#"
{{ render_expr({"FunctionCall": {"args": []}}, {}) }}
"#;
    let err = render_template(&dae, template).expect_err("missing FunctionCall name must fail");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("FunctionCall missing 'name' field"),
        "expected strict FunctionCall name diagnostic, got: {msg}"
    );
}

#[test]
fn test_render_function_args_reject_unrenderable_item() {
    let dae = dae::Dae::new();
    let template = r#"
{{ render_expr({
    "FunctionCall": {
        "name": "f",
        "args": [{"UnsupportedFunctionExpressionArg": true}]
    }
}, {}) }}
"#;
    let err = render_template(&dae, template).expect_err("unrenderable function arg must fail");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("UnsupportedFunctionExpressionArg"),
        "expected strict function-argument diagnostic, got: {msg}"
    );
}

#[test]
fn test_render_equation_rejects_unrenderable_explicit_rhs() {
    let dae = dae::Dae::new();
    let template = r#"
{{ render_equation({
    "lhs": "x",
    "rhs": {"UnsupportedExplicitEquationRhs": true}
}, {}) }}
"#;
    let err = render_template(&dae, template)
        .expect_err("unrenderable explicit equation RHS must be reported");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("explicit equation RHS failed to render")
            && msg.contains("UnsupportedExplicitEquationRhs"),
        "expected strict explicit-equation diagnostic, got: {msg}"
    );
}

#[test]
fn test_render_equation_rejects_unrenderable_residual_rhs() {
    let dae = dae::Dae::new();
    let template = r#"
{{ render_equation({
    "rhs": {
        "Binary": {
            "op": "Sub",
            "lhs": {"VarRef": {"name": "x", "subscripts": []}},
            "rhs": {"UnsupportedResidualEquationRhs": true}
        }
    }
}, {}) }}
"#;
    let err = render_template(&dae, template)
        .expect_err("unrenderable residual equation RHS must be reported");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("residual equation RHS failed to render")
            && msg.contains("UnsupportedResidualEquationRhs"),
        "expected strict residual-equation diagnostic, got: {msg}"
    );
}

#[test]
fn test_render_equation_rejects_missing_rhs() {
    let dae = dae::Dae::new();
    let template = r#"
{{ render_equation({
    "lhs": "x"
}, {}) }}
"#;
    let err = render_template(&dae, template).expect_err("missing equation RHS must fail");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("explicit equation RHS failed to render")
            || msg.contains("equation missing required `rhs` field"),
        "expected strict missing-equation-RHS diagnostic, got: {msg}"
    );
}

#[test]
fn test_render_flat_equation_rejects_unrenderable_residual_rhs() {
    let dae = dae::Dae::new();
    let template = r#"
{{ render_flat_equation({
    "residual": {
        "Binary": {
            "op": "Sub",
            "lhs": {"VarRef": {"name": "x", "subscripts": []}},
            "rhs": {"UnsupportedFlatResidualEquationRhs": true}
        }
    }
}, {}) }}
"#;
    let err = render_template(&dae, template)
        .expect_err("unrenderable flat residual equation RHS must be reported");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("flat residual equation RHS failed to render")
            && msg.contains("UnsupportedFlatResidualEquationRhs"),
        "expected strict flat residual-equation diagnostic, got: {msg}"
    );
}

#[test]
fn test_render_flat_equation_rejects_missing_residual() {
    let dae = dae::Dae::new();
    let template = r#"
{{ render_flat_equation({}, {}) }}
"#;
    let err = render_template(&dae, template).expect_err("missing flat residual must fail");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("flat equation missing required `residual` field"),
        "expected strict missing-flat-residual diagnostic, got: {msg}"
    );
}

#[test]
fn test_render_matmul_c_rejects_missing_lhs_sparsity() {
    let dae = dae::Dae::new();
    let template = r#"
{{ render_matmul_c({
    "lhs_ops": [],
    "lhs_start": 0,
    "rhs_ops": [],
    "rhs_start": 0,
    "m": 1,
    "k": 1,
    "n": 1
}, 0, {}) }}
"#;
    let err = render_template(&dae, template).expect_err("missing MatMul lhs_sparsity must fail");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("MatMul missing lhs_sparsity"),
        "expected strict MatMul C diagnostic, got: {msg}"
    );
}

#[test]
fn test_render_matmul_mlir_rejects_missing_lhs_sparsity() {
    let dae = dae::Dae::new();
    let template = r#"
{{ render_matmul_mlir({
    "lhs_ops": [],
    "lhs_start": 0,
    "rhs_ops": [],
    "rhs_start": 0,
    "m": 1,
    "k": 1,
    "n": 1
}, 0, 0) }}
"#;
    let err = render_template(&dae, template).expect_err("missing MatMul lhs_sparsity must fail");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("MatMul missing lhs_sparsity"),
        "expected strict MatMul MLIR diagnostic, got: {msg}"
    );
}

#[test]
fn test_render_matmul_c_rejects_malformed_explicit_sparsity() {
    let dae = dae::Dae::new();
    let template = r#"
{{ render_matmul_c({
    "lhs_ops": [],
    "lhs_start": 0,
    "rhs_ops": [],
    "rhs_start": 0,
    "m": 1,
    "k": 1,
    "n": 1,
    "lhs_sparsity": {"Explicit": {}}
}, 0, {}) }}
"#;
    let err = render_template(&dae, template).expect_err("malformed Explicit sparsity must fail");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("MatMul Explicit sparsity missing nnz"),
        "expected strict MatMul C Explicit diagnostic, got: {msg}"
    );
}

#[test]
fn test_render_matmul_mlir_rejects_malformed_explicit_sparsity() {
    let dae = dae::Dae::new();
    let template = r#"
{{ render_matmul_mlir({
    "lhs_ops": [],
    "lhs_start": 0,
    "rhs_ops": [],
    "rhs_start": 0,
    "m": 1,
    "k": 1,
    "n": 1,
    "lhs_sparsity": {"Explicit": {}}
}, 0, 0) }}
"#;
    let err = render_template(&dae, template).expect_err("malformed Explicit sparsity must fail");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("MatMul Explicit sparsity missing nnz"),
        "expected strict MatMul MLIR Explicit diagnostic, got: {msg}"
    );
}

#[test]
fn test_render_matmul_c_rejects_oversized_output_count() {
    let dae = dae::Dae::new();
    let template = r#"
{{ render_matmul_c({
    "lhs_ops": [],
    "lhs_start": 0,
    "rhs_ops": [],
    "rhs_start": 0,
    "m": 1000001,
    "k": 1,
    "n": 1,
    "lhs_sparsity": "Dense"
}, 0, {}) }}
"#;
    let err = render_template(&dae, template).expect_err("oversized MatMul output must fail");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("MatMul output count (1000001) exceeds render enumeration limit 1000000"),
        "expected strict MatMul C output-count diagnostic, got: {msg}"
    );
}

#[test]
fn test_render_matmul_mlir_rejects_oversized_output_count() {
    let dae = dae::Dae::new();
    let template = r#"
{{ render_matmul_mlir({
    "lhs_ops": [],
    "lhs_start": 0,
    "rhs_ops": [],
    "rhs_start": 0,
    "m": 1000001,
    "k": 1,
    "n": 1,
    "lhs_sparsity": "Dense"
}, 0, 0) }}
"#;
    let err = render_template(&dae, template).expect_err("oversized MatMul output must fail");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("MatMul output count (1000001) exceeds render enumeration limit 1000000"),
        "expected strict MatMul MLIR output-count diagnostic, got: {msg}"
    );
}

#[test]
fn test_render_linsolve_mlir_rejects_oversized_matrix_count() {
    let dae = dae::Dae::new();
    let template = r#"
{{ render_linsolve_mlir({
    "setup_ops": [],
    "matrix_start": 0,
    "rhs_start": 0,
    "n": 1001
}, 0, 0) }}
"#;
    let err = render_template(&dae, template).expect_err("oversized LinSolve matrix must fail");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains(
            "LinSolve matrix element count (1002001) exceeds render enumeration limit 1000000"
        ),
        "expected strict LinSolve MLIR matrix-count diagnostic, got: {msg}"
    );
}

#[test]
fn test_render_solve_row_c_rejects_oversized_linear_solve_component() {
    let template = r#"{{ render_solve_row_c(dae.row, {}) }}"#;
    let rendered = render_template_with_dae_json(
        &serde_json::json!({
            "row": [{
                "LinearSolveComponent": {
                    "dst": 0,
                    "matrix_start": 0,
                    "rhs_start": 0,
                    "n": 1001,
                    "component": 0
                }
            }]
        }),
        template,
    );
    let err = rendered.expect_err("oversized LinearSolveComponent matrix must fail");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains(
            "LinSolve matrix element count (1002001) exceeds render enumeration limit 1000000"
        ),
        "expected strict LinearSolveComponent matrix-count diagnostic, got: {msg}"
    );
}

#[test]
fn test_ode_rhs_rejects_unrenderable_matched_rhs() {
    let dae_json = serde_json::json!({
        "f_x": [{
            "rhs": {
                "Binary": {
                    "op": "Sub",
                    "lhs": {
                        "BuiltinCall": {
                            "function": "Der",
                            "args": [{
                                "VarRef": {
                                    "name": "x",
                                    "subscripts": []
                                }
                            }]
                        }
                    },
                    "rhs": {
                        "UnsupportedExpression": true
                    }
                }
            }
        }]
    });
    let template = r#"
{% set cfg = {"power": "pow", "subscript_underscore": true} %}
{{ ode_rhs(dae.f_x[0], cfg) }}
"#;
    let err = render_template_with_dae_json(&dae_json, template)
        .expect_err("matched ODE residual RHS render failure must be reported");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("unhandled Expression variant") && msg.contains("UnsupportedExpression"),
        "expected unrenderable RHS diagnostic, got: {msg}"
    );
}

#[test]
fn test_ode_rhs_rejects_missing_rhs() {
    let dae = dae::Dae::new();
    let template = r#"
{{ ode_rhs({}, {}) }}
"#;
    let err = render_template(&dae, template).expect_err("missing ode_rhs RHS must fail");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("ode_rhs equation missing required `rhs`/`residual` field"),
        "expected strict ode_rhs missing-RHS diagnostic, got: {msg}"
    );
}

#[test]
fn test_ode_rhs_for_state_rejects_unrenderable_matched_rhs() {
    let dae_json = serde_json::json!({
        "f_x": [{
            "rhs": {
                "Binary": {
                    "op": "Sub",
                    "lhs": {
                        "BuiltinCall": {
                            "function": "Der",
                            "args": [{
                                "VarRef": {
                                    "name": "x",
                                    "subscripts": []
                                }
                            }]
                        }
                    },
                    "rhs": {
                        "UnsupportedExpression": true
                    }
                }
            }
        }]
    });
    let template = r#"
{% set cfg = {"power": "pow", "subscript_underscore": true} %}
{{ ode_rhs_for_state("x", dae.f_x, cfg) }}
"#;
    let err = render_template_with_dae_json(&dae_json, template)
        .expect_err("matched state ODE RHS render failure must be reported");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("unhandled Expression variant") && msg.contains("UnsupportedExpression"),
        "expected unrenderable state RHS diagnostic, got: {msg}"
    );
}

#[test]
fn test_ode_rhs_for_state_rejects_unrenderable_derivative_coefficient() {
    let dae_json = serde_json::json!({
        "f_x": [{
            "rhs": {
                "Binary": {
                    "op": "Sub",
                    "lhs": {
                        "Binary": {
                            "op": "Mul",
                            "lhs": {
                                "UnsupportedCoefficient": true
                            },
                            "rhs": {
                                "BuiltinCall": {
                                    "function": "Der",
                                    "args": [{
                                        "VarRef": {
                                            "name": "x",
                                            "subscripts": []
                                        }
                                    }]
                                }
                            }
                        }
                    },
                    "rhs": {
                        "Literal": {
                            "value": {
                                "Real": 1.0
                            }
                        }
                    }
                }
            }
        }]
    });
    let template = r#"
{% set cfg = {"power": "pow", "subscript_underscore": true} %}
{{ ode_rhs_for_state("x", dae.f_x, cfg) }}
"#;
    let err = render_template_with_dae_json(&dae_json, template)
        .expect_err("matched derivative coefficient render failure must be reported");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("unhandled Expression variant") && msg.contains("UnsupportedCoefficient"),
        "expected unrenderable derivative coefficient diagnostic, got: {msg}"
    );
}

#[test]
fn test_ode_rhs_for_state_rejects_unrenderable_linear_solve_matrix() {
    let dae_json = serde_json::json!({
        "f_x": [{
            "rhs": {
                "Binary": {
                    "op": "Sub",
                    "lhs": {
                        "Binary": {
                            "op": "Mul",
                            "lhs": {
                                "UnsupportedMatrix": true
                            },
                            "rhs": {
                                "BuiltinCall": {
                                    "function": "Der",
                                    "args": [{
                                        "VarRef": {
                                            "name": "omega",
                                            "subscripts": []
                                        }
                                    }]
                                }
                            }
                        }
                    },
                    "rhs": {
                        "VarRef": {
                            "name": "M_body",
                            "subscripts": []
                        }
                    }
                }
            },
            "scalar_count": 2
        }]
    });
    let template = r#"
{% set cfg = {"power": "pow", "subscript_underscore": true} %}
{{ ode_rhs_for_state("omega[1]", dae.f_x, cfg) }}
"#;
    let err = render_template_with_dae_json(&dae_json, template)
        .expect_err("matched matrix-vector derivative solve render failure must be reported");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("unhandled Expression variant") && msg.contains("UnsupportedMatrix"),
        "expected unrenderable linear-solve matrix diagnostic, got: {msg}"
    );
}

fn der_omega_json(index: i64) -> serde_json::Value {
    serde_json::json!({
        "BuiltinCall": {
            "function": "Der",
            "args": [{
                "VarRef": {
                    "name": "omega",
                    "subscripts": [{"Index": index}]
                }
            }]
        }
    })
}

#[test]
fn test_ode_rhs_for_state_rejects_unrenderable_scalarized_linear_coefficient() {
    let dae_json = serde_json::json!({
        "f_x": [
            {
                "rhs": {
                    "Binary": {
                        "op": "Sub",
                        "lhs": {
                            "Binary": {
                                "op": "Add",
                                "lhs": {
                                    "Binary": {
                                        "op": "Mul",
                                        "lhs": {"UnsupportedCoefficient": true},
                                        "rhs": der_omega_json(1)
                                    }
                                },
                                "rhs": {
                                    "Binary": {
                                        "op": "Mul",
                                        "lhs": {
                                            "VarRef": {
                                                "name": "J12",
                                                "subscripts": []
                                            }
                                        },
                                        "rhs": der_omega_json(2)
                                    }
                                }
                            }
                        },
                        "rhs": {
                            "VarRef": {
                                "name": "M1",
                                "subscripts": []
                            }
                        }
                    }
                }
            },
            {
                "rhs": {
                    "Binary": {
                        "op": "Sub",
                        "lhs": {
                            "Binary": {
                                "op": "Add",
                                "lhs": {
                                    "Binary": {
                                        "op": "Mul",
                                        "lhs": {
                                            "VarRef": {
                                                "name": "J21",
                                                "subscripts": []
                                            }
                                        },
                                        "rhs": der_omega_json(1)
                                    }
                                },
                                "rhs": {
                                    "Binary": {
                                        "op": "Mul",
                                        "lhs": {
                                            "VarRef": {
                                                "name": "J22",
                                                "subscripts": []
                                            }
                                        },
                                        "rhs": der_omega_json(2)
                                    }
                                }
                            }
                        },
                        "rhs": {
                            "VarRef": {
                                "name": "M2",
                                "subscripts": []
                            }
                        }
                    }
                }
            }
        ]
    });
    let template = r#"
{% set cfg = {"power": "pow", "subscript_underscore": true} %}
{{ ode_rhs_for_state("omega[2]", dae.f_x, cfg) }}
"#;
    let err = render_template_with_dae_json(&dae_json, template)
        .expect_err("matched scalarized derivative solve render failure must be reported");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("unhandled Expression variant") && msg.contains("UnsupportedCoefficient"),
        "expected unrenderable scalarized coefficient diagnostic, got: {msg}"
    );
}

#[test]
fn test_alg_rhs_rejects_unrenderable_direct_assignment_rhs() {
    let dae_json = serde_json::json!({
        "f_x": [{
            "lhs": "y",
            "rhs": {
                "UnsupportedAlgebraicAssignment": true
            }
        }]
    });
    let template = r#"
{% set cfg = {"power": "pow", "subscript_underscore": true} %}
{{ alg_rhs_for_var("y", dae.f_x, cfg) }}
"#;
    let err = render_template_with_dae_json(&dae_json, template)
        .expect_err("matched algebraic assignment RHS render failure must be reported");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("unhandled Expression variant")
            && msg.contains("UnsupportedAlgebraicAssignment"),
        "expected unrenderable algebraic assignment diagnostic, got: {msg}"
    );
}

#[test]
fn test_alg_rhs_rejects_unrenderable_subtraction_rhs() {
    let dae_json = serde_json::json!({
        "f_x": [{
            "rhs": {
                "Binary": {
                    "op": "Sub",
                    "lhs": {
                        "VarRef": {
                            "name": "y",
                            "subscripts": []
                        }
                    },
                    "rhs": {
                        "UnsupportedAlgebraicResidual": true
                    }
                }
            }
        }]
    });
    let template = r#"
{% set cfg = {"power": "pow", "subscript_underscore": true} %}
{{ alg_rhs_for_var("y", dae.f_x, cfg) }}
"#;
    let err = render_template_with_dae_json(&dae_json, template)
        .expect_err("matched algebraic residual RHS render failure must be reported");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("unhandled Expression variant")
            && msg.contains("UnsupportedAlgebraicResidual"),
        "expected unrenderable algebraic residual diagnostic, got: {msg}"
    );
}

#[test]
fn test_alg_rhs_rejects_unrenderable_additive_term() {
    let dae_json = serde_json::json!({
        "f_x": [{
            "rhs": {
                "Binary": {
                    "op": "Add",
                    "lhs": {
                        "VarRef": {
                            "name": "y",
                            "subscripts": []
                        }
                    },
                    "rhs": {
                        "UnsupportedAdditiveTerm": true
                    }
                }
            }
        }]
    });
    let template = r#"
{% set cfg = {"power": "pow", "subscript_underscore": true} %}
{{ alg_rhs_for_var("y", dae.f_x, cfg) }}
"#;
    let err = render_template_with_dae_json(&dae_json, template)
        .expect_err("matched additive algebraic term render failure must be reported");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("unhandled Expression variant") && msg.contains("UnsupportedAdditiveTerm"),
        "expected unrenderable additive algebraic diagnostic, got: {msg}"
    );
}

#[test]
fn test_alg_rhs_rejects_malformed_array_rhs_builtin_wrapper() {
    let dae_json = serde_json::json!({
        "f_x": [{
            "lhs": {
                "VarRef": {
                    "name": "v",
                    "subscripts": []
                }
            },
            "rhs": {
                "BuiltinCall": {
                    "function": "Previous",
                    "args": []
                }
            }
        }]
    });
    let template = r#"
{% set cfg = {"power": "pow", "subscript_underscore": true} %}
{{ alg_rhs_for_var("v[1]", dae.f_x, cfg) }}
"#;
    let err = render_template_with_dae_json(&dae_json, template)
        .expect_err("array RHS builtin wrapper render failure must be reported");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("BuiltinCall Previous missing required argument 0"),
        "expected strict array RHS builtin wrapper diagnostic, got: {msg}"
    );
}

#[test]
fn test_discrete_rhs_rejects_unrenderable_matched_rhs() {
    let dae_json = serde_json::json!({
        "f_z": [{
            "lhs": "z",
            "rhs": {
                "UnsupportedDiscreteUpdate": true
            }
        }],
        "f_m": []
    });
    let template = r#"
{% set cfg = {"power": "pow", "subscript_underscore": true} %}
{{ discrete_rhs_for_var("z", dae.f_z, dae.f_m, dae, cfg) }}
"#;
    let err = render_template_with_dae_json(&dae_json, template)
        .expect_err("matched discrete RHS render failure must be reported");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("unhandled Expression variant") && msg.contains("UnsupportedDiscreteUpdate"),
        "expected unrenderable discrete RHS diagnostic, got: {msg}"
    );
}

#[test]
fn test_template_undefined_field_fails_fast() {
    let dae = dae::Dae::new();
    let template = "{% for x in dae.missing_field %}{{ x }}{% endfor %}";
    let err = render_template(&dae, template).expect_err("missing field must fail");
    let msg = format!("{err}");
    assert!(
        msg.contains("missing_field") || msg.contains("undefined"),
        "expected undefined-field error, got: {msg}"
    );
}

#[test]
fn test_template_missing_assignment_target_fails_fast() {
    let dae = dae::Dae::new();
    let template = r#"
{% set stmt = {"Assignment": {"value": {"Literal": {"Real": 1.0}}}} %}
{{ render_statement(stmt, {"if_style": "modelica"}, "") }}
"#;
    let err = render_template(&dae, template).expect_err("missing assignment target must fail");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("Assignment missing 'comp' field")
            || msg.contains("target resolved to empty component reference"),
        "expected strict assignment error, got: {msg}"
    );
}

#[test]
fn test_render_statement_function_call_requires_target() {
    let dae = dae::Dae::new();
    let template = r#"
{% set stmt = {"FunctionCall": {"args": []}} %}
{{ render_statement(stmt, {"if_style": "modelica"}, "") }}
"#;
    let err = render_template(&dae, template).expect_err("missing function target must fail");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("FunctionCall statement missing 'comp'")
            || msg.contains("target resolved to empty component reference"),
        "expected strict function-call target error, got: {msg}"
    );
}

#[test]
fn test_render_statement_function_call_rejects_unrenderable_arg() {
    let dae = dae::Dae::new();
    let template = r#"
{% set stmt = {"FunctionCall": {"comp": "f", "args": [{"UnsupportedFunctionArg": true}]}} %}
{{ render_statement(stmt, {"if_style": "modelica"}, "") }}
"#;
    let err = render_template(&dae, template).expect_err("unrenderable function arg must fail");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("FunctionCall statement args failed to render")
            && msg.contains("UnsupportedFunctionArg"),
        "expected strict function-call arg error, got: {msg}"
    );
}

#[test]
fn test_render_statement_reinit_requires_value() {
    let dae = dae::Dae::new();
    let template = r#"
{% set stmt = {"Reinit": {"variable": "x"}} %}
{{ render_statement(stmt, {"if_style": "modelica"}, "") }}
"#;
    let err = render_template(&dae, template).expect_err("missing reinit value must fail");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("Reinit value failed to render"),
        "expected strict reinit value error, got: {msg}"
    );
}

#[test]
fn test_render_statement_assert_requires_condition() {
    let dae = dae::Dae::new();
    let template = r#"
{% set stmt = {"Assert": {"message": {"Terminal": {"terminal_type": "String", "token": {"text": "bad"}}}}} %}
{{ render_statement(stmt, {"if_style": "modelica"}, "") }}
"#;
    let err = render_template(&dae, template).expect_err("missing assert condition must fail");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("Assert condition failed to render"),
        "expected strict assert condition error, got: {msg}"
    );
}

#[test]
fn test_render_ast_if_requires_then_branch() {
    let dae = dae::Dae::new();
    let template = r#"
{% set stmt = {"Reinit": {
    "variable": "x",
    "value": {
        "If": {
            "cond": {"Literal": {"value": {"Boolean": true}}},
            "else_expr": {"Literal": {"value": {"Real": 0.0}}}
        }
    }
}} %}
{{ render_statement(stmt, {"if_style": "modelica"}, "") }}
"#;
    let err = render_template(&dae, template).expect_err("missing AST If then branch must fail");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("AST If expression then branch failed to render"),
        "expected strict AST If diagnostic, got: {msg}"
    );
}

#[test]
fn test_render_ast_function_call_requires_name() {
    let dae = dae::Dae::new();
    let template = r#"
{% set stmt = {"Assert": {"condition": {"FunctionCall": {"args": []}}}} %}
{{ render_statement(stmt, {"if_style": "modelica"}, "") }}
"#;
    let err = render_template(&dae, template).expect_err("missing AST function name must fail");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("AST FunctionCall missing 'name'")
            || msg.contains("name resolved to empty component reference"),
        "expected strict AST function-call diagnostic, got: {msg}"
    );
}

#[test]
fn test_render_ast_named_argument_requires_value() {
    let dae = dae::Dae::new();
    let template = r#"
{% set stmt = {"Assert": {"condition": {
    "FunctionCall": {
        "name": "f",
        "args": [
            {"NamedArgument": {"name": {"text": "x"}}}
        ]
    }
}}} %}
{{ render_statement(stmt, {"if_style": "modelica"}, "") }}
"#;
    let err =
        render_template(&dae, template).expect_err("missing AST named argument value must fail");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("AST named argument value failed to render"),
        "expected strict AST named-argument diagnostic, got: {msg}"
    );
}

#[test]
fn test_render_ast_array_requires_elements_field() {
    let dae = dae::Dae::new();
    let template = r#"
{% set stmt = {"Reinit": {
    "variable": "x",
    "value": {"Array": {}}
}} %}
{{ render_statement(stmt, {"if_style": "modelica"}, "") }}
"#;
    let err = render_template(&dae, template).expect_err("missing AST Array elements must fail");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("AST Array missing 'elements'"),
        "expected strict AST Array diagnostic, got: {msg}"
    );
}

#[test]
fn test_render_ast_tuple_requires_elements_field() {
    let dae = dae::Dae::new();
    let template = r#"
{% set stmt = {"Reinit": {
    "variable": "x",
    "value": {"Tuple": {}}
}} %}
{{ render_statement(stmt, {"if_style": "modelica"}, "") }}
"#;
    let err = render_template(&dae, template).expect_err("missing AST Tuple elements must fail");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("AST Tuple missing 'elements'"),
        "expected strict AST Tuple diagnostic, got: {msg}"
    );
}

#[test]
fn test_render_ast_range_requires_start_and_end() {
    let dae = dae::Dae::new();
    let template = r#"
{% set stmt = {"Reinit": {
    "variable": "x",
    "value": {
        "Range": {
            "end": {"Terminal": {"terminal_type": "UnsignedInteger", "token": {"text": "3"}}}
        }
    }
}} %}
{{ render_statement(stmt, {"if_style": "modelica"}, "") }}
"#;
    let err = render_template(&dae, template).expect_err("missing AST Range start must fail");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("AST Range start failed to render"),
        "expected strict AST Range diagnostic, got: {msg}"
    );
}

#[test]
fn test_render_ast_terminal_requires_token_text() {
    let dae = dae::Dae::new();
    let template = r#"
{% set stmt = {"Reinit": {
    "variable": "x",
    "value": {
        "Terminal": {
            "terminal_type": "UnsignedReal",
            "token": {}
        }
    }
}} %}
{{ render_statement(stmt, {"if_style": "modelica"}, "") }}
"#;
    let err = render_template(&dae, template).expect_err("missing terminal text must fail");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("AST Terminal missing token text"),
        "expected strict AST Terminal diagnostic, got: {msg}"
    );
}

#[test]
fn test_render_component_reference_requires_parts() {
    let dae = dae::Dae::new();
    let template = r#"
{% set stmt = {"Assignment": {
    "comp": {},
    "value": {"Literal": {"value": {"Real": 1.0}}}
}} %}
{{ render_statement(stmt, {"if_style": "modelica"}, "") }}
"#;
    let err =
        render_template(&dae, template).expect_err("missing component-reference parts must fail");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("ComponentReference missing 'parts'"),
        "expected strict component-reference diagnostic, got: {msg}"
    );
}

#[test]
fn test_render_component_reference_part_requires_ident() {
    let dae = dae::Dae::new();
    let template = r#"
{% set stmt = {"Assignment": {
    "comp": {"parts": [{}]},
    "value": {"Literal": {"value": {"Real": 1.0}}}
}} %}
{{ render_statement(stmt, {"if_style": "modelica"}, "") }}
"#;
    let err =
        render_template(&dae, template).expect_err("missing component-reference ident must fail");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("ComponentReference part missing 'ident'"),
        "expected strict component-reference part diagnostic, got: {msg}"
    );
}

#[test]
fn test_render_component_reference_subscript_reports_render_error() {
    let dae = dae::Dae::new();
    let template = r#"
{% set stmt = {"Assignment": {
    "comp": {
        "parts": [{
            "ident": {"text": "x"},
            "subscripts": [
                {"Expr": {"UnsupportedSubscriptExpr": true}}
            ]
        }]
    },
    "value": {"Literal": {"value": {"Real": 1.0}}}
}} %}
{{ render_statement(stmt, {"if_style": "modelica"}, "") }}
"#;
    let err =
        render_template(&dae, template).expect_err("unrenderable component subscript must fail");
    assert_miette_template_span(&err);
    let msg = format!("{err}");
    assert!(
        msg.contains("UnsupportedSubscriptExpr"),
        "expected strict component-reference subscript diagnostic, got: {msg}"
    );
}

#[test]
fn test_render_statement_if_blocks_use_stmts_field() {
    let dae = dae::Dae::new();
    let template = r#"
{% set stmt = {"If": {"cond_blocks": [
    {"cond": {"Literal": {"Boolean": true}}, "stmts": [
        {"Assignment": {"comp": "lat", "value": {"Literal": {"Real": 40.0}}}},
        {"Assignment": {"comp": "lon", "value": {"Literal": {"Real": -86.0}}}}
    ]}
], "else_block": [
    {"Assignment": {"comp": "lat", "value": {"Literal": {"Real": 0.0}}}}
]}} %}
{{ render_statement(stmt, {"if_style": "ternary"}, "") }}
"#;
    let rendered = render_template(&dae, template).expect("render if statement");
    assert!(
        rendered.contains("lat = 40") && rendered.contains("lon = -86"),
        "if branch assignments should render from StatementBlock.stmts:\n{rendered}"
    );
}
