use super::*;

/// Removing a boundary/trivial equation that sits before a structured family must
/// shift that family's `first_equation_index` down to match the compacted equation
/// vector. Regression for a method-of-lines model (e.g. the docs turkey heat eq)
/// where eliminating a boundary `Q_cond[1] = 0` left the interior `der` family one
/// slot high, so Solve-IR lowering folded the adjacent surface `der` row into the
/// interior stencil and computed it with the wrong body.
#[test]
fn shift_structured_families_after_equation_removal_remaps_first_eq() {
    let family = |first_equation_index: usize| dae::StructuredEquationFamily {
        domain: rumoca_core::StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                id: 0,
                display_name: "i".to_string(),
                lower: 1,
                upper: 3,
                step: 1,
            }],
        },
        first_equation_index,
        equations_per_point: 1,
        span: test_span(),
        origin: "test".to_string(),
        regular: None,
        template: None,
        interiors_materialized: true,
    };
    let mut dae = Dae::new();
    // fam0 spans eq 1..4 (e.g. Q_cond[2:4]); fam1 spans eq 6..9 (e.g. der(T[1:3])).
    dae.continuous.structured_equations = vec![family(1), family(6)];

    // Eliminating eq 0 (a boundary scalar before both families) compacts everything
    // down by one: fam0 -> rows 0..3, fam1 -> rows 5..8.
    structural_ok(shift_structured_families_after_equation_removal(
        &mut dae,
        &[0],
    ));
    assert_eq!(
        dae.continuous.structured_equations[0].first_equation_index,
        0
    );
    assert_eq!(
        dae.continuous.structured_equations[1].first_equation_index,
        5
    );

    // A removal in the gap *between* the families (eq 4, outside both blocks
    // [0,3) and [5,8)) shifts only the family after it; both blocks stay intact.
    structural_ok(shift_structured_families_after_equation_removal(
        &mut dae,
        &[4],
    ));
    assert_eq!(dae.continuous.structured_equations.len(), 2);
    assert_eq!(
        dae.continuous.structured_equations[0].first_equation_index,
        0
    );
    assert_eq!(
        dae.continuous.structured_equations[1].first_equation_index,
        4
    );
}

/// A family one of whose own rows is eliminated can no longer describe a
/// contiguous array block, so it must be dropped (its survivors lower as scalars)
/// rather than left pointing at a hole. Regression for a constant `for k loop
/// a[k] = k*c` family folded away by trivial elimination, which otherwise left a
/// dangling family that failed the corner-incidence invariant downstream.
#[test]
fn shift_structured_families_drops_family_with_removed_interior_row() {
    let family = |first_equation_index: usize| dae::StructuredEquationFamily {
        domain: rumoca_core::StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                id: 0,
                display_name: "k".to_string(),
                lower: 1,
                upper: 3,
                step: 1,
            }],
        },
        first_equation_index,
        equations_per_point: 1,
        span: test_span(),
        origin: "test".to_string(),
        regular: None,
        template: None,
        interiors_materialized: true,
    };
    let mut dae = Dae::new();
    // fam0 spans rows 3..6; a later survivor family fam1 spans rows 8..11.
    dae.continuous.structured_equations = vec![family(3), family(8)];

    // Eliminating fam0's middle row (eq 4) drops fam0 entirely; fam1 still shifts
    // down by the one removed row before it.
    structural_ok(shift_structured_families_after_equation_removal(
        &mut dae,
        &[4],
    ));
    assert_eq!(dae.continuous.structured_equations.len(), 1);
    assert_eq!(
        dae.continuous.structured_equations[0].first_equation_index,
        7
    );
}

#[test]
fn shift_structured_families_rejects_invalid_domain_without_mutation() {
    let family = dae::StructuredEquationFamily {
        domain: rumoca_core::StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                id: 0,
                display_name: "i".to_string(),
                lower: 1,
                upper: 3,
                step: 0,
            }],
        },
        first_equation_index: 2,
        equations_per_point: 1,
        span: test_span(),
        origin: "invalid domain".to_string(),
        regular: None,
        template: None,
        interiors_materialized: true,
    };
    let mut dae = Dae::new();
    dae.continuous.structured_equations.push(family);

    let error = shift_structured_families_after_equation_removal(&mut dae, &[0])
        .expect_err("invalid family metadata must produce a structural error");

    assert!(matches!(error, StructuralError::ContractViolation { .. }));
    assert_eq!(
        dae.continuous.structured_equations[0].first_equation_index,
        2
    );
}

/// A substitution can rewrite a structured family's row bodies while leaving the
/// row count unchanged. The original family proof no longer applies, so the
/// family must be dropped and lowered as scalar rows.
#[test]
fn drop_structured_families_touching_equations_drops_rewritten_family() {
    let family = |first_equation_index: usize| dae::StructuredEquationFamily {
        domain: rumoca_core::StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                id: 0,
                display_name: "i".to_string(),
                lower: 1,
                upper: 2,
                step: 1,
            }],
        },
        first_equation_index,
        equations_per_point: 1,
        span: test_span(),
        origin: "test".to_string(),
        regular: None,
        template: None,
        interiors_materialized: true,
    };
    let mut dae = Dae::new();
    dae.continuous.structured_equations = vec![family(1), family(4)];

    structural_ok(drop_structured_families_touching_equations(
        &mut dae,
        &[2],
        &[],
    ));

    assert_eq!(dae.continuous.structured_equations.len(), 1);
    assert_eq!(
        dae.continuous.structured_equations[0].first_equation_index,
        4
    );
}

#[test]
fn scalarization_rejects_rewritten_cheapened_family() {
    let mut dae = Dae::new();
    dae.continuous.structured_equations = vec![dae::StructuredEquationFamily {
        domain: rumoca_core::StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                id: 0,
                display_name: "i".to_string(),
                lower: 1,
                upper: 3,
                step: 1,
            }],
        },
        first_equation_index: 0,
        equations_per_point: 1,
        span: test_span(),
        origin: "cheapened_derivative_family".to_string(),
        regular: None,
        template: None,
        interiors_materialized: false,
    }];

    let err = drop_structured_families_touching_equations(&mut dae, &[1], &[0])
        .expect_err("cheapened family rows must not become scalar authorities");

    assert!(matches!(
        err,
        StructuralError::ContractViolation { span, .. } if span == test_span()
    ));
    assert!(
        err.to_string()
            .contains("placeholder interior rows cannot become authoritative")
    );
}

#[test]
fn cheapened_family_survives_derived_row_normalization() {
    let mut dae = Dae::new();
    for name in ["x", "unused"] {
        dae.variables
            .algebraics
            .insert(VarName::new(name), test_dae_variable(name));
    }
    let canonical = binary(OpBinary::Add, var_ref("x"), real(0.0));
    dae.continuous.equations.push(dae::Equation::residual(
        canonical.clone(),
        test_span(),
        "derived corner",
    ));
    dae.continuous.structured_equations = vec![dae::StructuredEquationFamily {
        domain: rumoca_core::StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                id: 0,
                display_name: "i".to_string(),
                lower: 1,
                upper: 1,
                step: 1,
            }],
        },
        first_equation_index: 0,
        equations_per_point: 1,
        span: test_span(),
        origin: "canonical cheapened family".to_string(),
        regular: None,
        template: Some(rumoca_core::ComprehensionTemplate {
            body: vec![canonical],
            scalar_view: rumoca_core::ComprehensionScalarView::BinderSubstitution,
        }),
        interiors_materialized: false,
    }];

    structural_ok(apply_substitutions_to_remaining_once(
        &mut dae,
        &[false],
        &[test_substitution("unused", real(1.0))],
    ));

    assert_eq!(dae.continuous.structured_equations.len(), 1);
    assert_eq!(dae.continuous.equations[0].rhs, var_ref("x"));
}

#[test]
fn cheapened_family_rejects_canonical_template_substitution() {
    let mut dae = Dae::new();
    for name in ["x", "source"] {
        dae.variables
            .algebraics
            .insert(VarName::new(name), test_dae_variable(name));
    }
    dae.continuous.equations.push(dae::Equation::residual(
        var_ref("source"),
        test_span(),
        "derived corner",
    ));
    dae.continuous.structured_equations = vec![dae::StructuredEquationFamily {
        domain: rumoca_core::StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                id: 0,
                display_name: "i".to_string(),
                lower: 1,
                upper: 1,
                step: 1,
            }],
        },
        first_equation_index: 0,
        equations_per_point: 1,
        span: test_span(),
        origin: "canonical cheapened family".to_string(),
        regular: None,
        template: Some(rumoca_core::ComprehensionTemplate {
            body: vec![var_ref("source")],
            scalar_view: rumoca_core::ComprehensionScalarView::BinderSubstitution,
        }),
        interiors_materialized: false,
    }];

    let error = apply_substitutions_to_remaining_once(
        &mut dae,
        &[false],
        &[test_substitution("source", var_ref("x"))],
    )
    .expect_err("canonical template rewrites must be rejected");

    assert!(
        error
            .to_string()
            .contains("placeholder interior rows cannot become authoritative")
    );
}

#[test]
fn boundary_elimination_preserves_unknowns_owned_by_cheapened_family() {
    let mut dae = Dae::new();
    for name in ["x", "source"] {
        dae.variables
            .algebraics
            .insert(VarName::new(name), test_dae_variable(name));
    }
    dae.continuous
        .equations
        .push(residual(var_ref("x"), var_ref("source"), 1, "corner"));
    dae.continuous
        .equations
        .push(residual(var_ref("x"), real(0.0), 1, "placeholder"));
    dae.continuous
        .equations
        .push(residual(var_ref("source"), real(1.0), 1, "source"));
    dae.continuous.structured_equations = vec![dae::StructuredEquationFamily {
        domain: rumoca_core::StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                id: 0,
                display_name: "i".to_string(),
                lower: 1,
                upper: 2,
                step: 1,
            }],
        },
        first_equation_index: 0,
        equations_per_point: 1,
        span: test_span(),
        origin: "cheapened family".to_string(),
        regular: None,
        template: None,
        interiors_materialized: false,
    }];

    let result = structural_ok(resolve_boundary_equations(&mut dae));

    assert_eq!(result.n_eliminated, 0);
    assert!(result.substitutions.is_empty());
    assert_eq!(dae.continuous.structured_equations.len(), 1);
    assert_eq!(dae.continuous.equations.len(), 3);
}

#[test]
fn cheapened_family_protects_aggregate_with_only_scalarized_descendants() {
    let mut dae = Dae::new();
    dae.variables.algebraics.insert(
        VarName::new("vehicle.motor.omega_cmd[1]"),
        component_var("vehicle.motor.omega_cmd[1]"),
    );
    dae.continuous.equations.push(residual(
        var_ref("vehicle.motor.omega_cmd"),
        real(0.0),
        1,
        "aggregate family corner",
    ));
    dae.continuous.structured_equations = vec![dae::StructuredEquationFamily {
        domain: rumoca_core::StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                id: 0,
                display_name: "i".to_string(),
                lower: 1,
                upper: 1,
                step: 1,
            }],
        },
        first_equation_index: 0,
        equations_per_point: 1,
        span: test_span(),
        origin: "scalarized aggregate family".to_string(),
        regular: None,
        template: None,
        interiors_materialized: false,
    }];

    let protected =
        runtime_protected_unknown_names(&dae).expect("structured family metadata is valid");

    assert!(protected.contains("vehicle.motor.omega_cmd"));
    assert!(is_runtime_protected_unknown(
        &VarName::new("vehicle.motor.omega_cmd[1]"),
        &protected
    ));
}

#[test]
fn compact_family_protection_rejects_out_of_bounds_row_inventory() {
    let mut dae = Dae::new();
    dae.continuous
        .equations
        .push(residual(var_ref("x"), real(0.0), 1, "only row"));
    dae.continuous.structured_equations = vec![dae::StructuredEquationFamily {
        domain: rumoca_core::StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                id: 0,
                display_name: "i".to_string(),
                lower: 1,
                upper: 1,
                step: 1,
            }],
        },
        first_equation_index: 1,
        equations_per_point: 1,
        span: test_span(),
        origin: "out-of-bounds compact family".to_string(),
        regular: None,
        template: None,
        interiors_materialized: false,
    }];

    let error = runtime_protected_unknown_names(&dae)
        .expect_err("malformed compact family metadata must not be ignored");

    assert!(matches!(error, StructuralError::ContractViolation { .. }));
    assert!(error.to_string().contains("exceeds 1 continuous equations"));
}

/// Residual rows have no `lhs`, so substitution used to rewrite the RHS and
/// return before recording the touched row. Structured metadata for that row
/// must still be dropped because its compact body proof is now stale.
#[test]
fn substitution_drops_structured_family_for_rewritten_residual_row() {
    let mut dae = Dae::new();
    dae.continuous.equations.push(residual(
        var_ref("x"),
        var_ref("s"),
        1,
        "structured_residual",
    ));
    dae.continuous.structured_equations = vec![dae::StructuredEquationFamily {
        domain: rumoca_core::StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                id: 0,
                display_name: "i".to_string(),
                lower: 1,
                upper: 1,
                step: 1,
            }],
        },
        first_equation_index: 0,
        equations_per_point: 1,
        span: test_span(),
        origin: "test".to_string(),
        regular: None,
        template: None,
        interiors_materialized: true,
    }];

    structural_ok(apply_substitutions_to_remaining_once(
        &mut dae,
        &[false],
        &[test_substitution("s", real(1.0))],
    ));

    assert!(dae.continuous.structured_equations.is_empty());
}
