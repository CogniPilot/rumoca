use rumoca_ir_dae as dae;
use rumoca_sim_core::{
    ParameterValueError, TimeoutBudget, default_parameter_values,
    default_parameter_values_with_budget,
};

fn var(name: &str) -> dae::Expression {
    dae::Expression::VarRef {
        name: dae::VarName::new(name),
        subscripts: vec![],
    }
}

fn lit(value: f64) -> dae::Expression {
    dae::Expression::Literal(dae::Literal::Real(value))
}

fn binop(op: dae::OpBinary, lhs: dae::Expression, rhs: dae::Expression) -> dae::Expression {
    dae::Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

#[test]
fn test_default_parameter_values_empty() {
    let dae = dae::Dae::new();
    let params = default_parameter_values(&dae);
    assert!(params.is_empty());
}

#[test]
fn test_default_parameter_values_with_budget_rejects_non_finite_values() {
    let mut dae = dae::Dae::new();
    let mut p = dae::Variable::new(dae::VarName::new("p"));
    p.start = Some(dae::Expression::FunctionCall {
        name: dae::VarName::new("missingFunction"),
        args: vec![],
        is_constructor: false,
    });
    dae.parameters.insert(dae::VarName::new("p"), p);

    let budget = TimeoutBudget::new(None);
    let err = default_parameter_values_with_budget(&dae, &budget)
        .expect_err("non-finite parameter evaluation should fail in budgeted path");
    assert!(
        matches!(err, ParameterValueError::Invalid(ref msg) if msg.contains("non-finite parameter")),
        "unexpected error: {err:?}"
    );
}

#[test]
fn test_default_parameter_values_with_budget_allows_infinite_parameter_defaults() {
    let mut dae = dae::Dae::new();
    let mut p = dae::Variable::new(dae::VarName::new("p"));
    p.start = Some(lit(f64::NEG_INFINITY));
    dae.parameters.insert(dae::VarName::new("p"), p);

    let budget = TimeoutBudget::new(None);
    let params = default_parameter_values_with_budget(&dae, &budget)
        .expect("infinite parameter defaults are valid Modelica sentinels");
    assert_eq!(params.len(), 1);
    assert!(params[0].is_infinite() && params[0].is_sign_negative());
}

#[test]
fn test_default_parameter_values_with_budget_allows_transient_forward_ref_nan_in_pass1() {
    let mut dae = dae::Dae::new();

    let mut p0 = dae::Variable::new(dae::VarName::new("p0"));
    p0.start = Some(binop(
        dae::OpBinary::Div(Default::default()),
        binop(dae::OpBinary::Mul(Default::default()), var("p1"), var("p2")),
        binop(dae::OpBinary::Sub(Default::default()), var("p1"), var("p2")),
    ));
    dae.parameters.insert(dae::VarName::new("p0"), p0);

    let mut p1 = dae::Variable::new(dae::VarName::new("p1"));
    p1.start = Some(lit(1.0));
    dae.parameters.insert(dae::VarName::new("p1"), p1);

    let mut p2 = dae::Variable::new(dae::VarName::new("p2"));
    p2.start = Some(lit(2.0));
    dae.parameters.insert(dae::VarName::new("p2"), p2);

    let budget = TimeoutBudget::new(None);
    let params = default_parameter_values_with_budget(&dae, &budget)
        .expect("pass-2 forward-reference re-evaluation should resolve transient NaN");
    assert_eq!(params.len(), 3);
    assert!((params[0] + 2.0).abs() < 1e-12);
    assert!((params[1] - 1.0).abs() < 1e-12);
    assert!((params[2] - 2.0).abs() < 1e-12);
}

#[test]
fn test_default_parameter_values_with_budget_resolves_multi_level_forward_refs_to_avoid_nan() {
    let mut dae = dae::Dae::new();

    let mut l_sigma = dae::Variable::new(dae::VarName::new("Lsigma"));
    l_sigma.start = Some(binop(
        dae::OpBinary::Mul(Default::default()),
        binop(
            dae::OpBinary::Sub(Default::default()),
            lit(1.0),
            var("ratio"),
        ),
        var("L"),
    ));
    dae.parameters.insert(dae::VarName::new("Lsigma"), l_sigma);

    let mut l = dae::Variable::new(dae::VarName::new("L"));
    l.start = Some(binop(
        dae::OpBinary::Div(Default::default()),
        lit(1.0),
        var("f"),
    ));
    dae.parameters.insert(dae::VarName::new("L"), l);

    let mut f = dae::Variable::new(dae::VarName::new("f"));
    f.start = Some(var("fs"));
    dae.parameters.insert(dae::VarName::new("f"), f);

    let mut fs = dae::Variable::new(dae::VarName::new("fs"));
    fs.start = Some(lit(50.0));
    dae.parameters.insert(dae::VarName::new("fs"), fs);

    let mut ratio = dae::Variable::new(dae::VarName::new("ratio"));
    ratio.start = Some(lit(1.0));
    dae.parameters.insert(dae::VarName::new("ratio"), ratio);

    let budget = TimeoutBudget::new(None);
    let params = default_parameter_values_with_budget(&dae, &budget)
        .expect("multi-level forward references should converge to finite values");
    assert!(
        params[0].is_finite(),
        "Lsigma should be finite, got {}",
        params[0]
    );
    assert!(
        params[1].is_finite(),
        "L should be finite, got {}",
        params[1]
    );
    assert!((params[0] - 0.0).abs() < 1e-12);
    assert!((params[2] - 50.0).abs() < 1e-12);
}

#[test]
fn test_default_parameter_values_constant_array_index_uses_selected_entry() {
    let mut dae = dae::Dae::new();

    let mut conversion_table = dae::Variable::new(dae::VarName::new("conversionTable"));
    conversion_table.dims = vec![3];
    conversion_table.start = Some(dae::Expression::Array {
        elements: vec![lit(31536000.0), lit(3600.0), lit(1000.0)],
        is_matrix: false,
    });
    dae.constants
        .insert(dae::VarName::new("conversionTable"), conversion_table);

    let mut resolution = dae::Variable::new(dae::VarName::new("resolution"));
    resolution.start = Some(lit(3.0));
    dae.parameters
        .insert(dae::VarName::new("resolution"), resolution);

    let mut resolution_factor = dae::Variable::new(dae::VarName::new("resolutionFactor"));
    resolution_factor.start = Some(dae::Expression::Index {
        base: Box::new(var("conversionTable")),
        subscripts: vec![dae::Subscript::Expr(Box::new(
            dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Integer,
                args: vec![var("resolution")],
            },
        ))],
    });
    dae.parameters
        .insert(dae::VarName::new("resolutionFactor"), resolution_factor);

    let budget = TimeoutBudget::new(None);
    let params = default_parameter_values_with_budget(&dae, &budget)
        .expect("constant array indexing should resolve in parameter starts");

    let mut pidx = 0usize;
    let mut resolution_value = None;
    let mut resolution_factor_value = None;
    for (name, var) in &dae.parameters {
        if var.size() > 1 {
            pidx += var.size();
            continue;
        }
        let value = params[pidx];
        if name.as_str() == "resolution" {
            resolution_value = Some(value);
        } else if name.as_str() == "resolutionFactor" {
            resolution_factor_value = Some(value);
        }
        pidx += 1;
    }

    let resolution_value = resolution_value.expect("resolution parameter missing");
    let resolution_factor_value =
        resolution_factor_value.expect("resolutionFactor parameter missing");

    assert!((resolution_value - 3.0).abs() < 1e-12);
    assert!(
        (resolution_factor_value - 1000.0).abs() < 1e-12,
        "expected indexed conversion table entry 1000.0, got {resolution_factor_value}"
    );
}
