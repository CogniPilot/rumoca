use super::*;
use rumoca_core::Span;
use rumoca_ir_dae as dae;

mod array_boundary;
mod boundary_extra;
mod tearing;

mod substitution_more;
type BuiltinFunction = rumoca_core::BuiltinFunction;
type Literal = rumoca_core::Literal;

fn sub_op() -> OpBinary {
    OpBinary::Sub
}

fn minus_op() -> OpUnary {
    OpUnary::Minus
}

fn test_span() -> Span {
    rumoca_core::Span::from_offsets(rumoca_core::SourceId(1), 1, 2)
}

fn structural_ok<T>(result: Result<T, StructuralError>) -> T {
    match result {
        Ok(value) => value,
        Err(err) => panic!("unexpected structural error: {err}"),
    }
}

fn test_dae_variable(path: &str) -> dae::Variable {
    let mut var = dae::Variable::new(
        VarName::new(path),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    var.source_span = test_span();
    var
}

fn substitute_var(expr: &Expression, var: &VarName, replacement: &Expression) -> Expression {
    let substitution = test_substitution(var.as_str(), replacement.clone());
    structural_ok(
        SubstituteVarRewriter {
            substitution: &substitution,
            replacement,
            replacement_dims: &substitution.replacement_dims,
            derivative_replacement: None,
        }
        .rewrite_expression(expr),
    )
}

fn test_substitution(name: &str, expr: Expression) -> Substitution {
    Substitution {
        var_name: VarName::new(name),
        var_ref: Some(reference(name)),
        expr,
        var_dims: Vec::new(),
        replacement_dims: Vec::new(),
        env_keys: vec![name.to_string()],
    }
}

fn var_ref(name: &str) -> Expression {
    Expression::VarRef {
        name: reference(name),
        subscripts: vec![],
        span: test_span(),
    }
}

fn var_ref_idx(name: &str, idx: i64) -> Expression {
    let span = test_span();
    Expression::VarRef {
        name: reference(name),
        subscripts: vec![rumoca_core::Subscript::generated_index(idx, span)],
        span,
    }
}

fn real(value: f64) -> Expression {
    Expression::Literal {
        value: Literal::Real(value),
        span: test_span(),
    }
}

fn array(elements: Vec<Expression>) -> Expression {
    Expression::Array {
        elements,
        is_matrix: false,
        span: test_span(),
    }
}

fn der(expr: Expression) -> Expression {
    Expression::BuiltinCall {
        function: BuiltinFunction::Der,
        args: vec![expr],
        span: test_span(),
    }
}

fn binary(op: OpBinary, lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: test_span(),
    }
}

fn residual(lhs: Expression, rhs: Expression, scalar_count: usize, origin: &str) -> dae::Equation {
    dae::Equation {
        lhs: None,
        rhs: binary(OpBinary::Sub, lhs, rhs),
        span: test_span(),
        origin: origin.to_string(),
        scalar_count,
    }
}

fn reference(path: &str) -> rumoca_core::Reference {
    rumoca_core::Reference::from_component_reference(component_ref(path))
}

fn dummy_reference(path: &str) -> rumoca_core::Reference {
    rumoca_core::Reference::from_component_reference(dummy_component_ref(path))
}

fn component_var(path: &str) -> dae::Variable {
    let mut var = test_dae_variable(path);
    var.component_ref = Some(component_ref(path));
    var
}

fn component_ref(path: &str) -> rumoca_core::ComponentReference {
    rumoca_core::ComponentReference {
        local: false,
        span: test_span(),
        parts: rumoca_core::split_path_with_indices(path)
            .into_iter()
            .filter(|part| !part.is_empty())
            .map(component_ref_part)
            .collect(),
        def_id: None,
    }
}

fn component_ref_part(part: &str) -> rumoca_core::ComponentRefPart {
    let scalar = rumoca_core::parse_scalar_name(part);
    rumoca_core::ComponentRefPart {
        ident: scalar
            .as_ref()
            .map_or(part, |scalar| scalar.base)
            .to_string(),
        span: test_span(),
        subs: scalar
            .map(|scalar| {
                scalar
                    .indices
                    .into_iter()
                    .map(|index| rumoca_core::Subscript::generated_index(index, test_span()))
                    .collect()
            })
            .unwrap_or_default(),
    }
}

fn dummy_component_ref(path: &str) -> rumoca_core::ComponentReference {
    rumoca_core::ComponentReference {
        local: false,
        span: Span::DUMMY,
        parts: rumoca_core::split_path_with_indices(path)
            .into_iter()
            .filter(|part| !part.is_empty())
            .map(dummy_component_ref_part)
            .collect(),
        def_id: None,
    }
}

fn dummy_component_ref_part(part: &str) -> rumoca_core::ComponentRefPart {
    let scalar = rumoca_core::parse_scalar_name(part);
    rumoca_core::ComponentRefPart {
        ident: scalar
            .as_ref()
            .map_or(part, |scalar| scalar.base)
            .to_string(),
        span: Span::DUMMY,
        subs: scalar
            .map(|scalar| {
                scalar
                    .indices
                    .into_iter()
                    .map(|index| rumoca_core::Subscript::generated_index(index, Span::DUMMY))
                    .collect()
            })
            .unwrap_or_default(),
    }
}

fn contains_exact_var_ref(expr: &Expression, needle: &str) -> bool {
    match expr {
        Expression::VarRef { name, .. } => name.as_str() == needle,
        Expression::Binary { lhs, rhs, .. } => {
            contains_exact_var_ref(lhs, needle) || contains_exact_var_ref(rhs, needle)
        }
        Expression::Unary { rhs, .. } => contains_exact_var_ref(rhs, needle),
        Expression::BuiltinCall { args, .. } | Expression::FunctionCall { args, .. } => {
            args.iter().any(|arg| contains_exact_var_ref(arg, needle))
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches.iter().any(|(condition, value)| {
                contains_exact_var_ref(condition, needle) || contains_exact_var_ref(value, needle)
            }) || contains_exact_var_ref(else_branch, needle)
        }
        Expression::Array { elements, .. } => elements
            .iter()
            .any(|element| contains_exact_var_ref(element, needle)),
        Expression::Tuple { elements, .. } => elements
            .iter()
            .any(|element| contains_exact_var_ref(element, needle)),
        Expression::Index {
            base, subscripts, ..
        } => {
            contains_exact_var_ref(base, needle)
                || subscripts.iter().any(|subscript| match subscript {
                    rumoca_core::Subscript::Expr { expr, .. } => {
                        contains_exact_var_ref(expr, needle)
                    }
                    _ => false,
                })
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            contains_exact_var_ref(expr, needle)
                || indices
                    .iter()
                    .any(|index| contains_exact_var_ref(&index.range, needle))
                || filter
                    .as_ref()
                    .is_some_and(|filter| contains_exact_var_ref(filter, needle))
        }
        Expression::Range {
            start, step, end, ..
        } => {
            contains_exact_var_ref(start, needle)
                || step
                    .as_ref()
                    .is_some_and(|step| contains_exact_var_ref(step, needle))
                || contains_exact_var_ref(end, needle)
        }
        Expression::FieldAccess { base, .. } => contains_exact_var_ref(base, needle),
        Expression::Literal { .. } | Expression::Empty { .. } => false,
    }
}

fn contains_array_expr(expr: &Expression) -> bool {
    match expr {
        Expression::Array { .. } => true,
        Expression::Binary { lhs, rhs, .. } => contains_array_expr(lhs) || contains_array_expr(rhs),
        Expression::Unary { rhs, .. } => contains_array_expr(rhs),
        Expression::BuiltinCall { args, .. } | Expression::FunctionCall { args, .. } => {
            args.iter().any(contains_array_expr)
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches.iter().any(|(condition, value)| {
                contains_array_expr(condition) || contains_array_expr(value)
            }) || contains_array_expr(else_branch)
        }
        Expression::Index {
            base, subscripts, ..
        } => {
            contains_array_expr(base)
                || subscripts.iter().any(|subscript| match subscript {
                    rumoca_core::Subscript::Expr { expr, .. } => contains_array_expr(expr),
                    _ => false,
                })
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            contains_array_expr(expr)
                || indices
                    .iter()
                    .any(|index| contains_array_expr(&index.range))
                || filter
                    .as_ref()
                    .is_some_and(|filter| contains_array_expr(filter))
        }
        Expression::Range {
            start, step, end, ..
        } => {
            contains_array_expr(start)
                || step.as_ref().is_some_and(|step| contains_array_expr(step))
                || contains_array_expr(end)
        }
        Expression::FieldAccess { base, .. } => contains_array_expr(base),
        Expression::Tuple { elements, .. } => elements.iter().any(contains_array_expr),
        Expression::VarRef { .. } | Expression::Literal { .. } | Expression::Empty { .. } => false,
    }
}

fn contains_complex_constructor(expr: &Expression) -> bool {
    match expr {
        Expression::FunctionCall {
            name,
            is_constructor,
            ..
        } if *is_constructor && name.as_str() == "Complex" => true,
        Expression::Binary { lhs, rhs, .. } => {
            contains_complex_constructor(lhs) || contains_complex_constructor(rhs)
        }
        Expression::Unary { rhs, .. } => contains_complex_constructor(rhs),
        Expression::BuiltinCall { args, .. } | Expression::FunctionCall { args, .. } => {
            args.iter().any(contains_complex_constructor)
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches.iter().any(|(condition, value)| {
                contains_complex_constructor(condition) || contains_complex_constructor(value)
            }) || contains_complex_constructor(else_branch)
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements, .. } => {
            elements.iter().any(contains_complex_constructor)
        }
        Expression::Index {
            base, subscripts, ..
        } => {
            contains_complex_constructor(base)
                || subscripts.iter().any(|subscript| match subscript {
                    rumoca_core::Subscript::Expr { expr, .. } => contains_complex_constructor(expr),
                    _ => false,
                })
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            contains_complex_constructor(expr)
                || indices
                    .iter()
                    .any(|index| contains_complex_constructor(&index.range))
                || filter
                    .as_ref()
                    .is_some_and(|filter| contains_complex_constructor(filter))
        }
        Expression::Range {
            start, step, end, ..
        } => {
            contains_complex_constructor(start)
                || step
                    .as_ref()
                    .is_some_and(|step| contains_complex_constructor(step))
                || contains_complex_constructor(end)
        }
        Expression::FieldAccess { base, .. } => contains_complex_constructor(base),
        Expression::VarRef { .. } | Expression::Literal { .. } | Expression::Empty { .. } => false,
    }
}

fn lit(v: f64) -> Expression {
    Expression::Literal {
        value: Literal::Real(v),
        span: rumoca_core::Span::DUMMY,
    }
}

fn add_op() -> OpBinary {
    OpBinary::Add
}

fn mul_op() -> OpBinary {
    OpBinary::Mul
}

// ── try_solve_for_unknown ─────────────────────────────────────────

#[test]
fn test_try_solve_sub_lhs() {
    let rhs = Expression::Binary {
        op: sub_op(),
        lhs: Box::new(var_ref("z")),
        rhs: Box::new(Expression::Binary {
            op: add_op(),
            lhs: Box::new(var_ref("x")),
            rhs: Box::new(lit(1.0)),
            span: rumoca_core::Span::DUMMY,
        }),
        span: rumoca_core::Span::DUMMY,
    };
    let result = try_solve_for_unknown(&rhs, &VarName::new("z"));
    assert!(result.is_some());
    assert!(matches!(result.unwrap(), Expression::Binary { .. }));
}

#[test]
fn test_try_solve_sub_rhs() {
    let rhs = Expression::Binary {
        op: sub_op(),
        lhs: Box::new(var_ref("x")),
        rhs: Box::new(var_ref("z")),
        span: rumoca_core::Span::DUMMY,
    };
    let result = try_solve_for_unknown(&rhs, &VarName::new("z"));
    assert!(result.is_some());
    assert!(matches!(result.unwrap(), Expression::VarRef { .. }));
}

#[test]
fn test_try_solve_negated() {
    let inner = Expression::Binary {
        op: sub_op(),
        lhs: Box::new(var_ref("z")),
        rhs: Box::new(lit(5.0)),
        span: rumoca_core::Span::DUMMY,
    };
    let rhs = Expression::Unary {
        op: minus_op(),
        rhs: Box::new(inner),
        span: rumoca_core::Span::DUMMY,
    };
    let result = try_solve_for_unknown(&rhs, &VarName::new("z"));
    assert!(result.is_some());
    assert!(
        matches!(result.unwrap(), Expression::Literal { value: Literal::Real(v), .. } if v == 5.0)
    );
}

#[test]
fn test_try_solve_sub_lhs_with_unity_subscript_alias_matches() {
    let rhs = Expression::Binary {
        op: sub_op(),
        lhs: Box::new(var_ref_idx("z", 1)),
        rhs: Box::new(lit(3.0)),
        span: rumoca_core::Span::DUMMY,
    };
    let result = try_solve_for_unknown(&rhs, &VarName::new("z"));
    assert!(result.is_some());
    assert!(
        matches!(result.unwrap(), Expression::Literal { value: Literal::Real(v), .. } if v == 3.0)
    );
}

#[test]
fn test_try_solve_sub_lhs_with_non_unity_subscript_fails() {
    let rhs = Expression::Binary {
        op: sub_op(),
        lhs: Box::new(var_ref_idx("z", 2)),
        rhs: Box::new(lit(3.0)),
        span: rumoca_core::Span::DUMMY,
    };
    let result = try_solve_for_unknown(&rhs, &VarName::new("z"));
    assert!(result.is_none());
}

#[test]
fn test_try_solve_complex_fails() {
    let rhs = Expression::Binary {
        op: sub_op(),
        lhs: Box::new(Expression::Binary {
            op: mul_op(),
            lhs: Box::new(var_ref("z")),
            rhs: Box::new(var_ref("z")),
            span: rumoca_core::Span::DUMMY,
        }),
        rhs: Box::new(lit(4.0)),
        span: rumoca_core::Span::DUMMY,
    };
    let result = try_solve_for_unknown(&rhs, &VarName::new("z"));
    assert!(result.is_none());
}

#[test]
fn test_try_solve_does_not_match_complex_base_for_field_unknown() {
    let rhs = Expression::Binary {
        op: sub_op(),
        lhs: Box::new(var_ref("transferFunction.aSum")),
        rhs: Box::new(lit(1.0)),
        span: rumoca_core::Span::DUMMY,
    };
    let result = try_solve_for_unknown(&rhs, &VarName::new("transferFunction.aSum.re"));
    assert!(result.is_none());
}

#[test]
fn test_var_ref_matches_unknown_allows_complex_base_to_field_alias() {
    assert!(var_ref_matches_unknown(
        &rumoca_core::Reference::new("transferFunction.aSum"),
        &[],
        &VarName::new("transferFunction.aSum.re")
    ));
    assert!(var_ref_matches_unknown(
        &rumoca_core::Reference::new("transferFunction.aSum"),
        &[],
        &VarName::new("transferFunction.aSum.im")
    ));
}

#[test]
fn test_expr_contains_var_matches_complex_base_alias() {
    let expr = var_ref("transferFunction.aSum");
    assert!(expr_contains_var(
        &expr,
        &VarName::new("transferFunction.aSum.re")
    ));
    assert!(expr_contains_var(
        &expr,
        &VarName::new("transferFunction.aSum.im")
    ));
}

#[test]
fn test_expr_contains_der_of_matches_indexed_subscript_form() {
    let expr = Expression::BuiltinCall {
        function: BuiltinFunction::Der,
        args: vec![var_ref_idx("x", 2)],
        span: rumoca_core::Span::DUMMY,
    };
    assert!(expr_contains_der_of(&expr, &VarName::new("x")));
}

#[test]
fn test_expr_contains_der_of_matches_embedded_subscript_mid_path() {
    let expr = Expression::BuiltinCall {
        function: BuiltinFunction::Der,
        args: vec![var_ref("support[2].phi")],
        span: rumoca_core::Span::DUMMY,
    };
    assert!(expr_contains_der_of(&expr, &VarName::new("support.phi")));
}

#[test]
fn test_expr_contains_der_of_walks_array_wrapper() {
    let expr = Expression::Array {
        elements: vec![Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args: vec![var_ref("x")],
            span: rumoca_core::Span::DUMMY,
        }],
        is_matrix: false,
        span: rumoca_core::Span::DUMMY,
    };
    assert!(expr_contains_der_of(&expr, &VarName::new("x")));
}

#[test]
fn test_expr_contains_der_of_walks_index_and_field_wrappers() {
    let expr = Expression::FieldAccess {
        base: Box::new(Expression::Index {
            base: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
                span: rumoca_core::Span::DUMMY,
            }),
            subscripts: vec![rumoca_core::Subscript::generated_index(
                1,
                rumoca_core::Span::DUMMY,
            )],
            span: rumoca_core::Span::DUMMY,
        }),
        field: "re".to_string(),
        span: rumoca_core::Span::DUMMY,
    };
    assert!(expr_contains_der_of(&expr, &VarName::new("x")));
}

// ── substitute_var ────────────────────────────────────────────────

#[test]
fn test_substitute_var_simple() {
    let expr = Expression::Binary {
        op: mul_op(),
        lhs: Box::new(var_ref("z")),
        rhs: Box::new(lit(2.0)),
        span: rumoca_core::Span::DUMMY,
    };
    let replacement = Expression::Binary {
        op: add_op(),
        lhs: Box::new(var_ref("x")),
        rhs: Box::new(lit(1.0)),
        span: rumoca_core::Span::DUMMY,
    };
    let result = substitute_var(&expr, &VarName::new("z"), &replacement);
    if let Expression::Binary { lhs, .. } = &result {
        assert!(matches!(lhs.as_ref(), Expression::Binary { .. }));
    } else {
        panic!("expected Binary");
    }
}

#[test]
fn test_substitute_var_projects_embedded_array_alias_component() {
    let expr = var_ref("attitude.omega[2]");
    let result = substitute_var(&expr, &VarName::new("attitude.omega"), &var_ref("omega"));

    assert!(
        matches!(
            result,
            Expression::VarRef { name, subscripts, .. }
                if name.as_str() == "omega"
                    && matches!(subscripts.as_slice(), [rumoca_core::Subscript::Index { value: 2, .. }])
        ),
        "scalarized alias component should project replacement array"
    );
}

#[test]
fn substitute_var_subscripted_replacement_uses_reference_span_when_replacement_is_unspanned() {
    let span = Span::from_offsets(
        rumoca_core::SourceId::from_source_name("alias_span.mo"),
        7,
        13,
    );
    let expr = Expression::VarRef {
        name: reference("arr"),
        subscripts: vec![rumoca_core::Subscript::generated_index(1, Span::DUMMY)],
        span,
    };
    let substitution = Substitution {
        var_name: VarName::new("arr"),
        var_ref: Some(reference("arr")),
        expr: var_ref("source"),
        var_dims: vec![2],
        replacement_dims: vec![2],
        env_keys: vec!["arr".to_string()],
    };

    let result = structural_ok(apply_substitutions_to_expr(&expr, &[substitution]));

    assert_eq!(result.span(), Some(span));
    assert!(
        matches!(
            result,
            Expression::VarRef { name, subscripts, .. }
                if name.as_str() == "source"
                    && matches!(subscripts.as_slice(), [rumoca_core::Subscript::Index { value: 1, .. }])
        ),
        "subscripted alias replacement should retain source reference span"
    );
}

#[test]
fn substitute_var_rejects_unspanned_embedded_array_alias_projection() {
    let expr = Expression::VarRef {
        name: dummy_reference("arr[2]"),
        subscripts: Vec::new(),
        span: Span::DUMMY,
    };
    let substitution = Substitution {
        var_name: VarName::new("arr"),
        var_ref: Some(dummy_reference("arr")),
        expr: Expression::VarRef {
            name: dummy_reference("source"),
            subscripts: Vec::new(),
            span: Span::DUMMY,
        },
        var_dims: vec![2],
        replacement_dims: vec![2],
        env_keys: vec!["arr".to_string()],
    };

    match apply_substitutions_to_expr(&expr, &[substitution]) {
        Err(StructuralError::UnspannedContractViolation { reason }) => assert!(
            reason.contains("structural substitution projection"),
            "unexpected reason: {reason}"
        ),
        other => panic!("expected unspanned projection error, got {other:?}"),
    }
}

#[test]
fn test_substitute_var_does_not_project_scalarized_alias_as_aggregate() {
    let expr = var_ref(
        "aimc.stator.electroMagneticConverter.singlePhaseElectroMagneticConverter[2].abs_V_m",
    );
    let replacement = Expression::FunctionCall {
        name: rumoca_core::Reference::new("Modelica.ComplexMath.abs"),
        args: vec![
            var_ref(
                "aimc.stator.electroMagneticConverter.singlePhaseElectroMagneticConverter[1].V_m.re",
            ),
            var_ref(
                "aimc.stator.electroMagneticConverter.singlePhaseElectroMagneticConverter[1].V_m.im",
            ),
        ],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    };
    let result = substitute_var(
        &expr,
        &VarName::new(
            "aimc.stator.electroMagneticConverter.singlePhaseElectroMagneticConverter[1].abs_V_m",
        ),
        &replacement,
    );

    assert!(
        matches!(
            result,
            Expression::VarRef { name, subscripts, .. }
                if name.as_str()
                    == "aimc.stator.electroMagneticConverter.singlePhaseElectroMagneticConverter[2].abs_V_m"
                    && subscripts.is_empty()
        ),
        "a scalarized component substitution must not rewrite sibling scalarized components"
    );
}

#[test]
fn test_substitute_var_rewrites_exact_indexed_component_without_use_site_component_ref() {
    let expr = Expression::VarRef {
        name: rumoca_core::Reference::new("vehicle.motor[1].tau_inv"),
        subscripts: vec![],
        span: Span::DUMMY,
    };
    let substitutions = [Substitution {
        var_name: VarName::new("vehicle.motor[1].tau_inv"),
        var_ref: Some(reference("vehicle.motor[1].tau_inv")),
        expr: var_ref("vehicle.motor[1].tau_inv_mid"),
        var_dims: Vec::new(),
        replacement_dims: Vec::new(),
        env_keys: Vec::new(),
    }];

    let result = structural_ok(apply_substitutions_to_expr(&expr, &substitutions));

    assert!(
        matches!(
            result,
            Expression::VarRef { name, subscripts, .. }
                if name.as_str() == "vehicle.motor[1].tau_inv_mid" && subscripts.is_empty()
        ),
        "exact eliminated indexed component refs must substitute even if a use site lost component_ref metadata"
    );
}

#[test]
fn test_substitute_var_matches_subscripted_aggregate_ref_to_scalarized_unknown() {
    let expr = var_ref_idx("aimc.rotorCage.electroMagneticConverter.i", 2);
    let result = substitute_var(
        &expr,
        &VarName::new("aimc.rotorCage.electroMagneticConverter.i[2]"),
        &var_ref("aimc.rotorCage.electroMagneticConverter.plug_p.pin[2].i"),
    );

    assert!(
        matches!(
            result,
            Expression::VarRef { name, subscripts, .. }
                if name.as_str()
                    == "aimc.rotorCage.electroMagneticConverter.plug_p.pin[2].i"
                    && subscripts.is_empty()
        ),
        "substituting an eliminated scalar element must rewrite aggregate subscript refs to that element"
    );
}

#[test]
fn test_substitute_var_does_not_rewrite_aggregate_ref_to_scalarized_unknown() {
    let expr = var_ref("vehicle.attitude.omega");
    let result = substitute_var(
        &expr,
        &VarName::new("vehicle.attitude.omega[1]"),
        &var_ref_idx("vehicle.omega", 1),
    );

    assert!(
        matches!(
            result,
            Expression::VarRef { name, subscripts, .. }
                if name.as_str() == "vehicle.attitude.omega" && subscripts.is_empty()
        ),
        "scalar alias substitutions must not collapse aggregate vector function arguments"
    );
}

#[test]
fn test_complete_scalar_alias_group_rewrites_aggregate_function_argument() {
    let expr = var_ref("vehicle.attitude.omega");
    let substitutions = [
        Substitution {
            var_name: VarName::new("vehicle.attitude.omega[1]"),
            var_ref: Some(reference("vehicle.attitude.omega[1]")),
            expr: var_ref("vehicle.omega[1]"),
            var_dims: Vec::new(),
            replacement_dims: Vec::new(),
            env_keys: Vec::new(),
        },
        Substitution {
            var_name: VarName::new("vehicle.attitude.omega[2]"),
            var_ref: Some(reference("vehicle.attitude.omega[2]")),
            expr: var_ref("vehicle.omega[2]"),
            var_dims: Vec::new(),
            replacement_dims: Vec::new(),
            env_keys: Vec::new(),
        },
        Substitution {
            var_name: VarName::new("vehicle.attitude.omega[3]"),
            var_ref: Some(reference("vehicle.attitude.omega[3]")),
            expr: var_ref("vehicle.omega[3]"),
            var_dims: Vec::new(),
            replacement_dims: Vec::new(),
            env_keys: Vec::new(),
        },
    ];
    let result = structural_ok(apply_substitutions_to_expr(&expr, &substitutions));

    assert!(
        matches!(
            result,
            Expression::VarRef { name, subscripts, .. }
                if name.as_str() == "vehicle.omega" && subscripts.is_empty()
        ),
        "complete scalar alias groups should rewrite aggregate vector arguments"
    );
}

#[test]
fn test_complete_scalar_alias_group_without_exact_var_ref_rewrites_aggregate_function_argument() {
    let expr = var_ref("vehicle.attitude.omega");
    let substitutions = [
        Substitution {
            var_name: VarName::new("vehicle.attitude.omega[1]"),
            var_ref: None,
            expr: var_ref("vehicle.omega[1]"),
            var_dims: Vec::new(),
            replacement_dims: Vec::new(),
            env_keys: Vec::new(),
        },
        Substitution {
            var_name: VarName::new("vehicle.attitude.omega[2]"),
            var_ref: None,
            expr: var_ref("vehicle.omega[2]"),
            var_dims: Vec::new(),
            replacement_dims: Vec::new(),
            env_keys: Vec::new(),
        },
        Substitution {
            var_name: VarName::new("vehicle.attitude.omega[3]"),
            var_ref: None,
            expr: var_ref("vehicle.omega[3]"),
            var_dims: Vec::new(),
            replacement_dims: Vec::new(),
            env_keys: Vec::new(),
        },
    ];
    let result = structural_ok(apply_substitutions_to_expr(&expr, &substitutions));

    assert!(
        matches!(
            result,
            Expression::VarRef { name, subscripts, .. }
                if name.as_str() == "vehicle.omega" && subscripts.is_empty()
        ),
        "complete scalar alias groups from scalarized names should rewrite aggregate vector arguments"
    );
}

#[test]
fn test_single_scalar_alias_does_not_rewrite_aggregate_argument() {
    let expr = var_ref("vehicle.attitude.omega");
    let substitutions = [Substitution {
        var_name: VarName::new("vehicle.attitude.omega[1]"),
        var_ref: Some(reference("vehicle.attitude.omega[1]")),
        expr: var_ref("vehicle.omega[1]"),
        var_dims: Vec::new(),
        replacement_dims: Vec::new(),
        env_keys: Vec::new(),
    }];
    let result = structural_ok(apply_substitutions_to_expr(&expr, &substitutions));

    assert!(
        matches!(
            result,
            Expression::VarRef { name, subscripts, .. }
                if name.as_str() == "vehicle.attitude.omega" && subscripts.is_empty()
        ),
        "one scalar alias is not enough evidence to rewrite the aggregate"
    );
}

#[test]
fn test_scalarized_alias_without_exact_var_ref_does_not_rewrite_aggregate_argument() {
    let expr = var_ref("vehicle.attitude.omega");
    let substitutions = [Substitution {
        var_name: VarName::new("vehicle.attitude.omega[1]"),
        var_ref: None,
        expr: var_ref("vehicle.omega[1]"),
        var_dims: Vec::new(),
        replacement_dims: Vec::new(),
        env_keys: Vec::new(),
    }];
    let result = structural_ok(apply_substitutions_to_expr(&expr, &substitutions));

    assert!(
        matches!(
            result,
            Expression::VarRef { name, subscripts, .. }
                if name.as_str() == "vehicle.attitude.omega" && subscripts.is_empty()
        ),
        "a scalarized alias target without exact metadata must not collapse an aggregate argument"
    );
}

#[test]
fn test_substitute_var_projects_subscripted_aggregate_ref_through_aggregate_alias() {
    let expr = var_ref_idx("aimc.rotorCage.electroMagneticConverter.i", 2);
    let result = structural_ok(apply_substitutions_to_expr(
        &expr,
        &[Substitution {
            var_name: VarName::new("aimc.rotorCage.electroMagneticConverter.i"),
            var_ref: Some(reference("aimc.rotorCage.electroMagneticConverter.i")),
            expr: var_ref("aimc.rotorCage.i"),
            var_dims: vec![3],
            replacement_dims: vec![3],
            env_keys: Vec::new(),
        }],
    ));

    assert!(
        matches!(
            result,
            Expression::VarRef { name, subscripts, .. }
                if name.as_str() == "aimc.rotorCage.i"
                    && matches!(subscripts.as_slice(), [rumoca_core::Subscript::Index { value: 2, .. }])
        ),
        "aggregate alias substitution must rewrite existing subscripted refs before removing the aggregate"
    );
}

#[test]
fn test_complete_scalar_alias_group_rewrites_dynamic_aggregate_slice() {
    let expr = Expression::VarRef {
        name: reference("transformerL.i"),
        subscripts: vec![rumoca_core::Subscript::generated_expr(
            Box::new(Expression::Range {
                start: Box::new(Expression::Literal {
                    value: Literal::Integer(1),
                    span: Span::DUMMY,
                }),
                step: None,
                end: Box::new(var_ref("m")),
                span: Span::DUMMY,
            }),
            rumoca_core::Span::DUMMY,
        )],
        span: Span::DUMMY,
    };
    let substitutions = [
        Substitution {
            var_name: VarName::new("transformerL.i[1]"),
            var_ref: Some(reference("transformerL.i[1]")),
            expr: var_ref("transformerL.plug_p.pin[1].i"),
            var_dims: Vec::new(),
            replacement_dims: Vec::new(),
            env_keys: Vec::new(),
        },
        Substitution {
            var_name: VarName::new("transformerL.i[2]"),
            var_ref: Some(reference("transformerL.i[2]")),
            expr: var_ref("transformerL.plug_p.pin[2].i"),
            var_dims: Vec::new(),
            replacement_dims: Vec::new(),
            env_keys: Vec::new(),
        },
        Substitution {
            var_name: VarName::new("transformerL.i[3]"),
            var_ref: Some(reference("transformerL.i[3]")),
            expr: var_ref("transformerL.plug_p.pin[3].i"),
            var_dims: Vec::new(),
            replacement_dims: Vec::new(),
            env_keys: Vec::new(),
        },
    ];

    let result = structural_ok(apply_substitutions_to_expr(&expr, &substitutions));

    assert!(
        matches!(
            result,
            Expression::Index { base, subscripts, .. }
                if matches!(
                    base.as_ref(),
                    Expression::Array { elements, .. } if elements.len() == 3
                ) && matches!(subscripts.as_slice(), [rumoca_core::Subscript::Expr { .. }])
        ),
        "complete scalar alias groups should preserve a dynamic aggregate slice as an index over structured replacement values"
    );
}

#[test]
fn test_substitute_var_keeps_complex_base_when_substituting_field_unknown() {
    let expr = Expression::FieldAccess {
        base: Box::new(var_ref("transferFunction.aSum")),
        field: "im".to_string(),
        span: rumoca_core::Span::DUMMY,
    };
    let result = substitute_var(&expr, &VarName::new("transferFunction.aSum.re"), &lit(99.0));
    match result {
        Expression::FieldAccess { base, field, .. } => {
            assert_eq!(field, "im");
            match base.as_ref() {
                Expression::VarRef {
                    name, subscripts, ..
                } => {
                    assert_eq!(name.as_str(), "transferFunction.aSum");
                    assert!(subscripts.is_empty());
                }
                _ => panic!("expected base VarRef to remain unchanged"),
            }
        }
        _ => panic!("expected FieldAccess to remain unchanged"),
    }
}

#[test]
fn test_substitute_var_nested() {
    let expr = Expression::Unary {
        op: minus_op(),
        rhs: Box::new(Expression::Binary {
            op: add_op(),
            lhs: Box::new(var_ref("z")),
            rhs: Box::new(lit(1.0)),
            span: rumoca_core::Span::DUMMY,
        }),
        span: rumoca_core::Span::DUMMY,
    };
    let result = substitute_var(&expr, &VarName::new("z"), &lit(42.0));
    if let Expression::Unary { rhs, .. } = &result {
        if let Expression::Binary { lhs, .. } = rhs.as_ref() {
            assert!(
                matches!(lhs.as_ref(), Expression::Literal { value: Literal::Real(v), .. } if *v == 42.0)
            );
        } else {
            panic!("expected Binary inside Unary");
        }
    } else {
        panic!("expected Unary");
    }
}

#[test]
fn test_substitute_var_in_if() {
    let expr = Expression::If {
        branches: vec![(
            Expression::Literal {
                value: Literal::Boolean(true),
                span: rumoca_core::Span::DUMMY,
            },
            var_ref("z"),
        )],
        else_branch: Box::new(lit(0.0)),
        span: rumoca_core::Span::DUMMY,
    };
    let result = substitute_var(&expr, &VarName::new("z"), &lit(99.0));
    if let Expression::If { branches, .. } = &result {
        assert!(
            matches!(&branches[0].1, Expression::Literal { value: Literal::Real(v), .. } if *v == 99.0)
        );
    } else {
        panic!("expected If");
    }
}

#[test]
fn test_substitute_var_skips_pre_edge_change_arguments() {
    let expr = Expression::Binary {
        op: add_op(),
        lhs: Box::new(Expression::BuiltinCall {
            function: BuiltinFunction::Pre,
            args: vec![var_ref("z")],
            span: rumoca_core::Span::DUMMY,
        }),
        rhs: Box::new(Expression::Binary {
            op: add_op(),
            lhs: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Edge,
                args: vec![var_ref("z")],
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Change,
                args: vec![var_ref("z")],
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        }),
        span: rumoca_core::Span::DUMMY,
    };
    let result = substitute_var(&expr, &VarName::new("z"), &lit(99.0));
    let Expression::Binary { lhs, rhs, .. } = result else {
        panic!("expected binary expression");
    };
    assert!(
        matches!(
            lhs.as_ref(),
            Expression::BuiltinCall { function: BuiltinFunction::Pre, args, .. }
                if matches!(
                    args.as_slice(),
                    [Expression::VarRef { name, subscripts, .. }]
                        if name.as_str() == "z" && subscripts.is_empty()
                )
        ),
        "pre() argument should remain unchanged"
    );
    let Expression::Binary { lhs, rhs, .. } = rhs.as_ref() else {
        panic!("expected nested binary expression");
    };
    assert!(
        matches!(
            lhs.as_ref(),
            Expression::BuiltinCall { function: BuiltinFunction::Edge, args, .. }
                if matches!(
                    args.as_slice(),
                    [Expression::VarRef { name, subscripts, .. }]
                        if name.as_str() == "z" && subscripts.is_empty()
                )
        ),
        "edge() argument should remain unchanged"
    );
    assert!(
        matches!(
            rhs.as_ref(),
            Expression::BuiltinCall { function: BuiltinFunction::Change, args, .. }
                if matches!(
                    args.as_slice(),
                    [Expression::VarRef { name, subscripts, .. }]
                        if name.as_str() == "z" && subscripts.is_empty()
                )
        ),
        "change() argument should remain unchanged"
    );
}

#[test]
fn test_substitute_var_rewrites_regular_builtin_arguments() {
    let expr = Expression::BuiltinCall {
        function: BuiltinFunction::Sin,
        args: vec![var_ref("z")],
        span: rumoca_core::Span::DUMMY,
    };
    let result = substitute_var(&expr, &VarName::new("z"), &lit(7.0));
    assert!(
        matches!(
            result,
            Expression::BuiltinCall { function: BuiltinFunction::Sin, args, .. }
                if matches!(
                    args.as_slice(),
                    [Expression::Literal { value: Literal::Real(v), span }] if *v == 7.0 && *span == test_span()
                )
        ),
        "regular builtins should still be substituted"
    );
}

// ── expr_contains_var ─────────────────────────────────────────────

#[test]
fn test_expr_contains_var_true() {
    let expr = Expression::Binary {
        op: add_op(),
        lhs: Box::new(var_ref("x")),
        rhs: Box::new(var_ref("z")),
        span: rumoca_core::Span::DUMMY,
    };
    assert!(expr_contains_var(&expr, &VarName::new("z")));
}

#[test]
fn test_expr_contains_var_false() {
    let expr = Expression::Binary {
        op: add_op(),
        lhs: Box::new(var_ref("x")),
        rhs: Box::new(lit(1.0)),
        span: rumoca_core::Span::DUMMY,
    };
    assert!(!expr_contains_var(&expr, &VarName::new("z")));
}

#[test]
fn test_expr_contains_var_in_builtin() {
    let expr = Expression::BuiltinCall {
        function: BuiltinFunction::Sin,
        args: vec![var_ref("z")],
        span: rumoca_core::Span::DUMMY,
    };
    assert!(expr_contains_var(&expr, &VarName::new("z")));
}

#[test]
fn test_expr_contains_var_accepts_embedded_unity_subscript_alias() {
    let expr = var_ref("z[1]");
    assert!(expr_contains_var(&expr, &VarName::new("z")));
}

#[test]
fn test_expr_contains_var_rejects_embedded_non_unity_subscript() {
    let expr = var_ref("z[2]");
    assert!(!expr_contains_var(&expr, &VarName::new("z")));
}

#[test]
fn aggregate_alias_candidate_uses_replacement_reference_span() {
    let mut dae = Dae::new();
    dae.variables
        .algebraics
        .insert(VarName::new("arr"), test_dae_variable("arr"));

    let result = structural_ok(aggregate_alias_candidate(
        &dae,
        &reference("arr"),
        &reference("source"),
        Some(test_span()),
        &IndexSet::new(),
        &HashSet::new(),
    ));

    assert!(
        matches!(
            result,
            Some((name, Expression::VarRef { span, .. }))
                if name.as_str() == "arr" && span == test_span()
        ),
        "aggregate alias replacement should retain source provenance"
    );
}

#[test]
fn aggregate_alias_candidate_rejects_unspanned_replacement() {
    let mut dae = Dae::new();
    dae.variables
        .algebraics
        .insert(VarName::new("arr"), test_dae_variable("arr"));

    match aggregate_alias_candidate(
        &dae,
        &dummy_reference("arr"),
        &dummy_reference("source"),
        None,
        &IndexSet::new(),
        &HashSet::new(),
    ) {
        Err(StructuralError::UnspannedContractViolation { reason }) => assert!(
            reason.contains("without source provenance for replacement `source`"),
            "unexpected reason: {reason}"
        ),
        other => panic!("expected unspanned aggregate alias error, got {other:?}"),
    }
}

#[test]
fn choose_solvable_non_unknown_alias_accepts_real_spanned_residual() {
    let mut dae = Dae::new();
    dae.variables
        .algebraics
        .insert(VarName::new("lhs"), test_dae_variable("lhs"));
    dae.variables
        .algebraics
        .insert(VarName::new("rhs"), test_dae_variable("rhs"));

    let result = structural_ok(choose_solvable_non_unknown_alias_for_elimination(
        &dae,
        &binary(OpBinary::Sub, var_ref("lhs"), var_ref("rhs")),
        &IndexSet::new(),
        &HashSet::new(),
    ));

    assert!(
        matches!(
            result,
            Some((name, Expression::VarRef { name: replacement, .. }))
                if name.as_str() == "lhs" && replacement.as_str() == "rhs"
        ),
        "real-spanned alias residuals should remain eligible for structural elimination"
    );
}

// ── eliminate_trivial ─────────────────────────────────────────────

fn build_test_dae_3eq() -> Dae {
    let mut dae = Dae::new();

    let mut var_x = test_dae_variable("x");
    var_x.start = Some(Expression::Literal {
        value: Literal::Real(1.0),
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables.states.insert(VarName::new("x"), var_x);

    dae.variables
        .algebraics
        .insert(VarName::new("z"), test_dae_variable("z"));

    // ODE: 0 = der(x) - z
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(var_ref("z")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "ode".to_string(),
        scalar_count: 1,
    });

    // Algebraic: 0 = z - x
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("z")),
            rhs: Box::new(var_ref("x")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "alg".to_string(),
        scalar_count: 1,
    });

    dae
}

#[test]
fn test_eliminate_trivial_simple() {
    let mut dae = build_test_dae_3eq();
    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");

    assert!(result.blt_error.is_none());
    assert_eq!(result.n_eliminated, 1);
    assert_eq!(result.substitutions.len(), 1);
    assert_eq!(result.substitutions[0].var_name.as_str(), "z");
    assert_eq!(dae.continuous.equations.len(), 1);
    assert!(!dae.variables.algebraics.contains_key(&VarName::new("z")));
}

#[test]
fn test_eliminate_trivial_preserves_scalarized_matrix_derivative_rows() {
    let mut dae = Dae::new();
    let mut r = test_dae_variable("R");
    r.dims = vec![3, 3];
    r.source_span = test_span();
    dae.variables.states.insert(VarName::new("R"), r);
    let mut skew = test_dae_variable("skew");
    skew.dims = vec![3, 3];
    skew.source_span = test_span();
    dae.variables.algebraics.insert(VarName::new("skew"), skew);

    dae.continuous.equations.push(residual(
        var_ref("skew"),
        array(vec![
            array(vec![real(0.0), real(-1.0), real(0.0)]),
            array(vec![real(1.0), real(0.0), real(0.0)]),
            array(vec![real(0.0), real(0.0), real(0.0)]),
        ]),
        9,
        "binding equation for skew",
    ));
    dae.continuous.equations.push(residual(
        der(var_ref("R")),
        binary(OpBinary::Mul, var_ref("R"), var_ref("skew")),
        9,
        "top-level model equation",
    ));

    crate::scalarize::scalarize_equations(&mut dae).unwrap();
    let result = eliminate_trivial(&mut dae).expect("elimination should not fail structurally");

    assert!(
        result.blt_error.is_none(),
        "scalarized matrix derivative rows must remain matchable: {:?}",
        result.blt_error
    );
    // Scalarized aggregate elements such as `skew[i,j]` stay un-eliminated:
    // standalone substitution of matrix elements corrupted `der` rows, so
    // elimination skips them and the 9 binding rows remain alongside the 9
    // derivative rows.
    assert_eq!(dae.continuous.equations.len(), 18);
    assert_eq!(result.n_eliminated, 0);
    for row in 1..=3 {
        for col in 1..=3 {
            assert!(
                dae.continuous.equations.iter().any(|eq| {
                    expr_contains_der_of(&eq.rhs, &VarName::new(format!("R[{row},{col}]")))
                }),
                "missing derivative row for R[{row},{col}]"
            );
        }
    }
}

#[test]
fn test_eliminate_trivial_reports_missing_replacement_metadata() {
    let mut dae = Dae::new();
    dae.variables
        .algebraics
        .insert(VarName::new("z"), test_dae_variable("z"));
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("z")),
            rhs: Box::new(var_ref("missing")),
            span: Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "bad_metadata".to_string(),
        scalar_count: 1,
    });

    let err = eliminate_trivial(&mut dae).expect_err("missing metadata should fail structural IR");
    assert!(matches!(
        err,
        StructuralError::ContractViolation { reason, span }
            if reason.contains("missing DAE variable metadata for `missing`")
                && span == test_span()
    ));
}

#[test]
fn test_eliminate_trivial_reports_blt_singularity() {
    let mut dae = Dae::new();
    dae.variables
        .algebraics
        .insert(VarName::new("a"), test_dae_variable("a"));
    dae.variables
        .algebraics
        .insert(VarName::new("b"), test_dae_variable("b"));
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: add_op(),
            lhs: Box::new(var_ref("a")),
            rhs: Box::new(var_ref("b")),
            span: Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "underdetermined structural block".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");

    assert!(matches!(
        result.blt_error,
        Some(StructuralError::Singular { .. })
    ));
    assert_eq!(result.n_eliminated, 0);
    assert!(result.substitutions.is_empty());
}

#[test]
fn test_eliminate_trivial_chain() {
    let mut dae = Dae::new();

    let mut var_x = test_dae_variable("x");
    var_x.start = Some(Expression::Literal {
        value: Literal::Real(1.0),
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables.states.insert(VarName::new("x"), var_x);
    dae.variables
        .algebraics
        .insert(VarName::new("a"), test_dae_variable("a"));
    dae.variables
        .algebraics
        .insert(VarName::new("b"), test_dae_variable("b"));

    // ODE: 0 = der(x) - b
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(var_ref("b")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "ode".to_string(),
        scalar_count: 1,
    });

    // 0 = a - x  (a = x)
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("a")),
            rhs: Box::new(var_ref("x")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "alg1".to_string(),
        scalar_count: 1,
    });

    // 0 = b - a  (b = a)
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("b")),
            rhs: Box::new(var_ref("a")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "alg2".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");

    assert_eq!(result.n_eliminated, 2);
    assert_eq!(dae.continuous.equations.len(), 1);
    assert!(dae.variables.algebraics.is_empty());
}

#[test]
fn test_eliminate_trivial_alias_pair_two_unknowns() {
    let mut dae = Dae::new();
    dae.variables
        .algebraics
        .insert(VarName::new("a"), test_dae_variable("a"));
    dae.variables
        .algebraics
        .insert(VarName::new("b"), test_dae_variable("b"));

    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("a")),
            rhs: Box::new(var_ref("b")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "alias".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");
    assert_eq!(result.n_eliminated, 1);
    assert_eq!(result.substitutions.len(), 1);
    assert_eq!(dae.continuous.equations.len(), 0);
    assert_eq!(dae.variables.algebraics.len(), 1);
    assert!(
        dae.variables.algebraics.contains_key(&VarName::new("a"))
            || dae.variables.algebraics.contains_key(&VarName::new("b"))
    );
}

#[test]
fn test_eliminate_trivial_alias_pair_prefers_output_elimination() {
    let mut dae = Dae::new();
    dae.variables
        .algebraics
        .insert(VarName::new("z"), test_dae_variable("z"));
    dae.variables
        .outputs
        .insert(VarName::new("y"), test_dae_variable("y"));

    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("y")),
            rhs: Box::new(var_ref("z")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "output_alias".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");
    assert_eq!(result.n_eliminated, 1);
    assert_eq!(result.substitutions.len(), 1);
    assert_eq!(result.substitutions[0].var_name.as_str(), "y");
    assert!(!dae.variables.outputs.contains_key(&VarName::new("y")));
    assert!(dae.variables.algebraics.contains_key(&VarName::new("z")));
}

#[test]
fn test_eliminate_trivial_alias_keeps_output_with_own_definition() {
    let mut dae = Dae::new();
    dae.variables
        .algebraics
        .insert(VarName::new("z"), test_dae_variable("z"));
    dae.variables
        .outputs
        .insert(VarName::new("y"), test_dae_variable("y"));

    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("y")),
            rhs: Box::new(real(2.0)),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "output_definition".to_string(),
        scalar_count: 1,
    });
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("z")),
            rhs: Box::new(var_ref("y")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "alias_to_defined_output".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");
    assert!(
        result
            .substitutions
            .iter()
            .any(|sub| sub.var_name.as_str() == "z")
    );
    assert!(dae.variables.outputs.contains_key(&VarName::new("y")));
    assert!(!dae.variables.algebraics.contains_key(&VarName::new("z")));
}

#[test]
fn test_eliminate_trivial_keeps_fixed_alias_unknown() {
    let mut dae = Dae::new();
    let mut fixed = test_dae_variable("y");
    fixed.fixed = Some(true);
    fixed.start = Some(lit(0.0));
    dae.variables.algebraics.insert(VarName::new("y"), fixed);
    dae.variables
        .algebraics
        .insert(VarName::new("z"), test_dae_variable("z"));

    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("y")),
            rhs: Box::new(var_ref("z")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "fixed_alias".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");
    assert_eq!(result.n_eliminated, 1);
    assert_eq!(result.substitutions.len(), 1);
    assert_eq!(result.substitutions[0].var_name.as_str(), "z");
    assert!(dae.variables.algebraics.contains_key(&VarName::new("y")));
    assert!(!dae.variables.algebraics.contains_key(&VarName::new("z")));
}

mod boundary_cases;
