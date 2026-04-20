//! Pipeline integration tests for the Rumoca compiler.
//!
//! Tests the full compilation pipeline using the Session API.

use rumoca_ir_dae::{self as dae, Dae, VarName as DaeVarName};
use rumoca_ir_flat as flat;
use rumoca_phase_codegen::{render_flat_template_with_name, render_template, templates};
use rumoca_phase_solve::{BltBlock, analyze_structure, sort_dae};
use rumoca_session::compile::PhaseResult;
use rumoca_session::{Session, SessionConfig};

/// Helper to run the full pipeline on a model using Session.
fn compile_model(source: &str, model_name: &str) -> Result<Dae, String> {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .map_err(|e| format!("Parse/resolve/typecheck error: {:?}", e))?;

    let result = session
        .compile_model(model_name)
        .map_err(|e| format!("Compile error: {:?}", e))?;

    Ok(result.dae)
}

fn subscript_expr_contains_var(subscripts: &[dae::Subscript], var_name: &str) -> bool {
    subscripts.iter().any(|sub| match sub {
        dae::Subscript::Expr(sub_expr) => expr_contains_var(sub_expr, var_name),
        _ => false,
    })
}

fn expr_contains_var(expr: &dae::Expression, var_name: &str) -> bool {
    match expr {
        dae::Expression::VarRef { name, .. } => name.as_str() == var_name,
        dae::Expression::Binary { lhs, rhs, .. } => {
            expr_contains_var(lhs, var_name) || expr_contains_var(rhs, var_name)
        }
        dae::Expression::Unary { rhs, .. } => expr_contains_var(rhs, var_name),
        dae::Expression::BuiltinCall { args, .. } | dae::Expression::FunctionCall { args, .. } => {
            args.iter().any(|arg| expr_contains_var(arg, var_name))
        }
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(cond, value)| {
                expr_contains_var(cond, var_name) || expr_contains_var(value, var_name)
            }) || expr_contains_var(else_branch, var_name)
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            elements.iter().any(|e| expr_contains_var(e, var_name))
        }
        dae::Expression::Range { start, step, end } => {
            expr_contains_var(start, var_name)
                || step
                    .as_ref()
                    .is_some_and(|s| expr_contains_var(s, var_name))
                || expr_contains_var(end, var_name)
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expr_contains_var(expr, var_name)
                || indices
                    .iter()
                    .any(|range_idx| expr_contains_var(&range_idx.range, var_name))
                || filter
                    .as_ref()
                    .is_some_and(|f| expr_contains_var(f, var_name))
        }
        dae::Expression::Index { base, subscripts } => {
            expr_contains_var(base, var_name) || subscript_expr_contains_var(subscripts, var_name)
        }
        dae::Expression::FieldAccess { base, .. } => expr_contains_var(base, var_name),
        dae::Expression::Literal(_) | dae::Expression::Empty => false,
    }
}

fn expr_if_branch_mentions_var(expr: &dae::Expression, var_name: &str) -> bool {
    match expr {
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches
                .iter()
                .any(|(_, value)| expr_contains_var(value, var_name))
                || expr_contains_var(else_branch, var_name)
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            expr_if_branch_mentions_var(lhs, var_name) || expr_if_branch_mentions_var(rhs, var_name)
        }
        dae::Expression::Unary { rhs, .. } => expr_if_branch_mentions_var(rhs, var_name),
        dae::Expression::BuiltinCall { args, .. } | dae::Expression::FunctionCall { args, .. } => {
            args.iter()
                .any(|arg| expr_if_branch_mentions_var(arg, var_name))
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => elements
            .iter()
            .any(|e| expr_if_branch_mentions_var(e, var_name)),
        dae::Expression::Range { start, step, end } => {
            expr_if_branch_mentions_var(start, var_name)
                || step
                    .as_ref()
                    .is_some_and(|s| expr_if_branch_mentions_var(s, var_name))
                || expr_if_branch_mentions_var(end, var_name)
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expr_if_branch_mentions_var(expr, var_name)
                || indices
                    .iter()
                    .any(|range_idx| expr_if_branch_mentions_var(&range_idx.range, var_name))
                || filter
                    .as_ref()
                    .is_some_and(|f| expr_if_branch_mentions_var(f, var_name))
        }
        dae::Expression::Index { base, subscripts } => {
            expr_if_branch_mentions_var(base, var_name)
                || subscripts.iter().any(|sub| match sub {
                    dae::Subscript::Expr(sub_expr) => {
                        expr_if_branch_mentions_var(sub_expr, var_name)
                    }
                    _ => false,
                })
        }
        dae::Expression::FieldAccess { base, .. } => expr_if_branch_mentions_var(base, var_name),
        dae::Expression::VarRef { .. } | dae::Expression::Literal(_) | dae::Expression::Empty => {
            false
        }
    }
}

// =============================================================================
// Basic pipeline tests
// =============================================================================

#[test]
fn test_empty_model() {
    let source = r#"
model Empty
end Empty;
"#;
    let result = compile_model(source, "Empty");
    assert!(
        result.is_ok(),
        "Empty model should compile: {:?}",
        result.err()
    );

    let dae = result.unwrap();
    assert!(
        rumoca_eval_dae::analysis::is_balanced(&dae),
        "Empty model should be balanced"
    );
    assert_eq!(rumoca_eval_dae::analysis::balance(&dae), 0);
}

#[test]
fn test_simple_parameter() {
    let source = r#"
model SimpleParam
    parameter Real k = 1.0;
end SimpleParam;
"#;
    let result = compile_model(source, "SimpleParam");
    assert!(
        result.is_ok(),
        "Parameter model should compile: {:?}",
        result.err()
    );

    let dae = result.unwrap();
    println!(
        "SimpleParam: params={}, algebraics={}, balance={}",
        dae.parameters.len(),
        dae.algebraics.len(),
        rumoca_eval_dae::analysis::balance(&dae)
    );
}

#[test]
fn test_simple_variable() {
    let source = r#"
model SimpleVar
    Real x;
equation
    x = 1.0;
end SimpleVar;
"#;
    let result = compile_model(source, "SimpleVar");
    assert!(
        result.is_ok(),
        "Variable model should compile: {:?}",
        result.err()
    );

    let dae = result.unwrap();
    // One algebraic variable with one equation - balanced
    println!(
        "SimpleVar: {} states, {} algebraics, balance = {}",
        dae.states.len(),
        dae.algebraics.len(),
        rumoca_eval_dae::analysis::balance(&dae)
    );
}

#[test]
fn test_simple_equation() {
    let source = r#"
model SimpleEq
    Real x;
equation
    x = 1.0;
end SimpleEq;
"#;
    let result = compile_model(source, "SimpleEq");
    assert!(
        result.is_ok(),
        "Equation model should compile: {:?}",
        result.err()
    );

    let dae = result.unwrap();
    println!(
        "SimpleEq: states={}, algebraics={}, f_x={}, balance={}",
        dae.states.len(),
        dae.algebraics.len(),
        dae.f_x.len(),
        rumoca_eval_dae::analysis::balance(&dae)
    );
}

#[test]
fn test_inverse_block_constraints_style_input_alias_kept() {
    let source = r#"
model InverseLike
    input Real u1;
    input Real u2;
    output Real y1;
    output Real y2;
equation
    u1 = u2;
    y1 = y2;
end InverseLike;
"#;
    let result = compile_model(source, "InverseLike");
    assert!(
        result.is_ok(),
        "InverseLike should compile: {:?}",
        result.err()
    );

    let dae = result.unwrap();
    assert_eq!(
        dae.f_x.len(),
        2,
        "both equations should be preserved, including input alias constraint"
    );
    assert_eq!(
        rumoca_eval_dae::analysis::balance(&dae),
        0,
        "model should be structurally balanced"
    );
}

#[test]
fn test_component_for_range_uses_effective_parameter_override() {
    fn expr_refs_index(expr: &dae::Expression, var_name: &str, idx: i64) -> bool {
        let indexed_name = format!("{var_name}[{idx}]");
        let has_idx = |subscripts: &[dae::Subscript]| {
            subscripts
                .iter()
                .any(|sub| matches!(sub, dae::Subscript::Index(v) if *v == idx))
        };

        match expr {
            dae::Expression::VarRef { name, subscripts } => {
                (name.as_str() == var_name && has_idx(subscripts)) || name.as_str() == indexed_name
            }
            dae::Expression::Binary { lhs, rhs, .. } => {
                expr_refs_index(lhs, var_name, idx) || expr_refs_index(rhs, var_name, idx)
            }
            dae::Expression::Unary { rhs, .. } => expr_refs_index(rhs, var_name, idx),
            dae::Expression::BuiltinCall { args, .. }
            | dae::Expression::FunctionCall { args, .. } => {
                args.iter().any(|arg| expr_refs_index(arg, var_name, idx))
            }
            dae::Expression::If {
                branches,
                else_branch,
            } => {
                branches.iter().any(|(cond, value)| {
                    expr_refs_index(cond, var_name, idx) || expr_refs_index(value, var_name, idx)
                }) || expr_refs_index(else_branch, var_name, idx)
            }
            dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
                elements.iter().any(|e| expr_refs_index(e, var_name, idx))
            }
            dae::Expression::Range { start, step, end } => {
                expr_refs_index(start, var_name, idx)
                    || step
                        .as_ref()
                        .is_some_and(|s| expr_refs_index(s, var_name, idx))
                    || expr_refs_index(end, var_name, idx)
            }
            dae::Expression::ArrayComprehension {
                expr,
                indices,
                filter,
            } => {
                expr_refs_index(expr, var_name, idx)
                    || indices
                        .iter()
                        .any(|range_idx| expr_refs_index(&range_idx.range, var_name, idx))
                    || filter
                        .as_ref()
                        .is_some_and(|f| expr_refs_index(f, var_name, idx))
            }
            dae::Expression::Index { base, subscripts } => {
                expr_refs_index(base, var_name, idx)
                    || subscripts.iter().any(|sub| match sub {
                        dae::Subscript::Expr(sub_expr) => expr_refs_index(sub_expr, var_name, idx),
                        _ => false,
                    })
            }
            dae::Expression::FieldAccess { base, .. } => expr_refs_index(base, var_name, idx),
            dae::Expression::Literal(_) | dae::Expression::Empty => false,
        }
    }

    let source = r#"
model Top
  model Filter
    parameter Integer n = 2;
    Real x[n](each start = 0);
  equation
    der(x[1]) = -x[1];
    for i in 2:n loop
      der(x[i]) = x[i - 1] - x[i];
    end for;
  end Filter;
  Filter f(n = 1);
end Top;
"#;

    let dae = compile_model(source, "Top").expect("Top should compile");
    let offending: Vec<String> = dae
        .f_x
        .iter()
        .filter(|eq| expr_refs_index(&eq.rhs, "f.x", 2))
        .map(|eq| format!("{:?}", eq.rhs))
        .collect();
    assert!(
        offending.is_empty(),
        "for i in 2:n with n=1 must be empty; found f.x[2] references in: {offending:?}"
    );
}

#[test]
fn test_if_equation_with_branch_specific_lhs_preserves_branch_residuals() {
    let source = r#"
model BranchLhsSwitch
  Boolean CV(start = false, fixed = true);
  Real v;
  Real i;
equation
  CV = v >= 4;
  if CV then
    v = 4;
  else
    i = -1;
  end if;
  i = 0;
end BranchLhsSwitch;
"#;

    let dae = compile_model(source, "BranchLhsSwitch").expect("BranchLhsSwitch should compile");
    let if_with_i = dae
        .f_x
        .iter()
        .any(|eq| expr_if_branch_mentions_var(&eq.rhs, "i"));
    assert!(
        if_with_i,
        "if-equation lowering must preserve branch residual variable `i` for the else branch"
    );
}

#[test]
fn test_simple_ode() {
    let source = r#"
model SimpleODE
    Real x(start = 0);
equation
    der(x) = 1.0;
end SimpleODE;
"#;
    let result = compile_model(source, "SimpleODE");
    assert!(
        result.is_ok(),
        "ODE model should compile: {:?}",
        result.err()
    );

    let dae = result.unwrap();
    println!(
        "SimpleODE: states={}, algebraics={}, f_x={}, balance={}",
        dae.states.len(),
        dae.algebraics.len(),
        dae.f_x.len(),
        rumoca_eval_dae::analysis::balance(&dae)
    );

    // Should have 1 state (x) and 1 continuous equation
    assert_eq!(dae.states.len(), 1, "Should have 1 state variable");
    assert_eq!(dae.f_x.len(), 1, "Should have 1 continuous equation");
}

#[test]
fn test_harmonic_oscillator() {
    let source = r#"
model Oscillator
    Real x(start = 1);
    Real v(start = 0);
    parameter Real k = 1.0;
    parameter Real m = 1.0;
equation
    der(x) = v;
    m * der(v) = -k * x;
end Oscillator;
"#;
    let result = compile_model(source, "Oscillator");
    assert!(
        result.is_ok(),
        "Oscillator model should compile: {:?}",
        result.err()
    );

    let dae = result.unwrap();
    println!(
        "Oscillator: states={}, algebraics={}, params={}, f_x={}, balance={}",
        dae.states.len(),
        dae.algebraics.len(),
        dae.parameters.len(),
        dae.f_x.len(),
        rumoca_eval_dae::analysis::balance(&dae)
    );

    // State detection works - variables appearing in der() are states
    assert_eq!(dae.states.len(), 2, "Should have 2 state variables (x, v)");
    assert_eq!(dae.f_x.len(), 2, "Should have 2 continuous equations");
}

// =============================================================================
// Pipeline phase tests using Session
// =============================================================================

#[test]
fn test_parse_and_resolve_phase() {
    let source = "model Test Real x; end Test;";
    let mut session = Session::new(SessionConfig::default());
    let result = session.add_document("test.mo", source);
    assert!(result.is_ok(), "Parse/resolve should succeed");

    let resolved = session.resolved();
    assert!(resolved.is_ok(), "Should have resolved tree");
}

#[test]
fn test_compile_phase() {
    let source = "model Test Real x; equation x = 1; end Test;";
    let mut session = Session::new(SessionConfig::default());
    session.add_document("test.mo", source).unwrap();

    let result = session.compile_model("Test");
    assert!(result.is_ok(), "Compile should succeed");
}

#[test]
fn test_colon_binding_with_indexed_rhs_keeps_1d_dims() {
    let source = r#"
model DimProbe
  parameter Real eta[:] = {1.0, 2.0, 3.0};
  Real mflow_test = 10.0;
  Real m_flow[:] = {
    mflow_test * eta[1] / eta[3],
    mflow_test * eta[2] / eta[3],
    mflow_test
  };
end DimProbe;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse/resolve/typecheck failed");

    let result = session
        .compile_model("DimProbe")
        .expect("compile should succeed");

    let flat_dims = result
        .flat
        .variables
        .get(&flat::VarName::new("m_flow"))
        .map(|v| v.dims.clone())
        .expect("m_flow should exist in flat variables");
    assert_eq!(
        flat_dims,
        vec![3],
        "m_flow[:] binding with indexed eta[...] elements must remain 1D"
    );

    let dae_dims = result
        .dae
        .algebraics
        .get(&DaeVarName::new("m_flow"))
        .map(|v| v.dims.clone())
        .expect("m_flow should exist in DAE algebraics");
    assert_eq!(dae_dims, vec![3], "DAE m_flow dimensions should stay 1D");
}

#[test]
fn test_real_range_colon_dimension_inference_for_size_bound_array() {
    let source = r#"
model RealRangeDims
  parameter Real xsi[:] = 0:0.02:1.0;
  parameter Real T[size(xsi,1)] = 0:0.02:1.0;
  Real y;
equation
  y = T[1];
end RealRangeDims;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse/resolve/typecheck failed");

    let result = session
        .compile_model("RealRangeDims")
        .expect("compile should succeed for real-range colon dimensions");

    let xsi_dims = result
        .flat
        .variables
        .get(&flat::VarName::new("xsi"))
        .map(|v| v.dims.clone())
        .expect("xsi should exist in flat variables");
    assert_eq!(
        xsi_dims,
        vec![51],
        "xsi range 0:0.02:1.0 should infer length 51"
    );

    let t_dims = result
        .flat
        .variables
        .get(&flat::VarName::new("T"))
        .map(|v| v.dims.clone())
        .expect("T should exist in flat variables");
    assert_eq!(
        t_dims,
        vec![51],
        "T[size(xsi,1)] should resolve once xsi colon dimensions are inferred"
    );
}

#[test]
fn test_nested_modifier_colon_dimension_feeds_sibling_size() {
    let source = r#"
package ET004Fixture
  model Curve
    parameter Real V_flow[:];
    parameter Real dp[size(V_flow, 1)];
  end Curve;

  model Performance
    parameter Real q_flow[:] = {1.0, 2.0, 3.0};
    Curve pressure(V_flow=q_flow, dp={10.0, 20.0, 30.0});
  end Performance;

  model Mover
    parameter Real VolFloCur[:] = {1.0, 2.0, 3.0};
    Performance per(q_flow=VolFloCur);
    Real y;
  equation
    y = per.pressure.dp[1];
  end Mover;
end ET004Fixture;
"#;

    let result = compile_model(source, "ET004Fixture.Mover")
        .expect("nested modifier colon dimensions should feed sibling size()");

    let v_flow_dims = result
        .parameters
        .get(&DaeVarName::new("per.pressure.V_flow"))
        .map(|v| v.dims.clone())
        .expect("per.pressure.V_flow should exist in DAE parameters");
    assert_eq!(
        v_flow_dims,
        vec![3],
        "V_flow[:] should infer dimensions from the active modifier binding"
    );

    let dp_dims = result
        .parameters
        .get(&DaeVarName::new("per.pressure.dp"))
        .map(|v| v.dims.clone())
        .expect("per.pressure.dp should exist in DAE parameters");
    assert_eq!(
        dp_dims,
        vec![3],
        "dp[size(V_flow, 1)] should resolve through the sibling V_flow dimension"
    );
}

#[test]
fn test_array_component_modifier_colon_dimension_feeds_sibling_size() {
    let source = r#"
package ET004ArrayFixture
  model Curve
    parameter Real V_flow[:];
    parameter Real dp[size(V_flow, 1)];
  end Curve;

  model Mover
    parameter Real VolFloCur[:] = {1.0, 2.0, 3.0};
    Curve pressure(V_flow=VolFloCur, dp={10.0, 20.0, 30.0});
  end Mover;

  model Plant
    parameter Real curves[2, 3] = [1.0, 2.0, 3.0; 4.0, 5.0, 6.0];
    Mover pum[2](VolFloCur=curves);
    Real y;
  equation
    y = pum[2].pressure.dp[1];
  end Plant;
end ET004ArrayFixture;
"#;

    let result = compile_model(source, "ET004ArrayFixture.Plant")
        .expect("array component modifiers should feed sibling size()");

    let v_flow_dims = result
        .parameters
        .get(&DaeVarName::new("pum[2].pressure.V_flow"))
        .map(|v| v.dims.clone())
        .expect("pum[2].pressure.V_flow should exist in DAE parameters");
    assert_eq!(
        v_flow_dims,
        vec![3],
        "V_flow[:] should infer dimensions after array-component modifier distribution"
    );

    let dp_dims = result
        .parameters
        .get(&DaeVarName::new("pum[2].pressure.dp"))
        .map(|v| v.dims.clone())
        .expect("pum[2].pressure.dp should exist in DAE parameters");
    assert_eq!(
        dp_dims,
        vec![3],
        "dp[size(V_flow, 1)] should resolve for the distributed array element"
    );
}

#[test]
fn test_extends_forwarded_modifier_dotted_size_feeds_sibling_dimension() {
    let source = r#"
package ET004DottedFixture
  model Curve
    parameter Real V_flow[:];
    parameter Real eta[size(V_flow, 1)];
  end Curve;

  model PerformanceBase
    parameter Real flo[:];
    Curve motorEfficiency(V_flow=flo, eta=fill(1.0, size(flo, 1)));
  end PerformanceBase;

  model Performance
    extends PerformanceBase;
  end Performance;

  model MoverBase
    parameter Real VolFloCur[:];
    Performance per(flo=VolFloCur);
    parameter Integer n = size(per.motorEfficiency.V_flow, 1);
    parameter Real eff[n] = per.motorEfficiency.eta;
  end MoverBase;

  model Mover
    extends MoverBase(VolFloCur={1.0, 2.0, 3.0});
    Real y;
  equation
    y = eff[1];
  end Mover;
end ET004DottedFixture;
"#;

    let result = compile_model(source, "ET004DottedFixture.Mover").expect(
        "extends-forwarded nested modifier dimensions should feed dotted size() in active scope",
    );

    let v_flow_dims = result
        .parameters
        .get(&DaeVarName::new("per.motorEfficiency.V_flow"))
        .map(|v| v.dims.clone())
        .expect("per.motorEfficiency.V_flow should exist in DAE parameters");
    assert_eq!(
        v_flow_dims,
        vec![3],
        "nested V_flow[:] should infer dimensions from the forwarded modifier"
    );

    let eff_dims = result
        .parameters
        .get(&DaeVarName::new("eff"))
        .map(|v| v.dims.clone())
        .expect("eff should exist in DAE parameters");
    assert_eq!(
        eff_dims,
        vec![3],
        "eff[size(per.motorEfficiency.V_flow, 1)] should resolve in MoverBase scope"
    );
}

#[test]
fn test_array_comprehension_function_call_equation_preserves_dependencies() {
    use std::collections::HashSet;

    let source = r#"
package P
  function f
    input Real x;
    output Real y;
  algorithm
    y := x;
  end f;

  model ArrayCompFunctionEq
    parameter Integer n = 1;
    parameter Real u[n] = ones(n);
    Real y[n];
  equation
    y = {P.f(u[i]) for i in 1:n};
  end ArrayCompFunctionEq;
end P;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse/resolve/typecheck failed");

    let phase_result = session
        .compile_model_phases("P.ArrayCompFunctionEq")
        .expect("phase compilation should succeed");
    let result = match phase_result {
        PhaseResult::Success(result) => result,
        other => panic!(
            "expected successful phase result, got {:?}",
            std::mem::discriminant(&other)
        ),
    };

    let mut function_names = HashSet::new();
    let mut references_u = false;
    for eq in &result.flat.equations {
        let mut refs = HashSet::new();
        eq.residual.collect_var_refs(&mut refs);
        if refs
            .iter()
            .any(|name| name.as_str() == "u" || name.as_str().starts_with("u["))
        {
            references_u = true;
        }

        let mut collector = flat::visitor::FunctionCallCollector::new();
        flat::visitor::ExpressionVisitor::visit_expression(&mut collector, &eq.residual);
        function_names.extend(collector.into_names());
    }

    assert!(
        references_u,
        "array-comprehension equation must preserve var references from comprehension body"
    );
    assert!(
        function_names
            .iter()
            .any(|name| name == "P.f" || name.ends_with(".f")),
        "array-comprehension equation must preserve function call expression, found: {:?}",
        function_names
    );
}

#[test]
fn test_array_comprehension_with_size_parameter_preserves_dependencies() {
    use std::collections::HashSet;

    let source = r#"
package P
  record R
    Real a;
  end R;

  function f
    input R r;
    input Real x;
    output Real y;
  algorithm
    y := r.a + x;
  end f;

  model ArrayCompSizeEq
    parameter Real K[1] = {0};
    parameter Integer n = size(K, 1);
    parameter Real u[n] = ones(n);
    R r[n](each a = 1);
    Real y[n];
  equation
    y = {P.f(r[i], u[i]) for i in 1:n};
  end ArrayCompSizeEq;
end P;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse/resolve/typecheck failed");

    let phase_result = session
        .compile_model_phases("P.ArrayCompSizeEq")
        .expect("phase compilation should succeed");
    let result = match phase_result {
        PhaseResult::Success(result) => result,
        other => panic!(
            "expected successful phase result, got {:?}",
            std::mem::discriminant(&other)
        ),
    };

    let mut function_names = HashSet::new();
    let mut references_u = false;
    let mut references_r = false;
    for eq in &result.flat.equations {
        let mut refs = HashSet::new();
        eq.residual.collect_var_refs(&mut refs);
        if refs
            .iter()
            .any(|name| name.as_str() == "u" || name.as_str().starts_with("u["))
        {
            references_u = true;
        }
        if refs
            .iter()
            .any(|name| name.as_str() == "r" || name.as_str().starts_with("r["))
        {
            references_r = true;
        }

        let mut collector = flat::visitor::FunctionCallCollector::new();
        flat::visitor::ExpressionVisitor::visit_expression(&mut collector, &eq.residual);
        function_names.extend(collector.into_names());
    }

    assert!(
        references_u,
        "array-comprehension equation must preserve references to u from comprehension body"
    );
    assert!(
        references_r,
        "array-comprehension equation must preserve references to r from comprehension body"
    );
    assert!(
        function_names
            .iter()
            .any(|name| name == "P.f" || name.ends_with(".f")),
        "array-comprehension equation must preserve function call expression, found: {:?}",
        function_names
    );
}

#[test]
fn test_binding_equation_kept_when_explicit_rhs_refs_subscripted_unknowns() {
    let source = r#"
model KeepBindingWithSubscriptedUnknown
  parameter Integer n = 1;
  Real input_mdot[n];
  Real input_dp[n] = ones(n);
  Real DP[n] = {input_dp[i] for i in 1:n};
equation
  DP = {input_mdot[i] for i in 1:n};
end KeepBindingWithSubscriptedUnknown;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse/resolve/typecheck failed");

    let phase_result = session
        .compile_model_phases("KeepBindingWithSubscriptedUnknown")
        .expect("phase compilation should succeed");
    let result = match phase_result {
        PhaseResult::Success(result) => result,
        other => panic!(
            "expected successful phase result, got {:?}",
            std::mem::discriminant(&other)
        ),
    };

    assert_eq!(
        rumoca_eval_dae::analysis::balance(&result.dae),
        0,
        "model should remain balanced when declaration binding and explicit equation both contribute constraints"
    );
    assert!(
        result
            .dae
            .f_x
            .iter()
            .any(|eq| eq.origin.contains("binding equation for DP")),
        "DP declaration binding equation should be preserved in DAE"
    );
}

#[test]
fn test_component_modifier_binding_does_not_leak_to_nested_member() {
    let source = r#"
connector C
  Real x;
end C;

model A
  parameter Real x = 1;
  C c;
equation
  c.x = x;
end A;

model ModifierBindingScope
  A a(x = 2);
end ModifierBindingScope;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse/resolve/typecheck failed");

    let phase_result = session
        .compile_model_phases("ModifierBindingScope")
        .expect("phase compilation should succeed");
    let result = match phase_result {
        PhaseResult::Success(result) => result,
        other => panic!(
            "expected successful phase result, got {:?}",
            std::mem::discriminant(&other)
        ),
    };

    let leaked_binding = result
        .flat
        .variables
        .get(&flat::VarName::new("a.c.x"))
        .and_then(|var| var.binding.as_ref());
    assert!(
        leaked_binding.is_none(),
        "component modifier a(x=2) must not become a declaration binding on nested member a.c.x"
    );
    assert_eq!(
        rumoca_eval_dae::analysis::balance(&result.dae),
        0,
        "model should stay balanced without nested binding leakage"
    );
}

// =============================================================================
// CasADi MX template integration tests
// =============================================================================

/// Compare generated code against an expected output file.
///
/// On first run or when UPDATE_EXPECT=1 is set, writes the expected file.
/// On subsequent runs, compares the generated output against the saved file.
fn assert_matches_expected(generated: &str, expected_path: &str) {
    let expected_file = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/expected")
        .join(expected_path);

    if std::env::var("UPDATE_EXPECT").is_ok() || !expected_file.exists() {
        // Write the expected output
        std::fs::create_dir_all(expected_file.parent().unwrap()).unwrap();
        std::fs::write(&expected_file, generated).unwrap();
        eprintln!("Updated expected output: {}", expected_file.display());
        return;
    }

    let expected = std::fs::read_to_string(&expected_file).unwrap();
    assert_eq!(
        generated,
        expected,
        "\nGenerated output differs from expected file: {}\nRun with UPDATE_EXPECT=1 to update.",
        expected_file.display()
    );
}

#[test]
fn test_casadi_mx_template_oscillator() {
    let source = r#"
model Oscillator
    Real x(start = 1);
    Real v(start = 0);
    parameter Real k = 1.0;
    parameter Real m = 1.0;
equation
    der(x) = v;
    m * der(v) = -k * x;
end Oscillator;
"#;
    let dae = compile_model(source, "Oscillator").unwrap();
    let code = render_template(&dae, templates::CASADI_MX).unwrap();
    assert_matches_expected(&code, "oscillator_mx.py");
}

#[test]
fn test_casadi_mx_template_with_function() {
    let source = r#"
function sq
    input Real x;
    output Real y;
algorithm
    y := x * x;
end sq;

model WithFunc
    Real x(start = 1.0);
equation
    der(x) = -sq(x);
end WithFunc;
"#;
    let dae = compile_model(source, "WithFunc").unwrap();
    let code = render_template(&dae, templates::CASADI_MX).unwrap();
    assert_matches_expected(&code, "withfunc_mx.py");
}

#[test]
fn test_casadi_mx_template_dae() {
    let source = r#"
model DaeModel
    Real x(start = 1.0);
    Real y;
    parameter Real k = 2.0;
    parameter Real c = 0.5;
equation
    der(x) = -k * y;
    y = c * x;
end DaeModel;
"#;
    let dae = compile_model(source, "DaeModel").unwrap();
    let code = render_template(&dae, templates::CASADI_MX).unwrap();
    assert_matches_expected(&code, "dae_model_mx.py");
}

#[test]
fn test_casadi_mx_template_algorithm() {
    let source = r#"
model AlgorithmModel
    Real x(start = 1.0);
    Real y;
    parameter Real k = 2.0;
algorithm
    y := k * x;
equation
    der(x) = -y;
end AlgorithmModel;
"#;
    let dae = compile_model(source, "AlgorithmModel").unwrap();
    let code = render_template(&dae, templates::CASADI_MX).unwrap();
    assert_matches_expected(&code, "algorithm_mx.py");
}

// =============================================================================
// Structural analysis integration tests
// =============================================================================

#[test]
fn test_structural_analysis_simple_ode() {
    let source = r#"
model SimpleODE
    Real x(start = 0);
equation
    der(x) = 1.0;
end SimpleODE;
"#;
    let dae = compile_model(source, "SimpleODE").unwrap();
    let analysis = analyze_structure(&dae);

    assert_eq!(analysis.n_equations, 1);
    assert_eq!(analysis.n_unknowns, 1);
    assert_eq!(analysis.matching_size, 1, "perfect matching");
    assert!(
        analysis.diagnostics.is_empty(),
        "simple ODE should have no structural issues"
    );
    assert!(
        analysis.algebraic_loops.is_empty(),
        "no algebraic loops in simple ODE"
    );
}

#[test]
fn test_structural_analysis_oscillator() {
    let source = r#"
model Oscillator
    Real x(start = 1);
    Real v(start = 0);
    parameter Real k = 1.0;
    parameter Real m = 1.0;
equation
    der(x) = v;
    m * der(v) = -k * x;
end Oscillator;
"#;
    let dae = compile_model(source, "Oscillator").unwrap();
    let analysis = analyze_structure(&dae);

    assert_eq!(analysis.n_equations, 2);
    assert_eq!(analysis.n_unknowns, 2);
    assert_eq!(analysis.matching_size, 2, "perfect matching for oscillator");
    assert!(
        analysis.unmatched_unknowns.is_empty(),
        "no unmatched unknowns"
    );
    assert!(
        analysis.unmatched_equations.is_empty(),
        "no unmatched equations"
    );
}

#[test]
fn test_structural_analysis_dae_model() {
    let source = r#"
model DaeModel
    Real x(start = 1.0);
    Real y;
    parameter Real k = 2.0;
    parameter Real c = 0.5;
equation
    der(x) = -k * y;
    y = c * x;
end DaeModel;
"#;
    let dae = compile_model(source, "DaeModel").unwrap();
    let analysis = analyze_structure(&dae);

    assert_eq!(
        analysis.matching_size,
        analysis.n_equations.min(analysis.n_unknowns),
        "should find perfect matching"
    );
    // This system has no algebraic loop because y = c*x can be solved first,
    // then der(x) = -k*y can be computed.
    // However the structural analysis might detect a loop depending on the
    // matching — both equations reference the algebraic variable y.
    println!(
        "DAE analysis: {} eqs, {} unknowns, {} loops, {} warnings",
        analysis.n_equations,
        analysis.n_unknowns,
        analysis.algebraic_loops.len(),
        analysis.diagnostics.len()
    );
}

#[test]
fn test_structural_analysis_with_function() {
    let source = r#"
function sq
    input Real x;
    output Real y;
algorithm
    y := x * x;
end sq;

model WithFunc
    Real x(start = 1.0);
equation
    der(x) = -sq(x);
end WithFunc;
"#;
    let dae = compile_model(source, "WithFunc").unwrap();
    let analysis = analyze_structure(&dae);

    assert_eq!(analysis.n_equations, 1);
    assert_eq!(analysis.n_unknowns, 1);
    assert_eq!(analysis.matching_size, 1, "perfect matching");
    assert!(
        analysis.diagnostics.is_empty(),
        "no structural issues with function model"
    );
}

// =============================================================================
// sort_dae integration tests
// =============================================================================

#[test]
fn test_sort_dae_simple_ode() {
    let source = r#"
model SimpleODE
    Real x(start = 0);
equation
    der(x) = 1.0;
end SimpleODE;
"#;
    let dae = compile_model(source, "SimpleODE").unwrap();
    let sorted = sort_dae(&dae).expect("sort_dae should succeed for simple ODE");

    assert_eq!(sorted.blocks.len(), 1, "one scalar block for one ODE eq");
    assert!(
        matches!(&sorted.blocks[0], BltBlock::Scalar { .. }),
        "single ODE should be a scalar block"
    );
    assert_eq!(sorted.matching.len(), 1);
    assert!(sorted.diagnostics.is_empty(), "no warnings for simple ODE");
}

#[test]
fn test_sort_dae_oscillator() {
    let source = r#"
model Oscillator
    Real x(start = 1);
    Real v(start = 0);
    parameter Real k = 1.0;
    parameter Real m = 1.0;
equation
    der(x) = v;
    m * der(v) = -k * x;
end Oscillator;
"#;
    let dae = compile_model(source, "Oscillator").unwrap();
    let sorted = sort_dae(&dae).expect("sort_dae should succeed for oscillator");

    assert_eq!(sorted.blocks.len(), 2, "two scalar blocks for two ODE eqs");
    assert!(
        sorted
            .blocks
            .iter()
            .all(|b| matches!(b, BltBlock::Scalar { .. })),
        "oscillator should have no algebraic loops"
    );
    assert_eq!(sorted.matching.len(), 2);
    assert!(sorted.diagnostics.is_empty(), "no warnings for oscillator");
}

#[test]
fn test_sort_dae_with_algebraic() {
    let source = r#"
model DaeModel
    Real x(start = 1.0);
    Real y;
    parameter Real k = 2.0;
    parameter Real c = 0.5;
equation
    der(x) = -k * y;
    y = c * x;
end DaeModel;
"#;
    let dae = compile_model(source, "DaeModel").unwrap();
    let sorted = sort_dae(&dae).expect("sort_dae should succeed for DAE model");

    // Two blocks: y = c*x first (scalar), then der(x) = -k*y (scalar)
    assert_eq!(sorted.blocks.len(), 2, "two blocks");
    assert_eq!(sorted.matching.len(), 2);

    // Both blocks should be scalar (no algebraic loop: y can be computed first)
    assert!(
        sorted
            .blocks
            .iter()
            .all(|b| matches!(b, BltBlock::Scalar { .. })),
        "DAE model with causal ordering should have no algebraic loops"
    );
}

/// Test that record field expansion works with replaceable type redeclarations.
///
/// MLS §7.3: When a derived class redeclares a record type (e.g.,
/// `redeclare record extends ThermodynamicState`), components referencing
/// the original type must use the redeclared version with its fields.
/// This is the MSL pattern for Medium.BaseProperties.state.
#[test]
fn test_record_field_expansion_with_redeclaration() {
    let source = r#"
package PartialMedium
  replaceable record ThermodynamicState
  end ThermodynamicState;

  replaceable partial model BaseProperties
    input Real p;
    input Real h;
    Real d;
    Real T;
    ThermodynamicState state;
  end BaseProperties;
end PartialMedium;

package MyMedium
  extends PartialMedium;

  redeclare record extends ThermodynamicState
    Real p;
    Real T;
  end ThermodynamicState;

  redeclare model extends BaseProperties
  equation
    d = 1;
    h = 123456 * T;
    state.p = p;
    state.T = T;
  end BaseProperties;
end MyMedium;
"#;
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session
        .compile_model("MyMedium.BaseProperties")
        .expect("compile failed");

    // Verify record fields are expanded as flat variables
    let var_names: Vec<_> = result
        .flat
        .variables
        .keys()
        .map(|k| k.to_string())
        .collect();
    assert!(
        var_names.iter().any(|n| n == "state.p"),
        "state.p should be a flat variable (record field of redeclared ThermodynamicState)"
    );
    assert!(
        var_names.iter().any(|n| n == "state.T"),
        "state.T should be a flat variable (record field of redeclared ThermodynamicState)"
    );

    // Model should be balanced: 4 unknowns (d, T, state.p, state.T), 4 equations
    assert!(
        rumoca_eval_dae::analysis::is_balanced(&result.dae),
        "Model should be balanced: {}",
        rumoca_eval_dae::analysis::balance_detail(&result.dae)
    );
    assert_eq!(rumoca_eval_dae::analysis::balance(&result.dae), 0);
}

/// MLS §7.3: Record redeclarations inherited through package extends-chains
/// must still apply to components in descendant `BaseProperties` models.
#[test]
fn test_record_field_expansion_with_indirect_package_inheritance() {
    let source = r#"
package PartialMedium
  replaceable record ThermodynamicState
  end ThermodynamicState;

  replaceable partial model BaseProperties
    input Real p;
    input Real h;
    Real d;
    ThermodynamicState state;
  end BaseProperties;
end PartialMedium;

package WaterIF97_base
  extends PartialMedium;

  redeclare record extends ThermodynamicState
    Real h;
    Real p;
  end ThermodynamicState;

  redeclare model extends BaseProperties
  equation
    d = 1;
    h = state.h;
    p = state.p;
  end BaseProperties;
end WaterIF97_base;

package WaterIF97_ph
  extends WaterIF97_base;
end WaterIF97_ph;

package TwoPhaseWater
  extends WaterIF97_ph;

  redeclare model extends BaseProperties
  end BaseProperties;
end TwoPhaseWater;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session
        .compile_model("TwoPhaseWater.BaseProperties")
        .expect("compile failed");

    let var_names: Vec<_> = result
        .flat
        .variables
        .keys()
        .map(|k| k.to_string())
        .collect();
    assert!(
        var_names.iter().any(|n| n == "state.h"),
        "state.h should be expanded from inherited redeclared ThermodynamicState; vars={var_names:?}"
    );
    assert!(
        var_names.iter().any(|n| n == "state.p"),
        "state.p should be expanded from inherited redeclared ThermodynamicState; vars={var_names:?}"
    );

    assert!(
        rumoca_eval_dae::analysis::is_balanced(&result.dae),
        "Model should remain balanced: {}",
        rumoca_eval_dae::analysis::balance_detail(&result.dae)
    );
}

/// MLS §4.6 + §7.3: Dotted member type names must resolve to the nested type,
/// not the containing package, even when the member type is a record redeclared
/// via `redeclare record extends ...`.
#[test]
fn test_dotted_record_type_uses_member_not_package() {
    let source = r#"
package BaseMedium
  constant Integer leak = 42;

  replaceable record ThermodynamicState
  end ThermodynamicState;
end BaseMedium;

package TwoPhaseMedium
  extends BaseMedium;

  redeclare replaceable record extends ThermodynamicState
    Real phase;
  end ThermodynamicState;
end TwoPhaseMedium;

model DottedStateInputs
  replaceable package Medium = TwoPhaseMedium;
  input Medium.ThermodynamicState state_in;
  input Medium.ThermodynamicState state;
  Real y;
equation
  y = state_in.phase + state.phase;
end DottedStateInputs;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session
        .compile_model("DottedStateInputs")
        .expect("compile failed");

    let flat_var_names: Vec<_> = result
        .flat
        .variables
        .keys()
        .map(|k| k.to_string())
        .collect();

    assert!(
        flat_var_names.iter().any(|n| n == "state.phase"),
        "state.phase should be present as a record field"
    );
    assert!(
        flat_var_names.iter().any(|n| n == "state_in.phase"),
        "state_in.phase should be present as a record field"
    );
    assert!(
        !flat_var_names.iter().any(|n| n.starts_with("state.leak")),
        "package constants must not be expanded as state.* fields"
    );
    assert!(
        !flat_var_names
            .iter()
            .any(|n| n.starts_with("state_in.leak")),
        "package constants must not be expanded as state_in.* fields"
    );

    let dae_inputs: Vec<_> = result.dae.inputs.keys().map(|k| k.to_string()).collect();
    assert!(
        dae_inputs.iter().any(|n| n == "state.phase"),
        "state.phase should be tracked as an input scalar"
    );
    assert!(
        dae_inputs.iter().any(|n| n == "state_in.phase"),
        "state_in.phase should be tracked as an input scalar"
    );

    assert!(
        rumoca_eval_dae::analysis::is_balanced(&result.dae),
        "Model should remain balanced: {}",
        rumoca_eval_dae::analysis::balance_detail(&result.dae)
    );
}

/// MLS §4.6 + §7.3: Dotted package-member model types must instantiate the
/// member model (`Medium.BaseProperties`), not the containing package.
#[test]
fn test_dotted_model_type_uses_member_not_package() {
    let source = r#"
package BaseMedium
  constant Real leak = 42;

  replaceable partial model BaseProperties
    Real p;
    Real h;
    Real d;
  equation
    d = p + h;
  end BaseProperties;
end BaseMedium;

package MyMedium
  extends BaseMedium;

  redeclare model extends BaseProperties
    Real t;
  equation
    t = d - p;
  end BaseProperties;
end MyMedium;

model UsesBaseProperties
  replaceable package Medium = MyMedium;
  Medium.BaseProperties medium;
  Real y;
equation
  medium.p = 1;
  medium.h = 2;
  y = medium.t;
end UsesBaseProperties;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session
        .compile_model("UsesBaseProperties")
        .expect("compile failed");

    let flat_var_names: Vec<_> = result
        .flat
        .variables
        .keys()
        .map(|k| k.to_string())
        .collect();

    for expected in ["medium.p", "medium.h", "medium.d", "medium.t"] {
        assert!(
            flat_var_names.iter().any(|n| n == expected),
            "{expected} should be expanded from Medium.BaseProperties; vars={flat_var_names:?}"
        );
    }

    assert!(
        !flat_var_names.iter().any(|n| n == "medium.leak"),
        "package constants must not be expanded as component fields"
    );

    assert!(
        rumoca_eval_dae::analysis::is_balanced(&result.dae),
        "Model should remain balanced: {}",
        rumoca_eval_dae::analysis::balance_detail(&result.dae)
    );
}

/// MLS §7.3: When a replaceable package has a constraining type, dotted member
/// model types (e.g., `Medium.BaseProperties`) must still instantiate from the
/// actual redeclared package, not the constraining package.
#[test]
fn test_dotted_model_type_uses_redeclared_package_with_constrainedby() {
    let source = r#"
package PartialMedium
  replaceable partial model BaseProperties
    input Real p;
    input Real h;
    Real d;
  equation
    d = p + h;
  end BaseProperties;
end PartialMedium;

package RealMedium
  extends PartialMedium;

  redeclare model extends BaseProperties
    Real R_s;
  equation
    R_s = d - p;
  end BaseProperties;
end RealMedium;

model UsesConstrainedBaseProperties
  replaceable package Medium = RealMedium constrainedby PartialMedium;
  Medium.BaseProperties medium;
  Real y;
equation
  medium.p = 3;
  medium.h = 1;
  y = medium.R_s;
end UsesConstrainedBaseProperties;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session
        .compile_model("UsesConstrainedBaseProperties")
        .expect("compile failed");

    let flat_var_names: Vec<_> = result
        .flat
        .variables
        .keys()
        .map(|k| k.to_string())
        .collect();

    assert!(
        flat_var_names.iter().any(|n| n == "medium.R_s"),
        "medium.R_s should come from redeclared Medium.BaseProperties; vars={flat_var_names:?}"
    );

    assert!(
        rumoca_eval_dae::analysis::is_balanced(&result.dae),
        "Model should remain balanced: {}",
        rumoca_eval_dae::analysis::balance_detail(&result.dae)
    );
}

mod pipeline_cases;
