use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
package Kinematics
  function position
    input Real q;
    input Real v "Valid calls satisfy v = 2*q*der(q)";
    output Real y;
  algorithm
    y := q*q;
    annotation(derivative(noDerivative=q)=position_der);
  end position;
  function position_der
    input Real q;
    input Real v;
    input Real der_v;
    output Real der_y;
  algorithm
    der_y := v;
  end position_der;
end Kinematics;
model AnnotatedConstraint
  Real q(start=1,fixed=true);
  Real v(start=1,fixed=true);
  Real y;
  Real velocity;
  Real acceleration;
equation
  2*q*der(q) = v;
  der(v) = -1;
  y = Kinematics.position(q,v);
  velocity = der(y);
  acceleration = der(velocity);
end AnnotatedConstraint;
"#;

#[test]
fn an_annotated_derivative_closes_implicit_kinematic_constraints() {
    simulate_constraint(SOURCE);
}

fn simulate_constraint(source: &str) {
    let compiled = Compiler::new()
        .model("AnnotatedConstraint")
        .compile_str(source, "annotated_constraint.mo")
        .unwrap();
    for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        let result = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                t_end: 0.1,
                dt: Some(0.01),
                solver_mode,
                ..Default::default()
            },
        )
        .unwrap_or_else(|error| panic!("{solver_mode:?}: {error}"));
        for name in ["q", "v", "y", "velocity", "acceleration"] {
            let column = result.names.iter().position(|value| value == name).unwrap();
            for (&time, &actual) in result.times.iter().zip(&result.data[column]) {
                let position = 1.0 + time - 0.5 * time * time;
                let expected = match name {
                    "q" => position.sqrt(),
                    "y" => position,
                    "v" | "velocity" => 1.0 - time,
                    "acceleration" => -1.0,
                    _ => unreachable!(),
                };
                assert!(
                    (actual - expected).abs() < 1e-6,
                    "{solver_mode:?} {name}({time}): {actual} != {expected}"
                );
            }
        }
    }
}

#[test]
fn a_zero_derivative_restriction_does_not_apply_to_a_varying_argument() {
    let source = SOURCE.replace(
        "annotation(derivative(noDerivative=q)=position_der);",
        "annotation(derivative(zeroDerivative=v)=position_der_constant_v, derivative(noDerivative=q)=position_der);",
    ).replace("end Kinematics;", r#"
  function position_der_constant_v
    input Real q;
    input Real v;
    input Real der_q;
    output Real der_y;
  algorithm
    der_y := 2*q*der_q;
  end position_der_constant_v;
end Kinematics;"#);
    simulate_constraint(&source);
}

#[test]
fn a_singular_kinematic_derivative_coefficient_cannot_produce_a_successful_trace() {
    let source = SOURCE.replace("Real q(start=1,fixed=true)", "Real q(start=0,fixed=true)");
    let compiled = Compiler::new()
        .model("AnnotatedConstraint")
        .compile_str(&source, "singular_kinematics.mo")
        .unwrap();
    for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        let error = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                t_end: 0.1,
                dt: Some(0.01),
                solver_mode,
                ..Default::default()
            },
        )
        .expect_err("2*q*der(q)=v has no finite derivative at q=0, v=1");
        let message = error.to_string();
        assert!(
            message.contains("non-finite") || message.contains("not finite"),
            "{solver_mode:?}: {error}"
        );
    }
}

#[test]
fn derivative_specialization_preserves_array_shapes_values_and_higher_order_context() {
    let source = r#"
package K
  function f
    input Real u[:];
    input Integer n;
    output Real y[size(u,1)];
  algorithm
    y := u;
    annotation(derivative=df);
  end f;
  function df
    input Real u[:];
    input Integer n;
    input Real u_d[size(u,1)];
    output Real y_d[size(u,1)];
  protected
    Real scratch[n] = zeros(n);
  algorithm
    y_d := u_d;
    annotation(derivative(order=2)=ddf);
  end df;
  function ddf
    input Real u[:];
    input Integer n;
    input Real u_d[size(u,1)];
    input Real u_dd[size(u,1)];
    output Real y_dd[size(u,1)];
  algorithm
    y_dd := u_dd;
  end ddf;
end K;
model ArrayDerivatives
  Real u2[2]; Real u3[3];
  Real y2[2]; Real y3[3];
equation
  der(u2) = -u2;
  der(u3) = -u3;
  y2 = K.f(u2,2);
  y3 = K.f(u3,5);
end ArrayDerivatives;
"#;
    let compiled = Compiler::new()
        .model("ArrayDerivatives")
        .compile_str(source, "array_derivatives.mo")
        .unwrap();
    compiled.dae.inspect(|view| {
        let links = (0..view.function_count())
            .flat_map(|ordinal| {
                view.function(view.function_id(ordinal).unwrap())
                    .unwrap()
                    .derivatives()
            })
            .collect::<Vec<_>>();
        assert_eq!(links.len(), 4);
        let mut extents = Vec::new();
        for first in links.iter().filter(|link| link.order() == 1) {
            assert!(first.previous().is_none());
            assert_eq!(first.tangent_inputs().collect::<Vec<_>>(), [0]);
            let original = view.function(first.source()).unwrap();
            let derivative = view.function(first.target()).unwrap();
            let extent = view
                .value_type(original.parameter_types().get(0).unwrap())
                .unwrap()
                .dimensions()[0];
            let tangent = view
                .value_type(derivative.parameter_types().get(2).unwrap())
                .unwrap();
            assert_eq!(tangent.dimensions(), [extent]);
            let scratch = derivative
                .values()
                .find(|value| value.name().as_str() == "scratch")
                .unwrap();
            let scratch_type = view.value_type(scratch.value_type()).unwrap();
            assert_eq!(scratch_type.dimensions(), [if extent == 2 { 2 } else { 5 }]);
            let next = links
                .iter()
                .find(|link| link.previous() == Some(first.id()))
                .unwrap();
            assert_eq!(next.order(), 2);
            assert_eq!(next.source(), first.target());
            assert_eq!(next.tangent_inputs().collect::<Vec<_>>(), [2]);
            assert_eq!(
                view.function(next.target())
                    .unwrap()
                    .parameter_types()
                    .len(),
                4
            );
            extents.push(extent);
        }
        extents.sort_unstable();
        assert_eq!(extents, [2, 3]);
    });
}
