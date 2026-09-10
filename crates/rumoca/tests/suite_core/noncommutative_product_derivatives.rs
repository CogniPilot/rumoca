use rumoca_compile::compile::{Session, SessionConfig};
use rumoca_sim::{SimOptions, simulate_dae};

#[test]
fn matrix_vector_product_preserves_factor_order_in_derivatives() {
    check(
        "parameter Real P[2,2] = [0,1;0,0]; Real left[2,2] = identity(2)+time*P; Real right[2] = {time,time*time};",
        "[2]",
        &[("[1]", [0.0, 1.0, 0.0, 1.0]), ("[2]", [0.0, 0.0, 1.0, 0.0])],
    );
}

#[test]
fn vector_matrix_product_preserves_factor_order_in_derivatives() {
    check(
        "parameter Real P[2,2] = [0,1;0,0]; Real left[2] = {time,time*time}; Real right[2,2] = identity(2)+time*P;",
        "[2]",
        &[("[1]", [0.0, 1.0, 0.0, 0.0]), ("[2]", [0.0, 0.0, 2.0, 0.0])],
    );
}

#[test]
fn matrix_matrix_product_preserves_factor_order_in_derivatives() {
    check(
        "parameter Real P[2,2] = [0,1;0,0]; parameter Real Q[2,2] = [1,0;0,0]; parameter Real S[2,2] = [0,0;1,0]; parameter Real C[2,2] = [0,0;0,1]; Real left[2,2] = identity(2)+time*P; Real right[2,2] = time*Q+time*time*S+C;",
        "[2,2]",
        &[
            ("[1,1]", [0.0, 1.0, 0.0, 1.0]),
            ("[1,2]", [0.0, 1.0, 0.0, 0.0]),
            ("[2,1]", [0.0, 0.0, 1.0, 0.0]),
            ("[2,2]", [1.0, 0.0, 0.0, 0.0]),
        ],
    );
}

fn check(declarations: &str, shape: &str, polynomials: &[(&str, [f64; 4])]) {
    let source = format!(
        "model ProductDerivative\n{declarations}\nReal y{shape}; Real dy{shape}; Real ddy{shape};\nequation\ny=left*right; dy=der(y); ddy=der(dy);\nend ProductDerivative;"
    );
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("product_derivative.mo", &source)
        .unwrap();
    let compiled = session.compile_model("ProductDerivative").unwrap();
    let result = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 0.5,
            dt: Some(0.05),
            ..Default::default()
        },
    )
    .expect("time-defined tensor states admit exact structural differentiation");
    for (suffix, [c0, c1, c2, c3]) in polynomials {
        for (prefix, coefficients) in [
            ("y", [*c0, *c1, *c2, *c3]),
            ("dy", [*c1, 2.0 * c2, 3.0 * c3, 0.0]),
            ("ddy", [2.0 * c2, 6.0 * c3, 0.0, 0.0]),
        ] {
            let name = format!("{prefix}{suffix}");
            let column = result
                .names
                .iter()
                .position(|candidate| candidate == &name)
                .unwrap();
            for (time, actual) in result.times.iter().zip(&result.data[column]) {
                let expected = coefficients
                    .iter()
                    .rev()
                    .fold(0.0, |value, coefficient| value * time + coefficient);
                assert!(
                    (actual - expected).abs() < 1.0e-10,
                    "{name} at {time}: expected {expected}, got {actual}"
                );
            }
        }
    }
}
