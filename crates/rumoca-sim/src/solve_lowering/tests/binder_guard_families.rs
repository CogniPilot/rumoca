//! A guard over an equation-domain binder (`i == k` inside `for i`) is
//! invariant while its family is differentiated, so index reduction can take
//! `der(x[i])` through the selected branch of each scalar instance.
//!
//! The differentiated branch reads model coordinates under the family's binder
//! domain; they are rebuilt as model expressions, not as function bodies.

use super::tangent_jacobian::compile_with_roots;
use crate::{SimOptions, simulate_dae};

fn model(body: &str) -> String {
    format!(
        "model M\n  parameter Integer k = 2;\n  Real q(start = 1, fixed = true);\n  \
         Real x[3];\n  Real v[3];\nequation\n  der(q) = -q;\n{body}\nend M;"
    )
}

/// The values of `x[1..3]` and `v[2]` at `t = 1`.
fn final_values(source: &str) -> [f64; 4] {
    let dae = compile_with_roots(source, "M", &[]);
    let options = SimOptions {
        t_end: 1.0,
        ..SimOptions::default()
    };
    let result = simulate_dae(&dae, &options).expect("model simulates");
    ["x[1]", "x[2]", "x[3]", "v[2]"].map(|name| {
        let column = result
            .names
            .iter()
            .position(|candidate| candidate == name)
            .expect("variable is reported");
        *result.data[column].last().expect("nonempty trajectory")
    })
}

#[test]
fn binder_guarded_family_differentiates_like_its_unrolled_rows() {
    let looped = final_values(&model(
        "  for i in 1:3 loop\n    x[i] = if i == k then q else 2*q;\n    \
         v[i] = der(x[i]);\n  end for;",
    ));
    let unrolled = final_values(&model(
        "  x[1] = if 1 == k then q else 2*q;\n  x[2] = if 2 == k then q else 2*q;\n  \
         x[3] = if 3 == k then q else 2*q;\n  \
         v[1] = der(x[1]); v[2] = der(x[2]); v[3] = der(x[3]);",
    ));
    let decay = (-1.0_f64).exp();
    let expected = [2.0 * decay, decay, 2.0 * decay, -decay];
    for values in [looped, unrolled] {
        for (value, expected) in values.into_iter().zip(expected) {
            assert!(
                (value - expected).abs() < 1e-4,
                "{values:?} != {expected:?}"
            );
        }
    }
}
