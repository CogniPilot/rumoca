//! A reduced state selection integrates the coordinate whose nonzero `start`
//! the author wrote (SPEC_0040 STRUCT-T07, STRUCT-T02 seed information).
//!
//! `x[i] = i*q` with `der(x[i])` inside a for-loop reaches the formal state
//! candidate; the same equations unrolled keep `q` as a plain state. Both must
//! start from `q.start` (MLS 3.7 §8.6) rather than integrating an `x[i]` whose
//! only start is the zero DAE construction supplies.

use super::tangent_jacobian::compile_with_roots;
use crate::{SimOptions, simulate_dae};

fn model(body: &str) -> String {
    format!(
        "model M\n  Real q(start = 1);\n  Real x[3];\n  Real v[3];\nequation\n  \
         der(q) = -q;\n{body}\nend M;"
    )
}

/// `q` and `x[3]` at `t = 0` and `t = 1`.
fn endpoints(source: &str) -> [(f64, f64); 2] {
    let dae = compile_with_roots(source, "M", &[]);
    let options = SimOptions {
        t_end: 1.0,
        ..SimOptions::default()
    };
    let result = simulate_dae(&dae, &options).expect("model simulates");
    ["q", "x[3]"].map(|name| {
        let column = result
            .names
            .iter()
            .position(|candidate| candidate == name)
            .expect("variable is reported");
        let values = &result.data[column];
        (values[0], *values.last().expect("nonempty trajectory"))
    })
}

#[test]
fn reduced_loop_selection_keeps_the_authored_state_start() {
    let looped = endpoints(&model(
        "  for i in 1:3 loop\n    x[i] = i*q;\n    v[i] = der(x[i]);\n  end for;",
    ));
    let unrolled = endpoints(&model(
        "  x[1] = 1*q; x[2] = 2*q; x[3] = 3*q;\n  \
         v[1] = der(x[1]); v[2] = der(x[2]); v[3] = der(x[3]);",
    ));
    let decay = (-1.0_f64).exp();
    for [(q0, q1), (x0, x1)] in [looped, unrolled] {
        assert_eq!((q0, x0), (1.0, 3.0), "starts from q.start");
        assert!((q1 - decay).abs() < 1e-4, "q(1) = {q1}, expected {decay}");
        assert!((x1 - 3.0 * decay).abs() < 3e-4, "x[3](1) = {x1}");
    }
}
