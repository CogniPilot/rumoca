//! A `when` on a relation of a discrete variable that a `when sample(..)`
//! updates fires on the relation's edge in the same event iteration that
//! updates the variable (MLS 8.5): the relation's edge buffer is read by the
//! `when` before the iteration refreshes it.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
model CountedByClock
  Integer n(start = 0, fixed = true);
  Integer k(start = 0, fixed = true);
equation
  when sample(0.05, 0.1) then
    n = pre(n) + 1;
  end when;
  when n > 2 then
    k = pre(k) + 1;
  end when;
end CountedByClock;
"#;

#[test]
fn a_when_on_a_sampled_counter_fires_once_when_the_counter_passes_its_bound() {
    let compiled = Compiler::new()
        .model("CountedByClock")
        .compile_str(SOURCE, "CountedByClock.mo")
        .expect("compile the model");
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            dt: Some(0.1),
            ..SimOptions::default()
        },
    )
    .expect("simulate the model");
    let column = |name: &str| {
        let index = result
            .names
            .iter()
            .position(|candidate| candidate == name)
            .expect("the variable is recorded");
        &result.data[index]
    };
    let (n, k) = (column("n"), column("k"));
    for (row, time) in result.times.iter().enumerate() {
        let expected_k = if n[row] >= 3.0 { 1.0 } else { 0.0 };
        assert_eq!(k[row], expected_k, "k at t = {time} with n = {}", n[row]);
    }
    assert_eq!(*n.last().expect("a last row"), 10.0);
}
