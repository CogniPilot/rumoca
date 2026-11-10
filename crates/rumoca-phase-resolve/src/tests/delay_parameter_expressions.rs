//! MLS §3.7.2 and §8.3.2: delay parameters indexed by scoped loop constants.

use super::*;

#[test]
fn delay_accepts_parameter_arrays_indexed_by_fixed_loops() {
    for body in [
        "equation for i in 1:2 loop y[i] = delay(time, tau[i]); end for;",
        "algorithm for i in 1:2 loop y[i] := delay(time, tau[i]); end for;",
        "equation y = {delay(time, tau[i]) for i in 1:2};",
        "equation for i in 1:2 loop y[i] = delay(time, time, tau[i]); end for;",
        "equation for i in 1:2 loop for j in 1:i loop \
         y[i] = delay(time, tau[j]); end for; end for;",
        "equation for i in 1:2, j in 1:i loop \
         y[i] = delay(time, tau[j]); end for;",
    ] {
        let source =
            format!("model M parameter Real tau[2] = {{0.1, 0.2}}; Real y[2]; {body} end M;");
        resolve_test_source(&source).unwrap_or_else(|diagnostics| {
            panic!("expected valid delay in {body}: {diagnostics:?}")
        });
    }
}

#[test]
fn delay_rejects_nonparameter_indices_and_arrays() {
    for body in [
        "equation y = delay(time, tau[k]);",
        "equation y = delay(time, time, tau[k]);",
        "equation for i in 1:2 loop y = delay(time, u[i]); end for;",
        "algorithm for i in 1:k loop y := delay(time, tau[i]); end for;",
        "equation for i in 1:2 loop for i in 1:2 loop \
         y = delay(time, tau[i]); end for; end for; y = delay(time, tau[i]);",
        "algorithm for i in 1:2 loop for i in 1:k loop \
         y := delay(time, tau[i]); end for; end for;",
    ] {
        let source = format!(
            "model M parameter Real tau[2] = {{0.1, 0.2}}; \
             Integer k; Integer i; Real u[2]; Real y; {body} end M;"
        );
        let diagnostics = resolve_test_source(&source).expect_err("delay must remain strict");
        assert_eq!(
            diagnostics
                .iter()
                .filter(|diagnostic| diagnostic.code.as_deref() == Some("ER055"))
                .count(),
            1,
            "expected exactly one invalid delay in {body}: {diagnostics:?}"
        );
    }
}

#[test]
fn delay_restores_outer_loop_index_after_shadowing() {
    let source = r#"
model M
  parameter Real tau[2] = {0.1, 0.2};
  Real y[2];
algorithm
  for i in 1:2 loop
    for i in 1:2 loop
      y[i] := delay(time, tau[i]);
    end for;
    y[i] := delay(time, tau[i]);
  end for;
end M;
"#;
    resolve_test_source(source).expect("exiting an inner loop must restore the outer binding");
}
