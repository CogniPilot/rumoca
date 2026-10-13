//! One authored call occurrence is one call site, however deeply the loop
//! body nests it in conditionals (SPEC_0040 SOLVE-C73), and the native module
//! computes exactly what the source's own operation order computes.

use super::*;

const SOURCE: &str = r#"
function Heavy
  input Real x[:];
  output Real y[size(x,1)];
algorithm
  for i in 1:size(x,1) loop
    y[i] := 2*x[i] + 1;
  end for;
end Heavy;

function Dot
  input Real a[:];
  input Real b[size(a,1)];
  output Real v;
algorithm
  v := 0;
  for i in 1:size(a,1) loop
    v := v + a[i]*b[i];
  end for;
end Dot;

function Cg
  input Real r0[:];
  input Integer maxIt;
  output Real xs[size(r0,1)];
  output Real iterations;
protected
  Real d[size(r0,1)];
  Real p[size(r0,1)];
  Real r[size(r0,1)];
  Real den; Real alpha; Real rho; Real rn;
  Boolean running; Boolean valid;
algorithm
  xs := zeros(size(r0,1));
  r := r0; d := r0; p := zeros(size(r0,1));
  rho := Dot(r, r);
  running := rho > 1e-12;
  iterations := 0.0; valid := true; den := 0.0; alpha := 0.0; rn := 0.0;
  for k in 1:50 loop
    if running and k <= maxIt then
      iterations := iterations + 1.0;
      p := Heavy(d);
      den := Dot(d, p);
      valid := den > 0.0;
      if valid then
        alpha := rho/den;
        xs := xs + alpha*d;
        r := r - alpha*p;
        rn := Dot(r, r);
        running := rn > 1e-12;
        if running then
          d := r + (rn/rho)*d;
          rho := rn;
        end if;
      else
        running := false;
      end if;
    end if;
  end for;
end Cg;

model CgModel
  input Real r0[4] = {1.0, 2.0, 3.0, 4.0};
  input Integer maxIt = 5;
  output Real x[4];
  output Real it;
equation
  (x, it) = Cg(r0, maxIt);
end CgModel;
"#;

fn heavy(x: &[f64]) -> Vec<f64> {
    x.iter().map(|x| 2.0 * x + 1.0).collect()
}

fn dot(a: &[f64], b: &[f64]) -> f64 {
    a.iter().zip(b).fold(0.0, |v, (a, b)| v + a * b)
}

/// The source's operation order, one authored call per occurrence.
fn cg(r0: &[f64], max_iterations: i64) -> (Vec<f64>, f64) {
    let mut xs = vec![0.0; r0.len()];
    let (mut r, mut d) = (r0.to_vec(), r0.to_vec());
    let mut rho = dot(&r, &r);
    let mut running = rho > 1e-12;
    let mut iterations = 0.0;
    for k in 1..=50 {
        if !(running && k <= max_iterations) {
            continue;
        }
        iterations += 1.0;
        let p = heavy(&d);
        let den = dot(&d, &p);
        if den <= 0.0 {
            running = false;
            continue;
        }
        let alpha = rho / den;
        xs = xs.iter().zip(&d).map(|(x, d)| x + alpha * d).collect();
        r = r.iter().zip(&p).map(|(r, p)| r - alpha * p).collect();
        let rn = dot(&r, &r);
        running = rn > 1e-12;
        if running {
            let beta = rn / rho;
            d = r.iter().zip(&d).map(|(r, d)| r + beta * d).collect();
            rho = rn;
        }
    }
    (xs, iterations)
}

#[test]
fn nested_loop_conditionals_share_one_call_site_per_authored_occurrence() {
    let _lock = session_test_guard();
    let artifact: serde_json::Value = serde_json::from_str(
        &crate::native_program_api::prepare_native_program(SOURCE, "CgModel").unwrap(),
    )
    .unwrap();
    // Cg calls Heavy once and Dot three times (`rho`, `den`, `rn`): four
    // authored occurrences, so four callees with exactly one site each.
    let sites = artifact["call_sites"].as_array().unwrap();
    let largest_caller = sites
        .iter()
        .map(|site| site["caller"].as_u64().unwrap())
        .max_by_key(|caller| {
            sites
                .iter()
                .filter(|site| site["caller"].as_u64().unwrap() == *caller)
                .count()
        })
        .unwrap();
    let callees = sites
        .iter()
        .filter(|site| site["caller"].as_u64().unwrap() == largest_caller)
        .collect::<Vec<_>>();
    assert_eq!(callees.len(), 4, "{callees:?}");
    assert!(
        callees.iter().all(|site| site["sites"] == 1),
        "every authored call is one site: {callees:?}"
    );

    let mut execution = CallExecution::new(&artifact);
    let mut parameters = artifact["parameters"]
        .as_array()
        .unwrap()
        .iter()
        .map(|value| value.as_f64().unwrap())
        .collect::<Vec<_>>();
    let r0 = [1.0, 2.0, 3.0, 4.0];
    for (first, max_iterations) in [(1.0, 5), (0.5, 2), (1.0, 0), (3.0, 50)] {
        let input = r0.map(|value| value * first);
        for (index, value) in input.iter().enumerate() {
            parameters[slot(&artifact, &format!("r0[{}]", index + 1), "P")] = *value;
        }
        execution.set_input("maxIt", max_iterations);
        let y = execution.run(&parameters);
        let (expected, iterations) = cg(&input, max_iterations);
        for (index, value) in expected.iter().enumerate() {
            let cell = slot(&artifact, &format!("x[{}]", index + 1), "Y");
            assert_eq!(y[cell].to_bits(), value.to_bits(), "x[{index}]");
        }
        assert_eq!(
            y[slot(&artifact, "it", "Y")].to_bits(),
            iterations.to_bits()
        );
    }
}
