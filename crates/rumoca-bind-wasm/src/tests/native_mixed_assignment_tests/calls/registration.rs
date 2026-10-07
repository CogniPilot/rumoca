//! Function loops in whole native programs, and the forms not yet admitted.
use super::*;

const SOURCE: &str = r#"
function TranslationFit
  input Real sourcePoint[:, 3];
  input Real targetPoint[size(sourcePoint, 1), 3];
  input Boolean pairEnabled[size(sourcePoint, 1)];
  input Integer activeCount;
  output Real accepted;
  output Real translation[3];
  output Real rms;
  output Real validCount;
protected
  Real residual;
algorithm
  accepted := 0.0;
  translation := zeros(3);
  rms := 0;
  validCount := 0;
  if activeCount >= 1 and activeCount <= size(sourcePoint, 1) then
    for i in 1:size(sourcePoint, 1) loop
      if i <= activeCount and pairEnabled[i] then
        validCount := validCount + 1;
        for j in 1:3 loop
          translation[j] := translation[j] + targetPoint[i, j] - sourcePoint[i, j];
        end for;
      end if;
    end for;
    if validCount > 0 then
      translation := translation / validCount;
      for i in 1:size(sourcePoint, 1) loop
        if i <= activeCount and pairEnabled[i] then
          for j in 1:3 loop
            residual := targetPoint[i, j] - sourcePoint[i, j] - translation[j];
            rms := rms + residual * residual;
          end for;
        end if;
      end for;
      rms := sqrt(rms / validCount);
      accepted := 1.0;
    end if;
  end if;
end TranslationFit;
function DominantEigen
  input Real A[3, 3];
  output Real lambda;
  output Real vector[3];
protected
  Real w[3];
algorithm
  vector := {1.0, 0.5, 0.25};
  for s in 1:80 loop
    w := A * vector;
    vector := w / sqrt(w * w);
  end for;
  lambda := vector * (A * vector);
end DominantEigen;
model Registration
  parameter Integer n = 48;
  input Real sourcePoint[n, 3] = zeros(n, 3);
  input Real targetPoint[n, 3] = zeros(n, 3);
  input Boolean pairEnabled[n] = fill(true, n);
  input Integer activeCount = n;
  output Real accepted;
  output Real translation[3];
  output Real rms;
  output Real validCount;
  input Real A[3, 3] = {{2, 1, 0}, {1, 2, 0}, {0, 0, 1}};
  output Real lambda;
  output Real vector[3];
equation
  (lambda, vector) = DominantEigen(A);
  (accepted, translation, rms, validCount) =
    TranslationFit(sourcePoint, targetPoint, pairEnabled, activeCount);
end Registration;
"#;

/// The fit's source arithmetic in source order: accumulate, average, then the
/// root-mean-square residual over the active enabled pairs.
fn expected_fit(
    source: &[[f64; 3]],
    target: &[[f64; 3]],
    enabled: &[bool],
    active: usize,
) -> (f64, [f64; 3], f64, f64) {
    if active < 1 || active > source.len() {
        return (0.0, [0.0; 3], 0.0, 0.0);
    }
    let selected = |i: usize| i < active && enabled[i];
    let mut translation = [0.0; 3];
    let mut count = 0.0;
    for i in (0..source.len()).filter(|&i| selected(i)) {
        count += 1.0;
        for j in 0..3 {
            translation[j] = translation[j] + target[i][j] - source[i][j];
        }
    }
    if count == 0.0 {
        return (0.0, [0.0; 3], 0.0, 0.0);
    }
    translation = translation.map(|value| value / count);
    let mut rms = 0.0;
    for i in (0..source.len()).filter(|&i| selected(i)) {
        for j in 0..3 {
            let residual = target[i][j] - source[i][j] - translation[j];
            rms += residual * residual;
        }
    }
    (1.0, translation, (rms / count).sqrt(), count)
}

/// A multi-output fit and a power iteration, both with function loops, run as
/// one checked native program: every output equals the source arithmetic for
/// changed inputs, and an inactive configuration completes with the fit's
/// rejected outputs rather than a fault.
#[test]
fn function_loops_execute_in_the_native_program() {
    let _lock = session_test_guard();
    let artifact: serde_json::Value = serde_json::from_str(
        &crate::native_program_api::prepare_native_program(SOURCE, "Registration")
            .expect("function loops lower to a native program"),
    )
    .unwrap();
    let mut execution = CallExecution::new(&artifact);
    let mut parameters = artifact["parameters"]
        .as_array()
        .unwrap()
        .iter()
        .map(|value| value.as_f64().unwrap())
        .collect::<Vec<_>>();
    let source = (0..48)
        .map(|i| {
            [
                0.5 * i as f64,
                (i as f64 * 0.37).sin(),
                1.0 - 0.01 * i as f64,
            ]
        })
        .collect::<Vec<_>>();
    let target = source
        .iter()
        .enumerate()
        .map(|(i, point)| {
            let noise = 1.0e-3 * (i as f64 * 1.7).cos();
            [
                point[0] + 0.5 + noise,
                point[1] - 1.25,
                point[2] + 2.0 - noise,
            ]
        })
        .collect::<Vec<_>>();
    let enabled = (0..48).map(|i| i % 7 != 3).collect::<Vec<_>>();
    for i in 0..48 {
        for j in 0..3 {
            let name = |array: &str| format!("{array}[{},{}]", i + 1, j + 1);
            parameters[slot(&artifact, &name("sourcePoint"), "P")] = source[i][j];
            parameters[slot(&artifact, &name("targetPoint"), "P")] = target[i][j];
        }
        parameters[slot(&artifact, &format!("pairEnabled[{}]", i + 1), "P")] =
            f64::from(u8::from(enabled[i]));
    }
    let output = |values: &[f64], name: &str| values[slot(&artifact, name, "Y")];
    for active in [40, 0] {
        parameters[slot(&artifact, "activeCount", "P")] = active as f64;
        let values = execution.run(&parameters);
        let (accepted, translation, rms, count) = expected_fit(&source, &target, &enabled, active);
        assert_eq!(output(&values, "accepted"), accepted);
        assert_eq!(output(&values, "validCount"), count);
        for (j, expected) in translation.iter().enumerate() {
            let actual = output(&values, &format!("translation[{}]", j + 1));
            assert_eq!(
                actual.to_bits(),
                expected.to_bits(),
                "translation[{}]",
                j + 1
            );
        }
        assert_eq!(output(&values, "rms").to_bits(), rms.to_bits());
        // The dominant eigenpair of A is 3 with (1, 1, 0) / sqrt(2).
        assert!((output(&values, "lambda") - 3.0).abs() < 1.0e-12);
        let half = std::f64::consts::FRAC_1_SQRT_2;
        for (j, expected) in [half, half, 0.0].iter().enumerate() {
            let actual = output(&values, &format!("vector[{}]", j + 1));
            assert!((actual - expected).abs() < 1.0e-12, "vector[{}]", j + 1);
        }
    }
}

/// A power iteration whose sweep count is a runtime Integer input needs a
/// guard-bounded dependent domain, which is not yet representable: the
/// compiler refuses it with a typed construction diagnostic rather than
/// expanding scalar statements.
#[test]
fn runtime_bound_power_iteration_is_refused_until_dependent_domains_land() {
    let _lock = session_test_guard();
    let source = r#"
function RuntimeSweeps
  input Real A[3, 3];
  input Integer sweeps;
  output Real vector[3];
protected
  Real w[3];
algorithm
  vector := {1.0, 0.5, 0.25};
  for s in 1:sweeps loop
    w := A * vector;
    vector := w / sqrt(w * w);
  end for;
end RuntimeSweeps;
model RuntimeEigen
  input Real A[3, 3] = {{2, 1, 0}, {1, 2, 0}, {0, 0, 1}};
  input Integer sweeps = 80;
  output Real vector[3];
equation
  vector = RuntimeSweeps(A, sweeps);
end RuntimeEigen;
"#;
    let refusal = crate::native_program_api::prepare_native_program(source, "RuntimeEigen")
        .expect_err("a runtime loop bound has no compact dependent domain yet");
    let message = refusal.message();
    assert!(message.contains("ToDae"), "{message}");
    assert!(
        message.contains("requires a compact dependent-domain transition"),
        "{message}"
    );
}

/// An early `return` from a function with an array output is successful
/// completion in MLS §12.4, but the DAE definedness proof does not yet admit
/// it; the construction refuses with a typed diagnostic instead of guessing.
#[test]
fn early_return_with_an_array_output_is_refused_until_return_definedness_lands() {
    let _lock = session_test_guard();
    let source = r#"
function EarlyArray
  input Real u;
  output Real y[2];
algorithm
  y := zeros(2);
  if u < 0 then
    return;
  end if;
  y := {u, 2 * u};
end EarlyArray;
model EarlyArrayReturn
  input Real u = 1;
  output Real y[2];
equation
  y = EarlyArray(u);
end EarlyArrayReturn;
"#;
    let refusal = crate::native_program_api::prepare_native_program(source, "EarlyArrayReturn")
        .expect_err("array-output early return has no checked definedness proof yet");
    let message = refusal.message();
    assert!(message.contains("ToDae"), "{message}");
    assert!(
        message.contains("must define every output before returning"),
        "{message}"
    );
}
