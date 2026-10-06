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

/// A multi-output fit and a power iteration compile to checked Solve programs,
/// but their loops lower to scalar function folds, which the native
/// direct-assignment schedule does not admit yet; the binding refuses with a
/// typed schedule diagnostic instead of a partial program.
#[test]
fn function_folds_are_refused_by_the_native_schedule_until_admitted() {
    let _lock = session_test_guard();
    let refusal = crate::native_program_api::prepare_native_program_impl(SOURCE, "Registration")
        .expect_err("scalar function folds have no native direct-assignment stage yet");
    let message = refusal.message();
    assert!(
        message.contains("compiler did not issue a complete native direct-assignment schedule"),
        "{message}"
    );
    assert!(
        message.contains("unsupported or effectful operations"),
        "{message}"
    );
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
    let refusal = crate::native_program_api::prepare_native_program_impl(source, "RuntimeEigen")
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
    let refusal =
        crate::native_program_api::prepare_native_program_impl(source, "EarlyArrayReturn")
            .expect_err("array-output early return has no checked definedness proof yet");
    let message = refusal.message();
    assert!(message.contains("ToDae"), "{message}");
    assert!(
        message.contains("must define every output before returning"),
        "{message}"
    );
}
