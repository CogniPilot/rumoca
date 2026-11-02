//! SPEC_0034 GAL-028: a packed index proven in range by what every call site
//! passes the size parameter it is computed from.
use rumoca_eval_galec::{Evaluator, IntegerDomain, Value};
use rumoca_phase_galec::{GalecInput, GalecOptions, lower_to_algorithm_code};

/// The packed-transpose shape of `LinearAlgebra.LowerTriangular.transposePacked`.
/// Its write index depends on `n`, a parameter: in range only for the `n` its
/// callers pass.
const FUNCTIONS: &str = r#"
function upperIndex
  input Integer i;
  input Integer j;
  input Integer n;
  output Integer k;
algorithm
  k := div((i - 1) * (2 * n - i + 2), 2) + j - i + 1;
end upperIndex;
function lowerIndex
  input Integer i;
  input Integer j;
  output Integer k;
algorithm
  k := div(i * (i - 1), 2) + j;
end lowerIndex;
function transposePacked
  input Integer n;
  input Real p[div(n * (n + 1), 2)];
  output Real q[div(n * (n + 1), 2)];
algorithm
  q := zeros(div(n * (n + 1), 2));
  for i in 1:n loop
    for j in i:n loop
      q[upperIndex(i, j, n)] := p[lowerIndex(j, i)];
    end for;
  end for;
end transposePacked;
function scatter "a fixed 3 by 3 triangle, placed by the caller's n"
  input Integer n;
  input Real p[6];
  output Real q[6];
algorithm
  q := zeros(6);
  for i in 1:3 loop
    for j in i:3 loop
      q[upperIndex(i, j, n)] := p[lowerIndex(j, i)];
    end for;
  end for;
end scatter;
function forward "passes its own parameter on"
  input Integer m;
  input Real p[6];
  output Real q[6];
algorithm
  q := scatter(m, p);
end forward;
function bySize "passes a size, which is a literal once the shape is fixed"
  input Real A[3, 3];
  input Real p[6];
  output Real q[6];
algorithm
  q := transposePacked(size(A, 1), p);
end bySize;
"#;

fn model(name: &str, body: &str) -> String {
    format!(
        "{FUNCTIONS}model {name}\n  input Real u;\n  input Integer m;\n  \
         discrete output Real q[6](each start = 0.0);\n  \
         discrete output Real r[6](each start = 0.0);\nequation\n  \
         when sample(0.0, 0.01) then\n    {body}\n  end when;\nend {name};\n"
    )
}

fn lower(name: &str, body: &str) -> Result<rumoca_ir_galec::package::AlgorithmCodePackage, String> {
    let compiled = rumoca::Compiler::new()
        .model(name)
        .compile_str(&model(name, body), &format!("{name}.mo"))
        .expect("fixed-shape source compiles");
    lower_to_algorithm_code(
        &GalecInput::new(&compiled.dae, name),
        &GalecOptions::default(),
    )
    .map_err(|errors| format!("{errors:?}"))
}

const P: &str = "{u, 2.0*u, 3.0*u, 4.0*u, 5.0*u, 6.0*u}";

#[test]
fn a_size_parameter_every_call_site_fixes_proves_the_packed_index() {
    for (name, call) in [
        ("Direct", format!("transposePacked(3, {P})")),
        ("Scattered", format!("scatter(3, {P})")),
        ("Forwarded", format!("forward(3, {P})")),
        ("BySize", format!("bySize(identity(3), {P})")),
    ] {
        let package = lower(name, &format!("q = {call};\n    r = {call};"))
            .unwrap_or_else(|errors| panic!("{name}: every site passes n = 3: {errors}"));
        let mut evaluator = Evaluator::new(package.checked_block(), IntegerDomain::signed_32())
            .expect("checked GALEC evaluator");
        evaluator.startup().unwrap();
        evaluator.recalibrate().unwrap();
        for u in [-2.0, 0.5, 3.0] {
            evaluator.set_state("u", Value::Real(u)).unwrap();
            evaluator.do_step().unwrap();
            // Row-major packed lower {1; 2 3; 4 5 6} read as packed upper.
            let expected = [1.0, 2.0, 4.0, 3.0, 5.0, 6.0].map(|k| k * u);
            assert_array(&evaluator, "q", expected, name);
            assert!(evaluator.active_signals().is_empty(), "{name}");
        }
    }
}

#[test]
fn a_size_parameter_some_call_site_leaves_unbounded_is_refused() {
    // n = 4 indexes upperIndex(3, 3, 4) = 8 > 6; a runtime n has no range; one
    // in-range site does not excuse another sharing the body.
    for (name, body) in [
        ("TooLarge", format!("q = scatter(4, {P});\n    r = q;")),
        ("Runtime", format!("q = scatter(m, {P});\n    r = q;")),
        (
            "Mixed",
            format!("q = scatter(3, {P});\n    r = scatter(4, {P});"),
        ),
        (
            "ForwardedRuntime",
            format!("q = forward(m, {P});\n    r = q;"),
        ),
    ] {
        let errors = lower(name, &body)
            .err()
            .unwrap_or_else(|| panic!("{name} is refused"));
        assert!(errors.contains("dynamic-array-index"), "{name}: {errors}");
    }
}

fn assert_array(evaluator: &Evaluator<'_>, name: &str, expected: [f64; 6], model: &str) {
    let Value::Array(array) = evaluator.state(name).unwrap() else {
        panic!("{model}: {name} is the checked array output");
    };
    assert_eq!(array.len(), expected.len(), "{model}");
    for (got, expected) in array.iter().zip(expected) {
        assert_eq!(got, &Value::Real(expected), "{model}: {name}");
    }
}
