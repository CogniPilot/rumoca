//! A refused construct must be refused in both artifacts, identically.
//!
//! The Jacobian synthesis specification states that a construct is either
//! differentiated correctly or refused, with no third state, and that the
//! portable artifact and the compiled program are one artifact. Together those
//! two claims mean a refusal has to reach the compiler user *and* the writer of
//! `compile --emit-standard-modelica`, citing the same rule. A form that only
//! one of the two paths refuses would put a derivative nothing checks into a
//! file another tool runs, which is the exact failure mode the construct
//! exists to remove.
//!
//! Each case below names a refused form, the rule id that states it, and a
//! word the diagnostic has to contain.

use rumoca::Compiler;

/// One refused source, and what both artifacts must say about it.
struct Refused {
    /// Probe model name, which is also the compiled model.
    name: &'static str,
    /// Complete probe source.
    source: &'static str,
    /// Rule id both paths must cite.
    rule: &'static str,
    /// A word the diagnostic must contain, so the id is not the only anchor.
    says: &'static str,
    /// The `line:column` of the refused construct, which both paths must name.
    ///
    /// A rule id alone does not tell the author where to look, and a refusal
    /// reported at a generated line points at text the author never wrote.
    at: &'static str,
}

/// `A^2` for a matrix `A`: `d(A^2)` is `dA*A + A*dA`, not `2*A*dA`, and the
/// scalar power rule is stated only for a rank-0 base.
const MATRIX_POWER: Refused = Refused {
    name: "MatrixPower",
    source: r"
function squared
  input Real x[2];
  output Real y[2];
protected
  Real a[2, 2];
  Real b[2, 2];
algorithm
  a := {{x[1], 0.5}, {0.25, x[2]}};
  b := a^2;
  y := b*{1.0, 1.0};
end squared;

model MatrixPower
  parameter Real x[2] = {1.5, 0.8};
  Real J[2, 2] = jacobian(squared(x), x);
  Real clock(start = 0, fixed = true);
equation
  der(clock) = 0;
end MatrixPower;
",
    rule: "JAC-R5",
    says: "scalar",
    at: "16:18",
};

/// `for e in <Real array> loop`: the index carries the element's tangent, and
/// an engine that treats it as a structural zero mints an all-zero Jacobian.
const ARRAY_VALUED_FOR_INDEX: Refused = Refused {
    name: "ArrayForIndex",
    source: r"
function fsq
  input Real x[3];
  output Real y;
algorithm
  y := 0.0;
  for e in x loop
    y := y + e*e;
  end for;
end fsq;

model ArrayForIndex
  parameter Real x[3] = {1.5, 0.8, -0.4};
  Real J[1, 3] = jacobian(fsq(x), x);
  Real clock(start = 0, fixed = true);
equation
  der(clock) = 0;
end ArrayForIndex;
",
    rule: "JAC-R4",
    says: "for",
    at: "7:7",
};

/// `abs` on an array: the taken-branch convention needs a scalar condition,
/// and a vectorized call has one condition per element.
const VECTORIZED_ABS: Refused = Refused {
    name: "VectorAbs",
    source: r"
function magnitudes
  input Real x[3];
  output Real y[3];
algorithm
  y := abs(x);
end magnitudes;

model VectorAbs
  parameter Real x[3] = {1.5, 0.8, -0.4};
  Real J[3, 3] = jacobian(magnitudes(x), x);
  Real clock(start = 0, fixed = true);
equation
  der(clock) = 0;
end VectorAbs;
",
    rule: "JAC-R5",
    says: "abs",
    at: "11:18",
};

/// A declared function whose name is also a rule-carrying builtin: the rule
/// and the body are two different functions, and which one the primal takes is
/// not this engine's decision to make.
const SHADOWED_BUILTIN: Refused = Refused {
    name: "ShadowedBuiltin",
    source: r"
function tanh
  input Real a;
  output Real b;
algorithm
  b := a*a*a;
end tanh;

function shaped
  input Real x;
  output Real y;
algorithm
  y := tanh(x) + x;
end shaped;

model ShadowedBuiltin
  parameter Real x = 1.5;
  Real J[1, 1] = jacobian(shaped(x), x);
  Real clock(start = 0, fixed = true);
equation
  der(clock) = 0;
end ShadowedBuiltin;
",
    rule: "JAC-R2",
    says: "tanh",
    at: "13:8",
};

/// A vectorized call to a user function (MLS 12.4.6): `sq` takes a scalar and
/// the call passes `Real[3]`, so the primal is one call per element and its
/// Jacobian is `Real[3, 3]`. A wrapper reads its shape from declarations, so
/// it would state `Real[1, 1]`, the shape of neither the call nor its
/// Jacobian. Both tools reject the minted artifact downstream, at a generated
/// line the author never wrote; the refusal belongs at the call.
const VECTORIZED_CALL: Refused = Refused {
    name: "VectorizedCall",
    source: r"
function sq
  input Real a;
  output Real b;
algorithm
  b := a*a*a;
end sq;

model VectorizedCall
  parameter Real x[3] = {0.44, -0.55, 0.66};
  Real J[3, 3] = jacobian(sq(x), x);
  Real clock(start = 0, fixed = true);
equation
  der(clock) = 0;
end VectorizedCall;
",
    rule: "JAC-R10",
    says: "vectorizes",
    at: "11:18",
};

/// The same vectorized call under the declaration its collapsed wrapper
/// happens to fit. Nothing downstream catches this one: this compiler runs the
/// artifact and reports a `Real[3, 1, 1]` that is not the `Real[m, n]` JAC-S3
/// promises, and OpenModelica cannot build the same exported file at all. A
/// verdict that depends on the declaration around the call is not a verdict
/// about the call, so the refusal has to fire here too.
const VECTORIZED_CALL_THAT_FITS: Refused = Refused {
    name: "VectorizedCallThatFits",
    source: r"
function sq
  input Real a;
  output Real b;
algorithm
  b := a*a*a;
end sq;

model VectorizedCallThatFits
  parameter Real x[3] = {0.44, -0.55, 0.66};
  Real J[3, 1, 1] = jacobian(sq(x), x);
  Real clock(start = 0, fixed = true);
equation
  der(clock) = 0;
end VectorizedCallThatFits;
",
    rule: "JAC-R10",
    says: "vectorizes",
    at: "11:21",
};

const REFUSED: &[&Refused] = &[
    &MATRIX_POWER,
    &ARRAY_VALUED_FOR_INDEX,
    &VECTORIZED_ABS,
    &SHADOWED_BUILTIN,
    &VECTORIZED_CALL,
    &VECTORIZED_CALL_THAT_FITS,
];

#[test]
fn a_refused_form_is_refused_by_the_compiler() {
    for case in REFUSED {
        let error = Compiler::new()
            .model(case.name)
            .compile_str(case.source, &format!("{}.mo", case.name))
            .err()
            .unwrap_or_else(|| {
                panic!(
                    "{} must not compile: the construct is stated refused",
                    case.name
                )
            });
        let rendered = format!("{error:?}");
        assert!(
            rendered.contains(case.rule),
            "{} must cite {}: {rendered}",
            case.name,
            case.rule
        );
        assert!(
            rendered.contains(case.says),
            "{} must say what it refused ({}): {rendered}",
            case.name,
            case.says
        );
    }
}

#[test]
fn a_refused_form_is_refused_by_the_portable_writer() {
    for case in REFUSED {
        let error = rumoca_compile::parsing::expand_source_to_standard_modelica(
            case.source,
            &format!("{}.mo", case.name),
        )
        .err()
        .unwrap_or_else(|| {
            panic!(
                "{} must not expand: a refusal the portable writer skipped would put an \
                 unchecked derivative in a file another tool runs",
                case.name
            )
        });
        let rendered = format!("{error:#}");
        assert!(
            rendered.contains(case.rule),
            "{} must cite {} in the portable writer too: {rendered}",
            case.name,
            case.rule
        );
        assert!(
            rendered.contains(case.says),
            "{} must say what it refused ({}) in the portable writer too: {rendered}",
            case.name,
            case.says
        );
    }
}

/// The refusal both paths report has to be the same refusal, not two rules
/// that happen to fire on the same source, and it has to point at the
/// construct the author wrote.
///
/// The site is checked to the column, in both artifacts, against a value
/// stated here rather than read out of the diagnostic. A refusal reported at a
/// generated line, or with no column, sends the author to text they never
/// wrote, which is the failure mode a rule id at the call site exists to
/// remove.
#[test]
fn both_artifacts_refuse_with_the_same_rule_and_site() {
    for case in REFUSED {
        let file = format!("{}.mo", case.name);
        let expected = format!("{file}:{}", case.at);
        let compiler = format!(
            "{:?}",
            Compiler::new()
                .model(case.name)
                .compile_str(case.source, &file)
                .expect_err("the compiler refuses")
        );
        let portable = format!(
            "{:#}",
            rumoca_compile::parsing::expand_source_to_standard_modelica(case.source, &file)
                .expect_err("the portable writer refuses")
        );
        for (path, rendered) in [("compiler", &compiler), ("portable writer", &portable)] {
            assert!(
                rendered.contains(&expected),
                "{} must refuse at {expected} in the {path}, at the construct the author \
                 wrote: {rendered}",
                case.name
            );
        }
    }
}
