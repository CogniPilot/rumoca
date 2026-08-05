//! MLS §11.5 branch selection and MLS §10.4.1 array constructors with iterators
//! inside a value-proven function specialization.
//!
//! `Modelica.Electrical.Polyphase.Functions.symmetricOrientation` is the shape
//! this covers: its body is a nested conditional over the phase count `m`, one
//! arm of which is an array constructor with an iterator whose range is written
//! over `m`. Before the fix `ToDae` rejected the whole function with ED019
//! `function conditional` — "requires assignments or nested conditionals in
//! every checked branch" — because the checked-branch rule reads every arm as a runtime
//! branch, and it had no shape rule for the constructor at all.
//!
//! A specialization that proves `m` proves which arm MLS §11.5 executes, so the
//! selected statements are an ordinary unconditional algorithm section and the
//! constructor's extent is an ordinary translation-time constant.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

/// The MSL `symmetricOrientation` shape, reduced to what the rules need.
///
/// The even-`m` arm is deliberately left with a recursive call and a `fill`
/// extent: for `m = 3` MLS §11.5 never executes it, and this asserts the
/// analysis agrees rather than demanding owners for statements the program
/// never runs.
const ORIENTATION_MODEL: &str = r#"
within;
function orient
  input Integer m;
  output Real orientation[m];
protected
  constant Real pi = 3.141592653589793;
algorithm
  if mod(m, 2) == 0 then
    if m == 2 then
      orientation[1] := 0;
      orientation[2] := pi/2;
    else
      orientation[1:integer(m/2)] := orient(integer(m/2));
      orientation[integer(m/2) + 1:m] := orient(integer(m/2)) - fill(pi/m, integer(m/2));
    end if;
  else
    orientation := {(k - 1)*2*pi/m for k in 1:m};
  end if;
end orient;
model ProvenBranchOrientation
  Real three[3];
  Real five[5];
  Real two[2];
equation
  three = orient(3);
  five = orient(5);
  two = orient(2);
end ProvenBranchOrientation;
"#;

fn algebraic(report: &rumoca_sim::EvalAtReport, name: &str) -> f64 {
    report
        .solver_y
        .iter()
        .find(|slot| slot.name.replace(' ', "") == name)
        .unwrap_or_else(|| {
            panic!(
                "missing solver value {name}; have: {:?}",
                report
                    .solver_y
                    .iter()
                    .map(|slot| slot.name.clone())
                    .collect::<Vec<_>>()
            )
        })
        .value
}

fn evaluate(source: &str, model: &str, file: &str) -> rumoca_sim::EvalAtReport {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(source, file)
        .expect("model should compile to a checked DAE");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("checked DAE should evaluate");
    assert!(
        probe.report.error.is_none(),
        "eval error: {:?}",
        probe.report.error
    );
    probe.report
}

fn close(found: f64, expected: f64, name: &str) {
    assert!(
        (found - expected).abs() < 1e-9,
        "{name}: expected {expected}, found {found}"
    );
}

#[test]
fn a_proven_false_condition_selects_the_else_arm() {
    let report = evaluate(
        ORIENTATION_MODEL,
        "ProvenBranchOrientation",
        "ProvenBranchOrientation.mo",
    );
    // `mod(3, 2) == 0` is false, so MLS §11.5 executes the constructor arm:
    // orientation[k] = (k - 1)*2*pi/3.
    let pi = std::f64::consts::PI;
    close(algebraic(&report, "three[1]"), 0.0, "three[1]");
    close(algebraic(&report, "three[2]"), 2.0 * pi / 3.0, "three[2]");
    close(algebraic(&report, "three[3]"), 4.0 * pi / 3.0, "three[3]");
}

#[test]
fn each_specialization_owns_its_own_constructor_extent() {
    let report = evaluate(
        ORIENTATION_MODEL,
        "ProvenBranchOrientation",
        "ProvenBranchOrientation.mo",
    );
    // The same source span denotes `1:3` in one specialization and `1:5` in the
    // other, so the domain cannot be a property of the span alone.
    let pi = std::f64::consts::PI;
    for k in 1..=5 {
        close(
            algebraic(&report, &format!("five[{k}]")),
            (f64::from(k) - 1.0) * 2.0 * pi / 5.0,
            "five",
        );
    }
}

#[test]
fn a_proven_true_condition_selects_a_nested_arm() {
    let report = evaluate(
        ORIENTATION_MODEL,
        "ProvenBranchOrientation",
        "ProvenBranchOrientation.mo",
    );
    // `mod(2, 2) == 0` and `m == 2` both hold, so the two element writes of the
    // inner arm are the whole body. That nesting is exactly what the
    // checked-branch rule cannot admit and a proven branch does not need to.
    close(algebraic(&report, "two[1]"), 0.0, "two[1]");
    close(
        algebraic(&report, "two[2]"),
        std::f64::consts::PI / 2.0,
        "two[2]",
    );
}

/// The fold must not start at a *later* condition just because that one is
/// proven: MLS §11.5 reaches `elseif m == 3` only if `u < 0.5` evaluated to
/// false, and nothing here settles that. The function is value-keyed (`y[m]`
/// reads `m`), so the specialization exists and `m == 3` really is proven —
/// the rejection has to come from the scan order, not from an absent key.
#[test]
fn an_unproven_condition_keeps_the_checked_branch_rule() {
    let source = r#"
within;
function pickchain
  input Integer m;
  input Real u;
  output Real y[m];
algorithm
  if u < 0.5 then
    for i in 1:m loop
      y[i] := u;
    end for;
  elseif m == 3 then
    y := {1.0, 2.0, 3.0};
  else
    y := {0.0, 0.0, 0.0};
  end if;
end pickchain;
model UnprovenBranch
  Real z[3];
equation
  z = pickchain(3, time);
end UnprovenBranch;
"#;
    let error = Compiler::new()
        .model("UnprovenBranch")
        .compile_str(source, "UnprovenBranch.mo")
        .expect_err("a runtime branch still owns only direct value assignments");
    let rendered = format!("{error:?}");
    assert!(
        rendered.contains("requires assignments or nested conditionals in every checked branch"),
        "unexpected diagnostic: {rendered}"
    );
}

/// The complement of the scan-order rule: when the *first* condition is proven
/// false, MLS §11.5 does reach the second, and a proven second condition folds.
#[test]
fn a_proven_false_condition_lets_the_fold_reach_the_next_arm() {
    let source = r#"
within;
function chain2
  input Integer m;
  output Real y[m];
algorithm
  if m == 5 then
    y[1] := 99.0;
    y := {9.0, 9.0, 9.0};
  elseif m == 3 then
    y[1] := 1.0;
    y[2] := 2.0;
    y[3] := 3.0;
  else
    y := {0.0, 0.0, 0.0};
  end if;
end chain2;
model ElseIfChain
  Real z[3];
equation
  z = chain2(3);
end ElseIfChain;
"#;
    let report = evaluate(source, "ElseIfChain", "ElseIfChain.mo");
    close(algebraic(&report, "z[1]"), 1.0, "z[1]");
    close(algebraic(&report, "z[2]"), 2.0, "z[2]");
    close(algebraic(&report, "z[3]"), 3.0, "z[3]");
}

/// MLS §11.2.1 makes an assignment's two sides compatible or not as a property
/// of the *statement*. MLS §11.5 decides what runs, not what is well formed, so
/// folding must not turn a malformed statement into an accepted program. Before
/// this check the fold compiled this model clean while the unfolded compiler
/// rejected it at the same statement.
#[test]
fn a_dead_arm_shape_error_is_still_rejected() {
    let source = r#"
within;
function deadbad
  input Integer m;
  output Real y[m];
protected
  Boolean flag;
algorithm
  if m == 3 then
    y := {1.0, 2.0, 3.0};
  else
    flag := y;
  end if;
end deadbad;
model DeadArmShapeError
  Real a[3];
equation
  a = deadbad(3);
end DeadArmShapeError;
"#;
    let error = Compiler::new()
        .model("DeadArmShapeError")
        .compile_str(source, "DeadArmShapeError.mo")
        .expect_err("an unexecuted branch is still an algorithm section");
    let rendered = format!("{error:?}");
    assert!(
        rendered.contains("assigns a value of shape [3] to a target of shape []"),
        "unexpected diagnostic: {rendered}"
    );
}

/// The other side of the same rule: an unexecuted branch owes no *proof*, only
/// well-formedness. Its callee is never specialized — `unprovable` has an
/// extent this scope cannot settle and would be rejected if it were — so the
/// statement is exempt rather than checked against a proof the executed path
/// never needs. This is the MSL `symmetricOrientation` shape in miniature.
#[test]
fn a_dead_arm_call_no_specialization_can_prove_stays_accepted() {
    let source = r#"
within;
function unprovable
  input Integer n;
  output Real y[n];
protected
  Integer q;
algorithm
  q := n;
  y := {1.0*k for k in 1:q};
end unprovable;
function caller
  input Integer m;
  output Real y[m];
algorithm
  if m == 3 then
    y := {1.0, 2.0, 3.0};
  else
    y := unprovable(m);
  end if;
end caller;
model DeadArmUnprovableCallee
  Real a[3];
equation
  a = caller(3);
end DeadArmUnprovableCallee;
"#;
    let report = evaluate(
        source,
        "DeadArmUnprovableCallee",
        "DeadArmUnprovableCallee.mo",
    );
    close(algebraic(&report, "a[1]"), 1.0, "a[1]");
    close(algebraic(&report, "a[3]"), 3.0, "a[3]");
}

/// Whether a conditional folds is gated on `ValueReadInputs`: only an input
/// whose value a declared dimension, compact range, or `zeros`/`ones`/`fill`
/// extent reads becomes part of the specialization key. These two functions
/// differ only in that dimension, and they are rejected by two different owners
/// because of it — the unkeyed one never folds and hits the checked-branch
/// join, the keyed one folds and hits output totality. The gate is what keeps
/// MLS §4.5 non-structural parameters out of translation-time control flow, so
/// this pair pins it in both directions.
#[test]
fn a_declared_dimension_that_reads_the_input_is_what_enables_the_fold() {
    let unkeyed = r#"
within;
function partial_assembly
  input Integer m;
  output Real y[3];
algorithm
  if m == 3 then
    y[1] := 1.0;
    y[2] := 2.0;
  end if;
end partial_assembly;
model GateOff
  Real a[3];
equation
  a = partial_assembly(3);
end GateOff;
"#;
    let error = Compiler::new()
        .model("GateOff")
        .compile_str(unkeyed, "GateOff.mo")
        .expect_err("an unkeyed input leaves the conditional unproven");
    let rendered = format!("{error:?}");
    assert!(
        rendered.contains("without a definition on some branch"),
        "unkeyed function should hit the checked-branch join: {rendered}"
    );

    let keyed = unkeyed
        .replace("output Real y[3]", "output Real y[m]")
        .replace("GateOff", "GateOn");
    let error = Compiler::new()
        .model("GateOn")
        .compile_str(&keyed, "GateOn.mo")
        .expect_err("the folded arm still defines only two of three elements");
    let rendered = format!("{error:?}");
    assert!(
        rendered.contains("without defining every declared element"),
        "keyed function should fold and hit output totality: {rendered}"
    );
}

/// An MLS §12.9 external argument is inside a function specialization with no
/// Modelica body to lower into, so "has a body" is the wrong predicate for
/// which comprehension-domain owner applies. Keying on the body panicked here —
/// on both sides of the fold — because the span has no model-wide plan.
#[test]
fn an_external_argument_comprehension_is_owned_by_its_specialization() {
    let source = r#"
within;
function extcomp
  input Integer m;
  input Real u[m];
  output Real y;
  external "C" y = ext_sum({u[k]*k for k in 1:m}, m);
end extcomp;
model ExternalArgumentComprehension
  Real z;
equation
  z = extcomp(3, {time, time, time});
end ExternalArgumentComprehension;
"#;
    Compiler::new()
        .model("ExternalArgumentComprehension")
        .compile_str(source, "ExternalArgumentComprehension.mo")
        .expect("an external argument's comprehension is folded by its specialization");
}

/// `Modelica.Math.BooleanVectors.countTrue` writes its iterator range over an
/// argument's *shape* rather than an input's value, which is the other half of
/// what MLS §12.2 lets a range be written over. Before the fix this reached the
/// lowering with no plan for the span at all and panicked there.
#[test]
fn a_constructor_range_over_an_argument_shape_axis_is_settled() {
    let source = r#"
within;
function weigh
  input Real u[:];
  output Real total;
algorithm
  total := sum({u[i]*i for i in 1:size(u, 1)});
end weigh;
model ShapeBoundComprehension
  Real z;
equation
  z = weigh({time, 2*time, 3*time});
end ShapeBoundComprehension;
"#;
    let report = evaluate(
        source,
        "ShapeBoundComprehension",
        "ShapeBoundComprehension.mo",
    );
    // u = {0, 0, 0} at t = 0, so the sum is 0; the point is that the extent is
    // proven at all. A nonzero probe would need a time-varying evaluation.
    close(algebraic(&report, "z"), 0.0, "z");
}

#[test]
fn a_constructor_range_the_specialization_cannot_settle_is_rejected() {
    let source = r#"
within;
function ramp
  input Real u;
  output Real y[3];
protected
  Integer n;
algorithm
  n := integer(u);
  y := {u*k for k in 1:n};
end ramp;
model UnprovenComprehension
  Real z[3];
equation
  z = ramp(time);
end UnprovenComprehension;
"#;
    let error = Compiler::new()
        .model("UnprovenComprehension")
        .compile_str(source, "UnprovenComprehension.mo")
        .expect_err("a constructor extent this scope does not settle has no checked domain");
    let rendered = format!("{error:?}");
    // The rejection must name the unsettled *bound*, not merely the construct:
    // `n` depends on the runtime input value, so this specialization cannot
    // prove a compact constructor domain from the declared output shape alone.
    assert!(
        rendered.contains("range end is not an exact Integer this function specialization settles"),
        "unexpected diagnostic: {rendered}"
    );
    assert!(
        rendered.contains("value of scalar `n`"),
        "the rejection must name the unsettled bound: {rendered}"
    );
}

/// MLS §10.4.1 opens each index as a fresh scalar of the comprehension, so a
/// triangular domain reads an index that has a shape and no value. It must be
/// reported at that index, never folded to a rectangular over-approximation.
#[test]
fn a_triangular_constructor_domain_is_rejected_at_its_index() {
    let source = r#"
within;
function triangular
  input Integer m;
  output Real y;
algorithm
  y := sum({1.0*i*j for i in 1:m, j in 1:i});
end triangular;
model TriangularComprehension
  Real z;
equation
  z = triangular(3);
end TriangularComprehension;
"#;
    let error = Compiler::new()
        .model("TriangularComprehension")
        .compile_str(source, "TriangularComprehension.mo")
        .expect_err("a triangular domain is not rectangular");
    let rendered = format!("{error:?}");
    assert!(
        rendered.contains("value of scalar `i`"),
        "the rejection must name the comprehension index: {rendered}"
    );
}
