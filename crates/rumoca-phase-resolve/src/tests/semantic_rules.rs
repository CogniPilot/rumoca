//! Resolve-phase semantic checks: cardinality operands, loop-index shadowing,
//! `Evaluate` annotation scope, single-assignment `when` targets, and
//! unsupported state-machine operators.

use super::*;

#[test]
fn test_cardinality_allows_indexed_connector_array_element() {
    let source = r#"
connector Port
  Real p;
end Port;

model UsesIndexedCardinality
  Port ports[2];
equation
  if cardinality(ports[1]) == 0 then
ports[1].p = 0;
  end if;
end UsesIndexedCardinality;
"#;
    resolve_test_source(source).expect("indexed connector array element is scalar");
}

#[test]
fn test_cardinality_rejects_unindexed_connector_array() {
    let source = r#"
connector Port
  Real p;
end Port;

model UsesArrayCardinality
  Port ports[2];
equation
  if cardinality(ports) == 0 then
ports[1].p = 0;
  end if;
end UsesArrayCardinality;
"#;
    let diags = resolve_test_source(source).expect_err("connector array target must fail");
    assert!(
        diags
            .iter()
            .any(|d| d.code.as_deref() == Some("ER057")
                && d.message.contains("connector array 'ports'")),
        "expected cardinality connector-array diagnostic, got: {diags:?}"
    );
}

#[test]
fn test_loop_index_named_like_class_does_not_trigger_class_used_as_value() {
    let source = r#"
package P
  model j
  end j;
end P;

model UsesLoopIndexJ
  Integer y;
equation
  for j in 1:2 loop
y = j;
  end for;
end UsesLoopIndexJ;
"#;
    resolve_test_source(source)
        .expect("loop index `j` must resolve as a value, not as global class `j`");
}

#[test]
fn test_evaluate_on_non_parameter_component_is_always_an_error() {
    let source = r#"
model EvaluateScopeWarning
  Real x annotation(Evaluate=true);
equation
  x = 1;
end EvaluateScopeWarning;
"#;
    let diagnostics = resolve_test_source(source)
        .expect_err("Evaluate annotation scope is a mandatory MLS error");
    assert!(
        diagnostics
            .iter()
            .any(|diag| diag.code.as_deref() == Some("ER070")),
        "expected ER070 for invalid Evaluate annotation, got: {diagnostics:?}"
    );
}

#[test]
fn test_evaluate_on_function_local_component_is_an_error() {
    let source = r#"
function F
  input Real x[:];
  output Real y;
protected
  Integer m=size(x, 1) annotation(Evaluate=true);
algorithm
  y := m;
end F;
"#;
    let diagnostics =
        resolve_test_source(source).expect_err("function locals are not exempt from ANN-008");
    assert!(
        diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code.as_deref() == Some("ER070")),
        "expected ER070 for invalid function-local Evaluate annotation, got: {diagnostics:?}"
    );
}

#[test]
fn test_when_single_assign_allows_distinct_indexed_targets() {
    let source = r#"
model IndexedWhenTargets
  Boolean open1;
  Boolean open2;
  Real t0[2];
equation
  when edge(open1) then
t0[1] = time;
  end when;
  when edge(open2) then
t0[2] = time;
  end when;
end IndexedWhenTargets;
"#;
    resolve_test_source(source)
        .expect("distinct indexed targets in separate when-equations should be allowed");
}

#[test]
fn test_when_single_assign_rejects_same_target_across_when_equations() {
    let source = r#"
model DuplicateWhenTarget
  Boolean open1;
  Boolean open2;
  Real t0;
equation
  when edge(open1) then
t0 = time;
  end when;
  when edge(open2) then
t0 = time;
  end when;
end DuplicateWhenTarget;
"#;
    let result = resolve_test_source(source);
    assert!(result.is_err(), "duplicate when target should fail");
    let diagnostics = result.expect_err("expected diagnostics");
    assert!(
        diagnostics
            .iter()
            .any(|diag| diag.code.as_deref() == Some("ER053")),
        "expected ER053 for duplicate when target, got: {diagnostics:?}"
    );
}

#[test]
fn test_when_single_assign_rejects_duplicate_in_one_branch_at_second_target() {
    let source = r#"
model DuplicateWithinWhenBranch
  Boolean trigger;
  Real x;
equation
  when edge(trigger) then
    x = 1;
    x = 2;
  end when;
end DuplicateWithinWhenBranch;
"#;

    let diagnostics =
        resolve_test_source(source).expect_err("one when branch cannot define x twice");
    let diagnostic = diagnostics
        .iter()
        .find(|diagnostic| diagnostic.code.as_deref() == Some("ER053"))
        .expect("duplicate definition must report ER053");
    let label = diagnostic
        .labels
        .iter()
        .find(|label| label.primary)
        .expect("ER053 must identify the second definition");
    assert_eq!(&source[label.span.start.0..label.span.end.0], "x");
    assert_eq!(
        label.span.start.0,
        source.find("x = 2").expect("second assignment is present"),
        "ER053 must point at the second target, not the first"
    );
}

#[test]
fn test_when_single_assign_rejects_assignment_before_conditional_definition() {
    let source = r#"
model AssignmentBeforeConditional
  Boolean trigger;
  Boolean choose;
  Real x;
equation
  when edge(trigger) then
    x = 1;
    if choose then
      x = 2;
    else
      x = 3;
    end if;
  end when;
end AssignmentBeforeConditional;
"#;

    let diagnostics = resolve_test_source(source)
        .expect_err("a conditional contributes its target once to the enclosing sequence");
    let diagnostic = diagnostics
        .iter()
        .find(|diagnostic| diagnostic.code.as_deref() == Some("ER053"))
        .expect("sequential assignment and conditional definition must report ER053");
    let label = diagnostic
        .labels
        .iter()
        .find(|label| label.primary)
        .expect("ER053 must identify the colliding conditional target");
    assert_eq!(&source[label.span.start.0..label.span.end.0], "x");
    assert_eq!(
        label.span.start.0,
        source
            .find("x = 2")
            .expect("first conditional target is present"),
        "the conditional's representative target is the second occurrence"
    );
}

#[test]
fn test_when_single_assign_allows_target_in_mutually_exclusive_if_branches() {
    let source = r#"
model MutuallyExclusiveConditionalTargets
  Boolean trigger;
  Boolean choose;
  Real x;
equation
  when edge(trigger) then
    if choose then
      x = 1;
    else
      x = 2;
    end if;
  end when;
end MutuallyExclusiveConditionalTargets;
"#;

    resolve_test_source(source)
        .expect("mutually exclusive conditional branches may define the same target");
}

#[test]
fn test_when_single_assign_allows_target_in_elsewhen_branches() {
    let source = r#"
model MutuallyExclusiveElsewhenTargets
  Boolean firstTrigger;
  Boolean secondTrigger;
  Real x;
equation
  when edge(firstTrigger) then
    x = 1;
  elsewhen edge(secondTrigger) then
    x = 2;
  end when;
end MutuallyExclusiveElsewhenTargets;
"#;

    resolve_test_source(source)
        .expect("source branches of one when/elsewhen chain are mutually exclusive");
}

#[test]
fn test_when_single_assign_allows_target_in_outer_if_alternatives() {
    let source = r#"
model MutuallyExclusiveOuterIfTargets
  parameter Boolean chooseFirst = true;
  Boolean firstTrigger;
  Boolean secondTrigger;
  Real x;
equation
  if chooseFirst then
    when edge(firstTrigger) then
      x = 1;
    end when;
  else
    when edge(secondTrigger) then
      x = 2;
    end when;
  end if;
end MutuallyExclusiveOuterIfTargets;
"#;

    resolve_test_source(source)
        .expect("mutually exclusive outer if alternatives may own the same when target");
}

#[test]
fn test_when_single_assign_rejects_sequential_owners_in_outer_if_branch() {
    let source = r#"
model SequentialWhenOwnersInOuterIf
  parameter Boolean enabled = true;
  Boolean firstTrigger;
  Boolean secondTrigger;
  Real x;
equation
  if enabled then
    when edge(firstTrigger) then
      x = 1;
    end when;
    when edge(secondTrigger) then
      x = 2;
    end when;
  end if;
end SequentialWhenOwnersInOuterIf;
"#;

    let diagnostics = resolve_test_source(source)
        .expect_err("sequential when owners on one selected path cannot share a target");
    let diagnostic = diagnostics
        .iter()
        .find(|diagnostic| diagnostic.code.as_deref() == Some("ER053"))
        .expect("sequential owners must report ER053");
    let label = diagnostic
        .labels
        .iter()
        .find(|label| label.primary)
        .expect("ER053 must identify the second owner");
    assert_eq!(&source[label.span.start.0..label.span.end.0], "x");
    assert_eq!(
        label.span.start.0,
        source
            .find("x = 2")
            .expect("second owner target is present"),
        "ER053 must point at the target in the second sequential owner"
    );
}

#[test]
fn test_reinit_allows_same_state_in_outer_if_alternatives() {
    let source = r#"
model MutuallyExclusiveOuterIfReinit
  parameter Boolean chooseFirst = true;
  Boolean firstTrigger;
  Boolean secondTrigger;
  Real x(start = 0);
equation
  der(x) = 0;
  if chooseFirst then
    when edge(firstTrigger) then
      reinit(x, 1);
    end when;
  else
    when edge(secondTrigger) then
      reinit(x, 2);
    end when;
  end if;
end MutuallyExclusiveOuterIfReinit;
"#;

    resolve_test_source(source)
        .expect("mutually exclusive outer if alternatives may reinitialize the same state");
}

#[test]
fn test_reinit_rejects_sequential_owners_in_outer_if_branch_at_second_target() {
    let source = r#"
model SequentialReinitOwnersInOuterIf
  parameter Boolean enabled = true;
  Boolean firstTrigger;
  Boolean secondTrigger;
  Real x(start = 0);
equation
  der(x) = 0;
  if enabled then
    when edge(firstTrigger) then
      reinit(x, 1);
    end when;
    when edge(secondTrigger) then
      reinit(x, 2);
    end when;
  end if;
end SequentialReinitOwnersInOuterIf;
"#;

    let diagnostics = resolve_test_source(source)
        .expect_err("sequential when owners cannot both reinitialize one state");
    let diagnostic = diagnostics
        .iter()
        .find(|diagnostic| diagnostic.code.as_deref() == Some("ER051"))
        .expect("sequential reinit owners must report ER051");
    let label = diagnostic
        .labels
        .iter()
        .find(|label| label.primary)
        .expect("ER051 must identify the second reinit target");
    assert_eq!(&source[label.span.start.0..label.span.end.0], "x");
    assert_eq!(
        label.span.start.0,
        source
            .find("x, 2")
            .expect("second reinit target is present"),
        "ER051 must point at the target in the second sequential owner"
    );
}

#[test]
fn test_reinit_allows_same_state_in_mutually_exclusive_inner_if_branches() {
    let source = r#"
model MutuallyExclusiveInnerIfReinit
  Boolean trigger;
  Boolean chooseFirst;
  Real x(start = 0);
equation
  der(x) = 0;
  when edge(trigger) then
    if chooseFirst then
      reinit(x, 1);
    else
      reinit(x, 2);
    end if;
  end when;
end MutuallyExclusiveInnerIfReinit;
"#;

    resolve_test_source(source)
        .expect("mutually exclusive branches of one inner if may reinitialize the same state");
}

#[test]
fn test_reinit_rejects_overlapping_sequential_inner_if_paths() {
    let source = r#"
model OverlappingSequentialInnerIfReinit
  Boolean trigger;
  Boolean firstChoice;
  Boolean secondChoice;
  Real x(start = 0);
equation
  der(x) = 0;
  when edge(trigger) then
    if firstChoice then
      reinit(x, 1);
    end if;
    if secondChoice then
      assert(true, "no reinit on this alternative");
    else
      reinit(x, 2);
    end if;
  end when;
end OverlappingSequentialInnerIfReinit;
"#;

    let diagnostics = resolve_test_source(source)
        .expect_err("two sequential inner if paths may both reinitialize x");
    let diagnostic = diagnostics
        .iter()
        .find(|diagnostic| diagnostic.code.as_deref() == Some("ER052"))
        .expect("overlapping reinit paths must report ER052");
    let label = diagnostic
        .labels
        .iter()
        .find(|label| label.primary)
        .expect("ER052 must identify the second reinit target");
    assert_eq!(&source[label.span.start.0..label.span.end.0], "x");
    assert_eq!(
        label.span.start.0,
        source
            .find("x, 2")
            .expect("second reinit target is present"),
        "ER052 must point at the second potentially executable reinit"
    );
}

#[test]
fn test_single_branch_clocked_when_is_valid() {
    let source = r#"
model SingleClockedWhen
  Clock c = Clock(0.1);
  Real x(start = 0);
equation
  when c then
    x = previous(x) + 1;
  end when;
end SingleClockedWhen;
"#;

    resolve_test_source(source).expect("one branch is the complete clocked when grammar");
}

#[test]
fn test_clocked_elsewhen_reports_offending_condition_span() {
    let source = r#"
model ClockedElsewhen
  Clock firstClock = Clock(0.1);
  Clock secondClock = Clock(0.2);
  Real x(start = 0);
equation
  when firstClock then
    x = previous(x) + 1;
  elsewhen secondClock then
    x = previous(x) + 2;
  end when;
end ClockedElsewhen;
"#;

    let diagnostics =
        resolve_test_source(source).expect_err("clocked when cannot own an elsewhen branch");
    let diagnostic = diagnostics
        .iter()
        .find(|diagnostic| diagnostic.code.as_deref() == Some("ER131"))
        .expect("CLK-014 diagnostic must be present");
    assert!(
        diagnostic
            .message
            .contains("cannot contain elsewhen branches"),
        "unexpected CLK-014 diagnostic: {diagnostic:?}"
    );
    let label = diagnostic
        .labels
        .iter()
        .find(|label| label.primary)
        .expect("CLK-014 diagnostic has an exact primary label");
    assert_eq!(
        &source[label.span.start.0..label.span.end.0],
        "secondClock",
        "diagnostic must label the offending elsewhen condition"
    );
}

#[test]
fn test_clocked_elsewhen_nested_in_if_and_for_is_rejected() {
    let source = r#"
model NestedClockedElsewhen
  parameter Boolean enabled = true;
  Clock firstClock = Clock(0.1);
  Clock secondClock = Clock(0.2);
  Real x(start = 0);
equation
  if enabled then
    for i in 1:1 loop
      when firstClock then
        x = previous(x) + 1;
      elsewhen secondClock then
        x = previous(x) + 2;
      end when;
    end for;
  end if;
end NestedClockedElsewhen;
"#;

    let diagnostics = resolve_test_source(source)
        .expect_err("equation containers cannot hide a clocked elsewhen");
    let diagnostic = diagnostics
        .iter()
        .find(|diagnostic| diagnostic.code.as_deref() == Some("ER131"))
        .expect("recursive CLK-014 diagnostic must be present");
    let label = diagnostic
        .labels
        .iter()
        .find(|label| label.primary)
        .expect("recursive CLK-014 diagnostic has a primary label");
    assert_eq!(&source[label.span.start.0..label.span.end.0], "secondClock");
}

#[test]
fn test_state_machine_operator_reports_explicit_unsupported_diagnostic() {
    let source = r#"
model UnsupportedStateMachine
Real a;
equation
transition(a, a, true);
end UnsupportedStateMachine;
"#;

    let diagnostics =
        resolve_test_source(source).expect_err("state-machine operators are unsupported");
    assert!(
        diagnostics.iter().any(|diag| {
            diag.code.as_deref() == Some("ER073")
                && diag
                    .message
                    .contains("transition() requires Modelica state-machine")
        }),
        "expected explicit state-machine unsupported diagnostic, got: {diagnostics:?}"
    );
}

#[test]
fn test_qualified_initial_state_function_is_not_state_machine_operator() {
    let source = r#"
package P
function initialState
    output Real y;
algorithm
    y := 1;
end initialState;

model M
    Real x = P.initialState();
end M;
end P;
"#;

    let resolved = resolve_test_source(source).expect("qualified function should resolve");
    let model = resolved
        .inner()
        .definitions
        .classes
        .get("P")
        .and_then(|package| package.classes.get("M"))
        .expect("P.M should exist");
    let binding = model
        .components
        .get("x")
        .and_then(|component| component.binding.as_ref())
        .expect("x should have a binding");
    let ast::Expression::FunctionCall { comp, .. } = binding else {
        panic!("x binding should remain a function call");
    };
    assert_eq!(comp.to_string(), "P.initialState");
}
