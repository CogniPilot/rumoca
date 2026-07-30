//! Resolve-phase semantic checks: cardinality operands, loop-index shadowing,
//! `Evaluate` annotation scope, single-assignment `when` targets, and
//! unsupported state-machine operators.

use super::*;

#[test]
fn warning_only_semantics_construct_a_resolved_tree_with_advisories() {
    let source = r#"
function BareExternal
  input Real u;
  output Real y;
external "C" y = bare_external(u);
end BareExternal;

model WarningOnly
  parameter Real q(start=1) annotation(Evaluate=true);
  Real x;
equation
  x = 1;
end WarningOnly;
"#;

    let success = match resolve_with_diagnostics(parsed_tree_from_source(source)) {
        Ok(success) => success,
        Err(failure) => panic!(
            "warning-only source must receive a ResolvedTree proof: {:?}",
            failure.diagnostics()
        ),
    };
    let (_, diagnostics) = success.into_parts();
    assert!(!diagnostics.has_errors());
    for code in ["WR001", "WR005"] {
        let diagnostic = diagnostics
            .iter()
            .find(|diagnostic| diagnostic.code.as_deref() == Some(code))
            .unwrap_or_else(|| panic!("expected advisory {code}, got {diagnostics:?}"));
        assert!(
            !diagnostic.is_error(),
            "{code} must remain advisory: {diagnostic:?}"
        );
    }
}

#[test]
fn mixed_warning_and_error_semantics_cannot_construct_a_resolved_tree() {
    let source = r#"
model MixedSeverity
  parameter Real q(start=1) annotation(Evaluate=true);
  Real x;
equation
  x = missing;
end MixedSeverity;
"#;

    let failure = match resolve_with_diagnostics(parsed_tree_from_source(source)) {
        Ok(_) => panic!("an unresolved reference must prevent a ResolvedTree proof"),
        Err(failure) => failure,
    };
    let diagnostics = failure.diagnostics();
    assert!(
        diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code.as_deref() == Some("WR005")
                && !diagnostic.is_error()),
        "expected the advisory to remain visible: {diagnostics:?}"
    );
    assert!(
        diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code.as_deref() == Some("ER002") && diagnostic.is_error()),
        "expected the exact unresolved-reference error: {diagnostics:?}"
    );
}

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

/// MLS §18.6: `Evaluate` "only has effect for a component declared with the
/// prefix parameter". A function has no parameter-variability locals, so the
/// annotation is without effect there rather than illegal — the shape MSL
/// 4.1.0 relies on in
/// `Modelica.Electrical.Machines.SpacePhasors.Functions.ToSpacePhasor`. The
/// declaration must still resolve, and the advisory must stay advisory.
#[test]
fn test_evaluate_on_function_local_component_is_an_ignored_advisory() {
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
    let success = match resolve_with_diagnostics(parsed_tree_from_source(source)) {
        Ok(success) => success,
        Err(failure) => panic!(
            "a function-local Evaluate annotation must not block resolution: {:?}",
            failure.diagnostics()
        ),
    };
    let (_, diagnostics) = success.into_parts();
    assert!(
        !diagnostics.has_errors(),
        "function-local Evaluate must not be an error: {diagnostics:?}"
    );
    let advisory = diagnostics
        .iter()
        .find(|diagnostic| diagnostic.code.as_deref() == Some("WR006"))
        .unwrap_or_else(|| panic!("expected the WR006 no-effect advisory, got: {diagnostics:?}"));
    assert!(
        !advisory.is_error(),
        "WR006 must remain advisory: {advisory:?}"
    );
}

/// The same annotation outside a function keeps the ANN-008 hard rejection:
/// there the modeler can declare the component `parameter` or `constant`.
#[test]
fn test_evaluate_on_model_local_component_is_an_error() {
    let source = r#"
model M
  Integer m annotation(Evaluate=true);
equation
  m = 1;
end M;
"#;
    let diagnostics =
        resolve_test_source(source).expect_err("model components are not exempt from ANN-008");
    assert!(
        diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code.as_deref() == Some("ER070")),
        "expected ER070 for invalid model-local Evaluate annotation, got: {diagnostics:?}"
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
fn test_when_single_assign_leaves_cross_owner_identity_to_typed_ir() {
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
    resolve_test_source(source)
        .expect("Resolve does not compare ownership using rendered component references");
}

#[test]
fn test_when_single_assign_does_not_claim_branch_local_multiplicity() {
    let source = r#"
model DuplicateWithinWhenBranch
  Boolean trigger;
  discrete Real x;
equation
  when edge(trigger) then
    x = 1;
    x = 2;
  end when;
end DuplicateWithinWhenBranch;
"#;

    resolve_test_source(source)
        .expect("EQN-020 only compares definitions owned by distinct when-equations");
}

#[test]
fn test_eqn_012_branch_mismatch_is_not_claimed_by_when_owner_check() {
    let source = r#"
model BranchVariableSetMismatch
  Boolean sel;
  Integer i(start = 0);
  Integer j(start = 0);
  Boolean c = time > 1;
equation
  when c then
    if sel then
      i = 1;
    else
      j = 2;
    end if;
  end when;
end BranchVariableSetMismatch;
"#;

    resolve_test_source(source).expect(
        "Resolve must leave EQN-012 branch-set validation to ToDAE's semantic-owner constructor",
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
fn test_when_single_assign_leaves_nested_cross_owner_identity_to_typed_ir() {
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

    resolve_test_source(source)
        .expect("Resolve leaves cross-owner comparison to a typed semantic-owner constructor");
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
fn test_reinit_leaves_cross_owner_identity_to_typed_ir() {
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

    resolve_test_source(source)
        .expect("Resolve leaves cross-owner comparison to the typed DAE constructor");
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
fn test_reinit_leaves_branch_distribution_to_flatten() {
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

    resolve_test_source(source)
        .expect("Resolve leaves branch-distribution checks to Flat target identity");
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
