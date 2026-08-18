use rumoca::Compiler;
use rumoca_ir_dae::{
    ContinuousOwnerView, DaeGeneration, DaeProvenanceOrigin, DiscreteRealActivation, VariableRole,
};
use rumoca_sim::{SimOptions, simulate_dae};

const CONTINUOUS_ALGORITHM: &str = r#"
model ContinuousAlgorithm
  Real x(start = 1, fixed = true);
  Real y;
algorithm
  if x > 0 then
    y := 2 * x;
  else
    y := -x;
  end if;
equation
  der(x) = -y;
end ContinuousAlgorithm;
"#;

const EVENT_GUARDED_ALGORITHM: &str = r#"
package EventGuarded
  type L = enumeration(U, X, Z, ZERO, ONE);

  model Lookup
    parameter Real map[L, L] = [1,1,1,1,1;
                                1,2,2,2,2;
                                1,2,3,3,3;
                                1,2,3,4,4;
                                1,2,3,4,5];
    L a(start = L.U);
    L b(start = L.U);
    Real f(start = 1);
  algorithm
    if change(a) or change(b) then
      f := map[a, b];
    end if;
  end Lookup;
end EventGuarded;
"#;

#[test]
fn continuous_model_algorithm_remains_an_algebraic_equation() {
    let compiled = Compiler::new()
        .model("ContinuousAlgorithm")
        .compile_str(CONTINUOUS_ALGORITHM, "continuous_algorithm.mo")
        .expect("continuous algorithm should construct checked DAE");

    compiled.dae.inspect(|view| {
        let y = view
            .variables()
            .find(|(_, variable)| variable.name().as_str() == "y")
            .map(|(_, variable)| variable)
            .expect("algorithm target y remains in the checked catalog");
        assert_eq!(y.role(), VariableRole::Algebraic);
        let algorithm = (0..view.continuous_equation_count())
            .filter_map(|index| view.continuous_equation(index))
            .find(|equation| {
                equation.provenance().origin()
                    == DaeProvenanceOrigin::Generated(DaeGeneration::AlgorithmEquation)
            });
        assert!(
            algorithm
                .is_some_and(|equation| { view.source_text(equation.provenance()) == Some("x") }),
            "generated algorithm equation retains the responsible condition occurrence"
        );
    });

    let wire = serde_json::to_string(&compiled.dae)
        .expect("continuous algorithm should serialize through wire-v11");
    let decoded: rumoca_compile::compile::Dae =
        serde_json::from_str(&wire).expect("wire-v11 should reconstruct the checked owners");
    let simulation = simulate_dae(&decoded, &SimOptions::default())
        .expect("decoded algorithm equation should lower to computable Solve IR");
    let x = simulation
        .names
        .iter()
        .position(|name| name == "x")
        .expect("state x is visible");
    let y = simulation
        .names
        .iter()
        .position(|name| name == "y")
        .expect("algorithm target y is visible");
    let x_final = simulation.data[x]
        .last()
        .copied()
        .expect("state trace is non-empty");
    let y_final = simulation.data[y]
        .last()
        .copied()
        .expect("algebraic trace is non-empty");
    assert!((x_final - (-2.0_f64).exp()).abs() <= 5.0e-4);
    assert!((y_final - 2.0 * x_final).abs() <= 5.0e-8);
}

#[test]
fn event_guarded_model_algorithm_constructs_one_checked_discrete_action() {
    let compiled = Compiler::new()
        .model("EventGuarded.Lookup")
        .compile_str(EVENT_GUARDED_ALGORITHM, "event_guarded_algorithm.mo")
        .expect("event-guarded algorithm should construct checked DAE");

    compiled.dae.inspect(|view| {
        let f = view
            .variables()
            .find(|(_, variable)| variable.name().as_str() == "f")
            .map(|(_, variable)| variable)
            .expect("algorithm target f remains in the checked catalog");
        assert_eq!(f.role(), VariableRole::DiscreteReal);
        // A guarded discrete assignment is a condition-activated discrete Real
        // equation, not an assert/terminate/reinitialize event action: the
        // checked DAE models `f := map[a, b]` under `change(a) or change(b)`
        // as one `When`-activated row that owns the source occurrence.
        assert_eq!(view.event_action_count(), 0);
        assert_eq!(view.discrete_real_equation_count(), 1);
        let action = view
            .discrete_real_equation(0)
            .expect("one checked discrete action resolves within this DAE");
        assert!(matches!(
            action.activation(),
            DiscreteRealActivation::When { .. }
        ));
        assert_eq!(action.provenance().origin(), DaeProvenanceOrigin::Source);
        assert_eq!(view.source_text(action.provenance()), Some("f"));
    });

    let wire =
        serde_json::to_string(&compiled.dae).expect("event-guarded algorithm should serialize");
    let decoded: rumoca_compile::compile::Dae =
        serde_json::from_str(&wire).expect("wire-v11 should reconstruct the checked event owner");
    decoded.inspect(|view| {
        assert_eq!(view.discrete_real_equation_count(), 1);
        assert!(matches!(
            view.discrete_real_equation(0)
                .expect("decoded discrete action resolves")
                .activation(),
            DiscreteRealActivation::When { .. }
        ));
    });
}

#[test]
fn sampled_multi_result_record_receiver_projects_checked_discrete_leaves() {
    let compiled = Compiler::new()
        .model("RecordTransition.Controller")
        .compile_str(
            r#"
package RecordTransition
  record State
    Real first;
    Real second;
  end State;

  function advance
    input State previous;
    output State next;
    output Real command;
  algorithm
    next.first := previous.first + 1.0;
    next.second := previous.second + 2.0;
    command := next.first + next.second;
  end advance;

  block Controller
    discrete output Real first(start = 0.0);
    discrete output Real second(start = 0.0);
    discrete output Real command(start = 0.0);
  protected
    State nextState;
    discrete Real nextCommand(start = 0.0);
  algorithm
    when sample(0.0, 0.1) then
      (nextState, nextCommand) := advance(
        State(pre(first), pre(second)));
      first := nextState.first;
      second := nextState.second;
      command := nextCommand;
    end when;
  end Controller;
end RecordTransition;
"#,
            "record_transition.mo",
        )
        .expect("a whole record call receiver should construct discrete leaf owners");

    compiled.dae.inspect(|view| {
        let projected_calls = (0..view.expression_count())
            .filter_map(|index| view.expression(view.expression_id(index)?))
            .filter(|expression| {
                let rumoca_ir_dae::ExpressionOperation::Field { base, .. } = expression.operation()
                else {
                    return false;
                };
                view.expression(base).is_some_and(|base| {
                    matches!(
                        base.operation(),
                        rumoca_ir_dae::ExpressionOperation::Call { .. }
                    )
                })
            })
            .count();
        assert_eq!(
            projected_calls, 2,
            "the two record leaves must project one checked record-valued call"
        );
    });

    let simulation = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 0.0,
            ..SimOptions::default()
        },
    )
    .expect("the checked record transition should execute at the initial sample");
    for (name, expected) in [("first", 1.0), ("second", 2.0), ("command", 3.0)] {
        let index = simulation
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap_or_else(|| panic!("trace should contain `{name}`"));
        assert_eq!(simulation.data[index].last().copied(), Some(expected));
    }
}

#[test]
fn model_algorithm_read_before_definition_fails_in_dae_analysis() {
    let error = Compiler::new()
        .model("InvalidAlgorithmMemory")
        .compile_str(
            r#"
model InvalidAlgorithmMemory
  Real y;
algorithm
  y := y + 1;
end InvalidAlgorithmMemory;
"#,
            "invalid_algorithm_memory.mo",
        )
        .expect_err("an algorithm must not acquire implicit continuous memory");
    let message = error.to_string();
    assert!(
        message.contains("unsupported model algorithm")
            && message.contains("read before definition"),
        "the responsible algorithm owner should reject missing initialization: {message}"
    );
}

#[test]
fn mixed_continuous_event_algorithm_fails_before_construction() {
    let error = Compiler::new()
        .model("MixedAlgorithm")
        .compile_str(
            r#"
model MixedAlgorithm
  Real x;
  discrete Real z;
algorithm
  x := 1;
  when time > 0.5 then
    z := 1;
  end when;
end MixedAlgorithm;
"#,
            "mixed_algorithm.mo",
        )
        .expect_err("mixed partitions need an explicit checked atomic owner");
    let message = error.to_string();
    assert!(
        message.contains("mixed continuous/event algorithm")
            && message.contains("checked atomic owner"),
        "mixed ownership must fail at the source algorithm: {message}"
    );
}

#[test]
fn total_array_algorithm_stays_compact_and_computable() {
    let compiled = Compiler::new()
        .model("TotalArrayAlgorithm")
        .compile_str(
            r#"
model TotalArrayAlgorithm
  Real x[5];
algorithm
  for i in 1:5 loop
    x[i] := i * 1.0;
  end for;
end TotalArrayAlgorithm;
"#,
            "total_array_algorithm.mo",
        )
        .expect("total array algorithm should construct one compact owner");

    compiled.dae.inspect(|view| {
        assert_eq!(view.continuous_owner_count(), 1);
        let ContinuousOwnerView::Structured { family, .. } = view
            .continuous_owner(0)
            .expect("array algorithm has a checked equation owner")
        else {
            panic!("array algorithm must not scalar-unroll in DAE");
        };
        assert_eq!(family.scalar_rows(), 5);
        assert_eq!(
            family.provenance().origin(),
            DaeProvenanceOrigin::Generated(DaeGeneration::AlgorithmEquation)
        );
    });

    let wire =
        serde_json::to_string(&compiled.dae).expect("compact array algorithm should serialize");
    let decoded: rumoca_compile::compile::Dae =
        serde_json::from_str(&wire).expect("wire-v11 should reconstruct the compact owner");
    let simulation = simulate_dae(&decoded, &SimOptions::default())
        .expect("compact array algorithm should lower to computable Solve IR");
    for index in 1..=5 {
        let name = format!("x[{index}]");
        let variable = simulation
            .names
            .iter()
            .position(|candidate| candidate == &name)
            .unwrap_or_else(|| panic!("{name} is visible"));
        let value = simulation.data[variable]
            .last()
            .copied()
            .expect("array result trace is non-empty");
        assert!((value - index as f64).abs() <= 1.0e-10);
    }
}

#[test]
fn separated_array_reduction_stays_compact_and_computable() {
    let compiled = Compiler::new()
        .model("SeparatedArrayReduction")
        .compile_str(
            r#"
model SeparatedArrayReduction
  Real x[3];
  Real total;
algorithm
  total := 0;
  for i in 1:3 loop
    x[i] := i * 2.0;
    total := total + x[i];
  end for;
end SeparatedArrayReduction;
"#,
            "separated_array_reduction.mo",
        )
        .expect("proved array reduction should construct checked DAE");

    compiled.dae.inspect(|view| {
        let total = view
            .variables()
            .find(|(_, variable)| variable.name().as_str() == "total")
            .map(|(_, variable)| variable)
            .expect("reduction result remains in the checked catalog");
        assert_eq!(total.role(), VariableRole::Algebraic);
        assert_eq!(view.continuous_owner_count(), 2);
        let structured = view
            .continuous_owners()
            .find_map(|owner| match owner {
                ContinuousOwnerView::Structured { family, .. } => Some(family),
                ContinuousOwnerView::Residual { .. } => None,
            })
            .expect("array definition remains one structured owner");
        assert_eq!(structured.scalar_rows(), 3);
        assert_eq!(
            structured.provenance().origin(),
            DaeProvenanceOrigin::Generated(DaeGeneration::AlgorithmEquation)
        );
    });

    let wire = serde_json::to_string(&compiled.dae)
        .expect("compact array reduction should serialize through wire-v11");
    let decoded: rumoca_compile::compile::Dae =
        serde_json::from_str(&wire).expect("wire-v11 should reconstruct both checked owners");
    let simulation = simulate_dae(&decoded, &SimOptions::default())
        .expect("array reduction should lower to computable Solve IR");
    for (name, expected) in [("x[1]", 2.0), ("x[2]", 4.0), ("x[3]", 6.0), ("total", 12.0)] {
        let variable = simulation
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap_or_else(|| panic!("{name} is visible"));
        let value = simulation.data[variable]
            .last()
            .copied()
            .expect("reduction trace is non-empty");
        assert!((value - expected).abs() <= 1.0e-10, "{name} = {value}");
    }
}
