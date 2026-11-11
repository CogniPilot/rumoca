//! Repeated component equations reach Solve IR with their component occurrence:
//! the exact refresh programs that native assignment emission compiles, and the
//! explicit derivative programs, each name the class equation and the instance
//! they were lowered from (SPEC_0040 FLAT-C08).

use rumoca::Compiler;
use rumoca_core::{ClassEquation, ComponentOccurrence};
use rumoca_ir_solve as solve;
use rumoca_sim::SimOptions;
use std::collections::{BTreeMap, BTreeSet};

#[test]
fn root_equations_keep_the_pre_component_dae_wire() {
    let cases = [
        (
            "model Root Real x(start=1); initial equation x=1; equation der(x)=-x; end Root;",
            true,
        ),
        (
            "record Frame Real x; Real y; end Frame; model Root input Frame u; output Frame y; equation y=u; end Root;",
            false,
        ),
        (
            "function split input Real u; output Real a; output Real b; algorithm a:=u; b:=2*u; end split; model Root input Real u; Real a; Real b; equation (a,b)=split(u); end Root;",
            false,
        ),
        (
            "record Frame Real x; Real y; end Frame; model Root input Frame u; output Frame y; initial equation y=u; equation der(y.x)=-y.x; der(y.y)=-y.y; end Root;",
            true,
        ),
        (
            "function split input Real u; output Real a; output Real b; algorithm a:=u; b:=2*u; end split; model Root input Real u; Real a; Real b; initial equation (a,b)=split(u); equation der(a)=-a; der(b)=-b; end Root;",
            true,
        ),
    ];
    for (source, initial) in cases {
        let compiled = Compiler::new()
            .model("Root")
            .compile_str(source, "root_occurrence.mo")
            .unwrap();
        compiled.dae.inspect(|view| {
            assert!(view.continuous_equation_count() > 0);
            assert_eq!(view.initialization_equation_count() > 0, initial);
            for index in 0..view.continuous_equation_count() {
                assert_eq!(view.continuous_equation(index).unwrap().occurrence(), None);
            }
            for index in 0..view.initialization_equation_count() {
                assert_eq!(
                    view.initialization_equation(index).unwrap().occurrence(),
                    None
                );
            }
        });
        assert!(
            !serde_json::to_string(&compiled.dae)
                .unwrap()
                .contains("\"occurrence\":")
        );
    }
}

fn lower(source: &str, model: &str) -> solve::SolveModel {
    let compiled = match Compiler::new()
        .model(model)
        .compile_str(source, &format!("{model}.mo"))
    {
        Ok(compiled) => compiled,
        Err(error) => panic!("compile {model}: {error:#}"),
    };
    match rumoca_sim::lower_dae_for_simulation(&compiled.dae, &SimOptions::default()) {
        Ok(model) => model,
        Err(error) => panic!("lower {model}: {error:#}"),
    }
}

/// Instances seen per class equation; each class equation of a component
/// instantiated twice must be seen with two distinct instances.
fn instances_by_class_equation(
    occurrences: impl IntoIterator<Item = ComponentOccurrence>,
) -> BTreeMap<String, BTreeSet<u32>> {
    let mut seen = BTreeMap::<String, BTreeSet<u32>>::new();
    for occurrence in occurrences {
        let ClassEquation { scope, span } = occurrence.class_equation;
        seen.entry(format!("{scope:?}@{span:?}"))
            .or_default()
            .insert(occurrence.instance.0);
    }
    seen
}

/// Occurrences of every exact refresh program of `model`.
fn exact_refresh_occurrences(model: &solve::SolveModel) -> Vec<ComponentOccurrence> {
    let owners = &model.problem.continuous.refresh_owners;
    owners
        .exact_assignment_schedules()
        .iter()
        .flat_map(|schedule| schedule.program_ids().iter().copied())
        .filter_map(|id| owners.exact_assignment_program(id))
        .filter_map(solve::ExactRefreshAssignmentProgram::source_occurrence)
        .collect()
}

/// A component instantiated twice whose equations take the given DAE route.
fn assert_each_class_equation_twice(source: &str, model: &str) {
    let seen = instances_by_class_equation(exact_refresh_occurrences(&lower(source, model)));
    assert!(
        !seen.is_empty(),
        "{model}: exact refresh programs carry occurrences"
    );
    assert!(
        seen.values().all(|instances| instances.len() == 2),
        "{model}: each component class equation is seen once per instance: {seen:?}"
    );
}

#[test]
fn occurrences_survive_the_state_selection_rewrites() {
    // An alias equation and an evaluated parameter make state selection
    // rebuild the DAE (alias quotient, parameter folding) before lowering.
    assert_each_class_equation_twice(
        "
model Gain
  input Real u;
  parameter Real k = 2 annotation(Evaluate = true);
  Real w;
  Real y;
  Real z;
equation
  w = k * u;
  y = w + 1;
  z = y;
end Gain;
model Gains
  Real m(start = 1, fixed = true);
  Gain a(u = m);
  Gain b(u = 2 * m);
equation
  der(m) = -0.1 * (a.z + b.z);
end Gains;
",
        "Gains",
    );
}

#[test]
fn record_equations_keep_the_occurrence_on_every_field_row() {
    assert_each_class_equation_twice(
        "
record Frame
  Real x;
  Real y;
end Frame;
model Link
  input Real u;
  Frame f;
equation
  f = Frame(x = u, y = 2 * u);
end Link;
model Links
  Real m(start = 1, fixed = true);
  Link a(u = m);
  Link b(u = 2 * m);
equation
  der(m) = -0.1 * (a.f.x + b.f.y);
end Links;
",
        "Links",
    );
}

#[test]
fn tuple_call_equations_keep_the_occurrence_on_every_receiver() {
    assert_each_class_equation_twice(
        "
function twice
  input Real a;
  output Real x;
  output Real y;
algorithm
  x := a;
  y := 2 * a;
end twice;
model Split
  input Real u;
  Real p;
  Real q;
equation
  (p, q) = twice(u);
end Split;
model Splits
  Real m(start = 1, fixed = true);
  Split a(u = m);
  Split b(u = 2 * m);
equation
  der(m) = -0.1 * (a.p + b.q);
end Splits;
",
        "Splits",
    );
}

/// An array equation inside a component lowers as a structured DAE family.
/// Families carry no occurrence yet, so its exact programs name none: this
/// pins the documented exclusion until families are tagged.
#[test]
fn array_equations_lower_as_untagged_structured_families() {
    const ARMS: &str = "
model Arm
  input Real u;
  Real v[3];
equation
  v = {u, 2 * u, 3 * u};
end Arm;
model Arms
  Real m(start = 1, fixed = true);
  Arm a(u = m);
  Arm b(u = 2 * m);
equation
  der(m) = -0.1 * (a.v[1] + b.v[3]);
end Arms;
";
    let compiled = match Compiler::new().model("Arms").compile_str(ARMS, "Arms.mo") {
        Ok(compiled) => compiled,
        Err(error) => panic!("compile Arms: {error:#}"),
    };
    let families = compiled.dae.inspect(|view| {
        view.continuous_owners()
            .filter(|owner| matches!(owner, rumoca_ir_dae::ContinuousOwnerView::Structured { .. }))
            .count()
    });
    assert!(
        families >= 2,
        "each Arm's array equation is a structured family: {families}"
    );
    let tagged = instances_by_class_equation(exact_refresh_occurrences(&lower(ARMS, "Arms")));
    assert!(
        tagged.is_empty(),
        "structured families carry no occurrence yet: {tagged:?}"
    );
}

#[test]
fn exact_refresh_programs_name_the_component_class_equation_and_instance() {
    const PAIR: &str = "
model Unit
  input Real u;
  Real w;
  Real y;
equation
  w = 2 * u;
  y = w + 1;
end Unit;
model Pair
  Real m(start = 1, fixed = true);
  Unit a(u = m);
  Unit b(u = 2 * m);
equation
  der(m) = -0.1 * (a.y + b.y);
end Pair;
";
    let model = lower(PAIR, "Pair");
    let owners = &model.problem.continuous.refresh_owners;
    let occurrences = owners
        .exact_assignment_schedules()
        .iter()
        .flat_map(|schedule| schedule.program_ids().iter().copied())
        .filter_map(|id| owners.exact_assignment_program(id))
        .filter_map(solve::ExactRefreshAssignmentProgram::source_occurrence)
        .collect::<Vec<_>>();
    let seen = instances_by_class_equation(occurrences);
    assert!(
        !seen.is_empty(),
        "exact refresh programs carry component occurrences"
    );
    assert!(
        seen.values().all(|instances| instances.len() == 2),
        "each Unit class equation is seen once per instance: {seen:?}"
    );
}

#[test]
fn explicit_derivative_programs_name_the_component_class_equation_and_instance() {
    const TWO: &str = "
model Tank
  input Real q;
  Real h(start = 1, fixed = true);
equation
  der(h) = q - h;
end Tank;
model Two
  Tank t1(q = 1);
  Tank t2(q = 2);
end Two;
";
    let model = lower(TWO, "Two");
    let occurrences = model
        .problem
        .continuous
        .derivative_rhs
        .nodes
        .iter()
        .filter_map(|node| match node {
            solve::ComputeNode::ScalarPrograms(block) => Some(block),
            _ => None,
        })
        .flat_map(|block| {
            (0..block.programs().len()).filter_map(|row| block.program_occurrence(row))
        })
        .collect::<Vec<_>>();
    let seen = instances_by_class_equation(occurrences);
    assert_eq!(
        seen.len(),
        1,
        "both derivative rows come from the one Tank equation: {seen:?}"
    );
    assert!(
        seen.values().all(|instances| instances.len() == 2),
        "{seen:?}"
    );
}
