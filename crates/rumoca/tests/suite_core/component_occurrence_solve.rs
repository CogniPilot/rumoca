//! Repeated component equations reach Solve IR with their component occurrence:
//! the exact refresh programs that native assignment emission compiles, and the
//! explicit derivative programs, each name the class equation and the instance
//! they were lowered from (SPEC_0040 FLAT-C08).

use rumoca::Compiler;
use rumoca_core::{ClassEquation, ComponentOccurrence};
use rumoca_ir_solve as solve;
use rumoca_sim::SimOptions;
use std::collections::{BTreeMap, BTreeSet};

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
