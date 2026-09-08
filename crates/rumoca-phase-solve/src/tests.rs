//! Solve lowering tests, grouped by the part of the Solve problem they fix.
//!
//! This module owns only the source fixture every group shares; each submodule
//! states one lowering responsibility and builds its own checked DAE models.

use rumoca_core::{SourceMap, Span, StructuredIndexBinder, StructuredIndexDomain, TypeId, VarName};
use rumoca_ir_dae as dae;
use rumoca_ir_solve::{self as solve, ComputeNode, LinearOp, ScalarSlot};

use crate::{LowerError, lower_solve_package, lower_solve_problem};

mod affine_derivatives;
mod clocks;
mod continuous;
mod discrete;
mod events;
mod expressions;
mod fmi;
mod initial_discrete_values;
mod initialization;
mod sampling;
mod temporal;

struct TestSource {
    map: SourceMap,
    source: rumoca_core::SourceId,
}

impl TestSource {
    fn new(text: &str) -> Self {
        let mut map = SourceMap::new();
        let source = map.add("solve.mo", text);
        Self { map, source }
    }

    fn at(&self, start: usize, end: usize) -> dae::DaeProvenance {
        dae::DaeProvenance::source(Span::from_offsets(self.source, start, end)).unwrap()
    }
}

/// State declaration attributes for fixtures whose initialization is not the
/// behavior under test.
///
/// Supplying both attributes is semantically significant: MLS §8.6 turns the
/// exact start into the state's initialization equation only when `fixed` is
/// true; with `fixed` false the same start remains a numerical guess and the
/// fixture must provide a distinct initialization equation.
fn real_state_attributes<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    declaration: dae::DaeProvenance,
    start_value: f64,
    fixed: bool,
) -> Result<dae::VariableAttributes<'dae>, dae::DaeConstructionError> {
    let start = model.expressions(|expressions| {
        expressions
            .at(declaration)
            .literal(dae::DaeLiteral::Real(start_value))
    })?;
    Ok(dae::VariableAttributes {
        component_ref: None,
        binding: None,
        start: Some(start),
        fixed: Some(rumoca_core::Fixity::from(fixed)),
        min: None,
        max: None,
        nominal: None,
        unit: None,
        state_select: rumoca_core::StateSelect::Default,
        description: None,
        causality: dae::VariableCausality::Local,
        is_tunable: false,
        is_held: false,
        origin: dae::VariableOrigin::Source,
    })
}

/// Re-seal a lowered root by naming every one of its seven child aggregates at
/// [`solve::SolveProblem::construct`], then require the result to reproduce
/// the lowered root exactly.
///
/// The checked constructor is the sole way to obtain a `SolveProblem`, so a
/// fixture states the shape contract on a lowered root by handing that root's
/// own children back to the constructor. The equality check additionally
/// fixes that the seven getters expose the whole root: a sealed field the
/// children do not carry would make the re-sealed root differ.
fn reseal_solve_problem(
    problem: &solve::SolveProblem,
) -> Result<solve::SolveProblem, Box<solve::SolveProblemShapeContractError>> {
    let resealed = solve::SolveProblem::construct(
        problem.layout().clone(),
        problem.solve_layout().clone(),
        problem.continuous().clone(),
        problem.initialization().clone(),
        problem.discrete().clone(),
        problem.events().clone(),
        problem.clocks().clone(),
    )
    .map_err(Box::new)?;
    assert_eq!(
        serde_json::to_value(&resealed).expect("a re-sealed Solve root serializes"),
        serde_json::to_value(problem).expect("a lowered Solve root serializes"),
        "re-sealing a lowered root from its seven checked children reproduces it exactly"
    );
    Ok(resealed)
}
