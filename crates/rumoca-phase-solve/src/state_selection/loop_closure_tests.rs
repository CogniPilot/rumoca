//! SPEC_0053 §1: a redundant loop closure decides reduction from the source.

use rumoca_core::{SourceMap, Span, VarName};

use super::*;
use rumoca_phase_structural::prepare_for_solve;

/// `der(x)=vx; der(y)=vy; der(vx)=-lambda*x; der(vy)=-lambda*y-g;` closed by
/// `x*x+y*y=1` (a loop closure defining no state) when `closed`, or by `x=y`
/// (a direct state definition) otherwise.
fn pendulum(closed: bool) -> dae::Dae {
    let text = "Real x, y, vx, vy, lambda; equation der(x)=vx; der(y)=vy; \
                der(vx)=-lambda*x; der(vy)=-lambda*y-9.81; x*x+y*y=1;";
    let mut map = SourceMap::new();
    let source = map.add("loop_closure.mo", text);
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
    dae::Dae::construct(map, |model| {
        let real = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let starts = model.expressions(|e| {
            Ok([1.0, 0.0, 0.0, 0.0, 0.0]
                .map(|value| e.at(at).literal(dae::DaeLiteral::Real(value))))
        })?;
        let starts = starts.into_iter().collect::<Result<Vec<_>, _>>()?;
        let attributes = |start| dae::VariableAttributes {
            start: Some(start),
            ..Default::default()
        };
        let [x, y, vx, vy] = model.variables(|v| {
            Ok([
                v.state(VarName::new("x"), real, at, attributes(starts[0]))?,
                v.state(VarName::new("y"), real, at, attributes(starts[1]))?,
                v.state(VarName::new("vx"), real, at, attributes(starts[2]))?,
                v.state(VarName::new("vy"), real, at, attributes(starts[3]))?,
            ])
        })?;
        let lambda = model
            .variables(|v| v.algebraic(VarName::new("lambda"), real, at, attributes(starts[4])))?;
        let rows = model.expressions(|e| {
            use dae::{BinaryOperator as B, CoordinateInput as C};
            let mut rows = Vec::new();
            for (derivative, rate) in [(x, vx), (y, vy)] {
                let lhs = e.at(at).coordinate(C::Derivative(derivative))?;
                let rhs = e.at(at).coordinate(C::State(rate))?;
                rows.push(e.at(at).binary(B::Subtract, lhs, rhs)?);
            }
            for (rate, position, offset) in [(vx, x, 0.0), (vy, y, 9.81)] {
                let lhs = e.at(at).coordinate(C::Derivative(rate))?;
                let force = e.at(at).coordinate(C::Algebraic(lambda))?;
                let position = e.at(at).coordinate(C::State(position))?;
                let product = e.at(at).binary(B::Multiply, force, position)?;
                let offset = e.at(at).literal(dae::DaeLiteral::Real(offset))?;
                let force = e.at(at).binary(B::Add, product, offset)?;
                rows.push(e.at(at).binary(B::Add, lhs, force)?);
            }
            let px = e.at(at).coordinate(C::State(x))?;
            let py = e.at(at).coordinate(C::State(y))?;
            let closure = if closed {
                let xx = e.at(at).binary(B::Multiply, px, px)?;
                let yy = e.at(at).binary(B::Multiply, py, py)?;
                let sum = e.at(at).binary(B::Add, xx, yy)?;
                let one = e.at(at).literal(dae::DaeLiteral::Real(1.0))?;
                e.at(at).binary(B::Subtract, sum, one)?
            } else {
                e.at(at).binary(B::Subtract, px, py)?
            };
            rows.push(closure);
            Ok(rows)
        })?;
        model.continuous(|continuous| {
            for row in rows {
                continuous.value_equation(at, row)?;
            }
            Ok(())
        })
    })
    .unwrap()
}

#[test]
fn only_a_closure_defining_no_state_is_a_redundant_loop_closure() {
    assert!(SourceStructuralAnalysis::of(&pendulum(true)).holds_redundant_loop_closure());
    assert!(!SourceStructuralAnalysis::of(&pendulum(false)).holds_redundant_loop_closure());
}

#[test]
fn a_redundant_loop_closure_reduces_to_the_basis_the_reducer_selects() {
    let model = pendulum(true);
    let overrides = HashMap::new();
    let prepared = prepare_for_solve(&model).expect("the reducer prepares the pendulum");
    assert!(
        prepared.manifold_requires_reduction(),
        "the reducer classifies the closure redundant"
    );
    let through_reducer = reduce_or_retain(&model, prepared, &overrides).unwrap();
    let decided = reduce_loop_closure(&SourceStructuralAnalysis::of(&model), &overrides)
        .unwrap()
        .expect("the closure decides reduction without the reducer");
    assert_eq!(
        decided.integrated_names().unwrap(),
        through_reducer.integrated_names().unwrap()
    );
    let direct = pendulum(false);
    assert!(
        reduce_loop_closure(&SourceStructuralAnalysis::of(&direct), &overrides)
            .unwrap()
            .is_none()
    );
}

#[test]
fn a_loop_closure_selection_names_each_generated_state_by_its_source() {
    // The reduce-from-source path issues the same state coordinate map as the
    // reducer path: every integrated scalar of the lowered Solve model names
    // the source scalar (and formal order) its projection equation equates it
    // to, and those names are the selection's integrated basis.
    let model = pendulum(true);
    let decided = reduce_loop_closure(&SourceStructuralAnalysis::of(&model), &HashMap::new())
        .unwrap()
        .expect("the closure decides reduction without the reducer");
    let mut basis = decided.integrated_names().unwrap();
    basis.sort();
    assert_eq!(basis.len(), 2, "a closed pendulum integrates two scalars");
    let lowered = crate::lower_solve_model(&model, &HashMap::new(), |_| {}).unwrap();
    let mut sources = lowered
        .model()
        .variable_meta
        .iter()
        .filter(|meta| meta.is_state)
        .map(|meta| {
            meta.state_coordinate
                .as_ref()
                .map(solve::SolveStateCoordinate::source_name)
                .unwrap_or_else(|| panic!("{} names no source", meta.name))
        })
        .collect::<Vec<_>>();
    sources.sort();
    assert_eq!(sources, basis);
    let declared = ["x", "y", "vx", "vy"];
    assert!(sources.iter().all(|name| {
        declared
            .iter()
            .any(|source| name == source || *name == format!("der({source})"))
    }));
}
