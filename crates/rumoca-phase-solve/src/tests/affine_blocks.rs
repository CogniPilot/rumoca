//! Coupled affine derivative blocks: several scalar rows that define their
//! state derivatives together, including rows that reach the unknowns through a
//! function call.

use super::*;

const TEXT: &str = "Real x; Real y; Real z; Boolean q=true; der(x)+der(y)=1; second; z=der(x);";

fn at(source: &TestSource, needle: &str) -> dae::DaeProvenance {
    let start = TEXT.find(needle).unwrap();
    source.at(start, start + needle.len())
}

/// `f(flag, a)`: a result that adds `a` to a local only some paths assign, read
/// through the ordered assigned-state check.
fn partial_local_function<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    real: dae::ValueTypeId<'dae>,
    boolean: dae::ValueTypeId<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    let signature = dae::FunctionSignature::new(VarName::new("f"), [boolean, real], [real], at);
    let (function, ()) = model.function(signature, |model, reservation| {
        let (flag, a, y, spare) = model.functions(|functions| {
            Ok((
                functions.parameter(&reservation, VarName::new("flag"), 0, at)?,
                functions.parameter(&reservation, VarName::new("a"), 1, at)?,
                functions.output(&reservation, VarName::new("y"), 0, at)?,
                functions.local(&reservation, VarName::new("spare"), real, at)?,
            ))
        })?;
        let (condition, a, one, two) = model.expressions(|expressions| {
            Ok((
                expressions.at(at).function_parameter(flag)?,
                expressions.at(at).function_parameter(a)?,
                expressions.at(at).literal(dae::DaeLiteral::Real(1.0))?,
                expressions.at(at).literal(dae::DaeLiteral::Real(2.0))?,
            ))
        })?;
        let parent = model.functions(|functions| functions.begin(reservation, at))?;
        let (mut child, finish) =
            model.functions(|functions| functions.begin_branch(parent, at))?;
        model.functions(|functions| {
            functions.assign_all(&mut child, &[(y, one), (spare, one)], at)
        })?;
        let (parent, branch) =
            model.functions(|functions| functions.finish_branch(finish, child))?;
        let (mut child, finish) =
            model.functions(|functions| functions.begin_branch(parent, at))?;
        model.functions(|functions| functions.assign(&mut child, y, two, at))?;
        let (mut parent, fallback) =
            model.functions(|functions| functions.finish_branch(finish, child))?;
        model.functions(|functions| {
            functions.finish_conditional(&mut parent, vec![(condition, branch)], fallback, at)
        })?;
        model.functions(|functions| {
            functions
                .require_assigned(&mut parent, spare, at)
                .map(|_| ())
        })?;
        let read = model.functions(|functions| functions.read(&parent, spare, at))?;
        let sum = model.expressions(|expressions| {
            expressions.at(at).binary(dae::BinaryOperator::Add, read, a)
        })?;
        model.functions(|functions| functions.assign(&mut parent, y, sum, at))?;
        model.functions(|functions| functions.define(parent, at))
    })?;
    Ok(function)
}

/// Rows `dx + dy = 1`, `f(q, dx) - dy = 0` (that is `dx + 1 - dy = 0`) and `z = dx`.
fn coupled_model() -> dae::Dae {
    let source = TestSource::new(TEXT);
    let (x_at, y_at, z_at, q_at) = (
        at(&source, "Real x;"),
        at(&source, "Real y;"),
        at(&source, "Real z;"),
        at(&source, "Boolean q=true;"),
    );
    let (first_at, second_at, third_at) = (
        at(&source, "der(x)+der(y)=1;"),
        at(&source, "second;"),
        at(&source, "z=der(x);"),
    );
    dae::Dae::construct(source.map, |model| {
        let (real, boolean) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::scalar(dae::ScalarType::Real), x_at)?,
                types.derived(dae::ValueType::scalar(dae::ScalarType::Boolean), q_at)?,
            ))
        })?;
        let function = partial_local_function(model, real, boolean, x_at)?;
        let truth = model.expressions(|expressions| {
            expressions.at(q_at).literal(dae::DaeLiteral::Boolean(true))
        })?;
        let (x, y, z, q) = model.variables(|variables| {
            let attributes = dae::VariableAttributes::default;
            Ok((
                variables.state(VarName::new("x"), real, x_at, attributes())?,
                variables.state(VarName::new("y"), real, y_at, attributes())?,
                variables.algebraic(VarName::new("z"), real, z_at, attributes())?,
                variables.parameter(
                    VarName::new("q"),
                    boolean,
                    q_at,
                    dae::VariableAttributes {
                        binding: Some(truth),
                        ..attributes()
                    },
                )?,
            ))
        })?;
        let (first, second, third) = model.expressions(|expressions| {
            let one = expressions
                .at(first_at)
                .literal(dae::DaeLiteral::Real(1.0))?;
            let (dx, dy) = (
                expressions
                    .at(first_at)
                    .coordinate(dae::CoordinateInput::Derivative(x))?,
                expressions
                    .at(first_at)
                    .coordinate(dae::CoordinateInput::Derivative(y))?,
            );
            let sum = expressions
                .at(first_at)
                .binary(dae::BinaryOperator::Add, dx, dy)?;
            let first = expressions
                .at(first_at)
                .binary(dae::BinaryOperator::Subtract, sum, one)?;
            let (dx, dy) = (
                expressions
                    .at(second_at)
                    .coordinate(dae::CoordinateInput::Derivative(x))?,
                expressions
                    .at(second_at)
                    .coordinate(dae::CoordinateInput::Derivative(y))?,
            );
            let lhs = {
                let flag = expressions
                    .at(second_at)
                    .coordinate(dae::CoordinateInput::Parameter(q))?;
                expressions.at(second_at).call(function, 0, [flag, dx])?
            };
            let second =
                expressions
                    .at(second_at)
                    .binary(dae::BinaryOperator::Subtract, lhs, dy)?;
            let z_value = expressions
                .at(third_at)
                .coordinate(dae::CoordinateInput::Algebraic(z))?;
            let dx = expressions
                .at(third_at)
                .coordinate(dae::CoordinateInput::Derivative(x))?;
            let third =
                expressions
                    .at(third_at)
                    .binary(dae::BinaryOperator::Subtract, z_value, dx)?;
            Ok((first, second, third))
        })?;
        model.continuous(|continuous| {
            continuous.value_equation(first_at, first)?;
            continuous.value_equation(second_at, second)?;
            continuous.value_equation(third_at, third)
        })
    })
    .unwrap()
}

fn check_coupled_solve(model: &dae::Dae) {
    let solve = lower_solve_problem(model).unwrap();
    let linear_solves = solve
        .continuous
        .derivative_rhs
        .nodes
        .iter()
        .filter(|node| matches!(node, ComputeNode::LinSolve { n: 2, .. }))
        .count();
    assert_eq!(linear_solves, 1, "both derivatives are one coupled system");
    let reads_component = |block: &rumoca_ir_solve::ComputeBlock| {
        block.nodes.iter().any(|node| match node {
            ComputeNode::ScalarPrograms(rows) => rows.programs().iter().flatten().any(|op| {
                matches!(
                    op,
                    LinearOp::LinearSolveComponent {
                        n: 2,
                        component: 0,
                        ..
                    }
                )
            }),
            _ => false,
        })
    };
    assert!(
        reads_component(&solve.continuous.residual)
            || reads_component(&solve.continuous.derivative_rhs)
            || reads_component(&solve.continuous.implicit_rhs),
        "the algebraic row reads the first derivative from the shared solve"
    );
}

#[test]
fn a_call_over_a_derivative_stays_affine_when_its_checks_read_only_constants() {
    check_coupled_solve(&coupled_model());
}
