//! Shared expression diamonds remain shared inside a function's selected arm.

use super::*;
use rumoca_ir_solve::{LinearOpSliceKind, SolveVisitor};

fn diamond_expression<'dae>(
    expressions: &mut dae::Expressions<'_, 'dae>,
    input: dae::FunctionParameterId<'dae>,
    at: dae::DaeProvenance,
    depth: usize,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let input = expressions.at(at).function_parameter(input)?;
    let zero = expressions.at(at).literal(dae::DaeLiteral::Real(0.0))?;
    let one = expressions.at(at).literal(dae::DaeLiteral::Real(1.0))?;
    let array = expressions.at(at).array([input, zero])?;
    // Array maximum forces scalar AD expansion, as in the long SSA chain.
    let mut value = expressions.at(at).builtin(dae::PureBuiltin::Max, [array])?;
    for _ in 0..depth {
        let guard = expressions
            .at(at)
            .binary(dae::BinaryOperator::Greater, value, zero)?;
        let positive = expressions
            .at(at)
            .binary(dae::BinaryOperator::Add, value, one)?;
        let negative = expressions
            .at(at)
            .binary(dae::BinaryOperator::Subtract, value, one)?;
        value = expressions
            .at(at)
            .conditional([(guard, positive)], negative)?;
    }
    Ok(value)
}

fn diamond_model(depth: usize) -> dae::Dae {
    let source = TestSource::new("function f input Real x; output Real y; end f; Real z; z=f(z);");
    let at = source.at(0, 60);
    dae::Dae::construct(source.map, |model| {
        let real = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let signature = dae::FunctionSignature::new(VarName::new("f"), [real], [real], at);
        let (function, ()) = model.function(signature, |model, reservation| {
            let input = model.functions(|functions| {
                functions.parameter(&reservation, VarName::new("x"), 0, at)
            })?;
            let output = model
                .functions(|functions| functions.output(&reservation, VarName::new("y"), 0, at))?;
            let value = model
                .expressions(|expressions| diamond_expression(expressions, input, at, depth))?;
            let mut body = model.functions(|functions| functions.begin(reservation, at))?;
            model.functions(|functions| functions.assign(&mut body, output, value, at))?;
            model.functions(|functions| functions.define(body, at))
        })?;
        let variable = model.variables(|variables| {
            variables.algebraic(
                VarName::new("z"),
                real,
                at,
                dae::VariableAttributes::default(),
            )
        })?;
        let residual = model.expressions(|expressions| {
            let value = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(variable))?;
            let call = expressions.at(at).call(function, 0, [value])?;
            expressions
                .at(at)
                .binary(dae::BinaryOperator::Subtract, value, call)
        })?;
        model.continuous(|continuous| continuous.value_equation(at, residual))
    })
    .unwrap()
}

#[derive(Default)]
struct OperationCensus {
    total: usize,
    conditional: usize,
}

impl SolveVisitor for OperationCensus {
    type Error = std::convert::Infallible;

    fn visit_linear_op(
        &mut self,
        _: LinearOpSliceKind,
        _: usize,
        op: &LinearOp,
    ) -> Result<(), Self::Error> {
        self.total += 1;
        self.conditional += usize::from(matches!(
            op,
            LinearOp::FunctionConditional { .. } | LinearOp::Select { .. }
        ));
        Ok(())
    }
}

fn lowered_diamond_size(depth: usize) -> usize {
    let model = diamond_model(depth);
    let package = lower_solve_package(&model).unwrap();
    let mut census = OperationCensus::default();
    census.visit_solve_problem(&package.problem).unwrap();
    assert!(
        census.conditional > 0,
        "the scalar conditional path must be exercised"
    );
    let [ComputeNode::ScalarPrograms(rows)] = package.problem.continuous.residual.nodes.as_slice()
    else {
        panic!("one scalar residual block expected");
    };
    assert_eq!(
        eval_residual_rows_with_pure_calls(rows, &package.pure_calls, &[0.5], &[]),
        [-f64::from(u32::try_from(depth).unwrap())]
    );
    census.total
}

#[test]
fn shared_function_expression_diamonds_grow_with_the_dag() {
    let small = lowered_diamond_size(4);
    let large = lowered_diamond_size(8);
    eprintln!("shared conditional DAG operations: {small} -> {large}");
    assert!(
        large <= 3 * small,
        "doubling the shared DAG must remain linear: {small} -> {large}"
    );
    assert!(
        large < 512,
        "eight diamonds must not expand a branching tree: {large}"
    );
}
