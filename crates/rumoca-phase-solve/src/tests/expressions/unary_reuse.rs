//! Repeated elementary geometry must remain shared through numerical AD.

use super::*;
use rumoca_ir_solve::{BinaryOp, ScalarProgramBlock, UnaryOp};

fn trig_count(rows: &ScalarProgramBlock) -> usize {
    rows.programs()
        .iter()
        .flatten()
        .filter(|op| {
            matches!(
                op,
                LinearOp::Unary {
                    op: UnaryOp::Sin | UnaryOp::Cos,
                    ..
                }
            )
        })
        .count()
}

fn derivative_model() -> dae::Dae {
    let source = TestSource::new("Real x; der(x) = sin(x) + cos(x) + sin(x) + cos(x);");
    let declaration = source.at(0, 6);
    let owner = source.at(8, 48);
    dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let state = model.variables(|variables| {
            variables.state(
                VarName::new("x"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let residual = model.expressions(|expressions| {
            let derivative = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Derivative(state))?;
            let value = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::State(state))?;
            let mut sum = expressions
                .at(owner)
                .builtin(dae::PureBuiltin::Sin, [value])?;
            for builtin in [
                dae::PureBuiltin::Cos,
                dae::PureBuiltin::Sin,
                dae::PureBuiltin::Cos,
            ] {
                // Distinct expression occurrences with the identical typed operand.
                let term = expressions.at(owner).builtin(builtin, [value])?;
                sum = expressions
                    .at(owner)
                    .binary(dae::BinaryOperator::Add, sum, term)?;
            }
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, derivative, sum)
        })?;
        model.continuous(|continuous| continuous.value_equation(owner, residual))
    })
    .unwrap()
}

#[test]
fn lowering_reuses_elementary_values_without_changing_arithmetic_order() {
    let model = derivative_model();
    let lowered = lower_solve_problem(&model).unwrap();
    let [ComputeNode::ScalarPrograms(rows)] = lowered.continuous.derivative_rhs.nodes.as_slice()
    else {
        panic!("scalar derivative program expected");
    };
    for x in [-2.0_f64, -0.0, 0.0, 0.7, 3.0] {
        let actual = eval_residual_rows(rows, &[x], &[])[0];
        assert_eq!(
            actual.to_bits(),
            (((x.sin() + x.cos()) + x.sin()) + x.cos()).to_bits()
        );
    }
    assert_eq!(
        trig_count(rows),
        2,
        "each elementary value is constructed once"
    );
}

#[test]
fn numerical_ad_reuses_primal_geometry_for_sine_and_cosine() {
    let source = TestSource::new("der(x) = sin(x) + cos(x);");
    let provenance = source
        .at(0, 24)
        .span()
        .require_provenance("AD geometry fixture")
        .unwrap();
    let primal = vec![vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::Unary {
            dst: 1,
            op: UnaryOp::Sin,
            arg: 0,
        },
        LinearOp::Unary {
            dst: 2,
            op: UnaryOp::Cos,
            arg: 0,
        },
        LinearOp::Binary {
            dst: 3,
            op: BinaryOp::Add,
            lhs: 1,
            rhs: 2,
        },
        LinearOp::StoreOutput { src: 3 },
    ]];
    let derived = ScalarProgramBlock::with_source_span(
        crate::lower_scalar_program_block_ad(&primal).unwrap(),
        provenance,
    )
    .unwrap();
    for (x, seed) in [(-2.0_f64, 0.5), (-0.0, -1.0), (0.0, 0.0), (0.7, 3.0)] {
        let mut actual = [0.0];
        rumoca_eval_solve::eval_scalar_program_block_with_context(
            &derived,
            &[x],
            &[],
            0.0,
            rumoca_eval_solve::RowEvalContext {
                seed: Some(&[seed]),
                ..Default::default()
            },
            &mut actual,
        )
        .unwrap();
        assert_eq!(
            actual[0].to_bits(),
            ((seed * x.cos()) + (seed * -x.sin())).to_bits()
        );
    }
    assert_eq!(trig_count(&derived), 2, "AD retains one sine/cosine pair");
}
