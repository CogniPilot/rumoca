//! The three incidence zero-proof scenarios through the shared typed consumer.

use super::zero_products::{Coefficient, expression_call};
use super::*;

fn fixture(kind: Coefficient, guarded: bool) -> dae::Dae {
    let mut sources = SourceMap::new();
    let file = sources.add("tensor_coefficient.mo", "{gain,p;p,gain} * x");
    let at = dae::DaeProvenance::source(Span::from_offsets(file, 0, 19)).unwrap();
    dae::Dae::construct(sources, |model| {
        let (real, vector) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at)?,
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [2]), at)?,
            ))
        })?;
        let gain = model.variables(|variables| {
            variables.parameter(
                VarName::new("gain"),
                real,
                at,
                dae::VariableAttributes {
                    is_tunable: true,
                    ..Default::default()
                },
            )
        })?;
        let zero = model.expressions(|e| e.at(at).literal(dae::DaeLiteral::Real(0.0)))?;
        let binding = if matches!(kind, Coefficient::TunableParent) {
            let parent = model.variables(|variables| {
                variables.parameter(
                    VarName::new("parent"),
                    real,
                    at,
                    dae::VariableAttributes {
                        binding: Some(zero),
                        is_tunable: true,
                        ..Default::default()
                    },
                )
            })?;
            model.expressions(|e| e.at(at).coordinate(dae::CoordinateInput::Parameter(parent)))?
        } else {
            zero
        };
        let p = model.variables(|variables| {
            variables.parameter(
                VarName::new("p"),
                real,
                at,
                dae::VariableAttributes {
                    binding: Some(binding),
                    is_tunable: matches!(kind, Coefficient::Tunable),
                    fixed: Some(vec![!matches!(kind, Coefficient::Initialization)]),
                    ..Default::default()
                },
            )
        })?;
        let x = model.variables(|variables| {
            variables.algebraic(VarName::new("x"), vector, at, Default::default())
        })?;
        model.expressions(|e| {
            let gain = e.at(at).coordinate(dae::CoordinateInput::Parameter(gain))?;
            let p = e.at(at).coordinate(dae::CoordinateInput::Parameter(p))?;
            let first = e.at(at).array([gain, p])?;
            let second = e.at(at).array([p, gain])?;
            let matrix = e.at(at).array([first, second])?;
            let mut x = e.at(at).coordinate(dae::CoordinateInput::Algebraic(x))?;
            if guarded {
                x = e.at(at).builtin(dae::PureBuiltin::Sqrt, [x])?;
            }
            e.at(at).binary(dae::BinaryOperator::Multiply, matrix, x)?;
            Ok(())
        })
    })
    .unwrap()
}

fn run(model: &dae::Dae, gain: f64, p: f64, x: [f64; 2]) -> Vec<solve::SolveValueKind> {
    let table = expression_call(model, false);
    let owner = &table.owners().last().unwrap();
    assert_eq!(owner.inputs().len(), 3);
    let inputs = [vec![gain], vec![p], x.to_vec()]
        .into_iter()
        .zip(owner.inputs())
        .map(|(values, ty)| {
            rumoca_eval_solve::TypedValue::construct(
                ty.clone(),
                values
                    .into_iter()
                    .map(|v| solve::SolveValueKind::Real64(v.to_bits()))
                    .collect(),
            )
            .unwrap()
        })
        .collect::<Vec<_>>();
    rumoca_eval_solve::eval_pure_call_checked(&table, owner.id(), &inputs).unwrap()[0]
        .elements()
        .to_vec()
}

#[test]
fn fixed_tensor_zeros_omit_only_their_lanes_and_keep_runtime_gain() {
    let model = fixture(Coefficient::Fixed(0.0), false);
    for gain in [2.0, 4.0] {
        let output = run(&model, gain, 0.0, [f64::NAN, 3.0]);
        assert!(
            matches!(output[0],solve::SolveValueKind::Real64(bits) if f64::from_bits(bits).is_nan())
        );
        assert_eq!(
            output[1],
            solve::SolveValueKind::Real64((gain * 3.0).to_bits())
        );
    }
}

#[test]
fn tensor_default_parent_and_initialization_zero_stay_runtime_values() {
    for kind in [
        Coefficient::Tunable,
        Coefficient::TunableParent,
        Coefficient::Initialization,
    ] {
        let model = fixture(kind, false);
        assert_eq!(
            run(&model, 2.0, 0.5, [1.0, 2.0]),
            [
                solve::SolveValueKind::Real64(3.0f64.to_bits()),
                solve::SolveValueKind::Real64(4.5f64.to_bits())
            ]
        );
        assert!(run(&model, 2.0, 0.0, [f64::NAN, 3.0]).iter().all(
            |v| matches!(v,solve::SolveValueKind::Real64(bits) if f64::from_bits(*bits).is_nan())
        ));
    }
}

#[test]
fn tensor_zero_proof_keeps_the_domain_factor_evaluation() {
    let model = fixture(Coefficient::Fixed(0.0), true);
    assert!(run(&model, 2.0, 0.0, [-1.0, 9.0]).iter().all(
        |v| matches!(v,solve::SolveValueKind::Real64(bits) if f64::from_bits(*bits).is_nan())
    ));
}
