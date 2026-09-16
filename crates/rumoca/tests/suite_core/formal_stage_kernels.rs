//! Numerical stage analysis uses the same checked tensor evaluator and AD.

use rumoca_eval_solve::{TypedValue, eval_pure_call, eval_pure_call_directional};
use rumoca_ir_dae as dae;
use rumoca_ir_solve::{SolveValueKind, SolveValueType};
use rumoca_phase_solve::{FormalDerivativePrograms, lower_formal_derivative_stages};
use rumoca_phase_structural::{FormalDerivativeView, construct_formal_derivatives};

fn compile(source: &str, model: &str) -> std::sync::Arc<dae::Dae> {
    rumoca::Compiler::new()
        .model(model)
        .compile_str(source, "stage_kernels.mo")
        .unwrap()
        .dae
}

fn value(kind: &SolveValueType, values: Vec<f64>) -> TypedValue {
    TypedValue::construct(
        kind.clone(),
        values
            .into_iter()
            .map(|v| SolveValueKind::Real64(v.to_bits()))
            .collect(),
    )
    .unwrap()
}

fn reals(value: &TypedValue) -> Vec<f64> {
    value
        .elements()
        .iter()
        .map(|v| match v {
            SolveValueKind::Real64(v) => f64::from_bits(*v),
            _ => panic!("expected real value"),
        })
        .collect()
}

fn variable(coordinate: dae::CoordinateView<'_>) -> Option<u32> {
    match coordinate {
        dae::CoordinateView::Algebraic(v) => Some(v.index()),
        dae::CoordinateView::Parameter(v) => Some(v.index()),
        dae::CoordinateView::Input(v) => Some(v.index()),
        dae::CoordinateView::Time => None,
        other => panic!("unexpected coordinate {other:?}"),
    }
}

fn source_coordinate<'a>(
    formal: FormalDerivativeView<'_, 'a, '_>,
    id: u32,
) -> (dae::VariableId<'a>, usize) {
    formal
        .source
        .variables()
        .find_map(|(source, _)| {
            (0..=3).find_map(|order| {
                (formal.coordinate(source, order).map(|v| v.index()) == Some(id))
                    .then_some((source, order))
            })
        })
        .unwrap()
}

fn evaluate(
    programs: &FormalDerivativePrograms<'_, '_, '_>,
    point: impl Fn(Option<(&str, usize)>, usize) -> (f64, f64),
) -> (Vec<f64>, Vec<f64>, Vec<bool>) {
    let formal = programs.formal();
    let mut residuals = Vec::new();
    let mut tangents = Vec::new();
    let mut assertions = Vec::new();
    for equation in programs.stages().iter().flat_map(|stage| stage.equations()) {
        let mut args = Vec::new();
        let mut directions = Vec::new();
        for (&coordinate, kind) in equation.inputs().iter().zip(equation.site().inputs()) {
            let source = variable(coordinate).map(|id| source_coordinate(formal, id));
            let named = source
                .map(|(id, order)| (formal.source.variable(id).unwrap().name().as_str(), order));
            let (values, seeds): (Vec<_>, Vec<_>) = (0..kind.scalar_count() as usize)
                .map(|i| point(named, i))
                .unzip();
            let primal = value(kind, values);
            args.push(primal.clone());
            directions.extend([primal, value(kind, seeds)]);
        }
        let values = eval_pure_call(programs.table(), equation.site().owner(), &args).unwrap();
        let derivatives =
            eval_pure_call_directional(programs.table(), equation.site().owner(), &directions)
                .unwrap();
        let count = equation.residual_outputs();
        for (i, result) in values.iter().take(count).enumerate() {
            assert_eq!(*result, derivatives[2 * i]);
            residuals.extend(reals(result));
            tangents.extend(reals(&derivatives[2 * i + 1]));
        }
        for (i, result) in values.iter().skip(count).enumerate() {
            assert_eq!(*result, derivatives[2 * count + i]);
            let [SolveValueKind::Boolean(predicate)] = result.elements() else {
                panic!("assertion output");
            };
            assertions.push(*predicate);
        }
    }
    (residuals, tangents, assertions)
}

#[test]
fn tensor_stage_residual_and_ad_include_parameters_inputs_and_time() {
    let source = compile(
        "model Matrix parameter Real p=2; input Real u; Real x[2,2](each fixed=true); equation der(x)=p*x+fill(u+time,2,2); end Matrix;",
        "Matrix",
    );
    let formal = construct_formal_derivatives(&source).unwrap();
    formal.inspect(|formal| {
        let kernels = lower_formal_derivative_stages(formal).unwrap();
        let (residuals, directions, assertions) = evaluate(&kernels, |source, i| match source {
            Some(("x", 0)) => ((i + 1) as f64, 1.),
            Some(("x", 1)) => (10. + i as f64, 0.),
            Some(("p", 0)) => (2., 0.),
            Some(("u", 0)) => (3., 2.),
            None => (0.5, 3.),
            other => panic!("{other:?}"),
        });
        assert_eq!(residuals, [4.5, 3.5, 2.5, 1.5]);
        assert_eq!(directions, [-7.; 4]);
        assert!(assertions.is_empty());
        let equation = &kernels.stages()[1].equations()[0];
        assert_eq!(
            equation.site().outputs()[0].value_type().dimensions(),
            [2, 2]
        );
        let x = formal
            .source
            .variables()
            .find(|(_, v)| v.name().as_str() == "x")
            .unwrap()
            .0;
        assert_eq!(
            kernels
                .formal()
                .view
                .variable(formal.coordinate(x, 0).unwrap())
                .unwrap()
                .fixed(),
            Some(true)
        );
    });
}

#[test]
fn loop_stage_uses_a_compact_map_and_exact_binder_values() {
    let source = compile(
        "model Loop Real x[4]; equation for i in 1:4 loop der(x[i])=i*x[i]; end for; end Loop;",
        "Loop",
    );
    let formal = construct_formal_derivatives(&source).unwrap();
    formal.inspect(|formal| {
        let kernels = lower_formal_derivative_stages(formal).unwrap();
        let (residuals, directions, _) = evaluate(&kernels, |source, i| match source {
            Some(("x", 0)) => ((i + 1) as f64, 1.),
            Some(("x", 1)) => (10., 0.),
            other => panic!("{other:?}"),
        });
        assert_eq!(residuals, [9., 6., 1., -6.]);
        assert_eq!(directions, [-1., -2., -3., -4.]);
        let equation = &kernels.stages()[1].equations()[0];
        let body = kernels
            .table()
            .owner(equation.site().owner())
            .unwrap()
            .body();
        assert!(
            body.operations()
                .iter()
                .any(|i| matches!(i.operation(), rumoca_ir_solve::SolveOperation::Map { .. }))
        );
    });
}

#[test]
fn call_assertions_survive_stage_evaluation_and_ad() {
    let source = compile(
        "function square input Real u; output Real y; algorithm assert(u>0, \"positive\"); y:=u*u; end square; model Function Real x; equation der(x)=square(x); end Function;",
        "Function",
    );
    let formal = construct_formal_derivatives(&source).unwrap();
    formal.inspect(|formal| {
        let kernels = lower_formal_derivative_stages(formal).unwrap();
        for x in [2., -2.] {
            let (residuals, directions, assertions) =
                evaluate(&kernels, |source, _| match source {
                    Some(("x", 0)) => (x, 1.),
                    Some(("x", 1)) => (5., 3.),
                    other => panic!("{other:?}"),
                });
            assert_eq!(residuals, [1.]);
            assert_eq!(directions, [3. - 2. * x]);
            assert_eq!(assertions, [x > 0.]);
        }
        let assertion = &kernels.stages()[1].equations()[0].assertions()[0];
        assert!(formal.view.expression(assertion.message()).is_some());
        assert_ne!(assertion.provenance().span(), rumoca_core::Span::DUMMY);
    });
}

#[test]
fn rotation_stage_kernels_lower_every_coupled_equation() {
    let source = compile(
        include_str!("../fixtures/index_reduction/RateCancellation.mo"),
        "RateCancellation",
    );
    let formal = construct_formal_derivatives(&source).unwrap();
    formal.inspect(|formal| {
        let kernels = lower_formal_derivative_stages(formal).unwrap();
        assert_eq!(
            kernels
                .stages()
                .iter()
                .map(|s| s
                    .equations()
                    .iter()
                    .map(|e| e
                        .site()
                        .outputs()
                        .iter()
                        .take(e.residual_outputs())
                        .map(|o| o.value_type().scalar_count() as usize)
                        .sum::<usize>())
                    .sum::<usize>())
                .collect::<Vec<_>>(),
            [38, 44, 47]
        );
        assert!(
            kernels
                .stages()
                .iter()
                .flat_map(|s| s.equations())
                .all(|e| e.site().directional().is_some())
        );
    });
}

#[test]
fn loop_tensor_bodies_preserve_binder_prefix_and_row_major_axes() {
    let source = compile(
        "model Rows Real x[2,2]; equation for i in 1:2 loop der(x[i,:])=-x[i,:]+{i,2*i}; end for; end Rows;",
        "Rows",
    );
    let formal = construct_formal_derivatives(&source).unwrap();
    formal.inspect(|formal| {
        let kernels = lower_formal_derivative_stages(formal).unwrap();
        let (residuals, directions, _) = evaluate(&kernels, |source, i| match source {
            Some(("x", 0)) => ((i + 1) as f64, 1.),
            Some(("x", 1)) => (10. + i as f64, 0.),
            other => panic!("{other:?}"),
        });
        assert_eq!(residuals, [10., 11., 13., 13.]);
        assert_eq!(directions, [1.; 4]);
    });
}

#[test]
fn map_scoped_assertions_are_refused_instead_of_discarded() {
    let source = compile(
        "function positive input Real u; output Real y; algorithm assert(u>0, \"positive\"); y:=u*u; end positive; model LoopAssertion Real x[3]; equation for i in 1:3 loop der(x[i])=positive(x[i]); end for; end LoopAssertion;",
        "LoopAssertion",
    );
    let formal = construct_formal_derivatives(&source).unwrap();
    formal.inspect(|formal| {
        let Err(rumoca_ir_solve::SolveProgramConstructionError::InvalidCallInterface {
            provenance,
        }) = lower_formal_derivative_stages(formal)
        else {
            panic!("map assertion execution needs a checked region owner");
        };
        assert_ne!(provenance, rumoca_core::Span::DUMMY);
    });
}

#[test]
fn vector_conversion_preserves_tensor_and_scalar_values_and_ad() {
    let source = compile(
        "model Dimensions Real x[1,3,1]; Real v[3],z,w[1],one[1,1],u[1]; equation der(x)=zeros(1,3,1); v=vector(x); z=v[1]; w=vector(z); one=fill(v[1],1,1); u=vector(one); end Dimensions;",
        "Dimensions",
    );
    let formal = construct_formal_derivatives(&source).unwrap();
    formal.inspect(|formal| {
        let kernels = lower_formal_derivative_stages(formal).unwrap();
        let (residuals, directions, _) = evaluate(&kernels, |source, i| match source {
            Some(("x", 0)) => ((i + 1) as f64, 1.),
            Some(("x", 1)) => (0., 0.),
            Some(("v", 0)) => ((i + 1) as f64, 2.),
            Some(("z" | "one", 0)) => (1., 3.),
            Some(("w" | "u", 0)) => (1., 4.),
            other => panic!("{other:?}"),
        });
        assert_eq!(residuals, [0.; 10]);
        assert_eq!(directions.iter().filter(|&&v| v == 0.).count(), 3);
        assert_eq!(directions.iter().filter(|&&v| v == 1.).count(), 7);
    });
}
