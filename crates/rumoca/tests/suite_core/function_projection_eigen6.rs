//! Full, unchanged 32-sweep Jacobi source: incidence and numerical liveness.

use std::collections::BTreeSet;

use rumoca::Compiler;
use rumoca_eval_dae::{ScalarCoordinateProjectionCache, for_each_scalar_coordinate_cached};
use rumoca_ir_dae as dae;
use rumoca_sim::{SimOptions, SimSolverMode, SimulationSession};

const SOURCE: &str = include_str!("../fixtures/symmetric_eigen6.mo");

fn input_dependencies<'dae>(
    view: dae::DaeView<'dae>,
    call: dae::ExprId<'dae>,
    scalar: usize,
    cache: &mut ScalarCoordinateProjectionCache<'dae>,
) -> BTreeSet<(u32, usize)> {
    let mut dependencies = BTreeSet::new();
    for_each_scalar_coordinate_cached(view, call, scalar, None, cache, |coordinate, index| {
        if let dae::CoordinateView::Input(variable) = coordinate {
            dependencies.insert((variable.index(), index));
        } else {
            panic!("a pure result must depend only on its actual matrix input");
        }
    })
    .expect("every complete output scalar has a checked dependency view");
    dependencies
}

#[test]
fn full_eigen6_projection_preserves_all_input_coordinates_and_compact_sweeps() {
    let compiled = Compiler::new()
        .model("RegistrationEigen6")
        .compile_str(SOURCE, "symmetric_eigen6.mo")
        .expect("the unchanged full Jacobi source compiles");
    eprintln!("full Eigen6 compiled; checking all 42 typed scalar projections");
    compiled.dae.inspect(|view| {
        assert!(
            (0..view.domain_count()).any(|index| {
                view.domain(view.domain_id(index).unwrap())
                    .unwrap()
                    .structured()
                    .binders
                    .iter()
                    .any(|binder| binder.lower == 1 && binder.upper == 32 && binder.step == 1)
            }),
            "the complete 32-sweep owner must remain present"
        );
        let calls = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .filter(|expression| {
                matches!(
                    view.expression(*expression).unwrap().operation(),
                    dae::ExpressionOperation::Call { .. }
                )
            })
            .collect::<Vec<_>>();
        assert_eq!(
            calls.len(),
            2,
            "both outputs retain their typed call projections"
        );
        let mut cache = ScalarCoordinateProjectionCache::default();
        let mut projected = 0;
        for call in calls {
            let count = view
                .expression(call)
                .unwrap()
                .value_type()
                .dimensions()
                .iter()
                .map(|extent| *extent as usize)
                .product::<usize>();
            for scalar in 0..count {
                let dependencies = input_dependencies(view, call, scalar, &mut cache);
                assert_eq!(
                    dependencies.len(),
                    36,
                    "norm and Jacobi controls read the complete matrix"
                );
                assert_eq!(
                    dependencies
                        .iter()
                        .map(|(_, index)| *index)
                        .collect::<Vec<_>>(),
                    (0..36).collect::<Vec<_>>()
                );
                projected += 1;
            }
        }
        assert_eq!(
            projected, 42,
            "all six eigenvalues and 36 vector entries are covered"
        );
    });
}

fn check_eigenpairs(a: &[[f64; 6]; 6], values: &[f64; 6], vectors: &[[f64; 6]; 6]) {
    assert!(values.iter().all(|value| value.is_finite()));
    assert!(values.windows(2).all(|pair| pair[0] <= pair[1]));
    let scale = a
        .iter()
        .flatten()
        .map(|value| value.abs())
        .fold(1.0, f64::max);
    for i in 0..6 {
        for j in 0..6 {
            let av = (0..6).map(|k| a[i][k] * vectors[k][j]).sum::<f64>();
            assert!((av - vectors[i][j] * values[j]).abs() < 2e-11 * scale);
            let gram = (0..6).map(|k| vectors[k][i] * vectors[k][j]).sum::<f64>();
            assert!((gram - if i == j { 1.0 } else { 0.0 }).abs() < 2e-12);
        }
    }
}

#[test]
fn full_eigen6_session_retains_correlated_singular_and_indefinite_eigenpairs() {
    let compiled = Compiler::new()
        .model("RegistrationEigen6")
        .compile_str(SOURCE, "symmetric_eigen6.mo")
        .unwrap();
    eprintln!("full Eigen6 compiled; preparing persistent native session");
    let options = SimOptions {
        solver_mode: SimSolverMode::RkLike,
        dt: Some(0.1),
        ..Default::default()
    };
    let mut session = SimulationSession::new(&compiled.dae, options)
        .expect("preparation of the complete model must terminate");
    eprintln!("full Eigen6 session prepared; checking unchanged matrix sequence");
    let diagonal = [6.0, 3.0, 1.0, 0.0, -2.0, 4.0];
    let diagonal =
        std::array::from_fn(|i| std::array::from_fn(|j| if i == j { diagonal[i] } else { 0.0 }));
    let dense = std::array::from_fn(|i| {
        std::array::from_fn(|j| {
            (0..6)
                .map(|k| ((i + 1) * (k + 2)) as f64 * 0.17)
                .map(f64::sin)
                .zip((0..6).map(|k| (((j + 1) * (k + 2)) as f64 * 0.17).sin()))
                .map(|(x, y)| x * y)
                .sum::<f64>()
        })
    });
    for (frame, a) in [diagonal, dense, [[0.0; 6]; 6], diagonal]
        .iter()
        .enumerate()
    {
        let inputs = (0..6)
            .flat_map(|i| {
                (0..6).map(move |j| (format!("information[{},{}]", i + 1, j + 1), a[i][j]))
            })
            .collect::<Vec<_>>();
        session
            .set_inputs(
                &inputs
                    .iter()
                    .map(|(name, value)| (name.as_str(), *value))
                    .collect::<Vec<_>>(),
            )
            .unwrap();
        session.advance_to((frame + 1) as f64 * 0.1).unwrap();
        let state = session.state().unwrap();
        let values = std::array::from_fn(|i| state.values[&format!("eigenvalues[{}]", i + 1)]);
        let vectors = std::array::from_fn(|i| {
            std::array::from_fn(|j| state.values[&format!("eigenvectors[{},{}]", i + 1, j + 1)])
        });
        check_eigenpairs(a, &values, &vectors);
        if frame == 0 || frame == 3 {
            assert_eq!(values, [-2.0, 0.0, 1.0, 3.0, 4.0, 6.0]);
        }
    }
}
