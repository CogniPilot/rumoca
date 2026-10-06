//! A disabled optional body never changes a family's physical Flat row view.

use super::{SimOptions, compile, simulate_dae};

fn options() -> SimOptions {
    SimOptions {
        t_end: 0.1,
        dt: Some(0.01),
        rtol: 1.0e-11,
        atol: 1.0e-12,
        ..SimOptions::default()
    }
}

fn trajectory(source: &str, model: &str, values: &[(String, f64, f64)]) {
    let dae = compile(source, model);
    let result = simulate_dae(&dae, &options()).expect("stored array views simulate");
    assert_eq!(result.n_states, values.len());
    for (name, initial, rate) in values {
        let column = result
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap();
        for (time, actual) in result.times.iter().zip(&result.data[column]) {
            let expected = initial * (-rate * time).exp();
            assert!(
                (actual - expected).abs() < 1.0e-7,
                "{name} at {time}: {actual} != {expected}"
            );
        }
    }
}

#[test]
fn disabled_whole_array_template_keeps_one_row_and_all_states() {
    trajectory(
        "model WholeStored Real z[8](each start=1, each fixed=true);\n\
         equation der(z)=-z; end WholeStored;",
        "WholeStored",
        &(1..=8)
            .map(|i| (format!("z[{i}]"), 1.0, 1.0))
            .collect::<Vec<_>>(),
    );
}

#[test]
fn adjacent_disabled_array_templates_never_claim_each_others_rows() {
    trajectory(
        "model AdjacentStored\n\
         Real x[2](each start=1, each fixed=true);\n\
         Real y[2](each start=2, each fixed=true); Real a,b;\n\
         equation der(x)=-x; der(y)=-2*y; a=3; b=4; end AdjacentStored;",
        "AdjacentStored",
        &[
            ("x[1]".into(), 1.0, 1.0),
            ("x[2]".into(), 1.0, 1.0),
            ("y[1]".into(), 2.0, 2.0),
            ("y[2]".into(), 2.0, 2.0),
        ],
    );
}

#[test]
fn disabled_prefix_template_packs_outer_rows_once_and_retains_trailing_axes() {
    for direction in ["1:2", "2:-1:1"] {
        let source = format!(
            "model PrefixStored\n\
             Real x[2,3](start={{{{1,2,3}},{{4,5,6}}}}, each fixed=true);\n\
             equation for i in {direction} loop\n\
               der(x[i,:])={{-x[i,1],-2*x[i,2],-3*x[i,3]}};\n\
             end for; end PrefixStored;"
        );
        trajectory(
            &source,
            "PrefixStored",
            &(1..=2)
                .flat_map(|i| {
                    (1..=3).map(move |j| {
                        (
                            format!("x[{i},{j}]"),
                            f64::from((i - 1) * 3 + j),
                            f64::from(j),
                        )
                    })
                })
                .collect::<Vec<_>>(),
        );
    }
}

#[test]
fn disabled_scalar_binder_template_retains_source_coordinate_order() {
    trajectory(
        "model ScalarStored Real z[4](each start=1, each fixed=true);\n\
         equation for i in 1:4 loop der(z[i])=-i*z[i]; end for; end ScalarStored;",
        "ScalarStored",
        &(1..=4)
            .map(|i| (format!("z[{i}]"), 1.0, f64::from(i)))
            .collect::<Vec<_>>(),
    );
}
