//! MLS §5.3, §7.3: `Modelica.Media.Examples.PsychrometricData` declares
//! `package Medium = Modelica.Media.Air.MoistAir` and components
//! `Medium.BaseProperties medium_T[n_T]`. Their `X(start = reference_X)` and
//! `Xi(start = reference_X[1:nXi])` read the `reference_X` that `MoistAir`
//! modifies (`{0.01, 0.99}`), never `PartialMedium`'s default
//! `fill(1/nX, nX)`.
//!
//! Flatten visits the packages that expose constants in a hash order that
//! changes from one flatten to the next, and on that order the start used to
//! depend: so the model is flattened several times and every flatten must
//! give every medium MoistAir's composition.

use rumoca::Compiler;
use rumoca_core::{Expression, Literal};

const RUNS: usize = 6;

/// The literal Real elements of the composition a start reads: an array
/// expression, or the array a slice of it selects from (`Xi`'s
/// `reference_X[1:nXi]`).
fn real_elements(expression: &Expression) -> Option<Vec<f64>> {
    let expression = match expression {
        Expression::Index { base, .. } => base.as_ref(),
        other => other,
    };
    let Expression::Array { elements, .. } = expression else {
        return None;
    };
    elements
        .iter()
        .map(|element| match element {
            Expression::Literal {
                value: Literal::Real(value),
                ..
            } => Some(*value),
            _ => None,
        })
        .collect()
}

#[test]
fn every_medium_takes_moist_air_reference_composition_on_every_flatten() {
    let root = super::native_linear_solve::msl_root().expect("the required MSL fixture exists");
    let media = root.join("Modelica 4.1.0/Media/package.mo");
    for run in 0..RUNS {
        let flat = Compiler::new()
            .model("Modelica.Media.Examples.PsychrometricData")
            .source_root(root.to_string_lossy().as_ref())
            .compile_file_flat(media.to_string_lossy().as_ref())
            .unwrap_or_else(|error| panic!("PsychrometricData flattens: {error}"));
        let mut checked = 0;
        for (name, variable) in &flat.variables {
            let name = name.as_str();
            if !name.ends_with(".X") && !name.ends_with(".Xi") {
                continue;
            }
            let expected: &[f64] = &[0.01, 0.99];
            let start = variable
                .start
                .as_ref()
                .unwrap_or_else(|| panic!("{name} keeps its start"));
            assert_eq!(
                real_elements(start).as_deref(),
                Some(expected),
                "flatten {run}: {name} starts at {start:?}"
            );
            checked += 1;
        }
        assert!(
            checked >= 42,
            "flatten {run}: {checked} compositions, fewer than X and Xi of 21 media"
        );
    }
}
