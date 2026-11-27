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

/// The DAE as JSON without source positions and identities, which name where
/// and in which order things were made, not what the model means.
fn dae_meaning(dae: &rumoca_ir_dae::Dae) -> serde_json::Value {
    fn strip(value: &mut serde_json::Value) {
        match value {
            serde_json::Value::Object(map) => {
                for key in ["span", "source_span", "def_id", "instance_id", "source"] {
                    map.remove(key);
                }
                map.values_mut().for_each(strip);
            }
            serde_json::Value::Array(items) => items.iter_mut().for_each(strip),
            _ => {}
        }
    }
    let mut value = serde_json::to_value(dae).expect("the DAE serializes");
    strip(&mut value);
    value
}

/// The flat starts above must also survive the rest of the pipeline: a
/// constant left unresolved in flatten (`nS`, `dryair.R_s`) only fails when
/// the DAE is built, which the flatten test alone cannot see. Each run
/// builds the DAE, and every run gives the same one.
#[test]
fn psychrometric_data_compiles_to_the_same_dae_on_every_run() {
    let root = super::native_linear_solve::msl_root().expect("the required MSL fixture exists");
    let media = root.join("Modelica 4.1.0/Media/package.mo");
    let mut first = None;
    for run in 0..RUNS {
        let compiled = Compiler::new()
            .model("Modelica.Media.Examples.PsychrometricData")
            .source_root(root.to_string_lossy().as_ref())
            .compile_file(media.to_string_lossy().as_ref())
            .unwrap_or_else(|error| {
                panic!("run {run}: PsychrometricData compiles to a DAE: {error}")
            });
        let dae = dae_meaning(&compiled.dae);
        match &first {
            None => first = Some(dae),
            Some(first) => assert!(*first == dae, "run {run} builds a different DAE than run 0"),
        }
    }
}

/// The package a type is spelled through is the one Resolve resolves the
/// first segment to (MLS 3.7 section 5.3: inherited elements before enclosing
/// scopes). Here `M`'s `Medium` is the one `Base` declares (`A`), not the
/// `Medium` of the enclosing package `P` (`B`). `Probe` is a class both media
/// only inherit, so either one would contain it: only the lookup itself can
/// tell them apart, and the start must read `A`'s `c`.
const INHERITED_ALIAS: &str = "package Media
  partial package Partial
    constant Real c = 0;
    model Probe
      Real x(start = c, fixed = true);
    equation
      der(x) = 0;
    end Probe;
  end Partial;
  package A
    extends Partial(c = 1);
  end A;
  package B
    extends Partial(c = 2);
  end B;
end Media;
package P
  package Medium = Media.B;
  model Base
    package Medium = Media.A;
  end Base;
  model M
    extends Base;
    Medium.Probe p;
  end M;
end P;
";

#[test]
fn an_inherited_medium_alias_wins_over_the_enclosing_package_alias() {
    for run in 0..RUNS {
        let flat = Compiler::new()
            .model("P.M")
            .compile_str_flat(INHERITED_ALIAS, "InheritedAlias.mo")
            .unwrap_or_else(|error| panic!("P.M flattens: {error}"));
        let start = flat
            .variables
            .iter()
            .find(|(name, _)| name.as_str() == "p.x")
            .and_then(|(_, variable)| variable.start.as_ref())
            .expect("p.x keeps its start");
        assert!(
            matches!(
                start,
                Expression::Literal { value: Literal::Real(value), .. } if *value == 1.0
            ) || matches!(
                start,
                Expression::Literal {
                    value: Literal::Integer(1),
                    ..
                }
            ),
            "flatten {run}: p.x starts at {start:?}, not A's c = 1"
        );
    }
}
