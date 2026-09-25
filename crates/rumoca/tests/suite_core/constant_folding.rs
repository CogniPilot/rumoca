//! Solve lowering folds exact literal arithmetic, so the emitted C stores and
//! multiplies no literal a product or sum could absorb.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

const LITERAL_PRODUCTS: &str = "
model LiteralProducts
  Real x(start = 1, fixed = true);
  Real y[3];
  Real z;
  Real w;
equation
  y = {1, 0, 0} * x + {1, 2, 3} * 0.5;
  z = 2 * 3 * cos(x) + 1 * sin(x);
  w = -(-(-x)) / 1 - 0;
  der(x) = w + y[2] - 1 + z - z;
end LiteralProducts;";

/// Every emitted C translation unit of the fmi3 target.
fn fmi3_c(model: &str, source: &str) -> String {
    let compiled = match Compiler::new()
        .model(model)
        .compile_str(source, &format!("{model}.mo"))
    {
        Ok(compiled) => compiled,
        Err(error) => panic!("compile {model}: {error:#}"),
    };
    let files = match rumoca::render_target_files(&compiled, model, "fmi3", None) {
        Ok(files) => files,
        Err(error) => panic!("render {model}: {error:#}"),
    };
    files
        .into_iter()
        .filter(|file| file.path.starts_with("sources/rmc_") && file.path.ends_with(".c"))
        .map(|file| file.content)
        .collect::<Vec<_>>()
        .join("\n")
}

fn count(source: &str, pattern: &str) -> usize {
    source.matches(pattern).count()
}

#[test]
fn literal_arithmetic_leaves_no_foldable_store_or_product() {
    let source = fmi3_c("LiteralProducts", LITERAL_PRODUCTS);
    // Literal-only products and sums become one literal per value (`2*3` is
    // one `6.0` store, its factors are never stored); `1*x`, `x/1`, `x - 0`,
    // and `-(-x)` reuse their operand, so `-(-(-x))` is one negation; and
    // `0*x` over the state is dropped by the incidence proof, so no zero is
    // stored.
    for (pattern, expected) in [
        ("= 0.0;", 0),
        ("= 2.0;", 0),
        ("= 3.0;", 0),
        ("= 6.0;", 1),
        ("(-r[", 1),
    ] {
        let found = count(&source, pattern);
        assert_eq!(found, expected, "`{pattern}` appears {found} times");
    }
    let result = simulate_dae_with_diagnostics(
        &Compiler::new()
            .model("LiteralProducts")
            .compile_str(LITERAL_PRODUCTS, "LiteralProducts.mo")
            .unwrap()
            .dae,
        &SimOptions {
            solver_mode: SimSolverMode::Bdf,
            t_end: 1.0,
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .unwrap();
    let column = |name: &str| {
        let index = result.names.iter().position(|n| n == name).unwrap();
        &result.data[index]
    };
    for (row, &time) in result.times.iter().enumerate() {
        let x = (-time).exp();
        for (name, expected) in [("x", x), ("y[1]", x + 0.5), ("y[2]", 1.0), ("y[3]", 1.5)] {
            let actual = column(name)[row];
            assert!(
                (actual - expected).abs() < 1e-4,
                "{name} at {time}: {actual} != {expected}"
            );
        }
    }
}

/// `0 * cos(x)` is NaN for a non-finite `x`, and `cos(x)` is not a coordinate
/// the incidence proof can drop, so the product stays.
#[test]
fn an_unproven_zero_product_is_not_folded() {
    let source = fmi3_c(
        "UnprovenZero",
        "model UnprovenZero
           Real x(start = 1, fixed = true);
           Real a;
         equation
           a = 0 * cos(x);
           der(x) = -x + a;
         end UnprovenZero;",
    );
    assert!(count(&source, "= 0.0;") >= 1, "{source}");
    assert!(count(&source, " * ") >= 1, "{source}");
}
