//! Compact dependent ranges retain source point order without unrolling.

use super::*;
use rumoca_eval_dae::NumericEvaluator;
use rumoca_ir_dae::ExpressionOperation;

const STRIDED: &str = r#"
function ordered
  input Real u;
  output Real y;
algorithm
  y := 0;
  for i in 1:4 loop
    for j in i:2:4 loop
      y := y*10 + j*u;
    end for;
  end for;
end ordered;
model Ordered
  parameter Real u = 2;
  Real y;
equation
  y = ordered(u);
end Ordered;
"#;

fn compile(source: &str) -> anyhow::Result<CompilationResult> {
    let mut session = Session::default();
    session.add_document("bounded_domains.mo", source)?;
    session.compile_model("Ordered")
}

fn evaluate(source: &str, input: f64) -> f64 {
    let compiled = compile(source).expect("finite ordered domains construct checked DAE owners");
    assert!(compiled.is_balanced());
    compiled.dae.inspect(|view| {
        let calls = (0..view.expression_count())
            .filter_map(|ordinal| view.expression_id(ordinal))
            .filter(|id| {
                matches!(
                    view.expression(*id).unwrap().operation(),
                    ExpressionOperation::Call { .. }
                )
            })
            .collect::<Vec<_>>();
        assert_eq!(calls.len(), 1, "one source call has one checked owner");
        let ExpressionOperation::Call { function, .. } =
            view.expression(calls[0]).unwrap().operation()
        else {
            panic!("the selected owner is a function call")
        };
        assert!(
            view.function(function).unwrap().fold_count() > 0,
            "source loops remain compact FunctionFold owners"
        );
        NumericEvaluator::with_overrides(view, |variable, _| {
            (variable.name().as_str() == "u").then_some(input)
        })
        .expression(calls[0])
        .expect("checked compact function evaluates")[0]
    })
}

#[test]
fn dependent_positive_stride_reaches_the_compact_runtime() {
    // Source points [1,3], [2,4], [3], [4], with u=2.
    assert_eq!(evaluate(STRIDED, 2.0), 264868.0);
    assert_eq!(evaluate(STRIDED, -3.0), -397302.0);
}

#[test]
fn dependent_negative_stride_keeps_descending_order() {
    let source = STRIDED.replace("i:2:4", "i:-2:1");
    // Source points [1], [2], [3,1], [4,2], with u=2.
    assert_eq!(evaluate(&source, 2.0), 246284.0);
    assert_eq!(evaluate(&source, -3.0), -369426.0);
}

#[test]
fn nonlinear_dependent_range_keeps_its_interior_points() {
    let source = STRIDED
        .replace("1:4", "-3:3")
        .replace("i:2:4", "i*i:9")
        .replace("y*10 + j*u", "y + j*u");
    let expected: i64 = (-3_i64..=3).flat_map(|i| i * i..=9).sum();
    assert_eq!(evaluate(&source, 2.0), (2 * expected) as f64);
    assert_eq!(evaluate(&source, -3.0), (-3 * expected) as f64);
}

#[test]
fn mutating_a_range_operand_requires_the_entry_snapshot_owner() {
    let source = STRIDED
        .replace("output Real y;", "output Real y; protected Integer k;")
        .replace("y := 0;", "y := 0; k := 4;")
        .replace("i:2:4", "i:2:k")
        .replace("y := y*10 + j*u;", "y := y*10 + j*u; k := 0;");
    let error =
        compile(&source).expect_err("live membership must not replace an entry-evaluated range");
    assert!(format!("{error:?}").contains("entry snapshot"), "{error:?}");
}

#[test]
fn loop_binders_shadow_an_immutable_function_local_in_the_actual_pipeline() {
    let source = STRIDED.replace(
        "output Real y;",
        "output Real y; protected constant Integer i=987;",
    );
    assert_eq!(evaluate(&source, 2.0), 264868.0);
    assert_eq!(evaluate(&source, -3.0), -397302.0);
}

const FEATURE_SELECTION_FULL: &str = include_str!("fixtures/feature_selection_full.mo");

fn compile_feature_selection(source: &str) -> Result<(), String> {
    let mut session = Session::default();
    session
        .add_document("feature_selection_full.mo", source)
        .expect("the full editable source parses");
    session
        .compile_model("FeatureSelection")
        .map(|_| ())
        .map_err(|error| format!("{error:?}"))
}

/// With the border floor fixed at translation, the counted, windowed and
/// `while` domains of the full selection are all bounded.
#[test]
fn full14400_selection_bounds_every_compact_domain() {
    let tunable = "parameter Integer minimumBorder = 0;";
    assert!(FEATURE_SELECTION_FULL.contains("parameter Integer capacity = 14400;"));
    assert!(FEATURE_SELECTION_FULL.contains(tunable));
    let source =
        FEATURE_SELECTION_FULL.replace(tunable, "final parameter Integer minimumBorder = 0;");
    if let Err(error) = compile_feature_selection(&source) {
        panic!("the counted, windowed and while domains are all bounded: {error}");
    }
}

/// MLS 3.7 §11.2.2: a tunable `minimumBorder` is the only lower bound of the
/// border the grid traversal stops at, so that `while` loop has no
/// translation-time trip count; it is refused rather than frozen at the
/// parameter's declared value.
#[test]
fn full14400_selection_refuses_a_tunable_border_floor() {
    let error = compile_feature_selection(FEATURE_SELECTION_FULL)
        .expect_err("a tunable border floor leaves the grid traversal unbounded");
    assert!(
        error.contains("has no translation-time iteration bound")
            && error.contains("tunable parameter passed to the function"),
        "{error}"
    );
}
