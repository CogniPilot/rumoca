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

#[test]
fn full14400_selection_reports_the_remaining_compact_domain_owner() {
    let source = include_str!("fixtures/feature_selection_full.mo");
    assert!(source.contains("parameter Integer capacity = 14400;"));
    let mut session = Session::default();
    session
        .add_document("feature_selection_full.mo", source)
        .expect("the full editable source parses");
    let error = session
        .compile_model("FeatureSelection")
        .expect_err("generic While and unproved mutable domains still require checked owners");
    assert!(
        format!("{error:?}").contains("function loop domain"),
        "{error:?}"
    );
}
