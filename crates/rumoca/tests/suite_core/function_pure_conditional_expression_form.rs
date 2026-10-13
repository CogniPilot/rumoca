//! A function whose if/elseif statements only assign the same outputs is
//! constructed as one conditional expression per output, so call sites can
//! expand it. A branch assertion needs the ordered statement owner and keeps
//! it.

use rumoca::Compiler;
use rumoca_ir_dae as dae;
use rumoca_sim::{SimOptions, eval_dae_at};

fn source(branch_prefix: &str) -> String {
    format!(
        r#"
model Probe
  function F
    input Real u;
    output Real y;
    output Real z;
  protected
    Real k;
  algorithm
    if u > 1 then
      {branch_prefix}k := 2;
      y := k * u;
    elseif u > 0 then
      k := 3;
      y := k + u;
    else
      k := 4;
      y := -u;
    end if;
    z := y + k;
  end F;
  input Real u = 0.5;
  output Real y;
  output Real z;
equation
  (y, z) = F(u);
end Probe;
"#
    )
}

/// The statement shapes of function `F` and its evaluated outputs.
fn lower(branch_prefix: &str) -> (Vec<&'static str>, Vec<f64>) {
    let compiled = Compiler::new()
        .model("Probe")
        .compile_str(&source(branch_prefix), "Probe.mo")
        .expect("the function compiles");
    let mut shapes = Vec::new();
    compiled.dae.inspect(|view| {
        let function = (0..view.function_count())
            .filter_map(|index| view.function_id(index).and_then(|id| view.function(id)))
            .find(|function| function.name().as_str().ends_with("F"))
            .unwrap_or_else(|| panic!("no F among {} functions", view.function_count()));
        shapes = function
            .statements()
            .map(|statement| match statement {
                dae::FunctionStatementView::Assignment { .. } => "assignment",
                dae::FunctionStatementView::Conditional { .. } => "conditional",
                _ => "other",
            })
            .collect();
    });
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the function evaluates");
    let values = probe
        .report
        .solver_y
        .iter()
        .filter(|slot| slot.name == "y")
        .map(|slot| slot.value)
        .collect();
    (shapes, values)
}

#[test]
fn a_pure_if_elseif_statement_body_is_straight_line_assignments() {
    let (shapes, values) = lower("");
    assert!(
        shapes.iter().all(|shape| *shape == "assignment"),
        "a pure conditional is expression-owned: {shapes:?}"
    );
    assert_eq!(values, vec![3.5]);
}

#[test]
fn a_branch_assertion_keeps_the_ordered_conditional() {
    let (shapes, values) = lower("assert(u > -10, \"bounded\"); ");
    assert!(
        shapes.contains(&"conditional"),
        "an assertion needs the ordered owner: {shapes:?}"
    );
    assert_eq!(values, vec![3.5]);
}
