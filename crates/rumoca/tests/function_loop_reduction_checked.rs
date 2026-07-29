use rumoca::Compiler;
use rumoca_ir_dae::{DaeGeneration, DaeProvenanceOrigin};
use rumoca_sim::{SimOptions, simulate_dae};

const LOOP_REDUCTIONS: &str = r#"
function while_sum
  input Integer n;
  output Integer result;
protected
  Integer i;
algorithm
  result := 0;
  i := 0;
  while i < n loop
    result := result + i;
    i := i + 1;
  end while;
end while_sum;

function capped_sum
  input Integer n;
  output Integer result;
algorithm
  result := 0;
  for i in 1:n loop
    if i > 5 then
      break;
    end if;
    result := result + i;
  end for;
end capped_sum;

model CheckedLoopReductions
  Real whileValue;
  Real cappedValue;
  Real emptyWhile;
  Real shortCapped;
equation
  whileValue = while_sum(integer(10 * time));
  cappedValue = capped_sum(integer(20 * time));
  emptyWhile = while_sum(integer(-10 * time));
  shortCapped = capped_sum(integer(3 * time));
end CheckedLoopReductions;
"#;

#[test]
fn proved_loop_reductions_round_trip_and_remain_runtime_computable() {
    let compiled = Compiler::new()
        .model("CheckedLoopReductions")
        .compile_str(LOOP_REDUCTIONS, "checked_loop_reductions.mo")
        .expect("proved loop reductions should construct checked DAE");
    compiled.dae.inspect(|view| {
        assert!(
            (0..view.expression_count())
                .filter_map(|index| view.expression_id(index))
                .filter_map(|id| view.expression(id))
                .any(|expression| {
                    expression.provenance().origin()
                        == DaeProvenanceOrigin::Generated(DaeGeneration::FunctionLoopLowering)
                })
        );
    });

    let wire =
        serde_json::to_string(&compiled.dae).expect("loop reductions should serialize through v11");
    let decoded: rumoca_compile::compile::Dae =
        serde_json::from_str(&wire).expect("wire-v11 reconstructs loop-reduction expressions");
    let simulation = simulate_dae(&decoded, &SimOptions::default())
        .expect("loop reductions should lower to computable Solve IR");
    for (name, expected) in [
        ("whileValue", 45.0),
        ("cappedValue", 15.0),
        ("emptyWhile", 0.0),
        ("shortCapped", 6.0),
    ] {
        let variable = simulation
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap_or_else(|| panic!("{name} is visible"));
        let value = simulation.data[variable]
            .last()
            .copied()
            .expect("runtime loop-reduction trace is non-empty");
        assert!((value - expected).abs() <= 1.0e-10, "{name} = {value}");
    }
}

#[test]
fn non_unit_while_induction_fails_at_the_loop_owner() {
    let source = LOOP_REDUCTIONS.replace("i := i + 1;", "i := i + 2;");
    let error = Compiler::new()
        .model("CheckedLoopReductions")
        .compile_str(&source, "invalid_loop_reduction.mo")
        .expect_err("an unproved induction must not receive the closed form");
    let message = error.to_string();
    assert!(
        message.contains("function loop reduction")
            && message.contains("proved finite arithmetic-series form"),
        "the semantic loop owner should reject the invalid proof: {message}"
    );
}
