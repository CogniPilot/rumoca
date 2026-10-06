//! MLS 3.6 §11.2.6 executes exactly the statements of the selected branch. A
//! conditional whose branches own a loop runs as guarded statements and one
//! loop-free remainder conditional; when that conditional is itself selected
//! by an enclosing branch, its `else` part must still run only under that
//! enclosing selection. A value an earlier guarded branch defines is then read
//! by the captured selections that the same guard implies.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

const MODELS: &str = r#"
function countSigns "the else part of a nested loop conditional"
  input Boolean enabled;
  input Real x[2];
  output Real y;
algorithm
  y := 0;
  if enabled then
    for k in 1:2 loop
      if x[k] > 0 then
        for a in 1:1 loop
          y := y + 1;
        end for;
      else
        y := y + 100;
      end if;
    end for;
  end if;
end countSigns;

function allPositive
  input Real x[:];
  output Boolean ok;
algorithm
  ok := true;
  for i in 1:size(x, 1) loop
    ok := ok and x[i] > 0;
  end for;
end allPositive;

function synchronize "a local defined under a guard, read by nested selections"
  input Real p[:];
  input Real q[size(p, 1)];
  input Boolean accepted;
  input Boolean reset;
  output Real o[size(p, 1)];
  output Real reason;
protected
  Boolean valid;
  Boolean slotValid;
algorithm
  o := p;
  reason := 1;
  if accepted then
    reason := 2;
    valid := allPositive(p);
    if valid then
      reason := 3;
      if not reset then
        for k in 1:size(p, 1) loop
          if q[k] == 1 then
            slotValid := p[k] > 0;
            for a in 1:2 loop
              slotValid := slotValid and p[k] < 10;
            end for;
            valid := valid and slotValid;
          else
            valid := valid and q[k] == 0;
          end if;
        end for;
      end if;
      if valid then
        reason := 4;
        o := q;
      end if;
    end if;
  end if;
end synchronize;

model Remainders
  Real disabled;
  Real enabled;
  Real o[3];
  Real reason;
  Real skippedO[3];
  Real skippedReason;
equation
  disabled = countSigns(false, {-1, -2});
  enabled = countSigns(true, {-1, 2});
  (o, reason) = synchronize({1, 2, 3}, {1, 0, 1}, true, false);
  (skippedO, skippedReason) = synchronize({1, 2, 3}, {1, 0, 1}, false, false);
end Remainders;
"#;

#[test]
fn a_nested_remainder_else_runs_only_under_its_enclosing_selection() {
    let compiled = Compiler::new()
        .model("Remainders")
        .compile_str(MODELS, "Remainders.mo")
        .expect("guarded remainders and guarded reads are admitted");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the checked DAE evaluates");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    let value = |name: &str| {
        probe
            .report
            .solver_y
            .iter()
            .find(|slot| slot.name.replace(' ', "") == name)
            .unwrap_or_else(|| {
                panic!(
                    "Remainders has {name}; have: {:?}",
                    probe
                        .report
                        .solver_y
                        .iter()
                        .map(|slot| slot.name.clone())
                        .collect::<Vec<_>>()
                )
            })
            .value
    };
    assert_eq!(
        value("disabled"),
        0.0,
        "a false selection runs no else part"
    );
    assert_eq!(value("enabled"), 101.0);
    assert_eq!(value("reason"), 4.0);
    for (index, expected) in [1.0, 0.0, 1.0].into_iter().enumerate() {
        assert_eq!(value(&format!("o[{}]", index + 1)), expected);
    }
    assert_eq!(value("skippedReason"), 1.0);
    for (index, expected) in [1.0, 2.0, 3.0].into_iter().enumerate() {
        assert_eq!(value(&format!("skippedO[{}]", index + 1)), expected);
    }
}
