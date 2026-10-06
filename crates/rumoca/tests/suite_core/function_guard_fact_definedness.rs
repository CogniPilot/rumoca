//! MLS 3.6 §12.4.4 leaves a function value undefined until a statement
//! assigns it. A value that a conditional defines only on some paths is still
//! defined at a later point that no undefined path can reach: here `accepted`
//! is 0 on every path that leaves `confirmed` undefined, so the closing
//! `if accepted <> 1` branch runs on all of those paths and defines it. The
//! analysis is a coverage proof from literal assignments and comparisons; the
//! source executes unchanged. A path the facts do not exclude keeps the value
//! undefined and the function is refused.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

const MODELS: &str = r#"
function commit "a Real flag set by the commit path, reset by validation"
  input Boolean requested;
  input Real proposal[3];
  input Boolean anchorsValid;
  output Real confirmed[3];
  output Real accepted;
algorithm
  accepted := 0.0;
  if requested then
    confirmed := proposal;
    accepted := 1.0;
    if accepted == 1.0 then
      accepted := if anchorsValid then 1.0 else 0.0;
    end if;
  end if;
  if accepted <> 1.0 then
    for slot in 1:3 loop
      confirmed[slot] := -slot;
    end for;
  end if;
end commit;

function commitInteger "the same with an Integer flag"
  input Boolean requested;
  input Real proposal[3];
  output Real confirmed[3];
  output Integer accepted;
algorithm
  accepted := 0;
  if requested then
    confirmed := proposal;
    accepted := 1;
  end if;
  if accepted <> 1 then
    confirmed := zeros(3);
  end if;
end commitInteger;

function commitNested "a Boolean guard inside the commit path"
  input Boolean requested;
  input Boolean prepared;
  input Real proposal[3];
  output Real confirmed[3];
  output Real accepted;
algorithm
  accepted := 0.0;
  if requested then
    if prepared then
      confirmed := proposal;
      accepted := 1.0;
    end if;
  end if;
  if accepted <> 1.0 then
    confirmed := fill(-1.0, 3);
  end if;
end commitNested;

function uncovered "the closing guard misses the paths where accepted is 0"
  input Boolean requested;
  input Boolean prepared;
  input Real proposal[3];
  output Real confirmed[3];
  output Real accepted;
algorithm
  accepted := 0.0;
  if requested then
    if prepared then
      confirmed := proposal;
      accepted := 1.0;
    end if;
  end if;
  if accepted == 2.0 then
    confirmed := zeros(3);
  end if;
end uncovered;

model Commits
  input Boolean requested = true;
  input Boolean valid = true;
  input Boolean prepared = false;
  output Real confirmed[3];
  output Real accepted;
  output Real rolledBack[3];
  output Real rejected;
  output Real integerConfirmed[3];
  output Real nested[3];
  output Real nestedAccepted;
protected
  Integer integerAccepted;
equation
  (confirmed, accepted) = commit(requested, {1, 2, 3}, valid);
  (rolledBack, rejected) = commit(requested, {1, 2, 3}, not valid);
  (integerConfirmed, integerAccepted) = commitInteger(not requested, {1, 2, 3});
  (nested, nestedAccepted) = commitNested(requested, prepared, {1, 2, 3});
end Commits;

model Uncovered
  output Real confirmed[3];
  output Real accepted;
equation
  (confirmed, accepted) = uncovered(false, true, {1, 2, 3});
end Uncovered;
"#;

#[test]
fn a_closing_branch_the_undefined_paths_must_take_completes_the_output() {
    let compiled = Compiler::new()
        .model("Commits")
        .compile_str(MODELS, "GuardFactDefinedness.mo")
        .unwrap_or_else(|error| panic!("Commits should compile: {error}"));
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .unwrap_or_else(|error| panic!("Commits should evaluate: {error}"));
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    let value = |name: &str| {
        probe
            .report
            .solver_y
            .iter()
            .find(|slot| slot.name.replace(' ', "") == name)
            .unwrap_or_else(|| panic!("Commits has {name}"))
            .value
    };
    let vector = |name: &str| [1, 2, 3].map(|index| value(&format!("{name}[{index}]")));
    // Accepted commit keeps the proposal.
    assert_eq!(vector("confirmed"), [1.0, 2.0, 3.0]);
    assert_eq!(value("accepted"), 1.0);
    // Validation resets the flag, so the closing branch rolls back.
    assert_eq!(vector("rolledBack"), [-1.0, -2.0, -3.0]);
    assert_eq!(value("rejected"), 0.0);
    // Not requested: the Integer flag stays 0 and the closing branch defines.
    assert_eq!(vector("integerConfirmed"), [0.0, 0.0, 0.0]);
    // Requested but not prepared: still 0, still rolled back.
    assert_eq!(vector("nested"), [-1.0, -1.0, -1.0]);
    assert_eq!(value("nestedAccepted"), 0.0);
}

#[test]
fn a_path_the_facts_do_not_exclude_leaves_the_output_undefined() {
    let error = Compiler::new()
        .model("Uncovered")
        .compile_str(MODELS, "GuardFactDefinedness.mo")
        .map(|_| ())
        .expect_err("confirmed is undefined when requested is false");
    let message = error.to_string();
    assert!(
        message.contains("returns `confirmed` without defining every declared element"),
        "{message}"
    );
}
