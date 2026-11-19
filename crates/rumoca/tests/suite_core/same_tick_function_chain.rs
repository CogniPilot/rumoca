//! MLS §8.5: equations in one when body consume the current event's values.
//! A pure scalar function does not turn an acyclic equation chain into a
//! simultaneous discrete loop or correlate independent Integer assignments.

use rumoca::Compiler;

pub(super) const REFINE_FUNCTION: &str = r#"
  function refine
    input Real x;
    output Real y;
  protected
    Real lower;
    Real upper;
    Real middle;
  algorithm
    lower := 0;
    upper := 10;
    for j in 1:60 loop
      middle := (lower + upper)/2;
      if middle*middle < x then
        lower := middle;
      else
        upper := middle;
      end if;
    end for;
    y := (lower + upper)/2;
  end refine;
"#;

#[test]
fn independent_when_assignments_still_reject_current_value_cycles() {
    let source = r#"
model CurrentCycle
  discrete Integer first(start=0, fixed=true);
  discrete Integer second(start=0, fixed=true);
equation
  when sample(0, 1) then
    first = second + 1;
    second = first + 1;
  end when;
end CurrentCycle;
"#;
    let Err(error) = Compiler::new()
        .model("CurrentCycle")
        .compile_str(source, "same_tick_current_cycle.mo")
    else {
        panic!("a current-value Integer assignment cycle must remain rejected");
    };
    assert!(format!("{error:?}").contains("current-value dependency cycle"));
}

#[test]
fn independent_integer_equations_can_straddle_a_real_function_producer() {
    let source = format!(
        r#"
model RefineChain
{REFINE_FUNCTION}
  discrete Integer count(start=0, fixed=true);
  discrete Real requested;
  discrete Real root;
  discrete Integer mode(start=0, fixed=true);
equation
  when sample(0, 1) then
    count = pre(count) + 1;
    requested = 2*count;
    root = refine(requested);
    mode = if root > 2 then 1 else pre(mode);
  end when;
end RefineChain;
"#
    );
    let compiled = Compiler::new()
        .model("RefineChain")
        .compile_str(&source, "same_tick_function_chain.mo")
        .expect("the acyclic mixed-type when equations compile");
    let result = rumoca_sim::simulate_dae_with_diagnostics(
        &compiled.dae,
        &rumoca_sim::SimOptions {
            t_end: 2.0,
            dt: Some(0.25),
            ..Default::default()
        },
    )
    .expect("independent equations schedule through the pure function producer");
    for (name, first, last) in [
        ("count", 1.0, 3.0),
        ("requested", 2.0, 6.0),
        ("root", 2_f64.sqrt(), 6_f64.sqrt()),
        ("mode", 0.0, 1.0),
    ] {
        let index = result.names.iter().position(|entry| entry == name).unwrap();
        let actual = &result.data[index];
        assert!((actual.first().unwrap() - first).abs() < 1e-12, "{name}");
        assert!((actual.last().unwrap() - last).abs() < 1e-12, "{name}");
    }
}

// MLS §8.5: independent when equations consume current-event values; an
// unrelated selected array equation must not fuse index with its consumer.
#[test]
fn selected_real_array_does_not_fuse_independent_integer_owners() {
    let source = r#"model TupleAccumulator
function pair
input Integer i;
output Real a; output Real b;
algorithm a:=i; b:=i+1;
end pair;
discrete Integer index(start=0,fixed=true);
discrete Integer status;
discrete Real a; discrete Real b;
discrete Real energy[12](each start=0,each fixed=true);
equation
when sample(0,1) then
index=min(pre(index)+1,2);
(a,b)=pair(index);
status=if a<b then 1 else 0;
for m in 1:12 loop
energy[m]=pre(energy[m])+(if index==m then a else 0);
end for;
end when;
end TupleAccumulator;
"#;
    let compiled = Compiler::new()
        .model("TupleAccumulator")
        .compile_str(source, "selected_array_owners.mo")
        .expect("acyclic mixed-type equations compile");
    compiled.dae.inspect(|view| {
        assert_eq!(view.discrete_value_owner_count(), 2,
            "independent index/status equations retain separate owners despite the Real array update");
    });
    let result = rumoca_sim::simulate_dae_with_diagnostics(
        &compiled.dae,
        &rumoca_sim::SimOptions {
            t_end: 2.0,
            dt: Some(1.0),
            ..Default::default()
        },
    )
    .expect("array selection preserves independent current-event producers");
    for (name, expected) in [
        ("index", 2.0),
        ("status", 1.0),
        ("a", 2.0),
        ("b", 3.0),
        ("energy[1]", 1.0),
        ("energy[2]", 4.0),
    ] {
        let index = result.names.iter().position(|entry| entry == name).unwrap();
        assert!(
            (result.data[index].last().unwrap() - expected).abs() < 1e-12,
            "{name}"
        );
    }
}

// MLS §8.5: derived continuous array values observe their current-event inputs.
#[test]
fn selected_derived_array_reads_current_accumulator_values() {
    let source = r#"model DerivedArray
  discrete Integer index(start=0,fixed=true);
  discrete Real total[2](each start=0,each fixed=true);
  Real daily[2];
  discrete Real observed;
  discrete Real history;
equation
  for m in 1:2 loop daily[m]=total[m]/10; end for;
  when sample(0,1) then
    index=min(pre(index)+1,2);
    for m in 1:2 loop
      total[m]=pre(total[m])+(if index==m then 1 else 0);
    end for;
    observed=daily[index];
    history=pre(total[2])/10;
  end when;
end DerivedArray;"#;
    let compiled = Compiler::new()
        .model("DerivedArray")
        .compile_str(source, "same_tick_derived_array.mo")
        .unwrap();
    let result = rumoca_sim::simulate_dae_with_diagnostics(
        &compiled.dae,
        &rumoca_sim::SimOptions {
            t_end: 2.0,
            dt: Some(1.0),
            ..Default::default()
        },
    )
    .unwrap();
    let column = |name: &str| result.names.iter().position(|entry| entry == name).unwrap();
    for time in [0.0, 1.0, 2.0] {
        let row = result
            .times
            .iter()
            .rposition(|&value| value == time)
            .unwrap();
        let index = result.data[column("index")][row] as usize;
        let expected = result.data[column(&format!("total[{index}]"))][row] / 10.0;
        assert_eq!(result.data[column("total[1]")][row], 1.0);
        assert_eq!(result.data[column("total[2]")][row], time);
        assert!((result.data[column("history")][row] - (time - 1.0).max(0.0) / 10.0).abs() < 1e-12);
        assert!(
            (result.data[column("observed")][row] - expected).abs() < 1e-12,
            "current derived array at {time}: expected {expected}, received {}",
            result.data[column("observed")][row]
        );
    }
}

#[test]
fn selected_derived_matrix_retains_each_binder_point() {
    let source = r#"model DerivedMatrix
  discrete Integer index(start=0,fixed=true);
  discrete Real total[2,2](each start=0,each fixed=true);
  Real daily[2,2];
  discrete Real observed;
equation
  for r in 1:2, c in 1:2 loop daily[r,c]=total[r,c]/(10*r+c); end for;
  when sample(0,1) then
    index=min(pre(index)+1,2);
    for r in 1:2, c in 1:2 loop total[r,c]=pre(total[r,c])+10*r+c; end for;
    observed=daily[2,index];
  end when;
end DerivedMatrix;"#;
    let compiled = Compiler::new()
        .model("DerivedMatrix")
        .compile_str(source, "same_tick_derived_matrix.mo")
        .unwrap();
    let result = rumoca_sim::simulate_dae_with_diagnostics(
        &compiled.dae,
        &rumoca_sim::SimOptions {
            t_end: 2.0,
            dt: Some(1.0),
            ..Default::default()
        },
    )
    .unwrap();
    let column = result
        .names
        .iter()
        .position(|name| name == "observed")
        .unwrap();
    for time in [0.0, 1.0, 2.0] {
        let row = result
            .times
            .iter()
            .rposition(|&value| value == time)
            .unwrap();
        assert!(
            (result.data[column][row] - (time + 1.0)).abs() < 1e-12,
            "matrix family at {time}: expected {}, received {}",
            time + 1.0,
            result.data[column][row]
        );
    }
}

#[test]
fn derived_family_with_matched_derivative_reads_current_accumulator() {
    let source = r#"model DerivedDerivative
 Real x[2](each start=0,each fixed=true);
 discrete Integer index(start=0,fixed=true);
 discrete Real total[2](each start=0,each fixed=true);
 Real daily[2];
 discrete Real observed;
equation
 der(x)={1,2};
 for m in 1:2 loop daily[m]=der(x[m])+total[m]/10; end for;
 when sample(0,1) then
 index=min(pre(index)+1,2);
 for m in 1:2 loop total[m]=pre(total[m])+(if index==m then 1 else 0); end for;
 observed=daily[index];
 end when;
end DerivedDerivative;"#;
    let compiled = Compiler::new()
        .model("DerivedDerivative")
        .compile_str(source, "derived_family_derivative.mo")
        .unwrap();
    compiled.dae.inspect(|view| {
        let plan = rumoca_phase_structural::CausalDiscretePlan::derive(view).unwrap();
        let definitions =
            rumoca_phase_structural::SameTickDefinitions::derive(view, &plan).unwrap();
        let daily = view
            .variables()
            .find(|(_, variable)| variable.name().as_str() == "daily")
            .unwrap()
            .0;
        assert!(
            definitions.definition_domain(view, daily.index()).is_some(),
            "a derivative coordinate must preserve the source family's complete binder domain"
        );
    });
    let result = rumoca_sim::simulate_dae_with_diagnostics(
        &compiled.dae,
        &rumoca_sim::SimOptions {
            t_end: 2.0,
            dt: Some(1.0),
            ..Default::default()
        },
    )
    .unwrap();
    let column = result
        .names
        .iter()
        .position(|name| name == "observed")
        .unwrap();
    for (time, expected) in [(0.0, 1.1), (1.0, 2.1), (2.0, 2.2)] {
        let row = result
            .times
            .iter()
            .rposition(|&value| value == time)
            .unwrap();
        assert!(
            (result.data[column][row] - expected).abs() < 1e-12,
            "derived family with derivative at {time}: expected {expected}, received {}",
            result.data[column][row]
        );
    }
}
