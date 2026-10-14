//! MLS §8.3.5 and §10.5: element events retain their own triggers and holds.
use super::{SimOptions, compile, simulate_dae_with_diagnostics};

#[test]
fn whole_array_transactions_and_selected_real_events_retain_distinct_owners() {
    let source = r#"
function Pair
  input Real value;
  output Real a[2];
  output Real b[2];
algorithm
  a := {value, value+1};
  b := {value+10, value+11};
end Pair;
model WholeAndSelected
  discrete Real a[2](each start=0, each fixed=true);
  discrete Real b[2](each start=0, each fixed=true);
  discrete Real c[3](each start=0, each fixed=true);
equation
  when time>0.15 then c[2]=7; end when;
algorithm
  when sample(0, 0.1) then
    (a,b) := Pair(pre(a[1])+1);
  end when;
end WholeAndSelected;
"#;
    let compiled = compile(source, "WholeAndSelected");
    let result = simulate_dae_with_diagnostics(
        &compiled,
        &SimOptions {
            t_end: 0.35,
            ..SimOptions::default()
        },
    )
    .unwrap();
    for (name, expected) in [
        ("a[1]", 4.0),
        ("a[2]", 5.0),
        ("b[1]", 14.0),
        ("b[2]", 15.0),
        ("c[1]", 0.0),
        ("c[2]", 7.0),
        ("c[3]", 0.0),
    ] {
        let channel = result
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap();
        assert_eq!(result.data[channel].last(), Some(&expected), "{name}");
    }
}

const SOURCE: &str = r#"
model ElementEvents
  type Mode = enumeration(Off, On);
  Real x[2](each start=0, each fixed=true);
  Boolean e[4](each start=false, each fixed=true);
  Integer k[2](each start=0, each fixed=true);
  Mode m[2](each start=Mode.Off, each fixed=true);
  discrete Real z[2](each start=0, each fixed=true);
equation
  der(x)={1,2};
  when x[1]>0.2 then e[1]=true; k[1]=1; m[1]=Mode.On; z[1]=1; end when;
  when x[2]>0.4 then e[2]=true; k[2]=2; m[2]=Mode.On; z[2]=2; end when;
  when x[1]>0.5 then e[3]=true; end when;
end ElementEvents;
"#;

#[test]
fn independent_and_simultaneous_array_element_events_preserve_untouched_values() {
    let compiled = compile(SOURCE, "ElementEvents");
    let result = simulate_dae_with_diagnostics(
        &compiled,
        &SimOptions {
            t_end: 0.8,
            ..SimOptions::default()
        },
    )
    .unwrap();
    let at = |name: &str, time: f64| {
        let channel = result
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap();
        let sample = result
            .times
            .iter()
            .position(|&sample| sample >= time)
            .unwrap();
        result.data[channel][sample]
    };
    for element in 1..=4 {
        assert_eq!(at(&format!("e[{element}]"), 0.0), 0.0);
    }
    for element in 1..=2 {
        assert_eq!(at(&format!("e[{element}]"), 0.3), 1.0);
        assert_eq!(at(&format!("k[{element}]"), 0.3), element as f64);
        assert_eq!(at(&format!("m[{element}]"), 0.3), 2.0);
        assert_eq!(at(&format!("z[{element}]"), 0.3), element as f64);
    }
    assert_eq!(at("e[3]", 0.3), 0.0);
    assert_eq!(at("e[3]", 0.7), 1.0);
    assert_eq!(at("e[4]", 0.7), 0.0);
}

#[test]
fn selected_current_dependencies_and_elsewhen_priority_keep_array_storage() {
    let source = r#"
model SelectedReads
  Boolean e[3](each start=false,each fixed=true);
  Integer k[2](each start=0,each fixed=true);
equation
  when e[1] then e[2]=e[1]; end when;
  when time>0.1 then e[1]=true; end when;
  when time>0.1 then k[1]=1; elsewhen time>0.4 then k[1]=2; end when;
end SelectedReads;
"#;
    let compiled = compile(source, "SelectedReads");
    let result = simulate_dae_with_diagnostics(
        &compiled,
        &SimOptions {
            t_end: 0.6,
            ..SimOptions::default()
        },
    )
    .unwrap();
    let last = |name: &str| {
        let channel = result
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap();
        *result.data[channel].last().unwrap()
    };
    assert_eq!(last("e[1]"), 1.0);
    assert_eq!(last("e[2]"), 1.0);
    assert_eq!(last("e[3]"), 0.0);
    assert_eq!(last("k[1]"), 2.0);
    assert_eq!(last("k[2]"), 0.0);
}

#[test]
fn matrix_coordinate_mapping_is_row_major_and_preserves_each_hold() {
    let source = "model Matrix Boolean e[2,2](each start=false,each fixed=true); equation when time>0.1 then e[2,1]=true; end when; when time>0.2 then e[1,2]=true; end when; end Matrix;";
    let compiled = compile(source, "Matrix");
    let result = simulate_dae_with_diagnostics(
        &compiled,
        &SimOptions {
            t_end: 0.3,
            ..SimOptions::default()
        },
    )
    .unwrap();
    for (name, expected) in [
        ("e[1,1]", 0.0),
        ("e[1,2]", 1.0),
        ("e[2,1]", 1.0),
        ("e[2,2]", 0.0),
    ] {
        let channel = result
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap();
        assert_eq!(*result.data[channel].last().unwrap(), expected, "{name}");
    }
}

#[test]
fn selected_initial_dependencies_survive_mode_clear_and_refresh_observables() {
    let source = r#"
model SelectedInitial
  Real x(start=0, fixed=true);
  Boolean e[3](each start=false, each fixed=true);
  Real y;
equation
  der(x)=1;
  when initial() then e[1]=x>=0; end when;
  when initial() then e[2]=e[1]; end when;
  y=if initial() then -1 else if e[1] and e[2] then 1 else 0;
end SelectedInitial;
"#;
    let compiled = compile(source, "SelectedInitial");
    let result = simulate_dae_with_diagnostics(
        &compiled,
        &SimOptions {
            t_end: 0.2,
            ..SimOptions::default()
        },
    )
    .unwrap();
    for (name, expected) in [("e[1]", 1.0), ("e[2]", 1.0), ("e[3]", 0.0), ("y", 1.0)] {
        let channel = result
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap();
        assert_eq!(result.data[channel].first(), Some(&expected), "{name}");
        assert!(result.data[channel].iter().all(|&value| value == expected));
    }
}
