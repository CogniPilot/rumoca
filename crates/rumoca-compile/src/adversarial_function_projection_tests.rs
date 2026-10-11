use crate::{Session, SessionConfig};

fn compile(source: &str) {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("Probe.mo", source)
        .expect("the model parses");
    session
        .compile_model("Probe")
        .expect("the numeric function constructs checked DAE");
}

#[test]
fn mixed_numeric_cross_arguments_are_promoted_to_the_real_builtin_signature() {
    compile(
        r#"
function f
  input Real u;
  output Real y;
protected
  Real c[3];
algorithm
  c := cross({u,2,3},{4,5,6});
  y := sum(c);
end f;
model Probe
  Real x(start=1,fixed=true);
equation
  der(x)=f(x);
end Probe;
"#,
    );
}

#[test]
fn integer_vectors_are_promoted_for_cross_and_skew() {
    compile(
        r#"
function f
  input Real u;
  output Real y;
protected
  Real c[3];
  Real s[3,3];
algorithm
  c := cross({1,2,3},{4,5,6});
  s := skew({1,2,3});
  y := u + sum(c) + sum(s*{1,2,3});
end f;
model Probe
  Real x(start=1,fixed=true);
equation
  der(x)=f(x);
end Probe;
"#,
    );
}

#[test]
fn finite_real_fold_with_a_prefix_break_constructs_checked_dae() {
    compile(
        r#"
function f
  input Real u;
  output Real y;
algorithm
  y := 0;
  for i in 1:5 loop
    if i == 4 then
      break;
    end if;
    y := y + u + i;
  end for;
end f;
model Probe
  Real x(start=1,fixed=true);
equation
  der(x)=f(x);
end Probe;
"#,
    );
}
