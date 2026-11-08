//! MLS §§11 and 12: comprehensions read the selected function state while
//! keeping their point-dependent values and recursive invocations separate.

use rumoca::Compiler;
use rumoca_ir_dae::ExpressionOperation;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

fn check(source: &str, expected: &[(&str, f64)]) {
    let compiled = Compiler::new()
        .model("Control")
        .compile_str(source, "comprehension-state.mo")
        .expect("the model compiles");
    assert!(
        compiled.dae.inspect(|view| {
            (0..view.expression_count()).any(|index| {
                matches!(
                    view.expression(view.expression_id(index).unwrap())
                        .unwrap()
                        .operation(),
                    ExpressionOperation::FunctionConditionalOutput { .. }
                )
            })
        }),
        "the control must retain an executed conditional, not fold it away"
    );
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 0.02,
            dt: Some(0.01),
            solver_mode: SimSolverMode::RkLike,
            ..Default::default()
        },
    )
    .expect("the parameter function evaluates and the model simulates");
    for (name, value) in expected {
        let index = result.names.iter().position(|item| item == name).unwrap();
        let actual = result.data[index].last().unwrap();
        assert!(
            (actual - value).abs() < 1e-12,
            "{name}: {actual} != {value}"
        );
    }
}

#[test]
fn vector_points_read_the_selected_value_from_each_call() {
    check(
        r#"
function pick
 input Real x;
 output Real y[2];
protected
 constant Real eps=1;
 Real a;
algorithm
 if x<eps then a:=3; else a:=7; end if;
 for i in 1:2 loop y[i]:=a*i; end for;
end pick;
model Control
 parameter Real lo[2]=pick(0);
 parameter Real hi[2]=pick(2);
 Real z[2](each start=0,each fixed=true);
equation
 der(z)=lo+hi;
end Control;
"#,
        &[("z[1]", 0.2), ("z[2]", 0.4)],
    );
}

#[test]
fn nested_matrix_points_keep_both_domain_indices() {
    check(
        r#"
function pick
 input Real x;
 output Real y[2,2];
protected
 constant Real eps=1;
 Real a;
algorithm
 if x<eps then a:=3; else a:=7; end if;
 for i in 1:2 loop
  for j in 1:2 loop y[i,j]:=a*i*j; end for;
 end for;
end pick;
model Control
 parameter Real lo[2,2]=pick(0);
 parameter Real hi[2,2]=pick(2);
 Real z[2,2](each start=0,each fixed=true);
equation
 der(z)=lo+hi;
end Control;
"#,
        &[
            ("z[1,1]", 0.2),
            ("z[1,2]", 0.4),
            ("z[2,1]", 0.4),
            ("z[2,2]", 0.8),
        ],
    );
}

#[test]
fn recursive_calls_keep_their_own_selected_values() {
    check(
        r#"
function pick
 input Integer depth;
 input Real x;
 output Real y[2];
protected
 constant Real eps=1;
 Real a;
 Real child[2];
algorithm
 if x<eps then a:=3; else a:=7; end if;
 if depth>0 then child:=pick(depth-1,x+2); else child:=zeros(2); end if;
 for i in 1:2 loop y[i]:=a*i+child[i]; end for;
end pick;
model Control
 parameter Real lo[2]=pick(1,0);
 parameter Real hi[2]=pick(1,2);
 Real z[2](each start=0,each fixed=true);
equation
 der(z)=lo+hi;
end Control;
"#,
        &[("z[1]", 0.48), ("z[2]", 0.96)],
    );
}

#[test]
fn the_unselected_branch_is_not_evaluated_for_a_comprehension() {
    check(
        r#"
function pick
 input Real x;
 input Integer k;
 input Real v[2];
 output Real y[2];
protected
 constant Real eps=1;
 Real a;
algorithm
 if x<eps then a:=3; else a:=v[k]; end if;
 for i in 1:2 loop y[i]:=a*i; end for;
end pick;
model Control
 parameter Real p[2]=pick(0,3,{4,5});
 Real z[2](each start=0,each fixed=true);
equation
 der(z)=p;
end Control;
"#,
        &[("z[1]", 0.06), ("z[2]", 0.12)],
    );
}
