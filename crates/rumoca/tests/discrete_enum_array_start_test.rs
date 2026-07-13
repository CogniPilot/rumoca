//! Regression for array-valued enumeration starts crossing the DAE -> Solve boundary.

use rumoca::Compiler;
use rumoca_core::{BuiltinFunction, Expression};
use rumoca_phase_solve::lower_dae_to_solve_model;

const ENUM_ARRAY_START_MODEL: &str = r#"
within;
type Logic = enumeration(U, X);

model Register
  parameter Integer n = 1;
  Logic nextstate[n](start = fill(Logic.U, n));
equation
  when time >= 1 then
    nextstate = pre(nextstate);
  end when;
end Register;

model EnumArrayStart
  Register register(n = 2);
end EnumArrayStart;
"#;

#[test]
fn dae_to_solve_preserves_filled_enum_array_start_for_generated_pre_variable() {
    let compiled = Compiler::new()
        .model("EnumArrayStart")
        .compile_str(ENUM_ARRAY_START_MODEL, "EnumArrayStart.mo")
        .expect("enum array model should compile to DAE");

    let n = compiled
        .flat
        .variables
        .get(&rumoca_core::VarName::new("register.n"))
        .expect("flattening should retain the modified instance parameter");
    assert!(
        matches!(
            n.binding,
            Some(Expression::Literal {
                value: rumoca_core::Literal::Integer(2),
                ..
            })
        ),
        "the flattened instance parameter must keep its modification: {:?}",
        n.binding
    );

    let nextstate = compiled
        .dae
        .variables
        .discrete_valued
        .get(&rumoca_core::VarName::new("register.nextstate"))
        .expect("DAE should retain the declared enum array");
    assert_eq!(nextstate.dims, [2]);
    assert!(
        matches!(
            nextstate.start,
            Some(Expression::BuiltinCall {
                function: BuiltinFunction::Fill,
                ref args,
                ..
            }) if matches!(
                args.as_slice(),
                [_, Expression::Literal {
                    value: rumoca_core::Literal::Integer(2),
                    ..
                }]
            )
        ),
        "the instance parameter modification must reach the start expression: {:?}",
        nextstate.start
    );

    let pre_nextstate = compiled
        .dae
        .variables
        .parameters
        .get(&rumoca_core::VarName::new("__pre__.register.nextstate"))
        .expect("pre(nextstate) should generate an explicit DAE parameter");
    assert_eq!(pre_nextstate.dims, [2]);
    assert!(
        matches!(
            pre_nextstate.start,
            Some(Expression::BuiltinCall {
                function: BuiltinFunction::Fill,
                ref args,
                ..
            }) if matches!(
                args.as_slice(),
                [_, Expression::Literal {
                    value: rumoca_core::Literal::Integer(2),
                    ..
                }]
            )
        ),
        "the generated pre parameter must preserve the shaped start"
    );

    let prepared = lower_dae_to_solve_model(&compiled.dae)
        .expect("filled enum array starts should preserve both values in Solve lowering");
    for name in [
        "register.nextstate[1]",
        "register.nextstate[2]",
        "__pre__.register.nextstate[1]",
        "__pre__.register.nextstate[2]",
    ] {
        let rumoca_ir_solve::ScalarSlot::P { index, .. } = prepared
            .problem
            .layout
            .binding(name)
            .unwrap_or_else(|| panic!("{name} should have a Solve parameter slot"))
        else {
            panic!("{name} should lower to a Solve parameter slot");
        };
        assert_eq!(prepared.parameters[index], 1.0, "wrong start for {name}");
    }
}
