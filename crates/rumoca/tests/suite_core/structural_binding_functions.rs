use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
package Sizes
  function successor
    input Integer n;
    output Integer result;
  algorithm
    result := n + 1;
  end successor;
  function count
    input Integer n;
    output Integer result;
  algorithm
    result := successor(n);
  end count;
end Sizes;
model Cell
  import Sizes.count;
  parameter Integer n = 2 annotation(Evaluate=true);
  final parameter Integer width = count(n);
  Real x[width];
equation
  for j in 1:width loop
    x[j] = j;
  end for;
end Cell;
model StructuralBindingFunctions
  Cell first;
  Cell second(n=4);
end StructuralBindingFunctions;
"#;

#[test]
fn retained_binding_functions_and_callees_resolve_structural_loop_bounds() {
    let flat = Compiler::new()
        .model("StructuralBindingFunctions")
        .compile_str_flat(SOURCE, "StructuralBindingFunctions.mo")
        .unwrap();
    for (name, width) in [("first.x", 3), ("second.x", 5)] {
        assert_eq!(
            flat.variables[&rumoca_core::VarName::new(name)].dims,
            [width]
        );
    }
    for (name, width) in [("first.width", 3), ("second.width", 5)] {
        assert!(matches!(
            flat.variables[&rumoca_core::VarName::new(name)].binding,
            Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(value), ..
            }) if value == width
        ));
    }
    assert_eq!(flat.equations.len(), 8);
    assert!(flat.functions.is_empty());
}

const RECURSIVE_SOURCE: &str = r#"
package RecursiveSizes
  function count
    input Integer n;
    output Integer result;
  algorithm
    if n <= 1 then
      result := 1;
    else
      result := count(n - 1) + 1;
    end if;
  end count;
end RecursiveSizes;
model SizedCell
  import RecursiveSizes.count;
  parameter Integer n = 3 annotation(Evaluate=true);
  final parameter Integer width = count(n);
  Real x[width] = fill(1.0, width);
  Real integral(start=0, fixed=true);
equation
  der(integral) = sum(x);
end SizedCell;
model RecursiveStructuralBindings
  SizedCell first;
  SizedCell second(n=5);
end RecursiveStructuralBindings;
"#;

#[test]
fn recursive_structural_bindings_compile_and_simulate_each_instance() {
    let compiled = Compiler::new()
        .model("RecursiveStructuralBindings")
        .compile_str(RECURSIVE_SOURCE, "RecursiveStructuralBindings.mo")
        .unwrap();
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 0.2,
            dt: Some(0.05),
            ..Default::default()
        },
    )
    .unwrap();
    for (prefix, width) in [("first", 3), ("second", 5)] {
        let integral = result
            .names
            .iter()
            .position(|name| name == &format!("{prefix}.integral"))
            .unwrap();
        for (&time, &actual) in result.times.iter().zip(&result.data[integral]) {
            assert!((actual - f64::from(width) * time).abs() < 1e-7);
        }
        for j in 1..=width {
            let index = result
                .names
                .iter()
                .position(|name| name == &format!("{prefix}.x[{j}]"))
                .unwrap();
            assert!(result.data[index].iter().all(|x| (x - 1.0).abs() < 1e-10));
        }
    }
}

const NONINVARIANT_SOURCE: &str = r#"
function twice
  input Real u;
  output Real y;
algorithm
  y := 2*u;
end twice;
impure function sampleValue
  output Real y;
algorithm
  y := 7;
end sampleValue;
model TunableScalarBindings
  parameter Real parent = 0;
  final parameter Real alias = parent;
  final parameter Real value = twice(alias);
  Real x = value;
end TunableScalarBindings;
model UnsettledScalarBindings
  parameter Real parent(fixed=false) = 2 annotation(Evaluate=true);
  final parameter Real value = twice(parent);
  Real x = value;
end UnsettledScalarBindings;
model ImpureScalarBindings
  final parameter Real value = sampleValue();
  Real x = value;
end ImpureScalarBindings;
"#;

#[test]
fn transitive_final_bindings_keep_changeable_parent_overrides() {
    let compiled = Compiler::new()
        .model("TunableScalarBindings")
        .compile_str(NONINVARIANT_SOURCE, "NoninvariantBindings.mo")
        .unwrap();
    assert!(matches!(
        compiled.flat.variables[&rumoca_core::VarName::new("value")].binding,
        Some(rumoca_core::Expression::FunctionCall { .. })
    ));
    for parent in [0.0, 3.0] {
        let result = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                t_end: 0.2,
                dt: Some(0.05),
                param_overrides: vec![("parent".into(), parent)],
                ..Default::default()
            },
        )
        .unwrap();
        let index = result.names.iter().position(|name| name == "x").unwrap();
        assert!(!result.data[index].is_empty());
        assert!(
            result.data[index]
                .iter()
                .all(|x| (x - 2.0 * parent).abs() < 1e-10)
        );
    }
}

#[test]
fn scalar_binding_specialization_retains_unsettled_and_impure_calls() {
    for model in ["UnsettledScalarBindings", "ImpureScalarBindings"] {
        let flat = Compiler::new()
            .model(model)
            .compile_str_flat(NONINVARIANT_SOURCE, "NoninvariantBindings.mo")
            .unwrap();
        assert!(matches!(
            flat.variables[&rumoca_core::VarName::new("value")].binding,
            Some(rumoca_core::Expression::FunctionCall { .. })
        ));
        assert!(!flat.functions.is_empty());
    }
}
