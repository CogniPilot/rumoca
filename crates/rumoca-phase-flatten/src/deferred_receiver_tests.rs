//! Flat-identity proofs for references Resolve deferred across replaceable edges.
//!
//! A deferred reference leaves Resolve carrying a contiguous resolved prefix
//! whose last `DefId` names the instance-dependent edge. Instantiation must
//! consume exactly that identity to select the concrete class and prove the
//! remaining suffix. These tests assert the resulting Flat callable identity;
//! they do not claim to evaluate the function body.

use rumoca_core::{Expression, FunctionInstanceId, Literal, VarName};
use rumoca_ir_ast as ast;
use rumoca_ir_flat as flat;

fn flatten_source(source: &str, model: &str) -> Result<flat::Model, String> {
    let file_name = "<deferred_receiver_tests>";
    let stored = rumoca_phase_parse::parse_to_ast(source, file_name)
        .map_err(|error| format!("parse failed: {error}"))?;
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(file_name, source);
    let resolved = rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree))
        .map_err(|diagnostics| format!("resolve failed: {diagnostics:?}"))?;
    let overlay =
        match rumoca_phase_instantiate::instantiate_model_with_outcome(resolved.inner(), model) {
            rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
            rumoca_phase_instantiate::InstantiationOutcome::NeedsInner {
                missing_inners, ..
            } => {
                return Err(format!("instantiate needs inners: {missing_inners:?}"));
            }
            rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
                return Err(format!("instantiate failed: {error}"));
            }
        };
    let typed = rumoca_phase_typecheck::typecheck_instanced_tree(&resolved, overlay, model)
        .map_err(|error| format!("typecheck failed: {error:?}"))?;
    crate::flatten_typed(typed, crate::FlattenOptions::default())
        .map_err(|error| format!("flatten failed: {error}"))
}

/// The flat binding of `name`, which must be a fully proved function call:
/// every reference part carries a `DefId` and the callable itself carries its
/// resolved flat function identity. Returns the called name and the value of
/// its single real literal argument.
fn resolved_call_binding(model: &flat::Model, name: &str) -> (String, f64, FunctionInstanceId) {
    let variable = model
        .variables
        .get(&VarName::new(name))
        .unwrap_or_else(|| panic!("missing flat variable {name}"));
    let Some(Expression::FunctionCall {
        name: callable,
        args,
        ..
    }) = variable.binding.as_ref()
    else {
        panic!(
            "expected resolved call binding for {name}, got {:?}",
            variable.binding
        );
    };
    let reference = callable
        .component_ref()
        .unwrap_or_else(|| panic!("call binding of {name} must carry a structured identity"));
    for (index, part) in reference.parts().iter().enumerate() {
        assert_ne!(
            part.def_id.index(),
            0,
            "call part {index} of {name} has no proved identity"
        );
    }
    let resolved = callable
        .resolved_function()
        .unwrap_or_else(|| panic!("call binding of {name} must carry its flat function identity"));
    let named_function = model
        .functions
        .get(callable.var_name())
        .unwrap_or_else(|| panic!("missing named flat function {callable}"));
    assert_eq!(
        named_function.instance_id,
        Some(resolved.instance_id),
        "the call proof must identify the exact named flat function instance"
    );
    let [
        Expression::Literal {
            value: Literal::Real(argument),
            ..
        },
    ] = args.as_slice()
    else {
        panic!("expected one real literal argument for {name}, got {args:?}");
    };
    (
        callable.as_str().to_string(),
        *argument,
        resolved.instance_id,
    )
}

fn assert_distinct_selected_bodies(model: &flat::Model) {
    let function_a = model
        .functions
        .get(&VarName::new("LibA.Std.value"))
        .expect("missing LibA selected function body");
    let function_b = model
        .functions
        .get(&VarName::new("LibB.Deep.Std.value"))
        .expect("missing LibB selected function body");

    let [
        rumoca_core::Statement::Assignment {
            value: Expression::VarRef { .. },
            ..
        },
    ] = function_a.body.as_slice()
    else {
        panic!(
            "LibA selected body must retain `y := s`, got {:?}",
            function_a.body
        );
    };
    let [
        rumoca_core::Statement::Assignment {
            value:
                Expression::Binary {
                    op: rumoca_core::OpBinary::Mul,
                    lhs,
                    ..
                },
            ..
        },
    ] = function_b.body.as_slice()
    else {
        panic!(
            "LibB selected body must retain `y := 2 * s`, got {:?}",
            function_b.body
        );
    };
    assert!(
        matches!(
            lhs.as_ref(),
            Expression::Literal {
                value: Literal::Integer(2),
                ..
            }
        ),
        "LibB selected body must retain the factor 2, got {lhs:?}"
    );
}

/// The compressible-liquids shape from the pinned `Modelica.Media`: a static
/// package chain whose replaceable edge sits past the first segment, selected
/// only by its declared default alias. No redeclare exists anywhere, so the
/// concrete class is the alias's own identity, not an override-map entry.
#[test]
fn default_alias_dynamic_middle_segment_reaches_flat_with_exact_identity() {
    let source = r#"
package Lib
  package Water
    package WaterModel
      function density
        input Real s;
        output Real d;
      algorithm
        d := s;
      end density;
    end WaterModel;
    replaceable package StandardWater = WaterModel;
  end Water;
end Lib;
model Use
  Real d = Lib.Water.StandardWater.density(1.0);
end Use;
"#;
    let model = match flatten_source(source, "Use") {
        Ok(model) => model,
        Err(error) => panic!("default-alias deferred call must flatten: {error}"),
    };
    let (callable, argument, _) = resolved_call_binding(&model, "d");
    assert_eq!(callable, "Lib.Water.StandardWater.density");
    assert_eq!(argument, 1.0);
}

/// Two same-spelled replaceable aliases at different nesting depths must keep
/// their own package selections all the way to the flat model. A repair that
/// keys the receiver by spelling rather than by the recorded `DefId` collapses
/// both calls onto one target.
#[test]
fn same_spelled_receivers_select_different_packages_end_to_end() {
    let source = r#"
package LibA
  package ImplA
    function value
      input Real s;
      output Real y;
    algorithm
      y := s;
    end value;
  end ImplA;
  replaceable package Std = ImplA;
end LibA;
package LibB
  package Deep
    package ImplB
      function value
        input Real s;
        output Real y;
      algorithm
        y := 2 * s;
      end value;
    end ImplB;
    replaceable package Std = ImplB;
  end Deep;
end LibB;
model Use
  Real a = LibA.Std.value(1.0);
  Real b = LibB.Deep.Std.value(3.0);
end Use;
"#;
    let model = match flatten_source(source, "Use") {
        Ok(model) => model,
        Err(error) => panic!("same-spelled deferred receivers must flatten: {error}"),
    };
    let (callable_a, argument_a, instance_a) = resolved_call_binding(&model, "a");
    let (callable_b, argument_b, instance_b) = resolved_call_binding(&model, "b");
    assert_eq!(callable_a, "LibA.Std.value");
    assert_eq!(callable_b, "LibB.Deep.Std.value");
    assert_eq!(argument_a, 1.0);
    assert_eq!(argument_b, 3.0);
    assert_ne!(
        instance_a, instance_b,
        "same-spelled receivers must select distinct flat function instances"
    );
    assert_distinct_selected_bodies(&model);
}
