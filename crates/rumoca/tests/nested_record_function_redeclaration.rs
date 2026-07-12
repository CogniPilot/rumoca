//! Regression coverage for nested record-valued function inputs whose inner
//! record type is supplied by a redeclared package.

use rumoca::Compiler;

/// MLS 3.6 §5.6 and §7.3: flattening must use the instantiated class after
/// redeclarations are applied. The executable Flat function signature must
/// therefore carry the concrete nested record fields, not an unresolved type
/// name from the generic package.
#[test]
fn redeclared_package_record_fields_reach_nested_function_inputs() {
    let source = r#"
package P
  partial package PartialRotation
    replaceable record Orientation
      Real interfaceMarker[0];
    end Orientation;

    replaceable function first
      input Orientation element;
      output Real y;
    end first;
  end PartialRotation;

  package Quaternion
    extends PartialRotation;

    redeclare record extends Orientation
      Real q[4];
    end Orientation;

    redeclare function first
      input Orientation element;
      output Real y;
    algorithm
      y := element.q[1];
    end first;
  end Quaternion;

  package Generic
    replaceable package Rotation = Quaternion
      constrainedby PartialRotation;

    record Element
      Real position[3];
      Rotation.Orientation rotation;
    end Element;

    function product
      input Element left;
      output Real y;
    algorithm
      y := Rotation.first(left.rotation);
    end product;
  end Generic;

  package Concrete
    extends Generic(redeclare package Rotation = Quaternion);
  end Concrete;

  model Probe
    parameter Concrete.Element left = Concrete.Element(
      {0.0, 0.0, 0.0}, Quaternion.Orientation({}, {1.0, 0.0, 0.0, 0.0}));
    parameter Real y = Concrete.product(left);
  end Probe;
end P;
"#;

    let compiled = Compiler::new()
        .model("P.Probe")
        .compile_str(source, "nested_record_function_redeclaration.mo")
        .expect("redeclared package record fixture should compile");
    let product = compiled
        .flat
        .functions
        .get(&rumoca_core::VarName::new("P.Concrete.product"))
        .expect("concrete product function should be collected");
    let inputs = product
        .inputs
        .iter()
        .map(|input| (input.name.as_str(), input.type_class.clone()))
        .collect::<Vec<_>>();

    assert_eq!(
        inputs,
        vec![
            ("left_position", None),
            ("left_rotation_interfaceMarker", None),
            ("left_rotation_q", None),
        ],
        "nested concrete record fields must be fully represented in the Flat function signature"
    );
}
