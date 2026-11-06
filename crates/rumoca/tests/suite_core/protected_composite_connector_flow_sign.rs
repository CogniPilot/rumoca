//! GitHub #598, MLS 3.6 §9.1.2 / §9.2: a member of a composite connector is
//! an outside connector of its component whatever the component's visibility,
//! so its flow variable has the sign -1 in the component's own connection set.
//! A protected instance, or any protected ancestor, must not change that.

use rumoca::Compiler;
use rumoca_ir_flat::EquationOrigin;

const MODEL: &str = r#"
package SubConnectorSign
  connector Flange
    Real phi;
    flow Real tau;
  end Flange;

  connector Composite
    Flange flange;
  end Composite;

  model Inertia
    parameter Real J = 1;
    Flange flange(phi(start = 0, fixed = true));
    Real w(start = 0, fixed = true);
  equation
    der(flange.phi) = w;
    J*der(w) = flange.tau;
  end Inertia;

  model Shaft
    Composite composite;
    Inertia inertia;
  equation
    connect(inertia.flange, composite.flange);
  end Shaft;

  model ConstantTorque
    Flange flange;
  equation
    flange.tau = -1;
  end ConstantTorque;

  model Public
    ConstantTorque source;
    Shaft shaft;
  equation
    connect(source.flange, shaft.composite.flange);
  end Public;

  model Protected
    ConstantTorque source;
  protected
    Shaft shaft;
  equation
    connect(source.flange, shaft.composite.flange);
  end Protected;

  model Holder
    ConstantTorque source;
    Shaft shaft;
  equation
    connect(source.flange, shaft.composite.flange);
  end Holder;

  model NestedProtected "A public shaft inside a protected component"
  protected
    Holder holder;
  end NestedProtected;
end SubConnectorSign;
"#;

/// The flow sum of the shaft's own `connect(inertia.flange, composite.flange)`.
fn shaft_flow_sum(model: &str, shaft: &str) -> String {
    let flat = Compiler::new()
        .model(model)
        .compile_str_flat(MODEL, "SubConnectorSign.mo")
        .expect("the sub-connector fixture compiles through Flat");
    let inertia = format!("{shaft}.inertia.flange.tau");
    let composite = format!("{shaft}.composite.flange.tau");
    let sums: Vec<&String> = flat
        .equations
        .iter()
        .filter_map(|equation| match &equation.origin {
            EquationOrigin::FlowSum { description }
                if description.contains(&inertia) && description.contains(&composite) =>
            {
                Some(description)
            }
            _ => None,
        })
        .collect();
    assert_eq!(
        sums.len(),
        1,
        "one shaft-scope flow sum in {model}: {sums:?}"
    );
    sums[0].clone()
}

fn assert_outside_member_negated(model: &str, shaft: &str) {
    let sum = shaft_flow_sum(model, shaft);
    assert!(
        sum.contains(&format!("-{shaft}.composite.flange.tau")),
        "{model}: the composite member is an outside connector, got {sum}"
    );
    assert!(
        !sum.contains(&format!("-{shaft}.inertia.flange.tau")),
        "{model}: the inertia flange is an inside connector, got {sum}"
    );
}

#[test]
fn public_composite_member_is_an_outside_connector() {
    assert_outside_member_negated("SubConnectorSign.Public", "shaft");
}

#[test]
fn protected_composite_member_is_an_outside_connector() {
    assert_outside_member_negated("SubConnectorSign.Protected", "shaft");
}

#[test]
fn composite_member_below_a_protected_ancestor_is_an_outside_connector() {
    assert_outside_member_negated("SubConnectorSign.NestedProtected", "holder.shaft");
}
