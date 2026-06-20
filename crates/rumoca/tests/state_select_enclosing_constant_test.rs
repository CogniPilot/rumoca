//! Regression: a conditional `stateSelect` attribute referencing enclosing
//! package constants (MLS §5.3.2 lexical lookup) must evaluate instead of
//! failing EI032. This is the MSL Media pattern
//! `T(stateSelect = if pT_explicit and preferredMediumStates then ...)`.

use rumoca::Compiler;

const ENCLOSING_CONSTANT_MODEL: &str = r#"
package P
  constant Boolean pT_explicit = true;
  constant Boolean ph_explicit = false;
  model Base
    parameter Boolean preferredMediumStates = false;
    Real T(
      stateSelect = if (pT_explicit or ph_explicit) and preferredMediumStates
        then StateSelect.prefer else StateSelect.default,
      start = 300,
      fixed = true);
  equation
    der(T) = 1;
  end Base;
end P;
model StateSel
  P.Base b(preferredMediumStates = true);
end StateSel;
"#;

#[test]
fn state_select_conditional_on_enclosing_constants_evaluates() {
    let result = Compiler::new()
        .model("StateSel")
        .compile_str(ENCLOSING_CONSTANT_MODEL, "StateSel.mo")
        .expect("conditional stateSelect over enclosing constants must compile");
    assert_eq!(result.dae.variables.states.len(), 1);
}

const QUALIFIED_CONSTANT_MODEL: &str = r#"
package P
  constant Boolean pT_explicit = true;
  model Base
    Real T(
      stateSelect = if P.pT_explicit then StateSelect.prefer else StateSelect.default,
      start = 300,
      fixed = true);
  equation
    der(T) = 1;
  end Base;
end P;
model StateSelQ
  P.Base b;
end StateSelQ;
"#;

#[test]
fn state_select_conditional_on_qualified_class_constant_evaluates() {
    let result = Compiler::new()
        .model("StateSelQ")
        .compile_str(QUALIFIED_CONSTANT_MODEL, "StateSelQ.mo")
        .expect("conditional stateSelect over a qualified class constant must compile");
    assert_eq!(result.dae.variables.states.len(), 1);
}

const OUTER_SYSTEM_ENUM_MODEL: &str = r#"
package Modelica
  package Fluid
    package Types
      type Dynamics = enumeration(DynamicFreeInitial, SteadyState);
    end Types;

    model System
      parameter Modelica.Fluid.Types.Dynamics momentumDynamics =
        Modelica.Fluid.Types.Dynamics.SteadyState;
    end System;

    package Interfaces
      partial model PartialLumpedFlow
        outer Modelica.Fluid.System system;
        Real m_flow(
          stateSelect = if momentumDynamics == Types.Dynamics.SteadyState
            then StateSelect.default else StateSelect.prefer,
          start = 0,
          fixed = true);
        parameter Modelica.Fluid.Types.Dynamics momentumDynamics =
          system.momentumDynamics;
      equation
        der(m_flow) = 1;
      end PartialLumpedFlow;
    end Interfaces;

    package Examples
      model StateSelOuterSystem
        inner Modelica.Fluid.System system;
        extends Modelica.Fluid.Interfaces.PartialLumpedFlow;
      end StateSelOuterSystem;
    end Examples;
  end Fluid;
end Modelica;
"#;

#[test]
fn state_select_conditional_on_outer_system_enum_parameter_evaluates() {
    let result = Compiler::new()
        .model("Modelica.Fluid.Examples.StateSelOuterSystem")
        .compile_str(OUTER_SYSTEM_ENUM_MODEL, "StateSelOuterSystem.mo")
        .expect("outer system enum parameter stateSelect condition must compile");
    assert_eq!(result.dae.variables.states.len(), 1);
}

const FORWARDED_ENUM_MODIFIER_MODEL: &str = r#"
package Types
  type Dynamics = enumeration(DynamicFreeInitial, SteadyState);
end Types;

model FlowModel
  parameter Types.Dynamics momentumDynamics = Types.Dynamics.DynamicFreeInitial;
  Real m_flow(
    stateSelect = if momentumDynamics == Types.Dynamics.SteadyState
      then StateSelect.default else StateSelect.prefer,
    start = 0,
    fixed = true);
equation
  der(m_flow) = 1;
end FlowModel;

model StateSelForward
  parameter Types.Dynamics momentumDynamics = Types.Dynamics.SteadyState;
  FlowModel flowModel(final momentumDynamics = momentumDynamics);
end StateSelForward;
"#;

#[test]
fn state_select_conditional_uses_forwarded_enum_modifier_parent_scope() {
    let result = Compiler::new()
        .model("StateSelForward")
        .compile_str(FORWARDED_ENUM_MODIFIER_MODEL, "StateSelForward.mo")
        .expect("same-name forwarded enum modifier must not shadow declaration evaluation");
    assert_eq!(result.dae.variables.states.len(), 1);
}

const REDECLARED_MEDIUM_CONSTANT_MODEL: &str = r#"
partial package BaseMedium
  constant Boolean ph_explicit;
  constant Boolean pT_explicit;
  constant Integer nXi = if pT_explicit then 1 else 2;

  model BaseProperties
    parameter Boolean preferredMediumStates = false;
    Real Xi[nXi](each start = 0, each fixed = true);
    Real p(
      stateSelect = if (pT_explicit or ph_explicit) and preferredMediumStates
        then StateSelect.prefer else StateSelect.default,
      start = 1,
      fixed = true);
  equation
    der(Xi[1]) = 1;
    der(p) = 1;
  end BaseProperties;
end BaseMedium;

package ConcreteMedium
  extends BaseMedium(final ph_explicit = false, final pT_explicit = true);
end ConcreteMedium;

partial model PartialUseMedium
  replaceable package Medium = BaseMedium;
  Medium.BaseProperties medium(preferredMediumStates = true);
end PartialUseMedium;

model StateSelRedeclaredMedium
  extends PartialUseMedium(redeclare package Medium = ConcreteMedium);
end StateSelRedeclaredMedium;
"#;

#[test]
fn state_select_conditional_uses_redeclared_medium_constants() {
    let result = Compiler::new()
        .model("StateSelRedeclaredMedium")
        .compile_str(
            REDECLARED_MEDIUM_CONSTANT_MODEL,
            "StateSelRedeclaredMedium.mo",
        )
        .expect("stateSelect condition must evaluate constants from redeclared Medium");
    assert_eq!(result.dae.variables.states.len(), 2);
}
