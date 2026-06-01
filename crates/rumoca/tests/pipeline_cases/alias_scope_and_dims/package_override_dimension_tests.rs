use super::*;

/// MLS §7.3 + §12.4: component-level redeclare values can be function calls
/// carrying modifier actuals. The active instance must rewrite inherited
/// replaceable-function calls to the concrete function and preserve those
/// modifier actuals as named call arguments.
#[test]
fn test_component_redeclare_function_call_rewrites_inherited_calls() {
    let source = r#"
package Characteristics
  partial function baseFlow
    input Real x;
    input Real scale = 1;
    output Real y;
  end baseFlow;

  function quadraticFlow
    extends baseFlow;
  algorithm
    y := scale*x*x;
  end quadraticFlow;
end Characteristics;

model PartialPump
  replaceable function flowCharacteristic = Characteristics.baseFlow;
  parameter Real delta = flowCharacteristic(1);
  Real head;
equation
  head = flowCharacteristic(2);
end PartialPump;

model PrescribedPump
  extends PartialPump;
end PrescribedPump;

model Probe
  PrescribedPump pump(
    redeclare function flowCharacteristic =
      Characteristics.quadraticFlow(scale=3));
  Real y = pump.head;
end Probe;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session.compile_model("Probe").expect("compile failed");
    let flat_debug = format!("{:?}", result.flat);
    assert!(
        flat_debug.contains("Characteristics.quadraticFlow"),
        "component redeclare should rewrite inherited calls to concrete function; flat={flat_debug}"
    );
    assert!(
        !flat_debug.contains("PartialPump.flowCharacteristic"),
        "partial replaceable function call must not remain in flat IR; flat={flat_debug}"
    );
    assert!(
        flat_debug.contains("__rumoca_named_arg__.scale"),
        "component redeclare modifier actual should be appended to rewritten calls; flat={flat_debug}"
    );
}

/// MLS §7.3 + §12.4: modifier actuals on a function redeclared in an extends
/// clause are evaluated in the extending class instance, not in the inherited
/// partial base class where the replaceable function is called.
#[test]
fn test_extends_redeclare_function_modifier_actual_uses_extending_instance_scope() {
    let source = r#"
package Modelica
  package Fluid
    package Machines
      package BaseClasses
        package PumpCharacteristics
          partial function baseFlow
            input Real x;
            output Real y;
          end baseFlow;

          function quadraticFlow
            extends baseFlow;
            input Real V_flow_nominal[3];
            input Real head_nominal[3];
          algorithm
            y := V_flow_nominal[2]*x + head_nominal[2];
          end quadraticFlow;
        end PumpCharacteristics;

        model PartialPump
          replaceable function flowCharacteristic =
            PumpCharacteristics.baseFlow;
          parameter Real delta =
            flowCharacteristic(1.1) - flowCharacteristic(1.0);
          Real head;
        equation
          head = flowCharacteristic(2);
        end PartialPump;
      end BaseClasses;

      model ControlledPump
        extends Modelica.Fluid.Machines.BaseClasses.PartialPump(
          redeclare function flowCharacteristic =
            Modelica.Fluid.Machines.BaseClasses.PumpCharacteristics.quadraticFlow(
              V_flow_nominal={0, V_flow_op, 1.5*V_flow_op},
              head_nominal={2*head_op, head_op, 0}));

        parameter Real V_flow_op = 2;
        parameter Real head_op = 3;
      end ControlledPump;
    end Machines;

    package Examples
      model InverseParameterization
        Modelica.Fluid.Machines.ControlledPump pump;
        Real y = pump.head;
      end InverseParameterization;
    end Examples;
  end Fluid;
end Modelica;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session
        .compile_model("Modelica.Fluid.Examples.InverseParameterization")
        .expect("compile failed");
    let dae_debug = format!("{:?}", result.dae);
    assert!(
        dae_debug.contains("pump.V_flow_op"),
        "redeclare function modifier actual should be scoped to the pump instance; dae={dae_debug}"
    );
    assert!(
        !dae_debug.contains("PartialPump.V_flow_op"),
        "partial-base qualifier must not leak into modifier actuals; dae={dae_debug}"
    );
}

/// MLS §13.2: a qualified import of a package makes its final identifier
/// visible in the importing class scope. Function calls in package constant
/// bindings must keep that import when the package is used through a medium
/// alias.
#[test]
fn test_package_constant_binding_uses_imported_package_function() {
    let source = r#"
package Modelica
  package Math
    package Polynomials
      function fitting
        input Real u[:];
        input Real y[size(u, 1)];
        input Integer n;
        output Real p[n + 1];
      algorithm
        p := fill(y[1], n + 1);
      end fitting;
    end Polynomials;
  end Math;

  package Media
    package Incompressible
      package TableBased
        import Modelica.Math.Polynomials;

        final constant Real poly[:] =
          Polynomials.fitting({1, 2}, {3, 4}, 1);

        function h_T
          input Real T;
          output Real h;
        algorithm
          h := poly[1]*T;
        end h_T;
      end TableBased;

      package Glycol47
        extends TableBased;
      end Glycol47;
    end Incompressible;
  end Media;
end Modelica;

model Probe
  replaceable package Medium = Modelica.Media.Incompressible.Glycol47;
  parameter Real h = Medium.h_T(2);
  parameter Real p = Medium.poly[1];
end Probe;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session.compile_model("Probe").expect("compile failed");
    let dae_debug = format!("{:?}", result.dae);
    assert!(
        dae_debug.contains("Modelica.Math.Polynomials.fitting"),
        "imported package function should be fully qualified; dae={dae_debug}"
    );
    assert!(
        !dae_debug.contains("VarName(\"Polynomials.fitting\")"),
        "short imported package call must not leak into DAE; dae={dae_debug}"
    );
}

/// MLS §7.3: a component with a single package redeclare override must expose
/// that package's constants in component scope, even when the component type is
/// fully qualified (e.g. `Modelica.Fluid.Sources.Boundary_pT` style).
#[test]
fn test_single_package_override_applies_to_fully_qualified_component_type_scope() {
    let source = r#"
package PartialMedium
  constant Integer nX = 1;
  constant Integer nXi = 0;

  replaceable partial model BaseProperties
    Real Xi[nXi];
    Real X[nX];
  equation
    X[nX] = 1;
  end BaseProperties;
end PartialMedium;

package MixMedium
  extends PartialMedium(
    nX=2,
    nXi=1);

  redeclare model extends BaseProperties
  end BaseProperties;
end MixMedium;

package Sources
  model Boundary_pT
    replaceable package Medium = PartialMedium;
    Medium.BaseProperties medium;
  equation
    medium.Xi[1] = 0.5;
  end Boundary_pT;
end Sources;

model Top
  Sources.Boundary_pT src(redeclare package Medium = MixMedium);
equation
  src.medium.X[1] = src.medium.Xi[1];
end Top;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session.compile_model("Top").expect("compile failed");

    let xi_dims = result
        .flat
        .variables
        .iter()
        .find(|(name, _)| name.as_str() == "src.medium.Xi")
        .map(|(_, var)| var.dims.clone())
        .expect("src.medium.Xi should exist");
    assert_eq!(
        xi_dims,
        vec![1],
        "src.medium.Xi should use nXi from src's redeclared Medium package"
    );

    let x_dims = result
        .flat
        .variables
        .iter()
        .find(|(name, _)| name.as_str() == "src.medium.X")
        .map(|(_, var)| var.dims.clone())
        .expect("src.medium.X should exist");
    assert_eq!(
        x_dims,
        vec![2],
        "src.medium.X should use nX from src's redeclared Medium package"
    );
}

/// MLS §7.3 + §10.1: dimension lookup for nested members must use the
/// enclosing component's package-alias scope even when that component has
/// multiple class/model overrides.
#[test]
fn test_package_override_dimension_scope_with_multiple_class_overrides() {
    let source = r#"
package PartialMedium
  constant Integer nX = 1;
  constant Integer nXi = 0;

  replaceable partial model BaseProperties
    Real Xi[nXi];
    Real X[nX];
  equation
    X[nX] = 1;
  end BaseProperties;
end PartialMedium;

package MixMedium
  extends PartialMedium(
    nX=2,
    nXi=1);

  redeclare model extends BaseProperties
  end BaseProperties;
end MixMedium;

package Heat
  partial model PartialHeatTransfer
    Real q;
  equation
    q = 0;
  end PartialHeatTransfer;

  model IdealHeatTransfer
    extends PartialHeatTransfer;
  end IdealHeatTransfer;
end Heat;

package Sources
  model Boundary_pT
    replaceable package Medium = PartialMedium;
    replaceable model HeatTransfer = Heat.PartialHeatTransfer;
    Medium.BaseProperties medium;
    HeatTransfer heatTransfer;
  equation
    medium.Xi[1] = heatTransfer.q;
  end Boundary_pT;
end Sources;

model Top
  Sources.Boundary_pT src(
    redeclare package Medium = MixMedium,
    redeclare model HeatTransfer = Heat.IdealHeatTransfer);
equation
  src.medium.X[1] = src.medium.Xi[1];
end Top;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session.compile_model("Top").expect("compile failed");

    let xi_dims = result
        .flat
        .variables
        .iter()
        .find(|(name, _)| name.as_str() == "src.medium.Xi")
        .map(|(_, var)| var.dims.clone())
        .expect("src.medium.Xi should exist");
    assert_eq!(xi_dims, vec![1]);

    let x_dims = result
        .flat
        .variables
        .iter()
        .find(|(name, _)| name.as_str() == "src.medium.X")
        .map(|(_, var)| var.dims.clone())
        .expect("src.medium.X should exist");
    assert_eq!(x_dims, vec![2]);
}

/// MLS §7.1 + §10.1: dimensions on inherited members must evaluate against
/// the concrete component instance, so sibling structural parameters introduced
/// by the same base class are visible with the active modifications.
#[test]
fn test_inherited_member_dimensions_use_instance_structural_parameters() {
    let source = r#"
package Types
  type ModelStructure = enumeration(a_v_b, a_vb, av_b, av_vb);
end Types;

partial model PartialDistributedVolume
  parameter Integer n = 2;
end PartialDistributedVolume;

partial model PartialTwoPortFlow
  import Types.ModelStructure;
  extends PartialDistributedVolume(final n = nNodes);

  parameter Integer nNodes(min = 1) = 2;
  parameter ModelStructure modelStructure = ModelStructure.av_vb;
  parameter Boolean useLumpedPressure = false;
  final parameter Integer nFM = if useLumpedPressure then nFMLumped else nFMDistributed;
  final parameter Integer nFMDistributed =
    if modelStructure == ModelStructure.a_v_b then n + 1
    else if modelStructure == ModelStructure.a_vb or modelStructure == ModelStructure.av_b then n
    else n - 1;
  final parameter Integer nFMLumped =
    if modelStructure == ModelStructure.a_v_b then 2 else 1;

protected
  Real pathLengths[nFM];
  Real crossAreasFM[nFM + 1];
end PartialTwoPortFlow;

model DynamicPipe
  extends PartialTwoPortFlow;
equation
  pathLengths = fill(1.0, nFM);
  crossAreasFM = fill(2.0, nFM + 1);
end DynamicPipe;

model Top
  DynamicPipe pipe1(nNodes = 5, modelStructure = Types.ModelStructure.a_v_b);
end Top;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session.compile_model("Top").expect("compile failed");

    let path_lengths_dims = result
        .flat
        .variables
        .iter()
        .find(|(name, _)| name.as_str() == "pipe1.pathLengths")
        .map(|(_, var)| var.dims.clone())
        .expect("pipe1.pathLengths should exist");
    assert_eq!(path_lengths_dims, vec![6]);

    let cross_areas_dims = result
        .flat
        .variables
        .iter()
        .find(|(name, _)| name.as_str() == "pipe1.crossAreasFM")
        .map(|(_, var)| var.dims.clone())
        .expect("pipe1.crossAreasFM should exist");
    assert_eq!(cross_areas_dims, vec![7]);
}

/// MLS §7.3: local package aliases with class modifications (e.g.
/// `package Medium = PureMedium(AbsolutePressure(max=...))`) must preserve the
/// aliased package constants for member model dimensions (`Medium.nX/nXi`).
#[test]
fn test_local_package_alias_with_class_modification_preserves_member_model_dims() {
    let source = r#"
package PartialMedium
  type AbsolutePressure = Real;
  constant Integer nS = 2;
  final constant Integer nX = nS;
  final constant Integer nXi = 0;

  replaceable model BaseProperties
    AbsolutePressure p;
    Real h;
    Real d;
    Real X[nX];
    input Real Xi[nXi];
  equation
    d = p + h;
    X = fill(1.0, nX);
    Xi = fill(0.0, nXi);
  end BaseProperties;
end PartialMedium;

package PureMedium
  extends PartialMedium(nS = 1);
end PureMedium;

model UsesAliasWithModification
  package Medium = PureMedium(AbsolutePressure(max = 1e6));
  Medium.BaseProperties medium;
  Medium.BaseProperties medium2;
equation
  medium.p = 1;
  medium.h = 2;
  medium2.p = 3;
  medium2.h = 4;
end UsesAliasWithModification;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session
        .compile_model("UsesAliasWithModification")
        .expect("compile failed");

    let medium_x_dims = result
        .flat
        .variables
        .iter()
        .find(|(name, _)| name.as_str() == "medium.X")
        .map(|(_, var)| var.dims.clone())
        .expect("medium.X should exist");
    assert_eq!(
        medium_x_dims,
        vec![1],
        "medium.X should use Medium.nX=1 from the aliased PureMedium package"
    );

    let medium2_x_dims = result
        .flat
        .variables
        .iter()
        .find(|(name, _)| name.as_str() == "medium2.X")
        .map(|(_, var)| var.dims.clone())
        .expect("medium2.X should exist");
    assert_eq!(
        medium2_x_dims,
        vec![1],
        "medium2.X should use Medium.nX=1 from the aliased PureMedium package"
    );

    let medium_xi_dims = result
        .flat
        .variables
        .iter()
        .find(|(name, _)| name.as_str() == "medium.Xi")
        .map(|(_, var)| var.dims.clone())
        .expect("medium.Xi should exist");
    assert_eq!(
        medium_xi_dims,
        vec![0],
        "medium.Xi should use Medium.nXi=0 from the aliased PureMedium package"
    );
}

/// MLS §7.3 + §10.1: forwarding redeclares (`redeclare package Medium = Medium`)
/// inside nested components must evaluate dimensions against the enclosing
/// effective package override, not the local default package.
#[test]
fn test_forwarding_package_redeclare_applies_to_nested_stream_dimension() {
    let source = r#"
package P
  package MediumBase
    constant Integer nC = 0;
  end MediumBase;

  package MediumCO2
    extends MediumBase(nC = 1);
  end MediumCO2;

  connector Port
    replaceable package Medium = MediumBase;
    Real p;
    flow Real m_flow;
    stream Real C_outflow[Medium.nC];
  end Port;

  model Source
    replaceable package Medium = MediumBase;
    Port port(redeclare package Medium = Medium);
  equation
    port.p = 0;
    port.C_outflow = fill(0.0, Medium.nC);
  end Source;

  model M
    Source s(redeclare package Medium = MediumCO2);
  end M;
end P;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session.compile_model("P.M").expect("compile failed");

    let dims = result
        .flat
        .variables
        .iter()
        .find(|(name, _)| name.as_str() == "s.port.C_outflow")
        .map(|(_, var)| var.dims.clone())
        .expect("s.port.C_outflow should exist");
    assert_eq!(
        dims,
        vec![1],
        "s.port.C_outflow should use MediumCO2.nC through the forwarding redeclare"
    );
}

/// MLS §7.3: package aliases in sibling models must not leak into the active
/// model's alias resolution. Compiling `Examples.A` should use `A.Medium`.
#[test]
fn test_sibling_model_package_alias_does_not_pollute_active_model_dims() {
    let source = r#"
package PartialMedium
  constant Integer nX = 1;
  replaceable model BaseProperties
    Real p;
    Real h;
    Real d;
    Real X[nX];
  equation
    d = p + h;
    X = fill(1.0, nX);
  end BaseProperties;
end PartialMedium;

package MediumOne
  extends PartialMedium(nX = 1);
end MediumOne;

package MediumTwo
  extends PartialMedium(nX = 2);
end MediumTwo;

package Examples
  model A
    package Medium = MediumOne;
    Medium.BaseProperties medium;
  equation
    medium.p = 1;
    medium.h = 2;
  end A;

  model B
    package Medium = MediumTwo;
    Medium.BaseProperties medium;
  equation
    medium.p = 3;
    medium.h = 4;
  end B;
end Examples;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session.compile_model("Examples.A").expect("compile failed");

    let x_dims = result
        .flat
        .variables
        .iter()
        .find(|(name, _)| name.as_str() == "medium.X")
        .map(|(_, var)| var.dims.clone())
        .expect("medium.X should exist");
    assert_eq!(
        x_dims,
        vec![1],
        "Examples.A.medium.X should use A.Medium (MediumOne.nX=1), not sibling alias values"
    );
}

/// MLS §7.3: inherited package-constant chains (`PartialMedium ->
/// PartialPureSubstance -> PartialSimpleMedium`) must preserve `nS/nX/nXi`
/// when used through a local `package Medium = ...` alias.
#[test]
fn test_local_medium_alias_preserves_partial_pure_substance_constants() {
    let source = r#"
package Interfaces
  partial package PartialMedium
    type SpecificEnthalpy = Real;
    constant Boolean reducedX = true;
    constant Boolean fixedX = false;
    constant String substanceNames[:] = {"single"};
    final constant Integer nS = size(substanceNames, 1);
    constant Integer nX = nS;
    constant Integer nXi = if fixedX then 0 else if reducedX then nS - 1 else nS;
    constant Real reference_X[nX] = fill(1.0 / nX, nX);

    replaceable partial model BaseProperties
      Real p;
      SpecificEnthalpy h;
      Real d;
      Real X[nX];
      input Real Xi[nXi];
    equation
      X = reference_X;
      Xi = X[1:nXi];
      d = p + h;
    end BaseProperties;
  end PartialMedium;

  partial package PartialPureSubstance
    extends PartialMedium(final reducedX = true, final fixedX = true);
  end PartialPureSubstance;

  partial package PartialSimpleMedium
    extends PartialPureSubstance;
  end PartialSimpleMedium;
end Interfaces;

package WaterLike
  extends Interfaces.PartialSimpleMedium;
  redeclare model extends BaseProperties
  end BaseProperties;
end WaterLike;

model UsesLocalMediumAlias
  package Medium = WaterLike(SpecificEnthalpy(max = 1e6));
  Medium.BaseProperties medium;
equation
  medium.p = 1;
  medium.h = 2;
end UsesLocalMediumAlias;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session
        .compile_model("UsesLocalMediumAlias")
        .expect("compile failed");

    let x_dims = result
        .flat
        .variables
        .iter()
        .find(|(name, _)| name.as_str() == "medium.X")
        .map(|(_, var)| var.dims.clone())
        .expect("medium.X should exist");
    assert_eq!(
        x_dims,
        vec![1],
        "medium.X should use nX=1 for PartialPureSubstance aliases"
    );

    let xi_dims = result
        .flat
        .variables
        .iter()
        .find(|(name, _)| name.as_str() == "medium.Xi")
        .map(|(_, var)| var.dims.clone())
        .expect("medium.Xi should exist");
    assert_eq!(
        xi_dims,
        vec![0],
        "medium.Xi should use nXi=0 for PartialPureSubstance aliases"
    );
}

/// MLS §7.3: extends-modification redeclarations inside a package (e.g.
/// `extends PartialMedium(redeclare record ThermodynamicState=...)`) must be
/// visible when resolving dotted member types (`Medium.ThermodynamicState`).
#[test]
fn test_package_extends_redeclare_record_alias_applies_to_dotted_member_type() {
    let source = r#"
package Common
  record BaseProps_Tpoly
    Real T;
    Real p;
  end BaseProps_Tpoly;
end Common;

package Interfaces
  partial package PartialMedium
    replaceable record ThermodynamicState
      Real x;
    end ThermodynamicState;

    replaceable function setState_pTX
      input Real p;
      input Real T;
      output ThermodynamicState state;
    algorithm
      state := ThermodynamicState();
    end setState_pTX;
  end PartialMedium;
end Interfaces;

package TableBased
  extends Interfaces.PartialMedium(
    redeclare record ThermodynamicState = Common.BaseProps_Tpoly
  );

  redeclare function setState_pTX
    input Real p;
    input Real T;
    output ThermodynamicState state;
  algorithm
    state := Common.BaseProps_Tpoly(T=T, p=p);
  end setState_pTX;
end TableBased;

model UsesTableBasedState
  package Medium = TableBased;
  Medium.ThermodynamicState state = Medium.setState_pTX(1, 2);
end UsesTableBasedState;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session
        .compile_model("UsesTableBasedState")
        .expect("compile failed");

    let flat_var_names: Vec<_> = result
        .flat
        .variables
        .keys()
        .map(|k| k.to_string())
        .collect();

    assert!(
        flat_var_names.iter().any(|n| n == "state.T"),
        "state.T should come from redeclared ThermodynamicState; vars={flat_var_names:?}"
    );
    assert!(
        flat_var_names.iter().any(|n| n == "state.p"),
        "state.p should come from redeclared ThermodynamicState; vars={flat_var_names:?}"
    );
    assert!(
        !flat_var_names.iter().any(|n| n == "state.x"),
        "base ThermodynamicState field should be replaced by redeclare; vars={flat_var_names:?}"
    );
}

/// MLS §5.3 + §7.3: model-level `extends(... redeclare package Medium=...)`
/// must override unrelated import aliases, and short member types inside
/// `Medium.BaseProperties` (e.g. `ThermodynamicState state`) must resolve to
/// the redeclared package record.
#[test]
fn test_model_redeclare_package_controls_member_dims_and_short_record_type() {
    let source = r#"
package Common
  record BaseProps_Tpoly
    Real T;
    Real p;
  end BaseProps_Tpoly;
end Common;

package Interfaces
  partial package PartialMedium
    constant Boolean reducedX = true;
    constant Boolean fixedX = false;
    constant String substanceNames[:] = {"single"};
    final constant Integer nS = size(substanceNames, 1);
    constant Integer nX = nS;
    constant Integer nXi = if fixedX then 0 else if reducedX then nS - 1 else nS;
    constant Real reference_X[nX] = fill(1.0 / nX, nX);

    replaceable record ThermodynamicState
      Real x;
    end ThermodynamicState;

    replaceable model BaseProperties
      input Real Xi[nXi];
      Real X[nX];
      ThermodynamicState state;
    equation
      X = reference_X;
      Xi = X[1:nXi];
    end BaseProperties;
  end PartialMedium;
end Interfaces;

package TableBased
  extends Interfaces.PartialMedium(
    final reducedX = true,
    final fixedX = true,
    redeclare record ThermodynamicState = Common.BaseProps_Tpoly
  );

  redeclare model extends BaseProperties
  equation
    state.T = 1;
    state.p = 2;
  end BaseProperties;
end TableBased;

model Base
  replaceable package Medium = Interfaces.PartialMedium;
  Medium.BaseProperties medium;
equation
  medium.Xi = Medium.reference_X[1:Medium.nXi];
end Base;

model Probe
  extends Base(redeclare package Medium = TableBased);
end Probe;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session.compile_model("Probe").expect("compile failed");

    let medium_x_dims = result
        .flat
        .variables
        .iter()
        .find(|(name, _)| name.as_str() == "medium.X")
        .map(|(_, var)| var.dims.clone())
        .expect("medium.X should exist");
    assert_eq!(
        medium_x_dims,
        vec![1],
        "medium.X should use redeclared Medium.nX=1"
    );

    let medium_xi_dims = result
        .flat
        .variables
        .iter()
        .find(|(name, _)| name.as_str() == "medium.Xi")
        .map(|(_, var)| var.dims.clone())
        .expect("medium.Xi should exist");
    assert_eq!(
        medium_xi_dims,
        vec![0],
        "medium.Xi should use redeclared Medium.nXi=0"
    );

    let flat_var_names: Vec<_> = result
        .flat
        .variables
        .keys()
        .map(|k| k.to_string())
        .collect();
    assert!(
        flat_var_names.iter().any(|n| n == "medium.state.T"),
        "medium.state.T should come from redeclared ThermodynamicState; vars={flat_var_names:?}"
    );
    assert!(
        flat_var_names.iter().any(|n| n == "medium.state.p"),
        "medium.state.p should come from redeclared ThermodynamicState; vars={flat_var_names:?}"
    );
    assert!(
        !flat_var_names.iter().any(|n| n == "medium.state.x"),
        "base ThermodynamicState field should be replaced by redeclare; vars={flat_var_names:?}"
    );
}

/// MLS §5.3: a local nested package declaration must shadow import aliases
/// with the same name when evaluating constants used in dimensions.
#[test]
fn test_local_package_shadows_import_alias_for_dimension_constants() {
    let source = r#"
package Interfaces
  partial package PartialMedium
    constant String mediumName = "unset";
    constant String substanceNames[:] = {mediumName};
    final constant Integer nS = size(substanceNames, 1);
    constant Integer nX = nS;
    constant Integer nXi = 0;

    replaceable model BaseProperties
      Real p;
      Real h;
      Real X[nX];
      input Real Xi[nXi];
    equation
      X = fill(1.0, nX);
      Xi = fill(0.0, nXi);
    end BaseProperties;
  end PartialMedium;

  partial package PartialPureSubstance
    extends PartialMedium;
  end PartialPureSubstance;

  partial package PartialSimpleMedium
    extends PartialPureSubstance;
  end PartialSimpleMedium;
end Interfaces;

package MediumTwo
  extends Interfaces.PartialSimpleMedium(
    mediumName = "two",
    substanceNames = {"A", "B"}
  );
end MediumTwo;

package MediumOne
  extends Interfaces.PartialSimpleMedium(
    mediumName = "one",
    substanceNames = {"A"}
  );
end MediumOne;

model Target
  import Medium = MediumTwo;
  package Medium = MediumOne;
  Medium.BaseProperties medium;
equation
  medium.p = 1;
  medium.h = 2;
end Target;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session.compile_model("Target").expect("compile failed");

    let x_dims = result
        .flat
        .variables
        .iter()
        .find(|(name, _)| name.as_str() == "medium.X")
        .map(|(_, var)| var.dims.clone())
        .expect("medium.X should exist");
    assert_eq!(
        x_dims,
        vec![1],
        "local package Medium=MediumOne must shadow import Medium=MediumTwo for nX"
    );
}

/// MLS §7.3 + §12.4.3: functions inherited through a redeclared package must
/// execute in the concrete package context. Calls inside the inherited function
/// body therefore resolve to concrete redeclarations, not to partial base
/// functions with no algorithm body.
#[test]
fn test_redeclared_package_inherited_function_body_uses_concrete_context() {
    let source = r#"
package Interfaces
  partial package PartialMedium
    replaceable record ThermodynamicState
      Real x;
    end ThermodynamicState;

    replaceable partial function setState_pTX
      input Real p;
      input Real T;
      input Real X[:]={};
      output ThermodynamicState state;
    end setState_pTX;

    replaceable partial function specificEntropy
      input ThermodynamicState state;
      output Real s;
    end specificEntropy;

    replaceable function specificEntropy_pTX
      input Real p;
      input Real T;
      input Real X[:]={};
      output Real s;
    algorithm
      s := specificEntropy(setState_pTX(p, T, X));
    end specificEntropy_pTX;
  end PartialMedium;
end Interfaces;

package Concrete
  extends Interfaces.PartialMedium(
    redeclare record ThermodynamicState = State
  );

  record State
    Real p;
    Real T;
  end State;

  redeclare function setState_pTX
    input Real p;
    input Real T;
    input Real X[:]={};
    output ThermodynamicState state;
  algorithm
    state := State(p=p, T=T);
  end setState_pTX;

  redeclare function extends specificEntropy
  algorithm
    s := state.T;
  end specificEntropy;
end Concrete;

model Base
  replaceable package Medium = Interfaces.PartialMedium;
  Real s = Medium.specificEntropy_pTX(1, 2, {});
end Base;

model Probe
  extends Base(redeclare package Medium = Concrete);
end Probe;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session.compile_model("Probe").expect("compile failed");
    let inherited_function = result
        .flat
        .functions
        .get(&rumoca_core::VarName::new("Concrete.specificEntropy_pTX"))
        .expect("inherited function should be collected under concrete package context");
    let inherited_body = format!("{:?}", inherited_function.body);
    assert!(
        inherited_body.contains("Concrete.setState_pTX"),
        "inherited function body should call concrete setState_pTX; body={inherited_body}"
    );
    assert!(
        !inherited_body.contains("Interfaces.PartialMedium.setState_pTX"),
        "inherited function body must not call partial setState_pTX; body={inherited_body}"
    );
    assert!(
        !result
            .flat
            .functions
            .contains_key(&rumoca_core::VarName::new(
                "Interfaces.PartialMedium.specificEntropy_pTX"
            )),
        "unused partial-base function declarations should not be retained in Flat.functions"
    );
}

/// MLS §5.3 + §7.3 + §12.4.3: a local package alias (`package Medium = ...`)
/// is an effective class binding for member model equations and inherited
/// package functions. Enclosing package constants referenced from the member
/// model must resolve in that concrete package, and unused partial-base
/// functions must not leak into the translated Flat function set.
#[test]
fn test_local_package_alias_drives_member_constants_and_inherited_functions() {
    let source = r#"
package Interfaces
  partial package PartialMedium
    constant Integer nS = 1;
    constant Real reference_X[nS] = fill(1.0 / nS, nS);

    replaceable record State
      Real x;
    end State;

    replaceable partial function setState_pTX
      input Real p;
      input Real T;
      input Real X[:] = reference_X;
      output State state;
    end setState_pTX;

    replaceable partial function property
      input State state;
      output Real y;
    end property;

    function property_pTX
      input Real p;
      input Real T;
      input Real X[:] = reference_X;
      output Real y;
    algorithm
      y := property(setState_pTX(p, T, X));
    end property_pTX;

    replaceable model BaseProperties
      input Real Xi[nS];
      Real MM;
    end BaseProperties;
  end PartialMedium;
end Interfaces;

package Concrete
  extends Interfaces.PartialMedium(nS = 2, reference_X = {0.2, 0.8});
  constant Real MMX[2] = {3.0, 4.0};

  redeclare record extends State
    Real p;
  end State;

  redeclare function setState_pTX
    input Real p;
    input Real T;
    input Real X[:] = reference_X;
    output State state;
  algorithm
    state := State(x = sum(X), p = p);
  end setState_pTX;

  redeclare function extends property
  algorithm
    y := state.x;
  end property;

  redeclare model extends BaseProperties
  equation
    MM = 1 / (Xi[1] / MMX[1] + Xi[2] / MMX[2]);
  end BaseProperties;
end Concrete;

model Probe
  package Medium = Concrete;
  Medium.BaseProperties medium;
  Real y = Medium.property_pTX(1, 2, {0.2, 0.8});
equation
  medium.Xi[1] = 0.2;
  medium.Xi[2] = 0.8;
end Probe;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session.compile_model("Probe").expect("compile failed");
    let function_body = result
        .flat
        .functions
        .get(&rumoca_core::VarName::new("Concrete.property_pTX"))
        .map(|func| format!("{:?}", func.body))
        .expect("inherited function should be collected under Concrete");

    assert!(
        function_body.contains("Concrete.setState_pTX"),
        "local package alias should make inherited function call concrete setState_pTX; body={function_body}"
    );
    assert!(
        !function_body.contains("Interfaces.PartialMedium.setState_pTX"),
        "inherited function body must not call partial setState_pTX; body={function_body}"
    );
    assert!(
        !result
            .flat
            .functions
            .contains_key(&rumoca_core::VarName::new(
                "Interfaces.PartialMedium.property_pTX"
            )),
        "unused partial-base inherited function should not remain in Flat.functions"
    );

    let dae_debug = format!("{:?}", result.dae);
    assert!(
        !dae_debug.contains("MMX") && !dae_debug.contains("nS"),
        "member package constants should be resolved before DAE; dae={dae_debug}"
    );
}

/// MLS §5.3 + §7.3: an import alias to a concrete package is an effective
/// package binding for function calls in model-level bindings.
#[test]
fn test_import_package_alias_rewrites_nested_function_calls_in_binding() {
    let source = r#"
package Interfaces
  partial package PartialMedium
    replaceable record State
      Real x;
    end State;

    replaceable partial function setState_pTX
      input Real p;
      input Real T;
      input Real X[:]={};
      output State state;
    end setState_pTX;

    replaceable partial function property
      input State state;
      output Real y;
    end property;
  end PartialMedium;
end Interfaces;

package Concrete
  extends BaseMedium;

end Concrete;

package BaseMedium
  extends Interfaces.PartialMedium;

  redeclare record extends State
    Real p;
  end State;

  redeclare function setState_pTX
    input Real p;
    input Real T;
    input Real X[:]={};
    output State state;
  algorithm
    state := State(x=T, p=p);
  end setState_pTX;

  redeclare function extends property
  algorithm
    y := state.x;
  end property;
end BaseMedium;

model Probe
  import Medium = Concrete;
  Real y = Medium.property(Medium.setState_pTX(1, 2, {}));
end Probe;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session.compile_model("Probe").expect("compile failed");
    let flat_debug = format!("{:?}", result.flat);
    assert!(
        flat_debug.contains("Concrete.setState_pTX"),
        "import alias should rewrite nested call to concrete function; flat={flat_debug}"
    );
    assert!(
        !flat_debug.contains("Interfaces.PartialMedium.setState_pTX"),
        "partial package function must not remain in flat IR; flat={flat_debug}"
    );
}

/// MLS §5.3 + §7.3 + §12.4.3: member model equations inherited through a
/// package alias must use the concrete package context for inherited package
/// functions.
#[test]
fn test_package_alias_member_model_equation_uses_concrete_function_context() {
    let source = r#"
package Interfaces
  partial package PartialMedium
    replaceable record State
      Real x;
    end State;

    replaceable partial function setState_pTX
      input Real p;
      input Real T;
      input Real X[:]={};
      output State state;
    end setState_pTX;

    replaceable partial function property
      input State state;
      output Real y;
    end property;

    function property_pTX
      input Real p;
      input Real T;
      input Real X[:]={};
      output Real y;
    algorithm
      y := property(setState_pTX(p, T, X));
    end property_pTX;

    replaceable model BaseProperties
      Real h;
    equation
      h = property_pTX(1, 2, {});
    end BaseProperties;
  end PartialMedium;
end Interfaces;

package Concrete
  extends Interfaces.PartialMedium;

  redeclare record extends State
    Real p;
  end State;

  redeclare function setState_pTX
    input Real p;
    input Real T;
    input Real X[:]={};
    output State state;
  algorithm
    state := State(x=T, p=p);
  end setState_pTX;

  redeclare function extends property
  algorithm
    y := state.x;
  end property;
end Concrete;

model Probe
  package Medium = Concrete;
  Medium.BaseProperties medium;
end Probe;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let phase_result = session
        .compile_model_phases("Probe")
        .expect("phase compilation should succeed");
    let result = match phase_result {
        rumoca_compile::compile::PhaseResult::Success(result) => result,
        other => panic!("expected successful phase result, got {other:?}"),
    };
    let flat_debug = format!("{:?}", result.flat);
    assert!(
        flat_debug.contains("Concrete.setState_pTX"),
        "member model equation should rewrite inherited call to concrete function; flat={flat_debug}"
    );
    assert!(
        !flat_debug.contains("Interfaces.PartialMedium.setState_pTX"),
        "partial package function must not remain in flat IR; flat={flat_debug}"
    );
}
