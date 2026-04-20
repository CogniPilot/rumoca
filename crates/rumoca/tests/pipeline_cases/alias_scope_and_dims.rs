use super::*;

/// MLS §5.3 + §7.2: the right-hand side of a component modifier is resolved in
/// the lexical scope where the modifier occurs, not against the modified field.
#[test]
fn test_same_name_component_modifier_binding_uses_enclosing_parameter() {
    let source = r#"
model TunedComponent
  parameter Real p_start = p_start;
  Real y;
equation
  y = p_start;
end TunedComponent;

model UsesOuterStart
  parameter Real p_start = 3;
  TunedComponent c(p_start = p_start);
  Real y;
equation
  y = c.y;
end UsesOuterStart;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("same-name modifier binding should resolve without ER007");

    let result = session
        .compile_model("UsesOuterStart")
        .expect("compile should succeed");

    let flat_code =
        render_flat_template_with_name(&result.flat, templates::FLAT_MODELICA, "UsesOuterStart")
            .expect("flat rendering should succeed");

    assert!(
        flat_code.contains("parameter Real c.p_start") && flat_code.contains("= p_start;"),
        "component modifier should preserve enclosing parameter reference, got:\n{flat_code}"
    );
}

/// MLS §7.3: extends-clause package redeclarations that forward through a local
/// alias (`redeclare package Medium = Medium`) must resolve using the active
/// modification environment when instantiated through a component modifier.
#[test]
fn test_component_package_redeclare_forwarding_uses_active_mod_env() {
    let source = r#"
package PartialMedium
  replaceable partial model BaseProperties
    Real p;
    Real h;
    Real d;
  equation
    d = p + h;
  end BaseProperties;
end PartialMedium;

package RealMedium
  extends PartialMedium;
  constant String extraPropertiesNames[:] = fill("", 0);

  redeclare model extends BaseProperties
    Real R_s;
  equation
    R_s = d - p;
  end BaseProperties;
end RealMedium;

model ForwardingBase
  replaceable package Medium = PartialMedium;

  model Internal
    replaceable package Medium = PartialMedium;
    Medium.BaseProperties medium;
  equation
    medium.p = 3;
    medium.h = 2;
  end Internal;

  Internal inst(redeclare package Medium = Medium);
end ForwardingBase;

model UsesForwardedMedium
  extends ForwardingBase(redeclare package Medium = RealMedium);
  Real z;
equation
  z = inst.medium.R_s;
end UsesForwardedMedium;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session
        .compile_model("UsesForwardedMedium")
        .expect("compile failed");

    let flat_var_names: Vec<_> = result
        .flat
        .variables
        .keys()
        .map(|k| k.to_string())
        .collect();

    assert!(
        flat_var_names.iter().any(|n| n == "inst.medium.R_s"),
        "inst.medium.R_s should come from redeclared RealMedium.BaseProperties; vars={flat_var_names:?}"
    );

    assert!(
        rumoca_eval_dae::analysis::is_balanced(&result.dae),
        "Model should remain balanced: {}",
        rumoca_eval_dae::analysis::balance_detail(&result.dae)
    );
}

/// MLS §7.2 + §7.3: record constants referenced through extends modifiers must
/// resolve in lexical package scope and propagate through alias fields.
#[test]
fn test_extends_modifier_record_constant_field_is_resolved() {
    let source = r#"
package P
  package Common
    record DataRecord
      Real MM;
      Real R_s;
    end DataRecord;

    package SingleGasesData
      constant DataRecord H2O(
        MM=0.018,
        R_s=8.314/H2O.MM);
    end SingleGasesData;

    partial package SingleGasNasa
      constant DataRecord data;
    end SingleGasNasa;
  end Common;

  package SingleGases
    package H2O
      extends Common.SingleGasNasa(data=Common.SingleGasesData.H2O);
    end H2O;
  end SingleGases;

  model Example
    package Medium = SingleGases.H2O;
    Real mm = Medium.data.MM;
  end Example;
end P;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session.compile_model("P.Example").expect("compile failed");
    let binding_eq = result
        .dae
        .f_x
        .iter()
        .find(|eq| eq.origin.contains("binding equation for mm"))
        .expect("expected binding equation for mm");
    let rhs_debug = format!("{:?}", binding_eq.rhs);
    assert!(
        rhs_debug.contains("0.018"),
        "expected Medium.data.MM to fold to H2O.MM=0.018, rhs={rhs_debug}"
    );
    assert!(
        !rhs_debug.contains("Medium.data.MM"),
        "unresolved Medium.data.MM must not remain in DAE rhs={rhs_debug}"
    );
}

/// MLS §7.2 + §7.3: when extends-chain modifiers bind a base-package constant
/// (e.g. `SingleGasNasa.data`), function bodies that reference the base-qualified
/// symbol must observe the bound value in derived package aliases.
#[test]
fn test_extends_chain_mirrors_base_package_constant_bindings() {
    let source = r#"
package P
  package Common
    record DataRecord
      Real MM;
    end DataRecord;

    package SingleGasesData
      constant DataRecord H2O(MM=0.018);
    end SingleGasesData;

    partial package SingleGasNasa
      constant DataRecord data;

      function getMM
        output Real mm;
      algorithm
        mm := P.Common.SingleGasNasa.data.MM;
      end getMM;
    end SingleGasNasa;
  end Common;

  package SingleGases
    package H2O
      extends Common.SingleGasNasa(data=Common.SingleGasesData.H2O);
    end H2O;
  end SingleGases;

  model Example
    package Medium = SingleGases.H2O;
    Real mm = Medium.getMM();
  end Example;
end P;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session.compile_model("P.Example").expect("compile failed");
    let function_body = result
        .dae
        .functions
        .iter()
        .find(|(name, _)| name.to_string() == "P.Common.SingleGasNasa.getMM")
        .map(|(_, func)| format!("{:?}", func.body))
        .expect("expected function P.Common.SingleGasNasa.getMM in DAE");

    assert!(
        function_body.contains("0.018"),
        "expected base-qualified SingleGasNasa.data.MM to fold to 0.018, body={function_body}"
    );
    assert!(
        !function_body.contains("SingleGasNasa.data"),
        "base-qualified data reference must not remain unresolved, body={function_body}"
    );
}

/// MLS §8.3.4 + §7.3: if-equations without else inside package-member models
/// must use enclosing package constants (e.g., `fixedX`) for branch selection.
/// Otherwise, flattening can introduce a spurious `else 0` equation.
#[test]
fn test_component_scope_enclosing_boolean_constants_select_if_without_else() {
    let source = r#"
package PartialMedium
  constant Boolean fixedX = false;

  replaceable partial model BaseProperties
    Real x;
  equation
    if fixedX then
      x = 1;
    end if;
  end BaseProperties;
end PartialMedium;

package RealMedium
  extends PartialMedium;

  redeclare model extends BaseProperties
  equation
    x = 2;
  end BaseProperties;
end RealMedium;

model UsesMedium
  replaceable package Medium = RealMedium constrainedby PartialMedium;
  Medium.BaseProperties medium;
  Real y;
equation
  y = medium.x;
end UsesMedium;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session.compile_model("UsesMedium").expect("compile failed");

    assert!(
        rumoca_eval_dae::analysis::is_balanced(&result.dae),
        "Model should be balanced when fixedX=false disables the no-else branch: {}",
        rumoca_eval_dae::analysis::balance_detail(&result.dae)
    );
}

/// MLS §7.3 + §8.3.4: Enum constants from replaceable package members (e.g.
/// `Medium.ThermoStates`) must be resolved in component scope for compile-time
/// if-equation branch selection.
#[test]
fn test_component_scope_enum_constants_select_boundary_ph_branch() {
    let source = r#"
package Interfaces
  package Choices
    type IndependentVariables = enumeration(
      pT,
      ph,
      phX);
  end Choices;
end Interfaces;

package PartialMedium
  constant Interfaces.Choices.IndependentVariables ThermoStates = Interfaces.Choices.IndependentVariables.pT;

  replaceable partial model BaseProperties
    input Real h;
    input Real T;
  end BaseProperties;
end PartialMedium;

package WaterMedium
  extends PartialMedium(
    ThermoStates=Interfaces.Choices.IndependentVariables.ph);

  redeclare model extends BaseProperties
  end BaseProperties;
end WaterMedium;

model BoundaryLike
  import Interfaces.Choices.IndependentVariables;
  replaceable package Medium = WaterMedium;
  Medium.BaseProperties medium;
  input Real h_in;
  Real y;
equation
  if Medium.ThermoStates == IndependentVariables.ph or Medium.ThermoStates == IndependentVariables.phX then
    medium.h = h_in;
  else
    medium.T = -1;
  end if;
  y = medium.h;
end BoundaryLike;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session
        .compile_model("BoundaryLike")
        .expect("compile failed");

    let lhs_vars: Vec<String> = result
        .flat
        .equations
        .iter()
        .filter_map(|eq| {
            let rumoca_ir_flat::Expression::Binary { op, lhs, .. } = &eq.residual else {
                return None;
            };
            if !matches!(op, rumoca_ir_core::OpBinary::Sub(_)) {
                return None;
            }
            if let rumoca_ir_flat::Expression::VarRef { name, .. } = lhs.as_ref() {
                Some(name.to_string())
            } else {
                None
            }
        })
        .collect();

    assert!(
        lhs_vars.iter().any(|lhs| lhs == "medium.h"),
        "constant enum branch should select `medium.h = h_in`; lhs_vars={lhs_vars:?}"
    );
    assert!(
        !lhs_vars.iter().any(|lhs| lhs == "medium.T"),
        "else-branch equation `medium.T = -1` should be eliminated; lhs_vars={lhs_vars:?}"
    );
}

/// MLS §7.3 + §8.3.4: component-level package redeclare should specialize
/// package alias constants used in if-equation branch selection.
const COMPONENT_REDECLARE_SPECIALIZES_ALIAS_ENUM_IF_SOURCE: &str = r#"
package Interfaces
  package Choices
    type IndependentVariables = enumeration(
      pT,
      ph,
      phX);
  end Choices;
end Interfaces;

package PartialMedium
  constant Interfaces.Choices.IndependentVariables ThermoStates = Interfaces.Choices.IndependentVariables.pT;

  replaceable partial model BaseProperties
    input Real h;
    input Real T;
    Real y;
  end BaseProperties;
end PartialMedium;

package WaterMedium
  extends PartialMedium(
    ThermoStates=Interfaces.Choices.IndependentVariables.ph);

  redeclare model extends BaseProperties
  equation
    y = h;
  end BaseProperties;
end WaterMedium;

model BoundaryLike
  import Interfaces.Choices.IndependentVariables;
  replaceable package Medium = PartialMedium;
  Medium.BaseProperties medium;
  input Real h_in;
  Real out_h;
equation
  if Medium.ThermoStates == IndependentVariables.ph or Medium.ThermoStates == IndependentVariables.phX then
    medium.h = h_in;
  else
    medium.T = -1;
  end if;
  out_h = medium.y;
end BoundaryLike;

model Top
  BoundaryLike b(redeclare package Medium = WaterMedium);
  Real y;
equation
  y = b.out_h;
end Top;
"#;

#[test]
fn test_component_redeclare_specializes_alias_enum_constants_for_if_branch() {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document(
            "test.mo",
            COMPONENT_REDECLARE_SPECIALIZES_ALIAS_ENUM_IF_SOURCE,
        )
        .expect("parse failed");

    let result = session.compile_model("Top").expect("compile failed");

    let lhs_vars: Vec<String> = result
        .flat
        .equations
        .iter()
        .filter_map(|eq| {
            let rumoca_ir_flat::Expression::Binary { op, lhs, .. } = &eq.residual else {
                return None;
            };
            if !matches!(op, rumoca_ir_core::OpBinary::Sub(_)) {
                return None;
            }
            if let rumoca_ir_flat::Expression::VarRef { name, .. } = lhs.as_ref() {
                Some(name.to_string())
            } else {
                None
            }
        })
        .collect();

    let h_rhs = result
        .flat
        .equations
        .iter()
        .find_map(|eq| {
            let rumoca_ir_flat::Expression::Binary { op, lhs, rhs } = &eq.residual else {
                return None;
            };
            if !matches!(op, rumoca_ir_core::OpBinary::Sub(_)) {
                return None;
            }
            let rumoca_ir_flat::Expression::VarRef { name, .. } = lhs.as_ref() else {
                return None;
            };
            (name.to_string() == "b.medium.h").then_some(rhs.as_ref().clone())
        })
        .unwrap_or_else(|| panic!("missing equation for b.medium.h; lhs_vars={lhs_vars:?}"));

    assert!(
        matches!(
            h_rhs,
            rumoca_ir_flat::Expression::VarRef { ref name, .. } if name.to_string() == "b.h_in"
        ),
        "expected constant-true branch to flatten as `b.medium.h = b.h_in`, got rhs={h_rhs:?}"
    );

    let has_medium_t_lhs = result.flat.equations.iter().any(|eq| {
        let rumoca_ir_flat::Expression::Binary { op, lhs, .. } = &eq.residual else {
            return false;
        };
        if !matches!(op, rumoca_ir_core::OpBinary::Sub(_)) {
            return false;
        }
        matches!(
            lhs.as_ref(),
            rumoca_ir_flat::Expression::VarRef { name, .. } if name.to_string() == "b.medium.T"
        )
    });

    assert!(
        !has_medium_t_lhs,
        "else-branch equation `b.medium.T = -1` should be eliminated"
    );
}

/// MLS §8.3.2 + §7.3: for-equation ranges inside package-member models must
/// resolve constants through the member's package alias mapping (`medium.nXi`
/// should resolve to `Medium.nXi`).
#[test]
fn test_dotted_model_range_constant_resolves_via_package_alias() {
    let source = r#"
package BaseMedium
  constant Integer nXi = 2;

  replaceable partial model BaseProperties
    Real Xi[nXi];
  equation
    for i in 1:nXi loop
      Xi[i] = i;
    end for;
  end BaseProperties;
end BaseMedium;

package OtherPkg
  constant Integer nXi = 7;
end OtherPkg;

package MyMedium
  extends BaseMedium;

  redeclare model extends BaseProperties
  end BaseProperties;
end MyMedium;

model UsesRangeInBaseProperties
  replaceable package Medium = MyMedium;
  replaceable package Noise = OtherPkg;
  Medium.BaseProperties medium;
end UsesRangeInBaseProperties;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session
        .compile_model("UsesRangeInBaseProperties")
        .expect("compile failed");

    let xi_dims = result
        .flat
        .variables
        .iter()
        .find(|(name, _)| name.as_str() == "medium.Xi")
        .map(|(_, var)| var.dims.clone())
        .expect("medium.Xi should exist");
    assert_eq!(
        xi_dims,
        vec![2],
        "for-range and array dimensions should use Medium.nXi=2, not unrelated constants"
    );

    assert!(
        rumoca_eval_dae::analysis::is_balanced(&result.dae),
        "Model should remain balanced: {}",
        rumoca_eval_dae::analysis::balance_detail(&result.dae)
    );
}

/// MLS §7.3: A package redeclaration applied through `extends(...)` must affect
/// dotted member type resolution (e.g., `Medium.ThermodynamicState`).
#[test]
fn test_dotted_record_type_uses_redeclared_package_from_extends() {
    let source = r#"
package PartialMedium
  replaceable record ThermodynamicState
  end ThermodynamicState;
end PartialMedium;

package PartialTwoPhaseMedium
  extends PartialMedium;
  redeclare replaceable record extends ThermodynamicState
    Real phase;
  end ThermodynamicState;
end PartialTwoPhaseMedium;

model PumpMonitoringBase
  replaceable package Medium = PartialMedium;
  input Medium.ThermodynamicState state_in;
  input Medium.ThermodynamicState state;
end PumpMonitoringBase;

model PumpMonitoringNPSH
  extends PumpMonitoringBase(redeclare replaceable package Medium = PartialTwoPhaseMedium);
  Real y;
equation
  y = state_in.phase + state.phase;
end PumpMonitoringNPSH;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session
        .compile_model("PumpMonitoringNPSH")
        .expect("compile failed");

    let flat_var_names: Vec<_> = result
        .flat
        .variables
        .keys()
        .map(|k| k.to_string())
        .collect();

    assert!(
        flat_var_names.iter().any(|n| n == "state.phase"),
        "state.phase should come from the redeclared package record; vars={flat_var_names:?}"
    );
    assert!(
        flat_var_names.iter().any(|n| n == "state_in.phase"),
        "state_in.phase should come from the redeclared package record; vars={flat_var_names:?}"
    );

    let dae_inputs: Vec<_> = result.dae.inputs.keys().map(|k| k.to_string()).collect();
    assert!(
        dae_inputs.iter().any(|n| n == "state.phase"),
        "state.phase should be tracked as an input scalar"
    );
    assert!(
        dae_inputs.iter().any(|n| n == "state_in.phase"),
        "state_in.phase should be tracked as an input scalar"
    );
}

/// MLS §7.2/§7.3: extends-modifier constants must be applied before evaluating
/// inherited dependent constants such as nS/nX/nXi used in array dimensions.
#[test]
fn test_extends_modifier_constants_drive_inherited_array_dimensions() {
    let source = r#"
package PartialMedium
  constant Boolean reducedX = false;
  constant Boolean fixedX = false;
  constant String substanceNames[:] = {"single"};
  final constant Integer nS = size(substanceNames, 1);
  final constant Integer nX = nS;
  final constant Integer nXi = if fixedX then 0 else if reducedX or nS == 1 then nS - 1 else nS;

  replaceable partial model BaseProperties
    input Real Xi[nXi];
    Real X[nX];
    Real state_X[nX];
  equation
    state_X = X;
  end BaseProperties;
end PartialMedium;

package MoistLike
  extends PartialMedium(
    substanceNames={"water","air"},
    final reducedX=true,
    final fixedX=false);

  model DimProbe
    extends BaseProperties;
  equation
    Xi[1] = X[1];
    X[nX] = 1 - Xi[1];
  end DimProbe;
end MoistLike;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session
        .compile_model("MoistLike.DimProbe")
        .expect("compile failed");

    let xi_dims = result
        .flat
        .variables
        .iter()
        .find(|(name, _)| name.as_str() == "Xi")
        .map(|(_, var)| var.dims.clone())
        .expect("Xi variable should exist");
    assert_eq!(
        xi_dims,
        vec![1],
        "Xi should use nXi=1 from extends-modified constants"
    );

    let x_dims = result
        .flat
        .variables
        .iter()
        .find(|(name, _)| name.as_str() == "X")
        .map(|(_, var)| var.dims.clone())
        .expect("X variable should exist");
    assert_eq!(
        x_dims,
        vec![2],
        "X should use nX=2 from extends-modified substanceNames"
    );

    let state_x_dims = result
        .flat
        .variables
        .iter()
        .find(|(name, _)| name.as_str() == "state_X")
        .map(|(_, var)| var.dims.clone())
        .expect("state_X variable should exist");
    assert_eq!(
        state_x_dims,
        vec![2],
        "inherited dimensions should propagate to all dependent declarations"
    );
}

/// MLS §7.3 + §13.2: when an imported package applies extends-modifier
/// constants, full-path references (e.g. `Pkg.reference_X`) must resolve in
/// structural dimension inference without leaf-name heuristics.
#[test]
fn test_import_full_path_constant_from_extends_modifiers_drives_colon_dims() {
    let source = r#"
package BaseMedium
  constant Real reference_X[:] = {1.0};
end BaseMedium;

package DerivedMedium
  extends BaseMedium(reference_X={0.2, 0.8});
end DerivedMedium;

model Probe
  import Medium = DerivedMedium;
  parameter Real X[:] = DerivedMedium.reference_X;
  Real y[size(X, 1)];
equation
  for i in 1:size(X, 1) loop
    y[i] = X[i];
  end for;
end Probe;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session.compile_model("Probe").expect("compile failed");
    let y_dims = result
        .flat
        .variables
        .iter()
        .find(|(name, _)| name.as_str() == "y")
        .map(|(_, var)| var.dims.clone())
        .expect("y variable should exist");
    assert_eq!(
        y_dims,
        vec![2],
        "y should inherit size(X,1)=2 from extends-modified imported constant"
    );
}

/// MLS §7.3: `extends(... redeclare package Medium=...)` must be active while
/// evaluating inherited declarations that reference `Medium.*` constants.
#[test]
fn test_extends_redeclare_package_is_visible_for_inherited_dimensions() {
    let source = r#"
package BaseMedium
  constant Integer nX = 1;
  constant Integer nXi = 0;
  constant Real X_default[nX] = {1.0};
end BaseMedium;

package MixMedium
  extends BaseMedium(
    nX=3,
    nXi=2,
    X_default={0.2,0.3,0.5});
end MixMedium;

package Components
  partial model PartialTest
    replaceable package Medium = BaseMedium;
    parameter Real X_start[Medium.nX] = Medium.X_default;
    Real Xi[Medium.nXi];
  equation
    Xi[1] = X_start[1];
    Xi[2] = X_start[2];
  end PartialTest;
end Components;

model Derived
  extends Components.PartialTest(redeclare package Medium = MixMedium);
end Derived;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session.compile_model("Derived").expect("compile failed");

    let x_start_dims = result
        .flat
        .variables
        .iter()
        .find(|(name, _)| name.as_str() == "X_start")
        .map(|(_, var)| var.dims.clone())
        .expect("X_start should exist");
    assert_eq!(
        x_start_dims,
        vec![3],
        "X_start should use Medium.nX from the extends-redeclared package"
    );

    let xi_dims = result
        .flat
        .variables
        .iter()
        .find(|(name, _)| name.as_str() == "Xi")
        .map(|(_, var)| var.dims.clone())
        .expect("Xi should exist");
    assert_eq!(
        xi_dims,
        vec![2],
        "Xi should use Medium.nXi from the extends-redeclared package"
    );
}

/// MLS §7.1/§7.3: component type names that refer to inherited local classes
/// (e.g., `FlowModel`) must resolve in the effective class scope.
#[test]
fn test_inherited_local_class_type_resolves_for_component_instances() {
    let source = r#"
package FluidLike
  package BaseClasses
    partial model PartialStraightPipe
      replaceable model FlowModel
        Real x;
      end FlowModel;
    end PartialStraightPipe;
  end BaseClasses;

  model StaticPipe
    extends BaseClasses.PartialStraightPipe;
    FlowModel flowModel;
  equation
    flowModel.x = 1;
  end StaticPipe;

  model Probe
    StaticPipe pipe1;
    StaticPipe pipe2;
  end Probe;
end FluidLike;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session
        .compile_model("FluidLike.Probe")
        .expect("compile failed");

    let flat_var_names: Vec<_> = result
        .flat
        .variables
        .keys()
        .map(|name| name.to_string())
        .collect();

    assert!(
        flat_var_names
            .iter()
            .any(|name| name == "pipe1.flowModel.x"),
        "pipe1.flowModel.x should be instantiated from inherited FlowModel local class"
    );
    assert!(
        flat_var_names
            .iter()
            .any(|name| name == "pipe2.flowModel.x"),
        "pipe2.flowModel.x should be instantiated from inherited FlowModel local class"
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

/// MLS §7.3: chained forwarding redeclares inside component instances must keep
/// the active package override when a nested component instantiates
/// `Medium.BaseProperties`.
#[test]
fn test_component_redeclare_chain_instantiates_concrete_baseproperties() {
    let source = r#"
package PartialMedium
  replaceable partial model BaseProperties
    Real p;
    Real h;
    Real d;
  equation
    d = p + h;
  end BaseProperties;
end PartialMedium;

package RealMedium
  extends PartialMedium;

  redeclare replaceable model BaseProperties
    Real p;
    Real h;
    Real d;
    Real marker;
  equation
    d = p + h;
    marker = d - p;
  end BaseProperties;
end RealMedium;

model Volume
  replaceable package Medium = PartialMedium;

  model Balance
    replaceable package Medium = PartialMedium;
    Medium.BaseProperties medium;
  equation
    medium.p = 3;
    medium.h = 2;
  end Balance;

  Balance dynBal(redeclare package Medium = Medium);
end Volume;

partial model MediumCarrier
  replaceable package Medium = PartialMedium;
end MediumCarrier;

partial model PortCarrier
  replaceable package Medium = PartialMedium;
end PortCarrier;

model FanBase
  extends MediumCarrier;
  extends PortCarrier;
  Volume vol(redeclare package Medium = Medium);
end FanBase;

model Top
  package MediumAir = RealMedium(extraPropertiesNames = {"CO2"});

  model AirHandler
    replaceable package MediumAir = PartialMedium;
    FanBase fan(redeclare package Medium = MediumAir);
  end AirHandler;

  AirHandler ahu(redeclare package MediumAir = MediumAir);
  Real y;
equation
  y = ahu.fan.vol.dynBal.medium.marker;
end Top;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session.compile_model("Top").expect("compile failed");

    let flat_var_names: Vec<_> = result
        .flat
        .variables
        .keys()
        .map(|k| k.to_string())
        .collect();
    assert!(
        flat_var_names
            .iter()
            .any(|name| name == "ahu.fan.vol.dynBal.medium.marker"),
        "nested Medium.BaseProperties should use RealMedium, vars={flat_var_names:?}"
    );
}

const INHERITED_VOLUME_MEDIUM_BASEPROPERTIES_SOURCE: &str = r#"
package PartialMedium
  replaceable partial model BaseProperties
    Real p;
    Real h;
  end BaseProperties;
end PartialMedium;

package CondensingMedium
  extends PartialMedium;
end CondensingMedium;

package RealMedium
  extends PartialMedium;

  redeclare replaceable model BaseProperties
    Real p;
    Real h;
    Real marker;
  equation
    marker = p + h;
  end BaseProperties;
end RealMedium;

package Interfaces
  partial model LumpedVolumeDeclarations
    replaceable package Medium = PartialMedium;
  end LumpedVolumeDeclarations;

  model ConservationEquation
    extends LumpedVolumeDeclarations;
    Medium.BaseProperties medium;
  equation
    medium.p = 1;
    medium.h = 2;
  end ConservationEquation;
end Interfaces;

partial model PartialMixingVolume
  extends Interfaces.LumpedVolumeDeclarations;
  Interfaces.ConservationEquation dynBal(redeclare final package Medium = Medium);
end PartialMixingVolume;

model MixingVolume
  extends PartialMixingVolume;
end MixingVolume;

model MixingVolumeHeatPort
  extends PartialMixingVolume;
end MixingVolumeHeatPort;

model MixingVolumeHeatMoisturePort
  extends PartialMixingVolume;
end MixingVolumeHeatMoisturePort;

model FourPortHexBase
  replaceable package Medium1 = PartialMedium;
  replaceable package Medium2 = PartialMedium;
  replaceable MixingVolumeHeatPort vol1 constrainedby
    MixingVolumeHeatPort(redeclare final package Medium = Medium1);
  replaceable MixingVolume vol2 constrainedby
    MixingVolumeHeatPort(redeclare final package Medium = Medium2);
end FourPortHexBase;

model BaseHex
  extends FourPortHexBase;
end BaseHex;

model UsesDefaultConstrainedbyVolume
  extends FourPortHexBase(
    redeclare package Medium1 = RealMedium,
    redeclare package Medium2 = RealMedium);
  Real y;
equation
  y = vol1.dynBal.medium.marker;
end UsesDefaultConstrainedbyVolume;

model LatentHex
  extends BaseHex(
    redeclare final MixingVolumeHeatPort vol1,
    redeclare final MixingVolumeHeatMoisturePort vol2);
end LatentHex;

partial model PartialFourPort
  replaceable package Medium1 = PartialMedium;
  replaceable package Medium2 = PartialMedium;
end PartialFourPort;

model DryCoil
  extends PartialFourPort;
  replaceable model HexElement = BaseHex;
  HexElement ele[1](
    redeclare each package Medium1 = Medium1,
    redeclare each package Medium2 = Medium2);
end DryCoil;

model WetCoil
  extends DryCoil(
    redeclare replaceable package Medium2 = CondensingMedium,
    redeclare model HexElement = LatentHex);
end WetCoil;

model CoilWrapper
  replaceable package MediumAir = PartialMedium;
  replaceable package MediumWat = PartialMedium;
  WetCoil cooCoi(
    redeclare package Medium1 = MediumWat,
    redeclare package Medium2 = MediumAir);
end CoilWrapper;

partial model WatCoil
  replaceable package MediumAir = PartialMedium;
  replaceable package MediumWat = PartialMedium;
end WatCoil;

model CoolingCoil
  extends WatCoil;
  CoilWrapper coi(
    redeclare package MediumAir = MediumAir,
    redeclare package MediumWat = MediumWat);
end CoolingCoil;

model Top
  package MediumWater = RealMedium;
  package MediumAir = RealMedium;
  CoolingCoil cooCoi(
    redeclare package MediumAir = MediumAir,
    redeclare package MediumWat = MediumWater);
  Real y;
equation
  y = cooCoi.coi.cooCoi.ele[1].vol1.dynBal.medium.marker;
end Top;
"#;

/// MLS §7.3: inherited replaceable model arrays must keep active package
/// redeclarations when nested volumes instantiate `Medium.BaseProperties`.
#[test]
fn test_inherited_replaceable_model_array_keeps_active_medium_for_baseproperties() {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", INHERITED_VOLUME_MEDIUM_BASEPROPERTIES_SOURCE)
        .expect("parse failed");

    let direct_result = session
        .compile_model("UsesDefaultConstrainedbyVolume")
        .expect("default constrainedby volume compile failed");

    let direct_var_names: Vec<_> = direct_result
        .flat
        .variables
        .keys()
        .map(|k| k.to_string())
        .collect();
    assert!(
        direct_var_names
            .iter()
            .any(|name| name == "vol1.dynBal.medium.marker"),
        "default replaceable volume should use constrainedby RealMedium; vars={direct_var_names:?}"
    );

    let result = session.compile_model("Top").expect("compile failed");

    let flat_var_names: Vec<_> = result
        .flat
        .variables
        .keys()
        .map(|k| k.to_string())
        .collect();
    assert!(
        flat_var_names
            .iter()
            .any(|name| name == "cooCoi.coi.cooCoi.ele[1].vol1.dynBal.medium.marker"),
        "array element Medium.BaseProperties should use active RealMedium; vars={flat_var_names:?}"
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
