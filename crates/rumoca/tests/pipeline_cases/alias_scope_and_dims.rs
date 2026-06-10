use super::*;

mod package_override_dimension_tests;
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
        rumoca_phase_dae::balance::is_balanced(&result.dae),
        "Model should remain balanced: {}",
        rumoca_phase_dae::balance::balance_detail(&result.dae)
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
        .continuous
        .equations
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
    let function_or_model = result
        .dae
        .symbols
        .functions
        .iter()
        .find(|(name, _)| name.to_string() == "P.Common.SingleGasNasa.getMM")
        .map(|(_, func)| format!("{:?}", func.body))
        .unwrap_or_else(|| format!("{:?}", result.dae));

    assert!(
        function_or_model.contains("0.018"),
        "expected base-qualified SingleGasNasa.data.MM to fold to 0.018, body={function_or_model}"
    );
    assert!(
        !function_or_model.contains("SingleGasNasa.data"),
        "base-qualified data reference must not remain unresolved, body={function_or_model}"
    );
}

/// MLS §5.3 + §7.3: short class definitions are class aliases in their
/// enclosing scope. A sibling function must resolve `iter.delp` through the
/// aliased record class, without relying on function inlining.
#[test]
fn test_function_resolves_constants_through_record_short_class_alias() {
    let source = r#"
package P
  package Medium
    package Inverses
      record accuracy
        constant Real delp = 0.1;
      end accuracy;
    end Inverses;

    record iter = Inverses.accuracy;

    function pressureTolerance
      output Real y;
    algorithm
      y := iter.delp;
    end pressureTolerance;
  end Medium;

  model Example
    Real y = Medium.pressureTolerance();
  end Example;
end P;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session.compile_model("P.Example").expect("compile failed");
    let body = result
        .dae
        .symbols
        .functions
        .iter()
        .find(|(name, _)| name.to_string() == "P.Medium.pressureTolerance")
        .map(|(_, func)| format!("{:?}", func.body))
        .unwrap_or_else(|| format!("{:?}", result.dae));

    assert!(
        body.contains("0.1"),
        "expected iter.delp to fold through record alias, body={body}"
    );
    assert!(
        !body.contains("iter.delp"),
        "record short-class alias reference must not remain unresolved, body={body}"
    );
}

/// MLS §5.3 + §12.4.1: default expressions on function inputs use the
/// function declaration scope. A sibling package constant must resolve without
/// requiring the call site to pass the argument explicitly.
#[test]
fn test_function_input_default_resolves_sibling_package_constant() {
    let source = r#"
package P
  package Functions
    constant Boolean flag = true;
  end Functions;

  package Medium
    function f
      input Boolean b = Functions.flag;
      output Real y;
    algorithm
      y := if b then 1 else 0;
    end f;
  end Medium;

  model Example
    Real y = Medium.f();
  end Example;
end P;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session.compile_model("P.Example").expect("compile failed");
    let body = result
        .dae
        .symbols
        .functions
        .iter()
        .find(|(name, _)| name.to_string() == "P.Medium.f")
        .map(|(_, func)| format!("{:?}", func))
        .unwrap_or_else(|| format!("{:?}", result.dae));

    assert!(
        body.contains("Boolean(true)") || body.contains("true"),
        "expected default Functions.flag to fold to true, body={body}"
    );
    assert!(
        !body.contains("Functions.flag"),
        "default expression should not retain unresolved Functions.flag, body={body}"
    );
}

/// MLS §5.3 + §7.1: inherited package functions keep the lexical lookup
/// context of the defining class. Defaults in a base package function must
/// still resolve sibling package constants when the function is called through
/// an extending package.
#[test]
fn test_inherited_function_default_resolves_defining_sibling_package_constant() {
    let source = r#"
package P
  package Common
    package Functions
      constant Boolean flag = true;
    end Functions;

    partial package Base
      function f
        input Boolean b = Functions.flag;
        output Real y;
      algorithm
        y := if b then 1 else 0;
      end f;
    end Base;
  end Common;

  package Concrete
    extends Common.Base;
  end Concrete;

  model Example
    Real y = Concrete.f();
  end Example;
end P;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session.compile_model("P.Example").expect("compile failed");
    let body = format!("{:?}", result.dae.symbols.functions);
    assert!(
        body.contains("Boolean(true)") || body.contains("true"),
        "expected inherited default Functions.flag to fold to true, body={body}"
    );
    assert!(
        !body.contains("Functions.flag"),
        "inherited default should not retain unresolved Functions.flag, body={body}"
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
        rumoca_phase_dae::balance::is_balanced(&result.dae),
        "Model should be balanced when fixedX=false disables the no-else branch: {}",
        rumoca_phase_dae::balance::balance_detail(&result.dae)
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
            let rumoca_core::Expression::Binary { op, lhs, .. } = &eq.residual else {
                return None;
            };
            if !matches!(op, rumoca_core::OpBinary::Sub) {
                return None;
            }
            if let rumoca_core::Expression::VarRef { name, .. } = lhs.as_ref() {
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
  b.h_in = 2;
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
            let rumoca_core::Expression::Binary { op, lhs, .. } = &eq.residual else {
                return None;
            };
            if !matches!(op, rumoca_core::OpBinary::Sub) {
                return None;
            }
            if let rumoca_core::Expression::VarRef { name, .. } = lhs.as_ref() {
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
            let rumoca_core::Expression::Binary { op, lhs, rhs, .. } = &eq.residual else {
                return None;
            };
            if !matches!(op, rumoca_core::OpBinary::Sub) {
                return None;
            }
            let rumoca_core::Expression::VarRef { name, .. } = lhs.as_ref() else {
                return None;
            };
            (name.to_string() == "b.medium.h").then_some(rhs.as_ref().clone())
        })
        .unwrap_or_else(|| panic!("missing equation for b.medium.h; lhs_vars={lhs_vars:?}"));

    assert!(
        matches!(
            h_rhs,
            rumoca_core::Expression::VarRef { ref name, .. } if name.to_string() == "b.h_in"
        ),
        "expected constant-true branch to flatten as `b.medium.h = b.h_in`, got rhs={h_rhs:?}"
    );

    let has_medium_t_lhs = result.flat.equations.iter().any(|eq| {
        let rumoca_core::Expression::Binary { op, lhs, .. } = &eq.residual else {
            return false;
        };
        if !matches!(op, rumoca_core::OpBinary::Sub) {
            return false;
        }
        matches!(
            lhs.as_ref(),
            rumoca_core::Expression::VarRef { name, .. } if name.to_string() == "b.medium.T"
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
        rumoca_phase_dae::balance::is_balanced(&result.dae),
        "Model should remain balanced: {}",
        rumoca_phase_dae::balance::balance_detail(&result.dae)
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

    let dae_inputs: Vec<_> = result
        .dae
        .variables
        .inputs
        .keys()
        .map(|k| k.to_string())
        .collect();
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
