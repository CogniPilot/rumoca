use super::*;

/// MLS §7.3.2: if a replaceable package declaration has no explicit
/// `constrainedby`, the declaration type is its implicit constraining type.
///
/// A later redeclare must be checked against that original constraining type,
/// not against the local alias class introduced by the replaceable declaration.
#[test]
fn test_replaceable_package_implicit_constraint_uses_declared_type() {
    let source = r#"
package Modelica
  package Media
    package Interfaces
      partial package PartialMedium
      end PartialMedium;

      partial package PartialMixtureMedium
        extends PartialMedium;
      end PartialMixtureMedium;

      partial package PartialCondensingGases
        extends PartialMixtureMedium;
      end PartialCondensingGases;
    end Interfaces;
  end Media;
end Modelica;

package Buildings
  package Fluid
    package Interfaces
      partial model PartialFourPort
        replaceable package Medium2 =
          Modelica.Media.Interfaces.PartialMedium;
      end PartialFourPort;

      partial model PartialFourPortInterface
        extends PartialFourPort;
      end PartialFourPortInterface;
    end Interfaces;

    package HeatExchangers
      model DryCoilCounterFlow
        extends Interfaces.PartialFourPortInterface;
      end DryCoilCounterFlow;

      model WetCoilCounterFlow
        extends DryCoilCounterFlow(
          redeclare replaceable package Medium2 =
            Modelica.Media.Interfaces.PartialCondensingGases);
      end WetCoilCounterFlow;
    end HeatExchangers;
  end Fluid;
end Buildings;

model Probe
  Buildings.Fluid.HeatExchangers.WetCoilCounterFlow coil;
end Probe;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    session
        .compile_model("Probe")
        .expect("implicit constraining type should accept PartialCondensingGases");
}

/// MLS §7.3: constant evaluation in nested package aliases must use alias-local scope.
///
/// Without scope-aware lookup, `size(substanceNames, 1)` for `Medium.nS` can resolve to
/// another package alias in the same model, collapsing `Medium.nX/nXi` dimensions.
#[test]
fn test_nested_package_alias_constant_scope_isolation() {
    let source = r#"
model Dummy
end Dummy;

package P
  package Base
    constant Boolean reducedX = false;
    constant Boolean fixedX = false;
    constant String substanceNames[:] = {"single"};
    final constant Integer nS = size(substanceNames, 1);
    final constant Integer nX = nS;
    final constant Integer nXi = if fixedX then 0 else if reducedX or nS == 1 then nS - 1 else nS;
  end Base;

  package One
    extends Base(
      final substanceNames={"single"},
      final reducedX=true,
      final fixedX=false);
  end One;

  package Two
    extends Base(
      final substanceNames={"a","b"},
      final reducedX=true,
      final fixedX=false);
  end Two;

  model Probe
    package Other = One;
    package Medium = Two;
    Real X[Medium.nX];
    Real Xi[Medium.nXi];
  equation
    Xi[1] = 0.5;
    Xi[1] = X[1];
    X[2] = 1 - Xi[1];
  end Probe;
end P;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session.compile_model("P.Probe").expect("compile failed");

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
        "X should use Medium.nX from the Medium alias, not another alias in scope"
    );

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
        "Xi should use Medium.nXi from the Medium alias, not another alias in scope"
    );
}

/// MLS §7.3 + §8.3.4: component equations that use unqualified package constants
/// must resolve those constants from the component's active package alias.
///
/// This regression protects against collisions where another alias in scope defines
/// the same constant names (`fixedX`, `nX`, `nXi`) with different values.
#[test]
fn test_component_active_package_alias_controls_unqualified_constants() {
    let source = r#"
package BaseMedia
  constant Boolean reducedX = false;
  constant Boolean fixedX = false;
  constant String substanceNames[:] = {"single"};
  final constant Integer nS = size(substanceNames, 1);
  final constant Integer nX = nS;
  final constant Integer nXi = if fixedX then 0 else if reducedX or nS == 1 then nS - 1 else nS;

  model BaseProperties
    input Real Xi[nXi];
    Real X[nX];
  equation
    X[1:nXi] = Xi;
    X[nX] = 1 - sum(Xi);
  end BaseProperties;
end BaseMedia;

package OtherMedium
  extends BaseMedia(
    final substanceNames={"single"},
    final reducedX=false,
    final fixedX=true);
end OtherMedium;

package ActiveMedium
  extends BaseMedia(
    final substanceNames={"water","air"},
    final reducedX=true,
    final fixedX=false);
end ActiveMedium;

partial model AliasCollisionProbe
  package Other = OtherMedium;
  package Medium = ActiveMedium;
  Medium.BaseProperties medium;
equation
  medium.Xi[1] = 0.5;
end AliasCollisionProbe;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session
        .compile_model("AliasCollisionProbe")
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
        vec![1],
        "medium.Xi should use Medium.nXi from the active alias, not Other.nXi"
    );

    let x_dims = result
        .flat
        .variables
        .iter()
        .find(|(name, _)| name.as_str() == "medium.X")
        .map(|(_, var)| var.dims.clone())
        .expect("medium.X should exist");
    assert_eq!(
        x_dims,
        vec![2],
        "medium.X should use Medium.nX from the active alias, not Other.nX"
    );
}

/// MLS §7.3 + §10.1: package constants referenced as `Medium.*` inside
/// instance members must resolve in the instance's active package scope.
///
/// With two sibling instances redeclaring `Medium` differently, dimensions
/// must follow each local redeclare (`a.x[1]`, `b.x[2]`) instead of any global
/// suffix fallback.
#[test]
fn test_instance_redeclare_package_constants_resolve_per_instance_scope() {
    let source = r#"
package MediumA
  constant Integer nX = 1;
end MediumA;

package MediumB
  constant Integer nX = 2;
end MediumB;

model Base
  replaceable package Medium = MediumA;
  Real x[Medium.nX];
equation
  for i in 1:Medium.nX loop
    x[i] = i;
  end for;
end Base;

model Probe
  Base a(redeclare package Medium = MediumA);
  Base b(redeclare package Medium = MediumB);
end Probe;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session.compile_model("Probe").expect("compile failed");

    let a_dims = result
        .flat
        .variables
        .iter()
        .find(|(name, _)| name.as_str() == "a.x")
        .map(|(_, var)| var.dims.clone())
        .expect("a.x should exist");
    assert_eq!(a_dims, vec![1], "a.x should use MediumA.nX=1");

    let b_dims = result
        .flat
        .variables
        .iter()
        .find(|(name, _)| name.as_str() == "b.x")
        .map(|(_, var)| var.dims.clone())
        .expect("b.x should exist");
    assert_eq!(b_dims, vec![2], "b.x should use MediumB.nX=2");
}

/// MLS §10.1: explicit array dimensions must converge across multi-pass structural
/// evaluation without a post-hoc correction shim.
///
/// `inner.y[nout]` may initially see outer `nout=2` while `inner.nout=size(table,1)`
/// is still unresolved; later passes must update `inner.y` to the local `nout=3`.
#[test]
fn test_explicit_dimension_recomputes_after_local_parameter_resolves() {
    let source = r#"
model DimPass
  parameter Integer nout = 2;

  model Inner
    parameter Real table[:, 2] = [1, 0; 2, 0; 3, 0];
    parameter Integer nout = size(table, 1);
    Real y[nout];
  equation
    for i in 1:nout loop
      y[i] = i;
    end for;
  end Inner;

  Inner inst;
end DimPass;
"#;

    let stored_def = rumoca_phase_parse::parse_to_ast(source, "test.mo").expect("parse failed");
    let mut tree = rumoca_ir_ast::ClassTree::from_parsed(stored_def);
    tree.source_map.add("test.mo", source);
    let parsed = rumoca_ir_ast::ParsedTree::new(tree);
    let resolved = rumoca_phase_resolve::resolve(parsed).expect("resolve failed");
    let tree = resolved.into_inner();
    let mut overlay =
        match rumoca_phase_instantiate::instantiate_model_with_outcome(&tree, "DimPass") {
            rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
            rumoca_phase_instantiate::InstantiationOutcome::NeedsInner {
                missing_inners, ..
            } => {
                panic!("unexpected NeedsInner: {missing_inners:?}");
            }
            rumoca_phase_instantiate::InstantiationOutcome::Error(e) => {
                panic!("instantiate failed: {e:?}");
            }
        };

    if let Err(diags) = rumoca_phase_typecheck::typecheck_instanced(&tree, &mut overlay, "DimPass")
    {
        let messages: Vec<_> = diags.iter().map(|d| d.message.as_str()).collect();
        panic!("typecheck failed: {messages:?}");
    }

    let y_dims = overlay
        .components
        .values()
        .find(|d| d.qualified_name.to_flat_string() == "inst.y")
        .map(|d| d.dims.clone())
        .expect("inst.y should exist");
    assert_eq!(
        y_dims,
        vec![3],
        "inst.y must use inner.nout=size(table,1)=3 after multi-pass convergence"
    );
}

/// MLS §7.2/§12.6: a base-record component bound to a derived record constructor
/// must project the derived record's effective field defaults before component
/// modifiers use those fields for dimensions.
#[test]
fn test_derived_record_constructor_field_defaults_drive_table_dimensions() {
    let source = r#"
package Hyst
  record BaseData
    parameter Real tabris[:, :] = [0, 0; 1, 1];
  end BaseData;

  record M330_50A
    extends BaseData(tabris = [1, 10; 2, 20; 3, 30]);
  end M330_50A;

  block MIMOs
    parameter Integer n = 1;
    input Real u[n];
    output Real y[n];
  end MIMOs;

  block CombiTable1Dv
    extends MIMOs(final n = size(columns, 1));
    parameter Real table[:, :] = [0, 0; 1, 1];
    parameter Integer columns[:] = 2:size(table, 2);
  end CombiTable1Dv;

  model GenericHystTellinenTable
    parameter BaseData mat = M330_50A();
    CombiTable1Dv tabris(table = mat.tabris);
  end GenericHystTellinenTable;

  model Top
    GenericHystTellinenTable core;
  end Top;
end Hyst;
"#;

    let stored_def = rumoca_phase_parse::parse_to_ast(source, "test.mo").expect("parse failed");
    let mut tree = rumoca_ir_ast::ClassTree::from_parsed(stored_def);
    tree.source_map.add("test.mo", source);
    let parsed = rumoca_ir_ast::ParsedTree::new(tree);
    let resolved = rumoca_phase_resolve::resolve(parsed).expect("resolve failed");
    let tree = resolved.into_inner();
    let mut overlay =
        match rumoca_phase_instantiate::instantiate_model_with_outcome(&tree, "Hyst.Top") {
            rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
            rumoca_phase_instantiate::InstantiationOutcome::NeedsInner {
                missing_inners, ..
            } => {
                panic!("unexpected NeedsInner: {missing_inners:?}");
            }
            rumoca_phase_instantiate::InstantiationOutcome::Error(e) => {
                panic!("instantiate failed: {e:?}");
            }
        };

    if let Err(diags) = rumoca_phase_typecheck::typecheck_instanced(&tree, &mut overlay, "Hyst.Top")
    {
        let messages: Vec<_> = diags.iter().map(|d| d.message.as_str()).collect();
        panic!("typecheck failed: {messages:?}");
    }

    let u_dims = overlay
        .components
        .values()
        .find(|d| d.qualified_name.to_flat_string() == "core.tabris.u")
        .map(|d| d.dims.clone())
        .expect("core.tabris.u should exist");
    assert_eq!(
        u_dims,
        vec![1],
        "table columns 2:size(table,2) from M330_50A.tabris should give one MIMO input"
    );
}

/// MLS §12.4 / §8.3.3: structural for-equation ranges may depend on a
/// parameter computed by an imported pure function and a parent modifier.
#[test]
fn test_imported_function_parameter_drives_nested_connection_range() {
    let source = r#"
package Poly
  package Functions
    function numberOfSymmetricBaseSystems
      input Integer m = 3;
      output Integer n;
    algorithm
      n := 1;
      if mod(m, 2) == 0 then
        if m == 2 then
          n := 1;
        else
          n := n * 2 * numberOfSymmetricBaseSystems(integer(m / 2));
        end if;
      else
        n := 1;
      end if;
    end numberOfSymmetricBaseSystems;
  end Functions;

  connector Pin
    Real v;
    flow Real i;
  end Pin;

  connector Plug
    parameter Integer m = 3;
    Pin pin[m];
  end Plug;

  model MultiStar
    import Poly.Functions.numberOfSymmetricBaseSystems;
    parameter Integer m = 3;
    final parameter Integer mSystems = numberOfSymmetricBaseSystems(m);
    final parameter Integer mBasic = integer(m / mSystems);
    Plug plug_p(final m = m);
    Plug starpoints(final m = mSystems);
  equation
    for k in 1:mSystems loop
      for j in 1:mBasic loop
        connect(plug_p.pin[(k - 1) * mBasic + j], starpoints.pin[k]);
      end for;
    end for;
  end MultiStar;

  model MultiStarResistance
    import Poly.Functions.numberOfSymmetricBaseSystems;
    parameter Integer m = 3;
    final parameter Integer mBasic = numberOfSymmetricBaseSystems(m);
    Plug plug(m = m);
    MultiStar multiStar(m = m);
    Plug resistor(m = mBasic);
  equation
    connect(plug, multiStar.plug_p);
    connect(multiStar.starpoints, resistor);
  end MultiStarResistance;

  record Data
    import Poly.Functions.numberOfSymmetricBaseSystems;
    parameter Integer m = 6;
    parameter Integer mSystems = numberOfSymmetricBaseSystems(m);
    parameter Integer mBasic = integer(m / mSystems);
  end Data;

  model Top
    parameter Data data;
    MultiStarResistance multiStar(m = data.m);
  end Top;
end Poly;
"#;

    let stored_def = rumoca_phase_parse::parse_to_ast(source, "test.mo").expect("parse failed");
    let mut tree = rumoca_ir_ast::ClassTree::from_parsed(stored_def);
    tree.source_map.add("test.mo", source);
    let parsed = rumoca_ir_ast::ParsedTree::new(tree);
    let resolved = rumoca_phase_resolve::resolve(parsed).expect("resolve failed");
    let tree = resolved.into_inner();

    match rumoca_phase_instantiate::instantiate_model_with_outcome(&tree, "Poly.Top") {
        rumoca_phase_instantiate::InstantiationOutcome::Success(_) => {}
        rumoca_phase_instantiate::InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("unexpected NeedsInner: {missing_inners:?}");
        }
        rumoca_phase_instantiate::InstantiationOutcome::Error(e) => {
            panic!("instantiate failed: {e:?}");
        }
    }
}

/// MLS §7.3 + §8.3.2: for-equation ranges using `Medium.nXi` inside an
/// inherited/redeclared component class must resolve in component scope.
#[test]
fn test_component_scope_medium_nxi_resolves_through_redeclare() {
    let source = r#"
package PartialMedium
  constant Integer nXi = 1;
end PartialMedium;

package Medium2
  extends PartialMedium(nXi = 2);
end Medium2;

partial model PartialVolume
  replaceable package Medium = PartialMedium;
  Real Xi[2];
equation
  for i in 1:Medium.nXi loop
    Xi[i] = i;
  end for;
end PartialVolume;

model SweptVolume
  extends PartialVolume(redeclare package Medium = Medium2);
end SweptVolume;

model Top
  SweptVolume sweptVolume;
end Top;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session.compile_model("Top").expect("compile failed");

    assert!(
        rumoca_phase_dae::balance::is_balanced(&result.dae).expect("valid DAE balance fixture"),
        "for-range should expand with Medium.nXi=2 from redeclared package: {}",
        rumoca_phase_dae::balance::balance_detail(&result.dae).expect("valid DAE balance fixture")
    );
}

/// MLS §7.2/§7.3: extends modifiers must update dependent inherited constants
/// before flattening range subscripts in equations (e.g., `1:Medium.nXi`).
#[test]
fn test_extends_modified_dependent_constant_keeps_range_equation() {
    let source = r#"
package BaseMedium
  constant Boolean reducedX = false;
  constant Boolean fixedX = true;
  constant String substanceNames[:] = {"single"};
  final constant Integer nS = size(substanceNames, 1);
  final constant Integer nX = nS;
  final constant Integer nXi = if fixedX then 0 else if reducedX or nS == 1 then nS - 1 else nS;
  constant Real reference_X[nX] = fill(1.0 / nX, nX);
end BaseMedium;

package MoistMedium
  extends BaseMedium(
    final substanceNames={"water","air"},
    final reducedX=true,
    final fixedX=false);
end MoistMedium;

partial model BaseTest
  replaceable package Medium = BaseMedium;

  model Inner
    input Real Xi[Medium.nXi];
    Real z;
  equation
    z = sum(Xi);
  end Inner;

  Inner medium;
equation
  medium.Xi = Medium.reference_X[1:Medium.nXi];
end BaseTest;

model Derived
  extends BaseTest(redeclare package Medium = MoistMedium);
end Derived;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session.compile_model("Derived").expect("compile failed");
    assert!(
        rumoca_phase_dae::balance::is_balanced(&result.dae).expect("valid DAE balance fixture"),
        "equation with 1:Medium.nXi should not be dropped after extends modifiers: {}",
        rumoca_phase_dae::balance::balance_detail(&result.dae).expect("valid DAE balance fixture")
    );
}

/// Test that flat Modelica output renders equations in `lhs = rhs` form.
#[test]
fn test_flat_modelica_equation_format() {
    let source = r#"
model SpringMass
  parameter Real k = 1.0;
  parameter Real m = 1.0;
  Real x(start = 1.0);
  Real v(start = 0.0);
equation
  der(x) = v;
  m * der(v) = -k * x;
end SpringMass;
"#;
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session.compile_model("SpringMass").expect("compile failed");

    let tmpl = rumoca_phase_codegen::templates::builtin_template_source(
        "dae-modelica",
        "dae_modelica.mo.jinja",
    )
    .unwrap();
    let code =
        rumoca_phase_codegen::render_template_with_name(&result.dae, tmpl, "SpringMass").unwrap();

    // Equations should use "=" format, not residual "x - y" format
    assert!(
        code.contains("="),
        "Flat Modelica equations should use '=' format"
    );
    // Should NOT have bare residual expressions without '='
    for line in code.lines() {
        let trimmed = line.trim();
        if trimmed.ends_with(';')
            && !trimmed.starts_with("parameter")
            && !trimmed.starts_with("Real")
            && !trimmed.starts_with("output")
            && !trimmed.starts_with("input")
            && !trimmed.starts_with("discrete")
            && !trimmed.starts_with("constant")
            && !trimmed.starts_with("class")
            && !trimmed.starts_with("end")
            && !trimmed.starts_with("equation")
            && !trimmed.starts_with("initial")
            && !trimmed.is_empty()
        {
            // This is an equation line - it should contain '='
            assert!(
                trimmed.contains('='),
                "Equation line should contain '=': {}",
                trimmed
            );
        }
    }
}
