use super::*;

/// MLS §10.4: a subscripted array of records still has record element type, so
/// field access after `[:]` projects the field over the selected record array.
#[test]
fn test_constant_record_array_colon_field_projection_resolves_before_dae() {
    let source = r#"
package P
  record FluidConstant
    constant Real molarMass;
  end FluidConstant;

  package Medium
    constant FluidConstant fluidConstants[2] = {
      FluidConstant(molarMass = 0.018),
      FluidConstant(molarMass = 0.044)
    };
    constant Real molarMasses[2] = fluidConstants[:].molarMass;
  end Medium;

  model Example
    Real y = Medium.molarMasses[2];
  end Example;
end P;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session.compile_model("P.Example").expect("compile failed");
    let dae_debug = format!("{:?}", result.dae);
    assert!(
        dae_debug.contains("0.044"),
        "expected molarMasses[2] to fold through fluidConstants[:].molarMass; dae={dae_debug}"
    );
    assert!(
        !dae_debug.contains("fluidConstants"),
        "constant record array projection should not remain unresolved; dae={dae_debug}"
    );
}

/// MLS §10.4: scalar indexing into a constant record array preserves the record
/// element type, so the following field access is a constant field projection.
#[test]
fn test_constant_record_array_index_field_projection_resolves_before_dae() {
    let source = r#"
package P
  record FluidConstant
    constant Real molarMass;
  end FluidConstant;

  package Medium
    constant FluidConstant fluidConstants[2] = {
      FluidConstant(molarMass = 0.018),
      FluidConstant(molarMass = 0.044)
    };
  end Medium;

  model Example
    Real y = Medium.fluidConstants[1].molarMass;
  end Example;
end P;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session.compile_model("P.Example").expect("compile failed");
    let dae_debug = format!("{:?}", result.dae);
    assert!(
        dae_debug.contains("0.018"),
        "expected fluidConstants[1].molarMass to fold to the first record field; dae={dae_debug}"
    );
    assert!(
        !dae_debug.contains("fluidConstants"),
        "indexed record constant field access should not remain unresolved; dae={dae_debug}"
    );
}

/// MLS §7.2.5: `each` modifications on an array component apply the same
/// record-field modification to every element. The synthesized constant value
/// must therefore be an array of record values, not a scalar record value.
#[test]
fn test_each_modified_constant_record_array_index_field_resolves_before_dae() {
    let source = r#"
package P
  record FluidConstant
    constant Real molarMass;
  end FluidConstant;

  package Medium
    constant FluidConstant fluidConstants[1](each molarMass = 0.018);
  end Medium;

  model Example
    Real y = Medium.fluidConstants[1].molarMass;
  end Example;
end P;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session.compile_model("P.Example").expect("compile failed");
    let dae_debug = format!("{:?}", result.dae);
    assert!(
        dae_debug.contains("0.018"),
        "expected each-modified fluidConstants[1].molarMass to fold; dae={dae_debug}"
    );
    assert!(
        !dae_debug.contains("fluidConstants"),
        "each-modified record array field access should not remain unresolved; dae={dae_debug}"
    );
}

/// MLS §7.1 + §7.2: inherited package constants modified on an extends clause
/// are members of the derived package's effective class. Inherited function
/// bodies must therefore resolve and fold record-array constant field access
/// through the active package, not the partial source package.
#[test]
fn test_inherited_function_uses_active_extends_record_array_constant() {
    let source = r#"
package P
  record FluidConstant
    constant Real molarMass;
  end FluidConstant;

  partial package PartialMedium
    constant Integer nS = 1;
    constant FluidConstant fluidConstants[nS];

    replaceable record State
      Real x;
    end State;

    replaceable partial function molarMass
      input State state;
      output Real MM;
    end molarMass;

    replaceable function property_p
      input Real p;
      output Real y;
    protected
      State state;
    algorithm
      y := molarMass(state);
    end property_p;
  end PartialMedium;

  package Concrete
    extends PartialMedium(nS = 1, fluidConstants = {FluidConstant(molarMass = 0.018)});

    redeclare record extends State
      Real p;
    end State;

    redeclare function extends molarMass
    algorithm
      MM := fluidConstants[1].molarMass;
    end molarMass;
  end Concrete;

  model Probe
    package Medium = Concrete;
    Real y = Medium.property_p(1);
  end Probe;
end P;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session.compile_model("P.Probe").expect("compile failed");
    let function_body = result
        .flat
        .functions
        .get(&rumoca_core::VarName::new("P.Concrete.molarMass"))
        .map(|func| format!("{:?}", func.body))
        .expect("concrete inherited molarMass should be collected");
    assert!(
        function_body.contains("0.018"),
        "active extends record-array constant should fold in inherited function; body={function_body}"
    );
    assert!(
        !function_body.contains("PartialMedium.fluidConstants")
            && !function_body.contains("Concrete.fluidConstants"),
        "inherited function must not retain package constant references; body={function_body}"
    );
}

/// MLS §5.2/§7.1: a short package definition is an alias to the target package's
/// effective class. Constants modified through the aliased package's extends
/// chain must remain visible to inherited functions under the alias name.
#[test]
fn test_short_package_alias_preserves_active_extends_record_array_constant() {
    let source = r#"
package P
  record FluidConstant
    constant Real molarMass;
  end FluidConstant;

  partial package PartialMedium
    constant Integer nS = 1;
    constant FluidConstant fluidConstants[nS];

    replaceable record State
      Real x;
    end State;

    replaceable partial function molarMass
      input State state;
      output Real MM;
    end molarMass;

    replaceable function property_p
      input Real p;
      output Real y;
    protected
      State state;
    algorithm
      y := molarMass(state);
    end property_p;
  end PartialMedium;

  package Water
    constant FluidConstant waterConstants[1](each molarMass = 0.018);

    partial package WaterBase
      extends PartialMedium(nS = 1, fluidConstants = waterConstants);

      redeclare record extends State
        Real p;
      end State;

      redeclare function extends molarMass
      algorithm
        MM := fluidConstants[1].molarMass;
      end molarMass;
    end WaterBase;

    package WaterPH
      extends WaterBase;
    end WaterPH;

    package StandardWater = WaterPH;
  end Water;

  model Probe
    package Medium = Water.StandardWater;
    Real y = Medium.property_p(1);
  end Probe;
end P;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse failed");

    let result = session.compile_model("P.Probe").expect("compile failed");
    let function_body = result
        .flat
        .functions
        .get(&rumoca_core::VarName::new(
            "P.Water.StandardWater.molarMass",
        ))
        .map(|func| format!("{:?}", func.body))
        .expect("aliased inherited molarMass should be collected");
    assert!(
        function_body.contains("0.018"),
        "alias active extends record-array constant should fold in inherited function; body={function_body}"
    );
    assert!(
        !function_body.contains("fluidConstants"),
        "aliased inherited function must not retain package constant references; body={function_body}"
    );
}
