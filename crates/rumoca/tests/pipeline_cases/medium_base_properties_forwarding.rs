//! Regression for Buildings-style `Medium.BaseProperties` forwarding through
//! nested `redeclare package Medium = Medium` / `redeclare package Medium = MediumAir`.

use super::*;

const SOURCE: &str = r#"
package PartialMedium
  replaceable partial model BaseProperties
    Real p;
  end BaseProperties;
end PartialMedium;

package BuildingsMediaAir
  extends PartialMedium;

  redeclare model extends BaseProperties
    Real h;
  equation
    h = p + 1;
  end BaseProperties;
end BuildingsMediaAir;

block LumpedVolumeDeclarations
  replaceable package Medium = PartialMedium;
end LumpedVolumeDeclarations;

model ConservationEquation
  extends LumpedVolumeDeclarations;

  Medium.BaseProperties medium;
equation
  medium.p = 101325;
end ConservationEquation;

model PartialMixingVolume
  extends LumpedVolumeDeclarations;

  ConservationEquation dynBal(redeclare final package Medium = Medium);
end PartialMixingVolume;

model PartialFlowMachine
  extends LumpedVolumeDeclarations;

  PartialMixingVolume vol(redeclare package Medium = Medium);
end PartialFlowMachine;

model WithoutMotorLike
  replaceable package Medium = PartialMedium;

  PartialFlowMachine varSpeFloMov(redeclare package Medium = Medium);
end WithoutMotorLike;

model Floor
  replaceable package MediumAir = BuildingsMediaAir;

  WithoutMotorLike mover(redeclare package Medium = MediumAir);
end Floor;

model UsesForwardedMediumBaseProperties
  Floor floor1;
end UsesForwardedMediumBaseProperties;
"#;

#[test]
fn test_nested_medium_base_properties_forwarding_compiles() {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", SOURCE)
        .expect("nested Medium forwarding fixture should parse");

    session
        .compile_model("UsesForwardedMediumBaseProperties")
        .expect("nested package Medium forwarding must instantiate concrete BaseProperties");
}

const WATER_FINAL_BINDING_SOURCE: &str = r#"
package PartialMedium
  type MolarMass = Real(unit = "kg/mol");

  replaceable partial model BaseProperties
    Real p;
  end BaseProperties;
end PartialMedium;

package BuildingsMediaWater
  extends PartialMedium;

  constant MolarMass MM_const = 0.01801528;

  redeclare replaceable model BaseProperties
    Real p;
    final MolarMass MM = MM_const;
  equation
    p = 300000;
  end BaseProperties;
end BuildingsMediaWater;

model ConservationEquation
  replaceable package Medium = PartialMedium;

  Medium.BaseProperties medium;
end ConservationEquation;

model UsesWaterFinalBasePropertiesBinding
  ConservationEquation dynBal(redeclare final package Medium = BuildingsMediaWater);
end UsesWaterFinalBasePropertiesBinding;
"#;

#[test]
fn test_forwarded_water_base_properties_keeps_final_binding_equation() {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", WATER_FINAL_BINDING_SOURCE)
        .expect("water final binding fixture should parse");

    let result = session
        .compile_model("UsesWaterFinalBasePropertiesBinding")
        .expect("water final binding fixture should compile");

    assert!(
        result
            .dae
            .continuous
            .equations
            .iter()
            .any(|eq| eq.origin.contains("binding equation for dynBal.medium.MM")),
        "final medium field binding should define dynBal.medium.MM; balance={}",
        rumoca_phase_dae::balance::balance_detail(&result.dae).expect("valid DAE balance fixture")
    );
    assert!(
        rumoca_phase_dae::balance::is_balanced(&result.dae).expect("valid DAE balance fixture"),
        "model should remain balanced: {}",
        rumoca_phase_dae::balance::balance_detail(&result.dae).expect("valid DAE balance fixture")
    );
}

const HEX_SOURCE: &str = r#"
package PartialMedium
  replaceable partial model BaseProperties
    Real p;
  end BaseProperties;
end PartialMedium;

package BuildingsMediaAir
  extends PartialMedium;

  redeclare model extends BaseProperties
    Real h;
  equation
    h = p + 1;
  end BaseProperties;
end BuildingsMediaAir;

package BuildingsMediaWater
  extends PartialMedium;

  redeclare model extends BaseProperties
    Real h;
  equation
    h = p + 2;
  end BaseProperties;
end BuildingsMediaWater;

block LumpedVolumeDeclarations
  replaceable package Medium = PartialMedium;
end LumpedVolumeDeclarations;

model ConservationEquation
  extends LumpedVolumeDeclarations;

  Medium.BaseProperties medium;
equation
  medium.p = 101325;
end ConservationEquation;

model PartialMixingVolume
  extends LumpedVolumeDeclarations;

  ConservationEquation dynBal(redeclare final package Medium = Medium);
end PartialMixingVolume;

model PartialHexElementLike
  replaceable package Medium1 = PartialMedium;
  replaceable package Medium2 = PartialMedium;

  PartialMixingVolume vol1(redeclare final package Medium = Medium1);
  PartialMixingVolume vol2(redeclare final package Medium = Medium2);
end PartialHexElementLike;

model WetCoilLike
  replaceable package MediumWat = BuildingsMediaWater;
  replaceable package MediumAir = BuildingsMediaAir;

  PartialHexElementLike ele[1](
    redeclare each package Medium1 = MediumWat,
    redeclare each package Medium2 = MediumAir);
end WetCoilLike;

model UsesHexMediumBaseProperties
  WetCoilLike coil;
end UsesHexMediumBaseProperties;
"#;

const CONSTRAINEDBY_HEX_SOURCE: &str = r#"
package PartialMedium
  replaceable partial model BaseProperties
    Real p;
  end BaseProperties;
end PartialMedium;

package BuildingsMediaWater
  extends PartialMedium;

  redeclare model extends BaseProperties
    Real h;
  equation
    h = p + 2;
  end BaseProperties;
end BuildingsMediaWater;

block LumpedVolumeDeclarations
  replaceable package Medium = PartialMedium;
end LumpedVolumeDeclarations;

model ConservationEquation
  extends LumpedVolumeDeclarations;

  Medium.BaseProperties medium;
equation
  medium.p = 101325;
end ConservationEquation;

model PartialMixingVolume
  extends LumpedVolumeDeclarations;

  ConservationEquation dynBal(redeclare final package Medium = Medium);
end PartialMixingVolume;

model PartialHexElementLike
  replaceable package Medium1 = PartialMedium;

  replaceable PartialMixingVolume vol1 constrainedby PartialMixingVolume(
    redeclare final package Medium = Medium1);
end PartialHexElementLike;

model WetCoilLike
  replaceable package MediumWat = BuildingsMediaWater;

  PartialHexElementLike ele[1](redeclare each package Medium1 = MediumWat);
end WetCoilLike;

model UsesConstrainedbyHexMediumBaseProperties
  WetCoilLike coil;
end UsesConstrainedbyHexMediumBaseProperties;
"#;

const HEX_ELEMENT_LATENT_SOURCE: &str = r#"
package PartialMedium
  replaceable partial model BaseProperties
    Real p;
  end BaseProperties;
end PartialMedium;

package BuildingsMediaWater
  extends PartialMedium;

  redeclare model extends BaseProperties
    Real h;
  equation
    h = p + 2;
  end BaseProperties;
end BuildingsMediaWater;

block LumpedVolumeDeclarations
  replaceable package Medium = PartialMedium;
end LumpedVolumeDeclarations;

model ConservationEquation
  extends LumpedVolumeDeclarations;

  Medium.BaseProperties medium;
equation
  medium.p = 101325;
end ConservationEquation;

model PartialMixingVolume
  extends LumpedVolumeDeclarations;

  parameter Boolean initialize_p = true;
  ConservationEquation dynBal(
    redeclare final package Medium = Medium,
    final initialize_p = initialize_p);
end PartialMixingVolume;

model PartialHexElementLike
  replaceable package Medium1 = PartialMedium;
  parameter Boolean initialize_p1 = true;

  replaceable PartialMixingVolume vol1 constrainedby PartialMixingVolume(
    redeclare final package Medium = Medium1);
end PartialHexElementLike;

model HexElementLatentLike
  extends PartialHexElementLike(
    redeclare final PartialMixingVolume vol1(final initialize_p = initialize_p1));
end HexElementLatentLike;

model WetCoilLike
  replaceable package MediumWat = BuildingsMediaWater;

  HexElementLatentLike ele[1](redeclare each package Medium1 = MediumWat);
end WetCoilLike;

model UsesHexElementLatentMediumBaseProperties
  WetCoilLike coil;
end UsesHexElementLatentMediumBaseProperties;
"#;

#[test]
fn test_hex_element_latent_constrainedby_medium_forwarding_compiles() {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("hex_element_latent_test.mo", HEX_ELEMENT_LATENT_SOURCE)
        .expect("hex element latent constrainedby fixture should parse");

    session
        .compile_model("UsesHexElementLatentMediumBaseProperties")
        .expect("hex element latent constrainedby Medium forwarding must compile");
}

const HEX_ELEMENT_LATENT_VOLUME_REDECLARE_SOURCE: &str = r#"
connector RealInput = input Real;

model ConservationEquation
  parameter Boolean use_mWat_flow = false;
  RealInput mWat_flow if use_mWat_flow;
  Real mWat_flow_internal;
equation
  if use_mWat_flow then
    mWat_flow_internal = mWat_flow;
  else
    mWat_flow_internal = 0;
  end if;
end ConservationEquation;

model PartialMixingVolume
  ConservationEquation dynBal(final use_mWat_flow = false);
end PartialMixingVolume;

model MixingVolume
  extends PartialMixingVolume;
end MixingVolume;

model MixingVolumeHeatPort
  extends PartialMixingVolume;
end MixingVolumeHeatPort;

model MixingVolumeHeatMoisturePort
  extends PartialMixingVolume(dynBal(final use_mWat_flow = true));

  RealInput mWat_flow;
equation
  connect(mWat_flow, dynBal.mWat_flow);
end MixingVolumeHeatMoisturePort;

model FourPortHeatMassExchangerLike
  replaceable MixingVolume vol2 constrainedby MixingVolumeHeatPort;
end FourPortHeatMassExchangerLike;

model PartialHexElementLike
  extends FourPortHeatMassExchangerLike;
end PartialHexElementLike;

model HexElementLatentLike
  extends PartialHexElementLike(
    redeclare final MixingVolumeHeatMoisturePort vol2);
end HexElementLatentLike;

model UsesHexElementLatentVolumeRedeclare
  HexElementLatentLike ele[1];
  Real z;
equation
  ele[1].vol2.mWat_flow = 1;
  z = ele[1].vol2.dynBal.mWat_flow_internal;
end UsesHexElementLatentVolumeRedeclare;
"#;

#[test]
fn test_hex_element_latent_component_redeclare_uses_redeclared_volume_modifiers() {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document(
            "hex_element_latent_volume_redeclare_test.mo",
            HEX_ELEMENT_LATENT_VOLUME_REDECLARE_SOURCE,
        )
        .expect("hex element latent volume redeclare fixture should parse");

    let result = session
        .compile_model("UsesHexElementLatentVolumeRedeclare")
        .expect("component redeclare must instantiate the redeclared volume type");
    let json = serde_json::to_value(&result.dae).expect("DAE should serialize");

    assert!(
        json["u"].get("ele[1].vol2.mWat_flow").is_some()
            || json["w"].get("ele[1].vol2.mWat_flow").is_some()
            || json["y"].get("ele[1].vol2.mWat_flow").is_some(),
        "redeclared volume should expose mWat_flow connector; got DAE:\n{json:#}"
    );
    assert_eq!(
        json["p"]["ele[1].vol2.dynBal.use_mWat_flow"]["start"]["Literal"]["value"]["Boolean"], true,
        "redeclared volume's inherited dynBal modifier must force use_mWat_flow=true"
    );
}

#[test]
fn test_hex_medium1_medium2_base_properties_forwarding_compiles() {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("hex_test.mo", HEX_SOURCE)
        .expect("hex Medium1/Medium2 forwarding fixture should parse");

    session
        .compile_model("UsesHexMediumBaseProperties")
        .expect("hex element Medium1/Medium2 forwarding must instantiate concrete BaseProperties");
}

const MEDIUM_NXI_FOR_SOURCE: &str = r#"
package PartialMedium
  constant Integer nXi = 1;
end PartialMedium;

package MediumAir
  extends PartialMedium(nXi = 2);
end MediumAir;

block LumpedVolumeDeclarations
  replaceable package Medium = PartialMedium;
end LumpedVolumeDeclarations;

model ConservationEquation
  extends LumpedVolumeDeclarations;

  Real s[Medium.nXi];
equation
  for i in 1:Medium.nXi loop
    s[i] = i;
  end for;
end ConservationEquation;

model PartialMixingVolume
  extends LumpedVolumeDeclarations;

  ConservationEquation dynBal(redeclare final package Medium = Medium);
end PartialMixingVolume;

model PartialFlowMachine
  extends LumpedVolumeDeclarations;

  PartialMixingVolume vol(redeclare package Medium = Medium);
end PartialFlowMachine;

model WithoutMotorLike
  replaceable package Medium = PartialMedium;

  PartialFlowMachine varSpeFloMov(redeclare package Medium = MediumAir);
end WithoutMotorLike;

model UsesNestedMediumNxiForRange
  WithoutMotorLike mover;
end UsesNestedMediumNxiForRange;
"#;

#[test]
fn test_nested_medium_nxi_for_range_compiles() {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("medium_nxi_for_test.mo", MEDIUM_NXI_FOR_SOURCE)
        .expect("nested Medium.nXi for-range fixture should parse");

    let result = session
        .compile_model("UsesNestedMediumNxiForRange")
        .expect("nested Medium forwarding must resolve Medium.nXi in dynBal for-equations");

    assert!(
        rumoca_phase_dae::balance::is_balanced(&result.dae).expect("valid DAE balance fixture"),
        "nested dynBal for-range should expand with MediumAir.nXi=2: {}",
        rumoca_phase_dae::balance::balance_detail(&result.dae).expect("valid DAE balance fixture")
    );
}

#[test]
fn test_constrainedby_medium_base_properties_forwarding_compiles() {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("constrainedby_hex_test.mo", CONSTRAINEDBY_HEX_SOURCE)
        .expect("constrainedby Medium forwarding fixture should parse");

    session
        .compile_model("UsesConstrainedbyHexMediumBaseProperties")
        .expect("constrainedby Medium binding must forward to concrete BaseProperties");
}

const MEDIUM_FUNCTION_FORWARDING_SOURCE: &str = r#"
package Modelica
  package Media
    package Interfaces
      partial package PartialMedium
        replaceable record ThermodynamicState
          Real p;
          Real T;
        end ThermodynamicState;

        replaceable partial function setState_pTX
          input Real p;
          input Real T;
          output ThermodynamicState state;
        end setState_pTX;

        replaceable function specificEnthalpy
          input ThermodynamicState state;
          output Real h;
        algorithm
          h := state.T + state.p;
        end specificEnthalpy;

        replaceable function specificEnthalpy_pTX
          input Real p;
          input Real T;
          output Real h;
        algorithm
          h := specificEnthalpy(setState_pTX(p, T));
        end specificEnthalpy_pTX;
      end PartialMedium;
    end Interfaces;
  end Media;
end Modelica;

package BuildingsMediaAir
  extends Modelica.Media.Interfaces.PartialMedium;

  redeclare record extends ThermodynamicState
  end ThermodynamicState;

  redeclare function extends setState_pTX
  algorithm
    state := ThermodynamicState(p=p, T=T);
  end setState_pTX;
end BuildingsMediaAir;

block LumpedVolumeDeclarations
  replaceable package Medium = Modelica.Media.Interfaces.PartialMedium;
end LumpedVolumeDeclarations;

model ConservationEquation
  extends LumpedVolumeDeclarations;

  Real h;
equation
  h = Medium.specificEnthalpy(Medium.setState_pTX(101325, 293.15));
end ConservationEquation;

model InheritedFunctionCaller
  extends LumpedVolumeDeclarations;

  Real h;
equation
  h = Medium.specificEnthalpy_pTX(101325, 293.15);
end InheritedFunctionCaller;

model PartialMixingVolume
  extends LumpedVolumeDeclarations;

  ConservationEquation dynBal(redeclare final package Medium = Medium);
  InheritedFunctionCaller funBal(redeclare final package Medium = Medium);
end PartialMixingVolume;

model PartialFlowMachine
  extends LumpedVolumeDeclarations;

  PartialMixingVolume vol(redeclare package Medium = Medium);
end PartialFlowMachine;

model WithoutMotorLike
  replaceable package Medium = Modelica.Media.Interfaces.PartialMedium;

  PartialFlowMachine varSpeFloMov(redeclare package Medium = Medium);
end WithoutMotorLike;

model Floor
  replaceable package MediumAir = BuildingsMediaAir;

  WithoutMotorLike mover(redeclare package Medium = MediumAir);
end Floor;

model UsesNestedMediumFunctionForwarding
  Floor floor1;
end UsesNestedMediumFunctionForwarding;
"#;

#[test]
fn test_nested_medium_function_forwarding_compiles() {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document(
            "medium_function_forwarding_test.mo",
            MEDIUM_FUNCTION_FORWARDING_SOURCE,
        )
        .expect("nested Medium function forwarding fixture should parse");

    session
        .compile_model("UsesNestedMediumFunctionForwarding")
        .expect("nested Medium forwarding must rewrite setState_pTX to the concrete medium");
}
