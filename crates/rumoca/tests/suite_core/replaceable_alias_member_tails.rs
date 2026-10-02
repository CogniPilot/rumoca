//! Member tails through components typed by a replaceable alias (MLS 3.7 §7.3).
//!
//! `Modelica.Fluid` tanks declare `Medium.BaseProperties medium` with
//! `replaceable package Medium`, and an enclosing model reads `tank.medium.T`.
//! The class of `medium` is selected per occurrence by the redeclarations that
//! reach it, so the member `T` is proved against the class each materialized
//! occurrence selected, never a single class of the declaration, whether the
//! reference is written in an equation or in a modifier binding.

use rumoca::Compiler;

const SOURCE: &str = r#"
package Media
  partial package PartialMedium
    replaceable partial model BaseProperties
      Real T;
    end BaseProperties;
  end PartialMedium;
  package One
    extends PartialMedium;
    redeclare model extends BaseProperties
    equation
      T = 1;
    end BaseProperties;
  end One;
  package Two
    extends PartialMedium;
    redeclare model extends BaseProperties
    equation
      T = 2;
    end BaseProperties;
  end Two;
end Media;

model Tank
  replaceable package Medium = Media.One constrainedby Media.PartialMedium;
  Medium.BaseProperties medium;
end Tank;

model Sensor
  Real u;
end Sensor;

model Plant
  replaceable package PlantMedium = Media.Two constrainedby Media.PartialMedium;
  Tank redeclared(redeclare package Medium = PlantMedium);
  Tank nominal;
  Tank tanks[2](redeclare each package Medium = PlantMedium);
  Real redeclaredT;
  Real nominalT;
  Real arrayT;
  Sensor redeclaredSensor(u = redeclared.medium.T);
  Sensor nominalSensor(u = nominal.medium.T);
equation
  redeclaredT = redeclared.medium.T;
  nominalT = nominal.medium.T;
  arrayT = tanks[2].medium.T;
end Plant;
"#;

#[test]
fn member_tails_follow_the_class_each_occurrence_selected() {
    let compiled = Compiler::new()
        .model("Plant")
        .compile_str(SOURCE, "ReplaceableAliasMemberTails.mo")
        .unwrap_or_else(|error| panic!("Plant compiles: {error:?}"));
    let result = rumoca_sim::simulate_dae_with_diagnostics(
        &compiled.dae,
        &rumoca_sim::SimOptions {
            t_end: 0.1,
            ..Default::default()
        },
    )
    .unwrap_or_else(|error| panic!("Plant simulates: {error:?}"));
    let value = |name: &str| {
        let column = result
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap_or_else(|| panic!("{name} in {:?}", result.names));
        result.data[column][0]
    };
    assert_eq!(value("redeclaredT"), 2.0);
    assert_eq!(value("nominalT"), 1.0);
    assert_eq!(value("arrayT"), 2.0);
    // A binding written in a modifier of the enclosing class proves the same
    // member tail against the same occurrences.
    assert_eq!(value("redeclaredSensor.u"), 2.0);
    assert_eq!(value("nominalSensor.u"), 1.0);
}
