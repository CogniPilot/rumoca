//! Tests for the flatten module.

use super::*;
use crate::modelica_grammar::ModelicaGrammar;
use crate::modelica_parser::parse;

fn parse_test_code(code: &str) -> ir::ast::StoredDefinition {
    let mut grammar = ModelicaGrammar::new();
    parse(code, "test.mo", &mut grammar).expect("Failed to parse test code");
    grammar.modelica.expect("No AST produced")
}

#[test]
fn test_replaceable_package_simple() {
    // Test that Medium.nXi resolves when Medium is a replaceable package
    let code = r#"
package MyMedia
    partial package PartialMedium
        constant Integer nXi = 2;
    end PartialMedium;
end MyMedia;

model TestModel
    replaceable package Medium = MyMedia.PartialMedium;
    Real x[Medium.nXi];
equation
    x = {1, 2};
end TestModel;
"#;
    let ast = parse_test_code(code);
    let result = flatten(&ast, Some("TestModel"));
    assert!(
        result.is_ok(),
        "Should flatten successfully: {:?}",
        result.err()
    );

    let flat = result.unwrap();
    // x should be flattened as an array with size 2
    assert!(flat.components.contains_key("x"), "Should have component x");
}

#[test]
fn test_replaceable_package_parent_scope_type() {
    // Test that Medium.MassFraction resolves when MassFraction is defined
    // in the parent package of PartialMedium, not in PartialMedium itself
    let code = r#"
package Media
    package Interfaces
        type MassFraction = Real(min=0, max=1);
        type AbsolutePressure = Real(min=0);

        partial package PartialMedium
            constant Integer nXi = 2;
        end PartialMedium;
    end Interfaces;
end Media;

model TankWithMedium
    replaceable package Medium = Media.Interfaces.PartialMedium;

    Medium.MassFraction Xi[Medium.nXi];
    Medium.AbsolutePressure p;
equation
    Xi = {0.5, 0.5};
    p = 101325;
end TankWithMedium;
"#;
    let ast = parse_test_code(code);
    let result = flatten(&ast, Some("TankWithMedium"));
    assert!(
        result.is_ok(),
        "Should flatten model with parent scope types: {:?}",
        result.err()
    );

    let flat = result.unwrap();
    assert!(
        flat.components.contains_key("Xi"),
        "Should have component Xi"
    );
    assert!(flat.components.contains_key("p"), "Should have component p");
}

#[test]
fn test_replaceable_package_deeply_nested() {
    // Test deeply nested package structure similar to MSL
    let code = r#"
package Media
    package Interfaces
        type MassFraction = Real(min=0, max=1);
        type AbsolutePressure = Real(min=0);
        type Temperature = Real(min=0);

        partial package PartialMedium
            constant Integer nXi = 2;
        end PartialMedium;
    end Interfaces;
end Media;

package Fluid
    package Examples
        package BatchPlant
            package BaseClasses
                model TankWithMedium
                    replaceable package Medium = Media.Interfaces.PartialMedium;

                    Medium.MassFraction Xi[Medium.nXi];
                    Medium.AbsolutePressure p;
                    Medium.Temperature T;
                equation
                    Xi = {0.5, 0.5};
                    p = 101325;
                    T = 300;
                end TankWithMedium;
            end BaseClasses;
        end BatchPlant;
    end Examples;
end Fluid;
"#;
    let ast = parse_test_code(code);
    let result = flatten(
        &ast,
        Some("Fluid.Examples.BatchPlant.BaseClasses.TankWithMedium"),
    );
    assert!(
        result.is_ok(),
        "Should flatten deeply nested model: {:?}",
        result.err()
    );

    let flat = result.unwrap();
    assert!(
        flat.components.contains_key("Xi"),
        "Should have component Xi"
    );
    assert!(flat.components.contains_key("p"), "Should have component p");
    assert!(flat.components.contains_key("T"), "Should have component T");
}

#[test]
fn test_replaceable_package_with_extends() {
    // Test that Medium.AbsolutePressure resolves when AbsolutePressure is inherited
    // via extends in PartialMedium (like MSL where PartialMedium extends Types)
    let code = r#"
package Modelica
    package Media
        package Interfaces
            package Types
                type AbsolutePressure = Real(min=0);
                type MassFraction = Real(min=0, max=1);
                type Temperature = Real(min=0);
            end Types;

            partial package PartialMedium
                extends Modelica.Media.Interfaces.Types;
                constant Integer nXi = 2;
            end PartialMedium;
        end Interfaces;
    end Media;

    package Fluid
        model Pump
            replaceable package Medium = Modelica.Media.Interfaces.PartialMedium;

            Medium.AbsolutePressure p;
            Medium.MassFraction Xi[Medium.nXi];
            Medium.Temperature T;
        equation
            p = 101325;
            Xi = {0.5, 0.5};
            T = 300;
        end Pump;
    end Fluid;
end Modelica;
"#;
    let ast = parse_test_code(code);
    let result = flatten(&ast, Some("Modelica.Fluid.Pump"));
    assert!(
        result.is_ok(),
        "Should flatten model with inherited types: {:?}",
        result.err()
    );

    let flat = result.unwrap();
    assert!(flat.components.contains_key("p"), "Should have component p");
    assert!(
        flat.components.contains_key("Xi"),
        "Should have component Xi"
    );
    assert!(flat.components.contains_key("T"), "Should have component T");
}

#[test]
fn test_replaceable_package_inherited_through_extends() {
    // Test that Medium.AbsolutePressure resolves when Medium is defined
    // in a parent class via extends (like MSL Fluid components)
    // FluidPort -> PartialTwoPort -> ActualComponent
    let code = r#"
package Modelica
    package Media
        package Interfaces
            package Types
                type AbsolutePressure = Real(min=0);
                type MassFlowRate = Real;
            end Types;

            partial package PartialMedium
                extends Modelica.Media.Interfaces.Types;
            end PartialMedium;
        end Interfaces;
    end Media;

    package Fluid
        package Interfaces
            connector FluidPort
                replaceable package Medium = Modelica.Media.Interfaces.PartialMedium;
                flow Medium.MassFlowRate m_flow;
                Medium.AbsolutePressure p;
            end FluidPort;

            partial model PartialTwoPort
                replaceable package Medium = Modelica.Media.Interfaces.PartialMedium;
                FluidPort port_a(redeclare package Medium = Medium);
            end PartialTwoPort;
        end Interfaces;

        package Fittings
            model Valve
                extends Modelica.Fluid.Interfaces.PartialTwoPort;
                Medium.AbsolutePressure dp;
            equation
                dp = 1000;
            end Valve;
        end Fittings;
    end Fluid;
end Modelica;
"#;
    let ast = parse_test_code(code);
    let result = flatten(&ast, Some("Modelica.Fluid.Fittings.Valve"));
    assert!(
        result.is_ok(),
        "Should flatten model with inherited Medium package: {:?}",
        result.err()
    );

    let flat = result.unwrap();
    assert!(
        flat.components.contains_key("dp"),
        "Should have component dp"
    );
}

#[test]
fn test_model_alias() {
    // Test that model aliases like `replaceable model FlowModel = SomeModel` work
    let code = r#"
model BaseFlowModel
    Real dp;
equation
    dp = 100;
end BaseFlowModel;

model Pipe
    replaceable model FlowModel = BaseFlowModel;
    FlowModel flowModel;
end Pipe;
"#;
    let ast = parse_test_code(code);
    let result = flatten(&ast, Some("Pipe"));
    assert!(
        result.is_ok(),
        "Should flatten model with model alias: {:?}",
        result.err()
    );

    let flat = result.unwrap();
    assert!(
        flat.components.contains_key("flowModel.dp"),
        "Should have component flowModel.dp from expanded FlowModel"
    );
}

#[test]
fn test_numbered_package_aliases() {
    // Test numbered package aliases like Medium1, Medium2
    let code = r#"
package Media
    package Interfaces
        type SpecificHeatCapacity = Real;
        type Temperature = Real;
        partial package PartialMedium
            constant Integer n = 1;
        end PartialMedium;
    end Interfaces;
end Media;

model MixtureTest
    package Medium1 = Media.Interfaces.PartialMedium;
    package Medium2 = Media.Interfaces.PartialMedium;
    Medium1.SpecificHeatCapacity cp1;
    Medium2.Temperature T2;
equation
    cp1 = 1000;
    T2 = 300;
end MixtureTest;
"#;
    let ast = parse_test_code(code);
    let result = flatten(&ast, Some("MixtureTest"));
    assert!(
        result.is_ok(),
        "Should flatten model with numbered package aliases: {:?}",
        result.err()
    );

    let flat = result.unwrap();
    assert!(
        flat.components.contains_key("cp1"),
        "Should have component cp1"
    );
    assert!(
        flat.components.contains_key("T2"),
        "Should have component T2"
    );
}

#[test]
fn test_non_replaceable_package_alias() {
    // Test that non-replaceable package aliases work the same as replaceable ones
    let code = r#"
package MyMedia
    type MolarMass = Real(min=0);
    partial package PartialMedium
        constant Integer nX = 2;
    end PartialMedium;
end MyMedia;

model TestModel
    package Medium = MyMedia.PartialMedium;  // Note: no 'replaceable' keyword
    Medium.MolarMass MM;
    Real x[Medium.nX];
equation
    MM = 0.018;
    x = {1, 2};
end TestModel;
"#;
    let ast = parse_test_code(code);
    let result = flatten(&ast, Some("TestModel"));
    assert!(
        result.is_ok(),
        "Should flatten model with non-replaceable package alias: {:?}",
        result.err()
    );

    let flat = result.unwrap();
    assert!(
        flat.components.contains_key("MM"),
        "Should have component MM"
    );
    assert!(flat.components.contains_key("x"), "Should have component x");
}

#[test]
fn test_standalone_type_from_parent_scope() {
    // Test that types can be resolved from parent package scope when
    // a class extends another class from a different package.
    // This is the pattern used in MoistAir.BaseProperties -> MassFraction
    let code = r#"
package Modelica
    package Media
        package Interfaces
            type MassFraction = Real(min=0, max=1);
            type AbsolutePressure = Real(min=0);

            partial package PartialMedium
                constant Integer nX = 2;
            end PartialMedium;
        end Interfaces;

        package Air
            package MoistAir
                extends Modelica.Media.Interfaces.PartialMedium;

                model BaseProperties
                    // MassFraction should resolve to Modelica.Media.Interfaces.MassFraction
                    // because MoistAir extends from PartialMedium which is in Interfaces
                    MassFraction x_water;
                    AbsolutePressure p;
                equation
                    x_water = 0.5;
                    p = 101325;
                end BaseProperties;
            end MoistAir;
        end Air;
    end Media;
end Modelica;
"#;
    let ast = parse_test_code(code);
    let result = flatten(&ast, Some("Modelica.Media.Air.MoistAir.BaseProperties"));
    assert!(
        result.is_ok(),
        "Should flatten model with standalone types from parent scope: {:?}",
        result.err()
    );

    let flat = result.unwrap();
    assert!(
        flat.components.contains_key("x_water"),
        "Should have component x_water"
    );
    assert!(flat.components.contains_key("p"), "Should have component p");
}

#[test]
fn test_standalone_type_inside_parent_class() {
    // Test the actual MSL pattern where MassFraction is defined INSIDE PartialMedium,
    // not in the enclosing Interfaces package.
    // MoistAir extends PartialCondensingGases extends PartialMixtureMedium extends PartialMedium
    // MassFraction is defined in PartialMedium
    // BaseProperties in MoistAir should be able to use MassFraction
    let code = r#"
package Modelica
    package Media
        package Interfaces
            partial package PartialMedium
                type MassFraction = Real(min=0, max=1);
                type DynamicViscosity = Real(min=0);
                constant Integer nX = 2;
            end PartialMedium;

            partial package PartialMixtureMedium
                extends PartialMedium;
            end PartialMixtureMedium;

            partial package PartialCondensingGases
                extends PartialMixtureMedium;
            end PartialCondensingGases;
        end Interfaces;

        package Air
            package MoistAir
                extends Modelica.Media.Interfaces.PartialCondensingGases;

                model BaseProperties
                    // MassFraction should be found in PartialMedium through the extends chain
                    MassFraction x_water;
                    DynamicViscosity eta;
                equation
                    x_water = 0.5;
                    eta = 0.001;
                end BaseProperties;
            end MoistAir;
        end Air;
    end Media;
end Modelica;
"#;
    let ast = parse_test_code(code);
    let result = flatten(&ast, Some("Modelica.Media.Air.MoistAir.BaseProperties"));
    assert!(
        result.is_ok(),
        "Should flatten model with types from extends chain: {:?}",
        result.err()
    );

    let flat = result.unwrap();
    assert!(
        flat.components.contains_key("x_water"),
        "Should have component x_water"
    );
    assert!(
        flat.components.contains_key("eta"),
        "Should have component eta"
    );
}
