//! MLS §18: Annotations - Detailed Conformance Tests
//!
//! Comprehensive tests covering normative statements from MLS §18 including:
//! - §18.1: Documentation annotations
//! - §18.2: Graphical annotations
//! - §18.3: External function annotations
//! - §18.4: Version annotations
//! - §18.5: Code generation annotations
//!
//! Reference: https://specification.modelica.org/master/annotations.html

use crate::spec::expect_parse_success;

// ============================================================================
// §18.1 DOCUMENTATION ANNOTATIONS
// ============================================================================

/// MLS §18.1: Documentation annotations
mod documentation_annotations {
    use super::*;

    /// Basic documentation annotation
    #[test]
    fn mls_18_1_basic_documentation() {
        expect_parse_success(
            r#"
            model Test
                Real x = 1;
                annotation(Documentation(info="<html>A test model</html>"));
            end Test;
            "#,
        );
    }

    /// Documentation with revisions
    #[test]
    fn mls_18_1_documentation_revisions() {
        expect_parse_success(
            r#"
            model Test
                Real x = 1;
                annotation(Documentation(
                    info="<html>Model description</html>",
                    revisions="<html>
                        <ul>
                            <li>v1.0 - Initial version</li>
                        </ul>
                    </html>"
                ));
            end Test;
            "#,
        );
    }

    /// Component with documentation
    #[test]
    fn mls_18_1_component_documentation() {
        expect_parse_success(
            r#"
            model Test
                Real x "The x variable" annotation(Documentation(info="State variable"));
            equation
                x = 1;
            end Test;
            "#,
        );
    }

    /// Function with documentation
    #[test]
    fn mls_18_1_function_documentation() {
        expect_parse_success(
            r#"
            function Square "Compute the square of a number"
                input Real x "Input value";
                output Real y "Squared result";
            algorithm
                y := x * x;
                annotation(Documentation(info="Computes y = x^2"));
            end Square;
            "#,
        );
    }
}

// ============================================================================
// §18.2 GRAPHICAL ANNOTATIONS
// ============================================================================

/// MLS §18.2: Graphical annotations
mod graphical_annotations {
    use super::*;

    /// Icon annotation
    #[test]
    fn mls_18_2_icon_annotation() {
        expect_parse_success(
            r#"
            model Test
                Real x = 1;
                annotation(Icon(graphics={
                    Rectangle(extent={{-100,-100},{100,100}})
                }));
            end Test;
            "#,
        );
    }

    /// Diagram annotation
    #[test]
    fn mls_18_2_diagram_annotation() {
        expect_parse_success(
            r#"
            model Test
                Real x = 1;
                annotation(Diagram(graphics={
                    Text(extent={{-100,100},{100,80}}, textString="Test Model")
                }));
            end Test;
            "#,
        );
    }

    /// Placement annotation for components
    #[test]
    fn mls_18_2_placement() {
        expect_parse_success(
            r#"
            model Container
                model Inner
                    Real x = 1;
                end Inner;
                Inner i annotation(Placement(transformation(extent={{-10,-10},{10,10}})));
            equation
            end Container;
            "#,
        );
    }

    /// Line annotation for connections
    #[test]
    fn mls_18_2_line_annotation() {
        expect_parse_success(
            r#"
            connector C
                Real x;
            end C;

            model Test
                C a;
                C b;
            equation
                connect(a, b) annotation(Line(points={{-10,0},{10,0}}));
            end Test;
            "#,
        );
    }

    /// Multiple graphics elements
    #[test]
    fn mls_18_2_multiple_graphics() {
        expect_parse_success(
            r#"
            model Test
                Real x = 1;
                annotation(Icon(graphics={
                    Rectangle(extent={{-100,-100},{100,100}}, fillColor={255,255,255}),
                    Text(extent={{-80,40},{80,-40}}, textString="%name"),
                    Ellipse(extent={{-50,-50},{50,50}})
                }));
            end Test;
            "#,
        );
    }
}

// ============================================================================
// §18.3 EXTERNAL FUNCTION ANNOTATIONS
// ============================================================================

/// MLS §18.3: External function annotations
mod external_annotations {
    use super::*;

    /// Include annotation
    #[test]
    fn mls_18_3_include() {
        expect_parse_success(
            r#"
            function ExternalFunc
                input Real x;
                output Real y;
            external "C" y = external_func(x)
                annotation(Include="extern double external_func(double);");
            end ExternalFunc;
            "#,
        );
    }

    /// Library annotation
    #[test]
    fn mls_18_3_library() {
        expect_parse_success(
            r#"
            function ExternalFunc
                input Real x;
                output Real y;
            external "C"
                annotation(Library="mylib");
            end ExternalFunc;
            "#,
        );
    }

    /// Library path annotation
    #[test]
    fn mls_18_3_library_path() {
        expect_parse_success(
            r#"
            function ExternalFunc
                input Real x;
                output Real y;
            external "C"
                annotation(
                    Library={"m", "pthread"},
                    LibraryDirectory="modelica://MyLib/Resources/Library"
                );
            end ExternalFunc;
            "#,
        );
    }

    /// IncludeDirectory annotation
    #[test]
    fn mls_18_3_include_directory() {
        expect_parse_success(
            r#"
            function ExternalFunc
                input Real x;
                output Real y;
            external "C"
                annotation(IncludeDirectory="modelica://MyLib/Resources/Include");
            end ExternalFunc;
            "#,
        );
    }
}

// ============================================================================
// §18.4 VERSION ANNOTATIONS
// ============================================================================

/// MLS §18.4: Version annotations
mod version_annotations {
    use super::*;

    /// Version annotation on package
    #[test]
    fn mls_18_4_package_version() {
        expect_parse_success(
            r#"
            package MyPackage
                constant Real x = 1;
                annotation(version="1.0.0");
            end MyPackage;
            "#,
        );
    }

    /// Version with date
    #[test]
    fn mls_18_4_version_date() {
        expect_parse_success(
            r#"
            package MyPackage
                annotation(
                    version="2.0.0",
                    versionDate="2024-01-15"
                );
            end MyPackage;
            "#,
        );
    }

    /// Uses annotation
    #[test]
    fn mls_18_4_uses_annotation() {
        expect_parse_success(
            r#"
            package MyPackage
                annotation(uses(Modelica(version="4.0.0")));
            end MyPackage;
            "#,
        );
    }

    /// Multiple uses
    #[test]
    fn mls_18_4_multiple_uses() {
        expect_parse_success(
            r#"
            package MyPackage
                annotation(uses(
                    Modelica(version="4.0.0"),
                    ModelicaServices(version="4.0.0")
                ));
            end MyPackage;
            "#,
        );
    }

    /// Version build info
    #[test]
    fn mls_18_4_version_build() {
        expect_parse_success(
            r#"
            package MyPackage
                annotation(
                    version="1.0.0",
                    versionBuild=42,
                    dateModified="2024-01-15 12:00:00Z"
                );
            end MyPackage;
            "#,
        );
    }
}

// ============================================================================
// §18.5 CODE GENERATION ANNOTATIONS
// ============================================================================

/// MLS §18.5: Code generation annotations
mod code_generation_annotations {
    use super::*;

    /// Inline annotation
    #[test]
    fn mls_18_5_inline() {
        expect_parse_success(
            r#"
            function FastFunc
                input Real x;
                output Real y;
            algorithm
                y := x * x;
                annotation(Inline=true);
            end FastFunc;
            "#,
        );
    }

    /// LateInline annotation
    #[test]
    fn mls_18_5_late_inline() {
        expect_parse_success(
            r#"
            function LateFunc
                input Real x;
                output Real y;
            algorithm
                y := x * x;
                annotation(LateInline=true);
            end LateFunc;
            "#,
        );
    }

    /// GenerateEvents annotation
    #[test]
    fn mls_18_5_generate_events() {
        expect_parse_success(
            r#"
            function SmoothFunc
                input Real x;
                output Real y;
            algorithm
                y := if x > 0 then x else 0;
                annotation(GenerateEvents=false);
            end SmoothFunc;
            "#,
        );
    }

    /// smoothOrder annotation
    #[test]
    fn mls_18_5_smooth_order() {
        expect_parse_success(
            r#"
            function ContinuousFunc
                input Real x;
                output Real y;
            algorithm
                y := x * x * x;
                annotation(smoothOrder=2);
            end ContinuousFunc;
            "#,
        );
    }

    /// derivative annotation
    #[test]
    fn mls_18_5_derivative() {
        expect_parse_success(
            r#"
            function MyFunc
                input Real x;
                output Real y;
            algorithm
                y := x * x;
                annotation(derivative=MyFunc_der);
            end MyFunc;

            function MyFunc_der
                input Real x;
                input Real der_x;
                output Real der_y;
            algorithm
                der_y := 2 * x * der_x;
            end MyFunc_der;
            "#,
        );
    }
}

// ============================================================================
// EXPERIMENT ANNOTATIONS
// ============================================================================

/// Experiment annotations for simulation
mod experiment_annotations {
    use super::*;

    /// Basic experiment annotation
    #[test]
    fn experiment_basic() {
        expect_parse_success(
            r#"
            model Test
                Real x(start = 0);
            equation
                der(x) = 1;
                annotation(experiment(StopTime=10));
            end Test;
            "#,
        );
    }

    /// Full experiment settings
    #[test]
    fn experiment_full() {
        expect_parse_success(
            r#"
            model Test
                Real x(start = 0);
            equation
                der(x) = 1;
                annotation(experiment(
                    StartTime=0,
                    StopTime=10,
                    Interval=0.01,
                    Tolerance=1e-6
                ));
            end Test;
            "#,
        );
    }

    /// Experiment with algorithm selection
    #[test]
    fn experiment_algorithm() {
        expect_parse_success(
            r#"
            model Test
                Real x(start = 0);
            equation
                der(x) = 1;
                annotation(experiment(
                    StopTime=10,
                    __Dymola_Algorithm="Dassl"
                ));
            end Test;
            "#,
        );
    }
}

// ============================================================================
// COMPONENT ANNOTATIONS
// ============================================================================

/// Component-level annotations
mod component_annotations {
    use super::*;

    /// Dialog annotation
    #[test]
    fn component_dialog() {
        expect_parse_success(
            r#"
            model Test
                parameter Real k = 1 annotation(Dialog(group="Parameters", tab="General"));
            equation
            end Test;
            "#,
        );
    }

    /// Evaluate annotation
    #[test]
    fn component_evaluate() {
        expect_parse_success(
            r#"
            model Test
                parameter Integer n = 5 annotation(Evaluate=true);
                Real x[n];
            equation
                for i in 1:n loop
                    x[i] = i;
                end for;
            end Test;
            "#,
        );
    }

    /// HideResult annotation
    #[test]
    fn component_hide_result() {
        expect_parse_success(
            r#"
            model Test
                Real x annotation(HideResult=true);
                Real y;
            equation
                x = 1;
                y = x * 2;
            end Test;
            "#,
        );
    }

    /// choices annotation
    #[test]
    fn component_choices() {
        expect_parse_success(
            r#"
            model Test
                parameter String method = "euler" annotation(
                    choices(
                        choice="euler" "Euler method",
                        choice="rk4" "Runge-Kutta 4"
                    )
                );
            equation
            end Test;
            "#,
        );
    }
}
