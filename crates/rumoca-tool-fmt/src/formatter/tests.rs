use super::*;
use crate::format_options::{FormatProfile, LineEnding};

#[test]
fn test_format_error_uses_explicit_source_name() {
    let source = "model M Real x end M;";
    let err = format_with_source_name(source, &FormatOptions::default(), "test_input.mo")
        .expect_err("expected syntax error");
    let rendered = err.to_string();
    assert!(rendered.contains("test_input.mo"));
}

#[test]
fn test_default_dymola_format_preserves_compact_equation_spacing() {
    let source = "model Ball\n  Real x(start=10);\nequation\n  x=1;\n  y = 2;\nend Ball;";
    let expected = "model Ball\n  Real x(start=10);\nequation\n  x=1;\n  y = 2;\nend Ball;\n";
    let formatted = format(source, &FormatOptions::default()).expect("format");
    assert_eq!(formatted, expected);
}

#[test]
fn test_default_dymola_format_preserves_compact_statement_assignment_spacing() {
    let source = "function f\noutput Real y;\nalgorithm\ny:=1;\nend f;";
    let expected = "function f\n  output Real y;\nalgorithm\ny:=1;\nend f;\n";
    let formatted = format(source, &FormatOptions::default()).expect("format");
    assert_eq!(formatted, expected);
}

#[test]
fn test_default_dymola_format_preserves_compact_statement_call_output_spacing() {
    let source = "function f\noutput Real a;\noutput Real b;\nalgorithm\n(a,b):=g();\nend f;";
    let expected =
        "function f\n  output Real a;\n  output Real b;\nalgorithm\n(a,b):=g();\nend f;\n";
    let formatted = format(source, &FormatOptions::default()).expect("format");
    assert_eq!(formatted, expected);
}

#[test]
fn test_default_dymola_format_preserves_compact_expression_list_commas() {
    let source = "model C\n  Real x = f(1,2, 3, a=4, b =5);\n  Real y[3] = {1,2, 3};\nend C;";
    let expected = "model C\n  Real x = f(1,2, 3, a=4, b =5);\n  Real y[3] = {1,2, 3};\nend C;\n";
    let formatted = format(source, &FormatOptions::default()).expect("format");
    assert_eq!(formatted, expected);
}

#[test]
fn test_default_dymola_format_preserves_argument_assignment_spacing() {
    let source = "model C\n  Real x(start = 1);\nequation\n  y = f(a = 1, b=2);\nend C;";
    let expected = "model C\n  Real x(start = 1);\nequation\n  y = f(a = 1, b=2);\nend C;\n";
    let formatted = format(source, &FormatOptions::default()).expect("format");
    assert_eq!(formatted, expected);
}

#[test]
fn test_default_dymola_format_preserves_component_binding_spacing() {
    let source = "model C\n  parameter Real k=1;\n  Real x = k;\nend C;";
    let expected = "model C\n  parameter Real k=1;\n  Real x = k;\nend C;\n";
    let formatted = format(source, &FormatOptions::default()).expect("format");
    assert_eq!(formatted, expected);
}

#[test]
fn test_default_dymola_format_preserves_compact_binary_operator_spacing() {
    let source = "model C\n  Real x;\nequation\n  x=1+2*3;\nend C;";
    let expected = "model C\n  Real x;\nequation\n  x=1+2*3;\nend C;\n";
    let formatted = format(source, &FormatOptions::default()).expect("format");
    assert_eq!(formatted, expected);
}

#[test]
fn test_default_dymola_format_preserves_compact_connect_commas() {
    let source = "connector Pin\n  Real v;\n  flow Real i;\nend Pin;\nmodel C\n  Pin a;\n  Pin b;\nequation\n  connect(a,b);\nend C;";
    let expected = "connector Pin\n  Real v;\n  flow Real i;\nend Pin;\nmodel C\n  Pin a;\n  Pin b;\nequation\n  connect(a,b);\nend C;\n";
    let formatted = format(source, &FormatOptions::default()).expect("format");
    assert_eq!(formatted, expected);
}

#[test]
fn test_dymola_format_normalizes_class_keyword_name_spacing_from_ast() {
    let source = "package P\nmodel   C\nReal x;\nend C;\nend P;";
    let expected = "package P\nmodel C\n    Real x;\nend C;\nend P;\n";
    let formatted = format(source, &FormatOptions::default()).expect("format");
    assert_eq!(formatted, expected);
}

#[test]
fn test_dymola_format_normalizes_class_prefix_spacing_from_ast() {
    let source = "package P\npartial   model   C\nend C;\nimpure   function   f\nend f;\noperator   record   R\nend R;\nexpandable   connector   Bus\nend Bus;\nreplaceable   model   Medium\nend Medium;\nfinal   model   Fixed\nend Fixed;\nend P;";
    let expected = "package P\npartial model C\nend C;\nimpure function f\nend f;\noperator record R\nend R;\nexpandable connector Bus\nend Bus;\nreplaceable model Medium\nend Medium;\nfinal model Fixed\nend Fixed;\nend P;\n";
    let formatted = format(source, &FormatOptions::default()).expect("format");
    assert_eq!(formatted, expected);
}

#[test]
fn test_dymola_format_normalizes_class_end_name_spacing_from_ast() {
    let source = "model C\nReal x;\nend   C;";
    let expected = "model C\n  Real x;\nend C;\n";
    let formatted = format(source, &FormatOptions::default()).expect("format");
    assert_eq!(formatted, expected);
}

#[test]
fn test_dymola_format_preserves_type_alias_assignment_spacing() {
    let source = "type Voltage=Real;";
    let expected = "type Voltage=Real;\n";
    let formatted = format(source, &FormatOptions::default()).expect("format");
    assert_eq!(formatted, expected);
}

#[test]
fn test_canonical_format_normalizes_type_alias_assignment_from_ast() {
    let source =
        "type   Voltage=   Real;\npackage Medium= Modelica.Media.Interfaces.PartialMedium;";
    let expected =
        "type Voltage = Real;\n  package Medium = Modelica.Media.Interfaces.PartialMedium;\n";
    let formatted = format(
        source,
        &FormatOptions::for_profile(FormatProfile::Canonical),
    )
    .expect("format");
    assert_eq!(formatted, expected);
}

#[test]
fn test_canonical_profile_normalizes_type_alias_array_modifier_spacing() {
    let source = "type RotationSequence = Modelica.Icons.TypeInteger[3] (min={1,1,1});";
    let expected = "type RotationSequence = Modelica.Icons.TypeInteger[3](min={1, 1, 1});\n";
    let formatted = format(
        source,
        &FormatOptions::for_profile(FormatProfile::Canonical),
    )
    .expect("format");
    assert_eq!(formatted, expected);
}

#[test]
fn test_dymola_format_normalizes_component_type_name_spacing_from_ast() {
    let source = "model C\nReal   x;\nend C;";
    let expected = "model C\n  Real x;\nend C;\n";
    let formatted = format(source, &FormatOptions::default()).expect("format");
    assert_eq!(formatted, expected);
}

#[test]
fn test_dymola_format_normalizes_component_prefix_spacing_from_ast() {
    let source = "model C\ninput   Real   u;\nparameter   Real   k;\nfinal   constant   Real c;\n  replaceable   Real r;\n  inner   outer   StateGraphRoot root;\nend C;";
    let expected = "model C\n  input Real   u;\n  parameter Real   k;\n  final constant Real c;\n  replaceable Real r;\n  inner outer StateGraphRoot root;\nend C;\n";
    let formatted = format(source, &FormatOptions::default()).expect("format");
    assert_eq!(formatted, expected);
}

#[test]
fn test_dymola_format_preserves_aligned_component_declarations() {
    let source = "model C\n  SI.Velocity v;\n  SI.Length   l;\nend C;";
    let expected = "model C\n  SI.Velocity v;\n  SI.Length   l;\nend C;\n";
    let formatted = format(source, &FormatOptions::default()).expect("format");
    assert_eq!(formatted, expected);
}

#[test]
fn test_dymola_format_normalizes_qualified_name_separators_from_ast() {
    let source = "within Modelica . Blocks;\nmodel C\n  Modelica . Units . SI . Angle phi;\nend C;";
    let expected = "within Modelica.Blocks;\nmodel C\n  Modelica.Units.SI.Angle phi;\nend C;\n";
    let formatted = format(source, &FormatOptions::default()).expect("format");
    assert_eq!(formatted, expected);
}

#[test]
fn test_dymola_format_normalizes_import_spacing_from_ast() {
    let source = "model C\nimport SI   =   Modelica . Units . SI;\nimport Modelica.Utilities.{Streams,  Files};\nend C;";
    let expected = "model C\nimport SI = Modelica.Units.SI;\nimport Modelica.Utilities.{Streams, Files};\nend C;\n";
    let formatted = format(source, &FormatOptions::default()).expect("format");
    assert_eq!(formatted, expected);
}

#[test]
fn test_dymola_format_normalizes_extends_and_constrainedby_spacing_from_ast() {
    let source =
        "model C\n  extends   Base;\n  replaceable Real r constrainedby   PartialReal;\nend C;";
    let expected =
        "model C\n  extends Base;\n  replaceable Real r constrainedby PartialReal;\nend C;\n";
    let formatted = format(source, &FormatOptions::default()).expect("format");
    assert_eq!(formatted, expected);
}

#[test]
fn test_dymola_format_normalizes_for_index_spacing_from_ast() {
    let source = "model C\nalgorithm\nfor i   in   1:3 loop\nend for;\nend C;";
    let expected = "model C\nalgorithm\nfor i in 1:3 loop\nend for;\nend C;\n";
    let formatted = format(source, &FormatOptions::default()).expect("format");
    assert_eq!(formatted, expected);
}

#[test]
fn test_dymola_format_can_normalize_equation_spacing() {
    let source = "model Ball\n  Real x(start=10);\nequation\n  x=1;\n  y =2;\nend Ball;";
    let expected = "model Ball\n  Real x(start=10);\nequation\n  x = 1;\n  y = 2;\nend Ball;\n";
    let formatted = format(
        source,
        &FormatOptions {
            normalize_equation_spacing: true,
            ..FormatOptions::default()
        },
    )
    .expect("format");
    assert_eq!(formatted, expected);
}

#[test]
fn test_dymola_format_can_normalize_component_binding_spacing() {
    let source = "model C\nparameter Real k=1;\nReal y(start= 1)=k;\nend C;";
    let expected = "model C\n  parameter Real k = 1;\n  Real y(start= 1) = k;\nend C;\n";
    let formatted = format(
        source,
        &FormatOptions {
            normalize_equation_spacing: true,
            ..FormatOptions::default()
        },
    )
    .expect("format");
    assert_eq!(formatted, expected);
}

#[test]
fn test_dymola_format_can_normalize_statement_assignment_spacing() {
    let source = "function f\noutput Real y;\nalgorithm\ny:=1;\nz :=2;\nend f;";
    let expected = "function f\n  output Real y;\nalgorithm\ny := 1;\nz := 2;\nend f;\n";
    let formatted = format(
        source,
        &FormatOptions {
            normalize_equation_spacing: true,
            ..FormatOptions::default()
        },
    )
    .expect("format");
    assert_eq!(formatted, expected);
}

#[test]
fn test_dymola_format_can_normalize_statement_call_output_spacing() {
    let source =
        "function f\noutput Real a;\noutput Real b;\nalgorithm\n(a,b):=g();\n(c, d) :=h();\nend f;";
    let expected = "function f\n  output Real a;\n  output Real b;\nalgorithm\n(a,b) := g();\n(c, d) := h();\nend f;\n";
    let formatted = format(
        source,
        &FormatOptions {
            normalize_equation_spacing: true,
            ..FormatOptions::default()
        },
    )
    .expect("format");
    assert_eq!(formatted, expected);
}

#[test]
fn test_format_dymola_profile_preserves_modifier_spacing_and_normalizes_equations() {
    let source = "model DymolaStyle\r\n  Real x(start=1);  \r\n  Real y;\r\n\r\nequation\r\n  der(x)=y;\r\n  y=-9.8;\r\nend DymolaStyle;";
    let expected = "model DymolaStyle\r\n  Real x(start=1);\r\n  Real y;\r\n\r\nequation\r\n  der(x) = y;\r\n  y = -9.8;\r\nend DymolaStyle;\r\n";
    let formatted = format(
        source,
        &FormatOptions {
            profile: FormatProfile::Dymola,
            normalize_equation_spacing: true,
            ..FormatOptions::default()
        },
    )
    .expect("format");
    assert_eq!(formatted, expected);
}

#[test]
fn test_canonical_profile_normalizes_spacing_and_indentation_defaults() {
    let source = "model C\nReal x;\nequation\nx=1;\nend C;";
    let formatted = format(
        source,
        &FormatOptions::for_profile(FormatProfile::Canonical),
    )
    .expect("format");
    assert_eq!(
        formatted,
        "model C\n  Real x;\nequation\n  x = 1;\nend C;\n"
    );
}

#[test]
fn test_canonical_profile_normalizes_expression_list_commas() {
    let source = "function f\noutput Real a;\noutput Real b;\nalgorithm\n(a,b):=g(1,2, 3);\nx := h(a=1,b=2);\ny := {1,2, 3};\nend f;";
    let expected = "function f\n  output Real a;\n  output Real b;\nalgorithm\n  (a, b) := g(1, 2, 3);\n  x := h(a=1, b=2);\n  y := {1, 2, 3};\nend f;\n";
    let formatted = format(
        source,
        &FormatOptions::for_profile(FormatProfile::Canonical),
    )
    .expect("format");
    assert_eq!(formatted, expected);
}

#[test]
fn test_canonical_profile_compacts_argument_assignment_spacing() {
    let source = "model C\n  extends Base(use_reset = false, k= 2, u(unit = \"K\"),y(unit = \"s\"));\nReal y(start = 1, fixed= true) = 3;\nequation\ny = f(a = 1,b = 2);\nend C;";
    let expected = "model C\n  extends Base(use_reset=false, k=2, u(unit=\"K\"), y(unit=\"s\"));\n  Real y(start=1, fixed=true) = 3;\nequation\n  y = f(a=1, b=2);\nend C;\n";
    let formatted = format(
        source,
        &FormatOptions::for_profile(FormatProfile::Canonical),
    )
    .expect("format");
    assert_eq!(formatted, expected);
}

#[test]
fn test_canonical_profile_removes_call_opening_parenthesis_padding() {
    let source = "model C\n  extends Base ( k = 1);\n  Real x(        fixed = false);\nequation\n  assert( x > 0, \"positive\");\n  x = f( x);\nend C;";
    let expected = "model C\n  extends Base(k=1);\n  Real x(fixed=false);\nequation\n  assert(x > 0, \"positive\");\n  x = f(x);\nend C;\n";
    let formatted = format(
        source,
        &FormatOptions::for_profile(FormatProfile::Canonical),
    )
    .expect("format");
    assert_eq!(formatted, expected);
}

#[test]
fn test_canonical_profile_removes_padding_before_parenthesized_first_argument() {
    let source = "model C\n  Real x;\nequation\n  x = integer( (superSample(x)) + 0.5);\nend C;";
    let expected = "model C\n  Real x;\nequation\n  x = integer((superSample(x)) + 0.5);\nend C;\n";
    let formatted = format(
        source,
        &FormatOptions::for_profile(FormatProfile::Canonical),
    )
    .expect("format");
    assert_eq!(formatted, expected);
}

#[test]
fn test_canonical_profile_removes_opening_delimiter_padding() {
    let source = "model C\n  Real x;\n  Real Q1;\n  Real Q2;\nequation\n  x = exp(( 1)) + sum({ Q1,Q2});\nend C;";
    let expected = "model C\n  Real x;\n  Real Q1;\n  Real Q2;\nequation\n  x = exp((1)) + sum({Q1, Q2});\nend C;\n";
    let formatted = format(
        source,
        &FormatOptions::for_profile(FormatProfile::Canonical),
    )
    .expect("format");
    assert_eq!(formatted, expected);
}

#[test]
fn test_canonical_profile_normalizes_type_level_array_component_spacing() {
    let source = "model C\n  Real[3]    x;\nend C;";
    let expected = "model C\n  Real[3] x;\nend C;\n";
    let formatted = format(
        source,
        &FormatOptions::for_profile(FormatProfile::Canonical),
    )
    .expect("format");
    assert_eq!(formatted, expected);
}

#[test]
fn test_canonical_profile_normalizes_binding_after_multiline_component_modifiers() {
    let source = "block Pulse\n  parameter Real width(\n    final min=0,\n    final max=100)=50;\nend Pulse;";
    let expected = "block Pulse\n  parameter Real width(\n    final min=0,\n    final max=100) = 50;\nend Pulse;\n";
    let formatted = format(
        source,
        &FormatOptions::for_profile(FormatProfile::Canonical),
    )
    .expect("format");
    assert_eq!(formatted, expected);
}

#[test]
fn test_canonical_profile_normalizes_redeclare_type_name_spacing() {
    let source = "model C\n  extends Base(redeclare P.T     x, redeclare model HeatTransfer = P.Ideal (k=1));\nend C;";
    let expected = "model C\n  extends Base(redeclare P.T x, redeclare model HeatTransfer=P.Ideal(k=1));\nend C;\n";
    let formatted = format(
        source,
        &FormatOptions::for_profile(FormatProfile::Canonical),
    )
    .expect("format");
    assert_eq!(formatted, expected);
}

#[test]
fn test_dymola_profile_preserves_redeclare_type_name_spacing() {
    let source = "model C\n  extends Base(redeclare P.T     x);\nend C;";
    let formatted = format(source, &FormatOptions::default()).expect("format");
    assert_eq!(formatted, format!("{source}\n"));
}

#[test]
fn test_canonical_profile_normalizes_external_function_argument_commas() {
    let source = "function f\n  input Real tableID;\n  input Integer icol;\n  input Real timeIn;\n  output Real y;\n  external \"C\" y = getValue(tableID,icol, timeIn);\nend f;";
    let expected = "function f\n  input Real tableID;\n  input Integer icol;\n  input Real timeIn;\n  output Real y;\n  external \"C\" y = getValue(tableID, icol, timeIn);\nend f;\n";
    let formatted = format(
        source,
        &FormatOptions::for_profile(FormatProfile::Canonical),
    )
    .expect("format");
    assert_eq!(formatted, expected);
}

#[test]
fn test_dymola_profile_preserves_call_opening_parenthesis_padding() {
    let source = "model C\n  Real x(        fixed = false);\nequation\n  assert( x > 0, \"positive\");\n  x = f( x);\nend C;";
    let formatted = format(source, &FormatOptions::default()).expect("format");
    assert_eq!(formatted, format!("{source}\n"));
}

#[test]
fn test_canonical_profile_normalizes_binary_operator_spacing() {
    let source = "model C\nReal x;\nReal y[3];\nReal Q1;\nBoolean b;\nequation\nx=1+2*3 + y[3]-Q1 + (y[1]+y[2])+0.5;\nb=x<4 and  y[1]>=5;\nend C;";
    let expected = "model C\n  Real x;\n  Real y[3];\n  Real Q1;\n  Boolean b;\nequation\n  x = 1 + 2 * 3 + y[3] - Q1 + (y[1] + y[2]) + 0.5;\n  b = x < 4 and y[1] >= 5;\nend C;\n";
    let formatted = format(
        source,
        &FormatOptions::for_profile(FormatProfile::Canonical),
    )
    .expect("format");
    assert_eq!(formatted, expected);
}

#[test]
fn test_canonical_profile_normalizes_same_line_statement_separator() {
    let source = "function f\noutput Real x;\nalgorithm\nassert(true, \"ok\");x:=1;\nend f;";
    let expected =
        "function f\n  output Real x;\nalgorithm\n  assert(true, \"ok\"); x := 1;\nend f;\n";
    let formatted = format(
        source,
        &FormatOptions::for_profile(FormatProfile::Canonical),
    )
    .expect("format");
    assert_eq!(formatted, expected);
}

#[test]
fn test_canonical_profile_normalizes_component_reference_dot_spacing() {
    let source = "model C\n  Real x;\nequation\n  x = sub. u2 + step. y;\nend C;";
    let expected = "model C\n  Real x;\nequation\n  x = sub.u2 + step.y;\nend C;\n";
    let formatted = format(
        source,
        &FormatOptions::for_profile(FormatProfile::Canonical),
    )
    .expect("format");
    assert_eq!(formatted, expected);
}

#[test]
fn test_dymola_profile_preserves_component_reference_dot_spacing() {
    let source = "model C\n  Real x;\nequation\n  x = sub. u2 + step. y;\nend C;";
    let formatted = format(source, &FormatOptions::default()).expect("format");
    assert_eq!(formatted, format!("{source}\n"));
}

#[test]
fn test_canonical_profile_normalizes_connect_commas() {
    let source = "connector Pin\nReal v;\nflow Real i;\nend Pin;\nmodel C\nPin a;\nPin b;\nequation\nconnect(a,b);\nend C;";
    let expected = "connector Pin\n  Real v;\n  flow Real i;\nend Pin;\nmodel C\n  Pin a;\n  Pin b;\nequation\n  connect(a, b);\nend C;\n";
    let formatted = format(
        source,
        &FormatOptions::for_profile(FormatProfile::Canonical),
    )
    .expect("format");
    assert_eq!(formatted, expected);
}

#[test]
fn test_canonical_profile_normalizes_array_subscript_commas() {
    let source = "model C\nReal x[3,3];\nequation\nx[1,2]=x[2, 1];\nend C;";
    let expected = "model C\n  Real x[3, 3];\nequation\n  x[1, 2] = x[2, 1];\nend C;\n";
    let formatted = format(
        source,
        &FormatOptions::for_profile(FormatProfile::Canonical),
    )
    .expect("format");
    assert_eq!(formatted, expected);
}

#[test]
fn test_canonical_profile_normalizes_matrix_row_commas_but_dymola_preserves_alignment() {
    let source =
        "model C\n  parameter Real M[2, 3] = [\n    -d,   d,  -d;\n    Cm, -Cm,  Cm\n  ];\nend C;";
    let dymola = format(source, &FormatOptions::default()).expect("format dymola");
    assert_eq!(dymola, format!("{source}\n"));

    let canonical = format(
        source,
        &FormatOptions::for_profile(FormatProfile::Canonical),
    )
    .expect("format canonical");
    assert_eq!(
        canonical,
        "model C\n  parameter Real M[2, 3] = [\n    -d, d, -d;\n    Cm, -Cm, Cm\n  ];\nend C;\n"
    );
}

#[test]
fn test_canonical_profile_reports_full_classified_gap_coverage() {
    let options = FormatOptions::for_profile(FormatProfile::Canonical);
    let source = "within Modelica . Blocks;\ntype Voltage=Real;\nmodel C\nparameter Real k=1;\nReal x[3,3];\nBoolean b;\nequation\nx[1,2]=1+2*3;\nb=x[2, 1]<4 and  y>=5;\nconnect(a,b);\nassert( b, \"ok\");\nalgorithm\n(a,b):=g(1,2);\nend C;";
    let report = format_coverage_report(source, &options).expect("coverage");
    assert!(report.eligible_gaps > 0, "coverage report should see gaps");
    assert_eq!(report.covered_gaps, report.eligible_gaps);
    assert_eq!(report.coverage_percent(), 100.0);
    assert!(
        report
            .categories
            .iter()
            .any(|entry| entry.category == FormatCoverageCategory::BinaryOperator)
    );
    assert!(
        report
            .categories
            .iter()
            .any(|entry| entry.category == FormatCoverageCategory::ComponentBindingAssignment)
    );
    assert!(
        report
            .categories
            .iter()
            .any(|entry| entry.category == FormatCoverageCategory::ArraySubscriptComma)
    );
    assert!(
        report
            .categories
            .iter()
            .any(|entry| entry.category == FormatCoverageCategory::TypeAliasAssignment)
    );
    assert!(
        report
            .categories
            .iter()
            .any(|entry| entry.category == FormatCoverageCategory::OpeningParenthesisPadding)
    );
}

#[test]
fn test_format_dymola_profile_preserves_dymola_annotation_style() {
    // Patterns taken from Dymola-authored ModelicaServices/Physiolibrary-style
    // sources: compact modifiers, `annotation (` spacing, graphic extents,
    // experiment metadata, and `__Dymola_*` annotations.
    let source = r#"model DymolaStyle
  parameter Boolean LimitBackflow=false;
  Real passableVariable(start=0, final unit="1")
  "Auxiliary variable" annotation(HideResult = not LimitBackflow);
  Boolean open(start = true) annotation(HideResult = true);
equation
  open = passableVariable > Modelica.Constants.eps;
  annotation (Placement(transformation(extent={{10,-10},{30,10}}),
  iconTransformation(extent={{30,-10},{50,10}})),
  experiment(
  StopTime=3,
  Interval=0.001,
  Tolerance=1e-05,
  __Dymola_Algorithm="Cvode"));
end DymolaStyle;
"#;
    let formatted = format(
        source,
        &FormatOptions {
            profile: FormatProfile::Dymola,
            ..FormatOptions::default()
        },
    )
    .expect("format");
    assert!(formatted.contains("LimitBackflow=false"));
    assert!(formatted.contains("annotation(HideResult = not LimitBackflow)"));
    assert!(formatted.contains("Boolean open(start = true) annotation(HideResult = true);"));
    assert!(formatted.contains("open = passableVariable > Modelica.Constants.eps;"));
}

#[test]
fn test_multiline_string_is_preserved() {
    let source = r#"model C
  annotation(Documentation(info="<html>
<p>This function returns <em>re</em> and <em>im</em>.</p>
</html>"));
end C;
"#;
    let formatted = format(source, &FormatOptions::default()).expect("format");
    assert_eq!(formatted, source);
}

#[test]
fn test_trim_trailing_whitespace_preserves_multiline_string_text() {
    let source = "model C\n  Real x;  \n  annotation(Documentation(info=\"<html>\n<p>literal trailing space </p> \n</html>\"));\nend C;\n";
    let formatted = format(
        source,
        &FormatOptions {
            trim_trailing_whitespace: true,
            ..FormatOptions::default()
        },
    )
    .expect("format");
    assert_eq!(
        formatted,
        "model C\n  Real x;\n  annotation(Documentation(info=\"<html>\n<p>literal trailing space </p> \n</html>\"));\nend C;\n"
    );
}

#[test]
fn test_trim_trailing_whitespace_preserves_multiline_block_comment() {
    let source = "model C\n  /* keep aligned comment text  \n     and its blank spacer   \n  */\n  Real x;  \nend C;\n";
    let formatted = format(
        source,
        &FormatOptions {
            trim_trailing_whitespace: true,
            ..FormatOptions::default()
        },
    )
    .expect("format");
    assert_eq!(
        formatted,
        "model C\n  /* keep aligned comment text  \n     and its blank spacer   \n  */\n  Real x;\nend C;\n"
    );
}

#[test]
fn test_quoted_operator_identifier_is_preserved() {
    let source = r#"operator '-'
end '-';
"#;
    let formatted = format(source, &FormatOptions::default()).expect("format");
    assert_eq!(formatted, source);
}

#[test]
fn test_common_options_can_preserve_trailing_final_newline_and_crlf() {
    let source = "model C\r\n  Real x(start=1);  \r\nequation\r\n  x=1;\r\nend C;";
    let formatted = format(
        source,
        &FormatOptions {
            normalize_equation_spacing: false,
            trim_trailing_whitespace: false,
            insert_final_newline: false,
            line_ending: LineEnding::Crlf,
            ..FormatOptions::default()
        },
    )
    .expect("format");
    assert_eq!(formatted, source);
}

#[test]
fn test_common_options_normalize_indentation_with_tabs() {
    let source = "model C\nReal x;\nequation\nx=1;\nend C;\n";
    let formatted = format(
        source,
        &FormatOptions {
            normalize_indentation: true,
            normalize_equation_spacing: true,
            use_tabs: true,
            ..FormatOptions::default()
        },
    )
    .expect("format");
    assert_eq!(
        formatted,
        "model C\n\tReal x;\nequation\n\tx = 1;\nend C;\n"
    );
}

#[test]
fn test_default_repairs_missing_structural_indentation() {
    let source = "model C\nReal x;\nequation\nx=1;\nend C;";
    let formatted = format(source, &FormatOptions::default()).expect("format");
    assert_eq!(formatted, "model C\n  Real x;\nequation\nx=1;\nend C;\n");
}

#[test]
fn test_repair_missing_indentation_can_be_disabled() {
    let source = "model C\nReal x;\nequation\nx=1;\nend C;";
    let formatted = format(
        source,
        &FormatOptions {
            repair_missing_indentation: false,
            ..FormatOptions::default()
        },
    )
    .expect("format");
    assert_eq!(formatted, "model C\nReal x;\nequation\nx=1;\nend C;\n");
}

#[test]
fn test_normalize_indentation_preserves_continuation_alignment() {
    let source = "model C\nparameter Real x=1\n  annotation(Evaluate=true);\nequation\nif x > 0 or\n       x < 2 then\nx=1;\nend if;\nend C;\n";
    let formatted = format(
        source,
        &FormatOptions {
            normalize_indentation: true,
            normalize_equation_spacing: true,
            ..FormatOptions::default()
        },
    )
    .expect("format");
    assert_eq!(
        formatted,
        "model C\n  parameter Real x = 1\n  annotation(Evaluate=true);\nequation\n  if x > 0 or\n       x < 2 then\n    x = 1;\n  end if;\nend C;\n"
    );
}

#[test]
fn test_normalize_indentation_preserves_conditional_expression_alignment() {
    let source = "model C\nparameter Integer nx = if filterType == LowPass or\n                          filterType == HighPass then\n                          order else 2*order;\nparameter Real y;\nend C;\n";
    let formatted = format(
        source,
        &FormatOptions {
            normalize_indentation: true,
            ..FormatOptions::default()
        },
    )
    .expect("format");
    assert_eq!(
        formatted,
        "model C\n  parameter Integer nx = if filterType == LowPass or\n                          filterType == HighPass then\n                          order else 2*order;\n  parameter Real y;\nend C;\n"
    );
}

#[test]
fn test_normalize_indentation_closes_multiline_statement_condition() {
    let source =
        "model C\nReal x;\nequation\nif x > 0 or\n       x < 2 then\nx=1;\nend if;\nend C;\n";
    let formatted = format(
        source,
        &FormatOptions {
            normalize_indentation: true,
            normalize_equation_spacing: true,
            ..FormatOptions::default()
        },
    )
    .expect("format");
    assert_eq!(
        formatted,
        "model C\n  Real x;\nequation\n  if x > 0 or\n       x < 2 then\n    x = 1;\n  end if;\nend C;\n"
    );
}

#[test]
fn test_normalize_indentation_preserves_comment_anchored_equation_layout() {
    let source = "model C\nReal x;\nReal y;\nequation\nif x > 0 then\n     /* Dense filter equations keep local alignment:\n        real poles and complex poles are documented here. */\n     for i in 1:2 loop\n        y = y + i;\n     end for;\nelseif x < 0 then\n     // Alternative branch keeps generated alignment.\n       y = -x;\nend if;\nend C;\n";
    let formatted = format(
        source,
        &FormatOptions {
            normalize_indentation: true,
            ..FormatOptions::default()
        },
    )
    .expect("format");
    assert_eq!(
        formatted,
        "model C\n  Real x;\n  Real y;\nequation\n  if x > 0 then\n     /* Dense filter equations keep local alignment:\n        real poles and complex poles are documented here. */\n     for i in 1:2 loop\n        y = y + i;\n     end for;\n  elseif x < 0 then\n     // Alternative branch keeps generated alignment.\n       y = -x;\n  end if;\nend C;\n"
    );
}

#[test]
fn test_normalize_indentation_handles_operator_classes() {
    let source = "record Complex\nencapsulated operator '-'\nfunction negate\ninput Real x;\nalgorithm\nx := -x;\nend negate;\nend '-';\nend Complex;\n";
    let formatted = format(
        source,
        &FormatOptions {
            normalize_indentation: true,
            ..FormatOptions::default()
        },
    )
    .expect("format");
    assert_eq!(
        formatted,
        "record Complex\n  encapsulated operator '-'\n    function negate\n      input Real x;\n    algorithm\n      x := -x;\n    end negate;\n  end '-';\nend Complex;\n"
    );
}
