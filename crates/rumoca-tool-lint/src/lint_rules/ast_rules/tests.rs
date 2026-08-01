use super::*;

use crate::lint_options::LintOptions;
use crate::{LintLevel as PublicLintLevel, lint};

/// Run one rule over `source`, parsing it the same way [`crate::lint`] does.
fn check(rule: &dyn LintRule, source: &str) -> Vec<LintMessage> {
    let ast = rumoca_compile::parsing::parse_source_to_ast(source, "test.mo")
        .map_err(|error| format!("fixture must parse: {error}"))
        .expect("fixture must parse");
    // Constructing `LintContext` exhaustively here is the guard that keeps the
    // raw-source field from coming back: re-adding it fails this build.
    let ctx = LintContext {
        file_name: "test.mo",
        ast: &ast,
    };
    rule.check(&ctx)
}

fn messages(rule: &dyn LintRule, source: &str) -> Vec<String> {
    check(rule, source)
        .into_iter()
        .map(|message| message.message)
        .collect()
}

// ---------------------------------------------------------------------------
// naming-convention
// ---------------------------------------------------------------------------

#[test]
fn naming_convention_flags_lowercase_class_names() {
    let found = check(&NamingConventionRule, "model myModel Real x; end myModel;");
    assert_eq!(found.len(), 1, "{found:?}");
    assert_eq!(found[0].rule, "naming-convention");
    assert_eq!(found[0].level, LintLevel::Warning);
}

#[test]
fn naming_convention_accepts_pascal_case_class_names() {
    assert!(check(&NamingConventionRule, "model MyModel Real x; end MyModel;").is_empty());
}

#[test]
fn naming_convention_message_text_is_stable() {
    // Asserted verbatim by crates/rumoca-bind-wasm/src/tests.rs.
    let found = check(&NamingConventionRule, "model foo Real x; end foo;");
    assert_eq!(
        found[0].message,
        "model name 'foo' should start with uppercase (PascalCase)"
    );
    assert_eq!(found[0].suggestion.as_deref(), Some("Rename to 'Foo'"));
}

#[test]
fn naming_convention_ignores_class_keywords_inside_comments() {
    let source = "model Outer\n  /* model foo is described here\n     package p too */\n  // record r as well\n  Real x;\nend Outer;\n";
    assert!(
        check(&NamingConventionRule, source).is_empty(),
        "{:?}",
        messages(&NamingConventionRule, source)
    );
}

#[test]
fn naming_convention_ignores_class_keywords_inside_strings() {
    let source =
        "model Outer\n  constant String note = \"package p contains stuff\";\nend Outer;\n";
    assert!(
        check(&NamingConventionRule, source).is_empty(),
        "{:?}",
        messages(&NamingConventionRule, source)
    );
}

#[test]
fn naming_convention_reports_the_class_name_position() {
    let found = check(
        &NamingConventionRule,
        "package Lib\n  model wheelHub\n  end wheelHub;\nend Lib;\n",
    );
    assert_eq!(found.len(), 1, "{found:?}");
    assert_eq!(found[0].line, 2);
    assert_eq!(found[0].column, 9);
}

#[test]
fn naming_convention_ignores_functions_and_types() {
    let source = "package P\n  function f\n  end f;\n  type v = Real;\nend P;\n";
    assert!(check(&NamingConventionRule, source).is_empty());
}

// ---------------------------------------------------------------------------
// missing-documentation
// ---------------------------------------------------------------------------

#[test]
fn missing_documentation_accepts_description_string_after_class_name() {
    let source = "model Foo \"A documented model\"\n  Real x;\nend Foo;\n";
    assert!(
        check(&MissingDocumentationRule, source).is_empty(),
        "{:?}",
        messages(&MissingDocumentationRule, source)
    );
}

#[test]
fn missing_documentation_flags_class_without_description() {
    let found = check(
        &MissingDocumentationRule,
        "model Foo\n  Real x;\nend Foo;\n",
    );
    assert_eq!(found.len(), 1, "{found:?}");
    assert_eq!(found[0].rule, "missing-documentation");
    assert_eq!(found[0].level, LintLevel::Note);
    assert_eq!(
        found[0].message,
        "model 'Foo' is missing a description string"
    );
}

#[test]
fn missing_documentation_ignores_quote_on_previous_line() {
    // A stray string on the preceding line is not a description string.
    let source = "package P\n  constant String s = \"stray\";\n  model Foo\n    Real x;\n  end Foo;\nend P;\n";
    let found = check(&MissingDocumentationRule, source);
    assert_eq!(found.len(), 1, "{found:?}");
    assert_eq!(
        found[0].message,
        "model 'Foo' is missing a description string"
    );
}

#[test]
fn missing_documentation_covers_functions_but_not_packages() {
    let source = "package P\n  function f\n  end f;\n  model M \"doc\"\n  end M;\nend P;\n";
    let found = messages(&MissingDocumentationRule, source);
    assert_eq!(found, vec!["function 'f' is missing a description string"]);
}

// ---------------------------------------------------------------------------
// magic-number
// ---------------------------------------------------------------------------

#[test]
fn magic_number_reads_exponent_literal_as_one_number() {
    let found = messages(&MagicNumberRule, "model C\n  Real y = 3.5e2;\nend C;\n");
    assert_eq!(
        found,
        vec!["Consider extracting '3.5e2' as a named constant"]
    );
}

#[test]
fn magic_number_still_checks_declarations_with_start_modifier() {
    let found = messages(
        &MagicNumberRule,
        "model C\n  Real z(start = 7.0) = 42.0;\nend C;\n",
    );
    assert_eq!(
        found,
        vec!["Consider extracting '42.0' as a named constant"]
    );
}

#[test]
fn magic_number_skips_parameter_and_constant_declarations() {
    let source = "model C\n  parameter Real k = 42.0;\n  constant Real c = 137.0;\nend C;\n";
    assert!(
        check(&MagicNumberRule, source).is_empty(),
        "{:?}",
        messages(&MagicNumberRule, source)
    );
}

#[test]
fn magic_number_skips_annotation_expressions() {
    let source = "model C\n  Real x;\nequation\n  x = 1;\n  annotation(Placement(transformation(extent = {{-10, -10}, {10, 10}})));\nend C;\n";
    assert!(
        check(&MagicNumberRule, source).is_empty(),
        "{:?}",
        messages(&MagicNumberRule, source)
    );
}

#[test]
fn magic_number_ignores_numbers_inside_comments_and_strings() {
    let source = "model C\n  Real x;\n  constant String s = \"pressure 42 bar\";\nequation\n  // set to 42 later\n  x = 1; /* 42 */\nend C;\n";
    assert!(
        check(&MagicNumberRule, source).is_empty(),
        "{:?}",
        messages(&MagicNumberRule, source)
    );
}

#[test]
fn magic_number_reports_the_literal_position() {
    let found = check(
        &MagicNumberRule,
        "model C\n  Real x;\nequation\n  x = 42;\nend C;\n",
    );
    assert_eq!(found.len(), 1, "{found:?}");
    assert_eq!(found[0].line, 4);
    assert_eq!(found[0].column, 7);
    assert_eq!(found[0].level, LintLevel::Help);
}

#[test]
fn magic_number_ignores_array_dimensions_and_indices() {
    let source = "model C\n  Real v[7];\nequation\n  v[5] = 1;\nend C;\n";
    assert!(
        check(&MagicNumberRule, source).is_empty(),
        "{:?}",
        messages(&MagicNumberRule, source)
    );
}

#[test]
fn magic_number_ignores_for_range_bounds() {
    // `3` plays exactly the structural role in `for i in 1:3` that it plays in
    // `x[3]`, so the loop range must be exempt like array dimensions are.
    let source =
        "model C\n  Real x[3];\nequation\n  for i in 1:3 loop\n    x[i] = 0;\n  end for;\nend C;\n";
    assert!(
        check(&MagicNumberRule, source).is_empty(),
        "{:?}",
        messages(&MagicNumberRule, source)
    );
}

#[test]
fn magic_number_still_reports_literals_inside_a_for_loop_body() {
    let source = "model C\n  Real x[3];\nequation\n  for i in 1:3 loop\n    x[i] = 5.5;\n  end for;\nend C;\n";
    assert_eq!(
        messages(&MagicNumberRule, source),
        vec!["Consider extracting '5.5' as a named constant"]
    );
}

// ---------------------------------------------------------------------------
// external-purity-undeclared
// ---------------------------------------------------------------------------

#[test]
fn bare_external_function_is_reported() {
    let source = "function f \"bare\"\n  input Real u;\n  output Real y;\nexternal \"C\" y = my_func(u);\nend f;\n";
    let found = messages(&ExternalPurityRule, source);
    assert_eq!(found.len(), 1, "{found:?}");
    assert!(found[0].contains("treated as impure"), "{found:?}");
}

#[test]
fn declared_external_purity_reports_nothing() {
    for source in [
        "pure function f \"pure\"\n  input Real u;\n  output Real y;\nexternal \"C\" y = my_func(u);\nend f;\n",
        "impure function f \"impure\"\n  input Real u;\n  output Real y;\nexternal \"C\" y = my_func(u);\nend f;\n",
    ] {
        assert!(
            messages(&ExternalPurityRule, source).is_empty(),
            "an explicit prefix is what the rule asks for: {source}"
        );
    }
}

/// The rule is about the external interface, not about functions in general: a
/// Modelica body is pure by default and needs no prefix (MLS §12.3).
#[test]
fn modelica_function_without_a_purity_prefix_reports_nothing() {
    let source =
        "function f \"body\"\n  input Real u;\n  output Real y;\nalgorithm\n  y := u;\nend f;\n";
    assert!(messages(&ExternalPurityRule, source).is_empty());
}

// ---------------------------------------------------------------------------
// end-to-end through `lint`
// ---------------------------------------------------------------------------

#[test]
fn documented_model_reports_nothing_even_at_help_level() {
    let options = LintOptions {
        min_level: PublicLintLevel::Help,
        ..LintOptions::default()
    };
    let found = lint(
        "model Foo \"A documented model\"\n  Real x;\nequation\n  x = 1;\nend Foo;\n",
        "test.mo",
        &options,
    );
    assert!(found.is_empty(), "{found:?}");
}

#[test]
fn comment_prose_produces_no_findings_at_help_level() {
    let options = LintOptions {
        min_level: PublicLintLevel::Help,
        ..LintOptions::default()
    };
    let source = "model Foo \"doc\"\n  /* model foo has gain 42 */\n  Real x;\nequation\n  x = 1;\nend Foo;\n";
    let found = lint(source, "test.mo", &options);
    assert!(found.is_empty(), "{found:?}");
}
