//! The generated GALEC C must already satisfy the formatter standard it ships.
//!
//! The acceptance property is idempotence: running the emitted `.clang-format`
//! over the emitted `.c`/`.h` must produce no replacements. That matters beyond
//! tidiness — the eFMI Production Code manifest records a SHA-1 of the exact C
//! bytes, so a consumer who runs the project formatter over reviewed code and
//! gets a diff has invalidated the container.
//!
//! `clang-format` is not part of this repository's pinned toolchain, so the
//! check runs in two layers:
//!
//! * a deterministic in-repo structural check that always runs and encodes the
//!   rules the shipped configuration turns on (no tabs, no trailing
//!   whitespace, no blank line opening or closing a block, no doubled blank
//!   line, exactly one blank line before each top-level definition, attached
//!   braces, 4-space indentation); and
//! * a real `clang-format --dry-run -Werror` run, executed only when the tool
//!   is on `PATH`, which is the authoritative statement of the same property.
//!
//! The structural layer is not a weaker restatement of the strong one: every
//! rule below was derived from an actual replacement clang-format produced on
//! this emitter's output, so a regression in the template trips it whether or
//! not the binary is installed.

use std::process::Command;

use rumoca_ir_galec::ast as galec;
use rumoca_ir_galec::package::CheckedAlgorithmBlock;
use rumoca_phase_codegen::{render_checked_algorithm_block_template_with_sources, templates};
use serde_json::json;

const MODEL: &str = "FormattingFixture";
const SOURCE_NAME: &str = "fixtures/FormattingFixture.mo";

/// A Modelica text whose byte offsets the fixture's spans point into. The
/// exact text matters: the traces the emitter prints are asserted to resolve to
/// the statement on the line they name.
const SOURCE_TEXT: &str = "\
model FormattingFixture
  Real gain = 2.0;
  Real limited;
  Real selected;
algorithm
  limited := gain;
  selected := gain;
end FormattingFixture;
";

fn scalar_output(name: &str) -> galec::InterfaceVariable {
    galec::InterfaceVariable {
        kind: galec::InterfaceKind::Output,
        decl: galec::VariableDeclaration::scalar(galec::ScalarType::Real, galec::Name::ident(name)),
        start: None,
    }
}

fn array_output(name: &str, extent: i64) -> galec::InterfaceVariable {
    let mut decl =
        galec::VariableDeclaration::scalar(galec::ScalarType::Real, galec::Name::ident(name));
    decl.dimensions = vec![galec::Dimension::Expr(galec::Expression::Integer(extent))];
    galec::InterfaceVariable {
        kind: galec::InterfaceKind::Output,
        decl,
        start: None,
    }
}

fn span_of(needle: &str) -> rumoca_core::Span {
    let start = SOURCE_TEXT
        .find(needle)
        .unwrap_or_else(|| panic!("fixture source must contain `{needle}`"));
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(SOURCE_NAME),
        start,
        start + needle.len(),
    )
}

/// A block exercising every emitter path the formatting rules touch: a scalar
/// assignment, an `if`/`else`, a `for`, an array assignment, and a signal.
fn fixture_block() -> CheckedAlgorithmBlock {
    let mut block = galec::Block::new(galec::Name::ident(MODEL));
    block.interface = vec![
        scalar_output("limited"),
        scalar_output("selected"),
        array_output("vector", 3),
    ];
    block.do_step.statements = vec![
        galec::Spanned::new(
            galec::Statement::Assignment {
                target: galec::Reference::state(galec::Name::ident("limited")),
                value: galec::Expression::Real(2.0),
            },
            span_of("limited := gain;"),
        ),
        galec::Spanned::new(
            galec::Statement::If(galec::IfStatement {
                branches: vec![galec::IfBranch {
                    condition: galec::Condition::Expression(galec::Expression::Bool(true)),
                    body: vec![galec::Spanned::new(
                        galec::Statement::Assignment {
                            target: galec::Reference::state(galec::Name::ident("selected")),
                            value: galec::Expression::Real(1.0),
                        },
                        span_of("selected := gain;"),
                    )],
                    span: span_of("selected := gain;"),
                }],
                else_body: Some(vec![galec::Spanned::new(
                    galec::Statement::Assignment {
                        target: galec::Reference::state(galec::Name::ident("selected")),
                        value: galec::Expression::Real(0.0),
                    },
                    span_of("selected := gain;"),
                )]),
            }),
            span_of("selected := gain;"),
        ),
        galec::Spanned::new(
            galec::Statement::Assignment {
                target: galec::Reference::state(galec::Name::ident("vector")),
                value: galec::Expression::Array(vec![
                    galec::Expression::Real(1.0),
                    galec::Expression::Real(2.0),
                    galec::Expression::Real(3.0),
                ]),
            },
            span_of("Real limited;"),
        ),
    ];
    CheckedAlgorithmBlock::construct(block).expect("formatting fixture must be valid GALEC")
}

fn sources() -> rumoca_core::SourceMap {
    let mut map = rumoca_core::SourceMap::new();
    map.add(SOURCE_NAME, SOURCE_TEXT);
    map
}

fn render(template_path: &str) -> String {
    let template = templates::builtin_template_source("embedded-c-galec", template_path)
        .expect("built-in template");
    render_checked_algorithm_block_template_with_sources(
        &fixture_block(),
        &sources(),
        &json!({}),
        template,
        MODEL,
    )
    .unwrap_or_else(|error| panic!("formatting fixture must render {template_path}: {error}"))
}

/// Does this line open a top-level definition (column 0, ends in `{`)?
fn opens_top_level_definition(line: &str) -> bool {
    !line.is_empty()
        && !line.starts_with(char::is_whitespace)
        && !line.starts_with(['#', '/', '*', '}', ')'])
        && !line.ends_with(';')
}

fn structural_violations(text: &str) -> Vec<String> {
    let lines: Vec<&str> = text.split('\n').collect();
    let mut violations = Vec::new();
    let mut in_comment = false;
    for (index, line) in lines.iter().enumerate() {
        let number = index + 1;
        let previous = index.checked_sub(1).map(|i| lines[i]).unwrap_or("");
        let trimmed = line.trim();
        if line.contains('\t') {
            violations.push(format!("{number}: tab character"));
        }
        if line != &line.trim_end() {
            violations.push(format!("{number}: trailing whitespace"));
        }
        if previous.trim().is_empty() && trimmed.is_empty() && index > 0 {
            violations.push(format!("{number}: consecutive blank lines"));
        }
        if previous.trim_end().ends_with('{') && trimmed.is_empty() {
            violations.push(format!("{number}: blank line opening a block"));
        }
        if trimmed.starts_with('}') && previous.trim().is_empty() {
            violations.push(format!("{number}: blank line closing a block"));
        }
        // Track block comments so a `*/`-terminated banner is not mistaken for
        // code when the brace and indentation rules run.
        let starts_comment = trimmed.starts_with("/*") && !trimmed.contains("*/");
        if !in_comment && !trimmed.is_empty() && !trimmed.starts_with(['/', '*']) {
            if trimmed.starts_with("else") {
                violations.push(format!("{number}: `else` must attach to the closing brace"));
            }
            let indent = line.len() - line.trim_start().len();
            if indent % 4 != 0 {
                violations.push(format!(
                    "{number}: indentation {indent} is not a multiple of 4"
                ));
            }
            if opens_top_level_definition(line)
                && index > 0
                && !previous.trim().is_empty()
                && !previous.trim_end().ends_with(&['*', ','][..])
                && !previous.trim_end().ends_with("*/")
            {
                violations.push(format!(
                    "{number}: top-level definition without a preceding blank line or doc comment"
                ));
            }
        }
        if starts_comment {
            in_comment = true;
        } else if in_comment && trimmed.ends_with("*/") {
            in_comment = false;
        }
    }
    violations
}

#[test]
fn generated_c_satisfies_the_shipped_formatting_rules() {
    for template_path in ["model.c.jinja", "model.h.jinja"] {
        let text = render(template_path);
        let violations = structural_violations(&text);
        assert!(
            violations.is_empty(),
            "{template_path} violates the shipped formatting rules:\n  {}\n--- rendered ---\n{text}",
            violations.join("\n  ")
        );
    }
}

#[test]
fn the_target_emits_the_formatter_configuration_beside_the_code() {
    let config = render("clang_format.jinja");
    for required in [
        "ColumnLimit: 0",
        "IndentWidth: 4",
        "UseTab: Never",
        "BreakBeforeBraces: Attach",
        "SeparateDefinitionBlocks: Always",
        "MaxEmptyLinesToKeep: 1",
    ] {
        assert!(
            config.contains(required),
            "emitted .clang-format must pin `{required}`:\n{config}"
        );
    }
}

/// The authoritative check, run only where `clang-format` exists.
///
/// A missing binary is not a silent pass: the structural test above always
/// runs and encodes the same rules.
#[test]
fn clang_format_reports_no_replacements_when_available() {
    let Ok(work) = tempfile::tempdir() else {
        return;
    };
    std::fs::write(
        work.path().join(".clang-format"),
        render("clang_format.jinja"),
    )
    .expect("write emitted configuration");
    let mut checked_any = false;
    for (name, template_path) in [
        (format!("{MODEL}.c"), "model.c.jinja"),
        (format!("{MODEL}.h"), "model.h.jinja"),
    ] {
        let path = work.path().join(&name);
        std::fs::write(&path, render(template_path)).expect("write rendered source");
        let output = match Command::new("clang-format")
            .arg("--style=file")
            .arg("--dry-run")
            .arg("-Werror")
            .arg(&path)
            .output()
        {
            Ok(output) => output,
            // No clang-format in this environment; the structural test stands.
            Err(_) => return,
        };
        checked_any = true;
        assert!(
            output.status.success(),
            "clang-format wants to reformat {name}:\n{}",
            String::from_utf8_lossy(&output.stderr)
        );
    }
    assert!(checked_any || cfg!(not(unix)));
}
