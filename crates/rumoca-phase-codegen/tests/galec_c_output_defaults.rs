//! The GALEC C targets must emit a defaulted function output the way the
//! lowering handed it over: the MLS §12.4.4 declaration binding initializes the
//! output, and nothing overwrites it with a dead zero afterwards.
//!
//! This is the emission half of the rule fixed in 55ea493c (`stop the return
//! seed from clobbering a declared output default`). That fix removed the zero
//! store on the *lowering* side, and `rumoca`'s
//! `function_return_checked::a_declared_output_default_is_the_live_seed_of_the_lowered_body`
//! pins the checked-DAE shape. Neither of those sees the C printer, and the C
//! printer is a second place the same wrong code can appear: a
//! "zero-initialize every output parameter" prologue added to `model.c.jinja`
//! would reproduce the exact defect end to end while every lowering test still
//! passed. The fixtures below are therefore built at the GALEC layer — the
//! lowering's *output* — so that this file tests only what happens downstream
//! of it.
//!
//! The check is Rust, over the rendered text, and lives here rather than as a
//! template-side guard: the emission rule belongs to the views/renderer side,
//! never to jinja.

use rumoca_ir_galec::ast as galec;
use rumoca_ir_galec::package::CheckedAlgorithmBlock;
use rumoca_phase_codegen::{render_checked_algorithm_block_template_with_artifact, templates};

mod support;
use serde_json::json;

const MODEL: &str = "DefaultedOutput";

/// The two GALEC C targets. `galec-production` extends the shared body and
/// differs only in its conformance banner, so the rule must hold in both.
const TARGETS: [&str; 2] = ["embedded-c-galec", "galec-production"];

/// The declaration binding of the fixture's output, and the value the guarded
/// path writes instead. Both are exact in binary32, so the generated C spells
/// them literally and a text search is not a float-formatting bet.
const DECLARED_DEFAULT: f64 = 7.0;

fn real(name: &str) -> galec::VariableDeclaration {
    galec::VariableDeclaration::scalar(galec::ScalarType::Real, galec::Name::ident(name))
}

fn interface(kind: galec::InterfaceKind, name: &str) -> galec::InterfaceVariable {
    galec::InterfaceVariable {
        kind,
        decl: real(name),
        start: None,
    }
}

fn parameter(direction: galec::Direction, name: &str) -> galec::Parameter {
    galec::Parameter {
        direction,
        decl: real(name),
    }
}

fn local_ref(name: &str) -> galec::Reference {
    galec::Reference::local(galec::Name::ident(name))
}

fn state_ref(name: &str) -> galec::Reference {
    galec::Reference::state(galec::Name::ident(name))
}

fn local(name: &str) -> galec::Expression {
    galec::Expression::Ref(local_ref(name))
}

fn state(name: &str) -> galec::Expression {
    galec::Expression::Ref(state_ref(name))
}

fn assign(target: galec::Reference, value: galec::Expression) -> galec::Spanned<galec::Statement> {
    galec::Spanned::dummy(galec::Statement::Assignment { target, value })
}

/// `<name>(input Real x) => (output Real <output>)`, in exactly the shape the
/// DAE lowering produces for
///
/// ```modelica
/// function <name>
///   input Real x;
///   output Real <output> = 7.0;
/// algorithm
///   if x > 0 then
///     <output> := x;
///     return;
///   end if;
/// end <name>;
/// ```
///
/// The declaration binding is a leading unconditional store, and the guarded
/// return is a conditional store over it. Every path that does not take the
/// guard keeps the declared value, so the default is live, not dead.
fn defaulted_function(name: &str, output: &str) -> galec::UserFunction {
    let guarded = galec::Spanned::dummy(galec::Statement::If(galec::IfStatement {
        branches: vec![galec::IfBranch {
            condition: galec::Condition::Expression(galec::Expression::binary(
                galec::BinaryOp::Gt,
                local("x"),
                galec::Expression::Real(0.0),
            )),
            body: vec![assign(local_ref(output), local("x"))],
            span: rumoca_core::Span::DUMMY,
        }],
        else_body: None,
    }));
    galec::UserFunction {
        kind: galec::FunctionKind::Stateless,
        name: galec::Name::ident(name),
        // The guard compares reals, so `NAN` can escape the function (EG036).
        signals: vec![galec::Identifier::new(galec::PredefinedSignal::Nan.name())],
        parameters: vec![
            parameter(galec::Direction::Input, "x"),
            parameter(galec::Direction::Output, output),
        ],
        locals: Vec::new(),
        statements: vec![
            assign(local_ref(output), galec::Expression::Real(DECLARED_DEFAULT)),
            guarded,
        ],
        span: rumoca_core::Span::DUMMY,
    }
}

/// A block that calls two defaulted functions, so both are reachable and the
/// emitted unit has to carry both bodies.
///
/// **`decoy` is ordered first on purpose.** Its body is the same shape as
/// `defaultedReturn`'s, so it satisfies every assertion below on its own. A
/// body reader that anchors on the forward-declaration block instead of the
/// definition lands on `decoy`'s body and passes while never looking at
/// `defaultedReturn` — the tautology
/// [`the_body_reader_selects_the_definition_not_a_neighbour`] exists to
/// prevent, and which the reviewer demonstrated against the first version of
/// this file. Keeping the decoy in the *primary* fixture means every assertion
/// here is permanently taken under that hazard, not only the one test that
/// names it.
fn fixture() -> CheckedAlgorithmBlock {
    let mut block = galec::Block::new(galec::Name::ident(MODEL));
    block.interface = vec![
        interface(galec::InterfaceKind::Input, "u"),
        interface(galec::InterfaceKind::Output, "result"),
        interface(galec::InterfaceKind::Output, "shadow"),
    ];
    block.protected_functions = vec![
        defaulted_function("decoy", "h"),
        defaulted_function("defaultedReturn", "y"),
    ];
    block.do_step.signals = vec![galec::PredefinedSignal::Nan];
    block.do_step.statements = vec![
        galec::Spanned::dummy(galec::Statement::MultiAssignment {
            targets: vec![state_ref("shadow")],
            call: galec::FunctionCall {
                function: galec::Name::ident("decoy"),
                arguments: vec![state("u")],
            },
        }),
        galec::Spanned::dummy(galec::Statement::MultiAssignment {
            targets: vec![state_ref("result")],
            call: galec::FunctionCall {
                function: galec::Name::ident("defaultedReturn"),
                arguments: vec![state("u")],
            },
        }),
    ];
    CheckedAlgorithmBlock::construct(block).expect("defaulted-output fixture must be valid GALEC")
}

fn render(block: &CheckedAlgorithmBlock, target: &str, path: &str) -> String {
    let template = templates::builtin_template_source(target, path)
        .unwrap_or_else(|| panic!("built-in template {target}/{path}"));
    if target != "galec" && path.starts_with("model.") {
        // The model unit prints from the block's Solve refinement (#483).
        return support::render_refined(block, &rumoca_core::SourceMap::new(), template, MODEL)
            .unwrap_or_else(|error| panic!("{target}/{path} must render the fixture: {error}"));
    }
    render_checked_algorithm_block_template_with_artifact(block, &json!({}), template, MODEL)
        .unwrap_or_else(|error| panic!("{target}/{path} must render the fixture: {error}"))
}

// ---------------------------------------------------------------------------
// Emitted-C readers.
//
// TWIN: everything between this banner and the closing one is duplicated
// verbatim across two files, which live in different crates:
//
//   * crates/rumoca-phase-codegen/tests/galec_c_output_defaults.rs
//   * crates/rumoca/tests/suite_galec_fmu/cli_target_embedded_c_galec.rs
//
// A test-only reader has no home either crate can import without adding it to
// a shipped crate's public surface, so the copies are kept BYTE-identical
// instead — including this banner, so the two sections can simply be diffed.
// Change one, change the other.
// ---------------------------------------------------------------------------

/// Byte offset of the `{` opening the emitted C **definition** of `name`.
///
/// The GALEC C printer emits a forward-declaration block ahead of the
/// definition block, so the *first* occurrence of `name(` is the prototype and
/// the next `{` opens whichever function is defined first in the unit. A reader
/// anchored there returns some other function's body, and every assertion made
/// on it passes without ever looking at `name` — including with the clobber
/// these tests exist to catch fully restored. So: walk every occurrence,
/// balance its parameter list, and accept only the one whose `)` is followed by
/// `{` rather than `;`. Call sites (`name(a, b);`) fall out by the same rule.
fn definition_brace(source: &str, name: &str) -> Option<usize> {
    let needle = format!("{name}(");
    let mut cursor = 0;
    while let Some(offset) = source[cursor..].find(&needle) {
        let open = cursor + offset + needle.len() - 1;
        cursor = open + 1;
        let Some(close) = matching_parenthesis(source, open) else {
            continue;
        };
        let after = &source[close + 1..];
        if !after.trim_start().starts_with('{') {
            continue;
        }
        return Some(close + 1 + after.find('{').expect("the tail starts with a brace"));
    }
    None
}

/// The body of the emitted C definition of `name`. A missing definition is a
/// hard failure — an assertion that silently matched nothing would be worse
/// than no assertion.
fn emitted_definition_body<'a>(source: &'a str, name: &str, label: &str) -> &'a str {
    let brace = definition_brace(source, name)
        .unwrap_or_else(|| panic!("{label}: the emitted unit must define `{name}`:\n{source}"));
    let body = &source[brace + 1..];
    let end = body.find("\n}").unwrap_or_else(|| {
        panic!("{label}: the `{name}` definition must close its body:\n{source}")
    });
    &body[..end]
}

/// The index of the `)` that closes the `(` at `open`.
fn matching_parenthesis(source: &str, open: usize) -> Option<usize> {
    let mut depth = 0usize;
    for (offset, character) in source[open..].char_indices() {
        match character {
            '(' => depth += 1,
            ')' => {
                depth -= 1;
                if depth == 0 {
                    return Some(open + offset);
                }
            }
            _ => {}
        }
    }
    None
}

/// One `<lvalue> = <numeric literal>;` store found in an emitted body.
struct ConstantStore {
    target: String,
    value: String,
}

impl ConstantStore {
    fn is_value(&self, expected: f64) -> bool {
        self.value
            .trim_end_matches('f')
            .parse::<f64>()
            .is_ok_and(|value| (value - expected).abs() < 1.0e-9)
    }
}

/// Every constant store in an emitted body, keyed by lvalue.
///
/// Matched on the assignment form rather than on one expected spelling, so a
/// zero written `0`, `0.0f`, `-0.0f` or `0.0e0f` is caught the same way, and so
/// the test never depends on how the C printer spells the output.
fn constant_stores(body: &str) -> Vec<ConstantStore> {
    body.lines()
        .filter_map(|line| line.trim().strip_suffix(';'))
        .filter_map(|statement| statement.split_once(" = "))
        .filter(|(_, value)| {
            !value.is_empty()
                && value
                    .trim_start_matches('-')
                    .trim_end_matches('f')
                    .chars()
                    .all(|character| {
                        character.is_ascii_digit() || matches!(character, '.' | 'e' | '+')
                    })
        })
        .map(|(target, value)| ConstantStore {
            target: target.to_owned(),
            value: value.to_owned(),
        })
        .collect()
}

// ---------------------------------------------------------------------------
// End of the twinned section.
// ---------------------------------------------------------------------------

/// Both GALEC C targets must keep the declaration binding as the output's
/// initialization and must add no zero store of their own.
///
/// If the defect were reintroduced on the emission side — an output prologue
/// that zeroes every `output` parameter, a "definite assignment" repair that
/// stores 0 ahead of the guarded branch, a reordering that moves the default
/// below a generated store — the output stops carrying exactly one constant,
/// or stops carrying the declared one first, and this fails with the emitted
/// body attached.
#[test]
fn a_defaulted_output_keeps_its_declaration_binding_in_the_generated_c() {
    // The Solve-printed bodies are SSA: the declared constant is computed into
    // a register and stored, after the definedness fill, so "the first
    // constant store" no longer names the binding. The property is executed
    // instead, for both C targets: where the guard is false the output keeps
    // its declared default, and where it holds the guarded store wins. A dead
    // zero over the live default (the 55ea493c defect) fails the first case.
    let block = fixture();
    for target in TARGETS {
        let directory = tempfile::tempdir().expect("temporary generated-C directory");
        let base = directory.path();
        std::fs::write(
            base.join(format!("{MODEL}.h")),
            render(&block, target, "model.h.jinja"),
        )
        .expect("write generated header");
        let source = render(&block, target, "model.c.jinja");
        // The declaration binding is still printed as the declared constant,
        // and what it is stored into is read again: the binding is live, not
        // a dead store the executed check below could never observe.
        let body = emitted_definition_body(&source, "defaultedReturn", target);
        let binding = constant_stores(body)
            .into_iter()
            .find(|store| store.is_value(DECLARED_DEFAULT))
            .unwrap_or_else(|| {
                panic!("{target}: the body must print the declared {DECLARED_DEFAULT}:\n{body}")
            });
        let written = body
            .find(&format!("{} = ", binding.target))
            .expect("the binding was just read out of this body");
        assert!(
            body[written + binding.target.len()..].contains(&binding.target),
            "{target}: the declared {DECLARED_DEFAULT} in `{}` must be read again:\n{body}",
            binding.target
        );
        std::fs::write(base.join(format!("{MODEL}.c")), &source).expect("write generated source");
        std::fs::write(
            base.join("main.c"),
            format!(
                "#include <string.h>\n#include \"{MODEL}.h\"\n\
                 int main(void) {{\n\
                 static {MODEL}State state;\n\
                 memset(&state, 0, sizeof state);\n\
                 {MODEL}_startup(&state);\n\
                 state.u = -1.0f;\n\
                 {MODEL}_dostep(&state);\n\
                 if (state.result != {DECLARED_DEFAULT:?}f || state.shadow != {DECLARED_DEFAULT:?}f) return 1;\n\
                 state.u = 2.5f;\n\
                 {MODEL}_dostep(&state);\n\
                 if (state.result != 2.5f || state.shadow != 2.5f) return 2;\n\
                 return 0;\n\
                 }}\n"
            ),
        )
        .expect("write driver");
        let executable = base.join("defaulted");
        let compile = std::process::Command::new("cc")
            .args(["-std=c99", "-Wall", "-Wextra", "-Werror", "-o"])
            .arg(&executable)
            .arg(base.join("main.c"))
            .arg(base.join(format!("{MODEL}.c")))
            .arg("-lm")
            .output()
            .expect("run C compiler");
        assert!(
            compile.status.success(),
            "{target}: strict compile failed:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        );
        let run = std::process::Command::new(&executable)
            .status()
            .expect("run the defaulted-output driver");
        assert_eq!(
            run.code(),
            Some(0),
            "{target}: 1 = the declared default was lost on the unguarded path, \
             2 = the guarded store did not win"
        );
    }
}

/// The assertions above are only about `defaultedReturn` if the body reader
/// actually returns `defaultedReturn`'s body. It did not, in the first version
/// of this file: anchoring on the first `defaultedReturn(` landed on the
/// forward declaration, and the following `{` opened `decoy` — whose body has
/// the same shape and satisfies every assertion. The suite passed with the
/// clobber restored.
///
/// This pins the reader against exactly that. It proves the hazard is real
/// (the unit does contain a prototype ahead of the definition, and does define
/// a same-shaped neighbour first), then requires the returned body to be
/// `defaultedReturn`'s own: it stores that function's output and never touches
/// `decoy`'s.
#[test]
fn the_body_reader_selects_the_definition_not_a_neighbour() {
    let block = fixture();
    for target in TARGETS {
        let source = render(&block, target, "model.c.jinja");

        // The hazard must be present, or this test proves nothing.
        let definition = definition_brace(&source, "defaultedReturn")
            .unwrap_or_else(|| panic!("{target}: must define defaultedReturn:\n{source}"));
        let decoy = definition_brace(&source, "decoy")
            .unwrap_or_else(|| panic!("{target}: must define decoy:\n{source}"));
        assert!(
            source
                .find("defaultedReturn(")
                .expect("the unit names defaultedReturn")
                < definition,
            "{target}: this test is only meaningful while the printer emits a \
             forward declaration ahead of the definition. If that changed, \
             re-derive the hazard rather than deleting the test.\n{source}"
        );
        assert!(
            decoy < definition,
            "{target}: the decoy must be defined AHEAD of defaultedReturn, or \
             it cannot stand in for it.\n{source}"
        );

        // Function slots print by id, so the bodies are told apart by where
        // they sit: each must be the slice that follows its own definition's
        // opening brace, and `decoy`'s lies wholly before `defaultedReturn`'s.
        let offset = |part: &str| part.as_ptr() as usize - source.as_ptr() as usize;
        let body = emitted_definition_body(&source, "defaultedReturn", target);
        assert_eq!(
            offset(body),
            definition + 1,
            "{target}: the reader must return `defaultedReturn`'s own body.\nReturned body:\n{body}"
        );
        let decoy_body = emitted_definition_body(&source, "decoy", target);
        assert_eq!(
            offset(decoy_body),
            decoy + 1,
            "{target}: reading `decoy` must return `decoy`'s body.\nReturned body:\n{decoy_body}"
        );
        assert!(
            offset(decoy_body) + decoy_body.len() < definition,
            "{target}: the two bodies must not overlap"
        );
    }
}
