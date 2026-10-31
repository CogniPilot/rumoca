//! The emitted `ErrorSignalStatus` surface must obey SPEC_0034 GAL-040:
//! cleared exactly once per method, otherwise only ever OR-ed with a
//! compile-time-constant mask, and never read.
//!
//! That contract is what licenses hoisting a repeated signal-raising guard down
//! to one evaluation, and `rumoca_ir_galec::signal_effect` mints the permission
//! on the strength of it. The permission is a Rust type; the contract is a
//! property of emitted C, which no Rust type can constrain. This file is the
//! bridge: it fails if a kernel is ever added that writes the word in a
//! non-idempotent way, or that reads it back.
//!
//! Two instruments, because neither alone is enough:
//!
//! 1. [`every_status_write_in_the_template_is_a_reset_or_a_constant_or`] scans
//!    the template SOURCE. Kernels are emitted behind `uses.*` guards, so a
//!    kernel no fixture exercises never appears in any rendered artifact, but
//!    it does appear here. This is the instrument that catches a new kernel.
//! 2. [`every_status_write_in_rendered_c_is_a_reset_or_a_constant_or`] scans
//!    RENDERED C, proving the shapes the first test accepts are the shapes that
//!    actually reach a compiler, and pinning the three method resets.

use rumoca_ir_galec::ast as galec;
use rumoca_ir_galec::package::CheckedAlgorithmBlock;
use rumoca_ir_galec::signal_effect::{RepeatableSignalEffect, StatusEffect, expression_effect};
use rumoca_phase_codegen::{render_checked_algorithm_block_template_with_artifact, templates};

#[path = "support/solve_render.rs"]
mod support;
use serde_json::json;

const MODEL: &str = "SignalSurface";

/// The block-state field holding the eFMI `ErrorSignalStatus` word.
const STATUS_FIELD: &str = "rumoca_galec_error_signal_status";

/// The out-parameter names the emitted helpers give the same word.
const STATUS_PARAMS: [&str; 2] = ["status", "signals"];

/// Every C target whose body could touch the word. Both C targets extend one
/// shared body template, but naming them separately keeps this test honest if
/// that ever stops being true.
const C_TARGETS: [&str; 2] = ["embedded-c-galec", "galec-production"];

/// One classified mention of the status word.
#[derive(Debug, PartialEq, Eq)]
enum Mention {
    /// `… = UINT32_C(0);` at a method boundary.
    Reset,
    /// The Solve-printed exit: the word assigned whole from the method's own
    /// status bits, plus the constant fault bit on the fault exit (#483).
    Publish,
    /// `… |= <constant>;`.
    AccumulateOr,
    /// A declaration or a parameter: names the word without touching it.
    Declaration,
    /// `&self->…` handed to a helper whose own body this scan also covers.
    AddressTaken,
    /// A comment or template control text, with no C statement on the line.
    Prose,
}

/// Classify one source line that mentions the status word, or return the reason
/// it is not one of the permitted shapes.
///
/// Fail-closed by construction: the function returns `Err` for everything it
/// does not positively recognise, so a shape nobody anticipated is a test
/// failure rather than a silent acceptance.
fn classify(line: &str) -> Result<Mention, String> {
    let text = line.trim();
    // A line whose only mention is inside a comment says nothing about the
    // emitted C. Comment openers are checked before the write shapes so that
    // prose describing `|=` does not count as an occurrence of it.
    let code = text.split("/*").next().unwrap_or("").trim();
    // A continuation line of a block comment opens with `* ` or is the closing
    // `*/`. `*status` opens with `*` too and is emphatically not prose, so the
    // space matters and is deliberate.
    let comment_continuation = code.starts_with("* ") || code == "*" || code.starts_with("*/");
    if code.is_empty() || comment_continuation || code.starts_with("//") {
        return Ok(Mention::Prose);
    }
    if !mentions_status(code) {
        return Ok(Mention::Prose);
    }
    if code.contains("uint32_t *status") || code.contains("uint32_t *signals") {
        return Ok(Mention::Declaration);
    }
    if code.contains(&format!("uint32_t {STATUS_FIELD};")) || code.contains("\"rumoca_galec_error")
    {
        return Ok(Mention::Declaration);
    }
    if code.contains(&format!("&self->{STATUS_FIELD}")) {
        // The address is handed to a helper. That is only safe because the
        // helper bodies are themselves in scope of this scan.
        return Ok(Mention::AddressTaken);
    }
    if code.contains(&format!("self->{STATUS_FIELD} = UINT32_C(0);")) {
        return Ok(Mention::Reset);
    }
    if code == format!("self->{STATUS_FIELD} = rumoca_galec_status;")
        || code == format!("self->{STATUS_FIELD} = rumoca_galec_status | RUMOCA_GALEC_FAULT;")
    {
        return Ok(Mention::Publish);
    }
    if let Some(rest) = accumulation_right_hand_side(code) {
        return constant_or_mask(rest).map(|()| Mention::AccumulateOr);
    }
    Err(format!(
        "unrecognised ErrorSignalStatus write or read: `{text}`"
    ))
}

fn mentions_status(code: &str) -> bool {
    code.contains(STATUS_FIELD)
        || STATUS_PARAMS
            .iter()
            .any(|name| code.contains(&format!("*{name}")))
}

/// The text after `|=` on an accumulating line, or `None` if the line does not
/// accumulate.
fn accumulation_right_hand_side(code: &str) -> Option<&str> {
    let (target, rest) = code.split_once("|=")?;
    let target = target.trim();
    let names_the_word = target.ends_with(STATUS_FIELD)
        || STATUS_PARAMS
            .iter()
            .any(|name| target.ends_with(&format!("*{name}")));
    names_the_word.then_some(rest)
}

/// Accept an accumulated mask only when every value it can take is a literal
/// `UINT32_C(<digits>)`.
///
/// The right-hand side may be a template conditional choosing between masks,
/// which is still a compile-time constant in every rendering. What must not
/// appear is any read: a reference to the word, to a variable, or to a call.
fn constant_or_mask(rest: &str) -> Result<(), String> {
    let mut remainder = rest;
    let mut masks = 0usize;
    while let Some(start) = remainder.find("UINT32_C(") {
        let after = &remainder[start + "UINT32_C(".len()..];
        let Some(end) = after.find(')') else {
            return Err(format!("unterminated UINT32_C in `{rest}`"));
        };
        if !after[..end].chars().all(|c| c.is_ascii_digit()) {
            return Err(format!(
                "accumulated mask `{}` is not a decimal literal in `{rest}`",
                &after[..end]
            ));
        }
        masks += 1;
        remainder = &after[end + 1..];
    }
    if masks == 0 {
        return Err(format!("accumulation with no constant mask: `{rest}`"));
    }
    if rest.contains(STATUS_FIELD) {
        return Err(format!("accumulation reads the status word back: `{rest}`"));
    }
    Ok(())
}

/// Scan one unit and return how many whole-word writes (resets and
/// publishes) it contains.
fn scan(unit: &str, label: &str) -> usize {
    let mut writes = 0;
    for (index, line) in unit.lines().enumerate() {
        if !mentions_status(line) {
            continue;
        }
        match classify(line) {
            Ok(Mention::Reset | Mention::Publish) => writes += 1,
            Ok(_) => {}
            Err(reason) => panic!("{label}:{}: {reason}", index + 1),
        }
    }
    writes
}

#[test]
fn every_status_write_in_the_template_is_a_reset_or_a_constant_or() {
    for target in C_TARGETS {
        for path in ["model.c.jinja", "model.h.jinja", "kernels.c.jinja"] {
            let Some(source) = templates::builtin_template_source(target, path) else {
                // `galec-production` inherits the shared body rather than
                // carrying its own copy of every partial.
                continue;
            };
            let writes = scan(source, &format!("{target}/{path}"));
            if path == "model.c.jinja" && source.contains("{%- macro method(") {
                // One `method` macro serves all three methods: it publishes the
                // word once on the normal exit and once on the fault exit.
                assert_eq!(
                    writes, 2,
                    "{target}/{path}: the method macro must write the word on \
                     exactly its two exits and nowhere else"
                );
            }
        }
    }
}

#[test]
fn every_status_write_in_rendered_c_is_a_reset_or_a_constant_or() {
    let block = signal_surface_fixture();
    let source = render(&block, "model.c.jinja");
    let header = render(&block, "model.h.jinja");

    // Each method resets its six status bits on entry and publishes the word
    // whole on each of its two exits: once per invocation, never read back.
    assert_eq!(
        scan(&source, "rendered model.c"),
        6,
        "rendered C must write the word exactly once per method exit:\n{source}"
    );
    assert_eq!(scan(&header, "rendered model.h"), 0);
    assert_eq!(
        source
            .matches("for (size_t k = 0; k < 6u; ++k) { ctx->rumoca_galec_st[k] = false; }")
            .count(),
        3,
        "every method must start from cleared status bits:\n{source}"
    );

    // The fixture is only evidence if it actually raises: a NaN-checked Real
    // comparison and `integer()` each OR into a status bit.
    assert!(
        source.contains("rumoca_galec_lhs != rumoca_galec_rhs"),
        "the fixture must exercise a NaN-checked comparison:\n{source}"
    );
    assert!(
        source.contains("9223372036854775808.0"),
        "the fixture must exercise integer()'s domain test:\n{source}"
    );
}

/// The emitted helpers and the Rust classification must agree about which
/// constructs reach the word at all.
///
/// Without this, `signal_effect` could drift into calling a raising construct
/// inert while the template still emits a raise for it, and the permission
/// would be minted on a false premise.
#[test]
fn the_classification_agrees_with_what_the_template_emits_a_raise_for() {
    // The Rust classification must agree with what the refinement the C is
    // printed from actually raises: a raising construct adds stores to the
    // status bits of the refined program, an inert one adds none.
    let a = || galec::Expression::Ref(galec::Reference::state(galec::Name::ident("a")));
    let inert = status_stores(
        galec::Expression::binary(galec::BinaryOp::Add, a(), galec::Expression::Real(2.0)),
        galec::ScalarType::Real,
        Vec::new(),
    );

    let comparison =
        galec::Expression::binary(galec::BinaryOp::Gt, a(), galec::Expression::Real(0.0));
    assert_eq!(expression_effect(&comparison), StatusEffect::AccumulateOr);
    assert!(RepeatableSignalEffect::prove(&comparison).is_some());
    assert!(
        status_stores(
            comparison,
            galec::ScalarType::Boolean,
            vec![galec::PredefinedSignal::Nan],
        ) > inert,
        "a Real comparison must raise in the refined program"
    );

    let conversion = galec::Expression::Call(galec::FunctionCall {
        function: galec::Name::ident("integer"),
        arguments: vec![a()],
    });
    assert_eq!(expression_effect(&conversion), StatusEffect::AccumulateOr);
    assert!(
        status_stores(
            conversion,
            galec::ScalarType::Integer,
            vec![
                galec::PredefinedSignal::Overflow,
                galec::PredefinedSignal::Nan
            ],
        ) > inert,
        "integer() must raise in the refined program"
    );

    let sum = galec::Expression::binary(
        galec::BinaryOp::Add,
        galec::Expression::Real(1.0),
        galec::Expression::Real(2.0),
    );
    assert_eq!(expression_effect(&sum), StatusEffect::Inert);
}

/// Stores into status-bit slots in the refined DoStep of `result := value`.
fn status_stores(
    value: galec::Expression,
    result: galec::ScalarType,
    signals: Vec<galec::PredefinedSignal>,
) -> usize {
    let mut block = galec::Block::new(galec::Name::ident(MODEL));
    block.interface = vec![
        galec::InterfaceVariable {
            kind: galec::InterfaceKind::Input,
            decl: galec::VariableDeclaration::scalar(
                galec::ScalarType::Real,
                galec::Name::ident("a"),
            ),
            start: None,
        },
        galec::InterfaceVariable {
            kind: galec::InterfaceKind::Output,
            decl: galec::VariableDeclaration::scalar(result, galec::Name::ident("result")),
            start: None,
        },
    ];
    // The escape clause must equal what the statement can raise (GAL-018).
    block.do_step.signals = signals;
    block.do_step.statements = vec![galec::Spanned::dummy(galec::Statement::Assignment {
        target: galec::Reference::state(galec::Name::ident("result")),
        value,
    })];
    let block = CheckedAlgorithmBlock::construct(block).expect("valid status fixture");
    let (_, refined) = support::refine(&block).expect("status fixture refines");
    let program = serde_json::to_value(refined.do_step().program()).expect("serializable");
    let slots = program["slots"].as_array().expect("slots");
    program["operations"]
        .as_array()
        .expect("operations")
        .iter()
        .map(|issued| &issued["operation"])
        .filter(|operation| {
            operation["operation"] == "store"
                && operation["slot"]
                    .as_u64()
                    .and_then(|slot| slots.get(usize::try_from(slot).ok()?))
                    .is_some_and(|slot| slot["storage"] == "signal_status")
        })
        .count()
}

fn render(block: &CheckedAlgorithmBlock, path: &str) -> String {
    let template =
        templates::builtin_template_source("embedded-c-galec", path).expect("built-in template");
    if path.starts_with("model.") {
        // The model unit prints from the block's Solve refinement (#483).
        return support::render_refined(block, &rumoca_core::SourceMap::new(), template, MODEL)
            .expect("signal-surface fixture must refine and render");
    }
    render_checked_algorithm_block_template_with_artifact(block, &json!({}), template, MODEL)
        .expect("signal-surface fixture must render")
}

/// A block that emits one Real comparison and one `integer` conversion, so the
/// rendered unit contains both status-taking kernels.
fn signal_surface_fixture() -> CheckedAlgorithmBlock {
    let mut block = galec::Block::new(galec::Name::ident(MODEL));
    block.interface = vec![
        scalar_interface(galec::InterfaceKind::Input, galec::ScalarType::Real, "a"),
        scalar_interface(
            galec::InterfaceKind::Output,
            galec::ScalarType::Boolean,
            "above",
        ),
        scalar_interface(
            galec::InterfaceKind::Output,
            galec::ScalarType::Integer,
            "truncated",
        ),
    ];
    // The escape clause must EXACTLY equal the computed set (GAL-018): the
    // comparison can raise NAN and `integer` can raise NAN or OVERFLOW.
    block.do_step.signals = vec![
        galec::PredefinedSignal::Overflow,
        galec::PredefinedSignal::Nan,
    ];
    let state = |name: &str| galec::Reference::state(galec::Name::ident(name));
    block.do_step.statements = vec![
        galec::Spanned::dummy(galec::Statement::Assignment {
            target: state("above"),
            value: galec::Expression::binary(
                galec::BinaryOp::Gt,
                galec::Expression::Ref(state("a")),
                galec::Expression::Real(0.0),
            ),
        }),
        galec::Spanned::dummy(galec::Statement::Assignment {
            target: state("truncated"),
            value: galec::Expression::Call(galec::FunctionCall {
                function: galec::Name::ident("integer"),
                arguments: vec![galec::Expression::Ref(state("a"))],
            }),
        }),
    ];
    CheckedAlgorithmBlock::construct(block).expect("signal-surface fixture must be valid GALEC")
}

fn scalar_interface(
    kind: galec::InterfaceKind,
    scalar: galec::ScalarType,
    name: &str,
) -> galec::InterfaceVariable {
    galec::InterfaceVariable {
        kind,
        decl: galec::VariableDeclaration::scalar(scalar, galec::Name::ident(name)),
        start: None,
    }
}
