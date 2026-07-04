//! Source-span correctness (SPEC_0034 D11): a parsed node's span must slice the
//! exact source lexeme it came from. Compiling with spans is not enough — this
//! drives print -> parse and checks that byte offsets map back to real text,
//! which is the whole point of carrying spans (positioned diagnostics + LSP).
//!
//! Gated behind `parse` (the parser is the only span *producer*).
#![cfg(feature = "parse")]

use rumoca_ir_galec::ast::{
    Block, Expression, InterfaceKind, InterfaceVariable, Name, Reference, ScalarType, Span,
    Spanned, Statement, VariableDeclaration,
};
use rumoca_ir_galec::parse::{GalecParseError, parse};
use rumoca_ir_galec::print::print_block;
use rumoca_ir_galec::{span_of, validate};

/// Slice the source a span covers. `BytePos` is a `usize` newtype; `end` is
/// exclusive (matches `Span`'s convention and Rust range semantics).
fn slice(source: &str, span: Span) -> &str {
    &source[span.start.0..span.end.0]
}

/// A minimal, printable/parseable block: two interface variables and a single
/// `self.y := self.u;` assignment in `DoStep`. `Block::new` supplies the three
/// mandatory (empty) methods, so the printed text parses.
fn sample() -> Block {
    let mut block = Block::new(Name::ident("Ctrl"));
    block.interface.push(InterfaceVariable {
        kind: InterfaceKind::Input,
        decl: VariableDeclaration::scalar(ScalarType::Real, Name::ident("u")),
        start: None,
    });
    block.interface.push(InterfaceVariable {
        kind: InterfaceKind::Output,
        decl: VariableDeclaration::scalar(ScalarType::Real, Name::ident("y")),
        start: None,
    });
    block
        .do_step
        .statements
        .push(rumoca_ir_galec::ast::Spanned::dummy(
            Statement::Assignment {
                target: Reference::state(Name::ident("y")),
                value: rumoca_ir_galec::ast::Expression::Ref(Reference::state(Name::ident("u"))),
            },
        ));
    block
}

#[test]
fn declaration_span_slices_the_variable_name() {
    let text = print_block(&sample()).expect("fixture prints");
    let parsed = parse(&text, "spans").expect("fixture parses");

    // Interface variable declarations carry the span of their name (D11).
    assert_eq!(slice(&text, parsed.interface[0].decl.span), "u");
    assert_eq!(slice(&text, parsed.interface[1].decl.span), "y");
    // and neither is the dummy sentinel — a real, non-empty span was populated.
    assert!(!parsed.interface[0].decl.span.is_dummy());
    assert!(parsed.interface[0].decl.span.start.0 < parsed.interface[0].decl.span.end.0);
}

#[test]
fn block_span_runs_from_header_name_to_footer_name() {
    let text = print_block(&sample()).expect("fixture prints");
    let parsed = parse(&text, "spans").expect("fixture parses");

    // `union(header-name, footer-name)`: starts at the first `Ctrl`, ends at the
    // last `Ctrl` (the `end Ctrl;` terminator), covering the whole block body.
    let block_text = slice(&text, parsed.span);
    assert!(block_text.starts_with("Ctrl"), "got: {block_text:?}");
    assert!(block_text.ends_with("Ctrl"), "got: {block_text:?}");
    assert!(block_text.contains("DoStep"));
}

#[test]
fn statement_span_slices_its_assignment_target() {
    let text = print_block(&sample()).expect("fixture prints");
    let parsed = parse(&text, "spans").expect("fixture parses");

    // The single `self.y := self.u;` assignment: its span reconstructs from the
    // target state reference (`self.y`), whose last part is `y` (D11 M1: the
    // value expression is not yet spanned, so the target anchors the statement).
    let statement = &parsed.do_step.statements[0];
    assert!(matches!(statement.node, Statement::Assignment { .. }));
    assert_eq!(slice(&text, statement.span), "y");
}

#[test]
fn syntax_errors_carry_an_in_bounds_source_span() {
    // A statement position that begins with an integer literal is a syntax
    // error (a statement must start with a name / `self` / `if` / `for` / …).
    // `from_parol` must surface it positioned, not span-less.
    let bad = "block Bad\n\
               protected\n\
               public\n\
               method Startup\nalgorithm\nend Startup;\n\
               method Recalibrate\nalgorithm\nend Recalibrate;\n\
               method DoStep\nalgorithm\n1 := 2;\nend DoStep;\n\
               end Bad;\n";
    let err = parse(bad, "bad").expect_err("must fail to parse");
    let GalecParseError::Syntax { span, .. } = err else {
        panic!("expected a positioned Syntax error, got {err:?}");
    };
    let (start, end) = span.expect("syntax error must carry a source span");
    assert!(
        start <= end && end <= bad.len(),
        "span in bounds: {start}..{end}"
    );
    // The offending token `1` is well past the block header, so the error is
    // positioned at the body, not pinned at offset 0.
    assert!(
        start > 0,
        "span should point at the offending token, not the start"
    );
    assert!(
        !bad[start..end].is_empty(),
        "span slices a non-empty lexeme"
    );
}

#[test]
fn distinct_occurrences_get_distinct_spans() {
    let text = print_block(&sample()).expect("fixture prints");
    let parsed = parse(&text, "spans").expect("fixture parses");

    // The output variable `y` is declared once and assigned once; the two `y`
    // lexemes are at different byte offsets, so their spans must differ — proof
    // that spans track *occurrence*, not merely identity.
    let decl_y = parsed.interface[1].decl.span;
    let assign_y = parsed.do_step.statements[0].span;
    assert_eq!(slice(&text, decl_y), "y");
    assert_eq!(slice(&text, assign_y), "y");
    assert_ne!(
        decl_y, assign_y,
        "the declaration and the assignment of `y` are at different source offsets"
    );
}

fn input_real(name: &str) -> InterfaceVariable {
    InterfaceVariable {
        kind: InterfaceKind::Input,
        decl: VariableDeclaration::scalar(ScalarType::Real, Name::ident(name)),
        start: None,
    }
}

#[test]
fn span_of_positions_a_declaration_diagnostic() {
    // Two interface inputs named `u` -> EG012 duplicate name. Its structural
    // Location resolves via span_of to the declaration's span, which slices `u`.
    let mut block = Block::new(Name::ident("Dup"));
    block.interface.push(input_real("u"));
    block.interface.push(input_real("u"));
    let text = print_block(&block).expect("prints");
    let parsed = parse(&text, "dup").expect("parses");

    let diags = validate(&parsed).expect_err("a duplicate name is a diagnostic");
    let dup = diags
        .iter()
        .find(|d| d.code() == "EG012")
        .expect("EG012 duplicate-name present");
    let span = span_of(&parsed, dup.location()).expect("diagnostic is positioned");
    assert_eq!(slice(&text, span), "u");
}

#[test]
fn span_of_positions_a_statement_diagnostic() {
    // `self.y := self.nope;` in DoStep references an undeclared state -> EG014.
    // The statement-granular Location resolves to the statement's span.
    let mut block = Block::new(Name::ident("Ref"));
    block.interface.push(InterfaceVariable {
        kind: InterfaceKind::Output,
        decl: VariableDeclaration::scalar(ScalarType::Real, Name::ident("y")),
        start: None,
    });
    block
        .do_step
        .statements
        .push(Spanned::dummy(Statement::Assignment {
            target: Reference::state(Name::ident("y")),
            value: Expression::Ref(Reference::state(Name::ident("nope"))),
        }));
    let text = print_block(&block).expect("prints");
    let parsed = parse(&text, "ref").expect("parses");

    let diags = validate(&parsed).expect_err("an unresolved reference is a diagnostic");
    let unresolved = diags
        .iter()
        .find(|d| d.code() == "EG014")
        .expect("EG014 unresolved-reference present");
    let span = span_of(&parsed, unresolved.location()).expect("diagnostic is positioned");
    // The statement span reconstructs from the assignment target `self.y` (M1).
    let sliced = slice(&text, span);
    assert!(
        !sliced.is_empty() && text.contains(sliced),
        "in-bounds slice: {sliced:?}"
    );
    assert!(!span.is_dummy());
}
