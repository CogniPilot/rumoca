//! Source-span correctness (SPEC_0034 D11): a parsed node's span must slice the
//! exact source lexeme it came from. Compiling with spans is not enough — this
//! parses readable source and checks that byte offsets map back to real text,
//! which is the whole point of carrying spans (positioned diagnostics + LSP).
//!
//! The parser is the only source-span producer.

use rumoca_core::Span;
use rumoca_ir_galec::ast::Statement;
use rumoca_phase_parse_galec::{GalecParseError, GalecSyntaxError, parse, parse_document};

const SAMPLE: &str = "block Ctrl
input Real u;
output Real y;
protected
public
method Startup
algorithm
end Startup;
method Recalibrate
algorithm
end Recalibrate;
method DoStep
algorithm
self.y := self.u;
end DoStep;
end Ctrl;
";

/// Slice the source a span covers. `BytePos` is a `usize` newtype; `end` is
/// exclusive (matches `Span`'s convention and Rust range semantics).
fn slice(source: &str, span: Span) -> &str {
    &source[span.start.0..span.end.0]
}

#[test]
fn declaration_span_slices_the_variable_name() {
    let checked = parse(SAMPLE, "spans").expect("fixture parses");
    let parsed = checked.block();

    // Interface variable declarations carry the span of their name (D11).
    assert_eq!(slice(SAMPLE, parsed.interface[0].decl.span), "u");
    assert_eq!(slice(SAMPLE, parsed.interface[1].decl.span), "y");
    // and neither is the dummy sentinel — a real, non-empty span was populated.
    assert!(!parsed.interface[0].decl.span.is_dummy());
    assert!(parsed.interface[0].decl.span.start.0 < parsed.interface[0].decl.span.end.0);
}

#[test]
fn block_span_runs_from_header_name_to_footer_name() {
    let checked = parse(SAMPLE, "spans").expect("fixture parses");
    let parsed = checked.block();

    // `union(header-name, footer-name)`: starts at the first `Ctrl`, ends at the
    // last `Ctrl` (the `end Ctrl;` terminator), covering the whole block body.
    let block_text = slice(SAMPLE, parsed.span);
    assert!(block_text.starts_with("Ctrl"), "got: {block_text:?}");
    assert!(block_text.ends_with("Ctrl"), "got: {block_text:?}");
    assert!(block_text.contains("DoStep"));
}

#[test]
fn statement_span_slices_the_complete_assignment() {
    let checked = parse(SAMPLE, "spans").expect("fixture parses");
    let parsed = checked.block();

    // The statement owner covers its complete source syntax, from the retained
    // `self` owner token through the retained semicolon.
    let statement = &parsed.do_step.statements[0];
    assert!(matches!(statement.node, Statement::Assignment { .. }));
    assert_eq!(slice(SAMPLE, statement.span), "self.y := self.u;");
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
    let err = parse_document(bad, "bad").expect_err("must fail to parse");
    let GalecSyntaxError::Syntax { span, .. } = err else {
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
    let checked = parse(SAMPLE, "spans").expect("fixture parses");
    let parsed = checked.block();

    // The output variable `y` is declared once and assigned once; the two `y`
    // lexemes are at different byte offsets, so their spans must differ — proof
    // that spans track *occurrence*, not merely identity.
    let decl_y = parsed.interface[1].decl.span;
    let Statement::Assignment { target, .. } = &parsed.do_step.statements[0].node else {
        panic!("fixture statement must be an assignment");
    };
    let rumoca_ir_galec::ast::Reference::State(parts) = target else {
        panic!("fixture assignment target must be a state reference");
    };
    let assign_y = parts.first().expect("state reference has a part").span;
    assert_eq!(slice(SAMPLE, decl_y), "y");
    assert_eq!(slice(SAMPLE, assign_y), "y");
    assert_ne!(
        decl_y, assign_y,
        "the declaration and the assignment of `y` are at different source offsets"
    );
}

#[test]
fn symbol_at_resolves_a_reference_to_its_declaration() {
    // `self.y := self.u;` — the cursor on the `u` reference resolves to the
    // declaration `input Real u`, and hovers its type.
    let text = SAMPLE.replace("Ctrl", "Nav");
    let document = parse_document(&text, "nav").expect("parses");

    // Offset of the `u` in the `self.u` reference (not the `input Real u` decl).
    let reference_offset = text.find("self.u").expect("self.u present") + "self.".len();
    let info = document
        .symbol_at(reference_offset)
        .expect("cursor is on a reference");

    let def = info.definition_span().expect("reference has a declaration");
    assert_eq!(
        slice(&text, def),
        "u",
        "go-to-definition lands on the `u` declaration"
    );
    assert!(
        info.hover().contains("Real"),
        "hover shows the type: {}",
        info.hover()
    );
    // The definition span differs from the reference span (distinct occurrences).
    assert_ne!(def, info.reference_span());
}

#[test]
fn span_of_positions_a_declaration_diagnostic() {
    // Two interface inputs named `u` -> EG012 duplicate name. Its structural
    // Location resolves via span_of to the declaration's span, which slices `u`.
    let text = SAMPLE
        .replace("Ctrl", "Dup")
        .replace("output Real y;", "input Real u;");
    let document = parse_document(&text, "dup").expect("parses");
    let dup = document
        .diagnostics()
        .iter()
        .find(|d| d.code() == "EG012")
        .cloned()
        .expect("EG012 duplicate-name present");
    let span = dup.span().expect("diagnostic is positioned");
    assert_eq!(slice(&text, span), "u");
}

#[test]
fn production_parse_rejects_semantically_invalid_blocks() {
    let text = SAMPLE
        .replace("Ctrl", "Dup")
        .replace("output Real y;", "input Real u;");
    assert!(
        matches!(parse(&text, "dup"), Err(GalecParseError::Invalid(_))),
        "the production parser must never return an unchecked block"
    );
}

#[test]
fn span_of_positions_a_statement_diagnostic() {
    // `self.y := self.nope;` in DoStep references an undeclared state -> EG014.
    // The statement-granular Location resolves to the statement's span.
    let text = SAMPLE
        .replace("Ctrl", "Ref")
        .replace("input Real u;\n", "")
        .replace("self.u", "self.nope");
    let document = parse_document(&text, "ref").expect("parses");
    let unresolved = document
        .diagnostics()
        .iter()
        .find(|d| d.code() == "EG014")
        .cloned()
        .expect("EG014 unresolved-reference present");
    let span = unresolved.span().expect("diagnostic is positioned");
    // The statement-granular diagnostic covers the complete represented source
    // statement, while reference-level diagnostics retain their own use spans.
    let sliced = slice(&text, span);
    assert_eq!(sliced, "self.y := self.nope;");
    assert!(!span.is_dummy());
}
