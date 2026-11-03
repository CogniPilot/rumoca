//! Literal target text of a covered template: a text node can itself be a
//! solver, an AD rule or a scheduler with no template expression in it, so the
//! printed C is checked too. Numerical bodies must arrive as issued
//! instructions; literal text may only spell declarations, punctuation and the
//! fixed spelling of an issued instruction.
//!
//! There is no reviewed ABI-fragment exemption: control flow, accumulation and
//! numeric constants fail wherever they appear in literal text. A language with
//! no literal check fails closed on any literal text at all.

use super::{Language, Rule, Violation};
use minijinja::machinery::Span;
use minijinja::machinery::ast::{Expr, Stmt};

const CONTROL_KEYWORDS: &[&str] = &[
    "for", "while", "do", "goto", "if", "else", "switch", "case", "break", "continue",
];
const ACCUMULATING: &[&str] = &[
    "<<=", ">>=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "++", "--",
];

/// One piece of literal text and where it starts in its template.
struct Segment<'a> {
    span: Span,
    text: &'a str,
}

pub(super) fn check(
    file: &str,
    ast: &Stmt<'_>,
    language: Language,
    violations: &mut Vec<Violation>,
) {
    let mut segments = Vec::new();
    collect(std::slice::from_ref(ast), &mut segments);
    match language {
        Language::C => check_c(file, &segments, violations),
        Language::Unsupported => {
            if let Some(first) = segments.iter().find(|s| !s.text.trim().is_empty()) {
                violations.push(Violation {
                    file: file.to_owned(),
                    line: first.span.start_line,
                    column: first.span.start_col,
                    rule: Rule::UnsupportedLiteralLanguage,
                    detail: "literal text in a template whose target language has no literal check"
                        .to_owned(),
                });
            }
        }
    }
}

/// Literal text in document order, including string constants printed with
/// `{{ "..." }}`, which are literal text by another spelling.
fn collect<'s>(stmts: &'s [Stmt<'_>], out: &mut Vec<Segment<'s>>) {
    for stmt in stmts {
        match stmt {
            Stmt::Template(t) => collect(&t.children, out),
            Stmt::EmitRaw(r) => out.push(Segment {
                span: r.span(),
                text: r.raw,
            }),
            Stmt::EmitExpr(e) => out.extend(printed_constant(&e.expr)),
            Stmt::ForLoop(f) => {
                collect(&f.body, out);
                collect(&f.else_body, out);
            }
            Stmt::IfCond(i) => {
                collect(&i.true_body, out);
                collect(&i.false_body, out);
            }
            Stmt::Block(b) => collect(&b.body, out),
            Stmt::WithBlock(w) => collect(&w.body, out),
            Stmt::SetBlock(s) => collect(&s.body, out),
            Stmt::FilterBlock(f) => collect(&f.body, out),
            Stmt::AutoEscape(a) => collect(&a.body, out),
            Stmt::Macro(m) => collect(&m.body, out),
            Stmt::CallBlock(c) => collect(&c.macro_decl.body, out),
            _ => {}
        }
    }
}

fn printed_constant<'s>(expr: &'s Expr<'_>) -> Option<Segment<'s>> {
    match expr {
        Expr::Const(c) => c.value.as_str().map(|text| Segment {
            span: c.span(),
            text,
        }),
        _ => None,
    }
}

fn check_c(file: &str, segments: &[Segment<'_>], violations: &mut Vec<Violation>) {
    // Lex all literal text as one stream so a comment or string literal that a
    // template expression interrupts keeps its state; an issued value stands in
    // as one opaque identifier.
    let mut joined = String::new();
    let mut starts = Vec::with_capacity(segments.len());
    for segment in segments {
        starts.push(joined.len());
        joined.push_str(segment.text);
        joined.push_str(" __issued__ ");
    }
    let locate = |offset: usize| -> (u16, u16) {
        let index = starts
            .partition_point(|&start| start <= offset)
            .saturating_sub(1);
        let segment = &segments[index];
        let local = &segment.text[..(offset - starts[index]).min(segment.text.len())];
        let newlines = local.matches('\n').count();
        let line = segment
            .span
            .start_line
            .saturating_add(u16::try_from(newlines).unwrap_or(u16::MAX));
        let column = match local.rfind('\n') {
            Some(at) => u16::try_from(local.len() - at - 1).unwrap_or(u16::MAX),
            None => segment
                .span
                .start_col
                .saturating_add(u16::try_from(local.len()).unwrap_or(u16::MAX)),
        };
        (line, column)
    };
    for (offset, detail) in forbidden_c_tokens(&joined) {
        let (line, column) = locate(offset);
        violations.push(Violation {
            file: file.to_owned(),
            line,
            column,
            rule: Rule::LiteralExecutable,
            detail,
        });
    }
}

/// Offsets and descriptions of executable-body tokens in C text.
pub(super) fn forbidden_c_tokens(text: &str) -> Vec<(usize, String)> {
    let mut lexer = Lexer {
        bytes: text.as_bytes(),
        text,
        at: 0,
        line_start: true,
        found: Vec::new(),
    };
    lexer.run();
    lexer.found
}

struct Lexer<'t> {
    bytes: &'t [u8],
    text: &'t str,
    at: usize,
    line_start: bool,
    found: Vec<(usize, String)>,
}

impl Lexer<'_> {
    fn run(&mut self) {
        while self.at < self.bytes.len() {
            let c = self.bytes[self.at];
            if c == b'\n' {
                self.line_start = true;
                self.at += 1;
            } else if c.is_ascii_whitespace() {
                self.at += 1;
            } else if c == b'#' && self.line_start {
                self.directive();
            } else {
                self.line_start = false;
                self.token(c);
            }
        }
    }

    fn token(&mut self, c: u8) {
        let rest = &self.text[self.at..];
        if rest.starts_with("//") {
            self.at += rest.find('\n').unwrap_or(rest.len());
        } else if rest.starts_with("/*") {
            self.at += rest[2..].find("*/").map_or(rest.len(), |end| end + 4);
        } else if c == b'"' || c == b'\'' {
            self.quoted(c);
        } else if c.is_ascii_alphabetic() || c == b'_' {
            self.word();
        } else if c.is_ascii_digit()
            || (c == b'.' && self.bytes.get(self.at + 1).is_some_and(u8::is_ascii_digit))
        {
            self.number();
        } else {
            self.punctuator(rest);
        }
    }

    fn quoted(&mut self, quote: u8) {
        self.at += 1;
        while self.at < self.bytes.len()
            && self.bytes[self.at] != quote
            && self.bytes[self.at] != b'\n'
        {
            self.at += if self.bytes[self.at] == b'\\' { 2 } else { 1 };
        }
        self.at += 1;
    }

    fn word(&mut self) {
        let start = self.at;
        while self.at < self.bytes.len()
            && (self.bytes[self.at].is_ascii_alphanumeric() || self.bytes[self.at] == b'_')
        {
            self.at += 1;
        }
        let word = &self.text[start..self.at];
        if CONTROL_KEYWORDS.contains(&word) {
            self.found
                .push((start, format!("control flow `{word}` in literal text")));
        }
    }

    fn number(&mut self) {
        let start = self.at;
        let hex = self.text[start..].starts_with("0x") || self.text[start..].starts_with("0X");
        while self.at < self.bytes.len() {
            let c = self.bytes[self.at];
            let exponent_sign = (c == b'+' || c == b'-')
                && matches!(self.bytes[self.at - 1], b'e' | b'E' | b'p' | b'P')
                && (!hex || matches!(self.bytes[self.at - 1], b'p' | b'P'));
            if !(c.is_ascii_alphanumeric() || c == b'.' || c == b'_' || exponent_sign) {
                break;
            }
            self.at += 1;
        }
        let literal = &self.text[start..self.at];
        let floating = literal.contains('.')
            || if hex {
                literal.contains(['p', 'P'])
            } else {
                literal.contains(['e', 'E'])
            };
        if floating {
            self.found.push((
                start,
                format!("numeric constant `{literal}` in literal text"),
            ));
        }
    }

    fn punctuator(&mut self, rest: &str) {
        match ACCUMULATING.iter().find(|op| rest.starts_with(**op)) {
            Some(op) => {
                self.found.push((
                    self.at,
                    format!("accumulating operator `{op}` in literal text"),
                ));
                self.at += op.len();
            }
            None => self.at += rest.chars().next().map_or(1, char::len_utf8),
        }
    }

    /// A preprocessor line. `#define` bodies are code and are lexed; other
    /// directives (`#include`, `#if`, `#error`, ...) are compile-time only.
    fn directive(&mut self) {
        let rest = &self.text[self.at + 1..];
        let name_start = rest.len() - rest.trim_start_matches([' ', '\t']).len();
        let is_define = rest[name_start..].starts_with("define");
        let mut end = self.at;
        loop {
            let line_end = self.text[end..]
                .find('\n')
                .map_or(self.text.len(), |n| end + n);
            end = line_end;
            if line_end == self.text.len() || !self.text[..line_end].trim_end().ends_with('\\') {
                break;
            }
            end += 1;
        }
        if is_define {
            let body_start = self.at + 1 + name_start + "define".len();
            let mut inner = Lexer {
                bytes: self.bytes,
                text: self.text,
                at: body_start,
                line_start: false,
                found: Vec::new(),
            };
            while inner.at < end {
                let c = inner.bytes[inner.at];
                if c.is_ascii_whitespace() || c == b'\\' {
                    inner.at += 1
                } else {
                    inner.token(c)
                }
            }
            self.found.append(&mut inner.found);
        }
        self.at = end;
    }
}
