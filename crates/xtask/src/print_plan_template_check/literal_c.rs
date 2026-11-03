//! Literal target text of a covered template: a text node can itself be a
//! solver, an AD rule or a scheduler with no template expression in it, so the
//! printed C is checked too. Numerical bodies must arrive as issued
//! instructions; literal text may only spell declarations, punctuation and the
//! fixed spelling of an issued instruction.
//!
//! Admission is positive: literal C may hold only declaration specifiers in a
//! declaration-specifier position (base types, the fixed-width and size type
//! names, qualifiers, storage classes and tag keywords), `return` before an
//! issued value, the punctuation of declarations, calls, indexing, member access
//! and assignment, comments, and `#pragma once`. Every other name, every
//! number, every string or character literal, every include, every other
//! directive and every operator must arrive as an issued value. There is no reviewed ABI-fragment
//! exemption. A language with no literal check fails closed on any literal text
//! at all.
//!
//! Rendering can join text the template keeps apart, so the check reasons about
//! what is printed, not about each text node alone:
//! - Literal text and printed string constants that are adjacent in one
//!   statement list form one run, lexed as one stream after C line splicing.
//! - Literal text may not contain a NUL byte: the lexer's issued-value marker
//!   is minted by this check alone, never by literal bytes.
//! - An issued value or include prints text this check cannot see. It must sit
//!   between whitespace or delimiters in its own run, so it cannot complete a
//!   literal token, and two of them may not touch.
//! - At a control-block edge the text on either side depends on a branch or an
//!   iteration, so literal text must start and end there with whitespace or a
//!   delimiter, and a run may not end inside a comment, string, character
//!   literal or directive.

use super::{Language, Rule, Violation};
use minijinja::machinery::Span;
use minijinja::machinery::ast::{Expr, Stmt};

/// Base type keywords: they may combine with each other, as in `unsigned long`.
const BASE_TYPES: &[&str] = &[
    "void", "char", "short", "int", "long", "float", "double", "signed", "unsigned", "_Bool",
];
/// Fixed-width and size type names: one per declaration specifier list.
const NAMED_TYPES: &[&str] = &[
    "bool",
    "int8_t",
    "int16_t",
    "int32_t",
    "int64_t",
    "uint8_t",
    "uint16_t",
    "uint32_t",
    "uint64_t",
    "size_t",
    "ptrdiff_t",
    "intptr_t",
    "uintptr_t",
];
/// Qualifiers and storage classes.
const QUALIFIERS: &[&str] = &[
    "const", "volatile", "restrict", "static", "extern", "inline", "typedef",
];
/// Tag keywords: an issued tag or a `{` must follow.
const TAGS: &[&str] = &["struct", "union", "enum"];
/// Multi-character punctuators, longest first; only `->` is admitted.
const MULTI_PUNCTUATORS: &[&str] = &[
    "<<=", ">>=", "...", "%:%:", "->", "++", "--", "<<", ">>", "<=", ">=", "==", "!=", "&&", "||",
    "*=", "/=", "%=", "+=", "-=", "&=", "^=", "|=", "##", "%:", "<:", ":>", "<%", "%>",
];
const ADMITTED_PUNCTUATION: &[u8] = b"()[]{},;=.";
/// Delimiters, as `char`s, for the join checks.
const DELIMITERS: &[char] = &['(', ')', '[', ']', '{', '}', ',', ';'];
/// An issued value or include in a lexed run. Its NUL byte is minted only
/// here: a NUL in literal text is flagged and lexed as a rejected byte.
const OPAQUE: &str = " \u{0} ";

pub(super) fn check(
    file: &str,
    body: &[Stmt<'_>],
    language: Language,
    violations: &mut Vec<Violation>,
) {
    match language {
        Language::C => {
            let mut checker = CText { file, violations };
            match body {
                [Stmt::Template(t)] => checker.list(&t.children, Edge::File, Edge::File),
                _ => checker.list(body, Edge::Block, Edge::Block),
            }
        }
        Language::Unsupported => unsupported(file, body, violations),
    }
}

/// Without a literal check for the language, any literal text fails.
fn unsupported(file: &str, body: &[Stmt<'_>], violations: &mut Vec<Violation>) {
    let mut found = None;
    visit_text(body, &mut |span, text| {
        if found.is_none() && !text.trim().is_empty() {
            found = Some(span);
        }
    });
    if let Some(span) = found {
        violations.push(Violation {
            file: file.to_owned(),
            line: span.start_line,
            column: span.start_col,
            rule: Rule::UnsupportedLiteralLanguage,
            detail: "literal text in a template whose target language has no literal check"
                .to_owned(),
        });
    }
}

fn visit_text(stmts: &[Stmt<'_>], each: &mut dyn FnMut(Span, &str)) {
    for stmt in stmts {
        match piece(stmt) {
            Some(Piece::Text { span, text }) => each(span, text),
            Some(Piece::Opaque { .. }) => {}
            None => {
                for body in bodies(stmt) {
                    visit_text(body, each);
                }
            }
        }
    }
}

/// What borders a run: the start or end of an artifact file, where nothing is
/// printed beyond it, or a control block whose neighbouring text varies.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Edge {
    File,
    Block,
}

/// A statement as printed text: literal text (including a printed string
/// constant) or an opaque piece whose text this check cannot see.
enum Piece<'s> {
    Text { span: Span, text: &'s str },
    Opaque { span: Span, what: &'static str },
}

fn piece<'s>(stmt: &'s Stmt<'_>) -> Option<Piece<'s>> {
    match stmt {
        Stmt::EmitRaw(r) => Some(Piece::Text {
            span: r.span(),
            text: r.raw,
        }),
        Stmt::EmitExpr(e) => Some(match &e.expr {
            Expr::Const(c) if c.value.as_str().is_some() => Piece::Text {
                span: c.span(),
                text: c.value.as_str().unwrap_or_default(),
            },
            _ => Piece::Opaque {
                span: e.span(),
                what: "issued value",
            },
        }),
        Stmt::Include(i) => Some(Piece::Opaque {
            span: i.span(),
            what: "include",
        }),
        _ => None,
    }
}

fn bodies<'s, 'a>(stmt: &'s Stmt<'a>) -> Vec<&'s [Stmt<'a>]> {
    match stmt {
        Stmt::Template(t) => vec![&t.children],
        Stmt::ForLoop(f) => vec![&f.body, &f.else_body],
        Stmt::IfCond(i) => vec![&i.true_body, &i.false_body],
        Stmt::Block(b) => vec![&b.body],
        Stmt::WithBlock(w) => vec![&w.body],
        Stmt::SetBlock(s) => vec![&s.body],
        Stmt::FilterBlock(f) => vec![&f.body],
        Stmt::AutoEscape(a) => vec![&a.body],
        Stmt::Macro(m) => vec![&m.body],
        Stmt::CallBlock(c) => vec![&c.macro_decl.body],
        _ => Vec::new(),
    }
}

struct CText<'f, 'v> {
    file: &'f str,
    violations: &'v mut Vec<Violation>,
}

impl CText<'_, '_> {
    fn flag(&mut self, (line, column): (u16, u16), detail: String) {
        self.violations.push(Violation {
            file: self.file.to_owned(),
            line,
            column,
            rule: Rule::LiteralExecutable,
            detail,
        });
    }

    /// Splits a statement list into runs at every statement that is not
    /// printed text; such a statement (a branch, a loop, a `set`, ...) is a
    /// block edge for the runs on either side, and its bodies are lists too.
    fn list(&mut self, stmts: &[Stmt<'_>], first: Edge, last: Edge) {
        let mut run = Vec::new();
        let mut left = first;
        for stmt in stmts {
            if let Some(p) = piece(stmt) {
                run.push(p);
                continue;
            }
            self.run(&run, left, Edge::Block);
            run.clear();
            for body in bodies(stmt) {
                self.list(body, Edge::Block, Edge::Block);
            }
            left = Edge::Block;
        }
        self.run(&run, left, last);
    }

    fn run(&mut self, run: &[Piece<'_>], left: Edge, right: Edge) {
        let pieces: Vec<&Piece<'_>> = run
            .iter()
            .filter(|p| !matches!(p, Piece::Text { text, .. } if text.is_empty()))
            .collect();
        for current in &pieces {
            if let Piece::Text { span, text } = current
                && let Some(at) = text.find('\0')
            {
                self.flag(
                    position(*span, &text[..at]),
                    "NUL byte in literal text: only the check itself marks an issued value"
                        .to_owned(),
                );
            }
        }
        for (index, current) in pieces.iter().enumerate() {
            let before = index.checked_sub(1).map(|i| pieces[i]);
            let after = pieces.get(index + 1).copied();
            self.edges(current, before, after, (left, right));
        }
        self.lex(&pieces, right);
    }

    /// The join checks for one piece against its neighbours in the run, or
    /// against the run's edges.
    fn edges(
        &mut self,
        current: &Piece<'_>,
        before: Option<&Piece<'_>>,
        after: Option<&Piece<'_>>,
        (left, right): (Edge, Edge),
    ) {
        match current {
            Piece::Opaque { span, what } => {
                self.opaque_edges(*span, what, before, after, (left, right));
            }
            Piece::Text { span, text } => {
                let (first, last) = edge_chars(text);
                if before.is_none() && left == Edge::Block && !delimits(first) {
                    self.flag((span.start_line, span.start_col), "literal text at a control-block edge must start with whitespace or a delimiter".to_owned());
                }
                if after.is_none() && right == Edge::Block && !delimits(last) {
                    self.flag((span.start_line, span.start_col), "literal text at a control-block edge must end with whitespace or a delimiter".to_owned());
                }
            }
        }
    }

    fn opaque_edges(
        &mut self,
        span: Span,
        what: &str,
        before: Option<&Piece<'_>>,
        after: Option<&Piece<'_>>,
        (left, right): (Edge, Edge),
    ) {
        for (side, neighbour, edge) in [("before", before, left), ("after", after, right)] {
            if !opaque_side_safe(neighbour, edge, side == "before") {
                let detail = format!(
                    "{what} not delimited {side} by whitespace or ( ) [ ] {{ }} , ; in literal text"
                );
                self.flag((span.start_line, span.start_col), detail);
            }
        }
    }

    /// Lexes one run as the C it prints: text joined directly, an opaque piece
    /// as one separate identifier.
    fn lex(&mut self, pieces: &[&Piece<'_>], right: Edge) {
        let mut joined = String::new();
        let mut starts = Vec::with_capacity(pieces.len());
        for p in pieces {
            starts.push(joined.len());
            match p {
                Piece::Text { text, .. } => joined.push_str(&text.replace('\0', "\u{1}")),
                Piece::Opaque { .. } => joined.push_str(OPAQUE),
            }
        }
        let locate = |offset: usize| -> (u16, u16) {
            let index = starts.partition_point(|&s| s <= offset).saturating_sub(1);
            match pieces.get(index) {
                Some(Piece::Text { span, text }) => {
                    position(*span, &text[..(offset - starts[index]).min(text.len())])
                }
                Some(Piece::Opaque { span, .. }) => (span.start_line, span.start_col),
                None => (0, 0),
            }
        };
        let (found, open) = scan(&joined);
        for (offset, detail) in found {
            self.flag(locate(offset), detail);
        }
        if let Some((offset, construct)) = open
            && (right == Edge::Block || construct != "directive")
        {
            self.flag(
                locate(offset),
                format!("literal text ends inside a {construct} at a template boundary"),
            );
        }
    }
}

/// Whether the text beside an opaque piece ends any token it could print.
fn opaque_side_safe(neighbour: Option<&Piece<'_>>, edge: Edge, before: bool) -> bool {
    match neighbour {
        Some(Piece::Text { text, .. }) => {
            let (first, last) = edge_chars(text);
            delimits(if before { last } else { first })
        }
        Some(Piece::Opaque { .. }) => false,
        None => edge == Edge::File,
    }
}

fn position(span: Span, prefix: &str) -> (u16, u16) {
    let newlines = u16::try_from(prefix.matches('\n').count()).unwrap_or(u16::MAX);
    let column = match prefix.rfind('\n') {
        Some(at) => u16::try_from(prefix.len() - at - 1).unwrap_or(u16::MAX),
        None => span
            .start_col
            .saturating_add(u16::try_from(prefix.len()).unwrap_or(u16::MAX)),
    };
    (span.start_line.saturating_add(newlines), column)
}

/// First and last character a text prints after C line splicing.
fn edge_chars(text: &str) -> (Option<char>, Option<char>) {
    let (spliced, _) = splice(text);
    (spliced.chars().next(), spliced.chars().next_back())
}

fn delimits(c: Option<char>) -> bool {
    c.is_some_and(|c| c.is_whitespace() || DELIMITERS.contains(&c))
}

/// C translation phase 2: a backslash before a newline joins the lines. The
/// result maps each byte back to its offset in `text`.
fn splice(text: &str) -> (String, Vec<usize>) {
    let mut out = String::with_capacity(text.len());
    let mut map = Vec::with_capacity(text.len());
    let mut at = 0;
    while let Some(c) = text[at..].chars().next() {
        let rest = &text[at..];
        let join = ["\\\r\n", "\\\n"]
            .into_iter()
            .find(|j| rest.starts_with(*j));
        if let Some(join) = join {
            at += join.len();
            continue;
        }
        out.push(c);
        map.extend(std::iter::repeat_n(at, c.len_utf8()));
        at += c.len_utf8();
    }
    map.push(text.len());
    (out, map)
}

/// Offsets in `text` and descriptions of executable-body tokens in C text,
/// plus the construct the text ends inside, if any.
type FoundTokens = Vec<(usize, String)>;
type OpenConstruct = Option<(usize, &'static str)>;

pub(super) fn scan(text: &str) -> (FoundTokens, OpenConstruct) {
    let mut found = trigraphs(text);
    let (spliced, map) = splice(text);
    let mut lexer = Lexer::new(&spliced, 0);
    lexer.run();
    found.extend(
        lexer
            .found
            .into_iter()
            .map(|(offset, detail)| (map[offset], detail)),
    );
    found.sort();
    (
        found,
        lexer
            .open
            .map(|(offset, construct)| (map[offset], construct)),
    )
}

/// Offsets and descriptions of executable-body tokens in C text.
#[cfg(test)]
pub(super) fn forbidden_c_tokens(text: &str) -> Vec<(usize, String)> {
    let (mut found, open) = scan(text);
    found.extend(open.map(|(offset, construct)| (offset, format!("ends inside a {construct}"))));
    found
}

/// Trigraphs are replaced before line splicing (`??/` is a backslash), so a
/// strict C99 compile reads text no check here sees: fail closed.
fn trigraphs(text: &str) -> Vec<(usize, String)> {
    text.match_indices("??")
        .filter(|(at, _)| {
            text[at + 2..].starts_with(['=', '/', '\'', '(', ')', '!', '<', '>', '-'])
        })
        .map(|(at, _)| (at, "trigraph in literal text".to_owned()))
        .collect()
}

/// The role of the previous significant token, for the declaration roles of
/// the words that follow it.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Prev {
    /// A statement or declaration may start: the text start, `(` `{` `[` `,`
    /// `;` `}` `)` or `=`.
    Boundary,
    BaseType,
    NamedType,
    Qualifier,
    Tag,
    Return,
    Star,
    /// An issued value.
    Value,
    /// An issued tag after `struct`, `union` or `enum`: a type, so `*` may follow.
    TagName,
    /// Member access, whose member must be issued.
    Access,
}

struct Lexer<'t> {
    bytes: &'t [u8],
    text: &'t str,
    at: usize,
    line_start: bool,
    prev: Prev,
    found: Vec<(usize, String)>,
    open: Option<(usize, &'static str)>,
}

impl<'t> Lexer<'t> {
    fn new(text: &'t str, at: usize) -> Self {
        Self {
            bytes: text.as_bytes(),
            text,
            at,
            line_start: true,
            prev: Prev::Boundary,
            found: Vec::new(),
            open: None,
        }
    }

    fn reject(&mut self, at: usize, detail: String) {
        self.found.push((at, detail));
    }

    fn run(&mut self) {
        while self.at < self.bytes.len() {
            let c = self.bytes[self.at];
            if c == b'\n' {
                self.line_start = true;
                self.at += 1;
            } else if c == 0 {
                self.issued();
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

    /// An issued value: a whole token the plan supplied. A tag, `return`,
    /// member access or a declaration specifier list may precede it.
    fn issued(&mut self) {
        self.prev = if self.prev == Prev::Tag {
            Prev::TagName
        } else {
            Prev::Value
        };
        self.at += 1;
    }

    fn token(&mut self, c: u8) {
        let rest = &self.text[self.at..];
        if rest.starts_with("//") {
            self.at += rest.find('\n').unwrap_or(rest.len());
        } else if let Some(comment) = rest.strip_prefix("/*") {
            match comment.find("*/") {
                Some(end) => self.at += end + 4,
                None => self.end_open("comment"),
            }
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

    fn end_open(&mut self, construct: &'static str) {
        self.open = Some((self.at, construct));
        self.at = self.bytes.len();
    }

    /// String and character literals are data the plan has to issue: a
    /// character literal is an integer constant, and a string can be indexed.
    fn quoted(&mut self, quote: u8) {
        let start = self.at;
        self.at += 1;
        while self.at < self.bytes.len()
            && self.bytes[self.at] != quote
            && self.bytes[self.at] != b'\n'
        {
            self.at += if self.bytes[self.at] == b'\\' { 2 } else { 1 };
        }
        let kind = if quote == b'"' {
            "string literal"
        } else {
            "character literal"
        };
        if self.bytes.get(self.at) == Some(&quote) {
            self.at += 1;
            self.reject(
                start,
                format!("{kind} in literal text: constants are issued"),
            );
            self.prev = Prev::Value;
        } else {
            self.at = start;
            self.end_open(kind);
        }
    }

    fn word(&mut self) {
        let start = self.at;
        while self.at < self.bytes.len()
            && (self.bytes[self.at].is_ascii_alphanumeric() || self.bytes[self.at] == b'_')
        {
            self.at += 1;
        }
        let word = &self.text[start..self.at];
        let (role, admitted) = word_role(word, self.prev);
        match role {
            None => self.reject(
                start,
                format!(
                    "name `{word}` in literal text: names other than declaration words are issued"
                ),
            ),
            Some(_) if !admitted => self.reject(
                start,
                format!("`{word}` in literal text outside a declaration specifier position"),
            ),
            Some(_) => {}
        }
        self.prev = role.unwrap_or(Prev::Value);
    }

    /// `return`, a tag keyword and member access must be followed by an issued
    /// value (or `;`, `{`).
    fn follow_rule_failed(&mut self, at: usize, token: &str) {
        let what = match self.prev {
            Prev::Return => "`return` must be followed by an issued value or `;`",
            Prev::Tag => "a tag keyword must be followed by an issued tag or `{`",
            _ => "member access must name an issued member",
        };
        self.reject(at, format!("{what}, not `{token}`"));
    }

    fn number(&mut self) {
        let start = self.at;
        while self.at < self.bytes.len() {
            let c = self.bytes[self.at];
            let exponent_sign = (c == b'+' || c == b'-')
                && matches!(self.bytes[self.at - 1], b'e' | b'E' | b'p' | b'P');
            if !(c.is_ascii_alphanumeric() || c == b'.' || c == b'_' || exponent_sign) {
                break;
            }
            self.at += 1;
        }
        let literal = &self.text[start..self.at];
        self.reject(
            start,
            format!("numeric constant `{literal}` in literal text: constants are issued"),
        );
        self.prev = Prev::Value;
    }

    fn punctuator(&mut self, rest: &str) {
        let start = self.at;
        if let Some(op) = MULTI_PUNCTUATORS.iter().find(|op| rest.starts_with(**op)) {
            self.at += op.len();
            if *op != "->" {
                self.reject(
                    start,
                    format!("operator `{op}` in literal text: operators are issued"),
                );
            }
            self.after_punctuator(start, op);
            self.prev = if *op == "->" {
                Prev::Access
            } else {
                Prev::Boundary
            };
            return;
        }
        let c = rest.chars().next().unwrap_or_default();
        self.at += c.len_utf8().max(1);
        let token = c.to_string();
        if c == '*' {
            if !matches!(
                self.prev,
                Prev::BaseType | Prev::NamedType | Prev::Qualifier | Prev::Star | Prev::TagName
            ) {
                self.reject(
                    start,
                    "operator `*` in literal text: only a pointer declarator after a type"
                        .to_owned(),
                );
            }
            self.prev = Prev::Star;
            return;
        }
        if !c.is_ascii() || !ADMITTED_PUNCTUATION.contains(&(c as u8)) {
            self.reject(
                start,
                format!("operator `{c}` in literal text: operators are issued"),
            );
        }
        self.after_punctuator(start, &token);
        self.prev = if c == '.' {
            Prev::Access
        } else {
            Prev::Boundary
        };
    }

    /// The follow rules of the previous token, and `(` after a type word,
    /// which would start a function-style cast or a parenthesized declarator.
    fn after_punctuator(&mut self, at: usize, token: &str) {
        let allowed = match self.prev {
            Prev::Return => token == ";",
            Prev::Tag => token == "{",
            Prev::Access => false,
            Prev::BaseType | Prev::NamedType | Prev::Qualifier => token != "(",
            _ => true,
        };
        if !allowed {
            if matches!(
                self.prev,
                Prev::BaseType | Prev::NamedType | Prev::Qualifier
            ) {
                self.reject(at, "`(` after a type word in literal text".to_owned());
            } else {
                self.follow_rule_failed(at, token);
            }
        }
    }

    /// A preprocessor line (already spliced). Only `#pragma once` is admitted:
    /// it rewrites no token. An include brings in text whose closure no gate
    /// here has authorized, so a needed header arrives as an issued directive;
    /// any `#define`, even a bare one, can rewrite the tokens this check and a
    /// decoder read (`#define return`), and a conditional selects text the
    /// check cannot follow. An include guard is therefore not literal text.
    fn directive(&mut self) {
        let start = self.at;
        let end = self.text[start..]
            .find('\n')
            .map_or(self.bytes.len(), |n| start + n);
        let line = self.text[start + 1..end].trim();
        let mut words = line.split_whitespace();
        let name = words.next().unwrap_or_default();
        let rest: Vec<&str> = words.collect();
        if !(name == "pragma" && rest == ["once"]) {
            self.reject(start, format!("directive `#{line}` in literal text: only `#pragma once` is admitted; includes and guards are issued"));
        }
        self.at = end;
        if end == self.bytes.len() {
            self.at = start;
            self.end_open("directive");
        }
    }
}

/// A word's role, and whether that role may follow `prev`. `None` is a name,
/// which literal text never spells.
fn word_role(word: &str, prev: Prev) -> (Option<Prev>, bool) {
    let declaration_start = matches!(prev, Prev::Boundary | Prev::Qualifier);
    if BASE_TYPES.contains(&word) {
        (
            Some(Prev::BaseType),
            declaration_start || prev == Prev::BaseType,
        )
    } else if NAMED_TYPES.contains(&word) {
        (Some(Prev::NamedType), declaration_start)
    } else if QUALIFIERS.contains(&word) {
        let after = matches!(
            prev,
            Prev::BaseType | Prev::NamedType | Prev::Star | Prev::TagName
        );
        (Some(Prev::Qualifier), declaration_start || after)
    } else if TAGS.contains(&word) {
        (Some(Prev::Tag), declaration_start)
    } else if word == "return" {
        (Some(Prev::Return), prev == Prev::Boundary)
    } else {
        (None, false)
    }
}
