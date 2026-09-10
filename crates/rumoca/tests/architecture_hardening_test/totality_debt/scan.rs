//! Line-oriented scan for totality-debt call forms in production Rust source.
//!
//! The scan never parses Rust. It strips comment text, string literals and
//! character literals from each line, walks the stripped lines tracking whether
//! a line belongs to a `#[cfg(test)]` item, and counts the call forms whose
//! totality argument is carried by a message string rather than by a type.
//!
//! Block comments and string literals span lines, so the stripper carries which
//! one a line ended inside into the next line. Without that carry, the body of a
//! multi-line literal reads as code, and one unbalanced brace in it moves the
//! item state machine off the real structure of the file. That failure is silent
//! in the dangerous direction: it drops shipped lines from the count and leaves
//! no trace at the end of the file for the tracking assertion to catch.
//!
//! Both directions of the remaining imprecision are deliberate and safe for a
//! count-may-only-fall gate:
//!
//! * Failing to recognise a test-only construct counts test code as production,
//!   which raises the count and trips the gate. An author sees it immediately.
//! * A `#[cfg(test)]` item is genuinely absent from a release build, so removing
//!   it from the count hides no shipped obligation.

/// Call forms that assert totality at run time behind a message string.
pub(crate) const TOTALITY_DEBT_FORMS: &[&str] = &[
    ".expect(",
    "panic!(",
    "unreachable!(",
    "todo!(",
    "unimplemented!(",
];

/// The same obligation with the message dropped, counted separately so that
/// rewriting `.expect("...")` into `.unwrap()` cannot make the debt look smaller.
pub(crate) const SILENT_TOTALITY_DEBT_FORM: &str = ".unwrap(";

const CFG_TEST_ATTRIBUTE: &str = "#[cfg(test)]";

/// Where a line sits relative to the nearest `#[cfg(test)]` item.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum ItemScan {
    /// Outside any `#[cfg(test)]` item.
    Production,
    /// A `#[cfg(test)]` attribute was seen and further attributes may follow.
    Attributes,
    /// The attributed item started but has not yet reached its opening brace or
    /// its terminator, as in a signature broken across several lines. Carries
    /// the running parenthesis and bracket depth, so that a separator inside an
    /// argument list is not mistaken for the end of the item.
    Header(i32),
    /// Inside the attributed item's brace block, at this nesting depth.
    Body(i32),
}

impl ItemScan {
    /// Consumes one line of stripped code, returning the next state and whether
    /// the line is production source.
    fn advance(self, code: &str) -> (Self, bool) {
        match self {
            Self::Production => Self::advance_production(code),
            Self::Attributes => (Self::advance_attributes(code), false),
            Self::Header(nesting) => (Self::advance_header(nesting, code), false),
            Self::Body(depth) => (Self::advance_body(depth, code), false),
        }
    }

    fn advance_production(code: &str) -> (Self, bool) {
        let Some(rest) = code.trim_start().strip_prefix(CFG_TEST_ATTRIBUTE) else {
            return (Self::Production, true);
        };
        (Self::advance_attributes(rest), false)
    }

    /// Continues past any further attributes to the item they cover.
    ///
    /// An attribute and its item may share a line. The item is therefore looked
    /// for on this line before the next one: reading the line below a joined
    /// attribute as the attributed item would hide a shipped line and lower the
    /// count.
    fn advance_attributes(code: &str) -> Self {
        match item_after_attributes(code) {
            Some(item) => Self::advance_header(0, item),
            None => Self::Attributes,
        }
    }

    /// Decides how far the attributed item reaches, given the depth carried
    /// from its earlier lines.
    fn advance_header(nesting: i32, code: &str) -> Self {
        match header_step(code, nesting) {
            HeaderStep::Block => Self::from_depth(brace_delta(code)),
            HeaderStep::Ends => Self::Production,
            HeaderStep::Continues(next) => Self::Header(next),
        }
    }

    fn advance_body(depth: i32, code: &str) -> Self {
        Self::from_depth(depth + brace_delta(code))
    }

    fn from_depth(depth: i32) -> Self {
        if depth > 0 {
            Self::Body(depth)
        } else {
            Self::Production
        }
    }
}

/// The item text after any attributes at the start of `code`, or `None` when
/// the line carries attributes only and the item begins on a later line.
fn item_after_attributes(code: &str) -> Option<&str> {
    let mut rest = code.trim_start();
    while rest.starts_with("#[") {
        rest = attribute_end(rest)?.trim_start();
    }
    (!rest.is_empty()).then_some(rest)
}

/// The text after the `#[...]` attribute at the start of `code`, or `None` when
/// that attribute does not close on this line.
///
/// The brackets are matched rather than counted from the left, so a nested
/// bracket in `#[cfg(feature = "x")]` or an index in an attribute argument does
/// not end the attribute early. Attribute text is already stripped, so a
/// bracket inside a string in the attribute is gone before this runs.
fn attribute_end(code: &str) -> Option<&str> {
    let mut depth = 0usize;
    for (index, ch) in code.char_indices() {
        match ch {
            '[' => depth += 1,
            ']' => {
                depth = depth.saturating_sub(1);
                if depth == 0 {
                    return code.get(index + 1..);
                }
            }
            _ => {}
        }
    }
    None
}

/// What one line of an attributed item's header says about where the item ends.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum HeaderStep {
    /// A `{` opens the item's body.
    Block,
    /// A `;` or `,` outside every bracket ends the item on this line: a
    /// `mod tests;` declaration, a `use`, a `let`, a struct field, a
    /// struct-literal entry.
    Ends,
    /// The item continues on the next line, at this bracket depth.
    Continues(i32),
}

/// Walks one header line. Separators nested inside parentheses or brackets do
/// not end an item, so an argument list broken across lines and an array length
/// such as `[u8; 4]` are both read as part of the header.
fn header_step(code: &str, mut nesting: i32) -> HeaderStep {
    for ch in code.chars() {
        match ch {
            '{' => return HeaderStep::Block,
            '[' | '(' => nesting += 1,
            ']' | ')' => nesting -= 1,
            ';' | ',' if nesting <= 0 => return HeaderStep::Ends,
            _ => {}
        }
    }
    HeaderStep::Continues(nesting)
}

fn brace_delta(code: &str) -> i32 {
    code.chars().fold(0, |delta, ch| match ch {
        '{' => delta + 1,
        '}' => delta - 1,
        _ => delta,
    })
}

/// One file's scan.
struct FileScan {
    /// Production lines as stripped code, keyed by zero-based line index.
    production: Vec<(usize, String)>,
    /// Whether the scan finished outside every block comment and every
    /// `#[cfg(test)]` item.
    tracked_to_end: bool,
}

/// Whether the scan read a file all the way to its end.
///
/// A scan that is still inside a `#[cfg(test)]` item, a block comment or a
/// string literal when the file runs out lost track of where that construct
/// ended, and every line from that point was dropped from the count. Asserting
/// this separately keeps a scanning mistake loud instead of turning it into a
/// silently lower number.
///
/// It catches only the constructs still open at the end. A construct that opens
/// and closes leaves no trace here even when its contents corrupted the item
/// scan in between, which is why the stripper carries every one of them across
/// lines rather than relying on this assertion.
pub(crate) fn scan_tracks_file_to_its_end(content: &str) -> bool {
    scan_file(content, CodeStripper::default()).tracked_to_end
}

/// The shipped code of a file, as the production lines joined back together
/// with string-literal text intact.
///
/// A scan that answers a question about shipped code must not be answerable by
/// a comment or by a `#[cfg(test)]` body: neither reaches a release build, so
/// neither can mint anything. Literal text is kept because an argument written
/// as a literal (a diagnostic mnemonic, say) is the answer the caller is
/// after; [`ITEM_PUNCTUATION`] is removed from it so a brace inside a format
/// string still cannot move the item scan.
pub(crate) fn production_code(content: &str) -> String {
    scan_file(content, CodeStripper::keeping_literal_text())
        .production
        .into_iter()
        .map(|(_, code)| code)
        .collect::<Vec<_>>()
        .join("\n")
}

fn scan_file(content: &str, mut stripper: CodeStripper) -> FileScan {
    let mut state = ItemScan::Production;
    let mut production = Vec::new();
    for (index, line) in content.lines().enumerate() {
        let code = stripper.strip(line);
        let (next, is_production) = state.advance(&code);
        state = next;
        if is_production {
            production.push((index, code));
        }
    }
    FileScan {
        tracked_to_end: state == ItemScan::Production && stripper.finished_in_code(),
        production,
    }
}

/// Every totality-debt occurrence in production source, as
/// `(zero-based line index, matched form)`. A line carrying two occurrences
/// yields two entries.
pub(crate) fn totality_debt_sites(content: &str) -> Vec<(usize, &'static str)> {
    let mut sites = Vec::new();
    for (index, code) in scan_file(content, CodeStripper::default()).production {
        for form in TOTALITY_DEBT_FORMS {
            sites.extend(std::iter::repeat_n(
                (index, *form),
                code.matches(form).count(),
            ));
        }
    }
    sites
}

/// Every `.unwrap()` occurrence in production source, as zero-based line indices.
pub(crate) fn silent_totality_debt_sites(content: &str) -> Vec<usize> {
    let mut sites = Vec::new();
    for (index, code) in scan_file(content, CodeStripper::default()).production {
        sites.extend(std::iter::repeat_n(
            index,
            code.matches(SILENT_TOTALITY_DEBT_FORM).count(),
        ));
    }
    sites
}

/// Names declared as `#[cfg(test)] mod <name>;`, whose backing file is compiled
/// only under `cfg(test)` and therefore holds no shipped obligation.
pub(crate) fn cfg_test_module_names(content: &str) -> Vec<String> {
    let mut names = Vec::new();
    let mut in_attributes = false;
    for line in code_lines(content) {
        let trimmed = line.trim_start();
        let attributed = if in_attributes {
            trimmed
        } else {
            match trimmed.strip_prefix(CFG_TEST_ATTRIBUTE) {
                Some(rest) => rest,
                None => continue,
            }
        };
        match item_after_attributes(attributed) {
            Some(item) => {
                in_attributes = false;
                names.extend(semicolon_module_name(item).map(str::to_owned));
            }
            None => in_attributes = true,
        }
    }
    names
}

/// Names declared as `mod <name>;`, used to follow a test-only module into the
/// files it in turn declares.
pub(crate) fn declared_module_names(content: &str) -> Vec<String> {
    let mut names = Vec::new();
    for line in code_lines(content) {
        names.extend(semicolon_module_name(line.trim_start()).map(str::to_owned));
    }
    names
}

/// Every line of a file as code, with comment and literal text removed.
///
/// Module declarations are read from stripped code for the same reason the
/// counts are: a commented-out `mod name;` is not a declaration, and treating
/// it as one would pull a shipped file into the test-only closure and hide its
/// obligations.
fn code_lines(content: &str) -> Vec<String> {
    let mut stripper = CodeStripper::default();
    content.lines().map(|line| stripper.strip(line)).collect()
}

/// Extracts `name` from a `mod name;` declaration, ignoring any visibility.
fn semicolon_module_name(trimmed: &str) -> Option<&str> {
    let after_visibility = strip_visibility(trimmed);
    let rest = after_visibility.strip_prefix("mod ")?;
    let name = rest.trim_end().strip_suffix(';')?.trim();
    (!name.is_empty() && name.chars().all(|ch| ch.is_alphanumeric() || ch == '_')).then_some(name)
}

fn strip_visibility(trimmed: &str) -> &str {
    let Some(rest) = trimmed.strip_prefix("pub") else {
        return trimmed;
    };
    match rest.strip_prefix('(') {
        Some(scoped) => scoped
            .find(')')
            .map_or(trimmed, |end| scoped[end + 1..].trim_start()),
        None => rest.trim_start(),
    }
}

/// Removes comment text, string-literal contents and character literals from
/// source lines, carrying whatever construct a line ends inside into the next
/// line.
///
/// Braces, brackets, parentheses and separators that sit inside a comment or a
/// literal are not code, and letting them through corrupts the item scan rather
/// than merely adding a stray match: one unbalanced `}` inside a block comment
/// closes a `#[cfg(test)]` body early, and one unbalanced `{` extends it past
/// its real end, dropping every production line in between from the count.
///
/// Block comments and string literals both span lines in Rust, so both have to
/// be carried. A carried construct also self-heals: a string that opens and
/// closes with a net-zero brace imbalance leaves the scan in code at the end of
/// the file, so the end-of-file tracking assertion never sees the corruption
/// and the lost lines are simply absent from the count.
#[derive(Default)]
struct CodeStripper {
    carry: Carry,
    literals: LiteralPolicy,
}

/// Punctuation that shapes an item, removed from any literal text a policy
/// keeps: `format!("{value}")` is one balanced pair of braces to the compiler
/// and none at all to the item scan, and a `,` or a `;` inside a message would
/// otherwise end a header line early.
const ITEM_PUNCTUATION: &[char] = &['{', '}', '(', ')', '[', ']', ';', ',', '#', '"', '\\'];

/// What the stripper does with the text inside a string literal.
#[derive(Clone, Copy, Default, PartialEq, Eq, Debug)]
enum LiteralPolicy {
    /// The text is dropped and the literal collapses to a bare `"`. This is
    /// what a scan for call forms wants: a form named inside a message string
    /// is prose, not a call.
    #[default]
    Collapse,
    /// The text is kept, with [`ITEM_PUNCTUATION`] removed so it still cannot
    /// move the item scan. A scan that reads an argument written as a literal
    /// (a diagnostic mnemonic, say) needs the text the argument holds.
    KeepText,
}

/// The construct a line ends inside, and the next line therefore begins inside.
///
/// A block comment and a string literal are mutually exclusive: neither one's
/// opener is an opener inside the other, so one variant carries all of it.
#[derive(Clone, Copy, Default, PartialEq, Eq, Debug)]
enum Carry {
    /// The line ended in ordinary code.
    #[default]
    Code,
    /// Inside a block comment nested this many levels deep, never zero.
    BlockComment(usize),
    /// Inside a `"..."` literal, in any of its `b`/`c` prefixed forms. Escapes
    /// apply, so `\"` does not close it.
    CookedString,
    /// Inside an `r"..."` literal, in any of its `b`/`c` prefixed forms, closed
    /// by a quote followed by this many `#`. Escapes do not apply.
    RawString(usize),
}

/// How far a literal reached on one line.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum LiteralScan {
    /// The literal closed on this line, just before this byte index.
    Closed(usize),
    /// The literal runs on into the next line.
    Open,
}

impl CodeStripper {
    /// A stripper that keeps the text inside string literals.
    fn keeping_literal_text() -> Self {
        Self {
            carry: Carry::default(),
            literals: LiteralPolicy::KeepText,
        }
    }

    /// Whether the stripper ended in ordinary code rather than part-way through
    /// a block comment or a multi-line string literal.
    fn finished_in_code(&self) -> bool {
        self.carry == Carry::Code
    }

    /// Returns the code of one line. Under the default policy a string literal
    /// collapses to a bare `"` at the point it opens so the line still reads as
    /// one token sequence; a character literal and all comment text are dropped
    /// outright either way. A line that resumes a string opened above it
    /// contributes only the code that follows the closing quote.
    fn strip(&mut self, line: &str) -> String {
        let mut code = String::with_capacity(line.len());
        let mut index = 0;
        while index < line.len() {
            index = match self.carry {
                Carry::BlockComment(depth) => self.step_through_block_comment(depth, line, index),
                Carry::CookedString => {
                    self.resume(line, index, 1, cooked_string_scan(line, index), &mut code)
                }
                Carry::RawString(hashes) => self.resume(
                    line,
                    index,
                    1 + hashes,
                    raw_string_scan(line, index, hashes),
                    &mut code,
                ),
                Carry::Code => match self.step_through_code(line, index, &mut code) {
                    Some(next) => next,
                    None => break,
                },
            };
        }
        code
    }

    /// Consumes one construct of ordinary code, returning the next byte index,
    /// or `None` when the rest of the line is a line comment.
    fn step_through_code(&mut self, line: &str, index: usize, code: &mut String) -> Option<usize> {
        if line[index..].starts_with("//") {
            return None;
        }
        if line[index..].starts_with("/*") {
            self.carry = Carry::BlockComment(1);
            return Some(index + 2);
        }
        if let Some((body, hashes)) = raw_string_open(line, index) {
            code.push('"');
            self.carry = Carry::RawString(hashes);
            let scan = raw_string_scan(line, body, hashes);
            return Some(self.resume(line, body, 1 + hashes, scan, code));
        }
        if let Some(end) = char_literal_end(line, index) {
            return Some(end);
        }
        let ch = line[index..].chars().next()?;
        if ch == '"' {
            code.push('"');
            self.carry = Carry::CookedString;
            let body = index + ch.len_utf8();
            let scan = cooked_string_scan(line, body);
            return Some(self.resume(line, body, 1, scan, code));
        }
        code.push(ch);
        Some(index + ch.len_utf8())
    }

    /// Applies a literal scan: a literal that closed returns the scan to code,
    /// and one still open consumes the rest of the line and stays carried.
    ///
    /// `body` is the first byte of the literal's text and `closer` the width of
    /// the delimiter that ends it, so a policy that keeps the text knows which
    /// bytes are text rather than delimiter.
    fn resume(
        &mut self,
        line: &str,
        body: usize,
        closer: usize,
        scan: LiteralScan,
        code: &mut String,
    ) -> usize {
        match scan {
            LiteralScan::Closed(end) => {
                self.carry = Carry::Code;
                self.keep(&line[body..end - closer], code);
                if self.literals == LiteralPolicy::KeepText {
                    code.push('"');
                }
                end
            }
            LiteralScan::Open => {
                self.keep(&line[body..], code);
                line.len()
            }
        }
    }

    /// Appends the text a literal holds, when the policy keeps it.
    fn keep(&self, text: &str, code: &mut String) {
        if self.literals == LiteralPolicy::Collapse {
            return;
        }
        code.extend(text.chars().filter(|ch| !ITEM_PUNCTUATION.contains(ch)));
    }

    /// Advances one step inside a block comment. Rust nests block comments, so
    /// the depth rises on `/*` and falls on `*/`.
    fn step_through_block_comment(&mut self, depth: usize, line: &str, index: usize) -> usize {
        if line[index..].starts_with("/*") {
            self.carry = Carry::BlockComment(depth + 1);
            return index + 2;
        }
        if line[index..].starts_with("*/") {
            self.carry = match depth {
                0 | 1 => Carry::Code,
                _ => Carry::BlockComment(depth - 1),
            };
            return index + 2;
        }
        index + line[index..].chars().next().map_or(1, char::len_utf8)
    }
}

/// Walks the body of a cooked string from `start` to its closing quote.
///
/// The scan always begins unescaped. A `\` at the end of a line escapes the
/// newline itself, which line-oriented iteration has already removed, so the
/// escape is spent before the next line begins.
fn cooked_string_scan(line: &str, start: usize) -> LiteralScan {
    let mut index = start;
    let mut escaped = false;
    while index < line.len() {
        let Some(ch) = line[index..].chars().next() else {
            break;
        };
        index += ch.len_utf8();
        if escaped {
            escaped = false;
            continue;
        }
        match ch {
            '\\' => escaped = true,
            '"' => return LiteralScan::Closed(index),
            _ => {}
        }
    }
    LiteralScan::Open
}

/// Recognises a raw-string opener at `start`, returning the byte index just
/// past its opening quote together with the `#` count that closes it.
///
/// `r#foo` is a raw identifier rather than a raw string, and is rejected by the
/// missing quote after the hashes.
fn raw_string_open(line: &str, start: usize) -> Option<(usize, usize)> {
    let bytes = line.as_bytes();
    let mut index = match bytes.get(start..) {
        Some([b'b' | b'c', b'r', ..]) => start + 2,
        Some([b'r', ..]) => start + 1,
        _ => return None,
    };

    let mut hashes = 0;
    while bytes.get(index) == Some(&b'#') {
        index += 1;
        hashes += 1;
    }
    (bytes.get(index) == Some(&b'"')).then_some((index + 1, hashes))
}

/// Walks the body of a raw string from `start` to a quote followed by `hashes`
/// hashes. Escapes do not apply inside a raw string, and a quote followed by
/// too few hashes is body text.
fn raw_string_scan(line: &str, start: usize, hashes: usize) -> LiteralScan {
    let bytes = line.as_bytes();
    let mut search = start;
    while search < bytes.len() {
        if raw_string_closes_at(bytes, search, hashes) {
            return LiteralScan::Closed(search + 1 + hashes);
        }
        search += 1;
    }
    LiteralScan::Open
}

fn raw_string_closes_at(bytes: &[u8], quote_index: usize, hashes: usize) -> bool {
    if bytes.get(quote_index) != Some(&b'"') {
        return false;
    }
    let close_end = quote_index + 1 + hashes;
    close_end <= bytes.len()
        && bytes[quote_index + 1..close_end]
            .iter()
            .all(|byte| *byte == b'#')
}

/// The end of the character literal starting at `start`, or `None` when that
/// quote opens a lifetime instead.
///
/// A lifetime and a character literal both begin with `'`. A literal holds
/// either an escape sequence or exactly one character before its closing quote,
/// so a quote whose text does not close that way is a lifetime and stays in the
/// code as an ordinary character.
fn char_literal_end(line: &str, start: usize) -> Option<usize> {
    let rest = line.get(start..)?.strip_prefix('\'')?;
    if let Some(escape) = rest.strip_prefix('\\') {
        // The escape selector distinguishes `\n` from `\x41` and `\u{1f600}`,
        // whose bodies vary in length, so the quote is found by searching past
        // the selector rather than by counting characters.
        let selector = escape.chars().next()?;
        let after_selector = start + 2 + selector.len_utf8();
        let close = line.get(after_selector..)?.find('\'')?;
        return Some(after_selector + close + 1);
    }
    let mut characters = rest.chars();
    let value = characters.next()?;
    (characters.next() == Some('\'')).then_some(start + 1 + value.len_utf8() + 1)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// `#[cfg(test)] mod tests;` ends at its semicolon, so everything below it
    /// is still production source. A scan that instead treats the declaration
    /// as the start of a block blanks the rest of the file.
    #[test]
    fn semicolon_form_cfg_test_module_does_not_swallow_the_rest_of_the_file() {
        let source = "\
fn before() {
    let a = one().expect(\"before\");
}

#[cfg(test)]
mod tests;

fn after() {
    let b = two().expect(\"after\");
    unreachable!(\"tail\");
}
";
        let sites = totality_debt_sites(source);
        assert_eq!(
            sites,
            vec![(1, ".expect("), (8, ".expect("), (9, "unreachable!(")],
            "production lines on both sides of a semicolon-form cfg(test) module must be counted"
        );
    }

    /// The brace form hides its whole block, including nested braces, and
    /// resumes counting after the closing brace.
    #[test]
    fn brace_form_cfg_test_module_hides_only_its_own_block() {
        let source = "\
fn before() {
    let a = one().expect(\"before\");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fixture() {
        let inner = || {
            panic!(\"inside a nested block\");
        };
        two().expect(\"inside tests\");
    }
}

fn after() {
    todo!(\"tail\");
}
";
        let sites = totality_debt_sites(source);
        assert_eq!(
            sites,
            vec![(1, ".expect("), (18, "todo!(")],
            "only the cfg(test) block itself may be dropped"
        );
    }

    /// The two forms in one file, which is what the five back-end crates
    /// actually contain.
    #[test]
    fn both_cfg_test_forms_in_one_file_are_handled() {
        let source = "\
#[cfg(test)]
mod tests;

fn middle() {
    panic!(\"middle\");
}

#[cfg(test)]
mod inline {
    fn hidden() {
        panic!(\"hidden\");
    }
}

fn tail() {
    panic!(\"tail\");
}
";
        assert_eq!(
            totality_debt_sites(source),
            vec![(4, "panic!("), (15, "panic!(")]
        );
    }

    /// A `#[cfg(test)]` function whose signature wraps across lines still hides
    /// its body.
    #[test]
    fn multi_line_cfg_test_signature_hides_its_body() {
        let source = "\
impl Thing {
    #[cfg(test)]
    pub(super) fn owners(
        &self,
    ) -> impl Iterator<Item = Owner> + '_ {
        self.owners.iter().copied().expect(\"hidden\")
    }

    pub(crate) fn shipped(&self) -> u32 {
        self.value.expect(\"shipped\")
    }
}
";
        assert_eq!(totality_debt_sites(source), vec![(9, ".expect(")]);
    }

    /// `#[cfg(test)]` on a struct field or a struct-literal entry covers one
    /// line only.
    #[test]
    fn cfg_test_field_entry_hides_one_line() {
        let source = "\
fn build() -> Bundle {
    Bundle {
        #[cfg(test)]
        statements,
        locals: locals.expect(\"shipped\"),
    }
}
";
        assert_eq!(totality_debt_sites(source), vec![(4, ".expect(")]);
    }

    /// `#[cfg(test)] use ...;` and `#[cfg(test)] let ...;` are single statements.
    #[test]
    fn cfg_test_statement_forms_hide_one_line() {
        let source = "\
#[cfg(test)]
use helpers::fixture;

fn run() {
    #[cfg(test)]
    let ordered = order().expect(\"hidden\");
    let shipped = ship().expect(\"shipped\");
}
";
        assert_eq!(totality_debt_sites(source), vec![(6, ".expect(")]);
    }

    /// A semicolon inside an array length does not end the attributed item.
    #[test]
    fn bracketed_semicolon_does_not_end_a_cfg_test_item() {
        let source = "\
#[cfg(test)]
fn fixture(rows: [u8; 4]) -> u8 {
    rows.first().copied().expect(\"hidden\")
}

fn shipped() -> u8 {
    compute().expect(\"shipped\")
}
";
        assert_eq!(totality_debt_sites(source), vec![(6, ".expect(")]);
    }

    /// The forms are counted as call sites, not as text: a form named inside a
    /// string literal, a raw string or a comment is not a call site, and one
    /// line may carry two real ones.
    #[test]
    fn literals_and_comments_are_not_call_sites() {
        let source = "\
fn scan() {
    let needles = [\"panic!(\", \".expect(\"];
    let snippet = r#\"fn f() { todo!(\"x\"); }\"#;
    // unreachable!(\"in a comment\")
    let real = a.expect(\"one\") + b.expect(\"two\"); // .expect( in a trailing comment
}
";
        assert_eq!(
            totality_debt_sites(source),
            vec![(4, ".expect("), (4, ".expect(")]
        );
    }

    /// `.unwrap()` is counted on its own, and `.unwrap_or(` is not `.unwrap(`.
    #[test]
    fn silent_form_counts_unwrap_but_not_unwrap_or() {
        let source = "\
fn run() {
    let a = one().unwrap();
    let b = two().unwrap_or(0);
    let c = three().unwrap_or_else(|| 0);
    let d = four().unwrap() + five().unwrap();
}
";
        assert_eq!(silent_totality_debt_sites(source), vec![1, 4, 4]);
    }

    #[test]
    fn cfg_test_module_names_finds_only_the_semicolon_form() {
        let source = "\
#[cfg(test)]
mod liveness;
#[cfg(test)]
pub(crate) mod corpus;
#[cfg(test)]
mod inline_tests {
    fn hidden() {}
}
mod shipped;
";
        assert_eq!(cfg_test_module_names(source), vec!["liveness", "corpus"]);
        assert_eq!(
            declared_module_names(source),
            vec!["liveness", "corpus", "shipped"]
        );
    }

    /// A commented-out declaration is not a declaration. Reading one as a
    /// declaration pulls the named file into the test-only closure and deletes
    /// its obligations from the count, so the module reader works on stripped
    /// code like everything else. A `mod` line inside a multi-line block
    /// comment is the form that reads as a declaration on its own.
    #[test]
    fn commented_out_module_declarations_are_not_declarations() {
        let source = "\
#[cfg(test)]
mod real;
// mod line_commented;
/*
mod block_commented;
*/
mod shipped;
";
        assert_eq!(cfg_test_module_names(source), vec!["real"]);
        assert_eq!(declared_module_names(source), vec!["real", "shipped"]);
    }

    /// A well-formed file ends outside every attributed item; a file cut off
    /// inside one is reported, because everything below the cut was dropped.
    #[test]
    fn an_unterminated_cfg_test_item_is_reported() {
        assert!(scan_tracks_file_to_its_end(
            "#[cfg(test)]\nmod tests {\n    fn t() {}\n}\nfn shipped() {}\n"
        ));
        assert!(scan_tracks_file_to_its_end(
            "#[cfg(test)]\nmod tests;\nfn a() {}\n"
        ));
        assert!(
            !scan_tracks_file_to_its_end("#[cfg(test)]\nmod tests {\n    fn t() {}\n"),
            "a file ending inside a cfg(test) block is not tracked to the end"
        );
        assert!(
            !scan_tracks_file_to_its_end("fn a() {}\n#[cfg(test)]\n"),
            "a file ending on a dangling attribute is not tracked to the end"
        );
        assert!(
            !scan_tracks_file_to_its_end("fn a() {}\n/* still open\nfn b() {}\n"),
            "a file ending inside a block comment is not tracked to the end"
        );
    }

    /// An attribute stack between `#[cfg(test)]` and the item does not break
    /// the association.
    #[test]
    fn stacked_attributes_still_associate_with_the_item() {
        let source = "\
#[cfg(test)]
#[allow(dead_code)]
mod tests {
    fn hidden() {
        panic!(\"hidden\");
    }
}

fn shipped() {
    panic!(\"shipped\");
}
";
        assert_eq!(totality_debt_sites(source), vec![(9, "panic!(")]);
    }

    /// A block comment spanning lines is comment text on every one of them, and
    /// the braces it contains are not code. A scan that reads them as code
    /// leaves a `#[cfg(test)]` body early and drops the production lines that
    /// follow.
    #[test]
    fn a_multi_line_block_comment_inside_a_cfg_test_body_does_not_end_it() {
        let source = "\
#[cfg(test)]
mod tests {
    /* an unbalanced brace }
       and a form name .expect( that is only text
    */
    fn hidden() {
        one().expect(\"hidden\");
    }
}

fn shipped() {
    two().expect(\"shipped\");
}
";
        assert_eq!(totality_debt_sites(source), vec![(11, ".expect(")]);
    }

    /// The same corruption in the other direction: an unbalanced `{` in a block
    /// comment must not extend a `#[cfg(test)]` body past its real end.
    #[test]
    fn an_unbalanced_brace_in_a_block_comment_does_not_extend_a_cfg_test_body() {
        let source = "\
#[cfg(test)]
mod tests {
    /* an unbalanced opening brace { */
    fn hidden() {
        one().expect(\"hidden\");
    }
}

fn shipped() {
    two().expect(\"shipped\");
}
";
        assert_eq!(totality_debt_sites(source), vec![(9, ".expect(")]);
    }

    /// Rust nests block comments, so the inner `*/` closes only the inner one.
    #[test]
    fn nested_block_comments_close_in_order() {
        let source = "\
fn shipped() {
    /* outer /* inner */ still commented .expect( */
    one().expect(\"shipped\");
}
";
        assert_eq!(totality_debt_sites(source), vec![(2, ".expect(")]);
    }

    /// A character literal is not code either, and a brace inside one is not
    /// structure. An unmatched `{` in a literal extends a `#[cfg(test)]` body
    /// past its real end and drops every production line that follows, and a
    /// quote in one opens a string that swallows the rest of its line.
    #[test]
    fn an_opening_brace_in_a_character_literal_does_not_extend_a_cfg_test_body() {
        let source = "\
#[cfg(test)]
mod tests {
    fn opens(c: char) -> bool {
        c == '{'
    }
}

fn shipped(c: char) -> u32 {
    let quoted = c == '\\\"';
    one().expect(\"shipped\")
}
";
        assert_eq!(totality_debt_sites(source), vec![(9, ".expect(")]);
        assert!(
            scan_tracks_file_to_its_end(source),
            "a brace inside a character literal must not leave the scan mid-item"
        );
    }

    /// The same in the other direction: an unmatched `}` in a character literal
    /// must not end a `#[cfg(test)]` body early and pull its remaining lines
    /// into the count.
    #[test]
    fn a_closing_brace_in_a_character_literal_does_not_end_a_cfg_test_body() {
        let source = "\
#[cfg(test)]
mod tests {
    fn closes(c: char) -> bool {
        c == '}'
    }
    fn hidden() {
        two().expect(\"hidden\");
    }
}

fn shipped() {
    one().expect(\"shipped\");
}
";
        assert_eq!(totality_debt_sites(source), vec![(11, ".expect(")]);
    }

    /// The separators and quotes a character literal can hold are not code
    /// either, and the escaped forms close where the escape does.
    #[test]
    fn character_literal_separators_and_escapes_are_not_code() {
        let source = "\
#[cfg(test)]
fn fixture(open: char) -> bool {
    matches!(open, ';' | '(' | ')' | '[' | ']' | '\\'' | '\\u{7d}' | '\"')
}

fn shipped() {
    one().expect(\"shipped\");
}
";
        assert_eq!(totality_debt_sites(source), vec![(6, ".expect(")]);
    }

    /// A lifetime also begins with `'`, and consuming it as a literal would eat
    /// the code that follows.
    #[test]
    fn lifetimes_are_not_character_literals() {
        let source = "\
fn shipped<'dae>(view: &'dae Dae, tag: char) -> u32 {
    let brand: PhantomData<&'dae mut &'dae ()> = PhantomData;
    if tag == '\\u{7d}' { view.value.expect(\"first\") } else { view.other.expect(\"second\") }
}
";
        assert_eq!(
            totality_debt_sites(source),
            vec![(2, ".expect("), (2, ".expect(")]
        );
    }

    /// A string literal may span lines, and the braces in its body are not
    /// structure on any of them. This is the counterexample the second review
    /// found: a `#[cfg(test)]` body holding a multi-line string with an
    /// unbalanced `{` runs on past its real closing brace, so the shipped
    /// `.expect(` below it is dropped from the count.
    ///
    /// The trailing string closes the spurious depth again, so the
    /// end-of-file tracking assertion still passes. Nothing at all reports the
    /// loss, which is why the string state has to be carried across lines
    /// rather than merely detected at the end.
    #[test]
    fn a_multi_line_string_holding_an_opening_brace_does_not_extend_a_cfg_test_body() {
        let source = "\
#[cfg(test)]
mod tests {
    const OPENS: &str = \"start
an unbalanced opening brace {
end\";
    fn hidden() {
        one().expect(\"hidden\");
    }
}

fn shipped() {
    two().expect(\"shipped\");
}

const CLOSES: &str = \"start
an unbalanced closing brace }
end\";
";
        assert_eq!(
            totality_debt_sites(source),
            vec![(11, ".expect(")],
            "a brace inside a multi-line string must not extend a cfg(test) body over shipped code"
        );
        assert!(
            scan_tracks_file_to_its_end(source),
            "the file is well formed, so the tracking assertion cannot be what catches this"
        );
    }

    /// The same corruption in the other direction: an unbalanced `}` inside a
    /// multi-line string ends a `#[cfg(test)]` body early and pulls its test
    /// lines into the count.
    #[test]
    fn a_multi_line_string_holding_a_closing_brace_does_not_end_a_cfg_test_body() {
        let source = "\
#[cfg(test)]
mod tests {
    const CLOSES: &str = \"start
an unbalanced closing brace }
end\";
    fn hidden() {
        one().expect(\"hidden\");
    }
}

fn shipped() {
    two().expect(\"shipped\");
}
";
        assert_eq!(
            totality_debt_sites(source),
            vec![(11, ".expect(")],
            "a brace inside a multi-line string must not end a cfg(test) body early"
        );
    }

    /// A raw string carries its hash count across lines: only a quote followed
    /// by that many hashes closes it, and the braces before that are body text.
    #[test]
    fn a_multi_line_raw_string_carries_its_hash_count_across_lines() {
        let source = "\
#[cfg(test)]
mod tests {
    const TEMPLATE: &str = r##\"fn f() {
a lone \"# does not close a two-hash raw string
an unbalanced opening brace {
\"##;
    fn hidden() {
        one().expect(\"hidden\");
    }
}

fn shipped() {
    two().expect(\"shipped\");
}
";
        assert_eq!(totality_debt_sites(source), vec![(12, ".expect(")]);
        assert!(scan_tracks_file_to_its_end(source));
    }

    /// Byte and C string literals span lines under the same rules as their
    /// plain forms, in both the cooked and the raw spelling.
    #[test]
    fn byte_and_c_string_literals_span_lines_like_their_plain_forms() {
        let source = "\
#[cfg(test)]
mod tests {
    const COOKED_BYTES: &[u8] = b\"opens
an unbalanced opening brace {
end\";
    const RAW_C: &core::ffi::CStr = cr#\"opens
another unbalanced opening brace {
\"#;
}

fn shipped() {
    one().expect(\"shipped\");
}
";
        assert_eq!(totality_debt_sites(source), vec![(11, ".expect(")]);
        assert!(scan_tracks_file_to_its_end(source));
    }

    /// A multi-line string that quotes Rust source holds attribute text, and an
    /// attribute read out of a string opens an item that never closes.
    #[test]
    fn an_attribute_inside_a_multi_line_string_is_not_an_attribute() {
        let source = "\
const SNIPPET: &str = \"
#[cfg(test)]
mod tests {
\";

fn shipped() {
    one().expect(\"shipped\");
}
";
        assert_eq!(totality_debt_sites(source), vec![(6, ".expect(")]);
        assert!(scan_tracks_file_to_its_end(source));
    }

    /// A file that ends inside a string literal lost track of where that string
    /// ended, exactly as one ending inside a block comment did.
    #[test]
    fn a_file_ending_inside_a_string_literal_is_reported() {
        assert!(
            !scan_tracks_file_to_its_end("fn a() {}\nconst T: &str = \"still open\nfn b() {}\n"),
            "a file ending inside a cooked string is not tracked to the end"
        );
        assert!(
            !scan_tracks_file_to_its_end("fn a() {}\nconst T: &str = r#\"still open\nfn b() {}\n"),
            "a file ending inside a raw string is not tracked to the end"
        );
        assert!(
            scan_tracks_file_to_its_end("const T: &str = \"opens\nand closes\";\nfn a() {}\n"),
            "a string that closes on a later line leaves the scan in code"
        );
    }

    /// A `\\` at the end of a line inside a cooked string escapes the newline
    /// itself, and line-oriented iteration has already removed that newline. The
    /// escape is therefore spent before the next line begins, and carrying it
    /// across would make the first character of the next line escaped: a
    /// closing quote there would be read as body text and the string would
    /// swallow the rest of the file.
    #[test]
    fn a_line_continuation_escape_does_not_carry_into_the_next_line() {
        let source = "\
#[cfg(test)]
mod tests {
    const T: &str = \"opens {\\
\";
    fn hidden() {
        one().expect(\"hidden\");
    }
}

fn shipped() {
    two().expect(\"shipped\");
}
";
        assert_eq!(totality_debt_sites(source), vec![(10, ".expect(")]);
        assert!(
            scan_tracks_file_to_its_end(source),
            "the quote opening the second line closes the string rather than being escaped"
        );
    }

    /// An attribute and the item it covers may share a line. rustfmt splits the
    /// joined form, so it does not reach the tree through CI, but the scanner
    /// must not read the line below the attribute as the attributed item: that
    /// hides a shipped line and lowers the count.
    #[test]
    fn a_same_line_cfg_test_attribute_covers_only_its_own_item() {
        let statement = "\
#[cfg(test)] use helpers::fixture;
fn shipped() {
    one().expect(\"shipped\");
}
";
        assert_eq!(totality_debt_sites(statement), vec![(2, ".expect(")]);

        let block = "\
#[cfg(test)] mod tests { fn hidden() { panic!(\"hidden\"); } }
fn shipped() {
    one().expect(\"shipped\");
}
";
        assert_eq!(totality_debt_sites(block), vec![(2, ".expect(")]);

        let stacked = "\
#[cfg(test)] #[allow(dead_code)] use helpers::fixture;
fn shipped() {
    one().expect(\"shipped\");
}
";
        assert_eq!(totality_debt_sites(stacked), vec![(2, ".expect(")]);

        let opening = "\
#[cfg(test)] fn fixture() {
    one().expect(\"hidden\");
}

fn shipped() {
    two().expect(\"shipped\");
}
";
        assert_eq!(totality_debt_sites(opening), vec![(5, ".expect(")]);
    }

    /// The module reader sees the joined form as one declaration too, so a
    /// test-only module written that way still leaves the count.
    #[test]
    fn a_same_line_cfg_test_module_declaration_is_read_as_one() {
        let source = "\
#[cfg(test)] mod corpus;
#[cfg(test)]
mod liveness;
mod shipped;
";
        assert_eq!(cfg_test_module_names(source), vec!["corpus", "liveness"]);
        assert_eq!(
            declared_module_names(source),
            vec!["liveness", "shipped"],
            "the joined form is not a bare `mod name;` line and is read only as an attributed one"
        );
    }
}
