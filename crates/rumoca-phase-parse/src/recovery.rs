use indexmap::IndexMap;
use rumoca_ir_ast::{ClassDef, Name, StoredDefinition};
use rumoca_ir_core::{ClassType, Location, Token};
use std::sync::Arc;

#[derive(Clone, Copy, PartialEq, Eq)]
enum LexTokenKind {
    Ident,
    Dot,
    Semicolon,
    Equals,
}

#[derive(Clone)]
struct LexToken {
    kind: LexTokenKind,
    token: Token,
}

struct LexedSource {
    tokens: Vec<LexToken>,
    eof: Location,
}

struct Lexer<'a> {
    source: &'a str,
    file_name: &'a str,
    offset: usize,
    line: u32,
    column: u32,
}

impl<'a> Lexer<'a> {
    fn new(source: &'a str, file_name: &'a str) -> Self {
        Self {
            source,
            file_name,
            offset: 0,
            line: 1,
            column: 1,
        }
    }

    fn lex(mut self) -> LexedSource {
        let mut tokens = Vec::new();
        while let Some(ch) = self.peek() {
            match ch {
                '/' if self.peek_next() == Some('/') => self.skip_line_comment(),
                '/' if self.peek_next() == Some('*') => self.skip_block_comment(),
                '"' => self.skip_string_literal(),
                '\'' => tokens.push(self.consume_quoted_identifier()),
                '.' => tokens.push(self.consume_symbol(LexTokenKind::Dot)),
                ';' => tokens.push(self.consume_symbol(LexTokenKind::Semicolon)),
                '=' => tokens.push(self.consume_symbol(LexTokenKind::Equals)),
                _ if ch.is_whitespace() => {
                    let _ = self.bump();
                }
                _ if is_ident_start(ch) => tokens.push(self.consume_identifier()),
                _ => {
                    let _ = self.bump();
                }
            }
        }

        let eof = Location {
            start_line: self.line,
            start_column: self.column,
            end_line: self.line,
            end_column: self.column,
            start: self.offset as u32,
            end: self.offset as u32,
            file_name: self.file_name.to_string(),
        };
        LexedSource { tokens, eof }
    }

    fn peek(&self) -> Option<char> {
        self.source[self.offset..].chars().next()
    }

    fn peek_next(&self) -> Option<char> {
        let mut chars = self.source[self.offset..].chars();
        let _ = chars.next()?;
        chars.next()
    }

    fn bump(&mut self) -> Option<(usize, char, u32, u32)> {
        let ch = self.peek()?;
        let start = self.offset;
        let line = self.line;
        let column = self.column;
        self.offset += ch.len_utf8();
        if ch == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        Some((start, ch, line, column))
    }

    fn skip_line_comment(&mut self) {
        while let Some((_, ch, _, _)) = self.bump() {
            if ch == '\n' {
                break;
            }
        }
    }

    fn skip_block_comment(&mut self) {
        let _ = self.bump();
        let _ = self.bump();
        while let Some((_, ch, _, _)) = self.bump() {
            if ch == '*' && self.peek() == Some('/') {
                let _ = self.bump();
                break;
            }
        }
    }

    fn skip_string_literal(&mut self) {
        let _ = self.bump();
        let mut escaped = false;
        while let Some((_, ch, _, _)) = self.bump() {
            if escaped {
                escaped = false;
                continue;
            }
            if ch == '\\' {
                escaped = true;
                continue;
            }
            if ch == '"' {
                break;
            }
        }
    }

    fn consume_identifier(&mut self) -> LexToken {
        self.consume_while(LexTokenKind::Ident, is_ident_continue)
    }

    fn consume_quoted_identifier(&mut self) -> LexToken {
        let (start, _, start_line, start_column) = self.bump().expect("quoted ident start");
        while let Some((_, ch, _, _)) = self.bump() {
            if ch == '\'' {
                break;
            }
        }
        self.lex_token(LexTokenKind::Ident, start, start_line, start_column)
    }

    fn consume_symbol(&mut self, kind: LexTokenKind) -> LexToken {
        let (start, _, start_line, start_column) = self.bump().expect("symbol");
        self.lex_token(kind, start, start_line, start_column)
    }

    fn consume_while(&mut self, kind: LexTokenKind, predicate: impl Fn(char) -> bool) -> LexToken {
        let (start, _, start_line, start_column) = self.bump().expect("identifier start");
        while let Some(ch) = self.peek() {
            if !predicate(ch) {
                break;
            }
            let _ = self.bump();
        }
        self.lex_token(kind, start, start_line, start_column)
    }

    fn lex_token(
        &self,
        kind: LexTokenKind,
        start: usize,
        start_line: u32,
        start_column: u32,
    ) -> LexToken {
        let text = Arc::<str>::from(&self.source[start..self.offset]);
        let location = Location {
            start_line,
            start_column,
            end_line: self.line,
            end_column: self.column,
            start: start as u32,
            end: self.offset as u32,
            file_name: self.file_name.to_string(),
        };
        LexToken {
            kind,
            token: Token {
                text,
                location,
                token_number: 0,
                token_type: 0,
            },
        }
    }
}

fn is_ident_start(ch: char) -> bool {
    ch == '_' || ch.is_alphabetic()
}

fn is_ident_continue(ch: char) -> bool {
    ch == '_' || ch.is_alphanumeric()
}

fn token_text_eq(token: &LexToken, expected: &str) -> bool {
    token.kind == LexTokenKind::Ident && token.token.text.eq_ignore_ascii_case(expected)
}

fn is_control_end_name(token: &LexToken) -> bool {
    matches!(
        token.token.text.as_ref().to_ascii_lowercase().as_str(),
        "if" | "for" | "while" | "when"
    )
}

#[derive(Clone)]
struct RecoveredClassHeader {
    start: Location,
    name: Token,
    class_type: ClassType,
    class_type_token: Token,
    encapsulated: bool,
    partial: bool,
    expandable: bool,
    operator_record: bool,
    pure: bool,
}

struct RecoveredClassBuilder {
    header: RecoveredClassHeader,
    classes: IndexMap<String, ClassDef>,
}

impl RecoveredClassBuilder {
    fn finish(self, end: Location, end_name_token: Option<Token>) -> ClassDef {
        ClassDef {
            name: self.header.name,
            class_type: self.header.class_type,
            class_type_token: self.header.class_type_token,
            encapsulated: self.header.encapsulated,
            partial: self.header.partial,
            expandable: self.header.expandable,
            operator_record: self.header.operator_record,
            pure: self.header.pure,
            classes: self.classes,
            end_name_token,
            location: combine_locations(&self.header.start, &end),
            ..ClassDef::default()
        }
    }
}

struct RecoveryParser {
    tokens: Vec<LexToken>,
    position: usize,
    eof: Location,
    stack: Vec<RecoveredClassBuilder>,
    definition: StoredDefinition,
}

impl RecoveryParser {
    fn new(source: &str, file_name: &str) -> Self {
        let lexed = Lexer::new(source, file_name).lex();
        Self {
            tokens: lexed.tokens,
            position: 0,
            eof: lexed.eof,
            stack: Vec::new(),
            definition: StoredDefinition::default(),
        }
    }

    fn parse(mut self) -> StoredDefinition {
        while self.position < self.tokens.len() {
            if self.try_parse_within() || self.try_close_class() || self.try_open_class() {
                continue;
            }
            self.position += 1;
        }
        while let Some(builder) = self.stack.pop() {
            self.insert_completed_class(builder.finish(self.eof.clone(), None));
        }
        self.definition
    }

    fn try_parse_within(&mut self) -> bool {
        if !self.stack.is_empty() || self.definition.within.is_some() {
            return false;
        }
        if !self
            .tokens
            .get(self.position)
            .is_some_and(|token| token_text_eq(token, "within"))
        {
            return false;
        }
        let Some((name, next)) = parse_dotted_name(&self.tokens, self.position + 1) else {
            return false;
        };
        self.definition.within = Some(name);
        self.position = next;
        true
    }

    fn try_close_class(&mut self) -> bool {
        if self.stack.is_empty()
            || !self
                .tokens
                .get(self.position)
                .is_some_and(|token| token_text_eq(token, "end"))
        {
            return false;
        }

        let end_index = self.position;
        let Some(candidate) = self.tokens.get(end_index + 1) else {
            return false;
        };
        if candidate.kind != LexTokenKind::Ident || is_control_end_name(candidate) {
            return false;
        }

        let end_name_token = Some(candidate.token.clone());
        self.position = end_index + 2;
        let end_location = if let Some(token) = self.tokens.get(self.position) {
            if token.kind == LexTokenKind::Semicolon {
                self.position += 1;
                token.token.location.clone()
            } else {
                candidate.token.location.clone()
            }
        } else {
            candidate.token.location.clone()
        };

        let builder = self.stack.pop().expect("stack checked");
        self.insert_completed_class(builder.finish(end_location, end_name_token));
        true
    }

    fn try_open_class(&mut self) -> bool {
        let Some((header, next_index)) = parse_class_header(&self.tokens, self.position) else {
            return false;
        };

        if let Some(short_end) = find_short_class_terminator(&self.tokens, next_index) {
            let builder = RecoveredClassBuilder {
                header,
                classes: IndexMap::new(),
            };
            let end_location = self.tokens[short_end].token.location.clone();
            self.insert_completed_class(builder.finish(end_location, None));
            self.position = short_end + 1;
            return true;
        }

        self.stack.push(RecoveredClassBuilder {
            header,
            classes: IndexMap::new(),
        });
        self.position = next_index;
        true
    }

    fn insert_completed_class(&mut self, class: ClassDef) {
        if let Some(parent) = self.stack.last_mut() {
            parent.classes.insert(class.name.text.to_string(), class);
        } else {
            self.definition
                .classes
                .insert(class.name.text.to_string(), class);
        }
    }
}

fn parse_dotted_name(tokens: &[LexToken], start: usize) -> Option<(Name, usize)> {
    let mut idx = start;
    let mut parts = Vec::new();
    while let Some(token) = tokens.get(idx) {
        if token.kind != LexTokenKind::Ident {
            break;
        }
        parts.push(token.token.clone());
        idx += 1;
        if !tokens
            .get(idx)
            .is_some_and(|candidate| candidate.kind == LexTokenKind::Dot)
        {
            break;
        }
        idx += 1;
    }
    if parts.is_empty()
        || !tokens
            .get(idx)
            .is_some_and(|candidate| candidate.kind == LexTokenKind::Semicolon)
    {
        return None;
    }
    Some((
        Name {
            name: parts,
            def_id: None,
        },
        idx + 1,
    ))
}

fn parse_class_header(tokens: &[LexToken], start: usize) -> Option<(RecoveredClassHeader, usize)> {
    let start_token = tokens.get(start)?;
    if start_token.kind != LexTokenKind::Ident {
        return None;
    }

    let mut idx = start;
    let mut encapsulated = false;
    let mut partial = false;
    let mut expandable = false;
    let mut pure = true;
    while let Some(token) = tokens.get(idx) {
        if !matches!(token.kind, LexTokenKind::Ident) {
            break;
        }
        match token.token.text.as_ref().to_ascii_lowercase().as_str() {
            "encapsulated" => encapsulated = true,
            "partial" => partial = true,
            "expandable" => expandable = true,
            "pure" => pure = true,
            "impure" => pure = false,
            _ => break,
        }
        idx += 1;
    }

    let (class_type, class_type_token, operator_record, idx_after_keyword) =
        parse_class_keyword(tokens, idx)?;
    let name_token = tokens.get(idx_after_keyword)?;
    if name_token.kind != LexTokenKind::Ident {
        return None;
    }

    let header = RecoveredClassHeader {
        start: start_token.token.location.clone(),
        name: name_token.token.clone(),
        class_type,
        class_type_token,
        encapsulated,
        partial,
        expandable,
        operator_record,
        pure,
    };
    Some((header, idx_after_keyword + 1))
}

fn parse_class_keyword(tokens: &[LexToken], idx: usize) -> Option<(ClassType, Token, bool, usize)> {
    let token = tokens.get(idx)?;
    if token.kind != LexTokenKind::Ident {
        return None;
    }
    let lower = token.token.text.as_ref().to_ascii_lowercase();
    match lower.as_str() {
        "model" => Some((ClassType::Model, token.token.clone(), false, idx + 1)),
        "class" => Some((ClassType::Class, token.token.clone(), false, idx + 1)),
        "block" => Some((ClassType::Block, token.token.clone(), false, idx + 1)),
        "connector" => Some((ClassType::Connector, token.token.clone(), false, idx + 1)),
        "record" => Some((ClassType::Record, token.token.clone(), false, idx + 1)),
        "type" => Some((ClassType::Type, token.token.clone(), false, idx + 1)),
        "package" => Some((ClassType::Package, token.token.clone(), false, idx + 1)),
        "function" => Some((ClassType::Function, token.token.clone(), false, idx + 1)),
        "operator" => {
            if tokens
                .get(idx + 1)
                .is_some_and(|next| token_text_eq(next, "record"))
            {
                Some((
                    ClassType::Record,
                    tokens[idx + 1].token.clone(),
                    true,
                    idx + 2,
                ))
            } else {
                Some((ClassType::Operator, token.token.clone(), false, idx + 1))
            }
        }
        _ => None,
    }
}

fn find_short_class_terminator(tokens: &[LexToken], start: usize) -> Option<usize> {
    let mut idx = start;
    let mut saw_equals = false;
    while let Some(token) = tokens.get(idx) {
        match token.kind {
            LexTokenKind::Equals => saw_equals = true,
            LexTokenKind::Semicolon => return saw_equals.then_some(idx),
            _ => {
                if token_text_eq(token, "end") || parse_class_header(tokens, idx).is_some() {
                    return None;
                }
            }
        }
        idx += 1;
    }
    None
}

fn combine_locations(start: &Location, end: &Location) -> Location {
    Location {
        start_line: start.start_line,
        start_column: start.start_column,
        end_line: end.end_line,
        end_column: end.end_column,
        start: start.start,
        end: end.end,
        file_name: start.file_name.clone(),
    }
}

pub fn parse_to_recovered_ast(source: &str, file_name: &str) -> StoredDefinition {
    RecoveryParser::new(source, file_name).parse()
}

#[cfg(test)]
mod tests {
    use super::parse_to_recovered_ast;
    use rumoca_ir_core::ClassType;

    #[test]
    fn recovers_within_and_top_level_model_after_syntax_error() {
        let source = r#"
            within P;
            model Helper
              Real x
            equation
              der(x) = 1;
            end Helper;
        "#;
        let recovered = parse_to_recovered_ast(source, "Helper.mo");
        assert_eq!(recovered.within.as_ref().expect("within").to_string(), "P");
        let helper = recovered.classes.get("Helper").expect("helper class");
        assert_eq!(helper.class_type, ClassType::Model);
    }

    #[test]
    fn recovers_nested_classes_from_package_body() {
        let source = r#"
            package P
              model A
                Real x;
              end A;
              model B
                Real y
              equation
                y = 1;
              end B;
            end P;
        "#;
        let recovered = parse_to_recovered_ast(source, "package.mo");
        let package = recovered.classes.get("P").expect("package");
        assert_eq!(package.class_type, ClassType::Package);
        assert!(package.classes.contains_key("A"));
        assert!(package.classes.contains_key("B"));
    }

    #[test]
    fn recovers_short_type_definitions() {
        let source = "type T = Real;";
        let recovered = parse_to_recovered_ast(source, "T.mo");
        let class = recovered.classes.get("T").expect("type alias");
        assert_eq!(class.class_type, ClassType::Type);
    }
}
