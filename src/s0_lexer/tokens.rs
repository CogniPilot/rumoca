use logos::{Lexer, Logos, Skip};
use std::fmt; // to implement the Display trait later
use std::num::ParseIntError;

#[derive(Default, Debug, Clone, PartialEq)]
pub enum LexicalError {
    InvalidInteger(ParseIntError),
    #[default]
    InvalidToken,
}

impl From<ParseIntError> for LexicalError {
    fn from(err: ParseIntError) -> Self {
        LexicalError::InvalidInteger(err)
    }
}

/// Update the line count and the char index.
fn newline_callback(lex: &mut Lexer<Token>) -> Skip {
    lex.extras.0 += 1;
    lex.extras.1 = lex.span().end;
    Skip
}

fn quoted_string_callback(lex: &mut Lexer<Token>) -> String {
    let slice = lex.slice();
    slice[1..slice.len() - 1].to_string()
}

// Modelica 3.7-dev
// 2.3.1 Identifiers
// ============================================================================
// IDENT = NON-DIGIT { DIGIT | NON-DIGIT } | Q-IDENT
// Q-IDENT = "'" { Q-CHAR | S-ESCAPE } "'"
// NON-DIGIT = "_" | letters "a"
//  "z" | letters "A"
//  "Z"
// DIGIT = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
// Q-CHAR = NON-DIGIT | DIGIT | "!" | "#" | "$" | "%" | "&" | "(" | ")"
//    | "*" | "+" | "," | "-" | "." | "/" | ":" | ";" | "<" | ">" | "="
//    | "?" | "@" | "[" | "]" | "^" | "{" | "}" | "|" | "~" | " " | """
// S-ESCAPE = "\'" | "\"" | "\?" | "\\"
//    | "\a" | "\b" | "\f" | "\n" | "\r" | "\t" | "\v"
//
// 2.3.3 Modelica Keywords
// ============================================================================
#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(
    skip r"[ \t\f\r]+",
    skip r"//.*\n?",
    skip r"\/\*([^*]|\*[^\/])+\*\/",
    extras = (usize, usize),
    error = LexicalError)]
pub enum Token {
    #[regex(r"\n", newline_callback)]
    Newline,

    #[token("algorithm")]
    KeywordAlgorithm,
    #[token("and")]
    KeywordAnd,
    #[token("annotation")]
    KeywordAnnotation,
    #[token("block")]
    KeywordBlock,
    #[token("break")]
    KeywordBreak,
    #[token("class")]
    KeywordClass,
    #[token("connect")]
    KeywordConnect,
    #[token("connector")]
    KeywordConnector,
    #[token("constant")]
    KeywordConstant,
    #[token("constrainedby")]
    KeywordConstrainedby,
    #[token("der")]
    KeywordDer,
    #[token("discrete")]
    KeywordDiscrete,
    #[token("each")]
    KeywordEach,
    #[token("else")]
    KeywordElse,
    #[token("elseif")]
    KeywordElseif,
    #[token("elsewhen")]
    KeywordElsewhen,
    #[token("encapsulated")]
    KeywordEncapsulated,
    #[token("end")]
    KeywordEnd,
    #[token("enumeration")]
    KeywordEnumeration,
    #[token("equation")]
    KeywordEquation,
    #[token("expandable")]
    KeywordExpandable,
    #[token("extends")]
    KeywordExtends,
    #[token("external")]
    KeywordExternal,
    //#[token("false")]
    //KeywordFalse,
    #[token("final")]
    KeywordFinal,
    #[token("flow")]
    KeywordFlow,
    #[token("for")]
    KeywordFor,
    #[token("function")]
    KeywordFunction,
    #[token("if")]
    KeywordIf,
    #[token("import")]
    KeywordImport,
    #[token("impure")]
    KeywordImpure,
    #[token("in")]
    KeywordIn,
    #[token("initial")]
    KeywordInitial,
    #[token("inner")]
    KeywordInner,
    #[token("input")]
    KeywordInput,
    #[token("loop")]
    KeywordLoop,
    #[token("model")]
    KeywordModel,
    #[token("not")]
    KeywordNot,
    #[token("operator")]
    KeywordOperator,
    #[token("or")]
    KeywordOr,
    #[token("outer")]
    KeywordOuter,
    #[token("output")]
    KeywordOutput,
    #[token("package")]
    KeywordPackage,
    #[token("parameter")]
    KeywordParameter,
    #[token("partial")]
    KeywordPartial,
    #[token("protected")]
    KeywordProtected,
    #[token("public")]
    KeywordPublic,
    #[token("pure")]
    KeywordPure,
    #[token("record")]
    KeywordRecord,
    #[token("redeclare")]
    KeywordRedeclare,
    #[token("replaceable")]
    KeywordReplaceable,
    #[token("return")]
    KeywordReturn,
    #[token("stream")]
    KeywordStream,
    #[token("then")]
    KeywordThen,
    //#[token("true")]
    //KeywordTrue,
    #[token("type")]
    KeywordType,
    #[token("when")]
    KeywordWhen,
    #[token("while")]
    KeywordWhile,
    #[token("within")]
    KeywordWithin,

    #[regex("[_a-zA-Z][_0-9a-zA-Z]*", |lex| lex.slice().to_string())]
    Identifier(String),

    #[regex("\"[ _0-9a-zA-Z]*\"", quoted_string_callback)]
    String(String),

    #[regex("[1-9][0-9]*", |lex| lex.slice().parse::<i64>().unwrap(), priority=3)]
    UnsignedInteger(i64),

    #[regex(r"(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?", |lex| lex.slice().parse::<f64>().unwrap())]
    UnsignedReal(f64),

    #[token(r"(true|false)?", |lex| lex.slice().parse::<bool>().unwrap())]
    Boolean(bool),

    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("=")]
    Equal,
    #[token(":=")]
    Assign,
    #[token(";")]
    Semicolon,
    #[token(".")]
    Period,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,

    #[token("+")]
    OperatorAdd,
    #[token("-")]
    OperatorSub,
    #[token("*")]
    OperatorMul,
    #[token("/")]
    OperatorDiv,
    #[token("^")]
    OperatorExp,

    #[token(".+")]
    OperatorElemAdd,
    #[token(".-")]
    OperatorElemSub,
    #[token(".*")]
    OperatorElemMul,
    #[token("./")]
    OperatorElemDiv,
    #[token(".^")]
    OperatorElemExp,

    #[token("<")]
    OperatorLessThan,
    #[token("<=")]
    OperatorLessThanOrEqual,
    #[token(">")]
    OperatorGreaterThan,
    #[token(">=")]
    OperatorGreaterThanOrEqual,
    #[token("==")]
    OperatorEqual,
    #[token("<>")]
    OperatorNotEqual,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
