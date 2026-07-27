//! P4 parser allocation work: every parser token must carry the file's
//! `SourceId`, computed once per file, and never an owned path copy.

use rumoca_core::{SourceId, Token};
use rumoca_ir_ast::{ClassDef, StoredDefinition};
use rumoca_phase_parse::{parse_to_ast, parse_to_recovered_ast};

const SOURCE: &str = r#"
package Pkg
  model A
    Real x;
  equation
    x = 1.0;
  end A;

  model B
    Real y;
  equation
    y = 2.0;
  end B;
end Pkg;
"#;

/// Collect every token reachable from a class, including nested classes.
fn collect_class_tokens(class: &ClassDef, tokens: &mut Vec<Token>) {
    tokens.push(class.name.clone());
    tokens.push(class.class_type_token.clone());
    tokens.extend(class.description.iter().cloned());
    for component in class.components.values() {
        tokens.push(component.name_token.clone());
        tokens.extend(component.type_name.name.iter().cloned());
        tokens.extend(component.description.iter().cloned());
    }
    for extend in &class.extends {
        tokens.extend(extend.base_name.name.iter().cloned());
    }
    for nested in class.classes.values() {
        collect_class_tokens(nested, tokens);
    }
}

fn collect_tokens(def: &StoredDefinition) -> Vec<Token> {
    let mut tokens = Vec::new();
    if let Some(within) = &def.within {
        tokens.extend(within.name.iter().cloned());
    }
    for class in def.classes.values() {
        collect_class_tokens(class, &mut tokens);
    }
    tokens
}

fn collect_class_locations(class: &ClassDef, out: &mut Vec<rumoca_core::Location>) {
    out.push(class.location.clone());
    out.push(class.name.location.clone());
    out.push(class.class_type_token.location.clone());
    for component in class.components.values() {
        out.push(component.location.clone());
    }
    for nested in class.classes.values() {
        collect_class_locations(nested, out);
    }
}

#[test]
fn every_token_carries_the_file_source_id() {
    let file_name = "pkg/A.mo";
    let def = parse_to_ast(SOURCE, file_name).expect("source should parse");
    let expected = SourceId::from_source_name(file_name);
    assert_ne!(expected, SourceId::DUMMY);

    let tokens = collect_tokens(&def);
    assert!(tokens.len() > 5, "expected a non-trivial token set");
    for token in &tokens {
        assert_eq!(
            token.location.source, expected,
            "token {:?} carries the wrong source id",
            token.text
        );
    }

    let mut locations = Vec::new();
    for class in def.classes.values() {
        collect_class_locations(class, &mut locations);
    }
    for location in &locations {
        assert_eq!(location.source, expected);
        assert!(location.has_source(), "location {location} has no source");
    }
}

#[test]
fn recovered_ast_source_ids_match_normal_parse() {
    let file_name = "pkg/Recovered.mo";
    let expected = SourceId::from_source_name(file_name);
    let recovered = parse_to_recovered_ast(SOURCE, file_name);
    let tokens = collect_tokens(&recovered);
    assert!(
        !tokens.is_empty(),
        "recovery lexer should still produce tokens"
    );
    for token in &tokens {
        assert_eq!(
            token.location.source, expected,
            "recovered token {:?} carries the wrong source id",
            token.text
        );
    }
}

#[test]
fn recovery_after_syntax_error_still_stamps_the_file_source_id() {
    let file_name = "pkg/Broken.mo";
    let expected = SourceId::from_source_name(file_name);
    let broken = "model Broken\n  Real x\n  Real y;\nend Broken;\n";
    let recovered = parse_to_recovered_ast(broken, file_name);
    let tokens = collect_tokens(&recovered);
    assert!(!tokens.is_empty());
    for token in &tokens {
        assert_eq!(token.location.source, expected);
    }
}

#[test]
fn windows_separator_normalizes_to_same_source_id() {
    assert_eq!(
        SourceId::from_source_name("a\\b.mo"),
        SourceId::from_source_name("a/b.mo")
    );
    let backslash = parse_to_ast(SOURCE, "a\\b.mo").expect("source should parse");
    let forward = parse_to_ast(SOURCE, "a/b.mo").expect("source should parse");
    let backslash_tokens = collect_tokens(&backslash);
    let forward_tokens = collect_tokens(&forward);
    assert_eq!(backslash_tokens.len(), forward_tokens.len());
    for (lhs, rhs) in backslash_tokens.iter().zip(&forward_tokens) {
        assert_eq!(lhs.location.source, rhs.location.source);
    }
}

#[test]
fn distinct_files_parsed_in_sequence_get_distinct_source_ids() {
    // The parser memoizes the source id per file; a stale memo would leak the
    // previous file's identity into the next parse.
    let first = parse_to_ast(SOURCE, "one/A.mo").expect("source should parse");
    let second = parse_to_ast(SOURCE, "two/A.mo").expect("source should parse");
    let first_id = SourceId::from_source_name("one/A.mo");
    let second_id = SourceId::from_source_name("two/A.mo");
    assert_ne!(first_id, second_id);
    for token in collect_tokens(&first) {
        assert_eq!(token.location.source, first_id);
    }
    for token in collect_tokens(&second) {
        assert_eq!(token.location.source, second_id);
    }
}

#[test]
fn path_equal_but_textually_distinct_names_do_not_share_a_source_id() {
    // The per-file memo compares paths with `Path` equality, which normalizes
    // repeated separators away; `SourceId::from_source_name` hashes the raw
    // name bytes and does not. A memo that survives a parse would therefore
    // hand the second file the *first* file's identity even though the two
    // names hash differently - the aliasing hazard the memo reset guards.
    let plain = "alias/A.mo";
    let doubled = "alias//A.mo";
    assert_eq!(
        std::path::Path::new(plain),
        std::path::Path::new(doubled),
        "the two names must be `Path`-equal for this test to exercise the hazard"
    );
    let plain_id = SourceId::from_source_name(plain);
    let doubled_id = SourceId::from_source_name(doubled);
    assert_ne!(
        plain_id, doubled_id,
        "source identity is derived from the raw name bytes"
    );

    // Both parses run on this thread, in this order, so the second one sees
    // whatever the first one left behind.
    let first = parse_to_ast(SOURCE, plain).expect("source should parse");
    let second = parse_to_ast(SOURCE, doubled).expect("source should parse");
    for token in collect_tokens(&first) {
        assert_eq!(token.location.source, plain_id, "token {:?}", token.text);
    }
    for token in collect_tokens(&second) {
        assert_eq!(
            token.location.source, doubled_id,
            "token {:?} inherited the previous file's source id",
            token.text
        );
    }
}
