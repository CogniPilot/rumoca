//! Property-based tests for the syntax layer.
//!
//! `parse_to_syntax` documents an infallible contract: whatever the input, it
//! returns a best-effort tree plus the recovered parse errors. These properties
//! exercise that contract against inputs no hand-written corpus reaches —
//! arbitrary bytes, mutations of the recovery corpus, and deep nesting — so a
//! panic or a non-terminating path is caught before it reaches the LSP (which
//! parses every keystroke) or the MSL gate.

use proptest::prelude::*;
use std::fs;
use std::path::PathBuf;

const FUZZ_FILE_NAME: &str = "property.mo";

fn corpus_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/broken_modelica")
}

fn corpus_sources() -> Vec<String> {
    let mut entries = fs::read_dir(corpus_dir())
        .expect("read broken Modelica corpus")
        .collect::<Result<Vec<_>, _>>()
        .expect("read corpus entries");
    entries.sort_by_key(std::fs::DirEntry::path);
    let mut sources = entries
        .into_iter()
        .map(|entry| entry.path())
        .filter(|path| path.extension().is_some_and(|ext| ext == "mo"))
        .map(|path| fs::read_to_string(&path).expect("read corpus case"))
        .collect::<Vec<_>>();
    sources.extend(
        WELL_FORMED_SOURCES
            .iter()
            .map(|source| (*source).to_string()),
    );
    sources
}

const WELL_FORMED_SOURCES: &[&str] = &[
    "model M Real x; equation der(x) = -x; end M;",
    "package P model Q parameter Real k = 1.0; Real y(start = 0); equation y = k * time; end Q; end P;",
    "function f input Real u; output Real y; algorithm y := u * u; end f;",
    "model Cond Real x; equation if time > 1 then x = 1; else x = 0; end if; end Cond;",
];

/// Assert the infallible syntax-layer contract for one input.
fn assert_best_effort_tree(source: &str) {
    let syntax = rumoca_phase_parse::parse_to_syntax(source, FUZZ_FILE_NAME);
    assert!(
        syntax.recovered().is_some() || syntax.parsed().is_some(),
        "parse_to_syntax must always yield a best-effort tree"
    );
    // Touching the tree is part of the contract: a recovered tree that cannot be
    // walked is as bad as no tree at all.
    let _ = syntax.best_effort().classes.len();
}

/// Splice, truncate or duplicate a corpus source deterministically from the
/// proptest-supplied indices.
fn mutate(source: &str, mode: u8, a: usize, b: usize) -> String {
    let len = source.len();
    if len == 0 {
        return String::new();
    }
    let cut_a = floor_char_boundary(source, a % (len + 1));
    let cut_b = floor_char_boundary(source, b % (len + 1));
    let (lo, hi) = if cut_a <= cut_b {
        (cut_a, cut_b)
    } else {
        (cut_b, cut_a)
    };
    match mode % 4 {
        0 => source[..lo].to_string(),
        1 => format!("{}{}", &source[..lo], &source[hi..]),
        2 => format!("{}{}{}", &source[..hi], &source[lo..hi], &source[hi..]),
        _ => format!("{}{}", &source[lo..hi], &source[..lo]),
    }
}

fn floor_char_boundary(source: &str, mut index: usize) -> usize {
    while index > 0 && !source.is_char_boundary(index) {
        index -= 1;
    }
    index
}

proptest! {
    #![proptest_config(ProptestConfig { cases: 128, ..ProptestConfig::default() })]

    /// The syntax layer is the LSP's front door: it must never panic, whatever
    /// bytes the editor buffer currently holds.
    #[test]
    fn parse_to_syntax_never_panics_on_arbitrary_bytes(
        bytes in prop::collection::vec(any::<u8>(), 0..4096),
    ) {
        let source = String::from_utf8_lossy(&bytes).into_owned();
        assert_best_effort_tree(&source);
    }

    /// Mutating real Modelica is far more likely to reach deep parser states
    /// than random bytes, which mostly die in the lexer.
    #[test]
    fn parse_to_syntax_never_panics_on_mutated_corpus(
        index in 0usize..64,
        mode in any::<u8>(),
        a in any::<usize>(),
        b in any::<usize>(),
    ) {
        let sources = corpus_sources();
        prop_assume!(!sources.is_empty());
        let source = &sources[index % sources.len()];
        assert_best_effort_tree(&mutate(source, mode, a, b));
    }

    /// Parsing must be a pure function of the source: an LSP that re-parses an
    /// unchanged buffer has to see identical diagnostics.
    #[test]
    fn parse_to_syntax_is_deterministic(
        index in 0usize..64,
        mode in any::<u8>(),
        a in any::<usize>(),
        b in any::<usize>(),
    ) {
        let sources = corpus_sources();
        prop_assume!(!sources.is_empty());
        let source = mutate(&sources[index % sources.len()], mode, a, b);

        let first = rumoca_phase_parse::parse_to_syntax(&source, FUZZ_FILE_NAME);
        let second = rumoca_phase_parse::parse_to_syntax(&source, FUZZ_FILE_NAME);

        prop_assert_eq!(first.parse_errors().len(), second.parse_errors().len());
        prop_assert_eq!(first.parse_error().is_some(), second.parse_error().is_some());
        let first_names = first.best_effort().classes.keys().collect::<Vec<_>>();
        let second_names = second.best_effort().classes.keys().collect::<Vec<_>>();
        prop_assert_eq!(first_names, second_names);
    }
}

/// Deeply nested expressions must terminate (and not blow the stack) rather
/// than recursing without bound. Run outside `proptest!` so the depth ladder is
/// explicit and reproducible.
#[test]
fn parse_to_syntax_terminates_on_deep_nesting() {
    for depth in [1usize, 8, 32, 128, 256] {
        let expr = format!("{}1{}", "(".repeat(depth), ")".repeat(depth));
        let source = format!("model Deep Real x; equation x = {expr}; end Deep;");
        assert_best_effort_tree(&source);

        // Unbalanced nesting is the realistic editor state (the user has typed
        // the openers but not yet the closers).
        let unbalanced = format!(
            "model Deep Real x; equation x = {}1; end Deep;",
            "(".repeat(depth)
        );
        assert_best_effort_tree(&unbalanced);
    }
}

/// Deeply nested *class* structure is the other unbounded-recursion shape.
#[test]
fn parse_to_syntax_terminates_on_deep_class_nesting() {
    for depth in [1usize, 8, 32, 64] {
        let mut source = String::new();
        for level in 0..depth {
            source.push_str(&format!("package P{level} "));
        }
        source.push_str("model M Real x; equation x = 1; end M; ");
        for level in (0..depth).rev() {
            source.push_str(&format!("end P{level}; "));
        }
        assert_best_effort_tree(&source);
    }
}
