//! Enumerated coverage of the alternative join against a non-empty exit set.
//!
//! [`super::liveness`] answers "is this stored value ever read again" with two
//! composition operators, and the store-deletion evidence is only as sound as
//! the choice between them.
//! [`liveness::live_in_concatenation`] composes segments that fall through into
//! one another, so a definite write in an earlier segment hides a read in a
//! later one. [`liveness::live_in_alternatives`] joins segments that are
//! competing successors of one point, where no segment's write may hide
//! another's read, and
//! [`super::liveness::prove_unobserved_loop_local_store`] joins a store's back
//! edges with it for exactly that reason.
//!
//! [`super::liveness_differential_tests`] measures that join against an
//! execution oracle over the corpus grammar, but only with nothing live on
//! exit and only where the first segment is a single composite statement. The
//! other axis is enumerated here: both segments are straight-line statement
//! lists, and every subset of the value names is tried as the exit set. Three
//! clauses are asserted over the whole domain below:
//!
//! * the join keeps every name either segment observes;
//! * the join reports no name neither segment observes; and
//! * the sequence composition of the same segments is a subset of the join,
//!   which is what makes joining the back edges the conservative choice: it can
//!   only report more names live, so it can only refuse more deletions.
//!
//! # Bounded domain
//!
//! Three value names (`a`, `b`, `c`). Each statement is named by the low
//! [`CODE_BITS`] bits of one byte: [`TARGET_BITS`] select the target, where
//! zero means the empty statement, and [`READ_BITS`] select what the
//! right-hand side reads, where zero means it reads nothing. Two segments of
//! exactly [`SEGMENT_LEN`] statements are drawn from that encoding, and the
//! empty statement is what makes a shorter segment reachable. The enumeration
//! is complete over that domain and says nothing about a longer segment, a
//! fourth name, a right-hand side reading two names at once, or a statement
//! form the encoding cannot name.

use super::liveness::{self, LiveSet};
use super::*;
use rumoca_core::{ComponentReference, Literal, OpBinary};

/// Value names the bounded domain is built from.
const NAMES: [&str; 3] = ["a", "b", "c"];

/// Subsets of [`NAMES`] used as exit sets.
const SUBSET_COUNT: usize = 1 << NAMES.len();

/// Bits selecting the target, where zero names the empty statement.
const TARGET_BITS: u32 = 2;

/// Bits selecting the name the right-hand side reads, where zero reads none.
const READ_BITS: u32 = 2;

/// Bits of one statement code.
const CODE_BITS: u32 = TARGET_BITS + READ_BITS;

/// Low bits of a byte that name a statement.
const CODE_MASK: u8 = (1u8 << CODE_BITS) - 1;

/// Field mask shared by the target and the read selector.
const FIELD_MASK: u8 = (1u8 << TARGET_BITS) - 1;

/// Distinct statements the encoding names: the empty statement, plus one
/// assignment per target and read selector.
const STATEMENT_COUNT: usize = 1 + NAMES.len() * (NAMES.len() + 1);

/// Both fields have to be wide enough for every name plus the zero case.
const _: () = assert!(NAMES.len() < 1 << TARGET_BITS);
const _: () = assert!(TARGET_BITS == READ_BITS);

/// Statements per segment. Two is the shortest length that lets a write inside
/// one segment hide a read later in the same segment.
const SEGMENT_LEN: usize = 2;

fn span() -> Span {
    Span::from_offsets(rumoca_core::SourceId::DUMMY, 0, 1)
}

fn component(name: &str) -> ComponentReference {
    ComponentReference::construct(
        false,
        span(),
        vec![rumoca_core::ComponentRefPart {
            ident: name.to_string(),
            span: span(),
            subs: Vec::new(),
            def_id: rumoca_core::DefId::new(1),
        }],
    )
    .expect("bounded-domain component reference has exact identity")
}

/// The name a code field selects, or `None` for the field's zero case.
fn selected(field: u8) -> Option<&'static str> {
    let index = (field & FIELD_MASK).checked_sub(1)?;
    NAMES.get(usize::from(index)).copied()
}

/// An expression reading exactly the name `field` selects and nothing else.
fn reading(field: u8) -> Expression {
    let literal = Expression::Literal {
        value: Literal::Integer(1),
        span: span(),
    };
    let Some(name) = selected(field) else {
        return literal;
    };
    Expression::Binary {
        op: OpBinary::Add,
        lhs: Box::new(literal),
        rhs: Box::new(Expression::VarRef {
            name: Reference::new(name),
            subscripts: Vec::new(),
            span: span(),
        }),
        span: span(),
    }
}

/// The statement `code` names, or the empty statement where its target field
/// is zero or names no value.
fn statement(code: u8) -> rumoca_core::Statement {
    let code = code & CODE_MASK;
    let Some(target) = selected(code) else {
        return rumoca_core::Statement::Empty { span: span() };
    };
    rumoca_core::Statement::Assignment {
        comp: component(target),
        value: reading(code >> TARGET_BITS),
        span: span(),
    }
}

fn segment(picks: &[u8]) -> Vec<rumoca_core::Statement> {
    picks.iter().copied().map(statement).collect()
}

fn exit_set(subset: usize) -> LiveSet {
    NAMES
        .iter()
        .enumerate()
        .filter(|(position, _)| subset & (1 << position) != 0)
        .map(|(_, name)| VarName::new(*name))
        .collect()
}

/// A readable rendering, so a failure names the program it came from.
fn render(picks: &[u8]) -> String {
    picks
        .iter()
        .map(|pick| {
            let code = pick & CODE_MASK;
            match selected(code) {
                None => ";".to_string(),
                Some(target) => match selected(code >> TARGET_BITS) {
                    None => format!("{target} := 1;"),
                    Some(read) => format!("{target} := 1 + {read};"),
                },
            }
        })
        .collect::<Vec<_>>()
        .join(" ")
}

/// The three clauses, for one pair of segments and one exit set.
fn assert_join_is_sound(
    first: &[rumoca_core::Statement],
    second: &[rumoca_core::Statement],
    live_out: &LiveSet,
    program: &str,
) {
    let first_live = liveness::live_in(first, live_out);
    let second_live = liveness::live_in(second, live_out);
    let segments: [&[rumoca_core::Statement]; 2] = [first, second];
    let joined = liveness::live_in_alternatives(&segments, live_out);
    let sequenced = liveness::live_in_concatenation(&segments, live_out);

    for name in first_live.iter().chain(second_live.iter()) {
        assert!(
            joined.contains(name),
            "the join dropped `{}`, which a segment observes: {program}",
            name.as_str()
        );
    }
    for name in &joined {
        assert!(
            first_live.contains(name) || second_live.contains(name),
            "the join reported `{}`, which neither segment observes: {program}",
            name.as_str()
        );
    }
    for name in &sequenced {
        assert!(
            joined.contains(name),
            "sequencing reported `{}`, which the join dropped: {program}",
            name.as_str()
        );
    }
}

/// Every exit set for one pick vector, building the segments once rather than
/// once per exit set.
fn assert_over_every_exit_set(picks: &[u8]) {
    let first = segment(&picks[..SEGMENT_LEN]);
    let second = segment(&picks[SEGMENT_LEN..]);
    let program = format!(
        "{} | {}",
        render(&picks[..SEGMENT_LEN]),
        render(&picks[SEGMENT_LEN..])
    );
    for subset in 0..SUBSET_COUNT {
        assert_join_is_sound(&first, &second, &exit_set(subset), &program);
    }
}

/// Every pick vector over every statement code, against every exit set: the
/// whole bounded domain this module declares, enumerated rather than sampled.
#[test]
fn the_join_is_sound_over_the_whole_bounded_domain() {
    let width = u32::try_from(SEGMENT_LEN * 2).expect("segment width fits a u32");
    let codes = u32::from(CODE_MASK) + 1;
    for encoded in 0..codes.pow(width) {
        let mut picks = [0u8; SEGMENT_LEN * 2];
        let mut rest = encoded;
        for pick in &mut picks {
            *pick = u8::try_from(rest % codes).expect("statement code fits a byte");
            rest /= codes;
        }
        assert_over_every_exit_set(&picks);
    }
}

/// The encoding names exactly the statements the bounded domain declares, so
/// the sweep above covers the domain this module documents.
#[test]
fn the_encoding_names_exactly_the_declared_statements() {
    let distinct = (0..=CODE_MASK)
        .map(|code| render(&[code]))
        .collect::<std::collections::BTreeSet<_>>();
    assert_eq!(distinct.len(), STATEMENT_COUNT);
}
