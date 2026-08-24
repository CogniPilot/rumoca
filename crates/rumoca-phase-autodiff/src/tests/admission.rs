//! The admission table has to be closed and probeable.
//!
//! The table is what the finite-difference battery is generated from, so a
//! construct missing from it is a construct nothing checks and a probe that
//! reduces its result is a probe that could absorb a collapsed rank. These
//! checks pin both properties in the crate that owns the rules, so a rule
//! added here fails here rather than somewhere downstream.

use crate::admission::{
    Family, NAMED_FORMS, SHAPED_FORMS, Verdict, construct_universe, table, unstated_pairs,
};

#[test]
fn every_construct_the_engine_reaches_is_stated_exactly_once() {
    let unstated = unstated_pairs();
    assert!(
        unstated.is_empty(),
        "the admission table must state every construct this engine can reach, at every \
         shape of operands, exactly once. Unstated or doubly stated:\n{}",
        unstated.join("\n")
    );
}

#[test]
fn the_universe_is_read_from_the_engines_own_lists() {
    let universe = construct_universe();
    for name in crate::builtins::REFUSED {
        assert!(
            universe.iter().any(|(held, _)| held == name),
            "`{name}` is refused by the engine but is not in the construct universe"
        );
    }
    for (_, spelling) in crate::engine::BINARY_OPERATORS {
        assert!(
            universe
                .iter()
                .any(|(held, arity)| held == spelling && *arity == 2),
            "the binary operator `{spelling}` is not in the construct universe"
        );
    }
}

/// The hand-written half of the construct universe, pinned.
///
/// The builtins and the operators are read out of the engine's own lists, so a
/// rule added there without a row is reported by the closure check without
/// anyone remembering. The forms below are reached by matching an AST shape
/// and have no list to read: this is the closure's one hand-maintained edge.
///
/// Pinning the contents does not prove the list complete, and nothing here
/// can: proving it would need a list of the AST variants the engine handles,
/// which is the very thing this is. What it does prove is that a form cannot
/// leave the closure quietly. Dropping one from the universe, which would make
/// `unstated_pairs` stop asking for its rows, fails here first, and adding one
/// is an edit a reviewer reads next to the row that pays for it.
#[test]
fn the_forms_the_engine_matches_by_shape_are_pinned() {
    assert_eq!(
        NAMED_FORMS,
        &[
            "^ with a moving exponent",
            ".^ with a moving exponent",
            "a bound local",
            "a bound output",
            "a constant local",
            "a parameter local",
            "a binding reading a later declaration",
            "a binding that moves under a variability that holds still",
            "a vectorized call to a function in scope",
            "an assignment statement",
            "a for statement",
            "an if statement",
            "an assert statement",
            "a while statement",
            "a call statement",
        ],
        "the named half of the construct universe changed; every entry owes the table rows at \
         every shape, and an entry removed here is a construct the closure stops asking about"
    );
    assert_eq!(
        SHAPED_FORMS,
        &[
            ("an array literal", 1),
            ("an if expression", 1),
            ("a call to a function in scope", 1),
        ],
        "the shaped half of the construct universe changed; see above"
    );
}

/// Every entry of the hand-written half really is asked for by the closure,
/// and really is answered by the table.
///
/// A name in the universe with no row is what `unstated_pairs` reports; this
/// states the other direction, that no entry is a name nothing ever exercises.
#[test]
fn every_stated_form_carries_rows() {
    let rows = table();
    for name in NAMED_FORMS {
        assert!(
            rows.iter().any(|row| row.construct == *name),
            "`{name}` is in the construct universe but the table has no row for it"
        );
    }
    for (name, _) in SHAPED_FORMS {
        assert!(
            rows.iter().any(|row| row.construct == *name),
            "`{name}` is in the construct universe but the table has no row for it"
        );
    }
}

#[test]
fn every_family_carries_rows() {
    for family in Family::ALL {
        assert!(
            table().iter().any(|row| row.family == *family),
            "{family:?} has no rows, so the gate that runs it proves nothing"
        );
    }
}

/// A probe that reduced its result could absorb exactly the rank collapse the
/// shape rules exist to prevent, so every output entry has to be a component
/// of the result and nothing else.
#[test]
fn no_probe_reduces_its_result() {
    for row in table() {
        if row.body.is_some() {
            continue;
        }
        for entry in row.carrier.entries {
            assert!(
                entry.starts_with('r')
                    && entry[1..]
                        .chars()
                        .all(|character| character.is_ascii_digit() || " [],".contains(character)),
                "{row} carries its result out through `{entry}`, which is not a plain component \
                 of `r`"
            );
        }
    }
}

#[test]
fn every_probe_function_parses() {
    for (index, row) in table().iter().enumerate() {
        let name = format!("probe_{index}");
        let text = row.function(&name);
        rumoca_phase_parse::parse_to_ast(&text, "Probe.mo").unwrap_or_else(|error| {
            panic!("{row} must generate a parseable probe: {error}\n{text}")
        });
    }
}

#[test]
fn the_table_states_a_refusal_rule_for_every_refused_pair() {
    for row in table() {
        let Verdict::Refused { rule, says } = row.verdict else {
            continue;
        };
        assert!(
            rule.starts_with("JAC-R"),
            "{row} cites `{rule}`, which is not a refusal-set rule id"
        );
        assert!(!says.is_empty(), "{row} states no word for its diagnostic");
    }
}
