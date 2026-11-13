//! The assembler issues identities from the checked products it is fed.
use super::product::{self, Sources, id};
use crate::WasmCompileError;
use crate::emit::MathImport;
use crate::linked::{
    LINKED_DISPATCH_REFUSED, LinkedDemand, LinkedEntryKind, LinkedProductAssembler,
    linked_entry_status, linked_key_words,
};
use rumoca_ir_solve::fmi::FmiDerivativeStatus;

/// Each kind's key is the one its `PreparedKernelSource` constructor issues
/// over the original checked source, at the owner's dense position.
#[test]
fn the_assembler_issues_each_kinds_key_from_its_checked_source() {
    let sources = Sources::new();
    let built = product::build(&sources);
    let expected = product::expected_keys(&sources)
        .into_iter()
        .map(|(id, key)| (id, linked_key_words(&key).unwrap()))
        .collect::<Vec<_>>();
    let mut issued = built.linked.plan().entries().collect::<Vec<_>>();
    let mut expected_sorted = expected.clone();
    expected_sorted.sort();
    issued.sort();
    assert_eq!(issued, expected_sorted);
    assert_eq!(
        issued
            .iter()
            .filter(|(id, _)| id.kind == LinkedEntryKind::Kinematic)
            .map(|(id, _)| id.index)
            .collect::<Vec<_>>(),
        [1],
        "only the operation with a kinematic partition issues the entry"
    );
}

/// Equal checked sources at two positions issue equal keys; each position
/// keeps its own ordinal, symbol and export in its batch's entry order.
#[test]
fn equal_key_positions_keep_their_own_ordinal_and_export() {
    let sources = Sources::new();
    let built = product::build(&sources);
    let plan = built.linked.plan();
    let (first, second, again) = (
        id(LinkedEntryKind::Private, 0),
        id(LinkedEntryKind::Private, 1),
        id(LinkedEntryKind::Private, 2),
    );
    assert_eq!(plan.key_words(first), plan.key_words(again));
    assert_ne!(plan.key_words(first), plan.key_words(second));
    let module = built
        .linked
        .modules()
        .iter()
        .find(|module| module.exports().contains(&first.symbol()))
        .unwrap();
    assert_eq!(
        module.exports(),
        [first.symbol(), second.symbol(), again.symbol()]
    );
}

#[test]
fn a_demanded_entry_without_a_linked_form_refuses_the_product_by_name() {
    for (demand, expected) in [
        (
            LinkedDemand::Residual { name: "rows 0..3" },
            "residual entry rows 0..3",
        ),
        (
            LinkedDemand::TypedCall {
                name: "transaction 2",
            },
            "typed-call entry transaction 2",
        ),
    ] {
        let Err(WasmCompileError::Input(message)) = LinkedProductAssembler::new().finish(&[demand])
        else {
            panic!("a demanded entry without a linked form refuses the product");
        };
        assert!(message.contains(expected), "{message}");
    }
    assert!(LinkedProductAssembler::new().finish(&[]).is_ok());
}

#[test]
fn malformed_keys_refuse_and_words_are_little_endian() {
    for key in [
        "",
        &"0".repeat(63),
        &"0".repeat(65),
        &"G".repeat(64),
        &"A".repeat(64),
    ] {
        assert!(linked_key_words(key).is_err(), "{key}");
    }
    let key = format!("0100000000000000{}", "0".repeat(48));
    assert_eq!(linked_key_words(&key).unwrap(), [1, 0, 0, 0]);
}

/// No symbol is reused, and none is a math ABI name or a reserved slot.
#[test]
fn symbols_are_unique_and_disjoint_from_math_and_reserved_names() {
    let sources = Sources::new();
    let built = product::build(&sources);
    let mut symbols = built
        .linked
        .modules()
        .iter()
        .flat_map(|module| module.exports().iter().cloned())
        .collect::<Vec<_>>();
    let count = symbols.len();
    symbols.sort();
    symbols.dedup();
    assert_eq!(symbols.len(), count);
    let reserved = [
        "eval_assignments",
        "rumoca_source_capsule_len",
        "rumoca_source_capsule_copy",
        "rumoca_publication_token_len",
        "rumoca_publication_token_copy",
        "rumoca_dispatch",
    ];
    for symbol in &symbols {
        assert!(symbol.starts_with("rumoca_k_"), "{symbol}");
        assert!(!MathImport::ALL.iter().any(|math| math.symbol() == symbol));
        assert!(!reserved.contains(&symbol.as_str()));
    }
    for (id, _) in built.linked.plan().entries() {
        assert!(symbols.contains(&id.symbol()), "{}", id.symbol());
    }
}

/// The refusal is never an entry status and is mapped before any decoding.
#[test]
fn no_entry_status_can_equal_the_refusal() {
    assert_eq!(
        FmiDerivativeStatus::from_code(LINKED_DISPATCH_REFUSED),
        None
    );
    assert!(crate::linked::plan::refuse_reserved_status([0, 3, LINKED_DISPATCH_REFUSED]).is_err());
    assert!(crate::linked::plan::refuse_reserved_status([0, 3, u32::MAX - 1]).is_ok());
    let Err(WasmCompileError::Input(message)) = linked_entry_status(LINKED_DISPATCH_REFUSED) else {
        panic!("the refusal maps to an input error before decoding");
    };
    assert!(message.contains("no entry ran"));
    for status in [0, 1, 5, 1000, u32::MAX - 1] {
        assert_eq!(linked_entry_status(status).unwrap(), status);
    }
}
