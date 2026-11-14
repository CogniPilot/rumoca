//! Every issued entry through the one dispatch against its direct browser call.
use super::host::{Browser, Frame, Linked};
use super::product::{self, Shape, Sources, id};
use crate::linked::{
    LINKED_DISPATCH_EXPORT, LINKED_DISPATCH_REFUSED, LinkedBinding, LinkedEntryKind,
    linked_entry_status,
};
use crate::{TypedCallFaultKind, WasmCompileError};
use wasmparser::{Parser, Payload, TypeRef};

const REFUSED: i32 = LINKED_DISPATCH_REFUSED as i32;

/// Same status, same memory, and the dispatch reached exactly the entry's own
/// symbol first; a derivative is called with `cache_valid` 0 and 1.
#[test]
fn every_issued_entry_returns_through_the_dispatch_what_its_direct_call_returns() {
    let sources = Sources::new();
    let product = product::build(&sources);
    let mut browser = Browser::new(&product);
    let mut linked = Linked::new(&product);
    let issued = product
        .linked
        .plan()
        .entries()
        .map(|(id, _)| id)
        .collect::<Vec<_>>();
    assert_eq!(issued.len(), product.shapes.len());
    assert!(
        LinkedEntryKind::ALL
            .iter()
            .all(|kind| issued.iter().any(|id| id.kind == *kind)),
        "the product holds every dispatched kind"
    );
    let mut succeeded = 0;
    for entry in issued {
        let flags = if entry.kind == LinkedEntryKind::Derivative {
            &[0, 1][..]
        } else {
            &[0][..]
        };
        for &cache_valid in flags {
            let mut frame = Frame::new(product.shapes[&entry], None);
            frame.cache_valid = cache_valid;
            let direct = browser.call(entry, &frame);
            linked.reached();
            let dispatched = linked.call(&product, entry, &frame);
            assert_eq!(dispatched, direct, "{} {cache_valid}", entry.symbol());
            assert_ne!(direct, REFUSED);
            assert!(
                browser.host.image() == linked.host.image(),
                "{} writes the same memory through the dispatch",
                entry.symbol()
            );
            assert_eq!(linked.reached().first(), Some(&entry.symbol()));
            succeeded += usize::from(direct == 0);
        }
    }
    assert!(succeeded > 0, "at least one entry succeeds on these inputs");
}

/// `cache_valid` (a8) is a value 0 or 1, not a pointer. A first call with 0
/// prepares the cache; a second call from that memory with 1 uses it. Status,
/// outputs and cache words agree with the direct call each time.
#[test]
fn the_derivative_cache_valid_value_reaches_the_entry_unchanged() {
    let sources = Sources::new();
    let product = product::build(&sources);
    let mut browser = Browser::new(&product);
    let mut linked = Linked::new(&product);
    for index in 0..2 {
        let complete = id(LinkedEntryKind::Derivative, index);
        let Shape::Derivative(workspace) = product.shapes[&complete] else {
            panic!("a derivative entry has a workspace")
        };
        let mut frame = Frame::new(product.shapes[&complete], None);
        for cache_valid in [0, 1] {
            frame.cache_valid = cache_valid;
            let direct = browser.call(complete, &frame);
            let dispatched = linked.call(&product, complete, &frame);
            assert_eq!(dispatched, direct, "{index} {cache_valid}");
            let (direct_image, linked_image) = (browser.host.image(), linked.host.image());
            same_outputs_and_cache(&direct_image, &linked_image, &frame, workspace);
            assert!(direct_image == linked_image);
            frame.image = direct_image[..frame.image.len()].to_vec();
        }
    }
}

/// The output and the three cache regions agree byte for byte.
fn same_outputs_and_cache(
    direct: &[u8],
    linked: &[u8],
    frame: &Frame,
    workspace: rumoca_ir_solve::fmi::FmiDerivativeWorkspace,
) {
    let compared = workspace
        .regions()
        .into_iter()
        .filter(|(arg, _)| [3, 5, 6, 7].contains(arg));
    for (arg, bytes) in compared {
        let at = frame.pointers[arg as usize] as usize;
        let region = at..at + bytes as usize;
        assert_eq!(direct[region.clone()], linked[region], "argument {arg}");
    }
}

#[test]
fn an_absent_identity_is_refused_without_writing_memory() {
    let sources = Sources::new();
    let product = product::build(&sources);
    let mut linked = Linked::new(&product);
    let exact = id(LinkedEntryKind::Exact, 0);
    let key = product.linked.plan().key_words(exact).unwrap();
    let frame = Frame::new(product.shapes[&exact], None);
    for (kind, index) in [
        (0, 0),
        (7, 0),
        (u32::MAX, 0),
        (LinkedEntryKind::Exact.code(), 1),
        (LinkedEntryKind::Private.code(), 3),
        (LinkedEntryKind::Kinematic.code(), 0),
        (LinkedEntryKind::Exact.code(), u32::MAX),
    ] {
        assert_eq!(
            linked.call_raw(kind, index, key, &frame),
            REFUSED,
            "{kind} {index}"
        );
        assert_eq!(linked.host.image()[..frame.image.len()], frame.image[..]);
        assert!(linked.reached().is_empty(), "no entry ran");
    }
}

/// Same kind, same wasm signature, an issued index, a different source:
/// only the key tells the entries apart, and it is checked before the call.
#[test]
fn a_same_signature_entry_of_another_source_is_refused_before_any_state_is_written() {
    let sources = Sources::new();
    let product = product::build(&sources);
    let plan = product.linked.plan();
    let mut linked = Linked::new(&product);
    let first = id(LinkedEntryKind::Private, 0);
    let second = id(LinkedEntryKind::Private, 1);
    let frame = Frame::new(product.shapes[&second], None);
    let wrong = plan.key_words(first).unwrap();
    assert_ne!(wrong, plan.key_words(second).unwrap());
    assert_eq!(
        linked.call_raw(second.kind.code(), second.index, wrong, &frame),
        REFUSED
    );
    assert_eq!(linked.host.image()[..frame.image.len()], frame.image[..]);
    let mut one_word = plan.key_words(second).unwrap();
    one_word[3] ^= 1;
    assert_eq!(
        linked.call_raw(second.kind.code(), second.index, one_word, &frame),
        REFUSED
    );
    assert_eq!(linked.host.image()[..frame.image.len()], frame.image[..]);
    assert!(linked.reached().is_empty(), "no entry ran");
    assert_eq!(linked.call(&product, second, &frame), 0);
    assert_eq!(linked.reached(), [second.symbol()]);
}

/// Positions 0 and 2 hold the same checked source, so their keys are equal;
/// each complete triple still reaches its own position's entry.
#[test]
fn equal_key_positions_each_reach_their_own_entry() {
    let sources = Sources::new();
    let product = product::build(&sources);
    let plan = product.linked.plan();
    let mut linked = Linked::new(&product);
    let first = id(LinkedEntryKind::Private, 0);
    let again = id(LinkedEntryKind::Private, 2);
    assert_eq!(plan.key_words(first), plan.key_words(again));
    for entry in [again, first, again] {
        let frame = Frame::new(product.shapes[&entry], None);
        assert_eq!(linked.call(&product, entry, &frame), 0);
        assert_eq!(linked.reached(), [entry.symbol()]);
    }
}

#[test]
fn a_fault_inside_an_entry_reports_the_same_first_fault_and_fault_map_entry() {
    let sources = Sources::new();
    let product = product::build(&sources);
    let mut browser = Browser::new(&product);
    let mut linked = Linked::new(&product);
    let event = id(LinkedEntryKind::Event, 0);
    let Shape::Kernel { .. } = product.shapes[&event] else {
        panic!("the event entry is a kernel entry")
    };
    // A zero denominator faults the integer quotient of the event's call.
    let frame = Frame::new(product.shapes[&event], Some(&[0.0, 0.0]));
    let direct = browser.call(event, &frame);
    let dispatched = linked.call(&product, event, &frame);
    assert_ne!(direct, 0);
    assert_eq!(dispatched, direct);
    assert!(browser.host.image() == linked.host.image());
    let layout = &sources.event.source().model().problem.layout;
    let (code, _, _) = crate::emit::emit_event_program_module(
        &sources.event,
        layout,
        crate::emit::Linkage::Browser,
    )
    .unwrap();
    let payload = crate::program_payload::ProgramPayload::from(code);
    let status = linked_entry_status(dispatched as u32).unwrap();
    let WasmCompileError::TypedSource(fault) = payload.status_error(status) else {
        panic!("the dispatched status decodes to the original typed fault");
    };
    assert_eq!(fault.kind, TypedCallFaultKind::IntegerArithmetic);
    // The entry's own binding decodes the same fault-map entry.
    let LinkedBinding::Kernel { faults, .. } =
        product.linked.plan().entry(event).unwrap().binding()
    else {
        panic!("the event entry has a kernel binding")
    };
    let WasmCompileError::TypedSource(bound) = faults.status_error(status) else {
        panic!("the binding decodes the original typed fault");
    };
    assert_eq!(bound, fault);
}

/// A refusal leaves an earlier call's first-fault words in the workspace; the
/// caller maps the refusal before reading them.
#[test]
fn a_refusal_leaves_stale_first_fault_words_and_is_mapped_before_decoding() {
    let sources = Sources::new();
    let product = product::build(&sources);
    let mut linked = Linked::new(&product);
    let complete = id(LinkedEntryKind::Derivative, 0);
    let mut frame = Frame::new(product.shapes[&complete], None);
    let workspace = frame.pointers[4] as usize;
    let stale = [3u64, 7];
    for (offset, word) in stale.iter().enumerate() {
        let at = workspace + offset * 8;
        frame.image[at..at + 8].copy_from_slice(&word.to_le_bytes());
    }
    let mut wrong = product.linked.plan().key_words(complete).unwrap();
    wrong[0] ^= 1;
    let status = linked.call_raw(complete.kind.code(), complete.index, wrong, &frame);
    assert_eq!(status, REFUSED);
    let image = linked.host.image();
    for (offset, word) in stale.iter().enumerate() {
        let at = workspace + offset * 8;
        assert_eq!(image[at..at + 8], word.to_le_bytes(), "the words are stale");
    }
    assert!(linked_entry_status(status as u32).is_err());
    frame.cache_valid = 0;
    assert_ne!(linked.call(&product, complete, &frame), REFUSED);
}

type Inventory = (
    Vec<wasmparser::FuncType>,
    Vec<(String, String, u32)>,
    Vec<String>,
);

/// The dispatch module's function types, function imports and exports; it
/// has no memory or data of its own.
fn inventory(bytes: &[u8]) -> Inventory {
    let (mut types, mut imports, mut exports) = (Vec::new(), Vec::new(), Vec::new());
    for payload in Parser::new(0).parse_all(bytes) {
        match payload.unwrap() {
            Payload::TypeSection(reader) => {
                types.extend(reader.into_iter_err_on_gc_types().map(Result::unwrap));
            }
            Payload::ImportSection(reader) => {
                imports.extend(
                    reader
                        .into_iter()
                        .map(|import| function_import(import.unwrap())),
                );
            }
            Payload::ExportSection(reader) => {
                exports.extend(
                    reader
                        .into_iter()
                        .map(|export| export.unwrap().name.to_string()),
                );
            }
            Payload::MemorySection(_) | Payload::DataSection(_) => {
                panic!("the dispatch has no memory of its own")
            }
            _ => {}
        }
    }
    (types, imports, exports)
}

fn function_import(import: wasmparser::Import<'_>) -> (String, String, u32) {
    let TypeRef::Func(ty) = import.ty else {
        panic!("the dispatch imports functions only");
    };
    (import.module.to_string(), import.name.to_string(), ty)
}

#[test]
fn the_dispatch_imports_only_issued_symbols_with_their_own_types_and_exports_one_entry() {
    let sources = Sources::new();
    let product = product::build(&sources);
    let plan = product.linked.plan();
    let bytes = plan.dispatch();
    let (types, imports, exports) = inventory(&bytes);
    assert_eq!(exports, [LINKED_DISPATCH_EXPORT]);
    let expected = plan
        .entries()
        .map(|(id, _)| ("env".to_string(), id.symbol()))
        .collect::<Vec<_>>();
    assert_eq!(
        imports
            .iter()
            .map(|(module, name, _)| (module.clone(), name.clone()))
            .collect::<Vec<_>>(),
        expected
    );
    for ((_, name, ty), (id, _)) in imports.iter().zip(plan.entries()) {
        let params = types[*ty as usize].params().len();
        let expected = if id.kind == LinkedEntryKind::Derivative {
            9
        } else {
            5
        };
        assert_eq!(params, expected, "{name}");
        assert_eq!(types[*ty as usize].results(), [wasmparser::ValType::I32]);
    }
}

/// A kinematic entry reuses its derivative's index, so its indices have
/// holes: derivative 0 issues none, derivative 1 does. Membership in the
/// issued indices decides, not a count of kinematic entries.
#[test]
fn a_kinematic_index_is_admitted_only_where_its_derivative_issued_one() {
    let sources = Sources::new();
    let product = product::build(&sources);
    let plan = product.linked.plan();
    let mut linked = Linked::new(&product);
    let kinematic = |index| id(LinkedEntryKind::Kinematic, index);
    assert!(plan.entry(id(LinkedEntryKind::Derivative, 0)).is_some());
    assert!(plan.entry(kinematic(0)).is_none());
    let issued = kinematic(1);
    let key = plan.key_words(issued).unwrap();
    assert_eq!(
        Some(key),
        plan.key_words(id(LinkedEntryKind::Derivative, 1))
    );
    let frame = Frame::new(product.shapes[&issued], None);
    for absent in [0, 2] {
        assert_eq!(
            linked.call_raw(LinkedEntryKind::Kinematic.code(), absent, key, &frame),
            REFUSED,
            "kinematic {absent}"
        );
        assert!(linked.reached().is_empty());
    }
    assert_ne!(linked.call(&product, issued, &frame), REFUSED);
    assert_eq!(linked.reached().first(), Some(&issued.symbol()));
}
