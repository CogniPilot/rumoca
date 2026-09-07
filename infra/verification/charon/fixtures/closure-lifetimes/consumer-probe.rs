#![feature(rustc_private)]

extern crate rustc_borrowck;
extern crate rustc_driver;
extern crate rustc_hir;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_session;

use closure_region_facts::{ClosureRegionFacts, FactError, RegionPart};
use rustc_borrowck::consumers::{self, BodyWithBorrowckFacts, ConsumerOptions};
use rustc_hir::def_id::LocalDefId;
use rustc_middle::queries::mir_borrowck::ProvidedValue;
use rustc_middle::ty::{self, TyCtxt};
use rustc_middle::util::Providers;

struct Calls;

impl rustc_driver::Callbacks for Calls {
    fn config(&mut self, config: &mut rustc_interface::Config) {
        assert!(config.override_queries.is_none());
        config.override_queries = Some(|_, providers| {
            providers.queries.mir_borrowck = inspect_borrowck;
        });
    }
}

fn inspect_borrowck<'tcx>(tcx: TyCtxt<'tcx>, def_id: LocalDefId) -> ProvidedValue<'tcx> {
    let bodies =
        consumers::get_bodies_with_borrowck_facts(tcx, def_id, ConsumerOptions::PoloniusInputFacts);
    let mut ordered: Vec<_> = bodies.into_iter().collect();
    ordered.sort_by_key(|(id, _)| tcx.def_path_str(*id));
    for (id, mut facts) in ordered {
        println!("BODY {}", tcx.def_path_str(id));
        if matches!(tcx.def_kind(id), rustc_hir::def::DefKind::Closure) {
            let owned = ClosureRegionFacts::from_body(tcx, id, &facts)
                .expect("fixture closure region positions must match completely");
            assert_eq!(owned.owner(), tcx.def_path_hash(id.to_def_id()));
            assert_eq!(
                ClosureRegionFacts::from_body(tcx, def_id, &facts).err(),
                Some(FactError::WrongBodyOwner),
                "a borrowck root cannot claim its child closure's facts"
            );
            println!("  OWNED_CLOSURE_FACTS {owned:?}");
            check_fixture_relations(tcx, id, &facts, &owned);
            check_fact_refusals(tcx, id, &mut facts);
        }
        inspect_universal_relations(tcx, &facts);
    }
    // This diagnostic observes facts before bodies are stolen, then runs the
    // original checker. It retains no compiler-lifetime data outside this call.
    let mut providers = Providers::default();
    rustc_borrowck::provide(&mut providers.queries);
    let result = (providers.queries.mir_borrowck)(tcx, def_id);
    if let Ok(hidden_types) = result {
        for (opaque, hidden) in hidden_types {
            println!(
                "  DEFINITION_HIDDEN_TYPE {}: {hidden:?}",
                tcx.def_path_str(*opaque)
            );
        }
    }
    result
}

fn check_fact_refusals<'tcx>(
    tcx: TyCtxt<'tcx>,
    id: LocalDefId,
    facts: &mut BodyWithBorrowckFacts<'tcx>,
) {
    let arg_count = facts.body.arg_count;
    facts.body.arg_count = 0;
    assert_eq!(
        ClosureRegionFacts::from_body(tcx, id, facts).err(),
        Some(FactError::MissingReceiver)
    );
    facts.body.arg_count = arg_count;
    let receiver = facts.body.args_iter().next().expect("restored receiver");
    let original = facts.body.local_decls[receiver].ty;
    facts.body.local_decls[receiver].ty = tcx.types.u32;
    assert_eq!(
        ClosureRegionFacts::from_body(tcx, id, facts).err(),
        Some(FactError::NotClosure)
    );
    let other = tcx
        .hir_body_owners()
        .find(|&other| {
            other != id && matches!(tcx.def_kind(other), rustc_hir::def::DefKind::Closure)
        })
        .expect("fixtures contain another genuine closure type");
    facts.body.local_decls[receiver].ty = tcx.type_of(other).instantiate_identity().skip_norm_wip();
    assert_eq!(
        ClosureRegionFacts::from_body(tcx, id, facts).err(),
        Some(FactError::WrongReceiverOwner)
    );
    facts.body.local_decls[receiver].ty = original;
    ClosureRegionFacts::from_body(tcx, id, facts).expect("restored facts still accepted");
    println!("  CHECKED_REFUSALS {}", tcx.def_path_str(id));
}

fn inspect_universal_relations<'tcx>(tcx: TyCtxt<'tcx>, facts: &BodyWithBorrowckFacts<'tcx>) {
    let mut occurrences = Vec::new();
    for (local, decl) in facts
        .body
        .local_decls
        .iter_enumerated()
        .take(facts.body.arg_count + 1)
    {
        println!("  SIGNATURE {local:?}: {:?}", decl.ty);
        occurrences.extend(reference_regions(tcx, decl.ty));
    }
    let count = occurrences.len();
    occurrences.sort();
    occurrences.dedup();
    println!(
        "  BODY_REGION_OCCURRENCES {count}; DISTINCT_VIDS {}",
        occurrences.len()
    );
    let input = facts
        .input_facts
        .as_ref()
        .expect("requested Polonius input facts");
    println!("  UNIVERSAL_REGIONS {:?}", input.universal_region);
    println!(
        "  KNOWN_UNIVERSAL_SUBSET {:?}",
        input.known_placeholder_subset
    );
    for region in occurrences {
        let equivalents: Vec<ty::RegionVid> = input
            .universal_region
            .iter()
            .copied()
            .map(Into::into)
            .filter(|&universal| facts.region_inference_context.eval_equal(region, universal))
            .collect();
        println!("  UNIVERSAL_EQUIVALENTS {region:?}: {equivalents:?}");
    }
}

fn reference_regions<'tcx>(tcx: TyCtxt<'tcx>, ty: ty::Ty<'tcx>) -> Vec<ty::RegionVid> {
    let mut regions = Vec::new();
    tcx.for_each_free_region(&ty, |region| match region.kind() {
        ty::ReVar(vid) => regions.push(vid),
        other => panic!("fixture signature reference lacks a region variable: {other:?}"),
    });
    regions
}

fn check_fixture_relations<'tcx>(
    tcx: TyCtxt<'tcx>,
    id: LocalDefId,
    facts: &BodyWithBorrowckFacts<'tcx>,
    owned: &ClosureRegionFacts,
) {
    let name = tcx.def_path_str(id);
    let root = name
        .strip_suffix("::{closure#0}")
        .expect("fixture closure ordinal");
    if matches!(
        root,
        "one_region"
            | "two_regions_capture_first"
            | "two_regions_capture_second"
            | "two_regions_ref_item"
    ) {
        check_iterator_shape(root, owned);
        println!("  CHECKED_FIXTURE {root}");
        return;
    }
    if matches!(
        root,
        "phantom_capture" | "mixed_bound_free" | "consume_once"
    ) {
        check_region_shape(root, owned);
        println!("  CHECKED_FIXTURE {root}");
        return;
    }
    let expected: &[&[bool]] = match root {
        "named" | "higher_ranked" | "adapter" | "mapped" => &[&[true]],
        "distinct" | "captured" => &[&[false], &[true]],
        "first" | "captured_argument" => &[&[true], &[false]],
        "paired" => &[&[true, false], &[false, true]],
        "swapped" => &[&[false, true], &[true, false]],
        _ => panic!("unknown diagnostic fixture: {name}"),
    };
    let outputs = reference_regions(tcx, facts.body.return_ty());
    let mut sources: Vec<_> = facts
        .body
        .args_iter()
        .skip(1)
        .flat_map(|local| reference_regions(tcx, facts.body.local_decls[local].ty))
        .collect();
    let receiver = facts.body.local_decls[facts.body.args_iter().next().expect("receiver")].ty;
    let ty::Ref(_, closure, _) = receiver.kind() else {
        panic!("fixture receiver is not borrowed");
    };
    let ty::Closure(_, args) = closure.kind() else {
        panic!("fixture receiver lacks a closure type");
    };
    sources.extend(
        args.as_closure()
            .upvar_tys()
            .iter()
            .flat_map(|ty| reference_regions(tcx, ty)),
    );
    let raw_closure = tcx.type_of(id).instantiate_identity().skip_norm_wip();
    let ty::Closure(_, raw_args) = raw_closure.kind() else {
        panic!("fixture item lacks a closure type");
    };
    println!("  RAW_CLOSURE_SIGNATURE {:?}", raw_args.as_closure().sig());
    println!("  FACT_CLOSURE_SIGNATURE {:?}", args.as_closure().sig());
    assert_matrix(
        root,
        facts,
        &sources,
        &outputs,
        expected,
        root == "higher_ranked",
    );
    let raw_signature = raw_args.as_closure().sig();
    let fact_signature = args.as_closure().sig();
    assert_eq!(
        tcx.erase_and_anonymize_regions(raw_signature),
        tcx.erase_and_anonymize_regions(fact_signature)
    );
    if fact_signature.bound_vars().is_empty() {
        let signature = fact_signature.skip_binder();
        let mut inputs: Vec<_> = signature
            .inputs()
            .iter()
            .flat_map(|ty| reference_regions(tcx, *ty))
            .collect();
        inputs.extend(
            args.as_closure()
                .upvar_tys()
                .iter()
                .flat_map(|ty| reference_regions(tcx, ty)),
        );
        let outputs = reference_regions(tcx, signature.output());
        assert_matrix(root, facts, &inputs, &outputs, expected, false);
        assert_owned_matrix(root, owned, outputs.len(), expected);
        println!("  CHECKED_FREE_SIGNATURE {root}");
    } else {
        assert_eq!(raw_signature, fact_signature);
        assert!(owned
            .slots()
            .iter()
            .all(|slot| slot.part() != RegionPart::Signature));
        println!("  CHECKED_BOUND_SIGNATURE {root}");
    }
    println!("  CHECKED_FIXTURE {root}");
}

fn check_iterator_shape(root: &str, owned: &ClosureRegionFacts) {
    let captures: Vec<_> = owned
        .slots()
        .iter()
        .copied()
        .filter(|slot| slot.part() == RegionPart::Captures)
        .collect();
    let signature: Vec<_> = owned
        .slots()
        .iter()
        .copied()
        .filter(|slot| slot.part() == RegionPart::Signature)
        .collect();
    assert_eq!(captures.len(), 1);
    assert_eq!(signature.len(), usize::from(root == "two_regions_ref_item"));
    if let Some(&output) = signature.first() {
        assert!(owned
            .outlives()
            .iter()
            .any(|edge| edge.longer() == captures[0] && edge.shorter() == output));
    }
}

fn check_region_shape(root: &str, owned: &ClosureRegionFacts) {
    let expected_captures = if root == "phantom_capture" { 2 } else { 1 };
    assert_eq!(owned.slots().len(), expected_captures + 1);
    let signatures: Vec<_> = owned
        .slots()
        .iter()
        .copied()
        .filter(|slot| slot.part() == RegionPart::Signature)
        .collect();
    assert_eq!(
        signatures.len(),
        1,
        "bound argument/result occurrences must stay bound"
    );
    for slot in owned
        .slots()
        .iter()
        .copied()
        .filter(|slot| slot.part() == RegionPart::Captures)
    {
        let reaches_output = owned
            .outlives()
            .iter()
            .any(|edge| edge.longer() == slot && edge.shorter() == signatures[0]);
        assert_eq!(
            reaches_output,
            slot.occurrence() == expected_captures - 1,
            "{root}: phantom and actual capture must remain distinct"
        );
    }
}

fn assert_owned_matrix(
    root: &str,
    owned: &ClosureRegionFacts,
    output_count: usize,
    expected: &[&[bool]],
) {
    let mut signature: Vec<_> = owned
        .slots()
        .iter()
        .copied()
        .filter(|slot| slot.part() == RegionPart::Signature)
        .collect();
    let outputs = signature.split_off(
        signature
            .len()
            .checked_sub(output_count)
            .expect("output slots"),
    );
    let mut sources = signature;
    sources.extend(
        owned
            .slots()
            .iter()
            .copied()
            .filter(|slot| slot.part() == RegionPart::Captures),
    );
    assert_eq!(sources.len(), expected.len(), "{root}: owned source count");
    for (source, row) in sources.into_iter().zip(expected) {
        assert_eq!(outputs.len(), row.len(), "{root}: owned output count");
        for (&output, &wanted) in outputs.iter().zip(*row) {
            assert_eq!(
                owned
                    .outlives()
                    .iter()
                    .any(|edge| edge.longer() == source && edge.shorter() == output),
                wanted,
                "{root}: owned {source:?} outlives {output:?}"
            );
            assert!(
                !owned
                    .outlives()
                    .iter()
                    .any(|edge| edge.longer() == output && edge.shorter() == source),
                "{root}: owned reverse relation is not implied"
            );
        }
    }
}

fn assert_matrix(
    root: &str,
    facts: &BodyWithBorrowckFacts<'_>,
    sources: &[ty::RegionVid],
    outputs: &[ty::RegionVid],
    expected: &[&[bool]],
    reverse_is_equal: bool,
) {
    assert_eq!(sources.len(), expected.len(), "{root}: source count");
    for (input_index, (&source, row)) in sources.iter().zip(expected).enumerate() {
        assert_eq!(outputs.len(), row.len(), "{root}: output count");
        for (output_index, (&output, &wanted)) in outputs.iter().zip(*row).enumerate() {
            assert_eq!(
                facts.region_inference_context.eval_outlives(source, output),
                wanted,
                "{root}: source{input_index} outlives output{output_index}"
            );
            assert_eq!(
                facts.region_inference_context.eval_outlives(output, source),
                reverse_is_equal && wanted,
                "{root}: output{output_index} outlives source{input_index}"
            );
        }
    }
}

fn main() -> std::process::ExitCode {
    rustc_driver::catch_with_exit_code(|| {
        let args: Vec<_> = std::env::args().collect();
        rustc_driver::run_compiler(&args, &mut Calls);
    })
}
