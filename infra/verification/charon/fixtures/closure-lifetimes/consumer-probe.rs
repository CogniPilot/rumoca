#![feature(rustc_private)]

extern crate rustc_borrowck;
extern crate rustc_driver;
extern crate rustc_hir;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_session;

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
    let bodies = consumers::get_bodies_with_borrowck_facts(
        tcx,
        def_id,
        ConsumerOptions::RegionInferenceContext,
    );
    let mut ordered: Vec<_> = bodies.into_iter().collect();
    ordered.sort_by_key(|(id, _)| tcx.def_path_str(*id));
    for (id, facts) in ordered {
        println!("BODY {}", tcx.def_path_str(id));
        if matches!(tcx.def_kind(id), rustc_hir::def::DefKind::Closure { .. }) {
            check_fixture_relations(tcx, id, &facts);
        }
        let mut regions = Vec::new();
        for (local, decl) in facts
            .body
            .local_decls
            .iter_enumerated()
            .take(facts.body.arg_count + 1)
        {
            println!("  SIGNATURE {local:?}: {:?}", decl.ty);
            tcx.for_each_free_region(&decl.ty, |region| {
                if let ty::ReVar(vid) = region.kind() {
                    if !regions.contains(&vid) {
                        regions.push(vid);
                    }
                } else {
                    println!("  NON_VARIABLE_REGION {region:?}");
                }
            });
        }
        for &sup in &regions {
            for &sub in &regions {
                if sup != sub {
                    println!(
                        "  OUTLIVES {sup:?}: {sub:?} = {}",
                        facts.region_inference_context.eval_outlives(sup, sub)
                    );
                }
            }
        }
    }
    // This diagnostic observes facts before bodies are stolen, then runs the
    // original checker. It retains no compiler-lifetime data outside this call.
    let mut providers = Providers::default();
    rustc_borrowck::provide(&mut providers.queries);
    (providers.queries.mir_borrowck)(tcx, def_id)
}

fn reference_regions(ty: ty::Ty<'_>) -> Vec<ty::RegionVid> {
    match ty.kind() {
        ty::Ref(region, _, _) => match region.kind() {
            ty::ReVar(vid) => vec![vid],
            other => panic!("fixture signature reference lacks a region variable: {other:?}"),
        },
        ty::Tuple(fields) => fields.iter().flat_map(reference_regions).collect(),
        other => panic!("fixture signature has unexpected type: {other:?}"),
    }
}

fn check_fixture_relations<'tcx>(
    tcx: TyCtxt<'tcx>,
    id: LocalDefId,
    facts: &BodyWithBorrowckFacts<'tcx>,
) {
    let name = tcx.def_path_str(id);
    let root = name
        .strip_suffix("::{closure#0}")
        .expect("fixture closure ordinal");
    let expected: &[&[bool]] = match root {
        "named" | "higher_ranked" => &[&[true]],
        "distinct" | "captured" => &[&[false], &[true]],
        "first" | "captured_argument" => &[&[true], &[false]],
        "paired" => &[&[true, false], &[false, true]],
        "swapped" => &[&[false, true], &[true, false]],
        _ => panic!("unknown diagnostic fixture: {name}"),
    };
    let outputs = reference_regions(facts.body.return_ty());
    let mut sources: Vec<_> = facts
        .body
        .args_iter()
        .skip(1)
        .flat_map(|local| reference_regions(facts.body.local_decls[local].ty))
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
            .flat_map(reference_regions),
    );
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
                root == "higher_ranked" && wanted,
                "{root}: output{output_index} outlives source{input_index}"
            );
        }
    }
    println!("  CHECKED_FIXTURE {root}");
}

fn main() -> std::process::ExitCode {
    rustc_driver::catch_with_exit_code(|| {
        let args: Vec<_> = std::env::args().collect();
        rustc_driver::run_compiler(&args, &mut Calls);
    })
}
