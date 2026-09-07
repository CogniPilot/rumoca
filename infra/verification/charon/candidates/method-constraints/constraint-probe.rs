#![feature(rustc_private)]

extern crate rustc_borrowck;
extern crate rustc_driver;
extern crate rustc_hir;
extern crate rustc_interface;
extern crate rustc_middle;

use rustc_borrowck::consumers::{self, BodyWithBorrowckFacts, ConsumerOptions};
use rustc_hir::def_id::LocalDefId;
use rustc_middle::queries::mir_borrowck::ProvidedValue;
use rustc_middle::ty::{self, TyCtxt};
use rustc_middle::util::Providers;

struct Observe;

impl rustc_driver::Callbacks for Observe {
    fn config(&mut self, config: &mut rustc_interface::Config) {
        assert!(config.override_queries.is_none());
        config.override_queries = Some(|_, providers| {
            providers.queries.mir_borrowck = inspect;
        });
    }
}

fn inspect<'tcx>(tcx: TyCtxt<'tcx>, owner: LocalDefId) -> ProvidedValue<'tcx> {
    let bodies =
        consumers::get_bodies_with_borrowck_facts(tcx, owner, ConsumerOptions::PoloniusInputFacts);
    let mut bodies: Vec<_> = bodies.into_iter().collect();
    bodies.sort_by_key(|(id, _)| tcx.def_path_str(*id));
    for (id, facts) in bodies {
        if matches!(tcx.def_kind(id), rustc_hir::def::DefKind::Closure) {
            println!("CLOSURE {}", tcx.def_path_str(id));
            inspect_relations(tcx, &facts);
        }
    }
    // This is a diagnostic observer. Preserve the actual borrow-checker query,
    // return its result, and do not publish observations as compiler contracts.
    let mut providers = Providers::default();
    rustc_borrowck::provide(&mut providers.queries);
    (providers.queries.mir_borrowck)(tcx, owner)
}

fn inspect_relations<'tcx>(tcx: TyCtxt<'tcx>, facts: &BodyWithBorrowckFacts<'tcx>) {
    let input = facts.input_facts.as_ref().expect("requested input facts");
    let universals: Vec<ty::RegionVid> = input
        .universal_region
        .iter()
        .copied()
        .map(Into::into)
        .collect();
    println!("  UNIVERSALS {universals:?}");
    println!("  KNOWN_SUBSET {:?}", input.known_placeholder_subset);
    let mut regions = Vec::new();
    for (local, decl) in facts
        .body
        .local_decls
        .iter_enumerated()
        .take(facts.body.arg_count + 1)
    {
        println!("  POSITION {local:?}: {:?}", decl.ty);
        tcx.for_each_free_region(&decl.ty, |region| {
            if let ty::ReVar(vid) = region.kind() {
                regions.push(vid);
            }
        });
    }
    regions.sort();
    regions.dedup();
    let context = &facts.region_inference_context;
    let graph = context.constraint_sccs();
    for region in regions {
        let same_scc: Vec<_> = universals
            .iter()
            .filter(|&&universal| graph.scc(region) == graph.scc(universal))
            .collect();
        println!("  SAME_SCC {region:?}: {same_scc:?}");
    }
    for &longer in &universals {
        for &shorter in &universals {
            let required = reachable(graph.scc(longer), graph.scc(shorter), |node| {
                graph.successors(node).to_vec()
            });
            let solved = context.eval_outlives(longer, shorter);
            assert!(
                !required || solved,
                "constraint path must hold in the solution"
            );
            println!("  RELATION {longer:?}: {shorter:?}: required={required}, solved={solved}");
        }
    }
}

fn reachable<N: Copy + Eq>(start: N, end: N, successors: impl Fn(N) -> Vec<N>) -> bool {
    let mut pending = vec![start];
    let mut visited = Vec::new();
    while let Some(node) = pending.pop() {
        if node == end {
            return true;
        }
        if !visited.contains(&node) {
            visited.push(node);
            pending.extend(successors(node));
        }
    }
    false
}

fn main() {
    rustc_driver::run_compiler(&std::env::args().collect::<Vec<_>>(), &mut Observe);
}
