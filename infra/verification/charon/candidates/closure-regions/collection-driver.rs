#![feature(rustc_private)]

extern crate rustc_driver;
extern crate rustc_hir;
extern crate rustc_interface;
extern crate rustc_middle;

mod binding_checks;

use std::collections::BTreeMap;
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};

use closure_region_facts::ClosureRegionFacts;
use closure_region_facts::collection::{Collection, CollectionError};
use rustc_driver::{Callbacks, Compilation};
use rustc_hir::def::DefKind;
use rustc_hir::def_id::{DefPathHash, LocalDefId};
use rustc_interface::interface::Compiler;
use rustc_middle::queries::mir_borrowck::ProvidedValue;
use rustc_middle::ty::TyCtxt;

struct CheckCollection {
    collection: Collection,
    early: BTreeMap<DefPathHash, Arc<ClosureRegionFacts>>,
    compose: bool,
}

static ORIGINAL_QUERIES: AtomicUsize = AtomicUsize::new(0);

fn counted_borrowck<'tcx>(tcx: TyCtxt<'tcx>, owner: LocalDefId) -> ProvidedValue<'tcx> {
    ORIGINAL_QUERIES.fetch_add(1, Ordering::Relaxed);
    (rustc_interface::DEFAULT_QUERY_PROVIDERS
        .queries
        .mir_borrowck)(tcx, owner)
}

impl Callbacks for CheckCollection {
    fn config(&mut self, config: &mut rustc_interface::Config) {
        if self.compose {
            config.override_queries = Some(|_, providers| {
                providers.queries.mir_borrowck = counted_borrowck;
                Collection::install_query(providers);
            });
            assert_eq!(
                self.collection.configure(config),
                Err(CollectionError::ExistingQueryOverride)
            );
        } else {
            self.collection.configure(config).expect("fresh callbacks");
        }
    }

    fn after_expansion<'tcx>(&mut self, _: &Compiler, tcx: TyCtxt<'tcx>) -> Compilation {
        binding_checks::check_binder_scope(tcx);
        for owner in tcx.hir_body_owners() {
            if tcx.def_kind(owner) != DefKind::Closure {
                assert_eq!(
                    self.collection.local(tcx, owner).err(),
                    Some(CollectionError::NotClosure)
                );
                continue;
            }
            let facts = match self.collection.local(tcx, owner) {
                Ok(facts) => facts,
                Err(CollectionError::CompilationRejected) => {
                    assert!(tcx.dcx().has_errors().is_some());
                    println!("COLLECTION_COMPILATION_REJECTED");
                    return Compilation::Stop;
                }
                Err(error) => panic!("early facts: {error:?}"),
            };
            binding_checks::check_source_occurrences(tcx, owner);
            assert_eq!(facts.owner(), tcx.def_path_hash(owner.to_def_id()));
            assert!(self.early.insert(facts.owner(), facts).is_none());
        }
        Compilation::Continue
    }

    fn after_analysis<'tcx>(&mut self, _: &Compiler, tcx: TyCtxt<'tcx>) -> Compilation {
        let mut checked = 0;
        for owner in tcx.hir_body_owners() {
            if tcx.def_kind(owner) != DefKind::Closure {
                continue;
            }
            // The later query can steal promoted MIR. Retrieval must use the
            // early-issued owned facts, not re-run the consumer on stolen MIR.
            let _ = tcx.optimized_mir(owner.to_def_id());
            let facts = self.collection.local(tcx, owner).expect("retained facts");
            let early = self.early.get(&facts.owner()).expect("observed early");
            assert!(Arc::ptr_eq(early, &facts), "query must issue facts once");
            assert_eq!(facts.slots(), early.slots());
            assert_eq!(facts.required_outlives(), early.required_outlives());
            println!("COLLECTED {} {facts:?}", tcx.def_path_str(owner));
            checked += 1;
        }
        assert!(checked > 0, "fixture must exercise a closure");
        assert_eq!(checked, self.early.len());
        println!("CHECKED_EARLY_AND_STOLEN {checked}");
        Compilation::Continue
    }
}

fn run(
    args: &[String],
    compose: bool,
) -> Result<BTreeMap<DefPathHash, Arc<ClosureRegionFacts>>, ()> {
    let collection = Collection::begin().expect("first collection");
    assert_eq!(
        Collection::begin().err(),
        Some(CollectionError::AlreadyActive)
    );
    let mut callbacks = CheckCollection {
        collection,
        early: BTreeMap::new(),
        compose,
    };
    let result = rustc_driver::catch_fatal_errors(|| {
        rustc_driver::run_compiler(args, &mut callbacks);
    });
    let CheckCollection {
        collection, early, ..
    } = callbacks;
    drop(collection);
    result.map(|()| early).map_err(|_| ())
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let (Ok(first), Ok(second)) = (run(&args, false), run(&args, true)) else {
        std::process::exit(1);
    };
    assert_eq!(
        first.keys().collect::<Vec<_>>(),
        second.keys().collect::<Vec<_>>()
    );
    for (owner, facts) in &first {
        let current = &second[owner];
        assert!(
            !Arc::ptr_eq(facts, current),
            "no cross-session record reuse"
        );
        assert_eq!(facts.slots(), current.slots());
        assert_eq!(facts.required_outlives(), current.required_outlives());
    }
    assert!(ORIGINAL_QUERIES.load(Ordering::Relaxed) > 0);
    println!("CHECKED_COMPOSED_QUERY_PROVIDER");
    println!("CHECKED_SESSION_RETIREMENT {}", first.len());
}
