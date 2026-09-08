use std::collections::{BTreeMap, BTreeSet};
use std::sync::{Arc, Mutex, OnceLock};

use rustc_borrowck::consumers::{self, ConsumerOptions};
use rustc_hir::def::DefKind;
use rustc_hir::def_id::{DefPathHash, LocalDefId};
use rustc_middle::queries::mir_borrowck::ProvidedValue;
use rustc_middle::ty::TyCtxt;
use rustc_middle::util::Providers;
use serde::{Deserialize, Serialize};

use crate::{ClosureRegionFacts, FactError};

type BorrowckQuery = for<'tcx> fn(TyCtxt<'tcx>, LocalDefId) -> ProvidedValue<'tcx>;
type FactRecord = Result<Arc<ClosureRegionFacts>, FactError>;

struct CollectionState {
    original_query: OnceLock<BorrowckQuery>,
    closure_roots: OnceLock<BTreeSet<DefPathHash>>,
    records: Mutex<BTreeMap<DefPathHash, FactRecord>>,
}

// Query providers are function pointers, not capturing closures. The guard
// owns this registration; records contain no rustc-lifetime values or pointers.
static ACTIVE: Mutex<Option<Arc<CollectionState>>> = Mutex::new(None);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum CollectionError {
    AlreadyActive,
    Poisoned,
    ExistingQueryOverride,
    NotClosure,
    CompilationRejected,
    MissingFacts,
    InvalidFacts(FactError),
}

/// One compiler invocation's owned fact collection. Query callbacks may run on
/// different worker threads. A second concurrent invocation is rejected rather
/// than sharing facts; dropping the guard retires the registration.
/// The guard must outlive the complete `rustc_driver::run_compiler` invocation,
/// including its configuration and all worker callbacks, not only `config`.
#[must_use]
pub struct Collection {
    state: Arc<CollectionState>,
}

impl Collection {
    pub fn begin() -> Result<Self, CollectionError> {
        let mut active = ACTIVE.lock().map_err(|_| CollectionError::Poisoned)?;
        if active.is_some() {
            return Err(CollectionError::AlreadyActive);
        }
        let state = Arc::new(CollectionState {
            original_query: OnceLock::new(),
            closure_roots: OnceLock::new(),
            records: Mutex::new(BTreeMap::new()),
        });
        *active = Some(Arc::clone(&state));
        Ok(Self { state })
    }

    pub fn configure(&self, config: &mut rustc_interface::Config) -> Result<(), CollectionError> {
        if config.override_queries.is_some() {
            return Err(CollectionError::ExistingQueryOverride);
        }
        config.override_queries = Some(|_, providers| Self::install_query(providers));
        Ok(())
    }

    /// Also usable inside a driver's composed `override_queries` callback.
    /// Preserve the provider installed by the driver, not a guessed default.
    pub fn install_query(providers: &mut Providers) {
        let state = active_state();
        assert!(
            state
                .original_query
                .set(providers.queries.mir_borrowck)
                .is_ok(),
            "closure fact query must be installed once per compiler invocation"
        );
        providers.queries.mir_borrowck = collect_query;
    }

    /// Request the declaration owner's real borrow-checking query before
    /// retrieving its facts. Absence and unsupported shapes are distinct from
    /// a successfully checked declaration with no required relations.
    pub fn local(
        &self,
        tcx: TyCtxt<'_>,
        owner: LocalDefId,
    ) -> Result<Arc<ClosureRegionFacts>, CollectionError> {
        if tcx.def_kind(owner) != DefKind::Closure {
            return Err(CollectionError::NotClosure);
        }
        let root = tcx.typeck_root_def_id(owner.to_def_id()).expect_local();
        tcx.mir_borrowck(root)
            .map_err(|_| CollectionError::CompilationRejected)?;
        self.state
            .records
            .lock()
            .map_err(|_| CollectionError::Poisoned)?
            .get(&tcx.def_path_hash(owner.to_def_id()))
            .ok_or(CollectionError::MissingFacts)?
            .clone()
            .map_err(CollectionError::InvalidFacts)
    }
}

impl Drop for Collection {
    fn drop(&mut self) {
        let mut active = ACTIVE
            .lock()
            .expect("closure fact registration cannot be poisoned during normal retirement");
        assert!(
            active
                .as_ref()
                .is_some_and(|state| Arc::ptr_eq(state, &self.state)),
            "retiring collection must own the active registration"
        );
        *active = None;
    }
}

fn active_state() -> Arc<CollectionState> {
    Arc::clone(
        ACTIVE
            .lock()
            .expect("closure fact registration is poisoned")
            .as_ref()
            .expect("closure fact collection guard must outlive compiler callbacks"),
    )
}

fn collect_query<'tcx>(tcx: TyCtxt<'tcx>, root: LocalDefId) -> ProvidedValue<'tcx> {
    let state = active_state();
    let original = state
        .original_query
        .get()
        .expect("query installation records its original provider");
    // Compute the owner partition once, using HIR identity rather than names or
    // MIR contents. Roots without closures need only the original borrow check.
    let closure_roots = state.closure_roots.get_or_init(|| {
        tcx.hir_body_owners()
            .filter(|&owner| tcx.def_kind(owner) == DefKind::Closure)
            .map(|owner| tcx.def_path_hash(tcx.typeck_root_def_id(owner.to_def_id())))
            .collect()
    });
    if !closure_roots.contains(&tcx.def_path_hash(root.to_def_id())) {
        return original(tcx, root);
    }
    let bodies = consumers::get_bodies_with_borrowck_facts(
        tcx,
        root,
        ConsumerOptions::RegionInferenceContext,
    );
    let mut records = BTreeMap::new();
    for (owner, facts) in bodies {
        if tcx.def_kind(owner) == DefKind::Closure {
            records.insert(
                tcx.def_path_hash(owner.to_def_id()),
                ClosureRegionFacts::from_body(tcx, owner, &facts).map(Arc::new),
            );
        }
    }
    // The consumer API does not return the original query's hidden-type result.
    // Preserve that query, including errors, and publish only after it succeeds.
    let result = original(tcx, root)?;
    // Do not publish after an already diagnosed error. The caller must also
    // require the complete compiler invocation to succeed before exporting:
    // a later error can still reject a session that issued early observations.
    if let Some(error) = tcx.dcx().has_errors() {
        return Err(error);
    }
    let mut collected = state
        .records
        .lock()
        .expect("closure fact publication mutex is poisoned");
    for (owner, facts) in records {
        assert!(
            collected.insert(owner, facts).is_none(),
            "one declaration must have exactly one producing borrow-check query"
        );
    }
    Ok(result)
}
