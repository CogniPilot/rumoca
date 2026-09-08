//! One checked compiler invocation and its artifact-bound dependency facts.

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use rustc_driver::{Callbacks, Compilation};
use rustc_hir::def_id::{CrateNum, DefId};
use rustc_interface::interface::Compiler;
use rustc_middle::ty::TyCtxt;

use crate::ClosureRegionFacts;
use crate::artifact::{LoadedArtifact, PendingArtifact, executing_driver_digest};
use crate::collection::{Collection, CollectionError};

#[derive(Debug)]
pub enum TransportError {
    Io(std::io::Error),
    Decode(serde_json::Error),
    Collection(CollectionError),
    RustcRejected,
    InvalidArtifact(&'static str),
    DependencyNotBuiltByCollector(std::path::PathBuf),
    MissingDeclaration,
    Poisoned,
}

impl From<std::io::Error> for TransportError {
    fn from(error: std::io::Error) -> Self {
        Self::Io(error)
    }
}

impl From<serde_json::Error> for TransportError {
    fn from(error: serde_json::Error) -> Self {
        Self::Decode(error)
    }
}

impl From<CollectionError> for TransportError {
    fn from(error: CollectionError) -> Self {
        Self::Collection(error)
    }
}

/// Facts never contain rustc-lifetime values. Foreign records are decoded and
/// bound to rustc's resolved artifact once, then shared by opaque handles.
#[must_use]
pub struct FactSession {
    collection: Collection,
    driver_digest: String,
    imported: Mutex<BTreeMap<CrateNum, LoadedArtifact>>,
}

impl FactSession {
    /// A driver-owned Cargo namespace changes with the format, linked compiler
    /// version and executable bytes, including the collector implementation.
    pub fn cache_key() -> Result<String, TransportError> {
        Self::cache_key_for_driver(executing_driver_digest()?)
    }

    fn cache_key_for_driver(driver_digest: String) -> Result<String, TransportError> {
        let compiler = rustc_interface::util::rustc_version_str().ok_or(
            TransportError::InvalidArtifact("unknown Rust compiler version"),
        )?;
        let identity = serde_json::to_vec(&(crate::artifact::SCHEMA, compiler, driver_digest))?;
        Ok(rustc_span::SourceFileHash::new_in_memory(
            rustc_span::SourceFileHashAlgorithm::Sha256,
            identity,
        )
        .to_string())
    }

    pub fn begin() -> Result<Self, TransportError> {
        let driver_digest = executing_driver_digest()?;
        if let Some(expected) = std::env::var_os("CHARON_CLOSURE_FACT_CACHE_KEY")
            && expected != Self::cache_key_for_driver(driver_digest.clone())?.as_str()
        {
            return Err(TransportError::InvalidArtifact(
                "collector changed during Cargo invocation",
            ));
        }
        Ok(Self {
            collection: Collection::begin()?,
            driver_digest,
            imported: Mutex::new(BTreeMap::new()),
        })
    }

    pub fn facts(
        &self,
        tcx: TyCtxt<'_>,
        owner: DefId,
    ) -> Result<Arc<ClosureRegionFacts>, TransportError> {
        if let Some(local) = owner.as_local() {
            return Ok(self.collection.local(tcx, local)?);
        }
        let mut imported = self.imported.lock().map_err(|_| TransportError::Poisoned)?;
        if let std::collections::btree_map::Entry::Vacant(entry) = imported.entry(owner.krate) {
            entry.insert(LoadedArtifact::load(tcx, owner.krate, &self.driver_digest)?);
        }
        imported[&owner.krate].get(tcx.def_path_hash(owner))
    }

    /// The only publication entry owns the full compiler run. An analysis
    /// callback cannot publish and a failed run never reaches publication.
    pub fn run_compiler(
        &self,
        args: &[String],
        callbacks: &mut (dyn Callbacks + Send),
        emit_artifacts: bool,
    ) -> Result<(), TransportError> {
        let mut callbacks = CollectingCallbacks {
            inner: callbacks,
            session: self,
            emit_artifacts,
            pending: None,
        };
        // rustc_interface::run_compiler finishes diagnostics and calls
        // abort_if_errors after its closure (including codegen/linking) returns.
        // Its normal return therefore certifies more than after_analysis did.
        rustc_driver::catch_fatal_errors(|| rustc_driver::run_compiler(args, &mut callbacks))
            .map_err(|_| TransportError::RustcRejected)?;
        if let Some(pending) = callbacks.pending {
            pending?.publish()?;
        }
        Ok(())
    }
}

struct CollectingCallbacks<'a> {
    inner: &'a mut (dyn Callbacks + Send),
    session: &'a FactSession,
    emit_artifacts: bool,
    pending: Option<Result<PendingArtifact, TransportError>>,
}

impl Callbacks for CollectingCallbacks<'_> {
    fn config(&mut self, config: &mut rustc_interface::Config) {
        self.inner.config(config);
        self.session
            .collection
            .configure(config)
            .expect("checked driver must delegate query registration to its fact session");
    }

    fn after_crate_root_parsing(
        &mut self,
        compiler: &Compiler,
        krate: &mut rustc_ast::Crate,
    ) -> Compilation {
        self.inner.after_crate_root_parsing(compiler, krate)
    }

    fn after_expansion<'tcx>(&mut self, compiler: &Compiler, tcx: TyCtxt<'tcx>) -> Compilation {
        self.inner.after_expansion(compiler, tcx)
    }

    fn after_analysis<'tcx>(&mut self, compiler: &Compiler, tcx: TyCtxt<'tcx>) -> Compilation {
        let disposition = self.inner.after_analysis(compiler, tcx);
        if self.emit_artifacts && matches!(disposition, Compilation::Continue) {
            self.pending = Some(PendingArtifact::capture(
                tcx,
                &self.session.collection,
                &self.session.driver_digest,
            ));
        }
        disposition
    }
}
