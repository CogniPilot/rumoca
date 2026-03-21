use rumoca_session::compile::{DaeCompilationResult, Session};
use tokio::sync::Mutex;

#[derive(Debug, Default)]
pub(super) struct ServerWorkLanes {
    pub(super) interactive: Mutex<()>,
    pub(super) strict: Mutex<()>,
    pub(super) indexing: Mutex<()>,
}

#[derive(Debug, Clone)]
pub(super) struct StrictSessionSnapshot {
    session: Session,
}

impl StrictSessionSnapshot {
    pub(super) fn new(session: Session) -> Self {
        Self { session }
    }

    pub(super) fn compile_model(
        self,
        model: &str,
    ) -> std::result::Result<Box<DaeCompilationResult>, String> {
        let mut isolated_session = self.session;
        compile_model_in_isolated_session(&mut isolated_session, model)
    }

    pub(super) fn compile_models(
        self,
        models: &[String],
    ) -> Vec<(
        String,
        std::result::Result<Box<DaeCompilationResult>, String>,
    )> {
        let mut isolated_session = self.session;
        models
            .iter()
            .map(|model| {
                (
                    model.clone(),
                    compile_model_in_isolated_session(&mut isolated_session, model),
                )
            })
            .collect()
    }
}

fn compile_model_in_isolated_session(
    isolated_session: &mut Session,
    model: &str,
) -> std::result::Result<Box<DaeCompilationResult>, String> {
    // The LSP server already owns a simulation compile cache keyed by model,
    // source fingerprint, and source-root epoch, so the isolated strict path
    // should reuse lower-stage query artifacts and return only the DAE-stage
    // result that simulation needs.
    isolated_session.compile_model_dae_strict_reachable_uncached_with_recovery(model)
}
