//! One actual receiver inventory shared by instances; publication is not entry authority.
use rumoca_exec_wasm::{LinkedNativeGuestModel, receive_fixed_native_guest_model};
use rumoca_solver::fmi_me::{MeModelSource, MeRetainedComponent};
use std::cell::{Cell, OnceCell};

thread_local! {
    static MODEL: OnceCell<Result<LinkedNativeGuestModel, String>> = const { OnceCell::new() };
    static INSTANTIATED: Cell<bool> = const { Cell::new(false) };
}

fn with_model<T>(
    query: impl FnOnce(&LinkedNativeGuestModel) -> Result<T, String>,
) -> Result<T, String> {
    MODEL.with(|slot| {
        match slot
            .get_or_init(|| receive_fixed_native_guest_model().map_err(|error| error.to_string()))
        {
            Ok(model) => query(model),
            Err(error) => Err(error.clone()),
        }
    })
}

pub(super) fn prepare(token: &str) -> Result<MeRetainedComponent, String> {
    INSTANTIATED.with(|instantiated| instantiated.set(true));
    with_model(|model| {
        model
            .validate_instantiation_token(token)
            .map_err(|error| error.to_string())?;
        MeRetainedComponent::prepare_retained_native_wasm(
            MeModelSource::new(model.component()),
            model.prepared_code(),
        )
        .map_err(|error| error.to_string())
    })
}

pub(super) fn default_tolerance() -> Result<f64, String> {
    with_model(|model| Ok(model.component().co_simulation().default_tolerance()))
}

/// Receive the shared model once, before any instance exists, so a build-time
/// snapshot holds it. Traps after an instantiation or on a receive refusal:
/// the snapshot then fails instead of capturing instance state or an error.
#[cfg(feature = "preinit")]
pub(super) fn preinitialize() {
    assert!(
        !INSTANTIATED.with(Cell::get),
        "pre-initialisation after an instantiation"
    );
    if let Err(error) = with_model(|_| Ok(())) {
        panic!("pre-initialisation refused: {error}");
    }
}
