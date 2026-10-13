//! One actual receiver inventory shared by instances; publication is not entry authority.
use rumoca_exec_wasm::{LinkedNativeGuestModel, receive_fixed_native_guest_model};
use rumoca_solver::fmi_me::{MeModelSource, MeRetainedComponent};
use std::cell::OnceCell;

thread_local! {
    static MODEL: OnceCell<Result<LinkedNativeGuestModel, String>> = const { OnceCell::new() };
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
