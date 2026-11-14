//! One actual receiver inventory shared by instances; publication is not entry authority.
//!
//! The product kind is fixed when the guest is built: the default is the
//! stateless native product; `scalar-product` receives a stateful component
//! instead, and each receiver refuses a component of the other kind.
//! `scalar-linked-product` is the compiled stateful product: every program
//! runs as a linked entry and nothing is interpreted.
#[cfg(all(feature = "scalar-product", feature = "scalar-linked-product"))]
compile_error!("the interpreted and the compiled scalar product are exclusive");
#[cfg(not(any(feature = "scalar-product", feature = "scalar-linked-product")))]
use rumoca_exec_wasm::{
    LinkedNativeGuestModel as GuestModel, receive_fixed_native_guest_model as receive,
};
#[cfg(any(feature = "scalar-product", feature = "scalar-linked-product"))]
use rumoca_exec_wasm::{
    LinkedScalarGuestModel as GuestModel, receive_fixed_scalar_guest_model as receive,
};
use rumoca_solver::fmi_me::{MeModelSource, MeRetainedComponent};
use std::cell::OnceCell;

thread_local! {
    static MODEL: OnceCell<Result<GuestModel, String>> = const { OnceCell::new() };
    /// Latched at the exported instantiate entry, before any refusal.
    #[cfg(feature = "preinit")]
    static INSTANTIATED: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
    /// The first instance's component, constructed at pre-initialisation and
    /// moved (never cloned) into the first instance whose token checks.
    #[cfg(feature = "preinit")]
    static PREBUILT: std::cell::RefCell<Option<MeRetainedComponent>> =
        const { std::cell::RefCell::new(None) };
}

fn with_model<T>(query: impl FnOnce(&GuestModel) -> Result<T, String>) -> Result<T, String> {
    MODEL.with(
        |slot| match slot.get_or_init(|| receive().map_err(|error| error.to_string())) {
            Ok(model) => query(model),
            Err(error) => Err(error.clone()),
        },
    )
}

/// Record an instantiation attempt; any attempt ends pre-initialisation.
#[cfg(feature = "preinit")]
pub(super) fn note_instantiation() {
    INSTANTIATED.with(|instantiated| instantiated.set(true));
}

pub(super) fn prepare(token: &str, instance_name: &str) -> Result<MeRetainedComponent, String> {
    with_model(|model| {
        model
            .validate_instantiation_token(token)
            .map_err(|error| error.to_string())?;
        #[cfg(feature = "preinit")]
        if let Some(component) = PREBUILT.with(|slot| slot.borrow_mut().take()) {
            return adopt(component, instance_name);
        }
        construct(model, instance_name)
    })
}

/// The adopted component was constructed before its instance existed; a
/// scalar product binds the instance name now.
#[cfg(feature = "preinit")]
fn adopt(
    mut component: MeRetainedComponent,
    instance_name: &str,
) -> Result<MeRetainedComponent, String> {
    if cfg!(any(
        feature = "scalar-product",
        feature = "scalar-linked-product"
    )) {
        component
            .bind_instance_name(instance_name)
            .map_err(|error| error.to_string())?;
    }
    Ok(component)
}

/// The one construction every instance runs, at instantiation or pre-initialisation.
#[cfg(not(any(feature = "scalar-product", feature = "scalar-linked-product")))]
fn construct(model: &GuestModel, _instance_name: &str) -> Result<MeRetainedComponent, String> {
    MeRetainedComponent::prepare_retained_native_wasm(
        MeModelSource::new(model.component()),
        model.prepared_code(),
    )
    .map_err(|error| error.to_string())
}

/// The start coordinate a scalar instance is constructed at. A host request
/// for another start time is refused at initialization until supported.
#[cfg(any(feature = "scalar-product", feature = "scalar-linked-product"))]
const SCALAR_START_TIME: f64 = 0.0;

/// The one construction every instance runs, at instantiation or
/// pre-initialisation. INTERPRETED (bring-up stage M1a): the explicit
/// `Interpreter` execution policy admits no compiled backend, so every
/// program runs in the Solve IR interpreter and no compiled entry is
/// executed. Linked compiled entries replace this through dispatch (M1b).
#[cfg(feature = "scalar-product")]
fn construct(model: &GuestModel, instance_name: &str) -> Result<MeRetainedComponent, String> {
    use rumoca_solver::SimExecutionPolicy;
    use rumoca_solver::fmi_me::{MeInstanceConfig, admit_execution_backend};
    let backend = admit_execution_backend(SimExecutionPolicy::Interpreter, None)
        .map_err(|error| error.to_string())?;
    let config = MeInstanceConfig::open_ended(
        instance_name,
        model.component().co_simulation().default_tolerance(),
        SCALAR_START_TIME,
    )
    .map_err(|error| error.to_string())?;
    MeRetainedComponent::instantiate(MeModelSource::new(model.component()), &config, backend)
        .map_err(|error| error.to_string())
}

/// The one construction every instance runs, at instantiation or
/// pre-initialisation. COMPILED: the component is the saved product's own
/// checked replay, and its execution backend is compiled-only. Every program
/// the construction requests binds to the entry this guest issues from the
/// same checked source and runs through the issued dispatch; a missing or
/// refused program fails construction. The load closes only when every
/// saved program was bound and the linked artifact certifies against what
/// this guest issued, so a later demand finds no inventory and refuses.
#[cfg(feature = "scalar-linked-product")]
fn construct(model: &GuestModel, instance_name: &str) -> Result<MeRetainedComponent, String> {
    use rumoca_solver::SimExecutionPolicy;
    use rumoca_solver::fmi_me::{MeInstanceConfig, admit_execution_backend};
    let saved = model.saved_product();
    let (load, component) =
        rumoca_sim::load_linked_guest(saved).map_err(|error| error.to_string())?;
    let root = component.runtime_view().retain_assertion_root();
    let backend = load
        .execution_backend(root.model())
        .map_err(|error| error.to_string())?;
    drop(root);
    let backend = admit_execution_backend(SimExecutionPolicy::NativeRequired, Some(backend))
        .map_err(|error| error.to_string())?;
    let config = MeInstanceConfig::open_ended(
        instance_name,
        component.co_simulation().default_tolerance(),
        SCALAR_START_TIME,
    )
    .map_err(|error| error.to_string())?;
    let retained =
        MeRetainedComponent::instantiate(MeModelSource::new(&component), &config, backend)
            .map_err(|error| error.to_string())?;
    load.finish(saved, model.linked_artifact())
        .map_err(|error| error.to_string())?;
    Ok(retained)
}

pub(super) fn default_tolerance() -> Result<f64, String> {
    with_model(|model| Ok(model.component().co_simulation().default_tolerance()))
}

/// Receive the shared model and construct the first instance's component
/// before any instance exists, so a build-time snapshot holds both. Traps after
/// an instantiation, on a second call, or on a receive or construction refusal:
/// the snapshot then fails instead of capturing instance state or an error.
#[cfg(feature = "preinit")]
pub(super) fn preinitialize() {
    assert!(
        !INSTANTIATED.with(std::cell::Cell::get),
        "pre-initialisation after an instantiation"
    );
    // The scalar product binds the real instance name at adoption.
    match with_model(|model| construct(model, "rumoca-preinit")) {
        Ok(component) => PREBUILT.with(|slot| {
            assert!(
                slot.replace(Some(component)).is_none(),
                "pre-initialisation ran twice"
            );
        }),
        Err(error) => panic!("pre-initialisation refused: {error}"),
    }
}
