//! Source pairing for future direct-native FMI-LS assembly (SPEC_0044 ME-LSW-003b).

use rumoca_exec_wasm::NativeModelLibrary;

#[cfg(all(not(target_arch = "wasm32"), feature = "native-fmi-ls-build"))]
mod data;
#[cfg(all(not(target_arch = "wasm32"), feature = "native-fmi-ls-build"))]
mod linking;
#[cfg(all(not(target_arch = "wasm32"), feature = "native-fmi-ls-build"))]
mod publication;
#[cfg(all(not(target_arch = "wasm32"), feature = "native-fmi-ls-build"))]
pub use linking::LinkedNativeFmiLsComponent;

/// The existing canonical source projection and encoding failure owners.
#[derive(Debug, thiserror::Error)]
pub enum NativeFmiLsPreparationError {
    #[error(transparent)]
    Source(#[from] rumoca_eval_solve::fmi_wire::FmiWireError),
    #[error(transparent)]
    Encoding(#[from] serde_json::Error),
    #[error("native FMI-LS graph: {0}")]
    Linkage(String),
}

/// Original emitted library paired with its internally encoded source capsule.
/// This product grants preparation only, without engine or FMI publication admission.
///
/// ```compile_fail
/// use rumoca_exec_wasm::NativeModelLibrary;
/// use rumoca_sim::PreparedNativeFmiLsModel;
/// fn forge(library: NativeModelLibrary, capsule: Vec<u8>) -> PreparedNativeFmiLsModel {
///     PreparedNativeFmiLsModel { library, capsule }
/// }
/// ```
pub struct PreparedNativeFmiLsModel {
    library: NativeModelLibrary,
    capsule: Vec<u8>,
}

impl PreparedNativeFmiLsModel {
    /// Encode only the source root retained by the consumed checked library.
    pub fn from_library(library: NativeModelLibrary) -> Result<Self, NativeFmiLsPreparationError> {
        let source =
            rumoca_eval_solve::fmi_wire::retained_fmi_component_wire(library.inputs().root())?;
        let capsule = serde_json::to_vec(&source)?;
        Ok(Self { library, capsule })
    }

    pub fn library(&self) -> &NativeModelLibrary {
        &self.library
    }

    /// The canonical, uncompressed source wire.
    pub fn capsule_bytes(&self) -> &[u8] {
        &self.capsule
    }
}

#[cfg(test)]
mod tests;
