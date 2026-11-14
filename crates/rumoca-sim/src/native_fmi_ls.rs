//! Source pairing for future direct-native FMI-LS assembly (SPEC_0044 ME-LSW-003b).

use rumoca_exec_wasm::NativeModelLibrary;

#[cfg(all(not(target_arch = "wasm32"), feature = "native-fmi-ls-build"))]
mod data;
#[cfg(all(not(target_arch = "wasm32"), feature = "native-fmi-ls-build"))]
mod linking;
#[cfg(all(not(target_arch = "wasm32"), feature = "native-fmi-ls-build"))]
mod publication;
#[cfg(all(not(target_arch = "wasm32"), feature = "native-fmi-ls-build"))]
pub use linking::{LinkedGuestParts, LinkedNativeFmiLsComponent, LinkedScalarFmiLsComponent};

/// The existing canonical source projection and encoding failure owners.
#[derive(Debug, thiserror::Error)]
pub enum NativeFmiLsPreparationError {
    #[error(transparent)]
    Source(#[from] rumoca_eval_solve::fmi_wire::FmiWireError),
    #[error(transparent)]
    Encoding(#[from] serde_json::Error),
    #[error("native FMI-LS graph: {0}")]
    Linkage(String),
    #[error("FMI-LS product kind: {0}")]
    WrongProduct(&'static str),
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

/// A stateful (scalar-product) component's retained root paired with its
/// internally encoded source capsule: the same canonical wire as the native
/// product, without a native library. Preparation only, as the native pair.
///
/// ```compile_fail
/// use rumoca_ir_solve::RetainedAssertionRoot;
/// use rumoca_sim::PreparedScalarFmiLsModel;
/// fn forge(root: RetainedAssertionRoot, capsule: Vec<u8>) -> PreparedScalarFmiLsModel {
///     PreparedScalarFmiLsModel { root, capsule }
/// }
/// ```
pub struct PreparedScalarFmiLsModel {
    root: rumoca_ir_solve::RetainedAssertionRoot,
    capsule: Vec<u8>,
}

impl PreparedScalarFmiLsModel {
    /// Encode the component's own retained root with the canonical wire.
    pub fn from_component(
        component: &rumoca_ir_solve::fmi::FmiComponent,
    ) -> Result<Self, NativeFmiLsPreparationError> {
        let root = component.runtime_view().retain_assertion_root();
        // The same predicate the scalar guest receiver applies at
        // instantiation: a root the stateless native profile admits belongs to
        // the native product. A native-refused root may be scalar, stateful or
        // not, so the rule is the complement, never a state count.
        if root.native_preparation().is_ok() {
            return Err(NativeFmiLsPreparationError::WrongProduct(
                "a component the stateless native profile admits belongs to the native product, not the scalar product",
            ));
        }
        let capsule = serde_json::to_vec(
            &rumoca_eval_solve::fmi_wire::retained_fmi_component_wire(&root)?,
        )?;
        Ok(Self { root, capsule })
    }

    pub fn root(&self) -> &rumoca_ir_solve::RetainedAssertionRoot {
        &self.root
    }

    /// The canonical, uncompressed source wire.
    pub fn capsule_bytes(&self) -> &[u8] {
        &self.capsule
    }
}

#[cfg(test)]
mod tests;
