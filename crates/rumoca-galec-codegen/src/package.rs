//! The projection's output artifact: [`AlgorithmCodePackage`].
//!
//! # Construction contract (GAL-002/GAL-004)
//!
//! A package is constructed by the lowering entry point
//! [`crate::lower_to_algorithm_code`], which runs, in order:
//!
//! 1. [`crate::admissibility::check_admissibility`] over the untouched DAE;
//! 2. [`crate::classify::classify_variables`] +
//!    [`crate::manifest_vars::build_manifest_variables`];
//! 3. equation/method-body lowering into the GALEC block (guarded-`f_z`/
//!    `f_m` unwrapping, dependency ordering of the DoStep assignments,
//!    builtin mapping per T8, explicit casts per T5, `'previous(x)'` state
//!    commits at the end of DoStep per T2);
//! 4. post-validation (GAL-004): `rumoca_ir_galec::validate` over the block,
//!    and a full typed-manifest assembly
//!    (`rumoca_efmi::algorithm_code_manifest::AlgorithmCodeManifest::new`
//!    with a render-time UUID/timestamp/checksum) so the manifest fragment
//!    is proven assemblable before the package is returned.
//!
//! The fields are public so the eFMU emission layer can read them, and the
//! rendering facades re-run the GALEC validator on the block
//! ([`crate::emit::render_algorithm_code`] /
//! [`crate::emit::render_manifest_xml`]): a package assembled or mutated
//! outside the lowering entry point cannot print un-validated GALEC.
//!
//! The package never aliases or mutates the input DAE; it owns its data.
//!
//! The manifest side is a *fragment*, not a full
//! `AlgorithmCodeManifestParts`: `ManifestAttributes` (UUID, strict-UTC
//! generation timestamp, tool string) and `Files` checksums are
//! packaging-time facts owned by the eFMU emission layer (`rumoca-compile`
//! driving `rumoca-efmi`), not by the projection.

use rumoca_efmi::Identifier;
use rumoca_efmi::algorithm_code_manifest::{ErrorSignal, Variable as ManifestVariable};

/// The projection-owned parts of the Algorithm Code manifest.
#[derive(Debug, Clone)]
pub struct ManifestFragment {
    /// Ordered manifest `Variables` list (from
    /// [`crate::manifest_vars::build_manifest_variables`] over the kept
    /// classification, plus the synthesized sample-period constant when the
    /// clock wiring adds one; projection-internal condition machinery is
    /// never listed — lowering inlines or rejects references to it).
    pub variables: Vec<ManifestVariable>,
    /// Manifest id of the sample-period `constant` variable the `<Clock>`
    /// element references (GAL-016/D6).
    pub clock_variable_ref_id: Identifier,
    /// Error signals exposable by `Startup` (§3.2.5; escape set computed by
    /// the `rumoca-ir-galec` validator).
    pub startup_signals: Vec<ErrorSignal>,
    /// Error signals exposable by `Recalibrate`.
    pub recalibrate_signals: Vec<ErrorSignal>,
    /// Error signals exposable by `DoStep`.
    pub do_step_signals: Vec<ErrorSignal>,
}

/// A lowered eFMI Algorithm Code package: the GALEC block plus the
/// projection-owned manifest fragment (module docs).
#[derive(Debug, Clone)]
pub struct AlgorithmCodePackage {
    /// The lowered GALEC block, printable via `rumoca_ir_galec::print_block`.
    pub block: rumoca_ir_galec::Block,
    /// Projection-owned manifest content.
    pub manifest: ManifestFragment,
    /// File name of the emitted `.alg` code file (e.g. `Model.alg`),
    /// referenced by the manifest `Files` list at packaging time.
    pub alg_file_name: String,
}
