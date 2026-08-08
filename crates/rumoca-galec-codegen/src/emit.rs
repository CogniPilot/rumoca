//! Rendering facades over a lowered [`AlgorithmCodePackage`].
//!
//! - [`render_algorithm_code`] prints the `.alg` text via the
//!   `rumoca-ir-galec` printer (GAL-009);
//! - [`assemble_manifest_with_identity`] assembles a
//!   complete typed Algorithm Code manifest ([`crate::manifest_context`] models,
//!   D3 amended) from the lowered package plus the real SHA-1 of the
//!   rendered `.alg` bytes (GAL-021: no placeholder checksums, ever). The
//!   projection owns the manifest *fragment*; the container build step in
//!   the `rumoca` crate mints one id/timestamp per invocation and threads
//!   it through [`assemble_manifest_with_identity`] so a single identity is
//!   shared across manifests. The manifest carries **no XML text** — the
//!   product-agnostic [`crate::manifest_context::views`] serialize it and the
//!   minijinja templates own every element/attribute (SPEC_0034 D3 amended);
//! - [`c_template_context`] serializes the typed context the
//!   `embedded-c-galec` minijinja templates consume (GAL-008/GAL-024):
//!   C-mangled struct/function naming, per-variable C types + field names,
//!   and C-printed method statement lines.

use crate::manifest_context::algorithm_code_manifest::{
    AlgorithmCodeManifest, AlgorithmCodeManifestParts, BlockMethod, BlockMethods, Clock,
    ErrorSignalStatus, Variable as ManifestVariable,
};
use crate::manifest_context::manifest_common::{File, FileChecksum, FileRole, ManifestAttributes};
use crate::manifest_context::production_code_manifest::TargetTypeKind;
use crate::manifest_context::{
    FilePath, Identifier, ManifestId, NameWithoutSlashes, NormalizedText, Sha1Hex, UtcTimestamp,
};
use rumoca_ir_galec::ast::{Block, Name, ScalarType};

use crate::diagnostic::GalecTargetError;
use crate::package::AlgorithmCodePackage;

/// Render the package's `.alg` program text (GAL-009).
///
/// The GALEC validator is re-run on the block first, so GAL-004
/// post-validation holds on every rendering path by construction — the
/// package fields are public, and a package assembled or mutated outside
/// [`crate::lower_to_algorithm_code`] must not print un-validated GALEC.
///
/// # Errors
///
/// `ET018` for validator or printer rejections; for a package produced by
/// the lowering entry point both are projection bugs.
pub fn render_algorithm_code(package: &AlgorithmCodePackage) -> Result<String, GalecTargetError> {
    validated_alg_text(&package.block)
}

/// Validate, then print (see [`render_algorithm_code`]).
fn validated_alg_text(block: &Block) -> Result<String, GalecTargetError> {
    validate_block(block)?;
    render_block(block)
}

/// GAL-004 pre-render validation shared by every rendering facade
/// (`.alg`, manifest XML, the C template context, and the Production Code
/// manifest builder in [`crate::production_manifest`]).
pub(crate) fn validate_block(block: &Block) -> Result<(), GalecTargetError> {
    if let Err(diagnostics) = rumoca_ir_galec::validate(block) {
        let rendered: Vec<String> = diagnostics
            .iter()
            .map(std::string::ToString::to_string)
            .collect();
        return Err(GalecTargetError::LoweringInternal {
            detail: format!(
                "package block failed GALEC validation before rendering (was \
                 the package modified after lowering?): {}",
                rendered.join("; ")
            ),
        });
    }
    Ok(())
}

/// The embedded `.alg` walking template (SPEC_0034 D17): renders the
/// language-neutral template IR as conformant GALEC, byte-identical to the
/// `rumoca-ir-galec` typed printer (pinned by the parity test in
/// `tests/spec_0034_estimator.rs` — the printer stays the parser-facing
/// half of the language module; this template owns emission).
static ALG_TEMPLATE: &str = include_str!("templates/alg.jinja");

pub(crate) fn render_block(block: &Block) -> Result<String, GalecTargetError> {
    let context = crate::template_ir::galec_template_context_for_block(
        block,
        &block_display_name(&block.name),
    )?;
    let mut env = minijinja::Environment::new();
    env.set_undefined_behavior(minijinja::UndefinedBehavior::Strict);
    // The typed printer ends every file with a newline; minijinja strips it
    // by default (byte parity, D17).
    env.set_keep_trailing_newline(true);
    env.add_function(
        "fail",
        |message: String| -> Result<minijinja::Value, minijinja::Error> {
            Err(minijinja::Error::new(
                minijinja::ErrorKind::InvalidOperation,
                message,
            ))
        },
    );
    env.render_str(ALG_TEMPLATE, minijinja::Value::from_serialize(&context))
        .map_err(|error| GalecTargetError::LoweringInternal {
            detail: format!("alg template rejected the lowered block: {error}"),
        })
}

/// Shared, minted-once packaging identity of one manifest (contract §2b /
/// §4d): the brace-wrapped UUID, the strict-UTC timestamp, and the tool
/// string. The switch-dispatch build step (`rumoca` crate, contract §9
/// WI-5) mints these once per invocation and threads the *same* identity
/// into the Algorithm Code manifest, the Production Code manifest, and
/// `__content.xml`, so the cross-manifest `manifestRefId` links agree by
/// construction (never a template mint, never a re-parse).
#[derive(Debug, Clone)]
pub struct ManifestIdentity {
    /// Brace-wrapped manifest UUID.
    pub id: ManifestId,
    /// Strict-UTC generation timestamp.
    pub generated_at: UtcTimestamp,
    /// Generation-tool string (e.g. `"rumoca X.Y.Z"`).
    pub generation_tool: Option<NormalizedText>,
}

impl ManifestIdentity {
    /// Mint a fresh identity: a new UUID, the current UTC time, and this
    /// crate's `rumoca X.Y.Z` tool string (the documented per-build
    /// nondeterminism of a generated container).
    ///
    /// # Errors
    ///
    /// `ET018` if the constant tool string fails `NormalizedText` validation
    /// (impossible for a well-formed `CARGO_PKG_VERSION`).
    pub fn generated() -> Result<Self, GalecTargetError> {
        Ok(Self {
            id: ManifestId::generate(),
            generated_at: UtcTimestamp::now_utc(),
            generation_tool: Some(NormalizedText::new(format!(
                "rumoca {}",
                env!("CARGO_PKG_VERSION")
            ))?),
        })
    }

    /// A deterministic, identity-FREE placeholder identity (nil UUID, fixed
    /// timestamp, no tool string). For assembly whose *only* purpose is to
    /// prove the manifest fragment validates (the GAL-004 post-validation
    /// path) — it does not mint a real identity, so it calls neither
    /// `Uuid::new_v4` nor `SystemTime::now` and stays safe on `wasm32` (the
    /// identity-free render contract the WASM addon relies on). The minted
    /// identity for a real container comes from [`Self::generated`], threaded
    /// in via [`assemble_manifest_with_identity`].
    #[must_use]
    pub fn placeholder() -> Self {
        Self {
            id: ManifestId::nil(),
            generated_at: UtcTimestamp::placeholder(),
            generation_tool: None,
        }
    }
}

/// Assemble the full typed manifest from the projection-owned fragment plus
/// the `.alg` bytes, using an IDENTITY-FREE placeholder identity. This is the
/// GAL-004 post-validation path (and the test-assembly helper): it only proves
/// the manifest fragment assembles into a valid typed manifest, so it must NOT
/// mint a real UUID/timestamp — that keeps the shared lowering/render path free
/// of `Uuid::new_v4`/`SystemTime::now` and safe on `wasm32`. A real container
/// gets its minted, shared identity via [`assemble_manifest_with_identity`].
pub(crate) fn assemble_manifest(
    package: &AlgorithmCodePackage,
    alg_bytes: &[u8],
) -> Result<AlgorithmCodeManifest, GalecTargetError> {
    assemble_manifest_with_identity(
        package,
        Sha1Hex::of_bytes(alg_bytes),
        &ManifestIdentity::placeholder(),
    )
}

/// Assemble the full typed Algorithm Code manifest with a caller-supplied
/// identity and a caller-computed `.alg` SHA-1 (contract §4c: the digest is
/// of the exact rendered `.alg` bytes; no placeholder). The switch-dispatch
/// build step passes the shared [`ManifestIdentity`] so the manifest's UUID
/// matches the `__content.xml` `manifestRefId` and the Production Code
/// `ManifestReference/@manifestRefId`.
///
/// # Errors
///
/// `ET016` when the typed manifest model rejects the assembled parts,
/// `ET018` for validator/printer failures.
pub fn assemble_manifest_with_identity(
    package: &AlgorithmCodePackage,
    alg_checksum: Sha1Hex,
    identity: &ManifestIdentity,
) -> Result<AlgorithmCodeManifest, GalecTargetError> {
    let fragment = &package.manifest;
    let file_id = Identifier::new("F_ALG")?;
    let parts = AlgorithmCodeManifestParts {
        attributes: ManifestAttributes {
            id: identity.id,
            name: NormalizedText::new(block_display_name(&package.block.name))?,
            description: None,
            version: None,
            generation_date_and_time: identity.generated_at,
            generation_tool: identity.generation_tool.clone(),
            copyright: None,
            license: None,
        },
        file_ref_id: file_id.clone(),
        files: vec![File {
            id: file_id,
            name: NameWithoutSlashes::new(package.alg_file_name.clone())?,
            path: FilePath::root(),
            checksum: FileChecksum::Sha1(alg_checksum),
            role: FileRole::Code,
            description: None,
        }],
        clock: Clock {
            id: Identifier::new("CLK")?,
            variable_ref_id: fragment.clock_variable_ref_id.clone(),
        },
        block_methods: BlockMethods {
            startup: BlockMethod {
                id: Identifier::new("BM_STARTUP")?,
                signals: fragment.startup_signals.clone(),
            },
            recalibrate: BlockMethod {
                id: Identifier::new("BM_RECALIBRATE")?,
                signals: fragment.recalibrate_signals.clone(),
            },
            do_step: BlockMethod {
                id: Identifier::new("BM_DOSTEP")?,
                signals: fragment.do_step_signals.clone(),
            },
        },
        error_signal_status: ErrorSignalStatus {
            id: Identifier::new("ESS")?,
        },
        units: Vec::new(),
        variables: fragment.variables.clone(),
        annotations: Vec::new(),
    };
    AlgorithmCodeManifest::new(parts).map_err(GalecTargetError::from)
}

pub(crate) fn block_display_name(name: &Name) -> String {
    crate::mangle::manifest_name(name).to_owned()
}

// ---------------------------------------------------------------------------
// C template context (the `embedded-c-galec` target, GAL-024/D17)
// ---------------------------------------------------------------------------

/// The template-walkable context the `embedded-c-galec` walking templates
/// consume (SPEC_0034 D16/D17) — the same language-neutral GALEC block
/// context every GALEC-rendering target shares
/// ([`crate::template_ir::galec_template_context`]); the C templates own
/// every C spelling.
///
/// # Errors
///
/// `ET022` on base-name collisions, `ET023` for GALEC constructs the
/// export shape does not cover, `ET018` for validator rejections.
pub fn c_template_context(
    package: &AlgorithmCodePackage,
    model_name: &str,
) -> Result<serde_json::Value, GalecTargetError> {
    crate::template_ir::galec_template_context(package, model_name)
}

/// [`c_template_context`] directly from parsed GALEC Algorithm Code — the
/// browser/editor path for `.alg -> .h/.c` (positional ids and
/// declaration-kind causalities synthesized).
///
/// # Errors
///
/// Those of [`c_template_context`].
pub fn c_template_context_for_block(
    block: &Block,
    model_name: &str,
) -> Result<serde_json::Value, GalecTargetError> {
    crate::template_ir::galec_template_context_for_block(block, model_name)
}

/// Reject block shapes the current lowering never produces before any C is
/// printed (GAL-007: loud `ET023`, never a silent drop). Kept in lockstep
/// with [`crate::lower`]: it emits no compartments, user functions, error
/// signals, method locals, or method `signals` clauses. Shared with the
/// Production Code manifest builder ([`crate::production_manifest`]), whose
/// three-void-functions-plus-`self` description assumes exactly this shape.
pub(crate) fn ensure_c_exportable(block: &Block) -> Result<(), GalecTargetError> {
    crate::template_ir::reject_unrepresented(block)
}

/// Whether the generated C methods return the 32-bit ErrorSignalStatus word
/// (GAL-029): any declared method escape switches the whole block to the
/// status ABI (uniform signatures; signal-free methods return 0).
pub(crate) fn status_abi(block: &Block) -> bool {
    [&block.startup, &block.recalibrate, &block.do_step]
        .iter()
        .any(|method| !method.signals.is_empty())
}

/// The GALEC scalar type of a manifest variable (the manifest variable kinds
/// are exactly the GALEC scalar types — eFMI has no String variables).
pub(crate) fn manifest_scalar_type(variable: &ManifestVariable) -> ScalarType {
    match variable {
        ManifestVariable::Real(_) => ScalarType::Real,
        ManifestVariable::Integer(_) => ScalarType::Integer,
        ManifestVariable::Boolean(_) => ScalarType::Boolean,
    }
}

/// The single GALEC-scalar → C binding of the embedded C export: the eFMI
/// target-type kind and the C type token. Shared by the C template context
/// (struct field types) and the Production Code manifest builder
/// ([`crate::production_manifest`]: `TargetType@kind`/`@codedType` and the
/// alias `Typedef@name`), so the manifest describes the generated C in
/// lockstep by construction, not by comment.
pub(crate) fn c_scalar_binding(scalar: ScalarType) -> (TargetTypeKind, &'static str) {
    match scalar {
        ScalarType::Real => (TargetTypeKind::EfmiFloat64, "double"),
        ScalarType::Integer => (TargetTypeKind::EfmiInteger32, "int32_t"),
        ScalarType::Boolean => (TargetTypeKind::EfmiBool, "bool"),
    }
}

/// Make traceability text safe inside a C block comment by breaking both the
/// comment-close `*/` (into `* /`) and the comment-open `/*` (into `/ *`).
/// Modelica quoted identifiers may legally contain either sequence
/// ([`crate::mangle::check_quotable`] only rejects quotes, whitespace, and
/// control characters), and the templates put these fields in comments: an
/// unbroken `*/` would terminate the comment (a `cc` syntax error), and an
/// embedded `/*` fires `-Wcomment` under the mandated `cc -Wall -Werror`
/// (GAL-012: failures belong in rumoca, generated C must compile). Comments
/// are non-normative, so the inserted spaces are display-only; C identifiers
/// go through [`crate::c_mangle`] and never through this.
pub(crate) fn c_comment_text(text: &str) -> String {
    text.replace("*/", "* /").replace("/*", "/ *")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn comment_text_cannot_terminate_or_open_a_c_block_comment() {
        assert_eq!(c_comment_text("motor.emf.v"), "motor.emf.v");
        // The comment-close `*/` is broken...
        assert_eq!(c_comment_text("a*/b"), "a* /b");
        // ...and the comment-open `/*` too (fires -Wcomment under -Wall -Werror).
        assert_eq!(c_comment_text("a/*b"), "a/ *b");
        // Adjacent sequences: both are broken, including the `/*` the first
        // replacement leaves behind between two broken `*/`s.
        assert_eq!(c_comment_text("a*/*/b"), "a* / * /b");
        for hazard in ["**/", "a/*b", "x*/*y", "'/*'", "*/", "/*"] {
            let safe = c_comment_text(hazard);
            assert!(!safe.contains("*/"), "`{hazard}` -> `{safe}` still closes");
            assert!(!safe.contains("/*"), "`{hazard}` -> `{safe}` still opens");
        }
    }
}
