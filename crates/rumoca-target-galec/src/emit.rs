//! Rendering facades over a lowered [`AlgorithmCodePackage`].
//!
//! - [`render_algorithm_code`] prints the `.alg` text via the
//!   `rumoca-galec` printer (GAL-009);
//! - [`render_manifest_xml`] assembles and serializes a complete Algorithm
//!   Code manifest via the typed `rumoca-efmi` models (D3). The projection
//!   owns the manifest *fragment*; packaging-time facts are filled with
//!   real values generated at render time — a fresh manifest UUID, the
//!   current strict-UTC timestamp, and the actual SHA-1 of the rendered
//!   `.alg` bytes (GAL-021: no placeholder checksums, ever). The eFMU
//!   container emission layer (Phase 5b, `rumoca-compile` driving
//!   `rumoca-efmi`) supersedes this facade for full-container output where
//!   one id/timestamp must be shared across manifests;
//! - [`c_template_context`] serializes the typed minimal context a future
//!   embedded-C minijinja template consumes (GAL-008). **Unstable until
//!   slice 5c**: the shape will grow with the C template work and is not a
//!   compatibility surface yet.

use serde::Serialize;

use rumoca_efmi::algorithm_code_manifest::{
    AlgorithmCodeManifest, AlgorithmCodeManifestParts, BlockMethod, BlockMethods, Clock,
    ErrorSignalStatus, Variable as ManifestVariable,
};
use rumoca_efmi::manifest_common::{File, FileChecksum, FileRole, ManifestAttributes};
use rumoca_efmi::{
    FilePath, Identifier, ManifestId, NameWithoutSlashes, NormalizedText, Sha1Hex, UtcTimestamp,
};
use rumoca_galec::ast::{Block, Name, ScalarType, Statement};

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
    if let Err(diagnostics) = rumoca_galec::validate(block) {
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
    render_block(block)
}

pub(crate) fn render_block(block: &Block) -> Result<String, GalecTargetError> {
    rumoca_galec::print_block(block).map_err(|error| GalecTargetError::LoweringInternal {
        detail: format!("GALEC printer rejected the lowered block: {error}"),
    })
}

/// Render the package's Algorithm Code manifest as XML (module docs). The
/// block is re-validated before printing, exactly as in
/// [`render_algorithm_code`] (the `.alg` checksum must never be computed
/// over un-validated text).
///
/// # Errors
///
/// `ET016` when the typed manifest model rejects the assembled parts,
/// `ET018` for validator/printer failures.
pub fn render_manifest_xml(package: &AlgorithmCodePackage) -> Result<String, GalecTargetError> {
    let alg_text = validated_alg_text(&package.block)?;
    let manifest = assemble_manifest(package, alg_text.as_bytes())?;
    let bytes = rumoca_efmi::algorithm_code_manifest_to_xml(&manifest)?;
    String::from_utf8(bytes).map_err(|error| GalecTargetError::LoweringInternal {
        detail: format!("manifest XML is not valid UTF-8: {error}"),
    })
}

/// Assemble the full typed manifest from the projection-owned fragment
/// plus render-time packaging facts (also the GAL-004 post-validation
/// path for the manifest side).
pub(crate) fn assemble_manifest(
    package: &AlgorithmCodePackage,
    alg_bytes: &[u8],
) -> Result<AlgorithmCodeManifest, GalecTargetError> {
    let fragment = &package.manifest;
    let file_id = Identifier::new("F_ALG")?;
    let parts = AlgorithmCodeManifestParts {
        attributes: ManifestAttributes {
            id: ManifestId::generate(),
            name: NormalizedText::new(block_display_name(&package.block.name))?,
            description: None,
            version: None,
            generation_date_and_time: UtcTimestamp::now_utc(),
            generation_tool: Some(NormalizedText::new(format!(
                "rumoca {}",
                env!("CARGO_PKG_VERSION")
            ))?),
            copyright: None,
            license: None,
        },
        file_ref_id: file_id.clone(),
        files: vec![File {
            id: file_id,
            name: NameWithoutSlashes::new(package.alg_file_name.clone())?,
            path: FilePath::root(),
            checksum: FileChecksum::Sha1(Sha1Hex::of_bytes(alg_bytes)),
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

fn block_display_name(name: &Name) -> String {
    crate::mangle::manifest_name(name).to_owned()
}

// ---------------------------------------------------------------------------
// C template context (unstable until slice 5c)
// ---------------------------------------------------------------------------

/// Typed template context (serialized shape of [`c_template_context`]).
#[derive(Serialize)]
struct CContext {
    /// Manifest spelling of the block name.
    block_name: String,
    /// Manifest-listed variables with C-ish type info.
    variables: Vec<CVariable>,
    /// Method bodies as GALEC-printed assignment data.
    methods: CMethods,
}

#[derive(Serialize)]
struct CVariable {
    /// Manifest spelling (quoted-identifier content for quoted names).
    name: String,
    /// Manifest id (`V1`…).
    id: String,
    /// Manifest `blockCausality` literal.
    causality: &'static str,
    /// C scalar type the variable maps to.
    c_type: &'static str,
    /// Dimension sizes (empty = scalar).
    dimensions: Vec<u64>,
}

#[derive(Serialize)]
struct CMethods {
    startup: Vec<CStatement>,
    recalibrate: Vec<CStatement>,
    do_step: Vec<CStatement>,
}

/// One lowered statement. Slice 1 emits assignments only; the statement
/// kinds grow with the C-template slice.
#[derive(Serialize)]
struct CStatement {
    kind: &'static str,
    /// GALEC-printed assignment target (e.g. `self.'previous(x)'`).
    target: String,
    /// GALEC-printed value expression.
    value: String,
}

/// Serialize the typed minimal C-template context (module docs; unstable
/// until slice 5c).
///
/// # Errors
///
/// `ET018` when the package contains statement kinds the context does not
/// model yet or the printer rejects an expression.
pub fn c_template_context(
    package: &AlgorithmCodePackage,
) -> Result<serde_json::Value, GalecTargetError> {
    let variables = package
        .manifest
        .variables
        .iter()
        .map(|variable| {
            let common = variable.common();
            CVariable {
                name: common.name.as_str().to_owned(),
                id: common.id.as_str().to_owned(),
                causality: common.block_causality.as_str(),
                c_type: c_scalar_type(variable),
                dimensions: common.dimensions.clone(),
            }
        })
        .collect();
    let context = CContext {
        block_name: block_display_name(&package.block.name),
        variables,
        methods: CMethods {
            startup: statements(&package.block.startup.statements)?,
            recalibrate: statements(&package.block.recalibrate.statements)?,
            do_step: statements(&package.block.do_step.statements)?,
        },
    };
    serde_json::to_value(&context).map_err(|error| GalecTargetError::LoweringInternal {
        detail: format!("C template context serialization failed: {error}"),
    })
}

fn c_scalar_type(variable: &ManifestVariable) -> &'static str {
    match variable {
        ManifestVariable::Real(_) => scalar_c_type(ScalarType::Real),
        ManifestVariable::Integer(_) => scalar_c_type(ScalarType::Integer),
        ManifestVariable::Boolean(_) => scalar_c_type(ScalarType::Boolean),
    }
}

fn scalar_c_type(scalar: ScalarType) -> &'static str {
    match scalar {
        ScalarType::Real => "double",
        ScalarType::Integer => "int32_t",
        ScalarType::Boolean => "bool",
    }
}

fn statements(block_statements: &[Statement]) -> Result<Vec<CStatement>, GalecTargetError> {
    block_statements
        .iter()
        .map(|statement| match statement {
            Statement::Assignment { target, value } => Ok(CStatement {
                kind: "assignment",
                target: print_reference(target)?,
                value: print_expression(value)?,
            }),
            other => Err(GalecTargetError::LoweringInternal {
                detail: format!(
                    "C template context does not model this statement kind yet: {other:?}"
                ),
            }),
        })
        .collect()
}

fn print_expression(
    expression: &rumoca_galec::ast::Expression,
) -> Result<String, GalecTargetError> {
    rumoca_galec::print_expression(expression).map_err(|error| GalecTargetError::LoweringInternal {
        detail: format!("GALEC printer rejected an expression: {error}"),
    })
}

fn print_reference(reference: &rumoca_galec::ast::Reference) -> Result<String, GalecTargetError> {
    print_expression(&rumoca_galec::ast::Expression::Ref(reference.clone()))
}
