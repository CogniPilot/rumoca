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
//!   `galec-c` minijinja templates consume (GAL-008/GAL-024):
//!   C-mangled struct/function naming, per-variable C types + field names,
//!   and C-printed method statement lines.

use serde::Serialize;

use crate::manifest_context::algorithm_code_manifest::{
    AlgorithmCodeManifest, AlgorithmCodeManifestParts, BlockMethod, BlockMethods, Clock,
    ErrorSignalStatus, Variable as ManifestVariable,
};
use crate::manifest_context::manifest_common::{File, FileChecksum, FileRole, ManifestAttributes};
use crate::manifest_context::production_code_manifest::TargetTypeKind;
use crate::manifest_context::{
    FilePath, Identifier, ManifestId, NameWithoutSlashes, NormalizedText, Sha1Hex, UtcTimestamp,
};
use rumoca_ir_galec::ast::{
    Associativity, Block, Dimension, Expression, InterfaceKind, Name, PrecedenceClass,
    ProtectedKind, Reference, ScalarType, Spanned, Statement, TypeRef, VariableDeclaration,
};

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

pub(crate) fn render_block(block: &Block) -> Result<String, GalecTargetError> {
    rumoca_ir_galec::print_block(block).map_err(|error| GalecTargetError::LoweringInternal {
        detail: format!("GALEC printer rejected the lowered block: {error}"),
    })
}

/// Serialize the validated GALEC block as language-neutral codegen data.
///
/// Unlike [`render_algorithm_code`], this is the production codegen API:
/// declarations, references, expressions, statements, and block layout remain
/// structured so `model.alg.jinja` owns their textual spelling.
pub fn algorithm_template_context(
    package: &AlgorithmCodePackage,
) -> Result<serde_json::Value, GalecTargetError> {
    validate_block(&package.block)?;
    let block = &package.block;
    if !block.compartments.is_empty()
        || !block.error_signals.is_empty()
        || !block.protected_functions.is_empty()
        || !block.public_functions.is_empty()
    {
        return Err(GalecTargetError::LoweringInternal {
            detail: "the DAE projection produced a GALEC construct outside the current template codegen IR"
                .to_owned(),
        });
    }
    Ok(serde_json::json!({
        "name": name_context(&block.name),
        "interface": block.interface.iter().map(|variable| {
            Ok(serde_json::json!({
                "prefix": match variable.kind {
                    InterfaceKind::Input => "input",
                    InterfaceKind::Output => "output",
                    InterfaceKind::TunableParameter => "parameter",
                },
                "declaration": declaration_context(&variable.decl)?,
            }))
        }).collect::<Result<Vec<_>, GalecTargetError>>()?,
        "protected": block.protected.iter().map(|entity| {
            Ok(serde_json::json!({
                "prefix": match entity.kind {
                    ProtectedKind::DependentParameter => "parameter",
                    ProtectedKind::Constant => "constant",
                    ProtectedKind::State => "",
                },
                "declaration": declaration_context(&entity.decl)?,
            }))
        }).collect::<Result<Vec<_>, GalecTargetError>>()?,
        "methods": [
            method_context("Startup", &block.startup)?,
            method_context("Recalibrate", &block.recalibrate)?,
            method_context("DoStep", &block.do_step)?,
        ],
    }))
}

fn method_context(
    name: &'static str,
    method: &rumoca_ir_galec::ast::BlockMethod,
) -> Result<serde_json::Value, GalecTargetError> {
    if !method.signals.is_empty() || !method.locals.is_empty() {
        return Err(GalecTargetError::LoweringInternal {
            detail: format!("projected method {name} contains unsupported template context data"),
        });
    }
    Ok(serde_json::json!({
        "name": name,
        "statements": method.statements.iter().map(|statement| {
            match &statement.node {
                Statement::Assignment { target, value } => Ok(serde_json::json!({
                    "kind": "assignment",
                    "target": alg_reference_context(target)?,
                    "value": alg_expression_context(value)?,
                })),
                other => Err(GalecTargetError::LoweringInternal {
                    detail: format!("projected method {name} contains unsupported statement {other:?}"),
                }),
            }
        }).collect::<Result<Vec<_>, GalecTargetError>>()?,
    }))
}

fn declaration_context(decl: &VariableDeclaration) -> Result<serde_json::Value, GalecTargetError> {
    let ty = match &decl.ty {
        TypeRef::Primitive(scalar) => scalar.keyword().to_owned(),
        TypeRef::Compartment(name) => name.lexeme().to_owned(),
    };
    Ok(serde_json::json!({
        "type": ty,
        "name": name_context(&decl.name),
        "dimensions": decl.dimensions.iter().map(|dimension| match dimension {
            Dimension::Derived => Ok(serde_json::json!({"kind": "derived"})),
            Dimension::Expr(expression) => Ok(serde_json::json!({
                "kind": "expression",
                "value": alg_expression_context(expression)?,
            })),
        }).collect::<Result<Vec<_>, GalecTargetError>>()?,
        "min": decl.range.min.as_ref().map(alg_expression_context).transpose()?,
        "max": decl.range.max.as_ref().map(alg_expression_context).transpose()?,
    }))
}

fn name_context(name: &Name) -> serde_json::Value {
    match name {
        Name::Ident(identifier, _) => {
            serde_json::json!({"kind": "ident", "value": identifier.as_str()})
        }
        Name::Quoted(content, _) => serde_json::json!({"kind": "quoted", "value": content}),
    }
}

fn alg_reference_context(reference: &Reference) -> Result<serde_json::Value, GalecTargetError> {
    let (scope, parts) = match reference {
        Reference::Local(part) => ("local", std::slice::from_ref(part)),
        Reference::State(parts) => ("state", parts.as_slice()),
    };
    Ok(serde_json::json!({
        "scope": scope,
        "parts": parts.iter().map(|part| {
            Ok(serde_json::json!({
                "name": name_context(&part.name),
                "subscripts": part.subscripts.iter()
                    .map(alg_expression_context)
                    .collect::<Result<Vec<_>, _>>()?,
            }))
        }).collect::<Result<Vec<_>, GalecTargetError>>()?,
    }))
}

fn alg_expression_context(expression: &Expression) -> Result<serde_json::Value, GalecTargetError> {
    Ok(match expression {
        Expression::Bool(value) => serde_json::json!({"kind": "bool", "value": value}),
        Expression::Integer(value) => serde_json::json!({"kind": "integer", "value": value}),
        Expression::Real(value) => serde_json::json!({
            "kind": "real",
            "literal": rumoca_ir_galec::format_real_literal(*value).map_err(|error| {
                GalecTargetError::LoweringInternal {
                    detail: format!("GALEC codegen context met an unprintable Real literal: {error}"),
                }
            })?,
        }),
        Expression::Ref(reference) => {
            serde_json::json!({"kind": "ref", "reference": alg_reference_context(reference)?})
        }
        Expression::Size { array, dimension } => serde_json::json!({
            "kind": "size",
            "array": alg_reference_context(array)?,
            "dimension": alg_expression_context(dimension)?,
        }),
        Expression::Call(call) => serde_json::json!({
            "kind": "call",
            "function": name_context(&call.function),
            "arguments": call.arguments.iter().map(alg_expression_context)
                .collect::<Result<Vec<_>, _>>()?,
        }),
        Expression::Paren(inner) => {
            serde_json::json!({"kind": "paren", "value": alg_expression_context(inner)?})
        }
        Expression::If(if_expression) => serde_json::json!({
            "kind": "if",
            "branches": if_expression.branches.iter().map(|(condition, value)| {
                Ok(serde_json::json!({
                    "condition": alg_expression_context(condition)?,
                    "value": alg_expression_context(value)?,
                }))
            }).collect::<Result<Vec<_>, GalecTargetError>>()?,
            "else_value": alg_expression_context(&if_expression.else_value)?,
        }),
        Expression::Array(elements) => serde_json::json!({
            "kind": "array",
            "elements": elements.iter().map(alg_expression_context)
                .collect::<Result<Vec<_>, _>>()?,
        }),
        Expression::Neg(reference) => {
            serde_json::json!({"kind": "neg", "reference": alg_reference_context(reference)?})
        }
        Expression::Not(inner) => {
            serde_json::json!({"kind": "not", "value": alg_expression_context(inner)?})
        }
        Expression::Binary { op, lhs, rhs } => serde_json::json!({
            "kind": "binary",
            "operator": crate::c_lower::binary_op_name(*op),
            "lhs": alg_expression_context(lhs)?,
            "rhs": alg_expression_context(rhs)?,
            "wrap_lhs": alg_operand_needs_parens(lhs, op.precedence_class(), false),
            "wrap_rhs": alg_operand_needs_parens(rhs, op.precedence_class(), true),
        }),
    })
}

fn alg_operand_needs_parens(child: &Expression, parent: PrecedenceClass, right: bool) -> bool {
    match child {
        Expression::Binary { op, .. } => {
            let child_class = op.precedence_class();
            child_class != parent
                || (right && parent.associativity() == Associativity::Left)
                || (!right && parent.associativity() == Associativity::Right)
        }
        Expression::Neg(_) | Expression::Not(_) => true,
        Expression::Integer(value) => *value < 0,
        Expression::Real(value) => value.is_sign_negative(),
        _ => false,
    }
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
// C template context (the `galec-c` target, GAL-024)
// ---------------------------------------------------------------------------

/// Typed template context (serialized shape of [`c_template_context`]).
/// Every key is consumed by the `galec-c` templates. Rust supplies
/// names, types, and normalized codegen IR; templates own all C syntax
/// (D2/GAL-008).
#[derive(Serialize)]
struct CContext {
    /// CLI model identifier (used by the `[[files]]` path templates).
    model_name: String,
    /// Manifest spelling of the block name, comment-safe
    /// ([`c_comment_text`]) — interpolated in the file header comments.
    block_name: String,
    /// C typedef name of the block-state struct.
    struct_name: String,
    /// Prefix of the three exported method names
    /// (`<prefix>_startup` / `<prefix>_recalibrate` / `<prefix>_dostep`).
    function_prefix: String,
    /// Header include-guard macro.
    include_guard: String,
    /// Manifest-listed variables — exactly the block-state struct fields.
    variables: Vec<CVariable>,
    /// Method bodies as structured C codegen IR.
    methods: CMethods,
}

#[derive(Serialize)]
struct CVariable {
    /// Manifest spelling (quoted-identifier content for quoted names),
    /// comment-safe ([`c_comment_text`]) — the templates interpolate it
    /// inside C block comments only.
    name: String,
    /// Manifest id (`V1`…).
    id: String,
    /// Manifest `blockCausality` literal.
    causality: &'static str,
    /// Language-neutral GALEC scalar type; the C template owns its spelling.
    scalar_type: &'static str,
    /// Collision-checked C struct field name ([`crate::c_mangle`]).
    c_name: String,
    /// Dimension sizes (empty = scalar).
    dimensions: Vec<u64>,
}

#[derive(Serialize)]
struct CMethods {
    startup: Vec<serde_json::Value>,
    recalibrate: Vec<serde_json::Value>,
    do_step: Vec<serde_json::Value>,
}

/// Serialize the typed C-template context for the `galec-c`
/// target (module docs). The block is re-validated first, exactly as in
/// [`algorithm_template_context`] (GAL-004: no rendering path accepts an
/// un-validated package).
///
/// # Errors
///
/// `ET022` on C-name collisions, `ET023` for GALEC constructs the C
/// export does not support, `ET018` for validator/context rejections.
pub fn c_template_context(
    package: &AlgorithmCodePackage,
    model_name: &str,
) -> Result<serde_json::Value, GalecTargetError> {
    validate_block(&package.block)?;
    ensure_c_exportable(&package.block)?;
    let names = crate::c_mangle::CNameTable::build(&package.block)?;
    let variables = package
        .manifest
        .variables
        .iter()
        .map(|variable| {
            let common = variable.common();
            let spelling = common.name.as_str();
            Ok(CVariable {
                name: c_comment_text(spelling),
                id: common.id.as_str().to_owned(),
                causality: common.block_causality.as_str(),
                scalar_type: scalar_type_name(manifest_scalar_type(variable)),
                c_name: names.c_name_by_spelling(spelling)?.to_owned(),
                dimensions: common.dimensions.clone(),
            })
        })
        .collect::<Result<Vec<_>, GalecTargetError>>()?;
    let lowerer = crate::c_lower::CContextLowerer::new(&names);
    let function_prefix = crate::c_mangle::c_identifier(&package.block.name)?;
    let context = CContext {
        model_name: model_name.to_owned(),
        block_name: c_comment_text(&block_display_name(&package.block.name)),
        struct_name: format!("{function_prefix}State"),
        include_guard: format!("{}_GALEC_C_H", function_prefix.to_ascii_uppercase()),
        function_prefix,
        variables,
        methods: CMethods {
            startup: statements(&package.block.startup.statements, &lowerer)?,
            recalibrate: statements(&package.block.recalibrate.statements, &lowerer)?,
            do_step: statements(&package.block.do_step.statements, &lowerer)?,
        },
    };
    serde_json::to_value(&context).map_err(|error| GalecTargetError::LoweringInternal {
        detail: format!("C template context serialization failed: {error}"),
    })
}

/// Serialize the same typed C-template context directly from parsed GALEC
/// Algorithm Code. This is the browser/editor path for `.alg -> .h/.c`: it
/// validates the edited block and uses the same C name table, C printer, and
/// C-layout templates as the package-based export, but it does not assemble an
/// eFMI Production Code manifest or container.
///
/// # Errors
///
/// `ET018`/`ET022`/`ET023` for validator, C-name, or unsupported C-export
/// findings. Dimensioned variables are accepted when their dimensions are
/// literal positive integers, matching the shape the current projection emits.
pub fn c_template_context_for_block(
    block: &Block,
    model_name: &str,
) -> Result<serde_json::Value, GalecTargetError> {
    validate_block(block)?;
    ensure_c_exportable(block)?;
    let names = crate::c_mangle::CNameTable::build(block)?;
    let mut ordinal = 1usize;
    let mut variables = Vec::new();
    for variable in &block.interface {
        variables.push(c_variable_for_decl(
            &variable.decl,
            next_variable_id(&mut ordinal),
            interface_causality(variable.kind),
            &names,
        )?);
    }
    for entity in &block.protected {
        variables.push(c_variable_for_decl(
            &entity.decl,
            next_variable_id(&mut ordinal),
            protected_causality(entity.kind),
            &names,
        )?);
    }
    let lowerer = crate::c_lower::CContextLowerer::new(&names);
    let function_prefix = crate::c_mangle::c_identifier(&block.name)?;
    let context = CContext {
        model_name: model_name.to_owned(),
        block_name: c_comment_text(&block_display_name(&block.name)),
        struct_name: format!("{function_prefix}State"),
        include_guard: format!("{}_GALEC_C_H", function_prefix.to_ascii_uppercase()),
        function_prefix,
        variables,
        methods: CMethods {
            startup: statements(&block.startup.statements, &lowerer)?,
            recalibrate: statements(&block.recalibrate.statements, &lowerer)?,
            do_step: statements(&block.do_step.statements, &lowerer)?,
        },
    };
    serde_json::to_value(&context).map_err(|error| GalecTargetError::LoweringInternal {
        detail: format!("C template context serialization failed: {error}"),
    })
}

fn next_variable_id(ordinal: &mut usize) -> String {
    let id = format!("V{ordinal}");
    *ordinal += 1;
    id
}

fn interface_causality(kind: InterfaceKind) -> &'static str {
    match kind {
        InterfaceKind::Input => "input",
        InterfaceKind::Output => "output",
        InterfaceKind::TunableParameter => "tunableParameter",
    }
}

fn protected_causality(kind: ProtectedKind) -> &'static str {
    match kind {
        ProtectedKind::DependentParameter => "dependentParameter",
        ProtectedKind::Constant => "constant",
        ProtectedKind::State => "state",
    }
}

fn c_variable_for_decl(
    decl: &VariableDeclaration,
    id: String,
    causality: &'static str,
    names: &crate::c_mangle::CNameTable,
) -> Result<CVariable, GalecTargetError> {
    let spelling = crate::mangle::manifest_name(&decl.name);
    Ok(CVariable {
        name: c_comment_text(spelling),
        id,
        causality,
        scalar_type: scalar_type_name(scalar_type_for_decl(decl)?),
        c_name: names.c_name_by_spelling(spelling)?.to_owned(),
        dimensions: c_dimensions(&decl.dimensions)?,
    })
}

fn scalar_type_for_decl(decl: &VariableDeclaration) -> Result<ScalarType, GalecTargetError> {
    match &decl.ty {
        TypeRef::Primitive(scalar) => Ok(*scalar),
        TypeRef::Compartment(_) => Err(GalecTargetError::CExportUnsupported {
            construct: "a state-compartment variable",
            detail: "the standalone GALEC-to-C preview currently supports only primitive block variables"
                .to_owned(),
        }),
    }
}

fn c_dimensions(dimensions: &[Dimension]) -> Result<Vec<u64>, GalecTargetError> {
    dimensions.iter().map(c_dimension).collect()
}

fn c_dimension(dimension: &Dimension) -> Result<u64, GalecTargetError> {
    match dimension {
        Dimension::Expr(Expression::Integer(value)) if *value > 0 => {
            u64::try_from(*value).map_err(|_| GalecTargetError::CExportUnsupported {
                construct: "a too-large array dimension",
                detail: "dimension literals must fit in the generated C declaration".to_owned(),
            })
        }
        Dimension::Expr(_) => Err(GalecTargetError::CExportUnsupported {
            construct: "a non-literal array dimension",
            detail:
                "the standalone GALEC-to-C preview currently supports literal positive dimensions"
                    .to_owned(),
        }),
        Dimension::Derived => Err(GalecTargetError::CExportUnsupported {
            construct: "a derived array dimension",
            detail: "block variables need concrete dimensions for generated C fields".to_owned(),
        }),
    }
}

/// Reject block shapes the current lowering never produces before any C is
/// printed (GAL-007: loud `ET023`, never a silent drop). Kept in lockstep
/// with [`crate::lower`]: it emits no compartments, user functions, error
/// signals, method locals, or method `signals` clauses. Shared with the
/// Production Code manifest builder ([`crate::production_manifest`]), whose
/// three-void-functions-plus-`self` description assumes exactly this shape.
pub(crate) fn ensure_c_exportable(block: &Block) -> Result<(), GalecTargetError> {
    let unsupported = |construct: &'static str| GalecTargetError::CExportUnsupported {
        construct,
        detail: "the current DAE lowering (crate::lower) never emits this construct".to_owned(),
    };
    if !block.compartments.is_empty() {
        return Err(unsupported("record state compartments"));
    }
    if !block.error_signals.is_empty() {
        return Err(unsupported("user-defined error signals"));
    }
    if !block.protected_functions.is_empty() || !block.public_functions.is_empty() {
        return Err(unsupported("user-defined functions"));
    }
    for method in [&block.startup, &block.recalibrate, &block.do_step] {
        if !method.signals.is_empty() {
            return Err(unsupported("a block-method `signals` clause"));
        }
        if !method.locals.is_empty() {
            return Err(unsupported("method-local variables"));
        }
    }
    Ok(())
}

fn scalar_type_name(scalar: ScalarType) -> &'static str {
    match scalar {
        ScalarType::Real => "real",
        ScalarType::Integer => "integer",
        ScalarType::Boolean => "boolean",
    }
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

/// The GALEC-scalar → eFMI Production Code target binding used by the typed
/// manifest builder (`TargetType@kind`/`@codedType` and `Typedef@name`).
/// Generated C declarations use the language-neutral scalar kind in
/// [`CVariable`] and let the template choose its textual type spelling.
pub(crate) fn c_scalar_binding(scalar: ScalarType) -> (TargetTypeKind, &'static str) {
    match scalar {
        ScalarType::Real => (TargetTypeKind::EfmiFloat64, "double"),
        ScalarType::Integer => (TargetTypeKind::EfmiInteger32, "int32_t"),
        ScalarType::Boolean => (TargetTypeKind::EfmiBool, "bool"),
    }
}

fn statements(
    block_statements: &[Spanned<Statement>],
    lowerer: &crate::c_lower::CContextLowerer<'_>,
) -> Result<Vec<serde_json::Value>, GalecTargetError> {
    block_statements
        .iter()
        .map(|statement| lowerer.statement_contexts(&statement.node))
        .collect::<Result<Vec<_>, _>>()
        .map(|groups| groups.into_iter().flatten().collect())
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
fn c_comment_text(text: &str) -> String {
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
