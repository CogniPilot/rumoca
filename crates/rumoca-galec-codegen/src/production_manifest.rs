//! Production Code manifest assembly for the `galec-production` eFMU export
//! (SPEC_0034 GAL-024 conformant track).
//!
//! [`assemble_production_manifest`] builds the typed
//! [`ProductionCodeManifest`] describing the generated C99 files of the
//! shared `embedded-c-galec` layout, from **typed inputs only** — the
//! Algorithm Code manifest UUID and ids come from the typed
//! [`AlgorithmCodeManifest`], checksums arrive as [`Sha1Hex`] values computed
//! by the caller over the exact rendered bytes (GAL-021: no placeholder
//! checksums, no byte re-parsing anywhere).
//!
//! The C entity names are the same collision-checked names the C templates
//! receive ([`crate::c_mangle::CNameTable`] / [`crate::c_mangle::c_identifier`]
//! via [`crate::emit::c_template_context`]), and the scalar type bindings come
//! from the shared `crate::emit::c_scalar_binding` mapping — the manifest
//! stays in lockstep with the generated C by construction.
//!
//! # Shape of the emitted manifest (fixed local-id scheme)
//!
//! - `ManifestReference` `MR_AC`: the Algorithm Code manifest (typed UUID +
//!   SHA-1 of its exact rendered bytes);
//! - `Files` `F_H`/`F_C`: the generated `<Model>.h`/`<Model>.c` (`role=Code`,
//!   `path="./"`). The manifest itself is not listed (Algorithm Code
//!   precedent: self-checksum is impossible);
//! - `TargetTypes` `TT_F64`/`TT_I32`/`TT_BOOL`/`TT_VOID` and their alias
//!   `Typedefs` `TD_F64`/`TD_I32`/`TD_BOOL`/`TD_VOID` (names = the literal C
//!   tokens `double`/`int32_t`/`bool`/`void`);
//! - header `CodeFile` `CF_H`: the four aliases plus the block-state struct
//!   `Typedef` `TD_STATE` (one `Component` `CO_<i>` per manifest variable
//!   `V<i>`, with literal array dimensions passed through — arrays are
//!   first-class, D5);
//! - source `CodeFile` `CF_C` (includes `CF_H`): the three exported void
//!   block-method functions `FN_STARTUP`/`FN_RECALIBRATE`/`FN_DOSTEP`
//!   (return parameters `RP_*` typed `TD_VOID`, one `self` formal parameter
//!   `FP_*_SELF` pointing at `TD_STATE`). The `static inline` builtin
//!   helpers of the C prelude are file-internal and deliberately **not**
//!   published — the `Functions` list is for globally accessible entities;
//! - `LogicalData`: one `DataReference` per Algorithm Code variable,
//!   anchored on the DoStep `self` parameter (`FP_DOSTEP_SELF`) with the C
//!   field name as whole-field `componentIdentifier` (no indices — arrays
//!   map as the field; D5/D7), plus one `FunctionReference` per block
//!   method. No `ErrorSignalStatus` mapping is emitted: the generated
//!   methods return void and expose no status variable (D8; the
//!   cross-validator permits, never requires, an ESS mapping).
//!
//! Post-validation (GAL-004 idiom): the assembled manifest is checked by
//! [`ProductionCodeManifest::new`] and then cross-validated against the
//! Algorithm Code manifest
//! ([`rumoca_efmi::production_code_manifest::validate_against_algorithm_code`])
//! before it is returned, so an unmappable package fails here, in the owning
//! phase — never at container-write time.

use rumoca_efmi::algorithm_code_manifest::AlgorithmCodeManifest;
use rumoca_efmi::manifest_common::{File, FileChecksum, FileRole, ManifestAttributes};
use rumoca_efmi::production_code_manifest::{
    CodeContainer, CodeFile, CodeFileType, CodeType, Component, DataReference, FloatPrecision,
    ForeignReference, FormalParameter, Function, FunctionReference, LogicalData, ManifestReference,
    ParameterCore, ProductionCodeManifest, ProductionCodeManifestParts, SupportedLanguage,
    SupportedPlatform, TargetType, TargetTypeKind, Typedef, TypedefBody,
    validate_against_algorithm_code,
};
use rumoca_efmi::{
    FilePath, Identifier, ManifestId, NameWithoutSlashes, NormalizedText, Sha1Hex, UtcTimestamp,
};
use rumoca_ir_galec::ast::ScalarType;

use crate::diagnostic::GalecTargetError;
use crate::package::AlgorithmCodePackage;

/// Local id of the one `ManifestReference` (the Algorithm Code manifest).
const MANIFEST_REFERENCE_ID: &str = "MR_AC";
/// Local ids of the `Files` entries (header / source).
const HEADER_FILE_ID: &str = "F_H";
const SOURCE_FILE_ID: &str = "F_C";
/// Local ids of the `CodeFile` entries (header / source).
const HEADER_CODE_FILE_ID: &str = "CF_H";
const SOURCE_CODE_FILE_ID: &str = "CF_C";
/// Local id of the block-state struct `Typedef` (`Components` body).
const STATE_TYPEDEF_ID: &str = "TD_STATE";
/// Component ids are `CO_<i>`, aligned with the manifest variable ids
/// `V<i>` ([`crate::manifest_vars`] id scheme).
const COMPONENT_ID_PREFIX: &str = "CO_";
/// The `TT_VOID`/`TD_VOID` C token (the scalar tokens come from the shared
/// `crate::emit::c_scalar_binding`; `void` is not a GALEC scalar type).
const VOID_C_TYPE: &str = "void";

/// (`TargetType` id, alias `Typedef` id) per GALEC scalar type; the bound C
/// token and eFMI kind come from `crate::emit::c_scalar_binding`.
const fn scalar_type_ids(scalar: ScalarType) -> (&'static str, &'static str) {
    match scalar {
        ScalarType::Real => ("TT_F64", "TD_F64"),
        ScalarType::Integer => ("TT_I32", "TD_I32"),
        ScalarType::Boolean => ("TT_BOOL", "TD_BOOL"),
    }
}

/// The three GALEC scalar types, in the fixed emission order of the
/// `TargetTypes`/alias-`Typedefs` lists.
const SCALAR_TYPES: [ScalarType; 3] = [ScalarType::Real, ScalarType::Integer, ScalarType::Boolean];

/// (function id, return-parameter id, `self` formal-parameter id, C name
/// suffix) of the three exported block-method functions, in
/// Startup/Recalibrate/DoStep order.
const METHOD_FUNCTIONS: [(&str, &str, &str, &str); 3] = [
    ("FN_STARTUP", "RP_STARTUP", "FP_STARTUP_SELF", "startup"),
    (
        "FN_RECALIBRATE",
        "RP_RECALIBRATE",
        "FP_RECALIBRATE_SELF",
        "recalibrate",
    ),
    ("FN_DOSTEP", "RP_DOSTEP", "FP_DOSTEP_SELF", "dostep"),
];

/// `self` formal-parameter id of the DoStep function — the anchor of every
/// `DataReference` (D7: DoStep is the method whose I/O contract the
/// environment exercises every tick; triplicate mappings add no
/// information).
const DOSTEP_SELF_PARAMETER_ID: &str = "FP_DOSTEP_SELF";

/// One rendered C code file as the container layer knows it: its file name
/// and the SHA-1 of its exact rendered bytes. Constructed by the caller
/// *after* the bytes are final (GAL-021) — the builder never sees or hashes
/// file contents itself.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EmittedCodeFile {
    /// File name without directory components (e.g. `Model.h`).
    pub name: NameWithoutSlashes,
    /// SHA-1 over the exact rendered bytes.
    pub sha1: Sha1Hex,
}

/// Assemble the typed Production Code manifest describing the generated C
/// files of `package` (module docs), cross-validated against `ac_manifest` —
/// the typed half of the same
/// [`crate::render_manifest_document`] pass whose XML bytes
/// `ac_manifest_sha1` was computed over.
///
/// # Errors
///
/// `ET016` when the typed manifest model or the cross-manifest validator
/// rejects the assembled manifest (e.g. an Algorithm Code variable with no
/// C counterpart), `ET022` on C-name collisions, `ET023` for block shapes
/// outside the C export, `ET018` for validator failures or violated
/// projection invariants (e.g. a manifest variable id outside the `V<i>`
/// scheme).
pub fn assemble_production_manifest(
    package: &AlgorithmCodePackage,
    ac_manifest: &AlgorithmCodeManifest,
    ac_manifest_sha1: Sha1Hex,
    c_header: &EmittedCodeFile,
    c_source: &EmittedCodeFile,
) -> Result<ProductionCodeManifest, GalecTargetError> {
    // GAL-004 pre-assembly validation, exactly as on the other rendering
    // facades: the manifest must describe validated, C-exportable output.
    crate::emit::validate_block(&package.block)?;
    crate::emit::ensure_c_exportable(&package.block)?;
    let names = crate::c_mangle::CNameTable::build(&package.block)?;
    let function_prefix = crate::c_mangle::c_identifier(&package.block.name)?;

    let (components, data_references) = variable_mappings(package, &names)?;
    let (target_types, typedefs) = type_bindings(&format!("{function_prefix}State"), components)?;
    let (functions, function_references) = method_functions(&function_prefix, ac_manifest)?;

    let parts = ProductionCodeManifestParts {
        attributes: production_attributes(package)?,
        manifest_reference: ManifestReference {
            id: Identifier::new(MANIFEST_REFERENCE_ID)?,
            // Typed UUID from the typed model — never re-parsed from bytes.
            manifest_ref_id: ac_manifest.parts().attributes.id,
            checksum: ac_manifest_sha1,
        },
        files: vec![
            code_file_entry(HEADER_FILE_ID, c_header)?,
            code_file_entry(SOURCE_FILE_ID, c_source)?,
        ],
        code_container: code_container(
            target_types,
            typedefs,
            functions,
            LogicalData {
                data_references,
                function_references,
            },
        )?,
        annotations: vec![],
    };

    let manifest = ProductionCodeManifest::new(parts).map_err(GalecTargetError::from)?;
    // GAL-004 post-validation in the owning phase: an unmappable package
    // fails here, never at container-write time.
    validate_against_algorithm_code(&manifest, ac_manifest).map_err(GalecTargetError::from)?;
    Ok(manifest)
}

/// One state-struct `Component` and one whole-field `DataReference` per
/// manifest variable, in manifest order.
fn variable_mappings(
    package: &AlgorithmCodePackage,
    names: &crate::c_mangle::CNameTable,
) -> Result<(Vec<Component>, Vec<DataReference>), GalecTargetError> {
    let mut components = Vec::with_capacity(package.manifest.variables.len());
    let mut data_references = Vec::with_capacity(package.manifest.variables.len());
    for variable in &package.manifest.variables {
        let common = variable.common();
        let c_name = names.c_name_by_spelling(common.name.as_str())?;
        let (_, type_def_id) = scalar_type_ids(crate::emit::manifest_scalar_type(variable));
        components.push(Component {
            id: component_id(&common.id)?,
            name: NormalizedText::new(c_name)?,
            type_def_ref_id: Identifier::new(type_def_id)?,
            // D5: literal array dimensions pass through; arrays are
            // first-class on both sides.
            dimensions: common.dimensions.clone(),
            pointer: false,
        });
        data_references.push(DataReference {
            foreign: ForeignReference {
                manifest_reference_ref_id: Identifier::new(MANIFEST_REFERENCE_ID)?,
                foreign_ref_id: common.id.clone(),
            },
            formal_parameter_ref_id: Identifier::new(DOSTEP_SELF_PARAMETER_ID)?,
            // Whole-field mapping, no indices (D5): arrays map as the field.
            component_identifier: Some(NormalizedText::new(c_name)?),
        });
    }
    Ok((components, data_references))
}

/// The `TargetTypes` list and the header's `Typedefs` (the scalar + void
/// aliases via the shared `crate::emit::c_scalar_binding`, then the
/// `TD_STATE` struct typedef owning `components`).
fn type_bindings(
    struct_name: &str,
    components: Vec<Component>,
) -> Result<(Vec<TargetType>, Vec<Typedef>), GalecTargetError> {
    let mut target_types = Vec::with_capacity(SCALAR_TYPES.len() + 1);
    let mut typedefs = Vec::with_capacity(SCALAR_TYPES.len() + 2);
    for scalar in SCALAR_TYPES {
        let (kind, c_token) = crate::emit::c_scalar_binding(scalar);
        let (target_type_id, type_def_id) = scalar_type_ids(scalar);
        target_types.push(TargetType {
            id: Identifier::new(target_type_id)?,
            kind,
            coded_type: NormalizedText::new(c_token)?,
        });
        typedefs.push(alias_typedef(type_def_id, c_token, target_type_id)?);
    }
    target_types.push(TargetType {
        id: Identifier::new("TT_VOID")?,
        kind: TargetTypeKind::EfmiVoid,
        coded_type: NormalizedText::new(VOID_C_TYPE)?,
    });
    typedefs.push(alias_typedef("TD_VOID", VOID_C_TYPE, "TT_VOID")?);
    typedefs.push(Typedef {
        id: Identifier::new(STATE_TYPEDEF_ID)?,
        name: NormalizedText::new(struct_name)?,
        body: TypedefBody::Components(components),
    });
    Ok((target_types, typedefs))
}

/// The three exported block-method functions plus the `FunctionReference`s
/// mapping the typed Algorithm Code block-method ids onto them.
fn method_functions(
    function_prefix: &str,
    ac_manifest: &AlgorithmCodeManifest,
) -> Result<(Vec<Function>, Vec<FunctionReference>), GalecTargetError> {
    let methods = &ac_manifest.parts().block_methods;
    let method_ids = [
        &methods.startup.id,
        &methods.recalibrate.id,
        &methods.do_step.id,
    ];
    let mut functions = Vec::with_capacity(METHOD_FUNCTIONS.len());
    let mut function_references = Vec::with_capacity(METHOD_FUNCTIONS.len());
    for ((function_id, return_id, self_id, suffix), method_id) in
        METHOD_FUNCTIONS.into_iter().zip(method_ids)
    {
        functions.push(void_self_function(
            function_id,
            &format!("{function_prefix}_{suffix}"),
            return_id,
            self_id,
        )?);
        function_references.push(FunctionReference {
            foreign: ForeignReference {
                manifest_reference_ref_id: Identifier::new(MANIFEST_REFERENCE_ID)?,
                foreign_ref_id: method_id.clone(),
            },
            function_ref_id: Identifier::new(function_id)?,
        });
    }
    Ok((functions, function_references))
}

/// Top-level manifest attributes: fresh UUID, strict-UTC timestamp, tool
/// string — packaging-time facts matching the Algorithm Code assembly.
fn production_attributes(
    package: &AlgorithmCodePackage,
) -> Result<ManifestAttributes, GalecTargetError> {
    Ok(ManifestAttributes {
        id: ManifestId::generate(),
        name: NormalizedText::new(crate::emit::block_display_name(&package.block.name))?,
        description: None,
        version: None,
        generation_date_and_time: UtcTimestamp::now_utc(),
        generation_tool: Some(NormalizedText::new(format!(
            "rumoca {}",
            env!("CARGO_PKG_VERSION")
        ))?),
        copyright: None,
        license: None,
    })
}

/// The `CodeContainer`: C99/Legacy/64-bit `Generic` target, the header code
/// file `CF_H` owning the typedefs and the source code file `CF_C` owning
/// the exported functions (and including `CF_H`).
fn code_container(
    target_types: Vec<TargetType>,
    typedefs: Vec<Typedef>,
    functions: Vec<Function>,
    logical_data: LogicalData,
) -> Result<CodeContainer, GalecTargetError> {
    Ok(CodeContainer {
        language: SupportedLanguage::C,
        standard: NormalizedText::new("C99")?,
        platform: SupportedPlatform::Legacy,
        float_precision: FloatPrecision::Bits64,
        description: None,
        target: NormalizedText::new("Generic")?,
        target_types,
        code_files: vec![
            CodeFile {
                id: Identifier::new(HEADER_CODE_FILE_ID)?,
                file_type: CodeFileType::ProductionCode,
                code_type: CodeType::HeaderFile,
                file_ref_id: Identifier::new(HEADER_FILE_ID)?,
                file_ref_kind: None,
                includes: vec![],
                typedefs,
                functions: vec![],
            },
            CodeFile {
                id: Identifier::new(SOURCE_CODE_FILE_ID)?,
                file_type: CodeFileType::ProductionCode,
                code_type: CodeType::SourceFile,
                file_ref_id: Identifier::new(SOURCE_FILE_ID)?,
                file_ref_kind: None,
                includes: vec![Identifier::new(HEADER_CODE_FILE_ID)?],
                typedefs: vec![],
                functions,
            },
        ],
        logical_data,
    })
}

/// `CO_<i>` component id aligned with the manifest variable id `V<i>`.
///
/// # Errors
///
/// `ET018` when the variable id is outside the positional `V<i>` scheme
/// [`crate::manifest_vars`] owns — alignment would silently break, so scheme
/// drift is a loud projection bug.
fn component_id(variable_id: &Identifier) -> Result<Identifier, GalecTargetError> {
    let suffix = variable_id
        .as_str()
        .strip_prefix('V')
        .filter(|suffix| !suffix.is_empty() && suffix.bytes().all(|b| b.is_ascii_digit()))
        .ok_or_else(|| GalecTargetError::LoweringInternal {
            detail: format!(
                "manifest variable id `{}` is outside the positional `V<i>` \
                 id scheme; the Production Code component ids `CO_<i>` cannot \
                 stay aligned",
                variable_id.as_str()
            ),
        })?;
    Identifier::new(format!("{COMPONENT_ID_PREFIX}{suffix}")).map_err(GalecTargetError::from)
}

/// One alias `Typedef` binding a C token to a `TargetType`.
fn alias_typedef(
    id: &str,
    c_token: &str,
    target_type_id: &str,
) -> Result<Typedef, GalecTargetError> {
    Ok(Typedef {
        id: Identifier::new(id)?,
        name: NormalizedText::new(c_token)?,
        body: TypedefBody::Alias {
            target_type_ref_id: Identifier::new(target_type_id)?,
            type_def_ref_id: None,
        },
    })
}

/// One exported block-method function: `void <name>(<Struct> *self)`.
fn void_self_function(
    id: &str,
    name: &str,
    return_id: &str,
    self_id: &str,
) -> Result<Function, GalecTargetError> {
    Ok(Function {
        id: Identifier::new(id)?,
        name: NormalizedText::new(name)?,
        return_parameter: ParameterCore {
            id: Identifier::new(return_id)?,
            type_def_ref_id: Identifier::new("TD_VOID")?,
            constant: false,
            pointer: false,
            const_pointer: false,
            dimensions: vec![],
        },
        formal_parameters: vec![FormalParameter {
            name: NormalizedText::new("self")?,
            core: ParameterCore {
                id: Identifier::new(self_id)?,
                type_def_ref_id: Identifier::new(STATE_TYPEDEF_ID)?,
                constant: false,
                pointer: true,
                const_pointer: false,
                dimensions: vec![],
            },
        }],
    })
}

/// One `Files` entry for a rendered C code file (`role=Code`, root path).
fn code_file_entry(id: &str, file: &EmittedCodeFile) -> Result<File, GalecTargetError> {
    Ok(File {
        id: Identifier::new(id)?,
        name: file.name.clone(),
        path: FilePath::root(),
        checksum: FileChecksum::Sha1(file.sha1.clone()),
        role: FileRole::Code,
        description: None,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_efmi::algorithm_code_manifest::{
        BlockCausality, BooleanVariable, IntegerVariable, RealVariable, StartValue,
        Variable as ManifestVariable, VariableCommon,
    };
    use rumoca_ir_galec::ast::{
        Block, Dimension, Expression, InterfaceKind, InterfaceVariable, Name, ProtectedEntity,
        ProtectedKind, TypeRef, VariableDeclaration,
    };

    use crate::package::ManifestFragment;

    fn ident(value: &str) -> Identifier {
        Identifier::new(value).unwrap()
    }

    fn text(value: &str) -> NormalizedText {
        NormalizedText::new(value).unwrap()
    }

    fn common(id: &str, name: &str, causality: BlockCausality, dims: Vec<u64>) -> VariableCommon {
        VariableCommon {
            id: ident(id),
            name: text(name),
            description: None,
            block_causality: causality,
            dimensions: dims,
            annotations: vec![],
        }
    }

    fn real_variable(
        id: &str,
        name: &str,
        causality: BlockCausality,
        dims: Vec<u64>,
        start: f64,
    ) -> ManifestVariable {
        ManifestVariable::Real(RealVariable {
            common: common(id, name, causality, dims),
            start: StartValue::Scalar(start),
            unit_ref_id: None,
            relative_quantity: false,
            min: None,
            max: None,
            nominal: None,
        })
    }

    fn scalar_decl(ty: ScalarType, name: &str) -> VariableDeclaration {
        VariableDeclaration::scalar(ty, Name::ident(name))
    }

    /// Hand-built package with full variable coverage: Real input/output,
    /// Integer + Boolean state, a Real 2x3 array state (D5), and the Real
    /// constant sample period the manifest `Clock` references.
    fn base_package() -> AlgorithmCodePackage {
        let mut block = Block::new(Name::ident("TestBlock"));
        block.interface = vec![
            InterfaceVariable {
                kind: InterfaceKind::Input,
                decl: scalar_decl(ScalarType::Real, "u"),
                start: None,
            },
            InterfaceVariable {
                kind: InterfaceKind::Output,
                decl: scalar_decl(ScalarType::Real, "y"),
                start: Some(Expression::Real(0.0)),
            },
        ];
        block.protected = vec![
            ProtectedEntity {
                kind: ProtectedKind::State,
                decl: scalar_decl(ScalarType::Integer, "count"),
                start: Some(Expression::Integer(0)),
            },
            ProtectedEntity {
                kind: ProtectedKind::State,
                decl: scalar_decl(ScalarType::Boolean, "flag"),
                start: Some(Expression::Bool(false)),
            },
            ProtectedEntity {
                kind: ProtectedKind::State,
                decl: VariableDeclaration {
                    ty: TypeRef::Primitive(ScalarType::Real),
                    name: Name::ident("arr"),
                    dimensions: vec![
                        Dimension::Expr(Expression::Integer(2)),
                        Dimension::Expr(Expression::Integer(3)),
                    ],
                    range: Default::default(),
                },
                start: Some(Expression::Real(0.0)),
            },
            ProtectedEntity {
                kind: ProtectedKind::Constant,
                decl: scalar_decl(ScalarType::Real, "samplePeriod"),
                start: Some(Expression::Real(0.01)),
            },
        ];
        let variables = vec![
            real_variable("V1", "u", BlockCausality::Input, vec![], 0.0),
            real_variable("V2", "y", BlockCausality::Output, vec![], 0.0),
            ManifestVariable::Integer(IntegerVariable {
                common: common("V3", "count", BlockCausality::State, vec![]),
                start: StartValue::Scalar(0),
                min: None,
                max: None,
            }),
            ManifestVariable::Boolean(BooleanVariable {
                common: common("V4", "flag", BlockCausality::State, vec![]),
                start: StartValue::Scalar(false),
            }),
            real_variable("V5", "arr", BlockCausality::State, vec![2, 3], 0.0),
            real_variable("V6", "samplePeriod", BlockCausality::Constant, vec![], 0.01),
        ];
        AlgorithmCodePackage {
            block,
            manifest: ManifestFragment {
                variables,
                clock_variable_ref_id: ident("V6"),
                startup_signals: vec![],
                recalibrate_signals: vec![],
                do_step_signals: vec![],
            },
            alg_file_name: "TestBlock.alg".to_owned(),
        }
    }

    fn emitted(name: &str, seed: &[u8]) -> EmittedCodeFile {
        EmittedCodeFile {
            name: NameWithoutSlashes::new(name).unwrap(),
            sha1: Sha1Hex::of_bytes(seed),
        }
    }

    /// Assemble the typed AC manifest of a package (the same pub(crate)
    /// assembly path `render_manifest_document` uses).
    fn ac_of(package: &AlgorithmCodePackage) -> AlgorithmCodeManifest {
        crate::emit::assemble_manifest(package, b"rendered .alg bytes")
            .expect("AC manifest assembles")
    }

    fn assemble(
        package: &AlgorithmCodePackage,
        ac: &AlgorithmCodeManifest,
    ) -> Result<ProductionCodeManifest, GalecTargetError> {
        assemble_production_manifest(
            package,
            ac,
            Sha1Hex::of_bytes(b"rendered AC manifest bytes"),
            &emitted("TestBlock.h", b"header bytes"),
            &emitted("TestBlock.c", b"source bytes"),
        )
    }

    /// The state-struct components of an assembled manifest (header code
    /// file, last typedef).
    fn state_components(pc: &ProductionCodeManifest) -> &[Component] {
        let header = &pc.parts().code_container.code_files[0];
        let state = header
            .typedefs
            .iter()
            .find(|t| t.id.as_str() == "TD_STATE")
            .expect("TD_STATE typedef");
        let TypedefBody::Components(components) = &state.body else {
            panic!(
                "TD_STATE must be a Components typedef, got {:?}",
                state.body
            );
        };
        components
    }

    // ---- exactly-once mapping ------------------------------------------

    /// Every AC variable gets exactly one `DataReference` anchored on the
    /// DoStep `self` parameter (D7), and every block method exactly one
    /// `FunctionReference` — the returned manifest already passed
    /// `validate_against_algorithm_code`, so this locks the emitted ids.
    #[test]
    fn maps_every_variable_and_method_exactly_once() {
        let package = base_package();
        let ac = ac_of(&package);
        let pc = assemble(&package, &ac).expect("assembles");
        let logical = &pc.parts().code_container.logical_data;

        let foreign_ids: Vec<&str> = logical
            .data_references
            .iter()
            .map(|r| r.foreign.foreign_ref_id.as_str())
            .collect();
        assert_eq!(foreign_ids, ["V1", "V2", "V3", "V4", "V5", "V6"]);
        for reference in &logical.data_references {
            assert_eq!(
                reference.foreign.manifest_reference_ref_id.as_str(),
                "MR_AC"
            );
            assert_eq!(
                reference.formal_parameter_ref_id.as_str(),
                "FP_DOSTEP_SELF",
                "every DataReference anchors on the DoStep self parameter (D7)"
            );
        }

        let function_maps: Vec<(&str, &str)> = logical
            .function_references
            .iter()
            .map(|r| {
                (
                    r.foreign.foreign_ref_id.as_str(),
                    r.function_ref_id.as_str(),
                )
            })
            .collect();
        assert_eq!(
            function_maps,
            [
                ("BM_STARTUP", "FN_STARTUP"),
                ("BM_RECALIBRATE", "FN_RECALIBRATE"),
                ("BM_DOSTEP", "FN_DOSTEP"),
            ]
        );
        // D8: no ErrorSignalStatus mapping.
        assert!(!foreign_ids.contains(&"ESS"));
    }

    // ---- full variable coverage + lockstep C bindings -------------------

    /// One `CO_<i>` component per manifest variable `V<i>`, with the
    /// collision-checked C field names, per-scalar-type typedef references,
    /// and whole-field `componentIdentifier`s equal to the field names.
    #[test]
    fn components_cover_every_variable_with_lockstep_bindings() {
        let package = base_package();
        let ac = ac_of(&package);
        let pc = assemble(&package, &ac).expect("assembles");

        let got: Vec<(&str, &str, &str)> = state_components(&pc)
            .iter()
            .map(|c| (c.id.as_str(), c.name.as_str(), c.type_def_ref_id.as_str()))
            .collect();
        assert_eq!(
            got,
            [
                ("CO_1", "u", "TD_F64"),
                ("CO_2", "y", "TD_F64"),
                ("CO_3", "count", "TD_I32"),
                ("CO_4", "flag", "TD_BOOL"),
                ("CO_5", "arr", "TD_F64"),
                ("CO_6", "samplePeriod", "TD_F64"),
            ]
        );

        let identifiers: Vec<&str> = pc
            .parts()
            .code_container
            .logical_data
            .data_references
            .iter()
            .map(|r| {
                r.component_identifier
                    .as_ref()
                    .expect("identifier")
                    .as_str()
            })
            .collect();
        assert_eq!(
            identifiers,
            ["u", "y", "count", "flag", "arr", "samplePeriod"]
        );

        // TargetTypes carry the same kind/token pairs as the shared
        // `c_scalar_binding` (lockstep by construction).
        let target_types: Vec<(&str, TargetTypeKind, &str)> = pc
            .parts()
            .code_container
            .target_types
            .iter()
            .map(|t| (t.id.as_str(), t.kind, t.coded_type.as_str()))
            .collect();
        assert_eq!(
            target_types,
            [
                ("TT_F64", TargetTypeKind::EfmiFloat64, "double"),
                ("TT_I32", TargetTypeKind::EfmiInteger32, "int32_t"),
                ("TT_BOOL", TargetTypeKind::EfmiBool, "bool"),
                ("TT_VOID", TargetTypeKind::EfmiVoid, "void"),
            ]
        );

        // The struct token matches the C template context's struct name.
        let header = &pc.parts().code_container.code_files[0];
        let state = header
            .typedefs
            .iter()
            .find(|t| t.id.as_str() == "TD_STATE")
            .expect("TD_STATE");
        assert_eq!(state.name.as_str(), "TestBlockState");

        // Three exported void functions on the source code file, each with
        // the single `self` pointer parameter.
        let source = &pc.parts().code_container.code_files[1];
        assert_eq!(source.includes[0].as_str(), "CF_H");
        let functions: Vec<(&str, &str)> = source
            .functions
            .iter()
            .map(|f| (f.id.as_str(), f.name.as_str()))
            .collect();
        assert_eq!(
            functions,
            [
                ("FN_STARTUP", "TestBlock_startup"),
                ("FN_RECALIBRATE", "TestBlock_recalibrate"),
                ("FN_DOSTEP", "TestBlock_dostep"),
            ]
        );
        for function in &source.functions {
            assert_eq!(
                function.return_parameter.type_def_ref_id.as_str(),
                "TD_VOID"
            );
            assert!(!function.return_parameter.pointer);
            let [self_param] = function.formal_parameters.as_slice() else {
                panic!(
                    "exactly one formal parameter, got {:?}",
                    function.formal_parameters
                );
            };
            assert_eq!(self_param.name.as_str(), "self");
            assert_eq!(self_param.core.type_def_ref_id.as_str(), "TD_STATE");
            assert!(self_param.core.pointer);
            assert!(!self_param.core.constant);
            assert!(!self_param.core.const_pointer);
        }
    }

    // ---- array passthrough (D5) -----------------------------------------

    /// Array dimensions pass through to the `Component` unchanged and the
    /// variable maps whole-field (no index in the `componentIdentifier`).
    #[test]
    fn array_dimensions_pass_through_and_map_whole_field() {
        let package = base_package();
        let ac = ac_of(&package);
        let pc = assemble(&package, &ac).expect("assembles");

        let arr = state_components(&pc)
            .iter()
            .find(|c| c.id.as_str() == "CO_5")
            .expect("array component");
        assert_eq!(arr.dimensions, [2, 3]);

        let arr_reference = pc
            .parts()
            .code_container
            .logical_data
            .data_references
            .iter()
            .find(|r| r.foreign.foreign_ref_id.as_str() == "V5")
            .expect("array data reference");
        assert_eq!(
            arr_reference
                .component_identifier
                .as_ref()
                .unwrap()
                .as_str(),
            "arr",
            "arrays map as the whole field — never element-wise"
        );
    }

    // ---- typed UUID + SHA-1 threading ------------------------------------

    /// The `ManifestReference` carries the AC manifest's typed UUID and the
    /// caller's SHA-1s verbatim; the `Files` list carries the rendered code
    /// files' names and checksums — no byte re-parse anywhere.
    #[test]
    fn typed_uuid_and_sha1s_thread_verbatim() {
        let package = base_package();
        let ac = ac_of(&package);
        let ac_sha1 = Sha1Hex::of_bytes(b"rendered AC manifest bytes");
        let header = emitted("TestBlock.h", b"header bytes");
        let source = emitted("TestBlock.c", b"source bytes");
        let pc = assemble_production_manifest(&package, &ac, ac_sha1.clone(), &header, &source)
            .expect("assembles");

        let reference = &pc.parts().manifest_reference;
        assert_eq!(reference.id.as_str(), "MR_AC");
        // Typed equality against the UUID minted inside the typed AC model.
        assert_eq!(reference.manifest_ref_id, ac.parts().attributes.id);
        assert_eq!(reference.checksum, ac_sha1);

        let files: Vec<(&str, &str, &FileChecksum)> = pc
            .parts()
            .files
            .iter()
            .map(|f| (f.id.as_str(), f.name.as_str(), &f.checksum))
            .collect();
        assert_eq!(
            files.len(),
            2,
            "code files only; the manifest never lists itself"
        );
        assert_eq!(files[0].0, "F_H");
        assert_eq!(files[0].1, "TestBlock.h");
        assert_eq!(files[0].2, &FileChecksum::Sha1(header.sha1.clone()));
        assert_eq!(files[1].0, "F_C");
        assert_eq!(files[1].1, "TestBlock.c");
        assert_eq!(files[1].2, &FileChecksum::Sha1(source.sha1.clone()));

        // The PC manifest gets its own fresh UUID, distinct from the AC one.
        assert_ne!(pc.parts().attributes.id, ac.parts().attributes.id);
    }

    // ---- GAL-004 post-validation in the owning phase ---------------------

    /// An AC variable without a C counterpart fails cross-validation inside
    /// the builder (ET016 wrapping EFM040) — never at container-write time.
    #[test]
    fn unmapped_algorithm_code_variable_fails_here() {
        let package = base_package();
        let mut widened = package.clone();
        widened.manifest.variables.push(real_variable(
            "V7",
            "ghost",
            BlockCausality::State,
            vec![],
            0.0,
        ));
        let ac = ac_of(&widened);
        let error = assemble(&package, &ac).expect_err("V7 has no C counterpart");
        assert_eq!(error.code(), "ET016", "{error}");
        let GalecTargetError::Manifest { source } = &error else {
            panic!("expected the EfmiError funnel, got: {error}");
        };
        assert_eq!(source.code(), "EFM040", "{source}");
    }

    /// A manifest variable id outside the `V<i>` scheme breaks the `CO_<i>`
    /// alignment — a loud ET018 projection bug, never a silent misalignment.
    #[test]
    fn variable_id_outside_scheme_is_a_loud_projection_bug() {
        let package = base_package();
        let ac = ac_of(&package);
        let mut broken = package.clone();
        let ManifestVariable::Real(first) = &mut broken.manifest.variables[0] else {
            panic!("fixture starts with a Real variable");
        };
        first.common.id = ident("X1");
        let error = assemble(&broken, &ac).expect_err("id scheme drift must fail");
        assert_eq!(error.code(), "ET018", "{error}");
    }
}
