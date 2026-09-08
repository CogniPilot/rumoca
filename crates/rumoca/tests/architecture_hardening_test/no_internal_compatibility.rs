//! Hard-cutover gate for compiler-owned APIs and formats.
//!
//! SPEC_0033 permits only the current internal representation. Serialization
//! aliases and deprecated Rust surfaces keep an old reader or entry point
//! alive, while `dead_code` suppression hides items outside the current call
//! graph. They are forbidden rather than placed on a ledger. Retired CLI
//! spellings are tombstoned at their owning command because a deliberate
//! current shorthand is not necessarily backward compatibility.

use std::collections::BTreeSet;
use std::fs;

use syn::visit::Visit;

use crate::architecture_hardening_support::{collect_rs_files, workspace_root};

#[derive(Default)]
struct CompatibilityAttributeVisitor {
    offenders: Vec<String>,
}

impl CompatibilityAttributeVisitor {
    fn record_serde_aliases(&mut self, path: &syn::Path, tokens: &str) {
        for forbidden in ["alias", "aliases"] {
            if !tokens_contain_word(tokens, forbidden) {
                continue;
            }
            self.offenders
                .push(format!("{}({forbidden})", path_key(path)));
        }
    }

    fn record_dead_code_suppression(&mut self, path: &syn::Path, tokens: &str) {
        if !tokens_contain_word(tokens, "dead_code") {
            return;
        }
        self.offenders
            .push(format!("{}(dead_code)", path_key(path)));
    }
}

impl<'ast> Visit<'ast> for CompatibilityAttributeVisitor {
    fn visit_attribute(&mut self, attribute: &'ast syn::Attribute) {
        let path = attribute.path();
        if path.is_ident("deprecated") {
            self.offenders.push("deprecated".to_owned());
        }
        let tokens = attribute_meta_tokens(attribute);
        if path.is_ident("serde") {
            self.record_serde_aliases(path, &tokens);
        }
        if path.is_ident("allow") || path.is_ident("expect") {
            self.record_dead_code_suppression(path, &tokens);
        }
        syn::visit::visit_attribute(self, attribute);
    }
}

fn attribute_meta_tokens(attribute: &syn::Attribute) -> String {
    match &attribute.meta {
        syn::Meta::List(list) => list.tokens.to_string(),
        syn::Meta::Path(_) | syn::Meta::NameValue(_) => String::new(),
    }
}

fn tokens_contain_word(tokens: &str, expected: &str) -> bool {
    tokens
        .split(|character: char| !character.is_ascii_alphanumeric() && character != '_')
        .any(|word| word == expected)
}

fn path_key(path: &syn::Path) -> String {
    path.segments
        .iter()
        .map(|segment| segment.ident.to_string())
        .collect::<Vec<_>>()
        .join("::")
}

fn compatibility_attributes(source: &str) -> Vec<String> {
    let syntax = syn::parse_file(source).expect("architecture input must parse as Rust");
    let mut visitor = CompatibilityAttributeVisitor::default();
    visitor.visit_file(&syntax);
    visitor.offenders
}

fn serde_default_fields(source: &str, struct_name: &str) -> Vec<String> {
    let syntax = syn::parse_file(source).expect("architecture input must parse as Rust");
    syntax
        .items
        .iter()
        .filter_map(|item| match item {
            syn::Item::Struct(item) if item.ident == struct_name => Some(item),
            _ => None,
        })
        .flat_map(|item| {
            let container = item
                .attrs
                .iter()
                .any(serde_attribute_selects_default)
                .then(|| "<container>".to_owned());
            container.into_iter().chain(
                item.fields
                    .iter()
                    .filter(|field| field.attrs.iter().any(serde_attribute_selects_default))
                    .map(|field| {
                        field
                            .ident
                            .as_ref()
                            .map_or_else(|| "<unnamed>".to_owned(), ToString::to_string)
                    }),
            )
        })
        .collect()
}

fn serde_default_enum_fields(source: &str, enum_name: &str) -> Vec<String> {
    let syntax = syn::parse_file(source).expect("architecture input must parse as Rust");
    syntax
        .items
        .iter()
        .filter_map(|item| match item {
            syn::Item::Enum(item) if item.ident == enum_name => Some(item),
            _ => None,
        })
        .flat_map(enum_default_fields)
        .collect()
}

fn enum_default_fields(item: &syn::ItemEnum) -> Vec<String> {
    let mut offenders = Vec::new();
    for variant in &item.variants {
        for field in &variant.fields {
            if !field.attrs.iter().any(serde_attribute_selects_default) {
                continue;
            }
            let field_name = field
                .ident
                .as_ref()
                .map_or_else(|| "<unnamed>".to_owned(), ToString::to_string);
            offenders.push(format!("{}.{field_name}", variant.ident));
        }
    }
    offenders
}

fn serde_attribute_selects_default(attribute: &syn::Attribute) -> bool {
    attribute.path().is_ident("serde")
        && match &attribute.meta {
            syn::Meta::List(list) => list
                .tokens
                .to_string()
                .split(|character: char| !character.is_ascii_alphanumeric() && character != '_')
                .any(|word| word == "default"),
            syn::Meta::Path(_) | syn::Meta::NameValue(_) => false,
        }
}

const REQUIRED_OPTION_WIRE_FIELD_CATALOG: &[(&str, &str, &[&str])] = &[
    (
        "crates/rumoca-test-msl/src/msl_tools/band_table.rs",
        "TraceExitRecord",
        &["certification_profile"],
    ),
    (
        "crates/rumoca-test-msl/src/msl_tools/band_table.rs",
        "BandRow",
        &[
            "exit_reason",
            "exit_detail",
            "max_channel_bounded_normalized_l1",
            "mean_channel_bounded_normalized_l1",
            "bounded_normalized_l1_score",
        ],
    ),
    (
        "crates/rumoca-test-msl/src/msl_tools/band_table.rs",
        "BandTable",
        &["working_tree_digest", "omc_version"],
    ),
    (
        "crates/rumoca-sim/src/sim_trace_compare.rs",
        "SimTrace",
        &["model_name", "variable_meta", "certification_profile"],
    ),
    (
        "crates/rumoca-sim/src/sim_trace_compare.rs",
        "SimTraceVariableMeta",
        &["role", "value_type", "variability", "time_domain"],
    ),
    (
        "crates/rumoca-sim/src/sim_trace_compare.rs",
        "ChannelDeviationMetric",
        &[
            "reference_array_group_floor",
            "initial_abs_error",
            "initial_bounded_normalized_error",
        ],
    ),
    (
        "crates/xtask/src/verify_cmd/msl_quality_baseline.rs",
        "MslQualityBaselineHeader",
        &[
            "omc_context_migration",
            "metric_schema_migration",
            "partial_classification_migration",
            "compiler_contract_migration",
            "promoted_baseline_bridge",
        ],
    ),
    (
        "crates/rumoca-ir-ast/src/nodes.rs",
        "ClassDef",
        &["redeclare_target_def_id"],
    ),
    (
        "crates/rumoca-ir-ast/src/instance.rs",
        "ClassOverride",
        &["target_ref"],
    ),
    (
        "crates/rumoca-ir-ast/src/instance.rs",
        "ClassInstanceData",
        &["source_scope", "source_scope_id"],
    ),
    (
        "crates/rumoca-core/src/ir_primitives/component_refs_and_functions.rs",
        "Function",
        &["def_id", "instance_id"],
    ),
    (
        "crates/rumoca-core/src/ir_primitives/component_refs_and_functions.rs",
        "FunctionParam",
        &["def_id", "type_def_id", "min", "max"],
    ),
    (
        "crates/rumoca-ir-ast/src/instance.rs",
        "InstanceData",
        &["declaration_def_id"],
    ),
    (
        "crates/rumoca-phase-solve/src/model_wire.rs",
        "SolveVariableCatalogEntryWire",
        &[
            "unit",
            "description",
            "start",
            "minimum",
            "maximum",
            "nominal",
        ],
    ),
    (
        "crates/rumoca-ir-solve/src/model.rs",
        "GuardedAssignmentProgramWire",
        &["clock_owner"],
    ),
    (
        "crates/rumoca-ir-solve/src/model.rs",
        "StructuredDiscreteUpdate",
        &["clock_owner"],
    ),
    (
        "crates/rumoca-ir-solve/src/model.rs",
        "SolveStringConversionFormat::Options",
        &["minimum_length", "left_justified", "significant_digits"],
    ),
    (
        "crates/rumoca-ir-solve/src/model.rs",
        "PreParamBinding",
        &["clock_schedule"],
    ),
    (
        "crates/rumoca-ir-solve/src/model.rs",
        "SolveLayout",
        &[
            "initial_event_parameter_index",
            "terminal_event_parameter_index",
            "initial_homotopy_parameter_index",
        ],
    ),
    (
        "crates/rumoca-ir-solve/src/model.rs",
        "SolveVariableMeta",
        &[
            "value_type",
            "variability",
            "time_domain",
            "unit",
            "start",
            "min",
            "max",
            "nominal",
            "description",
        ],
    ),
    (
        "crates/rumoca-ir-solve/src/model/event_transaction.rs",
        "EventTransactionTargetWire",
        &["clock_owner"],
    ),
    (
        "crates/rumoca-ir-solve/src/refresh.rs",
        "AlgebraicRefreshRowWire",
        &["assignment_target", "assignment_shape"],
    ),
    (
        "crates/rumoca-ir-solve/src/typed_program/call.rs",
        "SolvePureCallSite",
        &["directional"],
    ),
    (
        "crates/rumoca-ir-solve/src/linear_op.rs",
        "TargetAssignmentShape::Affine",
        &["coefficient_reg"],
    ),
    (
        "crates/rumoca-ir-solve/src/linear_op.rs",
        "FoldTensorUpdate",
        &["condition"],
    ),
    (
        "crates/rumoca-ir-solve/src/linear_op.rs",
        "FunctionConditionalProgramWire",
        &["owner"],
    ),
    (
        "crates/rumoca-ir-solve/src/linear_op.rs",
        "LinearOp::TensorLoad",
        &["seed_start"],
    ),
    (
        "crates/rumoca-ir-solve/src/linear_op.rs",
        "LinearOp::StoreOutputFunctionFold",
        &["condition"],
    ),
    (
        "crates/xtask/src/verify_cmd/embedded_head_to_head/manifest.rs",
        "MeasuredToolPins",
        &["arm_size_sha256"],
    ),
];

fn required_option_wire_offenders(
    source: &str,
    struct_name: &str,
    field_names: &[&str],
) -> Vec<String> {
    let syntax = syn::parse_file(source).expect("architecture input must parse as Rust");
    let (type_name, variant_name) = struct_name
        .split_once("::")
        .map_or((struct_name, None), |(type_name, variant_name)| {
            (type_name, Some(variant_name))
        });
    let fields = syntax
        .items
        .iter()
        .find_map(|item| match (item, variant_name) {
            (syn::Item::Struct(item), None) if item.ident == type_name => Some(&item.fields),
            (syn::Item::Enum(item), Some(variant_name)) if item.ident == type_name => item
                .variants
                .iter()
                .find(|variant| variant.ident == variant_name)
                .map(|variant| &variant.fields),
            _ => None,
        })
        .unwrap_or_else(|| panic!("missing current wire type `{struct_name}`"));
    field_names
        .iter()
        .filter_map(|field_name| {
            let field = fields
                .iter()
                .find(|field| {
                    field
                        .ident
                        .as_ref()
                        .is_some_and(|ident| ident == *field_name)
                })
                .unwrap_or_else(|| {
                    panic!("missing current wire field `{struct_name}.{field_name}`")
                });
            let serde = field
                .attrs
                .iter()
                .filter(|attribute| attribute.path().is_ident("serde"))
                .map(|attribute| match &attribute.meta {
                    syn::Meta::List(list) => list.tokens.to_string(),
                    syn::Meta::Path(_) | syn::Meta::NameValue(_) => String::new(),
                })
                .collect::<Vec<_>>();
            let has_required_decoder = serde
                .iter()
                .any(|tokens| tokens.contains("deserialize_with") && tokens.contains("required"));
            let skips_null = serde
                .iter()
                .any(|tokens| tokens.contains("skip_serializing_if"));
            if has_required_decoder && !skips_null {
                None
            } else {
                Some((*field_name).to_owned())
            }
        })
        .collect()
}

#[derive(Default)]
struct RequiredOptionDeserializeVisitor {
    offenders: Vec<String>,
    option_aliases: BTreeSet<String>,
}

impl RequiredOptionDeserializeVisitor {
    fn inspect_fields(&mut self, type_name: &str, fields: &syn::Fields) {
        for field in fields {
            if !is_direct_or_alias_option(&field.ty, &self.option_aliases) {
                continue;
            }
            if !field.attrs.iter().any(serde_requires_explicit_option_key) {
                let field_name = field
                    .ident
                    .as_ref()
                    .map_or_else(|| "<unnamed>".to_owned(), ToString::to_string);
                self.offenders.push(format!("{type_name}.{field_name}"));
            }
        }
    }
}

impl<'ast> Visit<'ast> for RequiredOptionDeserializeVisitor {
    fn visit_item_struct(&mut self, item: &'ast syn::ItemStruct) {
        if item.attrs.iter().any(derives_deserialize) {
            self.inspect_fields(&item.ident.to_string(), &item.fields);
        }
        syn::visit::visit_item_struct(self, item);
    }

    fn visit_item_enum(&mut self, item: &'ast syn::ItemEnum) {
        if item.attrs.iter().any(derives_deserialize) {
            for variant in &item.variants {
                self.inspect_fields(
                    &format!("{}::{}", item.ident, variant.ident),
                    &variant.fields,
                );
            }
        }
        syn::visit::visit_item_enum(self, item);
    }

    fn visit_item_type(&mut self, item: &'ast syn::ItemType) {
        if is_direct_or_alias_option(&item.ty, &self.option_aliases) {
            self.option_aliases.insert(item.ident.to_string());
        }
        syn::visit::visit_item_type(self, item);
    }
}

fn derives_deserialize(attribute: &syn::Attribute) -> bool {
    attribute.path().is_ident("derive")
        && attribute.meta.require_list().is_ok_and(|list| {
            list.tokens
                .to_string()
                .split(|character: char| !character.is_ascii_alphanumeric() && character != '_')
                .any(|word| word == "Deserialize")
        })
}

fn is_direct_or_alias_option(ty: &syn::Type, option_aliases: &BTreeSet<String>) -> bool {
    matches!(ty, syn::Type::Path(path) if path.path.segments.last().is_some_and(|segment| {
        segment.ident == "Option" || option_aliases.contains(&segment.ident.to_string())
    }))
}

fn serde_requires_explicit_option_key(attribute: &syn::Attribute) -> bool {
    serde_tokens(attribute).is_some_and(|tokens| {
        tokens.contains("deserialize_with")
            && (tokens.contains("deserialize_required_option")
                || tokens.contains("required_option"))
            && !tokens.contains("default")
            && !tokens.contains("skip_deserializing")
            && !tokens.contains("skip")
            && !tokens.contains("skip_serializing_if")
    })
}

fn serde_tokens(attribute: &syn::Attribute) -> Option<String> {
    attribute
        .path()
        .is_ident("serde")
        .then(|| match &attribute.meta {
            syn::Meta::List(list) => list.tokens.to_string(),
            syn::Meta::Path(_) | syn::Meta::NameValue(_) => String::new(),
        })
}

fn required_option_deserialize_offenders(source: &str) -> Vec<String> {
    let syntax = syn::parse_file(source).expect("architecture input must parse as Rust");
    let mut visitor = RequiredOptionDeserializeVisitor::default();
    visitor.visit_file(&syntax);
    // A module can declare an alias after the struct that uses it. A second
    // AST pass makes alias position irrelevant while preserving the same
    // field-level decoder checks.
    visitor.offenders.clear();
    visitor.visit_file(&syntax);
    visitor.offenders
}

#[test]
fn compiler_owned_rust_has_no_compatibility_attributes() {
    let crates = workspace_root().join("crates");
    let mut files = Vec::new();
    collect_rs_files(&crates, &mut files);
    files.sort();

    let mut offenders = Vec::new();
    for file in files {
        let source = fs::read_to_string(&file)
            .unwrap_or_else(|error| panic!("read {}: {error}", file.display()));
        for attribute in compatibility_attributes(&source) {
            if attribute.ends_with("(dead_code)") && is_generated_parser_output(&file) {
                continue;
            }
            offenders.push(format!("{}: {attribute}", file.display()));
        }
    }

    assert!(
        offenders.is_empty(),
        "SPEC_0033 hard cutover forbids serialization compatibility readers, deprecated APIs, and hand-written dead-code suppression; delete the old surface and migrate every caller: {offenders:#?}"
    );
}

fn is_generated_parser_output(path: &std::path::Path) -> bool {
    let relative = path
        .strip_prefix(workspace_root())
        .unwrap_or(path)
        .to_string_lossy()
        .replace('\\', "/");
    relative.starts_with("crates/rumoca-phase-parse/src/generated/")
        || relative.starts_with("crates/rumoca-phase-parse-galec/src/parse/generated/")
}

#[test]
fn compatibility_attribute_gate_detects_each_retired_surface_kind() {
    let source = [
        "#[derive(serde::Deserialize)]\nstruct Wire {\n#[serde(",
        "alias",
        " = \"old_name\")]\ncurrent_name: u32,\n}\n",
        "#[",
        "deprecated",
        "]\nfn old_api() {}\n",
        "#[",
        "allow",
        "(",
        "dead_code",
        ")]\nfn abandoned_api() {}\n",
    ]
    .concat();
    let offenders = compatibility_attributes(&source);
    assert!(offenders.iter().any(|item| item == "serde(alias)"));
    assert!(offenders.iter().any(|item| item == "deprecated"));
    assert!(offenders.iter().any(|item| item == "allow(dead_code)"));
}

#[test]
fn model_failure_wire_has_no_obsolete_incomplete_reader() {
    let path =
        workspace_root().join("crates/rumoca-compile/src/session/model_failure_diagnostic.rs");
    let source = fs::read_to_string(&path)
        .unwrap_or_else(|error| panic!("read {}: {error}", path.display()));
    let offenders = serde_default_fields(&source, "ModelFailureDiagnostic");
    assert!(
        offenders.is_empty(),
        "the current failure wire must require every field; delete compatibility defaults instead of accepting an older incomplete record: {offenders:?}"
    );
}

#[test]
fn failure_wire_gate_detects_a_planted_default() {
    let source = "struct ModelFailureDiagnostic { #[serde(default)] notes: Vec<String> }";
    assert_eq!(
        serde_default_fields(source, "ModelFailureDiagnostic"),
        ["notes"]
    );
}

#[test]
fn solve_semantic_wire_has_no_obsolete_default_reader() {
    for (relative_path, struct_names) in [
        (
            "crates/rumoca-ir-solve/src/layout.rs",
            &["ComponentReferenceKeyPart"][..],
        ),
        (
            "crates/rumoca-ir-solve/src/model.rs",
            &[
                "AlgebraicProjectionBlock",
                "DiscreteSolveSystem",
                "SolveEventAction",
                "PeriodicEventScheduleWire",
            ][..],
        ),
    ] {
        let path = workspace_root().join(relative_path);
        let source = fs::read_to_string(&path)
            .unwrap_or_else(|error| panic!("read {}: {error}", path.display()));
        for struct_name in struct_names {
            let offenders = serde_default_fields(&source, struct_name);
            assert!(
                offenders.is_empty(),
                "the current Solve wire must require every semantic field; `{struct_name}` in {} retains compatibility defaults on {offenders:?}",
                path.display()
            );
        }
    }
}

#[test]
fn solve_wire_gate_detects_field_and_container_defaults() {
    let field = "struct DiscreteSolveSystem { #[serde(default)] schedule: Vec<u32> }";
    assert_eq!(
        serde_default_fields(field, "DiscreteSolveSystem"),
        ["schedule"]
    );

    let container = "#[serde(default)] struct DiscreteSolveSystem { schedule: Vec<u32> }";
    assert_eq!(
        serde_default_fields(container, "DiscreteSolveSystem"),
        ["<container>"]
    );
}

#[test]
fn current_ast_and_function_wires_have_no_semantic_default_reader() {
    for (relative_path, struct_names) in [
        ("crates/rumoca-ir-ast/src/lib.rs", &["ClassTree"][..]),
        (
            "crates/rumoca-ir-ast/src/nodes.rs",
            &["ExternalFunction", "ClassDef"][..],
        ),
        (
            "crates/rumoca-ir-ast/src/instance.rs",
            &["ClassOverride", "ClassInstanceData"][..],
        ),
        (
            "crates/rumoca-ir-ast/src/scope.rs",
            &["ScopeTree", "Scope"][..],
        ),
        (
            "crates/rumoca-core/src/ir_primitives/component_refs_and_functions.rs",
            &["Function", "FunctionParam"][..],
        ),
    ] {
        let path = workspace_root().join(relative_path);
        let source = fs::read_to_string(&path)
            .unwrap_or_else(|error| panic!("read {}: {error}", path.display()));
        for struct_name in struct_names {
            let offenders = serde_default_fields(&source, struct_name);
            assert!(
                offenders.is_empty(),
                "current semantic wire `{struct_name}` in {} must not invent omitted fields: {offenders:?}",
                path.display()
            );
        }
    }

    let nodes_path = workspace_root().join("crates/rumoca-ir-ast/src/nodes.rs");
    let nodes = fs::read_to_string(&nodes_path)
        .unwrap_or_else(|error| panic!("read {}: {error}", nodes_path.display()));
    let offenders = serde_default_enum_fields(&nodes, "Expression");
    assert!(
        offenders.is_empty(),
        "current AST Expression wire must not invent omitted semantic fields: {offenders:?}"
    );
}

#[test]
fn current_evidence_readers_have_no_omission_defaults() {
    for (relative_path, struct_names) in [
        (
            "crates/rumoca-test-msl/src/msl_tools/band_table.rs",
            &[
                "TraceExitRecord",
                "BandRow",
                "BandTableCounts",
                "BandTableSource",
                "BandTable",
            ][..],
        ),
        (
            "crates/rumoca-sim/src/sim_trace_compare.rs",
            &[
                "SimTrace",
                "SimTraceVariableMeta",
                "ChannelDeviationMetric",
                "ModelDeviationMetric",
            ][..],
        ),
        (
            "crates/xtask/src/verify_cmd/msl_quality_baseline.rs",
            &["MslQualityBaselineHeader", "TraceAccuracyStats"][..],
        ),
    ] {
        let path = workspace_root().join(relative_path);
        let source = fs::read_to_string(&path)
            .unwrap_or_else(|error| panic!("read {}: {error}", path.display()));
        for struct_name in struct_names {
            let offenders = serde_default_fields(&source, struct_name);
            assert!(
                offenders.is_empty(),
                "current evidence wire `{struct_name}` in {} must not synthesize omitted evidence: {offenders:?}",
                path.display()
            );
        }
    }
}

#[test]
fn semantic_default_gate_detects_struct_and_enum_omission_readers() {
    assert_eq!(
        serde_default_fields(
            "struct Function { #[serde(default)] inline: InlineAnnotation }",
            "Function"
        ),
        ["inline"]
    );
    assert_eq!(
        serde_default_enum_fields(
            "enum Expression { FunctionCall { #[serde(default)] is_partial: bool } }",
            "Expression"
        ),
        ["FunctionCall.is_partial"]
    );
}

#[test]
fn current_required_option_wire_catalog_has_no_omission_reader_or_null_skip() {
    for (relative_path, struct_name, fields) in REQUIRED_OPTION_WIRE_FIELD_CATALOG {
        let path = workspace_root().join(relative_path);
        let source = fs::read_to_string(&path)
            .unwrap_or_else(|error| panic!("read {}: {error}", path.display()));
        let offenders = required_option_wire_offenders(&source, struct_name, fields);
        assert!(
            offenders.is_empty(),
            "current wire `{struct_name}` in {} must require each optional key and serialize explicit null: {offenders:?}",
            path.display()
        );
    }
}

#[test]
fn current_solve_deserialize_options_require_explicit_null_keys() {
    let roots = [
        workspace_root().join("crates/rumoca-ir-solve/src"),
        workspace_root().join("crates/rumoca-phase-solve/src/model_wire.rs"),
    ];
    let mut offenders = Vec::new();
    for root in roots {
        let mut files = Vec::new();
        if root.is_file() {
            files.push(root);
        } else {
            collect_rs_files(&root, &mut files);
        }
        files.sort();
        for file in files {
            let source = fs::read_to_string(&file)
                .unwrap_or_else(|error| panic!("read {}: {error}", file.display()));
            for offender in required_option_deserialize_offenders(&source) {
                offenders.push(format!("{}: {offender}", file.display()));
            }
        }
    }
    assert!(
        offenders.is_empty(),
        "every current Solve Deserialize Option must require its key and represent absence as explicit null: {offenders:#?}"
    );
}

#[test]
fn required_option_wire_gate_detects_a_planted_omission_reader() {
    let source = r#"
        struct CurrentWire {
            #[serde(default, skip_serializing_if = "Option::is_none")]
            optional: Option<String>,
        }
    "#;
    assert_eq!(
        required_option_wire_offenders(source, "CurrentWire", &["optional"]),
        ["optional"]
    );
}

#[test]
fn solve_required_option_census_detects_a_planted_bare_option() {
    let source = r#"
        #[derive(Deserialize)]
        struct CurrentSolveWire {
            semantic_absence: Option<String>,
        }
    "#;
    assert_eq!(
        required_option_deserialize_offenders(source),
        ["CurrentSolveWire.semantic_absence"]
    );
}

#[test]
fn solve_required_option_census_rejects_aliases_and_serde_bypasses() {
    let source = r#"
        type Maybe<T> = std::option::Option<T>;
        #[derive(Deserialize)]
        struct CurrentSolveWire {
            #[serde(skip)]
            skipped: Maybe<String>,
            #[serde(default)]
            defaulted: std::option::Option<String>,
            #[serde(deserialize_with = "different_decoder")]
            substituted: Option<String>,
        }
    "#;
    assert_eq!(
        required_option_deserialize_offenders(source),
        [
            "CurrentSolveWire.skipped",
            "CurrentSolveWire.defaulted",
            "CurrentSolveWire.substituted",
        ]
    );
}

#[test]
fn retired_galec_contraction_fission_cannot_return() {
    let phase = workspace_root().join("crates/rumoca-phase-galec/src");
    let retired_module = phase.join("lower/expression_projection/contraction_fission.rs");
    assert!(
        !retired_module.exists(),
        "the superseded GALEC contraction-fission pass must be deleted"
    );

    let mut files = Vec::new();
    collect_rs_files(&phase, &mut files);
    for file in files {
        let source = fs::read_to_string(&file)
            .unwrap_or_else(|error| panic!("read {}: {error}", file.display()));
        for retired in ["fission_contraction_body", "ContractionFission"] {
            assert!(
                !source.contains(retired),
                "retired GALEC contraction-fission symbol `{retired}` returned in {}",
                file.display()
            );
        }
    }
}

#[test]
fn retired_galec_representation_policy_cannot_return() {
    // This is a creep-back tripwire. The structural protection is the absent
    // GalecOptions input surface, which makes a representation choice
    // unrepresentable before DAE-to-GALEC lowering begins.
    let phase = workspace_root().join("crates/rumoca-phase-galec/src");
    assert!(
        !phase.join("lower/inline_policy.rs").exists(),
        "Algorithm Code representation policy belongs to no DAE-to-GALEC phase module"
    );

    for root in [
        workspace_root().join("crates/rumoca-ir-galec"),
        workspace_root().join("crates/rumoca-phase-galec"),
    ] {
        let mut files = Vec::new();
        collect_rs_files(&root, &mut files);
        for file in files {
            let source = fs::read_to_string(&file)
                .unwrap_or_else(|error| panic!("read {}: {error}", file.display()));
            for retired in [
                "EmissionPolicy",
                "InlinePolicy",
                "ScalarizePolicy",
                "emission_policy",
            ] {
                assert!(
                    !source.contains(retired),
                    "retired GALEC representation-policy symbol `{retired}` returned in {}",
                    file.display()
                );
            }
        }
    }
}
