//! SEV-005/SEV-162: rank is a value-type property, never a graph flavor.

use super::architecture_hardening_support::{production_rust_sources, workspace_root};
use proc_macro2::{TokenStream, TokenTree};
use quote::ToTokens;
use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};
use syn::visit::{self, Visit};
use syn::{
    Fields, ItemConst, ItemEnum, ItemFn, ItemImpl, ItemMod, ItemStatic, ItemStruct, ItemType,
    ItemUnion, Macro, Type,
};

const RUNTIME_TENSOR_EXECUTION_CONSUMERS: &[(&str, &[&str])] = &[
    (
        "crates/rumoca-eval-solve/src/lib.rs",
        &[
            "tensor_register_offset",
            "tensor_update_value_offset",
            "tensor_update_register_value_offset",
            "tensor_index_coordinate",
            "tensor_slice_coordinate_offset",
        ],
    ),
    (
        "crates/rumoca-exec-cranelift/src/emit/interpreter.rs",
        &[
            "tensor_register_offset",
            "tensor_update_value_offset",
            "tensor_index_coordinate",
            "execute_general_op",
        ],
    ),
    (
        "crates/rumoca-exec-cranelift/src/emit.rs",
        &[
            "lower_indexed_register",
            "lower_indexed_fold_carried",
            "lower_indexed_fold_capture",
            "constant_tensor_coordinate",
            "lower_tensor_offset",
            "lower_tensor_update",
            "lower_fold_tensor_update",
        ],
    ),
    (
        "crates/rumoca-phase-codegen/src/codegen/render_solve.rs",
        &["render_solve_op_typed"],
    ),
];

mod integer_range_cases;

#[test]
fn deleted_scalar_indexed_input_vocabulary_stays_absent_from_ir_wire_and_backends() {
    let root = workspace_root();
    let mut sources = workspace_production_sources(&root);
    let mut template_paths = Vec::new();
    collect_regular_files(
        &root.join("crates/rumoca-phase-codegen/src/templates"),
        &mut template_paths,
    );
    for path in template_paths {
        let source = fs::read_to_string(&path)
            .unwrap_or_else(|error| panic!("read {}: {error}", path.display()));
        let relative = path.strip_prefix(&root).unwrap_or(&path).to_path_buf();
        sources.push((relative, source));
    }

    let findings = sources
        .iter()
        .flat_map(|(path, source)| deleted_indexed_input_findings(path, source))
        .collect::<Vec<_>>();
    assert!(
        findings.is_empty(),
        "deleted scalar indexed-input IR/wire/backend vocabulary reappeared:\n{}",
        findings.join("\n")
    );
}

#[test]
fn deleted_scalar_indexed_input_mutations_are_detected() {
    for mutation in [
        "enum LinearOp { LoadIndexedP { dst: u32, base: usize, count: usize, index: u32 } }",
        "enum LinearOp { Load_Indexed_Seed { dst: u32 } }",
        "fn resolve_indexed_slot(value: f64) -> usize { value.round() as usize }",
        "#[serde(rename = \"LoadIndexedP\")] struct CompatibilityWire;",
        "const TAG: &str = concat!(\"Load\", \"Indexed\", \"Seed\");",
    ] {
        assert!(
            !deleted_indexed_input_findings(Path::new("mutation.rs"), mutation).is_empty(),
            "deleted scalar indexed-input mutation escaped: {mutation}"
        );
    }
}

fn deleted_indexed_input_findings(path: &Path, source: &str) -> Vec<String> {
    let normalized = source
        .chars()
        .filter(|character| character.is_ascii_alphanumeric())
        .flat_map(char::to_lowercase)
        .collect::<String>();
    [
        (
            "loadindexedp",
            "scalar indexed parameter operation or wire tag",
        ),
        (
            "loadindexedseed",
            "scalar indexed seed operation or wire tag",
        ),
        ("resolveindexedslot", "runtime indexed-slot repair helper"),
    ]
    .into_iter()
    .filter(|(needle, _)| normalized.contains(needle))
    .map(|(_, description)| format!("{}: {description}", path.display()))
    .collect()
}

fn collect_regular_files(root: &Path, paths: &mut Vec<PathBuf>) {
    let mut entries = fs::read_dir(root)
        .unwrap_or_else(|error| panic!("read {}: {error}", root.display()))
        .map(|entry| entry.expect("template directory entry").path())
        .collect::<Vec<_>>();
    entries.sort();
    for path in entries {
        if path.is_dir() {
            collect_regular_files(&path, paths);
        } else if path.is_file() {
            paths.push(path);
        }
    }
}

#[test]
fn runtime_tensor_index_fallback_mutations_are_detected() {
    for mutation in [
        "fn offset(value: f64) { let _ = value.is_finite(); }",
        "fn offset(value: f64) { let _ = value.round(); }",
        "fn offset() { let _ = f64::NAN; }",
        "fn offset(builder: Builder, value: Value) { builder.nearest(value); }",
        "fn offset(builder: Builder, value: Value) { builder.fmax(value, value); }",
        "fn offset(builder: Builder, value: Value) { builder.fmin(value, value); }",
        "fn offset(value: usize) { let _ = value.saturating_mul(value); }",
        "fn offset(value: f64) { let _ = value.clamp(1.0, 2.0); }",
        "fn offset(value: f64) { if value.is_nan() { return; } }",
        "fn offset(count: usize) { if count == 0 { return; } }",
    ] {
        assert!(
            !tensor_index_fallback_findings(mutation).is_empty(),
            "runtime tensor-index fallback mutation escaped: {mutation}"
        );
    }

    let direct_consumer_mutation = r#"
        fn backend_coordinate(index: TensorIndex) -> usize {
            match index {
                TensorIndex::Runtime(register) => read(register).round() as usize,
                TensorIndex::Constant(value) => value as usize,
            }
        }
    "#;
    let region = runtime_coordinate_audit(rust_function(
        direct_consumer_mutation,
        "backend_coordinate",
    ));
    assert!(!tensor_index_fallback_findings(&region.source).is_empty());

    let template =
        r#"{% if op.kind == "LoadIndexedRegister" %}{{ round(op.indices[0]) }}{% endif %}"#;
    assert!(template_consumes_runtime_tensor_coordinate(template));
    assert!(!tensor_index_fallback_findings(template).is_empty());
}

fn tensor_index_fallback_findings(source: &str) -> Vec<&'static str> {
    let compact = source
        .chars()
        .filter(|character| !character.is_whitespace())
        .collect::<String>();
    [
        (".is_finite()", "finite recheck"),
        (".is_nan()", "NaN recheck"),
        (".fract()", "fractional recheck"),
        (".round()", "integer recheck"),
        ("round(", "rendered integer repair"),
        (".clamp(", "range clamp"),
        ("clamp(", "rendered range clamp"),
        ("isnan(", "rendered NaN recheck"),
        ("f64::NAN", "NaN substitution"),
        (".nearest(", "native integer recheck"),
        (".fmax(", "native lower clamp"),
        (".fmin(", "native upper clamp"),
        (".saturating_mul(", "saturating extent fallback"),
        ("count==0", "empty-domain fallback"),
    ]
    .into_iter()
    .filter_map(|(needle, finding)| compact.contains(needle).then_some(finding))
    .collect()
}

struct RuntimeCoordinateAudit {
    source: String,
}

fn runtime_coordinate_audit(source: &str) -> RuntimeCoordinateAudit {
    let block = parse_audited_function_block(source);
    let mut visitor = RuntimeTensorCoordinateRegionVisitor::default();
    visitor.visit_block(&block);
    if visitor.regions.is_empty() {
        RuntimeCoordinateAudit {
            source: source.to_string(),
        }
    } else {
        RuntimeCoordinateAudit {
            source: visitor.regions.join("\n"),
        }
    }
}

fn parse_audited_function_block(source: &str) -> syn::Block {
    if let Ok(function) = syn::parse_str::<ItemFn>(source) {
        return *function.block;
    }
    let wrapper = format!("impl RuntimeCoordinateAudit {{ {source} }}");
    let implementation = syn::parse_str::<ItemImpl>(&wrapper)
        .unwrap_or_else(|error| panic!("parse audited method `{source}`: {error}"));
    implementation
        .items
        .into_iter()
        .find_map(|item| match item {
            syn::ImplItem::Fn(function) => Some(function.block),
            _ => None,
        })
        .expect("audited method wrapper contains its function")
}

#[derive(Default)]
struct RuntimeTensorCoordinateRegionVisitor {
    regions: Vec<String>,
}

impl Visit<'_> for RuntimeTensorCoordinateRegionVisitor {
    fn visit_arm(&mut self, arm: &syn::Arm) {
        let mut detector = RuntimeTensorCoordinatePathVisitor::default();
        detector.visit_arm(arm);
        if detector.found {
            self.regions.push(arm.to_token_stream().to_string());
            return;
        }
        visit::visit_arm(self, arm);
    }

    fn visit_expr_if(&mut self, expression: &syn::ExprIf) {
        let mut detector = RuntimeTensorCoordinatePathVisitor::default();
        detector.visit_expr_if(expression);
        if detector.found {
            self.regions.push(expression.to_token_stream().to_string());
            return;
        }
        visit::visit_expr_if(self, expression);
    }
}

fn runtime_tensor_coordinate_sites(sources: &[(PathBuf, String)]) -> BTreeSet<String> {
    let mut sites = BTreeSet::new();
    for (path, source) in sources {
        let syntax = syn::parse_file(source)
            .unwrap_or_else(|error| panic!("parse {}: {error}", path.display()));
        let mut visitor = RuntimeTensorCoordinateSiteVisitor::default();
        visitor.visit_file(&syntax);
        sites.extend(
            visitor
                .functions
                .into_iter()
                .map(|function| format!("{}::{function}", path.display())),
        );
    }
    sites
}

#[derive(Default)]
struct RuntimeTensorCoordinateSiteVisitor {
    functions: BTreeSet<String>,
}

impl Visit<'_> for RuntimeTensorCoordinateSiteVisitor {
    fn visit_item_fn(&mut self, function: &ItemFn) {
        if block_contains_runtime_tensor_coordinate(&function.block) {
            self.functions.insert(function.sig.ident.to_string());
        }
        visit::visit_item_fn(self, function);
    }

    fn visit_impl_item_fn(&mut self, function: &syn::ImplItemFn) {
        if block_contains_runtime_tensor_coordinate(&function.block) {
            self.functions.insert(function.sig.ident.to_string());
        }
        visit::visit_impl_item_fn(self, function);
    }
}

fn block_contains_runtime_tensor_coordinate(block: &syn::Block) -> bool {
    let mut visitor = RuntimeTensorCoordinatePathVisitor::default();
    visitor.visit_block(block);
    visitor.found
}

#[derive(Default)]
struct RuntimeTensorCoordinatePathVisitor {
    found: bool,
}

impl Visit<'_> for RuntimeTensorCoordinatePathVisitor {
    fn visit_path(&mut self, path: &syn::Path) {
        self.found |= path_ends_with(path, &["TensorIndex", "Runtime"])
            || path_ends_with(path, &["TensorUpdateSubscript", "Slice"]);
        visit::visit_path(self, path);
    }

    fn visit_macro(&mut self, item: &Macro) {
        let compact = item
            .tokens
            .to_string()
            .chars()
            .filter(|character| character.is_ascii_alphanumeric())
            .collect::<String>();
        self.found |= compact.contains("TensorIndexRuntime")
            || compact.contains("TensorUpdateSubscriptSlice");
        visit::visit_macro(self, item);
    }
}

fn runtime_tensor_coordinate_template_consumers(root: &Path) -> BTreeSet<PathBuf> {
    let mut paths = Vec::new();
    collect_regular_files(
        &root.join("crates/rumoca-phase-codegen/src/templates"),
        &mut paths,
    );
    paths
        .into_iter()
        .filter(|path| {
            let source = fs::read_to_string(path)
                .unwrap_or_else(|error| panic!("read {}: {error}", path.display()));
            template_consumes_runtime_tensor_coordinate(&source)
        })
        .map(|path| path.strip_prefix(root).unwrap_or(&path).to_path_buf())
        .collect()
}

fn template_consumes_runtime_tensor_coordinate(source: &str) -> bool {
    let explicit_variant = [
        "LoadIndexedRegister",
        "LoadIndexedFoldCarried",
        "LoadIndexedFoldCapture",
        "StoreOutputFoldTensorUpdate",
        "TensorUpdateSubscript",
    ]
    .into_iter()
    .any(|needle| source.contains(needle));
    let generic_solve_access = source.contains("solve_blocks")
        && (source.contains("op.indices") || source.contains("op.subscripts"));
    explicit_variant || generic_solve_access
}

fn rust_function<'a>(source: &'a str, name: &str) -> &'a str {
    let marker = format!("fn {name}");
    let start = source
        .match_indices(&marker)
        .find_map(|(start, _)| {
            matches!(
                source.as_bytes().get(start + marker.len()),
                Some(b'(' | b'<')
            )
            .then_some(start)
        })
        .unwrap_or_else(|| panic!("required function `{name}` is absent"));
    let open = source[start..]
        .find('{')
        .map(|offset| start + offset)
        .unwrap_or_else(|| panic!("required function `{name}` has no body"));
    let mut depth = 0usize;
    for (offset, byte) in source.as_bytes()[open..].iter().copied().enumerate() {
        match byte {
            b'{' => depth += 1,
            b'}' => {
                depth -= 1;
                if depth == 0 {
                    return &source[start..=open + offset];
                }
            }
            _ => {}
        }
    }
    panic!("required function `{name}` has an unterminated body")
}

fn rust_function_if_present<'a>(source: &'a str, name: &str) -> Option<&'a str> {
    source
        .contains(&format!("fn {name}"))
        .then(|| rust_function(source, name))
}

#[test]
fn compiler_has_no_scalar_tensor_graph_flavor_vocabulary() {
    let root = workspace_root();
    let mut findings = Vec::new();
    let sources = workspace_production_sources(&root);
    for required_owner in [
        "crates/rumoca-ir-solve/",
        "crates/rumoca-eval-solve/",
        "crates/rumoca-exec-cranelift/",
        "crates/rumoca-phase-autodiff/",
        "crates/rumoca-phase-solve/",
        "crates/rumoca-phase-codegen/",
    ] {
        assert!(
            sources
                .iter()
                .any(|(path, _)| path.to_string_lossy().starts_with(required_owner)),
            "workspace graph-flavor scan omitted required owner `{required_owner}`"
        );
    }
    for (path, source) in &sources {
        findings.extend(graph_flavor_findings(path, source));
    }
    findings.extend(workspace_paired_flavor_findings(&sources));
    findings.extend(call_transfer_workspace_findings(&sources));
    assert!(
        findings.is_empty(),
        "SEV-005/SEV-162 forbid an SX/MX-style scalar-versus-tensor graph mode, conversion, cache, AD path, or call ABI:\n{}",
        findings.join("\n")
    );
}

#[test]
fn typed_tensor_dag_storage_cannot_become_real_only() {
    let root = workspace_root();
    let types = fs::read_to_string(root.join("crates/rumoca-ir-solve/src/typed_program/types.rs"))
        .expect("read typed Solve value storage");
    let operations =
        fs::read_to_string(root.join("crates/rumoca-ir-solve/src/typed_program/program.rs"))
            .expect("read typed Solve operations");
    let tensor =
        fs::read_to_string(root.join("crates/rumoca-ir-solve/src/typed_program/program/tensor.rs"))
            .expect("read typed Solve tensor constructors");

    for required in [
        "Real { format: SolveRealFormat }",
        "Integer(SolveIntegerDomain)",
        "Boolean",
    ] {
        assert!(
            types.contains(required),
            "the one typed tensor DAG lost admitted scalar category `{required}`"
        );
    }
    assert!(
        types.contains("scalar: SolveScalarType")
            && types.contains("dimensions: Box<[u32]>")
            && operations.contains("pub enum SolveOperation"),
        "tensor rank and element category must remain properties of one typed value in one operation DAG"
    );
    let transpose = rust_function(&tensor, "transpose");
    assert!(
        transpose.contains("register_type") && transpose.contains("issue_register"),
        "transpose must preserve one checked typed aggregate without scalarization"
    );
    assert!(
        real_only_tensor_findings(Path::new("types.rs"), &types).is_empty(),
        "typed tensor storage acquired a Real-only assumption"
    );
}

#[test]
fn real_only_tensor_dag_mutations_are_detected() {
    for mutation in [
        "struct SolveValueType { real_format: SolveRealFormat, dimensions: Vec<u32> }",
        "struct TensorValue { elements: Vec<f64>, dimensions: Vec<u32> }",
        "struct RealTensorValue { dimensions: Vec<u32> }",
        "enum TensorElementType { Real32, Real64 }",
        "fn scalarize_tensor_before_graph(value: TensorValue) {}",
        "enum SolveOperation { RealTensorTranspose { operand: u32 } }",
    ] {
        assert!(
            !real_only_tensor_findings(Path::new("mutation.rs"), mutation).is_empty(),
            "Real-only tensor DAG mutation escaped: {mutation}"
        );
    }
}

fn real_only_tensor_findings(path: &Path, source: &str) -> Vec<String> {
    let normalized = source
        .chars()
        .filter(|character| character.is_ascii_alphanumeric())
        .flat_map(char::to_lowercase)
        .collect::<String>();
    [
        (
            "solvevaluetyperealformat",
            "Solve value storage embeds only a Real format",
        ),
        (
            "tensorvalueelementsvecf64",
            "tensor payload is hard-wired to f64",
        ),
        ("realtensorvalue", "parallel Real-only tensor value"),
        (
            "tensorelementtypereal32real64",
            "tensor element catalog contains only Real",
        ),
        (
            "scalarizetensorbeforegraph",
            "tensor value is scalarized before the typed DAG",
        ),
        (
            "realtensortranspose",
            "operation vocabulary split by Real tensor category",
        ),
    ]
    .into_iter()
    .filter(|(needle, _)| normalized.contains(needle))
    .map(|(_, description)| format!("{}: {description}", path.display()))
    .collect()
}

fn workspace_production_sources(root: &Path) -> Vec<(PathBuf, String)> {
    let mut crates = fs::read_dir(root.join("crates"))
        .expect("read workspace crates")
        .map(|entry| entry.expect("workspace crate entry").path())
        .filter(|path| path.join("Cargo.toml").is_file())
        .collect::<Vec<_>>();
    crates.sort();
    crates
        .into_iter()
        .flat_map(|crate_root| production_rust_sources(&crate_root, root))
        .collect()
}

#[test]
fn graph_flavor_source_and_wire_mutations_are_detected() {
    for mutation in [
        "enum GraphKind { Scalar, Tensor }",
        "struct ScalarGraphCache;",
        "struct MyScalarGraphCache;",
        "struct TensorGraphDirectionalPath;",
        "enum MslGraphMode { Scalar, Tensor }",
        "enum RankMode { Rank0, RankN }",
        "enum ExecutionRank { Scalar, Tensor }",
        "enum CallAbi { Scalar, Tensor }",
        "enum CallAbi { Rank0, RankN }",
        "enum CallAbi { Sx, Mx }",
        "enum CallAbi { ScalarPath, TensorPath }",
        "enum RankSelectedCall { Scalar, Tensor }",
        "struct ScalarCallPlan; struct TensorCallPlan;",
        "struct DirectScalarCallPlan; struct DirectTensorCallPlan;",
        "struct ScalarTransferPlan; struct TensorTransferPlan;",
        "struct ScalarCallCache; struct TensorCallCache;",
        "struct ScalarAutodiffCache; struct TensorAutodiffCache;",
        "struct ScalarProgramExecutor; struct TensorProgramExecutor;",
        "struct Routes { scalar_program_cache: u8, tensor_program_cache: u8 }",
        "static SCALAR_PROGRAM_CACHE: u8 = 0; static TENSOR_PROGRAM_CACHE: u8 = 0;",
        "mod scalar { struct CallPlan; } mod tensor { struct CallPlan; }",
        "struct CallPlan { scalar: Option<u8>, tensor: Option<u8> }",
        "struct CallPlan { tensor_mode: bool }",
        "struct GraphPlan { scalar: bool }",
        "struct ProgramPlan { tensor_mode: bool }",
        "struct ProgramPlan { is_scalar: bool }",
        "struct ExecutionPlan { scalar: bool }",
        "enum CallPlan { Unified { tensor_mode: bool } }",
        "enum ProgramPlan { Unified { tensor_mode: bool } }",
        "fn lower_scalar_call() {} fn lower_tensor_call() {}",
        "fn scalar_call() {} fn tensor_call() {}",
        "fn scalar_execute() {} fn tensor_execute() {}",
        "fn eval_scalar() {} fn eval_tensor() {}",
        "fn dispatch_scalar() {} fn dispatch_tensor() {}",
        "fn compile_scalar() {} fn compile_tensor() {}",
        "struct Runner; impl Runner { fn scalar_call(&self) {} fn tensor_call(&self) {} }",
        "trait Runner { fn scalar_execute(&self); fn tensor_execute(&self); }",
        "enum SolveOperation { Binary, ScalarBinary, TensorBinary }",
        "enum SolveOp { Binary, ScalarBinary, TensorBinary }",
        "enum SolveNode { ScalarBinary, TensorBinary }",
        "enum SolveExpr { ScalarBinary, TensorBinary }",
        "enum ValueOp { ScalarBinary, TensorBinary }",
        "enum BinaryOperation { Scalar, Tensor }",
        "struct ScalarBinaryOperation; struct TensorBinaryOperation;",
        "struct ScalarBinaryOp; struct TensorBinaryOp;",
        "struct Rank0Graph; struct RankNGraph;",
        "mod scalar { enum BinaryOperation { Add } } mod tensor { enum BinaryOperation { Add } }",
        "enum CallTransferPlan { Direct(u8), OwnerStaged(u8), Scalarized }",
        "struct CallTransferPlan;",
        "type CallTransferPlan = u8;",
        "#[non_exhaustive] enum CallTransferPlan { Direct(DirectCallTransfer), OwnerStaged(OwnerStagedCallTransfer) }",
        "#[derive(Default)] enum CallTransferPlan { #[default] Direct(DirectCallTransfer), OwnerStaged(OwnerStagedCallTransfer) }",
        "#[derive(core::default::Default)] enum CallTransferPlan { #[default] Direct(DirectCallTransfer), OwnerStaged(OwnerStagedCallTransfer) }",
        "enum CallTransferPlan { Direct(DirectCallTransfer), OwnerStaged(OwnerStagedCallTransfer) } impl Default for CallTransferPlan { fn default() -> Self { todo!() } }",
        "enum CallTransferPlan { Direct(DirectCallTransfer), OwnerStaged(OwnerStagedCallTransfer) } use std::default::Default as D; impl D for CallTransferPlan { fn default() -> Self { todo!() } }",
        "enum CallTransferPlan { Direct(DirectCallTransfer), OwnerStaged(OwnerStagedCallTransfer) } type Plan = CallTransferPlan; impl Default for Plan { fn default() -> Self { todo!() } }",
        "enum CallTransferPlan { Direct(DirectCallTransfer), OwnerStaged(OwnerStagedCallTransfer) } enum CallTransferPlan { Direct(DirectCallTransfer), OwnerStaged(OwnerStagedCallTransfer) }",
        "enum CallTransferPlan<T> { Direct(DirectCallTransfer), OwnerStaged(OwnerStagedCallTransfer) }",
        "enum CallTransferPlan { #[cfg(any())] Direct(DirectCallTransfer), OwnerStaged(OwnerStagedCallTransfer) }",
        "enum CallTransferPlan { Direct(other::DirectCallTransfer), OwnerStaged(OwnerStagedCallTransfer) }",
        "struct DirectCallTransfer; struct OwnerStagedCallTransfer;",
        "struct SolveAlgorithmBlock; struct DirectTransfer; struct StagedTransfer; enum TransferDisposition { Direct(DirectTransfer), OwnerStaged(StagedTransfer) }",
        "macro_rules! plan { () => { struct DirectCallTransfer; struct OwnerStagedCallTransfer; enum CallTransferPlan { Direct(DirectCallTransfer), OwnerStaged(OwnerStagedCallTransfer), Fallback } } } plan!();",
        "macro_rules! default_plan { () => { impl Default for CallTransferPlan { fn default() -> Self { todo!() } } } }",
        "paste! { enum CallTransferPlan { Direct(DirectCallTransfer), OwnerStaged(OwnerStagedCallTransfer), Fallback } }",
        "macro_rules! leaves { ($s:ident,$t:ident) => { enum SolveOp { $s, $t } } } leaves!(ScalarBinary, TensorBinary);",
        "fn scalar_to_tensor_graph() {}",
        "#[derive(serde::Serialize)] struct Wire { graph_mode: bool }",
        "#[derive(serde::Serialize)] #[serde(rename = \"mx_graph\")] struct Wire;",
        "macro_rules! wire { ($name:ident) => {} } wire!(scalar_call_plan);",
    ] {
        assert!(
            !complete_graph_flavor_findings(Path::new("mutation.rs"), mutation).is_empty(),
            "graph-flavor mutation escaped the SEV-162 gate: {mutation}"
        );
    }
    assert_graph_flavor_false_positive_controls();
}

fn assert_graph_flavor_false_positive_controls() {
    let controls = r#"
        //! graph_mode is forbidden architecture vocabulary.
        /* graph_mode is also forbidden in block comments. */
        enum MslFlamegraphMode { Disabled, Enabled }
        enum CallOperation { ElementwiseMap, AggregateReduction }
        struct ScalarType;
        struct TensorShape;
        struct ScalarScalePlan;
        struct ScalarInstructionEncoding(u32);
        struct ScalarTypeConversionOperation;
        enum ScalarTypeConversionOperationKind { IntegerToReal, RealToInteger }
        struct ArithmeticProfile;
        macro_rules! unrelated { (CallTransferPlan) => { struct Counts; }; }
        fn macro_literal() { format!("enum CallTransferPlan"); }
        fn ordinary() { let x = 1; /* graph_mode */ let _ = x; }
    "#;
    assert!(
        complete_graph_flavor_findings(Path::new("unrelated.rs"), controls).is_empty(),
        "comments and ordinary scalar/tensor type vocabulary must not be graph flavors"
    );
    let cross_file_routes = [
        (
            PathBuf::from("crates/example/src/scalar.rs"),
            "pub struct ProgramCache;".to_owned(),
        ),
        (
            PathBuf::from("crates/example/src/tensor.rs"),
            "pub struct ProgramCache;".to_owned(),
        ),
    ];
    assert!(
        !workspace_paired_flavor_findings(&cross_file_routes).is_empty(),
        "cross-file scalar/tensor execution routes escaped the workspace gate"
    );
    for (path, source) in [
        (
            "crates/rumoca-exec-cranelift/src/scalar_instruction.rs",
            "enum ScalarInstructionEncoding { Fadd, Fmul }",
        ),
        (
            "crates/rumoca-phase-codegen/src/scalar_instruction.rs",
            "enum ScalarInstructionEncoding { Add, Multiply }",
        ),
        (
            "crates/rumoca-ir-solve/src/conversion.rs",
            "struct ScalarTypeConversionOperation;",
        ),
        (
            "crates/rumoca-ir-solve/src/operation.rs",
            "enum SolveOperation { BroadcastBinary { scalar_on_lhs: bool } }",
        ),
    ] {
        assert!(
            complete_graph_flavor_findings(Path::new(path), source).is_empty(),
            "legal scalar-element/backend vocabulary was mistaken for a graph flavor: {source}"
        );
    }
}

#[test]
fn sev_162_positive_shared_leaf_witness_is_registered() {
    let path = workspace_root().join("crates/rumoca-ir-solve/src/typed_program/program/tests.rs");
    let source = fs::read_to_string(&path)
        .unwrap_or_else(|error| panic!("read {}: {error}", path.display()));
    assert!(
        has_shared_leaf_witness(&source),
        "SEV-162 requires its executable rank-0/rank-N shared-leaf witness"
    );
    for mutation in [
        "#[test] fn sev_162_rank_zero_and_rank_n_values_share_one_binary_operation_leaf() {}",
        "fn sev_162_rank_zero_and_rank_n_values_share_one_binary_operation_leaf() { let _ = SolveOperation::Binary; }",
        "#[cfg(any())] #[test] fn sev_162_rank_zero_and_rank_n_values_share_one_binary_operation_leaf() { stringify!(SolveOperation::Binary); }",
        "#[test] #[should_panic] fn sev_162_rank_zero_and_rank_n_values_share_one_binary_operation_leaf() { stringify!(SolveOperation::Binary); }",
        "#[test] fn sev_162_rank_zero_and_rank_n_values_share_one_binary_operation_leaf() { stringify!(SolveOperation::Binary); }",
    ] {
        assert!(
            !has_shared_leaf_witness(mutation),
            "incomplete SEV-162 positive witness escaped the registration gate"
        );
    }
    let weakened = source.replacen(
        "fn sev_162_rank_zero_and_rank_n_values_share_one_binary_operation_leaf() {",
        "fn sev_162_rank_zero_and_rank_n_values_share_one_binary_operation_leaf() { return;",
        1,
    );
    assert!(
        !has_shared_leaf_witness(&weakened),
        "an unreachable SEV-162 witness escaped the reviewed-body ratchet"
    );
}

const SHARED_LEAF_WITNESS_NORMALIZED_LEN: usize = 1_273;
const SHARED_LEAF_WITNESS_BLAKE3: &str =
    "e23ea847add7e1c12288a17e1f338de6da3a676b3d895be32e9f0beda8ed45df";

fn has_shared_leaf_witness(source: &str) -> bool {
    let Ok(syntax) = syn::parse_file(source) else {
        return false;
    };
    syntax.items.iter().any(|item| {
        let syn::Item::Fn(function) = item else {
            return false;
        };
        let forbidden_attribute = function.attrs.iter().any(|attribute| {
            attribute.path().is_ident("cfg")
                || attribute.path().is_ident("cfg_attr")
                || attribute.path().is_ident("ignore")
                || attribute.path().is_ident("should_panic")
        });
        let mut witness = SharedLeafWitnessVisitor::default();
        witness.visit_block(&function.block);
        let normalized_body = function.block.to_token_stream().to_string();
        function.sig.ident == "sev_162_rank_zero_and_rank_n_values_share_one_binary_operation_leaf"
            && !forbidden_attribute
            && function
                .attrs
                .iter()
                .any(|attribute| attribute.path().is_ident("test"))
            && witness.binary_calls == 2
            && witness.fill_calls >= 2
            && witness.register_type_calls >= 2
            && witness.binary_patterns == 1
            && witness.assert_macros >= 2
            && !witness.stringify_macro
            && normalized_body.len() == SHARED_LEAF_WITNESS_NORMALIZED_LEN
            && blake3::hash(normalized_body.as_bytes()).to_hex().as_str()
                == SHARED_LEAF_WITNESS_BLAKE3
    })
}

#[derive(Default)]
struct SharedLeafWitnessVisitor {
    binary_calls: usize,
    fill_calls: usize,
    register_type_calls: usize,
    binary_patterns: usize,
    assert_macros: usize,
    stringify_macro: bool,
}

impl Visit<'_> for SharedLeafWitnessVisitor {
    fn visit_expr_method_call(&mut self, expression: &syn::ExprMethodCall) {
        match expression.method.to_string().as_str() {
            "binary" => self.binary_calls += 1,
            "fill" => self.fill_calls += 1,
            "register_type" => self.register_type_calls += 1,
            _ => {}
        }
        visit::visit_expr_method_call(self, expression);
    }

    fn visit_pat_struct(&mut self, pattern: &syn::PatStruct) {
        if path_ends_with(&pattern.path, &["SolveOperation", "Binary"]) {
            self.binary_patterns += 1;
        }
        visit::visit_pat_struct(self, pattern);
    }

    fn visit_macro(&mut self, item: &Macro) {
        if item.path.is_ident("assert") || item.path.is_ident("assert_eq") {
            self.assert_macros += 1;
            self.register_type_calls += token_words(item.tokens.clone())
                .windows(2)
                .filter(|words| words == &["register", "type"])
                .count();
        }
        if item.path.is_ident("stringify") {
            self.stringify_macro = true;
        }
        visit::visit_macro(self, item);
    }
}

fn path_ends_with(path: &syn::Path, expected: &[&str]) -> bool {
    path.segments.len() >= expected.len()
        && path
            .segments
            .iter()
            .rev()
            .zip(expected.iter().rev())
            .all(|(segment, expected)| segment.ident == expected)
}

#[test]
fn solve_model_cannot_default_its_arithmetic_root() {
    let root = workspace_root();
    let crate_root = root.join("crates/rumoca-ir-solve");
    let findings = production_rust_sources(&crate_root, &root)
        .into_iter()
        .flat_map(|(path, source)| {
            solve_model_default_findings(&source)
                .into_iter()
                .map(move |finding| format!("{}: {finding}", path.display()))
        })
        .collect::<Vec<_>>();
    assert!(
        findings.is_empty(),
        "an executable Solve root must receive an explicit arithmetic profile: {findings:?}",
    );

    for mutation in [
        "#[derive(Default)] struct SolveModel;",
        "#[derive(core::default::Default)] struct SolveModel;",
        "struct SolveModel; impl Default for SolveModel { fn default() -> Self { Self } }",
        "use core::default::Default as D; struct SolveModel; impl D for SolveModel { fn default() -> Self { Self } }",
        "struct SolveModel; type ImplicitRoot = SolveModel; impl Default for ImplicitRoot { fn default() -> Self { SolveModel } }",
    ] {
        assert!(
            !solve_model_default_findings(mutation).is_empty(),
            "implicit arithmetic-root mutation escaped the architecture gate: {mutation}"
        );
    }
}

fn graph_flavor_findings(path: &Path, source: &str) -> Vec<String> {
    let syntax = syn::parse_file(source).unwrap_or_else(|error| {
        panic!(
            "parse {} for graph-flavor ownership: {error}",
            path.display()
        )
    });
    let mut visitor = GraphFlavorVisitor {
        path,
        findings: Vec::new(),
        module_words: Vec::new(),
        flavored_routes: Vec::new(),
    };
    visitor.visit_file(&syntax);
    visitor.record_paired_flavor_routes();
    visitor.findings.sort();
    visitor.findings.dedup();
    visitor.findings
}

fn complete_graph_flavor_findings(path: &Path, source: &str) -> Vec<String> {
    let mut findings = graph_flavor_findings(path, source);
    findings.extend(call_transfer_workspace_findings(&[(
        path.to_path_buf(),
        source.to_owned(),
    )]));
    findings
}

fn workspace_paired_flavor_findings(sources: &[(PathBuf, String)]) -> Vec<String> {
    let mut routes = Vec::new();
    for (path, source) in sources {
        let syntax = syn::parse_file(source).unwrap_or_else(|error| {
            panic!(
                "parse {} for paired graph-flavor routes: {error}",
                path.display()
            )
        });
        let mut visitor = GraphFlavorVisitor {
            path,
            findings: Vec::new(),
            module_words: Vec::new(),
            flavored_routes: Vec::new(),
        };
        visitor.visit_file(&syntax);
        routes.extend(visitor.flavored_routes);
    }
    paired_flavor_route_findings(&routes, "workspace")
}

fn call_transfer_workspace_findings(sources: &[(PathBuf, String)]) -> Vec<String> {
    let mut declarations = Vec::new();
    let mut payload_declarations = Vec::new();
    let mut block_declarations = Vec::new();
    let mut findings = Vec::new();
    for (path, source) in sources {
        let syntax = syn::parse_file(source).unwrap_or_else(|error| {
            panic!(
                "parse {} for CallTransferPlan ownership: {error}",
                path.display()
            )
        });
        let mut visitor = CallTransferContractVisitor {
            path,
            declarations: &mut declarations,
            payload_declarations: &mut payload_declarations,
            block_declarations: &mut block_declarations,
            findings: &mut findings,
        };
        visitor.visit_file(&syntax);
    }
    if declarations.len() > 1 {
        findings.push(format!(
            "CallTransferPlan has {} declarations; the closed sum must have exactly one owner",
            declarations.len()
        ));
    }
    if !payload_declarations.is_empty() && declarations.len() != 1 {
        findings.push(format!(
            "CallTransferPlan payloads exist at {} but the exact closed-sum owner count is {}",
            payload_declarations.join(", "),
            declarations.len()
        ));
    }
    if !block_declarations.is_empty() && declarations.len() != 1 {
        findings.push(format!(
            "SolveAlgorithmBlock exists at {} but the exact CallTransferPlan owner count is {}",
            block_declarations.join(", "),
            declarations.len()
        ));
    }
    findings.sort();
    findings.dedup();
    findings
}

struct CallTransferContractVisitor<'a> {
    path: &'a Path,
    declarations: &'a mut Vec<String>,
    payload_declarations: &'a mut Vec<String>,
    block_declarations: &'a mut Vec<String>,
    findings: &'a mut Vec<String>,
}

impl CallTransferContractVisitor<'_> {
    fn record_payload(&mut self, identifier: &syn::Ident) {
        if identifier == "DirectCallTransfer" || identifier == "OwnerStagedCallTransfer" {
            self.payload_declarations
                .push(format!("{}::{identifier}", self.path.display()));
        }
    }

    fn record_non_enum_plan(&mut self, kind: &str) {
        self.declarations
            .push(format!("{}::{kind}", self.path.display()));
        self.findings.push(format!(
            "{}: CallTransferPlan is declared as {kind}, not the required enum",
            self.path.display()
        ));
    }
}

impl Visit<'_> for CallTransferContractVisitor<'_> {
    fn visit_item_enum(&mut self, item: &ItemEnum) {
        self.record_payload(&item.ident);
        if item.ident == "SolveAlgorithmBlock" {
            self.block_declarations
                .push(format!("{}::SolveAlgorithmBlock", self.path.display()));
        }
        if item.ident == "CallTransferPlan" {
            self.declarations
                .push(format!("{}::enum", self.path.display()));
            if !exact_call_transfer_plan(item) {
                self.findings.push(format!(
                    "{}: CallTransferPlan is not the exact non-defaulted Direct/OwnerStaged closed sum",
                    self.path.display()
                ));
            }
        }
        visit::visit_item_enum(self, item);
    }

    fn visit_item_struct(&mut self, item: &ItemStruct) {
        self.record_payload(&item.ident);
        if item.ident == "SolveAlgorithmBlock" {
            self.block_declarations
                .push(format!("{}::SolveAlgorithmBlock", self.path.display()));
        }
        if item.ident == "CallTransferPlan" {
            self.record_non_enum_plan("struct");
        }
        visit::visit_item_struct(self, item);
    }

    fn visit_item_type(&mut self, item: &ItemType) {
        self.record_payload(&item.ident);
        if item.ident == "SolveAlgorithmBlock" {
            self.block_declarations
                .push(format!("{}::SolveAlgorithmBlock", self.path.display()));
        }
        if type_ends_with(&item.ty, "CallTransferPlan") {
            self.findings.push(format!(
                "{}: `{}` aliases the invariant-bearing CallTransferPlan",
                self.path.display(),
                item.ident
            ));
        }
        if item.ident == "CallTransferPlan" {
            self.record_non_enum_plan("type alias");
        }
        visit::visit_item_type(self, item);
    }

    fn visit_item_union(&mut self, item: &ItemUnion) {
        self.record_payload(&item.ident);
        if item.ident == "SolveAlgorithmBlock" {
            self.block_declarations
                .push(format!("{}::SolveAlgorithmBlock", self.path.display()));
        }
        if item.ident == "CallTransferPlan" {
            self.record_non_enum_plan("union");
        }
        visit::visit_item_union(self, item);
    }

    fn visit_item_impl(&mut self, item: &ItemImpl) {
        let targets_plan = matches!(
            item.self_ty.as_ref(),
            Type::Path(path)
                if path.path.segments.last().is_some_and(|segment| segment.ident == "CallTransferPlan")
        );
        if item.trait_.is_some() && targets_plan {
            self.findings.push(format!(
                "{}: CallTransferPlan must not acquire a trait implementation outside its closed declaration",
                self.path.display()
            ));
        }
        visit::visit_item_impl(self, item);
    }

    fn visit_macro(&mut self, item: &Macro) {
        let declarations = macro_declared_identifiers(item.tokens.clone());
        let declares_transfer_owner = declarations.iter().any(|words| {
            words.as_slice() == ["call", "transfer", "plan"]
                || words.as_slice() == ["direct", "call", "transfer"]
                || words.as_slice() == ["owner", "staged", "call", "transfer"]
        });
        let implements_transfer_owner =
            macro_implements_identifier(item.tokens.clone(), &["call", "transfer", "plan"]);
        if declares_transfer_owner || implements_transfer_owner {
            self.findings.push(format!(
                "{}: a macro may not generate CallTransferPlan or its invariant-bearing payloads",
                self.path.display()
            ));
        }
        if declarations
            .iter()
            .any(|words| words.as_slice() == ["solve", "algorithm", "block"])
        {
            self.findings.push(format!(
                "{}: a macro may not generate the invariant-bearing SolveAlgorithmBlock",
                self.path.display()
            ));
        }
        visit::visit_macro(self, item);
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum ExecutionFlavor {
    Scalar,
    Tensor,
}

struct FlavoredRoute {
    flavor: ExecutionFlavor,
    signature: String,
    spelling: String,
}

struct GraphFlavorVisitor<'path> {
    path: &'path Path,
    findings: Vec<String>,
    module_words: Vec<String>,
    flavored_routes: Vec<FlavoredRoute>,
}

impl Visit<'_> for GraphFlavorVisitor<'_> {
    fn visit_attribute(&mut self, attribute: &syn::Attribute) {
        if !attribute.path().is_ident("doc") {
            if let syn::Meta::List(list) = &attribute.meta {
                self.record_tokens(list.tokens.clone());
            }
            visit::visit_attribute(self, attribute);
        }
    }

    fn visit_ident(&mut self, identifier: &syn::Ident) {
        self.record(identifier.to_string());
        visit::visit_ident(self, identifier);
    }

    fn visit_item_mod(&mut self, item: &ItemMod) {
        let depth = self.module_words.len();
        let mut context = self.path_words();
        context.extend(self.module_words.iter().cloned());
        let item_words = identifier_words(&item.ident.to_string());
        context.extend(item_words.iter().cloned());
        self.record_flavored_route_declaration(&context, &item_words, item.ident.to_string());
        self.module_words.extend(item_words);
        visit::visit_item_mod(self, item);
        self.module_words.truncate(depth);
    }

    fn visit_item_struct(&mut self, item: &ItemStruct) {
        let mut context = self.path_words();
        context.extend(self.module_words.iter().cloned());
        let item_words = identifier_words(&item.ident.to_string());
        context.extend(item_words.iter().cloned());
        self.record_flavored_route_declaration(&context, &item_words, item.ident.to_string());
        for field in &item.fields {
            self.record_field_route(&context, field);
        }
        let fields = item
            .fields
            .iter()
            .filter_map(|field| field.ident.as_ref())
            .flat_map(|identifier| identifier_words(&identifier.to_string()))
            .collect::<Vec<_>>();
        if ((owns_call_route(&item_words) || owns_transfer_route(&item_words))
            && (has_primary_scalar_flavor(&context) || has_primary_tensor_flavor(&context)))
            || (selects_executable_shape(&item_words)
                && has_primary_scalar_flavor(&fields)
                && has_primary_tensor_flavor(&fields))
            || (owns_rank_selector(&item_words)
                && item.fields.iter().any(|field| {
                    field.ident.as_ref().is_some_and(|identifier| {
                        let words = identifier_words(&identifier.to_string());
                        is_rank_selector_field(&item_words, &words) && is_bool_type(&field.ty)
                    })
                }))
        {
            self.findings.push(format!(
                "{}: `{}` owns a scalar/tensor-selected execution path",
                self.path.display(),
                item.ident
            ));
        }
        visit::visit_item_struct(self, item);
    }

    fn visit_item_enum(&mut self, item: &ItemEnum) {
        let mut enum_words = self.path_words();
        enum_words.extend(self.module_words.iter().cloned());
        let item_words = identifier_words(&item.ident.to_string());
        enum_words.extend(item_words.iter().cloned());
        self.record_flavored_route_declaration(&enum_words, &item_words, item.ident.to_string());
        for variant in &item.variants {
            let mut variant_context = enum_words.clone();
            variant_context.extend(identifier_words(&variant.ident.to_string()));
            for field in &variant.fields {
                self.record_field_route(&variant_context, field);
            }
        }
        let variants = item
            .variants
            .iter()
            .map(|variant| identifier_words(&variant.ident.to_string()))
            .collect::<Vec<_>>();
        let scalar_variant = variants
            .iter()
            .any(|words| has_primary_scalar_flavor(words));
        let tensor_variant = variants
            .iter()
            .any(|words| has_primary_tensor_flavor(words));
        if scalar_variant && tensor_variant && selects_executable_shape(&enum_words) {
            self.findings.push(format!(
                "{}: `{}` selects scalar versus tensor execution",
                self.path.display(),
                item.ident
            ));
        }
        if owns_operation_leaf(&item_words)
            && semantic_operation_scope(self.path)
            && !owns_scalar_element_vocabulary(&enum_words)
            && (scalar_variant
                || tensor_variant
                || has_primary_scalar_flavor(&enum_words)
                || has_primary_tensor_flavor(&enum_words))
        {
            self.findings.push(format!(
                "{}: `{}` owns a rank-flavored operation leaf",
                self.path.display(),
                item.ident
            ));
        }
        if owns_rank_selector(&item_words)
            && item
                .variants
                .iter()
                .flat_map(|variant| variant.fields.iter())
                .any(|field| {
                    field.ident.as_ref().is_some_and(|identifier| {
                        let words = identifier_words(&identifier.to_string());
                        is_rank_selector_field(&item_words, &words) && is_bool_type(&field.ty)
                    })
                })
        {
            self.findings.push(format!(
                "{}: `{}` carries a Boolean scalar/tensor selector",
                self.path.display(),
                item.ident
            ));
        }
        if item.ident == "CallTransferPlan" && !exact_call_transfer_plan(item) {
            self.findings.push(format!(
                "{}: `CallTransferPlan` is not the exact Direct/OwnerStaged closed sum",
                self.path.display()
            ));
        }
        visit::visit_item_enum(self, item);
    }

    fn visit_item_fn(&mut self, item: &ItemFn) {
        let mut context = self.path_words();
        context.extend(self.module_words.iter().cloned());
        context.extend(identifier_words(&item.sig.ident.to_string()));
        let item_words = identifier_words(&item.sig.ident.to_string());
        self.record_flavored_route_declaration(&context, &item_words, item.sig.ident.to_string());
        visit::visit_item_fn(self, item);
    }

    fn visit_item_const(&mut self, item: &ItemConst) {
        let mut context = self.path_words();
        context.extend(self.module_words.iter().cloned());
        let item_words = identifier_words(&item.ident.to_string());
        context.extend(item_words.iter().cloned());
        self.record_flavored_route_declaration(&context, &item_words, item.ident.to_string());
        visit::visit_item_const(self, item);
    }

    fn visit_item_static(&mut self, item: &ItemStatic) {
        let mut context = self.path_words();
        context.extend(self.module_words.iter().cloned());
        let item_words = identifier_words(&item.ident.to_string());
        context.extend(item_words.iter().cloned());
        self.record_flavored_route_declaration(&context, &item_words, item.ident.to_string());
        visit::visit_item_static(self, item);
    }

    fn visit_impl_item_fn(&mut self, item: &syn::ImplItemFn) {
        let mut context = self.path_words();
        context.extend(self.module_words.iter().cloned());
        context.extend(identifier_words(&item.sig.ident.to_string()));
        let item_words = identifier_words(&item.sig.ident.to_string());
        self.record_flavored_route_declaration(&context, &item_words, item.sig.ident.to_string());
        visit::visit_impl_item_fn(self, item);
    }

    fn visit_trait_item_fn(&mut self, item: &syn::TraitItemFn) {
        let mut context = self.path_words();
        context.extend(self.module_words.iter().cloned());
        context.extend(identifier_words(&item.sig.ident.to_string()));
        let item_words = identifier_words(&item.sig.ident.to_string());
        self.record_flavored_route_declaration(&context, &item_words, item.sig.ident.to_string());
        visit::visit_trait_item_fn(self, item);
    }

    fn visit_item_type(&mut self, item: &ItemType) {
        let mut context = self.path_words();
        context.extend(self.module_words.iter().cloned());
        context.extend(identifier_words(&item.ident.to_string()));
        let item_words = identifier_words(&item.ident.to_string());
        self.record_flavored_route_declaration(&context, &item_words, item.ident.to_string());
        if item.ident == "CallTransferPlan" {
            self.findings.push(format!(
                "{}: `CallTransferPlan` must be an enum, not a type alias",
                self.path.display()
            ));
        }
        visit::visit_item_type(self, item);
    }

    fn visit_item_union(&mut self, item: &ItemUnion) {
        let mut context = self.path_words();
        context.extend(self.module_words.iter().cloned());
        context.extend(identifier_words(&item.ident.to_string()));
        let item_words = identifier_words(&item.ident.to_string());
        self.record_flavored_route_declaration(&context, &item_words, item.ident.to_string());
        if item.ident == "CallTransferPlan" {
            self.findings.push(format!(
                "{}: `CallTransferPlan` must be an enum, not a union",
                self.path.display()
            ));
        }
        visit::visit_item_union(self, item);
    }

    fn visit_item_impl(&mut self, item: &ItemImpl) {
        let implements_default = item.trait_.as_ref().is_some_and(|(_, path, _)| {
            path.segments
                .last()
                .is_some_and(|segment| segment.ident == "Default")
        });
        let targets_plan = matches!(
            item.self_ty.as_ref(),
            Type::Path(path)
                if path.path.segments.last().is_some_and(|segment| segment.ident == "CallTransferPlan")
        );
        if implements_default && targets_plan {
            self.findings.push(format!(
                "{}: `CallTransferPlan` must not implement Default",
                self.path.display()
            ));
        }
        visit::visit_item_impl(self, item);
    }

    fn visit_macro(&mut self, item: &Macro) {
        let words = token_words(item.tokens.clone());
        self.record_tokens(item.tokens.clone());
        if !words.is_empty() {
            let spelling = words.join("_");
            self.record_flavored_route_declaration(&words, &words, spelling.clone());
            if macro_declares_semantic_owner(self.path, item.tokens.clone()) {
                self.findings.push(format!(
                    "{}: macro `{spelling}` can generate an invariant-bearing execution owner",
                    self.path.display()
                ));
            }
            self.record(spelling);
        }
        visit::visit_macro(self, item);
    }

    fn visit_lit_str(&mut self, literal: &syn::LitStr) {
        self.record(literal.value());
        visit::visit_lit_str(self, literal);
    }
}

impl GraphFlavorVisitor<'_> {
    fn path_words(&self) -> Vec<String> {
        self.path
            .components()
            .flat_map(|component| identifier_words(&component.as_os_str().to_string_lossy()))
            .collect()
    }

    fn record_tokens(&mut self, tokens: TokenStream) {
        for token in tokens {
            match token {
                TokenTree::Group(group) => self.record_tokens(group.stream()),
                TokenTree::Ident(identifier) => self.record(identifier.to_string()),
                TokenTree::Literal(literal) => self.record(literal.to_string()),
                TokenTree::Punct(_) => {}
            }
        }
    }

    fn record_flavored_route_declaration(
        &mut self,
        context_words: &[String],
        declaration_words: &[String],
        spelling: String,
    ) {
        let scalar = has_primary_scalar_flavor(context_words);
        let tensor = has_primary_tensor_flavor(context_words);
        if scalar == tensor || !owns_execution_route(declaration_words) {
            return;
        }
        let signature = route_signature(context_words);
        self.flavored_routes.push(FlavoredRoute {
            flavor: if scalar {
                ExecutionFlavor::Scalar
            } else {
                ExecutionFlavor::Tensor
            },
            signature,
            spelling,
        });
    }

    fn record_field_route(&mut self, owner_context: &[String], field: &syn::Field) {
        let Some(identifier) = &field.ident else {
            return;
        };
        let field_words = identifier_words(&identifier.to_string());
        let mut context = owner_context.to_vec();
        context.extend(field_words.iter().cloned());
        self.record_flavored_route_declaration(&context, &field_words, identifier.to_string());
    }

    fn record_paired_flavor_routes(&mut self) {
        self.findings.extend(paired_flavor_route_findings(
            &self.flavored_routes,
            &self.path.display().to_string(),
        ));
    }

    fn record(&mut self, spelling: String) {
        let words = identifier_words(&spelling);
        let has_adjacent = |lhs: &str, rhs: &str| {
            words
                .windows(2)
                .any(|pair| pair[0] == lhs && pair[1] == rhs)
        };
        let graph_flavor_selector = ["kind", "mode", "flavor", "flavour"]
            .iter()
            .any(|selector| has_adjacent("graph", selector));
        let scalar_flavor = ["scalar", "sx", "mx"]
            .iter()
            .any(|flavor| has_adjacent(flavor, "graph"));
        let rank_graph = has_adjacent("rank0", "graph")
            || has_adjacent("rankn", "graph")
            || words.windows(3).any(|part| {
                part == ["rank", "zero", "graph"]
                    || part == ["rank", "0", "graph"]
                    || part == ["rank", "n", "graph"]
                    || part == ["rank", "many", "graph"]
            });
        let tensor_graph = has_adjacent("tensor", "graph");
        let tensor_flavor_owner = tensor_graph
            && (words.len() == 2
                || [
                    "cache",
                    "abi",
                    "autodiff",
                    "directional",
                    "call",
                    "plan",
                    "transfer",
                    "program",
                    "executor",
                ]
                .iter()
                .any(|owner| words.iter().any(|word| word == owner)));
        let flavored_executable_route = (has_primary_scalar_flavor(&words)
            || has_primary_tensor_flavor(&words))
            && (owns_call_route(&words) || owns_transfer_route(&words));
        let conversion = words.windows(4).any(|part| {
            part == ["scalar", "to", "tensor", "graph"]
                || part == ["tensor", "to", "scalar", "graph"]
        }) || words
            .windows(3)
            .any(|part| part == ["to", "scalar", "graph"] || part == ["to", "tensor", "graph"]);
        let forbidden = graph_flavor_selector
            || scalar_flavor
            || rank_graph
            || tensor_flavor_owner
            || flavored_executable_route
            || conversion;
        if forbidden {
            self.findings
                .push(format!("{}: `{spelling}`", self.path.display()));
        }
    }
}

fn paired_flavor_route_findings(routes: &[FlavoredRoute], location: &str) -> Vec<String> {
    let mut findings = Vec::new();
    for (index, route) in routes.iter().enumerate() {
        if let Some(peer) = routes
            .iter()
            .skip(index + 1)
            .find(|peer| peer.flavor != route.flavor && peer.signature == route.signature)
        {
            findings.push(format!(
                "{location}: `{}` and `{}` form separate scalar/tensor execution routes",
                route.spelling, peer.spelling
            ));
        }
    }
    findings.sort();
    findings.dedup();
    findings
}

fn has_primary_scalar_flavor(words: &[String]) -> bool {
    words
        .iter()
        .any(|word| ["scalar", "scalarized", "scalarised", "sx", "rank0"].contains(&word.as_str()))
        || words
            .windows(2)
            .any(|part| part == ["rank", "0"] || part == ["rank", "zero"])
}

fn has_primary_tensor_flavor(words: &[String]) -> bool {
    words
        .iter()
        .any(|word| ["tensor", "mx", "rankn"].contains(&word.as_str()))
        || words
            .windows(2)
            .any(|part| part == ["rank", "n"] || part == ["rank", "many"])
}

fn is_primary_flavor_word(word: &str) -> bool {
    [
        "scalar",
        "scalarized",
        "scalarised",
        "sx",
        "tensor",
        "mx",
        "rank0",
        "rankn",
    ]
    .contains(&word)
}

fn route_signature(words: &[String]) -> String {
    let mut signature = Vec::new();
    let mut index = 0;
    while index < words.len() {
        if is_primary_flavor_word(&words[index]) {
            index += 1;
            continue;
        }
        if words[index] == "rank"
            && words
                .get(index + 1)
                .is_some_and(|word| ["0", "zero", "n", "many"].contains(&word.as_str()))
        {
            index += 2;
            continue;
        }
        signature.push(words[index].clone());
        index += 1;
    }
    signature.join("_")
}

fn owns_execution_route(words: &[String]) -> bool {
    words.iter().any(|word| {
        [
            "call",
            "cache",
            "autodiff",
            "ad",
            "program",
            "executor",
            "execution",
            "execute",
            "eval",
            "evaluate",
            "dispatch",
            "compile",
            "operation",
            "op",
            "opcode",
            "instruction",
            "node",
            "expr",
            "bytecode",
        ]
        .contains(&word.as_str())
    })
}

fn owns_operation_leaf(words: &[String]) -> bool {
    words
        .iter()
        .any(|word| ["operation", "opcode", "instruction", "bytecode"].contains(&word.as_str()))
        || (words
            .iter()
            .any(|word| ["op", "node", "expr"].contains(&word.as_str()))
            && words.iter().any(|part| {
                [
                    "solve",
                    "value",
                    "invoke",
                    "effect",
                    "terminator",
                    "binary",
                    "unary",
                    "call",
                    "arithmetic",
                ]
                .contains(&part.as_str())
            }))
}

fn semantic_operation_scope(path: &Path) -> bool {
    let path = path.to_string_lossy();
    path == "mutation.rs"
        || [
            "crates/rumoca-ir-solve/",
            "crates/rumoca-phase-solve/",
            "crates/rumoca-phase-autodiff/",
        ]
        .iter()
        .any(|owner| path.starts_with(owner))
}

fn owns_scalar_element_vocabulary(words: &[String]) -> bool {
    words.windows(2).any(|part| {
        part == ["scalar", "type"] || part == ["scalar", "value"] || part == ["scalar", "element"]
    })
}

fn macro_declares_semantic_owner(path: &Path, tokens: TokenStream) -> bool {
    macro_declared_identifiers(tokens).iter().any(|words| {
        selects_executable_shape(words)
            || (semantic_operation_scope(path) && owns_operation_leaf(words))
    })
}

fn macro_declared_identifiers(tokens: TokenStream) -> Vec<Vec<String>> {
    let tokens = tokens.into_iter().collect::<Vec<_>>();
    let mut declarations = Vec::new();
    for (index, token) in tokens.iter().enumerate() {
        if let TokenTree::Group(group) = token {
            declarations.extend(macro_declared_identifiers(group.stream()));
            continue;
        }
        let TokenTree::Ident(keyword) = token else {
            continue;
        };
        if !["enum", "struct", "union", "type"].contains(&keyword.to_string().as_str()) {
            continue;
        }
        if let Some(TokenTree::Ident(identifier)) = tokens.iter().skip(index + 1).find(|next| {
            matches!(next, TokenTree::Ident(_))
                || matches!(next, TokenTree::Group(_) | TokenTree::Literal(_))
        }) {
            declarations.push(identifier_words(&identifier.to_string()));
        }
    }
    declarations
}

fn macro_implements_identifier(tokens: TokenStream, expected: &[&str]) -> bool {
    let words = macro_identifier_words(tokens);
    words.iter().enumerate().any(|(index, word)| {
        let end = index.saturating_add(expected.len() + 3).min(words.len());
        word == "for"
            && words
                .get(index + 1..end)
                .is_some_and(|tail| tail.windows(expected.len()).any(|part| part == expected))
    })
}

fn macro_identifier_words(tokens: TokenStream) -> Vec<String> {
    let mut words = Vec::new();
    for token in tokens {
        match token {
            TokenTree::Group(group) => words.extend(macro_identifier_words(group.stream())),
            TokenTree::Ident(identifier) => {
                words.extend(identifier_words(&identifier.to_string()));
            }
            TokenTree::Literal(_) | TokenTree::Punct(_) => {}
        }
    }
    words
}

fn is_bool_type(ty: &Type) -> bool {
    matches!(
        ty,
        Type::Path(path)
            if path.qself.is_none()
                && path.path.segments.len() == 1
                && path.path.segments[0].ident == "bool"
    )
}

fn owns_call_route(words: &[String]) -> bool {
    words.iter().any(|word| word == "call")
        && words.iter().any(|word| {
            [
                "plan",
                "abi",
                "mode",
                "kind",
                "flavor",
                "flavour",
                "cache",
                "autodiff",
                "ad",
                "executor",
                "execution",
                "lower",
                "render",
                "emit",
                "transfer",
            ]
            .contains(&word.as_str())
        })
}

fn owns_transfer_route(words: &[String]) -> bool {
    words.iter().any(|word| word == "transfer") && words.iter().any(|word| word == "plan")
}

fn selects_executable_shape(words: &[String]) -> bool {
    words
        .iter()
        .any(|word| ["graph", "dag", "call", "abi", "transfer"].contains(&word.as_str()))
        || (words
            .iter()
            .any(|word| word == "program" || word == "execution")
            && words
                .iter()
                .any(|word| ["mode", "kind", "plan", "rank"].contains(&word.as_str())))
        || (words.iter().any(|word| word == "rank")
            && words
                .iter()
                .any(|word| ["mode", "kind", "flavor", "flavour", "plan"].contains(&word.as_str())))
        || (words.iter().any(|word| word == "transfer") && words.iter().any(|word| word == "plan"))
}

fn owns_rank_selector(words: &[String]) -> bool {
    selects_executable_shape(words)
        || words.iter().any(|word| {
            [
                "program",
                "execution",
                "operation",
                "opcode",
                "cache",
                "autodiff",
                "executor",
            ]
            .contains(&word.as_str())
        })
}

fn is_rank_selector_field(owner_words: &[String], field_words: &[String]) -> bool {
    let rank_flavored =
        has_primary_scalar_flavor(field_words) || has_primary_tensor_flavor(field_words);
    if !rank_flavored {
        return false;
    }
    let explicitly_selects_rank = field_words.iter().any(|word| {
        [
            "mode",
            "kind",
            "flavor",
            "flavour",
            "rank",
            "graph",
            "dag",
            "path",
            "is",
            "use",
            "uses",
            "has",
            "enable",
            "enabled",
            "prefer",
            "preferred",
        ]
        .contains(&word.as_str())
    });
    explicitly_selects_rank || (field_words.len() == 1 && selects_executable_shape(owner_words))
}

fn type_ends_with(ty: &Type, expected: &str) -> bool {
    matches!(
        ty,
        Type::Path(path)
            if path.path.segments.last().is_some_and(|segment| segment.ident == expected)
    )
}

fn exact_call_transfer_plan(item: &ItemEnum) -> bool {
    let forbidden_attribute = item.attrs.iter().any(|attribute| {
        attribute.path().is_ident("non_exhaustive")
            || attribute.path().is_ident("cfg")
            || attribute.path().is_ident("cfg_attr")
    }) || derives_trait(&item.attrs, "Default");
    if forbidden_attribute
        || !item.generics.params.is_empty()
        || item.generics.where_clause.is_some()
        || item.variants.len() != 2
    {
        return false;
    }
    [
        ("Direct", "DirectCallTransfer"),
        ("OwnerStaged", "OwnerStagedCallTransfer"),
    ]
    .iter()
    .all(|(variant_name, payload_name)| {
        item.variants.iter().any(|variant| {
            variant.ident == variant_name
                && variant.discriminant.is_none()
                && !has_conditional_attribute(&variant.attrs)
                && matches!(
                    &variant.fields,
                    Fields::Unnamed(fields)
                        if fields.unnamed.len() == 1
                            && !has_conditional_attribute(&fields.unnamed[0].attrs)
                            && matches!(
                                &fields.unnamed[0].ty,
                                Type::Path(path)
                                    if path.qself.is_none()
                                        && path.path.segments.len() == 1
                                        && path.path.segments[0].ident == payload_name
                            )
                )
        })
    })
}

fn has_conditional_attribute(attributes: &[syn::Attribute]) -> bool {
    attributes
        .iter()
        .any(|attribute| attribute.path().is_ident("cfg") || attribute.path().is_ident("cfg_attr"))
}

fn derives_trait(attributes: &[syn::Attribute], expected: &str) -> bool {
    attributes.iter().any(|attribute| {
        attribute.path().is_ident("derive")
            && attribute
                .parse_args_with(
                    syn::punctuated::Punctuated::<syn::Path, syn::Token![,]>::parse_terminated,
                )
                .is_ok_and(|paths| {
                    paths.iter().any(|path| {
                        path.segments
                            .last()
                            .is_some_and(|segment| segment.ident == expected)
                    })
                })
    })
}

fn token_words(tokens: TokenStream) -> Vec<String> {
    let mut words = Vec::new();
    for token in tokens {
        match token {
            TokenTree::Group(group) => words.extend(token_words(group.stream())),
            TokenTree::Ident(identifier) => {
                words.extend(identifier_words(&identifier.to_string()));
            }
            TokenTree::Literal(literal) => {
                words.extend(identifier_words(&literal.to_string()));
            }
            TokenTree::Punct(_) => {}
        }
    }
    words
}

fn identifier_words(spelling: &str) -> Vec<String> {
    let characters = spelling.chars().collect::<Vec<_>>();
    let mut words = Vec::new();
    let mut current = String::new();
    for (index, character) in characters.iter().copied().enumerate() {
        if !character.is_ascii_alphanumeric() {
            if !current.is_empty() {
                words.push(current.to_ascii_lowercase());
                current.clear();
            }
            continue;
        }
        let previous = index.checked_sub(1).and_then(|prior| characters.get(prior));
        let next = characters.get(index + 1);
        let camel_boundary = character.is_ascii_uppercase()
            && previous.is_some_and(|prior| {
                prior.is_ascii_lowercase()
                    || prior.is_ascii_digit()
                    || (prior.is_ascii_uppercase() && next.is_some_and(char::is_ascii_lowercase))
            });
        if camel_boundary && !current.is_empty() {
            words.push(current.to_ascii_lowercase());
            current.clear();
        }
        current.push(character);
    }
    if !current.is_empty() {
        words.push(current.to_ascii_lowercase());
    }
    words
}

fn solve_model_default_findings(source: &str) -> Vec<String> {
    let syntax = syn::parse_file(source)
        .unwrap_or_else(|error| panic!("parse SolveModel arithmetic-root owner: {error}"));
    let mut aliases = DefaultTraitAliasVisitor {
        names: vec!["Default".to_owned()],
    };
    aliases.visit_file(&syntax);
    let mut visitor = SolveModelDefaultVisitor {
        findings: Vec::new(),
        default_trait_names: aliases.names,
    };
    visitor.visit_file(&syntax);
    visitor.findings
}

struct SolveModelDefaultVisitor {
    findings: Vec<String>,
    default_trait_names: Vec<String>,
}

impl Visit<'_> for SolveModelDefaultVisitor {
    fn visit_item_struct(&mut self, item: &ItemStruct) {
        if item.ident == "SolveModel" && derives_trait(&item.attrs, "Default") {
            self.findings
                .push("#[derive(Default)] SolveModel".to_owned());
        }
        visit::visit_item_struct(self, item);
    }

    fn visit_item_impl(&mut self, item: &ItemImpl) {
        let implements_default = item.trait_.as_ref().is_some_and(|(_, path, _)| {
            path.segments.last().is_some_and(|segment| {
                self.default_trait_names
                    .iter()
                    .any(|name| segment.ident == name)
            })
        });
        let targets_solve_model = matches!(
            item.self_ty.as_ref(),
            Type::Path(path) if path.path.segments.last().is_some_and(|segment| segment.ident == "SolveModel")
        );
        if implements_default && targets_solve_model {
            self.findings.push("impl Default for SolveModel".to_owned());
        }
        visit::visit_item_impl(self, item);
    }

    fn visit_item_type(&mut self, item: &ItemType) {
        if type_ends_with(&item.ty, "SolveModel") {
            self.findings.push(format!(
                "type alias `{}` can hide an implicit SolveModel constructor",
                item.ident
            ));
        }
        visit::visit_item_type(self, item);
    }
}

struct DefaultTraitAliasVisitor {
    names: Vec<String>,
}

impl Visit<'_> for DefaultTraitAliasVisitor {
    fn visit_use_tree(&mut self, tree: &syn::UseTree) {
        if let syn::UseTree::Rename(rename) = tree
            && rename.ident == "Default"
        {
            self.names.push(rename.rename.to_string());
        }
        visit::visit_use_tree(self, tree);
    }
}
