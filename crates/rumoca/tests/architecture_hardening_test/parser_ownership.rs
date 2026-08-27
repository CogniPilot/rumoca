//! Parser ownership gates for pure IR crates (SPEC_0029 §3).

use super::*;
use std::path::{Path, PathBuf};
use syn::visit::Visit;

const PARSER_DEPENDENCIES: &[&str] = &[
    "lalrpop",
    "lalrpop-util",
    "parol",
    "parol_runtime",
    "pest",
    "pest_derive",
    "scnr2",
    "tree-sitter",
];

const GRAMMAR_EXTENSIONS: &[&str] = &["grammar", "lalrpop", "par", "pest"];

const GENERATED_PARSER_MARKERS: &[&str] = &[
    "parol_runtime",
    "scnr2",
    "LookaheadDFA",
    "GrammarAuto",
    "grammar_trait",
];

#[test]
fn test_ir_crates_do_not_own_source_parsers() {
    let root = workspace_root();
    let mut offenders = Vec::new();
    for crate_dir in ir_crate_directories(&root.join("crates")) {
        inspect_ir_crate(&crate_dir, &mut offenders);
    }
    assert!(
        offenders.is_empty(),
        "IR crates are pure checked data and must not own source parsers; \
move parser grammar, generated code, state, diagnostics, dependencies, and \
features into a rumoca-phase-parse* crate: {offenders:#?}"
    );
}

#[test]
fn test_galec_parser_public_api_returns_checked_data() {
    let root = workspace_root().join("crates/rumoca-phase-parse-galec/src");
    let public_api = fs::read_to_string(root.join("lib.rs")).expect("read GALEC parser API");
    let parser = fs::read_to_string(root.join("parse/mod.rs")).expect("read GALEC parser");

    assert!(
        public_api.contains(
            "pub fn parse(source: &str, file_name: &str) -> Result<CheckedAlgorithmBlock, GalecParseError>"
        ),
        "the production GALEC parse entry must return the opaque checked block"
    );
    assert!(
        !public_api.contains("pub use parse::{")
            && !public_api.contains("pub fn parse_expression")
            && parser.contains("pub(crate) fn parse_block")
            && parser.contains("pub(crate) fn parse_expression"),
        "raw GALEC block/expression parsing must remain private to the parse phase"
    );
}

#[test]
fn test_kani_inventory_consumes_one_parsed_rust_file() {
    let path = workspace_root().join("crates/xtask/src/verify_cmd/kani.rs");
    let source = fs::read_to_string(&path).expect("read Kani verification driver");
    let syntax = syn::parse_file(&source)
        .unwrap_or_else(|error| panic!("parse {} as Rust: {error}", path.display()));
    let discovery = item_function(&syntax, "discover_workspace_proofs");
    let inventory = item_function(&syntax, "discover_file_proofs");

    let first_input = inventory
        .sig
        .inputs
        .first()
        .expect("proof inventory has a parsed-source input");
    assert!(
        argument_is_reference_to(first_input, "File"),
        "Kani proof inventory must consume parsed `syn::File`, never source text"
    );

    let mut calls = SynParseFileCall::default();
    calls.visit_block(&discovery.block);
    assert!(
        calls.found,
        "workspace proof discovery must parse each Rust source with `syn::parse_file`"
    );
}

#[test]
fn test_flatten_dimension_inference_reuses_parsed_ast_and_borrowed_facts() {
    let path =
        workspace_root().join("crates/rumoca-phase-flatten/src/pipeline/constant_injection.rs");
    let source = fs::read_to_string(&path).expect("read flatten constant injection");
    let syntax = syn::parse_file(&source)
        .unwrap_or_else(|error| panic!("parse {} as Rust: {error}", path.display()));
    let inference = item_function(&syntax, "infer_dims_via_eval_ast");

    let expression = inference
        .sig
        .inputs
        .first()
        .expect("dimension inference has an expression input");
    assert!(
        argument_is_reference_to(expression, "Expression"),
        "flatten dimension inference must consume the parser-owned AST expression"
    );
    assert!(
        implements_trait_for(
            &syntax,
            "DimensionInferenceContext",
            "FlattenDimensionContext"
        ),
        "flatten must expose live semantic facts through the typed dimension-inference contract"
    );

    let mut calls = DimensionInferenceCalls::default();
    calls.visit_block(&inference.block);
    assert!(
        calls.shared_ast_walk && calls.borrowed_flatten_context,
        "flatten must pass its borrowed semantic context to the shared parsed-AST dimension walk"
    );
    assert!(
        !calls.constructs_typecheck_context,
        "flatten must not clone semantic maps into a parallel TypeCheckEvalContext"
    );
}

#[test]
fn test_parse_diagnostics_consume_typed_parser_facts_once() {
    let root = workspace_root();
    let errors_path = root.join("crates/rumoca-phase-parse/src/errors.rs");
    let errors_source = fs::read_to_string(&errors_path).expect("read parser diagnostics");
    let errors = syn::parse_file(&errors_source)
        .unwrap_or_else(|error| panic!("parse {} as Rust: {error}", errors_path.display()));

    let parse_error = errors
        .items
        .iter()
        .find_map(|item| match item {
            syn::Item::Enum(item) if item.ident == "ParseError" => Some(item),
            _ => None,
        })
        .expect("ParseError enum");
    for variant in &parse_error.variants {
        let span = variant
            .fields
            .iter()
            .find(|field| field.ident.as_ref().is_some_and(|ident| ident == "span"))
            .unwrap_or_else(|| {
                panic!("ParseError::{} must carry parser provenance", variant.ident)
            });
        let syn::Type::Path(span_type) = &span.ty else {
            panic!(
                "ParseError::{} span must be the concrete Span type",
                variant.ident
            );
        };
        assert!(
            span_type
                .path
                .segments
                .last()
                .is_some_and(|segment| segment.ident == "Span"),
            "ParseError::{} span must be non-optional",
            variant.ident
        );
    }

    let conversion = item_function(&errors, "convert_syntax_error");
    let mut cause_reads = CauseFieldReads::default();
    cause_reads.visit_block(&conversion.block);
    assert_eq!(
        cause_reads.count, 0,
        "parse diagnostics must consume Parol's structured unexpected-token/location fields, not reparse its Display cause"
    );
    assert!(
        !errors_source.contains("SourceId::DUMMY"),
        "parser diagnostics must retain the source identity established at the parse boundary"
    );

    for (path, obsolete_helpers) in [
        (
            "crates/rumoca-compile/src/session/strict_compile_diagnostics.rs",
            &["remap_parse_span"] as &[&str],
        ),
        (
            "crates/rumoca-tool-lsp/src/handlers/diagnostics.rs",
            &[
                "range_from_message_location",
                "range_from_textual_line_hint",
                "extract_la1_token",
            ],
        ),
    ] {
        let source = fs::read_to_string(root.join(path)).expect("read diagnostic consumer");
        let syntax = syn::parse_file(&source)
            .unwrap_or_else(|error| panic!("parse {path} as Rust: {error}"));
        for helper in obsolete_helpers {
            assert!(
                !syntax.items.iter().any(
                    |item| matches!(item, syn::Item::Fn(function) if function.sig.ident == *helper)
                ),
                "{path} must consume parser-owned spans directly; found `{helper}`"
            );
        }
    }
}

fn item_function<'a>(syntax: &'a syn::File, name: &str) -> &'a syn::ItemFn {
    syntax
        .items
        .iter()
        .find_map(|item| match item {
            syn::Item::Fn(function) if function.sig.ident == name => Some(function),
            _ => None,
        })
        .unwrap_or_else(|| panic!("expected function `{name}`"))
}

fn argument_is_reference_to(argument: &syn::FnArg, expected: &str) -> bool {
    let syn::FnArg::Typed(argument) = argument else {
        return false;
    };
    let syn::Type::Reference(reference) = argument.ty.as_ref() else {
        return false;
    };
    let syn::Type::Path(path) = reference.elem.as_ref() else {
        return false;
    };
    path.path
        .segments
        .last()
        .is_some_and(|segment| segment.ident == expected)
}

fn implements_trait_for(syntax: &syn::File, trait_name: &str, self_type: &str) -> bool {
    syntax.items.iter().any(|item| {
        let syn::Item::Impl(implementation) = item else {
            return false;
        };
        let Some((_, trait_path, _)) = &implementation.trait_ else {
            return false;
        };
        let syn::Type::Path(implemented_for) = implementation.self_ty.as_ref() else {
            return false;
        };
        trait_path
            .segments
            .last()
            .is_some_and(|segment| segment.ident == trait_name)
            && implemented_for
                .path
                .segments
                .last()
                .is_some_and(|segment| segment.ident == self_type)
    })
}

#[derive(Default)]
struct DimensionInferenceCalls {
    shared_ast_walk: bool,
    borrowed_flatten_context: bool,
    constructs_typecheck_context: bool,
}

impl<'ast> Visit<'ast> for DimensionInferenceCalls {
    fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
        if let syn::Expr::Path(function) = call.func.as_ref() {
            self.shared_ast_walk |=
                function.path.segments.last().is_some_and(|segment| {
                    segment.ident == "infer_dimensions_from_binding_with_scope"
                });
            self.constructs_typecheck_context |= function
                .path
                .segments
                .iter()
                .any(|segment| segment.ident == "TypeCheckEvalContext")
                && function
                    .path
                    .segments
                    .last()
                    .is_some_and(|segment| segment.ident == "new");
        }
        syn::visit::visit_expr_call(self, call);
    }

    fn visit_expr_struct(&mut self, expression: &'ast syn::ExprStruct) {
        let terminal = expression
            .path
            .segments
            .last()
            .map(|segment| &segment.ident);
        self.borrowed_flatten_context |=
            terminal.is_some_and(|ident| ident == "FlattenDimensionContext");
        self.constructs_typecheck_context |=
            terminal.is_some_and(|ident| ident == "TypeCheckEvalContext");
        syn::visit::visit_expr_struct(self, expression);
    }

    fn visit_expr_method_call(&mut self, call: &'ast syn::ExprMethodCall) {
        if call.method == "new"
            && let syn::Expr::Path(receiver) = call.receiver.as_ref()
        {
            self.constructs_typecheck_context |= receiver
                .path
                .segments
                .last()
                .is_some_and(|segment| segment.ident == "TypeCheckEvalContext");
        }
        syn::visit::visit_expr_method_call(self, call);
    }
}

#[derive(Default)]
struct SynParseFileCall {
    found: bool,
}

#[derive(Default)]
struct CauseFieldReads {
    count: usize,
}

impl<'ast> Visit<'ast> for CauseFieldReads {
    fn visit_expr_field(&mut self, field: &'ast syn::ExprField) {
        if matches!(&field.member, syn::Member::Named(name) if name == "cause") {
            self.count += 1;
        }
        syn::visit::visit_expr_field(self, field);
    }
}

impl<'ast> Visit<'ast> for SynParseFileCall {
    fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
        if let syn::Expr::Path(function) = call.func.as_ref() {
            let mut segments = function.path.segments.iter();
            self.found |= segments
                .next()
                .is_some_and(|segment| segment.ident == "syn")
                && segments
                    .next()
                    .is_some_and(|segment| segment.ident == "parse_file")
                && segments.next().is_none();
        }
        syn::visit::visit_expr_call(self, call);
    }
}

fn ir_crate_directories(crates_dir: &Path) -> Vec<PathBuf> {
    let mut directories = fs::read_dir(crates_dir)
        .expect("read crates directory")
        .filter_map(Result::ok)
        .map(|entry| entry.path())
        .filter(|path| {
            path.is_dir()
                && path
                    .file_name()
                    .is_some_and(|name| name.to_string_lossy().starts_with("rumoca-ir-"))
        })
        .collect::<Vec<_>>();
    directories.sort();
    directories
}

fn inspect_ir_crate(crate_dir: &Path, offenders: &mut Vec<String>) {
    let source_parser = crate_dir.join("src/parse");
    if source_parser.exists() {
        offenders.push(source_parser.display().to_string());
    }

    collect_grammar_files(&crate_dir.join("src"), offenders);
    collect_generated_parser_sources(&crate_dir.join("src"), offenders);
    inspect_build_script(crate_dir, offenders);
    inspect_manifest(crate_dir, offenders);
}

fn collect_grammar_files(directory: &Path, offenders: &mut Vec<String>) {
    let Ok(entries) = fs::read_dir(directory) else {
        return;
    };
    for entry in entries.filter_map(Result::ok) {
        let path = entry.path();
        if path.is_dir() {
            collect_grammar_files(&path, offenders);
        } else if path.extension().is_some_and(|extension| {
            GRAMMAR_EXTENSIONS
                .iter()
                .any(|candidate| extension == *candidate)
        }) {
            offenders.push(path.display().to_string());
        }
    }
}

fn collect_generated_parser_sources(directory: &Path, offenders: &mut Vec<String>) {
    let Ok(entries) = fs::read_dir(directory) else {
        return;
    };
    for entry in entries.filter_map(Result::ok) {
        let path = entry.path();
        if path.is_dir() {
            collect_generated_parser_sources(&path, offenders);
            continue;
        }
        let file_name = path
            .file_name()
            .map(|name| name.to_string_lossy())
            .unwrap_or_default();
        let parser_named = file_name.contains("grammar") || file_name.contains("parser");
        let generated_marker = fs::read_to_string(&path).is_ok_and(|content| {
            GENERATED_PARSER_MARKERS
                .iter()
                .any(|marker| content.contains(marker))
        });
        if parser_named || generated_marker {
            offenders.push(path.display().to_string());
        }
    }
}

fn inspect_build_script(crate_dir: &Path, offenders: &mut Vec<String>) {
    let build_script = crate_dir.join("build.rs");
    let Ok(content) = fs::read_to_string(&build_script) else {
        return;
    };
    if PARSER_DEPENDENCIES
        .iter()
        .any(|dependency| content.contains(dependency))
    {
        offenders.push(build_script.display().to_string());
    }
}

fn inspect_manifest(crate_dir: &Path, offenders: &mut Vec<String>) {
    let manifest = crate_dir.join("Cargo.toml");
    let content = fs::read_to_string(&manifest).expect("read IR crate manifest");
    for dependency in PARSER_DEPENDENCIES {
        if manifest_declares_key(&content, dependency) {
            offenders.push(format!("{}: dependency `{dependency}`", manifest.display()));
        }
    }
    for feature in manifest_feature_names(&content) {
        if feature.contains("parse") || feature.contains("parser") {
            offenders.push(format!(
                "{}: parser feature `{feature}`",
                manifest.display()
            ));
        }
    }
}

fn manifest_declares_key(content: &str, key: &str) -> bool {
    content.lines().any(|line| {
        let line = line.split('#').next().unwrap_or_default().trim();
        line.split_once('=')
            .is_some_and(|(candidate, _)| candidate.trim() == key)
    })
}

fn manifest_feature_names(content: &str) -> Vec<&str> {
    let mut in_features = false;
    let mut features = Vec::new();
    for line in content.lines() {
        let line = line.split('#').next().unwrap_or_default().trim();
        if line.starts_with('[') {
            in_features = line == "[features]";
            continue;
        }
        if in_features && let Some((name, _)) = line.split_once('=') {
            features.push(name.trim());
        }
    }
    features
}
