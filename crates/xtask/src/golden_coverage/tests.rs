use std::fs;
use std::num::NonZeroUsize;
use std::path::Path;

use syn::visit::Visit as _;
use xtask::golden_registry::{
    CoverageTest, ModelClaim, candidate_only_aggregate_report, check_candidate_capture,
    check_candidate_only_registry, checked_aggregate_denominator_history,
    ensure_exact_test_identity, parse_golden_registry,
};

use super::report::{format_ranges, read_previous_denominator, write_json};
use super::schema::{RustItemId, RustItemKind, RustItemScope, RustModuleId};
use super::source_analysis::{resolve_rust_item_capture, validate_rust_item_capture};
use super::{
    CaptureLock, DENOMINATOR_CARGO_ARGS, FileFootprint, analyze_source, cargo_test_target_args,
    denominator_cargo_args, invalidate_model_evidence, normalize_lcov, parse_lcov,
    production_relative_path, validate_exact_test_listing,
};

#[test]
fn prior_aggregate_reader_accepts_only_the_current_aggregate_schema() {
    let directory = tempfile::tempdir().expect("aggregate fixture");
    let path = directory.path().join("aggregate.json");
    let workspace = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("xtask has a workspace root");
    let registry = parse_golden_registry(
        include_str!("../../../../infra/verification/golden-models/registry.toml"),
        "checked fixture",
    )
    .expect("strict schema-2 registry");
    let admission = check_candidate_only_registry(workspace, registry)
        .expect("live registry contains candidates only");
    let denominator = NonZeroUsize::new(42).expect("nonzero denominator");
    let history = checked_aggregate_denominator_history(denominator, None)
        .expect("representable denominator history");
    let report = candidate_only_aggregate_report(admission, history);
    write_json(&path, &report).expect("current aggregate");
    assert_eq!(
        read_previous_denominator(&path).expect("read current"),
        Some(denominator)
    );

    let legacy = fs::read_to_string(&path).expect("read aggregate").replacen(
        "\"aggregate_schema_version\": 2",
        "\"aggregate_schema_version\": 1",
        1,
    );
    fs::write(&path, legacy).expect("legacy aggregate");
    assert!(read_previous_denominator(&path).is_err());

    write_json(&path, &report).expect("restore current aggregate");
    let ambiguous = fs::read_to_string(&path).expect("read aggregate").replacen(
        "aggregate_schema_version",
        "schema_version",
        1,
    );
    fs::write(&path, ambiguous).expect("ambiguous aggregate schema field");
    assert!(read_previous_denominator(&path).is_err());
}

#[test]
fn lcov_parser_unions_duplicate_line_records_and_keeps_instantiation_names() {
    let parsed = parse_lcov("SF:/repo/a.rs\nFN:4,generic::<f64>\nFNDA:2,generic::<f64>\nDA:4,2\nDA:5,0\nend_of_record\nSF:/repo/a.rs\nDA:5,3\nend_of_record\n")
        .expect("parse LCOV fixture");
    let file = &parsed[Path::new("/repo/a.rs")];
    assert_eq!(file.lines[&4], 2);
    assert_eq!(file.lines[&5], 3);
    assert_eq!(file.function_counts["generic::<f64>"], 2);
}

#[test]
fn covered_lines_render_as_compact_review_ranges() {
    assert_eq!(
        format_ranges(&[1, 2, 3, 7, 9, 10]).expect("sorted line ranges"),
        "1-3, 7, 9-10"
    );
    assert!(format_ranges(&[2, 1]).is_err());
}

#[test]
fn malformed_production_source_rejects_coverage_normalization() {
    let directory = tempfile::tempdir().expect("temporary source directory");
    let path = directory.path().join("broken.rs");
    fs::write(&path, "fn incomplete(").expect("write malformed source");

    let error = match analyze_source(&path) {
        Ok(_) => panic!("malformed source must fail closed"),
        Err(error) => error,
    };
    assert!(
        error
            .to_string()
            .contains("failed to parse production source"),
        "unexpected error: {error:#}"
    );
}

#[test]
fn cfg_test_classification_is_directional_and_covers_impl_members() {
    let directory = tempfile::tempdir().expect("temporary source directory");
    let path = directory.path().join("cfg_shapes.rs");
    fs::write(
        &path,
        "struct Product;\n#[cfg(not(test))]\nfn production() {}\n#[cfg(test)]\nimpl Product {\n    fn helper() {}\n}\n#[cfg(any(test, feature = \"diagnostics\"))]\nfn feature_product() {}\n#[cfg(all(test, feature = \"diagnostics\"))]\nfn test_helper() {}\n",
    )
    .expect("write cfg source");

    let analysis = analyze_source(&path).expect("analyze valid cfg source");
    assert!(!analysis.test_lines.contains(&2));
    assert!(!analysis.test_lines.contains(&3));
    assert!(analysis.test_lines.contains(&4));
    assert!(analysis.test_lines.contains(&6));
    assert!(!analysis.test_lines.contains(&8));
    assert!(!analysis.test_lines.contains(&9));
    assert!(analysis.test_lines.contains(&10));
    assert!(analysis.test_lines.contains(&11));
}

#[test]
fn production_classification_uses_exact_crate_identity_not_substrings() {
    let root = Path::new("/repo");
    assert_eq!(
        production_relative_path(root, Path::new("/repo/crates/contest-product/src/lib.rs")),
        Some("crates/contest-product/src/lib.rs".to_string())
    );
    assert_eq!(
        production_relative_path(root, Path::new("/repo/crates/rumoca-test-msl/src/lib.rs")),
        None
    );
    assert_eq!(
        production_relative_path(root, Path::new("/repo/crates/xtask/src/main.rs")),
        None
    );
}

#[test]
fn exact_test_identity_rejects_libtest_option_injection() {
    ensure_exact_test_identity("pipeline_test::one_model").expect("valid exact test identity");
    for invalid in ["", "--include-ignored", "pipeline_test::", "pipeline test"] {
        assert!(
            ensure_exact_test_identity(invalid).is_err(),
            "accepted invalid identity `{invalid}`"
        );
    }
}

#[test]
fn exact_test_listing_rejects_zero_or_multiple_matches() {
    let name = "pipeline_test::one_model";
    validate_exact_test_listing(name, "pipeline_test::one_model: test\n").expect("one exact match");
    assert!(validate_exact_test_listing(name, "").is_err());
    assert!(
        validate_exact_test_listing(
            name,
            "pipeline_test::one_model: test\npipeline_test::other: test\n"
        )
        .is_err()
    );
}

/// Every live schema-2 record is a checked candidate capture. No candidate
/// can return a golden/admitted authority in the candidate-only cut.
#[test]
fn live_registry_is_strict_schema_two_and_admission_matches_its_claim() {
    let registry = include_str!("../../../../infra/verification/golden-models/registry.toml");
    let parsed =
        parse_golden_registry(registry, "checked fixture").expect("strict schema-2 registry");
    assert!(!parsed.models().is_empty());
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("xtask has workspace root");
    for model in parsed.models() {
        assert!(matches!(model.claim, ModelClaim::Candidate(_)));
        let checked = check_candidate_capture(root, model)
            .unwrap_or_else(|error| panic!("live candidate must validate: {error:#}"));
        assert_eq!(checked.id(), model.id);
    }

    let legacy = registry.replacen("schema_version = 2", "schema_version = 1", 1);
    assert!(parse_golden_registry(&legacy, "legacy fixture").is_err());

    let ambiguous = registry.replacen("profile =", "status = \"candidate\"\nprofile =", 1);
    assert!(parse_golden_registry(&ambiguous, "schema ambiguity fixture").is_err());
}

/// No legacy schema identifier or golden authority survives. The checked
/// candidate carrier has one returner and one construction site, both in the
/// validator, and derives no clone/default/wire route.
#[test]
fn schema_two_architecture_forbids_compatibility_and_unchecked_admission() {
    let manifest = Path::new(env!("CARGO_MANIFEST_DIR"));
    let workspace = manifest
        .parent()
        .and_then(Path::parent)
        .expect("xtask has a workspace root");
    assert!(
        !workspace
            .join("crates/rumoca/tests/architecture_hardening_test/golden_model_registry.rs")
            .exists(),
        "the retired duplicate registry checker was recreated"
    );
    let rumoca_architecture = fs::read_to_string(
        workspace.join("crates/rumoca/tests/architecture_hardening_test/main.rs"),
    )
    .expect("read rumoca architecture module roster");
    assert!(!rumoca_architecture.contains("mod golden_model_registry;"));
    for retired in [
        "src/golden_coverage/admission.rs",
        "src/golden_coverage/admission_tests.rs",
    ] {
        assert!(
            !manifest.join(retired).exists(),
            "retired duplicate authority `{retired}` was recreated"
        );
    }
    let mut sources: Vec<_> = walkdir::WalkDir::new(manifest.join("src"))
        .into_iter()
        .map(Result::unwrap)
        .filter(|entry| entry.path().extension().is_some_and(|value| value == "rs"))
        .map(walkdir::DirEntry::into_path)
        .collect();
    sources.sort();
    let mut legacy = LegacyIdentifierVisitor::default();
    let mut checked = CheckedCandidateVisitor::default();
    for path in sources {
        let source = fs::read_to_string(&path).expect("read golden architecture source");
        let syntax = syn::parse_file(&source).expect("parse golden architecture source");
        legacy.visit_file(&syntax);
        checked.visit_file(&syntax);
    }
    assert!(
        legacy.identifiers.is_empty(),
        "legacy schema identifiers remain"
    );
    assert!(
        checked.golden_identifiers.is_empty(),
        "a golden authority exists before reviewed admission is implemented"
    );
    assert_eq!(checked.returners, ["check_candidate_capture"]);
    assert_eq!(checked.constructions, 1);
    assert_eq!(checked.definitions, 1);
    assert!(!checked.forbidden_derive);
    assert_eq!(checked.public_fields, 0);
    assert_eq!(checked.forbidden_impls, 0);
    assert_eq!(checked.registry_definitions, 1);
    assert_eq!(checked.profile_definitions, 1);
    assert_eq!(checked.endpoint_definitions, 1);
    assert_eq!(checked.registry_admission_definitions, 1);
    assert_eq!(checked.registry_admission_constructions, 1);
    assert_eq!(
        checked.registry_admission_returners,
        ["check_candidate_only_registry"]
    );
    assert_eq!(checked.registry_admission_public_fields, 0);
    assert!(!checked.registry_admission_forbidden_derive);
    assert_eq!(checked.registry_admission_forbidden_impls, 0);
    assert_eq!(checked.aggregate_report_definitions, 1);
    assert_eq!(checked.aggregate_report_constructions, 1);
    assert_eq!(
        checked.aggregate_report_returners,
        ["candidate_only_aggregate_report"]
    );
    assert_eq!(checked.aggregate_report_public_fields, 0);
    assert!(!checked.aggregate_report_forbidden_derive);
    assert_eq!(checked.aggregate_report_forbidden_impls, 0);
    assert_eq!(checked.aggregate_report_mutable_methods, 0);
    assert_eq!(checked.denominator_history_definitions, 1);
    assert_eq!(checked.denominator_history_constructions, 1);
    assert_eq!(
        checked.denominator_history_returners,
        ["checked_aggregate_denominator_history"]
    );
    assert_eq!(checked.denominator_history_public_fields, 0);
    assert!(!checked.denominator_history_forbidden_derive);
    assert_eq!(checked.denominator_history_forbidden_impls, 0);
}

#[test]
fn candidate_capture_route_consumes_only_the_checked_candidate() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("src/golden_coverage.rs");
    let source = fs::read_to_string(path).expect("read golden coverage entry point");
    let syntax = syn::parse_file(&source).expect("parse golden coverage entry point");
    let mut visitor = CandidateCaptureRouteVisitor::default();
    visitor.visit_file(&syntax);
    assert_eq!(visitor.candidate_checks, 1);
    assert_eq!(visitor.candidate_captures, 1);
    assert!(visitor.capture_consumes_checked);
    assert_eq!(visitor.raw_capture_definitions, 0);
}

#[derive(Default)]
struct CandidateCaptureRouteVisitor {
    candidate_checks: usize,
    candidate_captures: usize,
    capture_consumes_checked: bool,
    raw_capture_definitions: usize,
}

impl<'ast> syn::visit::Visit<'ast> for CandidateCaptureRouteVisitor {
    fn visit_item_fn(&mut self, function: &'ast syn::ItemFn) {
        if function.sig.ident == "capture_model" {
            self.raw_capture_definitions += 1;
        }
        syn::visit::visit_item_fn(self, function);
    }

    fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
        let Some(name) = called_function_name(&call.func) else {
            syn::visit::visit_expr_call(self, call);
            return;
        };
        match name.as_str() {
            "check_candidate_capture" => self.candidate_checks += 1,
            "capture_candidate_model" => {
                self.candidate_captures += 1;
                self.capture_consumes_checked |= call
                    .args
                    .iter()
                    .nth(1)
                    .is_some_and(is_checked_candidate_reference);
            }
            _ => {}
        }
        syn::visit::visit_expr_call(self, call);
    }
}

fn is_checked_candidate_reference(expression: &syn::Expr) -> bool {
    let syn::Expr::Reference(reference) = expression else {
        return false;
    };
    matches!(reference.expr.as_ref(), syn::Expr::Path(path) if path.path.is_ident("checked"))
}

#[test]
fn candidate_only_aggregate_has_no_registry_or_capture_numerator_path() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("src/golden_coverage.rs");
    let source = fs::read_to_string(path).expect("read golden coverage entry point");
    let syntax = syn::parse_file(&source).expect("parse golden coverage entry point");
    let function = syntax
        .items
        .iter()
        .find_map(|item| match item {
            syn::Item::Fn(function) if function.sig.ident == "run_all" => Some(function),
            _ => None,
        })
        .expect("run_all entry point");
    let mut visitor = CandidateOnlyAggregateVisitor::default();
    visitor.visit_block(&function.block);
    assert_eq!(visitor.registry_checks, 1);
    assert!(visitor.registry_is_consumed);
    assert_eq!(visitor.history_constructors, 1);
    assert!(visitor.history_consumes_typed_denominators);
    assert_eq!(visitor.report_constructors, 1);
    assert!(visitor.report_constructor_consumes_admission);
    assert!(visitor.report_constructor_consumes_history);
    assert!(visitor.forbidden_calls.is_empty());
    assert!(visitor.forbidden_fields.is_empty());
    assert_eq!(visitor.report_literals, 0);
    assert_eq!(visitor.report_reassignments, 0);
    assert!(!visitor.mutable_report_binding);
}

#[derive(Default)]
struct CandidateOnlyAggregateVisitor {
    registry_checks: usize,
    registry_is_consumed: bool,
    history_constructors: usize,
    history_consumes_typed_denominators: bool,
    report_constructors: usize,
    report_constructor_consumes_admission: bool,
    report_constructor_consumes_history: bool,
    forbidden_calls: Vec<String>,
    forbidden_fields: Vec<String>,
    report_literals: usize,
    report_reassignments: usize,
    mutable_report_binding: bool,
}

impl<'ast> syn::visit::Visit<'ast> for CandidateOnlyAggregateVisitor {
    fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
        if let Some(name) = called_function_name(&call.func) {
            if name == "check_candidate_only_registry" {
                self.registry_checks += 1;
                self.registry_is_consumed |= registry_check_consumes_registry(call);
            }
            if name == "candidate_only_aggregate_report" {
                self.report_constructors += 1;
                let consumes_authorities =
                    call_arguments_are(call, &["admission", "denominator_history"]);
                self.report_constructor_consumes_admission |= consumes_authorities;
                self.report_constructor_consumes_history |= consumes_authorities;
            }
            if name == "checked_aggregate_denominator_history" {
                self.history_constructors += 1;
                self.history_consumes_typed_denominators |=
                    call_arguments_are(call, &["denominator_size", "previous_denominator"]);
            }
            if matches!(
                name.as_str(),
                "capture_candidate_model"
                    | "capture_model"
                    | "attributed_covered_coordinates"
                    | "write_footprint"
            ) {
                self.forbidden_calls.push(name);
            }
        }
        syn::visit::visit_expr_call(self, call);
    }

    fn visit_expr_field(&mut self, field: &'ast syn::ExprField) {
        let syn::Member::Named(name) = &field.member else {
            syn::visit::visit_expr_field(self, field);
            return;
        };
        if matches!(
            name.to_string().as_str(),
            "models" | "claim" | "capture" | "coverage_footprint" | "scenarios"
        ) {
            self.forbidden_fields.push(name.to_string());
        }
        if is_path_named(&field.base, "report") {
            self.forbidden_fields.push(name.to_string());
        }
        syn::visit::visit_expr_field(self, field);
    }

    fn visit_expr_struct(&mut self, expression: &'ast syn::ExprStruct) {
        if expression.path.is_ident("CandidateOnlyAggregateReport") {
            self.report_literals += 1;
        }
        syn::visit::visit_expr_struct(self, expression);
    }

    fn visit_expr_assign(&mut self, expression: &'ast syn::ExprAssign) {
        if is_path_named(&expression.left, "report") || is_report_field(&expression.left) {
            self.report_reassignments += 1;
        }
        syn::visit::visit_expr_assign(self, expression);
    }

    fn visit_local(&mut self, local: &'ast syn::Local) {
        if matches!(&local.pat, syn::Pat::Ident(binding) if binding.ident == "report" && binding.mutability.is_some())
        {
            self.mutable_report_binding = true;
        }
        syn::visit::visit_local(self, local);
    }
}

fn registry_check_consumes_registry(call: &syn::ExprCall) -> bool {
    let Some(syn::Expr::Path(path)) = call.args.iter().nth(1) else {
        return false;
    };
    path.path.is_ident("registry")
}

fn is_path_named(expression: &syn::Expr, expected: &str) -> bool {
    matches!(expression, syn::Expr::Path(path) if path.path.is_ident(expected))
}

fn call_arguments_are(call: &syn::ExprCall, expected: &[&str]) -> bool {
    call.args.len() == expected.len()
        && call
            .args
            .iter()
            .zip(expected)
            .all(|(argument, expected)| is_path_named(argument, expected))
}

fn is_report_field(expression: &syn::Expr) -> bool {
    matches!(expression, syn::Expr::Field(field) if is_path_named(&field.base, "report"))
}

fn called_function_name(expression: &syn::Expr) -> Option<String> {
    let syn::Expr::Path(path) = expression else {
        return None;
    };
    path.path
        .segments
        .last()
        .map(|segment| segment.ident.to_string())
}

#[derive(Default)]
struct LegacyIdentifierVisitor {
    identifiers: Vec<String>,
}

impl<'ast> syn::visit::Visit<'ast> for LegacyIdentifierVisitor {
    fn visit_ident(&mut self, identifier: &'ast syn::Ident) {
        let name = identifier.to_string();
        if name.starts_with("CurrentCandidate") || name == "parse_current_candidate_registry" {
            self.identifiers.push(name);
        }
    }
}

#[derive(Default)]
struct CheckedCandidateVisitor {
    returners: Vec<String>,
    constructions: usize,
    definitions: usize,
    forbidden_derive: bool,
    public_fields: usize,
    forbidden_impls: usize,
    golden_identifiers: Vec<String>,
    registry_definitions: usize,
    profile_definitions: usize,
    endpoint_definitions: usize,
    registry_admission_definitions: usize,
    registry_admission_constructions: usize,
    registry_admission_returners: Vec<String>,
    registry_admission_public_fields: usize,
    registry_admission_forbidden_derive: bool,
    registry_admission_forbidden_impls: usize,
    aggregate_report_definitions: usize,
    aggregate_report_constructions: usize,
    aggregate_report_returners: Vec<String>,
    aggregate_report_public_fields: usize,
    aggregate_report_forbidden_derive: bool,
    aggregate_report_forbidden_impls: usize,
    aggregate_report_mutable_methods: usize,
    denominator_history_definitions: usize,
    denominator_history_constructions: usize,
    denominator_history_returners: Vec<String>,
    denominator_history_public_fields: usize,
    denominator_history_forbidden_derive: bool,
    denominator_history_forbidden_impls: usize,
}

impl CheckedCandidateVisitor {
    fn inspect_signature(&mut self, signature: &syn::Signature) {
        let syn::ReturnType::Type(_, return_type) = &signature.output else {
            return;
        };
        let mut visitor = CheckedTypeVisitor::default();
        visitor.visit_type(return_type);
        if visitor.found {
            self.returners.push(signature.ident.to_string());
        }
        if return_type_contains(return_type, "CandidateOnlyRegistryAdmission") {
            self.registry_admission_returners
                .push(signature.ident.to_string());
        }
        if return_type_contains(return_type, "CandidateOnlyAggregateReport") {
            self.aggregate_report_returners
                .push(signature.ident.to_string());
        }
        if return_type_contains(return_type, "AggregateDenominatorHistory") {
            self.denominator_history_returners
                .push(signature.ident.to_string());
        }
    }
}

impl<'ast> syn::visit::Visit<'ast> for CheckedCandidateVisitor {
    fn visit_ident(&mut self, identifier: &'ast syn::Ident) {
        if identifier == "CheckedGoldenModel" {
            self.golden_identifiers.push(identifier.to_string());
        }
        syn::visit::visit_ident(self, identifier);
    }

    fn visit_item_struct(&mut self, item: &'ast syn::ItemStruct) {
        if item.ident == "GoldenRegistry" {
            self.registry_definitions += 1;
        }
        if item.ident == "CheckedCandidateCapture" {
            self.definitions += 1;
            self.public_fields += item
                .fields
                .iter()
                .filter(|field| !matches!(field.vis, syn::Visibility::Inherited))
                .count();
            self.forbidden_derive |= has_forbidden_authority_derive(&item.attrs);
        }
        if item.ident == "CandidateOnlyRegistryAdmission" {
            self.registry_admission_definitions += 1;
            self.registry_admission_public_fields += public_field_count(&item.fields);
            self.registry_admission_forbidden_derive |= has_forbidden_authority_derive(&item.attrs);
        }
        if item.ident == "CandidateOnlyAggregateReport" {
            self.aggregate_report_definitions += 1;
            self.aggregate_report_public_fields += public_field_count(&item.fields);
            self.aggregate_report_forbidden_derive |= has_forbidden_report_derive(&item.attrs);
        }
        if item.ident == "AggregateDenominatorHistory" {
            self.denominator_history_definitions += 1;
            self.denominator_history_public_fields += public_field_count(&item.fields);
            self.denominator_history_forbidden_derive |=
                has_forbidden_authority_derive(&item.attrs);
        }
        syn::visit::visit_item_struct(self, item);
    }

    fn visit_item_enum(&mut self, item: &'ast syn::ItemEnum) {
        if item.ident == "GoldenProfile" {
            self.profile_definitions += 1;
        }
        if item.ident == "Endpoint" {
            self.endpoint_definitions += 1;
        }
        syn::visit::visit_item_enum(self, item);
    }

    fn visit_item_impl(&mut self, item: &'ast syn::ItemImpl) {
        let checked_candidate = impl_type_is(item, "CheckedCandidateCapture");
        let registry_admission = impl_type_is(item, "CandidateOnlyRegistryAdmission");
        let aggregate_report = impl_type_is(item, "CandidateOnlyAggregateReport");
        let denominator_history = impl_type_is(item, "AggregateDenominatorHistory");
        if checked_candidate
            && impl_has_trait(
                item,
                &["Clone", "Copy", "Default", "Serialize", "Deserialize"],
            )
        {
            self.forbidden_impls += 1;
        }
        if registry_admission
            && impl_has_trait(
                item,
                &["Clone", "Copy", "Default", "Serialize", "Deserialize"],
            )
        {
            self.registry_admission_forbidden_impls += 1;
        }
        if aggregate_report && impl_has_trait(item, &["Clone", "Copy", "Default", "Deserialize"]) {
            self.aggregate_report_forbidden_impls += 1;
        }
        if aggregate_report {
            self.aggregate_report_mutable_methods += item
                .items
                .iter()
                .filter(|member| {
                    matches!(member, syn::ImplItem::Fn(function) if signature_has_mutable_reference(&function.sig))
                })
                .count();
        }
        if denominator_history
            && impl_has_trait(
                item,
                &["Clone", "Copy", "Default", "Serialize", "Deserialize"],
            )
        {
            self.denominator_history_forbidden_impls += 1;
        }
        syn::visit::visit_item_impl(self, item);
    }

    fn visit_item_fn(&mut self, function: &'ast syn::ItemFn) {
        self.inspect_signature(&function.sig);
        syn::visit::visit_item_fn(self, function);
    }

    fn visit_impl_item_fn(&mut self, function: &'ast syn::ImplItemFn) {
        self.inspect_signature(&function.sig);
        syn::visit::visit_impl_item_fn(self, function);
    }

    fn visit_expr_struct(&mut self, expression: &'ast syn::ExprStruct) {
        if expression
            .path
            .segments
            .last()
            .is_some_and(|segment| segment.ident == "CheckedCandidateCapture")
        {
            self.constructions += 1;
        }
        if expression
            .path
            .segments
            .last()
            .is_some_and(|segment| segment.ident == "CandidateOnlyRegistryAdmission")
        {
            self.registry_admission_constructions += 1;
        }
        if expression
            .path
            .segments
            .last()
            .is_some_and(|segment| segment.ident == "CandidateOnlyAggregateReport")
        {
            self.aggregate_report_constructions += 1;
        }
        if expression
            .path
            .segments
            .last()
            .is_some_and(|segment| segment.ident == "AggregateDenominatorHistory")
        {
            self.denominator_history_constructions += 1;
        }
        syn::visit::visit_expr_struct(self, expression);
    }
}

fn public_field_count(fields: &syn::Fields) -> usize {
    fields
        .iter()
        .filter(|field| !matches!(field.vis, syn::Visibility::Inherited))
        .count()
}

fn impl_type_is(item: &syn::ItemImpl, expected: &str) -> bool {
    matches!(item.self_ty.as_ref(), syn::Type::Path(path) if path.path.segments.last().is_some_and(|segment| segment.ident == expected))
}

fn impl_has_trait(item: &syn::ItemImpl, forbidden: &[&str]) -> bool {
    item.trait_.as_ref().is_some_and(|(_, path, _)| {
        path.segments
            .last()
            .is_some_and(|segment| forbidden.contains(&segment.ident.to_string().as_str()))
    })
}

fn signature_has_mutable_reference(signature: &syn::Signature) -> bool {
    let mut visitor = MutableReferenceVisitor::default();
    for argument in &signature.inputs {
        visitor.visit_fn_arg(argument);
    }
    if let syn::ReturnType::Type(_, return_type) = &signature.output {
        visitor.visit_type(return_type);
    }
    visitor.found
}

#[derive(Default)]
struct MutableReferenceVisitor {
    found: bool,
}

impl<'ast> syn::visit::Visit<'ast> for MutableReferenceVisitor {
    fn visit_type_reference(&mut self, reference: &'ast syn::TypeReference) {
        self.found |= reference.mutability.is_some();
        syn::visit::visit_type_reference(self, reference);
    }
}

fn has_forbidden_authority_derive(attributes: &[syn::Attribute]) -> bool {
    attributes.iter().any(attribute_has_forbidden_derive)
}

fn has_forbidden_report_derive(attributes: &[syn::Attribute]) -> bool {
    attributes.iter().any(|attribute| {
        derive_contains_any(attribute, &["Clone", "Copy", "Default", "Deserialize"])
    })
}

fn attribute_has_forbidden_derive(attribute: &syn::Attribute) -> bool {
    derive_contains_any(
        attribute,
        &["Clone", "Copy", "Default", "Serialize", "Deserialize"],
    )
}

fn derive_contains_any(attribute: &syn::Attribute, forbidden: &[&str]) -> bool {
    if !attribute.path().is_ident("derive") {
        return false;
    }
    let syn::Meta::List(list) = &attribute.meta else {
        return false;
    };
    let tokens = list.tokens.to_string();
    forbidden
        .iter()
        .any(|name| tokens.split_whitespace().any(|token| token == *name))
}

fn return_type_contains(return_type: &syn::Type, expected: &str) -> bool {
    let mut visitor = NamedTypeVisitor {
        expected,
        found: false,
    };
    visitor.visit_type(return_type);
    visitor.found
}

struct NamedTypeVisitor<'name> {
    expected: &'name str,
    found: bool,
}

impl<'ast> syn::visit::Visit<'ast> for NamedTypeVisitor<'_> {
    fn visit_type_path(&mut self, path: &'ast syn::TypePath) {
        self.found |= path
            .path
            .segments
            .iter()
            .any(|segment| segment.ident == self.expected);
        syn::visit::visit_type_path(self, path);
    }
}

#[derive(Default)]
struct CheckedTypeVisitor {
    found: bool,
}

impl<'ast> syn::visit::Visit<'ast> for CheckedTypeVisitor {
    fn visit_type_path(&mut self, path: &'ast syn::TypePath) {
        self.found |= path
            .path
            .segments
            .iter()
            .any(|segment| segment.ident == "CheckedCandidateCapture");
        syn::visit::visit_type_path(self, path);
    }
}

#[test]
fn capture_lock_refuses_overlap_and_releases_on_drop() {
    let directory = tempfile::tempdir().expect("temporary lock directory");
    let path = directory.path().join("capture.lock");
    let first = CaptureLock::acquire(&path).expect("first capture lock");
    assert!(CaptureLock::acquire(&path).is_err());
    drop(first);
    CaptureLock::acquire(&path).expect("released capture lock");
}

#[test]
fn rerun_invalidates_prior_model_evidence() {
    let directory = tempfile::tempdir().expect("temporary evidence directory");
    for name in ["raw.lcov", "footprint.json", "review.md"] {
        fs::write(directory.path().join(name), "stale").expect("write stale evidence");
    }
    invalidate_model_evidence(directory.path()).expect("invalidate old evidence");
    for name in ["raw.lcov", "footprint.json", "review.md"] {
        assert!(!directory.path().join(name).exists());
    }
}

#[test]
fn denominator_base_enables_defaults_without_disabling_or_forcing_all_features() {
    assert_eq!(
        DENOMINATOR_CARGO_ARGS,
        [
            "test",
            "--workspace",
            "--all-targets",
            "--no-run",
            "--jobs",
            "4",
            "--locked",
        ]
    );
    // The base must not force `--all-features`: production sites that gate on
    // feature absence would be compiled out of the denominator while scenarios
    // still execute them, inverting the union-subset relationship.
    assert!(!DENOMINATOR_CARGO_ARGS.contains(&"--all-features"));
    assert!(!DENOMINATOR_CARGO_ARGS.contains(&"--no-default-features"));
}

#[test]
fn empty_scenario_feature_union_leaves_denominator_at_default_features() {
    let args = denominator_cargo_args(&std::collections::BTreeSet::new());
    let expected: Vec<String> = DENOMINATOR_CARGO_ARGS
        .iter()
        .map(|argument| (*argument).to_string())
        .collect();
    assert_eq!(args, expected);
    assert!(!args.iter().any(|argument| argument == "--features"));
}

#[test]
fn scenario_feature_union_is_appended_as_a_single_sorted_features_argument() {
    let union = ["template-runtime-tests", "diagnostics"]
        .into_iter()
        .map(str::to_string)
        .collect();
    let args = denominator_cargo_args(&union);
    let features_index = args
        .iter()
        .position(|argument| argument == "--features")
        .expect("feature union appends a --features argument");
    // A BTreeSet orders the union, so the joined value is deterministic.
    assert_eq!(
        args[features_index + 1],
        "diagnostics,template-runtime-tests"
    );
    assert_eq!(args.len(), DENOMINATOR_CARGO_ARGS.len() + 2);
}

#[test]
fn scenario_args_refuse_features_that_would_disable_defaults() {
    let disabling = CoverageTest {
        package: "rumoca".to_string(),
        test_target: "suite_core".to_string(),
        features: vec!["--no-default-features".to_string()],
        test_name: "example".to_string(),
    };
    assert!(cargo_test_target_args(&disabling).is_err());

    let well_formed = CoverageTest {
        package: "rumoca".to_string(),
        test_target: "suite_template_runtime".to_string(),
        features: vec!["template-runtime-tests".to_string()],
        test_name: "example".to_string(),
    };
    let args = cargo_test_target_args(&well_formed).expect("well-formed features build args");
    let features_index = args
        .iter()
        .position(|argument| argument == "--features")
        .expect("listed features append a --features argument");
    assert_eq!(args[features_index + 1], "template-runtime-tests");
    assert!(
        !args
            .iter()
            .any(|argument| argument == "--no-default-features")
    );
}

#[test]
fn machine_footprint_serializes_sorted_lines_as_compact_ranges() {
    let file = FileFootprint {
        path: "crates/product/src/lib.rs".to_string(),
        source_sha256: "00".repeat(32),
        instrumentable_lines: vec![1, 2, 3, 7],
        covered_lines: vec![2, 3],
        macro_attributed_covered_lines: Vec::new(),
        unattributable_covered_lines: Vec::new(),
        macro_attributed_function_identities: Vec::new(),
        executed_functions: Vec::new(),
    };
    let value = serde_json::to_value(file).expect("serialize compact footprint");
    assert_eq!(
        value["instrumentable_lines"],
        serde_json::json!(["1-3", "7"])
    );
    assert_eq!(value["covered_lines"], serde_json::json!(["2-3"]));
}

#[test]
fn rust_item_catalog_has_typed_crate_root_and_rejects_duplicate_shapes() {
    let directory = rust_source_repository("pub fn root() {}\n");
    let root = directory.path();
    let resolved = resolve_rust_item_capture(root, "crates/product/src/lib.rs", 1)
        .expect("resolve crate-root function");
    assert_eq!(
        resolved,
        RustItemId {
            package: "product".to_string(),
            module: RustModuleId::CrateRoot,
            scope: RustItemScope::Module,
            kind: RustItemKind::Function,
            item_name: "root".to_string(),
        }
    );
    validate_rust_item_capture(root, "crates/product/src/lib.rs", &resolved, 1)
        .expect("validate independently resolved item");

    fs::write(
        root.join("crates/product/src/lib.rs"),
        "pub fn duplicate() {}\npub fn duplicate() {}\n",
    )
    .expect("duplicate source shape");
    assert!(resolve_rust_item_capture(root, "crates/product/src/lib.rs", 1).is_err());

    fs::write(
        root.join("crates/product/src/lib.rs"),
        "struct Product;\nimpl Product { fn run(&self) {} }\nimpl Product { fn run(&self) {} }\n",
    )
    .expect("duplicate impl shape");
    assert!(resolve_rust_item_capture(root, "crates/product/src/lib.rs", 2).is_err());
}

#[test]
fn rust_item_catalog_distinguishes_fully_qualified_impl_types() {
    let source = "mod a { pub struct Product; }\nmod b { pub struct Product; }\nimpl a::Product { pub fn run(&self) {} }\nimpl b::Product { pub fn run(&self) {} }\n";
    let directory = rust_source_repository(source);
    let root = directory.path();
    let first = resolve_rust_item_capture(root, "crates/product/src/lib.rs", 3)
        .expect("first fully-qualified impl");
    let second = resolve_rust_item_capture(root, "crates/product/src/lib.rs", 4)
        .expect("second fully-qualified impl");
    assert_ne!(first, second);
}

#[test]
fn rust_item_catalog_refuses_generic_and_unrepresentable_trait_items() {
    let directory = rust_source_repository(
        "pub fn generic<T>() {}\nstruct Product;\ntrait Trait<T> { fn run(&self); }\nimpl Trait<u8> for Product { fn run(&self) {} }\n",
    );
    let root = directory.path();
    assert!(resolve_rust_item_capture(root, "crates/product/src/lib.rs", 1).is_err());
    assert!(resolve_rust_item_capture(root, "crates/product/src/lib.rs", 4).is_err());
}

#[test]
fn generic_function_coverage_is_not_attributed_to_a_runtime_owner() {
    let directory = rust_source_repository("pub fn generic<T>() {}\n");
    let root = directory.path();
    let source = root.join("crates/product/src/lib.rs");
    let lcov = root.join("generic.lcov");
    fs::write(
        &lcov,
        format!(
            "SF:{}\nFN:1,product::generic::<u8>\nFNDA:1,product::generic::<u8>\nDA:1,1\nend_of_record\n",
            source.display()
        ),
    )
    .expect("generic LCOV fixture");
    let coverage = normalize_lcov(root, &lcov, true).expect("normalize generic coverage");
    assert_eq!(coverage.files[0].unattributable_covered_lines, vec![1]);
    assert!(coverage.files[0].executed_functions.is_empty());
}

fn rust_source_repository(source: &str) -> tempfile::TempDir {
    let directory = tempfile::tempdir().expect("temporary Rust source repository");
    let crate_root = directory.path().join("crates/product");
    fs::create_dir_all(crate_root.join("src")).expect("crate source directory");
    fs::write(
        crate_root.join("Cargo.toml"),
        "[package]\nname = \"product\"\nversion = \"0.0.0\"\n",
    )
    .expect("crate manifest");
    fs::write(crate_root.join("src/lib.rs"), source).expect("crate source");
    directory
}
