//! SPEC_0008 syntax-aware gates for successful semantic-sentinel fabrication.

use super::architecture_hardening_support::{attributes_require_test, production_rust_sources};
use super::*;
use std::collections::BTreeMap;
use syn::visit::{self, Visit};

mod ast_recovery_success;
mod codegen_fallback;
mod semantic_owner_bindings;
mod semantic_zero;

use ast_recovery_success::semantic_zero_fabrications;
use codegen_fallback::codegen_fallback_findings;
use semantic_owner_bindings::SemanticSourceIdentity;

#[test]
fn test_codegen_fallback_inventory_does_not_grow() {
    let root = workspace_root();
    let sources = production_rust_sources(&root.join("crates/rumoca-phase-codegen"), &root);

    let mut value_undefined = Vec::new();
    let mut optional_empty = Vec::new();
    let mut unwrap_or_default = Vec::new();
    for (path, content) in sources {
        let findings = codegen_fallback_findings(&path, &content);
        value_undefined.extend(findings.value_undefined);
        optional_empty.extend(findings.optional_empty);
        unwrap_or_default.extend(findings.unwrap_or_default);
    }
    assert!(
        value_undefined.is_empty(),
        "production codegen must not construct Minijinja Value::UNDEFINED sentinels; \
use explicit optional objects or structured errors instead: {value_undefined:#?}"
    );
    assert!(
        optional_empty.is_empty(),
        "production codegen optional render misses must go through an explicit helper \
instead of raw Ok(None) fallback returns: {optional_empty:#?}"
    );
    assert!(
        unwrap_or_default.is_empty(),
        "codegen must not add unwrap_or_default fallback paths: {unwrap_or_default:#?}"
    );
}

#[test]
fn codegen_fallback_gate_scans_production_test_named_modules() {
    let temporary = tempfile::tempdir().expect("temporary Rust crate");
    let src = temporary.path().join("src");
    fs::create_dir_all(&src).expect("create fixture src");
    fs::write(
        temporary.path().join("Cargo.toml"),
        "[package]\nname = \"sentinel-fixture\"\nversion = \"0.0.0\"\nedition = \"2024\"\n",
    )
    .expect("write fixture manifest");
    fs::write(
        src.join("lib.rs"),
        r#"
            mod dialect_tests;
            #[cfg(test)]
            mod actual_tests { const OPEN_BRACE: &str = "{"; }
            fn production_after_test_module() { let _ = Value::UNDEFINED; }
        "#,
    )
    .expect("write fixture lib.rs");
    fs::write(
        src.join("dialect_tests.rs"),
        "fn fallback() { let _ = Value::UNDEFINED; }",
    )
    .expect("write production test-named module");
    fs::write(
        src.join("actual_tests.rs"),
        "fn fixture() { let _ = Value::UNDEFINED; }",
    )
    .expect("write cfg(test) module");

    let sources = production_rust_sources(temporary.path(), temporary.path());
    assert!(sources.iter().any(|(path, content)| {
        path.ends_with("dialect_tests.rs") && content.contains("Value::UNDEFINED")
    }));
    let findings = sources
        .iter()
        .map(|(path, content)| codegen_fallback_findings(path, content))
        .collect::<Vec<_>>();
    assert_eq!(
        findings
            .iter()
            .map(|finding| finding.value_undefined.len())
            .sum::<usize>(),
        2,
        "the syntax visitor must see the production test-named module and the item after an inline cfg(test) module, while skipping the test module body"
    );
}

#[test]
fn codegen_fallback_gate_rejects_aliases_defaults_and_qualified_none() {
    let path = Path::new("crates/rumoca-phase-codegen/src/codegen/fallback.rs");
    let findings = codegen_fallback_findings(
        path,
        r#"
            use minijinja::Value as TemplateValue;
            fn direct() -> TemplateValue { TemplateValue::UNDEFINED }
            fn defaulted() -> TemplateValue { TemplateValue::default() }
            type AliasedValue = minijinja::Value;
            fn type_defaulted() -> AliasedValue { AliasedValue::default() }
            fn ufcs_defaulted() -> minijinja::Value { <minijinja::Value as Default>::default() }
            fn callback_defaulted(missing: Option<TemplateValue>) -> TemplateValue {
                missing.unwrap_or_else(TemplateValue::default)
            }
            fn function_item_defaulted() -> TemplateValue {
                let make = TemplateValue::default;
                make()
            }
            fn missing() -> Result<Option<usize>, Error> { Ok(Option::None) }
        "#,
    );
    assert_eq!(findings.value_undefined.len(), 6);
    assert_eq!(findings.optional_empty.len(), 1);
}

fn path_expression_ends_with(expression: &syn::Expr, expected: &str) -> bool {
    let syn::Expr::Path(path) = expression else {
        return false;
    };
    path.path
        .segments
        .last()
        .is_some_and(|part| part.ident == expected)
}

#[derive(Default)]
struct RecoveryDefaultVisitor {
    recovery_enums: BTreeSet<String>,
    derived_defaults: BTreeSet<String>,
    manual_defaults: BTreeSet<String>,
    module_path: Vec<String>,
    imports: BTreeMap<(String, String), Vec<String>>,
}

impl<'ast> Visit<'ast> for RecoveryDefaultVisitor {
    fn visit_file(&mut self, file: &'ast syn::File) {
        self.record_imports(&file.items);
        visit::visit_file(self, file);
    }

    fn visit_item_mod(&mut self, module: &'ast syn::ItemMod) {
        if attributes_require_test(&module.attrs) {
            return;
        }
        if let Some((_, items)) = &module.content {
            self.module_path.push(module.ident.to_string());
            self.record_imports(items);
            visit::visit_item_mod(self, module);
            self.module_path.pop();
        }
    }

    fn visit_item_enum(&mut self, item: &'ast syn::ItemEnum) {
        if attributes_require_test(&item.attrs) {
            return;
        }
        let has_recovery_variant = item.variants.iter().any(|variant| {
            matches!(
                variant.ident.to_string().as_str(),
                "Empty" | "Missing" | "Absent" | "Recovery"
            )
        });
        if has_recovery_variant {
            let name = qualified_item_name(&self.module_path, &item.ident.to_string());
            self.recovery_enums.insert(name.clone());
            if enum_has_default_route(item) {
                self.derived_defaults.insert(name);
            }
        }
        visit::visit_item_enum(self, item);
    }

    fn visit_item_impl(&mut self, implementation: &'ast syn::ItemImpl) {
        if attributes_require_test(&implementation.attrs) {
            return;
        }
        let implements_default = implementation
            .trait_
            .as_ref()
            .and_then(|(_, path, _)| path.segments.last())
            .is_some_and(|segment| segment.ident == "Default");
        if implements_default
            && let syn::Type::Path(path) = implementation.self_ty.as_ref()
            && let Some(identity) =
                normalized_impl_type_path(&path.path, &self.module_path, &self.imports)
        {
            self.manual_defaults.insert(identity);
        }
        visit::visit_item_impl(self, implementation);
    }
}

impl RecoveryDefaultVisitor {
    fn record_imports(&mut self, items: &[syn::Item]) {
        for item in items {
            let syn::Item::Use(item_use) = item else {
                continue;
            };
            if attributes_require_test(&item_use.attrs) {
                continue;
            }
            let mut imports = Vec::new();
            flatten_use_tree(&item_use.tree, Vec::new(), &mut imports);
            for (alias, path) in imports {
                self.record_import(alias, &path);
            }
        }
    }

    fn record_import(&mut self, alias: String, path: &[String]) {
        if let Some(target) = normalized_source_path(path, &self.module_path) {
            self.imports
                .insert((self.module_path.join("::"), alias), target);
        }
    }
}

fn qualified_item_name(module_path: &[String], item: &str) -> String {
    module_path
        .iter()
        .map(String::as_str)
        .chain(std::iter::once(item))
        .collect::<Vec<_>>()
        .join("::")
}

fn normalized_impl_type_path(
    path: &syn::Path,
    module_path: &[String],
    imports: &BTreeMap<(String, String), Vec<String>>,
) -> Option<String> {
    let segments = path
        .segments
        .iter()
        .map(|segment| segment.ident.to_string())
        .collect::<Vec<_>>();
    let first = segments.first()?;
    if let Some(imported) = imports.get(&(module_path.join("::"), first.clone())) {
        return Some(
            imported
                .iter()
                .cloned()
                .chain(segments.into_iter().skip(1))
                .collect::<Vec<_>>()
                .join("::"),
        );
    }

    normalized_source_path(&segments, module_path).map(|segments| segments.join("::"))
}

fn normalized_source_path(segments: &[String], module_path: &[String]) -> Option<Vec<String>> {
    if segments.is_empty() {
        return None;
    }
    let mut segments = segments.to_vec();
    let mut base = module_path.to_vec();
    match segments.first().map(String::as_str) {
        Some("crate") => {
            base.truncate(usize::from(!base.is_empty()));
            segments.remove(0);
        }
        Some("self") => {
            segments.remove(0);
        }
        Some("super") => {
            while segments.first().is_some_and(|segment| segment == "super") {
                if base.len() > 1 {
                    base.pop();
                }
                segments.remove(0);
            }
        }
        Some(first)
            if base.first().is_some_and(|crate_name| {
                first.replace('_', "-").as_str() == crate_name.as_str()
            }) =>
        {
            base.truncate(usize::from(!base.is_empty()));
            segments.remove(0);
        }
        _ => {}
    }
    (!segments.is_empty()).then(|| base.into_iter().chain(segments).collect())
}

fn flatten_use_tree(
    tree: &syn::UseTree,
    mut prefix: Vec<String>,
    imports: &mut Vec<(String, Vec<String>)>,
) {
    match tree {
        syn::UseTree::Path(path) => {
            prefix.push(path.ident.to_string());
            flatten_use_tree(&path.tree, prefix, imports);
        }
        syn::UseTree::Name(name) => {
            prefix.push(name.ident.to_string());
            imports.push((name.ident.to_string(), prefix));
        }
        syn::UseTree::Rename(rename) => {
            prefix.push(rename.ident.to_string());
            imports.push((rename.rename.to_string(), prefix));
        }
        syn::UseTree::Group(group) => {
            for item in &group.items {
                flatten_use_tree(item, prefix.clone(), imports);
            }
        }
        syn::UseTree::Glob(_) => {}
    }
}

fn enum_has_default_route(item: &syn::ItemEnum) -> bool {
    item.attrs.iter().any(|attribute| {
        if !attribute.path().is_ident("derive") {
            return false;
        }
        attribute
            .parse_args_with(
                syn::punctuated::Punctuated::<syn::Path, syn::Token![,]>::parse_terminated,
            )
            .is_ok_and(|traits| {
                traits.iter().any(|path| {
                    path.segments
                        .last()
                        .is_some_and(|segment| segment.ident == "Default")
                })
            })
    }) || item.variants.iter().any(|variant| {
        variant
            .attrs
            .iter()
            .any(|attribute| attribute.path().is_ident("default"))
    })
}

fn recovery_default_routes(source: &str) -> Result<Vec<String>, syn::Error> {
    let syntax = syn::parse_file(source)?;
    let mut visitor = RecoveryDefaultVisitor::default();
    visitor.visit_file(&syntax);
    visitor.derived_defaults.extend(
        visitor
            .manual_defaults
            .intersection(&visitor.recovery_enums)
            .cloned(),
    );
    Ok(visitor.derived_defaults.into_iter().collect())
}

fn source_module_path(crate_name: &str, relative_to_src: &Path) -> Vec<String> {
    let mut module_path = vec![crate_name.to_string()];
    let components = relative_to_src
        .components()
        .filter_map(|component| component.as_os_str().to_str())
        .collect::<Vec<_>>();
    let Some((file, directories)) = components.split_last() else {
        return module_path;
    };
    let file = *file;
    module_path.extend(directories.iter().map(|component| (*component).to_string()));
    let stem = file.strip_suffix(".rs").unwrap_or(file);
    if !matches!(stem, "lib" | "main" | "mod") {
        module_path.push(stem.to_string());
    }
    module_path
}

fn collect_recovery_default_inventory(
    source: &str,
    module_path: Vec<String>,
) -> Result<RecoveryDefaultVisitor, syn::Error> {
    let syntax = syn::parse_file(source)?;
    let mut visitor = RecoveryDefaultVisitor {
        module_path,
        ..RecoveryDefaultVisitor::default()
    };
    visitor.visit_file(&syntax);
    Ok(visitor)
}

#[test]
fn ir_recovery_variants_cannot_be_constructed_through_default() {
    const EXPECTED_EXCEPTION_COUNT: usize = 0;
    const EXCEPTIONS: &[(&str, &str)] = &[];
    assert_eq!(
        EXCEPTIONS.len(),
        EXPECTED_EXCEPTION_COUNT,
        "recovery-default exception count changed without an explicit review"
    );

    let root = workspace_root();
    let crates_root = root.join("crates");
    let mut ir_crates = fs::read_dir(&crates_root)
        .expect("read workspace crates")
        .filter_map(Result::ok)
        .filter(|entry| entry.file_type().is_ok_and(|kind| kind.is_dir()))
        .filter_map(|entry| {
            let name = entry.file_name().to_str()?.to_string();
            (name == "rumoca-core" || name.starts_with("rumoca-ir-"))
                .then_some((name, entry.path().join("src")))
        })
        .collect::<Vec<_>>();
    ir_crates.sort_by(|left, right| left.0.cmp(&right.0));

    let mut recovery_enums = BTreeSet::new();
    let mut default_routes = BTreeSet::new();
    for (crate_name, source_root) in ir_crates {
        let mut files = Vec::new();
        collect_rs_files(&source_root, &mut files);
        files.sort();
        for path in files {
            let relative = normalized_rel_path(path.strip_prefix(&root).unwrap_or(&path));
            if is_test_or_example_path(Path::new(&relative)) {
                continue;
            }
            let source = fs::read_to_string(&path).expect("read IR source");
            let module_path = source_module_path(
                &crate_name,
                path.strip_prefix(&source_root)
                    .expect("IR source is below its crate source root"),
            );
            let visitor = collect_recovery_default_inventory(&source, module_path)
                .unwrap_or_else(|error| panic!("parse {relative} for recovery defaults: {error}"));
            recovery_enums.extend(visitor.recovery_enums);
            default_routes.extend(visitor.derived_defaults);
            default_routes.extend(visitor.manual_defaults);
        }
    }

    let mut offenders = Vec::new();
    let mut observed_exceptions = BTreeSet::new();
    let configured_exceptions = EXCEPTIONS
        .iter()
        .map(|(path, _)| (*path).to_string())
        .collect::<BTreeSet<_>>();
    assert_eq!(
        configured_exceptions.len(),
        EXPECTED_EXCEPTION_COUNT,
        "recovery-default exceptions must be unique"
    );

    for identity in default_routes.intersection(&recovery_enums) {
        if configured_exceptions.contains(identity) {
            observed_exceptions.insert(identity.clone());
        } else {
            offenders.push(format!(
                "{identity} has a generic Default construction route"
            ));
        }
    }

    assert_eq!(
        observed_exceptions, configured_exceptions,
        "recovery-default exceptions must be live and explicitly retired when fixed"
    );
    assert!(
        offenders.is_empty(),
        "AST absence/recovery sentinels require explicit construction: {offenders:#?}"
    );
}

#[test]
fn recovery_default_gate_detects_derive_and_manual_routes() {
    let source = r#"
#[derive(Default)]
enum DerivedRecovery { #[default] Empty, Value }
enum ManualRecovery { Empty, Value }
impl Default for ManualRecovery {
    fn default() -> Self { Self::Empty }
}
#[cfg(test)]
mod tests {
    #[derive(Default)]
    enum TestRecovery { #[default] Empty }
}
"#;

    assert_eq!(
        recovery_default_routes(source).expect("valid recovery-default fixture"),
        ["DerivedRecovery", "ManualRecovery"]
    );
}

#[test]
fn recovery_default_gate_does_not_cross_module_name_collisions() {
    let source = r#"
mod recovery_owner {
    enum State { Empty, Value }
}
mod unrelated_owner {
    struct State;
    impl Default for State {
        fn default() -> Self { Self }
    }
}
"#;

    assert!(
        recovery_default_routes(source)
            .expect("valid module-collision fixture")
            .is_empty()
    );
}

#[test]
fn recovery_default_gate_correlates_cross_file_impls_and_imports() {
    let enum_source = "enum ParseState { Empty, Value }";
    let impl_source = r#"
use crate::nodes::ParseState as ImportedState;
impl Default for ImportedState {
    fn default() -> Self { Self::Empty }
}
"#;
    let enum_visitor = collect_recovery_default_inventory(
        enum_source,
        vec!["fixture-ir".to_string(), "nodes".to_string()],
    )
    .expect("valid enum fixture");
    let impl_visitor = collect_recovery_default_inventory(
        impl_source,
        vec!["fixture-ir".to_string(), "defaults".to_string()],
    )
    .expect("valid impl fixture");

    assert_eq!(
        enum_visitor
            .recovery_enums
            .intersection(&impl_visitor.manual_defaults)
            .cloned()
            .collect::<Vec<_>>(),
        ["fixture-ir::nodes::ParseState".to_string()]
    );
}

#[test]
fn test_supported_semantic_sentinel_fabrications_are_absent() {
    let root = workspace_root();
    let mut files = Vec::new();
    collect_rs_files(&root.join("crates"), &mut files);
    files.sort();

    let mut offenders = Vec::new();
    for path in files {
        let relative = normalized_rel_path(path.strip_prefix(&root).unwrap_or(&path));
        if is_test_or_example_path(Path::new(&relative)) {
            continue;
        }
        let source = fs::read_to_string(&path).expect("read production Rust source");
        if !source.contains("Empty") && !source.contains(".is_none()") {
            continue;
        }
        let findings = semantic_zero_fabrications(
            &source,
            SemanticSourceIdentity::from_relative_path(&relative),
        )
        .unwrap_or_else(|error| panic!("parse {relative} for sentinel fabrication: {error}"));
        offenders.extend(
            findings
                .into_iter()
                .map(|finding| format!("{relative}: {finding}")),
        );
    }

    assert!(
        offenders.is_empty(),
        "production code used a supported direct semantic-sentinel fabrication or recovery \
placeholder form. SPEC_0008 requires missing semantic data to fail closed: {offenders:#?}"
    );
}

#[test]
fn sentinel_gate_detects_supported_direct_fabrications() {
    let positive_sources = [
        "use rumoca_ir_ast::Expression as AstExpression; fn f(x: E) -> R { match x { AstExpression::Empty { span: _ } => Ok(Value::Integer(0_i64)), _ => Err(()) } }",
        "fn f(x: E) -> R { match x { rumoca_ir_ast::TerminalType::Empty => Ok(Value::Real(0.0)), _ => Err(()) } }",
        "fn f(x: E) -> R { match x { rumoca_ir_ast::Expression::Empty { .. } => Ok(\"0\".into()), _ => Err(()) } }",
        "fn f(x: E) -> R { match x { rumoca_core::Expression::Empty { .. } => Ok(i64::default()), _ => Err(()) } }",
        "fn f(expression: Option<E>) -> R { if expression.is_none() { return Ok(0); } Err(()) }",
        "fn f(x: E) -> i64 { match x { rumoca_ir_ast::Statement::Empty => 0, _ => 1 } }",
        "fn f(x: E) -> R { if matches!(x, rumoca_ir_ast::Subscript::Empty) { return Some(0); } None }",
        "fn f(x: E) -> R { if let rumoca_ir_ast::Expression::Empty { .. } = x { return Ok(0); } Err(()) }",
        "fn f(&self) -> R { if self.body_expr.is_none() { return Ok(0); } Err(()) }",
        "fn f(slot: &mut E) { let _ = std::mem::replace(slot, rumoca_core::Expression::Empty { span: s() }); }",
        "fn f(x: E) -> R { match x { rumoca_core::OpUnary::Empty => Ok(x), _ => Err(()) } }",
        "fn f(x: E) -> R { match x { rumoca_core::OpBinary::Empty => Ok(x), _ => Err(()) } }",
        "fn f(x: E) -> R { match x { rumoca_ir_ast::Expression::Empty { .. } => { return Ok(0); }, _ => Err(()) } }",
        "fn f(x: E) -> R { match x { rumoca_core::OpUnary::Empty => { return Ok(x); }, _ => Err(()) } }",
        "fn f(x: E) -> R { match x { rumoca_core::OpBinary::Empty => { return Ok(x); }, _ => Err(()) } }",
    ];

    assert_eq!(
        positive_sources.len(),
        15,
        "supported detector forms are a reviewed capability ratchet"
    );

    for source in positive_sources {
        assert_eq!(
            semantic_zero_fabrications(source, SemanticSourceIdentity::External)
                .expect("valid positive fixture")
                .len(),
            1,
            "gate missed supported fabrication: {source}"
        );
    }
}

#[test]
fn sentinel_gate_ignores_comments_strings_tests_and_unrelated_absence() {
    let source = r#"
fn clean(value: Option<i64>) -> Result<i64, ()> {
    // Expression::Empty { .. } => Ok(Value::Integer(0))
    let _example = "TerminalType::Empty => Ok(Value::Integer(0))";
    if value.is_none() { return Ok(0); }
    match Some(1) { Some(value) => Ok(value), None => Err(()) }
}
fn empty_equation_is_preserved_by_an_ast_rewrite(equation: Equation) -> Equation {
    match equation {
        Equation::Empty => Equation::Empty,
        other => other,
    }
}
fn unrelated_empty_state_can_have_a_zero_value(state: State) -> Result<i64, ()> {
    match state {
        State::Empty => Ok(0),
        _ => Err(()),
    }
}
enum Expression { Empty }
fn local_expression_empty_can_have_a_zero_value(expression: Expression) -> Result<i64, ()> {
    match expression {
        Expression::Empty => Ok(0),
    }
}
fn unrelated_string_state_can_have_a_zero_value(state: &str) -> Result<i64, ()> {
    if state == "Empty" { return Ok(0); }
    Err(())
}
fn present_expression_can_have_a_zero_value(expression: Option<i64>) -> Result<i64, ()> {
    if !expression.is_none() { return Ok(0); }
    Err(())
}
fn a_diagnostic_offset_is_not_a_successful_zero(expression: Expression) -> Result<i64, Error> {
    match expression {
        Expression::Empty { .. } => Err(error_at(0)),
        _ => Ok(1),
    }
}
fn empty_operators_can_return_errors_without_becoming_successes(op: OpUnary) -> Result<i64, Error> {
    match op {
        OpUnary::Empty => { return Err(error()); }
        _ => Ok(1),
    }
}
fn empty_binary_operator_can_return_an_error(op: OpBinary) -> Result<i64, Error> {
    match op {
        OpBinary::Empty => { return Err(error()); }
        _ => Ok(1),
    }
}
#[cfg(test)]
impl Handler for Planted {
    fn run(expression: Option<i64>) -> Result<i64, ()> {
        if expression.is_none() { return Ok(0); }
        Err(())
    }
}
#[cfg(test)]
mod tests {
    fn planted(expression: Option<i64>) -> Result<i64, ()> {
        if expression.is_none() { return Ok(0); }
        Err(())
    }
}
"#;

    assert!(
        semantic_zero_fabrications(source, SemanticSourceIdentity::External)
            .expect("valid negative fixture")
            .is_empty()
    );

    let same_spelling_owner = "enum Expression { Empty } fn f(x: Expression) -> R { match x { Expression::Empty => Ok(0), } }";
    for source_identity in [
        SemanticSourceIdentity::AstOther,
        SemanticSourceIdentity::CoreOther,
    ] {
        assert!(
            semantic_zero_fabrications(same_spelling_owner, source_identity)
                .expect("valid out-of-line owner collision fixture")
                .is_empty(),
            "an unrelated file-local Expression must not acquire canonical semantic identity"
        );
    }

    assert_eq!(
        semantic_zero_fabrications(same_spelling_owner, SemanticSourceIdentity::AstNodes)
            .expect("valid canonical AST owner fixture")
            .len(),
        1,
    );
    assert_eq!(
        semantic_zero_fabrications(
            same_spelling_owner,
            SemanticSourceIdentity::CoreIrPrimitives,
        )
        .expect("valid canonical core owner fixture")
        .len(),
        1,
    );
}
