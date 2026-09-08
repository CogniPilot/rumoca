//! Mutation fixtures for every protected presentation-boundary escape class.

mod cargo_contexts;
mod schema_contexts;

use super::*;
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::PathBuf;

fn source_context(
    path: &str,
    target: &str,
    module_path: &[&str],
    source: &str,
) -> ProductionRustSourceContext {
    let path = PathBuf::from(path);
    ProductionRustSourceContext {
        canonical_path: path.clone(),
        crate_aliases: BTreeMap::new(),
        module_path: module_path
            .iter()
            .map(|segment| (*segment).to_string())
            .collect(),
        path,
        source: source.to_string(),
        target: target.to_string(),
    }
}

const RENAMED_HELPERS_FIXTURE: &str = r#"
                use minijinja::{Environment, UndefinedBehavior, Value};
                fn command(value: &Value) -> Value {
                    let name = extract_name(value);
                    Value::from(build_dialect(name))
                }
                fn extract_name(_value: &Value) -> &str { "x" }
                fn build_dialect(name: &str) -> String { format!("{name};") }
                fn closure_escape(value: &Value) { let _emit = || format!("{value:?}"); }
                fn semantic_change(value: &Value) { rewrite_tree(value); }
                fn quote(value: String) -> String { format!("\"{value}\"") }
                fn append_marker(mut value: String) -> String { value.push(';'); value }
                fn typed_default() -> String {
                    let mut out: String = Default::default();
                    out.push('x');
                    out
                }
                fn collect_text(values: impl Iterator<Item = char>) -> String {
                    values.collect::<String>()
                }
                fn inferred_collect(values: impl Iterator<Item = char>) -> String {
                    values.collect()
                }
                fn extended_text(values: impl Iterator<Item = char>) -> String {
                    let mut out: String = Default::default();
                    out.extend(values);
                    out.insert(0, 'x');
                    out
                }
                fn from_iterator(values: impl Iterator<Item = char>) -> String {
                    String::from_iter(values)
                }
                fn ufcs_sinks(out: &mut String, text: &str) {
                    String::push_str(out, text);
                    std::fmt::Write::write_str(out, text).unwrap();
                    std::ops::AddAssign::add_assign(out, text.to_owned());
                    std::iter::Extend::extend(out, text.chars());
                }
                use std::fmt::Write as TextWrite;
                fn aliased_ufcs_sink(out: &mut String, text: &str) {
                    TextWrite::write_fmt(out, format_args!("{text}"));
                }
                fn indirect_ufcs_sink(out: &mut String, text: &str) {
                    let push = String::push_str;
                    push(out, text);
                }
                struct Expression;
                impl Expression {
                    fn literal() -> Self { Self }
                    fn calculate(&self) -> f64 { 1.0 }
                }
                fn adjust(_value: &Expression) -> Expression { Expression::literal() }
                fn calculate(_value: &Expression) -> f64 { match 1 { 1 => 1.0, _ => 0.0 } }
                struct NumericView(f64);
                fn disguised_view(_value: &Expression) -> NumericView { NumericView(1.0) }
                fn nested_consumers() {
                    fn nested(_value: &Expression) -> f64 { 1.0 }
                    let closure = |_value: &Expression| 1.0;
                }
                use crate::Expression as Payload;
                fn alias_mutation(_value: &mut Payload) {}
                type Borrowed<'a> = &'a Expression;
                struct BorrowedView<'a>(&'a Expression);
                struct Adapter<'a>(&'a Expression);
                impl Adapter<'_> {
                    fn project_opcode(&self) -> Vec<u8> {
                        match self.0 { _ => vec![b'+'] }
                    }
                }
                enum LinearOp { Add, Sub }
                fn mlir_native_linear_op_supported(operation: &LinearOp) -> bool {
                    matches!(operation, LinearOp::Add)
                }
                struct FakeView(Expression);
                struct InteriorMutableView<'a>(&'a std::cell::RefCell<Expression>);
                enum SemanticEnvelope { Owned(Expression) }
                union SemanticUnion { owned: std::mem::ManuallyDrop<Expression> }
                const OP_ADD: &str = "+";
                static STATEMENT_END: &str = ";";
                macro_rules! spell_token { () => { format!("+") }; }
                fn dialect(_value: &Expression) -> &str { OP_ADD }
                trait TargetDialect {
                    fn token(&self, name: &str) -> String { format!("{name};") }
                }
                struct Writer;
                impl Writer { fn write_str(&mut self, _value: &str) {} }
                fn writer_sink(writer: &mut Writer) { writer.write_str("target"); }
                struct Dialect;
                impl Dialect {
                    fn spell(&self, name: &str) -> String { format!("{name};") }
                }
                fn method_indirection(value: &Value, dialect: &Dialect) {
                    dialect.spell(extract_name(value));
                }
                include!("hidden_lowering.rs");
            "#;

#[test]
fn mutation_gate_rejects_renames_helpers_closures_transforms_and_includes() {
    let sources = vec![(
        PathBuf::from("crates/rumoca-phase-codegen/src/views/innocent_name.rs"),
        RENAMED_HELPERS_FIXTURE.to_string(),
    )];
    let findings = analyze_sources(&sources);
    for expected in [
        "build_dialect:generated-text:macro-format",
        "closure_escape:generated-text:macro-format",
        "quote:generated-text:macro-format",
        "append_marker:generated-text:method-push",
        "typed_default:generated-text:method-push",
        "collect_text:generated-text:method-collect",
        "inferred_collect:generated-text:return-text",
        "extended_text:generated-text:method-extend",
        "extended_text:generated-text:method-insert",
        "from_iterator:generated-text:String::from_iter",
        "ufcs_sinks:generated-text:call-push_str",
        "ufcs_sinks:generated-text:call-write_str",
        "ufcs_sinks:generated-text:call-add_assign",
        "ufcs_sinks:generated-text:call-extend",
        "aliased_ufcs_sink:generated-text:call-write_fmt",
        "indirect_ufcs_sink:generated-text:call-push_str",
        "Dialect::spell:generated-text:macro-format",
        "adjust:semantic-ir-owned-return",
        "calculate:semantic-consumer",
        "Expression::calculate:semantic-consumer",
        "disguised_view:semantic-consumer",
        "nested-semantic-consumer:function",
        "nested-semantic-consumer:closure",
        "adjust:semantic-ir-construction:literal",
        "semantic-ir-alias:crate::Expression as Payload",
        "semantic-ir-type-alias:Borrowed",
        "semantic-ir-wrapper:FakeView",
        "semantic-ir-wrapper:InteriorMutableView",
        "semantic-ir-wrapper:SemanticEnvelope",
        "semantic-ir-wrapper:SemanticUnion",
        "Adapter::project_opcode:semantic-consumer",
        "Adapter::project_opcode:generated-text:return-text",
        "mlir_native_linear_op_supported:semantic-consumer",
        "OP_ADD:generated-text:text-const",
        "STATEMENT_END:generated-text:text-static",
        "spell_token:generated-text:macro-rules",
        "dialect:generated-text:return-text",
        "TargetDialect::token:generated-text:macro-format",
        "writer_sink:generated-text:method-write_str",
        "semantic-transform-call:rewrite_tree",
        "source-include:include",
    ] {
        assert!(
            findings.iter().any(|finding| finding.contains(expected)),
            "boundary escape was not classified: {expected}: {findings:#?}"
        );
    }
    assert!(
        !findings
            .iter()
            .any(|finding| finding.contains("semantic-ir-wrapper:BorrowedView"))
    );
}

#[test]
fn mutation_gate_rejects_evaluator_imports_and_aliased_dependencies() {
    assert_evaluator_imports_are_detected();
    assert_registry_calls_are_detected();
    assert_context_and_object_callables_are_detected();
    assert_indirect_template_callables_are_detected();
    assert_environment_construction_is_detected();
    assert_environment_policy_and_local_objects_are_detected();
    assert_registry_identity_failures_are_detected();
    assert_aliased_dependencies_are_detected();
}

fn assert_evaluator_imports_are_detected() {
    let sources = vec![(
        PathBuf::from("crates/rumoca-phase-codegen/src/views/evaluate.rs"),
        "use rumoca_eval_dae::NumericEvaluator; fn prepare() { evaluate_model(); }".to_string(),
    )];
    let findings = analyze_sources(&sources);
    assert!(
        findings
            .iter()
            .any(|finding| finding.contains("semantic-import:rumoca_eval_dae::NumericEvaluator"))
    );
    assert!(
        findings
            .iter()
            .any(|finding| finding.contains("semantic-transform-call:evaluate_model"))
    );
}

fn assert_registry_calls_are_detected() {
    let registry_escape = vec![(
        PathBuf::from("crates/rumoca-phase-codegen/src/codegen/registry_escape.rs"),
        r#"
            fn fold_equations(values: Vec<f64>) -> f64 { values.into_iter().sum() }
            fn configure(env: &mut Environment) {
                env.add_function("combine", fold_equations);
            }
        "#
        .to_string(),
    )];
    assert!(analyze_sources(&registry_escape).iter().any(|finding| {
        finding.contains("template-registry:function:combine=>")
            && finding.contains("disposition-unclassified")
    }));
    let ufcs_registry_escape = vec![(
        PathBuf::from("crates/rumoca-phase-codegen/src/codegen/ufcs_registry_escape.rs"),
        r#"
            fn fold_equations(values: Vec<f64>) -> f64 { values.into_iter().sum() }
            fn configure(env: &mut Environment) {
                Environment::add_function(&mut *env, "combine", fold_equations);
            }
        "#
        .to_string(),
    )];
    assert!(
        analyze_sources(&ufcs_registry_escape)
            .iter()
            .any(|finding| {
                finding.contains("template-registry:function:combine=>")
                    && finding.contains("::fold_equations:")
                    && finding.contains("disposition-unclassified")
            })
    );
    let hidden_registry_escape = vec![(
        PathBuf::from("crates/rumoca-phase-codegen/src/codegen/hidden_registry_escape.rs"),
        r#"
            macro_rules! install {
                ($env:expr) => { $env.add_function("macro_hidden", hidden) };
            }
            static INSTALL: LazyLock<()> = LazyLock::new(|| {
                env.add_filter("static_hidden", hidden);
            });
        "#
        .to_string(),
    )];
    assert!(
        analyze_sources(&hidden_registry_escape)
            .iter()
            .any(|finding| {
                finding.contains("template-registry-unresolved-token-count:2:inventoried-0")
            })
    );
    let hidden_findings = analyze_sources(&hidden_registry_escape);
    let laundered_baseline = grouped_findings(&hidden_findings);
    assert!(debt_mismatch(&laundered_baseline, &laundered_baseline).is_empty());
    assert!(
        absolute_registry_findings(&hidden_findings)
            .iter()
            .any(|finding| finding.contains("template-registry-unresolved-token-count"))
    );
}

fn assert_context_and_object_callables_are_detected() {
    let context_callable_escape = vec![(
        PathBuf::from("crates/rumoca-phase-codegen/src/codegen/context_callable_escape.rs"),
        r#"
            fn fold_equations(values: Vec<f64>) -> f64 { values.into_iter().sum() }
            fn expose() {
                let _context = minijinja::context! {
                    combine => Value::from_function(fold_equations),
                };
            }
        "#
        .to_string(),
    )];
    let context_findings = analyze_sources(&context_callable_escape);
    assert!(context_findings.iter().any(|finding| {
        finding.contains("template-callable-constructor-unresolved-token-count:1:inventoried-0")
    }));
    assert!(absolute_registry_findings(&context_findings)
        .iter()
        .any(|finding| finding.contains("template-callable-constructor-unresolved-token-count")));

    let object_callable_escape = vec![(
        PathBuf::from("crates/rumoca-phase-codegen/src/codegen/object_callable_escape.rs"),
        r#"
            struct Bridge;
            impl minijinja::value::Object for Bridge {
                fn call(
                    self: &std::sync::Arc<Self>,
                    _state: &minijinja::State<'_, '_>,
                    _args: &[Value],
                ) -> Result<Value, Error> {
                    Ok(Value::UNDEFINED)
                }
                fn call_method(
                    self: &std::sync::Arc<Self>,
                    _state: &minijinja::State<'_, '_>,
                    _name: &str,
                    _args: &[Value],
                ) -> Result<Value, Error> {
                    Ok(Value::UNDEFINED)
                }
            }
            fn expose() {
                let _context = minijinja::context! {
                    combine => Value::from_object(Bridge),
                };
            }
        "#
        .to_string(),
    )];
    let object_findings = analyze_sources(&object_callable_escape);
    assert!(
        object_findings
            .iter()
            .any(|finding| { finding.contains("template-object-call-method-token-count:1") })
    );
    assert!(
        object_findings
            .iter()
            .any(|finding| { finding.contains("template-object-call-token-count:1") })
    );
    assert!(
        absolute_registry_findings(&object_findings)
            .iter()
            .any(|finding| finding.contains("template-object-call-token-count"))
    );
}

fn assert_indirect_template_callables_are_detected() {
    let aliased_object_callable_escape = vec![(
        PathBuf::from("crates/rumoca-phase-codegen/src/codegen/aliased_object_escape.rs"),
        r#"
            use minijinja::value::Object as Harmless;
            struct Bridge;
            impl Harmless for Bridge {
                fn call(
                    self: &std::sync::Arc<Self>,
                    _state: &minijinja::State<'_, '_>,
                    _args: &[Value],
                ) -> Result<Value, Error> {
                    Ok(Value::UNDEFINED)
                }
            }
        "#
        .to_string(),
    )];
    let aliased_object_findings = analyze_sources(&aliased_object_callable_escape);
    assert!(
        aliased_object_findings
            .iter()
            .any(|finding| { finding.contains("template-object-trait-alias-token-count:1") })
    );
    assert!(
        absolute_registry_findings(&aliased_object_findings)
            .iter()
            .any(|finding| finding.contains("template-object-trait-alias-token-count"))
    );

    let unknown_method_callback = vec![(
        PathBuf::from("crates/rumoca-phase-codegen/src/codegen/unknown_method_callback.rs"),
        r#"
            fn configure(env: &mut Environment) {
                env.set_unknown_method_callback(|_state, value, method, args| {
                    Ok(Value::UNDEFINED)
                });
            }
        "#
        .to_string(),
    )];
    let callback_findings = analyze_sources(&unknown_method_callback);
    assert!(
        callback_findings
            .iter()
            .any(|finding| { finding.contains("template-unknown-method-callback-token-count:1") })
    );
    assert!(
        absolute_registry_findings(&callback_findings)
            .iter()
            .any(|finding| finding.contains("template-unknown-method-callback-token-count"))
    );

    let custom_formatter = vec![(
        PathBuf::from("crates/rumoca-phase-codegen/src/codegen/custom_formatter.rs"),
        r#"
            fn configure(env: &mut Environment) {
                env.set_formatter(|out, state, value| write!(out, "{value}"));
            }
        "#
        .to_string(),
    )];
    let formatter_findings = analyze_sources(&custom_formatter);
    assert!(
        formatter_findings
            .iter()
            .any(|finding| { finding.contains("template-custom-formatter-token-count:1") })
    );
    assert!(
        absolute_registry_findings(&formatter_findings)
            .iter()
            .any(|finding| finding.contains("template-custom-formatter-token-count"))
    );
}

fn assert_environment_construction_is_detected() {
    let unconfigured_environment = vec![(
        PathBuf::from("crates/rumoca-phase-codegen/src/codegen/alternate_environment.rs"),
        "fn alternate() { let _env = minijinja::Environment::new(); }".to_string(),
    )];
    let environment_findings = analyze_sources(&unconfigured_environment);
    assert!(environment_findings.iter().any(|finding| {
        finding.contains(
            "template-environment-policy:new-tokens-1:alternative-constructor-tokens-0:strict-tokens-0",
        )
    }));
    assert!(
        absolute_registry_findings(&environment_findings)
            .iter()
            .any(|finding| finding.contains("template-environment-policy"))
    );

    for (source, expected) in [
        (
            "fn alternate() { let _env = minijinja::Environment::empty(); }",
            "template-environment-policy",
        ),
        (
            "fn alternate() { let _env = minijinja::Environment::default(); }",
            "template-environment-policy",
        ),
        (
            "fn alternate() { let _env: minijinja::Environment = Default::default(); }",
            "template-ambiguous-default-constructor-token-count:1",
        ),
        (
            "use minijinja::Environment as E; fn alternate() { let _env = E::new(); }",
            "template-environment-alias-token-count:1",
        ),
        (
            "type TemplateEnv<'a> = minijinja::Environment<'a>; fn alternate() { let _env = TemplateEnv::new(); }",
            "template-environment-type-alias-token-count:1",
        ),
        (
            "type TemplateEnv<'a> = minijinja::Environment<'a>; fn alternate() { let _env = TemplateEnv::default(); }",
            "template-environment-type-alias-token-count:1",
        ),
        (
            "fn alternate() { type TemplateEnv<'a> = minijinja::Environment<'a>; let _env = TemplateEnv::new(); }",
            "template-environment-type-alias-token-count:1",
        ),
        (
            "fn alternate() { use minijinja::Environment as TemplateEnv; let _env = TemplateEnv::new(); }",
            "template-environment-alias-token-count:1",
        ),
        (
            "fn make<T: Default>() -> T { T::default() } fn alternate() -> minijinja::Environment<'static> { make() }",
            "template-environment-policy",
        ),
        (
            "fn alternate() -> minijinja::Environment<'static> { <minijinja::Environment as Default>::default() }",
            "template-environment-policy",
        ),
    ] {
        let findings = analyze_sources(&[(
            PathBuf::from("crates/rumoca-phase-codegen/src/codegen/alternate_environment.rs"),
            source.to_string(),
        )]);
        assert!(
            absolute_registry_findings(&findings)
                .iter()
                .any(|finding| finding.contains(expected)),
            "missing environment-construction rejection for {source}: {findings:#?}"
        );
    }
}

fn assert_environment_policy_and_local_objects_are_detected() {
    let permissive_environment = analyze_sources(&[(
        PathBuf::from("crates/rumoca-phase-codegen/src/codegen/mod.rs"),
        r#"
            use minijinja::{Environment, UndefinedBehavior};
            fn create_environment() -> Environment<'static> {
                let mut env = Environment::new();
                env.set_undefined_behavior(UndefinedBehavior::Strict);
                env
            }
            fn permissive_env() -> Environment<'static> {
                let mut env = create_environment();
                env.set_undefined_behavior(UndefinedBehavior::Lenient);
                env
            }
        "#
        .to_string(),
    )]);
    assert!(
        absolute_registry_findings(&permissive_environment)
            .iter()
            .any(|finding| finding.contains("template-environment-policy"))
    );

    let local_object = analyze_sources(&[(
        PathBuf::from("crates/rumoca-phase-codegen/src/codegen/local_object.rs"),
        r#"
            fn install() {
                struct Bridge;
                impl minijinja::value::Object for Bridge {
                    fn call(
                        self: &std::sync::Arc<Self>,
                        _state: &minijinja::State<'_, '_>,
                        _args: &[Value],
                    ) -> Result<Value, Error> { Ok(Value::UNDEFINED) }
                }
            }
        "#
        .to_string(),
    )]);
    assert!(
        absolute_registry_findings(&local_object)
            .iter()
            .any(|finding| finding.contains("template-object-call-token-count:1"))
    );
}

fn assert_registry_identity_failures_are_detected() {
    let test_only_registrations = vec![(
        PathBuf::from("crates/rumoca-phase-codegen/src/codegen/test_only_registrations.rs"),
        r#"
            struct TestInstaller;
            impl TestInstaller {
                #[cfg(test)]
                fn install(env: &mut Environment) {
                    env.add_function("test_only_function", hidden);
                }
            }
            trait TestRegistration {
                #[cfg(test)]
                fn install(env: &mut Environment) {
                    env.add_filter("test_only_filter", hidden);
                }
            }
        "#
        .to_string(),
    )];
    assert!(
        analyze_sources(&test_only_registrations)
            .iter()
            .all(|finding| !finding.contains("template-registry"))
    );
    let global_escape = vec![(
        PathBuf::from("crates/rumoca-phase-codegen/src/codegen/global_escape.rs"),
        r#"
            fn fold_equations(values: Vec<f64>) -> f64 { values.into_iter().sum() }
            fn configure(env: &mut Environment) {
                env.add_global("combine", Value::from_function(fold_equations));
            }
        "#
        .to_string(),
    )];
    assert!(analyze_sources(&global_escape).iter().any(|finding| {
        finding.contains("template-registry:global:combine=>")
            && finding.contains("::fold_equations:")
            && finding.contains("disposition-unclassified")
    }));
    let duplicate_registry = vec![(
        PathBuf::from("crates/rumoca-phase-codegen/src/codegen/duplicate_registry.rs"),
        r#"
            fn first(value: f64) -> f64 { value }
            fn second(value: f64) -> bool { value > 0.0 }
            fn configure(env: &mut Environment) {
                env.add_function("shared", first);
                env.add_test("shared", second);
            }
        "#
        .to_string(),
    )];
    assert!(
        analyze_sources(&duplicate_registry).iter().any(|finding| {
            finding.contains("template-registry-duplicate-public-name:shared:2")
        })
    );
    let stolen_reviewed_name = vec![(
        PathBuf::from("crates/rumoca-phase-codegen/src/codegen/mod.rs"),
        r#"
            fn first(value: f64) -> f64 { value }
            fn create_environment(env: &mut Environment) {
                env.add_function("fail", first);
            }
        "#
        .to_string(),
    )];
    assert!(
        analyze_sources(&stolen_reviewed_name)
            .iter()
            .any(|finding| {
                finding.contains("template-registry:function:fail=>")
                    && finding.contains("disposition-unclassified")
            })
    );
    assert!(
        analyze_template_source(
            "crates/rumoca-phase-codegen/src/templates/fake/registry_escape.jinja",
            "{{ combine(values) }}",
        )
        .iter()
        .any(|finding| finding.contains("template-reviewed-logic:1:"))
    );
}

fn assert_aliased_dependencies_are_detected() {
    let dependencies = forbidden_manifest_dependencies(
        r#"
            [package]
            name = "fixture"
            [target.'cfg(unix)'.dependencies]
            harmless_alias = { package = "rumoca-eval-dae", version = "1" }
        "#,
        "",
    );
    assert!(
        dependencies
            .iter()
            .any(|dependency| dependency == "target.cfg(unix).dependencies:rumoca-eval-dae")
    );

    let duplicate_identity = occurrence_counts(forbidden_manifest_dependencies(
        r#"
            [package]
            name = "fixture"
            [dependencies]
            rumoca-eval-dae = "1"
            second_route = { package = "rumoca-eval-dae", version = "1" }
        "#,
        "",
    ));
    assert_eq!(
        duplicate_identity.get("dependencies:rumoca-eval-dae"),
        Some(&2)
    );

    let inherited_alias = forbidden_manifest_dependencies(
        r#"
            [package]
            name = "fixture"
            [target.'cfg(unix)'.dependencies]
            harmless_alias = { workspace = true }
        "#,
        r#"
            [workspace]
            [workspace.dependencies]
            harmless_alias = { package = "rumoca-eval-dae", version = "1" }
        "#,
    );
    assert!(
        inherited_alias
            .iter()
            .any(|dependency| dependency == "target.cfg(unix).dependencies:rumoca-eval-dae")
    );

    let build_dependencies = forbidden_manifest_dependencies(
        r#"
            [package]
            name = "fixture"
            [build-dependencies]
            evaluator = { package = "rumoca-eval-dae", version = "1" }
            [target.'cfg(unix)'.build-dependencies]
            inherited_phase = { workspace = true }
        "#,
        r#"
            [workspace]
            [workspace.dependencies]
            inherited_phase = { package = "rumoca-phase-dae", version = "1" }
        "#,
    );
    assert!(
        build_dependencies
            .iter()
            .any(|dependency| dependency == "build-dependencies:rumoca-eval-dae")
    );
    assert!(build_dependencies.iter().any(|dependency| {
        dependency == "target.cfg(unix).build-dependencies:rumoca-phase-dae"
    }));
}

#[test]
fn mutation_gate_rejects_modelica_codec_cargo_identity_spoofs() {
    let canonical_phase = r#"
        [package]
        name = "rumoca-phase-codegen"
        [dependencies]
        rumoca-core = { workspace = true }
    "#;
    let canonical_workspace = r#"
        [workspace]
        members = ["crates/rumoca-phase-codegen"]
        [workspace.dependencies]
        rumoca-core = { path = "crates/rumoca-core" }
    "#;
    assert!(
        canonical_modelica_codec_dependency_violations(canonical_phase, canonical_workspace,)
            .is_empty()
    );

    let phase_package_spoof = r#"
        [package]
        name = "rumoca-phase-codegen"
        [dependencies]
        rumoca-core = { package = "plausible-codec", version = "1" }
    "#;
    assert!(
        !canonical_modelica_codec_dependency_violations(phase_package_spoof, canonical_workspace,)
            .is_empty()
    );

    let workspace_package_spoof = r#"
        [workspace]
        members = ["crates/rumoca-phase-codegen"]
        [workspace.dependencies]
        rumoca-core = { package = "plausible-codec", path = "crates/rumoca-core" }
    "#;
    assert!(
        !canonical_modelica_codec_dependency_violations(canonical_phase, workspace_package_spoof,)
            .is_empty()
    );

    let workspace_path_spoof = r#"
        [workspace]
        members = ["crates/rumoca-phase-codegen"]
        [workspace.dependencies]
        rumoca-core = { path = "crates/plausible-codec" }
    "#;
    assert!(
        !canonical_modelica_codec_dependency_violations(canonical_phase, workspace_path_spoof,)
            .is_empty()
    );

    let standalone_phase_spoof = r#"
        [workspace]
        [package]
        name = "rumoca-phase-codegen"
        [dependencies]
        rumoca-core = { workspace = true }
    "#;
    assert!(
        !canonical_modelica_codec_dependency_violations(
            standalone_phase_spoof,
            canonical_workspace,
        )
        .is_empty()
    );

    let detached_workspace_spoof = r#"
        [workspace]
        members = []
        [workspace.dependencies]
        rumoca-core = { path = "crates/rumoca-core" }
    "#;
    assert!(
        !canonical_modelica_codec_dependency_violations(canonical_phase, detached_workspace_spoof,)
            .is_empty()
    );
}

#[test]
fn mutation_gate_accepts_diagnostics_but_reviews_template_transport() {
    assert_exact_template_transport_is_reviewed();
    assert_spoofed_template_transport_is_not_exempt();
    assert_diagnostic_names_do_not_hide_text();
    assert_diagnostic_and_transport_bodies_are_reviewed();
}

#[test]
fn mutation_gate_allows_only_the_exact_grammar_owned_lexical_codec_delegate() {
    let exact = analyze_sources(&[modelica_codec_source(
        r#"::rumoca_core::escape_modelica_string(value)"#,
    )]);
    assert!(exact.iter().any(|finding| {
        finding.contains("template-registry:filter:modelica_string_escape=>")
            && finding.contains("disposition-reviewed-target-neutral-lexical-codec")
    }));
    assert!(
        !exact
            .iter()
            .any(|finding| { finding.contains("modelica_string_escape_filter:generated-text:") }),
        "the exact canonical delegate is the narrow lexical-codec exception: {exact:#?}"
    );

    let shadowable_owner = analyze_sources(&[modelica_codec_source(
        r#"rumoca_core::escape_modelica_string(value)"#,
    )]);
    assert!(shadowable_owner.iter().any(|finding| {
        finding.contains("template-registry:filter:modelica_string_escape=>")
            && finding.contains("disposition-reviewed-non-neutral-debt")
    }));
    assert!(shadowable_owner.iter().any(|finding| {
        finding.contains("modelica_string_escape_filter:generated-text:return-text")
    }));

    let branched = analyze_sources(&[modelica_codec_source(
        r#"if value.is_empty() {
            ::rumoca_core::escape_modelica_string(value)
        } else {
            ::rumoca_core::escape_modelica_string(value)
        }"#,
    )]);
    assert!(branched.iter().any(|finding| {
        finding.contains("template-registry:filter:modelica_string_escape=>")
            && finding.contains("disposition-reviewed-non-neutral-debt")
    }));
    assert!(branched.iter().any(|finding| {
        finding.contains("modelica_string_escape_filter:generated-text:return-text")
    }));

    let semantic_branch = analyze_sources(&[(
        PathBuf::from("crates/rumoca-phase-codegen/src/codegen/mod.rs"),
        r#"
            enum Expression { Present, Missing }
            fn modelica_string_escape_filter(
                value: &str,
                expression: &Expression,
            ) -> String {
                match expression {
                    Expression::Present => ::rumoca_core::escape_modelica_string(value),
                    Expression::Missing => String::new(),
                }
            }
            fn target_template_environment() {
                let mut env = Environment::new();
                env.add_filter("modelica_string_escape", modelica_string_escape_filter);
            }
        "#
        .to_string(),
    )]);
    assert!(
        semantic_branch
            .iter()
            .any(|finding| { finding.contains("modelica_string_escape_filter:semantic-consumer") })
    );
    assert!(semantic_branch.iter().any(|finding| {
        finding.contains("template-registry:filter:modelica_string_escape=>")
            && finding.contains("disposition-reviewed-non-neutral-debt")
    }));

    let syntax_assembly = analyze_sources(&[modelica_codec_source(
        r#"format!("\"{}\"", ::rumoca_core::escape_modelica_string(value))"#,
    )]);
    assert!(syntax_assembly.iter().any(|finding| {
        finding.contains("modelica_string_escape_filter:generated-text:macro-format")
    }));
    assert!(syntax_assembly.iter().any(|finding| {
        finding.contains("template-registry:filter:modelica_string_escape=>")
            && finding.contains("disposition-reviewed-non-neutral-debt")
    }));
}

fn modelica_codec_source(body: &str) -> (PathBuf, String) {
    (
        PathBuf::from("crates/rumoca-phase-codegen/src/codegen/mod.rs"),
        format!(
            r#"
                fn modelica_string_escape_filter(value: &str) -> String {{ {body} }}
                fn target_template_environment() {{
                    let mut env = Environment::new();
                    env.add_filter("modelica_string_escape", modelica_string_escape_filter);
                }}
            "#
        ),
    )
}

fn assert_exact_template_transport_is_reviewed() {
    let sources = vec![
        (
            PathBuf::from("crates/rumoca-phase-codegen/src/errors.rs"),
            r#"
                struct CodegenError;
                impl CodegenError { fn message(_message: String) -> Self { Self } }
                fn diagnostic_message(error: &CodegenError) -> CodegenError {
                    CodegenError::message(format!("{error}"))
                }
            "#
            .to_string(),
        ),
        (
            PathBuf::from("crates/rumoca-phase-codegen/src/codegen/mod.rs"),
            r#"
                use minijinja::{Environment, Value};
                fn create_environment() -> Environment<'static> {
                    let mut env = Environment::new();
                    env.set_undefined_behavior(UndefinedBehavior::Strict);
                    env
                }
                fn transport(
                    template: &minijinja::Template,
                    context: &minijinja::Value,
                ) -> Result<String, Error> {
                    template.render(context)
                }
            "#
            .to_string(),
        ),
        (
            PathBuf::from("crates/rumoca-phase-codegen/src/codegen/dae_diagnostics.rs"),
            r#"
                use minijinja::Environment;
                fn register(_environment: &mut Environment<'static>) {}
            "#
            .to_string(),
        ),
    ];
    let findings = analyze_sources(&sources);
    assert!(
        findings
            .iter()
            .any(|finding| finding.contains("transport:semantic-consumer")),
        "template transport remains an explicit review boundary: {findings:#?}"
    );
    assert!(
        findings
            .iter()
            .any(|finding| finding.contains("transport:generated-text:return-text")),
        "template transport returning target text remains reviewed: {findings:#?}"
    );
}

fn assert_spoofed_template_transport_is_not_exempt() {
    let spoofed_transport = analyze_sources(&[(
        PathBuf::from("crates/rumoca-phase-codegen/src/codegen/spoof.rs"),
        r#"
            struct Template;
            struct Value;
            impl Template { fn render(&self, _value: &Value) -> Result<String, Error> { todo!() } }
            fn transport(template: &Template, context: &Value) -> Result<String, Error> {
                template.render(context)
            }
        "#
        .to_string(),
    )]);
    assert!(
        spoofed_transport
            .iter()
            .any(|finding| finding.contains("transport:semantic-consumer")),
        "locally spelled template types cannot exempt transport: {spoofed_transport:#?}"
    );

    let aliased_spoof = analyze_sources(&[(
        PathBuf::from("crates/rumoca-phase-codegen/src/codegen/aliased_spoof.rs"),
        r#"
            mod evil {
                pub struct Template;
                pub struct Value;
                impl Template {
                    fn render(&self, _value: &Value) -> Result<String, Error> { todo!() }
                }
            }
            use crate::evil as minijinja;
            fn transport(
                template: &minijinja::Template,
                context: &minijinja::Value,
            ) -> Result<String, Error> {
                template.render(context)
            }
        "#
        .to_string(),
    )]);
    assert!(
        aliased_spoof
            .iter()
            .any(|finding| finding.contains("transport:generated-text:return-text")),
        "a local module aliased as minijinja cannot exempt target text: {aliased_spoof:#?}"
    );
}

fn assert_diagnostic_names_do_not_hide_text() {
    for (path, function) in [
        (
            "crates/rumoca-phase-codegen/src/views/diagnostic_escape.rs",
            r#"fn diagnostic_renderer(value: String) -> String { format!("{value};") }"#,
        ),
        (
            "crates/rumoca-phase-codegen/src/errors.rs",
            r#"
                struct Expression;
                fn renderer(value: &Expression) -> String { format!("{value:?};") }
            "#,
        ),
        (
            "crates/rumoca-phase-codegen/src/errors.rs",
            r#"
                struct CodegenError;
                fn dialect(value: String) -> Result<String, CodegenError> { Ok(format!("{value};")) }
            "#,
        ),
        (
            "crates/rumoca-phase-codegen/src/errors.rs",
            r#"
                struct FakeError;
                impl FakeError { fn dialect(&self, value: String) -> String { format!("{value};") } }
            "#,
        ),
        (
            "crates/rumoca-phase-codegen/src/errors.rs",
            r#"
                struct Expression;
                struct ModelicaError;
                fn dialect(_value: &Expression) -> ModelicaError {
                    let _text = format!("target;");
                    ModelicaError
                }
            "#,
        ),
        (
            "crates/rumoca-phase-codegen/src/modelica_syntax.rs",
            r#"
                struct ModelicaSyntax;
                impl std::fmt::Display for ModelicaSyntax {
                    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                        write!(formatter, "+")
                    }
                }
            "#,
        ),
    ] {
        let disguised = vec![(PathBuf::from(path), function.to_string())];
        assert!(
            analyze_sources(&disguised)
                .iter()
                .any(|finding| finding.contains("generated-text:"))
        );
    }
}

fn assert_diagnostic_and_transport_bodies_are_reviewed() {
    let diagnostic_return_escape = vec![(
        PathBuf::from("crates/rumoca-phase-codegen/src/errors.rs"),
        r#"
            struct Expression;
            impl Expression { fn clear(&mut self) {} }
            struct CodegenError;
            impl CodegenError { fn message(_message: String) -> Self { Self } }
            fn dialect(value: &mut Expression) -> CodegenError {
                value.clear();
                let target = format!("target grammar;");
                CodegenError::message(target)
            }
        "#
        .to_string(),
    )];
    let diagnostic_findings = analyze_sources(&diagnostic_return_escape);
    assert!(
        diagnostic_findings
            .iter()
            .any(|finding| finding.contains("dialect:semantic-consumer:"))
    );
    assert!(
        diagnostic_findings
            .iter()
            .any(|finding| finding.contains("semantic-input-mutation:clear"))
    );
    assert!(
        diagnostic_findings
            .iter()
            .any(|finding| finding.contains("generated-text:macro-format:"))
    );

    let transport_with_sink = vec![(
        PathBuf::from("crates/rumoca-phase-codegen/src/renderer.rs"),
        r#"
            use minijinja::Value;
            fn transport(template: &Template, context: &Value) -> Result<String, Error> {
                let dialect = format!("{context:?};");
                template.render(dialect)
            }
        "#
        .to_string(),
    )];
    assert!(
        analyze_sources(&transport_with_sink)
            .iter()
            .any(|finding| finding.contains("transport:generated-text:macro-format"))
    );

    let transport_with_semantics = vec![(
        PathBuf::from("crates/rumoca-phase-codegen/src/renderer.rs"),
        r#"
            use minijinja::Value;
            struct Expression;
            fn transport(
                template: &Template,
                context: &Value,
                expression: &Expression,
            ) -> Result<String, Error> {
                let _value = calculate(expression);
                template.render(context)
            }
        "#
        .to_string(),
    )];
    assert!(
        analyze_sources(&transport_with_semantics)
            .iter()
            .any(|finding| finding.contains("transport:semantic-consumer"))
    );
}

#[test]
fn mutation_gate_tracks_semantic_carriers_across_production_files() {
    let mut sources = cross_file_semantic_sources();
    sources.extend(same_name_collision_sources());
    let findings = analyze_source_contexts(&sources);
    for expected in [
        "Adapter::native_supported:semantic-consumer",
        "semantic-ir-alias:rumoca_ir_solve::DiscreteSolveSystem as Schedule",
        "decide:semantic-consumer",
        "inspect:semantic-consumer",
        "inspect_hidden:semantic-consumer",
        "inspect_legacy:semantic-consumer",
    ] {
        assert!(
            findings.iter().any(|finding| finding.contains(expected)),
            "cross-file or expanded-type carrier escaped: {expected}: {findings:#?}"
        );
    }
    for unrelated in [
        "numeric_adapter_count",
        "sibling_adapter_count",
        "unrelated_external",
    ] {
        assert!(
            !findings.iter().any(|finding| {
                finding.contains(unrelated) && finding.contains("semantic-consumer")
            }),
            "unrelated same-name carrier was tainted: {unrelated}: {findings:#?}"
        );
    }
}

fn cross_file_semantic_sources() -> Vec<ProductionRustSourceContext> {
    vec![
        source_context(
            "crates/rumoca-phase-codegen/src/carrier.rs",
            "lib",
            &["carrier"],
            r#"
                pub struct Adapter<'a>(pub &'a rumoca_ir_solve::LinearOp);
            "#,
        ),
        source_context(
            "crates/rumoca-phase-codegen/src/lower.rs",
            "lib",
            &["lower"],
            r#"
                use crate::carrier::Adapter;
                impl Adapter<'_> {
                    fn native_supported(&self) -> bool {
                        matches!(self.0, LinearOp::Add { .. })
                    }
                }
            "#,
        ),
        source_context(
            "crates/rumoca-phase-codegen/src/schedule.rs",
            "lib",
            &["schedule"],
            r#"
                use rumoca_ir_solve::DiscreteSolveSystem as Schedule;
                struct ScheduleAdapter<'a>(&'a Schedule);
                fn decide(_schedule: &ScheduleAdapter<'_>) -> bool { true }
            "#,
        ),
        source_context(
            "crates/rumoca-phase-codegen/src/root_alias.rs",
            "lib",
            &["root_alias"],
            r#"
                use rumoca_ir_galec as ir;
                fn inspect(block: &ir::ast::Block) -> bool {
                    !block.public_functions.is_empty()
                }
            "#,
        ),
        source_context(
            "crates/rumoca-phase-codegen/src/semantic_reexport.rs",
            "lib",
            &["semantic_reexport"],
            "pub use rumoca_ir_galec::ast as domain;",
        ),
        source_context(
            "crates/rumoca-phase-codegen/src/namespace_alias.rs",
            "lib",
            &["namespace_alias"],
            "pub use crate::semantic_reexport as semantic;",
        ),
        source_context(
            "crates/rumoca-phase-codegen/src/hidden_alias.rs",
            "lib",
            &["hidden_alias"],
            r#"
                use crate::namespace_alias::semantic::domain as hidden;
                fn inspect_hidden(block: &hidden::Block) -> bool {
                    !block.public_functions.is_empty()
                }
            "#,
        ),
    ]
}

fn same_name_collision_sources() -> Vec<ProductionRustSourceContext> {
    vec![
        source_context(
            "crates/rumoca-phase-codegen/src/numeric.rs",
            "lib",
            &["numeric"],
            r#"
                struct Adapter(usize);
                impl Adapter { fn numeric_adapter_count(&self) -> usize { self.0 } }
            "#,
        ),
        source_context(
            "crates/rumoca-phase-codegen/src/siblings.rs",
            "lib",
            &["siblings"],
            r#"
                mod semantic {
                    struct Expression;
                    struct Adapter<'a>(&'a Expression);
                }
                mod numeric {
                    struct Adapter(usize);
                    impl Adapter { fn sibling_adapter_count(&self) -> usize { self.0 } }
                }
            "#,
        ),
        source_context(
            "crates/rumoca-phase-codegen/src/legacy.rs",
            "lib",
            &["legacy"],
            r#"
                extern crate rumoca_ir_dae as legacy_ir;
                fn inspect_legacy(expression: &legacy_ir::Expression) -> bool {
                    matches!(expression, legacy_ir::Expression::Time)
                }
            "#,
        ),
        source_context(
            "crates/rumoca-phase-codegen/src/extern_collision.rs",
            "lib",
            &["extern_collision"],
            r#"
                extern crate numeric as rumoca_ir_dae;
                fn unrelated_external(expression: &rumoca_ir_dae::Expression) -> bool {
                    expression.is_finite()
                }
            "#,
        ),
    ]
}

#[test]
fn mutation_gate_rejects_zero_input_semantic_fabrication() {
    let findings = analyze_sources(&[(
        PathBuf::from("crates/rumoca-phase-codegen/src/factory.rs"),
        r#"
            use rumoca_ir_solve::DiscreteSolveSystem as Schedule;
            fn fabricate() -> Schedule { Default::default() }
            fn fabricate_lazy() -> impl Iterator<Item = Schedule> {
                std::iter::once(Default::default())
            }
            fn factory_type() -> fn() -> Schedule { fabricate }
            struct Expression;
            type Borrowed<'a> = &'a Expression;
            fn view<'a>(value: &'a Expression) -> Borrowed<'a> { value }
            const EMPTY: Schedule = Schedule::empty();
        "#
        .to_string(),
    )]);
    for expected in [
        "fabricate:semantic-ir-owned-return",
        "fabricate_lazy:semantic-ir-owned-return",
        "factory_type:semantic-ir-owned-return",
        "EMPTY:semantic-ir-const",
    ] {
        assert!(
            findings.iter().any(|finding| finding.contains(expected)),
            "zero-input semantic fabrication escaped: {expected}: {findings:#?}"
        );
    }
    assert!(
        !findings
            .iter()
            .any(|finding| finding.contains("view:semantic-ir-owned-return")),
        "a borrowed read-only semantic alias is not an owned IR product: {findings:#?}"
    );
}

mod semantic_carrier_cases;
