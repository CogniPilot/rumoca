use super::*;

#[test]
fn mutation_gate_tracks_text_aliases_bytes_and_target_paths() {
    let mut aliases = "type A1 = String;\n".to_string();
    for ordinal in 2..=12 {
        aliases.push_str(&format!("type A{ordinal} = A{};\n", ordinal - 1));
    }
    aliases.push_str(
        r#"
            type Text = String;
            fn keyword() -> Text { "model".into() }
            fn emit_alias(mut out: Text) { out.push('x'); }
            fn construct_alias() { let mut out = Text::new(); out.push('x'); }
            fn emit_long(out: &mut A12) { A12::push(out, 'x'); }
            fn emit_bytes(out: &mut Vec<u8>) { out.push(b'x'); }
            fn emit_char(out: &mut String) { std::fmt::Write::write_char(out, 'x').unwrap(); }
            fn emit_slice(out: &mut Vec<u8>) { out.extend_from_slice(b"model"); }
            fn emit_all(out: &mut impl std::io::Write) { out.write_all(b"model").unwrap(); }
            fn emit_strings(out: &mut Vec<String>) { out.push("model".into()); }
            fn construct_bytes() { let mut out = Vec::<u8>::new(); out.push(b'x'); }
            fn inferred_text() { let mut out = Default::default(); out.push('x'); }
            fn filename() -> std::path::PathBuf { std::path::PathBuf::from("model.c") }
            fn dialect() -> minijinja::Value { minijinja::Value::from("model") }
        "#,
    );
    let findings = analyze_source_contexts(&[
        source_context(
            "crates/rumoca-phase-codegen/src/text_aliases.rs",
            "lib",
            &["text_aliases"],
            &aliases,
        ),
        source_context(
            "crates/rumoca-phase-codegen/src/text_reexport.rs",
            "lib",
            &["text_reexport"],
            "pub use std::string::String as ExportedText;",
        ),
        source_context(
            "crates/rumoca-phase-codegen/src/text_consumer.rs",
            "lib",
            &["text_consumer"],
            r#"
                use crate::text_reexport::ExportedText as DeepText;
                fn emit_import(mut out: DeepText) { out.push('x'); }
            "#,
        ),
        source_context(
            "crates/rumoca-phase-codegen/src/numeric_text.rs",
            "lib",
            &["numeric_text"],
            r#"
                type Text = usize;
                fn numeric_text(value: Text) -> Text { value + 1 }
            "#,
        ),
    ]);
    for expected in [
        "keyword:generated-text:return-text",
        "emit_alias:generated-text:method-push",
        "construct_alias:generated-text:method-push",
        "emit_long:generated-text:call-push",
        "emit_bytes:generated-text:method-push",
        "emit_char:generated-text:call-write_char",
        "emit_slice:generated-text:method-extend_from_slice",
        "emit_all:generated-text:method-write_all",
        "emit_strings:generated-text:method-push",
        "construct_bytes:generated-text:method-push",
        "inferred_text:generated-text:method-push",
        "filename:generated-text:",
        "dialect:generated-text:template-value-text",
        "emit_import:generated-text:method-push",
    ] {
        assert!(
            findings.iter().any(|finding| finding.contains(expected)),
            "text carrier escaped: {expected}: {findings:#?}"
        );
    }
    assert!(
        !findings
            .iter()
            .any(|finding| finding.contains("numeric_text:generated-text")),
        "a same-name alias in another module inherited text identity: {findings:#?}"
    );
}

#[test]
fn mutation_gate_follows_production_modules_in_test_named_files() {
    let temporary = tempfile::tempdir().expect("temporary Rust crate");
    let src = temporary.path().join("src");
    fs::create_dir_all(&src).expect("create fixture src");
    fs::write(
        temporary.path().join("Cargo.toml"),
        "[package]\nname = \"production-modules-fixture\"\nversion = \"0.0.0\"\nedition = \"2024\"\n",
    )
    .expect("write fixture manifest");
    fs::write(
        src.join("lib.rs"),
        r#"
            mod dialect_tests;
            mod outer;
            mod inline_outer { #[path = "inline_impl.rs"] mod child; }
            #[cfg(test)] mod actual_tests;
        "#,
    )
    .expect("write fixture lib.rs");
    fs::write(
        src.join("dialect_tests.rs"),
        r#"fn spell(value: String) -> String { format!("{value};") }"#,
    )
    .expect("write production test-named module");
    fs::write(
        src.join("actual_tests.rs"),
        r#"fn ignored_fixture(value: String) -> String { format!("{value};") }"#,
    )
    .expect("write cfg(test) module");
    fs::create_dir_all(src.join("outer")).expect("create logical module directory");
    fs::write(src.join("outer.rs"), "#[path = \"impl.rs\"] mod child;")
        .expect("write non-mod.rs parent");
    fs::write(
        src.join("outer/impl.rs"),
        "fn scanner_must_not_pick_logical_dir_for_file_module() {}",
    )
    .expect("write incorrect logical-directory candidate");
    fs::write(
        src.join("impl.rs"),
        r#"fn file_module_spell(value: String) -> String { format!("{value};") }"#,
    )
    .expect("write rustc-resolved file-module path");
    fs::create_dir_all(src.join("inline_outer")).expect("create inline module directory");
    fs::write(
        src.join("inline_outer/inline_impl.rs"),
        r#"fn inline_module_spell(value: String) -> String { format!("{value};") }"#,
    )
    .expect("write rustc-resolved inline-module path");

    let sources = production_rust_sources(temporary.path(), temporary.path());
    let contexts = production_rust_source_contexts(temporary.path(), temporary.path());
    assert!(
        sources
            .iter()
            .any(|(path, _)| path.ends_with("dialect_tests.rs"))
    );
    assert!(
        !sources
            .iter()
            .any(|(path, _)| path.ends_with("actual_tests.rs"))
    );
    assert!(
        !sources
            .iter()
            .any(|(path, _)| path.ends_with("outer/impl.rs"))
    );
    assert!(
        sources
            .iter()
            .any(|(path, _)| path.ends_with("src/impl.rs"))
    );
    assert!(
        sources
            .iter()
            .any(|(path, _)| path.ends_with("inline_outer/inline_impl.rs"))
    );
    for (path, expected_module) in [
        ("src/impl.rs", vec!["outer", "child"]),
        ("inline_outer/inline_impl.rs", vec!["inline_outer", "child"]),
    ] {
        assert!(
            contexts.iter().any(|context| {
                context.path.ends_with(path)
                    && context.module_path == expected_module
                    && context.target == "lib"
            }),
            "#[path] source lost its lexical Rust module identity: {path}: {contexts:#?}"
        );
    }
    assert!(
        analyze_sources(&sources)
            .iter()
            .any(|finding| finding.contains("dialect_tests.rs::spell:generated-text:macro-format"))
    );
}

#[test]
fn mutation_gate_scans_every_cargo_production_target_root() {
    let temporary = tempfile::tempdir().expect("temporary Rust crate");
    let src = temporary.path().join("src");
    fs::create_dir_all(src.join("bin/nested")).expect("create target fixture tree");
    fs::write(
        temporary.path().join("Cargo.toml"),
        r#"
            [package]
            name = "target-roots-fixture"
            version = "0.0.0"
            edition = "2024"

            [lib]
            path = "src/hidden.rs"

            [[bin]]
            name = "explicit"
            path = "src/explicit.rs"
        "#,
    )
    .expect("write target fixture manifest");
    for (relative, source) in [
        (
            "hidden.rs",
            r#"fn hidden_lib() -> String { format!("lib;") }"#,
        ),
        ("main.rs", r#"fn main_bin() -> String { format!("main;") }"#),
        (
            "explicit.rs",
            r#"fn explicit_bin() -> String { format!("explicit;") }"#,
        ),
        (
            "bin/auto.rs",
            r#"fn auto_bin() -> String { format!("auto;") }"#,
        ),
        (
            "bin/nested/main.rs",
            r#"fn nested_bin() -> String { format!("nested;") }"#,
        ),
    ] {
        fs::write(src.join(relative), source).expect("write production target root");
    }
    fs::write(
        src.join("lib.rs"),
        "fn default_lib_must_not_be_scanned() {}",
    )
    .expect("write shadowed default lib root");

    let roots = production_rust_target_roots(temporary.path(), temporary.path());
    for expected in [
        ("lib", "src/hidden.rs"),
        ("bin:auto:main", "src/main.rs"),
        ("bin:explicit", "src/explicit.rs"),
        ("bin:auto", "src/bin/auto.rs"),
        ("bin:auto", "src/bin/nested/main.rs"),
    ] {
        assert!(
            roots
                .iter()
                .any(|(kind, path)| kind == expected.0 && path.ends_with(expected.1)),
            "missing Cargo target root {expected:?}: {roots:#?}"
        );
    }
    let sources = production_rust_sources(temporary.path(), temporary.path());
    let contexts = production_rust_source_contexts(temporary.path(), temporary.path());
    assert!(!sources.iter().any(|(path, _)| path.ends_with("src/lib.rs")));
    assert!(
        contexts.iter().any(|context| {
            context.path.ends_with("src/hidden.rs")
                && context.target == "lib"
                && context.module_path.is_empty()
        }),
        "a custom [lib] path is still the crate-root module: {contexts:#?}"
    );
    let findings = analyze_sources(&sources);
    for owner in [
        "hidden_lib:generated-text:macro-format",
        "main_bin:generated-text:macro-format",
        "explicit_bin:generated-text:macro-format",
        "auto_bin:generated-text:macro-format",
        "nested_bin:generated-text:macro-format",
    ] {
        assert!(
            findings.iter().any(|finding| finding.contains(owner)),
            "production target root escaped analysis: {owner}: {findings:#?}"
        );
    }
}

#[test]
fn mutation_gate_respects_disabled_cargo_auto_targets() {
    let temporary = tempfile::tempdir().expect("temporary Rust crate");
    let src = temporary.path().join("src");
    fs::create_dir_all(&src).expect("create target fixture tree");
    fs::write(
        temporary.path().join("Cargo.toml"),
        r#"
            [package]
            name = "disabled-auto-target-fixture"
            version = "0.0.0"
            edition = "2024"
            autolib = false
        "#,
    )
    .expect("write target fixture manifest");
    fs::write(src.join("main.rs"), "fn main() {}").expect("write automatic binary target root");
    fs::write(
        src.join("lib.rs"),
        r#"fn uncompiled_text() -> String { format!("must not be scanned;") }"#,
    )
    .expect("write disabled automatic library source");

    let roots = production_rust_target_roots(temporary.path(), temporary.path());
    assert_eq!(roots.len(), 1, "only the automatic binary is enabled");
    assert_eq!(roots[0].0, "bin:auto:main");
    let sources = production_rust_sources(temporary.path(), temporary.path());
    assert!(
        sources
            .iter()
            .any(|(path, _)| path.ends_with("src/main.rs"))
    );
    assert!(
        !sources.iter().any(|(path, _)| path.ends_with("src/lib.rs")),
        "[package].autolib=false must exclude an otherwise conventional src/lib.rs"
    );
}

#[cfg(unix)]
#[test]
fn mutation_gate_traverses_shared_sources_in_each_lexical_module_context() {
    use std::os::unix::fs::symlink;

    let temporary = tempfile::tempdir().expect("temporary Rust crate");
    let src = temporary.path().join("src");
    fs::create_dir_all(src.join("bin")).expect("create target fixture tree");
    fs::write(
        temporary.path().join("Cargo.toml"),
        r#"
            [package]
            name = "target-context-fixture"
            version = "0.0.0"
            edition = "2024"

            [lib]
            path = "src/hidden.rs"
        "#,
    )
    .expect("write target fixture manifest");
    fs::write(
        src.join("bin/tool.rs"),
        r#"mod payload; fn shared_target_text() -> String { format!("target") }"#,
    )
    .expect("write source shared across target contexts");
    symlink("bin/tool.rs", src.join("hidden.rs")).expect("link custom library root");
    fs::write(src.join("bin/payload.rs"), "fn benign_binary_payload() {}")
        .expect("write binary-context module");
    fs::write(
        src.join("payload.rs"),
        r#"fn hidden_library_lowering() -> String { format!("model Hidden; end Hidden;") }"#,
    )
    .expect("write library-context module");

    let contexts = production_rust_source_contexts(temporary.path(), temporary.path());
    let sources = production_rust_sources(temporary.path(), temporary.path());
    assert!(
        sources
            .iter()
            .any(|(path, _)| path.ends_with("src/payload.rs")),
        "a canonical source reached through two target paths must have its modules traversed in both lexical contexts: {sources:#?}"
    );
    assert!(
        analyze_source_contexts(&contexts)
            .iter()
            .any(|finding| finding.contains("hidden_library_lowering:generated-text:macro-format")),
        "semantic lowering in the second lexical target context escaped analysis"
    );
    assert_eq!(
        analyze_source_contexts(&contexts)
            .iter()
            .filter(|finding| {
                finding.contains("shared_target_text:generated-text:macro-format")
            })
            .count(),
        2,
        "one canonical source compiled in two target contexts needs two review identities"
    );
    let shared_root = contexts
        .iter()
        .filter(|context| context.canonical_path.ends_with("src/bin/tool.rs"))
        .map(|context| (context.target.as_str(), context.module_path.as_slice()))
        .collect::<BTreeSet<_>>();
    assert!(
        shared_root.contains(&("lib", &[][..]))
            && shared_root
                .iter()
                .any(|(target, module)| target.starts_with("bin:") && module.is_empty()),
        "one canonical source must retain both target/module contexts: {contexts:#?}"
    );
}

#[test]
fn mutation_gate_rejects_stale_debt_allowances() {
    assert_debt_ledger_and_consumer_fingerprints_are_exact();
    assert_production_source_set_fingerprint_is_exact();
    assert_text_and_registry_fingerprints_are_exact();
    assert_template_fingerprints_are_exact();
}

fn assert_debt_ledger_and_consumer_fingerprints_are_exact() {
    assert!(
        std::panic::catch_unwind(|| {
            parse_source_debt_baseline(&["same-finding|1", "same-finding|1"])
        })
        .is_err(),
        "duplicate debt rows must be rejected rather than overwritten"
    );
    let current = BTreeMap::<String, usize>::new();
    let baseline = BTreeMap::from([("deleted.rs::spell:generated-text:macro-format", 1)]);
    let mismatch = debt_mismatch(&current, &baseline);
    assert_eq!(mismatch.len(), 1);
    assert!(mismatch[0].contains("live 0, ledger 1"));

    let source_path = PathBuf::from("crates/rumoca-phase-codegen/src/views/same_owner.rs");
    let before = analyze_sources(&[(
        source_path.clone(),
        "struct Expression; fn same(value: &Expression) -> f64 { 1.0 }".to_string(),
    )]);
    let after = analyze_sources(&[(
        source_path,
        "struct Expression; fn same(value: &Expression) -> f64 { match value { _ => 2.0 } }"
            .to_string(),
    )]);
    let before_consumer = before
        .iter()
        .find(|finding| finding.contains("same:semantic-consumer:"))
        .expect("baseline semantic consumer");
    let after_consumer = after
        .iter()
        .find(|finding| finding.contains("same:semantic-consumer:"))
        .expect("modified semantic consumer");
    assert_ne!(before_consumer, after_consumer);
}

fn assert_production_source_set_fingerprint_is_exact() {
    let roots = [("lib".to_string(), PathBuf::from("src/lib.rs"))];
    let before_source_set = production_source_set_finding(
        "[package]\nname='before'",
        &roots,
        &[(
            PathBuf::from("crates/rumoca-phase-codegen/src/codegen/transitive.rs"),
            "static LIMIT: usize = 4;".to_string(),
        )],
    );
    let after_source_set = production_source_set_finding(
        "[package]\nname='before'",
        &roots,
        &[(
            PathBuf::from("crates/rumoca-phase-codegen/src/codegen/transitive.rs"),
            "static LIMIT: usize = 8;".to_string(),
        )],
    );
    assert_ne!(before_source_set, after_source_set);
    let changed_root_kind = production_source_set_finding(
        "[package]\nname='before'",
        &[("bin:auto".to_string(), PathBuf::from("src/lib.rs"))],
        &[(
            PathBuf::from("crates/rumoca-phase-codegen/src/codegen/transitive.rs"),
            "static LIMIT: usize = 4;".to_string(),
        )],
    );
    assert_ne!(before_source_set, changed_root_kind);
    let changed_manifest = production_source_set_finding(
        "[package]\nname='after'",
        &roots,
        &[(
            PathBuf::from("crates/rumoca-phase-codegen/src/codegen/transitive.rs"),
            "static LIMIT: usize = 4;".to_string(),
        )],
    );
    assert_ne!(before_source_set, changed_manifest);

    let before_template_set = template_source_set_finding(&[(
        "templates/model.c.jinja".to_string(),
        "{{ opcode }} + {{ rhs }}".to_string(),
    )]);
    let after_template_set = template_source_set_finding(&[(
        "templates/model.c.jinja".to_string(),
        "{{ opcode }} - {{ rhs }}".to_string(),
    )]);
    assert_ne!(
        before_template_set, after_template_set,
        "static target syntax outside Jinja blocks is review-identified"
    );
}

fn assert_text_and_registry_fingerprints_are_exact() {
    let text_path = PathBuf::from("crates/rumoca-phase-codegen/src/views/same_text_owner.rs");
    let before_text = analyze_sources(&[(
        text_path.clone(),
        r#"fn same() -> String { format!("old grammar") }"#.to_string(),
    )]);
    let after_text = analyze_sources(&[(
        text_path,
        r#"fn same() -> String { format!("changed grammar") }"#.to_string(),
    )]);
    let before_sink = before_text
        .iter()
        .find(|finding| finding.contains("same:generated-text:macro-format:"))
        .expect("baseline generated-text sink");
    let after_sink = after_text
        .iter()
        .find(|finding| finding.contains("same:generated-text:macro-format:"))
        .expect("modified generated-text sink");
    assert_ne!(before_sink, after_sink);

    let registry_path = PathBuf::from("crates/rumoca-phase-codegen/src/views/same_registry.rs");
    let before_registry = analyze_sources(&[(
        registry_path.clone(),
        r#"
            fn combine(values: Vec<f64>) -> f64 { values[0] }
            fn configure(env: &mut Environment) { env.add_function("combine", combine); }
        "#
        .to_string(),
    )]);
    let after_registry = analyze_sources(&[(
        registry_path,
        r#"
            fn combine(values: Vec<f64>) -> f64 { values.into_iter().sum() }
            fn configure(env: &mut Environment) { env.add_function("combine", combine); }
        "#
        .to_string(),
    )]);
    let before_registration = before_registry
        .iter()
        .find(|finding| finding.contains("template-registry:function:combine=>"))
        .expect("baseline template registration");
    let after_registration = after_registry
        .iter()
        .find(|finding| finding.contains("template-registry:function:combine=>"))
        .expect("modified template registration");
    assert_ne!(before_registration, after_registration);

    let guarded_registry = analyze_sources(&[(
        PathBuf::from("crates/rumoca-phase-codegen/src/views/same_registry.rs"),
        r#"
            fn combine(values: Vec<f64>) -> f64 { values[0] }
            fn configure(env: &mut Environment) {
                if false { env.add_function("combine", combine); }
            }
        "#
        .to_string(),
    )]);
    let guarded_registration = guarded_registry
        .iter()
        .find(|finding| finding.contains("template-registry:function:combine=>"))
        .expect("guarded template registration");
    assert_ne!(before_registration, guarded_registration);
}

fn assert_template_fingerprints_are_exact() {
    let template_path = "crates/rumoca-phase-codegen/src/templates/fake/same_line.jinja";
    let before_template =
        analyze_template_source(template_path, "{% set ns.value = ns.value ~ punctuation %}");
    let after_template = analyze_template_source(
        template_path,
        "{% set ns.value = ns.value + semantic.weight %}",
    );
    assert_ne!(before_template, after_template);

    let clean_template_path = "crates/rumoca-phase-codegen/src/templates/fake/logic.jinja";
    let simple = analyze_template_source(clean_template_path, "{{ prepared_values }}");
    let selected = analyze_template_source(
        clean_template_path,
        "{{ prepared_values | selectattr('enabled') | map(attribute='weight') | list }}",
    );
    let arithmetic = analyze_template_source(
        clean_template_path,
        "{% set total = rows[0].weight + rows[1].weight %}",
    );
    assert_ne!(simple, selected);
    assert_ne!(simple, arithmetic);
    assert!(
        selected
            .iter()
            .any(|finding| finding.contains("template-reviewed-logic:1:"))
    );
    assert!(
        arithmetic
            .iter()
            .any(|finding| finding.contains("template-reviewed-logic:1:"))
    );
    assert_ne!(
        analyze_template_source(clean_template_path, "{{ \"Real\" }}"),
        analyze_template_source(clean_template_path, "{{ \"real\" }}")
    );
    assert_ne!(
        analyze_template_source(clean_template_path, "{{ \"a b\" }}"),
        analyze_template_source(clean_template_path, "{{ \"ab\" }}")
    );
    assert_ne!(
        analyze_template_source(clean_template_path, "{{ value }}"),
        analyze_template_source(clean_template_path, "{{- value -}}")
    );
    assert_ne!(
        analyze_template_source(clean_template_path, "{{- value }}"),
        analyze_template_source(clean_template_path, "{{ - value }}")
    );
    let two_blocks = analyze_template_source(clean_template_path, "{{ alpha }}{% if beta %}");
    assert_ne!(
        two_blocks,
        analyze_template_source(clean_template_path, "{% if beta %}{{ alpha }}")
    );
    assert_ne!(
        two_blocks,
        analyze_template_source(clean_template_path, "{{ changed }}{% if beta %}")
    );
    assert_ne!(
        two_blocks,
        analyze_template_source(clean_template_path, "{{ alpha }}")
    );
    assert_ne!(
        two_blocks,
        analyze_template_source(clean_template_path, "{{ alpha }}{% if beta %}{{ added }}")
    );
    assert_ne!(
        analyze_template_source(clean_template_path, "{{ ab }}{{ c }}"),
        analyze_template_source(clean_template_path, "{{ a }}{{ bc }}")
    );
    let before_comment_escape = analyze_template_source(clean_template_path, "{{ alpha }}");
    let after_comment_escape = analyze_template_source(
        clean_template_path,
        "{{ alpha }}\n{# {{ #}\n{% set hidden = range(0, semantic_extent) %}",
    );
    assert_ne!(before_comment_escape, after_comment_escape);
    assert!(
        after_comment_escape
            .iter()
            .any(|finding| finding.contains("template-domain-expansion:range"))
    );
    assert!(
        analyze_template_source(clean_template_path, "{{ unclosed")
            .iter()
            .any(|finding| finding.contains("template-lex-error:unclosed-expression"))
    );
    let raw = analyze_template_source(
        clean_template_path,
        "{% raw %}{{ ignored }}{% set ignored = range(0, n) %}{% endraw %}{{ visible }}",
    );
    assert!(
        !raw.iter()
            .any(|finding| finding.contains("template-domain-expansion:range"))
    );
}

#[test]
fn mutation_gate_tombstones_files_and_registry_spellings() {
    let temporary = tempfile::tempdir().expect("temporary codegen tree");
    fs::create_dir_all(temporary.path().join("src/templates/demo")).expect("create fixture tree");
    fs::write(
        temporary.path().join("Cargo.toml"),
        "[package]\nname = \"tombstone-fixture\"\nversion = \"0.0.0\"\nedition = \"2024\"\n",
    )
    .expect("write tombstone fixture manifest");
    fs::write(
        temporary.path().join("src/templates/demo/output.jinja"),
        "{{ render_expression(node, cfg) }}",
    )
    .expect("write fixture template");
    fs::write(
        temporary.path().join("src/render_stmt.rs"),
        "fn harmless() {}",
    )
    .expect("write fixture source");
    fs::write(
        temporary.path().join("src/lib.rs"),
        r#"
            #[path = "hidden.rs"] mod hidden;
            #[cfg_attr(feature = "dialect", path = "conditional.rs")]
            mod conditional;
            #[cfg_attr(test, path = "test_conditional.rs")]
            mod default_for_production;
            #[cfg(test)] mod only_test;
        "#,
    )
    .expect("write forbidden path module fixture");
    fs::write(temporary.path().join("src/hidden.rs"), "fn hidden() {}")
        .expect("write direct path target");
    fs::write(
        temporary.path().join("src/conditional.rs"),
        "fn conditional() {}",
    )
    .expect("write default conditional path target");
    fs::write(
        temporary.path().join("src/default_for_production.rs"),
        "fn default_for_production() {}",
    )
    .expect("write test-only cfg_attr default target");
    fs::write(
        temporary.path().join("src/only_test.rs"),
        "#[path = \"test_helper.rs\"] mod only_test_helper;",
    )
    .expect("write cfg(test)-only path fixture");
    let offenders = legacy_surface_offenders(temporary.path());
    assert!(
        offenders
            .iter()
            .any(|finding| finding.contains("legacy file"))
    );
    assert!(
        offenders
            .iter()
            .any(|finding| { finding.contains("production #[cfg_attr(..., path = ...)] module") })
    );
    assert!(
        !offenders
            .iter()
            .any(|finding| finding.contains("only_test_helper"))
    );
    assert!(
        !offenders
            .iter()
            .any(|finding| finding.contains("default_for_production")),
        "a cfg_attr that can select an alternate path only in tests is not a production hiding surface"
    );
    assert!(
        offenders
            .iter()
            .any(|finding| finding.contains("`render_expression`"))
    );
}

const TEMPLATE_FALLBACK_FIXTURE: &str = r#"
            {% if variable.binding is not none %}
              = {{ variable.binding }}
            {% elif variable.start is not none %}
              = {{ variable.start }}
            {% endif %}
            {% for index in range(lower, upper) %}
              out[{{ index }}] = expression;
            {% endfor %}
            {% set expand = range %}
            {% set filtered_expand = range | list %}
            {% set schedule = namespace(index=0) %}
            {% set schedule.index = schedule.index + 1 %}
            {% set accumulator = namespace(value=0, any=false) %}
            {% set accumulator.value = accumulator.value + row.weight %}
            {% set accumulator.any = true %}
            {{ bad_semantic | default(0) ~ model_name }}
            {{ semantic_model_name | default(0) }}
            {{ other.variable_final_flags[name] | default(false) }}
            {{ semantic_value or (0) }}
            {{ variable.binding if variable.binding is not none else variable.start }}
            {% if other.binding is not none %}
            {% else %}
              {% if other.start is not none %}fallback{% endif %}
            {% endif %}
        "#;

#[test]
fn mutation_gate_rejects_template_fallback_and_domain_expansion() {
    let findings = analyze_template_source(
        "crates/rumoca-phase-codegen/src/templates/fake/output.jinja",
        TEMPLATE_FALLBACK_FIXTURE,
    );
    for expected in [
        "template-fallback:binding-from-start",
        "template-domain-expansion:range",
        "template-semantic-operation-alias",
        "template-namespace-state-mutation",
        "template-fallback:default",
        "template-fallback:literal",
    ] {
        assert!(
            findings.iter().any(|finding| finding.contains(expected)),
            "template escape was not classified: {expected}: {findings:#?}"
        );
    }
    assert_eq!(
        findings
            .iter()
            .filter(|finding| finding.contains("template-semantic-operation-alias"))
            .count(),
        2
    );
    assert_eq!(
        findings
            .iter()
            .filter(|finding| finding.contains("template-fallback:default"))
            .count(),
        3
    );
    assert!(
        findings
            .iter()
            .filter(|finding| finding.contains("template-namespace-state-mutation"))
            .count()
            >= 3
    );
    assert!(
        findings
            .iter()
            .filter(|finding| finding.contains("template-fallback:binding-from-start"))
            .count()
            >= 3
    );

    let presentation_defaults = analyze_template_source(
        "crates/rumoca-phase-codegen/src/templates/fake/presentation.jinja",
        r#"{{ model_name | default("model") }}{{ flat.variable_final_flags[name] | default(false) }}"#,
    );
    assert_eq!(
        presentation_defaults
            .iter()
            .filter(|finding| finding.contains("template-reviewed-logic:2:"))
            .count(),
        1
    );
    assert!(
        presentation_defaults
            .iter()
            .all(|finding| !finding.contains("template-fallback:"))
    );
}
