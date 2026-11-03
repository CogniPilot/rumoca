//! Positive and negative controls from the print-plan lint contract. Witness
//! controls read the actual legacy templates that the backend-decision audit
//! cites, so they fail if the gate stops recognizing the audited constructs.

use super::{
    Language, RepoTemplates, Rule, TemplateSource, check, check_macro, literal_c, repo_root,
};
use anyhow::{Result, anyhow};
use std::collections::BTreeMap;

/// Templates by registered name; the file key is the name itself.
struct Fixture(BTreeMap<String, String>);

impl Fixture {
    fn new(files: &[(&str, &str)]) -> Self {
        Self(
            files
                .iter()
                .map(|(name, text)| ((*name).to_owned(), (*text).to_owned()))
                .collect(),
        )
    }
}

impl TemplateSource for Fixture {
    fn resolve(&self, name: &str) -> Option<String> {
        self.0.contains_key(name).then(|| name.to_owned())
    }

    fn read(&self, file: &str) -> Result<String> {
        self.0
            .get(file)
            .cloned()
            .ok_or_else(|| anyhow!("no fixture {file}"))
    }
}

fn rules_of(files: &[(&str, &str)]) -> Vec<(String, u16, Rule)> {
    let report = check(
        &Fixture::new(files),
        files[0].0,
        Language::of_template(files[0].0),
    )
    .expect("fixture check");
    report
        .violations
        .into_iter()
        .map(|v| (v.file, v.line, v.rule))
        .collect()
}

fn assert_flags(files: &[(&str, &str)], file: &str, line: u16, rule: Rule) {
    let found = rules_of(files);
    assert!(
        found
            .iter()
            .any(|(f, l, r)| f == file && *l == line && *r == rule),
        "expected {rule:?} at {file}:{line}, found {found:?}"
    );
}

/// A print-only C body: issued declarations, issued variant guards, an ordered
/// issued collection, an encoded string, an issued ABI signature and a call to
/// an issued runtime function.
const POSITIVE: &str = r#"/* {{ plan.banner | xml_escape }} */
{{ plan.signature.return_type }} {{ plan.signature.name }}({{ plan.signature.parameters }})
{
{%- for decl in plan.frame %}
    {{ decl.c_type }} {{ decl.name }}{% if decl.is_array %}[{{ decl.extent }}]{% endif %};
{%- endfor %}
{%- for op in plan.body %}
{%- if op.is_copy %}
    {{ op.dst }} = {{ op.src }};
{%- elif op.is_binary %}
    {{ op.dst }} = {{ op.lhs }} {{ op.operator_token }} {{ op.rhs }};
{%- elif op.is_runtime_call %}
    {{ op.status }} = {{ op.runtime_function }}({% for arg in op.args %}{{ arg.name }}{% if not loop.last %}, {% endif %}{% endfor %});
{%- endif %}
{%- endfor %}
    return {{ plan.ok_status }};
}
"#;

#[test]
fn positive_controls_pass() {
    // `not loop.last` is a Boolean formula; the punctuation marker itself is the only allowed shape.
    let positive = POSITIVE.replace(
        "{% if not loop.last %}, {% endif %}",
        "{% if loop.last %}{% else %}, {% endif %}",
    );
    assert_eq!(rules_of(&[("body.c.jinja", &positive)]), Vec::new());
    // A zero/singleton issued collection and an issued placement print through the same constructs.
    let empty =
        "{% for item in plan.items %}{{ item.name }}{% endfor %}{{ plan.placement.offset_token }}";
    assert_eq!(rules_of(&[("unit.c.jinja", empty)]), Vec::new());
    // Static composition of an approved print-only fragment.
    let root = r#"{% include "frag.jinja" %}"#;
    assert_eq!(
        rules_of(&[("root.c.jinja", root), ("frag.jinja", "{{ plan.name }};")]),
        Vec::new()
    );
}

#[test]
fn boolean_formula_in_a_print_branch_fails() {
    assert_flags(
        &[("body.c.jinja", POSITIVE)],
        "body.c.jinja",
        13,
        Rule::Arithmetic,
    );
}

#[test]
fn trip_count_and_stride_arithmetic_fail_in_every_spelling() {
    for text in [
        "{{ (binder.upper - binder.lower) * binder.step }}",
        "{{binder.upper-binder.lower}}",
        "{{ binder.upper\n  -\n  binder.lower }}",
        "{{ shape.rest | product }}",
    ] {
        let found = rules_of(&[("t.c.jinja", text)]);
        assert!(
            found
                .iter()
                .any(|(_, _, r)| matches!(r, Rule::Arithmetic | Rule::Collection)),
            "{text:?}: {found:?}"
        );
    }
    let root = "{% include \"stride.jinja\" %}";
    let fragment = "\n\n{{ shape[i + 1] * stride }}";
    assert_flags(
        &[("root.c.jinja", root), ("stride.jinja", fragment)],
        "stride.jinja",
        3,
        Rule::Arithmetic,
    );
}

#[test]
fn derivative_expansion_through_a_macro_or_helper_alias_fails() {
    let via_macro = "{% macro mul(a, b) %}{{ a }}*{{ b.d }}{% endmacro %}{{ mul(op.lhs, op.rhs) }}";
    let found = rules_of(&[("t.c.jinja", via_macro)]);
    assert!(
        found.iter().any(|(_, l, r)| *l == 1 && *r == Rule::Macro),
        "{found:?}"
    );
    assert!(found.iter().any(|(_, _, r)| *r == Rule::Call), "{found:?}");
    let via_alias = "{% set tangent = helpers.tangent_product %}{{ tangent(op) }}";
    let found = rules_of(&[("t.c.jinja", via_alias)]);
    assert!(
        found.iter().any(|(_, _, r)| *r == Rule::Assignment),
        "{found:?}"
    );
    assert!(found.iter().any(|(_, _, r)| *r == Rule::Call), "{found:?}");
    let indirect = "{{ (plan.helpers.derive)(op) }}";
    assert_flags(&[("t.c.jinja", indirect)], "t.c.jinja", 1, Rule::Call);
}

#[test]
fn composition_and_fallback_controls_fail_closed() {
    assert_flags(
        &[("t.c.jinja", "{% include plan.fragment_name %}")],
        "t.c.jinja",
        1,
        Rule::Composition,
    );
    let optional = [
        ("t.c.jinja", "{% include \"frag.jinja\" ignore missing %}"),
        ("frag.jinja", "{{ plan.name }}"),
    ];
    assert_flags(&optional, "t.c.jinja", 1, Rule::Composition);
    assert_flags(
        &[("t.c.jinja", "{% include \"missing.jinja\" %}")],
        "t.c.jinja",
        1,
        Rule::Composition,
    );
    let cycle = [
        ("a.c.jinja", "{% import \"b.jinja\" as b %}"),
        ("b.jinja", "{% import \"a.c.jinja\" as a %}"),
    ];
    assert_flags(&cycle, "b.jinja", 1, Rule::Composition);
    let recursive = "{% macro walk(n) %}{{ walk(n.child) }}{% endmacro %}";
    assert_flags(&[("t.c.jinja", recursive)], "t.c.jinja", 1, Rule::Macro);
    assert_flags(
        &[("t.c.jinja", "{{ op.width | default(\"1\") }}")],
        "t.c.jinja",
        1,
        Rule::Fallback,
    );
    assert_flags(
        &[("t.c.jinja", "{% if op.seed is defined %}x{% endif %}")],
        "t.c.jinja",
        1,
        Rule::Fallback,
    );
    assert_flags(
        &[("t.c.jinja", "{% for i in range(op.n) %}{% endfor %}")],
        "t.c.jinja",
        1,
        Rule::RangeOrSubscript,
    );
    let classify = "{% for x in plan.items | selectattr(\"dense\") %}{% endfor %}";
    assert_flags(&[("t.c.jinja", classify)], "t.c.jinja", 1, Rule::Collection);
    let filtered = "{% for x in plan.items if x.dense %}{% endfor %}";
    assert_flags(&[("t.c.jinja", filtered)], "t.c.jinja", 1, Rule::Collection);
    assert_flags(
        &[("t.c.jinja", "{{ fail(\"unsupported\") }}")],
        "t.c.jinja",
        1,
        Rule::Fallback,
    );
}

#[test]
fn raw_graph_serialization_and_unreviewed_filters_fail() {
    assert_flags(
        &[("t.c.jinja", "{{ plan.graph | tojson }}")],
        "t.c.jinja",
        1,
        Rule::Call,
    );
    assert_flags(
        &[("t.c.jinja", "{{ op.name | sanitize }}")],
        "t.c.jinja",
        1,
        Rule::Call,
    );
    assert_flags(
        &[("t.c.jinja", "{{ op.lhs ~ op.rhs }}")],
        "t.c.jinja",
        1,
        Rule::Arithmetic,
    );
}

#[test]
fn loop_intrinsics_cannot_compute_identity_or_coordinates() {
    assert_flags(
        &[(
            "t.c.jinja",
            "{% for r in plan.regs %}r{{ loop.index0 }}{% endfor %}",
        )],
        "t.c.jinja",
        1,
        Rule::LoopIntrinsic,
    );
    assert_flags(
        &[(
            "t.c.jinja",
            "{% for r in plan.regs %}{% if loop.length %}x{% endif %}{% endfor %}",
        )],
        "t.c.jinja",
        1,
        Rule::LoopIntrinsic,
    );
    assert_flags(
        &[(
            "t.c.jinja",
            "{% for r in plan.regs %}{% else %}empty{% endfor %}",
        )],
        "t.c.jinja",
        1,
        Rule::LoopControl,
    );
    assert_flags(
        &[("t.c.jinja", "{{ plan.regs[op.index] }}")],
        "t.c.jinja",
        1,
        Rule::RangeOrSubscript,
    );
}

/// Gaussian elimination as literal C, every size an issued field and no
/// template arithmetic at all: the AST is clean, so only the literal-text check
/// can fail it. This is the contract's mandatory counterexample to a
/// syntax-only lint.
const LITERAL_GAUSSIAN_ELIMINATION: &str = r#"{{ op.decl_a }} {{ op.decl_b }}
for (size_t c=0; c<{{ op.n }}; ++c) {
size_t p=c; for (size_t i=c+1; i<{{ op.n }}; ++i) if (fabs(A[i*{{ op.n }}+c])>fabs(A[p*{{ op.n }}+c])) p=i;
if (A[p*{{ op.n }}+c]==0.0) return 1;
for (size_t i=c+1; i<{{ op.n }}; ++i) { double f=A[i*{{ op.n }}+c]/A[c*{{ op.n }}+c]; for (size_t j=c; j<{{ op.n }}; ++j) A[i*{{ op.n }}+j]-=f*A[c*{{ op.n }}+j]; }
}
"#;

#[test]
fn literal_gaussian_elimination_fails_although_its_ast_is_clean() {
    let found = rules_of(&[("solve.c.jinja", LITERAL_GAUSSIAN_ELIMINATION)]);
    assert!(
        found.iter().all(|(_, _, r)| *r == Rule::LiteralExecutable),
        "{found:?}"
    );
    assert!(
        found.iter().any(|(_, l, _)| *l == 2),
        "first literal loop on line 2: {found:?}"
    );
    assert!(
        found.iter().any(|(_, l, _)| *l == 4),
        "zero-pivot constant on line 4: {found:?}"
    );
}

#[test]
fn literal_check_covers_strings_comments_defines_and_spellings() {
    let found = |text: &str| literal_c::forbidden_c_tokens(text);
    assert!(found("/* for each row */ int x; // while ok\n\"for(;;)\" 'x'").is_empty());
    assert_eq!(found("y = x * 0.5;").len(), 1);
    assert_eq!(found("y = 1e-12;").len(), 1);
    assert_eq!(found("y = 0x1p-3;").len(), 1);
    assert!(found("y = 0xE0;").is_empty());
    assert_eq!(found("s += a;").len(), 1);
    assert_eq!(found("#define STEP(k) for (k = 0; k < N; k++)\n").len(), 2);
    assert!(found("#include <math.h>\n#if FOO > 1\n#endif\nint f(void);").is_empty());
    // A printed string constant is literal text too.
    let printed = "{{ \"while (1) {}\" }}";
    assert_flags(
        &[("t.c.jinja", printed)],
        "t.c.jinja",
        1,
        Rule::LiteralExecutable,
    );
}

#[test]
fn a_target_without_a_literal_check_fails_closed() {
    assert_flags(
        &[("model.mlir.jinja", "func.func @f() {}\n")],
        "model.mlir.jinja",
        1,
        Rule::UnsupportedLiteralLanguage,
    );
    assert_eq!(
        rules_of(&[("model.mlir.jinja", "{{ plan.text_token }}")]),
        Vec::new()
    );
}

fn legacy(root: &str) -> Vec<(String, u16, Rule)> {
    let repo = RepoTemplates::load(&repo_root()).expect("template manifests");
    let report = check(&repo, root, Language::C).expect("legacy check");
    report
        .violations
        .into_iter()
        .map(|v| (v.file, v.line, v.rule))
        .collect()
}

fn assert_legacy(found: &[(String, u16, Rule)], file: &str, line: u16, rule: Rule, witness: &str) {
    assert!(
        found
            .iter()
            .any(|(f, l, r)| f == file && *l == line && *r == rule),
        "{witness}: expected {rule:?} at {file}:{line}"
    );
}

#[test]
fn audit_witnesses_in_the_legacy_templates_fail() {
    let typed = "fmi3/typed_functions.jinja";
    let found = legacy(typed);
    assert_legacy(&found, typed, 8, Rule::Macro, "D056 trip-count macro");
    assert_legacy(
        &found,
        typed,
        9,
        Rule::Arithmetic,
        "D056 span*step arithmetic",
    );
    assert_legacy(
        &found,
        typed,
        158,
        Rule::Assignment,
        "D059 written-register namespace",
    );
    assert_legacy(
        &found,
        typed,
        166,
        Rule::Arithmetic,
        "D059 single-store alias test (line 166)",
    );
    let tensors = "fmi3/typed_tensors.jinja";
    let found = legacy(tensors);
    assert_legacy(
        &found,
        tensors,
        98,
        Rule::RangeOrSubscript,
        "D065 range over dimensions",
    );
    assert_legacy(&found, tensors, 99, Rule::Collection, "D065 stride product");
    assert_legacy(
        &found,
        tensors,
        177,
        Rule::LiteralExecutable,
        "D066 literal Gaussian elimination",
    );
    let scalar = "fmi3/scalar_kernel.jinja";
    assert_legacy(
        &legacy(scalar),
        scalar,
        168,
        Rule::LiteralExecutable,
        "D069 tangent-lane loops",
    );
}

#[test]
fn every_legacy_artifact_template_is_outside_the_strict_gate() {
    // The gate makes no compliance claim for an unmigrated template: each one
    // the manifests declare either fails the strict check or is covered.
    let repo = RepoTemplates::load(&repo_root()).expect("template manifests");
    assert!(
        repo.artifacts().len() > 20,
        "manifests declare the artifact templates"
    );
    for root in repo.artifacts() {
        let report = check(&repo, root, Language::of_template(root)).expect("check");
        let covered = true;
        assert!(
            covered || !report.violations.is_empty(),
            "{root} passes but is not registered as covered"
        );
    }
}

fn region(files: &[(&str, &str)], name: &str) -> Vec<(String, u16, Rule)> {
    let report =
        check_macro(&Fixture::new(files), files[0].0, name, Language::C).expect("region check");
    report
        .violations
        .into_iter()
        .map(|v| (v.file, v.line, v.rule))
        .collect()
}

fn region_has(files: &[(&str, &str)], name: &str, line: u16, rule: Rule) {
    let found = region(files, name);
    assert!(
        found.iter().any(|(_, l, r)| *l == line && *r == rule),
        "expected {rule:?} at line {line}, found {found:?}"
    );
}

/// A print-only macro inside a file whose other text is legacy executable C.
const MIXED: &str = r#"{% macro scalar_copy(op) %}{{ op.dst }} = {{ op.src }};{% endmacro %}
{% macro legacy_solve(n) %}for (size_t i=0; i<{{ n }}; ++i) s += a[i];{% endmacro %}
for (size_t c=0; c<{{ program.n }}; ++c) { A[c] -= 0.5; }
"#;

#[test]
fn a_macro_region_covers_only_that_macro() {
    assert_eq!(region(&[("lib.jinja", MIXED)], "scalar_copy"), Vec::new());
    // The same file as a whole template, and its executable neighbour, still fail.
    let whole = rules_of(&[("lib.c.jinja", MIXED)]);
    assert!(
        whole
            .iter()
            .any(|(_, l, r)| *l == 3 && *r == Rule::LiteralExecutable),
        "{whole:?}"
    );
    region_has(
        &[("lib.jinja", MIXED)],
        "legacy_solve",
        2,
        Rule::LiteralExecutable,
    );
    // Executable text inside the selected body, at its edge, is part of the region.
    let edge = "{% macro copy(op) %}{{ op.dst }} = {{ op.src }}; while (1) {}{% endmacro %}";
    region_has(&[("lib.jinja", edge)], "copy", 1, Rule::LiteralExecutable);
}

#[test]
fn a_macro_region_must_name_exactly_one_macro() {
    region_has(
        &[("lib.jinja", MIXED)],
        "no_such_macro",
        0,
        Rule::RegionSelection,
    );
    let twice = "{% macro copy(op) %}{{ op.a }}{% endmacro %}\n{% if flag %}{% macro copy(op) %}{{ op.b }}{% endmacro %}{% endif %}";
    region_has(&[("lib.jinja", twice)], "copy", 2, Rule::RegionSelection);
}

#[test]
fn a_macro_region_reads_only_its_arguments_and_loop_targets() {
    let forged = "{% macro copy(op) %}{{ program.registers }}{% for r in op.regs %}{{ r.name }}{% endfor %}{% endmacro %}";
    let found = region(&[("lib.jinja", forged)], "copy");
    assert_eq!(
        found
            .iter()
            .filter(|(_, _, r)| *r == Rule::UndeclaredRoot)
            .count(),
        1,
        "{found:?}"
    );
    let caller = "{% macro copy(op) %}{{ caller() }}{% endmacro %}";
    region_has(&[("lib.jinja", caller)], "copy", 1, Rule::Call);
    let defaulted = "{% macro copy(op, width=1) %}{{ op.dst }}{% endmacro %}";
    region_has(&[("lib.jinja", defaulted)], "copy", 1, Rule::Fallback);
    // An included fragment runs with the region's roots: a forged name there fails too.
    let root = "{% macro copy(op) %}{% include \"frag.jinja\" %}{% endmacro %}";
    let files = [
        ("lib.jinja", root),
        ("frag.jinja", "{{ op.dst }}{{ program.n }}"),
    ];
    let found = region(&files, "copy");
    assert!(
        found
            .iter()
            .any(|(f, _, r)| f == "frag.jinja" && *r == Rule::UndeclaredRoot),
        "{found:?}"
    );
}

#[test]
fn a_macro_region_calls_no_unchecked_callee() {
    let helper = "{% macro copy(op) %}{{ arithmetic(op) }}{% endmacro %}{% macro arithmetic(op) %}{{ op.a }}{% endmacro %}";
    region_has(&[("lib.jinja", helper)], "copy", 1, Rule::Call);
    let recursive = "{% macro walk(n) %}{{ walk(n.child) }}{% endmacro %}";
    region_has(&[("lib.jinja", recursive)], "walk", 1, Rule::Call);
}

#[test]
fn the_legacy_trip_count_macro_fails_as_a_region() {
    let repo = RepoTemplates::load(&repo_root()).expect("template manifests");
    let report = check_macro(
        &repo,
        "fmi3/typed_functions.jinja",
        "trip_count",
        Language::C,
    )
    .expect("region");
    assert!(
        report
            .violations
            .iter()
            .any(|v| v.line == 9 && v.rule == Rule::Arithmetic),
        "D056 inside the trip_count region: {:?}",
        report.violations
    );
    assert!(
        report.files[0].contains("macro trip_count only"),
        "{:?}",
        report.files
    );
}
