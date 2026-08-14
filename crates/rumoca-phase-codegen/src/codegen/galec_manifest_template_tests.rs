//! Architecture checks for the checked Algorithm Code template surface.

use super::{create_environment, xs_double_str};
use crate::templates;
use rumoca_ir_galec::ast as galec;
use rumoca_ir_galec::package::CheckedAlgorithmBlock;
use serde_json::json;

#[test]
fn galec_templates_parse_in_the_strict_shared_environment() {
    let mut env = create_environment();
    for target in ["galec", "galec-production", "embedded-c-galec"] {
        let bundle = templates::builtin_target(target).expect("built-in GALEC target");
        for template in bundle.templates {
            env.add_template_owned(
                format!("{target}/{}", template.path),
                template.source.to_owned(),
            )
            .unwrap_or_else(|error| panic!("{target}/{}: {error}", template.path));
        }
    }
}

#[test]
fn galec_templates_consume_checked_algorithm_code_and_artifact_facts() {
    for target in ["galec", "galec-production", "embedded-c-galec"] {
        let bundle = templates::builtin_target(target).expect("built-in GALEC target");
        for template in bundle.templates {
            assert!(
                !template.source.contains("ctx."),
                "{target}/{} retains the removed dynamic manifest context",
                template.path
            );
            assert!(
                !template.source.contains("galec_alg_source")
                    && !template.source.contains("galec_c_source")
                    && !template.source.contains("galec_c_header"),
                "{target}/{} retains a pre-rendered target-language passthrough",
                template.path
            );
        }
    }
}

fn is_conformant_real_literal(text: &str) -> bool {
    let text = text.strip_prefix('-').unwrap_or(text);
    let Some(decimal_index) = text.find('.') else {
        return false;
    };
    let (integer, fraction_with_separator) = text.split_at(decimal_index);
    let fraction = &fraction_with_separator[1..];
    if integer.is_empty()
        || !integer.bytes().all(|byte| byte.is_ascii_digit())
        || integer.starts_with('0') && integer.len() != 1
    {
        return false;
    }
    let (fraction, exponent) = fraction.split_once('e').unwrap_or((fraction, ""));
    if fraction.is_empty() || !fraction.bytes().all(|byte| byte.is_ascii_digit()) {
        return false;
    }
    exponent.is_empty()
        || exponent.strip_prefix(['+', '-']).is_some_and(|digits| {
            !digits.is_empty() && digits.bytes().all(|byte| byte.is_ascii_digit())
        })
}

#[test]
fn portable_real_filter_preserves_expected_galec_spellings() {
    for (value, expected) in [
        (0.0, "0.0"),
        (-0.0, "-0.0"),
        (0.5, "0.5"),
        (-2.5, "-2.5"),
        (100_000.0, "100000.0"),
        (0.000_001, "0.000001"),
        (1.0e300, "1.0e+300"),
        (-1.5e300, "-1.5e+300"),
        (1.0e-300, "1.0e-300"),
        (1.0e21, "1.0e+21"),
    ] {
        assert_eq!(xs_double_str(value).unwrap(), expected);
    }
}

#[test]
fn portable_real_filter_is_conformant_and_round_trips() {
    for value in [
        0.0,
        -0.0,
        1.0,
        -1.0,
        0.1 + 0.2,
        std::f64::consts::PI,
        1.0e-42,
        -3.25e17,
        f64::MAX,
        f64::MIN_POSITIVE,
        5e-324,
    ] {
        let rendered = xs_double_str(value).unwrap();
        assert!(is_conformant_real_literal(&rendered), "{rendered}");
        assert_eq!(rendered.parse::<f64>().unwrap(), value);
    }
}

#[test]
fn portable_real_filter_rejects_non_finite_values() {
    for value in [f64::NAN, f64::INFINITY, f64::NEG_INFINITY] {
        assert!(xs_double_str(value).is_err());
    }
}

fn collision_block() -> CheckedAlgorithmBlock {
    let input = |name| galec::InterfaceVariable {
        kind: galec::InterfaceKind::Input,
        decl: galec::VariableDeclaration::scalar(galec::ScalarType::Real, name),
        start: None,
    };
    let mut block = galec::Block::new(galec::Name::ident("constexpr"));
    block.interface = vec![
        input(galec::Name::quoted("a.b")),
        input(galec::Name::ident("a_b")),
        input(galec::Name::ident("volatile")),
        input(galec::Name::ident("imu_valid")),
    ];
    block.do_step.locals = vec![galec::VariableDeclaration::scalar(
        galec::ScalarType::Real,
        galec::Name::quoted("imu.valid"),
    )];
    CheckedAlgorithmBlock::construct(block).expect("valid collision fixture")
}

fn artifact_facts() -> serde_json::Value {
    json!({
        "generated_at": "2026-01-01T00:00:00Z",
        "generation_tool": "rumoca-test",
        "identities": {
            "pc_manifest": "10000000-0000-0000-0000-000000000001",
            "ac_manifest": "10000000-0000-0000-0000-000000000002"
        },
        "checksums": {
            "ac_manifest_sha1": "0000000000000000000000000000000000000000",
            "c_header_sha1": "0000000000000000000000000000000000000000",
            "c_source_sha1": "0000000000000000000000000000000000000000",
            "c_kernels_header_sha1": "0000000000000000000000000000000000000000",
            "c_kernels_source_sha1": "0000000000000000000000000000000000000000",
            "c_format_sha1": "0000000000000000000000000000000000000000"
        }
    })
}

fn render_block_fixture(block: &CheckedAlgorithmBlock, template: &str) -> String {
    crate::render_checked_algorithm_block_template_with_artifact(
        block,
        &artifact_facts(),
        template,
        "fixture",
    )
    .expect("render collision fixture")
}

fn render_fixture(template: &str) -> String {
    render_block_fixture(&collision_block(), template)
}

#[test]
fn galec_c_symbols_are_collision_safe_reserved_disjoint_and_consistent() {
    let header = render_fixture(
        templates::builtin_template_source("embedded-c-galec", "model.h.jinja")
            .expect("C header template"),
    );
    let source = render_fixture(
        templates::builtin_template_source("embedded-c-galec", "model.c.jinja")
            .expect("C source template"),
    );
    let manifest = render_fixture(
        templates::builtin_template_source("galec-production", "pc_manifest.xml.jinja")
            .expect("Production Code manifest template"),
    );

    assert_eq!(header.matches("float b; /* declaration 1: a.b").count(), 1);
    assert_eq!(
        header.matches("float a_b; /* declaration 2: a_b").count(),
        1
    );
    assert_eq!(
        header
            .matches("float volatile_2; /* declaration 3: volatile")
            .count(),
        1
    );
    assert_eq!(
        header
            .matches("float imu_valid; /* declaration 4: imu_valid")
            .count(),
        1,
        "a function-local collision must not rename a state field: {header}"
    );
    assert!(!header.contains("float imu_valid_2;"), "{header}");
    assert!(header.contains("constexpr_2State"), "{header}");
    assert!(source.contains("void constexpr_2_startup"), "{source}");
    assert!(manifest.contains("name=\"constexpr_2State\""), "{manifest}");
    assert!(
        manifest.contains("<Component id=\"CO_1\" name=\"b\" typeDefRefId=\"TD_F32\""),
        "{manifest}"
    );
    assert!(
        manifest.contains("<Component id=\"CO_2\" name=\"a_b\" typeDefRefId=\"TD_F32\""),
        "{manifest}"
    );
    assert!(
        manifest.contains("<Component id=\"CO_3\" name=\"volatile_2\" typeDefRefId=\"TD_F32\""),
        "{manifest}"
    );
    assert!(
        manifest.contains("componentIdentifier=\"a_b\""),
        "{manifest}"
    );
    assert!(manifest.contains("componentIdentifier=\"b\""), "{manifest}");
    assert!(
        manifest.contains("componentIdentifier=\"volatile_2\""),
        "{manifest}"
    );
    assert!(
        manifest.contains("componentIdentifier=\"imu_valid\""),
        "{manifest}"
    );
    assert!(
        manifest.contains("id=\"TT_U32\" kind=\"efmiUnsignedInteger32\" codedType=\"uint32_t\""),
        "{manifest}"
    );
    assert!(
        manifest.contains(
            "id=\"CO_ERROR_SIGNAL_STATUS\" name=\"rumoca_galec_error_signal_status\" typeDefRefId=\"TD_U32\""
        ),
        "{manifest}"
    );
    assert!(manifest.contains("foreignRefId=\"ESS\""), "{manifest}");
    assert!(
        manifest.contains("componentIdentifier=\"rumoca_galec_error_signal_status\""),
        "{manifest}"
    );
}

/// The loop counter that `array_assignment` in `model.c.jinja` emits for the
/// outermost copy dimension. The template writes this identifier itself and
/// never allocates it, so no user symbol may ever be spelled like it.
const GENERATED_COPY_ITERATOR: &str = "rumoca_galec_copy_0";

/// A block whose user names are deliberately spelled inside the generator's
/// private `rumoca_galec_` namespace: one method local named exactly like the
/// emitted array-copy loop counter, and one state variable in the same
/// namespace (header field + Production Code manifest component).
fn generated_prefix_block() -> CheckedAlgorithmBlock {
    fn real_array(name: &str) -> galec::VariableDeclaration {
        let mut declaration =
            galec::VariableDeclaration::scalar(galec::ScalarType::Real, galec::Name::ident(name));
        declaration.dimensions = vec![galec::Dimension::Expr(galec::Expression::Integer(2))];
        declaration
    }
    fn input(decl: galec::VariableDeclaration) -> galec::InterfaceVariable {
        galec::InterfaceVariable {
            kind: galec::InterfaceKind::Input,
            decl,
            start: None,
        }
    }
    fn local_ref(name: &str) -> galec::Reference {
        galec::Reference::local(galec::Name::ident(name))
    }
    fn state_ref(name: &str) -> galec::Reference {
        galec::Reference::state(galec::Name::ident(name))
    }

    let mut block = galec::Block::new(galec::Name::ident("PrefixCollision"));
    block.interface = vec![
        input(real_array("source")),
        input(galec::VariableDeclaration::scalar(
            galec::ScalarType::Real,
            galec::Name::ident("rumoca_galec_gain"),
        )),
        galec::InterfaceVariable {
            kind: galec::InterfaceKind::Output,
            decl: real_array("target"),
            start: None,
        },
    ];
    block.do_step.locals = vec![galec::VariableDeclaration::scalar(
        galec::ScalarType::Real,
        galec::Name::ident(GENERATED_COPY_ITERATOR),
    )];
    block.do_step.statements = vec![
        galec::Spanned::dummy(galec::Statement::Assignment {
            target: local_ref(GENERATED_COPY_ITERATOR),
            value: galec::Expression::Ref(state_ref("rumoca_galec_gain")),
        }),
        galec::Spanned::dummy(galec::Statement::Assignment {
            target: state_ref("target"),
            value: galec::Expression::binary(
                galec::BinaryOp::Sub,
                galec::Expression::Ref(state_ref("source")),
                galec::Expression::Ref(local_ref(GENERATED_COPY_ITERATOR)),
            ),
        }),
    ];
    CheckedAlgorithmBlock::construct(block).expect("valid generated-prefix fixture")
}

#[test]
fn galec_c_symbols_never_take_a_generated_prefix_spelling() {
    let block = generated_prefix_block();
    let header = render_block_fixture(
        &block,
        templates::builtin_template_source("embedded-c-galec", "model.h.jinja")
            .expect("C header template"),
    );
    let source = render_block_fixture(
        &block,
        templates::builtin_template_source("embedded-c-galec", "model.c.jinja")
            .expect("C source template"),
    );
    let manifest = render_block_fixture(
        &block,
        templates::builtin_template_source("galec-production", "pc_manifest.xml.jinja")
            .expect("Production Code manifest template"),
    );

    assert!(
        source.contains(&format!(
            "for (int32_t {GENERATED_COPY_ITERATOR} = INT32_C(0);"
        )),
        "the array copy must still emit its own loop counter: {source}"
    );
    assert!(
        !source.contains(&format!("float {GENERATED_COPY_ITERATOR};")),
        "a user local spelled like the emitted loop counter must be renamed, \
         otherwise the loop index silently replaces it inside the copy: {source}"
    );
    assert!(source.contains("float copy_0;"), "{source}");
    assert!(
        source.contains(&format!(
            "self->target[{GENERATED_COPY_ITERATOR}] = \
             (self->source[{GENERATED_COPY_ITERATOR}] - copy_0);"
        )),
        "the copy body must read the renamed local, not the loop index: {source}"
    );
    assert!(source.contains("copy_0 = self->gain;"), "{source}");
    assert!(
        !source.contains("rumoca_galec_gain"),
        "no emitted C symbol may live in the generated namespace: {source}"
    );

    assert!(
        header.contains("float gain; /* declaration 2: rumoca_galec_gain"),
        "the state field must be renamed out of the generated namespace, and the \
         source spelling must survive only in the declaration comment: {header}"
    );
    assert!(!header.contains("float rumoca_galec_gain;"), "{header}");
    assert!(
        manifest.contains("<Component id=\"CO_2\" name=\"gain\" typeDefRefId=\"TD_F32\""),
        "{manifest}"
    );
    assert!(
        manifest.contains("componentIdentifier=\"gain\""),
        "the manifest must name the same field the header declares: {manifest}"
    );
    assert!(!manifest.contains("rumoca_galec_gain"), "{manifest}");
}

/// A block whose names force the numbered fallback to count from a base whose
/// every suffixed spelling would land inside the generated namespace.
///
/// `rumoca_galec` is not itself spelled inside `rumoca_galec_` (it lacks the
/// trailing separator), so it is allocatable, but `rumoca_galec_2`,
/// `rumoca_galec_3`, ... all are. The quoted `"rumoca.galec"` reference is
/// what reaches the fallback: both of its candidates (`galec`, `rumoca_galec`)
/// are taken by the two plain inputs.
fn generated_prefix_stem_block() -> CheckedAlgorithmBlock {
    let input = |name| galec::InterfaceVariable {
        kind: galec::InterfaceKind::Input,
        decl: galec::VariableDeclaration::scalar(galec::ScalarType::Real, name),
        start: None,
    };
    let mut block = galec::Block::new(galec::Name::ident("StemCollision"));
    block.interface = vec![
        input(galec::Name::ident("galec")),
        input(galec::Name::ident("rumoca_galec")),
        input(galec::Name::quoted("rumoca.galec")),
    ];
    CheckedAlgorithmBlock::construct(block).expect("valid generated-prefix-stem fixture")
}

/// Regression for a non-terminating allocation: the numbered fallback must
/// never count from a base whose whole `{base}_{idx}` family is rejected by
/// the generated-prefix rule.
#[test]
fn galec_c_symbols_terminate_when_the_fallback_stem_is_a_generated_prefix() {
    let header = render_block_fixture(
        &generated_prefix_stem_block(),
        template_source("embedded-c-galec", "model.h.jinja"),
    );

    assert!(
        header.contains("float galec; /* declaration 1: galec"),
        "{header}"
    );
    assert!(
        header.contains("float rumoca_galec; /* declaration 2: rumoca_galec"),
        "a name that only borders the generated namespace stays as written: {header}"
    );
    assert!(
        header.contains("float _rumoca_galec; /* declaration 3: rumoca.galec"),
        "the fallback must escape the base whose whole numbered family lands \
         inside the generated namespace: {header}"
    );
    let fields = header
        .lines()
        .filter_map(|line| line.trim().strip_prefix("float "))
        .filter_map(|line| line.split(';').next())
        .map(str::to_owned)
        .collect::<Vec<_>>();
    assert_eq!(fields.len(), 3, "{header}");
    assert!(
        !fields[2].starts_with("rumoca_galec_"),
        "the fallback spelling must stay outside the generated namespace: {header}"
    );
    let mut unique = fields.clone();
    unique.sort();
    unique.dedup();
    assert_eq!(unique.len(), fields.len(), "{header}");
}

/// Regression for compat stealing: a reference spelled inside the generated
/// namespace falls back to its stripped spelling, and that stripped spelling
/// must not be taken away from a legitimate reference spelled exactly that
/// way. Both references here are equally short and unqualified, so only the
/// deferral of prefix-spelled references keeps `x` with the variable named
/// `x`; ordered by name alone, `rumoca_galec_x` would strip to `x` first.
#[test]
fn galec_c_symbols_let_legitimate_names_keep_their_spelling() {
    let input = |name| galec::InterfaceVariable {
        kind: galec::InterfaceKind::Input,
        decl: galec::VariableDeclaration::scalar(galec::ScalarType::Real, name),
        start: None,
    };
    let mut block = galec::Block::new(galec::Name::ident("StealCollision"));
    block.interface = vec![
        input(galec::Name::ident("rumoca_galec_x")),
        input(galec::Name::ident("x")),
    ];
    let block = CheckedAlgorithmBlock::construct(block).expect("valid stealing fixture");
    let header = render_block_fixture(&block, template_source("embedded-c-galec", "model.h.jinja"));

    assert!(
        header.contains("float x_2; /* declaration 1: rumoca_galec_x"),
        "the prefix-spelled reference must yield the stripped spelling: {header}"
    );
    assert!(
        header.contains("float x; /* declaration 2: x"),
        "a name that never collides must not be renamed: {header}"
    );

    // The benign order (no legitimate claimant) is unchanged: the stripped
    // spelling is still the first choice.
    let mut benign = galec::Block::new(galec::Name::ident("BenignCollision"));
    benign.interface = vec![
        input(galec::Name::ident("rumoca_galec_x")),
        input(galec::Name::ident("y")),
    ];
    let benign = CheckedAlgorithmBlock::construct(benign).expect("valid benign fixture");
    let header = render_block_fixture(
        &benign,
        template_source("embedded-c-galec", "model.h.jinja"),
    );
    assert!(
        header.contains("float x; /* declaration 1: rumoca_galec_x"),
        "{header}"
    );
    assert!(header.contains("float y; /* declaration 2: y"), "{header}");
}

/// Every identifier the `rumoca_galec_` helper bodies declare themselves — the
/// only identifiers the templates emit outside the generated namespace — must
/// be withheld from the table that can produce a file-scope C symbol, which
/// the helper local would otherwise shadow under `-Wshadow -Werror`. Struct
/// members shadow nothing, so `state_symbols` keeps them available.
#[test]
fn galec_c_policy_reserves_the_helper_local_identifiers() {
    let policy = template_source("embedded-c-galec", "symbols.jinja");
    let source = template_source("embedded-c-galec", "model.c.jinja");
    let withheld = policy
        .split_once("{%- set helper_locals = [")
        .and_then(|(_, rest)| rest.split_once("] -%}"))
        .map(|(list, _)| list)
        .unwrap_or_else(|| panic!("the shared policy must declare helper_locals: {policy}"));
    for local in [
        "result", "value", "extent", "lhs", "rhs", "u1", "u2", "status",
    ] {
        assert!(
            withheld.contains(&format!("\"{local}\"")),
            "the shared policy must withhold the helper local `{local}`: {policy}"
        );
        assert!(
            ["float ", "int32_t ", "uint32_t *", "bool "]
                .iter()
                .any(|ty| source.contains(&format!("{ty}{local}"))),
            "`{local}` is withheld but no helper body declares it: {source}"
        );
    }
    assert!(
        policy.contains("\"reserved\": reserved + helper_locals")
            && policy.contains("\"reserved\": reserved,"),
        "only the file-scope-capable table may take the helper locals: {policy}"
    );

    let input = |name| galec::InterfaceVariable {
        kind: galec::InterfaceKind::Input,
        decl: galec::VariableDeclaration::scalar(galec::ScalarType::Real, name),
        start: None,
    };
    let mut block = galec::Block::new(galec::Name::ident("HelperLocals"));
    block.interface = vec![input(galec::Name::ident("result"))];
    block.do_step.locals = vec![galec::VariableDeclaration::scalar(
        galec::ScalarType::Real,
        galec::Name::ident("value"),
    )];
    block.do_step.statements = vec![galec::Spanned::dummy(galec::Statement::Assignment {
        target: galec::Reference::local(galec::Name::ident("value")),
        value: galec::Expression::Ref(galec::Reference::state(galec::Name::ident("result"))),
    })];
    let block = CheckedAlgorithmBlock::construct(block).expect("valid helper-local fixture");

    let header = render_block_fixture(&block, template_source("embedded-c-galec", "model.h.jinja"));
    assert!(
        header.contains("float result; /* declaration 1: result"),
        "a struct member is in its own C namespace and must not be renamed: {header}"
    );
    let source = render_block_fixture(&block, template_source("embedded-c-galec", "model.c.jinja"));
    assert!(
        source.contains("float value_2;") && !source.contains("float value;"),
        "a symbol from the file-scope-capable table must yield to the helper \
         local spelling: {source}"
    );
}

/// `x ^ 2` on a Real base prints as `(x * x)` in the emitted C, while every
/// other power keeps its `powf` call (task #54).
///
/// This is an emission choice, not a semantic one, so it is pinned here on the
/// C text rather than in the `.alg` goldens — the checked GALEC still prints
/// `x ^ 2`, which `galec_c_square_reduction_does_not_change_the_alg` asserts.
///
/// The multiply is not an approximation of the call: it is the correctly-rounded
/// square for every binary32 input, and it is what picolibc's own `powf`
/// computes for `y == 2` (a single `vmul.f32`). See `is_square_reducible` in
/// `views/algorithm_code_typed.rs`.
#[test]
fn galec_c_prints_a_real_square_as_a_multiply() {
    let source = render_square_fixture(square_of_input(galec::Expression::Integer(2)));
    assert!(
        source.contains("(self->u * self->u)"),
        "a Real base squared must print as a multiply: {source}"
    );
    assert!(
        !source.contains("powf"),
        "the squared form must not also emit a powf call: {source}"
    );
}

/// A Real exponent literal `2.0` reduces exactly like the Integer `2`.
#[test]
fn galec_c_prints_a_real_square_with_a_real_exponent_as_a_multiply() {
    let source = render_square_fixture(square_of_input(galec::Expression::Real(2.0)));
    assert!(
        source.contains("(self->u * self->u)") && !source.contains("powf"),
        "a Real 2.0 exponent must reduce like the Integer 2: {source}"
    );
}

/// Any exponent other than 2 keeps `powf` — there is no single-multiply
/// equivalent, and inventing one would change the value.
#[test]
fn galec_c_keeps_powf_for_a_non_square_exponent() {
    let source = render_square_fixture(square_of_input(galec::Expression::Integer(3)));
    assert!(
        source.contains("powf(self->u, 3)"),
        "a cube must keep its powf call: {source}"
    );
}

/// A compound base keeps `powf`: printing it as a multiply would duplicate the
/// operand's text and recompute it.
#[test]
fn galec_c_keeps_powf_for_a_compound_base() {
    let source = render_square_fixture(galec::Expression::binary(
        galec::BinaryOp::Pow,
        galec::Expression::Paren(Box::new(galec::Expression::binary(
            galec::BinaryOp::Add,
            galec::Expression::Ref(galec::Reference::state(galec::Name::ident("u"))),
            galec::Expression::Real(1.0),
        ))),
        galec::Expression::Integer(2),
    ));
    assert!(
        source.contains("powf("),
        "a compound base must keep its powf call rather than be printed twice: {source}"
    );
}

/// The reduction is a C-target emission choice and must leave the normative
/// GALEC Algorithm Code untouched: the `.alg` still prints `u ^ 2`.
#[test]
fn galec_c_square_reduction_does_not_change_the_alg() {
    let block = square_block(square_of_input(galec::Expression::Integer(2)));
    let alg = render_block_fixture(
        &block,
        templates::builtin_template_source("galec", "model.alg.jinja").expect("alg template"),
    );
    assert!(
        alg.contains("^ 2"),
        "the checked GALEC must still print the power operator: {alg}"
    );
    assert!(
        !alg.contains("self.u * self.u"),
        "a C emission choice must not reach the Algorithm Code: {alg}"
    );
}

/// `u ^ <exponent>` over the fixture's Real input.
fn square_of_input(exponent: galec::Expression) -> galec::Expression {
    galec::Expression::binary(
        galec::BinaryOp::Pow,
        galec::Expression::Ref(galec::Reference::state(galec::Name::ident("u"))),
        exponent,
    )
}

/// A block whose `DoStep` is `self.y := <value>;` over a Real input `u` and a
/// Real output `y`.
fn square_block(value: galec::Expression) -> CheckedAlgorithmBlock {
    let variable = |kind, name| galec::InterfaceVariable {
        kind,
        decl: galec::VariableDeclaration::scalar(galec::ScalarType::Real, name),
        start: None,
    };
    let mut block = galec::Block::new(galec::Name::ident("Square"));
    block.interface = vec![
        variable(galec::InterfaceKind::Input, galec::Name::ident("u")),
        variable(galec::InterfaceKind::Output, galec::Name::ident("y")),
    ];
    block.do_step.statements = vec![galec::Spanned::dummy(galec::Statement::Assignment {
        target: galec::Reference::state(galec::Name::ident("y")),
        value,
    })];
    CheckedAlgorithmBlock::construct(block).expect("valid square fixture")
}

fn render_square_fixture(value: galec::Expression) -> String {
    render_block_fixture(
        &square_block(value),
        templates::builtin_template_source("embedded-c-galec", "model.c.jinja")
            .expect("C source template"),
    )
}

#[test]
fn galec_c_templates_do_not_use_lossy_sanitization() {
    for target in ["embedded-c-galec", "galec-production"] {
        let bundle = templates::builtin_target(target).expect("built-in C target");
        for template in bundle.templates {
            assert!(
                !template.source.contains("| sanitize"),
                "{target}/{} uses lossy symbol sanitization",
                template.path
            );
        }
    }
}

// ===========================================================================
// Declared ranges (SPEC_0042 T3) decided in the target's own numeric domain.
//
// The block state a generated method saturates is `float`/`int32_t` in
// caller-allocated memory, so two things are only decidable here and not in
// the checked block: whether a declared bound is expressible at all in that
// domain, and whether a slot is determinate at the boundary being emitted.
// ===========================================================================

/// The bound spelling a Modelica `Modelica.Constants.inf` declaration reaches
/// this target as. MSL defines it as `1e60`, which is finite in the checked
/// block's `f64` and *not* representable as a `float`: rendering it produces
/// `1.0e+60f`, which the target's own compile preflight rejects
/// (`floating constant exceeds range of 'float'`).
const MODELICA_INFINITY: f64 = 1.0e60;

fn ranged_real(name: &str, min: Option<f64>, max: Option<f64>) -> galec::VariableDeclaration {
    let mut declaration =
        galec::VariableDeclaration::scalar(galec::ScalarType::Real, galec::Name::ident(name));
    declaration.range = galec::RangeAttributes {
        min: min.map(galec::Expression::Real),
        max: max.map(galec::Expression::Real),
    };
    declaration
}

fn ranged_integer(name: &str, min: Option<i64>, max: Option<i64>) -> galec::VariableDeclaration {
    let mut declaration =
        galec::VariableDeclaration::scalar(galec::ScalarType::Integer, galec::Name::ident(name));
    declaration.range = galec::RangeAttributes {
        min: min.map(galec::Expression::Integer),
        max: max.map(galec::Expression::Integer),
    };
    declaration
}

/// Assign a state variable in `Startup`, so it counts as definitely written
/// at the `Startup` return boundary.
fn seed_state(name: &str) -> galec::Spanned<galec::Statement> {
    seed_typed_state(name, galec::ScalarType::Real)
}

fn seed_typed_state(name: &str, scalar: galec::ScalarType) -> galec::Spanned<galec::Statement> {
    galec::Spanned::dummy(galec::Statement::Assignment {
        target: galec::Reference::state(galec::Name::ident(name)),
        value: match scalar {
            galec::ScalarType::Real => galec::Expression::Real(0.0),
            galec::ScalarType::Integer => galec::Expression::Integer(0),
            galec::ScalarType::Boolean => galec::Expression::Bool(false),
        },
    })
}

/// One output declaration, seeded in `Startup` unless `seed` says otherwise.
fn range_block(name: &str, declarations: Vec<(galec::VariableDeclaration, bool)>) -> String {
    try_range_block(name, declarations).unwrap_or_else(|error| panic!("render {name}: {error}"))
}

fn try_range_block(
    name: &str,
    declarations: Vec<(galec::VariableDeclaration, bool)>,
) -> Result<String, crate::CodegenError> {
    let mut block = galec::Block::new(galec::Name::ident(name));
    for (declaration, seed) in declarations {
        if seed {
            let galec::TypeRef::Primitive(scalar) = declaration.ty else {
                panic!("range fixtures declare primitives only");
            };
            block
                .startup
                .statements
                .push(seed_typed_state(declaration.name.lexeme(), scalar));
        }
        block.interface.push(galec::InterfaceVariable {
            kind: if seed {
                galec::InterfaceKind::Output
            } else {
                galec::InterfaceKind::Input
            },
            decl: declaration,
            start: None,
        });
    }
    let block = CheckedAlgorithmBlock::construct(block).expect("valid range fixture");
    crate::render_checked_algorithm_block_template_with_artifact(
        &block,
        &artifact_facts(),
        template_source("embedded-c-galec", "model.c.jinja"),
        name,
    )
}

/// The three answers, taken directly from the decision the filters expose.
/// `None` is "no clamp survives", `Err` is "fail closed", and a literal is
/// rendered from the value the target will really compare against — so the
/// emitted constant is exact in the target's domain and the compiler is left
/// no rounding decision over a saturation bound.
#[test]
fn target_numeric_domain_decides_every_declared_bound() {
    use super::{BoundSide, binary32_bound_str, int32_bound_str};

    let real = |value, side| binary32_bound_str(value, side);
    assert_eq!(real(-2.5, BoundSide::Min).unwrap().as_deref(), Some("-2.5"));
    assert_eq!(
        real(f64::from(f32::MAX), BoundSide::Max)
            .unwrap()
            .as_deref(),
        Some("3.4028234663852886e+38")
    );
    assert_eq!(
        real(f64::from(0.1_f32), BoundSide::Min).unwrap().as_deref(),
        Some("0.10000000149011612"),
        "the literal must be the binary32 value, not a spelling the compiler \
         still has to round"
    );
    for unbounded in [
        (f64::NEG_INFINITY, BoundSide::Min),
        (-MODELICA_INFINITY, BoundSide::Min),
        (f64::INFINITY, BoundSide::Max),
        (MODELICA_INFINITY, BoundSide::Max),
    ] {
        assert_eq!(
            real(unbounded.0, unbounded.1).unwrap(),
            None,
            "{unbounded:?}"
        );
    }
    for unrepresentable in [
        (MODELICA_INFINITY, BoundSide::Min),
        (-MODELICA_INFINITY, BoundSide::Max),
        (f64::NAN, BoundSide::Min),
    ] {
        assert!(
            real(unrepresentable.0, unrepresentable.1).is_err(),
            "{unrepresentable:?}"
        );
    }

    let integer = |value, side| int32_bound_str(value, side);
    assert_eq!(integer(-5, BoundSide::Min).unwrap().as_deref(), Some("-5"));
    assert_eq!(integer(i64::from(i32::MIN), BoundSide::Min).unwrap(), None);
    assert_eq!(integer(i64::from(i32::MAX), BoundSide::Max).unwrap(), None);
    assert_eq!(
        integer(i64::from(i32::MAX), BoundSide::Min)
            .unwrap()
            .as_deref(),
        Some("2147483647"),
        "the same value is violable as a low bound and must keep its clamp"
    );
    assert!(integer(i64::from(i32::MIN) - 1, BoundSide::Min).is_err());
    assert!(integer(i64::from(i32::MAX) + 1, BoundSide::Max).is_err());
}

/// A bound that no value of the target type can violate is not a weaker
/// clamp — it is no clamp. The `1e60` spelling of "unbounded" must therefore
/// leave nothing behind, not an out-of-range `float` literal.
#[test]
fn galec_c_unbounded_declared_ranges_emit_no_clamp() {
    let source = range_block(
        "Unbounded",
        vec![(
            ranged_real("y", Some(-MODELICA_INFINITY), Some(MODELICA_INFINITY)),
            true,
        )],
    );
    assert!(
        !source.contains("e+60"),
        "an unbounded declaration must not render an out-of-range float literal: {source}"
    );
    assert!(
        !source.contains("rumoca_galec_limit"),
        "a block whose every declared bound is unbounded must emit no limiter at all: {source}"
    );

    // Mixed: only the side that is representable survives, so a `min`-only
    // saturation is emitted as a `min`-only saturation.
    let source = range_block(
        "MinOnly",
        vec![(ranged_real("y", Some(-2.5), Some(MODELICA_INFINITY)), true)],
    );
    assert!(
        source.contains("if (self->y < -2.5f) {"),
        "the representable low bound must still saturate: {source}"
    );
    assert!(
        !source.contains("self->y >"),
        "the unbounded high side must emit no comparison: {source}"
    );
}

/// The other direction is not "unbounded" but "unrepresentable": a `min` at
/// `+inf` would saturate every finite value to an infinity the target cannot
/// spell as a literal. Fail closed rather than emit anything for it.
#[test]
fn galec_c_unrepresentable_real_ranges_fail_closed() {
    for (name, min, max) in [
        ("MinAtInfinity", Some(MODELICA_INFINITY), None),
        ("MaxAtInfinity", None, Some(-MODELICA_INFINITY)),
    ] {
        let error = try_range_block(name, vec![(ranged_real("y", min, max), true)])
            .expect_err("an unrepresentable bound must not render")
            .to_string();
        assert!(
            error.contains("unsupported-feature:target-real-range"),
            "{name} must fail closed with a typed unsupported-feature: {error}"
        );
    }
}

/// An integer domain has no value standing for "beyond the finite range", so
/// a declared bound outside the target's Integer domain (GAL-028) declares a
/// variable domain the target cannot execute. That fails closed on both sides.
#[test]
fn galec_c_integer_ranges_outside_the_target_domain_fail_closed() {
    for (name, min, max) in [
        ("IntegerMinTooLow", Some(-3_000_000_000_i64), None),
        ("IntegerMaxTooHigh", None, Some(3_000_000_000_i64)),
    ] {
        let error = try_range_block(name, vec![(ranged_integer("i", min, max), true)])
            .expect_err("an out-of-domain Integer bound must not render")
            .to_string();
        assert!(
            error.contains("unsupported-feature:target-integer-range"),
            "{name} must fail closed with a typed unsupported-feature: {error}"
        );
    }
}

/// A bound *at* the edge of the Integer domain is unviolable, so it emits no
/// comparison — which is also what keeps the target's strict compile preflight
/// (`-Wtype-limits`) from rejecting an always-false test.
#[test]
fn galec_c_integer_domain_edge_bounds_emit_no_clamp() {
    let source = range_block(
        "IntegerEdges",
        vec![(
            ranged_integer("i", Some(i64::from(i32::MIN)), Some(i64::from(i32::MAX))),
            true,
        )],
    );
    assert!(
        !source.contains("rumoca_galec_limit"),
        "domain-edge Integer bounds must leave no limiter behind: {source}"
    );

    let source = range_block(
        "IntegerHighOnly",
        vec![(
            ranged_integer("i", Some(i64::from(i32::MIN)), Some(5)),
            true,
        )],
    );
    assert!(
        source.contains("if (self->i > INT32_C(5)) {"),
        "the interior high bound must still saturate: {source}"
    );
    assert!(
        !source.contains("self->i <"),
        "the domain-edge low bound must emit no comparison: {source}"
    );
}

/// The `Startup` return boundary runs on caller-allocated state that nothing
/// has written yet. Saturating a declaration `Startup` never assigned — a
/// control input, say — would read indeterminate memory, so that boundary gets
/// its own limiter over exactly the definitely-written declarations.
#[test]
fn galec_c_startup_limits_only_what_startup_wrote() {
    let source = range_block(
        "StartupScope",
        vec![
            (ranged_real("u", Some(-2.0), Some(2.0)), false),
            (ranged_real("y", Some(-3.0), Some(3.0)), true),
        ],
    );
    let startup_limiter = source
        .split_once("static void rumoca_galec_limit_startup(StartupScopeState *self) {")
        .and_then(|(_, rest)| rest.split_once("\n}"))
        .map(|(body, _)| body)
        .unwrap_or_else(|| panic!("the startup boundary needs its own limiter: {source}"));
    assert!(
        startup_limiter.contains("self->y") && !startup_limiter.contains("self->u"),
        "the startup limiter must touch only what Startup wrote: {source}"
    );
    assert!(
        source.contains("static void rumoca_galec_limit_self(StartupScopeState *self) {"),
        "later boundaries still limit the whole block state: {source}"
    );
    let startup_body = source
        .split_once("void StartupScope_startup(StartupScopeState *self) {")
        .and_then(|(_, rest)| rest.split_once("\n}"))
        .map(|(body, _)| body)
        .expect("startup method body");
    assert!(
        startup_body.contains("rumoca_galec_limit_startup(self);")
            && !startup_body.contains("rumoca_galec_limit_self(self);"),
        "Startup must call the restricted limiter: {source}"
    );

    // When Startup writes every ranged declaration the two boundaries are the
    // same set, and one function serves both.
    let source = range_block(
        "StartupCoversAll",
        vec![(ranged_real("y", Some(-3.0), Some(3.0)), true)],
    );
    assert!(
        !source.contains("rumoca_galec_limit_startup"),
        "an identical startup scope must not duplicate the limiter: {source}"
    );
    assert_eq!(
        source.matches("    rumoca_galec_limit_self(self);").count(),
        3,
        "Startup return, DoStep entry and DoStep return (empty Recalibrate \
         shares one boundary call): {source}"
    );
}

/// A conditional write is not a write: one arm of the `if` leaves the slot
/// indeterminate, so it stays out of the `Startup` scope.
#[test]
fn galec_c_startup_scope_excludes_conditionally_written_state() {
    let mut block = galec::Block::new(galec::Name::ident("ConditionalSeed"));
    block.interface = vec![
        galec::InterfaceVariable {
            kind: galec::InterfaceKind::Input,
            decl: galec::VariableDeclaration::scalar(
                galec::ScalarType::Boolean,
                galec::Name::ident("enable"),
            ),
            start: None,
        },
        galec::InterfaceVariable {
            kind: galec::InterfaceKind::Output,
            decl: ranged_real("y", Some(-1.0), Some(1.0)),
            start: None,
        },
        galec::InterfaceVariable {
            kind: galec::InterfaceKind::Output,
            decl: ranged_real("z", Some(-1.0), Some(1.0)),
            start: None,
        },
    ];
    block.startup.statements = vec![
        galec::Spanned::dummy(galec::Statement::If(galec::IfStatement {
            branches: vec![galec::IfBranch {
                condition: galec::Condition::Expression(galec::Expression::Ref(
                    galec::Reference::state(galec::Name::ident("enable")),
                )),
                body: vec![seed_state("y")],
                span: rumoca_core::Span::DUMMY,
            }],
            else_body: None,
        })),
        seed_state("z"),
    ];
    let block = CheckedAlgorithmBlock::construct(block).expect("valid conditional fixture");
    let source = crate::render_checked_algorithm_block_template_with_artifact(
        &block,
        &artifact_facts(),
        template_source("embedded-c-galec", "model.c.jinja"),
        "ConditionalSeed",
    )
    .expect("render conditional seed fixture");

    let startup_limiter = source
        .split_once("static void rumoca_galec_limit_startup(ConditionalSeedState *self) {")
        .and_then(|(_, rest)| rest.split_once("\n}"))
        .map(|(body, _)| body)
        .unwrap_or_else(|| panic!("the startup boundary needs its own limiter: {source}"));
    assert!(
        startup_limiter.contains("self->z") && !startup_limiter.contains("self->y"),
        "only the unconditionally written declaration is determinate: {source}"
    );
}

/// A method with an empty body limits its state on entry; saturation is
/// idempotent, so re-running the identical clamp on return is dead code.
#[test]
fn galec_c_empty_method_limits_its_boundary_once() {
    let source = range_block(
        "EmptyMethods",
        vec![(ranged_real("y", Some(-1.0), Some(1.0)), true)],
    );
    let recalibrate = source
        .split_once("void EmptyMethods_recalibrate(EmptyMethodsState *self) {")
        .and_then(|(_, rest)| rest.split_once("\n}"))
        .map(|(body, _)| body)
        .expect("recalibrate method body");
    assert_eq!(
        recalibrate
            .matches("rumoca_galec_limit_self(self);")
            .count(),
        1,
        "an empty method must not emit back-to-back boundary limits: {source}"
    );
}

#[test]
fn galec_real_min_max_are_relational_target_helpers() {
    let source = templates::builtin_template_source("embedded-c-galec", "model.c.jinja")
        .expect("C source template");

    assert!(!source.contains(r#"function == "min" -%}fmin"#), "{source}");
    assert!(!source.contains(r#"function == "max" -%}fmax"#), "{source}");
    assert!(
        source.contains(r#"function == "min" -%}rumoca_galec_min"#),
        "{source}"
    );
    assert!(
        source.contains(r#"function == "max" -%}rumoca_galec_max"#),
        "{source}"
    );
    assert!(source.contains("if (u1 < u2)"), "{source}");
    assert!(source.contains("if (u1 > u2)"), "{source}");
    assert!(
        source.contains("static inline int32_t rumoca_galec_imin"),
        "Integer min must retain its distinct builtin mapping"
    );
    assert!(
        source.contains("static inline int32_t rumoca_galec_imax"),
        "Integer max must retain its distinct builtin mapping"
    );
}

#[test]
fn galec_c_templates_enforce_the_assurance_profile_without_claiming_compliance() {
    let source = templates::builtin_template_source("embedded-c-galec", "model.c.jinja")
        .expect("C source template");
    let header = templates::builtin_template_source("embedded-c-galec", "model.h.jinja")
        .expect("C header template");

    for forbidden in [
        "malloc(",
        "calloc(",
        "realloc(",
        "free(",
        "#define rumoca_galec_",
    ] {
        assert!(
            !source.contains(forbidden),
            "forbidden `{forbidden}` in C template"
        );
        assert!(
            !header.contains(forbidden),
            "forbidden `{forbidden}` in H template"
        );
    }
    assert!(!header.contains("#  define EFMI_"), "{header}");
    assert!(header.contains("RUMOCA_{{ c_model | upper }}_GALEC_H_INCLUDED"));
    assert!(source.contains("MISRA compliance and DO-178C compliance are not claimed"));
    assert!(header.contains("MISRA compliance and DO-178C compliance are not claimed"));
    assert!(source.contains("dimensions|length == 1 %}const"));
    assert!(source.contains("rank > 1 therefore stays unqualified"));
}

/// The shared symbol policy, addressed the way the render environment
/// registers it and the way every C artifact template imports it.
const SHARED_SYMBOL_POLICY: &str = "galec-c-symbols.jinja";

/// The render environment registers exactly the declared shared templates —
/// no more (a name no manifest publishes) and no fewer (a declared name a
/// template could not import). This is what makes `[[partials]]` /
/// `[[files]].shared_as` the single authority over the shared namespace: the
/// environment holds no target-specific `include_str!` of its own.
#[test]
fn shared_render_environment_holds_exactly_the_declared_shared_templates() {
    let env = create_environment();
    let mut registered = env
        .templates()
        .map(|(name, _)| name.to_owned())
        .collect::<Vec<_>>();
    registered.sort();
    let mut declared = templates::shared_templates()
        .iter()
        .map(|shared| shared.name.to_owned())
        .collect::<Vec<_>>();
    declared.sort();
    assert_eq!(
        registered, declared,
        "the shared render environment must register exactly the manifest-declared \
         shared templates"
    );
    for shared in templates::shared_templates() {
        let template = env
            .get_template(shared.name)
            .unwrap_or_else(|error| panic!("{} must resolve: {error}", shared.name));
        assert_eq!(
            template.source(),
            shared.source,
            "{} must resolve to {}/{}",
            shared.name,
            shared.target,
            shared.path
        );
        assert_eq!(
            shared.source,
            templates::builtin_template_source(shared.target, shared.path)
                .expect("shared template must come from its declaring target bundle"),
            "{} must be served from the owning target bundle",
            shared.name
        );
    }
}

/// A support partial is exactly "declared, bundled, shared — and renders no
/// product file". The manifest-side half of that invariant (no `[[files]]`
/// entry) is checked where target manifests are parsed, in
/// `rumoca/tests/template_target_ci.rs`; this is the registry-side half.
#[test]
fn support_partials_are_shared_and_are_not_artifacts() {
    let mut partials = Vec::new();
    for target in templates::builtin_targets() {
        for template in target.support_partials() {
            let shared_name = template.shared_name.unwrap_or_else(|| {
                panic!(
                    "support partial {}/{} must declare a shared name; nothing else can \
                     reach it",
                    target.name, template.path
                )
            });
            let shared = templates::shared_template(shared_name)
                .unwrap_or_else(|| panic!("{shared_name} must be in the shared registry"));
            assert_eq!(
                (shared.target, shared.path),
                (target.name, template.path),
                "{shared_name} must resolve to the partial that declares it"
            );
            assert_eq!(shared.role, templates::BuiltinTemplateRole::SupportPartial);
            partials.push(format!("{}/{}", target.name, template.path));
        }
    }
    partials.sort();
    assert_eq!(
        partials,
        vec!["embedded-c-galec/symbols.jinja".to_string()],
        "the built-in support partials changed; keep the declaration and its \
         render-coverage exemption in step"
    );
}

/// How an artifact template reaches the shared symbol policy.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum PolicyBinding {
    /// The template imports the policy itself.
    Imports,
    /// The template `{% extends %}` a shared base that imports the policy, and
    /// overrides only its own blocks. The base is named by its shared
    /// render-environment name.
    Inherits(&'static str),
}

/// EVERY artifact that prints C identifiers from the checked block — the two
/// `embedded-c-galec` leaves, both `galec-production` ProductionCode leaves
/// (which extend them), and the Production Code manifest. A C artifact missing
/// from this list is a C artifact whose symbols nothing proves came from the
/// one policy, so `every_c_artifact_template_is_covered` re-derives the list
/// from the bundles.
const C_ARTIFACT_TEMPLATES: [(&str, &str, PolicyBinding); 5] = [
    ("embedded-c-galec", "model.c.jinja", PolicyBinding::Imports),
    ("embedded-c-galec", "model.h.jinja", PolicyBinding::Imports),
    (
        "galec-production",
        "model.c.jinja",
        PolicyBinding::Inherits("galec-model.c.jinja"),
    ),
    (
        "galec-production",
        "model.h.jinja",
        PolicyBinding::Inherits("galec-model.h.jinja"),
    ),
    (
        "galec-production",
        "pc_manifest.xml.jinja",
        PolicyBinding::Imports,
    ),
];

/// The shared render-environment name a template extends, if any.
fn extends_shared_name(source: &str) -> Option<&str> {
    let rest = source.split_once("extends")?.1.trim_start();
    let rest = rest.strip_prefix('"')?;
    let end = rest.find('"')?;
    Some(&rest[..end])
}

/// A template prints C identifiers when it reads either symbol table, or when
/// it extends a shared base that does: an `{% extends %}` leaf renders the
/// base's whole body, so it emits exactly those C identifiers.
fn prints_c_symbols(source: &str) -> bool {
    if source.contains("c_symbols") || source.contains("c_state_symbols") {
        return true;
    }
    extends_shared_name(source)
        .and_then(templates::shared_template)
        .is_some_and(|base| {
            // One level: the built-in bases import the policy directly, and a
            // deeper chain would have to declare its own shared name and land
            // in this list too.
            base.source.contains("c_symbols") || base.source.contains("c_state_symbols")
        })
}

/// Nothing may print C identifiers outside the covered set: a new C artifact
/// (or a new C target) must join `C_ARTIFACT_TEMPLATES`, not quietly allocate
/// its own spellings.
#[test]
fn every_c_artifact_template_is_covered() {
    let mut printing = Vec::new();
    for target in templates::builtin_targets() {
        for template in target.templates {
            // The policy itself defines the tables rather than printing from
            // them.
            if template.shared_name == Some(SHARED_SYMBOL_POLICY) {
                continue;
            }
            if prints_c_symbols(template.source) {
                printing.push((target.name, template.path));
            }
        }
    }
    printing.sort_unstable();
    let mut covered = C_ARTIFACT_TEMPLATES
        .iter()
        .map(|(target, path, _)| (*target, *path))
        .collect::<Vec<_>>();
    covered.sort_unstable();
    assert_eq!(
        printing, covered,
        "every template printing C symbols must be covered by the shared-policy tests"
    );
}

fn template_source(target: &str, path: &str) -> &'static str {
    templates::builtin_template_source(target, path)
        .unwrap_or_else(|| panic!("{target}/{path} missing"))
}

#[test]
fn galec_c_symbol_policy_has_exactly_one_declaration_site() {
    let policy = template_source("embedded-c-galec", "symbols.jinja");
    assert!(
        policy.contains("{%- set reserved = ["),
        "the shared policy must declare the reserved list: {policy}"
    );
    assert!(
        policy.contains("\"generated_prefixes\": [\"rumoca_galec_\"]"),
        "the shared policy must declare the generated namespace: {policy}"
    );
    assert_eq!(
        policy.matches("allocate_symbols(").count(),
        2,
        "the shared policy must allocate exactly the two symbol tables: {policy}"
    );

    // Global single-source check: no other built-in template of any target
    // may allocate C symbols, so a copied reserved list cannot reappear
    // anywhere in the template set.
    let mut allocating = Vec::new();
    for target in templates::builtin_targets() {
        for template in target.templates {
            if template.source.contains("allocate_symbols(") {
                allocating.push(format!("{}/{}", target.name, template.path));
            }
        }
    }
    assert_eq!(
        allocating,
        vec!["embedded-c-galec/symbols.jinja".to_string()],
        "symbol allocation must be declared once, in the shared policy"
    );

    for (target, path, binding) in C_ARTIFACT_TEMPLATES {
        let source = template_source(target, path);
        assert!(
            !source.contains("c_reserved"),
            "{target}/{path} re-declares a local reserved list; a second declaration \
             site is what renames the same variable differently across artifacts"
        );
        assert!(
            !source.contains("generated_prefixes"),
            "{target}/{path} re-declares the generated namespace"
        );
        match binding {
            PolicyBinding::Imports => {
                assert!(
                    source.contains(&format!(
                        "{{%- import \"{SHARED_SYMBOL_POLICY}\" as c_symbol_policy -%}}"
                    )),
                    "{target}/{path} must import the shared symbol policy"
                );
                assert!(
                    source.contains("{%- set c_symbols = c_symbol_policy.symbols -%}")
                        && source.contains(
                            "{%- set c_state_symbols = c_symbol_policy.state_symbols -%}"
                        ),
                    "{target}/{path} must bind both symbol tables from the shared policy"
                );
            }
            PolicyBinding::Inherits(base) => {
                // An extending leaf overrides named blocks only; it must not
                // re-import the policy (that would allocate a second time) and
                // its base must be the shared artifact template that does.
                assert_eq!(
                    extends_shared_name(source),
                    Some(base),
                    "{target}/{path} must extend the shared base {base}"
                );
                assert!(
                    !source.contains(SHARED_SYMBOL_POLICY),
                    "{target}/{path} extends {base} and must not import the policy again"
                );
                let shared = templates::shared_template(base)
                    .unwrap_or_else(|| panic!("{base} must be a declared shared template"));
                assert!(
                    C_ARTIFACT_TEMPLATES
                        .iter()
                        .any(|(covered_target, covered_path, covered)| {
                            (*covered_target, *covered_path) == (shared.target, shared.path)
                                && *covered == PolicyBinding::Imports
                        }),
                    "the base {base} ({}/{}) must itself be a covered importing artifact",
                    shared.target,
                    shared.path
                );
            }
        }
    }
}

/// Render one artifact template against a substitute shared symbol policy.
///
/// Overriding the registered policy name is the only difference from the
/// production render path, so whatever the artifact spells for a source
/// reference must have come from that one file.
fn render_block_with_symbol_policy(
    block: &CheckedAlgorithmBlock,
    template: &str,
    policy: &str,
) -> String {
    let mut env = create_environment();
    env.add_template_owned(SHARED_SYMBOL_POLICY.to_owned(), policy.to_owned())
        .expect("substitute symbol policy must parse");
    env.add_template_owned("inline".to_owned(), template.to_owned())
        .expect("artifact template must parse");
    let sources = rumoca_core::SourceMap::new();
    let view = crate::views::algorithm_code::CheckedAlgorithmBlockView::new(block, &sources)
        .expect("checked block view");
    env.get_template("inline")
        .expect("artifact template")
        .render(minijinja::context! {
            algorithm_code => minijinja::Value::from_serialize(view),
            artifact => minijinja::Value::from_serialize(artifact_facts()),
            ir_kind => "algorithm_code",
            model_name => "fixture",
        })
        .expect("render against the substitute symbol policy")
}

#[test]
fn galec_c_artifacts_resolve_every_symbol_through_the_shared_policy() {
    let block = generated_prefix_block();
    let policy = template_source("embedded-c-galec", "symbols.jinja");
    // The production policy plus two extra reserved names. Nothing else
    // changes, so every artifact that reads the shared policy must move to
    // the numbered spellings together; an artifact holding a private copy
    // would keep emitting `gain`/`copy_0`.
    let substitute = policy.replace(
        "\"self\", \"rumoca_galec_sign\"",
        "\"self\", \"gain\", \"copy_0\", \"rumoca_galec_sign\"",
    );
    assert_ne!(
        substitute, policy,
        "the substitute policy must actually differ from the production one"
    );

    for (target, path, _) in C_ARTIFACT_TEMPLATES {
        let template = template_source(target, path);
        let rendered = render_block_with_symbol_policy(&block, template, &substitute);
        assert!(
            rendered.contains("gain_2"),
            "{target}/{path} did not take the state-field symbol from the shared \
             policy: {rendered}"
        );
        // Remove the substitute policy's spelling and the source name that
        // survives in declaration comments; any remaining `gain` would be a
        // symbol this artifact allocated on its own.
        let residue = rendered
            .replace("gain_2", "")
            .replace("rumoca_galec_gain", "");
        assert!(
            !residue.contains("gain"),
            "{target}/{path} kept a privately allocated symbol: {rendered}"
        );
    }

    // The method local lives only in the C body, so its shared-policy
    // dependence is checked where it is emitted.
    let source = render_block_with_symbol_policy(
        &block,
        template_source("embedded-c-galec", "model.c.jinja"),
        &substitute,
    );
    assert!(
        source.contains("float copy_0_2;") && !source.contains("float copy_0;"),
        "the renamed local must also come from the shared policy: {source}"
    );
    assert!(
        source.contains(&format!(
            "for (int32_t {GENERATED_COPY_ITERATOR} = INT32_C(0);"
        )),
        "the generated loop counter is template-owned and must not move: {source}"
    );
}

#[test]
fn galec_c_artifacts_agree_on_one_state_field_ordering() {
    let block = collision_block();
    let header = render_block_fixture(&block, template_source("embedded-c-galec", "model.h.jinja"));
    let manifest = render_block_fixture(
        &block,
        template_source("galec-production", "pc_manifest.xml.jinja"),
    );

    // Header struct fields, in declaration order.
    let fields = header
        .lines()
        .filter_map(|line| line.trim().strip_prefix("float "))
        .filter_map(|line| line.split(';').next())
        .map(str::to_owned)
        .collect::<Vec<_>>();
    assert_eq!(fields.len(), 4, "{header}");

    // Manifest components, in the same declaration order, excluding the
    // template-owned error-signal component appended after the block's own
    // variables.
    let components = manifest
        .match_indices("<Component id=\"CO_")
        .filter_map(|(index, _)| manifest[index..].split_once("name=\""))
        .filter_map(|(_, rest)| rest.split_once('"'))
        .map(|(name, _)| name.to_owned())
        .filter(|name| name != "rumoca_galec_error_signal_status")
        .collect::<Vec<_>>();

    assert_eq!(
        fields, components,
        "LogicalData components must name the same fields, in the same order, as \
         the generated struct — both resolve through one shared allocation"
    );
}
