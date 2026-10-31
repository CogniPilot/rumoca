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

#[path = "../../tests/support/solve_render.rs"]
mod support;

/// A C model template (or one extending the shared GALEC C model units)
/// prints from the Solve view.
fn needs_solve(template: &str) -> bool {
    ["solve_block", "galec-model.c.jinja", "galec-model.h.jinja"]
        .iter()
        .any(|marker| template.contains(marker))
}

fn render_block_fixture(block: &CheckedAlgorithmBlock, template: &str) -> String {
    if needs_solve(template) {
        // The C model units print from the block's Solve refinement (#483).
        return support::render_refined(block, &rumoca_core::SourceMap::new(), template, "fixture")
            .expect("render collision fixture");
    }
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

    // The Solve-printed body names method locals by slot id and keeps its own
    // loop counter `k` withheld from the symbol tables: a user local spelled
    // like a generated identifier never reaches the C, so it cannot shadow
    // the loop index or be shadowed by it.
    assert!(
        source.contains("for (size_t k = 0; k < "),
        "the array copy must still emit its own loop counter: {source}"
    );
    assert!(
        !source.contains(GENERATED_COPY_ITERATOR) && !source.contains("copy_0"),
        "a method local's source name must not reach the C body: {source}"
    );
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
        .filter(|line| line.contains("/* declaration "))
        .filter_map(|line| line.trim().strip_prefix("float "))
        .filter_map(|line| line.split(';').next())
        // The clock constant the refinement harness adds is not a fixture field.
        .filter(|field| *field != support::FIXTURE_PERIOD)
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
    // The Solve-printed bodies declare these unprefixed identifiers (every
    // other local carries the `rumoca_galec_` prefix): a file-scope symbol
    // spelled like one would be shadowed inside them.
    let policy = template_source("embedded-c-galec", "symbols.jinja");
    let source = template_source("embedded-c-galec", "model.c.jinja");
    let withheld = policy
        .split_once("{%- set helper_locals = [")
        .and_then(|(_, rest)| rest.split_once("] -%}"))
        .map(|(list, _)| list)
        .unwrap_or_else(|| panic!("the shared policy must declare helper_locals: {policy}"));
    for (local, declaration) in [
        ("k", "size_t k "),
        ("ctx", "*const ctx = "),
        ("scratch", "*const scratch = "),
    ] {
        assert!(
            withheld.contains(&format!("\"{local}\"")),
            "the shared policy must withhold the helper local `{local}`: {policy}"
        );
        assert!(
            source.contains(declaration),
            "`{local}` is withheld but no emitted body declares it: {source}"
        );
    }
    assert!(
        policy.contains("\"reserved\": reserved + helper_locals")
            && policy.contains("\"reserved\": reserved,"),
        "only the file-scope-capable table may take the helper locals: {policy}"
    );

    // A user function spelled like a helper local is renamed at file scope;
    // a state field of that spelling is a struct member and keeps it.
    let real = |name: &str| {
        galec::VariableDeclaration::scalar(galec::ScalarType::Real, galec::Name::ident(name))
    };
    let mut block = galec::Block::new(galec::Name::ident("HelperLocals"));
    block.interface = vec![
        galec::InterfaceVariable {
            kind: galec::InterfaceKind::Input,
            decl: real("k"),
            start: None,
        },
        galec::InterfaceVariable {
            kind: galec::InterfaceKind::Output,
            decl: real("y"),
            start: None,
        },
    ];
    block.protected_functions = vec![galec::UserFunction {
        kind: galec::FunctionKind::Stateless,
        name: galec::Name::ident("k"),
        signals: Vec::new(),
        parameters: vec![
            galec::Parameter {
                direction: galec::Direction::Input,
                decl: real("x"),
            },
            galec::Parameter {
                direction: galec::Direction::Output,
                decl: real("r"),
            },
        ],
        locals: Vec::new(),
        statements: vec![galec::Spanned::dummy(galec::Statement::Assignment {
            target: galec::Reference::local(galec::Name::ident("r")),
            value: galec::Expression::Ref(galec::Reference::local(galec::Name::ident("x"))),
        })],
        span: rumoca_core::Span::DUMMY,
    }];
    block.do_step.statements = vec![galec::Spanned::dummy(galec::Statement::Assignment {
        target: galec::Reference::state(galec::Name::ident("y")),
        value: galec::Expression::Call(galec::FunctionCall {
            function: galec::Name::ident("k"),
            arguments: vec![galec::Expression::Ref(galec::Reference::state(
                galec::Name::ident("k"),
            ))],
        }),
    })];
    let block = CheckedAlgorithmBlock::construct(block).expect("valid helper-local fixture");

    let header = render_block_fixture(&block, template_source("embedded-c-galec", "model.h.jinja"));
    assert!(
        header.contains("float k; /* declaration 1: k"),
        "a struct member is in its own C namespace and must not be renamed: {header}"
    );
    let source = render_block_fixture(&block, template_source("embedded-c-galec", "model.c.jinja"));
    assert!(
        source.contains("static int k_2(") && !source.contains("static int k("),
        "a function spelled like a helper local must yield to it: {source}"
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

/// The bound spelling a Modelica `Modelica.Constants.inf` declaration reaches
/// this target as. MSL defines it as `1e60`, which is finite in the checked
/// block's `f64` and *not* representable as a `float`: rendering it produces
/// `1.0e+60f`, which the target's own compile preflight rejects
/// (`floating constant exceeds range of 'float'`).
const MODELICA_INFINITY: f64 = 1.0e60;

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
    // Function inputs are read-only at every rank in the Solve-printed C.
    assert!(source.contains("\"const \" ~ ctype(input) ~ \" *rumoca_galec_in\""));
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
/// `rumoca/tests/suite_template_runtime/template_target_ci.rs`; this is the registry-side half.
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
        vec![
            "embedded-c-galec/scratch.jinja".to_string(),
            "embedded-c-galec/solve_c.jinja".to_string(),
            "embedded-c-galec/symbols.jinja".to_string(),
            "fmi3/me_projection.jinja".to_string(),
            "fmi3/scalar_events.jinja".to_string(),
            "fmi3/scalar_kernel.jinja".to_string(),
            "fmi3/typed_arithmetic.jinja".to_string(),
            "fmi3/typed_functions.jinja".to_string(),
            "fmi3/typed_tensors.jinja".to_string(),
        ],
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
            // Support partials render no product artifact on their own. Their
            // consumers remain covered below; the symbol policy itself also
            // defines rather than prints the tables.
            if template.role == templates::BuiltinTemplateRole::SupportPartial {
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
    let template = env.get_template("inline").expect("artifact template");
    let rendered = if needs_solve(template.source()) {
        // The C model units print from the block's Solve refinement (#483).
        let (package, refined) = support::refine(block).expect("fixture refines");
        let view = crate::views::algorithm_code::AlgorithmCodeView::new(&package, &sources)
            .expect("package view");
        template.render(minijinja::context! {
            algorithm_code => minijinja::Value::from_serialize(view),
            solve_block => super::solve_block_renderer::solve_block_value(&refined, &sources),
            artifact => minijinja::Value::from_serialize(artifact_facts()),
            ir_kind => "algorithm_code",
            model_name => "fixture",
        })
    } else {
        let view = crate::views::algorithm_code::CheckedAlgorithmBlockView::new(block, &sources)
            .expect("checked block view");
        template.render(minijinja::context! {
            algorithm_code => minijinja::Value::from_serialize(view),
            artifact => minijinja::Value::from_serialize(artifact_facts()),
            ir_kind => "algorithm_code",
            model_name => "fixture",
        })
    };
    rendered.expect("render against the substitute symbol policy")
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

    // A method local is printed by slot id in the Solve-printed body, never
    // by name, so no spelling of it reaches the C for any policy to allocate.
    let source = render_block_with_symbol_policy(
        &block,
        template_source("embedded-c-galec", "model.c.jinja"),
        &substitute,
    );
    assert!(
        !source.contains("copy_0") && !source.contains(GENERATED_COPY_ITERATOR),
        "a method local's source name must not reach the C body: {source}"
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
        .filter(|line| line.contains("/* declaration "))
        .filter_map(|line| line.trim().strip_prefix("float "))
        .filter_map(|line| line.split(';').next())
        // The clock constant the refinement harness adds is not a fixture field.
        .filter(|field| *field != support::FIXTURE_PERIOD)
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
