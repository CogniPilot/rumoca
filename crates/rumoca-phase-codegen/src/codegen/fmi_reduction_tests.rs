//! MLS §10.3.4 and SPEC_0040 SOLVE-C66: FMI reductions preserve operation
//! order, NaNs and signed zeros through the shared typed-function renderer.

use rumoca_ir_solve as solve;
use solve::{SolveReductionOperator as Reduction, SolveScalarType as Scalar};

use super::codegen_test_support::builtin_template;

fn program(operator: Reduction, scalar: Scalar, count: u32) -> solve::TypedProgram {
    let profile = solve::SolveArithmeticProfile::construct(
        match scalar {
            Scalar::Real { format } => format,
            _ => solve::SolveRealFormat::Binary64,
        },
        solve::SolveIntegerDomain::FULL,
    );
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("fmi_reduction.mo"),
        0,
        1,
    );
    solve::TypedProgram::construct(profile, |builder| {
        let input = builder.declare_slot(
            solve::SolveValueType::tensor(scalar, vec![count]).unwrap(),
            solve::SolveStorageClass::Input,
            solve::SolveSlotAccess::ReadOnly,
            span,
        )?;
        let output = builder.declare_slot(
            solve::SolveValueType::scalar(scalar),
            solve::SolveStorageClass::Output,
            solve::SolveSlotAccess::ReadWrite,
            span,
        )?;
        let value = builder.load(input, span)?;
        let reduced = builder.reduce(operator, value, span)?;
        builder.store(output, reduced, span)
    })
    .expect("checked reduction program")
}

fn render(program: &solve::TypedProgram, name: &str) -> Result<String, minijinja::Error> {
    let mut environment = super::create_environment();
    for (name, file) in [
        ("fmi-typed-arithmetic.jinja", "typed_arithmetic.jinja"),
        ("fmi-typed-tensors.jinja", "typed_tensors.jinja"),
        ("functions", "typed_functions.jinja"),
    ] {
        environment.add_template(name, builtin_template("fmi3", file))?;
    }
    environment.render_str(
        "{% from 'functions' import definition %}{{ definition(program, name) }}",
        minijinja::context! { program => program, name => name },
    )
}

const REAL: Scalar = Scalar::Real {
    format: solve::SolveRealFormat::Binary64,
};
const INTEGER: Scalar = Scalar::Integer(solve::SolveIntegerDomain::FULL);

#[test]
fn generated_reductions_execute_with_ordered_values_and_ieee_edges() {
    let mut source = String::from(
        "#include <assert.h>\n#include <math.h>\n#include <stdbool.h>\n\
         #include <stdint.h>\n#include <stddef.h>\n#include <string.h>\n",
    );
    source.push_str(
        &super::create_environment()
            .render_str("{{ real_extremum_prelude('c') }}", ())
            .unwrap(),
    );
    source.push_str(OBSERVER_TYPE);
    for (name, operator, scalar) in [
        ("sum3", Reduction::Sum, REAL),
        ("product3", Reduction::Product, REAL),
        ("min3", Reduction::Minimum, REAL),
        ("max3", Reduction::Maximum, REAL),
        ("all3", Reduction::All, Scalar::Boolean),
        ("min_bool3", Reduction::Minimum, Scalar::Boolean),
        ("max_bool3", Reduction::Maximum, Scalar::Boolean),
        ("sum_integer3", Reduction::Sum, INTEGER),
        ("product_integer3", Reduction::Product, INTEGER),
        ("min_integer3", Reduction::Minimum, INTEGER),
        ("max_integer3", Reduction::Maximum, INTEGER),
    ] {
        source.push_str(&render(&program(operator, scalar, 3), name).unwrap());
    }
    source.push_str(HARNESS);
    compile_and_run(&source);
}

fn compile_and_run(source: &str) {
    let work = tempfile::tempdir().unwrap();
    let path = work.path().join("reductions.c");
    let binary = work.path().join("reductions");
    std::fs::write(&path, source).unwrap();
    let compiled = std::process::Command::new("cc")
        .args([
            "-std=c11",
            "-O2",
            "-ffp-contract=off",
            "-Wall",
            "-Wextra",
            "-Werror",
        ])
        .arg(&path)
        .args(["-lm", "-o"])
        .arg(&binary)
        .output()
        .expect("start C compiler");
    assert!(
        compiled.status.success(),
        "{}",
        String::from_utf8_lossy(&compiled.stderr)
    );
    assert!(
        std::process::Command::new(binary)
            .status()
            .unwrap()
            .success()
    );
}

const HARNESS: &str = r#"
#define RUN(fn, input) assert(fn((const void*[]){input}, (void*[]){&result}, NULL) == 0)
#define RUN_INTEGER(fn, input) assert(fn((const void*[]){input}, (void*[]){&integer_result}, NULL) == 0)
int main(void) {
    double result, ordinary[] = {5.0, 10.0, 15.0};
    RUN(sum3, ordinary); assert(result == 30.0);
    RUN(product3, ordinary); assert(result == 750.0);
    RUN(min3, ordinary); assert(result == 5.0);
    RUN(max3, ordinary); assert(result == 15.0);
    double ordered[] = {1e16, -1e16, 1.0};
    RUN(sum3, ordered); assert(result == 1.0);
    double zeros[] = {-0.0, -0.0, -0.0};
    RUN(sum3, zeros); assert(result == 0.0 && signbit(result));
    RUN(product3, zeros); assert(result == 0.0 && signbit(result));
    zeros[0] = 0.0;
    RUN(min3, zeros); assert(result == 0.0 && signbit(result));
    RUN(max3, zeros); assert(result == 0.0 && signbit(result));
    zeros[2] = 0.0;
    RUN(min3, zeros); assert(result == 0.0 && !signbit(result));
    RUN(max3, zeros); assert(result == 0.0 && !signbit(result));
    double nans[] = {NAN, NAN, NAN};
    RUN(min3, nans); assert(isnan(result));
    RUN(max3, nans); assert(isnan(result));
    nans[1] = 7.0;
    RUN(min3, nans); assert(result == 7.0);
    RUN(max3, nans); assert(result == 7.0);
    bool flags[] = {true, true, true}, all;
    assert(all3((const void*[]){flags}, (void*[]){&all}, NULL) == 0 && all);
    flags[1] = false;
    assert(all3((const void*[]){flags}, (void*[]){&all}, NULL) == 0 && !all);
    assert(min_bool3((const void*[]){flags}, (void*[]){&all}, NULL) == 0 && !all);
    assert(max_bool3((const void*[]){flags}, (void*[]){&all}, NULL) == 0 && all);
    int64_t integer_result, integers[] = {-3, 2, -4};
    RUN_INTEGER(sum_integer3, integers); assert(integer_result == -5);
    RUN_INTEGER(product_integer3, integers); assert(integer_result == 24);
    RUN_INTEGER(min_integer3, integers); assert(integer_result == -4);
    RUN_INTEGER(max_integer3, integers); assert(integer_result == 2);
    int64_t wide[] = {INT64_C(9007199254740993), 2, -1};
    RUN_INTEGER(sum_integer3, wide); assert(integer_result == INT64_C(9007199254740994));
    wide[1] = wide[2] = 1;
    RUN_INTEGER(product_integer3, wide); assert(integer_result == INT64_C(9007199254740993));
    wide[1] = wide[0]+2; wide[2] = wide[0]-2;
    RUN_INTEGER(min_integer3, wide); assert(integer_result == INT64_C(9007199254740991));
    RUN_INTEGER(max_integer3, wide); assert(integer_result == INT64_C(9007199254740995));
    int64_t overflow[] = {INT64_MAX, 1, 0};
    integer_result = 177;
    assert(sum_integer3((const void*[]){overflow}, (void*[]){&integer_result}, NULL) != 0);
    assert(integer_result == 177);
    overflow[1] = 2;
    assert(product_integer3((const void*[]){overflow}, (void*[]){&integer_result}, NULL) != 0);
    assert(integer_result == 177);
    return 0;
}
"#;

#[test]
fn reductions_outside_the_supported_profile_fail_during_rendering() {
    for operator in [
        Reduction::Sum,
        Reduction::Product,
        Reduction::Minimum,
        Reduction::Maximum,
    ] {
        let scalar = Scalar::Real {
            format: solve::SolveRealFormat::Binary32,
        };
        assert!(render(&program(operator, scalar, 3), "unsupported").is_err());
    }
}

/// The assertion observer the generated typed-function signature names; these
/// reductions carry no assertion, so every call passes no observer.
const OBSERVER_TYPE: &str = "typedef struct { void* context; int (*observe)(void*, size_t, const void* const[], size_t); } RmcAssertionObserver;\n\
static inline double rumoca_real64(uint64_t bits) { double value; memcpy(&value, &bits, sizeof(value)); return value; }\n";

#[test]
fn guarded_fold_predicate_receives_the_same_assigned_captures_as_its_transition() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("guarded_fold.mo"),
        0,
        1,
    );
    let profile = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    );
    let program = solve::TypedProgram::construct(profile, |b| {
        let output = b.declare_slot(
            solve::SolveValueType::scalar(REAL),
            solve::SolveStorageClass::Output,
            solve::SolveSlotAccess::ReadWrite,
            span,
        )?;
        let initial = b.constant(solve::SolveValue::real(profile, 1.0), span)?;
        let state = b.present_assigned_state(initial, span)?;
        let domain = rumoca_core::StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                provenance: rumoca_core::IndexBinderProvenance::Generated,
                id: 0,
                display_name: "i".into(),
                lower: 1,
                upper: 2,
                step: 1,
            }],
        };
        let carried = b.fold_while_assigned(
            domain,
            &[initial],
            (&[], &[state]),
            span,
            |region, states, _, _, _, outputs| {
                let present = region.assigned_state_is_present(states[0], span)?;
                region.store(outputs[0], present, span)
            },
            |region, _, carried, _, _, outputs| {
                let value = region.load(carried[0], span)?;
                region.store(outputs[0], value, span)
            },
        )?;
        b.store(output, carried[0], span)
    })
    .unwrap();
    let source = render(&program, "guarded_fold").unwrap();
    for suffix in ["_continue((const void*[])", "_fold((const void*[])"] {
        let call = source.lines().find(|line| line.contains(suffix)).unwrap();
        assert!(call.contains("&present0,v0,"), "{call}");
    }
}

#[test]
fn concatenate_copies_nonleading_axes_in_row_major_order() {
    let span =
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name("cat.mo"), 0, 1);
    let profile = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    );
    let mut source = String::from(
        "#include <assert.h>\n#include <stddef.h>\n#include <string.h>\n#include <stdint.h>\n#include <stdbool.h>\n",
    );
    source.push_str(OBSERVER_TYPE);
    for (axis, name, trailing, shape) in [
        (0, "rows", vec![1, 2], vec![3, 2]),
        (1, "cols", vec![2, 1], vec![2, 3]),
    ] {
        let program = solve::TypedProgram::construct(profile, |b| {
            let a = b.declare_slot(
                solve::SolveValueType::tensor(REAL, vec![2, 2]).unwrap(),
                solve::SolveStorageClass::Input,
                solve::SolveSlotAccess::ReadOnly,
                span,
            )?;
            let d = b.declare_slot(
                solve::SolveValueType::tensor(REAL, trailing).unwrap(),
                solve::SolveStorageClass::Input,
                solve::SolveSlotAccess::ReadOnly,
                span,
            )?;
            let out = b.declare_slot(
                solve::SolveValueType::tensor(REAL, shape).unwrap(),
                solve::SolveStorageClass::Output,
                solve::SolveSlotAccess::ReadWrite,
                span,
            )?;
            let a = b.load(a, span)?;
            let d = b.load(d, span)?;
            let combined = b.concatenate(axis, &[a, d], span)?;
            b.store(out, combined, span)
        })
        .unwrap();
        let rendered = render(&program, name).unwrap();
        assert!(!rendered.contains("cursor++"));
        source.push_str(&rendered);
    }
    source.push_str(
        r#"
int main(void) {
    double a[]={1,2,3,4}, b[]={5,6}, out[6]={0};
    const void* inputs[]={a,b};
    void* outputs[]={out};
    assert(rows(inputs,outputs,NULL)==0 && memcmp(out,(double[]){1,2,3,4,5,6},sizeof(out))==0);
    assert(cols(inputs,outputs,NULL)==0 && memcmp(out,(double[]){1,2,5,3,4,6},sizeof(out))==0);
}
"#,
    );
    compile_and_run(&source);
}
