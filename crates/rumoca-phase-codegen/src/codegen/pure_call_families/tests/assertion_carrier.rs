use super::*;

fn fault_table() -> solve::SolvePureCallTable {
    fault_table_at_level(solve::SolveAssertionLevel::Error)
}

fn fault_table_at_level(level: solve::SolveAssertionLevel) -> solve::SolvePureCallTable {
    let profile = profile();
    let integer = solve::SolveValueType::scalar(solve::SolveScalarType::integer(profile));
    solve::SolvePureCallTable::construct(profile, |table| {
        table.add_owner(
            identity(950),
            vec![
                solve::SolveValueType::scalar(solve::SolveScalarType::Boolean),
                integer.clone(),
            ],
            vec![
                solve::SolvePureCallOutput::result(integer.clone()),
                solve::SolvePureCallOutput::assertion_predicate_at_level(level),
                solve::SolvePureCallOutput::assertion_message_value(integer, 1),
            ],
            span(950),
            |builder, inputs, outputs| {
                let condition = builder.load(inputs[0], span(951))?;
                let assertion = builder.assertion_output(1, span(952))?;
                let message = builder.check_assertion(
                    assertion,
                    condition,
                    &[],
                    span(953),
                    |region, _, output| {
                        let capture = region.constant(
                            solve::SolveValue::integer(profile, 9007199254740993).unwrap(),
                            span(954),
                        )?;
                        region.store(output[0], capture, span(955))
                    },
                )?;
                let one =
                    builder.constant(solve::SolveValue::integer(profile, 1).unwrap(), span(956))?;
                let values = builder.construct_aggregate(&[one], vec![1], span(957))?;
                let index = builder.load(inputs[1], span(958))?;
                let fault = builder.project_element_dynamic(values, &[index], span(958))?;
                builder.store(outputs[0], fault, span(959))?;
                builder.store(outputs[1], condition, span(960))?;
                builder.store(outputs[2], message[0], span(961))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn observation_dependencies_include_inputs_used_after_the_first_assertion() {
    let table = fault_table();
    let site = table.owners()[0].call_site();
    let program = vec![
        solve::LinearOp::LoadY { dst: 0, index: 3 },
        solve::LinearOp::LoadY { dst: 1, index: 7 },
        solve::LinearOp::PureCallObservation {
            dst_start: 2,
            input_starts: Box::new([0, 1]),
            site: solve::SolveAssertionObservationSite::new(site).unwrap(),
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ];
    let dependency = solve::ScalarProgramYDependency::new(&program);
    let footprint = dependency
        .footprint([2])
        .expect("complete observation input proof");
    assert!(footprint.contains(3));
    assert!(footprint.contains(7));
    assert!(!footprint.contains(5));
}

const CONTROL: &str = r#"{% from "fmi-typed-functions.jinja" import functions, function_prototypes %}
#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <math.h>
#define RMC_API
typedef struct ModelInstance ModelInstance;
{{ function_prototypes(table) }}
{{ functions(table) }}
typedef struct { size_t reached; double* predicate; } Probe;
static int observed(void* context, size_t predicate, const void* const captures[], size_t count) {
    Probe* probe=context;
    assert(predicate==1 && count==1);
    assert(*(const int64_t*)captures[0]==INT64_C(9007199254740993));
    ++probe->reached;
    if (probe->predicate) *probe->predicate=0.0;
    return 0;
}
int main(void) {
    double input=0.0, index=2.0, output[3]={91.0, 92.0, 93.0};
    const double* arguments[]={&input, &index};
    Probe probe={0, NULL};
    RmcAssertionObserver observer={&probe, observed};
    assert(rumoca_scalar_pure_0(arguments, output, &observer)==2);
    assert(probe.reached==1 && output[0]==91.0 && output[1]==92.0 && output[2]==93.0);
    input=1.0; index=1.0;
    assert(rumoca_scalar_pure_0(arguments, output, &observer)==0);
    assert(probe.reached==1 && output[0]==1.0 && output[1]==1.0 && output[2]==0.0);
    output[0]=91.0; output[1]=92.0; output[2]=93.0; index=2.0;
    input=1.0;
    assert(rumoca_scalar_pure_0(arguments, output, &observer)==1);
    assert(probe.reached==1 && output[0]==91.0 && output[1]==92.0 && output[2]==93.0);
    input=0.0;
    double predicate=99.0;
    probe.predicate=&predicate;
    assert(rumoca_scalar_observe_0(arguments, &predicate, &observer)==2);
    assert(probe.reached==2 && predicate==0.0 && output[0]==91.0);
    assert(rumoca_scalar_pure_0(arguments, output, NULL)==1);
    return 0;
}
"#;

#[test]
fn compiled_c_keeps_first_fault_and_exact_captures_without_publishing_values() {
    let families = PureCallFamilies::new(&fault_table()).unwrap();
    let mut environment = super::super::super::create_environment();
    environment
        .add_template("assertion_carrier", CONTROL)
        .unwrap();
    let source = environment
        .get_template("assertion_carrier")
        .unwrap()
        .render(minijinja::context! { table => families.owners_value() })
        .unwrap();
    compile_and_execute(&source);
}

pub(super) fn compile_and_execute(source: &str) {
    let work = tempfile::tempdir().unwrap();
    let file = work.path().join("assertion.c");
    std::fs::write(&file, source).unwrap();
    let binary = work.path().join("assertion");
    let compiled = std::process::Command::new("cc")
        .args([
            "-std=c11",
            "-O2",
            "-ffp-contract=off",
            "-Wall",
            "-Wextra",
            "-Werror",
            "-Wvla",
        ])
        .arg(file)
        .args(["-lm", "-o"])
        .arg(&binary)
        .output()
        .unwrap();
    assert!(
        compiled.status.success(),
        "{}\n{source}",
        String::from_utf8_lossy(&compiled.stderr)
    );
    let executed = std::process::Command::new(binary).output().unwrap();
    assert!(
        executed.status.success(),
        "{}",
        String::from_utf8_lossy(&executed.stderr)
    );
}

fn observation_problem(table: &solve::SolvePureCallTable) -> solve::SolveProblem {
    let site = table.owners()[0].call_site();
    let message = vec![
        solve::LinearOp::Const { dst: 0, value: 0.0 },
        solve::LinearOp::Const { dst: 1, value: 2.0 },
        solve::LinearOp::PureCall {
            dst_start: 2,
            input_starts: vec![0, 1].into(),
            site: site.clone(),
        },
        solve::LinearOp::StoreOutput { src: 4 },
    ];
    let mut problem = solve::SolveProblem::default();
    problem.events.actions = vec![solve::SolveEventAction {
        kind: match table.owners()[0].outputs()[1].assertion_level().unwrap() {
            solve::SolveAssertionLevel::Error => solve::SolveEventActionKind::Assert,
            solve::SolveAssertionLevel::Warning => solve::SolveEventActionKind::Warning,
        },
        message: solve::SolveEventMessage {
            parts: vec![
                solve::SolveEventMessagePart::Text("first authored assertion captured ".to_owned()),
                solve::SolveEventMessagePart::Conversion {
                    value: message,
                    source: solve::SolveStringConversionSource::Integer,
                    format: solve::SolveStringConversionFormat::Options {
                        minimum_length: None,
                        left_justified: None,
                        significant_digits: None,
                    },
                },
            ],
        },
        span: span(953),
        origin: "source-positioned carrier control".to_owned(),
        clock_owner: None,
        assertion_projection: solve::SolveAssertionActionProjection::new(site.clone(), 1),
    }];
    problem.events.action_conditions = solve::ScalarProgramBlock::with_program_spans(
        vec![vec![
            solve::LinearOp::Const { dst: 0, value: 0.0 },
            solve::LinearOp::Const { dst: 1, value: 2.0 },
            solve::LinearOp::PureCallObservation {
                dst_start: 2,
                input_starts: vec![0, 1].into(),
                site: solve::SolveAssertionObservationSite::new(site).unwrap(),
            },
            solve::LinearOp::Unary {
                dst: 3,
                op: solve::UnaryOp::Not,
                arg: 2,
            },
            solve::LinearOp::StoreOutput { src: 3 },
        ]],
        vec![span(953)],
    )
    .unwrap();
    problem
}

const FORMATTED_CONTROL: &str = r#"{% from "fmi-typed-functions.jinja" import functions, function_prototypes %}
{% from "fmi-c-kernel.jinja" import message_support, observers %}
#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <inttypes.h>
#include <stddef.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define RMC_API
typedef struct { bool assertion_failed; bool rmc_warned[1]; } ModelInstance;
{{ function_prototypes(table) }}
{{ functions(table) }}
static size_t logs=0;
static void {{ "log_assertion" if level == "error" else "log_warning" }}(ModelInstance* model, const char* message) {
    (void)model;
    ++logs;
    assert(strcmp(message, "first authored assertion captured 9007199254740993")==0);
}
{{ message_support(rows, "fmi3", true) }}
{{ observers(observations) }}
int main(void) {
    ModelInstance model={0};
    double valid=0.0, index=2.0, output[3]={91.0, 92.0, 93.0}, predicate=99.0;
    const double* input[]={&valid, &index};
    RmcModelAssertionContext context={&model, 0, 1, NULL, NULL};
    RmcAssertionObserver observer={&context, rmc_model_assertion};
{% if level == "error" %}
    assert(rumoca_scalar_pure_0(input, output, &observer)==2);
    assert(logs==1 && model.assertion_failed && output[0]==91.0);
    model.assertion_failed=false; context.mode=0; context.predicates=&predicate;
    assert(rumoca_scalar_observe_0(input, &predicate, &observer)==2);
    assert(logs==1 && !model.assertion_failed && predicate==0.0);
    valid=1.0;
    assert(rumoca_scalar_observe_0(input, &predicate, &observer)==1);
    assert(logs==1 && !model.assertion_failed);
{% else %}
    context.mode=2;
    int status=rumoca_scalar_pure_0(input, output, &observer);
    assert(status==1 && context.notices && logs==0 && !model.rmc_warned[0]);
    rmc_finish_assertion_observation(&context, status);
    assert(!context.notices && logs==0 && !model.rmc_warned[0] && output[0]==91.0);
    index=1.0;
    status=rumoca_scalar_pure_0(input, output, &observer);
    assert(status==0 && context.notices && logs==0 && !model.rmc_warned[0]);
    rmc_finish_assertion_observation(&context, status);
    assert(!context.notices && logs==1 && model.rmc_warned[0] && output[0]==1.0);
    status=rumoca_scalar_pure_0(input, output, &observer);
    rmc_finish_assertion_observation(&context, status);
    assert(status==0 && logs==1 && !context.notices);
    context.mode=0; context.predicates=&predicate;
    assert(rumoca_scalar_observe_0(input, &predicate, &observer)==0);
    assert(logs==1 && predicate==0.0 && !model.assertion_failed);
{% endif %}
    return 0;
}
"#;

#[test]
fn compiled_c_formats_checked_first_fault_captures_without_replaying_value_rows() {
    formatted_control(solve::SolveAssertionLevel::Error);
}

#[test]
fn compiled_c_warning_reports_only_successful_observations_once() {
    formatted_control(solve::SolveAssertionLevel::Warning);
}

fn formatted_control(level: solve::SolveAssertionLevel) {
    let table = fault_table_at_level(level);
    let families = PureCallFamilies::new(&table).unwrap();
    let problem = observation_problem(&table);
    solve::validate_problem_pure_call_sites(&problem, &table).unwrap();
    let observations =
        super::super::super::fmi_c_assertions::observations(&problem, &table).unwrap();
    let mut environment = super::super::super::create_environment();
    environment
        .add_template("formatted_carrier", FORMATTED_CONTROL)
        .unwrap();
    let source = environment
        .get_template("formatted_carrier")
        .unwrap()
        .render(minijinja::context! {
            table => families.owners_value(), observations => observations,
            level => if level==solve::SolveAssertionLevel::Error { "error" } else { "warning" },
            rows => serde_json::json!({ "programs": [] }),
        })
        .unwrap();
    compile_and_execute(&source);
}

#[test]
fn observation_wire_rejects_foreign_scope_and_forged_predicate_slots() {
    let table = fault_table();
    let mut problem = observation_problem(&table);
    problem.events.dynamic_time_event_rhs = problem.events.action_conditions.clone();
    problem.events.dynamic_time_event_names = vec!["foreign observation scope".to_owned()];
    assert!(matches!(
        solve::validate_problem_pure_call_sites(&problem, &table),
        Err(
            solve::SolveProblemShapeContractError::PureCallSiteMismatch {
                context: "checked event-action assertion observation",
                ..
            }
        )
    ));
    let problem = observation_problem(&table);
    let mut wire = serde_json::to_value(&problem).unwrap();
    let encoded = wire.to_string();
    assert!(encoded.contains("PureCallObservation"), "{encoded}");
    fn forge(value: &mut Json) -> bool {
        match value {
            Json::Object(fields) => {
                if fields.contains_key("predicates") {
                    fields.insert("predicates".to_owned(), serde_json::json!([0]));
                    return true;
                }
                fields.values_mut().any(forge)
            }
            Json::Array(elements) => elements.iter_mut().any(forge),
            _ => false,
        }
    }
    assert!(forge(&mut wire));
    let replayed: solve::SolveProblem = serde_json::from_value(wire).unwrap();
    assert!(matches!(
        solve::validate_problem_pure_call_sites(&replayed, &table),
        Err(solve::SolveProblemShapeContractError::PureCallSiteMismatch { .. })
    ));
}

fn bounded_capture_table(minimum: i64, maximum: i64) -> solve::SolvePureCallTable {
    let domain = solve::SolveIntegerDomain::construct(minimum, maximum).unwrap();
    let profile =
        solve::SolveArithmeticProfile::construct(solve::SolveRealFormat::Binary64, domain);
    let boolean = solve::SolveValueType::scalar(solve::SolveScalarType::Boolean);
    let integer = solve::SolveValueType::scalar(solve::SolveScalarType::integer(profile));
    solve::SolvePureCallTable::construct(profile, |table| {
        table.add_owner(
            identity(960),
            vec![boolean.clone()],
            vec![
                solve::SolvePureCallOutput::result(boolean),
                solve::SolvePureCallOutput::assertion_predicate(),
                solve::SolvePureCallOutput::assertion_message_value(integer, 1),
            ],
            span(960),
            |builder, inputs, outputs| {
                let condition = builder.load(inputs[0], span(961))?;
                let assertion = builder.assertion_output(1, span(962))?;
                let captures = builder.check_assertion(
                    assertion,
                    condition,
                    &[],
                    span(963),
                    |region, _, outputs| {
                        let value = region.constant(
                            solve::SolveValue::integer(profile, minimum).unwrap(),
                            span(964),
                        )?;
                        region.store(outputs[0], value, span(965))
                    },
                )?;
                builder.store(outputs[0], condition, span(966))?;
                builder.store(outputs[1], condition, span(967))?;
                builder.store(outputs[2], captures[0], span(968))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn compiled_c_successful_check_uses_domain_valid_inactive_integer_captures() {
    for (minimum, maximum, expected) in [(2, 7, 2), (-9, -3, -3)] {
        let table = bounded_capture_table(minimum, maximum);
        let families = PureCallFamilies::new(&table).unwrap();
        let mut environment = super::super::super::create_environment();
        environment
            .add_template(
                "bounded_capture",
                r#"
{% from "fmi-typed-functions.jinja" import functions, function_prototypes %}
#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <math.h>
#define RMC_API
typedef struct ModelInstance ModelInstance;
{{ function_prototypes(table) }}
{{ functions(table) }}
int main(void) {
    double valid=1.0, result[3]={91.0, 92.0, 93.0};
    const double* inputs[]={&valid};
    assert(rumoca_scalar_pure_0(inputs, result, NULL)==0);
    assert(result[0]==1.0 && result[1]==1.0 && result[2]=={{ expected }});
    return 0;
}
"#,
            )
            .unwrap();
        let source = environment
            .get_template("bounded_capture")
            .unwrap()
            .render(minijinja::context! { table => families.owners_value(), expected => expected })
            .unwrap();
        compile_and_execute(&source);
    }
}
