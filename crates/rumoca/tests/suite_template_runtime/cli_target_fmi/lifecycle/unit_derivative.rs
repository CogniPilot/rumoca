//! FMI 3 ME+CS oracle for a scalar state with a constant derivative.

use std::collections::BTreeSet;
use std::path::Path;

use super::fmi3_description::{
    BuildDescription, Description, PreprocessorDefinition, c_string_contents,
};

pub(super) fn validate(fmu_root: &Path, standard: &Path, model_xml: &str, build_xml: &str) {
    let description = Description::parse(model_xml);
    let build_description = BuildDescription::parse(build_xml);
    assert_eq!(description.fmi_version, "3.0");
    assert_eq!(description.model_name, "UnitDerivative");
    assert_ne!(description.instantiation_token, "wrong");
    assert_capabilities(&description);
    let definition = assert_build_description(&description, &build_description);
    assert_variable_inventory(&description);

    let driver = driver(&description);
    super::compile_and_run_with_definitions(
        fmu_root,
        standard,
        &driver,
        "FMI 3 UnitDerivative lifecycle driver",
        &[(definition.name.as_str(), definition.value.as_str())],
    );
}

fn assert_capabilities(description: &Description) {
    assert_eq!(description.scheduled_execution_count, 0);
    assert_eq!(description.model_exchange.attributes.len(), 1);
    assert_eq!(
        description.model_exchange.attributes.get("modelIdentifier"),
        Some(&description.model_identifier)
    );
    assert!(
        !description
            .model_exchange
            .boolean_or_false("needsCompletedIntegratorStep"),
        "event-free ME uses the FMI 3 default needsCompletedIntegratorStep=false"
    );
    assert_eq!(description.co_simulation.attributes.len(), 2);
    assert_eq!(
        description.co_simulation.attributes.get("modelIdentifier"),
        Some(&description.model_identifier)
    );
    assert!(
        description
            .co_simulation
            .boolean_or_false("canHandleVariableCommunicationStepSize")
    );
    assert!(
        !description
            .co_simulation
            .boolean_or_false("providesIntermediateUpdate")
    );
    assert!(
        !description
            .co_simulation
            .boolean_or_false("canReturnEarlyAfterIntermediateUpdate")
    );
    assert!(!description.co_simulation.boolean_or_false("hasEventMode"));
}

fn assert_build_description<'a>(
    description: &Description,
    build: &'a BuildDescription,
) -> &'a PreprocessorDefinition {
    assert_eq!(build.fmi_version, "3.0");
    assert_eq!(
        build.model_identifiers,
        std::slice::from_ref(&description.model_identifier)
    );
    assert_eq!(build.source_file_sets.len(), 1);
    let source_file_set = &build.source_file_sets[0];
    assert_eq!(source_file_set.language, "C99");
    assert_eq!(source_file_set.source_files, ["model.c"]);
    assert_eq!(source_file_set.preprocessor_definitions.len(), 1);
    let definition = &source_file_set.preprocessor_definitions[0];
    assert_eq!(definition.name, "FMI3_OVERRIDE_FUNCTION_PREFIX");
    assert_eq!(definition.value, "1");
    definition
}

fn assert_variable_inventory(description: &Description) {
    let inventory = description
        .variables
        .iter()
        .map(|variable| (variable.name.as_str(), variable.value_reference))
        .collect::<Vec<_>>();
    assert_eq!(inventory, [("time", 0), ("x", 1), ("der(x)", 2)]);
    assert!(
        description.unexpected_variable_kinds.is_empty(),
        "UnitDerivative publishes only Float64 variables"
    );
    assert_eq!(description.dimension_count, 0);
    let names = description
        .variables
        .iter()
        .map(|variable| variable.name.as_str())
        .collect::<BTreeSet<_>>();
    let value_references = description
        .variables
        .iter()
        .map(|variable| variable.value_reference)
        .collect::<BTreeSet<_>>();
    assert_eq!(names.len(), description.variables.len());
    assert_eq!(value_references.len(), description.variables.len());

    let time = description.variable("time");
    assert_eq!(time.causality, "independent");
    assert_eq!(time.variability, "continuous");
    assert_eq!(time.initial, None);
    assert_eq!(time.start, None);
    assert_eq!(time.derivative, None);
    assert_eq!(time.unit.as_deref(), Some("s"));
    let state = description.variable("x");
    assert_eq!(
        state.value_reference, 1,
        "x has the expected value reference"
    );
    assert_eq!(state.causality, "local");
    assert_eq!(state.variability, "continuous");
    assert_eq!(state.initial.as_deref(), Some("exact"));
    assert_eq!(state.start.as_deref(), Some(&[2.0][..]));
    assert_eq!(state.derivative, None);
    assert!(!state.reinit, "x uses the FMI 3 default reinit=false");
    assert_eq!(state.unit, None);

    let derivative = description.variable("der(x)");
    assert_eq!(derivative.causality, "local");
    assert_eq!(derivative.variability, "continuous");
    assert_eq!(derivative.initial.as_deref(), Some("calculated"));
    assert_eq!(derivative.start, None);
    assert_eq!(derivative.unit, None);
    assert_eq!(
        derivative.derivative,
        Some(state.value_reference),
        "der(x) links to x by value reference"
    );
    assert_eq!(
        description.continuous_state_derivatives,
        [2],
        "ModelStructure names der(x) as the continuous-state derivative"
    );
    assert_eq!(
        description.initial_unknowns,
        [2],
        "ModelStructure names der(x) as the sole initial unknown"
    );
    assert!(
        description.unexpected_model_structure_children.is_empty(),
        "UnitDerivative ModelStructure contains only the exact derivative and initial-unknown inventories"
    );
}

fn driver(description: &Description) -> String {
    DRIVER
        .replace("{identifier}", &description.model_identifier)
        .replace(
            "{token}",
            &c_string_contents(&description.instantiation_token),
        )
        .replace(
            "{state_vr}",
            &description.variable("x").value_reference.to_string(),
        )
        .replace(
            "{derivative_vr}",
            &description.variable("der(x)").value_reference.to_string(),
        )
        .replace("{{", "{")
        .replace("}}", "}")
}

const DRIVER: &str = r#"
#include <math.h>
#include <stddef.h>

#define FMI3_FUNCTION_PREFIX {identifier}_
#include "fmi3Functions.h"

#define CHECK(condition) do {{ if (!(condition)) return __LINE__; }} while (0)

static int check_x(fmi3Instance instance, fmi3Float64 expected) {{
    const fmi3ValueReference vr = {state_vr};
    fmi3Float64 value = 0.0;
    CHECK(fmi3GetFloat64(instance, &vr, 1, &value, 1) == fmi3OK);
    CHECK(value == expected);
    return 0;
}}

static int set_x(fmi3Instance instance, fmi3Float64 value) {{
    const fmi3ValueReference vr = {state_vr};
    CHECK(fmi3SetFloat64(instance, &vr, 1, &value, 1) == fmi3OK);
    CHECK(check_x(instance, value) == 0);
    return 0;
}}

static int settle_me_event(fmi3Instance instance) {{
    fmi3Boolean discrete_states_needed = fmi3True;
    fmi3Boolean terminate = fmi3True;
    fmi3Boolean nominals_changed = fmi3True;
    fmi3Boolean values_changed = fmi3True;
    fmi3Boolean next_event_time_defined = fmi3True;
    fmi3Float64 next_event_time = 1.0;
    CHECK(fmi3UpdateDiscreteStates(instance, &discrete_states_needed, &terminate,
        &nominals_changed, &values_changed, &next_event_time_defined,
        &next_event_time) == fmi3OK);
    CHECK(!discrete_states_needed && !terminate && !nominals_changed &&
        !values_changed && !next_event_time_defined);
    return 0;
}}

static int enter_me_event(const char *name, fmi3Instance *instance) {{
    *instance = fmi3InstantiateModelExchange(
        name, "{token}", NULL, fmi3False, fmi3False, NULL, NULL);
    CHECK(*instance != NULL);
    CHECK(fmi3EnterInitializationMode(
        *instance, fmi3False, 0.0, 0.0, fmi3True, 1.0) == fmi3OK);
    CHECK(fmi3ExitInitializationMode(*instance) == fmi3OK);
    return 0;
}}

static int enter_me_continuous(const char *name, fmi3Instance *instance) {{
    CHECK(enter_me_event(name, instance) == 0);
    CHECK(settle_me_event(*instance) == 0);
    CHECK(fmi3EnterContinuousTimeMode(*instance) == fmi3OK);
    return 0;
}}

static int enter_cs_step(const char *name, fmi3Instance *instance) {{
    *instance = fmi3InstantiateCoSimulation(
        name, "{token}", NULL, fmi3False, fmi3False, fmi3False, fmi3False,
        NULL, 0, NULL, NULL, NULL);
    CHECK(*instance != NULL);
    CHECK(fmi3EnterInitializationMode(
        *instance, fmi3False, 0.0, 0.0, fmi3True, 1.0) == fmi3OK);
    CHECK(fmi3ExitInitializationMode(*instance) == fmi3OK);
    return 0;
}}

static int rejected_float64_write_preserves_x(
    const char *name,
    const fmi3ValueReference vr[],
    size_t nvr,
    const fmi3Float64 values[],
    size_t nvalues) {{
    fmi3Instance instance = NULL;
    CHECK(enter_me_continuous(name, &instance) == 0);
    CHECK(set_x(instance, 4.0) == 0);
    CHECK(fmi3SetFloat64(instance, vr, nvr, values, nvalues) == fmi3Error);
    CHECK(check_x(instance, 4.0) == 0);
    fmi3FreeInstance(instance);
    return 0;
}}

static int check_rejected_float64_batches(void) {{
    const fmi3ValueReference state = {state_vr};
    const fmi3ValueReference invalid = 99;
    const fmi3ValueReference trailing_invalid[] = {{state, invalid}};
    const fmi3ValueReference mixed[] = {{state, {derivative_vr}}};
    const fmi3Float64 one[] = {{5.0}};
    const fmi3Float64 two[] = {{5.0, 6.0}};
    const fmi3Float64 nonfinite[] = {{NAN}};
    CHECK(rejected_float64_write_preserves_x(
        "under-width", &state, 1, one, 0) == 0);
    CHECK(rejected_float64_write_preserves_x(
        "over-width", &state, 1, two, 2) == 0);
    CHECK(rejected_float64_write_preserves_x(
        "invalid-vr", &invalid, 1, one, 1) == 0);
    CHECK(rejected_float64_write_preserves_x(
        "trailing-invalid-vr", trailing_invalid, 2, two, 2) == 0);
    CHECK(rejected_float64_write_preserves_x(
        "mixed-read-only", mixed, 2, two, 2) == 0);
    CHECK(rejected_float64_write_preserves_x(
        "nonfinite", &state, 1, nonfinite, 1) == 0);
    return 0;
}}

static int check_mode_specific_refusals(void) {{
    fmi3Instance instance = NULL;
    fmi3Float64 value = 5.0;
    const fmi3ValueReference state = {state_vr};

    CHECK(enter_me_event("me-event-continuous-vector", &instance) == 0);
    CHECK(set_x(instance, 4.0) == 0);
    CHECK(fmi3SetContinuousStates(instance, &value, 1) == fmi3Error);
    CHECK(check_x(instance, 4.0) == 0);
    fmi3FreeInstance(instance);

    CHECK(enter_cs_step("cs-step-ordinary-write", &instance) == 0);
    CHECK(fmi3SetFloat64(instance, &state, 1, &value, 1) == fmi3Error);
    CHECK(check_x(instance, 2.0) == 0);
    fmi3FreeInstance(instance);

    CHECK(enter_cs_step("cs-step-continuous-vector", &instance) == 0);
    CHECK(fmi3SetContinuousStates(instance, &value, 1) == fmi3Error);
    CHECK(check_x(instance, 2.0) == 0);
    fmi3FreeInstance(instance);

    CHECK(enter_cs_step("cs-step-event-mode", &instance) == 0);
    CHECK(fmi3EnterEventMode(instance) == fmi3Error);
    CHECK(check_x(instance, 2.0) == 0);
    fmi3FreeInstance(instance);
    return 0;
}}

static int check_rejected_step_is_atomic(void) {{
    fmi3Instance instance = NULL;
    CHECK(enter_cs_step("cs-rejected-step", &instance) == 0);
    fmi3Boolean event_needed = fmi3True;
    fmi3Boolean terminate = fmi3True;
    fmi3Boolean early_return = fmi3True;
    fmi3Float64 last_time = -1.0;
    CHECK(fmi3DoStep(instance, 0.25, 0.25, fmi3True, &event_needed, &terminate,
        &early_return, &last_time) == fmi3Error);
    CHECK(event_needed && terminate && early_return && last_time == -1.0);
    CHECK(check_x(instance, 2.0) == 0);
    fmi3FreeInstance(instance);
    return 0;
}}

int main(void) {{
    CHECK(fmi3InstantiateModelExchange(
        "bad-me", "wrong", NULL, fmi3False, fmi3False, NULL, NULL) == NULL);
    CHECK(fmi3InstantiateCoSimulation(
        "bad-cs", "wrong", NULL, fmi3False, fmi3False, fmi3False, fmi3False,
        NULL, 0, NULL, NULL, NULL) == NULL);
    CHECK(fmi3InstantiateCoSimulation(
        "bad-cs-event-mode", "{token}", NULL, fmi3False, fmi3False,
        fmi3True, fmi3False, NULL, 0, NULL, NULL, NULL) == NULL);
    const fmi3ValueReference required_intermediate = {state_vr};
    CHECK(fmi3InstantiateCoSimulation(
        "bad-cs-intermediate", "{token}", NULL, fmi3False, fmi3False,
        fmi3False, fmi3False, &required_intermediate, 1,
        NULL, NULL, NULL) == NULL);

    CHECK(check_rejected_float64_batches() == 0);
    CHECK(check_mode_specific_refusals() == 0);
    CHECK(check_rejected_step_is_atomic() == 0);

    fmi3Instance me = fmi3InstantiateModelExchange(
        "me", "{token}", NULL, fmi3False, fmi3False, NULL, NULL);
    CHECK(me != NULL);
    CHECK(check_x(me, 2.0) == 0);
    CHECK(set_x(me, 2.25) == 0);
    CHECK(set_x(me, 2.0) == 0);
    CHECK(fmi3EnterInitializationMode(
        me, fmi3False, 0.0, 0.0, fmi3True, 1.0) == fmi3OK);
    CHECK(check_x(me, 2.0) == 0);
    CHECK(set_x(me, 2.5) == 0);
    CHECK(set_x(me, 2.0) == 0);
    CHECK(fmi3ExitInitializationMode(me) == fmi3OK);
    CHECK(check_x(me, 2.0) == 0);
    CHECK(set_x(me, 2.75) == 0);
    CHECK(set_x(me, 2.0) == 0);
    CHECK(settle_me_event(me) == 0);
    CHECK(fmi3EnterContinuousTimeMode(me) == fmi3OK);
    size_t continuous_state_count = 0;
    size_t event_indicator_count = 1;
    CHECK(fmi3GetNumberOfContinuousStates(me, &continuous_state_count) == fmi3OK);
    CHECK(continuous_state_count == 1);
    CHECK(fmi3GetNumberOfEventIndicators(me, &event_indicator_count) == fmi3OK);
    CHECK(event_indicator_count == 0);
    fmi3Float64 nominal = NAN;
    CHECK(fmi3GetNominalsOfContinuousStates(me, &nominal, 1) == fmi3OK);
    CHECK(nominal == 1.0);
    fmi3Float64 continuous_state = 0.0;
    CHECK(fmi3GetContinuousStates(me, &continuous_state, 1) == fmi3OK);
    CHECK(continuous_state == 2.0);
    CHECK(fmi3SetTime(me, 1.0) == fmi3OK);
    CHECK(set_x(me, 3.0) == 0);
    continuous_state = 3.25;
    CHECK(fmi3SetContinuousStates(me, &continuous_state, 1) == fmi3OK);
    continuous_state = 0.0;
    CHECK(fmi3GetContinuousStates(me, &continuous_state, 1) == fmi3OK);
    CHECK(continuous_state == 3.25);
    CHECK(check_x(me, 3.25) == 0);
    fmi3Float64 derivative = 0.0;
    CHECK(fmi3GetContinuousStateDerivatives(me, &derivative, 1) == fmi3OK);
    CHECK(derivative == 1.0);
    CHECK(fmi3Terminate(me) == fmi3OK);
    const fmi3ValueReference state = {state_vr};
    fmi3Float64 rejected_value = 9.0;
    CHECK(fmi3SetFloat64(me, &state, 1, &rejected_value, 1) == fmi3Error);
    CHECK(check_x(me, 3.25) == 0);
    fmi3FreeInstance(me);

    fmi3Instance cs = fmi3InstantiateCoSimulation(
        "cs", "{token}", NULL, fmi3False, fmi3False, fmi3False, fmi3True,
        NULL, 0, NULL, NULL, NULL);
    CHECK(cs != NULL);
    CHECK(check_x(cs, 2.0) == 0);
    CHECK(fmi3EnterInitializationMode(
        cs, fmi3False, 0.0, 0.0, fmi3True, 1.0) == fmi3OK);
    CHECK(check_x(cs, 2.0) == 0);
    CHECK(fmi3ExitInitializationMode(cs) == fmi3OK);
    CHECK(check_x(cs, 2.0) == 0);
    fmi3Boolean event_needed = fmi3True;
    fmi3Boolean terminate = fmi3True;
    fmi3Boolean early_return = fmi3True;
    fmi3Float64 last_time = 0.0;
    CHECK(fmi3DoStep(cs, 0.0, 0.25, fmi3True, &event_needed, &terminate,
        &early_return, &last_time) == fmi3OK);
    CHECK(!event_needed && !terminate && !early_return);
    CHECK(last_time == 0.25);
    CHECK(check_x(cs, 2.25) == 0);
    event_needed = terminate = early_return = fmi3True;
    CHECK(fmi3DoStep(cs, 0.25, 0.25, fmi3True, &event_needed, &terminate,
        &early_return, &last_time) == fmi3OK);
    CHECK(!event_needed && !terminate && !early_return);
    CHECK(last_time == 0.5);
    CHECK(check_x(cs, 2.5) == 0);
    event_needed = terminate = early_return = fmi3True;
    CHECK(fmi3DoStep(cs, 0.5, 0.5, fmi3True, &event_needed, &terminate,
        &early_return, &last_time) == fmi3OK);
    CHECK(!event_needed && !terminate && !early_return);
    CHECK(last_time == 1.0);
    CHECK(check_x(cs, 3.0) == 0);
    CHECK(fmi3Terminate(cs) == fmi3OK);
    CHECK(fmi3SetFloat64(cs, &state, 1, &rejected_value, 1) == fmi3Error);
    CHECK(check_x(cs, 3.0) == 0);
    fmi3FreeInstance(cs);
    return 0;
}}
"#;
