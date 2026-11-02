#include <float.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int fail_after = -1;
static void* controlled_calloc(size_t count, size_t size) {
    if (fail_after == 0) return NULL;
    if (fail_after > 0) --fail_after;
    return calloc(count, size);
}
#define calloc controlled_calloc
#include "model.c"
#undef calloc

#if defined(FMI2_FUNCTION_PREFIX)
typedef fmi2ValueReference Reference;
typedef fmi2Component Instance;
typedef fmi2Status Status;
#define OK fmi2OK
#define ERROR fmi2Error
#define FREE_INSTANCE fmi2FreeInstance
#define RESET fmi2Reset
#define ERROR_STATE MODEL_ERROR
#else
typedef fmi3ValueReference Reference;
typedef fmi3Instance Instance;
typedef fmi3Status Status;
#define OK fmi3OK
#define ERROR fmi3Error
#define FREE_INSTANCE fmi3FreeInstance
#define RESET fmi3Reset
#define ERROR_STATE MODEL_TERMINATED
#endif
#include "references.h"

#define CHECK(c) do { if (!(c)) { fprintf(stderr, "derivative check failed at line %d\n", __LINE__); exit(1); } } while (0)

static Status forward(Instance instance, const Reference* unknown, size_t nu,
                      const Reference* known, size_t nk, const double* input,
                      size_t ni, double* output, size_t no) {
#if FMI_VERSION == 2
    (void)ni; (void)no;
    return fmi2GetDirectionalDerivative(instance, unknown, nu, known, nk, input, output);
#else
    return fmi3GetDirectionalDerivative(instance, unknown, nu, known, nk, input, ni, output, no);
#endif
}

static void initialize(Instance instance, int cs, int phase) {
#if FMI_VERSION == 2
    CHECK(fmi2SetupExperiment(instance, 0, 0, 0, 0, 0) == OK);
    CHECK(fmi2EnterInitializationMode(instance) == OK);
    if (phase == 0) return;
    CHECK(fmi2ExitInitializationMode(instance) == OK);
    if (!cs) {
        fmi2EventInfo event;
        CHECK(fmi2NewDiscreteStates(instance, &event) == OK);
        CHECK(!event.newDiscreteStatesNeeded && !event.terminateSimulation);
        if (phase == 1) CHECK(fmi2EnterContinuousTimeMode(instance) == OK);
    }
#else
    CHECK(fmi3EnterInitializationMode(instance, 0, 0, 0, 0, 0) == OK);
    if (phase == 0) return;
    CHECK(fmi3ExitInitializationMode(instance) == OK);
    if (!cs) {
        fmi3Boolean needed, terminate, nominals, values, next;
        double next_time;
        CHECK(fmi3UpdateDiscreteStates(instance, &needed, &terminate,
            &nominals, &values, &next, &next_time) == OK);
        CHECK(!needed && !terminate);
        if (phase == 1) CHECK(fmi3EnterContinuousTimeMode(instance) == OK);
    }
#endif
}

static Instance instance(int phase) {
    int cs = phase == 3;
    fail_after = -1;
#if FMI_VERSION == 2
    fmi2CallbackFunctions callbacks = {NULL, controlled_calloc, free, NULL, NULL};
    Instance result = fmi2Instantiate("derivative-api", cs ? fmi2CoSimulation : fmi2ModelExchange,
                                     TOKEN, NULL, &callbacks, 0, 0);
#else
    Instance result = cs
        ? fmi3InstantiateCoSimulation("derivative-api", TOKEN, NULL, 0, 0, 0, 0, NULL, 0, NULL, NULL, NULL)
        : fmi3InstantiateModelExchange("derivative-api", TOKEN, NULL, 0, 0, NULL, NULL);
#endif
    CHECK(result != NULL);
    initialize(result, cs, phase);
    return result;
}

static int close_values(const double* actual, const double* wanted, size_t count) {
    for (size_t i = 0; i < count; ++i)
        if (!isfinite(actual[i]) || fabs(actual[i] - wanted[i]) > 1e-10) return 0;
    return 1;
}

static int primal_unchanged(Instance instance, const ModelInstance* before) {
    const ModelInstance* after = instance;
    return memcmp(after->y, before->y, sizeof after->y) == 0
        && memcmp(after->p, before->p, sizeof after->p) == 0
        && memcmp(&after->time, &before->time, sizeof after->time) == 0;
}
