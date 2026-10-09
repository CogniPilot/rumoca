#define _POSIX_C_SOURCE 200809L
#include <assert.h>
#include <inttypes.h>
#include <time.h>
#include "model.c"
#include "references.h"

struct Counts { uint64_t settle, refresh, ordinary, observation; };
static struct Counts counts;
static bool counting;
static unsigned messages;
static char last_message[256];
void __cyg_profile_func_enter(void*, void*) __attribute__((no_instrument_function));
void __cyg_profile_func_exit(void*, void*) __attribute__((no_instrument_function));
void __cyg_profile_func_enter(void* function, void* caller) {
    (void)caller;
    if (!counting) return;
    if (function == (void*)settle_public_values) ++counts.settle;
    if (function == (void*)refresh_algebraics) ++counts.refresh;
    if (function == (void*)rumoca_scalar_pure_0) ++counts.ordinary;
    if (function == (void*)rumoca_scalar_observe_0) ++counts.observation;
}
void __cyg_profile_func_exit(void* function, void* caller) { (void)function; (void)caller; }
static void logger(fmi3InstanceEnvironment env, fmi3Status status, const char* category, const char* message) {
    (void)env; (void)status; (void)category;
    ++messages;
    snprintf(last_message, sizeof last_message, "%s", message);
}
static void clear_counts(void) { counts = (struct Counts){0}; counting = true; }
static void emit(const char* name) {
    counting = false;
#ifdef INSTRUMENTED
    printf("{\"case\":\"%s\",\"instrumented\":true,\"settle\":%" PRIu64 ",\"refresh\":%" PRIu64 ",\"ordinary\":%" PRIu64 ",\"observation\":%" PRIu64 "}\n", name, counts.settle, counts.refresh, counts.ordinary, counts.observation);
#else
    printf("{\"case\":\"%s\",\"instrumented\":false,\"status\":\"checks-passed\"}\n", name);
#endif
}
static void expect_counts(uint64_t n) {
#ifdef INSTRUMENTED
    if (counts.settle != n || counts.refresh != n || counts.ordinary != n || counts.observation != n)
        fprintf(stderr, "expected %" PRIu64 ", actual settle=%" PRIu64 " refresh=%" PRIu64 " ordinary=%" PRIu64 " observation=%" PRIu64 "\n", n, counts.settle, counts.refresh, counts.ordinary, counts.observation);
    assert(counts.settle == n && counts.refresh == n);
    assert(counts.ordinary == n && counts.observation == n);
#else
    (void)n;
#endif
}
static void set_inputs(ModelInstance* m, double x, bool valid, int32_t index) {
    const fmi3ValueReference xr[] = {VR_X}, vr[] = {VR_VALID}, ir[] = {VR_INDEX};
    assert(fmi3SetFloat64(m, xr, 1, &x, 1) == fmi3OK);
    assert(fmi3SetBoolean(m, vr, 1, &valid, 1) == fmi3OK);
    assert(fmi3SetInt32(m, ir, 1, &index, 1) == fmi3OK);
}
static ModelInstance* fresh(double x, bool valid, int32_t index, fmi3Status expected) {
    ModelInstance* m = fmi3InstantiateCoSimulation("getter-control", TOKEN, NULL, false, true, false, false, NULL, 0, NULL, logger, NULL);
    assert(m);
    assert(fmi3EnterInitializationMode(m, false, 0, 0, true, 1) == fmi3OK);
    set_inputs(m, x, valid, index);
    messages = 0; last_message[0] = '\0';
    assert(fmi3ExitInitializationMode(m) == expected);
    return m;
}
static double expected_a(double x) {
    double total = 0;
    for (int k = 1; k <= 64; ++k) total = total + sin(x+k);
    return x + total;
}
static void values(double a, double b, double x) {
    assert(fabs(a-expected_a(x)) < 1e-12);
    assert(b == 2*a);
}
static void malformed(ModelInstance* m, const char* name, const fmi3ValueReference* refs, size_t nrefs, size_t nvalues, bool null_values) {
    ModelInstance before = *m;
    double output[3] = {-987, -987, -987};
    clear_counts();
    assert(fmi3GetFloat64(m, refs, nrefs, null_values ? NULL : output, nvalues) == fmi3Error);
    assert(memcmp(&before, m, sizeof before) == 0);
    assert(output[0] == -987 && output[1] == -987 && output[2] == -987);
    expect_counts(0); emit(name);
}
static void measure(ModelInstance* m, bool separate) {
    const fmi3ValueReference refs[] = {VR_A, VR_B};
    double output[2]; struct timespec begin, end;
    counting = false;
    assert(clock_gettime(CLOCK_MONOTONIC, &begin) == 0);
    for (int i=0; i<10000; ++i) {
        if (separate) {
            assert(fmi3GetFloat64(m, refs, 1, output, 1) == fmi3OK);
            assert(fmi3GetFloat64(m, refs+1, 1, output+1, 1) == fmi3OK);
        } else assert(fmi3GetFloat64(m, refs, 2, output, 2) == fmi3OK);
    }
    assert(clock_gettime(CLOCK_MONOTONIC, &end) == 0);
    values(output[0], output[1], 1.0);
    const double seconds = (double)(end.tv_sec-begin.tv_sec) + (double)(end.tv_nsec-begin.tv_nsec)/1e9;
    printf("{\"timing\":\"%s\",\"requests_per_iteration\":%d,\"iterations\":10000,\"seconds\":%.9f}\n", separate ? "separate" : "batch", separate ? 2 : 1, seconds);
}
int main(void) {
    const fmi3ValueReference refs[] = {VR_A, VR_B}, duplicate[] = {VR_A, VR_A};
    double output[2] = {-987, -987};
    ModelInstance* m = fresh(1, true, 1, fmi3OK);
    clear_counts(); assert(fmi3GetFloat64(m, refs, 2, output, 2) == fmi3OK);
    values(output[0], output[1], 1); expect_counts(1); emit("batch");
    const double batch_a=output[0], batch_b=output[1];
    fmi3FreeInstance(m); m=fresh(1, true, 1, fmi3OK);
    clear_counts(); assert(fmi3GetFloat64(m, refs, 1, output, 1) == fmi3OK);
    assert(fmi3GetFloat64(m, refs+1, 1, output+1, 1) == fmi3OK);
    values(output[0], output[1], 1); assert(output[0] == batch_a && output[1] == batch_b); expect_counts(2); emit("separate");
    clear_counts(); assert(fmi3GetFloat64(m, duplicate, 2, output, 2) == fmi3OK);
    assert(output[0] == output[1]); expect_counts(1); emit("duplicate");
    clear_counts(); assert(fmi3GetFloat64(m, NULL, 0, NULL, 0) == fmi3OK);
    expect_counts(0); emit("empty");
    const fmi3ValueReference mixed[] = {VR_TIME, VR_A, VR_INDICATOR, VR_B};
    double mixed_values[4], separate_values[4];
    clear_counts(); assert(fmi3GetFloat64(m, mixed, 4, mixed_values, 4) == fmi3OK);
#ifdef INSTRUMENTED
    assert(counts.settle == 3);
#endif
    emit("mixed-time-output-indicator-output");
    for (size_t k=0; k<4; ++k) assert(fmi3GetFloat64(m, mixed+k, 1, separate_values+k, 1) == fmi3OK);
    assert(memcmp(mixed_values, separate_values, sizeof mixed_values) == 0);
    assert(isfinite(mixed_values[2]) && mixed_values[0] == 0);
#ifdef HAS_STATE
    const fmi3ValueReference with_derivative[] = {VR_TIME, VR_A, VR_DERIVATIVE, VR_B};
    clear_counts(); assert(fmi3GetFloat64(m, with_derivative, 4, mixed_values, 4) == fmi3OK);
#ifdef INSTRUMENTED
    assert(counts.settle == 3);
#endif
    emit("mixed-time-output-derivative-output");
    for (size_t k=0; k<4; ++k) assert(fmi3GetFloat64(m, with_derivative+k, 1, separate_values+k, 1) == fmi3OK);
    assert(memcmp(mixed_values, separate_values, sizeof mixed_values) == 0);
    assert(mixed_values[2] == 1);
#endif
    const fmi3ValueReference bool_refs[] = {VR_VALID, VR_VALID}, int_refs[] = {VR_INDEX, VR_INDEX};
    bool bool_values[2] = {false, false}; int32_t int_values[2] = {0, 0};
    clear_counts(); assert(fmi3GetBoolean(m, bool_refs, 2, bool_values, 2) == fmi3OK);
    assert(bool_values[0] && bool_values[1]); expect_counts(1); emit("boolean-duplicate");
    clear_counts(); assert(fmi3GetInt32(m, int_refs, 2, int_values, 2) == fmi3OK);
    assert(int_values[0] == 1 && int_values[1] == 1); expect_counts(1); emit("integer-duplicate");
    const fmi3ValueReference wrong[] = {VR_VALID}, unknown[] = {999};
    malformed(m, "wrong-type", wrong, 1, 1, false);
    malformed(m, "unknown-vr", unknown, 1, 1, false);
    const fmi3ValueReference late_wrong[] = {VR_A, VR_VALID}, late_unknown[] = {VR_A, 999};
    malformed(m, "later-wrong-type", late_wrong, 2, 2, false);
    malformed(m, "later-unknown-vr", late_unknown, 2, 2, false);
    malformed(m, "empty-with-extra-value", NULL, 0, 1, false);
    malformed(m, "short-count", refs, 2, 1, false);
    malformed(m, "long-count", refs, 2, 3, false);
    malformed(m, "null-refs", NULL, 1, 1, false);
    malformed(m, "null-values", refs, 1, 1, true);
    set_inputs(m, 2, true, 1);
    clear_counts(); assert(fmi3GetFloat64(m, refs, 2, output, 2) == fmi3OK);
    values(output[0], output[1], 2); expect_counts(1); emit("input-mutation");
    bool event=false, terminated=false, early=false; double last=-1;
    assert(fmi3DoStep(m, 0, 0.01, false, &event, &terminated, &early, &last) == fmi3OK);
    assert(last == 0.01 && !event && !terminated && !early);
    assert(fmi3GetFloat64(m, refs, 2, output, 2) == fmi3OK);
    values(output[0], output[1], 2); assert(messages == 0);
    puts("{\"case\":\"scalar-event-profile-step\",\"status\":\"OK\",\"last_time\":0.01}");
    set_inputs(m, 2, false, 2);
    ModelInstance step_before = *m;
    assert(fmi3DoStep(m, 0.01, 0.01, false, &event, &terminated, &early, &last) == fmi3Error);
    assert(m->time == 0.01 && memcmp(step_before.y, m->y, sizeof step_before.y) == 0);
    assert(messages == 1 && strcmp(last_message, "first authored getter fault") == 0);
    puts("{\"case\":\"discrete-input-event-first-fatal-rollback\",\"status\":\"Error\",\"time\":0.01}");
    fmi3FreeInstance(m);
    m = fresh(1, true, 1, fmi3OK);
    set_inputs(m, 1, false, 2);
    ModelInstance getter_before = *m;
    output[0] = output[1] = -987;
    clear_counts(); assert(fmi3GetFloat64(m, refs, 2, output, 2) == fmi3Error);
    assert(output[0] == -987 && output[1] == -987);
    assert(memcmp(getter_before.y, m->y, sizeof getter_before.y) == 0);
    assert(messages == 1 && strcmp(last_message, "first authored getter fault") == 0);
#ifdef INSTRUMENTED
    assert(counts.settle == 1 && counts.refresh == 1 && counts.ordinary == 1 && counts.observation == 0);
#endif
    emit("getter-first-fault-no-publication");
    fmi3FreeInstance(m);
    m = fresh(1, false, 2, fmi3Error);
    assert(messages == 1 && strcmp(last_message, "first authored getter fault") == 0);
    ModelInstance before = *m; output[0] = output[1] = -987;
    clear_counts(); assert(fmi3GetFloat64(m, refs, 2, output, 2) == fmi3Error);
    assert(output[0] == -987 && output[1] == -987);
    assert(memcmp(before.y, m->y, sizeof before.y) == 0 && messages == 1);
#ifdef INSTRUMENTED
    assert(counts.settle == 1 && counts.refresh == 0 && counts.ordinary == 0 && counts.observation == 0);
#endif
    emit("latched-fatal-no-publication-or-replay");
    clear_counts(); assert(fmi3GetFloat64(m, NULL, 0, NULL, 0) == fmi3OK);
    assert(messages == 1); expect_counts(0); emit("empty-after-fatal");
    malformed(m, "malformed-after-fatal", unknown, 1, 1, false);
    fmi3FreeInstance(m);
    m = fresh(1, true, 2, fmi3Error);
    output[0] = output[1] = -987;
    assert(messages == 0);
    assert(fmi3GetFloat64(m, refs, 2, output, 2) == fmi3Error);
    assert(output[0] == -987 && output[1] == -987 && messages == 0);
    puts("{\"case\":\"bounds-only-refusal\",\"status\":\"Error\",\"ordinary_published\":false}");
    fmi3FreeInstance(m);
    m = fresh(1, true, 1, fmi3OK);
    assert(fmi3GetFloat64(m, refs, 2, output, 2) == fmi3OK);
#ifndef INSTRUMENTED
    measure(m, false); measure(m, true);
#else
    (void)measure;
#endif
    fmi3FreeInstance(m);
    puts("OK complete generated-C getter controls");
    return 0;
}
