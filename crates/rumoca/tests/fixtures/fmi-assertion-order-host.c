#include "model.h"
#include <assert.h>

static unsigned logs;
static const char* expected_message;
static void logger(fmi3InstanceEnvironment environment, fmi3Status status,
                   fmi3String category, fmi3String message) {
    (void)environment;
    assert(status == fmi3Error);
    assert(strcmp(category, "assertion") == 0);
    assert(strcmp(message, expected_message) == 0);
    ++logs;
}

int main(int argc, char** argv) {
    assert(argc == 6);
    expected_message = argv[5];
    const fmi3ValueReference valid_ref = (fmi3ValueReference)strtoul(argv[2], NULL, 10);
    const fmi3ValueReference index_ref = (fmi3ValueReference)strtoul(argv[3], NULL, 10);
    const fmi3ValueReference result_ref = (fmi3ValueReference)strtoul(argv[4], NULL, 10);
    fmi3Instance instance = fmi3InstantiateCoSimulation(
        "native-assertion-order", argv[1], "", false, true, false, false,
        NULL, 0, NULL, logger, NULL);
    assert(instance);
    for (int condition = 0; condition <= 1; ++condition) {
        for (fmi3Int32 index = 1; index <= 2; ++index) {
            const fmi3Boolean valid = condition != 0;
            const fmi3Status expected = valid && index == 1 ? fmi3OK : fmi3Error;
            logs = 0;
            assert(fmi3Reset(instance) == fmi3OK);
            assert(fmi3EnterInitializationMode(instance, false, 0, 0, true, 1) == fmi3OK);
            assert(fmi3SetBoolean(instance, &valid_ref, 1, &valid, 1) == fmi3OK);
            assert(fmi3SetInt32(instance, &index_ref, 1, &index, 1) == fmi3OK);
            assert(fmi3ExitInitializationMode(instance) == expected);
            assert(logs == (valid ? 0U : 1U));
            fmi3Float64 output = 123.0;
            assert(fmi3GetFloat64(instance, &result_ref, 1, &output, 1) == expected);
            assert(output == (expected == fmi3OK ? 1.0 : 123.0));
            assert(logs == (valid ? 0U : 1U));
        }
    }
    fmi3FreeInstance(instance);
    puts("OK native first authored fault, bounds control, atomic ordinary result, reset");
    return 0;
}
