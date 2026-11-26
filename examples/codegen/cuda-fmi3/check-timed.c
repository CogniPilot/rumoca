/* Hardware regression for TimedCrossing.mo. Link against the same FMU sources. */
#include <cuda.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "model.h"

#define GPU(call) do { \
    CUresult result = (call); \
    if (result != CUDA_SUCCESS) { \
        const char *message = 0; \
        cuGetErrorString(result, &message); \
        fprintf(stderr, "%s: %s\n", #call, message); \
        exit(1); \
    } \
} while (0)

static void check_fmi(fmi3Status status) {
    if (status > fmi3Warning) {
        fprintf(stderr, "FMI status %d\n", status);
        exit(1);
    }
}

static void bind_global(CUmodule module, const char *name, const void *value, size_t size) {
    CUdeviceptr address;
    size_t exported;
    GPU(cuModuleGetGlobal(&address, &exported, module, name));
    if (size != exported) exit(2);
    GPU(cuMemcpyHtoD(address, value, size));
}

int main(int argc, char **argv) {
    if (argc != 3) {
        fprintf(stderr, "Usage: check-timed ENSEMBLE.cubin INSTANTIATION_TOKEN\n");
        return 2;
    }
    enum { N = 2 };
    int count = N;
    const size_t stride = 64 * 1024;
    const fmi3ValueReference rate_ref = 1, observed[] = {2, 3, 4};
    ModelInstance *cpu[N], copy[N];
    CUdevice device;
    CUcontext context;
    CUmodule module;
    CUfunction step;
    CUdeviceptr arena, instances, results;
    GPU(cuInit(0));
    GPU(cuDeviceGet(&device, 0));
    GPU(cuCtxCreate_v2(&context, 0, device));
    GPU(cuModuleLoad(&module, argv[1]));
    GPU(cuModuleGetFunction(&step, module, "rmc_fmi3_step"));
    CUdeviceptr token_address, size_address;
    size_t token_length, size_length;
    unsigned long long device_size;
    GPU(cuModuleGetGlobal(&size_address, &size_length, module, "rmc_fmi3_instance_bytes"));
    if (size_length != sizeof device_size) return 2;
    GPU(cuMemcpyDtoH(&device_size, size_address, sizeof device_size));
    if (device_size != sizeof(ModelInstance)) return 2;
    GPU(cuModuleGetGlobal(&token_address, &token_length, module, "rmc_fmi3_token"));
    if (token_length != strlen(argv[2]) + 1) return 2;
    char *token = malloc(token_length);
    if (!token) return 2;
    GPU(cuMemcpyDtoH(token, token_address, token_length));
    if (memcmp(token, argv[2], token_length) != 0) return 2;
    free(token);
    GPU(cuMemAlloc(&arena, count * stride));
    GPU(cuMemsetD8(arena, 0, count * stride));
    bind_global(module, "rmc_scratch", &arena, sizeof arena);
    bind_global(module, "rmc_stride", &stride, sizeof stride);
    GPU(cuMemAlloc(&instances, sizeof copy));
    GPU(cuMemAlloc(&results, sizeof(int) * count));
    for (int lane = 0; lane < count; ++lane) {
        cpu[lane] = fmi3InstantiateCoSimulation("clock", argv[2], NULL, false,
            false, false, false, NULL, 0, NULL, NULL, NULL);
        if (!cpu[lane]) return 2;
        check_fmi(fmi3EnterInitializationMode(cpu[lane], false, 0, 0, false, 0));
        double rate = lane + 1;
        check_fmi(fmi3SetFloat64(cpu[lane], &rate_ref, 1, &rate, 1));
        check_fmi(fmi3ExitInitializationMode(cpu[lane]));
        copy[lane] = *cpu[lane];
    }
    GPU(cuMemcpyHtoD(instances, copy, sizeof copy));
    double dt = .02;
    for (int i = 0; i < 2; ++i) {
        double start = i * dt;
        void *args[] = {&instances, &start, &dt, &results, &count};
        GPU(cuLaunchKernel(step, 1, 1, 1, count, 1, 1, 0, NULL, args, NULL));
        int status[N];
        GPU(cuMemcpyDtoH(status, results, sizeof status));
        GPU(cuMemcpyDtoH(copy, instances, sizeof copy));
        for (int lane = 0; lane < count; ++lane) {
            bool event = false, terminate = false, early = false;
            double last, host[3], gpu[3];
            check_fmi(status[lane]);
            check_fmi(fmi3DoStep(cpu[lane], start, dt, true, &event, &terminate, &early, &last));
            check_fmi(fmi3GetFloat64(cpu[lane], observed, 3, host, 3));
            check_fmi(fmi3GetFloat64(&copy[lane], observed, 3, gpu, 3));
            double expected[] = {(lane + 1) * (start + dt), (i + 1) * 2, .011 / (lane + 1)};
            printf("lane %d t %.3f position %.9g ticks %.9g crossing %.9g\n",
                   lane, start + dt, gpu[0], gpu[1], gpu[2]);
            for (int k = 0; k < 3; ++k) {
                if (!isfinite(host[k]) || !isfinite(gpu[k]) || fabs(gpu[k] - host[k]) > 1e-10 ||
                    fabs(host[k] - expected[k]) > 1e-7) {
                    fprintf(stderr, "column %d expected %.12g CPU %.12g GPU %.12g\n",
                            k, expected[k], host[k], gpu[k]);
                    return 1;
                }
            }
        }
    }
    for (int lane = 0; lane < count; ++lane) fmi3FreeInstance(cpu[lane]);
    GPU(cuCtxDestroy(context));
    return 0;
}
