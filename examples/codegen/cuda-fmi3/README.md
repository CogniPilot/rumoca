# Numeric FMI 3 co-simulation on CUDA

This example runs Rumoca's generated FMI 3 co-simulation step on an NVIDIA GPU. Each CUDA thread advances one independent, initialized component. The numerical integrator, event localization, discrete iteration and scheduled clocks all come from the emitted FMI source. Host code initializes instances, sets inputs and reads output variables. A batch must share its communication start time and step size. Each lane has independent inputs and state.

## Build

Use a CUDA-capable LLVM/Clang toolchain, Node.js and a CUDA toolkit with libdevice and ptxas. LLVM/Clang 21.1.8 and CUDA 12.9 were tested on an RTX 3090. From the repository root:

```sh
example=examples/codegen/cuda-fmi3
rumoca compile "$example/TimedCrossing.mo" -m TimedCrossing --target fmi3 -o /tmp/cuda-timed
export CLANG=/path/to/clang
export LLVM_BIN=/path/to/llvm/bin
export CUDA_LIBDEVICE=/path/to/cuda/nvvm/libdevice/libdevice.10.bc
export PTXAS=/path/to/cuda/bin/ptxas
export CUDA_ARCH=sm_86
export FMI_INCLUDE="$PWD/crates/rumoca-phase-codegen/src/templates/fmi-ls-wasm/fmi3-headers"
node "$example/build.mjs" /tmp/cuda-timed/TimedCrossing /tmp/cuda-timed/gpu
```

The output directory contains an architecture-specific `ensemble.cubin` and an `ensemble.json` with its SHA-256, FMI instantiation token and workspace metadata.

## Hardware clock test

```sh
src=/tmp/cuda-timed/TimedCrossing/sources
cc -O2 -ffp-contract=off -DNDEBUG -I"$src" -I"$FMI_INCLUDE" -I"$CUDA_HOME/include" \
  "$example/check-timed.c" "$src"/*.c -lcuda -lm -o /tmp/cuda-timed/check
token=$(sed -n 's/.*instantiationToken="\([^"]*\)".*/\1/p' /tmp/cuda-timed/TimedCrossing/modelDescription.xml)
/tmp/cuda-timed/check /tmp/cuda-timed/gpu/ensemble.cubin "$token"
```

This compares two rates against the native CPU implementation and independent expected values. Both lanes process four clock ticks over 40 ms, with distinct state crossings at 11 ms and 5.5 ms.

## Driver contract

Compile the host from exactly the same generated source, architecture width and instantiation token as the CUBIN. Read `rmc_fmi3_token` (null-terminated UTF-8) and `rmc_fmi3_instance_bytes` (uint64) with `cuModuleGetGlobal`. Reject any mismatch before copying instances.

1. Initialize each numeric component on the host with null callbacks and environment, transfer its complete inline `ModelInstance` to consecutive device storage and allocate one integer status per lane.
2. Allocate device workspace for the active lanes. Publish the global device pointer in `rmc_scratch` and a per-lane stride in bytes in `rmc_stride`. The stride must be a multiple of 256. Zero the initial 256-byte header of each lane. The compiled manifest's `maxFrame` is the largest individual aggregate frame. Nested call depth determines total workspace. The bounds check traps on overflow. The diver fixture used 4 MiB per lane.
3. Reserve enough CUDA device stack and heap using `cuCtxSetLimit` before launch. Events use the standard device heap for temporary iteration history. Size provision must cover concurrent lanes. The full diver needed a 24,576-byte stack limit and a larger heap than the default at 64 lanes.
4. Launch `rmc_fmi3_step(instances, currentTime, dt, statuses, laneCount)`. Check CUDA execution and every FMI status before consuming a batch. Copy back the numeric instances to use the usual FMI getters. Reinitialize the batch after a failed step.

This implementation supports numeric components with inline state and null diagnostic callbacks. Host-owned storage, external services, strings and binaries require a serialization or device binding. Diagnostic formatting can trap on GPU. Numerical event clocks and both sides of the FMI transfer require model-specific regression tests. Each lane runs its own scalar solver. Throughput and latency depend heavily on model size, batch size, code generation and hardware.
