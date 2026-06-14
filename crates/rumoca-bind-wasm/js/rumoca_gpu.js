// Rumoca WebGPU RK4 driver.
//
// Canonical, packaged runtime helper for the GPU simulation path. The
// compiler emits per-state derivative kernels via the `wgsl-solve` target
// (WASM `prepare_gpu_simulation`); this module wraps a fixed-step classic
// RK4 integrator around them on the GPU. The RK4 stage/combine algebra runs
// in the two small hand-written kernels below.
//
// v1 semantics: only the first `n_states` slots of y integrate; algebraic
// slots and all parameters (including relation memory) stay frozen at their
// prepared initial values, so event-driven behavior does not fire on this
// path.
//
// Consumed from npm as `@cognipilot/rumoca/gpu` and by the mdBook live
// examples (imported from the same pkg base as the WASM glue).

const GPU_STAGE_WGSL = `
struct StageUniforms { scale: f32, n: u32, _pad0: u32, _pad1: u32 }
@group(0) @binding(0) var<storage, read> base: array<f32>;
@group(0) @binding(1) var<storage, read> k: array<f32>;
@group(0) @binding(2) var<storage, read_write> dst: array<f32>;
@group(0) @binding(3) var<uniform> su: StageUniforms;

// dst[i] = base[i] + scale * k[i]   (first n slots only)
@compute @workgroup_size(64)
fn axpy(@builtin(global_invocation_id) gid: vec3<u32>) {
    let i = gid.x;
    if (i >= su.n) { return; }
    dst[i] = base[i] + su.scale * k[i];
}
`;

const GPU_COMBINE_WGSL = `
struct CombineUniforms { h6: f32, n: u32, _pad0: u32, _pad1: u32 }
@group(0) @binding(0) var<storage, read> k1: array<f32>;
@group(0) @binding(1) var<storage, read> k2: array<f32>;
@group(0) @binding(2) var<storage, read> k3: array<f32>;
@group(0) @binding(3) var<storage, read> k4: array<f32>;
@group(0) @binding(4) var<storage, read_write> ystate: array<f32>;
@group(0) @binding(5) var<uniform> cu: CombineUniforms;

// y[i] += (h/6) * (k1 + 2 k2 + 2 k3 + k4)[i]
@compute @workgroup_size(64)
fn combine(@builtin(global_invocation_id) gid: vec3<u32>) {
    let i = gid.x;
    if (i >= cu.n) { return; }
    ystate[i] = ystate[i]
        + cu.h6 * (k1[i] + 2.0 * k2[i] + 2.0 * k3[i] + k4[i]);
}
`;

async function compileGpuModule(device, code, label) {
    const module = device.createShaderModule({ code, label });
    const info = await module.getCompilationInfo();
    const errors = info.messages.filter((m) => m.type === 'error');
    if (errors.length > 0) {
        throw new Error(`${label} WGSL error: ${errors[0].message}`);
    }
    return module;
}

// Acquire a WebGPU adapter, throwing actionable errors when WebGPU is
// unavailable. Returns a GPUAdapter suitable for `runGpuSimulation`.
export async function probeGpu() {
    if (!navigator.gpu) {
        throw new Error(
            'GPU requested but WebGPU is not available in this '
            + 'browser. Uncheck GPU to run on the CPU (WASM) path.'
        );
    }
    const adapter = await navigator.gpu.requestAdapter()
        || await navigator.gpu.requestAdapter({ forceFallbackAdapter: true });
    if (!adapter) {
        throw new Error(
            'GPU requested but no WebGPU adapter was found. '
            + 'On Linux Chrome, WebGPU is off by default: enable '
            + 'chrome://flags/#enable-unsafe-webgpu (or launch with '
            + '--enable-unsafe-webgpu --enable-features=Vulkan) and '
            + 'reload. Or uncheck GPU to run on the CPU (WASM) path.'
        );
    }
    return adapter;
}

// Build a reusable GPU program for a prepared model: a WebGPU device, the
// compiled WGSL modules, compute pipelines, device buffers, and bind groups,
// plus a per-run `simulate(prep, onPhase)` closure.
//
// Everything built here is fully determined by the rendered shader and layout
// (i.e. the model source) and never by parameter *values*, so a parameter-only
// re-run can reuse the whole program and just re-upload y0/p0. `runGpuSimulation`
// caches the program keyed on `prep.wgsl`; call this directly only if you want
// to manage the program lifetime yourself.
//
//   adapter : GPUAdapter (from `probeGpu`)
//   prep    : the parsed JSON from WASM `prepare_gpu_simulation`
//             ({ wgsl, layout, n_states, y0, p0, t_start, t_end, dt })
//   onPhase : optional (message, fraction|null) progress callback
//
// Returns { device, simulate } where `simulate(prepNow, onPhaseNow)` runs the
// RK4 loop and resolves to a result shaped like `simulate_model`.
export async function buildGpuProgram(adapter, prep, onPhase = () => {}) {
    const layout = prep.layout || {};
    const nStates = prep.n_states | 0;
    const yLen = Math.max(layout.y_len | 0, 1);
    const rows = Math.max(layout.rows | 0, 0);
    if (rows === 0 || nStates === 0) {
        throw new Error('Model has no continuous states to integrate on the GPU.');
    }
    if (rows !== nStates) {
        throw new Error(
            `GPU path expects one derivative row per state (rows=${rows}, `
            + `states=${nStates}); this model is not supported yet.`
        );
    }
    const tStart = Number(prep.t_start) || 0;
    const tEnd = Number(prep.t_end) || 1;
    const dt = Number(prep.dt) > 0
        ? Number(prep.dt) : (tEnd - tStart) / 500;
    const steps = Math.max(1, Math.round((tEnd - tStart) / dt));

    const device = await adapter.requestDevice();
    onPhase('Parsing GPU kernels (WGSL)', null);
    const derModule = await compileGpuModule(device, prep.wgsl, 'wgsl-solve');
    const stageModule = await compileGpuModule(device, GPU_STAGE_WGSL, 'rk4-stage');
    const combineModule = await compileGpuModule(device, GPU_COMBINE_WGSL, 'rk4-combine');

    // Kernel inventory: stencil-family kernels + residual chunks from
    // the layout manifest.
    if (!Array.isArray(layout.kernels) || layout.kernels.length === 0) {
        throw new Error(
            'GPU layout manifest has no kernel inventory; the WASM package '
            + 'predates stencil emission. Rebuild it from the wgsl-backend '
            + 'sources (wasm-pack build crates/rumoca-bind-wasm).'
        );
    }
    const kernelList = layout.kernels;
    let pipelinesBuilt = 0;
    onPhase(`Building GPU pipelines (0/${kernelList.length})`, 0);
    const derPipelines = await Promise.all(
        kernelList.map((kernel) => device.createComputePipelineAsync({
            layout: 'auto',
            compute: { module: derModule, entryPoint: kernel.entry },
        }).then((pipeline) => {
            pipelinesBuilt += 1;
            onPhase(
                `Building GPU pipelines (${pipelinesBuilt}/${kernelList.length})`,
                pipelinesBuilt / kernelList.length
            );
            return pipeline;
        }))
    );
    const kernelWorkgroups = kernelList.map(
        (kernel) => Math.max(1, Math.ceil((kernel.rows | 0) / 64)));
    const axpyPipeline = await device.createComputePipelineAsync({
        layout: 'auto', compute: { module: stageModule, entryPoint: 'axpy' },
    });
    const combinePipeline = await device.createComputePipelineAsync({
        layout: 'auto', compute: { module: combineModule, entryPoint: 'combine' },
    });

    const storage = (len, label) => device.createBuffer({
        label,
        size: Math.max(16, len * 4),
        usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST | GPUBufferUsage.COPY_SRC,
    });
    const yBuf = storage(yLen, 'y');
    const yStage = storage(yLen, 'y-stage');
    const pBuf = storage(Math.max(layout.p_len | 0, 1), 'p');
    const kBufs = [0, 1, 2, 3].map((i) => storage(rows, `k${i + 1}`));

    const timeUniform = device.createBuffer({
        size: 16, usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST,
    });
    const axpyUniforms = [0.5, 0.5, 1.0].map((scale) => {
        const buffer = device.createBuffer({
            size: 16, usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST,
        });
        const data = new ArrayBuffer(16);
        new Float32Array(data, 0, 1)[0] = scale * dt;
        new Uint32Array(data, 4, 1)[0] = nStates;
        device.queue.writeBuffer(buffer, 0, data);
        return buffer;
    });
    const combineUniform = device.createBuffer({
        size: 16, usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST,
    });
    {
        const data = new ArrayBuffer(16);
        new Float32Array(data, 0, 1)[0] = dt / 6.0;
        new Uint32Array(data, 4, 1)[0] = nStates;
        device.queue.writeBuffer(combineUniform, 0, data);
    }

    const derBind = (yIn, kOut) => derPipelines.map((pipe) => device.createBindGroup({
        layout: pipe.getBindGroupLayout(0),
        entries: [
            { binding: 0, resource: { buffer: yIn } },
            { binding: 1, resource: { buffer: pBuf } },
            { binding: 2, resource: { buffer: kOut } },
            { binding: 3, resource: { buffer: timeUniform } },
        ],
    }));
    const derBinds = [
        derBind(yBuf, kBufs[0]),   // k1 = f(t, y)
        derBind(yStage, kBufs[1]), // k2 = f(t + h/2, y + h/2 k1)
        derBind(yStage, kBufs[2]), // k3 = f(t + h/2, y + h/2 k2)
        derBind(yStage, kBufs[3]), // k4 = f(t + h, y + h k3)
    ];
    const axpyBind = (kBuf, uniform) => device.createBindGroup({
        layout: axpyPipeline.getBindGroupLayout(0),
        entries: [
            { binding: 0, resource: { buffer: yBuf } },
            { binding: 1, resource: { buffer: kBuf } },
            { binding: 2, resource: { buffer: yStage } },
            { binding: 3, resource: { buffer: uniform } },
        ],
    });
    const axpyBinds = [
        axpyBind(kBufs[0], axpyUniforms[0]),
        axpyBind(kBufs[1], axpyUniforms[1]),
        axpyBind(kBufs[2], axpyUniforms[2]),
    ];
    const combineBind = device.createBindGroup({
        layout: combinePipeline.getBindGroupLayout(0),
        entries: [
            { binding: 0, resource: { buffer: kBufs[0] } },
            { binding: 1, resource: { buffer: kBufs[1] } },
            { binding: 2, resource: { buffer: kBufs[2] } },
            { binding: 3, resource: { buffer: kBufs[3] } },
            { binding: 4, resource: { buffer: yBuf } },
            { binding: 5, resource: { buffer: combineUniform } },
        ],
    });

    const stageGroups = Math.ceil(nStates / 64);
    const dispatchDer = (enc, stage) => {
        const pass = enc.beginComputePass();
        derPipelines.forEach((pipe, c) => {
            pass.setPipeline(pipe);
            pass.setBindGroup(0, derBinds[stage][c]);
            pass.dispatchWorkgroups(kernelWorkgroups[c]);
        });
        pass.end();
    };
    const dispatchStage = (enc, pipeline, bind) => {
        const pass = enc.beginComputePass();
        pass.setPipeline(pipeline);
        pass.setBindGroup(0, bind);
        pass.dispatchWorkgroups(stageGroups);
        pass.end();
    };

    const readback = device.createBuffer({
        size: Math.max(16, yLen * 4),
        usage: GPUBufferUsage.COPY_DST | GPUBufferUsage.MAP_READ,
    });
    const writeTime = (t) => device.queue.writeBuffer(
        timeUniform, 0, new Float32Array([t, 0, 0, 0]));

    // Per-run execution. Only y0/p0 change when a parameter slider moves, so
    // this re-uploads them and steps the RK4 loop; the device, modules,
    // pipelines, buffers, and bind groups above are reused untouched.
    async function simulate(prepNow, onPhaseNow = () => {}) {
        const y0 = new Float32Array(prepNow.y0 || []);
        device.queue.writeBuffer(yBuf, 0, y0);
        device.queue.writeBuffer(yStage, 0, y0);
        device.queue.writeBuffer(pBuf, 0, new Float32Array(prepNow.p0 || []));

        const times = [tStart];
        const samples = [Array.from(y0)];
        onPhaseNow(`Simulating on WebGPU (0/${steps} steps)`, 0);
        const wallStart = performance.now();
        // One readback per step keeps the driver simple; the GPU work per
        // step is small enough that this is not the bottleneck yet.
        for (let step = 0; step < steps; step++) {
            const t = tStart + step * dt;
            const enc = device.createCommandEncoder();
            writeTime(t);
            dispatchDer(enc, 0);
            dispatchStage(enc, axpyPipeline, axpyBinds[0]);
            device.queue.submit([enc.finish()]);
            const enc2 = device.createCommandEncoder();
            writeTime(t + dt / 2);
            dispatchDer(enc2, 1);
            dispatchStage(enc2, axpyPipeline, axpyBinds[1]);
            device.queue.submit([enc2.finish()]);
            const enc3 = device.createCommandEncoder();
            dispatchDer(enc3, 2);
            dispatchStage(enc3, axpyPipeline, axpyBinds[2]);
            device.queue.submit([enc3.finish()]);
            const enc4 = device.createCommandEncoder();
            writeTime(t + dt);
            dispatchDer(enc4, 3);
            dispatchStage(enc4, combinePipeline, combineBind);
            enc4.copyBufferToBuffer(yBuf, 0, readback, 0, yLen * 4);
            device.queue.submit([enc4.finish()]);
            await readback.mapAsync(GPUMapMode.READ);
            samples.push(Array.from(new Float32Array(readback.getMappedRange())));
            readback.unmap();
            times.push(t + dt);
            if (step % 5 === 4 || step === steps - 1) {
                onPhaseNow(
                    `Simulating on WebGPU (${step + 1}/${steps} steps)`,
                    (step + 1) / steps
                );
            }
        }
        const gpuSeconds = (performance.now() - wallStart) / 1000;

        // Shape the result like simulate_model so plots and viz scripts work
        // unchanged. Names come from the layout bindings (y-kind slots).
        // Bindings include bare base-name aliases ("u" -> 0) alongside the
        // indexed names ("u[1,1]" -> 0); prefer indexed names so array
        // models keep their element naming.
        const names = new Array(yLen).fill(null);
        for (const [name, slot] of Object.entries(layout.bindings || {})) {
            if (!slot || slot.kind !== 'y' || slot.index >= yLen) {
                continue;
            }
            const existing = names[slot.index];
            if (!existing || (!existing.includes('[') && name.includes('['))) {
                names[slot.index] = name;
            }
        }
        for (let i = 0; i < yLen; i++) {
            if (!names[i]) names[i] = `y[${i}]`;
        }
        const allData = [times];
        for (let i = 0; i < yLen; i++) {
            allData.push(samples.map((row) => row[i]));
        }
        const eventNote = (layout.runtime_event_roots | 0) > 0
            ? ' · events frozen (GPU v1)' : '';
        return {
            payload: {
                names,
                allData,
                nStates,
                simDetails: {
                    actual: { t_start: tStart, t_end: times[times.length - 1], points: times.length, variables: names.length },
                    requested: { solver: `wgsl-solve RK4 (f32)${eventNote}`, t_start: tStart, t_end: tEnd, dt },
                },
            },
            metrics: { simulateSeconds: gpuSeconds },
        };
    }

    return { device, simulate };
}

// Module-level fallback cache for callers that do not supply their own. Pass an
// explicit per-instance `cache` object (e.g. one per widget) when running
// independent models concurrently so they do not evict each other.
const sharedGpuCache = {};

// Integrate a prepared model on the GPU with fixed-step RK4, reusing a compiled
// program across runs.
//
//   adapter : GPUAdapter (from `probeGpu`)
//   prep    : the parsed JSON from WASM `prepare_gpu_simulation`
//   onPhase : optional (message, fraction|null) progress callback
//   cache   : caller-owned `{ program?, wgsl? }` holder; defaults to a shared
//             module-level cache
//
// The program (device, modules, pipelines, buffers, bind groups) is fully
// determined by `prep.wgsl`, so a parameter-only re-run (same shader, new
// y0/p0) reuses the cached program and skips the shader recompile + pipeline
// rebuild entirely. A source edit re-renders the shader (new key -> rebuild,
// destroying the old device). If a reused device is lost (context loss, tab
// backgrounding), the cache is dropped so the next run rebuilds from a fresh
// device.
//
// Returns { payload: { names, allData, nStates, simDetails }, metrics } shaped
// like `simulate_model` so plots/viz scripts work unchanged.
export async function runGpuSimulation(adapter, prep, onPhase = () => {}, cache = sharedGpuCache) {
    if (!cache.program || cache.wgsl !== prep.wgsl) {
        if (cache.program) {
            try { cache.program.device.destroy(); } catch (err) { /* device already lost */ }
            cache.program = null;
        }
        cache.program = await buildGpuProgram(adapter, prep, onPhase);
        cache.wgsl = prep.wgsl;
    }
    try {
        return await cache.program.simulate(prep, onPhase);
    } catch (err) {
        // A reused device can be lost (context loss, tab backgrounding).
        // Drop the cache so the next run rebuilds from a fresh device,
        // restoring the self-healing the per-run rebuild used to give.
        cache.program = null;
        cache.wgsl = null;
        throw err;
    }
}
