// Rumoca WebGPU RK4 driver.
//
// Canonical, packaged runtime helper for the GPU simulation path. The
// compiler emits per-state derivative kernels via the `wgsl-solve` target
// (WASM `prepare_gpu_simulation`); the target also exposes implicit residual
// kernels in the layout for future implicit GPU solvers. This module wraps a
// fixed-step classic RK4 integrator around the derivative kernels. The RK4
// stage/combine algebra runs in the two small hand-written kernels below.
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

const GPU_STAGE_WORKGROUP_SIZE = 64;
const UINT32_MAX = 0xFFFF_FFFF;

async function compileGpuModule(device, code, label) {
    const module = device.createShaderModule({ code, label });
    const info = await module.getCompilationInfo();
    const errors = info.messages.filter((m) => m.type === 'error');
    if (errors.length > 0) {
        throw new Error(`${label} WGSL error: ${errors[0].message}`);
    }
    return module;
}

function integerField(value, field, label, minValue = 0) {
    const parsed = value?.[field];
    if (!Number.isSafeInteger(parsed) || parsed < minValue) {
        throw new Error(
            `${label} has invalid ${field} metadata (${value?.[field]}).`
        );
    }
    return parsed;
}

function safePositiveInteger(value, label) {
    if (!Number.isSafeInteger(value) || value < 1) {
        throw new Error(`${label} has invalid integer metadata (${value}).`);
    }
    return value;
}

function finiteNumberField(value, field, label) {
    const parsed = value?.[field];
    if (typeof parsed !== 'number' || !Number.isFinite(parsed)) {
        throw new Error(`${label} has invalid ${field} metadata (${value?.[field]}).`);
    }
    return parsed;
}

function u32Value(value, label) {
    if (!Number.isSafeInteger(value) || value < 0 || value > UINT32_MAX) {
        throw new Error(`${label}=${value} cannot be represented as a WGSL u32.`);
    }
    return value;
}

function hasOwn(value, field) {
    return value !== null
        && typeof value === 'object'
        && Object.prototype.hasOwnProperty.call(value, field);
}

function signedIntegerField(value, field, label) {
    const parsed = value?.[field];
    if (!Number.isSafeInteger(parsed)) {
        throw new Error(
            `${label} has invalid ${field} metadata (${value?.[field]}).`
        );
    }
    return parsed;
}

function checkedMetadataAdd(left, right, label) {
    const value = left + right;
    if (!Number.isSafeInteger(value)) {
        throw new Error(`${label} overflows JavaScript safe integer metadata range.`);
    }
    return value;
}

function checkedMetadataMul(left, right, label) {
    const value = left * right;
    if (!Number.isSafeInteger(value)) {
        throw new Error(`${label} overflows JavaScript safe integer metadata range.`);
    }
    return value;
}

function checkedWorkgroupCount(
    rows,
    workgroupSize,
    label,
    maxWorkgroups,
    usage = 'dispatch',
) {
    const limit = safePositiveInteger(maxWorkgroups, `${label} workgroup limit`);
    const groups = Math.floor((rows - 1) / workgroupSize) + 1;
    if (!Number.isSafeInteger(groups) || groups < 1) {
        throw new Error(`${label} ${usage} workgroup count is invalid.`);
    }
    if (groups > limit) {
        throw new Error(
            `${label} ${usage} needs ${groups} workgroups, exceeding `
            + `device limit ${limit}.`
        );
    }
    return groups;
}

function storageByteSize(elementCount, label) {
    const bytes = checkedMetadataMul(elementCount, 4, `${label} byte size`);
    return Math.max(16, bytes);
}

function deviceWorkgroupLimit(device) {
    return safePositiveInteger(
        device?.limits?.maxComputeWorkgroupsPerDimension,
        'GPU device maxComputeWorkgroupsPerDimension',
    );
}

function simulationStepCount(tStart, tEnd, dt) {
    if (tEnd < tStart) {
        throw new Error(`GPU simulation t_end=${tEnd} is before t_start=${tStart}.`);
    }
    if (dt <= 0) {
        throw new Error(`GPU simulation dt=${dt} must be greater than zero.`);
    }
    const rawSteps = (tEnd - tStart) / dt;
    if (!Number.isFinite(rawSteps)) {
        throw new Error('GPU simulation step count is not finite.');
    }
    const steps = Math.max(1, Math.round(rawSteps));
    if (!Number.isSafeInteger(steps)) {
        throw new Error('GPU simulation step count exceeds JavaScript safe integer range.');
    }
    return steps;
}

function workgroupTotal(kernels, label) {
    return kernels.reduce(
        (total, kernel, index) => checkedMetadataAdd(
            total,
            kernel.workgroups,
            `${label}[${index}] workgroup total`,
        ),
        0,
    );
}

function markOutputSlot(covered, slot, label, rows, outputName) {
    if (!Number.isSafeInteger(slot) || slot < 0 || slot >= rows) {
        throw new Error(
            `${label} writes ${outputName} output ${slot} outside layout.rows=${rows}.`
        );
    }
    const previous = covered.get(slot);
    if (previous !== undefined) {
        throw new Error(
            `${label} overlaps ${outputName} output ${slot} already written by `
            + `${previous}.`
        );
    }
    covered.set(slot, label);
}

function firstMissingOutputSlot(rows, covered) {
    if (covered.size === rows) {
        return -1;
    }
    for (let slot = 0; slot < rows; slot++) {
        if (!covered.has(slot)) {
            return slot;
        }
    }
    return -1;
}

function outputMap(value, shape, label) {
    if (typeof value.output_map !== 'object' || value.output_map === null) {
        throw new Error(`${label} is missing native output_map metadata.`);
    }
    const start = integerField(value.output_map, 'start', `${label} output_map`);
    if (!Array.isArray(value.output_map.strides)) {
        throw new Error(`${label} output_map is missing strides metadata.`);
    }
    const strides = new Array(shape.length).fill(0);
    const seen = new Array(shape.length).fill(false);
    for (let termIndex = 0; termIndex < value.output_map.strides.length; termIndex++) {
        const term = value.output_map.strides[termIndex];
        const termLabel = `${label} output_map.strides[${termIndex}]`;
        const dimension = integerField(term, 'dimension', termLabel);
        const stride = signedIntegerField(term, 'stride', termLabel);
        if (dimension >= shape.length) {
            throw new Error(
                `${termLabel} targets dimension ${dimension}, but domain rank is `
                + `${shape.length}.`
            );
        }
        if (seen[dimension]) {
            throw new Error(`${termLabel} duplicates dimension ${dimension}.`);
        }
        seen[dimension] = true;
        strides[dimension] = stride;
    }
    return { start, strides };
}

function visitNativeOutputSlots(kernel, family, label, rows, outputName, visitSlot) {
    const kernelRows = integerField(kernel, 'rows', label);
    const familyRows = integerField(family, 'rows', `${label} native family`);
    if (familyRows !== kernelRows) {
        throw new Error(
            `${label} row count ${kernelRows} does not match native family rows `
            + `${familyRows}.`
        );
    }
    if (!Array.isArray(family.domain_shape) || family.domain_shape.length === 0) {
        throw new Error(`${label} native family is missing domain_shape metadata.`);
    }
    const shape = family.domain_shape.map((_, dim) => (
        integerField(family.domain_shape, dim, `${label} domain_shape`, 1)
    ));
    const domainRows = shape.reduce((product, dim, dimIndex) => (
        checkedMetadataMul(product, dim, `${label} domain_shape[${dimIndex}] product`)
    ), 1);
    if (domainRows !== kernelRows) {
        throw new Error(
            `${label} rows=${kernelRows} does not match domain_shape product `
            + `${domainRows}.`
        );
    }
    const kernelOutputMap = outputMap(kernel, shape, label);
    const familyOutputMap = outputMap(family, shape, `${label} native family`);
    if (familyOutputMap.start !== kernelOutputMap.start) {
        throw new Error(
            `${label} output_map.start ${kernelOutputMap.start} does not match native family `
            + `start ${familyOutputMap.start}.`
        );
    }
    if (kernelOutputMap.strides.some((stride, dim) => stride !== familyOutputMap.strides[dim])) {
        throw new Error(`${label} output_map.strides do not match native family metadata.`);
    }

    for (let row = 0; row < kernelRows; row++) {
        let remainder = row;
        let slot = kernelOutputMap.start;
        for (let dim = shape.length - 1; dim >= 0; dim--) {
            const index = remainder % shape[dim];
            remainder = Math.floor(remainder / shape[dim]);
            const term = checkedMetadataMul(
                index,
                kernelOutputMap.strides[dim],
                `${label} output_map dimension ${dim}`,
            );
            slot = checkedMetadataAdd(slot, term, `${label} output_map slot`);
        }
        if (slot < 0 || slot >= rows) {
            throw new Error(
                `${label} writes ${outputName} output ${slot} outside layout.rows=${rows}.`
            );
        }
        visitSlot(slot);
    }
}

function scalarOutputSlots(kernel, label) {
    const kernelRows = integerField(kernel, 'rows', label);
    if (!Array.isArray(kernel.output_indices)) {
        throw new Error(`${label} is missing scalar output_indices metadata.`);
    }
    if (kernel.output_indices.length !== kernelRows) {
        throw new Error(
            `${label} output_indices length ${kernel.output_indices.length} `
            + `does not match rows=${kernelRows}.`
        );
    }
    return kernel.output_indices.map((slot, slotIndex) => {
        if (!Number.isSafeInteger(slot)) {
            throw new Error(
                `${label} output_indices[${slotIndex}] has invalid slot metadata (${slot}).`
            );
        }
        return slot;
    });
}

function stringField(value, field, label) {
    const fieldValue = value?.[field];
    if (typeof fieldValue !== 'string' || fieldValue.length === 0) {
        throw new Error(`${label} has invalid ${field} metadata.`);
    }
    return fieldValue;
}

function stringArrayField(value, field, label) {
    const fieldValue = value?.[field];
    if (!Array.isArray(fieldValue) || fieldValue.length === 0) {
        throw new Error(`${label} has invalid ${field} metadata.`);
    }
    return fieldValue.map((entry, index) => {
        if (typeof entry !== 'string' || entry.length === 0) {
            throw new Error(`${label}.${field}[${index}] has invalid prefix metadata.`);
        }
        return entry;
    });
}

function sameStrings(left, right) {
    return left.length === right.length
        && left.every((entry, index) => entry === right[index]);
}

function validatedEntryPrefixes(block, options) {
    const {
        layoutLabel,
        expectedNativeEntryPrefixes,
        expectedScalarEntryPrefix,
    } = options;
    if (Object.prototype.hasOwnProperty.call(block ?? {}, 'kernel_prefix')) {
        throw new Error(`${layoutLabel} has stale kernel_prefix metadata.`);
    }
    const prefixes = block?.entry_prefixes;
    if (typeof prefixes !== 'object' || prefixes === null) {
        throw new Error(`${layoutLabel} has invalid entry_prefixes metadata.`);
    }
    const nativeEntryPrefixes = stringArrayField(
        prefixes, 'native', `${layoutLabel} entry_prefixes`);
    const scalarEntryPrefix = stringField(
        prefixes, 'scalar', `${layoutLabel} entry_prefixes`);
    if (!sameStrings(nativeEntryPrefixes, expectedNativeEntryPrefixes)) {
        throw new Error(
            `${layoutLabel} native entry_prefixes must be `
            + `${expectedNativeEntryPrefixes.join(', ')}; got `
            + `${nativeEntryPrefixes.join(', ')}.`
        );
    }
    if (scalarEntryPrefix !== expectedScalarEntryPrefix) {
        throw new Error(
            `${layoutLabel} scalar entry_prefix must be ${expectedScalarEntryPrefix}; `
            + `got ${scalarEntryPrefix}.`
        );
    }
    return { nativeEntryPrefixes, scalarEntryPrefix };
}

function validatedKernelSchedule(block, options) {
    const {
        layoutLabel,
        kernelEntryLabel,
        outputName,
        nativeEntryPrefixes: expectedNativeEntryPrefixes,
        scalarEntryPrefix: expectedScalarEntryPrefix,
        denseOutputRequired,
        allowEmptySchedule = false,
        staleManifestHint = '',
    } = options;
    const { nativeEntryPrefixes, scalarEntryPrefix } = validatedEntryPrefixes(block, {
        layoutLabel,
        expectedNativeEntryPrefixes,
        expectedScalarEntryPrefix,
    });
    const rows = integerField(block, 'rows', layoutLabel);
    const layoutWorkgroupSize = integerField(block, 'workgroup_size', layoutLabel, 1);
    const chunkSize = integerField(block, 'chunk_size', layoutLabel, 1);
    if (chunkSize !== layoutWorkgroupSize) {
        throw new Error(
            `${layoutLabel} chunk_size=${chunkSize} does not match `
            + `workgroup_size=${layoutWorkgroupSize}.`
        );
    }
    if (!Array.isArray(block.kernels)) {
        throw new Error(
            `${layoutLabel} has invalid kernels metadata.${staleManifestHint}`
        );
    }
    const kernelCount = integerField(block, 'kernel_count', layoutLabel);
    if (kernelCount !== block.kernels.length) {
        throw new Error(
            `${layoutLabel} kernel_count=${kernelCount} does not match `
            + `${block.kernels.length} ${outputName} kernel entries.`
        );
    }
    const scalarChunkCount = integerField(block, 'chunks', layoutLabel);
    if (hasOwn(block, 'native_families') && !Array.isArray(block.native_families)) {
        throw new Error(`${layoutLabel} has invalid native_families metadata.`);
    }
    const nativeFamilies = Array.isArray(block.native_families) ? block.native_families : [];
    if (block.kernels.length === 0) {
        if (!allowEmptySchedule || rows !== 0) {
            throw new Error(
                `${layoutLabel} manifest has no kernel inventory.${staleManifestHint}`
            );
        }
        if (scalarChunkCount !== 0 || nativeFamilies.length !== 0) {
            throw new Error(
                `${layoutLabel} empty ${outputName} inventory must not report `
                + 'scalar chunks or native families.'
            );
        }
        return [];
    }
    const covered = new Map();
    const entries = new Set();
    let nativeIndex = 0;
    let scalarKernelCount = 0;
    const schedule = block.kernels.map((kernel) => {
        const entry = typeof kernel?.entry === 'string' ? kernel.entry : '';
        const label = `${kernelEntryLabel} ${kernel?.entry}`;
        const kernelRows = integerField(kernel, 'rows', label, 1);
        const workgroupSize = integerField(kernel, 'workgroup_size', label, 1);
        if (workgroupSize !== layoutWorkgroupSize) {
            throw new Error(
                `${label} workgroup_size=${workgroupSize} does not match `
                + `${layoutLabel} workgroup_size=${layoutWorkgroupSize}.`
            );
        }
        if (entry.length === 0) {
            throw new Error(`${label} has invalid entry metadata.`);
        }
        if (entries.has(entry)) {
            throw new Error(`${label} duplicates ${outputName} kernel entry ${entry}.`);
        }
        entries.add(entry);
        const hasTensorOutput = hasOwn(kernel, 'output_map');
        const hasScalarOutput = hasOwn(kernel, 'start_slot')
            || hasOwn(kernel, 'output_indices');
        if (hasTensorOutput && hasScalarOutput) {
            throw new Error(
                `${label} mixes native tensor output metadata with scalar chunk metadata.`
            );
        }
        if (hasTensorOutput) {
            if (!nativeEntryPrefixes.some((prefix) => entry.startsWith(prefix))) {
                throw new Error(
                    `${label} native entry must start with one of `
                    + `${nativeEntryPrefixes.join(', ')}; got ${entry}.`
                );
            }
            const family = nativeFamilies[nativeIndex];
            if (!family) {
                throw new Error(
                    `${kernelEntryLabel} ${entry} has no matching native family metadata.`
                );
            }
            visitNativeOutputSlots(
                kernel,
                family,
                `${kernelEntryLabel} ${entry}`,
                rows,
                outputName,
                (slot) => {
                    markOutputSlot(
                        covered, slot, `${kernelEntryLabel} ${entry}`, rows, outputName);
                },
            );
            nativeIndex += 1;
        } else {
            if (!entry.startsWith(scalarEntryPrefix)) {
                throw new Error(
                    `${label} scalar chunk entry must start with ${scalarEntryPrefix}; `
                    + `got ${entry}.`
                );
            }
            integerField(kernel, 'start_slot', `${kernelEntryLabel} ${entry}`);
            scalarKernelCount += 1;
            for (const slot of scalarOutputSlots(kernel, `${kernelEntryLabel} ${entry}`)) {
                markOutputSlot(
                    covered, slot, `${kernelEntryLabel} ${entry}`, rows, outputName);
            }
        }
        return { entry, rows: kernelRows, workgroupSize };
    });
    if (nativeIndex !== nativeFamilies.length) {
        throw new Error(
            `${layoutLabel} has ${nativeFamilies.length} native families but scheduled `
            + `${nativeIndex} native ${outputName} kernels.`
        );
    }
    if (scalarChunkCount !== scalarKernelCount) {
        throw new Error(
            `${layoutLabel} chunks=${scalarChunkCount} does not match `
            + `${scalarKernelCount} scalar ${outputName} kernel entries.`
        );
    }
    if (denseOutputRequired) {
        const gap = firstMissingOutputSlot(rows, covered);
        if (gap !== -1) {
            throw new Error(
                `${layoutLabel} schedule does not cover ${outputName} output ${gap}; `
                + 'GPU RK4 requires a dense derivative vector.'
            );
        }
    }
    return schedule;
}

// Normalize and validate the derivative kernel schedule in the wgsl-solve
// layout. Native kernels write through generated WGSL output maps, so the host
// only dispatches them; it still validates the maps before building pipelines
// because the RK4 path assumes a dense derivative vector matching state order.
export function derivativeKernelSchedule(layout) {
    return validatedKernelSchedule(layout, {
        layoutLabel: 'GPU layout',
        kernelEntryLabel: 'GPU kernel',
        outputName: 'derivative',
        nativeEntryPrefixes: ['derivative_rhs_map', 'derivative_rhs_stencil'],
        scalarEntryPrefix: 'derivative_rhs_chunk',
        denseOutputRequired: true,
        staleManifestHint: ' The WASM package predates stencil emission. '
            + 'Rebuild it from the wgsl-backend sources '
            + '(wasm-pack build crates/rumoca-bind-wasm).',
    });
}

// Validate the implicit RHS kernel inventory exposed by wgsl-solve. The
// browser RK4 path does not dispatch these kernels yet; this keeps the manifest
// contract executable for future implicit GPU solvers.
export function implicitKernelSchedule(layout) {
    if (layout === null || typeof layout !== 'object') {
        throw new Error('GPU layout has invalid implicit_rhs metadata.');
    }
    return validatedKernelSchedule(layout.implicit_rhs, {
        layoutLabel: 'GPU implicit_rhs layout',
        kernelEntryLabel: 'GPU implicit kernel',
        outputName: 'implicit RHS',
        nativeEntryPrefixes: ['implicit_rhs_map', 'implicit_rhs_stencil'],
        scalarEntryPrefix: 'implicit_rhs_chunk',
        denseOutputRequired: false,
        allowEmptySchedule: true,
    });
}

export function gpuKernelSchedules(layout) {
    return {
        derivative: derivativeKernelSchedule(layout),
        implicit: implicitKernelSchedule(layout),
    };
}

export function gpuKernelDispatchPlan(
    schedule,
    label = 'GPU kernel schedule',
    maxWorkgroups = Number.MAX_SAFE_INTEGER,
) {
    if (!Array.isArray(schedule) || schedule.length === 0) {
        throw new Error(`${label} has no kernels to dispatch.`);
    }
    return schedule.map((kernel, index) => {
        const entry = stringField(kernel, 'entry', `${label}[${index}]`);
        const rows = integerField(kernel, 'rows', `${label}[${index}]`, 1);
        const workgroupSize = integerField(
            kernel, 'workgroupSize', `${label}[${index}]`, 1);
        return {
            entry,
            rows,
            workgroupSize,
            workgroups: checkedWorkgroupCount(
                rows,
                workgroupSize,
                `${label}[${index}] ${entry}`,
                maxWorkgroups,
            ),
        };
    });
}

export function gpuKernelWorkgroupBudget(
    schedule,
    label = 'GPU kernel schedule',
    maxWorkgroups = Number.MAX_SAFE_INTEGER,
) {
    if (!Array.isArray(schedule)) {
        throw new Error(`${label} metadata is invalid.`);
    }
    return schedule.reduce((total, kernel, index) => {
        const entry = stringField(kernel, 'entry', `${label}[${index}]`);
        const rows = integerField(kernel, 'rows', `${label}[${index}]`, 1);
        const workgroupSize = integerField(
            kernel, 'workgroupSize', `${label}[${index}]`, 1);
        const workgroups = checkedWorkgroupCount(
            rows,
            workgroupSize,
            `${label}[${index}] ${entry}`,
            maxWorkgroups,
            'budget',
        );
        return checkedMetadataAdd(
            total,
            workgroups,
            `${label}[${index}] workgroup budget`,
        );
    }, 0);
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
    const nStates = integerField(prep, 'n_states', 'GPU preparation');
    const yLen = integerField(layout, 'y_len', 'GPU layout', 1);
    const rows = integerField(layout, 'rows', 'GPU layout');
    const pLen = integerField(layout, 'p_len', 'GPU layout');
    const runtimeEventRoots = integerField(layout, 'runtime_event_roots', 'GPU layout');
    u32Value(nStates, 'GPU preparation n_states');
    if (rows === 0 || nStates === 0) {
        throw new Error('Model has no continuous states to integrate on the GPU.');
    }
    if (rows !== nStates) {
        throw new Error(
            `GPU path expects one derivative row per state (rows=${rows}, `
            + `states=${nStates}); this model is not supported yet.`
        );
    }
    const tStart = finiteNumberField(prep, 't_start', 'GPU preparation');
    const tEnd = finiteNumberField(prep, 't_end', 'GPU preparation');
    const dt = finiteNumberField(prep, 'dt', 'GPU preparation');
    const steps = simulationStepCount(tStart, tEnd, dt);
    const schedules = gpuKernelSchedules(layout);

    const device = await adapter.requestDevice();
    const maxWorkgroups = deviceWorkgroupLimit(device);
    const kernelList = gpuKernelDispatchPlan(
        schedules.derivative, 'GPU derivative kernel schedule', maxWorkgroups);
    const implicitWorkgroups = gpuKernelWorkgroupBudget(
        schedules.implicit, 'GPU implicit kernel schedule', maxWorkgroups);
    onPhase('Parsing GPU kernels (WGSL)', null);
    const derModule = await compileGpuModule(device, prep.wgsl, 'wgsl-solve');
    const stageModule = await compileGpuModule(device, GPU_STAGE_WGSL, 'rk4-stage');
    const combineModule = await compileGpuModule(device, GPU_COMBINE_WGSL, 'rk4-combine');

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
    const axpyPipeline = await device.createComputePipelineAsync({
        layout: 'auto', compute: { module: stageModule, entryPoint: 'axpy' },
    });
    const combinePipeline = await device.createComputePipelineAsync({
        layout: 'auto', compute: { module: combineModule, entryPoint: 'combine' },
    });

    const storage = (len, label) => device.createBuffer({
        label,
        size: storageByteSize(len, label),
        usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST | GPUBufferUsage.COPY_SRC,
    });
    const yBuf = storage(yLen, 'y');
    const yStage = storage(yLen, 'y-stage');
    const pBuf = storage(Math.max(pLen, 1), 'p');
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

    const stageGroups = checkedWorkgroupCount(
        nStates,
        GPU_STAGE_WORKGROUP_SIZE,
        'GPU RK4 stage',
        maxWorkgroups,
    );
    const dispatchDer = (enc, stage) => {
        const pass = enc.beginComputePass();
        derPipelines.forEach((pipe, c) => {
            pass.setPipeline(pipe);
            pass.setBindGroup(0, derBinds[stage][c]);
            pass.dispatchWorkgroups(kernelList[c].workgroups);
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

    const yReadBytes = checkedMetadataMul(yLen, 4, 'y readback byte size');
    const readback = device.createBuffer({
        size: Math.max(16, yReadBytes),
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
            enc4.copyBufferToBuffer(yBuf, 0, readback, 0, yReadBytes);
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

        // Shape the result like simulate_model so plots and viz scripts work unchanged.
        const names = Array.isArray(prepNow.state_names)
            ? prepNow.state_names.slice(0, yLen)
            : [];
        for (let i = 0; i < yLen; i++) {
            if (!names[i]) names[i] = `y[${i}]`;
        }
        const allData = [times];
        for (let i = 0; i < yLen; i++) {
            allData.push(samples.map((row) => row[i]));
        }
        const eventNote = runtimeEventRoots > 0
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
            metrics: {
                simulateSeconds: gpuSeconds,
                derivativeKernels: kernelList.length,
                derivativeWorkgroups: workgroupTotal(
                    kernelList, 'GPU derivative kernel schedule'),
                implicitKernels: schedules.implicit.length,
                implicitWorkgroups,
            },
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
