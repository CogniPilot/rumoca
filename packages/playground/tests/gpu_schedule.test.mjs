import test from "node:test";
import assert from "node:assert/strict";

import {
  derivativeKernelSchedule as rawDerivativeKernelSchedule,
  gpuKernelDispatchPlan,
  gpuKernelSchedules as rawGpuKernelSchedules,
  gpuKernelWorkgroupBudget,
  implicitKernelSchedule as rawImplicitKernelSchedule,
} from "../../rumoca-web/runtime/rumoca_gpu.js";

const derivativeEntryPrefixes = {
  native: ["derivative_rhs_map", "derivative_rhs_stencil"],
  scalar: "derivative_rhs_chunk",
};

const implicitEntryPrefixes = {
  native: ["implicit_rhs_map", "implicit_rhs_stencil"],
  scalar: "implicit_rhs_chunk",
};

function withDerivativeEntryPrefixes(layout) {
  return { entry_prefixes: derivativeEntryPrefixes, ...layout };
}

function withImplicitEntryPrefixes(block) {
  return { entry_prefixes: implicitEntryPrefixes, ...block };
}

function derivativeKernelSchedule(layout) {
  return rawDerivativeKernelSchedule(withDerivativeEntryPrefixes(layout));
}

function implicitKernelSchedule(layout) {
  return rawImplicitKernelSchedule({
    ...layout,
    implicit_rhs: withImplicitEntryPrefixes(layout.implicit_rhs),
  });
}

function gpuKernelSchedules(layout) {
  return rawGpuKernelSchedules({
    ...withDerivativeEntryPrefixes(layout),
    implicit_rhs: withImplicitEntryPrefixes(layout.implicit_rhs),
  });
}

function nativeKernel(overrides = {}) {
  return {
    entry: "derivative_rhs_map0",
    rows: 3,
    output_map: { start: 0, strides: [{ dimension: 0, stride: 1 }] },
    workgroup_size: 64,
    ...overrides,
  };
}

function nativeFamily(overrides = {}) {
  return {
    kind: "map",
    rows: 3,
    output_map: { start: 0, strides: [{ dimension: 0, stride: 1 }] },
    domain_shape: [3],
    ...overrides,
  };
}

function scalarKernel(overrides = {}) {
  return {
    entry: "derivative_rhs_chunk0",
    rows: 2,
    start_slot: 0,
    output_indices: [0, 1],
    workgroup_size: 64,
    ...overrides,
  };
}

test("derivativeKernelSchedule accepts native and scalar derivative kernels", () => {
  assert.deepEqual(
    derivativeKernelSchedule({
      rows: 5,
      workgroup_size: 64,
      chunk_size: 64,
      chunks: 1,
      kernel_count: 2,
      kernels: [nativeKernel(), scalarKernel({ start_slot: 3, output_indices: [3, 4] })],
      native_families: [nativeFamily()],
    }),
    [
      { entry: "derivative_rhs_map0", rows: 3, workgroupSize: 64 },
      { entry: "derivative_rhs_chunk0", rows: 2, workgroupSize: 64 },
    ],
  );
});

test("derivativeKernelSchedule rejects missing entry prefix metadata", () => {
  assert.throws(
    () => rawDerivativeKernelSchedule({
      rows: 3,
      workgroup_size: 64,
      chunk_size: 64,
      chunks: 0,
      kernel_count: 1,
      kernels: [nativeKernel()],
      native_families: [nativeFamily()],
    }),
    /invalid entry_prefixes metadata/,
  );
});

test("derivativeKernelSchedule rejects mismatched entry prefix metadata", () => {
  assert.throws(
    () => rawDerivativeKernelSchedule({
      rows: 3,
      entry_prefixes: {
        native: ["implicit_rhs_map", "implicit_rhs_stencil"],
        scalar: "derivative_rhs_chunk",
      },
      workgroup_size: 64,
      chunk_size: 64,
      chunks: 0,
      kernel_count: 1,
      kernels: [nativeKernel()],
      native_families: [nativeFamily()],
    }),
    /native entry_prefixes must be derivative_rhs_map, derivative_rhs_stencil/,
  );
});

test("derivativeKernelSchedule rejects stale kernel prefix metadata", () => {
  assert.throws(
    () => rawDerivativeKernelSchedule({
      rows: 3,
      kernel_prefix: "derivative_rhs_chunk",
      entry_prefixes: derivativeEntryPrefixes,
      workgroup_size: 64,
      chunk_size: 64,
      chunks: 0,
      kernel_count: 1,
      kernels: [nativeKernel()],
      native_families: [nativeFamily()],
    }),
    /stale kernel_prefix metadata/,
  );
});

test("derivativeKernelSchedule accepts affine native output coverage", () => {
  assert.deepEqual(
    derivativeKernelSchedule({
      rows: 8,
      workgroup_size: 64,
      chunk_size: 64,
      chunks: 0,
      kernel_count: 3,
      kernels: [
        nativeKernel({
          entry: "derivative_rhs_map0",
          rows: 4,
          output_map: {
            start: 0,
            strides: [{ dimension: 0, stride: 2 }, { dimension: 1, stride: 1 }],
          },
        }),
        nativeKernel({
          entry: "derivative_rhs_map1",
          rows: 2,
          output_map: { start: 4, strides: [{ dimension: 0, stride: 2 }] },
        }),
        nativeKernel({
          entry: "derivative_rhs_map2",
          rows: 2,
          output_map: { start: 5, strides: [{ dimension: 0, stride: 2 }] },
        }),
      ],
      native_families: [
        nativeFamily({
          rows: 4,
          output_map: {
            start: 0,
            strides: [{ dimension: 0, stride: 2 }, { dimension: 1, stride: 1 }],
          },
          domain_shape: [2, 2],
        }),
        nativeFamily({
          rows: 2,
          output_map: { start: 4, strides: [{ dimension: 0, stride: 2 }] },
          domain_shape: [2],
        }),
        nativeFamily({
          rows: 2,
          output_map: { start: 5, strides: [{ dimension: 0, stride: 2 }] },
          domain_shape: [2],
        }),
      ],
    }),
    [
      { entry: "derivative_rhs_map0", rows: 4, workgroupSize: 64 },
      { entry: "derivative_rhs_map1", rows: 2, workgroupSize: 64 },
      { entry: "derivative_rhs_map2", rows: 2, workgroupSize: 64 },
    ],
  );
});

test("derivativeKernelSchedule rejects sparse derivative coverage", () => {
  assert.throws(
    () => derivativeKernelSchedule({
      rows: 5,
      workgroup_size: 64,
      chunk_size: 64,
      chunks: 0,
      kernel_count: 1,
      kernels: [nativeKernel({ output_map: { start: 0, strides: [{ dimension: 0, stride: 2 }] } })],
      native_families: [
        nativeFamily({ output_map: { start: 0, strides: [{ dimension: 0, stride: 2 }] } }),
      ],
    }),
    /does not cover derivative output/,
  );
});

test("derivativeKernelSchedule rejects overlapping derivative coverage", () => {
  assert.throws(
    () => derivativeKernelSchedule({
      rows: 3,
      workgroup_size: 64,
      chunk_size: 64,
      chunks: 1,
      kernel_count: 2,
      kernels: [nativeKernel(), scalarKernel({ rows: 1, start_slot: 2, output_indices: [2] })],
      native_families: [nativeFamily()],
    }),
    /overlaps derivative output/,
  );
});

test("derivativeKernelSchedule rejects native output bounds overflow", () => {
  assert.throws(
    () => derivativeKernelSchedule({
      rows: 3,
      workgroup_size: 64,
      chunk_size: 64,
      chunks: 0,
      kernel_count: 1,
      kernels: [nativeKernel({ output_map: { start: 2, strides: [{ dimension: 0, stride: 1 }] } })],
      native_families: [
        nativeFamily({ output_map: { start: 2, strides: [{ dimension: 0, stride: 1 }] } }),
      ],
    }),
    /outside layout\.rows/,
  );
});

test("derivativeKernelSchedule rejects mixed native and scalar kernel metadata", () => {
  assert.throws(
    () => derivativeKernelSchedule({
      rows: 3,
      workgroup_size: 64,
      chunk_size: 64,
      chunks: 0,
      kernel_count: 1,
      kernels: [nativeKernel({ start_slot: 0, output_indices: [0, 1, 2] })],
      native_families: [nativeFamily()],
    }),
    /mixes native tensor output metadata with scalar chunk metadata/,
  );
});

test("derivativeKernelSchedule rejects scalar chunks without start slots", () => {
  const kernel = scalarKernel();
  delete kernel.start_slot;

  assert.throws(
    () => derivativeKernelSchedule({
      rows: 2,
      workgroup_size: 64,
      chunk_size: 64,
      chunks: 1,
      kernel_count: 1,
      kernels: [kernel],
    }),
    /invalid start_slot metadata/,
  );
});

test("derivativeKernelSchedule rejects scalar chunks without output indices", () => {
  const kernel = scalarKernel();
  delete kernel.output_indices;

  assert.throws(
    () => derivativeKernelSchedule({
      rows: 2,
      workgroup_size: 64,
      chunk_size: 64,
      chunks: 1,
      kernel_count: 1,
      kernels: [kernel],
    }),
    /missing scalar output_indices metadata/,
  );
});

test("derivativeKernelSchedule rejects scalar chunk output index length mismatch", () => {
  assert.throws(
    () => derivativeKernelSchedule({
      rows: 2,
      workgroup_size: 64,
      chunk_size: 64,
      chunks: 1,
      kernel_count: 1,
      kernels: [scalarKernel({ output_indices: [0] })],
    }),
    /output_indices length 1 does not match rows=2/,
  );
});

test("derivativeKernelSchedule rejects nonnumeric scalar chunk output indices", () => {
  assert.throws(
    () => derivativeKernelSchedule({
      rows: 2,
      workgroup_size: 64,
      chunk_size: 64,
      chunks: 1,
      kernel_count: 1,
      kernels: [scalarKernel({ output_indices: [0, "1"] })],
    }),
    /output_indices\[1\] has invalid slot metadata/,
  );
});

test("derivativeKernelSchedule rejects unsafe scalar chunk output indices", () => {
  assert.throws(
    () => derivativeKernelSchedule({
      rows: 2,
      workgroup_size: 64,
      chunk_size: 64,
      chunks: 1,
      kernel_count: 1,
      kernels: [scalarKernel({ output_indices: [0, Number.MAX_SAFE_INTEGER + 1] })],
    }),
    /output_indices\[1\] has invalid slot metadata/,
  );
});

test("derivativeKernelSchedule uses scalar chunk output indices for coverage", () => {
  assert.throws(
    () => derivativeKernelSchedule({
      rows: 3,
      workgroup_size: 64,
      chunk_size: 64,
      chunks: 1,
      kernel_count: 1,
      kernels: [scalarKernel({ start_slot: 0, output_indices: [0, 2] })],
    }),
    /does not cover derivative output 1/,
  );
});

test("derivativeKernelSchedule rejects stringified numeric metadata", () => {
  assert.throws(
    () => derivativeKernelSchedule({
      rows: "3",
      workgroup_size: 64,
      chunk_size: 64,
      chunks: 0,
      kernel_count: 1,
      kernels: [nativeKernel()],
      native_families: [nativeFamily()],
    }),
    /invalid rows metadata/,
  );
});

test("derivativeKernelSchedule rejects unsafe integer metadata", () => {
  assert.throws(
    () => derivativeKernelSchedule({
      rows: Number.MAX_SAFE_INTEGER + 1,
      workgroup_size: 64,
      chunk_size: 64,
      chunks: 0,
      kernel_count: 1,
      kernels: [nativeKernel()],
      native_families: [nativeFamily()],
    }),
    /invalid rows metadata/,
  );
});

test("derivativeKernelSchedule rejects zero-row kernels", () => {
  assert.throws(
    () => derivativeKernelSchedule({
      rows: 1,
      workgroup_size: 64,
      chunk_size: 64,
      chunks: 1,
      kernel_count: 1,
      kernels: [scalarKernel({ rows: 0, output_indices: [] })],
    }),
    /invalid rows metadata/,
  );
});

test("derivativeKernelSchedule rejects duplicate kernel entries", () => {
  assert.throws(
    () => derivativeKernelSchedule({
      rows: 5,
      workgroup_size: 64,
      chunk_size: 64,
      chunks: 2,
      kernel_count: 2,
      kernels: [
        scalarKernel({ entry: "derivative_rhs_chunk0", output_indices: [0, 1] }),
        scalarKernel({
          entry: "derivative_rhs_chunk0",
          start_slot: 2,
          output_indices: [2, 3],
        }),
      ],
    }),
    /duplicates derivative kernel entry derivative_rhs_chunk0/,
  );
});

test("derivativeKernelSchedule rejects invalid native family list metadata", () => {
  assert.throws(
    () => derivativeKernelSchedule({
      rows: 2,
      workgroup_size: 64,
      chunk_size: 64,
      chunks: 1,
      kernel_count: 1,
      kernels: [scalarKernel()],
      native_families: {},
    }),
    /invalid native_families metadata/,
  );
});

test("derivativeKernelSchedule rejects native domain product overflow", () => {
  assert.throws(
    () => derivativeKernelSchedule({
      rows: 1,
      workgroup_size: 64,
      chunk_size: 64,
      chunks: 0,
      kernel_count: 1,
      kernels: [nativeKernel({ rows: 1 })],
      native_families: [
        nativeFamily({ rows: 1, domain_shape: [Number.MAX_SAFE_INTEGER, 2] }),
      ],
    }),
    /domain_shape\[1\] product overflows JavaScript safe integer metadata range/,
  );
});

test("derivativeKernelSchedule rejects native output slot overflow", () => {
  const outputMap = {
    start: Number.MAX_SAFE_INTEGER - 1,
    strides: [{ dimension: 0, stride: 2 }],
  };
  assert.throws(
    () => derivativeKernelSchedule({
      rows: Number.MAX_SAFE_INTEGER,
      workgroup_size: 64,
      chunk_size: 64,
      chunks: 0,
      kernel_count: 1,
      kernels: [nativeKernel({ rows: 2, output_map: outputMap })],
      native_families: [
        nativeFamily({ rows: 2, domain_shape: [2], output_map: outputMap }),
      ],
    }),
    /output_map slot overflows JavaScript safe integer metadata range/,
  );
});

test("derivativeKernelSchedule rejects null kernel entries", () => {
  assert.throws(
    () => derivativeKernelSchedule({
      rows: 3,
      workgroup_size: 64,
      chunk_size: 64,
      chunks: 1,
      kernel_count: 1,
      kernels: [null],
    }),
    /invalid rows metadata/,
  );
});

test("derivativeKernelSchedule rejects implicit entry names", () => {
  assert.throws(
    () => derivativeKernelSchedule({
      rows: 3,
      workgroup_size: 64,
      chunk_size: 64,
      chunks: 0,
      kernel_count: 1,
      kernels: [nativeKernel({ entry: "implicit_rhs_map0" })],
      native_families: [nativeFamily()],
    }),
    /native entry must start with one of derivative_rhs_map, derivative_rhs_stencil/,
  );
});

test("derivativeKernelSchedule rejects native chunk entry names", () => {
  assert.throws(
    () => derivativeKernelSchedule({
      rows: 3,
      workgroup_size: 64,
      chunk_size: 64,
      chunks: 0,
      kernel_count: 1,
      kernels: [nativeKernel({ entry: "derivative_rhs_chunk0" })],
      native_families: [nativeFamily()],
    }),
    /native entry must start with one of derivative_rhs_map, derivative_rhs_stencil/,
  );
});

test("derivativeKernelSchedule rejects scalar native entry names", () => {
  assert.throws(
    () => derivativeKernelSchedule({
      rows: 2,
      workgroup_size: 64,
      chunk_size: 64,
      chunks: 1,
      kernel_count: 1,
      kernels: [scalarKernel({ entry: "derivative_rhs_map0" })],
    }),
    /scalar chunk entry must start with derivative_rhs_chunk/,
  );
});

test("derivativeKernelSchedule rejects missing kernel count metadata", () => {
  assert.throws(
    () => derivativeKernelSchedule({
      rows: 3,
      workgroup_size: 64,
      chunk_size: 64,
      chunks: 0,
      kernels: [nativeKernel()],
      native_families: [nativeFamily()],
    }),
    /invalid kernel_count metadata/,
  );
});

test("derivativeKernelSchedule rejects mismatched kernel count metadata", () => {
  assert.throws(
    () => derivativeKernelSchedule({
      rows: 5,
      workgroup_size: 64,
      chunk_size: 64,
      chunks: 1,
      kernel_count: 1,
      kernels: [nativeKernel(), scalarKernel({ start_slot: 3, output_indices: [3, 4] })],
      native_families: [nativeFamily()],
    }),
    /kernel_count=1 does not match 2 derivative kernel entries/,
  );
});

test("derivativeKernelSchedule rejects missing scalar chunk count metadata", () => {
  assert.throws(
    () => derivativeKernelSchedule({
      rows: 3,
      workgroup_size: 64,
      chunk_size: 64,
      kernel_count: 1,
      kernels: [nativeKernel()],
      native_families: [nativeFamily()],
    }),
    /invalid chunks metadata/,
  );
});

test("derivativeKernelSchedule rejects mismatched scalar chunk count metadata", () => {
  assert.throws(
    () => derivativeKernelSchedule({
      rows: 5,
      workgroup_size: 64,
      chunk_size: 64,
      chunks: 0,
      kernel_count: 2,
      kernels: [nativeKernel(), scalarKernel({ start_slot: 3, output_indices: [3, 4] })],
      native_families: [nativeFamily()],
    }),
    /chunks=0 does not match 1 scalar derivative kernel entries/,
  );
});

test("derivativeKernelSchedule rejects missing top-level workgroup size", () => {
  assert.throws(
    () => derivativeKernelSchedule({
      rows: 3,
      chunk_size: 64,
      chunks: 0,
      kernel_count: 1,
      kernels: [nativeKernel()],
      native_families: [nativeFamily()],
    }),
    /invalid workgroup_size metadata/,
  );
});

test("derivativeKernelSchedule rejects mismatched kernel workgroup size", () => {
  assert.throws(
    () => derivativeKernelSchedule({
      rows: 3,
      workgroup_size: 32,
      chunk_size: 32,
      chunks: 0,
      kernel_count: 1,
      kernels: [nativeKernel()],
      native_families: [nativeFamily()],
    }),
    /workgroup_size=64 does not match GPU layout workgroup_size=32/,
  );
});

test("derivativeKernelSchedule rejects missing chunk size", () => {
  assert.throws(
    () => derivativeKernelSchedule({
      rows: 3,
      workgroup_size: 64,
      chunks: 0,
      kernel_count: 1,
      kernels: [nativeKernel()],
      native_families: [nativeFamily()],
    }),
    /invalid chunk_size metadata/,
  );
});

test("derivativeKernelSchedule rejects mismatched chunk size", () => {
  assert.throws(
    () => derivativeKernelSchedule({
      rows: 3,
      workgroup_size: 64,
      chunk_size: 32,
      chunks: 0,
      kernel_count: 1,
      kernels: [nativeKernel()],
      native_families: [nativeFamily()],
    }),
    /chunk_size=32 does not match workgroup_size=64/,
  );
});

test("implicitKernelSchedule accepts native and scalar implicit kernels", () => {
  assert.deepEqual(
    implicitKernelSchedule({
      implicit_rhs: {
        rows: 5,
        workgroup_size: 64,
        chunk_size: 64,
        chunks: 1,
        kernel_count: 2,
        kernels: [
          nativeKernel({ entry: "implicit_rhs_map0" }),
          scalarKernel({
            entry: "implicit_rhs_chunk0",
            start_slot: 3,
            output_indices: [3, 4],
          }),
        ],
        native_families: [nativeFamily()],
      },
    }),
    [
      { entry: "implicit_rhs_map0", rows: 3, workgroupSize: 64 },
      { entry: "implicit_rhs_chunk0", rows: 2, workgroupSize: 64 },
    ],
  );
});

test("implicitKernelSchedule accepts empty zero-row implicit inventory", () => {
  assert.deepEqual(
    implicitKernelSchedule({
      implicit_rhs: {
        rows: 0,
        workgroup_size: 64,
        chunk_size: 64,
        chunks: 0,
        kernel_count: 0,
        kernels: [],
        native_families: [],
      },
    }),
    [],
  );
});

test("implicitKernelSchedule rejects empty nonzero-row implicit inventory", () => {
  assert.throws(
    () => implicitKernelSchedule({
      implicit_rhs: {
        rows: 1,
        workgroup_size: 64,
        chunk_size: 64,
        chunks: 0,
        kernel_count: 0,
        kernels: [],
        native_families: [],
      },
    }),
    /manifest has no kernel inventory/,
  );
});

test("implicitKernelSchedule rejects empty implicit inventory with family counts", () => {
  assert.throws(
    () => implicitKernelSchedule({
      implicit_rhs: {
        rows: 0,
        workgroup_size: 64,
        chunk_size: 64,
        chunks: 1,
        kernel_count: 0,
        kernels: [],
        native_families: [],
      },
    }),
    /empty implicit RHS inventory must not report scalar chunks or native families/,
  );
});

test("implicitKernelSchedule rejects missing entry prefix metadata", () => {
  assert.throws(
    () => rawImplicitKernelSchedule({
      implicit_rhs: {
        rows: 3,
        workgroup_size: 64,
        chunk_size: 64,
        chunks: 0,
        kernel_count: 1,
        kernels: [nativeKernel({ entry: "implicit_rhs_map0" })],
        native_families: [nativeFamily()],
      },
    }),
    /invalid entry_prefixes metadata/,
  );
});

test("implicitKernelSchedule rejects mismatched entry prefix metadata", () => {
  assert.throws(
    () => rawImplicitKernelSchedule({
      implicit_rhs: {
        rows: 3,
        entry_prefixes: {
          native: ["derivative_rhs_map", "derivative_rhs_stencil"],
          scalar: "implicit_rhs_chunk",
        },
        workgroup_size: 64,
        chunk_size: 64,
        chunks: 0,
        kernel_count: 1,
        kernels: [nativeKernel({ entry: "implicit_rhs_map0" })],
        native_families: [nativeFamily()],
      },
    }),
    /native entry_prefixes must be implicit_rhs_map, implicit_rhs_stencil/,
  );
});

test("implicitKernelSchedule rejects stale kernel prefix metadata", () => {
  assert.throws(
    () => rawImplicitKernelSchedule({
      implicit_rhs: {
        rows: 3,
        kernel_prefix: "implicit_rhs_chunk",
        entry_prefixes: implicitEntryPrefixes,
        workgroup_size: 64,
        chunk_size: 64,
        chunks: 0,
        kernel_count: 1,
        kernels: [nativeKernel({ entry: "implicit_rhs_map0" })],
        native_families: [nativeFamily()],
      },
    }),
    /stale kernel_prefix metadata/,
  );
});

test("implicitKernelSchedule accepts sparse scalar implicit output coverage", () => {
  assert.deepEqual(
    implicitKernelSchedule({
      implicit_rhs: {
        rows: 5,
        workgroup_size: 64,
        chunk_size: 64,
        chunks: 1,
        kernel_count: 1,
        kernels: [scalarKernel({
          entry: "implicit_rhs_chunk0",
          start_slot: 0,
          output_indices: [1, 4],
        })],
      },
    }),
    [{ entry: "implicit_rhs_chunk0", rows: 2, workgroupSize: 64 }],
  );
});

test("implicitKernelSchedule allows sparse implicit output coverage", () => {
  assert.deepEqual(
    implicitKernelSchedule({
      implicit_rhs: {
        rows: 5,
        workgroup_size: 64,
        chunk_size: 64,
        chunks: 0,
        kernel_count: 1,
        kernels: [nativeKernel({
          entry: "implicit_rhs_map0",
          output_map: { start: 0, strides: [{ dimension: 0, stride: 2 }] },
        })],
        native_families: [
          nativeFamily({ output_map: { start: 0, strides: [{ dimension: 0, stride: 2 }] } }),
        ],
      },
    }),
    [{ entry: "implicit_rhs_map0", rows: 3, workgroupSize: 64 }],
  );
});

test("implicitKernelSchedule rejects mismatched implicit scalar chunk metadata", () => {
  assert.throws(
    () => implicitKernelSchedule({
      implicit_rhs: {
        rows: 5,
        workgroup_size: 64,
        chunk_size: 64,
        chunks: 0,
        kernel_count: 2,
        kernels: [
          nativeKernel({ entry: "implicit_rhs_map0" }),
          scalarKernel({
            entry: "implicit_rhs_chunk0",
            start_slot: 3,
            output_indices: [3, 4],
          }),
        ],
        native_families: [nativeFamily()],
      },
    }),
    /chunks=0 does not match 1 scalar implicit RHS kernel entries/,
  );
});

test("implicitKernelSchedule rejects duplicate kernel entries", () => {
  assert.throws(
    () => implicitKernelSchedule({
      implicit_rhs: {
        rows: 5,
        workgroup_size: 64,
        chunk_size: 64,
        chunks: 2,
        kernel_count: 2,
        kernels: [
          scalarKernel({
            entry: "implicit_rhs_chunk0",
            output_indices: [0, 1],
          }),
          scalarKernel({
            entry: "implicit_rhs_chunk0",
            start_slot: 2,
            output_indices: [2, 3],
          }),
        ],
      },
    }),
    /duplicates implicit RHS kernel entry implicit_rhs_chunk0/,
  );
});

test("implicitKernelSchedule rejects invalid native family list metadata", () => {
  assert.throws(
    () => implicitKernelSchedule({
      implicit_rhs: {
        rows: 2,
        workgroup_size: 64,
        chunk_size: 64,
        chunks: 1,
        kernel_count: 1,
        kernels: [scalarKernel({ entry: "implicit_rhs_chunk0" })],
        native_families: {},
      },
    }),
    /invalid native_families metadata/,
  );
});

test("implicitKernelSchedule rejects mixed native and scalar kernel metadata", () => {
  assert.throws(
    () => implicitKernelSchedule({
      implicit_rhs: {
        rows: 3,
        workgroup_size: 64,
        chunk_size: 64,
        chunks: 0,
        kernel_count: 1,
        kernels: [nativeKernel({
          entry: "implicit_rhs_map0",
          start_slot: 0,
          output_indices: [0, 1, 2],
        })],
        native_families: [nativeFamily()],
      },
    }),
    /mixes native tensor output metadata with scalar chunk metadata/,
  );
});

test("implicitKernelSchedule rejects derivative entry names", () => {
  assert.throws(
    () => implicitKernelSchedule({
      implicit_rhs: {
        rows: 3,
        workgroup_size: 64,
        chunk_size: 64,
        chunks: 0,
        kernel_count: 1,
        kernels: [nativeKernel({ entry: "derivative_rhs_map0" })],
        native_families: [nativeFamily()],
      },
    }),
    /native entry must start with one of implicit_rhs_map, implicit_rhs_stencil/,
  );
});

test("implicitKernelSchedule rejects native chunk entry names", () => {
  assert.throws(
    () => implicitKernelSchedule({
      implicit_rhs: {
        rows: 3,
        workgroup_size: 64,
        chunk_size: 64,
        chunks: 0,
        kernel_count: 1,
        kernels: [nativeKernel({ entry: "implicit_rhs_chunk0" })],
        native_families: [nativeFamily()],
      },
    }),
    /native entry must start with one of implicit_rhs_map, implicit_rhs_stencil/,
  );
});

test("implicitKernelSchedule rejects scalar native entry names", () => {
  assert.throws(
    () => implicitKernelSchedule({
      implicit_rhs: {
        rows: 2,
        workgroup_size: 64,
        chunk_size: 64,
        chunks: 1,
        kernel_count: 1,
        kernels: [scalarKernel({ entry: "implicit_rhs_map0" })],
      },
    }),
    /scalar chunk entry must start with implicit_rhs_chunk/,
  );
});

test("implicitKernelSchedule rejects missing implicit layout metadata", () => {
  assert.throws(
    () => implicitKernelSchedule({}),
    /invalid rows metadata/,
  );
});

test("gpuKernelSchedules validates derivative and implicit schedules together", () => {
  assert.deepEqual(
    gpuKernelSchedules({
      rows: 3,
      workgroup_size: 64,
      chunk_size: 64,
      chunks: 0,
      kernel_count: 1,
      kernels: [nativeKernel()],
      native_families: [nativeFamily()],
      implicit_rhs: {
        rows: 3,
        workgroup_size: 64,
        chunk_size: 64,
        chunks: 0,
        kernel_count: 1,
        kernels: [nativeKernel({ entry: "implicit_rhs_map0" })],
        native_families: [nativeFamily()],
      },
    }),
    {
      derivative: [{ entry: "derivative_rhs_map0", rows: 3, workgroupSize: 64 }],
      implicit: [{ entry: "implicit_rhs_map0", rows: 3, workgroupSize: 64 }],
    },
  );
});

test("gpuKernelSchedules accepts empty zero-row implicit schedule", () => {
  assert.deepEqual(
    gpuKernelSchedules({
      rows: 3,
      workgroup_size: 64,
      chunk_size: 64,
      chunks: 0,
      kernel_count: 1,
      kernels: [nativeKernel()],
      native_families: [nativeFamily()],
      implicit_rhs: {
        rows: 0,
        workgroup_size: 64,
        chunk_size: 64,
        chunks: 0,
        kernel_count: 0,
        kernels: [],
        native_families: [],
      },
    }),
    {
      derivative: [{ entry: "derivative_rhs_map0", rows: 3, workgroupSize: 64 }],
      implicit: [],
    },
  );
});

test("gpuKernelDispatchPlan computes per-kernel workgroups", () => {
  assert.deepEqual(
    gpuKernelDispatchPlan([
      { entry: "derivative_rhs_map0", rows: 64, workgroupSize: 64 },
      { entry: "derivative_rhs_stencil1", rows: 65, workgroupSize: 64 },
      { entry: "derivative_rhs_chunk0", rows: 1, workgroupSize: 32 },
    ], "test derivative dispatch"),
    [
      { entry: "derivative_rhs_map0", rows: 64, workgroupSize: 64, workgroups: 1 },
      { entry: "derivative_rhs_stencil1", rows: 65, workgroupSize: 64, workgroups: 2 },
      { entry: "derivative_rhs_chunk0", rows: 1, workgroupSize: 32, workgroups: 1 },
    ],
  );
});

test("gpuKernelDispatchPlan rejects dispatches beyond the device limit", () => {
  assert.throws(
    () => gpuKernelDispatchPlan([
      { entry: "derivative_rhs_map0", rows: 129, workgroupSize: 64 },
    ], "test derivative dispatch", 2),
    /dispatch needs 3 workgroups, exceeding device limit 2/,
  );
});

test("gpuKernelWorkgroupBudget accepts empty implicit schedules", () => {
  assert.equal(
    gpuKernelWorkgroupBudget([], "test implicit budget", 64),
    0,
  );
});

test("gpuKernelWorkgroupBudget computes validated implicit schedule budget", () => {
  const { implicit } = gpuKernelSchedules({
    rows: 3,
    workgroup_size: 64,
    chunk_size: 64,
    chunks: 0,
    kernel_count: 1,
    kernels: [nativeKernel()],
    native_families: [nativeFamily()],
    implicit_rhs: {
      rows: 3,
      workgroup_size: 64,
      chunk_size: 64,
      chunks: 0,
      kernel_count: 1,
      kernels: [nativeKernel({ entry: "implicit_rhs_map0" })],
      native_families: [nativeFamily()],
    },
  });

  assert.equal(
    gpuKernelWorkgroupBudget(implicit, "test implicit budget", 64),
    1,
  );
});

test("gpuKernelWorkgroupBudget rejects implicit schedules beyond the device limit", () => {
  assert.throws(
    () => gpuKernelWorkgroupBudget([
      { entry: "implicit_rhs_map0", rows: 65, workgroupSize: 64 },
    ], "test implicit budget", 1),
    /budget needs 2 workgroups, exceeding device limit 1/,
  );
});

test("gpuKernelDispatchPlan rejects empty dispatch schedules", () => {
  assert.throws(
    () => gpuKernelDispatchPlan([], "test derivative dispatch"),
    /has no kernels to dispatch/,
  );
});

test("gpuKernelDispatchPlan rejects malformed dispatch metadata", () => {
  assert.throws(
    () => gpuKernelDispatchPlan([
      { entry: "derivative_rhs_map0", rows: 3, workgroupSize: 0 },
    ], "test derivative dispatch"),
    /invalid workgroupSize metadata/,
  );
});

test("gpuKernelSchedules rejects invalid implicit schedule before GPU build", () => {
  assert.throws(
    () => gpuKernelSchedules({
      rows: 3,
      workgroup_size: 64,
      chunk_size: 64,
      chunks: 0,
      kernel_count: 1,
      kernels: [nativeKernel()],
      native_families: [nativeFamily()],
      implicit_rhs: {
        rows: 3,
        workgroup_size: 64,
        chunk_size: 64,
        chunks: 1,
        kernel_count: 1,
        kernels: [nativeKernel({ entry: "implicit_rhs_map0" })],
        native_families: [nativeFamily()],
      },
    }),
    /chunks=1 does not match 0 scalar implicit RHS kernel entries/,
  );
});
