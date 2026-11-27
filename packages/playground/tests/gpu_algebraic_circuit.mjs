// Run the repository's RC circuit on the hardware-admitted browser adapter.
export async function checkAlgebraicCircuit(source) {
  const [compiler, runtime] = await Promise.all([
    import("/packages/rumoca/dist/release-full-web/rumoca_bind_wasm.js"),
    import("/packages/rumoca/dist/release-full-web/rumoca_gpu.js"),
  ]);
  await compiler.default();
  const prep = JSON.parse(compiler.prepare_gpu_simulation(source, "Circuit.Test"));
  Object.assign(prep, {t_start: 0, t_end: 0.125, dt: 0.007, internal_dt: 0.0008});
  // Only initial continuous states supply initial data. Stale algebraics must
  // disappear before the very first observation and all derivative evaluations.
  prep.y0.fill(-123, prep.n_states);
  const adapter = await navigator.gpu.requestAdapter({powerPreference: "high-performance"});
  const {payload, metrics} = await runtime.runGpuSimulation(adapter, prep);
  const trace = (name) => {
    const column = payload.names.indexOf(name);
    if (column < 0) throw new Error("Missing circuit variable " + name);
    return payload.allData[column + 1];
  };
  const voltage = trace("cap.v");
  const current = trace("res.i");
  const supply = trace("src.v");
  return metrics.profile.algebraicDispatches > 4 * voltage.length
    && payload.allData[0].every((t, i) => {
      const expected = 5 + (voltage[0] - 5) * Math.exp(-t / 0.025);
      return Math.abs(expected - voltage[i]) < 4e-5
        && Math.abs(current[i] - (5 - expected) / 2.5) < 4e-5
        && Math.abs(supply[i] - 5) < 1e-6;
    });
}
