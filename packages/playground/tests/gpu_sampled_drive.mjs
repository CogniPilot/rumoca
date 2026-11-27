// Runs in the hardware-admitted browser against the actual packaged compiler and driver.
export async function checkSampledDrive() {
  const [compiler, runtime] = await Promise.all([
    import("/packages/rumoca/dist/release-full-web/rumoca_bind_wasm.js"),
    import("/packages/rumoca/dist/release-full-web/rumoca_gpu.js"),
  ]);
  await compiler.default();
  const source = `model SampledDrive
    Real x(start=0, fixed=true);
    discrete Real drive(start=2, fixed=true);
  equation
    der(x)=drive;
    when sample(0.05,0.1) then drive=pre(drive)+1; end when;
  end SampledDrive;`;
  const adapter = await navigator.gpu.requestAdapter({powerPreference: "high-performance"});
  for (const phase of [0.05, 0]) {
    const model = source.replace("sample(0.05,", "sample(" + phase + ",");
    const prepared = JSON.parse(compiler.prepare_gpu_simulation(model, "SampledDrive"));
    Object.assign(prepared, {t_start: 0, t_end: 0.427, dt: 0.037, internal_dt: 0.023});
    const {payload, metrics} = await runtime.runGpuSimulation(adapter, prepared);
    const [time, x] = payload.allData;
    const ticks = phase === 0 ? 5 : 4;
    if (metrics.profile.periodicTicks !== ticks || !time.every((t, i) => {
      let exact = 2 * t;
      for (let tick = 0; tick < ticks; tick++) exact += Math.max(0, t - (phase + 0.1 * tick));
      return Math.abs(x[i] - exact) < 2e-6;
    })) return false;
  }
  return true;
}
