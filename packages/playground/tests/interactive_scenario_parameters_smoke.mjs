// Exact authored model/config through the real browser interactive runtime.
import assert from "node:assert/strict";
import { chromium } from "playwright-core";
function argument(name, fallback) {
  const index = process.argv.indexOf(name);
  return index < 0 ? fallback : process.argv[index + 1];
}
const base = argument("--base-url", "http://127.0.0.1:8731");
const browser = await chromium.launch({
  executablePath: argument("--browser-binary", "google-chrome"),
  headless: true,
  args: ["--no-sandbox", "--enable-unsafe-swiftshader"],
});
try {
  const page = await browser.newPage();
  // This focused public-runtime proof uses the authored repository bytes.
  // It does not claim that the current compiler or complete book is qualified.
  const response = await page.goto(`${base}/examples/models/SympyDecay.mo`, { waitUntil: "domcontentloaded" });
  assert.ok(response.ok(), "the exact repository model must be served");
  const traces = await page.evaluate(async () => {
    const wasm = await import("/packages/rumoca/dist/release-full-web/rumoca_bind_wasm.js");
    await wasm.default();
    const THREE = await import("/packages/rumoca-web/vendor/three_viewer.js");
    const runtime = await import("/packages/rumoca/dist/release-full-web/rumoca_interactive.js");
    const root = "/examples/";
    const model = await (await fetch(`${root}models/SympyDecay.mo`)).text();
    const path = "simulation/rumoca-scenario.sympy_decay.toml";
    const text = await (await fetch(root + path)).text();
    const response = JSON.parse(wasm.scenario_get_scenario_config_full(JSON.stringify({ [path]: text }), path));
    if (!response.ok) throw new Error(JSON.stringify(response));
    const traces = [];
    // Retain the original scenario as a control. The second cell changes only
    // its documented numeric parameter configuration, never the Modelica source.
    for (const parameters of [{}, { k: 3 }]) {
      const config = { ...response.config, parameters };
      let session;
      const create = wasm.WasmSimulationSession.withInteractiveOptions;
      wasm.WasmSimulationSession.withInteractiveOptions = (...args) => {
        session = create(...args);
        return session;
      };
      const host = document.createElement("div");
      host.style.cssText = "width:600px;height:400px";
      document.body.append(host);
      let controller;
      try {
        controller = await runtime.createInteractiveSimulation({
          wasm, THREE, source: model, modelName: config.model.name,
          config, container: host,
        });
        if (!session) throw new Error("the actual runtime did not construct its session");
        const samples = [];
        for (const time of [0, 0.05, 0.1]) {
          session.advance_to(time);
          samples.push({ time: session.time(), x: session.get("x") });
        }
        const batch = JSON.parse(wasm.simulate_model(model, config.model.name,
          0.1, 0.05, "rk-like", JSON.stringify(parameters)));
        traces.push({ parameters, samples, batch: batch.payload ?? batch });
      } finally {
        controller?.dispose();
        host.remove();
        wasm.WasmSimulationSession.withInteractiveOptions = create;
      }
    }
    return traces;
  });
  for (const { parameters, samples, batch } of traces) {
    const k = parameters.k ?? 0.5;
    const index = batch.names.indexOf("x");
    assert(index >= 0);
    assert.equal(samples.length, 3);
    samples.forEach(({ time, x }, sample) => {
      const expected = Math.exp(-k * time);
      assert(Number.isFinite(x));
      assert(Math.abs(batch.allData[index + 1][sample] - expected) < 1e-5,
        `checked batch control at k=${k}, t=${time}`);
      assert(Math.abs(x - expected) < 1e-5,
        `interactive configured k=${k}, x(${time})=${x}, expected ${expected}`);
    });
  }
  console.log("PASS: actual authored scenario, configured parameter, independent analytic and checked batch controls");
} finally {
  await browser.close();
}
