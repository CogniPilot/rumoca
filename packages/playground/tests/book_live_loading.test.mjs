import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import test from "node:test";
import vm from "node:vm";

// Execute the shipped widget with a held network response. Compilation remains
// outside this test: it must never be offered before the source reaches its editor.
async function loadingWidget(directives, failureAt = null) {
  const elements = [];
  function element() {
    const node = {
      children: [], dataset: {}, textContent: "", value: "",
      append(...children) { this.children.push(...children); },
      appendChild(child) { this.append(child); },
      replaceWith() {}, setAttribute() {}, addEventListener() {},
      replaceChildren() {}, style: {}, classList: { toggle() {} },
    };
    elements.push(node);
    return node;
  }
  let releaseSource;
  let sourceRequested;
  const requested = new Promise((resolve) => { sourceRequested = resolve; });
  const response = new Promise((resolve) => { releaseSource = resolve; });
  const window = {
    location: new URL("http://localhost/docs/user-guide/book/simulation/interactive.html"),
    testWasm: {
      scenario_get_scenario_config_full: () => JSON.stringify(failureAt === "parse" ? {
        ok: false, error: "invalid scenario",
      } : {
        ok: true, config: { model: { file: "FixedWingSIL.mo", name: "FixedWing" } },
      }),
    },
  };
  const document = {
    readyState: "loading", addEventListener() {},
    createElement: element, createTextNode: (textContent) => ({ textContent }),
  };
  const context = vm.createContext({
    window, document, URL, console, performance, AbortController, setInterval, clearInterval,
    fetch: async (url) => {
      if (url.endsWith("scenario.toml")) {
        return failureAt === "fetch"
          ? { ok: false, status: 404, statusText: "Not Found" }
          : { ok: true, text: async () => "scenario" };
      }
      if (!url.endsWith(".mo")) return { ok: false };
      sourceRequested();
      return response;
    },
  });
  const source = await readFile(new URL("../../../docs/user-guide/live/rumoca-live.js", import.meta.url), "utf8");
  vm.runInContext(source.replace("const withWasm = async", "const withWasm = window.executeTestAction = async")
    .replace("    async function init() {", `
    window.buildTestWidget = buildWidget;
    loadWasm = async () => window.testWasm;
    loadDiffsolDriver = async () => null;
    async function init() {`), context);
  const widget = window.buildTestWidget({
    textContent: directives, className: "language-modelica interactive", parentElement: element(),
  }, null);
  if (!failureAt) await requested;
  // Drain scenario parsing and availability updates before inspecting the held fetch.
  await new Promise((resolve) => setImmediate(resolve));
  const run = elements.find((node) => node.className?.includes("rumoca-live-run"));
  const dae = elements.find((node) => node.textContent === "Show DAE");
  return { widget, run, dae, releaseSource, execute: window.executeTestAction };
}

test("scenario actions wait for the referenced Modelica source", async () => {
  const { widget, run, dae, releaseSource } = await loadingWidget("// rumoca-live-scenario: scenario.toml");
  assert.equal(run.disabled, true, "scenario metadata is not yet an executable source");
  assert.equal(dae.disabled, true);
  releaseSource({ ok: true, text: async () => "model FixedWing\nend FixedWing;\n" });
  await new Promise((resolve) => setImmediate(resolve));
  assert.equal(widget.getSource(), "model FixedWing\nend FixedWing;");
  assert.equal(run.disabled, false);
  assert.equal(dae.disabled, false);
});

test("a failed external source keeps compilation unavailable", async () => {
  const { run, dae, releaseSource } = await loadingWidget("// rumoca-live-scenario: scenario.toml");
  releaseSource({ ok: false, status: 404, statusText: "Not Found" });
  await new Promise((resolve) => setImmediate(resolve));
  assert.equal(run.disabled, true);
  assert.equal(dae.disabled, true);
});

test("direct source directives become executable only after source installation", async () => {
  const { widget, run, dae, releaseSource } = await loadingWidget("// rumoca-live-source: FixedWingSIL.mo");
  assert.equal(run.disabled, true);
  assert.equal(dae.disabled, true);
  releaseSource({ ok: true, text: async () => "model Direct\nend Direct;\n" });
  await new Promise((resolve) => setImmediate(resolve));
  assert.equal(widget.getSource(), "model Direct\nend Direct;");
  assert.equal(run.disabled, false);
  assert.equal(dae.disabled, false);
});

test("failed direct sources remain unavailable", async () => {
  const { run, dae, releaseSource } = await loadingWidget("// rumoca-live-source: FixedWingSIL.mo");
  releaseSource({ ok: false, status: 404, statusText: "Not Found" });
  await new Promise((resolve) => setImmediate(resolve));
  assert.equal(run.disabled, true);
  assert.equal(dae.disabled, true);
});

for (const failureAt of ["fetch", "parse"]) {
  test(`scenario ${failureAt} failure never enables execution`, async () => {
    const { run, dae } = await loadingWidget("// rumoca-live-scenario: scenario.toml", failureAt);
    assert.equal(run.disabled, true);
    assert.equal(dae.disabled, true);
  });
}

test("delayed libraries retain the source and model selected at execution", async () => {
  const { widget, releaseSource, execute } = await loadingWidget("// rumoca-live-scenario: scenario.toml");
  let called = false;
  await execute("test", "Loading", async () => { called = true; });
  assert.equal(called, false, "programmatic execution cannot bypass source readiness");
  const original = "model FixedWing\nend FixedWing;";
  releaseSource({ ok: true, text: async () => original });
  await new Promise((resolve) => setImmediate(resolve));
  let releaseLibrary;
  const library = new Promise((resolve) => { releaseLibrary = resolve; });
  let entered;
  const started = new Promise((resolve) => { entered = resolve; });
  let selected;
  const running = execute("test", "Loading", async (_wasm, source, model) => {
    entered();
    await library;
    selected = { source, model };
  });
  await started;
  widget.setSource("model LaterEdit\nend LaterEdit;");
  releaseLibrary();
  await running;
  assert.deepEqual(selected, { source: original, model: "FixedWing" });
});
