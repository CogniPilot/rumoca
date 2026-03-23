import test from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs";
import path from "node:path";
import { createRequire } from "node:module";

const require = createRequire(import.meta.url);

function loadResultsApp() {
  const shared = require(path.resolve("media", "vendor", "visualization_shared.js"));
  globalThis.RumocaVisualizationShared = shared;
  const resultsApp = require(path.resolve("media", "vendor", "results_app.js"));
  return { resultsApp, shared };
}

function createFakeElement(tagName) {
  const listeners = new Map();
  const classNames = new Set();
  const element = {
    tagName,
    children: [],
    parentNode: null,
    style: {},
    dataset: {},
    attributes: {},
    value: "",
    textContent: "",
    _className: "",
    appendChild(child) {
      child.parentNode = element;
      element.children.push(child);
      return child;
    },
    removeChild(child) {
      const index = element.children.indexOf(child);
      if (index >= 0) {
        element.children.splice(index, 1);
        child.parentNode = null;
      }
      return child;
    },
    addEventListener(type, listener) {
      listeners.set(type, listener);
    },
    removeEventListener(type) {
      listeners.delete(type);
    },
    dispatchEvent(event) {
      const type = typeof event === "string" ? event : event?.type;
      if (!type) {
        return false;
      }
      const listener = listeners.get(type);
      if (typeof listener === "function") {
        listener(event ?? { type, target: element });
        return true;
      }
      return false;
    },
    click() {
      return element.dispatchEvent({ type: "click", target: element });
    },
    setAttribute(name, value) {
      element.attributes[name] = String(value);
    },
    getAttribute(name) {
      return Object.prototype.hasOwnProperty.call(element.attributes, name)
        ? element.attributes[name]
        : null;
    },
  };
  Object.defineProperty(element, "innerHTML", {
    get() {
      return "";
    },
    set(_value) {
      element.children = [];
    },
  });
  Object.defineProperty(element, "className", {
    get() {
      return Array.from(classNames).join(" ");
    },
    set(value) {
      classNames.clear();
      String(value || "").split(/\s+/).filter(Boolean).forEach((entry) => classNames.add(entry));
    },
  });
  element.classList = {
    add(...entries) {
      entries.filter(Boolean).forEach((entry) => classNames.add(entry));
    },
    remove(...entries) {
      entries.forEach((entry) => classNames.delete(entry));
    },
    toggle(entry, force) {
      if (force === true) {
        classNames.add(entry);
        return true;
      }
      if (force === false) {
        classNames.delete(entry);
        return false;
      }
      if (classNames.has(entry)) {
        classNames.delete(entry);
        return false;
      }
      classNames.add(entry);
      return true;
    },
    contains(entry) {
      return classNames.has(entry);
    },
  };
  return element;
}

function installFakeDom() {
  const previous = {
    document: globalThis.document,
    requestAnimationFrame: globalThis.requestAnimationFrame,
    addEventListener: globalThis.addEventListener,
    removeEventListener: globalThis.removeEventListener,
  };
  globalThis.document = {
    createElement(tagName) {
      return createFakeElement(tagName);
    },
  };
  globalThis.requestAnimationFrame = (callback) => {
    callback();
    return 1;
  };
  globalThis.addEventListener = () => {};
  globalThis.removeEventListener = () => {};
  return () => {
    globalThis.document = previous.document;
    globalThis.requestAnimationFrame = previous.requestAnimationFrame;
    globalThis.addEventListener = previous.addEventListener;
    globalThis.removeEventListener = previous.removeEventListener;
  };
}

function collectText(node) {
  const texts = [];
  if (typeof node.textContent === "string" && node.textContent.length > 0) {
    texts.push(node.textContent);
  }
  for (const child of node.children || []) {
    texts.push(...collectText(child));
  }
  return texts;
}

function containsClass(node, className) {
  if (node.classList?.contains?.(className)) {
    return true;
  }
  return (node.children || []).some((child) => containsClass(child, className));
}

function findElementByText(node, text) {
  if (node?.textContent === text) {
    return node;
  }
  for (const child of node.children || []) {
    const match = findElementByText(child, text);
    if (match) {
      return match;
    }
  }
  return null;
}

test("shared results app exports pure helpers for host bridges", () => {
  const { resultsApp } = loadResultsApp();

  assert.deepEqual(
    resultsApp.buildResultsTimingSummary({
      compileSeconds: 0.5,
      simulateSeconds: 1.25,
      points: 42,
      variables: 7,
    }),
    ["compile 0.5s", "simulate 1.25s", "points 42", "vars 7"],
  );
  assert.equal(
    resultsApp.chooseActiveViewId(
      [{ id: "view_a" }, { id: "view_b" }],
      "view_b",
    ),
    "view_b",
  );
  assert.equal(
    resultsApp.chooseActiveViewId(
      [{ id: "view_a" }, { id: "view_b" }],
      "missing",
    ),
    "view_a",
  );
});

test("shared results app bridge helper preserves host overrides and fills noops", () => {
  const { resultsApp } = loadResultsApp();

  let persisted = null;
  const bridge = resultsApp.createNoopResultsHostBridge({
    persistState(next) {
      persisted = next;
    },
  });

  bridge.persistState({ activeViewId: "states_time" });
  bridge.notify("ok");

  assert.deepEqual(persisted, { activeViewId: "states_time" });
  assert.deepEqual(resultsApp.RESULTS_APP_BRIDGE_METHODS, [
    "loadViews",
    "saveViews",
    "resetViews",
    "savePng",
    "saveWebm",
    "notify",
  ]);
  assert.equal(typeof bridge.notify, "function");
  assert.equal(typeof bridge.loadViews, "function");
  assert.equal(typeof bridge.saveViews, "function");
  assert.equal(typeof bridge.resetViews, "function");
  assert.equal(bridge.savePng, null);
  assert.equal(bridge.saveWebm, null);
});

test("shared results app draft helpers normalize timeseries, scatter, and 3d views", () => {
  const { resultsApp, shared } = loadResultsApp();

  assert.deepEqual(resultsApp.parseSeriesList("time, x\n y "), ["time", "x", "y"]);
  assert.deepEqual(resultsApp.parseScatterSeriesText("traj | time | x\nspeed | time | v"), [
    { name: "traj", x: "time", y: "x" },
    { name: "speed", x: "time", y: "v" },
  ]);

  const normalized = resultsApp.normalizeResultsViewDrafts(shared, [
    {
      id: "states_time",
      title: "States",
      type: "timeseries",
      x: "time",
      yText: "x, y",
    },
    {
      id: "scatter_main",
      title: "Scatter",
      type: "scatter",
      scatterSeriesText: "x vs time | time | x",
    },
    {
      id: "viewer_3d",
      title: "Viewer",
      type: "3d",
      x: "x",
      yText: "y, z, ignored",
      script: "ctx.onFrame = () => {};",
      scriptPath: ".rumoca/models/by-id/uuid/viewer_3d.js",
    },
  ]);

  assert.deepEqual(normalized[0], {
    id: "states_time",
    title: "States",
    type: "timeseries",
    x: "time",
    y: ["x", "y"],
  });
  assert.deepEqual(normalized[1].scatterSeries, [
    { name: "x vs time", x: "time", y: "x" },
  ]);
  assert.equal(normalized[2].x, undefined);
  assert.deepEqual(normalized[2].y, []);
  assert.equal(normalized[2].script, "ctx.onFrame = () => {};");
  assert.equal(normalized[2].scriptPath, ".rumoca/models/by-id/uuid/viewer_3d.js");
});

test("shared results app default views match the shared saved-view model", () => {
  const { resultsApp } = loadResultsApp();

  assert.deepEqual(resultsApp.defaultResultsView("timeseries", 0), {
    id: "states_time",
    title: "States vs Time",
    type: "timeseries",
    x: "time",
    y: ["*states"],
  });
  assert.deepEqual(resultsApp.defaultResultsView("scatter", 1), {
    id: "scatter_2",
    title: "Scatter",
    type: "scatter",
    x: "time",
    y: ["x"],
    scatterSeries: [{ name: "x vs time", x: "time", y: "x" }],
  });
  assert.deepEqual(resultsApp.defaultResultsView("3d", 2), {
    id: "viewer_3",
    title: "3D View",
    type: "3d",
  });
});

test("shared results app hides editable settings UI in read-only mode", () => {
  const restoreDom = installFakeDom();
  try {
    const { resultsApp } = loadResultsApp();
    const root = createFakeElement("div");
    const app = resultsApp.createResultsApp({
      root,
      model: "Ball",
      views: [{ id: "states_time", title: "States vs Time", type: "timeseries", x: "time", y: ["*states"] }],
      payload: null,
      metrics: null,
      allowViewEditing: false,
    });

    const texts = collectText(root);
    assert.ok(!texts.includes("Settings"));
    assert.ok(containsClass(root, "rumoca-results-empty"));
    assert.ok(!containsClass(root, "rumoca-results-settings"));

    app.dispose();
  } finally {
    restoreDom();
  }
});

test("shared results app renders gear action in editable mode", () => {
  const restoreDom = installFakeDom();
  try {
    const { resultsApp } = loadResultsApp();
    const root = createFakeElement("div");
    const app = resultsApp.createResultsApp({
      root,
      model: "Ball",
      views: [{ id: "states_time", title: "States vs Time", type: "timeseries", x: "time", y: ["*states"] }],
      payload: null,
      metrics: null,
    });

    const texts = collectText(root);
    assert.ok(texts.includes("⚙"));
    assert.ok(!texts.includes("Settings"));
    assert.ok(containsClass(root, "rumoca-results-header-button"));

    app.dispose();
  } finally {
    restoreDom();
  }
});

test("shared results app keeps run metrics in details and omits the top timing strip", () => {
  const restoreDom = installFakeDom();
  try {
    const { resultsApp } = loadResultsApp();
    const root = createFakeElement("div");
    const app = resultsApp.createResultsApp({
      root,
      model: "Ball",
      views: [{ id: "states_time", title: "States vs Time", type: "timeseries", x: "time", y: ["x"] }],
      payload: {
        version: 1,
        names: ["x"],
        allData: [[0, 1], [1, 2]],
        nStates: 1,
        variableMeta: [],
        simDetails: {
          actual: { t_start: 0, t_end: 10 },
          requested: { solver: "bdf", dt: 0.1 },
        },
      },
      metrics: {
        compileSeconds: 0.5,
        simulateSeconds: 1.25,
        points: 42,
        variables: 7,
      },
    });

    assert.ok(
      !containsClass(root, "rumoca-results-timing"),
      "results header should not render a separate timing strip",
    );

    const detailsButton = findElementByText(root, "Run Details");
    assert.ok(detailsButton, "expected run details button");
    detailsButton.click();

    const texts = collectText(root);
    const detailsText = texts.find((entry) => entry.includes("Run\n  compile: 0.5s"));
    assert.ok(detailsText, "expected run details modal text");
    assert.match(detailsText, /Series: 1/);
    assert.match(detailsText, /Run\n  compile: 0.5s\n  simulate: 1.25s\n  points: 42\n  vars: 7/);
    assert.match(detailsText, /Actual\n  t_start: 0\n  t_end: 10/);
    assert.match(detailsText, /Requested\n  solver: bdf\n  dt: 0.1/);

    app.dispose();
  } finally {
    restoreDom();
  }
});

test("VS Code results webview boots the shared results app instead of an inline renderer", () => {
  const extensionSource = fs.readFileSync(path.resolve("src", "extension.ts"), "utf8");
  const sharedSource = fs.readFileSync(path.resolve("media", "vendor", "visualization_shared.js"), "utf8");

  assert.match(extensionSource, /buildHostedResultsDocument/);
  assert.match(extensionSource, /results_app\.js/);
  assert.match(sharedSource, /RumocaResultsApp\.createResultsApp/);
  assert.match(sharedSource, /results\.request/);
  assert.doesNotMatch(extensionSource, /function createViewer3dView\(/);
});
