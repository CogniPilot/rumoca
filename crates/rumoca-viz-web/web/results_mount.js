// Read-only results report bootstrap.
//
// Mounts the shared results app (visualization_shared.js + results_app.js)
// against an inlined simulation payload, for the self-contained HTML report
// produced by the CLI (`rumoca-viz-web::build_results_html_document`). The Rust
// side injects the model/payload/views/metrics JSON and calls
// `RumocaResultsReport.mount(...)`; keeping this logic in a real JS file (rather
// than Rust string literals) makes it lintable and shared.
(function (global) {
  function readStoredJson(key) {
    try {
      const raw = global.localStorage ? global.localStorage.getItem(key) : null;
      return raw ? JSON.parse(raw) : null;
    } catch (_) {
      return null;
    }
  }

  function writeStoredJson(key, value) {
    if (!global.localStorage) return;
    try {
      global.localStorage.setItem(key, JSON.stringify(value));
    } catch (_) {
      // ignore storage failures
    }
  }

  function mount(config) {
    const root = config.root || global.document.getElementById('resultsRoot');
    const modelName = config.model;
    const storageKey = ['rumoca-results', window.location.pathname || 'inline', modelName, 'state'].join(':');
    const persistedState = readStoredJson(storageKey) || {};
    const bridge = {
      notify(message) {
        if (message) {
          console.info('[rumoca-results]', message);
        }
      },
      persistState(nextState) {
        writeStoredJson(storageKey, Object.assign({}, readStoredJson(storageKey) || {}, nextState || {}));
      },
    };
    const app = global.RumocaResultsApp.createResultsApp({
      root,
      model: modelName,
      modelRef: { model: modelName },
      payload: config.payload,
      views: config.views,
      metrics: config.metrics,
      activeViewId: typeof persistedState.activeViewId === 'string' ? persistedState.activeViewId : undefined,
      bridge,
      allowViewEditing: false,
    });
    global.addEventListener('beforeunload', () => {
      if (app && typeof app.dispose === 'function') {
        app.dispose();
      }
    });
    return app;
  }

  global.RumocaResultsReport = { mount };
})(globalThis);
