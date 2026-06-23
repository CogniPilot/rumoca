// Read-only results report bootstrap for self-contained CLI HTML reports.
import { createResultsApp } from './results_app.js';

  function readStoredJson(key) {
    try {
      const raw = globalThis.localStorage ? globalThis.localStorage.getItem(key) : null;
      return raw ? JSON.parse(raw) : null;
    } catch (_) {
      return null;
    }
  }

  function writeStoredJson(key, value) {
    if (!globalThis.localStorage) return;
    try {
      globalThis.localStorage.setItem(key, JSON.stringify(value));
    } catch (_) {
      // ignore storage failures
    }
  }

  export function mount(config) {
    const root = config.root || globalThis.document.getElementById('resultsRoot');
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
    const app = createResultsApp({
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
    globalThis.addEventListener('beforeunload', () => {
      if (app && typeof app.dispose === 'function') {
        app.dispose();
      }
    });
    return app;
  }
