export function ensureNodeSelfForWasmBindgenRayon() {
  if (typeof globalThis.self === "undefined") {
    globalThis.self = globalThis;
  }

  const target = globalThis.self;
  if (typeof target.addEventListener === "function" && typeof target.removeEventListener === "function") {
    return;
  }

  const messageListeners = new Set();
  target.addEventListener = (type, listener) => {
    if (type === "message" && typeof listener === "function") {
      messageListeners.add(listener);
    }
  };
  target.removeEventListener = (type, listener) => {
    if (type === "message" && typeof listener === "function") {
      messageListeners.delete(listener);
    }
  };

  if (typeof target.postMessage !== "function") {
    target.postMessage = (data) => {
      for (const listener of messageListeners) {
        listener({ data });
      }
    };
  }
}
