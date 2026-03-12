import test from "node:test";
import assert from "node:assert/strict";
import path from "node:path";

import { createLanguageClientRuntime } from "../out/language_client_runtime.js";

test("configuration restart stops the old client and updates the notebook executable", async () => {
  const stoppedClientIds = [];
  let currentClient = { id: "client-0" };
  let startCallCount = 0;

  const runtime = createLanguageClientRuntime({
    getClient: () => currentClient,
    setClient: (client) => {
      currentClient = client;
    },
    startLanguageClient: async () => {
      startCallCount += 1;
      currentClient = { id: `client-${startCallCount}` };
      return {
        clientStarted: true,
        serverPath: path.join(path.sep, "tmp", "servers", `rumoca-${startCallCount}`, "rumoca-lsp"),
      };
    },
    stopLanguageClient: async (client) => {
      stoppedClientIds.push(client.id);
    },
    log: () => {},
    reportError: (msg) => {
      throw new Error(msg);
    },
  });

  runtime.setServerPath(path.join(path.sep, "tmp", "servers", "rumoca-0", "rumoca-lsp"));

  const changedKeys = await runtime.handleConfigurationChange(
    (section) => section === "rumoca.modelicaPath",
  );

  assert.deepEqual(changedKeys, ["modelicaPath"]);
  assert.equal(startCallCount, 1);
  assert.deepEqual(stoppedClientIds, ["client-0"]);
  assert.equal(
    runtime.getNotebookExecutable(),
    path.join(path.sep, "tmp", "servers", "rumoca-1", "rumoca"),
  );
});

test("failed restart clears the stale notebook executable path", async () => {
  let currentClient = { id: "client-0" };

  const runtime = createLanguageClientRuntime({
    getClient: () => currentClient,
    setClient: (client) => {
      currentClient = client;
    },
    startLanguageClient: async () => ({ clientStarted: false }),
    stopLanguageClient: async () => {},
    log: () => {},
    reportError: (msg) => {
      throw new Error(msg);
    },
  });

  runtime.setServerPath(path.join(path.sep, "tmp", "servers", "rumoca-0", "rumoca-lsp"));

  const changedKeys = await runtime.handleConfigurationChange(
    (section) => section === "rumoca.serverPath",
  );

  assert.deepEqual(changedKeys, ["serverPath"]);
  assert.equal(runtime.getNotebookExecutable(), undefined);
});

test("irrelevant configuration changes do not restart the language client", async () => {
  let startCallCount = 0;
  let stopCallCount = 0;

  const runtime = createLanguageClientRuntime({
    getClient: () => ({ id: "client-0" }),
    setClient: () => {},
    startLanguageClient: async () => {
      startCallCount += 1;
      return { clientStarted: true, serverPath: "/tmp/rumoca-lsp" };
    },
    stopLanguageClient: async () => {
      stopCallCount += 1;
    },
    log: () => {},
    reportError: (msg) => {
      throw new Error(msg);
    },
  });

  const changedKeys = await runtime.handleConfigurationChange(
    (section) => section === "rumoca.simulation.dt",
  );

  assert.deepEqual(changedKeys, []);
  assert.equal(startCallCount, 0);
  assert.equal(stopCallCount, 0);
});
