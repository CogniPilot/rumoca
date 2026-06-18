import test from "node:test";
import assert from "node:assert/strict";

import { createNotebookControllerRuntime } from "../out/notebook_controller_runtime.js";

test("configuration changes can create the notebook controller after an initial missing executable", async () => {
  let refreshCount = 0;
  let createCount = 0;
  let executable = undefined;

  const runtime = createNotebookControllerRuntime({
    refreshConfig: () => {
      refreshCount += 1;
    },
    languageClientRuntime: {
      getNotebookExecutable: () => executable,
      handleConfigurationChange: async (affectsConfiguration) => {
        if (affectsConfiguration("rumoca.serverPath")) {
          executable = "/tmp/rumoca";
          return ["serverPath"];
        }
        return [];
      },
    },
    fileExists: (targetPath) => targetPath === "/tmp/rumoca",
    createNotebookController: () => {
      createCount += 1;
      return { dispose() {} };
    },
    debugLog: () => {},
  });

  assert.equal(runtime.reconcileNotebookController(), false);

  const changedKeys = await runtime.handleConfigurationChange(
    (section) => section === "rumoca.serverPath",
  );

  assert.deepEqual(changedKeys, ["serverPath"]);
  assert.equal(refreshCount, 1);
  assert.equal(createCount, 1);
  assert.equal(runtime.reconcileNotebookController(), true);
});

test("configuration changes dispose the notebook controller when restart leaves no executable", async () => {
  let createCount = 0;
  let disposeCount = 0;
  let executable = "/tmp/rumoca";

  const runtime = createNotebookControllerRuntime({
    refreshConfig: () => {},
    languageClientRuntime: {
      getNotebookExecutable: () => executable,
      handleConfigurationChange: async (affectsConfiguration) => {
        if (affectsConfiguration("rumoca.useSystemServer")) {
          executable = undefined;
          return ["useSystemServer"];
        }
        return [];
      },
    },
    fileExists: (targetPath) => targetPath === "/tmp/rumoca",
    createNotebookController: () => {
      createCount += 1;
      return {
        dispose() {
          disposeCount += 1;
        },
      };
    },
    debugLog: () => {},
  });

  assert.equal(runtime.reconcileNotebookController(), true);

  const changedKeys = await runtime.handleConfigurationChange(
    (section) => section === "rumoca.useSystemServer",
  );

  assert.deepEqual(changedKeys, ["useSystemServer"]);
  assert.equal(createCount, 1);
  assert.equal(disposeCount, 1);
  assert.equal(runtime.reconcileNotebookController(), false);
});

test("irrelevant configuration changes leave the notebook controller untouched", async () => {
  let refreshCount = 0;
  let createCount = 0;
  let disposeCount = 0;

  const runtime = createNotebookControllerRuntime({
    refreshConfig: () => {
      refreshCount += 1;
    },
    languageClientRuntime: {
      getNotebookExecutable: () => "/tmp/rumoca",
      handleConfigurationChange: async () => [],
    },
    fileExists: (targetPath) => targetPath === "/tmp/rumoca",
    createNotebookController: () => {
      createCount += 1;
      return {
        dispose() {
          disposeCount += 1;
        },
      };
    },
    debugLog: () => {},
  });

  const changedKeys = await runtime.handleConfigurationChange(
    (section) => section === "rumoca.simulation.dt",
  );

  assert.deepEqual(changedKeys, []);
  assert.equal(refreshCount, 1);
  assert.equal(createCount, 0);
  assert.equal(disposeCount, 0);
});
