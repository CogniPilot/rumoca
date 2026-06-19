import assert from "node:assert/strict";
import test from "node:test";

import {
  workspaceModelicaSourceMap,
  workspaceModelicaSourcesJson,
  workspaceSourceRootSourceMap,
  workspaceSourceRootSourcesJson,
} from "../viz/visualization_shared.js";

const entries = [
  {
    path: "examples/models/Ball.mo",
    content: "model Ball\nend Ball;\n",
  },
  {
    path: "examples/simulation/Scenario.mo",
    content: "model Scenario\nend Scenario;\n",
  },
  {
    path: "target/cmm/CMM-v0.0.2/RigidBody/package.mo",
    content: "package RigidBody\nend RigidBody;\n",
  },
  {
    path: "target/cmm/CMM-v0.0.2/LieGroup/package.mo",
    content: "package LieGroup\nend LieGroup;\n",
  },
  {
    path: "archives/Modelica/package.mo",
    sourceKind: "packageArchive",
    content: "package Modelica\nend Modelica;\n",
  },
  {
    path: "README.md",
    content: "# README\n",
  },
];

test("workspace Modelica source map excludes primary source, source roots, and package archives", () => {
  const sources = workspaceModelicaSourceMap(entries, {
    excludePath: "examples/models/Ball.mo",
    excludeSourceRootPaths: ["target/cmm/CMM-v0.0.2"],
  });

  assert.deepEqual(Object.keys(sources), ["examples/simulation/Scenario.mo"]);
  assert.equal(sources["examples/simulation/Scenario.mo"], "model Scenario\nend Scenario;\n");
});

test("workspace Modelica source JSON mirrors the source map helper", () => {
  const sources = JSON.parse(workspaceModelicaSourcesJson(entries, {
    excludePath: "examples/models/Ball.mo",
    excludeSourceRootPaths: ["target/cmm/CMM-v0.0.2"],
  }));

  assert.deepEqual(Object.keys(sources), ["examples/simulation/Scenario.mo"]);
});

test("workspace source-root map includes only requested root Modelica files", () => {
  const sources = workspaceSourceRootSourceMap(entries, ["target/cmm/CMM-v0.0.2/RigidBody"]);

  assert.deepEqual(Object.keys(sources), [
    "target/cmm/CMM-v0.0.2/RigidBody/package.mo",
  ]);
});

test("workspace source-root JSON normalizes relative root paths", () => {
  const sources = JSON.parse(workspaceSourceRootSourcesJson(
    entries,
    ["target/cmm/CMM-v0.0.2/../CMM-v0.0.2/LieGroup"],
  ));

  assert.deepEqual(Object.keys(sources), [
    "target/cmm/CMM-v0.0.2/LieGroup/package.mo",
  ]);
});
