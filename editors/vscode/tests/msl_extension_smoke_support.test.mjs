import test from "node:test";
import assert from "node:assert/strict";

import {
  assertMslCompletionCacheProof,
  collectDefinitionUris,
  collectDocumentCompletionTimings,
  collectHoverText,
  findMslCompletionProbe,
  findMslNavigationProbe,
} from "./msl_extension_smoke_support.mjs";

test("collectDocumentCompletionTimings extracts library-load, cold, and warm entries", () => {
  const documentPath = "/tmp/Resistor.mo";
  const raw = [
    JSON.stringify({ uri: "/tmp/Other.mo", totalMs: 10 }),
    JSON.stringify({
      uri: documentPath,
      classNameCountAfterEnsure: 4,
      sessionCacheDelta: { library_completion_cache_misses: 1 },
    }),
    JSON.stringify({
      uri: documentPath,
      classNameCountAfterEnsure: 4,
      sessionCacheDelta: { library_completion_cache_hits: 1 },
    }),
    JSON.stringify({
      uri: documentPath,
      classNameCountAfterEnsure: 4,
      sessionCacheDelta: { library_completion_cache_hits: 1 },
    }),
  ].join("\n");

  const summary = collectDocumentCompletionTimings(raw, documentPath);
  assert.equal(summary.libraryStageTimings.uri, documentPath);
  assert.equal(summary.coldStageTimings.uri, documentPath);
  assert.equal(summary.warmStageTimings.uri, documentPath);
  assert.equal(summary.latestStageTimings.uri, documentPath);
  assert.equal(summary.libraryStageTimings.sessionCacheDelta.library_completion_cache_misses, 1);
  assert.equal(summary.coldStageTimings.sessionCacheDelta.library_completion_cache_hits, 1);
  assert.equal(summary.warmStageTimings.sessionCacheDelta.library_completion_cache_hits, 1);
});

test("collectDocumentCompletionTimings falls back to cold and warm for two entries", () => {
  const documentPath = "/tmp/Resistor.mo";
  const raw = [
    JSON.stringify({
      uri: documentPath,
      classNameCountAfterEnsure: 4,
      sessionCacheDelta: { library_completion_cache_misses: 1 },
    }),
    JSON.stringify({
      uri: documentPath,
      classNameCountAfterEnsure: 4,
      sessionCacheDelta: { library_completion_cache_hits: 1 },
    }),
  ].join("\n");

  const summary = collectDocumentCompletionTimings(raw, documentPath);
  assert.equal(summary.libraryStageTimings, undefined);
  assert.equal(summary.coldStageTimings.uri, documentPath);
  assert.equal(summary.warmStageTimings.uri, documentPath);
  assert.equal(summary.coldStageTimings.sessionCacheDelta.library_completion_cache_misses, 1);
  assert.equal(summary.warmStageTimings.sessionCacheDelta.library_completion_cache_hits, 1);
});

test("findMslCompletionProbe prefers the grounded MSL namespace probe", () => {
  const source = "model R\n  Modelica.Electrical.Analog.Basic.Ground g;\nend R;\n";
  const probe = findMslCompletionProbe(source);
  assert.equal(probe.probeText, "Modelica.");
  assert.equal(source.slice(probe.probeOffset, probe.probeOffset + probe.probeText.length), "Modelica.");
});

test("findMslNavigationProbe targets the terminal MSL symbol", () => {
  const source = "model R\n  Modelica.Electrical.Analog.Basic.Ground g;\nend R;\n";
  const probe = findMslNavigationProbe(source);
  assert.equal(probe.expectedLabel, "Ground");
  assert.equal(source.slice(probe.probeOffset, probe.probeOffset + probe.expectedLabel.length), "Ground");
});

test("collectHoverText normalizes hover provider payloads", () => {
  const hovers = [
    { contents: { value: "**Ground**" } },
    { contents: [{ language: "modelica", value: "model Ground" }] },
  ];
  assert.equal(collectHoverText(hovers), "**Ground**\nmodel Ground");
});

test("collectDefinitionUris reads both uri and targetUri shapes", () => {
  const definitions = [
    { uri: "file:///tmp/A.mo" },
    { targetUri: "file:///tmp/B.mo" },
    { targetUri: { toString: () => "file:///tmp/C.mo" } },
    { foo: "bar" },
  ];
  assert.deepEqual(collectDefinitionUris(definitions), [
    "file:///tmp/A.mo",
    "file:///tmp/B.mo",
    "file:///tmp/C.mo",
  ]);
});

test("assertMslCompletionCacheProof accepts a valid warm cache proof", () => {
  const summary = {
    libraryStageTimings: {
      uri: "/tmp/Resistor.mo",
      builtResolvedTree: false,
      classNameCountAfterEnsure: 42,
      sessionCacheDelta: {
        library_completion_cache_hits: 1,
        library_completion_cache_misses: 0,
        standard_resolved_builds: 0,
        semantic_navigation_builds: 0,
      },
    },
    coldStageTimings: {
      uri: "/tmp/Resistor.mo",
      builtResolvedTree: false,
      classNameCountAfterEnsure: 42,
      sessionCacheDelta: {
        library_completion_cache_hits: 1,
        library_completion_cache_misses: 0,
        library_files_parsed: 0,
        standard_resolved_builds: 0,
        semantic_navigation_builds: 0,
      },
    },
    warmStageTimings: {
      uri: "/tmp/Resistor.mo",
      builtResolvedTree: false,
      classNameCountAfterEnsure: 42,
      sessionCacheDelta: {
        library_completion_cache_hits: 1,
        library_completion_cache_misses: 0,
        library_files_parsed: 0,
        standard_resolved_builds: 0,
        semantic_navigation_builds: 0,
      },
    },
  };

  assert.doesNotThrow(() => assertMslCompletionCacheProof(summary, "/tmp/Resistor.mo"));
});

test("assertMslCompletionCacheProof rejects warm cache regressions", () => {
  const summary = {
    libraryStageTimings: {
      uri: "/tmp/Resistor.mo",
      builtResolvedTree: false,
      classNameCountAfterEnsure: 42,
      sessionCacheDelta: {
        library_completion_cache_hits: 1,
        library_completion_cache_misses: 0,
        standard_resolved_builds: 0,
        semantic_navigation_builds: 0,
      },
    },
    coldStageTimings: {
      uri: "/tmp/Resistor.mo",
      builtResolvedTree: false,
      classNameCountAfterEnsure: 42,
      sessionCacheDelta: {
        library_completion_cache_hits: 1,
        library_completion_cache_misses: 0,
        library_files_parsed: 0,
        standard_resolved_builds: 0,
        semantic_navigation_builds: 0,
      },
    },
    warmStageTimings: {
      uri: "/tmp/Resistor.mo",
      builtResolvedTree: false,
      classNameCountAfterEnsure: 42,
      sessionCacheDelta: {
        library_completion_cache_hits: 0,
        library_completion_cache_misses: 1,
        library_files_parsed: 3,
        standard_resolved_builds: 0,
        semantic_navigation_builds: 0,
      },
    },
  };

  assert.throws(
    () => assertMslCompletionCacheProof(summary, "/tmp/Resistor.mo"),
    /warm completion should hit the library completion cache/,
  );
});
