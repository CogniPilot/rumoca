function cacheDelta(entry) {
  return entry?.sessionCacheDelta ?? {};
}

export function findMslCompletionProbe(sourceText) {
  const preferredProbe = "Modelica.Electrical.Analog.Basic.Ground";
  const preferredOffset = sourceText.indexOf(preferredProbe);
  if (preferredOffset >= 0) {
    return {
      probeText: "Modelica.",
      probeOffset: preferredOffset,
    };
  }

  const fallbackProbe = "Modelica.";
  const fallbackOffset = sourceText.indexOf(fallbackProbe);
  if (fallbackOffset < 0) {
    throw new Error(`expected fixture to contain ${fallbackProbe}`);
  }
  return {
    probeText: fallbackProbe,
    probeOffset: fallbackOffset,
  };
}

export function findMslNavigationProbe(sourceText) {
  const preferredPath = "Modelica.Electrical.Analog.Basic.Ground";
  const preferredLabel = "Ground";
  const preferredOffset = sourceText.indexOf(preferredPath);
  if (preferredOffset >= 0) {
    return {
      probeOffset: preferredOffset + preferredPath.lastIndexOf(preferredLabel),
      expectedLabel: preferredLabel,
    };
  }

  const fallback = sourceText.match(/\bModelica(?:\.[A-Z][A-Za-z0-9_]*)+\b/);
  if (!fallback?.[0]) {
    throw new Error("expected fixture to contain a qualified Modelica symbol");
  }
  const probePath = fallback[0];
  const expectedLabel = probePath.split(".").at(-1);
  if (!expectedLabel) {
    throw new Error("qualified Modelica symbol should have a terminal segment");
  }
  return {
    probeOffset: fallback.index + probePath.lastIndexOf(expectedLabel),
    expectedLabel,
  };
}

export function collectDocumentCompletionTimings(rawTimings, documentPath) {
  const timings = rawTimings
    .trim()
    .split("\n")
    .filter(Boolean)
    .map((line) => JSON.parse(line))
    .filter((entry) => entry.uri === documentPath);

  const summary = {};
  if (timings.length >= 3) {
    summary.libraryStageTimings = timings[0];
    summary.coldStageTimings = timings[1];
    summary.warmStageTimings = timings[2];
  } else if (timings.length >= 2) {
    summary.coldStageTimings = timings[0];
    summary.warmStageTimings = timings[1];
  } else if (timings.length >= 1) {
    summary.coldStageTimings = timings[0];
  }
  if (timings.length >= 1) {
    summary.latestStageTimings = timings[timings.length - 1];
  }
  return summary;
}

export function collectHoverText(hovers) {
  if (!Array.isArray(hovers)) {
    return "";
  }
  return hovers
    .flatMap((hover) => hoverContentsParts(hover?.contents))
    .join("\n");
}

export function collectDefinitionUris(definitions) {
  if (!Array.isArray(definitions)) {
    return [];
  }
  return definitions
    .map((item) => normalizeUri(item?.targetUri ?? item?.uri ?? null))
    .filter(Boolean);
}

export function assertMslCompletionCacheProof(summary, documentPath) {
  const libraryLoad = summary.libraryStageTimings;
  const cold = summary.coldStageTimings;
  const warm = summary.warmStageTimings;
  if (!cold || !warm) {
    throw new Error(`expected cold and warm completion timings for ${documentPath}`);
  }
  if (
    cold.uri !== documentPath ||
    warm.uri !== documentPath ||
    (libraryLoad && libraryLoad.uri !== documentPath)
  ) {
    throw new Error("timing entries should match the probed document");
  }

  const load = libraryLoad ?? cold;
  const loadDelta = cacheDelta(load);
  const coldDelta = cacheDelta(cold);
  const warmDelta = cacheDelta(warm);
  if ((load.classNameCountAfterEnsure ?? 0) <= 0) {
    throw new Error("library load should populate cached class names");
  }
  if ((loadDelta.library_completion_cache_hits ?? 0) < 1) {
    throw new Error("library load should hit the library completion cache");
  }
  if ((loadDelta.library_completion_cache_misses ?? 0) !== 0) {
    throw new Error("library load should not miss the library completion cache");
  }
  if (load.builtResolvedTree) {
    throw new Error("MSL namespace completion should not build a resolved tree on library load");
  }
  if ((loadDelta.standard_resolved_builds ?? 0) !== 0) {
    throw new Error("library load should avoid standard resolved builds");
  }
  if ((loadDelta.semantic_navigation_builds ?? 0) !== 0) {
    throw new Error("library load should avoid semantic navigation builds");
  }

  if ((cold.classNameCountAfterEnsure ?? 0) <= 0) {
    throw new Error("cold completion should keep cached class names available");
  }
  if ((coldDelta.library_completion_cache_hits ?? 0) < 1) {
    throw new Error("cold completion should hit the library completion cache");
  }
  if ((coldDelta.library_completion_cache_misses ?? 0) !== 0) {
    throw new Error("cold completion should not miss the library completion cache");
  }
  if ((coldDelta.library_files_parsed ?? 0) !== 0) {
    throw new Error("cold completion should not reparse library files");
  }
  if (cold.builtResolvedTree) {
    throw new Error("cold MSL namespace completion should not build a resolved tree");
  }
  if ((coldDelta.standard_resolved_builds ?? 0) !== 0) {
    throw new Error("cold MSL namespace completion should avoid standard resolved builds");
  }
  if ((coldDelta.semantic_navigation_builds ?? 0) !== 0) {
    throw new Error("cold MSL namespace completion should avoid semantic navigation builds");
  }

  if ((warm.classNameCountAfterEnsure ?? 0) <= 0) {
    throw new Error("warm completion should keep cached class names available");
  }
  if ((warmDelta.library_completion_cache_hits ?? 0) < 1) {
    throw new Error("warm completion should hit the library completion cache");
  }
  if ((warmDelta.library_completion_cache_misses ?? 0) !== 0) {
    throw new Error("warm completion should not miss the library completion cache");
  }
  if ((warmDelta.library_files_parsed ?? 0) !== 0) {
    throw new Error("warm completion should not reparse library files");
  }
  if (warm.builtResolvedTree) {
    throw new Error("warm MSL namespace completion should not build a resolved tree");
  }
  if ((warmDelta.standard_resolved_builds ?? 0) !== 0) {
    throw new Error("warm MSL namespace completion should avoid standard resolved builds");
  }
  if ((warmDelta.semantic_navigation_builds ?? 0) !== 0) {
    throw new Error("warm MSL namespace completion should avoid semantic navigation builds");
  }
}

function hoverContentsParts(contents) {
  if (typeof contents === "string") {
    return [contents];
  }
  if (Array.isArray(contents)) {
    return contents.flatMap((part) => hoverContentsParts(part));
  }
  if (contents && typeof contents === "object") {
    if (typeof contents.value === "string") {
      return [contents.value];
    }
    if (typeof contents.language === "string" && typeof contents.value === "string") {
      return [contents.value];
    }
  }
  return [];
}

function normalizeUri(value) {
  if (typeof value === "string") {
    return value;
  }
  if (value && typeof value === "object" && typeof value.toString === "function") {
    const rendered = value.toString();
    if (rendered && rendered !== "[object Object]") {
      return rendered;
    }
  }
  return null;
}
