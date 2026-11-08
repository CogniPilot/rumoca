# Prebuilt simulation sessions

A site can compile a fixed Modelica model at build time and load its DAE in the
browser. This skips Modelica parsing, resolution, instantiation, flattening and
DAE construction from source. Loading still checks/reconstructs the serialized
DAE and performs the normal Solve lowering, kernel generation and simulation
initialization. It does not promise to remove all of a site's Prepare time.

With `sim-rk45` or `sim-diffsol`, the binding exports:

```js
const key = prebuilt_artifact_key(source, modelName, workspaceSourcesJson);
const artifact = export_prebuilt_artifact(source, modelName, workspaceSourcesJson);
// Save key and artifact as strings. At runtime, use the same source set:
const session = WasmSimulationSession.fromPrebuiltArtifact(
  artifact, source, modelName, workspaceSourcesJson, optionsJson,
);
```

`workspaceSourcesJson` is an object mapping canonical relative paths to source
strings, including every companion and library file (MSL included). It excludes
the primary source, which is reserved as `input.mo`. Paths use `/`, have no empty,
`.` or `..` segments and cannot repeat. Keys are sorted before hashing, so JSON
object order does not matter. Contents, paths and the exact requested model name
are significant; line endings and whitespace are significant source bytes.
The exporter compiles this explicit set in a fresh compiler session: ambient
loaded libraries and singleton caches cannot contribute hidden dependencies.

The Rust-owned v1 envelope contains `key`, `payload_sha256` and `payload_json`.
The key has `version`, `compiler_version`, `compiler_identity`, `model_name`,
`source_sha256` and `sources_sha256`. `source_sha256` hashes UTF-8 primary source.
`sources_sha256` hashes compact Rust JSON of the sorted path-to-SHA256 map,
including `input.mo`. Use the Rust key helper; do not duplicate this algorithm in
JavaScript. Export and load require the same known, clean compiler Git identity
and package version. An unknown/dirty identity refuses. A schema, source, library,
path, model or compiler change requires a freshly exported artifact.

Keep `payload_json` opaque: parsing and reserializing it in JavaScript can round
64-bit IR identifiers. It carries the current checked DAE and original experiment
start/stop/tolerance/interval/solver annotations. Loading validates the envelope,
checks the payload digest and invokes the DAE owner's current-schema checked
wire replay. Hashes detect stale/corrupt build products; they do not authenticate
a hostile producer. Serve artifacts from the same trusted build as the WASM.

This artifact contains no option-specialized Solve plan. `optionsJson` uses the
same checked interactive options, parameter overrides, initial inputs and execution
policy as `withInteractiveConfiguration`. They are applied through the same
lowering/session path after load, so they are not part of this DAE cache key.
Dynamic input frames remain session commands. If sources change, the loader
refuses: the site may explicitly compile edited sources and export a new artifact;
the loader never silently recompiles or substitutes an older version.
