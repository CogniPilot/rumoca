# SPEC_0018: Tool Configuration Loading

## Status
ACCEPTED

## Summary
Tool crates (formatter, linter) support configuration via TOML files with hierarchical lookup and CLI override merging.

## Motivation
Tools need configurable behavior:
- Different projects have different style preferences
- CI/CD may need different settings than local development
- Config files allow reproducible formatting/linting

## Specification

### Configuration Channels (No `RUMOCA_*` Environment Variables)

Configuration and behavior knobs MUST reach the code through a discoverable,
self-documenting channel. The `RUMOCA_*` environment-variable surface is
**literal zero** across first-party Rust and editor/JS source (`*.rs`, `*.ts`,
`*.mjs`, `*.cjs`, `*.js`); quoted `"RUMOCA_…"` literals or `…env.RUMOCA_…`
accesses fail the build.

| Need | Required channel | Why |
|---|---|---|
| Anything a human or agent might tune | A `clap` **CLI flag** (`--flag`) | Self-documenting via `--help`; no README lookup |
| A value that never needs to vary | A **baked-in constant** | One obvious place to edit; no runtime surface |
| Debug / diagnostic output | A **`--trace` target** (`tracing` feature) | Unified, filterable; never a bespoke env knob |
| Config for a child process that cannot take argv (libtest harness, node behind nested npm, VS Code extension host) | A **fixed-path config / marker file** written by the parent | Deterministic, inspectable; argv-equivalent without env |
| Shared editor/workspace context | A visible `rumoca-workspace.toml` **workspace file** (below) | Same source-root/cache context in VS Code, playground, and docs |
| User-authored model/sim/viz config | A `rumoca-scenario.toml` **scenario file** (below) | Colocated, version-controlled, editor-discoverable |

**REQUIRED:**
- Do NOT add a new `RUMOCA_*` (or other ad-hoc) environment variable to read
  configuration or behavior — in Rust **or** in editor/JS code. Pick a channel
  from the table above.
- A child process that genuinely cannot accept argv gets a fixed-path file, not
  an environment variable. Examples: `cargo xtask verify msl-parity` writes
  `target/msl/parity-config.json` for libtest; VS Code smoke jobs write
  `.code-workspace` settings that the extension forwards to `rumoca-lsp` flags.
- Standard, non-Rumoca environment variables a tool merely passes through
  (`MODELICAPATH`, `GITHUB_ACTIONS`, `ELECTRON_DISABLE_SANDBOX`, …) are not
  configuration knobs and are out of scope for this rule.

### Workspace Configuration

Editor/workspace context lives in visible `rumoca-workspace.toml` files.
Rumoca tools MUST NOT create hidden project configuration directories for
generated cache, result, or editor-session state.

A workspace file owns context that affects editing and discovery, not a specific
run:

- source roots and scoped source roots
- package/library mount points
- parsed source-root cache policy
- default scenario or editor layout hints that are portable across hosts
- docs/playground preload bundles

A workspace file MUST NOT contain model-specific solver, plot, codegen, viewer,
or input-routing settings. Those settings stay in `rumoca-scenario.toml` scenario files.

Workspace files cascade from parent to child. For a focused file, the effective
workspace config is built by walking from the open workspace root to the focused
file's directory and merging every `rumoca-workspace.toml` found on that path.

Source-root merge rules are deterministic:

1. Parent workspace `source_roots` are applied first.
2. Child workspace `source_roots` append after parent roots.
3. Matching scoped roots append after global roots. A scope key matches when the
   focused file path is inside that scope directory, relative to the workspace
   config file that declared it.
4. Scenario `source_roots` append after the effective workspace roots.
5. Host-local overrides append last and are for machine-specific paths only.
6. Duplicate resolved paths are removed while preserving first occurrence.

Each `source_roots` entry is resolved relative to the `rumoca-workspace.toml`
that declares it, unless it is already absolute. A scoped entry is written under
`[source_root_scopes."<relative/path>"]`:

```toml
source_roots = ["vendor/Modelica 4.1.0"]

[source_root_scopes."examples/control"]
source_roots = ["vendor/ControlLibraries"]
```

`replace_source_roots = true` may be used at the top level or inside a scope to
clear inherited roots at that merge point. It is intentionally explicit; absent
means append.

VS Code, the playground, and mdBook live examples MUST use the same effective
workspace config semantics. VS Code applies the nearest merged workspace config
to completion, hover, diagnostics, go-to-definition, and model discovery. The
playground and docs apply the same semantics through the shared WASM/runtime
workspace API.

**Enforcement:** `crates/rumoca/tests/architecture_hardening/env_var_registry.rs`
(`test_rumoca_env_vars_are_registered`) scans workspace source files, excluding
build output, dependencies, and vendored trees. Adding a `RUMOCA_*` name to its
allowlist is a deliberate, reviewable policy exception with written rationale.

### Model / Simulation / Visualization Configuration

User-authored Rumoca model run configuration is colocated TOML content in a
Rumoca scenario file, not hidden workspace project state. Scenario files follow
a filename convention — `rumoca-scenario.toml` for the default scenario and
`rumoca-scenario.<profile>.toml` for named profiles — which acts as the editor/discovery
hook. A required `[rumoca]` marker section (with a `version` field and the
`task`) is the authoritative declaration; editors enable Rumoca-specific
behavior only when that marker is present.

- A runnable scenario uses one `rumoca-scenario.toml`/`rumoca-scenario.<profile>.toml` file and declares
  exactly one `task` under `[rumoca]`. Accepted task values are `simulate` and
  `codegen`.
- The scenario file sits beside the example/model/scenario it configures. The
  `<profile>` segment should describe the task or scenario, such as `rumoca-scenario.toml`,
  `rumoca-scenario.ball.toml`, or `rumoca-scenario.acro.toml`.
- Each scenario file has at most one `[model]` section, one logical model
  target, and one runnable task. Do not add multiple named runnable configs or
  multiple runnable tasks inside a single scenario file.
- Shared settings should be factored through an explicit future include/extends
  mechanism or a scenario-specific shared file, not through hidden generated
  state.
- Human-authored simulation, compiler/source-root, visualization, and input
  routing settings MUST be stored in visible workspace or scenario files.
- Generated caches use visible tool-owned paths such as `rumoca-cache/`.
  Persisted simulation results are the `rumoca-result.<timestamp>.<model>.json`
  files themselves, written next to the scenario TOML by default or under the
  scenario's configured output directory.

Simulation and codegen scenarios use the same single-task shape. The `[rumoca]`
`task` selects the workflow; task-specific sections such as `[sim]`, `[plot]`,
`[viewer]`, and `[codegen]` configure that one workflow.

Editor run controls MUST operate on `rumoca-scenario.toml` scenario files, not on Modelica
source files. A play/run action executes the scenario's declared task. A
settings action edits the same scenario. Separate editor toolbar actions for
simulation versus template generation are intentionally avoided; the task owns
that choice.

Opening a scenario file in an editor defaults to the shared Rumoca scenario GUI.
The GUI is an authoring view for that same TOML file, not separate state. A raw
TOML toggle MUST edit the identical file content; GUI edits render TOML through
the shared scenario API, and raw TOML edits reparse into the GUI. Parse errors
are shown without discarding the raw text.

Simulation scenarios choose input routing, clock policy, and presentation as
separate concerns:

- `[sim].mode` controls pacing only: `as_fast_as_possible`, `realtime`, or
  `lockstep`.
- `[sim].solver`, `[sim].dt`, `[sim].atol`, and `[sim].rtol` configure the
  simulation integration session used by both batch and interactive runs.
- `[sim].t_end` is the finite output horizon for batch/results-panel runs.
  Scheduled and browser-interactive runs MUST NOT treat it as a terminal
  condition; they extend their finite solver horizon on demand and continue
  until the operator explicitly quits or the run fails.
- `[input]`, `[locals]`, `[derived]`, and `[signals.model_inputs]` enable
  live keyboard, gamepad, browser, or external input routing for the same
  regular `simulate` task. Input-enabled simulation is not a separate task or
  viewer mode.
- `[viewer].mode` controls the launch surface. `results_panel` runs the normal
  batch simulation and renders configured `[[plot.views]]` in the editor
  results panel. `external_web` starts the scheduled simulation and opens the
  HTTP/WebSocket viewer for scenarios with keyboard/gamepad/input routing.
- `[viewer].prefer_external` is an editor presentation hint: false/omitted
  prefers embedded VS Code panels; true opens the system browser.
  Command-line tools ignore it.
- Interactive viewer presentation settings live in the scenario `rumoca-scenario.toml`:
  `[viewer].status_title` (status panel title), `[viewer].show_armed` (armed
  row visibility), and `[[viewer.controls.keyboard]]`/`[[...gamepad]]` help
  rows (`keys` + `action`). Presentation metadata only; signal routing stays
  under `[input]`, `[locals]`, `[derived]`, and `[signals]`.
- The viewer is model-agnostic — nothing vehicle-specific is built in:

  | Table | Rule | Why |
  |---|---|---|
  | `[[viewer.frame]]` | Named frame in model FLU coordinates (x forward, y left, z up); `position` entries are `[signals.viewer]` names or numeric constants (2 entries = planar); orientation is `quaternion = [q0..q3]` (scalar-first) XOR `heading = "yaw_signal"`, else fixed | One signal-driven pose source per moving part; the viewer owns the single FLU→renderer conversion (renderer `X = -y`, `Y = z`, `Z = x`), so cameras, HUD, and scene placement cannot drift apart |
  | `[[viewer.camera]]` | `name` + `frame` + optional frame-local FLU `mount`/`look`/`up`; the C key cycles scene camera → configured cameras | Cameras hook onto any model part (rover hood, arm end-effector, wing tip) without viewer code changes |
  | `[viewer.hud]` | Opt-in; `mode = "flight"` with `frame` for attitude and optional `altitude`/`speed`/`sticks` signal names | Reusable feature, off by default — a robot arm scenario simply omits it |

  Signal references MUST be routed under `[signals.viewer]` and frame
  references MUST resolve (`sim check` rejects violations). Scenes receive
  the resolved matrices (`api.frames`) and SHOULD place meshes from them
  (meshes authored nose `+Z`, up `+Y`, matching frame identity).
- If `[viewer].mode` is omitted, editors infer `external_web` only when the
  scenario explicitly configures an HTTP transport or external coupling; input
  routing alone remains a regular simulation surface and defaults to
  `results_panel`.

Codegen/template runs MUST materialize the target renderer's returned files
under `[codegen].output_dir` when present, or under a deterministic colocated
default output directory. Editors MUST NOT make the primary codegen workflow a
preview text box with a separate save button; generated files should appear in
the normal workspace/project file surface.

Editor actions on Modelica source files MAY provide scaffolding only. A
source-file wizard may help the user choose a model, choose a single task, and
write a colocated `rumoca-scenario.toml` scenario, but it MUST NOT run simulation or codegen
directly from the Modelica source file.

`script_path` is an explicit reusable path. It is not derived from a hidden
model identity, and multiple `rumoca-scenario.toml` scenarios may point at the same 3D script.

### Configuration File Names

Each tool searches for config files in order:

| Tool | File Names |
|------|------------|
| Formatter | `.rumoca_fmt.toml`, `rumoca_fmt.toml` |
| Linter | `.rumoca_lint.toml`, `rumoca_lint.toml` |

Hidden files (dot-prefixed) take precedence.

### Hierarchical Lookup

Configuration search starts from the target file's directory and walks up to root:

```
/project/src/model.mo → searches:
  /project/src/.rumoca_fmt.toml
  /project/src/rumoca_fmt.toml
  /project/.rumoca_fmt.toml
  /project/rumoca_fmt.toml
  /.rumoca_fmt.toml  (stops at root)
```

First file found wins (no merging between files).

### CLI Override Pattern

CLI arguments override file configuration using partial option types: full
options have required fields, CLI override structs use `Option<T>`, and merge
logic keeps file/default values when the CLI omits a field.

### TOML Format

```toml
profile = "dymola"
indent_size = 2
use_tabs = false
normalize_indentation = false
repair_missing_indentation = true
normalize_equation_spacing = false
normalize_operator_spacing = false
normalize_argument_assignment_spacing = false
insert_final_newline = true
trim_trailing_whitespace = true
line_ending = "auto" # "auto", "lf", or "crlf"
```

`normalize_indentation = true` normalizes clear structural indentation for
class/package bodies, section headers, and control blocks. Continuation lines,
annotation alignment, comments, quoted identifiers, and multi-line strings keep
local MSL/Dymola alignment rather than a single global indentation rule.

`repair_missing_indentation = true` is the Dymola default. It only adds
structural indentation to otherwise unindented built-in scalar declaration
lines; existing nonzero indentation and local alignment are preserved.

`normalize_equation_spacing` covers declaration bindings and equation/algorithm
assignments. `normalize_operator_spacing` covers binary operators.
`normalize_argument_assignment_spacing` compacts call/modification assignments
such as `Real x(start=10)` and `h(a=1, b=2)`. Canonical enables it; Dymola
disables it to preserve MSL modifier whitespace.

`trim_trailing_whitespace = true` is the Dymola default. It trims only outside
strings, quoted identifiers, and block comments.

```toml
min_level = "warning"  # "help", "note", "warning", "error"
disabled_rules = ["magic-number", "naming-convention"]
warnings_as_errors = false
max_messages = 100
```

### Error Handling

- Missing config file: Use defaults (not an error)
- Malformed TOML: Return `ConfigError::ParseError`
- Unreadable file: Return `ConfigError::ReadError`
- Unknown fields: Ignored so newer config keys do not break older tools

## Rationale
- Hierarchical lookup matches rustfmt, eslint patterns
- Partial options enable clean CLI override semantics
- TOML is Rust ecosystem standard for config
- Separate error type enables specific error handling

## References
- rustfmt.toml: https://rust-lang.github.io/rustfmt/
- ESLint config: https://eslint.org/docs/user-guide/configuring/
