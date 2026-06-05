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
**literal zero** and is enforced by an architecture test that scans **all
first-party source — Rust and editor/JS alike** (`*.rs`, `*.ts`, `*.mjs`,
`*.cjs`, `*.js`); any `RUMOCA_*` env-var use (a quoted `"RUMOCA_…"` literal or a
`…env.RUMOCA_…` access) fails the build.

| Need | Required channel | Why |
|---|---|---|
| Anything a human or agent might tune | A `clap` **CLI flag** (`--flag`) | Self-documenting via `--help`; no README lookup |
| A value that never needs to vary | A **baked-in constant** | One obvious place to edit; no runtime surface |
| Debug / diagnostic output | A **`--trace` target** (`tracing` feature) | Unified, filterable; never a bespoke env knob |
| Config for a child process that cannot take argv (libtest harness, node behind nested npm, VS Code extension host) | A **fixed-path config / marker file** written by the parent | Deterministic, inspectable; argv-equivalent without env |
| User-authored model/sim/viz config | A `rum.toml` **scenario file** (below) | Colocated, version-controlled, editor-discoverable |

**REQUIRED:**
- Do NOT add a new `RUMOCA_*` (or other ad-hoc) environment variable to read
  configuration or behavior — in Rust **or** in editor/JS code. Pick a channel
  from the table above.
- A child process that genuinely cannot accept argv gets a fixed-path file, not
  an environment variable. Examples: `cargo xtask verify msl-parity` serializes
  its flags to `target/msl/parity-config.json` for the `rumoca-test-msl` libtest
  harness; the VS Code smoke runners write their parameters into the launched
  `.code-workspace` settings (`rumoca.benchmark.*`), which the extension-host
  test suites read via `getConfiguration` and the extension forwards to
  `rumoca-lsp` as CLI flags.
- Standard, non-Rumoca environment variables a tool merely passes through
  (`MODELICAPATH`, `GITHUB_ACTIONS`, `ELECTRON_DISABLE_SANDBOX`, …) are not
  configuration knobs and are out of scope for this rule.

**Enforcement:** `crates/rumoca/tests/architecture_hardening/env_var_registry.rs`
(`test_rumoca_env_vars_are_registered`) scans every source file under the
workspace (excluding build output, dependencies, and vendored trees). Adding a
`RUMOCA_*` name to its `REGISTERED_ENV_VARS` allowlist is a deliberate,
reviewable policy exception with written rationale — not the default escape
hatch.

### Model / Simulation / Visualization Configuration

User-authored Rumoca model run configuration is colocated TOML content in a
Rumoca scenario file, not hidden workspace project state. Scenario files follow
a filename convention — `rum.toml` for the default scenario and
`rum.<profile>.toml` for named profiles — which acts as the editor/discovery
hook. A required `[rumoca]` marker section (with a `version` field and the
`task`) is the authoritative declaration; editors enable Rumoca-specific
behavior only when that marker is present.

- A runnable scenario uses one `rum.toml`/`rum.<profile>.toml` file and declares
  exactly one `task` under `[rumoca]`. Accepted task values are `simulate` and
  `codegen`.
- The scenario file sits beside the example/model/scenario it configures. The
  `<profile>` segment should describe the task or scenario, such as `rum.toml`,
  `rum.ball.toml`, or `rum.acro.toml`.
- Each scenario file has at most one `[model]` section, one logical model
  target, and one runnable task. Do not add multiple named runnable configs or
  multiple runnable tasks inside a single scenario file.
- Shared settings should be factored through an explicit future include/extends
  mechanism or a scenario-specific shared file, not through `.rumoca` UUID
  sidecars.
- Human-authored simulation, compiler/source-root, visualization, and input
  routing settings MUST NOT be stored under `.rumoca/models/by-id` or
  `.rumoca/project.toml`.
- `.rumoca` is reserved for generated cache/result/editor state only.

Example:

```toml
source_roots = ["../Modelica"]

[rumoca]
version = "1"
task = "simulate"

[model]
file = "../models/Ball.mo"
name = "Ball"

[sim]
solver = "bdf"
t_end = 10.0
dt = 0.01

[[plot.views]]
id = "states_time"
title = "States vs Time"
type = "timeseries"
x = "time"
y = ["x", "v"]

[[plot.views]]
id = "viewer_3d"
title = "3D View"
type = "3d"
script_path = "../shared/ball_scene.js"

[viewer]
mode = "results_panel"
```

Template/codegen scenarios use the same single-task shape:

```toml
source_roots = ["../Modelica"]

[rumoca]
version = "1"
task = "codegen"

[model]
file = "../models/Ball.mo"
name = "Ball"

[codegen]
target = "sympy"
output_dir = "ball_sympy_out"
```

Editor run controls MUST operate on `rum.toml` scenario files, not on Modelica
source files. A play/run action executes the scenario's declared task. A
settings action edits the same scenario. Separate editor toolbar actions for
simulation versus template generation are intentionally avoided; the task owns
that
choice.

Simulation scenarios choose their presentation separately from solver pacing:

- `[sim].mode` controls pacing only: `as_fast_as_possible`, `realtime`, or
  `lockstep`.
- `[viewer].mode` controls the launch surface. `results_panel` runs the normal
  batch simulation and renders configured `[[plot.views]]` in the editor
  results panel. `external_web` starts the interactive runner and opens the
  HTTP/WebSocket viewer for scenarios with keyboard/gamepad/input routing.
- `[viewer].prefer_external` is an editor presentation hint. When false or
  omitted, editors should prefer embedded VS Code panels for both results and
  interactive HTTP viewers. When true, editors should open the relevant
  viewer/report in the system browser. Command-line tools ignore this hint.
- Interactive viewer presentation settings live in the scenario `rum.toml` file.
  `[viewer].status_title` sets the status panel title, `[viewer].show_armed`
  controls whether the standard armed row is visible, and
  `[[viewer.controls.keyboard]]` / `[[viewer.controls.gamepad]]` entries
  describe control help rows with `keys` and `action` strings. These fields are
  presentation metadata only; actual signal routing stays under `[input]`,
  `[locals]`, `[derived]`, and `[signals]`.
- `[viewer.onboard_camera]` configures the live viewer's onboard camera pose.
  Supported modes are `planar_xy_heading` for ground vehicles exposing planar
  `x`/`y`/heading signals and `quaternion_flu` for 6-DOF vehicles exposing
  position plus scalar-first quaternion signals. The block names viewer JSON
  signals, so each referenced signal MUST also be routed under
  `[signals.viewer]`. Optional `mount`, `forward`, and `up` arrays are
  vehicle-local 3-vectors. Viewer world space is the Three.js basis: world
  `Y` is up, the ground plane is world `X/Z`, and planar `x`/`y` map to world
  `X`/`Z` respectively. For `planar_xy_heading`, zero heading uses vehicle
  local `+X` as forward and `+Y` as up; positive heading is a rotation about
  world `+Y`, with the viewer applying `-heading` to align the configured
  vehicle-local `mount`, `forward`, and `up` vectors into world space.
- If `[viewer].mode` is omitted, editors infer `external_web` only when the
  scenario contains an HTTP transport, explicit input routing, signals, locals,
  derived signals, reset behavior, or external-interface coupling. Otherwise
  the default is `results_panel`.

Codegen/template runs MUST materialize the target renderer's returned files
under `[codegen].output_dir` when present, or under a deterministic colocated
default output directory. Editors MUST NOT make the primary codegen workflow a
preview text box with a separate save button; generated files should appear in
the normal workspace/project file surface.

Editor actions on Modelica source files MAY provide scaffolding only. A
source-file wizard may help the user choose a model, choose a single task, and
write a colocated `rum.toml` scenario, but it MUST NOT run simulation or codegen
directly from the Modelica source file.

`script_path` is an explicit reusable path. It is not derived from a hidden
model identity, and multiple `rum.toml` scenarios may point at the same 3D script.

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

### API

```rust
//! rumoca-tool-fmt/src/options.rs

/// Configuration file names to search for.
pub const CONFIG_FILE_NAMES: &[&str] = &[".rumoca_fmt.toml", "rumoca_fmt.toml"];

/// Error that can occur when loading configuration.
#[derive(Debug, Error)]
pub enum ConfigError {
    #[error("failed to read config file: {0}")]
    ReadError(#[from] std::io::Error),
    #[error("failed to parse config file: {0}")]
    ParseError(#[from] toml::de::Error),
}

/// Find a configuration file by searching directory and parents.
pub fn find_config(start_dir: &Path) -> Option<PathBuf>;

/// Load configuration from a specific file path.
pub fn load_config(path: &Path) -> Result<FormatOptions, ConfigError>;

/// Load configuration from a directory, searching parent directories.
pub fn load_config_from_dir(dir: &Path) -> Result<Option<FormatOptions>, ConfigError>;
```

### CLI Override Pattern

CLI arguments override file configuration using partial option types:

```rust
/// Full options (all fields required)
pub struct FormatOptions {
    pub profile: FormatProfile,
    pub indent_size: usize,
    pub use_tabs: bool,
    pub normalize_indentation: bool,
    pub repair_missing_indentation: bool,
    pub normalize_equation_spacing: bool,
    pub normalize_operator_spacing: bool,
    pub normalize_argument_assignment_spacing: bool,
    pub insert_final_newline: bool,
    pub trim_trailing_whitespace: bool,
    pub line_ending: LineEnding,
}

/// Partial options for CLI overrides (all fields optional)
pub struct PartialFormatOptions {
    pub profile: Option<FormatProfile>,
    pub indent_size: Option<usize>,
    pub use_tabs: Option<bool>,
    pub normalize_indentation: Option<bool>,
    pub repair_missing_indentation: Option<bool>,
    pub normalize_equation_spacing: Option<bool>,
    pub normalize_operator_spacing: Option<bool>,
    pub normalize_argument_assignment_spacing: Option<bool>,
    pub insert_final_newline: Option<bool>,
    pub trim_trailing_whitespace: Option<bool>,
    pub line_ending: Option<LineEnding>,
}

impl FormatOptions {
    /// Merge with partial options. CLI values override file values.
    pub fn merge(self, cli: PartialFormatOptions) -> Self {
        FormatOptions {
            profile: cli.profile.unwrap_or(self.profile),
        }
    }
}
```

### Usage Pattern

```rust
// Load config from file (if exists)
let file_config = load_config_from_dir(input.parent().unwrap())?
    .unwrap_or_default();

// Merge with CLI overrides
let options = file_config.merge(cli_options);

// Use merged options
let formatted = format(&source, &options)?;
```

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
