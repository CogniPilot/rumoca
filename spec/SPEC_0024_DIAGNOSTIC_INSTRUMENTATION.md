# SPEC_0024: Diagnostic Instrumentation

## Status
DRAFT

## Summary
Defines instrumentation infrastructure for debugging compiler issues, benefiting both human developers and AI assistants.

## Motivation

Compiler issues are difficult to diagnose without visibility into:
1. **What code paths are taken** - Which functions are called during compilation
2. **What expressions are evaluated** - Parameter evaluation success/failure
3. **Where processing stops** - Why certain patterns don't match
4. **Performance bottlenecks** - Which phases/models take time

## Specification

### 1. Tracing Framework

Use the `tracing` crate for structured, hierarchical instrumentation:

```rust
use tracing::{debug, info, instrument, span, Level};

#[instrument(skip(ctx), level = "debug")]
fn flatten_equation(ctx: &mut FlattenContext, eq: &InstEquation) -> Result<Vec<Equation>, FlattenError> {
    debug!(equation_kind = ?eq.kind(), "processing equation");
    // ...
}
```

### 2. Feature Flags

```toml
# Cargo.toml
[features]
default = []
tracing = ["dep:tracing", "dep:tracing-subscriber"]
```

Instrumentation is opt-in to avoid overhead in production builds.

### 3. Instrumentation Levels

| Level | Purpose | Example |
|-------|---------|---------|
| ERROR | Failures | "failed to evaluate expression" |
| WARN | Fallbacks | "using default for unevaluated parameter" |
| INFO | Progress | "flattening model X" |
| DEBUG | Details | "evaluating max(a, b) = 5" |
| TRACE | Verbose | "visiting AST node: Expression::BinOp" |

### 4. Required Instrumentation Pattern

All `#[instrument]` annotations MUST:
- Use `skip` for large context parameters (e.g., `skip(ctx)`)
- Set an explicit `level` (default is INFO, which is too noisy)
- Use `#[cfg(feature = "tracing")]` guards for inline `debug!`/`warn!` calls

**REQUIRED pattern:**
```rust
#[cfg(feature = "tracing")]
use tracing::{debug, warn};

fn try_eval_expr(...) -> Option<i64> {
    #[cfg(feature = "tracing")]
    debug!(variable = %name, "looking up variable");
    // ...
}
```

**PROHIBITED pattern:**
```rust
// BAD: unconditional tracing import without feature gate
use tracing::debug;

// BAD: no level on #[instrument] (defaults to INFO)
#[instrument]
fn flatten_equation(...) { ... }
```

### 5. Key Instrumentation Points

Phases that MUST have instrumentation when the `tracing` feature is enabled:

| Phase | What to instrument |
|-------|-------------------|
| Expression evaluation | Variable lookups, function calls, evaluation failures |
| For-equation processing | Range evaluation success/failure |
| Connection processing | Connection count, generated equations |
| Phase entry/exit | Model name, timing |

### 6. CLI Integration

```
rumoca compile --debug=flatten model.mo
rumoca compile --debug=all --trace-file=trace.json model.mo
```

| Flag | Purpose |
|------|---------|
| `--debug=PHASE` | Enable debug output for specific phase |
| `--debug=all` | Enable all debug output |
| `--trace-file=FILE` | Write JSON trace to file |
| `--trace-filter=EXPR` | Filter trace events (e.g., "flatten::equation") |

### 7. IR Dumps

Each IR type implements `serde::Serialize` for JSON output:

```
rumoca dump-ast model.mo > ast.json
rumoca dump-flat model.mo > flat.json
rumoca dump-dae model.mo > dae.json
```

## References

- [tracing crate](https://docs.rs/tracing/latest/tracing/)
- [tracing-subscriber](https://docs.rs/tracing-subscriber/latest/tracing_subscriber/)
- SPEC_0021: Code Complexity Guidelines
- SPEC_0023: Crate Architecture
