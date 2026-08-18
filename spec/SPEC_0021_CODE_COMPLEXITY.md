# SPEC_0021: Maintainability and Determinism Guidelines

## Status
ACCEPTED

## Summary
Establishes maintainability guardrails: function complexity limits, module
shape, deterministic public collections, and code-size discipline.

## Motivation

Complex functions impact:
1. **Human cognitive load** - Deep nesting and long functions are hard to follow
2. **AI context windows** - Complex code is harder to analyze correctly
3. **Code review quality** - Reviewers miss issues in complex code
4. **Bug density** - Studies show bugs correlate with nesting depth and function length

## Why Not `cognitive_complexity`?

Clippy's `cognitive_complexity` lint is in the `restriction` category with known problems:
> "The true Cognitive Complexity of a method is not something we can calculate using modern technology."
> — [rust-clippy#3793](https://github.com/rust-lang/rust-clippy/issues/3793)

Clippy recommends using `excessive_nesting` and `too_many_lines` instead.

## Metrics and Limits

### Excessive Nesting (Clippy Default: 4)

| Level | Depth | Action |
|-------|-------|--------|
| Target | 1-2 | Ideal |
| Acceptable | 3 | Fine |
| Limit | 4 | Clippy warns above this |
| Exception | >4 | Must document with `#[allow]` |

**Research:** 3-4 levels is the cognitive limit for tracking nested context. Beyond this, developers lose track of which branch they're in.

### Function Length (Clippy Default: 100)

| Level | Lines | Action |
|-------|-------|--------|
| Target | 10-30 | Ideal |
| Acceptable | 30-60 | Fine |
| Limit | 100 | Clippy warns above this |
| Exception | >100 | Must document with `#[allow]` |

**Research:** Bug density increases significantly above 60-100 lines (Code Complete, McConnell). Functions over 100 lines correlate with higher defect rates.

### Function Arguments (Clippy Default: 7)

| Level | Count | Action |
|-------|-------|--------|
| Target | 0-3 | Ideal |
| Acceptable | 4-5 | Fine |
| Limit | 7 | Clippy warns above this |
| Exception | >7 | Must document with `#[allow]` |

**Research:** Miller's Law - humans can hold 7±2 items in working memory. More than 7 parameters overwhelms cognition.

## Enforcement

### clippy.toml

```toml
# Using clippy defaults - they're research-backed
excessive-nesting-threshold = 4   # Cognitive limit for context tracking
too-many-lines-threshold = 100    # Bug density threshold
too-many-arguments-threshold = 7  # Miller's Law (7±2)
```

### Cargo.toml (workspace)

The root `Cargo.toml` sets these at `deny`, not `warn`. They are clippy lints,
so they are evaluated only when clippy runs: a threshold breach is an error
under `cargo clippy`, and `cargo check`/`cargo build` still succeed. The `deny`
level means the breach fails clippy on its own, without CI's `-D warnings`:

```toml
[workspace.lints.clippy]
all = { level = "deny", priority = -1 }
excessive_nesting = "deny"
too_many_lines = "deny"
too_many_arguments = "deny"
```

### CI Integration

There is no separate complexity job. The `lint` job in
`.github/workflows/ci.yml` runs several steps (checkout, dependency/toolchain
setup, cache, the lint gate, the pull-request architecture review scan, a
generated-parser drift check, and the crate-DAG artifact). Complexity is gated
by exactly one of them:

```yaml
- name: Run lint gate
  run: cargo make verify-lint
```

`cargo make verify-lint` runs the workspace rustfmt check and the
traversal-policy checks, then clippy over the whole workspace with all targets
and all features under `-D warnings`
(`infra/cargo-make/verify.toml`).

## Exceptions

High complexity is acceptable when:
1. **Exhaustive match** - Handling many enum variants in one place
2. **Entry points** - Top-level compiler phase functions
3. **Generated code** - Parser traits, grammar code

Document exceptions with:
```rust
// SPEC_0021: Exception - exhaustive match over TypedExprKind variants
#[allow(clippy::too_many_lines)]
fn check_expr(&mut self, expr: &ResolvedExpr) -> TypedExpr {
```

## Refactoring Strategies

### To Reduce Nesting
1. **Early returns** - Guard clauses at function start
2. **Extract functions** - Move nested blocks into named functions
3. **Match → if-let chains** - Flatten nested matches

### To Reduce Length
1. **Extract helper functions** - Move logical units into functions
2. **Split by responsibility** - One function, one job
3. **Use iterators** - Replace loops with iterator chains

### File Size (Guideline - Script Check)

| Level | Lines | Action |
|-------|-------|--------|
| Target | 200-500 | Ideal |
| Acceptable | 500-1000 | Fine |
| Warning | 1000-2000 | Consider splitting |
| Action Required | >2000 | Split by concern |

**Note:** Clippy has no file-level lint, so the >2000 row is enforced by the
workspace test `crates/rumoca/tests/suite_gates/code_size_budget_test.rs`, which fails
`cargo test --workspace` for any production Rust file over 2000 lines unless
that file's text contains all three of `SPEC_0021`, `file-size`, and
`split plan`. Use the script below for the earlier warning bands.

**Exceptions:** Generated code (any `generated/` path) and test sources are
skipped outright; every other file needs the three-phrase marker above, written
as a comment that states the split plan.

### Module Decomposition (Content-Based Paths Only)

Rust module source-path attributes MUST NOT be used anywhere in the workspace,
including production code, tests, build support, and generated test harnesses.
`include!(...)` MUST NOT be used as a workaround to bypass max-file-length or
complexity checks in production modules.

Required approach:
- Split large code into real Rust modules (`mod ...;`) with explicit boundaries.
- Declare external submodules at the top of their owning source file, before
  imports and implementation items.
- Keep each module responsible for one concern so clippy/file-size checks remain meaningful.

Allowed exception:
- Generated code include patterns are allowed when generation tooling requires it.

Test modules MUST use the ordinary content-based layout under their owning
`suite_*` directory and be declared with `mod ...;` at the top of the umbrella.
Shared test helpers MUST have one module owner; suites must be consolidated or
given an explicit shared crate boundary instead of loading one source through
multiple paths.

The architecture gate scans every Rust source under `crates/` and rejects any
module source-path attribute. That prohibition has no allowances or debt
ceilings; the generated-code `include!` exception above is separate.

Maintenance rule:
- Existing source-path bypasses are architecture violations and MUST be removed,
  not grandfathered as cleanup debt.

### Files Per Directory (Guideline)

| Level | Files | Action |
|-------|-------|--------|
| Target | 5-15 | Ideal - easy to scan and hold in memory |
| Acceptable | 16-25 | Fine if names are consistent and module boundary is clear |
| Warning | 26-40 | Refactor recommended - discoverability falls off, grep gets noisy |
| Avoid | >40 | Directory becomes a "junk drawer" (unless generated/test data) |

**Why this matters for AI assistance:**

AI tools work best when:
1. **Small candidate set** - The set of possible relevant files is small
2. **Coherent topic** - The directory forms a clear module boundary
3. **Predictable names** - File names follow consistent conventions

Once a directory grows large, it stops being a module and becomes a search problem. This impacts both human navigation and AI-assisted development.

**Script Check:**
```bash
# Count files per directory
find crates -type d -exec sh -c 'echo -n "$1: "; find "$1" -maxdepth 1 -name "*.rs" | wc -l' _ {} \; | awk -F: '$2 > 15' | sort -t: -k2 -rn
```

## Deterministic Public Collections

Public IR/DAE fields that affect output, hashing, serialization, or codegen
MUST use deterministic collection types such as `IndexMap`.

Required rules:

- Public fields on IR and DAE types use `IndexMap`/ordered collections when
  iteration order can affect output.
- `HashMap`/`HashSet` may be used for phase-internal lookup only when the map is
  not iterated to produce output, or when results are collected into an ordered
  structure before leaving the phase.
- Serialized output must be deterministic for the same input and compiler
  version.
- Do not expose `HashMap` or `HashSet` as public fields on IR or DAE types.

Permitted exception:

```rust
// Insertion order matches source order (already deterministic).
let mut map = IndexMap::new();
for item in already_ordered_items {
    map.insert(item.name.clone(), item);
}
```

## Metrics Tooling

```bash
# Check all complexity warnings (clippy)
cargo clippy 2>&1 | grep -E "(excessive_nesting|too_many_lines|too_many_arguments)"

# Count clippy violations
cargo clippy 2>&1 | grep -c "warning:"

# Check file sizes (>1000 lines, excluding generated)
find crates -name "*.rs" ! -path "*/generated/*" -exec wc -l {} \; | awk '$1 > 1000' | sort -rn
```

## References
- [Clippy excessive_nesting](https://rust-lang.github.io/rust-clippy/master/index.html#excessive_nesting)
- [Clippy too_many_lines](https://rust-lang.github.io/rust-clippy/master/index.html#too_many_lines)
- [Clippy too_many_arguments](https://rust-lang.github.io/rust-clippy/master/index.html#too_many_arguments)
- [Why cognitive_complexity is problematic](https://github.com/rust-lang/rust-clippy/issues/3793)
- [indexmap crate](https://docs.rs/indexmap)
