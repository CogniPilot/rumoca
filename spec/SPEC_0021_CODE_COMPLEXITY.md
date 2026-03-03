# SPEC_0021: Code Complexity Guidelines

## Status
ACCEPTED

## Summary
Establishes complexity limits for functions using clippy's recommended lints: `excessive_nesting`, `too_many_lines`, and `too_many_arguments`.

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

```toml
[workspace.lints.clippy]
excessive_nesting = "warn"
too_many_lines = "warn"
too_many_arguments = "warn"
```

### CI Integration

```yaml
- name: Check complexity
  run: cargo clippy -- -D clippy::excessive_nesting -D clippy::too_many_lines -D clippy::too_many_arguments
```

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

**Note:** Clippy has no file-level lint. Use the script below.

**Exceptions:** Generated code (parser files) and cohesive modules are exempt.

### Module Decomposition (No `include!` Complexity Bypass)

`include!(...)` MUST NOT be used as a workaround to bypass max-file-length or complexity checks.

Required approach:
- Split large code into real Rust modules (`mod ...;`) with explicit boundaries.
- Keep each module responsible for one concern so clippy/file-size checks remain meaningful.

Allowed exception:
- Generated code include patterns are allowed when generation tooling requires it.

Maintenance rule:
- Existing `include!(...)` usage in touched areas should be treated as cleanup debt and removed during maintainability work.

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
