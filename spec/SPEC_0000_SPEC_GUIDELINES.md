# SPEC_0000: Specification Writing Guidelines

## Status
ACCEPTED

## Summary
How to write a spec that is immediately useful to both human developers and AI agents. Readability at a glance is paramount — a contributor should understand what a spec requires within 30 seconds of opening it.

## Why This Matters

Specs serve two audiences simultaneously:
- **Human developers** who scan for rules while implementing or reviewing code
- **AI agents** who load specs into limited context windows and need unambiguous instructions

A spec that requires careful reading to extract the rules is a spec that will be ignored or misapplied. Every spec should be optimized for fast, correct comprehension.

## Required Sections

Every spec MUST have these sections in this order:

| Section | Purpose | Length guidance |
|---------|---------|----------------|
| `# SPEC_NNNN: Title` | Descriptive title in 3-8 words | — |
| `## Status` | ACCEPTED, DRAFT, or REFERENCE | 1 line |
| `## Summary` | One sentence: what this spec requires | 1-2 sentences max |
| `## Specification` | The actual rules | As needed |

Every spec SHOULD have these sections when applicable:

| Section | Purpose |
|---------|---------|
| `## Motivation` | Why this spec exists (keep brief — 3-5 bullets max) |
| `## Rationale` | Why alternatives were rejected (only if non-obvious) |
| `## References` | Links to MLS sections, related specs, external docs |

## Specification

### 1. Write Rules, Not Essays

**REQUIRED framing — use allow/deny lists:**

```markdown
**REQUIRED:**
- All IR fields use IndexMap for deterministic iteration
- Generated code includes source-equation comments

**PROHIBITED:**
- HashMap as a public field on any IR type
- Span::DUMMY without a justifying comment
```

**PROHIBITED framing — narrative prose for rules:**

```markdown
It is generally recommended that developers should consider using IndexMap
when they need deterministic iteration, although HashMap may sometimes be
appropriate in certain circumstances.
```

The first version takes 2 seconds to understand. The second takes 30 seconds and leaves ambiguity.

### 2. Show the Actual Code

Specs MUST show the actual types and signatures from the codebase, not idealized pseudocode. Include the source file location.

**REQUIRED:**
```markdown
Located in `rumoca-ir-dae/src/lib.rs`:
\```rust
pub struct Dae {
    pub states: IndexMap<VarName, Variable>,
    // ...
}
\```
```

**PROHIBITED:**
```markdown
\```rust
// Conceptual design (not actual code)
pub struct Dae {
    pub vars: Vars,
    pub eqs: Eqs,
}
\```
```

If the code doesn't match the spec, either update the spec or file an issue. Aspirational designs belong in DRAFT specs, clearly labeled.

### 3. Keep Specs Short

| Length | Assessment |
|--------|------------|
| < 80 lines | Ideal — fits in a single context load |
| 80-150 lines | Good — one focused topic |
| 150-250 lines | Acceptable — may need a clear section index |
| 250+ lines | Too long — split, trim, or add a section index (see SPEC_0022) |

**What to cut:**
- Implementation code that duplicates what's in the source (link to the file instead)
- Rationale for decisions that are obvious or uncontested
- Multiple examples showing the same pattern
- Aspirational features (move to DRAFT specs or issues)

### 4. Stay In Sync With the Codebase

A spec that doesn't match the code is worse than no spec — it actively misleads contributors.

**REQUIRED:**
- Specs MUST reflect the current codebase, not a planned future state
- If code structures change, the spec MUST be updated in the same PR
- If a spec is no longer implemented, move it to `archive/deferred/`

**PROHIBITED:**
- ACCEPTED specs that describe unimplemented features
- Code examples using types or functions that don't exist
- Stale references to archived or renamed specs

### 5. Spec Lifecycle and Governance

Every spec follows this lifecycle:

```
PROPOSED → ACCEPTED (majority vote)
         → REJECTED (majority vote) → archive/rejected/
         → DRAFT (needs more work before vote)

ACCEPTED → archive/deferred/ (if feature is removed or deferred)
         → archive/legacy/   (if superseded by another spec)

REFERENCE (no vote needed — lookup tables, catalogs)
```

#### Statuses

| Status | Meaning |
|--------|---------|
| PROPOSED | New spec awaiting maintainer vote |
| ACCEPTED | Implemented and enforced — code MUST follow this spec |
| REJECTED | Voted down by maintainers — moved to `archive/rejected/` with rationale |
| DRAFT | Work in progress — not yet ready for a vote, may describe unimplemented features |
| REFERENCE | Lookup table — not a set of rules (e.g., MLS contract catalog), no vote needed |

#### Voting Process

1. **Author opens a PR** with the new spec file (status: PROPOSED) and a README table entry
2. **Maintainers review** the spec in the PR — discussion happens in PR comments
3. **Vote**: each maintainer approves or requests changes
4. **Majority of maintainers approves** → status changes to ACCEPTED, PR merges
5. **Majority rejects** → status changes to REJECTED, spec moves to `archive/rejected/`, PR merges with the rejection rationale recorded in the spec

A spec may be sent back to DRAFT if maintainers agree it needs more work before a vote.

#### Amending Accepted Specs

Changes to ACCEPTED specs follow the same process — open a PR, maintainers vote. Trivial fixes (typos, line count updates, syncing code examples to match refactored code) do not require a vote.

### 6. Title and Summary Must Stand Alone

A contributor reading only the README table should know whether a spec is relevant to their task. The title and summary must be self-explanatory without reading the full spec.

**Good titles:** "Lean DAE (No Derived Data)", "Case-Sensitive Identifiers", "Phase-Local Error Types"

**Bad titles:** "Expression Handling", "Compiler Design", "Data Structures"

### 7. Cross-Reference Correctly

- Reference only active specs, or mark archived references: `SPEC_0015 (archived/deferred)`
- Use relative links in the README: `[SPEC_0001](SPEC_0001_DEFID.md)`
- When specs are complementary, state the relationship: "SPEC_0023 is *what* maps where, SPEC_0029 is *why*"

### 8. README Table Columns

The spec index (`spec/README.md`) MUST include:

| Column | Purpose |
|--------|---------|
| Spec | Link to the spec file |
| Title | Descriptive title |
| Domain | Topic tag for filtering (IR, phase, architecture, convention, etc.) |
| Lines | Approximate line count for context-window budgeting |
| Status | ACCEPTED, DRAFT, or REFERENCE |

## Checklist for New Specs

Before marking a spec as ACCEPTED:
- [ ] Summary is one sentence
- [ ] All code examples match actual codebase
- [ ] Rules use REQUIRED/PROHIBITED framing where possible
- [ ] Under 250 lines (or has a section index)
- [ ] README table entry includes Domain and Lines
- [ ] No references to archived specs without marking them as such
- [ ] Status matches reality (ACCEPTED = implemented)
