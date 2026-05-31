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

### 1. Write Rules As Tables, Not Essays

**Default shape — table of rules:** every multi-rule section MUST use a
table whose columns are `Rule | Owner/Where | Brief Justification`. Tables
scan in 2 seconds; prose scans in 30. A figure (ASCII tier diagram, IR
pipeline, etc.) belongs immediately above the table when the structure is
what the rule is about.

**REQUIRED — rules table:**

```markdown
| Rule | Owner | Why |
|---|---|---|
| Public IR fields use `IndexMap` | `rumoca-ir-*` crates | Deterministic serialization |
| Generated code embeds source-equation comments | codegen templates | Audit trail back to MLS source |
```

**REQUIRED — allow/deny lists are acceptable for short rule sets that
don't need a `Where` column:**

```markdown
**REQUIRED:**
- All IR fields use IndexMap for deterministic iteration
- Generated code includes source-equation comments

**PROHIBITED:**
- HashMap as a public field on any IR type
- Span::DUMMY without a justifying comment
```

**PROHIBITED — narrative prose for rules:**

```markdown
It is generally recommended that developers should consider using IndexMap
when they need deterministic iteration, although HashMap may sometimes be
appropriate in certain circumstances.
```

**Justification cells in tables MUST stay under 15 words.** If the
reasoning needs more, put it in a `**Why:**` block below the table. Long
prose inside table cells defeats the purpose of the table.

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

### 3. Keep The Spec Set Small And Each Spec Short

**Active spec count is capped.** The whole rule set must fit in a single
human reading session and a small number of AI context loads. If
contributors can't hold the set of rules in mind, the rules don't get
applied.

| Status | Cap | Rationale |
|---|---|---|
| ACCEPTED + DRAFT | 15 specs total | ~3000 lines of normative content at cap; fits 2-3 AI context loads |
| REFERENCE | uncapped | Lookup catalogs (e.g. SPEC_0022) are not rules; size doesn't burden rule comprehension |

Adding a 16th ACCEPTED/DRAFT spec requires either:
- merging into an existing spec, or
- moving inactive future-work proposals to `archive/deferred/`, or
- deleting inactive proposals that are not worth preserving.

Enforced by `crates/rumoca/tests/spec_budget_test.rs::test_active_spec_count_under_cap`.

### 3a. Hard Word and Line Budgets Per Spec

ACCEPTED design specs MUST fit a reader's working memory. Long specs get
skimmed; skimmed specs get misapplied.

| Spec size | Word budget | Line budget | Assessment |
|---|---|---|---|
| Ideal | < 500 words | < 80 lines | Fits in a single context load |
| Good | 500–1000 words | 80–150 lines | One focused topic |
| Acceptable | 1000–1800 words | 150–250 lines | Needs a clear section index |
| Too long | > 1800 words | > 250 lines | Split, trim, or add an index |

**Hard cap:** an ACCEPTED design spec MUST NOT exceed 2500 words / 350 lines
without an explicit `REFERENCE` status. Going over requires moving the spec
to REFERENCE (lookup catalogs only, like SPEC_0022) or splitting it into
multiple ACCEPTED specs.

**What to cut:**
- Implementation code that duplicates what's in the source (link to the file instead)
- Rationale for decisions that are obvious or uncontested
- Multiple examples showing the same pattern
- Aspirational features (move to DRAFT specs or issues)
- Long prose explaining a rule that already fits in a table row

### 4. Stay In Sync With the Codebase

A spec that doesn't match the code is worse than no spec — it actively misleads contributors.

**REQUIRED:**
- Specs MUST reflect the current codebase, not a planned future state
- If code structures change, the spec MUST be updated in the same PR
- If a spec is paused future work, move it to `archive/deferred/`
- If a spec is no longer implemented and not future work, delete it

**PROHIBITED:**
- ACCEPTED specs that describe unimplemented features
- Code examples using types or functions that don't exist
- Stale references to deleted or renamed specs

### 5. Spec Lifecycle and Governance

Every spec follows this lifecycle:

```
PROPOSED → ACCEPTED (majority vote)
         → REJECTED (majority vote) → delete
         → DRAFT (needs more work before vote)

DRAFT → DEFERRED (future work that maintainers want to preserve)

ACCEPTED → delete (if removed, rejected, or replaced)

REFERENCE (no vote needed — lookup tables, catalogs)
```

#### Statuses

| Status | Meaning |
|--------|---------|
| PROPOSED | New spec awaiting maintainer vote |
| ACCEPTED | Implemented and enforced — code MUST follow this spec |
| REJECTED | Voted down by maintainers; not retained in-tree |
| DRAFT | Work in progress — not yet ready for a vote, may describe unimplemented features |
| DEFERRED | Non-active future work in `archive/deferred/`; not review-gating |
| REFERENCE | Lookup table — not a set of rules (e.g., MLS contract catalog), no vote needed |

#### Voting Process

1. **Author opens a PR** with the new spec file (status: PROPOSED) and a README table entry
2. **Maintainers review** the spec in the PR — discussion happens in PR comments
3. **Vote**: each maintainer approves or requests changes
4. **Majority of maintainers approves** → status changes to ACCEPTED, PR merges
5. **Majority rejects** → the spec is removed before merge; the PR discussion records the rejection rationale

A spec may be sent back to DRAFT if maintainers agree it needs more work before a vote.

#### Amending Accepted Specs

Changes to ACCEPTED specs follow the same process — open a PR, maintainers vote. Trivial fixes (typos, line count updates, syncing code examples to match refactored code) do not require a vote.

### 6. Title and Summary Must Stand Alone

A contributor reading only the README table should know whether a spec is relevant to their task. The title and summary must be self-explanatory without reading the full spec.

**Good titles:** "Lean DAE (No Derived Data)", "Case-Sensitive Identifiers", "Phase-Local Error Types"

**Bad titles:** "Expression Handling", "Compiler Design", "Data Structures"

### 7. Cross-Reference Correctly

- Reference only active specs
- Deferred specs may reference other deferred specs; active specs should mention
  deferred specs only when explicitly labeling them as non-active future work
- Use relative links in the README: `[SPEC_0001](SPEC_0001_DEFID.md)`
- When specs are complementary, state the relationship clearly instead of duplicating the same rule in both files

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
- [ ] Multi-rule sections use a table (Rule | Owner/Where | Why), not prose
- [ ] All code examples match actual codebase
- [ ] Rules use REQUIRED/PROHIBITED framing where possible
- [ ] Under 1800 words / 250 lines (hard cap: 2500 words / 350 lines)
- [ ] README table entry includes Domain and Lines
- [ ] No references to deleted specs; deferred references are marked as non-active
- [ ] Status matches reality (ACCEPTED = implemented)
