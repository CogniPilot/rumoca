# Rumoca PR Review Checklist

> Archived legacy document. Superseded by `spec/SPEC_0025_PR_REVIEW_PROCESS.md`.
> This checklist is not part of the active PR compliance process.

This checklist ensures PRs maintain consistency with the architecture specs.
Use for both human reviewers and AI-assisted code generation.

## Quick Reference

Before approving, verify the PR doesn't violate these core specs:

### Stable IDs (SPEC_0013)
- [ ] All ID generation uses `stable_id(role_tag, canonical_bytes)` or `IdBuilder`
- [ ] No `format!()` + hash patterns for ID generation
- [ ] Role tags are unique byte constants (`b"var"`, `b"eq"`, etc.)

### Canonicalization (SPEC_0006)
- [ ] N-ary Add/Mul sorted by `semantic_digest()`, not a partial comparator
- [ ] No `expr_cmp` or similar functions that return `Ordering::Equal` for unhandled cases
- [ ] Children canonicalized before sorting

### Hashing (SPEC_0005)
- [ ] Cache keys use `[u8; 32]` digests, not `u64`
- [ ] `semantic_digest()` for 256-bit, not `semantic_hash()` for u64

### ~~Collision Detection (SPEC_0016)~~ **DEFERRED**
- Debug collision detection is not yet implemented

### ~~Eval Memoization (SPEC_0014)~~ **DEFERRED**
- Memoization is not yet implemented

### Ordered Collections (SPEC_0017)
- [ ] No direct `IndexMap::new()` without `// SAFETY:` comment
- [ ] Use `collect_sorted()` helper for IndexMap construction
- [ ] Vec collections sorted before use in deterministic contexts

### EqId Order-Independence (SPEC_0011)
- [ ] EqId derived from `(kind, instance_path, expr_digest)` — NO equation index
- [ ] Equations stored as `Vec<(EqId, Eq)>`, not `IndexMap<EqId, Eq>`
- [ ] Duplicate equations (same EqId) are valid and preserved
- [ ] Equations sorted by EqId in `finalize()`

### Case Sensitivity (SPEC_0010)
- [ ] No `to_lowercase()` or `to_ascii_lowercase()` on Modelica identifiers
- [ ] VarId, FuncId preserve original case

### CST vs AST (SPEC_0012)
- [ ] Formatter operates on CST (tokens + trivia), not AST
- [ ] AST code doesn't assume access to comments/whitespace

### Phase Errors (SPEC_0008)
- [ ] New errors use correct mnemonic prefix for their phase (ER = resolve, ET = typecheck, etc.)
- [ ] Errors defined in phase crate, not centrally

### DAE Contract (SPEC_0003, SPEC_0007)
- [ ] No derived data in DAE (incidence, sparsity, BLT)
- [ ] Variable classification matches spec (x, z, u, p, c, q)

---

## Red Flags (Reject PR if present)

1. **Partial comparator for sorting:**
   ```rust
   // REJECT: Returns Equal for unhandled
   _ => Ordering::Equal
   ```

2. **u64 cache keys:**
   ```rust
   // REJECT: Use [u8; 32] for cache keys
   HashMap<(u64, u64), Value>
   ```

3. **String formatting for IDs:**
   ```rust
   // REJECT: Use IdBuilder
   let input = format!("{}:{}", kind, name);
   ```

4. **Unguarded IndexMap construction:**
   ```rust
   // REJECT without SAFETY comment
   let mut map = IndexMap::new();
   ```

5. **Lowercase normalization:**
   ```rust
   // REJECT for identifiers
   name.to_lowercase()
   ```

6. **Equation index in EqId:**
   ```rust
   // REJECT: EqId must be order-independent
   EqId::new(kind, format!("{}:{}", path, equation_index), ...)
   ```

7. **IndexMap for equations:**
   ```rust
   // REJECT: Use Vec to handle duplicates
   pub ode: IndexMap<EqId, Eq>
   ```

---

## Spec Quick Links

| Spec | Title | Status | Key Invariant |
|------|-------|--------|---------------|
| [0001](SPEC_0001_DEFID.md) | DefId | ACCEPTED | Every declaration gets unique DefId |
| [0002](SPEC_0002_SCOPE_TREE.md) | Scope Tree | ACCEPTED | Push/pop/def pattern |
| [0003](SPEC_0003_HYBRID_DAE.md) | Hybrid DAE | ACCEPTED | x, z, u, p, c, q, events |
| [0004](SPEC_0004_INSTANTIATE_FLATTEN.md) | Instantiate/Flatten | ACCEPTED | Separate phases |
| [0005](SPEC_0005_TWO_HASH.md) | Two-Hash | ACCEPTED | Semantic vs debug |
| [0006](SPEC_0006_NARY_CANONICAL.md) | Canonicalization | ACCEPTED | Digest-based sorting |
| [0007](SPEC_0007_LEAN_DAE.md) | Lean DAE | ACCEPTED | No derived data |
| [0008](SPEC_0008_PHASE_ERRORS.md) | Phase Errors | ACCEPTED | EP (parse), ER/ET/EI/EF/ED/EC mnemonic prefixes |
| [0009](SPEC_0009_COMMON_CRATE.md) | Common Crate | ACCEPTED | Single foundation |
| [0010](SPEC_0010_CASE_SENSITIVE.md) | Case Sensitive | ACCEPTED | Preserve case |
| [0011](SPEC_0011_EQID_ORDER_INDEPENDENT.md) | EqId | ACCEPTED | No equation_index |
| [0012](../deferred/SPEC_0012_CST_AST.md) | CST vs AST | **DEFERRED** | CST for formatter |
| [0013](SPEC_0013_STABLE_ID.md) | Stable ID | ACCEPTED | Role-tagged, length-prefixed |
| [0014](../deferred/SPEC_0014_EVAL_MEMO.md) | Eval Memo | **DEFERRED** | EVAL_VERSION + 256-bit |
| [0015](../deferred/SPEC_0015_FORMATTER.md) | Formatter | **DEFERRED** | Token-based |
| [0016](../deferred/SPEC_0016_COLLISION_DETECTION.md) | Collision | **DEFERRED** | Debug detection |
| [0017](SPEC_0017_ORDERED_COLLECTIONS.md) | Ordered Collections | ACCEPTED | collect_sorted() |

**Note:** DEFERRED specs document future design intent. PRs should not be blocked for non-compliance with DEFERRED specs, but should avoid introducing patterns that would make future compliance harder.

---

## For AI Assistants

When generating code for Rumoca:

1. **Always check relevant specs before writing code**
2. **Use exact patterns from specs, not approximations**
3. **Reference spec numbers in code comments**
4. **Ask for clarification if specs conflict**

Example comment style:
```rust
// Per SPEC_0006: sort by semantic_digest(), not partial comparator
v.sort_by(|a, b| a.semantic_digest().cmp(&b.semantic_digest()));
```
