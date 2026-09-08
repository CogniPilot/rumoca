import Aeneas

open Aeneas

/- These abstractions are restricted to the read-only root-value projection.
Universal results quantify over their broadened domains, conditional on the
stated Rust storage correspondence. A Lean witness alone does not establish a
production-reachable Rust root. Broadened fields must stay unobserved, and a
production mutation claim also needs its independent real-Rust witness. Reading
map, hasher or nonzero payloads requires a new correspondence review. -/

/-- Binary64 storage only; no arithmetic or Rust PartialEq correspondence. -/
structure Aeneas.Std.F64 where
  storageBits : Aeneas.Std.U64

/-- Immutable logical contents, not sharing, reference counts or allocation. -/
@[rust_type "alloc::sync::Arc"]
structure alloc.sync.Arc (T : Type) where
  value : T

/-- Logical ordered entries and hasher payload. The root projection does not
observe either field. Capacity, buckets, key uniqueness and allocation are not
modelled; no IndexMap operation or whole-root identity claim is admitted. -/
@[rust_type "indexmap::map::IndexMap"]
structure indexmap.map.IndexMap (K V S : Type) where
  entries : List (K × V)
  hasher : S

/-- The pinned Rust type stores two u64 keys. Neither key is observed here;
hashing and random initialization have no admitted operations. -/
@[rust_type "std::hash::random::RandomState"]
structure std.hash.random.RandomState where
  k0 : Aeneas.Std.U64
  k1 : Aeneas.Std.U64

/-- Preserve the associated inner payload, not T. Its nonzero refinement is
forgotten, broadening the model domain; no NonZero operation is admitted. -/
@[rust_type "core::num::nonzero::NonZero"]
structure core.num.nonzero.NonZero (T Inner : Type) where
  value : Inner

/-- Unobserved underlying integer; the Rust nonzero range is not asserted. -/
@[rust_type "core::num::niche_types::NonZeroU32Inner"]
structure core.num.niche_types.NonZeroU32Inner where
  value : Aeneas.Std.U32

/-- Unobserved underlying integer; the Rust nonzero range is not asserted. -/
@[rust_type "core::num::niche_types::NonZeroU64Inner"]
structure core.num.niche_types.NonZeroU64Inner where
  value : Aeneas.Std.U64
