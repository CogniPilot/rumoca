//! The issued identity of every entry a linked product can invoke.
use super::LINKED_DISPATCH_REFUSED;
use crate::{NativeGatherFault, NativeLinearSolveFault, TypedCallFault, WasmCompileError};
use rumoca_ir_solve::fmi::FmiDerivativeWorkspace;
#[cfg(feature = "precompiled-programs")]
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

/// The kind of an entry the dispatch reaches. The code is the dispatch's
/// `kind` argument; it is fixed and never reused.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "precompiled-programs", derive(Serialize, Deserialize))]
#[repr(u32)]
pub enum LinkedEntryKind {
    /// `eval_derivatives` of a complete derivative control module (9 arguments).
    Derivative = 1,
    /// `eval_kinematic` of the same control module (5 arguments).
    Kinematic = 2,
    /// `eval_assignments` of an exact refresh assignment schedule.
    Exact = 3,
    /// `eval_assignments` of issued assignment rows.
    Issued = 4,
    /// One entry of a shared private program module.
    Private = 5,
    /// `eval_private` of a retained event program.
    Event = 6,
}

impl LinkedEntryKind {
    pub const ALL: [Self; 6] = [
        Self::Derivative,
        Self::Kinematic,
        Self::Exact,
        Self::Issued,
        Self::Private,
        Self::Event,
    ];

    pub const fn code(self) -> u32 {
        self as u32
    }

    /// Only the complete derivative entry takes the nine-argument convention.
    pub(super) const fn complete_derivative(self) -> bool {
        matches!(self, Self::Derivative)
    }

    /// The word of its link symbol: distinct per kind and free of digits.
    const fn word(self) -> &'static str {
        match self {
            Self::Derivative => "derivative",
            Self::Kinematic => "kinematic",
            Self::Exact => "exact",
            Self::Issued => "issued",
            Self::Private => "private",
            Self::Event => "event",
        }
    }
}

/// An entry's position in its module-set owner's list for its kind.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "precompiled-programs", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "precompiled-programs", serde(deny_unknown_fields))]
pub struct LinkedEntryId {
    pub kind: LinkedEntryKind,
    pub index: u32,
}

impl LinkedEntryId {
    /// `rumoca_k_<kind>_<index>`: unique because `(kind, index)` is.
    pub fn symbol(&self) -> String {
        format!("rumoca_k_{}_{}", self.kind.word(), self.index)
    }
}

/// An entry a product can demand at run time that its saved set does not
/// hold. The linked profile has no form for these; the set owner names every
/// one, including lazily demanded paths, and the assembler refuses.
#[derive(Clone, Copy, Debug)]
pub enum LinkedDemand<'a> {
    /// A residual row kernel (`eval_residual`), named by its owner.
    Residual { name: &'a str },
    /// A typed pure-call entry (`eval_typed_call`), named by its owner.
    TypedCall { name: &'a str },
}

impl LinkedDemand<'_> {
    pub(super) fn refusal(&self) -> WasmCompileError {
        let (kind, name) = match self {
            Self::Residual { name } => ("residual", name),
            Self::TypedCall { name } => ("typed-call", name),
        };
        WasmCompileError::Input(format!(
            "linked product needs {kind} entry {name}, which the linked profile cannot carry"
        ))
    }
}

/// The four little-endian words of a 32-byte source key written as 64
/// lowercase hex digits, as the dispatch compares them.
pub fn linked_key_words(source_key: &str) -> Result<[u64; 4], WasmCompileError> {
    let malformed = || {
        WasmCompileError::Input(format!(
            "linked source key {source_key:?} is not 32 hex bytes"
        ))
    };
    let digits = source_key.as_bytes();
    if digits.len() != 64
        || !digits
            .iter()
            .all(|digit| matches!(digit, b'0'..=b'9' | b'a'..=b'f'))
    {
        return Err(malformed());
    }
    let mut words = [0u64; 4];
    for (word, chunk) in words.iter_mut().zip(digits.chunks_exact(16)) {
        let mut bytes = [0u8; 8];
        for (byte, pair) in bytes.iter_mut().zip(chunk.chunks_exact(2)) {
            let text = std::str::from_utf8(pair).map_err(|_| malformed())?;
            *byte = u8::from_str_radix(text, 16).map_err(|_| malformed())?;
        }
        *word = u64::from_le_bytes(bytes);
    }
    Ok(words)
}

/// Every fault table an entry's status is decoded with, as emitted with it.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "precompiled-programs", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "precompiled-programs", serde(deny_unknown_fields))]
pub struct LinkedFaultTables {
    pub typed: Vec<TypedCallFault>,
    pub gather: Vec<NativeGatherFault>,
    pub linear: Vec<NativeLinearSolveFault>,
}

impl LinkedFaultTables {
    pub(super) fn statuses(&self) -> impl Iterator<Item = u32> + '_ {
        self.typed
            .iter()
            .map(|fault| fault.status)
            .chain(self.gather.iter().map(|fault| fault.status))
            .chain(self.linear.iter().map(|fault| fault.status))
    }

    /// The source fault a nonzero entry status names. Map the dispatch's
    /// refusal with `linked_entry_status` first: it names no fault.
    pub fn status_error(&self, status: u32) -> WasmCompileError {
        crate::program_payload::decode_status(status, &self.typed, &self.gather, &self.linear)
    }
}

/// The issued derivative workspace as plain extents, so it travels with the
/// plan; a receiver compares it with the workspace its bound operation issued.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "precompiled-programs", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "precompiled-programs", serde(deny_unknown_fields))]
pub struct LinkedWorkspace {
    regions: [(u32, u32); 7],
    bytes: u32,
    snapshot: u32,
    helper: u32,
}

impl LinkedWorkspace {
    pub(super) fn of(workspace: FmiDerivativeWorkspace) -> Self {
        Self {
            regions: workspace.regions(),
            bytes: workspace.bytes(),
            snapshot: workspace.snapshot(),
            helper: workspace.helper(),
        }
    }

    /// Argument ordinals and byte extents, as `FmiDerivativeWorkspace::regions`.
    pub fn regions(&self) -> [(u32, u32); 7] {
        self.regions
    }

    pub fn bytes(&self) -> u32 {
        self.bytes
    }

    /// Whether `workspace` is exactly the workspace this entry was issued with.
    pub fn matches(&self, workspace: FmiDerivativeWorkspace) -> bool {
        *self == Self::of(workspace)
    }
}

/// What a bound entry keeps beside its triple to check its frame and decode
/// its status, from the same emission as its linked module.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "precompiled-programs", derive(Serialize, Deserialize))]
pub enum LinkedBinding {
    /// The issued workspace and the tables of the control's own helpers in
    /// `static_refresh, dynamic_refresh, values, kinematic_values` order;
    /// an omitted helper is `None`.
    Derivative {
        workspace: LinkedWorkspace,
        helpers: Box<[Option<LinkedFaultTables>; 4]>,
    },
    /// The scratch extent and fault tables of a kernel entry.
    Kernel {
        scratch_bytes: u32,
        faults: LinkedFaultTables,
    },
}

/// One issued entry as a receiver stores it: the canonical owner-issued
/// triple `(kind, index, key)` and its binding. The receiver passes exactly
/// this triple to the dispatch and never re-derives an index.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "precompiled-programs", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "precompiled-programs", serde(deny_unknown_fields))]
pub struct LinkedEntry {
    id: LinkedEntryId,
    key: [u64; 4],
    binding: LinkedBinding,
}

impl LinkedEntry {
    pub(super) fn new(id: LinkedEntryId, key: [u64; 4], binding: LinkedBinding) -> Self {
        Self { id, key, binding }
    }

    pub fn id(&self) -> LinkedEntryId {
        self.id
    }

    /// The key words the dispatch compares, `k0..k3`.
    pub fn key_words(&self) -> [u64; 4] {
        self.key
    }

    pub fn binding(&self) -> &LinkedBinding {
        &self.binding
    }
}

/// The issued entries of one product. Only the set assembler, fed the
/// checked products in its owner's order, builds one; there is no constructor
/// from caller-supplied triples. Equal keys at two positions are legal.
/// It has no deserialiser: a plan read from a wire would be caller triples.
/// A receiver issues its own plan from its checked sources and certifies the
/// carried artifact against it (`LinkedArtifact::certify`).
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct LinkedEntryPlan {
    entries: BTreeMap<LinkedEntryId, LinkedEntry>,
}

impl LinkedEntryPlan {
    /// Every issued entry with its key words, in `(kind, index)` order.
    pub fn entries(&self) -> impl Iterator<Item = (LinkedEntryId, [u64; 4])> + '_ {
        self.entries.values().map(|entry| (entry.id, entry.key))
    }

    /// Every issued entry with its binding, in `(kind, index)` order.
    pub fn linked_entries(&self) -> impl Iterator<Item = &LinkedEntry> {
        self.entries.values()
    }

    pub fn entry(&self, id: LinkedEntryId) -> Option<&LinkedEntry> {
        self.entries.get(&id)
    }

    pub fn key_words(&self, id: LinkedEntryId) -> Option<[u64; 4]> {
        self.entries.get(&id).map(|entry| entry.key)
    }

    /// The next dense position of `kind`; a kinematic entry has its derivative's.
    pub(super) fn next_index(&self, kind: LinkedEntryKind) -> u32 {
        let issued = self.entries.keys().filter(|id| id.kind == kind).count();
        u32::try_from(issued).expect("issued entries are counted in u32")
    }

    pub(super) fn issue(&mut self, entry: LinkedEntry) {
        let previous = self.entries.insert(entry.id, entry);
        debug_assert!(previous.is_none(), "an assembler issues each position once");
    }
}

/// No status an entry returns may equal the dispatch's refusal.
pub(super) fn refuse_reserved_status(
    statuses: impl IntoIterator<Item = u32>,
) -> Result<(), WasmCompileError> {
    if statuses
        .into_iter()
        .any(|status| status == LINKED_DISPATCH_REFUSED)
    {
        return Err(WasmCompileError::Backend(
            "an entry status equals the linked dispatch refusal".into(),
        ));
    }
    Ok(())
}
