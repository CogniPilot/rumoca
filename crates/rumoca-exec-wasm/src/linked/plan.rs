//! The issued identity of every entry a linked product can invoke.
use super::LINKED_DISPATCH_REFUSED;
use crate::WasmCompileError;
use std::collections::BTreeMap;

/// The kind of an entry the dispatch reaches. The code is the dispatch's
/// `kind` argument; it is fixed and never reused.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

/// The issued identities of one product. Only the set assembler, fed the
/// checked products in its owner's order, builds one; there is no constructor
/// from caller-supplied triples. Equal keys at two positions are legal.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct LinkedEntryPlan {
    entries: BTreeMap<LinkedEntryId, [u64; 4]>,
}

impl LinkedEntryPlan {
    /// Every issued entry with its key words, in `(kind, index)` order.
    pub fn entries(&self) -> impl Iterator<Item = (LinkedEntryId, [u64; 4])> + '_ {
        self.entries.iter().map(|(id, key)| (*id, *key))
    }

    pub fn key_words(&self, id: LinkedEntryId) -> Option<[u64; 4]> {
        self.entries.get(&id).copied()
    }

    /// The next dense position of `kind`; a kinematic entry has its derivative's.
    pub(super) fn next_index(&self, kind: LinkedEntryKind) -> u32 {
        let issued = self.entries.keys().filter(|id| id.kind == kind).count();
        u32::try_from(issued).expect("issued entries are counted in u32")
    }

    pub(super) fn issue(&mut self, id: LinkedEntryId, key: [u64; 4]) {
        let previous = self.entries.insert(id, key);
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
