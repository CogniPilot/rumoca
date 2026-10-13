//! Wire form of pure-call sites.
//!
//! A site carries no fact of its own: its interface is the interface of its
//! owner. The wire therefore stores only the owner id, the owner table is
//! serialized once, and reading a site resolves the id against the table that
//! the enclosing reader made current with [`SolvePureCallTable::enter_wire_scope`].
//! Resolution is the single owner of this mapping, so every container that
//! holds a site (any operation, event action, observation or transaction)
//! shares it without a per-container walk. In memory the resolved sites share
//! the owner's immutable interface storage.

use super::SolvePureCallTable;
use super::{SolvePureCallDirectionalSite, SolvePureCallOwnerId, SolvePureCallSite};
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::cell::RefCell;
use std::marker::PhantomData;
use std::rc::Rc;

type Sites = Rc<[SolvePureCallSite]>;

thread_local! {
    static CURRENT_OWNERS: RefCell<Option<Sites>> = const { RefCell::new(None) };
}

/// Why a serialized site could not be resolved.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SolvePureCallWireError {
    /// No owner table is current: the table must be read before any site.
    NoOwnerTable,
    /// The serialized owner id names no owner of the current table.
    DanglingOwner(SolvePureCallOwnerId),
    /// A directional site names an owner without a directional interface.
    NoDirectionalInterface(SolvePureCallOwnerId),
}

impl std::fmt::Display for SolvePureCallWireError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NoOwnerTable => formatter
                .write_str("pure-call site read before its owner table (pure_calls must precede)"),
            Self::DanglingOwner(owner) => {
                write!(
                    formatter,
                    "pure-call site names unknown owner {}",
                    owner.index()
                )
            }
            Self::NoDirectionalInterface(owner) => write!(
                formatter,
                "directional pure-call site names owner {} without a directional interface",
                owner.index()
            ),
        }
    }
}

impl std::error::Error for SolvePureCallWireError {}

/// Keeps one owner table current for site reads on this thread until dropped.
/// Scopes nest; dropping restores the previous table.
#[must_use = "sites resolve only while the scope is alive"]
pub struct SolvePureCallWireScope {
    previous: Option<Sites>,
    _not_send: PhantomData<*const ()>,
}

impl Drop for SolvePureCallWireScope {
    fn drop(&mut self) {
        let previous = self.previous.take();
        CURRENT_OWNERS.with(|current| *current.borrow_mut() = previous);
    }
}

impl SolvePureCallTable {
    /// Make this table the one serialized sites resolve against.
    pub fn enter_wire_scope(&self) -> SolvePureCallWireScope {
        let sites: Sites = self.owners.iter().map(|owner| owner.call_site()).collect();
        let previous = CURRENT_OWNERS.with(|current| current.borrow_mut().replace(sites));
        SolvePureCallWireScope {
            previous,
            _not_send: PhantomData,
        }
    }
}

/// An owner table read from the wire that stays current for the sites that
/// follow it in the same document.
pub struct ScopedSolvePureCallTable {
    table: SolvePureCallTable,
    scope: SolvePureCallWireScope,
}

impl ScopedSolvePureCallTable {
    /// End the scope and return the table.
    #[must_use]
    pub fn into_table(self) -> SolvePureCallTable {
        drop(self.scope);
        self.table
    }
}

impl<'de> Deserialize<'de> for ScopedSolvePureCallTable {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let table = SolvePureCallTable::deserialize(deserializer)?;
        let scope = table.enter_wire_scope();
        Ok(Self { table, scope })
    }
}

fn resolve(owner: SolvePureCallOwnerId) -> Result<SolvePureCallSite, SolvePureCallWireError> {
    CURRENT_OWNERS.with(|current| {
        let current = current.borrow();
        let sites = current
            .as_ref()
            .ok_or(SolvePureCallWireError::NoOwnerTable)?;
        sites
            .get(owner.index() as usize)
            .filter(|site| site.owner() == owner)
            .cloned()
            .ok_or(SolvePureCallWireError::DanglingOwner(owner))
    })
}

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct OwnerReference {
    owner: SolvePureCallOwnerId,
}

impl Serialize for SolvePureCallSite {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        OwnerReference { owner: self.owner }.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for SolvePureCallSite {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let reference = OwnerReference::deserialize(deserializer)?;
        resolve(reference.owner).map_err(serde::de::Error::custom)
    }
}

impl Serialize for SolvePureCallDirectionalSite {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        OwnerReference { owner: self.owner }.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for SolvePureCallDirectionalSite {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let reference = OwnerReference::deserialize(deserializer)?;
        let site = resolve(reference.owner).map_err(serde::de::Error::custom)?;
        site.directional
            .map(|directional| *directional)
            .ok_or(SolvePureCallWireError::NoDirectionalInterface(
                reference.owner,
            ))
            .map_err(serde::de::Error::custom)
    }
}
