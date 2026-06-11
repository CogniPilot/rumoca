//! Interned variable names: identity, shared payload, and segmentation.
//!
//! `VarName` is the flat/DAE IR's variable identity. Equality and hashing use
//! the process-local interned id; the payload carries the rendered text plus
//! its top-level segmentation, computed exactly once at intern time so no
//! semantic code ever re-parses a rendered path.

use super::*;

/// Process-local interned identity for a [`VarName`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, Serialize, Deserialize)]
pub struct VarNameId(pub u32);

impl VarNameId {
    /// Get the compact interned index.
    pub fn index(self) -> u32 {
        self.0
    }
}

/// A globally unique, fully-qualified variable name (e.g., `"body.position.x"`).
///
/// Shared by the flat and DAE IRs.
///
/// `VarName` is serialized and displayed as text, but equality and hashing use
/// an interned process-local ID so hot lookup paths do not repeatedly compare
/// or hash long flattened component paths.
#[derive(Clone)]
pub struct VarName {
    id: VarNameId,
    data: Arc<VarNameData>,
}

/// Shared payload of an interned [`VarName`]: the rendered text plus its
/// top-level segmentation, computed exactly once at intern time. Accessors on
/// `VarName` slice these precomputed boundaries — no path parsing happens at
/// use sites.
struct VarNameData {
    text: Box<str>,
    /// Byte offsets of the top-level dots (dots inside subscript brackets are
    /// not segment boundaries).
    top_level_dots: Box<[u32]>,
}

impl VarNameData {
    fn new(text: &str) -> Self {
        let mut top_level_dots = Vec::new();
        let mut bracket_depth = 0usize;
        for (index, byte) in text.bytes().enumerate() {
            match byte {
                b'[' => bracket_depth += 1,
                b']' => bracket_depth = bracket_depth.saturating_sub(1),
                b'.' if bracket_depth == 0 => {
                    top_level_dots
                        .push(u32::try_from(index).expect("VarName text exceeds u32 bytes"));
                }
                _ => {}
            }
        }
        Self {
            text: Box::from(text),
            top_level_dots: top_level_dots.into_boxed_slice(),
        }
    }
}

impl VarName {
    /// Create a new variable name.
    pub fn new(name: impl Into<String>) -> Self {
        intern_var_name(&name.into())
    }

    /// Get the variable name as a string slice.
    pub fn as_str(&self) -> &str {
        &self.data.text
    }

    pub fn last_segment(&self) -> &str {
        self.data
            .top_level_dots
            .last()
            .map(|dot| &self.as_str()[*dot as usize + 1..])
            .unwrap_or_else(|| self.as_str())
    }

    /// Split into `(enclosing scope, last segment)` when the name is nested.
    ///
    /// Subscript brackets keep their embedded dots: `a.b[c.d].e` splits into
    /// `("a.b[c.d]", "e")`.
    pub fn scope_split(&self) -> Option<(&str, &str)> {
        let dot = *self.data.top_level_dots.last()? as usize;
        Some((&self.as_str()[..dot], &self.as_str()[dot + 1..]))
    }

    /// The enclosing scope prefix (everything before the last segment).
    pub fn enclosing_scope(&self) -> Option<&str> {
        self.scope_split().map(|(scope, _)| scope)
    }

    /// True when the name is nested inside a component scope.
    pub fn is_nested(&self) -> bool {
        !self.data.top_level_dots.is_empty()
    }

    /// Split into `(root segment, remainder)` when the name is nested.
    pub fn root_split(&self) -> Option<(&str, &str)> {
        let dot = *self.data.top_level_dots.first()? as usize;
        Some((&self.as_str()[..dot], &self.as_str()[dot + 1..]))
    }

    /// Top-level segments of the name; subscript brackets keep their embedded
    /// dots (`bus[data.medium].pin.v` yields `["bus[data.medium]", "pin", "v"]`).
    pub fn segments(&self) -> Vec<&str> {
        let text = self.as_str();
        let dots = &self.data.top_level_dots;
        let mut segments = Vec::with_capacity(dots.len() + 1);
        let mut start = 0usize;
        for dot in dots.iter() {
            let dot = *dot as usize;
            if start < dot {
                segments.push(&text[start..dot]);
            }
            start = dot + 1;
        }
        if start < text.len() {
            segments.push(&text[start..]);
        }
        segments
    }

    /// Get the compact process-local interned identity.
    pub fn id(&self) -> VarNameId {
        self.id
    }
}

impl Default for VarName {
    fn default() -> Self {
        Self::new("")
    }
}

impl std::fmt::Debug for VarName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("VarName").field(&self.as_str()).finish()
    }
}

impl PartialEq for VarName {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for VarName {}

impl PartialOrd for VarName {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for VarName {
    fn cmp(&self, other: &Self) -> Ordering {
        self.as_str().cmp(other.as_str())
    }
}

impl Hash for VarName {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl Serialize for VarName {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(self.as_str())
    }
}

impl<'de> Deserialize<'de> for VarName {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        String::deserialize(deserializer).map(Self::new)
    }
}

impl std::fmt::Display for VarName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl From<&str> for VarName {
    fn from(s: &str) -> Self {
        Self::new(s)
    }
}

impl From<String> for VarName {
    fn from(s: String) -> Self {
        Self::new(s)
    }
}

#[derive(Default)]
struct VarNameInterner {
    ids: IndexMap<String, VarNameId>,
    payloads: Vec<Arc<VarNameData>>,
}

impl VarNameInterner {
    fn intern(&mut self, text: &str) -> VarName {
        if let Some(var_name) = self.get(text) {
            return var_name;
        }

        let id = VarNameId(
            u32::try_from(self.payloads.len())
                .expect("process-local VarName interner exceeded u32::MAX entries"),
        );
        let data = Arc::new(VarNameData::new(text));
        self.payloads.push(data.clone());
        self.ids.insert(text.to_string(), id);
        VarName { id, data }
    }

    fn get(&self, text: &str) -> Option<VarName> {
        let id = *self.ids.get(text)?;
        let data = self.payloads.get(id.index() as usize)?.clone();
        Some(VarName { id, data })
    }
}

static VAR_NAME_INTERNER: OnceLock<RwLock<VarNameInterner>> = OnceLock::new();

fn intern_var_name(text: &str) -> VarName {
    let interner = VAR_NAME_INTERNER.get_or_init(|| RwLock::new(VarNameInterner::default()));
    {
        let guard = interner
            .read()
            .expect("VarName interner read lock should not be poisoned");
        if let Some(var_name) = guard.get(text) {
            return var_name;
        }
    }

    let mut guard = interner
        .write()
        .expect("VarName interner write lock should not be poisoned");
    guard.intern(text)
}
