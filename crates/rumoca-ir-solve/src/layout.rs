use indexmap::IndexMap;
use rumoca_core::{ComponentReference, Span, Subscript, VarName};
use serde::ser::SerializeStruct;
use serde::{Deserialize, Serialize};

fn intern_key_map<T>(values: IndexMap<String, T>) -> IndexMap<VarName, T> {
    values
        .into_iter()
        .map(|(name, value)| (VarName::new(name), value))
        .collect()
}

/// A scalar operand stores logical indices, not a second byte-address authority.
///
/// ```compile_fail
/// use rumoca_ir_solve::ScalarSlot;
/// let _ = ScalarSlot::Y { index: 0, byte_offset: 0 };
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub enum ScalarSlot {
    Time,
    Y { index: usize },
    P { index: usize },
    Constant(f64),
}

/// One logical storage column, independent of its concrete address format.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum SolveStorageColumn {
    Y,
    P,
}

/// A writable logical coordinate; its containing owner establishes membership.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum SolveStorageCoordinate {
    Y(usize),
    P(usize),
}

impl SolveStorageCoordinate {
    #[must_use]
    pub const fn new(column: SolveStorageColumn, index: usize) -> Self {
        match column {
            SolveStorageColumn::Y => Self::Y(index),
            SolveStorageColumn::P => Self::P(index),
        }
    }

    #[must_use]
    pub const fn column(self) -> SolveStorageColumn {
        match self {
            Self::Y(_) => SolveStorageColumn::Y,
            Self::P(_) => SolveStorageColumn::P,
        }
    }

    #[must_use]
    pub const fn index(self) -> usize {
        match self {
            Self::Y(index) | Self::P(index) => index,
        }
    }

    #[must_use]
    pub const fn scalar_slot(self) -> ScalarSlot {
        match self {
            Self::Y(index) => scalar_slot_y(index),
            Self::P(index) => scalar_slot_p(index),
        }
    }
}

impl ScalarSlot {
    #[must_use]
    pub const fn storage_coordinate(self) -> Option<SolveStorageCoordinate> {
        match self {
            Self::Y { index } => Some(SolveStorageCoordinate::Y(index)),
            Self::P { index } => Some(SolveStorageCoordinate::P(index)),
            Self::Time | Self::Constant(_) => None,
        }
    }
}

#[derive(Debug, Clone, Default, Deserialize)]
pub struct VarLayout {
    bindings: IndexMap<VarName, ScalarSlot>,
    shapes: IndexMap<VarName, Vec<usize>>,
    shape_spans: IndexMap<VarName, Span>,
    #[serde(skip)]
    shape_indexed_keys: IndexMap<VarName, ComponentReferenceKey>,
    #[serde(skip)]
    indexed_bindings: IndexMap<ComponentReferenceKey, Vec<IndexedScalarSlot>>,
    y_scalars: usize,
    p_scalars: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ComponentReferenceKey {
    Source {
        def_id: rumoca_core::DefId,
        parts: Vec<ComponentReferenceKeyPart>,
    },
    Generated {
        name: VarName,
    },
}

/// One member step of a [`ComponentReferenceKey::Source`] path.
///
/// `ident` is interned: the solver layout is keyed by these parts, so the
/// member name must hash as an id rather than as text. `VarName` serializes and
/// deserializes as its spelling, so the on-disk Solve IR shape is unchanged —
/// text crosses the process boundary, ids are used inside it.
///
/// This is the member *name*, not a member `DefId`: `ComponentReference`
/// carries a single `def_id` for the whole chain and no per-part id exists to
/// hold yet. Completing that is the remaining half of the (DefId, flat-offset)
/// layout identity.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ComponentReferenceKeyPart {
    pub ident: VarName,
    pub subscripts: Vec<ComponentReferenceSubscriptKey>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ComponentReferenceSubscriptKey {
    Index(i64),
    Colon,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ComponentReferenceKeyError {
    pub span: Span,
    pub kind: ComponentReferenceKeyErrorKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ComponentReferenceKeyErrorKind {
    DynamicSubscript,
}

impl std::fmt::Display for ComponentReferenceKeyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            ComponentReferenceKeyErrorKind::DynamicSubscript => {
                write!(f, "component reference contains a dynamic subscript")
            }
        }
    }
}

impl std::error::Error for ComponentReferenceKeyError {}

impl ComponentReferenceKey {
    pub fn generated(name: impl Into<String>) -> Self {
        Self::Generated {
            name: VarName::new(name),
        }
    }

    pub fn from_component_reference(
        reference: &ComponentReference,
    ) -> Result<Self, ComponentReferenceKeyError> {
        let def_id = reference.target_def_id();
        let parts = reference
            .parts()
            .iter()
            .map(|part| {
                let subscripts = part
                    .subs
                    .iter()
                    .map(component_reference_subscript_key)
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(ComponentReferenceKeyPart {
                    ident: VarName::intern(&part.ident),
                    subscripts,
                })
            })
            .collect::<Result<Vec<_>, ComponentReferenceKeyError>>()?;
        Ok(Self::Source { def_id, parts })
    }
}

fn component_reference_subscript_key(
    subscript: &Subscript,
) -> Result<ComponentReferenceSubscriptKey, ComponentReferenceKeyError> {
    match subscript {
        Subscript::Index { value, .. } => Ok(ComponentReferenceSubscriptKey::Index(*value)),
        Subscript::Colon { .. } => Ok(ComponentReferenceSubscriptKey::Colon),
        Subscript::Expr { span, .. } => Err(ComponentReferenceKeyError {
            span: *span,
            kind: ComponentReferenceKeyErrorKind::DynamicSubscript,
        }),
    }
}

impl Serialize for VarLayout {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_struct("VarLayout", 5)?;
        state.serialize_field("bindings", &self.bindings)?;
        state.serialize_field("shapes", &self.shapes)?;
        state.serialize_field("shape_spans", &self.shape_spans)?;
        state.serialize_field("y_scalars", &self.y_scalars)?;
        state.serialize_field("p_scalars", &self.p_scalars)?;
        state.end()
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IndexedScalarSlot {
    pub indices: Vec<usize>,
    pub slot: ScalarSlot,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VarLayoutShapeContractError {
    ShapeWithoutBinding {
        variable: String,
        span: Option<Span>,
    },
    ShapeSpanMissing {
        variable: String,
    },
    ShapeSpanWithoutShape {
        variable: String,
        span: Span,
    },
    EmptyShape {
        variable: String,
        span: Option<Span>,
    },
    ShapeOutOfBounds {
        variable: String,
        start: usize,
        count: usize,
        available: usize,
        span: Option<Span>,
    },
    ShapeSizeOverflow {
        variable: String,
        span: Option<Span>,
    },
}

impl VarLayoutShapeContractError {
    pub fn source_span(&self) -> Option<Span> {
        match self {
            Self::ShapeWithoutBinding { span, .. }
            | Self::EmptyShape { span, .. }
            | Self::ShapeOutOfBounds { span, .. }
            | Self::ShapeSizeOverflow { span, .. } => *span,
            Self::ShapeSpanWithoutShape { span, .. } => Some(*span),
            Self::ShapeSpanMissing { .. } => None,
        }
    }
}

impl std::fmt::Display for VarLayoutShapeContractError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ShapeWithoutBinding { variable, .. } => {
                write!(f, "shape metadata for `{variable}` has no scalar binding")
            }
            Self::ShapeSpanMissing { variable } => {
                write!(f, "shape metadata for `{variable}` has no source span")
            }
            Self::ShapeSpanWithoutShape { variable, .. } => {
                write!(f, "source span metadata for `{variable}` has no shape")
            }
            Self::EmptyShape { variable, .. } => {
                write!(f, "shape metadata for `{variable}` has empty dimensions")
            }
            Self::ShapeOutOfBounds {
                variable,
                start,
                count,
                available,
                ..
            } => {
                let end = start
                    .checked_add(*count)
                    .map_or_else(|| "overflow".to_string(), |end| end.to_string());
                write!(
                    f,
                    "shape metadata for `{variable}` covers slots {start}..{end} but only {available} slots are available"
                )
            }
            Self::ShapeSizeOverflow { variable, .. } => {
                write!(
                    f,
                    "shape metadata for `{variable}` has too many scalar slots"
                )
            }
        }
    }
}

impl std::error::Error for VarLayoutShapeContractError {}

impl VarLayout {
    pub fn from_parts(
        bindings: IndexMap<String, ScalarSlot>,
        y_scalars: usize,
        p_scalars: usize,
    ) -> Self {
        Self {
            bindings: intern_key_map(bindings),
            shapes: IndexMap::new(),
            shape_spans: IndexMap::new(),
            shape_indexed_keys: IndexMap::new(),
            indexed_bindings: IndexMap::new(),
            y_scalars,
            p_scalars,
        }
    }

    /// Build a layout from its parts, deriving each shaped variable's indexed
    /// element slots from its shape.
    ///
    /// This is the constructor a compilation takes. A shaped Y or P variable's
    /// elements occupy a contiguous run, so the element slots follow from the
    /// base slot and the shape and are never supplied.
    pub fn from_parts_with_shapes_and_spans(
        bindings: IndexMap<String, ScalarSlot>,
        shapes: IndexMap<String, Vec<usize>>,
        shape_spans: IndexMap<String, Span>,
        y_scalars: usize,
        p_scalars: usize,
    ) -> Result<Self, VarLayoutShapeContractError> {
        let bindings = intern_key_map(bindings);
        let shapes = intern_key_map(shapes);
        let shape_spans = intern_key_map(shape_spans);
        let indexed_bindings =
            indexed_bindings_from_shapes(&bindings, &shapes, &shape_spans, y_scalars, p_scalars)?;
        Self::checked(
            bindings,
            shapes,
            shape_spans,
            indexed_bindings,
            y_scalars,
            p_scalars,
        )
    }

    /// The span-free spelling of [`Self::from_parts_with_shapes_and_spans`].
    ///
    /// A layout built without shape spans reports a shape-contract violation
    /// with no source to point at, so a compilation always carries them. Only
    /// fixtures, which have no source in the first place, go without.
    #[cfg(test)]
    fn from_parts_with_shapes(
        bindings: IndexMap<String, ScalarSlot>,
        shapes: IndexMap<String, Vec<usize>>,
        y_scalars: usize,
        p_scalars: usize,
    ) -> Result<Self, VarLayoutShapeContractError> {
        Self::from_parts_with_shapes_and_spans(
            bindings,
            shapes,
            IndexMap::new(),
            y_scalars,
            p_scalars,
        )
    }

    /// Build a layout from parts that already carry their indexed element
    /// slots, for a shape whose elements do not follow from a base slot.
    ///
    /// A constant array is that case: its elements hold distinct values rather
    /// than occupying a run, so nothing derives them. No production path builds
    /// a layout this way, so this exists only for the tests that hold the shape
    /// contract to that case.
    #[cfg(test)]
    fn from_parts_with_shapes_and_indexed_bindings(
        bindings: IndexMap<String, ScalarSlot>,
        shapes: IndexMap<String, Vec<usize>>,
        indexed_bindings: IndexMap<ComponentReferenceKey, Vec<IndexedScalarSlot>>,
        y_scalars: usize,
        p_scalars: usize,
    ) -> Result<Self, VarLayoutShapeContractError> {
        Self::checked(
            intern_key_map(bindings),
            intern_key_map(shapes),
            IndexMap::new(),
            indexed_bindings,
            y_scalars,
            p_scalars,
        )
    }

    /// Name each shaped variable's indexed-binding key, hold the parts to the
    /// shape contract, and construct.
    ///
    /// Every constructor ends here, so the contract is checked on the way in
    /// however the indexed bindings were arrived at.
    fn checked(
        bindings: IndexMap<VarName, ScalarSlot>,
        shapes: IndexMap<VarName, Vec<usize>>,
        shape_spans: IndexMap<VarName, Span>,
        indexed_bindings: IndexMap<ComponentReferenceKey, Vec<IndexedScalarSlot>>,
        y_scalars: usize,
        p_scalars: usize,
    ) -> Result<Self, VarLayoutShapeContractError> {
        let shape_indexed_keys = generated_shape_indexed_keys(&shapes, &indexed_bindings);
        validate_shape_contract(
            &bindings,
            &shapes,
            &shape_spans,
            &shape_indexed_keys,
            &indexed_bindings,
            y_scalars,
            p_scalars,
        )?;
        Ok(Self {
            bindings,
            shapes,
            shape_spans,
            shape_indexed_keys,
            indexed_bindings,
            y_scalars,
            p_scalars,
        })
    }

    pub fn bindings(&self) -> &IndexMap<VarName, ScalarSlot> {
        &self.bindings
    }

    pub fn indexed_bindings(&self) -> &IndexMap<ComponentReferenceKey, Vec<IndexedScalarSlot>> {
        &self.indexed_bindings
    }

    pub fn binding(&self, name: &str) -> Option<ScalarSlot> {
        let name = VarName::intern(name);
        if let Some(slot) = self.bindings.get(&name).copied() {
            return Some(slot);
        }
        // Fall back to deriving an array element's slot from its base slot + shape.
        // Array Y/P slots are contiguous in row-major order, so `x[i,j]` lives at
        // `base + flat_index(i,j)` — no need to materialize a `bindings` entry per
        // element. (Constant arrays hold distinct per-element values and are not
        // derivable this way; their elements stay in `bindings`.)
        self.derive_array_element_binding(name.as_str())
    }

    /// Derive an array element's `ScalarSlot` from its base slot + shape, for a
    /// concrete element name like `x[2,3]` (1-based, row-major). Returns `None` when
    /// the name is not a subscripted element, the base has no contiguous slot, or any
    /// subscript is out of bounds.
    fn derive_array_element_binding(&self, name: &str) -> Option<ScalarSlot> {
        let scalar = rumoca_core::parse_scalar_name(name)?;
        let base = VarName::intern(scalar.base);
        let root = self.bindings.get(&base).copied()?;
        let dims = self.shapes.get(&base)?;
        let flat = row_major_flat_index(&scalar.indices, dims)?;
        offset_contiguous_slot(root, flat)
    }

    pub fn shape(&self, name: &str) -> Option<&[usize]> {
        self.shapes.get(&VarName::intern(name)).map(Vec::as_slice)
    }

    pub fn shape_span(&self, name: &str) -> Option<Span> {
        self.shape_spans.get(&VarName::intern(name)).copied()
    }

    pub fn y_scalars(&self) -> usize {
        self.y_scalars
    }

    pub fn p_scalars(&self) -> usize {
        self.p_scalars
    }

    pub fn validate_shape_contract(&self) -> Result<(), VarLayoutShapeContractError> {
        validate_shape_contract(
            &self.bindings,
            &self.shapes,
            &self.shape_spans,
            &self.shape_indexed_keys,
            &self.indexed_bindings,
            self.y_scalars,
            self.p_scalars,
        )
    }
}

fn validate_shape_contract(
    bindings: &IndexMap<VarName, ScalarSlot>,
    shapes: &IndexMap<VarName, Vec<usize>>,
    shape_spans: &IndexMap<VarName, Span>,
    shape_indexed_keys: &IndexMap<VarName, ComponentReferenceKey>,
    indexed_bindings: &IndexMap<ComponentReferenceKey, Vec<IndexedScalarSlot>>,
    y_scalars: usize,
    p_scalars: usize,
) -> Result<(), VarLayoutShapeContractError> {
    validate_shape_span_metadata(shapes, shape_spans)?;
    for (name, shape) in shapes {
        let span = shape_span(name, shape_spans)?;
        if shape.is_empty() {
            return Err(VarLayoutShapeContractError::EmptyShape {
                variable: name.to_string(),
                span,
            });
        }
        let count = shape_scalar_count(name, shape, span)?;
        if count == 0 {
            validate_zero_size_shape(name, bindings, shape_indexed_keys, indexed_bindings, span)?;
            continue;
        }
        let Some(slot) = bindings.get(name).copied() else {
            return Err(VarLayoutShapeContractError::ShapeWithoutBinding {
                variable: name.to_string(),
                span,
            });
        };
        let (start, available) = match slot {
            ScalarSlot::Y { index } => (index, y_scalars),
            ScalarSlot::P { index } => (index, p_scalars),
            ScalarSlot::Constant(_) => {
                validate_indexed_constant_shape(
                    name,
                    count,
                    shape_indexed_keys,
                    indexed_bindings,
                    span,
                )?;
                continue;
            }
            ScalarSlot::Time => {
                return Err(VarLayoutShapeContractError::ShapeWithoutBinding {
                    variable: name.to_string(),
                    span,
                });
            }
        };
        if slot_range_end(start, count).is_none_or(|end| end > available) {
            return Err(VarLayoutShapeContractError::ShapeOutOfBounds {
                variable: name.to_string(),
                start,
                count,
                available,
                span,
            });
        }
    }
    Ok(())
}

fn validate_shape_span_metadata(
    shapes: &IndexMap<VarName, Vec<usize>>,
    shape_spans: &IndexMap<VarName, Span>,
) -> Result<(), VarLayoutShapeContractError> {
    if shape_spans.is_empty() {
        return Ok(());
    }
    for (name, span) in shape_spans {
        if !shapes.contains_key(name) {
            return Err(VarLayoutShapeContractError::ShapeSpanWithoutShape {
                variable: name.to_string(),
                span: *span,
            });
        }
    }
    for name in shapes.keys() {
        if !shape_spans.contains_key(name) {
            return Err(VarLayoutShapeContractError::ShapeSpanMissing {
                variable: name.to_string(),
            });
        }
    }
    Ok(())
}

fn shape_span(
    name: &VarName,
    shape_spans: &IndexMap<VarName, Span>,
) -> Result<Option<Span>, VarLayoutShapeContractError> {
    if shape_spans.is_empty() {
        return Ok(None);
    }
    shape_spans.get(name).copied().map(Some).ok_or_else(|| {
        VarLayoutShapeContractError::ShapeSpanMissing {
            variable: name.to_string(),
        }
    })
}

fn shape_scalar_count(
    name: &VarName,
    shape: &[usize],
    span: Option<Span>,
) -> Result<usize, VarLayoutShapeContractError> {
    shape
        .iter()
        .try_fold(1usize, |count, dim| count.checked_mul(*dim))
        .ok_or_else(|| VarLayoutShapeContractError::ShapeSizeOverflow {
            variable: name.to_string(),
            span,
        })
}

fn slot_range_end(start: usize, count: usize) -> Option<usize> {
    start.checked_add(count)
}

fn validate_zero_size_shape(
    name: &VarName,
    bindings: &IndexMap<VarName, ScalarSlot>,
    shape_indexed_keys: &IndexMap<VarName, ComponentReferenceKey>,
    indexed_bindings: &IndexMap<ComponentReferenceKey, Vec<IndexedScalarSlot>>,
    span: Option<Span>,
) -> Result<(), VarLayoutShapeContractError> {
    if bindings.contains_key(name) {
        return Err(VarLayoutShapeContractError::ShapeOutOfBounds {
            variable: name.to_string(),
            start: 0,
            count: 0,
            available: bindings.len(),
            span,
        });
    }
    let indexed_entries = shape_indexed_keys
        .get(name)
        .and_then(|key| indexed_bindings.get(key));
    if indexed_entries.is_some_and(|entries| !entries.is_empty()) {
        return Err(VarLayoutShapeContractError::ShapeOutOfBounds {
            variable: name.to_string(),
            start: 0,
            count: 0,
            available: indexed_entries.map_or(0, Vec::len),
            span,
        });
    }
    Ok(())
}

fn validate_indexed_constant_shape(
    name: &VarName,
    count: usize,
    shape_indexed_keys: &IndexMap<VarName, ComponentReferenceKey>,
    indexed_bindings: &IndexMap<ComponentReferenceKey, Vec<IndexedScalarSlot>>,
    span: Option<Span>,
) -> Result<(), VarLayoutShapeContractError> {
    let entries = shape_indexed_keys
        .get(name)
        .and_then(|key| indexed_bindings.get(key));
    let Some(entries) = entries else {
        return Err(VarLayoutShapeContractError::ShapeWithoutBinding {
            variable: name.to_string(),
            span,
        });
    };
    if entries.len() != count {
        return Err(VarLayoutShapeContractError::ShapeOutOfBounds {
            variable: name.to_string(),
            start: 0,
            count,
            available: entries.len(),
            span,
        });
    }
    Ok(())
}

fn generated_shape_indexed_keys(
    shapes: &IndexMap<VarName, Vec<usize>>,
    indexed_bindings: &IndexMap<ComponentReferenceKey, Vec<IndexedScalarSlot>>,
) -> IndexMap<VarName, ComponentReferenceKey> {
    shapes
        .keys()
        .filter_map(|name| {
            let key = ComponentReferenceKey::generated(name.as_str());
            indexed_bindings
                .contains_key(&key)
                .then(|| (name.clone(), key))
        })
        .collect()
}

fn indexed_bindings_from_shapes(
    bindings: &IndexMap<VarName, ScalarSlot>,
    shapes: &IndexMap<VarName, Vec<usize>>,
    shape_spans: &IndexMap<VarName, Span>,
    y_scalars: usize,
    p_scalars: usize,
) -> Result<IndexMap<ComponentReferenceKey, Vec<IndexedScalarSlot>>, VarLayoutShapeContractError> {
    let mut indexed_bindings =
        indexed_binding_map_with_capacity(shapes.len(), "indexed binding map")?;
    for (name, shape) in shapes {
        let span = shape_span(name, shape_spans)?;
        let Some(slot) = bindings.get(name).copied() else {
            continue;
        };
        let Some((start, available)) = slot_start_and_available(slot, y_scalars, p_scalars) else {
            continue;
        };
        let count = shape_scalar_count(name, shape, span)?;
        if slot_range_end(start, count).is_none_or(|end| end > available) {
            return Err(VarLayoutShapeContractError::ShapeOutOfBounds {
                variable: name.to_string(),
                start,
                count,
                available,
                span,
            });
        }
        let mut entries = shape_vec_with_capacity(count, name.as_str(), span)?;
        for flat_index in 0..count {
            let Some(indices) = flat_index_to_subscripts(name.as_str(), shape, flat_index, span)?
            else {
                continue;
            };
            let offset = start.checked_add(flat_index).ok_or_else(|| {
                VarLayoutShapeContractError::ShapeOutOfBounds {
                    variable: name.to_string(),
                    start,
                    count,
                    available,
                    span,
                }
            })?;
            let Some(coordinate) = slot.storage_coordinate() else {
                continue;
            };
            let slot = SolveStorageCoordinate::new(coordinate.column(), offset).scalar_slot();
            entries.push(IndexedScalarSlot { indices, slot });
        }
        if !entries.is_empty() {
            indexed_bindings.insert(ComponentReferenceKey::generated(name.as_str()), entries);
        }
    }
    Ok(indexed_bindings)
}

fn indexed_binding_map_with_capacity<K, V>(
    capacity: usize,
    variable: &str,
) -> Result<IndexMap<K, V>, VarLayoutShapeContractError>
where
    K: std::hash::Hash + Eq,
{
    let mut values = IndexMap::new();
    values
        .try_reserve(capacity)
        .map_err(|_| VarLayoutShapeContractError::ShapeSizeOverflow {
            variable: variable.to_string(),
            span: None,
        })?;
    Ok(values)
}

fn shape_vec_with_capacity<T>(
    capacity: usize,
    variable: &str,
    span: Option<Span>,
) -> Result<Vec<T>, VarLayoutShapeContractError> {
    let mut values = Vec::new();
    values.try_reserve_exact(capacity).map_err(|_| {
        VarLayoutShapeContractError::ShapeSizeOverflow {
            variable: variable.to_string(),
            span,
        }
    })?;
    Ok(values)
}

fn slot_start_and_available(
    slot: ScalarSlot,
    y_scalars: usize,
    p_scalars: usize,
) -> Option<(usize, usize)> {
    match slot {
        ScalarSlot::Y { index } => Some((index, y_scalars)),
        ScalarSlot::P { index } => Some((index, p_scalars)),
        ScalarSlot::Time | ScalarSlot::Constant(_) => None,
    }
}

fn flat_index_to_subscripts(
    name: &str,
    shape: &[usize],
    flat_index: usize,
    span: Option<Span>,
) -> Result<Option<Vec<usize>>, VarLayoutShapeContractError> {
    if shape.is_empty() {
        return Ok(None);
    }
    let mut remainder = flat_index;
    let mut subscripts = shape_vec_with_capacity(shape.len(), name, span)?;
    for dim in shape.iter().rev().copied() {
        if dim == 0 {
            return Ok(None);
        }
        subscripts.push((remainder % dim) + 1);
        remainder /= dim;
    }
    if remainder != 0 {
        return Ok(None);
    }
    subscripts.reverse();
    Ok(Some(subscripts))
}

/// Row-major flat offset of 1-based `indices` within `dims`, or `None` if the count
/// mismatches or any index is out of `1..=dim`.
fn row_major_flat_index(indices: &[i64], dims: &[usize]) -> Option<usize> {
    if indices.len() != dims.len() || dims.is_empty() {
        return None;
    }
    let mut flat = 0usize;
    for (&index, &dim) in indices.iter().zip(dims) {
        let index = usize::try_from(index).ok()?;
        if index < 1 || index > dim {
            return None;
        }
        flat = flat.checked_mul(dim)?.checked_add(index - 1)?;
    }
    Some(flat)
}

/// Offset a contiguous (`Y`/`P`) base slot by `flat` elements. `None` for `Time` or
/// `Constant`, whose array elements are not a contiguous offset of the base.
fn offset_contiguous_slot(root: ScalarSlot, flat: usize) -> Option<ScalarSlot> {
    match root {
        ScalarSlot::Y { index } => Some(scalar_slot_y(index.checked_add(flat)?)),
        ScalarSlot::P { index } => Some(scalar_slot_p(index.checked_add(flat)?)),
        ScalarSlot::Time | ScalarSlot::Constant(_) => None,
    }
}

pub const fn scalar_slot_y(index: usize) -> ScalarSlot {
    ScalarSlot::Y { index }
}

pub const fn scalar_slot_p(index: usize) -> ScalarSlot {
    ScalarSlot::P { index }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn logical_storage_coordinates_preserve_column_and_index() {
        for index in [0, 1, usize::MAX] {
            for (column, coordinate, operand) in [
                (
                    SolveStorageColumn::Y,
                    SolveStorageCoordinate::Y(index),
                    ScalarSlot::Y { index },
                ),
                (
                    SolveStorageColumn::P,
                    SolveStorageCoordinate::P(index),
                    ScalarSlot::P { index },
                ),
            ] {
                assert_eq!(SolveStorageCoordinate::new(column, index), coordinate);
                assert_eq!(coordinate.column(), column);
                assert_eq!(coordinate.index(), index);
                assert_eq!(coordinate.scalar_slot(), operand);
                assert_eq!(operand.storage_coordinate(), Some(coordinate));
            }
            assert_ne!(
                SolveStorageCoordinate::Y(index),
                SolveStorageCoordinate::P(index)
            );
        }
        assert_eq!(ScalarSlot::Time.storage_coordinate(), None);
        assert_eq!(ScalarSlot::Constant(0.0).storage_coordinate(), None);
    }

    #[test]
    fn scalar_slot_wire_rejects_removed_byte_offset() {
        for column in ["Y", "P"] {
            for byte_offset in [0, 16] {
                let wire =
                    serde_json::json!({ (column): { "index": 0, "byte_offset": byte_offset } });
                let error = serde_json::from_value::<ScalarSlot>(wire)
                    .expect_err("an obsolete offset must not be silently ignored");
                assert!(error.to_string().contains("byte_offset"), "{error}");
            }
        }
    }

    #[test]
    fn storage_coordinate_wire_cannot_construct_nonstorage_operands() {
        for wire in [
            serde_json::json!("Time"),
            serde_json::json!({ "Constant": 1.0 }),
        ] {
            assert!(serde_json::from_value::<SolveStorageCoordinate>(wire).is_err());
        }
    }

    fn layout_source_span(source: u64, start: usize, end: usize) -> Span {
        let source_name = format!("ir_solve_layout_source_{source}.mo");
        Span::from_offsets(
            rumoca_core::SourceId::from_source_name(&source_name),
            start,
            end,
        )
    }

    #[test]
    fn binding_derives_array_element_slots_from_base_and_shape() {
        // Only the base slot + shape are stored; element slots are derived.
        let bindings = IndexMap::from([
            ("u".to_string(), scalar_slot_y(0)),
            ("p".to_string(), scalar_slot_p(10)),
        ]);
        let shapes = IndexMap::from([
            ("u".to_string(), vec![2usize, 3]),
            ("p".to_string(), vec![4usize]),
        ]);
        let layout = VarLayout {
            bindings: intern_key_map(bindings),
            shapes: intern_key_map(shapes),
            shape_spans: IndexMap::new(),
            shape_indexed_keys: IndexMap::new(),
            indexed_bindings: IndexMap::new(),
            y_scalars: 6,
            p_scalars: 14,
        };

        // Row-major 2x3: u[i,j] -> base + (i-1)*3 + (j-1).
        assert_eq!(layout.binding("u[1,1]"), Some(scalar_slot_y(0)));
        assert_eq!(layout.binding("u[1,3]"), Some(scalar_slot_y(2)));
        assert_eq!(layout.binding("u[2,1]"), Some(scalar_slot_y(3)));
        assert_eq!(layout.binding("u[2,3]"), Some(scalar_slot_y(5)));
        // 1-D parameter array p[k] -> base + (k-1).
        assert_eq!(layout.binding("p[1]"), Some(scalar_slot_p(10)));
        assert_eq!(layout.binding("p[4]"), Some(scalar_slot_p(13)));
        // The whole-array base still resolves directly.
        assert_eq!(layout.binding("u"), Some(scalar_slot_y(0)));
        // Out-of-bounds and shape-mismatched subscripts derive nothing.
        assert_eq!(layout.binding("u[3,1]"), None);
        assert_eq!(layout.binding("u[1]"), None);
        assert_eq!(layout.binding("u[0,1]"), None);
    }

    /// Source-reference identities are mandatory before Solve key construction.
    /// The member path hashes interned ids while serialization retains the
    /// readable spelling.
    #[test]
    fn component_reference_key_parts_are_interned_and_serialize_as_text() {
        let span = layout_source_span(23, 0, 5);
        let part = |ident: &str, def_id: u32| rumoca_core::ComponentRefPart {
            ident: ident.to_string(),
            span,
            subs: Vec::new(),
            def_id: rumoca_core::DefId::new(def_id),
        };
        let reference =
            ComponentReference::construct(false, span, vec![part("gear", 9), part("ratio", 10)])
                .expect("test reference carries an identity on every segment");

        let key = ComponentReferenceKey::from_component_reference(&reference)
            .expect("resolved source reference builds a key");
        let ComponentReferenceKey::Source { parts, .. } = &key else {
            panic!("a resolved source reference must not become a generated key");
        };
        assert_eq!(parts[0].ident, VarName::intern("gear"));
        assert_eq!(parts[1].ident, VarName::intern("ratio"));

        let json = serde_json::to_string(&key).expect("key serializes");
        assert!(
            json.contains("\"gear\"") && json.contains("\"ratio\""),
            "member names must still cross the process boundary as text: {json}"
        );
        assert_eq!(
            serde_json::from_str::<ComponentReferenceKey>(&json).expect("key round-trips"),
            key
        );
    }

    #[test]
    fn component_reference_key_error_displays_dynamic_subscript() {
        let span = layout_source_span(22, 12, 20);
        let reference = ComponentReference::construct(
            false,
            span,
            vec![rumoca_core::ComponentRefPart {
                ident: "x".to_string(),
                span,
                subs: vec![Subscript::expr(
                    Box::new(rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Real(1.0),
                        span,
                    }),
                    span,
                )],
                def_id: rumoca_core::DefId::new(7),
            }],
        )
        .expect("test reference carries an exact declaration identity");

        let err = ComponentReferenceKey::from_component_reference(&reference)
            .expect_err("dynamic component reference subscript should fail");

        assert_eq!(err.span, span);
        assert_eq!(
            err.to_string(),
            "component reference contains a dynamic subscript"
        );
    }

    #[test]
    fn layout_shape_contract_accepts_bound_array_shape() {
        let bindings = IndexMap::from([("x".to_string(), scalar_slot_y(0))]);
        let shapes = IndexMap::from([("x".to_string(), vec![2, 3])]);
        let layout = VarLayout::from_parts_with_shapes(bindings, shapes, 6, 0)
            .expect("shape matches y scalar extent");

        assert_eq!(layout.validate_shape_contract(), Ok(()));
        assert_eq!(
            layout.indexed_bindings()[&ComponentReferenceKey::generated("x")].len(),
            6
        );
    }

    #[test]
    fn layout_shape_contract_rejects_shape_without_binding() {
        let bindings = IndexMap::new();
        let shapes = IndexMap::from([("x".to_string(), vec![2])]);

        let err = VarLayout::from_parts_with_shapes(bindings, shapes, 0, 0)
            .expect_err("shape without scalar binding should fail");
        assert!(matches!(
            err,
            VarLayoutShapeContractError::ShapeWithoutBinding { ref variable, span } if variable == "x" && span.is_none()
        ));
        assert_eq!(err.source_span(), None);
        assert_eq!(
            err.to_string(),
            "shape metadata for `x` has no scalar binding"
        );
    }

    #[test]
    fn layout_shape_contract_accepts_zero_size_array_shape_without_scalar_binding() {
        let bindings = IndexMap::new();
        let shapes = IndexMap::from([("x".to_string(), vec![0])]);
        let layout = VarLayout::from_parts_with_shapes(bindings, shapes, 0, 0)
            .expect("zero-size arrays carry shape metadata but no scalar slots");

        assert_eq!(layout.shape("x"), Some([0].as_slice()));
        assert_eq!(layout.binding("x"), None);
        assert!(
            !layout
                .indexed_bindings()
                .contains_key(&ComponentReferenceKey::generated("x"))
        );
    }

    #[test]
    fn layout_shape_contract_rejects_zero_size_array_with_scalar_binding() {
        let bindings = IndexMap::from([("x".to_string(), scalar_slot_y(0))]);
        let shapes = IndexMap::from([("x".to_string(), vec![0])]);

        assert!(matches!(
            VarLayout::from_parts_with_shapes(bindings, shapes, 1, 0),
            Err(VarLayoutShapeContractError::ShapeOutOfBounds {
                variable,
                count: 0,
                span,
                ..
            }) if variable == "x" && span.is_none()
        ));
    }

    #[test]
    fn layout_shape_contract_rejects_out_of_bounds_shape() {
        let bindings = IndexMap::from([("x".to_string(), scalar_slot_y(1))]);
        let shapes = IndexMap::from([("x".to_string(), vec![3])]);

        assert!(matches!(
            VarLayout::from_parts_with_shapes(bindings, shapes, 3, 0),
            Err(VarLayoutShapeContractError::ShapeOutOfBounds {
                variable,
                start: 1,
                count: 3,
                available: 3,
                span,
            }) if variable == "x" && span.is_none()
        ));
    }

    #[test]
    fn layout_shape_contract_rejects_shape_size_overflow_with_span() {
        let span = layout_source_span(8, 2, 6);
        let bindings = IndexMap::from([("x".to_string(), scalar_slot_y(0))]);
        let shapes = IndexMap::from([("x".to_string(), vec![usize::MAX, 2])]);
        let shape_spans = IndexMap::from([("x".to_string(), span)]);

        let err = VarLayout::from_parts_with_shapes_and_spans(bindings, shapes, shape_spans, 0, 0)
            .expect_err("shape scalar count overflow should fail");

        assert_eq!(err.source_span(), Some(span));
        assert!(matches!(
            err,
            VarLayoutShapeContractError::ShapeSizeOverflow { variable, .. } if variable == "x"
        ));
    }

    #[test]
    fn layout_shape_contract_rejects_slot_range_overflow_before_indexing() {
        let span = layout_source_span(9, 4, 11);
        let start = usize::MAX;
        let bindings = IndexMap::from([("x".to_string(), ScalarSlot::Y { index: start })]);
        let shapes = IndexMap::from([("x".to_string(), vec![2])]);
        let shape_spans = IndexMap::from([("x".to_string(), span)]);

        let err = VarLayout::from_parts_with_shapes_and_spans(bindings, shapes, shape_spans, 0, 0)
            .expect_err("slot range overflow should fail before indexed binding generation");

        assert_eq!(err.source_span(), Some(span));
        assert!(matches!(
            err,
            VarLayoutShapeContractError::ShapeOutOfBounds {
                variable,
                start: actual_start,
                count: 2,
                ..
            } if variable == "x" && actual_start == start
        ));
    }

    #[test]
    fn layout_shape_contract_uses_logical_index_bounds_not_byte_width() {
        let span = layout_source_span(10, 5, 12);
        let start = usize::MAX - 1;
        let bindings = IndexMap::from([("x".to_string(), ScalarSlot::Y { index: start })]);
        let shapes = IndexMap::from([("x".to_string(), vec![1])]);
        let shape_spans = IndexMap::from([("x".to_string(), span)]);

        let layout = VarLayout::from_parts_with_shapes_and_spans(
            bindings,
            shapes,
            shape_spans,
            usize::MAX,
            0,
        )
        .expect("one logical coordinate fits without allocating its column");

        assert_eq!(layout.binding("x[1]"), Some(scalar_slot_y(start)));
        assert_eq!(layout.shape_span("x"), Some(span));
    }

    #[test]
    fn layout_shape_contract_reports_source_span() {
        let span = layout_source_span(4, 8, 13);
        let bindings = IndexMap::new();
        let shapes = IndexMap::from([("x".to_string(), vec![2])]);
        let shape_spans = IndexMap::from([("x".to_string(), span)]);

        assert!(matches!(
            VarLayout::from_parts_with_shapes_and_spans(bindings, shapes, shape_spans, 0, 0),
            Err(VarLayoutShapeContractError::ShapeWithoutBinding { variable, span: actual })
                if variable == "x" && actual == Some(span)
        ));
    }

    #[test]
    fn layout_shape_contract_rejects_partial_shape_span_metadata() {
        let bindings = IndexMap::from([
            ("x".to_string(), scalar_slot_y(0)),
            ("y".to_string(), scalar_slot_y(1)),
        ]);
        let shapes = IndexMap::from([("x".to_string(), vec![1]), ("y".to_string(), vec![1])]);
        let shape_spans = IndexMap::from([("x".to_string(), layout_source_span(11, 1, 2))]);

        let err = VarLayout::from_parts_with_shapes_and_spans(bindings, shapes, shape_spans, 2, 0)
            .expect_err("explicit shape spans must cover every shape");

        assert!(matches!(
            err,
            VarLayoutShapeContractError::ShapeSpanMissing { variable } if variable == "y"
        ));
    }

    #[test]
    fn layout_shape_contract_rejects_stale_shape_span_metadata() {
        let span = layout_source_span(12, 4, 8);
        let bindings = IndexMap::from([("x".to_string(), scalar_slot_y(0))]);
        let shapes = IndexMap::from([("x".to_string(), vec![1])]);
        let shape_spans = IndexMap::from([("x".to_string(), span), ("stale".to_string(), span)]);

        let err = VarLayout::from_parts_with_shapes_and_spans(bindings, shapes, shape_spans, 1, 0)
            .expect_err("explicit shape spans must not contain stale entries");

        assert!(matches!(
            err,
            VarLayoutShapeContractError::ShapeSpanWithoutShape {
                variable,
                span: actual,
            } if variable == "stale" && actual == span
        ));
    }

    #[test]
    fn layout_shape_contract_accepts_indexed_constant_array_shape() {
        let bindings = IndexMap::from([("table".to_string(), ScalarSlot::Constant(1.0))]);
        let shapes = IndexMap::from([("table".to_string(), vec![2])]);
        let indexed_bindings = IndexMap::from([(
            ComponentReferenceKey::generated("table"),
            vec![
                IndexedScalarSlot {
                    indices: vec![1],
                    slot: ScalarSlot::Constant(1.0),
                },
                IndexedScalarSlot {
                    indices: vec![2],
                    slot: ScalarSlot::Constant(2.0),
                },
            ],
        )]);

        let layout = VarLayout::from_parts_with_shapes_and_indexed_bindings(
            bindings,
            shapes,
            indexed_bindings,
            0,
            0,
        )
        .expect("constant array shape is represented by indexed constant slots");

        assert_eq!(layout.validate_shape_contract(), Ok(()));
    }
}
