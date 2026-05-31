use indexmap::IndexMap;
use rumoca_core::{ComponentPath, Span};
use serde::ser::SerializeStruct;
use serde::{Deserialize, Serialize};

const F64_BYTES: usize = std::mem::size_of::<f64>();

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum ScalarSlot {
    Time,
    Y { index: usize, byte_offset: usize },
    P { index: usize, byte_offset: usize },
    Constant(f64),
}

#[derive(Debug, Clone, Default, Deserialize)]
pub struct VarLayout {
    bindings: IndexMap<String, ScalarSlot>,
    #[serde(default)]
    shapes: IndexMap<String, Vec<usize>>,
    #[serde(default, skip_serializing_if = "IndexMap::is_empty")]
    shape_spans: IndexMap<String, Span>,
    #[serde(skip)]
    indexed_bindings: IndexMap<ComponentPath, Vec<IndexedScalarSlot>>,
    y_scalars: usize,
    p_scalars: usize,
}

impl Serialize for VarLayout {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let include_shape_spans = !serializer.is_human_readable() || !self.shape_spans.is_empty();
        let field_count = if include_shape_spans { 5 } else { 4 };
        let mut state = serializer.serialize_struct("VarLayout", field_count)?;
        state.serialize_field("bindings", &self.bindings)?;
        state.serialize_field("shapes", &self.shapes)?;
        if include_shape_spans {
            state.serialize_field("shape_spans", &self.shape_spans)?;
        }
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
        span: Span,
    },
    EmptyShape {
        variable: String,
        span: Span,
    },
    ZeroDimension {
        variable: String,
        span: Span,
    },
    ShapeOutOfBounds {
        variable: String,
        start: usize,
        count: usize,
        available: usize,
        span: Span,
    },
}

impl VarLayoutShapeContractError {
    pub fn span(&self) -> Span {
        match self {
            Self::ShapeWithoutBinding { span, .. }
            | Self::EmptyShape { span, .. }
            | Self::ZeroDimension { span, .. }
            | Self::ShapeOutOfBounds { span, .. } => *span,
        }
    }
}

impl VarLayout {
    pub fn from_parts(
        bindings: IndexMap<String, ScalarSlot>,
        y_scalars: usize,
        p_scalars: usize,
    ) -> Self {
        Self {
            bindings,
            shapes: IndexMap::new(),
            shape_spans: IndexMap::new(),
            indexed_bindings: IndexMap::new(),
            y_scalars,
            p_scalars,
        }
    }

    pub fn from_parts_with_shapes(
        bindings: IndexMap<String, ScalarSlot>,
        shapes: IndexMap<String, Vec<usize>>,
        y_scalars: usize,
        p_scalars: usize,
    ) -> Self {
        Self::try_from_parts_with_shapes(bindings, shapes, y_scalars, p_scalars)
            .expect("solve VarLayout shape metadata must match scalar bindings")
    }

    pub fn try_from_parts_with_shapes(
        bindings: IndexMap<String, ScalarSlot>,
        shapes: IndexMap<String, Vec<usize>>,
        y_scalars: usize,
        p_scalars: usize,
    ) -> Result<Self, VarLayoutShapeContractError> {
        Self::try_from_parts_with_shapes_and_spans(
            bindings,
            shapes,
            IndexMap::new(),
            y_scalars,
            p_scalars,
        )
    }

    pub fn from_parts_with_shapes_and_spans(
        bindings: IndexMap<String, ScalarSlot>,
        shapes: IndexMap<String, Vec<usize>>,
        shape_spans: IndexMap<String, Span>,
        y_scalars: usize,
        p_scalars: usize,
    ) -> Self {
        Self::try_from_parts_with_shapes_and_spans(
            bindings,
            shapes,
            shape_spans,
            y_scalars,
            p_scalars,
        )
        .expect("solve VarLayout shape metadata must match scalar bindings")
    }

    pub fn try_from_parts_with_shapes_and_spans(
        bindings: IndexMap<String, ScalarSlot>,
        shapes: IndexMap<String, Vec<usize>>,
        shape_spans: IndexMap<String, Span>,
        y_scalars: usize,
        p_scalars: usize,
    ) -> Result<Self, VarLayoutShapeContractError> {
        let indexed_bindings = indexed_bindings_from_shapes(&bindings, &shapes);
        validate_shape_contract(
            &bindings,
            &shapes,
            &shape_spans,
            &indexed_bindings,
            y_scalars,
            p_scalars,
        )?;
        Ok(Self {
            indexed_bindings,
            bindings,
            shapes,
            shape_spans,
            y_scalars,
            p_scalars,
        })
    }

    pub fn from_parts_with_shapes_and_indexed_bindings(
        bindings: IndexMap<String, ScalarSlot>,
        shapes: IndexMap<String, Vec<usize>>,
        indexed_bindings: IndexMap<ComponentPath, Vec<IndexedScalarSlot>>,
        y_scalars: usize,
        p_scalars: usize,
    ) -> Self {
        Self::try_from_parts_with_shapes_spans_and_indexed_bindings(
            bindings,
            shapes,
            IndexMap::new(),
            indexed_bindings,
            y_scalars,
            p_scalars,
        )
        .expect("solve VarLayout shape metadata must match scalar bindings")
    }

    pub fn try_from_parts_with_shapes_and_indexed_bindings(
        bindings: IndexMap<String, ScalarSlot>,
        shapes: IndexMap<String, Vec<usize>>,
        indexed_bindings: IndexMap<ComponentPath, Vec<IndexedScalarSlot>>,
        y_scalars: usize,
        p_scalars: usize,
    ) -> Result<Self, VarLayoutShapeContractError> {
        Self::try_from_parts_with_shapes_spans_and_indexed_bindings(
            bindings,
            shapes,
            IndexMap::new(),
            indexed_bindings,
            y_scalars,
            p_scalars,
        )
    }

    pub fn from_parts_with_shapes_spans_and_indexed_bindings(
        bindings: IndexMap<String, ScalarSlot>,
        shapes: IndexMap<String, Vec<usize>>,
        shape_spans: IndexMap<String, Span>,
        indexed_bindings: IndexMap<ComponentPath, Vec<IndexedScalarSlot>>,
        y_scalars: usize,
        p_scalars: usize,
    ) -> Self {
        Self::try_from_parts_with_shapes_spans_and_indexed_bindings(
            bindings,
            shapes,
            shape_spans,
            indexed_bindings,
            y_scalars,
            p_scalars,
        )
        .expect("solve VarLayout shape metadata must match scalar bindings")
    }

    pub fn try_from_parts_with_shapes_spans_and_indexed_bindings(
        bindings: IndexMap<String, ScalarSlot>,
        shapes: IndexMap<String, Vec<usize>>,
        shape_spans: IndexMap<String, Span>,
        indexed_bindings: IndexMap<ComponentPath, Vec<IndexedScalarSlot>>,
        y_scalars: usize,
        p_scalars: usize,
    ) -> Result<Self, VarLayoutShapeContractError> {
        validate_shape_contract(
            &bindings,
            &shapes,
            &shape_spans,
            &indexed_bindings,
            y_scalars,
            p_scalars,
        )?;
        Ok(Self {
            bindings,
            shapes,
            shape_spans,
            indexed_bindings,
            y_scalars,
            p_scalars,
        })
    }

    pub fn bindings(&self) -> &IndexMap<String, ScalarSlot> {
        &self.bindings
    }

    pub fn indexed_bindings(&self) -> &IndexMap<ComponentPath, Vec<IndexedScalarSlot>> {
        &self.indexed_bindings
    }

    pub fn binding(&self, name: &str) -> Option<ScalarSlot> {
        self.bindings.get(name).copied()
    }

    pub fn shape(&self, name: &str) -> Option<&[usize]> {
        self.shapes.get(name).map(Vec::as_slice)
    }

    pub fn shape_span(&self, name: &str) -> Option<Span> {
        self.shape_spans.get(name).copied()
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
            &self.indexed_bindings,
            self.y_scalars,
            self.p_scalars,
        )
    }
}

fn validate_shape_contract(
    bindings: &IndexMap<String, ScalarSlot>,
    shapes: &IndexMap<String, Vec<usize>>,
    shape_spans: &IndexMap<String, Span>,
    indexed_bindings: &IndexMap<ComponentPath, Vec<IndexedScalarSlot>>,
    y_scalars: usize,
    p_scalars: usize,
) -> Result<(), VarLayoutShapeContractError> {
    for (name, shape) in shapes {
        let span = shape_spans.get(name).copied().unwrap_or(Span::DUMMY);
        if shape.is_empty() {
            return Err(VarLayoutShapeContractError::EmptyShape {
                variable: name.clone(),
                span,
            });
        }
        if shape.contains(&0) {
            return Err(VarLayoutShapeContractError::ZeroDimension {
                variable: name.clone(),
                span,
            });
        }
        let Some(slot) = bindings.get(name).copied() else {
            return Err(VarLayoutShapeContractError::ShapeWithoutBinding {
                variable: name.clone(),
                span,
            });
        };
        let count = shape.iter().product::<usize>();
        let (start, available) = match slot {
            ScalarSlot::Y { index, .. } => (index, y_scalars),
            ScalarSlot::P { index, .. } => (index, p_scalars),
            ScalarSlot::Constant(_) => {
                validate_indexed_constant_shape(name, count, indexed_bindings, span)?;
                continue;
            }
            ScalarSlot::Time => {
                return Err(VarLayoutShapeContractError::ShapeWithoutBinding {
                    variable: name.clone(),
                    span,
                });
            }
        };
        if start.saturating_add(count) > available {
            return Err(VarLayoutShapeContractError::ShapeOutOfBounds {
                variable: name.clone(),
                start,
                count,
                available,
                span,
            });
        }
    }
    Ok(())
}

fn validate_indexed_constant_shape(
    name: &str,
    count: usize,
    indexed_bindings: &IndexMap<ComponentPath, Vec<IndexedScalarSlot>>,
    span: Span,
) -> Result<(), VarLayoutShapeContractError> {
    let key = ComponentPath::from_flat_path(name);
    let Some(entries) = indexed_bindings.get(&key) else {
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

fn indexed_bindings_from_shapes(
    bindings: &IndexMap<String, ScalarSlot>,
    shapes: &IndexMap<String, Vec<usize>>,
) -> IndexMap<ComponentPath, Vec<IndexedScalarSlot>> {
    let mut indexed_bindings = IndexMap::new();
    for (name, shape) in shapes {
        let Some(slot) = bindings.get(name).copied() else {
            continue;
        };
        let Some(start) = slot_start(slot) else {
            continue;
        };
        let count = shape.iter().product::<usize>();
        let entries = (0..count)
            .filter_map(|flat_index| {
                let indices = flat_index_to_subscripts(shape, flat_index)?;
                Some(IndexedScalarSlot {
                    indices,
                    slot: slot.with_index(start + flat_index)?,
                })
            })
            .collect::<Vec<_>>();
        if !entries.is_empty() {
            indexed_bindings.insert(ComponentPath::from_flat_path(name), entries);
        }
    }
    indexed_bindings
}

fn slot_start(slot: ScalarSlot) -> Option<usize> {
    match slot {
        ScalarSlot::Y { index, .. } | ScalarSlot::P { index, .. } => Some(index),
        ScalarSlot::Time | ScalarSlot::Constant(_) => None,
    }
}

fn flat_index_to_subscripts(shape: &[usize], flat_index: usize) -> Option<Vec<usize>> {
    if shape.is_empty() {
        return None;
    }
    let mut remainder = flat_index;
    let mut subscripts = Vec::with_capacity(shape.len());
    for dim in shape.iter().rev().copied() {
        if dim == 0 {
            return None;
        }
        subscripts.push((remainder % dim) + 1);
        remainder /= dim;
    }
    if remainder != 0 {
        return None;
    }
    subscripts.reverse();
    Some(subscripts)
}

impl ScalarSlot {
    fn with_index(self, index: usize) -> Option<Self> {
        match self {
            ScalarSlot::Y { .. } => Some(scalar_slot_y(index)),
            ScalarSlot::P { .. } => Some(scalar_slot_p(index)),
            ScalarSlot::Time | ScalarSlot::Constant(_) => None,
        }
    }
}

pub fn scalar_slot_y(index: usize) -> ScalarSlot {
    ScalarSlot::Y {
        index,
        byte_offset: index.saturating_mul(F64_BYTES),
    }
}

pub fn scalar_slot_p(index: usize) -> ScalarSlot {
    ScalarSlot::P {
        index,
        byte_offset: index.saturating_mul(F64_BYTES),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn layout_shape_contract_accepts_bound_array_shape() {
        let bindings = IndexMap::from([("x".to_string(), scalar_slot_y(0))]);
        let shapes = IndexMap::from([("x".to_string(), vec![2, 3])]);
        let layout = VarLayout::try_from_parts_with_shapes(bindings, shapes, 6, 0)
            .expect("shape matches y scalar extent");

        assert_eq!(layout.validate_shape_contract(), Ok(()));
        assert_eq!(
            layout.indexed_bindings()[&ComponentPath::from_flat_path("x")].len(),
            6
        );
    }

    #[test]
    fn layout_shape_contract_rejects_shape_without_binding() {
        let bindings = IndexMap::new();
        let shapes = IndexMap::from([("x".to_string(), vec![2])]);

        assert!(matches!(
            VarLayout::try_from_parts_with_shapes(bindings, shapes, 0, 0),
            Err(VarLayoutShapeContractError::ShapeWithoutBinding { variable, span }) if variable == "x" && span.is_dummy()
        ));
    }

    #[test]
    fn layout_shape_contract_rejects_out_of_bounds_shape() {
        let bindings = IndexMap::from([("x".to_string(), scalar_slot_y(1))]);
        let shapes = IndexMap::from([("x".to_string(), vec![3])]);

        assert!(matches!(
            VarLayout::try_from_parts_with_shapes(bindings, shapes, 3, 0),
            Err(VarLayoutShapeContractError::ShapeOutOfBounds {
                variable,
                start: 1,
                count: 3,
                available: 3,
                span,
            }) if variable == "x" && span.is_dummy()
        ));
    }

    #[test]
    fn layout_shape_contract_reports_source_span() {
        let span = Span::from_offsets(rumoca_core::SourceId(4), 8, 13);
        let bindings = IndexMap::new();
        let shapes = IndexMap::from([("x".to_string(), vec![2])]);
        let shape_spans = IndexMap::from([("x".to_string(), span)]);

        assert!(matches!(
            VarLayout::try_from_parts_with_shapes_and_spans(bindings, shapes, shape_spans, 0, 0),
            Err(VarLayoutShapeContractError::ShapeWithoutBinding { variable, span: actual })
                if variable == "x" && actual == span
        ));
    }

    #[test]
    fn layout_shape_contract_accepts_indexed_constant_array_shape() {
        let bindings = IndexMap::from([("table".to_string(), ScalarSlot::Constant(1.0))]);
        let shapes = IndexMap::from([("table".to_string(), vec![2])]);
        let indexed_bindings = IndexMap::from([(
            ComponentPath::from_flat_path("table"),
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

        let layout = VarLayout::try_from_parts_with_shapes_and_indexed_bindings(
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
