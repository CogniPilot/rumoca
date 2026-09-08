use super::*;

#[derive(Debug, Clone, Copy)]
pub enum Subscript<'dae> {
    Value {
        expression: ExprId<'dae>,
        provenance: DaeProvenance,
    },
    Index {
        expression: ExprId<'dae>,
        provenance: DaeProvenance,
    },
    Whole {
        provenance: DaeProvenance,
    },
    Slice {
        expression: ExprId<'dae>,
        provenance: DaeProvenance,
    },
}

impl Subscript<'_> {
    fn provenance(self) -> DaeProvenance {
        match self {
            Self::Value { provenance, .. }
            | Self::Index { provenance, .. }
            | Self::Whole { provenance }
            | Self::Slice { provenance, .. } => provenance,
        }
    }
}
impl<'dae> ExpressionAt<'_, 'dae> {
    pub fn index(
        self,
        base: ExprId<'dae>,
        subscripts: impl IntoIterator<Item = Subscript<'dae>>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let selection = pack_subscripts(
            self.source_map,
            self.storage,
            base,
            subscripts.into_iter().collect(),
            self.provenance,
        )?;
        let ty = self
            .storage
            .intern_type(selection.value_type, self.provenance)?;
        self.insert(
            ExprNode::Index {
                base: base.index(),
                subscripts: selection.range,
            },
            ty,
            selection.variability,
            selection.binder_domain,
        )
    }

    pub fn array_update(
        self,
        base: ExprId<'dae>,
        value: ExprId<'dae>,
        subscripts: impl IntoIterator<Item = Subscript<'dae>>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let base_type = self
            .storage
            .expressions
            .value_types
            .get(base.index() as usize)
            .copied()
            .ok_or_else(|| crate::model::unknown("expression", base.index(), self.provenance))?;
        let selection = pack_subscripts(
            self.source_map,
            self.storage,
            base,
            subscripts.into_iter().collect(),
            self.provenance,
        )?;
        let selected_type = self
            .storage
            .intern_type(selection.value_type, self.provenance)?;
        let value_type = definition_type(self.storage, value, self.provenance)?;
        self.storage.expect_value_type_compatible(
            selected_type.index(),
            value_type,
            self.provenance,
        )?;
        let variability = selection
            .variability
            .max(self.storage.expr_variability(value, self.provenance)?);
        let binder_domain = merge_binder_domain(
            self.storage,
            selection.binder_domain,
            self.storage.expr_binder_domain(value, self.provenance)?,
            self.provenance,
        )?;
        self.insert(
            ExprNode::ArrayUpdate {
                base: base.index(),
                value: value.index(),
                subscripts: selection.range,
            },
            ValueTypeId::from_raw(base_type),
            variability,
            binder_domain,
        )
    }
}

struct PackedSelection {
    value_type: ValueType,
    range: OperandRange,
    variability: ExpressionVariability,
    binder_domain: Option<u32>,
}

fn pack_subscripts(
    source_map: &rumoca_core::SourceMap,
    storage: &mut Storage,
    base: ExprId<'_>,
    subscripts: Vec<Subscript<'_>>,
    provenance: DaeProvenance,
) -> Result<PackedSelection, DaeConstructionError> {
    let base_type = storage.expr_type(base, provenance)?.clone();
    if subscripts.len() > base_type.dimensions().len() {
        return Err(DaeConstructionError::InvalidSubscript {
            span: provenance.span(),
        });
    }
    let start = storage.expressions.subscripts.len();
    let mut dimensions = Vec::new();
    let mut variability = storage.expr_variability(base, provenance)?;
    let mut binder_domain = storage.expr_binder_domain(base, provenance)?;
    for (axis, subscript) in subscripts.into_iter().enumerate() {
        let subscript_provenance = subscript.provenance();
        crate::model::check_provenance(source_map, subscript_provenance)?;
        let axis_extent = base_type.dimensions()[axis];
        let kind = match subscript {
            Subscript::Value { expression, .. } => {
                let ty = storage.expr_type(expression, provenance)?.clone();
                validate_subscript(storage, expression, ty.is_scalar(), provenance)?;
                validate_constructor_known_subscript_bounds(
                    storage,
                    expression,
                    axis_extent,
                    subscript_provenance,
                )?;
                variability = variability.max(storage.expr_variability(expression, provenance)?);
                binder_domain = merge_binder_domain(
                    storage,
                    binder_domain,
                    storage.expr_binder_domain(expression, provenance)?,
                    provenance,
                )?;
                packed_value_subscript(&ty, expression, &mut dimensions)
            }
            Subscript::Index { expression, .. } => {
                validate_subscript(storage, expression, true, provenance)?;
                validate_constructor_known_subscript_bounds(
                    storage,
                    expression,
                    axis_extent,
                    subscript_provenance,
                )?;
                variability = variability.max(storage.expr_variability(expression, provenance)?);
                binder_domain = merge_binder_domain(
                    storage,
                    binder_domain,
                    storage.expr_binder_domain(expression, provenance)?,
                    provenance,
                )?;
                PackedSubscriptKind::Index(expression.index())
            }
            Subscript::Whole { .. } => {
                dimensions.push(axis_extent);
                PackedSubscriptKind::Whole
            }
            Subscript::Slice { expression, .. } => {
                validate_subscript(storage, expression, false, provenance)?;
                validate_constructor_known_subscript_bounds(
                    storage,
                    expression,
                    axis_extent,
                    subscript_provenance,
                )?;
                dimensions
                    .extend_from_slice(storage.expr_type(expression, provenance)?.dimensions());
                variability = variability.max(storage.expr_variability(expression, provenance)?);
                binder_domain = merge_binder_domain(
                    storage,
                    binder_domain,
                    storage.expr_binder_domain(expression, provenance)?,
                    provenance,
                )?;
                PackedSubscriptKind::Slice(expression.index())
            }
        };
        storage.expressions.subscripts.push(PackedSubscript {
            kind,
            provenance: subscript.provenance(),
        });
    }
    let range = OperandRange::new(
        start,
        storage.expressions.subscripts.len() - start,
        provenance,
    )?;
    dimensions.extend_from_slice(&base_type.dimensions()[range.len as usize..]);
    Ok(PackedSelection {
        value_type: base_type.with_dimensions(dimensions),
        range,
        variability,
        binder_domain,
    })
}

/// Prove every constructor-known Integer coordinate lies inside its base axis.
///
/// Scalar literals and parameter bindings are checked directly. A checked DAE
/// range is monotone and owns a finite `u32` extent, so its first and actual
/// last coordinate prove every selected coordinate without enumeration. An
/// empty range performs no access. Literal array selectors are checked element
/// by element. Dynamic selectors remain explicit for checked consumers.
fn validate_constructor_known_subscript_bounds(
    storage: &mut Storage,
    expression: ExprId<'_>,
    axis_extent: u32,
    at: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let value_type = storage.expr_type(expression, at)?.clone();
    if value_type.scalar_type() != ScalarType::Integer {
        return Ok(());
    }
    let node = storage
        .expressions
        .nodes
        .get(expression.index() as usize)
        .cloned()
        .ok_or_else(|| crate::model::unknown("expression", expression.index(), at))?;
    let upper = i128::from(axis_extent);
    let in_bounds = |coordinate: i128| coordinate >= 1 && coordinate <= upper;
    if value_type.is_scalar() {
        return storage.check_or_defer_integer_subscript_bound(expression, axis_extent, at);
    }
    match node {
        ExprNode::Range {
            start,
            explicit_step,
            ..
        } => {
            let extent = value_type.dimensions()[0];
            if extent == 0 {
                return Ok(());
            }
            let start = storage
                .static_integer(ExprId::from_raw(start))
                .ok_or(DaeConstructionError::InvalidSubscript { span: at.span() })?;
            let step = match explicit_step {
                Some(step) => storage
                    .static_integer(ExprId::from_raw(step))
                    .ok_or(DaeConstructionError::InvalidSubscript { span: at.span() })?,
                None => 1,
            };
            let last = i128::from(extent)
                .checked_sub(1)
                .and_then(|ordinal| ordinal.checked_mul(i128::from(step)))
                .and_then(|offset| i128::from(start).checked_add(offset))
                .ok_or(DaeConstructionError::InvalidSubscript { span: at.span() })?;
            if !in_bounds(i128::from(start)) || !in_bounds(last) {
                return Err(DaeConstructionError::InvalidSubscript { span: at.span() });
            }
        }
        ExprNode::Array { operands } => {
            for index in operands.indices() {
                let operand = storage.expressions.operands[index];
                storage.check_or_defer_integer_subscript_bound(
                    ExprId::from_raw(operand),
                    axis_extent,
                    at,
                )?;
            }
        }
        _ => {}
    }
    Ok(())
}

fn packed_value_subscript(
    ty: &ValueType,
    expression: ExprId<'_>,
    dimensions: &mut Vec<u32>,
) -> PackedSubscriptKind {
    if ty.is_scalar() {
        return PackedSubscriptKind::Index(expression.index());
    }
    dimensions.extend_from_slice(ty.dimensions());
    PackedSubscriptKind::Slice(expression.index())
}
