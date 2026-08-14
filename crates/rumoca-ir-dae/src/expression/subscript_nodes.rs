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
        crate::model::check_provenance(source_map, subscript.provenance())?;
        let axis_extent = base_type.dimensions()[axis];
        let kind = match subscript {
            Subscript::Value { expression, .. } => {
                let ty = storage.expr_type(expression, provenance)?.clone();
                validate_subscript(storage, expression, ty.is_scalar(), provenance)?;
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
