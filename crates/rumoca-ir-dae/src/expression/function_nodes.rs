use super::*;

impl<'dae> ExpressionAt<'_, 'dae> {
    fn function_value_use(
        self,
        value: FunctionValueId<'dae>,
        definition: FunctionDefinitionId<'dae>,
        rhs: ExprId<'dae>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let ty = self.storage.function_value_facts(value, self.provenance)?;
        self.storage.expect_value_type_compatible(
            ty.index(),
            definition_type(self.storage, rhs, self.provenance)?,
            self.provenance,
        )?;
        let variability = self.storage.expr_variability(rhs, self.provenance)?;
        let binder_domain = self.storage.expr_binder_domain(rhs, self.provenance)?;
        self.insert(
            ExprNode::FunctionValue {
                function: value.function().index(),
                value: value.ordinal(),
                definition_ordinal: definition.ordinal(),
            },
            ty,
            variability,
            binder_domain,
        )
    }
}

pub(crate) fn insert_function_value_use<'dae>(
    source_map: &rumoca_core::SourceMap,
    storage: &mut Storage,
    value: FunctionValueId<'dae>,
    definition: FunctionDefinitionId<'dae>,
    domain: Option<DomainId<'dae>>,
    provenance: DaeProvenance,
) -> Result<ExprId<'dae>, DaeConstructionError> {
    let rhs = crate::model::function_definition_rhs(storage, value, definition, provenance)?;
    match domain {
        Some(domain) => {
            storage.expect_domain_expression(rhs, domain, provenance)?;
        }
        None => {
            if let Some(found_domain) = storage.expr_binder_domain(rhs, provenance)? {
                return Err(DaeConstructionError::InvalidBinderScope {
                    expected_domain: None,
                    found_domain,
                    span: provenance.span(),
                });
            }
        }
    }
    match storage.expr_function_scope(rhs, provenance)? {
        None => {}
        Some(function) if function == value.function().index() => {}
        Some(function) => {
            return Err(DaeConstructionError::InvalidFunctionScope {
                expected_function: Some(value.function().index()),
                found_function: function,
                span: provenance.span(),
            });
        }
    }
    ExpressionAt {
        source_map,
        storage,
        provenance,
        marker: std::marker::PhantomData,
    }
    .function_value_use(value, definition, rhs)
}

pub(crate) fn insert_function_fold_parameter<'dae>(
    source_map: &rumoca_core::SourceMap,
    storage: &mut Storage,
    fold: FunctionFoldId<'dae>,
    carried: usize,
    definition: FunctionDefinitionId<'dae>,
    provenance: DaeProvenance,
) -> Result<ExprId<'dae>, DaeConstructionError> {
    expect_pending_fold_definition(storage, fold, definition, provenance)?;
    let (value_type, domain) = function_fold_value_facts(storage, fold, carried, provenance)?;
    ExpressionAt {
        source_map,
        storage,
        provenance,
        marker: std::marker::PhantomData,
    }
    .insert(
        ExprNode::FunctionFoldParameter {
            function: fold.function().index(),
            fold: fold.ordinal(),
            carried: checked_u32(carried, "function fold parameter", provenance)?,
            definition_ordinal: definition.ordinal(),
        },
        ValueTypeId::from_raw(value_type),
        ExpressionVariability::Parameter,
        Some(domain),
    )
}

pub(crate) fn insert_function_fold_output<'dae>(
    source_map: &rumoca_core::SourceMap,
    storage: &mut Storage,
    fold: FunctionFoldId<'dae>,
    carried: usize,
    definition: FunctionDefinitionId<'dae>,
    provenance: DaeProvenance,
) -> Result<ExprId<'dae>, DaeConstructionError> {
    expect_pending_fold_definition(storage, fold, definition, provenance)?;
    let (value_type, _) = function_fold_value_facts(storage, fold, carried, provenance)?;
    let entry = function_fold_entry(storage, fold, provenance)?;
    let initial =
        FunctionDefinitionId::from_raw(fold.function().index(), entry.initial_definitions[carried]);
    let update =
        FunctionDefinitionId::from_raw(fold.function().index(), entry.update_definitions[carried]);
    let initial = crate::model::function_definition_rhs(
        storage,
        FunctionValueId::from_raw(fold.function().index(), entry.targets[carried]),
        initial,
        provenance,
    )?;
    let update = crate::model::function_definition_rhs(
        storage,
        FunctionValueId::from_raw(fold.function().index(), entry.targets[carried]),
        update,
        provenance,
    )?;
    let variability = storage
        .expr_variability(initial, provenance)?
        .max(storage.expr_variability(update, provenance)?);
    ExpressionAt {
        source_map,
        storage,
        provenance,
        marker: std::marker::PhantomData,
    }
    .insert(
        ExprNode::FunctionFoldOutput {
            function: fold.function().index(),
            fold: fold.ordinal(),
            carried: checked_u32(carried, "function fold output", provenance)?,
            definition_ordinal: definition.ordinal(),
        },
        ValueTypeId::from_raw(value_type),
        variability,
        None,
    )
}

fn expect_pending_fold_definition<'dae>(
    storage: &Storage,
    fold: FunctionFoldId<'dae>,
    definition: FunctionDefinitionId<'dae>,
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    if definition.function() != fold.function() {
        return Err(DaeConstructionError::InvalidFunctionScope {
            expected_function: Some(fold.function().index()),
            found_function: definition.function().index(),
            span: provenance.span(),
        });
    }
    let expected = storage
        .functions
        .get(fold.function().index() as usize)
        .ok_or_else(|| crate::model::unknown("function", fold.function().index(), provenance))?
        .definitions
        .len();
    if definition.ordinal() as usize == expected {
        return Ok(());
    }
    Err(DaeConstructionError::InvalidFunctionValueRead {
        value: 0,
        expected_definition: u32::try_from(expected).ok(),
        found_definition: definition.ordinal(),
        span: provenance.span(),
    })
}

fn function_fold_value_facts(
    storage: &Storage,
    fold: FunctionFoldId<'_>,
    carried: usize,
    provenance: DaeProvenance,
) -> Result<(u32, u32), DaeConstructionError> {
    let entry = function_fold_entry(storage, fold, provenance)?;
    let value = entry
        .targets
        .get(carried)
        .copied()
        .ok_or_else(|| invalid_arity(entry.targets.len(), carried + 1, provenance))?;
    let value_type = storage
        .functions
        .get(fold.function().index() as usize)
        .and_then(|function| function.values.get(value as usize))
        .map(|value| value.value_type)
        .ok_or_else(|| crate::model::unknown("function fold target", value, provenance))?;
    Ok((value_type, entry.domain))
}

pub(super) fn function_fold_entry<'storage>(
    storage: &'storage Storage,
    fold: FunctionFoldId<'_>,
    provenance: DaeProvenance,
) -> Result<&'storage crate::model::FunctionFoldEntry, DaeConstructionError> {
    let raw = storage
        .functions
        .get(fold.function().index() as usize)
        .and_then(|function| function.folds.get(fold.ordinal() as usize))
        .copied()
        .ok_or_else(|| crate::model::unknown("function fold", fold.ordinal(), provenance))?;
    storage
        .function_folds
        .get(raw as usize)
        .ok_or_else(|| crate::model::unknown("function fold", raw, provenance))
}
