use indexmap::IndexMap;
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use crate::LowerError;

#[derive(Debug, Clone, Copy)]
pub(crate) enum StorageClass {
    Y,
    P,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct VariableSlot {
    pub(crate) storage: StorageClass,
    pub(crate) base: usize,
    pub(crate) count: usize,
}

pub(crate) struct LoweredLayout<'dae> {
    pub(crate) variables: Vec<VariableSlot>,
    pub(crate) pre_variables: Vec<Option<usize>>,
    pub(crate) condition_memory: Vec<usize>,
    pub(crate) layout: solve::VarLayout,
    pub(crate) solve_layout: solve::SolveLayout,
    pub(crate) marker: std::marker::PhantomData<&'dae mut &'dae ()>,
}

/// Build the public variable layout directly from checked declarations.
pub fn build_var_layout(dae: &dae::Dae) -> Result<solve::VarLayout, LowerError> {
    dae.inspect(|view| lower_layout(view).map(|layout| layout.layout))
}

pub(crate) fn lower_layout<'dae>(
    view: dae::DaeView<'dae>,
) -> Result<LoweredLayout<'dae>, LowerError> {
    let mut bindings = IndexMap::new();
    let mut shapes = IndexMap::new();
    let mut shape_spans = IndexMap::new();
    let mut variables = vec![
        VariableSlot {
            storage: StorageClass::P,
            base: 0,
            count: 0,
        };
        view.variable_count()
    ];
    let y = append_y_variables(
        view,
        &mut bindings,
        &mut shapes,
        &mut shape_spans,
        &mut variables,
    )?;
    let p = append_p_variables(
        view,
        &mut bindings,
        &mut shapes,
        &mut shape_spans,
        &mut variables,
    )?;
    let (pre_variables, pre_param_bindings, pre_scalar_count) =
        append_pre_variables(view, &variables, p.names.len())?;

    let y_scalars = y.names.len();
    let condition_base =
        p.names.len().checked_add(pre_scalar_count).ok_or_else(|| {
            LowerError::contract("pre-value layout overflow", first_model_span(view))
        })?;
    let condition_memory = (0..view.condition_count())
        .map(|offset| {
            condition_base.checked_add(offset).ok_or_else(|| {
                LowerError::contract("condition-memory layout overflow", first_model_span(view))
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    let p_scalars = condition_base
        .checked_add(condition_memory.len())
        .ok_or_else(|| LowerError::contract("parameter layout overflow", first_model_span(view)))?;
    let layout = solve::VarLayout::from_parts_with_shapes_and_spans(
        bindings,
        shapes,
        shape_spans,
        y_scalars,
        p_scalars,
    )
    .map_err(|error| {
        LowerError::contract(
            error.to_string(),
            error
                .source_span()
                .unwrap_or_else(|| first_model_span(view)),
        )
    })?;
    let solver_maps = solve::SolverNameIndexMaps {
        name_to_idx: y
            .names
            .iter()
            .enumerate()
            .map(|(index, name)| (name.clone(), index))
            .collect(),
        base_to_indices: solver_base_indices(view, &variables),
        names: y.names,
    };
    let solve_layout = solve::SolveLayout {
        solver_maps,
        variable_base_slots: variables
            .iter()
            .map(|slot| match slot.storage {
                StorageClass::Y => solve::scalar_slot_y(slot.base),
                StorageClass::P => solve::scalar_slot_p(slot.base),
            })
            .collect(),
        state_scalar_count: y.state_count,
        algebraic_scalar_count: y.algebraic_count,
        output_scalar_count: y.output_count,
        parameter_count: p.parameter_count,
        compiled_parameter_len: p_scalars,
        input_scalar_names: p.input_names,
        discrete_real_scalar_names: p.discrete_real_names,
        discrete_valued_scalar_names: p.discrete_value_names,
        relation_memory_parameter_indices: Vec::new(),
        initial_event_parameter_index: None,
        terminal_event_parameter_index: None,
        initial_homotopy_parameter_index: None,
        pre_param_bindings,
    };
    Ok(LoweredLayout {
        variables,
        pre_variables,
        condition_memory,
        layout,
        solve_layout,
        marker: std::marker::PhantomData,
    })
}

fn append_pre_variables(
    view: dae::DaeView<'_>,
    variables: &[VariableSlot],
    first_pre_index: usize,
) -> Result<(Vec<Option<usize>>, Vec<solve::PreParamBinding>, usize), LowerError> {
    let mut pre_variables = vec![None; view.variable_count()];
    let mut bindings = Vec::new();
    let mut scalar_count = 0usize;
    for (id, variable) in view.variables().filter(|(_, variable)| {
        matches!(
            variable.role(),
            dae::VariableRole::DiscreteReal | dae::VariableRole::DiscreteValue
        )
    }) {
        let current = variables[id.index() as usize];
        debug_assert!(matches!(current.storage, StorageClass::P));
        let pre_base = first_pre_index.checked_add(scalar_count).ok_or_else(|| {
            LowerError::contract("pre-value layout overflow", variable.declaration().span())
        })?;
        pre_variables[id.index() as usize] = Some(pre_base);
        for scalar in 0..current.count {
            let source = current.base.checked_add(scalar).ok_or_else(|| {
                LowerError::contract(
                    "pre-value source layout overflow",
                    variable.declaration().span(),
                )
            })?;
            let dest_p_index = pre_base.checked_add(scalar).ok_or_else(|| {
                LowerError::contract(
                    "pre-value destination layout overflow",
                    variable.declaration().span(),
                )
            })?;
            bindings.push(solve::PreParamBinding {
                dest_p_index,
                source: solve::PreParamSource::P { index: source },
                clock_schedule: None,
            });
        }
        scalar_count = scalar_count.checked_add(current.count).ok_or_else(|| {
            LowerError::contract(
                "pre-value scalar count overflow",
                variable.declaration().span(),
            )
        })?;
    }
    Ok((pre_variables, bindings, scalar_count))
}

struct YColumns {
    names: Vec<String>,
    state_count: usize,
    algebraic_count: usize,
    output_count: usize,
}

fn append_y_variables(
    view: dae::DaeView<'_>,
    bindings: &mut IndexMap<String, solve::ScalarSlot>,
    shapes: &mut IndexMap<String, Vec<usize>>,
    shape_spans: &mut IndexMap<String, rumoca_core::Span>,
    slots: &mut [VariableSlot],
) -> Result<YColumns, LowerError> {
    let mut columns = YColumns {
        names: Vec::new(),
        state_count: 0,
        algebraic_count: 0,
        output_count: 0,
    };
    for role in [
        dae::VariableRole::State,
        dae::VariableRole::Algebraic,
        dae::VariableRole::Output,
    ] {
        for (id, variable) in view
            .variables()
            .filter(|(_, variable)| variable.role() == role)
        {
            let base = columns.names.len();
            insert_scalar_bindings(
                bindings,
                &mut columns.names,
                variable,
                StorageClass::Y,
                base,
            )?;
            record_shape(shapes, shape_spans, variable);
            slots[id.index() as usize] = VariableSlot {
                storage: StorageClass::Y,
                base,
                count: variable.scalar_count(),
            };
            match role {
                dae::VariableRole::State => columns.state_count += variable.scalar_count(),
                dae::VariableRole::Algebraic => {
                    columns.algebraic_count += variable.scalar_count();
                }
                dae::VariableRole::Output => columns.output_count += variable.scalar_count(),
                _ => unreachable!("selected solver variable role"),
            }
        }
    }
    Ok(columns)
}

struct PColumns {
    names: Vec<String>,
    parameter_count: usize,
    input_names: Vec<String>,
    discrete_real_names: Vec<String>,
    discrete_value_names: Vec<String>,
}

fn append_p_variables(
    view: dae::DaeView<'_>,
    bindings: &mut IndexMap<String, solve::ScalarSlot>,
    shapes: &mut IndexMap<String, Vec<usize>>,
    shape_spans: &mut IndexMap<String, rumoca_core::Span>,
    slots: &mut [VariableSlot],
) -> Result<PColumns, LowerError> {
    let mut columns = PColumns {
        names: Vec::new(),
        parameter_count: 0,
        input_names: Vec::new(),
        discrete_real_names: Vec::new(),
        discrete_value_names: Vec::new(),
    };
    for role in [
        dae::VariableRole::Parameter,
        dae::VariableRole::Constant,
        dae::VariableRole::Input,
        dae::VariableRole::DiscreteReal,
        dae::VariableRole::DiscreteValue,
    ] {
        for (id, variable) in view
            .variables()
            .filter(|(_, variable)| variable.role() == role)
        {
            let base = columns.names.len();
            insert_scalar_bindings(
                bindings,
                &mut columns.names,
                variable,
                StorageClass::P,
                base,
            )?;
            record_shape(shapes, shape_spans, variable);
            slots[id.index() as usize] = VariableSlot {
                storage: StorageClass::P,
                base,
                count: variable.scalar_count(),
            };
            append_parameter_role_names(&mut columns, variable)?;
        }
    }
    Ok(columns)
}

fn append_parameter_role_names(
    columns: &mut PColumns,
    variable: dae::VariableView<'_>,
) -> Result<(), LowerError> {
    let names = variable_scalar_names(variable)?;
    match variable.role() {
        dae::VariableRole::Parameter | dae::VariableRole::Constant => {
            columns.parameter_count += names.len();
        }
        dae::VariableRole::Input => columns.input_names.extend(names),
        dae::VariableRole::DiscreteReal => columns.discrete_real_names.extend(names),
        dae::VariableRole::DiscreteValue => columns.discrete_value_names.extend(names),
        dae::VariableRole::State | dae::VariableRole::Algebraic | dae::VariableRole::Output => {
            unreachable!("filtered parameter variable role")
        }
    }
    Ok(())
}

fn insert_scalar_bindings(
    bindings: &mut IndexMap<String, solve::ScalarSlot>,
    names: &mut Vec<String>,
    variable: dae::VariableView<'_>,
    storage: StorageClass,
    base: usize,
) -> Result<(), LowerError> {
    if !variable.value_type().is_scalar() && variable.scalar_count() != 0 {
        let slot = match storage {
            StorageClass::Y => solve::scalar_slot_y(base),
            StorageClass::P => solve::scalar_slot_p(base),
        };
        if bindings.insert(variable.name().to_string(), slot).is_some() {
            return Err(LowerError::contract(
                format!("duplicate aggregate layout name `{}`", variable.name()),
                variable.declaration().span(),
            ));
        }
    }
    for scalar in 0..variable.scalar_count() {
        let name = variable.scalar_name(scalar).ok_or_else(|| {
            LowerError::contract(
                format!(
                    "checked variable `{}` has no scalar name at ordinal {scalar}",
                    variable.name()
                ),
                variable.declaration().span(),
            )
        })?;
        let index = base.checked_add(scalar).ok_or_else(|| {
            LowerError::contract(
                "variable layout index overflow",
                variable.declaration().span(),
            )
        })?;
        let slot = match storage {
            StorageClass::Y => solve::scalar_slot_y(index),
            StorageClass::P => solve::scalar_slot_p(index),
        };
        if bindings.insert(name.clone(), slot).is_some() {
            return Err(LowerError::contract(
                format!("duplicate scalar layout name `{name}`"),
                variable.declaration().span(),
            ));
        }
        names.push(name);
    }
    Ok(())
}

fn record_shape(
    shapes: &mut IndexMap<String, Vec<usize>>,
    spans: &mut IndexMap<String, rumoca_core::Span>,
    variable: dae::VariableView<'_>,
) {
    if variable.value_type().is_scalar() {
        return;
    }
    let name = variable.name().to_string();
    shapes.insert(
        name.clone(),
        variable
            .value_type()
            .dimensions()
            .iter()
            .map(|extent| *extent as usize)
            .collect(),
    );
    spans.insert(name, variable.declaration().span());
}

fn variable_scalar_names(variable: dae::VariableView<'_>) -> Result<Vec<String>, LowerError> {
    (0..variable.scalar_count())
        .map(|scalar| {
            variable.scalar_name(scalar).ok_or_else(|| {
                LowerError::contract(
                    format!(
                        "checked variable `{}` has no scalar name at ordinal {scalar}",
                        variable.name()
                    ),
                    variable.declaration().span(),
                )
            })
        })
        .collect()
}

fn solver_base_indices(
    view: dae::DaeView<'_>,
    variables: &[VariableSlot],
) -> IndexMap<String, Vec<usize>> {
    view.variables()
        .filter_map(|(id, variable)| {
            let slot = variables[id.index() as usize];
            matches!(slot.storage, StorageClass::Y).then(|| {
                (
                    variable.name().to_string(),
                    (slot.base..slot.base + slot.count).collect(),
                )
            })
        })
        .collect()
}

fn first_model_span(view: dae::DaeView<'_>) -> rumoca_core::Span {
    view.responsible_span()
        .expect("nonempty checked DAE has responsible provenance")
}
