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
    pub(crate) pre_binding_starts: Vec<Option<usize>>,
    pub(crate) previous_values: Vec<usize>,
    pub(crate) condition_memory: Vec<usize>,
    pub(crate) clock_activations: Vec<usize>,
    pub(crate) delay_values: Vec<usize>,
    pub(crate) layout: solve::VarLayout,
    pub(crate) solve_layout: solve::SolveLayout,
    pub(crate) marker: std::marker::PhantomData<&'dae mut &'dae ()>,
}

struct RuntimeLayout {
    condition_memory: Vec<usize>,
    clock_activations: Vec<usize>,
    delay_values: Vec<usize>,
    initial_event_parameter_index: Option<usize>,
    terminal_event_parameter_index: Option<usize>,
    initial_homotopy_parameter_index: Option<usize>,
    scalar_count: usize,
}

struct RuntimeFlags {
    initial_event_parameter_index: Option<usize>,
    terminal_event_parameter_index: Option<usize>,
    initial_homotopy_parameter_index: Option<usize>,
    scalar_count: usize,
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
    let pre = append_pre_variables(view, &variables, p.names.len())?;

    let y_scalars = y.names.len();
    let runtime = append_runtime_layout(view, p.names.len(), pre.scalar_count)?;
    let layout = solve::VarLayout::from_parts_with_shapes_and_spans(
        bindings,
        shapes,
        shape_spans,
        y_scalars,
        runtime.scalar_count,
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
        variable_storage_runs: view
            .variables()
            .map(|(id, variable)| {
                let slot = variables[id.index() as usize];
                solve::SolveVariableStorageRun {
                    base: match slot.storage {
                        StorageClass::Y => solve::scalar_slot_y(slot.base),
                        StorageClass::P => solve::scalar_slot_p(slot.base),
                    },
                    scalar_count: slot.count,
                    role: solve_variable_storage_role(variable),
                    value_kind: solve_variable_value_kind(variable.value_type().scalar_type()),
                }
            })
            .collect(),
        variable_declarations: view
            .variables()
            .map(|(_, variable)| {
                solve::SolveVariableDeclaration::new(
                    solve_variable_storage_role(variable),
                    solve_variable_value_kind(variable.value_type().scalar_type()),
                )
            })
            .collect(),
        state_scalar_count: y.state_count,
        algebraic_scalar_count: y.algebraic_count,
        output_scalar_count: y.output_count,
        parameter_count: p.parameter_count,
        compiled_parameter_len: runtime.scalar_count,
        input_scalar_names: p.input_names,
        discrete_real_scalar_names: p.discrete_real_names,
        discrete_valued_scalar_names: p.discrete_value_names,
        relation_memory_parameter_indices: Vec::new(),
        initial_event_parameter_index: runtime.initial_event_parameter_index,
        terminal_event_parameter_index: runtime.terminal_event_parameter_index,
        initial_homotopy_parameter_index: runtime.initial_homotopy_parameter_index,
        pre_param_bindings: pre.bindings,
    };
    Ok(LoweredLayout {
        variables,
        pre_variables: pre.variables,
        pre_binding_starts: pre.binding_starts,
        previous_values: pre.previous_values,
        condition_memory: runtime.condition_memory,
        clock_activations: runtime.clock_activations,
        delay_values: runtime.delay_values,
        layout,
        solve_layout,
        marker: std::marker::PhantomData,
    })
}

fn solve_variable_storage_role(variable: dae::VariableView<'_>) -> solve::SolveVariableStorageRole {
    if variable.causality() == dae::VariableCausality::Input {
        return solve::SolveVariableStorageRole::ExternalInput;
    }
    match variable.role() {
        dae::VariableRole::Parameter => solve::SolveVariableStorageRole::Parameter,
        dae::VariableRole::Constant => solve::SolveVariableStorageRole::Constant,
        dae::VariableRole::Input => solve::SolveVariableStorageRole::ExternalInput,
        dae::VariableRole::State => solve::SolveVariableStorageRole::State,
        dae::VariableRole::Algebraic => solve::SolveVariableStorageRole::Algebraic,
        dae::VariableRole::Output => solve::SolveVariableStorageRole::Output,
        dae::VariableRole::DiscreteReal => solve::SolveVariableStorageRole::DiscreteReal,
        dae::VariableRole::DiscreteValue => solve::SolveVariableStorageRole::DiscreteValue,
    }
}

fn solve_variable_value_kind(kind: dae::ScalarType) -> solve::SolveVariableValueKind {
    match kind {
        dae::ScalarType::Real => solve::SolveVariableValueKind::Real,
        dae::ScalarType::Integer => solve::SolveVariableValueKind::Integer,
        dae::ScalarType::Boolean => solve::SolveVariableValueKind::Boolean,
        dae::ScalarType::Enumeration => solve::SolveVariableValueKind::Enumeration,
        dae::ScalarType::String => solve::SolveVariableValueKind::String,
        dae::ScalarType::Record => {
            unreachable!("checked model variables use primitive rectangular storage")
        }
    }
}

fn append_runtime_layout(
    view: dae::DaeView<'_>,
    parameter_count: usize,
    pre_count: usize,
) -> Result<RuntimeLayout, LowerError> {
    let condition_base = parameter_count
        .checked_add(pre_count)
        .ok_or_else(|| LowerError::contract("pre-value layout overflow", first_model_span(view)))?;
    let condition_memory = (0..view.condition_count())
        .map(|offset| {
            condition_base.checked_add(offset).ok_or_else(|| {
                LowerError::contract("condition-memory layout overflow", first_model_span(view))
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    let after_conditions = condition_base
        .checked_add(condition_memory.len())
        .ok_or_else(|| LowerError::contract("parameter layout overflow", first_model_span(view)))?;
    let clock_activations = (0..view.clock_count())
        .map(|offset| {
            after_conditions.checked_add(offset).ok_or_else(|| {
                LowerError::contract("clock-activation layout overflow", first_model_span(view))
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    let after_clocks = after_conditions
        .checked_add(clock_activations.len())
        .ok_or_else(|| LowerError::contract("parameter layout overflow", first_model_span(view)))?;
    let (delay_values, after_delays) = append_delay_values(view, after_clocks)?;
    let flags = append_runtime_flags(view, after_delays)?;
    Ok(RuntimeLayout {
        condition_memory,
        clock_activations,
        delay_values,
        initial_event_parameter_index: flags.initial_event_parameter_index,
        terminal_event_parameter_index: flags.terminal_event_parameter_index,
        initial_homotopy_parameter_index: flags.initial_homotopy_parameter_index,
        scalar_count: flags.scalar_count,
    })
}

fn append_delay_values(
    view: dae::DaeView<'_>,
    first_index: usize,
) -> Result<(Vec<usize>, usize), LowerError> {
    let mut bases = Vec::with_capacity(view.delay_count());
    let mut next = first_index;
    for index in 0..view.delay_count() {
        let delay = view
            .delay(view.delay_id(index).expect("dense delay identity"))
            .expect("checked delay identity resolves");
        bases.push(next);
        let count = delay.value_type().scalar_count().ok_or_else(|| {
            LowerError::contract(
                "delay value type exceeds scalar capacity",
                delay.provenance().span(),
            )
        })?;
        next = next.checked_add(count).ok_or_else(|| {
            LowerError::contract("delay-value layout overflow", delay.provenance().span())
        })?;
    }
    Ok((bases, next))
}

fn append_runtime_flags(
    view: dae::DaeView<'_>,
    first_index: usize,
) -> Result<RuntimeFlags, LowerError> {
    let has_initial = (0..view.condition_count()).any(|index| {
        view.condition(view.condition_id(index).expect("dense condition identity"))
            .is_some_and(|condition| {
                matches!(condition.operation(), dae::ConditionOperation::Initial)
            })
    });
    let after_initial = first_index
        .checked_add(usize::from(has_initial))
        .ok_or_else(|| LowerError::contract("parameter layout overflow", first_model_span(view)))?;
    let has_terminal = view.terminal_count() != 0;
    let after_terminal = after_initial
        .checked_add(usize::from(has_terminal))
        .ok_or_else(|| LowerError::contract("parameter layout overflow", first_model_span(view)))?;
    let has_homotopy = (0..view.expression_count()).any(|index| {
        view.expression(
            view.expression_id(index)
                .expect("dense expression identity"),
        )
        .is_some_and(|expression| {
            matches!(
                expression.operation(),
                dae::ExpressionOperation::Builtin {
                    builtin: dae::PureBuiltin::Homotopy,
                    ..
                }
            )
        })
    });
    let end = after_terminal
        .checked_add(usize::from(has_homotopy))
        .ok_or_else(|| LowerError::contract("parameter layout overflow", first_model_span(view)))?;
    Ok(RuntimeFlags {
        initial_event_parameter_index: has_initial.then_some(first_index),
        terminal_event_parameter_index: has_terminal.then_some(after_initial),
        initial_homotopy_parameter_index: has_homotopy.then_some(after_terminal),
        scalar_count: end,
    })
}

struct PreVariableLayout {
    variables: Vec<Option<usize>>,
    binding_starts: Vec<Option<usize>>,
    previous_values: Vec<usize>,
    bindings: Vec<solve::PreParamBinding>,
    scalar_count: usize,
}

fn append_pre_variables(
    view: dae::DaeView<'_>,
    variables: &[VariableSlot],
    first_pre_index: usize,
) -> Result<PreVariableLayout, LowerError> {
    let mut pre_variables = vec![None; view.variable_count()];
    let mut binding_starts = vec![None; view.variable_count()];
    let mut bindings = Vec::new();
    let mut scalar_count = 0usize;
    let continuous_pre = continuous_pre_variables(view);
    for (id, variable) in view.variables().filter(|(id, variable)| {
        matches!(
            variable.role(),
            dae::VariableRole::DiscreteReal | dae::VariableRole::DiscreteValue
        ) || continuous_pre[id.index() as usize]
    }) {
        let current = variables[id.index() as usize];
        let pre_base = first_pre_index.checked_add(scalar_count).ok_or_else(|| {
            LowerError::contract("pre-value layout overflow", variable.declaration().span())
        })?;
        pre_variables[id.index() as usize] = Some(pre_base);
        binding_starts[id.index() as usize] = Some(bindings.len());
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
            // MLS §3.7.5: the history lane holds the coordinate's left limit,
            // so it is snapshot from wherever the coordinate lives — solver
            // state `y` for a continuous coordinate, `p` for a discrete one.
            let source = match current.storage {
                StorageClass::Y => solve::PreParamSource::Y { index: source },
                StorageClass::P => solve::PreParamSource::P { index: source },
            };
            bindings.push(solve::PreParamBinding {
                dest_p_index,
                source,
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
    let mut previous_values = Vec::with_capacity(view.previous_value_count());
    for index in 0..view.previous_value_count() {
        let previous = view
            .previous(
                view.previous_id(index)
                    .expect("dense previous identity resolves"),
            )
            .expect("checked previous entry resolves");
        let variable = previous.variable().index() as usize;
        let current = variables[variable];
        let base = first_pre_index.checked_add(scalar_count).ok_or_else(|| {
            LowerError::contract(
                "previous-value layout overflow",
                previous.provenance().span(),
            )
        })?;
        previous_values.push(base);
        let clock_schedule = previous_schedule(view, previous)?;
        for scalar in 0..current.count {
            let source = current.base.checked_add(scalar).ok_or_else(|| {
                LowerError::contract(
                    "previous-value source layout overflow",
                    previous.provenance().span(),
                )
            })?;
            let dest_p_index = base.checked_add(scalar).ok_or_else(|| {
                LowerError::contract(
                    "previous-value destination layout overflow",
                    previous.provenance().span(),
                )
            })?;
            bindings.push(solve::PreParamBinding {
                dest_p_index,
                source: solve::PreParamSource::P { index: source },
                clock_schedule: Some(clock_schedule.clone()),
            });
        }
        scalar_count = scalar_count.checked_add(current.count).ok_or_else(|| {
            LowerError::contract(
                "previous-value scalar count overflow",
                previous.provenance().span(),
            )
        })?;
    }
    Ok(PreVariableLayout {
        variables: pre_variables,
        binding_starts,
        previous_values,
        bindings,
        scalar_count,
    })
}

/// Continuous coordinates some expression reads through `pre()`.
///
/// Every discrete coordinate gets a history lane unconditionally, because any
/// of them may be read by a later `pre()` the DAE does not name yet. A
/// continuous coordinate gets one only where the model actually takes its left
/// limit: giving every state and algebraic a lane would grow the parameter
/// vector of every model in the cohort for a value nothing reads.
fn continuous_pre_variables(view: dae::DaeView<'_>) -> Vec<bool> {
    let mut referenced = vec![false; view.variable_count()];
    for index in 0..view.expression_count() {
        let Some(expression) = view.expression(
            view.expression_id(index)
                .expect("dense expression identity"),
        ) else {
            continue;
        };
        let dae::ExpressionOperation::Coordinate(coordinate) = expression.operation() else {
            continue;
        };
        let variable = match coordinate {
            dae::CoordinateView::PreState(id) => id.index(),
            dae::CoordinateView::PreAlgebraic(id) => id.index(),
            _ => continue,
        };
        if let Some(slot) = referenced.get_mut(variable as usize) {
            *slot = true;
        }
    }
    mark_sampled_value_sources(view, &mut referenced);
    referenced
}

/// Add the continuous coordinates read by MLS §16.5.1 `sample(u)` owners.
///
/// Discrete coordinates already receive a pre lane unconditionally. A
/// continuous sample source needs the same event-entry lane so a tick that is
/// coincident with a source discontinuity reads `u(t - eps)`, not the value
/// produced while that event is settling.
fn mark_sampled_value_sources(view: dae::DaeView<'_>, referenced: &mut [bool]) {
    let sampled = sampled_clock_variables(view);
    for index in 0..view.discrete_real_equation_count() {
        let equation = view
            .discrete_real_equation(index)
            .expect("dense checked discrete Real equation resolves");
        let has_sampled_target =
            expression_mentions_sampled_variable(view, equation.residual(), &sampled);
        if has_sampled_target {
            mark_expression_variables(view, equation.residual(), referenced);
        }
    }
    for index in 0..view.discrete_value_owner_count() {
        let owner = view
            .discrete_value_owner(
                view.discrete_value_owner_id(index)
                    .expect("dense B.1c owner identity resolves"),
            )
            .expect("checked B.1c owner resolves");
        for (target_ordinal, target) in owner.targets().iter().enumerate() {
            if !sampled[target.index() as usize] {
                continue;
            }
            for branch in owner.branches().iter() {
                let (value, _) = branch
                    .values()
                    .get(target_ordinal)
                    .expect("checked B.1c branch matches its target arity");
                mark_expression_variables(view, value, referenced);
            }
        }
    }
}

fn expression_mentions_sampled_variable<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ExprId<'dae>,
    sampled: &[bool],
) -> bool {
    let mut found = false;
    dae::for_each_expression(view, root, |_, expression| {
        found |= expression.variable_coordinate().is_some_and(|variable| {
            sampled
                .get(variable.index() as usize)
                .copied()
                .unwrap_or(false)
        });
    });
    found
}

fn sampled_clock_variables(view: dae::DaeView<'_>) -> Vec<bool> {
    let mut sampled = vec![false; view.variable_count()];
    for index in 0..view.clock_ownership_count() {
        let ownership = view
            .clock_ownership(
                view.clock_ownership_id(index)
                    .expect("dense clock ownership identity resolves"),
            )
            .expect("checked clock ownership resolves");
        sampled[ownership.variable().index() as usize] = ownership.sampled();
    }
    sampled
}

fn mark_expression_variables<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ExprId<'dae>,
    referenced: &mut [bool],
) {
    dae::for_each_expression(view, root, |_, expression| {
        let Some(variable) = expression.variable_coordinate() else {
            return;
        };
        let role = view
            .variable(variable)
            .expect("checked expression coordinate resolves")
            .role();
        if matches!(
            role,
            dae::VariableRole::Input
                | dae::VariableRole::State
                | dae::VariableRole::Algebraic
                | dae::VariableRole::Output
        ) {
            referenced[variable.index() as usize] = true;
        }
    });
}

fn previous_schedule<'dae>(
    view: dae::DaeView<'dae>,
    previous: dae::PreviousView<'dae>,
) -> Result<solve::PeriodicEventSchedule, LowerError> {
    let clock = view
        .clock(previous.clock())
        .expect("checked previous clock resolves");
    let dae::ClockOperation::Periodic(schedule) = clock.operation() else {
        return Err(LowerError::unsupported(
            "triggered-clock previous history has no periodic Solve schedule",
            previous.provenance().span(),
        ));
    };
    solve::PeriodicEventSchedule::from_schedule(*schedule).map_err(|error| {
        LowerError::contract(
            format!("invalid previous-value clock schedule: {error}"),
            previous.provenance().span(),
        )
    })
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
    if variable.causality() == dae::VariableCausality::Input {
        columns.input_names.extend(names);
        return Ok(());
    }
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
