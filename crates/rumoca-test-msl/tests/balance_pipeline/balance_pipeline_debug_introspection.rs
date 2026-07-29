use super::*;

// =============================================================================
// Model-level introspection helpers for focused MSL debugging
// =============================================================================

fn checked_expression_source<'dae>(
    view: rumoca_ir_dae::DaeView<'dae>,
    expression: rumoca_ir_dae::ExprId<'dae>,
) -> &'dae str {
    let node = view
        .expression(expression)
        .expect("checked expression identity resolves");
    view.source_text(node.provenance())
        .unwrap_or("<generated from unavailable source text>")
}

fn visit_semantic_expression_roots<'dae>(
    view: rumoca_ir_dae::DaeView<'dae>,
    mut visit: impl FnMut(rumoca_ir_dae::ExprId<'dae>),
) {
    for index in 0..view.continuous_equation_count() {
        visit(
            view.continuous_equation(index)
                .expect("dense checked continuous equation resolves")
                .residual(),
        );
    }
    for index in 0..view.initialization_equation_count() {
        visit(
            view.initialization_equation(index)
                .expect("dense checked initialization equation resolves")
                .residual(),
        );
    }
    for index in 0..view.discrete_real_equation_count() {
        visit(
            view.discrete_real_equation(index)
                .expect("dense checked discrete equation resolves")
                .residual(),
        );
    }
    for index in 0..view.discrete_assignment_count() {
        let id = view
            .discrete_assignment_id(index)
            .expect("dense checked discrete assignment identity resolves");
        visit(
            view.discrete_assignment(id)
                .expect("dense checked discrete assignment resolves")
                .value(),
        );
    }
    for index in 0..view.relation_count() {
        let id = view
            .relation_id(index)
            .expect("dense checked relation identity resolves");
        visit(
            view.relation(id)
                .expect("dense checked relation resolves")
                .expression(),
        );
    }
}

fn record_coordinate_identity(
    expression: rumoca_ir_dae::ExpressionView<'_>,
    referenced: &mut std::collections::HashSet<u32>,
) {
    if let Some(variable) = expression.variable_coordinate() {
        referenced.insert(variable.index());
    }
}

fn expression_references_continuous_unknown<'dae>(
    view: rumoca_ir_dae::DaeView<'dae>,
    root: rumoca_ir_dae::ExprId<'dae>,
) -> bool {
    let mut has_unknown = false;
    rumoca_ir_dae::for_each_expression(view, root, |_, expression| {
        has_unknown |= expression
            .variable_coordinate()
            .and_then(|id| view.variable(id))
            .is_some_and(|variable| {
                matches!(
                    variable.role(),
                    rumoca_ir_dae::VariableRole::State
                        | rumoca_ir_dae::VariableRole::Algebraic
                        | rumoca_ir_dae::VariableRole::Output
                )
            });
    });
    has_unknown
}

/// Print continuous unknown declarations that appear in no semantic expression.
pub(super) fn print_orphaned_unknowns(dae: &Dae) {
    dae.inspect(|view| {
        let mut referenced = std::collections::HashSet::new();
        visit_semantic_expression_roots(view, |root| {
            rumoca_ir_dae::for_each_expression(view, root, |_, expression| {
                record_coordinate_identity(expression, &mut referenced);
            });
        });

        println!("\n--- Orphaned continuous unknowns (in no semantic expression) ---");
        let mut count = 0;
        for (id, variable) in view.variables().filter(|(_, variable)| {
            matches!(
                variable.role(),
                rumoca_ir_dae::VariableRole::State
                    | rumoca_ir_dae::VariableRole::Algebraic
                    | rumoca_ir_dae::VariableRole::Output
            )
        }) {
            if !referenced.contains(&id.index()) {
                count += 1;
                println!(
                    "  {} [{:?}] scalars={}",
                    variable.name(),
                    variable.role(),
                    variable.scalar_count()
                );
            }
        }
        println!("  Total orphaned declarations: {count}");
    });
}

/// Print flat equation summary (diagnostic helper).
pub(super) fn print_flat_equation_summary(flat: &rumoca_ir_flat::Model) {
    println!("\n--- Flat equation count ---");
    println!("  equations: {}", flat.equations.len());
    println!("  initial_equations: {}", flat.initial_equations.len());
    println!("  when_chains: {}", flat.when_chains.len());
    println!("  algorithms: {}", flat.algorithms.len());
    println!("  top_level_connectors: {:?}", flat.top_level_connectors);
    println!("  definite_roots: {:?}", flat.definite_roots);
    println!("  potential_roots: {:?}", flat.potential_roots);
    println!("  branches: {}", flat.branches.len());
    for (index, (left, right)) in flat.branches.iter().take(10).enumerate() {
        println!("    [{index}] {left} -> {right}");
    }
    println!(
        "  oc_break_edge_scalar_count: {}",
        flat.oc_break_edge_scalar_count
    );
}

pub(super) fn print_flat_variables(flat: &rumoca_ir_flat::Model) {
    println!("\n--- Flat Variables (causality) ---");
    let mut primitive_scalars = 0usize;
    let mut non_primitive_scalars = 0usize;
    for (name, variable) in &flat.variables {
        let scalar_size = if variable.dims.is_empty() {
            1usize
        } else if variable.dims.iter().any(|&extent| extent <= 0) {
            0usize
        } else {
            variable
                .dims
                .iter()
                .fold(1usize, |acc, &extent| acc.saturating_mul(extent as usize))
        };
        if variable.is_primitive {
            primitive_scalars += scalar_size;
        } else {
            non_primitive_scalars += scalar_size;
        }
        println!(
            "  {name} causality={:?} flow={} stream={} primitive={} dims={:?}",
            variable.causality,
            variable.flow,
            variable.stream,
            variable.is_primitive,
            variable.dims
        );
    }
    println!(
        "  [summary] primitive_scalars={primitive_scalars} non_primitive_scalars={non_primitive_scalars}"
    );
}

pub(super) fn print_dae_variables(dae: &Dae) {
    let counts = checked_dae_counts(dae);
    println!(
        "\n--- Checked DAE Scalar Summary ---\n  states={} algebraics={} outputs={} inputs={} parameters={} constants={} discrete_reals={} discrete_values={}",
        counts.state_scalars,
        counts.algebraic_scalars,
        counts.output_scalars,
        counts.input_scalars,
        counts.parameter_scalars,
        counts.constant_scalars,
        counts.discrete_real_scalars,
        counts.discrete_value_scalars,
    );
    dae.inspect(|view| {
        for role in [
            rumoca_ir_dae::VariableRole::State,
            rumoca_ir_dae::VariableRole::Algebraic,
            rumoca_ir_dae::VariableRole::Output,
            rumoca_ir_dae::VariableRole::Input,
            rumoca_ir_dae::VariableRole::Parameter,
            rumoca_ir_dae::VariableRole::Constant,
            rumoca_ir_dae::VariableRole::DiscreteReal,
            rumoca_ir_dae::VariableRole::DiscreteValue,
        ] {
            println!("\n--- {role:?} ---");
            for (_, variable) in view
                .variables()
                .filter(|(_, variable)| variable.role() == role)
            {
                println!(
                    "  {} (scalars={}, type={:?})",
                    variable.name(),
                    variable.scalar_count(),
                    variable.value_type()
                );
            }
        }
    });
}

pub(super) fn print_dae_equations(dae: &Dae, equation_limit: usize) {
    dae.inspect(|view| {
        let shown = view.continuous_equation_count().min(equation_limit);
        println!(
            "\n--- Checked continuous residuals ({}) showing {} ---",
            view.continuous_equation_count(),
            shown
        );
        for index in 0..shown {
            let equation = view
                .continuous_equation(index)
                .expect("dense checked continuous equation resolves");
            println!(
                "  [{index}] expr={} owner={:?} source={:?}",
                equation.residual().index(),
                equation.provenance().origin(),
                checked_expression_source(view, equation.residual())
            );
        }
        if view.continuous_equation_count() > shown {
            println!(
                "  ... omitted {} equations",
                view.continuous_equation_count() - shown
            );
        }
        println!(
            "  compact_families={} initialization_residuals={} discrete_real_residuals={} discrete_assignments={} relations={} conditions={}",
            view.continuous_family_count(),
            view.initialization_equation_count(),
            view.discrete_real_equation_count(),
            view.discrete_assignment_count(),
            view.relation_count(),
            view.condition_count(),
        );
    });
}

/// Print continuous residuals that do not reference any continuous unknown.
pub(super) fn print_equations_without_unknowns(dae: &Dae) {
    dae.inspect(|view| {
        let mut count = 0usize;
        println!("\n--- Continuous Residuals Without Continuous Unknown Refs ---");
        for index in 0..view.continuous_equation_count() {
            let equation = view
                .continuous_equation(index)
                .expect("dense checked continuous equation resolves");
            if !expression_references_continuous_unknown(view, equation.residual()) {
                count += 1;
                println!(
                    "  [{index}] {:?}",
                    checked_expression_source(view, equation.residual())
                );
            }
        }
        println!("  Total: {count} residuals");
    });
}

pub(super) fn print_compiled_debug_with_limit(
    dae: &Dae,
    flat: &rumoca_ir_flat::Model,
    balance: &rumoca_phase_dae::balance::BalanceDetail,
    equation_limit: usize,
) {
    println!(
        "Success! {}",
        rumoca_phase_dae::BalanceBreakdown::from(balance.clone())
    );
    println!(
        "active_discrete_scalar_count = {}",
        active_discrete_scalar_count(flat, dae)
    );
    println!(
        "flat class_type={} partial={}",
        flat.class_type.as_str(),
        flat.is_partial
    );
    print_flat_variables(flat);
    print_dae_variables(dae);
    print_dae_equations(dae, equation_limit);
    print_equations_without_unknowns(dae);
    print_orphaned_unknowns(dae);
    print_flat_equation_summary(flat);
}

pub(super) fn maybe_dump_model_introspection(
    name: &str,
    result: &rumoca_compile::compile::CompilationResult,
    ctx: &RenderSimContext<'_>,
) {
    if !msl_introspect_enabled() || !should_introspect_model(name) {
        return;
    }
    if ctx.run_simulation
        && (!is_explicit_msl_example_model(name) || !is_selected_sim_target(name, ctx))
    {
        return;
    }
    println!("\n=== MSL Introspection: {name} ===");
    print_compiled_debug_with_limit(
        &result.dae,
        &result.flat,
        &result.balance_detail,
        msl_introspect_eq_limit(),
    );
    println!("=== End Introspection: {name} ===");
}
