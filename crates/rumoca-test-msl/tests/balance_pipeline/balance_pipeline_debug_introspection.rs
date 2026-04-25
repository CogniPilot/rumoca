use super::*;

// =============================================================================
// Model-level introspection helpers for focused MSL debugging
// =============================================================================

/// Print unknowns that appear in no equation (diagnostic helper).
pub(super) fn print_orphaned_unknowns(dae: &rumoca_ir_dae::Dae) {
    use std::collections::HashSet as StdHashSet;
    let mut eq_refs = StdHashSet::<rumoca_ir_dae::VarName>::new();
    for eq in &dae.f_x {
        eq.rhs.collect_var_refs(&mut eq_refs);
    }
    for eq in dae.f_z.iter().chain(dae.f_m.iter()).chain(dae.f_c.iter()) {
        eq.rhs.collect_var_refs(&mut eq_refs);
    }
    for relation in &dae.relation {
        relation.collect_var_refs(&mut eq_refs);
    }
    println!("\n--- Orphaned unknowns (in no equation) ---");
    let mut orphaned = Vec::new();
    for (n, v) in &dae.states {
        if !eq_refs.contains(n) {
            orphaned.push((n.clone(), "state", v.size()));
        }
    }
    for (n, v) in &dae.algebraics {
        if !eq_refs.contains(n) {
            orphaned.push((n.clone(), "alg", v.size()));
        }
    }
    for (n, v) in &dae.outputs {
        if !eq_refs.contains(n) {
            orphaned.push((n.clone(), "out", v.size()));
        }
    }
    for (n, kind, sz) in &orphaned {
        println!("  {} [{}] size={}", n, kind, sz);
    }
    println!("  Total orphaned: {}", orphaned.len());
}

/// Print flat equation summary (diagnostic helper).
pub(super) fn print_flat_equation_summary(flat: &rumoca_ir_flat::Model) {
    println!("\n--- Flat equation count ---");
    println!("  equations: {}", flat.equations.len());
    println!("  initial_equations: {}", flat.initial_equations.len());
    println!("  when_clauses: {}", flat.when_clauses.len());
    println!("  algorithms: {}", flat.algorithms.len());
    println!("  top_level_connectors: {:?}", flat.top_level_connectors);
    println!("  definite_roots: {:?}", flat.definite_roots);
    println!("  potential_roots: {:?}", flat.potential_roots);
    println!("  branches: {}", flat.branches.len());
    for (i, (a, b)) in flat.branches.iter().take(10).enumerate() {
        println!("    [{}] {} -> {}", i, a, b);
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
    for (name, var) in &flat.variables {
        let scalar_size = if var.dims.is_empty() {
            1usize
        } else if var.dims.iter().any(|&d| d <= 0) {
            0usize
        } else {
            var.dims
                .iter()
                .fold(1usize, |acc, &d| acc.saturating_mul(d as usize))
        };
        if var.is_primitive {
            primitive_scalars += scalar_size;
        } else {
            non_primitive_scalars += scalar_size;
        }
        println!(
            "  {} causality={:?} flow={} stream={} primitive={} dims={:?}",
            name, var.causality, var.flow, var.stream, var.is_primitive, var.dims
        );
    }
    println!(
        "  [summary] primitive_scalars={} non_primitive_scalars={}",
        primitive_scalars, non_primitive_scalars
    );
}

pub(super) fn print_dae_variables(dae: &Dae) {
    let state_scalars: usize = dae.states.values().map(|v| v.size()).sum();
    let algebraic_scalars: usize = dae.algebraics.values().map(|v| v.size()).sum();
    let output_scalars: usize = dae.outputs.values().map(|v| v.size()).sum();
    let input_scalars: usize = dae.inputs.values().map(|v| v.size()).sum();
    let discrete_real_scalars: usize = dae.discrete_reals.values().map(|v| v.size()).sum();
    let discrete_valued_scalars: usize = dae.discrete_valued.values().map(|v| v.size()).sum();

    println!(
        "\n--- DAE Scalar Summary ---\n  states={} algebraics={} outputs={} inputs={} discrete_reals={} discrete_valued={}",
        state_scalars,
        algebraic_scalars,
        output_scalars,
        input_scalars,
        discrete_real_scalars,
        discrete_valued_scalars
    );

    println!("\n--- States ---");
    for (n, v) in &dae.states {
        println!("  {} (sc={})", n, v.size());
    }
    println!("\n--- Algebraics ---");
    for (n, v) in &dae.algebraics {
        println!("  {} (sc={})", n, v.size());
    }
    println!("\n--- Outputs ---");
    for (n, v) in &dae.outputs {
        println!("  {} (sc={})", n, v.size());
    }
    println!("\n--- Inputs ---");
    for (n, v) in &dae.inputs {
        println!("  {} (dims={:?})", n, v.dims);
    }
    println!("\n--- Parameters ---");
    for (n, _) in &dae.parameters {
        println!("  {}", n);
    }
    println!("\n--- Constants ---");
    for (n, _) in &dae.constants {
        println!("  {}", n);
    }
    println!("\n--- Discrete Reals ---");
    for (n, _) in &dae.discrete_reals {
        println!("  {}", n);
    }
    println!("\n--- Discrete Valued ---");
    for (n, _) in &dae.discrete_valued {
        println!("  {}", n);
    }
}

pub(super) fn print_dae_equations(dae: &Dae, eq_limit: usize) {
    let shown = dae.f_x.len().min(eq_limit);
    println!(
        "\n--- Continuous equations f_x ({}) showing {} ---",
        dae.f_x.len(),
        shown
    );
    for (i, eq) in dae.f_x.iter().take(eq_limit).enumerate() {
        let lhs_name = match &eq.rhs {
            rumoca_ir_dae::Expression::Binary { lhs, .. } => match lhs.as_ref() {
                rumoca_ir_dae::Expression::VarRef { name, .. } => name.as_str().to_string(),
                _ => "??".to_string(),
            },
            _ => "??".to_string(),
        };
        let s = format!("{:?}", eq.rhs);
        if s.contains("from_Q")
            || s.contains("orientationConstraint")
            || s.contains("\"body.frame_a.R\"")
        {
            println!(
                "  [f_x-{}] [sc={}] {} | first 300: {:.300}",
                i, eq.scalar_count, eq.origin, s
            );
        } else if eq.scalar_count == 2 {
            println!(
                "  [sc={}] {} | lhs={:?} | rhs_preview={:.120}",
                eq.scalar_count,
                eq.origin,
                eq.lhs,
                format!("{:?}", eq.rhs)
            );
        } else {
            println!(
                "  [sc={}] {} | LHS={}",
                eq.scalar_count, eq.origin, lhs_name
            );
        }
    }
    if dae.f_x.len() > shown {
        println!("  ... omitted {} equations", dae.f_x.len() - shown);
    }
}

/// Print equations that do not reference any continuous unknown.
pub(super) fn print_equations_without_unknowns(dae: &Dae) {
    use std::collections::HashSet;

    let unknowns: HashSet<rumoca_ir_dae::VarName> = dae
        .states
        .keys()
        .chain(dae.algebraics.keys())
        .chain(dae.outputs.keys())
        .cloned()
        .collect();

    let mut total_sc = 0usize;
    let mut count = 0usize;
    println!("\n--- Equations Without Unknown Refs ---");
    for (i, eq) in dae.f_x.iter().enumerate() {
        let mut refs = HashSet::new();
        eq.rhs.collect_var_refs(&mut refs);
        let has_unknown = refs.iter().any(|r| {
            unknowns.contains(r)
                || unknowns
                    .iter()
                    .any(|u| u.as_str().starts_with(&(r.as_str().to_string() + ".")))
        });
        if !has_unknown {
            count += 1;
            total_sc += eq.scalar_count;
            println!(
                "  [f_x-{}] [sc={}] {} | lhs={:?}",
                i,
                eq.scalar_count,
                eq.origin,
                eq.lhs.as_ref().map(|n| n.as_str())
            );
        }
    }
    println!("  Total: {} equations, {} scalars", count, total_sc);
}

pub(super) fn print_compiled_debug_with_limit(
    dae: &Dae,
    flat: &rumoca_ir_flat::Model,
    eq_limit: usize,
) {
    println!(
        "Success! {}",
        rumoca_eval_dae::analysis::balance_detail(dae)
    );
    println!(
        "active_discrete_scalar_count = {}",
        active_discrete_scalar_count(flat, dae)
    );
    println!("raw interface_flow_count = {}", dae.interface_flow_count);
    println!(
        "raw overconstrained_interface_count = {}",
        dae.overconstrained_interface_count
    );
    print_flat_variables(flat);
    print_dae_variables(dae);
    print_dae_equations(dae, eq_limit);
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
    println!("\n=== MSL Introspection: {} ===", name);
    print_compiled_debug_with_limit(&result.dae, &result.flat, msl_introspect_eq_limit());
    println!("=== End Introspection: {} ===", name);
}
