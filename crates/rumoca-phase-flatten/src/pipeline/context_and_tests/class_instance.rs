//! Class instance lowering: equations, initial equations, and algorithm
//! sections of one instantiated class become flat model entries with import
//! scopes, override rewriting, and reference scopes applied.

use super::*;

/// Process a class instance to extract equations and algorithms.
pub(crate) fn process_class_instance(
    ctx: &mut Context,
    flat: &mut Model,
    class_data: &ClassInstanceData,
    class_def_id: Option<rumoca_core::DefId>,
    component_override_map: &ComponentOverrideMap,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
) -> Result<(), FlattenError> {
    let previous_class_scope = ctx.current_class_scope_path.clone();
    ctx.current_class_scope_path = class_def_id.and_then(|id| tree.def_map.get(&id).cloned());
    let result = process_class_instance_body(
        ctx,
        flat,
        class_data,
        component_override_map,
        tree,
        class_index,
    );
    ctx.current_class_scope_path = previous_class_scope;
    result
}

// SPEC_0021: Exception - top-level flatten phase entry point for a class instance.
#[allow(clippy::too_many_lines)]
fn process_class_instance_body(
    ctx: &mut Context,
    flat: &mut Model,
    class_data: &ClassInstanceData,
    component_override_map: &ComponentOverrideMap,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
) -> Result<(), FlattenError> {
    let prefix = &class_data.qualified_name;
    let def_map = Some(&tree.def_map);
    let class_scope = class_data.qualified_name.to_component_path();
    let (override_packages, override_functions) =
        override_context_for_component_path(&class_scope, component_override_map);
    let override_package_names = override_package_names(&override_packages);
    let override_aliases =
        override_aliases_for_component_path(&class_scope, component_override_map);
    let owner_instance_path = ctx.algorithm_owner_instance_path(class_data)?;

    // Convert regular equations.
    for inst_eq in &class_data.equations {
        set_class_instance_imports_for_scope(
            ctx,
            class_data,
            tree,
            class_index,
            ImportScope {
                source_scope: inst_eq.source_scope.as_ref(),
                source_scope_id: inst_eq.source_scope_id,
                span: inst_eq.span,
            },
            &override_package_names,
            &override_aliases,
        )?;
        let inst_eq = mark_member_function_calls_in_instance_equation(
            inst_eq,
            tree,
            class_index,
            &override_functions,
        );
        // Handle when-equations separately (pass context for parameter evaluation).
        let chain = when_equations::flatten_when_equation(ctx, &inst_eq, prefix, def_map)?;
        if let Some(mut chain) = chain {
            attach_when_chain_reference_scopes(&mut chain, class_data.instance_id)?;
            rewrite_function_overrides_in_when_chain(
                &mut chain,
                tree,
                class_index,
                &override_packages,
                &override_functions,
            )?;
            flat.when_chains.push(chain);
        }

        // Handle other equations (including for-loops that may contain when-equations).
        let mut flattened =
            equations::flatten_equation_with_def_map(ctx, &inst_eq, prefix, def_map)?;
        attach_equation_reference_scopes(&mut flattened, class_data.instance_id)?;
        rewrite_function_overrides_in_flattened(
            &mut flattened,
            tree,
            class_index,
            &override_packages,
            &override_functions,
        )?;
        let equation_base = flat.equations.len();
        for eq in flattened.equations {
            flat.add_equation(eq);
        }
        for mut for_eq in flattened.structured_equations {
            for_eq.first_equation_index += equation_base;
            flat.add_structured_equation(for_eq);
        }
        flat.assert_equations.extend(flattened.assert_equations);
        flat.when_chains.extend(flattened.when_chains);
        flat.definite_roots.extend(flattened.definite_roots);
        flat.branches.extend(flattened.branches);
        flat.potential_roots.extend(flattened.potential_roots);
    }

    // Convert initial equations (when-equations are rejected per EQN-006).
    for inst_eq in &class_data.initial_equations {
        set_class_instance_imports_for_scope(
            ctx,
            class_data,
            tree,
            class_index,
            ImportScope {
                source_scope: inst_eq.source_scope.as_ref(),
                source_scope_id: inst_eq.source_scope_id,
                span: inst_eq.span,
            },
            &override_package_names,
            &override_aliases,
        )?;
        let inst_eq = mark_member_function_calls_in_instance_equation(
            inst_eq,
            tree,
            class_index,
            &override_functions,
        );
        if matches!(&inst_eq.equation, rumoca_ir_ast::Equation::When(_)) {
            return Err(FlattenError::unsupported_equation(
                "when-equations are not allowed in initial equations (MLS §8.6)",
                inst_eq.span,
            ));
        }

        let mut flattened =
            equations::flatten_equation_with_def_map(ctx, &inst_eq, prefix, def_map)?;
        attach_equation_reference_scopes(&mut flattened, class_data.instance_id)?;
        rewrite_function_overrides_in_flattened(
            &mut flattened,
            tree,
            class_index,
            &override_packages,
            &override_functions,
        )?;
        let equation_base = flat.initial_equations.len();
        for eq in flattened.equations {
            flat.add_initial_equation(eq);
        }
        for mut for_eq in flattened.structured_equations {
            for_eq.first_equation_index += equation_base;
            flat.add_initial_structured_equation(for_eq);
        }
        flat.initial_assert_equations
            .extend(flattened.assert_equations);
        if !flattened.when_chains.is_empty() {
            return Err(FlattenError::unsupported_equation(
                "when-equations are not allowed in initial equations (MLS §8.6)",
                inst_eq.span,
            ));
        }
    }

    // Convert algorithms (preserve structure per SPEC_0007 / MLS §11)
    for inst_algs in &class_data.algorithms {
        set_class_instance_imports_for_statement_block(
            ctx,
            class_data,
            tree,
            class_index,
            inst_algs,
            &override_package_names,
            &override_aliases,
        )?;
        let imports = &ctx.current_imports;
        let inst_algs = mark_member_function_calls_in_instance_statements(
            inst_algs,
            tree,
            class_index,
            &override_functions,
        );
        let instance_name = ctx.instance_name_for_prefix(prefix);
        let mut flat_alg = flatten_algorithm_section(
            &inst_algs,
            prefix,
            imports,
            &tree.source_map,
            instance_name.as_deref(),
            ctx.predefined_string_declaration,
            ast_lower::PredefinedIntrinsicIds::from_tree(tree),
        )?;
        algorithms::qualify_write_targets_with_owner_path(&mut flat_alg, &owner_instance_path)?;
        attach_statement_reference_scopes(&mut flat_alg.statements, class_data.instance_id)?;
        rewrite_function_overrides_in_algorithm(
            &mut flat_alg,
            tree,
            class_index,
            &override_packages,
            &override_functions,
        )?;
        flat.algorithms.push(flat_alg);
    }

    // Convert initial algorithms
    for inst_algs in &class_data.initial_algorithms {
        set_class_instance_imports_for_statement_block(
            ctx,
            class_data,
            tree,
            class_index,
            inst_algs,
            &override_package_names,
            &override_aliases,
        )?;
        let imports = &ctx.current_imports;
        let inst_algs = mark_member_function_calls_in_instance_statements(
            inst_algs,
            tree,
            class_index,
            &override_functions,
        );
        let instance_name = ctx.instance_name_for_prefix(prefix);
        let mut flat_alg = flatten_algorithm_section(
            &inst_algs,
            prefix,
            imports,
            &tree.source_map,
            instance_name.as_deref(),
            ctx.predefined_string_declaration,
            ast_lower::PredefinedIntrinsicIds::from_tree(tree),
        )?;
        algorithms::qualify_write_targets_with_owner_path(&mut flat_alg, &owner_instance_path)?;
        attach_statement_reference_scopes(&mut flat_alg.statements, class_data.instance_id)?;
        rewrite_function_overrides_in_algorithm(
            &mut flat_alg,
            tree,
            class_index,
            &override_packages,
            &override_functions,
        )?;
        flat.initial_algorithms.push(flat_alg);
    }

    Ok(())
}

fn attach_equation_reference_scopes(
    flattened: &mut equations::FlattenedEquations,
    instance_scope: rumoca_core::InstanceId,
) -> Result<(), FlattenError> {
    for equation in &mut flattened.equations {
        attach_reference_scope(&mut equation.residual, instance_scope)?;
    }
    for family in &mut flattened.structured_equations {
        if let Some(template) = family.template.as_mut() {
            for expression in &mut template.body {
                attach_reference_scope(expression, instance_scope)?;
            }
        }
    }
    for assertion in &mut flattened.assert_equations {
        attach_reference_scope(&mut assertion.condition, instance_scope)?;
        attach_reference_scope(&mut assertion.message, instance_scope)?;
        if let Some(level) = assertion.level.as_mut() {
            attach_reference_scope(level, instance_scope)?;
        }
    }
    for chain in &mut flattened.when_chains {
        attach_when_chain_reference_scopes(chain, instance_scope)?;
    }
    Ok(())
}

/// Flatten an algorithm section.
///
/// Per SPEC_0007 / MLS §11: algorithms are preserved as structured statements,
/// with variable names qualified and outputs identified.
pub(crate) fn flatten_algorithm_section(
    statements: &[InstanceStatement],
    prefix: &QualifiedName,
    imports: &qualify::ImportMap,
    source_map: &rumoca_core::SourceMap,
    instance_name: Option<&str>,
    predefined_string_declaration: Option<rumoca_core::DefId>,
    predefined_intrinsics: ast_lower::PredefinedIntrinsicIds,
) -> Result<Algorithm, FlattenError> {
    let span = statements
        .iter()
        .map(|statement| statement.span)
        .find(|span| !span.is_dummy())
        .ok_or_else(|| {
            FlattenError::missing_source_context(format!(
                "algorithm section for `{}` has no statement source span",
                prefix.to_flat_string()
            ))
        })?;

    // Extract raw statements from InstanceStatements
    let raw_statements: Vec<_> = statements.iter().map(|s| s.statement.clone()).collect();

    let origin = format!("algorithm from {}", prefix.to_flat_string());
    let no_locals: std::collections::HashSet<String> = std::collections::HashSet::new();

    // Use the algorithms module for qualification and output extraction
    algorithms::flatten_algorithm_section(
        &raw_statements,
        algorithms::AlgorithmSectionContext {
            prefix,
            imports,
            initial_locals: &no_locals,
            source_map: Some(source_map),
            instance_name,
            predefined_string_declaration,
            predefined_intrinsics,
        },
        algorithms::AlgorithmSectionMetadata::new(span, origin),
    )
}
