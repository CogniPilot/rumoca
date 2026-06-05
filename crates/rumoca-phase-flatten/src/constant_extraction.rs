use super::*;

pub(super) fn resolve_constants_from_tree(
    tree: &ast::ClassTree,
    eval_ctx: &mut rumoca_eval_flat::constant::EvalContext,
) -> Result<(), FlattenError> {
    // Well-known constant packages to resolve.
    // ModelicaServices.Machine must come first since Modelica.Constants.eps
    // references ModelicaServices.Machine.eps.
    const CONSTANT_PACKAGES: &[&str] = &["ModelicaServices.Machine", "Modelica.Constants"];

    for &pkg_name in CONSTANT_PACKAGES {
        let Some(class_def) = tree.get_class_by_qualified_name(pkg_name) else {
            continue;
        };
        for (comp_name, component) in &class_def.components {
            let qualified = format!("{}.{}", pkg_name, comp_name);
            if eval_ctx.get(&qualified).is_some() {
                continue;
            }
            let Some(binding) = &component.binding else {
                continue;
            };
            let flat_binding = qualify_expression(binding, &ast::QualifiedName::new())?;
            let Ok(val) = rumoca_eval_flat::constant::eval_expr(&flat_binding, eval_ctx) else {
                continue;
            };
            eval_ctx.add_parameter(qualified, val);
        }
    }
    Ok(())
}

pub(super) fn inject_referenced_qualified_class_constants(
    tree: &ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    model_name: &str,
    flat: &Model,
    overlay: &InstanceOverlay,
    ctx: &mut Context,
) -> Result<(), FlattenError> {
    inject_well_known_constant_values_from_tree(tree, ctx)?;
    const WELL_KNOWN_CONSTANT_PACKAGES: &[&str] =
        &["ModelicaServices.Machine", "Modelica.Constants"];
    const MAX_PASSES: usize = 4;
    let live_vars: HashSet<String> = flat
        .variables
        .keys()
        .map(|name| name.as_str().to_string())
        .collect();

    for _ in 0..MAX_PASSES {
        let prev = context_constant_footprint(ctx);
        let mut scopes = HashSet::new();
        collect_referenced_class_scopes(flat, &live_vars, &tree.def_map, &mut scopes);
        collect_dimension_referenced_class_scopes(overlay, &live_vars, &tree.def_map, &mut scopes)?;
        collect_context_constant_class_scopes(ctx, &live_vars, &tree.def_map, &mut scopes);
        for package in WELL_KNOWN_CONSTANT_PACKAGES {
            scopes.insert((*package).to_string());
        }

        for scope in &scopes {
            let resolved = resolve_referenced_scope_class(class_index, scope, model_name);
            let Some((resolved_scope, class_def)) = resolved else {
                continue;
            };
            let resolve_context = class_def
                .def_id
                .and_then(|id| {
                    tree.def_map
                        .get(&id)
                        .cloned()
                        .or_else(|| class_index.qualified_name(id).map(str::to_string))
                })
                .unwrap_or(resolved_scope);
            inject_class_import_enum_aliases(class_index, scope, class_def, ctx);
            extract_constants_from_class_with_prefix_and_imports(
                tree,
                class_index,
                scope,
                class_def,
                &resolve_context,
                ctx,
            );
            extract_referenced_nested_class_constants_with_prefix(
                tree,
                class_index,
                class_def,
                scope,
                &resolve_context,
                &scopes,
                ctx,
            );
            for ext in &class_def.extends {
                apply_extends_constants_for_scope(
                    tree,
                    class_index,
                    scope,
                    ext,
                    &resolve_context,
                    ctx,
                );
            }
        }

        if context_constant_footprint(ctx) == prev {
            break;
        }
    }
    Ok(())
}

fn inject_well_known_constant_values_from_tree(
    tree: &ClassTree,
    ctx: &mut Context,
) -> Result<(), FlattenError> {
    let mut eval_ctx = equations::build_eval_context(ctx, None);
    resolve_constants_from_tree(tree, &mut eval_ctx)?;
    for (name, value) in eval_ctx.parameters {
        if !is_well_known_constant_value_name(&name) {
            continue;
        }
        insert_evaluated_constant_value(ctx, name, value)?;
    }
    Ok(())
}

fn is_well_known_constant_value_name(name: &str) -> bool {
    let path = rumoca_core::ComponentPath::from_flat_path(name);
    path.starts_with(&rumoca_core::ComponentPath::from_flat_path(
        "ModelicaServices.Machine",
    )) || path.starts_with(&rumoca_core::ComponentPath::from_flat_path(
        "Modelica.Constants",
    ))
}

fn insert_evaluated_constant_value(
    ctx: &mut Context,
    name: String,
    value: rumoca_eval_flat::constant::Value,
) -> Result<(), FlattenError> {
    match value {
        rumoca_eval_flat::constant::Value::Integer(value) => {
            insert_unique_constant(&mut ctx.parameter_values, name, value)?;
        }
        rumoca_eval_flat::constant::Value::Real(value) if value.is_finite() => {
            insert_unique_constant(&mut ctx.real_parameter_values, name, value)?;
        }
        rumoca_eval_flat::constant::Value::Bool(value) => {
            insert_unique_constant(&mut ctx.boolean_parameter_values, name, value)?;
        }
        rumoca_eval_flat::constant::Value::Enum(type_name, literal) => {
            insert_unique_constant(
                &mut ctx.enum_parameter_values,
                name,
                format!("{type_name}.{literal}"),
            )?;
        }
        _ => {}
    }
    Ok(())
}

fn insert_unique_constant<T>(
    values: &mut rustc_hash::FxHashMap<String, T>,
    name: String,
    value: T,
) -> Result<(), FlattenError>
where
    T: PartialEq,
{
    if let Some(existing) = values.get(&name) {
        if existing == &value {
            return Ok(());
        }
        return Err(FlattenError::internal(format!(
            "conflicting evaluated value for package constant {name}"
        )));
    }
    values.insert(name, value);
    Ok(())
}

pub(super) fn collect_dimension_referenced_class_scopes(
    overlay: &InstanceOverlay,
    live_vars: &HashSet<String>,
    def_map: &crate::ResolveDefMap,
    scopes: &mut HashSet<String>,
) -> Result<(), FlattenError> {
    for instance_data in overlay.components.values() {
        for subscript in &instance_data.dims_expr {
            let ast::Subscript::Expression(expr) = subscript else {
                continue;
            };
            let lowered = ast_lower::expression_from_ast_with_def_map(expr, Some(def_map))?;
            collect_expression_class_scopes(&lowered, live_vars, def_map, scopes);
        }
    }
    Ok(())
}

pub(super) fn resolve_referenced_scope_class<'a>(
    class_index: &'a ast::ClassDefIndex<'a>,
    scope: &str,
    model_name: &str,
) -> Option<(String, &'a ClassDef)> {
    if let Some(class_def) = class_index.get_by_qualified_name(scope) {
        return Some((scope.to_string(), class_def));
    }

    let (class_def, resolved_name) = resolve_class_in_scope_indexed(class_index, scope, model_name);
    let class_def = class_def?;
    Some((
        resolved_name.unwrap_or_else(|| scope.to_string()),
        class_def,
    ))
}

pub(super) fn collect_context_constant_class_scopes(
    ctx: &Context,
    live_vars: &HashSet<String>,
    def_map: &crate::ResolveDefMap,
    scopes: &mut HashSet<String>,
) {
    for value in ctx.constant_values.values() {
        collect_expression_class_scopes(value, live_vars, def_map, scopes);
    }
    for value in ctx.enum_parameter_values.values() {
        maybe_add_referenced_class_scope(value, live_vars, scopes);
    }
}

pub(super) fn inject_class_import_enum_aliases(
    class_index: &ast::ClassDefIndex<'_>,
    scope: &str,
    class_def: &ClassDef,
    ctx: &mut Context,
) {
    for import in &class_def.imports {
        let ast::Import::Renamed { alias, path, .. } = import else {
            continue;
        };
        let target_path = path.to_string();
        let Some(enum_class) = class_index.get_by_qualified_name(&target_path) else {
            continue;
        };
        if enum_class.enum_literals.is_empty() {
            continue;
        }
        for literal in &enum_class.enum_literals {
            let lit = literal.ident.text.as_ref();
            let key = format!("{scope}.{}.{}", alias.text, lit);
            let value = format!("{target_path}.{lit}");
            ctx.enum_parameter_values
                .entry(key)
                .or_insert_with(|| value.clone());
        }
    }
}

pub(super) fn context_constant_footprint(ctx: &Context) -> usize {
    ctx.parameter_values.len()
        + ctx.real_parameter_values.len()
        + ctx.boolean_parameter_values.len()
        + ctx.enum_parameter_values.len()
        + ctx.constant_values.len()
        + ctx.array_dimensions.len()
}

pub(super) fn collect_referenced_class_scopes(
    flat: &Model,
    live_vars: &HashSet<String>,
    def_map: &crate::ResolveDefMap,
    scopes: &mut HashSet<String>,
) {
    for eq in &flat.equations {
        collect_expression_class_scopes(&eq.residual, live_vars, def_map, scopes);
    }
    for eq in &flat.initial_equations {
        collect_expression_class_scopes(&eq.residual, live_vars, def_map, scopes);
    }

    for when in &flat.when_clauses {
        collect_expression_class_scopes(&when.condition, live_vars, def_map, scopes);
        for eq in &when.equations {
            collect_when_equation_class_scopes(eq, live_vars, def_map, scopes);
        }
    }

    for alg in &flat.algorithms {
        for stmt in &alg.statements {
            collect_statement_class_scopes(stmt, live_vars, def_map, scopes);
        }
    }

    for var in flat.variables.values() {
        if let Some(binding) = &var.binding {
            collect_expression_class_scopes(binding, live_vars, def_map, scopes);
        }
        if let Some(start) = &var.start {
            collect_expression_class_scopes(start, live_vars, def_map, scopes);
        }
        if let Some(min) = &var.min {
            collect_expression_class_scopes(min, live_vars, def_map, scopes);
        }
        if let Some(max) = &var.max {
            collect_expression_class_scopes(max, live_vars, def_map, scopes);
        }
        if let Some(nominal) = &var.nominal {
            collect_expression_class_scopes(nominal, live_vars, def_map, scopes);
        }
    }

    for function in flat.functions.values() {
        for param in function
            .inputs
            .iter()
            .chain(function.outputs.iter())
            .chain(function.locals.iter())
        {
            if let Some(default_expr) = &param.default {
                collect_expression_class_scopes(default_expr, live_vars, def_map, scopes);
            }
        }
        for statement in &function.body {
            collect_statement_class_scopes(statement, live_vars, def_map, scopes);
        }
    }
}

pub(super) fn collect_expression_class_scopes(
    expr: &Expression,
    live_vars: &HashSet<String>,
    def_map: &crate::ResolveDefMap,
    scopes: &mut HashSet<String>,
) {
    let mut refs = HashSet::new();
    expr.collect_var_refs(&mut refs);
    for name in refs {
        maybe_add_referenced_class_scope(name.as_str(), live_vars, scopes);
    }
    let mut target_defs = TargetDefClassScopeCollector {
        def_map,
        live_vars,
        scopes,
    };
    target_defs.visit_expression(expr);
    collect_constructor_class_scopes(expr, live_vars, scopes);
}

pub(super) struct TargetDefClassScopeCollector<'a> {
    def_map: &'a crate::ResolveDefMap,
    live_vars: &'a HashSet<String>,
    scopes: &'a mut HashSet<String>,
}

impl ExpressionVisitor for TargetDefClassScopeCollector<'_> {
    fn visit_var_ref(&mut self, name: &Reference, subscripts: &[Subscript]) {
        if let Some(target_name) = name
            .target_def_id()
            .and_then(|def_id| self.def_map.get(&def_id))
        {
            maybe_add_referenced_class_scope(target_name, self.live_vars, self.scopes);
        }
        self.walk_var_ref(name, subscripts);
    }

    fn visit_function_call(&mut self, name: &Reference, args: &[Expression], is_constructor: bool) {
        if let Some(target_name) = name
            .target_def_id()
            .and_then(|def_id| self.def_map.get(&def_id))
        {
            maybe_add_referenced_class_scope(target_name, self.live_vars, self.scopes);
        }
        self.walk_function_call(name, args, is_constructor);
    }
}

pub(super) fn collect_constructor_class_scopes(
    expr: &Expression,
    live_vars: &HashSet<String>,
    scopes: &mut HashSet<String>,
) {
    match expr {
        Expression::FunctionCall {
            name,
            args,
            is_constructor,
            ..
        } => {
            if *is_constructor {
                let constructor_name = name.as_str();
                if constructor_name != "time" && !live_vars.contains(constructor_name) {
                    scopes.insert(constructor_name.to_string());
                }
            }
            for arg in args {
                collect_constructor_class_scopes(arg, live_vars, scopes);
            }
        }
        Expression::BuiltinCall { args, .. } => {
            for arg in args {
                collect_constructor_class_scopes(arg, live_vars, scopes);
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            collect_constructor_class_scopes(lhs, live_vars, scopes);
            collect_constructor_class_scopes(rhs, live_vars, scopes);
        }
        Expression::Unary { rhs, .. } => {
            collect_constructor_class_scopes(rhs, live_vars, scopes);
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (cond, value) in branches {
                collect_constructor_class_scopes(cond, live_vars, scopes);
                collect_constructor_class_scopes(value, live_vars, scopes);
            }
            collect_constructor_class_scopes(else_branch, live_vars, scopes);
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements, .. } => {
            for element in elements {
                collect_constructor_class_scopes(element, live_vars, scopes);
            }
        }
        Expression::Range {
            start, step, end, ..
        } => {
            collect_constructor_class_scopes(start, live_vars, scopes);
            if let Some(step) = step {
                collect_constructor_class_scopes(step, live_vars, scopes);
            }
            collect_constructor_class_scopes(end, live_vars, scopes);
        }
        Expression::Index {
            base, subscripts, ..
        } => {
            collect_constructor_class_scopes(base, live_vars, scopes);
            for subscript in subscripts {
                if let Subscript::Expr { expr, .. } = subscript {
                    collect_constructor_class_scopes(expr, live_vars, scopes);
                }
            }
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            collect_constructor_class_scopes(expr, live_vars, scopes);
            for index in indices {
                collect_constructor_class_scopes(&index.range, live_vars, scopes);
            }
            if let Some(filter_expr) = filter {
                collect_constructor_class_scopes(filter_expr, live_vars, scopes);
            }
        }
        Expression::FieldAccess { base, .. } => {
            collect_constructor_class_scopes(base, live_vars, scopes);
        }
        _ => {}
    }
}

pub(super) fn collect_when_equation_class_scopes(
    eq: &flat::WhenEquation,
    live_vars: &HashSet<String>,
    def_map: &crate::ResolveDefMap,
    scopes: &mut HashSet<String>,
) {
    match eq {
        flat::WhenEquation::Assign { value, .. } | flat::WhenEquation::Reinit { value, .. } => {
            collect_expression_class_scopes(value, live_vars, def_map, scopes)
        }
        flat::WhenEquation::Assert { condition, .. } => {
            collect_expression_class_scopes(condition, live_vars, def_map, scopes)
        }
        flat::WhenEquation::Terminate { .. } => {}
        flat::WhenEquation::Conditional {
            branches,
            else_branch,
            ..
        } => {
            for (cond, eqs) in branches {
                collect_expression_class_scopes(cond, live_vars, def_map, scopes);
                for nested in eqs {
                    collect_when_equation_class_scopes(nested, live_vars, def_map, scopes);
                }
            }
            for nested in else_branch {
                collect_when_equation_class_scopes(nested, live_vars, def_map, scopes);
            }
        }
        flat::WhenEquation::FunctionCallOutputs { function, .. } => {
            collect_expression_class_scopes(function, live_vars, def_map, scopes)
        }
    }
}

pub(super) fn collect_statement_class_scopes(
    stmt: &rumoca_core::Statement,
    live_vars: &HashSet<String>,
    def_map: &crate::ResolveDefMap,
    scopes: &mut HashSet<String>,
) {
    match stmt {
        rumoca_core::Statement::Assignment { value, .. } => {
            collect_expression_class_scopes(value, live_vars, def_map, scopes)
        }
        rumoca_core::Statement::For {
            indices, equations, ..
        } => {
            for idx in indices {
                collect_expression_class_scopes(&idx.range, live_vars, def_map, scopes);
            }
            for nested in equations {
                collect_statement_class_scopes(nested, live_vars, def_map, scopes);
            }
        }
        rumoca_core::Statement::While { block, .. } => {
            collect_expression_class_scopes(&block.cond, live_vars, def_map, scopes);
            for nested in &block.stmts {
                collect_statement_class_scopes(nested, live_vars, def_map, scopes);
            }
        }
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            for block in cond_blocks {
                collect_expression_class_scopes(&block.cond, live_vars, def_map, scopes);
                for nested in &block.stmts {
                    collect_statement_class_scopes(nested, live_vars, def_map, scopes);
                }
            }
            if let Some(else_block) = else_block {
                for nested in else_block {
                    collect_statement_class_scopes(nested, live_vars, def_map, scopes);
                }
            }
        }
        rumoca_core::Statement::When { blocks, .. } => {
            for block in blocks {
                collect_expression_class_scopes(&block.cond, live_vars, def_map, scopes);
                for nested in &block.stmts {
                    collect_statement_class_scopes(nested, live_vars, def_map, scopes);
                }
            }
        }
        rumoca_core::Statement::FunctionCall { args, .. } => {
            for arg in args {
                collect_expression_class_scopes(arg, live_vars, def_map, scopes);
            }
        }
        rumoca_core::Statement::Reinit { value, .. } => {
            collect_expression_class_scopes(value, live_vars, def_map, scopes)
        }
        rumoca_core::Statement::Assert {
            condition,
            message,
            level,
            ..
        } => {
            collect_expression_class_scopes(condition, live_vars, def_map, scopes);
            collect_expression_class_scopes(message, live_vars, def_map, scopes);
            if let Some(level) = level {
                collect_expression_class_scopes(level, live_vars, def_map, scopes);
            }
        }
        rumoca_core::Statement::Empty { .. }
        | rumoca_core::Statement::Return { .. }
        | rumoca_core::Statement::Break { .. } => {}
    }
}

pub(super) fn maybe_add_referenced_class_scope(
    name: &str,
    live_vars: &HashSet<String>,
    scopes: &mut HashSet<String>,
) {
    if name == "time" || live_vars.contains(name) {
        return;
    }

    let Some(scope) = path_utils::parent_scope(name) else {
        return;
    };
    let last = path_utils::top_level_last_segment(name);
    let base_last = path_utils::strip_array_index(last);
    if base_last.is_empty() {
        return;
    }

    let normalized_name = format!("{scope}.{base_last}");
    if live_vars.contains(&normalized_name) {
        return;
    }

    let mut current = Some(scope);
    while let Some(candidate) = current {
        scopes.insert(candidate.to_string());
        current = path_utils::parent_scope(candidate);
    }
}

/// Inject constants from the enclosing class into the flatten context (MLS §7.3).
///
/// When compiling a nested model like `Modelica.Media.Air.MoistAir.BaseProperties`,
/// constants from the enclosing package (e.g., `nX`, `nXi`, `nS`) must be available
/// for for-equation range evaluation. This walks the enclosing class's extends chain
/// to collect all inherited constants.
/// Inject constants from nested class declarations in the model being compiled (MLS §7.3).
///
/// When a model has `package Medium = SomeMedium`, constants like nX, nXi, nS
/// from that package need to be available for for-equation range evaluation.
/// Walks the model's ancestor chain and extracts constants from each ancestor's
/// nested class declarations (following extends chains to concrete types).
pub(super) fn inject_model_nested_class_constants(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    model_name: &str,
    ctx: &mut Context,
) {
    let model_ancestors = collect_ancestor_classes_with_index(tree, class_index, model_name);
    if model_ancestors.is_empty() {
        return;
    }
    const MAX_PASSES: usize = 5;
    for _pass in 0..MAX_PASSES {
        let prev = ctx.parameter_values.len()
            + ctx.array_dimensions.len()
            + ctx.boolean_parameter_values.len();
        for ancestor in &model_ancestors {
            extract_nested_class_constants_with_prefix(
                tree,
                class_index,
                ancestor,
                model_name,
                model_name,
                ctx,
            );
        }
        let new = ctx.parameter_values.len()
            + ctx.array_dimensions.len()
            + ctx.boolean_parameter_values.len();
        if new == prev {
            break;
        }
    }
}

/// Inject constants from direct model-level extends redeclare package overrides.
///
/// MLS §7.3: `extends(... redeclare package Alias = Pkg)` defines the effective
/// package alias in the derived model scope. Flatten-time constant lookup for
/// expressions like `Alias.nXi` must observe this effective package.
pub(super) fn inject_model_extends_redeclare_constants(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    model_name: &str,
    ctx: &mut Context,
) {
    let model_class = class_index.get_by_qualified_name(model_name).or_else(|| {
        class_index.get_by_qualified_name(crate::path_utils::top_level_last_segment(model_name))
    });
    let Some(model_class) = model_class else {
        return;
    };

    let resolve_context = model_class
        .def_id
        .and_then(|id| tree.def_map.get(&id).map(String::as_str))
        .unwrap_or(model_name);
    let redeclare_packages =
        collect_model_redeclare_packages(tree, class_index, model_class, resolve_context);
    if redeclare_packages.is_empty() {
        return;
    }

    const MAX_PASSES: usize = 5;
    for _pass in 0..MAX_PASSES {
        let prev = ctx.parameter_values.len()
            + ctx.array_dimensions.len()
            + ctx.boolean_parameter_values.len()
            + ctx.enum_parameter_values.len();

        for (alias_name, package_class, package_context) in &redeclare_packages {
            extract_constants_from_class_with_prefix(alias_name, package_class, ctx);
            for pkg_ext in &package_class.extends {
                apply_extends_constants_for_scope(
                    tree,
                    class_index,
                    alias_name,
                    pkg_ext,
                    package_context,
                    ctx,
                );
            }
        }

        let new = ctx.parameter_values.len()
            + ctx.array_dimensions.len()
            + ctx.boolean_parameter_values.len()
            + ctx.enum_parameter_values.len();
        if new == prev {
            break;
        }
    }
}

/// Collect direct model-level `extends(... redeclare package Alias = Pkg)` entries.
pub(super) fn collect_model_redeclare_packages<'a>(
    tree: &'a ast::ClassTree,
    class_index: &'a ast::ClassDefIndex<'a>,
    model_class: &'a ast::ClassDef,
    resolve_context: &str,
) -> Vec<(String, &'a ast::ClassDef, String)> {
    let mut entries = Vec::new();
    for ext in &model_class.extends {
        for ext_mod in &ext.modifications {
            if let Some(entry) =
                resolve_model_redeclare_package_entry(tree, class_index, resolve_context, ext_mod)
            {
                entries.push(entry);
            }
        }
    }
    entries
}

/// Resolve one model-level redeclare modifier to `(alias, package_class, package_context)`.
pub(super) fn resolve_model_redeclare_package_entry<'a>(
    tree: &'a ast::ClassTree,
    class_index: &'a ast::ClassDefIndex<'a>,
    resolve_context: &str,
    ext_mod: &rumoca_ir_ast::ExtendModification,
) -> Option<(String, &'a ast::ClassDef, String)> {
    if !ext_mod.redeclare {
        return None;
    }

    let ast::Expression::Modification { target, value, .. } = &ext_mod.expr else {
        return None;
    };
    if target.parts.len() != 1 {
        return None;
    }

    let alias_name = target.parts[0].ident.text.to_string();
    if !alias_name.starts_with(char::is_uppercase) {
        return None;
    }

    let package_name = redeclare_package_name(value)?;
    let package_class = resolve_class_in_scope_indexed(class_index, &package_name, resolve_context)
        .0
        .or_else(|| class_index.get_by_qualified_name(&package_name))?;
    if !matches!(package_class.class_type, rumoca_core::ClassType::Package) {
        return None;
    }

    let package_context = package_class
        .def_id
        .and_then(|id| tree.def_map.get(&id).cloned())
        .unwrap_or(package_name);
    Some((alias_name, package_class, package_context))
}

/// Extract the replacement package name from a redeclare modifier expression.
pub(super) fn redeclare_package_name(value: &ast::Expression) -> Option<String> {
    match value {
        ast::Expression::ClassModification { target, .. } => Some(target.to_string()),
        ast::Expression::ComponentReference(cr) => Some(cr.to_string()),
        _ => None,
    }
}

/// Extract constants from nested class declarations of a class definition.
/// For each nested class (e.g., `package Medium = ...`), follow extends chains
/// and extract integer constants and array dimensions.
pub(super) fn extract_nested_class_constants(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    class_def: &ast::ClassDef,
    resolve_context: &str,
    ctx: &mut Context,
) {
    extract_nested_class_constants_with_prefix(
        tree,
        class_index,
        class_def,
        resolve_context,
        resolve_context,
        ctx,
    );
}

pub(super) fn extract_nested_class_constants_with_prefix(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    class_def: &ast::ClassDef,
    prefix: &str,
    resolve_context: &str,
    ctx: &mut Context,
) {
    for (nested_name, nested_class) in &class_def.classes {
        if !matches!(
            nested_class.class_type,
            rumoca_core::ClassType::Package | rumoca_core::ClassType::Record
        ) {
            continue;
        }
        let nested_prefix = make_prefixed_name(prefix, nested_name);
        extract_constants_from_class_with_prefix_and_imports(
            tree,
            class_index,
            &nested_prefix,
            nested_class,
            resolve_context,
            ctx,
        );
        extract_constants_from_class_with_prefix_and_imports(
            tree,
            class_index,
            nested_name,
            nested_class,
            resolve_context,
            ctx,
        );
        let nested_context = nested_class
            .def_id
            .and_then(|id| tree.def_map.get(&id).map(String::as_str))
            .unwrap_or(resolve_context);
        for ext in &nested_class.extends {
            apply_extends_constants_for_scope(
                tree,
                class_index,
                &nested_prefix,
                ext,
                nested_context,
                ctx,
            );
            apply_extends_constants_for_scope(
                tree,
                class_index,
                nested_name,
                ext,
                nested_context,
                ctx,
            );
        }
        extract_nested_class_constants_with_prefix(
            tree,
            class_index,
            nested_class,
            &nested_prefix,
            nested_context,
            ctx,
        );
    }
}

pub(super) fn extract_referenced_nested_class_constants_with_prefix(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    class_def: &ast::ClassDef,
    prefix: &str,
    resolve_context: &str,
    referenced_scopes: &HashSet<String>,
    ctx: &mut Context,
) {
    let referenced_nested_names = referenced_direct_nested_names(prefix, referenced_scopes);
    for (nested_name, nested_class) in &class_def.classes {
        let nested_prefix = make_prefixed_name(prefix, nested_name);
        if !referenced_scopes.contains(&nested_prefix) {
            continue;
        }
        inject_referenced_nested_class_constants(
            tree,
            class_index,
            nested_class,
            &nested_prefix,
            ctx,
        );
    }

    let mut visited = HashSet::new();
    extract_inherited_referenced_nested_class_constants(
        InheritedNestedClassConstantCtx {
            tree,
            class_index,
            prefix,
            resolve_context,
            referenced_nested_names: &referenced_nested_names,
        },
        class_def,
        &mut visited,
        ctx,
    );
}

pub(super) fn referenced_direct_nested_names(
    prefix: &str,
    referenced_scopes: &HashSet<String>,
) -> HashSet<String> {
    let nested_scope_prefix = format!("{prefix}.");
    referenced_scopes
        .iter()
        .filter_map(|scope| scope.strip_prefix(&nested_scope_prefix))
        .filter_map(|rest| {
            let name = path_utils::split_first_top_level(rest)
                .map(|(first, _rest)| first)
                .unwrap_or(rest);
            (!name.is_empty()).then(|| path_utils::strip_array_index(name).to_string())
        })
        .collect()
}

pub(super) fn inject_referenced_nested_class_constants(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    nested_class: &ast::ClassDef,
    nested_prefix: &str,
    ctx: &mut Context,
) {
    let nested_context = nested_class
        .def_id
        .and_then(|id| tree.def_map.get(&id).map(String::as_str))
        .unwrap_or(nested_prefix);
    extract_constants_from_class_with_prefix_and_imports(
        tree,
        class_index,
        nested_prefix,
        nested_class,
        nested_context,
        ctx,
    );

    for ext in &nested_class.extends {
        apply_extends_constants_for_scope(
            tree,
            class_index,
            nested_prefix,
            ext,
            nested_context,
            ctx,
        );
    }
}

pub(super) struct InheritedNestedClassConstantCtx<'a> {
    tree: &'a ast::ClassTree,
    class_index: &'a ast::ClassDefIndex<'a>,
    prefix: &'a str,
    resolve_context: &'a str,
    referenced_nested_names: &'a HashSet<String>,
}

pub(super) fn extract_inherited_referenced_nested_class_constants(
    constants: InheritedNestedClassConstantCtx<'_>,
    class_def: &ast::ClassDef,
    visited: &mut HashSet<String>,
    ctx: &mut Context,
) {
    for ext in &class_def.extends {
        let (base_class, base_qname) = resolve_class_in_scope_indexed(
            constants.class_index,
            &ext.base_name.to_string(),
            constants.resolve_context,
        );
        let Some(base_class) = base_class else {
            continue;
        };
        let base_qname = base_qname.unwrap_or_else(|| ext.base_name.to_string());
        if !visited.insert(base_qname.clone()) {
            continue;
        }
        for nested_name in constants.referenced_nested_names {
            if class_def.classes.contains_key(nested_name) {
                continue;
            }
            let Some(nested_class) = base_class.classes.get(nested_name) else {
                continue;
            };
            let nested_prefix = make_prefixed_name(constants.prefix, nested_name);
            inject_referenced_nested_class_constants(
                constants.tree,
                constants.class_index,
                nested_class,
                &nested_prefix,
                ctx,
            );
        }
        extract_inherited_referenced_nested_class_constants(
            InheritedNestedClassConstantCtx {
                tree: constants.tree,
                class_index: constants.class_index,
                prefix: constants.prefix,
                resolve_context: &base_qname,
                referenced_nested_names: constants.referenced_nested_names,
            },
            base_class,
            visited,
            ctx,
        );
    }
}

/// Recursively extract constants from an extends chain, using a prefix alias.
/// Uses scope-based resolution for relative extends names.
pub(super) fn extract_extends_chain_constants(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    alias: &str,
    base_name: &str,
    resolve_context: &str,
    ctx: &mut Context,
) {
    let mut visited = std::collections::HashSet::new();
    extract_extends_chain_constants_inner(
        tree,
        class_index,
        alias,
        base_name,
        resolve_context,
        ctx,
        &mut visited,
    );
}

pub(super) fn extract_extends_chain_constants_inner(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    alias: &str,
    base_name: &str,
    resolve_context: &str,
    ctx: &mut Context,
    visited: &mut std::collections::HashSet<String>,
) {
    let (base_class, resolved_qname) =
        resolve_class_in_scope_indexed(class_index, base_name, resolve_context);
    let Some(base_class) = base_class else {
        return;
    };
    let qname = resolved_qname.unwrap_or_else(|| base_name.to_string());
    if !visited.insert(qname.clone()) {
        return;
    }
    extract_constants_from_class_with_prefix(alias, base_class, ctx);
    for ext in &base_class.extends {
        extract_extends_modification_constants(tree, class_index, alias, ext, &qname, ctx);
        if let Some(base_qname) =
            resolve_extends_base_qname(class_index, &ext.base_name.to_string(), &qname)
            && base_qname != alias
        {
            extract_extends_modification_constants(
                tree,
                class_index,
                &base_qname,
                ext,
                &base_qname,
                ctx,
            );
        }
        extract_extends_chain_constants_inner(
            tree,
            class_index,
            alias,
            &ext.base_name.to_string(),
            &qname,
            ctx,
            visited,
        );
    }
}

/// Extract constant-affecting extends modifiers into the flatten context.
///
/// MLS §7.2/§7.3: extends modifiers override inherited constants and must be
/// applied before evaluating dependent constants such as nS/nX/nXi.
pub(super) fn extract_extends_modification_constants(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    prefix: &str,
    ext: &rumoca_ir_ast::Extend,
    resolve_context: &str,
    ctx: &mut Context,
) {
    for ext_mod in &ext.modifications {
        if ext_mod.redeclare {
            continue;
        }
        extract_extends_modification_expr(
            tree,
            class_index,
            prefix,
            &ext_mod.expr,
            resolve_context,
            ctx,
        );
    }
}

/// Apply `redeclare package Alias = SomePackage` extends modifiers by injecting
/// constants from the redeclared package into the current scope.
pub(super) fn extract_extends_redeclare_package_constants(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    prefix: &str,
    ext: &rumoca_ir_ast::Extend,
    resolve_context: &str,
    ctx: &mut Context,
) {
    for ext_mod in &ext.modifications {
        if !ext_mod.redeclare {
            continue;
        }
        let ast::Expression::Modification { target, value, .. } = &ext_mod.expr else {
            continue;
        };
        if target.parts.len() != 1 {
            continue;
        }

        let alias_name = target.parts[0].ident.text.as_ref();
        if !alias_name.starts_with(char::is_uppercase) {
            continue;
        }

        let package_name = match value.as_ref() {
            ast::Expression::ClassModification { target, .. } => target.to_string(),
            ast::Expression::ComponentReference(cr) => cr.to_string(),
            _ => continue,
        };

        let package_class =
            resolve_class_in_scope_indexed(class_index, &package_name, resolve_context)
                .0
                .or_else(|| class_index.get_by_qualified_name(&package_name));
        let Some(package_class) = package_class else {
            continue;
        };
        if !matches!(package_class.class_type, rumoca_core::ClassType::Package) {
            continue;
        }

        let alias_scope = make_prefixed_name(prefix, alias_name);
        extract_constants_from_class_with_prefix(&alias_scope, package_class, ctx);
        extract_constants_from_class_with_prefix(prefix, package_class, ctx);

        for pkg_ext in &package_class.extends {
            extract_extends_modification_constants(
                tree,
                class_index,
                &alias_scope,
                pkg_ext,
                resolve_context,
                ctx,
            );
            extract_extends_chain_constants(
                tree,
                class_index,
                &alias_scope,
                &pkg_ext.base_name.to_string(),
                resolve_context,
                ctx,
            );

            extract_extends_modification_constants(
                tree,
                class_index,
                prefix,
                pkg_ext,
                resolve_context,
                ctx,
            );
            extract_extends_chain_constants(
                tree,
                class_index,
                prefix,
                &pkg_ext.base_name.to_string(),
                resolve_context,
                ctx,
            );
        }
    }
}

/// Walk an extends-modification expression and record scalar/dimension overrides.
pub(super) fn extract_extends_modification_expr(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    prefix: &str,
    expr: &ast::Expression,
    resolve_context: &str,
    ctx: &mut Context,
) {
    if let ast::Expression::Modification { target, value, .. } = expr {
        let target_name = target.to_string();
        let full_name = make_prefixed_name(prefix, &target_name);
        let mut imports = crate::qualify::ImportMap::default();
        crate::qualify::collect_lexical_package_aliases(
            tree,
            class_index,
            resolve_context,
            &mut imports,
        );
        let qualified_value = crate::qualify::qualify_expression_with_imports(
            value,
            &ast::QualifiedName::new(),
            crate::qualify::QualifyOptions::default(),
            &imports,
        );
        let alias_ref = constant_reference_name(&qualified_value);
        // MLS §7.2: extends modifiers override inherited declarations.
        // Preserve explicitly modified keys from later default-constant extraction.
        ctx.modified_constant_keys.insert(full_name.clone());
        extract_extends_numeric_modification(
            ctx,
            prefix,
            &target_name,
            &full_name,
            &qualified_value,
        );
        extract_extends_constant_modification(
            ctx,
            prefix,
            &target_name,
            &full_name,
            &qualified_value,
        );
        extract_extends_shape_and_alias_modification(
            ctx,
            prefix,
            &target_name,
            &full_name,
            &qualified_value,
            alias_ref,
        );
    }
}

fn extract_extends_numeric_modification(
    ctx: &mut Context,
    prefix: &str,
    target_name: &str,
    full_name: &str,
    qualified_value: &ast::Expression,
) {
    if let Some(val) = try_eval_const_integer_with_scope(qualified_value, ctx, prefix) {
        insert_with_prefix(
            &mut ctx.parameter_values,
            prefix,
            target_name,
            full_name,
            val,
        );
    }
    if let Some(val) = try_eval_const_boolean_with_scope(qualified_value, ctx, prefix) {
        insert_with_prefix(
            &mut ctx.boolean_parameter_values,
            prefix,
            target_name,
            full_name,
            val,
        );
    }
    if let Some(val) = try_eval_const_real_with_scope(qualified_value, ctx, prefix)
        && val.is_finite()
    {
        insert_with_prefix(
            &mut ctx.real_parameter_values,
            prefix,
            target_name,
            full_name,
            val,
        );
    }
}

fn extract_extends_constant_modification(
    ctx: &mut Context,
    prefix: &str,
    target_name: &str,
    full_name: &str,
    qualified_value: &ast::Expression,
) {
    if let Some(val) = try_eval_const_flat_expr_with_scope(qualified_value, ctx, prefix) {
        insert_with_prefix(
            &mut ctx.constant_values,
            prefix,
            target_name,
            full_name,
            val,
        );
    } else if let Some(val) =
        try_extract_record_array_constructor_constant(qualified_value, ctx, prefix, full_name)
    {
        insert_with_prefix(
            &mut ctx.constant_values,
            prefix,
            target_name,
            full_name,
            val,
        );
    } else if let Some(val) =
        try_extract_named_record_constructor_constant(qualified_value, ctx, prefix, full_name)
    {
        insert_with_prefix(
            &mut ctx.constant_values,
            prefix,
            target_name,
            full_name,
            val,
        );
    } else if let ast::Expression::ComponentReference(_) = qualified_value
        && let Ok(symbolic) =
            crate::ast_lower::expression_from_ast_with_def_map(qualified_value, None)
    {
        insert_with_prefix(
            &mut ctx.constant_values,
            prefix,
            target_name,
            full_name,
            symbolic,
        );
    }
}

fn extract_extends_shape_and_alias_modification(
    ctx: &mut Context,
    prefix: &str,
    target_name: &str,
    full_name: &str,
    qualified_value: &ast::Expression,
    alias_ref: Option<String>,
) {
    if let Some(val) = try_eval_const_enum_with_scope(qualified_value, ctx, prefix) {
        insert_with_prefix(
            &mut ctx.enum_parameter_values,
            prefix,
            target_name,
            full_name,
            val,
        );
    }
    if let Some(dims) = infer_dims_from_expr(qualified_value, ctx, prefix) {
        insert_with_prefix(
            &mut ctx.array_dimensions,
            prefix,
            target_name,
            full_name,
            dims,
        );
    }
    if let Some(alias_name) = alias_ref {
        materialize_record_constant_alias_fields(ctx, prefix, target_name, full_name, &alias_name);
    }
}

/// Extract integer constants and array dimensions from a class, using a prefix for names.
/// Constants are stored as both `prefix.name` (e.g., `Medium.nX`) and unprefixed `name`.
pub(super) fn extract_constants_from_class_with_prefix(
    prefix: &str,
    class_def: &ast::ClassDef,
    ctx: &mut Context,
) {
    for (name, comp) in &class_def.components {
        if !matches!(
            comp.variability,
            rumoca_core::Variability::Constant(_) | rumoca_core::Variability::Parameter(_)
        ) {
            continue;
        }
        let binding =
            comp.binding
                .as_ref()
                .or(if !matches!(comp.start, ast::Expression::Empty { .. }) {
                    Some(&comp.start)
                } else {
                    None
                });
        let synthesized = if binding.is_none() {
            synthesize_component_modification_binding(comp)
        } else {
            None
        };
        let expr = binding.or(synthesized.as_ref());
        let Some(expr) = expr else { continue };
        let full_name = make_prefixed_name(prefix, name);
        extract_single_constant_with_prefix(prefix, name, &full_name, comp, expr, ctx);
    }
}

pub(super) fn extract_constants_from_class_with_prefix_and_imports(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    prefix: &str,
    class_def: &ast::ClassDef,
    resolve_context: &str,
    ctx: &mut Context,
) {
    let imports = constant_extraction_imports(tree, class_index, resolve_context);
    let empty_prefix = ast::QualifiedName::new();
    let qualify_opts = qualify::QualifyOptions {
        preserve_def_id: true,
        ..qualify::QualifyOptions::default()
    };

    for (name, comp) in &class_def.components {
        if !matches!(
            comp.variability,
            rumoca_core::Variability::Constant(_) | rumoca_core::Variability::Parameter(_)
        ) {
            continue;
        }
        let binding =
            comp.binding
                .as_ref()
                .or(if !matches!(comp.start, ast::Expression::Empty { .. }) {
                    Some(&comp.start)
                } else {
                    None
                });
        let synthesized = if binding.is_none() {
            synthesize_component_modification_binding(comp)
        } else {
            None
        };
        let Some(expr) = binding.or(synthesized.as_ref()) else {
            continue;
        };
        let qualified_expr =
            qualify::qualify_expression_with_imports(expr, &empty_prefix, qualify_opts, &imports);
        let full_name = make_prefixed_name(prefix, name);
        extract_single_constant_with_prefix_and_function_scope(
            prefix,
            name,
            &full_name,
            comp,
            &qualified_expr,
            ctx,
            Some(FunctionCanonicalScope {
                tree,
                class_index,
                source_scope: resolve_context,
            }),
        );
    }
}

pub(super) fn constant_extraction_imports(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    resolve_context: &str,
) -> qualify::ImportMap {
    let mut imports = qualify::ImportMap::default();
    let source_scope = ast::QualifiedName::from_dotted(resolve_context);
    qualify::collect_imports_for_source_scope(class_index, &source_scope, &mut imports);
    qualify::collect_lexical_package_aliases(tree, class_index, resolve_context, &mut imports);
    qualify::collect_lexical_constant_aliases_for_source_scope_with_packages(
        tree,
        class_index,
        &source_scope,
        &[],
        &mut imports,
    );
    imports
}

/// Extract a single constant value (integer or array dims) into the context.
pub(super) fn extract_single_constant_with_prefix(
    prefix: &str,
    name: &str,
    full_name: &str,
    comp: &rumoca_ir_ast::Component,
    expr: &ast::Expression,
    ctx: &mut Context,
) {
    extract_single_constant_with_prefix_and_function_scope(
        prefix, name, full_name, comp, expr, ctx, None,
    );
}

#[derive(Clone, Copy)]
pub(super) struct FunctionCanonicalScope<'a> {
    tree: &'a ast::ClassTree,
    class_index: &'a ast::ClassDefIndex<'a>,
    source_scope: &'a str,
}

pub(super) fn canonicalize_constant_function_calls(
    mut expr: rumoca_core::Expression,
    function_scope: Option<FunctionCanonicalScope<'_>>,
) -> rumoca_core::Expression {
    if let Some(scope) = function_scope {
        functions::canonicalize_function_calls_in_expression_with_scope(
            &mut expr,
            scope.tree,
            scope.class_index,
            Some(scope.source_scope),
        );
    }
    expr
}

pub(super) fn extract_single_constant_with_prefix_and_function_scope(
    prefix: &str,
    name: &str,
    full_name: &str,
    comp: &rumoca_ir_ast::Component,
    expr: &ast::Expression,
    ctx: &mut Context,
    function_scope: Option<FunctionCanonicalScope<'_>>,
) {
    // Modified flat model variables are authoritative for instantiated
    // parameter/constant values. Do not inject declaration defaults for these
    // names; those defaults can override modifier-derived bindings (MLS §7.2.4).
    if ctx.modified_constant_keys.contains(full_name) {
        return;
    }

    let type_name = comp.type_name.to_string();
    let preserve_existing = ctx.flat_parameter_constant_keys.contains(full_name);
    if let Some(val) = try_extract_named_record_constructor_constant(expr, ctx, prefix, full_name)
        && (!preserve_existing || !ctx.constant_values.contains_key(full_name))
    {
        let val = canonicalize_constant_function_calls(val, function_scope);
        insert_with_prefix(&mut ctx.constant_values, prefix, name, full_name, val);
    }
    if is_record_like_type(&type_name)
        && let Some(val) =
            try_extract_record_array_constructor_constant(expr, ctx, prefix, full_name)
        && (!preserve_existing || !ctx.constant_values.contains_key(full_name))
    {
        let val = canonicalize_constant_function_calls(val, function_scope);
        insert_with_prefix(&mut ctx.constant_values, prefix, name, full_name, val);
    }
    // Integer constants
    if type_name == "Integer"
        && let Some(val) = try_eval_const_integer_with_scope(expr, ctx, prefix)
        && (!preserve_existing || !ctx.parameter_values.contains_key(full_name))
    {
        insert_with_prefix(&mut ctx.parameter_values, prefix, name, full_name, val);
    }
    // Boolean constants (needed for evaluating conditional integer constants like
    // `nXi = if fixedX then 0 else nS - 1` in replaceable packages)
    if type_name == "Boolean"
        && let Some(val) = try_eval_const_boolean_with_scope(expr, ctx, prefix)
        && (!preserve_existing || !ctx.boolean_parameter_values.contains_key(full_name))
    {
        insert_with_prefix(
            &mut ctx.boolean_parameter_values,
            prefix,
            name,
            full_name,
            val,
        );
    }
    // Real constants (and constants of aliased Real-derived units).
    if let Some(val) = try_eval_const_real_with_scope(expr, ctx, prefix)
        && val.is_finite()
        && (!preserve_existing || !ctx.real_parameter_values.contains_key(full_name))
    {
        insert_with_prefix(&mut ctx.real_parameter_values, prefix, name, full_name, val);
    }
    if let Some(val) = try_eval_const_flat_expr_with_scope(expr, ctx, prefix)
        && (!preserve_existing || !ctx.constant_values.contains_key(full_name))
    {
        let val = canonicalize_constant_function_calls(val, function_scope);
        insert_with_prefix(&mut ctx.constant_values, prefix, name, full_name, val);
    }
    if let Some(val) = try_extract_constant_alias_expr(expr)
        && (!preserve_existing || !ctx.constant_values.contains_key(full_name))
    {
        insert_with_prefix(&mut ctx.constant_values, prefix, name, full_name, val);
    }
    // Enumeration constants (e.g., `ThermoStates = IndependentVariables.ph`)
    if (!preserve_existing || !ctx.enum_parameter_values.contains_key(full_name))
        && let Some(val) = try_eval_const_enum_with_scope(expr, ctx, prefix)
    {
        insert_with_prefix(&mut ctx.enum_parameter_values, prefix, name, full_name, val);
    }
    // Array dimensions from shape
    if (!preserve_existing || !ctx.array_dimensions.contains_key(full_name))
        && !comp.shape.is_empty()
    {
        let dims: Vec<i64> = comp.shape.iter().map(|&d| d as i64).collect();
        insert_with_prefix(&mut ctx.array_dimensions, prefix, name, full_name, dims);
    }
    // Array dimensions from binding (array literal length)
    if (!preserve_existing || !ctx.array_dimensions.contains_key(full_name))
        && let Some(dims) = infer_dims_from_expr(expr, ctx, prefix)
    {
        insert_with_prefix(&mut ctx.array_dimensions, prefix, name, full_name, dims);
    }
}

pub(super) fn try_extract_constant_alias_expr(
    expr: &ast::Expression,
) -> Option<rumoca_core::Expression> {
    let ast::Expression::ComponentReference(reference) = expr else {
        return None;
    };
    if reference.parts.is_empty() || reference.parts.iter().any(|part| part.subs.is_some()) {
        return None;
    }
    Some(rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new(reference.to_string()),
        subscripts: vec![],
        span: expr.span(),
    })
}

pub(super) fn constant_reference_name(expr: &ast::Expression) -> Option<String> {
    let ast::Expression::ComponentReference(reference) = expr else {
        return None;
    };
    if reference.parts.is_empty() || reference.parts.iter().any(|part| part.subs.is_some()) {
        return None;
    }
    Some(reference.to_string())
}

pub(super) fn materialize_record_constant_alias_fields(
    ctx: &mut Context,
    prefix: &str,
    target_name: &str,
    full_name: &str,
    alias_name: &str,
) {
    if !ctx.constant_values.contains_key(alias_name) {
        return;
    }

    propagate_alias_fields_in_map(
        &mut ctx.parameter_values,
        prefix,
        target_name,
        full_name,
        alias_name,
    );
    propagate_alias_fields_in_map(
        &mut ctx.real_parameter_values,
        prefix,
        target_name,
        full_name,
        alias_name,
    );
    propagate_alias_fields_in_map(
        &mut ctx.boolean_parameter_values,
        prefix,
        target_name,
        full_name,
        alias_name,
    );
    propagate_alias_fields_in_map(
        &mut ctx.enum_parameter_values,
        prefix,
        target_name,
        full_name,
        alias_name,
    );
    propagate_alias_fields_in_map(
        &mut ctx.constant_values,
        prefix,
        target_name,
        full_name,
        alias_name,
    );
    propagate_alias_fields_in_map(
        &mut ctx.array_dimensions,
        prefix,
        target_name,
        full_name,
        alias_name,
    );
}

pub(super) fn propagate_alias_fields_in_map<V: Clone>(
    map: &mut rustc_hash::FxHashMap<String, V>,
    prefix: &str,
    target_name: &str,
    full_name: &str,
    alias_name: &str,
) {
    let alias_prefix = format!("{alias_name}.");
    let fields: Vec<(String, V)> = map
        .iter()
        .filter_map(|(key, value)| {
            let field = key.strip_prefix(&alias_prefix)?;
            (!field.is_empty()).then(|| (field.to_string(), value.clone()))
        })
        .collect();
    for (field, value) in fields {
        let field_name = format!("{target_name}.{field}");
        let field_full_name = format!("{full_name}.{field}");
        insert_with_prefix(map, prefix, &field_name, &field_full_name, value);
    }
}

/// Check if a dotted variable name passes through an expanded array component.
///
/// Returns true if any parent segment (NOT the last segment) contains embedded
/// array subscripts. For example:
/// - `l1sigma.inductor[1].L` → true (parent `inductor[1]` has subscripts)
/// - `plug_p.pin[1]` → false (only the last segment has subscripts)
/// - `l1sigma.inductor[1].v[2]` → true (parent has subscripts)
pub(super) fn has_embedded_array_subscript_in_parent(name: &str) -> bool {
    let mut bracket_depth = 0usize;
    let mut segment_has_subscript = false;

    for byte in name.bytes() {
        match byte {
            b'[' => bracket_depth += 1,
            b']' => {
                let Some(next_depth) = bracket_depth.checked_sub(1) else {
                    return false;
                };
                bracket_depth = next_depth;
                if bracket_depth == 0 {
                    segment_has_subscript = true;
                }
            }
            b'.' if bracket_depth == 0 => {
                if segment_has_subscript {
                    return true;
                }
                segment_has_subscript = false;
            }
            _ => {}
        }
    }

    false
}

#[cfg(test)]
mod parent_subscript_tests {
    use super::has_embedded_array_subscript_in_parent;

    #[test]
    fn ignores_dot_inside_top_level_subscript() {
        assert!(!has_embedded_array_subscript_in_parent("bus[data.medium]"));
    }

    #[test]
    fn detects_actual_parent_subscript_only() {
        assert!(has_embedded_array_subscript_in_parent(
            "stack.cell[data.medium].x"
        ));
        assert!(!has_embedded_array_subscript_in_parent(
            "stack.cell.x[data.medium]"
        ));
    }
}
