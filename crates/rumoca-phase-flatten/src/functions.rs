//! flat::Function collection and flattening for user-defined functions.
//!
//! This module is responsible for:
//! - Collecting function calls used in the model
//! - Looking up function definitions from the ast::ClassTree
//! - Converting function definitions to flat::Function
//!
//! Per MLS §12, functions in Modelica are callable units with:
//! - Input parameters (values passed in)
//! - Output parameters (values returned)
//! - An algorithm section (the function body)

use indexmap::IndexMap;
#[cfg(test)]
use rumoca_core::Span;
use rumoca_ir_ast as ast;
use rumoca_ir_flat as flat;
use std::collections::{HashMap, HashSet};

use crate::algorithms;
use crate::ast_lower;
use crate::errors::FlattenError;
use crate::qualify;

fn is_callable_class_type(class_type: &ast::ClassType) -> bool {
    !matches!(
        class_type,
        ast::ClassType::Package | ast::ClassType::Connector | ast::ClassType::Operator
    )
}

/// Collect all user function calls from a flat::Model.
///
/// Walks through all equations and expressions to find function calls,
/// returning a set of unique function names that need definitions.
pub(crate) fn collect_function_calls(flat: &flat::Model) -> HashSet<String> {
    let mut calls = HashSet::new();

    // Collect from equations
    for eq in &flat.equations {
        collect_from_expression(&eq.residual, &mut calls);
    }

    // Collect from initial equations
    for eq in &flat.initial_equations {
        collect_from_expression(&eq.residual, &mut calls);
    }

    // Collect from variable bindings and attributes
    for var in flat.variables.values() {
        if let Some(binding) = &var.binding {
            collect_from_expression(binding, &mut calls);
        }
        if let Some(start) = &var.start {
            collect_from_expression(start, &mut calls);
        }
        if let Some(min) = &var.min {
            collect_from_expression(min, &mut calls);
        }
        if let Some(max) = &var.max {
            collect_from_expression(max, &mut calls);
        }
        if let Some(nominal) = &var.nominal {
            collect_from_expression(nominal, &mut calls);
        }
    }

    // Collect from when clauses
    for when in &flat.when_clauses {
        collect_from_expression(&when.condition, &mut calls);
        for eq in &when.equations {
            collect_from_when_equation(eq, &mut calls);
        }
    }

    // Collect from assertions
    for assertion in &flat.assert_equations {
        collect_from_expression(&assertion.condition, &mut calls);
        collect_from_expression(&assertion.message, &mut calls);
        if let Some(level) = &assertion.level {
            collect_from_expression(level, &mut calls);
        }
    }
    for assertion in &flat.initial_assert_equations {
        collect_from_expression(&assertion.condition, &mut calls);
        collect_from_expression(&assertion.message, &mut calls);
        if let Some(level) = &assertion.level {
            collect_from_expression(level, &mut calls);
        }
    }

    // Collect from algorithm statements
    for algorithm in &flat.algorithms {
        for statement in &algorithm.statements {
            collect_from_statement(statement, &mut calls);
        }
    }
    for algorithm in &flat.initial_algorithms {
        for statement in &algorithm.statements {
            collect_from_statement(statement, &mut calls);
        }
    }

    calls
}

/// Collect function calls from a WhenEquation.
fn collect_from_when_equation(eq: &rumoca_ir_flat::WhenEquation, calls: &mut HashSet<String>) {
    match eq {
        flat::WhenEquation::Assign { value, .. } => {
            collect_from_expression(value, calls);
        }
        flat::WhenEquation::Reinit { value, .. } => {
            collect_from_expression(value, calls);
        }
        flat::WhenEquation::Assert { condition, .. } => {
            collect_from_expression(condition, calls);
        }
        flat::WhenEquation::Terminate { .. } => {}
        flat::WhenEquation::Conditional {
            branches,
            else_branch,
            ..
        } => {
            for (cond, eqs) in branches {
                collect_from_expression(cond, calls);
                for eq in eqs {
                    collect_from_when_equation(eq, calls);
                }
            }
            for eq in else_branch {
                collect_from_when_equation(eq, calls);
            }
        }
        flat::WhenEquation::FunctionCallOutputs { function, .. } => {
            // Collect function calls from the multi-output function call expression
            collect_from_expression(function, calls);
        }
    }
}

/// Collect function calls from an expression using the visitor pattern.
///
/// Uses `flat::ExpressionVisitor` to traverse the expression tree and collect
/// all user-defined function call names.
fn collect_from_expression(expr: &flat::Expression, calls: &mut HashSet<String>) {
    /// Local visitor that collects into an existing HashSet.
    struct Collector<'a> {
        calls: &'a mut HashSet<String>,
    }

    impl flat::ExpressionVisitor for Collector<'_> {
        fn visit_function_call(&mut self, name: &flat::VarName, args: &[flat::Expression]) {
            self.calls.insert(name.to_string());
            self.walk_function_call(name, args);
        }
    }

    let mut collector = Collector { calls };
    flat::ExpressionVisitor::visit_expression(&mut collector, expr);
}

/// Collect and flatten all function definitions used by the model.
///
/// This finds all function calls in the model, looks up their definitions
/// in the ast::ClassTree, and converts them to flat::Function objects.
pub(crate) fn collect_functions(
    flat: &mut flat::Model,
    tree: &ast::ClassTree,
) -> Result<(), FlattenError> {
    let mut pending: Vec<(String, Option<String>)> = collect_function_calls(flat)
        .into_iter()
        .map(|name| (name, None))
        .collect();
    pending.extend(
        flat.functions
            .keys()
            .map(|name| (name.as_str().to_string(), None)),
    );
    let mut requested = HashSet::new();
    let mut expanded = HashSet::new();
    let mut inserted: HashSet<String> = flat
        .functions
        .keys()
        .map(|n| n.as_str().to_string())
        .collect();

    while let Some((func_name, caller_scope)) = pending.pop() {
        if !requested.insert((func_name.clone(), caller_scope.clone())) {
            continue;
        }

        let resolved = lookup_function_with_scope(tree, &func_name, caller_scope.as_deref())
            .or_else(|| {
                flat.functions
                    .get(&flat::VarName::new(func_name.clone()))
                    .cloned()
                    .map(|f| (f.name.as_str().to_string(), f))
            })
            .or_else(|| lookup_function_in_known_packages(tree, &func_name, &inserted));
        let Some((qualified_name, flat_func)) = resolved else {
            // If not found or not a function type, it might be:
            // - An external function (MLS §12.9)
            // - A library function we don't have the source for
            // - A record constructor or operator function (MLS §14)
            // Code generators handle these cases or error appropriately
            continue;
        };

        if !expanded.insert(qualified_name.clone()) {
            continue;
        }

        for dep in collect_function_deps(&flat_func, tree) {
            if !requested.contains(&(dep.clone(), Some(qualified_name.clone()))) {
                pending.push((dep, Some(qualified_name.clone())));
            }
        }
        inserted.insert(qualified_name);
        flat.add_function(flat_func);
    }

    Ok(())
}

/// Look up a function by name from the ast::ClassTree and convert to flat::Function.
///
/// This is useful for lazy function lookup during constant evaluation.
/// Returns None if the function is not found or is not a function type.
pub(crate) fn lookup_function(tree: &ast::ClassTree, func_name: &str) -> Option<flat::Function> {
    let (_, func) = lookup_function_with_name(tree, func_name)?;
    Some(func)
}

fn lookup_function_with_name(
    tree: &ast::ClassTree,
    func_name: &str,
) -> Option<(String, flat::Function)> {
    lookup_function_with_scope(tree, func_name, None)
}

fn lookup_function_with_scope(
    tree: &ast::ClassTree,
    func_name: &str,
    caller_scope: Option<&str>,
) -> Option<(String, flat::Function)> {
    let qualified_name = resolve_function_qualified_name_with_scope(tree, func_name, caller_scope)?;
    let class_def = tree.get_class_by_qualified_name(&qualified_name)?;
    let flat_func = convert_callable(
        tree,
        class_def,
        &qualified_name,
        &tree.source_map,
        &tree.def_map,
    )?;
    Some((qualified_name, flat_func))
}

/// Resolve alias-style function names (e.g. `Medium.dynamicViscosity`) by
/// reusing package prefixes already present in the model's known function set.
fn lookup_function_in_known_packages(
    tree: &ast::ClassTree,
    func_name: &str,
    known_functions: &HashSet<String>,
) -> Option<(String, flat::Function)> {
    let mut parts = func_name.split('.');
    let _first = parts.next()?;
    let remainder = parts.collect::<Vec<_>>().join(".");
    if remainder.is_empty() {
        return None;
    }

    let mut matched: Option<String> = None;
    for known in known_functions {
        let Some((pkg_prefix, _leaf)) = known.rsplit_once('.') else {
            continue;
        };
        let candidate = format!("{pkg_prefix}.{remainder}");
        let Some(class_def) = tree.get_class_by_qualified_name(&candidate) else {
            continue;
        };
        if !is_callable_class_type(&class_def.class_type) {
            continue;
        }
        if matched
            .as_ref()
            .is_some_and(|existing| existing != &candidate)
        {
            return None;
        }
        matched = Some(candidate);
    }

    let qualified_name = matched?;
    let class_def = tree.get_class_by_qualified_name(&qualified_name)?;
    let flat_func = convert_callable(
        tree,
        class_def,
        &qualified_name,
        &tree.source_map,
        &tree.def_map,
    )?;
    Some((qualified_name, flat_func))
}

fn resolve_function_qualified_name_with_scope(
    tree: &ast::ClassTree,
    func_name: &str,
    caller_scope: Option<&str>,
) -> Option<String> {
    if let Some(class_def) = tree.get_class_by_qualified_name(func_name)
        && is_callable_class_type(&class_def.class_type)
    {
        return Some(func_name.to_string());
    }

    if let Some(matched) = resolve_unique_function_suffix(tree, func_name) {
        return Some(matched);
    }

    let short_name = func_name.rsplit('.').next().unwrap_or(func_name);
    if short_name != func_name {
        if let Some(caller_scope) = caller_scope
            && let Some(scoped_match) =
                resolve_function_in_caller_packages(tree, caller_scope, short_name)
        {
            return Some(scoped_match);
        }
        if let Some(matched) = resolve_unique_function_suffix(tree, short_name) {
            return Some(matched);
        }
    }

    None
}

fn collect_function_deps(func: &flat::Function, tree: &ast::ClassTree) -> HashSet<String> {
    let _ = tree;
    let mut deps = HashSet::new();

    for param in func
        .inputs
        .iter()
        .chain(func.outputs.iter())
        .chain(func.locals.iter())
    {
        if let Some(default) = &param.default {
            collect_from_expression(default, &mut deps);
        }
    }

    for stmt in &func.body {
        collect_from_statement(stmt, &mut deps);
    }

    deps
}

fn collect_from_statement(stmt: &flat::Statement, deps: &mut HashSet<String>) {
    match stmt {
        flat::Statement::Empty | flat::Statement::Return | flat::Statement::Break => {}
        flat::Statement::Assignment { value, .. } => {
            collect_from_expression(value, deps);
        }
        flat::Statement::For { indices, equations } => {
            for idx in indices {
                collect_from_expression(&idx.range, deps);
            }
            for inner in equations {
                collect_from_statement(inner, deps);
            }
        }
        flat::Statement::While(block) => {
            collect_from_expression(&block.cond, deps);
            for inner in &block.stmts {
                collect_from_statement(inner, deps);
            }
        }
        flat::Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                collect_from_expression(&block.cond, deps);
                for inner in &block.stmts {
                    collect_from_statement(inner, deps);
                }
            }
            if let Some(stmts) = else_block {
                for inner in stmts {
                    collect_from_statement(inner, deps);
                }
            }
        }
        flat::Statement::When(blocks) => {
            for block in blocks {
                collect_from_expression(&block.cond, deps);
                for inner in &block.stmts {
                    collect_from_statement(inner, deps);
                }
            }
        }
        flat::Statement::FunctionCall {
            comp,
            args,
            outputs,
        } => {
            deps.insert(component_ref_name(comp));
            for arg in args {
                collect_from_expression(arg, deps);
            }
            for output in outputs {
                collect_from_expression(output, deps);
            }
        }
        flat::Statement::Reinit { value, .. } => {
            collect_from_expression(value, deps);
        }
        flat::Statement::Assert {
            condition,
            message,
            level,
        } => {
            collect_from_expression(condition, deps);
            collect_from_expression(message, deps);
            if let Some(level_expr) = level {
                collect_from_expression(level_expr, deps);
            }
        }
    }
}

fn component_ref_name(comp: &rumoca_ir_flat::ComponentReference) -> String {
    comp.parts
        .iter()
        .map(|part| part.ident.as_str())
        .collect::<Vec<_>>()
        .join(".")
}

fn resolve_unique_function_suffix(tree: &ast::ClassTree, suffix_name: &str) -> Option<String> {
    let suffix = format!(".{suffix_name}");
    let mut matched: Option<String> = None;
    for candidate in tree.name_map.keys() {
        if !candidate.ends_with(&suffix) {
            continue;
        }
        let Some(class_def) = tree.get_class_by_qualified_name(candidate) else {
            continue;
        };
        if !is_callable_class_type(&class_def.class_type) {
            continue;
        }
        if matched.is_some() {
            return None;
        }
        matched = Some(candidate.clone());
    }
    matched
}

fn resolve_function_in_caller_packages(
    tree: &ast::ClassTree,
    caller_scope: &str,
    short_name: &str,
) -> Option<String> {
    let mut prefix = caller_scope.rsplit_once('.').map(|(pkg, _)| pkg)?;
    loop {
        let candidate = format!("{prefix}.{short_name}");
        if let Some(class_def) = tree.get_class_by_qualified_name(&candidate)
            && is_callable_class_type(&class_def.class_type)
        {
            return Some(candidate);
        }
        let Some((parent, _)) = prefix.rsplit_once('.') else {
            break;
        };
        prefix = parent;
    }
    None
}

#[derive(Default)]
struct FunctionClassContext {
    components: IndexMap<String, ast::Component>,
    algorithms: Vec<Vec<ast::Statement>>,
    imports: qualify::ImportMap,
}

fn collect_function_context(
    tree: &ast::ClassTree,
    class_def: &ast::ClassDef,
) -> FunctionClassContext {
    let mut visited = HashSet::new();
    let mut context = FunctionClassContext::default();
    collect_function_context_recursive(tree, class_def, &mut visited, &mut context);
    context
}

fn collect_function_context_recursive(
    tree: &ast::ClassTree,
    class_def: &ast::ClassDef,
    visited: &mut HashSet<usize>,
    context: &mut FunctionClassContext,
) {
    let class_key = class_def as *const ast::ClassDef as usize;
    if !visited.insert(class_key) {
        return;
    }

    for extend in &class_def.extends {
        let base_class = extend
            .base_def_id
            .and_then(|def_id| tree.get_class_by_def_id(def_id))
            .or_else(|| {
                let qualified = extend.base_name.to_string();
                tree.get_class_by_qualified_name(&qualified)
            });
        if let Some(base_class) = base_class {
            collect_function_context_recursive(tree, base_class, visited, context);
        }
    }

    resolve_import_pairs(&class_def.imports, tree, &mut context.imports);
    context.algorithms.extend(class_def.algorithms.clone());
    context.components.extend(class_def.components.clone());
}

fn collect_lexical_ancestor_imports(
    tree: &ast::ClassTree,
    class_name: &str,
    map: &mut qualify::ImportMap,
) {
    let mut ancestors = Vec::new();
    let mut scope = class_name;
    while let Some((parent, _)) = scope.rsplit_once('.') {
        ancestors.push(parent.to_string());
        scope = parent;
    }
    ancestors.reverse();
    for ancestor in ancestors {
        let Some(ancestor_class) = tree.get_class_by_qualified_name(&ancestor) else {
            continue;
        };
        resolve_import_pairs(&ancestor_class.imports, tree, map);
    }
}

fn resolve_import_pairs(
    imports: &[ast::Import],
    tree: &ast::ClassTree,
    map: &mut qualify::ImportMap,
) {
    for import in imports {
        match import {
            ast::Import::Qualified { path, .. } => {
                let fqn = path.to_string();
                if let Some(short) = fqn.rsplit('.').next() {
                    map.insert(short.to_string(), fqn);
                }
            }
            ast::Import::Renamed { alias, path, .. } => {
                map.insert(alias.text.to_string(), path.to_string());
            }
            ast::Import::Unqualified { path, .. } => {
                let pkg_name = path.to_string();
                let Some(class_def) = tree.get_class_by_qualified_name(&pkg_name) else {
                    continue;
                };
                for name in class_def.components.keys() {
                    map.insert(name.clone(), format!("{pkg_name}.{name}"));
                }
                for name in class_def.classes.keys() {
                    map.insert(name.clone(), format!("{pkg_name}.{name}"));
                }
            }
            ast::Import::Selective { path, names, .. } => {
                let pkg_name = path.to_string();
                for name_tok in names {
                    let name = name_tok.text.to_string();
                    map.insert(name.clone(), format!("{pkg_name}.{name}"));
                }
            }
        }
    }
}

fn convert_callable(
    tree: &ast::ClassTree,
    class_def: &ast::ClassDef,
    qualified_name: &str,
    source_map: &rumoca_core::SourceMap,
    def_map: &indexmap::IndexMap<rumoca_core::DefId, String>,
) -> Option<flat::Function> {
    match &class_def.class_type {
        ast::ClassType::Function => {
            convert_function(tree, class_def, qualified_name, source_map, def_map).ok()
        }
        class_type if is_callable_class_type(class_type) => Some(convert_constructor_signature(
            tree,
            class_def,
            qualified_name,
            source_map,
            def_map,
        )),
        _ => None,
    }
}

/// Convert a ast::ClassDef (function) to a flat::Function.
fn convert_function(
    tree: &ast::ClassTree,
    class_def: &ast::ClassDef,
    qualified_name: &str,
    source_map: &rumoca_core::SourceMap,
    def_map: &indexmap::IndexMap<rumoca_core::DefId, String>,
) -> Result<flat::Function, FlattenError> {
    // Use the location from class definition
    let span = source_map.location_to_span(
        &class_def.location.file_name,
        class_def.location.start as usize,
        class_def.location.end as usize,
    );
    let mut func = flat::Function::new(qualified_name, span);
    let context = collect_function_context(tree, class_def);
    let effective_components = context.components;
    let mut import_map = qualify::ImportMap::default();
    qualify::collect_lexical_package_aliases(tree, qualified_name, &mut import_map);
    collect_lexical_constant_aliases(tree, qualified_name, &mut import_map);
    collect_lexical_ancestor_imports(tree, qualified_name, &mut import_map);
    import_map.extend(context.imports);
    let prefix = ast::QualifiedName::new();
    let function_locals: HashSet<String> = effective_components.keys().cloned().collect();

    // Process components to find inputs, outputs, and locals
    for (comp_name, component) in &effective_components {
        let param = convert_component_to_param(
            comp_name,
            component,
            def_map,
            &import_map,
            &function_locals,
        );

        match &component.causality {
            rumoca_ir_core::Causality::Input(_) => func.add_input(param),
            rumoca_ir_core::Causality::Output(_) => func.add_output(param),
            rumoca_ir_core::Causality::Empty => func.add_local(param),
        }
    }

    // MLS §12.4.1: Function parameters are local to the function body.
    // Filter the def_map to exclude entries that resolve to the function's own
    // local parameters, so that function-typed parameters (e.g., `f` in
    // `quadratureLobatto(f, a, b, tolerance)`) are not over-qualified to their
    // fully-qualified path (e.g., `Modelica.Math.Nonlinear.quadratureLobatto.f`)
    // during AST lowering. The qualified path would produce a non-existent
    // global name after dot-to-underscore sanitization.
    let func_prefix_dot = format!("{qualified_name}.");
    let filtered_def_map: indexmap::IndexMap<rumoca_core::DefId, String> = def_map
        .iter()
        .filter(|(_, path)| {
            if let Some(suffix) = path.strip_prefix(&func_prefix_dot) {
                // Keep only entries that are NOT simple local parameter names.
                // Multi-segment suffixes (e.g., "sub.field") are kept since they
                // reference nested paths, not direct local parameters.
                !(suffix.find('.').is_none() && function_locals.contains(suffix))
            } else {
                true
            }
        })
        .map(|(k, v)| (*k, v.clone()))
        .collect();

    for alg in &context.algorithms {
        let flat_alg = algorithms::flatten_algorithm_section(
            alg,
            &prefix,
            span,
            qualified_name.to_string(),
            &import_map,
            Some(&filtered_def_map),
            &function_locals,
        )?;
        func.body.extend(flat_alg.statements);
    }

    // MLS §4.9: Rewrite FieldAccess on record-typed function parameters
    // to direct VarRef names (e.g., `c.re` → `c_re`). This allows backends
    // to render them as simple variable names. The function signature is NOT
    // changed here — that happens optionally in the codegen/DAE phase for
    // backends that need it.
    rewrite_record_field_access_in_body(&mut func);

    // Use pure flag from ast::ClassDef (MLS §12.3)
    // Functions are pure by default unless declared with `impure` keyword
    func.pure = class_def.pure;

    // Convert external function declaration (MLS §12.9)
    if let Some(ref ext) = class_def.external {
        func.external = Some(convert_external_function(ext, qualified_name));
    }

    // Extract derivative annotations (MLS §12.7.1)
    func.derivatives = extract_derivative_annotations(&class_def.annotation);

    Ok(func)
}

/// Known record types and their ordered scalar field names.
/// These records are decomposed into scalar fields for backends that
/// don't support record-typed function parameters natively.
const KNOWN_RECORD_FIELDS: &[(&str, &[&str])] = &[
    ("Complex", &["re", "im"]),
    ("Modelica.Units.SI.ComplexVoltage", &["re", "im"]),
    ("Modelica.Units.SI.ComplexCurrent", &["re", "im"]),
];

/// Get the field names for a known record type, checking both short and qualified names.
fn record_fields(type_name: &str) -> Option<&'static [&'static str]> {
    for &(name, fields) in KNOWN_RECORD_FIELDS {
        if type_name == name || type_name.ends_with(&format!(".{name}")) {
            return Some(fields);
        }
    }
    None
}

/// Rewrite FieldAccess on decomposed record params to direct VarRef.
fn rewrite_field_access_in_statement(stmt: &mut flat::Statement, params: &HashSet<String>) {
    match stmt {
        flat::Statement::Assignment { value, .. } => {
            rewrite_field_access_in_expr(value, params);
        }
        flat::Statement::For { indices, equations } => {
            for idx in indices {
                rewrite_field_access_in_expr(&mut idx.range, params);
            }
            for inner in equations {
                rewrite_field_access_in_statement(inner, params);
            }
        }
        flat::Statement::While(block) => {
            rewrite_field_access_in_expr(&mut block.cond, params);
            for inner in &mut block.stmts {
                rewrite_field_access_in_statement(inner, params);
            }
        }
        flat::Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                rewrite_field_access_in_expr(&mut block.cond, params);
                for inner in &mut block.stmts {
                    rewrite_field_access_in_statement(inner, params);
                }
            }
            if let Some(stmts) = else_block {
                for inner in stmts {
                    rewrite_field_access_in_statement(inner, params);
                }
            }
        }
        flat::Statement::FunctionCall { args, outputs, .. } => {
            for arg in args {
                rewrite_field_access_in_expr(arg, params);
            }
            for out in outputs {
                rewrite_field_access_in_expr(out, params);
            }
        }
        flat::Statement::Reinit { value, .. } => {
            rewrite_field_access_in_expr(value, params);
        }
        flat::Statement::Assert {
            condition,
            message,
            level,
        } => {
            rewrite_field_access_in_expr(condition, params);
            rewrite_field_access_in_expr(message, params);
            if let Some(l) = level {
                rewrite_field_access_in_expr(l, params);
            }
        }
        flat::Statement::Empty | flat::Statement::Return | flat::Statement::Break => {}
        flat::Statement::When(blocks) => {
            for block in blocks {
                rewrite_field_access_in_expr(&mut block.cond, params);
                for inner in &mut block.stmts {
                    rewrite_field_access_in_statement(inner, params);
                }
            }
        }
    }
}

fn rewrite_field_access_in_expr(expr: &mut flat::Expression, params: &HashSet<String>) {
    // Check if this is a FieldAccess on a decomposed record param
    if let flat::Expression::FieldAccess { base, field } = expr
        && let flat::Expression::VarRef {
            name, subscripts, ..
        } = base.as_ref()
        && subscripts.is_empty()
        && params.contains(name.as_str())
    {
        let new_name = format!("{}_{}", name.as_str(), field);
        *expr = flat::Expression::VarRef {
            name: flat::VarName::new(new_name),
            subscripts: vec![],
        };
        return;
    }

    // Recurse into sub-expressions
    match expr {
        flat::Expression::Binary { lhs, rhs, .. } => {
            rewrite_field_access_in_expr(lhs, params);
            rewrite_field_access_in_expr(rhs, params);
        }
        flat::Expression::Unary { rhs, .. } => {
            rewrite_field_access_in_expr(rhs, params);
        }
        flat::Expression::BuiltinCall { args, .. }
        | flat::Expression::FunctionCall { args, .. } => {
            for arg in args {
                rewrite_field_access_in_expr(arg, params);
            }
        }
        flat::Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                rewrite_field_access_in_expr(cond, params);
                rewrite_field_access_in_expr(then_expr, params);
            }
            rewrite_field_access_in_expr(else_branch, params);
        }
        flat::Expression::Array { elements, .. } | flat::Expression::Tuple { elements } => {
            for elem in elements {
                rewrite_field_access_in_expr(elem, params);
            }
        }
        flat::Expression::Range { start, step, end } => {
            rewrite_field_access_in_expr(start, params);
            if let Some(s) = step {
                rewrite_field_access_in_expr(s, params);
            }
            rewrite_field_access_in_expr(end, params);
        }
        flat::Expression::Index { base, .. } => {
            rewrite_field_access_in_expr(base, params);
        }
        flat::Expression::FieldAccess { base, .. } => {
            rewrite_field_access_in_expr(base, params);
        }
        flat::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            rewrite_field_access_in_expr(expr, params);
            for idx in indices {
                rewrite_field_access_in_expr(&mut idx.range, params);
            }
            if let Some(f) = filter {
                rewrite_field_access_in_expr(f, params);
            }
        }
        flat::Expression::VarRef { .. }
        | flat::Expression::Literal(_)
        | flat::Expression::Empty => {}
    }
}

/// Rewrite FieldAccess expressions on record-typed params in a function body.
///
/// Converts `c.re` → `c_re` in the body without changing the function signature.
/// This is safe for all backends since it only changes expression structure.
fn rewrite_record_field_access_in_body(func: &mut flat::Function) {
    let mut record_param_names: HashSet<String> = HashSet::new();
    for input in &func.inputs {
        if record_fields(&input.type_name).is_some() {
            record_param_names.insert(input.name.clone());
        }
    }
    if record_param_names.is_empty() {
        return;
    }
    for stmt in &mut func.body {
        rewrite_field_access_in_statement(stmt, &record_param_names);
    }
}

fn collect_lexical_constant_aliases(
    tree: &ast::ClassTree,
    class_name: &str,
    imports: &mut qualify::ImportMap,
) {
    let mut scope = class_name;
    while let Some((parent, _)) = scope.rsplit_once('.') {
        scope = parent;
        let Some(class_def) = tree.get_class_by_qualified_name(scope) else {
            continue;
        };
        for (name, component) in &class_def.components {
            if matches!(
                component.variability,
                rumoca_ir_core::Variability::Constant(_)
                    | rumoca_ir_core::Variability::Parameter(_)
            ) {
                imports
                    .entry(name.clone())
                    .or_insert_with(|| format!("{scope}.{name}"));
            }
        }
    }
}

fn collect_constructor_params(
    tree: &ast::ClassTree,
    class_def: &ast::ClassDef,
    visited_classes: &mut HashSet<usize>,
    params: &mut Vec<flat::FunctionParam>,
    param_index: &mut HashMap<String, usize>,
    def_map: &indexmap::IndexMap<rumoca_core::DefId, String>,
) {
    let class_ptr = class_def as *const ast::ClassDef as usize;
    if !visited_classes.insert(class_ptr) {
        return;
    }

    for ext in &class_def.extends {
        let base_class = ext
            .base_def_id
            .and_then(|def_id| tree.get_class_by_def_id(def_id))
            .or_else(|| {
                let name = ext.base_name.to_string();
                tree.get_class_by_qualified_name(&name)
            });
        if let Some(base_class) = base_class {
            collect_constructor_params(
                tree,
                base_class,
                visited_classes,
                params,
                param_index,
                def_map,
            );
        }
    }

    for (comp_name, component) in &class_def.components {
        let param = convert_component_to_param(
            comp_name,
            component,
            def_map,
            &qualify::ImportMap::default(),
            &HashSet::new(),
        );
        if let Some(index) = param_index.get(comp_name).copied() {
            params[index] = param;
        } else {
            param_index.insert(comp_name.clone(), params.len());
            params.push(param);
        }
    }
}

/// Build a synthetic constructor signature for constructor-like class calls.
fn convert_constructor_signature(
    tree: &ast::ClassTree,
    class_def: &ast::ClassDef,
    qualified_name: &str,
    source_map: &rumoca_core::SourceMap,
    def_map: &indexmap::IndexMap<rumoca_core::DefId, String>,
) -> flat::Function {
    let span = source_map.location_to_span(
        &class_def.location.file_name,
        class_def.location.start as usize,
        class_def.location.end as usize,
    );
    let mut params = Vec::new();
    let mut param_index = HashMap::new();
    let mut visited_classes = HashSet::new();
    collect_constructor_params(
        tree,
        class_def,
        &mut visited_classes,
        &mut params,
        &mut param_index,
        def_map,
    );

    let mut func = flat::Function::new(qualified_name, span);
    for param in params {
        func.add_input(param);
    }
    func
}

/// Convert an AST ExternalFunction to ExternalFunction.
fn convert_external_function(
    ext: &rumoca_ir_ast::ExternalFunction,
    _default_name: &str,
) -> rumoca_ir_flat::ExternalFunction {
    rumoca_ir_flat::ExternalFunction {
        language: ext.language.clone().unwrap_or_else(|| "C".to_string()),
        function_name: ext.function_name.as_ref().map(|t| t.text.to_string()),
        output_name: ext.output.as_ref().map(|o| {
            o.parts
                .iter()
                .map(|p| p.ident.text.to_string())
                .collect::<Vec<_>>()
                .join(".")
        }),
        arg_names: ext
            .args
            .iter()
            .filter_map(|arg| {
                // Extract variable names from expressions
                if let ast::Expression::ComponentReference(cr) = arg {
                    Some(
                        cr.parts
                            .iter()
                            .map(|p| p.ident.text.to_string())
                            .collect::<Vec<_>>()
                            .join("."),
                    )
                } else {
                    None
                }
            })
            .collect(),
    }
}

/// Extract derivative annotations from function annotation expressions (MLS §12.7.1).
///
/// Looks for annotations like:
/// - `derivative = funcName`
/// - `derivative(order=2) = funcName`
/// - `derivative(zeroDerivative=x, zeroDerivative=y) = funcName`
/// - `derivative(noDerivative=u) = funcName`
fn extract_derivative_annotations(
    annotations: &[ast::Expression],
) -> Vec<flat::DerivativeAnnotation> {
    let mut derivatives = Vec::new();

    for expr in annotations {
        if let Some(deriv) = extract_single_derivative(expr) {
            derivatives.push(deriv);
        }
    }

    derivatives
}

/// Extract a single derivative annotation from an expression.
fn extract_single_derivative(expr: &ast::Expression) -> Option<flat::DerivativeAnnotation> {
    // Pattern 1: NamedArgument { name: "derivative", value: ... }
    // This handles: derivative = funcName
    if let ast::Expression::NamedArgument { name, value } = expr
        && name.text.as_ref() == "derivative"
    {
        let func_name = extract_function_name(value)?;
        return Some(flat::DerivativeAnnotation {
            derivative_function: func_name,
            order: 1,
            zero_derivative: Vec::new(),
            no_derivative: Vec::new(),
        });
    }

    // Pattern 2: Modification { target: derivative(...), value: funcName }
    // This handles: derivative(order=2) = funcName, derivative(zeroDerivative=x) = funcName
    if let ast::Expression::Modification { target, value } = expr
        && let Some(annotation) = try_extract_modification_derivative(target, value)
    {
        return Some(annotation);
    }

    // Pattern 3: ClassModification { target: derivative, modifications: [...] }
    // This handles more complex cases where derivative has modifications
    if let ast::Expression::ClassModification {
        target,
        modifications,
    } = expr
        && let Some(annotation) = try_extract_class_mod_derivative(target, modifications)
    {
        return Some(annotation);
    }

    None
}

/// Try to extract a derivative annotation from a Modification expression.
fn try_extract_modification_derivative(
    target: &rumoca_ir_ast::ComponentReference,
    value: &ast::Expression,
) -> Option<flat::DerivativeAnnotation> {
    // Check if target is "derivative"
    if target.parts.len() != 1 || target.parts[0].ident.text.as_ref() != "derivative" {
        return None;
    }

    let func_name = extract_function_name(value)?;
    let mut annotation = flat::DerivativeAnnotation {
        derivative_function: func_name,
        order: 1,
        zero_derivative: Vec::new(),
        no_derivative: Vec::new(),
    };

    // Extract modifiers from subscripts
    extract_modifiers_from_subscripts(&target.parts[0].subs, &mut annotation);
    Some(annotation)
}

/// Try to extract a derivative annotation from a ClassModification expression.
fn try_extract_class_mod_derivative(
    target: &rumoca_ir_ast::ComponentReference,
    modifications: &[ast::Expression],
) -> Option<flat::DerivativeAnnotation> {
    // Check if target is "derivative"
    if target.parts.len() != 1 || target.parts[0].ident.text.as_ref() != "derivative" {
        return None;
    }

    let mut annotation = flat::DerivativeAnnotation {
        derivative_function: String::new(),
        order: 1,
        zero_derivative: Vec::new(),
        no_derivative: Vec::new(),
    };

    // Extract modifiers from the modifications list
    for mod_expr in modifications {
        extract_derivative_modifier(mod_expr, &mut annotation);
        // Check if this is the function name (ComponentReference without assignment)
        if let Some(name) = extract_function_name(mod_expr) {
            annotation.derivative_function = name;
        }
    }

    if annotation.derivative_function.is_empty() {
        None
    } else {
        Some(annotation)
    }
}

/// Extract modifiers from subscripts (used in derivative(order=2) style).
fn extract_modifiers_from_subscripts(
    subs: &Option<Vec<rumoca_ir_ast::Subscript>>,
    annotation: &mut flat::DerivativeAnnotation,
) {
    let Some(subs) = subs else { return };
    for sub in subs {
        if let rumoca_ir_ast::Subscript::Expression(sub_expr) = sub {
            extract_derivative_modifier(sub_expr, annotation);
        }
    }
}

/// Extract derivative modifiers like order, zeroDerivative, noDerivative from an expression.
fn extract_derivative_modifier(
    expr: &ast::Expression,
    annotation: &mut flat::DerivativeAnnotation,
) {
    // Handle NamedArgument { name: "order"|"zeroDerivative"|"noDerivative", value: ... }
    if let ast::Expression::NamedArgument { name, value } = expr {
        apply_modifier(name.text.as_ref(), value, annotation);
    }

    // Handle Modification { target: "order"|..., value: ... }
    if let ast::Expression::Modification { target, value } = expr
        && target.parts.len() == 1
    {
        apply_modifier(target.parts[0].ident.text.as_ref(), value, annotation);
    }
}

/// Apply a derivative modifier by name to the annotation.
fn apply_modifier(
    name: &str,
    value: &ast::Expression,
    annotation: &mut flat::DerivativeAnnotation,
) {
    match name {
        "order" => {
            if let Some(order) = extract_integer_value(value) {
                annotation.order = order as u32;
            }
        }
        "zeroDerivative" => {
            if let Some(var_name) = extract_variable_name(value) {
                annotation.zero_derivative.push(var_name);
            }
        }
        "noDerivative" => {
            if let Some(var_name) = extract_variable_name(value) {
                annotation.no_derivative.push(var_name);
            }
        }
        _ => {}
    }
}

/// Extract a function name from an expression (ComponentReference).
fn extract_function_name(expr: &ast::Expression) -> Option<String> {
    if let ast::Expression::ComponentReference(cr) = expr {
        Some(
            cr.parts
                .iter()
                .map(|p| p.ident.text.to_string())
                .collect::<Vec<_>>()
                .join("."),
        )
    } else {
        None
    }
}

/// Extract an integer value from an expression (Terminal with UnsignedInteger).
fn extract_integer_value(expr: &ast::Expression) -> Option<i64> {
    if let ast::Expression::Terminal {
        terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
        token,
    } = expr
    {
        token.text.parse().ok()
    } else {
        None
    }
}

/// Extract a variable name from an expression (ComponentReference).
fn extract_variable_name(expr: &ast::Expression) -> Option<String> {
    if let ast::Expression::ComponentReference(cr) = expr {
        Some(
            cr.parts
                .iter()
                .map(|p| p.ident.text.to_string())
                .collect::<Vec<_>>()
                .join("."),
        )
    } else {
        None
    }
}

/// Try to extract an integer value from a subscript expression.
fn extract_integer_from_subscript(sub: &rumoca_ir_ast::Subscript) -> Option<i64> {
    if let rumoca_ir_ast::Subscript::Expression(rumoca_ir_ast::Expression::Terminal {
        terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
        token,
    }) = sub
    {
        token.text.parse().ok()
    } else {
        None
    }
}

/// Convert a component declaration to a function parameter.
fn convert_component_to_param(
    name: &str,
    component: &ast::Component,
    def_map: &indexmap::IndexMap<rumoca_core::DefId, String>,
    imports: &qualify::ImportMap,
    locals: &HashSet<String>,
) -> flat::FunctionParam {
    // Get the type name from type_name.name (Vec<Token>)
    let type_name = component
        .type_name
        .name
        .iter()
        .map(|t| t.text.to_string())
        .collect::<Vec<_>>()
        .join(".");

    let mut param = flat::FunctionParam::new(name, type_name);

    // Get array dimensions from shape (resolved) or shape_expr (expressions).
    // For variable-size arrays (e.g., `Real x[:]`), use [0] as a sentinel
    // so that code generators know the parameter is an array even when
    // the exact size is unknown at compile time.
    if !component.shape.is_empty() {
        let dims: Vec<i64> = component.shape.iter().map(|&d| d as i64).collect();
        param = param.with_dims(dims);
    } else if !component.shape_expr.is_empty() {
        let dims: Vec<i64> = component
            .shape_expr
            .iter()
            .filter_map(extract_integer_from_subscript)
            .collect();

        if !dims.is_empty() {
            param = param.with_dims(dims);
        } else {
            // Variable-size array: shape_expr has entries (e.g., colon subscripts)
            // but no extractable integer dimensions. Use [0] as sentinel.
            param = param.with_dims(vec![0; component.shape_expr.len()]);
        }
    }

    // Use explicit declaration binding (`= expr`) for default function inputs.
    // Fall back to `start` only for legacy cases where binding is unavailable.
    if component.has_explicit_binding {
        if let Some(binding_expr) = component.binding.as_ref()
            && !matches!(binding_expr, ast::Expression::Empty)
        {
            let qualified = qualify_function_expr(binding_expr, imports, locals);
            param = param.with_default(ast_lower::expression_from_ast_with_def_map(
                &qualified,
                Some(def_map),
            ));
        } else if !matches!(component.start, ast::Expression::Empty) {
            let qualified = qualify_function_expr(&component.start, imports, locals);
            param = param.with_default(ast_lower::expression_from_ast_with_def_map(
                &qualified,
                Some(def_map),
            ));
        }
    }

    // Get description
    if !component.description.is_empty() {
        let desc: Vec<_> = component
            .description
            .iter()
            .map(|t| t.text.to_string())
            .collect();
        param.description = Some(desc.join(" "));
    }

    param
}

const FUNCTION_QUALIFY_OPTS: qualify::QualifyOptions = qualify::QualifyOptions {
    skip_local: true,
    preserve_def_id: true,
};

fn qualify_function_expr(
    expr: &ast::Expression,
    imports: &qualify::ImportMap,
    locals: &HashSet<String>,
) -> ast::Expression {
    qualify::qualify_expression_with_imports_and_locals(
        expr,
        &ast::QualifiedName::new(),
        FUNCTION_QUALIFY_OPTS,
        locals,
        imports,
    )
}

// =============================================================================
// Post-collection passes: record decomposition and array size insertion
// =============================================================================

/// Decompose record-typed function parameters and rewrite call sites.
///
/// For each function with a known record-typed input (e.g., `Complex`):
/// 1. Replace the record input with scalar field inputs in the signature.
/// 2. Rewrite FieldAccess in the body to VarRef.
/// 3. Walk all equations/functions and decompose call-site arguments.
pub(crate) fn lower_record_function_params(flat: &mut flat::Model) {
    let mut decomposition_map: HashMap<String, Vec<DecomposedParam>> = HashMap::new();

    for (func_name, func) in flat.functions.iter_mut() {
        let mut decomposed: Vec<DecomposedParam> = Vec::new();
        for (idx, input) in func.inputs.iter().enumerate() {
            if let Some(fields) = record_fields(&input.type_name) {
                decomposed.push(DecomposedParam {
                    original_index: idx,
                    param_name: input.name.clone(),
                    fields: fields.to_vec(),
                });
            }
        }
        if decomposed.is_empty() {
            continue;
        }

        // Rewrite FieldAccess in body
        let param_names: HashSet<String> =
            decomposed.iter().map(|d| d.param_name.clone()).collect();
        for stmt in &mut func.body {
            rewrite_field_access_in_statement(stmt, &param_names);
        }

        // Replace record inputs with scalar field inputs
        let old_inputs = std::mem::take(&mut func.inputs);
        for (idx, input) in old_inputs.into_iter().enumerate() {
            let Some(dp) = decomposed.iter().find(|d| d.original_index == idx) else {
                func.inputs.push(input);
                continue;
            };
            for field in &dp.fields {
                func.inputs.push(flat::FunctionParam::new(
                    format!("{}_{}", dp.param_name, field),
                    "Real",
                ));
            }
        }

        decomposition_map.insert(func_name.as_str().to_string(), decomposed);
    }

    if decomposition_map.is_empty() {
        return;
    }

    // Rewrite call sites in equations, variable bindings, and function bodies.
    for eq in &mut flat.equations {
        decompose_record_call_args_in_expr(&mut eq.residual, &decomposition_map);
    }
    for eq in &mut flat.initial_equations {
        decompose_record_call_args_in_expr(&mut eq.residual, &decomposition_map);
    }
    for var in flat.variables.values_mut() {
        if let Some(ref mut binding) = var.binding {
            decompose_record_call_args_in_expr(binding, &decomposition_map);
        }
        if let Some(ref mut start) = var.start {
            decompose_record_call_args_in_expr(start, &decomposition_map);
        }
    }
    for func in flat.functions.values_mut() {
        for stmt in &mut func.body {
            decompose_record_call_args_in_stmt(stmt, &decomposition_map);
        }
    }
}

struct DecomposedParam {
    original_index: usize,
    param_name: String,
    fields: Vec<&'static str>,
}

fn named_constructor_arg<'a>(
    ctor_args: &'a [flat::Expression],
    field: &str,
) -> Option<&'a flat::Expression> {
    for arg in ctor_args {
        if let flat::Expression::FunctionCall {
            name,
            args,
            is_constructor: true,
        } = arg
            && name.as_str().strip_prefix("__rumoca_named_arg__.") == Some(field)
        {
            return args.first();
        }
    }
    None
}

fn decompose_record_call_args_in_stmt(
    stmt: &mut flat::Statement,
    map: &HashMap<String, Vec<DecomposedParam>>,
) {
    match stmt {
        flat::Statement::Assignment { value, .. } => {
            decompose_record_call_args_in_expr(value, map);
        }
        flat::Statement::For { indices, equations } => {
            for idx in indices {
                decompose_record_call_args_in_expr(&mut idx.range, map);
            }
            for inner in equations {
                decompose_record_call_args_in_stmt(inner, map);
            }
        }
        flat::Statement::While(block) => {
            decompose_record_call_args_in_expr(&mut block.cond, map);
            for inner in &mut block.stmts {
                decompose_record_call_args_in_stmt(inner, map);
            }
        }
        flat::Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                decompose_record_call_args_in_expr(&mut block.cond, map);
                for inner in &mut block.stmts {
                    decompose_record_call_args_in_stmt(inner, map);
                }
            }
            if let Some(stmts) = else_block {
                for inner in stmts {
                    decompose_record_call_args_in_stmt(inner, map);
                }
            }
        }
        flat::Statement::FunctionCall { args, outputs, .. } => {
            for arg in args {
                decompose_record_call_args_in_expr(arg, map);
            }
            for out in outputs {
                decompose_record_call_args_in_expr(out, map);
            }
        }
        flat::Statement::Reinit { value, .. } => {
            decompose_record_call_args_in_expr(value, map);
        }
        flat::Statement::Assert {
            condition,
            message,
            level,
        } => {
            decompose_record_call_args_in_expr(condition, map);
            decompose_record_call_args_in_expr(message, map);
            if let Some(l) = level {
                decompose_record_call_args_in_expr(l, map);
            }
        }
        flat::Statement::When(blocks) => {
            for block in blocks {
                decompose_record_call_args_in_expr(&mut block.cond, map);
                for inner in &mut block.stmts {
                    decompose_record_call_args_in_stmt(inner, map);
                }
            }
        }
        flat::Statement::Empty | flat::Statement::Return | flat::Statement::Break => {}
    }
}

fn decompose_record_call_args_in_expr(
    expr: &mut flat::Expression,
    map: &HashMap<String, Vec<DecomposedParam>>,
) {
    match expr {
        flat::Expression::FunctionCall { name, args, .. } => {
            for arg in args.iter_mut() {
                decompose_record_call_args_in_expr(arg, map);
            }
            let Some(decomposed) = map.get(name.as_str()) else {
                return;
            };
            let old_args = std::mem::take(args);
            let mut old_idx = 0;
            for dp in decomposed {
                while old_idx < dp.original_index && old_idx < old_args.len() {
                    args.push(old_args[old_idx].clone());
                    old_idx += 1;
                }
                if old_idx < old_args.len() {
                    expand_record_arg(&old_args[old_idx], &dp.fields, args);
                    old_idx += 1;
                }
            }
            while old_idx < old_args.len() {
                args.push(old_args[old_idx].clone());
                old_idx += 1;
            }
        }
        flat::Expression::Binary { lhs, rhs, .. } => {
            decompose_record_call_args_in_expr(lhs, map);
            decompose_record_call_args_in_expr(rhs, map);
        }
        flat::Expression::Unary { rhs, .. } => {
            decompose_record_call_args_in_expr(rhs, map);
        }
        flat::Expression::BuiltinCall { args, .. } => {
            for arg in args {
                decompose_record_call_args_in_expr(arg, map);
            }
        }
        flat::Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                decompose_record_call_args_in_expr(cond, map);
                decompose_record_call_args_in_expr(then_expr, map);
            }
            decompose_record_call_args_in_expr(else_branch, map);
        }
        flat::Expression::Array { elements, .. } | flat::Expression::Tuple { elements } => {
            for elem in elements {
                decompose_record_call_args_in_expr(elem, map);
            }
        }
        flat::Expression::Range { start, step, end } => {
            decompose_record_call_args_in_expr(start, map);
            if let Some(s) = step {
                decompose_record_call_args_in_expr(s, map);
            }
            decompose_record_call_args_in_expr(end, map);
        }
        flat::Expression::Index { base, .. } | flat::Expression::FieldAccess { base, .. } => {
            decompose_record_call_args_in_expr(base, map);
        }
        flat::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            decompose_record_call_args_in_expr(expr, map);
            for idx in indices {
                decompose_record_call_args_in_expr(&mut idx.range, map);
            }
            if let Some(f) = filter {
                decompose_record_call_args_in_expr(f, map);
            }
        }
        flat::Expression::VarRef { .. }
        | flat::Expression::Literal(_)
        | flat::Expression::Empty => {}
    }
}

/// Expand a record argument into scalar field arguments.
fn expand_record_arg(arg: &flat::Expression, fields: &[&str], out: &mut Vec<flat::Expression>) {
    // Constructor call Complex(re, im) → extract positional/named args
    if let flat::Expression::FunctionCall {
        args: ctor_args,
        is_constructor: true,
        ..
    } = arg
    {
        let positional: Vec<&flat::Expression> = ctor_args
            .iter()
            .filter(|a| {
                !matches!(a, flat::Expression::FunctionCall { is_constructor: true, name, .. }
                if name.as_str().starts_with("__rumoca_named_arg__."))
            })
            .collect();

        for (i, field) in fields.iter().enumerate() {
            let named = named_constructor_arg(ctor_args, field);
            if let Some(val) = named {
                out.push(val.clone());
            } else if i < positional.len() {
                out.push(positional[i].clone());
            } else {
                out.push(flat::Expression::Literal(flat::Literal::Real(0.0)));
            }
        }
        return;
    }

    // Variable reference → emit field VarRefs
    if let flat::Expression::VarRef { name, .. } = arg {
        for field in fields {
            out.push(flat::Expression::VarRef {
                name: flat::VarName::new(format!("{}.{}", name.as_str(), field)),
                subscripts: vec![],
            });
        }
        return;
    }

    // General expression → emit FieldAccess
    for field in fields {
        out.push(flat::Expression::FieldAccess {
            base: Box::new(arg.clone()),
            field: field.to_string(),
        });
    }
}

/// Insert size arguments for variable-size array parameters at call sites.
pub(crate) fn insert_array_size_args(flat: &mut flat::Model) {
    let array_param_map: HashMap<String, Vec<usize>> = flat
        .functions
        .iter()
        .filter_map(|(name, func)| {
            let indices: Vec<usize> = func
                .inputs
                .iter()
                .enumerate()
                .filter(|(_, p)| !p.dims.is_empty())
                .map(|(i, _)| i)
                .collect();
            if indices.is_empty() {
                None
            } else {
                Some((name.as_str().to_string(), indices))
            }
        })
        .collect();

    if array_param_map.is_empty() {
        return;
    }

    for eq in &mut flat.equations {
        insert_size_args_in_expr(&mut eq.residual, &array_param_map);
    }
    for eq in &mut flat.initial_equations {
        insert_size_args_in_expr(&mut eq.residual, &array_param_map);
    }
    for var in flat.variables.values_mut() {
        if let Some(ref mut binding) = var.binding {
            insert_size_args_in_expr(binding, &array_param_map);
        }
    }
    for func in flat.functions.values_mut() {
        for stmt in &mut func.body {
            insert_size_args_in_stmt(stmt, &array_param_map);
        }
    }
}

fn insert_size_args_in_stmt(stmt: &mut flat::Statement, map: &HashMap<String, Vec<usize>>) {
    match stmt {
        flat::Statement::Assignment { value, .. } => insert_size_args_in_expr(value, map),
        flat::Statement::For { indices, equations } => {
            for idx in indices {
                insert_size_args_in_expr(&mut idx.range, map);
            }
            for inner in equations {
                insert_size_args_in_stmt(inner, map);
            }
        }
        flat::Statement::While(block) => {
            insert_size_args_in_expr(&mut block.cond, map);
            for inner in &mut block.stmts {
                insert_size_args_in_stmt(inner, map);
            }
        }
        flat::Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                insert_size_args_in_expr(&mut block.cond, map);
                for inner in &mut block.stmts {
                    insert_size_args_in_stmt(inner, map);
                }
            }
            if let Some(stmts) = else_block {
                for inner in stmts {
                    insert_size_args_in_stmt(inner, map);
                }
            }
        }
        flat::Statement::FunctionCall { args, outputs, .. } => {
            for arg in args.iter_mut() {
                insert_size_args_in_expr(arg, map);
            }
            for out in outputs {
                insert_size_args_in_expr(out, map);
            }
        }
        flat::Statement::Reinit { value, .. } => insert_size_args_in_expr(value, map),
        flat::Statement::Assert {
            condition,
            message,
            level,
        } => {
            insert_size_args_in_expr(condition, map);
            insert_size_args_in_expr(message, map);
            if let Some(l) = level {
                insert_size_args_in_expr(l, map);
            }
        }
        flat::Statement::When(blocks) => {
            for block in blocks {
                insert_size_args_in_expr(&mut block.cond, map);
                for inner in &mut block.stmts {
                    insert_size_args_in_stmt(inner, map);
                }
            }
        }
        flat::Statement::Empty | flat::Statement::Return | flat::Statement::Break => {}
    }
}

fn insert_size_args_in_expr(expr: &mut flat::Expression, map: &HashMap<String, Vec<usize>>) {
    match expr {
        flat::Expression::FunctionCall { name, args, .. } => {
            for arg in args.iter_mut() {
                insert_size_args_in_expr(arg, map);
            }
            let Some(array_indices) = map.get(name.as_str()) else {
                return;
            };
            let mut new_args = std::mem::take(args);
            for &param_idx in array_indices.iter().rev() {
                if param_idx < new_args.len() {
                    let size_expr = flat::Expression::BuiltinCall {
                        function: flat::BuiltinFunction::Size,
                        args: vec![
                            new_args[param_idx].clone(),
                            flat::Expression::Literal(flat::Literal::Integer(1)),
                        ],
                    };
                    new_args.insert(param_idx + 1, size_expr);
                }
            }
            *args = new_args;
        }
        flat::Expression::Binary { lhs, rhs, .. } => {
            insert_size_args_in_expr(lhs, map);
            insert_size_args_in_expr(rhs, map);
        }
        flat::Expression::Unary { rhs, .. } => insert_size_args_in_expr(rhs, map),
        flat::Expression::BuiltinCall { args, .. } => {
            for arg in args {
                insert_size_args_in_expr(arg, map);
            }
        }
        flat::Expression::If {
            branches,
            else_branch,
        } => {
            for (c, t) in branches {
                insert_size_args_in_expr(c, map);
                insert_size_args_in_expr(t, map);
            }
            insert_size_args_in_expr(else_branch, map);
        }
        flat::Expression::Array { elements, .. } | flat::Expression::Tuple { elements } => {
            for e in elements {
                insert_size_args_in_expr(e, map);
            }
        }
        flat::Expression::Range { start, step, end } => {
            insert_size_args_in_expr(start, map);
            if let Some(s) = step {
                insert_size_args_in_expr(s, map);
            }
            insert_size_args_in_expr(end, map);
        }
        flat::Expression::Index { base, .. } | flat::Expression::FieldAccess { base, .. } => {
            insert_size_args_in_expr(base, map);
        }
        flat::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            insert_size_args_in_expr(expr, map);
            for idx in indices {
                insert_size_args_in_expr(&mut idx.range, map);
            }
            if let Some(f) = filter {
                insert_size_args_in_expr(f, map);
            }
        }
        flat::Expression::VarRef { .. }
        | flat::Expression::Literal(_)
        | flat::Expression::Empty => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_collect_no_function_calls() {
        let flat = flat::Model::new();
        let calls = collect_function_calls(&flat);
        assert!(calls.is_empty());
    }

    #[test]
    fn test_collect_function_call_in_equation() {
        let mut flat = flat::Model::new();

        // Create an equation with a function call: 0 = myFunc(x) - y
        let func_call = flat::Expression::FunctionCall {
            name: flat::VarName::new("MyPackage.myFunc"),
            args: vec![flat::Expression::VarRef {
                name: flat::VarName::new("x"),
                subscripts: vec![],
            }],
            is_constructor: false,
        };
        let residual = flat::Expression::Binary {
            op: rumoca_ir_flat::OpBinary::Sub(rumoca_ir_flat::Token::default()),
            lhs: Box::new(func_call),
            rhs: Box::new(flat::Expression::VarRef {
                name: flat::VarName::new("y"),
                subscripts: vec![],
            }),
        };
        flat.add_equation(flat::Equation::new(
            residual,
            Span::DUMMY,
            rumoca_ir_flat::EquationOrigin::ComponentEquation {
                component: "test".to_string(),
            },
        ));

        let calls = collect_function_calls(&flat);
        assert!(calls.contains("MyPackage.myFunc"));
        assert_eq!(calls.len(), 1);
    }

    #[test]
    fn test_collect_nested_function_calls() {
        let mut flat = flat::Model::new();

        // Create: 0 = outer(inner(x)) - y
        let inner_call = flat::Expression::FunctionCall {
            name: flat::VarName::new("inner"),
            args: vec![flat::Expression::VarRef {
                name: flat::VarName::new("x"),
                subscripts: vec![],
            }],
            is_constructor: false,
        };
        let outer_call = flat::Expression::FunctionCall {
            name: flat::VarName::new("outer"),
            args: vec![inner_call],
            is_constructor: false,
        };
        let residual = flat::Expression::Binary {
            op: rumoca_ir_flat::OpBinary::Sub(rumoca_ir_flat::Token::default()),
            lhs: Box::new(outer_call),
            rhs: Box::new(flat::Expression::VarRef {
                name: flat::VarName::new("y"),
                subscripts: vec![],
            }),
        };
        flat.add_equation(flat::Equation::new(
            residual,
            Span::DUMMY,
            rumoca_ir_flat::EquationOrigin::ComponentEquation {
                component: "test".to_string(),
            },
        ));

        let calls = collect_function_calls(&flat);
        assert!(calls.contains("inner"));
        assert!(calls.contains("outer"));
        assert_eq!(calls.len(), 2);
    }

    #[test]
    fn test_convert_component_to_param_prefers_binding_over_start_default() {
        let component = ast::Component {
            type_name: ast::Name::from_string("Real"),
            has_explicit_binding: true,
            start: ast::Expression::Terminal {
                terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
                token: rumoca_ir_core::Token {
                    text: "0".into(),
                    ..Default::default()
                },
            },
            binding: Some(ast::Expression::Terminal {
                terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
                token: rumoca_ir_core::Token {
                    text: "3".into(),
                    ..Default::default()
                },
            }),
            ..Default::default()
        };

        let def_map = indexmap::IndexMap::new();
        let param = convert_component_to_param(
            "m",
            &component,
            &def_map,
            &qualify::ImportMap::default(),
            &HashSet::new(),
        );
        assert!(matches!(
            param.default,
            Some(flat::Expression::Literal(flat::Literal::Integer(3)))
        ));
    }

    #[test]
    fn test_extract_derivative_annotation_simple() {
        use rumoca_ir_ast::{ComponentRefPart, ComponentReference, Token};
        use std::sync::Arc;

        // Test: annotation(derivative = myFunc_der)
        let annotations = vec![ast::Expression::NamedArgument {
            name: Token {
                text: Arc::from("derivative"),
                ..Default::default()
            },
            value: Arc::new(ast::Expression::ComponentReference(ComponentReference {
                local: false,
                def_id: None,
                parts: vec![ComponentRefPart {
                    ident: Token {
                        text: Arc::from("myFunc_der"),
                        ..Default::default()
                    },
                    subs: None,
                }],
            })),
        }];

        let derivs = extract_derivative_annotations(&annotations);
        assert_eq!(derivs.len(), 1);
        assert_eq!(derivs[0].derivative_function, "myFunc_der");
        assert_eq!(derivs[0].order, 1);
        assert!(derivs[0].zero_derivative.is_empty());
        assert!(derivs[0].no_derivative.is_empty());
    }

    #[test]
    fn test_extract_derivative_annotation_with_modification() {
        use rumoca_ir_ast::{ComponentRefPart, ComponentReference, Token};
        use std::sync::Arc;

        // Test: annotation(derivative(order=2) = myFunc_der2)
        // This is represented as a Modification with target having subscripts
        let annotations = vec![ast::Expression::Modification {
            target: ComponentReference {
                local: false,
                def_id: None,
                parts: vec![ComponentRefPart {
                    ident: Token {
                        text: Arc::from("derivative"),
                        ..Default::default()
                    },
                    subs: Some(vec![ast::Subscript::Expression(
                        ast::Expression::NamedArgument {
                            name: Token {
                                text: Arc::from("order"),
                                ..Default::default()
                            },
                            value: Arc::new(ast::Expression::Terminal {
                                terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
                                token: Token {
                                    text: Arc::from("2"),
                                    ..Default::default()
                                },
                            }),
                        },
                    )]),
                }],
            },
            value: Arc::new(ast::Expression::ComponentReference(ComponentReference {
                local: false,
                def_id: None,
                parts: vec![ComponentRefPart {
                    ident: Token {
                        text: Arc::from("myFunc_der2"),
                        ..Default::default()
                    },
                    subs: None,
                }],
            })),
        }];

        let derivs = extract_derivative_annotations(&annotations);
        assert_eq!(derivs.len(), 1);
        assert_eq!(derivs[0].derivative_function, "myFunc_der2");
        assert_eq!(derivs[0].order, 2);
    }

    #[test]
    fn test_extract_derivative_annotation_with_zero_derivative() {
        use rumoca_ir_ast::{ComponentRefPart, ComponentReference, Token};
        use std::sync::Arc;

        // Test: annotation(derivative(zeroDerivative=k) = myFunc_der)
        let annotations = vec![ast::Expression::Modification {
            target: ComponentReference {
                local: false,
                def_id: None,
                parts: vec![ComponentRefPart {
                    ident: Token {
                        text: Arc::from("derivative"),
                        ..Default::default()
                    },
                    subs: Some(vec![ast::Subscript::Expression(
                        ast::Expression::NamedArgument {
                            name: Token {
                                text: Arc::from("zeroDerivative"),
                                ..Default::default()
                            },
                            value: Arc::new(ast::Expression::ComponentReference(
                                ComponentReference {
                                    local: false,
                                    def_id: None,
                                    parts: vec![ComponentRefPart {
                                        ident: Token {
                                            text: Arc::from("k"),
                                            ..Default::default()
                                        },
                                        subs: None,
                                    }],
                                },
                            )),
                        },
                    )]),
                }],
            },
            value: Arc::new(ast::Expression::ComponentReference(ComponentReference {
                local: false,
                def_id: None,
                parts: vec![ComponentRefPart {
                    ident: Token {
                        text: Arc::from("myFunc_der"),
                        ..Default::default()
                    },
                    subs: None,
                }],
            })),
        }];

        let derivs = extract_derivative_annotations(&annotations);
        assert_eq!(derivs.len(), 1);
        assert_eq!(derivs[0].derivative_function, "myFunc_der");
        assert_eq!(derivs[0].order, 1);
        assert_eq!(derivs[0].zero_derivative, vec!["k"]);
    }
}
