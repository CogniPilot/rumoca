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
pub fn collect_function_calls(flat: &flat::Model) -> HashSet<String> {
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
pub fn collect_functions(
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
pub fn lookup_function(tree: &ast::ClassTree, func_name: &str) -> Option<flat::Function> {
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
    def_map: &indexmap::IndexMap<rumoca_ir_ast::DefId, String>,
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
    def_map: &indexmap::IndexMap<rumoca_ir_ast::DefId, String>,
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
            ast::Causality::Input(_) => func.add_input(param),
            ast::Causality::Output(_) => func.add_output(param),
            ast::Causality::Empty => func.add_local(param),
        }
    }

    for alg in &context.algorithms {
        let flat_alg = algorithms::flatten_algorithm_section(
            alg,
            &prefix,
            span,
            qualified_name.to_string(),
            &import_map,
            Some(def_map),
            &function_locals,
        )?;
        func.body.extend(flat_alg.statements);
    }

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
                ast::Variability::Constant(_) | ast::Variability::Parameter(_)
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
    def_map: &indexmap::IndexMap<rumoca_ir_ast::DefId, String>,
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
    def_map: &indexmap::IndexMap<rumoca_ir_ast::DefId, String>,
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
    def_map: &indexmap::IndexMap<rumoca_ir_ast::DefId, String>,
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

    // Get array dimensions from shape (resolved) or shape_expr (expressions)
    // First try the resolved shape
    if !component.shape.is_empty() {
        let dims: Vec<i64> = component.shape.iter().map(|&d| d as i64).collect();
        param = param.with_dims(dims);
    } else if !component.shape_expr.is_empty() {
        // Try to extract constant integer dimensions from shape_expr
        let dims: Vec<i64> = component
            .shape_expr
            .iter()
            .filter_map(extract_integer_from_subscript)
            .collect();

        if !dims.is_empty() {
            param = param.with_dims(dims);
        }
    }

    // Use explicit declaration binding (`= expr`) for default function inputs.
    // Fall back to `start` only for legacy cases where binding is unavailable.
    if component.has_explicit_binding {
        if let Some(binding_expr) = component.binding.as_ref()
            && !matches!(binding_expr, ast::Expression::Empty)
        {
            let qualified = qualify_function_expr(binding_expr, imports, locals);
            param = param.with_default(flat::Expression::from_ast_with_def_map(
                &qualified,
                Some(def_map),
            ));
        } else if !matches!(component.start, ast::Expression::Empty) {
            let qualified = qualify_function_expr(&component.start, imports, locals);
            param = param.with_default(flat::Expression::from_ast_with_def_map(
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
            op: rumoca_ir_ast::OpBinary::Sub(rumoca_ir_ast::Token::default()),
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
            op: rumoca_ir_ast::OpBinary::Sub(rumoca_ir_ast::Token::default()),
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
                token: rumoca_ir_ast::Token {
                    text: "0".into(),
                    ..Default::default()
                },
            },
            binding: Some(ast::Expression::Terminal {
                terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
                token: rumoca_ir_ast::Token {
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
