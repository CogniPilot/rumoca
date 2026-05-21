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
use rumoca_ir_ast as ast;
use rumoca_ir_flat as flat;
use std::collections::{HashMap, HashSet};

use crate::algorithms;
use crate::ast_lower;
use crate::errors::FlattenError;
use crate::qualify;

const NON_FUNCTION_SCALAR_TYPES: &[&str] = &[
    "Real",
    "Integer",
    "Boolean",
    "String",
    "Clock",
    "Complex",
    "Modelica.ComplexMath.Complex",
];
type StaticBindingPairs = Vec<(String, String)>;
type SpecializationKey = (String, StaticBindingPairs);
type SpecializationCache = HashMap<SpecializationKey, flat::VarName>;
type StaticBindingResult = (SpecializationKey, String);

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

/// Specialize static function-typed parameters at flatten level.
///
/// For calls where a function-typed input has a concrete static binding
/// (explicit argument or function default), clone the callee into a specialized
/// function and rewrite local function-parameter calls to the concrete target.
///
/// MLS §12.4.1/§12.4.2: function formals are lexically scoped and call-time
/// bound; this pass resolves static bindings early for compile-time overhead
/// reduction while preserving semantics.
pub(crate) fn specialize_static_function_params(flat: &mut flat::Model) {
    let function_index = flat.functions.clone();
    let mut cache: SpecializationCache = HashMap::new();
    let mut pending_new_functions: Vec<flat::Function> = Vec::new();

    specialize_equation_lists(
        flat,
        &function_index,
        &mut cache,
        &mut pending_new_functions,
    );
    specialize_variable_annotations(
        flat,
        &function_index,
        &mut cache,
        &mut pending_new_functions,
    );
    specialize_assertions(
        flat,
        &function_index,
        &mut cache,
        &mut pending_new_functions,
    );
    specialize_when_clauses(
        flat,
        &function_index,
        &mut cache,
        &mut pending_new_functions,
    );
    specialize_algorithms(
        flat,
        &function_index,
        &mut cache,
        &mut pending_new_functions,
    );
    specialize_functions(
        flat,
        &function_index,
        &mut cache,
        &mut pending_new_functions,
    );

    for func in pending_new_functions {
        flat.functions.insert(func.name.clone(), func);
    }
}

fn specialize_equation_lists(
    flat: &mut flat::Model,
    function_index: &IndexMap<flat::VarName, flat::Function>,
    cache: &mut SpecializationCache,
    pending_new_functions: &mut Vec<flat::Function>,
) {
    for eq in &mut flat.equations {
        specialize_in_expr(
            &mut eq.residual,
            function_index,
            cache,
            pending_new_functions,
        );
    }
    for eq in &mut flat.initial_equations {
        specialize_in_expr(
            &mut eq.residual,
            function_index,
            cache,
            pending_new_functions,
        );
    }
}

fn specialize_variable_annotations(
    flat: &mut flat::Model,
    function_index: &IndexMap<flat::VarName, flat::Function>,
    cache: &mut SpecializationCache,
    pending_new_functions: &mut Vec<flat::Function>,
) {
    for var in flat.variables.values_mut() {
        for expr in [
            &mut var.binding,
            &mut var.start,
            &mut var.min,
            &mut var.max,
            &mut var.nominal,
        ] {
            rewrite_opt_expr(expr, function_index, cache, pending_new_functions);
        }
    }
}

fn specialize_assertions(
    flat: &mut flat::Model,
    function_index: &IndexMap<flat::VarName, flat::Function>,
    cache: &mut SpecializationCache,
    pending_new_functions: &mut Vec<flat::Function>,
) {
    for assertion in &mut flat.assert_equations {
        specialize_in_expr(
            &mut assertion.condition,
            function_index,
            cache,
            pending_new_functions,
        );
        specialize_in_expr(
            &mut assertion.message,
            function_index,
            cache,
            pending_new_functions,
        );
        rewrite_opt_expr(
            &mut assertion.level,
            function_index,
            cache,
            pending_new_functions,
        );
    }
    for assertion in &mut flat.initial_assert_equations {
        specialize_in_expr(
            &mut assertion.condition,
            function_index,
            cache,
            pending_new_functions,
        );
        specialize_in_expr(
            &mut assertion.message,
            function_index,
            cache,
            pending_new_functions,
        );
        rewrite_opt_expr(
            &mut assertion.level,
            function_index,
            cache,
            pending_new_functions,
        );
    }
}

fn specialize_when_clauses(
    flat: &mut flat::Model,
    function_index: &IndexMap<flat::VarName, flat::Function>,
    cache: &mut SpecializationCache,
    pending_new_functions: &mut Vec<flat::Function>,
) {
    for when in &mut flat.when_clauses {
        specialize_in_expr(
            &mut when.condition,
            function_index,
            cache,
            pending_new_functions,
        );
        for eq in &mut when.equations {
            specialize_in_when_equation(eq, function_index, cache, pending_new_functions);
        }
    }
}

fn specialize_algorithms(
    flat: &mut flat::Model,
    function_index: &IndexMap<flat::VarName, flat::Function>,
    cache: &mut SpecializationCache,
    pending_new_functions: &mut Vec<flat::Function>,
) {
    for alg in &mut flat.algorithms {
        for stmt in &mut alg.statements {
            specialize_in_statement(stmt, function_index, cache, pending_new_functions);
        }
    }
    for alg in &mut flat.initial_algorithms {
        for stmt in &mut alg.statements {
            specialize_in_statement(stmt, function_index, cache, pending_new_functions);
        }
    }
}

fn specialize_functions(
    flat: &mut flat::Model,
    function_index: &IndexMap<flat::VarName, flat::Function>,
    cache: &mut SpecializationCache,
    pending_new_functions: &mut Vec<flat::Function>,
) {
    for func in flat.functions.values_mut() {
        for param in func
            .inputs
            .iter_mut()
            .chain(func.outputs.iter_mut())
            .chain(func.locals.iter_mut())
        {
            rewrite_opt_expr(
                &mut param.default,
                function_index,
                cache,
                pending_new_functions,
            );
        }
        for stmt in &mut func.body {
            specialize_in_statement(stmt, function_index, cache, pending_new_functions);
        }
    }
}

fn rewrite_opt_expr(
    expr: &mut Option<flat::Expression>,
    function_index: &IndexMap<flat::VarName, flat::Function>,
    cache: &mut SpecializationCache,
    pending_new_functions: &mut Vec<flat::Function>,
) {
    if let Some(expr) = expr {
        specialize_in_expr(expr, function_index, cache, pending_new_functions);
    }
}

fn specialize_in_statement(
    stmt: &mut flat::Statement,
    function_index: &IndexMap<flat::VarName, flat::Function>,
    cache: &mut SpecializationCache,
    pending_new_functions: &mut Vec<flat::Function>,
) {
    match stmt {
        flat::Statement::Assignment { value, .. } => {
            specialize_in_expr(value, function_index, cache, pending_new_functions);
        }
        flat::Statement::For { indices, equations } => {
            for idx in indices {
                specialize_in_expr(&mut idx.range, function_index, cache, pending_new_functions);
            }
            for nested in equations {
                specialize_in_statement(nested, function_index, cache, pending_new_functions);
            }
        }
        flat::Statement::While(block) => {
            specialize_in_expr(
                &mut block.cond,
                function_index,
                cache,
                pending_new_functions,
            );
            for nested in &mut block.stmts {
                specialize_in_statement(nested, function_index, cache, pending_new_functions);
            }
        }
        flat::Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                specialize_in_expr(
                    &mut block.cond,
                    function_index,
                    cache,
                    pending_new_functions,
                );
                for nested in &mut block.stmts {
                    specialize_in_statement(nested, function_index, cache, pending_new_functions);
                }
            }
            if let Some(stmts) = else_block {
                for nested in stmts {
                    specialize_in_statement(nested, function_index, cache, pending_new_functions);
                }
            }
        }
        flat::Statement::When(blocks) => {
            for block in blocks {
                specialize_in_expr(
                    &mut block.cond,
                    function_index,
                    cache,
                    pending_new_functions,
                );
                for nested in &mut block.stmts {
                    specialize_in_statement(nested, function_index, cache, pending_new_functions);
                }
            }
        }
        flat::Statement::FunctionCall { args, outputs, .. } => {
            for arg in args {
                specialize_in_expr(arg, function_index, cache, pending_new_functions);
            }
            for output in outputs {
                specialize_in_expr(output, function_index, cache, pending_new_functions);
            }
        }
        flat::Statement::Reinit { value, .. } => {
            specialize_in_expr(value, function_index, cache, pending_new_functions);
        }
        flat::Statement::Assert {
            condition,
            message,
            level,
        } => {
            specialize_in_expr(condition, function_index, cache, pending_new_functions);
            specialize_in_expr(message, function_index, cache, pending_new_functions);
            if let Some(level_expr) = level {
                specialize_in_expr(level_expr, function_index, cache, pending_new_functions);
            }
        }
        flat::Statement::Empty | flat::Statement::Return | flat::Statement::Break => {}
    }
}

fn specialize_in_when_equation(
    eq: &mut flat::WhenEquation,
    function_index: &IndexMap<flat::VarName, flat::Function>,
    cache: &mut SpecializationCache,
    pending_new_functions: &mut Vec<flat::Function>,
) {
    match eq {
        flat::WhenEquation::Assign { value, .. } | flat::WhenEquation::Reinit { value, .. } => {
            specialize_in_expr(value, function_index, cache, pending_new_functions);
        }
        flat::WhenEquation::Assert { condition, .. } => {
            specialize_in_expr(condition, function_index, cache, pending_new_functions);
        }
        flat::WhenEquation::Conditional {
            branches,
            else_branch,
            ..
        } => {
            for (condition, equations) in branches {
                specialize_in_expr(condition, function_index, cache, pending_new_functions);
                for nested in equations {
                    specialize_in_when_equation(
                        nested,
                        function_index,
                        cache,
                        pending_new_functions,
                    );
                }
            }
            for nested in else_branch {
                specialize_in_when_equation(nested, function_index, cache, pending_new_functions);
            }
        }
        flat::WhenEquation::FunctionCallOutputs { function, .. } => {
            specialize_in_expr(function, function_index, cache, pending_new_functions);
        }
        flat::WhenEquation::Terminate { .. } => {}
    }
}

fn specialize_in_expr(
    expr: &mut flat::Expression,
    function_index: &IndexMap<flat::VarName, flat::Function>,
    cache: &mut SpecializationCache,
    pending_new_functions: &mut Vec<flat::Function>,
) {
    match expr {
        flat::Expression::Binary { lhs, rhs, .. } => {
            specialize_in_expr(lhs, function_index, cache, pending_new_functions);
            specialize_in_expr(rhs, function_index, cache, pending_new_functions);
        }
        flat::Expression::Unary { rhs, .. } => {
            specialize_in_expr(rhs, function_index, cache, pending_new_functions);
        }
        flat::Expression::BuiltinCall { args, .. } => {
            for arg in args {
                specialize_in_expr(arg, function_index, cache, pending_new_functions);
            }
        }
        flat::Expression::FunctionCall { name, args, .. } => {
            for arg in args.iter_mut() {
                specialize_in_expr(arg, function_index, cache, pending_new_functions);
            }
            rewrite_static_function_call(name, args, function_index, cache, pending_new_functions);
        }
        flat::Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                specialize_in_expr(cond, function_index, cache, pending_new_functions);
                specialize_in_expr(then_expr, function_index, cache, pending_new_functions);
            }
            specialize_in_expr(else_branch, function_index, cache, pending_new_functions);
        }
        flat::Expression::Array { elements, .. } | flat::Expression::Tuple { elements } => {
            for elem in elements {
                specialize_in_expr(elem, function_index, cache, pending_new_functions);
            }
        }
        flat::Expression::Range { start, step, end } => {
            specialize_in_expr(start, function_index, cache, pending_new_functions);
            if let Some(step) = step {
                specialize_in_expr(step, function_index, cache, pending_new_functions);
            }
            specialize_in_expr(end, function_index, cache, pending_new_functions);
        }
        flat::Expression::Index { base, subscripts } => {
            specialize_in_expr(base, function_index, cache, pending_new_functions);
            for sub in subscripts {
                if let flat::Subscript::Expr(inner) = sub {
                    specialize_in_expr(inner, function_index, cache, pending_new_functions);
                }
            }
        }
        flat::Expression::FieldAccess { base, .. } => {
            specialize_in_expr(base, function_index, cache, pending_new_functions);
        }
        flat::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            specialize_in_expr(expr, function_index, cache, pending_new_functions);
            for idx in indices {
                specialize_in_expr(&mut idx.range, function_index, cache, pending_new_functions);
            }
            if let Some(filter) = filter {
                specialize_in_expr(filter, function_index, cache, pending_new_functions);
            }
        }
        flat::Expression::VarRef { .. }
        | flat::Expression::Literal(_)
        | flat::Expression::Empty => {}
    }
}

fn rewrite_static_function_call(
    name: &mut flat::VarName,
    args: &mut [flat::Expression],
    function_index: &IndexMap<flat::VarName, flat::Function>,
    cache: &mut SpecializationCache,
    pending_new_functions: &mut Vec<flat::Function>,
) {
    let Some((key, specialized_name)) =
        build_static_function_binding(name.as_str(), args, function_index)
    else {
        return;
    };
    let Some(specialized) = resolve_specialized_name(
        &key,
        &specialized_name,
        name.as_str(),
        function_index,
        cache,
        pending_new_functions,
    ) else {
        return;
    };
    *name = specialized;
}

fn resolve_specialized_name(
    key: &SpecializationKey,
    specialized_name: &str,
    original_name: &str,
    function_index: &IndexMap<flat::VarName, flat::Function>,
    cache: &mut SpecializationCache,
    pending_new_functions: &mut Vec<flat::Function>,
) -> Option<flat::VarName> {
    if let Some(existing) = cache.get(key) {
        return Some(existing.clone());
    }
    let func =
        create_specialized_function(function_index, original_name, specialized_name, &key.1)?;
    let new_name = func.name.clone();
    cache.insert(key.clone(), new_name.clone());
    pending_new_functions.push(func);
    Some(new_name)
}

fn build_static_function_binding(
    function_name: &str,
    args: &[flat::Expression],
    function_index: &IndexMap<flat::VarName, flat::Function>,
) -> Option<StaticBindingResult> {
    let function = function_index.get(&flat::VarName::new(function_name))?;
    let mut binding_pairs = Vec::new();
    for (index, param) in function.inputs.iter().enumerate() {
        if !is_function_typed_param(param, function_index) {
            continue;
        }
        let arg_expr = args.get(index).or(param.default.as_ref())?;
        let target = function_name_from_expr(arg_expr)?;
        binding_pairs.push((param.name.clone(), target));
    }
    if binding_pairs.is_empty() {
        return None;
    }
    let suffix = binding_pairs
        .iter()
        .map(|(param, target)| format!("{param}={target}"))
        .collect::<Vec<_>>()
        .join(";");
    let specialized_name = format!(
        "{}$spec${}",
        function.name.as_str(),
        simple_hash_hex(&suffix)
    );
    Some((
        (function.name.as_str().to_string(), binding_pairs),
        specialized_name,
    ))
}

fn is_function_typed_param(
    param: &flat::FunctionParam,
    function_index: &IndexMap<flat::VarName, flat::Function>,
) -> bool {
    if NON_FUNCTION_SCALAR_TYPES.contains(&param.type_name.as_str()) {
        return false;
    }
    function_index.contains_key(&flat::VarName::new(param.type_name.as_str()))
        || param.type_name.contains("partial")
}

fn function_name_from_expr(expr: &flat::Expression) -> Option<String> {
    match expr {
        flat::Expression::VarRef { name, subscripts } if subscripts.is_empty() => {
            Some(name.as_str().to_string())
        }
        flat::Expression::FunctionCall {
            name,
            args,
            is_constructor: false,
        } if args.is_empty() => Some(name.as_str().to_string()),
        _ => None,
    }
}

fn create_specialized_function(
    function_index: &IndexMap<flat::VarName, flat::Function>,
    original_name: &str,
    specialized_name: &str,
    bindings: &[(String, String)],
) -> Option<flat::Function> {
    let mut cloned = function_index
        .get(&flat::VarName::new(original_name))?
        .clone();
    cloned.name = flat::VarName::new(specialized_name);
    let binding_map: HashMap<&str, &str> = bindings
        .iter()
        .map(|(param, target)| (param.as_str(), target.as_str()))
        .collect();
    for stmt in &mut cloned.body {
        rewrite_function_param_callees_in_statement(stmt, original_name, &binding_map);
    }
    Some(cloned)
}

fn rewrite_function_param_callees_in_statement(
    stmt: &mut flat::Statement,
    owner_name: &str,
    bindings: &HashMap<&str, &str>,
) {
    match stmt {
        flat::Statement::Assignment { value, .. } => {
            rewrite_function_param_callees_in_expr(value, owner_name, bindings);
        }
        flat::Statement::For { indices, equations } => {
            for idx in indices {
                rewrite_function_param_callees_in_expr(&mut idx.range, owner_name, bindings);
            }
            for nested in equations {
                rewrite_function_param_callees_in_statement(nested, owner_name, bindings);
            }
        }
        flat::Statement::While(block) => {
            rewrite_function_param_callees_in_expr(&mut block.cond, owner_name, bindings);
            for nested in &mut block.stmts {
                rewrite_function_param_callees_in_statement(nested, owner_name, bindings);
            }
        }
        flat::Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                rewrite_function_param_callees_in_expr(&mut block.cond, owner_name, bindings);
                for nested in &mut block.stmts {
                    rewrite_function_param_callees_in_statement(nested, owner_name, bindings);
                }
            }
            if let Some(stmts) = else_block {
                for nested in stmts {
                    rewrite_function_param_callees_in_statement(nested, owner_name, bindings);
                }
            }
        }
        flat::Statement::When(blocks) => {
            for block in blocks {
                rewrite_function_param_callees_in_expr(&mut block.cond, owner_name, bindings);
                for nested in &mut block.stmts {
                    rewrite_function_param_callees_in_statement(nested, owner_name, bindings);
                }
            }
        }
        flat::Statement::FunctionCall { args, outputs, .. } => {
            for arg in args {
                rewrite_function_param_callees_in_expr(arg, owner_name, bindings);
            }
            for output in outputs {
                rewrite_function_param_callees_in_expr(output, owner_name, bindings);
            }
        }
        flat::Statement::Reinit { value, .. } => {
            rewrite_function_param_callees_in_expr(value, owner_name, bindings);
        }
        flat::Statement::Assert {
            condition,
            message,
            level,
        } => {
            rewrite_function_param_callees_in_expr(condition, owner_name, bindings);
            rewrite_function_param_callees_in_expr(message, owner_name, bindings);
            if let Some(level_expr) = level {
                rewrite_function_param_callees_in_expr(level_expr, owner_name, bindings);
            }
        }
        flat::Statement::Empty | flat::Statement::Return | flat::Statement::Break => {}
    }
}

fn rewrite_function_param_callees_in_expr(
    expr: &mut flat::Expression,
    owner_name: &str,
    bindings: &HashMap<&str, &str>,
) {
    match expr {
        flat::Expression::Binary { lhs, rhs, .. } => {
            rewrite_function_param_callees_in_expr(lhs, owner_name, bindings);
            rewrite_function_param_callees_in_expr(rhs, owner_name, bindings);
        }
        flat::Expression::Unary { rhs, .. } => {
            rewrite_function_param_callees_in_expr(rhs, owner_name, bindings);
        }
        flat::Expression::BuiltinCall { args, .. } => {
            for arg in args {
                rewrite_function_param_callees_in_expr(arg, owner_name, bindings);
            }
        }
        flat::Expression::FunctionCall { name, args, .. } => {
            for arg in args {
                rewrite_function_param_callees_in_expr(arg, owner_name, bindings);
            }
            if let Some(rewritten) = rewrite_callee_name(name.as_str(), owner_name, bindings) {
                *name = flat::VarName::new(rewritten);
            }
        }
        flat::Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                rewrite_function_param_callees_in_expr(cond, owner_name, bindings);
                rewrite_function_param_callees_in_expr(then_expr, owner_name, bindings);
            }
            rewrite_function_param_callees_in_expr(else_branch, owner_name, bindings);
        }
        flat::Expression::Array { elements, .. } | flat::Expression::Tuple { elements } => {
            for elem in elements {
                rewrite_function_param_callees_in_expr(elem, owner_name, bindings);
            }
        }
        flat::Expression::Range { start, step, end } => {
            rewrite_function_param_callees_in_expr(start, owner_name, bindings);
            if let Some(step) = step {
                rewrite_function_param_callees_in_expr(step, owner_name, bindings);
            }
            rewrite_function_param_callees_in_expr(end, owner_name, bindings);
        }
        flat::Expression::Index { base, subscripts } => {
            rewrite_function_param_callees_in_expr(base, owner_name, bindings);
            for sub in subscripts {
                if let flat::Subscript::Expr(inner) = sub {
                    rewrite_function_param_callees_in_expr(inner, owner_name, bindings);
                }
            }
        }
        flat::Expression::FieldAccess { base, .. } => {
            rewrite_function_param_callees_in_expr(base, owner_name, bindings);
        }
        flat::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            rewrite_function_param_callees_in_expr(expr, owner_name, bindings);
            for idx in indices {
                rewrite_function_param_callees_in_expr(&mut idx.range, owner_name, bindings);
            }
            if let Some(filter) = filter {
                rewrite_function_param_callees_in_expr(filter, owner_name, bindings);
            }
        }
        flat::Expression::VarRef { .. }
        | flat::Expression::Literal(_)
        | flat::Expression::Empty => {}
    }
}

fn rewrite_callee_name(
    name: &str,
    owner_name: &str,
    bindings: &HashMap<&str, &str>,
) -> Option<String> {
    if let Some(target) = bindings.get(name) {
        return Some((*target).to_string());
    }
    if let Some((owner, member)) = name.rsplit_once('.')
        && owner == owner_name
        && let Some(target) = bindings.get(member)
    {
        return Some((*target).to_string());
    }
    None
}

fn simple_hash_hex(input: &str) -> String {
    use std::hash::{Hash, Hasher};
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    input.hash(&mut hasher);
    format!("{:016x}", hasher.finish())
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
    let (lookup_name, emitted_name) =
        resolve_function_target_with_scope(tree, func_name, caller_scope)?;
    let class_def = tree.get_class_by_qualified_name(&lookup_name)?;
    let flat_func = convert_callable(
        tree,
        class_def,
        &emitted_name,
        &tree.source_map,
        &tree.def_map,
    )?;
    Some((emitted_name, flat_func))
}

fn resolve_function_target_with_scope(
    tree: &ast::ClassTree,
    func_name: &str,
    caller_scope: Option<&str>,
) -> Option<(String, String)> {
    if let Some(class_def) = tree.get_class_by_qualified_name(func_name)
        && is_callable_class_type(&class_def.class_type)
    {
        return Some((func_name.to_string(), func_name.to_string()));
    }

    if let Some((package_name, leaf)) = func_name.rsplit_once('.')
        && tree.get_class_by_qualified_name(package_name).is_some()
        && let Some(inherited_name) =
            crate::pipeline::resolve_function_in_package_chain(tree, package_name, leaf)
    {
        return Some((inherited_name, func_name.to_string()));
    }

    resolve_function_qualified_name_with_scope(tree, func_name, caller_scope)
        .map(|name| (name.clone(), name))
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
    func.partial = class_def.partial;

    // Convert external function declaration (MLS §12.9)
    if let Some(ref ext) = class_def.external {
        func.external = Some(convert_external_function(ext, qualified_name));
    }

    // Extract derivative annotations (MLS §12.7.1)
    func.derivatives = extract_derivative_annotations(&class_def.annotation);

    Ok(func)
}

use crate::function_lowering::rewrite_record_field_access_in_body;

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

// Re-export lowering passes so callers can still use `functions::lower_record_function_params`
// and `functions::insert_array_size_args`.
pub(crate) use crate::function_lowering::{insert_array_size_args, lower_record_function_params};

#[cfg(test)]
#[path = "functions_tests.rs"]
mod functions_tests;
