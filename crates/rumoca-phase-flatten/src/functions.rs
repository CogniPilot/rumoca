//! rumoca_core::Function collection and flattening for user-defined functions.
//!
//! This module is responsible for:
//! - Collecting function calls used in the model
//! - Looking up function definitions from the ast::ClassTree
//! - Converting function definitions to rumoca_core::Function
//!
//! Per MLS §12, functions in Modelica are callable units with:
//! - Input parameters (values passed in)
//! - Output parameters (values returned)
//! - An algorithm section (the function body)
//!
//! SPEC_0021 file-size exception: function collection still coordinates AST
//! function conversion, constructor signatures, lexical aliases, and call
//! canonicalization. split plan: move lexical alias discovery and collected-call
//! canonicalization into focused modules with imports at the top.

use indexmap::IndexSet;
#[cfg(test)]
use rumoca_core::Span;
use rumoca_core::{ExpressionRewriter, ExpressionVisitor, StatementRewriter};
use rumoca_ir_ast as ast;
use rumoca_ir_ast::AstIndexMap as IndexMap;
use rumoca_ir_flat as flat;
use rustc_hash::FxHashSet;
use std::collections::{HashMap, HashSet};

mod call_args;
mod constructor_signature;
mod function_metadata;
mod function_output_validation;
#[cfg(test)]
mod tests;
pub(crate) use call_args::validate_flat_function_call_args;
use constructor_signature::{convert_constructor_signature, normalize_function_local_references};
use function_metadata::*;
pub(crate) use function_metadata::{
    lower_record_function_params, specialize_static_function_params,
};
use function_output_validation::validate_function_outputs_assigned;

use crate::algorithms;
use crate::ast_lower;
use crate::errors::FlattenError;
use crate::function_lowering::rewrite_record_field_access_in_body;
use crate::path_utils;
use crate::pipeline::{collect_package_chain, rewrite_function_extends_aliases_in_function};
use crate::qualify;
use crate::source_spans::required_location_span;

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct FunctionRequest {
    pub(crate) name: String,
    pub(crate) target_def_id: Option<rumoca_core::DefId>,
    component_ref: Option<rumoca_core::ComponentReference>,
}

impl FunctionRequest {
    fn from_reference(reference: &rumoca_core::Reference) -> Self {
        let component_ref = reference.component_ref().cloned();
        let name = component_ref
            .as_ref()
            .map(component_ref_name)
            .unwrap_or_else(|| reference.as_str().to_string());
        Self {
            name,
            target_def_id: reference.target_def_id(),
            component_ref,
        }
    }

    fn from_component_reference(reference: &rumoca_core::ComponentReference) -> Self {
        Self {
            name: component_ref_name(reference),
            target_def_id: reference.def_id,
            component_ref: Some(reference.clone()),
        }
    }

    pub(crate) fn from_resolved_ast_reference(
        name: String,
        reference: &rumoca_ir_ast::ComponentReference,
    ) -> Self {
        Self {
            name,
            target_def_id: reference.def_id,
            component_ref: Some(ast_component_ref_to_core(reference)),
        }
    }

    fn from_name(name: String) -> Self {
        Self {
            name,
            target_def_id: None,
            component_ref: None,
        }
    }
}

fn ast_component_ref_to_core(
    reference: &rumoca_ir_ast::ComponentReference,
) -> rumoca_core::ComponentReference {
    rumoca_core::ComponentReference {
        local: reference.local,
        span: reference.span,
        parts: reference
            .parts
            .iter()
            .map(|part| rumoca_core::ComponentRefPart {
                ident: part.ident.text.to_string(),
                span: reference.span,
                subs: Vec::new(),
            })
            .collect(),
        def_id: reference.def_id,
    }
}

#[derive(Default)]
pub(crate) struct FunctionRequests {
    entries: Vec<FunctionRequest>,
}

impl FunctionRequests {
    pub(crate) fn insert(&mut self, request: FunctionRequest) {
        let _ = self.insert_if_new(request);
    }

    pub(crate) fn insert_if_new(&mut self, request: FunctionRequest) -> bool {
        if self.contains(&request) {
            return false;
        }
        self.entries.push(request);
        true
    }

    pub(crate) fn contains(&self, request: &FunctionRequest) -> bool {
        self.entries
            .iter()
            .any(|existing| same_function_request(existing, request))
    }

    pub(crate) fn into_entries(self) -> Vec<FunctionRequest> {
        self.entries
    }
}

#[derive(Default)]
struct FunctionIdentitySet {
    entries: Vec<FunctionIdentity>,
}

#[derive(Clone)]
struct FunctionIdentity {
    def_id: Option<rumoca_core::DefId>,
    name: String,
}

impl FunctionIdentitySet {
    fn insert_request(&mut self, request: &FunctionRequest) -> bool {
        self.insert_identity(FunctionIdentity {
            def_id: request.target_def_id,
            name: request.name.clone(),
        })
    }

    fn insert_function(&mut self, function: &rumoca_core::Function) -> bool {
        self.insert_identity(FunctionIdentity {
            def_id: function.def_id,
            name: function.name.as_str().to_string(),
        })
    }

    fn contains_function(&self, function: &rumoca_core::Function) -> bool {
        self.entries
            .iter()
            .any(|entry| same_function_identity(entry, function.def_id, function.name.as_str()))
    }

    fn contains_name(&self, name: &str) -> bool {
        self.entries.iter().any(|entry| entry.name == name)
    }

    fn insert_identity(&mut self, identity: FunctionIdentity) -> bool {
        if self
            .entries
            .iter()
            .any(|entry| same_function_identity(entry, identity.def_id, &identity.name))
        {
            return false;
        }
        self.entries.push(identity);
        true
    }
}

fn same_function_identity(
    left: &FunctionIdentity,
    right_def_id: Option<rumoca_core::DefId>,
    right_name: &str,
) -> bool {
    match (left.def_id, right_def_id) {
        (Some(left_id), Some(right_id)) => left_id == right_id && left.name == right_name,
        _ => left.name == right_name,
    }
}

fn is_callable_class_type(class_type: &rumoca_core::ClassType) -> bool {
    !matches!(
        class_type,
        rumoca_core::ClassType::Package
            | rumoca_core::ClassType::Connector
            | rumoca_core::ClassType::Operator
    )
}

fn class_by_name_or_def_id<'a>(
    class_index: &ast::ClassDefIndex<'a>,
    name: &str,
    def_id: Option<rumoca_core::DefId>,
) -> Option<&'a ast::ClassDef> {
    def_id
        .and_then(|def_id| class_index.get(def_id))
        .or_else(|| class_index.get_by_qualified_name(name))
}

fn function_param_type_alias_dims(
    class_index: &ast::ClassDefIndex<'_>,
    component: &ast::Component,
    source_map: &rumoca_core::SourceMap,
) -> Result<Vec<i64>, FlattenError> {
    const MAX_DEPTH: usize = 16;
    let type_name = component.type_name.to_string();
    let mut current = class_by_name_or_def_id(class_index, &type_name, component.type_name.def_id);
    let mut dims = Vec::new();
    let mut visited_defs = HashSet::new();
    let mut visited_names = HashSet::new();

    for _ in 0..MAX_DEPTH {
        let Some(class_def) = current else {
            break;
        };
        if let Some(def_id) = class_def.def_id {
            if !visited_defs.insert(def_id) {
                break;
            }
        } else if !visited_names.insert(class_def.name.text.to_string()) {
            break;
        }

        dims.extend(subscripts_to_param_dims(
            &class_def.array_subscripts,
            class_def.name.text.as_ref(),
            source_map,
        )?);

        let Some(base) = class_def.extends.first() else {
            break;
        };
        let base_name = base.base_name.to_string();
        if rumoca_core::is_builtin_type(&base_name) {
            break;
        }
        current = class_by_name_or_def_id(class_index, &base_name, base.base_def_id);
    }

    Ok(dims)
}

/// Collect all user function calls from a flat::Model.
///
/// Walks through all equations and expressions to find function calls,
/// returning a set of unique function names that need definitions.
#[cfg(test)]
pub(crate) fn collect_function_calls(flat: &flat::Model) -> HashSet<String> {
    collect_function_call_requests(flat)
        .into_iter()
        .map(|request| request.name)
        .collect()
}

fn collect_function_call_requests(flat: &flat::Model) -> Vec<FunctionRequest> {
    let mut calls = FunctionRequests::default();

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

    calls.into_entries()
}

/// Collect function calls from a WhenEquation.
fn collect_from_when_equation(eq: &rumoca_ir_flat::WhenEquation, calls: &mut FunctionRequests) {
    match eq {
        flat::WhenEquation::Assign { value, .. } => {
            collect_from_expression(value, calls);
        }
        flat::WhenEquation::Reinit { value, .. } => {
            collect_from_expression(value, calls);
        }
        flat::WhenEquation::Assert {
            condition, message, ..
        } => {
            collect_from_expression(condition, calls);
            collect_from_expression(message, calls);
        }
        flat::WhenEquation::Terminate { message, .. } => {
            collect_from_expression(message, calls);
        }
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

struct FunctionCallCollector<'a> {
    calls: &'a mut FunctionRequests,
}

impl rumoca_core::ExpressionVisitor for FunctionCallCollector<'_> {
    fn visit_function_call(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        is_constructor: bool,
    ) {
        self.calls.insert(FunctionRequest::from_reference(name));
        self.walk_function_call(name, args, is_constructor);
    }
}

impl rumoca_ir_flat::visitor::StatementVisitor for FunctionCallCollector<'_> {
    fn visit_statement_function_call(
        &mut self,
        comp: &rumoca_core::ComponentReference,
        args: &[rumoca_core::Expression],
        outputs: &[rumoca_core::ComponentReference],
    ) {
        self.calls
            .insert(FunctionRequest::from_component_reference(comp));
        self.visit_component_reference(comp);
        for arg in args {
            self.visit_expression(arg);
        }
        for output in outputs {
            self.visit_component_reference(output);
        }
    }
}

/// Collect function calls from an expression using the visitor pattern.
fn collect_from_expression(expr: &rumoca_core::Expression, calls: &mut FunctionRequests) {
    let mut collector = FunctionCallCollector { calls };
    rumoca_core::ExpressionVisitor::visit_expression(&mut collector, expr);
}

/// Collect and flatten all function definitions used by the model.
///
/// This finds all function calls in the model, looks up their definitions
/// in the ast::ClassTree, and converts them to rumoca_core::Function objects.
pub(crate) fn collect_functions(
    flat: &mut flat::Model,
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    caller_scope: Option<&str>,
) -> Result<(), FlattenError> {
    let mut member_cache = qualify::MemberDefIdCache::default();
    let initial_calls = collect_function_call_requests(flat);
    let mut pending: Vec<(FunctionRequest, Option<String>)> = initial_calls
        .iter()
        .cloned()
        .map(|request| (request, caller_scope.map(str::to_string)))
        .collect();
    pending.extend(
        flat.functions
            .keys()
            .map(|name| (FunctionRequest::from_name(name.as_str().to_string()), None)),
    );
    let mut requested: Vec<(FunctionRequest, Option<String>)> = Vec::new();
    let mut expanded = FunctionIdentitySet::default();
    let mut inserted: Vec<String> = flat
        .functions
        .keys()
        .map(|n| n.as_str().to_string())
        .collect();
    for request in &initial_calls {
        insert_unique_name(&mut inserted, &request.name);
    }

    while let Some((request, caller_scope)) = pending.pop() {
        if request_seen_in_scope(&requested, &request, caller_scope.as_deref()) {
            continue;
        }
        requested.push((request.clone(), caller_scope.clone()));

        if let Some(existing) = existing_executable_flat_function(flat, &request.name) {
            let qualified_name = existing.name.as_str().to_string();
            if !expanded.insert_function(existing) {
                continue;
            }
            queue_unseen_function_dependencies(&mut pending, &requested, &qualified_name, existing);
            insert_unique_name(&mut inserted, &qualified_name);
            continue;
        }

        let resolved = if let Some(resolved) = lookup_function_request_with_scope(
            tree,
            class_index,
            &request,
            caller_scope.as_deref(),
            &mut member_cache,
        )? {
            if is_executable_flat_function(&resolved.1) {
                Some(resolved)
            } else {
                lookup_function_in_known_packages(
                    tree,
                    class_index,
                    &request.name,
                    &inserted,
                    &mut member_cache,
                )?
            }
        } else {
            flat.functions
                .get(&rumoca_core::VarName::new(request.name.clone()))
                .cloned()
                .map(|f| (f.name.as_str().to_string(), f))
        };
        let resolved = match resolved {
            Some(resolved) => Some(resolved),
            None => lookup_function_in_known_packages(
                tree,
                class_index,
                &request.name,
                &inserted,
                &mut member_cache,
            )?,
        };
        let Some((qualified_name, flat_func)) = resolved else {
            // If not found or not a function type, it might be:
            // - An external function (MLS §12.9)
            // - A library function we don't have the source for
            // - A record constructor or operator function (MLS §14)
            // Code generators handle these cases or error appropriately
            continue;
        };

        if !expanded.insert_function(&flat_func) {
            continue;
        }

        for dep in collect_function_dep_requests(&flat_func) {
            if !request_seen_in_scope(&requested, &dep, Some(&qualified_name)) {
                pending.push((dep, Some(qualified_name.clone())));
            }
        }
        insert_unique_name(&mut inserted, &qualified_name);
        if existing_executable_flat_function(flat, flat_func.name.as_str()).is_some() {
            continue;
        }
        flat.add_function(flat_func);
    }

    Ok(())
}

fn existing_executable_flat_function<'model>(
    flat: &'model flat::Model,
    func_name: &str,
) -> Option<&'model rumoca_core::Function> {
    let function = flat.functions.get(&rumoca_core::VarName::new(func_name))?;
    is_executable_flat_function(function).then_some(function)
}

fn function_by_request<'model>(
    flat: &'model flat::Model,
    request: &FunctionRequest,
) -> Option<&'model rumoca_core::Function> {
    if let Some(function) = flat
        .functions
        .get(&rumoca_core::VarName::new(&request.name))
    {
        return Some(function);
    }
    if let Some(def_id) = request.target_def_id
        && let Some(function) = flat
            .functions
            .values()
            .find(|function| function.def_id == Some(def_id))
    {
        return Some(function);
    }
    None
}

fn request_seen_in_scope(
    requested: &[(FunctionRequest, Option<String>)],
    request: &FunctionRequest,
    caller_scope: Option<&str>,
) -> bool {
    requested.iter().any(|(existing, existing_scope)| {
        same_function_request(existing, request) && existing_scope.as_deref() == caller_scope
    })
}

fn same_function_request(left: &FunctionRequest, right: &FunctionRequest) -> bool {
    if let (Some(left_ref), Some(right_ref)) = (&left.component_ref, &right.component_ref)
        && left_ref == right_ref
    {
        return true;
    }
    match (left.target_def_id, right.target_def_id) {
        (Some(left_id), Some(right_id)) => left_id == right_id && left.name == right.name,
        _ => left.name == right.name,
    }
}

fn queue_unseen_function_dependencies(
    pending: &mut Vec<(FunctionRequest, Option<String>)>,
    requested: &[(FunctionRequest, Option<String>)],
    qualified_name: &str,
    function: &rumoca_core::Function,
) {
    for dep in collect_function_dep_requests(function) {
        if !request_seen_in_scope(requested, &dep, Some(qualified_name)) {
            pending.push((dep, Some(qualified_name.to_string())));
        }
    }
}

fn insert_unique_name(names: &mut Vec<String>, name: &str) {
    if !names.iter().any(|existing| existing == name) {
        names.push(name.to_string());
    }
}

pub(crate) fn prune_unreachable_functions(flat: &mut flat::Model) {
    let mut reachable = FunctionIdentitySet::default();
    let mut pending = collect_function_call_requests(flat);
    for request in &pending {
        reachable.insert_request(request);
    }

    while let Some(request) = pending.pop() {
        let Some(function) = function_by_request(flat, &request) else {
            continue;
        };
        for dependency in collect_function_dep_requests(function) {
            if reachable.insert_request(&dependency) {
                pending.push(dependency);
            }
        }
    }

    flat.functions.retain(|name, function| {
        reachable.contains_function(function) || reachable.contains_name(name.as_str())
    });
}

pub(crate) fn validate_flat_function_bindings(flat: &flat::Model) -> Result<(), FlattenError> {
    for function in flat.functions.values() {
        if is_executable_flat_function(function) {
            continue;
        }
        return Err(FlattenError::function_without_body(
            function.name.as_str(),
            function.span,
        ));
    }
    Ok(())
}

pub(crate) fn canonicalize_collected_function_calls(
    flat: &mut flat::Model,
) -> Result<(), FlattenError> {
    let canonical_functions = flat
        .functions
        .values()
        .map(|function| {
            Ok(CanonicalFunction {
                name: function.name.as_str().to_string(),
                def_id: function.def_id,
                instance_id: function
                    .instance_id
                    .ok_or_else(|| FlattenError::internal("missing function instance identity"))?,
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    if canonical_functions.is_empty() {
        return Ok(());
    }
    let mut rewriter = CollectedFunctionCallCanonicalizer {
        canonical_functions,
    };

    for var in flat.variables.values_mut() {
        if let Some(binding) = &mut var.binding {
            *binding = rewriter.rewrite_expression(binding);
        }
        if let Some(start) = &mut var.start {
            *start = rewriter.rewrite_expression(start);
        }
        if let Some(min) = &mut var.min {
            *min = rewriter.rewrite_expression(min);
        }
        if let Some(max) = &mut var.max {
            *max = rewriter.rewrite_expression(max);
        }
        if let Some(nominal) = &mut var.nominal {
            *nominal = rewriter.rewrite_expression(nominal);
        }
    }
    for eq in &mut flat.equations {
        eq.residual = rewriter.rewrite_expression(&eq.residual);
    }
    for eq in &mut flat.initial_equations {
        eq.residual = rewriter.rewrite_expression(&eq.residual);
    }
    for when_clause in &mut flat.when_clauses {
        when_clause.condition = rewriter.rewrite_expression(&when_clause.condition);
        canonicalize_when_equations(&mut when_clause.equations, &mut rewriter);
    }
    for assertion in &mut flat.assert_equations {
        assertion.condition = rewriter.rewrite_expression(&assertion.condition);
        assertion.message = rewriter.rewrite_expression(&assertion.message);
        if let Some(level) = &mut assertion.level {
            *level = rewriter.rewrite_expression(level);
        }
    }
    for assertion in &mut flat.initial_assert_equations {
        assertion.condition = rewriter.rewrite_expression(&assertion.condition);
        assertion.message = rewriter.rewrite_expression(&assertion.message);
        if let Some(level) = &mut assertion.level {
            *level = rewriter.rewrite_expression(level);
        }
    }
    for algorithm in &mut flat.algorithms {
        for statement in &mut algorithm.statements {
            *statement = rewriter.rewrite_statement(statement);
        }
    }
    for algorithm in &mut flat.initial_algorithms {
        for statement in &mut algorithm.statements {
            *statement = rewriter.rewrite_statement(statement);
        }
    }
    for function in flat.functions.values_mut() {
        for input in &mut function.inputs {
            canonicalize_function_param_default(input, &mut rewriter);
        }
        for output in &mut function.outputs {
            canonicalize_function_param_default(output, &mut rewriter);
        }
        for local in &mut function.locals {
            canonicalize_function_param_default(local, &mut rewriter);
        }
        for statement in &mut function.body {
            *statement = rewriter.rewrite_statement(statement);
        }
    }
    Ok(())
}

fn canonicalize_when_equations(
    equations: &mut [flat::WhenEquation],
    rewriter: &mut CollectedFunctionCallCanonicalizer,
) {
    for equation in equations {
        match equation {
            flat::WhenEquation::Assign { value, .. } | flat::WhenEquation::Reinit { value, .. } => {
                *value = rewriter.rewrite_expression(value);
            }
            flat::WhenEquation::Assert {
                condition, message, ..
            } => {
                *condition = rewriter.rewrite_expression(condition);
                *message = rewriter.rewrite_expression(message);
            }
            flat::WhenEquation::Terminate { message, .. } => {
                *message = rewriter.rewrite_expression(message);
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                for (condition, branch_equations) in branches {
                    *condition = rewriter.rewrite_expression(condition);
                    canonicalize_when_equations(branch_equations, rewriter);
                }
                canonicalize_when_equations(else_branch, rewriter);
            }
            flat::WhenEquation::FunctionCallOutputs { function, .. } => {
                *function = rewriter.rewrite_expression(function);
            }
        }
    }
}

fn canonicalize_function_param_default(
    param: &mut rumoca_core::FunctionParam,
    rewriter: &mut CollectedFunctionCallCanonicalizer,
) {
    if let Some(default) = &mut param.default {
        *default = rewriter.rewrite_expression(default);
    }
    for subscript in &mut param.shape_expr {
        if let rumoca_core::Subscript::Expr { expr, .. } = subscript {
            **expr = rewriter.rewrite_expression(expr);
        }
    }
}

struct CanonicalFunction {
    name: String,
    def_id: Option<rumoca_core::DefId>,
    instance_id: rumoca_core::FunctionInstanceId,
}

struct CollectedFunctionCallCanonicalizer {
    canonical_functions: Vec<CanonicalFunction>,
}

impl CollectedFunctionCallCanonicalizer {
    fn canonical_function_for_reference(
        &self,
        reference: &rumoca_core::Reference,
    ) -> Option<&CanonicalFunction> {
        if let Some(resolved) = reference.resolved_function() {
            return self
                .canonical_functions
                .iter()
                .find(|function| function.instance_id == resolved.instance_id);
        }
        if reference
            .component_ref()
            .is_some_and(|component_ref| component_ref.to_var_name() != *reference.var_name())
        {
            return None;
        }
        let exact = self
            .canonical_functions
            .iter()
            .find(|function| function.name == reference.var_name().as_str());
        if let Some(def_id) = reference.target_def_id() {
            let mut matches = self
                .canonical_functions
                .iter()
                .filter(|function| function.def_id == Some(def_id));
            let first = matches.next()?;
            let second = matches.next();
            if let Some(exact) = exact {
                return (exact.def_id == Some(def_id)).then_some(exact);
            }
            return second.is_none().then_some(first);
        }
        exact
    }
}

impl ExpressionRewriter for CollectedFunctionCallCanonicalizer {
    fn rewrite_expression(&mut self, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
        let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor,
            span,
        } = expr
        else {
            return self.walk_expression(expr);
        };
        let args = self.rewrite_expressions(args);
        if let Some(canonical) = self.canonical_function_for_reference(name) {
            let base_part_count = name
                .component_ref()
                .map(|reference| reference.parts.len())
                .unwrap_or(0);
            return rumoca_core::Expression::FunctionCall {
                name: name
                    .with_var_name(rumoca_core::VarName::new(&canonical.name))
                    .with_resolved_function(rumoca_core::ResolvedFunctionReference {
                        instance_id: canonical.instance_id,
                        base_part_count,
                    }),
                args,
                is_constructor: *is_constructor,
                span: *span,
            };
        }
        rumoca_core::Expression::FunctionCall {
            name: name.clone(),
            args,
            is_constructor: *is_constructor,
            span: *span,
        }
    }
}

impl StatementRewriter for CollectedFunctionCallCanonicalizer {}

pub(crate) fn is_executable_flat_function(function: &rumoca_core::Function) -> bool {
    function.is_constructor
        || function.external.is_some()
        || !function.body.is_empty()
        || function
            .outputs
            .iter()
            .any(|output| output.default.is_some())
}

pub(crate) fn canonicalize_function_calls_in_expression_with_scope(
    expr: &mut rumoca_core::Expression,
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    caller_scope: Option<&str>,
) {
    if caller_scope.is_none() {
        return;
    }
    *expr = ScopedFunctionCallCanonicalizer {
        tree,
        class_index,
        caller_scope,
    }
    .rewrite_expression(expr);
}

struct ScopedFunctionCallCanonicalizer<'a> {
    tree: &'a ast::ClassTree,
    class_index: &'a ast::ClassDefIndex<'a>,
    caller_scope: Option<&'a str>,
}

impl ExpressionRewriter for ScopedFunctionCallCanonicalizer<'_> {
    fn rewrite_expression(&mut self, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
        let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor,
            span,
        } = expr
        else {
            return self.walk_expression(expr);
        };
        let args = self.rewrite_expressions(args);
        if *is_constructor {
            return rumoca_core::Expression::FunctionCall {
                name: name.clone(),
                args,
                is_constructor: *is_constructor,
                span: *span,
            };
        }
        let Some(resolved_name) = canonical_function_name_with_scope(
            name,
            self.tree,
            self.class_index,
            self.caller_scope,
        ) else {
            return rumoca_core::Expression::FunctionCall {
                name: name.clone(),
                args,
                is_constructor: *is_constructor,
                span: *span,
            };
        };
        rumoca_core::Expression::FunctionCall {
            name: resolved_function_reference(name, resolved_name, self.tree, self.class_index),
            args,
            is_constructor: *is_constructor,
            span: *span,
        }
    }
}

fn canonical_function_name_with_scope(
    reference: &rumoca_core::Reference,
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    caller_scope: Option<&str>,
) -> Option<String> {
    if let Some(def_id) = reference.target_def_id()
        && let Some(class_def) = class_index.get(def_id)
        && class_def.class_type == rumoca_core::ClassType::Function
        && !class_def.partial
    {
        return class_index
            .qualified_name(def_id)
            .map(str::to_string)
            .filter(|name| name != reference.as_str());
    }

    let resolved =
        resolve_function_class_with_scope(tree, class_index, reference.as_str(), caller_scope)?;
    if resolved.class_def.class_type != rumoca_core::ClassType::Function
        || resolved.class_def.partial
    {
        return None;
    }
    (resolved.exposed_name != reference.as_str()).then_some(resolved.exposed_name)
}

fn resolved_function_reference(
    original: &rumoca_core::Reference,
    resolved_name: String,
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
) -> rumoca_core::Reference {
    let Some(mut component_ref) = original.component_ref().cloned() else {
        return rumoca_core::Reference::new(resolved_name);
    };
    component_ref.def_id = tree.name_map.get(&resolved_name).copied().or_else(|| {
        class_index
            .get_by_qualified_name(&resolved_name)
            .and_then(|class_def| class_def.def_id)
    });
    rumoca_core::Reference::with_component_reference(resolved_name, component_ref)
}

fn lookup_function_with_scope<'tree>(
    tree: &'tree ast::ClassTree,
    class_index: &ast::ClassDefIndex<'tree>,
    func_name: &str,
    caller_scope: Option<&str>,
    member_cache: &mut qualify::MemberDefIdCache<'tree>,
) -> Result<Option<(String, rumoca_core::Function)>, FlattenError> {
    let Some(resolved) =
        resolve_function_class_with_scope(tree, class_index, func_name, caller_scope)
    else {
        return Ok(None);
    };
    let flat_func = convert_callable(
        tree,
        class_index,
        resolved.class_def,
        &resolved.exposed_name,
        &tree.source_map,
        &tree.def_map,
        member_cache,
    )?;
    let Some(flat_func) = flat_func else {
        return Ok(None);
    };
    Ok(Some((resolved.exposed_name, flat_func)))
}

fn lookup_function_request_with_scope<'tree>(
    tree: &'tree ast::ClassTree,
    class_index: &ast::ClassDefIndex<'tree>,
    request: &FunctionRequest,
    caller_scope: Option<&str>,
    member_cache: &mut qualify::MemberDefIdCache<'tree>,
) -> Result<Option<(String, rumoca_core::Function)>, FlattenError> {
    if let Some(resolved) = lookup_exposed_function_request_by_name(
        tree,
        class_index,
        request,
        caller_scope,
        member_cache,
    )? {
        return Ok(Some(resolved));
    }
    if let Some(def_id) = request.target_def_id
        && let Some(class_def) = class_index.get(def_id)
        && is_callable_class_type(&class_def.class_type)
        && !class_def.partial
    {
        let exposed_name = class_index
            .qualified_name(def_id)
            .unwrap_or(request.name.as_str())
            .to_string();
        if let Some(flat_func) = convert_callable(
            tree,
            class_index,
            class_def,
            &exposed_name,
            &tree.source_map,
            &tree.def_map,
            member_cache,
        )? && is_executable_flat_function(&flat_func)
        {
            return Ok(Some((exposed_name, flat_func)));
        }
    }

    lookup_function_with_scope(tree, class_index, &request.name, caller_scope, member_cache)
}

fn lookup_exposed_function_request_by_name<'tree>(
    tree: &'tree ast::ClassTree,
    class_index: &ast::ClassDefIndex<'tree>,
    request: &FunctionRequest,
    caller_scope: Option<&str>,
    member_cache: &mut qualify::MemberDefIdCache<'tree>,
) -> Result<Option<(String, rumoca_core::Function)>, FlattenError> {
    let Some(def_id) = request.target_def_id else {
        return Ok(None);
    };
    if class_index
        .qualified_name(def_id)
        .is_some_and(|canonical_name| canonical_name == request.name)
    {
        return Ok(None);
    }
    let Some(resolved) =
        resolve_function_class_with_scope(tree, class_index, &request.name, caller_scope)
    else {
        return Ok(None);
    };
    if resolved.class_def.def_id != Some(def_id)
        || !is_callable_class_type(&resolved.class_def.class_type)
        || resolved.class_def.partial
    {
        return Ok(None);
    }
    let Some(flat_func) = convert_callable(
        tree,
        class_index,
        resolved.class_def,
        &resolved.exposed_name,
        &tree.source_map,
        &tree.def_map,
        member_cache,
    )?
    else {
        return Ok(None);
    };
    Ok(is_executable_flat_function(&flat_func).then_some((resolved.exposed_name, flat_func)))
}

pub(crate) fn lookup_function_request(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    request: &FunctionRequest,
) -> Result<Option<(String, rumoca_core::Function)>, FlattenError> {
    let mut member_cache = qualify::MemberDefIdCache::default();
    lookup_function_request_with_scope(tree, class_index, request, None, &mut member_cache)
}

struct FunctionClassResolution<'a> {
    exposed_name: String,
    class_def: &'a ast::ClassDef,
}

/// Resolve alias-style function names (e.g. `Medium.dynamicViscosity`) by
/// reusing package prefixes already present in the model's known function set.
fn lookup_function_in_known_packages<'tree>(
    tree: &'tree ast::ClassTree,
    class_index: &ast::ClassDefIndex<'tree>,
    func_name: &str,
    known_functions: &[String],
    member_cache: &mut qualify::MemberDefIdCache<'tree>,
) -> Result<Option<(String, rumoca_core::Function)>, FlattenError> {
    let Some((_first, remainder)) = path_utils::root_split(func_name) else {
        return Ok(None);
    };
    if remainder.is_empty() {
        return Ok(None);
    }

    let mut matched: Option<String> = None;
    for known in known_functions {
        let Some(pkg_prefix) = path_utils::enclosing_scope(known) else {
            continue;
        };
        if resolve_function_in_package_chain_class(tree, class_index, pkg_prefix, remainder)
            .is_none()
        {
            continue;
        }
        let candidate = format!("{pkg_prefix}.{remainder}");
        if matched
            .as_ref()
            .is_some_and(|existing| existing != &candidate)
        {
            return Ok(None);
        }
        matched = Some(candidate);
    }

    let Some(qualified_name) = matched else {
        return Ok(None);
    };
    let Some((class_def, _source_name)) =
        path_utils::scope_split(&qualified_name).and_then(|(package, leaf)| {
            resolve_function_in_package_chain_class(tree, class_index, package, leaf)
        })
    else {
        return Ok(None);
    };
    let flat_func = convert_callable(
        tree,
        class_index,
        class_def,
        &qualified_name,
        &tree.source_map,
        &tree.def_map,
        member_cache,
    )?;
    let Some(flat_func) = flat_func else {
        return Ok(None);
    };
    if !is_executable_flat_function(&flat_func) {
        return Ok(None);
    }
    Ok(Some((qualified_name, flat_func)))
}

fn resolve_function_class_with_scope<'a>(
    tree: &'a ast::ClassTree,
    class_index: &ast::ClassDefIndex<'a>,
    func_name: &str,
    caller_scope: Option<&str>,
) -> Option<FunctionClassResolution<'a>> {
    if let Some(class_def) = class_index.get_by_qualified_name(func_name)
        && is_callable_class_type(&class_def.class_type)
    {
        if class_def.partial
            && let Some(caller_scope) = caller_scope
        {
            let short_name = path_utils::leaf_segment(func_name);
            if let Some(scoped_match) =
                resolve_function_in_caller_packages(tree, class_index, caller_scope, short_name)
                && scoped_match != func_name
            {
                return resolve_function_class_with_scope(
                    tree,
                    class_index,
                    &scoped_match,
                    Some(caller_scope),
                );
            }
        }
        return Some(FunctionClassResolution {
            exposed_name: func_name.to_string(),
            class_def,
        });
    }

    if let Some((package_name, function_leaf)) = path_utils::scope_split(func_name)
        && let Some((class_def, _source_name)) =
            resolve_function_in_package_chain_class(tree, class_index, package_name, function_leaf)
    {
        return Some(FunctionClassResolution {
            exposed_name: func_name.to_string(),
            class_def,
        });
    }

    if let Some(caller_scope) = caller_scope
        && let Some(scoped_match) =
            resolve_function_path_in_caller_packages(tree, class_index, caller_scope, func_name)
    {
        return resolve_function_class_with_scope(
            tree,
            class_index,
            &scoped_match,
            Some(caller_scope),
        );
    }

    let short_name = path_utils::leaf_segment(func_name);
    if short_name != func_name
        && let Some(caller_scope) = caller_scope
        && let Some(scoped_match) =
            resolve_function_in_caller_packages(tree, class_index, caller_scope, short_name)
    {
        return resolve_function_class_with_scope(
            tree,
            class_index,
            &scoped_match,
            Some(caller_scope),
        );
    }

    None
}

fn resolve_function_path_in_caller_packages(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    caller_scope: &str,
    func_path: &str,
) -> Option<String> {
    let mut visited = HashSet::new();
    resolve_function_path_in_caller_packages_inner(
        tree,
        class_index,
        caller_scope,
        func_path,
        &mut visited,
    )
}

fn resolve_function_path_in_caller_packages_inner(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    caller_scope: &str,
    func_path: &str,
    visited: &mut HashSet<String>,
) -> Option<String> {
    if !visited.insert(caller_scope.to_string()) {
        return None;
    }

    if let Some(class_def) = class_index.get_by_qualified_name(caller_scope) {
        for ext in &class_def.extends {
            let base_name = ext.base_name.to_string();
            if let Some(base_scope) =
                crate::pipeline::resolve_extends_base_qname(class_index, &base_name, caller_scope)
                && let Some(resolved) = resolve_function_path_in_caller_packages_inner(
                    tree,
                    class_index,
                    &base_scope,
                    func_path,
                    visited,
                )
            {
                return Some(resolved);
            }
        }
    }

    if !path_utils::is_nested_name(func_path) {
        return resolve_function_in_caller_packages(tree, class_index, caller_scope, func_path);
    }

    for prefix in path_utils::enclosing_scopes(caller_scope) {
        let candidate = format!("{prefix}.{func_path}");
        if let Some((package_name, function_leaf)) = path_utils::scope_split(&candidate)
            && resolve_function_in_package_chain_class(
                tree,
                class_index,
                package_name,
                function_leaf,
            )
            .is_some()
        {
            return Some(candidate);
        }
    }
    None
}

fn resolve_function_in_package_chain_class<'a>(
    tree: &'a ast::ClassTree,
    class_index: &ast::ClassDefIndex<'a>,
    package_name: &str,
    function_leaf: &str,
) -> Option<(&'a ast::ClassDef, String)> {
    fn resolve_inner<'a>(
        tree: &'a ast::ClassTree,
        class_index: &ast::ClassDefIndex<'a>,
        package_name: &str,
        function_leaf: &str,
        visited: &mut HashSet<String>,
    ) -> Option<(&'a ast::ClassDef, String)> {
        if !visited.insert(package_name.to_string()) {
            return None;
        }

        let direct = format!("{package_name}.{function_leaf}");
        if let Some(class_def) = class_index.get_by_qualified_name(&direct)
            && is_callable_class_type(&class_def.class_type)
        {
            return Some((class_def, direct));
        }

        let package_class = class_index.get_by_qualified_name(package_name)?;
        // MLS §7.3: an extends-clause class redeclare replaces the inherited
        // member, so it wins over the (possibly partial) lexical base member.
        if let Some(target) = crate::pipeline::extends_class_redeclare_target(
            tree,
            class_index,
            package_class,
            package_name,
            function_leaf,
        ) && is_callable_class_type(&target.class_type)
        {
            return Some((target, direct));
        }
        for ext in &package_class.extends {
            let base_name = ext.base_name.to_string();
            let resolved_base = ext
                .base_def_id
                .and_then(|def_id| tree.def_map.get(&def_id).cloned())
                .or_else(|| {
                    crate::resolve_class_in_scope_indexed(class_index, &base_name, package_name).1
                })
                .or_else(|| {
                    class_index
                        .get_by_qualified_name(&base_name)
                        .map(|_| base_name.clone())
                });
            if let Some(base) = resolved_base
                && let Some(resolved) =
                    resolve_inner(tree, class_index, &base, function_leaf, visited)
            {
                return Some(resolved);
            }
        }

        None
    }

    let mut visited = HashSet::new();
    resolve_inner(tree, class_index, package_name, function_leaf, &mut visited)
}

pub(crate) fn collect_function_dep_requests(func: &rumoca_core::Function) -> Vec<FunctionRequest> {
    let mut deps = FunctionRequests::default();

    for param in func
        .inputs
        .iter()
        .chain(func.outputs.iter())
        .chain(func.locals.iter())
    {
        if param.type_class == Some(rumoca_core::ClassType::Record) {
            deps.insert(FunctionRequest::from_name(param.type_name.clone()));
        }
        if let Some(default) = &param.default {
            collect_from_expression(default, &mut deps);
        }
        for subscript in &param.shape_expr {
            collect_from_subscript(subscript, &mut deps);
        }
    }

    for stmt in &func.body {
        collect_from_statement(stmt, &mut deps);
    }

    deps.into_entries()
}

fn collect_from_subscript(subscript: &rumoca_core::Subscript, deps: &mut FunctionRequests) {
    if let rumoca_core::Subscript::Expr { expr, .. } = subscript {
        collect_from_expression(expr, deps);
    }
}

fn collect_from_statement(stmt: &rumoca_core::Statement, deps: &mut FunctionRequests) {
    let mut collector = FunctionCallCollector { calls: deps };
    rumoca_ir_flat::visitor::StatementVisitor::visit_statement(&mut collector, stmt);
}

fn component_ref_name(comp: &rumoca_core::ComponentReference) -> String {
    comp.parts
        .iter()
        .map(|part| part.ident.as_str())
        .collect::<Vec<_>>()
        .join(".")
}

fn resolve_function_in_caller_packages(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    caller_scope: &str,
    short_name: &str,
) -> Option<String> {
    let mut prefix = path_utils::enclosing_scope(caller_scope)?;
    loop {
        let candidate = format!("{prefix}.{short_name}");
        if let Some((package_name, function_leaf)) = path_utils::scope_split(&candidate)
            && resolve_function_in_package_chain_class(
                tree,
                class_index,
                package_name,
                function_leaf,
            )
            .is_some()
        {
            return Some(candidate);
        }
        let Some(parent) = path_utils::enclosing_scope(prefix) else {
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

fn collect_function_context<'tree>(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'tree>,
    class_def: &'tree ast::ClassDef,
    member_cache: &mut qualify::MemberDefIdCache<'tree>,
) -> FunctionClassContext {
    let mut visited = HashSet::new();
    let mut context = FunctionClassContext::default();
    collect_function_context_recursive(
        tree,
        class_index,
        class_def,
        &mut visited,
        &mut context,
        member_cache,
    );
    context
}

fn collect_function_context_recursive<'tree>(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'tree>,
    class_def: &'tree ast::ClassDef,
    visited: &mut HashSet<usize>,
    context: &mut FunctionClassContext,
    member_cache: &mut qualify::MemberDefIdCache<'tree>,
) {
    let class_key = class_def as *const ast::ClassDef as usize;
    if !visited.insert(class_key) {
        return;
    }

    for extend in &class_def.extends {
        let base_class = extend
            .base_def_id
            .and_then(|def_id| class_index.get(def_id))
            .or_else(|| {
                let qualified = extend.base_name.to_string();
                class_index.get_by_qualified_name(&qualified)
            });
        if let Some(base_class) = base_class {
            collect_function_context_recursive(
                tree,
                class_index,
                base_class,
                visited,
                context,
                member_cache,
            );
        }
    }

    if let Some(class_def_id) = class_def.def_id {
        collect_lexical_ancestor_imports(class_index, class_def_id, &mut context.imports);
        qualify::collect_lexical_package_aliases_for_def_id_with_member_cache(
            tree,
            class_index,
            class_def_id,
            &mut context.imports,
            Some(member_cache),
        );
        qualify::collect_lexical_class_aliases_for_def_id_with_member_cache(
            tree,
            class_index,
            class_def_id,
            &mut context.imports,
            Some(member_cache),
        );
        qualify::collect_lexical_constant_aliases_for_def_id_with_packages_and_member_cache(
            tree,
            class_index,
            class_def_id,
            &[],
            &mut context.imports,
            Some(member_cache),
        );
    }
    resolve_import_pairs(&class_def.imports, class_index, &mut context.imports);
    context.algorithms.extend(class_def.algorithms.clone());
    context.components.extend(class_def.components.clone());
}

fn collect_lexical_ancestor_imports(
    class_index: &ast::ClassDefIndex<'_>,
    class_def_id: rumoca_core::DefId,
    map: &mut qualify::ImportMap,
) {
    let mut ancestor_def_ids = Vec::new();
    let mut current = class_index.parent_def_id(class_def_id);
    while let Some(def_id) = current {
        ancestor_def_ids.push(def_id);
        current = class_index.parent_def_id(def_id);
    }
    for ancestor_def_id in ancestor_def_ids.into_iter().rev() {
        let Some(ancestor_class) = class_index.get(ancestor_def_id) else {
            continue;
        };
        resolve_import_pairs(&ancestor_class.imports, class_index, map);
    }
}

fn resolve_import_pairs(
    imports: &[ast::Import],
    class_index: &ast::ClassDefIndex<'_>,
    map: &mut qualify::ImportMap,
) {
    for import in imports {
        match import {
            ast::Import::Qualified { path, .. } => {
                let fqn = path.to_string();
                map.insert(path_utils::leaf_segment(&fqn).to_string(), fqn);
            }
            ast::Import::Renamed { alias, path, .. } => {
                map.insert(alias.text.to_string(), path.to_string());
            }
            ast::Import::Unqualified { path, .. } => {
                let pkg_name = path.to_string();
                let Some(class_def) = class_index.get_by_qualified_name(&pkg_name) else {
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

fn convert_callable<'tree>(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'tree>,
    class_def: &'tree ast::ClassDef,
    qualified_name: &str,
    source_map: &rumoca_core::SourceMap,
    def_map: &crate::ResolveDefMap,
    member_cache: &mut qualify::MemberDefIdCache<'tree>,
) -> Result<Option<rumoca_core::Function>, FlattenError> {
    match &class_def.class_type {
        rumoca_core::ClassType::Function => convert_function(
            tree,
            class_index,
            class_def,
            qualified_name,
            source_map,
            def_map,
            member_cache,
        )
        .map(Some),
        class_type if is_callable_class_type(class_type) => {
            let mut constructor = convert_constructor_signature(
                class_index,
                class_def,
                qualified_name,
                source_map,
                def_map,
            )?;
            contextualize_record_param_type_names(
                tree,
                class_index,
                qualified_name,
                &mut constructor,
            );
            Ok(Some(constructor))
        }
        _ => Ok(None),
    }
}

/// Convert a ast::ClassDef (function) to a rumoca_core::Function.
fn convert_function<'tree>(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'tree>,
    class_def: &'tree ast::ClassDef,
    qualified_name: &str,
    source_map: &rumoca_core::SourceMap,
    def_map: &crate::ResolveDefMap,
    member_cache: &mut qualify::MemberDefIdCache<'tree>,
) -> Result<rumoca_core::Function, FlattenError> {
    let span = required_location_span(source_map, &class_def.location, "function definition")?;
    let mut func = rumoca_core::Function::new(qualified_name, span);
    func.def_id = class_def.def_id;
    let context = collect_function_context(tree, class_index, class_def, member_cache);
    let effective_components = context.components;
    let mut import_map =
        function_initial_import_map(tree, class_index, class_def, qualified_name, member_cache);
    extend_imports_if_absent(&mut import_map, context.imports);
    resolve_import_pairs(&class_def.imports, class_index, &mut import_map);
    if let Some(class_def_id) = class_def.def_id {
        collect_lexical_constant_aliases(tree, class_index, class_def_id, &mut import_map, true);
    }
    let prefix = ast::QualifiedName::new();
    let function_locals: HashSet<String> = effective_components.keys().cloned().collect();

    // Process components to find inputs, outputs, and locals
    for (comp_name, component) in &effective_components {
        let param = convert_component_to_param(
            class_index,
            comp_name,
            component,
            source_map,
            def_map,
            &import_map,
            &function_locals,
        )?;

        match &component.causality {
            rumoca_core::Causality::Input(_) => func.add_input(param),
            rumoca_core::Causality::Output(_) => func.add_output(param),
            rumoca_core::Causality::Empty => func.add_local(param),
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
    let active_constant_def_overrides =
        active_constant_def_overrides(tree, class_index, class_def, def_map);
    let filtered_def_map: crate::ResolveDefMap = def_map
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
        .map(|(k, v)| {
            (
                *k,
                active_constant_def_overrides
                    .get(k)
                    .cloned()
                    .unwrap_or_else(|| v.clone()),
            )
        })
        .collect();

    for alg in &context.algorithms {
        let flat_alg = algorithms::flatten_algorithm_section(
            alg,
            algorithms::AlgorithmSectionContext {
                prefix: &prefix,
                imports: &import_map,
                def_map: Some(&filtered_def_map),
                initial_locals: &function_locals,
                source_map: Some(source_map),
                instance_name: None,
            },
            algorithms::AlgorithmSectionMetadata::new(span, qualified_name.to_string()),
        )?;
        func.body.extend(flat_alg.statements);
    }

    normalize_function_local_references(&mut func);

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

    rewrite_function_extends_aliases_in_function(&mut func, tree, class_index)?;
    contextualize_record_param_type_names(tree, class_index, qualified_name, &mut func);
    if !class_def.partial && is_executable_flat_function(&func) {
        validate_function_outputs_assigned(&func)?;
    }

    Ok(func)
}

/// Rewrite record-typed parameter type names to the exposed qualified names
/// resolved in the callable's own scope (redeclare-aware, like body calls).
///
/// Downstream record decomposition and output projection perform exact
/// constructor lookups keyed by these names, so a record param must carry the
/// concrete resolved type name rather than source-relative text.
fn contextualize_record_param_type_names(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    exposed_name: &str,
    func: &mut rumoca_core::Function,
) {
    for param in func
        .inputs
        .iter_mut()
        .chain(func.outputs.iter_mut())
        .chain(func.locals.iter_mut())
    {
        if param.type_class != Some(rumoca_core::ClassType::Record) {
            continue;
        }
        let Some(resolution) = resolve_function_class_with_scope(
            tree,
            class_index,
            &param.type_name,
            Some(exposed_name),
        ) else {
            continue;
        };
        if resolution.class_def.class_type == rumoca_core::ClassType::Record {
            param.type_name = resolution.exposed_name;
            param.type_def_id = resolution.class_def.def_id;
        }
    }
}

fn function_initial_import_map<'tree>(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'tree>,
    class_def: &ast::ClassDef,
    qualified_name: &str,
    member_cache: &mut qualify::MemberDefIdCache<'tree>,
) -> qualify::ImportMap {
    let mut import_map = qualify::ImportMap::default();
    if let Some(class_def_id) = class_def.def_id {
        qualify::collect_lexical_package_aliases_for_def_id_with_member_cache(
            tree,
            class_index,
            class_def_id,
            &mut import_map,
            Some(member_cache),
        );
        qualify::collect_lexical_class_aliases_for_def_id_with_member_cache(
            tree,
            class_index,
            class_def_id,
            &mut import_map,
            Some(member_cache),
        );
        collect_lexical_constant_aliases(tree, class_index, class_def_id, &mut import_map, false);
        collect_lexical_ancestor_imports(class_index, class_def_id, &mut import_map);
    } else {
        qualify::collect_lexical_package_aliases(
            tree,
            class_index,
            qualified_name,
            &mut import_map,
        );
        qualify::collect_lexical_class_aliases(tree, class_index, qualified_name, &mut import_map);
    }
    import_map
}

fn extend_imports_if_absent(imports: &mut qualify::ImportMap, aliases: qualify::ImportMap) {
    for (name, target) in aliases {
        imports.entry(name).or_insert(target);
    }
}

fn collect_lexical_constant_aliases<'tree>(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'tree>,
    class_def_id: rumoca_core::DefId,
    imports: &mut qualify::ImportMap,
    overwrite: bool,
) {
    let mut ancestor_def_ids = Vec::new();
    let mut current = class_index.parent_def_id(class_def_id);
    while let Some(def_id) = current {
        ancestor_def_ids.push(def_id);
        current = class_index.parent_def_id(def_id);
    }
    for ancestor_def_id in ancestor_def_ids {
        let Some(scope) = class_index.qualified_name(ancestor_def_id) else {
            continue;
        };
        collect_effective_package_constant_aliases(tree, class_index, scope, imports, overwrite);
    }
}

fn collect_effective_package_constant_aliases(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    active_scope: &str,
    imports: &mut qualify::ImportMap,
    overwrite: bool,
) {
    let mut chain = Vec::new();
    let mut visited = FxHashSet::default();
    collect_package_chain(tree, class_index, active_scope, &mut chain, &mut visited);
    if chain.is_empty()
        && let Some(active_def_id) = class_index
            .get_by_qualified_name(active_scope)
            .and_then(|class_def| class_def.def_id)
    {
        chain.push(active_def_id);
    }
    for source_def_id in chain {
        let Some(class_def) = class_index.get(source_def_id) else {
            continue;
        };
        if !tree.def_map.contains_key(&source_def_id) {
            continue;
        }
        // MLS §5.3.2: enclosing-scope lookup reaches class constants (and,
        // for package enclosers, package members). A non-package class's
        // parameters are instance members and must never become
        // class-qualified alias targets.
        let is_package = matches!(class_def.class_type, rumoca_core::ClassType::Package);
        for (name, component) in &class_def.components {
            let alias_visible = match component.variability {
                rumoca_core::Variability::Constant(_) => true,
                rumoca_core::Variability::Parameter(_) => is_package,
                _ => false,
            };
            if !alias_visible {
                continue;
            }
            let target = format!("{active_scope}.{name}");
            if overwrite {
                imports.insert(name.clone(), target);
            } else {
                imports.entry(name.clone()).or_insert(target);
            }
        }
    }
}

fn active_constant_def_overrides(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    active_function: &ast::ClassDef,
    def_map: &crate::ResolveDefMap,
) -> crate::ResolveDefMap {
    let Some(active_function_def_id) = active_function.def_id else {
        return crate::ResolveDefMap::default();
    };
    let Some(active_package_def_id) = class_index.parent_def_id(active_function_def_id) else {
        return crate::ResolveDefMap::default();
    };
    let Some(active_package) = tree.def_map.get(&active_package_def_id) else {
        return crate::ResolveDefMap::default();
    };
    let mut package_chain = Vec::new();
    let mut visited = FxHashSet::default();
    collect_package_chain(
        tree,
        class_index,
        active_package,
        &mut package_chain,
        &mut visited,
    );
    if package_chain.is_empty() {
        package_chain.push(active_package_def_id);
    }
    let package_chain: FxHashSet<rumoca_core::DefId> = package_chain.into_iter().collect();
    def_map
        .iter()
        .filter_map(|(def_id, _resolved_path)| {
            let source_def_id = class_index.parent_def_id(*def_id)?;
            if source_def_id == active_package_def_id {
                return None;
            }
            if !package_chain.contains(&source_def_id) {
                return None;
            }
            let source_class = class_index.get(source_def_id)?;
            let leaf = class_index.local_name(*def_id)?;
            let component = source_class.components.get(leaf)?;
            if component.def_id != Some(*def_id)
                || !matches!(
                    component.variability,
                    rumoca_core::Variability::Constant(_) | rumoca_core::Variability::Parameter(_)
                )
            {
                return None;
            }
            Some((*def_id, format!("{active_package}.{leaf}")))
        })
        .collect()
}
