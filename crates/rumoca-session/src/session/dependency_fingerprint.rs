use indexmap::{IndexMap, IndexSet};
use rumoca_core::DefId;
use rumoca_ir_ast as ast;
use std::collections::HashMap;

use super::PhaseResult;

pub(crate) type Fingerprint = [u8; 32];

#[derive(Debug, Clone)]
pub(crate) struct CompileCacheEntry {
    pub(crate) fingerprint: Fingerprint,
    pub(crate) result: PhaseResult,
}

#[derive(Debug, Clone, Default)]
pub(crate) struct DependencyFingerprintCache {
    class_hashes: IndexMap<String, Fingerprint>,
    class_deps: IndexMap<String, IndexSet<String>>,
    model_fingerprints: IndexMap<String, Fingerprint>,
}

impl DependencyFingerprintCache {
    pub(crate) fn from_tree(tree: &ast::ClassTree) -> Self {
        let mut cache = Self::default();
        let mut file_bytes_cache: HashMap<String, Option<Vec<u8>>> = HashMap::new();

        for (qualified_name, &def_id) in &tree.name_map {
            let Some(class) = tree.get_class_by_def_id(def_id) else {
                continue;
            };

            cache.class_hashes.insert(
                qualified_name.clone(),
                class_source_fingerprint(tree, class, qualified_name, &mut file_bytes_cache),
            );
            cache.class_deps.insert(
                qualified_name.clone(),
                collect_class_dependencies(tree, class, qualified_name),
            );
        }

        cache
    }

    pub(crate) fn model_fingerprint(&mut self, model_name: &str) -> Fingerprint {
        let mut visiting = IndexSet::new();
        self.model_fingerprint_recursive(model_name, &mut visiting)
    }

    fn model_fingerprint_recursive(
        &mut self,
        model_name: &str,
        visiting: &mut IndexSet<String>,
    ) -> Fingerprint {
        if let Some(fingerprint) = self.model_fingerprints.get(model_name) {
            return *fingerprint;
        }
        if !visiting.insert(model_name.to_string()) {
            let mut hasher = blake3::Hasher::new();
            hasher.update(b"rumoca-model-fingerprint-cycle-v1");
            hasher.update(model_name.as_bytes());
            return *hasher.finalize().as_bytes();
        }

        let own_hash = self
            .class_hashes
            .get(model_name)
            .copied()
            .unwrap_or_else(|| {
                let mut hasher = blake3::Hasher::new();
                hasher.update(b"rumoca-model-missing-v1");
                hasher.update(model_name.as_bytes());
                *hasher.finalize().as_bytes()
            });
        let mut deps = self
            .class_deps
            .get(model_name)
            .map(|set| set.iter().cloned().collect::<Vec<_>>())
            .unwrap_or_default();
        deps.sort_unstable();

        let mut hasher = blake3::Hasher::new();
        hasher.update(b"rumoca-model-fingerprint-v1");
        hasher.update(model_name.as_bytes());
        hasher.update(&own_hash);
        for dep in deps {
            let dep_hash = self.model_fingerprint_recursive(&dep, visiting);
            hasher.update(dep.as_bytes());
            hasher.update(&dep_hash);
        }
        let fingerprint = *hasher.finalize().as_bytes();
        visiting.shift_remove(model_name);
        self.model_fingerprints
            .insert(model_name.to_string(), fingerprint);
        fingerprint
    }
}

fn collect_class_dependencies(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    class_name: &str,
) -> IndexSet<String> {
    let mut deps = IndexSet::new();

    for ext in &class.extends {
        if let Some(base_def_id) = ext.base_def_id {
            add_class_dep_by_def_id(tree, &mut deps, base_def_id);
        }
        add_class_dep_from_name(tree, &mut deps, &ext.base_name);
        for modification in &ext.modifications {
            collect_expression_class_deps(tree, &mut deps, &modification.expr);
        }
        for annotation in &ext.annotation {
            collect_expression_class_deps(tree, &mut deps, annotation);
        }
    }

    if let Some(constrainedby) = &class.constrainedby {
        add_class_dep_from_name(tree, &mut deps, constrainedby);
    }
    for import in &class.imports {
        add_class_dep_from_name(tree, &mut deps, import.base_path());
    }
    for subscript in &class.array_subscripts {
        collect_subscript_class_deps(tree, &mut deps, subscript);
    }
    for annotation in &class.annotation {
        collect_expression_class_deps(tree, &mut deps, annotation);
    }

    for component in class.components.values() {
        if let Some(type_def_id) = component.type_def_id {
            add_class_dep_by_def_id(tree, &mut deps, type_def_id);
        }
        add_class_dep_from_name(tree, &mut deps, &component.type_name);
        if let Some(constrainedby) = &component.constrainedby {
            add_class_dep_from_name(tree, &mut deps, constrainedby);
        }
        collect_expression_class_deps(tree, &mut deps, &component.start);
        if let Some(binding) = &component.binding {
            collect_expression_class_deps(tree, &mut deps, binding);
        }
        for shape in &component.shape_expr {
            collect_subscript_class_deps(tree, &mut deps, shape);
        }
        for annotation in &component.annotation {
            collect_expression_class_deps(tree, &mut deps, annotation);
        }
        for modification in component.modifications.values() {
            collect_expression_class_deps(tree, &mut deps, modification);
        }
        if let Some(condition) = &component.condition {
            collect_expression_class_deps(tree, &mut deps, condition);
        }
    }

    for equation in &class.equations {
        collect_equation_class_deps(tree, &mut deps, equation);
    }
    for equation in &class.initial_equations {
        collect_equation_class_deps(tree, &mut deps, equation);
    }
    for algorithm in &class.algorithms {
        for statement in algorithm {
            collect_statement_class_deps(tree, &mut deps, statement);
        }
    }
    for algorithm in &class.initial_algorithms {
        for statement in algorithm {
            collect_statement_class_deps(tree, &mut deps, statement);
        }
    }

    if let Some(external) = &class.external {
        if let Some(output) = &external.output {
            collect_component_ref_class_deps(tree, &mut deps, output);
        }
        for arg in &external.args {
            collect_expression_class_deps(tree, &mut deps, arg);
        }
    }

    deps.shift_remove(class_name);
    deps
}

fn collect_equation_class_deps(
    tree: &ast::ClassTree,
    deps: &mut IndexSet<String>,
    equation: &ast::Equation,
) {
    match equation {
        ast::Equation::Empty => {}
        ast::Equation::Simple { lhs, rhs } => {
            collect_expression_class_deps(tree, deps, lhs);
            collect_expression_class_deps(tree, deps, rhs);
        }
        ast::Equation::Connect { lhs, rhs } => {
            collect_component_ref_class_deps(tree, deps, lhs);
            collect_component_ref_class_deps(tree, deps, rhs);
        }
        ast::Equation::For { indices, equations } => {
            for index in indices {
                collect_expression_class_deps(tree, deps, &index.range);
            }
            for nested in equations {
                collect_equation_class_deps(tree, deps, nested);
            }
        }
        ast::Equation::When(blocks) => {
            for block in blocks {
                collect_expression_class_deps(tree, deps, &block.cond);
                for nested in &block.eqs {
                    collect_equation_class_deps(tree, deps, nested);
                }
            }
        }
        ast::Equation::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                collect_expression_class_deps(tree, deps, &block.cond);
                for nested in &block.eqs {
                    collect_equation_class_deps(tree, deps, nested);
                }
            }
            if let Some(else_eqs) = else_block {
                for nested in else_eqs {
                    collect_equation_class_deps(tree, deps, nested);
                }
            }
        }
        ast::Equation::FunctionCall { comp, args } => {
            collect_component_ref_class_deps(tree, deps, comp);
            for arg in args {
                collect_expression_class_deps(tree, deps, arg);
            }
        }
        ast::Equation::Assert {
            condition,
            message,
            level,
        } => {
            collect_expression_class_deps(tree, deps, condition);
            collect_expression_class_deps(tree, deps, message);
            if let Some(level) = level {
                collect_expression_class_deps(tree, deps, level);
            }
        }
    }
}

fn collect_statement_class_deps(
    tree: &ast::ClassTree,
    deps: &mut IndexSet<String>,
    statement: &ast::Statement,
) {
    match statement {
        ast::Statement::Empty | ast::Statement::Return { .. } | ast::Statement::Break { .. } => {}
        ast::Statement::Assignment { comp, value } => {
            collect_component_ref_class_deps(tree, deps, comp);
            collect_expression_class_deps(tree, deps, value);
        }
        ast::Statement::For { indices, equations } => {
            for index in indices {
                collect_expression_class_deps(tree, deps, &index.range);
            }
            for nested in equations {
                collect_statement_class_deps(tree, deps, nested);
            }
        }
        ast::Statement::While(block) => {
            collect_expression_class_deps(tree, deps, &block.cond);
            for nested in &block.stmts {
                collect_statement_class_deps(tree, deps, nested);
            }
        }
        ast::Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                collect_expression_class_deps(tree, deps, &block.cond);
                for nested in &block.stmts {
                    collect_statement_class_deps(tree, deps, nested);
                }
            }
            if let Some(else_stmts) = else_block {
                for nested in else_stmts {
                    collect_statement_class_deps(tree, deps, nested);
                }
            }
        }
        ast::Statement::When(blocks) => {
            for block in blocks {
                collect_expression_class_deps(tree, deps, &block.cond);
                for nested in &block.stmts {
                    collect_statement_class_deps(tree, deps, nested);
                }
            }
        }
        ast::Statement::FunctionCall {
            comp,
            args,
            outputs,
        } => {
            collect_component_ref_class_deps(tree, deps, comp);
            for arg in args {
                collect_expression_class_deps(tree, deps, arg);
            }
            for output in outputs {
                collect_expression_class_deps(tree, deps, output);
            }
        }
        ast::Statement::Reinit { variable, value } => {
            collect_component_ref_class_deps(tree, deps, variable);
            collect_expression_class_deps(tree, deps, value);
        }
        ast::Statement::Assert {
            condition,
            message,
            level,
        } => {
            collect_expression_class_deps(tree, deps, condition);
            collect_expression_class_deps(tree, deps, message);
            if let Some(level) = level {
                collect_expression_class_deps(tree, deps, level);
            }
        }
    }
}

fn collect_expression_class_deps(
    tree: &ast::ClassTree,
    deps: &mut IndexSet<String>,
    expr: &ast::Expression,
) {
    match expr {
        ast::Expression::Empty | ast::Expression::Terminal { .. } => {}
        ast::Expression::Range { start, step, end } => {
            collect_expression_class_deps(tree, deps, start);
            if let Some(step) = step {
                collect_expression_class_deps(tree, deps, step);
            }
            collect_expression_class_deps(tree, deps, end);
        }
        ast::Expression::Unary { rhs, .. } => {
            collect_expression_class_deps(tree, deps, rhs);
        }
        ast::Expression::Binary { lhs, rhs, .. } => {
            collect_expression_class_deps(tree, deps, lhs);
            collect_expression_class_deps(tree, deps, rhs);
        }
        ast::Expression::ComponentReference(comp) => {
            collect_component_ref_class_deps(tree, deps, comp);
        }
        ast::Expression::FunctionCall { comp, args } => {
            collect_component_ref_class_deps(tree, deps, comp);
            for arg in args {
                collect_expression_class_deps(tree, deps, arg);
            }
        }
        ast::Expression::ClassModification {
            target,
            modifications,
        } => {
            collect_component_ref_class_deps(tree, deps, target);
            for modification in modifications {
                collect_expression_class_deps(tree, deps, modification);
            }
        }
        ast::Expression::NamedArgument { value, .. } => {
            collect_expression_class_deps(tree, deps, value);
        }
        ast::Expression::Modification { target, value } => {
            collect_component_ref_class_deps(tree, deps, target);
            collect_expression_class_deps(tree, deps, value);
        }
        ast::Expression::Array { elements, .. } | ast::Expression::Tuple { elements } => {
            for element in elements {
                collect_expression_class_deps(tree, deps, element);
            }
        }
        ast::Expression::If {
            branches,
            else_branch,
        } => {
            for (condition, value) in branches {
                collect_expression_class_deps(tree, deps, condition);
                collect_expression_class_deps(tree, deps, value);
            }
            collect_expression_class_deps(tree, deps, else_branch);
        }
        ast::Expression::Parenthesized { inner } => {
            collect_expression_class_deps(tree, deps, inner);
        }
        ast::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            collect_expression_class_deps(tree, deps, expr);
            for index in indices {
                collect_expression_class_deps(tree, deps, &index.range);
            }
            if let Some(filter) = filter {
                collect_expression_class_deps(tree, deps, filter);
            }
        }
        ast::Expression::ArrayIndex { base, subscripts } => {
            collect_expression_class_deps(tree, deps, base);
            for subscript in subscripts {
                collect_subscript_class_deps(tree, deps, subscript);
            }
        }
        ast::Expression::FieldAccess { base, .. } => {
            collect_expression_class_deps(tree, deps, base);
        }
    }
}

fn collect_component_ref_class_deps(
    tree: &ast::ClassTree,
    deps: &mut IndexSet<String>,
    comp: &ast::ComponentReference,
) {
    if let Some(def_id) = comp.def_id {
        add_class_dep_by_def_id(tree, deps, def_id);
    }
    for part in &comp.parts {
        if let Some(subscripts) = &part.subs {
            for subscript in subscripts {
                collect_subscript_class_deps(tree, deps, subscript);
            }
        }
    }
}

fn collect_subscript_class_deps(
    tree: &ast::ClassTree,
    deps: &mut IndexSet<String>,
    subscript: &ast::Subscript,
) {
    if let ast::Subscript::Expression(expr) = subscript {
        collect_expression_class_deps(tree, deps, expr);
    }
}

fn add_class_dep_from_name(tree: &ast::ClassTree, deps: &mut IndexSet<String>, name: &ast::Name) {
    if let Some(def_id) = name.def_id {
        add_class_dep_by_def_id(tree, deps, def_id);
    }
}

fn add_class_dep_by_def_id(tree: &ast::ClassTree, deps: &mut IndexSet<String>, def_id: DefId) {
    let Some(qualified_name) = tree.def_map.get(&def_id) else {
        return;
    };
    if tree.get_class_by_def_id(def_id).is_some() {
        deps.insert(qualified_name.clone());
    }
}

fn class_source_fingerprint(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    class_name: &str,
    file_bytes_cache: &mut HashMap<String, Option<Vec<u8>>>,
) -> Fingerprint {
    let location = &class.location;
    let start = location.start as usize;
    let end = location.end as usize;
    let mut hasher = blake3::Hasher::new();
    hasher.update(b"rumoca-class-source-v1");
    hasher.update(class_name.as_bytes());

    if let Some(source_id) = tree.source_map.get_id(&location.file_name)
        && let Some((_, content)) = tree.source_map.get_source(source_id)
        && !content.is_empty()
    {
        let bytes = content.as_bytes();
        if start < end && end <= bytes.len() {
            hasher.update(&bytes[start..end]);
            return *hasher.finalize().as_bytes();
        }
    }

    let file_bytes = file_bytes_cache
        .entry(location.file_name.clone())
        .or_insert_with(|| std::fs::read(&location.file_name).ok());
    if let Some(bytes) = file_bytes.as_deref()
        && start < end
        && end <= bytes.len()
    {
        hasher.update(&bytes[start..end]);
        return *hasher.finalize().as_bytes();
    }

    // Fallback for virtual or unavailable files.
    hasher.update(location.file_name.as_bytes());
    hasher.update(&location.start.to_le_bytes());
    hasher.update(&location.end.to_le_bytes());
    hasher.update(format!("{:?}", class.class_type).as_bytes());
    hasher.update(class.name.text.as_bytes());
    *hasher.finalize().as_bytes()
}
