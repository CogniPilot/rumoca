//! Phase 2b: Contents Resolution - resolve equations, statements, expressions.
//!
//! This phase resolves component references in equations, algorithms, and
//! component start/modification expressions.

use crate::Resolver;
use rumoca_core::{DefId, ScopeId};
use rumoca_ir_ast as ast;
use std::sync::Arc;

type ClassDef = ast::ClassDef;
type ComponentRefPart = ast::ComponentRefPart;
type ComponentReference = ast::ComponentReference;
type Equation = ast::Equation;
type Expression = ast::Expression;
type ScopeKind = ast::ScopeKind;
type Statement = ast::Statement;
type StoredDefinition = ast::StoredDefinition;

impl Resolver {
    /// Resolve equations, statements, expressions in a StoredDefinition (Phase 2b).
    pub(crate) fn resolve_contents_all(
        &mut self,
        def: &mut StoredDefinition,
        scope: ScopeId,
        prefix: &str,
    ) {
        for (name, class) in def.classes.iter_mut() {
            let qualified_name = if prefix.is_empty() {
                name.clone()
            } else {
                format!("{}.{}", prefix, name)
            };
            self.resolve_contents_class(class, scope, &qualified_name);
        }
    }

    /// Resolve equations, statements, expressions in a ClassDef (Phase 2b).
    pub(crate) fn resolve_contents_class(
        &mut self,
        class: &mut ClassDef,
        _parent_scope: ScopeId,
        qualified_name: &str,
    ) {
        let class_scope = class
            .scope_id
            .expect("Class scope should be set in registration phase");

        // Resolve component references in equations and algorithms
        // MLS §5.3: Full name lookup happens during instantiation/flattening,
        // but we can do partial resolution here for the Class Tree.
        for eq in class.equations.iter_mut() {
            self.resolve_equation(eq, class_scope);
        }
        for eq in class.initial_equations.iter_mut() {
            self.resolve_equation(eq, class_scope);
        }
        for alg in class.algorithms.iter_mut() {
            for stmt in alg.iter_mut() {
                self.resolve_statement(stmt, class_scope);
            }
        }
        for alg in class.initial_algorithms.iter_mut() {
            for stmt in alg.iter_mut() {
                self.resolve_statement(stmt, class_scope);
            }
        }

        // Resolve component start/modification expressions and type names
        for (_name, comp) in class.components.iter_mut() {
            self.resolve_expression(&mut comp.start, class_scope);
            if let Some(binding) = &mut comp.binding {
                self.resolve_expression(binding, class_scope);
            }
            for mod_expr in comp.modifications.values_mut() {
                self.resolve_expression(mod_expr, class_scope);
            }
            if let Some(ref mut cond) = comp.condition {
                self.resolve_expression(cond, class_scope);
            }

            // Resolve the component's type name to its DefId (MLS §5.3).
            // This enables O(1) type lookup during instantiation instead of string-based search.
            // Builtins are registered in global scope, so normal lookup finds them.
            if let Some(type_def_id) = self.resolve_qualified_name(&comp.type_name, class_scope) {
                // Full resolution succeeded
                comp.type_name.def_id = Some(type_def_id);
                comp.type_def_id = Some(type_def_id);
                self.stats.types_fully_resolved += 1;
            } else if !comp.type_name.name.is_empty() {
                // Full resolution failed. Try partial resolution (MLS §7.3).
                self.try_partial_type_resolution(comp, class_scope, qualified_name);
            }
        }

        // Recursively resolve nested classes' contents
        for (name, nested) in class.classes.iter_mut() {
            let nested_qualified = format!("{}.{}", qualified_name, name);
            self.resolve_contents_class(nested, class_scope, &nested_qualified);
        }
    }

    /// Try partial type resolution for qualified names (MLS §7.3).
    ///
    /// For types like `Medium.AbsolutePressure` where `Medium` is a replaceable
    /// package, we can't fully resolve until instantiation when the actual
    /// redeclared type is known. Set type_name.def_id to the first part's DefId
    /// to indicate partial resolution succeeded.
    fn try_partial_type_resolution(
        &mut self,
        comp: &mut rumoca_ir_ast::Component,
        class_scope: ScopeId,
        qualified_name: &str,
    ) {
        let first_part = &comp.type_name.name[0].text;

        // First check direct scope lookup
        if let Some(first_def_id) = self.scope_tree.lookup(class_scope, first_part) {
            comp.type_name.def_id = Some(first_def_id);
            self.stats.types_partial_direct += 1;
            return;
        }

        // If not in direct scope, check inherited members from base classes.
        // We need to search the entire enclosing class hierarchy.
        if let Some(def_id) = self.find_inherited_type(qualified_name, first_part) {
            comp.type_name.def_id = Some(def_id);
            self.stats.types_partial_inherited += 1;
            return;
        }

        // Failed to resolve
        self.stats.types_unresolved += 1;
        self.stats
            .types_unresolved_details
            .push((comp.type_name.to_string(), qualified_name.to_string()));
    }

    /// Find an inherited type by searching the enclosing class hierarchy.
    fn find_inherited_type(
        &self,
        qualified_name: &str,
        type_name: &str,
    ) -> Option<rumoca_core::DefId> {
        // Try the current class first
        if let Some(def_id) = self.lookup_inherited_member(qualified_name, type_name) {
            return Some(def_id);
        }

        // Walk up the enclosing class hierarchy
        let mut container = qualified_name;
        while let Some(dot_pos) = container.rfind('.') {
            container = &container[..dot_pos];
            if let Some(def_id) = self.lookup_inherited_member(container, type_name) {
                return Some(def_id);
            }
        }

        None
    }

    /// Resolve references in an equation.
    pub(crate) fn resolve_equation(&mut self, eq: &mut Equation, scope: ScopeId) {
        match eq {
            Equation::Simple { lhs, rhs } => {
                self.resolve_expression(lhs, scope);
                self.resolve_expression(rhs, scope);
            }
            Equation::Connect { lhs, rhs } => {
                self.resolve_component_reference(lhs, scope);
                self.resolve_component_reference(rhs, scope);
            }
            Equation::For { indices, equations } => {
                // Create a for-loop scope for the index variables
                let for_scope = self.scope_tree.create_scope(scope, ScopeKind::ForLoop);
                for idx in indices {
                    let name = idx.ident.text.to_string();
                    let def_id = self.alloc_def_id(name.clone());
                    self.scope_tree.add_member(for_scope, name, def_id);
                    self.resolve_expression(&mut idx.range, for_scope);
                }
                self.resolve_equations(equations, for_scope);
            }
            Equation::If {
                cond_blocks,
                else_block,
            } => {
                for block in cond_blocks.iter_mut() {
                    self.resolve_equation_block(block, scope);
                }
                if let Some(else_body) = else_block {
                    self.resolve_equations(else_body, scope);
                }
            }
            Equation::When(blocks) => {
                for block in blocks.iter_mut() {
                    self.resolve_equation_block(block, scope);
                }
            }
            Equation::FunctionCall { comp, args } => {
                self.resolve_function_reference(comp, scope);
                self.resolve_expressions(args, scope);
            }
            Equation::Assert {
                condition,
                message,
                level,
            } => {
                self.resolve_expression(condition, scope);
                self.resolve_expression(message, scope);
                if let Some(lvl) = level {
                    self.resolve_expression(lvl, scope);
                }
            }
            Equation::Empty => {}
        }
    }

    /// Resolve references in a list of equations.
    fn resolve_equations(&mut self, equations: &mut [Equation], scope: ScopeId) {
        for eq in equations.iter_mut() {
            self.resolve_equation(eq, scope);
        }
    }

    /// Resolve references in an equation block (condition + equations).
    fn resolve_equation_block(&mut self, block: &mut rumoca_ir_ast::EquationBlock, scope: ScopeId) {
        self.resolve_expression(&mut block.cond, scope);
        self.resolve_equations(&mut block.eqs, scope);
    }

    /// Resolve references in a list of expressions.
    fn resolve_expressions(&mut self, exprs: &mut [Expression], scope: ScopeId) {
        for expr in exprs.iter_mut() {
            self.resolve_expression(expr, scope);
        }
    }

    /// Resolve references in a statement.
    pub(crate) fn resolve_statement(&mut self, stmt: &mut Statement, scope: ScopeId) {
        match stmt {
            Statement::Assignment { comp, value } => {
                self.resolve_component_reference(comp, scope);
                self.resolve_expression(value, scope);
            }
            Statement::FunctionCall {
                comp,
                args,
                outputs,
            } => {
                self.resolve_function_reference(comp, scope);
                self.resolve_expressions(args, scope);
                self.resolve_expressions(outputs, scope);
            }
            Statement::If {
                cond_blocks,
                else_block,
            } => {
                for block in cond_blocks.iter_mut() {
                    self.resolve_statement_block(block, scope);
                }
                if let Some(else_body) = else_block {
                    self.resolve_statements(else_body, scope);
                }
            }
            Statement::For { indices, equations } => {
                let for_scope = self.scope_tree.create_scope(scope, ScopeKind::ForLoop);
                for idx in indices {
                    let name = idx.ident.text.to_string();
                    let def_id = self.alloc_def_id(name.clone());
                    self.scope_tree.add_member(for_scope, name, def_id);
                    self.resolve_expression(&mut idx.range, for_scope);
                }
                self.resolve_statements(equations, for_scope);
            }
            Statement::While(block) => {
                self.resolve_expression(&mut block.cond, scope);
                self.resolve_statements(&mut block.stmts, scope);
            }
            Statement::When(blocks) => {
                for block in blocks.iter_mut() {
                    self.resolve_statement_block(block, scope);
                }
            }
            Statement::Reinit { variable, value } => {
                self.resolve_component_reference(variable, scope);
                self.resolve_expression(value, scope);
            }
            Statement::Assert {
                condition,
                message,
                level,
            } => {
                self.resolve_expression(condition, scope);
                self.resolve_expression(message, scope);
                if let Some(lvl) = level {
                    self.resolve_expression(lvl, scope);
                }
            }
            Statement::Return { .. } | Statement::Break { .. } | Statement::Empty => {}
        }
    }

    /// Resolve references in a list of statements.
    fn resolve_statements(&mut self, statements: &mut [Statement], scope: ScopeId) {
        for stmt in statements.iter_mut() {
            self.resolve_statement(stmt, scope);
        }
    }

    /// Resolve references in a statement block (condition + statements).
    fn resolve_statement_block(
        &mut self,
        block: &mut rumoca_ir_ast::StatementBlock,
        scope: ScopeId,
    ) {
        self.resolve_expression(&mut block.cond, scope);
        self.resolve_statements(&mut block.stmts, scope);
    }

    /// Resolve references in an expression.
    pub(crate) fn resolve_expression(&mut self, expr: &mut Expression, scope: ScopeId) {
        match expr {
            Expression::ComponentReference(comp) => {
                self.resolve_component_reference(comp, scope);
            }
            Expression::FunctionCall { comp, args } => {
                self.resolve_function_reference(comp, scope);
                for arg in args.iter_mut() {
                    self.resolve_expression(arg, scope);
                }
            }
            Expression::Binary { lhs, rhs, .. } => {
                // Arc::make_mut clones if shared, returns &mut
                self.resolve_expression(Arc::make_mut(lhs), scope);
                self.resolve_expression(Arc::make_mut(rhs), scope);
            }
            Expression::Unary { rhs, .. } => {
                self.resolve_expression(Arc::make_mut(rhs), scope);
            }
            Expression::Range { start, step, end } => {
                self.resolve_expression(Arc::make_mut(start), scope);
                if let Some(s) = step {
                    self.resolve_expression(Arc::make_mut(s), scope);
                }
                self.resolve_expression(Arc::make_mut(end), scope);
            }
            Expression::Array { elements, .. } => {
                for elem in elements.iter_mut() {
                    self.resolve_expression(elem, scope);
                }
            }
            Expression::If {
                branches,
                else_branch,
            } => {
                for (cond, then_expr) in branches.iter_mut() {
                    self.resolve_expression(cond, scope);
                    self.resolve_expression(then_expr, scope);
                }
                self.resolve_expression(Arc::make_mut(else_branch), scope);
            }
            Expression::ClassModification {
                target,
                modifications,
            } => {
                self.resolve_function_reference(target, scope);
                for m in modifications.iter_mut() {
                    self.resolve_expression(m, scope);
                }
            }
            Expression::Modification { target, value } => {
                self.resolve_component_reference(target, scope);
                self.resolve_expression(Arc::make_mut(value), scope);
            }
            Expression::NamedArgument { value, .. } => {
                self.resolve_expression(Arc::make_mut(value), scope);
            }
            Expression::Tuple { elements } => {
                for elem in elements.iter_mut() {
                    self.resolve_expression(elem, scope);
                }
            }
            Expression::Parenthesized { inner } => {
                self.resolve_expression(Arc::make_mut(inner), scope);
            }
            Expression::ArrayComprehension {
                expr,
                indices,
                filter,
            } => {
                // Array comprehension loop indices introduce a local scope,
                // similar to for-equation/for-statement indices.
                let comp_scope = self.scope_tree.create_scope(scope, ScopeKind::ForLoop);
                for idx in indices {
                    let name = idx.ident.text.to_string();
                    let def_id = self.alloc_def_id(name.clone());
                    self.scope_tree.add_member(comp_scope, name, def_id);
                    self.resolve_expression(&mut idx.range, comp_scope);
                }
                self.resolve_expression(Arc::make_mut(expr), comp_scope);
                if let Some(filt) = filter {
                    self.resolve_expression(Arc::make_mut(filt), comp_scope);
                }
            }
            Expression::ArrayIndex { base, subscripts } => {
                self.resolve_expression(Arc::make_mut(base), scope);
                self.resolve_subscripts(subscripts, scope);
            }
            Expression::FieldAccess { base, .. } => {
                self.resolve_expression(Arc::make_mut(base), scope);
            }
            // Terminal expressions don't contain references.
            Expression::Terminal { .. } | Expression::Empty => {}
        }
    }

    /// Resolve a component reference.
    ///
    /// MLS §5.3.1: Simple name lookup starts in the current scope and
    /// proceeds to enclosing scopes. For composite names (a.b.c), we
    /// only resolve the first part here; full resolution happens during
    /// instantiation when the instance tree is available.
    pub(crate) fn resolve_component_reference(
        &mut self,
        comp: &mut ComponentReference,
        scope: ScopeId,
    ) {
        if comp.parts.is_empty() {
            return;
        }

        // Get the first part of the reference
        let first_name = &comp.parts[0].ident.text;

        // Look up the name in the scope tree
        if let Some(def_id) = self.scope_tree.lookup(scope, first_name) {
            comp.def_id = Some(def_id);
            self.stats.comp_refs_resolved += 1;
        } else {
            self.stats.comp_refs_unresolved += 1;
        }
        // Note: We don't report undefined references here because:
        // 1. The name might be from an import that hasn't been resolved yet
        // 2. The name might be from a base class (extends)
        // 3. Full MLS name lookup happens during instantiation/flattening
        // Errors will be reported during type checking or instantiation.

        // Also resolve subscript expressions
        for part in comp.parts.iter_mut() {
            if let Some(subs) = &mut part.subs {
                self.resolve_subscripts(subs, scope);
            }
        }
    }

    /// Resolve a function reference to its canonical fully-qualified path.
    ///
    /// Unlike generic component references, function calls should resolve the
    /// entire path (including inherited package members) at resolve time so
    /// later phases do exact function lookup without name heuristics.
    fn resolve_function_reference(&mut self, comp: &mut ComponentReference, scope: ScopeId) {
        self.resolve_component_reference(comp, scope);

        let Some((resolved_def_id, qualified_name)) =
            self.resolve_component_reference_full_path(comp, scope)
        else {
            return;
        };

        comp.def_id = Some(resolved_def_id);
        rewrite_component_reference(comp, &qualified_name);
    }

    fn enclosing_class_qualified_name(&self, scope: ScopeId) -> Option<&str> {
        let mut current = Some(scope);
        while let Some(scope_id) = current {
            if let Some(class_def_id) = self.scope_to_class_def.get(&scope_id)
                && let Some(name) = self.def_names.get(class_def_id)
            {
                return Some(name.as_str());
            }
            current = self.scope_tree.parent(scope_id);
        }
        None
    }

    fn resolve_function_first_part(&self, first_part: &str, scope: ScopeId) -> Option<DefId> {
        if let Some(def_id) = self.scope_tree.lookup(scope, first_part) {
            return Some(def_id);
        }

        let mut container = self.enclosing_class_qualified_name(scope);
        while let Some(container_name) = container {
            if let Some(def_id) = self.lookup_inherited_member(container_name, first_part) {
                return Some(def_id);
            }
            container = container_name.rsplit_once('.').map(|(parent, _)| parent);
        }
        None
    }

    fn resolve_component_reference_full_path(
        &self,
        comp: &ComponentReference,
        scope: ScopeId,
    ) -> Option<(rumoca_core::DefId, String)> {
        let first_part = comp.parts.first()?.ident.text.as_ref();
        let mut current_def_id = comp
            .def_id
            .or_else(|| self.resolve_function_first_part(first_part, scope))?;
        let mut current_qualified = self.def_names.get(&current_def_id)?.clone();

        for part in comp.parts.iter().skip(1) {
            let member = part.ident.text.as_ref();
            let direct_name = format!("{current_qualified}.{member}");
            if let Some(&next_def_id) = self.name_to_def.get(&direct_name) {
                current_def_id = next_def_id;
                current_qualified = self.def_names.get(&next_def_id)?.clone();
                continue;
            }

            let inherited_def_id = self.lookup_inherited_member(&current_qualified, member)?;
            current_def_id = inherited_def_id;
            current_qualified = self.def_names.get(&inherited_def_id)?.clone();
        }

        Some((current_def_id, current_qualified))
    }

    /// Resolve references in a list of subscripts.
    fn resolve_subscripts(&mut self, subs: &mut [rumoca_ir_ast::Subscript], scope: ScopeId) {
        for sub in subs.iter_mut() {
            match sub {
                rumoca_ir_ast::Subscript::Expression(expr) => {
                    self.resolve_expression(expr, scope);
                }
                rumoca_ir_ast::Subscript::Range { .. } | rumoca_ir_ast::Subscript::Empty => {}
            }
        }
    }
}

fn rewrite_component_reference(comp: &mut ComponentReference, qualified_name: &str) {
    if comp.parts.is_empty() {
        return;
    }

    let original_parts = std::mem::take(&mut comp.parts);
    let old_len = original_parts.len();
    let new_parts: Vec<&str> = qualified_name.split('.').collect();
    let new_len = new_parts.len();

    comp.parts = new_parts
        .into_iter()
        .enumerate()
        .map(|(idx, part_name)| {
            // Right-align canonical segments against original segments so the
            // final identifier keeps the original source span.
            let aligned = idx + old_len >= new_len;
            let source_idx = if aligned {
                idx + old_len - new_len
            } else {
                old_len - 1
            };
            let source = &original_parts[source_idx];
            let mut ident = source.ident.clone();
            ident.text = std::sync::Arc::from(part_name);

            ComponentRefPart {
                ident,
                subs: if aligned { source.subs.clone() } else { None },
            }
        })
        .collect();
}
