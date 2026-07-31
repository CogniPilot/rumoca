//! Collapse structured reference trees onto the flat variables the model owns.
//!
//! Instantiation leaves a reference such as `comp[1].port_p.Phi` as an
//! `Index`/`FieldAccess` tree, while the flat model owns one scalarized
//! variable per leaf (plus a record instance for each scalarized record base).
//! This pass rewrites every such tree to the single `VarRef` that names the
//! flat variable, folding compile-time subscripts on the way (MLS §4.5).
//!
//! One reference resolves to more than one variable: `arr.member` where `arr`
//! is an array of components (MLS §10.5) denotes the array of the elements'
//! members. The occurrence graph already holds one occurrence per element, so
//! that reference expands here into an array expression over the element
//! variables — before, and independently of, any name shortening.

use super::*;
use rumoca_core::{ExpressionRewriter, StatementRewriter};

pub(crate) fn collapse_index_refs_to_known_varrefs(flat: &mut flat::Model) {
    let known_flat_vars = KnownFlatVars::build(flat);

    for eq in &mut flat.equations {
        collapse_index_expr(&mut eq.residual, &known_flat_vars);
    }
    for eq in &mut flat.initial_equations {
        collapse_index_expr(&mut eq.residual, &known_flat_vars);
    }
    // A structured family's comprehension template is a peer copy of its scalar
    // residual, and downstream phases read the template rather than
    // reconstructing it from the materialized cells. Collapsing only the scalar
    // copy would leave the template naming references the flat model does not
    // own.
    for family in flat
        .structured_equations
        .iter_mut()
        .chain(flat.initial_structured_equations.iter_mut())
    {
        let Some(template) = family.template.as_mut() else {
            continue;
        };
        for body in &mut template.body {
            collapse_index_expr(body, &known_flat_vars);
        }
    }
    for assert_eq in &mut flat.assert_equations {
        collapse_index_expr(&mut assert_eq.condition, &known_flat_vars);
        collapse_index_expr(&mut assert_eq.message, &known_flat_vars);
        if let Some(level) = &mut assert_eq.level {
            collapse_index_expr(level, &known_flat_vars);
        }
    }
    for assert_eq in &mut flat.initial_assert_equations {
        collapse_index_expr(&mut assert_eq.condition, &known_flat_vars);
        collapse_index_expr(&mut assert_eq.message, &known_flat_vars);
        if let Some(level) = &mut assert_eq.level {
            collapse_index_expr(level, &known_flat_vars);
        }
    }

    for var in flat.variables.values_mut() {
        if let Some(binding) = &mut var.binding {
            collapse_index_expr(binding, &known_flat_vars);
        }
        if let Some(start) = &mut var.start {
            collapse_index_expr(start, &known_flat_vars);
        }
        if let Some(min) = &mut var.min {
            collapse_index_expr(min, &known_flat_vars);
        }
        if let Some(max) = &mut var.max {
            collapse_index_expr(max, &known_flat_vars);
        }
        if let Some(nominal) = &mut var.nominal {
            collapse_index_expr(nominal, &known_flat_vars);
        }
    }

    for chain in &mut flat.when_chains {
        for branch in chain.branches_mut() {
            collapse_index_expr(&mut branch.condition, &known_flat_vars);
            collapse_index_when_equations(&mut branch.equations, &known_flat_vars);
        }
    }

    for algorithm in &mut flat.algorithms {
        collapse_index_statements(&mut algorithm.statements, &known_flat_vars);
    }
    for algorithm in &mut flat.initial_algorithms {
        collapse_index_statements(&mut algorithm.statements, &known_flat_vars);
    }

    for function in flat.functions.values_mut() {
        for input in &mut function.inputs {
            if let Some(default) = &mut input.default {
                collapse_index_expr(default, &known_flat_vars);
            }
        }
        for output in &mut function.outputs {
            if let Some(default) = &mut output.default {
                collapse_index_expr(default, &known_flat_vars);
            }
        }
        for local in &mut function.locals {
            if let Some(default) = &mut local.default {
                collapse_index_expr(default, &known_flat_vars);
            }
        }
        collapse_index_statements(&mut function.body, &known_flat_vars);
    }
}

fn collapse_index_when_equations(
    equations: &mut [rumoca_ir_flat::WhenEquation],
    known_flat_vars: &KnownFlatVars,
) {
    for equation in equations {
        match equation {
            rumoca_ir_flat::WhenEquation::Assign { value, .. }
            | rumoca_ir_flat::WhenEquation::Reinit { value, .. } => {
                collapse_index_expr(value, known_flat_vars);
            }
            rumoca_ir_flat::WhenEquation::Assert {
                condition,
                message,
                level,
                ..
            } => {
                collapse_index_expr(condition, known_flat_vars);
                collapse_index_expr(message, known_flat_vars);
                if let Some(level) = level {
                    collapse_index_expr(level, known_flat_vars);
                }
            }
            rumoca_ir_flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                for (cond, branch_equations) in branches {
                    collapse_index_expr(cond, known_flat_vars);
                    collapse_index_when_equations(branch_equations, known_flat_vars);
                }
                if let Some(else_branch) = else_branch {
                    collapse_index_when_equations(else_branch, known_flat_vars);
                }
            }
            rumoca_ir_flat::WhenEquation::FunctionCallOutputs { function, .. } => {
                collapse_index_expr(function, known_flat_vars);
            }
            rumoca_ir_flat::WhenEquation::Terminate { message, .. } => {
                collapse_index_expr(message, known_flat_vars)
            }
        }
    }
}

fn collapse_index_statements(
    statements: &mut [rumoca_core::Statement],
    known_flat_vars: &KnownFlatVars,
) {
    for statement in statements {
        *statement = CollapseIndexRewriter { known_flat_vars }.rewrite_statement(statement);
    }
}

fn collapse_index_expr(expr: &mut rumoca_core::Expression, known_flat_vars: &KnownFlatVars) {
    *expr = CollapseIndexRewriter { known_flat_vars }.rewrite_expression(expr);
}

/// Flat variable lookup for the collapse pass.
///
/// The primary route is exact: the occurrence graph resolves a reference path
/// to the occurrence it names, and the occurrence-keyed tables say which flat
/// variable (or scalarized record container) materialized it. The name-keyed
/// tables remain for references that carry no occurrence identity, such as the
/// flat-named variables connection generation writes.
struct KnownFlatVars {
    names: rustc_hash::FxHashMap<rumoca_core::VarNameId, rumoca_core::Reference>,
    record_instances: rustc_hash::FxHashMap<rumoca_core::VarNameId, rumoca_core::Reference>,
    /// Flat variables keyed by the exact occurrence they materialize.
    by_occurrence: rustc_hash::FxHashMap<rumoca_core::InstanceId, rumoca_core::Reference>,
    /// Scalarized record containers keyed by the exact occurrence they hold.
    records_by_occurrence: rustc_hash::FxHashMap<rumoca_core::InstanceId, rumoca_core::Reference>,
    /// Exact containment graph the reference paths are resolved against.
    occurrences: occurrence_graph::OccurrenceGraph,
    /// Compile-time integer values of `constant`/`parameter` variables, used to
    /// fold subscripts that are written as a symbolic reference (MLS §4.5
    /// requires array subscripts to be evaluable at compile time).
    integer_values: rustc_hash::FxHashMap<String, i64>,
    /// The same compile-time values keyed by exact occurrence, so a subscript
    /// reference folds without depending on how its name is spelled.
    integer_values_by_occurrence: rustc_hash::FxHashMap<rumoca_core::InstanceId, i64>,
}

impl KnownFlatVars {
    fn build(flat: &flat::Model) -> Self {
        let variable_references = || {
            flat.variables.iter().filter_map(|(name, var)| {
                let component_ref = var.component_ref.clone()?;
                Some((
                    name,
                    var.instance_id,
                    rumoca_core::Reference::with_component_reference(name.as_str(), component_ref)
                        .with_instance_id(var.instance_id),
                ))
            })
        };
        let names = variable_references()
            .map(|(name, _, reference)| (name.id(), reference))
            .collect();
        let by_occurrence = variable_references()
            .map(|(_, instance_id, reference)| (instance_id, reference))
            .collect();
        let record_references = || {
            flat.record_instances.iter().map(|(name, record)| {
                (
                    name,
                    record.instance_id,
                    rumoca_core::Reference::with_component_reference(
                        name.as_str(),
                        record.component_ref.clone(),
                    )
                    .with_instance_id(record.instance_id),
                )
            })
        };
        let record_instances = record_references()
            .map(|(name, _, reference)| (name.id(), reference))
            .collect();
        let records_by_occurrence = record_references()
            .map(|(_, instance_id, reference)| (instance_id, reference))
            .collect();
        let structural_values = || {
            flat.variables.iter().filter_map(|(name, var)| {
                rumoca_eval_flat::flat_int::structural_integer_value(var, flat)
                    .map(|value| (name, var.instance_id, value))
            })
        };
        let integer_values = structural_values()
            .map(|(name, _, value)| (name.as_str().to_string(), value))
            .collect();
        let integer_values_by_occurrence = structural_values()
            .map(|(_, instance_id, value)| (instance_id, value))
            .collect();
        Self {
            names,
            record_instances,
            by_occurrence,
            records_by_occurrence,
            occurrences: occurrence_graph::OccurrenceGraph::build(flat),
            integer_values,
            integer_values_by_occurrence,
        }
    }

    fn expression(&self, name: &str, span: rumoca_core::Span) -> Option<rumoca_core::Expression> {
        let name = rumoca_core::VarName::new(name);
        let reference = self.names.get(&name.id())?;
        Some(rumoca_core::Expression::VarRef {
            name: reference.with_var_name(name),
            subscripts: Vec::new(),
            span,
        })
    }

    /// Compile-time integer value of a `constant`/`parameter` flat variable.
    fn integer_value(&self, name: &str) -> Option<i64> {
        self.integer_values.get(name).copied()
    }

    /// Exact structured occurrence for a scalarized record base.
    fn record_base_expression(
        &self,
        path: &str,
        span: rumoca_core::Span,
    ) -> Option<rumoca_core::Expression> {
        let path = rumoca_core::VarName::new(path);
        let reference = self.record_instances.get(&path.id())?;
        Some(rumoca_core::Expression::VarRef {
            name: reference.with_var_name(path),
            subscripts: Vec::new(),
            span,
        })
    }

    /// Walk a reference expression over the occurrence graph.
    ///
    /// The walk starts at the class occurrence the reference was written in
    /// (`Reference::instance_id`) and selects one member per path part, so it
    /// is independent of how the reference's name happens to be rendered.
    fn path_cursor(&self, expr: &rumoca_core::Expression) -> Option<occurrence_graph::PathCursor> {
        match expr {
            rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } => {
                let scope = name.instance_id()?;
                // Only a class-body occurrence scopes a written path. A
                // reference this pass already collapsed carries the occurrence
                // it names instead, and its parts spell the whole flat path
                // rather than a path relative to that occurrence.
                if self.occurrences.kind(scope)? != flat::InstanceKind::Class {
                    return None;
                }
                let mut cursor = occurrence_graph::PathCursor::At(scope);
                for part in name.component_ref()?.parts() {
                    let indices = fold_indices(&part.subs, self)?;
                    cursor = self
                        .occurrences
                        .select_member(cursor, part.def_id, &indices)?;
                }
                self.occurrences
                    .apply_indices(cursor, &fold_indices(subscripts, self)?)
            }
            rumoca_core::Expression::Index {
                base, subscripts, ..
            } => {
                let cursor = self.path_cursor(base)?;
                self.occurrences
                    .apply_indices(cursor, &fold_indices(subscripts, self)?)
            }
            rumoca_core::Expression::FieldAccess {
                base, field_def_id, ..
            } => {
                let cursor = self.path_cursor(base)?;
                self.occurrences.select_member(cursor, *field_def_id, &[])
            }
            _ => None,
        }
    }

    /// Flat reference for whatever `cursor` names, if the model materialized it
    /// as a variable or retained it as a scalarized record container.
    fn cursor_expression(
        &self,
        cursor: occurrence_graph::PathCursor,
        span: rumoca_core::Span,
    ) -> Option<rumoca_core::Expression> {
        let occurrence_graph::PathCursor::At(instance_id) = cursor else {
            return None;
        };
        let reference = self
            .by_occurrence
            .get(&instance_id)
            .or_else(|| self.records_by_occurrence.get(&instance_id))?;
        Some(rumoca_core::Expression::VarRef {
            name: reference.clone(),
            subscripts: Vec::new(),
            span,
        })
    }

    /// Collapse `<base>.<field>` through the occurrence graph.
    fn field_occurrence_expression(
        &self,
        base: &rumoca_core::Expression,
        field_def_id: rumoca_core::DefId,
        span: rumoca_core::Span,
    ) -> Option<rumoca_core::Expression> {
        let cursor = self.path_cursor(base)?;
        if matches!(cursor, occurrence_graph::PathCursor::PendingIndices { .. }) {
            return self.expand_projection_elements(cursor, &[(field_def_id, Vec::new())], span);
        }
        let cursor = self.occurrences.select_member(cursor, field_def_id, &[])?;
        self.cursor_expression(cursor, span)
    }

    /// Expand a projection through an unindexed component array.
    ///
    /// `cursor` is parked on the array declaration and `members` are the parts
    /// still to be selected inside each element. `arr.member` denotes the array
    /// of the elements' members (MLS §10.5), which the model already owns one
    /// occurrence per element of, so the expansion is a walk rather than a
    /// rewrite of a rendered path.
    ///
    /// Accepted: a projection whose every element resolves to a flat variable
    /// or scalarized record container this model owns. Rejected as `None`: an
    /// element the walk cannot resolve, or a projection with nothing selected
    /// past the array — `arr` alone names the component array itself, not a
    /// member of it. A rejected projection is left exactly as written so the
    /// DAE phase reports it against its source span instead of this pass
    /// substituting a guess.
    fn expand_projection_elements(
        &self,
        cursor: occurrence_graph::PathCursor,
        members: &[(rumoca_core::DefId, Vec<i64>)],
        span: rumoca_core::Span,
    ) -> Option<rumoca_core::Expression> {
        if members.is_empty() {
            return None;
        }
        let element_cursors = self.occurrences.pending_elements(cursor)?;
        let mut elements = Vec::with_capacity(element_cursors.len());
        for mut element in element_cursors {
            for (declaration, indices) in members {
                element = self
                    .occurrences
                    .select_member(element, *declaration, indices)?;
            }
            elements.push(self.cursor_expression(element, span)?);
        }
        Some(rumoca_core::Expression::Array {
            elements,
            is_matrix: false,
            span,
        })
    }

    /// Expand `arr.member` spelled as one dotted reference.
    ///
    /// Flatten keeps a source-written path as a single `VarRef` whose parts are
    /// the written segments, so the projection that `field_occurrence_expression`
    /// handles for a `FieldAccess` tree arrives here as an unindexed array part
    /// followed by the projected members.
    fn component_array_projection_expression(
        &self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
    ) -> Option<rumoca_core::Expression> {
        // Subscripts on the reference itself select into the projected result,
        // which is a value rather than an occurrence; leave those to the
        // `Index` collapse above.
        if !subscripts.is_empty() {
            return None;
        }
        let scope = name.instance_id()?;
        if self.occurrences.kind(scope)? != flat::InstanceKind::Class {
            return None;
        }
        let parts = name.component_ref()?.parts();
        let mut cursor = occurrence_graph::PathCursor::At(scope);
        for (position, part) in parts.iter().enumerate() {
            let indices = fold_indices(&part.subs, self)?;
            cursor = self
                .occurrences
                .select_member(cursor, part.def_id, &indices)?;
            if matches!(cursor, occurrence_graph::PathCursor::PendingIndices { .. }) {
                let members = parts[position + 1..]
                    .iter()
                    .map(|part| Some((part.def_id, fold_indices(&part.subs, self)?)))
                    .collect::<Option<Vec<_>>>()?;
                return self.expand_projection_elements(cursor, &members, span);
            }
        }
        None
    }

    /// Collapse `<base>[i...]` through the occurrence graph.
    ///
    /// The subscripts select a separate element occurrence when the base is an
    /// array component (each element is its own occurrence). When the base is a
    /// materialized array variable there is one occurrence for the whole array,
    /// so the subscripts stay on the resolved reference.
    fn indexed_occurrence_expression(
        &self,
        base: &rumoca_core::Expression,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
    ) -> Option<rumoca_core::Expression> {
        let cursor = self.path_cursor(base)?;
        if let Some(indices) = fold_indices(subscripts, self)
            && let Some(element) = self.occurrences.apply_indices(cursor, &indices)
            && let Some(expression) = self.cursor_expression(element, span)
        {
            return Some(expression);
        }
        let occurrence_graph::PathCursor::At(instance_id) = cursor else {
            return None;
        };
        let reference = self.by_occurrence.get(&instance_id)?;
        Some(rumoca_core::Expression::VarRef {
            name: reference.clone(),
            subscripts: subscripts.to_vec(),
            span,
        })
    }

    /// Compile-time integer value of whatever occurrence `expr` names.
    fn occurrence_integer_value(&self, expr: &rumoca_core::Expression) -> Option<i64> {
        if self.occurrences.is_empty() {
            return None;
        }
        let occurrence_graph::PathCursor::At(instance_id) = self.path_cursor(expr)? else {
            return None;
        };
        self.integer_values_by_occurrence.get(&instance_id).copied()
    }
}

struct CollapseIndexRewriter<'a> {
    known_flat_vars: &'a KnownFlatVars,
}

impl ExpressionRewriter for CollapseIndexRewriter<'_> {
    fn rewrite_expression(&mut self, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
        if let rumoca_core::Expression::VarRef {
            name,
            subscripts,
            span,
        } = expr
            && let Some(expanded) = self
                .known_flat_vars
                .component_array_projection_expression(name, subscripts, *span)
        {
            return expanded;
        }
        if let rumoca_core::Expression::FieldAccess {
            base,
            field,
            field_def_id,
            span,
        } = expr
        {
            let base = self.rewrite_expression(base);
            if let Some(collapsed) =
                self.known_flat_vars
                    .field_occurrence_expression(&base, *field_def_id, *span)
            {
                return collapsed;
            }
            if let Some(collapsed) =
                collapse_field_access_to_known_var(&base, field, *span, self.known_flat_vars)
            {
                return collapsed;
            }
            return rumoca_core::Expression::FieldAccess {
                base: Box::new(base),
                field: field.clone(),
                field_def_id: *field_def_id,
                span: *span,
            };
        }
        if let rumoca_core::Expression::Index {
            base,
            subscripts,
            span,
        } = expr
        {
            let base = self.rewrite_expression(base);
            let subscripts = self.rewrite_subscripts(subscripts);
            if let Some(collapsed) =
                self.known_flat_vars
                    .indexed_occurrence_expression(&base, &subscripts, *span)
            {
                return collapsed;
            }
            if let Some(collapsed) =
                collapse_indexed_expression(&base, &subscripts, *span, self.known_flat_vars)
            {
                return collapsed;
            }
            return rumoca_core::Expression::Index {
                base: Box::new(base),
                subscripts,
                span: *span,
            };
        }
        self.walk_expression(expr)
    }
}

impl StatementRewriter for CollapseIndexRewriter<'_> {}

/// Collapse `<base>[i...]` onto a known flat variable, for whichever shape the
/// already-rewritten base has.
fn collapse_indexed_expression(
    base: &rumoca_core::Expression,
    subscripts: &[rumoca_core::Subscript],
    span: rumoca_core::Span,
    known_flat_vars: &KnownFlatVars,
) -> Option<rumoca_core::Expression> {
    if let rumoca_core::Expression::VarRef {
        name,
        subscripts: base_subscripts,
        ..
    } = base
    {
        return collapse_indexed_var_ref_to_known_var(
            name,
            base_subscripts,
            subscripts,
            span,
            known_flat_vars,
        );
    }
    collapse_indexed_field_access_to_known_var(base, subscripts, span, known_flat_vars)
}

/// Collapse `<field-access chain>[i]` onto a known flat variable.
///
/// A component that is itself an array element keeps a subscripted part in the
/// middle of its path (`plugToPins_p.plugToPin_p[1].plug_p.pin`), so flatten
/// leaves the reference as a field-access chain rather than a single dotted
/// `VarRef`. Rendering the chain to its flat path lets the same
/// known-variable/record-base collapse apply.
fn collapse_indexed_field_access_to_known_var(
    base: &rumoca_core::Expression,
    subscripts: &[rumoca_core::Subscript],
    span: rumoca_core::Span,
    known_flat_vars: &KnownFlatVars,
) -> Option<rumoca_core::Expression> {
    let base_path = rumoca_core::flat_expression_component_path(base)?.to_flat_string();
    let candidate = format!(
        "{base_path}{}",
        subscript_suffix(subscripts, known_flat_vars)?
    );
    if let Some(expression) = known_flat_vars.expression(candidate.as_str(), span) {
        return Some(expression);
    }
    known_flat_vars.record_base_expression(&candidate, span)
}

fn collapse_indexed_var_ref_to_known_var(
    name: &rumoca_core::Reference,
    base_subscripts: &[rumoca_core::Subscript],
    subscripts: &[rumoca_core::Subscript],
    span: rumoca_core::Span,
    known_flat_vars: &KnownFlatVars,
) -> Option<rumoca_core::Expression> {
    let mut merged = base_subscripts.to_vec();
    merged.extend_from_slice(subscripts);
    if let Some(suffix) = subscript_suffix(&merged, known_flat_vars) {
        let candidate = format!("{}{}", name.as_str(), suffix);
        if let Some(expression) = known_flat_vars.expression(candidate.as_str(), span) {
            return Some(expression);
        }
        // Element of a scalarized record array (`r[2]` whose flat variables
        // are the field leaves `r[2].a`...): same record-base collapse as for
        // field accesses.
        if let Some(expression) = known_flat_vars.record_base_expression(&candidate, span) {
            return Some(expression);
        }
    }
    if known_flat_vars.names.contains_key(&name.var_name().id()) {
        return Some(rumoca_core::Expression::VarRef {
            name: name.clone(),
            subscripts: merged,
            span,
        });
    }
    None
}

fn collapse_field_access_to_known_var(
    base: &rumoca_core::Expression,
    field: &str,
    span: rumoca_core::Span,
    known_flat_vars: &KnownFlatVars,
) -> Option<rumoca_core::Expression> {
    if let Some(candidate) = field_access_flat_path(base, field) {
        if let Some(expression) = known_flat_vars.expression(candidate.as_str(), span) {
            return Some(expression);
        }
        // Scalarized record base (`comp[1].port_p.Phi` where only the
        // `.re`/`.im` leaves exist as flat variables): collapse to a single
        // structured VarRef so downstream record-equation expansion sees the
        // record reference instead of an Index/FieldAccess tree it cannot
        // match (and shape inference does not inflate the equation to the
        // whole component array).
        if let Some(expression) = known_flat_vars.record_base_expression(&candidate, span) {
            return Some(expression);
        }
    }

    match base {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => collapse_var_field_access(name.as_str(), subscripts, field, span, known_flat_vars),
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            let rumoca_core::Expression::VarRef {
                name,
                subscripts: base_subscripts,
                ..
            } = base.as_ref()
            else {
                return None;
            };
            let mut merged = base_subscripts.clone();
            merged.extend_from_slice(subscripts);
            collapse_var_field_access(name.as_str(), &merged, field, span, known_flat_vars)
        }
        _ => None,
    }
}

pub(crate) fn field_access_flat_path(
    base: &rumoca_core::Expression,
    field: &str,
) -> Option<String> {
    Some(format!(
        "{}.{field}",
        rumoca_core::flat_expression_component_path(base)?.to_flat_string()
    ))
}

fn collapse_var_field_access(
    base_name: &str,
    subscripts: &[rumoca_core::Subscript],
    field: &str,
    span: rumoca_core::Span,
    known_flat_vars: &KnownFlatVars,
) -> Option<rumoca_core::Expression> {
    let subscript_suffix = subscript_suffix(subscripts, known_flat_vars)?;
    for candidate in [
        format!("{base_name}{subscript_suffix}.{field}"),
        format!("{base_name}.{field}{subscript_suffix}"),
    ] {
        if let Some(expression) = known_flat_vars.expression(candidate.as_str(), span) {
            return Some(expression);
        }
    }
    None
}

/// Compile-time element indices a subscript list selects (MLS §4.5).
///
/// An empty list yields an empty selection; a subscript that is not evaluable
/// at compile time yields `None` so the reference is left untouched.
fn fold_indices(
    subscripts: &[rumoca_core::Subscript],
    known_flat_vars: &KnownFlatVars,
) -> Option<Vec<i64>> {
    let mut indices = Vec::with_capacity(subscripts.len());
    for subscript in subscripts {
        match subscript {
            rumoca_core::Subscript::Index { value, .. } => indices.push(*value),
            rumoca_core::Subscript::Expr { expr, .. } => {
                indices.push(fold_subscript_expr(expr, known_flat_vars, 0)?);
            }
            rumoca_core::Subscript::Colon { .. } => return None,
        }
    }
    Some(indices)
}

fn subscript_suffix(
    subscripts: &[rumoca_core::Subscript],
    known_flat_vars: &KnownFlatVars,
) -> Option<String> {
    let indices = fold_indices(subscripts, known_flat_vars)?;
    if indices.is_empty() {
        return Some(String::new());
    }
    let values: Vec<String> = indices.iter().map(i64::to_string).collect();
    Some(format!("[{}]", values.join(",")))
}

/// Maximum expression depth folded while resolving one subscript.
const MAX_SUBSCRIPT_FOLD_DEPTH: u8 = 8;

/// Fold a subscript expression to its compile-time integer value.
///
/// MLS §4.5 requires an array subscript to be evaluable at compile time, so a
/// subscript is either an integer literal, a reference to a `constant` or
/// `parameter` with a known binding, or arithmetic over those. Anything else
/// (a discrete/continuous variable, an unbound parameter, a `for` index that
/// survived expansion) yields `None`, leaving the reference untouched.
fn fold_subscript_expr(
    expr: &rumoca_core::Expression,
    known_flat_vars: &KnownFlatVars,
    depth: u8,
) -> Option<i64> {
    if depth > MAX_SUBSCRIPT_FOLD_DEPTH {
        return None;
    }
    match expr {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(value),
            ..
        } => Some(*value),
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => known_flat_vars
            .occurrence_integer_value(expr)
            .or_else(|| known_flat_vars.integer_value(name.as_str())),
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
            let lhs = fold_subscript_expr(lhs, known_flat_vars, depth + 1)?;
            let rhs = fold_subscript_expr(rhs, known_flat_vars, depth + 1)?;
            rumoca_eval_flat::flat_int::eval_binary_op_i64(op, lhs, rhs)
        }
        _ => None,
    }
}
