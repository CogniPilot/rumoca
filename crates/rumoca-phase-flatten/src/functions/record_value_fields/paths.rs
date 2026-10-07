//! Reference paths through a split record value: which paths use a record
//! node whole, how a whole write of a split node expands into field writes,
//! and how every remaining path is rewritten onto its field local.

use super::*;

/// A component path read by an expression: a reference, or a field access
/// chain over one. `trailing` holds the expression subscripts of the last
/// part of a reference.
struct ExpressionPath {
    parts: Vec<ComponentRefPart>,
    trailing: Vec<rumoca_core::Subscript>,
}

fn expression_path(expr: &Expression) -> Option<ExpressionPath> {
    match expr {
        Expression::VarRef {
            name, subscripts, ..
        } => Some(ExpressionPath {
            parts: name.component_ref()?.parts().to_vec(),
            trailing: subscripts.clone(),
        }),
        Expression::FieldAccess {
            base,
            field,
            field_def_id,
            span,
        } => {
            let mut path = expression_path(base)?;
            if let Some(last) = path.parts.last_mut() {
                last.subs.append(&mut path.trailing);
            }
            path.parts
                .push(part(field, *field_def_id, Vec::new(), *span));
            Some(path)
        }
        Expression::Index {
            base, subscripts, ..
        } => {
            let mut path = expression_path(base)?;
            path.trailing.extend(subscripts.iter().cloned());
            Some(path)
        }
        _ => None,
    }
}

/// Whether a whole write of a record node can be expanded field by field
/// without evaluating its value more than once: a record constructor, a
/// component path, or an array or comprehension of those.
fn has_field_projection(value: &Expression) -> bool {
    match value {
        Expression::FunctionCall {
            is_constructor: true,
            ..
        } => true,
        Expression::Array {
            elements,
            kind: rumoca_core::ArrayConstructor::Array,
            ..
        } => !elements.is_empty() && elements.iter().all(has_field_projection),
        Expression::ArrayComprehension { expr, .. } => has_field_projection(expr),
        Expression::VarRef { subscripts, .. } => {
            subscripts.is_empty() && expression_path(value).is_some()
        }
        Expression::FieldAccess { .. } => expression_path(value).is_some(),
        _ => false,
    }
}

/// Whether `value` reads any path of the record named `root`. A whole write
/// projected field by field would let a later field's value see an earlier
/// field's new value; MLS §11.2.1 evaluates the whole value first.
fn reads_record(value: &Expression, root: &str) -> bool {
    let mut reads = RecordReads { root, found: false };
    reads.visit_expression(value);
    reads.found
}

/// Finds a read of any path under one record root.
struct RecordReads<'root> {
    root: &'root str,
    found: bool,
}

impl ExpressionVisitor for RecordReads<'_> {
    fn visit_var_ref(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
    ) {
        match name.component_ref() {
            Some(comp) => {
                self.found |= comp
                    .parts()
                    .first()
                    .is_some_and(|part| part.ident == self.root);
                for subscript in comp.parts().iter().flat_map(|part| &part.subs) {
                    self.visit_subscript(subscript);
                }
            }
            None => self.found |= name.segments().first() == Some(&self.root),
        }
        self.walk_var_ref(name, subscripts);
    }
}

/// How a body uses the record node at a path.
#[derive(Clone, Copy, Default)]
pub(super) struct NodeUses {
    /// The node is read as one value (not through one of its fields).
    pub(super) read_whole: bool,
    /// The node is written whole (an assignment or a call result).
    pub(super) written_whole: bool,
    /// Some statement writes a path strictly below the node.
    pub(super) written_below: bool,
    /// Some expression reads a path strictly below the node.
    pub(super) read_below: bool,
    /// Statements that write exactly this path, with no subscript on any part
    /// (a subscripted write updates one element, not the whole value).
    pub(super) writes: usize,
}

/// The uses of every component path a function body reads or writes, by
/// the identifiers of its parts, collected in one pass.
#[derive(Default)]
pub(super) struct BodyPaths {
    uses: HashMap<Vec<String>, NodeUses>,
}

impl BodyPaths {
    pub(super) fn collect(statements: &[Statement]) -> Self {
        let mut paths = Self::default();
        paths.statements(statements);
        paths
    }

    pub(super) fn uses(&self, path: &[String]) -> NodeUses {
        self.uses.get(path).copied().unwrap_or_default()
    }

    /// Whether some path below `root` is written by more than one statement.
    pub(super) fn overwrites_below(&self, root: &str) -> bool {
        self.uses
            .iter()
            .any(|(path, uses)| path.len() > 1 && path[0] == root && uses.writes > 1)
    }

    fn entry(&mut self, parts: &[ComponentRefPart]) -> &mut NodeUses {
        let idents = parts.iter().map(|part| part.ident.clone()).collect();
        self.uses.entry(idents).or_default()
    }

    fn target(&mut self, comp: &ComponentReference) {
        let parts = comp.parts();
        let node = self.entry(parts);
        node.written_whole = true;
        node.writes += usize::from(parts.iter().all(|part| part.subs.is_empty()));
        for prefix in 1..parts.len() {
            self.entry(&parts[..prefix]).written_below = true;
        }
        self.part_subscripts(parts);
    }

    fn part_subscripts(&mut self, parts: &[ComponentRefPart]) {
        for part in parts {
            self.subscripts(&part.subs);
        }
    }

    fn subscripts(&mut self, subscripts: &[rumoca_core::Subscript]) {
        for subscript in subscripts {
            if let rumoca_core::Subscript::Expr { expr, .. } = subscript {
                self.visit_expression(expr);
            }
        }
    }

    fn statements(&mut self, statements: &[Statement]) {
        for statement in statements {
            self.statement(statement);
        }
    }

    fn statement(&mut self, statement: &Statement) {
        match statement {
            Statement::Assignment { comp, value, .. } => {
                self.target(comp);
                self.visit_expression(value);
            }
            Statement::FunctionCall { args, outputs, .. } => {
                for arg in args {
                    self.visit_expression(arg);
                }
                for output in outputs.iter().flatten() {
                    self.target(output);
                }
            }
            Statement::If {
                cond_blocks,
                else_block,
                ..
            } => {
                for block in cond_blocks {
                    self.visit_expression(&block.cond);
                    self.statements(&block.stmts);
                }
                if let Some(block) = else_block {
                    self.statements(block);
                }
            }
            Statement::When { blocks, .. } => {
                for block in blocks {
                    self.visit_expression(&block.cond);
                    self.statements(&block.stmts);
                }
            }
            Statement::For {
                indices, equations, ..
            } => {
                for index in indices {
                    self.visit_expression(&index.range);
                }
                self.statements(equations);
            }
            Statement::While { block, .. } => {
                self.visit_expression(&block.cond);
                self.statements(&block.stmts);
            }
            Statement::Assert {
                condition,
                message,
                level,
                ..
            } => {
                self.visit_expression(condition);
                self.visit_expression(message);
                if let Some(level) = level {
                    self.visit_expression(level);
                }
            }
            Statement::Reinit {
                variable, value, ..
            } => {
                self.target(variable);
                self.visit_expression(value);
            }
            Statement::Empty { .. } | Statement::Return { .. } | Statement::Break { .. } => {}
        }
    }
}

impl ExpressionVisitor for BodyPaths {
    fn visit_expression(&mut self, expr: &Expression) {
        let Some(path) = expression_path(expr) else {
            return self.walk_expression(expr);
        };
        self.entry(&path.parts).read_whole = true;
        for prefix in 1..path.parts.len() {
            self.entry(&path.parts[..prefix]).read_below = true;
        }
        self.part_subscripts(&path.parts);
        self.subscripts(&path.trailing);
    }
}

/// Subscripts of the column a path selects: each enclosing array of records
/// contributes its part's subscripts, padded with `:` over its remaining
/// axes when a later part selects elements.
#[derive(Default)]
struct ColumnSubscripts {
    selected: Vec<rumoca_core::Subscript>,
    pending: Vec<Span>,
}

impl ColumnSubscripts {
    /// Whether the path crosses no enclosing array of records.
    fn is_empty(&self) -> bool {
        self.selected.is_empty() && self.pending.is_empty()
    }

    fn push(&mut self, subs: &[rumoca_core::Subscript], rank: usize, span: Span) {
        self.extend(subs);
        self.pending
            .extend(std::iter::repeat_n(span, rank.saturating_sub(subs.len())));
    }

    fn extend(&mut self, subs: &[rumoca_core::Subscript]) {
        if subs.is_empty() {
            return;
        }
        self.selected.extend(
            self.pending
                .drain(..)
                .map(|span| rumoca_core::Subscript::Colon { span }),
        );
        self.selected.extend(subs.iter().cloned());
    }

    fn finish(mut self, subs: &[rumoca_core::Subscript]) -> Vec<rumoca_core::Subscript> {
        self.extend(subs);
        self.selected
    }

    /// Whether the path selects one element of an array of records of `rank`
    /// axes: every axis has a scalar subscript.
    fn selects_element(&self, rank: usize) -> bool {
        self.pending.is_empty()
            && self.selected.len() == rank
            && self.selected.iter().all(|subscript| {
                matches!(
                    subscript,
                    rumoca_core::Subscript::Expr { .. } | rumoca_core::Subscript::Index { .. }
                )
            })
    }
}

/// Where a path under the split record lands.
enum Landing<'tree, 'path> {
    /// A field held in one local, the column subscripts of the enclosing
    /// arrays of records, and the parts below the field.
    Local {
        field: &'tree SplitField,
        columns: ColumnSubscripts,
        part: &'path ComponentRefPart,
        rest: &'path [ComponentRefPart],
    },
    /// A split record node itself, and the column subscripts it selects.
    Node(SplitNode<'tree>, ColumnSubscripts),
}

fn land<'tree, 'path>(
    record: &'tree SplitRecord,
    parts: &'path [ComponentRefPart],
    rewrite: &mut impl FnMut(&[rumoca_core::Subscript]) -> Vec<rumoca_core::Subscript>,
) -> Option<Landing<'tree, 'path>> {
    let [root, below @ ..] = parts else {
        return None;
    };
    if root.ident != record.name || !root.subs.is_empty() {
        return None;
    }
    let mut node = SplitNode::Root(record);
    let mut columns = ColumnSubscripts::default();
    for (index, part) in below.iter().enumerate() {
        let field = node
            .fields()
            .iter()
            .find(|field| field.param.name == part.ident)?;
        if field.fields.is_none() {
            return Some(Landing::Local {
                field,
                columns,
                part,
                rest: &below[index + 1..],
            });
        }
        columns.push(&rewrite(&part.subs), field.rank(), part.span);
        node = SplitNode::Field(field);
    }
    Some(Landing::Node(node, columns))
}

/// Expands every whole write of a split record node into one write per field
/// of its value, recursively, in statement order.
///
/// A value with a field-wise projection that reads no path of the record is
/// projected in place. Any other value (a function call, a conditional
/// expression, a call result, or a value reading the record it writes) is first
/// written whole to the node's own record-valued local, named like the
/// node's field prefix (the record's holder for the root), and the node's
/// fields are projected from that local, so the value is evaluated exactly
/// once.
pub(super) struct WholeWriteExpander<'record> {
    pub(super) record: &'record SplitRecord,
    pub(super) function_name: &'record str,
    pub(super) aggregate_values: &'record HashSet<String>,
    /// Split nodes whose record-valued local holds a whole value, in first
    /// use order.
    pub(super) value_locals: Vec<(&'record SplitField, bool)>,
    /// Whether the record's holder holds a whole value of the root.
    pub(super) root_held: bool,
}

impl<'record> WholeWriteExpander<'record> {
    pub(super) fn expand(
        &mut self,
        statements: &[Statement],
    ) -> Result<Vec<Statement>, FlattenError> {
        let mut expanded = Vec::with_capacity(statements.len());
        for statement in statements {
            self.expand_statement(statement, &mut expanded)?;
        }
        Ok(expanded)
    }

    fn expand_statement(
        &mut self,
        statement: &Statement,
        expanded: &mut Vec<Statement>,
    ) -> Result<(), FlattenError> {
        match statement {
            Statement::Assignment { comp, value, span } => {
                self.expand_assignment(statement, comp, value, *span, expanded)?;
            }
            Statement::FunctionCall {
                comp,
                args,
                outputs,
                span,
            } => {
                let mut projected = Vec::new();
                let outputs = outputs
                    .iter()
                    .map(|output| self.call_output(output.as_ref(), *span, &mut projected))
                    .collect::<Result<Vec<_>, FlattenError>>()?;
                expanded.push(Statement::FunctionCall {
                    comp: comp.clone(),
                    args: args.clone(),
                    outputs,
                    span: *span,
                });
                expanded.extend(self.expand(&projected)?);
            }
            Statement::If {
                cond_blocks,
                else_block,
                span,
            } => expanded.push(Statement::If {
                cond_blocks: cond_blocks
                    .iter()
                    .map(|block| self.expand_block(block))
                    .collect::<Result<_, FlattenError>>()?,
                else_block: else_block
                    .as_deref()
                    .map(|block| self.expand(block))
                    .transpose()?,
                span: *span,
            }),
            Statement::For {
                indices,
                equations,
                span,
            } => expanded.push(Statement::For {
                indices: indices.clone(),
                equations: self.expand(equations)?,
                span: *span,
            }),
            Statement::While { block, span } => expanded.push(Statement::While {
                block: self.expand_block(block)?,
                span: *span,
            }),
            other => expanded.push(other.clone()),
        }
        Ok(())
    }

    fn expand_block(
        &mut self,
        block: &rumoca_core::StatementBlock,
    ) -> Result<rumoca_core::StatementBlock, FlattenError> {
        Ok(rumoca_core::StatementBlock {
            cond: block.cond.clone(),
            stmts: self.expand(&block.stmts)?,
        })
    }

    fn expand_assignment(
        &mut self,
        statement: &Statement,
        comp: &ComponentReference,
        value: &Expression,
        span: Span,
        expanded: &mut Vec<Statement>,
    ) -> Result<(), FlattenError> {
        let Some((node, local, element)) = self.split_node(comp) else {
            expanded.push(statement.clone());
            return Ok(());
        };
        let writes = if has_field_projection(value) && !reads_record(value, &self.record.name) {
            self.field_writes(node, comp, value, span)?
        } else {
            let held = self.hold(node, &local, element, span);
            expanded.push(Statement::Assignment {
                comp: local,
                value: value.clone(),
                span,
            });
            self.field_writes(node, comp, &held, span)?
        };
        expanded.extend(self.expand(&writes)?);
        Ok(())
    }

    /// A call result written to a split node lands in the node's record
    /// local, whose fields are projected after the call.
    fn call_output(
        &mut self,
        output: Option<&ComponentReference>,
        span: Span,
        projected: &mut Vec<Statement>,
    ) -> Result<Option<ComponentReference>, FlattenError> {
        let Some(output) = output else {
            return Ok(None);
        };
        let Some((node, local, element)) = self.split_node(output) else {
            return Ok(Some(output.clone()));
        };
        let held = self.hold(node, &local, element, span);
        projected.extend(self.field_writes(node, output, &held, span)?);
        Ok(Some(local))
    }

    /// The split node a target names whole, and the reference to the node's
    /// record-valued local at the same element selection.
    fn split_node(
        &self,
        comp: &ComponentReference,
    ) -> Option<(SplitNode<'record>, ComponentReference, bool)> {
        let Landing::Node(node, columns) =
            land(self.record, comp.parts(), &mut |subs| subs.to_vec())?
        else {
            return None;
        };
        let span = comp.span();
        // One element of an array of records is held in a record local of its
        // own, not in an element of the array-valued local.
        let element = matches!(node, SplitNode::Field(field)
            if field.enclosing.is_empty() && !columns.is_empty() && columns.selects_element(field.rank()));
        let local = match node {
            SplitNode::Root(record) => part(&record.holder, record.def_id, Vec::new(), span),
            SplitNode::Field(field) if element => {
                part(&field.element_local(), field.def_id, Vec::new(), span)
            }
            SplitNode::Field(field) => part(&field.local, field.def_id, columns.finish(&[]), span),
        };
        Some((node, reference(false, span, vec![local]), element))
    }

    /// Declare `node`'s record-valued local and read it at `local`.
    fn hold(
        &mut self,
        node: SplitNode<'record>,
        local: &ComponentReference,
        element: bool,
        span: Span,
    ) -> Expression {
        match node {
            SplitNode::Root(_) => self.root_held = true,
            SplitNode::Field(node) => {
                if !self.value_locals.iter().any(|(held, held_element)| {
                    std::ptr::eq(*held, node) && *held_element == element
                }) {
                    self.value_locals.push((node, element));
                }
            }
        }
        Expression::VarRef {
            name: rumoca_core::Reference::with_component_reference(
                local.to_var_name().as_str(),
                local.clone(),
            ),
            subscripts: Vec::new(),
            span,
        }
    }

    /// `node := value` as `node.f := value.f` for every field `f` of the node.
    fn field_writes(
        &self,
        node: SplitNode<'_>,
        comp: &ComponentReference,
        value: &Expression,
        span: Span,
    ) -> Result<Vec<Statement>, FlattenError> {
        let fields = node.fields();
        let params = fields
            .iter()
            .map(|field| field.param.clone())
            .collect::<Vec<_>>();
        let mut values = Vec::with_capacity(params.len());
        expand_record_arg(
            self.function_name,
            value,
            &params,
            None,
            Some(self.aggregate_values),
            &mut values,
        )?;
        Ok(fields
            .iter()
            .zip(values)
            .map(|(field, value)| {
                let mut parts = comp.parts().to_vec();
                parts.push(part(&field.param.name, field.def_id, Vec::new(), span));
                Statement::Assignment {
                    comp: reference(comp.local(), comp.span(), parts),
                    value,
                    span,
                }
            })
            .collect())
    }
}

/// Rewrites every path through a split record onto its field local.
pub(super) struct SplitRewriter<'record> {
    pub(super) record: &'record SplitRecord,
}

impl<'record> SplitRewriter<'record> {
    /// The local part a landed path starts with, and the parts below it.
    fn local_parts(
        &mut self,
        landing: Landing<'_, '_>,
        trailing: &[rumoca_core::Subscript],
    ) -> Option<(Vec<ComponentRefPart>, Vec<rumoca_core::Subscript>)> {
        let Landing::Local {
            field,
            columns,
            part: field_part,
            rest,
        } = landing
        else {
            return None;
        };
        let own = self.rewrite_subscripts(&field_part.subs);
        let trailing = self.rewrite_subscripts(trailing);
        if rest.is_empty() {
            let subs = columns.finish(&[own, trailing].concat());
            let local = part(&field.local, field.def_id, Vec::new(), field_part.span);
            return Some((vec![local], subs));
        }
        let mut parts = vec![part(
            &field.local,
            field.def_id,
            columns.finish(&own),
            field_part.span,
        )];
        parts.extend(
            rest.iter()
                .map(|part| self.rewrite_component_ref_part(part)),
        );
        Some((parts, trailing))
    }

    fn land_path<'tree, 'path>(
        &mut self,
        parts: &'path [ComponentRefPart],
    ) -> Option<Landing<'tree, 'path>>
    where
        'record: 'tree,
    {
        let record = self.record;
        land(record, parts, &mut |subs| self.rewrite_subscripts(subs))
    }
}

impl ExpressionRewriter for SplitRewriter<'_> {
    fn rewrite_expression(&mut self, expr: &Expression) -> Expression {
        let Some(path) = expression_path(expr) else {
            return self.walk_expression(expr);
        };
        let Some(landing) = self.land_path(&path.parts) else {
            return self.walk_expression(expr);
        };
        if let Landing::Node(node, columns) = &landing
            && columns.is_empty()
            && path.trailing.is_empty()
            && let Some(value) = reassembled(*node, expr.span().unwrap_or(path.parts[0].span))
        {
            return value;
        }
        let Some((parts, subscripts)) = self.local_parts(landing, &path.trailing) else {
            return self.walk_expression(expr);
        };
        let span = expr.span().unwrap_or(parts[0].span);
        let component = reference(false, span, parts);
        Expression::VarRef {
            name: rumoca_core::Reference::with_component_reference(
                component.to_var_name().as_str(),
                component,
            ),
            subscripts,
            span,
        }
    }
}

impl StatementRewriter for SplitRewriter<'_> {
    fn rewrite_component_reference(&mut self, comp: &ComponentReference) -> ComponentReference {
        let landed = self
            .land_path(comp.parts())
            .and_then(|landing| self.local_parts(landing, &[]));
        let Some((mut parts, subs)) = landed else {
            let parts = comp
                .parts()
                .iter()
                .map(|part| self.rewrite_component_ref_part(part))
                .collect();
            return reference(comp.local(), comp.span(), parts);
        };
        if let Some(last) = parts.last_mut() {
            last.subs.extend(subs);
        }
        reference(false, comp.span(), parts)
    }
}

/// A whole read of a split record node, reassembled through its constructor
/// from the current values of its field locals (MLS §12.6). Admission splits
/// a node read whole only when [`SplitField::reconstructable`] holds, so every
/// node below it has a constructor and no array axes.
fn reassembled(node: SplitNode<'_>, span: Span) -> Option<Expression> {
    let args = node
        .fields()
        .iter()
        .map(|field| match field.fields {
            Some(_) => reassembled(SplitNode::Field(field), span),
            None => Some(local_reference(
                &field.local,
                field.def_id,
                Vec::new(),
                span,
            )),
        })
        .collect::<Option<Vec<_>>>()?;
    Some(Expression::FunctionCall {
        name: node.constructor()?.clone(),
        args,
        is_constructor: true,
        span,
    })
}
