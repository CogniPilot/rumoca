//! Field values of a record that a function assigns field by field inside
//! its control flow.
//!
//! MLS §12.2 lets a function assign the fields of a record result or record
//! local one at a time, also inside `if` and `for` statements, as the IF97
//! `waterBaseProp_*` functions of `Modelica.Media.Water` do for their
//! auxiliary record. The checked DAE assembles a record value from one
//! straight-line group of field assignments, so a record whose fields are
//! assigned inside a nested statement is rewritten here into one function
//! local per field: every field write and read names the field local, and a
//! record result is assembled from its field locals, in declaration order, at
//! the end of the algorithm. The record value is only ever observed through
//! its fields, so the rewrite preserves every value the function computes.
//!
//! A record that is read or written whole, whose fields are themselves
//! records, or whose function returns early keeps its source form and the
//! existing record-assembly rules.

use super::*;
use rumoca_core::{ComponentRefPart, ComponentReference, Expression, Span, Statement};

/// Rewrite every function whose record values are assigned field by field
/// inside nested statements.
pub(crate) fn split_branch_assigned_records(flat: &mut flat::Model) {
    let constructors = flat
        .functions
        .values()
        .filter(|function| function.is_constructor)
        .cloned()
        .collect::<Vec<_>>();
    for function in flat.functions.values_mut() {
        if function.is_constructor || function.external.is_some() {
            continue;
        }
        for record in branch_assigned_records(function, &constructors) {
            split_record(function, &record);
        }
    }
}

/// One record value of a function and the field parameters of its type.
struct SplitRecord {
    name: String,
    def_id: rumoca_core::DefId,
    is_output: bool,
    fields: Vec<rumoca_core::FunctionParam>,
}

impl SplitRecord {
    fn field_local(&self, field: &str) -> String {
        format!("{}__{}", self.name, field)
    }
}

fn branch_assigned_records(
    function: &rumoca_core::Function,
    constructors: &[rumoca_core::Function],
) -> Vec<SplitRecord> {
    if contains_return(&function.body) {
        return Vec::new();
    }
    let outputs = function.outputs.iter().map(|value| (value, true));
    let locals = function.locals.iter().map(|value| (value, false));
    outputs
        .chain(locals)
        .filter(|(value, _)| {
            value.type_class == Some(rumoca_core::ClassType::Record)
                && value.dimensions().is_empty()
                && value.default.is_none()
        })
        .filter_map(|(value, is_output)| {
            let constructor = rumoca_core::resolve_record_constructor(
                constructors,
                &value.type_name,
                value.type_def_id?,
            )
            .ok()?;
            let scalar_fields = constructor
                .inputs
                .iter()
                .all(|field| field.type_class != Some(rumoca_core::ClassType::Record));
            let identified = value.def_id.is_some_and(|id| id.index() != 0)
                && constructor
                    .inputs
                    .iter()
                    .all(|field| field.def_id.is_some_and(|id| id.index() != 0));
            let record = SplitRecord {
                name: value.name.clone(),
                def_id: value.def_id?,
                is_output,
                fields: constructor.inputs.clone(),
            };
            (identified
                && scalar_fields
                && assigns_field_in_nested_statement(&function.body, &record.name, false)
                && assigns_every_field(&function.body, &record)
                && !reads_or_writes_whole(&function.body, &record)
                && !field_locals_collide(function, &record))
            .then_some(record)
        })
        .collect()
}

fn contains_return(statements: &[Statement]) -> bool {
    statements.iter().any(|statement| match statement {
        Statement::Return { .. } => true,
        Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            cond_blocks
                .iter()
                .any(|block| contains_return(&block.stmts))
                || else_block.as_deref().is_some_and(contains_return)
        }
        Statement::For { equations, .. } => contains_return(equations),
        Statement::While { block, .. } => contains_return(&block.stmts),
        _ => false,
    })
}

/// Whether a field of `record` is assigned by a statement nested in a
/// conditional or loop.
fn assigns_field_in_nested_statement(statements: &[Statement], record: &str, nested: bool) -> bool {
    statements.iter().any(|statement| match statement {
        Statement::Assignment { comp, .. } => nested && is_field_of(comp, record),
        Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            cond_blocks
                .iter()
                .any(|block| assigns_field_in_nested_statement(&block.stmts, record, true))
                || else_block
                    .as_deref()
                    .is_some_and(|block| assigns_field_in_nested_statement(block, record, true))
        }
        Statement::For { equations, .. } => {
            assigns_field_in_nested_statement(equations, record, true)
        }
        Statement::While { block, .. } => {
            assigns_field_in_nested_statement(&block.stmts, record, true)
        }
        _ => false,
    })
}

fn is_field_of(comp: &ComponentReference, record: &str) -> bool {
    matches!(comp.parts(), [root, _field] if root.ident == record && root.subs.is_empty())
}

/// Whether the body names `record` other than through one of its fields.
fn reads_or_writes_whole(statements: &[Statement], record: &SplitRecord) -> bool {
    let mut finder = WholeUseFinder {
        record,
        found: false,
    };
    let _ = finder.rewrite_statements(statements);
    finder.found
}

struct WholeUseFinder<'record> {
    record: &'record SplitRecord,
    found: bool,
}

impl ExpressionRewriter for WholeUseFinder<'_> {
    fn rewrite_expression(&mut self, expr: &Expression) -> Expression {
        match expr {
            Expression::VarRef { name, .. } if names_whole(name, &self.record.name) => {
                self.found = true;
                expr.clone()
            }
            Expression::FieldAccess { base, .. }
                if matches!(base.as_ref(), Expression::VarRef { name, .. }
                    if names_whole(name, &self.record.name)) =>
            {
                expr.clone()
            }
            _ => self.walk_expression(expr),
        }
    }
}

impl StatementRewriter for WholeUseFinder<'_> {
    fn rewrite_statement(&mut self, statement: &Statement) -> Statement {
        match statement {
            Statement::Assignment { comp, .. } if matches!(comp.parts(), [root] if root.ident == self.record.name) =>
            {
                self.found = true;
            }
            Statement::FunctionCall { outputs, .. }
                if outputs.iter().flatten().any(
                    |output| matches!(output.parts(), [root] if root.ident == self.record.name),
                ) =>
            {
                self.found = true;
            }
            _ => {}
        }
        self.walk_statement(statement)
    }
}

/// Whether a reference names the record value itself rather than a field.
fn names_whole(name: &rumoca_core::Reference, record: &str) -> bool {
    match name.component_ref() {
        Some(component) => matches!(component.parts(), [root] if root.ident == record),
        None => name.as_str() == record,
    }
}

fn field_locals_collide(function: &rumoca_core::Function, record: &SplitRecord) -> bool {
    record.fields.iter().any(|field| {
        let local = record.field_local(&field.name);
        function
            .inputs
            .iter()
            .chain(&function.outputs)
            .chain(&function.locals)
            .any(|value| value.name == local)
    })
}

fn split_record(function: &mut rumoca_core::Function, record: &SplitRecord) {
    let mut rewriter = FieldLocalRewriter { record };
    let mut body = rewriter.rewrite_statements(&function.body);
    for field in &record.fields {
        let mut local = field.clone();
        local.name = record.field_local(&field.name);
        local.default = None;
        function.locals.push(local);
    }
    if record.is_output {
        body.extend(
            record
                .fields
                .iter()
                .map(|field| assemble_field(record, field)),
        );
    } else {
        function.locals.retain(|value| value.name != record.name);
    }
    function.body = body;
}

/// `record.field := record__field`, one row of the trailing assembly group.
fn assemble_field(record: &SplitRecord, field: &rumoca_core::FunctionParam) -> Statement {
    let span = field.span;
    let field_def_id = field.def_id.expect("split record fields carry identities");
    let target = reference(
        false,
        span,
        vec![
            ComponentRefPart {
                ident: record.name.clone(),
                span,
                subs: Vec::new(),
                def_id: record.def_id,
            },
            ComponentRefPart {
                ident: field.name.clone(),
                span,
                subs: Vec::new(),
                def_id: field_def_id,
            },
        ],
    );
    Statement::Assignment {
        comp: target,
        value: field_local_reference(record, &field.name, field_def_id, Vec::new(), span),
        span,
    }
}

fn field_local_reference(
    record: &SplitRecord,
    field: &str,
    def_id: rumoca_core::DefId,
    subs: Vec<rumoca_core::Subscript>,
    span: Span,
) -> Expression {
    let name = record.field_local(field);
    let component = reference(
        false,
        span,
        vec![ComponentRefPart {
            ident: name.clone(),
            span,
            subs: Vec::new(),
            def_id,
        }],
    );
    Expression::VarRef {
        name: rumoca_core::Reference::with_component_reference(&name, component),
        subscripts: subs,
        span,
    }
}

struct FieldLocalRewriter<'record> {
    record: &'record SplitRecord,
}

impl FieldLocalRewriter<'_> {
    fn field_part<'part>(
        &self,
        comp: &'part ComponentReference,
    ) -> Option<&'part ComponentRefPart> {
        match comp.parts() {
            [root, field] if root.ident == self.record.name && root.subs.is_empty() => Some(field),
            _ => None,
        }
    }
}

impl ExpressionRewriter for FieldLocalRewriter<'_> {
    fn rewrite_expression(&mut self, expr: &Expression) -> Expression {
        match expr {
            Expression::VarRef {
                name,
                subscripts,
                span,
            } => {
                let field = name.component_ref().and_then(|comp| self.field_part(comp));
                match field {
                    Some(field) => {
                        let mut subs = field.subs.clone();
                        subs.extend(self.rewrite_subscripts(subscripts));
                        field_local_reference(self.record, &field.ident, field.def_id, subs, *span)
                    }
                    None => self.walk_expression(expr),
                }
            }
            Expression::FieldAccess {
                base,
                field,
                field_def_id,
                span,
            } if matches!(base.as_ref(), Expression::VarRef { name, .. }
                if names_whole(name, &self.record.name)) =>
            {
                field_local_reference(self.record, field, *field_def_id, Vec::new(), *span)
            }
            _ => self.walk_expression(expr),
        }
    }
}

impl StatementRewriter for FieldLocalRewriter<'_> {
    fn rewrite_component_reference(&mut self, comp: &ComponentReference) -> ComponentReference {
        let Some(field) = self.field_part(comp) else {
            let parts = comp
                .parts()
                .iter()
                .map(|part| self.rewrite_component_ref_part(part))
                .collect();
            return reference(comp.local(), comp.span(), parts);
        };
        reference(
            false,
            comp.span(),
            vec![ComponentRefPart {
                ident: self.record.field_local(&field.ident),
                span: field.span,
                subs: self.rewrite_subscripts(&field.subs),
                def_id: field.def_id,
            }],
        )
    }
}

/// A reference over parts whose identities the split already validated.
fn reference(local: bool, span: Span, parts: Vec<ComponentRefPart>) -> ComponentReference {
    ComponentReference::construct(local, span, parts)
        .expect("split record references carry nonzero part identities")
}

/// Whether the body writes every field of `record` somewhere. A field no
/// statement writes has no value at all, and the record keeps its source form
/// so the record-assembly rules report that field.
fn assigns_every_field(statements: &[Statement], record: &SplitRecord) -> bool {
    record
        .fields
        .iter()
        .all(|field| writes_field(statements, &record.name, &field.name))
}

fn writes_field(statements: &[Statement], record: &str, field: &str) -> bool {
    statements.iter().any(|statement| {
        match statement {
        Statement::Assignment { comp, .. } => {
            matches!(comp.parts(), [root, part] if root.ident == record && part.ident == field)
        }
        Statement::FunctionCall { outputs, .. } => outputs.iter().flatten().any(|output| {
            matches!(output.parts(), [root, part] if root.ident == record && part.ident == field)
        }),
        Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            cond_blocks
                .iter()
                .any(|block| writes_field(&block.stmts, record, field))
                || else_block
                    .as_deref()
                    .is_some_and(|block| writes_field(block, record, field))
        }
        Statement::For { equations, .. } => writes_field(equations, record, field),
        Statement::While { block, .. } => writes_field(&block.stmts, record, field),
        _ => false,
    }
    })
}
