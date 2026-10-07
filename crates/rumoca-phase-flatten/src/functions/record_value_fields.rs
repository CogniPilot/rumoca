//! Field values of a record that a function assigns field by field inside
//! its control flow.
//!
//! MLS §12.2 lets a function assign the fields of a record result or record
//! local one at a time, also inside `if` and `for` statements, as the IF97
//! `waterBaseProp_*` functions of `Modelica.Media.Water` do for their
//! auxiliary record. The checked DAE assembles a record value from one
//! straight-line group of field assignments, so a record whose fields are
//! assigned inside a nested statement is rewritten here into function locals:
//! every field write and read names a local, and a record result is assembled
//! from its locals, in declaration order, at the end of the algorithm. The
//! record value is only ever observed through its fields, so the rewrite
//! preserves every value the function computes.
//!
//! The rewrite is recursive over record nesting. A record-typed field (or an
//! array-of-records field) that is never read whole, and whose whole writes
//! all assign a value with a field-wise projection (a record constructor, a
//! component reference, or an array or comprehension of those), is split in
//! turn: `r.f.g` becomes `r__f__g`, and a field of the elements of an array of
//! records becomes one column over the element extents (`r.f[i].g` is
//! `r__f__g[i]`, struct of arrays). A whole write of a split field becomes one
//! write per field of its projected value, so a field assigned whole and then
//! updated field by field inside a branch is an ordinary sequence of local
//! writes. Any other record-typed field becomes one record-valued local
//! (`r.f.g` becomes `r__f.g`).
//!
//! A record that is read or written whole, or whose function returns early,
//! keeps its source form and the existing record-assembly rules.

mod paths;

use super::*;
use crate::function_lowering::{expand_record_arg, record_field_column_param};
use paths::{BodyPaths, SplitRewriter, WholeWriteExpander};
use rumoca_core::{ComponentRefPart, ComponentReference, Expression, Span, Statement};

/// Rewrite every function whose record values are assigned field by field
/// inside nested statements.
pub(crate) fn split_branch_assigned_records(flat: &mut flat::Model) -> Result<(), FlattenError> {
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
            split_record(function, &record)?;
        }
    }
    Ok(())
}

/// One record value of a function and the locals that replace its fields.
struct SplitRecord {
    name: String,
    def_id: rumoca_core::DefId,
    is_output: bool,
    fields: Vec<SplitField>,
}

/// One field of a split record value.
struct SplitField {
    /// The constructor field this node stands for.
    param: rumoca_core::FunctionParam,
    /// Exact identity of the constructor field.
    def_id: rumoca_core::DefId,
    /// The local holding this field, or the name prefix of its own fields'
    /// locals when the field is split further.
    local: String,
    /// Extents of the enclosing arrays of records (struct of arrays).
    enclosing: Vec<i64>,
    enclosing_shape: Vec<rumoca_core::Subscript>,
    /// The fields of a split record-typed field, `None` for a field held in
    /// one local.
    fields: Option<Vec<SplitField>>,
    /// A split field the body writes both whole and through its fields.
    written_whole_and_by_field: bool,
}

impl SplitField {
    /// The function local declaration of a field held in one local.
    fn local_param(&self) -> rumoca_core::FunctionParam {
        record_field_column_param(
            self.local.clone(),
            &self.enclosing,
            &self.enclosing_shape,
            &self.param,
        )
    }

    /// The array rank this field adds to the columns below it.
    fn rank(&self) -> usize {
        self.param.dimensions().len()
    }
}

/// One record-typed node of the split tree: the root value or a split field.
#[derive(Clone, Copy)]
enum SplitNode<'tree> {
    Root(&'tree SplitRecord),
    Field(&'tree SplitField),
}

impl<'tree> SplitNode<'tree> {
    fn fields(self) -> &'tree [SplitField] {
        match self {
            Self::Root(record) => &record.fields,
            Self::Field(field) => field.fields.as_deref().unwrap_or_default(),
        }
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
    let candidates = outputs
        .chain(locals)
        .filter(|(value, _)| {
            value.type_class == Some(rumoca_core::ClassType::Record)
                && value.dimensions().is_empty()
                && value.default.is_none()
        })
        .collect::<Vec<_>>();
    if candidates.is_empty() {
        return Vec::new();
    }
    let paths = BodyPaths::collect(&function.body);
    candidates
        .into_iter()
        .filter_map(|(value, is_output)| {
            let constructor = record_constructor(constructors, value)?;
            let name = value.name.clone();
            let path = vec![name.clone()];
            let root = paths.uses(&path);
            (assigns_every_field(&function.body, &name, &constructor.inputs)
                && !root.read_whole
                && !root.written_whole)
                .then_some(())?;
            let record = SplitRecord {
                def_id: value.def_id.filter(|id| id.index() != 0)?,
                fields: split_fields(
                    &paths,
                    constructors,
                    &path,
                    &name,
                    (&[], &[]),
                    &constructor.inputs,
                )?,
                name,
                is_output,
            };
            let mut mixed = false;
            collect_nodes(&record.fields, &mut |field| {
                mixed |= field.written_whole_and_by_field
            });
            ((mixed || assigns_field_in_nested_statement(&function.body, &record.name, false))
                && !field_locals_collide(function, &record))
            .then_some(record)
        })
        .collect()
}

/// The constructor of a record value's type, when every field carries an
/// exact identity.
fn record_constructor<'scope>(
    constructors: &'scope [rumoca_core::Function],
    value: &rumoca_core::FunctionParam,
) -> Option<&'scope rumoca_core::Function> {
    let constructor =
        rumoca_core::resolve_record_constructor(constructors, &value.type_name, value.type_def_id?)
            .ok()?;
    constructor
        .inputs
        .iter()
        .all(|field| field.def_id.is_some_and(|id| id.index() != 0))
        .then_some(constructor)
}

/// The split tree of the fields of the record at `path`, whose field locals
/// are named `<prefix>__<field>` and whose enclosing arrays of records have
/// extents `enclosing`. `None` when a field has no exact identity.
fn split_fields(
    paths: &BodyPaths,
    constructors: &[rumoca_core::Function],
    path: &[String],
    prefix: &str,
    enclosing: (&[i64], &[rumoca_core::Subscript]),
    fields: &[rumoca_core::FunctionParam],
) -> Option<Vec<SplitField>> {
    fields
        .iter()
        .map(|field| {
            let local = format!("{prefix}__{}", field.name);
            let mut field_path = path.to_vec();
            field_path.push(field.name.clone());
            let field_enclosing = enclosing
                .0
                .iter()
                .chain(field.dimensions())
                .copied()
                .collect::<Vec<_>>();
            let field_enclosing_shape = enclosing
                .1
                .iter()
                .chain(&field.shape_expr)
                .cloned()
                .collect::<Vec<_>>();
            let uses = paths.uses(&field_path);
            let nested = (field.type_class == Some(rumoca_core::ClassType::Record))
                .then(|| record_constructor(constructors, field))
                .flatten()
                .filter(|_| !uses.read_whole && uses.written_below)
                .map(|constructor| {
                    split_fields(
                        paths,
                        constructors,
                        &field_path,
                        &local,
                        (&field_enclosing, &field_enclosing_shape),
                        &constructor.inputs,
                    )
                });
            let fields = match nested {
                Some(fields) => Some(fields?),
                None => None,
            };
            Some(SplitField {
                param: field.clone(),
                def_id: field.def_id?,
                local,
                enclosing: enclosing.0.to_vec(),
                enclosing_shape: enclosing.1.to_vec(),
                written_whole_and_by_field: fields.is_some() && uses.written_whole,
                fields,
            })
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

/// Whether `comp` names a field of `record`, or a path inside one.
fn is_field_of(comp: &ComponentReference, record: &str) -> bool {
    matches!(comp.parts(), [root, _field, ..] if root.ident == record && root.subs.is_empty())
}

/// Whether a local the split may declare (a field local, or the record-valued
/// local of a split field) already names a function value.
fn field_locals_collide(function: &rumoca_core::Function, record: &SplitRecord) -> bool {
    let mut locals = Vec::new();
    collect_nodes(&record.fields, &mut |field| {
        locals.push(field.local.as_str())
    });
    locals.iter().any(|local| {
        function
            .inputs
            .iter()
            .chain(&function.outputs)
            .chain(&function.locals)
            .any(|value| value.name == *local)
    })
}

/// Visit every field of the split tree, depth first in declaration order.
fn collect_nodes<'tree>(fields: &'tree [SplitField], visit: &mut impl FnMut(&'tree SplitField)) {
    for field in fields {
        visit(field);
        if let Some(nested) = &field.fields {
            collect_nodes(nested, visit);
        }
    }
}

fn split_record(
    function: &mut rumoca_core::Function,
    record: &SplitRecord,
) -> Result<(), FlattenError> {
    let aggregate_values = function
        .outputs
        .iter()
        .chain(&function.locals)
        .filter(|value| value.type_class == Some(rumoca_core::ClassType::Record))
        .map(|value| value.name.clone())
        .collect::<HashSet<_>>();
    let mut expander = WholeWriteExpander {
        record,
        function_name: function.name.as_str(),
        aggregate_values: &aggregate_values,
        value_locals: Vec::new(),
    };
    let expanded = expander.expand(&function.body)?;
    let value_locals = expander
        .value_locals
        .iter()
        .map(|node| node.local_param())
        .collect::<Vec<_>>();
    let mut body = SplitRewriter { record }.rewrite_statements(&expanded);
    let mut locals = Vec::new();
    collect_nodes(&record.fields, &mut |field| {
        if field.fields.is_none() {
            locals.push(field);
        }
    });
    function.locals.extend(value_locals);
    function
        .locals
        .extend(locals.iter().map(|field| field.local_param()));
    if record.is_output {
        let mut path = Vec::new();
        assemble_fields(record, &record.fields, &mut path, &mut body);
    } else {
        function.locals.retain(|value| value.name != record.name);
    }
    function.body = body;
    Ok(())
}

/// `record.path := local`, one row of the trailing assembly group per field
/// held in one local, in declaration order.
fn assemble_fields<'tree>(
    record: &SplitRecord,
    fields: &'tree [SplitField],
    path: &mut Vec<&'tree SplitField>,
    body: &mut Vec<Statement>,
) {
    for field in fields {
        path.push(field);
        match &field.fields {
            Some(nested) => assemble_fields(record, nested, path, body),
            None => body.push(assemble_field(record, field, path)),
        }
        path.pop();
    }
}

fn assemble_field(record: &SplitRecord, field: &SplitField, path: &[&SplitField]) -> Statement {
    let span = field.param.span;
    let mut parts = vec![part(&record.name, record.def_id, Vec::new(), span)];
    parts.extend(
        path.iter()
            .map(|step| part(&step.param.name, step.def_id, Vec::new(), span)),
    );
    Statement::Assignment {
        comp: reference(false, span, parts),
        value: local_reference(&field.local, field.def_id, Vec::new(), span),
        span,
    }
}

fn part(
    ident: &str,
    def_id: rumoca_core::DefId,
    subs: Vec<rumoca_core::Subscript>,
    span: Span,
) -> ComponentRefPart {
    ComponentRefPart {
        ident: ident.to_string(),
        span,
        subs,
        def_id,
    }
}

/// A read of one field local.
fn local_reference(
    local: &str,
    def_id: rumoca_core::DefId,
    subscripts: Vec<rumoca_core::Subscript>,
    span: Span,
) -> Expression {
    let component = reference(false, span, vec![part(local, def_id, Vec::new(), span)]);
    Expression::VarRef {
        name: rumoca_core::Reference::with_component_reference(local, component),
        subscripts,
        span,
    }
}

/// A reference over parts whose identities the split already validated.
fn reference(local: bool, span: Span, parts: Vec<ComponentRefPart>) -> ComponentReference {
    ComponentReference::construct(local, span, parts)
        .expect("split record references carry nonzero part identities")
}

/// Whether the body writes every field of record `record` somewhere, through
/// the field. A field no statement writes
/// has no value at all, and the record keeps its source form so the
/// record-assembly rules report that field.
fn assigns_every_field(
    statements: &[Statement],
    record: &str,
    fields: &[rumoca_core::FunctionParam],
) -> bool {
    fields
        .iter()
        .all(|field| writes_field(statements, record, &field.name))
}

fn writes_field(statements: &[Statement], record: &str, field: &str) -> bool {
    let writes = |comp: &ComponentReference| matches!(comp.parts(), [root, part, ..] if root.ident == record && part.ident == field);
    statements.iter().any(|statement| match statement {
        Statement::Assignment { comp, .. } => writes(comp),
        Statement::FunctionCall { outputs, .. } => outputs.iter().flatten().any(writes),
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
    })
}
