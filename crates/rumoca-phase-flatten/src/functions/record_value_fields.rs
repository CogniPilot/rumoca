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
//! record value is observed through its fields, or reassembled from them
//! where it is read whole, so the rewrite preserves every value the function
//! computes.
//!
//! The rewrite is recursive over record nesting. A record-typed field (or an
//! array-of-records field) written below its own level is split in turn,
//! unless it is read whole and cannot be reassembled: `r.f.g` becomes
//! `r__f__g`, and a field of the elements of an array of records becomes one
//! column over the element extents (`r.f[i].g` is `r__f__g[i]`, struct of
//! arrays). A whole write of a split field becomes one
//! write per field of its value, projected in place when the value has a
//! field-wise projection (a record constructor, a component reference, or an
//! array or comprehension of those) that reads no path of the record, and
//! otherwise held once in the field's
//! record-valued local, so a field assigned whole and then updated field by
//! field inside a branch is an ordinary sequence of local writes. Any other
//! record-typed field becomes one record-valued local (`r.f.g` becomes
//! `r__f.g`).
//!
//! A whole write of the record itself is expanded the same way, a value with
//! no field-wise projection being held once in a fresh record-valued local.
//! A whole read of a split record, or of a split field with no enclosing
//! array, is reassembled by its record constructor from the current field
//! locals (MLS §12.6), so a record a function builds from a call, updates
//! inside branches and then returns or passes on is split too. A record whose
//! function returns early, or whose whole reads cannot be reassembled, keeps
//! its source form and the existing record-assembly rules.

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
    /// The record value's declaration.
    param: rumoca_core::FunctionParam,
    /// The record constructor that reassembles a whole read of the value,
    /// present whenever the body reads the value whole.
    constructor: Option<rumoca_core::Reference>,
    /// The record-valued local a whole write of a value with no field-wise
    /// projection is held in; no other function value or split local uses
    /// the name.
    holder: String,
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
    /// The record constructor of a split record-typed field, which
    /// reassembles a whole read of it.
    constructor: Option<rumoca_core::Reference>,
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

    /// The record local holding one whole element of an array of records.
    fn element_local(&self) -> String {
        format!("{}__element", self.local)
    }

    /// The declaration of [`Self::element_local`]: one record of the array.
    fn element_param(&self) -> rumoca_core::FunctionParam {
        let mut param = record_field_column_param(self.element_local(), &[], &[], &self.param);
        param.effective_type = rumoca_core::EffectiveType::new(
            self.param.effective_type.nominal_type(),
            self.param.effective_type.canonical_type(),
            Vec::new(),
        )
        .expect("a record element keeps the declared type contract");
        param.shape_expr = Vec::new();
        param
    }

    /// Whether a whole read of this node can be reassembled: a field held
    /// in one local, or one split record value (no enclosing or own array
    /// axes) with a constructor whose split fields are reassembled in turn.
    fn reconstructable(&self) -> bool {
        self.fields.as_deref().is_none_or(|fields| {
            self.enclosing.is_empty()
                && self.rank() == 0
                && self.constructor.is_some()
                && fields.iter().all(Self::reconstructable)
        })
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

    fn constructor(self) -> Option<&'tree rumoca_core::Reference> {
        match self {
            Self::Root(record) => record.constructor.as_ref(),
            Self::Field(field) => field.constructor.as_ref(),
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
            // A whole write defines every field; a whole read reassembles the
            // value from its field locals through the constructor.
            (root.written_whole || assigns_every_field(&function.body, &name, &constructor.inputs))
                .then_some(())?;
            let mut record = SplitRecord {
                def_id: value.def_id.filter(|id| id.index() != 0)?,
                fields: split_fields(
                    &paths,
                    constructors,
                    &path,
                    &name,
                    (&[], &[]),
                    &constructor.inputs,
                    root.read_whole,
                )?,
                param: value.clone(),
                constructor: constructor_reference(constructor),
                holder: String::new(),
                name,
                is_output,
            };
            let mut mixed = false;
            collect_nodes(&record.fields, &mut |field| {
                mixed |= field.written_whole_and_by_field
            });
            (mixed
                || paths.overwrites_below(&record.name)
                || assigns_field_in_nested_statement(&function.body, &record.name, false))
            .then_some(())?;
            (!field_locals_collide(function, &record)
                && (!root.read_whole
                    || record.constructor.is_some()
                        && record.fields.iter().all(SplitField::reconstructable)))
            .then_some(())?;
            record.holder = fresh_holder_name(function, &record);
            Some(record)
        })
        .collect()
}

/// A resolved reference to a record constructor, for a call that reassembles
/// a split record value.
fn constructor_reference(constructor: &rumoca_core::Function) -> Option<rumoca_core::Reference> {
    Some(
        rumoca_core::Reference::from_var_name(constructor.name.clone()).with_resolved_function(
            rumoca_core::ResolvedFunctionReference {
                instance_id: constructor.instance_id?,
                base_part_count: 0,
                transitively_non_replaceable: constructor.transitively_non_replaceable,
            },
        ),
    )
}

/// `<record>__whole`, suffixed by the first ordinal that names no function
/// value and no split local.
fn fresh_holder_name(function: &rumoca_core::Function, record: &SplitRecord) -> String {
    let mut taken = function
        .inputs
        .iter()
        .chain(&function.outputs)
        .chain(&function.locals)
        .map(|value| value.name.as_str())
        .collect::<HashSet<_>>();
    let mut elements = Vec::new();
    collect_nodes(&record.fields, &mut |field| {
        taken.insert(field.local.as_str());
        elements.push(field.element_local());
    });
    taken.extend(elements.iter().map(String::as_str));
    let base = format!("{}__whole", record.name);
    std::iter::once(base.clone())
        .chain((1..).map(|ordinal| format!("{base}{ordinal}")))
        .find(|name| !taken.contains(name.as_str()))
        .unwrap_or(base)
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
    read_whole_above: bool,
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
            // A node read whole, or below a node read whole, splits only when
            // its constructor can reassemble the read from the field locals;
            // otherwise it stays one local while the nodes around it split.
            let nested = (field.type_class == Some(rumoca_core::ClassType::Record))
                .then(|| record_constructor(constructors, field))
                .flatten()
                .filter(|_| uses.written_below)
                .map(|constructor| {
                    let fields = split_fields(
                        paths,
                        constructors,
                        &field_path,
                        &local,
                        (&field_enclosing, &field_enclosing_shape),
                        &constructor.inputs,
                        read_whole_above || uses.read_whole,
                    );
                    (fields, constructor_reference(constructor))
                });
            let (fields, constructor) = match nested {
                Some((fields, reference)) => (Some(fields?), reference),
                None => (None, None),
            };
            let reconstructable = |fields: &[SplitField]| {
                constructor.is_some() && fields.iter().all(SplitField::reconstructable)
            };
            let (fields, constructor) = match fields {
                Some(fields)
                    if (read_whole_above || uses.read_whole) && !reconstructable(&fields) =>
                {
                    (None, None)
                }
                fields => (fields, constructor),
            };
            Some(SplitField {
                param: field.clone(),
                def_id: field.def_id?,
                local,
                enclosing: enclosing.0.to_vec(),
                enclosing_shape: enclosing.1.to_vec(),
                written_whole_and_by_field: fields.is_some() && uses.written_whole,
                fields,
                constructor,
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
        locals.push(field.local.clone());
        locals.push(field.element_local());
    });
    let distinct = locals.iter().collect::<HashSet<_>>().len() == locals.len();
    !distinct
        || locals.iter().any(|local| {
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
        root_held: false,
    };
    let expanded = expander.expand(&function.body)?;
    let root_holder = expander
        .root_held
        .then(|| record_field_column_param(record.holder.clone(), &[], &[], &record.param));
    let value_locals = root_holder
        .into_iter()
        .chain(expander.value_locals.iter().map(|(node, element)| {
            if *element {
                node.element_param()
            } else {
                node.local_param()
            }
        }))
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
