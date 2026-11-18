//! Declaration defaults for record-valued function variables.

use rumoca_ir_flat as flat;
use std::collections::{HashMap, HashSet};

pub(super) fn seed_complete_record_defaults(flat: &mut flat::Model) {
    let fields = flat
        .functions
        .values()
        .filter(|function| function.is_constructor)
        .filter_map(|function| {
            Some((
                (function.def_id?, function.name.as_str().to_string()),
                function.inputs.clone(),
            ))
        })
        .collect::<HashMap<_, _>>();
    for function in flat
        .functions
        .values_mut()
        .filter(|function| !function.is_constructor)
    {
        seed_partial_record_defaults(function, &fields);
    }
    let constructors = flat
        .functions
        .values()
        .filter(|function| function.is_constructor)
        .filter_map(|function| {
            let identity = rumoca_core::RecordConstructorRef::of(function)?;
            function
                .inputs
                .iter()
                .all(|input| input.default.is_some())
                .then(|| ((identity.def_id, identity.name.clone()), identity))
        })
        .collect::<HashMap<_, _>>();
    for function in flat
        .functions
        .values_mut()
        .filter(|function| !function.is_constructor)
    {
        for value in function.outputs.iter_mut().chain(&mut function.locals) {
            if value.default.is_some()
                || value.type_class != Some(rumoca_core::ClassType::Record)
                || !record_writes(&function.body, &value.name).fields.is_empty()
            {
                continue;
            }
            let Some(type_def_id) = value.type_def_id else {
                continue;
            };
            let Some(constructor) = constructors.get(&(type_def_id, value.type_name.clone()))
            else {
                continue;
            };
            value.default = repeated_over_shape(
                constructor.call(Vec::new(), value.span),
                &value.shape_expr,
                value.span,
            );
        }
    }
}

/// `element` as the default of a declaration with `shape`: itself for a
/// scalar, `fill(element, n1, ...)` for an array (MLS 3.7 §10.3.3), so every
/// element of a record array starts at the record's defaults. `None` while an
/// extent is unknown (`:`).
fn repeated_over_shape(
    element: rumoca_core::Expression,
    shape: &[rumoca_core::Subscript],
    span: rumoca_core::Span,
) -> Option<rumoca_core::Expression> {
    if shape.is_empty() {
        return Some(element);
    }
    let mut args = Vec::with_capacity(shape.len() + 1);
    args.push(element);
    for extent in shape {
        args.push(match extent {
            rumoca_core::Subscript::Index { value, span } => rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(*value),
                span: *span,
            },
            rumoca_core::Subscript::Expr { expr, .. } => (**expr).clone(),
            rumoca_core::Subscript::Colon { .. } => return None,
        });
    }
    Some(rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Fill,
        args,
        span,
    })
}

/// MLS 3.7 §12.4.4: a record result or protected record is initialized by the
/// declaration equations of its type's fields before the algorithm runs. A
/// field the algorithm never writes therefore keeps its default, so it is
/// assigned that default at entry; a field without a default stays undefined
/// and the record assembly refuses it (FUNC-024). A default that reads a
/// sibling field, and a record the algorithm writes whole, are left as written.
fn seed_partial_record_defaults(
    function: &mut rumoca_core::Function,
    constructors: &HashMap<(rumoca_core::DefId, String), Vec<rumoca_core::FunctionParam>>,
) {
    let mut seeded = Vec::new();
    for value in function.outputs.iter().chain(&function.locals) {
        if value.default.is_some() || value.type_class != Some(rumoca_core::ClassType::Record) {
            continue;
        }
        let (Some(type_def_id), Some(value_def_id)) = (value.type_def_id, value.def_id) else {
            continue;
        };
        let Some(fields) = constructors.get(&(type_def_id, value.type_name.clone())) else {
            continue;
        };
        let writes = record_writes(&function.body, &value.name);
        if writes.whole || writes.fields.is_empty() {
            continue;
        }
        let field_names = fields
            .iter()
            .map(|field| field.name.as_str())
            .collect::<HashSet<_>>();
        for field in fields {
            let (Some(default), Some(field_def_id)) = (&field.default, field.def_id) else {
                continue;
            };
            if writes.fields.contains(&field.name) || reads_any(default, &field_names) {
                continue;
            }
            let part = |ident: &str, def_id| rumoca_core::ComponentRefPart {
                ident: ident.to_string(),
                span: value.span,
                subs: Vec::new(),
                def_id,
            };
            let Ok(comp) = rumoca_core::ComponentReference::construct(
                false,
                value.span,
                vec![
                    part(&value.name, value_def_id),
                    part(&field.name, field_def_id),
                ],
            ) else {
                continue;
            };
            seeded.push(rumoca_core::Statement::Assignment {
                comp,
                value: default.clone(),
                span: value.span,
            });
        }
    }
    if !seeded.is_empty() {
        seeded.append(&mut function.body);
        function.body = seeded;
    }
}

#[derive(Default)]
struct RecordWrites {
    whole: bool,
    fields: HashSet<String>,
}

/// Which parts of the record `name` any statement of `statements` writes.
fn record_writes(statements: &[rumoca_core::Statement], name: &str) -> RecordWrites {
    let mut writes = RecordWrites::default();
    collect_record_writes(statements, name, &mut writes);
    writes
}

fn collect_record_writes(
    statements: &[rumoca_core::Statement],
    name: &str,
    writes: &mut RecordWrites,
) {
    fn target(comp: &rumoca_core::ComponentReference, name: &str, writes: &mut RecordWrites) {
        match comp.parts() {
            [root] if root.ident == name => writes.whole = true,
            [root, field, ..] if root.ident == name => {
                writes.fields.insert(field.ident.clone());
            }
            _ => {}
        }
    }
    for statement in statements {
        match statement {
            rumoca_core::Statement::Assignment { comp, .. } => target(comp, name, writes),
            rumoca_core::Statement::FunctionCall { outputs, .. } => {
                for comp in outputs.iter().flatten() {
                    target(comp, name, writes);
                }
            }
            rumoca_core::Statement::For { equations, .. } => {
                collect_record_writes(equations, name, writes);
            }
            rumoca_core::Statement::While { block, .. } => {
                collect_record_writes(&block.stmts, name, writes);
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => {
                for block in cond_blocks {
                    collect_record_writes(&block.stmts, name, writes);
                }
                if let Some(block) = else_block {
                    collect_record_writes(block, name, writes);
                }
            }
            rumoca_core::Statement::When { blocks, .. } => {
                for block in blocks {
                    collect_record_writes(&block.stmts, name, writes);
                }
            }
            _ => {}
        }
    }
}

/// Whether `expression` reads any of `names` as a bare reference.
fn reads_any(expression: &rumoca_core::Expression, names: &HashSet<&str>) -> bool {
    struct Reads<'a> {
        names: &'a HashSet<&'a str>,
        found: bool,
    }
    impl rumoca_core::ExpressionVisitor for Reads<'_> {
        fn visit_var_ref(
            &mut self,
            name: &rumoca_core::Reference,
            subscripts: &[rumoca_core::Subscript],
        ) {
            let root = name.as_str().split(['.', '[']).next().unwrap_or_default();
            self.found |= self.names.contains(root);
            self.walk_var_ref(name, subscripts);
        }
    }
    let mut reads = Reads {
        names,
        found: false,
    };
    rumoca_core::ExpressionVisitor::visit_expression(&mut reads, expression);
    reads.found
}
