use super::*;
use indexmap::{IndexMap, IndexSet};
use std::collections::HashSet;

type ConstructorFieldGroupKey = (rumoca_core::VarName, String);

/// Drop FieldAccess bindings whose targets don't exist in the flat model.
/// During modifier propagation, record bindings like `x = someRecord.field` may reference
/// internal component structure that was eliminated during flattening. These dangling
/// FieldAccess bindings would cause incorrect equation generation in todae if kept.
pub(crate) fn drop_invalid_field_access_bindings(flat: &mut flat::Model) {
    let all_names: HashSet<rumoca_core::VarName> = flat.variables.keys().cloned().collect();

    let to_clear: Vec<rumoca_core::VarName> = flat
        .variables
        .iter()
        .filter_map(|(name, var)| {
            let binding = var.binding.as_ref()?;
            let target_name = field_access_target_name(binding)?;
            if all_names.contains(&target_name)
                || !field_access_targets_flat_namespace(binding, &all_names)
            {
                None
            } else {
                Some(name.clone())
            }
        })
        .collect();

    for name in &to_clear {
        if let Some(var) = flat.variables.get_mut(name) {
            var.binding = None;
        }
    }
}

pub(crate) fn resolve_nested_constructor_field_access_bindings(flat: &mut flat::Model) {
    let groups = collect_constructor_field_groups(flat);
    let rewrites = flat
        .variables
        .iter()
        .filter_map(|(name, var)| {
            let binding = var.binding.as_ref()?;
            let access = direct_constructor_field_access(binding, flat)?;
            let prefix = variable_field_prefix(name)?;
            let group = groups.get(&(access.constructor_name.clone(), prefix))?;
            let outer_field = unique_nested_record_field(flat, access.constructor, group)?;
            Some((
                name.clone(),
                nested_constructor_field_access(binding, outer_field),
            ))
        })
        .collect::<Vec<_>>();

    for (name, binding) in rewrites {
        if let Some(var) = flat.variables.get_mut(&name) {
            var.binding = Some(binding);
        }
    }
}

struct DirectConstructorFieldAccess<'a> {
    constructor_name: rumoca_core::VarName,
    constructor: &'a rumoca_core::Function,
    field: &'a str,
}

fn collect_constructor_field_groups(
    flat: &flat::Model,
) -> IndexMap<ConstructorFieldGroupKey, IndexSet<String>> {
    let mut groups = IndexMap::new();
    for (name, var) in &flat.variables {
        let Some(binding) = var.binding.as_ref() else {
            continue;
        };
        let Some(access) = direct_constructor_field_access(binding, flat) else {
            continue;
        };
        let Some(prefix) = variable_field_prefix(name) else {
            continue;
        };
        groups
            .entry((access.constructor_name, prefix))
            .or_insert_with(IndexSet::new)
            .insert(access.field.to_string());
    }
    groups
}

fn direct_constructor_field_access<'a>(
    expr: &'a rumoca_core::Expression,
    flat: &'a flat::Model,
) -> Option<DirectConstructorFieldAccess<'a>> {
    let rumoca_core::Expression::FieldAccess { base, field, .. } = expr else {
        return None;
    };
    let rumoca_core::Expression::FunctionCall {
        name,
        is_constructor,
        ..
    } = base.as_ref()
    else {
        return None;
    };
    let constructor = flat.functions.get(name.var_name())?;
    if !(*is_constructor || constructor.is_constructor) {
        return None;
    }
    if constructor_has_field(constructor, field) {
        return None;
    }
    Some(DirectConstructorFieldAccess {
        constructor_name: name.var_name().clone(),
        constructor,
        field,
    })
}

fn constructor_has_field(constructor: &rumoca_core::Function, field: &str) -> bool {
    constructor.inputs.iter().any(|param| param.name == field)
        || constructor.outputs.iter().any(|param| param.name == field)
}

fn variable_field_prefix(name: &rumoca_core::VarName) -> Option<String> {
    name.enclosing_scope().map(str::to_string)
}

fn unique_nested_record_field(
    flat: &flat::Model,
    constructor: &rumoca_core::Function,
    selected_fields: &IndexSet<String>,
) -> Option<String> {
    let mut matches = constructor
        .inputs
        .iter()
        .filter(|param| param.type_class == Some(rumoca_core::ClassType::Record))
        .filter(|param| record_type_contains_fields(flat, &param.type_name, selected_fields))
        .map(|param| param.name.clone());
    let first = matches.next()?;
    matches.next().is_none().then_some(first)
}

fn record_type_contains_fields(
    flat: &flat::Model,
    type_name: &str,
    selected_fields: &IndexSet<String>,
) -> bool {
    let Some(fields) = record_constructor_field_names(flat, type_name) else {
        return false;
    };
    selected_fields
        .iter()
        .all(|field| fields.contains(field.as_str()))
}

fn record_constructor_field_names(flat: &flat::Model, type_name: &str) -> Option<IndexSet<String>> {
    flat.functions
        .iter()
        .find(|(name, function)| {
            function.is_constructor
                && rumoca_core::qualified_type_name_matches(name.as_str(), type_name)
        })
        .map(|(_, function)| {
            function
                .inputs
                .iter()
                .map(|field| field.name.clone())
                .collect()
        })
}

fn nested_constructor_field_access(
    expr: &rumoca_core::Expression,
    outer_field: String,
) -> rumoca_core::Expression {
    let rumoca_core::Expression::FieldAccess { base, field, span } = expr else {
        return expr.clone();
    };
    rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::FieldAccess {
            base: base.clone(),
            field: outer_field,
            span: *span,
        }),
        field: field.clone(),
        span: *span,
    }
}

fn field_access_target_name(expr: &rumoca_core::Expression) -> Option<rumoca_core::VarName> {
    let rumoca_core::Expression::FieldAccess { .. } = expr else {
        return None;
    };
    rendered_component_target(expr).map(rumoca_core::VarName::new)
}

fn rendered_component_target(expr: &rumoca_core::Expression) -> Option<String> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            let mut rendered = name.as_str().to_string();
            append_flat_subscripts(&mut rendered, subscripts)?;
            Some(rendered)
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            let mut rendered = rendered_component_target(base)?;
            append_flat_subscripts(&mut rendered, subscripts)?;
            Some(rendered)
        }
        rumoca_core::Expression::FieldAccess { base, field, .. } => {
            Some(format!("{}.{}", rendered_component_target(base)?, field))
        }
        _ => None,
    }
}

fn field_access_targets_flat_namespace(
    expr: &rumoca_core::Expression,
    all_names: &HashSet<rumoca_core::VarName>,
) -> bool {
    field_access_base_target(expr)
        .into_iter()
        .chain(leftmost_reference_target(expr))
        .any(|target| flat_namespace_contains(all_names, &target))
}

fn field_access_base_target(expr: &rumoca_core::Expression) -> Option<String> {
    let rumoca_core::Expression::FieldAccess { base, .. } = expr else {
        return None;
    };
    rendered_component_target(base)
}

fn leftmost_reference_target(expr: &rumoca_core::Expression) -> Option<String> {
    match expr {
        rumoca_core::Expression::VarRef { name, .. } => Some(name.as_str().to_string()),
        rumoca_core::Expression::Index { base, .. }
        | rumoca_core::Expression::FieldAccess { base, .. } => leftmost_reference_target(base),
        _ => None,
    }
}

fn flat_namespace_contains(all_names: &HashSet<rumoca_core::VarName>, target: &str) -> bool {
    let exact = rumoca_core::VarName::new(target);
    all_names.contains(&exact)
        || all_names.iter().any(|name| {
            let name = name.as_str();
            name.strip_prefix(target)
                .is_some_and(|suffix| suffix.starts_with('.') || suffix.starts_with('['))
        })
}

fn append_flat_subscripts(
    rendered: &mut String,
    subscripts: &[rumoca_core::Subscript],
) -> Option<()> {
    for subscript in subscripts {
        match subscript {
            rumoca_core::Subscript::Index { value, .. } => {
                rendered.push('[');
                rendered.push_str(&value.to_string());
                rendered.push(']');
            }
            rumoca_core::Subscript::Expr { expr, .. } => {
                let value = constant_integer_bound(expr)?;
                rendered.push('[');
                rendered.push_str(&value.to_string());
                rendered.push(']');
            }
            rumoca_core::Subscript::Colon { .. } => return None,
        }
    }
    Some(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_span() -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("postprocess_field_access_test.mo"),
            1,
            2,
        )
    }

    fn constructor(name: &str, inputs: Vec<rumoca_core::FunctionParam>) -> rumoca_core::Function {
        let mut function = rumoca_core::Function::new(name, rumoca_core::Span::DUMMY);
        function.is_constructor = true;
        for input in inputs {
            function.add_input(input);
        }
        function
    }

    fn record_param(name: &str, type_name: &str) -> rumoca_core::FunctionParam {
        rumoca_core::FunctionParam::new(name, type_name, test_span())
            .with_type_class(rumoca_core::ClassType::Record)
    }

    fn direct_constructor_field(constructor: &str, field: &str) -> rumoca_core::Expression {
        rumoca_core::Expression::FieldAccess {
            base: Box::new(rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new(constructor),
                args: Vec::new(),
                is_constructor: true,
                span: rumoca_core::Span::DUMMY,
            }),
            field: field.to_string(),
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn variable(name: &str, binding: rumoca_core::Expression) -> flat::Variable {
        flat::Variable {
            name: rumoca_core::VarName::new(name),
            binding: Some(binding),
            ..flat::Variable::empty_with_span(test_span())
        }
    }

    #[test]
    fn resolves_direct_constructor_fields_through_unique_nested_record() {
        let mut flat = flat::Model::new();
        flat.add_function(constructor(
            "Pkg.Outer",
            vec![record_param("inner", "Pkg.Inner")],
        ));
        flat.add_function(constructor(
            "Pkg.Inner",
            vec![
                rumoca_core::FunctionParam::new("x", "Real", test_span()),
                rumoca_core::FunctionParam::new("y", "Real", test_span()),
            ],
        ));
        flat.add_variable(
            rumoca_core::VarName::new("target.record.x"),
            variable(
                "target.record.x",
                direct_constructor_field("Pkg.Outer", "x"),
            ),
        );
        flat.add_variable(
            rumoca_core::VarName::new("target.record.y"),
            variable(
                "target.record.y",
                direct_constructor_field("Pkg.Outer", "y"),
            ),
        );

        resolve_nested_constructor_field_access_bindings(&mut flat);

        let Some(rumoca_core::Expression::FieldAccess { base, field, .. }) = flat
            .variables
            .get(&rumoca_core::VarName::new("target.record.x"))
            .and_then(|var| var.binding.as_ref())
        else {
            panic!("expected projected field access");
        };
        assert_eq!(field, "x");
        let rumoca_core::Expression::FieldAccess {
            field: outer_field, ..
        } = base.as_ref()
        else {
            panic!("expected nested constructor field access");
        };
        assert_eq!(outer_field, "inner");
    }
}
