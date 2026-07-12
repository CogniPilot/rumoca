#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct OutputProjectionSuffix {
    pub(crate) output_name: String,
    pub(crate) output_fields: Vec<String>,
    pub(crate) indices: Vec<usize>,
}

pub(crate) fn resolve_function_reference<'a>(
    functions: &'a indexmap::IndexMap<rumoca_core::VarName, rumoca_core::Function>,
    name: &rumoca_core::Reference,
) -> Option<(&'a rumoca_core::VarName, &'a rumoca_core::Function)> {
    if let Some(resolved) = name.resolved_function() {
        return functions
            .iter()
            .find(|(_, function)| function.instance_id == Some(resolved.instance_id));
    }
    None
}

pub(crate) fn output_projection_suffix(
    function: &rumoca_core::Function,
    name: &rumoca_core::Reference,
) -> Option<OutputProjectionSuffix> {
    if let Some(resolved) = name.resolved_function() {
        (function.instance_id == Some(resolved.instance_id)).then_some(())?;
        let suffix = name
            .component_ref()?
            .parts
            .get(resolved.base_part_count..)?;
        return parse_output_projection_suffix(suffix);
    }
    None
}

fn parse_output_projection_suffix(
    suffix: &[rumoca_core::ComponentRefPart],
) -> Option<OutputProjectionSuffix> {
    if suffix.is_empty()
        || suffix.iter().any(|part| part.ident.is_empty())
        || suffix[..suffix.len() - 1]
            .iter()
            .any(|part| !part.subs.is_empty())
    {
        return None;
    }
    let indices = suffix
        .last()?
        .subs
        .iter()
        .map(|subscript| match subscript {
            rumoca_core::Subscript::Index { value, .. } => {
                usize::try_from(*value).ok().filter(|index| *index > 0)
            }
            _ => None,
        })
        .collect::<Option<Vec<_>>>()?;
    Some(OutputProjectionSuffix {
        output_name: suffix[0].ident.clone(),
        output_fields: suffix[1..].iter().map(|part| part.ident.clone()).collect(),
        indices,
    })
}

pub(crate) fn record_output_field_param<'a>(
    functions: &'a indexmap::IndexMap<rumoca_core::VarName, rumoca_core::Function>,
    output: &'a rumoca_core::FunctionParam,
    field_path: &[String],
) -> Option<&'a rumoca_core::FunctionParam> {
    if output.type_class != Some(rumoca_core::ClassType::Record) || field_path.is_empty() {
        return None;
    }
    let mut type_def_id = output.type_def_id?;
    let mut selected = None;
    for (index, field_name) in field_path.iter().enumerate() {
        let constructor = functions
            .values()
            .find(|function| function.def_id == Some(type_def_id))?;
        if !constructor.is_constructor {
            return None;
        }
        let field = constructor
            .inputs
            .iter()
            .find(|input| input.name == *field_name)?;
        selected = Some(field);
        if index + 1 < field_path.len() {
            if field.type_class != Some(rumoca_core::ClassType::Record) {
                return None;
            }
            type_def_id = field.type_def_id?;
        }
    }
    selected
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_INSTANCE_ID: rumoca_core::FunctionInstanceId = rumoca_core::FunctionInstanceId(7);

    fn projection_reference(
        suffix_parts: Vec<rumoca_core::ComponentRefPart>,
    ) -> rumoca_core::Reference {
        let span = rumoca_core::Span::DUMMY;
        let mut parts = vec![
            rumoca_core::ComponentRefPart {
                ident: "Pkg".to_string(),
                span,
                subs: Vec::new(),
            },
            rumoca_core::ComponentRefPart {
                ident: "f".to_string(),
                span,
                subs: Vec::new(),
            },
        ];
        parts.extend(suffix_parts);
        rumoca_core::Reference::from_component_reference(rumoca_core::ComponentReference {
            local: false,
            span,
            parts,
            def_id: Some(rumoca_core::DefId::new(7)),
        })
        .with_resolved_function(rumoca_core::ResolvedFunctionReference {
            instance_id: TEST_INSTANCE_ID,
            base_part_count: 2,
        })
    }

    fn function_with_instance(name: &rumoca_core::VarName) -> rumoca_core::Function {
        let mut function = rumoca_core::Function::new(name.as_str(), rumoca_core::Span::DUMMY);
        function.instance_id = Some(TEST_INSTANCE_ID);
        function
    }

    fn part(name: &str, indices: &[i64]) -> rumoca_core::ComponentRefPart {
        rumoca_core::ComponentRefPart {
            ident: name.to_string(),
            span: rumoca_core::Span::DUMMY,
            subs: indices
                .iter()
                .map(|index| rumoca_core::Subscript::index(*index, rumoca_core::Span::DUMMY))
                .collect(),
        }
    }

    #[test]
    fn reads_output_projection_from_structured_reference() {
        let function_name = rumoca_core::VarName::new("Pkg.f");
        assert_eq!(
            output_projection_suffix(
                &function_with_instance(&function_name),
                &projection_reference(vec![part("out", &[])])
            )
            .expect("plain output"),
            OutputProjectionSuffix {
                output_name: "out".to_string(),
                output_fields: vec![],
                indices: vec![],
            }
        );
        assert_eq!(
            output_projection_suffix(
                &function_with_instance(&function_name),
                &projection_reference(vec![part("out", &[]), part("re", &[2, 3])]),
            )
            .expect("fielded output"),
            OutputProjectionSuffix {
                output_name: "out".to_string(),
                output_fields: vec!["re".to_string()],
                indices: vec![2, 3],
            }
        );
        assert!(
            output_projection_suffix(
                &function_with_instance(&function_name),
                &projection_reference(vec![part("out", &[1]), part("re", &[])]),
            )
            .is_none()
        );
        assert!(
            output_projection_suffix(
                &function_with_instance(&function_name),
                &projection_reference(vec![part("out", &[0])]),
            )
            .is_none()
        );
    }

    #[test]
    fn duplicate_inherited_def_ids_use_flattened_instance_identity() {
        let span = rumoca_core::Span::DUMMY;
        let def_id = rumoca_core::DefId::new(17);
        let mut functions = indexmap::IndexMap::new();
        for (index, name) in ["Pkg.A.f", "Pkg.B.f"].into_iter().enumerate() {
            let mut function = rumoca_core::Function::new(name, span);
            function.def_id = Some(def_id);
            function.instance_id = Some(rumoca_core::FunctionInstanceId::new(index as u32));
            functions.insert(function.name.clone(), function);
        }
        let mut component_ref = rumoca_core::component_reference_from_flat_name(
            &rumoca_core::VarName::new("Pkg.B.f.out"),
            span,
        )
        .expect("structured projected call");
        component_ref.def_id = Some(def_id);
        let name = rumoca_core::Reference::from_component_reference(component_ref)
            .with_resolved_function(rumoca_core::ResolvedFunctionReference {
                instance_id: rumoca_core::FunctionInstanceId::new(1),
                base_part_count: 3,
            });

        let (resolved, _) =
            resolve_function_reference(&functions, &name).expect("concrete function identity");

        assert_eq!(resolved.as_str(), "Pkg.B.f");
    }

    #[test]
    fn stale_function_instance_is_rejected_as_an_identity_contract_violation() {
        let span = rumoca_core::Span::DUMMY;
        let mut functions = indexmap::IndexMap::new();
        let mut function = rumoca_core::Function::new("Pkg.Random.random", span);
        function.def_id = Some(rumoca_core::DefId::new(23));
        function.instance_id = Some(rumoca_core::FunctionInstanceId::new(3));
        functions.insert(function.name.clone(), function);
        let mut component_ref = rumoca_core::component_reference_from_flat_name(
            &rumoca_core::VarName::new("Pkg.Random.random.result"),
            span,
        )
        .expect("structured projected call");
        component_ref.def_id = Some(rumoca_core::DefId::new(29));
        let name = rumoca_core::Reference::from_component_reference(component_ref)
            .with_resolved_function(rumoca_core::ResolvedFunctionReference {
                instance_id: rumoca_core::FunctionInstanceId::new(4),
                base_part_count: 3,
            });

        assert!(resolve_function_reference(&functions, &name).is_none());
    }
}
