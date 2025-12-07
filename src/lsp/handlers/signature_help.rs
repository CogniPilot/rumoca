//! Signature help handler for Modelica files.

use std::collections::HashMap;

use lsp_types::{
    ParameterInformation, ParameterLabel, SignatureHelp, SignatureHelpParams, SignatureInformation,
    Uri,
};

use crate::ir::ast::{Causality, ClassType};
use crate::ir::transform::constants::get_builtin_functions;
use crate::lsp::utils::{find_function_at_cursor, parse_document};

/// Handle signature help request
pub fn handle_signature_help(
    documents: &HashMap<Uri, String>,
    params: SignatureHelpParams,
) -> Option<SignatureHelp> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;
    let text = documents.get(uri)?;
    let path = uri.path().as_str();

    let (func_name, active_param) = find_function_at_cursor(text, position)?;
    let simple_name = func_name.rsplit('.').next().unwrap_or(&func_name);

    // First check built-in functions
    for func in get_builtin_functions() {
        if func.name == simple_name {
            let params: Vec<ParameterInformation> = func
                .parameters
                .iter()
                .map(|(name, doc)| ParameterInformation {
                    label: ParameterLabel::Simple(name.to_string()),
                    documentation: Some(lsp_types::Documentation::String(doc.to_string())),
                })
                .collect();

            return Some(SignatureHelp {
                signatures: vec![SignatureInformation {
                    label: func.signature.to_string(),
                    documentation: Some(lsp_types::Documentation::String(
                        func.documentation.to_string(),
                    )),
                    parameters: Some(params),
                    active_parameter: Some(active_param as u32),
                }],
                active_signature: Some(0),
                active_parameter: Some(active_param as u32),
            });
        }
    }

    // Check user-defined functions in the AST
    if let Some(ast) = parse_document(text, path) {
        for class in ast.class_list.values() {
            for (func_class_name, func_class) in &class.classes {
                if func_class_name == simple_name && func_class.class_type == ClassType::Function {
                    let mut inputs = Vec::new();
                    let mut outputs = Vec::new();

                    for (comp_name, comp) in &func_class.components {
                        match &comp.causality {
                            Causality::Input(_) => {
                                inputs.push((comp_name.clone(), comp.type_name.to_string()));
                            }
                            Causality::Output(_) => {
                                outputs.push((comp_name.clone(), comp.type_name.to_string()));
                            }
                            _ => {}
                        }
                    }

                    let params_str = inputs
                        .iter()
                        .map(|(n, t)| format!("{}: {}", n, t))
                        .collect::<Vec<_>>()
                        .join(", ");

                    let return_str = if outputs.len() == 1 {
                        outputs[0].1.clone()
                    } else if outputs.is_empty() {
                        "()".to_string()
                    } else {
                        format!(
                            "({})",
                            outputs
                                .iter()
                                .map(|(n, t)| format!("{}: {}", n, t))
                                .collect::<Vec<_>>()
                                .join(", ")
                        )
                    };

                    let signature =
                        format!("{}({}) -> {}", func_class_name, params_str, return_str);

                    let params: Vec<ParameterInformation> = inputs
                        .iter()
                        .map(|(name, type_name)| ParameterInformation {
                            label: ParameterLabel::Simple(format!("{}: {}", name, type_name)),
                            documentation: None,
                        })
                        .collect();

                    return Some(SignatureHelp {
                        signatures: vec![SignatureInformation {
                            label: signature,
                            documentation: None,
                            parameters: Some(params),
                            active_parameter: Some(active_param as u32),
                        }],
                        active_signature: Some(0),
                        active_parameter: Some(active_param as u32),
                    });
                }
            }
        }
    }

    None
}
