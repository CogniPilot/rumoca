//! Analyze command handler for Modelica classes.
//!
//! Provides on-demand compilation and balance analysis for specific classes.

use lsp_types::Uri;

use super::WorkspaceState;
use super::utils::parse_document;

/// Result of analyzing a class
#[derive(Debug, Clone)]
pub struct AnalyzeResult {
    /// The class that was analyzed
    pub class_name: String,
    /// Number of state variables
    pub num_states: usize,
    /// Number of unknown variables
    pub num_unknowns: usize,
    /// Number of equations
    pub num_equations: usize,
    /// Number of algebraic variables
    pub num_algebraic: usize,
    /// Number of parameters
    pub num_parameters: usize,
    /// Number of inputs
    pub num_inputs: usize,
    /// Whether the system is balanced
    pub is_balanced: bool,
    /// Error message if compilation failed
    pub error: Option<String>,
}

/// Analyze a specific class in a document
///
/// This compiles the class and computes its balance information,
/// caching the result in the workspace for display in code lens.
pub fn analyze_class(workspace: &mut WorkspaceState, uri: &Uri, class_name: &str) -> AnalyzeResult {
    let text = match workspace.get_document(uri) {
        Some(t) => t.clone(),
        None => {
            return AnalyzeResult {
                class_name: class_name.to_string(),
                num_states: 0,
                num_unknowns: 0,
                num_equations: 0,
                num_algebraic: 0,
                num_parameters: 0,
                num_inputs: 0,
                is_balanced: false,
                error: Some("Document not found".to_string()),
            };
        }
    };

    let path = uri.path().as_str();

    // First verify the class exists by parsing
    let ast = match parse_document(&text, path) {
        Some(ast) => ast,
        None => {
            return AnalyzeResult {
                class_name: class_name.to_string(),
                num_states: 0,
                num_unknowns: 0,
                num_equations: 0,
                num_algebraic: 0,
                num_parameters: 0,
                num_inputs: 0,
                is_balanced: false,
                error: Some("Failed to parse document".to_string()),
            };
        }
    };

    // Try to compile the specific class
    match crate::Compiler::new()
        .model(class_name)
        .compile_str(&text, path)
    {
        Ok(result) => {
            let balance = result.dae.check_balance();

            // Cache the balance result
            workspace.set_balance(uri.clone(), class_name.to_string(), balance.clone());

            AnalyzeResult {
                class_name: class_name.to_string(),
                num_states: balance.num_states,
                num_unknowns: balance.num_unknowns,
                num_equations: balance.num_equations,
                num_algebraic: balance.num_algebraic,
                num_parameters: balance.num_parameters,
                num_inputs: balance.num_inputs,
                is_balanced: balance.is_balanced,
                error: None,
            }
        }
        Err(e) => {
            // Check if the class exists in the AST but just failed to compile
            let class_exists = class_exists_in_ast(&ast, class_name);

            AnalyzeResult {
                class_name: class_name.to_string(),
                num_states: 0,
                num_unknowns: 0,
                num_equations: 0,
                num_algebraic: 0,
                num_parameters: 0,
                num_inputs: 0,
                is_balanced: false,
                error: Some(if class_exists {
                    format!("Compilation failed: {}", e)
                } else {
                    format!("Class '{}' not found", class_name)
                }),
            }
        }
    }
}

/// Check if a class exists in the AST (supports dotted paths for nested classes)
fn class_exists_in_ast(ast: &crate::ir::ast::StoredDefinition, class_name: &str) -> bool {
    let parts: Vec<&str> = class_name.split('.').collect();

    if parts.is_empty() {
        return false;
    }

    // Find the top-level class
    let top_class = match ast.class_list.get(parts[0]) {
        Some(c) => c,
        None => return false,
    };

    // Navigate to nested classes if path has multiple parts
    let mut current = top_class;
    for part in parts.iter().skip(1) {
        match current.classes.get(*part) {
            Some(nested) => current = nested,
            None => return false,
        }
    }

    true
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_analyze_result_creation() {
        let result = AnalyzeResult {
            class_name: "Test".to_string(),
            num_states: 2,
            num_unknowns: 4,
            num_equations: 4,
            num_algebraic: 2,
            num_parameters: 1,
            num_inputs: 0,
            is_balanced: true,
            error: None,
        };
        assert!(result.is_balanced);
        assert_eq!(result.num_states, 2);
    }
}
