//! Analyze command handler for Modelica classes.
//!
//! Provides on-demand compilation and balance analysis for specific classes.
//! Uses the shared `BalanceResult` from `dae/balance.rs` for balance information.

use lsp_types::Uri;

use crate::dae::balance::BalanceResult;

use super::WorkspaceState;
use super::utils::parse_document;

/// Result of analyzing a class.
///
/// This wraps `BalanceResult` with additional context about the analysis:
/// - `class_name`: The class that was analyzed
/// - `error`: Error message if compilation failed
///
/// For numerical balance information, use the `balance` field directly.
#[derive(Debug, Clone)]
pub struct AnalyzeResult {
    /// The class that was analyzed
    pub class_name: String,
    /// Balance information from the DAE (None if compilation failed)
    pub balance: Option<BalanceResult>,
    /// Error message if compilation failed
    pub error: Option<String>,
}

impl AnalyzeResult {
    /// Create a successful result with balance information
    pub fn success(class_name: String, balance: BalanceResult) -> Self {
        Self {
            class_name,
            balance: Some(balance),
            error: None,
        }
    }

    /// Create a failed result with an error message
    pub fn failed(class_name: String, error: String) -> Self {
        Self {
            class_name,
            balance: None,
            error: Some(error),
        }
    }

    /// Convenience accessor for num_states (0 if no balance)
    pub fn num_states(&self) -> usize {
        self.balance.as_ref().map(|b| b.num_states).unwrap_or(0)
    }

    /// Convenience accessor for num_unknowns (0 if no balance)
    pub fn num_unknowns(&self) -> usize {
        self.balance.as_ref().map(|b| b.num_unknowns).unwrap_or(0)
    }

    /// Convenience accessor for num_equations (0 if no balance)
    pub fn num_equations(&self) -> usize {
        self.balance.as_ref().map(|b| b.num_equations).unwrap_or(0)
    }

    /// Convenience accessor for num_algebraic (0 if no balance)
    pub fn num_algebraic(&self) -> usize {
        self.balance.as_ref().map(|b| b.num_algebraic).unwrap_or(0)
    }

    /// Convenience accessor for num_parameters (0 if no balance)
    pub fn num_parameters(&self) -> usize {
        self.balance.as_ref().map(|b| b.num_parameters).unwrap_or(0)
    }

    /// Convenience accessor for num_inputs (0 if no balance)
    pub fn num_inputs(&self) -> usize {
        self.balance.as_ref().map(|b| b.num_inputs).unwrap_or(0)
    }

    /// Convenience accessor for is_balanced (false if no balance)
    pub fn is_balanced(&self) -> bool {
        self.balance
            .as_ref()
            .map(|b| b.is_balanced())
            .unwrap_or(false)
    }
}

/// Analyze a specific class in a document
///
/// This compiles the class and computes its balance information,
/// caching the result in the workspace for display in code lens.
pub fn analyze_class(workspace: &mut WorkspaceState, uri: &Uri, class_name: &str) -> AnalyzeResult {
    let text = match workspace.get_document(uri) {
        Some(t) => t.clone(),
        None => {
            return AnalyzeResult::failed(class_name.to_string(), "Document not found".to_string());
        }
    };

    let path = uri.path().as_str();

    // First verify the class exists by parsing
    let ast = match parse_document(&text, path) {
        Some(ast) => ast,
        None => {
            return AnalyzeResult::failed(
                class_name.to_string(),
                "Failed to parse document".to_string(),
            );
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

            AnalyzeResult::success(class_name.to_string(), balance)
        }
        Err(e) => {
            // Check if the class exists in the AST but just failed to compile
            let class_exists = class_exists_in_ast(&ast, class_name);

            AnalyzeResult::failed(
                class_name.to_string(),
                if class_exists {
                    format!("Compilation failed: {}", e)
                } else {
                    format!("Class '{}' not found", class_name)
                },
            )
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
    use crate::dae::balance::BalanceStatus;

    #[test]
    fn test_analyze_result_success() {
        let balance = BalanceResult {
            num_states: 2,
            num_unknowns: 4,
            num_equations: 4,
            num_algebraic: 2,
            num_parameters: 1,
            num_inputs: 0,
            num_external_connectors: 0,
            status: BalanceStatus::Balanced,
            compile_time_ms: 0,
        };
        let result = AnalyzeResult::success("Test".to_string(), balance);
        assert!(result.is_balanced());
        assert_eq!(result.num_states(), 2);
        assert!(result.error.is_none());
    }

    #[test]
    fn test_analyze_result_failed() {
        let result = AnalyzeResult::failed("Test".to_string(), "Some error".to_string());
        assert!(!result.is_balanced());
        assert_eq!(result.num_states(), 0);
        assert!(result.error.is_some());
    }
}
