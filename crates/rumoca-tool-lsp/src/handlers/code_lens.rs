//! Code lens handler for Modelica files.

use lsp_types::{CodeLens, Command, Url};
use rumoca_session::parsing::ast;
use rumoca_session::parsing::ir_core as rumoca_ir_core;

use super::workspace_symbols::collect_model_names;

/// Cached balance information for a model.
struct BalanceInfo {
    pub name: String,
    pub n_unknowns: usize,
    pub n_equations: usize,
}

impl BalanceInfo {
    fn is_balanced(&self) -> bool {
        self.n_unknowns == self.n_equations
    }

    fn label(&self) -> String {
        if self.is_balanced() {
            format!(
                "Balanced ({} states, {} eqs)",
                self.n_unknowns, self.n_equations
            )
        } else {
            format!(
                "Unbalanced ({} unknowns, {} eqs)",
                self.n_unknowns, self.n_equations
            )
        }
    }
}

/// Compute balance info from AST (simple heuristic based on component/equation counts).
fn compute_balance_info(ast: &ast::StoredDefinition) -> Vec<BalanceInfo> {
    let mut results = Vec::new();
    for (name, class) in &ast.classes {
        if !matches!(
            class.class_type,
            ast::ClassType::Model | ast::ClassType::Block
        ) {
            continue;
        }
        // Count non-parameter, non-constant components as unknowns
        let n_unknowns = class
            .components
            .values()
            .filter(|c| {
                !matches!(
                    c.variability,
                    rumoca_ir_core::Variability::Parameter(_)
                        | rumoca_ir_core::Variability::Constant(_)
                )
            })
            .count();
        let n_equations = class.equations.len();
        results.push(BalanceInfo {
            name: name.clone(),
            n_unknowns,
            n_equations,
        });
    }
    results
}

/// Handle code lens request - show balance status for models.
pub fn handle_code_lens(ast: &ast::StoredDefinition, _uri: &Url) -> Vec<CodeLens> {
    let model_names = collect_model_names(ast);
    let balance_infos = compute_balance_info(ast);

    let mut lenses = Vec::new();
    for (name, range) in &model_names {
        if let Some(info) = balance_infos.iter().find(|b| &b.name == name) {
            lenses.push(CodeLens {
                range: *range,
                command: Some(Command {
                    title: info.label(),
                    command: String::new(),
                    arguments: None,
                }),
                data: None,
            });
        }
    }

    lenses
}
