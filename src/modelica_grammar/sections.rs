//! Conversion for equation and algorithm sections.

use crate::ir;
use crate::modelica_grammar_trait;

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]

pub struct EquationSection {
    pub initial: bool,
    pub equations: Vec<ir::ast::Equation>,
    /// Token for "equation" keyword
    pub equation_keyword: ir::ast::Token,
    /// Token for "initial" keyword (if present)
    pub initial_keyword: Option<ir::ast::Token>,
}

impl TryFrom<&modelica_grammar_trait::EquationSection> for EquationSection {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::EquationSection,
    ) -> std::result::Result<Self, Self::Error> {
        let initial_keyword = ast
            .equation_section_opt
            .as_ref()
            .map(|opt| opt.initial.initial.clone());
        let mut def = EquationSection {
            initial: ast.equation_section_opt.is_some(),
            equations: vec![],
            equation_keyword: ast.equation.equation.clone(),
            initial_keyword,
        };
        for eq in &ast.equation_section_list {
            def.equations.push(eq.some_equation.clone());
        }
        Ok(def)
    }
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]

pub struct AlgorithmSection {
    pub initial: bool,
    pub statements: Vec<ir::ast::Statement>,
    /// Token for "algorithm" keyword
    pub algorithm_keyword: ir::ast::Token,
    /// Token for "initial" keyword (if present)
    pub initial_keyword: Option<ir::ast::Token>,
}

impl TryFrom<&modelica_grammar_trait::AlgorithmSection> for AlgorithmSection {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::AlgorithmSection,
    ) -> std::result::Result<Self, Self::Error> {
        let initial_keyword = ast
            .algorithm_section_opt
            .as_ref()
            .map(|opt| opt.initial.initial.clone());
        let mut def = AlgorithmSection {
            initial: ast.algorithm_section_opt.is_some(),
            statements: vec![],
            algorithm_keyword: ast.algorithm.algorithm.clone(),
            initial_keyword,
        };
        for alg in &ast.algorithm_section_list {
            def.statements.push(alg.statement.clone());
        }
        Ok(def)
    }
}
