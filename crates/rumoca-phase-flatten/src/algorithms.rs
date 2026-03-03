//! flat::Algorithm section flattening for SPEC_0020 compliance.
//!
//! This module handles:
//! - Variable name qualification in algorithm statements (Task 3.1)
//! - Output variable identification (Task 3.2)
//!
//! Algorithms are preserved as structured statements through the DAE phase,
//! with expansion to flat equations deferred to code generation.

use rumoca_core::Span;
use rumoca_ir_ast as ast;
use rumoca_ir_flat as flat;
use std::collections::HashSet;

use crate::errors::FlattenError;
use crate::qualify::{self, ImportMap, QualifyOptions};

// =============================================================================
// Task 3.1: Variable Name Qualification
// =============================================================================

/// Qualify all variable references in an algorithm section.
///
/// Per ALG-001: All variable references become globally qualified names.
/// For example, `x := y + 1` in component `comp` becomes `comp.x := comp.y + 1`.
pub fn qualify_algorithm(
    statements: &[ast::Statement],
    prefix: &ast::QualifiedName,
    imports: &ImportMap,
    initial_locals: &HashSet<String>,
) -> Vec<ast::Statement> {
    let locals = initial_locals.clone();
    statements
        .iter()
        .map(|stmt| qualify_statement(stmt, prefix, imports, &locals))
        .collect()
}

/// Qualify variable references in a single statement.
fn qualify_statement(
    stmt: &ast::Statement,
    prefix: &ast::QualifiedName,
    imports: &ImportMap,
    locals: &HashSet<String>,
) -> ast::Statement {
    match stmt {
        ast::Statement::Empty => ast::Statement::Empty,

        ast::Statement::Assignment { comp, value } => ast::Statement::Assignment {
            comp: qualify_component_ref(comp, prefix, imports, locals),
            value: qualify_expr(value, prefix, imports, locals),
        },

        ast::Statement::Return { token } => ast::Statement::Return {
            token: token.clone(),
        },

        ast::Statement::Break { token } => ast::Statement::Break {
            token: token.clone(),
        },

        ast::Statement::For { indices, equations } => {
            let mut active_locals = locals.clone();
            let mut qualified_indices: Vec<ast::ForIndex> = Vec::with_capacity(indices.len());
            // MLS §10.4.1: evaluate/index ranges left-to-right with lexical scope.
            for idx in indices {
                qualified_indices.push(ast::ForIndex {
                    ident: idx.ident.clone(),
                    range: qualify_expr(&idx.range, prefix, imports, &active_locals),
                });
                active_locals.insert(idx.ident.text.to_string());
            }

            ast::Statement::For {
                indices: qualified_indices,
                equations: equations
                    .iter()
                    .map(|s| qualify_statement(s, prefix, imports, &active_locals))
                    .collect(),
            }
        }

        ast::Statement::While(block) => {
            ast::Statement::While(qualify_statement_block(block, prefix, imports, locals))
        }

        ast::Statement::If {
            cond_blocks,
            else_block,
        } => ast::Statement::If {
            cond_blocks: cond_blocks
                .iter()
                .map(|b| qualify_statement_block(b, prefix, imports, locals))
                .collect(),
            else_block: else_block.as_ref().map(|stmts| {
                stmts
                    .iter()
                    .map(|s| qualify_statement(s, prefix, imports, locals))
                    .collect()
            }),
        },

        ast::Statement::When(blocks) => ast::Statement::When(
            blocks
                .iter()
                .map(|b| qualify_statement_block(b, prefix, imports, locals))
                .collect(),
        ),

        ast::Statement::FunctionCall {
            comp,
            args,
            outputs,
        } => ast::Statement::FunctionCall {
            // Don't qualify function name (it's a global reference)
            comp: comp.clone(),
            args: args
                .iter()
                .map(|a| qualify_expr(a, prefix, imports, locals))
                .collect(),
            outputs: outputs
                .iter()
                .map(|o| qualify_expr(o, prefix, imports, locals))
                .collect(),
        },

        ast::Statement::Reinit { variable, value } => ast::Statement::Reinit {
            variable: qualify_component_ref(variable, prefix, imports, locals),
            value: qualify_expr(value, prefix, imports, locals),
        },

        ast::Statement::Assert {
            condition,
            message,
            level,
        } => ast::Statement::Assert {
            condition: qualify_expr(condition, prefix, imports, locals),
            message: qualify_expr(message, prefix, imports, locals),
            level: level
                .as_ref()
                .map(|l| qualify_expr(l, prefix, imports, locals)),
        },
    }
}

/// Qualify a statement block (condition + statements).
fn qualify_statement_block(
    block: &ast::StatementBlock,
    prefix: &ast::QualifiedName,
    imports: &ImportMap,
    locals: &HashSet<String>,
) -> ast::StatementBlock {
    ast::StatementBlock {
        cond: qualify_expr(&block.cond, prefix, imports, locals),
        stmts: block
            .stmts
            .iter()
            .map(|s| qualify_statement(s, prefix, imports, locals))
            .collect(),
    }
}

/// Options for algorithm variable qualification.
///
/// Algorithms use different options than equations:
/// - `skip_local: true` - don't qualify local references (like loop variables)
/// - `preserve_def_id: true` - keep the original def_id for analysis
const ALGORITHM_QUALIFY_OPTS: QualifyOptions = QualifyOptions {
    skip_local: true,
    preserve_def_id: true,
};

/// Qualify a component reference by prepending the prefix.
///
/// Uses algorithm-specific options (skip local refs, preserve def_id).
fn qualify_component_ref(
    comp: &ast::ComponentReference,
    prefix: &ast::QualifiedName,
    imports: &ImportMap,
    locals: &HashSet<String>,
) -> ast::ComponentReference {
    qualify::qualify_component_ref_with_imports_and_locals(
        comp,
        prefix,
        ALGORITHM_QUALIFY_OPTS,
        locals,
        imports,
    )
}

/// Qualify expressions by qualifying component references.
///
/// Uses algorithm-specific options (skip local refs, preserve def_id).
fn qualify_expr(
    expr: &ast::Expression,
    prefix: &ast::QualifiedName,
    imports: &ImportMap,
    locals: &HashSet<String>,
) -> ast::Expression {
    qualify::qualify_expression_with_imports_and_locals(
        expr,
        prefix,
        ALGORITHM_QUALIFY_OPTS,
        locals,
        imports,
    )
}

// =============================================================================
// Task 3.2: Output Variable Identification
// =============================================================================

/// Extract all output variables (left-hand sides of assignments) from statements.
///
/// Per SPEC_0020: Track which variables are assigned in algorithms.
/// This is needed for balance checking and causality analysis.
pub fn extract_outputs(statements: &[flat::Statement]) -> Vec<flat::VarName> {
    rumoca_ir_flat::extract_algorithm_outputs(statements)
}

// =============================================================================
// Main Entry Point
// =============================================================================

/// Flatten an algorithm section with variable qualification and output extraction.
///
/// This is the main entry point for algorithm flattening, implementing
/// Tasks 3.1 (qualification) and 3.2 (outputs).
pub fn flatten_algorithm_section(
    statements: &[ast::Statement],
    prefix: &ast::QualifiedName,
    span: Span,
    origin: String,
    imports: &ImportMap,
    def_map: Option<&crate::ResolveDefMap>,
    initial_locals: &HashSet<String>,
) -> Result<flat::Algorithm, FlattenError> {
    // Task 3.1: Qualify all variable names
    let qualified_ast = qualify_algorithm(statements, prefix, imports, initial_locals);
    let qualified_statements: Vec<flat::Statement> = qualified_ast
        .iter()
        .map(|stmt| flat::Statement::from_ast_with_def_map(stmt, def_map))
        .collect();

    // Task 3.2: Extract output variables
    let outputs = extract_outputs(&qualified_statements);

    Ok(flat::Algorithm {
        statements: qualified_statements,
        outputs,
        span,
        origin,
    })
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::qualify::int_expr;
    use rumoca_ir_ast::{self as ast, ComponentRefPart, OpBinary, Subscript, Token};
    use std::sync::Arc;

    fn make_comp_ref(names: &[&str]) -> ast::ComponentReference {
        ast::ComponentReference {
            local: false,
            parts: names
                .iter()
                .map(|n| ComponentRefPart {
                    ident: Token {
                        text: std::sync::Arc::from(*n),
                        ..Default::default()
                    },
                    subs: None,
                })
                .collect(),
            def_id: None,
        }
    }

    fn make_var_expr(name: &str) -> ast::Expression {
        ast::Expression::ComponentReference(make_comp_ref(&[name]))
    }

    fn ast_to_flat(stmts: &[ast::Statement]) -> Vec<flat::Statement> {
        stmts
            .iter()
            .map(|stmt| flat::Statement::from_ast_with_def_map(stmt, None))
            .collect()
    }

    #[test]
    fn test_qualify_assignment() {
        let stmt = ast::Statement::Assignment {
            comp: make_comp_ref(&["x"]),
            value: make_var_expr("y"),
        };

        let mut prefix = ast::QualifiedName::new();
        prefix.push("comp".to_string(), vec![]);

        let qualified = qualify_statement(&stmt, &prefix, &ImportMap::default(), &HashSet::new());

        if let ast::Statement::Assignment { comp, value } = qualified {
            // x becomes comp.x
            assert_eq!(comp.parts.len(), 2);
            assert_eq!(&*comp.parts[0].ident.text, "comp");
            assert_eq!(&*comp.parts[1].ident.text, "x");

            // y becomes comp.y
            if let ast::Expression::ComponentReference(cr) = value {
                assert_eq!(cr.parts.len(), 2);
                assert_eq!(&*cr.parts[0].ident.text, "comp");
                assert_eq!(&*cr.parts[1].ident.text, "y");
            } else {
                panic!("Expected ast::ComponentReference");
            }
        } else {
            panic!("Expected Assignment");
        }
    }

    #[test]
    fn test_qualify_for_loop() {
        // for i in 1:n loop x := x + i; end for;
        let stmt = ast::Statement::For {
            indices: vec![ast::ForIndex {
                ident: Token {
                    text: std::sync::Arc::from("i"),
                    ..Default::default()
                },
                range: ast::Expression::Range {
                    start: Arc::new(int_expr(1)),
                    step: None,
                    end: Arc::new(make_var_expr("n")),
                },
            }],
            equations: vec![ast::Statement::Assignment {
                comp: make_comp_ref(&["x"]),
                value: ast::Expression::Binary {
                    op: OpBinary::Add(Token::default()),
                    lhs: Arc::new(make_var_expr("x")),
                    rhs: Arc::new(make_var_expr("i")),
                },
            }],
        };

        let mut prefix = ast::QualifiedName::new();
        prefix.push("comp".to_string(), vec![]);

        let qualified = qualify_statement(&stmt, &prefix, &ImportMap::default(), &HashSet::new());

        let ast::Statement::For { indices, equations } = qualified else {
            panic!("Expected For");
        };

        // Loop variable 'i' is NOT qualified (it's local)
        assert_eq!(&*indices[0].ident.text, "i");

        // Range end 'n' IS qualified to comp.n
        let ast::Expression::Range { end, .. } = &indices[0].range else {
            panic!("Expected Range");
        };
        let ast::Expression::ComponentReference(cr) = end.as_ref() else {
            panic!("Expected ast::ComponentReference");
        };
        assert_eq!(cr.parts.len(), 2);
        assert_eq!(&*cr.parts[0].ident.text, "comp");
        assert_eq!(&*cr.parts[1].ident.text, "n");

        // Body assignment 'x := x + i' - x is qualified, i is not (loop var)
        let ast::Statement::Assignment { comp, .. } = &equations[0] else {
            panic!("Expected Assignment");
        };
        assert_eq!(comp.parts.len(), 2);
        assert_eq!(&*comp.parts[0].ident.text, "comp");
        assert_eq!(&*comp.parts[1].ident.text, "x");
    }

    #[test]
    fn test_qualify_for_loop_keeps_index_local_in_subscript_expr() {
        // for i in 1:n loop y := t[i]; end for;
        let stmt = ast::Statement::For {
            indices: vec![ast::ForIndex {
                ident: Token {
                    text: Arc::from("i"),
                    ..Default::default()
                },
                range: ast::Expression::Range {
                    start: Arc::new(int_expr(1)),
                    step: None,
                    end: Arc::new(make_var_expr("n")),
                },
            }],
            equations: vec![ast::Statement::Assignment {
                comp: make_comp_ref(&["y"]),
                value: ast::Expression::ComponentReference(ast::ComponentReference {
                    local: false,
                    parts: vec![ComponentRefPart {
                        ident: Token {
                            text: Arc::from("t"),
                            ..Default::default()
                        },
                        subs: Some(vec![Subscript::Expression(make_var_expr("i"))]),
                    }],
                    def_id: None,
                }),
            }],
        };

        let mut prefix = ast::QualifiedName::new();
        prefix.push("a".to_string(), vec![]);
        let qualified = qualify_statement(&stmt, &prefix, &ImportMap::default(), &HashSet::new());

        let ast::Statement::For { equations, .. } = qualified else {
            panic!("Expected For");
        };
        let ast::Statement::Assignment { value, .. } = &equations[0] else {
            panic!("Expected Assignment");
        };
        let ast::Expression::ComponentReference(cr) = value else {
            panic!("Expected ast::ComponentReference");
        };
        assert_eq!(cr.parts.len(), 2);
        assert_eq!(&*cr.parts[0].ident.text, "a");
        assert_eq!(&*cr.parts[1].ident.text, "t");
        let subs = cr.parts[1].subs.as_ref().expect("expected subscript on t");
        let Subscript::Expression(sub_expr) = &subs[0] else {
            panic!("expected expression subscript");
        };
        let ast::Expression::ComponentReference(sub_cr) = sub_expr else {
            panic!("expected component reference inside subscript");
        };
        assert_eq!(sub_cr.parts.len(), 1);
        assert_eq!(&*sub_cr.parts[0].ident.text, "i");
    }

    #[test]
    fn test_extract_outputs_simple() {
        let ast_stmts = vec![
            ast::Statement::Assignment {
                comp: make_comp_ref(&["x"]),
                value: int_expr(1),
            },
            ast::Statement::Assignment {
                comp: make_comp_ref(&["y"]),
                value: int_expr(2),
            },
        ];
        let stmts = ast_to_flat(&ast_stmts);

        let outputs = extract_outputs(&stmts);

        assert_eq!(outputs.len(), 2);
        assert_eq!(outputs[0].as_str(), "x");
        assert_eq!(outputs[1].as_str(), "y");
    }

    #[test]
    fn test_extract_outputs_dedup() {
        // x := 1; x := x + 1; (x appears twice, should only be in outputs once)
        let ast_stmts = vec![
            ast::Statement::Assignment {
                comp: make_comp_ref(&["x"]),
                value: int_expr(1),
            },
            ast::Statement::Assignment {
                comp: make_comp_ref(&["x"]),
                value: ast::Expression::Binary {
                    op: OpBinary::Add(Token::default()),
                    lhs: Arc::new(make_var_expr("x")),
                    rhs: Arc::new(int_expr(1)),
                },
            },
        ];
        let stmts = ast_to_flat(&ast_stmts);

        let outputs = extract_outputs(&stmts);

        assert_eq!(outputs.len(), 1);
        assert_eq!(outputs[0].as_str(), "x");
    }

    #[test]
    fn test_extract_outputs_for_loop() {
        // for i in 1:10 loop y := y + i; end for;
        let ast_stmts = vec![ast::Statement::For {
            indices: vec![ast::ForIndex {
                ident: Token {
                    text: std::sync::Arc::from("i"),
                    ..Default::default()
                },
                range: ast::Expression::Empty,
            }],
            equations: vec![ast::Statement::Assignment {
                comp: make_comp_ref(&["y"]),
                value: int_expr(0),
            }],
        }];
        let stmts = ast_to_flat(&ast_stmts);

        let outputs = extract_outputs(&stmts);

        assert_eq!(outputs.len(), 1);
        assert_eq!(outputs[0].as_str(), "y");
    }

    #[test]
    fn test_qualify_algorithm_respects_initial_function_locals() {
        let stmt = ast::Statement::Assignment {
            comp: make_comp_ref(&["y"]),
            value: ast::Expression::ComponentReference(ast::ComponentReference {
                local: false,
                parts: vec![ComponentRefPart {
                    ident: Token {
                        text: Arc::from("table"),
                        ..Default::default()
                    },
                    subs: Some(vec![Subscript::Expression(int_expr(1))]),
                }],
                def_id: None,
            }),
        };

        let mut prefix = ast::QualifiedName::new();
        prefix.push("comp".to_string(), vec![]);

        let mut imports = ImportMap::default();
        imports.insert("table".to_string(), "Pkg.table".to_string());

        let mut initial_locals = HashSet::new();
        initial_locals.insert("y".to_string());
        initial_locals.insert("table".to_string());

        let qualified = qualify_algorithm(&[stmt], &prefix, &imports, &initial_locals);
        let ast::Statement::Assignment { comp, value } = &qualified[0] else {
            panic!("expected assignment");
        };

        assert_eq!(comp.parts.len(), 1);
        assert_eq!(&*comp.parts[0].ident.text, "y");

        let ast::Expression::ComponentReference(cr) = value else {
            panic!("expected component reference");
        };
        assert_eq!(cr.parts.len(), 1);
        assert_eq!(&*cr.parts[0].ident.text, "table");
        assert!(cr.parts[0].subs.is_some());
    }
}
