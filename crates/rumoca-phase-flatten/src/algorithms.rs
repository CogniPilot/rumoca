//! `flat::Algorithm` section flattening for SPEC_0007 / MLS §11 compliance.
//!
//! This module handles:
//! - Variable name qualification in algorithm statements (Task 3.1)
//! - Output variable identification (Task 3.2)
//!
//! Algorithms are preserved as structured statements through the DAE phase,
//! with expansion to flat equations deferred to code generation.

use rumoca_core::SourceMap;
use rumoca_core::{Span, extract_algorithm_outputs};
use rumoca_ir_ast as ast;
use rumoca_ir_flat as flat;
use std::collections::HashSet;

use crate::ast_lower;
use crate::errors::FlattenError;
use crate::qualify::{self, ImportMap, QualifyOptions};

// =============================================================================
// Task 3.1: Variable Name Qualification
// =============================================================================

/// Qualify all variable references in an algorithm section.
///
/// Per ALG-001: All variable references become globally unique flat names.
/// For example, `x := y + 1` in component `comp` becomes `comp.x := comp.y + 1`.
pub(crate) fn qualify_algorithm(
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
                .map(|l| Box::new(qualify_expr(l, prefix, imports, locals))),
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
const ALGORITHM_QUALIFY_OPTS: QualifyOptions = QualifyOptions { skip_local: true };

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
/// Per SPEC_0007 / MLS §11: track variables assigned in algorithms.
/// This is needed for balance checking and causality analysis.
pub(crate) fn extract_outputs(
    statements: &[rumoca_core::Statement],
) -> Vec<rumoca_core::Reference> {
    extract_algorithm_outputs(statements)
}

// =============================================================================
// Write targets: owning instance path materialization
// =============================================================================

/// Materialize the owning component instance path into an algorithm section's
/// write targets.
///
/// MLS §11.2.1.1 (assignment), §11.2.1.2 (multiple-output call) and §8.3.6
/// (`reinit`) make every write target a component reference looked up in the
/// enclosing class scope, so inside the algorithm section of component
/// occurrence `alg` the target `y` denotes `alg.y`.
///
/// Expression references reach Flat as a `Reference`, which carries the flat
/// coordinate name beside the exact source occurrence. A write target reaches
/// Flat as a bare `ComponentReference`, whose identity *is* its part chain
/// (`ComponentReference::to_var_name`), so there is no name slot to carry the
/// prefix: the owning instance path has to become part of the chain. `owner`
/// is the reference Instantiate proved for the owning component occurrence, so
/// each materialized part carries its own Resolve declaration identity and no
/// name is recovered from rendered text.
///
/// The root class occurrence has no owning component and passes an empty
/// `owner`, which leaves every target untouched — as does a target rooted in a
/// loop index or another algorithm-local name (MLS §11.2.2.2), which is never a
/// class member.
pub(crate) fn qualify_write_targets_with_owner_path(
    algorithm: &mut flat::Algorithm,
    owner: &[rumoca_core::ComponentRefPart],
) -> Result<(), FlattenError> {
    if owner.is_empty() {
        return Ok(());
    }
    // A class algorithm section opens with no algorithm-local names; the only
    // ones it can introduce are its own loop indices (MLS §11.2.2.2).
    qualify_statement_write_targets(&mut algorithm.statements, owner, &HashSet::new())?;
    algorithm.outputs = extract_outputs(&algorithm.statements);
    Ok(())
}

fn qualify_statement_write_targets(
    statements: &mut [rumoca_core::Statement],
    owner: &[rumoca_core::ComponentRefPart],
    locals: &HashSet<String>,
) -> Result<(), FlattenError> {
    for statement in statements {
        match statement {
            rumoca_core::Statement::Assignment { comp, .. } => {
                prepend_owner_path(comp, owner, locals)?;
            }
            rumoca_core::Statement::Reinit { variable, .. } => {
                prepend_owner_path(variable, owner, locals)?;
            }
            rumoca_core::Statement::FunctionCall { outputs, .. } => {
                for output in outputs.iter_mut().flatten() {
                    prepend_owner_path(output, owner, locals)?;
                }
            }
            rumoca_core::Statement::For {
                indices, equations, ..
            } => {
                // MLS §11.2.2.2: a loop index is local to its body and shadows
                // any class member of the same name for the statements inside.
                let mut body_locals = locals.clone();
                body_locals.extend(indices.iter().map(|index| index.ident.clone()));
                qualify_statement_write_targets(equations, owner, &body_locals)?;
            }
            rumoca_core::Statement::While { block, .. } => {
                qualify_statement_write_targets(&mut block.stmts, owner, locals)?;
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => {
                for block in cond_blocks {
                    qualify_statement_write_targets(&mut block.stmts, owner, locals)?;
                }
                if let Some(statements) = else_block {
                    qualify_statement_write_targets(statements, owner, locals)?;
                }
            }
            rumoca_core::Statement::When { blocks, .. } => {
                for block in blocks {
                    qualify_statement_write_targets(&mut block.stmts, owner, locals)?;
                }
            }
            rumoca_core::Statement::Empty { .. }
            | rumoca_core::Statement::Return { .. }
            | rumoca_core::Statement::Break { .. }
            | rumoca_core::Statement::Assert { .. } => {}
        }
    }
    Ok(())
}

fn prepend_owner_path(
    target: &mut rumoca_core::ComponentReference,
    owner: &[rumoca_core::ComponentRefPart],
    locals: &HashSet<String>,
) -> Result<(), FlattenError> {
    // `ComponentReference::construct` refuses an empty part chain, so a
    // reference that exists always has a root part.
    let root = &target.parts()[0];
    if locals.contains(root.ident.as_str()) {
        return Ok(());
    }
    let mut parts = owner.to_vec();
    parts.extend(target.parts().iter().cloned());
    *target = target.with_replaced_parts(parts).map_err(|error| {
        FlattenError::missing_flat_variable_identity(error.to_string(), target.span())
    })?;
    Ok(())
}

// =============================================================================
// Main Entry Point
// =============================================================================

pub(crate) struct AlgorithmSectionContext<'a> {
    pub(crate) prefix: &'a ast::QualifiedName,
    pub(crate) imports: &'a ImportMap,
    pub(crate) initial_locals: &'a HashSet<String>,
    pub(crate) source_map: Option<&'a SourceMap>,
    pub(crate) instance_name: Option<&'a str>,
    pub(crate) predefined_string_declaration: Option<rumoca_core::DefId>,
    pub(crate) predefined_intrinsics: ast_lower::PredefinedIntrinsicIds,
}

pub(crate) struct AlgorithmSectionMetadata {
    pub(crate) span: Span,
    pub(crate) origin: String,
}

impl AlgorithmSectionMetadata {
    pub(crate) fn new(span: Span, origin: String) -> Self {
        Self { span, origin }
    }
}

/// Flatten an algorithm section with variable qualification and output extraction.
///
/// This is the main entry point for algorithm flattening, implementing
/// Tasks 3.1 (qualification) and 3.2 (outputs).
pub(crate) fn flatten_algorithm_section(
    statements: &[ast::Statement],
    context: AlgorithmSectionContext<'_>,
    metadata: AlgorithmSectionMetadata,
) -> Result<flat::Algorithm, FlattenError> {
    // Task 3.1: Qualify all variable names
    let qualified_ast = qualify_algorithm(
        statements,
        context.prefix,
        context.imports,
        context.initial_locals,
    );
    let qualified_statements: Vec<rumoca_core::Statement> = qualified_ast
        .iter()
        .map(|stmt| {
            ast_lower::statement_from_ast_with_context_and_source_map(
                stmt,
                ast_lower::LoweringContext {
                    instance_name: context.instance_name,
                    predefined_string_declaration: context.predefined_string_declaration,
                    predefined_intrinsics: context.predefined_intrinsics,
                },
                context.source_map,
            )
        })
        .collect::<Result<_, FlattenError>>()?;

    // Task 3.2: Extract output variables
    let outputs = extract_outputs(&qualified_statements);

    Ok(flat::Algorithm {
        statements: qualified_statements,
        outputs,
        span: metadata.span,
        origin: metadata.origin,
    })
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::qualify::int_expr;
    use rumoca_core::{OpBinary, Token};
    use rumoca_ir_ast::{self as ast, ComponentRefPart, Subscript};
    use std::sync::Arc;

    fn test_span() -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("phase_flatten_algorithms_source_0.mo"),
            0,
            1,
        )
    }

    fn fixture_def_id(name: &str) -> rumoca_core::DefId {
        let hash = name.bytes().fold(2_166_136_261_u32, |hash, byte| {
            hash.wrapping_mul(16_777_619) ^ u32::from(byte)
        });
        rumoca_core::DefId::new(hash.max(1))
    }

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
                    def_id: Some(fixture_def_id(n)),
                })
                .collect(),
            span: test_span(),
            qualified_display_name: None,
        }
    }

    fn make_var_expr(name: &str) -> ast::Expression {
        ast::Expression::ComponentReference(make_comp_ref(&[name]))
    }

    fn ast_to_flat(stmts: &[ast::Statement]) -> Vec<rumoca_core::Statement> {
        stmts
            .iter()
            .map(|stmt| ast_lower::statement_from_ast(stmt).unwrap())
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
            assert_eq!(comp.parts.len(), 1);
            assert_eq!(&*comp.parts[0].ident.text, "x");
            assert_eq!(
                comp.qualified_display_name().map(|name| name.as_str()),
                Some("comp.x")
            );

            if let ast::Expression::ComponentReference(cr) = value {
                assert_eq!(cr.parts.len(), 1);
                assert_eq!(&*cr.parts[0].ident.text, "y");
                assert_eq!(
                    cr.qualified_display_name().map(|name| name.as_str()),
                    Some("comp.y")
                );
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
                    span: rumoca_core::Span::DUMMY,
                },
            }],
            equations: vec![ast::Statement::Assignment {
                comp: make_comp_ref(&["x"]),
                value: ast::Expression::Binary {
                    op: OpBinary::Add,
                    lhs: Arc::new(make_var_expr("x")),
                    rhs: Arc::new(make_var_expr("i")),
                    span: rumoca_core::Span::DUMMY,
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
        assert_eq!(cr.parts.len(), 1);
        assert_eq!(&*cr.parts[0].ident.text, "n");
        assert_eq!(
            cr.qualified_display_name().map(|name| name.as_str()),
            Some("comp.n")
        );

        // Body assignment 'x := x + i' - x is qualified, i is not (loop var)
        let ast::Statement::Assignment { comp, .. } = &equations[0] else {
            panic!("Expected Assignment");
        };
        assert_eq!(comp.parts.len(), 1);
        assert_eq!(&*comp.parts[0].ident.text, "x");
        assert_eq!(
            comp.qualified_display_name().map(|name| name.as_str()),
            Some("comp.x")
        );
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
                    span: rumoca_core::Span::DUMMY,
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
                        def_id: Some(rumoca_core::DefId::new(1)),
                    }],
                    span: rumoca_core::Span::DUMMY,
                    qualified_display_name: None,
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
        assert_eq!(cr.parts.len(), 1);
        assert_eq!(&*cr.parts[0].ident.text, "t");
        assert_eq!(
            cr.qualified_display_name().map(|name| name.as_str()),
            Some("a.t[i]")
        );
        let subs = cr.parts[0].subs.as_ref().expect("expected subscript on t");
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
                    op: OpBinary::Add,
                    lhs: Arc::new(make_var_expr("x")),
                    rhs: Arc::new(int_expr(1)),
                    span: rumoca_core::Span::DUMMY,
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
                range: ast::Expression::Empty { span: test_span() },
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
                    def_id: Some(rumoca_core::DefId::new(1)),
                }],
                span: rumoca_core::Span::DUMMY,
                qualified_display_name: None,
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

    #[test]
    fn test_qualify_algorithm_keeps_local_record_root_before_import_alias() {
        let stmt = ast::Statement::Assignment {
            comp: make_comp_ref(&["tau2"]),
            value: ast::Expression::ComponentReference(make_comp_ref(&["g", "tau"])),
        };

        let mut imports = ImportMap::default();
        imports.insert("g".to_string(), "Modelica.Constants.g_n".to_string());

        let mut initial_locals = HashSet::new();
        initial_locals.insert("g".to_string());
        initial_locals.insert("tau2".to_string());

        let qualified = qualify_algorithm(
            &[stmt],
            &ast::QualifiedName::new(),
            &imports,
            &initial_locals,
        );
        let ast::Statement::Assignment { value, .. } = &qualified[0] else {
            panic!("expected assignment");
        };
        let ast::Expression::ComponentReference(cr) = value else {
            panic!("expected component reference");
        };

        assert_eq!(cr.parts.len(), 2);
        assert_eq!(&*cr.parts[0].ident.text, "g");
        assert_eq!(&*cr.parts[1].ident.text, "tau");
        assert_eq!(cr.root_def_id(), Some(fixture_def_id("g")));
    }

    fn core_part(name: &str) -> rumoca_core::ComponentRefPart {
        rumoca_core::ComponentRefPart {
            ident: name.to_string(),
            span: test_span(),
            subs: Vec::new(),
            def_id: fixture_def_id(name),
        }
    }

    fn algorithm_of(statements: Vec<rumoca_core::Statement>) -> flat::Algorithm {
        let outputs = extract_outputs(&statements);
        flat::Algorithm {
            statements,
            outputs,
            span: test_span(),
            origin: "algorithm from alg".to_string(),
        }
    }

    /// MLS §11.2.1.1: the target of an assignment inside the algorithm section
    /// of component occurrence `alg` names a member of `alg`, and a Flat write
    /// target's identity is its part chain.
    #[test]
    fn component_algorithm_write_target_carries_the_owner_instance_path() {
        let target =
            rumoca_core::ComponentReference::construct(false, test_span(), vec![core_part("y")])
                .expect("fixture target");
        let mut algorithm = algorithm_of(vec![rumoca_core::Statement::Assignment {
            comp: target,
            value: rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(1.0),
                span: test_span(),
            },
            span: test_span(),
        }]);

        qualify_write_targets_with_owner_path(&mut algorithm, &[core_part("alg")])
            .expect("owner path materialization");

        let rumoca_core::Statement::Assignment { comp, .. } = &algorithm.statements[0] else {
            panic!("expected assignment");
        };
        assert_eq!(comp.to_var_name().as_str(), "alg.y");
        assert_eq!(comp.root_def_id(), fixture_def_id("alg"));
        assert_eq!(comp.target_def_id(), fixture_def_id("y"));
        assert_eq!(algorithm.outputs.len(), 1);
        assert_eq!(algorithm.outputs[0].as_str(), "alg.y");
    }

    /// MLS §11.2.2.2: a loop index is local to its body, so it is never a
    /// member of the owning component occurrence.
    #[test]
    fn loop_index_target_is_left_out_of_the_owner_instance_path() {
        let indexed =
            rumoca_core::ComponentReference::construct(false, test_span(), vec![core_part("i")])
                .expect("fixture target");
        let mut algorithm = algorithm_of(vec![rumoca_core::Statement::For {
            indices: vec![rumoca_core::ForIndex {
                ident: "i".to_string(),
                range: rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(1),
                    span: test_span(),
                },
            }],
            equations: vec![rumoca_core::Statement::Assignment {
                comp: indexed,
                value: rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(1.0),
                    span: test_span(),
                },
                span: test_span(),
            }],
            span: test_span(),
        }]);

        qualify_write_targets_with_owner_path(&mut algorithm, &[core_part("alg")])
            .expect("owner path materialization");

        let rumoca_core::Statement::For { equations, .. } = &algorithm.statements[0] else {
            panic!("expected for statement");
        };
        let rumoca_core::Statement::Assignment { comp, .. } = &equations[0] else {
            panic!("expected assignment");
        };
        assert_eq!(comp.to_var_name().as_str(), "i");
    }
}
