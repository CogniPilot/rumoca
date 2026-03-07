use super::*;

/// Collect variable references from for-loop ranges and if-equation conditions.
///
/// Per MLS §18.3, parameters used in for-loop ranges and if-equation conditions
/// are structural and must be evaluable at translation time.
pub(crate) fn collect_structural_refs_from_equations(
    equations: &[rumoca_ir_ast::Equation],
    refs: &mut std::collections::HashSet<String>,
) {
    for eq in equations {
        match eq {
            Equation::For { indices, equations } => {
                for index in indices {
                    refs.extend(rumoca_eval_ast::eval::collect_variable_refs(&index.range));
                }
                collect_structural_refs_from_equations(equations, refs);
            }
            Equation::If {
                cond_blocks,
                else_block,
            } => {
                for block in cond_blocks {
                    refs.extend(rumoca_eval_ast::eval::collect_variable_refs(&block.cond));
                    collect_structural_refs_from_equations(&block.eqs, refs);
                }
                if let Some(else_eqs) = else_block {
                    collect_structural_refs_from_equations(else_eqs, refs);
                }
            }
            _ => {}
        }
    }
}

/// Type check a ResolvedTree.
///
/// This is the main entry point for type checking.
/// Takes a `ResolvedTree` and returns a `TypedTree` with all TypeIds populated.
pub fn typecheck(resolved: ResolvedTree) -> Result<TypedTree, Diagnostics> {
    let mut tree = resolved.into_inner();
    let mut checker = TypeChecker::new();
    checker.check(&mut tree);

    if checker.has_errors() {
        Err(checker.take_diagnostics())
    } else {
        Ok(TypedTree::new(tree))
    }
}

/// Type check an instanced model (after instantiation).
///
/// This function performs type checking on an already-instantiated model.
/// It runs after instantiation, which means:
/// - All modifications have been applied
/// - Structural parameters have their final values from modifiers
/// - Dimension expressions can be evaluated with full context
///
/// Running type checking after instantiation ensures it has access to the
/// complete modification context for evaluating dimension expressions (MLS §10.1).
///
/// # Arguments
///
/// * `tree` - Reference to the class tree (shared, not cloned)
/// * `overlay` - The instance overlay with modification values
///
/// # Returns
///
/// Ok(()) if type checking succeeds, or diagnostics on error.
/// The overlay is modified in place with evaluated dimensions.
pub fn typecheck_instanced(
    tree: &ClassTree,
    overlay: &mut InstanceOverlay,
    model_name: &str,
) -> Result<(), Diagnostics> {
    let mut checker = TypeChecker::new();
    checker.check_instanced(tree, overlay, model_name);

    if checker.has_errors() {
        Err(checker.take_diagnostics())
    } else {
        Ok(())
    }
}
