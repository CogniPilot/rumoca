use super::*;
use rumoca_ir_ast::Visitor;
use std::ops::ControlFlow;

/// Collect variable references from for-loop ranges and if-equation conditions.
///
/// Per MLS §18.3, parameters used in for-loop ranges and if-equation conditions
/// are structural and must be evaluable at translation time.
pub(crate) fn collect_structural_refs_from_equations(
    equations: &[rumoca_ir_ast::Equation],
    refs: &mut std::collections::HashSet<String>,
) {
    struct StructuralEquationRefCollector<'a> {
        refs: &'a mut std::collections::HashSet<String>,
    }

    impl rumoca_ir_ast::Visitor for StructuralEquationRefCollector<'_> {
        fn visit_equation(&mut self, eq: &rumoca_ir_ast::Equation) -> ControlFlow<()> {
            match eq {
                rumoca_ir_ast::Equation::For { indices, equations } => {
                    self.visit_for_equation(indices, equations)
                }
                rumoca_ir_ast::Equation::If {
                    cond_blocks,
                    else_block,
                } => self.visit_if_equation(cond_blocks, else_block.as_deref()),
                _ => ControlFlow::Continue(()),
            }
        }

        fn visit_for_equation(
            &mut self,
            indices: &[rumoca_ir_ast::ForIndex],
            equations: &[rumoca_ir_ast::Equation],
        ) -> ControlFlow<()> {
            for index in indices {
                self.refs
                    .extend(rumoca_eval_ast::eval::collect_variable_refs(&index.range));
            }
            self.visit_each(equations, Self::visit_equation)
        }

        fn visit_if_equation(
            &mut self,
            cond_blocks: &[rumoca_ir_ast::EquationBlock],
            else_block: Option<&[rumoca_ir_ast::Equation]>,
        ) -> ControlFlow<()> {
            for block in cond_blocks {
                self.refs
                    .extend(rumoca_eval_ast::eval::collect_variable_refs(&block.cond));
                self.visit_each(&block.eqs, Self::visit_equation)?;
            }
            if let Some(else_eqs) = else_block {
                self.visit_each(else_eqs, Self::visit_equation)?;
            }
            ControlFlow::Continue(())
        }
    }

    let mut collector = StructuralEquationRefCollector { refs };
    let _ = collector.visit_each(
        equations,
        <StructuralEquationRefCollector<'_> as Visitor>::visit_equation,
    );
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

#[cfg(test)]
mod tests {
    use super::collect_structural_refs_from_equations;
    use rumoca_phase_parse::parse_to_ast;

    #[test]
    fn collect_structural_refs_tracks_for_ranges_and_if_conditions_only() {
        let source = r#"
model Test
  parameter Integer n = 2;
  parameter Integer m = 3;
  parameter Integer k = 4;
  Real x[n];
equation
  for i in 1:m loop
    x[i] = 0;
  end for;
  if k > 0 then
    x[1] = 1;
  end if;
  when sample(0, n) then
    x[1] = 2;
  end when;
end Test;
"#;
        let def = parse_to_ast(source, "test.mo").expect("parse should succeed");
        let class = def.classes.get("Test").expect("class should exist");
        let mut refs = std::collections::HashSet::new();
        collect_structural_refs_from_equations(&class.equations, &mut refs);

        assert!(refs.contains("m"), "for-range ref should be collected");
        assert!(refs.contains("k"), "if-condition ref should be collected");
        assert!(
            !refs.contains("n"),
            "when-condition refs should remain excluded for parity"
        );
    }
}
