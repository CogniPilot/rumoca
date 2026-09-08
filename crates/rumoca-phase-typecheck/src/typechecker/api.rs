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
    let _visit_outcome = collector.visit_each(
        equations,
        <StructuralEquationRefCollector<'_> as Visitor>::visit_equation,
    );
}

/// Type check a ResolvedTree as a standalone diagnostics query.
///
/// Returns the checked `ClassTree` with all TypeIds populated. This is data
/// plus diagnostics, not a proof: nothing downstream accepts this output as
/// phase evidence, and the production pipeline mints its proof only through
/// [`typecheck_instanced_tree`].
///
/// The mutable phase context is deliberately not part of the public API, so a
/// caller cannot reuse tree-local identities or diagnostics across roots.
///
/// ```compile_fail
/// use rumoca_phase_typecheck::TypeChecker;
/// ```
pub fn typecheck(resolved: ResolvedTree) -> Result<ClassTree, Diagnostics> {
    // This standalone diagnostics query returns data, not a phase proof. It
    // copies the borrowed Resolve view explicitly so the proof carrier itself
    // exposes no owned extraction route.
    let mut tree = resolved.inner().clone();
    let diagnostics = TypeChecker::new().check(&mut tree);

    if diagnostics.has_errors() {
        Err(diagnostics)
    } else {
        Ok(tree)
    }
}

/// Type check an instanced model (after instantiation) and mint its proof.
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
/// This is the sole mint of [`crate::TypedInstancedTree`]. The overlay is
/// consumed by value: type checking annotates its own working copy and, on
/// zero errors, seals it inside the immutable proof. No mutable predecessor
/// alias crosses this boundary in either direction; failure exposes neither
/// a partial overlay nor a proof.
///
/// # Arguments
///
/// * `resolved` - The Resolve-issued proof carrying the class tree
/// * `overlay` - The instance overlay with modification values, by value
/// * `model_name` - The qualified model name being compiled
///
/// # Returns
///
/// The minted [`crate::TypedInstancedTree`] on success, or diagnostics on
/// error.
pub fn typecheck_instanced_tree(
    resolved: &ResolvedTree,
    overlay: InstanceOverlay,
    model_name: &str,
) -> Result<crate::TypedInstancedTree, Diagnostics> {
    TypeChecker::new().check_instanced(resolved, overlay, model_name)
}

/// Unit-fixture entry point for deliberately hand-built or mutated ClassTree
/// values. Production code cannot call this path and therefore cannot bypass
/// the Resolve-issued semantic-catalog proof.
#[cfg(test)]
pub(crate) fn typecheck_instanced_test_projection(
    tree: &ClassTree,
    overlay: &mut InstanceOverlay,
    model_name: &str,
) -> Result<(), Diagnostics> {
    let diagnostics = TypeChecker::new().check_instanced_test_projection(tree, overlay, model_name);

    if diagnostics.has_errors() {
        Err(diagnostics)
    } else {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::{TypeChecker, collect_structural_refs_from_equations, typecheck};
    use rumoca_ir_ast::{ClassTree, ParsedTree};
    use rumoca_phase_parse::parse_to_ast;
    use rumoca_phase_resolve::{ResolvedTree, resolve};
    use std::path::{Path, PathBuf};
    use std::sync::Arc;

    fn resolve_source(source: &str) -> ResolvedTree {
        let file_name = "<one-shot-typecheck-test>";
        let definition = parse_to_ast(source, file_name).expect("parse should succeed");
        let mut tree = ClassTree::from_parsed(definition);
        tree.source_map.add(file_name, source);
        resolve(ParsedTree::new(tree)).expect("resolve should succeed")
    }

    fn rust_sources_below(root: &Path) -> Vec<(PathBuf, String)> {
        let mut pending = vec![root.to_path_buf()];
        let mut sources = Vec::new();
        while let Some(directory) = pending.pop() {
            let entries = std::fs::read_dir(&directory)
                .unwrap_or_else(|error| panic!("cannot read {}: {error}", directory.display()));
            for entry in entries {
                let entry = entry.expect("source-tree directory entry must be readable");
                collect_rust_source_path(entry.path(), &mut pending, &mut sources);
            }
        }
        sources.sort_by(|left, right| left.0.cmp(&right.0));
        sources
    }

    fn collect_rust_source_path(
        path: PathBuf,
        pending: &mut Vec<PathBuf>,
        sources: &mut Vec<(PathBuf, String)>,
    ) {
        if path.is_dir() {
            pending.push(path);
            return;
        }
        if path.extension().is_none_or(|extension| extension != "rs") {
            return;
        }
        let source = std::fs::read_to_string(&path)
            .unwrap_or_else(|error| panic!("cannot read Rust source {}: {error}", path.display()));
        sources.push((path, source));
    }

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

    #[test]
    fn sequential_public_alias_typechecks_cannot_observe_checker_state() {
        let invalid = resolve_source(
            r#"
            type First = Real;
            model Bad
                First aliasValue;
                Real invalid(start = true);
            end Bad;
            "#,
        );
        let diagnostics = typecheck(invalid)
            .expect_err("the first root must issue its local modifier type error");
        assert!(
            diagnostics
                .iter()
                .any(|diagnostic| diagnostic.code.as_deref() == Some("ET002")),
            "expected the first root's ET002 diagnostic: {diagnostics:?}",
        );

        let valid = resolve_source(
            r#"
            type Second = Integer;
            model Good
                Second value(start = 1);
            end Good;
            "#,
        );
        typecheck(valid).expect(
            "a subsequent public phase entry must have fresh alias identities and diagnostics",
        );
    }

    #[test]
    fn mutated_alias_identity_fails_at_the_one_shot_issuing_boundary() {
        let mut tree = resolve_source(
            r#"
            type Exact = Real;
            model Test
                Exact value;
            end Test;
            "#,
        )
        .inner()
        .clone();
        let alias = tree
            .definitions
            .classes
            .get_mut("Exact")
            .expect("fixture has an alias declaration");
        let base = alias
            .extends
            .first_mut()
            .expect("an alias has one base declaration");
        base.base_def_id = None;
        base.base_name.def_id = None;
        base.base_name.name[0].text = Arc::from("Missing");

        let diagnostics = TypeChecker::new().check(&mut tree);
        assert!(
            diagnostics.iter().any(|diagnostic| {
                diagnostic.code.as_deref() == Some("ET014")
                    && diagnostic.message.contains("Missing")
            }),
            "a malformed exact alias must issue a typed error immediately: {diagnostics:?}",
        );
    }

    #[test]
    fn one_shot_checker_api_and_removed_alias_cache_are_tombstoned() {
        let source_root = Path::new(env!("CARGO_MANIFEST_DIR")).join("src");
        let checker_source = std::fs::read_to_string(source_root.join("lib.rs"))
            .expect("checker source must be readable");
        let instanced_source = std::fs::read_to_string(source_root.join("instanced.rs"))
            .expect("instanced checker source must be readable");

        assert!(checker_source.contains("\nstruct TypeChecker"));
        assert!(checker_source.contains("fn check(self"));
        assert!(checker_source.contains("fn check_detached(mut self"));
        assert!(instanced_source.contains("fn check_instanced(\n        self"));
        assert!(instanced_source.contains("fn check_instanced_detached(\n        mut self"));
        let sources = rust_sources_below(&source_root);
        for removed in [
            concat!("pub ", "struct TypeChecker"),
            concat!("pub(crate) ", "struct TypeChecker"),
            concat!("impl Default ", "for TypeChecker"),
            concat!("deferred_alias_", "errors"),
            concat!("resolve_alias_target_", "or_defer"),
            concat!("resolve_", "alias_root"),
            concat!("aliased: TypeId::", "UNKNOWN"),
            concat!("unwrap_or(TypeId::", "UNKNOWN)"),
            concat!("take_", "diagnostics"),
        ] {
            let offenders = sources
                .iter()
                .filter_map(|(path, source)| source.contains(removed).then_some(path))
                .collect::<Vec<_>>();
            assert!(offenders.is_empty(), "found `{removed}` in {offenders:?}");
        }
    }
}
