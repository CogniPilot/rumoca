//! Exact restatement of a callable reference's structured path.
//!
//! A callable reference carries two spellings: the structured `ComponentRef`
//! parts (its use-site path, one exact `DefId` per segment) and the rendered
//! `VarName` (its lookup-qualified path, the key the collected function table
//! uses). This module closes the gap between them without inventing identity.

use rumoca_ir_ast as ast;

/// Restate `reference`'s structured path so it spells the rendered callable
/// name, when the rendered name names the same declarations as that path under
/// their own declared names, qualified by the enclosing scopes of its root
/// segment.
///
/// Resolve records a callable's use-site spelling in the structured parts and
/// its lookup-qualified spelling (MLS §5.3, §13.2) as the rendered name, so a
/// reference written `Concrete.Element` inside `package P` renders
/// `P.Concrete.Element`. Both name the same declaration, but the rendered form
/// is the exposure identity the collected function table is keyed by, and only
/// the rendered form separates two exposures of one declaration
/// (`P.Generic.Element` versus `P.Concrete.Element`), so it cannot be dropped.
///
/// Two use-site spellings split from the rendered name, and both are closed
/// here without inventing identity:
///
/// * an enclosing-scope-relative path, closed by prepending exact enclosing
///   `DefId`s taken from the class index; and
/// * a renaming import (MLS §13.2.1) such as
///   `import generator = Modelica.Math.Random.Generators.Xorshift128plus`,
///   where the root segment resolved to the imported declaration but spells
///   the local alias. The alias is replaced by that same root `DefId`'s own
///   declared name, so the segment keeps its exact resolved identity, span,
///   and subscripts and only stops spelling the alias.
///
/// Every restatement is admitted only when the rebuilt path spells the
/// rendered name exactly. A reference whose rendered name disagrees for any
/// other reason keeps its structured path and is left for the
/// callable-identity check to reject.
pub(super) fn scope_qualified_reference(
    class_index: &ast::ClassDefIndex<'_>,
    reference: &rumoca_core::Reference,
) -> Option<rumoca_core::Reference> {
    if reference.resolved_function().is_some() {
        return None;
    }
    let component_ref = reference.component_ref()?;
    let rendered = reference.var_name();
    let candidate = scope_qualified_path(class_index, component_ref, rendered)?;
    Some(reference.with_rewritten_component_reference(rendered.as_str(), candidate))
}

/// Restate a callable's structured path so it spells `rendered` exactly.
///
/// This is the path half of [`scope_qualified_reference`], shared with the
/// statement call form. A statement call names its callee by a bare
/// `ComponentReference` with no rendered companion, so the collected function
/// table's key is supplied by the caller instead of read off a `Reference`.
pub(super) fn scope_qualified_path(
    class_index: &ast::ClassDefIndex<'_>,
    component_ref: &rumoca_core::ComponentReference,
    rendered: &rumoca_core::VarName,
) -> Option<rumoca_core::ComponentReference> {
    if component_ref.to_var_name() == *rendered {
        return None;
    }
    let root = component_ref.parts().first()?;
    let mut enclosing = Vec::new();
    let mut current = class_index.parent_def_id(root.def_id);
    while let Some(def_id) = current {
        enclosing.push(def_id);
        current = class_index.parent_def_id(def_id);
    }
    // The path as written needs at least one enclosing scope to change; the
    // import-alias restatement already changes the root segment's spelling, so
    // it also has to be tried unqualified.
    let mut spellings = vec![(component_ref.parts().to_vec(), 1)];
    if let Some(declared) = class_index
        .local_name(root.def_id)
        .filter(|declared| *declared != root.ident)
    {
        let mut parts = component_ref.parts().to_vec();
        parts[0].ident = declared.to_string();
        spellings.push((parts, 0));
    }
    for (tail, lowest_depth) in spellings {
        // `enclosing` runs innermost-first; try every scope depth so a rendered
        // name that names only part of the chain still reconciles exactly.
        for depth in lowest_depth..=enclosing.len() {
            let mut parts = Vec::with_capacity(depth + tail.len());
            for def_id in enclosing[..depth].iter().rev() {
                let scope = class_index.get(*def_id)?;
                parts.push(rumoca_core::ComponentRefPart {
                    ident: scope.name.text.to_string(),
                    span: root.span,
                    subs: Vec::new(),
                    def_id: *def_id,
                });
            }
            parts.extend(tail.iter().cloned());
            let candidate = component_ref.with_replaced_parts(parts).ok()?;
            if candidate.to_var_name() == *rendered {
                return Some(candidate);
            }
        }
    }
    None
}

#[cfg(test)]
mod renaming_import_callable_tests {
    //! `Modelica.Blocks.Interfaces.PartialNoise` writes
    //! `import generator = Modelica.Math.Random.Generators.Xorshift128plus`
    //! and then calls `generator.initialState(...)`, so the structured path
    //! spells the one-segment alias while the rendered name spells the four
    //! enclosing packages plus the declaration. Before the alias restatement
    //! `Modelica.Blocks.Examples.Noise.AutomaticSeed` failed flatten with
    //! `EF019 inconsistent resolved function reference: rendered
    //! Modelica.Math.Random.Generators.Xorshift128plus.initialState, structured
    //! generator.initialState`.

    use rumoca_ir_ast as ast;
    use rumoca_ir_flat as flat;

    const SOURCE: &str = r"
package Lib
  package Generators
    package Fast
      constant Integer nState = 4;
      function initialState
        input Integer localSeed;
        output Integer state[nState];
      algorithm
        state := fill(localSeed, nState);
      end initialState;
    end Fast;

    package Slow
      constant Integer nState = 2;
      function initialState
        input Integer localSeed;
        output Integer state[nState];
      algorithm
        state := fill(localSeed, nState);
      end initialState;
    end Slow;
  end Generators;

  model Noise
    import fast = Lib.Generators.Fast;
    import slow = Lib.Generators.Slow;
    Integer fastState[fast.nState];
    Integer slowState[slow.nState];
    Real y;
  initial equation
    fastState = fast.initialState(3);
    slowState = slow.initialState(5);
  equation
    y = time;
  end Noise;

  model Top
    Noise noise;
  end Top;
end Lib;
";

    fn flatten_source(model: &str) -> flat::Model {
        let file_name = "<renaming_import_callable_tests>";
        let stored =
            rumoca_phase_parse::parse_to_ast(SOURCE, file_name).expect("fixture should parse");
        let mut tree = ast::ClassTree::from_parsed(stored);
        tree.source_map.add(file_name, SOURCE);
        let resolved = rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree))
            .expect("fixture should resolve");
        let instanced = rumoca_phase_instantiate::instantiate(resolved, model)
            .expect("fixture should instantiate");
        let ast::InstancedTree { tree, mut overlay } = instanced;
        rumoca_phase_typecheck::typecheck_instanced(&tree, &mut overlay, model)
            .expect("fixture should typecheck");
        crate::flatten_ref(&tree, &overlay, model).expect("fixture should flatten")
    }

    fn output_dimensions(model: &flat::Model, function: &str) -> Vec<i64> {
        let collected = model
            .functions
            .iter()
            .find(|(name, _)| name.as_str() == function)
            .map(|(_, collected)| collected)
            .unwrap_or_else(|| panic!("no collected function `{function}`"));
        let output = collected
            .outputs
            .first()
            .unwrap_or_else(|| panic!("`{function}` has no output"));
        output.effective_type.dimensions().to_vec()
    }

    #[test]
    fn renaming_import_calls_collect_under_their_declaration_path() {
        let model = flatten_source("Lib.Top");
        // Each alias names a different declaration, so both have to reach the
        // collected table under their own package path (MLS §13.2.1).
        let collected: Vec<&str> = model.functions.keys().map(|key| key.as_str()).collect();
        assert!(
            collected.contains(&"Lib.Generators.Fast.initialState"),
            "expected the aliased fast generator, got {collected:?}"
        );
        assert!(
            collected.contains(&"Lib.Generators.Slow.initialState"),
            "expected the aliased slow generator, got {collected:?}"
        );
    }

    #[test]
    fn same_leaf_package_constants_materialize_per_declaration() {
        let model = flatten_source("Lib.Top");
        // `Fast.nState` and `Slow.nState` share a leaf name; each output shape
        // must be materialized from the constant of its own package.
        assert_eq!(
            output_dimensions(&model, "Lib.Generators.Fast.initialState"),
            vec![4]
        );
        assert_eq!(
            output_dimensions(&model, "Lib.Generators.Slow.initialState"),
            vec![2]
        );
        let dims_of = |name: &str| {
            model
                .variables
                .iter()
                .find(|(var_name, _)| var_name.as_str() == name)
                .map(|(_, variable)| variable.dims.clone())
                .unwrap_or_else(|| panic!("no flat variable `{name}`"))
        };
        assert_eq!(dims_of("noise.fastState"), vec![4]);
        assert_eq!(dims_of("noise.slowState"), vec![2]);
    }
}

#[cfg(test)]
mod statement_callable_tests {
    //! A statement call names its callee by a bare `ComponentReference`, so a
    //! call written inside the class that declares the callee spells one
    //! segment while the collected table is keyed by the declaration path.
    //!
    //! `Modelica.Blocks.Sources.BooleanTable` writes `isValidTable(table)` in
    //! its initial algorithm and `Modelica.Blocks.Examples.BooleanNetwork1`
    //! failed the DAE with `ED013 a call statement to isValidTable has no
    //! checked initialization owner`, because reachability read the two
    //! spellings as different callables and pruned the collected function.

    use rumoca_ir_ast as ast;
    use rumoca_ir_flat as flat;

    const SOURCE: &str = r"
package Lib
  model Table
    parameter Real table[:] = {1.0, 2.0};
  protected
    function isValidTable
      input Real t[:];
    algorithm
      assert(size(t, 1) > 0, 'empty table');
    end isValidTable;

    function bounds
      input Real t[:];
      output Real low;
      output Real high;
    algorithm
      low := t[1];
      high := t[size(t, 1)];
    end bounds;
  public
    Real lo;
    Real hi;
    Real y;
  initial algorithm
    isValidTable(table);
  algorithm
    (lo, hi) := bounds(table);
  equation
    y = time;
  end Table;

  model Top
    Table plant;
  end Top;
end Lib;
";

    fn flatten_source(model: &str) -> flat::Model {
        let file_name = "<statement_callable_tests>";
        let source = SOURCE.replace('\'', "\"");
        let stored =
            rumoca_phase_parse::parse_to_ast(&source, file_name).expect("fixture should parse");
        let mut tree = ast::ClassTree::from_parsed(stored);
        tree.source_map.add(file_name, &source);
        let resolved = rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree))
            .expect("fixture should resolve");
        let instanced = rumoca_phase_instantiate::instantiate(resolved, model)
            .expect("fixture should instantiate");
        let ast::InstancedTree { tree, mut overlay } = instanced;
        rumoca_phase_typecheck::typecheck_instanced(&tree, &mut overlay, model)
            .expect("fixture should typecheck");
        crate::flatten_ref(&tree, &overlay, model).expect("fixture should flatten")
    }

    fn statement_callees(statements: &[rumoca_core::Statement]) -> Vec<String> {
        statements
            .iter()
            .filter_map(|statement| match statement {
                rumoca_core::Statement::FunctionCall { comp, .. } => {
                    Some(comp.to_var_name().as_str().to_string())
                }
                _ => None,
            })
            .collect()
    }

    #[test]
    fn a_zero_output_statement_callee_reaches_the_collected_table() {
        let model = flatten_source("Lib.Top");
        let collected: Vec<&str> = model.functions.keys().map(|key| key.as_str()).collect();
        assert!(
            collected.contains(&"Lib.Table.isValidTable"),
            "a zero-output statement call must keep its callee collected, got {collected:?}"
        );
    }

    #[test]
    fn a_multi_output_statement_callee_reaches_the_collected_table() {
        let model = flatten_source("Lib.Top");
        let collected: Vec<&str> = model.functions.keys().map(|key| key.as_str()).collect();
        assert!(
            collected.contains(&"Lib.Table.bounds"),
            "a multi-output statement call must keep its callee collected, got {collected:?}"
        );
    }

    /// Every consumer that resolves a statement callee does so by its rendered
    /// name, so the call site has to spell the collected table's key.
    #[test]
    fn statement_callees_spell_their_collected_key() {
        let model = flatten_source("Lib.Top");
        let mut callees = Vec::new();
        for algorithm in model.algorithms.iter().chain(&model.initial_algorithms) {
            callees.extend(statement_callees(&algorithm.statements));
        }
        callees.sort();
        assert_eq!(
            callees,
            vec![
                "Lib.Table.bounds".to_string(),
                "Lib.Table.isValidTable".to_string()
            ],
            "statement callees must spell the collected declaration path"
        );
    }
}
