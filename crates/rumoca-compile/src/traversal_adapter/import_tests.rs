use super::*;
use crate::Session;
use rumoca_ir_ast as ast;
use rumoca_phase_parse::parse_to_ast;
use rumoca_phase_resolve::{ResolvedTree, resolve};

const INHERITED_IMPORTS: &str = r#"
package Outer
  package Base
    constant Integer n = 3;
    type Choice = enumeration(off, on);
  end Base;

  package Mid
    extends Base;
  end Mid;

  model Single
    import Outer.Mid.n;
    import Outer.Mid.Choice;
    parameter Choice choice = Choice.on;
    Real x[n];
    Real enabled if choice == Choice.on;
  end Single;

  model Selective
    import Outer.Mid.{n, Choice};
    parameter Choice choice = Choice.on;
    Real x[n];
    Real enabled if choice == Choice.on;
  end Selective;

  model Wildcard
    import Outer.Mid.*;
    parameter Choice choice = Choice.on;
    Real x[n];
    Real enabled if choice == Choice.on;
  end Wildcard;

  model EnumSingle
    import Outer.Mid.Choice;
    parameter Choice choice = Choice.on;
    Real enabled if choice == Choice.on;
  end EnumSingle;

  model EnumSelective
    import Outer.Mid.{Choice};
    parameter Choice choice = Choice.on;
    Real enabled if choice == Choice.on;
  end EnumSelective;

  model EnumWildcard
    import Outer.Mid.*;
    parameter Choice choice = Choice.on;
    Real enabled if choice == Choice.on;
  end EnumWildcard;
end Outer;
"#;

fn resolved_tree(source: &str) -> ResolvedTree {
    let file = "<inherited_import_closure>";
    let stored = parse_to_ast(source, file).expect("fixture parses");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(file, source);
    resolve(ast::ParsedTree::new(tree)).expect("fixture resolves")
}

fn dependencies(tree: &ResolvedTree, class_name: &str) -> IndexSet<String> {
    let tree = tree.inner();
    let index = ast::ClassDefIndex::from_tree(tree);
    let substitutions = RedeclareSubstitutions::from_index(&index);
    let class = index
        .get_by_qualified_name(class_name)
        .expect("fixture class exists");
    collect_class_dependencies(tree, &index, &substitutions, class, class_name)
}

#[test]
fn every_import_form_retains_the_complete_resolved_identity_route() {
    let tree = resolved_tree(INHERITED_IMPORTS);
    for model in ["Outer.Single", "Outer.Selective", "Outer.Wildcard"] {
        let deps = dependencies(&tree, model);
        for required in ["Outer", "Outer.Mid", "Outer.Base"] {
            assert!(
                deps.contains(required),
                "{model} lost Resolve-issued import route owner {required}: {deps:?}"
            );
        }
    }
}

#[test]
fn inherited_enum_imports_survive_strict_pruning_end_to_end() {
    for model in [
        "Outer.EnumSingle",
        "Outer.EnumSelective",
        "Outer.EnumWildcard",
    ] {
        let mut session = Session::default();
        session
            .add_document("inherited_imports.mo", INHERITED_IMPORTS)
            .expect("fixture document parses");
        session
            .compile_model_flat_strict_reachable_uncached_with_recovery(model)
            .unwrap_or_else(|error| panic!("{model} compiles through strict closure: {error}"));
    }
}

#[test]
fn modified_inherited_quoted_import_dimensions_reach_flat_by_identity() {
    let source = r#"
    package P
      package Base
        constant Integer m = 2;
        constant Integer 'n.x' = m + 1;
      end Base;
      package DerivedSibling extends Base(m = 4); end DerivedSibling;
      package DerivedDirect extends Base('n.x' = 7); end DerivedDirect;
      model Sibling
        import N = P.DerivedSibling.'n.x';
        Real x[N];
      end Sibling;
      model Direct
        import N = P.DerivedDirect.'n.x';
        Real x[N];
      end Direct;
    end P;
    "#;
    for (model, expected) in [("P.Sibling", 5), ("P.Direct", 7)] {
        let mut session = Session::default();
        session
            .add_document("quoted_inherited_import.mo", source)
            .expect("fixture document parses");
        let flat = session
            .compile_model_flat_strict_reachable_uncached_with_recovery(model)
            .unwrap_or_else(|error| panic!("{model} compiles through strict Flat: {error}"));
        let x = flat
            .variables
            .get(&rumoca_core::VarName::new("x"))
            .unwrap_or_else(|| panic!("{model} retains x in Flat"));
        assert_eq!(x.dims, vec![expected], "{model} specialized wrong target");
    }
}

#[test]
fn noninteger_imported_dimension_fails_typed_instead_of_deferring() {
    let error = strict_error(
        r#"
        package P
          package Values
            constant Real n = 3.0;
          end Values;
          model M
            import P.Values.n;
            Real x[n];
          end M;
        end P;
        "#,
        "P.M",
    );
    assert!(
        error.contains("failed in Typecheck") || error.contains("failed in Flatten"),
        "invalid structural dimension must surface as a typed phase failure: {error}"
    );
}

fn strict_error(source: &str, model: &str) -> String {
    let mut session = Session::default();
    session
        .add_document("invalid_inherited_import.mo", source)
        .expect("negative fixture parses");
    session
        .compile_model_flat_strict_reachable_uncached_with_recovery(model)
        .expect_err("negative import fixture must fail")
        .to_string()
}

#[test]
fn invalid_inherited_imports_fail_closed() {
    let ambiguous = strict_error(
        r#"
        package P
          package A constant Integer n = 1; end A;
          package B constant Integer n = 2; end B;
          model M
            import P.A.*;
            import P.B.*;
            Real x[n];
          end M;
        end P;
        "#,
        "P.M",
    );
    assert!(ambiguous.contains("ambiguous") || ambiguous.contains("more than one package"));

    let missing = strict_error(
        r#"
        package P
          package Base constant Integer n = 1; end Base;
          package Derived extends Base; end Derived;
          model M import P.Derived.missing; Real x; end M;
        end P;
        "#,
        "P.M",
    );
    assert!(missing.contains("unresolved import"));

    let cycle = strict_error(
        r#"
        package P
          package A extends B; constant Integer n = 1; end A;
          package B extends A; end B;
          model M import P.A.n; Real x[n]; end M;
        end P;
        "#,
        "P.M",
    );
    assert!(cycle.contains("circular inheritance"));

    let not_inherited = strict_error(
        r#"
        package P
          package Values constant Integer n = 1; end Values;
          model Base import P.Values.n; Real x[n]; end Base;
          model Derived extends Base; Real y[n]; end Derived;
        end P;
        "#,
        "P.Derived",
    );
    assert!(not_inherited.contains("unresolved") || not_inherited.contains("not found"));
}

#[test]
fn a_direct_member_shadows_the_inherited_import_target() {
    let source = r#"
    package P
      package Base constant Integer n = 1; end Base;
      package Derived
        extends Base;
        constant Integer n = 4;
      end Derived;
      model M import P.Derived.n; Real x[n]; end M;
    end P;
    "#;
    let tree = resolved_tree(source);
    let deps = dependencies(&tree, "P.M");
    assert!(deps.contains("P.Derived"));
    assert!(!deps.contains("P.Base"), "direct shadow must win: {deps:?}");
}
