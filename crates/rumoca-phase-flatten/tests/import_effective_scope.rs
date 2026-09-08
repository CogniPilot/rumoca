//! Full-pipeline coverage for the effective-import view Flatten consumes.
//!
//! Every fixture runs parse, resolve, instantiate, typecheck, and flatten, so
//! each assertion witnesses the import decision that actually reaches Flat:
//! imports resolve in the scope that wrote the expression (MLS §13.2), a
//! wildcard reaches a package's inherited export view, one declaration
//! reachable through two spellings still binds, and an ambiguous name is a
//! typed refusal rather than a silent binding or a silent absence.

use rumoca_core::ExpressionVisitor;
use rumoca_ir_ast as ast;

const SOURCE_NAME: &str = "<import_effective_scope>";

#[derive(Default)]
struct LiteralCollector {
    integers: Vec<i64>,
    reals: Vec<f64>,
    variables: Vec<String>,
}

impl ExpressionVisitor for LiteralCollector {
    fn visit_expression(&mut self, expression: &rumoca_core::Expression) {
        match expression {
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(value),
                ..
            } => self.integers.push(*value),
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(value),
                ..
            } => self.reals.push(*value),
            rumoca_core::Expression::VarRef { name, .. } => {
                self.variables.push(name.to_string());
            }
            _ => {}
        }
        self.walk_expression(expression);
    }
}

fn flatten_pipeline(source: &str, model: &str) -> Result<rumoca_ir_flat::Model, String> {
    let stored =
        rumoca_phase_parse::parse_to_ast(source, SOURCE_NAME).map_err(|e| e.to_string())?;
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(SOURCE_NAME, source);
    let resolved = rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree))
        .map_err(|e| format!("resolve: {e:?}"))?;
    let overlay =
        match rumoca_phase_instantiate::instantiate_model_with_outcome(resolved.inner(), model) {
            rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
            rumoca_phase_instantiate::InstantiationOutcome::NeedsInner {
                missing_inners, ..
            } => {
                return Err(format!(
                    "instantiate: unexpectedly needs inner declarations: {missing_inners:?}"
                ));
            }
            rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
                return Err(format!("instantiate: {error}"));
            }
        };
    let typed = rumoca_phase_typecheck::typecheck_instanced_tree(&resolved, overlay, model)
        .map_err(|e| format!("typecheck: {e:?}"))?;
    rumoca_phase_flatten::flatten_typed(typed, rumoca_phase_flatten::FlattenOptions::default())
        .map_err(|e| format!("flatten: {e}"))
}

fn equation_literals(model: &rumoca_ir_flat::Model) -> Vec<(Vec<String>, Vec<i64>, Vec<f64>)> {
    model
        .equations
        .iter()
        .map(|equation| {
            let mut collector = LiteralCollector::default();
            collector.visit_expression(&equation.residual);
            (collector.variables, collector.integers, collector.reals)
        })
        .collect()
}

/// A wildcard import reaches a member the imported package only inherits
/// (MLS §13.2: the wildcard covers the package's export view, which includes
/// inherited declarations).
#[test]
fn wildcard_import_reaches_inherited_member() {
    let source = r#"
package Base
  constant Real X = 1.0;
end Base;

package Wild
  extends Base;
end Wild;

model M
  import Wild.*;
  Real y;
equation
  y = X;
end M;
"#;
    let model = flatten_pipeline(source, "M").expect("inherited wildcard member must flatten");
    let equations = equation_literals(&model);
    assert!(
        equations
            .iter()
            .any(|(_, _, reals)| reals.contains(&1.0_f64)),
        "the inherited constant Base.X = 1.0 must reach Flat through `import Wild.*`, got {equations:?}"
    );
}

/// Imports are not inherited (MLS §13.2): an inherited equation binds through
/// the base class's own import, and the derived class's equation binds through
/// its own import, even when both spell the same short name.
#[test]
fn each_equation_binds_its_own_scopes_import() {
    let source = r#"
package BN
  constant Integer X = 1;
end BN;

package DN
  constant Integer X = 2;
end DN;

model B
  import BN.X;
  Integer yb;
equation
  yb = X;
end B;

model D
  extends B;
  import DN.X;
  Integer yd;
equation
  yd = X;
end D;
"#;
    let model = flatten_pipeline(source, "D").expect("per-scope import fixture must flatten");
    let equations = equation_literals(&model);
    let base_equation = equations
        .iter()
        .find(|(variables, _, _)| variables.iter().any(|name| name.ends_with("yb")))
        .expect("the inherited equation for yb must reach Flat");
    assert!(
        base_equation.1.contains(&1) && !base_equation.1.contains(&2),
        "the inherited equation must bind the base scope's BN.X = 1, got {base_equation:?}"
    );
    let derived_equation = equations
        .iter()
        .find(|(variables, _, _)| variables.iter().any(|name| name.ends_with("yd")))
        .expect("the derived equation for yd must reach Flat");
    assert!(
        derived_equation.1.contains(&2) && !derived_equation.1.contains(&1),
        "the derived equation must bind its own DN.X = 2, got {derived_equation:?}"
    );
}

/// The negative twin: a derived class using a short name with no import of
/// its own must fail, never silently bind the base class's import.
#[test]
fn derived_use_without_own_import_is_refused() {
    let source = r#"
package BN
  constant Integer X = 1;
end BN;

model B
  import BN.X;
  Integer yb;
equation
  yb = X;
end B;

model D
  extends B;
  Integer yd;
equation
  yd = X;
end D;
"#;
    let error = flatten_pipeline(source, "D")
        .expect_err("a derived class must not inherit the base class's import");
    assert!(
        error.starts_with("resolve:"),
        "the unimported name must already fail name resolution, got: {error}"
    );
}

/// One declaration reachable through two spellings still binds: `import
/// Wild.X` names the declaration Base.X through Wild's export view, and the
/// binding must project that identity instead of refusing to rewrite because
/// the spellings differ.
#[test]
fn import_through_reexported_spelling_binds_the_declaration() {
    let source = r#"
package Base
  constant Real X = 3.0;
end Base;

package Wild
  extends Base;
end Wild;

model M
  import Wild.X;
  Real y;
equation
  y = X;
end M;
"#;
    let model = flatten_pipeline(source, "M").expect("re-exported import spelling must flatten");
    let equations = equation_literals(&model);
    assert!(
        equations
            .iter()
            .any(|(_, _, reals)| reals.contains(&3.0_f64)),
        "Base.X = 3.0 must bind through the Wild.X spelling, got {equations:?}"
    );
}

/// A name supplied by more than one wildcard import is a typed refusal at its
/// use site (MLS §5.3.1), never a silent last-clause-wins binding.
#[test]
fn ambiguous_unqualified_import_use_is_refused() {
    let source = r#"
package P1
  constant Integer X = 1;
end P1;

package P2
  constant Integer X = 2;
end P2;

model M
  import P1.*;
  import P2.*;
  Integer y;
equation
  y = X;
end M;
"#;
    let error = flatten_pipeline(source, "M")
        .expect_err("a doubly wildcard-supplied name must be refused, not bound");
    assert!(
        error.to_lowercase().contains("ambiguous"),
        "the refusal must name the ambiguity, got: {error}"
    );
}

/// A wildcard-supplied name that is ambiguous among the imported package's
/// inherited declarations is a typed refusal at its use site, never a silent
/// absence.
#[test]
fn ambiguous_inherited_wildcard_member_use_is_refused() {
    let source = r#"
package A
  constant Integer X = 1;
end A;

package B
  constant Integer X = 2;
end B;

package Wild
  extends A;
  extends B;
end Wild;

model M
  import Wild.*;
  Integer y;
equation
  y = X;
end M;
"#;
    let error = flatten_pipeline(source, "M")
        .expect_err("an inherited-ambiguous wildcard member must be refused, not dropped");
    assert!(
        error.to_lowercase().contains("ambiguous"),
        "the refusal must name the ambiguity, got: {error}"
    );
}
