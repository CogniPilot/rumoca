//! Which `for` families flatten certifies as continuous algebraic owners and
//! which keep every row (SPEC_0043 §6c).
//!
//! The proof reads the still-symbolic body and the declarations of the class
//! occurrence, never the expanded rows: a family is an owner only when every
//! target is a continuous-time `Real` of its own occurrence that no `der`
//! equation reads, its nested loop ranges are independent of enclosing
//! binders, and its body reads only arithmetic, references, literals, arrays,
//! ranges, unfiltered comprehensions, and smooth builtins.

use rumoca_ir_ast as ast;
use rumoca_ir_flat as flat;
use rumoca_ir_flat::FamilyInteriors;

fn flatten_source(source: &str, model: &str) -> flat::Model {
    let file_name = "<continuous_algebraic_families>";
    let stored = rumoca_phase_parse::parse_to_ast(source, file_name).expect("source parses");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(file_name, source);
    let resolved =
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).expect("source resolves");
    let instanced =
        rumoca_phase_instantiate::instantiate(resolved, model).expect("model instantiates");
    let ast::InstancedTree { tree, mut overlay } = instanced;
    rumoca_phase_typecheck::typecheck_instanced(&tree, &mut overlay, model)
        .expect("instanced model typechecks");
    rumoca_phase_flatten::flatten_ref(&tree, &overlay, model).expect("model flattens")
}

/// The interiors of the families of `Probe`, whose declarations and
/// equations are given; `x` is a state every body may read.
fn families(declarations: &str, equations: &str) -> Vec<(FamilyInteriors, usize)> {
    let source = format!(
        "model Probe\n  Real x(start = 1, fixed = true);\n{declarations}\nequation\n  der(x) = -x;\n{equations}\nend Probe;\n"
    );
    let model = flatten_source(&source, "Probe");
    model
        .structured_equations
        .iter()
        .map(|family| (family.interiors, family.domain.binders.len()))
        .collect()
}

fn interiors(declarations: &str, equations: &str) -> Vec<FamilyInteriors> {
    families(declarations, equations)
        .into_iter()
        .map(|(interiors, _)| interiors)
        .collect()
}

fn owned(declarations: &str, equations: &str) -> bool {
    interiors(declarations, equations).contains(&FamilyInteriors::ContinuousAlgebraic)
}

#[test]
fn an_arithmetic_family_over_a_continuous_real_is_an_owner() {
    assert_eq!(
        interiors(
            "  Real y[4];",
            "  for i in 1:4 loop y[i] = exp(-x)*i + sum({x, 2.0}); end for;"
        ),
        [FamilyInteriors::ContinuousAlgebraic]
    );
}

#[test]
fn a_target_a_derivative_equation_reads_keeps_its_rows() {
    assert_eq!(
        interiors(
            "  Real y[4];\n  Real r[4](each start = 0, each fixed = true);",
            "  for i in 1:4 loop y[i] = 2*x; end for;\n  for j in 1:4 loop der(r[j]) = -y[j]; end for;"
        ),
        [
            FamilyInteriors::Materialized,
            FamilyInteriors::StateDerivative
        ]
    );
    assert!(owned(
        "  Real y[4];\n  Real r[4](each start = 0, each fixed = true);",
        "  for i in 1:4 loop y[i] = 2*x; end for;\n  for j in 1:4 loop der(r[j]) = -x; end for;"
    ));
}

#[test]
fn a_nested_range_depending_on_an_outer_binder_is_no_owner_of_the_nest() {
    let nested = families(
        "  Real t[3,3];",
        "  for i in 1:3 loop for j in i:3 loop t[i,j] = x*i*j; end for; end for;\n  for i in 2:3 loop for j in 1:i-1 loop t[i,j] = x + i + j; end for; end for;",
    );
    // The outer binder is unrolled: each inner loop is a one-binder family of
    // its own, never one two-binder owner of the nest.
    assert!(
        nested.iter().all(|(_, binders)| *binders == 1),
        "{nested:?}"
    );
    let independent = interiors(
        "  Real t[3,3];",
        "  for i in 1:3 loop for j in 1:3 loop t[i,j] = x*i*j; end for; end for;",
    );
    assert_eq!(independent, [FamilyInteriors::ContinuousAlgebraic]);
}

#[test]
fn event_memory_call_and_relation_bodies_keep_their_rows() {
    for body in [
        "if x > 0.5 then 1 else 2",
        "abs(x) + i",
        "floor(x*i)",
        "min(x, i)",
        "smooth(1, x*i)",
        "noEvent(x)",
        "twice(x)",
    ] {
        let functions =
            "  function twice input Real u; output Real v; algorithm v := 2*u; end twice;\n";
        let declarations = format!("{functions}  Real y[4];");
        assert_eq!(
            interiors(
                &declarations,
                &format!("  for i in 1:4 loop y[i] = {body}; end for;")
            ),
            [FamilyInteriors::Materialized],
            "{body}"
        );
    }
}

#[test]
fn only_continuous_real_declarations_of_the_occurrence_are_targets() {
    for declaration in ["  discrete Real y[4];", "  Integer y[4];"] {
        assert_eq!(
            interiors(declaration, "  for i in 1:4 loop y[i] = i; end for;"),
            [FamilyInteriors::Materialized],
            "{declaration}"
        );
    }
}
