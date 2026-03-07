//! Canonicalize enum literal references in flattened expressions.
//!
//! MLS §4.9.5: enumeration literals are constants of an enumeration type.
//! Flat output should keep enum literal references resolvable by emitting
//! canonical qualification paths where possible.

use rumoca_ir_ast as ast;
use rumoca_ir_flat as flat;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::flat_eval::{canonicalize_enum_literal, looks_like_enum_literal_path};

/// Canonicalize enum literal references in the final flat model.
pub(crate) fn canonicalize_flat_enum_literals(
    flat: &mut flat::Model,
    tree: &ast::ClassTree,
    known_enum_values: &FxHashMap<String, String>,
) {
    let candidates = build_enum_literal_candidates(tree, known_enum_values);
    if candidates.is_empty() {
        return;
    }

    let variable_names: FxHashSet<String> =
        flat.variables.keys().map(ToString::to_string).collect();

    for var in flat.variables.values_mut() {
        canonicalize_optional_expr(var.binding.as_mut(), &candidates, &variable_names);
        canonicalize_optional_expr(var.start.as_mut(), &candidates, &variable_names);
        canonicalize_optional_expr(var.min.as_mut(), &candidates, &variable_names);
        canonicalize_optional_expr(var.max.as_mut(), &candidates, &variable_names);
        canonicalize_optional_expr(var.nominal.as_mut(), &candidates, &variable_names);
    }

    for eq in &mut flat.equations {
        canonicalize_expr(&mut eq.residual, &candidates, &variable_names);
    }
    for eq in &mut flat.initial_equations {
        canonicalize_expr(&mut eq.residual, &candidates, &variable_names);
    }

    for when_clause in &mut flat.when_clauses {
        canonicalize_expr(&mut when_clause.condition, &candidates, &variable_names);
        canonicalize_when_equations(&mut when_clause.equations, &candidates, &variable_names);
    }
}

fn build_enum_literal_candidates(
    tree: &ast::ClassTree,
    known_enum_values: &FxHashMap<String, String>,
) -> FxHashMap<String, String> {
    let mut candidates = known_enum_values.clone();

    for (def_id, qualified_name) in &tree.def_map {
        let Some(class_def) = tree.get_class_by_def_id(*def_id) else {
            continue;
        };
        if class_def.enum_literals.is_empty() {
            continue;
        }
        for literal in &class_def.enum_literals {
            let literal_name = literal.ident.text.as_ref();
            let full_path = format!("{}.{}", qualified_name, literal_name);
            candidates.entry(full_path.clone()).or_insert(full_path);
        }
    }

    candidates
}

fn canonicalize_optional_expr(
    expr: Option<&mut flat::Expression>,
    known_enums: &FxHashMap<String, String>,
    variable_names: &FxHashSet<String>,
) {
    if let Some(expr) = expr {
        canonicalize_expr(expr, known_enums, variable_names);
    }
}

fn canonicalize_expr(
    expr: &mut flat::Expression,
    known_enums: &FxHashMap<String, String>,
    variable_names: &FxHashSet<String>,
) {
    match expr {
        flat::Expression::VarRef { name, subscripts } => {
            canonicalize_var_ref_if_enum_literal(
                name,
                subscripts.is_empty(),
                known_enums,
                variable_names,
            );
            for sub in subscripts {
                if let flat::Subscript::Expr(expr) = sub {
                    canonicalize_expr(expr, known_enums, variable_names);
                }
            }
        }
        flat::Expression::Binary { lhs, rhs, .. } => {
            canonicalize_expr(lhs, known_enums, variable_names);
            canonicalize_expr(rhs, known_enums, variable_names);
        }
        flat::Expression::Unary { rhs, .. } => {
            canonicalize_expr(rhs, known_enums, variable_names);
        }
        flat::Expression::BuiltinCall { args, .. }
        | flat::Expression::FunctionCall { args, .. } => {
            for arg in args {
                canonicalize_expr(arg, known_enums, variable_names);
            }
        }
        flat::Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                canonicalize_expr(cond, known_enums, variable_names);
                canonicalize_expr(then_expr, known_enums, variable_names);
            }
            canonicalize_expr(else_branch, known_enums, variable_names);
        }
        flat::Expression::Array { elements, .. } | flat::Expression::Tuple { elements } => {
            for elem in elements {
                canonicalize_expr(elem, known_enums, variable_names);
            }
        }
        flat::Expression::Range { start, step, end } => {
            canonicalize_expr(start, known_enums, variable_names);
            if let Some(step) = step {
                canonicalize_expr(step, known_enums, variable_names);
            }
            canonicalize_expr(end, known_enums, variable_names);
        }
        flat::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            for index in indices {
                canonicalize_expr(&mut index.range, known_enums, variable_names);
            }
            canonicalize_expr(expr, known_enums, variable_names);
            if let Some(filter) = filter {
                canonicalize_expr(filter, known_enums, variable_names);
            }
        }
        flat::Expression::Index { base, subscripts } => {
            canonicalize_expr(base, known_enums, variable_names);
            for sub in subscripts {
                if let flat::Subscript::Expr(expr) = sub {
                    canonicalize_expr(expr, known_enums, variable_names);
                }
            }
        }
        flat::Expression::FieldAccess { base, .. } => {
            canonicalize_expr(base, known_enums, variable_names);
        }
        flat::Expression::Literal(_) | flat::Expression::Empty => {}
    }
}

fn canonicalize_var_ref_if_enum_literal(
    name: &mut flat::VarName,
    scalar_ref: bool,
    known_enums: &FxHashMap<String, String>,
    variable_names: &FxHashSet<String>,
) {
    if !scalar_ref {
        return;
    }

    let raw = name.as_str();
    if variable_names.contains(raw) || !looks_like_enum_literal_path(raw) {
        return;
    }

    let canonical = canonicalize_enum_literal(raw, known_enums);
    if canonical != raw {
        *name = flat::VarName::new(canonical);
    }
}

fn canonicalize_when_equations(
    equations: &mut [flat::WhenEquation],
    known_enums: &FxHashMap<String, String>,
    variable_names: &FxHashSet<String>,
) {
    for equation in equations.iter_mut() {
        match equation {
            flat::WhenEquation::Assign { value, .. } | flat::WhenEquation::Reinit { value, .. } => {
                canonicalize_expr(value, known_enums, variable_names);
            }
            flat::WhenEquation::Assert { condition, .. } => {
                canonicalize_expr(condition, known_enums, variable_names);
            }
            flat::WhenEquation::Terminate { .. } => {}
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                for (condition, branch_equations) in branches {
                    canonicalize_expr(condition, known_enums, variable_names);
                    canonicalize_when_equations(branch_equations, known_enums, variable_names);
                }
                canonicalize_when_equations(else_branch, known_enums, variable_names);
            }
            flat::WhenEquation::FunctionCallOutputs { function, .. } => {
                canonicalize_expr(function, known_enums, variable_names);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::Span;

    fn var_ref(name: &str) -> flat::Expression {
        flat::Expression::VarRef {
            name: flat::VarName::new(name),
            subscripts: vec![],
        }
    }

    #[test]
    fn rewrites_short_enum_literal_to_most_qualified_candidate() {
        let mut flat = flat::Model::new();
        flat.variables.insert(
            flat::VarName::new("y"),
            flat::Variable {
                binding: Some(var_ref("Init.NoInit")),
                ..flat::Variable::default()
            },
        );
        flat.equations.push(rumoca_ir_flat::Equation::new(
            flat::Expression::Binary {
                op: flat::OpBinary::Eq(flat::Token::default()),
                lhs: Box::new(var_ref("y")),
                rhs: Box::new(var_ref("Init.NoInit")),
            },
            Span::DUMMY,
            rumoca_ir_flat::EquationOrigin::Binding {
                variable: "y".to_string(),
            },
        ));

        let mut known_enums = FxHashMap::default();
        known_enums.insert("enumParam".to_string(), "TypesPkg.Init.NoInit".to_string());
        // Also include short form to ensure canonicalization prefers the most-qualified path.
        known_enums.insert("short".to_string(), "Init.NoInit".to_string());

        canonicalize_flat_enum_literals(&mut flat, &ast::ClassTree::new(), &known_enums);

        let binding = flat
            .variables
            .get(&flat::VarName::new("y"))
            .and_then(|var| var.binding.as_ref())
            .expect("binding should exist");
        let flat::Expression::VarRef { name, .. } = binding else {
            panic!("binding should remain a var ref");
        };
        assert_eq!(name.as_str(), "TypesPkg.Init.NoInit");

        let flat::Expression::Binary { rhs, .. } = &flat.equations[0].residual else {
            panic!("expected binary equation");
        };
        let flat::Expression::VarRef { name, .. } = rhs.as_ref() else {
            panic!("rhs should be enum literal var ref");
        };
        assert_eq!(name.as_str(), "TypesPkg.Init.NoInit");
    }

    #[test]
    fn does_not_rewrite_regular_variable_references() {
        let mut flat = flat::Model::new();
        flat.variables.insert(
            flat::VarName::new("Plant.Init.NoInit"),
            flat::Variable {
                binding: Some(flat::Expression::Literal(flat::Literal::Integer(1))),
                ..flat::Variable::default()
            },
        );
        flat.equations.push(rumoca_ir_flat::Equation::new(
            var_ref("Plant.Init.NoInit"),
            Span::DUMMY,
            rumoca_ir_flat::EquationOrigin::Binding {
                variable: "x".to_string(),
            },
        ));

        let mut known_enums = FxHashMap::default();
        known_enums.insert(
            "enumParam".to_string(),
            "Modelica.Blocks.Types.Init.NoInit".to_string(),
        );

        canonicalize_flat_enum_literals(&mut flat, &ast::ClassTree::new(), &known_enums);

        let flat::Expression::VarRef { name, .. } = &flat.equations[0].residual else {
            panic!("expected var ref");
        };
        assert_eq!(name.as_str(), "Plant.Init.NoInit");
    }
}
