//! Canonicalize enum literal references in flattened expressions.
//!
//! MLS §4.9.5: enumeration literals are constants of an enumeration type.
//! Flat output should keep enum literal references resolvable by emitting
//! canonical qualification paths where possible.

use rumoca_core::{ComponentPath, ExpressionRewriter, Reference, StatementRewriter, VarName};
use rumoca_ir_ast as ast;
use rumoca_ir_flat as flat;
use rustc_hash::{FxHashMap, FxHashSet};

use rumoca_eval_flat::phase_constant::looks_like_enum_literal_path;

/// Canonicalize enum literal references in the final flat model.
pub(crate) fn canonicalize_flat_enum_literals(
    flat: &mut flat::Model,
    tree: &ast::ClassTree,
    known_enum_values: &FxHashMap<String, String>,
) {
    let enum_literals = EnumLiteralIndex::new(tree, known_enum_values);
    if enum_literals.is_empty() {
        return;
    }

    let variable_names: FxHashSet<VarName> = flat.variables.keys().cloned().collect();

    for var in flat.variables.values_mut() {
        canonicalize_optional_expr(var.binding.as_mut(), &enum_literals, &variable_names);
        canonicalize_optional_expr(var.start.as_mut(), &enum_literals, &variable_names);
        canonicalize_optional_expr(var.min.as_mut(), &enum_literals, &variable_names);
        canonicalize_optional_expr(var.max.as_mut(), &enum_literals, &variable_names);
        canonicalize_optional_expr(var.nominal.as_mut(), &enum_literals, &variable_names);
    }

    for eq in &mut flat.equations {
        canonicalize_expr(&mut eq.residual, &enum_literals, &variable_names);
    }
    for eq in &mut flat.initial_equations {
        canonicalize_expr(&mut eq.residual, &enum_literals, &variable_names);
    }

    for when_clause in &mut flat.when_clauses {
        canonicalize_expr(&mut when_clause.condition, &enum_literals, &variable_names);
        canonicalize_when_equations(&mut when_clause.equations, &enum_literals, &variable_names);
    }

    for algorithm in &mut flat.algorithms {
        canonicalize_statements(&mut algorithm.statements, &enum_literals, &variable_names);
    }
    for algorithm in &mut flat.initial_algorithms {
        canonicalize_statements(&mut algorithm.statements, &enum_literals, &variable_names);
    }
}

#[derive(Debug, Clone)]
struct EnumLiteralSuffixMatch {
    value: String,
    segments: usize,
    ambiguous: bool,
}

#[derive(Debug, Default)]
struct EnumLiteralIndex {
    by_suffix: FxHashMap<ComponentPath, EnumLiteralSuffixMatch>,
}

impl EnumLiteralIndex {
    fn new(tree: &ast::ClassTree, known_enum_values: &FxHashMap<String, String>) -> Self {
        let mut index = Self::default();
        for value in known_enum_values.values() {
            index.record_value(value);
        }

        for (def_id, qualified_name) in &tree.def_map {
            let Some(class_def) = tree.get_class_by_def_id(*def_id) else {
                continue;
            };
            if class_def.enum_literals.is_empty() {
                continue;
            }
            for literal in &class_def.enum_literals {
                let literal_name = literal.ident.text.as_ref();
                index.record_value(&format!("{qualified_name}.{literal_name}"));
            }
        }

        index
    }

    fn is_empty(&self) -> bool {
        self.by_suffix.is_empty()
    }

    fn record_value(&mut self, value: &str) {
        let path = ComponentPath::from_flat_path(value);
        let segments = path.len();
        if segments < 2 {
            return;
        }

        for start in 0..segments.saturating_sub(1) {
            let Some(suffix) = path.suffix_from(start) else {
                continue;
            };
            self.record_suffix(suffix, value, segments);
        }
    }

    fn record_suffix(&mut self, suffix: ComponentPath, value: &str, segments: usize) {
        match self.by_suffix.entry(suffix) {
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(EnumLiteralSuffixMatch {
                    value: value.to_string(),
                    segments,
                    ambiguous: false,
                });
            }
            std::collections::hash_map::Entry::Occupied(mut entry) => {
                let current = entry.get_mut();
                if segments > current.segments {
                    *current = EnumLiteralSuffixMatch {
                        value: value.to_string(),
                        segments,
                        ambiguous: false,
                    };
                } else if segments == current.segments && current.value != value {
                    current.ambiguous = true;
                }
            }
        }
    }

    fn canonicalize<'index>(&'index self, literal: &Reference) -> Option<&'index str> {
        let path = reference_component_path(literal)?;
        if path.len() < 2 {
            return None;
        }

        for start in 0..path.len().saturating_sub(1) {
            let Some(suffix) = path.suffix_from(start) else {
                continue;
            };
            let Some(candidate) = self.by_suffix.get(&suffix) else {
                continue;
            };
            if !candidate.ambiguous {
                return Some(candidate.value.as_str());
            }
        }

        None
    }
}

fn canonicalize_optional_expr(
    expr: Option<&mut rumoca_core::Expression>,
    enum_literals: &EnumLiteralIndex,
    variable_names: &FxHashSet<VarName>,
) {
    if let Some(expr) = expr {
        canonicalize_expr(expr, enum_literals, variable_names);
    }
}

fn canonicalize_expr(
    expr: &mut rumoca_core::Expression,
    enum_literals: &EnumLiteralIndex,
    variable_names: &FxHashSet<VarName>,
) {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            canonicalize_var_ref_if_enum_literal(
                name,
                subscripts.is_empty(),
                enum_literals,
                variable_names,
            );
            for sub in subscripts {
                if let rumoca_core::Subscript::Expr { expr, .. } = sub {
                    canonicalize_expr(expr, enum_literals, variable_names);
                }
            }
        }
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            canonicalize_expr(lhs, enum_literals, variable_names);
            canonicalize_expr(rhs, enum_literals, variable_names);
        }
        rumoca_core::Expression::Unary { rhs, .. } => {
            canonicalize_expr(rhs, enum_literals, variable_names);
        }
        rumoca_core::Expression::BuiltinCall { args, .. }
        | rumoca_core::Expression::FunctionCall { args, .. } => {
            for arg in args {
                canonicalize_expr(arg, enum_literals, variable_names);
            }
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (cond, then_expr) in branches {
                canonicalize_expr(cond, enum_literals, variable_names);
                canonicalize_expr(then_expr, enum_literals, variable_names);
            }
            canonicalize_expr(else_branch, enum_literals, variable_names);
        }
        rumoca_core::Expression::Array { elements, .. }
        | rumoca_core::Expression::Tuple { elements, .. } => {
            for elem in elements {
                canonicalize_expr(elem, enum_literals, variable_names);
            }
        }
        rumoca_core::Expression::Range {
            start, step, end, ..
        } => {
            canonicalize_expr(start, enum_literals, variable_names);
            if let Some(step) = step {
                canonicalize_expr(step, enum_literals, variable_names);
            }
            canonicalize_expr(end, enum_literals, variable_names);
        }
        rumoca_core::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            for index in indices {
                canonicalize_expr(&mut index.range, enum_literals, variable_names);
            }
            canonicalize_expr(expr, enum_literals, variable_names);
            if let Some(filter) = filter {
                canonicalize_expr(filter, enum_literals, variable_names);
            }
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            canonicalize_expr(base, enum_literals, variable_names);
            for sub in subscripts {
                if let rumoca_core::Subscript::Expr { expr, .. } = sub {
                    canonicalize_expr(expr, enum_literals, variable_names);
                }
            }
        }
        rumoca_core::Expression::FieldAccess { base, .. } => {
            canonicalize_expr(base, enum_literals, variable_names);
        }
        rumoca_core::Expression::Literal { value: _, .. }
        | rumoca_core::Expression::Empty { .. } => {}
    }
}

fn canonicalize_var_ref_if_enum_literal(
    name: &mut rumoca_core::Reference,
    scalar_ref: bool,
    enum_literals: &EnumLiteralIndex,
    variable_names: &FxHashSet<VarName>,
) {
    if !scalar_ref {
        return;
    }

    let raw = name.as_str();
    if variable_names.contains(name.var_name()) || !looks_like_enum_literal_reference(name) {
        return;
    }

    if let Some(canonical) = enum_literals.canonicalize(name)
        && canonical != raw
    {
        *name = rumoca_core::Reference::new(canonical.to_string());
    }
}

fn looks_like_enum_literal_reference(reference: &Reference) -> bool {
    let Some(component_ref) = reference.component_ref() else {
        return looks_like_enum_literal_path(reference.as_str());
    };
    if component_ref.parts.len() < 2 || component_ref.parts.iter().any(|part| !part.subs.is_empty())
    {
        return false;
    }
    component_ref.parts[..component_ref.parts.len() - 1]
        .iter()
        .any(|part| part.ident.chars().next().is_some_and(char::is_uppercase))
}

fn reference_component_path(reference: &Reference) -> Option<ComponentPath> {
    if let Some(component_ref) = reference.component_ref() {
        if component_ref.parts.iter().any(|part| !part.subs.is_empty()) {
            return None;
        }
        return Some(ComponentPath::from_component_reference(component_ref));
    }
    Some(ComponentPath::from_flat_path(reference.as_str()))
}

fn canonicalize_when_equations(
    equations: &mut [flat::WhenEquation],
    enum_literals: &EnumLiteralIndex,
    variable_names: &FxHashSet<VarName>,
) {
    for equation in equations.iter_mut() {
        match equation {
            flat::WhenEquation::Assign { value, .. } | flat::WhenEquation::Reinit { value, .. } => {
                canonicalize_expr(value, enum_literals, variable_names);
            }
            flat::WhenEquation::Assert {
                condition, message, ..
            } => {
                canonicalize_expr(condition, enum_literals, variable_names);
                canonicalize_expr(message, enum_literals, variable_names);
            }
            flat::WhenEquation::Terminate { message, .. } => {
                canonicalize_expr(message, enum_literals, variable_names);
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                for (condition, branch_equations) in branches {
                    canonicalize_expr(condition, enum_literals, variable_names);
                    canonicalize_when_equations(branch_equations, enum_literals, variable_names);
                }
                canonicalize_when_equations(else_branch, enum_literals, variable_names);
            }
            flat::WhenEquation::FunctionCallOutputs { function, .. } => {
                canonicalize_expr(function, enum_literals, variable_names);
            }
        }
    }
}

fn canonicalize_statements(
    statements: &mut [rumoca_core::Statement],
    enum_literals: &EnumLiteralIndex,
    variable_names: &FxHashSet<VarName>,
) {
    let mut rewriter = EnumLiteralStatementCanonicalizer {
        enum_literals,
        variable_names,
    };
    for statement in statements {
        *statement = rewriter.rewrite_statement(statement);
    }
}

struct EnumLiteralStatementCanonicalizer<'a> {
    enum_literals: &'a EnumLiteralIndex,
    variable_names: &'a FxHashSet<VarName>,
}

impl ExpressionRewriter for EnumLiteralStatementCanonicalizer<'_> {
    fn rewrite_expression(&mut self, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
        let mut rewritten = expr.clone();
        canonicalize_expr(&mut rewritten, self.enum_literals, self.variable_names);
        rewritten
    }
}

impl StatementRewriter for EnumLiteralStatementCanonicalizer<'_> {}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::Span;

    fn test_span() -> Span {
        Span::from_offsets(
            rumoca_core::SourceId::from_source_name("enum_literals_test.mo"),
            1,
            2,
        )
    }

    fn var_ref(name: &str) -> rumoca_core::Expression {
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new(name),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn component_ref(name: &str) -> rumoca_core::ComponentReference {
        rumoca_core::ComponentReference {
            local: false,
            span: rumoca_core::Span::DUMMY,
            parts: vec![rumoca_core::ComponentRefPart {
                ident: name.to_string(),
                span: rumoca_core::Span::DUMMY,
                subs: vec![],
            }],
            def_id: None,
        }
    }

    #[test]
    fn rewrites_short_enum_literal_to_most_qualified_candidate() {
        let mut flat = flat::Model::new();
        flat.variables.insert(
            rumoca_core::VarName::new("y"),
            flat::Variable {
                binding: Some(var_ref("Init.NoInit")),
                ..flat::Variable::empty_with_span(test_span())
            },
        );
        flat.equations.push(rumoca_ir_flat::Equation::new(
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Eq,
                lhs: Box::new(var_ref("y")),
                rhs: Box::new(var_ref("Init.NoInit")),
                span: rumoca_core::Span::DUMMY,
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
            .get(&rumoca_core::VarName::new("y"))
            .and_then(|var| var.binding.as_ref())
            .expect("binding should exist");
        let rumoca_core::Expression::VarRef { name, .. } = binding else {
            panic!("binding should remain a var ref");
        };
        assert_eq!(name.as_str(), "TypesPkg.Init.NoInit");

        let rumoca_core::Expression::Binary { rhs, .. } = &flat.equations[0].residual else {
            panic!("expected binary equation");
        };
        let rumoca_core::Expression::VarRef { name, .. } = rhs.as_ref() else {
            panic!("rhs should be enum literal var ref");
        };
        assert_eq!(name.as_str(), "TypesPkg.Init.NoInit");
    }

    #[test]
    fn does_not_rewrite_regular_variable_references() {
        let mut flat = flat::Model::new();
        flat.variables.insert(
            rumoca_core::VarName::new("Plant.Init.NoInit"),
            flat::Variable {
                binding: Some(rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(1),
                    span: rumoca_core::Span::DUMMY,
                }),
                ..flat::Variable::empty_with_span(test_span())
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

        let rumoca_core::Expression::VarRef { name, .. } = &flat.equations[0].residual else {
            panic!("expected var ref");
        };
        assert_eq!(name.as_str(), "Plant.Init.NoInit");
    }

    #[test]
    fn rewrites_enum_literals_inside_algorithm_statements() {
        let mut flat = flat::Model::new();
        flat.algorithms.push(flat::Algorithm::new(
            vec![rumoca_core::Statement::Assignment {
                comp: component_ref("y"),
                value: var_ref("Init.NoInit"),
                span: rumoca_core::Span::DUMMY,
            }],
            Span::DUMMY,
            "test",
        ));

        let mut known_enums = FxHashMap::default();
        known_enums.insert("enumParam".to_string(), "TypesPkg.Init.NoInit".to_string());

        canonicalize_flat_enum_literals(&mut flat, &ast::ClassTree::new(), &known_enums);

        let rumoca_core::Statement::Assignment { value, .. } = &flat.algorithms[0].statements[0]
        else {
            panic!("expected assignment statement");
        };
        let rumoca_core::Expression::VarRef { name, .. } = value else {
            panic!("expected enum literal var ref");
        };
        assert_eq!(name.as_str(), "TypesPkg.Init.NoInit");
    }
}
