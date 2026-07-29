//! Flatten-exit pass: attach structured component references to every
//! declared variable reference and reject unresolved occurrences.
//!
//! Flatten owns its name encoding. Connector/array expansion historically
//! rendered element references (`sum.u[2]`) as plain strings and downstream
//! phases re-derived the structure (the resolver scalar-name parse, balance
//! prefix matching). This pass resolves each rendered reference against the
//! flat variable table once, at the producing phase's boundary. A reference
//! that already carries resolved structure keeps it. Any other non-generated
//! reference must name a declaration in that table; the phase fails at its
//! exact occurrence instead of emitting an ambiguous Flat tree.

use super::*;
use rumoca_core::{ExpressionRewriter, StatementRewriter};

pub(crate) fn attach_structured_references(flat: &mut flat::Model) -> Result<(), FlattenError> {
    let index = StructuredRefIndex::build(flat)?;

    let mut rewriter = StructuredRefRewriter {
        index: &index,
        error: None,
    };
    let mut variables = std::mem::take(&mut flat.variables);
    for var in variables.values_mut() {
        rewrite_opt_expr(&mut var.binding, &mut rewriter);
        rewrite_opt_expr(&mut var.start, &mut rewriter);
        rewrite_opt_expr(&mut var.min, &mut rewriter);
        rewrite_opt_expr(&mut var.max, &mut rewriter);
        rewrite_opt_expr(&mut var.nominal, &mut rewriter);
    }
    flat.variables = variables;

    for equation in &mut flat.equations {
        equation.residual = rewriter.rewrite_expression(&equation.residual);
    }
    for equation in &mut flat.initial_equations {
        equation.residual = rewriter.rewrite_expression(&equation.residual);
    }
    for family in flat
        .structured_equations
        .iter_mut()
        .chain(flat.initial_structured_equations.iter_mut())
    {
        let Some(template) = family.template.as_mut() else {
            continue;
        };
        for body in &mut template.body {
            *body = rewriter.rewrite_expression(body);
        }
    }
    for assert_eq in flat
        .assert_equations
        .iter_mut()
        .chain(flat.initial_assert_equations.iter_mut())
    {
        assert_eq.condition = rewriter.rewrite_expression(&assert_eq.condition);
        assert_eq.message = rewriter.rewrite_expression(&assert_eq.message);
        rewrite_opt_expr(&mut assert_eq.level, &mut rewriter);
    }
    for chain in &mut flat.when_chains {
        for branch in &mut chain.branches {
            branch.condition = rewriter.rewrite_expression(&branch.condition);
            for equation in &mut branch.equations {
                rewrite_when_equation(equation, &mut rewriter);
            }
        }
    }
    for algorithm in flat
        .algorithms
        .iter_mut()
        .chain(flat.initial_algorithms.iter_mut())
    {
        for statement in &mut algorithm.statements {
            *statement = rewriter.rewrite_statement(statement);
        }
    }
    match rewriter.error {
        Some(error) => Err(error),
        None => Ok(()),
    }
}

fn rewrite_when_equation(
    equation: &mut flat::WhenEquation,
    rewriter: &mut StructuredRefRewriter<'_>,
) {
    match equation {
        flat::WhenEquation::Assign { value, .. } | flat::WhenEquation::Reinit { value, .. } => {
            *value = rewriter.rewrite_expression(value);
        }
        flat::WhenEquation::Assert {
            condition,
            message,
            level,
            ..
        } => {
            *condition = rewriter.rewrite_expression(condition);
            *message = rewriter.rewrite_expression(message);
            if let Some(level) = level.as_deref_mut() {
                *level = rewriter.rewrite_expression(level);
            }
        }
        flat::WhenEquation::Terminate { message, .. } => {
            *message = rewriter.rewrite_expression(message);
        }
        flat::WhenEquation::Conditional {
            branches,
            else_branch,
            ..
        } => {
            for (condition, equations) in branches.iter_mut() {
                *condition = rewriter.rewrite_expression(condition);
                for nested in equations {
                    rewrite_when_equation(nested, rewriter);
                }
            }
            if let Some(else_branch) = else_branch {
                for nested in else_branch {
                    rewrite_when_equation(nested, rewriter);
                }
            }
        }
        flat::WhenEquation::FunctionCallOutputs { function, .. } => {
            *function = rewriter.rewrite_expression(function);
        }
    }
}

fn rewrite_opt_expr(
    expr: &mut Option<rumoca_core::Expression>,
    rewriter: &mut StructuredRefRewriter<'_>,
) {
    if let Some(inner) = expr.as_mut() {
        *inner = rewriter.rewrite_expression(inner);
    }
}

/// Structured references for every flat variable, plus element references for
/// array variables (`base[i]` -> base reference + literal index subscripts).
struct StructuredRefIndex {
    by_name: std::collections::HashMap<rumoca_core::VarNameId, rumoca_core::ComponentReference>,
}

impl StructuredRefIndex {
    fn build(flat: &flat::Model) -> Result<Self, FlattenError> {
        let mut by_name = std::collections::HashMap::new();
        for (name, var) in &flat.variables {
            let Some(reference) = var.component_ref.as_ref() else {
                return Err(FlattenError::missing_flat_variable_identity(
                    name.as_str(),
                    var.source_span,
                ));
            };
            by_name.insert(name.id(), reference.clone());
        }
        Ok(Self { by_name })
    }

    fn structured_for(
        &self,
        name: &rumoca_core::VarName,
    ) -> Result<Option<rumoca_core::ComponentReference>, FlattenError> {
        if let Some(reference) = self.by_name.get(&name.id()) {
            return Ok(Some(reference.clone()));
        }
        // Element of an array variable: recover `(base, indices)` once, here
        // at the producing boundary, and compose the element reference from
        // the base variable's structured reference.
        let Some(scalar) = rumoca_core::parse_scalar_name(name.as_str()) else {
            return Ok(None);
        };
        let base = rumoca_core::VarName::new(scalar.base);
        let Some(base_ref) = self.by_name.get(&base.id()) else {
            return Ok(None);
        };
        let mut reference = base_ref.clone();
        let Some(part) = reference.parts.last_mut() else {
            return Ok(None);
        };
        let mut subs = Vec::with_capacity(scalar.indices.len());
        for index in scalar.indices {
            subs.push(generated_index_subscript(
                index,
                reference.span,
                "flat structured reference subscript",
            )?);
        }
        part.subs.extend(subs);
        Ok(Some(reference))
    }
}

struct StructuredRefRewriter<'a> {
    index: &'a StructuredRefIndex,
    error: Option<FlattenError>,
}

impl ExpressionRewriter for StructuredRefRewriter<'_> {
    fn rewrite_expression(&mut self, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
        if self.error.is_some() {
            return expr.clone();
        }
        let rumoca_core::Expression::VarRef {
            name,
            subscripts,
            span,
        } = expr
        else {
            return self.walk_expression(expr);
        };
        if name.is_generated() {
            return self.walk_expression(expr);
        }
        if name.target_def_id().is_some() {
            return self.walk_expression(expr);
        }
        let reference = match self.index.structured_for(name.var_name()) {
            Ok(Some(reference)) => reference,
            Ok(None) if name.has_structure() => return self.walk_expression(expr),
            Ok(None) => {
                self.error = Some(FlattenError::unresolved_flat_reference(
                    name.as_str(),
                    *span,
                ));
                return expr.clone();
            }
            Err(error) => {
                self.error = Some(error);
                return expr.clone();
            }
        };
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::with_component_reference(name.as_str(), reference),
            subscripts: subscripts
                .iter()
                .map(|sub| self.rewrite_subscript(sub))
                .collect(),
            span: *span,
        }
    }
}

impl StatementRewriter for StructuredRefRewriter<'_> {}

fn generated_index_subscript(
    index: i64,
    span: rumoca_core::Span,
    context: &'static str,
) -> Result<rumoca_core::Subscript, FlattenError> {
    rumoca_core::Subscript::try_generated_index(index, span, context)
        .map_err(|err| FlattenError::missing_source_context(err.to_string()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{
        ComponentRefPart, ComponentReference, ComprehensionScalarView, ComprehensionTemplate,
        DefId, Expression, Reference, SourceId, StructuredIndexDomain, VarName,
    };

    fn span() -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(SourceId::from_source_name("structured_refs_test.mo"), 1, 2)
    }

    fn component_reference(parts: &[&str], def_id: Option<DefId>) -> ComponentReference {
        let span = span();
        ComponentReference {
            local: false,
            span,
            parts: parts
                .iter()
                .map(|ident| ComponentRefPart {
                    ident: (*ident).to_string(),
                    span,
                    subs: Vec::new(),
                })
                .collect(),
            def_id,
        }
    }

    #[test]
    fn structured_template_reference_uses_declared_variable_identity() {
        let mut flat = flat::Model::default();
        let name = VarName::new("vehicle.R");
        let def_id = DefId::new(42);
        flat.variables.insert(
            name.clone(),
            flat::Variable {
                name: name.clone(),
                component_ref: Some(component_reference(&["vehicle", "R"], Some(def_id))),
                ..flat::Variable::empty_with_span(span())
            },
        );
        flat.structured_equations
            .push(flat::StructuredEquationFamily {
                domain: StructuredIndexDomain {
                    binders: Vec::new(),
                },
                first_equation_index: 0,
                equations_per_point: 1,
                span: span(),
                origin: flat::EquationOrigin::ComponentEquation {
                    component: "vehicle".to_string(),
                },
                regular: None,
                template: Some(ComprehensionTemplate {
                    body: vec![Expression::VarRef {
                        name: Reference::with_component_reference(
                            name.as_str(),
                            component_reference(&["vehicle", "R"], None),
                        ),
                        subscripts: Vec::new(),
                        span: span(),
                    }],
                    scalar_view: ComprehensionScalarView::BinderSubstitution,
                }),
                interiors_materialized: true,
            });

        attach_structured_references(&mut flat).expect("structured references attach");

        let Expression::VarRef { name, .. } = &flat.structured_equations[0]
            .template
            .as_ref()
            .expect("template")
            .body[0]
        else {
            panic!("expected template var ref");
        };
        assert_eq!(name.target_def_id(), Some(def_id));
    }

    #[test]
    fn unresolved_reference_fails_at_exact_occurrence() {
        let occurrence = rumoca_core::Span::from_offsets(
            SourceId::from_source_name("unresolved_flat_reference.mo"),
            18,
            25,
        );
        let mut flat = flat::Model::default();
        flat.equations.push(flat::Equation::new(
            Expression::VarRef {
                name: Reference::new("missing"),
                subscripts: Vec::new(),
                span: occurrence,
            },
            occurrence,
            flat::EquationOrigin::ComponentEquation {
                component: "Root".to_string(),
            },
        ));

        let error = attach_structured_references(&mut flat)
            .expect_err("an unresolved source reference must not enter Flat IR");
        assert!(matches!(
            error,
            FlattenError::UnresolvedFlatReference { ref name, span }
                if name == "missing" && span == occurrence
        ));
        let diagnostic = rumoca_core::PhaseError::to_diagnostic(&error);
        assert_eq!(diagnostic.code.as_deref(), Some("EF023"));
        assert_eq!(diagnostic.labels[0].span, occurrence);
    }

    #[test]
    fn unstructured_variable_fails_at_declaration_span() {
        let declaration = rumoca_core::Span::from_offsets(
            SourceId::from_source_name("unstructured_flat_variable.mo"),
            9,
            15,
        );
        let mut flat = flat::Model::default();
        flat.variables.insert(
            VarName::new("orphan"),
            flat::Variable {
                name: VarName::new("orphan"),
                component_ref: None,
                ..flat::Variable::empty_with_span(declaration)
            },
        );

        let error = attach_structured_references(&mut flat)
            .expect_err("an unstructured declaration must not enter Flat IR");
        assert!(matches!(
            error,
            FlattenError::MissingFlatVariableIdentity { ref name, span }
                if name == "orphan" && span == declaration
        ));
        let diagnostic = rumoca_core::PhaseError::to_diagnostic(&error);
        assert_eq!(diagnostic.code.as_deref(), Some("EF024"));
        assert_eq!(diagnostic.labels[0].span, declaration);
    }

    #[test]
    fn resolved_external_reference_keeps_its_source_identity() {
        let occurrence = rumoca_core::Span::from_offsets(
            SourceId::from_source_name("resolved_flat_reference.mo"),
            11,
            29,
        );
        let resolved = component_reference(&["Pkg", "Literal"], Some(DefId::new(71)));
        let mut flat = flat::Model::default();
        flat.equations.push(flat::Equation::new(
            Expression::VarRef {
                name: Reference::with_component_reference("Pkg.Literal", resolved),
                subscripts: Vec::new(),
                span: occurrence,
            },
            occurrence,
            flat::EquationOrigin::ComponentEquation {
                component: "Root".to_string(),
            },
        ));

        attach_structured_references(&mut flat)
            .expect("a resolved non-variable reference is already valid");

        let Expression::VarRef { name, span, .. } = &flat.equations[0].residual else {
            panic!("expected variable reference");
        };
        assert_eq!(name.target_def_id(), Some(DefId::new(71)));
        assert_eq!(*span, occurrence);
    }
}
