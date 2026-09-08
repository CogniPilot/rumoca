use super::*;

struct VariableExpressionProjection {
    binding: Option<Expression>,
    start: Option<Expression>,
    min: Option<Expression>,
    max: Option<Expression>,
    nominal: Option<Expression>,
}

struct AssertionExpressionProjection {
    condition: Expression,
    message: Expression,
    level: Option<Expression>,
}

struct OwnerRewrite<Owner, Replacement> {
    owner: Owner,
    replacement: Replacement,
}

type PlannedOwnerRewrites<Owner, Replacement> = Vec<Option<OwnerRewrite<Owner, Replacement>>>;
type AlgorithmStatements = Vec<rumoca_core::Statement>;
type StructuredEquationBody = Vec<Expression>;

struct VariableOwnerRewrite {
    key: rumoca_core::VarName,
    instance_id: rumoca_core::InstanceId,
    owner: flat::Variable,
    expressions: VariableExpressionProjection,
}

fn retain_owner<Owner: Clone, Replacement>(
    owner: &Owner,
    replacement: Option<Replacement>,
) -> Option<OwnerRewrite<Owner, Replacement>> {
    replacement.map(|replacement| OwnerRewrite {
        owner: owner.clone(),
        replacement,
    })
}

/// Bounded stream delta: each affected owner retains one complete typed
/// snapshot plus its replacement; unaffected semantic trees and the Flat root
/// are never cloned. Variables also retain exact declaration key + InstanceId.
pub(in crate::connections) struct StreamRewriteProjection {
    equations: Vec<Option<OwnerRewrite<flat::Equation, Expression>>>,
    initial_equations: Vec<Option<OwnerRewrite<flat::Equation, Expression>>>,
    structured_bodies: Vec<Option<OwnerRewrite<flat::StructuredEquationFamily, Vec<Expression>>>>,
    initial_structured_bodies:
        Vec<Option<OwnerRewrite<flat::StructuredEquationFamily, Vec<Expression>>>>,
    variable_count: usize,
    variables: Vec<VariableOwnerRewrite>,
    assertions: Vec<Option<OwnerRewrite<flat::AssertEquation, AssertionExpressionProjection>>>,
    initial_assertions:
        Vec<Option<OwnerRewrite<flat::AssertEquation, AssertionExpressionProjection>>>,
    algorithms: PlannedOwnerRewrites<flat::Algorithm, AlgorithmStatements>,
    initial_algorithms: PlannedOwnerRewrites<flat::Algorithm, AlgorithmStatements>,
    when_chains: Vec<Option<OwnerRewrite<flat::WhenChain, flat::WhenChain>>>,
}

/// Private-field consuming proof required by the infallible apply path.
pub(in crate::connections) struct ValidatedStreamRewrite {
    projection: StreamRewriteProjection,
}

fn plan_equation_rewrites(
    equations: &[flat::Equation],
    identities: StreamOperatorIdentities,
    rewriter: &mut StreamOperatorRewriter,
) -> Result<Vec<Option<OwnerRewrite<flat::Equation, Expression>>>, FlattenError> {
    equations
        .iter()
        .map(|equation| {
            rewrite_if_stream(&equation.residual, identities, rewriter)
                .map(|replacement| retain_owner(equation, replacement))
        })
        .collect()
}

fn plan_variable_rewrites(
    model: &flat::Model,
    identities: StreamOperatorIdentities,
    rewriter: &mut StreamOperatorRewriter,
) -> Result<Vec<VariableOwnerRewrite>, FlattenError> {
    model
        .variables
        .iter()
        .map(|(key, variable)| {
            let expressions = VariableExpressionProjection {
                binding: rewrite_optional_if_stream(&variable.binding, identities, rewriter)?,
                start: rewrite_optional_if_stream(&variable.start, identities, rewriter)?,
                min: rewrite_optional_if_stream(&variable.min, identities, rewriter)?,
                max: rewrite_optional_if_stream(&variable.max, identities, rewriter)?,
                nominal: rewrite_optional_if_stream(&variable.nominal, identities, rewriter)?,
            };
            Ok(variable_rewrite_if_affected(key, variable, expressions))
        })
        .collect::<Result<Vec<_>, FlattenError>>()
        .map(|rewrites| rewrites.into_iter().flatten().collect())
}

fn variable_rewrite_if_affected(
    key: &rumoca_core::VarName,
    variable: &flat::Variable,
    expressions: VariableExpressionProjection,
) -> Option<VariableOwnerRewrite> {
    (expressions.binding.is_some()
        || expressions.start.is_some()
        || expressions.min.is_some()
        || expressions.max.is_some()
        || expressions.nominal.is_some())
    .then(|| VariableOwnerRewrite {
        key: key.clone(),
        instance_id: variable.instance_id,
        owner: variable.clone(),
        expressions,
    })
}

fn plan_assertion_rewrites(
    assertions: &[flat::AssertEquation],
    identities: StreamOperatorIdentities,
    rewriter: &mut StreamOperatorRewriter,
) -> Result<
    Vec<Option<OwnerRewrite<flat::AssertEquation, AssertionExpressionProjection>>>,
    FlattenError,
> {
    assertions
        .iter()
        .map(|assertion| {
            plan_assertion_if_stream(assertion, identities, rewriter)
                .map(|replacement| retain_owner(assertion, replacement))
        })
        .collect()
}

fn plan_algorithm_rewrites(
    algorithms: &[flat::Algorithm],
    identities: StreamOperatorIdentities,
    rewriter: &mut StreamOperatorRewriter,
) -> Result<PlannedOwnerRewrites<flat::Algorithm, AlgorithmStatements>, FlattenError> {
    algorithms
        .iter()
        .map(|algorithm| {
            let replacement =
                statements_contain_stream_operator(&algorithm.statements, identities)?
                    .then(|| rewriter.rewrite_statements(&algorithm.statements))
                    .transpose()?;
            Ok(retain_owner(algorithm, replacement))
        })
        .collect()
}

fn plan_when_rewrites(
    chains: &[flat::WhenChain],
    identities: StreamOperatorIdentities,
    rewriter: &mut StreamOperatorRewriter,
) -> Result<Vec<Option<OwnerRewrite<flat::WhenChain, flat::WhenChain>>>, FlattenError> {
    chains
        .iter()
        .map(|chain| {
            if !when_chain_contains_stream_operator(chain, identities)? {
                return Ok(None);
            }
            let mut rewritten = chain.clone();
            rewrite_when_chain_slice(std::slice::from_mut(&mut rewritten), rewriter)?;
            Ok(Some(OwnerRewrite {
                owner: chain.clone(),
                replacement: rewritten,
            }))
        })
        .collect()
}

impl StreamRewriteProjection {
    pub(super) fn plan(
        model: &flat::Model,
        rewriter: &mut StreamOperatorRewriter,
    ) -> Result<Self, FlattenError> {
        let operator_identities = rewriter.operator_identities;
        let equations = plan_equation_rewrites(&model.equations, operator_identities, rewriter)?;
        let initial_equations =
            plan_equation_rewrites(&model.initial_equations, operator_identities, rewriter)?;
        let structured_bodies =
            rewrite_structured_bodies(&model.structured_equations, operator_identities, rewriter)?;
        let initial_structured_bodies = rewrite_structured_bodies(
            &model.initial_structured_equations,
            operator_identities,
            rewriter,
        )?;
        let variables = plan_variable_rewrites(model, operator_identities, rewriter)?;
        let assertions =
            plan_assertion_rewrites(&model.assert_equations, operator_identities, rewriter)?;
        let initial_assertions = plan_assertion_rewrites(
            &model.initial_assert_equations,
            operator_identities,
            rewriter,
        )?;
        let algorithms = plan_algorithm_rewrites(&model.algorithms, operator_identities, rewriter)?;
        let initial_algorithms =
            plan_algorithm_rewrites(&model.initial_algorithms, operator_identities, rewriter)?;
        let when_chains = plan_when_rewrites(&model.when_chains, operator_identities, rewriter)?;
        Ok(Self {
            equations,
            initial_equations,
            structured_bodies,
            initial_structured_bodies,
            variable_count: model.variables.len(),
            variables,
            assertions,
            initial_assertions,
            algorithms,
            initial_algorithms,
            when_chains,
        })
    }

    #[cfg(test)]
    fn commit_checked(self, model: &mut flat::Model) -> Result<(), FlattenError> {
        self.validate(model)?.apply(model);
        Ok(())
    }

    pub(in crate::connections) fn validate(
        self,
        model: &flat::Model,
    ) -> Result<ValidatedStreamRewrite, FlattenError> {
        self.validate_matches(model)?;
        Ok(ValidatedStreamRewrite { projection: self })
    }

    fn validate_matches(&self, model: &flat::Model) -> Result<(), FlattenError> {
        validate_owner_rewrites(&model.equations, &self.equations, "equation")?;
        validate_owner_rewrites(
            &model.initial_equations,
            &self.initial_equations,
            "initial equation",
        )?;
        validate_owner_rewrites(
            &model.structured_equations,
            &self.structured_bodies,
            "structured equation family",
        )?;
        validate_owner_rewrites(
            &model.initial_structured_equations,
            &self.initial_structured_bodies,
            "initial structured equation family",
        )?;
        if model.variables.len() != self.variable_count {
            return Err(changed_stream_owner("variable catalog cardinality"));
        }
        for rewrite in &self.variables {
            let variable = model
                .variables
                .get(&rewrite.key)
                .ok_or_else(|| changed_stream_owner("variable declaration key"))?;
            if variable.instance_id != rewrite.instance_id || variable != &rewrite.owner {
                return Err(changed_stream_owner("variable declaration"));
            }
        }
        validate_owner_rewrites(&model.assert_equations, &self.assertions, "assertion")?;
        validate_owner_rewrites(
            &model.initial_assert_equations,
            &self.initial_assertions,
            "initial assertion",
        )?;
        validate_owner_rewrites(&model.algorithms, &self.algorithms, "algorithm")?;
        validate_owner_rewrites(
            &model.initial_algorithms,
            &self.initial_algorithms,
            "initial algorithm",
        )?;
        validate_owner_rewrites(&model.when_chains, &self.when_chains, "when chain")?;
        Ok(())
    }
}

impl ValidatedStreamRewrite {
    pub(in crate::connections) fn apply(self, model: &mut flat::Model) {
        let projection = self.projection;
        for (equation, rewrite) in model.equations.iter_mut().zip(projection.equations) {
            if let Some(rewrite) = rewrite {
                equation.residual = rewrite.replacement;
            }
        }
        for (equation, rewrite) in model
            .initial_equations
            .iter_mut()
            .zip(projection.initial_equations)
        {
            if let Some(rewrite) = rewrite {
                equation.residual = rewrite.replacement;
            }
        }
        commit_structured_bodies(
            &mut model.structured_equations,
            projection.structured_bodies,
        );
        commit_structured_bodies(
            &mut model.initial_structured_equations,
            projection.initial_structured_bodies,
        );
        for rewrite in projection.variables {
            let variable = model
                .variables
                .get_mut(&rewrite.key)
                .expect("preflight retained the exact variable owner key");
            assert_eq!(variable.instance_id, rewrite.instance_id);
            let expressions = rewrite.expressions;
            if let Some(value) = expressions.binding {
                variable.binding = Some(value);
            }
            if let Some(value) = expressions.start {
                variable.start = Some(value);
            }
            if let Some(value) = expressions.min {
                variable.min = Some(value);
            }
            if let Some(value) = expressions.max {
                variable.max = Some(value);
            }
            if let Some(value) = expressions.nominal {
                variable.nominal = Some(value);
            }
        }
        commit_assertions(&mut model.assert_equations, projection.assertions);
        commit_assertions(
            &mut model.initial_assert_equations,
            projection.initial_assertions,
        );
        for (algorithm, rewrite) in model.algorithms.iter_mut().zip(projection.algorithms) {
            if let Some(rewrite) = rewrite {
                algorithm.statements = rewrite.replacement;
            }
        }
        for (algorithm, rewrite) in model
            .initial_algorithms
            .iter_mut()
            .zip(projection.initial_algorithms)
        {
            if let Some(rewrite) = rewrite {
                algorithm.statements = rewrite.replacement;
            }
        }
        for (chain, rewrite) in model.when_chains.iter_mut().zip(projection.when_chains) {
            if let Some(rewrite) = rewrite {
                *chain = rewrite.replacement;
            }
        }
    }
}

fn validate_owner_rewrites<Owner: PartialEq, Replacement>(
    owners: &[Owner],
    rewrites: &[Option<OwnerRewrite<Owner, Replacement>>],
    description: &'static str,
) -> Result<(), FlattenError> {
    if owners.len() != rewrites.len() {
        return Err(changed_stream_owner(description));
    }
    for (owner, rewrite) in owners.iter().zip(rewrites) {
        if let Some(rewrite) = rewrite
            && owner != &rewrite.owner
        {
            return Err(changed_stream_owner(description));
        }
    }
    Ok(())
}

fn changed_stream_owner(description: &'static str) -> FlattenError {
    FlattenError::internal(format!(
        "a planned stream rewrite no longer owns its exact {description}"
    ))
}

fn rewrite_structured_bodies(
    families: &[flat::StructuredEquationFamily],
    operator_identities: StreamOperatorIdentities,
    rewriter: &mut StreamOperatorRewriter,
) -> Result<
    PlannedOwnerRewrites<flat::StructuredEquationFamily, StructuredEquationBody>,
    FlattenError,
> {
    families
        .iter()
        .map(|family| {
            let replacement = match family.template.as_ref() {
                Some(template)
                    if expressions_contain_stream_operator(
                        &template.body,
                        operator_identities,
                    )? =>
                {
                    Some(rewriter.rewrite_expressions(&template.body)?)
                }
                _ => None,
            };
            Ok(retain_owner(family, replacement))
        })
        .collect()
}

fn commit_structured_bodies(
    families: &mut [flat::StructuredEquationFamily],
    bodies: Vec<Option<OwnerRewrite<flat::StructuredEquationFamily, Vec<Expression>>>>,
) {
    assert_eq!(families.len(), bodies.len());
    for (family, rewrite) in families.iter_mut().zip(bodies) {
        if let Some(rewrite) = rewrite {
            family
                .template
                .as_mut()
                .expect("a planned structured body retains its owner")
                .body = rewrite.replacement;
        }
    }
}

fn rewrite_if_stream(
    expression: &Expression,
    operator_identities: StreamOperatorIdentities,
    rewriter: &mut StreamOperatorRewriter,
) -> Result<Option<Expression>, FlattenError> {
    contains_stream_operator(expression, operator_identities)?
        .then(|| rewriter.rewrite_expression(expression))
        .transpose()
}

fn rewrite_optional_if_stream(
    expression: &Option<Expression>,
    operator_identities: StreamOperatorIdentities,
    rewriter: &mut StreamOperatorRewriter,
) -> Result<Option<Expression>, FlattenError> {
    expression
        .as_ref()
        .map(|value| rewrite_if_stream(value, operator_identities, rewriter))
        .transpose()
        .map(Option::flatten)
}

fn plan_assertion_expressions(
    assertion: &flat::AssertEquation,
    rewriter: &mut StreamOperatorRewriter,
) -> Result<AssertionExpressionProjection, FlattenError> {
    Ok(AssertionExpressionProjection {
        condition: rewriter.rewrite_expression(&assertion.condition)?,
        message: rewriter.rewrite_expression(&assertion.message)?,
        level: assertion
            .level
            .as_ref()
            .map(|level| rewriter.rewrite_expression(level))
            .transpose()?,
    })
}

fn assertion_contains_stream_operator(
    assertion: &flat::AssertEquation,
    operator_identities: StreamOperatorIdentities,
) -> Result<bool, FlattenError> {
    Ok(
        contains_stream_operator(&assertion.condition, operator_identities)?
            || contains_stream_operator(&assertion.message, operator_identities)?
            || match assertion.level.as_ref() {
                Some(level) => contains_stream_operator(level, operator_identities)?,
                None => false,
            },
    )
}

fn plan_assertion_if_stream(
    assertion: &flat::AssertEquation,
    operator_identities: StreamOperatorIdentities,
    rewriter: &mut StreamOperatorRewriter,
) -> Result<Option<AssertionExpressionProjection>, FlattenError> {
    assertion_contains_stream_operator(assertion, operator_identities)?
        .then(|| plan_assertion_expressions(assertion, rewriter))
        .transpose()
}

pub(super) fn rewrite_assertion_expressions(
    assertion: &mut flat::AssertEquation,
    rewriter: &mut StreamOperatorRewriter,
) -> Result<(), FlattenError> {
    let operator_identities = rewriter.operator_identities;
    let Some(planned) = plan_assertion_if_stream(assertion, operator_identities, rewriter)? else {
        return Ok(());
    };
    assertion.condition = planned.condition;
    assertion.message = planned.message;
    assertion.level = planned.level;
    Ok(())
}

fn commit_assertions(
    assertions: &mut [flat::AssertEquation],
    expressions: Vec<Option<OwnerRewrite<flat::AssertEquation, AssertionExpressionProjection>>>,
) {
    assert_eq!(assertions.len(), expressions.len());
    for (assertion, rewrite) in assertions.iter_mut().zip(expressions) {
        if let Some(rewrite) = rewrite {
            let expressions = rewrite.replacement;
            assertion.condition = expressions.condition;
            assertion.message = expressions.message;
            assertion.level = expressions.level;
        }
    }
}

#[cfg(test)]
mod projection_tests {
    use super::*;

    fn operator_reference(spelling: &str, def_id: DefId) -> Reference {
        Reference::generated_component_reference(
            ComponentReference::construct(
                true,
                Span::DUMMY,
                vec![ComponentRefPart {
                    ident: spelling.to_string(),
                    span: Span::DUMMY,
                    subs: Vec::new(),
                    def_id,
                }],
            )
            .expect("fixture operator reference is structurally valid"),
        )
    }

    #[test]
    fn same_spelling_user_function_is_not_a_stream_operator() {
        let identities = StreamOperatorIdentities::fixture();
        let user = operator_reference("inStream", DefId::new(0x00fe_2999));
        assert_eq!(
            identities
                .classify(&user, Span::DUMMY)
                .expect("a resolved user function remains ordinary"),
            None
        );
    }

    #[test]
    fn apparent_stream_operator_without_identity_is_refused() {
        let identities = StreamOperatorIdentities::fixture();
        let error = identities
            .classify(&Reference::new("inStream"), Span::DUMMY)
            .expect_err("spelling cannot substitute for Resolve identity");
        assert!(error.to_string().contains("lacks exact Resolve target"));
    }

    #[test]
    fn swapped_stream_operator_role_is_refused() {
        let identities = StreamOperatorIdentities::fixture();
        let swapped = operator_reference("inStream", identities.actual_stream);
        let error = identities
            .classify(&swapped, Span::DUMMY)
            .expect_err("spelling and predefined role must agree");
        assert!(error.to_string().contains("contradicts"));
    }

    #[test]
    fn stream_commit_cannot_truncate_a_mismatched_owner_vector() {
        let mut model = flat::Model::new();
        let projection = StreamRewriteProjection {
            equations: vec![None],
            initial_equations: Vec::new(),
            structured_bodies: Vec::new(),
            initial_structured_bodies: Vec::new(),
            variable_count: 0,
            variables: Vec::new(),
            assertions: Vec::new(),
            initial_assertions: Vec::new(),
            algorithms: Vec::new(),
            initial_algorithms: Vec::new(),
            when_chains: Vec::new(),
        };
        let before = crate::connections::connection_mutation_snapshot(&model);
        let result = projection.commit_checked(&mut model);
        assert!(result.is_err());
        assert_eq!(
            crate::connections::connection_mutation_snapshot(&model),
            before
        );
    }

    #[test]
    fn stream_commit_refuses_an_equal_length_equation_reorder() {
        let mut model = flat::Model::new();
        let source = rumoca_core::SourceId::from_source_name("stream_owner_reorder.mo");
        let first_span = Span::from_offsets(source, 1, 2);
        let second_span = Span::from_offsets(source, 3, 4);
        model.equations.push(flat::Equation::new(
            real_literal(1.0, first_span),
            first_span,
            flat::EquationOrigin::ComponentEquation {
                component: "first".to_string(),
            },
        ));
        model.equations.push(flat::Equation::new(
            real_literal(1.0, second_span),
            second_span,
            flat::EquationOrigin::ComponentEquation {
                component: "second".to_string(),
            },
        ));
        let projection = StreamRewriteProjection {
            equations: vec![
                Some(OwnerRewrite {
                    owner: model.equations[0].clone(),
                    replacement: real_literal(3.0, Span::DUMMY),
                }),
                None,
            ],
            initial_equations: Vec::new(),
            structured_bodies: Vec::new(),
            initial_structured_bodies: Vec::new(),
            variable_count: 0,
            variables: Vec::new(),
            assertions: Vec::new(),
            initial_assertions: Vec::new(),
            algorithms: Vec::new(),
            initial_algorithms: Vec::new(),
            when_chains: Vec::new(),
        };
        model.equations.swap(0, 1);
        let before = crate::connections::connection_mutation_snapshot(&model);
        let result = projection.commit_checked(&mut model);
        assert!(result.is_err());
        assert_eq!(
            crate::connections::connection_mutation_snapshot(&model),
            before
        );
    }

    #[test]
    fn stream_commit_refuses_a_changed_non_expression_owner_field() {
        let mut model = flat::Model::new();
        model.equations.push(flat::Equation::new(
            real_literal(1.0, Span::DUMMY),
            Span::DUMMY,
            flat::EquationOrigin::ComponentEquation {
                component: "owner".to_string(),
            },
        ));
        let projection = StreamRewriteProjection {
            equations: vec![Some(OwnerRewrite {
                owner: model.equations[0].clone(),
                replacement: real_literal(3.0, Span::DUMMY),
            })],
            initial_equations: Vec::new(),
            structured_bodies: Vec::new(),
            initial_structured_bodies: Vec::new(),
            variable_count: 0,
            variables: Vec::new(),
            assertions: Vec::new(),
            initial_assertions: Vec::new(),
            algorithms: Vec::new(),
            initial_algorithms: Vec::new(),
            when_chains: Vec::new(),
        };
        model.equations[0].scalar_count = 2;
        let before = crate::connections::connection_mutation_snapshot(&model);
        let result = projection.commit_checked(&mut model);
        assert!(result.is_err());
        assert_eq!(
            crate::connections::connection_mutation_snapshot(&model),
            before
        );
    }
}
