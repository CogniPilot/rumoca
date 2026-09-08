use rumoca_ir_flat::FallibleStatementVisitor;

use super::*;

/// One complete traversal of every Flat expression owner consumed by DAE
/// analysis.
///
/// An analyzer implements the ordinary expression/statement visitor hooks and
/// may override [`Self::enter_function_owners`] when MLS function scope changes
/// its admissibility rules. Keeping the root-owner enumeration here prevents a
/// newly added Flat owner from reaching only one of the runtime-operator passes.
pub(super) trait ModelExpressionOwnerVisitor: FallibleStatementVisitor {
    fn visit_model_owners(
        &mut self,
        owners: &StructuredEquationOwners<'_>,
    ) -> Result<(), Self::Error> {
        let flat = owners.model();
        self.visit_equation_owners(owners)?;
        self.visit_assertion_and_algorithm_owners(flat)?;
        self.visit_when_owners(flat)?;
        self.enter_function_owners()?;
        self.visit_function_owners(flat)
    }

    fn enter_function_owners(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_equation_owners(
        &mut self,
        owners: &StructuredEquationOwners<'_>,
    ) -> Result<(), Self::Error> {
        owners
            .model()
            .variables
            .values()
            .flat_map(variable_attribute_expressions)
            .try_for_each(|expression| self.visit_expression(expression))?;
        for owner in owners
            .continuous()
            .owners()
            .iter()
            .chain(owners.initialization().owners())
        {
            match owner {
                flat::CheckedEquationOwner::Standalone(row) => {
                    self.visit_expression(&row.equation().residual)?;
                }
                flat::CheckedEquationOwner::Template(owner) => {
                    self.enter_structured_family(owner.family())?;
                    owner
                        .template()
                        .body
                        .iter()
                        .try_for_each(|expression| self.visit_expression(expression))?;
                    self.leave_structured_family()?;
                }
            }
        }
        Ok(())
    }

    fn enter_structured_family(
        &mut self,
        _family: &flat::StructuredEquationFamily,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn leave_structured_family(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_assertion_and_algorithm_owners(
        &mut self,
        flat: &flat::Model,
    ) -> Result<(), Self::Error> {
        for assertion in flat
            .assert_equations
            .iter()
            .chain(&flat.initial_assert_equations)
        {
            self.visit_expression(&assertion.condition)?;
            self.visit_expression(&assertion.message)?;
            if let Some(level) = &assertion.level {
                self.visit_expression(level)?;
            }
        }
        flat.algorithms
            .iter()
            .chain(&flat.initial_algorithms)
            .flat_map(|algorithm| &algorithm.statements)
            .try_for_each(|statement| self.visit_statement(statement))
    }

    fn visit_when_owners(&mut self, flat: &flat::Model) -> Result<(), Self::Error> {
        flat.when_chains
            .iter()
            .flat_map(flat::WhenChain::branches)
            .try_for_each(|branch| {
                self.visit_expression(&branch.condition)?;
                self.visit_when_equations(&branch.equations)
            })
    }

    fn visit_when_equations(
        &mut self,
        equations: &[flat::WhenEquation],
    ) -> Result<(), Self::Error> {
        equations
            .iter()
            .try_for_each(|equation| self.visit_when_equation(equation))
    }

    fn visit_when_equation(&mut self, equation: &flat::WhenEquation) -> Result<(), Self::Error> {
        match equation {
            flat::WhenEquation::Assign { value, .. }
            | flat::WhenEquation::Reinit { value, .. }
            | flat::WhenEquation::Terminate { message: value, .. }
            | flat::WhenEquation::FunctionCallOutputs {
                function: value, ..
            } => self.visit_expression(value),
            flat::WhenEquation::Assert {
                condition,
                message,
                level,
                ..
            } => {
                self.visit_expression(condition)?;
                self.visit_expression(message)?;
                level
                    .iter()
                    .try_for_each(|level| self.visit_expression(level))
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => self.visit_conditional_when(branches, else_branch),
        }
    }

    fn visit_conditional_when(
        &mut self,
        branches: &[(Expression, Vec<flat::WhenEquation>)],
        else_branch: &Option<Vec<flat::WhenEquation>>,
    ) -> Result<(), Self::Error> {
        branches.iter().try_for_each(|(condition, equations)| {
            self.visit_expression(condition)?;
            self.visit_when_equations(equations)
        })?;
        else_branch
            .as_deref()
            .map_or(Ok(()), |equations| self.visit_when_equations(equations))
    }

    fn visit_function_owners(&mut self, flat: &flat::Model) -> Result<(), Self::Error> {
        flat.functions
            .values()
            .try_for_each(|function| self.visit_function(function))
    }

    fn visit_function(&mut self, function: &rumoca_core::Function) -> Result<(), Self::Error> {
        function
            .inputs
            .iter()
            .chain(&function.outputs)
            .chain(&function.locals)
            .try_for_each(|parameter| self.visit_function_parameter(parameter))?;
        function
            .body
            .iter()
            .try_for_each(|statement| self.visit_statement(statement))?;
        if let Some(external) = &function.external {
            external
                .args
                .iter()
                .try_for_each(|argument| self.visit_expression(argument))?;
            external
                .annotations
                .iter()
                .try_for_each(|annotation| self.visit_expression(&annotation.value))?;
        }
        Ok(())
    }

    fn visit_function_parameter(
        &mut self,
        parameter: &rumoca_core::FunctionParam,
    ) -> Result<(), Self::Error> {
        [&parameter.default, &parameter.min, &parameter.max]
            .into_iter()
            .flatten()
            .try_for_each(|expression| self.visit_expression(expression))?;
        parameter
            .shape_expr
            .iter()
            .try_for_each(|subscript| self.visit_subscript(subscript))
    }
}

#[cfg(test)]
mod tests {
    use std::convert::Infallible;

    use rumoca_core::{
        BytePos, ComprehensionScalarView, ComprehensionTemplate, EffectiveType, ExternalFunction,
        ExternalFunctionAnnotation, FallibleExpressionVisitor, Function, FunctionParam, Literal,
        SourceId, Statement, StructuredIndexDomain, TypeId,
    };

    use super::*;

    #[derive(Default)]
    struct MarkerVisitor {
        in_function: bool,
        model: Vec<i64>,
        function: Vec<i64>,
    }

    impl MarkerVisitor {
        fn record(&mut self, marker: i64) {
            let owner = if self.in_function {
                &mut self.function
            } else {
                &mut self.model
            };
            owner.push(marker);
        }
    }

    impl FallibleExpressionVisitor for MarkerVisitor {
        type Error = Infallible;

        fn visit_expression(&mut self, expression: &Expression) -> Result<(), Self::Error> {
            if let Expression::Literal {
                value: Literal::Integer(marker),
                ..
            } = expression
            {
                self.record(*marker);
            }
            self.walk_expression(expression)
        }
    }

    impl FallibleStatementVisitor for MarkerVisitor {}

    impl ModelExpressionOwnerVisitor for MarkerVisitor {
        fn enter_function_owners(&mut self) -> Result<(), Self::Error> {
            self.in_function = true;
            Ok(())
        }
    }

    fn marker(value: i64) -> Expression {
        Expression::Literal {
            value: Literal::Integer(value),
            span: Span::DUMMY,
        }
    }

    fn owner_span() -> Span {
        Span::new(
            SourceId::from_source_name("model_expression_owners.mo"),
            BytePos(1),
            BytePos(2),
        )
    }

    fn origin() -> flat::EquationOrigin {
        flat::EquationOrigin::ComponentEquation {
            component: "owner traversal test".to_string(),
        }
    }

    fn structured(
        marker_value: i64,
        first_equation_index: usize,
    ) -> flat::StructuredEquationFamily {
        flat::StructuredEquationFamily {
            domain: StructuredIndexDomain {
                binders: Vec::new(),
            },
            first_equation_index,
            equations_per_point: 1,
            span: owner_span(),
            origin: origin(),
            regular: None,
            template: Some(ComprehensionTemplate {
                body: vec![marker(marker_value)],
                scalar_view: ComprehensionScalarView::BinderSubstitution,
            }),
            interiors_materialized: true,
        }
    }

    fn assertion(markers: [i64; 3]) -> flat::AssertEquation {
        flat::AssertEquation::new(
            marker(markers[0]),
            marker(markers[1]),
            Some(marker(markers[2])),
            Span::DUMMY,
            origin(),
        )
    }

    fn statement(markers: [i64; 3]) -> Statement {
        Statement::Assert {
            condition: marker(markers[0]),
            message: Box::new(marker(markers[1])),
            level: Some(Box::new(marker(markers[2]))),
            span: Span::DUMMY,
        }
    }

    fn parameter(name: &str, first_marker: i64) -> FunctionParam {
        let effective = EffectiveType::new(TypeId::new(7), TypeId::new(7), Vec::new())
            .expect("test function parameter type is exact");
        let mut parameter = FunctionParam::new(name, "Integer", effective, Span::DUMMY);
        parameter.default = Some(marker(first_marker));
        parameter.min = Some(marker(first_marker + 1));
        parameter.max = Some(marker(first_marker + 2));
        parameter.shape_expr = vec![Subscript::expr(
            Box::new(marker(first_marker + 3)),
            Span::DUMMY,
        )];
        parameter
    }

    fn add_function(model: &mut flat::Model) {
        let mut function = Function::new("f", rumoca_core::DefId::new(64_001), Span::DUMMY);
        function.inputs.push(parameter("input", 21));
        function.outputs.push(parameter("output", 25));
        function.locals.push(parameter("local", 29));
        function.body.push(statement([33, 34, 35]));
        function.external = Some(ExternalFunction {
            language: "C".to_owned(),
            function_name: Some("f_ext".to_owned()),
            output_name: None,
            args: vec![marker(36)],
            annotations: vec![ExternalFunctionAnnotation {
                name: vec!["Library".to_owned()],
                value: marker(37),
                span: owner_span(),
            }],
        });
        model.add_function(function);
    }

    fn complete_owner_model() -> flat::Model {
        let mut model = flat::Model::new();
        model.add_equation(flat::Equation::new(marker(1), Span::DUMMY, origin()));
        model.add_equation(flat::Equation::new(marker(3), Span::DUMMY, origin()));
        model.add_initial_equation(flat::Equation::new(marker(2), Span::DUMMY, origin()));
        model.add_initial_equation(flat::Equation::new(marker(4), Span::DUMMY, origin()));
        model.add_structured_equation(structured(3, 1));
        model.add_initial_structured_equation(structured(4, 1));
        model.assert_equations.push(assertion([5, 6, 7]));
        model.initial_assert_equations.push(assertion([8, 9, 10]));
        model.algorithms.push(flat::Algorithm::new(
            vec![statement([11, 12, 13])],
            Span::DUMMY,
            "model",
        ));
        model.initial_algorithms.push(flat::Algorithm::new(
            vec![statement([14, 15, 16])],
            Span::DUMMY,
            "initial model",
        ));
        let mut branch = flat::WhenBranch::new(marker(17), Span::DUMMY);
        branch.add_equation(flat::WhenEquation::assign(
            VarName::new("x"),
            marker(18),
            Span::DUMMY,
            "when owner",
        ));
        model
            .when_chains
            .push(flat::WhenChain::new(branch, Span::DUMMY));
        add_function(&mut model);
        model
    }

    #[test]
    fn complete_owner_traversal_covers_every_model_family_before_function_scope() {
        let model = complete_owner_model();
        let owners = model
            .structured_equation_owners()
            .expect("owner fixture has one checked structured partition");
        let mut visitor = MarkerVisitor::default();
        visitor
            .visit_model_owners(&owners)
            .expect("marker traversal is infallible");
        assert_eq!(
            visitor.model,
            [1, 3, 2, 4].into_iter().chain(5..=18).collect::<Vec<_>>()
        );
        assert_eq!(visitor.function, (21..=37).collect::<Vec<_>>());
    }
}
