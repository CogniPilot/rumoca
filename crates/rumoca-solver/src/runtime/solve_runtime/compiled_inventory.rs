use super::*;

/// Required source expressions of a compiled scalar runtime. A missing kernel
/// has no representation in this inventory.
#[derive(Clone)]
pub(super) struct CompiledExpressions {
    pub(super) event_transactions: Box<[Rc<dyn CompiledSolveEventTransaction>]>,
    pub(super) lifecycle: CompiledLifecycle,
    pub(super) implicit_rhs: Rc<dyn CompiledSolveExpression>,
    pub(super) implicit_projection_jacobian_v: Rc<dyn CompiledSolveJacobianExpression>,
    pub(super) implicit_full_jacobian_v: Rc<dyn CompiledSolveJacobianExpression>,
    pub(super) initial_residual: Rc<dyn CompiledSolveExpression>,
    pub(super) initial_residual_jacobian_v: Rc<dyn CompiledSolveJacobianExpression>,
    pub(super) derivative_rhs: Rc<dyn CompiledSolveExpression>,
    pub(super) derivative_jacobian_v: Rc<dyn CompiledSolveJacobianExpression>,
    pub(super) root_conditions: Rc<dyn CompiledSolveExpression>,
}

#[derive(Clone)]
pub(super) enum PreparedExpressions {
    Reference,
    Compiled(Box<CompiledExpressions>),
    #[cfg(test)]
    Fixture(Box<FixtureExpressions>),
}

pub(super) fn require_prepared<T>(label: &str, prepared: Option<T>) -> Result<T, EvalSolveError> {
    prepared.ok_or_else(|| EvalSolveError::CompiledExecution {
        message: format!("required compiled entry `{label}` was not prepared"),
        span: None,
    })
}

impl ScalarExecutionInventory {
    pub(super) fn compiled_event_transaction(
        &self,
        index: usize,
    ) -> Result<Option<&Rc<dyn CompiledSolveEventTransaction>>, RuntimeSolveError> {
        match &self.expressions {
            PreparedExpressions::Reference => Ok(None),
            PreparedExpressions::Compiled(code) => {
                code.event_transactions.get(index).map(Some).ok_or_else(|| {
                    RuntimeSolveError::solve_ir(
                        "required event transaction is outside its prepared source",
                    )
                })
            }
            #[cfg(test)]
            PreparedExpressions::Fixture(code) => Ok(code.event_transactions.get(index)),
        }
    }
    pub(super) fn compiled_implicit_rhs(&self) -> Option<&Rc<dyn CompiledSolveExpression>> {
        match &self.expressions {
            PreparedExpressions::Reference => None,
            PreparedExpressions::Compiled(code) => Some(&code.implicit_rhs),
            #[cfg(test)]
            PreparedExpressions::Fixture(code) => code.implicit_rhs.as_ref(),
        }
    }
    pub(super) fn compiled_implicit_projection_jacobian_v(
        &self,
    ) -> Option<&Rc<dyn CompiledSolveJacobianExpression>> {
        match &self.expressions {
            PreparedExpressions::Reference => None,
            PreparedExpressions::Compiled(code) => Some(&code.implicit_projection_jacobian_v),
            #[cfg(test)]
            PreparedExpressions::Fixture(code) => code.implicit_projection_jacobian_v.as_ref(),
        }
    }
    pub(super) fn compiled_implicit_full_jacobian_v(
        &self,
    ) -> Option<&Rc<dyn CompiledSolveJacobianExpression>> {
        match &self.expressions {
            PreparedExpressions::Reference => None,
            PreparedExpressions::Compiled(code) => Some(&code.implicit_full_jacobian_v),
            #[cfg(test)]
            PreparedExpressions::Fixture(code) => code.implicit_full_jacobian_v.as_ref(),
        }
    }
    pub(super) fn compiled_initial_residual(&self) -> Option<&Rc<dyn CompiledSolveExpression>> {
        match &self.expressions {
            PreparedExpressions::Reference => None,
            PreparedExpressions::Compiled(code) => Some(&code.initial_residual),
            #[cfg(test)]
            PreparedExpressions::Fixture(code) => code.initial_residual.as_ref(),
        }
    }
    pub(super) fn compiled_initial_residual_jacobian_v(
        &self,
    ) -> Option<&Rc<dyn CompiledSolveJacobianExpression>> {
        match &self.expressions {
            PreparedExpressions::Reference => None,
            PreparedExpressions::Compiled(code) => Some(&code.initial_residual_jacobian_v),
            #[cfg(test)]
            PreparedExpressions::Fixture(code) => code.initial_residual_jacobian_v.as_ref(),
        }
    }
    pub(super) fn compiled_derivative_rhs(&self) -> Option<&Rc<dyn CompiledSolveExpression>> {
        match &self.expressions {
            PreparedExpressions::Reference => None,
            PreparedExpressions::Compiled(code) => Some(&code.derivative_rhs),
            #[cfg(test)]
            PreparedExpressions::Fixture(code) => code.derivative_rhs.as_ref(),
        }
    }
    pub(super) fn compiled_derivative_jacobian_v(
        &self,
    ) -> Option<&Rc<dyn CompiledSolveJacobianExpression>> {
        match &self.expressions {
            PreparedExpressions::Reference => None,
            PreparedExpressions::Compiled(code) => Some(&code.derivative_jacobian_v),
            #[cfg(test)]
            PreparedExpressions::Fixture(code) => code.derivative_jacobian_v.as_ref(),
        }
    }
    pub(super) fn compiled_root_conditions(&self) -> Option<&Rc<dyn CompiledSolveExpression>> {
        match &self.expressions {
            PreparedExpressions::Reference => None,
            PreparedExpressions::Compiled(code) => Some(&code.root_conditions),
            #[cfg(test)]
            PreparedExpressions::Fixture(code) => code.root_conditions.as_ref(),
        }
    }
}

#[cfg(test)]
#[derive(Clone, Default)]
pub(super) struct FixtureExpressions {
    implicit_rhs: Option<Rc<dyn CompiledSolveExpression>>,
    implicit_projection_jacobian_v: Option<Rc<dyn CompiledSolveJacobianExpression>>,
    implicit_full_jacobian_v: Option<Rc<dyn CompiledSolveJacobianExpression>>,
    initial_residual: Option<Rc<dyn CompiledSolveExpression>>,
    initial_residual_jacobian_v: Option<Rc<dyn CompiledSolveJacobianExpression>>,
    derivative_rhs: Option<Rc<dyn CompiledSolveExpression>>,
    derivative_jacobian_v: Option<Rc<dyn CompiledSolveJacobianExpression>>,
    root_conditions: Option<Rc<dyn CompiledSolveExpression>>,
    event_transactions: Vec<Rc<dyn CompiledSolveEventTransaction>>,
}

#[cfg(test)]
impl ScalarExecutionInventory {
    fn fixture_expressions_mut(&mut self) -> &mut FixtureExpressions {
        if !matches!(self.expressions, PreparedExpressions::Fixture(_)) {
            let code = match &self.expressions {
                PreparedExpressions::Reference => FixtureExpressions::default(),
                PreparedExpressions::Compiled(code) => FixtureExpressions {
                    implicit_rhs: Some(Rc::clone(&code.implicit_rhs)),
                    implicit_projection_jacobian_v: Some(Rc::clone(
                        &code.implicit_projection_jacobian_v,
                    )),
                    implicit_full_jacobian_v: Some(Rc::clone(&code.implicit_full_jacobian_v)),
                    initial_residual: Some(Rc::clone(&code.initial_residual)),
                    initial_residual_jacobian_v: Some(Rc::clone(&code.initial_residual_jacobian_v)),
                    derivative_rhs: Some(Rc::clone(&code.derivative_rhs)),
                    derivative_jacobian_v: Some(Rc::clone(&code.derivative_jacobian_v)),
                    root_conditions: Some(Rc::clone(&code.root_conditions)),
                    event_transactions: code.event_transactions.to_vec(),
                },
                PreparedExpressions::Fixture(_) => unreachable!("fixture handled above"),
            };
            self.expressions = PreparedExpressions::Fixture(Box::new(code));
        }
        match &mut self.expressions {
            PreparedExpressions::Fixture(code) => code,
            _ => unreachable!("the test fixture was installed"),
        }
    }
    pub(super) fn fixture_compiled_implicit_rhs(
        &mut self,
    ) -> &mut Option<Rc<dyn CompiledSolveExpression>> {
        &mut self.fixture_expressions_mut().implicit_rhs
    }
    pub(super) fn fixture_compiled_implicit_projection_jacobian_v(
        &mut self,
    ) -> &mut Option<Rc<dyn CompiledSolveJacobianExpression>> {
        &mut self
            .fixture_expressions_mut()
            .implicit_projection_jacobian_v
    }
    pub(super) fn fixture_compiled_implicit_full_jacobian_v(
        &mut self,
    ) -> &mut Option<Rc<dyn CompiledSolveJacobianExpression>> {
        &mut self.fixture_expressions_mut().implicit_full_jacobian_v
    }
    pub(super) fn fixture_compiled_initial_residual(
        &mut self,
    ) -> &mut Option<Rc<dyn CompiledSolveExpression>> {
        &mut self.fixture_expressions_mut().initial_residual
    }
    pub(super) fn fixture_compiled_initial_residual_jacobian_v(
        &mut self,
    ) -> &mut Option<Rc<dyn CompiledSolveJacobianExpression>> {
        &mut self.fixture_expressions_mut().initial_residual_jacobian_v
    }
}
