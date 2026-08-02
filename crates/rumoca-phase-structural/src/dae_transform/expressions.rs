//! Translate every source expression into the rebuilt construction.
//!
//! [`ExpressionRebuilder`] memoizes by source ordinal, so a shared subgraph is
//! rebuilt once and every later reference resolves to the same target
//! identity. Coordinate translation is where the demotion becomes visible: a
//! derivative of the demoted state is not rebuilt as a coordinate at all but
//! replaced by the exact symbolic derivative of its definition, which
//! [`differentiation`](super::differentiation) supplies.

use rumoca_ir_dae as dae;

use super::DirectStateConstraint;
use super::constraints::DifferentiationFacts;
use super::declarations::RebuiltDomain;
use super::differentiation::Derivative;
use super::functions::RebuiltFunction;
use super::temporal::RebuiltClock;
use super::variables::{ReservedVariable, TargetVariable};

pub(super) struct ExpressionRebuilder<'source, 'borrow, 'storage, 'target> {
    pub(super) source: dae::DaeView<'source>,
    pub(super) target: &'borrow mut dae::Expressions<'storage, 'target>,
    types: &'borrow [dae::ValueTypeId<'target>],
    functions: &'borrow [RebuiltFunction<'target>],
    pub(super) variables: &'borrow [ReservedVariable<'target>],
    domains: &'borrow [RebuiltDomain<'target>],
    conditions: &'borrow [dae::ConditionId<'target>],
    clocks: &'borrow [RebuiltClock<'target>],
    previous: &'borrow [dae::PreviousId<'target>],
    terminals: &'borrow [dae::TerminalId<'target>],
    pub(super) facts: &'borrow DifferentiationFacts,
    candidate: Option<DirectStateConstraint>,
    rebuilt: &'borrow mut [Option<dae::ExprId<'target>>],
}

#[derive(Clone, Copy)]
pub(super) struct RebuiltBaseIdentities<'borrow, 'target> {
    pub(super) types: &'borrow [dae::ValueTypeId<'target>],
    pub(super) variables: &'borrow [ReservedVariable<'target>],
    pub(super) domains: &'borrow [RebuiltDomain<'target>],
    pub(super) conditions: &'borrow [dae::ConditionId<'target>],
    pub(super) clocks: &'borrow [RebuiltClock<'target>],
    pub(super) previous: &'borrow [dae::PreviousId<'target>],
    pub(super) terminals: &'borrow [dae::TerminalId<'target>],
}

#[derive(Clone, Copy)]
pub(super) struct RebuiltIdentities<'borrow, 'target> {
    pub(super) base: RebuiltBaseIdentities<'borrow, 'target>,
    pub(super) functions: &'borrow [RebuiltFunction<'target>],
}

impl<'source, 'borrow, 'storage, 'target> ExpressionRebuilder<'source, 'borrow, 'storage, 'target> {
    pub(super) fn new(
        source: dae::DaeView<'source>,
        target: &'borrow mut dae::Expressions<'storage, 'target>,
        identities: RebuiltIdentities<'borrow, 'target>,
        facts: &'borrow DifferentiationFacts,
        candidate: Option<DirectStateConstraint>,
        rebuilt: &'borrow mut [Option<dae::ExprId<'target>>],
    ) -> Self {
        Self {
            source,
            target,
            types: identities.base.types,
            functions: identities.functions,
            variables: identities.base.variables,
            domains: identities.base.domains,
            conditions: identities.base.conditions,
            clocks: identities.base.clocks,
            previous: identities.base.previous,
            terminals: identities.base.terminals,
            facts,
            candidate,
            rebuilt,
        }
    }

    pub(super) fn rebuild(
        &mut self,
        source_id: dae::ExprId<'source>,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        let index = source_id.index() as usize;
        if let Some(rebuilt) = self.rebuilt[index] {
            return Ok(rebuilt);
        }
        let source = self
            .source
            .expression(source_id)
            .expect("finalized expression identity resolves");
        let provenance = source.provenance();
        let value_type = self.types[source.value_type_id().index() as usize];
        let rebuilt = match source.operation() {
            dae::ExpressionOperation::Literal(literal) => {
                self.rebuild_literal(literal, provenance)?
            }
            dae::ExpressionOperation::Coordinate(coordinate) => {
                self.rebuild_coordinate(coordinate, provenance)?
            }
            dae::ExpressionOperation::Unary { operator, operand } => {
                let operand = self.rebuild(operand)?;
                self.target.at(provenance).unary(operator, operand)?
            }
            dae::ExpressionOperation::Binary { operator, lhs, rhs } => {
                let lhs = self.rebuild(lhs)?;
                let rhs = self.rebuild(rhs)?;
                self.target.at(provenance).binary(operator, lhs, rhs)?
            }
            dae::ExpressionOperation::StringConversion {
                declaration,
                value,
                format,
            } => self.rebuild_string_conversion(declaration, value, format, provenance)?,
            dae::ExpressionOperation::Conditional(operands) => {
                self.rebuild_conditional(operands, provenance)?
            }
            dae::ExpressionOperation::Array(operands) => {
                if operands.is_empty() {
                    self.target.at(provenance).empty_array(value_type)?
                } else {
                    let elements = self.rebuild_operands(operands)?;
                    self.target.at(provenance).array(elements)?
                }
            }
            dae::ExpressionOperation::Record(fields) => {
                let fields = self.rebuild_operands(fields)?;
                self.target.at(provenance).record(value_type, fields)?
            }
            dae::ExpressionOperation::Field { base, field } => {
                let base = self.rebuild(base)?;
                self.target.at(provenance).field(base, field as usize)?
            }
            dae::ExpressionOperation::Range(range) => self.rebuild_range(range, provenance)?,
            dae::ExpressionOperation::Comprehension { domain, body } => {
                let body = self.rebuild(body)?;
                self.target
                    .at(provenance)
                    .comprehension(self.domains[domain.index() as usize].id, body)?
            }
            dae::ExpressionOperation::Index { base, subscripts } => {
                let base = self.rebuild(base)?;
                let subscripts = self.rebuild_subscripts(subscripts)?;
                self.target.at(provenance).index(base, subscripts)?
            }
            dae::ExpressionOperation::ArrayUpdate {
                base,
                value,
                subscripts,
            } => {
                let base = self.rebuild(base)?;
                let value = self.rebuild(value)?;
                let subscripts = self.rebuild_subscripts(subscripts)?;
                self.target
                    .at(provenance)
                    .array_update(base, value, subscripts)?
            }
            dae::ExpressionOperation::Builtin { builtin, arguments } => {
                let arguments = self.rebuild_operands(arguments)?;
                self.target.at(provenance).builtin(builtin, arguments)?
            }
            dae::ExpressionOperation::Call {
                function,
                output,
                arguments,
            } => {
                let arguments = self.rebuild_operands(arguments)?;
                self.target.at(provenance).call(
                    self.functions[function.index() as usize].id,
                    output as usize,
                    arguments,
                )?
            }
            dae::ExpressionOperation::FunctionValue { .. }
            | dae::ExpressionOperation::FunctionFoldParameter { .. }
            | dae::ExpressionOperation::FunctionFoldOutput { .. } => {
                unreachable!("function-owner reconstruction seeds scoped expression identities")
            }
        };
        self.rebuilt[index] = Some(rebuilt);
        Ok(rebuilt)
    }

    /// Replay one source literal through the constructor that checks it.
    ///
    /// The generic literal entry point rejects enumeration values outright so
    /// that an ordinal always passes its one-based check, which makes the
    /// dedicated enumeration constructor the only way to replay one.
    fn rebuild_literal(
        &mut self,
        literal: &dae::DaeLiteral,
        provenance: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        match literal {
            dae::DaeLiteral::Enumeration(ordinal) => {
                self.target.at(provenance).enumeration_literal(*ordinal)
            }
            literal => self.target.at(provenance).literal(literal.clone()),
        }
    }

    fn rebuild_string_conversion(
        &mut self,
        declaration: rumoca_core::DefId,
        value: dae::ExprId<'source>,
        format: dae::StringConversionFormatView<'source>,
        provenance: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        let value = self.rebuild(value)?;
        let format = match format {
            dae::StringConversionFormatView::Options {
                minimum_length,
                left_justified,
                significant_digits,
            } => dae::StringConversionFormatInput::Options {
                minimum_length: minimum_length
                    .map(|value| self.rebuild(value))
                    .transpose()?,
                left_justified: left_justified
                    .map(|value| self.rebuild(value))
                    .transpose()?,
                significant_digits: significant_digits
                    .map(|value| self.rebuild(value))
                    .transpose()?,
            },
            dae::StringConversionFormatView::Format { value } => {
                dae::StringConversionFormatInput::Format {
                    value: self.rebuild(value)?,
                }
            }
        };
        self.target
            .at(provenance)
            .string_conversion(declaration, value, format)
    }

    fn rebuild_operands(
        &mut self,
        operands: dae::ExpressionOperands<'source>,
    ) -> Result<Vec<dae::ExprId<'target>>, dae::DaeConstructionError> {
        operands
            .iter()
            .map(|operand| self.rebuild(operand))
            .collect()
    }

    fn rebuild_range(
        &mut self,
        range: dae::RangeView<'source>,
        provenance: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        let start = self.rebuild(range.start().expression())?;
        let explicit_step = range
            .explicit_step()
            .map(|step| self.rebuild(step.expression()))
            .transpose()?;
        let stop = self.rebuild(range.stop().expression())?;
        self.target.at(provenance).range(start, explicit_step, stop)
    }

    fn rebuild_conditional(
        &mut self,
        operands: dae::ExpressionOperands<'source>,
        provenance: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        let fallback = self.rebuild(
            operands
                .get(operands.len() - 1)
                .expect("checked conditional has a fallback"),
        )?;
        let mut branches = Vec::with_capacity((operands.len() - 1) / 2);
        for index in (0..operands.len() - 1).step_by(2) {
            branches.push((
                self.rebuild(
                    operands
                        .get(index)
                        .expect("checked conditional branch has a condition"),
                )?,
                self.rebuild(
                    operands
                        .get(index + 1)
                        .expect("checked conditional branch has a value"),
                )?,
            ));
        }
        self.target.at(provenance).conditional(branches, fallback)
    }

    fn rebuild_subscripts(
        &mut self,
        subscripts: dae::SubscriptsView<'source>,
    ) -> Result<Vec<dae::Subscript<'target>>, dae::DaeConstructionError> {
        subscripts
            .iter()
            .map(|subscript| match subscript {
                dae::SubscriptView::Index {
                    expression,
                    provenance,
                } => Ok(dae::Subscript::Index {
                    expression: self.rebuild(expression)?,
                    provenance,
                }),
                dae::SubscriptView::Whole { provenance } => {
                    Ok(dae::Subscript::Whole { provenance })
                }
                dae::SubscriptView::Slice {
                    expression,
                    provenance,
                } => Ok(dae::Subscript::Slice {
                    expression: self.rebuild(expression)?,
                    provenance,
                }),
            })
            .collect()
    }

    fn rebuild_coordinate(
        &mut self,
        coordinate: dae::CoordinateView<'source>,
        provenance: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        if let (Some(candidate), dae::CoordinateView::Derivative(state)) =
            (self.candidate, coordinate)
            && state.index() == candidate.state
        {
            return self.rebuild_demoted_derivative(candidate);
        }
        let coordinate = match coordinate {
            dae::CoordinateView::Parameter(id) => {
                match self.variables[id.index() as usize].identity {
                    TargetVariable::Parameter(id) => dae::CoordinateInput::Parameter(id),
                    _ => unreachable!("parameter role is preserved"),
                }
            }
            dae::CoordinateView::Input(id) => match self.variables[id.index() as usize].identity {
                TargetVariable::Input(id) => dae::CoordinateInput::Input(id),
                _ => unreachable!("input role is preserved"),
            },
            dae::CoordinateView::State(id) => match self.variables[id.index() as usize].identity {
                TargetVariable::State(id) => dae::CoordinateInput::State(id),
                TargetVariable::Algebraic(id) => dae::CoordinateInput::Algebraic(id),
                _ => unreachable!("state becomes a state or algebraic"),
            },
            dae::CoordinateView::Derivative(id) => {
                match self.variables[id.index() as usize].identity {
                    TargetVariable::State(id) => dae::CoordinateInput::Derivative(id),
                    _ => unreachable!("only the selected state derivative is substituted"),
                }
            }
            dae::CoordinateView::Algebraic(id) => {
                match self.variables[id.index() as usize].identity {
                    TargetVariable::Algebraic(id) => dae::CoordinateInput::Algebraic(id),
                    _ => unreachable!("algebraic role is preserved"),
                }
            }
            // Index reduction can demote a state to an algebraic, and the
            // event-entry left limit follows the coordinate to its new role.
            //
            // The demoting branch is defensive: no model is currently known
            // that both takes `pre()` of a state and reduces that same state
            // away — the reviewer's attempts (a high-index pendulum and a
            // constrained-drive shape) fail earlier in flattening — so the
            // `Algebraic` arm below is unexercised by the suite. It is written
            // out rather than left to `unreachable!` because the demotion is a
            // legal transform whose result is well defined.
            dae::CoordinateView::PreState(id) => {
                match self.variables[id.index() as usize].identity {
                    TargetVariable::State(id) => dae::CoordinateInput::PreState(id),
                    TargetVariable::Algebraic(id) => dae::CoordinateInput::PreAlgebraic(id),
                    _ => unreachable!("state becomes a state or algebraic"),
                }
            }
            dae::CoordinateView::PreAlgebraic(id) => {
                match self.variables[id.index() as usize].identity {
                    TargetVariable::Algebraic(id) => dae::CoordinateInput::PreAlgebraic(id),
                    _ => unreachable!("algebraic role is preserved"),
                }
            }
            dae::CoordinateView::Time => dae::CoordinateInput::Time,
            dae::CoordinateView::ClockInterval(source) => {
                dae::CoordinateInput::ClockInterval(self.clocks[source.index() as usize].periodic())
            }
            discrete @ (dae::CoordinateView::DiscreteReal(_)
            | dae::CoordinateView::DiscreteValue(_)
            | dae::CoordinateView::PreDiscreteReal(_)
            | dae::CoordinateView::PreDiscreteValue(_)) => {
                return self.rebuild_discrete_coordinate(discrete, provenance);
            }
            dae::CoordinateView::Condition(source) => {
                dae::CoordinateInput::Condition(self.conditions[source.index() as usize])
            }
            dae::CoordinateView::Previous(source) => {
                dae::CoordinateInput::Previous(self.previous[source.index() as usize])
            }
            dae::CoordinateView::Terminal(source) => {
                dae::CoordinateInput::Terminal(self.terminals[source.index() as usize])
            }
            dae::CoordinateView::Binder(binder) => {
                return self.target.at(provenance).binder(
                    self.domains[binder.domain().index() as usize].binders
                        [binder.ordinal() as usize],
                );
            }
            dae::CoordinateView::FunctionParameter(parameter) => {
                return self.target.at(provenance).function_parameter(
                    self.functions[parameter.function().index() as usize].parameters
                        [parameter.ordinal() as usize],
                );
            }
            dae::CoordinateView::Delay(_) => {
                unreachable!("delay-owner reconstruction seeds its coordinate identity")
            }
        };
        self.target.at(provenance).coordinate(coordinate)
    }

    fn rebuild_demoted_derivative(
        &mut self,
        candidate: DirectStateConstraint,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        let generated = dae::DaeProvenance::generated(
            dae::DaeGeneration::IndexReduction,
            candidate.owner.span(),
        )?;
        let derivative = self.differentiate(
            self.source
                .expression_id(candidate.rhs as usize)
                .expect("candidate RHS resolves"),
            generated,
        )?;
        match (candidate.rhs_sign, derivative) {
            (_, Derivative::Zero) => self
                .target
                .at(generated)
                .literal(dae::DaeLiteral::Real(0.0)),
            (super::equalities::EqualitySign::Same, Derivative::Expression(expression)) => {
                Ok(expression)
            }
            (super::equalities::EqualitySign::Opposite, Derivative::Expression(expression)) => self
                .target
                .at(generated)
                .unary(dae::UnaryOperator::Negate, expression),
        }
    }

    fn rebuild_discrete_coordinate(
        &mut self,
        coordinate: dae::CoordinateView<'source>,
        provenance: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        let coordinate = match coordinate {
            dae::CoordinateView::DiscreteReal(source) => {
                let TargetVariable::DiscreteReal(target) =
                    self.variables[source.index() as usize].identity
                else {
                    unreachable!("discrete-real coordinate retains its variable role")
                };
                dae::CoordinateInput::DiscreteReal(target)
            }
            dae::CoordinateView::DiscreteValue(source) => {
                let TargetVariable::DiscreteValue(target) =
                    self.variables[source.index() as usize].identity
                else {
                    unreachable!("discrete-value coordinate retains its variable role")
                };
                dae::CoordinateInput::DiscreteValue(target)
            }
            dae::CoordinateView::PreDiscreteReal(source) => {
                let TargetVariable::DiscreteReal(target) =
                    self.variables[source.index() as usize].identity
                else {
                    unreachable!("pre(discrete-real) retains its variable role")
                };
                dae::CoordinateInput::PreDiscreteReal(target)
            }
            dae::CoordinateView::PreDiscreteValue(source) => {
                let TargetVariable::DiscreteValue(target) =
                    self.variables[source.index() as usize].identity
                else {
                    unreachable!("pre(discrete-value) retains its variable role")
                };
                dae::CoordinateInput::PreDiscreteValue(target)
            }
            _ => unreachable!("caller passes a discrete coordinate"),
        };
        self.target.at(provenance).coordinate(coordinate)
    }
}
