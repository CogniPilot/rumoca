//! Translate every source expression into the rebuilt construction.
//!
//! [`ExpressionRebuilder`] memoizes by source ordinal, so a shared subgraph is
//! rebuilt once and every later reference resolves to the same target
//! identity. Coordinate translation is where the demotion becomes visible: a
//! derivative of the demoted state is not rebuilt as a coordinate at all but
//! replaced by the exact symbolic derivative of its definition, which
//! [`differentiation`](super::differentiation) supplies.

mod scoped_cache;

use rumoca_ir_dae as dae;

use scoped_cache::ScopedReconstructionCache;

use super::constraints::DifferentiationFacts;
use super::declarations::RebuiltDomain;
use super::differentiation::Derivative;
use super::functions::RebuiltFunction;
use super::temporal::RebuiltClock;
use super::variables::{ReservedVariable, TargetVariable};
use super::{DirectStateConstraint, StateDefinition};
use rumoca_eval_dae::FunctionCallContext;

pub(super) fn shaped_zero<'target>(
    target: &mut dae::Expressions<'_, 'target>,
    dimensions: &[u32],
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
    if dimensions.is_empty() {
        return target.at(provenance).literal(dae::DaeLiteral::Real(0.0));
    }
    let extents = dimensions
        .iter()
        .map(|&extent| {
            target
                .at(provenance)
                .literal(dae::DaeLiteral::Integer(i64::from(extent)))
        })
        .collect::<Result<Vec<_>, _>>()?;
    target
        .at(provenance)
        .builtin(dae::PureBuiltin::Zeros, extents)
}

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
    pub(super) auxiliary_functions: &'borrow super::auxiliary_blocks::AuxiliaryFunctions<'target>,
    pub(super) auxiliary_expressions: std::collections::BTreeMap<
        (u32, u8, bool),
        super::auxiliary_blocks::AuxiliaryExpression<'target>,
    >,
    pub(super) candidate: Option<DirectStateConstraint>,
    substitute_demoted_value: bool,
    pub(super) state_only_derivative: bool,
    pub(super) formal_derivatives: bool,
    pub(super) derivative_anchors: super::equalities::DerivativeAnchors,
    pub(super) function_context: FunctionCallContext<'source>,
    pub(super) scoped_cache: ScopedReconstructionCache<'source, 'target>,
    /// STRUCT-T10(b): source call nodes replaced by their substituted body.
    inline_calls: &'borrow [bool],
    rebuilt: &'borrow mut [Option<dae::ExprId<'target>>],
}

#[derive(Clone, Copy)]
pub(super) struct RebuiltBaseIdentities<'borrow, 'target> {
    pub(super) auxiliary_functions: &'borrow super::auxiliary_blocks::AuxiliaryFunctions<'target>,
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
    pub(super) fn rebuilt_derivative(
        &self,
        source: dae::FunctionDerivativeId<'source>,
    ) -> dae::FunctionDerivativeId<'target> {
        self.functions[source.function().index() as usize].derivatives[source.ordinal() as usize]
    }

    pub(super) fn rebuild_call_value(
        &mut self,
        source_id: dae::ExprId<'source>,
        arguments: &[dae::ExprId<'target>],
        provenance: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        let source = self.source.expression(source_id).expect("checked call");
        let dae::ExpressionOperation::Call {
            owner,
            function,
            output,
            ..
        } = source.operation()
        else {
            unreachable!("call reconstruction consumes a call")
        };
        if owner != source_id {
            let owner = self.rebuild_call_value(owner, arguments, provenance)?;
            return self.target.at(provenance).call_projection(
                owner,
                self.functions[function.index() as usize].id,
                output as usize,
                arguments.iter().copied(),
            );
        }
        if let Some((primal, link)) = source.call_derivative() {
            let dae::ExpressionOperation::Call {
                arguments: prefix, ..
            } = self.source.expression(primal).unwrap().operation()
            else {
                unreachable!("derivative origin is a checked call")
            };
            let primal = self.rebuild_call_value(primal, &arguments[..prefix.len()], provenance)?;
            let link = self.rebuilt_derivative(link);
            return self.target.at(provenance).differentiated_call(
                primal,
                link.ordinal(),
                arguments.iter().copied(),
            );
        }
        self.target.at(provenance).call(
            self.functions[function.index() as usize].id,
            output as usize,
            arguments.iter().copied(),
        )
    }

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
            auxiliary_functions: identities.base.auxiliary_functions,
            auxiliary_expressions: std::collections::BTreeMap::new(),
            candidate,
            substitute_demoted_value: false,
            state_only_derivative: false,
            formal_derivatives: false,
            derivative_anchors: Default::default(),
            function_context: FunctionCallContext::default(),
            scoped_cache: ScopedReconstructionCache::default(),
            inline_calls: &[],
            rebuilt,
        }
    }

    /// Rebuild a retained manifold after a proved dummy-state demotion,
    /// substituting that state's exact value definition instead of exposing
    /// its new algebraic storage role at the Solve boundary.
    pub(super) fn substituting_demoted_value(mut self) -> Self {
        assert!(self.candidate.is_some());
        self.substitute_demoted_value = true;
        self
    }

    /// Replace the flagged source calls by their substituted bodies.
    pub(super) fn inlining_calls(mut self, inline_calls: &'borrow [bool]) -> Self {
        self.inline_calls = inline_calls;
        self
    }

    pub(super) fn with_formal_derivatives(mut self) -> Self {
        assert!(self.candidate.is_none());
        self.formal_derivatives = true;
        self
    }

    pub(super) fn rebuild(
        &mut self,
        source_id: dae::ExprId<'source>,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        let index = source_id.index() as usize;
        if let Some(rebuilt) = self.rebuilt[index] {
            return Ok(rebuilt);
        }
        if let Some(owner) = self.source.runtime_quotient_owner(source_id) {
            let provenance = self
                .source
                .expression(source_id)
                .expect("finalized quotient expression resolves")
                .provenance();
            return Err(dae::DaeConstructionError::IncompleteDefinition {
                kind: match owner.kind() {
                    dae::RuntimeQuotientOwnerKind::ModelEvent { .. } => {
                        "runtime quotient model owner replay"
                    }
                    dae::RuntimeQuotientOwnerKind::FunctionBody { .. } => {
                        "runtime quotient function owner replay"
                    }
                },
                index: source_id.index(),
                span: provenance.span(),
            });
        }
        let source = self
            .source
            .expression(source_id)
            .expect("finalized expression identity resolves");
        let provenance = source.provenance();
        let value_type = self.types[source.value_type_id().index() as usize];
        let rebuilt =
            self.rebuild_operation(source_id, source.operation(), provenance, value_type)?;
        self.rebuilt[index] = Some(rebuilt);
        Ok(rebuilt)
    }

    /// Rebuild an expression from a checked Modelica function body after
    /// substituting the exact arguments of the call being differentiated.
    pub(super) fn rebuild_instantiated(
        &mut self,
        source_id: dae::ExprId<'source>,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        let scoped = self
            .function_context
            .scoped_to_expression(self.source, source_id);
        let previous = std::mem::replace(&mut self.function_context, scoped);
        let provenance = self.source.expression(source_id).unwrap().provenance();
        let key = self.scoped_reconstruction_key(source_id, 0, provenance);
        let rebuilt = match self.scoped_cache.instantiated.get(&key).copied() {
            Some(value) => Ok(value),
            None => {
                let result = self
                    .rebuild_instantiated_scoped(source_id)
                    .and_then(|value| self.preserve_real_value_type(source_id, value, provenance));
                if let Ok(value) = result {
                    self.scoped_cache.instantiated.insert(key, value);
                }
                result
            }
        };
        self.function_context = previous;
        rebuilt
    }

    /// STRUCT-T10(b): the single straight-line result of an admitted call,
    /// with the call's own arguments substituted.
    fn rebuild_inlined_call(
        &mut self,
        call: dae::ExprId<'source>,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        let (result, context) = FunctionCallContext::default()
            .call_result(self.source, call)
            .expect("the inline plan admits only single-assignment callees");
        let previous = std::mem::replace(&mut self.function_context, context);
        let value = self.rebuild_instantiated(result);
        self.function_context = previous;
        value
    }

    /// Substitution retains the MLS §10.6.13 promotion at a Real argument,
    /// result, or value anchor. Scalar scaling preserves compact tensor shapes
    /// and lets the ordinary mixed-numeric constructor derive the Real type.
    pub(super) fn preserve_real_value_type(
        &mut self,
        source_id: dae::ExprId<'source>,
        value: dae::ExprId<'target>,
        provenance: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        let expected = self.source.expression(source_id).unwrap().value_type();
        if expected.scalar_type() != dae::ScalarType::Real
            || self.target.value_type(value, provenance)?.scalar_type() != dae::ScalarType::Integer
        {
            return Ok(value);
        }
        let one = self
            .target
            .at(provenance)
            .literal(dae::DaeLiteral::Real(1.0))?;
        self.target
            .at(provenance)
            .binary(dae::BinaryOperator::Multiply, one, value)
    }

    fn rebuild_instantiated_scoped(
        &mut self,
        source_id: dae::ExprId<'source>,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        if let Some(branch) = self
            .function_context
            .selected_branch(self.source, source_id)
        {
            return self.rebuild_instantiated(branch);
        }
        let source = self
            .source
            .expression(source_id)
            .expect("instantiated expression resolves");
        if let dae::ExpressionOperation::Call { arguments, .. } = source.operation() {
            if source.function_scope().is_none() && source.binder_domain().is_none() {
                return self.rebuild(source_id);
            }
            let arguments = arguments
                .iter()
                .map(|argument| self.rebuild_instantiated(argument))
                .collect::<Result<Vec<_>, _>>()?;
            return self.rebuild_call_value(source_id, &arguments, source.provenance());
        }
        if source.function_scope().is_none() && source.binder_domain().is_none() {
            return self.rebuild(source_id);
        }
        let provenance = source.provenance();
        match source.operation() {
            dae::ExpressionOperation::Literal(literal) => self.rebuild_literal(literal, provenance),
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Binder(_)) => {
                self.rebuild(source_id)
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(
                parameter,
            )) => {
                let argument = self
                    .function_context
                    .parameter_argument(parameter)
                    .expect("instantiation proof resolves this function parameter");
                self.rebuild_instantiated(argument)
            }
            dae::ExpressionOperation::Unary { operator, operand } => {
                let operand = self.rebuild_instantiated(operand)?;
                self.target.at(provenance).unary(operator, operand)
            }
            dae::ExpressionOperation::Binary { operator, lhs, rhs } => {
                let lhs = self.rebuild_instantiated(lhs)?;
                let rhs = self.rebuild_instantiated(rhs)?;
                self.target.at(provenance).binary(operator, lhs, rhs)
            }
            dae::ExpressionOperation::Array(elements) => {
                let elements = elements
                    .iter()
                    .map(|element| self.rebuild_instantiated(element))
                    .collect::<Result<Vec<_>, _>>()?;
                self.target.at(provenance).array(elements)
            }
            dae::ExpressionOperation::Record(fields) => {
                let fields = fields
                    .iter()
                    .map(|field| self.rebuild_instantiated(field))
                    .collect::<Result<Vec<_>, _>>()?;
                let value_type = self.types[source.value_type_id().index() as usize];
                self.target.at(provenance).record(value_type, fields)
            }
            dae::ExpressionOperation::Field { base, field } => {
                let (projected, projected_context) = self
                    .function_context
                    .projected_field(self.source, base, field)
                    .expect("instantiation proof resolves this record field");
                let previous = std::mem::replace(&mut self.function_context, projected_context);
                let rebuilt = self.rebuild_instantiated(projected);
                self.function_context = previous;
                rebuilt
            }
            dae::ExpressionOperation::Index { base, subscripts } => {
                let base = self.rebuild_instantiated(base)?;
                let subscripts = self.rebuild_instantiated_subscripts(subscripts)?;
                self.target.at(provenance).index(base, subscripts)
            }
            dae::ExpressionOperation::Builtin { builtin, arguments } => {
                let arguments = arguments
                    .iter()
                    .map(|argument| self.rebuild_instantiated(argument))
                    .collect::<Result<Vec<_>, _>>()?;
                self.target.at(provenance).builtin(builtin, arguments)
            }
            _ => unreachable!("instantiation preflight rejects this function expression"),
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "exhaustive ExpressionOperation reconstruction keeps every IR variant visible"
    )]
    fn rebuild_operation(
        &mut self,
        source_id: dae::ExprId<'source>,
        operation: dae::ExpressionOperation<'source>,
        provenance: dae::DaeProvenance,
        value_type: dae::ValueTypeId<'target>,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        Ok(match operation {
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
                let function = (builtin == dae::PureBuiltin::LinearSolve
                    && self
                        .source
                        .expression(source_id)
                        .expect("source expression")
                        .function_scope()
                        .is_none())
                .then(|| self.linear_solve_function(arguments));
                let arguments = self.rebuild_operands(arguments)?;
                match function {
                    Some(function) => self.target.at(provenance).call(function, 0, arguments)?,
                    None => self.target.at(provenance).builtin(builtin, arguments)?,
                }
            }
            dae::ExpressionOperation::Call {
                owner,
                function,
                output,
                arguments,
            } => {
                if self
                    .inline_calls
                    .get(source_id.index() as usize)
                    .copied()
                    .unwrap_or(false)
                {
                    return self.rebuild_inlined_call(source_id);
                }
                let arguments = self.rebuild_operands(arguments)?;
                let function = self.functions[function.index() as usize].id;
                if owner == source_id {
                    self.rebuild_call_value(source_id, &arguments, provenance)?
                } else {
                    let owner = self.rebuild(owner)?;
                    self.target.at(provenance).call_projection(
                        owner,
                        function,
                        output as usize,
                        arguments,
                    )?
                }
            }
            dae::ExpressionOperation::FunctionValue { .. }
            | dae::ExpressionOperation::FunctionFoldParameter { .. }
            | dae::ExpressionOperation::FunctionFoldOutput { .. } => {
                unreachable!("function-owner reconstruction seeds scoped expression identities")
            }
            dae::ExpressionOperation::ClockTransfer {
                kind,
                source,
                source_clock,
                target_clock,
            } => {
                let source = self.rebuild(source)?;
                self.target.at(provenance).clock_transfer(
                    kind,
                    source,
                    self.clocks[source_clock.index() as usize].clock_id(),
                    self.clocks[target_clock.index() as usize].clock_id(),
                )?
            }
        })
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

    pub(super) fn rebuild_subscripts(
        &mut self,
        subscripts: dae::SubscriptsView<'source>,
    ) -> Result<Vec<dae::Subscript<'target>>, dae::DaeConstructionError> {
        self.rebuild_subscripts_with(subscripts, Self::rebuild)
    }

    pub(super) fn rebuild_instantiated_subscripts(
        &mut self,
        subscripts: dae::SubscriptsView<'source>,
    ) -> Result<Vec<dae::Subscript<'target>>, dae::DaeConstructionError> {
        self.rebuild_subscripts_with(subscripts, Self::rebuild_instantiated)
    }

    fn rebuild_subscripts_with(
        &mut self,
        subscripts: dae::SubscriptsView<'source>,
        rebuild: fn(
            &mut Self,
            dae::ExprId<'source>,
        ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError>,
    ) -> Result<Vec<dae::Subscript<'target>>, dae::DaeConstructionError> {
        subscripts
            .iter()
            .map(|subscript| match subscript {
                dae::SubscriptView::Index {
                    expression,
                    provenance,
                } => Ok(dae::Subscript::Index {
                    expression: rebuild(self, expression)?,
                    provenance,
                }),
                dae::SubscriptView::Whole { provenance } => {
                    Ok(dae::Subscript::Whole { provenance })
                }
                dae::SubscriptView::Slice {
                    expression,
                    provenance,
                } => Ok(dae::Subscript::Slice {
                    expression: rebuild(self, expression)?,
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
        if let (Some(candidate), dae::CoordinateView::State(state)) = (self.candidate, coordinate)
            && self.substitute_demoted_value
            && state.index() == candidate.state
        {
            return self.rebuild_demoted_value(candidate);
        }
        if let (Some(candidate), dae::CoordinateView::Derivative(state)) =
            (self.candidate, coordinate)
            && state.index() == candidate.state
        {
            return self.rebuild_demoted_derivative(candidate);
        }
        if let Some(rebuilt) = self.rebuild_coordinate_alias(coordinate, provenance) {
            return rebuilt;
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
                    TargetVariable::State(id) => dae::CoordinateInput::State(id),
                    _ => unreachable!("algebraic becomes an algebraic or promoted state"),
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
                    TargetVariable::State(id) => dae::CoordinateInput::PreState(id),
                    _ => unreachable!("algebraic becomes an algebraic or promoted state"),
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

    /// Read a STRUCT-T09 derivative alias, a STRUCT-T10(a) folded parameter
    /// as its value, or an eliminated STRUCT-T02 alias member through its
    /// class representative, keeping the source occurrence's provenance.
    fn rebuild_coordinate_alias(
        &mut self,
        coordinate: dae::CoordinateView<'source>,
        provenance: dae::DaeProvenance,
    ) -> Option<Result<dae::ExprId<'target>, dae::DaeConstructionError>> {
        if let dae::CoordinateView::Derivative(state) = coordinate
            && let Some(alias) = self.variables[state.index() as usize].derivative_alias
        {
            return Some(
                self.target
                    .at(provenance)
                    .coordinate(dae::CoordinateInput::Algebraic(alias)),
            );
        }
        if let dae::CoordinateView::Parameter(parameter) = coordinate {
            let folded = self.variables[parameter.index() as usize]
                .folded
                .as_deref()?;
            return Some(super::evaluable_parameters::folded_literal(
                self.target,
                folded,
                provenance,
            ));
        }
        let dae::CoordinateView::Algebraic(id) = coordinate else {
            return None;
        };
        let alias = self.variables[id.index() as usize].value_alias?;
        let value = self
            .target
            .at(provenance)
            .coordinate(super::variables::value_alias_coordinate(alias));
        Some(value.and_then(|value| {
            if alias.negated {
                self.target
                    .at(provenance)
                    .unary(dae::UnaryOperator::Negate, value)
            } else {
                Ok(value)
            }
        }))
    }

    fn rebuild_demoted_value(
        &mut self,
        candidate: DirectStateConstraint,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        let generated = dae::DaeProvenance::generated(
            dae::DaeGeneration::IndexReduction,
            candidate.owner.span(),
        )?;
        let previous = std::mem::replace(&mut self.substitute_demoted_value, false);
        let value = match candidate.rhs {
            StateDefinition::DerivativeExpression(_) => {
                unreachable!("manifold preflight requires an exact value definition")
            }
            StateDefinition::Expression(rhs) => self.materialize_exact_value(
                self.source
                    .expression_id(rhs as usize)
                    .expect("candidate RHS resolves"),
                generated,
            ),
            StateDefinition::Auxiliary(variable) => self.auxiliary_value(variable, 0, generated),
        };
        self.substitute_demoted_value = previous;
        let value = value?;
        match candidate.rhs_sign {
            super::equalities::EqualitySign::Same => Ok(value),
            super::equalities::EqualitySign::Opposite => self
                .target
                .at(generated)
                .unary(dae::UnaryOperator::Negate, value),
        }
    }

    fn rebuild_demoted_derivative(
        &mut self,
        candidate: DirectStateConstraint,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        let generated = dae::DaeProvenance::generated(
            dae::DaeGeneration::IndexReduction,
            candidate.owner.span(),
        )?;
        let derivative = match candidate.rhs {
            StateDefinition::Expression(rhs) | StateDefinition::DerivativeExpression(rhs) => self
                .differentiate(
                self.source
                    .expression_id(rhs as usize)
                    .expect("candidate RHS resolves"),
                generated,
            )?,
            StateDefinition::Auxiliary(variable) => {
                Derivative::Expression(self.auxiliary_value(variable, 1, generated)?)
            }
        };
        match (candidate.rhs_sign, derivative) {
            (_, Derivative::Zero) => self.zero_for_demoted_state(candidate.state, generated),
            (super::equalities::EqualitySign::Same, Derivative::Expression(expression)) => {
                Ok(expression)
            }
            (super::equalities::EqualitySign::Opposite, Derivative::Expression(expression)) => self
                .target
                .at(generated)
                .unary(dae::UnaryOperator::Negate, expression),
        }
    }

    fn zero_for_demoted_state(
        &mut self,
        state: u32,
        provenance: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        let dimensions = self
            .source
            .variable_id(state as usize)
            .and_then(|id| self.source.variable(id))
            .expect("demoted state declaration resolves")
            .value_type()
            .dimensions()
            .to_vec();
        shaped_zero(self.target, &dimensions, provenance)
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
