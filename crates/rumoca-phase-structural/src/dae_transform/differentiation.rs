//! Exact symbolic differentiation over the rebuilt expression graph.
//!
//! Differentiation emits into the same construction as the rebuild, so a
//! derivative and the value it was taken from share subexpressions rather than
//! duplicating them. [`Derivative::Zero`] is a real algebraic zero, not a
//! literal, which keeps a structurally vanishing term out of the rebuilt graph
//! entirely instead of leaving `0 * x` for a later pass to notice.
//!
//! Every `unreachable!` below is discharged by a preflight in
//! [`constraints`](super::constraints): only an expression that
//! `is_differentiable` or `can_differentiate_order` already accepted ever
//! reaches these arms.

mod algebra;
mod conditionals;
mod geometry;

use rumoca_ir_dae as dae;

use super::HolonomicDifferentiationProof;
use super::builtin_profiles::{is_linear_tensor_map, is_materializable_builtin};
use super::component_projection::projected_element;
use super::equalities::{EqualityAnchor, EqualitySign, forwarded_call_argument, is_time_invariant};
use super::expressions::ExpressionRebuilder;
use super::variables::TargetVariable;

impl<'source, 'borrow, 'storage, 'target> ExpressionRebuilder<'source, 'borrow, 'storage, 'target> {
    /// Differentiate only under the certificate collected for this exact
    /// residual from the finalized source DAE.
    pub(super) fn differentiate_holonomic(
        &mut self,
        source_id: dae::ExprId<'source>,
        order: u8,
        proof: &HolonomicDifferentiationProof,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        assert_eq!(source_id.index(), proof.residual);
        assert!(order <= proof.maximum_order);
        assert!(!proof.anchored_states.is_empty());
        let previous = self.state_only_derivative;
        // Only a lower-order derivative retained as a manifold row must be
        // materialized through exact state anchors. A first derivative that
        // is itself the replacement equation must retain its algebraic
        // unknowns.
        self.state_only_derivative = order < proof.maximum_order;
        let differentiated = match &proof.component {
            Some(component) => self.differentiate_component(component, order, provenance),
            None => self.differentiate_order(source_id, order, provenance),
        };
        self.state_only_derivative = previous;
        differentiated
    }

    /// Re-express a proved holonomic position residual entirely through exact
    /// state or invariant anchors before it enters the retained manifold.
    pub(super) fn materialize_holonomic_value(
        &mut self,
        source_id: dae::ExprId<'source>,
        proof: &HolonomicDifferentiationProof,
        provenance: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        assert_eq!(source_id.index(), proof.residual);
        match &proof.component {
            Some(component) => self.materialize_component_value(component, provenance),
            None => self.materialize_exact_value(source_id, provenance),
        }
    }

    pub(super) fn differentiate(
        &mut self,
        source_id: dae::ExprId<'source>,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        self.differentiate_order(source_id, 1, provenance)
    }

    pub(super) fn differentiate_lifted_algebraic(
        &mut self,
        source_algebraic: u32,
        proof: &HolonomicDifferentiationProof,
        provenance: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        let definition = self
            .source
            .expression_id(proof.residual as usize)
            .expect("lift proof definition resolves");
        assert_eq!(proof.maximum_order, 1);
        let TargetVariable::State(target_state) =
            self.variables[source_algebraic as usize].identity
        else {
            unreachable!("lift proof target was reserved as a state")
        };
        let derivative = self
            .target
            .at(provenance)
            .coordinate(dae::CoordinateInput::Derivative(target_state))?;
        assert_eq!(definition.index(), proof.residual);
        assert!(!proof.anchored_states.is_empty());
        let rhs = self.differentiate_order(definition, 1, provenance)?;
        match rhs {
            Derivative::Zero => Ok(derivative),
            Derivative::Expression(rhs) => {
                self.target
                    .at(provenance)
                    .binary(dae::BinaryOperator::Subtract, derivative, rhs)
            }
        }
    }

    pub(super) fn materialize_lifted_algebraic_value(
        &mut self,
        source_algebraic: u32,
        proof: &HolonomicDifferentiationProof,
        provenance: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        let definition = self
            .source
            .expression_id(proof.residual as usize)
            .expect("lift proof definition resolves");
        let TargetVariable::State(target_state) =
            self.variables[source_algebraic as usize].identity
        else {
            unreachable!("lift proof target was reserved as a state")
        };
        let state = self
            .target
            .at(provenance)
            .coordinate(dae::CoordinateInput::State(target_state))?;
        let value = self.materialize_exact_value(definition, provenance)?;
        self.target
            .at(provenance)
            .binary(dae::BinaryOperator::Subtract, state, value)
    }

    pub(super) fn differentiate_order(
        &mut self,
        source_id: dae::ExprId<'source>,
        order: u8,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        let scoped = self
            .function_context
            .scoped_to_expression(self.source, source_id);
        let previous = std::mem::replace(&mut self.function_context, scoped);
        let key = self.scoped_reconstruction_key(source_id, order, provenance);
        let differentiated = match self.scoped_cache.derivatives.get(&key).copied() {
            Some(value) => Ok(value),
            None => {
                let result = self.differentiate_order_scoped(source_id, order, provenance);
                if let Ok(value) = result {
                    self.scoped_cache.derivatives.insert(key, value);
                }
                result
            }
        };
        self.function_context = previous;
        differentiated
    }

    fn differentiate_order_scoped(
        &mut self,
        source_id: dae::ExprId<'source>,
        order: u8,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        if let Some(branch) = self
            .function_context
            .selected_branch(self.source, source_id)
        {
            return self.differentiate_order(branch, order, provenance);
        }
        if let Some(element) = projected_element(self.source, self.facts, source_id) {
            return self.differentiate_order(element, order, provenance);
        }
        if let Some(selected) = super::function_derivatives::select_derivative(
            self.source,
            &self.function_context,
            source_id,
            order,
        ) {
            return self.differentiate_supplied_function(selected, provenance);
        }
        if let Some((result, nested)) = self.function_context.call_result(self.source, source_id) {
            let previous = std::mem::replace(&mut self.function_context, nested);
            let differentiated = self.differentiate_order(result, order, provenance);
            self.function_context = previous;
            return differentiated;
        }
        if self
            .facts
            .expression_is_zero(self.source, source_id, &self.function_context)
        {
            return Ok(Derivative::Zero);
        }
        let source = self
            .source
            .expression(source_id)
            .expect("differentiable expression identity resolves");
        if self.function_context.is_empty() && is_time_invariant(self.source, source_id) {
            return Ok(Derivative::Zero);
        }
        match source.operation() {
            dae::ExpressionOperation::Literal(_) => Ok(Derivative::Zero),
            dae::ExpressionOperation::Coordinate(coordinate) => {
                self.differentiate_coordinate(coordinate, order, provenance)
            }
            dae::ExpressionOperation::Unary { operator, operand } => {
                let derivative = self.differentiate_order(operand, order, provenance)?;
                match (operator, derivative) {
                    (_, Derivative::Zero) => Ok(Derivative::Zero),
                    (dae::UnaryOperator::Plus, derivative) => Ok(derivative),
                    (dae::UnaryOperator::Negate, Derivative::Expression(operand)) => self
                        .target
                        .at(provenance)
                        .unary(dae::UnaryOperator::Negate, operand)
                        .map(Derivative::Expression),
                    (dae::UnaryOperator::Not, _) => {
                        unreachable!("differentiability preflight rejects Boolean negation")
                    }
                }
            }
            dae::ExpressionOperation::Binary { operator, lhs, rhs } => {
                self.differentiate_binary(operator, lhs, rhs, order, provenance)
            }
            dae::ExpressionOperation::Array(elements) => {
                self.differentiate_array(elements, order, provenance)
            }
            dae::ExpressionOperation::Conditional(operands) => self
                .materialize_parameter_conditional(operands, Some(order), provenance)
                .map(Derivative::Expression),
            dae::ExpressionOperation::Builtin { builtin, arguments }
                if is_linear_tensor_map(builtin) =>
            {
                self.differentiate_linear_tensor_map(builtin, arguments, order, provenance)
            }
            dae::ExpressionOperation::Builtin { builtin, arguments } => {
                self.differentiate_builtin(builtin, arguments, order, provenance)
            }
            dae::ExpressionOperation::Index { base, subscripts } => {
                self.differentiate_index(base, subscripts, order, provenance)
            }
            dae::ExpressionOperation::Field { base, field } => {
                self.differentiate_projected_field(base, field, order, provenance)
            }
            _ => unreachable!("differentiability preflight rejects this operation"),
        }
    }

    fn differentiate_coordinate(
        &mut self,
        coordinate: dae::CoordinateView<'source>,
        order: u8,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        match coordinate {
            dae::CoordinateView::Parameter(_) => Ok(Derivative::Zero),
            dae::CoordinateView::Time if order == 1 => self
                .target
                .at(provenance)
                .literal(dae::DaeLiteral::Real(1.0))
                .map(Derivative::Expression),
            dae::CoordinateView::Time => Ok(Derivative::Zero),
            dae::CoordinateView::State(state) => self.differentiate_state(state, order, provenance),
            dae::CoordinateView::FunctionParameter(parameter) => {
                let argument = self
                    .function_context
                    .parameter_argument(parameter)
                    .expect("differentiability preflight resolved this function parameter");
                self.differentiate_order(argument, order, provenance)
            }
            dae::CoordinateView::Algebraic(algebraic) => {
                self.differentiate_algebraic(algebraic, order, provenance)
            }
            dae::CoordinateView::Derivative(state) => {
                let definition = self.facts.derivative_definitions[state.index() as usize]
                    .expect("differentiability preflight proved this derivative defined");
                let definition = self
                    .source
                    .expression_id(definition as usize)
                    .expect("explicit derivative definition resolves");
                self.differentiate_order(definition, order, provenance)
            }
            _ => unreachable!("differentiability preflight rejects this coordinate"),
        }
    }

    fn differentiate_supplied_function(
        &mut self,
        selected: super::function_derivatives::SelectedFunctionDerivative<'source>,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        let mut arguments = Vec::new();
        for argument in selected.arguments.iter() {
            let value = if self.state_only_derivative {
                self.materialize_exact_value(argument, provenance)?
            } else {
                self.rebuild_instantiated(argument)?
            };
            arguments.push(value);
        }
        for ordinal in selected.link.tangent_inputs() {
            let argument = selected
                .arguments
                .get(ordinal)
                .expect("checked derivative input");
            let tangent = self.differentiate_order(argument, 1, provenance)?;
            arguments.push(self.materialize_derivative(tangent, argument, provenance)?);
        }
        let target = self.rebuilt_function(selected.link.target());
        self.target
            .at(provenance)
            .call(target, selected.output, arguments)
            .map(Derivative::Expression)
    }

    fn differentiate_index(
        &mut self,
        base: dae::ExprId<'source>,
        subscripts: dae::SubscriptsView<'source>,
        order: u8,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        let Derivative::Expression(derivative) =
            self.differentiate_order(base, order, provenance)?
        else {
            return Ok(Derivative::Zero);
        };
        let subscripts = self.rebuild_subscripts(subscripts)?;
        self.target
            .at(provenance)
            .index(derivative, subscripts)
            .map(Derivative::Expression)
    }

    fn differentiate_projected_field(
        &mut self,
        base: dae::ExprId<'source>,
        field: u32,
        order: u8,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        let (projected, projected_context) = self
            .function_context
            .projected_field(self.source, base, field)
            .expect("differentiability preflight resolved this record field");
        let previous = std::mem::replace(&mut self.function_context, projected_context);
        let differentiated = self.differentiate_order(projected, order, provenance);
        self.function_context = previous;
        differentiated
    }

    /// Differentiate an algebraic through either its equality-class anchor or
    /// the unique acyclic causal definition proved for the finalized DAE.
    fn differentiate_algebraic(
        &mut self,
        algebraic: dae::AlgebraicId<'source>,
        order: u8,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        if self.facts.auxiliary_blocks[algebraic.index() as usize].is_some() {
            return self
                .auxiliary_value(algebraic.index(), order, provenance)
                .map(Derivative::Expression);
        }
        let Some((anchor, sign)) = self.facts.equalities.anchor_of(algebraic.index()) else {
            let definition = self
                .facts
                .algebraic_definition(self.source, algebraic)
                .expect("differentiability preflight proves a causal algebraic definition");
            return self.differentiate_order(definition, order, provenance);
        };
        let EqualityAnchor::State(_) = anchor else {
            // A class pinned to a time-invariant value has derivative zero.
            return Ok(Derivative::Zero);
        };
        let anchor = self
            .facts
            .equalities
            .anchor_expression(anchor)
            .and_then(|anchor| self.source.expression_id(anchor as usize))
            .expect("a state equality anchor has a checked scalar expression");
        let derivative = match self.differentiate_order(anchor, order, provenance)? {
            Derivative::Zero => Derivative::Zero,
            Derivative::Expression(value) => {
                Derivative::Expression(self.shape_equality_anchor(algebraic, value, provenance)?)
            }
        };
        match (sign, derivative) {
            (EqualitySign::Same, derivative)
            | (EqualitySign::Opposite, derivative @ Derivative::Zero) => Ok(derivative),
            (EqualitySign::Opposite, Derivative::Expression(anchor)) => self
                .target
                .at(provenance)
                .unary(dae::UnaryOperator::Negate, anchor)
                .map(Derivative::Expression),
        }
    }

    fn differentiate_state(
        &mut self,
        source_state: dae::StateId<'source>,
        order: u8,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        if let Some(definition) = self.facts.derivative_definitions[source_state.index() as usize] {
            let definition = self
                .source
                .expression_id(definition as usize)
                .expect("explicit derivative definition resolves");
            if order > 1 {
                return self.differentiate_order(definition, order - 1, provenance);
            }
            let definition = if self.state_only_derivative {
                self.materialize_exact_value(definition, provenance)?
            } else {
                self.rebuild(definition)?
            };
            return Ok(Derivative::Expression(definition));
        }
        let TargetVariable::State(state) = self.variables[source_state.index() as usize].identity
        else {
            unreachable!("candidate RHS cannot refer to the demoted state")
        };
        self.target
            .at(provenance)
            .coordinate(dae::CoordinateInput::Derivative(state))
            .map(Derivative::Expression)
    }

    /// Rebuild one scalar expression while replacing algebraic coordinates by
    /// the exact value anchor proved for their equality class.
    pub(super) fn materialize_exact_value(
        &mut self,
        source_id: dae::ExprId<'source>,
        provenance: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        let scoped = self
            .function_context
            .scoped_to_expression(self.source, source_id);
        let previous = std::mem::replace(&mut self.function_context, scoped);
        let key = self.scoped_reconstruction_key(source_id, 0, provenance);
        let materialized = match self.scoped_cache.materialized.get(&key).copied() {
            Some(value) => Ok(value),
            None => {
                let result = self
                    .materialize_exact_value_scoped(source_id, provenance)
                    .and_then(|value| self.preserve_real_value_type(source_id, value, provenance));
                if let Ok(value) = result {
                    self.scoped_cache.materialized.insert(key, value);
                }
                result
            }
        };
        self.function_context = previous;
        materialized
    }

    fn materialize_exact_value_scoped(
        &mut self,
        source_id: dae::ExprId<'source>,
        provenance: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        if let Some(branch) = self
            .function_context
            .selected_branch(self.source, source_id)
        {
            return self.materialize_exact_value(branch, provenance);
        }
        if let Some(element) = projected_element(self.source, self.facts, source_id) {
            return self.materialize_exact_value(element, provenance);
        }
        if let Some((result, nested)) = self.function_context.call_result(self.source, source_id) {
            let previous = std::mem::replace(&mut self.function_context, nested);
            let materialized = self.materialize_exact_value(result, provenance);
            self.function_context = previous;
            return materialized;
        }
        if let Some(argument) = forwarded_call_argument(self.source, source_id) {
            return self.materialize_exact_value(argument, provenance);
        }
        let source = self
            .source
            .expression(source_id)
            .expect("materializable expression resolves");
        match source.operation() {
            dae::ExpressionOperation::Literal(_)
            | dae::ExpressionOperation::Coordinate(
                dae::CoordinateView::Parameter(_)
                | dae::CoordinateView::Time
                | dae::CoordinateView::State(_),
            ) => self.rebuild(source_id),
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(
                parameter,
            )) => {
                let argument = self
                    .function_context
                    .parameter_argument(parameter)
                    .expect("holonomic value preflight resolves this function parameter");
                self.materialize_exact_value(argument, provenance)
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(algebraic)) => {
                self.materialize_algebraic_value(algebraic, provenance)
            }
            dae::ExpressionOperation::Unary {
                operator: operator @ (dae::UnaryOperator::Plus | dae::UnaryOperator::Negate),
                operand,
            } => {
                let operand = self.materialize_exact_value(operand, provenance)?;
                self.target.at(provenance).unary(operator, operand)
            }
            dae::ExpressionOperation::Binary { operator, lhs, rhs }
                if super::builtin_profiles::is_differentiable_binary(operator) =>
            {
                let lhs = self.materialize_exact_value(lhs, provenance)?;
                let rhs = self.materialize_exact_value(rhs, provenance)?;
                self.target.at(provenance).binary(operator, lhs, rhs)
            }
            dae::ExpressionOperation::Array(elements) => {
                let elements = elements
                    .iter()
                    .map(|element| self.materialize_exact_value(element, provenance))
                    .collect::<Result<Vec<_>, _>>()?;
                self.target.at(provenance).array(elements)
            }
            dae::ExpressionOperation::Conditional(operands) => {
                self.materialize_parameter_conditional(operands, None, provenance)
            }
            dae::ExpressionOperation::Field { base, field } => {
                self.materialize_projected_field(source_id, base, field, provenance)
            }
            dae::ExpressionOperation::Index { base, subscripts } => {
                let base = self.materialize_exact_value(base, provenance)?;
                let subscripts = self.rebuild_subscripts(subscripts)?;
                self.target.at(provenance).index(base, subscripts)
            }
            dae::ExpressionOperation::Builtin { builtin, arguments }
                if is_materializable_builtin(builtin) =>
            {
                self.materialize_builtin_value(builtin, arguments, provenance)
            }
            _ => Err(dae::DaeConstructionError::IncompleteDefinition {
                kind: "state-only manifold substitution",
                index: source_id.index(),
                span: provenance.span(),
            }),
        }
    }

    fn materialize_projected_field(
        &mut self,
        source_id: dae::ExprId<'source>,
        base: dae::ExprId<'source>,
        field: u32,
        provenance: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        let (projected, context) = self
            .function_context
            .projected_field(self.source, base, field)
            .ok_or(dae::DaeConstructionError::IncompleteDefinition {
                kind: "state-only manifold substitution",
                index: source_id.index(),
                span: provenance.span(),
            })?;
        let previous = std::mem::replace(&mut self.function_context, context);
        let materialized = self.materialize_exact_value(projected, provenance);
        self.function_context = previous;
        materialized
    }

    fn materialize_algebraic_value(
        &mut self,
        algebraic: dae::AlgebraicId<'source>,
        provenance: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        if self.facts.auxiliary_blocks[algebraic.index() as usize].is_some() {
            return self.auxiliary_value(algebraic.index(), 0, provenance);
        }
        let Some((anchor, sign)) = self.facts.equalities.value_anchor_of(algebraic.index()) else {
            let definition = self
                .facts
                .algebraic_definition(self.source, algebraic)
                .expect("holonomic preflight proves a causal algebraic definition");
            return self.materialize_exact_value(definition, provenance);
        };
        let anchor = self
            .facts
            .equalities
            .anchor_expression(anchor)
            .and_then(|anchor| self.source.expression_id(anchor as usize))
            .expect("holonomic value preflight proves a materializable anchor");
        let anchor = self.materialize_exact_value(anchor, provenance)?;
        let anchor = self.shape_equality_anchor(algebraic, anchor, provenance)?;
        match sign {
            EqualitySign::Same => Ok(anchor),
            EqualitySign::Opposite => self
                .target
                .at(provenance)
                .unary(dae::UnaryOperator::Negate, anchor),
        }
    }

    fn materialize_builtin_value(
        &mut self,
        builtin: dae::PureBuiltin,
        arguments: dae::ExpressionOperands<'source>,
        provenance: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        let arguments = arguments
            .iter()
            .map(|argument| self.materialize_exact_value(argument, provenance))
            .collect::<Result<Vec<_>, _>>()?;
        self.target.at(provenance).builtin(builtin, arguments)
    }

    fn differentiate_array(
        &mut self,
        elements: dae::ExpressionOperands<'source>,
        order: u8,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        let mut derivatives = Vec::with_capacity(elements.len());
        let mut any_nonzero = false;
        for element in elements.iter() {
            let derivative = self.differentiate_order(element, order, provenance)?;
            any_nonzero |= matches!(derivative, Derivative::Expression(_));
            derivatives.push((element, derivative));
        }
        if !any_nonzero {
            return Ok(Derivative::Zero);
        }
        let elements = derivatives
            .into_iter()
            .map(|(source, derivative)| self.materialize_derivative(derivative, source, provenance))
            .collect::<Result<Vec<_>, _>>()?;
        self.target
            .at(provenance)
            .array(elements)
            .map(Derivative::Expression)
    }

    fn differentiate_linear_tensor_map(
        &mut self,
        builtin: dae::PureBuiltin,
        arguments: dae::ExpressionOperands<'source>,
        order: u8,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        let mut derivatives = Vec::with_capacity(arguments.len());
        let mut any_nonzero = false;
        for argument in arguments.iter() {
            let derivative = self.differentiate_order(argument, order, provenance)?;
            any_nonzero |= matches!(derivative, Derivative::Expression(_));
            derivatives.push((argument, derivative));
        }
        if !any_nonzero {
            return Ok(Derivative::Zero);
        }
        let arguments = derivatives
            .into_iter()
            .map(|(source, derivative)| self.materialize_derivative(derivative, source, provenance))
            .collect::<Result<Vec<_>, _>>()?;
        self.target
            .at(provenance)
            .builtin(builtin, arguments)
            .map(Derivative::Expression)
    }

    fn differentiate_binary(
        &mut self,
        operator: dae::BinaryOperator,
        lhs: dae::ExprId<'source>,
        rhs: dae::ExprId<'source>,
        order: u8,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        match operator {
            dae::BinaryOperator::Add
            | dae::BinaryOperator::Subtract
            | dae::BinaryOperator::ElementwiseAdd
            | dae::BinaryOperator::ElementwiseSubtract => {
                self.differentiate_sum(operator, lhs, rhs, order, provenance)
            }
            dae::BinaryOperator::Multiply | dae::BinaryOperator::ElementwiseMultiply => {
                self.differentiate_product(operator, lhs, rhs, order, provenance)
            }
            dae::BinaryOperator::Divide | dae::BinaryOperator::ElementwiseDivide => {
                self.differentiate_quotient(operator, lhs, rhs, order, provenance)
            }
            _ => unreachable!("differentiability preflight rejects this binary operator"),
        }
    }

    fn differentiate_builtin(
        &mut self,
        builtin: dae::PureBuiltin,
        arguments: dae::ExpressionOperands<'source>,
        order: u8,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        use dae::PureBuiltin as Builtin;

        match builtin {
            // Their operands are checked structural extents, not values in the
            // continuous system, so these constructors are time invariant.
            Builtin::Zeros | Builtin::Ones | Builtin::Identity => Ok(Derivative::Zero),
            Builtin::Cross | Builtin::OuterProduct => {
                self.differentiate_bilinear_builtin(builtin, arguments, order, provenance)
            }
            Builtin::Sin | Builtin::Cos | Builtin::Sqrt => {
                self.differentiate_unary_geometry(builtin, arguments, order, provenance)
            }
            Builtin::Atan2 => self.differentiate_atan2_builtin(arguments, provenance),
            _ => unreachable!("differentiability preflight rejects this builtin"),
        }
    }

    fn differentiate_atan2_builtin(
        &mut self,
        arguments: dae::ExpressionOperands<'source>,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        let mut arguments = arguments.iter();
        let y = arguments.next().expect("checked atan2 y argument");
        let x = arguments.next().expect("checked atan2 x argument");
        let y_derivative = self.differentiate_order(y, 1, provenance)?;
        let x_derivative = self.differentiate_order(x, 1, provenance)?;
        if matches!(y_derivative, Derivative::Zero) && matches!(x_derivative, Derivative::Zero) {
            return Ok(Derivative::Zero);
        }
        let y_value = self.differentiation_value(y, provenance)?;
        let x_value = self.differentiation_value(x, provenance)?;
        let x_dy = self.multiply(y_derivative, Derivative::Expression(x_value), provenance)?;
        let y_dx = self.multiply(x_derivative, Derivative::Expression(y_value), provenance)?;
        let numerator = self.combine_sum(dae::BinaryOperator::Subtract, x_dy, y_dx, provenance)?;
        let Derivative::Expression(numerator) = numerator else {
            return Ok(Derivative::Zero);
        };
        let x_squared =
            self.target
                .at(provenance)
                .binary(dae::BinaryOperator::Multiply, x_value, x_value)?;
        let y_squared =
            self.target
                .at(provenance)
                .binary(dae::BinaryOperator::Multiply, y_value, y_value)?;
        let denominator =
            self.target
                .at(provenance)
                .binary(dae::BinaryOperator::Add, x_squared, y_squared)?;
        self.target
            .at(provenance)
            .binary(dae::BinaryOperator::Divide, numerator, denominator)
            .map(Derivative::Expression)
    }

    pub(super) fn materialize_derivative(
        &mut self,
        derivative: Derivative<'target>,
        source_value: dae::ExprId<'source>,
        provenance: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        match derivative {
            Derivative::Zero => self.materialize_shaped_zero(source_value, provenance),
            Derivative::Expression(expression) => self
                .target
                .at(provenance)
                .unary(dae::UnaryOperator::Plus, expression),
        }
    }

    /// Materialize an exact algebraic zero with the range-preserving shape of
    /// the value whose derivative vanished.
    fn materialize_shaped_zero(
        &mut self,
        source_value: dae::ExprId<'source>,
        provenance: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        let source = self
            .source
            .expression(source_value)
            .expect("differentiated source expression resolves");
        if source.value_type().is_scalar() {
            return self
                .target
                .at(provenance)
                .literal(dae::DaeLiteral::Real(0.0));
        }
        let extents = source
            .value_type()
            .dimensions()
            .iter()
            .map(|extent| {
                self.target
                    .at(provenance)
                    .literal(dae::DaeLiteral::Integer(i64::from(*extent)))
            })
            .collect::<Result<Vec<_>, _>>()?;
        self.target
            .at(provenance)
            .builtin(dae::PureBuiltin::Zeros, extents)
    }

    fn multiply(
        &mut self,
        lhs: Derivative<'target>,
        rhs: Derivative<'target>,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        let (Derivative::Expression(lhs), Derivative::Expression(rhs)) = (lhs, rhs) else {
            return Ok(Derivative::Zero);
        };
        self.target
            .at(provenance)
            .binary(dae::BinaryOperator::Multiply, lhs, rhs)
            .map(Derivative::Expression)
    }

    pub(super) fn combine_sum(
        &mut self,
        operator: dae::BinaryOperator,
        lhs: Derivative<'target>,
        rhs: Derivative<'target>,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        match (lhs, rhs) {
            (Derivative::Zero, Derivative::Zero) => Ok(Derivative::Zero),
            (Derivative::Expression(expression), Derivative::Zero) => {
                Ok(Derivative::Expression(expression))
            }
            (Derivative::Zero, Derivative::Expression(expression))
                if matches!(
                    operator,
                    dae::BinaryOperator::Add | dae::BinaryOperator::ElementwiseAdd
                ) =>
            {
                Ok(Derivative::Expression(expression))
            }
            (Derivative::Zero, Derivative::Expression(expression)) => self
                .target
                .at(provenance)
                .unary(dae::UnaryOperator::Negate, expression)
                .map(Derivative::Expression),
            (Derivative::Expression(lhs), Derivative::Expression(rhs)) => {
                self.combine_nonzero_sum(operator, lhs, rhs, provenance)
            }
        }
    }

    fn combine_nonzero_sum(
        &mut self,
        operator: dae::BinaryOperator,
        lhs: dae::ExprId<'target>,
        rhs: dae::ExprId<'target>,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        if matches!(
            operator,
            dae::BinaryOperator::Subtract | dae::BinaryOperator::ElementwiseSubtract
        ) && lhs == rhs
        {
            return Ok(Derivative::Zero);
        }
        self.target
            .at(provenance)
            .binary(operator, lhs, rhs)
            .map(Derivative::Expression)
    }
}

#[derive(Clone, Copy)]
pub(super) enum Derivative<'dae> {
    Zero,
    Expression(dae::ExprId<'dae>),
}
