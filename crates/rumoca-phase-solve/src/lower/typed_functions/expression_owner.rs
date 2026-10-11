//! Integer quotients and comparisons use the canonical typed expression owner.

use super::*;
use crate::lower::call_scoped_actions::CallAssertionProjection;
use rumoca_core::Span;

pub(super) struct RegisteredExactExpression<'dae> {
    call: RegisteredCall<'dae>,
    assertion_projections: Box<[CallAssertionProjection]>,
}

pub(crate) struct ExactExpressionForm<'dae> {
    expression: dae::ExprId<'dae>,
    inputs: Vec<(
        ExpressionInput<'dae>,
        dae::ValueTypeId<'dae>,
        dae::ExprId<'dae>,
    )>,
}

#[derive(Clone, Copy)]
enum ExpressionInput<'dae> {
    Coordinate(ModelCoordinateKey<'dae>),
    Binder(dae::DomainId<'dae>, u32),
}

impl<'dae> ExactExpressionForm<'dae> {
    pub(crate) fn try_admit(
        view: dae::DaeView<'dae>,
        expression: dae::ExprId<'dae>,
    ) -> Result<Option<Self>, solve::SolveProgramConstructionError> {
        let node = view
            .expression(expression)
            .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
        if !needs_integer_owner(view, node) {
            return Ok(None);
        }
        let provenance = node.provenance().span();
        let mut supported = true;
        let mut readers = HashMap::new();
        let mut binders = HashMap::new();
        let mut owned_domains = BTreeSet::new();
        dae::for_each_expression(view, expression, |expression, node| {
            match node.operation() {
                dae::ExpressionOperation::Coordinate(dae::CoordinateView::Binder(binder)) => {
                    binders
                        .entry((binder.domain(), binder.ordinal()))
                        .or_insert((node.value_type_id(), expression));
                }
                dae::ExpressionOperation::Coordinate(coordinate) => {
                    supported &=
                        collect_coordinate_reader(coordinate, node, expression, &mut readers);
                }
                dae::ExpressionOperation::FunctionValue { .. }
                | dae::ExpressionOperation::FunctionFoldParameter { .. }
                | dae::ExpressionOperation::FunctionFoldOutput { .. }
                | dae::ExpressionOperation::ClockTransfer { .. }
                | dae::ExpressionOperation::StringConversion { .. } => supported = false,
                dae::ExpressionOperation::Comprehension { domain, .. } => {
                    owned_domains.insert(domain.index());
                }
                _ => {}
            }
        });
        if !supported {
            return Err(solve::SolveProgramConstructionError::InvalidCallInterface { provenance });
        }
        let mut inputs: Vec<_> =
            model_coordinates::collect_model_coordinate_types(view, [expression], [], provenance)?
                .into_iter()
                .map(|(key, ty)| (ExpressionInput::Coordinate(key), ty, readers[&key]))
                .collect();
        let mut binders: Vec<_> = binders.into_iter().collect();
        binders.retain(|((domain, _), _)| !owned_domains.contains(&domain.index()));
        binders.sort_by_key(|((domain, ordinal), _)| (domain.index(), *ordinal));
        inputs.extend(
            binders
                .into_iter()
                .map(|((domain, ordinal), (ty, expression))| {
                    (ExpressionInput::Binder(domain, ordinal), ty, expression)
                }),
        );
        Ok(Some(Self { expression, inputs }))
    }

    pub(crate) fn input_readers(&self) -> impl Iterator<Item = dae::ExprId<'dae>> + '_ {
        self.inputs.iter().map(|(_, _, reader)| *reader)
    }

    fn load_inputs<'builder, 'program>(
        &self,
        view: dae::DaeView<'dae>,
        builder: &'builder mut solve::TypedProgramBuilder<'program>,
        inputs: &[solve::ProgramSlot<'program>],
        provenance: Span,
    ) -> Result<ExpressionLowerer<'builder, 'program, 'dae>, solve::SolveProgramConstructionError>
    {
        let mut lowerer = ExpressionLowerer::for_model_coordinates(view, builder, HashMap::new());
        for ((key, ty, _), input) in self.inputs.iter().zip(inputs) {
            let register = lowerer.builder.load(*input, provenance)?;
            match key {
                ExpressionInput::Coordinate(key) => {
                    lowerer
                        .model_coordinates
                        .insert(*key, LoweredValue::scalar(*ty, register));
                }
                ExpressionInput::Binder(domain, ordinal) => {
                    lowerer.binders.insert((domain.index(), *ordinal), register);
                }
            }
        }
        Ok(lowerer)
    }
}

fn collect_coordinate_reader<'dae>(
    coordinate: dae::CoordinateView<'dae>,
    node: dae::ExpressionView<'dae>,
    expression: dae::ExprId<'dae>,
    readers: &mut HashMap<ModelCoordinateKey<'dae>, dae::ExprId<'dae>>,
) -> bool {
    let Some(key) =
        ModelCoordinateKey::from_view(coordinate).filter(|_| !node.value_type().is_record())
    else {
        return false;
    };
    readers.entry(key).or_insert(expression);
    true
}

fn needs_integer_owner<'dae>(view: dae::DaeView<'dae>, node: dae::ExpressionView<'dae>) -> bool {
    match node.operation() {
        dae::ExpressionOperation::Builtin {
            builtin: dae::PureBuiltin::Div | dae::PureBuiltin::Mod | dae::PureBuiltin::Rem,
            ..
        } => node.value_type().scalar_type() == dae::ScalarType::Integer,
        dae::ExpressionOperation::Binary {
            operator:
                dae::BinaryOperator::Equal
                | dae::BinaryOperator::NotEqual
                | dae::BinaryOperator::Less
                | dae::BinaryOperator::LessEqual
                | dae::BinaryOperator::Greater
                | dae::BinaryOperator::GreaterEqual,
            lhs,
            rhs,
        } => [lhs, rhs].into_iter().all(|operand| {
            // MLS §3.5, §6.7: compare Integer operands in their declared
            // type; only a mixed Real operand requires language conversion.
            view.expression(operand)
                .expect("checked comparison operand resolves")
                .value_type()
                .scalar_type()
                == dae::ScalarType::Integer
        }),
        _ => false,
    }
}

impl<'dae> PureCallRegistry<'dae> {
    pub(crate) fn register_exact_expression(
        &mut self,
        view: dae::DaeView<'dae>,
        form: &ExactExpressionForm<'dae>,
    ) -> Result<RegisteredCall<'dae>, solve::SolveProgramConstructionError> {
        if let Some(registered) = self.identities.expressions.get(&form.expression) {
            return Ok(registered.call.clone());
        }
        let node = view
            .expression(form.expression)
            .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
        let provenance = node.provenance().span();
        let inputs = form
            .inputs
            .iter()
            .map(|(_, ty, _)| lower_primitive_type(view, *ty, arithmetic_profile()))
            .collect::<Result<Vec<_>, _>>()?;
        let output = lower_primitive_type(view, node.value_type_id(), arithmetic_profile())?;
        let (callees, predicate_ranges, assertions, assertion_slots) =
            self.register_expression_calls(view, [(form.expression, ())])?;
        let assertion_projections = assertions.iter().map(|entry| entry.projection).collect();
        let assertions = assertions
            .into_iter()
            .map(|entry| {
                let target_start = entry.slot_start.checked_add(1).ok_or(
                    solve::SolveProgramConstructionError::InvalidCallOutput { provenance },
                )?;
                entry
                    .assertion
                    .project_outputs(entry.source_result_count, target_start)
            })
            .collect::<Result<Box<[_]>, _>>()?;
        let mut outputs = vec![solve::SolvePureCallOutput::result(output)];
        outputs.extend(assertion_slots.iter().map(AssertionSlot::output));
        let recursive = callees.values().any(|callee| callee.recursive);
        let identity = self.identities.issue(provenance)?;
        let owner = self.table.add_owner(
            identity,
            inputs,
            outputs,
            provenance,
            |builder, inputs, outputs| {
                let mut lowerer = form.load_inputs(view, builder, inputs, provenance)?;
                lowerer.callees = callees;
                lowerer.predicate_ranges = predicate_ranges;
                lowerer.predicate_values = vec![None; assertion_slots.len()];
                lowerer.assertion_slots = assertion_slots.clone();
                let value = lowerer
                    .expression(form.expression)?
                    .only_register(provenance)?;
                lowerer.builder.store(outputs[0], value, provenance)?;
                for (&output, value) in outputs[1..].iter().zip(lowerer.predicate_values) {
                    let value =
                        value.ok_or(solve::SolveProgramConstructionError::InvalidCallOutput {
                            provenance,
                        })?;
                    lowerer.builder.store(output, value, provenance)?;
                }
                Ok(())
            },
        )?;
        let site = self
            .table
            .call_site(owner)
            .ok_or(solve::SolveProgramConstructionError::UnknownCallOwner { provenance })?;
        let registered = RegisteredCall {
            site,
            callee: CalleeInterface {
                owner,
                result_ranges: std::iter::once(0..1).collect(),
                result_leaf_count: 1,
                assertion_slots,
                assertions,
                recursive,
            },
        };
        self.identities.expressions.insert(
            form.expression,
            RegisteredExactExpression {
                call: registered.clone(),
                assertion_projections,
            },
        );
        Ok(registered)
    }

    pub(in crate::lower) fn exact_assertion_projection(
        &self,
        expression: dae::ExprId<'dae>,
        assertion: usize,
    ) -> Option<CallAssertionProjection> {
        self.identities
            .expressions
            .get(&expression)?
            .assertion_projections
            .get(assertion)
            .copied()
    }
}
