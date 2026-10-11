//! Integer quotients and comparisons use the canonical typed expression owner.

use super::*;

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
                dae::ExpressionOperation::Coordinate(coordinate) => {
                    if let dae::CoordinateView::Binder(binder) = coordinate {
                        binders
                            .entry((binder.domain(), binder.ordinal()))
                            .or_insert((node.value_type_id(), expression));
                        return;
                    }
                    match ModelCoordinateKey::from_view(coordinate) {
                        Some(key) if !node.value_type().is_record() => {
                            readers.entry(key).or_insert(expression);
                        }
                        _ => supported = false,
                    }
                }
                dae::ExpressionOperation::Call { .. }
                | dae::ExpressionOperation::FunctionValue { .. }
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
    ) -> Result<solve::SolvePureCallSite, solve::SolveProgramConstructionError> {
        if let Some(site) = self.identities.expressions.get(&form.expression) {
            return Ok(site.clone());
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
        let identity = self.identities.issue(provenance)?;
        let owner = self.table.add_owner(
            identity,
            inputs,
            vec![solve::SolvePureCallOutput::result(output)],
            provenance,
            |builder, inputs, outputs| {
                let values = form
                    .inputs
                    .iter()
                    .zip(inputs)
                    .map(|((key, ty, _), input)| {
                        let register = builder.load(*input, provenance)?;
                        Ok((*key, LoweredValue::scalar(*ty, register)))
                    })
                    .collect::<Result<Vec<_>, solve::SolveProgramConstructionError>>()?;
                let mut lowerer =
                    ExpressionLowerer::for_model_coordinates(view, builder, HashMap::new());
                for (key, value) in values {
                    match key {
                        ExpressionInput::Coordinate(key) => {
                            lowerer.model_coordinates.insert(key, value);
                        }
                        ExpressionInput::Binder(domain, ordinal) => {
                            lowerer.binders.insert(
                                (domain.index(), ordinal),
                                value.only_register(provenance)?,
                            );
                        }
                    }
                }
                let value = lowerer
                    .expression(form.expression)?
                    .only_register(provenance)?;
                let [output] = outputs else {
                    return Err(solve::SolveProgramConstructionError::InvalidCallOutput {
                        provenance,
                    });
                };
                lowerer.builder.store(*output, value, provenance)
            },
        )?;
        let site = self
            .table
            .call_site(owner)
            .ok_or(solve::SolveProgramConstructionError::UnknownCallOwner { provenance })?;
        self.identities
            .expressions
            .insert(form.expression, site.clone());
        Ok(site)
    }
}
