//! One checked selected body per reused source node, never per output point.

use super::*;
use std::{cell::Cell, collections::HashSet, rc::Rc, sync::Arc};

pub(in crate::lower::typed_functions) type SelectedCalls<'dae> =
    Arc<HashMap<dae::ExprId<'dae>, SelectedCall<'dae>>>;

#[derive(Clone)]
pub(in crate::lower::typed_functions) struct SelectedCall<'dae> {
    site: solve::SolvePureCallSite,
    captures: Arc<[Capture<'dae>]>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum Key<'dae> {
    Model(ModelCoordinateKey<'dae>),
    Parameter(dae::FunctionParameterId<'dae>),
    Binder(dae::DomainId<'dae>, u32),
}

#[derive(Clone, Copy)]
struct Capture<'dae> {
    key: Key<'dae>,
    value_type: dae::ValueTypeId<'dae>,
}

struct SourceNode<'dae> {
    source: SourceExpression<'dae>,
    shared: Cell<bool>,
}

impl SourceNode<'_> {
    fn needs_owner(&self) -> bool {
        self.shared.get()
            && !self.source.node.value_type().dimensions().contains(&0)
            && !matches!(
                self.source.node.operation(),
                dae::ExpressionOperation::Literal(_) | dae::ExpressionOperation::Coordinate(_)
            )
    }
}

struct SourceGraph<'dae> {
    order: Vec<Rc<SourceNode<'dae>>>,
    seen: HashMap<dae::ExprId<'dae>, Rc<SourceNode<'dae>>>,
}

impl<'dae> SourceGraph<'dae> {
    fn of(
        view: dae::DaeView<'dae>,
        roots: impl IntoIterator<Item = dae::ExprId<'dae>>,
    ) -> Result<Self, solve::SolveProgramConstructionError> {
        let mut proof = rumoca_eval_dae::ZeroCoefficients::default();
        let mut selected = Vec::new();
        let mut traversal = dae::ExpressionTraversal::new();
        traversal.visit_pruned(view, roots, |_, node| {
            selected.extend(selected_operands(view, node, &mut proof));
            true
        });
        let mut graph = Self {
            order: Vec::new(),
            seen: HashMap::new(),
        };
        for root in selected {
            graph.visit(view, root)?;
        }
        Ok(graph)
    }

    fn visit(
        &mut self,
        view: dae::DaeView<'dae>,
        expression: dae::ExprId<'dae>,
    ) -> Result<(), solve::SolveProgramConstructionError> {
        if let Some(node) = self.seen.get(&expression) {
            node.shared.set(true);
            return Ok(());
        }
        let source = SourceExpression::construct(view, expression)?;
        let node = Rc::new(SourceNode {
            source,
            shared: Cell::new(false),
        });
        self.seen.insert(expression, node.clone());
        for child in children(source.node.operation()) {
            self.visit(view, child)?;
        }
        self.order.push(node);
        Ok(())
    }
}

fn selected_operands<'dae>(
    view: dae::DaeView<'dae>,
    node: dae::ExpressionView<'dae>,
    proof: &mut rumoca_eval_dae::ZeroCoefficients<'dae>,
) -> Vec<dae::ExprId<'dae>> {
    let dae::ExpressionOperation::Binary {
        operator: dae::BinaryOperator::Multiply | dae::BinaryOperator::ElementwiseMultiply,
        lhs,
        rhs,
    } = node.operation()
    else {
        return Vec::new();
    };
    let patterns = [
        proof.omission_pattern(view, rhs, lhs),
        proof.omission_pattern(view, lhs, rhs),
    ];
    if !patterns.iter().flatten().any(|p| p.may_prove_zero())
        || patterns.iter().flatten().any(|p| p.proves_all_zero())
    {
        return Vec::new();
    }
    [(lhs, patterns[1].is_some()), (rhs, patterns[0].is_some())]
        .into_iter()
        .filter_map(|(expression, total)| total.then_some(expression))
        .collect()
}

fn children(operation: dae::ExpressionOperation<'_>) -> Vec<dae::ExprId<'_>> {
    match operation {
        dae::ExpressionOperation::Unary { operand, .. } => vec![operand],
        dae::ExpressionOperation::Binary { lhs, rhs, .. } => vec![lhs, rhs],
        dae::ExpressionOperation::Array(values) => values.iter().collect(),
        dae::ExpressionOperation::Builtin { arguments, .. } => arguments.iter().collect(),
        dae::ExpressionOperation::Index { base, .. } => vec![base],
        _ => Vec::new(),
    }
}

pub(in crate::lower::typed_functions) fn register_selections<'dae>(
    table: &mut solve::SolvePureCallTableBuilder,
    view: dae::DaeView<'dae>,
    roots: impl IntoIterator<Item = dae::ExprId<'dae>>,
    identities: &mut CallRegistration<'dae>,
) -> Result<SelectedCalls<'dae>, solve::SolveProgramConstructionError> {
    let graph = SourceGraph::of(view, roots)?;
    for node in &graph.order {
        let source = node.source;
        let expression = source.id;
        if !node.needs_owner() || identities.selected_calls.contains_key(&expression) {
            continue;
        }
        let node = source.node;
        let at = node.provenance().span();
        let captures = collect_captures(view, expression)?;
        let mut inputs = captures
            .iter()
            .map(|capture| lower_primitive_type(view, capture.value_type, arithmetic_profile()))
            .collect::<Result<Vec<_>, _>>()?;
        let rank = node.value_type().dimensions().len();
        inputs.extend(std::iter::repeat_n(
            solve::SolveValueType::scalar(solve::SolveScalarType::Integer(
                arithmetic_profile().integer_domain(),
            )),
            rank,
        ));
        let scalar =
            lower_primitive_type(view, node.value_type_id(), arithmetic_profile())?.element_type();
        let output = solve::SolveValueType::scalar(scalar);
        let selected: SelectedCalls<'dae> = Arc::new(identities.selected_calls.clone());
        let identity = identities.issue(at)?;
        let owner = table.add_owner(
            identity,
            inputs,
            vec![solve::SolvePureCallOutput::result(output)],
            at,
            |builder, inputs, outputs| {
                let mut lowerer =
                    ExpressionLowerer::for_model_coordinates(view, builder, HashMap::new());
                lowerer.selected_calls = selected;
                for (capture, &slot) in captures.iter().zip(inputs) {
                    capture.bind(&mut lowerer, slot, at)?;
                }
                let coordinates = inputs[captures.len()..]
                    .iter()
                    .map(|&slot| lowerer.builder.load(slot, at))
                    .collect::<Result<Vec<_>, _>>()?;
                let value = lowerer.selected_operation(source, &coordinates)?;
                lowerer.builder.store(outputs[0], value, at)
            },
        )?;
        let site = table
            .call_site(owner)
            .ok_or(solve::SolveProgramConstructionError::UnknownCallOwner { provenance: at })?;
        identities.selected_calls.insert(
            expression,
            SelectedCall {
                site,
                captures: captures.into(),
            },
        );
    }
    Ok(Arc::new(identities.selected_calls.clone()))
}

fn collect_captures<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
) -> Result<Vec<Capture<'dae>>, solve::SolveProgramConstructionError> {
    let mut captures = Vec::new();
    let mut seen = HashSet::new();
    let mut invalid = None;
    let mut traversal = dae::ExpressionTraversal::new();
    traversal.visit_pruned(view, [expression], |_, node| {
        if let dae::ExpressionOperation::Coordinate(coordinate) = node.operation() {
            let key = match coordinate {
                dae::CoordinateView::FunctionParameter(parameter) => {
                    Some(Key::Parameter(parameter))
                }
                dae::CoordinateView::Binder(binder) => {
                    Some(Key::Binder(binder.domain(), binder.ordinal()))
                }
                _ => ModelCoordinateKey::from_view(coordinate).map(Key::Model),
            };
            let Some(key) = key else {
                invalid.get_or_insert(node.provenance().span());
                return false;
            };
            if seen.insert(key) {
                captures.push(Capture {
                    key,
                    value_type: node.value_type_id(),
                });
            }
        }
        true
    });
    match invalid {
        Some(provenance) => {
            Err(solve::SolveProgramConstructionError::InvalidCallInterface { provenance })
        }
        None => Ok(captures),
    }
}

impl<'dae> Capture<'dae> {
    fn bind<'program>(
        &self,
        lowerer: &mut ExpressionLowerer<'_, 'program, 'dae>,
        slot: solve::ProgramSlot<'program>,
        at: rumoca_core::Span,
    ) -> Result<(), solve::SolveProgramConstructionError> {
        let value = lowerer.builder.load(slot, at)?;
        match self.key {
            Key::Model(key) => {
                lowerer
                    .model_coordinates
                    .insert(key, LoweredValue::scalar(self.value_type, value));
            }
            Key::Parameter(key) => {
                lowerer
                    .parameters
                    .insert(key, LoweredValue::scalar(self.value_type, value));
            }
            Key::Binder(domain, ordinal) => {
                lowerer.binders.insert((domain.index(), ordinal), value);
            }
        }
        Ok(())
    }

    fn read<'program>(
        &self,
        lowerer: &ExpressionLowerer<'_, 'program, 'dae>,
        at: rumoca_core::Span,
    ) -> Result<solve::ProgramRegister<'program>, solve::SolveProgramConstructionError> {
        match self.key {
            Key::Model(key) => lowerer
                .model_coordinates
                .get(&key)
                .and_then(|value| value.leaves.first())
                .copied(),
            Key::Parameter(key) => lowerer
                .parameters
                .get(&key)
                .and_then(|value| value.leaves.first())
                .copied(),
            Key::Binder(domain, ordinal) => {
                lowerer.binders.get(&(domain.index(), ordinal)).copied()
            }
        }
        .ok_or(solve::SolveProgramConstructionError::InvalidCallInterface { provenance: at })
    }
}

impl<'program, 'dae> ExpressionLowerer<'_, 'program, 'dae> {
    pub(in crate::lower::typed_functions) fn selected_call(
        &mut self,
        expression: dae::ExprId<'dae>,
        coordinates: &[solve::ProgramRegister<'program>],
        at: rumoca_core::Span,
    ) -> Result<Option<solve::ProgramRegister<'program>>, solve::SolveProgramConstructionError>
    {
        let Some(call) = self.selected_calls.get(&expression).cloned() else {
            return Ok(None);
        };
        let mut arguments = call
            .captures
            .iter()
            .map(|capture| capture.read(self, at))
            .collect::<Result<Vec<_>, _>>()?;
        arguments.extend_from_slice(coordinates);
        self.builder
            .emit_call(call.site.owner(), &arguments, at)
            .map(|call| Some(call.into_registers()[0]))
    }
}
