//! One continuous algebraic family as one synthesized pure-call owner
//! (SPEC_0043 §6c, SPEC_0040 SOLVE-C70).
//!
//! The admission proof reads only the DAE family: one body
//! `target[binders] - rhs` whose subscripts are the family binders in
//! declaration order, each ranging `1:extent` over the target's extents, so
//! domain order is the target's row-major order; `rhs` is a scalar of the
//! target's scalar type; and it reads no coordinate of the target, no
//! derivative or other non-variable coordinate, and no function call. Literal
//! constants it reads are folded into the body. The owner's result is
//! `{rhs for domain}`, one typed map over the domain, so its body has the
//! size of the source, not of the domain.

use std::collections::HashMap;

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use super::{
    ExpressionLowerer, LoweredValue, PureCallRegistry, arithmetic_profile, lower_primitive_type,
    model_coordinates::{ModelCoordinateKey, collect_model_coordinate_types},
};

/// The checked form of an admitted family.
pub(crate) struct AlgebraicFamilyForm<'dae> {
    /// The algebraic variable whose every scalar the family defines.
    pub(crate) target: dae::AlgebraicId<'dae>,
    /// The aggregate coordinate expression of the target.
    pub(crate) target_expression: dae::ExprId<'dae>,
    rhs: dae::ExprId<'dae>,
    domain: dae::DomainId<'dae>,
    /// The aggregate model coordinates `rhs` reads, in stable owner input
    /// order, each with one expression that reads it whole.
    inputs: Vec<(
        ModelCoordinateKey<'dae>,
        dae::ValueTypeId<'dae>,
        dae::ExprId<'dae>,
    )>,
    /// Constants (MLS 3.7 §4.5) `rhs` reads, each with its literal binding:
    /// translation-time values of the body, not owner inputs.
    constants: Vec<(ModelCoordinateKey<'dae>, dae::ExprId<'dae>)>,
    pub(crate) scalar_count: usize,
    pub(crate) provenance: rumoca_core::Span,
}

impl<'dae> AlgebraicFamilyForm<'dae> {
    /// One expression reading each owner input whole, in input order.
    pub(crate) fn input_readers(&self) -> impl Iterator<Item = dae::ExprId<'dae>> + '_ {
        self.inputs.iter().map(|(_, _, reader)| *reader)
    }

    /// The family's form when its admission proof holds.
    pub(crate) fn admit(
        view: dae::DaeView<'dae>,
        family: dae::StructuredFamilyView<'dae>,
    ) -> Option<Self> {
        if family.scalar_view() != rumoca_core::ComprehensionScalarView::BinderSubstitution {
            return None;
        }
        let [body] = family.bodies().iter().collect::<Vec<_>>()[..] else {
            return None;
        };
        let dae::ExpressionOperation::Binary {
            operator: dae::BinaryOperator::Subtract,
            lhs,
            rhs,
        } = view.expression(body)?.operation()
        else {
            return None;
        };
        let dae::ExpressionOperation::Index { base, subscripts } =
            view.expression(lhs)?.operation()
        else {
            return None;
        };
        let target_node = view.expression(base)?;
        let dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(target)) =
            target_node.operation()
        else {
            return None;
        };
        // The result is an aggregate of the target's type, so `rhs` must
        // already have the target's scalar type (no implicit conversion).
        let rhs_type = view.expression(rhs)?.value_type();
        if !rhs_type.dimensions().is_empty()
            || rhs_type.scalar_type() != target_node.value_type().scalar_type()
            || rhs_type.is_record()
        {
            return None;
        }
        let domain = view.domain(family.domain())?;
        let extents = target_node.value_type().dimensions();
        let binders = &domain.structured().binders;
        if subscripts.len() != binders.len() || extents.len() != binders.len() {
            return None;
        }
        for (ordinal, ((subscript, binder), &extent)) in
            subscripts.iter().zip(binders).zip(extents).enumerate()
        {
            let dae::SubscriptView::Index { expression, .. } = subscript else {
                return None;
            };
            let dae::ExpressionOperation::Coordinate(dae::CoordinateView::Binder(reference)) =
                view.expression(expression)?.operation()
            else {
                return None;
            };
            if reference.domain() != family.domain()
                || reference.ordinal() as usize != ordinal
                || binder.lower != 1
                || binder.step != 1
                || binder.upper != i64::from(extent)
            {
                return None;
            }
        }
        let (constants, inputs) = admitted_inputs(view, rhs, target, family.provenance().span())?;
        Some(Self {
            target,
            target_expression: base,
            rhs,
            domain: family.domain(),
            inputs,
            constants,
            scalar_count: domain.scalar_count() as usize,
            provenance: family.provenance().span(),
        })
    }
}

/// The coordinates `rhs` reads when it reads none of the target, no
/// derivative, and only parameter, input, state, and other algebraic
/// coordinates.
#[allow(clippy::type_complexity)]
fn admitted_inputs<'dae>(
    view: dae::DaeView<'dae>,
    rhs: dae::ExprId<'dae>,
    target: dae::AlgebraicId<'dae>,
    provenance: rumoca_core::Span,
) -> Option<(
    Vec<(ModelCoordinateKey<'dae>, dae::ExprId<'dae>)>,
    Vec<(
        ModelCoordinateKey<'dae>,
        dae::ValueTypeId<'dae>,
        dae::ExprId<'dae>,
    )>,
)> {
    let mut admitted = true;
    let mut readers = HashMap::new();
    dae::for_each_expression(view, rhs, |expression, node| match node.operation() {
        dae::ExpressionOperation::Coordinate(coordinate) => match coordinate {
            dae::CoordinateView::Algebraic(read) if read == target => admitted = false,
            dae::CoordinateView::Parameter(_)
            | dae::CoordinateView::Input(_)
            | dae::CoordinateView::State(_)
            | dae::CoordinateView::Algebraic(_) => {
                if let Some(key) = ModelCoordinateKey::from_view(coordinate) {
                    readers.entry(key).or_insert(expression);
                }
            }
            dae::CoordinateView::Binder(_) => {}
            _ => admitted = false,
        },
        dae::ExpressionOperation::Call { .. } => admitted = false,
        _ => {}
    });
    if !admitted {
        return None;
    }
    let coordinates = collect_model_coordinate_types(view, [rhs], [], provenance).ok()?;
    let mut constants = Vec::new();
    let mut inputs = Vec::new();
    for (key, value_type) in coordinates {
        match constant_binding(view, key) {
            Some(binding) => constants.push((key, binding)),
            None => inputs.push((key, value_type, *readers.get(&key)?)),
        }
    }
    Some((constants, inputs))
}

/// The literal binding of a constant-variability parameter.
fn constant_binding<'dae>(
    view: dae::DaeView<'dae>,
    key: ModelCoordinateKey<'dae>,
) -> Option<dae::ExprId<'dae>> {
    let ModelCoordinateKey::Parameter(parameter) = key else {
        return None;
    };
    let variable = view.variable(parameter.into())?;
    let binding = variable.binding()?;
    (variable.variability() == dae::ExpressionVariability::Constant
        && matches!(
            view.expression(binding)?.operation(),
            dae::ExpressionOperation::Literal(_)
        ))
    .then_some(binding)
}

impl<'dae> PureCallRegistry<'dae> {
    /// The owner of an admitted family, issued once per DAE family.
    pub(crate) fn register_algebraic_family(
        &mut self,
        view: dae::DaeView<'dae>,
        family: dae::ContinuousFamilyId<'dae>,
        form: &AlgebraicFamilyForm<'dae>,
    ) -> Result<solve::SolvePureCallSite, solve::SolveProgramConstructionError> {
        if let Some(site) = self.identities.families.get(&family) {
            return Ok(site.clone());
        }
        let provenance = form.provenance;
        let inputs = form
            .inputs
            .iter()
            .map(|(_, value_type, _)| lower_primitive_type(view, *value_type, arithmetic_profile()))
            .collect::<Result<Vec<_>, _>>()?;
        let result_type = view
            .expression(form.target_expression)
            .ok_or(solve::SolveProgramConstructionError::WireMismatch)?
            .value_type_id();
        let output = lower_primitive_type(view, result_type, arithmetic_profile())?;
        let identity = self.identities.issue(provenance)?;
        let owner = self.table.add_owner(
            identity,
            inputs,
            vec![solve::SolvePureCallOutput::result(output)],
            provenance,
            |builder, inputs, outputs| {
                let model_coordinates = form
                    .inputs
                    .iter()
                    .zip(inputs)
                    .map(|((key, value_type, _), input)| {
                        let register = builder.load(*input, provenance)?;
                        Ok((*key, LoweredValue::scalar(*value_type, register)))
                    })
                    .collect::<Result<HashMap<_, _>, solve::SolveProgramConstructionError>>()?;
                let mut lowerer =
                    ExpressionLowerer::for_model_coordinates(view, builder, model_coordinates);
                for (key, binding) in &form.constants {
                    let value = lowerer.expression(*binding)?;
                    lowerer.model_coordinates.insert(*key, value);
                }
                let value = lowerer
                    .comprehension(result_type, form.domain, form.rhs, provenance)?
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
        self.identities.families.insert(family, site.clone());
        Ok(site)
    }
}
