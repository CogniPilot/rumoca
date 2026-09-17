use super::*;
use rumoca_core::FunctionDerivativeInput;

#[derive(Clone)]
pub(in crate::construction) struct FunctionDerivativeCertificate {
    pub(in crate::construction) source: usize,
    pub(in crate::construction) target: usize,
    pub(in crate::construction) previous: Option<usize>,
    pub(in crate::construction) priority: u32,
    pub(in crate::construction) inputs: Vec<FunctionDerivativeInput>,
    pub(in crate::construction) span: Span,
    order: u32,
}

impl ShapeAnalyzer<'_> {
    pub(super) fn discover_derivative_calls(&mut self) -> Result<(), ToDaeError> {
        let mut function = 0;
        let mut chain = 0;
        while function < self.analysis.certificates.len() || chain < self.analysis.derivatives.len()
        {
            if function < self.analysis.certificates.len() {
                self.discover_derivative_order(function, None, 1)?;
                function += 1;
            } else {
                self.discover_next_derivative(chain)?;
                chain += 1;
            }
        }
        Ok(())
    }

    fn discover_next_derivative(&mut self, chain: usize) -> Result<(), ToDaeError> {
        let prior = &self.analysis.derivatives[chain];
        let Some(order) = prior.order.checked_add(1) else {
            return Ok(());
        };
        self.discover_derivative_order(prior.target, Some(chain), order)
    }

    fn discover_derivative_order(
        &mut self,
        source: usize,
        previous: Option<usize>,
        order: u32,
    ) -> Result<(), ToDaeError> {
        let function = &self.flat.functions[&self.analysis.certificates[source].key.function];
        for (priority, annotation) in function.derivatives.iter().enumerate() {
            if annotation.order != order {
                continue;
            }
            let span = annotation
                .derivative_function
                .span()
                .unwrap_or(function.span);
            let priority = u32::try_from(priority).map_err(|_| {
                ToDaeError::unsupported_flat(
                    "function derivative",
                    "derivative priority exceeds the DAE domain",
                    span,
                )
            })?;
            let key = self.derivative_key(source, previous, annotation, span)?;
            // Annotation reachability is not an executable call edge. A
            // supplied derivative may call its primal (e.g. Frames.resolve1).
            let target = self.ensure_specialization(key, span)?;
            self.analysis
                .derivatives
                .push(FunctionDerivativeCertificate {
                    source,
                    target,
                    previous,
                    priority,
                    inputs: annotation.inputs.clone(),
                    span,
                    order,
                });
        }
        Ok(())
    }

    fn derivative_key(
        &self,
        source: usize,
        previous: Option<usize>,
        annotation: &rumoca_core::DerivativeAnnotation,
        span: Span,
    ) -> Result<FunctionSpecializationKey, ToDaeError> {
        let reference = &annotation.derivative_function;
        let target = reference
            .resolved_function()
            .and_then(|reference| self.flat.get_function_instance(reference.instance_id))
            .ok_or_else(|| {
                ToDaeError::unsupported_flat(
                    "function derivative",
                    "derivative target has no exact collected function instance",
                    span,
                )
            })?;
        let certificate = &self.analysis.certificates[source];
        let function = &self.flat.functions[&certificate.key.function];
        if annotation.inputs.len() != function.inputs.len() {
            return Err(ToDaeError::unsupported_flat(
                "function derivative",
                "derivative restrictions do not cover the source inputs",
                span,
            ));
        }
        let tangent_start = previous.map_or(0, |previous| {
            let previous = &self.analysis.derivatives[previous];
            self.analysis.certificates[previous.source].parameters.len()
        });
        let mut inputs = certificate.parameters.clone();
        for (ordinal, parameter) in function.inputs.iter().enumerate().skip(tangent_start) {
            if annotation.inputs[ordinal] != FunctionDerivativeInput::Differentiate {
                continue;
            }
            match self.classify_derivative_input(parameter) {
                DerivativeInputKind::Real => {
                    inputs.push(certificate.parameters[ordinal].clone());
                }
                DerivativeInputKind::NoReal
                | DerivativeInputKind::ExternalObject
                | DerivativeInputKind::RealFreeRecord => {}
                DerivativeInputKind::RealBearingRecord => {
                    return Err(ToDaeError::unsupported_flat(
                        "function derivative",
                        "record inputs must be decomposed before derivative specialization",
                        parameter.span,
                    ));
                }
                DerivativeInputKind::Opaque => {
                    return Err(ToDaeError::unsupported_flat(
                        "function derivative",
                        "derivative specialization does not model this input type",
                        parameter.span,
                    ));
                }
            }
        }
        let input_values = (0..inputs.len())
            .map(|ordinal| {
                self.analysis
                    .value_read_inputs
                    .reads_value(&target.name, ordinal)
                    .then(|| certificate.key.input_values.get(ordinal).copied().flatten())
                    .flatten()
            })
            .collect();
        Ok(FunctionSpecializationKey {
            function: target.name.clone(),
            inputs,
            input_values,
        })
    }

    /// Classify one source function input for MLS §12.7.1 tangent selection.
    ///
    /// MLS §12.7.1 gives the derivative function's inputs as all original
    /// inputs followed by one derivative "for each input containing reals". A
    /// predefined non-Real scalar, an enumeration, and an `ExternalObject`
    /// (MLS §12.9.7, a class extending the predefined `ExternalObject` that
    /// holds opaque C state) contain no Real, so none receives a tangent. A
    /// record is Real-bearing exactly when one of its leaves is a Real, and a
    /// Real-bearing record must have been decomposed into scalar fields before
    /// this point; a record with no Real leaf carries no tangent.
    fn classify_derivative_input(
        &self,
        parameter: &rumoca_core::FunctionParam,
    ) -> DerivativeInputKind {
        match analysis::effective_function_scalar_type(self.flat, parameter) {
            Some(dae::ScalarType::Real) => DerivativeInputKind::Real,
            Some(_) => DerivativeInputKind::NoReal,
            None => self.classify_aggregate_input(parameter),
        }
    }

    /// Classify a function input whose effective type is not a predefined
    /// scalar or enumeration: a record, an `ExternalObject`, or an unmodeled
    /// type.
    fn classify_aggregate_input(
        &self,
        parameter: &rumoca_core::FunctionParam,
    ) -> DerivativeInputKind {
        let Some(type_def_id) = parameter.type_def_id else {
            return DerivativeInputKind::Opaque;
        };
        if self.flat.record_types.contains_key(&type_def_id) {
            return self.classify_record_input(type_def_id, &parameter.type_name);
        }
        if external_object_constructor(self.flat, type_def_id).is_some() {
            return DerivativeInputKind::ExternalObject;
        }
        DerivativeInputKind::Opaque
    }

    /// Classify a record function input by whether it carries a Real leaf.
    fn classify_record_input(&self, type_def_id: DefId, type_name: &str) -> DerivativeInputKind {
        let mut visited = HashSet::default();
        if self.record_contains_real(type_def_id, type_name, &mut visited) {
            DerivativeInputKind::RealBearingRecord
        } else {
            DerivativeInputKind::RealFreeRecord
        }
    }

    /// Whether the record type `type_def_id` has a Real leaf.
    ///
    /// The record's constructor inputs are its declared fields (see
    /// [`rumoca_core::resolve_record_constructor`]); a field is a Real leaf
    /// when its effective scalar type is Real, and a nested record contributes
    /// a Real leaf exactly when it is itself Real-bearing. A field whose type
    /// resolves to no record layout and no scalar type cannot contribute a
    /// Real leaf. `visited` guards the recursion against a field layout that
    /// names its own type.
    fn record_contains_real(
        &self,
        type_def_id: DefId,
        type_name: &str,
        visited: &mut HashSet<DefId>,
    ) -> bool {
        if !visited.insert(type_def_id) {
            return false;
        }
        let Ok(constructor) = rumoca_core::resolve_record_constructor(
            self.flat.functions.values(),
            type_name,
            type_def_id,
        ) else {
            return false;
        };
        let contains = constructor
            .inputs
            .iter()
            .any(|field| self.field_contains_real(field, visited));
        visited.remove(&type_def_id);
        contains
    }

    /// Whether one record field is, or transitively holds, a Real leaf.
    fn field_contains_real(
        &self,
        field: &rumoca_core::FunctionParam,
        visited: &mut HashSet<DefId>,
    ) -> bool {
        match analysis::effective_function_scalar_type(self.flat, field) {
            Some(dae::ScalarType::Real) => true,
            Some(_) => false,
            None => field.type_def_id.is_some_and(|field_type_def_id| {
                self.flat.record_types.contains_key(&field_type_def_id)
                    && self.record_contains_real(field_type_def_id, &field.type_name, visited)
            }),
        }
    }
}

/// MLS §12.7.1 classification of one source function input.
///
/// The classification is total: every accepted function input is a predefined
/// scalar, an enumeration, an `ExternalObject`, or a record. `Opaque` names a
/// type derivative specialization does not model so the tangent decision never
/// silently drops or invents an input.
enum DerivativeInputKind {
    /// A Real scalar; the derivative call appends one tangent.
    Real,
    /// A predefined non-Real scalar or an enumeration; contains no Real.
    NoReal,
    /// An `ExternalObject` (MLS §12.9.7); contains no Real.
    ExternalObject,
    /// A record with at least one Real leaf, not decomposed into scalar fields.
    RealBearingRecord,
    /// A record whose leaves are all non-Real.
    RealFreeRecord,
    /// A type this analysis does not classify.
    Opaque,
}

/// The `ExternalObject` constructor for `type_def_id`, if the Flat IR retained
/// it.
///
/// MLS §12.9.7 lets an `ExternalObject` value be produced only by its own
/// constructor, an external function that returns the external-object type. So
/// a Flat function with an external body whose single output declares
/// `type_def_id` is that type's constructor, and its presence proves the type
/// is an `ExternalObject` rather than a record or a predefined type.
fn external_object_constructor(
    flat: &flat::Model,
    type_def_id: DefId,
) -> Option<&rumoca_core::Function> {
    flat.functions.values().find(|function| {
        function.external.is_some()
            && matches!(
                function.outputs.as_slice(),
                [output] if output.type_def_id == Some(type_def_id)
            )
    })
}
