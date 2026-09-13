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
            match analysis::effective_function_scalar_type(self.flat, parameter) {
                Some(dae::ScalarType::Real) => inputs.push(certificate.parameters[ordinal].clone()),
                Some(_) => {}
                None => {
                    return Err(ToDaeError::unsupported_flat(
                        "function derivative",
                        "record inputs must be decomposed before derivative specialization",
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
}
