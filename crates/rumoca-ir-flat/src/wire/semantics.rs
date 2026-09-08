use super::*;

mod expression_shapes;
mod record_values;

use record_values::*;

pub(super) fn validate_family_provenance(
    checker: &mut WireSemanticChecker<'_>,
    family: &StructuredEquationFamily,
) -> Result<(), FlatWireError> {
    require_span(family.span, "structured equation owner")?;
    if let Some(template) = &family.template {
        checker.enter_structured_scope(&family.domain)?;
        for expression in &template.body {
            let result = checker.visit_owned_expression(
                expression,
                &family.origin,
                ConnectionExpressionKind::Residual,
            );
            if let Err(error) = result {
                checker.leave_structured_scope();
                return Err(error);
            }
        }
        checker.leave_structured_scope();
    } else if matches!(
        family.origin,
        EquationOrigin::Connection { .. }
            | EquationOrigin::OutsideStream { .. }
            | EquationOrigin::EqualityConstraint { .. }
            | EquationOrigin::FlowSum { .. }
    ) {
        return Err(FlatWireError::InvalidConnectionEvidence {
            reason: "a connection-owned structured family requires its exact equation template",
        });
    }
    Ok(())
}

pub(super) fn validate_algorithm(
    checker: &mut WireSemanticChecker<'_>,
    algorithm: &Algorithm,
) -> Result<(), FlatWireError> {
    require_span(algorithm.span, "algorithm owner")?;
    let derived_outputs = extract_algorithm_outputs(&algorithm.statements);
    if derived_outputs != algorithm.outputs {
        return Err(FlatWireError::InvalidAlgorithm {
            reason: "its serialized output summary contradicts its statement write targets",
        });
    }
    for statement in &algorithm.statements {
        FallibleStatementVisitor::visit_statement(checker, statement)?;
    }
    for output in &derived_outputs {
        let component_ref =
            output
                .component_ref()
                .ok_or_else(|| FlatWireError::InvalidWriteTarget {
                    name: output.var_name().clone(),
                    reason: "a derived algorithm output requires its exact component reference",
                })?;
        checker.validate_write_target(component_ref)?;
    }
    Ok(())
}

pub(super) fn validate_when_chain(
    checker: &mut WireSemanticChecker<'_>,
    chain: &WhenChain,
) -> Result<(), FlatWireError> {
    require_span(chain.span(), "when-chain owner")?;
    for branch in chain.branches() {
        require_span(branch.span, "when branch")?;
        checker.visit_expression(&branch.condition)?;
        for equation in &branch.equations {
            validate_when_equation(checker, equation)?;
        }
    }
    Ok(())
}

pub(super) fn validate_function_provenance(
    checker: &mut WireSemanticChecker<'_>,
    function: &Function,
) -> Result<(), FlatWireError> {
    require_span(function.span, "function declaration")?;
    checker.enter_function_scope(function)?;
    let result = (|| {
        for param in function
            .inputs
            .iter()
            .chain(&function.outputs)
            .chain(&function.locals)
        {
            validate_function_param(checker, param)?;
        }
        for statement in &function.body {
            FallibleStatementVisitor::visit_statement(checker, statement)?;
        }
        if let Some(external) = &function.external {
            validate_external_function(checker, external)?;
        }
        Ok(())
    })();
    checker.leave_function_scope();
    result
}

pub(super) fn validate_function_param(
    checker: &mut WireSemanticChecker<'_>,
    param: &FunctionParam,
) -> Result<(), FlatWireError> {
    require_span(param.span, "function parameter declaration")?;
    let model = checker.model;
    let nominal = param.effective_type.nominal_type();
    let canonical = param.effective_type.canonical_type();
    let declared = param
        .type_def_id
        .and_then(|declaration| model.type_ids_by_def_id.get(&declaration).copied());
    if model.type_roots.get(&nominal) != Some(&canonical)
        || model.type_roots.get(&canonical) != Some(&canonical)
        || param.type_def_id.is_some() && declared != Some(nominal)
    {
        return Err(FlatWireError::InvalidTypeCatalog {
            reason: "a function value contradicts its issued nominal, canonical, or declaration identity",
        });
    }
    validate_function_record_param(model, param)?;
    for subscript in &param.shape_expr {
        checker.visit_subscript(subscript)?;
    }
    for expression in [
        param.default.as_ref(),
        param.min.as_ref(),
        param.max.as_ref(),
    ]
    .into_iter()
    .flatten()
    {
        checker.visit_expression(expression)?;
    }
    Ok(())
}

fn validate_function_record_param(
    model: &Model,
    param: &FunctionParam,
) -> Result<(), FlatWireError> {
    let record_declaration = param
        .type_def_id
        .filter(|declaration| model.record_types.contains_key(declaration));
    let claims_record = param.type_class == Some(rumoca_core::ClassType::Record);
    if claims_record != record_declaration.is_some() {
        return Err(FlatWireError::InvalidTypeCatalog {
            reason: "a function value's record class and exact layout declaration disagree",
        });
    }
    let Some(declaration) = record_declaration else {
        return Ok(());
    };
    let layout = model
        .record_types
        .get(&declaration)
        .ok_or(FlatWireError::InvalidTypeCatalog {
            reason: "a function record value has no exact retained layout",
        })?;
    if layout.name != param.type_name {
        return Err(FlatWireError::InvalidTypeCatalog {
            reason: "a function record value's display type contradicts its exact layout",
        });
    }
    let constructor = rumoca_core::resolve_record_constructor(
        model.functions.values(),
        &param.type_name,
        declaration,
    )
    .map_err(|_| FlatWireError::InvalidTypeCatalog {
        reason: "a function record value has no unambiguous exact constructor layout",
    })?;
    validate_record_constructor_layout(model, constructor)
}

fn validate_record_constructor_layout(
    model: &Model,
    constructor: &Function,
) -> Result<(), FlatWireError> {
    let record = constructor
        .def_id
        .filter(|record| constructor.is_constructor && model.record_types.contains_key(record))
        .ok_or(FlatWireError::InvalidTypeCatalog {
            reason: "a record constructor has no exact retained record declaration",
        })?;
    let layout = model
        .record_types
        .get(&record)
        .ok_or(FlatWireError::InvalidRecordCatalog {
            record,
            reason: "a record constructor declaration has no exact field layout",
        })?;
    let exact_fields = constructor.outputs.is_empty()
        && constructor.inputs.len() == layout.fields.len()
        && constructor
            .inputs
            .iter()
            .zip(&layout.fields)
            .all(|(input, field)| {
                let field_is_record = model.record_types.contains_key(&field.type_def_id);
                input.name == field.name
                    && input.def_id == Some(field.def_id)
                    && input.type_def_id == Some(field.type_def_id)
                    && input.effective_type == field.effective_type
                    && input.dimensions() == field.dims
                    && (input.type_class == Some(rumoca_core::ClassType::Record)) == field_is_record
            });
    if exact_fields {
        Ok(())
    } else {
        Err(FlatWireError::InvalidRecordCatalog {
            record,
            reason: "a record constructor's exact input order, types, or empty result ABI contradicts its retained layout",
        })
    }
}

pub(super) fn validate_external_function(
    checker: &mut WireSemanticChecker<'_>,
    external: &ExternalFunction,
) -> Result<(), FlatWireError> {
    for argument in &external.args {
        checker.visit_expression(argument)?;
    }
    for annotation in &external.annotations {
        require_span(annotation.span, "external-function annotation")?;
        checker.visit_expression(&annotation.value)?;
    }
    Ok(())
}

pub(super) fn invalid_dimensions(dimensions: &[i64]) -> bool {
    dimensions
        .iter()
        .try_fold(1usize, |count, dimension| {
            count.checked_mul(usize::try_from(*dimension).ok()?)
        })
        .is_none()
}

pub(super) fn require_span(span: Span, context: &'static str) -> Result<(), FlatWireError> {
    if span.is_dummy() {
        Err(FlatWireError::MissingProvenance { context })
    } else {
        Ok(())
    }
}

/// Standalone equation owners have no Flat root from which to close callable
/// and occurrence catalogs. They may replay locally checkable expressions,
/// but must reject any semantic call rather than treating an absent catalog as
/// permission to accept it.
pub(super) fn validate_standalone_equation_expression(
    expression: &Expression,
) -> Result<(), FlatWireError> {
    StandaloneWireExpressionChecker.visit_expression(expression)
}

struct StandaloneWireExpressionChecker;

impl FallibleExpressionVisitor for StandaloneWireExpressionChecker {
    type Error = FlatWireError;

    fn visit_expression(&mut self, expression: &Expression) -> Result<(), Self::Error> {
        require_span(raw_expression_span(expression), "expression occurrence")?;
        match expression {
            Expression::Empty { .. } => {
                return Err(FlatWireError::RecoveryNode {
                    context: "expression",
                });
            }
            Expression::FunctionCall { name, .. } => {
                return Err(FlatWireError::InvalidFunctionCall {
                    function: name.var_name().clone(),
                    reason: "a standalone equation cannot authenticate a semantic call without the root callable inventory",
                });
            }
            Expression::VarRef { name, .. } => {
                if name.resolved_function().is_some() {
                    return Err(FlatWireError::InvalidReferenceTarget {
                        name: name.var_name().clone(),
                        instance_id: name.instance_id().unwrap_or(InstanceId::UNSET),
                        reason: "a value reference cannot also carry callable-instance identity",
                    });
                }
                if let Some(component_ref) = name.component_ref() {
                    validate_component_reference_provenance(component_ref)?;
                }
                if name.structured_binder().is_none() && name.instance_id().is_none() {
                    return Err(FlatWireError::InvalidReferenceTarget {
                        name: name.var_name().clone(),
                        instance_id: InstanceId::UNSET,
                        reason: "a semantic value reference requires exact target evidence",
                    });
                }
            }
            _ => {}
        }
        self.walk_expression(expression)
    }

    fn visit_subscript(&mut self, subscript: &Subscript) -> Result<(), Self::Error> {
        require_span(subscript.span(), "subscript occurrence")?;
        if let Subscript::Expr { expr, .. } = subscript {
            self.visit_expression(expr)?;
        }
        Ok(())
    }
}

pub(super) struct WireSemanticChecker<'model> {
    model: &'model Model,
    targets: &'model WireTargetIndex<'model>,
    function_roots: rustc_hash::FxHashMap<DefId, WireFunctionRoot>,
    function_names: rustc_hash::FxHashSet<VarName>,
    in_function_scope: bool,
    structured_binders: rustc_hash::FxHashMap<StructuredIndexBinderId, String>,
}

pub(super) struct WireFunctionRoot {
    name: String,
    record_type: Option<DefId>,
    effective_type: EffectiveType,
    dimensions: Box<[i64]>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct WireRecordValueIdentity {
    declaration: DefId,
    nominal: TypeId,
    canonical: TypeId,
}

#[derive(Clone, PartialEq, Eq)]
struct WireExpressionShape {
    dimensions: Vec<i64>,
    record: Option<WireRecordValueIdentity>,
}

impl WireExpressionShape {
    fn scalar() -> Self {
        Self {
            dimensions: Vec::new(),
            record: None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ConnectionExpressionKind {
    Residual,
    Assertion,
}

impl<'model> WireSemanticChecker<'model> {
    pub(super) fn from_closed_root(
        model: &'model Model,
        catalogs: &'model ClosedWireCatalogs<'model>,
    ) -> Self {
        Self {
            model,
            targets: &catalogs.targets,
            function_roots: rustc_hash::FxHashMap::default(),
            function_names: rustc_hash::FxHashSet::default(),
            in_function_scope: false,
            structured_binders: rustc_hash::FxHashMap::default(),
        }
    }

    fn enter_function_scope(&mut self, function: &Function) -> Result<(), FlatWireError> {
        self.function_roots.clear();
        self.function_names.clear();
        self.in_function_scope = true;
        for parameter in function
            .inputs
            .iter()
            .chain(&function.outputs)
            .chain(&function.locals)
        {
            let name = VarName::new(&parameter.name);
            if !self.function_names.insert(name.clone()) {
                return Err(FlatWireError::InvalidWriteTarget {
                    name,
                    reason: "two function-local roots claim one spelling",
                });
            }
            let def_id = parameter
                .def_id
                .ok_or_else(|| FlatWireError::InvalidWriteTarget {
                    name: name.clone(),
                    reason: "a function-local root requires its exact declaration identity",
                })?;
            if def_id.index() == 0 {
                return Err(FlatWireError::InvalidWriteTarget {
                    name,
                    reason: "a function-local root cannot use the reserved global-scope DefId(0)",
                });
            }
            if self
                .function_roots
                .insert(
                    def_id,
                    WireFunctionRoot {
                        name: parameter.name.clone(),
                        record_type: (parameter.type_class == Some(rumoca_core::ClassType::Record))
                            .then_some(parameter.type_def_id)
                            .flatten(),
                        effective_type: parameter.effective_type.clone(),
                        dimensions: parameter.dimensions().into(),
                    },
                )
                .is_some()
            {
                return Err(FlatWireError::InvalidWriteTarget {
                    name: VarName::new(&parameter.name),
                    reason: "two function-local roots claim one declaration identity",
                });
            }
        }
        Ok(())
    }

    fn leave_function_scope(&mut self) {
        self.function_roots.clear();
        self.function_names.clear();
        self.in_function_scope = false;
    }

    fn enter_structured_scope(
        &mut self,
        domain: &rumoca_core::StructuredIndexDomain,
    ) -> Result<(), FlatWireError> {
        if !self.structured_binders.is_empty() {
            return Err(FlatWireError::InvalidStructuredEquationShape {
                partition: "template",
                index: 0,
                reason: "structured binder scopes cannot overlap during replay",
            });
        }
        for (ordinal, binder) in domain.binders.iter().enumerate() {
            let id = StructuredIndexBinderId::from_ordinal(ordinal).ok_or(
                FlatWireError::InvalidStructuredEquationShape {
                    partition: "template",
                    index: 0,
                    reason: "structured binder identity exceeds the current wire range",
                },
            )?;
            if binder.id != id || binder.display_name.is_empty() {
                return Err(FlatWireError::InvalidStructuredEquationShape {
                    partition: "template",
                    index: 0,
                    reason: "structured binders require canonical IDs and nonempty display names",
                });
            }
            if self
                .structured_binders
                .insert(id, binder.display_name.clone())
                .is_some()
            {
                return Err(FlatWireError::InvalidStructuredEquationShape {
                    partition: "template",
                    index: 0,
                    reason: "structured binder identities must be unique within their domain",
                });
            }
        }
        Ok(())
    }

    fn leave_structured_scope(&mut self) {
        self.structured_binders.clear();
    }

    pub(super) fn visit_owned_expression(
        &mut self,
        expression: &Expression,
        origin: &EquationOrigin,
        kind: ConnectionExpressionKind,
    ) -> Result<(), FlatWireError> {
        self.validate_equation_origin(expression, origin, kind)?;
        self.visit_expression(expression)?;
        Ok(())
    }

    fn validate_equation_origin(
        &self,
        expression: &Expression,
        origin: &EquationOrigin,
        kind: ConnectionExpressionKind,
    ) -> Result<(), FlatWireError> {
        let model = self.model;
        match origin {
            EquationOrigin::Connection { .. }
            | EquationOrigin::OutsideStream { .. }
            | EquationOrigin::EqualityConstraint { .. }
            | EquationOrigin::FlowSum { .. }
            | EquationOrigin::UnconnectedFlow { .. } => {
                Err(FlatWireError::InvalidConnectionEvidence {
                    reason: "connection-derived rows require canonical Instance source groups and cannot be admitted by standalone equation origins",
                })
            }
            EquationOrigin::Binding { variable } => {
                validate_named_equation_owner(model, expression, variable, kind, "binding")?;
                Ok(())
            }
            EquationOrigin::Reinit { state } => {
                validate_named_equation_owner(model, expression, state, kind, "reinit")?;
                Ok(())
            }
            EquationOrigin::WhenAssignment { target } => {
                validate_named_equation_owner(model, expression, target, kind, "when assignment")?;
                Ok(())
            }
            EquationOrigin::Algorithm { .. } => Err(FlatWireError::InvalidEquationOrigin {
                reason: "an algorithm-derived equation lacks an exact Flat owner identity",
            }),
            EquationOrigin::ComponentEquation { .. } => Ok(()),
        }
    }

    fn validate_value_reference(&mut self, reference: &Reference) -> Result<(), FlatWireError> {
        if let Some(binder) = reference.structured_binder() {
            return self.validate_structured_binder_reference(reference, binder);
        }
        if self
            .structured_binders
            .values()
            .any(|display_name| display_name == reference.var_name().as_str())
            && !reference
                .component_ref()
                .is_some_and(ComponentReference::local)
        {
            return Err(FlatWireError::InvalidReferenceTarget {
                name: reference.var_name().clone(),
                instance_id: reference.instance_id().unwrap_or(InstanceId::UNSET),
                reason: "a lexical structured-binder spelling lacks its typed domain identity",
            });
        }
        if reference.resolved_function().is_some() {
            return Err(FlatWireError::InvalidReferenceTarget {
                name: reference.var_name().clone(),
                instance_id: reference.instance_id().unwrap_or(InstanceId::UNSET),
                reason: "a value reference cannot also carry callable-instance identity",
            });
        }
        if let Some(component_ref) = reference.component_ref() {
            validate_component_reference_provenance(component_ref)?;
        }
        let Some(instance_id) = reference.instance_id() else {
            if let Some(component_ref) = reference.component_ref()
                && self.validate_function_local_reference(reference, component_ref)?
            {
                return Ok(());
            }
            return Err(FlatWireError::InvalidReferenceTarget {
                name: reference.var_name().clone(),
                instance_id: InstanceId::UNSET,
                reason: "a semantic value reference requires exact target evidence",
            });
        };
        let Some(target) = self.targets.by_occurrence.get(&instance_id) else {
            return Err(FlatWireError::InvalidReferenceTarget {
                name: reference.var_name().clone(),
                instance_id,
                reason: "the exact occurrence has no Flat variable or record target",
            });
        };
        if reference.component_ref() != Some(target.component_ref) {
            return Err(FlatWireError::InvalidReferenceTarget {
                name: reference.var_name().clone(),
                instance_id,
                reason: "the component reference contradicts the exact occurrence target",
            });
        }
        if reference.var_name() != target.name {
            return Err(FlatWireError::InvalidReferenceTarget {
                name: reference.var_name().clone(),
                instance_id,
                reason: "the cached Flat name contradicts the exact occurrence target",
            });
        }
        Ok(())
    }

    fn validate_structured_binder_reference(
        &self,
        reference: &Reference,
        binder: StructuredIndexBinderId,
    ) -> Result<(), FlatWireError> {
        let Some(display_name) = self.structured_binders.get(&binder) else {
            return Err(FlatWireError::InvalidReferenceTarget {
                name: reference.var_name().clone(),
                instance_id: InstanceId::UNSET,
                reason: "a structured binder target is outside its exact family domain",
            });
        };
        let Some(component_ref) = reference.component_ref() else {
            return Err(FlatWireError::InvalidReferenceTarget {
                name: reference.var_name().clone(),
                instance_id: InstanceId::UNSET,
                reason: "a structured binder target lacks its source spelling and span",
            });
        };
        require_span(component_ref.span(), "structured binder source token")?;
        let [part] = component_ref.parts() else {
            return Err(FlatWireError::InvalidReferenceTarget {
                name: reference.var_name().clone(),
                instance_id: InstanceId::UNSET,
                reason: "a structured binder target must preserve one source token",
            });
        };
        // A source `for` index currently has no resolver-issued declaration
        // DefId. Its semantic identity is `StructuredIndexBinderId`; requiring
        // or manufacturing a DefId here would cross identity namespaces. The
        // component part is retained solely for its source spelling and span.
        if part.span.is_dummy() || !part.subs.is_empty() {
            return Err(FlatWireError::InvalidReferenceTarget {
                name: reference.var_name().clone(),
                instance_id: InstanceId::UNSET,
                reason: "a structured binder target has missing provenance or subscripts",
            });
        }
        if part.ident.as_str() != display_name
            || reference.var_name().as_str() != display_name
            || component_ref.to_var_name() != reference.var_name().clone()
        {
            return Err(FlatWireError::InvalidReferenceTarget {
                name: reference.var_name().clone(),
                instance_id: InstanceId::UNSET,
                reason: "a structured binder target contradicts its exact domain spelling",
            });
        }
        Ok(())
    }

    fn validate_function_local_reference(
        &self,
        reference: &Reference,
        component_ref: &ComponentReference,
    ) -> Result<bool, FlatWireError> {
        if !self.in_function_scope {
            return Ok(false);
        }
        // `ComponentReference::local` records source lookup syntax (including
        // leading-dot/local lookup behavior); it is not a storage-class claim.
        // Function-local ownership is authenticated by the enclosing
        // function's exact (DefId, spelling) root inventory instead.
        if component_ref.to_var_name() != *reference.var_name() {
            return Err(FlatWireError::InvalidReferenceTarget {
                name: reference.var_name().clone(),
                instance_id: InstanceId::UNSET,
                reason: "the cached local name contradicts its component-reference path",
            });
        }
        match self.validate_function_component_path(component_ref) {
            Ok(()) => Ok(true),
            Err(reason) => Err(FlatWireError::InvalidReferenceTarget {
                name: reference.var_name().clone(),
                instance_id: InstanceId::UNSET,
                reason,
            }),
        }
    }

    fn validate_write_target(
        &mut self,
        component_ref: &ComponentReference,
    ) -> Result<(), FlatWireError> {
        self.write_target_shape(component_ref).map(drop)
    }

    fn write_target_shape(
        &self,
        component_ref: &ComponentReference,
    ) -> Result<WireExpressionShape, FlatWireError> {
        validate_component_reference_provenance(component_ref)?;
        let name = component_ref.to_var_name();
        if self.in_function_scope {
            // See `validate_function_local_reference`: the source-lookup flag
            // does not classify this target. Exact root identity does.
            return self
                .function_component_shape(component_ref)
                .map_err(|reason| FlatWireError::InvalidWriteTarget { name, reason });
        }
        let model = self.model;
        let mut matched = None;
        for variable in model.variables.values() {
            let Some(canonical) = variable.component_ref.as_ref() else {
                continue;
            };
            let candidate = matched_write_target_shape(
                component_ref,
                canonical,
                WireExpressionShape {
                    dimensions: variable.dims.clone(),
                    record: None,
                },
                &name,
            )?;
            insert_write_target_match(&mut matched, candidate, &name)?;
        }
        for record in model.record_instances.values() {
            let candidate = matched_write_target_shape(
                component_ref,
                &record.component_ref,
                WireExpressionShape {
                    dimensions: record.dims.clone(),
                    record: Some(record_value_identity(model, record)?),
                },
                &name,
            )?;
            insert_write_target_match(&mut matched, candidate, &name)?;
        }
        if let Some(shape) = matched {
            return Ok(shape);
        }
        Err(FlatWireError::InvalidWriteTarget {
            name,
            reason: "it does not identify an exact Flat variable target",
        })
    }

    fn validate_named_write_target(&self, name: &VarName) -> Result<(), FlatWireError> {
        if self.model.variables.contains_key(name) {
            Ok(())
        } else {
            Err(FlatWireError::InvalidWriteTarget {
                name: name.clone(),
                reason: "it is absent from the Flat variable inventory",
            })
        }
    }

    fn validate_named_write_target_shape(
        &self,
        name: &VarName,
        expected_dimensions: &[i64],
    ) -> Result<(), FlatWireError> {
        self.validate_named_write_target(name)?;
        let actual_dimensions = self
            .model
            .variables
            .get(name)
            .map(|variable| &variable.dims)
            .ok_or_else(|| FlatWireError::InvalidWriteTarget {
                name: name.clone(),
                reason: "a checked named target is absent from the model inventory",
            })?;
        if actual_dimensions == expected_dimensions {
            return Ok(());
        }
        Err(FlatWireError::InvalidWriteTarget {
            name: name.clone(),
            reason: "its exact dimensions contradict the corresponding function output slot",
        })
    }

    fn validate_function_component_path(
        &self,
        component_ref: &ComponentReference,
    ) -> Result<(), &'static str> {
        self.function_component_dimensions(component_ref).map(drop)
    }

    fn function_component_dimensions(
        &self,
        component_ref: &ComponentReference,
    ) -> Result<Vec<i64>, &'static str> {
        self.function_component_shape(component_ref)
            .map(|shape| shape.dimensions)
    }

    fn function_component_shape(
        &self,
        component_ref: &ComponentReference,
    ) -> Result<WireExpressionShape, &'static str> {
        let (root_part, suffix) = component_ref
            .parts()
            .split_first()
            .ok_or("a function-local path has no exact root")?;
        let root = self
            .function_roots
            .get(&root_part.def_id)
            .filter(|root| root.name == root_part.ident)
            .ok_or("its local root is absent from the enclosing function interface")?;
        let mut dimensions = remaining_dimensions(&root_part.subs, &root.dimensions)?;
        if suffix.is_empty() {
            let record = self.function_record_root_identity(root)?;
            return Ok(WireExpressionShape { dimensions, record });
        }
        if suffix.len() != 1 {
            return Err(
                "a nested function-local projection lacks transitive record-field type evidence",
            );
        }
        let record_type_id = root
            .record_type
            .ok_or("a function-local projection lacks exact record-type identity")?;
        let record_type = self
            .model
            .record_types
            .get(&record_type_id)
            .ok_or("a function-local projection's record type is absent from Flat metadata")?;
        let field = suffix
            .first()
            .ok_or("a function-local projection has no exact field")?;
        let exact_field = record_type
            .fields
            .iter()
            .find(|candidate| candidate.def_id == field.def_id && candidate.name == field.ident)
            .ok_or("its projected field is absent from the exact Flat record type")?;
        dimensions.extend(remaining_dimensions(&field.subs, &exact_field.dims)?);
        let record = record_field_value_identity(self.model, exact_field)
            .map_err(|_| "a function record field contradicts its exact declared type identity")?;
        Ok(WireExpressionShape { dimensions, record })
    }

    fn function_record_root_identity(
        &self,
        root: &WireFunctionRoot,
    ) -> Result<Option<WireRecordValueIdentity>, &'static str> {
        let Some(declaration) = root.record_type else {
            return Ok(None);
        };
        let identity = record_declaration_identity(self.model, declaration)
            .map_err(|_| "a function record root contradicts its exact declaration identity")?;
        if identity.nominal != root.effective_type.nominal_type()
            || identity.canonical != root.effective_type.canonical_type()
        {
            return Err("a function record root contradicts its exact effective identity");
        }
        Ok(Some(identity))
    }

    fn validate_read_subscripts(
        &self,
        reference: &Reference,
        subscripts: &[Subscript],
    ) -> Result<(), FlatWireError> {
        let dimensions = if reference.structured_binder().is_some() {
            Some(Vec::new())
        } else {
            match reference.instance_id() {
                Some(instance_id) => {
                    self.targets
                        .by_occurrence
                        .get(&instance_id)
                        .and_then(|target| match target.kind {
                            WireTargetKind::Variable => self
                                .model
                                .variables
                                .get(target.name)
                                .map(|variable| variable.dims.clone()),
                            WireTargetKind::Record => self
                                .model
                                .record_instances
                                .get(target.name)
                                .map(|record| record.dims.clone()),
                        })
                }
                None if self.in_function_scope => self.function_reference_dimensions(reference),
                None => None,
            }
        };
        if let Some(dimensions) = dimensions {
            validate_subscript_shape(subscripts, &dimensions).map_err(|reason| {
                FlatWireError::InvalidReferenceTarget {
                    name: reference.var_name().clone(),
                    instance_id: reference.instance_id().unwrap_or(InstanceId::UNSET),
                    reason,
                }
            })?;
        }
        Ok(())
    }

    fn validate_index_expression(
        &self,
        base: &Expression,
        subscripts: &[Subscript],
    ) -> Result<(), FlatWireError> {
        let shape = self.infer_expression_shape(base)?;
        validate_subscript_shape(subscripts, &shape.dimensions)
            .map_err(|reason| FlatWireError::InvalidSubscript { reason })
    }

    fn validate_call_argument_shapes(
        &self,
        target: &WireFunctionTarget<'_>,
        arguments: &[Expression],
        resolved: rumoca_core::ResolvedFunctionReference,
        call_kind: FunctionCallKind,
    ) -> Result<Vec<i64>, FlatWireError> {
        let bindings = bind_call_arguments(target, arguments, call_kind)?;
        let mut vector_prefix: Option<Vec<i64>> = None;
        for (slot, value) in bindings {
            let actual = self.infer_expression_shape(value)?;
            let formal_param = target.function.inputs.get(slot).ok_or_else(|| {
                FlatWireError::InvalidFunctionCall {
                    function: target.name.clone(),
                    reason: "a bound argument slot is outside the exact input interface",
                }
            })?;
            let formal = formal_param.dimensions();
            if actual.dimensions.len() < formal.len()
                || actual.dimensions[actual.dimensions.len() - formal.len()..] != *formal
                || !same_record_argument_identity(&actual, formal_param)
            {
                return Err(FlatWireError::InvalidFunctionCall {
                    function: target.name.clone(),
                    reason: "an argument shape or exact record identity contradicts its input slot",
                });
            }
            let prefix = &actual.dimensions[..actual.dimensions.len() - formal.len()];
            if prefix.is_empty() {
                continue;
            }
            if vector_prefix
                .as_ref()
                .is_some_and(|expected| expected != prefix)
            {
                return Err(FlatWireError::InvalidFunctionCall {
                    function: target.name.clone(),
                    reason: "automatic-vectorization prefixes disagree across arguments",
                });
            }
            vector_prefix.get_or_insert_with(|| prefix.to_vec());
        }
        if vector_prefix.is_some() {
            if call_kind != FunctionCallKind::Invocation {
                return Err(FlatWireError::InvalidFunctionCall {
                    function: target.name.clone(),
                    reason: "automatic vectorization requires an ordinary function invocation",
                });
            }
            require_automatic_vectorization_authority(target.function, resolved).map_err(
                |reason| FlatWireError::InvalidFunctionCall {
                    function: target.name.clone(),
                    reason,
                },
            )?;
        }
        Ok(vector_prefix.unwrap_or_default())
    }

    fn visit_call_argument(&mut self, argument: &Expression) -> Result<(), FlatWireError> {
        let marker = match argument {
            Expression::FunctionCall {
                name,
                args,
                is_constructor,
                call_kind,
                ..
            } => rumoca_core::classify_named_function_arg_marker(
                name,
                args,
                *is_constructor,
                *call_kind,
            ),
            _ => rumoca_core::NamedFunctionArgMarker::NotMarker,
        };
        if let rumoca_core::NamedFunctionArgMarker::Valid { value, .. } = marker {
            require_span(raw_expression_span(argument), "named function argument")?;
            return self.visit_expression(value);
        }
        self.visit_expression(argument)
    }

    pub(super) fn validate_connected_state(&self) -> Result<(), FlatWireError> {
        // The standalone wire has no canonical Instance connection source
        // groups. Therefore it cannot reconstruct positive connected-state
        // evidence. Accepting a caller-authored domain would create a second,
        // unverifiable authority beside the build-local connection transaction.
        for (name, variable) in &self.model.variables {
            if !variable.connected.is_unconnected() {
                return Err(FlatWireError::ContradictoryConnectedState {
                    variable: name.clone(),
                    claimed: true,
                    equation_evidence: false,
                });
            }
        }
        Ok(())
    }
}

fn require_automatic_vectorization_authority(
    function: &Function,
    occurrence: rumoca_core::ResolvedFunctionReference,
) -> Result<(), &'static str> {
    function
        .automatic_vectorization_authority(occurrence)
        .map(drop)
        .map_err(|_| {
            "automatic vectorization lacks exact transitive non-replaceability and function-instance proof"
        })
}

#[cfg(test)]
mod automatic_vectorization_authority_tests {
    use super::*;

    #[test]
    fn wire_vectorization_rejects_a_distinct_non_replaceable_instance() {
        let function_instance = FunctionInstanceId::new(81_002);
        let mut function = Function::new("same", DefId::new(81_000), Span::DUMMY);
        function.instance_id = Some(function_instance);
        function.transitively_non_replaceable = true;

        let occurrence = rumoca_core::ResolvedFunctionReference {
            instance_id: FunctionInstanceId::new(81_001),
            base_part_count: 1,
            transitively_non_replaceable: true,
        };
        assert!(require_automatic_vectorization_authority(&function, occurrence).is_err());

        let exact_occurrence = rumoca_core::ResolvedFunctionReference {
            instance_id: function_instance,
            ..occurrence
        };
        assert!(require_automatic_vectorization_authority(&function, exact_occurrence).is_ok());
    }
}

fn expression_shape_error(reason: &'static str) -> FlatWireError {
    FlatWireError::InvalidFunctionCall {
        function: VarName::new("<expression-shape>"),
        reason,
    }
}

fn plain_shape(dimensions: Vec<i64>) -> WireExpressionShape {
    WireExpressionShape {
        dimensions,
        record: None,
    }
}

fn require_non_record_shape(
    shape: WireExpressionShape,
    reason: &'static str,
) -> Result<WireExpressionShape, FlatWireError> {
    if shape.record.is_none() {
        Ok(shape)
    } else {
        Err(expression_shape_error(reason))
    }
}

fn require_plain_shape(shape: WireExpressionShape) -> Result<WireExpressionShape, FlatWireError> {
    require_non_record_shape(shape, "a builtin operation cannot consume a record value")
}

fn broadcast_shapes(
    left: WireExpressionShape,
    right: WireExpressionShape,
) -> Result<WireExpressionShape, FlatWireError> {
    let left = require_plain_shape(left)?;
    let right = require_plain_shape(right)?;
    match (left.dimensions.is_empty(), right.dimensions.is_empty()) {
        (_, _) if left.dimensions == right.dimensions => Ok(left),
        (true, false) => Ok(right),
        (false, true) => Ok(left),
        _ => Err(expression_shape_error(
            "operands do not have one exact broadcast-compatible shape",
        )),
    }
}

fn require_scalar_shape(shape: WireExpressionShape) -> Result<WireExpressionShape, FlatWireError> {
    let shape = require_plain_shape(shape)?;
    if shape.dimensions.is_empty() {
        Ok(shape)
    } else {
        Err(expression_shape_error(
            "an operand requires an exact scalar shape",
        ))
    }
}

fn require_equal_plain_shapes(
    left: WireExpressionShape,
    right: WireExpressionShape,
) -> Result<WireExpressionShape, FlatWireError> {
    let left = require_plain_shape(left)?;
    let right = require_plain_shape(right)?;
    if left.dimensions == right.dimensions {
        Ok(left)
    } else {
        Err(expression_shape_error(
            "operands do not have one exact equal shape",
        ))
    }
}

fn require_scalar_rhs_shape(
    left: WireExpressionShape,
    right: WireExpressionShape,
) -> Result<WireExpressionShape, FlatWireError> {
    let left = require_plain_shape(left)?;
    require_scalar_shape(right)?;
    Ok(left)
}

fn multiply_shapes(
    left: WireExpressionShape,
    right: WireExpressionShape,
) -> Result<WireExpressionShape, FlatWireError> {
    let left = require_plain_shape(left)?;
    let right = require_plain_shape(right)?;
    if left.dimensions.is_empty() {
        return Ok(right);
    }
    if right.dimensions.is_empty() {
        return Ok(left);
    }
    let result = match (left.dimensions.as_slice(), right.dimensions.as_slice()) {
        ([left], [right]) if left == right => Vec::new(),
        ([rows, inner], [right]) if inner == right => vec![*rows],
        ([left], [inner, columns]) if left == inner => vec![*columns],
        ([rows, inner], [right, columns]) if inner == right => vec![*rows, *columns],
        _ => {
            return Err(expression_shape_error(
                "matrix-product operands have incompatible exact dimensions",
            ));
        }
    };
    Ok(plain_shape(result))
}

fn literal_integer(expression: &Expression) -> Result<i64, FlatWireError> {
    match expression {
        Expression::Literal {
            value: Literal::Integer(value),
            ..
        } => Ok(*value),
        _ => Err(expression_shape_error(
            "shape construction requires an exact integer literal",
        )),
    }
}

fn nonnegative_literal_dimension(expression: &Expression) -> Result<i64, FlatWireError> {
    let value = literal_integer(expression)?;
    if value >= 0 {
        Ok(value)
    } else {
        Err(expression_shape_error(
            "an array dimension cannot be negative",
        ))
    }
}

fn positive_literal_dimension(expression: &Expression) -> Result<i64, FlatWireError> {
    let value = literal_integer(expression)?;
    if value > 0 {
        Ok(value)
    } else {
        Err(expression_shape_error(
            "a one-based axis or extent must be positive",
        ))
    }
}

fn dimension_argument_shape(args: &[Expression]) -> Result<WireExpressionShape, FlatWireError> {
    if args.is_empty() {
        return Err(expression_shape_error(
            "an array constructor requires at least one dimension",
        ));
    }
    args.iter()
        .map(nonnegative_literal_dimension)
        .collect::<Result<Vec<_>, _>>()
        .map(plain_shape)
}

fn infer_range_shape(
    start: &Expression,
    step: Option<&Expression>,
    end: &Expression,
) -> Result<WireExpressionShape, FlatWireError> {
    let start = literal_integer(start)?;
    let end = literal_integer(end)?;
    let step = step.map(literal_integer).transpose()?.unwrap_or(1);
    if step == 0 {
        return Err(expression_shape_error("range step cannot be zero"));
    }
    let distance = if step > 0 && end >= start {
        end.checked_sub(start)
    } else if step < 0 && start >= end {
        start.checked_sub(end)
    } else {
        return Ok(plain_shape(vec![0]));
    }
    .ok_or_else(|| expression_shape_error("range distance overflows i64"))?;
    let stride = step
        .checked_abs()
        .ok_or_else(|| expression_shape_error("range stride overflows i64"))?;
    let extent = distance
        .checked_div(stride)
        .and_then(|value| value.checked_add(1))
        .ok_or_else(|| expression_shape_error("range cardinality overflows i64"))?;
    Ok(plain_shape(vec![extent]))
}

fn require_scalar_expression(
    checker: &WireSemanticChecker<'_>,
    expression: &Expression,
) -> Result<(), FlatWireError> {
    let shape = require_plain_shape(checker.infer_expression_shape(expression)?)?;
    if shape.dimensions.is_empty() {
        Ok(())
    } else {
        Err(expression_shape_error(
            "a builtin shape argument must be an exact scalar",
        ))
    }
}

fn checked_dimension_product(dimensions: &[i64]) -> Result<i64, FlatWireError> {
    dimensions.iter().try_fold(1_i64, |count, dimension| {
        if *dimension < 0 {
            return Err(expression_shape_error(
                "an exact dimension cannot be negative",
            ));
        }
        count
            .checked_mul(*dimension)
            .ok_or_else(|| expression_shape_error("array cardinality overflows i64"))
    })
}

fn infer_matrix_builtin_shape(
    shape: WireExpressionShape,
) -> Result<WireExpressionShape, FlatWireError> {
    let shape = require_plain_shape(shape)?;
    match shape.dimensions.as_slice() {
        [] => Ok(plain_shape(vec![1, 1])),
        [extent] => Ok(plain_shape(vec![*extent, 1])),
        [_, _] => Ok(shape),
        _ => Err(expression_shape_error(
            "matrix requires a scalar, vector, or rank-two array",
        )),
    }
}

fn require_square_matrix(shape: WireExpressionShape) -> Result<WireExpressionShape, FlatWireError> {
    let shape = require_plain_shape(shape)?;
    match shape.dimensions.as_slice() {
        [rows, columns] if rows == columns => Ok(shape),
        _ => Err(expression_shape_error(
            "the builtin requires an exact square matrix",
        )),
    }
}

fn require_vector(shape: WireExpressionShape) -> Result<i64, FlatWireError> {
    let shape = require_plain_shape(shape)?;
    match shape.dimensions.as_slice() {
        [extent] => Ok(*extent),
        _ => Err(expression_shape_error(
            "the builtin requires an exact vector",
        )),
    }
}

fn require_vector_extent(shape: WireExpressionShape, expected: i64) -> Result<(), FlatWireError> {
    if require_vector(shape)? == expected {
        Ok(())
    } else {
        Err(expression_shape_error(
            "the builtin vector has the wrong exact extent",
        ))
    }
}

pub(super) fn validate_component_reference_provenance(
    component_ref: &ComponentReference,
) -> Result<(), FlatWireError> {
    require_span(component_ref.span(), "component reference")?;
    for part in component_ref.parts() {
        require_span(part.span, "component-reference part")?;
        for subscript in &part.subs {
            require_span(subscript.span(), "component-reference subscript")?;
        }
    }
    Ok(())
}

pub(super) fn component_reference_value_subscripts(
    target: &ComponentReference,
    canonical: &ComponentReference,
) -> Option<Vec<Subscript>> {
    if target.local() != canonical.local() || target.parts().len() != canonical.parts().len() {
        return None;
    }
    let mut value_subscripts = Vec::new();
    for (target_part, canonical_part) in target.parts().iter().zip(canonical.parts()) {
        if target_part.ident != canonical_part.ident
            || target_part.def_id != canonical_part.def_id
            || target_part.subs.len() < canonical_part.subs.len()
            || !target_part
                .subs
                .iter()
                .zip(&canonical_part.subs)
                .all(|(target, canonical)| {
                    matches!((target, canonical),
                        (Subscript::Index { value: target, .. }, Subscript::Index { value: canonical, .. })
                            if target == canonical
                    )
                })
        {
            return None;
        }
        value_subscripts.extend_from_slice(&target_part.subs[canonical_part.subs.len()..]);
    }
    Some(value_subscripts)
}

fn matched_write_target_shape(
    target: &ComponentReference,
    canonical: &ComponentReference,
    mut shape: WireExpressionShape,
    name: &VarName,
) -> Result<Option<WireExpressionShape>, FlatWireError> {
    let Some(value_subscripts) = component_reference_value_subscripts(target, canonical) else {
        return Ok(None);
    };
    shape.dimensions =
        remaining_dimensions(&value_subscripts, &shape.dimensions).map_err(|reason| {
            FlatWireError::InvalidWriteTarget {
                name: name.clone(),
                reason,
            }
        })?;
    Ok(Some(shape))
}

fn insert_write_target_match(
    matched: &mut Option<WireExpressionShape>,
    candidate: Option<WireExpressionShape>,
    name: &VarName,
) -> Result<(), FlatWireError> {
    let Some(candidate) = candidate else {
        return Ok(());
    };
    if matched.replace(candidate).is_some() {
        return Err(FlatWireError::InvalidWriteTarget {
            name: name.clone(),
            reason: "it ambiguously extends two exact Flat targets",
        });
    }
    Ok(())
}

pub(super) fn validate_subscript_shape(
    subscripts: &[Subscript],
    dimensions: &[i64],
) -> Result<(), &'static str> {
    if subscripts.len() > dimensions.len() {
        return Err("its subscript rank exceeds the exact target rank");
    }
    for (subscript, extent) in subscripts.iter().zip(dimensions) {
        if let Subscript::Index { value, .. } = subscript
            && (*value < 1 || *value > *extent)
        {
            return Err("a literal subscript is outside the exact one-based target bounds");
        }
    }
    Ok(())
}

pub(super) fn remaining_dimensions(
    subscripts: &[Subscript],
    dimensions: &[i64],
) -> Result<Vec<i64>, &'static str> {
    validate_subscript_shape(subscripts, dimensions)?;
    let mut remaining = Vec::with_capacity(dimensions.len());
    for (subscript, extent) in subscripts.iter().zip(dimensions) {
        if matches!(subscript, Subscript::Colon { .. }) {
            remaining.push(*extent);
        }
    }
    remaining.extend_from_slice(&dimensions[subscripts.len()..]);
    Ok(remaining)
}

pub(super) fn validate_named_equation_owner(
    model: &Model,
    expression: &Expression,
    rendered_target: &str,
    kind: ConnectionExpressionKind,
    owner_kind: &'static str,
) -> Result<(), FlatWireError> {
    if kind != ConnectionExpressionKind::Residual {
        return Err(FlatWireError::InvalidEquationOrigin {
            reason: "a target-owning residual origin cannot own an assertion condition",
        });
    }
    let target = exact_variable_name(model, rendered_target).ok_or(
        FlatWireError::InvalidEquationOrigin {
            reason: "a target-owning origin is absent from the exact Flat variable catalog",
        },
    )?;
    let mut references = crate::VarRefCollector::new();
    rumoca_core::ExpressionVisitor::visit_expression(&mut references, expression);
    if !references.vars().contains(&target) {
        let reason = match owner_kind {
            "binding" => "a binding origin does not occur in its claimed residual",
            "reinit" => "a reinit origin does not occur in its claimed residual",
            "when assignment" => "a when-assignment origin does not occur in its claimed residual",
            _ => "a target-owning origin does not occur in its claimed residual",
        };
        return Err(FlatWireError::InvalidEquationOrigin { reason });
    }
    Ok(())
}

pub(super) fn exact_variable_name(model: &Model, rendered: &str) -> Option<VarName> {
    let name = VarName::new(rendered);
    model
        .variables
        .contains_key(&name)
        .then_some(name)
        .filter(|name| name.as_str() == rendered)
}

impl FallibleExpressionVisitor for WireSemanticChecker<'_> {
    type Error = FlatWireError;

    fn visit_expression(&mut self, expression: &Expression) -> Result<(), Self::Error> {
        require_span(raw_expression_span(expression), "expression occurrence")?;
        if matches!(expression, Expression::Empty { .. }) {
            return Err(FlatWireError::RecoveryNode {
                context: "expression",
            });
        }
        if let Expression::VarRef {
            name, subscripts, ..
        } = expression
        {
            self.validate_value_reference(name)?;
            self.validate_read_subscripts(name, subscripts)?;
        }
        if let Expression::FunctionCall {
            name,
            args,
            is_constructor,
            call_kind,
            ..
        } = expression
        {
            if let Some(component_ref) = name.component_ref() {
                validate_component_reference_provenance(component_ref)?;
            }
            if *is_constructor && *call_kind == FunctionCallKind::PartialApplication {
                return Err(FlatWireError::InvalidFunctionCall {
                    function: name.var_name().clone(),
                    reason: "a record constructor cannot be a function partial application",
                });
            }
            let marker = rumoca_core::classify_named_function_arg_marker(
                name,
                args,
                *is_constructor,
                *call_kind,
            );
            match marker {
                rumoca_core::NamedFunctionArgMarker::Malformed => {
                    return Err(FlatWireError::InvalidFunctionCall {
                        function: name.var_name().clone(),
                        reason: "the generated named-argument marker is contradictory",
                    });
                }
                rumoca_core::NamedFunctionArgMarker::Valid { .. } => {
                    return Err(FlatWireError::InvalidFunctionCall {
                        function: name.var_name().clone(),
                        reason: "a named-argument marker must be a direct slot of an exact call",
                    });
                }
                rumoca_core::NamedFunctionArgMarker::NotMarker => {}
            }
            let resolved =
                name.resolved_function()
                    .ok_or_else(|| FlatWireError::InvalidFunctionCall {
                        function: name.var_name().clone(),
                        reason: "a semantic function call requires an exact function identity",
                    })?;
            let target = validate_resolved_call_target(
                self.targets,
                name,
                resolved.instance_id,
                *is_constructor,
            )?;
            self.validate_call_argument_shapes(target, args, resolved, *call_kind)?;
            if *is_constructor {
                validate_record_constructor_layout(self.model, target.function)?;
            } else if target.function.outputs.is_empty() {
                return Err(FlatWireError::InvalidFunctionCall {
                    function: name.var_name().clone(),
                    reason: "a value expression cannot invoke an ordinary function with no output slot",
                });
            }
            for argument in args {
                self.visit_call_argument(argument)?;
            }
            return Ok(());
        }
        if let Expression::Index {
            base, subscripts, ..
        } = expression
        {
            self.validate_index_expression(base, subscripts)?;
        }
        self.walk_expression(expression)
    }

    fn visit_subscript(&mut self, subscript: &Subscript) -> Result<(), Self::Error> {
        require_span(subscript.span(), "subscript occurrence")?;
        if let Subscript::Expr { expr, .. } = subscript {
            let shape = self.infer_expression_shape(expr)?;
            if !shape.dimensions.is_empty() || shape.record.is_some() {
                return Err(FlatWireError::InvalidSubscript {
                    reason: "a dynamic index requires exact scalar shape evidence",
                });
            }
            self.visit_expression(expr)?;
        }
        Ok(())
    }
}

pub(super) fn validate_resolved_call_target<'targets, 'model>(
    targets: &'targets WireTargetIndex<'model>,
    reference: &Reference,
    instance_id: FunctionInstanceId,
    is_constructor: bool,
) -> Result<&'targets WireFunctionTarget<'model>, FlatWireError> {
    let target = targets
        .by_function_instance
        .get(&instance_id)
        .ok_or_else(|| FlatWireError::InvalidFunctionCall {
            function: reference.var_name().clone(),
            reason: "the resolved function instance is absent",
        })?;
    if target.name != reference.var_name() {
        return Err(FlatWireError::InvalidFunctionCall {
            function: reference.var_name().clone(),
            reason: "the cached function name contradicts its exact function instance",
        });
    }
    if target.function.is_constructor != is_constructor {
        return Err(FlatWireError::InvalidFunctionCall {
            function: reference.var_name().clone(),
            reason: "the occurrence call kind contradicts its resolved function",
        });
    }
    validate_callable_reference_provenance(reference, target)?;
    Ok(target)
}

pub(super) fn validate_callable_reference_provenance(
    reference: &Reference,
    target: &WireFunctionTarget<'_>,
) -> Result<(), FlatWireError> {
    let resolved =
        reference
            .resolved_function()
            .ok_or_else(|| FlatWireError::InvalidFunctionCall {
                function: reference.var_name().clone(),
                reason: "call target validation requires exact resolved identity",
            })?;
    let Some(component_ref) = reference.component_ref() else {
        if resolved.base_part_count == 0 {
            return Ok(());
        }
        return Err(FlatWireError::InvalidFunctionCall {
            function: reference.var_name().clone(),
            reason: "a callable base-path boundary requires exact component-reference provenance",
        });
    };
    if resolved.base_part_count != component_ref.parts().len() {
        return Err(FlatWireError::InvalidFunctionCall {
            function: reference.var_name().clone(),
            reason: "the callable base-path boundary contradicts its exact component-reference path",
        });
    }
    if component_ref.target_def_id() != target.function.exposure_def_id {
        return Err(FlatWireError::InvalidFunctionCall {
            function: reference.var_name().clone(),
            reason: "the callable component path contradicts its exact exposure declaration",
        });
    }
    if resolved.transitively_non_replaceable && !target.function.transitively_non_replaceable {
        return Err(FlatWireError::InvalidFunctionCall {
            function: reference.var_name().clone(),
            reason: "the call occurrence forges transitive non-replaceability",
        });
    }
    Ok(())
}

pub(super) fn bind_call_arguments<'expression>(
    target: &WireFunctionTarget<'_>,
    arguments: &'expression [Expression],
    call_kind: FunctionCallKind,
) -> Result<Vec<(usize, &'expression Expression)>, FlatWireError> {
    let mut filled = vec![false; target.function.inputs.len()];
    let mut bindings = Vec::with_capacity(arguments.len());
    let mut next_positional = 0usize;
    let mut saw_named = false;
    for argument in arguments {
        let marker = match argument {
            Expression::FunctionCall {
                name,
                args,
                is_constructor,
                call_kind,
                ..
            } => rumoca_core::classify_named_function_arg_marker(
                name,
                args,
                *is_constructor,
                *call_kind,
            ),
            _ => rumoca_core::NamedFunctionArgMarker::NotMarker,
        };
        let (slot, value) = match marker {
            rumoca_core::NamedFunctionArgMarker::Valid { name, value } => {
                saw_named = true;
                (
                    target
                        .function
                        .inputs
                        .iter()
                        .position(|input| input.name == name)
                        .ok_or_else(|| FlatWireError::InvalidFunctionCall {
                            function: target.name.clone(),
                            reason: "a named argument does not identify an exact input slot",
                        })?,
                    value,
                )
            }
            rumoca_core::NamedFunctionArgMarker::Malformed => {
                return Err(FlatWireError::InvalidFunctionCall {
                    function: target.name.clone(),
                    reason: "the generated named-argument marker is contradictory",
                });
            }
            rumoca_core::NamedFunctionArgMarker::NotMarker => {
                if saw_named {
                    return Err(FlatWireError::InvalidFunctionCall {
                        function: target.name.clone(),
                        reason: "a positional argument follows a named argument",
                    });
                }
                while filled.get(next_positional).copied() == Some(true) {
                    next_positional += 1;
                }
                let slot = next_positional;
                next_positional += 1;
                (slot, argument)
            }
        };
        let Some(filled_slot) = filled.get_mut(slot) else {
            return Err(FlatWireError::InvalidFunctionCall {
                function: target.name.clone(),
                reason: "the call supplies more arguments than its exact input interface",
            });
        };
        if std::mem::replace(filled_slot, true) {
            return Err(FlatWireError::InvalidFunctionCall {
                function: target.name.clone(),
                reason: "two arguments claim one exact function input slot",
            });
        }
        bindings.push((slot, value));
    }
    if call_kind == FunctionCallKind::Invocation
        && target
            .function
            .inputs
            .iter()
            .zip(filled)
            .any(|(input, filled)| !filled && input.default.is_none())
    {
        return Err(FlatWireError::InvalidFunctionCall {
            function: target.name.clone(),
            reason: "the invocation leaves a required input slot unfilled",
        });
    }
    Ok(bindings)
}

impl FallibleStatementVisitor for WireSemanticChecker<'_> {
    fn visit_statement(&mut self, statement: &Statement) -> Result<(), Self::Error> {
        if matches!(statement, Statement::Empty { .. }) {
            return Err(FlatWireError::RecoveryNode {
                context: "algorithm statement",
            });
        }
        let span = statement
            .source_span()
            .ok_or(FlatWireError::MissingProvenance {
                context: "algorithm statement",
            })?;
        require_span(span, "algorithm statement")?;
        self.walk_statement(statement)
    }

    fn visit_component_reference(
        &mut self,
        component_ref: &ComponentReference,
    ) -> Result<(), Self::Error> {
        self.validate_write_target(component_ref)?;
        for part in component_ref.parts() {
            for subscript in &part.subs {
                self.visit_subscript(subscript)?;
            }
        }
        Ok(())
    }

    fn visit_statement_function_call(
        &mut self,
        callable: &Reference,
        args: &[Expression],
        outputs: &[Option<ComponentReference>],
    ) -> Result<(), Self::Error> {
        if let Some(component_ref) = callable.component_ref() {
            validate_component_reference_provenance(component_ref)?;
        }
        let resolved =
            callable
                .resolved_function()
                .ok_or_else(|| FlatWireError::InvalidFunctionCall {
                    function: callable.var_name().clone(),
                    reason: "a statement function call requires an exact function identity",
                })?;
        let target =
            validate_resolved_call_target(self.targets, callable, resolved.instance_id, false)?;
        let vector_prefix = self.validate_call_argument_shapes(
            target,
            args,
            resolved,
            FunctionCallKind::Invocation,
        )?;
        if outputs.len() > target.function.outputs.len() {
            return Err(FlatWireError::InvalidFunctionCall {
                function: callable.var_name().clone(),
                reason: "the statement claims more output slots than the exact function interface",
            });
        }
        for argument in args {
            self.visit_call_argument(argument)?;
        }
        for (slot, output) in outputs.iter().enumerate() {
            let Some(output) = output else {
                continue;
            };
            let actual = self.write_target_shape(output)?;
            let mut expected_dimensions = vector_prefix.clone();
            let expected_slot = &target.function.outputs[slot];
            expected_dimensions.extend_from_slice(expected_slot.dimensions());
            let expected = WireExpressionShape {
                dimensions: expected_dimensions,
                record: function_param_record_identity(self.model, expected_slot)?,
            };
            if actual != expected {
                return Err(FlatWireError::InvalidFunctionCall {
                    function: callable.var_name().clone(),
                    reason: "a statement output target contradicts its exact output-slot shape or record identity",
                });
            }
            self.visit_output_subscripts(output)?;
        }
        Ok(())
    }
}

impl WireSemanticChecker<'_> {
    fn function_reference_dimensions(&self, reference: &Reference) -> Option<Vec<i64>> {
        let component_ref = reference.component_ref()?;
        self.function_component_dimensions(component_ref).ok()
    }

    fn visit_output_subscripts(
        &mut self,
        output: &ComponentReference,
    ) -> Result<(), FlatWireError> {
        for part in output.parts() {
            for subscript in &part.subs {
                self.visit_subscript(subscript)?;
            }
        }
        Ok(())
    }
}

mod when_validation;
pub(super) use when_validation::{raw_expression_span, validate_when_equation};
