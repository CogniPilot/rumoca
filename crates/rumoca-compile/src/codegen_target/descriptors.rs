use super::*;

pub fn builtin_target_descriptors() -> Result<Vec<BuiltinTargetDescriptor>> {
    templates::builtin_targets()
        .iter()
        .map(|target| {
            let bundle = TargetBundle::builtin(target.name)
                .with_context(|| format!("Resolve built-in target '{}'", target.name))?;
            let (_checked, facts) = bundle
                .check_with_descriptor_facts()
                .with_context(|| format!("Check built-in target '{}'", target.name))?;
            Ok(BuiltinTargetDescriptor {
                id: target.name.to_string(),
                label: facts.label,
                description: facts.description,
                required_product: facts.required_product,
                capabilities: facts.capabilities,
                file_plans: facts.file_plans,
            })
        })
        .collect()
}

pub fn builtin_target_descriptors_requiring(
    context: TargetSemanticContext,
) -> Result<Vec<BuiltinTargetDescriptor>> {
    builtin_target_descriptors().map(|descriptors| {
        descriptors
            .into_iter()
            .filter(|descriptor| {
                descriptor
                    .file_plans
                    .iter()
                    .any(|file| file.semantic_context == context)
            })
            .collect()
    })
}

pub fn builtin_target_compatibility_matrix() -> Result<Vec<TargetCompatibilityEntry>> {
    templates::builtin_targets()
        .iter()
        .map(|target| {
            let bundle = TargetBundle::builtin(target.name)
                .with_context(|| format!("Resolve built-in target '{}'", target.name))?;
            let (_checked, facts) = bundle
                .check_with_descriptor_facts()
                .with_context(|| format!("Check built-in target '{}'", target.name))?;
            Ok(target_compatibility_entry(target.name, &facts))
        })
        .collect()
}

pub(super) fn target_compatibility_entry(
    id: &str,
    facts: &TargetDescriptorFacts,
) -> TargetCompatibilityEntry {
    let capabilities = facts.capabilities.as_ref();
    let tensor = capabilities.and_then(|capabilities| capabilities.tensor.as_ref());
    let scalar_fallback = capabilities.is_some_and(|capabilities| capabilities.scalar_fallback);
    TargetCompatibilityEntry {
        id: id.to_string(),
        label: facts.label.clone(),
        required_product: facts.required_product,
        execution_mode: facts.execution_mode.clone(),
        deployment_class: facts.deployment_class.clone(),
        readiness_level: facts.readiness_level,
        scalar_programs: scalar_program_support(facts.required_product),
        matmul: tensor_feature_support(
            facts.required_product,
            scalar_fallback,
            tensor.and_then(|tensor| tensor.matmul),
        ),
        linsolve: tensor_feature_support(
            facts.required_product,
            scalar_fallback,
            tensor.and_then(|tensor| tensor.linsolve),
        ),
        elementwise: tensor_feature_support(
            facts.required_product,
            scalar_fallback,
            tensor.and_then(|tensor| tensor.elementwise),
        ),
        stencil: tensor_feature_support(
            facts.required_product,
            scalar_fallback,
            tensor.and_then(|tensor| tensor.stencil),
        ),
        reductions: tensor_feature_support(
            facts.required_product,
            scalar_fallback,
            tensor.and_then(|tensor| tensor.reductions),
        ),
        supports_dynamic_shapes: tensor.and_then(|tensor| tensor.supports_dynamic_shapes),
        sparse: feature_support(tensor.and_then(|tensor| tensor.sparse)),
        dtypes: tensor_dtypes(tensor),
        events: feature_support(capabilities.and_then(|capabilities| capabilities.events)),
        runtime_events: feature_support(
            capabilities.and_then(|capabilities| capabilities.runtime_events),
        ),
        forward_ad: feature_support(capabilities.and_then(|capabilities| capabilities.forward_ad)),
        reverse_ad: feature_support(capabilities.and_then(|capabilities| capabilities.reverse_ad)),
        dynamic_control_flow: feature_support(
            capabilities.and_then(|capabilities| capabilities.dynamic_control_flow),
        ),
        host_callbacks: feature_support(
            capabilities.and_then(|capabilities| capabilities.host_callbacks),
        ),
    }
}

pub(super) fn tensor_dtypes(tensor: Option<&TensorCapabilities>) -> Vec<String> {
    match tensor.and_then(|tensor| tensor.dtypes.as_ref()) {
        Some(dtypes) => dtypes.clone(),
        None => Vec::new(),
    }
}

pub(super) fn scalar_program_support(product: TargetRequiredProduct) -> TargetFeatureSupport {
    match product {
        TargetRequiredProduct::SolveModel
        | TargetRequiredProduct::FmiComponent
        | TargetRequiredProduct::SolveAlgorithmProduct => TargetFeatureSupport::Native,
        TargetRequiredProduct::Ast
        | TargetRequiredProduct::Flat
        | TargetRequiredProduct::Dae
        | TargetRequiredProduct::AlgorithmCodePackage => TargetFeatureSupport::Unsupported,
    }
}

pub(super) fn tensor_feature_support(
    product: TargetRequiredProduct,
    scalar_fallback: bool,
    capability: Option<TensorCapability>,
) -> TargetFeatureSupport {
    if !product.carries_solve_tensor_program() {
        return TargetFeatureSupport::Unsupported;
    }
    match capability {
        Some(TensorCapability::Native) => TargetFeatureSupport::Native,
        Some(TensorCapability::Scalar) if scalar_fallback => TargetFeatureSupport::Scalar,
        Some(TensorCapability::Scalar) => TargetFeatureSupport::Unsupported,
        Some(TensorCapability::Unsupported) => TargetFeatureSupport::Unsupported,
        None => TargetFeatureSupport::Unknown,
    }
}

pub(super) fn feature_support(value: Option<bool>) -> TargetFeatureSupport {
    match value {
        Some(true) => TargetFeatureSupport::Native,
        Some(false) => TargetFeatureSupport::Unsupported,
        None => TargetFeatureSupport::Unknown,
    }
}

pub(super) fn parse_target_manifest_construction(
    source: &str,
) -> Result<TargetManifestConstruction> {
    let draft = toml::from_str::<TargetManifestDraft>(source)
        .map_err(|error| anyhow::anyhow!("Parse target.toml: {error}"))?;
    TargetManifest::from_draft(draft)
}

pub(super) fn phase_artifact_kind(
    kind: TargetArtifactKind,
) -> rumoca_phase_codegen::TemplateArtifactKind {
    match kind {
        TargetArtifactKind::AlgorithmCode => {
            rumoca_phase_codegen::TemplateArtifactKind::AlgorithmCode
        }
        TargetArtifactKind::CHeader => rumoca_phase_codegen::TemplateArtifactKind::CHeader,
        TargetArtifactKind::CSource => rumoca_phase_codegen::TemplateArtifactKind::CSource,
        TargetArtifactKind::CudaSource => rumoca_phase_codegen::TemplateArtifactKind::CudaSource,
        TargetArtifactKind::Json => rumoca_phase_codegen::TemplateArtifactKind::Json,
        TargetArtifactKind::Markdown => rumoca_phase_codegen::TemplateArtifactKind::Markdown,
        TargetArtifactKind::MlirSource => rumoca_phase_codegen::TemplateArtifactKind::MlirSource,
        TargetArtifactKind::ModelicaSource => {
            rumoca_phase_codegen::TemplateArtifactKind::ModelicaSource
        }
        TargetArtifactKind::PythonSource => {
            rumoca_phase_codegen::TemplateArtifactKind::PythonSource
        }
        TargetArtifactKind::RustSource => rumoca_phase_codegen::TemplateArtifactKind::RustSource,
        TargetArtifactKind::Text => rumoca_phase_codegen::TemplateArtifactKind::Text,
        TargetArtifactKind::Toml => rumoca_phase_codegen::TemplateArtifactKind::Toml,
        TargetArtifactKind::WgslSource => rumoca_phase_codegen::TemplateArtifactKind::WgslSource,
        TargetArtifactKind::Xml => rumoca_phase_codegen::TemplateArtifactKind::Xml,
    }
}

pub(super) fn phase_semantic_context(
    context: TargetSemanticContext,
) -> rumoca_phase_codegen::TemplateSemanticContext {
    match context {
        TargetSemanticContext::Ast => rumoca_phase_codegen::TemplateSemanticContext::Ast,
        TargetSemanticContext::Flat => rumoca_phase_codegen::TemplateSemanticContext::Flat,
        TargetSemanticContext::Dae => rumoca_phase_codegen::TemplateSemanticContext::Dae,
        TargetSemanticContext::Galec => rumoca_phase_codegen::TemplateSemanticContext::Galec,
        TargetSemanticContext::Solve => rumoca_phase_codegen::TemplateSemanticContext::Solve,
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum CapabilityDiagnosticScope {
    Target,
    GalecProjection,
}

#[derive(Clone, Copy)]
pub(super) struct CapabilityTarget<'a> {
    label: &'a str,
    required_product: TargetRequiredProduct,
    diagnostic_scope: CapabilityDiagnosticScope,
}

impl<'a> CapabilityTarget<'a> {
    pub(super) fn for_dae_source(label: &'a str, required_product: TargetRequiredProduct) -> Self {
        let diagnostic_scope = if required_product.carries_algorithm_code_package() {
            CapabilityDiagnosticScope::GalecProjection
        } else {
            CapabilityDiagnosticScope::Target
        };
        Self {
            label,
            required_product,
            diagnostic_scope,
        }
    }

    pub(super) const fn for_solve_product(
        label: &'a str,
        required_product: TargetRequiredProduct,
    ) -> Self {
        Self {
            label,
            required_product,
            diagnostic_scope: CapabilityDiagnosticScope::Target,
        }
    }

    pub(super) const fn label(self) -> &'a str {
        self.label
    }

    pub(super) const fn owns_dae_structured_families(self) -> bool {
        matches!(self.required_product, TargetRequiredProduct::Dae)
    }

    pub(super) const fn names_galec_projection(self) -> bool {
        matches!(
            self.diagnostic_scope,
            CapabilityDiagnosticScope::GalecProjection
        )
    }
}

pub(super) fn validate_dae_render_capability_contract(
    dae: &dae::Dae,
    contract: &CheckedTargetCapabilityContract,
) -> Result<()> {
    validate_dae_capabilities(
        dae,
        CapabilityTarget::for_dae_source(&contract.label, contract.required_product),
        &contract.capabilities,
    )
}

pub(super) fn validate_dae_capabilities(
    dae: &dae::Dae,
    target: CapabilityTarget<'_>,
    capabilities: &TargetCapabilities,
) -> Result<()> {
    let (state_count, first_state_span, residual_owner_count, continuous_family_count) = dae
        .inspect(|view| {
            let (state_count, first_state_span) =
                view.variables()
                    .fold((0usize, None), |(count, first_span), (_, variable)| {
                        if variable.role() == dae::VariableRole::State {
                            (
                                count + 1,
                                first_span.or_else(|| Some(variable.declaration().span())),
                            )
                        } else {
                            (count, first_span)
                        }
                    });
            let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
            (
                state_count,
                first_state_span,
                definitions.remaining_owner_count(),
                view.continuous_family_count(),
            )
        });
    // DAE templates must explicitly consume compact families. Algorithm Code
    // targets own a stronger, target-specific projection proof and re-check
    // every family before lowering; the generic renderer must not reject that
    // canonical input before the projection can inspect it.
    if target.owns_dae_structured_families()
        && capabilities.structured_equation_families != Some(true)
        && continuous_family_count != 0
    {
        unsupported_feature(
            target,
            "structured_equation_families",
            format!("{continuous_family_count} compact equation family owner(s)"),
        )?;
    }
    if capabilities.continuous_states == Some(false) && state_count != 0 {
        unsupported_feature_at(
            target,
            "continuous_states",
            format!("{state_count} state(s)"),
            first_state_span,
        )?;
    }
    // A Solve/FMI target does not emit DAE owners directly. Its later checked
    // projection distinguishes explicit derivative rows from retained
    // algebraic residuals; rejecting every source equation here would make a
    // plain explicit ODE impossible to export.
    if target.owns_dae_structured_families()
        && capabilities.residual_equations == Some(false)
        && residual_owner_count != 0
    {
        unsupported_feature(
            target,
            "residual_equations",
            format!("{residual_owner_count} equation(s)"),
        )?;
    }
    if dae_has_external_functions(dae) {
        unsupported_feature(
            target,
            "external_functions",
            "external declarations are not executable before the Invoke/Effect grammar exists",
        )?;
    }
    if capabilities.random == Some(false) && dae_uses_random(dae) {
        unsupported_feature(target, "random", "random runtime calls present")?;
    }
    if capabilities.initialization == Some(false) && dae_has_initialization(dae) {
        unsupported_feature(target, "initialization", "initial equations present")?;
    }
    if capabilities.events != Some(true) && dae_has_events(dae) {
        unsupported_feature(target, "events", "event or condition partitions present")?;
    }
    if capabilities.runtime_events == Some(false) && dae_has_runtime_events(dae) {
        unsupported_feature(
            target,
            "runtime_events",
            "delay-history or terminal-event runtime support is required",
        )?;
    }
    if capabilities.clocks != Some(true) && dae_has_clocks(dae) {
        unsupported_feature(target, "clocks", "clock partition entries present")?;
    }
    if capabilities.dynamic_ranges == Some(false) && dae_has_dynamic_ranges(dae) {
        unsupported_feature(
            target,
            "dynamic_ranges",
            "non-literal range expressions present",
        )?;
    }
    if capabilities.dynamic_derivative_subscripts == Some(false)
        && dae_has_dynamic_derivative_subscripts(dae)
    {
        unsupported_feature(
            target,
            "dynamic_derivative_subscripts",
            "derivative references with dynamic subscripts present",
        )?;
    }
    Ok(())
}

pub(super) fn validate_solve_capability_contract(
    solve: &rumoca_ir_solve::SolveProblem,
    contract: &CheckedTargetCapabilityContract,
) -> Result<()> {
    validate_solve_capabilities(
        solve,
        CapabilityTarget::for_solve_product(&contract.label, contract.required_product),
        &contract.capabilities,
    )
}

pub(super) fn validate_fmi_capability_contract(
    admitted: &rumoca_phase_codegen::AdmittedFmiRenderingInput,
    contract: &CheckedTargetCapabilityContract,
) -> Result<()> {
    let target = CapabilityTarget::for_solve_product(&contract.label, contract.required_product);
    let capabilities = &contract.capabilities;
    if capabilities.residual_equations != Some(true)
        && admitted.has_exact_algebraic_system()
        && capabilities.exact_algebraic_assignments != Some(true)
    {
        unsupported_feature(
            target,
            "residual_equations",
            "checked algebraic refresh retains residual projection stages",
        )?;
    }
    validate_solve_tensor_inventory_for_target(
        target,
        capabilities,
        admitted.tensor_inventory(),
        false,
        [None; 4],
    )
}

pub(super) fn validate_solve_capabilities(
    solve: &rumoca_ir_solve::SolveProblem,
    target: CapabilityTarget<'_>,
    capabilities: &TargetCapabilities,
) -> Result<()> {
    if capabilities.residual_equations != Some(true)
        && solve_requires_residual_equations(solve, capabilities.exact_algebraic_assignments)
    {
        unsupported_feature(
            target,
            "residual_equations",
            "checked algebraic refresh retains residual projection stages",
        )?;
    }
    // The generic Solve presence queries below are owned by `rumoca_ir_solve`
    // (SPEC_0041 §1). FMI rendering instead consumes its affine admission
    // above, so it does not repeat these queries over the same root.
    if capabilities.initialization == Some(false)
        && rumoca_ir_solve::solve_has_initialization(solve)
    {
        unsupported_feature(
            target,
            "initialization",
            "initialization residual, projection, or assignment owners present",
        )?;
    }
    if capabilities.events != Some(true) && rumoca_ir_solve::solve_has_events(solve) {
        unsupported_feature(target, "events", "event or discrete partitions present")?;
    }
    if capabilities.runtime_events == Some(false)
        && rumoca_ir_solve::solve_has_runtime_events(solve)
    {
        unsupported_feature(
            target,
            "runtime_events",
            "delay-history or terminal-event runtime support is required",
        )?;
    }
    if capabilities.clocks != Some(true) && rumoca_ir_solve::solve_has_clocks(solve) {
        unsupported_feature(target, "clocks", "clock partition entries present")?;
    }
    let mut inventory = solve.compute_node_counts();
    inventory.add_assign(solve.initialization().residual().compute_node_counts());
    let uses_linear_solve_component = solve.uses_linear_solve_component()
        || solve
            .initialization()
            .residual()
            .uses_linear_solve_component();
    let feature_spans = solve_tensor_feature_spans(solve);
    validate_solve_tensor_inventory_for_target(
        target,
        capabilities,
        inventory,
        uses_linear_solve_component,
        feature_spans,
    )
}

fn solve_tensor_feature_spans(
    solve: &rumoca_ir_solve::SolveProblem,
) -> [Option<rumoca_core::Span>; 4] {
    let continuous = solve.continuous();
    let blocks = [
        continuous.implicit_rhs(),
        continuous.residual(),
        continuous.manifold_residual(),
        continuous.derivative_rhs(),
        solve.initialization().residual(),
    ];
    [
        first_tensor_node_span(&blocks, |node| match node {
            rumoca_ir_solve::ComputeNode::MatMul { span, .. } => Some(*span),
            _ => None,
        }),
        first_tensor_node_span(&blocks, |node| match node {
            rumoca_ir_solve::ComputeNode::LinSolve { span, .. } => Some(*span),
            rumoca_ir_solve::ComputeNode::ScalarPrograms(block) => scalar_linear_solve_span(block),
            rumoca_ir_solve::ComputeNode::Map { base_ops, span, .. }
            | rumoca_ir_solve::ComputeNode::AffineStencil { base_ops, span, .. }
                if base_ops.iter().any(|operation| {
                    matches!(
                        operation,
                        rumoca_ir_solve::LinearOp::LinearSolveComponent { .. }
                    )
                }) =>
            {
                Some(*span)
            }
            _ => None,
        }),
        first_tensor_node_span(&blocks, |node| match node {
            rumoca_ir_solve::ComputeNode::Map { span, .. } => Some(*span),
            _ => None,
        }),
        first_tensor_node_span(&blocks, |node| match node {
            rumoca_ir_solve::ComputeNode::AffineStencil { span, .. } => Some(*span),
            _ => None,
        }),
    ]
}

fn scalar_linear_solve_span(
    block: &rumoca_ir_solve::ScalarProgramBlock,
) -> Option<rumoca_core::Span> {
    block
        .programs()
        .iter()
        .position(|operations| {
            operations.iter().any(|operation| {
                matches!(
                    operation,
                    rumoca_ir_solve::LinearOp::LinearSolveComponent { .. }
                )
            })
        })
        .and_then(|program| block.program_span(program))
}

fn first_tensor_node_span(
    blocks: &[&rumoca_ir_solve::ComputeBlock],
    select: impl Fn(&rumoca_ir_solve::ComputeNode) -> Option<rumoca_core::Span>,
) -> Option<rumoca_core::Span> {
    blocks
        .iter()
        .find_map(|block| block.nodes.iter().find_map(&select))
}

pub(super) fn validate_solve_tensor_inventory_for_target(
    target: CapabilityTarget<'_>,
    capabilities: &TargetCapabilities,
    inventory: rumoca_ir_solve::ComputeNodeCounts,
    uses_linear_solve_component: bool,
    feature_spans: [Option<rumoca_core::Span>; 4],
) -> Result<()> {
    let scalar_fallback = capabilities.scalar_fallback;
    let tensor = capabilities.tensor.as_ref();

    validate_solve_tensor_feature(
        target,
        "tensor.matmul",
        "MatMul",
        inventory.matmul,
        tensor.and_then(|tensor| tensor.matmul),
        scalar_fallback,
        feature_spans[0],
    )?;
    validate_solve_tensor_feature(
        target,
        "tensor.linsolve",
        "LinSolve",
        inventory
            .linsolve
            .saturating_add(usize::from(uses_linear_solve_component)),
        tensor.and_then(|tensor| tensor.linsolve),
        scalar_fallback,
        feature_spans[1],
    )?;
    validate_solve_tensor_feature(
        target,
        "tensor.elementwise",
        "Map",
        inventory.map,
        tensor.and_then(|tensor| tensor.elementwise),
        scalar_fallback,
        feature_spans[2],
    )?;
    validate_solve_tensor_feature(
        target,
        "tensor.stencil",
        "AffineStencil",
        inventory.affine_stencil,
        tensor.and_then(|tensor| tensor.stencil),
        scalar_fallback,
        feature_spans[3],
    )
}

pub(super) fn validate_solve_tensor_feature(
    target: CapabilityTarget<'_>,
    feature: &'static str,
    display_name: &str,
    count: usize,
    capability: Option<TensorCapability>,
    scalar_fallback: bool,
    span: Option<rumoca_core::Span>,
) -> Result<()> {
    if count == 0 {
        return Ok(());
    }
    match capability {
        Some(TensorCapability::Native) => Ok(()),
        Some(TensorCapability::Scalar) if scalar_fallback => Ok(()),
        Some(TensorCapability::Scalar) => unsupported_tensor_feature(
            target,
            feature,
            format!(
                "{display_name} is configured for scalar fallback but scalar fallback is disabled"
            ),
            span,
        ),
        Some(TensorCapability::Unsupported) => unsupported_tensor_feature(
            target,
            feature,
            format!(
                "{display_name} nodes are present but the target declares {feature} unsupported"
            ),
            span,
        ),
        None if scalar_fallback => Ok(()),
        None => unsupported_tensor_feature(
            target,
            feature,
            format!(
                "{display_name} nodes are present but the target does not declare native \
                 {display_name} support and scalar fallback is disabled"
            ),
            span,
        ),
    }
}

pub(super) fn unsupported_tensor_feature(
    target: CapabilityTarget<'_>,
    feature: &'static str,
    detail: impl std::fmt::Display,
    span: Option<rumoca_core::Span>,
) -> Result<()> {
    unsupported_feature_at(target, feature, detail, span)
}
