use rumoca_compile::compile::{Session, SessionConfig, VariableRole};
use rumoca_sim::{DiffsolMethod, SimOptions, SimPacingMode, SimSolverMode, prepare_simulation};
use rumoca_solver::SimExecutionPolicy;

pub(super) const UNIT_DERIVATIVE_SOURCE: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/tests/fixtures/golden/UnitDerivative.mo"
));

fn compile_model(
    file_name: &str,
    source: &str,
    model_name: &str,
) -> rumoca_compile::compile::CompilationResult {
    let mut session = Session::new(SessionConfig {
        parallel: false,
        instantiation_depth_limit: rumoca_phase_instantiate::DEFAULT_INSTANTIATION_DEPTH_LIMIT,
    });
    session
        .add_document(file_name, source)
        .expect("source fixture parses");
    session
        .compile_model(model_name)
        .unwrap_or_else(|error| panic!("{model_name} reaches checked DAE: {error:?}"))
}

fn compile_unit_derivative_strict() -> rumoca_compile::compile::StrictCompilation {
    let mut session = Session::new(SessionConfig {
        parallel: false,
        instantiation_depth_limit: rumoca_phase_instantiate::DEFAULT_INSTANTIATION_DEPTH_LIMIT,
    });
    session
        .add_document("unit_derivative.mo", UNIT_DERIVATIVE_SOURCE)
        .expect("UnitDerivative source parses without recovery");
    session
        .compile_model_strict("UnitDerivative")
        .unwrap_or_else(|report| panic!("UnitDerivative strict compilation failed: {report:?}"))
}

fn unit_derivative_options() -> SimOptions {
    SimOptions {
        t_start: 0.0,
        t_end: 1.0,
        rtol: 1.0e-12,
        atol: 1.0e-12,
        dt: Some(0.1),
        scalarize: false,
        max_wall_seconds: None,
        solver_mode: SimSolverMode::RkLike,
        diffsol_method: DiffsolMethod::Bdf,
        pacing_mode: SimPacingMode::AsFastAsPossible,
        param_overrides: Vec::new(),
        start_overrides: Vec::new(),
        execution_policy: SimExecutionPolicy::Interpreter,
    }
}

const PARAMETER_DECAY_SOURCE: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/tests/fixtures/golden/ParameterDecay.mo"
));

fn compile_parameter_decay_strict() -> rumoca_compile::compile::StrictCompilation {
    let mut session = Session::new(SessionConfig {
        parallel: false,
        instantiation_depth_limit: rumoca_phase_instantiate::DEFAULT_INSTANTIATION_DEPTH_LIMIT,
    });
    session
        .add_document("parameter_decay.mo", PARAMETER_DECAY_SOURCE)
        .expect("ParameterDecay source parses without recovery");
    session
        .compile_model_strict("ParameterDecay")
        .unwrap_or_else(|report| panic!("ParameterDecay strict compilation failed: {report:?}"))
}

/// The parameter-decay trace is integrated under exactly the UnitDerivative
/// tolerances and output cadence, so the two golden traces come from
/// identically configured integrators and differ only in the model.
fn parameter_decay_options() -> SimOptions {
    unit_derivative_options()
}

/// The checked DAE declares the parameter before the state, so the role, not
/// the position, selects the state whose declaration span the eFMI refusals
/// must cite.
fn parameter_decay_state_span(
    compilation: &rumoca_compile::compile::StrictCompilation,
) -> rumoca_core::Span {
    compilation.result().dae.inspect(|view| {
        view.variables()
            .find(|(_, variable)| variable.role() == VariableRole::State)
            .expect("ParameterDecay has one state")
            .1
            .declaration()
            .span()
    })
}

fn pinned_artifact_input() -> rumoca_compile::codegen::targets::ArtifactSessionInput {
    rumoca_compile::codegen::targets::ArtifactSessionInput::construct(
        "2026-08-30T00:00:00Z"
            .parse()
            .expect("test artifact instant is canonical"),
        "12345678-1234-5678-9234-567812345678"
            .parse()
            .expect("test artifact seed is canonical"),
    )
}

fn compile_decay() -> rumoca_compile::compile::CompilationResult {
    compile_model(
        "pipeline.mo",
        "model PipelineDecay Real x(start=1, fixed=true); equation der(x)=-x; end PipelineDecay;",
        "PipelineDecay",
    )
}

/// The scalar fixture exercises only empty behavior for these Flat owners.
/// This checks absence of invented products, not support for their nonempty cases.
fn assert_unit_derivative_flat_empty_products(flat: &rumoca_ir_flat::Model) {
    assert!(flat.record_instances.is_empty());
    assert!(flat.record_types.is_empty());
    assert!(flat.structured_equations.is_empty());
    assert!(flat.assert_equations.is_empty());
    assert!(flat.initial_equations.is_empty());
    assert!(flat.initial_structured_equations.is_empty());
    assert!(flat.initial_assert_equations.is_empty());
    assert!(flat.algorithms.is_empty());
    assert!(flat.initial_algorithms.is_empty());
    assert!(flat.when_chains.is_empty());
    assert!(flat.functions.is_empty());
    assert!(flat.definite_roots.is_empty());
    assert!(flat.branches.is_empty());
    assert!(flat.optional_edges.is_empty());
    assert!(flat.potential_roots.is_empty());
    assert!(flat.top_level_connectors.is_empty());
    assert!(flat.top_level_input_components.is_empty());
    assert_eq!(flat.oc_break_edge_scalar_count, 0);
}

fn assert_fixed_start_modifiers(modifications: &[rumoca_ir_ast::Expression]) {
    let [start_modification, fixed_modification] = modifications else {
        panic!("the parsed x declaration must retain start and fixed modifiers");
    };
    let rumoca_ir_ast::Expression::Modification {
        target: start_target,
        value: Some(start_value),
        ..
    } = start_modification
    else {
        panic!("the first parsed x modifier must be a typed modification");
    };
    let [start_part] = start_target.parts.as_slice() else {
        panic!("the parsed start modifier must have one target part");
    };
    assert_eq!(start_part.ident.text.as_ref(), "start");
    let rumoca_ir_ast::Expression::Terminal {
        terminal_type: rumoca_ir_ast::TerminalType::UnsignedReal,
        token: ast_start,
        ..
    } = start_value.as_ref()
    else {
        panic!("the parsed start modifier must contain one Real literal");
    };
    assert_eq!(
        ast_start
            .text
            .parse::<f64>()
            .expect("the parsed start token is a Real literal")
            .to_bits(),
        2.0_f64.to_bits()
    );
    let rumoca_ir_ast::Expression::Modification {
        target: fixed_target,
        value: Some(fixed_value),
        ..
    } = fixed_modification
    else {
        panic!("the second parsed x modifier must be a typed modification");
    };
    let [fixed_part] = fixed_target.parts.as_slice() else {
        panic!("the parsed fixed modifier must have one target part");
    };
    assert_eq!(fixed_part.ident.text.as_ref(), "fixed");
    let rumoca_ir_ast::Expression::Terminal {
        terminal_type: rumoca_ir_ast::TerminalType::Bool,
        token: ast_fixed,
        ..
    } = fixed_value.as_ref()
    else {
        panic!("the parsed fixed modifier must contain one Boolean literal");
    };
    assert!(
        ast_fixed
            .text
            .parse::<bool>()
            .expect("the parsed fixed token is a Boolean literal")
    );
}

fn assert_parameter_decay_ast_parameter(
    ast_parameter: &rumoca_ir_ast::Component,
) -> rumoca_core::DefId {
    assert_eq!(ast_parameter.name, "a");
    let rumoca_core::Variability::Parameter(ast_parameter_prefix) = &ast_parameter.variability
    else {
        panic!("the parsed a declaration must carry the parameter prefix");
    };
    assert_eq!(ast_parameter_prefix.text.as_ref(), "parameter");
    let ast_parameter_id = ast_parameter
        .def_id
        .expect("Resolve assigns the parsed a declaration an identity");
    assert!(ast_parameter.has_explicit_binding);
    assert!(
        ast_parameter.source_modifications.is_empty(),
        "the parsed a declaration spells no attribute, so its fixed default is decided downstream"
    );
    // `-1.0` lexes as the negation of the unsigned literal `1.0`; nothing
    // before Solve folds it, so every layer must retain the negation.
    let Some(rumoca_ir_ast::Expression::Unary {
        op: rumoca_core::OpUnary::Minus,
        rhs: ast_binding_operand,
        ..
    }) = &ast_parameter.binding
    else {
        panic!("the parsed a binding must be the negation of a Real literal");
    };
    let rumoca_ir_ast::Expression::Terminal {
        terminal_type: rumoca_ir_ast::TerminalType::UnsignedReal,
        token: ast_binding_literal,
        ..
    } = ast_binding_operand.as_ref()
    else {
        panic!("the parsed a binding operand must be one Real literal");
    };
    assert_eq!(
        ast_binding_literal
            .text
            .parse::<f64>()
            .expect("the parsed binding token is a Real literal")
            .to_bits(),
        1.0_f64.to_bits()
    );

    ast_parameter_id
}

fn assert_unit_derivative_ast(
    compilation: &rumoca_compile::compile::StrictCompilation,
) -> (rumoca_core::DefId, rumoca_core::DefId, &rumoca_core::Span) {
    assert_eq!(compilation.model_name(), "UnitDerivative");
    let ast = &compilation.resolved().inner().definitions;
    let ast_model = ast
        .classes
        .get("UnitDerivative")
        .expect("the resolved AST retains the parsed model");
    let mut ast_components = ast_model.components.values();
    let ast_component = ast_components
        .next()
        .expect("the parsed model must contain one component declaration");
    assert!(
        ast_components.next().is_none(),
        "the parsed model must contain exactly one component declaration"
    );
    assert_eq!(ast_component.name, "x");
    let [ast_type_name] = ast_component.type_name.name.as_slice() else {
        panic!("the parsed component must have one unqualified type name");
    };
    assert_eq!(ast_type_name.text.as_ref(), "Real");
    let ast_real_declaration = ast_component
        .type_name
        .def_id
        .expect("Resolve assigns the parsed Real type an identity");
    let ast_component_id = ast_component
        .def_id
        .expect("Resolve assigns the parsed x declaration an identity");
    assert_fixed_start_modifiers(&ast_component.source_modifications);
    let [ast_equation] = ast_model.equations.as_slice() else {
        panic!("the parsed model must contain exactly one equation");
    };
    let rumoca_ir_ast::Equation::Simple {
        lhs: ast_lhs,
        rhs: ast_rhs,
    } = ast_equation
    else {
        panic!("der(x)=1.0 must parse as one simple equation");
    };
    let rumoca_ir_ast::Expression::DerivativeCall {
        args: ast_der_args,
        span: ast_der_span,
    } = ast_lhs
    else {
        panic!("the parsed equation left-hand side must carry typed derivative identity");
    };
    let [rumoca_ir_ast::Expression::ComponentReference(ast_x_reference)] = ast_der_args.as_slice()
    else {
        panic!("the parsed derivative operator must receive one component reference");
    };
    let [ast_x_part] = ast_x_reference.parts.as_slice() else {
        panic!("the parsed derivative argument must have one reference part");
    };
    assert_eq!(ast_x_part.ident.text.as_ref(), "x");
    assert_eq!(ast_x_part.def_id, Some(ast_component_id));
    let rumoca_ir_ast::Expression::Terminal {
        terminal_type: rumoca_ir_ast::TerminalType::UnsignedReal,
        token: ast_rhs,
        ..
    } = ast_rhs
    else {
        panic!("the parsed derivative right-hand side must be one Real literal");
    };
    assert_eq!(
        ast_rhs
            .text
            .parse::<f64>()
            .expect("the parsed derivative right-hand side is a Real literal")
            .to_bits(),
        1.0_f64.to_bits()
    );

    (ast_real_declaration, ast_component_id, ast_der_span)
}

fn assert_parameter_decay_ast(
    compilation: &rumoca_compile::compile::StrictCompilation,
) -> (rumoca_core::DefId, rumoca_core::DefId, &rumoca_core::Span) {
    assert_eq!(compilation.model_name(), "ParameterDecay");
    let ast = &compilation.resolved().inner().definitions;
    let ast_model = ast
        .classes
        .get("ParameterDecay")
        .expect("the resolved AST retains the parsed model");
    let mut ast_components = ast_model.components.values();
    let ast_parameter = ast_components
        .next()
        .expect("the parsed model must declare the parameter first");
    let ast_state = ast_components
        .next()
        .expect("the parsed model must declare the state second");
    assert!(
        ast_components.next().is_none(),
        "the parsed model must contain exactly two component declarations"
    );

    let ast_parameter_id = assert_parameter_decay_ast_parameter(ast_parameter);

    assert_eq!(ast_state.name, "x");
    assert!(matches!(
        ast_state.variability,
        rumoca_core::Variability::Empty
    ));
    assert!(!ast_state.has_explicit_binding);
    assert!(ast_state.binding.is_none());
    let ast_state_id = ast_state
        .def_id
        .expect("Resolve assigns the parsed x declaration an identity");
    assert_fixed_start_modifiers(&ast_state.source_modifications);

    let [ast_equation] = ast_model.equations.as_slice() else {
        panic!("the parsed model must contain exactly one equation");
    };
    let rumoca_ir_ast::Equation::Simple {
        lhs: ast_lhs,
        rhs: ast_rhs,
    } = ast_equation
    else {
        panic!("der(x) = a * x must parse as one simple equation");
    };
    let rumoca_ir_ast::Expression::DerivativeCall {
        args: ast_der_args,
        span: ast_der_span,
    } = ast_lhs
    else {
        panic!("the parsed equation left-hand side must carry typed derivative identity");
    };
    let [rumoca_ir_ast::Expression::ComponentReference(ast_der_argument)] = ast_der_args.as_slice()
    else {
        panic!("the parsed derivative operator must receive one component reference");
    };
    let [ast_der_argument_part] = ast_der_argument.parts.as_slice() else {
        panic!("the parsed derivative argument must have one reference part");
    };
    assert_eq!(ast_der_argument_part.ident.text.as_ref(), "x");
    assert_eq!(ast_der_argument_part.def_id, Some(ast_state_id));
    let rumoca_ir_ast::Expression::Binary {
        op: rumoca_core::OpBinary::Mul,
        lhs: ast_product_lhs,
        rhs: ast_product_rhs,
        ..
    } = ast_rhs
    else {
        panic!("the parsed derivative right-hand side must be one product");
    };
    let rumoca_ir_ast::Expression::ComponentReference(ast_product_parameter) =
        ast_product_lhs.as_ref()
    else {
        panic!("the parsed product left factor must be a component reference");
    };
    let [ast_product_parameter_part] = ast_product_parameter.parts.as_slice() else {
        panic!("the parsed product left factor must have one reference part");
    };
    assert_eq!(ast_product_parameter_part.ident.text.as_ref(), "a");
    assert_eq!(ast_product_parameter_part.def_id, Some(ast_parameter_id));
    let rumoca_ir_ast::Expression::ComponentReference(ast_product_state) = ast_product_rhs.as_ref()
    else {
        panic!("the parsed product right factor must be a component reference");
    };
    let [ast_product_state_part] = ast_product_state.parts.as_slice() else {
        panic!("the parsed product right factor must have one reference part");
    };
    assert_eq!(ast_product_state_part.ident.text.as_ref(), "x");
    assert_eq!(ast_product_state_part.def_id, Some(ast_state_id));

    (ast_parameter_id, ast_state_id, ast_der_span)
}

fn assert_single_state_balance(balance: &rumoca_phase_dae::BalanceDetail) {
    assert_eq!(
        *balance,
        rumoca_phase_dae::BalanceDetail {
            state_unknowns: 1,
            algebraic_unknowns: 0,
            output_unknowns: 0,
            discrete_real_unknowns: 0,
            discrete_value_unknowns: 0,
            continuous_equations: 1,
            discrete_real_equations: 0,
            discrete_value_definitions: 0,
        }
    );
}

fn assert_unit_derivative_flat_declaration(
    flat: &rumoca_ir_flat::Model,
    ast_real_declaration: rumoca_core::DefId,
    ast_component_id: rumoca_core::DefId,
) -> rumoca_core::InstanceId {
    assert_unit_derivative_flat_empty_products(flat);
    assert_eq!(flat.variables.len(), 1);
    let flat_variable = flat.variables.values().next().expect("one Flat variable");
    assert!(!flat_variable.instance_id.is_unset());
    let flat_instance_id = flat_variable.instance_id;
    let declared_real_type = flat
        .type_ids_by_def_id
        .get(&ast_real_declaration)
        .copied()
        .expect("Flat retains the resolved Real declaration identity");
    let flat_effective_type = flat
        .effective_types
        .get(&flat_variable.type_id)
        .expect("the Flat x declaration has one effective type");
    assert_eq!(flat_effective_type.nominal_type(), declared_real_type);
    assert_eq!(flat_effective_type.canonical_type(), declared_real_type);
    assert!(flat_effective_type.dimensions().is_empty());
    assert_eq!(flat_variable.fixed, Some(true));
    assert!(flat_variable.binding.is_none());
    let Some(rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(flat_start),
        ..
    }) = flat_variable.start.as_ref()
    else {
        panic!("the Flat declaration must retain the Real start literal");
    };
    assert_eq!(flat_start.to_bits(), 2.0_f64.to_bits());
    let flat_component_reference = flat_variable
        .component_ref
        .as_ref()
        .expect("the Flat declaration retains its structured AST reference");
    let [flat_component_part] = flat_component_reference.parts() else {
        panic!("the Flat declaration must retain one structured reference part");
    };
    assert_eq!(flat_component_part.def_id, ast_component_id);
    flat_instance_id
}

fn assert_unit_derivative_flat_equation(
    flat: &rumoca_ir_flat::Model,
    ast_der_span: &rumoca_core::Span,
    flat_instance_id: rumoca_core::InstanceId,
) {
    let [flat_equation] = flat.equations.as_slice() else {
        panic!("the Flat model must contain exactly one equation");
    };
    assert_eq!(flat_equation.scalar_count, 1);
    assert!(matches!(
        &flat_equation.origin,
        rumoca_ir_flat::EquationOrigin::ComponentEquation { component } if component.is_empty()
    ));
    let rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: flat_lhs,
        rhs: flat_rhs,
        ..
    } = &flat_equation.residual
    else {
        panic!("the Flat equation must be the typed residual der(x) - 1.0");
    };
    let rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Der,
        args: flat_der_args,
        span: flat_der_span,
    } = flat_lhs.as_ref()
    else {
        panic!("the Flat residual left operand must be the typed der builtin");
    };
    assert_eq!(flat_der_span, ast_der_span);
    let [
        rumoca_core::Expression::VarRef {
            name: flat_x_reference,
            subscripts,
            ..
        },
    ] = flat_der_args.as_slice()
    else {
        panic!("the Flat derivative must receive one variable reference");
    };
    assert!(subscripts.is_empty());
    assert_eq!(flat_x_reference.instance_id(), Some(flat_instance_id));
    let rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(flat_rhs),
        ..
    } = flat_rhs.as_ref()
    else {
        panic!("the Flat derivative right-hand side must remain a Real literal");
    };
    assert_eq!(flat_rhs.to_bits(), 1.0_f64.to_bits());
}

fn assert_parameter_decay_flat_declarations(
    flat: &rumoca_ir_flat::Model,
    ast_parameter_id: rumoca_core::DefId,
    ast_state_id: rumoca_core::DefId,
) -> (rumoca_core::InstanceId, rumoca_core::InstanceId) {
    let flat_variables: Vec<&rumoca_ir_flat::Variable> = flat.variables.values().collect();
    let [flat_parameter, flat_state] = flat_variables.as_slice() else {
        panic!("the Flat model must contain exactly two declarations in source order");
    };
    assert!(matches!(
        flat_parameter.variability,
        rumoca_core::Variability::Parameter(_)
    ));
    assert!(!flat_parameter.instance_id.is_unset());
    assert!(!flat_state.instance_id.is_unset());
    assert_ne!(flat_parameter.instance_id, flat_state.instance_id);
    let flat_parameter_instance_id = flat_parameter.instance_id;
    let flat_state_instance_id = flat_state.instance_id;
    let flat_parameter_reference = flat_parameter
        .component_ref
        .as_ref()
        .expect("the Flat parameter retains its structured AST reference");
    let [flat_parameter_part] = flat_parameter_reference.parts() else {
        panic!("the Flat parameter must retain one structured reference part");
    };
    assert_eq!(flat_parameter_part.def_id, ast_parameter_id);
    // Flat carries the absent spelling through as absence; the MLS 3.6 §4.8.1
    // parameter default is decided once, at the DAE join checked below.
    assert_eq!(flat_parameter.fixed, None);
    assert!(flat_parameter.dims.is_empty());
    let Some(rumoca_core::Expression::Unary {
        op: rumoca_core::OpUnary::Minus,
        rhs: flat_binding_operand,
        ..
    }) = flat_parameter.binding.as_ref()
    else {
        panic!("the Flat parameter binding must remain the negation of a Real literal");
    };
    let rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(flat_binding_literal),
        ..
    } = flat_binding_operand.as_ref()
    else {
        panic!("the Flat parameter binding operand must remain a Real literal");
    };
    assert_eq!(flat_binding_literal.to_bits(), 1.0_f64.to_bits());
    assert_eq!(flat_state.fixed, Some(true));
    assert!(flat_state.binding.is_none());
    let Some(rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(flat_start),
        ..
    }) = flat_state.start.as_ref()
    else {
        panic!("the Flat state must retain the Real start literal");
    };
    assert_eq!(flat_start.to_bits(), 2.0_f64.to_bits());
    let flat_state_reference = flat_state
        .component_ref
        .as_ref()
        .expect("the Flat state retains its structured AST reference");
    let [flat_state_part] = flat_state_reference.parts() else {
        panic!("the Flat state must retain one structured reference part");
    };
    assert_eq!(flat_state_part.def_id, ast_state_id);
    (flat_parameter_instance_id, flat_state_instance_id)
}

fn assert_parameter_decay_flat_equation(
    flat: &rumoca_ir_flat::Model,
    ast_der_span: &rumoca_core::Span,
    flat_parameter_instance_id: rumoca_core::InstanceId,
    flat_state_instance_id: rumoca_core::InstanceId,
) {
    let [flat_equation] = flat.equations.as_slice() else {
        panic!("the Flat model must contain exactly one equation");
    };
    assert_eq!(flat_equation.scalar_count, 1);
    let rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: flat_lhs,
        rhs: flat_rhs,
        ..
    } = &flat_equation.residual
    else {
        panic!("the Flat equation must be the typed residual der(x) - a * x");
    };
    let rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Der,
        args: flat_der_args,
        span: flat_der_span,
    } = flat_lhs.as_ref()
    else {
        panic!("the Flat residual left operand must be the typed der builtin");
    };
    assert_eq!(flat_der_span, ast_der_span);
    let [
        rumoca_core::Expression::VarRef {
            name: flat_der_reference,
            subscripts: flat_der_subscripts,
            ..
        },
    ] = flat_der_args.as_slice()
    else {
        panic!("the Flat derivative must receive one variable reference");
    };
    assert!(flat_der_subscripts.is_empty());
    assert_eq!(
        flat_der_reference.instance_id(),
        Some(flat_state_instance_id)
    );
    let rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Mul,
        lhs: flat_product_lhs,
        rhs: flat_product_rhs,
        ..
    } = flat_rhs.as_ref()
    else {
        panic!("the Flat derivative right-hand side must remain one product");
    };
    let rumoca_core::Expression::VarRef {
        name: flat_product_parameter,
        subscripts: flat_product_parameter_subscripts,
        ..
    } = flat_product_lhs.as_ref()
    else {
        panic!("the Flat product left factor must be a variable reference");
    };
    assert!(flat_product_parameter_subscripts.is_empty());
    assert_eq!(
        flat_product_parameter.instance_id(),
        Some(flat_parameter_instance_id)
    );
    let rumoca_core::Expression::VarRef {
        name: flat_product_state,
        subscripts: flat_product_state_subscripts,
        ..
    } = flat_product_rhs.as_ref()
    else {
        panic!("the Flat product right factor must be a variable reference");
    };
    assert!(flat_product_state_subscripts.is_empty());
    assert_eq!(
        flat_product_state.instance_id(),
        Some(flat_state_instance_id)
    );
}

fn assert_no_dae_event_owners(view: rumoca_ir_dae::DaeView<'_>) {
    assert_eq!(view.discrete_real_equation_count(), 0);
    assert_eq!(view.discrete_value_owner_count(), 0);
    assert_eq!(view.model_event_transaction_count(), 0);
    assert_eq!(view.relation_count(), 0);
    assert_eq!(view.condition_count(), 0);
    assert_eq!(view.root_count(), 0);
    assert_eq!(view.time_event_count(), 0);
    assert_eq!(view.event_action_count(), 0);
    assert_eq!(view.clock_count(), 0);
    assert_eq!(view.previous_value_count(), 0);
    assert_eq!(view.terminal_count(), 0);
    assert_eq!(view.delay_count(), 0);
}

fn assert_parameter_decay_dae_parameter<'dae>(
    view: rumoca_ir_dae::DaeView<'dae>,
    parameter: rumoca_ir_dae::VariableView<'dae>,
    flat_parameter_instance_id: rumoca_core::InstanceId,
) -> rumoca_ir_dae::ParameterId<'dae> {
    assert_eq!(parameter.name().as_str(), "a");
    assert_eq!(
        parameter.source_occurrence().instance_id(),
        flat_parameter_instance_id
    );
    assert_eq!(parameter.role(), VariableRole::Parameter);
    assert_eq!(
        parameter.variability(),
        rumoca_ir_dae::ExpressionVariability::Parameter
    );
    assert_eq!(
        parameter.causality(),
        rumoca_ir_dae::VariableCausality::Parameter
    );
    // The declaration spells no `fixed`; the checked DAE is the one
    // layer that turns that absence into the §4.8.1 parameter default.
    assert_eq!(parameter.fixed(), rumoca_core::Fixity::Fixed);
    assert!(parameter.is_tunable());
    let rumoca_ir_dae::VariableIdentity::Parameter(parameter_id) = parameter.identity() else {
        panic!("the parameter declaration must own a typed parameter identity");
    };
    let binding = parameter
        .binding()
        .expect("the checked parameter owns its binding expression");
    let rumoca_ir_dae::ExpressionOperation::Unary {
        operator: rumoca_ir_dae::UnaryOperator::Negate,
        operand: binding_operand,
    } = view.exact_expression(binding).operation()
    else {
        panic!("the checked binding must remain the negation of a Real literal");
    };
    let rumoca_ir_dae::ExpressionOperation::Literal(rumoca_ir_dae::DaeLiteral::Real(
        binding_literal,
    )) = view.exact_expression(binding_operand).operation()
    else {
        panic!("the checked binding operand must remain a Real literal");
    };
    assert_eq!(binding_literal.to_bits(), 1.0_f64.to_bits());
    assert_eq!(view.source_text(parameter.declaration()), Some("Real a"));

    parameter_id
}

fn assert_parameter_decay_dae_state<'dae>(
    view: rumoca_ir_dae::DaeView<'dae>,
    state: rumoca_ir_dae::VariableView<'dae>,
    flat_state_instance_id: rumoca_core::InstanceId,
) -> rumoca_ir_dae::StateId<'dae> {
    assert_eq!(state.name().as_str(), "x");
    assert_eq!(
        state.source_occurrence().instance_id(),
        flat_state_instance_id
    );
    assert_eq!(state.role(), VariableRole::State);
    assert_eq!(
        state.variability(),
        rumoca_ir_dae::ExpressionVariability::Continuous
    );
    assert_eq!(state.causality(), rumoca_ir_dae::VariableCausality::Local);
    assert!(!state.is_tunable());
    let rumoca_ir_dae::VariableIdentity::State(state_id) = state.identity() else {
        panic!("the state declaration must own a typed state identity");
    };
    assert_eq!(state.fixed(), rumoca_core::Fixity::Fixed);
    assert!(state.binding().is_none());
    let start = state
        .start()
        .expect("fixed state owns its start expression");
    let rumoca_ir_dae::ExpressionOperation::Literal(rumoca_ir_dae::DaeLiteral::Real(start)) =
        view.exact_expression(start).operation()
    else {
        panic!("the fixed initial value must remain the exact Real literal");
    };
    assert_eq!(start.to_bits(), 2.0_f64.to_bits());
    assert_eq!(view.source_text(state.declaration()), Some("Real x"));

    state_id
}

fn assert_unit_derivative_dae(
    dae: &rumoca_ir_dae::Dae,
    flat_instance_id: rumoca_core::InstanceId,
) -> rumoca_core::SourceOccurrenceId {
    dae.inspect(|view| {
        assert_eq!(view.variables().len(), 1);
        let (_, variable) = view.variables().next().expect("one checked variable");
        let source_occurrence = variable.source_occurrence();
        assert_eq!(source_occurrence.instance_id(), flat_instance_id);
        assert_eq!(variable.name().as_str(), "x");
        assert_eq!(variable.role(), VariableRole::State);
        let rumoca_ir_dae::VariableIdentity::State(state) = variable.identity() else {
            panic!("the sole declaration must own a typed state identity");
        };
        assert_eq!(variable.fixed(), rumoca_core::Fixity::Fixed);
        let start = variable
            .start()
            .expect("fixed state owns its start expression");
        let rumoca_ir_dae::ExpressionOperation::Literal(rumoca_ir_dae::DaeLiteral::Real(start)) =
            view.exact_expression(start).operation()
        else {
            panic!("the fixed initial value must remain the exact Real literal");
        };
        assert_eq!(start.to_bits(), 2.0_f64.to_bits());
        assert_eq!(view.source_text(variable.declaration()), Some("Real x"));
        assert_eq!(view.continuous_owner_count(), 1);
        let equation = view
            .continuous_equation(0)
            .expect("one checked differential equation");
        let rumoca_ir_dae::ExpressionOperation::Binary {
            operator: rumoca_ir_dae::BinaryOperator::Subtract,
            lhs,
            rhs,
        } = view.exact_expression(equation.residual()).operation()
        else {
            panic!("the equation must retain one exact derivative residual");
        };
        assert!(matches!(
            view.exact_expression(lhs).operation(),
            rumoca_ir_dae::ExpressionOperation::Coordinate(
                rumoca_ir_dae::CoordinateView::Derivative(owner)
            ) if owner == state
        ));
        let rumoca_ir_dae::ExpressionOperation::Literal(rumoca_ir_dae::DaeLiteral::Real(rhs)) =
            view.exact_expression(rhs).operation()
        else {
            panic!("the derivative right-hand side must remain a Real literal");
        };
        assert_eq!(rhs.to_bits(), 1.0_f64.to_bits());
        assert_eq!(
            view.source_text(equation.provenance()),
            Some("der(x) = 1.0")
        );
        assert_eq!(view.initialization_owner_count(), 0);
        assert_no_dae_event_owners(view);
        source_occurrence
    })
}

fn assert_parameter_decay_dae(
    dae: &rumoca_ir_dae::Dae,
    flat_parameter_instance_id: rumoca_core::InstanceId,
    flat_state_instance_id: rumoca_core::InstanceId,
) -> (
    rumoca_core::SourceOccurrenceId,
    rumoca_core::SourceOccurrenceId,
) {
    dae.inspect(|view| {
        assert_eq!(view.variable_count(), 2);
        let mut variables = view.variables();
        let (_, parameter) = variables.next().expect("the checked parameter");
        let (_, state) = variables.next().expect("the checked state");
        assert!(variables.next().is_none());

        let parameter_id =
            assert_parameter_decay_dae_parameter(view, parameter, flat_parameter_instance_id);

        let state_id = assert_parameter_decay_dae_state(view, state, flat_state_instance_id);

        assert_eq!(view.continuous_owner_count(), 1);
        let equation = view
            .continuous_equation(0)
            .expect("one checked differential equation");
        let rumoca_ir_dae::ExpressionOperation::Binary {
            operator: rumoca_ir_dae::BinaryOperator::Subtract,
            lhs,
            rhs,
        } = view.exact_expression(equation.residual()).operation()
        else {
            panic!("the equation must retain one exact derivative residual");
        };
        assert!(matches!(
            view.exact_expression(lhs).operation(),
            rumoca_ir_dae::ExpressionOperation::Coordinate(
                rumoca_ir_dae::CoordinateView::Derivative(owner)
            ) if owner == state_id
        ));
        let rumoca_ir_dae::ExpressionOperation::Binary {
            operator: rumoca_ir_dae::BinaryOperator::Multiply,
            lhs: product_lhs,
            rhs: product_rhs,
        } = view.exact_expression(rhs).operation()
        else {
            panic!("the derivative right-hand side must remain one product");
        };
        assert!(matches!(
            view.exact_expression(product_lhs).operation(),
            rumoca_ir_dae::ExpressionOperation::Coordinate(
                rumoca_ir_dae::CoordinateView::Parameter(owner)
            ) if owner == parameter_id
        ));
        assert!(matches!(
            view.exact_expression(product_rhs).operation(),
            rumoca_ir_dae::ExpressionOperation::Coordinate(
                rumoca_ir_dae::CoordinateView::State(owner)
            ) if owner == state_id
        ));
        assert_eq!(
            view.source_text(equation.provenance()),
            Some("der(x) = a * x")
        );
        // The parameter binding is a value, not an initialization equation:
        // the checked DAE contributes no initialization owner for it.
        assert_eq!(view.initialization_owner_count(), 0);
        assert_no_dae_event_owners(view);
        (parameter.source_occurrence(), state.source_occurrence())
    })
}

fn assert_unit_derivative_solve(
    solve: &rumoca_ir_solve::SolveModel,
    dae_source_occurrence: rumoca_core::SourceOccurrenceId,
) {
    assert_eq!(solve.problem().solve_layout().state_scalar_count(), 1);
    assert_eq!(solve.problem().solve_layout().algebraic_scalar_count(), 0);
    assert_eq!(solve.problem().solve_layout().output_scalar_count(), 0);
    assert!(
        solve
            .problem()
            .initialization()
            .projection_plan()
            .blocks
            .is_empty(),
        "fixed=true owns the initial equation; no nonlinear guess projection exists"
    );
    assert_eq!(solve.initial_y(), [2.0]);
    assert_eq!(solve.solver_nominals(), [1.0]);
    let [entry] = solve.variable_catalog().entries() else {
        panic!("UnitDerivative Solve root must retain exactly one declaration");
    };
    assert_eq!(entry.source_occurrence(), dae_source_occurrence);
    assert_eq!(entry.id().index(), 0);
    assert_eq!(entry.name(), "x");
    assert!(entry.dimensions().is_empty());
    assert_eq!(entry.scalar_names(), ["x"]);
    assert_eq!(
        entry.causality(),
        rumoca_ir_solve::SolveVariableCausality::Local
    );
    assert_eq!(
        entry.variability(),
        rumoca_ir_solve::SolveVariableVariability::Continuous
    );
    assert_eq!(entry.fixed(), rumoca_core::Fixity::Fixed);
    assert_eq!(entry.start(), Some([2.0].as_slice()));
    assert_eq!(
        entry.role(),
        rumoca_ir_solve::SolveVariableStorageRole::State
    );
    assert_eq!(
        entry.state_initialization(),
        rumoca_ir_solve::SolveStateInitialization::Exact
    );
    assert_eq!(entry.storage().scalar_count, 1);
    assert_eq!(
        entry.storage().base,
        rumoca_ir_solve::SolveStorageCoordinate::Y(0)
    );
    assert_eq!(
        solve
            .problem()
            .continuous()
            .derivative_rhs()
            .len()
            .expect("checked derivative block has a finite output count"),
        1
    );
    let [rumoca_ir_solve::ComputeNode::ScalarPrograms(derivative)] = solve
        .problem()
        .continuous()
        .derivative_rhs()
        .nodes
        .as_slice()
    else {
        panic!("der(x)=1 must remain one exact scalar derivative program");
    };
    assert_eq!(
        derivative.program(0),
        Some(
            [
                rumoca_ir_solve::LinearOp::Const { dst: 0, value: 1.0 },
                rumoca_ir_solve::LinearOp::StoreOutput { src: 0 },
            ]
            .as_slice()
        )
    );
    assert_eq!(derivative.programs().len(), 1);
}

fn assert_parameter_decay_solve_parameter(
    parameter_entry: &rumoca_ir_solve::SolveVariableCatalogEntry,
    dae_parameter_occurrence: rumoca_core::SourceOccurrenceId,
) {
    assert_eq!(
        parameter_entry.source_occurrence(),
        dae_parameter_occurrence
    );
    assert_eq!(parameter_entry.id().index(), 0);
    assert_eq!(parameter_entry.name(), "a");
    assert!(parameter_entry.dimensions().is_empty());
    assert_eq!(parameter_entry.scalar_names(), ["a"]);
    assert_eq!(
        parameter_entry.causality(),
        rumoca_ir_solve::SolveVariableCausality::Parameter
    );
    assert_eq!(
        parameter_entry.variability(),
        rumoca_ir_solve::SolveVariableVariability::Tunable
    );
    assert!(parameter_entry.is_tunable());
    assert_eq!(parameter_entry.fixed(), rumoca_core::Fixity::Fixed);
    assert_eq!(parameter_entry.start(), Some([-1.0].as_slice()));
    assert_eq!(
        parameter_entry.role(),
        rumoca_ir_solve::SolveVariableStorageRole::Parameter
    );
    assert_eq!(
        parameter_entry.state_initialization(),
        rumoca_ir_solve::SolveStateInitialization::NotState
    );
    assert_eq!(parameter_entry.storage().scalar_count, 1);
    assert_eq!(
        parameter_entry.storage().base,
        rumoca_ir_solve::SolveStorageCoordinate::P(0)
    );
}

fn assert_parameter_decay_solve_state(
    state_entry: &rumoca_ir_solve::SolveVariableCatalogEntry,
    dae_state_occurrence: rumoca_core::SourceOccurrenceId,
) {
    assert_eq!(state_entry.source_occurrence(), dae_state_occurrence);
    assert_eq!(state_entry.id().index(), 1);
    assert_eq!(state_entry.name(), "x");
    assert!(state_entry.dimensions().is_empty());
    assert_eq!(state_entry.scalar_names(), ["x"]);
    assert_eq!(
        state_entry.causality(),
        rumoca_ir_solve::SolveVariableCausality::Local
    );
    assert_eq!(
        state_entry.variability(),
        rumoca_ir_solve::SolveVariableVariability::Continuous
    );
    assert!(!state_entry.is_tunable());
    assert_eq!(state_entry.fixed(), rumoca_core::Fixity::Fixed);
    assert_eq!(state_entry.start(), Some([2.0].as_slice()));
    assert_eq!(
        state_entry.role(),
        rumoca_ir_solve::SolveVariableStorageRole::State
    );
    assert_eq!(
        state_entry.state_initialization(),
        rumoca_ir_solve::SolveStateInitialization::Exact
    );
    assert_eq!(state_entry.storage().scalar_count, 1);
    assert_eq!(
        state_entry.storage().base,
        rumoca_ir_solve::SolveStorageCoordinate::Y(0)
    );
}

fn assert_parameter_decay_solve(
    solve: &rumoca_ir_solve::SolveModel,
    dae_parameter_occurrence: rumoca_core::SourceOccurrenceId,
    dae_state_occurrence: rumoca_core::SourceOccurrenceId,
) {
    assert_eq!(solve.problem().solve_layout().state_scalar_count(), 1);
    assert_eq!(solve.problem().solve_layout().algebraic_scalar_count(), 0);
    assert_eq!(solve.problem().solve_layout().output_scalar_count(), 0);
    assert_eq!(solve.problem().layout().p_scalars(), 1);
    assert!(
        solve
            .problem()
            .initialization()
            .projection_plan()
            .blocks
            .is_empty(),
        "fixed=true owns the initial equation; no nonlinear guess projection exists"
    );
    assert_eq!(solve.initial_y(), [2.0]);
    assert_eq!(solve.solver_nominals(), [1.0]);
    // The binding `-1.0` is evaluated exactly once, into the P column.
    let [parameter_value] = solve.parameters() else {
        panic!("ParameterDecay Solve root must carry exactly one parameter scalar");
    };
    assert_eq!(parameter_value.to_bits(), (-1.0_f64).to_bits());
    let [parameter_entry, state_entry] = solve.variable_catalog().entries() else {
        panic!("ParameterDecay Solve root must retain exactly two declarations");
    };
    assert_parameter_decay_solve_parameter(parameter_entry, dae_parameter_occurrence);
    assert_parameter_decay_solve_state(state_entry, dae_state_occurrence);
    assert_eq!(
        solve
            .problem()
            .continuous()
            .derivative_rhs()
            .len()
            .expect("checked derivative block has a finite output count"),
        1
    );
    let [rumoca_ir_solve::ComputeNode::ScalarPrograms(derivative)] = solve
        .problem()
        .continuous()
        .derivative_rhs()
        .nodes
        .as_slice()
    else {
        panic!("der(x) = a * x must remain one exact scalar derivative program");
    };
    // The kernel reads the parameter column and the state, multiplies,
    // and stores: no constant is folded in, which is exactly why the
    // C61 scalar-constant-derivative profile does not cover this model.
    assert_eq!(
        derivative.program(0),
        Some(
            [
                rumoca_ir_solve::LinearOp::LoadP { dst: 0, index: 0 },
                rumoca_ir_solve::LinearOp::LoadY { dst: 1, index: 0 },
                rumoca_ir_solve::LinearOp::Binary {
                    dst: 2,
                    op: rumoca_ir_solve::BinaryOp::Mul,
                    lhs: 0,
                    rhs: 1,
                },
                rumoca_ir_solve::LinearOp::StoreOutput { src: 2 },
            ]
            .as_slice()
        )
    );
    assert_eq!(derivative.programs().len(), 1);
}

fn assert_unit_derivative_trace(
    simulation: &rumoca_solver::SimResult,
    observed_trace_names: &[String],
) {
    assert_eq!(simulation.n_states, 1);
    assert!(simulation.termination.is_none());
    assert_eq!(simulation.names, ["x"]);
    assert_eq!(simulation.names, observed_trace_names);
    assert_eq!(simulation.data.len(), 1);
    let x = &simulation.data[0];
    assert_eq!(simulation.times.len(), 11);
    assert_eq!(x.len(), 11);
    assert_eq!(
        simulation.times.first().map(|time| time.to_bits()),
        Some(0.0f64.to_bits())
    );
    assert_eq!(
        simulation.times.last().map(|time| time.to_bits()),
        Some(1.0f64.to_bits())
    );
    assert!(
        simulation.times.windows(2).all(|pair| pair[0] < pair[1]),
        "the requested trace interval must be sampled in strictly increasing time order"
    );
    // Human-derived oracle: the source states x(0)=2 and der(x)=1, hence x(t)=2+t.
    // It is intentionally independent of every compiler and runtime artifact.
    // The model's constant stages cannot witness RK order, dense-interpolation
    // coefficients, rejection, or error control; those obligations belong to
    // a nonconstant-dynamics golden model.
    for (time, value) in simulation.times.iter().zip(x) {
        let expected_value = 2.0 + time;
        assert!(time.is_finite());
        assert!(value.is_finite());
        assert!(
            (value - expected_value).abs() <= 1.0e-13,
            "der(x)=1 with x(0)=2 requires x({time})={expected_value}, found {value}"
        );
    }
}

fn assert_parameter_decay_trace(
    simulation: &rumoca_solver::SimResult,
    observed_trace_names: &[String],
) {
    assert_eq!(simulation.n_states, 1);
    assert!(simulation.termination.is_none());
    assert_eq!(simulation.names, ["x"]);
    assert_eq!(simulation.names, observed_trace_names);
    assert_eq!(simulation.data.len(), 1);
    let x = &simulation.data[0];
    assert_eq!(simulation.times.len(), 11);
    assert_eq!(x.len(), 11);
    // Human-derived oracle: the source states x(0)=2 and der(x)=a*x with
    // a=-1, hence x(t)=2*exp(-t). It is intentionally independent of every
    // compiler and runtime artifact.
    //
    // Unlike the constant-derivative trace this is not integrated exactly,
    // so the bound is derived from the integrator contract rather than from
    // the observed residual. The options request rtol=atol=1e-12 with a unit
    // state nominal, so the Dormand-Prince controller accepts a step only when
    // its embedded error estimate is at most atol + rtol*|x| <= 3e-12. The
    // system contracts (a < 0), so accepted local errors add without
    // amplification: after N accepted steps the endpoint error is at most
    // N*3e-12, and each 0.1 output sample is read from the order-four
    // continuous extension of the accepted step that contains it, whose error
    // is of the same order as the accepted estimate. A bound of 1e-9 covers
    // roughly three hundred accepted steps plus that interpolation, which the
    // controller never approaches on [0, 1] from a 0.01 initial step. It is
    // also tight: any method of order four or lower stepping only on the 0.1
    // output grid misses by at least three orders of magnitude, so the check
    // exercises the step controller and the dense output, not the tableau
    // alone.
    const TRACE_TOLERANCE: f64 = 1.0e-9;
    for (index, (time, value)) in simulation.times.iter().zip(x).enumerate() {
        let expected_time = 0.1 * index as f64;
        let expected_value = 2.0 * (-expected_time).exp();
        assert!(time.is_finite());
        assert!(value.is_finite());
        assert!(
            (time - expected_time).abs() <= 1.0e-12,
            "trace sample {index} must occur at t={expected_time}, found {time}"
        );
        assert!(
            (value - expected_value).abs() <= TRACE_TOLERANCE,
            "der(x)=-x with x(0)=2 requires x({expected_time})={expected_value} within {TRACE_TOLERANCE:e}, found {value}"
        );
    }
}

/// The smallest end-to-end executable witness: one state declaration, one
/// differential equation, and the closed-form trajectory x(t) = 2 + t. Every
/// larger source/MSL regression builds on this production path.
#[test]
fn minimal_unit_derivative_reaches_solve_and_matches_closed_form() {
    let compilation = compile_unit_derivative_strict();
    let (ast_real_declaration, ast_component_id, ast_der_span) =
        assert_unit_derivative_ast(&compilation);

    let result = compilation.result();
    assert_single_state_balance(&result.balance_detail);
    let flat_instance_id = assert_unit_derivative_flat_declaration(
        &result.flat,
        ast_real_declaration,
        ast_component_id,
    );
    assert_unit_derivative_flat_equation(&result.flat, ast_der_span, flat_instance_id);

    let dae_source_occurrence = assert_unit_derivative_dae(&result.dae, flat_instance_id);

    let options = unit_derivative_options();
    let mut solve_observer_invocations = 0;
    let mut observed_trace_names = Vec::new();
    let (prepared, _) = prepare_simulation(
        &result.dae,
        &options,
        |_| {},
        |solve| {
            solve_observer_invocations += 1;
            assert_unit_derivative_solve(solve, dae_source_occurrence);
            observed_trace_names = solve.visible_names().map(str::to_owned).collect();
        },
    )
    .expect("one correlated Solve/FMI artifact is prepared");
    assert_eq!(solve_observer_invocations, 1);
    assert_eq!(prepared.backend(), rumoca_sim::SimBackend::Rk45);
    let simulation = prepared
        .run()
        .expect("the observed Solve artifact executes");
    assert_unit_derivative_trace(&simulation, &observed_trace_names);
}

/// The C61 equation-refinement receipt is minted by the correlated production
/// lowering that the golden model's runtime artifact is finished from, over
/// the exact strict compilation of the golden source. The receipt is opaque
/// and mintable only by the phase checker, so holding a borrow of it is the
/// proof that the checker accepted this construction.
#[test]
fn unit_derivative_production_lowering_carries_the_equation_refinement_receipt() {
    let compilation = compile_unit_derivative_strict();
    let options = unit_derivative_options();
    let lowered = rumoca_sim::lower_correlated_for_simulation_with_overrides(
        compilation.result().dae.as_ref(),
        &options,
    )
    .expect("UnitDerivative lowers on the correlated production route");
    if let Err(unsupported) = lowered.scalar_constant_derivative_refinement() {
        panic!("UnitDerivative is inside the scalar constant-derivative profile: {unsupported}");
    }
}

/// This is a separate capability disposition, not part of the executable
/// MLS/FMI-ME acceptance result above. A continuous unclocked model does not
/// supply the discretization required by the current eFMI Algorithm Code
/// profile, so the only sound result is a typed refusal before construction.
#[test]
fn unit_derivative_efmi_algorithm_code_is_refused_before_construction() {
    let compilation = compile_unit_derivative_strict();
    let state_span = compilation.result().dae.inspect(|view| {
        view.variables()
            .next()
            .expect("UnitDerivative has one state")
            .1
            .declaration()
            .span()
    });
    let errors = rumoca_core::with_target_invocation_brand(|brand| {
        rumoca_phase_galec::lower_to_algorithm_code(
            brand,
            &rumoca_phase_galec::GalecInput::new(
                compilation.result().dae.as_ref(),
                compilation.model_name(),
            ),
            &rumoca_phase_galec::GalecOptions::new(
                rumoca_ir_galec::package::AlgorithmCodeArithmeticProfile::construct(
                    rumoca_ir_galec::package::AlgorithmCodeRealFormat::Binary64,
                    rumoca_ir_galec::package::AlgorithmCodeIntegerFormat::I32,
                    rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingPositiveZero,
                ),
            ),
        )
        .expect_err("continuous dynamics cannot construct an Algorithm Code product")
    });
    assert!(matches!(
        errors.as_slice(),
        [
            rumoca_phase_galec::GalecTargetError::ContinuousDynamics {
                states: 1,
                equations: 1,
                span: Some(span),
            },
            rumoca_phase_galec::GalecTargetError::NoPeriodicClock,
        ] if *span == state_span
    ));
    assert_eq!(errors[0].code(), "EGT001");
    assert!(
        errors[0]
            .to_string()
            .contains("unsupported-feature:continuous-dynamics")
    );
}

#[test]
fn unit_derivative_registered_efmi_targets_refuse_typed_at_the_state() {
    use miette::Diagnostic as _;

    let compilation = compile_unit_derivative_strict();
    let state_span = compilation.result().dae.inspect(|view| {
        view.variables()
            .next()
            .expect("UnitDerivative has one state")
            .1
            .declaration()
            .span()
    });
    for target_id in ["galec", "efmu"] {
        let target = rumoca_compile::codegen::targets::TargetBundle::load(target_id)
            .unwrap_or_else(|error| panic!("load registered {target_id} target: {error:#}"))
            .check()
            .unwrap_or_else(|error| panic!("check registered {target_id} target: {error:#}"));
        let error = match compilation.render_target(target, pinned_artifact_input()) {
            Err(error) => error,
            Ok(_) => panic!("continuous UnitDerivative must not produce an eFMI artifact"),
        };
        let typed = error
            .downcast_ref::<rumoca_phase_codegen::CodegenError>()
            .unwrap_or_else(|| panic!("{target_id} refusal must retain CodegenError: {error:#}"));
        assert_eq!(
            typed.code().map(|code| code.to_string()),
            Some("rumoca::codegen::EC009".to_owned())
        );
        assert!(matches!(
            typed,
            rumoca_phase_codegen::CodegenError::UnsupportedTargetFeature {
                target,
                feature: "continuous_states",
                detail,
                span: Some(span),
            } if target == target_id
                && detail == "1 state(s); not yet supported by the Rumoca GALEC projection"
                && *span == state_span
        ));
    }
}

/// The second golden scenario. One parameter, one state, one differential
/// equation, and the closed-form trajectory x(t) = 2 exp(a t). It adds exactly
/// one axis to the UnitDerivative core: a parameter declaration, its binding
/// evaluation, the P storage column, and a derivative kernel that multiplies
/// parameter and state storage instead of storing a constant.
#[test]
fn parameter_decay_reaches_solve_with_a_parameter_column_and_matches_exponential_decay() {
    let compilation = compile_parameter_decay_strict();
    let (ast_parameter_id, ast_state_id, ast_der_span) = assert_parameter_decay_ast(&compilation);

    let result = compilation.result();
    assert_single_state_balance(&result.balance_detail);
    let (flat_parameter_instance_id, flat_state_instance_id) =
        assert_parameter_decay_flat_declarations(&result.flat, ast_parameter_id, ast_state_id);
    assert_parameter_decay_flat_equation(
        &result.flat,
        ast_der_span,
        flat_parameter_instance_id,
        flat_state_instance_id,
    );

    let (dae_parameter_occurrence, dae_state_occurrence) = assert_parameter_decay_dae(
        &result.dae,
        flat_parameter_instance_id,
        flat_state_instance_id,
    );

    let options = parameter_decay_options();
    let mut solve_observer_invocations = 0;
    let mut observed_trace_names = Vec::new();
    let (prepared, _) = prepare_simulation(
        &result.dae,
        &options,
        |_| {},
        |solve| {
            solve_observer_invocations += 1;
            assert_parameter_decay_solve(solve, dae_parameter_occurrence, dae_state_occurrence);
            observed_trace_names = solve.visible_names().map(str::to_owned).collect();
        },
    )
    .expect("one correlated Solve/FMI artifact is prepared");
    assert_eq!(solve_observer_invocations, 1);
    assert_eq!(prepared.backend(), rumoca_sim::SimBackend::Rk45);
    let simulation = prepared
        .run()
        .expect("the observed Solve artifact executes");
    assert_parameter_decay_trace(&simulation, &observed_trace_names);
}

/// The C61 scalar-constant-derivative profile admits exactly one DAE variable
/// with the kernel `[Const, StoreOutput]`. ParameterDecay declares two, and
/// its kernel multiplies parameter and state storage, so the production
/// lowering carries the typed unclaimed disposition instead of a receipt.
/// The `dae-solve-production-refinement` endpoint is therefore absent from
/// this model's golden profile: no receipt can be minted or owed for it.
#[test]
fn parameter_decay_is_outside_the_scalar_constant_derivative_profile() {
    let compilation = compile_parameter_decay_strict();
    let options = parameter_decay_options();
    let lowered = rumoca_sim::lower_correlated_for_simulation_with_overrides(
        compilation.result().dae.as_ref(),
        &options,
    )
    .expect("ParameterDecay lowers on the correlated production route");
    let unsupported = lowered
        .scalar_constant_derivative_refinement()
        .err()
        .expect("a two-variable model is outside the scalar constant-derivative profile");
    assert_eq!(
        *unsupported,
        rumoca_phase_solve::ScalarConstantDerivativeUnsupported::VariableCount { actual: 2 }
    );
}

/// A continuous unclocked model does not supply the discretization the eFMI
/// Algorithm Code profile requires, whatever its parameter count, so the
/// direct GALEC route refuses ParameterDecay at the state before construction
/// exactly as it refuses UnitDerivative.
#[test]
fn parameter_decay_efmi_algorithm_code_is_refused_before_construction() {
    let compilation = compile_parameter_decay_strict();
    let state_span = parameter_decay_state_span(&compilation);
    let errors = rumoca_core::with_target_invocation_brand(|brand| {
        rumoca_phase_galec::lower_to_algorithm_code(
            brand,
            &rumoca_phase_galec::GalecInput::new(
                compilation.result().dae.as_ref(),
                compilation.model_name(),
            ),
            &rumoca_phase_galec::GalecOptions::new(
                rumoca_ir_galec::package::AlgorithmCodeArithmeticProfile::construct(
                    rumoca_ir_galec::package::AlgorithmCodeRealFormat::Binary64,
                    rumoca_ir_galec::package::AlgorithmCodeIntegerFormat::I32,
                    rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingPositiveZero,
                ),
            ),
        )
        .expect_err("continuous dynamics cannot construct an Algorithm Code product")
    });
    assert!(matches!(
        errors.as_slice(),
        [
            rumoca_phase_galec::GalecTargetError::ContinuousDynamics {
                states: 1,
                equations: 1,
                span: Some(span),
            },
            rumoca_phase_galec::GalecTargetError::NoPeriodicClock,
        ] if *span == state_span
    ));
    assert_eq!(errors[0].code(), "EGT001");
    assert!(
        errors[0]
            .to_string()
            .contains("unsupported-feature:continuous-dynamics")
    );
}

#[test]
fn parameter_decay_registered_efmi_targets_refuse_typed_at_the_state() {
    use miette::Diagnostic as _;

    let compilation = compile_parameter_decay_strict();
    let state_span = parameter_decay_state_span(&compilation);
    for target_id in ["galec", "efmu"] {
        let target = rumoca_compile::codegen::targets::TargetBundle::load(target_id)
            .unwrap_or_else(|error| panic!("load registered {target_id} target: {error:#}"))
            .check()
            .unwrap_or_else(|error| panic!("check registered {target_id} target: {error:#}"));
        let error = match compilation.render_target(target, pinned_artifact_input()) {
            Err(error) => error,
            Ok(_) => panic!("continuous ParameterDecay must not produce an eFMI artifact"),
        };
        let typed = error
            .downcast_ref::<rumoca_phase_codegen::CodegenError>()
            .unwrap_or_else(|| panic!("{target_id} refusal must retain CodegenError: {error:#}"));
        assert_eq!(
            typed.code().map(|code| code.to_string()),
            Some("rumoca::codegen::EC009".to_owned())
        );
        assert!(matches!(
            typed,
            rumoca_phase_codegen::CodegenError::UnsupportedTargetFeature {
                target,
                feature: "continuous_states",
                detail,
                span: Some(span),
            } if target == target_id
                && detail == "1 state(s); not yet supported by the Rumoca GALEC projection"
                && *span == state_span
        ));
    }
}

#[test]
fn capability_refusal_precedes_malformed_completion_messages() {
    use miette::Diagnostic as _;
    use std::collections::BTreeMap;

    fn refusal_order_target(
        capabilities: &str,
    ) -> rumoca_compile::codegen::targets::CheckedTargetBundle {
        let manifest = format!(
            r#"
version = 1
name = "refusal-order"
readiness_level = 1
completion_message = "{{% broken"

[arithmetic]
source_real = "binary64"
source_integer = "i32"
real_matrix_multiply = "separate_mul_add_ascending_positive_zero"

[capabilities]
scalar_fallback = false
{capabilities}

[[files]]
artifact_kind = "algorithm-code"
semantic_context = "galec"
path = "model.alg"
template = "model.alg.jinja"
"#
        );
        rumoca_compile::codegen::targets::TargetBundle::check_in_memory(
            "refusal-order".to_owned(),
            manifest,
            BTreeMap::from([(
                "model.alg.jinja".to_owned(),
                "{{ algorithm_code }}".to_owned(),
            )]),
            BTreeMap::new(),
        )
        .expect("refusal-order target checks; its completion message is prepared only at render")
    }

    let compilation = compile_unit_derivative_strict();

    // Control arm: with continuous states admitted by the contract, the
    // GALEC projection itself refuses UnitDerivative during pre-session
    // semantic lowering, so the refusal cites the bound target label and the
    // malformed completion message is never reached. This witnesses that
    // semantic lowering precedes completion-message preparation. The proof
    // that this exact message text is genuinely malformed lives in the FMI
    // sibling test below, whose product admits and reaches presentation.
    let error = compilation
        .render_target(refusal_order_target(""), pinned_artifact_input())
        .err()
        .expect("the GALEC projection must refuse continuous dynamics");
    assert!(
        error
            .downcast_ref::<rumoca_phase_galec::GalecTargetErrors>()
            .is_some(),
        "pre-session lowering must surface the typed GALEC refusal: {error:#}"
    );
    let chain = format!("{error:#}");
    assert!(
        chain.contains("GALEC projection rejected target 'refusal-order'"),
        "the projection refusal must cite the admitted target identity: {chain}"
    );
    assert!(
        !chain.contains("completion_message"),
        "completion-message preparation must not run before lowering: {chain}"
    );

    // Refusal arm: with continuous states excluded, EC009 must win over the
    // malformed completion message because capability admission precedes
    // artifact-session and completion-presentation construction. The typed
    // capability gate retains the owning GALEC-projection scope required by
    // GAL-025 rather than attributing the limitation to eFMI.
    let error = compilation
        .render_target(
            refusal_order_target("continuous_states = false"),
            pinned_artifact_input(),
        )
        .err()
        .expect("continuous UnitDerivative must be refused by capabilities");
    let typed = error
        .downcast_ref::<rumoca_phase_codegen::CodegenError>()
        .unwrap_or_else(|| panic!("capability refusal retains CodegenError: {error:#}"));
    assert_eq!(
        typed.code().map(|code| code.to_string()),
        Some("rumoca::codegen::EC009".to_owned()),
        "capability refusal must precede completion-message preparation"
    );
    assert!(matches!(
        typed,
        rumoca_phase_codegen::CodegenError::UnsupportedTargetFeature {
            target,
            feature: "continuous_states",
            detail,
            span: Some(_),
        } if target == "refusal-order"
            && detail == "1 state(s); not yet supported by the Rumoca GALEC projection"
    ));
}

#[test]
fn fmi_capability_refusal_precedes_malformed_completion_messages() {
    use miette::Diagnostic as _;
    use std::collections::BTreeMap;

    fn fmi_refusal_order_target(
        capabilities: &str,
    ) -> rumoca_compile::codegen::targets::CheckedTargetBundle {
        let manifest = format!(
            r#"
version = 1
name = "fmi-refusal-order"
readiness_level = 1
completion_message = "{{% broken"

[capabilities]
scalar_fallback = false
{capabilities}

[package]
root = "out"

[[package.members]]
kind = "file"
file = "fmu"

[[files]]
id = "fmu"
artifact_kind = "xml"
semantic_context = "solve"
view = "fmi-component"
path = "modelDescription.xml"
template = "modelDescription.xml.jinja"
"#
        );
        rumoca_compile::codegen::targets::TargetBundle::check_in_memory(
            "fmi-refusal-order".to_owned(),
            manifest,
            BTreeMap::from([("modelDescription.xml.jinja".to_owned(), "ok".to_owned())]),
            BTreeMap::new(),
        )
        .expect("fmi-refusal-order target checks; its completion message is prepared at render")
    }

    let compilation = compile_unit_derivative_strict();

    // Control arm: with continuous states admitted, the FMI component
    // admission succeeds for UnitDerivative and the malformed completion
    // message is the first refusal, proving the message genuinely fails to
    // prepare on the direct FMI route as well.
    let error = compilation
        .render_target(fmi_refusal_order_target(""), pinned_artifact_input())
        .err()
        .expect("a malformed completion message must refuse FMI rendering");
    let control = error
        .downcast_ref::<rumoca_phase_codegen::CodegenError>()
        .unwrap_or_else(|| panic!("completion-message refusal retains CodegenError: {error:#}"));
    assert_ne!(
        control.code().map(|code| code.to_string()),
        Some("rumoca::codegen::EC009".to_owned()),
        "the FMI control arm must fail on the completion message, not on capabilities"
    );
    assert!(
        format!("{error:#}").contains("completion_message"),
        "the FMI control arm must name the malformed completion message: {error:#}"
    );

    // Refusal arm: the direct FMI product admits its capability contract
    // before artifact-session and completion-presentation construction, so
    // EC009 wins over the malformed completion message on the same route the
    // one-model FMI proof artifact takes.
    let error = compilation
        .render_target(
            fmi_refusal_order_target("continuous_states = false"),
            pinned_artifact_input(),
        )
        .err()
        .expect("continuous UnitDerivative must be refused by FMI capabilities");
    let typed = error
        .downcast_ref::<rumoca_phase_codegen::CodegenError>()
        .unwrap_or_else(|| panic!("FMI capability refusal retains CodegenError: {error:#}"));
    assert_eq!(
        typed.code().map(|code| code.to_string()),
        Some("rumoca::codegen::EC009".to_owned()),
        "FMI capability refusal must precede completion-message preparation"
    );
    assert!(matches!(
        typed,
        rumoca_phase_codegen::CodegenError::UnsupportedTargetFeature {
            target,
            feature: "continuous_states",
            detail,
            span: Some(_),
        } if target == "fmi-refusal-order" && detail == "1 state(s)"
    ));
}

#[test]
fn unit_derivative_with_modelica_default_fixed_false_is_rejected() {
    let mut session = Session::new(SessionConfig {
        parallel: false,
        instantiation_depth_limit: rumoca_phase_instantiate::DEFAULT_INSTANTIATION_DEPTH_LIMIT,
    });
    session
        .add_document(
            "unit_derivative_without_initial_equation.mo",
            r#"
model UnitDerivative
  Real x(start = 0.0);
equation
  der(x) = 1.0;
end UnitDerivative;
"#,
        )
        .expect("the initialization mutation is syntactically valid");
    let compilation = session
        .compile_model_strict("UnitDerivative")
        .unwrap_or_else(|report| panic!("the source mutation must reach checked DAE: {report:?}"));
    let declaration_span = compilation.result().dae.inspect(|view| {
        let (_, variable) = view.variables().next().expect("one checked variable");
        assert_eq!(variable.fixed(), rumoca_core::Fixity::Free);
        assert_eq!(variable.role(), VariableRole::State);
        variable.declaration().span()
    });
    let attempted = rumoca_phase_solve::lower_solve_model(
        &compilation.result().dae,
        &std::collections::HashMap::new(),
        |_| {},
    );
    let Err(rumoca_phase_solve::SolveModelLoweringError::Lower(
        rumoca_phase_solve::LowerError::NonComputable { span, .. },
    )) = attempted
    else {
        panic!(
            "default fixed=false without an initial equation must not become executable Solve IR"
        );
    };
    assert_eq!(span, declaration_span);
}

/// A runtime-only function must not poison the unrelated pre-Flat evaluation
/// that proves `n = 2`. The function itself and its runtime call must still be
/// retained through final DAE construction.
#[test]
fn runtime_function_does_not_poison_parameter_evaluation() {
    let result = compile_model(
        "runtime_function_parameter.mo",
        r#"
function runtimeOnly
  input Real u;
  output Real y;
algorithm
  y := u*u + 3.0;
end runtimeOnly;

model RuntimeFunctionParameter
  final parameter Integer n = 1 + 1;
  Real samples[n];
  Real y;
equation
  for i in 1:n loop
    samples[i] = i*time;
  end for;
  y = runtimeOnly(time);
end RuntimeFunctionParameter;
"#,
        "RuntimeFunctionParameter",
    );

    result.dae.inspect(|view| {
        let samples = view
            .variables()
            .map(|(_, variable)| variable)
            .find(|variable| variable.name().as_str() == "samples")
            .expect("the parameter-sized array remains in checked DAE");
        assert_eq!(samples.scalar_count(), 2);

        let function = (0..view.function_count())
            .filter_map(|index| view.function_id(index))
            .filter_map(|id| view.function(id))
            .find(|function| function.name().as_str().ends_with("runtimeOnly"))
            .expect("the runtime-only function remains in checked DAE");
        assert!(function.definition_count() > 0);
        let function_id = function.id();
        let calls = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .filter_map(|id| view.expression(id))
            .filter(|expression| {
                matches!(
                    expression.operation(),
                    rumoca_ir_dae::ExpressionOperation::Call { function, .. }
                        if function == function_id
                )
            })
            .count();
        assert_eq!(calls, 1, "the runtime call must not be dropped or folded");
    });
}

#[test]
fn checked_dae_round_trips_wire_v11_and_preserves_source_text() {
    let result = compile_decay();
    let encoded = serde_json::to_string(&result.dae).expect("wire-v11 encoding succeeds");
    let decoded: rumoca_compile::compile::Dae =
        serde_json::from_str(&encoded).expect("wire-v11 decodes through checked construction");

    decoded.inspect(|view| {
        assert_eq!(view.continuous_owner_count(), 1);
        let equation = view
            .continuous_equation(0)
            .expect("checked continuous equation exists");
        assert_eq!(view.source_text(equation.provenance()), Some("der(x)=-x"));
    });
}

#[test]
fn checked_dae_flows_through_structural_and_solve_phases() {
    let result = compile_decay();
    result.dae.inspect(|view| {
        let structural = rumoca_compile::phase_structural::sort(view)
            .expect("checked DAE has a perfect structural matching");
        assert_eq!(structural.matching.len(), 1);
    });

    let solve = rumoca_sim::lower_solve_problem(&result.dae)
        .expect("checked DAE lowers to computable Solve IR");
    assert_eq!(solve.layout().y_scalars(), 1);
}

#[test]
fn codegen_context_is_the_checked_semantic_projection() {
    let result = compile_decay();
    let json = rumoca_compile::codegen::dae_to_template_json(&result.dae)
        .expect("checked DAE template projection serializes");

    assert_eq!(json["schema"]["name"], "rumoca.checked-dae-template");
    // Pinned to `dae_backend::TEMPLATE_SCHEMA_VERSION`: every change to the
    // projected template shape bumps that constant, and this literal must be
    // bumped with it so template consumers see the break loudly. Version 5 is
    // the shape carrying checked function owners, checked discrete ownership,
    // and the proved-projection gate.
    assert_eq!(json["schema"]["version"], 5);
    assert!(json["value_types"].is_array());
    assert!(json["variables"].is_array());
    assert!(json["expressions"].is_array());
    assert!(json["systems"]["continuous"]["owners"].is_array());
    assert!(
        json.get("storage").is_none(),
        "templates consume the checked semantic projection, not raw wire records"
    );
}
