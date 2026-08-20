use super::{
    TargetFeatureSupport, TargetManifest, TargetTemplateIr, TensorCapability,
    TensorLayoutCapability, builtin_target_compatibility_matrix, parse_target_manifest,
    safe_target_join, target_asset_relative_path, templates, validate_dae_target_capabilities,
    validate_solve_target_capabilities, validate_target_manifest,
};
use rumoca_core::{SourceMap, StructuredIndexBinder, StructuredIndexDomain, VarName};
use rumoca_ir_dae::{
    CoordinateInput, Dae, DaeLiteral, DaeProvenance, ExternalArgument, ExternalFunctionBody,
    ExternalLanguage, ExternalLinkage, FunctionPurity, FunctionSignature, ScalarType, ValueType,
};
use std::path::Path;

fn dae_with_placeholder_family() -> Dae {
    let source_text = "for i in 1:4 loop 0.0 = 0.0; end for;";
    let mut source_map = SourceMap::new();
    let source_id = source_map.add("target-capability.mo", source_text);
    let owner = DaeProvenance::source(rumoca_core::Span::from_offsets(
        source_id,
        0,
        source_text.len(),
    ))
    .expect("fixture source span is exact");
    Dae::construct(source_map, |dae| {
        let domain = dae.domains(|domains| {
            domains.structured(
                StructuredIndexDomain {
                    binders: vec![StructuredIndexBinder {
                        id: 0,
                        display_name: "i".to_string(),
                        lower: 1,
                        upper: 4,
                        step: 1,
                    }],
                },
                owner,
            )
        })?;
        let residual =
            dae.expressions(|expressions| expressions.at(owner).literal(DaeLiteral::Real(0.0)))?;
        dae.continuous(|equations| {
            equations.structured_family(
                owner,
                domain,
                rumoca_core::ComprehensionScalarView::BinderSubstitution,
                |family| family.body(residual),
            )
        })?;
        Ok(())
    })
    .expect("checked structured-family fixture is valid")
}

/// One checked DAE whose only function is an MLS §12.9 external interface:
/// `pure function f input Real u; output Real y; external "C" y = my_func(u); end f;`
///
/// SEV-155's witness input: the `external_functions` capability gate must fire
/// on this DAE.
fn dae_with_external_function() -> Dae {
    let source_text =
        "pure function f input Real u; output Real y; external \"C\" y = my_func(u); end f;";
    let mut source_map = SourceMap::new();
    let source_id = source_map.add("external-capability.mo", source_text);
    let at = DaeProvenance::source(rumoca_core::Span::from_offsets(
        source_id,
        0,
        source_text.len(),
    ))
    .expect("fixture source span is exact");
    Dae::construct(source_map, |dae| {
        let real = dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), at))?;
        dae.function(
            FunctionSignature::new(VarName::new("f"), [real], [real], at),
            |dae, reservation| {
                let parameter = dae.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("u"), 0, at)
                })?;
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, at)
                })?;
                let argument = dae.expressions(|expressions| {
                    expressions
                        .at(at)
                        .coordinate(CoordinateInput::FunctionParameter(parameter))
                })?;
                let body = ExternalFunctionBody::new(
                    FunctionPurity::Pure,
                    ExternalLanguage::C,
                    VarName::new("my_func"),
                    [ExternalArgument::Input(argument)],
                    Some(output),
                    ExternalLinkage::new(["ModelicaExternalC".to_string()], None, None, None),
                );
                dae.functions(|functions| functions.define_external(reservation, body, at))
            },
        )
        .map(|_| ())
    })
    .expect("a checked external interface defines its reserved function")
}

/// The mirror fixture: the same one-function DAE with a *Modelica* body,
/// `function f input Real u; output Real y; algorithm y := u; end f;`.
///
/// It is what keeps the SEV-155 probe from being satisfied by a constant
/// `true`: the checked function table is nonempty, so only a real per-function
/// body test tells the two fixtures apart.
fn dae_with_modelica_function() -> Dae {
    let source_text = "function f input Real u; output Real y; algorithm y := u; end f;";
    let mut source_map = SourceMap::new();
    let source_id = source_map.add("modelica-function.mo", source_text);
    let at = DaeProvenance::source(rumoca_core::Span::from_offsets(
        source_id,
        0,
        source_text.len(),
    ))
    .expect("fixture source span is exact");
    Dae::construct(source_map, |dae| {
        let real = dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), at))?;
        dae.function(
            FunctionSignature::new(VarName::new("f"), [real], [real], at),
            |dae, reservation| {
                let parameter = dae.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("u"), 0, at)
                })?;
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, at)
                })?;
                let value = dae.expressions(|expressions| {
                    expressions
                        .at(at)
                        .coordinate(CoordinateInput::FunctionParameter(parameter))
                })?;
                let mut body = dae.functions(|functions| functions.begin(reservation, at))?;
                dae.functions(|functions| functions.assign(&mut body, output, value, at))?;
                dae.functions(|functions| functions.define(body, at))
            },
        )
        .map(|_| ())
    })
    .expect("a checked Modelica function body defines its reserved function")
}

fn manifest_with_capabilities(capabilities: &str) -> TargetManifest {
    toml::from_str(&format!(
        r#"
version = 1
ir = "dae"
name = "custom"
readiness_level = 3

{capabilities}

[[files]]
path = "model.out"
template = "model.out.jinja"
"#
    ))
    .expect("parse target manifest")
}

fn parse_manifest_with_ir_capabilities(ir: &str, capabilities: &str) -> TargetManifest {
    super::parse_target_manifest(&format!(
        r#"
version = 1
ir = "{ir}"
name = "custom"
readiness_level = 1

{capabilities}

[[files]]
path = "model.out"
template = "model.out.jinja"
"#
    ))
    .expect("parse and validate target manifest")
}

fn solve_with_issued_exact_algebraic_assignment() -> rumoca_ir_solve::SolveProblem {
    use rumoca_ir_solve as solve;

    let implicit_rhs = exact_algebraic_fixture_source();
    let row = solve::AlgebraicRefreshRow::checked(solve::AlgebraicRefreshRowDraft {
        owner_id: solve::RefreshRowOwnerId::checked(0).expect("fixture owner fits"),
        source: solve::RefreshScalarProgramSource::checked(0, 0).expect("fixture source fits"),
        equation_index: 0,
        output_offset: 0,
        target_index: 0,
        assignment_target: Some(0),
        assignment_shape: Some(solve::TargetAssignmentShape::Direct {
            target_y_index: 0,
            expr_reg: 1,
            target_scale: 1.0,
            expr_eval_len: 2,
        }),
        direct_assignment_certified: true,
        exact_assignment_certified: true,
    })
    .expect("fixture refresh row is valid");
    let algebraic = solve::RefreshPlan {
        simultaneous_plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![0],
                y_indices: vec![0],
            }],
        },
        simultaneous_block_indices: vec![0],
        rows: vec![row],
        causal_seed_rows: solve::RefreshRowSelection::checked(1, [0])
            .expect("fixture selection is valid"),
        dynamic_causal_seed_rows: solve::RefreshRowSelection::checked(1, [0])
            .expect("fixture selection is valid"),
        value_stages: vec![solve::RefreshStage::ExactAssignments {
            static_sequence: Default::default(),
            dynamic_sequence: Default::default(),
            static_rows: solve::RefreshRowSelection::default(),
            dynamic_rows: solve::RefreshRowSelection::checked(1, [0])
                .expect("fixture selection is valid"),
        }],
        causal_solution_certified: true,
        ..solve::RefreshPlan::default()
    };
    let refresh_owners = solve::ContinuousRefreshOwners::checked_for_source(
        &implicit_rhs,
        algebraic,
        solve::RefreshPlan::default(),
        solve::RefreshPlan::default(),
        solve::RefreshPlan::default(),
        Vec::new(),
    )
    .expect("fixture refresh owners are valid");
    let solve_layout = solve::SolveLayout {
        solver_maps: solve::SolverNameIndexMaps {
            names: vec!["y".to_string()],
            name_to_idx: indexmap::IndexMap::from([("y".to_string(), 0)]),
            base_to_indices: indexmap::IndexMap::from([("y".to_string(), vec![0])]),
        },
        algebraic_scalar_count: 1,
        ..solve::SolveLayout::default()
    };
    let continuous = solve::ContinuousSolveSystem {
        implicit_rhs: implicit_rhs.clone(),
        residual: implicit_rhs,
        implicit_row_targets: vec![Some(solve::ScalarSlot::Y {
            index: 0,
            byte_offset: 0,
        })],
        algebraic_projection_plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![0],
                y_indices: vec![0],
            }],
        },
        refresh_owners,
        ..solve::ContinuousSolveSystem::default()
    };
    solve::SolveProblem {
        layout: solve::VarLayout::from_parts(indexmap::IndexMap::new(), 1, 0),
        solve_layout,
        continuous,
        ..solve::SolveProblem::default()
    }
}

fn exact_algebraic_fixture_source() -> rumoca_ir_solve::ComputeBlock {
    use rumoca_ir_solve as solve;

    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("exact-algebraic-target.mo"),
        0,
        1,
    );
    let provenance = rumoca_core::ProvenanceSpan::new(span, "exact algebraic target fixture")
        .expect("fixture provenance is valid");
    solve::ComputeBlock::from_scalar_program_block(
        solve::ScalarProgramBlock::with_source_span(
            vec![vec![
                solve::LinearOp::LoadY { dst: 0, index: 0 },
                solve::LinearOp::Const { dst: 1, value: 1.0 },
                solve::LinearOp::Binary {
                    dst: 2,
                    op: solve::BinaryOp::Sub,
                    lhs: 0,
                    rhs: 1,
                },
                solve::LinearOp::StoreOutput { src: 2 },
            ]],
            provenance,
        )
        .expect("fixture scalar program is valid"),
    )
}

fn unrelated_single_output_block() -> rumoca_ir_solve::ComputeBlock {
    use rumoca_ir_solve as solve;

    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("forged-exact-algebraic.mo"),
        0,
        1,
    );
    let provenance = rumoca_core::ProvenanceSpan::new(span, "forged exact algebraic fixture")
        .expect("fixture provenance is valid");
    solve::ComputeBlock::from_scalar_program_block(
        solve::ScalarProgramBlock::with_source_span(
            vec![vec![
                solve::LinearOp::Const { dst: 0, value: 7.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]],
            provenance,
        )
        .expect("fixture scalar program is valid"),
    )
}

fn block_with_unowned_second_output() -> rumoca_ir_solve::ComputeBlock {
    use rumoca_ir_solve as solve;

    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("unowned-exact-algebraic.mo"),
        0,
        1,
    );
    let provenance = rumoca_core::ProvenanceSpan::new(span, "unowned exact algebraic fixture")
        .expect("fixture provenance is valid");
    solve::ComputeBlock::from_scalar_program_block(
        solve::ScalarProgramBlock::with_source_span(
            vec![
                vec![
                    solve::LinearOp::LoadY { dst: 0, index: 0 },
                    solve::LinearOp::Const { dst: 1, value: 1.0 },
                    solve::LinearOp::Binary {
                        dst: 2,
                        op: solve::BinaryOp::Sub,
                        lhs: 0,
                        rhs: 1,
                    },
                    solve::LinearOp::StoreOutput { src: 2 },
                ],
                vec![
                    solve::LinearOp::Const { dst: 0, value: 9.0 },
                    solve::LinearOp::StoreOutput { src: 0 },
                ],
            ],
            provenance,
        )
        .expect("fixture scalar programs are valid"),
    )
}

fn post_output_redefinition_block() -> rumoca_ir_solve::ComputeBlock {
    use rumoca_ir_solve as solve;

    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("post-output-redefinition.mo"),
        0,
        1,
    );
    let provenance = rumoca_core::ProvenanceSpan::new(span, "post-output redefinition fixture")
        .expect("fixture provenance is valid");
    solve::ComputeBlock::from_scalar_program_block(
        solve::ScalarProgramBlock::with_source_span(
            vec![vec![
                solve::LinearOp::LoadY { dst: 0, index: 0 },
                solve::LinearOp::Const { dst: 1, value: 1.0 },
                solve::LinearOp::Binary {
                    dst: 2,
                    op: solve::BinaryOp::Sub,
                    lhs: 0,
                    rhs: 1,
                },
                solve::LinearOp::StoreOutput { src: 2 },
                solve::LinearOp::Const { dst: 1, value: 2.0 },
            ]],
            provenance,
        )
        .expect("fixture scalar program is valid"),
    )
}

fn dependent_algebraic_fixture_source() -> rumoca_ir_solve::ComputeBlock {
    use rumoca_ir_solve as solve;

    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("dependent-algebraic-wire.mo"),
        0,
        1,
    );
    let provenance = rumoca_core::ProvenanceSpan::new(span, "dependent algebraic wire fixture")
        .expect("fixture provenance is valid");
    solve::ComputeBlock::from_scalar_program_block(
        solve::ScalarProgramBlock::with_source_span(
            vec![
                vec![
                    solve::LinearOp::LoadY { dst: 0, index: 0 },
                    solve::LinearOp::Const { dst: 1, value: 2.0 },
                    solve::LinearOp::Binary {
                        dst: 2,
                        op: solve::BinaryOp::Sub,
                        lhs: 0,
                        rhs: 1,
                    },
                    solve::LinearOp::StoreOutput { src: 2 },
                ],
                vec![
                    solve::LinearOp::LoadY { dst: 0, index: 1 },
                    solve::LinearOp::LoadY { dst: 1, index: 0 },
                    solve::LinearOp::Binary {
                        dst: 2,
                        op: solve::BinaryOp::Sub,
                        lhs: 0,
                        rhs: 1,
                    },
                    solve::LinearOp::StoreOutput { src: 2 },
                ],
            ],
            provenance,
        )
        .expect("fixture scalar programs are valid"),
    )
}

fn dependent_algebraic_fixture_row(
    ordinal: usize,
    expression: u32,
) -> rumoca_ir_solve::AlgebraicRefreshRow {
    use rumoca_ir_solve as solve;

    solve::AlgebraicRefreshRow::checked(solve::AlgebraicRefreshRowDraft {
        owner_id: solve::RefreshRowOwnerId::checked(ordinal).expect("fixture owner fits"),
        source: solve::RefreshScalarProgramSource::checked(0, ordinal)
            .expect("fixture source fits"),
        equation_index: ordinal,
        output_offset: 0,
        target_index: ordinal,
        assignment_target: Some(ordinal),
        assignment_shape: Some(solve::TargetAssignmentShape::Direct {
            target_y_index: ordinal,
            expr_reg: expression,
            target_scale: 1.0,
            expr_eval_len: 2,
        }),
        direct_assignment_certified: true,
        exact_assignment_certified: true,
    })
    .expect("fixture refresh row is valid")
}

fn solve_with_dependent_algebraic_assignments() -> rumoca_ir_solve::SolveProblem {
    use rumoca_ir_solve as solve;

    let implicit_rhs = dependent_algebraic_fixture_source();
    let projection = solve::AlgebraicProjectionPlan {
        blocks: vec![
            solve::AlgebraicProjectionBlock {
                rows: vec![0],
                y_indices: vec![0],
            },
            solve::AlgebraicProjectionBlock {
                rows: vec![1],
                y_indices: vec![1],
            },
        ],
    };
    let algebraic = solve::RefreshPlan {
        simultaneous_plan: projection.clone(),
        simultaneous_block_indices: vec![0, 1],
        rows: vec![
            dependent_algebraic_fixture_row(0, 1),
            dependent_algebraic_fixture_row(1, 1),
        ],
        causal_seed_rows: solve::RefreshRowSelection::checked(2, [0, 1])
            .expect("fixture selection is valid"),
        dynamic_causal_seed_rows: solve::RefreshRowSelection::checked(2, [0, 1])
            .expect("fixture selection is valid"),
        value_stages: vec![solve::RefreshStage::ExactAssignments {
            static_sequence: Default::default(),
            dynamic_sequence: Default::default(),
            static_rows: solve::RefreshRowSelection::default(),
            dynamic_rows: solve::RefreshRowSelection::checked(2, [0, 1])
                .expect("fixture selection is valid"),
        }],
        causal_solution_certified: true,
        ..solve::RefreshPlan::default()
    };
    let refresh_owners = solve::ContinuousRefreshOwners::checked_for_source(
        &implicit_rhs,
        algebraic,
        solve::RefreshPlan::default(),
        solve::RefreshPlan::default(),
        solve::RefreshPlan::default(),
        Vec::new(),
    )
    .expect("fixture refresh owners are internally consistent");
    let solve_layout = solve::SolveLayout {
        solver_maps: solve::SolverNameIndexMaps {
            names: vec!["y0".to_string(), "y1".to_string()],
            name_to_idx: indexmap::IndexMap::from([("y0".to_string(), 0), ("y1".to_string(), 1)]),
            base_to_indices: indexmap::IndexMap::from([
                ("y0".to_string(), vec![0]),
                ("y1".to_string(), vec![1]),
            ]),
        },
        algebraic_scalar_count: 2,
        ..solve::SolveLayout::default()
    };
    let continuous = solve::ContinuousSolveSystem {
        implicit_rhs: implicit_rhs.clone(),
        residual: implicit_rhs,
        implicit_row_targets: vec![
            Some(solve::ScalarSlot::Y {
                index: 0,
                byte_offset: 0,
            }),
            Some(solve::ScalarSlot::Y {
                index: 1,
                byte_offset: 8,
            }),
        ],
        algebraic_projection_plan: projection,
        refresh_owners,
        ..solve::ContinuousSolveSystem::default()
    };
    solve::SolveProblem {
        layout: solve::VarLayout::from_parts(indexmap::IndexMap::new(), 2, 0),
        solve_layout,
        continuous,
        ..solve::SolveProblem::default()
    }
}

fn pre_output_redefinition_block() -> rumoca_ir_solve::ComputeBlock {
    use rumoca_ir_solve as solve;

    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("pre-output-redefinition.mo"),
        0,
        1,
    );
    let provenance = rumoca_core::ProvenanceSpan::new(span, "pre-output redefinition fixture")
        .expect("fixture provenance is valid");
    solve::ComputeBlock::from_scalar_program_block(
        solve::ScalarProgramBlock::with_source_span(
            vec![vec![
                solve::LinearOp::LoadY { dst: 0, index: 0 },
                solve::LinearOp::Const { dst: 0, value: 7.0 },
                solve::LinearOp::Const { dst: 1, value: 1.0 },
                solve::LinearOp::Binary {
                    dst: 2,
                    op: solve::BinaryOp::Sub,
                    lhs: 0,
                    rhs: 1,
                },
                solve::LinearOp::StoreOutput { src: 2 },
            ]],
            provenance,
        )
        .expect("fixture scalar program is valid"),
    )
}

#[test]
fn exact_algebraic_assignment_completeness_is_rederived_on_wire_replay() {
    let problem = solve_with_issued_exact_algebraic_assignment();
    assert!(rumoca_phase_codegen::explicit_algebraic_assignment_complete(&problem));

    let json = serde_json::to_string(&problem).expect("serialize exact algebraic Solve problem");
    assert!(!json.contains("algebraic_assignment_complete"));
    let replayed_json: rumoca_ir_solve::SolveProblem =
        serde_json::from_str(&json).expect("replay exact algebraic Solve problem from JSON");
    assert!(rumoca_phase_codegen::explicit_algebraic_assignment_complete(&replayed_json));

    let bytes = bincode::serialize(&problem).expect("serialize exact algebraic Solve problem");
    let replayed_binary: rumoca_ir_solve::SolveProblem =
        bincode::deserialize(&bytes).expect("replay exact algebraic Solve problem from bincode");
    assert!(rumoca_phase_codegen::explicit_algebraic_assignment_complete(&replayed_binary));
}

#[test]
fn solve_wire_rejects_a_forged_exact_assignment_register() {
    let mut wire = serde_json::to_value(solve_with_issued_exact_algebraic_assignment())
        .expect("serialize exact algebraic Solve problem");
    wire["continuous"]["refresh_owners"]["algebraic"]["rows"][0]["assignment_shape"]["Direct"]["expr_reg"] =
        serde_json::json!(0);

    let error = serde_json::from_value::<rumoca_ir_solve::SolveProblem>(wire)
        .expect_err("wire replay must rederive the assignment shape from its canonical source");
    assert!(
        error
            .to_string()
            .contains("assignment certificate disagrees"),
        "unexpected replay error: {error}"
    );
}

#[test]
fn solve_wire_rejects_forged_direct_assignment_metadata() {
    for (field, value, expected) in [
        (
            "expr_eval_len",
            serde_json::json!(3),
            "assignment certificate disagrees",
        ),
        (
            "target_scale",
            serde_json::json!(-1.0),
            "assignment certificate disagrees",
        ),
        (
            "target_y_index",
            serde_json::json!(1),
            "assignment certificate belongs to another target",
        ),
    ] {
        let mut wire = serde_json::to_value(solve_with_issued_exact_algebraic_assignment())
            .expect("serialize exact algebraic Solve problem");
        wire["continuous"]["refresh_owners"]["algebraic"]["rows"][0]["assignment_shape"]["Direct"]
            [field] = value;

        let error = serde_json::from_value::<rumoca_ir_solve::SolveProblem>(wire)
            .expect_err("wire replay must reject every forged Direct certificate field");
        assert!(
            error.to_string().contains(expected),
            "forged {field} returned the wrong replay error: {error}"
        );
    }
}

#[test]
fn exact_assignment_consumer_rejects_a_certificate_for_another_source() {
    let mut solve = solve_with_issued_exact_algebraic_assignment();
    solve.continuous.implicit_rhs = unrelated_single_output_block();
    let target = templates::builtin_targets()
        .iter()
        .find(|target| target.name == "fmi3")
        .expect("fmi3 target is built in");
    let manifest = parse_target_manifest(target.manifest).expect("fmi3 manifest is valid");
    let capabilities = manifest.capabilities.as_ref().expect("capabilities");

    let error = validate_solve_target_capabilities(&solve, &manifest, capabilities)
        .expect_err("an assignment certificate cannot outlive its canonical source");
    assert!(
        error
            .to_string()
            .contains("assignment certificate disagrees")
    );
}

#[test]
fn exact_assignment_consumer_uses_the_definition_reaching_the_output() {
    let mut solve = solve_with_issued_exact_algebraic_assignment();
    solve.continuous.implicit_rhs = post_output_redefinition_block();
    let target = templates::builtin_targets()
        .iter()
        .find(|target| target.name == "fmi3")
        .expect("fmi3 target is built in");
    let manifest = parse_target_manifest(target.manifest).expect("fmi3 manifest is valid");
    let capabilities = manifest.capabilities.as_ref().expect("capabilities");

    validate_solve_target_capabilities(&solve, &manifest, capabilities)
        .expect("a later register rewrite cannot change the certified output value");
}

#[test]
fn exact_assignment_consumer_rejects_a_pre_output_register_redefinition() {
    let mut solve = solve_with_issued_exact_algebraic_assignment();
    solve.continuous.implicit_rhs = pre_output_redefinition_block();
    let target = templates::builtin_targets()
        .iter()
        .find(|target| target.name == "fmi3")
        .expect("fmi3 target is built in");
    let manifest = parse_target_manifest(target.manifest).expect("fmi3 manifest is valid");
    let capabilities = manifest.capabilities.as_ref().expect("capabilities");

    let error = validate_solve_target_capabilities(&solve, &manifest, capabilities)
        .expect_err("overlapping register writes cannot certify an explicit assignment");
    assert!(
        error
            .to_string()
            .contains("assignment certificate disagrees")
    );
}

#[test]
fn exact_assignment_consumer_rejects_reordered_dependent_algebraics_after_replay() {
    let mut wire = serde_json::to_value(solve_with_dependent_algebraic_assignments())
        .expect("serialize the causal exact schedule");
    wire["continuous"]["algebraic_projection_plan"]["blocks"]
        .as_array_mut()
        .expect("root projection is an array")
        .reverse();
    wire["continuous"]["refresh_owners"]["algebraic"]["simultaneous_plan"]["blocks"]
        .as_array_mut()
        .expect("owner projection is an array")
        .reverse();
    for selection in ["causal_seed_rows", "dynamic_causal_seed_rows"] {
        wire["continuous"]["refresh_owners"]["algebraic"][selection] = serde_json::json!([1, 0]);
    }
    wire["continuous"]["refresh_owners"]["algebraic"]["value_stages"][0]["ExactAssignments"]["dynamic_rows"] =
        serde_json::json!([1, 0]);

    let error = serde_json::from_value::<rumoca_ir_solve::SolveProblem>(wire)
        .expect_err("wire replay must reject a non-causal exact schedule");
    assert!(error.to_string().contains("non-causal"));
}

#[test]
fn exact_assignment_consumer_accepts_causal_dependent_algebraics_after_replay() {
    let problem = solve_with_dependent_algebraic_assignments();
    assert!(rumoca_phase_codegen::explicit_algebraic_assignment_complete(&problem));

    let json = serde_json::to_string(&problem).expect("serialize the causal exact schedule");
    let replayed: rumoca_ir_solve::SolveProblem =
        serde_json::from_str(&json).expect("replay the causal exact schedule");
    assert!(rumoca_phase_codegen::explicit_algebraic_assignment_complete(&replayed));
}

#[test]
fn exact_assignment_consumer_rejects_an_unowned_implicit_output() {
    let mut solve = solve_with_issued_exact_algebraic_assignment();
    let implicit_rhs = block_with_unowned_second_output();
    solve.continuous.implicit_rhs = implicit_rhs.clone();
    solve.continuous.residual = implicit_rhs;
    solve.continuous.implicit_row_targets.push(None);
    let target = templates::builtin_targets()
        .iter()
        .find(|target| target.name == "fmi3")
        .expect("fmi3 target is built in");
    let manifest = parse_target_manifest(target.manifest).expect("fmi3 manifest is valid");
    let capabilities = manifest.capabilities.as_ref().expect("capabilities");

    let error = validate_solve_target_capabilities(&solve, &manifest, capabilities)
        .expect_err("every produced implicit output must have an authoritative owner");
    assert!(
        error
            .to_string()
            .contains("do not cover the required algebraic Y inventory")
    );
}

#[test]
fn exact_assignment_consumer_rejects_an_unissued_algebraic_suffix() {
    let solve = rumoca_ir_solve::SolveProblem {
        layout: rumoca_ir_solve::VarLayout::from_parts(indexmap::IndexMap::new(), 1, 0),
        solve_layout: rumoca_ir_solve::SolveLayout {
            solver_maps: rumoca_ir_solve::SolverNameIndexMaps {
                names: vec!["y".to_string()],
                name_to_idx: indexmap::IndexMap::from([("y".to_string(), 0)]),
                base_to_indices: indexmap::IndexMap::from([("y".to_string(), vec![0])]),
            },
            algebraic_scalar_count: 1,
            ..rumoca_ir_solve::SolveLayout::default()
        },
        ..rumoca_ir_solve::SolveProblem::default()
    };
    let target = templates::builtin_targets()
        .iter()
        .find(|target| target.name == "fmi3")
        .expect("fmi3 target is built in");
    let manifest = parse_target_manifest(target.manifest).expect("fmi3 manifest is valid");
    let capabilities = manifest.capabilities.as_ref().expect("capabilities");

    let error = validate_solve_target_capabilities(&solve, &manifest, capabilities)
        .expect_err("an algebraic suffix without an issued schedule must fail closed");
    assert!(
        error
            .to_string()
            .contains("do not cover the required algebraic Y inventory"),
        "unexpected unissued-algebraic error: {error:#}"
    );
}

#[test]
fn target_manifest_rejects_escaping_paths() {
    let root = Path::new("out");
    assert!(safe_target_join(root, "../escape").is_err());
    assert!(safe_target_join(root, "/absolute").is_err());
    assert_eq!(
        safe_target_join(root, "nested/file.c").unwrap(),
        root.join("nested/file.c")
    );
}

#[test]
fn target_manifest_parses_capabilities_table() {
    let manifest = manifest_with_capabilities(
        r#"
[capabilities]
external_functions = false
events = true
runtime_events = false
exact_algebraic_assignments = true
forward_ad = true
reverse_ad = false
dynamic_control_flow = true
host_callbacks = false
"#,
    );
    let capabilities = manifest.capabilities.expect("capabilities table");

    assert_eq!(manifest.readiness_level, Some(3));
    assert_eq!(capabilities.external_functions, Some(false));
    assert_eq!(capabilities.external_tables, None);
    assert_eq!(capabilities.events, Some(true));
    assert_eq!(capabilities.runtime_events, Some(false));
    assert_eq!(capabilities.exact_algebraic_assignments, Some(true));
    assert_eq!(capabilities.forward_ad, Some(true));
    assert_eq!(capabilities.reverse_ad, Some(false));
    assert_eq!(capabilities.dynamic_control_flow, Some(true));
    assert_eq!(capabilities.host_callbacks, Some(false));
}

#[test]
fn all_builtin_target_manifests_parse() {
    for target in templates::builtin_targets() {
        parse_target_manifest(target.manifest).unwrap_or_else(|err| {
            panic!("built-in target '{}' failed to parse: {err}", target.name)
        });
    }
}

#[test]
fn dae_target_rejects_exact_algebraic_assignment_capability() {
    let error = super::parse_target_manifest(
        r#"
version = 1
ir = "dae"
name = "invalid-dae-capability"
readiness_level = 1

[capabilities]
exact_algebraic_assignments = true

[[files]]
path = "model.out"
template = "model.out.jinja"
"#,
    )
    .expect_err("only Solve-derived targets may consume exact algebraic schedules");
    assert!(
        error
            .to_string()
            .contains("exact_algebraic_assignments capability is only valid")
    );
}

#[test]
fn builtin_structured_dae_capability_matches_template_consumption() {
    for target in templates::builtin_targets() {
        let manifest = parse_target_manifest(target.manifest).unwrap_or_else(|err| {
            panic!("built-in target '{}' failed to parse: {err}", target.name)
        });
        let family_aware = manifest
            .capabilities
            .as_ref()
            .and_then(|capabilities| capabilities.structured_equation_families)
            == Some(true);
        if !family_aware {
            continue;
        }
        for owner_path in [
            "dae.systems.continuous.owners",
            "dae.systems.initialization.owners",
        ] {
            assert!(
                target
                    .templates
                    .iter()
                    .any(|template| template.source.contains(owner_path)),
                "built-in target '{}' declares structured family ownership but no template \
                     consumes checked owner projection `{owner_path}`",
                target.name
            );
        }
        assert!(
            target
                .templates
                .iter()
                .all(|template| !template.source.contains("dae.f_x")),
            "built-in target '{}' declares structured family ownership but reads the removed \
                 scalar residual field",
            target.name
        );
    }
}

#[test]
fn builtin_dae_consumers_use_only_the_checked_template_schema() {
    fn dae_root_fields(source: &str) -> impl Iterator<Item = &str> {
        source.match_indices("dae.").filter_map(|(start, _)| {
            let field = &source[start + "dae.".len()..];
            let end = field
                .find(|character: char| !character.is_ascii_alphanumeric() && character != '_')
                .unwrap_or(field.len());
            (end != 0).then_some(&field[..end])
        })
    }

    const CHECKED_ROOT_FIELDS: &[&str] = &[
        "schema",
        "value_types",
        "variables",
        "functions",
        "domains",
        "expressions",
        "modelica",
        "systems",
    ];
    let mut offenders = Vec::new();
    for target in templates::builtin_targets() {
        let manifest = parse_target_manifest(target.manifest).unwrap_or_else(|error| {
            panic!("built-in target '{}' failed to parse: {error}", target.name)
        });
        if !matches!(
            manifest.ir,
            TargetTemplateIr::Dae
                | TargetTemplateIr::Solve
                | TargetTemplateIr::Fmi
                | TargetTemplateIr::AlgorithmCode
        ) {
            continue;
        }
        for template in target.templates {
            offenders.extend(
                dae_root_fields(template.source)
                    .filter(|field| !CHECKED_ROOT_FIELDS.contains(field))
                    .map(|field| format!("{}:{}:dae.{field}", target.name, template.path)),
            );
        }
    }
    assert!(
        offenders.is_empty(),
        "built-in templates still consume fields outside the checked DAE schema: \
             {offenders:#?}"
    );
}

#[test]
fn all_builtin_target_manifests_describe_matrix_axes() {
    for target in templates::builtin_targets() {
        let manifest = parse_target_manifest(target.manifest).unwrap_or_else(|err| {
            panic!("built-in target '{}' failed to parse: {err}", target.name)
        });
        assert!(
            manifest.execution_mode.is_some(),
            "built-in target '{}' must declare execution_mode",
            target.name
        );
        assert!(
            manifest.deployment_class.is_some(),
            "built-in target '{}' must declare deployment_class",
            target.name
        );
    }
}

#[test]
fn builtin_target_compatibility_matrix_reports_solve_tensor_fallback() {
    let matrix = builtin_target_compatibility_matrix()
        .expect("built-in target compatibility matrix should build");
    let c_ode = matrix
        .iter()
        .find(|entry| entry.id == "c-ode")
        .expect("c-ode target should be listed");
    assert_eq!(c_ode.ir, TargetTemplateIr::Solve);
    assert_eq!(c_ode.readiness_level, Some(2));
    assert_eq!(c_ode.scalar_programs, TargetFeatureSupport::Native);
    assert_eq!(c_ode.matmul, TargetFeatureSupport::Scalar);
    assert_eq!(c_ode.linsolve, TargetFeatureSupport::Scalar);
    assert_eq!(c_ode.elementwise, TargetFeatureSupport::Unknown);
    assert_eq!(c_ode.sparse, TargetFeatureSupport::Unsupported);
    assert_eq!(c_ode.dtypes, vec!["f64"]);
    assert_eq!(c_ode.events, TargetFeatureSupport::Unsupported);
    assert_eq!(c_ode.runtime_events, TargetFeatureSupport::Unsupported);
    assert_eq!(c_ode.forward_ad, TargetFeatureSupport::Unsupported);
    assert_eq!(c_ode.reverse_ad, TargetFeatureSupport::Unsupported);
    assert_eq!(
        c_ode.dynamic_control_flow,
        TargetFeatureSupport::Unsupported
    );
    assert_eq!(c_ode.host_callbacks, TargetFeatureSupport::Unsupported);

    let mlir = matrix
        .iter()
        .find(|entry| entry.id == "mlir")
        .expect("mlir target should be listed");
    assert_eq!(mlir.readiness_level, Some(1));
    assert_eq!(mlir.forward_ad, TargetFeatureSupport::Native);
    assert_eq!(mlir.reverse_ad, TargetFeatureSupport::Unsupported);

    let rust_ode = matrix
        .iter()
        .find(|entry| entry.id == "rust-ode")
        .expect("rust-ode target should be listed");
    assert_eq!(rust_ode.readiness_level, Some(2));
    assert_eq!(rust_ode.matmul, TargetFeatureSupport::Scalar);

    let rust_fixed_ode = matrix
        .iter()
        .find(|entry| entry.id == "rust-fixed-ode")
        .expect("rust-fixed-ode target should be listed");
    assert_eq!(rust_fixed_ode.readiness_level, Some(2));
    assert_eq!(rust_fixed_ode.deployment_class.as_deref(), Some("cpu"));
    assert_eq!(rust_fixed_ode.execution_mode.as_deref(), Some("compiled"));
    assert_eq!(rust_fixed_ode.matmul, TargetFeatureSupport::Scalar);
    assert_eq!(rust_fixed_ode.linsolve, TargetFeatureSupport::Unsupported);
    assert_eq!(rust_fixed_ode.sparse, TargetFeatureSupport::Unsupported);
    assert_eq!(rust_fixed_ode.dtypes, vec!["f64"]);

    let cuda_ode = matrix
        .iter()
        .find(|entry| entry.id == "cuda-ode")
        .expect("cuda-ode target should be listed");
    assert_eq!(cuda_ode.readiness_level, Some(1));
    assert_eq!(cuda_ode.deployment_class.as_deref(), Some("gpu"));
    assert_eq!(cuda_ode.matmul, TargetFeatureSupport::Scalar);
    assert_eq!(cuda_ode.linsolve, TargetFeatureSupport::Unsupported);
    assert_eq!(cuda_ode.sparse, TargetFeatureSupport::Unsupported);
    assert_eq!(cuda_ode.dtypes, vec!["f64"]);

    let wgsl_ode = matrix
        .iter()
        .find(|entry| entry.id == "wgsl-ode")
        .expect("wgsl-ode target should be listed");
    assert_eq!(wgsl_ode.readiness_level, Some(0));
    assert_eq!(wgsl_ode.deployment_class.as_deref(), Some("gpu"));
    assert_eq!(wgsl_ode.matmul, TargetFeatureSupport::Scalar);
    assert_eq!(wgsl_ode.elementwise, TargetFeatureSupport::Native);
    assert_eq!(wgsl_ode.stencil, TargetFeatureSupport::Native);
}

#[test]
fn removed_analysis_targets_stay_absent() {
    let matrix = builtin_target_compatibility_matrix()
        .expect("built-in target compatibility matrix should build");
    for removed in [
        "casadi-mx",
        "casadi-sx",
        "jax",
        "julia-mtk",
        "onnx",
        "symforce",
        "sympy",
    ] {
        assert!(
            matrix.iter().all(|entry| entry.id != removed),
            "target `{removed}` consumed the removed DAE template schema"
        );
    }
}

#[test]
fn builtin_fmi_targets_report_one_checked_projection() {
    let matrix = builtin_target_compatibility_matrix()
        .expect("built-in target compatibility matrix should build");
    for fmi in ["fmi2", "fmi3"] {
        let entry = matrix
            .iter()
            .find(|entry| entry.id == fmi)
            .unwrap_or_else(|| panic!("{fmi} target should be listed"));
        assert_eq!(entry.ir, TargetTemplateIr::Fmi);
        assert_eq!(entry.deployment_class.as_deref(), Some("fmu"));
        assert_eq!(entry.events, TargetFeatureSupport::Unsupported);
    }
}

#[test]
fn removed_file_render_context_is_rejected() {
    parse_target_manifest(
        r#"
version = 1
ir = "solve"
name = "removed-context"

[[files]]
path = "modelDescription.xml"
template = "modelDescription.xml.jinja"
render_context = "fmi-model-description"
"#,
    )
    .expect_err("removed per-file render contexts must not parse");
}

#[test]
fn target_manifest_parses_solve_tensor_capabilities() {
    let manifest = parse_manifest_with_ir_capabilities(
        "solve",
        r#"
[capabilities]
scalar_fallback = true

[capabilities.tensor]
matmul = "native"
linsolve = "scalar"
stencil = "native"
layout = "row-major"
supports_dynamic_shapes = false
sparse = false
dtypes = ["f32", "f64"]
"#,
    );
    let capabilities = manifest.capabilities.expect("capabilities table");
    let tensor = capabilities.tensor.expect("tensor capabilities");

    assert_eq!(manifest.ir, TargetTemplateIr::Solve);
    assert_eq!(capabilities.scalar_fallback, Some(true));
    assert_eq!(tensor.matmul, Some(TensorCapability::Native));
    assert_eq!(tensor.linsolve, Some(TensorCapability::Scalar));
    assert_eq!(tensor.stencil, Some(TensorCapability::Native));
    assert_eq!(tensor.layout, Some(TensorLayoutCapability::RowMajor));
    assert_eq!(tensor.supports_dynamic_shapes, Some(false));
    assert_eq!(tensor.sparse, Some(false));
    assert_eq!(
        tensor.dtypes,
        Some(vec!["f32".to_string(), "f64".to_string()])
    );
}

#[test]
fn solve_target_rejects_event_partition_without_event_support() {
    let mut solve = rumoca_ir_solve::SolveProblem::default();
    solve.events.scheduled_time_events.push(1.0);
    let manifest = parse_manifest_with_ir_capabilities(
        "solve",
        r#"
[capabilities]
events = false
"#,
    );
    let capabilities = manifest.capabilities.as_ref().expect("capabilities");

    let error = validate_solve_target_capabilities(&solve, &manifest, capabilities)
        .expect_err("an event-free target must reject an event partition");

    assert!(error.to_string().contains("unsupported-feature:events"));
}

#[test]
fn explicit_rhs_target_rejects_required_algebraic_projection() {
    let mut solve = rumoca_ir_solve::SolveProblem::default();
    solve
        .continuous
        .algebraic_projection_plan
        .blocks
        .push(rumoca_ir_solve::AlgebraicProjectionBlock::default());
    let manifest = parse_manifest_with_ir_capabilities(
        "solve",
        r#"
[capabilities]
residual_equations = false
"#,
    );
    let capabilities = manifest.capabilities.as_ref().expect("capabilities");

    let error = validate_solve_target_capabilities(&solve, &manifest, capabilities)
        .expect_err("an explicit RHS target must reject an algebraic projection");

    assert!(
        error
            .to_string()
            .contains("unsupported-feature:residual_equations")
    );
}

#[test]
fn explicit_rhs_target_accepts_issued_exact_algebraic_assignments() {
    let solve = solve_with_issued_exact_algebraic_assignment();
    let target = templates::builtin_targets()
        .iter()
        .find(|target| target.name == "fmi3")
        .expect("fmi3 target is built in");
    let manifest = parse_target_manifest(target.manifest).expect("fmi3 manifest is valid");
    let capabilities = manifest.capabilities.as_ref().expect("capabilities");

    validate_solve_target_capabilities(&solve, &manifest, capabilities)
        .expect("an issued exact assignment schedule needs no residual solver");
}

#[test]
fn target_without_exact_assignment_consumer_rejects_exact_algebraic_schedule() {
    let solve = solve_with_issued_exact_algebraic_assignment();
    let target = templates::builtin_targets()
        .iter()
        .find(|target| target.name == "c-ode")
        .expect("c-ode target is built in");
    let manifest = parse_target_manifest(target.manifest).expect("c-ode manifest is valid");
    let capabilities = manifest.capabilities.as_ref().expect("capabilities");

    let error = validate_solve_target_capabilities(&solve, &manifest, capabilities)
        .expect_err("a target that does not consume exact schedules must fail closed");
    assert!(
        error
            .to_string()
            .contains("unsupported-feature:residual_equations")
    );
}

#[test]
fn exact_assignment_consumer_rejects_incomplete_algebraic_schedule() {
    let mut solve = solve_with_issued_exact_algebraic_assignment();
    solve.continuous.refresh_owners = rumoca_ir_solve::ContinuousRefreshOwners::checked_for_source(
        &solve.continuous.implicit_rhs,
        rumoca_ir_solve::RefreshPlan::default(),
        rumoca_ir_solve::RefreshPlan::default(),
        rumoca_ir_solve::RefreshPlan::default(),
        rumoca_ir_solve::RefreshPlan::default(),
        Vec::new(),
    )
    .expect("an incomplete residual schedule is valid checked Solve input");
    let target = templates::builtin_targets()
        .iter()
        .find(|target| target.name == "fmi3")
        .expect("fmi3 target is built in");
    let manifest = parse_target_manifest(target.manifest).expect("fmi3 manifest is valid");
    let capabilities = manifest.capabilities.as_ref().expect("capabilities");

    let error = validate_solve_target_capabilities(&solve, &manifest, capabilities)
        .expect_err("an incomplete schedule must still require residual support");
    assert!(
        error
            .to_string()
            .contains("unsupported-feature:residual_equations")
    );
}

#[test]
fn residual_kernel_target_accepts_algebraic_projection_contract() {
    let mut solve = rumoca_ir_solve::SolveProblem::default();
    solve
        .continuous
        .algebraic_projection_plan
        .blocks
        .push(rumoca_ir_solve::AlgebraicProjectionBlock::default());
    let manifest = parse_manifest_with_ir_capabilities(
        "solve",
        r#"
[capabilities]
residual_equations = true
"#,
    );
    let capabilities = manifest.capabilities.as_ref().expect("capabilities");

    validate_solve_target_capabilities(&solve, &manifest, capabilities)
        .expect("a residual-kernel target may expose the projection contract");
}

#[test]
fn fmi_projection_defers_residual_classification_until_checked_solve() {
    let dae = dae_with_placeholder_family();
    let manifest = parse_manifest_with_ir_capabilities(
        "fmi",
        r#"
[capabilities]
continuous_states = true
residual_equations = false
"#,
    );
    let capabilities = manifest.capabilities.as_ref().expect("capabilities");

    validate_dae_target_capabilities(&dae, &manifest, capabilities)
        .expect("FMI must classify derivative and algebraic rows from checked Solve");
}

#[test]
fn algorithm_code_defers_algebraic_owner_classification_to_its_projection() {
    let dae = dae_with_placeholder_family();
    let manifest = parse_manifest_with_ir_capabilities(
        "algorithm-code",
        r#"
[capabilities]
continuous_states = false
residual_equations = false
"#,
    );
    let capabilities = manifest.capabilities.as_ref().expect("capabilities");

    validate_dae_target_capabilities(&dae, &manifest, capabilities)
        .expect("Algorithm Code must classify algebraic owners in its checked projection");
}

#[test]
fn solve_target_rejects_terminal_runtime_without_runtime_event_support() {
    let mut solve = rumoca_ir_solve::SolveProblem {
        layout: rumoca_ir_solve::VarLayout::from_parts(indexmap::IndexMap::new(), 0, 1),
        solve_layout: rumoca_ir_solve::SolveLayout {
            terminal_event_parameter_index: Some(0),
            ..rumoca_ir_solve::SolveLayout::default()
        },
        ..rumoca_ir_solve::SolveProblem::default()
    };
    solve.events.has_terminal_event = true;
    let manifest = parse_manifest_with_ir_capabilities(
        "solve",
        r#"
[capabilities]
events = true
runtime_events = false
"#,
    );
    let capabilities = manifest.capabilities.as_ref().expect("capabilities");

    let error = validate_solve_target_capabilities(&solve, &manifest, capabilities)
        .expect_err("a runtime-event-free target must reject termination");

    assert!(
        error
            .to_string()
            .contains("unsupported-feature:runtime_events"),
        "unexpected error: {error:#}"
    );
}

#[test]
fn solve_target_rejects_clock_partition_without_clock_support() {
    let mut solve = rumoca_ir_solve::SolveProblem {
        layout: rumoca_ir_solve::VarLayout::from_parts(indexmap::IndexMap::new(), 0, 1),
        ..rumoca_ir_solve::SolveProblem::default()
    };
    solve
        .clocks
        .periodic_event_schedules
        .push(rumoca_ir_solve::PeriodicEventSchedule::default());
    solve.clocks.activation_parameter_indices.push(0);
    let manifest = parse_manifest_with_ir_capabilities(
        "solve",
        r#"
[capabilities]
events = true
clocks = false
"#,
    );
    let capabilities = manifest.capabilities.as_ref().expect("capabilities");

    let error = validate_solve_target_capabilities(&solve, &manifest, capabilities)
        .expect_err("a clock-free target must reject a clock partition");

    assert!(
        error.to_string().contains("unsupported-feature:clocks"),
        "unexpected error: {error:#}"
    );
}

/// The invariant that licenses `dae_has_external_functions` to `expect` its
/// way through the function table instead of carrying a fail-closed arm: a
/// reserved-but-undefined function cannot reach a finalized `Dae`, so there is
/// no "unreadable body" state for the probe to be conservative about.
#[test]
fn an_undefined_function_cannot_reach_a_finalized_dae() {
    let source_text = "function f input Real u; output Real y; end f;";
    let mut source_map = SourceMap::new();
    let source_id = source_map.add("undefined-function.mo", source_text);
    let at = DaeProvenance::source(rumoca_core::Span::from_offsets(
        source_id,
        0,
        source_text.len(),
    ))
    .expect("fixture source span is exact");

    let error = Dae::construct(source_map, |dae| {
        let real = dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), at))?;
        dae.function(
            FunctionSignature::new(VarName::new("f"), [real], [real], at),
            // Reserve the function and never define a body for it.
            |_, _| Ok(()),
        )
        .map(|_| ())
    })
    .expect_err("a function reserved without a body must not finalize");

    assert!(
        matches!(
            error,
            rumoca_ir_dae::DaeConstructionError::IncompleteDefinition {
                kind: "function",
                ..
            }
        ),
        "expected an incomplete-definition rejection, got: {error:?}"
    );
}

/// SEV-155 NEGATIVE. The gate at `codegen_target.rs` must FIRE for a DAE that
/// carries an external interface. This test cannot pass against the former
/// constant-`false` probe.
#[test]
fn dae_target_rejects_external_interface_without_external_function_support() {
    let dae = dae_with_external_function();
    let manifest = manifest_with_capabilities(
        r#"
[capabilities]
residual_equations = true
external_functions = false
"#,
    );
    let capabilities = manifest.capabilities.as_ref().expect("capabilities");

    let error = validate_dae_target_capabilities(&dae, &manifest, capabilities)
        .expect_err("a target without external-function support must reject an external interface");

    assert!(
        error
            .to_string()
            .contains("unsupported-feature:external_functions"),
        "external-function capability must fail closed, got: {error}"
    );
}

/// The declared-capability arm: a target that admits external functions still
/// accepts the same DAE, so the probe gates on the declaration rather than
/// rejecting unconditionally.
#[test]
fn dae_target_admits_external_interface_when_the_capability_is_declared() {
    let dae = dae_with_external_function();
    let manifest = manifest_with_capabilities(
        r#"
[capabilities]
residual_equations = true
external_functions = true
"#,
    );
    let capabilities = manifest.capabilities.as_ref().expect("capabilities");

    validate_dae_target_capabilities(&dae, &manifest, capabilities)
        .expect("a target declaring external-function support admits an external interface");
}

/// The POSITIVE guard against an over-firing probe: a DAE whose function table
/// is nonempty but holds only a Modelica body must stay admissible on an
/// `external_functions = false` target. A derivation mutated to a constant
/// `true` turns this red.
#[test]
fn external_function_free_dae_stays_admissible_without_external_function_support() {
    let dae = dae_with_modelica_function();
    let manifest = manifest_with_capabilities(
        r#"
[capabilities]
residual_equations = true
external_functions = false
"#,
    );
    let capabilities = manifest.capabilities.as_ref().expect("capabilities");

    validate_dae_target_capabilities(&dae, &manifest, capabilities)
        .expect("a DAE with no external interface must not trip the external-function gate");
}

/// Every built-in target declares `external_functions = false`, so the fixed
/// probe makes the external DAE reject against all of them. This is the
/// consumer-visible behavior change SEV-155 asks for.
#[test]
fn every_builtin_target_rejects_an_external_interface() {
    let dae = dae_with_external_function();
    for target in templates::builtin_targets() {
        let manifest = parse_target_manifest(target.manifest).unwrap_or_else(|err| {
            panic!("built-in target '{}' failed to parse: {err}", target.name)
        });
        let Some(capabilities) = manifest.capabilities.as_ref() else {
            continue;
        };
        if capabilities.external_functions != Some(false) {
            continue;
        }
        let error = validate_dae_target_capabilities(&dae, &manifest, capabilities)
            .expect_err("no built-in target supports external functions");
        assert!(
            error
                .to_string()
                .contains("unsupported-feature:external_functions"),
            "built-in target '{}' must reject on external_functions, got: {error}",
            target.name
        );
    }
}

#[test]
fn dae_target_must_declare_structured_family_consumption() {
    let dae = dae_with_placeholder_family();
    let manifest = manifest_with_capabilities(
        r#"
[capabilities]
residual_equations = true
"#,
    );
    let capabilities = manifest.capabilities.as_ref().expect("capabilities");

    let error = validate_dae_target_capabilities(&dae, &manifest, capabilities)
        .expect_err("scalar-only DAE target must reject placeholder rows");

    assert!(
        error
            .to_string()
            .contains("unsupported-feature:structured_equation_families")
    );
}

#[test]
fn family_aware_dae_target_accepts_canonical_structured_owner() {
    let dae = dae_with_placeholder_family();
    let manifest = manifest_with_capabilities(
        r#"
[capabilities]
residual_equations = true
structured_equation_families = true
"#,
    );
    let capabilities = manifest.capabilities.as_ref().expect("capabilities");

    validate_dae_target_capabilities(&dae, &manifest, capabilities)
        .expect("declared family-aware target may consume the compact owner");
}

#[test]
fn target_manifest_rejects_tensor_capabilities_for_non_solve_ir() {
    let err = super::parse_target_manifest(
        r#"
version = 1
ir = "dae"
name = "custom"

[capabilities.tensor]
matmul = "native"

[[files]]
path = "model.out"
template = "model.out.jinja"
"#,
    )
    .expect_err("tensor capabilities should require solve IR");

    assert!(
        err.to_string()
            .contains("tensor capabilities are only valid")
    );
}

#[test]
fn target_manifest_rejects_scalar_tensor_ops_without_scalar_fallback() {
    let manifest = parse_manifest_with_ir_capabilities(
        "solve",
        r#"
[capabilities]
scalar_fallback = false

[capabilities.tensor]
matmul = "native"
linsolve = "native"
"#,
    );
    let capabilities = manifest.capabilities.as_ref().expect("capabilities");
    validate_target_manifest(&manifest).expect("native tensor ops need no scalar fallback");
    assert_eq!(capabilities.scalar_fallback, Some(false));

    let err = super::parse_target_manifest(
        r#"
version = 1
ir = "solve"
name = "custom"

[capabilities]
scalar_fallback = false

[capabilities.tensor]
matmul = "scalar"

[[files]]
path = "model.out"
template = "model.out.jinja"
"#,
    )
    .expect_err("scalar tensor op should require scalar fallback");

    assert!(err.to_string().contains("scalar_fallback = false"));
}

#[test]
fn target_manifest_rejects_invalid_readiness_level() {
    let err = super::parse_target_manifest(
        r#"
version = 1
ir = "solve"
name = "invalid"
readiness_level = 6

[[files]]
path = "model.out"
template = "model.out.jinja"
"#,
    )
    .expect_err("readiness level above 5 should fail");

    assert!(err.to_string().contains("readiness_level"), "{err}");
}

#[test]
fn target_manifest_rejects_manifest_only_readiness_zero() {
    let error = super::parse_target_manifest(
        r#"
version = 1
ir = "solve"
name = "future-target"
readiness_level = 0
"#,
    )
    .expect_err("readiness level 0 must not permit a manifest-only placeholder");

    assert!(error.to_string().contains("file entry"), "{error}");
}

#[test]
fn target_manifest_rejects_missing_files_after_readiness_zero() {
    let err = super::parse_target_manifest(
        r#"
version = 1
ir = "solve"
name = "unfinished"
readiness_level = 1
"#,
    )
    .expect_err("every target requires generated files");

    assert!(err.to_string().contains("file entry"), "{err}");
}

#[test]
fn target_manifest_rejects_empty_tensor_dtype() {
    let err = super::parse_target_manifest(
        r#"
version = 1
ir = "solve"
name = "invalid-dtypes"

[capabilities.tensor]
dtypes = ["f64", ""]

[[files]]
path = "model.out"
template = "model.out.jinja"
"#,
    )
    .expect_err("empty tensor dtype should fail");

    assert!(err.to_string().contains("dtypes"), "{err}");
}

#[test]
fn target_manifest_accepts_requirements_as_capabilities_alias() {
    let manifest = manifest_with_capabilities(
        r#"
[requirements]
continuous_states = false
residual_equations = false
"#,
    );
    let capabilities = manifest.capabilities.expect("requirements alias");

    assert_eq!(capabilities.continuous_states, Some(false));
    assert_eq!(capabilities.residual_equations, Some(false));
}

#[test]
fn shared_dae_renderer_blocks_placeholder_scalar_residuals() {
    let manifest = manifest_with_capabilities(
        r#"
[capabilities]
residual_equations = true
"#,
    );
    let templates =
        std::collections::BTreeMap::from([("model.out.jinja".to_string(), String::new())]);

    let error =
        super::render_dae_target_files(&templates, &manifest, &dae_with_placeholder_family(), "M")
            .expect_err("shared DAE rendering must not expose placeholder residuals");

    assert!(
        error
            .to_string()
            .contains("unsupported-feature:structured_equation_families"),
        "{error}"
    );
}

#[test]
fn shared_dae_renderer_allows_declared_structured_owner_consumer() {
    let manifest = manifest_with_capabilities(
        r#"
[capabilities]
residual_equations = true
structured_equation_families = true
"#,
    );
    let templates = std::collections::BTreeMap::from([(
        "model.out.jinja".to_string(),
        "{{ dae.systems.continuous.owners | length }}".to_string(),
    )]);

    let files =
        super::render_dae_target_files(&templates, &manifest, &dae_with_placeholder_family(), "M")
            .expect("declared family-aware consumer may render the canonical owner");

    assert_eq!(files[0].content, "1");
}

/// A well-formed checksum web (one producer, one consumer edge) parses and
/// validates — the positive control for the rejection tests below.
#[test]
fn checksum_web_accepts_a_wellformed_declaration() {
    super::parse_target_manifest(
        r#"
version = 1
ir = "solve"
name = "checksum-web"
[[files]]
path = "a.txt"
template = "a.jinja"
id = "a"
[[files]]
path = "b.txt"
template = "b.jinja"
[[files.checksums]]
of = "a"
  algorithm = "sha1"
as = "a_sha1"
"#,
    )
    .expect("a well-formed checksum web validates");
}

fn expect_target_error(source: &str, needle: &str) {
    let err =
        super::parse_target_manifest(source).expect_err("malformed target.toml must be rejected");
    assert!(
        err.to_string().contains(needle),
        "error `{err}` should mention `{needle}`"
    );
}

#[test]
fn checksum_web_rejects_duplicate_file_ids() {
    expect_target_error(
        r#"
version = 1
ir = "solve"
name = "dup-id"
[[files]]
path = "a.txt"
template = "a.jinja"
id = "x"
[[files]]
path = "b.txt"
template = "b.jinja"
id = "x"
"#,
        "duplicate [[files]] id",
    );
}

#[test]
fn checksum_web_rejects_dangling_of() {
    expect_target_error(
        r#"
version = 1
ir = "solve"
name = "dangling"
[[files]]
path = "b.txt"
template = "b.jinja"
[[files.checksums]]
of = "ghost"
  algorithm = "sha1"
as = "ghost_sha1"
"#,
        "names no [[files]] id",
    );
}

#[test]
fn checksum_web_rejects_self_hash() {
    expect_target_error(
        r#"
version = 1
ir = "solve"
name = "self-hash"
[[files]]
path = "a.txt"
template = "a.jinja"
id = "a"
[[files.checksums]]
of = "a"
  algorithm = "sha1"
as = "a_sha1"
"#,
        "checksums itself",
    );
}

#[test]
fn checksum_web_rejects_empty_as_key() {
    expect_target_error(
        r#"
version = 1
ir = "solve"
name = "empty-as"
[[files]]
path = "a.txt"
template = "a.jinja"
id = "a"
[[files]]
path = "b.txt"
template = "b.jinja"
[[files.checksums]]
of = "a"
  algorithm = "sha1"
as = ""
"#,
        "`as` must not be empty",
    );
}

#[test]
fn checksum_web_rejects_duplicate_as_key_on_one_file() {
    expect_target_error(
        r#"
version = 1
ir = "solve"
name = "dup-as"
[[files]]
path = "a.txt"
template = "a.jinja"
id = "a"
[[files]]
path = "c.txt"
template = "c.jinja"
id = "c"
[[files]]
path = "b.txt"
template = "b.jinja"
[[files.checksums]]
of = "a"
  algorithm = "sha1"
as = "sha1"
[[files.checksums]]
of = "c"
  algorithm = "sha1"
as = "sha1"
"#,
        "declared twice",
    );
}

#[test]
fn asset_tree_rejects_empty_source_and_dest() {
    expect_target_error(
        r#"
version = 1
ir = "solve"
name = "empty-bundle"
[[files]]
path = "a.txt"
template = "a.jinja"
[[assets]]
source = ""
dest = "schemas/"
"#,
        "source must not be empty",
    );
    expect_target_error(
        r#"
version = 1
ir = "solve"
name = "empty-dest"
[[files]]
path = "a.txt"
template = "a.jinja"
[[assets]]
source = "schemas"
dest = ""
"#,
        "dest",
    );
}

#[test]
fn target_asset_relative_path_is_portable_and_nested() {
    let root = Path::new("target/assets");
    let path = root.join("schémas").join("nested").join("model.xsd");

    assert_eq!(
        target_asset_relative_path(root, &path).expect("nested UTF-8 asset path"),
        "schémas/nested/model.xsd"
    );
}

#[test]
fn target_asset_relative_path_rejects_paths_outside_root() {
    let error = target_asset_relative_path(
        Path::new("target/assets"),
        Path::new("target/templates/model.jinja"),
    )
    .expect_err("asset outside its declared root must fail");

    assert!(
        error.to_string().contains("is not beneath source root"),
        "unexpected error: {error:#}"
    );
}
