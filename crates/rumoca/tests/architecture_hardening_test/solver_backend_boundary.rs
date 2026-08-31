//! Concrete numerical-plugin dependency boundary (SPEC_0041 §4).

use super::*;
use std::path::PathBuf;
use syn::visit::{self, Visit};

/// Concrete solver backends consume only `rumoca-solver`'s opaque FMI ME
/// importer/host contract. Target encoders are banned from every dependency
/// table. Backend tests exercise the ME interface directly; Solve IR and its
/// evaluator are not backend fixture dependencies.
#[test]
fn concrete_solver_backends_consume_the_me_contract_only() {
    let root = workspace_root();
    let offenders = ["rumoca-solver-diffsol", "rumoca-solver-rk45"]
        .iter()
        .flat_map(|crate_name| solver_backend_boundary_offenders(&root, crate_name))
        .collect::<Vec<_>>();

    assert!(
        offenders.is_empty(),
        "concrete solver backends consume only rumoca-solver's opaque FMI ME importer/host \
contract; phase, compiler IR/evaluator, facade, and rumoca-exec-* dependencies are forbidden \
in every dependency table: {offenders:?}"
    );
}

fn solver_backend_boundary_offenders(root: &Path, crate_name: &str) -> Vec<String> {
    let cargo_toml = root.join(format!("crates/{crate_name}/Cargo.toml"));
    let content = fs::read_to_string(&cargo_toml).expect("read solver backend Cargo.toml");
    all_manifest_dependency_names(&content)
        .into_iter()
        .filter(|(section, dependency)| solver_backend_dep_is_banned(section, dependency))
        .map(|(section, dependency)| format!("{crate_name} {section} {dependency}"))
        .collect()
}

pub(super) fn solver_backend_dep_is_banned(_section: &str, dependency: &str) -> bool {
    const BANNED_EXACT: &[&str] = &[
        "rumoca-compile",
        "rumoca-ir-ast",
        "rumoca-ir-flat",
        "rumoca-ir-dae",
        "rumoca-eval-ast",
        "rumoca-eval-flat",
        "rumoca-eval-dae",
        "rumoca-sim",
    ];

    matches!(dependency, "rumoca-ir-solve" | "rumoca-eval-solve")
        || dependency.starts_with("rumoca-phase-")
        || dependency.starts_with("rumoca-exec-")
        || dependency == "rumoca-phase-codegen"
        || BANNED_EXACT.contains(&dependency)
}

fn prepared_refresh_projection_violations(sources: &[&str]) -> Vec<&'static str> {
    let mut violations = Vec::new();
    for (needle, label) in [
        ("refresh_program_rows:", "runtime refresh source map field"),
        ("fn refresh_program_row(", "fallible runtime refresh lookup"),
        (
            "self.refresh_program_rows",
            "execution-time refresh source map",
        ),
        (
            "|row| self.refresh_program",
            "execution-time refresh lookup closure",
        ),
    ] {
        if sources.iter().any(|source| source.contains(needle)) {
            violations.push(label);
        }
    }
    violations
}

fn struct_fields_mentioning_type(source: &str, struct_name: &str, type_name: &str) -> Vec<String> {
    let syntax = syn::parse_file(source).expect("parse prepared refresh owner");
    let owner = syntax.items.iter().find_map(|item| match item {
        syn::Item::Struct(owner) if owner.ident == struct_name => Some(owner),
        _ => None,
    });
    let owner = owner.unwrap_or_else(|| panic!("missing {struct_name} owner"));
    owner
        .fields
        .iter()
        .enumerate()
        .filter_map(|(ordinal, field)| {
            let mut visitor = TypeNameVisitor {
                type_name,
                found: false,
            };
            visitor.visit_type(&field.ty);
            visitor.found.then(|| {
                field
                    .ident
                    .as_ref()
                    .map_or_else(|| ordinal.to_string(), ToString::to_string)
            })
        })
        .collect()
}

fn positional_refresh_builder_violations(source: &str) -> Vec<&'static str> {
    let syntax = syn::parse_file(source).expect("parse refresh-plan mutation");
    let mut visitor = PositionalRefreshVisitor::default();
    visitor.visit_file(&syntax);
    visitor.findings
}

#[derive(Default)]
struct PositionalRefreshVisitor {
    findings: Vec<&'static str>,
}

impl<'ast> Visit<'ast> for PositionalRefreshVisitor {
    fn visit_item_mod(&mut self, item: &'ast syn::ItemMod) {
        if test_only(&item.attrs) {
            return;
        }
        visit::visit_item_mod(self, item);
    }

    fn visit_item_fn(&mut self, item: &'ast syn::ItemFn) {
        if test_only(&item.attrs) {
            return;
        }
        match item.sig.ident.to_string().as_str() {
            "build_algebraic_refresh_plan" => self.record_algebraic_builder(),
            "build_derivative_refresh_plan" => self.record_derivative_builder(),
            _ => {}
        }
        visit::visit_item_fn(self, item);
    }

    fn visit_item_impl(&mut self, item: &'ast syn::ItemImpl) {
        if test_only(&item.attrs) {
            return;
        }
        let trait_name = item
            .trait_
            .as_ref()
            .and_then(|(_, path, _)| path.segments.last())
            .map(|segment| segment.ident.to_string());
        let self_name = match item.self_ty.as_ref() {
            syn::Type::Path(path) => path
                .path
                .segments
                .last()
                .map(|segment| segment.ident.to_string()),
            _ => None,
        };
        if trait_name.as_deref() == Some("RefreshProgramAccess")
            && self_name.as_deref() == Some("ScalarProgramBlock")
        {
            self.findings
                .push("deleted positional ScalarProgramBlock access implementation");
        }
        visit::visit_item_impl(self, item);
    }

    fn visit_impl_item_fn(&mut self, item: &'ast syn::ImplItemFn) {
        match item.sig.ident.to_string().as_str() {
            "build_algebraic_refresh_plan" => self.record_algebraic_builder(),
            "build_derivative_refresh_plan" => self.record_derivative_builder(),
            _ => {}
        }
        visit::visit_impl_item_fn(self, item);
    }

    fn visit_expr_call(&mut self, expression: &'ast syn::ExprCall) {
        let positional_source = match expression.func.as_ref() {
            syn::Expr::Path(path) => {
                let segments = &path.path.segments;
                segments
                    .last()
                    .is_some_and(|segment| segment.ident == "checked")
                    && segments
                        .iter()
                        .any(|segment| segment.ident == "RefreshScalarProgramSource")
                    && expression.args.first().is_some_and(expr_is_zero)
            }
            _ => false,
        };
        if positional_source {
            self.findings
                .push("fabricated node-zero refresh source identity");
        }
        visit::visit_expr_call(self, expression);
    }

    fn visit_path_segment(&mut self, segment: &'ast syn::PathSegment) {
        match segment.ident.to_string().as_str() {
            "build_algebraic_refresh_plan" => self.record_algebraic_builder(),
            "build_derivative_refresh_plan" => self.record_derivative_builder(),
            _ => {}
        }
        visit::visit_path_segment(self, segment);
    }
}

impl PositionalRefreshVisitor {
    fn record_algebraic_builder(&mut self) {
        if !self
            .findings
            .contains(&"deleted positional algebraic refresh builder")
        {
            self.findings
                .push("deleted positional algebraic refresh builder");
        }
    }

    fn record_derivative_builder(&mut self) {
        if !self
            .findings
            .contains(&"deleted positional derivative refresh builder")
        {
            self.findings
                .push("deleted positional derivative refresh builder");
        }
    }
}

fn cfg_test(attributes: &[syn::Attribute]) -> bool {
    attributes.iter().any(|attribute| {
        attribute.path().is_ident("cfg")
            && matches!(&attribute.meta, syn::Meta::List(meta) if meta.tokens.to_string().contains("test"))
    })
}

fn test_only(attributes: &[syn::Attribute]) -> bool {
    cfg_test(attributes)
        || attributes
            .iter()
            .any(|attribute| attribute.path().is_ident("test"))
}

fn expr_is_zero(expression: &syn::Expr) -> bool {
    matches!(
        expression,
        syn::Expr::Lit(syn::ExprLit {
            lit: syn::Lit::Int(integer),
            ..
        }) if integer.base10_parse::<u128>().is_ok_and(|value| value == 0)
    )
}

fn positional_refresh_sources(root: &Path) -> Vec<(PathBuf, String)> {
    ["rumoca-eval-solve", "rumoca-solver"]
        .into_iter()
        .flat_map(|name| production_rust_sources(&root.join("crates").join(name), root))
        .collect()
}

/// AS-070/F1: all refresh owners must be issued from the canonical continuous
/// projection. Positional block adapters are a deleted construction authority,
/// not a compatibility surface for tests or future callers.
#[test]
fn positional_refresh_builders_and_access_stay_deleted() {
    let root = workspace_root();
    let violations = positional_refresh_sources(&root)
        .into_iter()
        .flat_map(|(path, source)| {
            positional_refresh_builder_violations(&source)
                .into_iter()
                .map(move |finding| format!("{}: {finding}", path.display()))
        })
        .collect::<Vec<_>>();
    assert!(
        violations.is_empty(),
        "positional refresh construction survived the canonical-owner cutover: {violations:?}"
    );
}

#[test]
fn positional_refresh_builder_mutations_are_detected() {
    for mutation in [
        "pub fn build_algebraic_refresh_plan(problem: &SolveProblem) {}",
        "pub fn build_derivative_refresh_plan(problem: &SolveProblem) {}",
        "impl RefreshProgramAccess for solve::ScalarProgramBlock {}",
        "fn planted() { let _ = RefreshScalarProgramSource::checked(0, index); }",
    ] {
        assert!(
            !positional_refresh_builder_violations(mutation).is_empty(),
            "AS-070/F1 tombstone missed mutation: {mutation}"
        );
    }
    assert!(
        positional_refresh_builder_violations(
            "#[cfg_attr(any(), allow(dead_code))]\nfn extracted() { let _ = RefreshScalarProgramSource::checked(0, index); }"
        )
        .contains(&"fabricated node-zero refresh source identity"),
        "gate must detect a moved production helper"
    );
    assert!(
        positional_refresh_builder_violations(
            "#[cfg(test)] mod tests { fn fixture() { let _ = RefreshScalarProgramSource::checked(0, index); } }"
        )
        .is_empty(),
        "local unit fixtures may construct synthetic source identities"
    );
}

struct TypeNameVisitor<'a> {
    type_name: &'a str,
    found: bool,
}

impl<'ast> Visit<'ast> for TypeNameVisitor<'_> {
    fn visit_path_segment(&mut self, segment: &'ast syn::PathSegment) {
        self.found |= segment.ident == self.type_name;
        visit::visit_path_segment(self, segment);
    }
}

/// SPEC_0043 section 6: final scalar projection is bound once while preparing
/// each refresh row. The runtime aggregate cannot retain or revisit the
/// ephemeral source catalog.
#[test]
fn prepared_refresh_rows_own_their_final_scalar_projection() {
    let root = workspace_root();
    let runtime =
        fs::read_to_string(root.join("crates/rumoca-solver/src/runtime/solve_runtime.rs"))
            .expect("read Solve runtime root");
    let execution = fs::read_to_string(
        root.join("crates/rumoca-solver/src/runtime/solve_runtime/refresh_execution.rs"),
    )
    .expect("read refresh execution");
    let batch = fs::read_to_string(
        root.join("crates/rumoca-solver/src/runtime/solve_runtime/refresh_batch.rs"),
    )
    .expect("read refresh batch execution");
    let preparation = fs::read_to_string(
        root.join("crates/rumoca-solver/src/runtime/solve_runtime/refresh_projection.rs"),
    )
    .expect("read refresh preparation");
    assert!(
        runtime.contains("program_rows: Box<[PreparedRefreshProgramRow]>")
            && runtime.contains("PreparedRefreshProgramRow::checked(")
            && preparation.contains("program_catalog.bind(row.source())"),
        "prepared refresh plans must retain construction-issued scalar projections"
    );
    let retained_source_fields =
        struct_fields_mentioning_type(&runtime, "SolveRuntime", "RefreshScalarProgramSource");
    assert!(
        retained_source_fields.is_empty(),
        "SolveRuntime retained refresh source identities in fields: {retained_source_fields:?}"
    );
    let violations = prepared_refresh_projection_violations(&[&runtime, &execution, &batch]);
    assert!(
        violations.is_empty(),
        "runtime refresh projection lookup returned: {violations:?}"
    );
}

#[test]
fn prepared_refresh_projection_gate_detects_runtime_lookup_mutations() {
    for mutation in [
        "struct SolveRuntime { refresh_program_rows: Map }",
        "fn refresh_program_row(&self) {}",
        "self.refresh_program_rows.get(&row.source())",
        "|row| self.refresh_program_row(row)",
    ] {
        assert!(
            !prepared_refresh_projection_violations(&[mutation]).is_empty(),
            "gate missed planted runtime lookup: {mutation}"
        );
    }
    assert_eq!(
        struct_fields_mentioning_type(
            "struct SolveRuntime { renamed: Option<Vec<RefreshScalarProgramSource>> }",
            "SolveRuntime",
            "RefreshScalarProgramSource",
        ),
        ["renamed"],
        "gate missed a renamed and nested runtime source-identity field"
    );
}

/// The facade owns solver selection. Concrete backends must not grow public
/// batch entry points that bypass `SimOptions::solver_mode`, and retired names
/// must not be restored as aliases.
#[test]
fn dae_simulation_has_one_typed_mode_dispatch_entry() {
    let root = workspace_root();
    let facade = fs::read_to_string(root.join("crates/rumoca-sim/src/lib.rs"))
        .expect("read rumoca-sim facade");
    let diffsol = fs::read_to_string(root.join("crates/rumoca-sim/src/diffsol.rs"))
        .expect("read diffsol facade backend");
    let rk45 = fs::read_to_string(root.join("crates/rumoca-sim/src/rk45.rs"))
        .expect("read rk45 facade backend");
    let session = fs::read_to_string(root.join("crates/rumoca-sim/src/simulation_session.rs"))
        .expect("read canonical simulation session");

    assert_eq!(
        facade.matches("pub fn simulate_dae(").count(),
        1,
        "rumoca-sim must expose exactly one canonical DAE batch entry"
    );
    let dispatch = facade
        .split_once("pub fn simulate_dae(")
        .expect("canonical DAE entry exists")
        .1
        .split_once("/// Simulate one already-constructed correlated FMI component")
        .expect("canonical DAE entry remains a bounded facade function")
        .0;
    for required in [
        "Result<SimResult, SimulationDiagnosticError>",
        "match opts.solver_mode",
        "SimSolverMode::Auto",
        "SimSolverMode::RkLike",
        "SimSolverMode::Bdf",
    ] {
        assert!(
            dispatch.contains(required),
            "canonical DAE entry must retain typed exhaustive mode dispatch: missing `{required}`"
        );
    }

    for retired in [
        "simulate_dae_with_diagnostics",
        "simulate_with_diagnostics_auto_nan_trace",
        "pub fn simulate(",
        "pub fn simulate_with_diagnostics(",
        "pub use simulate_dae as",
        "pub use simulate_with_diagnostics",
        "check_prepared_initialization",
        "run_prepared_simulation",
    ] {
        assert!(
            !facade.contains(retired),
            "retired root simulation entry/alias must not return: `{retired}`"
        );
    }
    for (backend, source) in [("diffsol", diffsol), ("rk45", rk45)] {
        for retired in [
            "pub fn simulate(",
            "pub fn simulate_dae(",
            "pub fn simulate_with_diagnostics(",
            "pub use simulate as simulate_dae",
            "pub use simulate_with_diagnostics",
            "fn run_dae_backend(",
        ] {
            assert!(
                !source.contains(retired),
                "{backend} must not expose a DAE batch bypass: `{retired}`"
            );
        }
    }
    assert!(
        !session.contains("new_with_diagnostics"),
        "the typed SimulationSession::new constructor needs no diagnostic-name alias"
    );
}

/// SEV-050 / TRP-053 tombstones: native execution is a preparation-issued arm,
/// never an optional accelerator that can disappear after a failure.
fn interpreter_contexts_are_confined(
    runtime_source: &str,
    production_runtime_modules: &str,
    solve_events: &str,
) -> bool {
    runtime_source.matches("RowEvalContext {").count() == 3
        && runtime_source.matches("..Default::default()").count() == 3
        && !runtime_source.contains("RowEvalContext::default()")
        && !production_runtime_modules.contains("RowEvalContext {")
        && !production_runtime_modules.contains("RowEvalContext::default()")
        && !solve_events.contains("RowEvalContext {")
        && !solve_events.contains("RowEvalContext::default()")
        && !production_runtime_modules.contains("_brand: PermitBrand")
        && !solve_events.contains("_brand: PermitBrand")
        && !production_runtime_modules.contains("DynamicTimeEventsPermit {")
        && !production_runtime_modules.contains("PreparationConstantRootsPermit {")
        && !solve_events.contains("DynamicTimeEventsPermit {")
        && runtime_source.contains("struct RuntimeRowEvalContextPermit")
        && runtime_source.contains("RuntimeRowEvalContextPermit::for_runtime(runtime)")
        && runtime_source.contains("struct PermitBrand;")
        && runtime_source.matches("_brand: PermitBrand").count() == 31
        && runtime_source.contains("impl PreparationConstantRootsPermit {")
        && runtime_source.contains("fn row_eval_context_for_model<'model>(")
        && runtime_source.contains("impl DynamicTimeEventsPermit {")
        && solve_events.contains("execution.row_eval_context_for_model(model, runtime_state)")
}

#[test]
fn native_execution_plan_cannot_regrow_demotion_or_interpreter_retry() {
    let root = workspace_root();
    let runtime_root = root.join("crates/rumoca-solver/src/runtime/solve_runtime");
    let runtime_source =
        fs::read_to_string(root.join("crates/rumoca-solver/src/runtime/solve_runtime.rs"))
            .expect("read Solve runtime root");
    let mut runtime = runtime_source.clone();
    let mut production_runtime_modules = String::new();
    for entry in fs::read_dir(runtime_root).expect("read Solve runtime modules") {
        let path = entry.expect("read Solve runtime module entry").path();
        if path.extension().is_some_and(|extension| extension == "rs") {
            let source = fs::read_to_string(&path).expect("read Solve runtime module");
            runtime.push_str(&source);
            if path.file_name().is_some_and(|name| name != "tests.rs") {
                production_runtime_modules.push_str(&source);
            }
        }
    }
    let fmi = fs::read_to_string(root.join("crates/rumoca-solver/src/fmi_me.rs"))
        .expect("read FMI ME boundary");
    let solve_events =
        fs::read_to_string(root.join("crates/rumoca-solver/src/runtime/solve_events.rs"))
            .expect("read Solve event scheduling boundary");
    let sim_native = fs::read_to_string(root.join("crates/rumoca-sim/src/native_execution.rs"))
        .expect("read native facade adapter");
    let diffsol = fs::read_to_string(root.join("crates/rumoca-sim/src/diffsol.rs"))
        .expect("read diffsol facade");
    let eval_solve = fs::read_to_string(root.join("crates/rumoca-eval-solve/src/lib.rs"))
        .expect("read Solve reference evaluator");
    let prepared_eval = fs::read_to_string(root.join("crates/rumoca-eval-solve/src/prepared.rs"))
        .expect("read prepared Solve evaluator");

    for required in [
        "enum ExecutionArm<T, P>",
        "struct RuntimeExecutionPlan",
        "enum InterpreterExecutionOwner",
        "struct InterpreterExecutionPlan",
        "enum PreparedRefreshExecution",
        "enum PreparedRefreshStage",
        "CertifiedCausal",
        "CertifiedStages",
        "FullProjection",
        "interpreter_permit!(ExactAssignmentPermit)",
        "interpreter_permit!(RootConditionsPermit)",
        "interpreter_permit!(PreparationConstantRootsPermit)",
        "struct DynamicTimeEventsPermit",
        "fn dynamic_time_events_permit_for_test()",
        "fn catalog_ordinal(self)",
        "fn native_execution_owner_catalog_ordinal",
        "ExecutionArm::Interpreter",
        "ExecutionArm::Native",
    ] {
        assert!(
            runtime.contains(required),
            "sealed per-operation execution plan lost `{required}`"
        );
    }
    assert!(
        fmi.contains("pub enum MeExecutionSelection"),
        "FMI construction must receive a closed interpreter/native selection"
    );
    assert!(
        interpreter_contexts_are_confined(
            &runtime_source,
            &production_runtime_modules,
            &solve_events,
        ),
        "runtime and event evaluator sites must consume operation-specific typed owners"
    );
    assert!(
        runtime_source
            .matches("pure_calls: Some(&model.pure_calls)")
            .count()
            == 2,
        "the dynamic-time-event and preparation permits must each carry the model pure-call table"
    );

    let mut raw_context_mutation = production_runtime_modules.clone();
    raw_context_mutation.push_str("fn bypass() { let _ = RowEvalContext::default(); }");
    assert!(
        !interpreter_contexts_are_confined(&runtime_source, &raw_context_mutation, &solve_events,),
        "the context confinement gate must reject an unowned default-context mutation"
    );
    let mut event_context_mutation = solve_events.clone();
    event_context_mutation.push_str("fn bypass() { let _ = RowEvalContext { }; }");
    assert!(
        !interpreter_contexts_are_confined(
            &runtime_source,
            &production_runtime_modules,
            &event_context_mutation,
        ),
        "the event confinement gate must reject a raw-context mutation"
    );
    let mut root_context_mutation = runtime_source.clone();
    root_context_mutation.push_str("fn bypass() { let _ = RowEvalContext::default(); }");
    assert!(
        !interpreter_contexts_are_confined(
            &root_context_mutation,
            &production_runtime_modules,
            &solve_events,
        ),
        "the context confinement gate must reject a raw context outside a permit"
    );
    let mut permit_forgery_mutation = solve_events.clone();
    permit_forgery_mutation
        .push_str("fn bypass() { let _ = DynamicTimeEventsPermit { _brand: PermitBrand }; }");
    assert!(
        !interpreter_contexts_are_confined(
            &runtime_source,
            &production_runtime_modules,
            &permit_forgery_mutation,
        ),
        "the context gate must reject a sibling module minting a typed permit"
    );

    let joined = format!(
        "{runtime}\n{solve_events}\n{fmi}\n{sim_native}\n{diffsol}\n{eval_solve}\n{prepared_eval}"
    );
    for retired in [
        "optional_compiled",
        "native_specialization",
        "specialized_row_program",
        "SpecializedRowProgram",
        "compiled_assignment_schedules",
        "failed_discrete_rows",
        "failed_clock_partition_intermediates",
        "failed_guarded_assignments",
        "failed_root_rows",
        "failed_visible_rows",
        "failed_event_action_rows",
        "compile_torn_assignment_rows",
        "new_with_execution_backend",
        "new_fixture",
        "admit_execution_backend",
        "drop(check_prepared_component",
        "current runtime semantics fall back to the interpreter",
        "restore_after_causal_seed_error",
        "seed_error_allows_projection",
        "causal_seed_failed",
        "try_native_assignment_refresh",
        "validate_discrete_event_rows",
        "InterpreterArm",
        "value_stage_schedule_is_certified",
        "stage: &solve::RefreshStage",
    ] {
        assert!(
            !joined.contains(retired),
            "retired native demotion/retry surface must not return: `{retired}`"
        );
    }
}
