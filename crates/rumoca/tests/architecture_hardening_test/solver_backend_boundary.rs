//! Concrete numerical-plugin dependency boundary (SPEC_0041 §4).

use super::*;
use quote::ToTokens;
use std::collections::{BTreeMap, VecDeque};
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

/// AS-044: all refresh owners must be issued from the canonical continuous
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
            "AS-044 tombstone missed mutation: {mutation}"
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

#[derive(Default)]
struct SolveExecutionApiVisitor {
    violations: Vec<String>,
    opaque_refresh_entries: usize,
}

impl SolveExecutionApiVisitor {
    fn record_public_fields(&mut self, item: &syn::ItemStruct) {
        for (ordinal, field) in item.fields.iter().enumerate() {
            if matches!(field.vis, syn::Visibility::Public(_)) {
                self.violations.push(format!(
                    "{} exposes public field {}",
                    item.ident,
                    field
                        .ident
                        .as_ref()
                        .map_or_else(|| ordinal.to_string(), ToString::to_string)
                ));
            }
        }
    }

    fn record_owner_methods(&mut self, self_name: Option<&str>, items: &[syn::ImplItem]) {
        let Some(self_name) = self_name else {
            return;
        };
        for method in items.iter().filter_map(|item| match item {
            syn::ImplItem::Fn(method) => Some(method),
            _ => None,
        }) {
            self.record_owner_method(self_name, method);
        }
    }

    fn record_owner_method(&mut self, self_name: &str, method: &syn::ImplItemFn) {
        if self_name == "ContinuousRefreshOwners"
            && matches!(method.vis, syn::Visibility::Public(_))
            && method
                .sig
                .output
                .to_token_stream()
                .to_string()
                .contains("Self")
        {
            self.violations
                .push("ContinuousRefreshOwners exposes a standalone public issuer".to_string());
        }
        if self_name == "ContinuousSolveSystem"
            && method.sig.ident == "construct"
            && method
                .sig
                .inputs
                .to_token_stream()
                .to_string()
                .contains("ContinuousRefreshOwners")
        {
            self.violations.push(
                "ContinuousSolveSystem accepts a separately issued refresh owner".to_string(),
            );
        }
    }
}

impl<'ast> Visit<'ast> for SolveExecutionApiVisitor {
    fn visit_item_struct(&mut self, item: &'ast syn::ItemStruct) {
        let private_fields = matches!(
            item.ident.to_string().as_str(),
            "SolveArtifacts"
                | "ContinuousRefreshPlanInputs"
                | "ContinuousSolveSystemInputs"
                | "ExactRefreshAssignmentExecution"
                | "ExactRefreshAssignmentExecutionProgram"
                | "PureExplicitStateCount"
                | "RefreshRowOwnerId"
                | "RefreshRowSelection"
                | "RefreshSequenceId"
                | "IssuedRefreshPlan"
        );
        if private_fields {
            self.record_public_fields(item);
        }
        let name = item.ident.to_string();
        if name == "ContinuousSolveSystem"
            && item
                .fields
                .iter()
                .any(|field| matches!(field.vis, syn::Visibility::Public(_)))
        {
            self.violations
                .push("ContinuousSolveSystem exposes publicly mutable semantic fields".to_string());
        }
        let derives_forbidden_capability = item.attrs.iter().any(|attribute| {
            attribute.path().is_ident("derive")
                && attribute
                    .meta
                    .to_token_stream()
                    .to_string()
                    .split([',', '(', ')'])
                    .map(str::trim)
                    .any(|derive| {
                        derive == "Default"
                            && matches!(
                                name.as_str(),
                                "SolveArtifacts"
                                    | "SolveArtifactInputs"
                                    | "ContinuousRefreshPlanInputs"
                                    | "ContinuousSolveSystemInputs"
                                    | "ExactRefreshAssignmentExecution"
                                    | "ExactRefreshAssignmentExecutionProgram"
                                    | "PureExplicitStateCount"
                                    | "RefreshRowOwnerId"
                                    | "RefreshRowSelection"
                                    | "RefreshSequenceId"
                                    | "IssuedRefreshPlan"
                                    | "RefreshPlan"
                            )
                            || matches!(derive, "Serialize" | "Deserialize")
                                && matches!(
                                    name.as_str(),
                                    "SolveArtifacts"
                                        | "ExactRefreshAssignmentExecution"
                                        | "ExactRefreshAssignmentExecutionProgram"
                                        | "PureExplicitStateCount"
                                        | "RefreshSequenceId"
                                        | "IssuedRefreshPlan"
                                )
                    })
        });
        if derives_forbidden_capability {
            self.violations
                .push(format!("{} derives a fabrication/wire trait", item.ident));
        }
        visit::visit_item_struct(self, item);
    }

    fn visit_item_fn(&mut self, item: &'ast syn::ItemFn) {
        let inputs = item.sig.inputs.to_token_stream().to_string();
        if matches!(item.vis, syn::Visibility::Public(_)) {
            if inputs.contains("ExactRefreshAssignmentExecution") {
                self.opaque_refresh_entries += 1;
            }
            if inputs.contains("ExactRefreshAssignmentSchedule")
                || inputs.contains("ContinuousRefreshOwners")
                || (inputs.contains("Vec < LinearOp >") && inputs.contains("targets"))
            {
                self.violations.push(format!(
                    "public function `{}` accepts an independently assembled refresh input",
                    item.sig.ident
                ));
            }
        }
        if matches!(
            item.sig.ident.to_string().as_str(),
            "compile_assignment_schedule"
                | "compile_exact_assignment_schedule"
                | "compile_exact_assignment_schedule_with_pure_calls"
                | "final_scalar_program"
                | "plan_row"
                | "plan_rows"
                | "residual_program_output_count"
                | "max_reg_index"
                | "validate_row_sources"
        ) {
            self.violations
                .push(format!("retired function `{}` returned", item.sig.ident));
        }
        if matches!(
            item.sig.ident.to_string().as_str(),
            "compile_residual_rows"
                | "compile_residual_rows_with_pure_calls"
                | "compile_jacobian_rows"
                | "compile_jacobian_rows_with_pure_calls"
        ) && item
            .sig
            .inputs
            .to_token_stream()
            .to_string()
            .contains("Vec < LinearOp >")
        {
            self.violations.push(format!(
                "scalar-program compiler `{}` accepts unissued row storage",
                item.sig.ident
            ));
        }
        visit::visit_item_fn(self, item);
    }

    fn visit_impl_item_fn(&mut self, item: &'ast syn::ImplItemFn) {
        if matches!(
            item.sig.ident.to_string().as_str(),
            "compile_exact_assignment_schedule"
                | "compile_exact_assignment_schedule_with_pure_calls"
                | "final_scalar_program"
        ) {
            self.violations
                .push(format!("retired method `{}` returned", item.sig.ident));
        }
        visit::visit_impl_item_fn(self, item);
    }

    fn visit_item_impl(&mut self, item: &'ast syn::ItemImpl) {
        let self_name = match item.self_ty.as_ref() {
            syn::Type::Path(path) => path
                .path
                .segments
                .last()
                .map(|segment| segment.ident.to_string()),
            _ => None,
        };
        let trait_name = item
            .trait_
            .as_ref()
            .and_then(|(_, path, _)| path.segments.last())
            .map(|segment| segment.ident.to_string());
        if matches!(
            self_name.as_deref(),
            Some("ContinuousRefreshOwners" | "ContinuousSolveSystem")
        ) && trait_name.as_deref() == Some("Deserialize")
        {
            self.violations.push(format!(
                "{} exposes a standalone wire construction route",
                self_name.as_deref().expect("matched owner name")
            ));
        }
        if self_name.as_deref() == Some("ContinuousSolveSystemWire") {
            self.violations.push(
                "ContinuousSolveSystemWire exposes a standalone replay implementation".to_string(),
            );
        }
        self.record_owner_methods(self_name.as_deref(), &item.items);
        visit::visit_item_impl(self, item);
    }
}

fn solve_execution_api_violations(sources: &[&str]) -> Vec<String> {
    solve_execution_api_report(sources).violations
}

fn solve_execution_api_report(sources: &[&str]) -> SolveExecutionApiVisitor {
    let mut visitor = SolveExecutionApiVisitor::default();
    for source in sources {
        visitor.visit_file(&syn::parse_file(source).expect("parse Solve execution boundary"));
    }
    visitor
}

/// The only production Cranelift assignment entry consumes one opaque view
/// issued by `SolveModel`; the correlated artifacts and backend capability
/// cannot regain public fields, defaults, wire construction, or loose inputs.
#[test]
fn solve_execution_capabilities_and_deleted_routes_stay_sealed() {
    let root = workspace_root();
    let sources = [
        "rumoca-ir-solve",
        "rumoca-eval-solve",
        "rumoca-phase-solve",
        "rumoca-exec-cranelift",
        "rumoca-exec-mlir",
        "rumoca-sim",
        "rumoca-solver",
    ]
    .into_iter()
    .flat_map(|crate_name| {
        production_rust_sources(&root.join(format!("crates/{crate_name}")), &root)
            .into_iter()
            .map(|(_, source)| source)
    })
    .collect::<Vec<_>>();
    let source_refs = sources.iter().map(String::as_str).collect::<Vec<_>>();
    let report = solve_execution_api_report(&source_refs);
    assert!(
        report.violations.is_empty(),
        "sealed Solve execution API regressed: {:?}",
        report.violations
    );
    assert_eq!(
        report.opaque_refresh_entries, 1,
        "Cranelift must expose exactly one opaque exact-refresh entry"
    );
}

#[test]
fn solve_execution_api_gate_kills_manual_route_and_capability_mutations() {
    for mutation in [
        "pub fn compile_assignment_schedule(rows: &[Vec<LinearOp>], targets: &[usize]) {}",
        "fn compile_residual_rows(rows: &[Vec<LinearOp>]) {}",
        "fn plan_row(row: &[LinearOp]) {}",
        "fn residual_program_output_count(row: &[LinearOp]) -> usize { 0 }",
        "fn max_reg_index(op: LinearOp) -> usize { 0 }",
        "fn validate_row_sources(row: &[LinearOp]) {}",
        "impl Owner { pub fn final_scalar_program(&self, source: usize) {} }",
        "#[derive(Default)] pub struct PureExplicitStateCount(usize);",
        "#[derive(Default)] pub struct SolveArtifactInputs;",
        "#[derive(Default)] pub struct ContinuousRefreshPlanInputs;",
        "#[derive(Default)] pub struct ContinuousSolveSystemInputs;",
        "#[derive(Default)] pub struct RefreshPlan { rows: Vec<usize> }",
        "#[derive(Default)] pub struct RefreshRowOwnerId(u32);",
        "#[derive(Default)] pub struct RefreshRowSelection(Vec<u32>);",
        "#[derive(Deserialize)] pub struct IssuedRefreshPlan { rows: Vec<usize> }",
        "impl<'de> Deserialize<'de> for ContinuousRefreshOwners {}",
        "impl<'de> Deserialize<'de> for ContinuousSolveSystem {}",
        "impl ContinuousSolveSystemWire { fn checked(self, layout: &SolveLayout) {} }",
        "pub struct RefreshSequenceId(pub u64);",
        "#[derive(Serialize)] pub struct ExactRefreshAssignmentExecution<'a> { field: &'a [usize] }",
        "pub struct SolveArtifacts { pub continuous: ContinuousSolveArtifacts }",
        "pub struct ContinuousSolveSystem { pub implicit_rhs: ComputeBlock }",
        "impl ContinuousRefreshOwners { pub fn mint_foreign() -> Self { loop {} } }",
        "impl ContinuousSolveSystem { pub fn construct(refresh_owners: ContinuousRefreshOwners) {} }",
    ] {
        assert!(
            !solve_execution_api_violations(&[mutation]).is_empty(),
            "Solve execution API gate missed mutation: {mutation}"
        );
    }
}

#[derive(Default)]
struct SolveProgramFieldDefinitions {
    fields: BTreeMap<String, Vec<(String, syn::Type)>>,
}

impl<'ast> Visit<'ast> for SolveProgramFieldDefinitions {
    fn visit_item_struct(&mut self, item: &'ast syn::ItemStruct) {
        let fields = item
            .fields
            .iter()
            .enumerate()
            .map(|(ordinal, field)| {
                let name = field
                    .ident
                    .as_ref()
                    .map_or_else(|| ordinal.to_string(), ToString::to_string);
                (name, field.ty.clone())
            })
            .collect::<Vec<_>>();
        self.fields.insert(item.ident.to_string(), fields);
        visit::visit_item_struct(self, item);
    }

    fn visit_item_enum(&mut self, item: &'ast syn::ItemEnum) {
        let mut fields = Vec::new();
        for variant in &item.variants {
            for (ordinal, field) in variant.fields.iter().enumerate() {
                let field_name = field
                    .ident
                    .as_ref()
                    .map_or_else(|| ordinal.to_string(), ToString::to_string);
                fields.push((format!("{}.{field_name}", variant.ident), field.ty.clone()));
            }
        }
        self.fields.insert(item.ident.to_string(), fields);
        visit::visit_item_enum(self, item);
    }
}

#[derive(Default)]
struct TypeIdentifierVisitor {
    identifiers: BTreeSet<String>,
}

impl<'ast> Visit<'ast> for TypeIdentifierVisitor {
    fn visit_path_segment(&mut self, segment: &'ast syn::PathSegment) {
        self.identifiers.insert(segment.ident.to_string());
        visit::visit_path_segment(self, segment);
    }
}

fn type_identifiers(field_type: &syn::Type) -> BTreeSet<String> {
    let mut visitor = TypeIdentifierVisitor::default();
    visitor.visit_type(field_type);
    visitor.identifiers
}

fn solve_program_field_inventory(root: &Path) -> BTreeSet<String> {
    let crate_root = root.join("crates/rumoca-ir-solve");
    let mut definitions = SolveProgramFieldDefinitions::default();
    for (_, source) in production_rust_sources(&crate_root, root) {
        definitions.visit_file(&syn::parse_file(&source).expect("parse Solve production source"));
    }
    solve_program_field_inventory_from_definitions(&definitions)
}

fn solve_program_field_inventory_from_definitions(
    definitions: &SolveProgramFieldDefinitions,
) -> BTreeSet<String> {
    let mut pending = VecDeque::from([
        "SolveModel".to_string(),
        "SolveProblem".to_string(),
        "SolveArtifactInputs".to_string(),
    ]);
    let mut reached = BTreeSet::new();
    let mut inventory = BTreeSet::new();
    while let Some(owner) = pending.pop_front() {
        if !reached.insert(owner.clone()) {
            continue;
        }
        let Some(fields) = definitions.fields.get(&owner) else {
            continue;
        };
        for (field, field_type) in fields {
            let identifiers = type_identifiers(field_type);
            if identifiers.iter().any(|name| {
                matches!(
                    name.as_str(),
                    "ComputeBlock" | "ScalarProgramBlock" | "LinearOp"
                )
            }) {
                inventory.insert(format!("{owner}.{field}"));
            }
            pending.extend(
                identifiers
                    .into_iter()
                    .filter(|name| definitions.fields.contains_key(name)),
            );
        }
    }
    inventory
}

#[derive(Default)]
struct ScalarProgramVisitorOwnerCalls {
    owners: BTreeSet<String>,
}

impl ScalarProgramVisitorOwnerCalls {
    fn record_owner(&mut self, expression: &syn::Expr) {
        let path = match expression {
            syn::Expr::Path(path) => Some(&path.path),
            syn::Expr::Call(call) => match call.func.as_ref() {
                syn::Expr::Path(path) => Some(&path.path),
                _ => None,
            },
            _ => None,
        };
        let Some(path) = path else {
            return;
        };
        if path
            .segments
            .last()
            .is_some_and(|segment| segment.ident == "scalar_program_owner_for_compute_block")
        {
            self.owners.insert("ComputeBlock".to_string());
            return;
        }
        let mut segments = path.segments.iter().rev();
        let Some(owner) = segments.next() else {
            return;
        };
        if segments
            .next()
            .is_some_and(|segment| segment.ident == "ScalarProgramBlockOwner")
        {
            self.owners.insert(owner.ident.to_string());
        }
    }
}

impl<'ast> Visit<'ast> for ScalarProgramVisitorOwnerCalls {
    fn visit_item_mod(&mut self, item: &'ast syn::ItemMod) {
        let is_test_module = item.attrs.iter().any(|attribute| {
            attribute.path().is_ident("cfg")
                && attribute
                    .meta
                    .to_token_stream()
                    .to_string()
                    .contains("test")
        });
        if !is_test_module {
            visit::visit_item_mod(self, item);
        }
    }

    fn visit_expr_method_call(&mut self, call: &'ast syn::ExprMethodCall) {
        if call.method == "visit_scalar_program_block"
            && let Some(owner) = call.args.first()
        {
            self.record_owner(owner);
        }
        visit::visit_expr_method_call(self, call);
    }
}

fn scalar_program_visitor_owner_calls(source: &str) -> BTreeSet<String> {
    let mut visitor = ScalarProgramVisitorOwnerCalls::default();
    visitor.visit_file(&syn::parse_file(source).expect("parse canonical Solve visitor"));
    visitor.owners
}

/// The recursive owner graph is the architecture backstop for the closed
/// `SolveVisitor` catalog. A newly stored executable program must acquire one
/// reviewed owner before it can enter a sealed Solve root.
#[test]
fn solve_program_storage_inventory_matches_the_closed_owner_catalog() {
    let actual = solve_program_field_inventory(&workspace_root());
    let expected = BTreeSet::from([
        "ContinuousSolveArtifacts.full_jacobian_v".to_string(),
        "ContinuousSolveArtifacts.implicit_jacobian_v".to_string(),
        "ContinuousSolveArtifacts.implicit_jacobian_v_scalar".to_string(),
        "ContinuousSolveArtifacts.manifold_jacobian_v".to_string(),
        "ContinuousSolveSystem.derivative_rhs".to_string(),
        "ContinuousSolveSystem.implicit_rhs".to_string(),
        "ContinuousSolveSystem.manifold_residual".to_string(),
        "ContinuousSolveSystem.residual".to_string(),
        "DiscreteSolveSystem.clock_partition_intermediates".to_string(),
        "DiscreteSolveSystem.post_commit_assignment_rhs".to_string(),
        "DiscreteSolveSystem.rhs".to_string(),
        "DiscreteSolveSystem.runtime_assignment_rhs".to_string(),
        "DiscreteSolveSystem.structured_rhs".to_string(),
        "ExactRefreshAssignmentProgram.final_program".to_string(),
        "FunctionConditionalArmProgram.condition".to_string(),
        "FunctionConditionalArmProgram.result".to_string(),
        "FunctionConditionalProgram.fallback".to_string(),
        "FunctionFoldProgram.update".to_string(),
        "GuardedAssignmentProgram.program".to_string(),
        "InitializationSolveArtifacts.residual_jacobian_v".to_string(),
        "InitializationSolveSystem.residual".to_string(),
        "InitializationSolveSystem.update_rhs".to_string(),
        "ScalarProgramBlock.programs".to_string(),
        "SolveDelayPartition.delay_max_rhs".to_string(),
        "SolveDelayPartition.delay_time_rhs".to_string(),
        "SolveDelayPartition.source_rhs".to_string(),
        "SolveEventMessagePart.Conversion.value".to_string(),
        "SolveEventPartition.action_conditions".to_string(),
        "SolveEventPartition.dynamic_time_event_rhs".to_string(),
        "SolveEventPartition.root_conditions".to_string(),
        "SolveModel.visible_value_rows".to_string(),
        "SolveStringConversionFormat.Options.left_justified".to_string(),
        "SolveStringConversionFormat.Options.minimum_length".to_string(),
        "SolveStringConversionFormat.Options.significant_digits".to_string(),
        "ComputeNode.AffineStencil.base_ops".to_string(),
        "ComputeNode.LinSolve.setup_ops".to_string(),
        "ComputeNode.Map.base_ops".to_string(),
        "ComputeNode.MatMul.lhs_ops".to_string(),
        "ComputeNode.MatMul.rhs_ops".to_string(),
        "ComputeNode.ScalarPrograms.0".to_string(),
    ]);
    assert_eq!(actual, expected, "Solve program owner catalog drifted");
}

#[test]
fn stored_scalar_program_owners_are_all_reached_by_the_canonical_visitor() {
    let visitor_source =
        fs::read_to_string(workspace_root().join("crates/rumoca-ir-solve/src/visitor.rs"))
            .expect("read canonical Solve visitor");
    let actual = scalar_program_visitor_owner_calls(&visitor_source);
    let expected = BTreeSet::from([
        "ComputeBlock".to_string(),
        "InitializationUpdateRhs".to_string(),
        "DiscreteRuntimeAssignmentRhs".to_string(),
        "DiscretePostCommitAssignmentRhs".to_string(),
        "DiscreteRhs".to_string(),
        "DiscreteClockPartitionIntermediates".to_string(),
        "EventRootConditions".to_string(),
        "EventDynamicTimeEventRhs".to_string(),
        "EventActionConditions".to_string(),
        "EventDelaySourceRhs".to_string(),
        "EventDelayTimeRhs".to_string(),
        "EventDelayMaxRhs".to_string(),
        "ContinuousExactRefreshAssignmentFinalProgram".to_string(),
        "ContinuousFullJacobianV".to_string(),
        "ContinuousImplicitJacobianVScalar".to_string(),
        "VisibleValueRows".to_string(),
    ]);
    assert_eq!(
        actual, expected,
        "stored scalar-program inventory and canonical visitor owner calls drifted"
    );
}

#[test]
fn solve_program_storage_inventory_detects_direct_and_nested_owner_mutations() {
    let source = r"
        struct SolveModel { problem: SolveProblem }
        struct SolveProblem { direct: ScalarProgramBlock, nested: AddedOwner }
        struct AddedOwner { rows: Vec<LinearOp> }
        struct ScalarProgramBlock { programs: Vec<Vec<LinearOp>> }
        enum LinearOp { Const }
    ";
    let mut definitions = SolveProgramFieldDefinitions::default();
    definitions.visit_file(&syn::parse_file(source).expect("parse owner mutation"));

    let inventory = solve_program_field_inventory_from_definitions(&definitions);

    assert!(inventory.contains("SolveProblem.direct"));
    assert!(inventory.contains("AddedOwner.rows"));
    assert!(inventory.contains("ScalarProgramBlock.programs"));
}

#[test]
fn scalar_program_visitor_inventory_detects_an_unwalked_stored_owner() {
    let mutation = r#"
        enum ScalarProgramBlockOwner { Existing, Added }
        fn walk(visitor: &mut Visitor, existing: &ScalarProgramBlock) {
            visitor.visit_scalar_program_block(ScalarProgramBlockOwner::Existing, existing);
        }
    "#;
    let reached = scalar_program_visitor_owner_calls(mutation);
    assert!(reached.contains("Existing"));
    assert!(!reached.contains("Added"));
}

#[test]
fn fictional_refresh_catalog_label_stays_deleted() {
    let root = workspace_root();
    let stale = ["AS-", "070"].concat();
    for path in [
        root.join("spec/SPEC_0043_CONSTRUCTION_CATALOG.md"),
        root.join("crates/rumoca/tests/architecture_hardening_test/solver_backend_boundary.rs"),
    ] {
        let source = fs::read_to_string(&path).expect("read refresh construction contract");
        assert!(
            !source.contains(&stale),
            "fictional refresh catalog label returned in {}",
            path.display()
        );
    }
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

#[derive(Default)]
struct PreparedSimulationFacts {
    private_root: bool,
    public_producers: usize,
    consuming_run: bool,
    initialization_checks: usize,
    selected_batch_admissions: usize,
    capability_probe_admissions: usize,
    mode_dispatches: Vec<(BTreeSet<String>, bool)>,
}

impl PreparedSimulationFacts {
    /// Record the `PreparedSimulation` methods this census counts.
    ///
    /// Split out of `visit_item_impl` so the per-member classification is not
    /// nested inside the type-match guard; the cases and their effects are
    /// unchanged.
    fn record_prepared_simulation_members(&mut self, members: &[syn::ImplItem]) {
        for member in members {
            let syn::ImplItem::Fn(method) = member else {
                continue;
            };
            if method.sig.ident == "run" {
                self.consuming_run = method
                    .sig
                    .receiver()
                    .is_some_and(|receiver| receiver.reference.is_none());
            }
            if method.sig.ident == "check_initialization" {
                self.initialization_checks += 1;
            }
        }
    }
}

impl<'ast> Visit<'ast> for PreparedSimulationFacts {
    fn visit_item_struct(&mut self, item: &'ast syn::ItemStruct) {
        if item.ident == "PreparedSimulation" {
            self.private_root = item
                .fields
                .iter()
                .all(|field| matches!(field.vis, syn::Visibility::Inherited));
        }
        visit::visit_item_struct(self, item);
    }

    fn visit_item_fn(&mut self, item: &'ast syn::ItemFn) {
        if matches!(item.vis, syn::Visibility::Public(_))
            && item
                .sig
                .output
                .to_token_stream()
                .to_string()
                .contains("PreparedSimulation")
        {
            self.public_producers += 1;
        }
        visit::visit_item_fn(self, item);
    }

    fn visit_item_impl(&mut self, item: &'ast syn::ItemImpl) {
        let is_prepared = matches!(item.self_ty.as_ref(), syn::Type::Path(path)
            if path.path.segments.last().is_some_and(|segment| segment.ident == "PreparedSimulation"));
        if is_prepared {
            self.record_prepared_simulation_members(&item.items);
        }
        visit::visit_item_impl(self, item);
    }

    fn visit_expr_method_call(&mut self, call: &'ast syn::ExprMethodCall) {
        if call.method == "into_batch_admission" {
            self.selected_batch_admissions += 1;
        }
        if call.method == "admit_batch" {
            self.capability_probe_admissions += 1;
        }
        visit::visit_expr_method_call(self, call);
    }

    fn visit_expr_match(&mut self, expression: &'ast syn::ExprMatch) {
        if expression.expr.to_token_stream().to_string() == "opts . solver_mode" {
            let variants = expression
                .arms
                .iter()
                .filter_map(|arm| match &arm.pat {
                    syn::Pat::Path(path) => path.path.segments.last(),
                    _ => None,
                })
                .map(|segment| segment.ident.to_string())
                .collect();
            let has_wildcard = expression
                .arms
                .iter()
                .any(|arm| matches!(arm.pat, syn::Pat::Wild(_)));
            self.mode_dispatches.push((variants, has_wildcard));
        }
        visit::visit_expr_match(self, expression);
    }
}

fn prepared_simulation_facts(source: &str) -> PreparedSimulationFacts {
    let mut facts = PreparedSimulationFacts::default();
    facts.visit_file(&syn::parse_file(source).expect("parse prepared-simulation boundary"));
    facts
}

#[derive(Default)]
struct WorkerPreparedDispatchFacts {
    canonical_prepares: usize,
    legacy_diffsol_builds: usize,
}

impl<'ast> Visit<'ast> for WorkerPreparedDispatchFacts {
    fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
        if let syn::Expr::Path(path) = call.func.as_ref()
            && let Some(function) = path.path.segments.last()
        {
            if function.ident == "prepare_simulation" {
                self.canonical_prepares += 1;
            }
            if function.ident == "build_simulation"
                || function.ident == "build_simulation_with_stage_timing"
            {
                self.legacy_diffsol_builds += 1;
            }
        }
        visit::visit_expr_call(self, call);
    }
}

fn worker_prepared_dispatch_facts(source: &str) -> WorkerPreparedDispatchFacts {
    let mut facts = WorkerPreparedDispatchFacts::default();
    facts.visit_file(&syn::parse_file(source).expect("parse MSL worker dispatch boundary"));
    facts
}

/// Assert the censused dispatch-shape facts for the prepared and worker sources.
fn assert_prepared_dispatch_facts(
    facts: &PreparedSimulationFacts,
    worker_facts: &WorkerPreparedDispatchFacts,
    facade: &str,
) {
    assert_eq!(
        facts.public_producers, 1,
        "exactly one public function may construct a PreparedSimulation"
    );
    assert!(
        facts.private_root,
        "PreparedSimulation fields must remain private"
    );
    assert!(
        facts.consuming_run,
        "PreparedSimulation::run must consume self"
    );
    assert_eq!(
        facts.initialization_checks, 0,
        "repeatable initialization checks are forbidden"
    );
    assert_eq!(
        facts.selected_batch_admissions, 1,
        "the selected preparation must consume exactly one batch admission"
    );
    assert_eq!(
        facts.capability_probe_admissions, 1,
        "Auto owns one distinct borrowed capability-probe admission"
    );
    assert_eq!(
        facts.mode_dispatches,
        vec![(
            BTreeSet::from(["Auto".to_owned(), "Bdf".to_owned(), "RkLike".to_owned()]),
            false
        )],
        "the canonical prepared path must exhaustively dispatch every solver mode without a wildcard"
    );
    assert!(
        facade.contains("prepared_simulation::prepare_artifact"),
        "the DAE batch facade must consume the canonical prepared product"
    );
    assert_eq!(
        worker_facts.canonical_prepares, 1,
        "the MSL worker must prepare through the canonical typed dispatch exactly once"
    );
    assert_eq!(
        worker_facts.legacy_diffsol_builds, 0,
        "the MSL worker must not bypass solver-mode dispatch through a Diffsol builder"
    );
}

/// Assert every retired root simulation entry and alias stays absent.
fn assert_retired_dispatch_entries_absent(facade: &str, prepared: &str, diffsol: &str, rk45: &str) {
    for retired in [
        "simulate_dae_with_diagnostics",
        "simulate_with_diagnostics_auto_nan_trace",
        "pub fn simulate(",
        "pub fn simulate_with_diagnostics(",
        "pub use simulate_dae as",
        "pub use simulate_with_diagnostics",
        "check_prepared_initialization",
        "check_initialization",
        "build_simulation_with_stage_timing",
        "build_simulation_artifact",
        "simulate_prepared",
        "run_prepared_simulation",
    ] {
        assert!(
            ![&facade, &prepared, &diffsol, &rk45]
                .iter()
                .any(|source| source.contains(retired)),
            "retired root simulation entry/alias must not return: `{retired}`"
        );
    }
}

/// Assert each backend source refuses the retired per-backend vocabulary.
fn assert_backend_sources_stay_dispatch_free(diffsol: &str, rk45: &str, session: &str) {
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

/// Assert the census detects duplicate-admission, bypass and repeatable-run mutants.
fn assert_prepared_dispatch_mutations_are_detected(prepared: &str) {
    for mutation in [
        prepared.replacen(
            "let admission = retained.into_batch_admission(batch_options(opts)?)?;",
            "let first = retained.into_batch_admission(batch_options(opts)?)?;\nlet admission = retained.into_batch_admission(batch_options(opts)?)?;",
            1,
        ),
        prepared.replacen(
            "match opts.solver_mode {",
            "match SimSolverMode::Bdf {",
            1,
        ),
        prepared.replacen("pub fn run(self)", "pub fn run(&mut self)", 1),
    ] {
        let changed = prepared_simulation_facts(&mutation);
        assert!(
            changed.selected_batch_admissions != 1
                || changed.mode_dispatches
                    != vec![(
                        BTreeSet::from([
                            "Auto".to_owned(),
                            "Bdf".to_owned(),
                            "RkLike".to_owned(),
                        ]),
                        false,
                    )]
                || !changed.consuming_run,
            "architecture gate missed a duplicate-admission, solver-bypass, or repeatable-run mutation"
        );
    }
}

/// The facade owns one opaque, once-initialized batch product. Concrete
/// backends cannot grow batch entries that bypass typed solver selection.
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
    let prepared = fs::read_to_string(root.join("crates/rumoca-sim/src/prepared_simulation.rs"))
        .expect("read canonical prepared simulation");
    let worker = fs::read_to_string(root.join("crates/rumoca-worker/src/bin/rumoca-worker.rs"))
        .expect("read MSL worker dispatch boundary");
    let facts = prepared_simulation_facts(&prepared);
    let worker_facts = worker_prepared_dispatch_facts(&worker);

    assert_prepared_dispatch_facts(&facts, &worker_facts, &facade);
    assert_retired_dispatch_entries_absent(&facade, &prepared, &diffsol, &rk45);
    assert_backend_sources_stay_dispatch_free(&diffsol, &rk45, &session);
    assert_prepared_dispatch_mutations_are_detected(&prepared);

    let worker_bypass = worker.replacen(
        "prepare_simulation(",
        "build_simulation_with_stage_timing(",
        1,
    );
    let changed_worker = worker_prepared_dispatch_facts(&worker_bypass);
    assert!(
        changed_worker.canonical_prepares != 1 || changed_worker.legacy_diffsol_builds != 0,
        "architecture gate missed an MSL worker solver-mode bypass mutation"
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

struct NativeExecutionSources {
    runtime_source: String,
    runtime: String,
    production_runtime_modules: String,
    fmi: String,
    solve_events: String,
    sim_native: String,
    diffsol: String,
    eval_solve: String,
    prepared_eval: String,
}

fn native_execution_sources(root: &Path) -> NativeExecutionSources {
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
    NativeExecutionSources {
        runtime_source,
        runtime,
        production_runtime_modules,
        fmi: fs::read_to_string(root.join("crates/rumoca-solver/src/fmi_me.rs"))
            .expect("read FMI ME boundary"),
        solve_events: fs::read_to_string(
            root.join("crates/rumoca-solver/src/runtime/solve_events.rs"),
        )
        .expect("read Solve event scheduling boundary"),
        sim_native: fs::read_to_string(root.join("crates/rumoca-sim/src/native_execution.rs"))
            .expect("read native facade adapter"),
        diffsol: fs::read_to_string(root.join("crates/rumoca-sim/src/diffsol.rs"))
            .expect("read diffsol facade"),
        eval_solve: fs::read_to_string(root.join("crates/rumoca-eval-solve/src/lib.rs"))
            .expect("read Solve reference evaluator"),
        prepared_eval: fs::read_to_string(root.join("crates/rumoca-eval-solve/src/prepared.rs"))
            .expect("read prepared Solve evaluator"),
    }
}

fn assert_native_execution_owner_inventory(sources: &NativeExecutionSources) {
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
            sources.runtime.contains(required),
            "sealed per-operation execution plan lost `{required}`"
        );
    }
    assert!(
        sources.fmi.contains("pub enum MeExecutionSelection"),
        "FMI construction must receive a closed interpreter/native selection"
    );
}

fn assert_interpreter_context_ownership(sources: &NativeExecutionSources) {
    assert!(
        interpreter_contexts_are_confined(
            &sources.runtime_source,
            &sources.production_runtime_modules,
            &sources.solve_events,
        ),
        "runtime and event evaluator sites must consume operation-specific typed owners"
    );
    assert!(
        sources
            .runtime_source
            .matches("pure_calls: Some(model.pure_calls())")
            .count()
            == 2,
        "the dynamic-time-event and preparation permits must each carry the model pure-call table"
    );

    let mut raw_context_mutation = sources.production_runtime_modules.clone();
    raw_context_mutation.push_str("fn bypass() { let _ = RowEvalContext::default(); }");
    assert!(
        !interpreter_contexts_are_confined(
            &sources.runtime_source,
            &raw_context_mutation,
            &sources.solve_events,
        ),
        "the context confinement gate must reject an unowned default-context mutation"
    );
    let mut event_context_mutation = sources.solve_events.clone();
    event_context_mutation.push_str("fn bypass() { let _ = RowEvalContext { }; }");
    assert!(
        !interpreter_contexts_are_confined(
            &sources.runtime_source,
            &sources.production_runtime_modules,
            &event_context_mutation,
        ),
        "the event confinement gate must reject a raw-context mutation"
    );
    let mut root_context_mutation = sources.runtime_source.clone();
    root_context_mutation.push_str("fn bypass() { let _ = RowEvalContext::default(); }");
    assert!(
        !interpreter_contexts_are_confined(
            &root_context_mutation,
            &sources.production_runtime_modules,
            &sources.solve_events,
        ),
        "the context confinement gate must reject a raw context outside a permit"
    );
    let mut permit_forgery_mutation = sources.solve_events.clone();
    permit_forgery_mutation
        .push_str("fn bypass() { let _ = DynamicTimeEventsPermit { _brand: PermitBrand }; }");
    assert!(
        !interpreter_contexts_are_confined(
            &sources.runtime_source,
            &sources.production_runtime_modules,
            &permit_forgery_mutation,
        ),
        "the context gate must reject a sibling module minting a typed permit"
    );
}

fn assert_native_execution_tombstones(sources: &NativeExecutionSources) {
    let joined = format!(
        "{}\n{}\n{}\n{}\n{}\n{}\n{}",
        sources.runtime,
        sources.solve_events,
        sources.fmi,
        sources.sim_native,
        sources.diffsol,
        sources.eval_solve,
        sources.prepared_eval,
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

#[test]
fn native_execution_plan_cannot_regrow_demotion_or_interpreter_retry() {
    let root = workspace_root();
    let sources = native_execution_sources(&root);
    assert_native_execution_owner_inventory(&sources);
    assert_interpreter_context_ownership(&sources);
    assert_native_execution_tombstones(&sources);
}
