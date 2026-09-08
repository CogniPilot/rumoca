//! Tombstones independent ordinary-equation classification after DAE analysis.

use std::fs;
use std::path::Path;

use syn::punctuated::Punctuated;
use syn::visit::{self, Visit};
use syn::{GenericArgument, PathArguments, Token, Type};

use super::architecture_hardening_support::{collect_rs_files, workspace_root};

const PARTITION_SOURCE: &str =
    "crates/rumoca-phase-dae/src/construction/analysis/equation_partitions.rs";
const ANALYSIS_SOURCE: &str = "crates/rumoca-phase-dae/src/construction/analysis.rs";
const LOWERING_SOURCE: &str = "crates/rumoca-phase-dae/src/construction/equation_lowering.rs";
const CONSTRUCTION_SOURCE: &str = "crates/rumoca-phase-dae/src/construction.rs";

struct Sources {
    partition: String,
    analysis: String,
    lowering: String,
    construction: String,
}

impl Sources {
    fn read() -> Self {
        Self {
            partition: source(PARTITION_SOURCE),
            analysis: source(ANALYSIS_SOURCE),
            lowering: source(LOWERING_SOURCE),
            construction: source(CONSTRUCTION_SOURCE),
        }
    }
}

struct CallCounter<'name> {
    qualifier: Option<&'name str>,
    name: &'name str,
    calls: usize,
}

impl<'ast> Visit<'ast> for CallCounter<'_> {
    fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
        if let syn::Expr::Path(path) = call.func.as_ref() {
            let segments = path.path.segments.iter().collect::<Vec<_>>();
            let name_matches = segments
                .last()
                .is_some_and(|segment| segment.ident == self.name);
            let qualifier_matches = self.qualifier.is_none_or(|qualifier| {
                segments
                    .iter()
                    .rev()
                    .nth(1)
                    .is_some_and(|segment| segment.ident == qualifier)
            });
            if name_matches && qualifier_matches {
                self.calls += 1;
            }
        }
        visit::visit_expr_call(self, call);
    }
}

struct MethodCallCounter<'name> {
    name: &'name str,
    calls: usize,
}

struct FieldMethodCallCounter<'name> {
    base: &'name str,
    field: &'name str,
    method: &'name str,
    calls: usize,
}

impl<'ast> Visit<'ast> for FieldMethodCallCounter<'_> {
    fn visit_expr_method_call(&mut self, call: &'ast syn::ExprMethodCall) {
        let receiver_matches = matches!(
            call.receiver.as_ref(),
            syn::Expr::Field(field)
                if matches!(field.base.as_ref(), syn::Expr::Path(base)
                    if base.path.is_ident(self.base))
                    && matches!(&field.member, syn::Member::Named(name) if name == self.field)
        );
        if receiver_matches && call.method == self.method {
            self.calls += 1;
        }
        visit::visit_expr_method_call(self, call);
    }
}

impl<'ast> Visit<'ast> for MethodCallCounter<'_> {
    fn visit_expr_method_call(&mut self, call: &'ast syn::ExprMethodCall) {
        if call.method == self.name {
            self.calls += 1;
        }
        visit::visit_expr_method_call(self, call);
    }
}

fn source(relative: &str) -> String {
    let path = workspace_root().join(relative);
    fs::read_to_string(&path).unwrap_or_else(|error| panic!("read {}: {error}", path.display()))
}

fn syntax(source: &str) -> syn::File {
    syn::parse_file(source).unwrap_or_else(|error| panic!("parse phase-DAE source: {error}"))
}

fn call_count(source: &str, qualifier: Option<&str>, name: &str) -> usize {
    let syntax = syntax(source);
    let mut counter = CallCounter {
        qualifier,
        name,
        calls: 0,
    };
    counter.visit_file(&syntax);
    counter.calls
}

fn method_call_count(source: &str, name: &str) -> usize {
    let syntax = syntax(source);
    let mut counter = MethodCallCounter { name, calls: 0 };
    counter.visit_file(&syntax);
    counter.calls
}

fn field_method_call_count(source: &str, base: &str, field: &str, method: &str) -> usize {
    let syntax = syntax(source);
    let mut counter = FieldMethodCallCounter {
        base,
        field,
        method,
        calls: 0,
    };
    counter.visit_file(&syntax);
    counter.calls
}

fn path_ends_with(ty: &Type, expected: &str) -> bool {
    let Type::Path(path) = ty else {
        return false;
    };
    path.path
        .segments
        .last()
        .is_some_and(|segment| segment.ident == expected)
}

fn type_contains_reference(ty: &Type) -> bool {
    match ty {
        Type::Reference(_) => true,
        Type::Path(path) => path.path.segments.iter().any(|segment| {
            let PathArguments::AngleBracketed(arguments) = &segment.arguments else {
                return false;
            };
            arguments.args.iter().any(|argument| {
                matches!(argument, GenericArgument::Type(inner) if type_contains_reference(inner))
            })
        }),
        Type::Tuple(tuple) => tuple.elems.iter().any(type_contains_reference),
        _ => false,
    }
}

fn sequence_forbidden_derives(source: &str) -> Vec<String> {
    let syntax = syntax(source);
    let Some(item) = syntax.items.iter().find_map(|item| match item {
        syn::Item::Struct(item) if item.ident == "ModelEquationSequence" => Some(item),
        _ => None,
    }) else {
        return vec!["missing ModelEquationSequence".to_owned()];
    };
    item.attrs
        .iter()
        .filter(|attribute| attribute.path().is_ident("derive"))
        .flat_map(|attribute| {
            attribute
                .parse_args_with(Punctuated::<syn::Path, Token![,]>::parse_terminated)
                .unwrap_or_else(|error| panic!("parse ModelEquationSequence derive: {error}"))
        })
        .filter_map(|path| {
            let trait_name = path.segments.last()?.ident.to_string();
            matches!(trait_name.as_str(), "Clone" | "Copy" | "Default").then_some(trait_name)
        })
        .collect()
}

fn classifier_is_private(source: &str) -> bool {
    syntax(source).items.iter().any(|item| {
        matches!(
            item,
            syn::Item::Fn(function)
                if function.sig.ident == "equation_partition"
                    && matches!(function.vis, syn::Visibility::Inherited)
        )
    })
}

fn issuer_is_analysis_only(source: &str) -> bool {
    syntax(source).items.iter().any(|item| {
        let syn::Item::Impl(item) = item else {
            return false;
        };
        path_ends_with(&item.self_ty, "ModelEquationSequence")
            && item.items.iter().any(|item| {
                matches!(
                    item,
                    syn::ImplItem::Fn(method)
                        if method.sig.ident == "issue"
                            && matches!(&method.vis, syn::Visibility::Restricted(scope)
                                if scope.path.is_ident("super"))
                )
            })
    })
}

fn struct_has_sequence_field(source: &str, owner: &str) -> bool {
    syntax(source).items.iter().any(|item| {
        let syn::Item::Struct(item) = item else {
            return false;
        };
        item.ident == owner
            && item
                .fields
                .iter()
                .any(|field| path_ends_with(&field.ty, "ModelEquationSequence"))
    })
}

fn analyzed_model_has_deref(source: &str) -> bool {
    syntax(source).items.iter().any(|item| {
        let syn::Item::Impl(item) = item else {
            return false;
        };
        item.trait_
            .as_ref()
            .and_then(|(_, path, _)| path.segments.last())
            .is_some_and(|segment| segment.ident == "Deref")
            && path_ends_with(&item.self_ty, "AnalyzedModel")
    })
}

fn model_source_is_owned(source: &str) -> bool {
    syntax(source).items.iter().any(|item| {
        let syn::Item::Enum(item) = item else {
            return false;
        };
        item.ident == "EquationSource"
            && item.variants.iter().any(|variant| {
                variant.ident == "Model"
                    && variant.fields.iter().any(|field| {
                        path_ends_with(&field.ty, "ModelEquationSequence")
                            && !type_contains_reference(&field.ty)
                    })
            })
    })
}

fn build_checked_consumes_analysis(source: &str) -> bool {
    syntax(source).items.iter().any(|item| {
        let syn::Item::Fn(item) = item else {
            return false;
        };
        item.sig.ident == "build_checked"
            && item.sig.inputs.iter().any(|argument| {
                let syn::FnArg::Typed(argument) = argument else {
                    return false;
                };
                matches!(
                    argument.pat.as_ref(),
                    syn::Pat::Ident(ident) if ident.ident == "analyzed"
                ) && path_ends_with(&argument.ty, "AnalyzedModel")
                    && !type_contains_reference(&argument.ty)
            })
    })
}

fn boundary_violations(sources: &Sources) -> Vec<&'static str> {
    let mut violations = Vec::new();
    if !classifier_is_private(&sources.partition) {
        violations.push("classifier must be private");
    }
    if !issuer_is_analysis_only(&sources.partition) {
        violations.push("issuer must be analysis-only");
    }
    if call_count(&sources.partition, None, "equation_partition") != 1 {
        violations.push("classifier must have one caller");
    }
    if call_count(&sources.analysis, Some("ModelEquationSequence"), "issue") != 1 {
        violations.push("sequence must have one exact issuer");
    }
    if !sequence_forbidden_derives(&sources.partition).is_empty() {
        violations.push("sequence must be affine");
    }
    if field_method_call_count(
        &sources.partition,
        "aggregate_connections",
        "owners",
        "remove",
    ) != 1
        || field_method_call_count(
            &sources.partition,
            "aggregate_connections",
            "members",
            "remove",
        ) != 1
        || field_method_call_count(
            &sources.partition,
            "aggregate_connections",
            "owners",
            "into_iter",
        ) != 1
        || field_method_call_count(
            &sources.partition,
            "aggregate_connections",
            "members",
            "into_iter",
        ) != 1
    {
        violations.push("aggregate authority must move and be exhausted");
    }
    if struct_has_sequence_field(&sources.analysis, "Analysis")
        || !struct_has_sequence_field(&sources.analysis, "AnalyzedModel")
        || analyzed_model_has_deref(&sources.analysis)
    {
        violations.push("sequence must belong only to affine analyzed model");
    }
    if !model_source_is_owned(&sources.lowering) {
        violations.push("model source must own sequence");
    }
    // One `into_parts` lowers an immediate row and the other moves a deferred
    // structured row out of its keyed store. No third consumer may exist.
    if method_call_count(&sources.lowering, "into_rows") != 1
        || method_call_count(&sources.lowering, "into_parts") != 2
        || method_call_count(&sources.lowering, "rows") != 0
        || method_call_count(&sources.lowering, "partition") != 0
    {
        violations.push("lowering must consume each sequence row and partition");
    }
    if !build_checked_consumes_analysis(&sources.construction) {
        violations.push("final construction must consume analyzed model");
    }
    if call_count(&sources.analysis, None, "equation_partition") != 0
        || call_count(&sources.lowering, None, "equation_partition") != 0
        || call_count(&sources.construction, None, "equation_partition") != 0
    {
        violations.push("downstream code must not reclassify rows");
    }
    if sources
        .lowering
        .contains("analysis already validates equation ownership")
    {
        violations.push("deleted ownership proof panic must stay deleted");
    }
    violations
}

fn construction_reclassification_paths(root: &Path) -> Vec<String> {
    let partition = root.join(PARTITION_SOURCE);
    let mut files = Vec::new();
    collect_rs_files(
        &root.join("crates/rumoca-phase-dae/src/construction"),
        &mut files,
    );
    let mut offenders = files
        .into_iter()
        .filter(|path| path != &partition)
        .filter_map(|path| {
            let contents = fs::read_to_string(&path)
                .unwrap_or_else(|error| panic!("read {}: {error}", path.display()));
            (call_count(&contents, None, "equation_partition") != 0).then(|| {
                path.strip_prefix(root)
                    .unwrap_or(&path)
                    .display()
                    .to_string()
            })
        })
        .collect::<Vec<_>>();
    offenders.sort();
    offenders
}

#[test]
fn ordinary_equation_partition_has_one_affine_authority() {
    let sources = Sources::read();
    assert_eq!(boundary_violations(&sources), Vec::<&str>::new());
    let offenders = construction_reclassification_paths(&workspace_root());
    assert!(
        offenders.is_empty(),
        "ordinary equation roles must only be read from ModelEquationSequence: {offenders:?}"
    );
}

#[test]
fn mutations_kill_public_clone_borrow_reuse_and_reclassification_routes() {
    let baseline = Sources::read();

    let mut mutation = Sources::read();
    mutation.partition = mutation.partition.replacen(
        "fn equation_partition<'flat>",
        "pub(super) fn equation_partition<'flat>",
        1,
    );
    assert!(boundary_violations(&mutation).contains(&"classifier must be private"));

    let mut mutation = Sources::read();
    mutation.partition = mutation.partition.replacen(
        "pub(super) fn issue(",
        "pub(in crate::construction) fn issue(",
        1,
    );
    assert!(boundary_violations(&mutation).contains(&"issuer must be analysis-only"));

    let mut mutation = Sources::read();
    mutation.partition = mutation.partition.replacen(
        "pub(in crate::construction) struct ModelEquationSequence",
        "#[derive(Clone)]\npub(in crate::construction) struct ModelEquationSequence",
        1,
    );
    assert!(boundary_violations(&mutation).contains(&"sequence must be affine"));

    let mut mutation = Sources::read();
    mutation.partition = mutation.partition.replacen(
        "aggregate_connections.owners.remove(&index)",
        "aggregate_connections.owners.get(&index).cloned()",
        1,
    );
    assert!(
        boundary_violations(&mutation).contains(&"aggregate authority must move and be exhausted")
    );

    let mut mutation = Sources::read();
    mutation.analysis.push_str(
        "\nimpl std::ops::Deref for AnalyzedModel<'_> {\n\
         type Target = Analysis<'_>;\n\
         fn deref(&self) -> &Self::Target { &self.analysis }\n\
         }\n",
    );
    assert!(
        boundary_violations(&mutation)
            .contains(&"sequence must belong only to affine analyzed model")
    );

    let mut mutation = Sources::read();
    mutation.lowering = mutation.lowering.replacen(
        "Model(ModelEquationSequence<'scope>)",
        "Model(&'scope ModelEquationSequence<'scope>)",
        1,
    );
    assert!(boundary_violations(&mutation).contains(&"model source must own sequence"));

    let mut mutation = Sources::read();
    mutation.lowering =
        mutation
            .lowering
            .replacen("sequence.into_rows()", "sequence.rows().iter()", 1);
    assert!(
        boundary_violations(&mutation)
            .contains(&"lowering must consume each sequence row and partition")
    );

    let mut mutation = Sources::read();
    mutation.lowering = mutation.lowering.replacen(
        "let (index, equation, partition) = row.into_parts();",
        "let _duplicate = row.into_parts();\n                let (index, equation, partition) = row.into_parts();",
        1,
    );
    assert!(
        boundary_violations(&mutation)
            .contains(&"lowering must consume each sequence row and partition")
    );

    let mut mutation = Sources::read();
    mutation.lowering = mutation.lowering.replacen(
        "for row in sequence.into_rows() {",
        "let _duplicate = sequence.into_rows();\n            for row in sequence.into_rows() {",
        1,
    );
    assert!(
        boundary_violations(&mutation)
            .contains(&"lowering must consume each sequence row and partition")
    );

    let mut mutation = Sources::read();
    mutation.lowering = mutation.lowering.replacen(
        "let environment = EquationRowEnvironment {",
        "let _forbidden = equation_partition();\n    let environment = EquationRowEnvironment {",
        1,
    );
    assert!(boundary_violations(&mutation).contains(&"downstream code must not reclassify rows"));

    let mut mutation = Sources::read();
    mutation.lowering = mutation.lowering.replacen(
        "let environment = EquationRowEnvironment {",
        "let _forbidden = Result::<(), ()>::Ok(())\n        .expect(\"analysis already validates equation ownership\");\n    let environment = EquationRowEnvironment {",
        1,
    );
    assert!(
        boundary_violations(&mutation).contains(&"deleted ownership proof panic must stay deleted")
    );

    assert_eq!(boundary_violations(&baseline), Vec::<&str>::new());
}
