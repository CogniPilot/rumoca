use std::fs;

use crate::architecture_hardening_support::{
    attributes_require_test, production_rust_sources, workspace_root,
};
use syn::visit::{self, Visit};

fn implementation_type_name(implementation: &syn::ItemImpl) -> Option<&syn::Ident> {
    let syn::Type::Path(path) = implementation.self_ty.as_ref() else {
        return None;
    };
    path.path.segments.last().map(|segment| &segment.ident)
}

fn return_type_contains(output: &syn::ReturnType, expected: &str) -> bool {
    let syn::ReturnType::Type(_, ty) = output else {
        return false;
    };
    type_contains(ty, expected)
}

fn type_contains(ty: &syn::Type, expected: &str) -> bool {
    match ty {
        syn::Type::Path(path) => path.path.segments.iter().any(|segment| {
            segment.ident == expected
                || matches!(&segment.arguments, syn::PathArguments::AngleBracketed(arguments)
                    if arguments.args.iter().any(|argument| matches!(argument,
                        syn::GenericArgument::Type(ty) if type_contains(ty, expected))))
        }),
        syn::Type::Reference(reference) => type_contains(&reference.elem, expected),
        syn::Type::Tuple(tuple) => tuple.elems.iter().any(|ty| type_contains(ty, expected)),
        syn::Type::Paren(paren) => type_contains(&paren.elem, expected),
        syn::Type::Group(group) => type_contains(&group.elem, expected),
        syn::Type::Array(array) => type_contains(&array.elem, expected),
        syn::Type::Slice(slice) => type_contains(&slice.elem, expected),
        _ => false,
    }
}

#[test]
fn connection_projection_can_only_commit_after_consuming_stream_seal() {
    let source = fs::read_to_string(
        workspace_root().join("crates/rumoca-phase-flatten/src/connections/transaction.rs"),
    )
    .expect("read connection transaction source");
    let syntax = syn::parse_file(&source).expect("parse connection transaction source");

    let mut sealed_methods = Vec::new();
    let mut consuming_seal = false;
    let mut test_only_stream_free_seal = false;
    let mut production_sealed_constructors = Vec::new();
    for item in &syntax.items {
        if let syn::Item::Fn(function) = item
            && !attributes_require_test(&function.attrs)
            && return_type_contains(&function.sig.output, "SealedConnectionProjection")
        {
            production_sealed_constructors.push(function.sig.ident.to_string());
        }
        let syn::Item::Impl(implementation) = item else {
            continue;
        };
        let Some(owner) = implementation_type_name(implementation) else {
            continue;
        };
        for item in &implementation.items {
            let syn::ImplItem::Fn(method) = item else {
                continue;
            };
            let test_only = attributes_require_test(&method.attrs);
            if owner == "SealedConnectionProjection" && !test_only {
                sealed_methods.push(method.sig.ident.to_string());
            }
            if owner == "OpenConnectionProjection" && method.sig.ident == "seal_stream_rewrite" {
                consuming_seal = !test_only
                    && matches!(
                        method.sig.inputs.first(),
                        Some(syn::FnArg::Receiver(receiver)) if receiver.reference.is_none()
                    )
                    && return_type_contains(&method.sig.output, "SealedConnectionProjection");
            }
            if !test_only && return_type_contains(&method.sig.output, "SealedConnectionProjection")
            {
                production_sealed_constructors.push(format!("{owner}::{}", method.sig.ident));
            }
            if owner == "OpenConnectionProjection"
                && method.sig.ident == "seal_without_stream"
                && test_only
            {
                test_only_stream_free_seal = true;
            }
        }
    }

    sealed_methods.sort();
    assert_eq!(
        sealed_methods,
        vec!["commit".to_string()],
        "a sealed connection projection must expose no append or re-seal operation"
    );
    assert!(
        consuming_seal,
        "stream sealing must consume the open projection and return the sealed type"
    );
    assert_eq!(
        production_sealed_constructors,
        vec!["OpenConnectionProjection::seal_stream_rewrite".to_string()],
        "no second production constructor may issue a sealed connection projection"
    );
    assert!(
        test_only_stream_free_seal,
        "the distinct stream-free test projection must remain explicitly test-only"
    );
}

#[derive(Default)]
struct CapabilityLiteralVisitor {
    names: Vec<String>,
}

impl<'ast> Visit<'ast> for CapabilityLiteralVisitor {
    fn visit_expr_struct(&mut self, expression: &'ast syn::ExprStruct) {
        if let Some(name) = expression.path.segments.last()
            && matches!(
                name.ident.to_string().as_str(),
                "ConnectionDeclarationEvidence" | "ConnectionDeclarationOwner"
            )
        {
            self.names.push(name.ident.to_string());
        }
        visit::visit_expr_struct(self, expression);
    }
}

#[test]
fn connection_declaration_capabilities_have_one_literal_owner() {
    let root = workspace_root();
    let crate_root = root.join("crates/rumoca-phase-flatten");
    let mut offenders = Vec::new();
    for (path, source) in production_rust_sources(&crate_root, &root) {
        let syntax = syn::parse_file(&source)
            .unwrap_or_else(|error| panic!("parse {}: {error}", path.display()));
        let mut visitor = CapabilityLiteralVisitor::default();
        visitor.visit_file(&syntax);
        for name in visitor.names {
            let expected = match name.as_str() {
                "ConnectionDeclarationEvidence" => "connections/selection_evidence.rs",
                "ConnectionDeclarationOwner" => "connections/transaction.rs",
                _ => unreachable!("visitor records only connection declaration capabilities"),
            };
            if !path.to_string_lossy().ends_with(expected) {
                offenders.push(format!("{} constructs {name}", path.display()));
            }
        }
    }

    assert!(
        offenders.is_empty(),
        "connection declaration capabilities have one construction owner: {offenders:?}"
    );

    for (relative, name) in [
        (
            "src/connections/selection_evidence.rs",
            "ConnectionDeclarationEvidence",
        ),
        (
            "src/connections/transaction.rs",
            "ConnectionDeclarationOwner",
        ),
    ] {
        let source = fs::read_to_string(crate_root.join(relative))
            .unwrap_or_else(|error| panic!("read {relative}: {error}"));
        let syntax =
            syn::parse_file(&source).unwrap_or_else(|error| panic!("parse {relative}: {error}"));
        let item = syntax.items.iter().find_map(|item| match item {
            syn::Item::Struct(item) if item.ident == name => Some(item),
            _ => None,
        });
        let item = item.unwrap_or_else(|| panic!("{relative} owns `{name}`"));
        assert!(
            item.fields
                .iter()
                .all(|field| matches!(field.vis, syn::Visibility::Inherited)),
            "`{name}` capability fields must remain private to {relative}"
        );
    }
}
