//! Direct-syntax type facts for the success-root route inventory.
//!
//! These helpers deliberately do not resolve HIR or expand macros. The parent
//! gate records the resulting source boundary instead of treating these facts
//! as compiler proof.

use std::collections::BTreeSet;

use quote::ToTokens;
use syn::{
    GenericArgument, GenericParam, Generics, PathArguments, ReturnType, Type, WherePredicate,
};

use super::token_string_mentions;

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub(super) struct GenericRootCapabilities {
    pub(super) factory: bool,
    pub(super) mentions_root: bool,
    pub(super) mutates_root: bool,
}

impl GenericRootCapabilities {
    fn merge(&mut self, other: Self) {
        self.factory |= other.factory;
        self.mentions_root |= other.mentions_root;
        self.mutates_root |= other.mutates_root;
    }
}

pub(super) fn type_mentions_names(ty: &Type, names: &BTreeSet<String>) -> bool {
    match ty {
        Type::Array(array) => type_mentions_names(&array.elem, names),
        Type::BareFn(function) => {
            function
                .inputs
                .iter()
                .any(|input| type_mentions_names(&input.ty, names))
                || matches!(
                    &function.output,
                    ReturnType::Type(_, ty) if type_mentions_names(ty, names)
                )
        }
        Type::Group(group) => type_mentions_names(&group.elem, names),
        Type::ImplTrait(implementation) => implementation.bounds.iter().any(|bound| {
            let syn::TypeParamBound::Trait(bound) = bound else {
                return false;
            };
            path_mentions_names(&bound.path, names)
        }),
        Type::Infer(_) | Type::Never(_) => false,
        Type::Macro(item_macro) => names.iter().any(|name| {
            token_string_mentions(&item_macro.mac.tokens.to_token_stream().to_string(), name)
        }),
        Type::Paren(parenthesized) => type_mentions_names(&parenthesized.elem, names),
        Type::Path(path) => {
            path.qself
                .as_ref()
                .is_some_and(|qself| type_mentions_names(&qself.ty, names))
                || path_mentions_names(&path.path, names)
        }
        Type::Ptr(pointer) => type_mentions_names(&pointer.elem, names),
        Type::Reference(reference) => type_mentions_names(&reference.elem, names),
        Type::Slice(slice) => type_mentions_names(&slice.elem, names),
        Type::TraitObject(object) => object.bounds.iter().any(|bound| {
            let syn::TypeParamBound::Trait(bound) = bound else {
                return false;
            };
            path_mentions_names(&bound.path, names)
        }),
        Type::Tuple(tuple) => tuple
            .elems
            .iter()
            .any(|element| type_mentions_names(element, names)),
        _ => false,
    }
}

fn path_mentions_names(path: &syn::Path, names: &BTreeSet<String>) -> bool {
    path.segments.iter().any(|segment| {
        names.contains(&segment.ident.to_string())
            || match &segment.arguments {
                PathArguments::AngleBracketed(arguments) => arguments.args.iter().any(|argument| {
                    matches!(argument, GenericArgument::Type(ty) if type_mentions_names(ty, names))
                        || matches!(
                            argument,
                            GenericArgument::AssocType(association)
                                if type_mentions_names(&association.ty, names)
                        )
                        || matches!(
                            argument,
                            GenericArgument::Constraint(constraint)
                                if constraint.bounds.iter().any(|bound| {
                                    matches!(
                                        bound,
                                        syn::TypeParamBound::Trait(bound)
                                            if path_mentions_names(&bound.path, names)
                                    )
                                })
                        )
                }),
                PathArguments::Parenthesized(arguments) => {
                    arguments
                        .inputs
                        .iter()
                        .any(|ty| type_mentions_names(ty, names))
                        || matches!(
                            &arguments.output,
                            ReturnType::Type(_, ty) if type_mentions_names(ty, names)
                        )
                }
                PathArguments::None => false,
            }
    })
}

pub(super) fn generic_root_capabilities(
    generics: &Generics,
    carriers: &BTreeSet<String>,
) -> GenericRootCapabilities {
    let mut facts = GenericRootCapabilities::default();
    for parameter in &generics.params {
        match parameter {
            GenericParam::Type(parameter) => {
                for bound in &parameter.bounds {
                    facts.merge(bound_root_capabilities(bound, carriers));
                }
                if let Some(default) = &parameter.default
                    && type_mentions_names(default, carriers)
                {
                    facts.mentions_root = true;
                }
            }
            GenericParam::Const(parameter) => {
                if type_mentions_names(&parameter.ty, carriers) {
                    facts.mentions_root = true;
                }
            }
            GenericParam::Lifetime(_) => {}
        }
    }
    let Some(where_clause) = &generics.where_clause else {
        return facts;
    };
    for predicate in &where_clause.predicates {
        match predicate {
            WherePredicate::Type(predicate) => {
                if type_mentions_names(&predicate.bounded_ty, carriers) {
                    facts.mentions_root = true;
                }
                for bound in &predicate.bounds {
                    facts.merge(bound_root_capabilities(bound, carriers));
                }
            }
            WherePredicate::Lifetime(_) => {}
            _ => {}
        }
    }
    facts
}

fn bound_root_capabilities(
    bound: &syn::TypeParamBound,
    carriers: &BTreeSet<String>,
) -> GenericRootCapabilities {
    let syn::TypeParamBound::Trait(bound) = bound else {
        return GenericRootCapabilities::default();
    };
    let mentions_root = path_mentions_names(&bound.path, carriers);
    GenericRootCapabilities {
        factory: path_is_root_factory(&bound.path, carriers),
        mentions_root,
        mutates_root: path_callback_mutates_root(&bound.path, carriers)
            || (mentions_root && path_contains_mutable_capability(&bound.path)),
    }
}

pub(super) fn type_param_bound_mentions_names(
    bound: &syn::TypeParamBound,
    carriers: &BTreeSet<String>,
) -> bool {
    let syn::TypeParamBound::Trait(bound) = bound else {
        return false;
    };
    path_mentions_names(&bound.path, carriers)
}

pub(super) fn type_contains_reference(ty: &Type) -> bool {
    match ty {
        Type::Reference(_) => true,
        Type::Array(array) => type_contains_reference(&array.elem),
        Type::BareFn(function) => {
            function
                .inputs
                .iter()
                .any(|input| type_contains_reference(&input.ty))
                || matches!(
                    &function.output,
                    ReturnType::Type(_, ty) if type_contains_reference(ty)
                )
        }
        Type::Group(group) => type_contains_reference(&group.elem),
        Type::Paren(parenthesized) => type_contains_reference(&parenthesized.elem),
        Type::Path(path) => path.path.segments.iter().any(|segment| {
            match &segment.arguments {
                PathArguments::AngleBracketed(arguments) => arguments.args.iter().any(|argument| {
                    matches!(argument, GenericArgument::Type(ty) if type_contains_reference(ty))
                }),
                PathArguments::Parenthesized(arguments) => {
                    arguments.inputs.iter().any(type_contains_reference)
                        || matches!(
                            &arguments.output,
                            ReturnType::Type(_, ty) if type_contains_reference(ty)
                        )
                }
                PathArguments::None => false,
            }
        }),
        Type::Ptr(pointer) => type_contains_reference(&pointer.elem),
        Type::Slice(slice) => type_contains_reference(&slice.elem),
        Type::Tuple(tuple) => tuple.elems.iter().any(type_contains_reference),
        _ => false,
    }
}

pub(super) fn type_contains_mutable_capability(ty: &Type) -> bool {
    match ty {
        Type::Reference(reference) => {
            reference.mutability.is_some() || type_contains_mutable_capability(&reference.elem)
        }
        Type::Ptr(pointer) => {
            pointer.mutability.is_some() || type_contains_mutable_capability(&pointer.elem)
        }
        Type::Array(array) => type_contains_mutable_capability(&array.elem),
        Type::BareFn(function) => {
            function
                .inputs
                .iter()
                .any(|input| type_contains_mutable_capability(&input.ty))
                || matches!(
                    &function.output,
                    ReturnType::Type(_, ty) if type_contains_mutable_capability(ty)
                )
        }
        Type::Group(group) => type_contains_mutable_capability(&group.elem),
        Type::ImplTrait(implementation) => implementation.bounds.iter().any(|bound| {
            matches!(
                bound,
                syn::TypeParamBound::Trait(bound)
                    if path_contains_mutable_capability(&bound.path)
            )
        }),
        Type::Paren(parenthesized) => type_contains_mutable_capability(&parenthesized.elem),
        Type::Path(path) => path_contains_mutable_capability(&path.path),
        Type::Slice(slice) => type_contains_mutable_capability(&slice.elem),
        Type::TraitObject(object) => object.bounds.iter().any(|bound| {
            matches!(
                bound,
                syn::TypeParamBound::Trait(bound)
                    if path_contains_mutable_capability(&bound.path)
            )
        }),
        Type::Tuple(tuple) => tuple.elems.iter().any(type_contains_mutable_capability),
        _ => false,
    }
}

fn path_contains_mutable_capability(path: &syn::Path) -> bool {
    const MUTABLE_WRAPPERS: &[&str] = &[
        "AsMut",
        "BorrowMut",
        "Cell",
        "DerefMut",
        "FnMut",
        "FnOnce",
        "IndexMut",
        "MutexGuard",
        "NonNull",
        "RefCell",
        "RefMut",
        "RwLockWriteGuard",
        "UnsafeCell",
    ];
    path.segments.iter().any(|segment| {
        MUTABLE_WRAPPERS.iter().any(|name| segment.ident == *name)
            || match &segment.arguments {
                PathArguments::AngleBracketed(arguments) => arguments.args.iter().any(|argument| {
                    matches!(
                        argument,
                        GenericArgument::Type(ty) if type_contains_mutable_capability(ty)
                    )
                }),
                PathArguments::Parenthesized(arguments) => {
                    arguments
                        .inputs
                        .iter()
                        .any(type_contains_mutable_capability)
                        || matches!(
                            &arguments.output,
                            ReturnType::Type(_, ty) if type_contains_mutable_capability(ty)
                        )
                }
                PathArguments::None => false,
            }
    })
}

pub(super) fn type_is_root_factory(ty: &Type, carriers: &BTreeSet<String>) -> bool {
    match ty {
        Type::BareFn(function) => matches!(
            &function.output,
            ReturnType::Type(_, ty) if type_mentions_names(ty, carriers)
        ),
        Type::Group(group) => type_is_root_factory(&group.elem, carriers),
        Type::Paren(parenthesized) => type_is_root_factory(&parenthesized.elem, carriers),
        Type::ImplTrait(implementation) => implementation.bounds.iter().any(|bound| {
            matches!(
                bound,
                syn::TypeParamBound::Trait(bound)
                    if path_is_root_factory(&bound.path, carriers)
            )
        }),
        Type::Path(path) => path_is_root_factory(&path.path, carriers),
        Type::TraitObject(object) => object.bounds.iter().any(|bound| {
            matches!(
                bound,
                syn::TypeParamBound::Trait(bound)
                    if path_is_root_factory(&bound.path, carriers)
            )
        }),
        _ => false,
    }
}

fn path_is_root_factory(path: &syn::Path, carriers: &BTreeSet<String>) -> bool {
    path.segments.iter().any(|segment| {
        let PathArguments::Parenthesized(arguments) = &segment.arguments else {
            return false;
        };
        matches!(
            &arguments.output,
            ReturnType::Type(_, ty) if type_mentions_names(ty, carriers)
        )
    })
}

pub(super) fn type_callback_mutates_root(ty: &Type, carriers: &BTreeSet<String>) -> bool {
    match ty {
        Type::BareFn(function) => function.inputs.iter().any(|input| {
            type_mentions_names(&input.ty, carriers) && type_contains_mutable_capability(&input.ty)
        }),
        Type::ImplTrait(implementation) => implementation.bounds.iter().any(|bound| {
            matches!(
                bound,
                syn::TypeParamBound::Trait(bound)
                    if path_callback_mutates_root(&bound.path, carriers)
            )
        }),
        Type::Path(path) => path_callback_mutates_root(&path.path, carriers),
        Type::TraitObject(object) => object.bounds.iter().any(|bound| {
            matches!(
                bound,
                syn::TypeParamBound::Trait(bound)
                    if path_callback_mutates_root(&bound.path, carriers)
            )
        }),
        _ => false,
    }
}

fn path_callback_mutates_root(path: &syn::Path, carriers: &BTreeSet<String>) -> bool {
    path.segments.iter().any(|segment| {
        let PathArguments::Parenthesized(arguments) = &segment.arguments else {
            return false;
        };
        arguments.inputs.iter().any(|input| {
            type_mentions_names(input, carriers) && type_contains_mutable_capability(input)
        })
    })
}
