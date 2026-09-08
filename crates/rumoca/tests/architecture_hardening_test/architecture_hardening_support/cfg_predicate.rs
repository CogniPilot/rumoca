//! Whether a `cfg` predicate can hold in a production build.
//!
//! Every architecture scan asks one question of a conditional item: can it be
//! compiled into a shipped artifact? A production build fixes `test` and `kani`
//! false and leaves ordinary feature flags free, so the honest answer is
//! three-valued: absent from every production build, present in every one, or
//! dependent on a flag this evaluator does not resolve.
//!
//! Only [`ProductionVisibility::AbsentInProduction`] exempts an item from a
//! scan. Both other answers keep it scanned, which is why an unrecognised,
//! malformed or unparseable predicate reads as
//! [`ProductionVisibility::FlagDependent`]: a predicate this evaluator cannot
//! understand must never quietly remove code from a gate. The failure direction
//! is the whole point. A wrong `FlagDependent` is a noisy scan, a wrong
//! `AbsentInProduction` is a gate that silently stops looking.
//!
//! This is a conservative evaluator over the parsed attribute, not a decision
//! procedure. It judges each occurrence of a flag independently, so
//! `all(feature = "a", not(feature = "a"))` reads as flag-dependent rather than
//! absent, and no caller may describe it as deciding satisfiability.
//!
//! The input is always the parsed [`syn::Meta`]. Reading the predicate from
//! flattened token text cannot survive a value containing a comma or a
//! parenthesis, so the source-text entry point parses before it judges.

use syn::parse::Parser as _;
use syn::punctuated::Punctuated;

/// The flags a production build fixes to false. Every other flag stays free.
const PRODUCTION_FALSE_FLAGS: [&str; 2] = ["test", "kani"];

/// Whether an item carrying a `cfg` predicate reaches a production build.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum ProductionVisibility {
    /// False in every production build, whatever the free flags are.
    AbsentInProduction,
    /// True in every production build, whatever the free flags are.
    PresentInProduction,
    /// Decided by a flag this evaluator does not resolve.
    FlagDependent,
}

impl ProductionVisibility {
    /// Whether an item carrying this predicate must still be scanned.
    ///
    /// Only a predicate that is false in *every* production build may be
    /// exempted; flag-dependent code ships whenever the flag is on.
    pub(crate) fn reaches_production(self) -> bool {
        !matches!(self, Self::AbsentInProduction)
    }

    fn negated(self) -> Self {
        match self {
            Self::AbsentInProduction => Self::PresentInProduction,
            Self::PresentInProduction => Self::AbsentInProduction,
            Self::FlagDependent => Self::FlagDependent,
        }
    }
}

/// The visibility of the single predicate inside one `cfg(..)` list.
pub(crate) fn cfg_list_visibility(list: &syn::MetaList) -> ProductionVisibility {
    if !list.path.is_ident("cfg") {
        return ProductionVisibility::FlagDependent;
    }
    match parse_operands(list).as_deref() {
        // `cfg` takes exactly one predicate; anything else is malformed and is
        // retained rather than exempted.
        Some([predicate]) => predicate_visibility(predicate),
        _ => ProductionVisibility::FlagDependent,
    }
}

/// The visibility of one attribute as it appears in source text.
///
/// A line that is not an attribute at all, or that carries no `cfg`, reaches
/// production: the scans call this on every line and must keep the ones they
/// cannot classify.
pub(crate) fn attribute_text_visibility(text: &str) -> ProductionVisibility {
    let Ok(attributes) = syn::Attribute::parse_outer.parse_str(text.trim()) else {
        return ProductionVisibility::FlagDependent;
    };
    let mut visibility = ProductionVisibility::PresentInProduction;
    for attribute in &attributes {
        let syn::Meta::List(list) = &attribute.meta else {
            continue;
        };
        if !list.path.is_ident("cfg") {
            continue;
        }
        match cfg_list_visibility(list) {
            ProductionVisibility::AbsentInProduction => {
                return ProductionVisibility::AbsentInProduction;
            }
            ProductionVisibility::FlagDependent => {
                visibility = ProductionVisibility::FlagDependent;
            }
            ProductionVisibility::PresentInProduction => {}
        }
    }
    visibility
}

/// Whether one `cfg_attr(..)` can set a module `path` in a production build.
///
/// `cfg_attr(predicate, attr, ..)` applies its attributes only where the
/// predicate holds, so both halves decide the answer: a predicate absent from
/// production sets nothing, and a predicate that reaches production sets a path
/// only if one of its attributes is `path`.
pub(crate) fn cfg_attr_sets_production_path(list: &syn::MetaList) -> bool {
    if !list.path.is_ident("cfg_attr") {
        return false;
    }
    let Some(operands) = parse_operands(list) else {
        return false;
    };
    let Some((predicate, applied)) = operands.split_first() else {
        return false;
    };
    predicate_visibility(predicate).reaches_production() && applied.iter().any(attribute_sets_path)
}

/// Whether one attribute applied by a `cfg_attr` sets a module `path`.
///
/// A nested `cfg_attr` is followed rather than ignored, because
/// `cfg_attr(a, cfg_attr(b, path = ".."))` sets a path exactly as the flat form
/// does. Reading the parsed attribute means `doc = "path"` is no longer counted,
/// which the replaced substring scan could not distinguish.
fn attribute_sets_path(attribute: &syn::Meta) -> bool {
    if attribute.path().is_ident("path") {
        return true;
    }
    match attribute {
        syn::Meta::List(nested) if nested.path.is_ident("cfg_attr") => {
            cfg_attr_sets_production_path(nested)
        }
        _ => false,
    }
}

fn predicate_visibility(meta: &syn::Meta) -> ProductionVisibility {
    match meta {
        syn::Meta::Path(path) => {
            if PRODUCTION_FALSE_FLAGS
                .iter()
                .any(|flag| path.is_ident(flag))
            {
                ProductionVisibility::AbsentInProduction
            } else {
                ProductionVisibility::FlagDependent
            }
        }
        // `feature = "x"`, `target_os = "linux"` and friends: free.
        syn::Meta::NameValue(_) => ProductionVisibility::FlagDependent,
        syn::Meta::List(list) => list_visibility(list),
    }
}

fn list_visibility(list: &syn::MetaList) -> ProductionVisibility {
    let Some(operands) = parse_operands(list) else {
        return ProductionVisibility::FlagDependent;
    };
    if list.path.is_ident("all") {
        return conjunction_visibility(&operands);
    }
    if list.path.is_ident("any") {
        return disjunction_visibility(&operands);
    }
    if list.path.is_ident("not") {
        return match operands.as_slice() {
            [operand] => predicate_visibility(operand).negated(),
            // `not` takes exactly one operand; a malformed one is retained.
            _ => ProductionVisibility::FlagDependent,
        };
    }
    // An unrecognised predicate form, such as `target_has_atomic("8")`.
    ProductionVisibility::FlagDependent
}

/// `all(..)` is false as soon as one operand is, and true only if every operand
/// is. The empty `all()` is true, which is what an empty conjunction means.
fn conjunction_visibility(operands: &[syn::Meta]) -> ProductionVisibility {
    let mut visibility = ProductionVisibility::PresentInProduction;
    for operand in operands {
        match predicate_visibility(operand) {
            ProductionVisibility::AbsentInProduction => {
                return ProductionVisibility::AbsentInProduction;
            }
            ProductionVisibility::FlagDependent => {
                visibility = ProductionVisibility::FlagDependent;
            }
            ProductionVisibility::PresentInProduction => {}
        }
    }
    visibility
}

/// `any(..)` is true as soon as one operand is, and false only if every operand
/// is false. The empty `any()` is false, which is what an empty disjunction
/// means, and is the one shape that exempts without naming a flag.
fn disjunction_visibility(operands: &[syn::Meta]) -> ProductionVisibility {
    let mut visibility = ProductionVisibility::AbsentInProduction;
    for operand in operands {
        match predicate_visibility(operand) {
            ProductionVisibility::PresentInProduction => {
                return ProductionVisibility::PresentInProduction;
            }
            ProductionVisibility::FlagDependent => {
                visibility = ProductionVisibility::FlagDependent;
            }
            ProductionVisibility::AbsentInProduction => {}
        }
    }
    visibility
}

fn parse_operands(list: &syn::MetaList) -> Option<Vec<syn::Meta>> {
    list.parse_args_with(Punctuated::<syn::Meta, syn::Token![,]>::parse_terminated)
        .ok()
        .map(|operands| operands.into_iter().collect())
}

/// Pins for both directions of the classification, and for the token shapes
/// the replaced string predicate could not read.
mod tests {
    use super::{ProductionVisibility, attribute_text_visibility};

    fn visibility(attribute: &str) -> ProductionVisibility {
        attribute_text_visibility(attribute)
    }

    /// The divergence this evaluator replaced: the string predicate recognised
    /// only bare `test` and `all(..)`, so a verification module gated on
    /// `any(test, kani)` was scanned as production.
    #[test]
    fn verification_flags_are_absent_from_production() {
        for attribute in [
            "#[cfg(test)]",
            "#[cfg(kani)]",
            "#[cfg(any(test, kani))]",
            "#[cfg(any(kani, test))]",
            "#[cfg(all(feature = \"fmi\", test))]",
            "#[cfg(all(feature = \"fmi\", any(test, kani)))]",
            "#[cfg(any(all(test, feature = \"fmi\"), kani))]",
        ] {
            assert_eq!(
                visibility(attribute),
                ProductionVisibility::AbsentInProduction,
                "`{attribute}` cannot be compiled into a production build"
            );
        }
    }

    /// The direction that must never regress. Exempting one of these stops a
    /// gate scanning real production code, and every gate keeps passing while
    /// it does, so each spelling is pinned, `kani` included.
    #[test]
    fn free_and_negated_flags_still_reach_production() {
        for attribute in [
            "#[cfg(feature = \"fmi\")]",
            "#[cfg(any(test, feature = \"fmi\"))]",
            "#[cfg(any(kani, feature = \"fmi\"))]",
            "#[cfg(not(test))]",
            "#[cfg(not(kani))]",
            "#[cfg(not(any(test, kani)))]",
            "#[cfg(all(not(test), feature = \"fmi\"))]",
            "#[cfg(any(all(test, kani), feature = \"fmi\"))]",
            "#[derive(Debug)]",
            "#[cfg(target_has_atomic = \"8\")]",
        ] {
            assert!(
                visibility(attribute).reaches_production(),
                "`{attribute}` can be compiled into a production build"
            );
        }
    }

    /// A value carrying `,` or `(` broke the flattened-token split that this
    /// evaluator replaced: the predicate was cut inside its own string literal.
    #[test]
    fn punctuation_inside_a_value_does_not_split_the_predicate() {
        assert_eq!(
            visibility("#[cfg(all(feature = \"a,b\", test))]"),
            ProductionVisibility::AbsentInProduction,
            "a comma inside a value must not hide the `test` conjunct"
        );
        assert!(
            visibility("#[cfg(any(feature = \"a,b\", test))]").reaches_production(),
            "a comma inside a value must not turn a free feature into an absent one"
        );
        assert!(
            visibility("#[cfg(feature = \"a(b)\")]").reaches_production(),
            "a parenthesis inside a value must not read as a nested predicate"
        );
        assert_eq!(
            visibility("#[cfg(all(feature = \"a)b\", kani))]"),
            ProductionVisibility::AbsentInProduction,
            "an unbalanced parenthesis inside a value must not hide the `kani` conjunct"
        );
    }

    /// Anything this evaluator cannot classify stays scanned. These are the
    /// shapes that would otherwise be exempted by accident.
    #[test]
    fn unparseable_and_malformed_predicates_are_retained() {
        for attribute in [
            "fn not_an_attribute() {",
            "#[cfg(",
            "#[cfg()]",
            "#[cfg(test, kani)]",
            "#[cfg(not(test, kani))]",
            "#[cfg(all())]",
        ] {
            assert!(
                visibility(attribute).reaches_production(),
                "`{attribute}` is not understood, so it must stay scanned"
            );
        }
    }

    /// The one shape that is absent without naming a flag. Kept explicit so a
    /// later reader does not read the empty-disjunction case as an oversight.
    #[test]
    fn an_empty_disjunction_is_absent() {
        assert_eq!(
            visibility("#[cfg(any())]"),
            ProductionVisibility::AbsentInProduction,
            "`any()` is false, so nothing carrying it is ever built"
        );
    }
}
