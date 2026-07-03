//! GALEC name → C identifier mangling for the `embedded-c-galec` export
//! (SPEC_0034 GAL-024; injectivity discipline mirroring GAL-015).
//!
//! # Scheme
//!
//! The input is a GALEC [`Name`]'s manifest spelling ([`crate::mangle::manifest_name`]):
//! the plain identifier text, or the quoted-identifier content such as
//! `motor.emf.v` or `previous(y)`.
//!
//! 1. every ASCII alphanumeric character is kept; every other character
//!    (`.` of scalarized hierarchies, `(`/`)` of the `previous(x)` state
//!    convention, `[`/`]` of indexed quoted identifiers) becomes `_`;
//! 2. trailing `_` runs are trimmed (`previous(y)` → `previous_y`);
//! 3. a result equal to a C keyword or to a macro/typedef name of the
//!    headers the generated translation unit includes gets a `_` suffix
//!    (`if` → `if_`, `NAN` → `NAN_`).
//!
//! The map is deterministic but not injective on its own (`a.b` and `a_b`
//! both yield `a_b`), so [`CNameTable::build`] checks collisions over the
//! whole package and fails with a typed `ET022` — names are never silently
//! renamed apart (SPEC_0008: no silent defaults).

use std::collections::HashMap;

use rumoca_ir_galec::ast::{Block, Name};

use crate::diagnostic::GalecTargetError;
use crate::mangle::manifest_name;

/// C keywords (C11) plus the `<stdbool.h>` macro names; a mangled result
/// equal to one of these gets a `_` suffix (struct members and file-scope
/// identifiers must not be keywords).
const C_KEYWORDS: &[&str] = &[
    "alignas",
    "alignof",
    "auto",
    "bool",
    "break",
    "case",
    "char",
    "const",
    "continue",
    "default",
    "do",
    "double",
    "else",
    "enum",
    "extern",
    "false",
    "float",
    "for",
    "goto",
    "if",
    "inline",
    "int",
    "long",
    "register",
    "restrict",
    "return",
    "short",
    "signed",
    "sizeof",
    "static",
    "struct",
    "switch",
    "true",
    "typedef",
    "union",
    "unsigned",
    "void",
    "volatile",
    "while",
    "_Alignas",
    "_Alignof",
    "_Atomic",
    "_Bool",
    "_Complex",
    "_Generic",
    "_Imaginary",
    "_Noreturn",
    "_Static_assert",
    "_Thread_local",
];

/// Identifiers the generated translation unit's includes claim
/// (`<stdbool.h>`/`<stdint.h>` in the header, `<math.h>` in the source):
/// object-like macros rewrite any use site (`self->NAN` stops being a
/// member access), typedef names break member declarations (`double
/// int32_t;` parses as two type specifiers), and the function-like macros
/// are included defensively. A mangled result equal to one of these gets
/// the same `_` suffix as a keyword — otherwise the export would print C
/// that `cc` rejects instead of failing in rumoca (GAL-012/GAL-015).
/// Reserved-namespace spellings (leading `_`) are unreachable: mangled
/// names always start with an ASCII letter.
const C_RESERVED_IDENTIFIERS: &[&str] = &[
    // <math.h> object-like macros.
    "NAN",
    "INFINITY",
    "HUGE_VAL",
    "HUGE_VALF",
    "HUGE_VALL",
    "FP_INFINITE",
    "FP_NAN",
    "FP_NORMAL",
    "FP_SUBNORMAL",
    "FP_ZERO",
    "FP_FAST_FMA",
    "FP_FAST_FMAF",
    "FP_FAST_FMAL",
    "FP_ILOGB0",
    "FP_ILOGBNAN",
    "MATH_ERRNO",
    "MATH_ERREXCEPT",
    "math_errhandling",
    // <math.h> function-like classification/comparison macros.
    "fpclassify",
    "isfinite",
    "isinf",
    "isnan",
    "isnormal",
    "signbit",
    "isgreater",
    "isgreaterequal",
    "isless",
    "islessequal",
    "islessgreater",
    "isunordered",
    // <math.h> typedef names.
    "float_t",
    "double_t",
    // <stdint.h> typedef names.
    "int8_t",
    "int16_t",
    "int32_t",
    "int64_t",
    "uint8_t",
    "uint16_t",
    "uint32_t",
    "uint64_t",
    "int_least8_t",
    "int_least16_t",
    "int_least32_t",
    "int_least64_t",
    "uint_least8_t",
    "uint_least16_t",
    "uint_least32_t",
    "uint_least64_t",
    "int_fast8_t",
    "int_fast16_t",
    "int_fast32_t",
    "int_fast64_t",
    "uint_fast8_t",
    "uint_fast16_t",
    "uint_fast32_t",
    "uint_fast64_t",
    "intptr_t",
    "uintptr_t",
    "intmax_t",
    "uintmax_t",
    // <stdint.h> limit and constant macros.
    "INT8_MIN",
    "INT16_MIN",
    "INT32_MIN",
    "INT64_MIN",
    "INT8_MAX",
    "INT16_MAX",
    "INT32_MAX",
    "INT64_MAX",
    "UINT8_MAX",
    "UINT16_MAX",
    "UINT32_MAX",
    "UINT64_MAX",
    "INT_LEAST8_MIN",
    "INT_LEAST16_MIN",
    "INT_LEAST32_MIN",
    "INT_LEAST64_MIN",
    "INT_LEAST8_MAX",
    "INT_LEAST16_MAX",
    "INT_LEAST32_MAX",
    "INT_LEAST64_MAX",
    "UINT_LEAST8_MAX",
    "UINT_LEAST16_MAX",
    "UINT_LEAST32_MAX",
    "UINT_LEAST64_MAX",
    "INT_FAST8_MIN",
    "INT_FAST16_MIN",
    "INT_FAST32_MIN",
    "INT_FAST64_MIN",
    "INT_FAST8_MAX",
    "INT_FAST16_MAX",
    "INT_FAST32_MAX",
    "INT_FAST64_MAX",
    "UINT_FAST8_MAX",
    "UINT_FAST16_MAX",
    "UINT_FAST32_MAX",
    "UINT_FAST64_MAX",
    "INTPTR_MIN",
    "INTPTR_MAX",
    "UINTPTR_MAX",
    "INTMAX_MIN",
    "INTMAX_MAX",
    "UINTMAX_MAX",
    "PTRDIFF_MIN",
    "PTRDIFF_MAX",
    "SIG_ATOMIC_MIN",
    "SIG_ATOMIC_MAX",
    "SIZE_MAX",
    "WCHAR_MIN",
    "WCHAR_MAX",
    "WINT_MIN",
    "WINT_MAX",
    "INT8_C",
    "INT16_C",
    "INT32_C",
    "INT64_C",
    "UINT8_C",
    "UINT16_C",
    "UINT32_C",
    "UINT64_C",
    "INTMAX_C",
    "UINTMAX_C",
];

/// Deterministically mangle one GALEC name to a C identifier (module-docs
/// scheme).
///
/// # Errors
///
/// `ET023` when the name cannot begin a C identifier (empty or not
/// ASCII-letter-first — GALEC name legality makes this a projection bug,
/// but a corrupt name must fail loudly, never be patched up).
pub fn c_identifier(name: &Name) -> Result<String, GalecTargetError> {
    let spelling = manifest_name(name);
    let mut chars = spelling.chars();
    let first_is_letter = chars
        .next()
        .is_some_and(|first| first.is_ascii_alphabetic());
    if !first_is_letter {
        return Err(GalecTargetError::CExportUnsupported {
            construct: "a name that cannot begin a C identifier",
            detail: format!("GALEC name `{spelling}` does not start with an ASCII letter"),
        });
    }
    let mut mangled: String = spelling
        .chars()
        .map(|c| if c.is_ascii_alphanumeric() { c } else { '_' })
        .collect();
    while mangled.ends_with('_') {
        mangled.pop();
    }
    if C_KEYWORDS.contains(&mangled.as_str()) || C_RESERVED_IDENTIFIERS.contains(&mangled.as_str())
    {
        mangled.push('_');
    }
    Ok(mangled)
}

/// Collision-checked GALEC-name → C-identifier table over one package's
/// declared block variables (interface + protected sections), keyed by the
/// manifest spelling (injective per [`crate::mangle::manifest_name`]).
#[derive(Debug)]
pub struct CNameTable {
    by_spelling: HashMap<String, String>,
}

impl CNameTable {
    /// Build the table over every variable the block declares, failing with
    /// `ET022` on the first C-name collision (module docs).
    ///
    /// # Errors
    ///
    /// `ET022` on collision; `ET023` for names outside the C-manglable set.
    pub fn build(block: &Block) -> Result<Self, GalecTargetError> {
        let declared = block
            .interface
            .iter()
            .map(|variable| &variable.decl.name)
            .chain(block.protected.iter().map(|entity| &entity.decl.name));
        let mut by_spelling = HashMap::new();
        let mut owners: HashMap<String, String> = HashMap::new();
        for name in declared {
            let spelling = manifest_name(name).to_owned();
            let c_name = c_identifier(name)?;
            if let Some(first) = owners.get(&c_name)
                && first != &spelling
            {
                return Err(GalecTargetError::CNameCollision {
                    first: first.clone(),
                    second: spelling,
                    c_name,
                });
            }
            owners.insert(c_name.clone(), spelling.clone());
            by_spelling.insert(spelling, c_name);
        }
        Ok(Self { by_spelling })
    }

    /// The C identifier of a declared GALEC name.
    ///
    /// # Errors
    ///
    /// `ET018` for a name the block never declared — the lowering resolves
    /// every reference through the classification, so this is a projection
    /// bug.
    pub fn c_name(&self, name: &Name) -> Result<&str, GalecTargetError> {
        self.c_name_by_spelling(manifest_name(name))
    }

    /// [`Self::c_name`] keyed by the manifest spelling directly (the
    /// manifest variable list carries spellings, not [`Name`]s).
    ///
    /// # Errors
    ///
    /// `ET018` for an undeclared spelling (see [`Self::c_name`]).
    pub fn c_name_by_spelling(&self, spelling: &str) -> Result<&str, GalecTargetError> {
        self.by_spelling
            .get(spelling)
            .map(String::as_str)
            .ok_or_else(|| GalecTargetError::LoweringInternal {
                detail: format!(
                    "C export met a reference to `{spelling}`, which the block never declared"
                ),
            })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_ir_galec::ast::{ProtectedEntity, ProtectedKind, ScalarType, VariableDeclaration};

    fn block_with_states(names: &[Name]) -> Block {
        let mut block = Block::new(Name::ident("M"));
        block.protected = names
            .iter()
            .map(|name| ProtectedEntity {
                kind: ProtectedKind::State,
                decl: VariableDeclaration::scalar(ScalarType::Real, name.clone()),
                start: None,
            })
            .collect();
        block
    }

    #[test]
    fn plain_identifiers_pass_through() {
        assert_eq!(c_identifier(&Name::ident("gain")).unwrap(), "gain");
        assert_eq!(c_identifier(&Name::ident("y2")).unwrap(), "y2");
    }

    #[test]
    fn quoted_hierarchical_names_flatten_deterministically() {
        assert_eq!(
            c_identifier(&Name::quoted("motor.emf.v")).unwrap(),
            "motor_emf_v"
        );
        assert_eq!(
            c_identifier(&Name::quoted("x[2].y")).unwrap(),
            "x_2__y",
            "subscript brackets each map to `_` (no collapsing)"
        );
    }

    #[test]
    fn previous_slots_trim_the_trailing_parenthesis() {
        assert_eq!(
            c_identifier(&Name::quoted("previous(y)")).unwrap(),
            "previous_y"
        );
        assert_eq!(
            c_identifier(&Name::quoted("previous(pid.ix)")).unwrap(),
            "previous_pid_ix"
        );
    }

    #[test]
    fn c_keywords_get_a_suffix() {
        // `if`/`double` are GALEC-representable only as quoted identifiers.
        assert_eq!(c_identifier(&Name::quoted("if")).unwrap(), "if_");
        assert_eq!(c_identifier(&Name::quoted("double")).unwrap(), "double_");
        assert_eq!(c_identifier(&Name::ident("iff")).unwrap(), "iff");
    }

    #[test]
    fn non_letter_first_names_are_rejected_loudly() {
        let error = c_identifier(&Name::quoted("2fast")).unwrap_err();
        assert_eq!(error.code(), "ET023", "{error}");
    }

    #[test]
    fn table_is_injective_over_distinct_inputs() {
        let names = [
            Name::ident("gain"),
            Name::quoted("motor.v"),
            Name::quoted("previous(y)"),
            Name::ident("y"),
        ];
        let table = CNameTable::build(&block_with_states(&names)).unwrap();
        let mut seen = std::collections::HashSet::new();
        for name in &names {
            assert!(
                seen.insert(table.c_name(name).unwrap().to_owned()),
                "mangled names must stay pairwise distinct"
            );
        }
    }

    #[test]
    fn colliding_names_fail_with_a_typed_error_not_a_silent_rename() {
        let names = [Name::quoted("a.b"), Name::ident("a_b")];
        let error = CNameTable::build(&block_with_states(&names)).unwrap_err();
        let GalecTargetError::CNameCollision {
            first,
            second,
            c_name,
        } = &error
        else {
            panic!("expected ET022 CNameCollision, got: {error}");
        };
        assert_eq!(error.code(), "ET022");
        assert_eq!(c_name, "a_b");
        assert_ne!(first, second);
    }

    #[test]
    fn undeclared_references_are_a_loud_projection_bug() {
        let table = CNameTable::build(&block_with_states(&[Name::ident("y")])).unwrap();
        let error = table.c_name(&Name::ident("ghost")).unwrap_err();
        assert_eq!(error.code(), "ET018", "{error}");
    }
}
