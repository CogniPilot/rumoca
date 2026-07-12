//! Projection input and options.
//!
//! [`GalecInput`] borrows the untouched canonical DAE (GAL-002: the
//! projection never mutates it) plus the auxiliary provenance the DAE does
//! not carry itself (GAL-003/D4: extra data rides *beside* the DAE, never in
//! it). No `rumoca-ir-solve` input is needed for the fixed-sample discrete
//! subset: admissibility, classification, and manifest population all read
//! canonical DAE fields directly, and the flattened `f_z`/`f_m`/`f_c`
//! partitions already carry the guarded update form the lowering slice
//! consumes. Should equation scheduling ever require Solve output, an
//! optional field is added here rather than changing existing ones.
//!
//! # Scalar-type provenance
//!
//! `rumoca_ir_dae::Variable` deliberately carries no scalar type: DAE
//! partitions fix `Real` for `x`/`y`/`u`/`w`/`z`, and the runtime treats
//! every slot numerically. eFMI manifests, however, are typed
//! (`RealVariable`/`IntegerVariable`/`BooleanVariable`), and GALEC has no
//! implicit promotion (trap T5). Parameters, constants, and discrete-valued
//! (`m`) variables are therefore typed via [`ScalarTypeMap`] provenance
//! built by the caller from Flat-side declared types (`flat::Variable::
//! type_id`), keyed by the DAE variable name. When the map is absent or has
//! no entry and the partition contract does not fix the type either,
//! classification fails with `ET011` — a type is never inferred from start
//! values and never defaulted (SPEC_0008, S8).

use std::collections::HashMap;

use rumoca_core::VarName;
use rumoca_ir_dae::Dae;
use rumoca_ir_galec::ast::ScalarType;

/// Declared scalar types of DAE variables, keyed by DAE variable name.
///
/// Built by the projection caller (`rumoca-compile`) from Flat-side declared
/// types; entries are authoritative and take precedence over partition
/// contracts. Generated `__pre__.` slots need no entries — they inherit the
/// type of their base variable.
pub type ScalarTypeMap = HashMap<VarName, ScalarType>;

/// Pinned eFMI profile the projection emits for (SPEC_0034 GAL-022).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum GalecProfile {
    /// eFMI Standard 1.0.0 Beta 1 (`efmi-1.0.0-beta-1`), the only profile
    /// currently supported.
    #[default]
    Efmi1_0_0Beta1,
}

impl GalecProfile {
    /// The eFMI profile string (GAL-022).
    #[must_use]
    pub const fn profile_string(self) -> &'static str {
        match self {
            Self::Efmi1_0_0Beta1 => crate::manifest_context::EFMI_PROFILE,
        }
    }
}

/// Options controlling the projection.
#[derive(Debug, Clone, Default)]
pub struct GalecOptions {
    /// Target eFMI profile.
    pub profile: GalecProfile,
    /// Override for the emitted GALEC block name. When `None`, the block is
    /// named after [`GalecInput::model_name`] (mangled per GAL-015 if the
    /// Modelica name is not a legal GALEC identifier).
    pub block_name: Option<String>,
}

/// Borrowed projection input: the untouched canonical DAE plus auxiliary
/// provenance (never stored, never mutated — GAL-002).
#[derive(Debug, Clone, Copy)]
pub struct GalecInput<'a> {
    /// The canonical DAE, read-only.
    pub dae: &'a Dae,
    /// Name of the compiled root model. The DAE itself carries no model
    /// name; the caller (CLI / `rumoca-compile` session) supplies it.
    pub model_name: &'a str,
    /// Declared scalar types from the Flat IR (see module docs). Optional:
    /// without it, only partition-typed variables resolve.
    pub scalar_types: Option<&'a ScalarTypeMap>,
}

impl<'a> GalecInput<'a> {
    /// Input over an untouched DAE without type provenance.
    #[must_use]
    pub fn new(dae: &'a Dae, model_name: &'a str) -> Self {
        Self {
            dae,
            model_name,
            scalar_types: None,
        }
    }

    /// Attach Flat-side scalar-type provenance.
    #[must_use]
    pub fn with_scalar_types(mut self, scalar_types: &'a ScalarTypeMap) -> Self {
        self.scalar_types = Some(scalar_types);
        self
    }
}
