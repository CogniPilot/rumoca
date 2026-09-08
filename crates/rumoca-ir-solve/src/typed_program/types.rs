use serde::{Deserialize, Deserializer, Serialize};

use rumoca_core::RealMatrixMultiplySemantics;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum SolveRealFormat {
    Binary32,
    Binary64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub struct SolveIntegerDomain {
    minimum: i64,
    maximum: i64,
}

impl<'de> Deserialize<'de> for SolveIntegerDomain {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(deny_unknown_fields)]
        struct Wire {
            minimum: i64,
            maximum: i64,
        }

        let wire = Wire::deserialize(deserializer)?;
        Self::construct(wire.minimum, wire.maximum).map_err(serde::de::Error::custom)
    }
}

impl SolveIntegerDomain {
    /// The whole signed 8-bit range.
    pub const I8: Self = Self {
        minimum: i8::MIN as i64,
        maximum: i8::MAX as i64,
    };

    /// The whole signed 16-bit range.
    pub const I16: Self = Self {
        minimum: i16::MIN as i64,
        maximum: i16::MAX as i64,
    };

    /// The whole `i64` range.
    ///
    /// [`SolveIntegerDomain::construct`] is fallible because a caller can name
    /// an empty range. The full range is not one, so naming it as a constant is
    /// what removes the emptiness obligation from every caller that wants it.
    pub const FULL: Self = Self {
        minimum: i64::MIN,
        maximum: i64::MAX,
    };

    /// The signed 32-bit range used by the eFMI Production C profile.
    pub const I32: Self = Self {
        minimum: i32::MIN as i64,
        maximum: i32::MAX as i64,
    };

    pub fn construct(minimum: i64, maximum: i64) -> Result<Self, SolveTypeConstructionError> {
        if minimum > maximum {
            return Err(SolveTypeConstructionError::EmptyIntegerDomain { minimum, maximum });
        }
        Ok(Self { minimum, maximum })
    }

    #[must_use]
    pub const fn minimum(self) -> i64 {
        self.minimum
    }

    #[must_use]
    pub const fn maximum(self) -> i64 {
        self.maximum
    }

    #[must_use]
    pub const fn contains(self, value: i64) -> bool {
        self.minimum <= value && value <= self.maximum
    }
}

/// The arithmetic a program's values are evaluated in: the Real format and the
/// Integer domain.
///
/// Rounding is not among them. Every Real operation this IR admits rounds to
/// nearest, ties to even, on every backend it reaches, so a field carrying that
/// one choice discriminated nothing and made every profile equality a
/// tautology. SPEC_0047 §4.3 reserves a real rounding *contract* for this
/// profile (accumulator, order, per-step and result rounding, contraction),
/// and that contract arrives with the operations that can differ under it, not
/// before.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct SolveArithmeticProfile {
    real_format: SolveRealFormat,
    integer_domain: SolveIntegerDomain,
    real_matrix_multiply: RealMatrixMultiplySemantics,
}

impl SolveArithmeticProfile {
    #[must_use]
    pub const fn construct(
        real_format: SolveRealFormat,
        integer_domain: SolveIntegerDomain,
        real_matrix_multiply: RealMatrixMultiplySemantics,
    ) -> Self {
        Self {
            real_format,
            integer_domain,
            real_matrix_multiply,
        }
    }

    #[must_use]
    pub const fn real_format(self) -> SolveRealFormat {
        self.real_format
    }

    #[must_use]
    pub const fn integer_domain(self) -> SolveIntegerDomain {
        self.integer_domain
    }

    #[must_use]
    pub const fn real_matrix_multiply_semantics(self) -> RealMatrixMultiplySemantics {
        self.real_matrix_multiply
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(
    deny_unknown_fields,
    tag = "kind",
    content = "profile",
    rename_all = "snake_case"
)]
pub enum SolveScalarType {
    Real { format: SolveRealFormat },
    Integer(SolveIntegerDomain),
    Boolean,
}

impl SolveScalarType {
    #[must_use]
    pub const fn real(profile: SolveArithmeticProfile) -> Self {
        Self::Real {
            format: profile.real_format,
        }
    }

    #[must_use]
    pub const fn integer(profile: SolveArithmeticProfile) -> Self {
        Self::Integer(profile.integer_domain)
    }

    #[must_use]
    pub const fn is_numeric(self) -> bool {
        matches!(self, Self::Real { .. } | Self::Integer(_))
    }

    #[must_use]
    pub fn belongs_to(self, profile: SolveArithmeticProfile) -> bool {
        match self {
            Self::Real { format } => format == profile.real_format,
            Self::Integer(domain) => {
                domain.minimum == profile.integer_domain.minimum
                    && domain.maximum == profile.integer_domain.maximum
            }
            Self::Boolean => true,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
pub struct SolveValueType {
    scalar: SolveScalarType,
    dimensions: Box<[u32]>,
    scalar_count: u32,
}

impl<'de> Deserialize<'de> for SolveValueType {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(deny_unknown_fields)]
        struct Wire {
            scalar: SolveScalarType,
            dimensions: Vec<u32>,
            scalar_count: u32,
        }

        let wire = Wire::deserialize(deserializer)?;
        let value_type = if wire.dimensions.is_empty() {
            Self::scalar(wire.scalar)
        } else {
            Self::tensor(wire.scalar, wire.dimensions).map_err(serde::de::Error::custom)?
        };
        if value_type.scalar_count != wire.scalar_count {
            return Err(serde::de::Error::custom(
                "typed value scalar count does not match its dimensions",
            ));
        }
        Ok(value_type)
    }
}

impl SolveValueType {
    #[must_use]
    pub fn scalar(scalar: SolveScalarType) -> Self {
        Self {
            scalar,
            dimensions: Box::new([]),
            scalar_count: 1,
        }
    }

    pub fn tensor(
        scalar: SolveScalarType,
        dimensions: Vec<u32>,
    ) -> Result<Self, SolveTypeConstructionError> {
        if dimensions.is_empty() {
            return Err(SolveTypeConstructionError::EmptyTensorRank);
        }
        let scalar_count = if dimensions.contains(&0) {
            0
        } else {
            dimensions.iter().copied().try_fold(1u32, |count, extent| {
                count
                    .checked_mul(extent)
                    .ok_or(SolveTypeConstructionError::TensorScalarCountOverflow)
            })?
        };
        Ok(Self {
            scalar,
            dimensions: dimensions.into_boxed_slice(),
            scalar_count,
        })
    }

    #[must_use]
    pub const fn element_type(&self) -> SolveScalarType {
        self.scalar
    }

    #[must_use]
    pub fn dimensions(&self) -> &[u32] {
        &self.dimensions
    }

    #[must_use]
    pub const fn scalar_count(&self) -> u32 {
        self.scalar_count
    }

    #[must_use]
    pub fn boolean_with_same_shape(&self) -> Self {
        Self {
            scalar: SolveScalarType::Boolean,
            dimensions: self.dimensions.clone(),
            scalar_count: self.scalar_count,
        }
    }

    #[must_use]
    pub fn with_element_type(&self, scalar: SolveScalarType) -> Self {
        Self {
            scalar,
            dimensions: self.dimensions.clone(),
            scalar_count: self.scalar_count,
        }
    }

    #[must_use]
    pub fn belongs_to(&self, profile: SolveArithmeticProfile) -> bool {
        self.scalar.belongs_to(profile)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(
    deny_unknown_fields,
    tag = "kind",
    content = "bits",
    rename_all = "snake_case"
)]
pub enum SolveValueKind {
    Real32(u32),
    Real64(u64),
    Integer(i64),
    Boolean(bool),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
pub struct SolveValue {
    value_type: SolveValueType,
    kind: SolveValueKind,
}

impl<'de> Deserialize<'de> for SolveValue {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(deny_unknown_fields)]
        struct Wire {
            value_type: SolveValueType,
            kind: SolveValueKind,
        }

        let wire = Wire::deserialize(deserializer)?;
        if !wire.value_type.dimensions().is_empty()
            || !value_kind_matches_type(wire.kind, wire.value_type.element_type())
        {
            return Err(serde::de::Error::custom(
                "typed scalar value does not match its declared type",
            ));
        }
        if let (SolveValueKind::Integer(value), SolveScalarType::Integer(domain)) =
            (wire.kind, wire.value_type.element_type())
            && !domain.contains(value)
        {
            return Err(serde::de::Error::custom(
                "typed Integer value is outside its declared domain",
            ));
        }
        Ok(Self {
            value_type: wire.value_type,
            kind: wire.kind,
        })
    }
}

fn value_kind_matches_type(kind: SolveValueKind, scalar: SolveScalarType) -> bool {
    matches!(
        (kind, scalar),
        (
            SolveValueKind::Real32(_),
            SolveScalarType::Real {
                format: SolveRealFormat::Binary32,
                ..
            }
        ) | (
            SolveValueKind::Real64(_),
            SolveScalarType::Real {
                format: SolveRealFormat::Binary64,
                ..
            }
        ) | (SolveValueKind::Integer(_), SolveScalarType::Integer(_))
            | (SolveValueKind::Boolean(_), SolveScalarType::Boolean)
    )
}

impl SolveValue {
    #[must_use]
    pub fn real(profile: SolveArithmeticProfile, value: f64) -> Self {
        let kind = match profile.real_format {
            SolveRealFormat::Binary32 => SolveValueKind::Real32((value as f32).to_bits()),
            SolveRealFormat::Binary64 => SolveValueKind::Real64(value.to_bits()),
        };
        Self {
            value_type: SolveValueType::scalar(SolveScalarType::real(profile)),
            kind,
        }
    }

    pub fn integer(
        profile: SolveArithmeticProfile,
        value: i64,
    ) -> Result<Self, SolveTypeConstructionError> {
        if !profile.integer_domain.contains(value) {
            return Err(SolveTypeConstructionError::IntegerOutsideDomain {
                value,
                minimum: profile.integer_domain.minimum,
                maximum: profile.integer_domain.maximum,
            });
        }
        Ok(Self {
            value_type: SolveValueType::scalar(SolveScalarType::integer(profile)),
            kind: SolveValueKind::Integer(value),
        })
    }

    #[must_use]
    pub fn boolean(value: bool) -> Self {
        Self {
            value_type: SolveValueType::scalar(SolveScalarType::Boolean),
            kind: SolveValueKind::Boolean(value),
        }
    }

    #[must_use]
    pub const fn value_type(&self) -> &SolveValueType {
        &self.value_type
    }

    #[must_use]
    pub const fn kind(&self) -> SolveValueKind {
        self.kind
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SolveTypeConstructionError {
    EmptyIntegerDomain {
        minimum: i64,
        maximum: i64,
    },
    EmptyTensorRank,
    TensorScalarCountOverflow,
    IntegerOutsideDomain {
        value: i64,
        minimum: i64,
        maximum: i64,
    },
}

impl std::fmt::Display for SolveTypeConstructionError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EmptyIntegerDomain { minimum, maximum } => {
                write!(formatter, "empty Integer domain {minimum}..{maximum}")
            }
            Self::EmptyTensorRank => write!(formatter, "tensor type has no dimensions"),
            Self::TensorScalarCountOverflow => {
                write!(
                    formatter,
                    "tensor scalar count exceeds the Solve identity capacity"
                )
            }
            Self::IntegerOutsideDomain {
                value,
                minimum,
                maximum,
            } => write!(
                formatter,
                "Integer value {value} is outside target domain {minimum}..{maximum}"
            ),
        }
    }
}

impl std::error::Error for SolveTypeConstructionError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn wire_rejects_empty_integer_domain() {
        let error = serde_json::from_str::<SolveIntegerDomain>(r#"{"minimum":2,"maximum":1}"#)
            .expect_err("wire cannot forge an empty Integer domain");
        assert!(error.to_string().contains("empty Integer domain"));
    }

    #[test]
    fn wire_rejects_forged_tensor_scalar_count() {
        let error = serde_json::from_str::<SolveValueType>(
            r#"{
                "scalar":{"kind":"boolean"},
                "dimensions":[2,3],
                "scalar_count":5
            }"#,
        )
        .expect_err("wire cannot forge a tensor scalar count");
        assert!(error.to_string().contains("scalar count"));
    }

    #[test]
    fn wire_rejects_value_kind_type_mismatch() {
        let error = serde_json::from_str::<SolveValue>(
            r#"{
                "value_type":{
                    "scalar":{"kind":"boolean"},
                    "dimensions":[],
                    "scalar_count":1
                },
                "kind":{"kind":"integer","bits":1}
            }"#,
        )
        .expect_err("wire cannot put Integer bits in Boolean storage");
        assert!(error.to_string().contains("does not match"));
    }

    #[test]
    fn wire_rejects_integer_value_outside_declared_domain() {
        let error = serde_json::from_str::<SolveValue>(
            r#"{
                "value_type":{
                    "scalar":{
                        "kind":"integer",
                        "profile":{"minimum":0,"maximum":1}
                    },
                    "dimensions":[],
                    "scalar_count":1
                },
                "kind":{"kind":"integer","bits":2}
            }"#,
        )
        .expect_err("wire cannot forge an Integer value outside its declared domain");
        assert!(error.to_string().contains("outside its declared domain"));
    }

    #[test]
    fn wire_rejects_unknown_fields_at_each_type_boundary() {
        let domain = SolveIntegerDomain::FULL;
        let mut domain_wire = serde_json::to_value(domain).unwrap();
        domain_wire["future_policy"] = serde_json::json!(true);
        assert!(serde_json::from_value::<SolveIntegerDomain>(domain_wire).is_err());

        let profile = SolveArithmeticProfile::construct(
            SolveRealFormat::Binary64,
            domain,
            RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
        );
        let mut profile_wire = serde_json::to_value(profile).unwrap();
        profile_wire["future_policy"] = serde_json::json!(true);
        assert!(serde_json::from_value::<SolveArithmeticProfile>(profile_wire).is_err());

        let value_type = SolveValueType::scalar(SolveScalarType::Boolean);
        let mut value_type_wire = serde_json::to_value(&value_type).unwrap();
        value_type_wire["future_policy"] = serde_json::json!(true);
        assert!(serde_json::from_value::<SolveValueType>(value_type_wire).is_err());

        let mut scalar_wire = serde_json::to_value(SolveScalarType::Boolean).unwrap();
        scalar_wire["future_policy"] = serde_json::json!(true);
        assert!(serde_json::from_value::<SolveScalarType>(scalar_wire).is_err());

        let mut kind_wire = serde_json::to_value(SolveValueKind::Boolean(true)).unwrap();
        kind_wire["future_policy"] = serde_json::json!(true);
        assert!(serde_json::from_value::<SolveValueKind>(kind_wire).is_err());

        let mut value_wire = serde_json::to_value(SolveValue::boolean(true)).unwrap();
        value_wire["future_policy"] = serde_json::json!(true);
        assert!(serde_json::from_value::<SolveValue>(value_wire).is_err());
    }

    #[test]
    fn empty_tensor_identity_is_shape_preserving_and_order_independent() {
        let scalar = SolveScalarType::Boolean;
        let first = SolveValueType::tensor(scalar, vec![0, 3]).unwrap();
        let second = SolveValueType::tensor(scalar, vec![0, 4]).unwrap();
        let late_zero = SolveValueType::tensor(scalar, vec![u32::MAX, u32::MAX, 0]).unwrap();

        assert_eq!(first.scalar_count(), 0);
        assert_eq!(second.scalar_count(), 0);
        assert_eq!(late_zero.scalar_count(), 0);
        assert_ne!(first, second);
        assert_eq!(
            serde_json::from_str::<SolveValueType>(&serde_json::to_string(&first).unwrap())
                .unwrap(),
            first
        );
        assert_eq!(
            serde_json::from_str::<SolveValueType>(&serde_json::to_string(&second).unwrap())
                .unwrap(),
            second
        );
        assert_eq!(
            SolveValueType::tensor(scalar, vec![u32::MAX, u32::MAX]),
            Err(SolveTypeConstructionError::TensorScalarCountOverflow)
        );
    }
}
