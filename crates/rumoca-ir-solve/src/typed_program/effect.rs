//! Explicit GALEC effect vocabulary: error signals and saturation ranges.
//!
//! Effects are never implied by an arithmetic operation. A limit saturates a
//! declared range, and every error signal is a member of one checked status
//! word whose reserved bits can never be set.

use serde::{Deserialize, Deserializer, Serialize};

use super::types::{SolveScalarType, SolveValue, SolveValueKind};

/// The six predefined eFMI error signals in their normative bit order.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum SolvePredefinedSignal {
    InvalidArgument,
    Overflow,
    Nan,
    SolveLinearEquationsFailed,
    NoSolutionFound,
    UnspecifiedError,
}

impl SolvePredefinedSignal {
    /// Every predefined signal in status-bit order.
    pub const ALL: [Self; 6] = [
        Self::InvalidArgument,
        Self::Overflow,
        Self::Nan,
        Self::SolveLinearEquationsFailed,
        Self::NoSolutionFound,
        Self::UnspecifiedError,
    ];

    #[must_use]
    pub const fn bit(self) -> u32 {
        match self {
            Self::InvalidArgument => 0,
            Self::Overflow => 1,
            Self::Nan => 2,
            Self::SolveLinearEquationsFailed => 3,
            Self::NoSolutionFound => 4,
            Self::UnspecifiedError => 5,
        }
    }
}

/// One declared user error signal.
///
/// Status bits 6 to 15 are reserved by the standard, so user signals start at
/// bit 16 and the status word admits at most sixteen of them.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub struct SolveUserSignal(u8);

impl<'de> Deserialize<'de> for SolveUserSignal {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        Self::construct(u8::deserialize(deserializer)?).map_err(serde::de::Error::custom)
    }
}

impl SolveUserSignal {
    /// Maximum number of declarable user signals.
    pub const CAPACITY: u8 = 16;
    /// Status bit of user signal zero.
    pub const FIRST_BIT: u32 = 16;

    pub fn construct(index: u8) -> Result<Self, SolveEffectConstructionError> {
        if index >= Self::CAPACITY {
            return Err(SolveEffectConstructionError::UserSignalOutsideCapacity { index });
        }
        Ok(Self(index))
    }

    #[must_use]
    pub const fn index(self) -> u8 {
        self.0
    }

    #[must_use]
    pub const fn bit(self) -> u32 {
        Self::FIRST_BIT + self.0 as u32
    }
}

/// One error signal identity.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(
    tag = "kind",
    content = "signal",
    rename_all = "snake_case",
    deny_unknown_fields
)]
pub enum SolveSignal {
    Predefined(SolvePredefinedSignal),
    User(SolveUserSignal),
}

impl SolveSignal {
    #[must_use]
    pub const fn bit(self) -> u32 {
        match self {
            Self::Predefined(signal) => signal.bit(),
            Self::User(signal) => signal.bit(),
        }
    }
}

/// One checked error-signal status word.
///
/// Membership is the only observable content; the numeric encoding exists so
/// that set effects, catches, and escape sets share one compact lattice.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, Serialize)]
pub struct SolveSignalSet {
    bits: u32,
}

impl<'de> Deserialize<'de> for SolveSignalSet {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(deny_unknown_fields)]
        struct Wire {
            bits: u32,
        }

        Self::from_bits(Wire::deserialize(deserializer)?.bits).map_err(serde::de::Error::custom)
    }
}

impl SolveSignalSet {
    /// Status bits reserved by the standard; they must always stay zero.
    pub const RESERVED_BITS: u32 = 0x0000_FFC0;
    /// The set containing no signal.
    pub const EMPTY: Self = Self { bits: 0 };

    #[must_use]
    pub fn construct(signals: &[SolveSignal]) -> Self {
        Self {
            bits: signals
                .iter()
                .fold(0u32, |bits, signal| bits | (1u32 << signal.bit())),
        }
    }

    pub fn from_bits(bits: u32) -> Result<Self, SolveEffectConstructionError> {
        if bits & Self::RESERVED_BITS != 0 {
            return Err(SolveEffectConstructionError::ReservedSignalBits { bits });
        }
        Ok(Self { bits })
    }

    /// Every signal the status word can carry.
    #[must_use]
    pub fn universe() -> Self {
        Self {
            bits: !Self::RESERVED_BITS,
        }
    }

    #[must_use]
    pub const fn bits(self) -> u32 {
        self.bits
    }

    #[must_use]
    pub const fn is_empty(self) -> bool {
        self.bits == 0
    }

    #[must_use]
    pub const fn contains(self, signal: SolveSignal) -> bool {
        self.bits & (1u32 << signal.bit()) != 0
    }

    #[must_use]
    pub const fn contains_all(self, other: Self) -> bool {
        self.bits & other.bits == other.bits
    }

    #[must_use]
    pub const fn union(self, other: Self) -> Self {
        Self {
            bits: self.bits | other.bits,
        }
    }

    #[must_use]
    pub const fn difference(self, other: Self) -> Self {
        Self {
            bits: self.bits & !other.bits,
        }
    }

    #[must_use]
    pub const fn intersection(self, other: Self) -> Self {
        Self {
            bits: self.bits & other.bits,
        }
    }

    /// True when no member of `other` can be a member of this set.
    #[must_use]
    pub const fn is_disjoint(self, other: Self) -> bool {
        self.intersection(other).is_empty()
    }

    /// Every member in deterministic status-bit order.
    #[must_use]
    pub fn signals(self) -> Vec<SolveSignal> {
        let predefined = SolvePredefinedSignal::ALL
            .into_iter()
            .map(SolveSignal::Predefined);
        let user =
            (0..SolveUserSignal::CAPACITY).map(|index| SolveSignal::User(SolveUserSignal(index)));
        predefined
            .chain(user)
            .filter(|signal| self.contains(*signal))
            .collect()
    }
}

/// One checked saturation range for a ranged Real or Integer entity.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SolveValueRange {
    minimum: SolveValue,
    maximum: SolveValue,
}

impl<'de> Deserialize<'de> for SolveValueRange {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(deny_unknown_fields)]
        struct Wire {
            minimum: SolveValue,
            maximum: SolveValue,
        }

        let wire = Wire::deserialize(deserializer)?;
        Self::construct(wire.minimum, wire.maximum).map_err(serde::de::Error::custom)
    }
}

impl SolveValueRange {
    pub fn construct(
        minimum: SolveValue,
        maximum: SolveValue,
    ) -> Result<Self, SolveEffectConstructionError> {
        if minimum.value_type() != maximum.value_type()
            || !minimum.value_type().dimensions().is_empty()
            || !minimum.value_type().element_type().is_numeric()
        {
            return Err(SolveEffectConstructionError::RangeTypeMismatch);
        }
        if !bounds_are_ordered(&minimum, &maximum) {
            return Err(SolveEffectConstructionError::EmptyRange);
        }
        Ok(Self { minimum, maximum })
    }

    #[must_use]
    pub const fn minimum(&self) -> &SolveValue {
        &self.minimum
    }

    #[must_use]
    pub const fn maximum(&self) -> &SolveValue {
        &self.maximum
    }

    /// True when this range saturates the given element type exactly.
    #[must_use]
    pub fn limits(&self, element: SolveScalarType) -> bool {
        self.minimum.value_type().element_type() == element
    }
}

fn bounds_are_ordered(minimum: &SolveValue, maximum: &SolveValue) -> bool {
    match (minimum.kind(), maximum.kind()) {
        (SolveValueKind::Integer(low), SolveValueKind::Integer(high)) => low <= high,
        _ => match (minimum.real_as_f64(), maximum.real_as_f64()) {
            (Some(low), Some(high)) => low <= high,
            _ => false,
        },
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SolveEffectConstructionError {
    UserSignalOutsideCapacity { index: u8 },
    ReservedSignalBits { bits: u32 },
    RangeTypeMismatch,
    EmptyRange,
}

impl std::fmt::Display for SolveEffectConstructionError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UserSignalOutsideCapacity { index } => write!(
                formatter,
                "user error signal {index} exceeds the checked status capacity"
            ),
            Self::ReservedSignalBits { bits } => write!(
                formatter,
                "error-signal status {bits:#010x} sets reserved bits"
            ),
            Self::RangeTypeMismatch => {
                formatter.write_str("saturation bounds are not one numeric scalar type")
            }
            Self::EmptyRange => formatter.write_str("saturation range is empty or unordered"),
        }
    }
}

impl std::error::Error for SolveEffectConstructionError {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{SolveArithmeticProfile, SolveIntegerDomain, SolveRealFormat, SolveRoundingMode};

    fn profile() -> SolveArithmeticProfile {
        SolveArithmeticProfile::construct(
            SolveRealFormat::Binary64,
            SolveRoundingMode::NearestTiesToEven,
            SolveIntegerDomain::construct(i32::MIN.into(), i32::MAX.into()).unwrap(),
        )
    }

    #[test]
    fn user_signals_cannot_exceed_the_checked_status_capacity() {
        assert!(SolveUserSignal::construct(15).is_ok());
        assert_eq!(
            SolveUserSignal::construct(16),
            Err(SolveEffectConstructionError::UserSignalOutsideCapacity { index: 16 })
        );
    }

    #[test]
    fn signal_sets_keep_reserved_bits_clear_and_order_deterministic() {
        let set = SolveSignalSet::construct(&[
            SolveSignal::User(SolveUserSignal::construct(1).unwrap()),
            SolveSignal::Predefined(SolvePredefinedSignal::Nan),
        ]);
        assert_eq!(set.bits() & SolveSignalSet::RESERVED_BITS, 0);
        assert_eq!(
            set.signals(),
            vec![
                SolveSignal::Predefined(SolvePredefinedSignal::Nan),
                SolveSignal::User(SolveUserSignal::construct(1).unwrap()),
            ]
        );
        assert!(SolveSignalSet::universe().contains_all(set));
        assert!(set.difference(set).is_empty());
        let only_nan =
            SolveSignalSet::construct(&[SolveSignal::Predefined(SolvePredefinedSignal::Nan)]);
        assert_eq!(set.intersection(only_nan), only_nan);
        assert!(only_nan.is_disjoint(set.difference(only_nan)));
    }

    #[test]
    fn wire_rejects_a_forged_reserved_status_bit() {
        let error = serde_json::from_str::<SolveSignalSet>(r#"{"bits":64}"#)
            .expect_err("reserved status bits cannot be forged");
        assert!(error.to_string().contains("reserved bits"), "{error}");
    }

    /// Every effect wire record is closed, so a fact written beside a checked
    /// input is a rejected unknown field rather than a silently ignored byte.
    #[test]
    fn effect_wires_deny_fields_beside_their_checked_inputs() {
        let set = serde_json::from_str::<SolveSignalSet>(r#"{"bits":4,"signals":["nan"]}"#)
            .expect_err("a membership claim cannot ride beside the status word");
        assert!(set.to_string().contains("signals"), "{set}");

        let signal =
            serde_json::from_str::<SolveSignal>(r#"{"kind":"predefined","signal":"nan","bit":2}"#)
                .expect_err("a derived status bit cannot ride beside its signal identity");
        assert!(signal.to_string().contains("bit"), "{signal}");

        let arithmetic = profile();
        let range = SolveValueRange::construct(
            SolveValue::real(arithmetic, 0.0),
            SolveValue::real(arithmetic, 1.0),
        )
        .unwrap();
        let mut json = serde_json::to_value(&range).unwrap();
        json.as_object_mut()
            .expect("a range is an object")
            .insert("element".to_owned(), serde_json::json!("real"));
        let range = serde_json::from_value::<SolveValueRange>(json)
            .expect_err("the saturated element type is derived from the bounds");
        assert!(range.to_string().contains("element"), "{range}");
    }

    #[test]
    fn saturation_ranges_reject_mismatched_unordered_and_nan_bounds() {
        let arithmetic = profile();
        assert!(
            SolveValueRange::construct(
                SolveValue::real(arithmetic, -1.0),
                SolveValue::real(arithmetic, 1.0),
            )
            .is_ok()
        );
        assert_eq!(
            SolveValueRange::construct(
                SolveValue::real(arithmetic, 1.0),
                SolveValue::integer(arithmetic, 2).unwrap(),
            ),
            Err(SolveEffectConstructionError::RangeTypeMismatch)
        );
        assert_eq!(
            SolveValueRange::construct(SolveValue::boolean(false), SolveValue::boolean(true)),
            Err(SolveEffectConstructionError::RangeTypeMismatch)
        );
        assert_eq!(
            SolveValueRange::construct(
                SolveValue::real(arithmetic, 1.0),
                SolveValue::real(arithmetic, -1.0),
            ),
            Err(SolveEffectConstructionError::EmptyRange)
        );
        assert_eq!(
            SolveValueRange::construct(
                SolveValue::real(arithmetic, f64::NAN),
                SolveValue::real(arithmetic, 1.0),
            ),
            Err(SolveEffectConstructionError::EmptyRange)
        );
    }

    #[test]
    fn range_wire_replays_through_the_checked_constructor() {
        let arithmetic = profile();
        let range = SolveValueRange::construct(
            SolveValue::real(arithmetic, 0.0),
            SolveValue::real(arithmetic, 4.0),
        )
        .unwrap();
        assert!(range.limits(SolveScalarType::real(arithmetic)));
        let mut json = serde_json::to_value(&range).unwrap();
        let replayed: SolveValueRange = serde_json::from_value(json.clone()).unwrap();
        assert_eq!(replayed, range);

        let minimum = json["minimum"].clone();
        json["minimum"] = json["maximum"].clone();
        json["maximum"] = minimum;
        assert!(serde_json::from_value::<SolveValueRange>(json).is_err());
    }
}
