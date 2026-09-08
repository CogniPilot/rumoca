//! Version-specific FMI write-mode masks.
//!
//! FMI 2.0.5 and FMI 3.0.2 place different variables under the boundary write
//! at each lifecycle mode, so one shared mask cannot describe both. A single
//! table with a per-version delta is exactly the shape that once denied every
//! continuous-state write, so the two tables here are written out in full from
//! their standard clauses and never derived one from the other.
//!
//! Each mask is decided once, here, from a [`FmiWritePolicy`]. Emission places
//! the resulting integer; it never re-derives a policy from a rendered
//! spelling. The kernel's dynamic admission reads [`Fmi3WriteModes::admits`],
//! so the generated C and the in-process solver consult the one FMI 3 table
//! rather than two hand-kept copies. The five mode-bit meanings a template
//! reads are emitted from the same `bit` definitions, so a template and the
//! compiler cannot disagree about which bit is which mode.

use super::metadata::{FmiStateInitial, FmiStateReinit, FmiWritePolicy};
use serde::Serialize;
use serde::ser::{SerializeMap, Serializer};

/// A lifecycle mode in which the FMI 2.0.5 boundary may issue a write.
///
/// The bit position is shared with generated C: the same integer names the
/// same mode in a variable mask and in the per-mode admissible mask a template
/// returns.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Fmi2WriteMode {
    /// Instantiated, before `fmi2EnterInitializationMode`, where start values
    /// are set (FMI 2.0.5 §2.1.3).
    Instantiated,
    /// Initialization Mode.
    InitializationMode,
    /// Model Exchange Event Mode.
    EventMode,
    /// Model Exchange Continuous-Time Mode.
    ContinuousTimeMode,
    /// Co-Simulation, between `fmi2DoStep` calls.
    StepComplete,
}

impl Fmi2WriteMode {
    const fn bit(self) -> u32 {
        match self {
            Self::Instantiated => 1 << 0,
            Self::InitializationMode => 1 << 1,
            Self::EventMode => 1 << 2,
            Self::ContinuousTimeMode => 1 << 3,
            Self::StepComplete => 1 << 4,
        }
    }
}

/// A lifecycle mode in which the FMI 3.0.2 boundary may issue a write.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Fmi3WriteMode {
    /// Instantiated, before `fmi3EnterInitializationMode`, where start values
    /// are set.
    Instantiated,
    /// Initialization Mode.
    InitializationMode,
    /// Model Exchange Event Mode.
    EventMode,
    /// Model Exchange Continuous-Time Mode.
    ContinuousTimeMode,
    /// Co-Simulation Step Mode.
    StepMode,
}

impl Fmi3WriteMode {
    const fn bit(self) -> u32 {
        match self {
            Self::Instantiated => 1 << 0,
            Self::InitializationMode => 1 << 1,
            Self::EventMode => 1 << 2,
            Self::ContinuousTimeMode => 1 << 3,
            Self::StepMode => 1 << 4,
        }
    }
}

/// The FMI 2.0.5 write modes one projected variable authorizes.
///
/// The only constructor is [`Self::of`], which decides the mask from a checked
/// policy, so no caller can mint an arbitrary set of bits. The mask exposes no
/// raw bit accessor: it serializes as the integer emission places, and its
/// admission question is answered by [`Self::admits`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Fmi2WriteModes(u32);

impl Fmi2WriteModes {
    /// Decide the FMI 2.0.5 write mask for one policy.
    ///
    /// FMI 2.0.5 clauses, cited per row:
    /// - Instantiated admits `variability != constant` with an `initial` in
    ///   {exact, approx} and excludes inputs (§2.1.3): a parameter or state
    ///   qualifies, an input never does.
    /// - Initialization Mode admits `initial = exact`, and additionally
    ///   `causality = input` (fmi2SetReal, allowed-set table).
    /// - Event Mode admits only `causality = input` or a tunable parameter
    ///   (fmi2SetReal footnote 4); it admits NO continuous state, with any
    ///   reinit.
    /// - Continuous-Time Mode admits every continuous state and inputs with
    ///   `variability = continuous` (footnote 5); a discrete input is excluded.
    /// - Co-Simulation exposes inputs and tunable parameters between steps, not
    ///   the internally integrated states.
    pub(crate) const fn of(policy: FmiWritePolicy) -> Self {
        use Fmi2WriteMode::{
            ContinuousTimeMode, EventMode, InitializationMode, Instantiated, StepComplete,
        };
        let bits = match policy {
            FmiWritePolicy::ReadOnly => 0,
            FmiWritePolicy::FixedParameter => Instantiated.bit() | InitializationMode.bit(),
            FmiWritePolicy::TunableParameter => {
                Instantiated.bit() | InitializationMode.bit() | EventMode.bit() | StepComplete.bit()
            }
            FmiWritePolicy::ContinuousInput => {
                InitializationMode.bit()
                    | EventMode.bit()
                    | ContinuousTimeMode.bit()
                    | StepComplete.bit()
            }
            FmiWritePolicy::DiscreteInput => {
                InitializationMode.bit() | EventMode.bit() | StepComplete.bit()
            }
            FmiWritePolicy::ContinuousState { initial, reinit: _ } => {
                // FMI 2 Event Mode admits no continuous state at all, so the
                // reinit evidence changes nothing here.
                let init_bit = match initial {
                    FmiStateInitial::Exact => InitializationMode.bit(),
                    FmiStateInitial::Approx => 0,
                };
                Instantiated.bit() | ContinuousTimeMode.bit() | init_bit
            }
        };
        Self(bits)
    }

    /// Whether the boundary may write this variable in `mode`.
    #[must_use]
    pub const fn admits(self, mode: Fmi2WriteMode) -> bool {
        self.0 & mode.bit() != 0
    }
}

impl Serialize for Fmi2WriteModes {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_u32(self.0)
    }
}

/// The FMI 3.0.2 write modes one projected variable authorizes.
///
/// This is the single FMI 3 admission source: the generated C reads the
/// serialized integer, and the in-process Model Exchange kernel reads
/// [`Self::admits`]. The two therefore cannot drift.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Fmi3WriteModes(u32);

impl Fmi3WriteModes {
    /// Decide the FMI 3.0.2 write mask for one policy.
    ///
    /// FMI 3.0.2 clauses, cited per row:
    /// - Instantiated admits `variability != constant` with an `initial` in
    ///   {exact, approx} and includes inputs, whose `initial` defaults to
    ///   exact.
    /// - Initialization Mode admits `initial = exact`; an input qualifies
    ///   through that default, so no separate input clause is needed.
    /// - Event Mode admits inputs and tunable parameters, and additionally
    ///   continuous states carrying `reinit = false`.
    /// - Continuous-Time Mode admits inputs with `variability = continuous`
    ///   and continuous-time states; a discrete input is excluded.
    /// - Co-Simulation Step Mode exposes inputs and tunable parameters, not the
    ///   internally integrated states.
    pub(crate) const fn of(policy: FmiWritePolicy) -> Self {
        use Fmi3WriteMode::{
            ContinuousTimeMode, EventMode, InitializationMode, Instantiated, StepMode,
        };
        let bits = match policy {
            FmiWritePolicy::ReadOnly => 0,
            FmiWritePolicy::FixedParameter => Instantiated.bit() | InitializationMode.bit(),
            FmiWritePolicy::TunableParameter => {
                Instantiated.bit() | InitializationMode.bit() | EventMode.bit() | StepMode.bit()
            }
            FmiWritePolicy::ContinuousInput => {
                Instantiated.bit()
                    | InitializationMode.bit()
                    | EventMode.bit()
                    | ContinuousTimeMode.bit()
                    | StepMode.bit()
            }
            FmiWritePolicy::DiscreteInput => {
                Instantiated.bit() | InitializationMode.bit() | EventMode.bit() | StepMode.bit()
            }
            FmiWritePolicy::ContinuousState { initial, reinit } => {
                let init_bit = match initial {
                    FmiStateInitial::Exact => InitializationMode.bit(),
                    FmiStateInitial::Approx => 0,
                };
                let event_bit = match reinit {
                    FmiStateReinit::False => EventMode.bit(),
                    FmiStateReinit::Reinitializable => 0,
                };
                Instantiated.bit() | ContinuousTimeMode.bit() | init_bit | event_bit
            }
        };
        Self(bits)
    }

    /// Whether the boundary may write this variable in `mode`.
    #[must_use]
    pub const fn admits(self, mode: Fmi3WriteMode) -> bool {
        self.0 & mode.bit() != 0
    }
}

impl Serialize for Fmi3WriteModes {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_u32(self.0)
    }
}

/// The FMI 2.0.5 mode-bit meanings a version template reads, issued from the
/// [`Fmi2WriteMode`] definitions so a template and the compiler agree on which
/// bit is which mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct Fmi2WriteModeBits;

impl Serialize for Fmi2WriteModeBits {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut map = serializer.serialize_map(Some(5))?;
        map.serialize_entry("instantiated", &Fmi2WriteMode::Instantiated.bit())?;
        map.serialize_entry("initialization", &Fmi2WriteMode::InitializationMode.bit())?;
        map.serialize_entry("me_event", &Fmi2WriteMode::EventMode.bit())?;
        map.serialize_entry("me_continuous", &Fmi2WriteMode::ContinuousTimeMode.bit())?;
        map.serialize_entry("cs_step", &Fmi2WriteMode::StepComplete.bit())?;
        map.end()
    }
}

/// The FMI 3.0.2 mode-bit meanings a version template reads, issued from the
/// [`Fmi3WriteMode`] definitions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct Fmi3WriteModeBits;

impl Serialize for Fmi3WriteModeBits {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut map = serializer.serialize_map(Some(5))?;
        map.serialize_entry("instantiated", &Fmi3WriteMode::Instantiated.bit())?;
        map.serialize_entry("initialization", &Fmi3WriteMode::InitializationMode.bit())?;
        map.serialize_entry("me_event", &Fmi3WriteMode::EventMode.bit())?;
        map.serialize_entry("me_continuous", &Fmi3WriteMode::ContinuousTimeMode.bit())?;
        map.serialize_entry("cs_step", &Fmi3WriteMode::StepMode.bit())?;
        map.end()
    }
}
