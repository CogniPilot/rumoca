//! Semantic DAE/Solve projection into checked GALEC Algorithm Code.
//!
//! This phase owns admissibility and lowering only. Checked export data and
//! constructor invariants live in `rumoca-ir-galec`; textual rendering and
//! packaging live beyond this phase.

mod admissibility;
mod diagnostic;
mod input;
mod lower;
mod mangle;

pub use admissibility::{AdmittedClock, AdmittedClockDomain, check_admissibility};
pub use diagnostic::GalecTargetError;
pub use input::{GalecInput, GalecOptions};
pub use lower::lower_to_algorithm_code;
