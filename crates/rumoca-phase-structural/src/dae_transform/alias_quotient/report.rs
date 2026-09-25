//! Named report of the STRUCT-T02 alias quotient, for structure inspection.

use std::fmt;

use rumoca_ir_dae as dae;

use super::{AliasPlan, AliasRefusal, alias_classes, member_facts, quotient_class, representative};

/// One eliminated member and the sign it reads its representative with.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AliasMemberReport {
    pub name: String,
    pub negated: bool,
}

/// One alias class: its representative and eliminated members when
/// quotiented, or every member and the reason it stays unchanged.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AliasClassReport {
    Quotiented {
        representative: String,
        eliminated: Vec<AliasMemberReport>,
        /// Other states of the class, which keep their role and edge equation.
        retained_states: Vec<String>,
    },
    Unchanged {
        members: Vec<String>,
        reason: AliasRefusal,
    },
}

/// Every alias class of one DAE, in deterministic class order.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct AliasQuotientReport {
    pub classes: Vec<AliasClassReport>,
}

impl AliasQuotientReport {
    /// Number of declarations the quotient eliminates.
    #[must_use]
    pub fn eliminated_count(&self) -> usize {
        self.classes
            .iter()
            .map(|class| match class {
                AliasClassReport::Quotiented { eliminated, .. } => eliminated.len(),
                AliasClassReport::Unchanged { .. } => 0,
            })
            .sum()
    }
}

/// Report the alias classes `quotient_aliases` would find in `model`.
#[must_use]
pub fn alias_quotient_report(model: &dae::Dae) -> AliasQuotientReport {
    model.inspect(|view| {
        let members = member_facts(view);
        let name = |ordinal: u32| {
            view.variable_id(ordinal as usize)
                .and_then(|id| view.variable(id))
                .map(|variable| variable.name().to_string())
                .unwrap_or_default()
        };
        let classes = alias_classes(view, &members)
            .iter()
            .map(|class| match representative(class, &members) {
                Err(reason) => AliasClassReport::Unchanged {
                    members: class.members.iter().copied().map(name).collect(),
                    reason,
                },
                Ok(representative) => {
                    let mut plan = AliasPlan {
                        substitutions: vec![None; view.variable_count()],
                        ..AliasPlan::default()
                    };
                    quotient_class(class, representative, &members, &mut plan);
                    let (eliminated, retained): (Vec<u32>, Vec<u32>) = class
                        .members
                        .iter()
                        .copied()
                        .filter(|member| *member != representative)
                        .partition(|member| plan.substitutions[*member as usize].is_some());
                    AliasClassReport::Quotiented {
                        representative: name(representative),
                        eliminated: eliminated
                            .into_iter()
                            .map(|member| AliasMemberReport {
                                name: name(member),
                                negated: plan.substitutions[member as usize]
                                    .is_some_and(|substitution| substitution.negated),
                            })
                            .collect(),
                        retained_states: retained.into_iter().map(name).collect(),
                    }
                }
            })
            .collect();
        AliasQuotientReport { classes }
    })
}

impl fmt::Display for AliasRefusal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Cycle => "cyclic (redundant or sign-inconsistent edges)",
            Self::SeveralAnchors => "several members request a state or carry a seed",
            Self::AnchorIsNotState => "the seeded member is not a state but the class has one",
        })
    }
}

impl fmt::Display for AliasQuotientReport {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let quotiented = self
            .classes
            .iter()
            .filter(|class| matches!(class, AliasClassReport::Quotiented { .. }))
            .count();
        writeln!(
            f,
            "alias quotient (STRUCT-T02): {} class(es), {quotiented} quotiented, {} member(s) eliminated",
            self.classes.len(),
            self.eliminated_count()
        )?;
        for class in &self.classes {
            write!(f, "{class}")?;
        }
        Ok(())
    }
}

impl fmt::Display for AliasMemberReport {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let sign = if self.negated { "-" } else { "" };
        write!(f, "{sign}{}", self.name)
    }
}

impl fmt::Display for AliasClassReport {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Quotiented {
                representative,
                eliminated,
                retained_states,
            } => {
                let eliminated = eliminated.iter().map(ToString::to_string);
                let eliminated = eliminated.collect::<Vec<_>>().join(", ");
                writeln!(f, "  {representative} <- {eliminated}")?;
                if retained_states.is_empty() {
                    return Ok(());
                }
                writeln!(f, "    retained states: {}", retained_states.join(", "))
            }
            Self::Unchanged { members, reason } => {
                writeln!(f, "  unchanged ({reason}): {}", members.join(", "))
            }
        }
    }
}
