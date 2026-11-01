//! MLS §7.2.4: a lower-precedence modification merged below a stated one.
use super::*;
use rumoca_core::OpBinary;
use std::sync::Arc;

impl Expression {
    /// The element a modification argument names: `x = e`, `x(...)` or
    /// `x(...) = e`.
    pub fn modification_target(&self) -> Option<&ComponentReference> {
        match self {
            Self::Modification { target, .. } | Self::ClassModification { target, .. } => {
                Some(target)
            }
            Self::Binary {
                op: OpBinary::Assign,
                lhs,
                ..
            } => match lhs.as_ref() {
                Self::ClassModification { target, .. } => Some(target),
                _ => None,
            },
            _ => None,
        }
    }

    /// Merge `default`, a lower-precedence modification of the same element,
    /// below this one: what this modification states wins, nested arguments
    /// merge argument by argument, and a binding only `default` states is kept.
    ///
    /// Both use one storage shape: a full argument (`x = e`, `x(...)`,
    /// `x(...) = e`) or, in a component's modification map, its value
    /// (`e`, `x(...)`, `x(...) = e`).
    ///
    /// MLS §7.2.6: an element `default` makes `final` cannot be modified
    /// again. If this modification states one, the merge fails with that
    /// element's path below the merged one (`a`, `p.a`); `self` is then
    /// partly merged and must not be used.
    pub fn merge_modification_below(&mut self, default: &Self) -> Result<(), String> {
        if let Some(lower) = default.nested_modification() {
            if let Some(stated) = self.nested_modification_mut() {
                merge_arguments_below(stated, lower)?;
            } else if let Some(binding) = self.modification_binding() {
                // A stated binding replaces the whole value, so it modifies
                // every element below, final ones included.
                first_final(lower).map_or(Ok(()), Err)?;
                // Otherwise it takes the lower modification's arguments.
                let merged = bound(lower, binding, self.span());
                *self = merged;
                return Ok(());
            }
        }
        if self.modification_binding().is_none()
            && let Some(binding) = default.modification_binding()
            && let Some(stated) = self.nested_modification()
        {
            let merged = bound(stated, binding, self.span());
            *self = merged;
        }
        Ok(())
    }

    /// The `x(...)` part of a modification, if it has one.
    fn nested_modification(&self) -> Option<&Self> {
        match self {
            Self::ClassModification { .. } => Some(self),
            Self::Binary {
                op: OpBinary::Assign,
                lhs,
                ..
            } if matches!(lhs.as_ref(), Self::ClassModification { .. }) => Some(lhs),
            Self::Modification { value, .. }
                if matches!(value.as_ref(), Self::ClassModification { .. }) =>
            {
                Some(value)
            }
            _ => None,
        }
    }

    fn nested_modification_mut(&mut self) -> Option<&mut Self> {
        match self {
            Self::ClassModification { .. } => Some(self),
            Self::Binary {
                op: OpBinary::Assign,
                lhs,
                ..
            } if matches!(lhs.as_ref(), Self::ClassModification { .. }) => Some(Arc::make_mut(lhs)),
            Self::Modification { value, .. }
                if matches!(value.as_ref(), Self::ClassModification { .. }) =>
            {
                Some(Arc::make_mut(value))
            }
            _ => None,
        }
    }

    /// The value a modification binds, if it binds one.
    fn modification_binding(&self) -> Option<&Self> {
        match self {
            Self::ClassModification { .. } => None,
            Self::Binary {
                op: OpBinary::Assign,
                lhs,
                rhs,
                ..
            } if matches!(lhs.as_ref(), Self::ClassModification { .. }) => Some(rhs),
            Self::Modification { value, .. } => {
                (!matches!(value.as_ref(), Self::ClassModification { .. })).then_some(value)
            }
            value => Some(value),
        }
    }
}

/// The path of the first element a class modification makes `final`, at any
/// depth of its nested arguments.
fn first_final(modification: &Expression) -> Option<String> {
    let Expression::ClassModification {
        modifications,
        final_flags,
        ..
    } = modification
    else {
        return None;
    };
    modifications
        .iter()
        .enumerate()
        .find_map(|(index, argument)| {
            let name = argument.modification_target()?.to_string();
            if final_flags.get(index).copied().unwrap_or(false) {
                return Some(name);
            }
            let inner = first_final(argument.nested_modification()?)?;
            Some(format!("{name}.{inner}"))
        })
}

/// `nested = binding`, the stored form of `x(...) = e`.
fn bound(nested: &Expression, binding: &Expression, span: Span) -> Expression {
    Expression::Binary {
        op: OpBinary::Assign,
        lhs: Arc::new(nested.clone()),
        rhs: Arc::new(binding.clone()),
        span,
    }
}

/// Append each argument of `lower` whose element `stated` does not modify,
/// and merge the ones it does, refusing one that `lower` makes `final`.
fn merge_arguments_below(stated: &mut Expression, lower: &Expression) -> Result<(), String> {
    let (
        Expression::ClassModification {
            modifications,
            each_flags,
            final_flags,
            redeclare_flags,
            ..
        },
        Expression::ClassModification {
            modifications: lower_modifications,
            each_flags: lower_each,
            final_flags: lower_final,
            redeclare_flags: lower_redeclare,
            ..
        },
    ) = (stated, lower)
    else {
        return Ok(());
    };
    for (index, argument) in lower_modifications.iter().enumerate() {
        let target = argument.modification_target().map(ToString::to_string);
        let existing = target.as_ref().and_then(|target| {
            modifications.iter_mut().find(|stated| {
                stated
                    .modification_target()
                    .map(ToString::to_string)
                    .as_ref()
                    == Some(target)
            })
        });
        let name = target.unwrap_or_default();
        match existing {
            Some(_) if lower_final.get(index).copied().unwrap_or(false) => return Err(name),
            Some(stated) => stated
                .merge_modification_below(argument)
                .map_err(|inner| format!("{name}.{inner}"))?,
            None => {
                let count = modifications.len();
                for flags in [&mut *each_flags, &mut *final_flags, &mut *redeclare_flags] {
                    flags.resize(count, false);
                }
                modifications.push(argument.clone());
                each_flags.push(lower_each.get(index).copied().unwrap_or(false));
                final_flags.push(lower_final.get(index).copied().unwrap_or(false));
                redeclare_flags.push(lower_redeclare.get(index).copied().unwrap_or(false));
            }
        }
    }
    Ok(())
}
