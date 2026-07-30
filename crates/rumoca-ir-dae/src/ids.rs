use std::marker::PhantomData;

/// Invariant construction brand. It is erased from final storage and wire data.
type Brand<'dae> = PhantomData<&'dae mut &'dae ()>;

macro_rules! branded_ids {
    ($($(#[$meta:meta])* $name:ident;)+) => {$(
        $(#[$meta])*
        #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $name<'dae> {
            raw: u32,
            brand: Brand<'dae>,
        }

        impl<'dae> $name<'dae> {
            pub(crate) const fn from_raw(raw: u32) -> Self {
                Self {
                    raw,
                    brand: PhantomData,
                }
            }

            pub const fn index(self) -> u32 {
                self.raw
            }
        }

        impl Copy for $name<'_> {}

        impl Clone for $name<'_> {
            fn clone(&self) -> Self {
                *self
            }
        }
    )+};
}

macro_rules! owner_local_ids {
    ($($(#[$meta:meta])* $name:ident, $owner:ident: $owner_type:ident;)+) => {$(
        $(#[$meta])*
        #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $name<'dae> {
            $owner: u32,
            ordinal: u32,
            brand: Brand<'dae>,
        }

        impl<'dae> $name<'dae> {
            pub(crate) const fn from_raw($owner: u32, ordinal: u32) -> Self {
                Self {
                    $owner,
                    ordinal,
                    brand: PhantomData,
                }
            }

            pub const fn $owner(self) -> $owner_type<'dae> {
                $owner_type::from_raw(self.$owner)
            }

            pub const fn ordinal(self) -> u32 {
                self.ordinal
            }
        }

        impl Copy for $name<'_> {}

        impl Clone for $name<'_> {
            fn clone(&self) -> Self {
                *self
            }
        }
    )+};
}

branded_ids! {
    /// Identity of one node in a DAE-wide expression arena.
    ExprId;
    /// Canonical identity of an expression value type.
    ValueTypeId;
    /// Identity of one DAE variable declaration.
    VariableId;
    /// Identity of one parameter or constant coordinate.
    ParameterId;
    /// Identity of one externally supplied input coordinate.
    InputId;
    /// Identity of one continuous state coordinate.
    StateId;
    /// Identity of one continuous algebraic coordinate.
    AlgebraicId;
    /// Identity of one discrete Real coordinate.
    DiscreteRealId;
    /// Identity of one discrete-valued coordinate.
    DiscreteValueId;
    /// Identity of one DAE function declaration.
    FunctionId;
}
owner_local_ids! {
    /// Owner-local identity of one function parameter.
    FunctionParameterId, function: FunctionId;
    /// Owner-local identity of one function output or local value.
    FunctionValueId, function: FunctionId;
    /// Owner-local identity of one function SSA definition.
    FunctionDefinitionId, function: FunctionId;
    /// Owner-local identity of one function loop transition.
    FunctionFoldId, function: FunctionId;
}
branded_ids! {
    /// Identity of one compact comprehension/equation domain.
    DomainId;
}
owner_local_ids! {
    /// Owner-local identity of one binder in a compact DAE domain.
    DomainBinderId, domain: DomainId;
}
branded_ids! {
    /// Identity of one continuous B.1a residual equation.
    ContinuousEquationId;
    /// Identity of one initialization-only residual equation.
    InitializationEquationId;
    /// Identity of one coupled discrete Real B.1b residual equation.
    DiscreteRealEquationId;
    /// Identity of one atomic source-priority B.1c definition owner.
    DiscreteValueOwnerId;
    /// Identity of one condition reserved for a forward body definition.
    ConditionId;
    /// Identity of one primitive relation.
    RelationId;
    /// Identity of one continuously monitored root surface.
    RootId;
    /// Identity of one scheduled event instant.
    TimeEventId;
    /// Identity of one guarded runtime event action.
    EventActionId;
    /// Identity of one synchronous clock.
    ClockId;
    /// Identity proving that one synchronous clock has an exact periodic lattice.
    PeriodicClockId;
    /// Identity of one checked clock-to-variable ownership fact.
    ClockOwnershipId;
    /// Identity of one clock-owned previous-value coordinate.
    PreviousId;
    /// Identity of the terminal-event state coordinate.
    TerminalId;
    /// Identity of one runtime-managed transport-delay channel.
    DelayId;
    /// Identity of one compact continuous equation family.
    ContinuousFamilyId;
    /// Identity of one compact initialization equation family.
    InitializationFamilyId;
}

impl<'dae> From<PeriodicClockId<'dae>> for ClockId<'dae> {
    fn from(value: PeriodicClockId<'dae>) -> Self {
        Self::from_raw(value.index())
    }
}

macro_rules! variable_identity_conversion {
    ($role:ident) => {
        impl<'dae> From<$role<'dae>> for VariableId<'dae> {
            fn from(value: $role<'dae>) -> Self {
                Self::from_raw(value.index())
            }
        }
    };
}

variable_identity_conversion!(ParameterId);
variable_identity_conversion!(InputId);
variable_identity_conversion!(StateId);
variable_identity_conversion!(AlgebraicId);
variable_identity_conversion!(DiscreteRealId);
variable_identity_conversion!(DiscreteValueId);
