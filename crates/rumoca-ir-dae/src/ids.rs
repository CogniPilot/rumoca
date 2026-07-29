use std::marker::PhantomData;

/// Invariant construction brand. It is erased from final storage and wire data.
type Brand<'dae> = PhantomData<&'dae mut &'dae ()>;

macro_rules! branded_id {
    ($(#[$meta:meta])* $name:ident) => {
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
    };
}

branded_id!(
    /// Identity of one node in a DAE-wide expression arena.
    ExprId
);
branded_id!(
    /// Canonical identity of an expression value type.
    ValueTypeId
);
branded_id!(
    /// Identity of one DAE variable declaration.
    VariableId
);
branded_id!(
    /// Identity of one parameter or constant coordinate.
    ParameterId
);
branded_id!(
    /// Identity of one externally supplied input coordinate.
    InputId
);
branded_id!(
    /// Identity of one continuous state coordinate.
    StateId
);
branded_id!(
    /// Identity of one continuous algebraic coordinate.
    AlgebraicId
);
branded_id!(
    /// Identity of one discrete Real coordinate.
    DiscreteRealId
);
branded_id!(
    /// Identity of one discrete-valued coordinate.
    DiscreteValueId
);
branded_id!(
    /// Identity of one DAE function declaration.
    FunctionId
);

/// Owner-local identity of one function parameter.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionParameterId<'dae> {
    function: u32,
    ordinal: u32,
    brand: Brand<'dae>,
}

impl<'dae> FunctionParameterId<'dae> {
    pub(crate) const fn from_raw(function: u32, ordinal: u32) -> Self {
        Self {
            function,
            ordinal,
            brand: PhantomData,
        }
    }

    pub const fn function(self) -> FunctionId<'dae> {
        FunctionId::from_raw(self.function)
    }

    pub const fn ordinal(self) -> u32 {
        self.ordinal
    }
}

impl Copy for FunctionParameterId<'_> {}

impl Clone for FunctionParameterId<'_> {
    fn clone(&self) -> Self {
        *self
    }
}

/// Owner-local identity of one function output or local value.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionValueId<'dae> {
    function: u32,
    ordinal: u32,
    brand: Brand<'dae>,
}

impl<'dae> FunctionValueId<'dae> {
    pub(crate) const fn from_raw(function: u32, ordinal: u32) -> Self {
        Self {
            function,
            ordinal,
            brand: PhantomData,
        }
    }

    pub const fn function(self) -> FunctionId<'dae> {
        FunctionId::from_raw(self.function)
    }

    pub const fn ordinal(self) -> u32 {
        self.ordinal
    }
}

impl Copy for FunctionValueId<'_> {}

impl Clone for FunctionValueId<'_> {
    fn clone(&self) -> Self {
        *self
    }
}

/// Owner-local identity of one function loop transition.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionFoldId<'dae> {
    function: u32,
    ordinal: u32,
    brand: Brand<'dae>,
}

impl<'dae> FunctionFoldId<'dae> {
    pub(crate) const fn from_raw(function: u32, ordinal: u32) -> Self {
        Self {
            function,
            ordinal,
            brand: PhantomData,
        }
    }

    pub const fn function(self) -> FunctionId<'dae> {
        FunctionId::from_raw(self.function)
    }

    pub const fn ordinal(self) -> u32 {
        self.ordinal
    }
}

impl Copy for FunctionFoldId<'_> {}

impl Clone for FunctionFoldId<'_> {
    fn clone(&self) -> Self {
        *self
    }
}
branded_id!(
    /// Identity of one compact comprehension/equation domain.
    DomainId
);

/// Owner-local identity of one binder in a compact DAE domain.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DomainBinderId<'dae> {
    domain: u32,
    ordinal: u32,
    brand: Brand<'dae>,
}

impl<'dae> DomainBinderId<'dae> {
    pub(crate) const fn from_raw(domain: u32, ordinal: u32) -> Self {
        Self {
            domain,
            ordinal,
            brand: PhantomData,
        }
    }

    pub const fn domain(self) -> DomainId<'dae> {
        DomainId::from_raw(self.domain)
    }

    pub const fn ordinal(self) -> u32 {
        self.ordinal
    }
}

impl Copy for DomainBinderId<'_> {}

impl Clone for DomainBinderId<'_> {
    fn clone(&self) -> Self {
        *self
    }
}
branded_id!(
    /// Identity of one continuous B.1a residual equation.
    ContinuousEquationId
);
branded_id!(
    /// Identity of one initialization-only residual equation.
    InitializationEquationId
);
branded_id!(
    /// Identity of one coupled discrete Real B.1b residual equation.
    DiscreteRealEquationId
);
branded_id!(
    /// Identity of one ordered discrete-valued B.1c assignment.
    DiscreteAssignmentId
);
branded_id!(
    /// Identity of one condition reserved for a forward body definition.
    ConditionId
);
branded_id!(
    /// Identity of one primitive relation.
    RelationId
);
branded_id!(
    /// Identity of one continuously monitored root surface.
    RootId
);
branded_id!(
    /// Identity of one scheduled event instant.
    TimeEventId
);
branded_id!(
    /// Identity of one guarded runtime event action.
    EventActionId
);
branded_id!(
    /// Identity of one synchronous clock.
    ClockId
);
branded_id!(
    /// Identity of one checked clock-to-variable ownership fact.
    ClockOwnershipId
);
branded_id!(
    /// Identity of one clock-owned previous-value coordinate.
    PreviousId
);
branded_id!(
    /// Identity of the terminal-event state coordinate.
    TerminalId
);
branded_id!(
    /// Identity of one runtime-managed transport-delay channel.
    DelayId
);
branded_id!(
    /// Identity of one compact continuous equation family.
    ContinuousFamilyId
);
branded_id!(
    /// Identity of one compact initialization equation family.
    InitializationFamilyId
);

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
