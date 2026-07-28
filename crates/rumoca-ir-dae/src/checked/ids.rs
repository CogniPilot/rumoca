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
branded_id!(
    /// Identity of one compact comprehension/equation domain.
    DomainId
);
branded_id!(
    /// Identity of one checked equation.
    EquationId
);
branded_id!(
    /// Identity of one condition reserved for a forward body definition.
    ConditionId
);
