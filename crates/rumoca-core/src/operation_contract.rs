//! Target-neutral operation-contract key vocabulary.

use serde::{Deserialize, Serialize};

macro_rules! operation_contract_keys {
    ($($variant:ident),+ $(,)?) => {
        /// Complete closed key vocabulary for the semantic leaves in SPEC_0049.
        ///
        /// This enum names contracts. It does not admit an operation, select a
        /// contract, or implement one. Those authorities remain with checked
        /// root construction and target preparation.
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
        #[serde(rename_all = "kebab-case")]
        pub enum OperationContractKey {
            $($variant),+
        }

        impl OperationContractKey {
            /// Exhaustive discriminant census for exact profile coverage.
            pub const ALL: &'static [Self] = &[$(Self::$variant),+];
        }
    };
}

operation_contract_keys! {
    Constant,
    Load,
    Store,
    UnaryNotBoolean,
    UnaryNegateInteger,
    UnaryNegateReal,
    UnaryAbsInteger,
    UnaryAbsReal,
    UnarySignInteger,
    UnarySignReal,
    UnarySqrtReal,
    UnaryFloorReal,
    UnaryCeilingReal,
    UnaryTruncateReal,
    UnarySinReal,
    UnaryCosReal,
    UnaryTanReal,
    UnaryAsinReal,
    UnaryAcosReal,
    UnaryAtanReal,
    UnarySinhReal,
    UnaryCoshReal,
    UnaryTanhReal,
    UnaryExpReal,
    UnaryLogReal,
    UnaryLog10Real,
    BinaryAndBoolean,
    BinaryOrBoolean,
    BinaryAddInteger,
    BinaryAddReal,
    BinarySubtractInteger,
    BinarySubtractReal,
    BinaryMultiplyInteger,
    BinaryMultiplyReal,
    BinaryMinInteger,
    BinaryMinReal,
    BinaryMaxInteger,
    BinaryMaxReal,
    BinaryDivideInteger,
    BinaryDivideReal,
    BinaryPowerInteger,
    BinaryPowerReal,
    BinaryAtan2Real,
    CompareEqualBoolean,
    CompareEqualInteger,
    CompareEqualReal,
    CompareNotEqualBoolean,
    CompareNotEqualInteger,
    CompareNotEqualReal,
    CompareLessInteger,
    CompareLessReal,
    CompareLessEqualInteger,
    CompareLessEqualReal,
    CompareGreaterInteger,
    CompareGreaterReal,
    CompareGreaterEqualInteger,
    CompareGreaterEqualReal,
    ConvertIntegerToReal,
    ConvertRealToIntegerTowardZero,
    ConvertRealToIntegerTowardNegativeInfinity,
    Select,
    Conditional,
    Map,
    Fold,
    ReduceAllBoolean,
    ReduceSumInteger,
    ReduceSumReal,
    ReduceProductInteger,
    ReduceProductReal,
    ReduceMinimumInteger,
    ReduceMinimumReal,
    ReduceMaximumInteger,
    ReduceMaximumReal,
    ScaleInteger,
    ScaleReal,
    BroadcastAndBoolean,
    BroadcastOrBoolean,
    BroadcastAddInteger,
    BroadcastAddReal,
    BroadcastSubtractInteger,
    BroadcastSubtractReal,
    BroadcastMultiplyInteger,
    BroadcastMultiplyReal,
    BroadcastMinInteger,
    BroadcastMinReal,
    BroadcastMaxInteger,
    BroadcastMaxReal,
    BroadcastDivideInteger,
    BroadcastDivideReal,
    BroadcastPowerInteger,
    BroadcastPowerReal,
    BroadcastAtan2Real,
    Transpose,
    MatrixMultiplyInteger,
    MatrixMultiplyReal,
    CrossInteger,
    CrossReal,
    IdentityInteger,
    IdentityReal,
    Diagonal,
    Concatenate,
    Fill,
    ConstructAggregate,
    ProjectElement,
    ProjectElementDynamic,
    SelectElement,
    ProjectSlice,
    ProjectView,
    UpdateElement,
    UpdateSlice,
    UpdateView,
    Call,
    InvokePure,
    InvokeImpure,
    EffectVolatile,
    Terminator,
    DeclarationInitialization,
    ErrorSignalReset,
    LifecycleMethod,
}

#[cfg(test)]
mod tests {
    use super::OperationContractKey;
    use std::collections::BTreeSet;

    #[test]
    fn operation_contract_discriminant_census_is_unique() {
        let unique = OperationContractKey::ALL
            .iter()
            .copied()
            .collect::<BTreeSet<_>>();
        assert_eq!(unique.len(), OperationContractKey::ALL.len());
    }
}
