//! Named temporary capability boundaries for Algorithm Code refinement.

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnsupportedTensorInitializationPlan {
    NonUniformLiteral,
    Symbolic,
}

impl UnsupportedTensorInitializationPlan {
    #[must_use]
    pub const fn feature_name(self) -> &'static str {
        match self {
            Self::NonUniformLiteral => "future-non-uniform-tensor-initialization-plan",
            Self::Symbolic => "future-symbolic-tensor-initialization-plan",
        }
    }
}

impl std::fmt::Display for UnsupportedTensorInitializationPlan {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(match self {
            Self::NonUniformLiteral => "future non-uniform tensor initialization plan",
            Self::Symbolic => "future symbolic tensor initialization plan",
        })
    }
}
