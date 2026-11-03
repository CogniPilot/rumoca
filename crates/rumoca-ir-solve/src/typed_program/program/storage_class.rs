use super::SolveStorageClass;

impl SolveStorageClass {
    /// Whether the caller supplies this slot before executing the program.
    pub fn initialized_externally(self) -> bool {
        !matches!(self, Self::Output | Self::MethodLocal)
    }
}
