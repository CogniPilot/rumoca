use super::*;

/// A single borrowed structural derivative authority for all event programs.
/// Storage remains owned by the same lowered layout; no row matching is repeated.
pub(super) struct EventCompilationContext<'layout, 'dae> {
    pub(super) layout: &'layout LoweredLayout<'dae>,
    pub(super) derivatives: &'layout super::super::ContinuousRowIndex<'dae>,
}

impl<'layout, 'dae> std::ops::Deref for EventCompilationContext<'layout, 'dae> {
    type Target = LoweredLayout<'dae>;

    fn deref(&self) -> &Self::Target {
        self.layout
    }
}

impl<'dae> EventCompilationContext<'_, 'dae> {
    /// A primal-only compiler for discrete updates: missing directional support does
    /// not authorize expanding a checked compact call owner.
    pub(super) fn discrete_scalar_compiler<'scope>(
        &'scope self,
        view: dae::DaeView<'dae>,
        point: Option<(dae::DomainId<'dae>, &'scope [i64])>,
    ) -> ScalarCompiler<'scope, 'dae> {
        ScalarCompiler::for_discrete(view, self.layout, point)
            .with_derivative_definitions(self.derivatives)
    }

    pub(super) fn scalar_compiler<'scope>(
        &'scope self,
        view: dae::DaeView<'dae>,
        point: Option<(dae::DomainId<'dae>, &'scope [i64])>,
    ) -> ScalarCompiler<'scope, 'dae> {
        ScalarCompiler::new(view, self.layout, point).with_derivative_definitions(self.derivatives)
    }
}
