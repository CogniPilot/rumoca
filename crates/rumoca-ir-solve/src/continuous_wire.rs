//! Wire form of a continuous solve system.
//!
//! Every field is written as is, except that each alternate reduced chart's
//! plan travels as a [`ChartPlanDelta`] against the system itself: its few
//! replaced rows, targets, blocks, and refresh-owner rows and stages. Decoding
//! patches the primary to reproduce each plan. An empty chart set is dropped
//! from human-readable formats, keeping IR without charts byte-identical,
//! while positional binary formats keep every field.

use serde::de::Error as _;
use serde::ser::{Error as _, SerializeStruct};
use serde::{Deserialize, Deserializer, Serialize, Serializer};

use crate::{
    AlgebraicProjectionPlan, ChartExchange, ChartPlanDelta, ComputeBlock, ContinuousRefreshOwners,
    ContinuousSolveSystem, ReducedChart, ReducedChartSet, ScalarSlot,
};

/// One chart on the wire: its partition, trial conditioning, and plan delta.
struct ChartWire<'a> {
    chart: &'a ReducedChart,
    plan: Option<ChartPlanDelta>,
}

impl Serialize for ChartWire<'_> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let omit_plan = serializer.is_human_readable() && self.plan.is_none();
        let mut state =
            serializer.serialize_struct("ReducedChart", if omit_plan { 4 } else { 5 })?;
        state.serialize_field("independent_y_indices", &self.chart.independent_y_indices)?;
        state.serialize_field("dependent_y_indices", &self.chart.dependent_y_indices)?;
        state.serialize_field("trial_rcond", &self.chart.trial_rcond)?;
        state.serialize_field(
            "trial_singular_threshold",
            &self.chart.trial_singular_threshold,
        )?;
        if !omit_plan {
            state.serialize_field("plan", &self.plan)?;
        }
        state.end()
    }
}

#[derive(Deserialize)]
struct ChartWireOwned {
    independent_y_indices: Vec<usize>,
    dependent_y_indices: Vec<usize>,
    trial_rcond: f64,
    trial_singular_threshold: f64,
    #[serde(default)]
    plan: Option<ChartPlanDelta>,
}

struct ChartSetWire<'a> {
    charts: Vec<ChartWire<'a>>,
    exchanges: &'a [ChartExchange],
}

impl Serialize for ChartSetWire<'_> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let omit_exchanges = serializer.is_human_readable() && self.exchanges.is_empty();
        let field_count = if omit_exchanges { 1 } else { 2 };
        let mut state = serializer.serialize_struct("ReducedChartSet", field_count)?;
        state.serialize_field("charts", &self.charts)?;
        if !omit_exchanges {
            state.serialize_field("exchanges", &self.exchanges)?;
        }
        state.end()
    }
}

#[derive(Default, Deserialize)]
struct ChartSetWireOwned {
    charts: Vec<ChartWireOwned>,
    #[serde(default)]
    exchanges: Vec<ChartExchange>,
}

impl Serialize for ContinuousSolveSystem {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let charts = self
            .reduced_chart_set
            .charts
            .iter()
            .map(|chart| {
                let plan = chart
                    .plan
                    .as_ref()
                    .map(|plan| ChartPlanDelta::diff(self, plan))
                    .transpose()
                    .map_err(S::Error::custom)?;
                Ok(ChartWire { chart, plan })
            })
            .collect::<Result<Vec<_>, S::Error>>()?;
        let omit_charts = serializer.is_human_readable() && charts.is_empty();
        let field_count = if omit_charts { 8 } else { 9 };
        let mut state = serializer.serialize_struct("ContinuousSolveSystem", field_count)?;
        state.serialize_field("implicit_rhs", &self.implicit_rhs)?;
        state.serialize_field("implicit_row_targets", &self.implicit_row_targets)?;
        state.serialize_field("algebraic_projection_plan", &self.algebraic_projection_plan)?;
        state.serialize_field("residual", &self.residual)?;
        state.serialize_field("manifold_residual", &self.manifold_residual)?;
        state.serialize_field("manifold_projection_plan", &self.manifold_projection_plan)?;
        state.serialize_field("derivative_rhs", &self.derivative_rhs)?;
        state.serialize_field("refresh_owners", &self.refresh_owners)?;
        if !omit_charts {
            state.serialize_field(
                "reduced_chart_set",
                &ChartSetWire {
                    charts,
                    exchanges: &self.reduced_chart_set.exchanges,
                },
            )?;
        }
        state.end()
    }
}

#[derive(Deserialize)]
struct ContinuousSolveSystemWire {
    implicit_rhs: ComputeBlock,
    implicit_row_targets: Vec<Option<ScalarSlot>>,
    algebraic_projection_plan: AlgebraicProjectionPlan,
    residual: ComputeBlock,
    manifold_residual: ComputeBlock,
    manifold_projection_plan: AlgebraicProjectionPlan,
    derivative_rhs: ComputeBlock,
    refresh_owners: ContinuousRefreshOwners,
    #[serde(default)]
    reduced_chart_set: ChartSetWireOwned,
}

impl<'de> Deserialize<'de> for ContinuousSolveSystem {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let wire = ContinuousSolveSystemWire::deserialize(deserializer)?;
        let mut system = Self {
            implicit_rhs: wire.implicit_rhs,
            implicit_row_targets: wire.implicit_row_targets,
            algebraic_projection_plan: wire.algebraic_projection_plan,
            residual: wire.residual,
            manifold_residual: wire.manifold_residual,
            manifold_projection_plan: wire.manifold_projection_plan,
            derivative_rhs: wire.derivative_rhs,
            refresh_owners: wire.refresh_owners,
            reduced_chart_set: ReducedChartSet::default(),
        };
        let charts = wire
            .reduced_chart_set
            .charts
            .into_iter()
            .map(|chart| {
                let plan = chart
                    .plan
                    .map(|delta| delta.apply(&system))
                    .transpose()
                    .map_err(D::Error::custom)?;
                Ok(ReducedChart {
                    independent_y_indices: chart.independent_y_indices,
                    dependent_y_indices: chart.dependent_y_indices,
                    trial_rcond: chart.trial_rcond,
                    trial_singular_threshold: chart.trial_singular_threshold,
                    plan,
                })
            })
            .collect::<Result<Vec<_>, D::Error>>()?;
        system.reduced_chart_set = ReducedChartSet {
            charts,
            exchanges: wire.reduced_chart_set.exchanges,
        };
        Ok(system)
    }
}
