//! Bounded disjoint whole-program and complete call tuple storage.
use super::memo::Memo;
use super::*;
use crate::typed_call::program::CallLayout;

pub(in crate::emit) struct CallProgramPlan {
    pub bytes: u32,
    /// Components of `bytes`, reported to the host with the per-owner layouts.
    pub components: Components,
    pub work_bytes: u32,
    /// Bytes of the host Y buffer copied into and published from the work
    /// region; the work region also holds private derived-output slots.
    pub host_y_bytes: u32,
    /// Staging offset and size of the typed output lanes, published to the
    /// output pointer only after every stage succeeds.
    pub lanes: u32,
    pub lane_bytes: u32,
    /// Typed input lanes the host writes at the start of its typed lane
    /// buffer; the published output lanes follow them (SOLVE-C69).
    pub input_lane_bytes: u32,
    /// Private copy of P whose typed-input slots hold the Real views of
    /// their lanes (offset, bytes); empty without typed input lanes.
    pub p_copy: u32,
    pub p_copy_bytes: u32,
    /// Scratch cell holding the host typed lane pointer, which stage bodies
    /// read after they reuse the pointer argument for their targets.
    pub typed_lanes_cell: u32,
    pub input: u32,
    pub output: u32,
    pub scratch: u32,
    pub saved_y: u32,
    pub status: u32,
    pub integer: u32,
    pub helpers: Vec<(solve::SolvePureCallSite, CallLayout)>,
    pub(in crate::emit::compute::call_program) memos: Vec<Memo>,
}

/// Where the whole-program scratch goes, in bytes.
#[derive(Clone, Copy, Debug, Default)]
pub(in crate::emit) struct Components {
    /// Shared by every call: the widest input, output and scratch of any site.
    pub call_input: u32,
    pub call_output: u32,
    pub call_scratch: u32,
    /// The widest scratch of any site laid out without sharing.
    pub unshared_call_scratch: u32,
    pub memos: u32,
    pub lane_stage: u32,
    pub p_copy: u32,
    /// The root owner of the site with the widest scratch.
    pub widest_owner: Option<usize>,
}

impl CallProgramPlan {
    pub(in crate::emit) fn new(
        rows: &[Vec<LinearOp>],
        helpers: &ProgramHelpers,
        y: usize,
        last: u32,
    ) -> Result<Self, String> {
        let mut calls = Vec::new();
        let (mut input, mut output, mut scratch) = (0, 0, 0);
        let mut components = Components::default();
        conditional::visit_operations(rows, |op| {
            let LinearOp::PureCall { site, .. } = op else {
                return Ok(());
            };
            let layout = helpers.layout(site).map_err(|e| e.to_string())?;
            input = input.max(layout.input);
            output = output.max(layout.output);
            if layout.scratch > scratch || components.widest_owner.is_none() {
                components.widest_owner = Some(site.owner().index() as usize);
            }
            scratch = scratch.max(layout.scratch);
            components.unshared_call_scratch =
                components.unshared_call_scratch.max(layout.unshared);
            if !calls.iter().any(|(existing, _)| existing == site) {
                calls.push((site.clone(), layout));
            }
            Ok(())
        })?;
        let work_bytes = y
            .checked_mul(8)
            .and_then(|n| u32::try_from(n).ok())
            .ok_or("native work Y overflow")?;
        let output_offset = work_bytes
            .checked_add(input)
            .ok_or("native call input overflow")?;
        let scratch_offset = output_offset
            .checked_add(output)
            .ok_or("native call output overflow")?;
        let mut bytes = scratch_offset
            .checked_add(scratch)
            .filter(|&n| n <= 64 * 1024 * 1024)
            .ok_or("native whole-program scratch exceeds 64 MiB")?;
        let calls_end = bytes;
        let memos = super::memo::derive(rows, helpers, &mut bytes)?;
        components.call_input = input;
        components.call_output = output;
        components.call_scratch = scratch;
        components.memos = bytes - calls_end;
        Ok(Self {
            bytes,
            components,
            work_bytes,
            host_y_bytes: work_bytes,
            lanes: 0,
            lane_bytes: 0,
            input_lane_bytes: 0,
            p_copy: 0,
            p_copy_bytes: 0,
            typed_lanes_cell: 0,
            input: work_bytes,
            output: output_offset,
            scratch: scratch_offset,
            saved_y: last.checked_add(1).ok_or("native local overflow")?,
            status: last.checked_add(2).ok_or("native local overflow")?,
            integer: last.checked_add(3).ok_or("native local overflow")?,
            helpers: calls,
            memos,
        })
    }

    /// Publish `host_y` of the work scalars to the host Y buffer and stage
    /// `lane_bytes` of typed output lanes after the existing storage; the
    /// host lane buffer starts with `input_lane_bytes` of typed input lanes,
    /// whose Real views a private copy of the `p_scalars` P slots holds.
    pub(in crate::emit) fn with_typed_lanes(
        mut self,
        host_y: usize,
        lane_bytes: usize,
        input_lane_bytes: usize,
        p_scalars: usize,
    ) -> Result<Self, String> {
        self.input_lane_bytes =
            u32::try_from(input_lane_bytes).map_err(|_| "native input lanes overflow")?;
        let host_y_bytes = host_y
            .checked_mul(8)
            .and_then(|n| u32::try_from(n).ok())
            .filter(|&n| n <= self.work_bytes)
            .ok_or("native host Y exceeds the work region")?;
        let lane_bytes = u32::try_from(lane_bytes).map_err(|_| "native output lanes overflow")?;
        let lanes = self
            .bytes
            .checked_next_multiple_of(8)
            .ok_or("native output lanes overflow")?;
        let bytes = lanes
            .checked_add(lane_bytes)
            .filter(|&n| n <= 64 * 1024 * 1024)
            .ok_or("native whole-program scratch exceeds 64 MiB")?;
        self.components.lane_stage = lane_bytes;
        self.host_y_bytes = host_y_bytes;
        self.lanes = lanes;
        self.lane_bytes = lane_bytes;
        self.bytes = bytes;
        if self.input_lane_bytes != 0 {
            self.p_copy_bytes = p_scalars
                .checked_mul(8)
                .and_then(|n| u32::try_from(n).ok())
                .ok_or("native P copy overflows")?;
            self.p_copy = self
                .bytes
                .checked_next_multiple_of(8)
                .ok_or("native P copy overflows")?;
            self.typed_lanes_cell = self
                .p_copy
                .checked_add(self.p_copy_bytes)
                .ok_or("native P copy overflows")?;
            self.components.p_copy = self.p_copy_bytes;
            self.bytes = self
                .typed_lanes_cell
                .checked_add(8)
                .filter(|&n| n <= 64 * 1024 * 1024)
                .ok_or("native whole-program scratch exceeds 64 MiB")?;
        }
        Ok(self)
    }

    pub(super) fn call(&self, site: &solve::SolvePureCallSite) -> Result<CallLayout, String> {
        self.helpers
            .iter()
            .find(|(issued, _)| issued == site)
            .map(|(_, layout)| *layout)
            .ok_or_else(|| "native call differs from the issued model table".into())
    }
}

impl CallProgramPlan {
    /// [`Self::with_typed_lanes`] for the typed lanes `schedule` issues over
    /// the host `layout`.
    pub(in crate::emit) fn with_schedule_lanes(
        self,
        schedule: &solve::NativeRefreshAssignmentSchedule,
        layout: &VarLayout,
    ) -> Result<Self, String> {
        self.with_typed_lanes(
            layout.y_scalars(),
            schedule.lane_bytes(),
            schedule.input_lane_bytes(),
            layout.p_scalars(),
        )
    }
}

impl CallProgramPlan {
    /// The scratch components and the per-owner frame layouts they come from.
    pub(in crate::emit) fn scratch_report(
        &self,
        owners: Vec<crate::ScratchOwner>,
    ) -> crate::ScratchReport {
        let c = &self.components;
        crate::ScratchReport {
            total_bytes: self.bytes,
            work_y_bytes: self.work_bytes,
            call_input_bytes: c.call_input,
            call_output_bytes: c.call_output,
            call_scratch_bytes: c.call_scratch,
            memo_bytes: c.memos,
            typed_lane_bytes: c.lane_stage,
            p_copy_bytes: c.p_copy,
            unshared_call_scratch_bytes: c.unshared_call_scratch,
            widest_owner: c.widest_owner,
            owners,
        }
    }
}
