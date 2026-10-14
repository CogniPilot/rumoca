//! Bounded disjoint whole-program and complete call tuple storage.
use super::memo::Memo;
use super::*;
use crate::typed_call::program::CallLayout;

pub(in crate::emit) struct CallProgramPlan {
    layout: CallProgramLayout,
    #[cfg(test)]
    policy: crate::CheckedNativeWasmCapacityPolicy,
}

pub(in crate::emit) struct CallProgramLayout {
    pub bytes: u32,
    /// Components of `bytes`, reported to the host with the per-owner layouts.
    pub components: Components,
    pub work_bytes: u32,
    /// Bytes of the host Y buffer published from the work region; the work
    /// region also holds private derived-output slots.
    pub host_y_bytes: u32,
    /// The byte ranges of the host Y buffer copied into the work region: the
    /// cells some operation reads. Every Y cell is the target of one stage, so
    /// the other cells are written before they are published.
    pub y_reads: Vec<std::ops::Range<u32>>,
    /// Staging offset and size of the typed output lanes, published to the
    /// output pointer only after every stage succeeds.
    pub lanes: u32,
    pub lane_bytes: u32,
    /// Typed input lanes the host writes at the start of its typed lane
    /// buffer; the published output lanes follow them (SOLVE-C69).
    pub input_lane_bytes: u32,
    /// Saved P cells (offset, bytes): the typed-input slots the stages patch
    /// in place with the Real views of their lanes, restored when the
    /// program returns; empty without typed input lanes.
    pub p_save: u32,
    pub p_save_bytes: u32,
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
    pub directional_helpers: Vec<(solve::SolvePureCallDirectionalSite, CallLayout)>,
    pub(in crate::emit::compute::call_program) memos: Vec<Memo>,
    /// The runs of host Y the program publishes into P (SOLVE-C101), in Y order.
    pub carried: Vec<CarriedRun>,
    /// The staged output lane bytes the program publishes into the input lanes
    /// instead of the output lanes (SOLVE-C101), in staging order.
    pub carried_lanes: Vec<CarriedLaneCopy>,
    /// The pieces of carried runs whose commit a call store decides: the glue
    /// records a piece when it builds the piece's value in place.
    pub pieces: std::cell::RefCell<Vec<CommitPiece>>,
    /// The commit flag cells not yet given to a piece, and the end of the pool.
    /// A piece is the overlap of a carried run with a call output, and runs and
    /// outputs are each disjoint, so at most `runs + outputs` overlaps exist.
    next_flag: std::cell::Cell<u32>,
    flag_end: u32,
    /// Outputs of the call occurrences of the program.
    call_outputs: usize,
}

/// One carried run as the entry publishes it.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::emit) struct CarriedRun {
    /// Byte range of the run in the work Y region.
    pub y: std::ops::Range<u32>,
    /// Byte offset of the paired cells in P.
    pub p: u32,
}

/// One carried lane run as the entry publishes it.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::emit) struct CarriedLaneCopy {
    /// Byte range of the run in the staged output lanes.
    pub from: std::ops::Range<u32>,
    /// Byte offset in the host input lanes it is published to.
    pub to: u32,
}

/// The part of a carried run that one call output writes in place.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::emit) struct CommitPiece {
    /// Byte range of the piece in the work Y region.
    pub y: std::ops::Range<u32>,
    /// Byte offset of the piece within the output that holds it.
    pub at: u32,
    /// Byte offset in P of the cells the piece is published to.
    pub p: u32,
    /// Scratch offset of the cell that is nonzero when the work cells must be
    /// published: the output's store writes it.
    pub flag: u32,
}

/// The byte ranges of Y that any operation of `rows` reads.
fn y_read_bytes(rows: &[Vec<LinearOp>]) -> Result<Vec<std::ops::Range<u32>>, String> {
    let mut cells = Vec::new();
    conditional::visit_operations(rows, |op| {
        match op {
            LinearOp::LoadY { index, .. } => cells.push(*index..*index + 1),
            LinearOp::TensorLoad {
                input: solve::TensorInputKind::Y,
                input_start,
                count,
                ..
            } => cells.push(*input_start..input_start.saturating_add(*count)),
            _ => {}
        }
        Ok(())
    })?;
    merge_cells(cells, crate::cost_model::joined_copy_gap_cells())
        .into_iter()
        .map(|range| {
            let bytes = |cell: usize| {
                cell.checked_mul(8)
                    .and_then(|n| u32::try_from(n).ok())
                    .ok_or("native Y read overflows")
            };
            Ok(bytes(range.start)?..bytes(range.end)?)
        })
        .collect()
}

/// The union of the ranges, with ranges at most `gap` apart joined.
fn merge_cells(mut ranges: Vec<std::ops::Range<usize>>, gap: usize) -> Vec<std::ops::Range<usize>> {
    ranges.sort_by_key(|range| range.start);
    let mut merged: Vec<std::ops::Range<usize>> = Vec::new();
    for range in ranges {
        match merged.last_mut() {
            Some(last) if range.start <= last.end.saturating_add(gap) => {
                last.end = last.end.max(range.end);
            }
            _ => merged.push(range),
        }
    }
    merged
}

/// Where the whole-program scratch goes, in bytes.
#[derive(Clone, Copy, Debug, Default)]
pub(in crate::emit) struct Components {
    /// Shared by every call: the widest input, output and scratch of any site.
    pub call_input: u32,
    pub call_output: u32,
    pub call_scratch: u32,
    /// The widest scratch of any site laid out without sharing.
    pub unshared_call_scratch: u64,
    pub memos: u32,
    pub lane_stage: u32,
    pub p_save: u32,
    /// The root owner of the site with the widest scratch.
    pub widest_owner: Option<usize>,
}

impl CallProgramLayout {
    fn derive(
        rows: &[Vec<LinearOp>],
        helpers: &ProgramHelpers,
        y: usize,
        last: u32,
    ) -> Result<Self, String> {
        let mut calls = Vec::new();
        let mut directional_calls = Vec::new();
        let (mut input, mut output, mut scratch) = (0, 0, 0);
        let mut call_outputs = 0usize;
        let mut components = Components::default();
        conditional::visit_operations(rows, |op| {
            let (layout, owner, outputs) = match op {
                LinearOp::PureCall { site, .. } => {
                    (helpers.layout(site), site.owner(), site.outputs().len())
                }
                LinearOp::PureCallObservation { site, .. } => (
                    helpers.layout(site.value_site()),
                    site.value_site().owner(),
                    site.value_site().outputs().len(),
                ),
                LinearOp::PureCallDirectional { site, .. } => (
                    helpers.directional_layout(site),
                    site.owner(),
                    site.outputs().len(),
                ),
                _ => return Ok(()),
            };
            call_outputs += outputs;
            let layout = layout.map_err(|error| error.to_string())?;
            input = input.max(
                layout
                    .input
                    .checked_add(super::glue::staging_bytes(&layout.abi)?)
                    .ok_or("native call staging overflows")?,
            );
            output = output.max(layout.output);
            if layout.scratch > scratch || components.widest_owner.is_none() {
                components.widest_owner = Some(owner.index() as usize);
            }
            scratch = scratch.max(layout.scratch);
            components.unshared_call_scratch =
                components.unshared_call_scratch.max(layout.unshared);
            match op {
                LinearOp::PureCall { site, .. } => insert_call(&mut calls, site, layout),
                LinearOp::PureCallObservation { site, .. } => {
                    insert_call(&mut calls, site.value_site(), layout)
                }
                LinearOp::PureCallDirectional { site, .. } => {
                    insert_call(&mut directional_calls, site, layout)
                }
                _ => return Err("call layout lost its exact source form".into()),
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
            .ok_or("native whole-program scratch overflows memory32")?;
        let calls_end = bytes;
        let memos = super::memo::derive(rows, helpers, &mut bytes)?;
        let y_reads = y_read_bytes(rows)?;
        components.call_input = input;
        components.call_output = output;
        components.call_scratch = scratch;
        components.memos = bytes - calls_end;
        Ok(Self {
            bytes,
            components,
            work_bytes,
            host_y_bytes: work_bytes,
            y_reads,
            lanes: 0,
            lane_bytes: 0,
            input_lane_bytes: 0,
            p_save: 0,
            p_save_bytes: 0,
            typed_lanes_cell: 0,
            input: work_bytes,
            output: output_offset,
            scratch: scratch_offset,
            saved_y: last.checked_add(1).ok_or("native local overflow")?,
            status: last.checked_add(2).ok_or("native local overflow")?,
            integer: last.checked_add(3).ok_or("native local overflow")?,
            helpers: calls,
            directional_helpers: directional_calls,
            memos,
            carried: Vec::new(),
            carried_lanes: Vec::new(),
            pieces: Default::default(),
            next_flag: Default::default(),
            flag_end: 0,
            call_outputs,
        })
    }

    /// Publish `host_y` of the work scalars to the host Y buffer and stage
    /// `lane_bytes` of typed output lanes after the existing storage; the
    /// host lane buffer starts with `input_lane_bytes` of typed input lanes,
    /// whose Real views the `patched_cells` typed-input P slots hold while the
    /// stages run; the program saves and restores those cells.
    pub(in crate::emit) fn with_typed_lanes(
        mut self,
        host_y: usize,
        lane_bytes: usize,
        input_lane_bytes: usize,
        patched_cells: usize,
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
            .ok_or("native whole-program scratch overflows memory32")?;
        self.components.lane_stage = lane_bytes;
        self.host_y_bytes = host_y_bytes;
        self.y_reads.retain_mut(|range| {
            range.end = range.end.min(host_y_bytes);
            range.start < range.end
        });
        self.lanes = lanes;
        self.lane_bytes = lane_bytes;
        self.bytes = bytes;
        if self.input_lane_bytes != 0 {
            self.p_save_bytes = patched_cells
                .checked_mul(8)
                .and_then(|n| u32::try_from(n).ok())
                .ok_or("native P save overflows")?;
            self.p_save = self
                .bytes
                .checked_next_multiple_of(8)
                .ok_or("native P save overflows")?;
            self.typed_lanes_cell = self
                .p_save
                .checked_add(self.p_save_bytes)
                .ok_or("native P save overflows")?;
            self.components.p_save = self.p_save_bytes;
            self.bytes = self
                .typed_lanes_cell
                .checked_add(8)
                .ok_or("native whole-program scratch overflows memory32")?;
        }
        Ok(self)
    }

    /// Publish the carried runs of `state` into P instead of the host Y
    /// (SOLVE-C101) and reserve the commit flag cells their pieces may need.
    pub(in crate::emit) fn with_carried(
        mut self,
        state: &solve::NativeCarriedState,
    ) -> Result<Self, String> {
        let bytes = |cell: usize| {
            cell.checked_mul(8)
                .and_then(|n| u32::try_from(n).ok())
                .ok_or("native carried range overflows")
        };
        for run in state.runs() {
            let (y, p) = (run.y_range(), run.p_range());
            let y = bytes(y.start)?..bytes(y.end)?;
            if y.end > self.host_y_bytes {
                return Err("native carried range exceeds the host Y".into());
            }
            self.carried.push(CarriedRun {
                y,
                p: bytes(p.start)?,
            });
        }
        for lane in state.lanes() {
            let (from, to) = (lane.output_range(), lane.input_range());
            let narrow =
                |bytes: usize| u32::try_from(bytes).map_err(|_| "native carried lane overflows");
            let (from, to) = (narrow(from.start)?..narrow(from.end)?, narrow(to.start)?);
            if from.end > self.lane_bytes || to + (from.end - from.start) > self.input_lane_bytes {
                return Err("native carried lane exceeds the typed lanes".into());
            }
            self.carried_lanes.push(CarriedLaneCopy { from, to });
        }
        let flags = self
            .carried
            .len()
            .checked_add(self.call_outputs)
            .and_then(|flags| u32::try_from(flags).ok())
            .and_then(|flags| flags.checked_mul(8))
            .ok_or("native carried flags overflow")?;
        let first = self
            .bytes
            .checked_next_multiple_of(8)
            .ok_or("native carried flags overflow")?;
        self.next_flag.set(first);
        self.flag_end = first
            .checked_add(flags)
            .ok_or("native carried flags overflow")?;
        self.bytes = self.flag_end;
        Ok(self)
    }

    /// The pieces of carried runs that the output occupying the work Y bytes
    /// `output` writes in place, each recorded for the final publication. A
    /// piece is an overlap of the output with a run that no operation reads
    /// from Y, so leaving its work cells unwritten is unobservable; the rest of
    /// the output and of each run is written and published unconditionally.
    pub(in crate::emit) fn commit_pieces(&self, output: &std::ops::Range<u32>) -> Vec<CommitPiece> {
        self.carried
            .iter()
            .filter_map(|run| {
                let y = run.y.start.max(output.start)..run.y.end.min(output.end);
                let readable = y.start >= y.end
                    || self
                        .y_reads
                        .iter()
                        .any(|range| range.start < y.end && y.start < range.end);
                (!readable).then(|| self.piece(run, y, output.start))?
            })
            .collect()
    }

    /// The piece of `run` over `y`, whose output starts at `output_start`: the
    /// recorded one, or a new one with the next flag cell of the pool.
    fn piece(
        &self,
        run: &CarriedRun,
        y: std::ops::Range<u32>,
        output_start: u32,
    ) -> Option<CommitPiece> {
        if let Some(known) = self.pieces.borrow().iter().find(|piece| piece.y == y) {
            return Some(known.clone());
        }
        let flag = self.next_flag.get();
        if flag >= self.flag_end {
            return None;
        }
        self.next_flag.set(flag + 8);
        let piece = CommitPiece {
            at: y.start - output_start,
            p: run.p + (y.start - run.y.start),
            y,
            flag,
        };
        self.pieces.borrow_mut().push(piece.clone());
        Some(piece)
    }

    pub(in crate::emit) fn call(
        &self,
        site: &solve::SolvePureCallSite,
    ) -> Result<CallLayout, String> {
        self.helpers
            .iter()
            .find(|(issued, _)| issued == site)
            .map(|(_, layout)| layout.clone())
            .ok_or_else(|| "native call differs from the issued model table".into())
    }
}

impl CallProgramLayout {
    fn admit(
        self,
        policy: crate::CheckedNativeWasmCapacityPolicy,
    ) -> Result<CallProgramPlan, String> {
        policy.require_public_bytes(self.bytes)?;
        Ok(CallProgramPlan {
            layout: self,
            #[cfg(test)]
            policy,
        })
    }
}

impl CallProgramPlan {
    pub(in crate::emit) fn with_private_extension_bytes(
        mut self,
        bytes: u32,
    ) -> Result<Self, String> {
        crate::CheckedNativeWasmCapacityPolicy::default().require_public_bytes(bytes)?;
        self.layout.components.call_scratch += bytes
            .checked_sub(self.layout.bytes)
            .ok_or("private scratch extension size shrank")?;
        self.layout.bytes = bytes;
        Ok(self)
    }

    pub(in crate::emit) fn view(&self) -> &CallProgramLayout {
        &self.layout
    }

    pub(in crate::emit) fn new(
        rows: &[Vec<LinearOp>],
        helpers: &ProgramHelpers,
        y: usize,
        last: u32,
    ) -> Result<Self, String> {
        Self::new_with_policy(
            rows,
            helpers,
            y,
            last,
            crate::CheckedNativeWasmCapacityPolicy::default(),
        )
    }

    pub(in crate::emit) fn new_with_policy(
        rows: &[Vec<LinearOp>],
        helpers: &ProgramHelpers,
        y: usize,
        last: u32,
        policy: crate::CheckedNativeWasmCapacityPolicy,
    ) -> Result<Self, String> {
        CallProgramLayout::derive(rows, helpers, y, last)?.admit(policy)
    }

    #[cfg(test)]
    pub(in crate::emit) fn with_typed_lanes(
        self,
        host_y: usize,
        lane_bytes: usize,
        input_lane_bytes: usize,
        patched_cells: usize,
    ) -> Result<Self, String> {
        self.layout
            .with_typed_lanes(host_y, lane_bytes, input_lane_bytes, patched_cells)?
            .admit(self.policy)
    }

    #[cfg(test)]
    pub(in crate::emit) fn with_carried(
        self,
        state: &solve::NativeCarriedState,
    ) -> Result<Self, String> {
        self.layout.with_carried(state)?.admit(self.policy)
    }

    pub(in crate::emit) fn call(
        &self,
        site: &solve::SolvePureCallSite,
    ) -> Result<CallLayout, String> {
        self.layout.call(site)
    }

    pub(in crate::emit) fn directional_call(
        &self,
        site: &solve::SolvePureCallDirectionalSite,
    ) -> Result<CallLayout, String> {
        self.layout
            .directional_helpers
            .iter()
            .find(|(issued, _)| issued == site)
            .map(|(_, layout)| layout.clone())
            .ok_or_else(|| "directional call differs from its issued model table".into())
    }

    pub(in crate::emit) fn for_schedule(
        rows: &[Vec<LinearOp>],
        helpers: &ProgramHelpers,
        last: u32,
        schedule: &solve::NativeRefreshAssignmentSchedule,
        layout: &VarLayout,
        policy: crate::CheckedNativeWasmCapacityPolicy,
    ) -> Result<Self, String> {
        CallProgramLayout::derive(rows, helpers, schedule.work_layout().y_scalars(), last)?
            .with_typed_lanes(
                layout.y_scalars(),
                schedule.lane_bytes(),
                schedule.input_lane_bytes(),
                schedule
                    .input_runs()
                    .iter()
                    .filter(|run| run.view_is_read())
                    .map(solve::NativeInputRun::scalar_count)
                    .sum(),
            )?
            .with_carried(schedule.carried_state())?
            .admit(policy)
    }
}

impl CallProgramPlan {
    /// The scratch components and the per-owner frame layouts they come from.
    pub(in crate::emit) fn scratch_report(
        &self,
        owners: Vec<crate::ScratchOwner>,
    ) -> crate::ScratchReport {
        let layout = self.view();
        let c = &layout.components;
        crate::ScratchReport {
            total_bytes: layout.bytes,
            work_y_bytes: layout.work_bytes,
            call_input_bytes: c.call_input,
            call_output_bytes: c.call_output,
            call_scratch_bytes: c.call_scratch,
            memo_bytes: c.memos,
            typed_lane_bytes: c.lane_stage,
            p_save_bytes: c.p_save,
            unshared_call_scratch_bytes: c.unshared_call_scratch,
            widest_owner: c.widest_owner,
            owners,
        }
    }
}

#[cfg(test)]
mod capacity_tests {
    use super::*;

    fn memo_fixture() -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
        let arithmetic = solve::SolveArithmeticProfile::construct(
            solve::SolveRealFormat::Binary64,
            solve::SolveIntegerDomain::FULL,
        );
        let scalar = solve::SolveValueType::scalar(solve::SolveScalarType::real(arithmetic));
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("complete_demand.mo"),
            0,
            1,
        );
        let mut table = solve::SolvePureCallTable::builder(arithmetic);
        let owner = table
            .add_owner(
                solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(1).unwrap()),
                vec![scalar.clone()],
                vec![solve::SolvePureCallOutput::result(scalar)],
                span,
                |builder, inputs, outputs| {
                    let value = builder.load(inputs[0], span)?;
                    builder.store(outputs[0], value, span)
                },
            )
            .unwrap();
        let site = table.call_site(owner).unwrap();
        (table.finish(), site)
    }

    #[test]
    fn complete_demand_retains_memos_lanes_and_p_save_before_admission() {
        let (table, site) = memo_fixture();
        let helpers = ProgramHelpers::new(&table, std::slice::from_ref(&site)).unwrap();
        let rows = [vec![
            LinearOp::LoadP { dst: 0, index: 0 },
            LinearOp::PureCall {
                dst_start: 1,
                input_starts: vec![0].into_boxed_slice(),
                site,
            },
        ]];
        let pending = CallProgramLayout::derive(&rows, &helpers, 8000, 0).unwrap();
        assert_eq!(pending.memos.len(), 1);
        assert_eq!(pending.components.memos, 8);
        let base = pending.bytes;
        assert!(base < 65536);
        let complete = pending.with_typed_lanes(8000, 2000, 8, 100).unwrap();
        assert_eq!(complete.lanes, base.next_multiple_of(8));
        assert_eq!(complete.p_save, (complete.lanes + 2000).next_multiple_of(8));
        let required = complete.p_save + 800 + 8;
        assert_eq!(complete.bytes, required);
        let policy =
            crate::CheckedNativeWasmCapacityPolicy::new(crate::NativeWasmCapacityRequest {
                private_arena_bytes: 65536,
                public_scratch_bytes: 65536,
            })
            .unwrap();
        let error = complete.admit(policy).err().unwrap();
        assert!(error.contains(&format!("required_bytes={required} capacity_bytes=65536")));
        let larger =
            crate::CheckedNativeWasmCapacityPolicy::new(crate::NativeWasmCapacityRequest {
                private_arena_bytes: 65536,
                public_scratch_bytes: 131072,
            })
            .unwrap();
        let admitted = CallProgramLayout::derive(&rows, &helpers, 8000, 0)
            .unwrap()
            .with_typed_lanes(8000, 2000, 8, 100)
            .unwrap()
            .admit(larger)
            .unwrap();
        assert_eq!(admitted.view().bytes, required);
        assert_eq!(admitted.scratch_report(vec![]).memo_bytes, 8);
    }

    #[test]
    fn explicit_public_bound_checks_complete_work_lanes_and_p_save() {
        let table = solve::SolvePureCallTable::default();
        let helpers = ProgramHelpers::new(&table, &[]).unwrap();
        let policy = |pages: u32| {
            crate::CheckedNativeWasmCapacityPolicy::new(crate::NativeWasmCapacityRequest {
                private_arena_bytes: 65_536,
                public_scratch_bytes: pages * 65_536,
            })
            .unwrap()
        };
        let error = CallProgramPlan::new_with_policy(&[], &helpers, 9000, 0, policy(1))
            .err()
            .unwrap();
        assert!(error.contains("required_bytes=72000 capacity_bytes=65536"));
        let plan = CallProgramPlan::new_with_policy(&[], &helpers, 9000, 0, policy(2)).unwrap();
        assert_eq!(plan.view().bytes, 72000);
        let error = plan.with_typed_lanes(9000, 60_000, 0, 0).err().unwrap();
        assert!(error.contains("required_bytes=132000 capacity_bytes=131072"));
        let plan = CallProgramPlan::new_with_policy(&[], &helpers, 9000, 0, policy(2)).unwrap();
        let error = plan.with_typed_lanes(9000, 0, 8, 8000).err().unwrap();
        assert!(error.contains("required_bytes=136008 capacity_bytes=131072"));
        assert!(CallProgramPlan::new_with_policy(&[], &helpers, usize::MAX, 0, policy(2)).is_err());
    }
}

#[cfg(test)]
mod y_read_tests {
    use super::*;

    #[test]
    fn y_reads_are_the_cells_operations_read_joined_across_small_gaps() {
        let rows = vec![vec![
            LinearOp::LoadY { dst: 0, index: 10 },
            LinearOp::LoadY { dst: 1, index: 12 },
            LinearOp::LoadY { dst: 4, index: 16 },
            LinearOp::TensorLoad {
                dst_start: 2,
                input: solve::TensorInputKind::Y,
                input_start: 1000,
                count: 50,
                seed_start: None,
                lanes: 1,
            },
            LinearOp::TensorLoad {
                dst_start: 60,
                input: solve::TensorInputKind::P,
                input_start: 5000,
                count: 50,
                seed_start: None,
                lanes: 1,
            },
        ]];
        // Cells 10 and 12 are joined across the one cell between them (a gap of
        // at most two cells costs less than a second copy), 16 is three cells
        // past 13 and stands alone, as does 1000..1050, and P is not Y.
        assert_eq!(
            y_read_bytes(&rows).unwrap(),
            [80..104, 128..136, 8000..8400]
        );
        assert!(y_read_bytes(&[]).unwrap().is_empty());
    }

    #[test]
    fn the_host_y_bound_clips_the_cells_copied_in() {
        let table = solve::SolvePureCallTable::default();
        let helpers = ProgramHelpers::new(&table, &[]).unwrap();
        let rows = [vec![
            LinearOp::LoadY { dst: 0, index: 3 },
            LinearOp::LoadY { dst: 1, index: 9 },
        ]];
        let layout = CallProgramLayout::derive(&rows, &helpers, 10, 0).unwrap();
        // The host Y has four cells; cell 9 is a private derived-output slot.
        let layout = layout.with_typed_lanes(4, 0, 0, 0).unwrap();
        assert_eq!(layout.y_reads.len(), 1);
        assert_eq!((layout.y_reads[0].start, layout.y_reads[0].end), (24, 32));
    }
}

fn insert_call<S: PartialEq + Clone>(
    calls: &mut Vec<(S, CallLayout)>,
    site: &S,
    layout: CallLayout,
) {
    if !calls.iter().any(|(existing, _)| existing == site) {
        calls.push((site.clone(), layout));
    }
}
