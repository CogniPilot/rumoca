//! Bounded disjoint whole-program and complete call tuple storage.
use super::memo::Memo;
use super::*;
use crate::typed_call::program::CallLayout;

pub(in crate::emit) struct CallProgramPlan {
    pub bytes: u32,
    pub work_bytes: u32,
    pub input: u32,
    pub output: u32,
    pub scratch: u32,
    pub saved_y: u32,
    pub status: u32,
    pub integer: u32,
    pub helpers: Vec<(solve::SolvePureCallSite, CallLayout)>,
    pub(in crate::emit::compute::call_program) memos: Vec<Memo>,
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
        conditional::visit_operations(rows, |op| {
            let LinearOp::PureCall { site, .. } = op else {
                return Ok(());
            };
            let layout = helpers.layout(site).map_err(|e| e.to_string())?;
            input = input.max(layout.input);
            output = output.max(layout.output);
            scratch = scratch.max(layout.scratch);
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
        let memos = super::memo::derive(rows, helpers, &mut bytes)?;
        Ok(Self {
            bytes,
            work_bytes,
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

    pub(super) fn call(&self, site: &solve::SolvePureCallSite) -> Result<CallLayout, String> {
        self.helpers
            .iter()
            .find(|(issued, _)| issued == site)
            .map(|(_, layout)| *layout)
            .ok_or_else(|| "native call differs from the issued model table".into())
    }
}
