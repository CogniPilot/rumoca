use super::control_plan::{Helper, Instruction};
use super::*;
use FmiDerivativeStatus as Status;

const Y: u32 = 0;
const P: u32 = 1;
const T: u32 = 2;
const OUT: u32 = 3;
const WORK: u32 = 4;
const CACHE: u32 = 5;
const PRESENT: u32 = 6;
const KEYS: u32 = 7;
const VALID_ARG: u32 = 8;
const INDEX: u32 = 9;
const CACHED: u32 = 10;
const NATIVE: u32 = 11;
const VALID: u32 = 12;

pub(super) fn instructions(
    op: &FmiDerivativeCausalOperation<'_>,
    w: FmiDerivativeWorkspace,
) -> Vec<Instruction> {
    let stat = op.static_schedule().map(|_| Helper::Static);
    let dynamic = op.dynamic_schedule().map(|_| Helper::Dynamic);
    let values = Helper::Values;
    let mut b = Body {
        function: Vec::new(),
        workspace: w,
    };
    b.guards();
    b.address(WORK, 0);
    b.i(0);
    b.i(16);
    b.p(Instruction::MemoryFill);
    b.copy((WORK, w.snapshot()), (Y, 0), w.y_bytes());
    if let Some(stat) = stat {
        b.static_refresh(op, stat);
    }
    if let Some(dynamic) = dynamic {
        b.record(0, Status::DynamicRefresh as u32);
        b.call(dynamic);
        b.child_failure(Status::DynamicRefresh, true);
    }
    b.record(0, Status::Values as u32);
    b.call(values);
    b.child_failure(Status::Values, false);
    // The vector is complete before its first finite check, including the
    // diagnostic failed vector. Earlier NaN cannot hide a later source fault.
    b.copy((OUT, 0), (WORK, w.helper()), w.output_bytes());
    b.range(0, w.output_bytes() / 8, |b| {
        b.indexed(OUT, 0, 8);
        b.p(Instruction::F64Load);
        b.p(Instruction::F64Abs);
        b.p(Instruction::F64Const(f64::MAX.to_bits()));
        b.p(Instruction::F64Le);
        b.p(Instruction::I32Eqz);
        b.p(Instruction::If);
        b.record(0, Status::NonFinite as u32);
        b.address(WORK, 8);
        b.p(Instruction::LocalGet(INDEX));
        b.p(Instruction::I32Store);
        b.ret(Status::NonFinite);
        b.p(Instruction::End);
    });
    b.record(0, Status::Success as u32);
    b.i(Status::Success as u32);
    b.p(Instruction::End);
    b.function
}

struct Body {
    function: Vec<Instruction>,
    workspace: FmiDerivativeWorkspace,
}
impl Body {
    fn p(&mut self, i: Instruction) {
        self.function.push(i);
    }
    fn i(&mut self, n: u32) {
        self.p(Instruction::I32Const(n as i32));
    }
    fn address(&mut self, arg: u32, offset: u32) {
        self.p(Instruction::LocalGet(arg));
        if offset != 0 {
            self.i(offset);
            self.p(Instruction::I32Add);
        }
    }
    fn indexed(&mut self, arg: u32, base: u32, scale: u32) {
        self.address(arg, base);
        self.p(Instruction::LocalGet(INDEX));
        self.i(scale);
        self.p(Instruction::I32Mul);
        self.p(Instruction::I32Add);
    }
    fn end64(&mut self, arg: u32, bytes: u32) {
        self.p(Instruction::LocalGet(arg));
        self.p(Instruction::I64ExtendI32U);
        self.p(Instruction::I64Const(i64::from(bytes)));
        self.p(Instruction::I64Add);
    }
    fn start64(&mut self, arg: u32) {
        self.p(Instruction::LocalGet(arg));
        self.p(Instruction::I64ExtendI32U);
    }
    fn reject_if(&mut self) {
        self.p(Instruction::If);
        self.ret(Status::InvalidBuffers);
        self.p(Instruction::End);
    }
    fn ret(&mut self, status: Status) {
        self.i(status as u32);
        self.p(Instruction::Return);
    }
    fn record(&mut self, offset: u32, value: u32) {
        self.address(WORK, offset);
        self.i(value);
        self.p(Instruction::I32Store);
    }
    fn copy(&mut self, to: (u32, u32), from: (u32, u32), bytes: u32) {
        self.address(to.0, to.1);
        self.address(from.0, from.1);
        self.i(bytes);
        self.p(Instruction::MemoryCopy);
    }
    fn range(&mut self, start: u32, end: u32, body: impl FnOnce(&mut Self)) {
        self.i(start);
        self.p(Instruction::LocalSet(INDEX));
        self.p(Instruction::Block);
        self.p(Instruction::Loop);
        self.p(Instruction::LocalGet(INDEX));
        self.i(end);
        self.p(Instruction::I32GeU);
        self.p(Instruction::BrIf(1));
        body(self);
        self.p(Instruction::LocalGet(INDEX));
        self.i(1);
        self.p(Instruction::I32Add);
        self.p(Instruction::LocalSet(INDEX));
        self.p(Instruction::Br(0));
        self.p(Instruction::End);
        self.p(Instruction::End);
    }
    fn guards(&mut self) {
        let regions = self.workspace.regions();
        for &(arg, bytes) in &regions {
            if bytes == 0 {
                continue;
            }
            self.end64(arg, bytes);
            self.p(Instruction::MemorySize);
            self.p(Instruction::I64ExtendI32U);
            self.p(Instruction::I64Const(16));
            self.p(Instruction::I64Shl);
            self.p(Instruction::I64GtU);
            self.reject_if();
        }
        for (position, &(a, abytes)) in regions
            .iter()
            .enumerate()
            .filter(|(_, (_, bytes))| *bytes != 0)
        {
            for &(b, bbytes) in regions[position + 1..]
                .iter()
                .filter(|(_, bytes)| *bytes != 0)
            {
                self.end64(a, abytes);
                self.start64(b);
                self.p(Instruction::I64GtU);
                self.end64(b, bbytes);
                self.start64(a);
                self.p(Instruction::I64GtU);
                self.p(Instruction::I32And);
                self.reject_if();
            }
        }
        self.p(Instruction::LocalGet(VALID_ARG));
        self.i(1);
        self.p(Instruction::I32GtU);
        self.reject_if();
    }
    fn call(&mut self, function: Helper) {
        self.p(Instruction::LocalGet(Y));
        self.p(Instruction::LocalGet(P));
        self.p(Instruction::LocalGet(T));
        self.address(WORK, self.workspace.helper());
        self.i(0);
        self.p(Instruction::Call(function));
        self.p(Instruction::LocalSet(NATIVE));
    }
    fn child_failure(&mut self, phase: Status, rollback: bool) {
        self.p(Instruction::LocalGet(NATIVE));
        self.p(Instruction::If);
        self.record(0, phase as u32);
        self.address(WORK, 4);
        self.p(Instruction::LocalGet(NATIVE));
        self.p(Instruction::I32Store);
        if rollback {
            self.copy(
                (Y, 0),
                (WORK, self.workspace.snapshot()),
                self.workspace.y_bytes(),
            );
        }
        self.ret(phase);
        self.p(Instruction::End);
    }
    fn static_refresh(&mut self, op: &FmiDerivativeCausalOperation<'_>, function: Helper) {
        self.record(0, Status::StaticRefresh as u32);
        self.p(Instruction::LocalGet(VALID_ARG));
        self.p(Instruction::LocalSet(VALID));
        for &(key, parameter, count) in op.parameter_runs() {
            self.range(0, count as u32, |b| {
                b.indexed(KEYS, key as u32 * 8, 8);
                b.p(Instruction::I64Load);
                b.indexed(P, parameter as u32 * 8, 8);
                b.p(Instruction::I64Load);
                b.p(Instruction::I64Ne);
                b.p(Instruction::If);
                b.i(0);
                b.p(Instruction::LocalSet(VALID));
                b.p(Instruction::End);
            });
        }
        self.p(Instruction::LocalGet(VALID));
        self.p(Instruction::I32Eqz);
        self.p(Instruction::If);
        for &(key, parameter, count) in op.parameter_runs() {
            self.copy(
                (KEYS, key as u32 * 8),
                (P, parameter as u32 * 8),
                count as u32 * 8,
            );
        }
        self.address(PRESENT, 0);
        self.i(0);
        self.i(self.workspace.y_bytes() / 8);
        self.p(Instruction::MemoryFill);
        self.p(Instruction::End);
        self.record(12, 1);
        self.i(1);
        self.p(Instruction::LocalSet(CACHED));
        for run in op.static_targets().intervals() {
            self.range(*run.start() as u32, *run.end() as u32 + 1, |b| {
                b.indexed(PRESENT, 0, 1);
                b.p(Instruction::I32Load8U);
                b.p(Instruction::I32Eqz);
                b.p(Instruction::If);
                b.i(0);
                b.p(Instruction::LocalSet(CACHED));
                b.p(Instruction::End);
            });
        }
        self.p(Instruction::LocalGet(CACHED));
        self.p(Instruction::If);
        for run in op.static_targets().intervals() {
            let start = *run.start() as u32;
            let count = (*run.end() - *run.start() + 1) as u32;
            self.copy((Y, start * 8), (CACHE, start * 8), count * 8);
        }
        self.p(Instruction::Else);
        self.call(function);
        self.child_failure(Status::StaticRefresh, true);
        for run in op.static_targets().intervals() {
            let start = *run.start() as u32;
            let count = (*run.end() - *run.start() + 1) as u32;
            self.copy((CACHE, start * 8), (Y, start * 8), count * 8);
            self.address(PRESENT, start);
            self.i(1);
            self.i(count);
            self.p(Instruction::MemoryFill);
        }
        self.p(Instruction::End);
    }
}
