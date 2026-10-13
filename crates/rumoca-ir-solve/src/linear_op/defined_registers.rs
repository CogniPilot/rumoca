//! The set of registers an earlier write defined, as sorted disjoint runs.
//!
//! Register-flow proofs ask "is every register of this range defined" and
//! record "this destination range is now defined" once per operation. Tensor
//! operations span up to 10^8 registers, so the set stores coalesced runs and
//! both questions cost a logarithmic search, independent of the range length.

use std::collections::BTreeMap;

use super::Reg;

/// Registers marked defined, as non-empty runs that are neither overlapping
/// nor adjacent (`start -> exclusive end`).
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct DefinedRegisters {
    runs: BTreeMap<usize, usize>,
}

impl DefinedRegisters {
    /// The registers `flags` marks true.
    pub(crate) fn from_flags(flags: &[bool]) -> Self {
        let mut defined = Self::default();
        let mut index = 0;
        while index < flags.len() {
            if !flags[index] {
                index += 1;
                continue;
            }
            let start = index;
            while index < flags.len() && flags[index] {
                index += 1;
            }
            defined.runs.insert(start, index);
        }
        defined
    }

    pub(crate) fn contains(&self, register: Reg) -> bool {
        self.first_undefined(register, 1).is_none()
    }

    /// The lowest register of `start..start + len` that is not defined.
    pub(crate) fn first_undefined(&self, start: Reg, len: usize) -> Option<Reg> {
        let start = start as usize;
        let end = start.saturating_add(len);
        if start >= end {
            return None;
        }
        let covered_to = match self.runs.range(..=start).next_back() {
            Some((_, &run_end)) if run_end > start => run_end,
            _ => return Some(start as Reg),
        };
        // Runs are coalesced, so the register after a run's end is undefined.
        (covered_to < end).then_some(covered_to as Reg)
    }

    /// The lowest of the `count` registers `start`, `start + stride`, ... that
    /// is not defined; the caller has proven every element lies in the register
    /// space. Each run covers a block of consecutive elements at once,
    /// so the search costs one lookup per run crossed, not one per element.
    pub(crate) fn first_undefined_strided(
        &self,
        start: Reg,
        count: usize,
        stride: usize,
    ) -> Option<Reg> {
        let mut ordinal = 0;
        while ordinal < count {
            let register = start as usize + ordinal * stride;
            let covered_to = match self.runs.range(..=register).next_back() {
                Some((_, &run_end)) if run_end > register => run_end,
                _ => return Some(register as Reg),
            };
            // Elements `register`, `register + stride`, ... below `covered_to`.
            ordinal += (covered_to - 1 - register) / stride.max(1) + 1;
        }
        None
    }

    /// The lowest defined register of `start..start + len`.
    pub(crate) fn first_defined(&self, start: Reg, len: usize) -> Option<Reg> {
        let start = start as usize;
        let end = start.saturating_add(len);
        if let Some((_, &run_end)) = self.runs.range(..=start).next_back()
            && run_end > start
        {
            return (start < end).then_some(start as Reg);
        }
        let (&run_start, _) = self.runs.range(start..end).next()?;
        Some(run_start as Reg)
    }

    /// Mark `start..start + len` defined.
    pub(crate) fn insert(&mut self, start: Reg, len: usize) {
        let mut new_start = start as usize;
        let mut new_end = new_start.saturating_add(len);
        if new_start >= new_end {
            return;
        }
        // Registers are mostly defined in ascending order, past every run: extend
        // the last run or open a new one without searching.
        match self.runs.last_entry() {
            None => {
                self.runs.insert(new_start, new_end);
                return;
            }
            Some(mut last) if new_start >= *last.get() => {
                if new_start == *last.get() {
                    *last.get_mut() = new_end;
                } else {
                    self.runs.insert(new_start, new_end);
                }
                return;
            }
            Some(_) => {}
        }
        if let Some((&run_start, &run_end)) = self.runs.range(..=new_start).next_back()
            && run_end >= new_start
        {
            new_start = run_start;
            new_end = new_end.max(run_end);
        }
        let absorbed: Vec<usize> = self
            .runs
            .range(new_start..=new_end)
            .map(|(&run_start, _)| run_start)
            .collect();
        for run_start in absorbed {
            if let Some(run_end) = self.runs.remove(&run_start) {
                new_end = new_end.max(run_end);
            }
        }
        self.runs.insert(new_start, new_end);
    }

    /// Mark `start..start + len` undefined.
    pub(crate) fn remove(&mut self, start: Reg, len: usize) {
        let cut_start = start as usize;
        let cut_end = cut_start.saturating_add(len);
        if cut_start >= cut_end {
            return;
        }
        let overlapping: Vec<(usize, usize)> = self
            .runs
            .range(..cut_end)
            .rev()
            .take_while(|&(_, &run_end)| run_end > cut_start)
            .map(|(&run_start, &run_end)| (run_start, run_end))
            .collect();
        for (run_start, run_end) in overlapping {
            self.runs.remove(&run_start);
            if run_start < cut_start {
                self.runs.insert(run_start, cut_start);
            }
            if run_end > cut_end {
                self.runs.insert(cut_end, run_end);
            }
        }
    }

    /// Define or undefine `start..start + len`.
    pub(crate) fn set(&mut self, start: Reg, len: usize, defined: bool) {
        if defined {
            self.insert(start, len);
        } else {
            self.remove(start, len);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const SPAN: usize = 96;

    /// Deterministic xorshift stream, so failures replay.
    struct Stream(u64);

    impl Stream {
        fn next(&mut self, bound: usize) -> usize {
            self.0 ^= self.0 << 13;
            self.0 ^= self.0 >> 7;
            self.0 ^= self.0 << 17;
            (self.0 % bound as u64) as usize
        }
    }

    fn first_undefined_in_flags(flags: &[bool], start: usize, len: usize) -> Option<Reg> {
        (start..start + len)
            .find(|&register| !flags.get(register).copied().unwrap_or(false))
            .map(|register| register as Reg)
    }

    fn assert_runs_canonical(defined: &DefinedRegisters) {
        let mut previous_end = None;
        for (&start, &end) in &defined.runs {
            assert!(start < end, "runs are non-empty");
            if let Some(previous) = previous_end {
                assert!(previous < start, "runs are disjoint and not adjacent");
            }
            previous_end = Some(end);
        }
    }

    /// One random step applied to both the interval set and the flag oracle.
    fn step(stream: &mut Stream, defined: &mut DefinedRegisters, flags: &mut [bool]) {
        let start = stream.next(SPAN);
        let len = stream.next(24);
        match stream.next(4) {
            0 => {
                defined.set(start as Reg, len, false);
                flags[start..start + len].fill(false);
            }
            1 => {
                defined.set(start as Reg, len, true);
                flags[start..start + len].fill(true);
            }
            _ => check_queries(defined, flags, start, len),
        }
        assert_runs_canonical(defined);
    }

    fn check_queries(defined: &DefinedRegisters, flags: &[bool], start: usize, len: usize) {
        assert_eq!(
            defined.first_undefined(start as Reg, len),
            first_undefined_in_flags(flags, start, len),
            "range {start}+{len}"
        );
        let first_defined = (start..start + len).find(|&register| flags[register]);
        assert_eq!(
            defined.first_defined(start as Reg, len),
            first_defined.map(|register| register as Reg)
        );
        for stride in 1..=4 {
            let strided = (0..len)
                .map(|ordinal| start + ordinal * stride)
                .find(|&register| !flags.get(register).copied().unwrap_or(false));
            assert_eq!(
                defined.first_undefined_strided(start as Reg, len, stride),
                strided.map(|register| register as Reg),
                "range {start}+{len} stride {stride}"
            );
        }
    }

    #[test]
    fn interval_set_matches_per_register_flags_on_random_mark_and_require_sequences() {
        for seed in 1..=64u64 {
            let mut stream = Stream(seed.wrapping_mul(0x9e37_79b9_7f4a_7c15) | 1);
            let mut flags = vec![false; SPAN + 40];
            let mut defined = DefinedRegisters::default();
            for _ in 0..200 {
                step(&mut stream, &mut defined, &mut flags);
            }
            let expected = flags.iter().enumerate().filter(|(_, set)| **set);
            for (register, _) in expected {
                assert!(defined.contains(register as Reg));
            }
            assert_eq!(defined, DefinedRegisters::from_flags(&flags));
        }
    }

    #[test]
    fn ascending_definitions_extend_one_run_and_leave_gaps_between_runs() {
        let mut defined = DefinedRegisters::default();
        for register in 0..50 {
            defined.insert(register, 1);
        }
        defined.insert(50, 10);
        assert_eq!(defined.runs.len(), 1);
        assert_eq!(defined.first_undefined(0, 60), None);
        defined.insert(70, 5);
        assert_eq!(defined.runs.len(), 2);
        assert_eq!(defined.first_undefined(0, 80), Some(60));
        defined.insert(60, 10);
        assert_eq!(defined.runs.len(), 1);
        assert_eq!(defined.first_undefined(0, 75), None);
    }

    #[test]
    fn huge_ranges_cost_one_run_and_name_the_first_undefined_register() {
        let mut defined = DefinedRegisters::default();
        defined.insert(0, 100_000_000);
        defined.insert(100_000_000, 5);
        assert_eq!(defined.runs.len(), 1);
        assert_eq!(defined.first_undefined(10, 99_999_995), None);
        assert_eq!(defined.first_undefined(10, 99_999_996), Some(100_000_005));
        defined.remove(7, 1);
        assert_eq!(defined.first_undefined(0, 100), Some(7));
    }
}
