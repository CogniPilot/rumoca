//! Ordered history transfers shared by event entry and event commit.
use crate::{PreParamSource, SolveLayout};

#[derive(Debug, PartialEq, Eq, serde::Serialize)]
pub(crate) struct HistoryRun {
    src: usize,
    dst: usize,
    count: usize,
    from_y: bool,
}

impl SolveLayout {
    pub(crate) fn history_runs(&self) -> Vec<HistoryRun> {
        let mut runs: Vec<HistoryRun> = Vec::new();
        for binding in &self.pre_param_bindings {
            let (from_y, src) = match binding.source {
                PreParamSource::Y { index } => (true, index),
                PreParamSource::P { index } => (false, index),
            };
            let dst = binding.dest_p_index;
            // An in-place forward copy may read an earlier destination. Split
            // before that dependency so each run observes the ordered scalar semantics.
            if let Some(run) = runs.last_mut()
                && run.from_y == from_y
                && run.dst.checked_add(run.count) == Some(dst)
                && run.src.checked_add(run.count) == Some(src)
                && (from_y || run.dst <= run.src || run.count < run.dst - run.src)
            {
                run.count += 1;
            } else {
                runs.push(HistoryRun {
                    src,
                    dst,
                    count: 1,
                    from_y,
                });
            }
        }
        runs
    }
}

#[test]
fn overlapping_history_runs_replay_the_ordered_scalar_copies() {
    for src in 0..4 {
        for dst in 0..4 {
            for from_y in [false, true] {
                assert_runs_replay_scalar_copies(src, dst, from_y);
            }
        }
    }
}

/// Four bindings from `src` to `dst`: the runs must leave what the ordered
/// scalar copies leave.
#[cfg(test)]
fn assert_runs_replay_scalar_copies(src: usize, dst: usize, from_y: bool) {
    let mut layout = SolveLayout::default();
    let y = [10, 11, 12, 13, 14, 15, 16];
    let mut expected = [0, 1, 2, 3, 4, 5, 6];
    let mut actual = expected;
    for lane in 0..4 {
        let index = src + lane;
        let source = if from_y {
            PreParamSource::Y { index }
        } else {
            PreParamSource::P { index }
        };
        layout.pre_param_bindings.push(crate::PreParamBinding {
            dest_p_index: dst + lane,
            source,
            clock_schedule: None,
        });
        expected[dst + lane] = if from_y { y[index] } else { expected[index] };
    }
    for run in layout.history_runs() {
        let range = run.src..run.src + run.count;
        if run.from_y {
            actual[run.dst..run.dst + run.count].copy_from_slice(&y[range]);
        } else {
            actual.copy_within(range, run.dst);
        }
    }
    assert_eq!(actual, expected, "src={src} dst={dst} from_y={from_y}");
}
