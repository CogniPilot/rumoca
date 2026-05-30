//! Bounded bulk simulation helpers.

use std::sync::{Arc, Mutex, mpsc};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BulkSimulationPlan {
    pub worker_threads: usize,
    pub queue_bound: usize,
}

impl BulkSimulationPlan {
    pub fn new(worker_threads: usize, queue_bound: usize) -> Self {
        Self {
            worker_threads: worker_threads.max(1),
            queue_bound: queue_bound.max(1),
        }
    }
}

#[derive(Debug)]
pub struct PreparedSimulationJob<T> {
    pub index: usize,
    pub payload: T,
}

pub type PreparedSimulationSender<T> = mpsc::SyncSender<PreparedSimulationJob<T>>;

pub fn run_prepared_simulation_pipeline<P, O, Produce, ProduceOutput, Run>(
    plan: BulkSimulationPlan,
    produce: Produce,
    run: Run,
) -> (ProduceOutput, Vec<(usize, O)>)
where
    P: Send,
    O: Send,
    Produce: FnOnce(PreparedSimulationSender<P>) -> ProduceOutput,
    Run: Fn(P) -> O + Sync,
{
    let (job_tx, job_rx) = mpsc::sync_channel::<PreparedSimulationJob<P>>(plan.queue_bound);
    let job_rx = Arc::new(Mutex::new(job_rx));
    let (result_tx, result_rx) = mpsc::channel::<(usize, O)>();

    let produce_output = std::thread::scope(|scope| {
        for _ in 0..plan.worker_threads {
            let job_rx = Arc::clone(&job_rx);
            let result_tx = result_tx.clone();
            let run = &run;
            scope.spawn(move || worker_loop(job_rx, result_tx, run));
        }
        drop(result_tx);
        produce(job_tx)
    });

    let mut results: Vec<_> = result_rx.into_iter().collect();
    results.sort_by_key(|(index, _)| *index);
    (produce_output, results)
}

fn worker_loop<P, O, Run>(
    job_rx: Arc<Mutex<mpsc::Receiver<PreparedSimulationJob<P>>>>,
    result_tx: mpsc::Sender<(usize, O)>,
    run: &Run,
) where
    P: Send,
    O: Send,
    Run: Fn(P) -> O + Sync,
{
    loop {
        let job = {
            let Ok(job_rx) = job_rx.lock() else {
                break;
            };
            job_rx.recv()
        };
        let Ok(job) = job else {
            break;
        };
        let result = run(job.payload);
        let _ = result_tx.send((job.index, result));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bulk_simulation_pipeline_preserves_result_order() {
        let plan = BulkSimulationPlan::new(2, 1);
        let (produced, results) = run_prepared_simulation_pipeline(
            plan,
            |tx| {
                tx.send(PreparedSimulationJob {
                    index: 1,
                    payload: 10,
                })
                .expect("send job 1");
                tx.send(PreparedSimulationJob {
                    index: 0,
                    payload: 20,
                })
                .expect("send job 0");
                2
            },
            |value| value + 1,
        );

        assert_eq!(produced, 2);
        assert_eq!(results, vec![(0, 21), (1, 11)]);
    }

    #[test]
    fn bulk_simulation_workers_run_jobs_concurrently() {
        use std::sync::atomic::{AtomicUsize, Ordering};
        use std::time::Duration;

        let active = Arc::new(AtomicUsize::new(0));
        let max_active = Arc::new(AtomicUsize::new(0));
        let plan = BulkSimulationPlan::new(2, 2);
        let active_for_run = Arc::clone(&active);
        let max_active_for_run = Arc::clone(&max_active);

        let (_produced, results) = run_prepared_simulation_pipeline(
            plan,
            |tx| {
                for index in 0..2 {
                    tx.send(PreparedSimulationJob {
                        index,
                        payload: index,
                    })
                    .expect("send simulation job");
                }
            },
            move |value| {
                let now_active = active_for_run.fetch_add(1, Ordering::SeqCst) + 1;
                max_active_for_run.fetch_max(now_active, Ordering::SeqCst);
                std::thread::sleep(Duration::from_millis(50));
                active_for_run.fetch_sub(1, Ordering::SeqCst);
                value
            },
        );

        assert_eq!(results, vec![(0, 0), (1, 1)]);
        assert_eq!(max_active.load(Ordering::SeqCst), 2);
    }
}
