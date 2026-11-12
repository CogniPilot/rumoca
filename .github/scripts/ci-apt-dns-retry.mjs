const aptSteps = new Set([
  'Install system dependencies',
  'Install system dependencies (Linux)',
  'Install Xvfb and system dependencies',
  'Install closure archive tooling',
  'Install musl tools (Linux musl)',
  'Run ./.github/actions/apt-install',
]);

function eligibleRun(run, repository) {
  return Number.isSafeInteger(run?.id) && run.id > 0 &&
    Number.isSafeInteger(run.workflow_id) && run.workflow_id > 0 &&
    run.repository?.id === repository.id && run.name === 'CI' &&
    run.path?.split('@')[0] === '.github/workflows/ci.yml' &&
    run.status === 'completed' && run.conclusion === 'failure' &&
    run.run_attempt === 1 && /^[a-f0-9]{40}$/.test(run.head_sha) &&
    (run.event === 'pull_request' ||
      (run.event === 'push' && run.head_branch === repository.default_branch &&
        run.head_repository?.id === repository.id));
}

function failedSetupJobs(run, jobs) {
  if (!jobs.length || jobs.length > 500 || jobs.some(job =>
    job.run_id !== run.id || job.run_attempt !== 1 || job.head_sha !== run.head_sha ||
    job.status !== 'completed' ||
    !['success', 'skipped', 'failure'].includes(job.conclusion))) return [];
  const failed = jobs.filter(job => job.conclusion === 'failure');
  return failed.every(job => {
    const steps = job.steps?.filter(step => step.conclusion === 'failure') ?? [];
    return aptSteps.has(steps[0]?.name) && steps.slice(1).every(step =>
      step.name === 'Upload GNU static-PIE qualification (Linux)');
  }) ? failed : [];
}

async function currentPullRequest(github, repo, run) {
  if (run.event !== 'pull_request') return true;
  const owner = run.head_repository?.owner?.login;
  if (!owner || !run.head_branch) return false;
  const { data } = await github.rest.pulls.list({ ...repo, state: 'open',
    head: `${owner}:${run.head_branch}`, per_page: 100 });
  return data.some(pr => pr.head.sha === run.head_sha && !pr.draft);
}

async function markedDnsFailures(github, repo, jobs) {
  for (const job of jobs) {
    const checkRun = Number(job.check_run_url?.split('/').at(-1));
    if (!Number.isSafeInteger(checkRun) || checkRun < 1) return false;
    const { data } = await github.rest.checks.listAnnotations({ ...repo,
      check_run_id: checkRun, per_page: 100 });
    if (!data.some(item => item.annotation_level === 'failure' &&
      item.title === 'APT mirror DNS failure')) return false;
  }
  return true;
}

// Runs only trusted default-branch code; job annotations are data, never code.
// Rerunning preserves the original CI source and gates; it grants no pass.
export async function retryAptDnsRun({ github, context, core,
  wait = ms => new Promise(resolve => setTimeout(resolve, ms)) }) {
  const run_id = context.payload.workflow_run.id;
  const repository = context.payload.repository;
  const repo = context.repo;
  if (!Number.isSafeInteger(run_id) || run_id < 1) return false;
  const readRun = async () => (await github.rest.actions.getWorkflowRun({ ...repo, run_id })).data;
  const run = await readRun();
  if (run.id !== run_id || !eligibleRun(run, repository) ||
    !await currentPullRequest(github, repo, run)) return false;
  const jobs = await github.paginate(github.rest.actions.listJobsForWorkflowRunAttempt,
    { ...repo, run_id, attempt_number: 1, per_page: 100 });
  const failed = failedSetupJobs(run, jobs);
  if (!failed.length || !await markedDnsFailures(github, repo, failed)) return false;
  core.info(`Run ${run_id}: only marked apt DNS setup failures; waiting two minutes.`);
  await wait(120_000);
  const current = await readRun();
  if (current.id !== run_id || current.workflow_id !== run.workflow_id ||
    !eligibleRun(current, repository) || current.head_sha !== run.head_sha ||
    !await currentPullRequest(github, repo, current)) return false;
  await github.request('POST /repos/{owner}/{repo}/actions/runs/{run_id}/rerun-failed-jobs',
    { ...repo, run_id });
  core.notice(`Requested one failed-job retry of CI run ${run_id}; all original gates still apply.`);
  return true;
}
