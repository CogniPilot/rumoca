import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';

const batchId = 'climamind-rumoca-broken-main-2026-07';
const label = 'recovery-exact-head-ci';
const shaPattern = /^[0-9a-f]{40}$/;
const utcTimestampPattern = /^(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2}):(\d{2})(?:\.\d+)?Z$/;

const requireValue = (condition, message) => {
  if (!condition) throw new Error(message);
};

const bodyFields = (body) => {
  const wanted = new Set([
    'recovery_batch_id',
    'recovery_head_sha',
    'recovery_expires_at',
  ]);
  const fields = new Map();
  for (const line of String(body ?? '').split(/\r?\n/)) {
    const match = line.match(/^\s*(recovery_[a-z_]+):\s*(\S+)\s*$/);
    if (!match || !wanted.has(match[1])) continue;
    requireValue(!fields.has(match[1]), `duplicate ${match[1]}`);
    fields.set(match[1], match[2]);
  }
  for (const field of wanted) requireValue(fields.has(field), `missing ${field}`);
  return fields;
};

const utcTimestampMillis = (value) => {
  const match = String(value).match(utcTimestampPattern);
  requireValue(match, 'recovery_expires_at must be a valid RFC 3339 UTC timestamp');
  const [, year, month, day, hour, minute, second] = match.map(Number);
  const calendar = new Date(Date.UTC(year, month - 1, day, hour, minute, second));
  requireValue(
    calendar.getUTCFullYear() === year
      && calendar.getUTCMonth() === month - 1
      && calendar.getUTCDate() === day
      && calendar.getUTCHours() === hour
      && calendar.getUTCMinutes() === minute
      && calendar.getUTCSeconds() === second,
    'recovery_expires_at must be a valid RFC 3339 UTC timestamp',
  );
  const millis = Date.parse(value);
  requireValue(!Number.isNaN(millis), 'recovery_expires_at must be a valid RFC 3339 UTC timestamp');
  return millis;
};

export const validateRecoveryContract = ({
  event,
  eventName,
  githubSha,
  repository,
  selectedSha,
  now = new Date(),
}) => {
  requireValue(shaPattern.test(selectedSha), 'selected SHA must be a lowercase 40-hex commit');
  requireValue(shaPattern.test(githubSha), 'GITHUB_SHA must be a lowercase 40-hex commit');
  if (eventName !== 'pull_request') {
    requireValue(selectedSha === githubSha, 'normal CI must select GITHUB_SHA');
    return { mode: 'normal', sha: selectedSha };
  }
  const pr = event?.pull_request;
  requireValue(pr, 'pull_request payload data is required');
  const labels = Array.isArray(pr.labels) ? pr.labels : [];
  const recoveryRequested = String(pr.head?.ref ?? '').startsWith('integration/recovery-')
    || labels.some((item) => item?.name === label)
    || /^\s*recovery_[a-z_]+:/m.test(String(pr.body ?? ''));
  if (!recoveryRequested) {
    requireValue(selectedSha === githubSha, 'normal CI must select GITHUB_SHA');
    return { mode: 'normal', sha: selectedSha };
  }

  requireValue(event?.repository?.full_name === repository, 'event repository mismatch');
  requireValue(
    pr.head?.repo?.full_name === repository && pr.base?.repo?.full_name === repository,
    'recovery head and base must use the same repository',
  );
  requireValue(pr.draft === true, 'recovery pull request must remain Draft');
  requireValue(
    String(pr.head?.ref ?? '').startsWith('integration/recovery-'),
    'recovery branch prefix must be integration/recovery-',
  );
  requireValue(event?.action === 'labeled', 'recovery must be authorized by a labeled event');
  requireValue(event?.label?.name === label, 'recovery labeled event must apply recovery-exact-head-ci');
  requireValue(
    labels.some((item) => item?.name === label),
    `recovery pull request requires the ${label} label`,
  );
  const fields = bodyFields(pr.body);
  requireValue(fields.get('recovery_batch_id') === batchId, 'recovery batch id mismatch');
  requireValue(
    fields.get('recovery_head_sha') === pr.head?.sha,
    'body-recorded head must equal payload head SHA',
  );
  const expiresAt = fields.get('recovery_expires_at');
  const expiryMillis = utcTimestampMillis(expiresAt);
  requireValue(expiryMillis > now.getTime(), 'recovery authorization is expired');
  requireValue(pr.head?.sha === selectedSha, 'selected SHA must equal payload head SHA');
  return { mode: 'recovery', sha: selectedSha };
};

const main = () => {
  const eventPath = process.env.GITHUB_EVENT_PATH;
  requireValue(eventPath, 'GITHUB_EVENT_PATH is required');
  const event = JSON.parse(readFileSync(eventPath, 'utf8'));
  const result = validateRecoveryContract({
    event,
    eventName: process.env.GITHUB_EVENT_NAME,
    githubSha: process.env.GITHUB_SHA,
    repository: process.env.GITHUB_REPOSITORY,
    selectedSha: process.env.RUMOCA_CI_HEAD_SHA,
  });
  console.log(`${result.mode} CI head verified: ${result.sha}`);
};

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  try {
    main();
  } catch (error) {
    console.error(`recovery-exact-head-contract: ${error.message}`);
    process.exitCode = 1;
  }
}
