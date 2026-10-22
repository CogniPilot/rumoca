'use strict';

const MARKER = '<!-- rumoca-msl-quality-summary -->';

// `issueNumber` defaults to the triggering pull request; a `workflow_run`
// publisher passes the number it resolved for the completed run's head.
module.exports = async function upsertMslPrComment({
  github,
  context,
  body,
  issueNumber = context.payload.pull_request?.number,
}) {
  if (!body.includes(MARKER)) {
    throw new Error(`Refusing to publish MSL PR comment: missing ${MARKER}`);
  }
  if (!Number.isInteger(issueNumber)) {
    throw new Error('Refusing to publish MSL PR comment: no pull request number');
  }

  const { owner, repo } = context.repo;
  const issue_number = issueNumber;
  const comments = await github.paginate(github.rest.issues.listComments, {
    owner,
    repo,
    issue_number,
    per_page: 100,
  });
  const previous = comments.find(comment =>
    comment.user?.type === 'Bot' && comment.body?.includes(MARKER)
  );
  if (previous) {
    await github.rest.issues.updateComment({
      owner,
      repo,
      comment_id: previous.id,
      body,
    });
    return;
  }
  await github.rest.issues.createComment({ owner, repo, issue_number, body });
};
