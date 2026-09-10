'use strict';

const MARKER = '<!-- rumoca-msl-quality-summary -->';

module.exports = async function upsertMslPrComment({ github, context, body }) {
  if (!body.includes(MARKER)) {
    throw new Error(`Refusing to publish MSL PR comment: missing ${MARKER}`);
  }

  const { owner, repo } = context.repo;
  const issue_number = context.payload.pull_request.number;
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
