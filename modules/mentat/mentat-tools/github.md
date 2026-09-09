# GitHub workflows

Use `gh` directly through `mentat-run-process` for simple actions: viewing a
PR or diff, listing checks, reading reviews, posting a general comment, or
submitting a review decision without inline comments. Do not create wrappers
for individual commands unless they add useful checks or coordination.
Use gh's existing authentication; never print or pass token values.

## Submit an inline review

Use `mentat-github-submit-inline-review` to submit line comments together with
an approved review decision. Only publish comments and a decision that the
user has authorized. The helper does not decide what to say or whether to
approve. Approval does not merge the PR.

Supply:

- `repository`: explicit `owner/repo`.
- `pull-number`: the PR number.
- `reviewed-commit`: the full SHA actually reviewed, not a newly fetched SHA
  that has not been assessed.
- `event`: `APPROVE`, `COMMENT`, or `REQUEST_CHANGES`.
- `comments`: objects with `path`, `line`, `body`, and optional `side`.
  Paths are relative to the repository. Lines must belong to the PR diff.
  `RIGHT` (the default) uses new-file line numbers; `LEFT` uses old-file line
  numbers. These are not diff positions. Each comment targets one line.
- `body`: required for `COMMENT` and `REQUEST_CHANGES`, optional for `APPROVE`.
- `directory`: optional local working directory.

The helper checks that the PR is open and its head matches the reviewed SHA,
then sends one review containing all comments and the decision. If the head
has changed, assess the intervening commits before trying again. Do not
silently replace the reviewed SHA to get past the check.

GitHub has no atomic head-match condition for review creation. The helper
pins the review to the supplied SHA, even if the head moves after the check.
It returns the review state, commit, review link, and inline comment links.

## Failures and cancellation

Never automatically retry a submission after an error, timeout, or
cancellation. GitHub may have accepted it. Inspect the PR's reviews first.
If submission succeeds but reading the comment links fails, the error includes
the review link and state. Do not post the review again.

## API references

The helper uses GitHub's
[create-review endpoint](https://docs.github.com/en/rest/pulls/reviews#create-a-review-for-a-pull-request)
and [gh authentication](https://cli.github.com/manual/gh_auth_login).
For another operation, check its current official documentation before acting.
