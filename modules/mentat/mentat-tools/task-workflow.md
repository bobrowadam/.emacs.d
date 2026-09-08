# Bradwell task workflow

Use this private workflow when Bob asks to start a task in
`~/source/gist/bradwell-monorepo`.

## Confirm the Linear input

An existing issue is optional.

- For an existing issue, obtain its identifier, such as `BRA-588`.
- For a new issue, propose a short title and description and identify the Linear
  project. Confirm all three with Bob before creating it.

Do not route ordinary tasks through issue-decomposition workflows.

## Start the task

Call `mentat-task-start` once with either the existing identifier or the confirmed
new-issue fields. The operation performs the deterministic workflow:

1. Fetch the existing issue, or resolve the `BRA` team and confirmed project and
   create the issue assigned to the current Linear user.
2. Fetch `origin/main` in the main Bradwell checkout.
3. Derive a compact branch name from the issue identifier and at most three words
   from its title.
4. Create the branch from `origin/main` and put its worktree directly under the
   main checkout's `.worktrees` directory. Branch slashes are flattened, so this
   workflow never nests worktrees.
5. Register the project, link the shared `.venv` and `pyrightconfig.json` when
   available, and start `npm install && npm run build:services-common` in the
   background. Graphify also updates when its executable is installed.
6. Create an undisplayed native Mentat session rooted in the worktree. Its handoff
   contains the issue, branch, worktree, and setup output, and instructs the new
   session to wait for further user instructions.

The result confirms issue and Git creation plus session handoff. It does not mean
background dependency setup has completed. Use the returned setup process and
buffer names when reporting its state.

## Linear status

After task startup succeeds, ask Bob whether to move the issue to **In Progress**.
Only after confirmation, call `mentat-linear-set-state` with the issue identifier
and the returned `in-progress-state` ID. Do not change status as part of task
creation.

## Clean up a worktree

Call `mentat-worktree-clean` with the exact Bradwell worktree directory and
`check` before cleanup. If the branch does not exist on origin, the operation
removes the clean worktree and local branch without asking about the remote.

If the result contains `confirmation-required`, no files or branches changed.
Ask Bob whether to delete the existing origin branch, then call the tool again
with `delete` or `keep` according to the answer. Never infer this choice.

The tool refuses the primary checkout and relies on Git's normal clean-worktree
check. It does not force removal or discard uncommitted changes.

## Failure boundaries

Never delete, reset, overwrite, or silently reuse an existing branch or worktree.
If task startup fails after issue or worktree creation, inspect the returned or
reported state before retrying. Cancellation does not undo completed external
changes.

Starting a task does not authorise comments, commits, pushes, pull requests, or
other Linear mutations.
