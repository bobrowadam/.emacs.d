# Eglot TypeScript + Python Setup Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Wire up eglot for TypeScript (typescript-language-server + biome via rass) and Python (pyright) in the bradwell-monorepo, working correctly from both the repo root and git worktrees.

**Architecture:** Install `rass` globally via pip3. Replace the broken `("rass" "ts")` TS server entry in `init.org` with a contact function that detects `node_modules/.bin/biome` by walking up the directory tree (so worktrees work). Remove the noisy pyright auto-install block — pyright is already installed.

**Tech Stack:** Emacs eglot, rassumfrassum (pip3), typescript-language-server (fnm/npm, already installed), pyright-langserver (fnm/npm, already installed), biome (project node_modules, already present)

---

### Task 1: Install rass

**Files:**
- No file changes — shell install only

**Step 1: Install rassumfrassum**

```bash
pip3 install rassumfrassum
```

**Step 2: Verify the binary is on PATH**

```bash
which rass && rass --version
```

Expected output: something like `/opt/homebrew/bin/rass` and a version string.

**Step 3: Verify rass can multiplex two echo processes (smoke test)**

```bash
echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}' | rass -- cat -- cat 2>&1 | head -5
```

Expected: some JSON output (or a clean exit) — not "command not found".

---

### Task 2: Replace TypeScript eglot server entry in init.org

**Files:**
- Modify: `init.org:2006-2008`

**Step 1: Locate the entry to replace**

In `init.org`, lines 2006–2008 currently read:

```elisp
(add-to-list 'eglot-server-programs
             `((js2-mode js-mode js-ts-mode typescript-ts-mode typescript-mode tsx-ts-mode)
               . ("rass" "ts")))
```

**Step 2: Replace it with the dynamic contact function**

Replace those 3 lines with:

```elisp
(add-to-list 'eglot-server-programs
             `((js2-mode js-mode js-ts-mode typescript-ts-mode typescript-mode tsx-ts-mode)
               . ,(lambda (&rest _)
                    (if-let* ((nm-dir (locate-dominating-file default-directory "node_modules"))
                              (biome  (expand-file-name "node_modules/.bin/biome" nm-dir))
                              ((file-executable-p biome)))
                        (list "rass" "--" "typescript-language-server" "--stdio"
                              "--" biome "lsp-proxy")
                      '("typescript-language-server" "--stdio")))))
```

**Step 3: Verify the org block still has matching parentheses**

After editing, check that the `use-package eglot` block closes correctly — the `#+end_src` at line 2045 should still be present with balanced parens inside the block.

---

### Task 3: Remove pyright auto-install block from init.org

**Files:**
- Modify: `init.org:2019-2024`

**Step 1: Locate the block to remove**

Lines 2019–2024 currently read:

```elisp
(unless (executable-find "pyright-langserver")
  (progn
    (print "Installing pyright-langserver for python eglot support")
    (async-shell-command "npm install -g pyright"
                         "*pyright-installation-stdout*"
                         "*pyright-installation-error*")))
```

**Step 2: Delete those 6 lines**

Leave line 2025 intact:

```elisp
(add-to-list 'eglot-server-programs '((python-ts-mode) . ("pyright-langserver" "--stdio")))
```

**Step 3: Commit both init.org changes**

```bash
git add init.org
git commit -m "Fix eglot TS server to use rass+biome, remove pyright auto-install"
```

---

### Task 4: Verify TypeScript eglot in the monorepo

**Step 1: Open a TypeScript file in the monorepo**

```bash
emacsclient ~/source/bradwell-monorepo/services/chats/src/main.ts
```

(or any `.ts` file)

**Step 2: Start eglot and confirm the server**

In Emacs, run `M-x eglot` (or it should auto-start via the hook).

Then run:

```
M-x eglot
```

Check the mode line — it should show `[eglot:rass]` or similar, not an error.

**Step 3: Confirm rass launched both servers**

```
M-x eglot-events-buffer
```

You should see initialization messages from both `typescript-language-server` and `biome lsp-proxy` in the events log.

**Step 4: Verify biome diagnostics appear**

Open a TypeScript file with a known lint issue, or temporarily introduce one (e.g., an unused import). Flymake should show a biome diagnostic within a few seconds.

**Step 5: Verify formatting works**

Run `C-c C-f` (`eglot-format`) in a TypeScript buffer. The file should be formatted by biome (not prettier).

---

### Task 5: Verify TypeScript eglot from a worktree

**Step 1: Create a test worktree**

```bash
cd ~/source/bradwell-monorepo
git worktree add .claude/worktrees/eglot-test-wt HEAD
```

**Step 2: Open a TypeScript file from inside the worktree**

```bash
emacsclient ~/.claude/worktrees/eglot-test-wt/services/chats/src/main.ts
```

**Step 3: Start eglot and confirm biome is still picked up**

Run `M-x eglot` and check that `rass` is used (not the fallback `typescript-language-server` alone). The `locate-dominating-file` call should traverse up from the worktree dir and find `node_modules` in the main repo.

**Step 4: Clean up the test worktree**

```bash
git worktree remove ~/.claude/worktrees/eglot-test-wt
```

---

### Task 6: Verify Python eglot

**Step 1: Open a Python file in the monorepo**

```bash
emacsclient ~/source/bradwell-monorepo/services/research-py/research_py/main.py
```

(or any `.py` file)

**Step 2: Confirm pyright starts**

Run `M-x eglot`. The mode line should show `[eglot:pyright-langserver]` or similar.

**Step 3: Verify type-checking diagnostics**

Introduce a deliberate type error (e.g., assign `x: int = "hello"`), save, and confirm a flymake error appears from pyright.

**Step 4: Undo the test change**

Revert with `C-x u` or `git checkout`.

---

### Task 7: Final commit and cleanup

**Step 1: Confirm init.el was regenerated**

The PostToolUse hook should auto-regenerate `init.el` from `init.org` after each edit. Verify:

```bash
ls -la ~/.emacs.d/init.el
```

The modification time should be recent.

**Step 2: Final git status check**

```bash
git status
git log --oneline -5
```

Ensure only intended files were changed.
