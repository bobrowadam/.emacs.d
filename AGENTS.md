# Emacs Configuration Repository

## File Layout
- `init.org` — main config (literate programming, ~2,900 lines).
- `early-init.el` — bootstrap, GC, Elpaca setup.
- `init-generated.el` — auto-generated from init.org; never edit it directly.
- `modules/` — custom elisp modules, loaded via `use-package` with `:ensure nil`.

## Conventions
- Package management via Elpaca (not package.el).
- Custom functions prefixed with `bob/`.
- Tangle `init.org` instead of editing `init-generated.el` directly.
- Reload: `emacsclient --eval "(bob/compile-and-load-init-file)"` after editing.

## Module boundaries
- Keep reusable private Emacs behavior in standalone modules under `/Users/bob/.emacs.d/modules/`. Keep Mentat-specific adapters in `/Users/bob/.emacs.d/modules/mentat/mentat-tools/`; private modules must not depend on Mentat.

## Commits
- Commit changes directly when asked; do not defer commit acceptance to the user.
- Use command-line Git by default. Use Magit only when explicitly requested.
