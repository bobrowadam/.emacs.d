# Emacs Configuration Repository

## File Layout
- `init.org` — main config (literate programming, ~2,900 lines).
- `early-init.el` — bootstrap, GC, Elpaca setup.
- `init-generated.el` — auto-generated from init.org.
- `modules/` — custom elisp modules, loaded via `use-package` with `:ensure nil`.

## Conventions
- Package management via Elpaca (not package.el).
- Custom functions prefixed with `bob/`.
- Reload: `emacsclient --eval "(bob/compile-and-load-init-file)"` after editing.
