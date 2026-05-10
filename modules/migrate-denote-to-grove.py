#!/usr/bin/env python3
"""Migrate Denote notes into a Grove vault.

Denote layout:
    20240104T120451--inbox__project.org
    #+title:      inbox
    #+date:       [2024-01-04 ...]
    #+filetags:   :project:
    #+identifier: 20240104T120451
    #+signature:

Grove layout:
    inbox.org   (filename = title slug; collisions get -N suffix)
    #+title:    inbox
    #+date:     [2024-01-04 ...]
    #+filetags: :project:
    (denote-specific frontmatter stripped)

Also rewrites `[[denote:IDENTIFIER]]` links into Grove wikilinks
`[[note title]]`, looked up via the identifier→title map built in pass 1.

Usage:
    # Preview what would happen (no writes):
    python migrate-denote-to-grove.py

    # Actually copy files:
    python migrate-denote-to-grove.py --apply
"""

from __future__ import annotations

import argparse
import re
import sys
from dataclasses import dataclass
from pathlib import Path

# --- Configuration -----------------------------------------------------------

ORG_DIRECTORY = Path(
    "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents"
).expanduser()
DEFAULT_SOURCE = ORG_DIRECTORY / "archive" / "denote"
DEFAULT_TARGET = ORG_DIRECTORY / "grove"

# Denote filename: 20240104T120451--title-slug__tag1_tag2.org
DENOTE_FILENAME_RE = re.compile(
    r"^(?P<id>\d{8}T\d{6})--(?P<slug>[^_]+?)(?:__(?P<tags>[^.]+))?\.org$"
)

# Frontmatter lines we strip (denote-specific, noisy in Grove).
STRIP_FRONTMATTER_RE = re.compile(r"^\s*#\+(identifier|signature):.*$", re.IGNORECASE)

# Match [[denote:20240104T120451]] and [[denote:ID::*Heading]] (with optional desc)
DENOTE_LINK_RE = re.compile(
    r"\[\[denote:(?P<id>\d{8}T\d{6})(?:::[^\]]*)?\](?:\[(?P<desc>[^\]]*)\])?\]"
)


# --- Data --------------------------------------------------------------------


@dataclass
class DenoteFile:
    path: Path
    identifier: str
    slug: str  # filename slug (e.g. "elfeed-for-review")
    title: str  # from #+title: header (preferred for wikilinks)
    body: str  # raw file contents


# --- Helpers -----------------------------------------------------------------


def read_title(text: str, fallback: str) -> str:
    """Extract `#+title:` value, fall back to the slug."""
    m = re.search(r"^\s*#\+title:\s*(.+?)\s*$", text, re.IGNORECASE | re.MULTILINE)
    return m.group(1).strip() if m else fallback


def parse_denote_file(path: Path) -> DenoteFile | None:
    m = DENOTE_FILENAME_RE.match(path.name)
    if not m:
        return None
    text = path.read_text(encoding="utf-8")
    return DenoteFile(
        path=path,
        identifier=m.group("id"),
        slug=m.group("slug"),
        title=read_title(text, m.group("slug")),
        body=text,
    )


def discover(source: Path) -> list[DenoteFile]:
    """Find all Denote-named .org files (skip .org_archive, non-org)."""
    notes: list[DenoteFile] = []
    for p in sorted(source.glob("*.org")):
        if p.suffix != ".org":
            continue
        d = parse_denote_file(p)
        if d:
            notes.append(d)
    return notes


def transform_body(body: str, id_to_title: dict[str, str]) -> str:
    """Strip denote-specific frontmatter; rewrite denote: links into wikilinks."""
    lines = [ln for ln in body.splitlines() if not STRIP_FRONTMATTER_RE.match(ln)]
    cleaned = "\n".join(lines)

    def repl(match: re.Match[str]) -> str:
        ident = match.group("id")
        desc = match.group("desc")
        target = id_to_title.get(ident)
        if target is None:
            # Unknown reference — leave the original alone so we don't lose info.
            return match.group(0)
        # Prefer the description if it differs from the title, else just title.
        return f"[[{desc}]]" if desc and desc != target else f"[[{target}]]"

    return DENOTE_LINK_RE.sub(repl, cleaned)


def pick_target_name(slug: str, used: set[str]) -> str:
    """Return `slug.org`, or `slug-N.org` if there's a collision."""
    base = f"{slug}.org"
    if base not in used:
        used.add(base)
        return base
    n = 2
    while True:
        candidate = f"{slug}-{n}.org"
        if candidate not in used:
            used.add(candidate)
            return candidate
        n += 1


# --- Main --------------------------------------------------------------------


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--source", type=Path, default=DEFAULT_SOURCE, help=f"Denote source dir (default: {DEFAULT_SOURCE})")
    parser.add_argument("--target", type=Path, default=DEFAULT_TARGET, help=f"Grove vault dir (default: {DEFAULT_TARGET})")
    parser.add_argument("--apply", action="store_true", help="Actually write files (default: dry-run)")
    parser.add_argument("--force", action="store_true", help="Overwrite existing files in target")
    args = parser.parse_args()

    if not args.source.is_dir():
        print(f"error: source not found: {args.source}", file=sys.stderr)
        return 1

    notes = discover(args.source)
    if not notes:
        print(f"No Denote-formatted .org files found in {args.source}")
        return 0

    id_to_title = {n.identifier: n.title for n in notes}
    used_names: set[str] = set()
    plan: list[tuple[DenoteFile, Path]] = []
    for note in notes:
        target_path = args.target / pick_target_name(note.slug, used_names)
        plan.append((note, target_path))

    mode = "APPLY" if args.apply else "DRY-RUN"
    print(f"[{mode}] {len(plan)} files: {args.source} -> {args.target}\n")

    if args.apply:
        args.target.mkdir(parents=True, exist_ok=True)

    written = skipped = 0
    for note, target_path in plan:
        rel = target_path.relative_to(args.target)
        if target_path.exists() and not args.force:
            print(f"  SKIP  {rel}  (exists; use --force to overwrite)")
            skipped += 1
            continue

        new_body = transform_body(note.body, id_to_title)
        print(f"  COPY  {note.path.name}  ->  {rel}")

        if args.apply:
            target_path.write_text(new_body, encoding="utf-8")
            written += 1

    print()
    if args.apply:
        print(f"Done. Wrote {written}, skipped {skipped}.")
    else:
        print(f"Dry-run complete. Re-run with --apply to write {len(plan) - skipped} files.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
