---
name: init-emacs-python-project
description: Bootstrap a Python project for Emacs development by generating .project, .dir-locals.el, .venv, pyrightconfig.json, .isort.cfg and setup.cfg with values inferred from the real repo layout. Trigger whenever the user says things like "init project", "set up Emacs for this Python project", "为 Emacs 开发做准备", "补齐 .project / .dir-locals.el", "create a venv and wire it into Emacs", "generate pyrightconfig.json", "生成 isort / autopep8 配置", or otherwise asks to make a Python codebase Emacs-friendly — even when they don't explicitly mention every file.
---

# Initialize Emacs Python Project

## What this skill does

Bring a Python repository to a state where Emacs + Pyright + isort + autopep8 all work out of the box, using values derived from the **actual** repo (not from hard-coded examples). Concretely, it can create or update:

- `.project` — marks the project root so Emacs' `project.el` recognizes it
- `.dir-locals.el` — points Emacs at the project's virtualenv
- `.venv/` — a Python virtualenv inside the project
- `pyrightconfig.json` — type-checking config aligned with the real source layout and Python version
- `.isort.cfg` and `setup.cfg` — consistent formatter defaults for isort, pycodestyle, autopep8

The skill is conservative: it never silently overwrites existing config, and never copies template paths verbatim into a real project.

## Operating principles

These are the invariants. Everything else is mechanics.

1. **One project root.** Determine a single absolute path before doing anything else. Prefer `git rev-parse --show-toplevel`; if that fails, ask the user.
2. **Real values, not template values.** The example snippets in `assets/` show *shape*, not *content*. Always recompute paths, Python version, and platform from the actual environment.
3. **Preserve existing config.** If a target file already exists, read it first. Only modify what the user clearly wants changed; otherwise leave it alone and tell them you did.
4. **Validate after each write.** A failed JSON or INI parse means the step isn't done — fix it before moving on.
5. **Ask before doing the expensive / opinionated things.** Creating a venv and generating `pyrightconfig.json` are user choices. The other files (`.project`, `.dir-locals.el` stub, formatter configs) are safe defaults and can be created without prompting.

## Execution flow

Follow these steps in order. Skip any step the user has already opted out of.

### Step 1 — Resolve the project root

```bash
git rev-parse --show-toplevel
```

If that returns a path, use it. If it errors, ask the user for an absolute path; falling back to the current working directory is acceptable only if the user confirms.

**Done when:** you have a single absolute path stored as `PROJECT_ROOT` for the rest of the run.

### Step 2 — Create the always-safe baseline files

In `PROJECT_ROOT`, ensure both files exist:

- `.project` — empty file is fine; it exists purely as a marker.
- `.dir-locals.el` — if absent, start from the empty stub in `assets/dir-locals.empty.el`. If present, leave its content alone for now; Step 3 will edit it only if a venv is being added.

These are safe to create without asking, because they don't change behavior on their own.

### Step 3 — Offer to create a Python virtualenv

Ask the user something like:

> "Want me to create a virtualenv at `PROJECT_ROOT/.venv`? I'll wire it into `.dir-locals.el` so Emacs picks it up automatically."

If yes:

```bash
python3 -m venv <PROJECT_ROOT>/.venv
```

Then **rewrite** `.dir-locals.el` using the template at `assets/dir-locals.with-venv.el`, substituting the **absolute** venv path. Two details that matter:

- The path assigned to `python-shell-virtualenv-root` must end with `/`.
- `python-shell-interpreter` stays as the bare string `"python"` — Emacs resolves it inside the venv.

If the user chose a non-default venv path, honor it.

**Validate:** `<venv>/bin/python` exists and is executable; `.dir-locals.el` loads as valid Elisp (a basic visual check is enough — the file is tiny).

### Step 4 — Offer to generate `pyrightconfig.json`

Ask whether they want type-checking config. If yes, generate it via the helper script rather than writing it by hand — the script handles source-tree discovery, Python version detection, and platform detection consistently:

```bash
python3 scripts/generate_pyrightconfig.py --project-root <PROJECT_ROOT> [--venv <PROJECT_ROOT>/.venv] [--output <PROJECT_ROOT>/pyrightconfig.json]
```

What the script does (see `scripts/generate_pyrightconfig.py` for the exact logic):

- Walks `PROJECT_ROOT` for `.py` files, excluding `.venv`, `node_modules`, `__pycache__`, `build`, `dist`, `.git`, `tmp`.
- Groups them into source roots (a directory with `__init__.py`, or the top-level dir of unique `.py` trees). Multiple independent Python subprojects become multiple `executionEnvironments`.
- Reads the Python version from `<venv>/bin/python` when a venv exists; otherwise from `python3`. Normalizes to `major.minor` (e.g. `3.14.5` → `3.14`).
- Sets `pythonPlatform` from `platform.system()` mapped to `Darwin` / `Linux` / `Windows`.
- Always excludes `**/.venv`, `**/node_modules`, `**/tmp`, `**/__pycache__`.

If the user already has a `pyrightconfig.json`, the script prints a diff and asks before overwriting (pass `--force` to skip the prompt).

**Validate:** the output file parses as JSON (`python3 -m json.tool pyrightconfig.json > /dev/null`).

### Step 5 — Generate `.isort.cfg` and `setup.cfg`

Copy the templates from `assets/isort.cfg.template` and `assets/setup.cfg.template` into `PROJECT_ROOT`, with one dynamic edit:

- `src_paths` in `.isort.cfg` should list the same source roots discovered in Step 4. If Step 4 was skipped, do a quick scan (or omit `src_paths` entirely — isort will still work).

Keep `line_length = 88` consistent across both files. If either file already exists, diff against the template and ask before overwriting.

**Validate:** both files parse with Python's `configparser`.

## Reporting back

After running, give the user a compact summary like:

```
project-root:  /Users/alice/code/widgets
created:       .project, .dir-locals.el, .venv/, pyrightconfig.json, .isort.cfg, setup.cfg
preserved:     (none)
skipped:       (none)
venv:          /Users/alice/code/widgets/.venv/
pyright:
  include:               ["src/widgets"]
  executionEnvironments: [{ root: "src/widgets/", pythonVersion: "3.12" }]
  pythonPlatform:        Darwin
validation:    all files parse OK
```

The point is to make it obvious what changed and what didn't, so the user can verify nothing surprising happened.

## Things to watch out for

- **Don't paste example paths.** The `assets/` files are templates. `legacy/server` in the reference snippets is just an artifact of the project this skill was extracted from — never write it into someone else's repo.
- **Multiple Python subprojects.** If the repo has, say, `services/api/` and `services/worker/` as independent Python packages, emit multiple `executionEnvironments`. Don't collapse them.
- **Existing configs.** Always diff before overwriting. A user with a hand-tuned `pyrightconfig.json` will be unhappy if you blow it away.
- **Path separators on Windows.** The script handles this, but if you're writing JSON by hand, use forward slashes — Pyright accepts them on every platform and they avoid JSON-escaping headaches.
- **Venv path trailing slash.** Emacs is picky here. `/abs/.venv/` works; `/abs/.venv` does not.

## Reference material

- `assets/dir-locals.empty.el` — stub for a fresh `.dir-locals.el`
- `assets/dir-locals.with-venv.el` — template with `__VENV_ABS_PATH__` placeholder
- `assets/pyrightconfig.template.json` — minimal Pyright config (shape only)
- `assets/isort.cfg.template` — isort defaults
- `assets/setup.cfg.template` — pycodestyle + autopep8 defaults
- `scripts/generate_pyrightconfig.py` — discovers source roots, Python version, platform; writes a real `pyrightconfig.json`
- `references/checklist.md` — terse pre-flight / post-flight checklist for executors who want one
