# Pre-flight / Post-flight Checklist

Use this when you want a terse, executor-style view of the skill.

## Pre-flight

- [ ] Resolved `PROJECT_ROOT` (absolute path, single value)
- [ ] Confirmed permission to write inside `PROJECT_ROOT`
- [ ] User intent recorded: create venv? (y/n) generate pyrightconfig? (y/n)

## Per-step

- [ ] `.project` exists
- [ ] `.dir-locals.el` exists
  - [ ] If venv created: `python-shell-virtualenv-root` is an absolute path ending in `/`
  - [ ] If venv created: `python-shell-interpreter` is `"python"`
- [ ] `.venv/bin/python` (or `.venv/Scripts/python.exe` on Windows) is executable, if venv requested
- [ ] `pyrightconfig.json` valid JSON, if generated
  - [ ] `include` matches actual source roots
  - [ ] `executionEnvironments[*].root` ends with `/`
  - [ ] `pythonVersion` is `major.minor`, e.g. `3.12`
  - [ ] `pythonPlatform` matches host (`Darwin` / `Linux` / `Windows`)
  - [ ] `exclude` covers `**/.venv`, `**/node_modules`, `**/tmp`, `**/__pycache__`
- [ ] `.isort.cfg` parseable by `configparser`
- [ ] `setup.cfg` parseable by `configparser`
- [ ] `.isort.cfg` and `setup.cfg` share the same `line_length` / `max_line_length`

## Post-flight

- [ ] Report includes: project root, files created, files preserved, venv path, pyright key values
- [ ] No example paths from this skill's `assets/` leaked into the user's project (e.g. `legacy/server`)
