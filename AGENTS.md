# AGENTS.md

Directives and governance for AI coding agents operating in `recode-studio`.

## Governance & Operating Principles
All agents working in this repository must strictly adhere to **The Clanker Constitution**:
* Full directives: [.agents/constitution.md](.agents/constitution.md)
* Core principles:
  1. **Request Fidelity:** Treat instructions, constraints, and conventions as binding.
  2. **Autonomous Judgment:** Scale process to the task; execute simple, robust fixes directly.
  3. **Task Completion:** Persist until verified or genuinely blocked.
  4. **Work Preservation:** Retain progress; write durable shared docs/tests; keep atomic local git commits.
  5. **Reality Verification:** Verify with fresh test runs and direct evidence; never guess or assume.
  6. **Human Communication:** Direct, terse, actionable updates.
  7. **Durable Learning:** Record gotchas and architecture updates in shared instructions.

## Technical Architecture & Operational Directives
* Primary project guidance, gotchas, coding conventions, and testing commands: [CLAUDE.md](CLAUDE.md).
* General overview and user quick-start: [README.md](README.md).

## Repository Hygiene & Path Standards
* **Relative Links & Paths:** Always use standard relative Markdown links (e.g. `[text](path/to/file.md)` or `[text](README.md)`) within repository documentation (`.md` files).
* **No Absolute Local Paths:** Never commit files, docs, or test commands containing host-specific absolute filepaths or local usernames (`C:\Users\...`, `/home/...`, `file:///...`).

## Test Command
```bash
Rscript -e "testthat::test_dir('tests/testthat')"
```
