# Path and Markdown Linking Standards

## Rules
When writing or editing code, comments, or Markdown documentation (`.md`) committed to any repository:
1. **No Absolute Local Paths:** Never commit files containing host-specific absolute filepaths, machine paths, or local usernames (e.g. `C:\Users\<user>\...`, `/home/<user>/...`, `C:\Program Files\...`). Always use project-relative paths, environment variables, or generic tool invocation (`Rscript`, `python`).
2. **Standard Relative Markdown Links:** Always use standard relative links (e.g., `[README](README.md)`, `[Constitution](.agents/constitution.md)`, `[spec](../docs/specs/feature.md)`). Never use file URI schemes (`file:///...`) or host-specific paths in committed markdown files.
3. **Cross-Platform Portability:** Ensure documentation and scripts remain portable across different user environments, operating systems, CI/CD runners, and repository forks.
