# Spellcheck dictionary

Three supplementary tiers feeding the Hunspell spellchecker. A word is treated
as correctly spelled if it appears in any tier OR in Hunspell's system `en_US`
dictionary.

| File | Tier | Scope | Committed |
|---|---|---|---|
| `seed_terms.txt`   | 1 | Bundled domain seed. Ships empty. | Yes |
| `custom_terms.txt` | 2 | Project-shared additions. App "Add to dictionary -> Project" writes here. | Yes |
| `user_terms.txt`   | 3 | Per-user additions. App "Add to dictionary -> User" writes here. | No (gitignored) |

All entries are lowercase, one word per line. Lines starting with `#` are ignored.

## Discipline dictionaries

`dictionary/disciplines/` holds optional, domain-specific word lists (e.g.
`medical.txt`). On the Spellcheck tab you can select one or more to add their
terms to the active spellchecker — useful for jargon-heavy data where the
generic `en_US` dictionary flags everything.

- **Select**: multi-select box on the Spellcheck tab. Selection is per-session.
- **Import**: the "Import a discipline dictionary" file picker copies an
  uploaded `.txt` (one word per line) into this folder and selects it. Imported
  files are committed (shareable) — re-run by anyone who pulls the repo.
- **Bundled** (hand-curated general domain vocabulary; expand via the app):
  - `medical.txt` — drug / clinical / cause-of-death terms
  - `public_health.txt` — epidemiology / surveillance / population health
  - `public_policy.txt` — policy / budgeting / grants / legislative-administrative
  - `education.txt` — K-12 & postsecondary / assessment / instruction

To add a discipline by hand, drop a `<name>.txt` (one lowercase word per line,
`#` comments allowed) into `dictionary/disciplines/` and it appears in the
selector on next app start.

> The bundled lists are term vocabularies (not copyrightable) compiled to
> suppress false-positive spellcheck flags on common professional jargon. They
> are starting points — extend them for your own data via the "Add to
> dictionary" button or by editing the files.
