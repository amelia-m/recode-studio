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
