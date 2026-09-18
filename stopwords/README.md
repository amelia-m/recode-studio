# Stopword tiers

Stopwords are common words the Text analysis tab leaves out of its "Top words"
and "Top phrases" tables and its word cloud when "Remove common stopwords" is
ticked. Three tiers, mirroring the Spellcheck [dictionary tiers](../dictionary/README.md).
A token is a stopword if it appears in any tier selected on the tab.

| File | Tier | Scope | Committed |
|---|---|---|---|
| `base_stopwords.txt`   | Base    | Standard English stopwords, seeded from `EN_STOPWORDS` in `R/text_helpers.R`. | Yes |
| `custom_stopwords.txt` | Project | Project-shared additions. The tab's "+ Stop" button with target Project writes here. | Yes |
| `user_stopwords.txt`   | User    | Personal additions. "+ Stop" with target User writes here. | No (gitignored) |

All entries are lowercase, one word per line. Lines starting with `#` are
ignored, as are blank lines, multi-word lines and purely numeric lines (the
same cleaning as the dictionary loader).

To share a word added with target Project, `git add` and commit
`custom_stopwords.txt`. The base list is edited by hand only; the app never
writes to it. If `base_stopwords.txt` is missing, the base tier falls back to
the built-in `EN_STOPWORDS`.

Stopwords only change what the Text analysis tab counts. They never change the
data, the recode rules or any export.
