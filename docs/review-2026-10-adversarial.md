# Adversarial review, October 2026: open defects

Four read-only reviewers attacked `origin/main..origin/upstream-followups`
(13 commits, tip `2e94179`) before it was merged: rule engine and codegen,
Shiny behaviour and state, test quality, and security with untrusted input.
Each confirmed the baseline (`FAIL 0 | WARN 0 | SKIP 0 | PASS 2214`) and
reproduced every CONFIRMED finding by running it.

Their full reports and runnable probes were written under `_scratch/reviews/`,
which is gitignored. This file is the durable record. Nothing below is fixed.

Verdict from three of the four lenses independently: do not merge before the
data-loss and silent-wrong-output items are fixed.

## Fix first

1. **Delete removes rules you did not click** (Shiny F1). The Recodes tab
   deletes by matching the stored id string, so duplicated or blank ids mean
   one click removes several, and a rule whose id is NA is replaced by an
   all-NA row on ANY delete, which then rides into the exported rule file.
   Imported files legitimately carry such ids. Three clicks removed five rules
   in the reproduction. Fix: delete by row identity, or
   `which(!is.na(id) & id == x)[1]`. `mod_recategorize.R` has the same defect
   with lower exposure; fix both together.
2. **A rule file can inject JavaScript** (security F8). The Delete and Remove
   buttons in the Recodes, Normalize and Recategorize tables paste the rule id
   into a quoted JS string inside the one column DT does not escape, and ids
   come from imported CSVs and are never rewritten. Confirmed against the real
   module and DT's render hook. Spellcheck and Text analysis already do this
   correctly with `.js_value()` plus `.attr_value()`; copy that at the three
   call sites. The `escape = FALSE` guard test cannot see this.
3. **A dictionary upload destroys a committed dictionary** (security F2).
   Uploading a file named `medical.txt` overwrites the bundled one and reports
   success. Four tracked files are exposed. Fix: refuse or confirm on
   collision, or de-duplicate the name.
4. **A new rule can be silently ignored** (engine 2, Shiny F4).
   `merge_recode_rules()` still matches the stored id while import matches by
   key, so after a stale id arrives, a newly created rule for the same key is
   appended behind the old one and never runs. No warning fires, because the
   rule matched cells but changed none. Fix: key-based comparison, plus a
   warning for a fully shadowed rule.
5. **The generated script can disagree with the app again** (engine 1). The
   overlap WARNING tests patterns only against columns some rule names, so two
   sibling patterns sharing a third column diverge silently. Fix: test patterns
   against the data's column names, or merge groups that can share a column.
6. **A rule for a column absent from the data breaks the bundle's own script**
   (engine 3, Shiny F5). The app skips the rule, the script dies, and the
   provenance tells the reader to run that script. Nothing clears the rule set
   when a different dataset is loaded. Fix: skip such a rule in codegen as
   invalid sibling patterns already are.
7. **Recategorize can rewrite a source column** (engine 5, Shiny F3). An
   `out_col` naming an existing column is accepted by the UI and by
   `validate_recat()`; gaps in that column are filled, which the docs say never
   happens, and provenance reports no columns added. Fix: refuse the collision
   or require an explicit overwrite.

## Then

8. **Right-to-left text breaks the exported script** (security F3). `deparse()`
   does not escape U+202A to U+202E or U+2066 to U+2069 and R's parser refuses
   them, so ordinary Arabic or Hebrew pasted from a word processor yields a
   script that will not run. Fix: escape those code points when emitting
   literals.
9. **Exported CSVs carry spreadsheet formulas** (security F4). A cell whose
   text is `-2+3` displays as `1` for the recipient, and `=HYPERLINK(...)` is a
   live exfiltration vector. DECISION: escaping at the writers breaks the
   byte-identical reproduction promise unless the generated script escapes too.
10. **A plausible recat regex term freezes the app** (security F5).
    `(\w+\s*)+!` took 303 seconds on a 41-character value and did not finish on
    a longer one; nothing can interrupt it. The recode path is clean because
    this range moved it to base R's engine. Fix: bound it, ideally a timeout.
11. **The cluster cap counts values, not characters** (security F6). 20 values
    of 6400 characters took 164 seconds with no warning; the linear algorithms
    took 0.07. Fix: cap on total characters or median length too.
12. **A binary file renamed .txt kills Spellcheck until restart** (security
    F7). The failure latches, so deselecting the bad dictionary does not
    recover, and the user sees a raw UTF-8 error. Fix: validate before copying,
    make the reader total, clear the latch on selection change.
13. **"Take newer" compares timestamps as text** (Shiny F6), the default
    conflict choice, so a stamp separated by a space beats every `T`-separated
    stamp of the same date. Fix: parse both sides; unparseable is never newer.
14. **`apply_to_siblings` edits accept anything** (Shiny F7). `yes` or `True `
    silently switch sibling propagation off, though the card says invalid
    entries are rejected.
15. **No warning for a recat rule that matches nothing** (Shiny F10), while the
    all-clear wording is literally true.
16. **The app will not start without `wordcloud2`** (Shiny F8), pre-existing:
    the UI call is unguarded while `cld2` beside it is guarded.
17. **Duplicate column names apply to the first column only**, silently, with
    no validation issue (tests 6). `read_dataset()` repairs duplicates, so this
    is API-level today.
18. **A literal `<NA>` value is destroyed by the CSV round trip** (engine 4,
    security F11). DECISION: the sentinel is deliberate and documented, and
    changing it costs compatibility with existing rule files.

## Test gaps found by mutation

33 mutants, 29 caught, 4 survived. The apply-versus-codegen agreement tests and
the Spellcheck escaping tests are the strongest parts of the suite.

- **An assertion that cannot fail:** `test-empty_values.R:197` compares a
  constant against markup built by interpolating that same constant. Blanking
  the empty-value label passes all 2214 assertions.
- **`MAX_CLUSTER_CARDS` is unpinned:** raising it to a million passes the
  suite. Its sibling cap is pinned by a literal assertion, which the project's
  own rule discourages. DECISION: which way to resolve.
- **The `.rule_hits()` NA scrub is dead code today** and nothing holds the
  invariant `CLAUDE.md` says never to break.
- **Six source-text assertions**, four of them weaker duplicates of the guard
  files; one of them misses the split-line form the guard catches.
- **No test at all** for the six engine findings above, nor for duplicate
  column names, embedded newlines, very long values, right-to-left text or a
  regex capture group.
- A latin-1 encoded value makes `.rule_hits()` throw rather than return a
  logical; whether `read_dataset()` can produce one is unchecked.

## Decisions needed from the owner

1. Fix before merging, or merge and fix after.
2. `cld2` and `wordcloud2`: `Imports` or `Suggests`. The code and tests treat
   both as optional; `cld2` is declared required and `wordcloud2` is required
   in practice but declared nowhere near the guard.
3. Formula escaping on CSV write versus the byte-identical reproduction promise
   (item 9).
4. The `<NA>` sentinel collision (item 18).
5. `author = Sys.info()[["user"]]` writes the OS account name into every
   exported rule file, which on a managed machine identifies the employee.
   Leave and document, or make it settable.
6. Whether to pin or unpin the two cluster caps.
7. Whether to install `shinytest2` to settle the one unresolved hypothesis:
   per-cluster Exclude ticks may outlive the cluster they belong to when the
   threshold, algorithm or variable changes.
8. Whether `mod_text_analysis`, `mod_variable_picker`, `mod_data_input` and
   `mod_normalize` need mutation coverage too; this round assessed them by
   reading only.

## Unresolved hypotheses

- Per-cluster input ids outliving their cluster (Shiny F12): needs a browser or
  `shinytest2`.
- The client can send an arbitrary `delete_row` value, dropping the wrong rows
  (security F12): ten minutes with `testServer`.
- Multi-sheet Excel upload ordering: `input$sheet` can hold the previous
  workbook's sheet when a new file arrives. Read from code, not reproduced.
- Whether Excel rewrites `updated_at` on a round trip, which affects how likely
  item 13 is in practice.
- Whether a priming `flushReact()` before the first `setInputs` on an
  `ignoreInit` observer is a harness artifact only. If it is not, a test of the
  three Delete handlers could pass while asserting nothing.
