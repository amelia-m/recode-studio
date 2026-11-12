# TODO

Open items for Recode Studio, in full wording so they can be answered later.
Defect detail lives in [docs/review-2026-10-adversarial.md](docs/review-2026-10-adversarial.md);
cross-repo items in [docs/upstream-port-candidates.md](docs/upstream-port-candidates.md).

An options memo covering questions 3 to 8 below (what is at stake, the options
with their costs, what comparable packages do, a recommendation and what it
assumes) is at `_scratch/decisions/memo.md`, with its seven read-only probe
scripts beside it. It deliberately does NOT cover question 9.

## Waiting on Amelia

1. **Merge and push, once the defects are fixed.** Amelia, 2026-11-12: "fix
   first, don't merge until the defects are done." Three branches of October
   work are committed and unpushed or unmerged: `shared-defect-fixes`,
   `text-tools` and `upstream-followups` (tip `2e94179` on the remote), plus
   local commits on `neutral-wording`. `main` is at `ee408d1`. Fast-forwarding
   `main` to the tip of the chain would bring all of them, and the fix branch
   `fix-review-defects` sits on top. Merging and pushing both still need her
   yes at that point, and which of the no-decision defects count as "done"
   enough to merge is hers to call: the seven decision items below will still
   be open.
3. **`cld2` and `wordcloud2`: `Imports` or `Suggests`?** The code and tests
   treat both as optional (`lang_detection_available()`, and
   `skip_if_not_installed()` in the tests), but `cld2` is declared under
   `Imports` and `wordcloud2` is required in practice because its UI call is
   unguarded. Either move both to `Suggests` and keep the guards, or keep them
   in `Imports` and drop the guards and skips. README and CLAUDE.md must match
   whichever is chosen.
4. **Spreadsheet formula escaping versus the byte-identical promise.** Cells
   beginning `=`, `+`, `-`, `@` or a tab are written verbatim into the cleaned
   CSV and the rule files, so a spreadsheet evaluates them on open (a cell
   whose text is `-2+3` displays as `1`, and `=HYPERLINK(...)` can exfiltrate
   neighbouring cells). Escaping them at the writers changes the bytes, so the
   bundle's "the script reproduces this CSV byte for byte" claim breaks unless
   the generated script escapes identically. Which property wins?
5. **The literal `<NA>` collision.** A rule whose `old_value` or `new_value` is
   the four-character text `<NA>` is destroyed by the CSV round trip, because
   that string is the file's missing-value sentinel: on read the rule silently
   switches to targeting missing cells, or becomes a blanking rule. The
   convention is deliberate and documented, and changing it costs
   compatibility with existing rule files. Change the encoding, add a
   validation class only, or accept it?
6. **`author = Sys.info()[["user"]]`.** Every rule minted by Browse values or
   Spellcheck records the OS account name, which lands in `recodes_master.csv`
   and therefore in any shared export bundle. On a managed machine that is an
   employee identifier the user never sees before sharing. Leave it and
   document it, or make the author settable with this as the default?
7. **The two cluster caps.** `PAIRWISE_CLUSTER_CAP` is pinned to 1000 by a
   literal assertion, which the project's own testing rule discourages, while
   `MAX_CLUSTER_CARDS` is not pinned at all (raising it to a million passes the
   whole suite). Delete the value pin and leave both caps covered by behaviour
   tests with the shipped values in a comment, or pin both?
8. **Install `shinytest2`?** One hypothesis cannot be settled without a browser
   or `shinytest2`: per-cluster `exclude_<cid>` and `target_<cid>` inputs may
   outlive the cluster they belong to when the threshold, algorithm or selected
   variable changes, which would mean Apply excluding members the user never
   ticked. Installing it would also let the suite cover re-render input
   lifetimes generally.
9. **Mutation coverage for the four less-reviewed modules.** The test-quality
   review assessed `mod_text_analysis`, `mod_variable_picker`, `mod_data_input`
   and `mod_normalize` by reading only; its mutation budget went to the engine
   and the named headline fixes. Three or four more mutants in
   `text_helpers.R` and `stopword_helpers.R` would settle whether that is
   enough.
10. **A "(missing)" row in Browse values.** `read_dataset()` turns empty,
    quoted-blank and whitespace-only cells into NA, so a loaded file never
    contains `""` and the empty-string work added in September rarely fires for
    CSV input. NA cells cannot currently be targeted from Browse values at all,
    though the engine, the generated script and the bundle all handle an
    NA-targeting rule. Should Browse values offer a clickable "(missing)" row
    that mints such a rule?
11. **The five word-fixes spec questions**, in
    [docs/superpowers/specs/2026-09-16-word-fixes.md](docs/superpowers/specs/2026-09-16-word-fixes.md):
    may a word fix delete a word (empty replacement); should the replacement
    mirror the original word's capitalisation or be used as typed; should word
    fixes be left out of the `norm_spec` stamp; should they live on the
    Normalize tab or a tab of their own; and should Spellcheck default to "fix
    this word everywhere" for long-text columns and "fix this value" for short
    ones, or always ask?
12. **Showcase data.** From the dataset scout's report: is data with an
    unconfirmed licence acceptable if the app or a vignette loads it from a
    URL rather than bundling it; should `ISOcodes` be added to `DESCRIPTION`
    for a country-name taxonomy demo; is a cocktails (alcohol) dataset
    acceptable in a public demo; and should the free-text classifier stop
    flagging date, licence-number and image-URL columns as free text?
13. **The `norm_spec` stamp is ambiguous for some column names.** The stamp
    joins targets with `|` and separates a column from its ops with `=`
    (`drink1=uppercase|drink2=uppercase`), so a column whose own name contains
    `=` or `|` produces a stamp that cannot be parsed back. Changing the format
    changes what every existing rule file compares against, which could turn
    quiet notes into drift warnings on files already in use. Change the format,
    escape the delimiters, or accept it and document the limit?
14. **Should codegen merge groups that can reach a shared column?** The overlap
    WARNING tells the reader that two sibling patterns both reach a column and
    that the blocks run in sequence. Merging those groups into one
    `case_when()` would remove the hazard instead of reporting it, but
    `CLAUDE.md` documents one block per `effective_pattern`, and merging
    changes which rule claims a cell. Keep reporting, or change the contract?
15. **The Recategorize preview only notices the form changing.** It records the
    parsed form when computed and greys out while the form differs, but it does
    not notice that the recode rules or the loaded dataset changed underneath
    it, so a stale preview can read as current. `CLAUDE.md` documents the
    snapshot behaviour deliberately, so widening it is a product call: should
    the preview also invalidate on a rule-set or dataset change?
16. **One sentence classifying this repository for unattended work.** The
    global rules let agents build and change code without check-ins in a
    personal repository, and allow the same in a work-adjacent project only
    where the project profile records Amelia classifying it low risk for
    unattended work, quoted with the date. This repo is public, domain-neutral
    and holds no work data, but it was spun out of a work pipeline, so an agent
    cannot settle which category it is. One sentence from Amelia would go into
    `.agents/project-profile.md` verbatim.
17. **This repository has no `.gitattributes`.** `git commit` warns "LF will be
    replaced by CRLF" on this machine, where `core.autocrlf=true` is global, so
    the working tree can hold mixed endings and byte-level comparisons (the
    bundle's byte-identical reproduction test, SHA-256 of a loaded file) can
    differ between checkouts. The file wanted is `* text=auto eol=lf`, with
    `text eol=crlf` for `.bat` and `.cmd`, `binary` for `.rds`, `.RData`,
    `.xlsx`, `.pdf` and images, and `-text` for `.reg`, followed by
    `git add --renormalize .`. Deliberately NOT done on 2026-11-12: three fix
    agents held worktrees of every file in `R/` and `tests/`, and a tree-wide
    renormalize would have collided with all three. Do it as its own commit on
    a clean tree once those branches are integrated.
18. **The development-story deck.** The draft task contract is at
    `_scratch/contracts/2026-11-10-dev-story-deck-draft.md` and has four open
    decisions: whether agents may read this project's 8 session transcripts
    (several of those sessions read the private upstream work repository, so
    the transcripts hold material derived from it); what may be named in a
    public room; which story is the spine; and where the deck and its claims
    table live. Nothing is dispatched until those are settled.

## Known defects not yet fixed

See [docs/review-2026-10-adversarial.md](docs/review-2026-10-adversarial.md)
for all 18 with reproductions. Items 4, 5, 6 and 9 of that document's
"Decisions needed" list are questions 4 to 7 above.

Unresolved hypotheses recorded there: per-cluster input ids outliving their
cluster; an arbitrary `delete_row` value from the client dropping the wrong
rows; multi-sheet Excel upload ordering; whether Excel rewrites `updated_at`
on a round trip; and whether a priming `flushReact()` before the first
`setInputs` on an `ignoreInit` observer is only a test-harness artifact.

## Deferred by agents, carried here

- Excel is the only remaining path by which an empty-string recode rule can
  fire, and it was never tested: writing a one-cell `.xlsx` needs a package
  that is not a dependency. The check is to write a workbook holding one empty
  cell and one space-only cell, read it with `read_dataset()` and print what
  came back.
- The `.xlsx` branch of `generate_clean_script()` was never exercised, and it
  inherits the machine fact that `readxl` fails on a path containing spaces.
- Whether `mod_recategorize` can ever supply a character `priority` (the CSV
  path types it as integer, so the exposure is API-level only).
- Whether a latin-1 encoded value can reach `.rule_hits()` through the app; it
  throws rather than returning a logical when it does.
- Observer accumulation over long sessions (repeated re-renders of cluster
  cards and spellcheck action cells) was never measured.
- A value with two misspellings can only ever be half-corrected on Spellcheck,
  because only the first unrecognised token per value is flagged and the tab
  reads values before recodes are applied.
- Reporting the DT escaping defect upstream is done; upstream fixed it along
  with 15 other items and declined only the cluster debounce.
- **The three vignettes have never been checked against the code, and no test
  covers them.** `vignettes/getting-started.Rmd`, `long-text-analysis.Rmd` and
  `recategorization.Rmd` run real code against the bundled CSVs and
  `DESCRIPTION` declares `VignetteBuilder: knitr`, so a behaviour change can
  break them silently. Neither `CLAUDE.md` nor `README.md` mentioned them at
  all until 2026-11-12. The check is to render all three and read the output.
- Whether the README should link the three vignettes for users, as opposed to
  listing the folder for agents, is a content decision nobody has made.
- **The readxl half of the blanks rule was read from the package's formals,
  not end to end.** readxl's `na` default is `""` alone, so a cell holding the
  literal text `NA` should arrive from Excel as the string `"NA"` while CSV
  gives missing. The check is to read an `.xlsx` whose cells hold a blank, a
  whitespace-only value and the two letters `NA` through `read_dataset()` and
  confirm the first two are NA and the third is not. It needs a package that
  writes `.xlsx`, which is deliberately not a dependency.
- The CI cache reasoning in `CLAUDE.md` was confirmed only as far as the env
  var's placement and the workflow's own comment. Nobody has read
  `r-lib/actions`' source or a CI run log, which is what would settle it.
- A defensive fix for the per-cluster input hypothesis, from the options memo:
  key the `exclude_<cid>` / `target_<cid>` ids on a hash of the member set
  (`digest` is already a dependency) rather than on a cluster index, which
  makes a stale tick unreachable whether or not the browser still sends it.
  Four read sites. It was not given to the October fix agents, because it
  changes behaviour on an unconfirmed hypothesis; it belongs with whoever
  takes question 8.
- The documentation pass of 2026-11-12 deliberately left four areas alone
  because the three fix branches will change them: the whole testing table
  and its total, the `source_dataset` / `source_quarter` paragraph (a fix
  agent is moving that literal behind a named constant), the free-text
  classifier and example-dataset paragraphs, and the emitted-shape paragraph
  in "Generated R shape". Re-measure each from a fresh run after integration;
  do not hand-adjust the counts.

## Done, kept for context

- cspell now reports 0 issues in 60 files (re-run 2026-11-12), after
  allowlisting 13 deliberate fixture misspellings and 33 technical terms.
  `.claude/**` and `_scratch/**` were added to its ignore list on 2026-11-12:
  agent worktrees and review scratch hold whole extra copies of the repo, and
  without those entries the run scanned 1855 files and died on a mutant copy's
  missing dictionary.
- Git history is not being rewritten: the early fixtures that hinted at the
  upstream agency are fixed in the current tree only, which Amelia accepted on
  2026-09-15 because no agreement prohibits naming the partner.
