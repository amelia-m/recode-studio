# Project profile: recode-studio

Project-specific rules and facts for agents working here. The global working
preferences still apply; this file adds to them and never relaxes them.
`CLAUDE.md` holds the architecture, conventions and gotchas; this file holds
the decisions an agent cannot infer from the code.

## AI attribution in git and pull requests

**Keep it.** Commits end with a `Co-Authored-By:` trailer naming the model, and
pull request descriptions keep their generated-with footer.

Amelia, 2026-10-16, answering whether to keep the trailer now that attribution
is decided per project: "Keep it, and record that in a profile."

This covers git and forge text only. Disclosure in published work is a
separate question and is not settled here.

## Review, push and merge

No delegated autonomy: pushing to the remote and merging into `main` wait for
Amelia's explicit yes each time, as the global rule requires. Nothing in this
project overrides that.

Local review before a pull request uses the suite below plus a review chosen
from the global routing table. Hosted bot reviews are requested only when she
asks for one by name.

## Test and check commands

From the repository root:

```
Rscript -e "testthat::test_dir('tests/testthat')"
npx cspell lint "**/*.{R,Rmd,md,json}"
```

Set `RENV_CONFIG_SYNCHRONIZED_CHECK=FALSE` in the environment for local runs:
renv's startup check otherwise costs minutes per run. Never commit that
setting. On a checkout whose project library is empty, `renv::restore()` first,
or the suite reports skips for the optional compiled packages and the count
will not match the documented total.

## Scratch space

`_scratch/` at the repository root, gitignored. Agent review findings,
contracts, probes and mutant copies live there. Anything that must survive a
fresh clone belongs in `docs/` instead.
