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

## Data risk and unattended work

Amelia, 2026-11-12, classifying the repository: "this repo contains no
nonpublic data (it's developing a tool) and no secrets, so risk is low and
agents can work unattended but should routinely have naive and adversarial
agents conduct code review".

So, in practice:

- **Low risk.** No work data, no secrets. The global rule "Nothing above low
  risk goes into a session" is satisfied by the repository itself; a file
  Amelia brings in from elsewhere is classified on its own merits, and the
  bundled fixtures stay synthetic and non-sensitive.
- **Unattended work is allowed**, on the terms the global rule sets for it: a
  new local branch, local commits, nothing pushed, deleted, installed or sent,
  no settings changed, and Amelia reviews the branch at the end. The work still
  gets a task contract, and in place of her approval a fresh agent that has
  seen only the contract reviews it before anything is dispatched.
- **Cloud agents are permitted here**, since the repository holds no work data.

## Review, push and merge

No delegated autonomy: pushing to the remote and merging into `main` wait for
Amelia's explicit yes each time, as the global rule requires. Nothing in this
project overrides that.

Local review before a pull request uses the suite below plus a review chosen
from the global routing table. Hosted bot reviews are requested only when she
asks for one by name.

**Code review is routine here, by two kinds of agent** (Amelia, 2026-11-12,
quoted in full above): a naive reviewer, given the requirements and the diff
and nothing about what the author believes, and an adversarial reviewer, told
to attack the change and reproduce what it finds. Run both on anything beyond
a trivial change, not only when asked, and record what each found and what was
done about it. The October round under
[docs/review-2026-10-adversarial.md](../docs/review-2026-10-adversarial.md) is
the worked example: four read-only reviewers on one branch, every CONFIRMED
finding reproduced by running it.

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
