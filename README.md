# my-cran-watch

Personal CRAN operations watcher. Run daily by GitHub Actions:

    fetch external sources of truth -> diff against last-observed state -> act on the delta

Everything the tool does is an instance of that loop:

- **version watcher**: CRAN publishes a new version of one of my packages
  -> locate the release commit, push a tag, cut a GitHub release. Tagging
  happens when CRAN says yes, which is the semantically correct moment,
  and requires nothing of me on submission day.
- **check watcher**: a package goes NOTE/WARN/ERROR on a CRAN flavor ->
  open an issue *in this repo*, with upstream attribution attached when a
  dependency published or broke in the same window. Closed automatically
  when the condition clears. The issue history is a searchable log of
  every CRAN wobble, and typically exists days before CRAN's email does.
- **r-universe watcher**: a package on CRAN is absent from, or failing
  in, its universe -> issue.
- **incoming queue**: via foghorn, logs where submissions sit in the
  newbies/pretest/waiting folders during the days-to-weeks limbo.

## Design points

**State is committed back to the repo** (`state/state.json`), so the diff
survives between runs, works from any machine, and its git history is a
free audit log.

**Flavors flake, so conditions have a lifecycle**, not a boolean:

- `observed` - seen once; silent (requires `consecutive_required` runs)
- `active`   - persisted; issue open
- `snoozed`  - add the `snoozed` label to the issue and the watcher stops
  talking, until the condition *changes character* (different status or
  detail text), at which point it wakes the issue. Correctly deciding to
  do nothing is a large fraction of CRAN maintenance; this makes it a
  recorded decision instead of an ignored email.

**Upstream attribution**: failures are cross-referenced against the
package's own dependency list - a dep that published or broke in the last
10 days gets named in the issue body. Upstream-attributed failures
usually fix themselves from someone else's keyboard; snooze and wait.

**Acting vs reporting are separated.** Reporting uses the default
`GITHUB_TOKEN` and never leaves this repo. Acting (tag + release pushes)
requires an explicit `WATCH_PAT` secret *and* an explicit entry in
`repo_map` - the watcher never pushes to a repo it was not told about.

**Release commits are located, never guessed**, in order of confidence:

1. `CRAN-SUBMISSION` file (version + SHA recorded at submission time;
   adopt `devtools::submit_cran()` / usethis conventions and this is exact)
2. `git log -S "Version: x.y.z" -- DESCRIPTION`, taking the newest commit
   that actually contains the version (the bump *to* it)

If neither works, it reports and refuses.

## Setup

1. Create the repo, push this content.
2. Edit `config.yml` (emails, universes). Generate the repo map draft:
   `Rscript scripts/init_repo_map.R` and paste/curate into `config.yml`.
3. Add a fine-grained PAT as the `WATCH_PAT` secret, contents:write
   scoped to the repos in `repo_map` (spans the mdsumner, hypertidy and
   Trackage orgs).
4. Create a `snoozed` label in this repo.
5. First run bootstraps state silently (no retro-firing on old versions):
   trigger `watch` manually with dry run on, eyeball the log, then run
   for real.

## Retro-tagging (one-shot)

`scripts/retro_tag.R` clears historical tag debt: for each package it
walks the full CRAN version history (current db + Archive listing),
proposes a commit per version using the same location logic, and asks
before each push. Cross-check doubtful ones against the metacran mirror
(`github.com/cran/<pkg>` has one commit per CRAN release). Interactive by
design, run once, then the daily watcher keeps things clean forever.

    Rscript scripts/retro_tag.R            # all packages in repo_map
    Rscript scripts/retro_tag.R trip palr  # or a subset

## Not-config rule

Nothing me-specific lives outside `config.yml`. When the pattern is
solid this becomes a template: blank one file, fork, modify, deploy.

## Leans on

- `tools::CRAN_package_db()`, `CRAN_check_results()`, `CRAN_check_details()`
- foghorn (incoming queue; optional)
- r-universe APIs (`<universe>.r-universe.dev/api/packages`)
- metacran mirror (`github.com/cran/<pkg>`) for retro-tag cross-reference
- gh CLI on the Actions runner for all issue/release operations

## Roadmap sketches

- attribution v2: check whether an upstream dep's *reverse* dependency
  cohort is failing too (strong upstream signal)
- snooze-until dates via issue comment parsing
- weekly digest issue summarizing all-quiet runs
- winbuilder / r-hub submission hooks
- template-ification once the pattern has survived a few months
