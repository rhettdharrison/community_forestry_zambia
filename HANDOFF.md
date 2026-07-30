# HANDOFF — CFM in Zambia: Is It Working? (CFMG_analyses)

## 1. Project purpose

This is the analysis + write-up for a report evaluating whether Community Forest
Management (CFM) is delivering on its goals in Zambia. It's a **Quarto website
project** (not a paper) that renders a multi-chapter report to both HTML (published
via GitHub Pages) and per-chapter PDFs. The work combines:

- A household/CFMG survey (governance, SFM perceptions, economic outcomes, equity)
- Remote-sensing analysis (NASA FIRMS fire detections, Hansen Global Forest Change
  deforestation, biomass) inside vs. outside CFMG boundaries and 1 km buffer zones
- Synthesis chapters (Discussion, Recommendations) aimed at government, NGOs
  (Landscape Alliance / SAFE project / GIZ), and carbon enterprises (BCP)

Published site: https://github.com/rhettdharrison/community_forestry_zambia
(GitHub Pages serves straight from `docs/` — there is no CI workflow; rendering
and pushing `docs/` is done manually).

## 2. Architecture

**Quarto website**, config in [`_quarto.yml`](_quarto.yml). Source is `.qmd` files
in the project root; `output-dir: docs`. `execute: freeze: auto` — cached
computation results live in `_freeze/` and are reused unless a chapter's code
changes.

**Chapters (navbar order)**: Home (`index.qmd`) → Summary → Recommendations →
Introduction → Survey Overview (`CFMG Survey.qmd`) → Governance
(`CFMG Governance.qmd`) → Sustainable Forest Management (`CFMG SFM.qmd`) →
Remote Sensing (`Remote Sensing.qmd`) → Economic Transformation (`CFMG ET.qmd`)
→ Equity and Inclusion (`CFMG BS.qmd`) → Discussion (`Conclusions.qmd`) →
Appendices (Methods, Appendix (survey tool), Appendix II (CF list),
Appendix III (index scores)).

**Styling/layout**: [`custom.css`](custom.css) (navbar, callout boxes, sidebar),
[`title-banner.html`](title-banner.html) (banner above navbar, injected via
`include-before-body`), [`sidebar-footer.html`](sidebar-footer.html) (now emptied
— logos/acknowledgements were moved into `index.qmd` instead of living in the
sidebar).

**Data pipeline (R scripts, run manually, not part of Quarto render)**:
- `CFMG_data_prep.R` — reads raw survey Excel (`../GIZ_CFM Survey_clean_Final.xlsx`,
  one level up, not in this repo), cleans into `cfmg.RData`/`.RData` used by the
  chapter `.qmd` files.
- `create_cf_buffers.R` — builds 1 km buffer polygons around each CF
  (`cfmg_buffers_1km.gpkg`), excising overlaps with neighbouring CFs.
- `download_firms_fire.R` — assigns NASA FIRMS VIIRS fire detections to
  Core/Buffer zones → `fire_cfmg_zones.csv`, used by `Remote Sensing.qmd`.
- `download_hansen_deforestation.R` + `extract_buffer_deforestation.R` — extract
  Hansen GFC tree-cover-loss (2014–2024) per CF (`cfm_hansen_loss_2014_2024.csv`)
  and per buffer (`cfm_hansen_buffer_loss_2014_2024.csv`).

**PDF production**: [`render_pdf.sh`](render_pdf.sh) renders each chapter to PDF
individually and splices with `pdfunite`/ghostscript into
`CFM_in_Zambia_Final_Report.pdf`. `pdf-print.css` is a PDF-only stylesheet. A
Quarto **book** build (`_quarto-book.yml`, not yet created) is the planned
final-PDF approach — see [`PUBLICATION_STRATEGY.md`](PUBLICATION_STRATEGY.md)
Phase 4 for the full skeleton (continuous pagination, roman numerals for front
matter, arabic for main chapters via `\frontmatter`/`\mainmatter`).

**Full plan of record**: [`PUBLICATION_STRATEGY.md`](PUBLICATION_STRATEGY.md) —
read this first for the phased plan (structural changes → content additions →
Rhett-provided text → PDF). It documents before/after target structure and is
the source of truth for what "done" means for each phase.

## 3. Decisions and rationale (non-obvious)

- **Filenames with spaces** (`CFMG ET.qmd`, `CFMG Survey.qmd`, etc.) are
  intentional — renaming would break the published URL structure. This causes a
  **known Quarto freeze bug**: PDF figures render to `<Slug-With-Hyphens>_files/figure-pdf/`
  but Quarto's freeze cache looks for them under `_freeze/<Name With Spaces>/figure-pdf/`,
  so lualatex silently gets missing figures on chapters whose knitr code re-runs.
  Full fix procedure is in memory (`feedback_quarto_pdf_rendering.md`); short
  version: capture the hyphen-named figure dir, copy into
  `_freeze/<Slug-With-Hyphens>/figure-pdf/`, and set `supporting: []` in that
  chapter's `tex.json`/`html.json` so Quarto doesn't clean the dir up. Affected
  chapters: CFMG ET, CFMG BS, CFMG Governance, CFMG SFM, CFMG Survey, Remote Sensing.
- **`Conclusions_cache/` and `Conclusions_files/` are untracked** by convention —
  only commit once `Conclusions.qmd` is fully rendered and stable.
- **Git workflow**: single-author + Claude, work directly on `main`, push
  periodically — no feature branches or PRs. Milestones are marked with tags
  (`Draft1.0` = commit `c284c87`, archived; `Draft2.0` begins at `b2525b4`).
- **Sidebar vs. index page**: logos/acknowledgements used to live in the sidebar
  (injected via `sidebar-footer.html`) and have been moved into `index.qmd`
  instead; the sidebar was narrowed back toward the stock Cosmo width as part of
  that change (see `custom.css` diff / Phase 1.3 in the strategy doc).
- **Title banner**: chapter title now shown in a full-width banner above the
  navbar (`title-banner.html` + `.site-title-banner` CSS) instead of the navbar
  brand text, so the long title doesn't crowd out nav items — `.navbar-brand` is
  hidden via CSS.

## 4. Current state — READ THIS FIRST

**There are ~124 modified + ~30 untracked files sitting uncommitted in the
working tree right now.** This is Phase 1 + Phase 2 of `PUBLICATION_STRATEGY.md`
already implemented locally but not yet committed or pushed:

- `_quarto.yml`: navbar reordered to target order (Summary/Recommendations moved
  up front, Remote Sensing moved before Economic Transformation), title updated,
  `title-banner.html` wired in.
- `custom.css`: title-banner styling added, sidebar-width override removed,
  navbar tweaks.
- `sidebar-footer.html`: emptied (logos moved to `index.qmd`).
- `index.qmd`: Foreword section added (**still a placeholder**, see below),
  Acknowledgements + logos moved in from sidebar, a manual Contents/ToC table
  added, Abbreviations retained.
- `Summary.qmd`: expanded to ~4,150 words per the Phase 2.1 spec.
- All six main chapters (`CFMG Survey.qmd`, `CFMG Governance.qmd`,
  `CFMG SFM.qmd`, `Remote Sensing.qmd`, `CFMG ET.qmd`, `CFMG BS.qmd`) have edits
  — largest diffs are in `Appendix.qmd` (2149 lines) and `CFMG ET.qmd` (662
  lines), likely a mix of author edits in RStudio and Claude's Phase 2 callout
  insertions. **Diff these carefully before assuming they're all just the
  planned callout-box additions** — verify against Phase 2.2 spec before
  committing.
- `docs/` has been fully re-rendered to match (HTML + PDFs), including new
  `docs/PUBLICATION_STRATEGY.html` (this file is website content by accident —
  probably shouldn't be published; check `_quarto.yml` resources/exclude rules).
- Untracked: new `_freeze/*/figure-pdf/` dirs (part of the space-in-filename
  workaround), `PUBLICATION_STRATEGY.md` itself, `render_pdf.sh`,
  `title-banner.html`, `pdf-print.css`, `images/Landscape-Alliance-logo.png`,
  plus some stray `firms_data/`, `hansen_tiles/`, `Rplots.pdf`, `.DS_Store` files
  that likely don't belong in the commit.
- Stale `.claude/worktrees/*` directories exist from prior agent sessions
  (`condescending-varahamihira`, `strange-khayyam`, etc.) — gitignored, safe to
  ignore or clean up, not part of this diff.

**Before doing anything else**: run `git status` and `git diff --stat`, review
what's actually in the working tree, and confirm with Rhett before committing —
per his stated workflow, work happens on `main` and gets pushed directly, but a
124-file uncommitted diff should be reviewed/split sensibly rather than pushed
blind.

## 5. Known issues / open questions

- **Foreword text is still a placeholder** (`index.qmd:34`:
  `*[Foreword text to be inserted]*`) — Phase 3.1, blocked on Rhett.
- **Recommendations.qmd** — Phase 3.2 in the strategy doc says it's blocked on
  additional text from Rhett; no explicit placeholder marker was found by
  grepping for "placeholder/TBD/to be inserted", so check with Rhett whether
  this has already been resolved or is still pending.
- **Full document review pass** (Phase 3.3 — consistency, cross-references,
  final edits) not yet done.
- **`docs/PUBLICATION_STRATEGY.html`** is untracked and appears to have been
  rendered as if it were a chapter — likely unintentional; check whether
  `PUBLICATION_STRATEGY.md` should be excluded from the Quarto render
  (`.quartoignore` currently only excludes `.claude/`).
- **Quarto book PDF (`_quarto-book.yml`)** — planned but not created. Needed for
  a single continuously-paginated final PDF; current `render_pdf.sh` splice
  approach restarts page numbers per chapter.
- **Space-in-filename freeze bug** may resurface on any of the six affected
  chapters if their knitr code is edited again — see §3 and
  `feedback_quarto_pdf_rendering.md` for the recovery steps.
- Large uncommitted diffs in `Appendix.qmd` and `CFMG ET.qmd` haven't been
  characterized in detail here — worth a real read-through before committing to
  confirm they're intentional edits and not accidental regressions.

## 6. Next steps (priority order)

1. **Review and commit the current uncommitted work** (§4) in sensible chunks —
   at minimum separate "structural/config" (Phase 1) from "content" (Phase 2)
   from "render output" (`docs/`, `_freeze/`) commits, and decide whether the
   stray untracked files (`Rplots.pdf`, `firms_data/`, `hansen_tiles/`,
   `.DS_Store`) should be committed, gitignored, or deleted.
2. Get Foreword text and any outstanding Recommendations text from Rhett
   (Phase 3.1/3.2), insert, re-render.
3. Do the full consistency/cross-reference review pass (Phase 3.3).
4. Build `_quarto-book.yml` per the skeleton in
   [`PUBLICATION_STRATEGY.md`](PUBLICATION_STRATEGY.md) §"Phase 4 — Quarto Book
   PDF" and produce the final single continuously-paginated PDF; remove the
   per-chapter `\pagenumbering` header entries first to avoid conflicting with
   `\frontmatter`/`\mainmatter`.
5. Decide fate of `docs/PUBLICATION_STRATEGY.html` and clean up
   `.claude/worktrees/*` if no longer needed.
6. Tag the next milestone (e.g. `Draft3.0`) once the above lands, per the
   existing tagging convention (`Draft1.0` @ `c284c87`, `Draft2.0` @ `b2525b4`).

## 7. How to run it

**Requirements**: [Quarto CLI](https://quarto.org), R with packages
`tidyverse`, `readxl`, `gt`, `ggplot2`, `lme4`, `lmerTest`, `emmeans`, `scales`,
`gitcreds` (data prep), plus `sf`, `terra`, `exactextractr` (spatial/remote-sensing
scripts). PDF rendering needs a LaTeX engine (lualatex, via TinyTeX or full
TeX Live) and `pdfunite` (poppler-utils) or `ghostscript` for splicing.

Render the website:
```bash
quarto render
```
Output goes to `docs/` (matches `output-dir` in `_quarto.yml`). Preview locally:
```bash
quarto preview
```

Render a single chapter (e.g. after editing it) to pick up figure/table changes:
```bash
quarto render "CFMG ET.qmd"
```

Render + splice a full PDF (current, pre-book approach):
```bash
bash render_pdf.sh
```
Output: `CFM_in_Zambia_Final_Report.pdf` (per-chapter PDFs also left alongside).

Data pipeline (run only if raw survey/spatial inputs change — outputs are
already committed as `.RData`/`.csv`/`.gpkg` and consumed directly by the
chapters via `freeze`):
```bash
Rscript CFMG_data_prep.R              # needs ../GIZ_CFM Survey_clean_Final.xlsx
Rscript create_cf_buffers.R           # needs ../CFMG_merged/cfmg_merged.shp
Rscript download_firms_fire.R         # needs firms_data/DL_FIRE_SV-C2_*.zip
Rscript download_hansen_deforestation.R
Rscript extract_buffer_deforestation.R
```

**Git**: work on `main`, push directly (no PR workflow for this project). Tag
milestones with `Draft<N>.0`.
