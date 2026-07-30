# CFM Zambia Report — Publication Strategy
*Last updated: 2026-06-19*

## Project overview

Quarto website project at `CFMG_analyses/`. Output: rendered HTML site (`docs/`) already structured for online publishing, plus a standalone PDF to be produced. Source files are `.qmd`; navigation is controlled by `_quarto.yml`; styling by `custom.css`; sidebar logos/acknowledgements are injected via `sidebar-footer.html`.

---

## Current structure (before changes)

**Navbar order:** Introduction → Survey overview → Governance → SFM → Economic Transformation → Equity and Inclusion → Remote Sensing → Discussion → Recommendations → Appendices

**`index.qmd`**: Title + brief intro paragraph + ## Summary (long) + ## Abbreviations  
**`Summary.qmd`**: Exists but empty (placeholder only)  
**Each chapter**: ## Summary (brief, at top) → Introduction → body → ## Discussion (at bottom = conclusions text)  
**Sidebar**: Logos + acknowledgements text injected via JS in `sidebar-footer.html`; sidebar intentionally widened in `custom.css`

---

## Target structure (after all changes)

**Navbar order:** Summary | Recommendations | Introduction | Survey Overview | Governance | SFM | Remote Sensing | Economic Transformation | Equity and Inclusion | Discussion | Appendices

**`index.qmd`** (landing page): Foreword (placeholder → final text when provided) + Logos/Acknowledgements (moved from sidebar) + ToC + Abbreviations  
**`Summary.qmd`**: ~4,000-word expanded summary with boxes and subheadings  
**Each main chapter**: ## Summary → *[Conclusions callout box]* → Introduction → body → ## Discussion  
**Sidebar**: Narrowed to standard width (no logos/acknowledgements)

---

## Implementation plan

### Phase 1 — Structural & config changes *(implementable now)*

**Step 1.1 — Reorder navbar in `_quarto.yml`**  
New order: Summary, Recommendations, Introduction, Survey Overview, Governance, SFM, Remote Sensing, Economic Transformation, Equity and Inclusion, Discussion, Appendices.  
Also widen navbar container so all items fit without abbreviation — adjust CSS `max-width` on `.nav-link` or increase navbar height.

**Step 1.2 — Rework `index.qmd`**  
- Remove the ## Summary section (content moves to `Summary.qmd`)  
- Add a `## Foreword` section with placeholder text `[Foreword text to be inserted]`  
- Move logos and acknowledgements from `sidebar-footer.html` into the index body (use larger images and larger font than current sidebar version)  
- Add a `## Contents` section with a manually authored ToC linking to all chapters  
- Retain ## Abbreviations table  

**Step 1.3 — Narrow the sidebar in `custom.css`**  
Remove or reduce the custom `grid-template-columns` override that widens the sidebar. Standard Cosmo theme sidebar (~250px) is appropriate now that logos/acknowledgements are on the index page.

**Step 1.4 — Remove sidebar footer injection**  
Clear or gut `sidebar-footer.html` so logos/acknowledgements no longer appear in the sidebar. (Keep file to avoid breaking the `include-after-body` reference — just empty the script content.)

---

### Phase 2 — Content additions *(implementable now)*

**Step 2.1 — Write expanded Summary (`Summary.qmd`)**  
~4,000 words. Structure:

1. **Headline conclusions** — in a styled callout/box, ~8–10 punchy bullet points
2. **Survey details** — sample, design, coverage, representativeness
3. **Governance** — key findings on legal processes, financial management, elections, FD support
4. **Sustainable Forest Management** — fire, deforestation, wildlife, restoration
5. **Remote Sensing analysis** — satellite-confirmed fire reductions, biomass gains, deforestation caveat
6. **Economic transformation** — carbon income, employment, NTFP underexploitation, diversification gap
7. **Equity and Inclusion** — women's voice, youth concerns, elite capture warnings, benefit sharing

Source material: existing ## Summary paragraphs in `index.qmd` (current draft), Discussion sections of all chapters, and `Conclusions.qmd`.

**Step 2.2 — Add Conclusions boxes to each main chapter**  
For each of the six main chapters (Survey, Governance, SFM, Remote Sensing, Economic Transformation, Equity and Inclusion):
- Extract the ## Discussion text from the bottom of the chapter
- Expand it to approximately 1.5× its current length (add context, nuance, implications)
- Format as a Quarto callout box (e.g. `::: {.callout-note}`) titled "Chapter conclusions"
- Insert immediately after the ## Summary section, before ## Introduction

Chapters and their Discussion sections to expand:
- `CFMG Survey.qmd` — last ~6 paragraphs under ## Discussion
- `CFMG Governance.qmd` — last ~5 paragraphs under ## Discussion  
- `CFMG SFM.qmd` — last ~5 paragraphs under ## Discussion
- `Remote Sensing.qmd` — last ~4 paragraphs under ## Discussion
- `CFMG ET.qmd` — last ~6 paragraphs under ## Discussion
- `CFMG BS.qmd` — last ~5 paragraphs under ## Discussion

---

### Phase 3 — Waiting on Rhett *(hold until text provided)*

**Step 3.1 — Insert Foreword**  
Replace `[Foreword text to be inserted]` placeholder in `index.qmd` with final Foreword text.

**Step 3.2 — Add Recommendations content**  
Insert additional text in `Recommendations.qmd` where indicated. (Current file has structured bullet-point recommendations under Governance, SFM, Economic transformation, Equity and inclusion.)

**Step 3.3 — Full document review pass**  
Read-through of all chapters for consistency, cross-references, and final edits.

---

### Phase 4 — PDF production

**Approach (recommended): render per-chapter PDFs and splice**  
Each `.qmd` can render to PDF individually. Steps:
1. Add `page-break-before: always` CSS and a `pagedjs-break` div to the top of each chapter `.qmd` for clean pagination
2. Add page numbers via CSS `@page` / `@bottom-center` counter in a PDF-specific CSS file, loaded only in PDF format
3. Render each chapter to PDF using `quarto render <file>.qmd --to pdf`
4. Splice the chapter PDFs into one document using `pdfunite` or `ghostscript` (both available in the Linux shell)

Alternative: create a Quarto book-format version (`_quarto-book.yml`) that combines all chapters into a single document. More elegant but requires more restructuring. Recommend only if the splice approach produces poor results (e.g. inconsistent headers/footers).

**PDF-specific changes needed:**
- YAML front matter: each chapter's `format: pdf` section needs `include-in-header` for page numbering
- Adjust any figure widths that look wrong in PDF vs HTML
- Add chapter title to page header for orientation

---

## Files to touch

| File | Changes |
|------|---------|
| `_quarto.yml` | Reorder navbar items |
| `index.qmd` | Remove Summary; add Foreword placeholder, Acknowledgements, ToC |
| `Summary.qmd` | Write full ~4,000-word expanded summary |
| `custom.css` | Narrow sidebar; adjust navbar if needed |
| `sidebar-footer.html` | Empty/gut the injection script |
| `CFMG Survey.qmd` | Add Conclusions box after ## Summary |
| `CFMG Governance.qmd` | Add Conclusions box after ## Summary |
| `CFMG SFM.qmd` | Add Conclusions box after ## Summary |
| `Remote Sensing.qmd` | Add Conclusions box after ## Summary |
| `CFMG ET.qmd` | Add Conclusions box after ## Summary |
| `CFMG BS.qmd` | Add Conclusions box after ## Summary |
| `Recommendations.qmd` | Additional text (Phase 3 — waiting on Rhett) |
| PDF CSS file (new) | Page numbers, breaks for PDF output |

---

## Dependencies & sequencing

```
Phase 1 (structural) → DONE
Phase 2 (content)    → DONE
Phase 3 (Foreword, Recommendations, review) → BLOCKED on Rhett providing text
Phase 4 (PDF — Quarto Book) → after Phase 3 complete
```

Phase 3 items can be inserted at any point once text is available. Phase 4 must come last.

---

## Phase 4 — Quarto Book PDF (final step)

**Why:** Each chapter currently renders as a separate PDF, so page numbers restart in every file. A Quarto Book compiles all chapters into a single LaTeX document with continuous, correctly-formatted pagination throughout.

**Page numbering convention agreed:**
- Front matter (Index, Summary, Recommendations): lowercase Roman numerals — i, ii, iii…
- Main chapters (Introduction onwards): Arabic numerals — 1, 2, 3…

**What to create:** a new `_quarto-book.yml` in `CFMG_analyses/` alongside the existing `_quarto.yml` (which stays for the website). The book config references the same `.qmd` source files — no duplication of content.

**Skeleton `_quarto-book.yml`:**

```yaml
project:
  type: book
  output-dir: book-output

book:
  title: "Community Forest Management in Zambia: Is It Working?"
  subtitle: "Final Report"
  author: "Rhett D Harrison"
  date: today
  chapters:
    - index.qmd
    - Summary.qmd
    - Recommendations.qmd
    - part: "Main Report"
      chapters:
        - Introduction.qmd
        - "CFMG Survey.qmd"
        - "CFMG Governance.qmd"
        - "CFMG SFM.qmd"
        - "Remote Sensing.qmd"
        - "CFMG ET.qmd"
        - "CFMG BS.qmd"
        - Conclusions.qmd
    - part: "Appendices"
      chapters:
        - Methods.qmd
        - Appendix.qmd
        - "Appendix II.qmd"
        - "Appendix III.qmd"

format:
  pdf:
    documentclass: scrbook
    classoption: [oneside]
    geometry: "margin=2.5cm"
    toc: true
    toc-depth: 2
    number-sections: true
    include-in-header:
      text: |
        \usepackage{graphicx}
        \frontmatter          % Roman numerals for index, summary, recommendations
    include-before-body:
      text: |
        \mainmatter           % Switches to Arabic numerals at Introduction
```

**Key points for implementation:**
- The `\frontmatter` / `\mainmatter` LaTeX commands handle the numeral switch automatically when using `scrbook` or `book` document class.
- The `index.qmd` PDF-specific `\vfill` + logos block (`{.content-visible when-format="pdf"}`) will still render correctly in book format.
- The per-chapter `include-in-header: \pagenumbering{...}` entries added to each QMD should be **removed** from the individual files before building the book, to avoid conflicts with `\frontmatter`/`\mainmatter`.
- Render with: `quarto render --profile book` or `quarto render _quarto-book.yml --to pdf`
- Output goes to `book-output/` — keep separate from `docs/` (website output).

---

## Notes on Quarto callout box syntax

For the Conclusions boxes (Phase 2.2):

```markdown
::: {.callout-note icon=false}
## Key conclusions

Paragraph text...
:::
```

For the Headline conclusions in the Summary (Phase 2.1), use a more visually prominent style:

```markdown
::: {.callout-important icon=false}
## Headline conclusions

- Finding one
- Finding two
:::
```

Custom CSS can be added to `custom.css` to further style these boxes (e.g. border colour, background).
