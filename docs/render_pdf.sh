#!/bin/bash
# render_pdf.sh — Render each chapter to PDF and splice into one document.
# Run from the CFMG_analyses directory: bash render_pdf.sh
# Requires: quarto, pdfunite (from poppler-utils) or ghostscript

set -e
cd "$(dirname "$0")"

CHAPTERS=(
  "index.qmd"
  "Summary.qmd"
  "Recommendations.qmd"
  "Introduction.qmd"
  "CFMG Survey.qmd"
  "CFMG Governance.qmd"
  "CFMG SFM.qmd"
  "Remote Sensing.qmd"
  "CFMG ET.qmd"
  "CFMG BS.qmd"
  "Conclusions.qmd"
  "Methods.qmd"
  "Appendix.qmd"
  "Appendix II.qmd"
  "Appendix III.qmd"
)

PDFS=()

echo "Rendering chapters to PDF..."
for qmd in "${CHAPTERS[@]}"; do
  if [ -f "$qmd" ]; then
    base="${qmd%.qmd}"
    echo "  Rendering: $qmd"
    quarto render "$qmd" --to pdf --output "${base}.pdf" 2>/dev/null || \
      echo "  WARNING: Failed to render $qmd — skipping"
    if [ -f "${base}.pdf" ]; then
      PDFS+=("${base}.pdf")
    fi
  else
    echo "  SKIP (not found): $qmd"
  fi
done

echo ""
echo "Splicing PDFs..."
OUTPUT="CFM_in_Zambia_Final_Report.pdf"

if command -v pdfunite &> /dev/null; then
  pdfunite "${PDFS[@]}" "$OUTPUT"
  echo "Done: $OUTPUT (using pdfunite)"
elif command -v gs &> /dev/null; then
  gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite \
     -sOutputFile="$OUTPUT" "${PDFS[@]}"
  echo "Done: $OUTPUT (using ghostscript)"
else
  echo "ERROR: Neither pdfunite nor ghostscript found. Install poppler-utils or ghostscript."
  echo "Individual chapter PDFs are available:"
  printf '  %s\n' "${PDFS[@]}"
fi
