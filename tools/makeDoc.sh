#!/bin/bash
OUTFILE="../pdf/rino.pdf"

[ -e "$OUTFILE" ] && rm $OUTFILE

R CMD Rd2dvi --pdf --no-preview --title="rino: Curt Signorino's R code" \
    --output="$OUTFILE" ../man/*.Rd 

