#!/bin/sh

MAINFILE=TurboGroups

REFSIZE=''

rm -f .pdflatex.log .bibtex.log
echo pdflatex.1 && pdflatex $MAINFILE              </dev/null > .pdflatex.log && \
echo bibtex.1   && bibtex   $MAINFILE              </dev/null >   .bibtex.log && \
sed -i 's|^\(\\begin{thebibliography}{.*}\)$|\1\n\n'$REFSIZE'|' $MAINFILE.bbl && \
echo pdflatex.2 && pdflatex $MAINFILE              </dev/null > .pdflatex.log && \
echo bibtex.2   && bibtex   $MAINFILE              </dev/null >   .bibtex.log && \
sed -i 's|^\(\\begin{thebibliography}{.*}\)$|\1\n\n'$REFSIZE'|' $MAINFILE.bbl && \
echo pdflatex.3 && pdflatex $MAINFILE              </dev/null > .pdflatex.log && \
echo            && egrep --color -i 'warning|error|written'     .pdflatex.log || \
cat .pdflatex.log 2>/dev/null && echo
cat   .bibtex.log 2>/dev/null && echo
