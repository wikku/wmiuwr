.PHONY: all clean

all: prog/program.html prog/program.ipynb prog/program.jl \
     doc/sprawozdanie.pdf

clean:
	-rm -f zadanie.zip prog/program.html
	-rm -f doc/*.pdf doc/sprawozdanie.log doc/sprawozdanie.aux
	-rm -f doc/*.pdf_tex
	-rm -rf prog/.ipynb_checkpoints

zadanie.zip: all
	zip -r zadanie.zip doc prog Makefile

prog/program.html: prog/program.ipynb
	jupyter nbconvert --to html prog/program.ipynb

doc/%.pdf: doc/%.svg
	inkscape -D doc/$*.svg -o doc/$*.pdf --export-latex

doc/%.pdf_tex: doc/%.pdf ;

PLOTS=p1 p2 p3
PLOTS_PDFS=$(PLOTS:%=doc/%.pdf)

.ONESHELL:
doc/sprawozdanie.pdf: doc/sprawozdanie.tex $(PLOTS_PDFS) $(PLOTS_PDFS:=_tex)
	cd doc
	pdflatex sprawozdanie.tex
