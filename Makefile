msnbase2.pdf: msnbase2.tex
	pdflatex msnbase2.tex
	bibtex msnbase2
	pdflatex msnbase2.tex
	pdflatex msnbase2.tex

msnbase2.tex: msnbase2.Rnw
	Rscript -e 'knitr::knit("msnbase2.Rnw")'
