report.pdf: report.tex
	pdflatex report
	bibtex report
	pdflatex report
	pdflatex report

clean:
	-rm report.pdf
	-rm *.out
	-rm *.bbl
	-rm *.aux
	-rm *.blg
