premiers-pas.pdf: premiers-pas.Rmd
	echo 'library(rmarkdown); render("premiers-pas.Rmd", "pdf_document")' | R --vanilla --quiet

premiers-pas.html: premiers-pas.Rmd
	echo 'library(rmarkdown); render("premiers-pas.Rmd", "html_document")' | R --vanilla --quiet

prediction-genomique.pdf: prediction-genomique.Rmd
	echo 'library(rmarkdown); render("prediction-genomique.Rmd", "pdf_document")' | R --vanilla --quiet

prediction-genomique.html: prediction-genomique.Rmd
	echo 'library(rmarkdown); render("prediction-genomique.Rmd", "html_document")' | R --vanilla --quiet

programme-selection.pdf: programme-selection.Rmd
	echo 'library(rmarkdown); render("programme-selection.Rmd", "pdf_document")' | R --vanilla --quiet

programme-selection.html: programme-selection.Rmd
	echo 'library(rmarkdown); render("programme-selection.Rmd", "html_document")' | R --vanilla --quiet

clean:
	rm -f premiers-pas.md \
		premiers-pas.html \
		premiers-pas.pdf \
		*~
	rm -rf premiers-pas_cache/
	rm -rf premiers-pas_files/
