regression-lineaire-simple.pdf: regression-lineaire-simple.Rmd
	echo 'library(rmarkdown); render("regression-lineaire-simple.Rmd", "pdf_document")' | R --vanilla --quiet

regression-lineaire-simple.html: regression-lineaire-simple.Rmd
	echo 'library(rmarkdown); render("regression-lineaire-simple.Rmd", "html_document")' | R --vanilla --quiet

clean:
	rm -f regression-lineaire-simple.md \
		regression-lineaire-simple.html \
		regression-lineaire-simple.pdf \
		*~
	rm -rf regression-lineaire-simple_cache/
	rm -rf regression-lineaire-simple_files/
