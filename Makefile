R:

	Rscript -e "rmarkdown::render('data/pike-time-analysis.Rmd')"
	open data/pike-time-analysis.html

R_deploy:

	cp data/pike-time-analysis.html /Volumes/www_html/multimedia/graphics/projectFiles/Rmd/
	rsync -rv data/pike-time-analysis_files /Volumes/www_html/multimedia/graphics/projectFiles/Rmd
	open http://private.boston.com/multimedia/graphics/projectFiles/Rmd/pike-time-analysis.html