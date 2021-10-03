# Script for rendering the README fild

rmarkdown::render(input = 'README.Rmd', output_format = 'github_document', output_options = list(  keep_html = FALSE))
