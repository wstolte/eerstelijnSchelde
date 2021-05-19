## rendering naar een of meerdere van de onderstaande formats

# recommended latex distribution
# install.packages("tinytex")
# tinytex::install_tinytex()  # install TinyTeX

## convert refs to better format 
# require(RefManageR)
# refs <- RefManageR::ReadBib("bib/Oosterschelde.bib")
# RefManageR::WriteBib(refs, file = "bib/Oosterschelde_adapted.bib", biblatex = T)

rm(list = ls())
bookdown::clean_book()
bookdown::clean_book(TRUE)

require(rmarkdown)
# require(tufte)
require(bookdown)

file.remove("_main.Rmd")
file.remove("_main.md")
# gitbook formatted html pages (gebruikt op testpagina)
bookdown::render_book("index.Rmd", 
                      output_format = bookdown::gitbook(
                        ), 
                      new_session = F, clean_envir = F)



# "normal pdf
# options(tinytex.verbose = TRUE)
file.remove("_main.md")
bookdown::render_book("index.Rmd", output_format = bookdown::pdf_book(), 
                      new_session = T, clean_envir = T)



# Veel gemaakte fouten
# Figuurlabels (label in code block) mogen geen underscore (_) bevatten bij rendering naar pdf
# Dubbele labels mogen niet (door hele document, alle hoofdstukken)
# Tufte output laat maar 2 niveau's toe (chapters # en sections ##)
# gebruik geen \\ als directory afscheiding. Gaat niet goed van latex naar pdf