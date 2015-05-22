library("knitr")
library("rmarkdown")

render("merge-cow-polity.Rmd", output_file = "README.md")
purl("merge-cow-polity.Rmd", output = "merge-cow-polity.r")
