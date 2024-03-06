# test script Chantal screenen artikelen

library(shiny)
library(revtools)

setwd()
dta <- read_bibliography("test revtools.bib")

screen_abstracts(dta)


