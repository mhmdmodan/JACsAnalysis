library(tufte)
library(tidyverse)
library(ggplot2)
library(tint)
library(lubridate)
library(rvest)
library(stringr)
load('tables.RData')

getAbstract <- function(doi) {
  paper <- try(read_html(paste0('http://pubs.acs.org/doi/abs/',doi)))
    if(class(paper)[1]=='try-error') return(NA);
  abstract <- 
    try(
      paper %>%
      html_nodes(".articleBody_abstractText") %>%
      html_text()
    )
  if(class(abstract)=='try-error') return(NA);
  return(abstract)
}

abstracts <- vector(mode='character')
progress <- 0
for(doi in Papers$doi[1:2]) {
  Sys.sleep(sample(seq(1, 3, by=0.001), 1))
  abstract <- try(getAbstract(doi))
  progress
    if(class(abstract)=='try-error') {
      append(abstracts,NA)
      next
    }
  abstracts <- append(abstracts,abstract)
}