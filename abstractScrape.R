library(tufte)
library(tidyverse)
library(ggplot2)
library(tint)
library(lubridate)
library(rvest)
library(stringr)
load('tables.RData')

#A function to return the abstract and # views
getNewData <- function(doi) {
  #download the paper, return NAs if can't
  paper <- try(read_html(paste0('http://pubs.acs.org/doi/abs/',doi)))
    if(class(paper)[1]=='try-error') {return(c(NA,NA))}
  
  #Extract the abstract/views. Return NA if can't
  abstract <- 
    try(
      paper %>%
      html_nodes(".articleBody_abstractText") %>%
      html_text()
    )
  if(class(abstract)=='try-error') {abstract <- NA}
  
  views <- 
    try(
      paper %>%
        html_nodes("#ee87036d-9dc1-4a7f-a75c-773ce7320897 .body-none") %>%
        html_text() %>% 
        str_extract('\\d*,*\\d*,*\\d+')
    )
  if(class(views)=='try-error') {views <- NA}
  
  return(c(abstract,views))
}

#Initialize vectors for abstracts, views, progress bar
abstracts <- vector(mode='character')
views <- vector(mode='character')
pb <- txtProgressBar(min = 0, max = 5, style = 3)
progress <- 0
for(doi in Papers$doi[1:5]) {
  Sys.sleep(sample(seq(1, 3, by=0.001), 1))
  
  progress = progress + 1
  setTxtProgressBar(pb, progress)
  
  out <- try(getNewData(doi))
    if(class(out) =='try-error') {
      abstracts <- append(abstracts,NA)
      views <- append(views,NA)
      next
    }
  
  abstracts <- append(abstracts,out[1])
  views <- append(views,out[2])
}
close(pb)