library(tufte)
library(tidyverse)
library(ggplot2)
library(tint)
library(lubridate)
library(rvest)
library(stringr)
load('tables.RData')

set.seed(123)
smallPapers <- Papers[sample(nrow(Papers), 2800), ]

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
progress <- 0
for(doi in smallPapers$doi) {
  Sys.sleep(sample(seq(1, 3, by=0.001), 1))
  
  progress = progress + 1
  print(progress)
  
  out <- try(getNewData(doi))
    if(class(out) =='try-error') {
      abstracts <- append(abstracts,NA)
      views <- append(views,NA)
      next
    }
  
  abstracts <- append(abstracts,out[1])
  views <- append(views,out[2])
}
