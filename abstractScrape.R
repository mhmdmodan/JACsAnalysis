library(tidyverse)
library(lubridate)
library(rvest)
library(stringr)
library(R.utils)
load('tables.RData')

#fewer papers bc 5 seconds per paper
set.seed(123)
randomPapers <- Papers[sample(nrow(Papers)), ]

#A function to return the abstract and # views
getNewData <- function(doi) {
  #download the paper, return NAs if can't
  paper <- try(read_html(paste0('http://pubs.acs.org/doi/abs/',doi)))
    if(class(paper)[1]=='try-error') {return(c('timeout','timeout'))}
  
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

#Initialize vectors for abstracts, views
abstracts <- vector(mode='character')
views <- vector(mode='character')
progress <- 0
startTime <- seconds(Sys.time())
for(doi in randomPapers$doi) {
  Sys.sleep(sample(seq(1, 3, by=0.001), 1))
  
  progress = progress + 1
  print(progress)
  
  out <- try(withTimeout(getNewData(doi), timeout = 3, onTimeout = 'error'), silent=TRUE)
    if(class(out) =='try-error') {
      print(paste0(doi,' timed out'))
      abstracts <- append(abstracts,'timeout')
      views <- append(views,'timeout')
      next
    }
  
  abstracts <- append(abstracts,out[1])
  views <- append(views,out[2])
}
print(seconds(Sys.time())-startTime)

papersAbstract <- randomPapers[1:length(abstracts),]
papersAbstract$abstract <- abstracts
papersAbstract$views <- views

save(papersAbstract, file='papersAbstract.RData')