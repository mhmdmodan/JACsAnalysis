library(tufte)
library(tidyverse)
library(ggplot2)
library(tint)
library(lubridate)
library(stringr)
library(ggthemes)
load('tables.RData')

test<-c('hello','goodbye','bye')
grepl('bye',test, ignore.case = TRUE)


grepl('synthesis',Papers$title, ignore.case = TRUE)


testTib <- tibble(fred=c(1,24,7,34,7), joe=c(3,6,8,3,8))
testTib[c(TRUE,TRUE,FALSE,TRUE,FALSE),]

matches <- grepl('synthe(tic|s[ei]s)',Papers$title, ignore.case = TRUE)


synthesisPapers <- Papers[matches,]

five38Mod <- theme_fivethirtyeight() + theme(legend.position = "right", 
                                               legend.direction = 'vertical',
                                               axis.title = element_text())

synthesisPapersGraphing <- synthesisPapers %>% filter(type != 'Computer Software Review',
                                                      type != 'Editorial')

ggplot(synthesisPapersGraphing, aes(x=pubdate,y=..scaled..)) + 
  geom_density(color=NA,alpha=.3,aes(fill=type)) + 
  facet_wrap(~type)+
  five38Mod +
  labs(
    x='Date of Publishing',
    y='Density',
    title='Density of papers on synthesis'
  )
