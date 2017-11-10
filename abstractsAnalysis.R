library(tidyverse)
library(ggplot2)
library(lubridate)
library(stringr)
library(ggthemes)
load('papersAbstract.RData')
load('tables.RData')

five38Mod <- theme_fivethirtyeight() + theme(legend.position = "right", 
                                             legend.direction = 'vertical',
                                             axis.title = element_text())

#Have a lot of of NAs for views, not for abstracts. Have equal timeouts
lapply(papersAbstract[,c('abstract','views')], function(x) table(is.na(x)))
lapply(papersAbstract[,c('abstract','views')], function(x) table(x == 'timeout'))

papersAbstract <- drop_na(papersAbstract, abstract) %>% filter(abstract != 'timeout',
                                                               abstract != '')
levels(papersAbstract$type)

#Inspect each field, make sure it worked out
meanChars <- papersAbstract %>% group_by(type) %>% summarize(meanChar = mean(nchar(abstract)))
scrapeSample <- papersAbstract %>% distinct(type, .keep_all=TRUE)

#Looks like spotlights shouldn't be included bc they are groups of articles
#
#All except perspective, article, communication actually have no abstract.
#Communications with <100 chars have no abstract, views are abstract
#Instead, views are in the abstract field. Switch them

papersAbstract <- papersAbstract %>% filter(type != 'Spotlights')

papersAbstract[!(papersAbstract$type %in% c('Article','Perspective','Communication')),'views'] <- 
  papersAbstract[!(papersAbstract$type %in% c('Article','Perspective','Communication')),'abstract']
papersAbstract[!(papersAbstract$type %in% c('Article','Perspective','Communication')),'abstract'] <- NA

#Add character count for abstract
papersAbstract <- papersAbstract %>% mutate(abstractChars = nchar(abstract))

#Fix communications with nchar <100
papersAbstract[which(papersAbstract$abstractChars<100),'views'] <- 
  papersAbstract[which(papersAbstract$abstractChars<100),'abstract']
papersAbstract[which(papersAbstract$abstractChars<100),c('abstract','abstractChars')] <- NA

#Make views in to a number
papersAbstract$views <- gsub(',','',papersAbstract$views) %>% as.numeric()

papersAbstractClean <- papersAbstract
save(papersAbstractClean, file='papersAbstractClean.RData')

#Plot length of abstracts by type of paper
ggplot(papersAbstract, aes(x=abstractChars, fill=type)) + 
  geom_density(alpha = .3, color = NA, na.rm=TRUE) + 
  five38Mod + 
  labs(
    x = 'Length of abstract (# Characters)',
    y = 'Density',
    title = 'Abstract lengths by type of article',
    subtitle = 'Only including those with abstracts'
  )

#Boxplot of views by type
ggplot(papersAbstract, aes(x=views, y=..scaled.., fill=type)) + 
  geom_density(color = NA,na.rm=TRUE) + 
  facet_wrap(~type) +
  five38Mod + 
  labs(
    x='Number of Views',
    y = 'Scaled density',
    title = 'Number of views by type of paper',
    fill = 'Paper Type'
  )

