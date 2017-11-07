library(tufte)
library(tidyverse)
library(ggplot2)
library(tint)
library(lubridate)
library(rvest)
library(stringr)
library(ggthemes)
load('tables.RData')

PapersNoRet <- Papers %>% filter(type != 'Retraction')
PapersDPlot <- rbind(PapersNoRet, PapersNoRet %>% mutate(type='All'))

five38Mod <- theme_fivethirtyeight() + theme(legend.position = "right", 
                                               legend.direction = 'vertical',
                                               axis.title = element_text())

#Plot of paper publishing density
ggplot(PapersDPlot, aes(x=pubdate, fill=type)) + 
  geom_density(color=NA, alpha=.5, na.rm = TRUE) +
  facet_wrap(~type) +
  labs(
    x='Published Date',
    y='Density',
    fill='Type',
    title='Density of paper types from 1995-2016'
  ) +
  five38Mod

#Filled area plot of papers by type
ggplot(PapersNoRet, aes(x=pubdate, fill=type)) + 
  geom_density(color=NA, na.rm = TRUE, position='fill') + 
  labs(
    x='Published Date',
    y='Percent',
    fill='Type',
    title='Distribution of paper types from 1995-2016'
  ) +
  five38Mod

#Calculate and plot mean and 95% CI of papers by volume
PapersVol <- Papers %>% 
  group_by(issue,volume) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  group_by(volume) %>% 
  summarize(avg = mean(count),
            stdev = sd(count),
            minim = t.test(count)[[4]][[1]],
            maxim = t.test(count)[[4]][[2]])

ggplot(PapersVol, 
  aes(x=volume, y=avg)
) + 
  geom_col(fill='salmon',color=NA) + 
  geom_errorbar(aes(ymin=minim, ymax=maxim), width=.5) +
  five38Mod +
  labs(
    x='Volume',
    y='Average Number of Papers',
    title='Average number of papers by volume',
    subtitle = 'Error bars represent the 95% confidence interval'
  )
