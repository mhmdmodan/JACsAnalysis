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
    fill='Type of Publication',
    title='Distribution of paper types from 1996-2016'
  ) +
  five38Mod

#Bar plot of avg paper lengths
PapersLen <-   PapersNoRet %>% 
  mutate(paperLen = endpg-startpg) %>% 
  filter(paperLen >= 0)

ggplot(
  PapersLen %>% 
    group_by(type) %>% 
    summarize(avg = mean(paperLen),
              stDev = sd(paperLen)),
  aes(x=reorder(type,avg), y=avg)
) +
  geom_col(fill='dodgerblue3') +
  geom_errorbar(aes(ymin=avg-stDev,ymax=avg+stDev), width=.3, color='grey30') +
  labs(
    x = 'Type of Publication',
    y = 'Average Length (pages)',
    title = 'Average length of publication by type',
    subtitle = 'Error bars represent 1 standard deviation'
  ) +
  coord_flip() +
  five38Mod

#See how paper length changed over time
ggplot(PapersLen %>% filter(type=='Article'), aes(x=pubdate, y=paperLen)) + 
  geom_jitter(alpha=.08) + 
  geom_smooth(method='lm', se=FALSE, color='dodgerblue') + 
  labs(
    x='Date of Publication',
    y='Length of Publication (pages)',
    title='Length of articles from 1996-2016'
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
