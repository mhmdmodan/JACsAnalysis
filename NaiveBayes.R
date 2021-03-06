library(tidyverse)
library(ggplot2)
library(lubridate)
library(stringr)
library(tm)
library(SnowballC)
library(wordcloud)
library(e1071)
library(gmodels)
library(caret)
load('papersAbstractClean.RData')
load('tables.RData')

#Title Analysis
replacePunctuation <- function(x) {
  gsub('[[:punct:]]+', ' ', x)
}

prop.table(table(Papers$type))
set.seed(123)
papersRandom <- Papers[sample(nrow(Papers)),] %>% 
  filter(type %in% c('Article','Communication'))

#Redo factors to only have article and communication
papersRandom$type <- papersRandom$type %>% as.character() %>% as.factor()

#This order so amino-1-hexanol becomes amino hexanol
titleCorpus <- VCorpus(VectorSource(papersRandom$title))
titleCorpusClean <- tm_map(titleCorpus, content_transformer(tolower))
titleCorpusClean <- tm_map(titleCorpusClean, content_transformer(replacePunctuation))
titleCorpusClean <- tm_map(titleCorpusClean, removeWords, append(stopwords(),'synthesis'))
titleCorpusClean <- tm_map(titleCorpusClean, removeNumbers)

titleCorpusClean <- tm_map(titleCorpusClean, stemDocument)
titleCorpusClean <- tm_map(titleCorpusClean, stripWhitespace)

titleDTM <- DocumentTermMatrix(titleCorpusClean)

trainSet <- c(1:round(.75*nrow(titleDTM)))
titleTrain <- titleDTM[trainSet,]
titleTest <- titleDTM[-trainSet,]
titleTrainLabels <- papersRandom[trainSet,]$type
titleTestLabels <- papersRandom[-trainSet,]$type

#Make a word cloud
wordcloud(titleCorpusClean, max.words = 150, random.order = FALSE, colors = c("deeppink2", "seagreen3", "steelblue2"))

papersNoSynth <- papersRandom
papersNoSynth$title <- gsub('synthesis','',papersNoSynth$title,ignore.case = TRUE)
articles <- subset(papersNoSynth, type == 'Article')
comms <- subset(papersNoSynth, type == 'Communication')

wordcloud(articles$title, max.words = 40, random.order = FALSE, colors = c("deeppink2", "seagreen3", "steelblue2"))
wordcloud(comms$title, max.words = 40, random.order = FALSE, colors = c("deeppink2", "seagreen3", "steelblue2"))

#Remove uncommon words
minTitles <- round(.001*length(trainSet))
titleFreqWords <-findFreqTerms(titleTrain, minTitles)

titleTest <- titleTest[,titleFreqWords]
titleTrain <- titleTrain[,titleFreqWords]

#Convert counts to yes or no
convertCounts <- function(x) {
  x <- ifelse(x>0, 'yes', 'no')
}

titleTrain <- apply(titleTrain, MARGIN = 2,convertCounts)
titleTest <- apply(titleTest, MARGIN = 2,convertCounts)
rm(titleCorpus,titleCorpusClean)

m.bayes <- naiveBayes(titleTrain, titleTrainLabels, laplace=1)
p.bayes <- predict(m.bayes, titleTest)
confusionMatrix(p.bayes, titleTestLabels)
