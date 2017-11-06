library(tufte)
library(tidyverse)
library(ggplot2)
library(tint)
library(lubridate)
library(rvest)
library(stringr)
library(DBI)
library(RSQLite)

con = dbConnect(SQLite(), dbname="database.sqlite")
dfs <- dbListTables(con)

#Make a tibble for each df
for (df in dfs) {
  assign(
    df,
    dbGetQuery(conn=con, statement=paste("SELECT * FROM '", df, "'", sep="")) %>% as.tibble
  )
}
dbDisconnect(con)
rm(con,df,dfs)

#Convert to date
Papers$pubdate <- ymd(substr(Papers$pubdate,1,10))
#Convert types to factors
Papers$type <- as.factor(Papers$type)
Authors$authorID <- as.factor(Authors$authorID)
Authors[Authors$initials=='','initials'] <- NA
Paper_Authors <- lapply(Paper_Authors,as.factor) %>% as.tibble
Papers$paperID <- as.factor(Papers$paperID)

#Delete unnecessary vars
Authors <- select(Authors,-title,-orcidID)