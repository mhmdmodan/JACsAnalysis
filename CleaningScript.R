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
#Convert type to factor
Papers$type <- as.factor(Papers$type)
#Delete unnecessary vars
Authors <- select(Authors,-title,-orcidID)