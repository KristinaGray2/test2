library(jsonlite)
library(dplyr)
library(tidyr)
library(tidyverse)

mypath = "~/Dropbox (BBC)/Visual Journalism/Data/2021/vjdata.vote.errors/data"
jsonfiles <- list.files(path = mypath, pattern=".json", full.names=TRUE)

jsonparse <- function(file){
  origininfo <- fromJSON(file)

  tempList <- lapply(seq_along(origininfo), function(i){

    temp <- c(
      origininfo$DivisionId,
      format(as.Date(origininfo$Date), format="%Y"),
      origininfo$AyeCount,
      origininfo$NoCount,
      nrow(origininfo$Ayes),
      nrow(origininfo$Noes)

    )

    column_names <- c(

      "division_id",
      "Year",
      "Aye_count",
      "Noes_count",
      "Ayes_list",
      "Noes_list"
    )

    df <- data.frame(temp,column_names)

  })

  jsondf <- do.call(rbind, tempList)

}

dfList <- lapply(jsonfiles, jsonparse)

finaldf <- do.call(rbind, dfList) %>% distinct()
colnames(df) <- colnames(df, prefix="social")







setwd("~/Dropbox (BBC)/Visual Journalism/Data/2021/vjdata.vote.errors/data/")
listjson <- dir(pattern = "*.json")

for(k in 1:length(listjson)){

  data < fromJSON(listjson[k])

  Division_id <- data$DivisionId
  Year <- format(as.Date(data$Date), format="%Y")
  Aye_count <- data$AyeCount
  Noes_count <-data$NoCount
  Ayes_list <- nrow(data$Ayes)
  Noes_list <- nrow(data$Noes)

}


files <- list.files(path ="~/Dropbox (BBC)/Visual Journalism/Data/2021/vjdata.vote.errors/data/")

dfList <- lapply(files, function(i) {
  df <- fromJSON(i)

  Division_id <- df$DivisionId
  Year <- format(as.Date(df$Date), format="%Y")
  Aye_count <- df$AyeCount
  Noes_count <-df$NoCount
  Ayes_list <- nrow(df$Ayes)
  Noes_list <- nrow(df$Noes)

  data <- rbind(Division_id, Year, Aye_count, Noes_count, Ayes_list, Noes_list)
  data <- as.data.frame(t(data))

  return(data)

})

finaldf <- do.call(rbind, dfList)


df <- files %>% map( function(x){

  fromJSON(paste0("~/Dropbox (BBC)/Visual Journalism/Data/2021/vjdata.vote.errors/data/", x))

})

winners <- fromJSON("~/Dropbox (BBC)/Visual Journalism/Data/2021/vjdata.vote.errors/data/691.json", flatten=TRUE)

Division_id <- winners$DivisionId
Year <- format(as.Date(winners$Date), format="%Y")
Aye_count <- winners$AyeCount
Noes_count <-winners$NoCount
Ayes_list <- nrow(winners$Ayes)
Noes_list <- nrow(winners$Noes)

data <- rbind(Division_id, Year, Aye_count, Noes_count, Ayes_list, Noes_list)
data <- as.data.frame(t(data))

data$Division_id

data$mismatch <- ifelse(data$Aye_count == data$Ayes_list | data$No_count == data$Noes_list)

data["mismatch"][data["mismatch"] == "1"] <- ifelse(data["Aye_count"] == data["Ayes_list"] | data["No_count"] == data["Noes_list"])

data_2 <- data %>% mutate(mismatch == case_when(

  mismatch = (Aye_count == Ayes_list) ~ "1",
  TRUE ~ "Other"



))

