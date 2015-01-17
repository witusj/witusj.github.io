library(shiny)
library(xtable)
library(stringr)

# Function to read Google Spreadheet
readSpreadsheet <- function(url, sheet = 1){
  library(XML)
  library(httr)
  r <- GET(url)
  html <- content(r)
  sheets <- readHTMLTable(html, header=FALSE, stringsAsFactors=FALSE)
  df <- sheets[[sheet]]
  dfClean <- function(df){
    nms <- t(df[1,])
    names(df) <- nms
    df <- df[-1,-1] 
    df <- df[df[,1] != "",]
    row.names(df) <- seq(1,nrow(df))
    df
  }
  dfClean(df)
}

# Read data from Google Spreadsheet
url <- "https://docs.google.com/spreadsheets/d/1NaBPxnSMkad9K1BpQAVSIipP4HlS3tUvQvq5GSj1zVg/pubhtml"
SchrData <- readSpreadsheet(url)

# Split string into elements by comma, add to separate columns and rename columns
# Only select relevant colums and unique records and delete whitespaces from 'ID'
tmp <- matrix(unlist(strsplit(SchrData$Studentgegevens, ",")), ncol=4,
              byrow=TRUE)
SchrData <- cbind(SchrData, as.data.frame(tmp))
colnames(SchrData)[c(3:7)] <- c('Score.Schr','Last2', 'First', 'Last1', 'ID')
SchrData <- SchrData[2:7]
SchrData <- unique(SchrData)
SchrData$ID <- str_trim(SchrData$ID, side = "both")
SchrData$Score.Schr <- as.numeric(SchrData$Score.Schr)

