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
url <- "https://docs.google.com/spreadsheets/d/1z5SFV9ZeEqBUb9kgXAD7LO6TTR5nF3ACQnRPQ4sroEQ/pubhtml?gid=1490289913&single=true"
MetaData <- readSpreadsheet(url)
MetaData$ID <- str_trim(MetaData$ID, side = "both")
MetaData$ID <- as.factor(MetaData$ID)

