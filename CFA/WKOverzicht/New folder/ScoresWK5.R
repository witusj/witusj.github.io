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
url <- "https://docs.google.com/spreadsheets/d/1TUEsVLeNM6G6wIb_K7Xz7kM6ZZ2w9lBZSWkzu0IM9m4/pubhtml"
WK5Data <- readSpreadsheet(url)

# Split string into elements by comma, add to separate columns and rename columns
# Only select relevant colums and unique records and delete whitespaces from 'ID'
tmp <- matrix(unlist(strsplit(WK5Data$Studentgegevens, ",")), ncol=4,
              byrow=TRUE)
WK5Data <- cbind(WK5Data, as.data.frame(tmp))
colnames(WK5Data)[c(3:7)] <- c('Score','Last2', 'First', 'Last1', 'ID')
WK5Data <- WK5Data[2:7]
WK5Data <- unique(WK5Data)
WK5Data$ID <- str_trim(WK5Data$ID, side = "both")
