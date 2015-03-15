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
url <- "https://docs.google.com/spreadsheets/d/1hhQLPOAiULCWBfWEXvjmsoDUMiGmIreZWggagk_I-Q0/pubhtml?gid=479611730&single=true"
WK6Data <- readSpreadsheet(url)

# Split string into elements by comma, add to separate columns and rename columns
# Only select relevant colums and unique records and delete whitespaces from 'ID'
tmp <- matrix(unlist(strsplit(WK5Data$Studentgegevens, ",")), ncol=4,
              byrow=TRUE)
WK6Data <- cbind(WK6Data, as.data.frame(tmp))
colnames(WK6Data)[c(3:7)] <- c('Score','Last2', 'First', 'Last1', 'ID')
WK6Data <- WK6Data[2:7]
WK6Data <- unique(WK6Data)
WK6Data$ID <- str_trim(WK6Data$ID, side = "both")
