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
url <- "https://docs.google.com/spreadsheets/d/1gY60VeGU4bGjKYPHELlCXeeZHgOjHd_jF_Ai9Qfcpoo/pubhtml?gid=462974669&single=true"
WK7Data <- readSpreadsheet(url)

# Split string into elements by comma, add to separate columns and rename columns
# Only select relevant colums and unique records and delete whitespaces from 'ID'
tmp <- matrix(unlist(strsplit(WK7Data$Studentgegevens, ",")), ncol=4,
              byrow=TRUE)
WK7Data <- cbind(WK7Data, as.data.frame(tmp))
colnames(WK7Data)[c(3:7)] <- c('Score','Last2', 'First', 'Last1', 'ID')
WK7Data <- WK7Data[2:7]
WK7Data <- unique(WK7Data)
WK7Data$ID <- str_trim(WK7Data$ID, side = "both")
