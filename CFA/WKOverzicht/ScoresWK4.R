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
url <- "https://docs.google.com/spreadsheets/d/1Q8e9g8162AzWRlPh_nAhyWYfxg48Kz1ziEAzxzHYeyU/pubhtml"
WK4Data <- readSpreadsheet(url)

# Split string into elements by comma, add to separate columns and rename columns
# Only select relevant colums and unique records and delete whitespaces from 'ID'
tmp <- matrix(unlist(strsplit(WK4Data$Studentgegevens, ",")), ncol=4,
              byrow=TRUE)
WK4Data <- cbind(WK4Data, as.data.frame(tmp))
colnames(WK4Data)[c(3:7)] <- c('Score','Last2', 'First', 'Last1', 'ID')
WK4Data <- WK4Data[2:7]
WK4Data <- unique(WK4Data)
WK4Data$ID <- str_trim(WK4Data$ID, side = "both")
