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
url <- "https://docs.google.com/spreadsheets/d/1YfGo8kZb0YKko4LYynSX1V0hVVS0OU7fol9kHIqQ5x4/pubhtml"
TtsData <- readSpreadsheet(url)


Correct <- read.csv("AntwoordenPT2.csv", header = TRUE, sep = ",", quote = "\"",
                    , fill = TRUE, stringsAsFactors = FALSE)

## Create new data frame with selected student data, compare answers with correct solution
## and create per question a new column with scores.
TtsScore <- TtsData[c(2:8)]
TtsScore[8:29] <- sapply(9:30, function(x) as.numeric(Correct[2,x-8])*grepl(Correct[1,x-8], TtsData[[x]])
                         + as.numeric(Correct[2,x+12])*grepl(Correct[1,x+12], TtsData[[22+x]]))

TtsScore$Tot <- rowSums(TtsScore[8:29])
TtsScore$Score <- round(TtsScore$Tot/6, 1)

table(TtsScore$Score < 5.5)
hist(TtsScore$Score)
