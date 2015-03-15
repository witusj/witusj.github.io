library(shiny)

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
url <- "https://docs.google.com/spreadsheets/d/1z5SFV9ZeEqBUb9kgXAD7LO6TTR5nF3ACQnRPQ4sroEQ/pubhtml"
WK1Data <- readSpreadsheet(url)

# Isolate line with correct answers
Correct <- WK1Data[WK1Data[,2] == "antwoorden",]
Correct <- unlist(Correct[1,])
WK1Data <- WK1Data[WK1Data[,2] != "antwoorden",]

# Only select set with particular password
WK1Data <- WK1Data[WK1Data$Sleutel == "karelMarx",]

# Simplify column names
names(WK1Data)[2] <- "Voor"
names(WK1Data)[3] <- "Tussen"
names(WK1Data)[4] <- "Achter"
names(WK1Data)[5] <- "Nummer"

for (i in 13:26){
  names(WK1Data)[i] <- paste0("A",i-12)
}

for (j in 27:40){
  names(WK1Data)[j] <- paste0("B",j-26)
}

# Assess answers
WK1New <- WK1Data[c(2:5, 7:8, 12)]
WK1New[8:21] <- sapply(13:26, function(x) 1*(WK1Data[x] == Correct[x])+1*(WK1Data[x+14] == Correct[x+14]))
QNames <- sapply(1:14, function(x) paste0("V",x))
names(WK1New)[8:21] <- QNames

# Calculate and scores and order by last name
WK1New$Sum <- apply(WK1New[8:20],1,sum)
WK1New$Score <- round(WK1New$Sum/0.13)

WK1New <- WK1New[order(WK1New$Achter),]
row.names(WK1New) <- seq(1,nrow(WK1New))

write.csv(WK1New, file="rekentoets.csv")


table(WK1New$Score<68)
apply(WK1New[8:20],2,mean)
