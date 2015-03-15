## Function to read in Google Spreadsheet (old method)
google_ss <- function(gid = NA, key = NA) 
{
  if (is.na(gid)) {stop("\nWorksheetnumber (gid) is missing\n")}
  if (is.na(key)) {stop("\nDocumentkey (key) is missing\n")}
  require(RCurl)
  url <- getURL(paste("https://docs.google.com/spreadsheet/pub?key=", key,
                      "&single=true&gid=", gid, "&output=csv", sep = ""),
                cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  read.csv(textConnection(url), header = T, sep = ",", na.strings="", stringsAsFactors = FALSE)
}

## Read in data from Google and correct answers from csv file
TtsData <- google_ss(gid=0, key = "0Aq3Xv-VjVpxSdGs0cTJ0UTNXMmtJYmNVMTBUOU0xQVE")


Correct <- read.csv("AntwoordenPT.csv", header = TRUE, sep = ",", quote = "\"",
                    , fill = TRUE, stringsAsFactors = FALSE)

## Create new data frame with selected student data, compare answers with correct solution
## and create per question a new column with scores.
TtsScore <- TtsData[c(2:7, 9)]
TtsScore[8:29] <- sapply(10:31, function(x) as.numeric(Correct[2,x-9])*grepl(Correct[1,x-9], TtsData[[x]])
                         + as.numeric(Correct[2,x+13])*grepl(Correct[1,x+13], TtsData[[22+x]]))

TtsScore$Tot <- rowSums(TtsScore[8:29])
TtsScore$Score <- round(TtsScore$Tot/6, 1)

table(TtsScore$Score < 5.5)
# hist(TtsScore$Score)

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
TtsData2 <- readSpreadsheet(url)


Correct2 <- read.csv("AntwoordenPT2.csv", header = TRUE, sep = ",", quote = "\"",
                    , fill = TRUE, stringsAsFactors = FALSE)

## Create new data frame with selected student data, compare answers with correct solution
## and create per question a new column with scores.
TtsScore2 <- TtsData2[c(2:8)]
TtsScore2[8:29] <- sapply(9:30, function(x) as.numeric(Correct2[2,x-8])*grepl(Correct2[1,x-8], TtsData2[[x]])
                         + as.numeric(Correct2[2,x+14])*grepl(Correct2[1,x+14], TtsData2[[22+x]]))

TtsScore2$Tot <- rowSums(TtsScore2[8:29])
TtsScore2$Score <- round(TtsScore2$Tot/6, 1)

table(TtsScore2$Score < 5.5)
# hist(TtsScore2$Score)

PrtScore <- rbind(TtsScore, TtsScore2)
PrtScore <- PrtScore[order(PrtScore$Achternaam),]
write.csv(PrtScore, 'PTScores.csv')

