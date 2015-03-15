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


Correct <- read.csv("Antwoorden.csv", header = TRUE, sep = ",", quote = "\"",
                    , fill = TRUE, stringsAsFactors = FALSE)

## Create new data frame with selected student data, compare answers with correct solution
## and create per question a new column with scores.
TtsScore <- TtsData[c(2:7, 9)]
TtsScore[8:29] <- sapply(10:31, function(x) as.numeric(Correct[2,x-9])*grepl(Correct[1,x-9], TtsData[[x]])
                         + as.numeric(Correct[2,x+13])*grepl(Correct[1,x+13], TtsData[[22+x]]))

TtsScore$Tot <- rowSums(TtsScore[8:29])
TtsScore$Score <- round(TtsScore$Tot/6.6, 1)


