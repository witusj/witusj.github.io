setwd("~/GitHub/CFA")
library(shiny)
library(xtable)

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
names(WK1Data)[9] <- "BGA"
names(WK1Data)[10] <- "Opleiding"

for (i in 13:26){
  names(WK1Data)[i] <- paste0("A",i-12)
}

for (j in 27:40){
  names(WK1Data)[j] <- paste0("B",j-26)
}

# Assess answers
WK1New <- WK1Data[c(2:5, 7:8, 9:10, 12)]
WK1New[10:23] <- sapply(13:26, function(x) 1*(WK1Data[x] == Correct[x])+1*(WK1Data[x+14] == Correct[x+14]))
QNames <- sapply(1:14, function(x) paste0("V",x))
names(WK1New)[10:23] <- QNames

# Calculate and scores and order by last name
WK1New$Sum <- apply(WK1New[10:22],1,sum)
WK1New$Score <- round(WK1New$Sum/0.13)

# Create new factor level "Overig", set classes, order and create row names
WK1New$Opleiding[WK1New$Opleiding != "MBO"&WK1New$Opleiding != "HAVO"&WK1New$Opleiding != "VWO"] <- "Overig"
WK1New$Opleiding <- as.factor(WK1New$Opleiding)
WK1New$Locatie <- as.factor(WK1New$Locatie)
WK1New$Groep <- as.factor(WK1New$Groep)
WK1New <- WK1New[order(WK1New$Achter),]
row.names(WK1New) <- seq(1,nrow(WK1New))

write.csv(WK1New, file="rekentoets.csv")

stdCnt <- nrow(WK1New)
stdCntOv <- nrow(WK1New[WK1New$Groep == "Overige",])

WK1New$Klas <- paste0(substr(WK1New$Locatie, start=1, stop=1), '-', WK1New$Groep)
tblKl <- data.frame(table(WK1New$Klas))
names(tblKl) <- c("Groep", "Aantal")
xtblKl <- xtable(tblKl, caption = c("Aantal studenten per groep", "Aantal studenten"))

tbltblOplKl <- table(WK1New$Klas, WK1New$Opleiding)
xtblOplKl <- xtable(tbltblOplKl, caption = c("Laatst behaalde opleiding?", "Opleidingsniveaus"))

tblScr <- table(WK1New$Score<68)
xtblScr <- xtable(table(WK1New$Score<68))

tblKlVs <- table(WK1New$Klas,WK1New$Versie)
xtblKlVs <- xtable(table(WK1New$Klas,WK1New$Versie))

tblKlSc <- table(WK1New$Klas[WK1New$Klas == "N-C02"|WK1New$Klas == "N-C03"],WK1New$Score[WK1New$Klas == "N-C02"|WK1New$Klas == "N-C03"]<68)
dimnames(tblKlSc)[[2]] <- c("Voldoende", "Onvoldoende")
xtblKlSc <- xtable(tblKlSc, caption = c("Niveau rekenvaardigheden per groep- Score > 70% is voldoende", "Niveau rekenvaardigheden per groep"))

tblKlBGA <- table(WK1New$Klas,WK1New$BGA)
xtblKlBGA <- xtable(table(WK1New$Klas,WK1New$BGA), caption = c("Alle BGA tentamens gehaald?", "Status BGA tentamens"))

tblOplSc <- table(WK1New$Opleiding[WK1New$Klas == "N-C02"|WK1New$Klas == "N-C03"],WK1New$Score[WK1New$Klas == "N-C02"|WK1New$Klas == "N-C03"]<68)
dimnames(tblOplSc)[[2]] <- c("Voldoende", "Onvoldoende")
xtblOplSc <- xtable(tblOplSc, caption = c("Niveau rekenvaardigheden per opleidingsniveau- Score > 70% is voldoende?", "Niveau rekenvaardigheden per opleiding"))

tblBGASc <- table(WK1New$BGA[WK1New$Klas == "N-C02"|WK1New$Klas == "N-C03"],WK1New$Score[WK1New$Klas == "N-C02"|WK1New$Klas == "N-C03"]<68)
xtblBGASc <- xtable(table(WK1New$BGA,WK1New$Score<68))

tblOplBGA <- table(WK1New$Opleiding,WK1New$BGA)
xtblOplBGA <- xtable(tblOplBGA, caption = c("Alle BGA tentamens gehaald?", "Status BGA tentamens"), comment = FALSE)

apply(WK1New[10:22],2,mean)

## Alleen eigen klassen

WK1NewW <- subset(WK1New, Klas == "N-C02"|Klas == "N-C03")
stdCntW <- nrow(WK1NewW)

tblKlW <- data.frame(table(WK1NewW$Klas))
names(tblKlW) <- c("Groep", "Aantal")
xtblKlW <- xtable(tblKlW, caption = c("Aantal studenten per groep", "Aantal studenten"))

tbltblOplKlW <- table(WK1NewW$Klas, WK1NewW$Opleiding)
xtblOplKlW <- xtable(tbltblOplKlW, caption = c("Laatst behaalde opleiding?", "Opleidingsniveaus"))

tblKlBGAW <- table(WK1NewW$Klas,WK1NewW$BGA)
xtblKlBGAW <- xtable(table(WK1NewW$Klas,WK1NewW$BGA), caption = c("Alle Economie tentamens Propedeuse gehaald?", "Status Economie tentamens"))

tblOplScW <- table(WK1NewW$Opleiding, WK1NewW$Score<68)
dimnames(tblOplScW)[[2]] <- c("Voldoende", "Onvoldoende")
xtblOplScW <- xtable(tblOplScW, caption = c("Niveau rekenvaardigheden per opleidingsniveau- Score > 70% is voldoende?", "Niveau rekenvaardigheden per opleiding"))

