library(googleVis)

## Function to read Google Spreadheet
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

## Read data from Google Spreadsheet - Economists
url <- "https://docs.google.com/spreadsheets/d/1ZTFJzQrjysDqbnWi9Q-OEBs-R4C5T8m9VHsZQxr9IRo/pubhtml"

EcData <- readSpreadsheet(url)
colnames(EcData)[24:34] <- c("economen.belangrijk.collegae", "economen.moeilijk.collegae", "economen.interessant.collegae",
                              "economen.belangrijk.studenten", "economen.moeilijk.studenten", "economen.leuk.studenten",
                             "huidige.eisen", "toekomstige.eisen",
                             "piketty", "friedman", "keynes")

## Read data from Google Spreadsheet - Non-Economists and make subsets per role
url <- "https://docs.google.com/spreadsheets/d/1E3KltaM2YSwhSkwfJyTnT0EJzszalGjTV5af1nywM7I/pubhtml"

EcData.Nonec <- readSpreadsheet(url)

colnames(EcData.Nonec)[2] <- "Rol"
EcData.collegae <- EcData.Nonec[EcData.Nonec$Rol == "docent",c(2:5)]
EcData.studenten <- EcData.Nonec[EcData.Nonec$Rol == "student",c(2,7:9)]
colnames(EcData.collegae)[2:4] <- c("belangrijk", "moeilijk", "interessant")
colnames(EcData.studenten)[2:4] <- c("belangrijk", "moeilijk", "leuk")

## Make subset of economist data and add role
EcData.economen.collegae <- EcData[,c(24:26)]
EcData.economen.studenten <- EcData[,c(27:29)]


colnames(EcData.economen.collegae)[1:3] <- c("belangrijk", "moeilijk", "interessant")
colnames(EcData.economen.studenten)[1:3] <- c("belangrijk", "moeilijk", "leuk")
EcData.economen.collegae$Rol <- c(rep("econoom", length(EcData.economen.collegae$belangrijk)))
EcData.economen.studenten$Rol <- c(rep("econoom", length(EcData.economen.studenten$belangrijk)))
EcData.economen.collegae <- EcData.economen.collegae[,c(4,1:3)]
EcData.economen.studenten <- EcData.economen.studenten[,c(4,1:3)]

## Combine data
EcData.economen.collegae <- rbind(EcData.economen.collegae, EcData.collegae)
EcData.economen.studenten <- rbind(EcData.economen.studenten, EcData.studenten)
EcData.economen.collegae <- EcData.economen.collegae[!(EcData.economen.collegae$belangrijk == "" ), ]
EcData.economen.studenten <- EcData.economen.studenten[!(EcData.economen.studenten$belangrijk == "" ), ]
factLevels <- c("Volledig oneens", "Neigend naar oneens", "Neutraal", "Neigend naar eens", "Volledig eens")

EcData.economen.collegae[2:4] <- lapply(EcData.economen.collegae[2:4], function(x) ordered(x, levels = factLevels))
EcData.economen.studenten[,2:4] <- lapply(EcData.economen.studenten[,2:4], function(x) ordered(x, levels = factLevels))

## Build graphs colleagues
tblBelangrijk.c <- prop.table(table(EcData.economen.collegae$belangrijk, EcData.economen.collegae$Rol), 2)
tblBelangrijk.c <-  as.data.frame.matrix(tblBelangrijk.c)
tblBelangrijk.c$score <- rownames(tblBelangrijk.c)
gphBel.c <- gvisColumnChart(tblBelangrijk.c, xvar="score", yvar=c("econoom", "docent"),
                          options = list(isStacked = FALSE, width = 1000, height = 400,
                                         title = "Economie is belangrijk",
                                         hAxis = "", vAxis = "{frequentie: 'Aantal'}"))

tblMoeilijk.c <- prop.table(table(EcData.economen.collegae$moeilijk, EcData.economen.collegae$Rol), 2)
tblMoeilijk.c <-  as.data.frame.matrix(tblMoeilijk.c)
tblMoeilijk.c$score <- rownames(tblMoeilijk.c)
gphMoei.c <- gvisColumnChart(tblMoeilijk.c, xvar="score", yvar=c("econoom", "docent"),
                           options = list(isStacked = FALSE, width = 1000, height = 400,
                                          title = "Economie is moeilijk",
                                          hAxis = "", vAxis = "{frequentie: 'Aantal'}"))

tblInt<- prop.table(table(EcData.economen.collegae$interessant, EcData.economen.collegae$Rol), 2)
tblInt <-  as.data.frame.matrix(tblInt)
tblInt$score <- rownames(tblInt)
gphInt <- gvisColumnChart(tblInt, xvar="score", yvar=c("econoom", "docent"),
                           options = list(isStacked = FALSE, width = 1000, height = 400,
                                          title = "Economie is interessant",
                                          hAxis = "", vAxis = "{frequentie: 'Aantal'}"))


gphDocent <- gvisMerge(gphBel.c, gphMoei.c, horizontal = FALSE)
gphDocent <- gvisMerge(gphDocent, gphInt, horizontal = FALSE)

plot(gphDocent)

## Build graphs students
tblBelangrijk <- prop.table(table(EcData.economen.studenten$belangrijk, EcData.economen.studenten$Rol), 2)
tblBelangrijk <-  as.data.frame.matrix(tblBelangrijk)
tblBelangrijk$score <- rownames(tblBelangrijk)
gphBel <- gvisColumnChart(tblBelangrijk, xvar="score", yvar=c("econoom", "student"),
                          options = list(isStacked = FALSE, width = 1000, height = 400,
                                         title = "Economie is belangrijk",
                                         hAxis = "", vAxis = "{frequentie: 'Aantal'}"))

tblMoeilijk <- prop.table(table(EcData.economen.studenten$moeilijk, EcData.economen.studenten$Rol), 2)
tblMoeilijk <-  as.data.frame.matrix(tblMoeilijk)
tblMoeilijk$score <- rownames(tblMoeilijk)
gphMoei <- gvisColumnChart(tblMoeilijk, xvar="score", yvar=c("econoom", "student"),
                           options = list(isStacked = FALSE, width = 1000, height = 400,
                                          title = "Economie is moeilijk",
                                          hAxis = "", vAxis = "{frequentie: 'Aantal'}"))

tblLeuk <- prop.table(table(EcData.economen.studenten$leuk, EcData.economen.studenten$Rol), 2)
tblLeuk <-  as.data.frame.matrix(tblLeuk)
tblLeuk$score <- rownames(tblLeuk)
gphLeuk <- gvisColumnChart(tblLeuk, xvar="score", yvar=c("econoom", "student"),
                           options = list(isStacked = FALSE, width = 1000, height = 400,
                                          title = "Economie is leuk",
                                          hAxis = "", vAxis = "{frequentie: 'Aantal'}"))


gphStudent <- gvisMerge(gphBel, gphMoei, horizontal = FALSE)
gphStudent <- gvisMerge(gphStudent, gphLeuk, horizontal = FALSE)

plot(gphStudent)

## Create wordcloud
library(wordcloud)
library(RColorBrewer)
library(gdata)

vect2 <- NULL
for(i in EcData[, 16]) {
   vect <- strsplit(i, ",")
   vect2 <- c(vect2, unlist(vect))
   vect2 <- trim(vect2)
  
}
words <- table(vect2)

pal <- brewer.pal(8, "Dark2")
pal <- pal[-(1:2)]
png("wordcloud.png", width=2500,height=1600)
wordcloud(names(words),as.vector(words), scale=c(10,.7),min.freq=0,max.words=100,
          random.order=T, rot.per=0, colors=pal)
dev.off()