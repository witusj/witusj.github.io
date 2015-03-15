# Load data frames
source('MetaData.R')
source('ScoresWK4.R')
source('ScoresWK5.R')
source('ScoresWK6.R')
source('ScoresWK7.R')
source('ScoresPT.R')
source('ScoresSchr.R')

# Merge the data frames by 'ID' variable
ScoresAll <- merge(WK4Data, WK5Data, 
      by.x = 'ID', by.y = 'ID', all = TRUE, all.x = TRUE, all.y = TRUE,
      sort = TRUE, suffixes = c(".WK4",".WK5"),
      incomparables = NULL)

ScoresAll <- merge(ScoresAll, WK6Data, 
                   by.x = 'ID', by.y = 'ID', all = TRUE, all.x = TRUE, all.y = TRUE,
                   sort = TRUE, suffixes = c("",".WK6"),
                   incomparables = NULL)

ScoresAll <- merge(ScoresAll, WK7Data, 
                   by.x = 'ID', by.y = 'ID', all = TRUE, all.x = TRUE, all.y = TRUE,
                   sort = TRUE, suffixes = c("",".WK7"),
                   incomparables = NULL)

ScoresAll <- merge(ScoresAll, MetaData, 
                   by.x = 'ID', by.y = 'ID', all = TRUE, all.x = TRUE, all.y = TRUE,
                   sort = TRUE, 
                   incomparables = NULL)

names(ScoresAll)[13] <- "Score.WK6"

# Calculate weekly scores
ScoresAll$Score.WK4 <- round(as.numeric(ScoresAll$Score.WK4)/1.6, 1)
ScoresAll$Score.WK5 <- round(as.numeric(ScoresAll$Score.WK5)/1.6, 1)
ScoresAll$Score.WK6 <- round(as.numeric(ScoresAll$Score.WK6)/1.6, 1)
ScoresAll$Score.WK7 <- round(as.numeric(ScoresAll$Score.WK7)/1.6, 1)

# Create overview and calculate mean
ScoresOverv <- ScoresAll[c(22, 23, 24, 1, 25, 26, 27, 3, 8, 13, 18)]
ScoresOverv[is.na(ScoresOverv)] <- 0
SumScr <- rowSums (ScoresOverv[8:11], na.rm = FALSE, dims = 1)
MinScr <- apply(ScoresOverv[,8:11],1,min)
TotScr <- SumScr - MinScr
ScoresOverv$Gemiddelde.Top3 <- round(TotScr/3,1)

# Add bonusscores
ScoresOverv <- merge(ScoresOverv, PrtScore[c(4,31)], 
                     by.x = 'ID', by.y = 'Studentnummer', all.x = TRUE, all.y = FALSE,
                     sort = TRUE, 
                     incomparables = NULL)

ScoresOverv$Bonus <- (ScoresOverv$Score >= 5.5 & ScoresOverv$Gemiddelde.Top3 >= 5.5)*ScoresOverv$Score
ScoresOverv$Bonus[is.na(ScoresOverv$Bonus)] <- 0

# Calculate endscore
ScoresOverv <- merge(ScoresOverv, SchrData[c(2,6)], 
                     by.x = 'ID', by.y = 'ID', all.x = TRUE, all.y = FALSE,
                     sort = TRUE, 
                     incomparables = NULL)
ScoresOverv$Eindscore <- ScoresOverv$Score.Schr + ScoresOverv$Bonus

ScoresOverv <- ScoresOverv[order(ScoresOverv$Last1.y),]

colnames(ScoresOverv)[c(2:4,13)] <- c('Voor', 'Tussen', 'Achter', 'Score.PT')