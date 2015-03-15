# Load data frames
source('MetaData.R')
source('ScoresWK4.R')
source('ScoresWK5.R')
source('ScoresWK6.R')


# Merge the data frames by 'ID' variable
ScoresAll <- merge(WK4Data, WK5Data, 
      by.x = 'ID', by.y = 'ID', all = TRUE, all.x = TRUE, all.y = TRUE,
      sort = TRUE, suffixes = c(".WK4",".WK5"),
      incomparables = NULL)

ScoresAll <- merge(ScoresAll, WK6Data, 
                   by.x = 'ID', by.y = 'ID', all = TRUE, all.x = TRUE, all.y = TRUE,
                   sort = TRUE, suffixes = c("",".WK6"),
                   incomparables = NULL)

ScoresAll <- merge(ScoresAll, MetaData, 
                   by.x = 'ID', by.y = 'ID', all = TRUE, all.x = TRUE, all.y = TRUE,
                   sort = TRUE, 
                   incomparables = NULL)
names(ScoresAll)[13] <- "Score.WK6"

ScoresAll$Score.WK4 <- round(as.numeric(ScoresAll$Score.WK4)/1.6, 1)
ScoresAll$Score.WK5 <- round(as.numeric(ScoresAll$Score.WK5)/1.6, 1)
ScoresAll$Score.WK6 <- round(as.numeric(ScoresAll$Score.WK6)/1.6, 1)

ScoresOverv <- ScoresAll[c(17, 18, 19, 1, 20, 21, 22, 3, 8, 13)]
ScoresOverv <- ScoresOverv[order(ScoresOverv$Last1),]
