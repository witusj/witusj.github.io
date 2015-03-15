# Load data frames
source('MetaData.R')
source('ScoresWK4.R')
source('ScoresWK5.R')
source('ScoresWK6.R')


# Merge the data frames by 'ID' variable
ScoresAll <- merge(WK4Data, WK5Data, 
      by.x = 'ID', by.y = 'ID', all = TRUE, all.x = FALSE, all.y = FALSE,
      sort = TRUE, suffixes = c(".WK4",".WK5"),
      incomparables = NULL)

ScoresAll <- merge(ScoresAll, WK6Data, 
                   by.x = 'ID', by.y = 'ID', all = TRUE, all.x = FALSE, all.y = FALSE,
                   sort = TRUE, suffixes = c("",".WK5"),
                   incomparables = NULL)

ScoresAll <- merge(ScoresAll, MetaData, 
                   by.x = 'ID', by.y = 'ID', all = TRUE, all.x = FALSE, all.y = FALSE,
                   sort = TRUE, 
                   incomparables = NULL)

ScoresAll$Score.WK4 <- round(as.numeric(ScoresAll$Score.WK4)/1.6, 1)
ScoresAll$Score.WK5 <- round(as.numeric(ScoresAll$Score.WK5)/1.6, 1)
ScoresAll$Score.WK6 <- round(as.numeric(ScoresAll$Score.WK6)/1.6, 1)

ScoresOverv <- ScoresAll[c(12, 13, 14, 1, 15, 16, 17, 3, 8)]
ScoresOverv <- ScoresOverv[order(ScoresOverv$Last1),]
