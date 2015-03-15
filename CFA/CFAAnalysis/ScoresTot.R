# Load data frames
source('MetaData.R')
source('ScoresWK4.R')
source('ScoresWK5.R')
source('ScoresWK6.R')
source('ScoresWK7.R')


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
ScoresOverv <- ScoresAll[c(22, 23, 24, 1, 25, 26, 27, 29, 30, 3, 8, 13, 18)]
ScoresOverv[is.na(ScoresOverv)] <- 0
SumScr <- rowSums (ScoresOverv[10:13], na.rm = FALSE, dims = 1)
MinScr <- apply(ScoresOverv[,10:13],1,min)
TotScr <- SumScr - MinScr
ScoresOverv$Gemiddelde.Top3 <- round(TotScr/3,1)
ScoresOverv <- ScoresOverv[order(ScoresOverv$Last1.y),]
colnames(ScoresOverv)[c(1:3)] <- c('Voor', 'Tussen', 'Achter')

## Count number of sufficient scores in selected groups
table(ScoresOverv$Gemiddelde.Top3[ScoresOverv$Locatie == 'Nijmegen'
                                  & ScoresOverv$Groep %in% c('C02', 'C03')]<5.5)

## Count number of sufficient scores for location Nijmegen
AvgN <- table(ScoresOverv$Gemiddelde.Top3[ScoresOverv$Locatie == 'Nijmegen' & ScoresOverv$Gemiddelde.Top3 != 0]<5.5, 
      ScoresOverv$Groep[ScoresOverv$Locatie == 'Nijmegen' & ScoresOverv$Gemiddelde.Top3 != 0])

## Count number of sufficient scores for location Arnhem
AvgA <- table(ScoresOverv$Gemiddelde.Top3[ScoresOverv$Locatie == 'Arnhem' & ScoresOverv$Gemiddelde.Top3 != 0]<5.5, 
      ScoresOverv$Groep[ScoresOverv$Locatie == 'Arnhem' & ScoresOverv$Gemiddelde.Top3 != 0])

## Count number of sufficient scores per education level
AvgOpl <- table(ScoresOverv$Gemiddelde.Top3[ScoresOverv$Opleiding %in% c('MBO', 'HAVO', 'VWO') & ScoresOverv$Gemiddelde.Top3 != 0]<5.5, 
              ScoresOverv$Opleiding[ScoresOverv$Opleiding %in% c('MBO', 'HAVO', 'VWO') & ScoresOverv$Gemiddelde.Top3 != 0])
dimnames(AvgOpl)[[1]] <- c('Voldoende', 'Onvoldoende')
PercVold <- sapply(c(1:3), function(x) round(AvgOpl[1,x]/(AvgOpl[1,x]+AvgOpl[2,x]),2))
AvgOpl <- rbind(AvgOpl, PercVold)

## Calculate average per group
ScoresOvervNZ <- ScoresOverv[ScoresOverv$Gemiddelde.Top3 != 0,]
AggGrp <- aggregate(ScoresOvervNZ[10:14], by = list(ScoresOvervNZ$Locatie, ScoresOvervNZ$Groep), FUN = mean, na.rm = TRUE)
AggGrp <- AggGrp[order(AggGrp$Group.1),]

## Write tables to Excel
write.csv(ScoresOverv[ScoresOverv$Locatie == 'Nijmegen',], 'ScoresTotN.csv')
write.csv(ScoresOverv[ScoresOverv$Locatie == 'Arnhem',], 'ScoresTotA.csv')
write.csv(AvgN, 'AvgN.csv')
write.csv(AvgA, 'AvgA.csv')

