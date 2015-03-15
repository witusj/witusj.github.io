library(googleVis)
library(ggplot2)
library(rCharts)
library(knitr)

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


## Statistics, tables and graphs

### Table with distribution suff vs unsuff for selected groeps
table(ScoresOverv$Gemiddelde.Top3[ScoresOverv$Locatie == 'Nijmegen'
                                  & ScoresOverv$Groep %in% c('C02', 'C03')]<5.5)

AvgN <- table(ScoresOverv$Gemiddelde.Top3[ScoresOverv$Locatie == 'Nijmegen' & ScoresOverv$Gemiddelde.Top3 != 0]<5.5, 
      ScoresOverv$Groep[ScoresOverv$Locatie == 'Nijmegen' & ScoresOverv$Gemiddelde.Top3 != 0])

AvgA <- table(ScoresOverv$Gemiddelde.Top3[ScoresOverv$Locatie == 'Arnhem' & ScoresOverv$Gemiddelde.Top3 != 0]<5.5, 
      ScoresOverv$Groep[ScoresOverv$Locatie == 'Arnhem' & ScoresOverv$Gemiddelde.Top3 != 0])

### Save data tot CSV
write.csv(ScoresOverv[ScoresOverv$Locatie == 'Nijmegen',], 'ScoresTotN.csv')
write.csv(ScoresOverv[ScoresOverv$Locatie == 'Arnhem',], 'ScoresTotA.csv')
write.csv(AvgN, 'AvgN.csv')
write.csv(AvgA, 'AvgA.csv')

var(ScoresOverv$Gemiddelde.Top3, ScoresOverv$Score.PT, na.rm = TRUE)

# Bar charts with scores 'Kennistoets' per week
dataWKs <- na.omit(ScoresOverv[c(6:11)])
dataWKs$Grps <- paste0(substr(dataWKs$Locatie,1,1),'.',dataWKs$Groep)
dataWKs[c(8:11)]<- sapply(c(3:6), function(x) (dataWKs[x]>0)*1)
names(dataWKs)[c(8:11)] <- c('Aanw.WK4', 'Aanw.WK5', 'Aanw.WK6', 'Aanw.WK7')
dataWKsAgg0 <- aggregate(cbind(Score.WK4, Score.WK5, Score.WK6, Score.WK7) ~ Grps,
                        data = dataWKs, FUN = 'mean')
dataWKsAgg1 <- aggregate(cbind(Aanw.WK4, Aanw.WK5, Aanw.WK6, Aanw.WK7) ~ Grps,
                        data = dataWKs, FUN = 'sum')
dataWKsAgg <- merge(dataWKsAgg0, dataWKsAgg1, 
                     by.x = 'Grps', by.y = 'Grps', all.x = TRUE, all.y = TRUE,
                     sort = FALSE, 
                     incomparables = NULL)
dataWKsAgg[-1] <- round(dataWKsAgg[-1],1)
dataWKsAgg[,c(6:9)] <- sapply(c(2:5), function(x) paste0('Gem.Score: ', dataWKsAgg[,x], ', N = ', dataWKsAgg[,x+4]))
names(dataWKsAgg)[c(6:9)] <- c('WK4.html.tooltip', 'WK5.html.tooltip', 'WK6.html.tooltip', 'WK7.html.tooltip')
scrWKs <- gvisColumnChart(dataWKsAgg, xvar = 'Grps', yvar = c(c('Score.WK4', 'WK4.html.tooltip'), c('Score.WK5', 'WK5.html.tooltip'),
                                                              c('Score.WK6', 'WK6.html.tooltip'), c('Score.WK7', 'WK7.html.tooltip')),
                           options = list(isStacked = FALSE, width = 1000, height = 500,
                                          title = "Gemiddelde scores kennistoetsen per groep",
                                          hAxis = "{title: 'Groepen'}", vAxis = "{title: 'Gemiddelde'}"),
                           chartid = 'Chart5')
# plot(scrWKs)

### Stacked bar charts with distribution suff vs unsuff per groep
dataLoc <- na.omit(ScoresOverv[c(6,7,15)])
dataLoc$Grps <- paste0(substr(dataLoc$Locatie,1,1),'.',dataLoc$Groep)
dataLoc$Vold <- (dataLoc$Score.Schr >= 27.5)*1

dataLocAgg1 <- aggregate(Vold ~ Grps, data = dataLoc, FUN = 'sum')
dataLocAgg2 <- aggregate(Vold ~ Grps, data = dataLoc, FUN = 'length')
dataLocAgg3 <- cbind(dataLocAgg1, Onvold = dataLocAgg2[2] - dataLocAgg1[2])
names(dataLocAgg3) <- c('Groep', 'Vold', 'Onvold')
 
scrLoc1 <- gvisColumnChart(dataLocAgg3, xvar = 'Groep', yvar = c('Vold', 'Onvold'),
                       options = list(isStacked = TRUE, width = 500, height = 400, title = "Aantal voldoende vs onvoldoende per groep (zonder bonus)", hAxis = "{title: 'Groepen'}", vAxis = "{title: 'Aantal'}"), chartid = 'Chart1')

dataLoc2 <- na.omit(ScoresOverv[c(6,7,16)])
dataLoc2$Grps <- paste0(substr(dataLoc2$Locatie,1,1),'.',dataLoc2$Groep)
dataLoc2$Vold <- (dataLoc2$Eindscore >= 27.5)*1

dataLocAgg4 <- aggregate(Vold ~ Grps, data = dataLoc2, FUN = 'sum')
dataLocAgg5 <- aggregate(Vold ~ Grps, data = dataLoc2, FUN = 'length')
dataLocAgg6 <- cbind(dataLocAgg4, Onvold = dataLocAgg5[2] - dataLocAgg4[2])
names(dataLocAgg6) <- c('Groep', 'Vold', 'Onvold')

scrLoc2 <- gvisColumnChart(dataLocAgg6, xvar = 'Groep', yvar = c('Vold', 'Onvold'),
                          options = list(isStacked = TRUE, width = 500, height = 400, title = "Aantal voldoende vs onvoldoende per groep (met bonus)", hAxis = "{title: 'Groepen'}", vAxis = "{title: 'Aantal'}"), chartid = 'Chart2')

scrLoc <- gvisMerge(scrLoc1, scrLoc2, horizontal = TRUE)

### Stacked bar charts with distribution ' Vold' vs 'Onvold' per 'Opleiding'
dataOpl1 <- na.omit(ScoresOverv[c(1,15)])
dataOpl1 <- merge(ScoresAll[c(1,29)], dataOpl1, 
                 by.x = 'ID', by.y = 'ID', all.x = FALSE, all.y = FALSE,
                 sort = TRUE, 
                 incomparables = NULL)
dataOpl1 <- dataOpl1[dataOpl1$Opleiding %in% c('MBO', 'HAVO', 'VWO'),]
dataOpl1$Vold <- (dataOpl1$Score.Schr >= 27.5)*1

dataOplAgg1 <- aggregate(Vold ~ Opleiding, data = dataOpl1, FUN = 'sum')
dataOplAgg2 <- aggregate(Vold ~ Opleiding, data = dataOpl1, FUN = 'length')
dataOplAgg3 <- cbind(dataOplAgg1, Onvold = dataOplAgg2[2] - dataOplAgg1[2])
names(dataOplAgg3) <- c('Opleiding', 'Vold', 'Onvold')

dataOpl2 <- na.omit(ScoresOverv[c(1,16)])
dataOpl2 <- merge(ScoresAll[c(1,29)], dataOpl2, 
                  by.x = 'ID', by.y = 'ID', all.x = FALSE, all.y = FALSE,
                  sort = TRUE, 
                  incomparables = NULL)
dataOpl2 <- dataOpl2[dataOpl2$Opleiding %in% c('MBO', 'HAVO', 'VWO'),]
dataOpl2$Vold <- (dataOpl2$Eindscore >= 27.5)*1

dataOplAgg4 <- aggregate(Vold ~ Opleiding, data = dataOpl2, FUN = 'sum')
dataOplAgg5 <- aggregate(Vold ~ Opleiding, data = dataOpl2, FUN = 'length')
dataOplAgg6 <- cbind(dataOplAgg4, Onvold = dataOplAgg5[2] - dataOplAgg4[2])
names(dataOplAgg6) <- c('Opleiding', 'Vold', 'Onvold')

scrOpl1 <- gvisColumnChart(dataOplAgg3, xvar = 'Opleiding', yvar = c('Vold', 'Onvold'),
                           options = list(isStacked = TRUE, width = 600, height = 400, title = "Aantal voldoende vs onvoldoende per opleiding (zonder bonus)",
                                          hAxis = "{title: 'Opleiding'}", vAxis = "{title: 'Aantal'}"), chartid = 'Chart3')

scrOpl2 <- gvisColumnChart(dataOplAgg6, xvar = 'Opleiding', yvar = c('Vold', 'Onvold'),
                           options = list(isStacked = TRUE, width = 600, height = 400, title = "Aantal voldoende vs onvoldoende per opleiding (met bonus)",
                                          hAxis = "{title: 'Opleiding'}", vAxis = "{title: 'Aantal'}"), chartid = 'Chart4')
scrOpl <- gvisMerge(scrOpl1, scrOpl2, horizontal = TRUE)

scrGph <- gvisMerge(scrLoc, scrOpl, horizontal = FALSE)
# plot(scrGph)

### Scatter plot with 'Gemiddelde.Top3' vs 'Groep' per 'Opleiding'
dfOpl <- merge(ScoresOverv, ScoresAll[c(1,29)], 
                  by.x = 'ID', by.y = 'ID', all.x = FALSE, all.y = FALSE,
                  sort = TRUE, 
                  incomparables = NULL)
dfOpl <- dfOpl[dfOpl$Opleiding %in% c('MBO', 'HAVO', 'VWO'),]
dfOpl <- dfOpl[!is.na(dfOpl$Score.Schr),]
dfOpl$Groep <- as.factor(dfOpl$Groep)


ggplot(dfOpl, aes(x=Groep, y=Gemiddelde.Top3, color=Opleiding)) +
geom_point(shape=19, size=10, alpha = 3/4, position=position_jitter(width=0, height=1.2)) +
  scale_color_manual(values=c("red", "#0066CC", "#33CC33"))

### Scatter plot with 'Scr.Schr' vs 'Groep' per 'Opleiding'
dfOpl <- merge(ScoresOverv, ScoresAll[c(1,29)], 
               by.x = 'ID', by.y = 'ID', all.x = FALSE, all.y = FALSE,
               sort = TRUE, 
               incomparables = NULL)
dfOpl <- dfOpl[dfOpl$Opleiding %in% c('MBO', 'HAVO', 'VWO'),]
dfOpl <- dfOpl[!is.na(dfOpl$Score.Schr),]
dfOpl$Groep <- as.factor(dfOpl$Groep)

ggplot(dfOpl, aes(x=Groep, y=Score.Schr, color=Opleiding)) +
  geom_point(shape=19, size=10, alpha = 3/4, position=position_jitter(width=0, height=1.2)) +
  scale_color_manual(values=c("red", "#0066CC", "#33CC33"))

## Scatter plot with scores 'vs 'Groep' per 'Opleiding'
dfScrs <- merge(ScoresOverv, ScoresAll[c(1,29,30)], 
               by.x = 'ID', by.y = 'ID', all.x = FALSE, all.y = FALSE,
               sort = TRUE, 
               incomparables = NULL)
dfScrs <- dfScrs[dfScrs$Opleiding %in% c('MBO', 'HAVO', 'VWO'),]
dfScrs <- dfScrs[!is.na(dfScrs$Score.Schr),]
dfScrs$Groep <- as.factor(dfScrs$Groep)
# 
# p1 <- nPlot(Score.Schr ~ Gemiddelde.Top3,
#             group = 'Opleiding', 
#             data = dfScrs, 
#             type = 'scatterChart')
# 
# p1$chart(showControls = TRUE, tooltipContent = "#! function(key, x, y, e){return key + ' ' + e.point.Voor} !#")
# p1
# 
# # data
# df <- data.frame(x = dfScrs[12][[1]], y = dfScrs[15][[1]], s = dfScrs[17][[1]], z = dfScrs[2][[1]])
# 
# # create plot object
# p <- hPlot(y ~ x, data = df,  group = "s", type = "scatter", radius = 10)
# 
# # fix data format
# p$params$series[[1]]$data <- toJSONArray(df, json = F)
# 
# # add tooltip formatter
# p$tooltip(formatter = "#! function() {return(this.point.z + '<br/>' + '<b>Kennistoets: </b>' + this.point.x + '/10<br/>' + '<b>Schriftelijk: </b>' + this.y + '/50');} !#")
# p$colors('rgba(223, 83, 83, 1)', 'rgba(119, 152, 191, 1)', 'rgba(60, 179, 113, 1)')
# # show
# p

# This one is prettiest 1
df <- data.frame(Kennistoets = dfScrs[12][[1]], Schriftelijk = dfScrs[15][[1]]/5, Opleiding = dfScrs[17][[1]], Naam = dfScrs[2][[1]])
p1 <- rPlot(Schriftelijk ~ Kennistoets | Opleiding, data = df, color = 'Opleiding', 
            type = 'point',
            size = list(const = 8),
            tooltip = "#! function(item) {return(item.Naam + ' - Kennistoets: ' + item.Kennistoets + '/10 - Schriftelijk: ' + item.Schriftelijk + '/10');} !#"
            )
p1$addParams(height = 500, width = 900, legendPosition = 'none',
             title = "Resultaten CFA-BEC")
p1$guides(x = list(min = 0, max = 11))
p1$guides(y = list(min = 0, max = 11))

p1$save("ResOplNaam.html")

# This one is prettiest 2
dfScrs2 <- merge(ScoresOverv, ScoresAll[c(1,29,30)], 
                by.x = 'ID', by.y = 'ID', all.x = FALSE, all.y = FALSE,
                sort = TRUE, 
                incomparables = NULL)
dfScrs2 <- dfScrs2[!is.na(dfScrs2$Score.Schr),]
dfScrs2$Groep <- as.factor(dfScrs2$Groep)
df <- data.frame(Kennistoets = dfScrs2[12][[1]], Schriftelijk = dfScrs2[15][[1]]/5, Locatie = dfScrs2[6][[1]], Groep = dfScrs2[7][[1]], Naam = dfScrs2[2][[1]])
df$Grps <- paste0(substr(df$Locatie,1,1),'.',df$Groep)
df <- df[order(df$Grps),]
p2 <- rPlot(Schriftelijk ~ Kennistoets | Grps, data = df, color = 'Groep', 
            type = 'point',
            size = list(const = 8),
            tooltip = "#! function(item) {return(item.Naam + ' - Kennistoets: ' + item.Kennistoets + '/10 - Schriftelijk: ' + item.Schriftelijk + '/10');} !#"
)
p2$addParams(height = 500, width = 900, legendPosition = 'none',
             title = "Resultaten kennistoets vs schriftelijk (zonder bonus)")
p2$guides(x = list(min = 0, max = 11))
p2$guides(y = list(min = 0, max = 11))

p2$save("ResGrpNaam.html")

# With regression
df <- data.frame(Kennistoets = dfScrs[12][[1]], Schriftelijk = dfScrs[15][[1]]/5, Opleiding = dfScrs[17][[1]], Naam = dfScrs[2][[1]])
model1 <- lm(Schriftelijk ~ Kennistoets, data = df)
summ <- summary(model1)
linef <- paste0('regressiefunctie: y = ', round(summ$coefficient[1,1], 2),' + ', round(summ$coefficient[2,1], 2), ' * x; met R-sq = ', round(summ$r.squared,2))
#toolt <- paste0("#! function(item) {return('",linef,"');} !#")'
dat = fortify(model1)
names(dat) = gsub('\\.', '_', names(dat))

p3 <- rPlot(Schriftelijk ~ Kennistoets, data = df, color = 'Opleiding', 
            type = 'point',
            size = list(const = 8),
            tooltip = "#! function(item) {return(item.Naam +
            ' - Kennistoets: ' + item.Kennistoets + '/10 - Schriftelijk: ' + item.Schriftelijk + '/10');} !#"
)
p3$layer(y = '_fitted', data = dat, copy_layer = T, type = 'line',
         color = list(const = 'LightBlue'),
         size = list(const = 2)
         #, tooltip = "#! function(item1) {return(item1._fitted);} !#"
         )
p3$addParams(height = 500, width = 900, legendPosition = 'right',
             title = linef)
p3$guides(x = list(min = 0, max = 11, title = 'x: Kennistoets'))
p3$guides(y = list(min = 0, max = 11, title = 'y: Schriftelijk'))

p3$save("ResOplRegr.html")

# With regression 2
df <- data.frame(Kennistoets = dfScrs[12][[1]], Schriftelijk = dfScrs[15][[1]]/5, Opleiding = dfScrs[17][[1]], Naam = dfScrs[2][[1]])
model1 <- lm(Schriftelijk ~ Kennistoets, data = df)
summ <- summary(model1)
linef <- paste0('regressiefunctie: y = ', round(summ$coefficient[1,1], 2),' + ', round(summ$coefficient[2,1], 2), ' * x; met R-sq = ', round(summ$r.squared,2))
#toolt <- paste0("#! function(item) {return('",linef,"');} !#")'
dat = fortify(model1)
names(dat) = gsub('\\.', '_', names(dat))

p31 <- rPlot(Schriftelijk ~ Kennistoets, data = df, color = 'Opleiding', 
            type = 'point',
            size = list(const = 8),
            tooltip = "#! function(item) {return('Kennistoets: ' + item.Kennistoets + '/10 - Schriftelijk: ' +
            item.Schriftelijk + '/10');} !#"
            )
p31$layer(y = '_fitted', data = dat, copy_layer = T, type = 'line',
         color = list(const = 'LightBlue'),
         size = list(const = 2)
         #, tooltip = "#! function(item1) {return(item1._fitted);} !#"
)
p31$addParams(height = 500, width = 900, legendPosition = 'right',
             title = linef)
p31$guides(x = list(min = 0, max = 11, title = 'x: Kennistoets'))
p31$guides(y = list(min = 0, max = 11, title = 'y: Schriftelijk'))


# Scatter BGA
df2 <- data.frame(ID = dfScrs2[1][[1]], Kennistoets = dfScrs2[12][[1]], Schriftelijk = dfScrs2[15][[1]]/5,
                  Locatie = dfScrs2[6][[1]], Groep = dfScrs2[7][[1]], BGA = dfScrs2[18][[1]], Naam = dfScrs2[2][[1]])
df2$Grps <- paste0(substr(df2$Locatie,1,1),'.',df2$Groep)
df2 <- df2[df2$BGA %in% c('Ja', 'Nee'),]
df2 <- df2[order(df2$Grps),]
p4 <- rPlot(Schriftelijk ~ Kennistoets | Grps, data = df2, color = 'BGA', 
            type = 'point',
            size = list(const = 8),
            tooltip = "#! function(item) {return(item.Naam +
            ' - Kennistoets: ' + item.Kennistoets + '/10 - Schriftelijk: ' + item.Schriftelijk + '/10');} !#"
            )
p4$guides(color = list(scale = "#! function(value){
      color_mapping = {Ja: '#1E90FF',Nee:'#FF0000'}
      return color_mapping[value];                  
                       } !#"))
p4$addParams(height = 500, width = 900, legendPosition = 'right')
p4$guides(x = list(min = 0, max = 11, title = 'x: Kennistoets'))
p4$guides(y = list(min = 0, max = 11, title = 'y: Schriftelijk'))

# df <- data.frame(x = dfScrs[12][[1]], y = dfScrs[15][[1]], s = dfScrs[17][[1]], z = dfScrs[2][[1]])
# p2 <- nvd3Plot(y ~ x, data = df, group = 's', 
#                type = 'scatterChart'
#                )
# p2$chart(showControls = TRUE, tooltipContent = "#! function(key, x, y, e){return key + ' ' + e.point.z} !#")
# p2

# Bar chart BGA
df3 <- na.omit(ScoresOverv[c(1,12,15)])
df3 <- merge(ScoresAll[c(1,29,30)], df3, 
                 by.x = 'ID', by.y = 'ID', all.x = FALSE, all.y = FALSE,
                 sort = TRUE, 
                 incomparables = NULL)

df3 <- df3[df3$Tent.BClust %in% c('Ja', 'Nee'),]
df3$Score.Schr <- df3$Score.Schr/5
df3$Schr.Vold <- (df3$Score.Schr >= 5.5)*1

# df3$Schr.Vold <- as.numeric(df3$Schr.Vold)-1
df3Agg1 <- aggregate(Schr.Vold ~ Tent.BClust, data = df3, FUN = 'sum')
df3Agg2 <- aggregate(Schr.Vold ~ Tent.BClust, data = df3, FUN = 'length')
df3Agg3 <- cbind(df3Agg1, Schr.Onvold = df3Agg2[2] - df3Agg1[2])
names(df3Agg3) <- c('BGA', 'Vold', 'Onvold')

scrBGA <- gvisColumnChart(df3Agg3, xvar = 'BGA', yvar = c('Vold', 'Onvold'),
                          options = list(isStacked = TRUE, width = 600, height = 400,
                                         title = "Aantal voldoende/onvoldoende schriftelijk (zonder bonus)",
                                         hAxis = "{title: 'BGA gehaald'}",
                                         vAxis = "{title: 'Aantal'}"), chartid = 'Chart6')

plot(scrBGA)

## Logistic regression
dataLog <- na.omit(ScoresOverv[c(1,12,15)])
dataLog <- merge(ScoresAll[c(1,29,30)], dataLog, 
                  by.x = 'ID', by.y = 'ID', all.x = FALSE, all.y = FALSE,
                  sort = TRUE, 
                  incomparables = NULL)
dataLog <- dataLog[dataLog$Opleiding %in% c('MBO', 'HAVO', 'VWO'),]
dataLog <- dataLog[dataLog$Tent.BClust %in% c('Ja', 'Nee'),]
dataLog$Score.Schr <- dataLog$Score.Schr/5
dataLog$Schr.Vold <- (dataLog$Score.Schr >= 5.5)*1
dataLog$KT.Vold <- (dataLog$Gemiddelde.Top3 >= 5.5)*1
dataLog$Opleiding <- as.factor(dataLog$Opleiding)
dataLog$Tent.BClust <- as.factor(dataLog$Tent.BClust)
dataLog$Schr.Vold <- as.factor(dataLog$Schr.Vold)
dataLog$KT.Vold <- as.factor(dataLog$KT.Vold)

model2 <- glm(Schr.Vold ~ Opleiding + Tent.BClust + Gemiddelde.Top3, data = dataLog, family = binomial())
model3 <- glm(Schr.Vold ~ Tent.BClust, data = df3, family = binomial())
p.Schr.v.Bclust.o <- 1/(1+exp(-1*(model3$coefficients[[1]]+model3$coefficients[[2]])))
p.Schr.v.BClust.v <- 1/(1+exp(-1*(model3$coefficients[[1]])))