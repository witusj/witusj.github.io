# Read records from CSV file, set each column to correct class
resultsRaw <- read.csv(file='../WK4/results.csv', colClasses = c(rep('character', each = 7), rep('factor', each = 3),
                                                                 rep('integer', each = 14)), header = TRUE)
resultsRaw$Time <- strptime(resultsRaw$Time, format = "%Y-%m-%d %H:%M:%S")

# Convert Passwords
resultsRaw$Pass <- paste0(resultsRaw$Loc, resultsRaw$Grp ,resultsRaw$Pass)

# Only select records with passwords matching elements of given set
resultsRaw <- resultsRaw[resultsRaw$Pass %in% c("NC01feinMan", "NC02bartRussel", "NC03tomPiketti", "NC04mmeCurry",
                                                "NC05GerZalm", "AC01LosAalamos", "AC02nielsBoor", "AOverigbankiMoen",
                                                "NOverignellsonMandella"),]

# Create new variable with sum of scores
resultsRaw$Sum <- rowSums(resultsRaw[, c(12:19)])
resultsRaw[, c(12:19, 25)]

# Change factor level names of Loc
levels(resultsRaw$Loc) <- c("Arnhem", "Nijmegen")

resultsRaw$Score <- round(10*resultsRaw$Sum / 16, 1)
resultsClean <- resultsRaw[c(4, 5, 6, 7, 8, 9, 11:19, 25, 26)]
names(resultsClean) <- c("Voornaam", "Tussen", "Achternaam", "Nummer", "Locatie", "Groep", "Poging", "V1", "V2", "V3",
                         "V4", "V5", "V6", "V7", "V8", "Som", "Score")
resultsClean <- resultsClean[order(resultsClean$Achternaam),]
rownames(resultsClean) <- NULL
