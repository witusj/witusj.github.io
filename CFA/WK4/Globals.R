#Load questions
questions <- read.csv(file='Vragen.csv', colClasses = 'character', header = TRUE)
questionsMC <- questions[questions$Onderwerp == 'Financial Accounting' & questions$Subcategorie == 'Kengetallen' & questions$Type == 'MC',]
questionsOpen <- questions[questions$Onderwerp == 'Financial Accounting' & questions$Subcategorie == 'Kengetallen' & questions$Type == 'Open',]

history <- read.csv(file='results.csv', colClasses = 'character', header = TRUE)