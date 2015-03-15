shinyUI(fluidPage(
  
  titlePanel('Module CFA - Scores'),
  
  fluidRow(
    
    column(2,
           checkboxGroupInput('locatie', 
                              label = h3('Locatie'), 
                              choices = list('Arnhem' = 'Arnhem', 
                                             'Nijmegen' = 'Nijmegen'),
                              selected = c('Arnhem','Nijmegen')),
           
           checkboxGroupInput('groep', 
                              label = h3('Groep'), 
                              choices = list('C01' = 'C01', 
                                             'C02' = 'C02',
                                             'C03' = 'C03',
                                             'C04' = 'C04',
                                             'C05' = 'C05',
                                             'C06' = 'C06',
                                             'Overig' = 'Overig'),
                              selected = c('C01', 'C02', 'C03', 'C04', 'C05', 'C06', 'Overig'))),
    column(10,
 dataTableOutput('tableResults')
             
    )
  )
))
