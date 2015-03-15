library(shiny)

shinyServer(function(input, output) { 
  
  source('WK4Analysis.R')
  
  Loc <- reactive({as.vector(input$locatie)})
  
  Grp <- reactive({input$groep})
  
    
  output$tableResults <- renderDataTable({
    
    subset(resultsClean, Locatie %in% Loc() & Groep %in% Grp())},
    options = list(aLengthMenu = c(30, 60, 100), iDisplayLength = 60),
    
  )
  
  
})