source('Globals.R')

# Define UI for Quiz application
shinyUI(fluidPage(
  
  titlePanel('Kennistoets Week 4'),

  fluidRow(

    # Questions
    column(8,
           h5('Per vraag kunnen 2 punten worden verdiend. Voor ieder fout 
               antwoord wordt het aantal te verdienen punten verminderd met 1 punt.
              De antwoorden worden geregistreerd nadat op \'Verzenden\' is gedrukt.
              Alleen de eerste twee pogingen worden bewaard.'),
           
           uiOutput("ui")),
           
    # user data and results
        
    column(4, list(
                   h4('Gegevens'),
                   textInput(inputId="userFirst", label = "Voornaam", value = ""),
                   textInput(inputId="userLast1", label = "Tussenvoegsels", value = ""),
                   textInput(inputId="userLast2", label = "Achternaam", value = ""),
                   textInput(inputId="userNr", label = "Studentnummer", value = ""),
                   selectInput(inputId="userLoc", label = "Locatie", 
                               choices = list("Kies"= 0,"Arnhem" = "A", "Nijmegen" = "N"), 
                               selected = 0),
                   selectInput(inputId="userGrp", label = "Groep", 
                               choices = list("Kies"= 0, "C01" = "C01", "C02" = "C02", "C03" = "C03",
                                              "C04" = "C04", "C05" = "C05", "C06" = "C06",
                                              "Overig" = "Overig"), 
                               selected = 0),
                   textInput(inputId="userPass", label = "Wachtwoord", value = "--"),
                   h4('Resultaten'),
                   verbatimTextOutput('value'),
                   verbatimTextOutput('result1'),
                   verbatimTextOutput('result2'),
                   verbatimTextOutput('result3'),
                   verbatimTextOutput('result4'),
                   verbatimTextOutput('result5'),
                   verbatimTextOutput('result6'),
                   verbatimTextOutput('result7'),
                   verbatimTextOutput('result8'),
                   verbatimTextOutput('scr'),
                   verbatimTextOutput('user')
                              
                  )   
          )
         )
))
    