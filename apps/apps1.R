library(plyr)
qtn <<- c("What is the capital of Love?",
          "In what country did Shakespeare live and work?",
          "What's the purpose of human life?")

answ <<- c("Paris mon Dieu!", "Great Britain", "Paying taxes")
opt1 <<- c("London you fool!", "USA", "Love")
opt2 <<- c("Berlin ohnehin!", "Italy", "See Napels and die")
opt3 <<- c("My place cherry!", "Oosterbeek (where's that!?)", "God only knows!")
answers <<- data.frame(answ, opt1, opt2, opt3)  


p <- sample(1:3,size = 1)
q <- sample(1:4, size = 4)


output$ui <- renderUI({
  radioButtons('answ', paste('Question -',qtn[p]),
               c(paste(answers[p,q[1]]),
                 paste(answers[p,q[2]]),
                 paste(answers[p,q[3]]),
                 paste(answers[p,q[4]])), selected = 0)
})

chkQuestion <- function(answer, correct) {
  message <- if(answer == correct) 'Correct' else 'False'
  return(message)}

rltInput1 <- reactive({try_default(chkQuestion(input$answ, answers$answ[p]),
                                           default = 'Choose', quiet = TRUE)
                      })
output$result <- renderText({paste('Result:',rltInput1())})

