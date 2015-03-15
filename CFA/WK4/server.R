#Load all necessary supporting files
library(shiny)
library(plyr)
source('Globals.R')
source('Setup.R')

#Set up server
shinyServer(function(input, output, session) {
  
  ## Choose set of questions by sampling
  m <- sample(4, size =1)
  questionsMCSet <<- questionsMC[questionsMC$Set == m,]
  questionsOpenSet <<- questionsOpen[questionsOpen$Set == m,]
  
              
  ##Sample n MC and open questions from database. For each MC question randomly choose
  ##order of answers. Save correct answers in reactive value variables.
  
  n <- sample(c(1:length(questionsMCSet$Vraag)), size =4)
  a <- sample(c(6:9), size =4, replace=FALSE)
  b <- sample(c(6:9), size =4, replace=FALSE)
  d <- sample(c(6:9), size =4, replace=FALSE)
  e <- sample(c(6:9), size =4, replace=FALSE)
  
  cat('m = ', m, '\n', 'n = ', n, '\n')
  vraagMC <<- questionsMCSet[n,]
  vraagOpen <<- questionsOpenSet[n,]
  
  values <- reactiveValues()
  values[['corr1']] <- vraagMC$Antwoord[1]
  values[['corr2']] <- vraagMC$Antwoord[2]
  values[['corr3']] <- vraagMC$Antwoord[3]
  values[['corr4']] <- vraagMC$Antwoord[4]
  values[['corr5']] <- vraagOpen$Antwoord[1]
  values[['corr6']] <- vraagOpen$Antwoord[2]
  values[['corr7']] <- vraagOpen$Antwoord[3]
  values[['corr8']] <- vraagOpen$Antwoord[4]
  
  ## Compare input with passwords. If correct: build user interface with 5 MC and 5 open questions. Save choices
  ##in reactive variables. Set initial choice of radio buttons to zero (no choice)
  
  passVal <- reactive(paste0(input$userLoc, input$userGrp, input$userPass))
  
  output$ui <- renderUI(if(passVal() %in% c("NC01feinMan", "NC02bartRussel", "NC03tomPiketti", "NC04mmeCurry",
                                            "NC05GerZalm", "AC01LosAalamos", "AC02nielsBoor", "AOverigbankiMoen",
                                            "NOverignellsonMandella")) {
    {fluidPage(
        
    radioButtons('answ1', paste('V1 -',vraagMC$Vraag[1]),
                 c(vraagMC[1,a[1]],
                   vraagMC[1,a[2]],
                   vraagMC[1,a[3]],
                   vraagMC[1,a[4]]), selected = 0),
    
    radioButtons('answ2', paste('V2 -',vraagMC$Vraag[2]),
                 c(vraagMC[2,b[1]],
                   vraagMC[2,b[2]],
                   vraagMC[2,b[3]],
                   vraagMC[2,b[4]]), selected = 0),
    
    radioButtons('answ3', paste('V3 -',vraagMC$Vraag[3]),
                 c(vraagMC[3,d[1]],
                   vraagMC[3,d[2]],
                   vraagMC[3,d[3]],
                   vraagMC[3,d[4]]), selected = 0),
    
    radioButtons('answ4', paste('V4 -',vraagMC$Vraag[4]),
                 c(vraagMC[4,e[1]],
                   vraagMC[4,e[2]],
                   vraagMC[4,e[3]],
                   vraagMC[4,e[4]]), selected = 0),
    
    textInput('answ5', paste('V5 -',vraagOpen$Vraag[1]), value = ''),
    
    textInput('answ6', paste('V6 -',vraagOpen$Vraag[2]), value = ''),
    
    textInput('answ7', paste('V7 -',vraagOpen$Vraag[3]), value = ''),
    
    textInput('answ8', paste('V8 -',vraagOpen$Vraag[4]), value = ''),
    
    actionButton('goButton', 'Verzenden')
  )
    }
    })
  
  #Build server output
  
  ##Check questions and put results (Correct/False) in reactive variable.
  ##Initial choice (no choice) is neutral and equals 'Choose'
  rltInput1 <- reactive({input$goButton
                         isolate(try_default(chkQuestion(input$answ1, values$corr1,1),
                                     default = 'Leeg', quiet = TRUE))
                         })
  rltInput2 <- reactive({input$goButton
                         isolate(try_default(chkQuestion(input$answ2, values$corr2,2),
                                             default = 'Leeg', quiet = TRUE))
                         })
  rltInput3 <- reactive({input$goButton
                         isolate(try_default(chkQuestion(input$answ3, values$corr3,3),
                                             default = 'Leeg', quiet = TRUE))
                         })
  
  rltInput4 <- reactive({input$goButton
                         isolate(try_default(chkQuestion(input$answ4, values$corr4,4),
                                             default = 'Leeg', quiet = TRUE))
  })
  
  rltInput5 <- reactive({input$goButton 
                         isolate(try_default(chkQuestionOpen(input$answ5, values$corr5),
                                             default = 'Leeg', quiet = TRUE))
                         })
                         
  rltInput6 <- reactive({input$goButton 
                         isolate(try_default(chkQuestionOpen(input$answ6, values$corr6),
                                            default = 'Leeg', quiet = TRUE))
                         }) 
                         
  rltInput7 <- reactive({input$goButton 
                         isolate(try_default(chkQuestionOpen(input$answ7, values$corr7),
                                            default = 'Leeg', quiet = TRUE))
                         })
  
  rltInput8 <- reactive({input$goButton 
                         isolate(try_default(chkQuestionOpen(input$answ8, values$corr8),
                                             default = 'Leeg', quiet = TRUE))
  })
  
  ##Print results
  output$result1 <- renderText({paste('V1:',rltInput1())})
  output$result2 <- renderText({paste('V2:',rltInput2())})
  output$result3 <- renderText({paste('V3:',rltInput3())})
  output$result4 <- renderText({paste('V4:',rltInput4())})
  output$result5 <- renderText({paste('V5:',rltInput5())})
  output$result6 <- renderText({paste('V6:',rltInput6())})
  output$result7 <- renderText({paste('V7:',rltInput7())})
  output$result8 <- renderText({paste('V8:',rltInput8())})
  
  ##Calculate points left per MC question and only add them when answer is correct
  scrInput1 <- reactive({scrQuestion(rltInput1(),1)*(rltInput1()=='Correct')})
  scrInput2 <- reactive({scrQuestion(rltInput2(),2)*(rltInput2()=='Correct')})
  scrInput3 <- reactive({scrQuestion(rltInput3(),3)*(rltInput3()=='Correct')})
  scrInput4 <- reactive({scrQuestion(rltInput4(),4)*(rltInput4()=='Correct')})
  scrInput5 <- reactive({scrQuestion(rltInput5(),5)*(rltInput5()=='Correct')})
  scrInput6 <- reactive({scrQuestion(rltInput6(),6)*(rltInput6()=='Correct')})
  scrInput7 <- reactive({scrQuestion(rltInput7(),7)*(rltInput7()=='Correct')})
  scrInput8 <- reactive({scrQuestion(rltInput8(),8)*(rltInput8()=='Correct')})
  
  ##Calculate total points left and print result
  output$scr <- renderText({paste0('Je score is: ',scrInput1() + scrInput2() + scrInput3() + scrInput4() + scrInput5() + scrInput6() + scrInput7() + scrInput8(),
                                   ' (of ', round((scrInput1() + scrInput2() + scrInput3() + scrInput4() + scrInput5() + scrInput6() + scrInput7() + scrInput8())*100/16,0),
                                   '%)')})

  ##Prepare user data and print
  userData <- reactive({input$goButton
                         c(input$userFirst, input$userLast1, input$userLast2, input$userNr, input$userLoc, input$userGrp, input$userPass)
                        })
  output$user <- renderText(paste0("Naam: ",paste(userData()[1], userData()[2], userData()[3]),", ID: ",userData()[4]))
  
  ##Assess number of trials and print
  trials <- reactive({input$goButton})
  output$value <- renderText(paste0('Pogingen: ', trials()))
  
  urlText <- renderText({
    paste(sep = "",
          session$clientData$url_protocol, "//",
          session$clientData$url_hostname, ":",
          session$clientData$url_port,"",
          session$clientData$url_pathname
    )
  })
  
  ##Compare studentID to history and set trials() to empty if ID is in history
  checkUser <- reactive(input$userNr %in% history$Nr)
  
  ##Check whether number of trials variable is empty and if not save data to csv
  observe({if(length(trials()) != 0) {
          if(trials() != 0 & trials() < 3 & checkUser() != TRUE) {
          quiz <- data.frame(Sys.time(), urlText(), session$clientData$url_search, 
                             userData()[1], userData()[2], userData()[3], userData()[4], userData()[5], userData()[6], userData()[7], trials(),
                             scrInput1(), scrInput2(),scrInput3(), scrInput4(), scrInput5(), scrInput6(), scrInput7(), scrInput8() ,m, n[1], n[2], n[3], n[4])
          write.table(quiz, file='results.csv', append=TRUE, sep=",", row.names=FALSE,
                      col.names=FALSE)
          }
          }
          })
  })

  