---
title       : Quizii
subtitle    : A quiz application build in Shiny
author      : Witek ten Hove
job         : Instructor
framework   : io2012   # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : [mathjax, bootstrap, quiz]            # {mathjax, quiz, bootstrap}
mode        : standalone # {standalone, draft, selfcontained}
logo        : Logo_short.png

--- &radio

## 1. A quiz in Slidify

_(Sorry but the quiz widget only seems to work in io2012 )_

A simple quiz question: $sin(2pi)=$

1. _0_
2. 1
3. -1
4. $\infty$

*** .hint

![alt text](assets/img/unit.png)

*** .explanation

The sinus is the vertical distance of a point on a unit circle.

---

## 2. Pretty neat but ...


* Can we get a random question from a set of questions?

YES, we actually can with R!


```r
question <<- c("What is the capital of Love?", "In what country did Shakespeare live and work?", "What's the purpose of human life?")
correct <<- c("Paris, mon Dieu!", "Great Britain", "Paying taxes")
alt1 <<- c("London, you fool!", "USA", "Love")
alt2 <<- c("Berlin, ohnehin!", "Italy", "See Napels and die")
alt3 <- c("My place, cherry!", "Oosterbeek (where's that!?)", "God only knows!")
hint <<- c("Another croissant, ma cherie?", "It'wasn't in the USSR", "This hint costs you 100$ incl. VAT")
answer <<- c("Does Love need a place?", "Where they eat beans at breakfast", "Ask your local tax authority")
n <<- sample(1:3,size=1)
```
A sampled quiz question: `question[n]`



--- &radio

## 3. Let's see ...


A sampled quiz question: What's the purpose of human life?

1. _Paying taxes_
2. Love
3. See Napels and die
4. God only knows!

*** .hint

This hint costs you 100$ incl. VAT

*** .explanation

Ask your local tax authority

---

## 4. So, yes we can!

* But can we randomize the order of the answers?
* Not in Slidify
* Enter Shiny!


```r
 #This code goes into the server side of Shiny (app*.R)/ UI goes into Slide

library(plyr)
# Build data frame of Q&A
qtn <<- c("What is the capital of Love?",
          "In what country did Shakespeare live and work?",
          "What's the purpose of human life?")

answ <<- c("Paris mon Dieu!", "Great Britain", "Paying taxes")
opt1 <<- c("London you fool!", "USA", "Love")
opt2 <<- c("Berlin ohnehin!", "Italy", "See Napels and die")
opt3 <<- c("My place cherry!", "Oosterbeek (where's that!?)", "God only knows!")
answers <<- data.frame(answ, opt1, opt2, opt3)  

# Sample row and column coordinates
p <- sample(1:3,size = 1)
q <- sample(1:4, size = 4)

# Build radiobuttons for UI using sampled Q&A
output$ui <- renderUI({
  radioButtons('answ', paste('Question -',qtn[p]),
               c(paste(answers[p,q[1]]),
                 paste(answers[p,q[2]]),
                 paste(answers[p,q[3]]),
                 paste(answers[p,q[4]])), selected = 0)
})

# Function to check answer and returning result
chkQuestion <- function(answer, correct) {
  message <- if(answer == correct) 'Correct' else 'False'
  return(message)}

# Push result to server output
rltInput1 <- reactive({try_default(chkQuestion(input$answ, answers$answ[p]),
                                           default = 'Choose', quiet = TRUE)
                      })
output$result <- renderText({paste('Result:',rltInput1())})
```

---

## 5. Not entirely Shiny,

But so you get an idea. Publishing Slidies with built in Shinies is maybe possible, but I couldn't figure it out yet. Runs smoothly when used locally, though.




A scrambled quiz question: What's the purpose of human life?

1. Paying taxes
2. See Napels and die
3. God only knows!
4. Love

And this is how it looks as a standalone [app](https://tenhove.shinyapps.io/Quizii)
