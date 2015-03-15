source('Globals.R')

# Function to check MC question
chkQuestion <- function(answer, correct, index) {
  message <- if(answer == correct) 'Correct' else 'Onjuist'
  return(message)}

# Function to check open question
chkQuestionOpen <- function(answer, correct) {
  message <- if(grepl(correct, answer, perl=TRUE)) 'Correct' else if(answer != '') 'Onjuist' else 'Leeg'
  return(message)}

# Function to calculate score / needs the setup of a vector 'tscore'
# with max scores per question
scrQuestion <- function(result, i) {
  vscore <- (result == 'Onjuist') * -1
  tscore[i] <<- tscore[i] + vscore
  return(as.numeric(max(0,tscore[i])))}
