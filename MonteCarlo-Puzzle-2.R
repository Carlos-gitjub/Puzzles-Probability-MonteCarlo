# date: '2022-06-27'
# You have a bag with a 6-sided dice and a 12-sided dice.
# You pull one out at random, roll it, and get the result 3.
# What is the probability that you pull out the 6-sided dice?

library(dplyr)
N<-10^4

calculateProbability <- function(){
  df <- data.frame("value"=integer(),"diceType"=vector())
  for(i in 1:N){
    diceType <- sample(c("6-sided", "12-sided"),1, replace = TRUE)
    if(diceType=="6-sided"){
      value <- sample(1:6,1,replace = TRUE)
    }else{
      value <- sample(1:12,1,replace = TRUE)
    }
    df[nrow(df)+1,] <- list(value,diceType)
  }
    
  x6s<- subset(df, value==3 & diceType=="6-sided") %>% nrow()
  x12s<- subset(df, value==3 & diceType=="12-sided") %>% nrow()
  probability<-x6s/(x12s+x6s)
  
  cat(paste("The probability is",round(probability, digits = 2)))
}

calculateProbability()

