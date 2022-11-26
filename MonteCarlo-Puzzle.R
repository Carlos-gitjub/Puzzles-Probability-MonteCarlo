# Should you bet?

# You have an X amount of money in the bank and you are thinking of playing a 
# game. You would throw a six-sided dice and:  
#   - If 1, you pay 50% of your current money.
# 
#   - If 6, you recieve 50% of your current money. 
# 
#   - If 2,3,4 or 5, you recieve 5% of your current money.
# 
#   - The dice is thrown 300 times.
###########
options(scipen = 0, digits = 4) #delete scientific notation
X <- 10^6 # inicial money
P <- 100  # number of times the game is played

play <- function(money){
  throws<-round(runif(300,1,6))
  for(pitch in throws){
    if(pitch == 1){
      money <- money - 0.5*money
    } 
    if(pitch == 6){
      money <- money + 0.5*money
    }
    if(pitch %in% c(2,3,4,5)){
      money <- money + 0.05*money
    }
  }
  return(money)
}
games <- sort(replicate(P, play(X))); games

# Plot
plot(games)
# Plot with adjusted scale
library(ggplot2)
dfgames <- data.frame(games)
ggplot(dfgames) +
  geom_point(aes(1:P,games)) +
    scale_y_log10() +
  geom_hline(aes(yintercept=X), color="red")
# Stripchart
stripchart(games)
# Range of values
range(games)
# expected rate of return for each throw knowing that:
# probability of 1/6 for a return of 50%
# probability of 4/6 for a return of 05%
# probability of 1/6 for a return of -50%
ER <- round(((1/6)*0.5+(4/6)*0.05-(1/6)*0.5)*100, 2)
cat(paste0("The expected rate of return for each throw is: ", ER, "%"))
# Even with a 3.3% expected gain for each throw, is not safe to play as we 
# would see:

t<-table(games<X)
percentageOfDefeats<- (t[[2]] / P) * 100
cat(paste0("Percentage of lost games: ", percentageOfDefeats,"%"))

t2<-table(games<=(X/2))
percentageOfDefeatsWithHalfTheMoneyOrLess <- (t2[[2]] / P) * 100
cat(paste0("Porcentaje of lost games that left with half of the initial money or less: ", percentageOfDefeatsWithHalfTheMoneyOrLess,"%"))

# CONCLUSION: No is not a safe bet to play:  aprox 25% of the times you lose half of your money
