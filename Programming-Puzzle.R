library(stringi)
library(gtools)
library(dplyr)
# Combine 1,3,6,6 to make 30. 
# - You can add, substract, multiply, divide and use parentheses.
# - Use each digit once.
# - NO: exponentiation, modulo, logs, factorials, decimals, rotating nums (9s to make 6),
# two digits together (1 and 4 aren't 14)


# Generate all the permutations of 6,6,1,3. In this case the function permutations
# does not allow repeated numbers so whe use 0 for the second 6 
n<-permutations(4,4,c("6","0","3","1"))
# then substitute the 0s for 6s.
n[n=="0"]<-"6"
# Generate all the permutations for the symbols + - * / with possibibility for 
# repetitions
s<-permutations(4,3,c("+","-","*","/"), repeats.allowed = TRUE)

# Apply all the permutations of symbols in each permutation of numbers
df<-data.frame(strings=character())
for (i in 1:nrow(n)) {
  for (j in 1:nrow(s)) {
    string<-paste0(n[i,1], s[j,1], n[i,2], s[j,2], n[i,3], s[j,3], n[i,4])
    df[nrow(df)+1,]<-string
  } 
}

# Now we have the 1536 possible combinations of numbers and symbols. To include the
# parenthesis we will duplicate(because without parenthesis is valid) all these combinations 
# and get the different combinations with parentheses. 
# Because parenthesis can only be used with two or three numbers and they cannot be mixed it 
# is easy to see that there are only 6 combinations of parentheses uses in this problem:
# (A+B)+C+D, A+(B+C)+D, A+B+(C+D), (A+B+C)+D, A+(B+C+D), (A+B)+(C+D)

lengthDf<-nrow(df)
for(k in 1:lengthDf){
  # (A+B)+C+D
  val1<-df[k,]
  stri_sub(val1, 1, 0) <- "("
  stri_sub(val1, 5, 4) <- ")"
  df[nrow(df)+1,]<-val1
  
  # A+(B+C)+D 
  val2<-df[k,]
  stri_sub(val2, 3, 2) <- "("
  stri_sub(val2, 7, 6) <- ")"
  df[nrow(df)+1,]<-val2
  
  # A+B+(C+D)
  val3<-df[k,] 
  stri_sub(val3, 5, 4) <- "("
  stri_sub(val3, 9, 8) <- ")"
  df[nrow(df)+1,] <- val3
  
  # A+(B+C+D)
  val4<-df[k,] 
  stri_sub(val4, 3, 2) <- "("
  stri_sub(val4, 9, 8) <- ")"
  df[nrow(df)+1,]<-val4
  
  # (A+B+C)+D
  val5<-df[k,] 
  stri_sub(val5, 1, 0) <- "("
  stri_sub(val5, 7, 6) <- ")"
  df[nrow(df)+1,]<-val5
  
  #(A+B)+(C+D)
  val6<-df[k,] 
  stri_sub(val6, 1, 0) <- "("
  stri_sub(val6, 5, 4) <- ")"
  stri_sub(val6, 7, 6) <- "("
  stri_sub(val6, 11, 10) <- ")"
  df[nrow(df)+1,]<-val6
  
}

dfFinal<-data.frame(operations=character())
lengthDf<-nrow(df)
# Check all the expressions to find the correct ones
for (i in 1:lengthDf) {
  x<-df[i,]
  x1<-parse(text = x)
  sol<-eval(x1)
  if(sol==30){
    dfFinal[nrow(dfFinal)+1,] <- x
  }
}
# Because of the fix of 0s as second 6s in the first step, there are some repeated
# solutions. So to get the unique values
dfFinal <- dfFinal %>% unique()
View(dfFinal)


