### Test script: Example Wager Results
### 8/16/20
### Description: Method to determine the profit/losses of selecting certain outcomes
###  of different sports events to be bet on from all possible combinations.
###



# Dataframe of the lines of games to be wagered on
odds.df <- data.frame(underdog = c(150, 200, 110, 130, 110),
                      favorite = c(-170, -220, -130, -150, -130))

# Your selection of wagers from the events
picks <- c(1,1,2,2,1)
# Winners after events are completed
winners <- c(1,2,2,1,1)
# Losers of event
losers  <- sapply(winners, function(x) ifelse(x == 1, y <- 2, y <- 1))

# Dataframe for indexing
df_1or2 <- matrix(c(1,2), nrow(odds.df), 2, byrow = TRUE)

# All possible permutations of 3 to 8 events
#Permut = n^r #e.g 2^5 here
nwagers <- 5
if (nwagers == 3){
  potential.picks <- expand.grid(game1 = c(1,2), game2 = c(1,2), game3 = c(1,2))
} else if (nwagers == 4) {
  potential.picks <- expand.grid(game1 = c(1,2), game2 = c(1,2), game3 = c(1,2), game4 = c(1,2))
} else if (nwagers == 5) {
  potential.picks <- expand.grid(game1 = c(1,2), game2 = c(1,2), game3 = c(1,2), game4 = c(1,2), game5 = c(1,2))
} else if (nwagers == 6) {
  potential.picks <- expand.grid(game1 = c(1,2), game2 = c(1,2), game3 = c(1,2), game4 = c(1,2), game5 = c(1,2), game6 = c(1,2))
} else if (nwagers == 7) {
  potential.picks <- expand.grid(game1 = c(1,2), game2 = c(1,2), game3 = c(1,2), game4 = c(1,2), game5 = c(1,2), game6 = c(1,2), game7 = c(1,2))
} else if (nwagers == 8) {
  potential.picks <- expand.grid(game1 = c(1,2), game2 = c(1,2), game3 = c(1,2), game4 = c(1,2), game5 = c(1,2), game6 = c(1,2), game7 = c(1,2), game8 = c(1,2))
}

# Picks selected
potential.picks[23,]
potential.picks[sample(c(1:nrow(potential.picks)), 1),]
potential.pickst <- t(potential.picks)

# Dataframe of all possible outcomes of the events (each team/player can win or lose)
outcomes <- as.data.frame(matrix(NA,nrow(odds.df),3))
for (i in 1:nrow(odds.df)){
  outcomes[i,1] <- odds.df[i,winners[i]] #winnners
  outcomes[i,2] <- odds.df[i,losers[i]] #losers
  outcomes[i,3] <- odds.df[i,picks[i]] #mypicks
}
colnames(outcomes) <- c("winner", "loser", "my_pick"); outcomes

# Calculate profit (losses if profit < 0)
earnings <- abs(outcomes[,2][which(outcomes[,3] == outcomes[,1])])
losings  <- abs(outcomes[,2][which(outcomes[,3] != outcomes[,1])])

profit <- sum(earnings) - sum(losings)
paste0("Result: ","$",profit)
