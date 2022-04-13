library(statsr)
library(dplyr)
library(ggplot2)

data(kobe_basket)


# Serie of hitting shot
kobe_streak <- calc_streak(kobe_basket$shot)


#Q3 
#Which of the following is false about the distribution 
#of Kobe’s streak lengths from the 2009 NBA finals.

ggplot(data = kobe_streak, aes(x = length)) +
  geom_histogram(binwidth = 1)

# sample coin
coin_outcomes <- c("heads", "tails")
sample(coin_outcomes, size = 1, replace = TRUE)

sim_fair_coin <- sample(coin_outcomes, size = 100, replace = TRUE)


# table from vector
table(sim_fair_coin)


# sample unfair coin
sim_unfair_coin <- sample(coin_outcomes, size = 100, replace = TRUE, 
                          prob = c(0.2, 0.8))
table(sim_unfair_coin)

# sim shoot

shot_outcomes <- c("H", "M")
sim_basket <- sample(shot_outcomes, size = 133, replace = TRUE,
                     prob = c(0.45, 0.55))

#calc streak
sim_streak <- calc_streak(sim_basket)

# plot streak

ggplot(data = sim_streak, aes(x = length)) +
  geom_histogram(binwidth = 1)
