install.packages("devtools")
library(devtools)

install.packages("dplyr")
install.packages("ggplot2")
install.packages("shiny")
install_github("StatsWithR/statsr")

library(statsr)
library(dplyr)
library(shiny)
library(ggplot2)


data(ames)

### real data
ggplot(data = ames, aes(x = area)) +
  geom_histogram(binwidth = 250)

ames %>%
  summarise(mu = mean(area), pop_med = median(area), 
            sigma = sd(area), pop_iqr = IQR(area),
            pop_min = min(area), pop_max = max(area),
            pop_q1 = quantile(area, 0.25),  # first quartile, 25th percentile
            pop_q3 = quantile(area, 0.75))  # third quartile, 75th percentile


#### Sampling distribution
samp1 <- ames %>%
  sample_n(size = 50)

ggplot(data = samp1, aes(x = area)) +
  geom_histogram(binwidth = 250)


samp1 %>%
  summarise(mu = mean(area), pop_med = median(area), 
            sigma = sd(area), pop_iqr = IQR(area),
            pop_min = min(area), pop_max = max(area),
            pop_q1 = quantile(area, 0.25),  # first quartile, 25th percentile
            pop_q3 = quantile(area, 0.75))  # third quartile, 75th percentile

##### Sampling mean distribution

sample_means50 <- ames %>%
  rep_sample_n(size = 50, reps = 15000, replace = TRUE) %>%
  summarise(x_bar = mean(area))

ggplot(data = sample_means50, aes(x = x_bar)) +
  geom_histogram(binwidth = 20)

sample_means50 %>%
  summarise(mu = mean(x_bar), pop_med = median(x_bar), 
            sigma = sd(x_bar), pop_iqr = IQR(x_bar),
            pop_min = min(x_bar), pop_max = max(x_bar),
            pop_q1 = quantile(x_bar, 0.25),  # first quartile, 25th percentile
            pop_q3 = quantile(x_bar, 0.75))  # third quartile, 75th percentile
