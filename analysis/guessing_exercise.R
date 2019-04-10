## Guessing exercise
## 
## Fill a jar with jelly beans (or similar small items) 
## and ask as many people as possible to guess how many there are.

library("tidyverse")
library("plotly")
library("here")

# the real value people tried to guess
n <- 127

# read the file with guesses
data <- read_csv(here("data", "guessing_exercise.csv"))

# remove NAs 
data <- data[!is.na(data[["guess"]]),]

# take the mean and median of all guesses
mean_guess <- mean(data[["guess"]])

# make a plot to see how it differs from the real value
ggplot(data, aes(x = guess)) +
  stat_density(position="identity",geom="line") +
  geom_vline(xintercept = n, colour = "red") +
  geom_text(aes(x = (n-3), label="real value", y = 0.010), 
            colour = "red", vjust = 1.2) +
  geom_vline(xintercept = mean_guess, colour = "darkblue") +
  geom_text(aes(x = (mean_guess + 4), label="mean guess", y = 0.005), 
            colour = "darkblue", vjust = 1.2) + 
  labs(x = "Guessed value",
       y = "Density")

