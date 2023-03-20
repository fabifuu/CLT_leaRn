library(tidydice)
library(ggplot2)
library(dplyr)

set.seed(2343)

# ------ 1. Fair Die -----------

# Roll fair 6-side dice 10 times, in 1000 rounds.
x <- tidydice::roll_dice(times = 10, rounds = 1000, success = c(4, 5, 6), sides = 6)

# Histogram
x |> ggplot(aes(result)) +
  geom_histogram(color = "black", fill = "darkcyan", binwidth = 1) +
  theme_bw() +
  labs(title = "Fair 6-sided Die")
ggsave("./CLT/01 - fair die.png", width = 4, height = 4, units = "in")

# Sum every rounds
x |> group_by(round) |> 
  summarize(sum = sum(result)) |> 
  ggplot(aes(sum)) +
  geom_histogram(color = "black", fill = "darkcyan", binwidth = 2.9) +
  theme_bw() +
  labs(title = "Sum of Fair 6-sided Die")
ggsave("./CLT/02 - sum fair die.png", width = 4, height = 4, units = "in")

# ------ 2. Weighted Die (with manually inserted probability) -----------

# Roll weighted 6-side dice 10 times, in 1000 rounds.
y <- tidydice::roll_dice(times = 10, rounds = 1000,
                         success = c(4, 5, 6),
                         sides = 6,
                         prob = c(0.05, 0.1, 0.25,
                                  0.35, 0.05, 0.2))

# Histogram
y |> ggplot(aes(result)) +
  geom_histogram(color = "black", fill = "darkcyan", binwidth = 1) +
  theme_bw() +
  labs(title = "Weighted 6-sided Die")
ggsave("./CLT/03 - weighted die.png", width = 4, height = 4, units = "in")

# Sum every rounds
y |> group_by(round) |> 
  summarize(sum = sum(result)) |> 
  ggplot(aes(sum)) +
  geom_histogram(color = "black", fill = "darkcyan", binwidth = 2.9) +
  theme_bw() +
  labs(title = "Sum of Weighted 6-sided Die")
ggsave("./CLT/04 - weighted die.png", width = 4, height = 4, units = "in")

# ------ 3. Weighted Die with multiple probability from multiple distributions --------

runif <- runif(6) # uniform distribution
prob_runif <- runif/sum(runif)
rbinom <- rbinom(6, 4, 0.3) # binomial distribution
prob_rbinom <- rbinom/sum(rbinom)
rexp <- rexp(6, rate = 1) # exponential distribution
prob_rexp <- rexp/sum(rexp)
rchisq <- rchisq(6, 2, 0) # chi-square distribution
prob_rchisq <- rchisq/sum(rchisq)
rpois <- rpois(6, 6) # poisson distribution
prob_rpois <- rpois/sum(rpois)
rnorm <- rnorm(6, 10, 5) # normal distribution
prob_rnorm <- rnorm/sum(rnorm)

z1 <- roll_dice(times = 10, rounds = 1000,
                success = c(4, 5, 6),
                sides = 6,
                prob = prob_runif)
z1 |> mutate(prob = "Uniform Distribution") -> z1

z2 <- roll_dice(times = 10, rounds = 1000,
                success = c(4, 5, 6),
                sides = 6,
                prob = prob_rchisq)
z2 |> mutate(prob = "Chi-square Distribution") -> z2

z3 <- roll_dice(times = 10, rounds = 1000,
                success = c(4, 5, 6),
                sides = 6,
                prob = prob_rexp)
z3 |> mutate(prob = "Exponentioal Distribution") -> z3

z4 <- roll_dice(times = 10, rounds = 1000,
                success = c(4, 5, 6),
                sides = 6,
                prob = prob_rbinom)
z4 |> mutate(prob = "Binomial Distribution") -> z4

z5 <- roll_dice(times = 10, rounds = 1000,
                success = c(4, 5, 6),
                sides = 6,
                prob = prob_rpois)
z5 |> mutate(prob = "Poisson Distribution") -> z5

z6 <- roll_dice(times = 10, rounds = 1000,
                success = c(4, 5, 6),
                sides = 6,
                prob = prob_rnorm)
z6 |> mutate(prob = "Normal Distribution") -> z6

z_all <- bind_rows(z1, z2, z3, z4, z5, z6)

# Histogram
z_all |> ggplot(aes(result)) + 
  geom_histogram(color = "black", fill = "darkcyan", binwidth = 1) +
  theme_bw() +
  labs(title = "Multiple Probablity Weighted 6-sided Die",
       subtitle = "1000 round of rolling, 10 roll each round") +
  xlab("Results (side of die)") +
  ylab("Counts") +
  facet_wrap(vars(prob))
ggsave("./CLT/Multiple Probablity.png", width = 8, height = 6, units = "in")

# Sum every rounds
z_all |> group_by(prob, round) |> 
  summarize(sum = sum(result),
            prob = prob) |> 
  ggplot(aes(sum)) +
  geom_histogram(color = "black", fill = "darkcyan", binwidth = 2) +
  labs(title = "Sum of Multiple Probablity Weighted 6-sided Die",
       subtitle = "1000 round of rolling, 10 roll each round, we sum each round") +
  xlab("Results (side of die)") +
  ylab("Counts") +
  theme_bw() +
  facet_wrap(vars(prob))
ggsave("./CLT/Sum of Multiple Probablity.png", width = 9, height = 6, units = "in")

# Mean, Variance, Std
z_all |> group_by(prob) |> 
  summarize(mean = mean(result),
            variance = var(result),
            sd = sd(result))
